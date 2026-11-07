module Span = Kilim.Span

type state = Waiting | Moved | Cleared | Ran

type timer =
  {due: float; seq: int; f: unit -> unit; counted: bool; mutable state: state}

module Due = Map.Make (struct
  type t = float * int

  let compare (a, i) (b, j) =
    match Float.compare a b with 0 -> Int.compare i j | c -> c
end)

exception Fault

(* A fault at call number [at] of one host function. [at = 0]: no fault. *)
type fault = {mutable calls: int; mutable at: int}

let fault () = {calls= 0; at= 0}

let call f =
  f.calls <- f.calls + 1 ;
  if f.calls = f.at then begin
    f.at <- 0 ;
    raise Fault
  end

let fail_at f n = f.at <- f.calls + n

type t =
  { mutable now: float
  ; cost: float
  ; frame_ms: float
  ; clock: fault
  ; posts: fault
  ; micros: fault
  ; micro: (unit -> unit) Queue.t
  ; ready: (unit -> unit) Queue.t
  ; mutable timers: timer Due.t  (** The timers that wait. *)
  ; mutable seq: int
  ; mutable live: int
  ; mutable frames: (unit -> unit) list  (** The newest first. *)
  ; mutable frame_req: float option  (** The time of the first request. *)
  ; mutable frame_moved: bool  (** The frame is in the ready FIFO. *)
  ; mutable hidden: bool
  ; mutable errors: exn list  (** The newest first. *)
  ; mutable warnings: string list  (** The newest first. *) }

let create ?(cost = 1. /. 128.) ?(frame_ms = 16) () =
  if not (cost > 0. && cost <= 50.) then
    invalid_arg "Kilim_testing.Host.create: cost" ;
  if frame_ms <= 0 then invalid_arg "Kilim_testing.Host.create: frame_ms" ;
  let t =
    { now= 0.
    ; cost
    ; frame_ms= float frame_ms
    ; clock= fault ()
    ; posts= fault ()
    ; micros= fault ()
    ; micro= Queue.create ()
    ; ready= Queue.create ()
    ; timers= Due.empty
    ; seq= 0
    ; live= 0
    ; frames= []
    ; frame_req= None
    ; frame_moved= false
    ; hidden= false
    ; errors= []
    ; warnings= [] }
  in
  Kilim.Private.set_error_handler (fun e -> t.errors <- e :: t.errors) ;
  Kilim.Private.set_warning_handler (fun s -> t.warnings <- s :: t.warnings) ;
  t

let guard t f = try f () with e -> t.errors <- e :: t.errors

let microtasks t =
  while not (Queue.is_empty t.micro) do
    guard t (Queue.pop t.micro)
  done

let uncount tm t = if tm.counted then t.live <- t.live - 1

let add_timer t ~counted ms f =
  t.seq <- t.seq + 1 ;
  let tm = {due= t.now +. ms; seq= t.seq; f; counted; state= Waiting} in
  t.timers <- Due.add (tm.due, tm.seq) tm t.timers ;
  if counted then t.live <- t.live + 1 ;
  fun () ->
    match tm.state with
    | Waiting ->
        t.timers <- Due.remove (tm.due, tm.seq) t.timers ;
        tm.state <- Cleared ;
        uncount tm t
    | Moved ->
        tm.state <- Cleared ;
        uncount tm t
    | Cleared | Ran ->
        ()

let fire t tm () =
  match tm.state with
  | Moved ->
      tm.state <- Ran ;
      uncount tm t ;
      tm.f ()
  | Waiting | Cleared | Ran ->
      ()

let frame_due t =
  match t.frame_req with
  | Some r when (not t.hidden) && not t.frame_moved ->
      Some ((Float.floor (r /. t.frame_ms) +. 1.) *. t.frame_ms)
  | _ ->
      None

let run_frame t () =
  let fs = List.rev t.frames in
  t.frames <- [] ;
  t.frame_req <- None ;
  t.frame_moved <- false ;
  List.iter (guard t) fs

let rec move_timers t =
  match Due.min_binding_opt t.timers with
  | Some (k, tm) when tm.due <= t.now ->
      t.timers <- Due.remove k t.timers ;
      tm.state <- Moved ;
      Queue.push (fire t tm) t.ready ;
      move_timers t
  | _ ->
      ()

let move_due t =
  move_timers t ;
  match frame_due t with
  | Some d when d <= t.now ->
      t.frame_moved <- true ;
      Queue.push (run_frame t) t.ready
  | _ ->
      ()

let step t =
  microtasks t ;
  move_due t ;
  match Queue.take_opt t.ready with
  | None ->
      false
  | Some f ->
      guard t f ; microtasks t ; true

let run t =
  while step t do
    ()
  done

let next_due t =
  let timer =
    match Due.min_binding_opt t.timers with
    | Some ((d, _), _) ->
        Some d
    | None ->
        None
  in
  match (timer, frame_due t) with
  | Some a, Some b ->
      Some (Float.min a b)
  | (Some _ as d), None | None, (Some _ as d) ->
      d
  | None, None ->
      None

let advance t span =
  let target = t.now +. float (Span.to_ms span) in
  let fin = ref false in
  while not !fin do
    run t ;
    match next_due t with
    | Some d when d <= target ->
        t.now <- Float.max t.now d
    | _ ->
        fin := true
  done ;
  t.now <- Float.max t.now target

let idle ?(limit = 1_000_000) ?(until = fun () -> false) t =
  let n = ref 0 and fin = ref false in
  while not (!fin || until ()) do
    if !n >= limit then failwith "Kilim_testing.Host.idle: the step limit" ;
    incr n ;
    if not (step t) then
      match next_due t with
      | Some d ->
          t.now <- Float.max t.now d
      | None ->
          fin := true
  done

let host t : Kilim.Private.Host.t =
  { now_ms= (fun () -> call t.clock ; t.now)
  ; set_timer= (fun ~ms f -> add_timer t ~counted:true (float ms) f)
  ; request_frame=
      (fun f ->
        if List.is_empty t.frames then t.frame_req <- Some t.now ;
        t.frames <- f :: t.frames )
  ; queue_microtask= (fun f -> call t.micros ; Queue.push f t.micro)
  ; post_task= (fun f -> call t.posts ; Queue.push f t.ready) }

let now t = t.now

let hidden t = t.hidden

let set_hidden t b = t.hidden <- b

let raw t ?(after = 0) f =
  let (_ : unit -> unit) = add_timer t ~counted:false (float after) f in
  ()

let raw_microtask t f = Queue.push f t.micro

let live_timers t = t.live

let frames t = List.length t.frames

let errors t = List.rev t.errors

let warnings t = List.rev t.warnings

let env t =
  { Kilim.Private.Runtime.host= host t
  ; hidden= (fun () -> t.hidden)
  ; cost= (fun _ -> t.now <- t.now +. t.cost)
  ; same= ( == ) }

let spend t ms = t.now <- t.now +. ms

let clock_reads t = t.clock.calls

let fail_clock t n = fail_at t.clock n

let fail_post_task t n = fail_at t.posts n

let fail_microtask t n = fail_at t.micros n
