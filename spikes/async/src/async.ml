module Run_queue = Run_queue

type host =
  {now_ms: unit -> float; set_timer: ms:int -> (unit -> unit) -> unit -> unit}

type ctx = {cancel: Cancel.t; host: host; run: Run_queue.t}

type 'a t = ctx -> ok:('a -> unit) -> error:(exn -> unit) -> unit

let on_error = ref (fun (_ : exn) -> ())

let set_error_handler f = on_error := f

let start ~host ~run cancel t ~ok ~error = t {cancel; host; run} ~ok ~error

let return x : 'a t = fun _ ~ok ~error:_ -> ok x

let fail e : 'a t = fun _ ~ok:_ ~error -> error e

(* The trampoline protects the start of [t] (a left-nested chain) and the
   continuation (a right-nested chain). *)
let bind (t : 'a t) (f : 'a -> 'b t) : 'b t =
 fun ctx ~ok ~error ->
  Run_queue.step ctx.run (fun () ->
      t ctx ~error ~ok:(fun x ->
          Run_queue.step ctx.run (fun () ->
              if not (Cancel.cancelled ctx.cancel) then
                match f x with
                | t' ->
                    t' ctx ~ok ~error
                | exception e ->
                    error e ) ) )

let map f t = bind t (fun x -> return (f x))

let make (start : Cancel.t -> ok:('a -> unit) -> error:(exn -> unit) -> unit) :
    'a t =
 fun ctx ~ok ~error ->
  if not (Cancel.cancelled ctx.cancel) then begin
    let step = Cancel.child ctx.cancel in
    (* The callback keeps only this cell. The end of the step clears it, thus a
       JS callback that never runs does not keep the continuation alive. *)
    let cell = ref (Some (ok, error)) in
    Cancel.on_end step (fun () -> cell := None) ;
    let finish pick v =
      match !cell with
      | Some ks when not (Cancel.cancelled step) ->
          cell := None ;
          Cancel.release step ;
          let k = pick ks in
          (* Never inside the callback. Examine the token again at the run. *)
          Run_queue.push ctx.run (fun () ->
              if not (Cancel.cancelled ctx.cancel) then k v )
      | _ ->
          ()
    in
    try start step ~ok:(finish fst) ~error:(finish snd)
    with e -> if Option.is_none !cell then !on_error e else finish snd e
  end

(* Each branch has a child token and starts through the run queue. *)
let both (a : 'a t) (b : 'b t) : ('a * 'b) t =
 fun ctx ~ok ~error ->
  let ta = Cancel.child ctx.cancel and tb = Cancel.child ctx.cancel in
  let ra = ref None and rb = ref None and failed = ref false in
  let check () =
    match (!ra, !rb) with Some x, Some y -> ok (x, y) | _ -> ()
  in
  let fail other e =
    if not !failed then begin
      failed := true ;
      Cancel.cancel other ;
      error e
    end
  in
  Run_queue.step ctx.run (fun () ->
      a {ctx with cancel= ta}
        ~ok:(fun x ->
          Cancel.release ta ;
          ra := Some x ;
          check () )
        ~error:(fail tb) ) ;
  Run_queue.step ctx.run (fun () ->
      b {ctx with cancel= tb}
        ~ok:(fun y ->
          Cancel.release tb ;
          rb := Some y ;
          check () )
        ~error:(fail ta) )

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) t f = map f t

  let ( and+ ) = both
end

let sleep ~ms : unit t =
 fun ctx ~ok ~error ->
  make
    (fun tok ~ok ~error:_ ->
      let clear = ctx.host.set_timer ~ms (fun () -> ok ()) in
      Cancel.on_end tok clear )
    ctx ~ok ~error

(* The first ok wins and cancels the other branches. If all fail, the last
   error. *)
let first (ts : 'a t list) : 'a t =
 fun ctx ~ok ~error ->
  match ts with
  | [] ->
      error (Invalid_argument "Async.first []")
  | _ ->
      let n = List.length ts in
      let failures = ref 0 and settled = ref false in
      let toks = List.map (fun _ -> Cancel.child ctx.cancel) ts in
      let cancel_others mine =
        List.iter (fun t -> if t != mine then Cancel.cancel t) toks
      in
      List.iter2
        (fun t tok ->
          Run_queue.step ctx.run (fun () ->
              t {ctx with cancel= tok}
                ~ok:(fun x ->
                  if not !settled then begin
                    settled := true ;
                    Cancel.release tok ;
                    cancel_others tok ;
                    ok x
                  end )
                ~error:(fun e ->
                  incr failures ;
                  if (not !settled) && !failures = n then begin
                    settled := true ;
                    error e
                  end ) ) )
        ts toks

let timeout ~ms t =
  first [map (fun x -> Ok x) t; map (fun () -> Error `Timeout) (sleep ~ms)]
