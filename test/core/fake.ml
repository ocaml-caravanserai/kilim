(* A fake host with a virtual clock, timers and microtasks, and a model of a
   turn: the runtime runs a callback, then drains the run queue. *)

open Kilim
module Q = Private.Run_queue

let clock = ref 0.

let timers : (float * int * (unit -> unit)) list ref = ref []

let timer_id = ref 0

let microtasks : (unit -> unit) Queue.t = Queue.create ()

let schedules = ref 0

let live_timers () = List.length !timers

let set_timer ~ms f =
  incr timer_id ;
  let id = !timer_id in
  timers := (!clock +. float ms, id, f) :: !timers ;
  fun () -> timers := List.filter (fun (_, i, _) -> i <> id) !timers

let host =
  { Private.Host.now_ms= (fun () -> !clock)
  ; set_timer
  ; request_frame= (fun _ -> ())
  ; queue_microtask= (fun f -> Queue.push f microtasks)
  ; post_task= (fun _ -> ()) }

let run_queue : Q.t option ref = ref None

let rec run () =
  match !run_queue with
  | Some r ->
      r
  | None ->
      let r = Q.create ~schedule in
      run_queue := Some r ;
      r

and schedule () =
  incr schedules ;
  Queue.push (fun () -> turn ignore) microtasks

and turn f =
  let r = run () in
  Q.with_turn r (fun () -> f () ; Q.drain r)

let run_microtasks () =
  while not (Queue.is_empty microtasks) do
    (Queue.pop microtasks) ()
  done

(* Fire the timers that are due, each one in its own turn. *)
let advance ms =
  clock := !clock +. ms ;
  let rec loop () =
    match
      List.sort compare (List.filter (fun (t, _, _) -> t <= !clock) !timers)
    with
    | [] ->
        ()
    | (_, id, f) :: _ ->
        timers := List.filter (fun (_, i, _) -> i <> id) !timers ;
        turn f ;
        run_microtasks () ;
        loop ()
  in
  loop ()

let reset () =
  clock := 0. ;
  timers := [] ;
  Queue.clear microtasks ;
  schedules := 0 ;
  run_queue := None ;
  Private.set_error_handler (fun _ -> ())

let start ?(tok = Private.Cancel.root ()) t ~ok ~error =
  turn (fun () -> Private.Async.start ~host ~run:(run ()) tok t ~ok ~error) ;
  run_microtasks ()

(* The result of a task that ends in its first turn. *)
let result t =
  let r = ref None in
  start t ~ok:(fun x -> r := Some (Ok x)) ~error:(fun e -> r := Some (Error e)) ;
  !r
