(* Spike 1 of Phase 0. A fake host with a virtual clock and a model of a turn:
   the test checks the rules of section 12 of docs/design.html. *)
open Kilim_spike_async
open Async.Syntax

let failures = ref 0

let check name ok =
  if ok then print_endline ("ok   " ^ name)
  else begin
    incr failures ;
    print_endline ("FAIL " ^ name)
  end

(* The fake host: a virtual clock, timers, and microtasks. *)
let clock = ref 0.

let timers : (float * int * (unit -> unit)) list ref = ref []

let timer_id = ref 0

let live_timers () = List.length !timers

let set_timer ~ms f =
  incr timer_id ;
  let id = !timer_id in
  timers := (!clock +. float ms, id, f) :: !timers ;
  fun () -> timers := List.filter (fun (_, i, _) -> i <> id) !timers

let host = {Async.now_ms= (fun () -> !clock); set_timer}

let microtasks = Queue.create ()

let rec run =
  lazy
    (Async.Run_queue.create ~schedule:(fun () -> Queue.push turn_now microtasks))

(* A turn: the runtime runs the callback, then drains the run queue. *)
and turn f =
  let r = Lazy.force run in
  r.in_turn <- true ;
  f () ;
  Async.Run_queue.drain r ;
  r.in_turn <- false

and turn_now () = turn (fun () -> ())

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

let start ?(tok = Cancel.root ()) t ~ok ~error =
  turn (fun () -> Async.start ~host ~run:(Lazy.force run) tok t ~ok ~error) ;
  run_microtasks ()

let result t =
  let r = ref None in
  start t ~ok:(fun x -> r := Some (Ok x)) ~error:(fun e -> r := Some (Error e)) ;
  !r

let () =
  (* 1. A chain of 1,000,000 synchronous steps, right-nested and left-nested. *)
  let n = 1_000_000 in
  let rec loop k =
    if k = 0 then Async.return 0
    else
      let* () = Async.return () in
      loop (k - 1)
  in
  check "right-nested chain of 1,000,000 steps" (result (loop n) = Some (Ok 0)) ;
  let left =
    List.fold_left
      (fun acc _ -> Async.bind acc (fun x -> Async.return (x + 1)))
      (Async.return 0) (List.init n Fun.id)
  in
  check "left-nested chain of 1,000,000 steps" (result left = Some (Ok n)) ;
  (* 2. Once: a second ok runs the continuation one time only. *)
  let count = ref 0 in
  let twice = Async.make (fun _ ~ok ~error:_ -> ok 1 ; ok 2) in
  let r =
    result
      (let* x = twice in
       incr count ; Async.return x )
  in
  check "once: the second ok does nothing" (r = Some (Ok 1) && !count = 1) ;
  (* 3. The error of a later step goes to the error function of that step. *)
  let r =
    result
      (let* _ = Async.make (fun _ ~ok ~error:_ -> ok 1) in
       Async.make (fun _ ~ok:_ ~error:_ -> failwith "later") )
  in
  check "an error of a later step"
    (match r with Some (Error (Failure _)) -> true | _ -> false) ;
  (* 4. An error in the queue before a cancel does not arrive. *)
  let tok = Cancel.root () and got = ref false in
  turn (fun () ->
      Async.start ~host ~run:(Lazy.force run) tok
        (Async.make (fun _ ~ok:_ ~error -> error Not_found))
        ~ok:(fun _ -> ())
        ~error:(fun _ -> got := true) ;
      Cancel.cancel tok ) ;
  check "an error before a cancel does not arrive" (not !got) ;
  (* 5. ok from a callback that kilim does not wrap: a microtask turn drains. *)
  let pending = ref (fun () -> ()) and got = ref None in
  start
    (let* x = Async.make (fun _ ~ok ~error:_ -> pending := fun () -> ok 41) in
     Async.return (x + 1) )
    ~ok:(fun x -> got := Some x)
    ~error:(fun _ -> ()) ;
  !pending () ;
  run_microtasks () ;
  check "ok from a raw callback" (!got = Some 42) ;
  (* 6. End actions run when the step ends, also after a release. *)
  let live = ref 0 in
  ignore
    (result
       (Async.make (fun tok ~ok ~error:_ ->
            incr live ;
            Cancel.on_end tok (fun () -> decr live) ;
            ok () ) ) ) ;
  let live2 = ref 0 in
  ignore
    (result
       (Async.make (fun tok ~ok ~error:_ ->
            incr live2 ;
            ok () ;
            Cancel.on_end tok (fun () -> decr live2) ) ) ) ;
  check "end actions after ok, and on_end after the release"
    (!live = 0 && !live2 = 0) ;
  (* 7. Cancel order: children from the newest, then the actions in reverse
     order. *)
  let order = Buffer.create 16 in
  let r = Cancel.root () in
  let a = Cancel.child r in
  let b = Cancel.child r in
  Cancel.on_end r (fun () -> Buffer.add_string order "r1 ") ;
  Cancel.on_end r (fun () -> Buffer.add_string order "r2 ") ;
  Cancel.on_end a (fun () -> Buffer.add_string order "a ") ;
  Cancel.on_end b (fun () -> Buffer.add_string order "b ") ;
  Cancel.cancel r ;
  check "cancel order" (Buffer.contents order = "b a r2 r1 ") ;
  (* 8. A cancel of an ended token sets its cancelled flag. *)
  let r = Cancel.root () in
  let t = Cancel.child r in
  Cancel.release t ;
  Cancel.cancel t ;
  check "a cancel of an ended token sets the flag" (Cancel.cancelled t) ;
  (* 9. A fold of 100,000 nested and+: the start and the cancel do not
     overflow. *)
  let m = 100_000 in
  let never = Async.make (fun _ ~ok:_ ~error:_ -> ()) in
  let t =
    List.fold_left
      (fun acc _ ->
        let+ x, _ =
          let+ x = acc and+ y = never in
          (x, y)
        in
        x )
      (Async.return 0) (List.init m Fun.id)
  in
  let tok = Cancel.root () in
  ( match start ~tok t ~ok:(fun _ -> ()) ~error:(fun _ -> ()) with
  | () ->
      check "100,000 nested and+: start" true
  | exception Stack_overflow ->
      check "100,000 nested and+: start" false ) ;
  ( match Cancel.cancel tok with
  | () ->
      check "100,000 nested and+: cancel" true
  | exception Stack_overflow ->
      check "100,000 nested and+: cancel" false ) ;
  (* 10. sleep on the virtual clock, and a cancel clears its timer. *)
  let got = ref false in
  start (Async.sleep ~ms:300) ~ok:(fun () -> got := true) ~error:(fun _ -> ()) ;
  advance 299. ;
  let early = !got in
  advance 1. ;
  check "sleep ends at 300 ms" ((not early) && !got) ;
  let tok = Cancel.root () and got = ref false in
  start ~tok (Async.sleep ~ms:300)
    ~ok:(fun () -> got := true)
    ~error:(fun _ -> ()) ;
  let before = live_timers () in
  Cancel.cancel tok ;
  advance 1000. ;
  check "a cancel clears the timer"
    (before = 1 && live_timers () = 0 && not !got) ;
  (* 11. first and timeout. *)
  let got = ref None in
  start
    (Async.first
       [ Async.map (fun () -> "slow") (Async.sleep ~ms:500)
       ; Async.map (fun () -> "fast") (Async.sleep ~ms:100) ] )
    ~ok:(fun x -> got := Some x)
    ~error:(fun _ -> ()) ;
  advance 100. ;
  check "first: the first ok wins, the other timer is cleared"
    (!got = Some "fast" && live_timers () = 0) ;
  let got = ref None in
  start
    (Async.timeout ~ms:200 (Async.sleep ~ms:1000))
    ~ok:(fun r -> got := Some r)
    ~error:(fun _ -> ()) ;
  advance 200. ;
  check "timeout" (!got = Some (Error `Timeout) && live_timers () = 0) ;
  check "first [] fails"
    ( match result (Async.first []) with
    | Some (Error (Invalid_argument _)) ->
        true
    | _ ->
        false ) ;
  if !failures > 0 then exit 1
