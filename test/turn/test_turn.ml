(* The turn (design section 11): the exit tests of Phase 1 with no time limit,
   the render timing, the model compare, a re-entrant event and the stop. The
   main of the suite is at the end. *)

open Windtrap
open Kilim
open Support

(* A synchronous result in a click: one render, with one DOM operation. This is
   also the count budget of a click. *)
let sync_click () =
  let check task =
    let p = P.start (counter ~task ()) in
    equal int 1 (stats p).renders ;
    F.reset (P.doc p) ;
    click p ;
    equal int ~msg:"one render in the click turn" 2 (stats p).renders ;
    equal int 2 (stats p).messages ;
    equal int 2 (stats p).turns ;
    equal (list (pair string int)) [("set_text", 1)] (F.counts (P.doc p)) ;
    equal string "<div><button data-testid=\"b\">1</button></div>" (P.html p) ;
    equal int 0 (stats p).requests ;
    equal int 1 (stats p).max_renders ;
    is_false (ins p).pending ;
    equal int 0 (H.frames (P.host p)) ;
    no_errors p
  in
  check (fun () -> Async.return 1) ;
  check (fun () -> sync 1)

(* An ok from a raw callback: one microtask request, one Micro turn, and the
   render at the next frame. *)
let raw_ok () =
  let h = H.create () in
  let task () =
    Async.make (fun _ ~ok ~error:_ -> H.raw h ~after:10 (fun () -> ok 5))
  in
  let p = P.start ~host:h (counter ~task ()) in
  click p ;
  equal int 1 (stats p).renders ;
  P.advance p (Span.ms 10) ;
  equal int 1 (stats p).requests ;
  equal int 3 (stats p).turns ;
  equal int 2 (stats p).messages ;
  equal int ~msg:"no render yet" 1 (stats p).renders ;
  is_true (ins p).pending ;
  is_true (ins p).scheduled ;
  equal int 1 (H.frames h) ;
  equal int 1 (H.live_timers h) ;
  P.advance p (Span.ms 6) ;
  equal int 2 (stats p).renders ;
  equal int 0 (H.live_timers h) ;
  equal string "<div><button data-testid=\"b\">5</button></div>" (P.html p) ;
  settled p ;
  no_errors p

(* Render timing. *)
type tmsg = Focus | Tick | Click

let timing ?(init = Cmd.none) () =
  App.make
    ~init:(fun () -> (0, init))
    ~update:(fun m -> function
      | Focus | Tick -> (m + 1, Cmd.none) | Click -> (m + 10, Cmd.none) )
    ~view:(fun m ->
      El.div
        [ El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus Focus] ()
        ; button Click (string_of_int m) ] )
    ()

let frame_render () =
  let p = P.start (timing ()) in
  let h = P.host p in
  P.focus p (By.test_id "f") ;
  equal int ~msg:"a Macro turn does not render" 1 (stats p).renders ;
  equal int 1 (H.frames h) ;
  equal int 1 (H.live_timers h) ;
  settled p ;
  P.advance p (Span.ms 16) ;
  equal int 2 (stats p).renders ;
  equal int 0 (H.live_timers h) ;
  is_false (ins p).scheduled ;
  (* The 100 ms timer renders first when the frames are slow. *)
  let h = H.create ~frame_ms:1000 () in
  let p = P.start ~host:h (timing ()) in
  P.focus p (By.test_id "f") ;
  P.advance p (Span.ms 99) ;
  equal int 1 (stats p).renders ;
  P.advance p (Span.ms 1) ;
  equal int 2 (stats p).renders ;
  equal int 1 (H.frames h) ;
  P.advance p (Span.ms 900) ;
  equal int 0 (H.frames h) ;
  equal int ~msg:"the later frame does nothing" 2 (stats p).renders ;
  equal int 3 (stats p).turns

let hidden_render () =
  let h = H.create () in
  H.set_hidden h true ;
  let p =
    P.start ~host:h
      (timing
         ~init:
           (Cmd.task (Async.map (fun () -> Tick) (Async.sleep (Span.ms 50))))
         () )
  in
  (* The raw callback runs after the timer turn, and before its render task. *)
  let seen = ref None in
  H.raw h ~after:50 (fun () ->
      seen :=
        Some
          ( (stats p).renders
          , (ins p).pending
          , (ins p).scheduled
          , H.frames h
          , H.live_timers h ) ) ;
  P.advance p (Span.ms 50) ;
  equal
    (option (quad int bool bool (pair int int)))
    (Some (1, true, true, (0, 0)))
    (Option.map (fun (a, b, c, d, e) -> (a, b, c, (d, e))) !seen) ;
  equal int 2 (stats p).renders ;
  is_false (ins p).pending ;
  no_errors p

(* 100 raw sends due at the same time in a hidden page: one render. *)
let hidden_burst () =
  let h = H.create () in
  let q = Queue.create () in
  let p =
    P.start ~host:h
      (timing
         ~init:(Cmd.batch (List.init 100 (fun _ -> Cmd.task (waiting q))))
         () )
  in
  P.set_hidden p true ;
  Queue.iter (fun ok -> H.raw h ~after:10 (fun () -> ok Tick)) q ;
  P.advance p (Span.ms 10) ;
  equal int 100 (stats p).messages ;
  equal int 2 (stats p).renders ;
  equal int 0 (H.frames h) ;
  equal int 0 (H.live_timers h) ;
  equal string "100" (F.text (P.find p (By.test_id "b")))

(* The page becomes hidden with a pending frame render: one render at once, and
   the 100 ms timer is cleared. *)
let hidden_now () =
  let p = P.start (timing ()) in
  let h = P.host p in
  P.focus p (By.test_id "f") ;
  equal int 1 (H.live_timers h) ;
  P.set_hidden p true ;
  equal int 2 (stats p).renders ;
  equal int 0 (H.live_timers h) ;
  is_false (ins p).pending ;
  is_false (ins p).scheduled ;
  (* No pending render: nothing. *)
  P.set_hidden p true ;
  P.set_hidden p false ;
  equal int 2 (stats p).renders ;
  equal int 3 (stats p).turns ;
  (* The old frame callback runs, and does nothing. *)
  equal int 1 (H.frames h) ;
  P.advance p (Span.ms 32) ;
  equal int 0 (H.frames h) ;
  equal int 3 (stats p).turns ;
  settled p

(* A click with a pending render renders at once. The old frame callback then
   does nothing. *)
let click_pending () =
  let p = P.start (timing ()) in
  let h = P.host p in
  P.focus p (By.test_id "f") ;
  click p ;
  equal int 2 (stats p).renders ;
  equal string "11" (F.text (P.find p (By.test_id "b"))) ;
  equal int 0 (H.live_timers h) ;
  is_false (ins p).pending ;
  let turns = (stats p).turns in
  P.advance p (Span.ms 16) ;
  equal int 0 (H.frames h) ;
  equal int ~msg:"the old frame does nothing" turns (stats p).turns ;
  equal int 2 (stats p).renders

(* A view that sends a message on each render: at most 3 renders in one turn,
   and the rest at the next frame. *)
type vmsg = Inc

let render_cap () =
  let page = ref None in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m Inc -> (m + 1, Cmd.none))
      ~view:(fun m ->
        if m > 0 && m < 7 then
          Option.iter (fun p -> P.click p (By.test_id "b")) !page ;
        button Inc (string_of_int m) )
      ()
  in
  let p = P.start app in
  page := Some p ;
  click p ;
  equal int 4 (stats p).renders ;
  equal int 3 (stats p).max_renders ;
  equal int 4 (stats p).messages ;
  equal string "3" (F.text (P.find p (By.test_id "b"))) ;
  is_true (ins p).pending ;
  P.advance p (Span.ms 16) ;
  equal int 7 (stats p).renders ;
  equal int 7 (stats p).messages ;
  P.advance p (Span.ms 16) ;
  equal int 8 (stats p).renders ;
  equal string "7" (F.text (P.find p (By.test_id "b"))) ;
  equal int 3 (stats p).max_renders ;
  settled p

(* The model compare is physical. *)
type mmsg = Same | Change

let compare_app ~init ~change =
  App.make
    ~init:(fun () -> (init, Cmd.none))
    ~update:(fun m -> function
      | Same -> (m, Cmd.none) | Change -> (change m, Cmd.none) )
    ~view:(fun _ ->
      El.div [button ~id:"s" Same "same"; button ~id:"c" Change "change"] )
    ()

let model_compare () =
  let p = P.start (compare_app ~init:[1] ~change:(fun m -> 2 :: m)) in
  P.click p (By.test_id "s") ;
  equal int ~msg:"the same model: no render" 1 (stats p).renders ;
  equal int 1 (stats p).messages ;
  P.click p (By.test_id "c") ;
  equal int 2 (stats p).renders ;
  (* A model with a closure. *)
  let p =
    P.start (compare_app ~init:(fun x -> x + 1) ~change:(fun f x -> f x + 1))
  in
  P.click p (By.test_id "s") ;
  P.click p (By.test_id "c") ;
  equal int 2 (stats p).renders ;
  no_errors p ;
  (* 0. to -0. *)
  let p = P.start (compare_app ~init:0. ~change:(fun _ -> -0.)) in
  P.click p (By.test_id "c") ;
  equal int 2 (stats p).renders

(* A re-entrant event only puts its message in the queue, and the same turn
   processes it. *)
type rmsg = Outer | Inner

let reentrant () =
  let page = ref None in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m -> function
        | Outer ->
            Option.iter (fun p -> P.click p (By.test_id "i")) !page ;
            (m + 1, Cmd.none)
        | Inner ->
            (m + 10, Cmd.none) )
      ~view:(fun m ->
        El.div [button ~id:"o" Outer (string_of_int m); button ~id:"i" Inner "i"] )
      ()
  in
  let p = P.start app in
  page := Some p ;
  P.click p (By.test_id "o") ;
  equal int 2 (stats p).turns ;
  equal int 2 (stats p).messages ;
  equal int 2 (stats p).renders ;
  equal string "11" (F.text (P.find p (By.test_id "o"))) ;
  settled p

(* The stop. *)
let stop () =
  let h = H.create () in
  let q = Queue.create () in
  let p =
    P.start ~host:h
      (timing
         ~init:
           (Cmd.batch
              [ Cmd.task (waiting q)
              ; Cmd.task (waiting q)
              ; Cmd.task (Async.map (fun () -> Tick) (Async.sleep (Span.s 1)))
              ] )
         () )
  in
  equal int 1 (H.live_timers h) ;
  P.focus p (By.test_id "f") ;
  is_true (ins p).scheduled ;
  (* A handler that fires outside a turn puts its message in the queue, and a
     task result goes to the run queue. *)
  let button = P.find p (By.test_id "b") in
  H.raw h (fun () ->
      F.fire button "click" Private.Dom.Unit () ;
      (Queue.pop q) Tick ;
      equal int 1 (ins p).messages_left ;
      equal int 1 (ins p).continuations_left ;
      P.stop p ;
      equal int 0 (ins p).messages_left ;
      equal int 0 (ins p).continuations_left ) ;
  P.run p ;
  equal string "<div></div>" (P.html p) ;
  equal int 0 (H.live_timers h) ;
  is_false (ins p).alive ;
  is_false (ins p).pending ;
  is_false (ins p).scheduled ;
  let turns = (stats p).turns in
  H.raw h (fun () -> (Queue.pop q) Tick) ;
  H.idle h ;
  P.click p (By.node (P.root p)) ;
  P.stop p ;
  equal int turns (stats p).turns ;
  equal int 0 (ins p).messages_left ;
  equal int 0 (ins p).continuations_left ;
  P.expect_no_tasks p ;
  no_errors p

(* A stop in [update]: the turn ends, and nothing stays. *)
type smsg = Stop | Other

let stop_in_update () =
  let h = H.create () in
  let q = Queue.create () in
  let page = ref None in
  let app =
    App.make
      ~init:(fun () ->
        (0, Cmd.batch [Cmd.task (waiting q); Cmd.task (waiting q)]) )
      ~update:(fun m -> function
        | Stop ->
            Option.iter P.stop !page ;
            ( m + 1
            , Cmd.task (Async.map (fun () -> Other) (Async.sleep (Span.s 1))) )
        | Other ->
            (m + 1, Cmd.none) )
      ~view:(fun m -> txt (string_of_int m))
      ()
  in
  let p = P.start ~host:h app in
  page := Some p ;
  H.raw h (fun () ->
      (Queue.pop q) Stop ;
      (Queue.pop q) Other ) ;
  P.run p ;
  let i = ins p in
  equal int 0 i.messages_left ;
  equal int 0 i.requests_left ;
  is_false i.pending ;
  is_false i.scheduled ;
  equal int 1 (stats p).messages ;
  equal string "<div></div>" (P.html p) ;
  equal int 0 (H.live_timers h) ;
  equal int 0 (H.frames h) ;
  no_errors p

(* A handler that fires outside a turn: its message asks for one turn in a
   microtask, also for two messages. After the stop, a message is dropped. *)
let fire_outside () =
  let p = P.start (counter ()) in
  let h = P.host p in
  let button = P.find p (By.test_id "b") in
  let turns = (stats p).turns in
  H.raw h (fun () ->
      F.fire button "click" Private.Dom.Unit () ;
      F.fire button "click" Private.Dom.Unit () ;
      equal int 2 (ins p).messages_left ;
      is_true (ins p).turn_requested ) ;
  P.run p ;
  equal int 1 (stats p).requests ;
  equal int (turns + 1) (stats p).turns ;
  equal int 0 (ins p).messages_left ;
  equal int 4 (stats p).messages ;
  P.stop p ;
  F.fire button "click" Private.Dom.Unit () ;
  equal int 0 (ins p).messages_left ;
  is_false (ins p).turn_requested ;
  equal int 1 (stats p).requests ;
  no_errors p

(* A DOM fault in the stop goes to the error handler. *)
let stop_fault () =
  let p = P.start (counter ()) in
  F.fail_at (P.doc p) 1 ;
  P.stop p ;
  is_true (match errors p with [F.Fault] -> true | _ -> false) ;
  equal int 0 (H.live_timers (P.host p))

let tests =
  [ group "turn"
      [ test "a synchronous result in a click renders one time" sync_click
      ; test "an ok from a raw callback, then the frame renders" raw_ok
      ; test "a Macro turn renders at the frame or after 100 ms" frame_render
      ; test "a hidden page renders in a new task" hidden_render
      ; test "100 raw sends in a hidden page give one render" hidden_burst
      ; test "a page that becomes hidden renders at once" hidden_now
      ; test "a click with a pending render renders at once" click_pending
      ; test "the model compare is physical" model_compare
      ; test "a re-entrant event goes to the queue" reentrant
      ; test "the stop" stop
      ; test "at most 3 renders in one turn" render_cap
      ; test "a stop in update ends the turn" stop_in_update
      ; test "a DOM fault in the stop" stop_fault
      ; test "a handler that fires outside a turn" fire_outside ] ]

let () =
  exit
    (Windtrap.run "turn"
       ( Test_values.tests @ Test_host.tests @ tests @ Test_errors.tests
       @ Test_latest.tests @ Test_event.tests @ Test_props.tests
       @ Test_leak.tests @ Test_limit.tests @ Test_faults.tests ) )
