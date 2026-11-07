(* The faults of the host (design section 11): the catch of the turn clears the
   flags of the turn, adds the time of the turn one time, posts the tick again,
   and asks for a new turn if work stays. *)

open Windtrap
open Kilim
open Support

type msg = Got of string | Loop | Click | Plain

let is_fault = function [H.Fault] -> true | _ -> false

(* Three tasks from init. One raw callback at 10 calls their [ok]. *)
let abc h =
  let q = Queue.create () in
  let app =
    App.make
      ~init:(fun () ->
        ([], Cmd.batch (List.init 3 (fun _ -> Cmd.task (waiting q)))) )
      ~update:(fun m -> function
        | Got s -> (s :: m, Cmd.none) | _ -> (m, Cmd.none) )
      ~view:(fun m -> txt (String.concat "" (List.rev m)))
      ()
  in
  let p = P.start ~host:h app in
  H.raw h ~after:10 (fun () ->
      List.iter2
        (fun ok s -> ok (Got s))
        (List.of_seq (Queue.to_seq q))
        ["A"; "B"; "C"] ) ;
  P.advance p (Span.ms 9) ;
  p

(* A fault at each clock read in turn. *)
let sweep () =
  let h = H.create () in
  let p = abc h in
  let before = H.clock_reads h in
  H.idle h ;
  let n = H.clock_reads h - before in
  equal string "<div>ABC</div>" (P.html p) ;
  (* The Micro turn: its start, the check before the drain, the read of the
     drain, the check before each of the 3 messages, and its end. The frame
     turn: its start and its end. *)
  equal int ~msg:"clock reads" 9 n ;
  for k = 1 to n do
    let h = H.create () in
    let p = abc h in
    H.fail_clock h k ;
    H.idle h ~until:(fun () ->
        is_false ~msg:"in_turn after each step" (ins p).in_turn ;
        false ) ;
    let msg = "fault at read " ^ string_of_int k in
    is_true ~msg (is_fault (H.errors h)) ;
    equal int ~msg 3 (stats p).messages ;
    equal string ~msg "<div>ABC</div>" (P.html p) ;
    let i = ins p in
    equal int ~msg 0 i.messages_left ;
    equal int ~msg 0 i.continuations_left ;
    equal int ~msg 0 i.requests_left ;
    is_false ~msg i.pending ;
    is_false ~msg i.scheduled ;
    is_false ~msg i.continuing ;
    is_false ~msg i.turn_requested ;
    is_false ~msg i.tick_pending
  done

(* A click starts a task whose ok comes from a microtask: its Micro turn runs
   before the tick, on the busy sum of the click. *)
let click_app h =
  let q = Queue.create () in
  let app =
    App.make
      ~init:(fun () -> ([], Cmd.task (waiting q)))
      ~update:(fun m -> function
        | Click ->
            ( m
            , Cmd.task
                (Async.make (fun _ ~ok ~error:_ ->
                     H.raw_microtask h (fun () -> ok (Got "m")) ) ) )
        | Got s ->
            (s :: m, Cmd.none)
        | Loop | Plain ->
            (m, Cmd.none) )
      ~view:(fun m ->
        El.div [button Click (String.concat "" m); button ~id:"p" Plain "p"] )
      ()
  in
  (P.start ~host:h app, q)

(* A fault of the post of the tick at the end of a click turn: the catch posts
   the tick again, and adds the busy time only one time. *)
let tick_fault () =
  let h = H.create ~cost:20. () in
  let p, _ = click_app h in
  H.run h ;
  is_false (ins p).tick_pending ;
  H.fail_post_task h 1 ;
  let turns = (stats p).turns in
  click p ;
  is_true (is_fault (H.errors h)) ;
  is_true ~msg:"a posted tick" (ins p).tick_pending ;
  equal int ~msg:"the Micro turn made progress" 2 (stats p).messages ;
  equal int 0 (stats p).stops ;
  equal int ~msg:"the click and the Micro turn" (turns + 2) (stats p).turns ;
  equal int 1 (stats p).requests ;
  is_true (H.step h) ;
  is_false (ins p).tick_pending ;
  (* The next turn posts a tick normally, and starts with a busy sum of 0. *)
  P.advance p (Span.ms 100) ;
  click p ;
  is_true (ins p).tick_pending ;
  equal int 4 (stats p).messages ;
  equal int 0 (stats p).stops ;
  equal int 1 (List.length (H.errors h)) ;
  (* A turn with no work and no render: the catch asks for no turn and no
     render. *)
  P.advance p (Span.ms 100) ;
  H.fail_post_task h 1 ;
  let turns = (stats p).turns and requests = (stats p).requests in
  P.click p (By.test_id "p") ;
  equal int 2 (List.length (H.errors h)) ;
  equal int (turns + 1) (stats p).turns ;
  equal int requests (stats p).requests ;
  is_false (ins p).pending ;
  equal int 0 (H.frames h) ;
  equal int 0 (H.live_timers h)

(* A fault at the first clock read of a turn: the catch adds no time, because
   the start of the turn is not known. The next Micro turn makes progress. *)
let first_read_fault () =
  let h = H.create ~cost:20. () in
  let p, q = click_app h in
  P.advance p (Span.ms 100) ;
  H.fail_clock h 1 ;
  H.raw_microtask h (fun () -> (Queue.pop q) (Got "q")) ;
  click p ;
  is_true (is_fault (H.errors h)) ;
  equal int ~msg:"the Micro turn made progress" 1 (stats p).messages ;
  equal int 0 (stats p).stops ;
  equal int 0 (ins p).continuations_left ;
  settled p

(* A fault of the post of the continuation, with the loop of the warning test: a
   Micro turn continues, and the next stop posts a continuation. *)
let continuation_fault () =
  let h = H.create ~cost:1. () in
  let task () =
    Async.make (fun _ ~ok ~error:_ -> H.raw_microtask h (fun () -> ok Loop))
  in
  (* Post number 1 is the tick of the start, number 2 the continuation. *)
  H.fail_post_task h 2 ;
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.task (task ())))
         ~update:(fun m _ -> (m + 1, Cmd.task (task ())))
         ~view:(fun m -> txt (string_of_int m))
         () )
  in
  is_true (is_fault (H.errors h)) ;
  equal int 2 (stats p).stops ;
  equal int 1 (stats p).yields ;
  is_true (ins p).continuing ;
  is_false (ins p).turn_requested ;
  let m = (stats p).messages in
  equal int 25 m ;
  H.idle h ~until:(fun () -> (stats p).stops >= 12) ;
  is_true ~msg:"the messages go on" ((stats p).messages > m) ;
  equal int 1 (List.length (H.errors h))

(* A fault of the microtask request: no flag stays. The next push does not ask,
   because the run queue is not empty. A click drains it. *)
let microtask_fault () =
  let h = H.create () in
  let q = Queue.create () in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () ->
           ([], Cmd.batch [Cmd.task (waiting q); Cmd.task (waiting q)]) )
         ~update:(fun m -> function
           | Got s -> (s :: m, Cmd.none) | _ -> ("c" :: m, Cmd.none) )
         ~view:(fun m -> button Click (String.concat "" (List.rev m)))
         () )
  in
  H.fail_microtask h 1 ;
  (* The fault goes to the error handler, and the raw callback goes on. *)
  H.raw h (fun () ->
      (Queue.pop q) (Got "a") ;
      is_false (ins p).turn_requested ;
      equal int 1 (ins p).continuations_left ;
      (Queue.pop q) (Got "b") ) ;
  P.run p ;
  is_true (is_fault (H.errors h)) ;
  is_false (ins p).turn_requested ;
  equal int 0 (stats p).requests ;
  equal int 2 (ins p).continuations_left ;
  equal int 1 (stats p).turns ;
  click p ;
  equal int 2 (stats p).turns ;
  equal int 3 (stats p).messages ;
  equal string ~msg:"the click first, then the drain" "cab"
    (F.text (P.find p (By.test_id "b"))) ;
  equal int 1 (List.length (H.errors h))

(* A fault in a click turn with a pending render: a recovery turn renders at
   once. A fault in the recovery turn only marks the render as pending. *)
let recovery () =
  let check ~again =
    let h = H.create () in
    let errors = ref [] in
    let p =
      P.start ~host:h
        (App.make
           ~init:(fun () -> (0, Cmd.none))
           ~update:(fun m -> function
             | Got _ -> (m + 1, Cmd.none) | _ -> (m + 10, Cmd.none) )
           ~view:(fun m ->
             El.div
               [ button Click (string_of_int m)
               ; El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus (Got "f")] ()
               ] )
           () )
    in
    P.focus p (By.test_id "f") ;
    Private.set_error_handler (fun e ->
        errors := e :: !errors ;
        (* Read 1 is the end of the click turn, read 2 the start of the recovery
           turn. *)
        if again && List.length !errors = 1 then H.fail_clock h 2 ) ;
    (* Read 1 starts the click turn, read 2 is the check before the message. *)
    H.fail_clock h 2 ;
    click p ;
    let text () = F.text (P.find p (By.test_id "b")) in
    if again then begin
      equal int 2 (List.length !errors) ;
      equal string "0" (text ()) ;
      is_true (ins p).pending ;
      is_true (ins p).scheduled ;
      P.advance p (Span.ms 16)
    end
    else equal int 1 (List.length !errors) ;
    equal string "11" (text ()) ;
    equal int 2 (stats p).renders ;
    settled p
  in
  check ~again:false ; check ~again:true

(* A second fault inside the catch of the turn goes to the error handler. The
   error handler arms the second fault. *)
let double () =
  let arm h f =
    let errors = ref [] in
    Private.set_error_handler (fun e ->
        errors := e :: !errors ;
        if List.length !errors = 1 then f h ) ;
    errors
  in
  (* The post of the tick fails, then the post again in the catch. *)
  let h = H.create ~cost:20. () in
  let p, _ = click_app h in
  H.run h ;
  let errors = arm h (fun h -> H.fail_post_task h 1) in
  H.fail_post_task h 1 ;
  click p ;
  equal int 2 (List.length !errors) ;
  equal int 2 (stats p).messages ;
  (* The clock read at the end of the turn fails, then the read in the catch. *)
  let h = H.create ~cost:20. () in
  let p, _ = click_app h in
  H.run h ;
  let errors = arm h (fun h -> H.fail_clock h 1) in
  (* The start, the check before the message, the check before the request, then
     the end. *)
  H.fail_clock h 4 ;
  click p ;
  equal int 2 (List.length !errors) ;
  is_false (ins p).in_turn ;
  (* In a hidden page, the post of the render fails, then the post of the render
     in the catch. Post 1 is the tick of the click turn, post 2 the render of
     the Micro turn. *)
  let h = H.create () in
  let p, _ = click_app h in
  P.set_hidden p true ;
  H.run h ;
  let errors = arm h (fun h -> H.fail_post_task h 1) in
  H.fail_post_task h 2 ;
  click p ;
  equal int 2 (List.length !errors) ;
  is_true (ins p).pending ;
  is_false (ins p).scheduled

(* Runtime.abort: a trap of wasm_of_ocaml leaves a turn open. Here update calls
   abort in the turn: the turn ends, the error goes to the error handler, and
   the app still works. Outside a turn, abort only reports. kilim.browser calls
   abort only from a microtask, when no OCaml code of a turn runs, thus no live
   outer turn is open then: native code cannot make that state, and no test here
   has an outer turn. *)
exception Boom

type amsg = Abort | Inc

let abort () =
  let page = ref None and seen = ref [] in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m -> function
        | Abort ->
            (* The second abort finds no open turn: it only reports. *)
            R.abort Boom ;
            R.abort Boom ;
            seen := (ins (Option.get !page)).in_turn :: !seen ;
            (m, Cmd.none)
        | Inc ->
            (m + 1, Cmd.none) )
      ~view:(fun m ->
        El.div
          [button ~id:"a" Abort "abort"; button ~id:"i" Inc (string_of_int m)] )
      ()
  in
  let p = P.start app in
  page := Some p ;
  (* Outside a turn, abort only reports. *)
  let turns = (stats p).turns in
  R.abort Boom ;
  equal int 1 (List.length (errors p)) ;
  equal int turns (stats p).turns ;
  is_false (ins p).turn_requested ;
  P.click p (By.test_id "a") ;
  equal (list bool) ~msg:"the turn ends at the abort" [false] !seen ;
  is_true (match errors p with [Boom; Boom; Boom] -> true | _ -> false) ;
  P.click p (By.test_id "i") ;
  equal string "1" (F.text (P.find p (By.test_id "i"))) ;
  settled p ;
  quiet p ;
  R.abort Boom ;
  equal int 4 (List.length (errors p)) ;
  (* A task that calls abort in its start, then ok: the result arrives. This
     does not test the reset of the run queue (native code always leaves the
     turn of the run queue): only the wasm pages of test/dom (D31, D35) test
     it. *)
  let p =
    P.start
      (App.make
         ~init:(fun () ->
           (0, Cmd.task (Async.make (fun _ ~ok ~error:_ -> R.abort Boom ; ok 1))) )
         ~update:(fun m n -> (m + n, Cmd.none))
         ~view:(fun m -> txt (string_of_int m))
         () )
  in
  equal int ~msg:"the request of the continuation" 1 (stats p).requests ;
  quiet p ;
  equal string "<div>1</div>" (P.html p)

(* The list of the open turns with two runtimes. A trap leaves its turn open
   with no OCaml catch: here an effect does the same, because its handler in the
   outer update drops the continuation. The update of page A clicks a button of
   page B, and the update of B leaves its turn so. The turn of A ends normally:
   it removes its own entry, not the entry of B. Then the abort ends the turn of
   B, and B works again. *)
type _ Effect.t += Leave : unit Effect.t

type omsg = Go | Inc_o

let open_turns () =
  let leave = ref true in
  let pb =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Go ->
               if !leave then begin
                 leave := false ;
                 Effect.perform Leave
               end ;
               (m + 1000, Cmd.none)
           | Inc_o ->
               (m + 1, Cmd.none) )
         ~view:(fun m ->
           El.div [button ~id:"t" Go "t"; button ~id:"i" Inc_o (string_of_int m)] )
         () )
  in
  let pa =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Go ->
               Effect.Deep.match_with
                 (fun () -> P.click pb (By.test_id "t"))
                 ()
                 { retc= Fun.id
                 ; exnc= raise
                 ; effc=
                     (fun (type a) (e : a Effect.t) ->
                       match e with
                       | Leave ->
                           Some
                             (fun (_ : (a, unit) Effect.Deep.continuation) -> ())
                       | _ ->
                           None ) } ;
               (m + 100, Cmd.none)
           | Inc_o ->
               (m + 1, Cmd.none) )
         ~view:(fun m ->
           El.div [button ~id:"t" Go "t"; button ~id:"i" Inc_o (string_of_int m)] )
         () )
  in
  P.click pa (By.test_id "t") ;
  is_false ~msg:"the turn of A ends" (ins pa).in_turn ;
  is_true ~msg:"the turn of B stays open" (ins pb).in_turn ;
  R.abort Boom ;
  is_false ~msg:"the abort ends the turn of B" (ins pb).in_turn ;
  P.click pb (By.test_id "i") ;
  equal string "1" (F.text (P.find pb (By.test_id "i"))) ;
  equal string "100" (F.text (P.find pa (By.test_id "i")))

(* abort gives its error to the error handler one time: also when it ends two
   turns, and also when an earlier abort already ended them. The updates of
   pages B and C leave their turns open (an effect, as a trap), both in one
   update of page A. *)
exception E1

exception E2

let abort_once () =
  let leaving () =
    let leave = ref true in
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () ->
           if !leave then begin
             leave := false ;
             Effect.perform Leave
           end ;
           (m + 1, Cmd.none) )
         ~view:(fun m -> button () (string_of_int m))
         () )
  in
  let pb = leaving () and pc = leaving () in
  let drop f =
    Effect.Deep.match_with f ()
      { retc= Fun.id
      ; exnc= raise
      ; effc=
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Leave ->
                Some (fun (_ : (a, unit) Effect.Deep.continuation) -> ())
            | _ ->
                None ) }
  in
  let pa =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () ->
           drop (fun () -> P.click pb (By.test_id "b")) ;
           drop (fun () -> P.click pc (By.test_id "b")) ;
           (m + 1, Cmd.none) )
         ~view:(fun m -> button () (string_of_int m))
         () )
  in
  P.click pa (By.test_id "b") ;
  is_true ~msg:"two open turns" ((ins pb).in_turn && (ins pc).in_turn) ;
  (* The last host gets the errors. *)
  R.abort E1 ;
  is_false ~msg:"one abort ends the two turns"
    ((ins pb).in_turn || (ins pc).in_turn) ;
  (* The first abort empties the list: the second one does not end the turns of
     B and C again (it would post a tick for each one). *)
  P.stop pb ;
  P.stop pc ;
  H.idle (P.host pb) ;
  H.idle (P.host pc) ;
  R.abort E2 ;
  is_true ~msg:"each error one time"
    (match errors pa with [E1; E2] -> true | _ -> false) ;
  is_false ~msg:"no second end of B" (ins pb).tick_pending ;
  is_false ~msg:"no second end of C" (ins pc).tick_pending

(* Runtime.abort_open: the abort of an error event whose error is null (WebKit
   gives a trap of a wasm:js-string function so). It aborts only if a turn is
   still open: with no open turn, no report and no turn. With an open turn (left
   by an effect, as a trap), one report, and the turn ends. A second abort_open
   finds no open turn. *)
let abort_open () =
  let leave = ref true in
  let pb =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () ->
           if !leave then begin
             leave := false ;
             Effect.perform Leave
           end ;
           (m + 1, Cmd.none) )
         ~view:(fun m -> button () (string_of_int m))
         () )
  in
  let turns = (stats pb).turns in
  R.abort_open E1 ;
  equal int ~msg:"no open turn: no report" 0 (List.length (errors pb)) ;
  equal int ~msg:"no open turn: no turn" turns (stats pb).turns ;
  is_false (ins pb).turn_requested ;
  Effect.Deep.match_with
    (fun () -> P.click pb (By.test_id "b"))
    ()
    { retc= Fun.id
    ; exnc= raise
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Leave ->
              Some (fun (_ : (a, unit) Effect.Deep.continuation) -> ())
          | _ ->
              None ) } ;
  is_true ~msg:"an open turn" (ins pb).in_turn ;
  R.abort_open E1 ;
  R.abort_open E2 ;
  is_false ~msg:"the abort ends the turn" (ins pb).in_turn ;
  is_true ~msg:"one report" (match errors pb with [E1] -> true | _ -> false) ;
  P.click pb (By.test_id "b") ;
  equal string "1" (F.text (P.find pb (By.test_id "b")))

let tests =
  [ group "faults"
      [ test "a fault at each clock read" sweep
      ; test "a fault of the tick post" tick_fault
      ; test "a fault at the first clock read of a turn" first_read_fault
      ; test "a fault of the continuation post" continuation_fault
      ; test "a fault of the microtask request" microtask_fault
      ; test "a fault in a click turn renders in a recovery turn" recovery
      ; test "a second fault in the catch of the turn" double
      ; test "an abort ends the open turns" abort
      ; test "a turn removes its own entry from the open turns" open_turns
      ; test "an abort gives its error one time" abort_once
      ; test "abort_open aborts only an open turn" abort_open ] ]
