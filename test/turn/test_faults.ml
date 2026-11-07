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

let tests =
  [ group "faults"
      [ test "a fault at each clock read" sweep
      ; test "a fault of the tick post" tick_fault
      ; test "a fault at the first clock read of a turn" first_read_fault
      ; test "a fault of the continuation post" continuation_fault
      ; test "a fault of the microtask request" microtask_fault
      ; test "a fault in a click turn renders in a recovery turn" recovery
      ; test "a second fault in the catch of the turn" double ] ]
