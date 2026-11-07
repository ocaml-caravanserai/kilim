(* The run queue: the trampoline, the drain and the push (design sections 11 and
   12). *)

open Windtrap
module Q = Kilim.Private.Run_queue

let make () =
  let asks = ref 0 in
  (Q.create ~schedule:(fun () -> incr asks), asks)

(* Exactly max_depth nested steps run at once, and the next goes on the
   queue. *)
let depth_limit () =
  let q, _ = make () in
  let ran = ref 0 in
  let rec nest n =
    if n > 0 then
      Q.step q (fun () ->
          incr ran ;
          nest (n - 1) )
  in
  nest (Q.max_depth + 1) ;
  equal int Q.max_depth !ran ;
  equal int 1 (Q.length q) ;
  Q.drain q ;
  equal int (Q.max_depth + 1) !ran ;
  equal int 256 Q.max_depth

(* The depth goes back down after a step that raises. *)
let depth_after_raise () =
  let q, _ = make () in
  for _ = 1 to 300 do
    match Q.step q (fun () -> raise Exit) with () -> () | exception Exit -> ()
  done ;
  let ran = ref false in
  Q.step q (fun () -> ran := true) ;
  is_true !ran ;
  equal int 0 (Q.length q)

(* Steps one after another, not nested, all run at once. *)
let sequential_steps () =
  let q, _ = make () in
  let ran = ref 0 in
  for _ = 1 to 1000 do
    Q.step q (fun () -> incr ran)
  done ;
  equal int 1000 !ran ;
  equal int 0 (Q.length q)

let push_asks () =
  let q, asks = make () in
  Q.push q ignore ;
  equal int 1 !asks ;
  Q.push q ignore ;
  equal int 1 !asks ;
  Q.drain q ;
  (* Inside a turn: no ask. *)
  Q.with_turn q (fun () -> Q.push q ignore) ;
  equal int 1 !asks ;
  Q.drain q ;
  (* During a drain outside a turn: no ask. *)
  Q.push q (fun () -> Q.push q ignore) ;
  equal int 2 !asks ;
  Q.drain q ;
  equal int 2 !asks ;
  equal int 0 (Q.length q)

(* The turn ends also when its function raises, and a nested turn keeps the
   outer turn. *)
let turn_ends () =
  let q, asks = make () in
  ( match Q.with_turn q (fun () -> raise Exit) with
  | () ->
      fail "no raise"
  | exception Exit ->
      () ) ;
  Q.push q ignore ;
  equal int 1 !asks ;
  Q.drain q ;
  Q.with_turn q (fun () -> Q.with_turn q ignore ; Q.push q ignore) ;
  equal int 1 !asks ;
  equal int 7 (Q.with_turn q (fun () -> 7))

(* over: no call on an empty queue; a call before the first continuation and
   once every 256 continuations. *)
let drain_over () =
  let q, _ = make () in
  let calls = ref 0 in
  Q.drain ~over:(fun () -> incr calls ; false) q ;
  equal int 0 !calls ;
  let ran = ref 0 in
  for _ = 1 to 600 do
    Q.push q (fun () -> incr ran)
  done ;
  Q.drain ~over:(fun () -> incr calls ; false) q ;
  equal int 600 !ran ;
  equal int 3 !calls ;
  (* over stops before the first continuation. *)
  Q.push q (fun () -> incr ran) ;
  Q.drain ~over:(fun () -> true) q ;
  equal int 600 !ran ;
  equal int 1 (Q.length q) ;
  Q.drain q ;
  (* over stops after exactly 256 continuations. *)
  let ran = ref 0 and calls = ref 0 in
  for _ = 1 to 600 do
    Q.push q (fun () -> incr ran)
  done ;
  Q.drain ~over:(fun () -> incr calls ; !calls >= 2) q ;
  equal int 256 !ran ;
  equal int (600 - 256) (Q.length q)

let drain_reentry () =
  let q, _ = make () in
  let order = Buffer.create 8 in
  Q.push q (fun () ->
      Buffer.add_string order "a" ;
      Q.drain q ;
      Buffer.add_string order "c" ) ;
  Q.push q (fun () -> Buffer.add_string order "b") ;
  Q.drain q ;
  equal string "acb" (Buffer.contents order)

let drain_after_raise () =
  let q, _ = make () in
  Q.push q (fun () -> raise Exit) ;
  let ran = ref false in
  Q.push q (fun () -> ran := true) ;
  (match Q.drain q with () -> fail "no raise" | exception Exit -> ()) ;
  is_true (not !ran) ;
  Q.drain q ;
  is_true !ran ;
  is_true (Q.is_empty q)

(* The clear empties the queue and keeps the depth. A push after the clear asks
   for a turn again. *)
let clear () =
  let q, asks = make () in
  Q.push q ignore ;
  Q.push q ignore ;
  equal int 1 !asks ;
  let ran = ref 0 in
  let rec nest n =
    if n > 0 then
      Q.step q (fun () ->
          incr ran ;
          if n = 1 then begin
            Q.clear q ;
            is_true (Q.is_empty q) ;
            (* The depth stays: this step goes on the queue. *)
            Q.step q (fun () -> incr ran) ;
            equal int 1 (Q.length q)
          end
          else nest (n - 1) )
  in
  nest Q.max_depth ;
  equal int Q.max_depth !ran ;
  equal int ~msg:"the push after the clear asks" 2 !asks ;
  Q.clear q ;
  Q.push q ignore ;
  equal int 3 !asks ;
  Q.drain q ;
  equal int Q.max_depth !ran

(* The reset clears the flags that a JS trap can leave set: the depth, the turn
   and the drain. *)
let reset () =
  (* The depth: after a reset at depth 200, 256 more nested steps run at
     once. *)
  let q, _ = make () in
  let ran = ref 0 in
  let rec nest n =
    if n > 0 then
      Q.step q (fun () ->
          incr ran ;
          if !ran = 200 then Q.reset q ;
          nest (n - 1) )
  in
  nest 1000 ;
  equal int (200 + Q.max_depth) !ran ;
  (* The turn: a push after the reset asks for a turn. *)
  let q, asks = make () in
  Q.with_turn q (fun () -> Q.reset q ; Q.push q ignore) ;
  equal int 1 !asks ;
  (* The drain: a drain after the reset runs. *)
  let q, _ = make () in
  let log = ref [] in
  Q.push q (fun () ->
      log := "a" :: !log ;
      Q.reset q ;
      Q.drain q ) ;
  Q.push q (fun () -> log := "b" :: !log) ;
  Q.drain q ;
  equal (list string) ["a"; "b"] (List.rev !log)

let tests =
  [ group "run queue"
      [ test "exactly 256 nested steps run at once" depth_limit
      ; test "the depth goes back after a step that raises" depth_after_raise
      ; test "1,000 steps one after another run at once" sequential_steps
      ; test "a push asks for one turn only outside a turn and a drain"
          push_asks
      ; test "a turn ends also after a raise, and a nested turn keeps the outer"
          turn_ends
      ; test "the drain reads the clock before the first and every 256"
          drain_over
      ; test "a drain inside a drain does nothing" drain_reentry
      ; test "the drain runs again after an exception" drain_after_raise
      ; test "the clear empties the queue and keeps the depth" clear
      ; test "the reset clears the depth and the flags" reset ] ]
