(* The virtual host of kilim.testing (design section 14): one fixed case for
   each order rule. *)

open Windtrap
open Kilim
module H = Kilim_testing.Host

let log () =
  let l = ref [] in
  ((fun s () -> l := s :: !l), fun () -> List.rev !l)

let timer h ms f =
  let (_ : unit -> unit) = (H.host h).set_timer ~ms f in
  ()

(* By due time, then by order of creation. *)
let timer_order () =
  let h = H.create () in
  let add, got = log () in
  timer h 20 (add "a") ;
  timer h 10 (add "b") ;
  timer h 20 (add "c") ;
  timer h 10 (add "d") ;
  equal int 4 (H.live_timers h) ;
  H.advance h (Span.ms 15) ;
  equal (list string) ["b"; "d"] (got ()) ;
  equal int 2 (H.live_timers h) ;
  equal float_exact 15. (H.now h) ;
  H.advance h (Span.ms 15) ;
  equal (list string) ["b"; "d"; "a"; "c"] (got ()) ;
  equal int 0 (H.live_timers h)

(* The tasks in FIFO order, and the microtasks after each one. A due timer goes
   to the end of the FIFO at the start of a step: before the task that the head
   posts. *)
let fifo () =
  let h = H.create () in
  let add, got = log () in
  let host = H.host h in
  host.post_task (fun () ->
      add "t1" () ;
      host.queue_microtask (add "m1") ;
      host.post_task (add "t3") ) ;
  timer h 0 (add "timer") ;
  host.post_task (add "t2") ;
  is_true (H.step h) ;
  equal (list string) ["t1"; "m1"] (got ()) ;
  H.run h ;
  equal (list string) ["t1"; "m1"; "t2"; "timer"; "t3"] (got ()) ;
  is_false (H.step h)

(* A microtask that waits runs before the next macrotask. *)
let micro_first () =
  let h = H.create () in
  let add, got = log () in
  (H.host h).post_task (add "task") ;
  H.raw_microtask h (fun () ->
      add "m1" () ;
      H.raw_microtask h (add "m2") ) ;
  is_true (H.step h) ;
  equal (list string) ["m1"; "m2"; "task"] (got ()) ;
  is_false (H.step h) ;
  H.raw_microtask h (add "m3") ;
  H.microtasks h ;
  equal (list string) ["m1"; "m2"; "task"; "m3"] (got ())

(* A frame at 16 runs the waiting callbacks. A request at 16 runs at 32. *)
let frames () =
  let h = H.create () in
  let host = H.host h in
  let at = ref [] in
  let mark () = at := H.now h :: !at in
  host.request_frame mark ;
  host.request_frame mark ;
  equal int 2 (H.frames h) ;
  H.advance h (Span.ms 16) ;
  equal (list float_exact) [16.; 16.] !at ;
  equal int 0 (H.frames h) ;
  host.request_frame mark ;
  H.advance h (Span.ms 100) ;
  equal (list float_exact) [32.; 16.; 16.] !at ;
  equal float_exact 116. (H.now h) ;
  (* A request during a frame waits for the next frame. *)
  at := [] ;
  host.request_frame (fun () -> mark () ; host.request_frame mark) ;
  H.advance h (Span.ms 20) ;
  equal (list float_exact) [128.] !at ;
  H.advance h (Span.ms 20) ;
  equal (list float_exact) [144.; 128.] !at ;
  (* Another frame time. *)
  let h = H.create ~frame_ms:1000 () in
  let ran = ref false in
  (H.host h).request_frame (fun () -> ran := true) ;
  H.advance h (Span.ms 999) ;
  is_false !ran ;
  H.advance h (Span.ms 1) ;
  is_true !ran ;
  raises_match (Exn_match.invalid_arg "frame_ms") (fun () ->
      H.create ~frame_ms:0 () )

let hidden () =
  let h = H.create () in
  let ran = ref 0 in
  H.set_hidden h true ;
  is_true (H.hidden h) ;
  (H.host h).request_frame (fun () -> incr ran) ;
  H.advance h (Span.ms 100) ;
  equal int 0 !ran ;
  equal int 1 (H.frames h) ;
  (* idle does not wait for a frame of a hidden page. *)
  H.idle h ;
  equal int 0 !ran ;
  H.set_hidden h false ;
  is_false (H.hidden h) ;
  H.run h ;
  equal int 1 !ran ;
  equal float_exact 100. (H.now h)

let cleared () =
  let h = H.create () in
  let ran = ref [] in
  let host = H.host h in
  let c1 = host.set_timer ~ms:10 (fun () -> ran := "a" :: !ran) in
  c1 () ;
  c1 () ;
  (* The two timers at 20 go to the FIFO together. The first one clears the
     second one. *)
  let c3 = ref ignore in
  timer h 20 (fun () ->
      ran := "b" :: !ran ;
      !c3 () ) ;
  c3 := host.set_timer ~ms:20 (fun () -> ran := "c" :: !ran) ;
  equal int 2 (H.live_timers h) ;
  H.advance h (Span.ms 20) ;
  equal (list string) ["b"] !ran ;
  equal int 0 (H.live_timers h) ;
  !c3 () ;
  equal int 0 (H.live_timers h)

let stops_at_target () =
  let h = H.create () in
  let ran = ref 0 in
  timer h 50 (fun () -> incr ran) ;
  H.advance h (Span.ms 49) ;
  equal int 0 !ran ;
  equal float_exact 49. (H.now h) ;
  H.advance h (Span.ms 1) ;
  equal int 1 !ran ;
  equal float_exact 50. (H.now h) ;
  H.advance h Span.zero ;
  equal float_exact 50. (H.now h)

let raw () =
  let h = H.create () in
  let ran = ref [] in
  H.raw h ~after:10 (fun () ->
      ran := H.now h :: !ran ;
      raise Exit ) ;
  H.raw h (fun () -> ran := H.now h :: !ran) ;
  equal int 0 (H.live_timers h) ;
  H.raw_microtask h (fun () -> raise Not_found) ;
  H.idle h ;
  equal (list float_exact) [10.; 0.] !ran ;
  equal (list string)
    ["Not_found"; "Stdlib.Exit"]
    (List.map Printexc.to_string (H.errors h))

(* create installs the error handler and the warning handler. *)
let handlers () =
  let h = H.create () in
  let tok = Private.Cancel.root () in
  Cancel.on_end tok (fun () -> raise Exit) ;
  Private.Cancel.cancel tok ;
  equal int 1 (List.length (H.errors h)) ;
  equal (list string) [] (H.warnings h)

let idle_limit () =
  let h = H.create () in
  let rec again () = (H.host h).post_task again in
  again () ;
  raises_match (Exn_match.failure "limit") (fun () -> H.idle ~limit:100 h) ;
  let h = H.create () in
  timer h 5 ignore ;
  timer h 9 ignore ;
  H.idle ~limit:5 h ;
  equal float_exact 9. (H.now h) ;
  let h = H.create () in
  timer h 5 ignore ;
  timer h 9 ignore ;
  raises_match (Exn_match.failure "limit") (fun () -> H.idle ~limit:4 h)

(* The cost of the env, spend, the clock reads and the faults. *)
let cost () =
  let h = H.create () in
  let env = H.env h in
  env.cost Private.Runtime.Update ;
  env.cost Private.Runtime.Drain ;
  equal float_exact (2. /. 128.) (H.now h) ;
  let h = H.create ~cost:50. () in
  (H.env h).cost Private.Runtime.Update ;
  equal float_exact 50. (H.now h) ;
  H.spend h 0.5 ;
  equal float_exact 50.5 (H.now h) ;
  List.iter
    (fun cost ->
      raises_match (Exn_match.invalid_arg "cost") (fun () -> H.create ~cost ()) )
    [0.; 50.5; -1.; Float.nan]

let faults () =
  let h = H.create () in
  let host = H.host h in
  equal int 0 (H.clock_reads h) ;
  ignore (host.now_ms ()) ;
  H.fail_clock h 2 ;
  ignore (host.now_ms ()) ;
  raises H.Fault (fun () -> host.now_ms ()) ;
  ignore (host.now_ms ()) ;
  equal int 4 (H.clock_reads h) ;
  let ran = ref 0 in
  H.fail_post_task h 1 ;
  raises H.Fault (fun () -> host.post_task (fun () -> incr ran)) ;
  host.post_task (fun () -> incr ran) ;
  H.fail_microtask h 2 ;
  host.queue_microtask (fun () -> incr ran) ;
  raises H.Fault (fun () -> host.queue_microtask (fun () -> incr ran)) ;
  host.queue_microtask (fun () -> incr ran) ;
  H.idle h ;
  equal int ~msg:"a fault has no effect" 3 !ran

let tests =
  [ group "host"
      [ test "timers run by due time, then by order of creation" timer_order
      ; test "macrotasks in FIFO order, microtasks after each" fifo
      ; test "the waiting microtasks run before a macrotask" micro_first
      ; test "frames at multiples of frame_ms after the request" frames
      ; test "no frame runs while hidden" hidden
      ; test "a cleared timer does not run, also in the FIFO" cleared
      ; test "advance stops at the target" stops_at_target
      ; test "a raw callback that raises goes to errors" raw
      ; test "create installs the handlers" handlers
      ; test "idle stops after the limit" idle_limit
      ; test "the cost of the env and spend" cost
      ; test "the clock reads and the faults" faults ] ]
