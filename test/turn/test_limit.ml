(* The time limit on the virtual clock (design section 11). Each number is
   exact, because the cost is a binary fraction. c is the cost of an update and
   of a clock read of the drain. *)

open Windtrap
open Kilim
open Support

type msg = Loop | Finished | Focus | Typed of string | Got of string

(* A loop of messages through microtasks: the [ok] of each task comes from a raw
   microtask. *)
let loop_app ?(spend = 0.) ?(init = true) h =
  let task () =
    Async.make (fun _ ~ok ~error:_ ->
        H.raw_microtask h (fun () ->
            if spend > 0. then H.spend h spend ;
            ok Loop ) )
  in
  App.make
    ~init:(fun () -> (0, if init then Cmd.task (task ()) else Cmd.none))
    ~update:(fun m _ -> (m + 1, Cmd.task (task ())))
    ~view:(fun m -> button Loop (string_of_int m))
    ()

(* A microtask loop with the default cost. Each Micro turn costs 2c, and turn k
   checks (2k - 1)c at its drain. *)
let micro_loop () =
  let h = H.create () in
  let p = P.start ~host:h (loop_app h) in
  equal int 3202 (stats p).turns ;
  equal int 3200 (stats p).messages ;
  equal int 1 (stats p).stops ;
  equal int 1 (stats p).yields ;
  equal float_exact 50.0078125 (H.now h) ;
  equal int 1 (stats p).renders ;
  equal int 1 (stats p).clocks ;
  is_true (ins p).continuing ;
  is_true (ins p).pending ;
  is_true (ins p).tick_pending ;
  (* The tick, then the continuation. *)
  is_true (H.step h) ;
  is_false (ins p).tick_pending ;
  equal int 3200 (stats p).messages ;
  is_true (H.step h) ;
  equal int 6400 (stats p).messages ;
  equal int 2 (stats p).yields ;
  equal int 2 (stats p).stops ;
  equal int 0 (List.length (H.warnings h))

(* The times of the stops, the yields and the warnings, step by step. *)
let trace h p ~until =
  let stops = ref [] and yields = ref [] and warns = ref [] in
  let s0 = ref 0 and y0 = ref 0 and w0 = ref 0 in
  let record () =
    let s = stats p in
    if s.stops > !s0 then stops := H.now h :: !stops ;
    if s.yields > !y0 then yields := H.now h :: !yields ;
    if s.warnings > !w0 then warns := H.now h :: !warns ;
    s0 := s.stops ;
    y0 := s.yields ;
    w0 := s.warnings
  in
  record () ;
  H.idle h ~until:(fun () ->
      record () ;
      until (stats p) ) ;
  (List.rev !stops, List.rev !yields, List.rev !warns)

(* The warning, with c = 1 ms. A frame turn drains the run queue and starts a
   new chain while a continuation is posted, thus every second stop yields. *)
let warning () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (loop_app h) in
  let stops, yields, warns = trace h p ~until:(fun s -> s.yields >= 12) in
  let ms l = List.map (fun k -> Float.of_int (51 * k)) l in
  equal (list float_exact) ~msg:"the stops"
    (ms (List.init 22 (fun k -> k + 1)))
    stops ;
  equal (list float_exact) ~msg:"the yields"
    (ms (1 :: List.init 11 (fun k -> 2 * (k + 1))))
    yields ;
  equal (list float_exact) ~msg:"the warning at the 10th stop" [510.] warns ;
  equal int 22 (stats p).stops ;
  equal int 550 (stats p).messages ;
  equal float_exact 1122. (H.now h) ;
  equal int 1 (stats p).warnings ;
  equal (list string)
    [ "kilim: 10 stops at the limit in 1 s. A loop of messages or tasks blocks \
       the page." ]
    (H.warnings h)

(* Results after idle time: each one gets a new clock, because the tick ran
   during the idle time. *)
let idle_results () =
  let run ~n ~d =
    let h = H.create () in
    let task i =
      Async.make (fun _ ~ok ~error:_ ->
          H.raw h ~after:(i * d) (fun () -> ok Loop) )
    in
    let p =
      P.start ~host:h
        (App.make
           ~init:(fun () ->
             (0, Cmd.batch (List.init n (fun i -> Cmd.task (task (i + 1))))) )
           ~update:(fun m _ -> (m + 1, Cmd.none))
           ~view:(fun m -> txt (string_of_int m))
           () )
    in
    H.idle h ;
    let s = stats p in
    equal int ~msg:"messages" n s.messages ;
    equal int ~msg:"one Micro turn for each result" n s.requests ;
    equal int ~msg:"a new clock in each turn" s.turns s.clocks ;
    equal int ~msg:"stops" 0 s.stops ;
    equal int ~msg:"warnings" 0 s.warnings ;
    s
  in
  List.iter
    (fun d ->
      let s = run ~n:10 ~d in
      equal int ~msg:"the start, 10 Micro turns, 10 frames" 21 s.turns ;
      equal int 11 s.renders )
    [500; 1500; 2000] ;
  equal int 33 (run ~n:500 ~d:1).renders ;
  equal int 64 (run ~n:500 ~d:2).renders

(* [n] steps of Async.make with a synchronous ok. [ran] counts the steps that
   started. The first step starts with the task, and each continuation starts
   the next step. Thus [!ran - 1] continuations ran. *)
let ran = ref 0

let rec chain n =
  if n = 0 then Async.return Finished
  else
    Async.bind
      (Async.make (fun _ ~ok ~error:_ -> incr ran ; ok ()))
      (fun () -> chain (n - 1))

let chain_app ?(steps = 20_000) ?(change = true) task =
  let next m = if change then m + 1 else m in
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m msg ->
      match msg with
      | Loop | Focus ->
          (next m, Cmd.task (task steps))
      | _ ->
          (next m, Cmd.none) )
    ~view:(fun m ->
      El.div
        [ button Loop (string_of_int m)
        ; El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus Focus] () ] )
    ()

(* A synchronous loop in the run queue, with c = 1 ms. *)
let sync_loop () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (chain_app chain) in
  ran := 0 ;
  let reads = H.clock_reads h in
  click p ;
  (* The start, 4 checks, 50 reads of the drain, and the end. *)
  equal int 55 (H.clock_reads h - reads) ;
  equal int ~msg:"no Done in the click turn" 1 (stats p).messages ;
  equal int 1 (stats p).renders ;
  equal int ~msg:"12,544 continuations" (12_544 + 1) !ran ;
  equal int 1 (ins p).continuations_left ;
  equal int 1 (stats p).stops ;
  equal int 1 (stats p).yields ;
  equal float_exact 51. (H.now h) ;
  H.idle h ;
  equal int 20_000 !ran ;
  equal int 0 (ins p).continuations_left ;
  equal int 2 (stats p).messages ;
  equal int 1 (stats p).stops ;
  equal int 1 (stats p).yields ;
  equal int 2 (stats p).renders ;
  equal float_exact 82. (H.now h) ;
  (* Microtasks with no message: each step of 60 comes from a raw microtask. *)
  let h = H.create ~cost:1. () in
  ran := 0 ;
  let rec micro n =
    if n = 0 then Async.return Finished
    else
      Async.bind
        (Async.make (fun _ ~ok ~error:_ ->
             H.raw_microtask h (fun () -> incr ran ; ok ()) ) )
        (fun () -> micro (n - 1))
  in
  let p = P.start ~host:h (chain_app ~steps:60 micro) in
  click p ;
  equal int ~msg:"the start, the click and 50 Micro turns" 52 (stats p).turns ;
  equal int 50 !ran ;
  equal int 1 (stats p).stops ;
  equal int 1 (stats p).yields ;
  equal float_exact 51. (H.now h) ;
  H.idle h ;
  equal int 60 !ran ;
  equal int 1 (stats p).stops ;
  equal int 2 (stats p).messages ;
  equal float_exact 64. (H.now h)

(* A stop with no model change: the frame render writes nothing. *)
let no_change () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (chain_app ~change:false chain) in
  F.reset (P.doc p) ;
  click p ;
  equal int 1 (stats p).stops ;
  is_true (ins p).pending ;
  H.idle h ;
  is_false (ins p).pending ;
  equal int 1 (stats p).renders ;
  equal int 0 (F.total (P.doc p)) ;
  no_errors p

(* A stop at the limit with a request that waits: P.stop empties the request
   queue. With c = 30, the drain read and the update pass 50 ms. *)
let stop_with_request () =
  let h = H.create ~cost:30. () in
  let q = Queue.create () in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.task (waiting q)))
         ~update:(fun m _ -> (m + 1, Cmd.task (Async.return Finished)))
         ~view:(fun m -> txt (string_of_int m))
         () )
  in
  H.run h ;
  H.raw h (fun () -> (Queue.pop q) Loop) ;
  is_true (H.step h) ;
  equal int 1 (stats p).stops ;
  equal int 1 (ins p).requests_left ;
  P.stop p ;
  equal int 0 (ins p).requests_left ;
  H.idle h ;
  equal int 1 (stats p).messages ;
  no_errors p

(* A frame turn that stops at the limit: its callback clears the 100 ms timer,
   and its end schedules the render again. *)
let frame_stop () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (chain_app ~steps:100_000 chain) in
  P.focus p (By.test_id "f") ;
  H.idle h ~until:(fun () -> (stats p).stops >= 4) ;
  equal float_exact 204. (H.now h) ;
  equal int 1 (stats p).renders ;
  equal int 1 (H.live_timers h) ;
  equal int 1 (H.frames h) ;
  is_true (ins p).scheduled ;
  H.idle h ;
  equal int 2 (stats p).renders ;
  equal int 0 (H.live_timers h)

(* The hidden page, with c = 20 ms: a turn that becomes hidden stops at the
   limit, and its end schedules the render with a task. *)
let hidden_yield () =
  let h = H.create ~cost:20. () in
  let p = P.start ~host:h (chain_app ~steps:900 chain) in
  ran := 0 ;
  P.focus p (By.test_id "f") ;
  equal int ~msg:"256 continuations" (256 + 1) !ran ;
  is_true (ins p).pending ;
  equal int 1 (H.frames h) ;
  P.set_hidden p true ;
  equal int ~msg:"512 more continuations" (256 + 512 + 1) !ran ;
  equal int 1 (stats p).renders ;
  equal int 2 (stats p).stops ;
  equal int 1 (stats p).yields ;
  is_true (ins p).scheduled ;
  equal int 0 (H.live_timers h) ;
  P.run p ;
  equal int 900 !ran ;
  equal int 2 (stats p).renders ;
  equal int 2 (stats p).messages ;
  is_false (ins p).pending ;
  settled p

(* A stop leaves the last continuation of "a" in the run queue, then a click
   sends "ab": the result of "a" is dropped. *)
let stale_after_stop () =
  let h = H.create ~cost:20. () in
  let q = Queue.create () and got = ref [] in
  let app =
    App.make
      ~init:(fun () -> ("", Cmd.task (waiting q)))
      ~update:(fun m -> function
        | Typed "a" ->
            ("a", Cmd.latest ~key:"fetch" (sync (Got "a")))
        | Typed s ->
            (s, Cmd.latest ~key:"fetch" (waiting (Queue.create ())))
        | Got r ->
            got := r :: !got ;
            (m, Cmd.none)
        | _ ->
            (m, Cmd.none) )
      ~view:(fun m -> button (Typed "ab") m)
      ()
  in
  let p = P.start ~host:h app in
  (* The tick of the start, then the raw send in a Micro turn. *)
  is_true (H.step h) ;
  H.raw h (fun () -> (Queue.pop q) (Typed "a")) ;
  is_true (H.step h) ;
  equal int 1 (stats p).stops ;
  equal int 1 (ins p).continuations_left ;
  click p ;
  equal int 1 (stats p).dropped ;
  equal (list string) [] !got ;
  equal int 2 (stats p).messages ;
  H.idle h ;
  equal (list string) [] !got ;
  equal int 1 (ins p).latest_keys ;
  no_errors p

(* 10 stops in exactly 1 s give a warning: 10 clicks at 0, 100, ..., 800 and
   1,000 ms, each with a chain that stops 51 ms later. *)
let warning_span () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (chain_app chain) in
  let at t =
    P.advance p (Span.ms (t - int_of_float (H.now h))) ;
    equal float_exact (Float.of_int t) (H.now h) ;
    click p
  in
  List.iter at [0; 100; 200; 300; 400; 500; 600; 700; 800] ;
  P.advance p (Span.ms 100) ;
  equal int 9 (stats p).stops ;
  equal int 0 (stats p).warnings ;
  at 1000 ;
  equal int 10 (stats p).stops ;
  equal float_exact 1051. (H.now h) ;
  equal int 1 (stats p).warnings

(* The ring keeps the last 10 stops: 10 stops in 1,001 ms give no warning, and
   the 11th stop 99 ms later makes 10 stops in 1,000 ms: a warning. *)
let warning_ring () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (chain_app chain) in
  let at t =
    P.advance p (Span.ms (t - int_of_float (H.now h))) ;
    click p
  in
  List.iter at [0; 100; 200; 300; 400; 500; 600; 700; 800; 1001] ;
  equal int 10 (stats p).stops ;
  equal int 0 (stats p).warnings ;
  at 1100 ;
  equal int 11 (stats p).stops ;
  equal int 1 (stats p).warnings

(* A dead root with a pending render: the render after the fallback write does
   not write it again. The fallback function spends 60 ms and sends a message (a
   re-entrant event), thus its turn stops at the limit, and a render is
   pending. *)
let after_fallback () =
  let h = H.create () in
  let page = ref None and calls = ref 0 in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun _ _ -> raise Exit)
      ~view:(fun m -> button Loop (string_of_int m))
      ~fallback:(fun _ ->
        incr calls ;
        H.spend h 60. ;
        Option.iter click !page ;
        button Loop "fallback" )
      ()
  in
  let p = P.start ~host:h app in
  page := Some p ;
  click p ;
  equal int 1 !calls ;
  equal int 1 (stats p).stops ;
  is_true (ins p).pending ;
  quiet p ;
  equal int 1 !calls ;
  equal int 2 (stats p).renders ;
  equal int 1 (stats p).dropped

(* A loop after 2 s also gives one warning. *)
let warning_later () =
  let h = H.create ~cost:1. () in
  let p = P.start ~host:h (loop_app ~init:false h) in
  P.advance p (Span.s 2) ;
  click p ;
  H.idle h ~until:(fun () -> (stats p).stops >= 12) ;
  equal int 1 (stats p).warnings

(* The wall-time cap uses a strict [>]: a drain read at exactly 1,000 ms does
   not stop. *)
let wall_exact () =
  let h = H.create () in
  let c = 1. /. 128. in
  let task =
    Async.make (fun _ ~ok ~error:_ ->
        H.raw_microtask h (fun () ->
            H.spend h (1000. -. (2. *. c)) ;
            ok Finished ) )
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Loop -> (m, Cmd.task task) | _ -> (m + 1, Cmd.none) )
         ~view:(fun m -> button Loop (string_of_int m))
         () )
  in
  click p ;
  equal int 0 (stats p).stops ;
  equal int 2 (stats p).messages ;
  equal float_exact (1000. +. c) (H.now h)

(* The wall-time cap: 5 ms of JS work outside kilim in each step. The busy sum
   stays small, but the wall time grows. *)
let wall_cap () =
  let h = H.create () in
  let p = P.start ~host:h (loop_app ~spend:5. ~init:false h) in
  equal int 0 (stats p).stops ;
  click p ;
  equal int 1 (stats p).stops ;
  equal int 1 (stats p).yields ;
  equal int 200 (stats p).messages ;
  equal int ~msg:"the start, the click and 200 Micro turns" 202 (stats p).turns ;
  equal float_exact 1003.1171875 (H.now h) ;
  equal int 0 (stats p).warnings

let tests =
  [ group "limit"
      [ test "a microtask loop stops at 50 ms and continues" micro_loop
      ; test "10 stops in 1 s give one warning" warning
      ; test "results after idle time get a new clock" idle_results
      ; test "a synchronous loop in the run queue stops one time" sync_loop
      ; test "a hidden turn that stops renders in a task" hidden_yield
      ; test "a stale result after a stop is dropped" stale_after_stop
      ; test "the wall-time cap stops a chain after 1 s" wall_cap
      ; test "a stop with no model change: no DOM write" no_change
      ; test "P.stop empties the request queue after a stop at the limit"
          stop_with_request
      ; test "a frame turn that stops clears its timer" frame_stop
      ; test "10 stops in exactly 1 s give a warning" warning_span
      ; test "a loop after 2 s gives a warning" warning_later
      ; test "the ring keeps the last 10 stops" warning_ring
      ; test "a render after the fallback write writes nothing" after_fallback
      ; test "the wall-time cap at exactly 1 s does not stop" wall_exact ] ]
