(* The async tasks (design section 12), on the fake host of fake.ml. *)

open Windtrap
open Kilim
open Async.Syntax
module C = Private.Cancel

let is_error_like f = function Some (Error e) -> f e | _ -> false

let chain_right () =
  Fake.reset () ;
  let rec loop k =
    if k = 0 then Async.return 0
    else
      let* () = Async.return () in
      loop (k - 1)
  in
  is_true (Fake.result (loop 5_000) = Some (Ok 0))

let chain_left () =
  Fake.reset () ;
  let t =
    List.fold_left
      (fun acc _ -> Async.bind acc (fun x -> Async.return (x + 1)))
      (Async.return 0) (List.init 5_000 Fun.id)
  in
  is_true (Fake.result t = Some (Ok 5_000))

let map_fail () =
  Fake.reset () ;
  is_true
    (Fake.result (Async.map (fun x -> x * 2) (Async.return 21)) = Some (Ok 42)) ;
  is_true
    ( Fake.result
        (let+ x = Async.return 1 in
         x + 1 )
    = Some (Ok 2) ) ;
  is_true
    (is_error_like (( = ) Not_found)
       (Fake.result (Async.fail Not_found : int Async.t)) )

let bind_raise () =
  Fake.reset () ;
  let t =
    let* () = Async.return () in
    (failwith "in f" : int Async.t)
  in
  is_true
    (is_error_like (function Failure _ -> true | _ -> false) (Fake.result t))

let bind_cancelled () =
  Fake.reset () ;
  let tok = C.root () and called = ref false in
  C.cancel tok ;
  Fake.start ~tok
    (let* () = Async.return () in
     called := true ;
     Async.return () )
    ~ok:ignore ~error:ignore ;
  is_true (not !called)

let once () =
  Fake.reset () ;
  let handled = ref 0 in
  Private.set_error_handler (fun _ -> incr handled) ;
  let count = ref 0 in
  let r =
    Fake.result
      (let* x = Async.make (fun _ ~ok ~error:_ -> ok 1 ; ok 2) in
       incr count ; Async.return x )
  in
  is_true (r = Some (Ok 1)) ;
  equal int 1 !count ;
  let errs = ref 0 in
  Fake.start
    (Async.make (fun _ ~ok:_ ~error -> error Exit ; error Not_found))
    ~ok:ignore
    ~error:(fun _ -> incr errs) ;
  equal int 1 !errs ;
  (* An end action that calls ok again: the continuation runs one time. *)
  let count = ref 0 in
  Fake.start
    (Async.make (fun tok ~ok ~error:_ ->
         Cancel.on_end tok (fun () -> ok ()) ;
         ok () ) )
    ~ok:(fun () -> incr count)
    ~error:ignore ;
  equal int 1 !count ;
  equal int 0 !handled ;
  Private.set_error_handler ignore

let make_not_started_when_cancelled () =
  Fake.reset () ;
  let calls = ref 0 and tok = C.root () in
  C.cancel tok ;
  Fake.start ~tok
    (Async.make (fun _ ~ok ~error:_ -> incr calls ; ok ()))
    ~ok:ignore ~error:ignore ;
  equal int 0 !calls

let continuation_not_in_callback () =
  Fake.reset () ;
  let inside = ref false and ran = ref false and ran_inside = ref false in
  Fake.start
    (Async.make (fun _ ~ok ~error:_ ->
         inside := true ;
         ok () ;
         inside := false ) )
    ~ok:(fun () ->
      ran := true ;
      if !inside then ran_inside := true )
    ~error:ignore ;
  is_true !ran ;
  is_true (not !ran_inside)

let make_raise () =
  Fake.reset () ;
  let handled = ref [] in
  Private.set_error_handler (fun e -> handled := e :: !handled) ;
  (* Before ok: the error of the step. *)
  is_true
    (is_error_like (( = ) Exit)
       (Fake.result
          (Async.make (fun _ ~ok:_ ~error:_ -> raise Exit) : unit Async.t) ) ) ;
  (* After ok: the error handler. *)
  is_true
    ( Fake.result (Async.make (fun _ ~ok ~error:_ -> ok 7 ; raise Not_found))
    = Some (Ok 7) ) ;
  is_true (!handled = [Not_found]) ;
  (* After a cancel: dropped. *)
  let tok = C.root () and got = ref false in
  Fake.start ~tok
    (Async.make (fun _ ~ok:_ ~error:_ -> C.cancel tok ; raise Exit))
    ~ok:(fun () -> got := true)
    ~error:(fun _ -> got := true) ;
  is_true (not !got) ;
  is_true (!handled = [Not_found]) ;
  Private.set_error_handler ignore

let error_before_cancel () =
  Fake.reset () ;
  let tok = C.root () and got = ref false in
  Fake.turn (fun () ->
      Private.Async.start ~host:Fake.host ~run:(Fake.run ()) tok
        (Async.make (fun _ ~ok:_ ~error -> error Not_found) : unit Async.t)
        ~ok:ignore
        ~error:(fun _ -> got := true) ;
      C.cancel tok ) ;
  is_true (not !got)

let raw_callback () =
  Fake.reset () ;
  let pending = ref ignore and got = ref None in
  Fake.start
    (let* x = Async.make (fun _ ~ok ~error:_ -> pending := fun () -> ok 41) in
     Async.return (x + 1) )
    ~ok:(fun x -> got := Some x)
    ~error:ignore ;
  !pending () ;
  equal int 1 !Fake.schedules ;
  Fake.run_microtasks () ;
  is_true (!got = Some 42)

let end_actions () =
  Fake.reset () ;
  let live = ref 0 in
  ignore
    (Fake.result
       (Async.make (fun tok ~ok ~error:_ ->
            incr live ;
            Cancel.on_end tok (fun () -> decr live) ;
            ok () ) ) ) ;
  equal int 0 !live ;
  ignore
    (Fake.result
       (Async.make (fun tok ~ok ~error:_ ->
            incr live ;
            ok () ;
            Cancel.on_end tok (fun () -> decr live) ) ) ) ;
  equal int 0 !live

(* A cancelled task whose JS callback never runs does not keep its continuation
   alive. *)
let no_leak () =
  Fake.reset () ;
  let held = ref ignore and sink = ref 0 in
  let w = Weak.create 1 in
  let tok = C.root () in
  (fun () ->
    let big = Bytes.make 100_000 'x' in
    Weak.set w 0 (Some big) ;
    Fake.start ~tok
      (Async.make (fun _ ~ok ~error:_ -> held := fun () -> ok ()))
      ~ok:(fun () -> sink := !sink + Char.code (Bytes.get big 0))
      ~error:ignore )
    () ;
  C.cancel tok ;
  Gc.full_major () ;
  is_true (not (Weak.check w 0)) ;
  let (_ : unit -> unit) = Sys.opaque_identity !held in
  ()

(* The tokens of the branches leave the task token when they end. *)
let no_branch_tokens () =
  Fake.reset () ;
  let tok = C.root () in
  let left t =
    Fake.start ~tok t ~ok:ignore ~error:ignore ;
    C.children tok
  in
  equal int 0
    (left
       (let+ _ = Async.return 1 and+ _ = Async.return 2 in
        () ) ) ;
  equal int 0
    (left
       (let+ _ = (Async.fail Exit : int Async.t) and+ _ = Async.return 2 in
        () ) ) ;
  equal int 0
    (left
       (let+ _ = Async.return 2 and+ _ = (Async.fail Exit : int Async.t) in
        () ) ) ;
  equal int 0 (left (Async.first [Async.return 1; Async.return 2])) ;
  equal int 0 (left (Async.first [Async.fail Exit; Async.fail Not_found])) ;
  equal int 0 (left (Async.first [Async.fail Exit; Async.return 3])) ;
  equal int 0 (left (Async.timeout (Span.ms 10) (Async.return 1))) ;
  equal int 0
    (left (Async.timeout (Span.ms 10) (Async.fail Exit : int Async.t))) ;
  equal int 0 (Fake.live_timers ()) ;
  is_true (not (Cancel.cancelled tok))

(* Past the depth limit, the trampoline defers the starts and the results. A
   cancel before the drain: nothing arrives. *)
let deferred_after_cancel () =
  let nest name wrap =
    List.iter
      (fun (seed_name, seed) ->
        Fake.reset () ;
        let t =
          List.fold_left (fun acc _ -> wrap acc) seed (List.init 300 Fun.id)
        in
        let tok = C.root () and got = ref false in
        Fake.turn (fun () ->
            Private.Async.start ~host:Fake.host ~run:(Fake.run ()) tok t
              ~ok:(fun _ -> got := true)
              ~error:(fun _ -> got := true) ;
            C.cancel tok ) ;
        is_true ~msg:(name ^ " over " ^ seed_name) (not !got) )
      [("return", Async.return 7); ("fail", Async.fail Exit)]
  in
  nest "bind" (fun acc -> Async.bind acc (fun x -> Async.return (x + 1))) ;
  nest "first" (fun acc -> Async.first [acc]) ;
  nest "and+" (fun acc ->
      Async.map fst (Async.Syntax.( and+ ) acc (Async.return ())) ) ;
  nest "timeout" (fun acc ->
      Async.map
        (function Ok x -> x | Error _ -> 0)
        (Async.timeout (Span.s 1) acc) )

let both_ok () =
  Fake.reset () ;
  is_true
    ( Fake.result
        (let+ a = Async.return 1 and+ b = Async.return 2 in
         a + b )
    = Some (Ok 3) )

let both_errors () =
  Fake.reset () ;
  let errs = ref [] in
  Fake.start
    (let+ _ = (Async.fail Exit : int Async.t)
     and+ _ = (Async.fail Not_found : int Async.t) in
     () )
    ~ok:ignore
    ~error:(fun e -> errs := e :: !errs) ;
  is_true (!errs = [Exit]) ;
  (* The error of a branch cancels the other branch: its timer goes. *)
  Fake.reset () ;
  let got = ref None in
  Fake.start
    (let+ () = Async.sleep (Span.ms 100)
     and+ () = (Async.fail Exit : unit Async.t) in
     () )
    ~ok:(fun () -> got := Some "ok")
    ~error:(fun _ -> got := Some "error") ;
  is_true (!got = Some "error") ;
  equal int 0 (Fake.live_timers ())

let both_wait () =
  Fake.reset () ;
  let got = ref None in
  Fake.start
    (let+ a = Async.map (fun () -> 1) (Async.sleep (Span.ms 50))
     and+ b = Async.map (fun () -> 2) (Async.sleep (Span.ms 80)) in
     a + b )
    ~ok:(fun x -> got := Some x)
    ~error:ignore ;
  Fake.advance 50. ;
  is_true (!got = None) ;
  Fake.advance 30. ;
  is_true (!got = Some 3)

let sleep () =
  Fake.reset () ;
  let got = ref false in
  Fake.start
    (Async.sleep (Span.ms 300))
    ~ok:(fun () -> got := true)
    ~error:ignore ;
  Fake.advance 299. ;
  is_true (not !got) ;
  Fake.advance 1. ;
  is_true !got ;
  equal int 0 (Fake.live_timers ()) ;
  let tok = C.root () and got = ref false in
  Fake.start ~tok
    (Async.sleep (Span.ms 300))
    ~ok:(fun () -> got := true)
    ~error:ignore ;
  equal int 1 (Fake.live_timers ()) ;
  C.cancel tok ;
  equal int 0 (Fake.live_timers ()) ;
  Fake.advance 1000. ;
  is_true (not !got)

let first () =
  Fake.reset () ;
  let got = ref [] in
  Fake.start
    (Async.first
       [ Async.map (fun () -> "slow") (Async.sleep (Span.ms 500))
       ; Async.map (fun () -> "fast") (Async.sleep (Span.ms 100)) ] )
    ~ok:(fun x -> got := x :: !got)
    ~error:ignore ;
  Fake.advance 100. ;
  is_true (!got = ["fast"]) ;
  equal int 0 (Fake.live_timers ()) ;
  Fake.reset () ;
  let got = ref [] in
  Fake.start
    (Async.first [Async.return 1; Async.return 2])
    ~ok:(fun x -> got := x :: !got)
    ~error:ignore ;
  is_true (!got = [1]) ;
  is_true
    (is_error_like (( = ) Not_found)
       (Fake.result
          (Async.first [Async.fail Exit; Async.fail Not_found] : int Async.t) ) ) ;
  is_true
    (is_error_like
       (function Invalid_argument _ -> true | _ -> false)
       (Fake.result (Async.first [] : int Async.t)) ) ;
  (* One branch fails, the other gives ok: ok. *)
  is_true
    (Fake.result (Async.first [Async.fail Exit; Async.return 3]) = Some (Ok 3))

let timeout () =
  Fake.reset () ;
  let got = ref None in
  Fake.start
    (Async.timeout (Span.ms 200) (Async.sleep (Span.ms 1000)))
    ~ok:(fun r -> got := Some r)
    ~error:ignore ;
  Fake.advance 200. ;
  is_true (!got = Some (Error `Timeout)) ;
  equal int 0 (Fake.live_timers ()) ;
  let got = ref None in
  Fake.start
    (Async.timeout (Span.ms 200)
       (Async.map (fun () -> 5) (Async.sleep (Span.ms 50))) )
    ~ok:(fun r -> got := Some r)
    ~error:ignore ;
  Fake.advance 50. ;
  is_true (!got = Some (Ok 5)) ;
  equal int 0 (Fake.live_timers ()) ;
  (* The error of the task, at once or later: the error, and no timer stays. *)
  is_true
    (is_error_like (( = ) Not_found)
       (Fake.result
          (Async.timeout (Span.s 10) (Async.fail Not_found : int Async.t)) ) ) ;
  equal int 0 (Fake.live_timers ()) ;
  let got = ref None in
  Fake.start
    (Async.timeout (Span.s 10)
       (let* () = Async.sleep (Span.ms 50) in
        (Async.fail Exit : int Async.t) ) )
    ~ok:(fun _ -> got := Some "ok")
    ~error:(fun e -> got := Some (if e = Exit then "Exit" else "other")) ;
  Fake.advance 50. ;
  is_true (!got = Some "Exit") ;
  equal int 0 (Fake.live_timers ()) ;
  (* A cancel of the task token stops the two. *)
  let tok = C.root () and got = ref false in
  Fake.start ~tok
    (Async.timeout (Span.ms 100) (Async.sleep (Span.ms 50)))
    ~ok:(fun _ -> got := true)
    ~error:(fun _ -> got := true) ;
  C.cancel tok ;
  equal int 0 (Fake.live_timers ()) ;
  Fake.advance 200. ;
  is_true (not !got)

(* A host whose set_timer raises: timeout gives one error, and its other branch
   is cancelled. *)
let timer_raises () =
  Fake.reset () ;
  let host =
    { Fake.host with
      Private.Host.set_timer= (fun ~ms:_ _ -> failwith "set_timer") }
  in
  let results = ref [] and tok = C.root () in
  let pending = ref ignore in
  Fake.turn (fun () ->
      Private.Async.start ~host ~run:(Fake.run ()) tok
        (Async.timeout (Span.s 1)
           (Async.make (fun _ ~ok ~error:_ -> pending := fun () -> ok 7)) )
        ~ok:(fun r -> results := `Ok r :: !results)
        ~error:(fun e -> results := `Error e :: !results) ) ;
  !pending () ;
  Fake.run_microtasks () ;
  is_true (List.length !results = 1) ;
  is_true (match !results with [`Error (Failure _)] -> true | _ -> false) ;
  equal int 0 (C.children tok)

let span () =
  equal int 1500 (Span.to_ms (Span.ms 1500)) ;
  equal int 3000 (Span.to_ms (Span.s 3)) ;
  equal int 0 (Span.to_ms Span.zero) ;
  equal int 0 (Span.to_ms (Span.ms 0)) ;
  equal int 0 (Span.to_ms (Span.s 0)) ;
  raises (Invalid_argument "Kilim.Span.ms: a negative span") (fun () ->
      Span.ms (-1) ) ;
  raises (Invalid_argument "Kilim.Span.s: a negative span") (fun () ->
      Span.s (-1) ) ;
  raises (Invalid_argument "Kilim.Span.s: out of range") (fun () ->
      Span.s ((Span.max_ms / 1000) + 1) ) ;
  equal int
    (Span.max_ms / 1000 * 1000)
    (Span.to_ms (Span.s (Span.max_ms / 1000))) ;
  raises (Invalid_argument "Kilim.Span.ms: out of range") (fun () ->
      Span.ms (Span.max_ms + 1) ) ;
  equal int Span.max_ms (Span.to_ms (Span.ms Span.max_ms)) ;
  equal int 1073741823 Span.max_ms ;
  is_true (Span.equal (Span.ms 1000) (Span.s 1)) ;
  is_true (Span.compare (Span.ms 1) (Span.ms 2) < 0)

let tests =
  [ group "async"
      [ test "a right-nested chain" chain_right
      ; test "a left-nested chain" chain_left
      ; test "map, let+ and fail" map_fail
      ; test "an exception in f is the error of the task" bind_raise
      ; test "bind under a cancelled token does not call f" bind_cancelled
      ; test "ok and error have an effect one time only" once
      ; test "make does not call the callback under a cancelled token"
          make_not_started_when_cancelled
      ; test "the continuation never runs inside the callback"
          continuation_not_in_callback
      ; test "an exception from the callback: error, handler or dropped"
          make_raise
      ; test "an error before a cancel does not arrive" error_before_cancel
      ; test "an ok from a raw callback asks for a turn" raw_callback
      ; test "end actions run after ok, also when added after ok" end_actions
      ; test "a cancelled task does not keep its continuation alive" no_leak
      ; test "and+ gives the two results" both_ok
      ; test "the tokens of the branches leave the task token" no_branch_tokens
      ; test "past the depth limit, nothing arrives after a cancel"
          deferred_after_cancel
      ; test "and+: the first error, and a cancel of the other branch"
          both_errors
      ; test "and+ waits for the two branches" both_wait
      ; test "sleep, and a cancel that clears the timer" sleep
      ; test "first" first
      ; test "timeout" timeout
      ; test "a timer that raises in timeout gives one error" timer_raises ]
  ; group "span" [test "Span" span] ]
