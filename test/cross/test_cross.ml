(* Stack depth and the once rule in each build (design section 12). Under
   js_of_ocaml, a chain of 2,000 synchronous steps overflowed the stack before
   the trampoline. *)

open Kilim
open Async.Syntax
module C = Private.Cancel
module Q = Private.Run_queue

let failures = ref 0

let check name ok =
  print_endline ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

let host =
  { Private.Host.now_ms= (fun () -> 0.)
  ; set_timer= (fun ~ms:_ _ () -> ())
  ; request_frame= ignore
  ; queue_microtask= ignore
  ; post_task= ignore }

let result ?(tok = C.root ()) t =
  let q = Q.create ~schedule:ignore in
  let r = ref None in
  ( match
      Q.with_turn q (fun () ->
          Private.Async.start ~host ~run:q tok t
            ~ok:(fun x -> r := Some (Ok x))
            ~error:(fun e -> r := Some (Error e)) ;
          Q.drain q )
    with
  | () ->
      ()
  | exception Stack_overflow ->
      r := Some (Error Stack_overflow) ) ;
  !r

let () =
  let n = 1_000_000 in
  let rec loop k =
    if k = 0 then Async.return 0
    else
      let* () = Async.return () in
      loop (k - 1)
  in
  check "a right-nested chain of 1,000,000 steps" (result (loop n) = Some (Ok 0)) ;
  let left =
    List.fold_left
      (fun acc _ -> Async.bind acc (fun x -> Async.return (x + 1)))
      (Async.return 0)
      (List.init n (fun i -> i))
  in
  check "a left-nested chain of 1,000,000 steps" (result left = Some (Ok n)) ;
  let count = ref 0 in
  let r =
    result
      (let* x = Async.make (fun _ ~ok ~error:_ -> ok 1 ; ok 2) in
       incr count ; Async.return x )
  in
  check "once: the second ok does nothing" (r = Some (Ok 1) && !count = 1) ;
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
      (Async.return 0)
      (List.init m (fun i -> i))
  in
  let tok = C.root () in
  check "100,000 nested and+: the start" (result ~tok t = None) ;
  check "100,000 nested and+: the cancel"
    (match C.cancel tok with () -> true | exception Stack_overflow -> false) ;
  (* An error through 100,000 nested and+, and an ok through 100,000 nested
     first: the results go through the trampoline. *)
  let t =
    List.fold_left
      (fun acc _ ->
        let+ x, _ =
          let+ x = acc and+ y = Async.return () in
          (x, y)
        in
        x )
      (Async.fail Exit : int Async.t)
      (List.init m (fun i -> i))
  in
  check "an error through 100,000 nested and+" (result t = Some (Error Exit)) ;
  let t =
    List.fold_left
      (fun acc _ -> Async.first [acc])
      (Async.return 7)
      (List.init m (fun i -> i))
  in
  check "an ok through 100,000 nested first" (result t = Some (Ok 7)) ;
  let t =
    List.fold_left
      (fun acc _ -> Async.first [acc])
      (Async.fail Exit : int Async.t)
      (List.init m (fun i -> i))
  in
  check "an error through 100,000 nested first" (result t = Some (Error Exit)) ;
  (* An ok and an error through 100,000 nested timeout, and an ok through
     100,000 nested and+ with no let+ around a level: each result goes through
     the trampoline. *)
  let rec nest_timeout : type a. int -> a Async.t -> unit Async.t =
   fun k t ->
    if k = 0 then Async.map ignore t
    else
      nest_timeout (k - 1)
        (Async.map (fun _ -> ()) (Async.timeout (Span.s 1) t))
  in
  check "an ok through 100,000 nested timeout"
    (result (nest_timeout m (Async.return 7)) = Some (Ok ())) ;
  check "an error through 100,000 nested timeout"
    (result (nest_timeout m (Async.fail Exit : int Async.t)) = Some (Error Exit)) ;
  let rec nest_both : type a. int -> a Async.t -> unit Async.t =
   fun k t ->
    if k = 0 then Async.map ignore t
    else nest_both (k - 1) (Async.Syntax.( and+ ) t (Async.return ()))
  in
  check "an ok through 100,000 nested and+ with no let+"
    (result (nest_both m (Async.return 7)) = Some (Ok ())) ;
  if !failures > 0 then exit 1
