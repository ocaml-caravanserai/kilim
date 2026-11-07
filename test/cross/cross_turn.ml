(* The big loops of the turn in each build (design sections 11 and 14), on the
   virtual host of kilim.testing. Each number is exact, because the cost c (of
   an update and of a clock read of the drain) is a binary fraction. *)

open Kilim
module H = Kilim_testing.Host
module P = Kilim_testing.Page

type msg = Loop | Finished | Clicked

let button = El.button ~at:[At.data "testid" "b"] ~ev:[Ev.click Clicked] []

let click p = P.click p (Kilim_testing.By.test_id "b")

(* A microtask loop with the default cost c = 1/128 ms. Each Micro turn costs
   2c: one clock read of the drain and one update. The drain read of turn 3,201
   sees 3,200 x 2c + c = 50.0078125 ms, thus each chain stops after 3,200
   messages, and the stop k is at k x 50.0078125 ms. The first two stops post a
   continuation. Then a frame turn drains the run queue and starts a new chain
   while a continuation is posted, thus only every second stop posts one. The
   10th stop gives the warning: 10 stops in 450 ms. At 30 yields: stop 58, 58 x
   3,200 messages, 58 x 50.0078125 ms. *)
let micro_loop check =
  let h = H.create () in
  let task () =
    Async.make (fun _ ~ok ~error:_ -> H.raw_microtask h (fun () -> ok Loop))
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.task (task ())))
         ~update:(fun m _ -> (m + 1, Cmd.task (task ())))
         ~view:(fun _ -> button)
         () )
  in
  let stops = ref [] and yields = ref [] and warns = ref [] in
  let s0 = ref 0 and y0 = ref 0 and w0 = ref 0 in
  let record () =
    let s = P.stats p in
    if s.stops > !s0 then stops := H.now h :: !stops ;
    if s.yields > !y0 then yields := s.stops :: !yields ;
    if s.warnings > !w0 then warns := H.now h :: !warns ;
    s0 := s.stops ;
    y0 := s.yields ;
    w0 := s.warnings
  in
  record () ;
  H.idle h ~until:(fun () -> record () ; (P.stats p).yields >= 30) ;
  let s = P.stats p in
  check "the stops at k x 50.0078125 ms"
    (List.rev !stops = List.init 58 (fun k -> float (k + 1) *. 50.0078125)) ;
  check "the yields at the stops 1, 2, 4, 6 and so on"
    (List.rev !yields = 1 :: List.init 29 (fun k -> 2 * (k + 1))) ;
  check "the warning at the 10th stop, at 500.078125 ms" (!warns = [500.078125]) ;
  check "58 stops and 185,600 messages at 30 yields"
    (s.stops = 58 && s.messages = 185_600 && s.warnings = 1) ;
  check "the clock at 30 yields" (H.now h = 2900.453125)

(* [n] steps of Async.make with a synchronous ok. [ran] counts the steps that
   started: the first step starts with the task. *)
let ran = ref 0

let rec chain n =
  if n = 0 then Async.return Finished
  else
    Async.bind
      (Async.make (fun _ ~ok ~error:_ -> incr ran ; ok ()))
      (fun () -> chain (n - 1))

(* A synchronous loop in the run queue, with c = 1/16 ms. The click update costs
   c. The drain read k sees (k + 1)c, which is more than 50 ms at k = 800, after
   799 x 256 = 204,544 continuations, at 50.0625 ms. The continuation turn has a
   new clock: it runs the other 95,456 continuations with 373 drain reads, then
   one update: 50.0625 + 374c = 73.4375 ms. *)
let sync_loop check =
  let h = H.create ~cost:(1. /. 16.) () in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Clicked ->
               (m + 1, Cmd.task (chain 300_000))
           | _ ->
               (m + 1, Cmd.none) )
         ~view:(fun _ -> button)
         () )
  in
  ran := 0 ;
  click p ;
  let s = P.stats p in
  check "the click turn stops after 204,544 continuations"
    (!ran = 204_544 + 1 && s.stops = 1 && s.yields = 1 && H.now h = 50.0625) ;
  H.idle h ;
  let s = P.stats p in
  check "the continuation runs the rest, with 1 yield"
    ( !ran = 300_000 && s.stops = 1 && s.yields = 1 && s.messages = 2
    && H.now h = 73.4375 )

(* 10,000 tasks from init with synchronous results. *)
let init_tasks check =
  let h = H.create () in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () ->
           ( 0
           , Cmd.batch
               (List.init 10_000 (fun _ ->
                    Cmd.task (Async.make (fun _ ~ok ~error:_ -> ok Loop)) ) ) ) )
         ~update:(fun m _ -> (m + 1, Cmd.none))
         ~view:(fun _ -> button)
         () )
  in
  let s = P.stats p in
  check "the start turn stops after 6,361 messages at 50.0078125 ms"
    (s.messages = 6361 && s.stops = 1 && H.now h = 50.0078125) ;
  H.idle h ;
  let s = P.stats p in
  check "the continuation processes 3,639 messages"
    ( s.messages = 10_000 && s.stops = 1 && s.yields = 1 && s.warnings = 0
    && H.now h = 78.4375 )

(* A deep batch and a deep map. *)
let deep check =
  let n = 100_000 in
  let left =
    List.fold_left
      (fun acc i -> Cmd.batch [acc; Cmd.task (Async.return i)])
      Cmd.none
      (List.init n (fun i -> i))
  in
  let reqs = Private.Cmd.requests left in
  let host = H.host (H.create ()) in
  let values reqs =
    List.map
      (fun (Private.Cmd.Request {task; to_msg; _}) ->
        let r = ref (-1) in
        Private.Async.start ~host
          ~run:(Private.Run_queue.create ~schedule:ignore)
          (Private.Cancel.root ()) task
          ~ok:(fun x -> r := to_msg x)
          ~error:raise ;
        !r )
      reqs
  in
  check "a left-nested batch of 100,000" (values reqs = List.init n (fun i -> i)) ;
  let nested =
    List.fold_left
      (fun acc _ -> Cmd.map (fun x -> x + 1) acc)
      (Cmd.task (Async.return 0))
      (List.init n (fun i -> i))
  in
  check "100,000 nested maps" (values (Private.Cmd.requests nested) = [n])

(* 100,000 raw sends from one raw callback at 10 ms, with c = 1/128 ms. One
   Micro turn with a new clock: the drain runs the 100,000 continuations with
   391 reads (391c), then the messages. Message n passes the limit when (391 +
   n)c > 50 ms: the first stop is after 6,010 messages, at 60.0078125 ms. Each
   continuation has a new clock and stops after 6,401 messages (6,401c =
   50.0078125 ms). After 15 stops, 4,376 messages stay: the end is at
   760.1171875 + 4,376c = 794.3046875 ms. 4 of the 15 stops come in a frame turn
   while a continuation is posted, thus 11 yields. The 18 turns: the start, the
   Micro turn, 11 continuations and 5 frame turns. *)
let burst check =
  let h = H.create () in
  let q = Queue.create () in
  let waiting = Async.make (fun _ ~ok ~error:_ -> Queue.push ok q) in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () ->
           (0, Cmd.batch (List.init 100_000 (fun _ -> Cmd.task waiting))) )
         ~update:(fun m _ -> (m + 1, Cmd.none))
         ~view:(fun _ -> button)
         () )
  in
  H.raw h ~after:10 (fun () -> Queue.iter (fun ok -> ok Loop) q) ;
  let first = ref None and warn = ref None in
  H.idle h ~until:(fun () ->
      let s = P.stats p in
      if s.stops = 1 && Option.is_none !first then
        first := Some (s.messages, H.now h) ;
      if s.warnings = 1 && Option.is_none !warn then
        warn := Some (s.stops, H.now h) ;
      false ) ;
  let s = P.stats p in
  check "the first stop after 6,010 messages at 60.0078125 ms"
    (!first = Some (6010, 60.0078125)) ;
  check "the warning at the 10th stop, at 510.078125 ms"
    (!warn = Some (10, 510.078125)) ;
  check "15 stops, 11 yields and 1 warning"
    ( s.messages = 100_000 && s.stops = 15 && s.yields = 11 && s.warnings = 1
    && s.requests = 1 && s.turns = 18 ) ;
  check "the clock at the end" (H.now h = 794.3046875)

(* A left-nested batch of 100,000 subscriptions: the walk uses an explicit
   stack. *)
let deep_subs check =
  let n = 100_000 in
  let left = ref Sub.none in
  for i = 0 to n - 1 do
    left :=
      Sub.batch
        [!left; Sub.run ~key:(string_of_int i) (fun _ -> Async.return ())]
  done ;
  let keys =
    List.map
      (function
        | Private.Sub.Run_leaf (k, _) -> k | Private.Sub.Auto_leaf _ -> "" )
      (Private.Sub.leaves !left)
  in
  check "a left-nested batch of 100,000 subscriptions"
    (List.length keys = n && List.nth keys (n - 1) = string_of_int (n - 1))

(* 10,000 keys of Sub.run start in one render and stop in the next. *)
let many_keys check =
  let never = Async.make (fun _ ~ok:_ ~error:_ -> ()) in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (true, Cmd.none))
         ~update:(fun _ _ -> (false, Cmd.none))
         ~view:(fun _ -> button)
         ~subs:(fun on ->
           if on then
             Sub.batch
               (List.init 10_000 (fun i ->
                    Sub.run ~key:(string_of_int i) (fun _ -> never) ) )
           else Sub.none )
         () )
  in
  let started = (P.inspect p).token_children in
  click p ;
  let i = P.inspect p in
  check "10,000 keys start in one render and stop in the next"
    (started = 10_000 && i.token_children = 0 && i.sub_keys = 0)

let run check =
  deep_subs check ;
  many_keys check ;
  micro_loop check ;
  sync_loop check ;
  init_tasks check ;
  deep check ;
  burst check
