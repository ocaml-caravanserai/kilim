(* The subscriptions in the turn (design sections 08 and 11): the pass at the
   render, the early call at the end of a turn that leaves a render pending, the
   start of a key, send and the drop rules, and the error scope. *)

open Windtrap
open Kilim
open Support
module S = Private.Sub

exception Boom

(* A task that never ends. Its end action runs [f] at the cancel of the key. *)
let hold ?(on_end = ignore) () =
  Async.make (fun tok ~ok:_ ~error:_ -> Cancel.on_end tok on_end)

(* An input whose message function raises. A click on it inside a turn puts its
   item in the queue, and the root catch comes when the item leaves the
   queue. *)
let raiser () =
  El.input
    ~at:[At.data "testid" "raise"]
    ~ev:[Ev.check (fun _ -> invalid_arg "a raise")]
    ()

let reraise page = Option.iter (fun p -> P.click p (By.test_id "raise")) !page

(* A send of Sub.run from a raw callback: one microtask turn, and the render at
   the next frame. *)
let raw_send () =
  let h = H.create () in
  let subs _ =
    Sub.run ~key:"r" (fun send ->
        Async.make (fun _ ~ok:_ ~error:_ ->
            H.raw h ~after:10 (fun () -> send 5) ) )
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m n -> (m + n, Cmd.none))
         ~view:(fun m -> button 0 (string_of_int m))
         ~subs () )
  in
  equal int 1 (ins p).sub_keys ;
  equal int 1 (ins p).token_children ;
  P.advance p (Span.ms 10) ;
  equal int 1 (stats p).requests ;
  equal int 2 (stats p).turns ;
  equal int 1 (stats p).messages ;
  equal int ~msg:"no render yet" 1 (stats p).renders ;
  is_true (ins p).pending ;
  P.advance p (Span.ms 6) ;
  equal int 2 (stats p).renders ;
  equal string "<div><button data-testid=\"b\">5</button></div>" (P.html p) ;
  equal int 1 (ins p).token_children ;
  settled p ;
  no_errors p

(* The room of a chat: a change of the room changes the key. *)
type room = {room: string; said: string list}

type rmsg = Room of string | Said of string | Filler | Bye

(* Each start records the room and its send. The end action of a room records
   the room in [ended], then runs [on_end]. *)
let chat ?(control = false) ?(on_end = fun _ -> ()) () =
  let starts = ref [] and ended = ref [] and keep = ref ignore in
  let subs m =
    Sub.batch
      [ ( if control then
            Sub.run ~key:"control" (fun send ->
                keep := send ;
                hold () )
          else Sub.none )
      ; Sub.run ~key:("room/" ^ m.room) (fun send ->
            starts := (m.room, send) :: !starts ;
            hold
              ~on_end:(fun () ->
                ended := m.room :: !ended ;
                on_end m.room )
              () ) ]
  in
  let app =
    App.make
      ~init:(fun () -> ({room= "x"; said= []}, Cmd.none))
      ~update:(fun m -> function
        | Room r ->
            ({m with room= r}, Cmd.none)
        | Said s ->
            ({m with said= s :: m.said}, Cmd.none)
        | Filler ->
            ({m with said= m.said}, Cmd.none)
        | Bye ->
            ({m with said= "bye" :: m.said}, Cmd.none) )
      ~view:(fun m ->
        El.div
          [ El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus (Room "y")] ()
          ; button (Room "z") m.room
          ; txt (String.concat "," m.said) ] )
      ~subs ()
  in
  (app, starts, ended, keep)

let send_of starts room = List.assoc room !starts

(* A change of the room in a non-discrete turn: the early call stops the old
   room at the end of the turn. The new room starts at the frame. *)
let room_change () =
  let app, starts, ended, _ = chat () in
  let p = P.start app in
  equal int 1 (ins p).sub_keys ;
  P.focus p (By.test_id "f") ;
  equal (list string) ["x"] !ended ;
  equal int ~msg:"no key until the render" 0 (ins p).sub_keys ;
  equal int ~msg:"y has not started" 1 (List.length !starts) ;
  is_true (ins p).pending ;
  let requests = (stats p).requests in
  H.raw (P.host p) (fun () -> send_of starts "x" (Said "late")) ;
  P.run p ;
  equal int 0 (ins p).messages_left ;
  equal int ~msg:"a send after the cancel asks for no turn" requests
    (stats p).requests ;
  P.advance p (Span.ms 16) ;
  equal (list string) ["y"; "x"] (List.map fst !starts) ;
  equal int 1 (ins p).sub_keys ;
  equal int 1 (ins p).token_children ;
  quiet p ;
  equal string ~msg:"the late send never reaches update" "y"
    (F.text (P.find p (By.test_id "b"))) ;
  no_errors p

(* With a cost of 20 ms, one raw callback sends a change of the room, two
   fillers, and a message of the old room. The microtask turn processes 3
   messages (at 30, 50 and 70 ms, and 60 ms of busy time is more than 50) and
   stops. The early call stops the old room, thus the continuation drops its
   message. *)
let room_drop () =
  let h = H.create ~cost:20. () in
  let app, starts, ended, keep = chat ~control:true () in
  let p = P.start ~host:h app in
  H.raw h ~after:10 (fun () ->
      !keep (Room "y") ;
      !keep Filler ;
      !keep Filler ;
      send_of starts "x" (Said "x") ) ;
  H.idle ~until:(fun () -> (stats p).stops > 0) h ;
  equal int 1 (stats p).stops ;
  equal int 3 (stats p).messages ;
  equal int 1 (ins p).messages_left ;
  equal float_exact 70. (H.now h) ;
  equal (list string) ["x"] !ended ;
  P.run p ;
  equal int 0 (ins p).messages_left ;
  equal int 1 (stats p).dropped ;
  equal int 3 (stats p).messages ;
  quiet p ;
  equal string ~msg:"Said x never reaches update" "y" (F.text (P.root p)) ;
  no_errors p

(* The end action of the old room sends with the key that stays. The turn
   settles that message: no new request, and no message waits. *)
let end_action_send () =
  let keep = ref ignore in
  let app, _, _, k = chat ~control:true ~on_end:(fun _ -> !keep Bye) () in
  let p = P.start app in
  keep := !k ;
  let requests = (stats p).requests in
  P.focus p (By.test_id "f") ;
  equal int 2 (stats p).messages ;
  equal int 0 (ins p).messages_left ;
  equal int requests (stats p).requests ;
  P.advance p (Span.ms 16) ;
  equal string "ybye" (F.text (P.root p)) ;
  quiet p ;
  no_errors p

(* A key that stops and starts again before the render: the early call stops it,
   does not start it again, and the frame starts it. *)
type tmsg = Toggle

let toggle_key () =
  let starts = ref 0 and tokens = ref [] in
  let subs on =
    if on then
      Sub.run ~key:"k" (fun _ ->
          incr starts ;
          Async.make (fun tok ~ok:_ ~error:_ -> tokens := tok :: !tokens) )
    else Sub.none
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (true, Cmd.none))
         ~update:(fun on Toggle -> (not on, Cmd.none))
         ~view:(fun on ->
           El.input
             ~at:[At.data "testid" "f"; At.title (string_of_bool on)]
             ~ev:[Ev.focus Toggle]
             () )
         ~subs () )
  in
  P.focus p (By.test_id "f") ;
  equal int 0 (ins p).sub_keys ;
  P.focus p (By.test_id "f") ;
  equal int 0 (ins p).sub_keys ;
  equal int 1 !starts ;
  P.advance p (Span.ms 16) ;
  equal int 2 !starts ;
  equal int 1 (ins p).sub_keys ;
  equal (list bool) [false; true] (List.map Cancel.cancelled !tokens) ;
  quiet p ;
  no_errors p

(* Sub.every: a fixed delay. Tick k comes at 100k + (k - 1)c ms: the timer
   starts at the clock read of the drain (c) that runs its continuation, and the
   start turn costs 0. update gets it one more c later, at 100k + (k + 1)c. With
   c = 1/128 ms, tick 10 comes after 1,000 ms. *)
type emsg = Tick of int | Next_gen

let ticker h =
  let ticks = ref [] in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun g -> function
        | Tick k ->
            ticks := (k, H.now h) :: !ticks ;
            (g, Cmd.none)
        | Next_gen ->
            (g + 1, Cmd.none) )
      ~view:(fun g -> button Next_gen (string_of_int g))
      ~subs:(fun g -> Sub.every (Span.ms 100) (Tick g))
      ()
  in
  (app, ticks)

let c = 1. /. 128.

let every () =
  let h = H.create () in
  let app, ticks = ticker h in
  let p = P.start ~host:h app in
  equal int 1 (H.live_timers h) ;
  P.advance p (Span.ms 1000) ;
  equal int 9 (List.length !ticks) ;
  P.advance p (Span.ms 1) ;
  equal int 10 (List.length !ticks) ;
  equal (list float_exact)
    (List.init 10 (fun i ->
         let k = float_of_int (i + 1) in
         (100. *. k) +. ((k +. 1.) *. c) ) )
    (List.rev_map snd !ticks) ;
  P.stop p ;
  equal int 0 (H.live_timers h) ;
  (* A new generation after tick 3: tick 4 carries it, and the timer does not
     start again. *)
  let h = H.create () in
  let app, ticks = ticker h in
  let p = P.start ~host:h app in
  P.advance p (Span.ms 301) ;
  equal int 3 (List.length !ticks) ;
  click p ;
  P.advance p (Span.ms 120) ;
  equal int 1 (H.live_timers h) ;
  equal
    (list (pair int float_exact))
    [(0, 100. +. (2. *. c)); (0, 200. +. (3. *. c)); (0, 300. +. (4. *. c))]
    (List.rev (List.tl !ticks)) ;
  equal (pair int float_exact) (1, 400. +. (5. *. c)) (List.hd !ticks) ;
  equal int 1 (ins p).sub_keys ;
  equal int 1 (ins p).token_children ;
  settled p ;
  no_errors p

(* An auto key with a custom kind. A payload takes the newest message function
   when it leaves the queue. A deliver after the cancel pushes nothing. *)
let custom : string S.kind = S.kind ()

type amsg = Gen of int | Payload of int * string | A_filler | Off

let auto_key () =
  let h = H.create ~cost:20. () in
  let deliver = ref ignore and keep = ref ignore in
  let got = ref [] in
  let subs (g, on) =
    Sub.batch
      [ Sub.run ~key:"control" (fun send ->
            keep := send ;
            hold () )
      ; ( if on then
            S.auto custom ~params:"p"
              ~start:(fun d ->
                deliver := d ;
                hold () )
              (fun s -> Payload (g, s))
          else Sub.none ) ]
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> ((0, true), Cmd.none))
         ~update:(fun (g, on) -> function
           | Gen k ->
               ((k, on), Cmd.none)
           | Payload (k, s) ->
               got := (k, s) :: !got ;
               ((g, on), Cmd.none)
           | A_filler ->
               ((g, on), Cmd.none)
           | Off ->
               ((g, false), Cmd.none) )
         ~view:(fun (g, _) -> button Off (string_of_int g))
         ~subs () )
  in
  (* The turn processes 3 messages and stops. The early call gives the key the
     message function of generation 1. *)
  H.raw h ~after:10 (fun () ->
      !keep (Gen 1) ; !keep A_filler ; !keep A_filler ; !deliver "a" ) ;
  H.idle ~until:(fun () -> (stats p).stops > 0) h ;
  equal int 1 (stats p).stops ;
  equal int 1 (ins p).messages_left ;
  P.run p ;
  equal (list (pair int string)) [(1, "a")] !got ;
  (* In the same way, Off removes the key: the continuation drops the payload
     that waits. *)
  H.raw h (fun () ->
      !keep Off ; !keep A_filler ; !keep A_filler ; !deliver "b" ) ;
  H.idle ~until:(fun () -> (stats p).stops > 1) h ;
  equal int 1 (ins p).sub_keys ;
  equal int 1 (ins p).messages_left ;
  P.run p ;
  equal int 1 (stats p).dropped ;
  equal (list (pair int string)) [(1, "a")] !got ;
  (* A deliver after the cancel pushes nothing. *)
  let requests = (stats p).requests in
  H.raw h (fun () -> !deliver "c") ;
  P.run p ;
  equal int 0 (ins p).messages_left ;
  equal int requests (stats p).requests ;
  equal (list (pair int string)) [(1, "a")] !got ;
  quiet p ;
  no_errors p

(* An error of a key that the early call removed is dropped when it leaves the
   queue. With a cost of 20 ms, the raw callback fails the task of the room "x"
   and sends a change of the room and two fillers. The drain of the error costs
   one c, and then the turn processes 2 messages and stops. *)
let error_dropped () =
  let h = H.create ~cost:20. () in
  let fail = ref ignore and keep = ref ignore in
  let subs m =
    Sub.batch
      [ Sub.run ~key:"control" (fun send ->
            keep := send ;
            hold () )
      ; ( if String.equal m.room "x" then
            Sub.run ~key:"x" (fun _ ->
                Async.make (fun _ ~ok:_ ~error -> fail := error) )
          else Sub.none ) ]
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> ({room= "x"; said= []}, Cmd.none))
         ~update:(fun m -> function
           | Room r -> ({m with room= r}, Cmd.none) | _ -> (m, Cmd.none) )
         ~view:(fun m -> txt m.room)
         ~subs () )
  in
  H.raw h ~after:10 (fun () ->
      !keep (Room "y") ; !keep Filler ; !keep Filler ; !fail Boom ) ;
  H.idle ~until:(fun () -> (stats p).stops > 0) h ;
  equal int 2 (stats p).messages ;
  equal int 2 (ins p).messages_left ;
  P.run p ;
  equal int 1 (stats p).dropped ;
  is_true (ins p).alive ;
  equal int 1 (ins p).sub_keys ;
  quiet p ;
  equal string "<div>y</div>" (P.html p) ;
  no_errors p

(* A new auto key in a non-discrete turn starts at the render, as a new key of
   Sub.run. *)
let auto_late () =
  let h = H.create () in
  let starts = ref 0 in
  let subs on =
    if on then
      S.auto custom ~params:"p"
        ~start:(fun _ -> incr starts ; hold ())
        (fun _ -> Toggle)
    else Sub.none
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (false, Cmd.none))
         ~update:(fun on Toggle -> (not on, Cmd.none))
         ~view:(fun on ->
           El.input
             ~at:[At.data "testid" "f"; At.title (string_of_bool on)]
             ~ev:[Ev.focus Toggle]
             () )
         ~subs () )
  in
  P.focus p (By.test_id "f") ;
  equal int 0 !starts ;
  equal int 0 (ins p).sub_keys ;
  P.advance p (Span.ms 16) ;
  equal int 1 !starts ;
  equal int 1 (ins p).sub_keys ;
  quiet p ;
  no_errors p

(* The 3-render limit: each start sends a message that changes the key. A click
   gives 3 renders (the model gets to 3), the next frame 3 more (to 5), then
   none. *)
type nmsg = Go | Next

let render_cap () =
  let starts = ref 0 in
  let subs n =
    if n >= 0 && n < 5 then
      Sub.run ~key:(string_of_int n) (fun send ->
          incr starts ; send Next ; hold () )
    else Sub.none
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (-1, Cmd.none))
         ~update:(fun n -> function
           | Go -> (0, Cmd.none) | Next -> (n + 1, Cmd.none) )
         ~view:(fun n -> button Go (string_of_int n))
         ~subs () )
  in
  click p ;
  equal int 4 (stats p).renders ;
  equal int 3 (stats p).max_renders ;
  equal int 3 !starts ;
  equal int 0 (ins p).sub_keys ;
  is_true (ins p).pending ;
  P.advance p (Span.ms 16) ;
  equal int 7 (stats p).renders ;
  equal int 5 !starts ;
  equal string "5" (F.text (P.find p (By.test_id "b"))) ;
  quiet p ;
  equal int 7 (stats p).renders ;
  equal int 3 (stats p).max_renders ;
  equal int 0 (ins p).token_children ;
  equal int 0 (ins p).sub_keys ;
  no_errors p

(* The error scope: an error in subs or in a subscription unmounts the app. *)
type smsg = Bad | Fine

let failing ~subs =
  App.make
    ~init:(fun () -> (false, Cmd.none))
    ~update:(fun _ -> function
      | Bad -> (true, Cmd.none) | Fine -> (false, Cmd.none) )
    ~view:(fun b ->
      El.div
        [ button Bad (string_of_bool b)
        ; El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus Bad] () ] )
    ~subs
    ~fallback:(fun _ -> txt "fallback")
    ()

let dead p =
  is_false (ins p).alive ;
  equal int 0 (ins p).token_children ;
  equal int 0 (ins p).sub_keys ;
  equal int 1 (List.length (errors p))

let errors_in_subs () =
  let dup b =
    Sub.batch
      [ Sub.run ~key:"a" (fun _ -> hold ())
      ; (if b then Sub.run ~key:"a" (fun _ -> hold ()) else Sub.none) ]
  in
  (* In the pass of the render: the fallback in the same render. *)
  let p = P.start (failing ~subs:dup) in
  equal int 1 (ins p).token_children ;
  click p ;
  equal string "<div>fallback</div>" (P.html p) ;
  equal int 2 (stats p).renders ;
  dead p ;
  quiet p ;
  (* In the early call: the fallback at the next frame. *)
  let p = P.start (failing ~subs:dup) in
  P.focus p (By.test_id "f") ;
  dead p ;
  is_true (ins p).pending ;
  P.advance p (Span.ms 16) ;
  equal string "<div>fallback</div>" (P.html p) ;
  quiet p ;
  (* A zero span of Sub.every. *)
  let p =
    P.start
      (failing ~subs:(fun b ->
           if b then Sub.every Span.zero Fine else Sub.none ) )
  in
  click p ;
  equal string "<div>fallback</div>" (P.html p) ;
  is_false (ins p).alive ;
  is_true (match errors p with [Invalid_argument _] -> true | _ -> false) ;
  quiet p ;
  (* A task that fails: the error leaves the queue, then the fallback. *)
  let p =
    P.start
      (failing ~subs:(fun _ -> Sub.run ~key:"f" (fun _ -> Async.fail Boom)))
  in
  equal string "<div>fallback</div>" (P.html p) ;
  is_false (ins p).alive ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int 0 (ins p).token_children ;
  quiet p

(* User code in the pass can stop the page or send a fault (a root catch). *)
let guards () =
  (* subs stops the page: the new key does not start, and no DOM stays. *)
  let page = ref None and starts = ref 0 in
  let subs n =
    if n > 0 then Option.iter P.stop !page ;
    Sub.run ~key:(string_of_int n) (fun _ -> incr starts ; hold ())
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n () -> (n + 1, Cmd.none))
         ~view:(fun n -> button () (string_of_int n))
         ~subs () )
  in
  page := Some p ;
  click p ;
  equal int 1 !starts ;
  equal string "<div></div>" (P.html p) ;
  equal int 0 (ins p).token_children ;
  quiet p ;
  (* The end action of a removed key stops the page: the next key does not
     start. *)
  let page = ref None and starts = ref [] in
  let subs n =
    Sub.run ~key:(string_of_int n) (fun _ ->
        starts := n :: !starts ;
        hold ~on_end:(fun () -> Option.iter P.stop !page) () )
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n () -> (n + 1, Cmd.none))
         ~view:(fun n -> button () (string_of_int n))
         ~subs () )
  in
  page := Some p ;
  click p ;
  equal (list int) [0] !starts ;
  equal string "<div></div>" (P.html p) ;
  is_false (ins p).alive ;
  equal int 0 (ins p).token_children ;
  quiet p ;
  no_errors p ;
  (* The function of Sub.run sends a fault (a root catch at once), and its task
     fails at once: the task does not start, thus no error item goes to the
     queue. A stop cannot show this: after a stop, the queue takes nothing. *)
  let f =
    faulty
      ~update:(fun _ n () -> (n + 1, Cmd.none))
      ~view:(fun _ n -> button () (string_of_int n))
      ~subs:(fun fault n ->
        if n > 0 then Sub.run ~key:"k" (fun _ -> fault () ; Async.fail Boom)
        else Sub.none )
      ~fallback:(fun _ -> txt "fallback")
      ()
  in
  fclick f ;
  equal int 0 (R.stats f.rt).dropped ;
  is_true (match H.errors f.h with [Fault] -> true | _ -> false) ;
  equal string "<div>fallback</div>" (F.to_html f.root) ;
  equal int 0 (R.inspect f.rt).continuations_left ;
  fquiet f ;
  (* The function of Sub.run stops the page: no task and no continuation. *)
  let page = ref None in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n () -> (n + 1, Cmd.none))
         ~view:(fun n -> button () (string_of_int n))
         ~subs:(fun n ->
           if n > 0 then
             Sub.run ~key:"k" (fun _ ->
                 Option.iter P.stop !page ;
                 Async.map ignore (sync ()) )
           else Sub.none )
         () )
  in
  page := Some p ;
  click p ;
  equal int 0 (ins p).continuations_left ;
  equal string "<div></div>" (P.html p) ;
  quiet p ;
  no_errors p

(* A task of Sub.run that ended keeps its send. After a root catch or a stop,
   that send pushes nothing: no request, no turn and no drop. *)
let ended_send () =
  let check ~auto kill =
    let saved = ref ignore in
    let h = H.create () in
    let subs _ =
      if auto then
        S.auto custom ~params:"p"
          ~start:(fun d ->
            (saved := fun n -> d (string_of_int n)) ;
            Async.return () )
          int_of_string
      else
        Sub.run ~key:"r" (fun send ->
            saved := send ;
            Async.return () )
    in
    let p =
      P.start ~host:h
        (App.make
           ~init:(fun () -> (0, Cmd.none))
           ~update:(fun m n -> if n = 99 then raise Boom else (m + n, Cmd.none))
           ~view:(fun m -> button 99 (string_of_int m))
           ~subs () )
    in
    equal int 0 (ins p).token_children ;
    kill p ;
    let s = stats p in
    H.raw h (fun () -> !saved 5) ;
    P.run p ;
    equal int ~msg:"no request" s.requests (stats p).requests ;
    equal int ~msg:"no turn" s.turns (stats p).turns ;
    equal int ~msg:"no drop" s.dropped (stats p).dropped ;
    equal int 0 (ins p).messages_left ;
    quiet p
  in
  check ~auto:false click ;
  check ~auto:false P.stop ;
  check ~auto:true click ;
  check ~auto:true P.stop

(* The early call runs only when a render stays pending and the model changed
   since the last pass. *)
type cmsg = Change | Same

let subs_calls () =
  let calls = ref 0 in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n -> function
           | Change -> (n + 1, Cmd.none) | Same -> (n, Cmd.none) )
         ~view:(fun n ->
           El.div
             [ button Change (string_of_int n)
             ; El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus Change] ()
             ; El.input ~at:[At.data "testid" "s"] ~ev:[Ev.focus Same] () ] )
         ~subs:(fun _ -> incr calls ; Sub.none)
         () )
  in
  equal int 1 !calls ;
  (* A click renders: only the pass of the render. *)
  click p ;
  equal int 2 !calls ;
  (* A focus leaves a render pending: the early call. *)
  P.focus p (By.test_id "f") ;
  equal int 3 !calls ;
  (* No change of the model: no call. *)
  P.focus p (By.test_id "s") ;
  equal int 3 !calls ;
  P.advance p (Span.ms 16) ;
  equal int 4 !calls ;
  quiet p ;
  no_errors p

(* A task that ends keeps its key, releases its token, and does not start
   again. *)
let task_ends () =
  let starts = ref 0 in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n () -> (n + 1, Cmd.none))
         ~view:(fun n -> button () (string_of_int n))
         ~subs:(fun _ ->
           Sub.run ~key:"k" (fun _ -> incr starts ; Async.return ()) )
         () )
  in
  equal int 1 (ins p).sub_keys ;
  equal int 0 (ins p).token_children ;
  click p ;
  click p ;
  equal int 1 !starts ;
  equal int 1 (ins p).sub_keys ;
  quiet p ;
  no_errors p

(* The subscriptions on random schedules. The model is a set of keys of Sub.run,
   an optional Sub.every and a generation. A start can send, make a root catch
   or stop the page, and its task can hold, end or fail later. An end action can
   send with its own send.

   The checks record a violation, and never raise inside update.

   A key starts only at a render, with the model of the newest view, and only
   while the root is alive.

   A key has at most one live task, and a live task has a key of the model.

   A message reaches update at most one time, and never after the end action of
   its task. A tick has the generation of the last subs call.

   With no render pending, the recorded keys are the keys of the model.

   After a root catch or a stop, no update, task, timer or DOM stays. *)
type how =
  | Hold
  | Ends
  | Fail_later of int
  | Send_start
  | Raise_start
  | Stop_start
  | End_send

type paction =
  | Change_click of string list * bool * how list
  | Change_focus of string list * bool * how list
  | Change_blur of string list * bool * how list
  | Change_check of string list * bool * how list * bool
      (** A click on a checkbox, prevented if the flag is true. *)
  | Change_compose of string list * bool * how list
  | Send of int
  | Burst
  | Wait of int
  | Raise_subs
  | Stop_page

type pmsg =
  | Changed
  | Sent of int * int  (** The start id and the send id. *)
  | Tick of int

type pmodel = {keys: string list; every: bool; gen: int}

let pp_how = function
  | Hold ->
      "hold"
  | Ends ->
      "ends"
  | Fail_later d ->
      "fail " ^ string_of_int d
  | Send_start ->
      "send"
  | Raise_start ->
      "raise"
  | Stop_start ->
      "stop"
  | End_send ->
      "end send"

let pp_change (l, every, hows) =
  "[" ^ String.concat "; " l ^ "]"
  ^ (if every then " every" else "")
  ^ " ("
  ^ String.concat ", " (List.map pp_how hows)
  ^ ")"

let pp_paction = function
  | Change_click (l, e, h) ->
      "click " ^ pp_change (l, e, h)
  | Change_focus (l, e, h) ->
      "focus " ^ pp_change (l, e, h)
  | Change_blur (l, e, h) ->
      "blur " ^ pp_change (l, e, h)
  | Change_check (l, e, h, prevent) ->
      (if prevent then "prevented check " else "check ") ^ pp_change (l, e, h)
  | Change_compose (l, e, h) ->
      "compose " ^ pp_change (l, e, h)
  | Send i ->
      "send " ^ string_of_int i
  | Burst ->
      "burst"
  | Wait n ->
      "wait " ^ string_of_int n
  | Raise_subs ->
      "raise in subs"
  | Stop_page ->
      "stop"

let gen_keys =
  Gen.map
    (fun l -> List.sort_uniq String.compare l)
    (Gen.list ~size:(Gen.int_range 0 4)
       (Gen.of_list ["k0"; "k1"; "k2"; "k3"; "k4"; "k5"]) )

let gen_how =
  Gen.frequency
    [ (6, Gen.constant Hold)
    ; (2, Gen.constant Ends)
    ; (1, Gen.map (fun d -> Fail_later d) (Gen.int_range 0 30))
    ; (2, Gen.constant Send_start)
    ; (1, Gen.constant Raise_start)
    ; (1, Gen.constant Stop_start)
    ; (2, Gen.constant End_send) ]

let gen_change =
  Gen.triple gen_keys
    (Gen.frequency [(3, Gen.constant false); (1, Gen.constant true)])
    (Gen.list ~size:(Gen.int_range 0 4) gen_how)

let gen_paction =
  Gen.frequency
    [ (3, Gen.map (fun (l, e, h) -> Change_click (l, e, h)) gen_change)
    ; (3, Gen.map (fun (l, e, h) -> Change_focus (l, e, h)) gen_change)
    ; (1, Gen.map (fun (l, e, h) -> Change_blur (l, e, h)) gen_change)
    ; ( 2
      , Gen.map
          (fun ((l, e, h), prevent) -> Change_check (l, e, h, prevent))
          (Gen.pair gen_change Gen.bool) )
    ; (1, Gen.map (fun (l, e, h) -> Change_compose (l, e, h)) gen_change)
    ; (3, Gen.map (fun i -> Send i) (Gen.int_range 0 20))
    ; (1, Gen.constant Burst)
    ; (2, Gen.map (fun n -> Wait n) (Gen.int_range 1 60))
    ; (1, Gen.constant Raise_subs)
    ; (1, Gen.constant Stop_page) ]

let pgen =
  Gen.with_pp
    (fun ppf (c, l) ->
      Format.pp_print_string ppf
        ( "cost " ^ string_of_float c ^ ": "
        ^ String.concat ", " (List.map pp_paction l) ) )
    (Gen.pair
       (Gen.of_list [1. /. 128.; 12.5; 20.])
       (Gen.list ~size:(Gen.int_range 1 30) gen_paction) )

type status = Live | Gone | Ended

let subs_law (cost, actions) =
  let h = H.create ~cost () in
  let page = ref None and stopped = ref false and raise_next = ref false in
  let violations = ref [] in
  let note s = violations := s :: !violations in
  let alive () = match !page with Some p -> (ins p).alive | None -> true in
  (* Each start gets an id, with its key, its status and its send. *)
  let next = ref 0 and tasks = ref [] and hows = Queue.create () in
  let fresh () = incr next ; !next in
  let processed = ref [] and last_pass = ref (-1) in
  let last_view = ref None and updates_since_view = ref 0 in
  let plans = Queue.create () and last = ref {keys= []; every= false; gen= 0} in
  let live k =
    List.filter (fun (_, (k', st, _)) -> String.equal k k' && !st = Live) !tasks
  in
  let start m k send =
    ( match !last_view with
    | Some v when v == m ->
        ()
    | _ ->
        note "a start with a model that is not the newest view" ) ;
    if !updates_since_view > 0 then note "a start after an update with no view" ;
    if not (alive ()) then note "a start after a root catch or a stop" ;
    if live k <> [] then note "two live tasks of one key" ;
    let id = fresh () in
    let st = ref Ended in
    tasks := (id, (k, st, send)) :: !tasks ;
    let how = Option.value ~default:Hold (Queue.take_opt hows) in
    ( match how with
    | Send_start ->
        send (Sent (id, fresh ()))
    | Raise_start ->
        reraise page
    | Stop_start ->
        Option.iter P.stop !page ;
        stopped := true
    | Hold | Ends | Fail_later _ | End_send ->
        () ) ;
    match how with
    | Ends ->
        Async.return ()
    | Fail_later d ->
        (* The task ends with its error, and its send stays valid until the
           error leaves the queue. *)
        Async.make (fun tok ~ok:_ ~error ->
            st := Live ;
            Cancel.on_end tok (fun () -> if !st = Live then st := Gone) ;
            H.raw h ~after:d (fun () ->
                st := Ended ;
                error Boom ) )
    | Hold | Send_start | Raise_start | Stop_start | End_send ->
        Async.make (fun tok ~ok:_ ~error:_ ->
            st := Live ;
            Cancel.on_end tok (fun () ->
                st := Gone ;
                if how = End_send then send (Sent (id, fresh ())) ) )
  in
  let subs m =
    if !raise_next then begin
      raise_next := false ;
      reraise page ;
      raise Boom
    end ;
    last_pass := m.gen ;
    Sub.batch
      [ Sub.batch (List.map (fun k -> Sub.run ~key:k (start m k)) m.keys)
      ; (if m.every then Sub.every (Span.ms 30) (Tick m.gen) else Sub.none) ]
  in
  let update m msg =
    if !stopped then note "an update after a stop" ;
    incr updates_since_view ;
    let m =
      match msg with
      | Changed -> (
        match Queue.take_opt plans with
        | Some (keys, every) ->
            {keys; every; gen= m.gen + 1}
        | None ->
            m )
      | Sent (id, sid) ->
          if List.mem sid !processed then note "a message two times" ;
          processed := sid :: !processed ;
          ( match List.assoc_opt id !tasks with
          | Some (_, st, _) when !st = Gone ->
              note "a message after the end action"
          | _ ->
              () ) ;
          m
      | Tick g ->
          if g <> !last_pass then note "a tick of an older pass" ;
          m
    in
    last := m ;
    (m, Cmd.none)
  in
  let view m =
    last_view := Some m ;
    updates_since_view := 0 ;
    El.div
      [ button Changed (String.concat "," m.keys)
      ; El.input
          ~at:[At.data "testid" "f"]
          ~ev:[Ev.focus Changed; Ev.blur Changed; Ev.input (fun _ -> Changed)]
          ()
      ; El.input
          ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked false]
          ~ev:[Ev.check (fun _ -> Changed)]
          ()
      ; El.input
          ~at:[At.data "testid" "cp"; At.type_ "checkbox"; At.checked false]
          ~ev:[Ev.check ~prevent_default:true (fun _ -> Changed)]
          ()
      ; raiser () ]
  in
  let p =
    P.start ~host:h
      (App.make ~init:(fun () -> (!last, Cmd.none)) ~update ~view ~subs ())
  in
  page := Some p ;
  let change (l, every, hs) f =
    if (ins p).alive then begin
      Queue.clear hows ;
      List.iter (fun x -> Queue.push x hows) hs ;
      Queue.push (l, every) plans ;
      f () ;
      Queue.clear plans
    end
  in
  let send_of (id, (_, _, send)) = send (Sent (id, fresh ())) in
  List.iter
    (fun a ->
      ( match a with
      | Change_click (l, e, hs) ->
          change (l, e, hs) (fun () -> click p)
      | Change_focus (l, e, hs) ->
          change (l, e, hs) (fun () -> P.focus p (By.test_id "f"))
      | Change_blur (l, e, hs) ->
          change (l, e, hs) (fun () -> P.blur p (By.test_id "f"))
      | Change_check (l, e, hs, prevent) ->
          change (l, e, hs) (fun () ->
              P.click p (By.test_id (if prevent then "cp" else "c")) )
      | Change_compose (l, e, hs) ->
          change (l, e, hs) (fun () -> P.compose p (By.test_id "f") ["a"; "ab"])
      | Send i -> (
        match List.nth_opt !tasks (i mod max 1 (List.length !tasks)) with
        | Some t ->
            H.raw h (fun () -> send_of t) ;
            P.run p
        | None ->
            () )
      | Burst ->
          let l = !tasks in
          H.raw h (fun () ->
              List.iter (fun t -> send_of t ; send_of t ; send_of t) l ) ;
          P.run p
      | Wait n ->
          P.advance p (Span.ms n)
      | Raise_subs ->
          raise_next := true
      | Stop_page ->
          P.stop p ;
          stopped := true ) ;
      settled p ;
      let i = ins p in
      if i.alive then begin
        List.iter
          (fun (_, (k, st, _)) ->
            if !st = Live && not (List.mem k !last.keys) then
              note "a live task with a key that is not in the model" )
          !tasks ;
        if (not i.pending) && i.messages_left = 0 then
          equal int ~msg:"the recorded keys"
            (List.length !last.keys + if !last.every then 1 else 0)
            i.sub_keys
      end )
    actions ;
  raise_next := false ;
  (* The timer of Sub.every runs for ever: remove it before idle. *)
  change (!last.keys, false, []) (fun () -> click p) ;
  quiet p ;
  equal (list string) [] (List.sort_uniq String.compare !violations) ;
  let i = ins p in
  if not i.alive then begin
    equal int 0 i.token_children ;
    equal int 0 i.sub_keys ;
    equal int 0 (H.live_timers h) ;
    is_false ~msg:"a live task after the root died"
      (List.exists (fun (_, (_, st, _)) -> !st = Live) !tasks)
  end ;
  if !stopped then
    equal string ~msg:"no DOM after a stop" "<div></div>" (P.html p) ;
  equal int 0 i.messages_left ;
  equal int 0 i.continuations_left

let tests =
  [ group "subscriptions"
      [ test "a send of Sub.run from a raw callback" raw_send
      ; test "a change of the room stops the old room at the end of the turn"
          room_change
      ; test "a message of the old room after a stop at the limit is dropped"
          room_drop
      ; test "the turn settles a send from an end action of the early call"
          end_action_send
      ; test "a key that stops and starts again before the render" toggle_key
      ; test "Sub.every: a fixed delay and the newest message" every
      ; test "an auto key: the newest message function, no deliver after a stop"
          auto_key
      ; test "an error of a removed key is dropped" error_dropped
      ; test "a new auto key starts at the render" auto_late
      ; test "at most 3 renders in one turn with Sub.run" render_cap
      ; test "an error in subs or in a subscription unmounts the app"
          errors_in_subs
      ; test "user code in the pass stops the page or sends a fault" guards
      ; test "the early call runs only for a change and a pending render"
          subs_calls
      ; test "a task that ends keeps its key" task_ends
      ; test "a send of an ended task after a root catch or a stop" ended_send
      ; prop ~count:300 "the subscriptions on random schedules" pgen subs_law ]
  ]
