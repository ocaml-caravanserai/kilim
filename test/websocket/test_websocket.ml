(* The tests of kilim.websocket with Websocket_mock (design sections 08 and 14),
   on the virtual clock. *)

open Windtrap
open Kilim
module W = Kilim_websocket
module H = Kilim_testing.Host
module P = Kilim_testing.Page
module By = Kilim_testing.By
module M = Kilim_testing.Websocket_mock

let show = function
  | W.Opened _ ->
      "open"
  | Message m ->
      "msg " ^ m
  | Closed {code; reason; clean} ->
      "closed " ^ string_of_int code ^ " " ^ reason ^ " " ^ string_of_bool clean
  | Failed ->
      "failed"

type msg =
  | Got of W.event
  | Send of string
  | Send_first
  | Sent of string
  | Close
  | Closing
  | Retry
  | Toggle

(* [first]: the connection of the first Opened. *)
type model = {conn: W.conn option; first: W.conn option; id: int; on: bool}

(* The events and the results of the tasks, the newest first. *)
let log = ref []

let events () = List.rev !log

let add s = log := s :: !log

(* The app: one connection to [url] while [on]. With [~retry], a close gives a
   new [~id] one second later. *)
let app ?(url = "/ws") ?(retry = false) () =
  log := [] ;
  App.make
    ~init:(fun () -> ({conn= None; first= None; id= 0; on= true}, Cmd.none))
    ~update:(fun m -> function
      | Got e -> (
          add (show e) ;
          match e with
          | Opened c ->
              let first = if m.first = None then Some c else m.first in
              ({m with conn= Some c; first}, Cmd.none)
          | Closed _ when retry ->
              ( m
              , Cmd.task
                  (Async.map (fun () -> Retry) (Async.sleep (Span.ms 1000))) )
          | _ ->
              (m, Cmd.none) )
      | Send t ->
          ( m
          , Cmd.task
              (Async.map
                 (fun r ->
                   Sent (match r with Ok () -> "ok" | Error W.Not_open -> "no") )
                 (W.send (Option.get m.conn) t) ) )
      | Send_first ->
          ( m
          , Cmd.task
              (Async.map
                 (fun r ->
                   Sent (match r with Ok () -> "ok" | Error W.Not_open -> "no") )
                 (W.send (Option.get m.first) "old") ) )
      | Sent s ->
          add ("sent " ^ s) ;
          (m, Cmd.none)
      | Close ->
          ( m
          , Cmd.task
              (Async.map (fun () -> Closing) (W.close (Option.get m.conn))) )
      | Closing ->
          add "closing" ; (m, Cmd.none)
      | Retry ->
          add "retry" ;
          ({m with id= m.id + 1}, Cmd.none)
      | Toggle ->
          ({m with on= not m.on}, Cmd.none) )
    ~view:(fun _ ->
      El.div
        [ El.button ~ev:[Ev.click (Send "hi")] [txt "send"]
        ; El.button ~ev:[Ev.click Send_first] [txt "send first"]
        ; El.button ~ev:[Ev.click Close] [txt "close"]
        ; El.button ~ev:[Ev.click Toggle] [txt "toggle"] ] )
    ~subs:(fun m ->
      if m.on then W.connect ~id:(string_of_int m.id) url (fun e -> Got e)
      else Sub.none )
    ()

let click p s = P.click p (By.text s)

let no_errors p =
  equal int ~msg:"no error" 0 (List.length (H.errors (P.host p)))

(* The page of [app], and the stop at the end, also after a failure. *)
let with_page ?url ?retry f =
  let p = P.start (app ?url ?retry ()) in
  Fun.protect ~finally:(fun () -> P.stop p) (fun () -> f p)

let open_send_push () =
  with_page (fun p ->
      let s = M.server p "/ws" in
      equal int ~msg:"connecting" 1 (M.connections s) ;
      equal int ~msg:"live" 1 (W.Private.live ()) ;
      M.push s "early" ;
      P.run p ;
      equal (list string) ~msg:"no message before the open" [] (events ()) ;
      M.accept s ;
      equal (list string) ~msg:"a macrotask" [] (events ()) ;
      P.run p ;
      M.accept s ;
      P.run p ;
      equal (list string) ~msg:"one open" ["open"] (events ()) ;
      click p "send" ;
      P.run p ;
      equal (list string) ["hi"] (M.received s) ;
      M.push s "a" ;
      M.push s "b" ;
      P.run p ;
      equal (list string) ["open"; "sent ok"; "msg a"; "msg b"] (events ()) ;
      no_errors p )

let client_close () =
  with_page (fun p ->
      let s = M.server p "/ws" in
      M.accept s ;
      P.run p ;
      click p "close" ;
      equal int ~msg:"closed at once" 0 (M.connections s) ;
      click p "send" ;
      P.run p ;
      equal (list string)
        ["open"; "closing"; "sent no"; "closed 1000  true"]
        (events ()) ;
      equal (list string) ~msg:"nothing sent" [] (M.received s) ;
      click p "close" ;
      P.run p ;
      equal int ~msg:"a second close: no event" 5 (List.length (events ())) ;
      equal int ~msg:"the key is live" 1 (W.Private.live ()) ;
      no_errors p )

let server_drop () =
  with_page (fun p ->
      let s = M.server p "ws://localhost/ws" in
      M.accept s ;
      P.run p ;
      M.drop s {code= 4001; reason= "bye"; clean= true} ;
      M.drop s {code= 4002; reason= "again"; clean= true} ;
      P.run p ;
      equal (list string) ["open"; "closed 4001 bye true"] (events ()) ;
      equal int 0 (M.connections s) ;
      equal int ~msg:"the count goes down at the end of the key" 1
        (W.Private.live ()) ;
      M.push s "late" ;
      P.run p ;
      click p "send" ;
      P.run p ;
      equal (list string)
        ["open"; "closed 4001 bye true"; "sent no"]
        (events ()) )

let refuse () =
  with_page (fun p ->
      let s = M.server p "http://localhost/ws" in
      M.drop s {code= 4001; reason= "r"; clean= true} ;
      P.run p ;
      equal (list string) ["failed"; "closed 1006  false"] (events ()) ;
      M.accept s ;
      P.run p ;
      equal int ~msg:"no open after a refuse" 2 (List.length (events ())) )

(* A close, then a new [~id] after exactly one second. *)
let reconnect () =
  with_page ~retry:true (fun p ->
      let s = M.server p "/ws" in
      M.accept s ;
      P.run p ;
      M.drop s {code= 1001; reason= ""; clean= true} ;
      P.advance p Span.zero ;
      equal int 0 (M.connections s) ;
      P.advance p (Span.ms 999) ;
      equal int ~msg:"999 ms: no connection" 0 (M.connections s) ;
      P.advance p (Span.ms 1) ;
      equal (list string) ~msg:"1000 ms: the message"
        ["open"; "closed 1001  true"; "retry"]
        (events ()) ;
      (* The message of a task renders at the next frame: then the new key
         starts. *)
      P.advance p (Span.ms 16) ;
      equal int ~msg:"a new connection at the frame" 1 (M.connections s) ;
      M.accept s ;
      P.run p ;
      equal (list string)
        ["open"; "closed 1001  true"; "retry"; "open"]
        (events ()) ;
      equal int ~msg:"one live key" 1 (W.Private.live ()) ;
      (* The connection of the old key does not reach the new socket. *)
      click p "send first" ;
      P.run p ;
      equal (list string) ~msg:"the old connection" [] (M.received s) ;
      click p "send" ;
      P.run p ;
      equal (list string) ~msg:"the new connection" ["hi"] (M.received s) ;
      equal (list string) ~msg:"sent" ["sent no"; "sent ok"]
        (List.filteri (fun i _ -> i >= 4) (events ())) )

(* A protocol failure of an open socket: Failed, then Closed 1006. *)
let failure () =
  with_page (fun p ->
      let s = M.server p "/ws" in
      M.accept s ;
      P.run p ;
      M.fail s ;
      M.fail s ;
      P.run p ;
      equal (list string) ["open"; "failed"; "closed 1006  false"] (events ()) ;
      equal int 0 (M.connections s) )

(* The end of the key closes the socket at once, and no event comes after it. A
   send of the kept connection gives Not_open. *)
let unmount () =
  with_page (fun p ->
      let s = M.server p "/ws" in
      M.accept s ;
      P.run p ;
      M.push s "late" ;
      click p "toggle" ;
      equal int ~msg:"no connection" 0 (M.connections s) ;
      equal int ~msg:"no live key" 0 (W.Private.live ()) ;
      P.run p ;
      click p "send" ;
      P.run p ;
      equal (list string) ["open"; "sent no"] (events ()) ;
      equal (list string) [] (M.received s) ;
      click p "toggle" ;
      equal int ~msg:"a new socket" 1 (M.connections s) ;
      equal int 1 (W.Private.live ()) )

(* The key comes from the URL and the id. A key that stays keeps its socket and
   gets the newest message function. *)
type kmsg = K of string * W.event | Bump

let keys () =
  let got = ref [] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n -> function
           | K (tag, e) ->
               got := (tag ^ " " ^ show e) :: !got ;
               (n, Cmd.none)
           | Bump ->
               (n + 1, Cmd.none) )
         ~view:(fun _ -> El.button ~ev:[Ev.click Bump] [txt "bump"])
         ~subs:(fun n ->
           let tag = string_of_int n in
           Sub.batch
             [ W.connect "/a" (fun e -> K ("a" ^ tag, e))
             ; W.connect "/a" (fun e -> K ("a2", e))
             ; W.connect ~id:"x" "/a" (fun e -> K ("ax", e))
             ; W.connect "x" (fun e -> K ("rel", e))
             ; ( if n = 0 then W.connect ~id:"1" "/" (fun e -> K ("1", e))
                 else Sub.none )
             ; W.connect ~id:"" "1/" (fun e -> K ("2", e)) ] )
         () )
  in
  Fun.protect
    ~finally:(fun () -> P.stop p)
    (fun () ->
      let a = M.server p "/a" in
      equal int ~msg:"three sockets to /a" 3 (M.connections a) ;
      equal int ~msg:"a relative URL" 1
        (M.connections (M.server p "ws://localhost/x")) ;
      equal int 1 (M.connections (M.server p "/")) ;
      equal int 6 (W.Private.live ()) ;
      P.click p (By.text "bump") ;
      equal int ~msg:"the keys stay" 3 (M.connections a) ;
      (* The length of the id is in the key: the key of id "1" and "/" is not
         the key of id "" and "1/". *)
      equal int ~msg:"the removed key" 0 (M.connections (M.server p "/")) ;
      equal int ~msg:"the key that stays" 1 (M.connections (M.server p "/1/")) ;
      M.push a "m" ;
      M.accept a ;
      M.push a "m" ;
      P.run p ;
      equal (list string)
        ["a1 open"; "a2 open"; "ax open"; "a1 msg m"; "a2 msg m"; "ax msg m"]
        (List.rev !got) ;
      equal int ~msg:"https" 0 (M.connections (M.server p "https://h/a")) )

let no_driver () =
  with_page (fun p ->
      click p "toggle" ;
      W.Private.set_driver None ;
      equal bool ~msg:"no driver" true (W.Private.driver () = None) ;
      click p "toggle" ;
      equal int ~msg:"no live key" 0 (W.Private.live ()) ;
      match H.errors (P.host p) with
      | [Failure m] ->
          equal string
            "kilim.websocket: no driver: call Kilim_websocket_browser.install \
             () in main.ml, or use Websocket_mock in tests"
            m
      | l ->
          fail (string_of_int (List.length l) ^ " errors") )

(* The URLs of the mock: http becomes ws, https becomes wss. *)
let urls () =
  with_page ~url:"https://h/s" (fun p ->
      equal int 1 (M.connections (M.server p "wss://h/s")) ;
      equal int 0 (M.connections (M.server p "ws://h/s")) ) ;
  with_page ~url:"http://h/s" (fun p ->
      equal int 1 (M.connections (M.server p "ws://h/s")) )

let after_stop () =
  let p = P.start (app ()) in
  let s = M.server p "/ws" in
  M.accept s ;
  P.run p ;
  click p "send" ;
  P.run p ;
  P.stop p ;
  equal int ~msg:"the stop closes" 0 (M.connections s) ;
  equal int 0 (W.Private.live ()) ;
  equal (list string) ~msg:"received after the stop" ["hi"] (M.received s) ;
  equal int ~msg:"server after the stop" 0 (M.connections (M.server p "/ws")) ;
  let q = P.start (app ()) in
  Fun.protect
    ~finally:(fun () -> P.stop q)
    (fun () ->
      raises
        (Failure "Kilim_testing.Websocket_mock: the page is not the last page")
        (fun () -> ignore (M.server p "/ws") ) ;
      equal int ~msg:"a new host: a new server" 0
        (List.length (M.received (M.server q "/ws"))) ;
      let r = P.start ~host:(P.host q) (app ()) in
      equal int ~msg:"the same host keeps its servers" 2
        (M.connections (M.server q "/ws")) ;
      P.stop r )

let leak () =
  let p = P.start (app ()) in
  let s = M.server p "/ws" in
  for _ = 1 to 100 do
    M.accept s ; P.run p ; click p "toggle" ; click p "toggle"
  done ;
  click p "toggle" ;
  equal int 0 (M.connections s) ;
  equal int 0 (W.Private.live ()) ;
  equal int 100 (List.length (List.filter (String.equal "open") (events ()))) ;
  P.stop p

(* A connection that escapes its instance: the child gives its connection to the
   parent, and the parent keeps it after the child unmounts. The model of the
   child must be free: at the end of the key, the connection drops the closures
   of the driver. The sensitivity control: a driver that keeps each handlers
   record keeps the model, thus the check finds a model that the handlers
   keep. *)
module Child = struct
  include Component.Defaults

  type props = unit

  type model = Bytes.t

  type msg = W.event

  type out = W.conn

  let id : (props, model, msg, out) Component.id = Component.id ()

  let models = Weak.create 1

  let init () =
    let m = Bytes.make 64 'x' in
    Weak.set models 0 (Some m) ; (m, Cmd.none)

  let update ctx () m = function
    | W.Opened c ->
        (m, Ctx.emit ctx c)
    | _ ->
        (m, Cmd.none)

  let view () m = txt (string_of_int (Bytes.length m))

  let subs () m = W.connect "/w" (fun e -> if Bytes.length m > 0 then e else e)
end

type pmsg = Toggle_child | Keep of W.conn

(* The handlers that the leaky driver keeps. *)
let kept : W.handlers list ref = ref []

(* The child mounts, gives its connection to the parent, and unmounts. The
   result: the model of the child is alive. *)
let escape ~leak =
  kept := [] ;
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((false, None), Cmd.none))
         ~update:(fun (shown, c) -> function
           | Toggle_child ->
               ((not shown, c), Cmd.none)
           | Keep c ->
               ((shown, Some c), Cmd.none) )
         ~view:(fun (shown, _) ->
           El.div
             [ El.button ~ev:[Ev.click Toggle_child] [txt "toggle"]
             ; ( if shown then
                   Component.v (module Child) ~on:(fun c -> Keep c) ()
                 else txt "" ) ] )
         () )
  in
  Fun.protect
    ~finally:(fun () ->
      P.stop p ;
      kept := [] )
    (fun () ->
      if leak then
        W.Private.set_driver
          (Some
             { connect=
                 (fun _ h ->
                   kept := h :: !kept ;
                   W.Private.none ) } ) ;
      click p "toggle" ;
      ( match !kept with
      | h :: _ ->
          h.opened ()
      | [] ->
          M.accept (M.server p "/w") ) ;
      P.run p ; click p "toggle" ; Gc.full_major () ; Weak.check Child.models 0 )

let weak () =
  is_false ~msg:"a kept connection: the model of the child is free"
    (escape ~leak:false) ;
  is_true ~msg:"the sensitivity control: the handlers keep the model"
    (escape ~leak:true)

let errors () = equal string "not open" (W.error_to_string W.Not_open)

let tests =
  [ group "websocket"
      [ test "open, send and push" open_send_push
      ; test "a close of the client" client_close
      ; test "a close of the server" server_drop
      ; test "a refused connection" refuse
      ; test "a failure of an open socket" failure
      ; test "connect again with a new id after one second" reconnect
      ; test "the end of the key closes the socket" unmount
      ; test "the keys" keys
      ; test "no driver" no_driver
      ; test "error_to_string" errors
      ; test "the URLs of the mock" urls
      ; test "the reads after the stop, the last host" after_stop
      ; test "100 mounts and stops" leak
      ; test "weak: a connection that escapes its instance" weak ] ]

let () = exit (Windtrap.run "websocket" tests)
