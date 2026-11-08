(* The tests of kilim.websocket.browser in real browsers: Chrome, Firefox and
   WebKit (design sections 08 and 13), in the js_of_ocaml and wasm_of_ocaml
   builds, in the dev and the release profile. The /ws endpoints are in run.mjs,
   with the server of examples/ws.mjs. A spy on the constructor WebSocket keeps
   each socket and the code of its close event (through the property onclose,
   thus the listener spy does not count it), and a spy counts the warnings of
   the console. test/dom/guards.txt lists the guards with their tests. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private
module W = Kilim_websocket

let spy_code =
  {|(() => {
  const C = WebSocket, socks = [];
  globalThis.WebSocket = class extends C {
    constructor(...a) {
      super(...a);
      this.kilimClose = '';
      this.onclose = (e) => { this.kilimClose = e.code + ' ' + e.wasClean; };
      socks.push(this);
    }
  };
  globalThis.kilimSockets = socks;
  let n = 0;
  const warn = console.warn;
  console.warn = function (...a) { n++; return warn.apply(this, a); };
  globalThis.kilimWarns = () => n;
  globalThis.kilimHttpScheme = () => {
    try { new C('http://' + location.host + '/ws/none').close(); return true; }
    catch { return false; }
  };
})()|}

let sockets () = Jv.get Jv.global "kilimSockets"

let socket_count () = int (Jv.get (sockets ()) "length")

let socket i = Jv.get (sockets ()) (string_of_int i)

let last_socket () = socket (socket_count () - 1)

let ready s = int (Jv.get s "readyState")

let closed_with s = Jv.to_string (Jv.get s "kilimClose")

let warns () = int (Jv.call Jv.global "kilimWarns" [||])

let show = function
  | W.Opened _ ->
      "open"
  | Message m ->
      "msg " ^ m
  | Closed {code; reason; clean} ->
      "closed " ^ string_of_int code ^ " " ^ reason ^ " " ^ string_of_bool clean
  | Failed ->
      "failed"

(* The events of the apps, the newest first, and the last connection. *)
let evs = ref []

let events () = List.rev !evs

let conn = ref None

type msg = Got of string * W.event

(* An app with one connection for each URL of [urls], with the URL as the tag of
   its events. *)
let app urls =
  evs := [] ;
  conn := None ;
  App.make
    ~init:(fun () -> ((), Cmd.none))
    ~update:(fun () (Got (tag, e)) ->
      (match e with W.Opened c -> conn := Some c | _ -> ()) ;
      evs := ((if List.length urls > 1 then tag ^ " " else "") ^ show e) :: !evs ;
      ((), Cmd.none) )
    ~view:(fun () -> txt "")
    ~subs:(fun () ->
      Sub.batch
        (List.mapi
           (fun i u ->
             W.connect ~id:(string_of_int i) u (fun e ->
                 Got (string_of_int i, e) ) )
           urls ) )
    ()

let start urls = B.start (el (container ())) (app urls)

(* The result of a task in a new app, at most 2 s. *)
let result task =
  let r = ref None in
  let a =
    B.start
      (el (container ()))
      (App.make
         ~init:(fun () -> ((), Cmd.task (Async.map (fun s -> Some s) task)))
         ~update:(fun () s ->
           r := s ;
           ((), Cmd.none) )
         ~view:(fun () -> txt "")
         () )
  in
  let* () = until (fun () -> Option.is_some !r) in
  B.stop a ;
  Fut.return (Option.value ~default:"none" !r)

let send text =
  result
    (Async.map
       (function Ok () -> "ok" | Error W.Not_open -> "not open")
       (W.send (Option.get !conn) text) )

let has n = until (fun () -> List.length !evs >= n)

(* No listener of the driver stays on the socket. *)
let no_listener s = count s = 0

(* WS1: an echo, with a relative URL. A binary frame gives a warning and no
   message. A close of the client gives the code 1000, then a send gives
   Not_open. *)
let ws1 () =
  let w = warns () in
  let a = start ["/ws/echo"] in
  let* () = has 1 in
  let s = last_socket () in
  let* r = send "hello" in
  let* () = has 2 in
  let* b = send "bin:abc" in
  let* r2 = send "after" in
  let* () = has 3 in
  check "WS1 open, the echo of two texts, no message for a binary frame"
    ( events () = ["open"; "msg hello"; "msg after"]
    && r = "ok" && b = "ok" && r2 = "ok" ) ;
  check "WS1 a binary frame: one warning" (warns () = w + 1) ;
  let host = Jv.to_string (Jv.get (Jv.get Jv.global "location") "host") in
  check "WS1 the relative URL resolves to ws:"
    (Jv.to_string (Jv.get s "url") = "ws://" ^ host ^ "/ws/echo") ;
  let* _ = result (Async.map (fun () -> "") (W.close (Option.get !conn))) in
  let* () = has 4 in
  let* n = send "late" in
  check "WS1 a close of the client: Closed 1000, then Not_open"
    (List.nth (events ()) 3 = "closed 1000  true" && n = "not open") ;
  B.stop a ;
  check "WS1 no listener after the stop" (no_listener s && W.Private.live () = 0) ;
  Fut.return ()

(* WS2: the server closes with a code and a reason after the open. *)
let ws2 () =
  let a = start ["/ws/close?code=4001&reason=bye"] in
  let* () = has 2 in
  check "WS2 a close of the server: the code and the reason"
    (events () = ["open"; "closed 4001 bye true"]) ;
  B.stop a ;
  Fut.return ()

(* WS3: an unmount in CONNECTING and in OPEN closes the socket, and no event
   comes after it. *)
let ws3 () =
  let a = start ["/ws/echo"] in
  let s = last_socket () in
  let connecting = ready s = 0 in
  B.stop a ;
  let closing = ready s >= 2 in
  let* () = until (fun () -> closed_with s <> "") in
  let* () = sleep 50 in
  check "WS3 an unmount in CONNECTING: the socket fails (1006), no event"
    ( connecting && closing
    && closed_with s = "1006 false"
    && events () = []
    && no_listener s
    && W.Private.live () = 0 ) ;
  let a = start ["/ws/echo"] in
  let* () = has 1 in
  let s = last_socket () in
  B.stop a ;
  let closing = ready s = 2 in
  let* () = until (fun () -> closed_with s <> "") in
  let* () = sleep 50 in
  check "WS3 an unmount in OPEN: a close with the code 1000, no event"
    ( closing
    && closed_with s = "1000 true"
    && events () = ["open"]
    && no_listener s
    && W.Private.live () = 0 ) ;
  Fut.return ()

let ftp = "ftp://127.0.0.1/"

(* WS4: a refused upgrade (404) and two URLs that the browser refuses. *)
let ws4 () =
  let a = start ["/ws/none"] in
  let* () = has 2 in
  check "WS4 a refused upgrade: Failed, then Closed 1006"
    (events () = ["failed"; "closed 1006  false"]) ;
  B.stop a ;
  let n = socket_count () in
  List.fold_left
    (fun acc url ->
      let* () = acc in
      let w = warns () in
      let report = if url = ftp then facts.scheme_report else None in
      Option.iter (fun r -> log ("EXPECT EXCEPTION " ^ r)) report ;
      let a = start [url] in
      let* () = has 2 in
      if Option.is_some report then log "EXPECT END" ;
      check
        ("WS4 the browser refuses " ^ url ^ ": a warning, Failed, Closed 1006")
        ( events () = ["failed"; "closed 1006  false"]
        && warns () = w + 1
        && socket_count () = n ) ;
      B.stop a ;
      Fut.return () )
    (Fut.return ()) [ftp; "ws://[/"]

(* WS5: a room sends each text to its two connections. *)
let ws5 () =
  let a = start ["/ws/room"; "/ws/room"] in
  let* () = has 2 in
  let* r = send "x" in
  let* () = has 4 in
  let tail = List.sort compare (List.filteri (fun i _ -> i >= 2) (events ())) in
  check "WS5 a broadcast to the two connections of a room"
    (r = "ok" && tail = ["0 msg x"; "1 msg x"]) ;
  B.stop a ;
  Fut.return ()

(* WS6: 20 starts and stops of an open socket: no listener stays, each socket
   closes with the code 1000, and the live count is 0. *)
let ws6 () =
  let first = socket_count () in
  let root = container () in
  let rec go i clean =
    if i = 0 then Fut.return clean
    else begin
      let a = B.start (el root) (app ["/ws/echo"]) in
      let ins () = Private.Runtime.inspect (B.runtime a) in
      let* () = has 1 in
      let s = last_socket () in
      B.stop a ;
      go (i - 1)
        ( clean && no_listener s
        && (ins ()).token_children = 0
        && W.Private.live () = 0
        && B.live_timers () = 0 )
    end
  in
  let* clean = go 20 true in
  let all_closed () =
    List.for_all
      (fun i -> closed_with (socket i) = "1000 true")
      (List.init 20 (fun i -> first + i))
  in
  let* () = until all_closed in
  check "WS6 20 starts and stops: no listener, 20 closes with the code 1000"
    (clean && socket_count () = first + 20 && all_closed ()) ;
  Fut.return ()

(* The fact http_scheme: the constructor accepts an http URL. *)
let fact () =
  let got = Jv.to_bool (Jv.call Jv.global "kilimHttpScheme" [||]) in
  check
    ("WS0 the fact http_scheme: " ^ string_of_bool facts.http_scheme)
    (got = facts.http_scheme) ;
  Fut.return ()

let () =
  ignore (Lazy.force spy) ;
  ignore (js spy_code) ;
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     let* () = fact () in
     Kilim_websocket_browser.install () ;
     Kilim_websocket_browser.install () ;
     let* () = ws1 () in
     let* () = ws2 () in
     let* () = ws3 () in
     let* () = ws4 () in
     let* () = ws5 () in
     ws6 () )
    finish
