(* The tests of kilim.http.browser in real browsers: Chrome, Firefox and WebKit
   (design sections 12 and 13), in the js_of_ocaml and wasm_of_ocaml builds, in
   the dev and the release profile. The /http endpoints are in run.mjs. A spy
   counts the abort events of the AbortSignal objects on the client, and a
   wrapper of fetch in the head of the page (run.mjs) records the signal of each
   call. test/dom/guards.txt lists the guards with their tests. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private
module Http = Kilim_http
module D = Kilim_http.Decode

let spy_code =
  {|(() => {
  const P = AbortController.prototype, abort = P.abort;
  let n = 0;
  P.abort = function (...a) {
    this.signal.addEventListener('abort', () => { n++; }, { once: true });
    return abort.apply(this, a);
  };
  globalThis.kilimAborts = () => n;
})()|}

let aborts () = int (Jv.call Jv.global "kilimAborts" [||])

(* The signals of the fetch calls from the call [from]: A for an aborted signal,
   P for a signal that is not aborted, N for no signal. *)
let fetches () = int (Jv.call Jv.global "kilimFetches" [||])

let signals from =
  Jv.to_string (Jv.call Jv.global "kilimSignals" [|Jv.of_int from|])

let show = function Ok s -> s | Error e -> Http.error_to_string e

type msg = Got of string

(* An app whose init task is [task]. The result goes to [r], with no wait for a
   render. The fallback shows the error. *)
let app r task =
  App.make
    ~init:(fun () -> ((), Cmd.task (Async.map (fun s -> Got s) task)))
    ~update:(fun () (Got s) ->
      r := Some s ;
      ((), Cmd.none) )
    ~view:(fun () -> txt "")
    ~fallback:(function
      | Failure m ->
          El.p ~at:[At.data "testid" "fb"] [txt m]
      | _ ->
          El.p ~at:[At.data "testid" "fb"] [txt "other"] )
    ()

(* The result of [task] in a new app, at most 2 s. *)
let result task =
  let r = ref None in
  let a = B.start (el (container ())) (app r task) in
  let* () = until (fun () -> Option.is_some !r) in
  B.stop a ;
  Fut.return (Option.value ~default:"none" !r)

let req ?(headers = []) ?body meth url = {Http.meth; url; headers; body}

let send ?timeout r =
  Async.map
    (function
      | Ok (x : Http.response) ->
          string_of_int x.status ^ " " ^ x.body
      | Error e ->
          show (Error e) )
    (Http.request ?timeout r)

let echo =
  let open D.Syntax in
  let+ m = D.field "method" D.string
  and+ h = D.field "headers" (D.field_opt "x-kilim" D.string)
  and+ b = D.field "body" D.string in
  m ^ "|" ^ Option.value ~default:"-" h ^ "|" ^ b

let starts_with p s = String.starts_with ~prefix:p s

(* HT0: no transport before install: the task fails with Failure. *)
let ht0 () =
  let root = container () in
  let a = B.start (el root) (app (ref None) (Async.map show (Http.get "/x"))) in
  let find () =
    Jv.call root "querySelector" [|Jv.of_string "[data-testid=fb]"|]
  in
  let* () = until (fun () -> not (Jv.is_null (find ()))) in
  let fb = find () in
  check "HT0 no transport: the fallback gets the Failure"
    ( (not (Jv.is_null fb))
    && starts_with "kilim.http: no transport: call Kilim_http_browser.install"
         (text fb) ) ;
  B.stop a ;
  Fut.return ()

(* HT1: the method, the headers and the body, and no abort at the normal end. *)
let ht1 () =
  let n = aborts () and f = fetches () in
  let* post =
    result
      (Async.map
         (fun r -> show (Result.bind r (fun (x : Http.response) -> Ok x.body)))
         (Http.request
            (req Post "/http/echo" ~body:"abc \xc3\xa9"
               ~headers:[("x-kilim", "1"); ("content-type", "text/plain")] ) ) )
  in
  let decoded =
    match D.decode_string echo post with
    | Ok s ->
        s
    | Error e ->
        D.error_to_string e
  in
  check "HT1 POST: the method, a header and the body (UTF-8)"
    (decoded = "POST|1|abc \xc3\xa9") ;
  let* get = result (Async.map show (Http.get_json echo "/http/echo")) in
  check "HT1 get_json: GET with no body" (get = "GET|-|") ;
  let* put =
    result
      (Async.map show
         (Http.get_json echo "/http/echo" ~headers:[("x-kilim", "2")]) )
  in
  check "HT1 the headers of get" (put = "GET|2|") ;
  (* The server reads each byte of a header as one character (latin1): thus "é"
     tells one byte 0xE9, and "Ã©" the two bytes of UTF-8. *)
  let* e =
    result
      (Async.map show
         (Http.get_json echo "/http/echo" ~headers:[("x-kilim", "\xc3\xa9")]) )
  in
  check "HT1 a header value U+00E9: the bytes of the fact header_byte"
    (e = if facts.header_byte then "GET|\xc3\xa9|" else "GET|\xc3\x83\xc2\xa9|") ;
  let* euro =
    result
      (Async.map show
         (Http.get_json echo "/http/echo"
            ~headers:[("x-kilim", "\xe2\x82\xac")] ) )
  in
  check "HT1 a header value above U+00FF: Network TypeError"
    (starts_with "network TypeError: " euro) ;
  let* put = result (send (req Put "/http/echo" ~body:"p")) in
  let* del = result (send (req Delete "/http/echo")) in
  check "HT1 PUT and DELETE"
    ( starts_with "200 {\"method\":\"PUT\"" put
    && starts_with "200 {\"method\":\"DELETE\"" del ) ;
  let* head =
    result
      (Async.map
         (function
           | Ok (x : Http.response) ->
               let host =
                 Jv.to_string (Jv.get (Jv.get Jv.global "location") "host")
               in
               string_of_bool
                 ( x.url = "http://" ^ host ^ "/http/echo"
                 && List.assoc_opt "content-type" x.headers
                    = Some "application/json" )
           | Error e ->
               show (Error e) )
         (Http.request (req Get "/http/echo")) )
  in
  check "HT1 the URL and the headers of the response (lowercase)" (head = "true") ;
  let* bad = result (send (req Get "/http/echo" ~headers:[("a b", "1")])) in
  check "HT1 a header name that is not valid: Network TypeError"
    (starts_with "network TypeError: " bad) ;
  check "HT1 no abort at the normal end of a request" (aborts () = n) ;
  check "HT1 each fetch has a signal, not aborted"
    (fetches () - f = 7 && signals f = "PPPPPPP") ;
  Fut.return ()

(* HT2: a status that is not 2xx. *)
let ht2 () =
  let* g = result (Async.map show (Http.get "/http/status/404")) in
  let* r = result (send (req Get "/http/status/500")) in
  check "HT2 get: a 404 is Status" (g = "status 404 status 404") ;
  check "HT2 request: a 500 is a response" (r = "500 status 500") ;
  Fut.return ()

(* HT3: the body comes 300 ms after the headers: the request reads all of it. A
   connection that closes in the body gives a network error. *)
let ht3 () =
  let n = aborts () in
  let* s = result (Async.map show (Http.get "/http/body-slow")) in
  check "HT3 a slow body is complete, with no abort"
    (s = "slow body" && aborts () = n) ;
  let* b = result (Async.map show (Http.get "/http/body-broken")) in
  check "HT3 a body that breaks: Network TypeError"
    (starts_with "network TypeError: " b) ;
  if not (starts_with "network TypeError: " b) then log ("HT3 got: " ^ b) ;
  Fut.return ()

(* HT4: a stop during a request aborts it: no message, no console error. *)
let ht4 () =
  let n = aborts () and e = console_errors () and f = fetches () in
  let r = ref None in
  let a =
    B.start
      (el (container ()))
      (app r (Async.map show (Http.get "/http/never")))
  in
  let* () = sleep 100 in
  B.stop a ;
  let* () = sleep 100 in
  check "HT4 a stop during a request: one abort, no message, no error"
    (aborts () = n + 1 && !r = None && console_errors () = e) ;
  check "HT4 the signal of the fetch is aborted" (signals f = "A") ;
  Fut.return ()

(* HT5: three keys of Cmd.latest: two aborts and one message. *)
type lmsg = Key of string | Res of string

let ht5 () =
  let n = aborts () and f = fetches () in
  let got = ref [] in
  let key ms url =
    Cmd.task (Async.map (fun () -> Key url) (Async.sleep (Span.ms ms)))
  in
  let a =
    B.start
      (el (container ()))
      (App.make
         ~init:(fun () ->
           ( ()
           , Cmd.batch
               [ key 0 "/http/never"
               ; key 100 "/http/never"
               ; key 200 "/http/status/200" ] ) )
         ~update:(fun () -> function
           | Key url ->
               ( ()
               , Cmd.latest ~key:"k"
                   (Async.map (fun r -> Res (show r)) (Http.get url)) )
           | Res s ->
               got := s :: !got ;
               ((), Cmd.none) )
         ~view:(fun () -> txt "")
         () )
  in
  let* () = until (fun () -> !got <> []) in
  let* () = sleep 50 in
  check "HT5 Cmd.latest x3: two aborts and one message"
    (aborts () = n + 2 && !got = ["status 200"]) ;
  check "HT5 the signals: two aborted, the last one not" (signals f = "AAP") ;
  B.stop a ;
  Fut.return ()

(* HT6: the end of the timeout aborts the request. *)
let ht6 () =
  let n = aborts () and f = fetches () in
  let* s = result (send ~timeout:(Span.ms 200) (req Get "/http/never")) in
  check "HT6 a timeout: Timeout and one abort"
    (s = "timeout" && aborts () = n + 1) ;
  check "HT6 the signal of the fetch is aborted" (signals f = "A") ;
  Fut.return ()

(* HT7: a network error: a port that the browser blocks. *)
let ht7 () =
  let* s = result (Async.map show (Http.get "http://127.0.0.1:1/")) in
  check
    ("HT7 a network error: " ^ facts.network_error)
    (s = "network " ^ facts.network_error) ;
  if s <> "network " ^ facts.network_error then log ("HT7 got: " ^ s) ;
  Fut.return ()

(* HT8: a body that is not JSON. *)
let ht8 () =
  let* s =
    result
      (Async.map show
         (Http.get_json (D.map Http.Json.to_string D.value) "/http/bad-json") )
  in
  check "HT8 a body that is not JSON: Decode"
    (s = "decode unexpected end at offset 5") ;
  Fut.return ()

(* HT9: 20 starts and stops of an app whose task waits for /http/never. *)
let ht9 () =
  let n = aborts () and f = fetches () in
  let doc_vis = count ~name:"visibilitychange" document in
  let root = container () in
  let clean = ref true in
  for _ = 1 to 20 do
    let a =
      B.start (el root)
        (app (ref None) (Async.map show (Http.get "/http/never")))
    in
    let ins () = Private.Runtime.inspect (B.runtime a) in
    let started = (ins ()).token_children = 1 in
    B.stop a ;
    clean :=
      !clean && started
      && (ins ()).token_children = 0
      && count root = 0
      && count ~name:"visibilitychange" document = doc_vis
      && B.live_timers () = 0
      && int (Jv.get (Jv.get root "childNodes") "length") = 0
  done ;
  check "HT9 20 starts and stops during a request: 20 aborts, each count 0"
    (!clean && aborts () = n + 20) ;
  check "HT9 the 20 signals of the fetch calls are aborted"
    (signals f = String.make 20 'A') ;
  Fut.return ()

let () =
  ignore (Lazy.force spy) ;
  ignore (js spy_code) ;
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     let* () = ht0 () in
     Kilim_http_browser.install () ;
     Kilim_http_browser.install () ;
     let* () = ht1 () in
     let* () = ht2 () in
     let* () = ht3 () in
     let* () = ht4 () in
     let* () = ht5 () in
     let* () = ht6 () in
     let* () = ht7 () in
     let* () = ht8 () in
     ht9 () )
    finish
