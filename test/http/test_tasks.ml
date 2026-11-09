(* The tasks of kilim.http on Http_mock and the virtual clock (design section
   14), and the stop hook of Page. Each test that changes the global transport
   clears it at the end, also when it fails. *)

open Windtrap
open Kilim
module Http = Kilim_http
module D = Kilim_http.Decode
module P = Kilim_testing.Page
module By = Kilim_testing.By
module M = Kilim_testing.Http_mock
module Host = Kilim_testing.Host

(* No transport. *)
let clear () = Http.Private.set_transport None

let show = function Ok s -> s | Error e -> Http.error_to_string e

type msg = Go | Got of string

(* The last result of the apps, with no wait for a render. *)
let last = ref ""

let text _ = !last

(* A click on "go" starts [!task ()], with Cmd.latest if [latest]. *)
let app ?(latest = false) task =
  last := "" ;
  App.make
    ~init:(fun () -> ((), Cmd.none))
    ~update:(fun () -> function
      | Go ->
          let t = Async.map (fun s -> Got s) (!task ()) in
          ((), if latest then Cmd.latest ~key:"k" t else Cmd.task t)
      | Got s ->
          last := s ;
          ((), Cmd.none) )
    ~view:(fun () -> El.button ~ev:[Ev.click Go] [txt "go"])
    ()

let go p = P.click p (By.text "go")

(* The page of [app], and the stop at the end, also after a failure. *)
let with_page ?latest task f =
  let p = P.start (app ?latest task) in
  Fun.protect ~finally:(fun () -> P.stop p ; clear ()) (fun () -> f p)

let get_json ?timeout url () =
  Async.map
    (fun r -> show (Result.map (String.concat ",") r))
    (Http.get_json ?timeout D.(list string) url)

let req_meth (r : Http.request) = Http.string_of_meth r.meth ^ " " ^ r.url

let json_ok () =
  with_page
    (ref (get_json "/api/cities?q=par"))
    (fun p ->
      M.on_get p "/api/cities?q=par" (`Json {|["Paris","Parma"]|}) ;
      go p ;
      equal int ~msg:"in flight" 1 (M.pending p) ;
      P.run p ;
      equal string "Paris,Parma" (text p) ;
      equal (list string) ["GET /api/cities?q=par"]
        (List.map req_meth (M.requests p)) ;
      equal int 0 (M.pending p) ;
      M.expect_done p )

let req url = {Http.meth= Get; url; headers= []; body= None}

let send r =
  Async.map
    (function
      | Ok (x : Http.response) ->
          string_of_int x.status ^ " " ^ x.url ^ " "
          ^ String.concat ";" (List.map (fun (k, v) -> k ^ "=" ^ v) x.headers)
          ^ " " ^ x.body
      | Error _ ->
          "error" )
    (Http.request r)

let replies () =
  let task = ref (fun () -> Async.return "") in
  with_page task (fun p ->
      let run t expected =
        (task := fun () -> t) ;
        go p ;
        P.run p ;
        equal string expected (text p)
      in
      M.on_get p "/404" (`Status (404, "nf")) ;
      M.on_get p "/down" (`Network "down") ;
      M.on_get p "/text" (`Text "plain") ;
      M.on_get p "/bad" (`Json "[1,") ;
      M.on_get p "/ints" (`Json "[1]") ;
      M.on_get p "/300" (`Status (300, "mc")) ;
      M.on_get p "/r"
        (`Response
           {Http.status= 201; url= "u"; headers= [("a", "b")]; body= "made"} ) ;
      run (get_json "/404" ()) "status 404 nf" ;
      run (Async.map show (Http.get "/300")) "status 300 mc" ;
      run (send (req "/r")) "201 u a=b made" ;
      run
        (send (req "/ints"))
        "200 http://localhost/ints content-type=application/json [1]" ;
      run (get_json "/down" ()) "network down" ;
      run (Async.map show (Http.get "/text")) "plain" ;
      run (get_json "/bad" ()) "decode unexpected end at offset 3" ;
      run (get_json "/ints" ()) "decode 0: expected a string, got a number" ;
      (* request gives each status. *)
      run
        (Async.map
           (function
             | Ok (r : Http.response) ->
                 string_of_int r.status ^ " " ^ r.url ^ " " ^ r.body
             | Error _ ->
                 "error" )
           (Http.request {meth= Get; url= "/404"; headers= []; body= None}) )
        "404 http://localhost/404 nf" ;
      run
        (Async.map
           (function
             | Ok (r : Http.response) ->
                 String.concat ";"
                   (List.map (fun (k, v) -> k ^ "=" ^ v) r.headers)
             | Error _ ->
                 "error" )
           (Http.request {meth= Get; url= "/text"; headers= []; body= None}) )
        "content-type=text/plain" ;
      M.expect_done p )

let unmatched () =
  with_page
    (ref (get_json "/x"))
    (fun p ->
      go p ;
      equal (list string) ["GET /x"] (List.map req_meth (M.unmatched p)) ;
      raises (Failure "1 requests in flight") (fun () -> M.expect_done p) ;
      P.run p ;
      equal string "network Http_mock: no reply for GET /x" (text p) ;
      raises (Failure "no reply for GET /x and 0 other requests") (fun () ->
          M.expect_done p ) )

(* A request of an init task, before the test gives a reply: unmatched. *)
let init_unmatched () =
  last := "" ;
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.task (get_json "/init" ())))
         ~update:(fun () s ->
           last := s ;
           ((), Cmd.none) )
         ~view:(fun () -> txt "")
         () )
  in
  Fun.protect
    ~finally:(fun () -> P.stop p ; clear ())
    (fun () ->
      equal int 1 (List.length (M.unmatched p)) ;
      P.run p ;
      equal string "network Http_mock: no reply for GET /init" (text p) )

let methods () =
  let task = ref (fun () -> Async.return "") in
  with_page task (fun p ->
      let send meth url ?body headers expected =
        (task :=
           fun () ->
             Async.map
               (function
                 | Ok (r : Http.response) -> r.body | Error e -> show (Error e) )
               (Http.request {meth; url; headers; body}) ) ;
        go p ;
        P.run p ;
        equal string ~msg:url expected (text p)
      in
      M.on_post p "/p" (`Text "posted") ;
      M.on p Put "/p" (`Text "put") ;
      M.on p Delete "/p" (`Text "deleted") ;
      M.on p Get "api/rel" (`Text "relative") ;
      M.on p Get "//localhost/abs" (`Text "host") ;
      send Post "/p" ~body:"b" [("x-a", "1")] "posted" ;
      send Put "http://localhost/p" [] "put" ;
      send Delete "/p" [] "deleted" ;
      send Get "/api/rel" [] "relative" ;
      send Get "http://localhost/abs" [] "host" ;
      send Get "/p" [] "network Http_mock: no reply for GET /p" ;
      send Get "https://localhost/abs" []
        "network Http_mock: no reply for GET https://localhost/abs" ;
      send Post "/n" [] "network Http_mock: no reply for POST /n" ;
      send Put "/n" [] "network Http_mock: no reply for PUT /n" ;
      send Delete "/n" [] "network Http_mock: no reply for DELETE /n" ;
      match M.requests p with
      | r :: _ ->
          equal (option string) (Some "b") r.body ;
          equal (list (pair string string)) [("x-a", "1")] r.headers ;
          equal int 5 (List.length (M.unmatched p))
      | [] ->
          fail "no request" )

(* The routes of ":x", "1http:x" and "a1+-.:x": a URL with a scheme stays, and
   the others resolve against the page. *)
let schemes () =
  let task = ref (fun () -> Async.return "") in
  with_page task (fun p ->
      let send url expected =
        (task := fun () -> Async.map show (Http.get url)) ;
        go p ;
        P.run p ;
        equal string ~msg:url expected (text p)
      in
      M.on_get p "http://localhost/:x" (`Text "colon") ;
      M.on_get p "http://localhost/1http:x" (`Text "digit") ;
      M.on_get p "a1+-.:x" (`Text "scheme") ;
      M.on_get p "http://localhost/a b:x" (`Text "space") ;
      M.on_get p "http://localhost/rel" (`Text "relative") ;
      M.on_get p "http://localhost/a1:x" (`Text "not a scheme") ;
      send ":x" "colon" ;
      send "1http:x" "digit" ;
      send "a1+-.:x" "scheme" ;
      send "a b:x" "space" ;
      send "rel" "relative" ;
      send "a1:x" "network Http_mock: no reply for GET a1:x" ;
      equal int 1 (List.length (M.unmatched p)) )

let once_and_delay () =
  let task = ref (get_json "/o") in
  with_page task (fun p ->
      M.on_get p ~once:true "/o" (`Json {|["a"]|}) ;
      M.on_get p ~delay:(Span.ms 50) "/o" (`Json {|["b"]|}) ;
      go p ;
      P.run p ;
      equal string "a" (text p) ;
      go p ;
      P.advance p (Span.ms 49) ;
      equal string "a" (text p) ;
      equal int 1 (M.pending p) ;
      P.advance p (Span.ms 1) ;
      equal string "b" (text p) ;
      go p ;
      P.advance p (Span.ms 50) ;
      equal string "b" (text p) ;
      equal int 0 (M.pending p) ;
      M.expect_done p )

(* The timeout aborts the request at the exact time. *)
let timeout () =
  with_page
    (ref (get_json ~timeout:(Span.ms 100) "/never"))
    (fun p ->
      M.on_get p "/never" `Never ;
      go p ;
      P.advance p (Span.ms 99) ;
      equal string "" (text p) ;
      equal int 1 (M.pending p) ;
      equal int 0 (M.aborted p) ;
      P.advance p (Span.ms 1) ;
      equal string "timeout" (text p) ;
      equal int 1 (M.aborted p) ;
      equal int 0 (M.pending p) ;
      M.expect_done p ;
      P.expect_no_tasks p )

(* A reply after the timeout: the request stays aborted. *)
let late () =
  with_page
    (ref (get_json ~timeout:(Span.ms 100) "/late"))
    (fun p ->
      M.on_get p ~delay:(Span.ms 200) "/late" (`Json {|["late"]|}) ;
      go p ;
      P.advance p (Span.ms 300) ;
      equal string "timeout" (text p) ;
      equal int 1 (M.aborted p) ;
      equal int 0 (M.pending p) )

(* A reply before the end of the timeout. *)
let in_time () =
  with_page
    (ref (get_json ~timeout:(Span.ms 100) "/slow"))
    (fun p ->
      M.on_get p ~delay:(Span.ms 99) "/slow" (`Json {|["s"]|}) ;
      go p ;
      P.advance p (Span.ms 200) ;
      equal string "s" (text p) ;
      equal int 0 (M.aborted p) ;
      P.expect_no_tasks p )

(* Three keys of Cmd.latest: two aborts and one message. *)
let latest () =
  let n = ref 0 in
  let task =
    ref (fun () ->
        incr n ;
        get_json ("/q" ^ string_of_int !n) () )
  in
  with_page ~latest:true task (fun p ->
      M.on_get p "/q1" `Never ;
      M.on_get p "/q2" `Never ;
      M.on_get p ~delay:(Span.ms 10) "/q3" (`Json {|["three"]|}) ;
      go p ;
      go p ;
      go p ;
      equal int 2 (M.aborted p) ;
      equal int 1 (M.pending p) ;
      P.advance p (Span.ms 10) ;
      equal string "three" (text p) ;
      equal int ~msg:"three clicks, one result" 4 (P.stats p).messages ;
      M.expect_done p )

module Holder = struct
  include Component.Defaults

  type props = Bytes.t Weak.t

  type model = Bytes.t

  type msg = string

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init w =
    let m = Bytes.make 1000 'x' in
    Weak.set w 0 (Some m) ;
    (m, Cmd.task (Async.map (fun s -> s ^ Bytes.to_string m) (get_json "/n" ())))

  let update _ _ m _ = (m, Cmd.none)

  let view _ m = txt (string_of_int (Bytes.length m))
end

type hmsg = Show | Hide

let holder w =
  App.make
    ~init:(fun () -> (false, Cmd.none))
    ~update:(fun _ -> function
      | Show -> (true, Cmd.none) | Hide -> (false, Cmd.none) )
    ~view:(fun shown ->
      El.div
        [ El.button ~ev:[Ev.click Show] [txt "show"]
        ; El.button ~ev:[Ev.click Hide] [txt "hide"]
        ; ( if shown then Component.v (module Holder) ~on:(function _ -> .) w
            else txt "" ) ] )
    ()

(* An unmount during a request aborts it. The model of the instance is free. *)
let unmount () =
  let w = Weak.create 1 in
  let p = P.start (holder w) in
  Fun.protect
    ~finally:(fun () -> P.stop p ; clear ())
    (fun () ->
      M.on_get p "/n" `Never ;
      P.click p (By.text "show") ;
      equal int 1 (M.pending p) ;
      Gc.full_major () ;
      is_true ~msg:"live" (Weak.check w 0) ;
      P.click p (By.text "hide") ;
      equal int 1 (M.aborted p) ;
      equal int 0 (M.pending p) ;
      P.expect_no_tasks p ;
      M.expect_done p ;
      Gc.full_major () ;
      is_false ~msg:"free" (Weak.check w 0) )

(* No transport: the task fails with Failure. *)
let no_transport () =
  clear () ;
  let r = ref None in
  let q = Private.Run_queue.create ~schedule:ignore in
  let host = Host.host (Host.create ()) in
  Private.Run_queue.with_turn q (fun () ->
      Private.Async.start ~host ~run:q (Private.Cancel.root ()) (Http.get "/x")
        ~ok:(fun _ -> r := Some "ok")
        ~error:(function Failure m -> r := Some m | _ -> r := Some "other") ;
      Private.Run_queue.drain q ) ;
  equal (option string)
    (Some
       "kilim.http: no transport: call Kilim_http_browser.install () in \
        main.ml, or use Http_mock in tests" )
    !r

(* A stop during a request aborts it: the state of the mock tells it after the
   stop. *)
let stop_aborts () =
  let p = P.start (app (ref (get_json "/n"))) in
  Fun.protect ~finally:clear (fun () ->
      M.on_get p "/n" `Never ;
      go p ;
      equal int 1 (M.pending p) ;
      P.stop p ;
      equal int 1 (M.aborted p) ;
      equal int 0 (M.pending p) ;
      equal int 1 (List.length (M.requests p)) ;
      equal int 0 (List.length (M.unmatched p)) ;
      M.expect_done p )

let current () =
  match Http.Private.transport () with Some _ -> "set" | None -> "none"

(* The stop hook: Page.stop stops the mock of its host. *)
let stop_hook () =
  Fun.protect ~finally:clear (fun () ->
      let task = ref (get_json "/a") in
      let p1 = P.start (app task) in
      M.on_get p1 "/a" (`Json {|["one"]|}) ;
      equal string "set" (current ()) ;
      P.stop p1 ;
      equal string "none" (current ()) ;
      (* The state stays readable, and a reply raises. *)
      equal int 0 (List.length (M.requests p1)) ;
      raises_match (Exn_match.invalid_arg "Http_mock") (fun () ->
          M.on_get p1 "/b" `Never ) ;
      P.stop p1 ;
      (* A new page has an empty mock. *)
      let p2 = P.start (app task) in
      go p2 ;
      P.run p2 ;
      equal string "network Http_mock: no reply for GET /a" (text p2) ;
      (* A second page of the same host: the mock stays until its stop. *)
      let p3 = P.start ~host:(P.host p2) (app task) in
      M.on_get p3 "/a" (`Json {|["three"]|}) ;
      P.stop p2 ;
      equal string "set" (current ()) ;
      go p3 ;
      P.run p3 ;
      equal string "three" (text p3) ;
      (* The last host wins: a page of another host stops the mock of p3. *)
      let p4 = P.start (app task) in
      raises_match (Exn_match.invalid_arg "Http_mock") (fun () ->
          M.on_get p3 "/a" `Never ) ;
      equal int ~msg:"p2 and p3 share the mock of their host" 2
        (List.length (M.requests p3)) ;
      P.stop p3 ;
      equal string "set" (current ()) ;
      M.on_get p4 "/a" (`Json {|["four"]|}) ;
      go p4 ;
      P.run p4 ;
      equal string "four" (text p4) ;
      (* The stop does not clear the transport of another library. *)
      Http.Private.set_transport
        (Some {send= (fun _ -> Async.return (Error "other"))}) ;
      P.stop p4 ;
      equal string "set" (current ()) )

let tests =
  [ group "tasks"
      [ test "get_json gives the decoded body" json_ok
      ; test "each reply, request and the response headers" replies
      ; test "a request with no reply: unmatched, a network error" unmatched
      ; test "a request of an init task is unmatched" init_unmatched
      ; test "the methods, the body, the headers and the URLs" methods
      ; test "a URL with a scheme stays" schemes
      ; test "once and delay" once_and_delay
      ; test "a timeout aborts the request at the exact time" timeout
      ; test "a reply before the timeout" in_time
      ; test "a reply after the timeout" late
      ; test "Cmd.latest with three keys: two aborts, one message" latest
      ; test "an unmount aborts the request, and the model is free" unmount
      ; test "no transport: a Failure" no_transport
      ; test "Page.stop stops the mock of its host" stop_hook
      ; test "a stop aborts a request, and the mock tells it" stop_aborts ] ]
