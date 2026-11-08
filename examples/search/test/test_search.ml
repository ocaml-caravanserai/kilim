(* The native test of the search example: Http_mock and History_mock on the
   virtual clock. The first test is the test of design section 14. *)

open Windtrap
open Kilim
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module Host = Kilim_testing.Host
module Fake_dom = Kilim_testing.Fake_dom
module Http_mock = Kilim_testing.Http_mock
module History_mock = Kilim_testing.History_mock
open Search

let test_search () =
  Kilim_testing.mount (module Search) { Search.endpoint = "/api/cities" } (fun page ->
    Http_mock.on_get page "/api/cities?q=par" (`Json {|["Paris"]|});
    Page.type_ page (By.label "City") "par";
    Page.advance page (Span.ms 300);           (* virtual clock: no real wait *)
    Page.click page (By.text "Paris");
    assert (Page.outputs page = [Search.Picked "Paris"]);
    Page.stop page;
    Page.expect_no_tasks page)                 (* the stop cancels all tasks *)
[@@ocamlformat "disable"]

let cities p =
  List.map Fake_dom.text (Fake_dom.children (Page.find p (By.label "Cities")))

let value p = Fake_dom.value (Page.find p (By.label "City"))

let no_errors p =
  equal int ~msg:"no error" 0 (List.length (Host.errors (Page.host p)))

(* Three keys in 250 ms: one request. The result puts the query in the URL, with
   no new entry. *)
let debounce () =
  Kilim_testing.mount
    (module Search)
    {Search.endpoint= "/api/cities"}
    (fun p ->
      Http_mock.on_get p "/api/cities?q=par" (`Json {|["Paris","Parma"]|}) ;
      Page.type_ p (By.label "City") "par" ;
      Page.advance p (Span.ms 249) ;
      equal int ~msg:"no request yet" 0 (List.length (Http_mock.requests p)) ;
      Page.advance p (Span.ms 50) ;
      equal (list string) ["Paris"; "Parma"] (cities p) ;
      equal int ~msg:"one request" 1 (List.length (Http_mock.requests p)) ;
      equal string "/?q=par" (History_mock.url p) ;
      equal int ~msg:"no new entry" 1 (List.length (History_mock.entries p)) ;
      Http_mock.expect_done p ;
      no_errors p )

(* A key press during a request aborts it. *)
let cancel () =
  Kilim_testing.mount
    (module Search)
    {Search.endpoint= "/api/cities"}
    (fun p ->
      Http_mock.on_get p "/api/cities?q=p" `Never ;
      Http_mock.on_get p "/api/cities?q=pa" (`Json {|["Paris"]|}) ;
      Page.type_ p (By.label "City") "p" ;
      Page.advance p (Span.ms 300) ;
      equal int 1 (Http_mock.pending p) ;
      Page.type_ p (By.label "City") "a" ;
      equal int 1 (Http_mock.aborted p) ;
      Page.advance p (Span.ms 300) ;
      equal (list string) ["Paris"] (cities p) ;
      equal string "/?q=pa" (History_mock.url p) ;
      no_errors p )

let failed p =
  match Page.find p (By.text "The search failed.") with
  | _ ->
      true
  | exception Page.Query _ ->
      false

let busy p = Fake_dom.attribute (Page.find p (By.label "Cities")) "aria-busy"

(* A failed request shows a line, and the next result removes it. *)
let error () =
  Kilim_testing.mount
    (module Search)
    {Search.endpoint= "/api/cities"}
    (fun p ->
      Http_mock.on_get p "/api/cities?q=x" (`Status (500, "")) ;
      Http_mock.on_get p "/api/cities?q=xy" (`Json "[]") ;
      Page.type_ p (By.label "City") "x" ;
      equal (option string) (Some "true") (busy p) ;
      Page.advance p (Span.ms 300) ;
      is_true ~msg:"the error line" (failed p) ;
      equal (option string) (Some "false") (busy p) ;
      equal (list string) [] (cities p) ;
      Page.type_ p (By.label "City") "y" ;
      Page.advance p (Span.ms 300) ;
      is_false ~msg:"no error line" (failed p) ;
      no_errors p )

(* A deep link loads its results. *)
let deep_link () =
  let host = Host.create () in
  History_mock.set_url host "/?q=par" ;
  Kilim_testing.mount ~host
    (module Search)
    {Search.endpoint= "/api/cities"}
    (fun p ->
      equal string "par" (value p) ;
      Http_mock.on_get p "/api/cities?q=par" (`Json {|["Paris","Parma"]|}) ;
      Page.advance p (Span.ms 300) ;
      equal (list string) ["Paris"; "Parma"] (cities p) ;
      equal string "/?q=par" (History_mock.url p) ;
      no_errors p )

(* A link pushes a query, and the back button gives the query before it. *)
let back () =
  let p = Page.start app in
  Fun.protect
    ~finally:(fun () -> Page.stop p)
    (fun () ->
      Http_mock.on_get p "/api/cities?q=par" (`Json {|["Paris","Parma"]|}) ;
      Http_mock.on_get p "/api/cities?q=lon" (`Json {|["London"]|}) ;
      Page.type_ p (By.label "City") "par" ;
      Page.advance p (Span.ms 300) ;
      equal (list string) ["Paris"; "Parma"] (cities p) ;
      Page.click p (By.text "London") ;
      equal string "/?q=lon" (History_mock.url p) ;
      equal string "lon" (value p) ;
      Page.advance p (Span.ms 300) ;
      equal (list string) ["London"] (cities p) ;
      History_mock.back p ;
      Page.advance p (Span.ms 300) ;
      equal string "/?q=par" (History_mock.url p) ;
      equal string "par" (value p) ;
      equal (list string) ["Paris"; "Parma"] (cities p) ;
      equal (list string) ["/?q=par"; "/?q=lon"] (History_mock.entries p) ;
      Page.click p (By.text "Paris") ;
      equal string "Picked: Paris"
        (Fake_dom.text (Page.find p (By.test_id "picked"))) ;
      equal int ~msg:"three requests" 3 (List.length (Http_mock.requests p)) ;
      no_errors p )

let () =
  exit
    (Windtrap.run "search"
       [ group "search"
           [ test "the test of design section 14" test_search
           ; test "the debounce, and the query in the URL" debounce
           ; test "a key press during a request aborts it" cancel
           ; test "a failed request shows a line" error
           ; test "a deep link loads its results" deep_link
           ; test "a link, then back" back ] ] )
