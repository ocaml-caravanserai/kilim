(* The tests of kilim.router with History_mock (design sections 06 and 14). *)

open Windtrap
open Kilim
module Router = Kilim_router
module H = Kilim_testing.Host
module P = Kilim_testing.Page
module F = Kilim_testing.Fake_dom
module By = Kilim_testing.By
module M = Kilim_testing.History_mock

type route = Home | User of int | Other of string list

let parse (l : Url.location) =
  match l.path with
  | [] ->
      Home
  | ["users"; n] -> (
    match int_of_string_opt n with Some n -> User n | None -> Other l.path )
  | p ->
      Other p

let loc path = {Url.path; query= []; fragment= None}

let print = function
  | Home ->
      loc []
  | User n ->
      loc ["users"; string_of_int n]
  | Other p ->
      loc p

let show = function
  | Home ->
      "home"
  | User n ->
      "user " ^ string_of_int n
  | Other p ->
      "other /" ^ String.concat "/" p

let router = Router.make ~base:"/app" ~parse ~print ()

type msg = Go of route | Replace of route | Back | Routed of route | Done

type model = {route: route; routed: int; dones: int}

(* The app: the route of the URL, the count of the Routed messages and of the
   results of the tasks. *)
let app ?(r = router) ?(subs = true) () =
  App.make
    ~init:(fun () ->
      ( {route= Other ["start"]; routed= 0; dones= 0}
      , Cmd.task (Async.map (fun x -> Routed x) (Router.current r)) ) )
    ~update:(fun m -> function
      | Go x ->
          (m, Cmd.task (Async.map (fun () -> Done) (Router.push r x)))
      | Replace x ->
          (m, Cmd.task (Async.map (fun () -> Done) (Router.replace r x)))
      | Back ->
          (m, Cmd.task (Async.map (fun () -> Done) (Router.back ())))
      | Routed x ->
          ({m with route= x; routed= m.routed + 1}, Cmd.none)
      | Done ->
          ({m with dones= m.dones + 1}, Cmd.none) )
    ~view:(fun m ->
      El.div
        [ El.p ~at:[At.data "testid" "route"] [txt (show m.route)]
        ; El.p ~at:[At.data "testid" "routed"] [txt (string_of_int m.routed)]
        ; El.p ~at:[At.data "testid" "dones"] [txt (string_of_int m.dones)]
        ; Router.link r
            ~at:[At.data "testid" "home"]
            ~on:(fun x -> Go x)
            Home
            [txt "home"]
        ; Router.link r
            ~at:[At.data "testid" "u42"]
            ~on:(fun x -> Go x)
            (User 42)
            [El.span ~at:[At.data "testid" "in"] [txt "user"]]
        ; El.button
            ~at:[At.data "testid" "rep"]
            ~ev:[Ev.click (Replace (User 7))]
            [txt "replace"]
        ; El.button ~at:[At.data "testid" "back"] ~ev:[Ev.click Back] [txt "b"]
        ] )
    ~subs:(fun _ ->
      if subs then Router.changes r (fun x -> Routed x) else Sub.none )
    ()

let start ?(url = "/app") ?r ?subs () =
  let host = H.create () in
  M.set_url host url ;
  P.start ~host (app ?r ?subs ())

let text p id = F.text (P.find p (By.test_id id))

let route p = text p "route"

let routed p = int_of_string (text p "routed")

(* The end of a test: no error, and the stop of the page. *)
let no_errors p =
  equal int ~msg:"no error" 0 (List.length (H.errors (P.host p))) ;
  P.stop p

(* With no driver, a task fails. The next page sets the driver again. *)
let no_driver () =
  Router.Private.set_history None ;
  is_true ~msg:"no driver" (Router.Private.history () = None) ;
  let host = H.create () in
  let got = ref None in
  let q = Private.Run_queue.create ~schedule:ignore in
  Private.Async.start ~host:(H.host host) ~run:q (Private.Cancel.root ())
    (Router.current router)
    ~ok:(fun _ -> got := Some "ok")
    ~error:(function Failure s -> got := Some s | _ -> got := Some "other") ;
  Private.Run_queue.drain q ;
  equal (option string)
    (Some
       "kilim.router: no history: call Kilim_router_browser.install () in \
        main.ml, or use History_mock in tests" )
    !got

let bases () =
  List.iter
    (fun b ->
      raises
        (Invalid_argument ("Kilim_router.make: the base " ^ b))
        (fun () -> ignore (Router.make ~base:b ~parse ~print ())) )
    [ "app"
    ; "/app/"
    ; "//app"
    ; "//"
    ; "a/"
    ; "/my app"
    ; "/\\evil.example"
    ; "/a?b"
    ; "/a#b"
    ; "/a%20b"
    ; "/\xc3\xa9" ] ;
  equal string "/A-z.0_~/b"
    ( Router.href (Router.make ~base:"/A-z.0_~/b" ~parse ~print ()) Home
    |> fun s -> String.sub s 0 (String.length s - 1) ) ;
  let href base x = Router.href (Router.make ~base ~parse ~print ()) x in
  equal string "/users/1" (href "" (User 1)) ;
  equal string "/users/1" (href "/" (User 1)) ;
  equal string "/" (href "" Home) ;
  equal string "/a/b/users/1" (href "/a/b" (User 1)) ;
  equal string "/app/" (Router.href router Home) ;
  equal string "/app/.//x" (Router.href router (Other [""; "x"])) ;
  equal string "/app/a%20b%2F" (Router.href router (Other ["a b/"]))

(* The route of each URL: below the base, the location with no base. *)
let routes () =
  List.iter
    (fun (url, expected) ->
      let p = start ~url () in
      equal string ~msg:url expected (route p) ;
      P.stop p )
    [ ("/app", "home")
    ; ("/app/", "home")
    ; ("/app?x=1", "home")
    ; ("/app#top", "home")
    ; ("/app/users/42", "user 42")
    ; ("/app/users/42?tab=a#f", "user 42")
    ; ("/app/a%20b", "other /a b")
    ; ("/apple", "other /apple")
    ; ("/ap", "other /ap")
    ; ("/", "home")
    ; ("/x/app", "other /x/app") ]

(* Router.current in init of the App: the first render shows the route. *)
let current_root () =
  let p = start ~url:"/app/users/3" () in
  equal string "user 3" (route p) ;
  equal int ~msg:"one render" 1 (P.stats p).renders ;
  equal int ~msg:"one view" 1 (P.stats p).views ;
  equal int 1 (routed p) ;
  no_errors p

(* In init of a nested component, the result comes after the first render, in
   the same turn: two renders. *)
module Child = struct
  include Component.Defaults

  type props = unit

  type model = route option

  type msg = route

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init () = (None, Cmd.task (Router.current router))

  let update _ () _ x = (Some x, Cmd.none)

  let view () m =
    El.p
      ~at:[At.data "testid" "child"]
      [txt (match m with Some x -> show x | None -> "none")]
end

let current_child () =
  let host = H.create () in
  M.set_url host "/app/users/5" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () -> Component.v (module Child) ~on:(function _ -> .) ())
         () )
  in
  equal string "user 5" (text p "child") ;
  equal int ~msg:"two renders in one turn" 2 (P.stats p).max_renders ;
  equal int ~msg:"one turn" 1 (P.stats p).turns ;
  no_errors p

(* A plain click on a link: one push, one Routed message and one render in the
   turn of the click. *)
let push () =
  let p = start () in
  let renders = (P.stats p).renders in
  M.follow p (By.test_id "u42") ;
  equal string "user 42" (route p) ;
  equal int 2 (routed p) ;
  equal string "1" (text p "dones") ;
  equal int ~msg:"one render" (renders + 1) (P.stats p).renders ;
  is_true ~msg:"prevented" (P.prevented p) ;
  equal (list string) [] (M.loads p) ;
  equal string "/app/users/42" (M.url p) ;
  equal (list string) ["/app"; "/app/users/42"] (M.entries p) ;
  equal int 1 (M.index p) ;
  equal int ~msg:"one scroll" 1 (M.scrolls p) ;
  (* A push of the URL of the page: a new entry, and no message. *)
  M.follow p (By.test_id "in") ;
  equal int ~msg:"no message" 2 (routed p) ;
  equal string "2" (text p "dones") ;
  equal (list string) ["/app"; "/app/users/42"; "/app/users/42"] (M.entries p) ;
  equal int 2 (M.scrolls p) ;
  (* A push after a back removes the entries after the index. *)
  M.back p ;
  P.advance p (Span.ms 100) ;
  M.back p ;
  P.advance p (Span.ms 100) ;
  equal int 0 (M.index p) ;
  M.follow p (By.test_id "home") ;
  equal (list string) ["/app"; "/app/"] (M.entries p) ;
  equal int 1 (M.index p) ;
  no_errors p

let replace () =
  let p = start () in
  P.click p (By.test_id "rep") ;
  equal string "user 7" (route p) ;
  equal int 2 (routed p) ;
  equal (list string) ["/app/users/7"] (M.entries p) ;
  equal int 0 (M.index p) ;
  equal int ~msg:"no scroll" 0 (M.scrolls p) ;
  P.click p (By.test_id "rep") ;
  equal int ~msg:"the same URL: no message" 2 (routed p) ;
  equal string "2" (text p "dones") ;
  no_errors p

(* Back: the popstate event comes in a raw macrotask, and its message renders at
   the next frame. *)
let back () =
  let p = start () in
  M.follow p (By.test_id "u42") ;
  P.click p (By.test_id "back") ;
  equal string "2" (text p "dones") ;
  equal string ~msg:"no change in the click" "user 42" (route p) ;
  equal int 1 (M.index p) ;
  P.run p ;
  equal int ~msg:"the popstate task" 0 (M.index p) ;
  equal string ~msg:"no render yet" "user 42" (route p) ;
  P.advance p (Span.ms 16) ;
  equal string "home" (route p) ;
  equal int 3 (routed p) ;
  (* At the first entry, back does nothing. *)
  M.back p ;
  P.advance p (Span.ms 100) ;
  equal int 0 (M.index p) ;
  equal int 3 (routed p) ;
  no_errors p

(* A popstate to the same URL gives no message. *)
let same_url () =
  let p = start () in
  M.follow p (By.test_id "home") ;
  M.follow p (By.test_id "home") ;
  equal int 2 (routed p) ;
  M.back p ;
  P.advance p (Span.ms 100) ;
  equal int 1 (M.index p) ;
  equal int ~msg:"no message" 2 (routed p) ;
  M.back p ;
  P.advance p (Span.ms 100) ;
  equal string "home" (route p) ;
  equal int ~msg:"/app and /app/: two URLs" 3 (routed p) ;
  no_errors p

(* A click with a modifier key or the middle button: no message, no push, and
   the browser loads the URL. *)
let modifiers () =
  let p = start () in
  M.follow p ~ctrl:true (By.test_id "u42") ;
  is_false ~msg:"Ctrl: not prevented" (P.prevented p) ;
  M.follow p ~button:1 (By.test_id "in") ;
  M.follow p ~meta:true ~shift:true ~alt:true (By.test_id "home") ;
  equal int 1 (routed p) ;
  equal string "home" (route p) ;
  equal (list string) ["/app"] (M.entries p) ;
  equal (list string) ["/app/users/42"; "/app/users/42"; "/app/"] (M.loads p) ;
  equal int 0 (M.scrolls p) ;
  no_errors p

(* follow: a load of the href of the nearest link when no handler prevents the
   default. *)
let follow () =
  let host = H.create () in
  M.set_url host "/start" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n () -> (n + 1, Cmd.none))
         ~view:(fun n ->
           El.div
             [ El.a
                 ~at:[At.href "/out"; At.data "testid" "out"]
                 [El.span ~at:[At.data "testid" "in"] [txt "out"]]
             ; El.a ~at:[At.data "testid" "nohref"] [txt "x"]
             ; El.button
                 ~at:[At.data "testid" "b"]
                 ~ev:[Ev.click ()]
                 [txt (string_of_int n)]
             ; El.a
                 ~at:[At.href "/p"; At.data "testid" "p"]
                 ~ev:[Ev.click ~prevent_default:true ()]
                 [txt "p"] ] )
         () )
  in
  M.follow p (By.test_id "out") ;
  M.follow p (By.test_id "in") ;
  M.follow p (By.test_id "nohref") ;
  M.follow p (By.test_id "b") ;
  M.follow p (By.test_id "p") ;
  equal (list string) ["/out"; "/out"] (M.loads p) ;
  equal string ~msg:"the clicks ran" "2" (text p "b") ;
  equal string "/start" (M.url p) ;
  equal (list string) ["/start"] (M.entries p) ;
  no_errors p

(* Two routers: the key of changes comes from the base, and a push of one router
   notifies the subscriptions of the two. *)
let push_both () =
  let other = Router.make ~parse ~print () in
  let host = H.create () in
  let seen = ref [] in
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () x ->
           match x with
           | `Push r ->
               ( ()
               , Cmd.task (Async.map (fun () -> `Done) (Router.push router r))
               )
           | `Done ->
               ((), Cmd.none)
           | x ->
               seen := x :: !seen ;
               ((), Cmd.none) )
         ~view:(fun () ->
           El.button
             ~at:[At.data "testid" "b"]
             ~ev:[Ev.click (`Push (User 9))]
             [] )
         ~subs:(fun () ->
           Sub.batch
             [ Router.changes router (fun x -> `App x)
             ; Router.changes other (fun x -> `Root x) ] )
         () )
  in
  (* Two keys: two subscriptions. *)
  equal int 2 (Router.Private.listeners ()) ;
  P.click p (By.test_id "b") ;
  is_true ~msg:"the two subscriptions"
    ( List.sort compare !seen
    = List.sort compare [`App (User 9); `Root (Other ["app"; "users"; "9"])] ) ;
  P.stop p ;
  equal int 0 (Router.Private.listeners ())

(* The cancel of a subscription removes its check, and a new start adds it. *)
let restart () =
  let host = H.create () in
  M.set_url host "/app" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> (true, Cmd.none))
         ~update:(fun on () -> (not on, Cmd.none))
         ~view:(fun _ ->
           El.button ~at:[At.data "testid" "t"] ~ev:[Ev.click ()] [] )
         ~subs:(fun on ->
           if on then Router.changes router (fun _ -> ()) else Sub.none )
         () )
  in
  equal int 1 (Router.Private.listeners ()) ;
  P.click p (By.test_id "t") ;
  equal int ~msg:"the cancel removes the check" 0 (Router.Private.listeners ()) ;
  P.click p (By.test_id "t") ;
  equal int 1 (Router.Private.listeners ()) ;
  P.stop p ;
  equal int 0 (Router.Private.listeners ())

(* A log of the messages of an app, the oldest first. *)
let names = function
  | Go _ ->
      "Go"
  | Replace _ ->
      "Replace"
  | Back ->
      "Back"
  | Routed x ->
      "Routed " ^ show x
  | Done ->
      "Done"

(* The order of the messages of a push in a click: Go, Routed, Done. *)
let order () =
  let log = ref [] in
  let host = H.create () in
  M.set_url host "/app" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           log := names m :: !log ;
           match m with
           | Go x ->
               ((), Cmd.task (Async.map (fun () -> Done) (Router.push router x)))
           | _ ->
               ((), Cmd.none) )
         ~view:(fun () ->
           Router.link router
             ~at:[At.data "testid" "u"]
             ~on:(fun x -> Go x)
             (User 1) [] )
         ~subs:(fun () -> Router.changes router (fun x -> Routed x))
         () )
  in
  equal (list string) ~msg:"no message at the start" [] !log ;
  P.click p (By.test_id "u") ;
  equal (list string) ["Go"; "Routed user 1"; "Done"] (List.rev !log) ;
  no_errors p

(* A redirect: init gives current, and update replaces an unknown route before
   the subscription starts. The subscription gives the new route. *)
let redirect_app () =
  App.make
    ~init:(fun () ->
      ( Other ["start"]
      , Cmd.task (Async.map (fun x -> Routed x) (Router.current router)) ) )
    ~update:(fun r -> function
      | Routed (Other _) ->
          (r, Cmd.task (Async.map (fun () -> Done) (Router.replace router Home)))
      | Routed x ->
          (x, Cmd.none)
      | _ ->
          (r, Cmd.none) )
    ~view:(fun r -> El.p ~at:[At.data "testid" "route"] [txt (show r)])
    ~subs:(fun _ -> Router.changes router (fun x -> Routed x))
    ()

let redirect () =
  let host = H.create () in
  M.set_url host "/app/nowhere" ;
  let p = P.start ~host (redirect_app ()) in
  equal string "home" (route p) ;
  equal string "/app/" (M.url p) ;
  equal (list string) ["/app/"] (M.entries p) ;
  no_errors p

(* A replace with the URL of the page before the subscription starts: no change,
   thus no message. *)
let same_before () =
  let host = H.create () in
  M.set_url host "/app/" ;
  let n = ref 0 in
  let p =
    P.start ~host
      (App.make
         ~init:(fun () ->
           ( ()
           , Cmd.batch
               [ Cmd.task (Async.map (fun x -> Routed x) (Router.current router))
               ; Cmd.task
                   (Async.map (fun () -> Done) (Router.replace router Home)) ]
           ) )
         ~update:(fun () m ->
           (match m with Routed _ -> incr n | _ -> ()) ;
           ((), Cmd.none) )
         ~view:(fun () -> txt "")
         ~subs:(fun () -> Router.changes router (fun x -> Routed x))
         () )
  in
  equal int ~msg:"only the Routed of current" 1 !n ;
  no_errors p

(* A push, then current, in init: current saw the push, thus the subscription
   gives no message when it starts. The next page has a new driver, with no
   change: no message either. *)
let push_then_current () =
  let n = ref 0 in
  let app init =
    App.make
      ~init:(fun () -> ((), init))
      ~update:(fun () m ->
        (match m with Routed _ -> incr n | _ -> ()) ;
        ((), Cmd.none) )
      ~view:(fun () -> txt "")
      ~subs:(fun () -> Router.changes router (fun x -> Routed x))
      ()
  in
  let host = H.create () in
  M.set_url host "/app" ;
  let p =
    P.start ~host
      (app
         (Cmd.batch
            [ Cmd.task (Async.map (fun () -> Done) (Router.push router (User 1)))
            ; Cmd.task (Async.map (fun x -> Routed x) (Router.current router))
            ] ) )
  in
  equal int ~msg:"only the Routed of current" 1 !n ;
  no_errors p ;
  n := 0 ;
  let q = P.start (app Cmd.none) in
  equal int ~msg:"a new driver: no message" 0 !n ;
  no_errors q

(* A subscription that stops and starts with no change of the URL gives no
   message: after a push, and after a back. *)
let remount () =
  let log = ref [] in
  let host = H.create () in
  M.set_url host "/app" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () ->
           ( true
           , Cmd.task (Async.map (fun x -> Routed x) (Router.current router)) ) )
         ~update:(fun on m ->
           log := names m :: !log ;
           match m with
           | Go x ->
               (on, Cmd.task (Async.map (fun () -> Done) (Router.push router x)))
           | Back ->
               (not on, Cmd.none)
           | _ ->
               (on, Cmd.none) )
         ~view:(fun _ ->
           El.div
             [ Router.link router
                 ~at:[At.data "testid" "u"]
                 ~on:(fun x -> Go x)
                 (User 1) []
             ; El.button ~at:[At.data "testid" "t"] ~ev:[Ev.click Back] [] ] )
         ~subs:(fun on ->
           if on then Router.changes router (fun x -> Routed x) else Sub.none )
         () )
  in
  let toggle2 () =
    P.click p (By.test_id "t") ;
    P.click p (By.test_id "t")
  in
  P.click p (By.test_id "u") ;
  toggle2 () ;
  equal (list string)
    ["Routed home"; "Go"; "Routed user 1"; "Done"; "Back"; "Back"]
    (List.rev !log) ;
  log := [] ;
  M.back p ;
  P.advance p (Span.ms 100) ;
  toggle2 () ;
  equal (list string) ["Routed home"; "Back"; "Back"] (List.rev !log) ;
  no_errors p

(* A push in init, with no current: the subscription gives the route. *)
let push_init () =
  let host = H.create () in
  M.set_url host "/app" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () ->
           ( Other ["start"]
           , Cmd.task (Async.map (fun () -> Done) (Router.push router (User 3)))
           ) )
         ~update:(fun r -> function
           | Routed x -> (x, Cmd.none) | _ -> (r, Cmd.none) )
         ~view:(fun r -> El.p ~at:[At.data "testid" "route"] [txt (show r)])
         ~subs:(fun _ -> Router.changes router (fun x -> Routed x))
         () )
  in
  equal string "user 3" (route p) ;
  equal string "/app/users/3" (M.url p) ;
  no_errors p

(* An update that adds the changes leaf and pushes in the same turn. *)
let leaf_and_push () =
  let routed = ref 0 in
  let host = H.create () in
  M.set_url host "/app" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> ((false, Other ["start"]), Cmd.none))
         ~update:(fun (on, r) -> function
           | Go x ->
               ( (true, r)
               , Cmd.task (Async.map (fun () -> Done) (Router.push router x)) )
           | Routed x ->
               incr routed ;
               ((on, x), Cmd.none)
           | Back ->
               ((not on, r), Cmd.none)
           | _ ->
               ((on, r), Cmd.none) )
         ~view:(fun (_, r) ->
           El.div
             [ El.p ~at:[At.data "testid" "route"] [txt (show r)]
             ; El.button
                 ~at:[At.data "testid" "go"]
                 ~ev:[Ev.click (Go (User 4))]
                 []
             ; El.button ~at:[At.data "testid" "t"] ~ev:[Ev.click Back] [] ] )
         ~subs:(fun (on, _) ->
           if on then Router.changes router (fun x -> Routed x) else Sub.none )
         () )
  in
  P.click p (By.test_id "go") ;
  equal string "user 4" (route p) ;
  equal int 1 !routed ;
  (* The start gave the route: a stop and a start give no message. *)
  P.click p (By.test_id "t") ;
  P.click p (By.test_id "t") ;
  equal int 1 !routed ;
  no_errors p

(* A URL that is not the URL of the last current, with no push: a subscription
   that starts later gives the route. History_mock.set_url after the mount
   changes the live state. *)
let moved () =
  let host = H.create () in
  M.set_url host "/app/users/1" ;
  let p =
    P.start ~host
      (App.make
         ~init:(fun () ->
           ( (false, Other ["start"])
           , Cmd.task (Async.map (fun x -> Routed x) (Router.current router)) ) )
         ~update:(fun (on, r) -> function
           | Back ->
               ((not on, r), Cmd.none)
           | Routed x ->
               ((on, x), Cmd.none)
           | _ ->
               ((on, r), Cmd.none) )
         ~view:(fun (_, r) ->
           El.div
             [ El.p ~at:[At.data "testid" "route"] [txt (show r)]
             ; El.button ~at:[At.data "testid" "t"] ~ev:[Ev.click Back] [] ] )
         ~subs:(fun (on, _) ->
           if on then Router.changes router (fun x -> Routed x) else Sub.none )
         () )
  in
  equal string "user 1" (route p) ;
  M.set_url host "/app/users/2" ;
  equal string "/app/users/2" (M.url p) ;
  P.click p (By.test_id "t") ;
  equal string "user 2" (route p) ;
  no_errors p

(* set_url after the mount: the live subscription sees the new URL. *)
let set_url_live () =
  let host = H.create () in
  M.set_url host "/app" ;
  let p = P.start ~host (app ()) in
  M.set_url host "/app/users/5" ;
  equal (list string) ["/app/users/5"] (M.entries p) ;
  equal int 0 (M.index p) ;
  M.follow p (By.test_id "home") ;
  equal string "home" (route p) ;
  equal int ~msg:"one listener" 1 (M.listeners p) ;
  equal (list string) ["/app/users/5"; "/app/"] (M.entries p) ;
  no_errors p

(* 100 mounts and unmounts: no subscription and no popstate listener stays. *)
let leak () =
  for _ = 1 to 100 do
    let p = start () in
    M.follow p (By.test_id "u42") ;
    equal int 1 (Router.Private.listeners ()) ;
    equal int ~msg:"one popstate listener" 1 (M.listeners p) ;
    P.stop p ;
    equal int 0 (Router.Private.listeners ()) ;
    equal int ~msg:"no popstate listener" 0 (M.listeners p) ;
    M.back p ;
    P.advance p (Span.ms 100) ;
    equal int ~msg:"no error after the stop" 0
      (List.length (H.errors (P.host p)))
  done

(* The page of an older host has no mock. *)
let old_page () =
  let p = start () in
  let q = start () in
  raises (Failure "Kilim_testing.History_mock: the page is not the last page")
    (fun () -> ignore (M.url p) ) ;
  equal string "/app" (M.url q) ;
  P.stop p ;
  P.stop q

(* A page with no set_url starts at "/". A set_url of another host does not
   change the next page. *)
let default_url () =
  let other = H.create () in
  M.set_url other "/app/users/8" ;
  let p = P.start (app ()) in
  equal string "/" (M.url p) ;
  equal string ~msg:"/ is not below /app: the full location" "home" (route p) ;
  P.stop p ;
  let host = H.create () in
  M.set_url host "/app/users/8" ;
  let p = P.start ~host (app ()) in
  let q = P.start ~host (app ()) in
  equal string ~msg:"the same host keeps its URL" "user 8" (route q) ;
  P.stop p ;
  P.stop q

(* The model of an unmounted instance with a changes subscription is free: the
   mock removes the listener. *)
module Holder = struct
  include Component.Defaults

  type props = unit

  type model = Bytes.t

  type msg = route

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let models = Weak.create 1

  let init () =
    let m = Bytes.make 64 'x' in
    Weak.set models 0 (Some m) ; (m, Cmd.none)

  let update _ () m _ = (m, Cmd.none)

  let view () m = txt (string_of_int (Bytes.length m))

  let subs () m =
    Router.changes router (fun x -> if Bytes.length m > 0 then x else x)
end

let weak () =
  let host = H.create () in
  let p =
    P.start ~host
      (App.make
         ~init:(fun () -> (true, Cmd.none))
         ~update:(fun _ () -> (false, Cmd.none))
         ~view:(fun on ->
           El.div
             [ El.button ~at:[At.data "testid" "b"] ~ev:[Ev.click ()] []
             ; ( if on then
                   Component.v (module Holder) ~on:(function _ -> .) ()
                 else txt "" ) ] )
         () )
  in
  Gc.full_major () ;
  is_true ~msg:"live" (Weak.check Holder.models 0) ;
  P.click p (By.test_id "b") ;
  Gc.full_major () ;
  is_false ~msg:"free" (Weak.check Holder.models 0) ;
  P.stop p

let tests =
  [ group "router"
      [ test "no driver: a task fails" no_driver
      ; test "make checks the base, href" bases
      ; test "the route of a URL, below the base" routes
      ; test "current in the App: one render" current_root
      ; test "current in a child: two renders in one turn" current_child
      ; test "push: one message and one render in the click" push
      ; test "replace" replace
      ; test "back: popstate in a macrotask, render at the frame" back
      ; test "a popstate to the same URL gives no message" same_url
      ; test "Ctrl and the middle button: no message" modifiers
      ; test "History_mock.follow loads the href" follow
      ; test "two routers: a push notifies each subscription" push_both
      ; test "a cancel removes the subscription" restart
      ; test "the order of the messages of a push" order
      ; test "a redirect before the subscription starts" redirect
      ; test "a replace of the same URL in init: no message" same_before
      ; test "a push in init" push_init
      ; test "a stop and a start with no change: no message" remount
      ; test "a push, then current, in init" push_then_current
      ; test "a changes leaf and a push in one turn" leaf_and_push
      ; test "a URL that moved after current" moved
      ; test "set_url after the mount" set_url_live
      ; test "100 mounts and stops: no listener" leak
      ; test "the page of an older host" old_page
      ; test "the default URL, and set_url of a host" default_url
      ; test "weak: the model of an unmounted instance is free" weak ] ]

let () = exit (Windtrap.run "router" tests)
