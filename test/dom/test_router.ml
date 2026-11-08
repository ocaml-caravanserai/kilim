(* The tests of kilim.router.browser and Kilim_web.Scroll in real browsers:
   Chrome, Firefox and WebKit (design sections 06 and 13), in the js_of_ocaml
   and wasm_of_ocaml builds, in the dev and the release profile. The router has
   the base of the page, /page/<prog>: run.mjs serves the page for each path
   below it. test/dom/guards.txt lists the guards of kilim.router.browser and of
   Kilim_web.Scroll with the test that fails when a guard changes. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private
module Router = Kilim_router

type route = Home | User of int | Other of string list

let parse (l : Url.location) =
  match l.path with
  | [] ->
      Home
  | ["users"; n] -> (
    match int_of_string_opt n with Some n -> User n | None -> Other l.path )
  | p ->
      Other p

let print r =
  let path =
    match r with
    | Home ->
        []
    | User n ->
        ["users"; string_of_int n]
    | Other p ->
        p
  in
  {Url.path; query= []; fragment= None}

let show = function
  | Home ->
      "home"
  | User n ->
      "user " ^ string_of_int n
  | Other p ->
      "other /" ^ String.concat "/" p

let location k = Jv.to_string (Jv.get (Jv.get Jv.global "location") k)

let history_length () = int (Jv.get (Jv.get Jv.global "history") "length")

(* The base: the path of the page at the start. *)
let base = location "pathname"

let router = Router.make ~base ~parse ~print ()

type msg = Go of route | Replace of route | Back | Routed of route | Done

let box = At.style "display: block; width: 80px; height: 30px"

let app () =
  App.make
    ~init:(fun () ->
      ( (Other ["start"], 0)
      , Cmd.task (Async.map (fun r -> Routed r) (Router.current router)) ) )
    ~update:(fun (r, n) -> function
      | Go x ->
          ((r, n), Cmd.task (Async.map (fun () -> Done) (Router.push router x)))
      | Replace x ->
          ( (r, n)
          , Cmd.task (Async.map (fun () -> Done) (Router.replace router x)) )
      | Back ->
          ((r, n), Cmd.task (Async.map (fun () -> Done) (Router.back ())))
      | Routed x ->
          ((x, n + 1), Cmd.none)
      | Done ->
          ((r, n), Cmd.none) )
    ~view:(fun (r, n) ->
      El.div
        [ El.p ~at:[At.data "testid" "route"] [txt (show r)]
        ; El.p ~at:[At.data "testid" "n"] [txt (string_of_int n)]
        ; Router.link router
            ~at:[At.data "testid" "u42"; box]
            ~on:(fun x -> Go x)
            (User 42)
            [txt "user 42"]
        ; El.button
            ~at:[At.data "testid" "rep"; box]
            ~ev:[Ev.click (Replace (User 7))]
            [txt "replace"]
        ; El.button
            ~at:[At.data "testid" "back"; box]
            ~ev:[Ev.click Back]
            [txt "back"] ] )
    ~subs:(fun _ -> Router.changes router (fun x -> Routed x))
    ()

let start () =
  let root = container () in
  (root, B.start (el root) (app ()))

let prevent ev = ignore (Jv.call ev "preventDefault" [||])

(* R1 to R5: push, replace, back, a Ctrl click and a middle click. *)
let r1_r5 () =
  let root, a = start () in
  let route () = text (q root "route") and n () = text (q root "n") in
  check "R1 the start route" (route () = "home" && n () = "1") ;
  let popstates = ref 0 in
  let on_pop = listen window "popstate" (fun _ -> incr popstates) in
  let len = history_length () in
  let* () = click (q root "u42") in
  check "R1 a click on the link: the route in the turn of the click"
    (route () = "user 42" && n () = "2") ;
  check "R1 push: the location"
    (location "pathname" = base ^ "/users/42" && location "search" = "") ;
  check "R1 push: one more entry" (history_length () = len + 1) ;
  let* () = click (q root "rep") in
  check "R2 replace: the route" (route () = "user 7" && n () = "3") ;
  check "R2 replace: the location" (location "pathname" = base ^ "/users/7") ;
  check "R2 replace: no new entry" (history_length () = len + 1) ;
  check "R1 R2 no popstate for push and replace" (!popstates = 0) ;
  let* () = click (q root "back") in
  let* () = until (fun () -> route () = "home") in
  check "R3 back: the route comes with popstate"
    (route () = "home" && n () = "4" && !popstates = 1) ;
  check "R3 back: the location of the start" (location "pathname" = base) ;
  (* A window listener prevents the default after the app: thus the browser
     opens no tab. *)
  let on_click = listen window "click" prevent in
  let on_aux = listen window "auxclick" prevent in
  let* () = click ~modifiers:ctrl (q root "u42") in
  let* () = drive [] in
  check "R4 a Ctrl click: no route, no push"
    (route () = "home" && n () = "4" && location "pathname" = base) ;
  let* () = click ~button:"middle" (q root "u42") in
  let* () = drive [] in
  check "R5 a middle click: no route, no push"
    (route () = "home" && n () = "4" && location "pathname" = base) ;
  check "R4 R5 no new entry" (history_length () = len + 1) ;
  unlisten window "click" on_click ;
  unlisten window "auxclick" on_aux ;
  unlisten window "popstate" on_pop ;
  B.stop a ;
  Fut.return ()

(* R6: the browser refuses a URL of another origin and a URL that is not valid:
   push and replace write a warning, and the location stays. *)
let r6 () =
  let h = Option.get (Router.Private.history ()) in
  let before = h.location () in
  let len = history_length () in
  let warns = ref 0 in
  let console = Jv.get Jv.global "console" in
  let warn = Jv.get console "warn" in
  Jv.set console "warn" (Jv.callback ~arity:1 (fun _ -> incr warns)) ;
  let ok f = match f () with () -> true | exception _ -> false in
  let pushed = ok (fun () -> h.push "http://other.example/x") in
  let replaced = ok (fun () -> h.replace "http://other.example/x") in
  let bad = ok (fun () -> h.push "http://[") in
  Jv.set console "warn" warn ;
  check "R6 no exception" (pushed && replaced && bad) ;
  check "R6 three warnings" (!warns = 3) ;
  check "R6 the location and the history stay"
    (h.location () = before && history_length () = len) ;
  Fut.return ()

let scroll_y () = Jv.to_float (Jv.get Jv.global "scrollY")

(* W7: Kilim_web.Scroll on a page that is tall enough. Then R7: a push scrolls
   to the top, and a replace does not. *)
let w7_r7 () =
  let tall = container () in
  Jv.set (Jv.get tall "style") "height" (Jv.of_string "5000px") ;
  let root = Jv.get document "documentElement" in
  let range =
    int (Jv.get root "scrollHeight") - int (Jv.get Jv.global "innerHeight")
  in
  check "W7 the page is tall enough" (range >= 1000) ;
  Kilim_web.Scroll.to_ ~left:0. ~top:500. () ;
  check "W7 to_ ~top:500: at once" (scroll_y () = 500.) ;
  Kilim_web.Scroll.to_ ~left:0. ~top:1e9 () ;
  check "W7 the limit of the range" (scroll_y () = float range) ;
  Kilim_web.Scroll.to_ ~smooth:true ~left:0. ~top:300. () ;
  let at_once = scroll_y () = 300. in
  check "W7 smooth: not at once" (not at_once) ;
  let* () = until (fun () -> scroll_y () = 300.) in
  check "W7 smooth: the position at the end" (scroll_y () = 300.) ;
  let h = Option.get (Router.Private.history ()) in
  h.replace (base ^ "/r7") ;
  check "R7 replace: no scroll" (scroll_y () = 300.) ;
  h.push (base ^ "/r7/push") ;
  check "R7 push: the top" (scroll_y () = 0.) ;
  h.replace (base ^ "/r7/a%20b?q=1#f") ;
  check "R7 location: the path, the query and the fragment"
    (h.location () = base ^ "/r7/a%20b?q=1#f") ;
  ignore (Jv.call tall "remove" [||]) ;
  Fut.return ()

(* The leak test: 20 starts and stops. After each stop, no popstate listener and
   no subscription stays. *)
let leak () =
  let rec loop i ok =
    if i = 0 then ok
    else
      let root, a = start () in
      let live =
        count ~name:"popstate" window = 1 && Router.Private.listeners () = 1
      in
      B.stop a ;
      ignore (Jv.call root "remove" [||]) ;
      loop (i - 1)
        ( ok && live
        && count ~name:"popstate" window = 0
        && Router.Private.listeners () = 0 )
  in
  check "R8 20 starts and stops: no popstate listener stays" (loop 20 true) ;
  Fut.return ()

let () =
  ignore (Lazy.force spy) ;
  Kilim_router_browser.install () ;
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     let* () = r1_r5 () in
     let* () = r6 () in
     let* () = w7_r7 () in
     leak () )
    finish
