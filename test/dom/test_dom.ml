(* The DOM tests of kilim.browser in a real Chrome (design sections 11 and 14),
   in the js_of_ocaml and wasm_of_ocaml builds. run.mjs gives trusted input from
   DevTools. A check "in the same dispatch" reads the DOM in a bubble listener
   on window for the same event: it runs after the listener of the mount
   element. test/dom/guards.txt lists the guards of kilim.browser with the test
   that fails when a guard changes. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private
module R = Private.Runtime

let stats a = R.stats (B.runtime a)

let ins a = R.inspect (B.runtime a)

(* A new app in a new container. *)
let start app =
  let root = container () in
  (root, B.start (el root) app)

(* The children of [n]. *)
let children n = int (Jv.get (Jv.get n "childNodes") "length")

let button ?(id = "b") msg label =
  El.button ~at:[At.data "testid" id] ~ev:[Ev.click msg] [txt label]

(* A box with a size, for the mouse. *)
let box_style = At.style "display: block; width: 60px; height: 30px"

(* The counter of D1 and D2. *)
type cmsg = Inc | Dec

let counter () =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m -> function
      | Inc -> (m + 1, Cmd.none) | Dec -> (m - 1, Cmd.none) )
    ~view:(fun m ->
      El.div
        [ button ~id:"dec" Dec "-"
        ; El.span ~at:[At.data "testid" "n"] [txt (string_of_int m)]
        ; button ~id:"inc" Inc "+" ] )
    ()

(* D1: run in the state "loading" waits for the element. A missing element after
   the wait goes to the error handler. *)
let d1_before () =
  check "D1 the document is loading"
    (String.equal (Jv.to_string (Jv.get document "readyState")) "loading") ;
  check "D1 no #app yet" (Jv.is_null (by_id "app")) ;
  Kilim_browser.run ~mount:"app" (counter ()) ;
  Kilim_browser.run ~mount:"missing-after-wait" (counter ())

let d1_after errors_before =
  let app = by_id "app" in
  check "D1 the app is in #app" (String.equal (text (q app "n")) "0") ;
  check "D1 the placeholder is gone"
    (Jv.is_null (Jv.call app "querySelector" [|Jv.of_string "p"|])) ;
  check "D1 a missing element after the wait goes to the error handler"
    (console_errors () = errors_before + 1) ;
  check "D1 the wait listeners remove themselves"
    (count ~name:"readystatechange" document = 0) ;
  let raised =
    match Kilim_browser.run ~mount:"missing" (counter ()) with
    | () ->
        false
    | exception Invalid_argument _ ->
        true
  in
  check "D1 a missing element when the document is loaded raises" raised ;
  let found = container () in
  Jv.set found "id" (Jv.of_string "found") ;
  Kilim_browser.run ~mount:"found" (counter ()) ;
  check "D1 an element that exists: the app starts at once"
    (String.equal (text (q found "n")) "0") ;
  Fut.return ()

(* D2: two trusted clicks. In the window click listener, the text is already
   "1", then "2". *)
let d2 () =
  let root, a = start (counter ()) in
  let seen = ref [] in
  let fn =
    listen window "click" (fun _ ->
        seen := (text (q root "n"), (stats a).renders) :: !seen )
  in
  let* () = click (q root "inc") in
  let* () = click (q root "inc") in
  unlisten window "click" fn ;
  check "D2 the text in the same dispatch, one render for each click"
    (List.rev !seen = [("1", 2); ("2", 3)]) ;
  B.stop a ;
  Fut.return ()

(* D3: the delegation. *)
type dmsg = Inner | Outer | Wrap | Submitted | Div of string

let d3 () =
  let got = ref [] in
  let outer = container () in
  let root = Jv.call document "createElement" [|Jv.of_string "div"|] in
  ignore (Jv.call outer "appendChild" [|root|]) ;
  let a =
    B.start (el root)
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           got := m :: !got ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             ~ev:[Ev.click Outer]
             [ El.button
                 ~at:[At.data "testid" "b"]
                 ~ev:[Ev.click Inner]
                 [El.span ~at:[At.data "testid" "s"] [txt "in"]]
             ; El.button
                 ~at:[At.data "testid" "stop"]
                 ~ev:[Ev.click ~stop:true Inner]
                 [txt "stop"]
             ; El.form
                 ~at:[At.data "testid" "f"]
                 ~ev:[Ev.submit ~prevent_default:true Submitted]
                 [ El.button
                     ~at:[At.data "testid" "go"; At.type_ "submit"]
                     [txt "go"] ]
             ; El.div
                 ~at:[At.data "testid" "d"]
                 ~ev:[Ev.input (fun s -> Div s)]
                 []
             ; El.svg ~at:[At.data "testid" "svg"] [] ] )
         () )
  in
  check "D3 one listener on the mount element for each delegated name"
    ( List.for_all
        (fun n -> count ~name:n root = 1)
        Private.Event.delegated_events
    && count root = 10 ) ;
  check "D3 no listener on a button" (count (q root "b") = 0) ;
  check "D3 an svg element in the SVG namespace"
    (String.equal
       (Jv.to_string (Jv.get (q root "svg") "namespaceURI"))
       "http://www.w3.org/2000/svg" ) ;
  (* A cell with a click handler above the mount element: the walk stops at the
     mount element. *)
  (B.ops a).attach outer
    { Private.Dom.events=
        Private.Dom.Events ([Ev.click Wrap], fun m -> got := m :: !got)
    ; value= None
    ; checked= None
    ; composing= false } ;
  let* () = click (q root "s") in
  check "D3 a click in a span runs the button handler, then the outer one"
    (List.rev !got = [Inner; Outer]) ;
  got := [] ;
  let doc_clicks = ref 0 in
  let fn = listen document "click" (fun _ -> incr doc_clicks) in
  let* () = click (q root "stop") in
  unlisten document "click" fn ;
  check "D3 stop: no outer handler and no document listener"
    (List.rev !got = [Inner] && !doc_clicks = 0) ;
  got := [] ;
  let prevented = ref false in
  let fn =
    listen window "submit" (fun ev ->
        prevented := Jv.to_bool (Jv.get ev "defaultPrevented") )
  in
  let* () = click (q root "go") in
  let* () = task () in
  unlisten window "submit" fn ;
  check "D3 prevent_default on submit, no navigation"
    (!prevented && List.mem Submitted !got && not (Jv.is_null (by_id "app"))) ;
  got := [] ;
  dispatch (q root "d")
    (Jv.new' (Jv.get Jv.global "Event")
       [|Jv.of_string "input"; Jv.obj [|("bubbles", Jv.true')|]|] ) ;
  check "D3 the value of a div is empty" (List.rev !got = [Div ""]) ;
  B.stop a ;
  Fut.return ()

(* D4: the direct listeners. *)
type emsg = Enter | Leave | Scroll | Drop | Remove_box

let d4 () =
  (* The mouse goes away first: a box that comes under the mouse gets a
     mouseenter from Chrome, with no input, at a time that Chrome selects. *)
  let* () = move_xy (1., 1.) in
  let got = ref [] in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((true, true), Cmd.none))
         ~update:(fun (h, b) m ->
           got := m :: !got ;
           match m with
           | Drop ->
               ((false, b), Cmd.none)
           | Remove_box ->
               ((h, false), Cmd.none)
           | _ ->
               ((h, b), Cmd.none) )
         ~view:(fun (h, b) ->
           El.div
             ( [ El.div
                   ~at:[At.data "testid" "box"; box_style]
                   ~ev:
                     ( if h then [Ev.mouseenter Enter; Ev.mouseleave Leave]
                       else [] )
                   [txt "box"]
               ; El.div ~at:[box_style] [txt "away"]
               ; button ~id:"drop" Drop "drop"
               ; button ~id:"rm" Remove_box "rm" ]
             @
             if b then
               [ El.div
                   ~at:
                     [ At.data "testid" "sc"
                     ; At.style "height: 30px; overflow: auto" ]
                   ~ev:[Ev.scroll Scroll]
                   [El.div ~at:[At.style "height: 300px"] [txt "long"]] ]
             else [] ) )
         () )
  in
  let box = q root "box" in
  let* () = move_to box in
  let* () = move_xy (1., 1.) in
  check "D4 mouseenter, then mouseleave" (List.rev !got = [Enter; Leave]) ;
  check "D4 two listeners on the box" (count box = 2) ;
  let* () = click (q root "drop") in
  check "D4 a patch removes the listeners" (count box = 0) ;
  got := [] ;
  let sc = q root "sc" in
  Jv.set sc "scrollTop" (Jv.of_int 100) ;
  ignore (Jv.call (q root "rm") "click" [||]) ;
  let* () = frame () in
  let* () = frame () in
  check "D4 no scroll message after the removal"
    (List.rev !got = [Remove_box] && not (Jv.to_bool (Jv.get sc "isConnected"))) ;
  B.stop a ;
  Fut.return ()

(* D5: a wheel listener is passive with no prevent_default. *)
let d5 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (false, Cmd.none))
         ~update:(fun _ m -> (m, Cmd.none))
         ~view:(fun p ->
           El.div
             [ El.div
                 ~at:[At.data "testid" "w"; box_style]
                 ~ev:[Ev.wheel ~prevent_default:p true]
                 [txt "wheel"]
             ; button ~id:"p" true "p" ] )
         () )
  in
  let w = q root "w" in
  check "D5 passive with no prevent_default"
    (Jv.strict_equal (passive w "wheel") Jv.true') ;
  let* () = click (q root "p") in
  check "D5 not passive with prevent_default"
    (Jv.strict_equal (passive w "wheel") Jv.false' && count w = 1) ;
  let prevented = ref false in
  let fn =
    listen window "wheel" (fun ev ->
        prevented := Jv.to_bool (Jv.get ev "defaultPrevented") )
  in
  let* () = wheel w in
  unlisten window "wheel" fn ;
  check "D5 a trusted wheel is prevented" !prevented ;
  B.stop a ;
  Fut.return ()

(* D6 and D7: a controlled input. In the window input listener, the value is
   already the view value. *)
let text_input f =
  App.make
    ~init:(fun () -> ("", Cmd.none))
    ~update:(fun m s -> (f m s, Cmd.none))
    ~view:(fun m ->
      El.input
        ~at:[At.data "testid" "i"; At.value m]
        ~ev:[Ev.input (fun s -> s)]
        () )
    ()

let input_case name f typed expected =
  let root, a = start (text_input f) in
  let i = q root "i" in
  focus i ;
  let seen = ref "?" in
  let fn = listen window "input" (fun _ -> seen := value i) in
  let* () = insert_text typed in
  unlisten window "input" fn ;
  check
    (name ^ ": the value in the same dispatch is " ^ expected)
    (String.equal !seen expected) ;
  B.stop a ;
  Fut.return ()

let d6 () = input_case "D6 a rejected input" (fun m _ -> m) "x" ""

let d7 () =
  input_case "D7 an uppercase transform"
    (fun _ s -> String.uppercase_ascii s)
    "ab" "AB"

(* D8 and D9: a controlled checkbox. *)
let checkbox ?(prevent = false) ~accept () =
  let got = ref [] in
  ( got
  , App.make
      ~init:(fun () -> (false, Cmd.none))
      ~update:(fun m b ->
        got := b :: !got ;
        ((if accept then b else m), Cmd.none) )
      ~view:(fun m ->
        El.input
          ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked m]
          ~ev:[Ev.check ~prevent_default:prevent (fun b -> b)]
          () )
      () )

let d8 () =
  let got, app = checkbox ~accept:false () in
  let root, a = start app in
  let* () = click (q root "c") in
  let* () = task () in
  check "D8 a rejected check: the message gets true, the box is unchecked"
    (!got = [true] && not (checked (q root "c"))) ;
  B.stop a ;
  let got, app = checkbox ~accept:true () in
  let root, a = start app in
  let w = writes "checked" in
  let* () = click (q root "c") in
  let* () = task () in
  check "D8 an accepted check: checked, with no write"
    (!got = [true] && checked (q root "c") && writes "checked" = w) ;
  B.stop a ;
  Fut.return ()

let d9 () =
  let _, app = checkbox ~prevent:true ~accept:true () in
  let root, a = start app in
  let* () = click (q root "c") in
  let after_dispatch = checked (q root "c") in
  let* () = task () in
  log ("D9 checked after the dispatch: " ^ string_of_bool after_dispatch) ;
  check "D9 a prevented check that the model accepts: checked after one task"
    (checked (q root "c")) ;
  B.stop a ;
  Fut.return ()

(* D10: a document listener (not kilim) prevents the click, on a checkbox and on
   a radio button. *)
let prevent_click ev = ignore (Jv.call ev "preventDefault" [||])

let d10 () =
  let _, app = checkbox ~accept:true () in
  let root, a = start app in
  let fn = listen document "click" prevent_click in
  let* () = click (q root "c") in
  unlisten document "click" fn ;
  let* () = task () in
  check "D10 a checkbox click prevented above the app: the DOM follows the view"
    (checked (q root "c")) ;
  B.stop a ;
  let root, a =
    start
      (App.make
         ~init:(fun () -> ("r1", Cmd.none))
         ~update:(fun _ id -> (id, Cmd.none))
         ~view:(fun m ->
           let radio id =
             El.input
               ~at:
                 [ At.data "testid" id
                 ; At.type_ "radio"
                 ; At.name "g10"
                 ; At.checked (String.equal m id) ]
               ~ev:[Ev.check (fun _ -> id)]
               ()
           in
           El.div [radio "r1"; radio "r2"] )
         () )
  in
  let fn = listen document "click" prevent_click in
  let* () = click (q root "r2") in
  unlisten document "click" fn ;
  let* () = task () in
  check "D10 a radio click prevented above the app: the DOM follows the view"
    (checked (q root "r2") && not (checked (q root "r1"))) ;
  B.stop a ;
  (* A link with the type checkbox (a type property that reflects the attribute)
     is not an input: no restore task. *)
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () ->
           El.a
             ~at:[At.data "testid" "l"; At.type_ "checkbox"]
             ~ev:[Ev.click ()]
             [txt "link"] )
         () )
  in
  let r = (stats a).restores in
  let* () = click (q root "l") in
  let* () = task () in
  check "D10 a link with the type checkbox: one restore, no restore task"
    ((stats a).restores = r + 1) ;
  B.stop a ;
  Fut.return ()

(* D11: the radio group. The form owner gives the group: a button with form=f11
   outside the form is in the group of the form, a button with no form owner is
   not. A button with no name has no group. A button of the group outside the
   app has no cell and does not change. Each click is rejected. A test write of
   [checked] (a drift) on a button of another group stays: the restore does not
   write it. *)
let d11 () =
  let radio ?(at = []) ?(name = "g") id on =
    El.input
      ~at:
        ( [At.data "testid" id; At.type_ "radio"; At.checked on]
        @ (if String.equal name "" then [] else [At.name name])
        @ at )
      ~ev:[Ev.check (fun _ -> id)]
      ()
  in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () _ -> ((), Cmd.none))
         ~view:(fun () ->
           El.div
             [ El.form
                 ~at:[At.id "f11"]
                 [ radio "a" true
                 ; radio "b" false
                 ; radio ~name:"h" "h" false
                 ; El.input
                     ~at:
                       [ At.data "testid" "k"
                       ; At.type_ "checkbox"
                       ; At.name "g"
                       ; At.checked false ]
                     () ]
             ; radio ~at:[At.v "form" "f11"] "c" false
             ; radio "d" true
             ; radio "e" false
             ; radio ~name:"" "x" false
             ; radio ~name:"" "y" true ] )
         () )
  in
  let out = Jv.call document "createElement" [|Jv.of_string "input"|] in
  Jv.set out "type" (Jv.of_string "radio") ;
  Jv.set out "name" (Jv.of_string "g") ;
  ignore
    (Jv.call out "setAttribute" [|Jv.of_string "form"; Jv.of_string "f11"|]) ;
  ignore (Jv.call (Jv.get document "body") "appendChild" [|out|]) ;
  let ids = ["a"; "b"; "c"; "d"; "e"; "x"; "y"] in
  let state () =
    String.concat ""
      (List.map (fun id -> if checked (q root id) then id else ".") ids)
    ^ if checked out then "o" else "."
  in
  let set id b = Jv.set (q root id) "checked" (Jv.of_bool b) in
  let click_all id =
    let* () = click (q root id) in
    task ()
  in
  let* () = click_all "b" in
  check "D11 a rejected click in a form: the old button is checked again"
    (String.equal (state ()) "a..d..y.") ;
  let* () = click_all "c" in
  check "D11 a button with form=f11 outside the form is in its group"
    (String.equal (state ()) "a..d..y.") ;
  set "a" false ;
  let* () = click_all "e" in
  check "D11 no form owner: d is in the group of e, the drift of a stays"
    (String.equal (state ()) "...d..y.") ;
  set "a" true ;
  set "d" false ;
  let* () = click_all "b" in
  check "D11 a form owner: d is not in the group of b, its drift stays"
    (String.equal (state ()) "a.....y.") ;
  set "d" true ;
  set "y" false ;
  let* () = click_all "x" in
  check "D11 no name: no group, the drift of y stays"
    (String.equal (state ()) "a..d....") ;
  set "y" true ;
  set "h" true ;
  set "k" true ;
  let* () = click_all "b" in
  check
    "D11 a radio of another name and a checkbox of the same name are not in \
     the group: their drifts stay"
    ( String.equal (state ()) "a..d..y."
    && checked (q root "h")
    && checked (q root "k") ) ;
  set "a" false ;
  let* () = click_all "k" in
  check "D11 a checkbox with the name of the group has no group: a stays"
    (String.equal (state ()) "...d..y.") ;
  set "a" true ;
  ignore (Jv.call out "remove" [||]) ;
  B.stop a ;
  Fut.return ()

(* D12: a prevented click on r2 whose render removes the checked r1. *)
let d12 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (false, Cmd.none))
         ~update:(fun _ _ -> (true, Cmd.none))
         ~view:(fun picked ->
           let radio id on =
             El.input
               ~at:
                 [ At.data "testid" id
                 ; At.type_ "radio"
                 ; At.name "g12"
                 ; At.checked on ]
               ~ev:[Ev.check ~prevent_default:true (fun _ -> ())]
               ()
           in
           El.div
             (radio "r2" false :: (if picked then [] else [radio "r1" true])) )
         () )
  in
  let r2 = q root "r2" in
  let* () = click r2 in
  log ("D12 r2 after the dispatch: " ^ string_of_bool (checked r2)) ;
  let* () = task () in
  check "D12 after one task, the DOM follows the view" (not (checked r2)) ;
  B.stop a ;
  Fut.return ()

(* D13: a select. *)
type smsg = Pick of string | Rows

let d13 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (false, Cmd.none))
         ~update:(fun _ -> function
           | Pick _ -> (false, Cmd.none) | Rows -> (true, Cmd.none) )
         ~view:(fun short ->
           let opt v = El.option ~at:[At.value v] [txt v] in
           El.div
             [ El.select
                 ~at:[At.data "testid" "s"; At.value "b"]
                 ~ev:[Ev.change (fun s -> Pick s)]
                 (List.map opt (if short then ["b"; "c"] else ["a"; "b"; "c"]))
             ; button Rows "rows" ] )
         () )
  in
  let s = q root "s" in
  focus s ;
  let* () = key "ArrowDown" 40 in
  let* () = task () in
  check "D13 a rejected keyboard change: the old option again"
    (String.equal (value s) "b") ;
  ignore (Jv.call (q root "b") "click" [||]) ;
  check "D13 a change of the options: the patch writes the view value"
    (String.equal (value s) "b") ;
  B.stop a ;
  Fut.return ()

(* D14: an IME composition on an uppercase input, with Sub.every. *)
type imsg = Typed of string | Tick

let ime_app () =
  let ticks = ref 0 in
  ( ticks
  , App.make
      ~init:(fun () -> (("", 0), Cmd.none))
      ~update:(fun (s, n) -> function
        | Typed v ->
            ((String.uppercase_ascii v, n), Cmd.none)
        | Tick ->
            incr ticks ;
            ((s, n + 1), Cmd.none) )
      ~view:(fun (s, n) ->
        El.div
          [ El.input
              ~at:[At.data "testid" "i"; At.value s]
              ~ev:[Ev.input (fun v -> Typed v)]
              ()
          ; El.span [txt (string_of_int n)] ] )
      ~subs:(fun _ -> Sub.every (Span.ms 10) Tick)
      () )

let d14 () =
  let ticks, app = ime_app () in
  let root, a = start app in
  let i = q root "i" in
  focus i ;
  let starts = ref 0 and raw = ref [] in
  let f1 = listen window "compositionstart" (fun _ -> incr starts) in
  let f2 =
    listen window "input" (fun ev ->
        if Jv.to_bool (Jv.get ev "isComposing") then raw := value i :: !raw )
  in
  let w = writes "value" in
  let* () = ime "k" in
  let* () = ime "ka" in
  let renders = (stats a).renders and t = !ticks in
  let* () = sleep 60 in
  check "D14 renders during the composition"
    ((stats a).renders > renders && !ticks > t) ;
  check "D14 no value write during the composition" (writes "value" = w) ;
  check "D14 the raw text during the composition" (List.rev !raw = ["k"; "ka"]) ;
  let* () = insert_text "ka" in
  let* () = task () in
  unlisten window "compositionstart" f1 ;
  unlisten window "input" f2 ;
  check "D14 one compositionstart" (!starts = 1) ;
  check "D14 the uppercase text after compositionend"
    (String.equal (value i) "KA") ;
  B.stop a ;
  Fut.return ()

(* D15: a keyed move of the composing input. Chrome sends focusout, but no
   compositionend. *)
type mmsg = Swap | Typed_m of string | More

let d15 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((false, ""), Cmd.none))
         ~update:(fun (sw, s) -> function
           | Swap ->
               ((not sw, s), Cmd.none)
           | Typed_m v ->
               ((sw, String.uppercase_ascii v), Cmd.none)
           | More ->
               ((sw, s ^ "!"), Cmd.none) )
         ~view:(fun (sw, s) ->
           let rows =
             [ ( Key.int 1
               , El.input
                   ~at:[At.data "testid" "i"; At.value s]
                   ~ev:[Ev.input (fun v -> Typed_m v)]
                   () )
             ; (Key.int 2, button ~id:"sw" Swap "swap")
             ; (Key.int 3, button ~id:"more" More "more") ]
           in
           (* The swap moves only the input: the two buttons keep their
              order. *)
           View.keyed El.div (if sw then List.tl rows @ [List.hd rows] else rows) )
         () )
  in
  let i = q root "i" in
  focus i ;
  let* () = ime "ka" in
  let composing () =
    match B.cell i with Some c -> c.Private.Dom.composing | None -> false
  in
  check "D15 composing before the move" (composing ()) ;
  ignore (Jv.call (q root "sw") "click" [||]) ;
  check "D15 the move ends the composition, and the restore writes the value"
    ((not (composing ())) && String.equal (value i) "KA") ;
  let* () = task () in
  ignore (Jv.call (q root "more") "click" [||]) ;
  let* () = task () in
  check "D15 after the move and the focusout, a model change writes the value"
    (String.equal (value i) "KA!") ;
  B.stop a ;
  Fut.return ()

(* D16: a synthetic keydown with isComposing. *)
let d16 () =
  let got = ref 0 in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () _ -> incr got ; ((), Cmd.none))
         ~view:(fun () ->
           El.input ~at:[At.data "testid" "i"] ~ev:[Ev.keydown (fun k -> k)] () )
         () )
  in
  let key composing =
    Jv.new'
      (Jv.get Jv.global "KeyboardEvent")
      [| Jv.of_string "keydown"
       ; Jv.obj
           [| ("key", Jv.of_string "Enter")
            ; ("bubbles", Jv.true')
            ; ("isComposing", Jv.of_bool composing) |] |]
  in
  dispatch (q root "i") (key true) ;
  check "D16 a keydown with isComposing: no message" (!got = 0) ;
  dispatch (q root "i") (key false) ;
  check "D16 a keydown with no composition: one message" (!got = 1) ;
  B.stop a ;
  Fut.return ()

(* D17: a float model, 0. then -0. *)
type fmsg = Neg | Same

let d17 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0., Cmd.none))
         ~update:(fun m -> function
           | Neg -> (-0., Cmd.none) | Same -> (m, Cmd.none) )
         ~view:(fun m ->
           El.div
             [ El.span
                 ~at:[At.data "testid" "t"]
                 [txt (if 1. /. m < 0. then "-0" else "0")]
             ; button ~id:"neg" Neg "neg"
             ; button ~id:"same" Same "same" ] )
         () )
  in
  let* () = click (q root "neg") in
  check "D17 0. to -0.: one render"
    ((stats a).renders = 2 && String.equal (text (q root "t")) "-0") ;
  let* () = click (q root "same") in
  check "D17 the same model: no render" ((stats a).renders = 2) ;
  B.stop a ;
  Fut.return ()

(* D18: the time limit on the real clock. A loop of messages through microtasks
   yields, and a trusted click gets its message within 2 s. *)
type lmsg = Loop | Halt

let d18 () =
  let host = B.host in
  let halted_at = ref None in
  let next () =
    Async.make (fun _ ~ok ~error:_ -> host.queue_microtask (fun () -> ok Loop))
  in
  let root, a =
    start
      (App.make
         ~init:(fun () -> (true, Cmd.task (next ())))
         ~update:(fun running -> function
           | Loop ->
               (running, if running then Cmd.task (next ()) else Cmd.none)
           | Halt ->
               halted_at := Some (Brr.Performance.now_ms Brr.G.performance) ;
               (false, Cmd.none) )
         ~view:(fun _ -> button Halt "halt")
         () )
  in
  let* () = sleep 200 in
  let t0 = Brr.Performance.now_ms Brr.G.performance in
  let* () = click (q root "b") in
  let* () = sleep 100 in
  check "D18 stops and yields" ((stats a).stops >= 1 && (stats a).yields >= 1) ;
  check "D18 the click gets its message within 2 s"
    (match !halted_at with Some t -> t -. t0 < 2000. | None -> false) ;
  B.stop a ;
  Fut.return ()

(* D19: an ok from a raw setTimeout: a microtask turn, then the render at the
   next frame. *)
let raw_after ms f =
  ignore
    (Jv.call Jv.global "setTimeout"
       [|Jv.callback ~arity:1 (fun _ -> f ()); Jv.of_int ms|] )

let microtask f =
  ignore
    (Jv.call Jv.global "queueMicrotask"
       [|Jv.callback ~arity:1 (fun _ -> f ())|] )

let raw_app oks =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m -> function
      | `Wait ->
          (m, Cmd.task (Async.make (fun _ ~ok ~error:_ -> Queue.push ok oks)))
      | `Got n ->
          (m + n, Cmd.none) )
    ~view:(fun m -> button `Wait (string_of_int m))
    ()

let d19 () =
  let oks = Queue.create () in
  let root, a = start (raw_app oks) in
  ignore (Jv.call (q root "b") "click" [||]) ;
  let fut, set = Fut.create () in
  raw_after 20 (fun () ->
      (Queue.pop oks) (`Got 5) ;
      microtask (fun () ->
          let s = stats a in
          check "D19 the message in a microtask turn, the render waits"
            (s.messages = 2 && s.renders = 1 && (ins a).pending) ;
          set () ) ) ;
  let* () = fut in
  let* () = frame () in
  check "D19 the render at the next frame"
    ((stats a).renders = 2 && String.equal (text (q root "b")) "5") ;
  B.stop a ;
  Fut.return ()

(* D20: a hidden page, simulated. *)
let d20 () =
  let oks = Queue.create () in
  let root, a = start (raw_app oks) in
  ignore (Jv.call (q root "b") "click" [||]) ;
  ignore (Jv.call (q root "b") "click" [||]) ;
  let set_hidden h =
    if h then
      ignore
        (js
           "Object.defineProperty(document, 'visibilityState', { get: () => \
            'hidden', configurable: true })" )
    else ignore (js "delete document.visibilityState") ;
    dispatch document
      (Jv.new' (Jv.get Jv.global "Event") [|Jv.of_string "visibilitychange"|])
  in
  let fut, set = Fut.create () in
  raw_after 0 (fun () ->
      (Queue.pop oks) (`Got 1) ;
      microtask (fun () ->
          let r = (stats a).renders in
          set_hidden true ;
          check "D20 a page that becomes hidden renders at once"
            ((stats a).renders = r + 1) ;
          set () ) ) ;
  let* () = fut in
  let fut, set = Fut.create () in
  raw_after 0 (fun () ->
      (Queue.pop oks) (`Got 1) ;
      microtask (fun () -> set ((stats a).renders, (ins a).scheduled)) ) ;
  let* r, scheduled = fut in
  let* () = task () in
  let* () = task () in
  check "D20 a hidden page renders in a new task"
    ( scheduled
    && (stats a).renders = r + 1
    && String.equal (text (q root "b")) "2" ) ;
  set_hidden false ;
  B.stop a ;
  Fut.return ()

(* D21: a view that raises: the fallback, one console.error, no uncaught
   exception. *)
exception Boom

let d21 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () -> (m + 1, Cmd.none))
         ~view:(fun m -> if m = 1 then raise Boom else button () "go")
         ~fallback:(fun _ -> button ~id:"fb" () "fallback")
         () )
  in
  let e = console_errors () in
  let* () = click (q root "b") in
  check "D21 the fallback shows, one console.error"
    (String.equal (text (q root "fb")) "fallback" && console_errors () = e + 1) ;
  let m = (stats a).messages in
  let* () = click (q root "fb") in
  check "D21 a later click does nothing"
    ((stats a).messages = m && console_errors () = e + 1) ;
  B.stop a ;
  Fut.return ()

(* D22: a render removes the focused input that has Ev.blur and a dirty
   value. *)
type bmsg = Typed_b of string | Hide | Blurred

let d22 () =
  let blurs = ref 0 in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((true, "v"), Cmd.none))
         ~update:(fun (shown, s) -> function
           | Typed_b v ->
               ((shown, v), Cmd.none)
           | Hide ->
               ((false, s), Cmd.none)
           | Blurred ->
               incr blurs ;
               ((shown, s), Cmd.none) )
         ~view:(fun (shown, s) ->
           El.div
             ( button ~id:"hide" Hide "hide"
             ::
             ( if shown then
                 [ El.input
                     ~at:[At.data "testid" "i"; At.value s]
                     ~ev:[Ev.input (fun v -> Typed_b v); Ev.blur Blurred]
                     () ]
               else [] ) ) )
         () )
  in
  let i = q root "i" in
  focus i ;
  let* () = insert_text "x" in
  let e = console_errors ()
  and turns = (stats a).turns
  and w = writes "value" in
  ignore (Jv.call (q root "hide") "click" [||]) ;
  let* () = task () in
  check "D22 the input is gone, no error, no value write"
    ( (not (Jv.to_bool (Jv.get i "isConnected")))
    && console_errors () = e
    && writes "value" = w ) ;
  check "D22 one turn for the click, no nested turn"
    ((stats a).turns = turns + 1) ;
  check "D22 at most one blur message" (!blurs <= 1) ;
  log ("D22 blur messages (Chrome only): " ^ string_of_int !blurs) ;
  B.stop a ;
  Fut.return ()

(* D23: two apps side by side, each with a mouseenter whose prevent_default
   follows the model. *)
let d23 () =
  let app name got =
    App.make
      ~init:(fun () -> (false, Cmd.none))
      ~update:(fun p () ->
        got := !got + 1 ;
        (not p, Cmd.none) )
      ~view:(fun p ->
        El.div
          [ El.div
              ~at:[At.data "testid" "box"; box_style]
              ~ev:[Ev.mouseenter ~prevent_default:p ()]
              [txt name]
          ; button () "click" ] )
      ()
  in
  (* The mouse goes away first: a box that comes under the mouse gets a
     mouseenter from Chrome, with no input, at a time that Chrome selects. *)
  let* () = move_xy (1., 1.) in
  let g1 = ref 0 and g2 = ref 0 in
  let r1, a1 = start (app "one" g1) in
  let r2, a2 = start (app "two" g2) in
  let b1 = q r1 "box" and b2 = q r2 "box" in
  let* () = move_to b1 in
  check "D23 one message to app 1" (!g1 = 1 && !g2 = 0) ;
  let* () = move_to b2 in
  check "D23 one message to app 2" (!g1 = 1 && !g2 = 1) ;
  check "D23 one listener for each box, passive toggled"
    ( count b1 = 1
    && count b2 = 1
    && Jv.strict_equal (passive b1 "mouseenter") Jv.false' ) ;
  B.stop a2 ;
  let* () = move_to b1 in
  let* () = click (q r1 "b") in
  check "D23 after the stop of app 2, app 1 still works" (!g1 = 3 && !g2 = 1) ;
  B.stop a1 ;
  Fut.return ()

(* D24: Sub.window. *)
type wmsg = Got of string | Set of int * string

let d24 () =
  let got = ref [] in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((1, "a"), Cmd.none))
         ~update:(fun m -> function
           | Got s ->
               got := s :: !got ;
               (m, Cmd.none)
           | Set (n, t) ->
               ((n, t), Cmd.none) )
         ~view:(fun _ ->
           El.div
             [ button ~id:"two" (Set (2, "a")) "two"
             ; button ~id:"new" (Set (2, "b")) "new"
             ; button ~id:"none" (Set (0, "b")) "none" ] )
         ~subs:(fun (n, t) ->
           Sub.batch
             (List.init n (fun _ ->
                  Kilim_browser.Sub.window "kilim-test" (fun _ -> Got t) ) ) )
         () )
  in
  let fire () =
    dispatch window
      (Jv.new' (Jv.get Jv.global "Event") [|Jv.of_string "kilim-test"|]) ;
    task ()
  in
  let listeners () = count ~name:"kilim-test" window in
  check "D24 one passive window listener"
    (listeners () = 1 && Jv.strict_equal (passive window "kilim-test") Jv.true') ;
  let* () = fire () in
  check "D24 an event gives one message" (!got = ["a"]) ;
  ignore (Jv.call (q root "two") "click" [||]) ;
  got := [] ;
  let* () = fire () in
  check "D24 two with the same name: two listeners, two messages"
    (listeners () = 2 && !got = ["a"; "a"]) ;
  ignore (Jv.call (q root "new") "click" [||]) ;
  got := [] ;
  let* () = fire () in
  check "D24 a kept key uses the newest function"
    (listeners () = 2 && !got = ["b"; "b"]) ;
  ignore (Jv.call (q root "none") "click" [||]) ;
  got := [] ;
  let* () = fire () in
  check "D24 a removed key: no listener, no message"
    (listeners () = 0 && !got = []) ;
  B.stop a ;
  Fut.return ()

(* D25: the stop removes the listeners, the timers and the DOM. *)
let d25 () =
  let doc_vis = count ~name:"visibilitychange" document in
  let root, a = start (counter ()) in
  let inc = q root "inc" in
  check "D25 one visibilitychange listener"
    (count ~name:"visibilitychange" document = doc_vis + 1) ;
  B.stop a ;
  ignore (Jv.call inc "click" [||]) ;
  check "D25 after the stop: no listener, no timer, no DOM"
    ( count root = 0
    && count ~name:"visibilitychange" document = doc_vis
    && B.live_timers () = 0
    && children root = 0
    && (stats a).messages = 0 ) ;
  let tick_app () =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m () -> (m + 1, Cmd.none))
      ~view:(fun m -> button () (string_of_int m))
      ~subs:(fun _ ->
        Sub.batch
          [ Sub.every (Span.ms 1000) ()
          ; Kilim_browser.Sub.window "kilim-cycle" (fun _ -> ()) ] )
      ()
  in
  let root = container () in
  for _ = 1 to 1000 do
    B.stop (B.start (el root) (tick_app ()))
  done ;
  check "D25 1,000 starts and stops: each count 0"
    ( count root = 0
    && count ~name:"kilim-cycle" window = 0
    && count ~name:"visibilitychange" document = doc_vis
    && B.live_timers () = 0
    && children root = 0 ) ;
  Fut.return ()

(* D26: At.value on a progress element: its value property is a number. *)
let d26 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (10, Cmd.none))
         ~update:(fun n () -> (n + 10, Cmd.none))
         ~view:(fun n ->
           El.div
             [ El.progress
                 ~at:
                   [ At.data "testid" "p"
                   ; At.v "max" "100"
                   ; At.value (string_of_int n) ]
                 []
             ; El.div ~at:[At.data "testid" "d"; At.value "undefined"] []
             ; button () (string_of_int n) ] )
         () )
  in
  check "D26 an undefined value is \"\": the mount writes the view value"
    (Jv.strict_equal (Jv.get (q root "d") "value") (Jv.of_string "undefined")) ;
  let e = console_errors () in
  let* () = click (q root "b") in
  let* () = click (q root "b") in
  let p = Jv.to_float (Jv.get (q root "p") "value") in
  check "D26 a progress with a value: the app works, the value follows the view"
    (String.equal (text (q root "b")) "30" && p = 30. && console_errors () = e) ;
  B.stop a ;
  Fut.return ()

(* D27: Ev.input on a contenteditable li: its value property is a number, thus
   the payload is "". *)
let d27 () =
  let got = ref [] in
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n s ->
           got := s :: !got ;
           (n + 1, Cmd.none) )
         ~view:(fun n ->
           El.ul
             [ El.li
                 ~at:[At.data "testid" "li"; At.v "contenteditable" "true"]
                 ~ev:[Ev.input (fun s -> s)]
                 [txt "item"]
             ; El.li ~at:[At.data "testid" "n"] [txt (string_of_int n)] ] )
         () )
  in
  focus (q root "li") ;
  let* () = insert_text "x" in
  check "D27 an input on a contenteditable li: one message with \"\""
    (!got = [""] && String.equal (text (q root "n")) "1") ;
  B.stop a ;
  Fut.return ()

(* D28: a control named "elements" hides form.elements. The radio group does not
   use it. *)
let d28 () =
  let radio id on =
    El.input
      ~at:[At.data "testid" id; At.type_ "radio"; At.name "g28"; At.checked on]
      ~ev:[Ev.check (fun _ -> ())]
      ()
  in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () ->
           El.form
             [ El.input ~at:[At.name "elements"; At.type_ "text"] ()
             ; radio "a" true
             ; radio "b" false ] )
         () )
  in
  let e = console_errors () in
  let* () = click (q root "b") in
  let* () = task () in
  check "D28 a control named elements: the rejected click restores a"
    ( checked (q root "a")
    && (not (checked (q root "b")))
    && console_errors () = e ) ;
  B.stop a ;
  (* One or two controls named __kilim hide the cell of their form (an element
     or a RadioNodeList). The walk ignores the value. *)
  let got = ref 0 in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> incr got ; ((), Cmd.none))
         ~view:(fun () ->
           let named () =
             El.input ~at:[At.name "__kilim"; At.type_ "text"] ()
           in
           El.div
             ~ev:[Ev.click ()]
             [ El.form
                 [named (); El.span ~at:[At.data "testid" "one"] [txt "one"]]
             ; El.form
                 [ named ()
                 ; named ()
                 ; El.span ~at:[At.data "testid" "two"] [txt "two"] ] ] )
         () )
  in
  let* () = click (q root "one") in
  let* () = click (q root "two") in
  check "D28 controls named __kilim: no error, the handlers run"
    (!got = 2 && console_errors () = e) ;
  B.stop a ;
  Fut.return ()

(* D29: a capture listener above the app removes the parent of the target before
   the dispatch reaches the app. *)
let d29 () =
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             ~ev:[Ev.click ()]
             [ El.div [El.span ~at:[At.data "testid" "s"] [txt "x"]]
             ; El.span ~at:[At.data "testid" "n"] [txt (string_of_int m)] ] )
         ~fallback:(fun _ -> El.p ~at:[At.data "testid" "fb"] [txt "fallback"])
         () )
  in
  let s = q root "s" in
  let fn =
    Jv.callback ~arity:1 (fun ev ->
        if Jv.strict_equal (Jv.get ev "target") s then
          ignore (Jv.call (Jv.get s "parentNode") "remove" [||]) )
  in
  let add = [|Jv.of_string "click"; fn; Jv.true'|] in
  ignore (Jv.call document "addEventListener" add) ;
  let e = console_errors () in
  let* () = click s in
  ignore (Jv.call document "removeEventListener" add) ;
  check "D29 an outside removal of the parent of the target: no error"
    (console_errors () = e && String.equal (text (q root "n")) "0") ;
  B.stop a ;
  Fut.return ()

(* D30: one app for each mount element. *)
let d30 () =
  let root = container () in
  let a1 = B.start (el root) (counter ()) in
  let raised =
    match B.start (el root) (counter ()) with
    | _ ->
        false
    | exception Invalid_argument _ ->
        true
  in
  check "D30 a second start on a mount element with a live app raises" raised ;
  B.stop a1 ;
  let a2 = B.start (el root) (counter ()) in
  let* () = click (q root "inc") in
  check "D30 after the stop, a new start works"
    (String.equal (text (q root "n")) "1") ;
  B.stop a1 ;
  let raised =
    match B.start (el root) (counter ()) with
    | _ ->
        false
    | exception Invalid_argument _ ->
        true
  in
  check "D30 a second stop of the old app keeps the mark of the new app" raised ;
  B.stop a2 ;
  Fut.return ()

(* D31: a trap of wasm_of_ocaml in update (an illegal cast) goes past each OCaml
   catch. The app still works: the next click renders, and a task that ends in a
   raw timer gives its message (the run queue is not left in a turn). The
   js_of_ocaml build has no trap, thus it does not make one. *)
type tmsg = Trap | Inc_t | Later | Got_t of int

let d31 () =
  let wasm = Sys.backend_type = Other "wasm_of_ocaml" in
  let later () =
    Async.make (fun _ ~ok ~error:_ -> raw_after 10 (fun () -> ok (Got_t 5)))
  in
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Trap ->
               if wasm && String.equal (Jv.to_string (Jv.obj [||])) "never" then
                 failwith "no trap" ;
               (m + 1, Cmd.none)
           | Inc_t ->
               (m + 1, Cmd.none)
           | Later ->
               (m, Cmd.task (later ()))
           | Got_t n ->
               (m + n, Cmd.none) )
         ~view:(fun m ->
           El.div
             [ button ~id:"trap" Trap "trap"
             ; button ~id:"later" Later "later"
             ; button ~id:"inc" Inc_t (string_of_int m) ] )
         () )
  in
  if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
  let* () = click (q root "trap") in
  let* () = task () in
  if wasm then log "EXPECT END" ;
  let* () = click (q root "inc") in
  let* () = task () in
  let n = text (q root "inc") in
  check
    ("D31 a trap in a turn: the next click renders (" ^ n ^ ")")
    (String.equal n (if wasm then "1" else "2") && not (ins a).in_turn) ;
  let* () = click (q root "later") in
  let* () = sleep 50 in
  let* () = frame () in
  let n = text (q root "inc") in
  check
    ("D31 after the trap, a task gives its message (" ^ n ^ ")")
    (String.equal n (if wasm then "6" else "7")) ;
  B.stop a ;
  Fut.return ()

(* D32: a JS error that is not a trap, in a listener that update reaches by a
   synthetic event, while the turn is open: the browser reports it, and the turn
   goes on (no abort). *)
let d32 () =
  let other = Jv.call document "createElement" [|Jv.of_string "div"|] in
  (* A JS listener: an OCaml callback that raises is not the same case. *)
  ignore
    (Jv.call other "addEventListener"
       [| Jv.of_string "kilim-throw"
        ; js "(function () { throw new Error('listener') })" |] ) ;
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () ->
           dispatch other
             (Jv.new' (Jv.get Jv.global "Event") [|Jv.of_string "kilim-throw"|]) ;
           (m + 1, Cmd.none) )
         ~view:(fun m -> button () (string_of_int m))
         () )
  in
  let e = console_errors () in
  log "EXPECT EXCEPTION Error: listener" ;
  let* () = click (q root "b") in
  log "EXPECT END" ;
  check "D32 a JS error in a nested listener: the turn goes on, no abort"
    (String.equal (text (q root "b")) "1" && console_errors () = e) ;
  B.stop a ;
  Fut.return ()

(* D33: a trap in an OCaml listener of the page (not of kilim) that update
   reaches by a synthetic event, then a click on a button of the app in the same
   update. The trap does not end the turn of the app: the click message waits in
   the queue, thus the result is the same as in the js build. *)
type m33 = Go33 | X33

let d33 () =
  let wasm = Sys.backend_type = Other "wasm_of_ocaml" in
  let other = Jv.call document "createElement" [|Jv.of_string "div"|] in
  let _ =
    listen other "kilim-trap" (fun _ ->
        if wasm && String.equal (Jv.to_string (Jv.obj [||])) "never" then
          failwith "no trap" )
  in
  let rootr = ref Jv.null in
  let root, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Go33 ->
               dispatch other
                 (Jv.new' (Jv.get Jv.global "Event")
                    [|Jv.of_string "kilim-trap"|] ) ;
               ignore (Jv.call (q !rootr "x") "click" [||]) ;
               (m + 100, Cmd.none)
           | X33 ->
               (m + 1, Cmd.none) )
         ~view:(fun m ->
           El.div
             [button ~id:"go" Go33 "go"; button ~id:"x" X33 (string_of_int m)] )
         () )
  in
  rootr := root ;
  if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
  let* () = click (q root "go") in
  let* () = sleep 50 in
  if wasm then log "EXPECT END" ;
  let n = text (q root "x") in
  check
    ("D33 a trap in a listener of the page: the model is 101 (" ^ n ^ ")")
    (String.equal n "101" && not (ins a).in_turn) ;
  B.stop a ;
  Fut.return ()

(* A trap of wasm_of_ocaml (an illegal cast). The js_of_ocaml build has no trap,
   thus it does not make one. *)
let wasm = Sys.backend_type = Other "wasm_of_ocaml"

let trap () =
  if wasm && String.equal (Jv.to_string (Jv.obj [||])) "never" then
    failwith "no trap"

(* D34: a trap in the first turn (in init, in the first view, and in a start in
   a promise reaction) leaves an app that Private.mounted finds and that stop
   ends. Then a new start on the element works. *)
let d34 () =
  let case name ~in_init ~in_view ~promise =
    let app =
      App.make
        ~init:(fun () ->
          if in_init then trap () ;
          (0, Cmd.none) )
        ~update:(fun m () -> (m + 1, Cmd.none))
        ~view:(fun m ->
          if in_view then trap () ;
          button () (string_of_int m) )
        ()
    in
    let root = container () in
    if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
    let go () = ignore (B.start (el root) app) in
    if promise then Fut.await (Fut.return ()) go else raw_after 0 go ;
    let* () = sleep 50 in
    if wasm then log "EXPECT END" ;
    let stopped =
      match B.mounted (el root) with
      | Some a ->
          let open_turn = (ins a).in_turn in
          B.stop a ;
          (not open_turn) && Option.is_none (B.mounted (el root))
      | None ->
          false
    in
    let a = B.start (el root) (counter ()) in
    let* () = click (q root "inc") in
    check
      ("D34 a trap " ^ name ^ ": the app stops, and a new start works")
      (stopped && String.equal (text (q root "n")) "1") ;
    B.stop a ;
    Fut.return ()
  in
  let* () = case "in init" ~in_init:true ~in_view:false ~promise:false in
  let* () =
    case "in the first view" ~in_init:false ~in_view:true ~promise:false
  in
  case "in a start in a promise reaction" ~in_init:true ~in_view:false
    ~promise:true

(* D35: a trap, then a microtask turn of kilim that was already queued, then the
   abort. The work that a raw callback queued after the trap stays in the run
   queue; the abort asks for a turn, which processes it. *)
type m35 = Trap35 | Got35 of int

let d35 () =
  let oks = Queue.create () in
  let wait () = Async.make (fun _ ~ok ~error:_ -> Queue.push ok oks) in
  let root, a =
    start
      (App.make
         ~init:(fun () ->
           ( 0
           , Cmd.batch
               [ Cmd.task (Async.map (fun n -> Got35 n) (wait ()))
               ; Cmd.task (Async.map (fun n -> Got35 n) (wait ())) ] ) )
         ~update:(fun m -> function
           | Trap35 -> trap () ; (m, Cmd.none) | Got35 n -> (m + n, Cmd.none) )
         ~view:(fun m -> button Trap35 (string_of_int m))
         () )
  in
  if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
  (* The first ok asks for a microtask turn. The click traps in its turn, which
     also processes the first result. The second ok only queues: the run queue
     is in the dead turn. Then the microtask turn runs, then the abort. *)
  (Queue.pop oks) 1 ;
  ignore (Jv.call (q root "b") "click" [||]) ;
  (Queue.pop oks) 7 ;
  let* () = task () in
  let* () = frame () in
  if wasm then log "EXPECT END" ;
  let n = text (q root "b") in
  check
    ( "D35 a trap, a queued microtask turn, then the abort: the work goes on ("
    ^ n ^ ")" )
    (String.equal n "8" && not (ins a).in_turn) ;
  B.stop a ;
  Fut.return ()

(* D36: the update of app A clicks a button of app B: a nested turn of another
   runtime. The update of B traps. The turn of A ends normally after it: it must
   not remove the entry of the dead turn of B, thus the abort ends the turn of
   B, and B works again. *)
type m36b = Trap_b | Inc_b

type m36a = Go_a | Inc_a

let d36 () =
  let armed = ref true in
  let rootb, b =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Trap_b ->
               if !armed then begin
                 armed := false ;
                 trap ()
               end ;
               (m + 1000, Cmd.none)
           | Inc_b ->
               (m + 1, Cmd.none) )
         ~view:(fun m ->
           El.div
             [ button ~id:"t" Trap_b "t"
             ; button ~id:"inc" Inc_b (string_of_int m) ] )
         () )
  in
  let roota, a =
    start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | Go_a ->
               ignore (Jv.call (q rootb "t") "click" [||]) ;
               (m + 100, Cmd.none)
           | Inc_a ->
               (m + 1, Cmd.none) )
         ~view:(fun m ->
           El.div
             [ button ~id:"go" Go_a "go"
             ; button ~id:"inc" Inc_a (string_of_int m) ] )
         () )
  in
  if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
  let* () = click (q roota "go") in
  let* () = sleep 50 in
  if wasm then log "EXPECT END" ;
  let* () = click (q rootb "inc") in
  let* () = click (q roota "inc") in
  let* () = sleep 50 in
  let na = text (q roota "inc") and nb = text (q rootb "inc") in
  check
    ( "D36 a trap in a nested turn of app B: A gives 101, B works again (A "
    ^ na ^ ", B " ^ nb ^ ")" )
    ( String.equal na "101"
    && String.equal nb (if wasm then "1" else "1001")
    && (not (ins a).in_turn) && not (ins b).in_turn ) ;
  B.stop a ;
  B.stop b ;
  Fut.return ()

(* D37: the error handler gets each trap one time. The update of A clicks B,
   then C, and the two updates trap: two distinct errors. The update of A starts
   D, and the init of D traps: the trap ends the turns of D and of A, one error.
   The test records the errors with its own handler, then gives the console
   back. *)

let d37 () =
  let errs = ref [] in
  Private.set_error_handler (fun e -> errs := e :: !errs) ;
  let trapper () =
    let armed = ref true in
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () ->
           if !armed then begin
             armed := false ;
             trap ()
           end ;
           ((), Cmd.none) )
         ~view:(fun () -> button () "t")
         () )
  in
  let rootb, b = trapper () in
  let rootc, c = trapper () in
  let roota, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () ->
           ignore (Jv.call (q rootb "b") "click" [||]) ;
           ignore (Jv.call (q rootc "b") "click" [||]) ;
           ((), Cmd.none) )
         ~view:(fun () -> button () "go")
         () )
  in
  if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
  let* () = click (q roota "b") in
  let* () = sleep 50 in
  if wasm then log "EXPECT END" ;
  let two = match !errs with [x; y] -> x != y | [] -> not wasm | _ -> false in
  check "D37 two traps in one task: each error one time" two ;
  B.stop a ;
  B.stop b ;
  B.stop c ;
  errs := [] ;
  let rootd = container () in
  let roota, a =
    start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () ->
           ignore
             (B.start (el rootd)
                (App.make
                   ~init:(fun () -> trap () ; ((), Cmd.none))
                   ~update:(fun () () -> ((), Cmd.none))
                   ~view:(fun () -> El.div [])
                   () ) ) ;
           ((), Cmd.none) )
         ~view:(fun () -> button () "go")
         () )
  in
  if wasm then log "EXPECT EXCEPTION RuntimeError: illegal cast" ;
  let* () = click (q roota "b") in
  let* () = sleep 50 in
  if wasm then log "EXPECT END" ;
  check "D37 one trap that ends two turns: one error"
    (List.length !errs = if wasm then 1 else 0) ;
  Option.iter B.stop (B.mounted (el rootd)) ;
  B.stop a ;
  Private.set_error_handler (fun e ->
      ignore
        (Jv.call console "error"
           [| Jv.of_string "kilim:"
            ; ( match e with
              | Jv.Error j ->
                  Jv.of_error j
              | _ ->
                  Jv.of_string "error" ) |] ) ) ;
  Fut.return ()

(* D38: Unsafe_html.of_trusted. Chrome has trustedTypes on each page, also with
   no CSP. The view reads the value of [src] at each render. With no CSP, a
   string in innerHTML gives the same DOM: test/loader runs the case with
   require-trusted-types-for. *)
let d38 () =
  let policy =
    js "trustedTypes.createPolicy('d38', { createHTML: (s) => s })"
  in
  let html s = Jv.call policy "createHTML" [|Jv.of_string s|] in
  let src = ref (html "<i>x</i>") in
  let app () =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m () -> (m + 1, Cmd.none))
      ~view:(fun _ ->
        El.div
          [ View.unsafe_html
              ~el:(El.div ~at:[At.data "testid" "h"])
              (Kilim_browser.Unsafe_html.of_trusted !src)
          ; button () "go" ] )
      ~fallback:(fun _ -> El.p ~at:[At.data "testid" "fb"] [txt "fallback"])
      ()
  in
  (* The HTML of the element, or "none" if the app has no such element. *)
  let find root id =
    let n =
      Jv.call root "querySelector" [|Jv.of_string ("[data-testid=" ^ id ^ "]")|]
    in
    if Jv.is_null n then None else Some n
  in
  let inner root =
    match find root "h" with
    | Some n ->
        Jv.to_string (Jv.get n "innerHTML")
    | None ->
        "none"
  in
  let root, a = start (app ()) in
  check "D38 a TrustedHTML: the element has its HTML"
    (String.equal (inner root) "<i>x</i>") ;
  let records =
    js
      "(() => { const r = []; const o = new MutationObserver((l) => \
       r.push(...l)); return { o, r }; })()"
  in
  ignore
    (Jv.call (Jv.get records "o") "observe"
       [| q root "h"
        ; Jv.obj
            [| ("childList", Jv.true')
             ; ("subtree", Jv.true')
             ; ("characterData", Jv.true') |] |] ) ;
  let count () =
    let o = Jv.get records "o" and r = Jv.get records "r" in
    ignore (Jv.call r "push" (Jv.to_jv_array (Jv.call o "takeRecords" [||]))) ;
    let n = int (Jv.get r "length") in
    ignore (Jv.call r "splice" [|Jv.of_int 0|]) ;
    n
  in
  src := html "<i>x</i>" ;
  let* () = click (q root "b") in
  check "D38 a new TrustedHTML with the same text: no write" (count () = 0) ;
  src := html "<b>y</b>" ;
  let* () = click (q root "b") in
  check "D38 a new text: one write"
    (count () = 1 && String.equal (inner root) "<b>y</b>") ;
  src := Jv.of_string "<i>x</i>" ;
  let e = console_errors () in
  let* () = click (q root "b") in
  check "D38 a JS string: Invalid_argument, the root fallback"
    (find root "fb" <> None && console_errors () = e + 1) ;
  B.stop a ;
  (* With no trustedTypes (a browser with no Trusted Types), the text is
     written. *)
  let restore =
    js
      "(() => { const d = Object.getOwnPropertyDescriptor(window, \
       'trustedTypes'); Object.defineProperty(window, 'trustedTypes', { value: \
       undefined, configurable: true }); return () => { if (d) \
       Object.defineProperty(window, 'trustedTypes', d); else delete \
       window.trustedTypes; }; })()"
  in
  src := Jv.of_string "<u>z</u>" ;
  let root, a = start (app ()) in
  let shadowed = Jv.is_undefined (Jv.get Jv.global "trustedTypes") in
  ignore (Jv.apply restore [||]) ;
  check "D38 no trustedTypes: a JS string is written"
    (shadowed && String.equal (inner root) "<u>z</u>") ;
  B.stop a ;
  Fut.return ()

let tests =
  [ d2
  ; d3
  ; d4
  ; d5
  ; d6
  ; d7
  ; d8
  ; d9
  ; d10
  ; d11
  ; d12
  ; d13
  ; d14
  ; d15
  ; d16
  ; d17
  ; d18
  ; d19
  ; d20
  ; d21
  ; d22
  ; d23
  ; d24
  ; d25
  ; d26
  ; d27
  ; d28
  ; d29
  ; d30
  ; d31
  ; d32
  ; d33
  ; d34
  ; d35
  ; d36
  ; d37
  ; d38 ]

let () =
  ignore (Lazy.force spy) ;
  d1_before () ;
  let errors = console_errors () in
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     let* () = d1_after errors in
     List.fold_left
       (fun acc t ->
         let* () = acc in
         t () )
       (Fut.return ()) tests )
    finish
