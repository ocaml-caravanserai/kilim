(* The queries of Page (design section 14): by text, by label and by test id. *)

open Windtrap
open Kilim
open Support

let page v =
  P.start
    (App.make
       ~init:(fun () -> ((), Cmd.none))
       ~update:(fun m () -> (m, Cmd.none))
       ~view:(fun () -> v)
       () )

let query q = raises_match (function P.Query _ -> true | _ -> false) q

let id p n = Option.value ~default:"" (F.attribute (P.find p n) "id")

(* The deepest element with the text: the white space collapses, and nested
   spans join their text. *)
let by_text () =
  let p =
    page
      (El.div
         [ El.div ~at:[At.id "outer"] [El.p ~at:[At.id "deep"] [txt "Paris"]]
         ; El.button
             ~at:[At.id "b"]
             [txt "  Save\n\t"; El.span [txt "the"]; txt "  file "]
         ; El.p ~at:[At.id "two"] [txt "Lyon "; txt " Nice"]
         ; El.p ~at:[At.id "cr"] [txt "Rome\r\rBern"]
         ; El.p ~at:[At.id "ff"] [txt "Oslo\012Riga"]
         ; El.span [txt "dup"]
         ; El.span [txt "dup"] ] )
  in
  equal string "deep" (id p (By.text "Paris")) ;
  equal string "deep" (id p (By.text " Paris ")) ;
  equal string "two" (id p (By.text "Lyon Nice")) ;
  equal string "cr" (id p (By.text "Rome Bern")) ;
  equal string "ff" (id p (By.text "Oslo Riga")) ;
  equal string "b" (id p (By.text "Save the file")) ;
  query (fun () -> P.find p (By.text "Pari")) ;
  query (fun () -> P.find p (By.text "dup")) ;
  query (fun () -> P.find p (By.test_id "none")) ;
  query (fun () ->
      P.find
        (page
           (El.div
              [ El.p ~at:[At.data "testid" "t"] []
              ; El.p ~at:[At.data "testid" "t"] [] ] ) )
        (By.test_id "t") )

(* A query finds only the nodes of the app, never the mount element. *)
let not_root () =
  let p = page (txt "hi") in
  query (fun () -> P.find p (By.text "hi")) ;
  let p = page (El.p [txt "hi"]) in
  is_true (P.find p (By.text "hi") != P.root p) ;
  equal string "p" (F.name (P.find p (By.text "hi")))

(* The control of a label: by its for attribute, or the first control inside it.
   Also an element with this aria-label. *)
let by_label () =
  let p =
    page
      (El.div
         [ El.label ~at:[At.for_ "city"] [txt " City "]
         ; El.input ~at:[At.id "city"] ()
         ; El.label
             [ txt "Name"
             ; El.span [El.textarea ~at:[At.id "name"] []]
             ; El.input ~at:[At.id "second"] () ]
         ; El.button
             ~at:[At.id "close"; At.aria "label" "Close  dialog"]
             [txt "x"]
         ; El.label [txt "Empty"]
         ; El.label ~at:[At.for_ "nothing"] [txt "Lost"]
         ; El.label [txt "Twice"; El.select ~at:[At.id "s1"] []]
         ; El.input ~at:[At.id "s2"; At.aria "label" "Twice"] ()
         ; El.label ~at:[At.for_ "same"] [txt "Same"]
         ; El.input ~at:[At.id "same"; At.aria "label" "Same"] () ] )
  in
  equal string "city" (id p (By.label "City")) ;
  equal string "name" (id p (By.label "Name")) ;
  equal string "close" (id p (By.label "Close dialog")) ;
  equal string "same" (id p (By.label "Same")) ;
  query (fun () -> P.find p (By.label "Empty")) ;
  query (fun () -> P.find p (By.label "Lost")) ;
  query (fun () -> P.find p (By.label "Twice")) ;
  query (fun () -> P.find p (By.label "Nobody"))

(* A query drives an event: type into a field by its label, then click by
   text. *)
type msg = Typed of string | Pick

let drive () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> ("", Cmd.none))
         ~update:(fun m -> function
           | Typed s -> (s, Cmd.none) | Pick -> (m ^ "!", Cmd.none) )
         ~view:(fun m ->
           El.div
             [ El.label
                 [ txt "City"
                 ; El.input
                     ~at:[At.value m]
                     ~ev:[Ev.input (fun s -> Typed s)]
                     () ]
             ; El.button ~ev:[Ev.click Pick] [txt ("Go " ^ m)] ] )
         () )
  in
  P.type_ p (By.label "City") "par" ;
  P.click p (By.text "Go par") ;
  equal string "par!" (F.value (P.find p (By.label "City"))) ;
  no_errors p

let tests =
  [ group "queries"
      [ test "by text: the deepest element" by_text
      ; test "a query never gives the mount element" not_root
      ; test "by label: for, a nested control, aria-label" by_label
      ; test "a query drives the events" drive ] ]
