(* The diff on the fake DOM (design sections 11 and 14). *)

open Windtrap
open Kilim
open Support

let ops = list (pair string int)

let html p = F.to_html p.root

let mount_html () =
  let p =
    mount
      (El.div
         ~at:[At.id "a"]
         [txt "x<"; El.br (); El.span ~at:[At.title "\"t\""] [txt "y"]] )
  in
  equal string
    "<div><div id=\"a\">x&lt;<br><span \
     title=\"&quot;t&quot;\">y</span></div></div>"
    (html p) ;
  equal ops
    [ ("attach", 3)
    ; ("create_element", 3)
    ; ("create_text", 2)
    ; ("insert_before", 5)
    ; ("set_attribute", 2) ]
    (counts p) ;
  check_tree p

let mount_order () =
  let log = ref [] in
  let doc = F.create () in
  let root = F.element "div" in
  ignore
    (Diff.mount
       (logged (F.ops doc) log)
       ~parent:root
       ~post:(Private.Dom.post_now ignore)
       (El.div
          ~at:[At.id "i"; At.type_ "t"; At.value "v"; At.checked true]
          ~ev:[Ev.scroll "s"; Ev.click "c"]
          [txt "x"] ) ) ;
  equal (list string)
    [ "create div"
    ; "attach"
    ; "attr type"
    ; "attr id"
    ; "listen scroll"
    ; "insert"
    ; "insert"
    ; "get value"
    ; "prop"
    ; "get checked"
    ; "prop" ]
    (List.rev !log)

let replace () =
  let p = mount (El.div [El.p [txt "a"]; txt "b"]) in
  let old = kids p in
  patch p (El.div [El.span [txt "a"]; txt "b"]) ;
  equal ops
    [ ("attach", 1)
    ; ("create_element", 1)
    ; ("create_text", 1)
    ; ("insert_before", 1)
    ; ("replace", 1) ]
    (counts p) ;
  is_false (List.hd old == List.hd (kids p)) ;
  is_true (List.nth old 1 == List.nth (kids p) 1) ;
  equal string "<div><div><span>a</span>b</div></div>" (html p) ;
  check_tree p ;
  (* Text and element at the same place. *)
  patch p (El.div [txt "a"; El.b [txt "b"]]) ;
  equal string "<div><div>a<b>b</b></div></div>" (html p) ;
  check_tree p ;
  (* The root. *)
  let r = node p in
  patch p (txt "z") ;
  equal ops [("create_text", 1); ("replace", 1)] (counts p) ;
  is_false (r == node p) ;
  equal string "<div>z</div>" (html p) ;
  patch p (El.div []) ;
  equal string "<div><div></div></div>" (html p) ;
  check_tree p

let text () =
  let p = mount (El.p [txt "a"]) in
  let t = List.hd (kids p) in
  patch p (El.p [txt "b"]) ;
  equal ops [("set_text", 1)] (counts p) ;
  is_true (t == List.hd (kids p)) ;
  patch p (El.p [txt "b"]) ;
  equal ops [] (counts p) ;
  patch p (El.p [txt "a"]) ;
  equal ops [("set_text", 1)] (counts p) ;
  equal string "<div><p>a</p></div>" (html p) ;
  is_true
    ( match List.hd (Option.get (Diff.children (Diff.root p.tree))) with
    | Diff.Child c ->
        Diff.children c = None )

let attributes () =
  let p = mount (El.div ~at:[At.id "a"; At.title "t"] []) in
  patch p (El.div ~at:[At.id "b"; At.class_ "c"] []) ;
  equal ops [("remove_attribute", 1); ("set_attribute", 2)] (counts p) ;
  equal string "<div><div class=\"c\" id=\"b\"></div></div>" (html p) ;
  patch p (El.div ~at:[At.class_ "c"; At.id "b"] []) ;
  equal ops [] (counts p) ;
  patch p (El.div []) ;
  equal ops [("remove_attribute", 2)] (counts p) ;
  equal string "<div><div></div></div>" (html p) ;
  (* The removals come first. *)
  let log = ref [] in
  let doc = F.create () in
  let t =
    Diff.mount
      (logged (F.ops doc) log)
      ~parent:(F.element "div")
      ~post:(Private.Dom.post_now ignore)
      (El.svg ~at:[At.v "viewBox" "0"] [])
  in
  log := [] ;
  Diff.patch t (El.svg ~at:[At.v "viewbox" "0"] []) ;
  equal (list string) ["remove viewBox"; "attr viewbox"] (List.rev !log)

(* The diff reads the live property, and writes only a different value. *)
let properties () =
  let p = mount (El.input ~at:[At.value "a"; At.checked false] ()) in
  let n = node p in
  equal string "a" (F.value n) ;
  equal bool false (F.checked n) ;
  (* A new input has the value "" and is not checked. *)
  equal ops
    [ ("attach", 1)
    ; ("create_element", 1)
    ; ("get_checked", 1)
    ; ("get_value", 1)
    ; ("insert_before", 1)
    ; ("set_property", 1) ]
    (counts p) ;
  let reads = [("get_checked", 1); ("get_value", 1)] in
  patch p (El.input ~at:[At.value "a"; At.checked false] ()) ;
  equal ops reads (counts p) ;
  patch p (El.input ~at:[At.value "b"; At.checked true] ()) ;
  equal ops (reads @ [("set_property", 2)]) (counts p) ;
  equal string "b" (F.value n) ;
  equal bool true (F.checked n) ;
  patch p (El.input ~at:[At.checked true; At.value "b"] ()) ;
  equal ops reads (counts p) ;
  (* With no value in the view, the diff reads and writes nothing. *)
  patch p (El.input ()) ;
  equal ops [] (counts p) ;
  equal string "b" (F.value n) ;
  let cell = Option.get (F.cell n) in
  is_true (cell.value = None && cell.checked = None) ;
  patch p (El.input ~at:[At.value "b"; At.checked true] ()) ;
  equal ops reads (counts p) ;
  is_true (cell.value = Some "b" && cell.checked = Some true) ;
  (* One property only. *)
  patch p (El.input ~at:[At.value "c"] ()) ;
  equal ops [("get_value", 1); ("set_property", 1)] (counts p) ;
  patch p (El.input ~at:[At.checked false] ()) ;
  equal ops [("get_checked", 1); ("set_property", 1)] (counts p) ;
  is_true (cell.value = None && cell.checked = Some false)

(* While the cell is composing (an IME composition) and the element has the
   focus, the patch and the restore do not write [value]: the cell keeps the new
   view value. A [checked] change is still written. With no focus, the first
   write site clears the flag and writes. *)
let composing () =
  let p = mount (El.input ~at:[At.value "a"; At.checked false] ()) in
  let n = node p in
  let cell = Option.get (F.cell n) in
  is_false cell.composing ;
  cell.composing <- true ;
  F.focus p.doc (Some n) ;
  patch p (El.input ~at:[At.value "b"; At.checked true] ()) ;
  equal ops [("focused", 1); ("get_checked", 1); ("set_property", 1)] (counts p) ;
  equal string "a" (F.value n) ;
  is_true (F.checked n) ;
  is_true (cell.value = Some "b") ;
  (* The restore: no value write while composing. *)
  F.reset p.doc ;
  Diff.restore (F.ops p.doc) n cell ;
  equal ops [("focused", 1); ("get_checked", 1)] (counts p) ;
  equal string "a" (F.value n) ;
  (* With no composition, one write. *)
  cell.composing <- false ;
  Diff.restore (F.ops p.doc) n cell ;
  equal ops
    [("focused", 1); ("get_checked", 2); ("get_value", 1); ("set_property", 1)]
    (counts p) ;
  equal string "b" (F.value n) ;
  (* With the flag and no focus: the restore clears the flag and writes. *)
  cell.composing <- true ;
  F.focus p.doc None ;
  F.browser_set n (Value "x") ;
  F.reset p.doc ;
  Diff.restore (F.ops p.doc) n cell ;
  is_false cell.composing ;
  equal string "b" (F.value n) ;
  equal ops
    [("focused", 1); ("get_checked", 1); ("get_value", 1); ("set_property", 1)]
    (counts p) ;
  (* The flag is clear: the next patch reads no focus. *)
  patch p (El.input ~at:[At.value "c"; At.checked true] ()) ;
  equal string "c" (F.value n) ;
  equal ops
    [("get_checked", 1); ("get_value", 1); ("set_property", 1)]
    (counts p)

(* A patch from [a] to [b] gives the DOM of a fresh mount of [b], with the
   counts [expected]. *)
let as_fresh a b expected =
  let p = mount a in
  patch p b ;
  equal string ~msg:"the DOM of a fresh mount" (fresh b) (dump p) ;
  equal ops expected (counts p) ;
  check_tree p

let opt ?value t =
  El.option
    ~at:(match value with Some v -> [At.v "value" v] | None -> [])
    [txt t]

let select v kids = El.select ~at:[At.value v] kids

let keyed_select v l =
  View.keyed
    (El.select ~at:[At.value v])
    (List.map (fun (k, t) -> (Key.int k, opt t)) l)

(* The selected option follows its node. Thus after a change of the options, the
   live value can be different, and the diff writes the view value again. Each
   case has one kind of operation in the select. *)
let select_value () =
  (* set_text: the text is the value of an option with no value attribute. *)
  as_fresh
    (select "b" [opt "b"; opt "a"])
    (select "b" [opt "a"; opt "b"])
    [("get_value", 1); ("set_property", 1); ("set_text", 2)] ;
  (* set_attribute *)
  as_fresh
    (select "b" [opt ~value:"a" "x"; opt ~value:"b" "y"])
    (select "b" [opt ~value:"b" "x"; opt ~value:"a" "y"])
    [("get_value", 1); ("set_attribute", 2); ("set_property", 1)] ;
  (* remove_attribute *)
  as_fresh
    (select "b" [opt ~value:"b" "a"; opt "b"])
    (select "b" [opt "a"; opt "b"])
    [("get_value", 1); ("remove_attribute", 1); ("set_property", 1)] ;
  (* set_property: the value property of an option. *)
  let vopt v = El.option ~at:[At.value v] [txt "x"] in
  as_fresh
    (select "b" [vopt "b"; vopt "c"])
    (select "b" [vopt "c"; vopt "b"])
    [("get_value", 3); ("set_property", 3)] ;
  (* insert_before: the first option of the select becomes selected. *)
  as_fresh
    (select "b" [opt "a"])
    (select "b" [opt "a"; opt "b"])
    [ ("attach", 1)
    ; ("create_element", 1)
    ; ("create_text", 1)
    ; ("get_value", 1)
    ; ("insert_before", 2)
    ; ("set_property", 1) ] ;
  (* remove: a removal of the selected option selects the first option. *)
  as_fresh
    (select "b" [opt "a"; opt "b"])
    (select "b" [opt "a"])
    [("get_value", 1); ("remove", 1); ("set_property", 1)] ;
  (* replace *)
  as_fresh
    (select "b" [opt "a"; opt "b"])
    (select "b" [opt "a"; El.optgroup []])
    [ ("attach", 1)
    ; ("create_element", 1)
    ; ("get_value", 1)
    ; ("replace", 1)
    ; ("set_property", 1) ] ;
  (* clear, in an optgroup *)
  as_fresh
    (select "b" [El.optgroup [opt "b"]; opt "a"])
    (select "b" [El.optgroup []; opt "a"])
    [("clear", 1); ("get_value", 1); ("set_property", 1)] ;
  (* set_inner_html: the fake DOM has no options in HTML text. *)
  let html h =
    View.unsafe_html
      ~el:(El.select ~at:[At.value "b"])
      (Unsafe_html.of_string h)
  in
  as_fresh
    (html "<option>a</option>")
    (html "<option>b</option>")
    [("get_value", 1); ("set_inner_html", 1); ("set_property", 1)] ;
  (* A key change, and plain to keyed. *)
  as_fresh
    (keyed_select "b" [(1, "a"); (2, "b")])
    (keyed_select "b" [(1, "a"); (3, "b")])
    [ ("attach", 1)
    ; ("create_element", 1)
    ; ("create_text", 1)
    ; ("get_value", 1)
    ; ("insert_before", 2)
    ; ("remove", 1)
    ; ("set_property", 1) ] ;
  as_fresh
    (select "b" [opt "a"; opt "b"])
    (keyed_select "b" [(1, "a"); (2, "b")])
    [ ("attach", 2)
    ; ("clear", 1)
    ; ("create_element", 2)
    ; ("create_text", 2)
    ; ("get_value", 1)
    ; ("insert_before", 4)
    ; ("set_property", 1) ] ;
  (* No write with no change, with no value in the view, or for an element that
     is not a select. *)
  as_fresh
    (select "b" [opt "a"; opt "b"])
    (select "b" [opt "a"; opt "b"])
    [("get_value", 1)] ;
  as_fresh (El.select [opt "a"]) (El.select [opt "b"]) [("set_text", 1)] ;
  as_fresh
    (El.textarea ~at:[At.value "v"] [txt "a"])
    (El.textarea ~at:[At.value "v"] [txt "b"])
    [("get_value", 1); ("set_text", 1)]

let range ats v = El.input ~at:(At.type_ "range" :: At.value v :: ats) ()

(* The browser clamps the value of a range input. Thus after a change of type,
   min or max, the live value can be different, and the diff writes the view
   value again. *)
let input_value () =
  as_fresh
    (range [At.v "max" "40"] "50")
    (range [At.v "max" "100"] "50")
    [("get_value", 1); ("set_attribute", 1); ("set_property", 1)] ;
  as_fresh
    (range [At.v "min" "60"] "50")
    (range [] "50")
    [("get_value", 1); ("remove_attribute", 1); ("set_property", 1)] ;
  as_fresh (range [] "x")
    (El.input ~at:[At.type_ "text"; At.value "x"] ())
    [("get_value", 1); ("set_attribute", 1); ("set_property", 1)] ;
  (* The live value does not change: no write. The fake DOM ignores step. *)
  as_fresh
    (range [At.v "step" "1"] "50")
    (range [At.v "step" "2"] "50")
    [("get_value", 1); ("set_attribute", 1)] ;
  as_fresh
    (range [At.class_ "a"] "50")
    (range [At.class_ "b"; At.title "t"] "50")
    [("get_value", 1); ("set_attribute", 2)] ;
  as_fresh
    (range [At.class_ "a"; At.title "t"] "50")
    (range [] "50")
    [("get_value", 1); ("remove_attribute", 2)]

(* A patch from [a] to [b] gives the DOM of a fresh mount of [b]. *)
let like_fresh a b =
  let p = mount a in
  patch p b ;
  equal string ~msg:"the DOM of a fresh mount" (fresh b) (dump p) ;
  check_tree p ;
  p

let radio ?(ty = "radio") name c =
  El.input ~at:[At.type_ ty; At.name name; At.checked c] ()

(* The browser unchecks the other radio buttons of a group with no write of the
   diff. Thus the diff compares with the live property, after all the attributes
   and children of the patch. *)
let radio_groups () =
  let question (n, a, b) = El.div [radio n a; radio n b] in
  let questions l = El.div (List.map question l) in
  (* A sort of the questions: the first row gets the name of the second row. *)
  let p =
    like_fresh
      (questions [("q1", true, false); ("q2", true, false)])
      (questions [("q2", true, false); ("q1", true, false)])
  in
  equal (list bool) [true; false; true; false]
    (List.concat_map (fun d -> List.map F.checked (F.children d)) (kids p)) ;
  (* Two groups merge: the checked button of the second group joins the first
     group, and then it becomes unchecked. *)
  let p =
    like_fresh
      (El.form [radio "g1" true; radio "g2" true])
      (El.form [radio "g1" true; radio "g1" false])
  in
  equal (list bool) [true; false] (List.map F.checked (kids p)) ;
  (* A checkbox that becomes a radio button. *)
  ignore
    (like_fresh
       (El.div [radio "g" true; radio ~ty:"checkbox" "g" true])
       (El.div [radio "g" true; radio "g" false]) ) ;
  (* A keyed move. *)
  let row k c = (Key.int k, radio (if k < 2 then "g" else "h") c) in
  ignore
    (like_fresh
       (View.keyed El.div [row 0 true; row 1 false; row 2 true])
       (View.keyed El.div [row 2 false; row 1 true; row 0 false]) ) ;
  (* Two checked buttons in one group: the last button in the document is
     checked. The diff writes the properties in the order of the document, also
     in the common end and in the middle of a keyed list. *)
  let two l =
    View.keyed El.div
      (List.map
         (fun k -> (Key.int k, if k = 0 then txt "x" else radio "q" true))
         l )
  in
  let last l a b =
    let p = like_fresh (two a) (two b) in
    equal (list bool) l
      (List.filter_map
         (fun n ->
           if String.equal (F.name n) "input" then Some (F.checked n) else None )
         (kids p) )
  in
  last [false; true] [1; 2] [0; 1; 2] ;
  last [false; true] [1; 2] [2; 1] ;
  last [false; false; true] [1; 2] [3; 1; 2] ;
  last [false; false; true] [1; 2; 3] [2; 0; 3; 1] ;
  last [false; false; true] [2; 3] [1; 0; 2; 3]

(* The browser selects the first option of a drop-down select with no selected
   option after the removal of size or multiple. *)
let select_size () =
  let s ats = El.select ~at:(At.value "" :: ats) [opt "a"; opt "b"] in
  ignore (like_fresh (s [At.v "size" "4"]) (s [])) ;
  ignore (like_fresh (s [At.multiple]) (s [])) ;
  ignore (like_fresh (s [At.multiple]) (s [At.v "size" "1"]))

(* The user changes a property: the next patch writes the view value again, also
   with no change of the view. *)
let browser_change () =
  let v =
    El.input ~at:[At.type_ "checkbox"; At.value "a"; At.checked false] ()
  in
  let p = mount v in
  let n = node p in
  F.browser_set n (Value "typed") ;
  F.browser_set n (Checked true) ;
  patch p v ;
  equal string "a" (F.value n) ;
  equal bool false (F.checked n) ;
  let s = El.select ~at:[At.value "b"] [opt "a"; opt "b"] in
  let p = mount s in
  F.browser_set (node p) (Value "a") ;
  patch p s ;
  equal string "b" (F.value (node p))

(* A fault stops a patch before the writes of the properties. The next patch
   compares each controlled element one time. *)
let property_fault () =
  let v t = El.div [El.input ~at:[At.value "a"] (); txt t] in
  let p = mount (v "x") in
  F.reset p.doc ;
  F.fail_at p.doc 1 ;
  raises_match
    (function F.Fault -> true | _ -> false)
    (fun () -> Diff.patch p.tree (v "y")) ;
  patch p (v "y") ;
  equal ops [("get_value", 1); ("set_text", 1)] (counts p) ;
  equal string "<div><div><input .value=\"a\">\"y\"</div></div>" (dump p)

let handlers () =
  let p = mount (El.div ~ev:[Ev.click "a"; Ev.input (fun s -> "in:" ^ s)] []) in
  let n = node p in
  F.fire n "click" Private.Dom.Unit () ;
  F.fire n "input" Private.Dom.String "x" ;
  equal (list string) ["in:x"; "a"] !(p.msgs) ;
  patch p (El.div ~ev:[Ev.click "b"; Ev.keydown (fun k -> "key:" ^ k)] []) ;
  equal ops [] (counts p) ;
  F.fire n "click" Private.Dom.Unit () ;
  F.fire n "keydown" Private.Dom.Key "Enter" ;
  equal (list string) ["key:Enter"; "b"; "in:x"; "a"] !(p.msgs) ;
  raises_match
    (function F.Misuse _ -> true | _ -> false)
    (fun () -> F.fire n "input" Private.Dom.String "x") ;
  patch p (El.div []) ;
  equal int 0 (List.length (Private.Dom.handlers (Option.get (F.cell n))))

let listeners () =
  let p = mount (El.div ~ev:[Ev.scroll "s"; Ev.click "c"] []) in
  let n = node p in
  equal (list (pair string bool)) [("scroll", true)] (F.listeners n) ;
  patch p (El.div ~ev:[Ev.scroll ~prevent_default:true "s"] []) ;
  equal ops [("listen", 1); ("unlisten", 1)] (counts p) ;
  equal (list (pair string bool)) [("scroll", false)] (F.listeners n) ;
  patch p (El.div ~ev:[Ev.scroll ~prevent_default:true "t"] []) ;
  equal ops [] (counts p) ;
  patch p (El.div ~ev:[Ev.mouseenter "m"; Ev.focus "f"] []) ;
  equal ops [("listen", 1); ("unlisten", 1)] (counts p) ;
  equal (list (pair string bool)) [("mouseenter", true)] (F.listeners n) ;
  patch p (El.div ~ev:[Ev.mouseleave "m"; Ev.mouseenter "m"] []) ;
  equal ops [("listen", 1)] (counts p) ;
  patch p (El.div []) ;
  equal ops [("unlisten", 2)] (counts p) ;
  equal (list (pair string bool)) [] (F.listeners n) ;
  (* wheel, touchstart and touchmove: passive with no prevent_default. A patch
     that adds prevent_default removes the listener and adds it again. *)
  List.iter
    (fun (name, ev) ->
      patch p (El.div ~ev:[ev ~prevent_default:false name] []) ;
      equal ops ~msg:name [("listen", 1)] (counts p) ;
      equal (list (pair string bool)) [(name, true)] (F.listeners n) ;
      patch p (El.div ~ev:[ev ~prevent_default:true name] []) ;
      equal ops ~msg:name [("listen", 1); ("unlisten", 1)] (counts p) ;
      equal (list (pair string bool)) [(name, false)] (F.listeners n) ;
      patch p (El.div []) ;
      equal ops [("unlisten", 1)] (counts p) )
    [ ("wheel", fun ~prevent_default m -> Ev.wheel ~prevent_default m)
    ; ("touchstart", fun ~prevent_default m -> Ev.touchstart ~prevent_default m)
    ; ("touchmove", fun ~prevent_default m -> Ev.touchmove ~prevent_default m)
    ]

let namespaces () =
  let v =
    El.div
      [ El.svg
          [ El.circle []
          ; El.foreign_object [El.div [El.span []; El.svg [El.g []]]]
          ; El.a [] ]
      ; El.a [] ]
  in
  let p = mount v in
  equal string
    "<div><div><svg:svg><svg:circle></svg:circle><svg:foreignObject><div><span></span><svg:svg><svg:g></svg:g></svg:svg></div></svg:foreignObject><svg:a></svg:a></svg:svg><a></a></div></div>"
    (dump p) ;
  let svg = List.hd (kids p) in
  patch p v ;
  equal ops [] (counts p) ;
  is_true (svg == List.hd (kids p)) ;
  patch p (El.div [El.div []; El.a []]) ;
  equal ops [("attach", 1); ("create_element", 1); ("replace", 1)] (counts p) ;
  equal string "<div><div><div></div><a></a></div></div>" (dump p)

type Private.Unsafe_html.native += Native

let inner () =
  let log = ref [] in
  let doc = F.create () in
  let root = F.element "div" in
  let h s = Private.Unsafe_html.of_native s Native in
  let t =
    Diff.mount
      (logged (F.ops doc) log)
      ~parent:root
      ~post:(Private.Dom.post_now ignore)
      (View.unsafe_html ~el:El.div (h "<b>x</b>"))
  in
  equal string "<div><div><b>x</b></div></div>" (F.to_html root) ;
  equal (option int) None (Option.map List.length (Diff.children (Diff.root t))) ;
  log := [] ;
  Diff.patch t (View.unsafe_html ~el:El.div (h "<b>x</b>")) ;
  equal (list string) [] !log ;
  Diff.patch t (View.unsafe_html ~el:El.div (h "<i>y</i>")) ;
  equal (list string) ["html <i>y</i>"] !log ;
  equal string "<div><div><i>y</i></div></div>" (F.to_html root) ;
  equal string "" (F.text root)

let kinds () =
  let p = mount (El.div [txt "a"; txt "b"]) in
  patch p (View.keyed El.div (rows [1; 2])) ;
  equal ops
    [ ("attach", 2)
    ; ("clear", 1)
    ; ("create_element", 2)
    ; ("create_text", 2)
    ; ("insert_before", 4) ]
    (counts p) ;
  equal (list string) ["1"; "2"] (texts p) ;
  check_tree p ;
  patch p (View.unsafe_html ~el:El.div (Unsafe_html.of_string "<b>x</b>")) ;
  equal ops [("set_inner_html", 1)] (counts p) ;
  patch p (El.div [txt "c"]) ;
  equal ops [("clear", 1); ("create_text", 1); ("insert_before", 1)] (counts p) ;
  equal string "<div><div>c</div></div>" (html p) ;
  check_tree p ;
  patch p (View.unsafe_html ~el:El.div (Unsafe_html.of_string "<i></i>")) ;
  equal ops [("set_inner_html", 1)] (counts p) ;
  patch p (View.keyed El.div (rows [3])) ;
  equal ops
    [ ("attach", 1)
    ; ("clear", 1)
    ; ("create_element", 1)
    ; ("create_text", 1)
    ; ("insert_before", 2) ]
    (counts p) ;
  check_tree p ;
  patch p (El.div []) ;
  equal ops [("clear", 1)] (counts p) ;
  patch p (View.keyed El.div []) ;
  equal ops [] (counts p) ;
  patch p (El.div []) ;
  equal ops [] (counts p) ;
  patch p (View.unsafe_html ~el:El.div (Unsafe_html.of_string "")) ;
  equal ops [("set_inner_html", 1)] (counts p) ;
  patch p (View.keyed El.div []) ;
  equal ops [("clear", 1)] (counts p) ;
  patch p (El.div [txt "d"]) ;
  equal ops [("create_text", 1); ("insert_before", 1)] (counts p) ;
  check_tree p

let plain () =
  let p = mount (El.div [txt "a"]) in
  patch p (El.div [txt "a"; txt "b"; txt "c"]) ;
  equal ops [("create_text", 2); ("insert_before", 2)] (counts p) ;
  check_tree p ;
  patch p (El.div [txt "a"; txt "b"; txt "c"]) ;
  equal ops [] (counts p) ;
  check_tree p ;
  patch p (El.div [txt "x"]) ;
  equal ops [("remove", 2); ("set_text", 1)] (counts p) ;
  equal string "<div><div>x</div></div>" (html p) ;
  check_tree p ;
  patch p (El.div [txt "x"; txt "y"]) ;
  patch p (El.div []) ;
  equal ops [("clear", 1)] (counts p) ;
  check_tree p ;
  patch p (El.div []) ;
  equal ops [] (counts p)

(* A keyed patch from [a] to [b]: the texts, the moves, and the kept nodes. *)
let keyed_case a b ~moves ~total =
  let p = mount (ul a) in
  let before = List.combine a (kids p) in
  patch p (ul b) ;
  equal (list string) (List.map string_of_int b) (texts p) ;
  equal int ~msg:"moves" moves (F.moves p.doc) ;
  equal int ~msg:"total" total (F.total p.doc) ;
  List.iter2
    (fun k n ->
      match List.assoc_opt k before with
      | Some n' ->
          is_true ~msg:"a kept key keeps its node" (n == n')
      | None ->
          () )
    b (kids p) ;
  check_tree p

let keyed () =
  keyed_case [1; 2; 3] [1; 2; 3] ~moves:0 ~total:0 ;
  keyed_case [1; 2; 3] [1; 2; 3; 4] ~moves:0 ~total:5 ;
  keyed_case [1; 2; 3] [0; 1; 2; 3] ~moves:0 ~total:5 ;
  keyed_case [1; 3] [1; 2; 3] ~moves:0 ~total:5 ;
  keyed_case [1; 2; 3] [1; 3] ~moves:0 ~total:1 ;
  keyed_case [1; 2; 3] [2; 3] ~moves:0 ~total:1 ;
  keyed_case [1; 2; 3] [1; 2] ~moves:0 ~total:1 ;
  keyed_case [1; 2; 3; 4] [1; 3; 4; 2] ~moves:1 ~total:1 ;
  keyed_case [1; 2; 3; 4] [4; 1; 2; 3] ~moves:1 ~total:1 ;
  keyed_case [1; 2; 3; 4; 5] [5; 4; 3; 2; 1] ~moves:4 ~total:4 ;
  keyed_case [1; 2; 3; 4; 5] [1; 4; 3; 2; 5] ~moves:2 ~total:2 ;
  keyed_case [0; 1; 2] [2; 1; 0] ~moves:2 ~total:2 ;
  keyed_case [0; 1; 2; 3] [3; 0; 9; 1] ~moves:1 ~total:7 ;
  keyed_case [1; 2] [3; 4] ~moves:0 ~total:12 ;
  keyed_case [] [1; 2] ~moves:0 ~total:10 ;
  keyed_case [1; 2] [] ~moves:0 ~total:1 ;
  keyed_case [] [] ~moves:0 ~total:0 ;
  (* More small int keys. *)
  keyed_case [1; 2] [0; 5] ~moves:0 ~total:12 ;
  keyed_case [1; 2] [4; 9] ~moves:0 ~total:12 ;
  keyed_case [0; 5; 9] [5; 0; 9] ~moves:1 ~total:1 ;
  keyed_case [5; 0; 9] [0; 5; 9] ~moves:1 ~total:1 ;
  keyed_case [4; 9; 1] [9; 4; 1] ~moves:1 ~total:1 ;
  (* A row that changes its element at a kept key. *)
  let p = mount (ul [1; 2]) in
  patch p
    (View.keyed El.ul
       [(Key.int 2, El.p [txt "2"]); (Key.int 1, El.li [txt "x"])] ) ;
  equal string "<div><ul><p>2</p><li>x</li></ul></div>" (html p) ;
  check_tree p ;
  (* String keys. *)
  let s l =
    View.keyed El.ul (List.map (fun k -> (Key.string k, El.li [txt k])) l)
  in
  let p = mount (s ["a"; "b"; "c"]) in
  patch p (s ["c"; "a"; "b"]) ;
  equal int 1 (F.moves p.doc) ;
  equal (list string) ["c"; "a"; "b"] (texts p)

(* The table of the keyed diff: a mixed hash, a power of 2 of slots, and a map
   if the plan reads more than 8 slots for each key. *)
let table () =
  let probes nk ok = Diff.probes (Array.of_list nk) (Array.of_list ok) in
  let ints f l = List.map (fun i -> Key.int (f i)) l in
  let n = 10_000 in
  let l = List.init n Fun.id in
  let r = List.rev l in
  let below name nk ok =
    match probes nk ok with
    | Some c ->
        is_true ~msg:(name ^ ": " ^ string_of_int c) (c < 4 * n)
    | None ->
        fail name
  in
  let id = Fun.id in
  below "consecutive" (ints id l) (ints id r) ;
  below "shifted by 2k + 1" (ints (fun i -> i + 20_001) l) (ints id l) ;
  below "shifted by 3 (2k + 1)" (ints (fun i -> i + 60_003) l) (ints id l) ;
  below "shifted by an odd offset" (ints (fun i -> i + 20_000) l) (ints id l) ;
  below "a stride of 2k + 1"
    (ints (fun i -> i * 20_001) l)
    (ints (fun i -> i * 20_001) r) ;
  below "a stride of 2^15" (ints (fun i -> i lsl 15) l) (ints id l) ;
  below "a stride of 2^30" (ints (fun i -> i lsl 30) l) (ints id l) ;
  below "negative" (ints (fun i -> -i) l) (ints id l) ;
  let rows f = List.map (fun i -> Key.string ("row-" ^ string_of_int i)) f in
  below "strings" (rows l) (rows r) ;
  (* The large tables (16,384 and 65,536 consecutive keys, 20 offsets) are in
     test/cross, which is not in the mutation suite. *)
  (* Exact counts with keys that start at one slot. [j] such keys read j (j + 1)
     / 2 slots, and the budget of 16 keys is 128 slots. *)
  let c = colliding 33 7 in
  let keys a b = List.init (b - a) (fun i -> Key.int c.(a + i)) in
  equal (option int) (Some 0) (probes [] []) ;
  equal (option int) (Some 0) (probes (keys 0 1) (keys 0 1)) ;
  equal (option int) (Some 1) (probes (keys 0 1) []) ;
  equal (option int) (Some 3) (probes (keys 0 2) []) ;
  equal (option int) (Some 6) (probes (keys 0 3) []) ;
  equal (option int) (Some 120) (probes (keys 0 15) []) ;
  equal (option int) (Some 107) (probes (keys 0 14) (keys 1 2)) ;
  equal (option int) (Some 120) (probes (keys 0 14) (keys 14 15)) ;
  equal (option int) None (probes (keys 0 16) []) ;
  (* Keys that start at an empty slot of a table of 32 slots: the search reads
     one slot. *)
  let free =
    List.filter
      (fun i -> Private.Key.hash (Key.int i) land 31 >= 16)
      (List.init 20 (fun i -> 1000 + i))
  in
  let free j = Key.int (List.nth free j) in
  equal (option int) None (probes (keys 0 16) [free 0]) ;
  equal (option int) (Some 138) (probes (keys 0 16) [free 0; free 1]) ;
  (* The budget of a plan counts the keys of the new list and of the old middle:
     here 16 and 1, thus 136 slots. *)
  let x = Key.int c.(16) and y = free 0 in
  let c14 = Key.int c.(14) and c15 = Key.int c.(15) in
  equal (option int) (Some 136) (probes ((x :: keys 0 14) @ [y]) [x; c14; y]) ;
  equal (option int) None (probes ((x :: keys 0 14) @ [y]) [x; c14; c15; y]) ;
  (* The size of the table: 2 slots for 1 key, 4 for 2, 8 for 3. *)
  let differ size =
    let h k = Private.Key.hash (Key.int k) in
    let rec go i =
      if h i land (size - 1) = 0 && h i land (size + size - 1) <> 0 then i
      else go (i + 1)
    in
    Key.int (go 1)
  in
  let k0 = Key.int 0 in
  equal (option int) (Some 3) (probes [k0] [differ 2]) ;
  equal (option int) (Some 3) (probes [k0; differ 4] []) ;
  equal (option int) (Some 6) (probes [k0; Key.int c.(1); differ 8] []) ;
  (* A search goes past the last slot to slot 0. *)
  let last =
    List.filter
      (fun i -> Private.Key.hash (Key.int i) land 3 = 3)
      (List.init 100 Fun.id)
  in
  let a = Key.int (List.nth last 0) and b = Key.int (List.nth last 1) in
  equal (option int) (Some 7) (probes [a; b] [b; Key.int c.(1)]) ;
  raises_match (Exn_match.invalid_arg "duplicate") (fun () ->
      ignore (probes [a; b; a] []) )

(* A keyed patch with colliding keys: the plan reads too many slots and goes to
   the map. *)
let colliding_keys () =
  let c = colliding 40 7 in
  let key i = Key.int c.(i) in
  let l = List.init 40 Fun.id in
  let p = mount (ul ~key l) in
  let before = kids p in
  patch p (ul ~key (List.rev l)) ;
  equal (list string) (List.map string_of_int (List.rev l)) (texts p) ;
  equal int 39 (F.moves p.doc) ;
  equal int 39 (F.total p.doc) ;
  is_true (same_nodes (List.rev before) (kids p)) ;
  check_tree p ;
  (* Duplicates: in the middle, and in the start and the middle. *)
  let dup b =
    let p = mount (ul ~key l) in
    F.reset p.doc ;
    raises_match (Exn_match.invalid_arg "duplicate") (fun () ->
        Diff.patch p.tree (ul ~key b) ) ;
    equal int 0 (F.total p.doc) ;
    check_tree p
  in
  dup (List.rev l @ [5]) ;
  dup ((0 :: List.rev (List.tl l)) @ [0]) ;
  dup (List.rev (List.tl l) @ [39]) ;
  (* A middle of 35 colliding new keys (the map), and old keys that are not in
     it. *)
  let p = mount (ul ~key (List.init 20 Fun.id)) in
  patch p (ul ~key (List.init 35 (fun i -> 39 - i))) ;
  equal (list string) (List.init 35 (fun i -> string_of_int (39 - i))) (texts p) ;
  check_tree p

let duplicates () =
  let dup a b =
    let p = mount (View.keyed (El.ul ~at:[At.id "a"]) (rows a)) in
    let before = dump p and kids_before = retained_kids p in
    F.reset p.doc ;
    raises_match (Exn_match.invalid_arg "duplicate") (fun () ->
        Diff.patch p.tree (View.keyed (El.ul ~at:[At.id "b"]) (rows b)) ) ;
    equal int 0 (F.total p.doc) ;
    equal string before (dump p) ;
    is_true (same_nodes kids_before (retained_kids p)) ;
    patch p (ul (List.sort_uniq compare b)) ;
    check_tree p
  in
  (* In the common start, in the middle, in the common end. *)
  dup [1; 2; 3] [1; 2; 3; 1] ;
  dup [1; 2; 3] [1; 4; 4; 3] ;
  dup [1; 2; 3] [3; 1; 2; 3] ;
  dup [1; 2] [1; 1; 2] ;
  dup [] [1; 1] ;
  dup [1] [2; 2] ;
  (* More small int keys. *)
  dup [1] [0; 7; 7] ;
  dup [1] [6; 13; 13] ;
  dup [0; 1] [0; 5; 0] ;
  (* At a mount, before any operation, and from plain children. *)
  let doc = F.create () in
  raises_match (Exn_match.invalid_arg "duplicate") (fun () ->
      mount_with doc (ul [1; 2; 1]) ) ;
  equal int 0 (F.total doc) ;
  let p = mount (El.ul ~at:[At.id "a"] [txt "x"]) in
  F.reset p.doc ;
  raises_match (Exn_match.invalid_arg "duplicate") (fun () ->
      Diff.patch p.tree (View.keyed (El.ul ~at:[At.id "b"]) (rows [5; 5])) ) ;
  equal int 0 (F.total p.doc) ;
  (* A string key and an int key are different. *)
  ignore
    (mount (View.keyed El.ul [(Key.int 1, txt "a"); (Key.string "1", txt "b")]))

(* A patch of a read patches the view of the read in place: the same DOM node,
   one set_attribute and one set_text. A replace of the node would create a new
   element. *)
let read_in_place () =
  let v s =
    El.div
      [Private.Context.read ca (fun x -> El.span ~at:[At.title s] [txt (s ^ x)])]
  in
  let p = mount (v "a") in
  let span = List.hd (kids p) in
  patch p (v "b") ;
  is_true ~msg:"the same node" (List.hd (kids p) == span) ;
  equal ops [("set_attribute", 1); ("set_text", 1)] (counts p) ;
  equal string (fresh (v "b")) (dump p)

let tests =
  [ group "diff"
      [ test "a patch of a read keeps its node" read_in_place
      ; test "a mount builds the DOM" mount_html
      ; test "a mount writes attributes (type first), children, properties"
          mount_order
      ; test "a new name or kind replaces the node" replace
      ; test "a text change is one set_text" text
      ; test "attributes: add, change and remove" attributes
      ; test "an IME composition: no value write" composing
      ; test "properties: a write only if the live value is different"
          properties
      ; test "a select writes its value again after a change of its options"
          select_value
      ; test "an input writes its value again after a change of its range"
          input_value
      ; test "radio groups: the live checked property" radio_groups
      ; test "a select with no size: the live value" select_size
      ; test "a change of the browser: the live properties" browser_change
      ; test "a fault before the properties: the next patch" property_fault
      ; test "handlers change with no DOM operation" handlers
      ; test "listeners for the events that do not bubble" listeners
      ; test "namespaces come from the parent" namespaces
      ; test "unsafe HTML: a write only on a change of the string" inner
      ; test "a change of the kind of children" kinds
      ; test "plain children: append, remove, clear" plain
      ; test "keyed children: start, end, insert, remove, moves" keyed
      ; test "a duplicate key raises before any DOM operation" duplicates
      ; test "the table of keys: probes, size and budget" table
      ; test "colliding keys: the map" colliding_keys ] ]
