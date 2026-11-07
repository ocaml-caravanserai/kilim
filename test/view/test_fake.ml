(* The strict fake DOM of kilim.testing. *)

open Windtrap
open Kilim
module F = Kilim_testing.Fake_dom
module Dom = Private.Dom

let misuse f = raises_match (function F.Misuse _ -> true | _ -> false) f

let names l = List.map F.text l

let parent_is n p = match F.parent n with Some q -> q == p | None -> false

let setup () =
  let t = F.create () in
  let o = F.ops t in
  let root = F.element "div" in
  (t, o, root)

let text o s = o.Dom.create_text s

let cell evs dispatch =
  {Dom.events= Events (evs, dispatch); value= None; checked= None}

let ev ?(prevent_default = false) ?(stop = false) event payload f =
  {Dom.event; bubbles= true; prevent_default; stop; decode= Decode (payload, f)}

let tree () =
  let t, o, root = setup () in
  equal int 0 (F.total t) ;
  equal string "div" (F.name root) ;
  is_true (F.ns root = Some Dom.Html) ;
  let a = text o "a" and b = text o "b" and c = text o "c" and d = text o "d" in
  o.insert_before root b None ;
  o.insert_before root d None ;
  o.insert_before root a (Some b) ;
  o.insert_before root c (Some d) ;
  equal (list string) ["a"; "b"; "c"; "d"] (names (F.children root)) ;
  is_true (parent_is a root) ;
  equal int 0 (F.moves t) ;
  (* Moves: to the front, to the end, to the middle. *)
  o.insert_before root d (Some a) ;
  o.insert_before root a None ;
  o.insert_before root b (Some c) ;
  o.insert_before root c (Some d) ;
  equal (list string) ["c"; "d"; "b"; "a"] (names (F.children root)) ;
  equal int 4 (F.moves t) ;
  o.remove root c ;
  o.remove root a ;
  equal (list string) ["d"; "b"] (names (F.children root)) ;
  is_true (F.parent a = None) ;
  o.insert_before root a None ;
  o.insert_before root c (Some d) ;
  equal (list string) ["c"; "d"; "b"; "a"] (names (F.children root)) ;
  o.remove root b ;
  equal (list string) ["c"; "d"; "a"] (names (F.children root)) ;
  o.insert_before root b (Some a) ;
  equal (list string) ["c"; "d"; "b"; "a"] (names (F.children root)) ;
  (* Replace the first, a middle and the last child. *)
  let x = text o "x" and y = text o "y" and z = text o "z" in
  o.replace root x c ;
  o.replace root y b ;
  o.replace root z a ;
  equal (list string) ["x"; "d"; "y"; "z"] (names (F.children root)) ;
  is_true (F.parent c = None) ;
  o.insert_before root a None ;
  equal (list string) ["x"; "d"; "y"; "z"; "a"] (names (F.children root)) ;
  equal string "xdyza" (F.text root) ;
  o.clear root ;
  equal (list string) [] (names (F.children root)) ;
  is_true (F.parent x = None) ;
  o.insert_before root x None ;
  equal (list string) ["x"] (names (F.children root)) ;
  equal
    (list (pair string int))
    [ ("clear", 1)
    ; ("create_text", 7)
    ; ("insert_before", 13)
    ; ("remove", 3)
    ; ("replace", 3) ]
    (F.counts t) ;
  equal int 27 (F.total t) ;
  F.reset t ;
  equal int 0 (F.total t) ;
  equal int 0 (F.moves t) ;
  equal (list (pair string int)) [] (F.counts t)

let misuses () =
  let t, o, root = setup () in
  let a = text o "a" and b = text o "b" in
  let e = o.create_element Html "p" and e2 = o.create_element Svg "g" in
  is_true (F.ns e2 = Some Dom.Svg) ;
  is_true (F.ns a = None) ;
  o.insert_before root a None ;
  o.insert_before root e None ;
  o.insert_before e2 b None ;
  let n = F.total t in
  misuse (fun () -> o.insert_before a (text o "c") None) ;
  misuse (fun () -> o.insert_before root root None) ;
  misuse (fun () -> o.insert_before e root None) ;
  misuse (fun () -> o.insert_before root b None) ;
  misuse (fun () -> o.insert_before root a (Some a)) ;
  misuse (fun () -> o.insert_before root (text o "c") (Some b)) ;
  misuse (fun () -> o.remove root b) ;
  misuse (fun () -> o.replace root (text o "c") b) ;
  misuse (fun () -> o.replace root b a) ;
  misuse (fun () -> o.clear a) ;
  misuse (fun () -> o.set_text e "x") ;
  misuse (fun () -> o.set_attribute a "id" "x") ;
  misuse (fun () -> o.remove_attribute a "id") ;
  misuse (fun () -> o.remove_attribute e "id") ;
  misuse (fun () -> o.set_property a (Value "x")) ;
  misuse (fun () -> o.set_inner_html a (Unsafe_html.of_string "x")) ;
  misuse (fun () -> o.attach a (cell [] ignore)) ;
  misuse (fun () -> o.get_value a) ;
  misuse (fun () -> o.get_checked a) ;
  misuse (fun () -> F.browser_set a (Checked true)) ;
  misuse (fun () -> o.listen a "scroll" ~passive:true) ;
  misuse (fun () -> o.unlisten e "scroll") ;
  (* Three creations of text nodes are the only operations. *)
  equal int (n + 3) (F.total t) ;
  let c = cell [] ignore in
  o.attach e c ;
  is_true (Option.get (F.cell e) == c) ;
  misuse (fun () -> o.attach e c) ;
  o.listen e "scroll" ~passive:true ;
  misuse (fun () -> o.listen e "scroll" ~passive:false) ;
  o.listen e "mouseenter" ~passive:false ;
  equal
    (list (pair string bool))
    [("mouseenter", false); ("scroll", true)]
    (F.listeners e) ;
  o.unlisten e "scroll" ;
  equal (list (pair string bool)) [("mouseenter", false)] (F.listeners e) ;
  o.set_text a "z" ;
  equal string "z" (F.text a) ;
  equal string "#text" (F.name a)

let state () =
  let t, o, root = setup () in
  let e = o.create_element Html "input" in
  o.insert_before root e None ;
  o.set_attribute e "id" "a" ;
  o.set_attribute e "class" "c\"<&>" ;
  o.set_attribute e "id" "b" ;
  equal (option string) (Some "b") (F.attribute e "id") ;
  o.remove_attribute e "class" ;
  equal (option string) None (F.attribute e "class") ;
  o.set_attribute e "class" "c\"<&>" ;
  is_true (F.value e = "" && not (F.checked e)) ;
  o.set_property e (Value "v\"") ;
  o.set_property e (Checked true) ;
  is_true (F.value e = "v\"" && F.checked e) ;
  equal string "<div><input class=\"c&quot;&lt;&amp;&gt;\" id=\"b\"></div>"
    (F.to_html root) ;
  let s = o.create_element Svg "circle" in
  o.insert_before root s None ;
  o.insert_before s (text o "x<&>\"") None ;
  let h = o.create_element Html "p" in
  o.insert_before root h None ;
  o.insert_before h (text o "q") None ;
  o.set_inner_html h (Unsafe_html.of_string "<b>r</b>") ;
  equal (list string) ["#html"] (List.map F.name (F.children h)) ;
  equal string "<b>r</b>" (F.text (List.hd (F.children h))) ;
  equal string
    "<div><input class=\"c&quot;&lt;&amp;&gt;\" \
     id=\"b\"><circle>x&lt;&amp;&gt;\"</circle><p><b>r</b></p></div>"
    (F.to_html root) ;
  equal string "x<&>\"" (F.text root) ;
  let c =
    cell
      [ ev ~prevent_default:true "click" Unit ignore
      ; ev ~stop:true "input" String ignore
      ; ev ~prevent_default:true ~stop:true "keydown" Key ignore
      ; ev "change" Bool ignore ]
      ignore
  in
  o.attach e c ;
  o.listen e "scroll" ~passive:true ;
  o.listen e "mouseenter" ~passive:false ;
  equal string
    "<div><input class=\"c&quot;&lt;&amp;&gt;\" id=\"b\" .value=\"v&quot;\" \
     .checked @mouseenter @scroll~ !change/checked !click+p !input/value+s \
     !keydown/key+p+s><svg:circle>\"x&lt;&amp;&gt;&quot;\"</svg:circle><p><b>r</b></p></div>"
    (F.dump root) ;
  ignore t

(* The model of a select: the options, their values, the selected option. *)
let select_model () =
  let _, o, root = setup () in
  let el name = o.Dom.create_element Html name in
  let append p n = o.insert_before p n None in
  let opt ?value t =
    let n = el "option" in
    Option.iter (o.set_attribute n "value") value ;
    append n (text o t) ;
    n
  in
  let s = el "select" in
  append root s ;
  let value () = F.value s in
  (* No option: no value. An insert selects the first option. *)
  equal string "" (value ()) ;
  let a = opt "a" in
  append s a ;
  equal string "a" (value ()) ;
  o.set_property s (Value "a") ;
  equal string "a" (value ()) ;
  (* The value attribute comes before the text, and the property before the
     attribute. *)
  let b = opt ~value:"b" "x" in
  append s b ;
  o.set_property s (Value "b") ;
  equal string "b" (value ()) ;
  o.set_property b (Value "c") ;
  equal string "c" (value ()) ;
  o.remove s b ;
  equal string "a" (value ()) ;
  o.set_property s (Value "x") ;
  equal string "" (value ()) ;
  (* With no selected option, an insert selects the first option. *)
  let c = opt "c" in
  o.insert_before s c (Some a) ;
  equal string "c" (value ()) ;
  (* The selected option moves with its node. *)
  o.set_property s (Value "a") ;
  o.insert_before s a (Some c) ;
  equal string "a" (value ()) ;
  o.set_text (List.hd (F.children a)) "z" ;
  equal string "z" (value ()) ;
  (* The first option with the value, and no other option. A removal of the
     selected option selects the first option. *)
  let d = opt "c" in
  append s d ;
  equal string "z" (value ()) ;
  o.set_property s (Value "c") ;
  o.remove s c ;
  equal string "z" (value ()) ;
  o.insert_before s c None ;
  o.set_property s (Value "c") ;
  o.remove s d ;
  equal string "z" (value ()) ;
  (* A replace of the selected option selects the first option. *)
  o.set_property s (Value "c") ;
  o.replace s (el "hr") c ;
  equal string "z" (value ()) ;
  (* Other children are not options. *)
  let s2 = el "select" in
  append s2 (text o "t") ;
  append s2 (el "div") ;
  append s2 (o.create_element Svg "option") ;
  o.set_property s2 (Value "t") ;
  equal string "" (F.value s2) ;
  (* The options of an optgroup. A removed option keeps its selectedness. *)
  let g = el "optgroup" in
  let e = opt "e" and f = opt "f" and k = opt "k" in
  append g e ;
  append g (el "div") ;
  append g f ;
  append s2 g ;
  equal string "e" (F.value s2) ;
  o.set_property s2 (Value "f") ;
  equal string "f" (F.value s2) ;
  o.remove g f ;
  equal string "e" (F.value s2) ;
  o.set_property s2 (Value "x") ;
  o.insert_before g f None ;
  equal string "f" (F.value s2) ;
  append s2 k ;
  o.clear g ;
  equal string "k" (F.value s2) ;
  append g e ;
  append g f ;
  o.set_property s2 (Value "f") ;
  o.set_inner_html g (Unsafe_html.of_string "<option>f</option>") ;
  equal string "k" (F.value s2) ;
  (* An optgroup that is not in a select, and a select with no options. *)
  let g2 = el "optgroup" and h = opt "h" in
  append g2 h ;
  equal string "h" (F.value h) ;
  append root g2 ;
  append g2 (opt "h2") ;
  o.remove root g2 ;
  let div = el "div" in
  append div (opt "i") ;
  let s3 = el "select" in
  o.set_property s3 (Value "x") ;
  append s3 g2 ;
  equal string "h" (F.value s3) ;
  o.set_property s3 (Value "h2") ;
  equal string "h2" (F.value s3) ;
  o.clear s3 ;
  equal string "" (F.value s3) ;
  equal string "<select .value=\"\"></select>" (F.dump s3)

(* The model of a range input: the value stays between min and max. *)
let range_model () =
  let _, o, root = setup () in
  let i = o.Dom.create_element Html "input" in
  o.insert_before root i None ;
  let value v = o.set_property i (Value v) ; F.value i in
  (* With no write: the value attribute, else "". *)
  equal string "" (F.value i) ;
  o.set_attribute i "value" "200" ;
  equal string "200" (F.value i) ;
  (* Not a range input: no change. *)
  equal string "200" (value "200") ;
  o.set_attribute i "type" "text" ;
  equal string "x" (value "x") ;
  o.set_attribute i "type" "range" ;
  equal string "50" (F.value i) ;
  equal string "100" (value "200") ;
  equal string "0" (value "-1") ;
  equal string "100.0" (value "100.0") ;
  equal string "0.0" (value "0.0") ;
  equal string "7" (value "7") ;
  equal string "50" (value "x") ;
  o.set_attribute i "max" "5" ;
  equal string "2.5" (value "x") ;
  equal string "5" (value "6") ;
  o.set_attribute i "min" "10" ;
  equal string "10" (F.value i) ;
  equal string "10" (value "6") ;
  o.set_attribute i "min" "x" ;
  equal string "2.5" (value "y") ;
  o.remove_attribute i "max" ;
  o.remove_attribute i "min" ;
  equal string "60" (value "60") ;
  o.set_attribute i "max" "40" ;
  equal string "40" (F.value i) ;
  o.set_attribute i "type" "text" ;
  equal string "abc" (value "abc") ;
  o.set_attribute i "class" "c" ;
  equal string "abc" (F.value i) ;
  o.remove_attribute i "type" ;
  equal string "abc" (F.value i) ;
  o.set_attribute i "type" "range" ;
  equal string "20" (F.value i) ;
  (* The case of the type is not important. *)
  o.set_attribute i "type" "text" ;
  equal string "60" (value "60") ;
  o.set_attribute i "type" "RANGE" ;
  equal string "40" (F.value i) ;
  let j = o.Dom.create_element Svg "input" in
  o.set_attribute j "type" "range" ;
  o.set_property j (Value "200") ;
  equal string "200" (F.value j)

(* A select with a size above 1 or with multiple is not a drop-down: it can have
   no selected option. *)
let select_size () =
  let _, o, root = setup () in
  let el name = o.Dom.create_element Html name in
  let opt t =
    let n = el "option" in
    o.insert_before n (text o t) None ;
    n
  in
  let s = el "select" in
  o.insert_before root s None ;
  o.set_attribute s "size" "4" ;
  o.insert_before s (opt "a") None ;
  equal string "" (F.value s) ;
  o.set_property s (Value "a") ;
  equal string "a" (F.value s) ;
  o.set_property s (Value "x") ;
  o.insert_before s (opt "b") None ;
  equal string "" (F.value s) ;
  (* Another attribute: no change. *)
  o.set_attribute s "class" "c" ;
  equal string "" (F.value s) ;
  o.set_attribute s "size" "1" ;
  equal string "a" (F.value s) ;
  o.set_property s (Value "x") ;
  o.set_attribute s "size" "x" ;
  equal string "a" (F.value s) ;
  o.set_property s (Value "x") ;
  o.remove_attribute s "size" ;
  equal string "a" (F.value s) ;
  o.set_attribute s "multiple" "" ;
  o.set_property s (Value "x") ;
  o.remove s (List.hd (F.children s)) ;
  equal string "" (F.value s) ;
  o.set_attribute s "size" "0" ;
  equal string "" (F.value s) ;
  o.remove_attribute s "multiple" ;
  equal string "b" (F.value s) ;
  equal string
    "<select class=\"c\" size=\"0\" \
     .value=\"b\"><option>\"b\"</option></select>"
    (F.dump s)

(* The value with no write, and the dump of a value. *)
let default_value () =
  let _, o, root = setup () in
  let el name = o.Dom.create_element Html name in
  let add n = o.insert_before root n None in
  let r = el "input" in
  add r ;
  o.set_attribute r "type" "range" ;
  equal string "50" (F.value r) ;
  o.set_attribute r "value" "70" ;
  equal string "70" (F.value r) ;
  o.set_attribute r "value" "170" ;
  equal string "100" (F.value r) ;
  let t = el "textarea" and q = el "option" and d = el "div" in
  add t ;
  add q ;
  add d ;
  o.insert_before t (text o "t") None ;
  o.insert_before q (text o "q") None ;
  o.insert_before d (text o "d") None ;
  equal string "t" (F.value t) ;
  equal string "q" (F.value q) ;
  equal string "" (F.value d) ;
  o.set_attribute q "value" "v" ;
  equal string "v" (F.value q) ;
  (* A write of the value with no write gives no .value in the dump. *)
  o.set_property t (Value "t") ;
  o.set_property q (Value "w") ;
  o.set_property d (Value "") ;
  o.set_property r (Value "200") ;
  equal string
    "<div><input type=\"range\" \
     value=\"170\"><textarea>\"t\"</textarea><option value=\"v\" \
     .value=\"w\">\"q\"</option><div>\"d\"</div></div>"
    (F.dump root) ;
  o.set_property r (Value "20") ;
  o.set_property d (Value "e") ;
  equal string
    "<div><input type=\"range\" value=\"170\" \
     .value=\"20\"><textarea>\"t\"</textarea><option value=\"v\" \
     .value=\"w\">\"q\"</option><div .value=\"e\">\"d\"</div></div>"
    (F.dump root)

(* The model of a radio group. *)
let radio_model () =
  let t, o, root = setup () in
  let el name = o.Dom.create_element Html name in
  let radio ?(ty = "radio") ?name parent =
    let r = el "input" in
    o.set_attribute r "type" ty ;
    Option.iter (o.set_attribute r "name") name ;
    o.insert_before parent r None ;
    r
  in
  let state l = List.map F.checked l in
  let a = radio ~name:"g" root and b = radio ~name:"g" root in
  let c = radio ~name:"h" root and d = radio root and e = radio root in
  let all = [a; b; c; d; e] in
  equal (list bool) [false; false; false; false; false] (state all) ;
  o.set_property a (Checked true) ;
  o.set_property c (Checked true) ;
  o.set_property d (Checked true) ;
  o.set_property e (Checked true) ;
  equal (list bool) [true; false; true; true; true] (state all) ;
  (* A write of true unchecks the group. *)
  o.set_property b (Checked true) ;
  equal (list bool) [false; true; true; true; true] (state all) ;
  o.set_property b (Checked false) ;
  equal (list bool) [false; false; true; true; true] (state all) ;
  (* A checked button joins a group: a change of name or of type. *)
  o.set_property a (Checked true) ;
  o.set_attribute c "name" "g" ;
  equal (list bool) [false; false; true; true; true] (state all) ;
  o.set_attribute a "class" "x" ;
  equal (list bool) [false; false; true; true; true] (state all) ;
  let x = radio ~ty:"text" ~name:"g" root in
  o.set_property x (Checked true) ;
  equal (list bool) [false; false; true; true; true] (state all) ;
  o.set_attribute x "type" "RADIO" ;
  equal (list bool) [false; false; false; true; true] (state all) ;
  equal bool true (F.checked x) ;
  (* A form makes another group, also for a move of a checked button. *)
  let f = el "form" in
  o.insert_before root f None ;
  let y = radio ~name:"g" f in
  o.set_property y (Checked true) ;
  equal bool true (F.checked x) ;
  o.remove root x ;
  o.insert_before f x None ;
  equal bool false (F.checked y) ;
  (* An insert of a subtree: the groups of its checked buttons. *)
  let z = el "div" in
  let w = radio ~name:"g" z in
  o.set_property w (Checked true) ;
  o.set_property a (Checked true) ;
  equal bool true (F.checked w) ;
  o.insert_before root z None ;
  equal (list bool) [false; true] [F.checked a; F.checked w] ;
  let z2 = el "div" in
  o.set_property (radio ~name:"g" z2) (Checked true) ;
  o.set_property a (Checked true) ;
  o.replace root z2 z ;
  equal (list bool) [false; true] [F.checked a; F.checked x] ;
  (* The name is case-sensitive. A button with no name or an empty name has no
     group. *)
  o.set_attribute b "name" "G" ;
  o.set_property b (Checked true) ;
  o.set_attribute e "name" "" ;
  o.set_property e (Checked true) ;
  equal (list bool) [false; true; false; true; true] (state all) ;
  (* The browser writes. *)
  F.browser_set a (Checked true) ;
  equal (list bool) [true; true; false; true; true] (state all) ;
  o.set_attribute a "name" "G" ;
  equal (list bool) [true; false; false; true; true] (state all) ;
  F.browser_set b (Checked true) ;
  equal (list bool) [false; true; false; true; true] (state all) ;
  F.browser_set b (Checked false) ;
  equal (list bool) [false; false; false; true; true] (state all) ;
  let s = el "select" and i = el "input" in
  o.insert_before root s None ;
  o.insert_before s (el "option") None ;
  o.insert_before root i None ;
  o.set_attribute i "type" "range" ;
  F.browser_set s (Value "x") ;
  F.browser_set i (Value "200") ;
  equal (list string) [""; "100"] [F.value s; F.value i] ;
  (* No count. *)
  F.reset t ;
  F.browser_set a (Checked true) ;
  equal int 0 (F.total t)

(* The reads of the live properties count. *)
let reads () =
  let t, o, root = setup () in
  let i = o.Dom.create_element Html "input" in
  o.insert_before root i None ;
  F.browser_set i (Value "v") ;
  F.browser_set i (Checked true) ;
  F.reset t ;
  equal string "v" (o.get_value i) ;
  is_true (o.get_checked i) ;
  equal
    (list (pair string int))
    [("get_checked", 1); ("get_value", 1)]
    (F.counts t) ;
  F.fail_at t 1 ;
  raises_match
    (function F.Fault -> true | _ -> false)
    (fun () -> o.get_value i)

let faults () =
  let t, o, root = setup () in
  let a = text o "a" in
  F.fail_at t 2 ;
  o.insert_before root a None ;
  raises_match
    (function F.Fault -> true | _ -> false)
    (fun () -> o.set_text a "b") ;
  equal string "a" (F.text a) ;
  equal int 2 (F.total t) ;
  o.set_text a "c" ;
  equal string "c" (F.text a) ;
  F.fail_at t 1 ;
  raises_match
    (function F.Fault -> true | _ -> false)
    (fun () -> o.remove root a) ;
  is_true (parent_is a root) ;
  o.remove root a ;
  equal int 4 (F.total t)

let fire () =
  let got = ref [] in
  let t, o, root = setup () in
  ignore root ;
  let e = o.create_element Html "button" in
  misuse (fun () -> F.fire e "click" Dom.Unit ()) ;
  let c =
    cell
      [ ev "click" Unit (fun () -> "click")
      ; ev "input" String Fun.id
      ; ev "keydown" Key (fun s -> "key " ^ s)
      ; ev "change" Bool string_of_bool ]
      (fun m -> got := m :: !got)
  in
  o.attach e c ;
  F.fire e "click" Dom.Unit () ;
  F.fire e "input" Dom.String "s" ;
  F.fire e "keydown" Dom.Key "k" ;
  F.fire e "change" Dom.Bool true ;
  equal (list string) ["true"; "key k"; "s"; "click"] !got ;
  misuse (fun () -> F.fire e "click" Dom.String "x") ;
  misuse (fun () -> F.fire e "input" Dom.Key "x") ;
  misuse (fun () -> F.fire e "keydown" Dom.Bool true) ;
  misuse (fun () -> F.fire e "change" Dom.Unit ()) ;
  misuse (fun () -> F.fire e "scroll" Dom.Unit ()) ;
  ignore t

let tests =
  [ group "fake DOM"
      [ test "insert, move, remove, replace and clear" tree
      ; test "an operation that the diff must not do raises" misuses
      ; test "attributes, properties, HTML and the dump" state
      ; test "the model of a select" select_model
      ; test "the model of a range input" range_model
      ; test "a select with a size or multiple" select_size
      ; test "the value with no write and the dump" default_value
      ; test "the model of a radio group, and the writes of the browser"
          radio_model
      ; test "the reads of the live properties" reads
      ; test "a fault raises one time, with no effect" faults
      ; test "fire calls the handler with its payload" fire ] ]
