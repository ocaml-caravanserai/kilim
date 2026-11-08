(* The view constructors: the allowlist, the name checks, the URL check, the
   attributes, the events and the keys (design sections 08 and 13). *)

open Windtrap
open Kilim
open Support

let refused f =
  raises_match (Exn.invalid_arg ~substring:"kilim") (fun () -> ignore (f ()))

let element (v : string View.t) =
  match v with
  | V.Element e ->
      e
  | V.Text _ | V.Comp _ | V.Boundary _ | V.Provide _ | V.Read _ ->
      fail "not an element"

let refused_elements =
  [ "script"
  ; "SCRIPT"
  ; "Script"
  ; "iframe"
  ; "IFrame"
  ; "object"
  ; "embed"
  ; "base"
  ; "meta"
  ; "Meta"
  ; "link"
  ; "LINK"
  ; "set"
  ; "animate"
  ; "animateMotion"
  ; "ANIMATEMOTION"
  ; "animateTransform"
  ; "animatetransform"
  ; "frame"
  ; "template"
  ; "noscript"
  ; "html"
  ; "body"
  ; "my-widget"
  ; ""
  ; "div " ]

let allowlist () =
  List.iter (fun n -> refused (fun () -> El.v n [])) refused_elements ;
  List.iter
    (fun (n, usual) -> equal string usual (element (El.v n [])).name)
    [ ("div", "div")
    ; ("DIV", "div")
    ; ("Div", "div")
    ; ("SVG", "svg")
    ; ("foreignobject", "foreignObject")
    ; ("FOREIGNOBJECT", "foreignObject")
    ; ("feimage", "feImage")
    ; ("A", "a") ]

(* Each constructor of El and the usual spelling of its name. *)
let constructors =
  [ (El.area (), "area")
  ; (El.br (), "br")
  ; (El.col (), "col")
  ; (El.hr (), "hr")
  ; (El.img (), "img")
  ; (El.input (), "input")
  ; (El.source (), "source")
  ; (El.track (), "track")
  ; (El.wbr (), "wbr")
  ; (El.a [], "a")
  ; (El.abbr [], "abbr")
  ; (El.address [], "address")
  ; (El.article [], "article")
  ; (El.aside [], "aside")
  ; (El.audio [], "audio")
  ; (El.b [], "b")
  ; (El.bdi [], "bdi")
  ; (El.bdo [], "bdo")
  ; (El.blockquote [], "blockquote")
  ; (El.button [], "button")
  ; (El.canvas [], "canvas")
  ; (El.caption [], "caption")
  ; (El.cite [], "cite")
  ; (El.code [], "code")
  ; (El.colgroup [], "colgroup")
  ; (El.data [], "data")
  ; (El.datalist [], "datalist")
  ; (El.dd [], "dd")
  ; (El.del [], "del")
  ; (El.details [], "details")
  ; (El.dfn [], "dfn")
  ; (El.dialog [], "dialog")
  ; (El.div [], "div")
  ; (El.dl [], "dl")
  ; (El.dt [], "dt")
  ; (El.em [], "em")
  ; (El.fieldset [], "fieldset")
  ; (El.figcaption [], "figcaption")
  ; (El.figure [], "figure")
  ; (El.footer [], "footer")
  ; (El.form [], "form")
  ; (El.h1 [], "h1")
  ; (El.h2 [], "h2")
  ; (El.h3 [], "h3")
  ; (El.h4 [], "h4")
  ; (El.h5 [], "h5")
  ; (El.h6 [], "h6")
  ; (El.header [], "header")
  ; (El.hgroup [], "hgroup")
  ; (El.i [], "i")
  ; (El.ins [], "ins")
  ; (El.kbd [], "kbd")
  ; (El.label [], "label")
  ; (El.legend [], "legend")
  ; (El.li [], "li")
  ; (El.main [], "main")
  ; (El.map [], "map")
  ; (El.mark [], "mark")
  ; (El.menu [], "menu")
  ; (El.meter [], "meter")
  ; (El.nav [], "nav")
  ; (El.ol [], "ol")
  ; (El.optgroup [], "optgroup")
  ; (El.option [], "option")
  ; (El.output [], "output")
  ; (El.p [], "p")
  ; (El.picture [], "picture")
  ; (El.pre [], "pre")
  ; (El.progress [], "progress")
  ; (El.q [], "q")
  ; (El.rp [], "rp")
  ; (El.rt [], "rt")
  ; (El.ruby [], "ruby")
  ; (El.s [], "s")
  ; (El.samp [], "samp")
  ; (El.search [], "search")
  ; (El.section [], "section")
  ; (El.select [], "select")
  ; (El.slot [], "slot")
  ; (El.small [], "small")
  ; (El.span [], "span")
  ; (El.strong [], "strong")
  ; (El.style [], "style")
  ; (El.sub [], "sub")
  ; (El.summary [], "summary")
  ; (El.sup [], "sup")
  ; (El.table [], "table")
  ; (El.tbody [], "tbody")
  ; (El.td [], "td")
  ; (El.textarea [], "textarea")
  ; (El.tfoot [], "tfoot")
  ; (El.th [], "th")
  ; (El.thead [], "thead")
  ; (El.time [], "time")
  ; (El.title [], "title")
  ; (El.tr [], "tr")
  ; (El.u [], "u")
  ; (El.ul [], "ul")
  ; (El.var [], "var")
  ; (El.video [], "video")
  ; (El.svg [], "svg")
  ; (El.g [], "g")
  ; (El.defs [], "defs")
  ; (El.symbol [], "symbol")
  ; (El.use [], "use")
  ; (El.image [], "image")
  ; (El.switch [], "switch")
  ; (El.desc [], "desc")
  ; (El.metadata [], "metadata")
  ; (El.path [], "path")
  ; (El.rect [], "rect")
  ; (El.circle [], "circle")
  ; (El.ellipse [], "ellipse")
  ; (El.line [], "line")
  ; (El.polyline [], "polyline")
  ; (El.polygon [], "polygon")
  ; (El.text [], "text")
  ; (El.tspan [], "tspan")
  ; (El.text_path [], "textPath")
  ; (El.marker [], "marker")
  ; (El.pattern [], "pattern")
  ; (El.clip_path [], "clipPath")
  ; (El.mask [], "mask")
  ; (El.linear_gradient [], "linearGradient")
  ; (El.radial_gradient [], "radialGradient")
  ; (El.stop [], "stop")
  ; (El.filter [], "filter")
  ; (El.fe_blend [], "feBlend")
  ; (El.fe_color_matrix [], "feColorMatrix")
  ; (El.fe_component_transfer [], "feComponentTransfer")
  ; (El.fe_composite [], "feComposite")
  ; (El.fe_convolve_matrix [], "feConvolveMatrix")
  ; (El.fe_diffuse_lighting [], "feDiffuseLighting")
  ; (El.fe_displacement_map [], "feDisplacementMap")
  ; (El.fe_distant_light [], "feDistantLight")
  ; (El.fe_drop_shadow [], "feDropShadow")
  ; (El.fe_flood [], "feFlood")
  ; (El.fe_func_a [], "feFuncA")
  ; (El.fe_func_b [], "feFuncB")
  ; (El.fe_func_g [], "feFuncG")
  ; (El.fe_func_r [], "feFuncR")
  ; (El.fe_gaussian_blur [], "feGaussianBlur")
  ; (El.fe_image [], "feImage")
  ; (El.fe_merge [], "feMerge")
  ; (El.fe_merge_node [], "feMergeNode")
  ; (El.fe_morphology [], "feMorphology")
  ; (El.fe_offset [], "feOffset")
  ; (El.fe_point_light [], "fePointLight")
  ; (El.fe_specular_lighting [], "feSpecularLighting")
  ; (El.fe_spot_light [], "feSpotLight")
  ; (El.fe_tile [], "feTile")
  ; (El.fe_turbulence [], "feTurbulence")
  ; (El.foreign_object [], "foreignObject")
  ; (El.view [], "view") ]

(* Each constructor gives its name, El.v gives the same name in any case, and
   the table has each name of the allowlist one time. *)
let each_constructor () =
  List.iter
    (fun (v, n) ->
      equal string n (element v).name ;
      equal string n (element (El.v n [])).name ;
      equal string n (element (El.v (String.uppercase_ascii n) [])).name ;
      equal string n (element (El.v (String.lowercase_ascii n) [])).name )
    constructors ;
  let names = List.map snd constructors in
  equal int (List.length Private.Names.all) (List.length names) ;
  equal (list string)
    (List.sort compare Private.Names.all)
    (List.sort_uniq compare names)

(* A diff of [El.v "DIV"] after [El.div] keeps the node, and [El.v "SVG"] starts
   the SVG namespace. *)
let case_in_the_diff () =
  let p = mount (El.div [El.v "SVG" [El.v "foreignobject" [El.v "P" []]]]) in
  equal string
    "<div><div><svg:svg><svg:foreignObject><p></p></svg:foreignObject></svg:svg></div></div>"
    (dump p) ;
  let n = node p in
  patch p (El.v "DIV" [El.svg [El.foreign_object [El.p []]]]) ;
  equal (list (pair string int)) [] (counts p) ;
  is_true (n == node p)

let void_children () =
  refused (fun () -> El.v "br" [txt "x"]) ;
  refused (fun () -> El.v "INPUT" [txt "x"]) ;
  equal int 0
    ( match (element (El.v "br" [])).children with
    | V.Plain l ->
        List.length l
    | _ ->
        -1 ) ;
  ignore (El.v "div" [txt "x"])

let attr_names () =
  List.iter
    (fun n -> refused (fun () -> At.v n "x"))
    [ "onclick"
    ; "ONCLICK"
    ; "OnLoad"
    ; "on"
    ; "one"
    ; "srcdoc"
    ; "SrcDoc"
    ; "data-a b"
    ; ""
    ; "1a"
    ; "-a"
    ; ".a"
    ; "a:b"
    ; "xlink:href"
    ; "a/b"
    ; "a=b"
    ; "a\""
    ; "\xc3\xa9" ] ;
  List.iter
    (fun n ->
      match At.v n "x" with
      | V.Attr a ->
          equal string n a.name ;
          equal string (String.lowercase_ascii n) a.key
      | _ ->
          fail "not an attribute" )
    [ "data-x"
    ; "aria-label"
    ; "_a"
    ; "a.b"
    ; "viewBox"
    ; "Z9"
    ; "a-0"
    ; "zz"
    ; "o"
    ; "no" ]

(* [accepts name s ok]: [At.v name s] gives an attribute if [ok], and raises
   [Invalid_argument] if not. *)
let accepts name s ok =
  match At.v name s with
  | _ ->
      is_true ~msg:(name ^ " " ^ String.escaped s) ok
  | exception Invalid_argument _ ->
      is_false ~msg:(name ^ " " ^ String.escaped s) ok

(* [url] is permitted in [href]. *)
let urls =
  [ ("http://a.b/", true)
  ; ("https://a.b/", true)
  ; ("HTTPS://a.b/", true)
  ; ("mailto:a@b.c", true)
  ; ("tel:+123", true)
  ; ("TeL:1", true)
  ; ("/a/b", true)
  ; ("a/b", true)
  ; ("//evil.com", true)
  ; ("?q=1", true)
  ; ("#x", true)
  ; ("", true)
  ; ("   ", true)
  ; ("java script:x", true)
  ; (":x", true)
  ; ("1a:x", true)
  ; ("javascript&#58;alert(1)", true)
  ; ("java\x00script:x", true)
  ; ("javascript:alert(1)", false)
  ; ("JAVASCRIPT:x", false)
  ; (" javascript:x", false)
  ; ("\x01javascript:x", false)
  ; ("\x1f\x20javascript:x", false)
  ; ("java\tscript:x", false)
  ; ("java\nscript:x", false)
  ; ("java\rscript:x", false)
  ; ("\tjavascript:x", false)
  ; ("\njavascript:x", false)
  ; ("data:text/html,x", false)
  ; ("vbscript:x", false)
  ; ("a:x", false)
  ; ("z:x", false)
  ; ("A:x", false)
  ; ("Z:x", false)
  ; ("x0:y", false)
  ; ("x9:y", false)
  ; ("x+y:z", false)
  ; ("x-y:z", false)
  ; ("x.y:z", false)
  ; ("httpx:y", false)
  ; ("http:", true)
  ; ("h\nttps:", true)
  ; ("ftp:x", false)
  ; ("blob:x", false) ]

let url_check () =
  List.iter
    (fun (u, ok) ->
      List.iter
        (fun name -> accepts name u ok)
        [ "href"
        ; "HREF"
        ; "src"
        ; "action"
        ; "formaction"
        ; "poster"
        ; "cite"
        ; "data"
        ; "background" ] ;
      (* Not a URL attribute: no check. *)
      ignore (At.v "title" u) )
    urls ;
  List.iter
    (fun f ->
      refused (fun () -> f "javascript:x") ;
      ignore (f "/x") )
    [At.href; At.src; At.action]

let srcsets =
  [ ("a.png", true)
  ; ("a.png 1x, b.png 2x", true)
  ; ("a.png 1x,b.png 2x", true)
  ; ("a.png, javascript:x", false)
  ; ("a.png 1x, javascript:x 2x", false)
  ; ("a.png 1x,javascript:x", false)
  ; ("a.png,javascript:x", true)
  ; ("javascript:x 1x", false)
  ; (" ,, javascript:x", false)
  ; ("a.png 1x (javascript:x), b.png", true)
  ; ("a.png x(, javascript:x), b.png", true)
  ; ("a.png x(, b), javascript:x", false)
  ; ("a.png x, javascript:x", false)
  ; ("a.png 1x\tjavascript:x", true)
  ; ("a.png,\tjavascript:x", false)
  ; ("a.png,\njavascript:x", false)
  ; ("a.png,\x0cjavascript:x", false)
  ; ("a.png,\rjavascript:x", false)
  ; ("a.png 1x, data:x", false)
  ; ("a,b.png 1x", true)
  ; ("a.png 1x,, javascript:x", false)
  ; ("a.png 1x x(", true)
  ; ("a.png x(()), javascript:x", false)
  ; ("", true)
  ; (",", true) ]

let srcset () = List.iter (fun (s, ok) -> accepts "srcset" s ok) srcsets

let ping () =
  List.iter
    (fun (s, ok) -> accepts "ping" s ok)
    [ ("/a /b", true)
    ; ("/a javascript:x", false)
    ; ("/a\tjavascript:x", false)
    ; ("/a\njavascript:x", false)
    ; ("/a\x0cjavascript:x", false)
    ; ("/a\rjavascript:x", false)
    ; ("  /a  ", true)
    ; ("", true)
    ; ("javascript:x /a", false)
    ; ("/a,javascript:x", true) ]

let attrs ats = (element (El.div ~at:ats [])).attrs

let attributes () =
  equal
    (list (pair string string))
    [("type", "text"); ("id", "a"); ("title", "t"); ("class", "x y")]
    (attrs
       [ At.id "a"
       ; At.class_ "x"
       ; At.none
       ; At.title "t"
       ; At.class_ ""
       ; At.type_ "text"
       ; At.classes [""; "y"] ] ) ;
  equal (list (pair string string)) [] (attrs [At.class_ ""; At.classes []]) ;
  equal
    (list (pair string string))
    [("type", "a"); ("class", "c")]
    (attrs [At.v "CLASS" "c"; At.v "type" "a"]) ;
  refused (fun () -> El.div ~at:[At.id "a"; At.v "ID" "b"] []) ;
  refused (fun () -> El.div ~at:[At.type_ "a"; At.type_ "b"] []) ;
  refused (fun () -> El.div ~at:[At.value "a"; At.value "b"] []) ;
  refused (fun () -> El.div ~at:[At.checked true; At.checked false] []) ;
  let e =
    element (El.input ~at:[At.value "v"; At.checked true; At.v "value" "w"] ())
  in
  equal (option string) (Some "v") e.value ;
  equal (option bool) (Some true) e.checked ;
  equal (list (pair string string)) [("value", "w")] e.attrs ;
  let named =
    [ (At.id, "id")
    ; (At.href, "href")
    ; (At.src, "src")
    ; (At.action, "action")
    ; (At.method_, "method")
    ; (At.name, "name")
    ; (At.type_, "type")
    ; (At.for_, "for")
    ; (At.alt, "alt")
    ; (At.title, "title")
    ; (At.lang, "lang")
    ; (At.rel, "rel")
    ; (At.target, "target")
    ; (At.placeholder, "placeholder")
    ; (At.style, "style")
    ; (At.role, "role")
    ; (At.aria_label, "aria-label")
    ; (At.width, "width")
    ; (At.height, "height")
    ; (At.class_, "class")
    ; (At.data "k", "data-k")
    ; (At.aria "busy", "aria-busy") ]
  in
  List.iter
    (fun (f, n) -> equal (list (pair string string)) [(n, "v")] (attrs [f "v"]))
    named ;
  equal
    (list (pair string string))
    [("tabindex", "-1")]
    (attrs [At.tabindex (-1)]) ;
  List.iter
    (fun (a, n) -> equal (list (pair string string)) [(n, "")] (attrs [a]))
    [ (At.disabled, "disabled")
    ; (At.hidden, "hidden")
    ; (At.required, "required")
    ; (At.readonly, "readonly")
    ; (At.multiple, "multiple")
    ; (At.autofocus, "autofocus")
    ; (At.selected, "selected")
    ; (At.bool "open", "open") ] ;
  refused (fun () -> At.data "" "x") ;
  refused (fun () -> At.aria "" "x") ;
  refused (fun () -> At.data "a b" "x") ;
  refused (fun () -> At.bool "onx")

let events () =
  let check (h : string Ev.t) event bubbles payload =
    equal string event h.event ;
    equal bool bubbles h.bubbles ;
    is_false h.prevent_default ;
    is_false h.stop ;
    match h.decode with
    | V.Decode (p, f) -> (
      match (p, payload) with
      | Private.Dom.Unit, `Unit ->
          equal string "m" (f ())
      | Private.Dom.String, `String ->
          equal string "s!" (f "s")
      | Private.Dom.Key, `Key ->
          equal string "k!" (f "k")
      | Private.Dom.Bool, `Bool ->
          equal string "true" (f true)
      | _ ->
          fail ("the payload of " ^ event) )
  in
  let bang s = s ^ "!" in
  check (Ev.click "m") "click" true `Unit ;
  check (Ev.check string_of_bool) "click" true `Bool ;
  check (Ev.input bang) "input" true `String ;
  check (Ev.change bang) "change" true `String ;
  check (Ev.submit "m") "submit" true `Unit ;
  check (Ev.keydown bang) "keydown" true `Key ;
  check (Ev.keyup bang) "keyup" true `Key ;
  check (Ev.focus "m") "focusin" true `Unit ;
  check (Ev.blur "m") "focusout" true `Unit ;
  check (Ev.mouseenter "m") "mouseenter" false `Unit ;
  check (Ev.mouseleave "m") "mouseleave" false `Unit ;
  check (Ev.scroll "m") "scroll" false `Unit ;
  let h = Ev.submit ~prevent_default:true ~stop:true "m" in
  is_true (h.prevent_default && h.stop) ;
  refused (fun () -> El.div ~ev:[Ev.click "a"; Ev.check string_of_bool] []) ;
  refused (fun () -> El.div ~ev:[Ev.focus "a"; Ev.blur "b"; Ev.focus "c"] []) ;
  equal int 2
    (List.length (element (El.div ~ev:[Ev.focus "a"; Ev.blur "b"] [])).events)

let keys () =
  let module K = Private.Key in
  let i = Key.int and s = Key.string in
  is_true (K.equal (i 1) (i 1)) ;
  is_false (K.equal (i 1) (i 2)) ;
  is_true (K.equal (s "a") (s "a")) ;
  is_false (K.equal (s "a") (s "b")) ;
  is_false (K.equal (i 1) (s "1")) ;
  is_false (K.equal (s "1") (i 1)) ;
  equal int (K.hash (i 7)) (K.hash (i 7)) ;
  equal int (K.hash (s "ab")) (K.hash (s (String.make 1 'a' ^ "b"))) ;
  (* The values of the hash. test/cross makes sure that the three builds give
     the same values for ints below 2^30 and for strings. *)
  equal (list int)
    [0; 749930575; 719903339; 450586307; 789315851; 586705084]
    (List.map (fun x -> K.hash (i x)) [0; 1; 2; 7; 1000; 0x3fff_ffff]) ;
  equal (list int)
    [493303829; 122118257; 232771046; 110798886; 566909467; 923304462]
    (List.map
       (fun x -> K.hash (s x))
       [""; "a"; "row-1"; "Aa"; "BB"; "\255\255"] ) ;
  (* Native code: the bits above bit 29. *)
  equal (list int)
    [381853419; 18459301; 303064771; 336610466; 887827958; 915574941]
    (List.map
       (fun x -> K.hash (i x))
       [1 lsl 30; 1 lsl 61; -1; min_int; max_int; (3 lsl 30) + 5] ) ;
  (* The order of the map. *)
  equal (list int)
    [-1; 0; 1; -1; 0; 1; -1; 1]
    (List.map
       (fun (a, b) -> Int.compare (K.compare a b) 0)
       [ (i 1, i 2)
       ; (i 2, i 2)
       ; (i 3, i 2)
       ; (s "a", s "b")
       ; (s "b", s "b")
       ; (s "c", s "b")
       ; (i 9, s "1")
       ; (s "1", i 9) ] ) ;
  List.iter
    (fun k -> is_true (K.hash k >= 0))
    [i (-1); i min_int; i max_int; s ""; s (String.make 100 '\255')] ;
  equal string "7" (K.to_string (i 7)) ;
  equal string "x" (K.to_string (s "x"))

type Private.Unsafe_html.native += Native of int

let unsafe () =
  let module U = Private.Unsafe_html in
  let h = Unsafe_html.of_string "<b>x</b>" in
  equal string "<b>x</b>" (Unsafe_html.to_string h) ;
  is_true (Option.is_none (U.native h)) ;
  let n = U.of_native "<i>y</i>" (Native 3) in
  equal string "<i>y</i>" (Unsafe_html.to_string n) ;
  is_true (match U.native n with Some (Native 3) -> true | _ -> false) ;
  ( match (element (View.unsafe_html ~el:El.div h)).children with
  | V.Inner h' ->
      is_true (h == h')
  | _ ->
      fail "not inner" ) ;
  refused (fun () -> View.unsafe_html ~el:(fun _ -> txt "x") h) ;
  refused (fun () -> View.unsafe_html ~el:(El.v "br") h)

let keyed () =
  let e = element (View.keyed (El.ul ~at:[At.id "l"]) (rows [1; 2])) in
  equal string "ul" e.name ;
  equal (list (pair string string)) [("id", "l")] e.attrs ;
  ( match e.children with
  | V.Keyed l ->
      equal (list string) ["1"; "2"]
        (List.map (fun (k, _) -> Private.Key.to_string k) l)
  | _ ->
      fail "not keyed" ) ;
  refused (fun () -> View.keyed (fun _ -> txt "x") []) ;
  refused (fun () -> View.keyed (El.v "BR") []) ;
  equal string "x" (match txt "x" with V.Text s -> s | _ -> "")

let tests =
  [ group "view"
      [ test "El.v refuses the names that are not on the allowlist, in any case"
          allowlist
      ; test "each constructor gives the usual spelling of its name"
          each_constructor
      ; test "El.v in any case gives the same node and namespace"
          case_in_the_diff
      ; test "a void element refuses children" void_children
      ; test "At.v checks the name" attr_names
      ; test "the URL check finds the scheme as WHATWG does" url_check
      ; test "each URL of srcset is checked" srcset
      ; test "each URL of ping is checked" ping
      ; test "attributes: class join, type first, duplicates" attributes
      ; test "events: names, payloads and options" events
      ; test "keys: equal, hash and compare" keys
      ; test "unsafe HTML keeps its native value" unsafe
      ; test "View.keyed takes an element function" keyed ] ]
