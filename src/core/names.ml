module S = Set.Make (String)
module M = Map.Make (String)

let void = ["area"; "br"; "col"; "hr"; "img"; "input"; "source"; "track"; "wbr"]

let html =
  [ "a"
  ; "abbr"
  ; "address"
  ; "article"
  ; "aside"
  ; "audio"
  ; "b"
  ; "bdi"
  ; "bdo"
  ; "blockquote"
  ; "button"
  ; "canvas"
  ; "caption"
  ; "cite"
  ; "code"
  ; "colgroup"
  ; "data"
  ; "datalist"
  ; "dd"
  ; "del"
  ; "details"
  ; "dfn"
  ; "dialog"
  ; "div"
  ; "dl"
  ; "dt"
  ; "em"
  ; "fieldset"
  ; "figcaption"
  ; "figure"
  ; "footer"
  ; "form"
  ; "h1"
  ; "h2"
  ; "h3"
  ; "h4"
  ; "h5"
  ; "h6"
  ; "header"
  ; "hgroup"
  ; "i"
  ; "ins"
  ; "kbd"
  ; "label"
  ; "legend"
  ; "li"
  ; "main"
  ; "map"
  ; "mark"
  ; "menu"
  ; "meter"
  ; "nav"
  ; "ol"
  ; "optgroup"
  ; "option"
  ; "output"
  ; "p"
  ; "picture"
  ; "pre"
  ; "progress"
  ; "q"
  ; "rp"
  ; "rt"
  ; "ruby"
  ; "s"
  ; "samp"
  ; "search"
  ; "section"
  ; "select"
  ; "slot"
  ; "small"
  ; "span"
  ; "strong"
  ; "style"
  ; "sub"
  ; "summary"
  ; "sup"
  ; "table"
  ; "tbody"
  ; "td"
  ; "textarea"
  ; "tfoot"
  ; "th"
  ; "thead"
  ; "time"
  ; "title"
  ; "tr"
  ; "u"
  ; "ul"
  ; "var"
  ; "video" ]

let svg =
  [ "svg"
  ; "g"
  ; "defs"
  ; "symbol"
  ; "use"
  ; "image"
  ; "switch"
  ; "desc"
  ; "metadata"
  ; "path"
  ; "rect"
  ; "circle"
  ; "ellipse"
  ; "line"
  ; "polyline"
  ; "polygon"
  ; "text"
  ; "tspan"
  ; "textPath"
  ; "marker"
  ; "pattern"
  ; "clipPath"
  ; "mask"
  ; "linearGradient"
  ; "radialGradient"
  ; "stop"
  ; "filter"
  ; "feBlend"
  ; "feColorMatrix"
  ; "feComponentTransfer"
  ; "feComposite"
  ; "feConvolveMatrix"
  ; "feDiffuseLighting"
  ; "feDisplacementMap"
  ; "feDistantLight"
  ; "feDropShadow"
  ; "feFlood"
  ; "feFuncA"
  ; "feFuncB"
  ; "feFuncG"
  ; "feFuncR"
  ; "feGaussianBlur"
  ; "feImage"
  ; "feMerge"
  ; "feMergeNode"
  ; "feMorphology"
  ; "feOffset"
  ; "fePointLight"
  ; "feSpecularLighting"
  ; "feSpotLight"
  ; "feTile"
  ; "feTurbulence"
  ; "foreignObject"
  ; "view" ]

let all = void @ html @ svg

(* The usual spelling of each name, from the name in lowercase. *)
let spellings =
  List.fold_left
    (fun m name -> M.add (String.lowercase_ascii name) name m)
    M.empty all

let voids = S.of_list void

let element key = M.find_opt key spellings

let is_void name = S.mem name voids

let is_start c = (c >= 'a' && c <= 'z') || c = '_'

let is_char c = is_start c || (c >= '0' && c <= '9') || c = '-' || c = '.'

let attribute_ok name =
  let n = String.length name in
  let rec chars i = i = n || (is_char name.[i] && chars (i + 1)) in
  n > 0
  && is_start name.[0]
  && chars 1
  && (not (String.starts_with ~prefix:"on" name))
  && not (String.equal name "srcdoc")
