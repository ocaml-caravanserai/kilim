(* The elements (design sections 08 and 13). The signature and its text are in
   kilim.mli. *)

type 'msg el =
     ?at:View.at list
  -> ?ev:'msg View.handler list
  -> 'msg View.t list
  -> 'msg View.t

type 'msg void =
  ?at:View.at list -> ?ev:'msg View.handler list -> unit -> 'msg View.t

let make name ?(at = []) ?(ev = []) children =
  View.element name at ev (View.Plain children)

let v name ?at ?ev children =
  match Names.element (String.lowercase_ascii name) with
  | None ->
      invalid_arg ("kilim: El.v: a refused element: " ^ name)
  | Some name ->
      ( match children with
      | _ :: _ when Names.is_void name ->
          invalid_arg ("kilim: El.v: a void element with children: " ^ name)
      | _ ->
          () ) ;
      make name ?at ?ev children

let void name ?at ?ev () = make name ?at ?ev []

let area ?at ?ev () = void "area" ?at ?ev ()

let br ?at ?ev () = void "br" ?at ?ev ()

let col ?at ?ev () = void "col" ?at ?ev ()

let hr ?at ?ev () = void "hr" ?at ?ev ()

let img ?at ?ev () = void "img" ?at ?ev ()

let input ?at ?ev () = void "input" ?at ?ev ()

let source ?at ?ev () = void "source" ?at ?ev ()

let track ?at ?ev () = void "track" ?at ?ev ()

let wbr ?at ?ev () = void "wbr" ?at ?ev ()

let a ?at ?ev children = make "a" ?at ?ev children

let abbr ?at ?ev children = make "abbr" ?at ?ev children

let address ?at ?ev children = make "address" ?at ?ev children

let article ?at ?ev children = make "article" ?at ?ev children

let aside ?at ?ev children = make "aside" ?at ?ev children

let audio ?at ?ev children = make "audio" ?at ?ev children

let b ?at ?ev children = make "b" ?at ?ev children

let bdi ?at ?ev children = make "bdi" ?at ?ev children

let bdo ?at ?ev children = make "bdo" ?at ?ev children

let blockquote ?at ?ev children = make "blockquote" ?at ?ev children

let button ?at ?ev children = make "button" ?at ?ev children

let canvas ?at ?ev children = make "canvas" ?at ?ev children

let caption ?at ?ev children = make "caption" ?at ?ev children

let cite ?at ?ev children = make "cite" ?at ?ev children

let code ?at ?ev children = make "code" ?at ?ev children

let colgroup ?at ?ev children = make "colgroup" ?at ?ev children

let data ?at ?ev children = make "data" ?at ?ev children

let datalist ?at ?ev children = make "datalist" ?at ?ev children

let dd ?at ?ev children = make "dd" ?at ?ev children

let del ?at ?ev children = make "del" ?at ?ev children

let details ?at ?ev children = make "details" ?at ?ev children

let dfn ?at ?ev children = make "dfn" ?at ?ev children

let dialog ?at ?ev children = make "dialog" ?at ?ev children

let div ?at ?ev children = make "div" ?at ?ev children

let dl ?at ?ev children = make "dl" ?at ?ev children

let dt ?at ?ev children = make "dt" ?at ?ev children

let em ?at ?ev children = make "em" ?at ?ev children

let fieldset ?at ?ev children = make "fieldset" ?at ?ev children

let figcaption ?at ?ev children = make "figcaption" ?at ?ev children

let figure ?at ?ev children = make "figure" ?at ?ev children

let footer ?at ?ev children = make "footer" ?at ?ev children

let form ?at ?ev children = make "form" ?at ?ev children

let h1 ?at ?ev children = make "h1" ?at ?ev children

let h2 ?at ?ev children = make "h2" ?at ?ev children

let h3 ?at ?ev children = make "h3" ?at ?ev children

let h4 ?at ?ev children = make "h4" ?at ?ev children

let h5 ?at ?ev children = make "h5" ?at ?ev children

let h6 ?at ?ev children = make "h6" ?at ?ev children

let header ?at ?ev children = make "header" ?at ?ev children

let hgroup ?at ?ev children = make "hgroup" ?at ?ev children

let i ?at ?ev children = make "i" ?at ?ev children

let ins ?at ?ev children = make "ins" ?at ?ev children

let kbd ?at ?ev children = make "kbd" ?at ?ev children

let label ?at ?ev children = make "label" ?at ?ev children

let legend ?at ?ev children = make "legend" ?at ?ev children

let li ?at ?ev children = make "li" ?at ?ev children

let main ?at ?ev children = make "main" ?at ?ev children

let map ?at ?ev children = make "map" ?at ?ev children

let mark ?at ?ev children = make "mark" ?at ?ev children

let menu ?at ?ev children = make "menu" ?at ?ev children

let meter ?at ?ev children = make "meter" ?at ?ev children

let nav ?at ?ev children = make "nav" ?at ?ev children

let ol ?at ?ev children = make "ol" ?at ?ev children

let optgroup ?at ?ev children = make "optgroup" ?at ?ev children

let option ?at ?ev children = make "option" ?at ?ev children

let output ?at ?ev children = make "output" ?at ?ev children

let p ?at ?ev children = make "p" ?at ?ev children

let picture ?at ?ev children = make "picture" ?at ?ev children

let pre ?at ?ev children = make "pre" ?at ?ev children

let progress ?at ?ev children = make "progress" ?at ?ev children

let q ?at ?ev children = make "q" ?at ?ev children

let rp ?at ?ev children = make "rp" ?at ?ev children

let rt ?at ?ev children = make "rt" ?at ?ev children

let ruby ?at ?ev children = make "ruby" ?at ?ev children

let s ?at ?ev children = make "s" ?at ?ev children

let samp ?at ?ev children = make "samp" ?at ?ev children

let search ?at ?ev children = make "search" ?at ?ev children

let section ?at ?ev children = make "section" ?at ?ev children

let select ?at ?ev children = make "select" ?at ?ev children

let slot ?at ?ev children = make "slot" ?at ?ev children

let small ?at ?ev children = make "small" ?at ?ev children

let span ?at ?ev children = make "span" ?at ?ev children

let strong ?at ?ev children = make "strong" ?at ?ev children

let style ?at ?ev children = make "style" ?at ?ev children

let sub ?at ?ev children = make "sub" ?at ?ev children

let summary ?at ?ev children = make "summary" ?at ?ev children

let sup ?at ?ev children = make "sup" ?at ?ev children

let table ?at ?ev children = make "table" ?at ?ev children

let tbody ?at ?ev children = make "tbody" ?at ?ev children

let td ?at ?ev children = make "td" ?at ?ev children

let textarea ?at ?ev children = make "textarea" ?at ?ev children

let tfoot ?at ?ev children = make "tfoot" ?at ?ev children

let th ?at ?ev children = make "th" ?at ?ev children

let thead ?at ?ev children = make "thead" ?at ?ev children

let time ?at ?ev children = make "time" ?at ?ev children

let title ?at ?ev children = make "title" ?at ?ev children

let tr ?at ?ev children = make "tr" ?at ?ev children

let u ?at ?ev children = make "u" ?at ?ev children

let ul ?at ?ev children = make "ul" ?at ?ev children

let var ?at ?ev children = make "var" ?at ?ev children

let video ?at ?ev children = make "video" ?at ?ev children

let svg ?at ?ev children = make "svg" ?at ?ev children

let g ?at ?ev children = make "g" ?at ?ev children

let defs ?at ?ev children = make "defs" ?at ?ev children

let symbol ?at ?ev children = make "symbol" ?at ?ev children

let use ?at ?ev children = make "use" ?at ?ev children

let image ?at ?ev children = make "image" ?at ?ev children

let switch ?at ?ev children = make "switch" ?at ?ev children

let desc ?at ?ev children = make "desc" ?at ?ev children

let metadata ?at ?ev children = make "metadata" ?at ?ev children

let path ?at ?ev children = make "path" ?at ?ev children

let rect ?at ?ev children = make "rect" ?at ?ev children

let circle ?at ?ev children = make "circle" ?at ?ev children

let ellipse ?at ?ev children = make "ellipse" ?at ?ev children

let line ?at ?ev children = make "line" ?at ?ev children

let polyline ?at ?ev children = make "polyline" ?at ?ev children

let polygon ?at ?ev children = make "polygon" ?at ?ev children

let text ?at ?ev children = make "text" ?at ?ev children

let tspan ?at ?ev children = make "tspan" ?at ?ev children

let text_path ?at ?ev children = make "textPath" ?at ?ev children

let marker ?at ?ev children = make "marker" ?at ?ev children

let pattern ?at ?ev children = make "pattern" ?at ?ev children

let clip_path ?at ?ev children = make "clipPath" ?at ?ev children

let mask ?at ?ev children = make "mask" ?at ?ev children

let linear_gradient ?at ?ev children = make "linearGradient" ?at ?ev children

let radial_gradient ?at ?ev children = make "radialGradient" ?at ?ev children

let stop ?at ?ev children = make "stop" ?at ?ev children

let filter ?at ?ev children = make "filter" ?at ?ev children

let fe_blend ?at ?ev children = make "feBlend" ?at ?ev children

let fe_color_matrix ?at ?ev children = make "feColorMatrix" ?at ?ev children

let fe_component_transfer ?at ?ev children =
  make "feComponentTransfer" ?at ?ev children

let fe_composite ?at ?ev children = make "feComposite" ?at ?ev children

let fe_convolve_matrix ?at ?ev children =
  make "feConvolveMatrix" ?at ?ev children

let fe_diffuse_lighting ?at ?ev children =
  make "feDiffuseLighting" ?at ?ev children

let fe_displacement_map ?at ?ev children =
  make "feDisplacementMap" ?at ?ev children

let fe_distant_light ?at ?ev children = make "feDistantLight" ?at ?ev children

let fe_drop_shadow ?at ?ev children = make "feDropShadow" ?at ?ev children

let fe_flood ?at ?ev children = make "feFlood" ?at ?ev children

let fe_func_a ?at ?ev children = make "feFuncA" ?at ?ev children

let fe_func_b ?at ?ev children = make "feFuncB" ?at ?ev children

let fe_func_g ?at ?ev children = make "feFuncG" ?at ?ev children

let fe_func_r ?at ?ev children = make "feFuncR" ?at ?ev children

let fe_gaussian_blur ?at ?ev children = make "feGaussianBlur" ?at ?ev children

let fe_image ?at ?ev children = make "feImage" ?at ?ev children

let fe_merge ?at ?ev children = make "feMerge" ?at ?ev children

let fe_merge_node ?at ?ev children = make "feMergeNode" ?at ?ev children

let fe_morphology ?at ?ev children = make "feMorphology" ?at ?ev children

let fe_offset ?at ?ev children = make "feOffset" ?at ?ev children

let fe_point_light ?at ?ev children = make "fePointLight" ?at ?ev children

let fe_specular_lighting ?at ?ev children =
  make "feSpecularLighting" ?at ?ev children

let fe_spot_light ?at ?ev children = make "feSpotLight" ?at ?ev children

let fe_tile ?at ?ev children = make "feTile" ?at ?ev children

let fe_turbulence ?at ?ev children = make "feTurbulence" ?at ?ev children

let foreign_object ?at ?ev children = make "foreignObject" ?at ?ev children

let view ?at ?ev children = make "view" ?at ?ev children
