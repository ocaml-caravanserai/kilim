(* The attributes (design sections 08 and 13). The signature and its text are in
   kilim.mli. *)

type t = View.at

(* An attribute with a static name in lowercase. *)
let attr name value = View.Attr {name; key= name; value}

let v name value =
  let key = String.lowercase_ascii name in
  if not (Names.attribute_ok key) then
    invalid_arg ("kilim: At.v: a refused name: " ^ name) ;
  if not (Url.attribute_ok key value) then
    invalid_arg ("kilim: At.v: a refused URL in " ^ name) ;
  View.Attr {name; key; value}

let none = View.No_at

let bool name = v name ""

let prefixed prefix name value =
  if String.equal name "" then
    invalid_arg ("kilim: At: an empty name after " ^ prefix) ;
  v (prefix ^ name) value

let data name value = prefixed "data-" name value

let aria name value = prefixed "aria-" name value

let class_ c = attr "class" c

let classes l =
  attr "class"
    (String.concat " " (List.filter (fun c -> not (String.equal c "")) l))

let id s = attr "id" s

let href s = v "href" s

let src s = v "src" s

let action s = v "action" s

let method_ s = attr "method" s

let name s = attr "name" s

let type_ s = attr "type" s

let for_ s = attr "for" s

let alt s = attr "alt" s

let title s = attr "title" s

let lang s = attr "lang" s

let rel s = attr "rel" s

let target s = attr "target" s

let placeholder s = attr "placeholder" s

let style s = attr "style" s

let role s = attr "role" s

let aria_label s = attr "aria-label" s

let tabindex i = attr "tabindex" (string_of_int i)

let width s = attr "width" s

let height s = attr "height" s

let disabled = attr "disabled" ""

let hidden = attr "hidden" ""

let required = attr "required" ""

let readonly = attr "readonly" ""

let multiple = attr "multiple" ""

let autofocus = attr "autofocus" ""

let selected = attr "selected" ""

let value s = View.Prop (Dom.Value s)

let checked b = View.Prop (Dom.Checked b)
