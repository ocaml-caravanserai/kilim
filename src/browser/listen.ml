(* The listeners of one app (design section 11). The browser part is only
   bindings: it builds the Event.env of the app and the Event.event of each JS
   event, and calls Event.dispatch, which holds the rules. *)

module Dom = Kilim.Private.Dom
module Event = Kilim.Private.Event
module Runtime = Kilim.Private.Runtime

(* The state of one app. [live] is set after Runtime.create, before the
   listeners come: the listeners read the env and the runtime from it. *)
type state =
  { root: Jv.t  (** The mount element. *)
  ; mutable live: (Jv.t Event.env * Jv.t Runtime.t) option }

(* The page controls the values that kilim.browser reads, and under
   wasm_of_ocaml a cast of a value of the wrong JS type is a trap, not an OCaml
   exception. Thus each read examines the JS type first and has a safe
   default. *)

let node = lazy (Jv.get Jv.global "Node")

let radio_node_list = lazy (Jv.get Jv.global "RadioNodeList")

(* The cell is an OCaml value in the property [__kilim] of the element. Two apps
   on one page own disjoint elements, thus one name is enough. A control of a
   form with the name [__kilim] hides the property of the form: then the form
   has no cell. *)
let cell n : Dom.cell option =
  let c = Jv.get n "__kilim" in
  if
    Jv.is_undefined c
    || Jv.instanceof c ~cons:(Lazy.force node)
    || Jv.instanceof c ~cons:(Lazy.force radio_node_list)
  then None
  else Some (Jv.Id.of_jv c)

let truthy v = Jv.strict_equal v Jv.true'

let string_js = Jstr.v "string"

let is_string v = Jstr.equal (Jv.typeof v) string_js

(* A string payload: [""] for a value that is not a string, for example the
   [value] of an [li] (a number) or a missing [key]. *)
let string_or_empty v = if is_string v then Jv.to_string v else ""

(* The live [value] as the diff compares it: [String(v)] for a value that is not
   a string, for example the [value] of a [progress] (a number). *)
let value_string v =
  if is_string v then Jv.to_string v
  else if Jv.is_none v then ""
  else Jv.to_string (Jv.call Jv.global "String" [|v|])

(* The walk stops at the mount element. A listener above the app can remove an
   ancestor of the target before the dispatch reaches the app: then the walk
   stops at the top of the removed part. *)
let parent st n =
  if Js.same_node n st.root then None
  else
    let p = Jv.get n "parentNode" in
    if Jv.is_null p then None else Some p

(* The nodes of an app are under its mount element, which is in the document:
   thus [isConnected] is enough. A removed element keeps its cell and its direct
   listeners. *)
let connected n = truthy (Jv.get n "isConnected")

let input = lazy (Jv.get Jv.global "HTMLInputElement")

let checkbox_js = Jv.of_string "checkbox"

let radio_js = Jv.of_string "radio"

let empty_js = Jv.of_string ""

let radios_js = Jv.of_string "input[type=radio]"

(* An HTML input with the type [kind], a JS string. The [type] property is in
   lowercase. *)
let is_input n kind =
  Jv.instanceof n ~cons:(Lazy.force input)
  && Jv.strict_equal (Jv.get n "type") kind

let toggles n = is_input n checkbox_js || is_input n radio_js

(* The buttons of the radio group of [n]: the radio buttons of the app with the
   same name and the same form owner ([null] for no form owner). A button with
   no name has no group. The query is one literal selector, thus a name never
   goes into a selector, and [form.elements] is not read: a control with the
   name [elements] hides it. The names compare as JS strings. A button of the
   group outside the app has no cell, thus the restore does not change it. The
   list contains [n]: a second restore of [n] writes nothing. *)
let group st n =
  if not (is_input n radio_js) then []
  else
    let name = Jv.get n "name" in
    if Jv.strict_equal name empty_js then []
    else
      let form = Jv.get n "form" in
      let all = Jv.call st.root "querySelectorAll" [|radios_js|] in
      let same m =
        Jv.strict_equal (Jv.get m "name") name
        && Jv.strict_equal (Jv.get m "form") form
      in
      let rec go i acc =
        if i < 0 then acc
        else
          let m = Jv.Jarray.get all i in
          go (i - 1) (if same m then m :: acc else acc)
      in
      go (Jv.to_int (Jv.get all "length") - 1) []

let env st ops =
  { Event.ops
  ; parent= parent st
  ; cell
  ; connected
  ; group= group st
  ; toggles
  ; post_task= Browser_host.host.post_task }

(* One JS event: the payload comes from the target, as Event.dispatch reads
   it. *)
let handle st name target ev =
  match st.live with
  | Some (env, rt) ->
      let read : type a. a Dom.payload -> a = function
        | Dom.Unit ->
            ()
        | Dom.String ->
            string_or_empty (Jv.get target "value")
        | Dom.Key ->
            string_or_empty (Jv.get ev "key")
        | Dom.Bool ->
            truthy (Jv.get target "checked")
      in
      ignore
        (Event.dispatch env rt
           { name
           ; target
           ; composing= truthy (Jv.get ev "isComposing")
           ; read
           ; prevent= (fun () -> ignore (Jv.call ev "preventDefault" [||]))
           ; stop= (fun () -> ignore (Jv.call ev "stopPropagation" [||])) } )
  | None ->
      (* This does not occur: start sets [live] before it adds the listeners. *)
      ()

(* A delegated listener: one on the mount element for each name, with its name
   in the closure. *)
let delegated st name =
  Jv.callback ~arity:1 (fun ev -> handle st name (Jv.get ev "target") ev)

(* A direct listener: the element of the listener is the target. *)
let direct st name =
  Jv.callback ~arity:1 (fun ev -> handle st name (Jv.get ev "currentTarget") ev)
