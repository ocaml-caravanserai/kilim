type at =
  | Attr of {name: string; key: string; value: string}
  | Prop of Dom.prop
  | No_at

type 'msg decode = 'msg Dom.decode =
  | Decode : 'a Dom.payload * ('a -> 'msg) -> 'msg decode

type 'msg handler = 'msg Dom.event =
  { event: string
  ; bubbles: bool
  ; prevent_default: bool
  ; stop: bool
  ; decode: 'msg decode }

type ('p, 'm, 'msg, 'o) id =
  { tid: ('p * 'm * 'msg * 'o) Type.Id.t
  ; mutable last: ('p, 'm, 'msg, 'o) spec option }

and 'msg t =
  | Text of string
  | Element of 'msg element
  | Comp of 'msg comp
  | Boundary of 'msg boundary

and 'msg element =
  { name: string
  ; attrs: (string * string) list
  ; value: string option
  ; checked: bool option
  ; events: 'msg handler list
  ; children: 'msg children }

and 'msg children =
  | Plain of 'msg t list
  | Keyed of (Key.t * 'msg t) list
  | Inner of Unsafe_html.t

and ('p, 'm, 'msg, 'o) spec =
  { id: ('p, 'm, 'msg, 'o) id
  ; init: 'p -> 'm * 'msg Cmd.t
  ; update: ('msg, 'o) Cmd.ctx -> 'p -> 'm -> 'msg -> 'm * 'msg Cmd.t
  ; view: 'p -> 'm -> 'msg t
  ; subs: ('p -> 'm -> 'msg Sub.t) option
  ; equal_props: 'p -> 'p -> bool }

and 'msg comp =
  | C :
      { spec: ('p, 'm, 'c, 'o) spec
      ; key: string option
      ; on: 'o -> 'msg
      ; props: 'p }
      -> 'msg comp

and 'msg boundary = {key: string option; fallback: exn -> 'msg t; child: 'msg t}

let id () = {tid= Type.Id.make (); last= None}

let duplicate what name =
  invalid_arg ("kilim: a duplicate " ^ what ^ ": " ^ name)

let rec check_events = function
  | [] ->
      ()
  | h :: rest ->
      if List.exists (fun h' -> String.equal h'.event h.event) rest then
        duplicate "handler" h.event ;
      check_events rest

let element name ats events children =
  let seen = ref []
  and first = ref []
  and attrs = ref []
  and classes = ref [] in
  let value = ref None and checked = ref None in
  List.iter
    (function
      | No_at ->
          ()
      | Prop (Dom.Value v) ->
          if Option.is_some !value then duplicate "property" "value" ;
          value := Some v
      | Prop (Dom.Checked b) ->
          if Option.is_some !checked then duplicate "property" "checked" ;
          checked := Some b
      | Attr a ->
          if String.equal a.key "class" then
            begin if not (String.equal a.value "") then
              classes := a.value :: !classes
            end
          else begin
            if List.exists (String.equal a.key) !seen then
              duplicate "attribute" a.name ;
            seen := a.key :: !seen ;
            if String.equal a.key "type" then first := [(a.name, a.value)]
            else attrs := (a.name, a.value) :: !attrs
          end )
    ats ;
  let attrs =
    match !classes with
    | [] ->
        !attrs
    | l ->
        ("class", String.concat " " (List.rev l)) :: !attrs
  in
  check_events events ;
  Element
    { name
    ; attrs= !first @ List.rev attrs
    ; value= !value
    ; checked= !checked
    ; events
    ; children }

let txt s = Text s

let with_children what el children =
  match el [] with
  | Element e when not (Names.is_void e.name) ->
      Element {e with children}
  | _ ->
      invalid_arg
        ("kilim: View." ^ what ^ ": el must give an element that is not void")

let keyed el l = with_children "keyed" el (Keyed l)

let unsafe_html ~el h = with_children "unsafe_html" el (Inner h)
