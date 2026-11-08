(** The virtual DOM tree (design section 08). The values are immutable. A later
    version adds nodes for providers. *)

(** An attribute or a controlled property. *)
type at =
  | Attr of {name: string; key: string; value: string}
      (** [key] is [name] in lowercase, for the checks. *)
  | Prop of Dom.prop
  | No_at

(** The payload of an event and the function that makes the message. *)
type 'msg decode = 'msg Dom.decode =
  | Decode : 'a Dom.payload * ('a -> 'msg) -> 'msg decode

type 'msg handler = 'msg Dom.event =
  { event: string  (** The DOM event name. *)
  ; bubbles: bool  (** [false]: a listener on the element ([Event.bubbles]). *)
  ; prevent_default: bool
  ; stop: bool
  ; decode: 'msg decode }

(** The identity of a component: a witness of its four types (design section
    09). [last] is the spec of the last call of [Component.v]: a cache with one
    entry. *)
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
        (** The usual spelling of the name, for example [div] and
            [foreignObject]. *)
  ; attrs: (string * string) list
        (** Each name one time, [type] first, the [class] values joined. *)
  ; value: string option
  ; checked: bool option
  ; events: 'msg handler list  (** Each DOM event name one time. *)
  ; children: 'msg children }

and 'msg children =
  | Plain of 'msg t list
  | Keyed of (Key.t * 'msg t) list
  | Inner of Unsafe_html.t

(** The functions of an instance: of a component, or of the App. Each function
    gets the props first. The App has the props [()]. *)
and ('p, 'm, 'msg, 'o) spec =
  { id: ('p, 'm, 'msg, 'o) id
  ; init: 'p -> 'm * 'msg Cmd.t
  ; update: ('msg, 'o) Cmd.ctx -> 'p -> 'm -> 'msg -> 'm * 'msg Cmd.t
  ; view: 'p -> 'm -> 'msg t
  ; subs: ('p -> 'm -> 'msg Sub.t) option  (** [None]: no subscription pass. *)
  ; equal_props: 'p -> 'p -> bool }

(** A component node: the spec, the [?key] of [Component.v], the function that
    maps an output to a message of the parent, and the props. The node has no
    DOM node of its own: its DOM node is the DOM node of the view of its
    instance. *)
and 'msg comp =
  | C :
      { spec: ('p, 'm, 'c, 'o) spec
      ; key: string option
      ; on: 'o -> 'msg
      ; props: 'p }
      -> 'msg comp

(** A boundary node (design section 09): the [?key] of [View.boundary], the
    fallback and the one child. The node has no DOM node of its own: its DOM
    node is the DOM node of its child, or of its fallback after an error. *)
and 'msg boundary = {key: string option; fallback: exn -> 'msg t; child: 'msg t}

val id : unit -> ('p, 'm, 'msg, 'o) id
(** A new identity, with an empty cache. *)

val element : string -> at list -> 'msg handler list -> 'msg children -> 'msg t
(** [element name at ev children] makes an element. It does not check [name]. It
    joins the values of the [class] attributes, in order, with one space, and it
    drops the empty values.

    @raise Invalid_argument
      for a duplicate attribute name (the case is ignored), a duplicate
      property, or two handlers for one DOM event name. *)

val txt : string -> 'msg t

val keyed : ('msg t list -> 'msg t) -> (Key.t * 'msg t) list -> 'msg t
(** See {!Kilim.View.keyed}. *)

val unsafe_html : el:('msg t list -> 'msg t) -> Unsafe_html.t -> 'msg t
(** See {!Kilim.View.unsafe_html}. *)
