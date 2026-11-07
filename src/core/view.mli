(** The virtual DOM tree (design section 08). The values are immutable. Later
    versions add nodes for components, boundaries and providers. *)

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

type 'msg t = Text of string | Element of 'msg element

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
