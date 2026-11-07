(** The DOM operations that the diff calls directly, with no patch list (design
    section 11), and the cell of each element. kilim.browser gives these
    operations on the real DOM, and kilim.testing gives a fake DOM. *)

type ns = Html | Svg  (** The namespace of an element. *)

(** A controlled property: the diff writes it as a property, not as an
    attribute. *)
type prop = Value of string | Checked of bool

(** What the listener reads from the event for the handler. *)
type _ payload =
  | Unit : unit payload  (** Nothing. *)
  | String : string payload  (** The [value] of the target. *)
  | Key : string payload  (** The [key] of a keyboard event. *)
  | Bool : bool payload
      (** The [checked] of the target, read before the restore. *)

(** The payload of an event and the function that makes the message. *)
type 'msg decode = Decode : 'a payload * ('a -> 'msg) -> 'msg decode

(** An event handler of a view. {!View.handler} is this type. *)
type 'msg event =
  { event: string  (** The DOM event name. *)
  ; bubbles: bool
        (** [false] for an event for which a listener on its element is
            necessary. *)
  ; prevent_default: bool
  ; stop: bool
  ; decode: 'msg decode }

(** An event handler, bound to the dispatch of the runtime. The listener applies
    the static options synchronously, then calls [fire]. *)
type handler =
  | Handler :
      {payload: 'a payload; prevent_default: bool; stop: bool; fire: 'a -> unit}
      -> handler

(** The event handlers of the view of an element and the dispatch, with no
    ['msg] in the cell. Each DOM event name occurs one time. *)
type events = Events : 'msg event list * ('msg -> unit) -> events

(** The cell of an element. The diff makes one cell for each element and
    attaches it when it creates the element. A patch changes the cell with no
    DOM operation. *)
type cell =
  { mutable events: events
  ; mutable value: string option
        (** The value of the [value] property in the last view. *)
  ; mutable checked: bool option
        (** The value of the [checked] property in the last view. *) }

val handler : cell -> string -> handler option
(** [handler cell name] is the handler for the DOM event name [name], for
    example [focusin] for {!Kilim.Ev.focus}. It makes the handler at each call:
    call it when the event occurs. *)

val handlers : cell -> (string * handler) list
(** All the handlers of the cell, with their DOM event names, for the tests. *)

type 'node t =
  { create_element: ns -> string -> 'node
  ; create_text: string -> 'node
  ; set_text: 'node -> string -> unit
  ; set_attribute: 'node -> string -> string -> unit
  ; remove_attribute: 'node -> string -> unit
  ; set_property: 'node -> prop -> unit
  ; get_value: 'node -> string
        (** The live [value] property. The browser can change it with no write
            of the diff, thus the diff compares with it. *)
  ; get_checked: 'node -> bool  (** The live [checked] property. *)
  ; insert_before: 'node -> 'node -> 'node option -> unit
        (** [insert_before parent node ref] inserts or moves [node] before the
            child [ref] of [parent], or at the end with [None]. *)
  ; remove: 'node -> 'node -> unit  (** [remove parent node]. *)
  ; replace: 'node -> 'node -> 'node -> unit
        (** [replace parent node old] puts [node] in the place of the child
            [old]. *)
  ; clear: 'node -> unit  (** Removes all children. *)
  ; set_inner_html: 'node -> Unsafe_html.t -> unit
        (** Writes the native value if it exists, or else the string. *)
  ; attach: 'node -> cell -> unit
  ; listen: 'node -> string -> passive:bool -> unit
        (** A listener on the element, only for an event that does not bubble
            ([mouseenter], [mouseleave] and [scroll]). The other events use
            delegation at the root, which reads the cells. *)
  ; unlisten: 'node -> string -> unit }
