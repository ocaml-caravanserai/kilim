(** A strict fake DOM in pure OCaml (design section 14). It gives the DOM
    operations of {!Kilim.Private.Dom} and counts each operation.

    The fake DOM is strict: an operation that the real DOM permits, but that the
    diff must not do, raises {!Misuse}. For example, for [insert_before], a
    reference that is a child of the parent is necessary, and a node can move
    only inside its parent.

    The fake DOM has a model of the state that the browser changes with no
    operation of the diff:
    - The [value] property of an element is the last value that the diff or
      {!browser_set} wrote. With no write, it is the [value] attribute, else the
      text of an [option] or a [textarea], else [""].
    - A [select] element has options: its [option] children and the [option]
      children of its [optgroup] children. Each option keeps its selectedness
      when it moves. Its [value] property is the value of its first selected
      option, or [""]. A write of the property selects the first option with
      this value, and no other option. A drop-down select has no [multiple]
      attribute and no [size] above 1. If no option of a drop-down select is
      selected after a change of the options, or of the attribute [size] or
      [multiple], the first option becomes selected. The model ignores the
      [selected] and [disabled] attributes.
    - An [input] element with the attribute [type="range"] keeps its [value]
      property between the attributes [min] and [max] (by default 0 and 100). A
      value that is not a number becomes the middle. The model does this after
      each write of the property or of an attribute. It ignores [step].
    - The radio buttons ([input] with [type="radio"]) with the same non-empty
      [name], in the same [form] ancestor, or in the same tree with no [form]
      ancestor, are a group. When a radio button becomes checked, or when a
      checked radio button joins a group (a change of an attribute, an insert or
      a move), the other buttons of its group become unchecked. The [checked]
      property is [false] until a write. The model ignores the [checked]
      attribute and the [form] attribute.
    - As in a browser, the case of the [type] attribute is not important:
      [type="RANGE"] is a range input.
    - The document has one focused element or none. Only {!focus} gives the
      focus. An [insert_before], [remove] or [replace] of the focused element or
      of one of its ancestors takes the focus away, as in a browser. A [clear]
      or [set_inner_html] of an ancestor does the same. The fake DOM sends no
      event. *)

(** A node: an element, a text node, or the HTML of [set_inner_html]. *)
type node

(** A document: the counts of the operations and the fault injection. *)
type t

(** An operation that the diff must not do. *)
exception Misuse of string

(** The fault of {!fail_at}. *)
exception Fault

val create : unit -> t

val ops : t -> node Kilim.Private.Dom.t
(** The DOM operations on the nodes of [t]. *)

val element : string -> node
(** [element name] is a new HTML element with no operation counted, for example
    a mount root. *)

(** {1 Counts and faults} *)

val total : t -> int
(** The number of operations. *)

val counts : t -> (string * int) list
(** The number of each operation that occurred, sorted by name. The names are
    those of the fields of {!Kilim.Private.Dom.t}. *)

val moves : t -> int
(** The number of [insert_before] operations on a node that was already in its
    parent. {!counts} also counts them as [insert_before]. *)

val reset : t -> unit
(** Set all counts to 0. *)

val fail_at : t -> int -> unit
(** [fail_at t k] makes operation number [k] from now raise {!Fault}, with no
    effect. [k] starts at 1. The operation after it works again. *)

(** {1 The focus} *)

val focus : t -> node option -> unit
(** [focus t n] gives the focus to the element [n], or to no element with
    [None], with no count and no event. *)

val focused : t -> node option
(** The focused element, with no count. *)

(** {1 The tree} *)

val parent : node -> node option

val children : node -> node list

val name : node -> string
(** The name of an element, ["#text"] for a text node, or ["#html"] for the HTML
    of [set_inner_html]. *)

val ns : node -> Kilim.Private.Dom.ns option
(** The namespace of an element. *)

val text : node -> string
(** The text of a text node or of the HTML of [set_inner_html], or the text of
    all text nodes inside an element. *)

val attribute : node -> string -> string option

val value : node -> string
(** The live [value] property (see the model above), with no count. *)

val checked : node -> bool
(** The live [checked] property, with no count. *)

val group : node -> node list
(** The other radio buttons of the group of a radio button (see the model
    above), in the order of the document, or [[]]. *)

val browser_set : node -> Kilim.Private.Dom.prop -> unit
(** [browser_set node prop] writes a property as the browser does, for example
    when the user types: no count and no fault. The model above applies.

    @raise Misuse if [node] is not an element. *)

val cell : node -> Kilim.Private.Dom.cell option

val listeners : node -> (string * bool) list
(** The listeners of the element, with their passive flag, sorted. *)

val fire : node -> string -> 'a Kilim.Private.Dom.payload -> 'a -> unit
(** [fire node event payload x] calls the handler for [event] in the cell of
    [node] with [x].

    @raise Misuse if no handler exists, or if its payload is different. *)

val to_html : node -> string
(** The HTML of the node, with the attributes sorted by name. *)

val dump : node -> string
(** As {!to_html}, but with all the state that the diff writes: the namespace
    ([svg:] before an SVG name), the properties ([.value] as {!value} gives it,
    for a [select] and for a value that is not the value with no write, and
    [.checked] if it is [true]), the listeners ([@] before the event name, [~]
    after a passive one), and the handlers of the cell. A handler is [!], the
    event name, the suffix of its payload ([/value], [/key] or [/checked], or
    none for no payload), then [+p] for prevent_default and [+s] for stop. A
    text node is in double quotes. *)
