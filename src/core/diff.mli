(** The diff and its retained tree (design sections 09 and 11). The diff
    compares a new view with the retained tree of the last view, and it calls
    the DOM operations directly, with no patch list.

    The rules:
    - A node with the same kind, the same element name and the same namespace at
      the same place keeps its DOM node. If not, the diff replaces the node.
    - Plain children compare by position. Keyed children compare by key, with
      the algorithm of ivi and Inferno: the common start and end, a hash table
      of the new middle, and moves along the longest increasing subsequence. The
      table has a budget for each patch: the patch can read 8 slots of the table
      for each key that it adds to the table or finds in it. With usual keys,
      the patch reads about 2 slots for each key. Only the total is important,
      not the length of one search. Many keys that collide, by chance or by
      intent, go above the budget. Then the diff uses a map for this patch. Thus
      a keyed patch costs O(n) or O(n log n), not O(n^2).
    - A duplicate key in a keyed list raises [Invalid_argument] before the diff
      writes the DOM of the element of the list.
    - The browser can change [value] and [checked] with no write of the diff:
      the user types, a radio button unchecks the other buttons of its group, a
      select selects an option, a range input clamps its value. Thus the diff
      compares the view value with the live DOM property, as React does, and
      writes only a different value. The cell keeps the view value.
    - While the cell of an element is composing (an IME composition) and the
      element has the focus, the diff and {!restore} do not write [value],
      because a write stops the composition. The cell keeps the new view value,
      and the restore at the end of the composition writes it. A composition
      cannot go on in an element with no focus: a write site that finds the flag
      on an element with no focus clears the flag and writes.
    - The namespace of an element comes from its parent. [svg] starts the SVG
      namespace, and the children of [foreignObject] are HTML.
    - For an element, the order is: the attributes ([type] first), the
      listeners, the children, then the properties. The diff writes the
      properties after all the other operations of the mount or the patch,
      because a later change of a name, a type or a form can uncheck a radio
      button. A mount and a patch write the properties in the same order: the
      order of the end tags of the elements in the document. Thus two checked
      radio buttons in one group give the same result.

    If a DOM operation raises, the diff stops and gives the exception. Each node
    of the retained tree then is a child of the DOM node of its retained parent,
    and each DOM child of a retained element is in the retained tree. The order
    of the children can be different from the DOM. *)

(** A node of the retained tree. *)
type 'node node

(** The retained tree of one root. *)
type ('node, 'msg) t

val mount :
     'node Dom.t
  -> parent:'node
  -> post:'msg Dom.post
  -> 'msg View.t
  -> ('node, 'msg) t
(** [mount dom ~parent ~post view] builds the DOM of [view] and appends it to
    [parent]. The handlers of the cells call [post] with the payload and the
    message function. *)

val patch : ('node, 'msg) t -> 'msg View.t -> unit
(** [patch t view] changes the DOM and the retained tree of [t] to [view]. *)

val restore : 'node Dom.t -> 'node -> Dom.cell -> unit
(** [restore dom node cell] writes each view value of [cell] that is different
    from the live property of [node], as a patch does. After a discrete event,
    the platform calls it for the target of the event (design section 11): thus
    a rejected input does not stay. *)

val root : ('node, 'msg) t -> 'node node

val dom : 'node node -> 'node
(** The DOM node of a retained node. *)

val probes : Key.t array -> Key.t array -> int option
(** [probes nk ok] makes the plan of a keyed patch from the old keys [ok] to the
    new keys [nk]. The result is the number of slots that the plan read in the
    table, or [None] if the plan went above its budget and used a map. With no
    common start and end, the plan adds each key of [nk] to the table, then
    finds each key of [ok]. For the tests.

    @raise Invalid_argument for a duplicate key in [nk]. *)

val children : 'node node -> 'node node list option
(** The retained children of an element, or [None] for a text node and for an
    element with {!View.unsafe_html} content. *)
