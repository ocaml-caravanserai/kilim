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

    - A component node has no DOM node of its own. A component node with the
      same id and the same [?key] at the same place keeps its instance. If not,
      the diff builds the new node, puts it in the place of the old node, then
      unmounts the old instance. When the diff removes or replaces a subtree, it
      first does the DOM operation, then unmounts each instance of the subtree
      (the removal walk). If the owner of the view has no child instance and no
      boundary, the view has no component node and no boundary node, and the
      diff does no walk.

    - A boundary node has no DOM node of its own. A boundary node with the same
      [?key] at the same place keeps its state. If not, the diff builds the new
      node, puts it in the place of the old node, then unmounts each instance of
      the old node and unlinks each old boundary from its lists. The diff builds
      and patches the child of a boundary in [try]: an exception goes to the
      boundary, which shows its fallback (see {!Boundary}). The diff writes a
      controlled property after the diff: an exception of an element in a
      boundary goes to the nearest boundary of the element, as a catch outside
      the diff, in the same render, and the other elements get their writes. An
      element whose boundary failed or left the tree gets no write.

    If a DOM operation raises, the diff stops and gives the exception. Each node
    of the retained tree then is a child of the DOM node of its retained parent,
    and each DOM child of a retained element is in the retained tree. The order
    of the children can be different from the DOM. *)

(** A node of the retained tree of the view of an instance. ['msg] is the
    message type of that instance. A component node has no DOM node of its own
    (see {!dom}). *)
type ('node, 'msg) node = ('node, 'msg) Tree.node

type ('node, 'msg) ctx = ('node, 'msg) Tree.ctx

(** The retained tree of one root: the context of the diff, the DOM parent and
    the root node. The view of a child instance has the same record. *)
type ('node, 'msg) t = ('node, 'msg) Tree.mount =
  {ctx: ('node, 'msg) ctx; parent: 'node; root: ('node, 'msg) node}

(** The functions of the component nodes. [build ctx parent c] builds the node
    of [c] as a child of the DOM node [parent]. [patch ctx parent r c] patches
    the component node [r] to [c]. [render i] renders the live instance [i]
    alone. [walk env r] and [walk_children env c] unmount each instance of a
    removed subtree (the removal walk). [flush env catch] renders each instance
    of the work queue alone and writes the fallback of each boundary of the work
    queue, from the top. It gives each error to [catch], with the boundary of
    the error ([None] for the root catch), and tells if it wrote one. *)
type hooks =
  { build:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> 'msg View.comp -> ('node, 'msg) node
  ; patch:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> ('node, 'msg) node -> 'msg View.comp -> unit
  ; render: 'node 'msg. ('node, 'msg) Tree.inst -> unit
  ; walk: 'node 'msg. 'node Tree.env -> ('node, 'msg) node -> unit
  ; walk_children:
      'node 'msg. 'node Tree.env -> ('node, 'msg) Tree.children -> unit
  ; flush:
      'node.
      'node Tree.env -> ('node Tree.any_boundary option -> exn -> unit) -> bool
  }

val hooks : hooks ref
(** The functions of the component nodes. [Component.v] installs them: thus an
    app with no component does not link them. The default [build], [patch] and
    [render] raise [Invalid_argument]. The default [walk], [walk_children] and
    [flush] do nothing: an app with no component has no instance below its root.
*)

(** The functions of the boundary nodes. [build_boundary] and [patch_boundary]
    are as [build] and [patch] of {!hooks}. [show env b e] writes the fallback
    of the boundary [b], which failed with [e], in the place of its child, and
    removes [b] from the work queue. [fail env b e] is the catch outside the
    diff: [b] unmounts its subtree, keeps [e], and goes into the work queue:
    thus a render is pending, and the render writes the fallback.
    [write env b dom node cell] writes the controlled properties of an element
    in [b] after the diff (see {!run}). *)
type bounds =
  { build_boundary:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> 'msg View.boundary -> ('node, 'msg) node
  ; patch_boundary:
      'node 'msg.
         ('node, 'msg) ctx
      -> 'node
      -> ('node, 'msg) node
      -> 'msg View.boundary
      -> unit
  ; show:
      'node 'msg. 'node Tree.env -> ('node, 'msg) Tree.boundary -> exn -> unit
  ; fail: 'node. 'node Tree.env -> 'node Tree.any_boundary -> exn -> unit
  ; write:
      'node.
         'node Tree.env
      -> 'node Tree.any_boundary
      -> 'node Dom.t
      -> 'node
      -> Dom.cell
      -> unit }

val bounds : bounds ref
(** The functions of the boundary nodes. [View.boundary] installs them: thus an
    app with no boundary does not link them. The default [build_boundary] and
    [patch_boundary] raise [Invalid_argument]. The default [show], [fail] and
    [write] do nothing: an app with no boundary has no instance and no element
    with a boundary. *)

(** The functions of the providers and the reads (design section 10).
    [build_provider] and [patch_provider] are as [build] and [patch] of
    {!hooks}. [read ctx r] registers the owner of [ctx] with the nearest
    provider of the context of [r] in the providers of [ctx], and gives the view
    of [r] for the value of that provider, or for the default value. The diff
    puts that view in the place of [r]. *)
type contexts =
  { build_provider:
      'node 'msg.
      ('node, 'msg) ctx -> 'node -> 'msg View.provide -> ('node, 'msg) node
  ; patch_provider:
      'node 'msg.
         ('node, 'msg) ctx
      -> 'node
      -> ('node, 'msg) node
      -> 'msg View.provide
      -> unit
  ; read: 'node 'msg. ('node, 'msg) ctx -> 'msg View.read -> 'msg View.t }

val contexts : contexts ref
(** The functions of the providers and the reads. [Context.provide] and
    [Context.read] install them: thus an app with no context does not link them.
    The defaults raise [Invalid_argument]. *)

val start : ('node, 'msg) ctx -> parent:'node -> 'msg View.t -> ('node, 'msg) t
(** [start ctx ~parent view] builds the DOM of [view] with [ctx] and appends it
    to [parent]. The owner of [ctx] is the instance of [view]. *)

val mount :
     'node Dom.t
  -> parent:'node
  -> post:'msg Dom.post
  -> 'msg View.t
  -> ('node, 'msg) t
(** [mount dom ~parent ~post view] is {!start} with no runtime: a new owner
    instance, and an env where the messages of a component go nowhere and its
    commands start no task. The handlers of the cells call [post] with the
    payload and the message function. *)

val patch : ('node, 'msg) t -> 'msg View.t -> unit
(** [patch t view] removes the registrations of the owner of [t] (see
    {!Instance.forget}), then changes the DOM and the retained tree of [t] to
    [view]. *)

val build : ('node, 'msg) ctx -> 'node -> 'msg View.t -> ('node, 'msg) node
(** [build ctx parent view] builds the DOM of [view], for a child of the DOM
    node [parent]. The caller inserts it. *)

val patch_node :
  ('node, 'msg) ctx -> 'node -> ('node, 'msg) node -> 'msg View.t -> unit
(** [patch_node ctx parent r view] changes [r], a child of the DOM node
    [parent], to [view]. *)

val replace :
  ('node, 'msg) ctx -> 'node -> ('node, 'msg) node -> 'msg View.t -> unit
(** [replace ctx parent r view] builds [view], puts its DOM node in the place of
    the DOM node of [r], changes [r] in place, and then unmounts each instance
    of the old subtree. *)

val run : ('node, 'msg) ctx -> (unit -> 'a) -> 'a
(** [run ctx f] runs [f ()], then writes the controlled properties of the
    elements that [f] built or patched, in the order of their end tags. *)

val render : ('node, 'msg) Tree.inst -> unit
(** [render i] is [!hooks.render i]. *)

val restore : 'node Dom.t -> 'node -> Dom.cell -> unit
(** [restore dom node cell] writes each view value of [cell] that is different
    from the live property of [node], as a patch does. After a discrete event,
    the platform calls it for the target of the event (design section 11): thus
    a rejected input does not stay. *)

val root : ('node, 'msg) t -> ('node, 'msg) node

val dom : ('node, 'msg) node -> 'node
(** The DOM node of a retained node. A component node is transparent: its DOM
    node is the DOM node of the root of the view of its instance, also after a
    render of the instance alone that replaced that root. A boundary node is
    transparent: its DOM node is the DOM node of its child or of its fallback. A
    provider node is transparent: its DOM node is the DOM node of its child. *)

val probes : Key.t array -> Key.t array -> int option
(** [probes nk ok] makes the plan of a keyed patch from the old keys [ok] to the
    new keys [nk]. The result is the number of slots that the plan read in the
    table, or [None] if the plan went above its budget and used a map. With no
    common start and end, the plan adds each key of [nk] to the table, then
    finds each key of [ok]. For the tests.

    @raise Invalid_argument for a duplicate key in [nk]. *)

(** A retained node, with its message type hidden. *)
type 'node child = Child : ('node, 'msg) node -> 'node child

val children : ('node, 'msg) node -> 'node child list option
(** The retained children of an element, or [None] for a text node and for an
    element with {!View.unsafe_html} content. A component node, a boundary node
    and a provider node are transparent: the result is the children of the root
    of the view of the instance, or of the child of the boundary or of the
    provider. *)
