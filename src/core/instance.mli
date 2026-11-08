(** The instance tree (design section 09). The App is the root instance, and
    each component node of a view has one instance. The turn uses these
    operations on an instance: take a message ([update]), render ([view]),
    compare the subscriptions ([subs]), catch an error, get the token, and
    unmount. *)

(** The tokens of [Cmd.latest], by key path. *)
module Latest = Tree.Latest

(** The recorded subscriptions, by key. *)
module Subs = Tree.Subs

(** The work queue of a render, by depth, then by the order of the mount. *)
module Work = Tree.Work

(** A started subscription of an auto kind. Its token is a child of the token of
    the instance. *)
type 'msg auto = 'msg Tree.auto =
  | Auto :
      {token: Cancel.t; kind: 'p Sub.kind; on: ('p -> 'msg) ref}
      -> 'msg auto
      (** [on] is the newest message function: a pass of a key that stays
          changes it, and a payload takes it when it leaves the queue. *)

(** The state of an instance. The component node of a child keeps the same
    record. *)
type ('p, 'm, 'msg, 'o) data = ('p, 'm, 'msg, 'o) Tree.data =
  { mutable spec: ('p, 'm, 'msg, 'o) View.spec
        (** The newest spec: two modules can share one id. *)
  ; mutable props: 'p
        (** The props of the last patch where [equal_props] gave [false]. *)
  ; mutable model: 'm }

type 'msg state = 'msg Tree.state =
  | State : ('p, 'm, 'msg, 'o) data -> 'msg state

(** An instance. The fields (see {!Tree.inst}):
    - [base]: the runtime token, the parent of the token of the root.
    - [pos]: the depth (0 for the root) and the serial (the order of the mount).
    - [parent], [children] and [sibling]: the links of the tree. A child links
      into the child list of its parent before its [init] runs.
    - [mount]: the context of the diff, the DOM parent and the root of the
      retained tree of the view of a child. The root has [None]: the runtime
      keeps its retained tree.
    - [alive], [token] (made at the first task), [latest], [dirty] (the model
      changed since the last render), [runs] and [autos] (the recorded
      subscription keys), [early_stale] (the model changed since the last
      subscription pass) and [state] ([None] after the unmount). *)
type ('node, 'msg) t = ('node, 'msg) Tree.inst

type 'node any = 'node Tree.any = Any : ('node, 'msg) t -> 'node any

type 'node env = 'node Tree.env

(** The parts of the instance operations that only components need.
    [Component.v] installs them (see {!Reconcile}): [detach env i] removes [i]
    from the work queue and from the child list of its parent, clears [parent]
    and [mount], and unmounts each child. [enqueue env i] puts [i] in the work
    queue. The default does nothing: an app with no component has only the root.
*)
type ext =
  { detach: 'node 'msg. 'node env -> ('node, 'msg) t -> unit
  ; enqueue: 'node 'msg. 'node env -> ('node, 'msg) t -> unit }

val ext : ext ref

val root : Cancel.t -> ('node, 'msg) t
(** [root base] is a live root instance with no state, no token and no key, at
    depth 0. *)

val child : 'node env -> ('node, 'p) t -> ('node, 'msg) t
(** [child env p] is a new instance under [p], with the next serial of [env]. If
    [p] is alive, the instance is alive and first in the child list of [p]. If
    not (a component in the root fallback), the instance is dead and in no list:
    its commands do not run and its messages are dropped, but it renders one
    time as a part of the fallback. *)

val token : ('node, 'msg) t -> Cancel.t
(** The token of the instance, made at the first call. If an ancestor has no
    token yet, the ancestors get their tokens first, from the top: thus the
    token of a child is always under the token of its parent. The token of the
    root is a child of [base]. A dead instance gets a cancelled token. *)

val changed : 'node env -> ('node, 'msg) t -> unit
(** The model changed: sets [dirty] and [early_stale]. An instance below the
    root goes into the work queue of [env] (see {!ext}). The runtime calls it
    only for a live instance. *)

val clean : 'node env -> ('node, 'msg) t -> unit
(** Clears [dirty], and removes the instance from the work queue. *)

val unmount : 'node env -> ('node, 'msg) t -> unit
(** Clears [alive] and [dirty], calls [detach] of {!ext} (thus leaves the work
    queue and the child list of its parent, clears [parent] and [mount], and
    unmounts each child, also for a dead instance), cancels the token (thus each
    subscription stops), cancels the token of each recorded key (also of a task
    that ended), empties [latest], [runs] and [autos], and sets [state] to
    [None]. Thus a dead instance keeps no model, no DOM node and no child. A
    subscription pass does nothing for an instance with no state, thus
    [early_stale] does not matter after the unmount. The runtime never makes a
    dead instance dirty again. Thus after a root catch, the write of the
    fallback is the last render of the root. A second call does nothing. *)
