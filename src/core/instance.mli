(** The instance record (design section 09). In Phase 1 the App is the only
    instance. The turn uses only these operations on an instance: take a message
    ([update]), render ([view]), catch an error, and get the token. Phase 2 adds
    the instance tree, and does not change the turn. *)

(** The tokens of [Cmd.latest], by key path. *)
module Latest : Map.S with type key = string list

type ('model, 'msg) spec =
  {update: 'model -> 'msg -> 'model * 'msg Cmd.t; view: 'model -> 'msg View.t}

type 'msg state =
  | State : {spec: ('model, 'msg) spec; mutable model: 'model} -> 'msg state

type 'msg t =
  { base: Cancel.t  (** The runtime token: the parent of the instance token. *)
  ; mutable alive: bool
  ; mutable token: Cancel.t option  (** Made at the first task. *)
  ; mutable latest: Cancel.t Latest.t
  ; mutable dirty: bool  (** The model changed since the last render. *)
  ; mutable state: 'msg state option  (** [None] after the unmount. *) }

val make : Cancel.t -> 'msg t
(** [make base] is a live instance with no state, no token and no key. *)

val token : 'msg t -> Cancel.t
(** The token of the instance: [Cancel.child base] at the first call. The
    runtime calls it only for a live instance. *)

val unmount : 'msg t -> unit
(** Clears [alive] and [dirty], cancels the token, empties [latest], and sets
    [state] to [None]. The runtime never makes a dead instance dirty again. Thus
    after a root catch, the write of the fallback is the last render. *)
