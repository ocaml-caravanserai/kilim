(** The instance record (design section 09). In Phase 1 the App is the only
    instance. The turn uses only these operations on an instance: take a message
    ([update]), render ([view]), compare the subscriptions ([subs]), catch an
    error, and get the token. Phase 2 adds the instance tree, and does not
    change the turn. *)

(** The tokens of [Cmd.latest], by key path. *)
module Latest : Map.S with type key = string list

(** The recorded subscriptions, by key. *)
module Subs : Map.S with type key = Sub.Key.t

(** A started subscription of an auto kind. Its token is a child of the token of
    the instance. *)
type 'msg auto =
  | Auto :
      {token: Cancel.t; kind: 'p Sub.kind; on: ('p -> 'msg) ref}
      -> 'msg auto
      (** [on] is the newest message function: a pass of a key that stays
          changes it, and a payload takes it when it leaves the queue. *)

type ('model, 'msg) spec =
  { update: 'model -> 'msg -> 'model * 'msg Cmd.t
  ; view: 'model -> 'msg View.t
  ; subs: ('model -> 'msg Sub.t) option  (** [None]: no subscription pass. *) }

type 'msg state =
  | State : {spec: ('model, 'msg) spec; mutable model: 'model} -> 'msg state

type 'msg t =
  { base: Cancel.t  (** The runtime token: the parent of the instance token. *)
  ; mutable alive: bool
  ; mutable token: Cancel.t option  (** Made at the first task. *)
  ; mutable latest: Cancel.t Latest.t
  ; mutable dirty: bool  (** The model changed since the last render. *)
  ; mutable runs: Cancel.t Subs.t
        (** The recorded keys of [Sub.run], with the token of their task. *)
  ; mutable autos: 'msg auto Subs.t  (** The recorded auto keys. *)
  ; mutable early_stale: bool
        (** The model changed since the last subscription pass. *)
  ; mutable state: 'msg state option  (** [None] after the unmount. *) }

val make : Cancel.t -> 'msg t
(** [make base] is a live instance with no state, no token and no key. *)

val token : 'msg t -> Cancel.t
(** The token of the instance: [Cancel.child base] at the first call. The
    runtime calls it only for a live instance. *)

val changed : 'msg t -> unit
(** The model changed: sets [dirty] and [early_stale]. The runtime calls it only
    for a live instance. *)

val unmount : 'msg t -> unit
(** Clears [alive] and [dirty], cancels the token (thus each subscription
    stops), cancels the token of each recorded key (also of a task that ended),
    empties [latest], [runs] and [autos], and sets [state] to [None]. A
    subscription pass does nothing for an instance with no state, thus
    [early_stale] does not matter after the unmount. The runtime never makes a
    dead instance dirty again. Thus after a root catch, the write of the
    fallback is the last render. *)
