(** Subscriptions (design section 08): a set of leaves with keys, as a value.
    The runtime takes the leaves with {!leaves} at each pass, compares their
    keys with the recorded set, stops the removed keys and starts the new keys.
*)

(** A kind of subscription with no key, for example [every] here and [window] in
    kilim.browser: a witness of the type of the payload. *)
type 'p kind = 'p Type.Id.t

val kind : unit -> 'p kind
(** [kind ()] makes a new kind. Make each kind one time, at the top level: each
    call gives a different kind. *)

type 'msg t =
  | None
  | Batch of 'msg t list
  | Run of string * (('msg -> unit) -> unit Async.t)
      (** [Run (key, f)]: the task [f send]. *)
  | Auto :
      { kind: 'p kind
      ; params: string  (** The parameters, as a part of the key. *)
      ; start: ('p -> unit) -> unit Async.t
            (** The task, with the function that delivers a payload. *)
      ; on: 'p -> 'msg  (** The message of a payload. *) }
      -> 'msg t

val auto :
     'p kind
  -> params:string
  -> start:(('p -> unit) -> unit Async.t)
  -> ('p -> 'msg)
  -> 'msg t
(** [auto kind ~params ~start on] is a leaf of [kind] whose key comes from
    [params]. Put each parameter of [start] in [params]. *)

val every : Span.t -> 'msg -> 'msg t
(** See {!Kilim.Sub.every}. *)

(** The key of a leaf. *)
module Key : sig
  (** The key of an auto leaf: [Type.Id.uid] of its kind, its params, and its
      position among the leaves with the same kind and the same params. Thus the
      same key always has the same kind. *)
  type auto = {uid: int; params: string; nth: int}

  type t = Run of string  (** The key of a [Run]. *) | Auto of auto

  val compare : t -> t -> int
  (** [Run] keys come first. *)
end

(** A leaf, with its key. The runtime records the key as [Key.Run] or
    [Key.Auto]. *)
type 'msg leaf =
  | Run_leaf of string * (('msg -> unit) -> unit Async.t)
  | Auto_leaf :
      Key.auto * 'p kind * (('p -> unit) -> unit Async.t) * ('p -> 'msg)
      -> 'msg leaf

val leaves : 'msg t -> 'msg leaf list
(** The leaves in the order of the tree (left to right). The walk uses an
    explicit stack: a deep [Batch] does not overflow the stack.

    @raise Invalid_argument for a duplicate key of [Run]. *)
