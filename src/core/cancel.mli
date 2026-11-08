(** Cancel tokens (design section 12). The tokens make a tree: the root token,
    an instance token, a task token, and a child token for each step of
    {!Async.make} and for each branch of [and+], {!Async.first} and
    {!Async.timeout}. *)

type t

val cancelled : t -> bool

val on_end : t -> (unit -> unit) -> unit
(** See {!Kilim.Cancel.on_end}. *)

val root : unit -> t

val child : t -> t
(** A child of a cancelled token is cancelled at once and is in no list. *)

val release : t -> unit
(** The token detaches from its parent in O(1), is marked as ended, and runs its
    end actions. The children of the token stay. *)

val cancel : t -> unit
(** Iterative: the children first, the newest first, then the actions in the
    reverse order of registration. A cancel of an ended token sets its cancelled
    flag and runs no action again. *)

val children : t -> int
(** The number of children in the list of the token (for the tests). *)

val descendants : t -> int
(** The number of tokens below the token, in the lists of children at all levels
    (for the tests). A cancelled or ended token is in no list. *)
