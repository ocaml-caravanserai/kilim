(** Cancel tokens. The tokens make a tree. A cancel cancels the children first,
    then runs the end actions of the token. *)

type t

val cancelled : t -> bool

val on_end : t -> (unit -> unit) -> unit
(** [on_end t f] runs [f] one time: at the cancel of [t], or when the step of
    [t] ends. If [t] is already cancelled or ended, [f] runs at once. *)

(**/**)

(* Internal: the runtime and Async use these functions. *)

val root : unit -> t

val child : t -> t
(** A child of a cancelled token is cancelled at once. *)

val release : t -> unit
(** O(1): the token detaches from its parent, is marked as ended, and runs its
    end actions. *)

val cancel : t -> unit
(** Iterative. A cancel of an ended token sets its cancelled flag and runs no
    action again. *)

val set_error_handler : (exn -> unit) -> unit
(** The function that gets an exception from an end action. *)
