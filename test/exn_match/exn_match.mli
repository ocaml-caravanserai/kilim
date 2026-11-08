(** Exception predicates for [Windtrap.raises_match]. *)

val invalid_arg : string -> exn -> bool
(** [invalid_arg sub e] is [true] if [e] is [Invalid_argument m] and [m]
    contains [sub]. *)

val failure : string -> exn -> bool
(** [failure sub e] is [true] if [e] is [Failure m] and [m] contains [sub]. *)
