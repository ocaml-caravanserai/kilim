(** The keys of {!View.keyed} (design section 11): an int or a string. *)

type t

val int : int -> t

val string : string -> t

val equal : t -> t -> bool
(** [int 1] and [string "1"] are different keys. *)

val compare : t -> t -> int
(** A total order for the map of the keyed diff. *)

val hash : t -> int
(** A hash for the table of the keyed diff: two equal keys have the same hash.
    The hash has 30 bits, thus it is not negative. For an int from 0 to
    [2^30 - 1] and for a string, the hash is the same in the three builds. *)

val to_string : t -> string
(** The int or the string, for an error message. *)
