(** HTML text that the diff writes with [innerHTML], with no check (design
    section 13). Do not make a value from data of a user. *)

(** A value of the platform. kilim.browser adds a constructor for a
    [TrustedHTML] value. *)
type native = ..

type t

val of_string : string -> t
(** [of_string html] is [html] with no native value, for a page with no Trusted
    Types. *)

val of_native : string -> native -> t
(** [of_native html n] is the native value [n] and its HTML text [html]. The
    diff compares [html], and it writes [n]. *)

val to_string : t -> string

val native : t -> native option
