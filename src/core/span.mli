(** Durations: a count of milliseconds, from 0 to [max_ms]. *)

(** A duration. *)
type t

val max_ms : int
(** [max_ms] is 1,073,741,823 ms (2{^ 30} - 1, about 12.4 days). It is the same
    in each build: wasm_of_ocaml has 31-bit integers, and js_of_ocaml 32-bit
    integers. *)

val zero : t
(** [zero] is 0 ms. *)

val ms : int -> t
(** [ms n] is [n] milliseconds.

    Raises [Invalid_argument] if [n] is negative or more than [max_ms]. *)

val s : int -> t
(** [s n] is [n] seconds.

    Raises [Invalid_argument] if [n] is negative or the result is more than
    [max_ms]. *)

val to_ms : t -> int
(** [to_ms d] is the number of milliseconds of [d]. *)

val compare : t -> t -> int
(** [compare a b] compares the two durations, as [Int.compare] does. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same duration. *)
