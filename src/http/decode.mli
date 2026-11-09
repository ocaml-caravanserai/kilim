(** Decoders: functions from a {!Json.t} to an OCaml value. *)

(** A decoder of a value of type ['a]. *)
type 'a t

(** The error of a decoder. [path] gives the place of the error from the root:
    the names of the fields and the indexes of the arrays, for example
    [["results"; "3"; "name"]]. *)
type error = {path: string list; message: string}

val decode : 'a t -> Json.t -> ('a, error) result
(** [decode d v] gives the result of [d] for the value [v]. *)

val decode_string : 'a t -> string -> ('a, error) result
(** [decode_string d s] parses [s] with {!Json.parse}, then decodes the value. A
    parse error has the path [[]]. *)

val error_to_string : error -> string
(** For example ["results.3.name: expected a string, got a number"]. With the
    path [[]], only the message. *)

val string : string t
(** A JSON string. *)

val bool : bool t
(** A JSON boolean. *)

val float : float t
(** A JSON number. *)

val int : int t
(** An integral number in the range of [int] of the build: from [min_int] to
    [max_int]. The range depends on the build: 63 bits in native code, 32 bits
    under js_of_ocaml and 31 bits under wasm_of_ocaml (thus [max_int] is
    1,073,741,823 there). Decode a larger integer, for example an id, with
    {!float} or {!string}. *)

val null : 'a -> 'a t
(** [null x] gives [x] for [null]. *)

val nullable : 'a t -> 'a option t
(** [None] for [null], else [Some] of the value. *)

val list : 'a t -> 'a list t
(** An array. The index of an element that fails is in the path. *)

val field : string -> 'a t -> 'a t
(** [field name d] decodes the member [name] of an object with [d]. If the
    object has two members [name], the last one, as [JSON.parse] does. A missing
    member is an error. *)

val field_opt : string -> 'a t -> 'a option t
(** As {!field}, but a missing member gives [None]. *)

val index : int -> 'a t -> 'a t
(** [index i d] decodes the element [i] of an array (from 0) with [d]. *)

val value : Json.t t
(** The value with no change. *)

val succeed : 'a -> 'a t
(** [succeed x] gives [x] for each value. *)

val fail : string -> 'a t
(** [fail message] fails with [message]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f d] gives [f x] when [d] gives [x]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind d f] decodes the same value with [f x] when [d] gives [x]. *)

val one_of : 'a t list -> 'a t
(** The result of the first decoder that does not fail. If all fail, the error
    of the last one. [one_of []] always fails. *)

(** The binding operators of the decoders. For example, a decoder of a record:
    [let+ x = field "x" int and+ y = field "y" int in {x; y}]. *)
module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** [let+ x = d in e] is [map (fun x -> e) d]. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** The two decoders decode the same value. *)
end
