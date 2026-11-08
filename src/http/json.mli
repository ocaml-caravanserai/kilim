(** JSON values (RFC 8259). The parser is OCaml code: it gives the same result
    in native code, under js_of_ocaml and under wasm_of_ocaml. *)

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string  (** UTF-8. *)
  | Array of t list
  | Object of (string * t) list
      (** The members in the order of the text, also a duplicate name.
          {!Decode.field} takes the last one, as [JSON.parse] does. *)

val max_depth : int
(** [max_depth] is 512: the maximum number of nested arrays and objects. *)

val parse : string -> (t, string) result
(** [parse s] gives the value of the JSON text [s]. The white space at the start
    and at the end is permitted, and other text after the value is an error. A
    [\u] escape gives UTF-8: a surrogate pair gives one character, and a lone
    surrogate gives U+FFFD, as the conversion of a JS string to UTF-8 does. The
    other bytes of a string stay as they are. A number becomes the nearest
    float: a number out of the range of floats gives an infinity. The error
    message gives the byte offset of the error, for example
    ["unexpected character at offset 3"]. *)

val to_string : t -> string
(** [to_string v] is the JSON text of [v], with no white space: for example the
    body of a request. A number has the shortest of the forms [%.15g], [%.16g]
    and [%.17g] that gives the same float again, and an infinity or a NaN gives
    [null], as [JSON.stringify] does. A string escapes the quote, the backslash
    and the control characters. Thus [parse (to_string v)] is [Ok v] for a value
    with finite numbers: the tests use this round trip as the oracle of the
    parser. *)
