(** [Object.is].

    Web API: [Object.is(a, b)] of ECMAScript. Brr 0.0.9 has no binding for it.
    [Jv.strict_equal] is the operator [===], which gives true for [0] and [-0],
    and false for two NaN values. Under js_of_ocaml, kilim.browser compares the
    models and the fields of [Component.shallow_equal] with [Object.is]. *)

val is : Jv.t -> Jv.t -> bool
(** [is a b] is [Object.is(a, b)]: the same value. [0] and [-0] are different,
    two NaN values are the same, an object is the same only as itself, and two
    equal strings are the same. *)
