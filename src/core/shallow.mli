(** The props compare of components (design section 09): see
    {!Kilim.Component.shallow_equal}. *)

(** A field compare. *)
type same = {same: 'a. 'a -> 'a -> bool}

val default : same
(** The field compare of native code and wasm_of_ocaml. [default.same x y] is
    [true] if [x == y], or if [x] and [y] have the same tag and:
    - They are two strings with the same characters.
    - They are two floats with the same bits.

    Thus [0.] and [-0.] are not equal. In a record or an array of floats,
    {!equal} compares the bits of each float with no allocation. *)

val set_same : same -> unit
(** [set_same s] makes [s.same] the field compare, also for each float of a
    record or an array of floats. The start value is {!default}. kilim.browser
    gives [Object.is] at its start in the js_of_ocaml build. Under js_of_ocaml,
    [==] is [===], which does not keep [0.] and [-0.] apart. Also the tag of a
    JS array that starts with an unsigned integer is that integer: the compare
    by tag would find the arrays [[253, 1]] and [[253, 2]] equal, as two NaN
    floats. *)

val equal : 'a -> 'a -> bool
(** [equal a b] is [true] if the field compare finds [a] and [b] equal. If not,
    and [a] and [b] are two blocks with the same tag and the same size, [equal]
    compares their fields one at a time with the field compare. The scan is only
    for a tag below [Lazy_tag] (a record, a tuple, a variant) and for a record
    or an array of floats. A closure, a lazy value that is not forced, an object
    and a custom block (for example an [int64]) have no scan. In native code, a
    forced lazy value can become its value, and then it compares as its value.
*)

val equal_boxed : 'a -> 'a -> bool
(** As {!equal}, but it reads each float of a record or an array of floats with
    [Obj.double_field], as {!equal} does on a compiler with
    --disable-flat-float-array. For the tests. *)
