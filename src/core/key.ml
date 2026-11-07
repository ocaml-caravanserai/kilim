type t = Int of int | String of string

let int i = Int i

let string s = String s

let equal a b =
  match (a, b) with
  | Int x, Int y ->
      Int.equal x y
  | String x, String y ->
      String.equal x y
  | Int _, String _ | String _, Int _ ->
      false

let compare a b =
  match (a, b) with
  | Int x, Int y ->
      Int.compare x y
  | String x, String y ->
      String.compare x y
  | Int _, String _ ->
      -1
  | String _, Int _ ->
      1

(* The hash has 30 bits: an int of wasm_of_ocaml has 31 bits, with the sign. A
   product of two 30-bit values overflows in js_of_ocaml (32 bits) and in
   wasm_of_ocaml (31 bits). But the low 30 bits of the product are the same in
   the three builds. Thus each step keeps only the low 30 bits before a right
   shift, and the hash of a key is the same in the three builds. *)
let bits = 0x3fff_ffff

(* A finalizer of the murmur3 type: each bit of the input changes about half of
   the bits of the result. Each step is a bijection on 30 bits. *)
let mix h =
  let h = h land bits in
  let h = h lxor (h lsr 15) in
  let h = h * 0x3feb_352d land bits in
  let h = h lxor (h lsr 13) in
  let h = h * 0x046c_a68b land bits in
  h lxor (h lsr 16)

(* The bits above bit 29 go into the low 30 bits, after a mix: thus [i] and [i
   lsl 30] have different hashes. A shift of more than the size of an int is not
   specified, thus two shifts of 30. For an int from 0 to [2^30 - 1], the result
   is the int. *)
let fold i =
  let high = i lsr 30 in
  i land bits lxor mix (high lxor (high lsr 30))

(* FNV-1a on 30 bits. The operators [lxor], [*] and [land] have the same
   precedence, from left to right: [a lxor c * p land bits] is [((a lxor c) * p)
   land bits]. *)
let fnv s =
  let h = ref 0x011c_9dc5 in
  for i = 0 to String.length s - 1 do
    h := !h lxor Char.code (String.unsafe_get s i) * 0x0100_0193 land bits
  done ;
  !h

let hash = function Int i -> mix (fold i) | String s -> mix (fnv s)

let to_string = function Int i -> string_of_int i | String s -> s
