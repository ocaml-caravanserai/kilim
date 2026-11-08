(* The props compare of design section 09. It reads the tags of the values with
   Obj, as spike 3 of Phase 0 did (docs/phase0.md). *)

type same = {same: 'a. 'a -> 'a -> bool}

let bits (x : Obj.t) = Int64.bits_of_float (Obj.obj x)

let by_tag (a : Obj.t) (b : Obj.t) =
  let t = Obj.tag a in
  t = Obj.tag b
  &&
  if t = Obj.string_tag then String.equal (Obj.obj a) (Obj.obj b)
  else t = Obj.double_tag && Int64.equal (bits a) (bits b)

(* The field compare of native code and wasm_of_ocaml. kilim.browser replaces
   all of it under js_of_ocaml: there [0. == -0.] is true, and a JS array [[253,
   x]] has the tag of a float. *)
let default = {same= (fun a b -> a == b || by_tag (Obj.repr a) (Obj.repr b))}

let hook = ref default

let set_same s = hook := s

let field a b = !hook.same a b

let rec fields a b n i =
  i = n || (field (Obj.field a i) (Obj.field b i) && fields a b n (i + 1))

(* A record of floats has the tag of a float array. With flat float arrays, its
   floats are not boxes, and the array read gives a float with no allocation.
   [Obj.double_field] is a C call that gives a box. A compiler with
   --disable-flat-float-array keeps boxes in a float array, but a record of
   floats stays flat: there the array read takes the path of a pointer and can
   crash, thus [Obj.double_field]. The default compares the bits. A hook gets
   the floats. *)
let flat = Obj.tag (Obj.repr (Array.make 1 0.)) = Obj.double_array_tag

let[@inline] float_equal s (x : float) y =
  if s == default then Int64.bits_of_float x = Int64.bits_of_float y
  else s.same x y

let float_at (a : Obj.t) i = Array.unsafe_get (Obj.obj a : float array) i

let rec floats a b n i =
  i = n
  || (float_equal !hook (float_at a i) (float_at b i) && floats a b n (i + 1))

let rec boxed_floats a b n i =
  i = n
  || float_equal !hook (Obj.double_field a i) (Obj.double_field b i)
     && boxed_floats a b n (i + 1)

let compare ~flat (a : 'a) (b : 'a) =
  let a = Obj.repr a and b = Obj.repr b in
  field a b
  ||
  let t = Obj.tag a in
  t = Obj.tag b
  && (t < Obj.lazy_tag || t = Obj.double_array_tag)
  &&
  let n = Obj.size a in
  n = Obj.size b
  &&
  if t <> Obj.double_array_tag then fields a b n 0
  else if flat then floats a b n 0
  else boxed_floats a b n 0

let equal a b = compare ~flat a b

let equal_boxed a b = compare ~flat:false a b
