(* The property of Component.shallow_equal (design section 14): a true result
   means the same view. test/core runs it in native code with windtrap, and
   test/browser runs it under js_of_ocaml and wasm_of_ocaml with the identity of
   kilim.browser. A list of numbers gives the values, thus each build makes the
   same values from the same list. *)

let shallow_equal = Kilim.Component.shallow_equal

type r = {i: int; s: string; f: float}

type fr = {x: float; y: float}

type v =
  | N
  | I of int
  | S of string
  | F of float
  | R of r
  | Fr of fr
  | A of float array
  | P of v * v
  | C of (unit -> int)

(* The view of a float shows its bits, but one text for all NaN values: a view
   cannot show the bits of a NaN, and Object.is finds all NaN values equal. *)
let float_view x =
  if Float.is_nan x then "nan" else Int64.to_string (Int64.bits_of_float x)

let r_view r = Printf.sprintf "{%d %S %s}" r.i r.s (float_view r.f)

let fr_view r = Printf.sprintf "{%s %s}" (float_view r.x) (float_view r.y)

let a_view a =
  "[" ^ String.concat " " (Array.to_list (Array.map float_view a)) ^ "]"

let rec view = function
  | N ->
      "N"
  | I n ->
      "I " ^ string_of_int n
  | S s ->
      Printf.sprintf "S %S" s
  | F x ->
      "F " ^ float_view x
  | R r ->
      "R " ^ r_view r
  | Fr r ->
      "Fr " ^ fr_view r
  | A a ->
      "A " ^ a_view a
  | P (a, b) ->
      "P (" ^ view a ^ ", " ^ view b ^ ")"
  | C f ->
      "C " ^ string_of_int (f ())

(* The numbers, one at a time. After the last number, the list starts again. *)
type src = {draws: int array; mutable pos: int}

let draw src bound =
  let n = Array.length src.draws in
  if n = 0 then 0
  else begin
    let d = src.draws.(src.pos mod n) in
    src.pos <- src.pos + 1 ;
    abs d mod bound
  end

(* A new box for each float, and a new string for each string. *)
let floats = [|0.; -0.; nan; 1.5; -1.5; infinity; neg_infinity; 5e-324; 0.1|]

let new_float x = Int64.float_of_bits (Int64.bits_of_float x)

let new_string s = Bytes.to_string (Bytes.of_string s)

let strings = [|""; "a"; "b"; "ab"; "row 1"|]

let gen_float src = new_float floats.(draw src (Array.length floats))

let gen_string src = new_string strings.(draw src (Array.length strings))

let gen_r src =
  let i = draw src 3 in
  let s = gen_string src in
  {i; s; f= gen_float src}

let gen_fr src =
  let x = gen_float src in
  {x; y= gen_float src}

let closure k () = k

let rec gen src depth =
  match draw src (if depth > 0 then 9 else 8) with
  | 0 ->
      N
  | 1 ->
      I (draw src 3)
  | 2 ->
      S (gen_string src)
  | 3 ->
      F (gen_float src)
  | 4 ->
      R (gen_r src)
  | 5 ->
      Fr (gen_fr src)
  | 6 ->
      A (Array.init (draw src 3) (fun _ -> gen_float src))
  | 7 ->
      C (closure (draw src 2))
  | _ ->
      let a = gen src (depth - 1) in
      P (a, gen src (depth - 1))

(* A copy with new blocks at each level: no field is the same block. *)
let copy_r r = {i= r.i; s= new_string r.s; f= new_float r.f}

let copy_fr r = {x= new_float r.x; y= new_float r.y}

let rec copy = function
  | N ->
      N
  | I n ->
      I n
  | S s ->
      S (new_string s)
  | F x ->
      F (new_float x)
  | R r ->
      R (copy_r r)
  | Fr r ->
      Fr (copy_fr r)
  | A a ->
      A (Array.map new_float a)
  | P (a, b) ->
      P (copy a, copy b)
  | C f ->
      C (fun () -> f ())

(* A new top block with the same fields. *)
let share = function
  | N ->
      N
  | I n ->
      I n
  | S s ->
      S s
  | F x ->
      F x
  | R r ->
      R r
  | Fr r ->
      Fr r
  | A a ->
      A a
  | P (a, b) ->
      P (a, b)
  | C f ->
      C f

type mode = Two | Copy | Same | Share

(* [pair draws] gives two values and the way that the second one was made. *)
let pair draws =
  let src = {draws= Array.of_list draws; pos= 0} in
  let a = gen src 2 in
  match draw src 4 with
  | 0 ->
      (Two, a, gen src 2)
  | 1 ->
      (Copy, a, copy a)
  | 2 ->
      (Same, a, a)
  | _ ->
      (Share, a, share a)

(* The checks of one pair, with each payload of the same constructor: - A true
   result gives the same view. - A value with no block in its fields, built two
   times, is equal. Also a value with the same fields in a new top block, and
   the same value. *)
let check draws =
  let errors = ref [] in
  let law name view must a b =
    let eq = shallow_equal a b in
    if eq && view a <> view b then
      errors :=
        Printf.sprintf "%s: true for %s and %s" name (view a) (view b)
        :: !errors ;
    if must && not eq then
      errors :=
        Printf.sprintf "%s: false for %s and %s" name (view a) (view b)
        :: !errors
  in
  let mode, a, b = pair draws in
  let flat = function N | I _ | S _ | F _ -> true | _ -> false in
  let new_fields = mode = Copy || mode = Share || mode = Same in
  let rec go a b ~same =
    law "value" view (same || (new_fields && flat a)) a b ;
    match (a, b) with
    | S x, S y ->
        law "string" (Printf.sprintf "%S") new_fields x y
    | F x, F y ->
        law "float" float_view new_fields x y
    | R x, R y ->
        law "record" r_view new_fields x y
    | Fr x, Fr y ->
        law "float record" fr_view new_fields x y
    | A x, A y ->
        law "float array" a_view new_fields x y
    | P (x1, y1), P (x2, y2) ->
        go x1 x2 ~same ; go y1 y2 ~same
    | _ ->
        ()
  in
  go a b ~same:(mode = Same || mode = Share) ;
  List.rev !errors
