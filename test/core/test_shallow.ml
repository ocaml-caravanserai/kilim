(* Component.shallow_equal (design section 09). Sys.opaque_identity and the
   functions [s] and [f] make a new string or a new float box at each call. *)

open Windtrap
open Kilim

let eq = Component.shallow_equal

let s n = "row " ^ string_of_int (Sys.opaque_identity n)

let f x = Int64.float_of_bits (Int64.bits_of_float (Sys.opaque_identity x))

type r = {v: float; s: string; i: int}

type fr = {x: float; y: float}

type tag = A of int | B of int

type m = {mutable n: int}

let mk n x = x + n

let strings () =
  is_true (s 1 != s 1) ;
  is_true (eq (s 1) (s 1)) ;
  is_true (eq {v= 1.; s= s 1; i= 1} {v= 1.; s= s 1; i= 1}) ;
  is_false (eq (s 1) (s 2)) ;
  is_false (eq {v= 1.; s= s 1; i= 1} {v= 1.; s= s 2; i= 1})

let zeros () =
  is_false (eq (f 0.) (f (-0.))) ;
  is_false (eq {v= f 0.; s= ""; i= 0} {v= f (-0.); s= ""; i= 0}) ;
  is_false (eq {x= f 0.; y= 1.} {x= f (-0.); y= 1.}) ;
  is_false (eq [|1.; f 0.|] [|1.; f (-0.)|])

let floats () =
  is_true (f 2.5 != f 2.5) ;
  is_true (eq (f 2.5) (f 2.5)) ;
  is_true (eq {v= f 2.5; s= ""; i= 0} {v= f 2.5; s= ""; i= 0}) ;
  is_true (eq {x= f 1.; y= f 2.} {x= f 1.; y= f 2.}) ;
  is_true (eq [|f 1.; f 2.|] [|f 1.; f 2.|]) ;
  is_true (eq [||] [||]) ;
  is_false (eq (f 1.) (f 2.)) ;
  is_false (eq {x= 1.; y= f 2.} {x= 1.; y= f 3.}) ;
  is_false (eq [|1.; f 2.|] [|1.; f 3.|])

let nans () =
  is_true (eq (f nan) (f nan)) ;
  is_true (eq {v= f nan; s= ""; i= 0} {v= f nan; s= ""; i= 0}) ;
  is_true (eq {x= f nan; y= 1.} {x= f nan; y= 1.})

let ints () =
  is_true (eq 1 1) ;
  is_false (eq 1 2) ;
  is_false (eq (1, 2, 3) (1, 2, 4)) ;
  is_false (eq (0, 2, 3) (1, 2, 3)) ;
  is_true (eq (1, 2, 3) (1, 2, 3))

let closures () =
  is_false (eq (mk (Sys.opaque_identity 1)) (mk (Sys.opaque_identity 1)))

let same_closure () =
  let g = mk (Sys.opaque_identity 1) in
  is_true (eq g g)

let closure_field () =
  let g = mk (Sys.opaque_identity 1) in
  is_false (eq (1, mk (Sys.opaque_identity 1)) (1, mk (Sys.opaque_identity 1))) ;
  is_true (eq (1, g) (1, g))

(* A lazy value that is not forced. In native code, a forced lazy value can
   become its value. *)
let no_scan () =
  let k () = 1 in
  is_false (eq (Lazy.from_fun k) (Lazy.from_fun k)) ;
  is_false (eq (Int64.of_int (Sys.opaque_identity 5)) (Int64.of_int 5)) ;
  let i = Sys.opaque_identity (Int64.of_int 5) in
  is_true (eq i i)

(* The default compare of a float record and of a float array allocates nothing.
   The first call of [run] is a warm call. The box of each count goes into the
   next difference, thus the second difference has the same box. *)
let no_alloc () =
  let r1 = {x= f 1.; y= f 2.} and r2 = {x= f 1.; y= f 2.} in
  let a1 = [|f 1.; f 2.; f 3.|] and a2 = [|f 1.; f 2.; f 3.|] in
  let run () =
    for _ = 1 to 100 do
      ignore (Sys.opaque_identity (eq r1 r2 && eq a1 a2))
    done
  in
  run () ;
  is_true (eq r1 r2 && eq a1 a2) ;
  let w0 = Gc.minor_words () in
  let w1 = Gc.minor_words () in
  run () ;
  let w2 = Gc.minor_words () in
  equal int 0 (int_of_float (w2 -. w1 -. (w1 -. w0)))

let shapes () =
  is_false (eq [|1|] [|1; 2|]) ;
  is_false (eq [|1.|] [|1.; 2.|]) ;
  is_false (eq (A 1) (B 1)) ;
  is_false (eq None (Some 1)) ;
  is_false (eq (Some 1) None) ;
  is_true (eq (A 1) (A 1))

let mutable_field () =
  let p = {n= 1} in
  let q = p in
  p.n <- 2 ;
  is_true (eq q p)

(* The read of [Obj.double_field], for a compiler with no flat float arrays. *)
let boxed () =
  let eqb = Private.Shallow.equal_boxed in
  is_true (eqb {x= f 1.; y= f 2.} {x= f 1.; y= f 2.}) ;
  is_true (eqb [|f 1.; f 2.; f nan|] [|f 1.; f 2.; f nan|]) ;
  is_false (eqb {x= f 0.; y= 1.} {x= f (-0.); y= 1.}) ;
  is_false (eqb {x= 1.; y= f 2.} {x= 1.; y= f 3.}) ;
  is_false (eqb [|1.; f 0.|] [|1.; f (-0.)|]) ;
  is_false (eqb [|1.|] [|1.; 2.|]) ;
  is_true (eqb (s 1) (s 1)) ;
  is_false (eqb (1, s 1) (1, s 2)) ;
  let is_float x =
    Obj.is_block (Obj.repr x) && Obj.tag (Obj.repr x) = Obj.double_tag
  in
  Private.Shallow.set_same {same= (fun x _ -> is_float x)} ;
  let hooked = eqb {x= f 0.; y= 1.} {x= f (-0.); y= 1.} in
  Private.Shallow.set_same Private.Shallow.default ;
  is_true hooked

let hook () =
  let restore () = Private.Shallow.set_same Private.Shallow.default in
  Private.Shallow.set_same {same= (fun _ _ -> false)} ;
  let g = mk (Sys.opaque_identity 1) in
  let never = (eq g g, eq 1 1, eq (s 1) (s 1)) in
  Private.Shallow.set_same {same= (fun _ _ -> true)} ;
  let always =
    ( eq (f 0.) (f (-0.))
    , eq (1, f 0.) (1, f (-0.))
    , eq {x= f 0.; y= 1.} {x= f (-0.); y= 1.} )
  in
  (* A hook that finds two floats equal, and no other values: the float record
     and the float array give it each float. *)
  let is_float x =
    Obj.is_block (Obj.repr x) && Obj.tag (Obj.repr x) = Obj.double_tag
  in
  Private.Shallow.set_same {same= (fun x _ -> is_float x)} ;
  let floats =
    (eq {x= f 0.; y= 1.} {x= f (-0.); y= 1.}, eq [|f 0.|] [|f (-0.)|])
  in
  restore () ;
  equal (pair bool bool) (true, true) floats ;
  equal (triple bool bool bool) (false, false, false) never ;
  equal (triple bool bool bool) (true, true, true) always ;
  is_false (eq {x= f 0.; y= 1.} {x= f (-0.); y= 1.}) ;
  is_true (eq (s 1) (s 1))

let draws = Gen.list ~size:(Gen.int_range 1 40) (Gen.int_range 0 1000)

let view_law d = equal (list string) [] (Shallow_law.check d)

let tests =
  [ group "shallow_equal"
      [ test "a string built two times is equal" strings
      ; test
          "0. and -0. are not equal, also in a float record and a float array"
          zeros
      ; test "a float built two times is equal" floats
      ; test "a NaN built two times is equal" nans
      ; test "ints compare by value, also in a field" ints
      ; test "two closures with the same code are not equal" closures
      ; test "the same closure is equal" same_closure
      ; test "a closure in a field compares by identity" closure_field
      ; test "a lazy value that is not forced and an int64 have no scan" no_scan
      ; test "the compare of a float record and a float array allocates nothing"
          no_alloc
      ; test "two records of different sizes or tags are not equal" shapes
      ; test "a mutable field changed in place compares equal" mutable_field
      ; test "the boxed read of the floats gives the same results" boxed
      ; test "the hook of set_same replaces the field compare" hook
      ; prop ~count:2000 "true means the same view" draws view_law ] ]
