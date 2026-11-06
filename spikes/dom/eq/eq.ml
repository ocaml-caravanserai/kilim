(* The props compare of section 09 of docs/design.html. *)
let by_tag a b =
  a == b
  || Obj.is_block a && Obj.is_block b
     &&
     let ta = Obj.tag a in
     ta = Obj.tag b
     &&
     if ta = Obj.string_tag then String.equal (Obj.obj a) (Obj.obj b)
     else if ta = Obj.double_tag then
       Int64.equal
         (Int64.bits_of_float (Obj.obj a))
         (Int64.bits_of_float (Obj.obj b))
     else false

(* kilim.browser installs Object.is in the js_of_ocaml build. *)
let field_equal_hook : (Obj.t -> Obj.t -> bool) ref = ref by_tag

let field_equal a b = !field_equal_hook a b

let shallow_equal (a : 'a) (b : 'a) =
  let a = Obj.repr a and b = Obj.repr b in
  if field_equal a b then true
  else if Obj.is_int a || Obj.is_int b then false
  else if Obj.tag a = Obj.double_array_tag && Obj.tag b = Obj.double_array_tag
  then
    let n = Obj.size a in
    n = Obj.size b
    &&
    let rec go i =
      i = n
      || field_equal
           (Obj.repr (Obj.double_field a i))
           (Obj.repr (Obj.double_field b i))
         && go (i + 1)
    in
    go 0
  else if Obj.tag a <> Obj.tag b || Obj.tag a >= Obj.lazy_tag then false
  else
    let n = Obj.size a in
    n = Obj.size b
    &&
    let rec go i =
      i = n || (field_equal (Obj.field a i) (Obj.field b i) && go (i + 1))
    in
    go 0

(* The cases that do not need a JS value. *)
type r = {v: float; s: string; i: int} [@@warning "-69"]

type fr = {x: float; y: float} [@@warning "-69"]

let failures = ref 0

let check name ok =
  print_endline ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

let s n = "row " ^ string_of_int (Sys.opaque_identity n)

let f x = Sys.opaque_identity x +. 0.

let run ~extra =
  print_endline
    ( "backend: "
    ^
    match Sys.backend_type with
    | Sys.Native ->
        "native"
    | Sys.Bytecode ->
        "bytecode"
    | Sys.Other s ->
        s ) ;
  check "a string built two times: equal"
    (shallow_equal {v= 1.; s= s 1; i= 1} {v= 1.; s= s 1; i= 1}) ;
  check "a float built two times: equal"
    (shallow_equal {v= f 2.5; s= ""; i= 0} {v= f 2.5; s= ""; i= 0}) ;
  check "0. and -0.: not equal"
    (not (shallow_equal {v= 0.; s= ""; i= 0} {v= -0.; s= ""; i= 0})) ;
  check "float record, 0. and -0.: not equal"
    (not (shallow_equal {x= 0.; y= 1.} {x= -0.; y= 1.})) ;
  check "float record built two times: equal"
    (shallow_equal {x= f 1.; y= f 2.} {x= f 1.; y= f 2.}) ;
  check "different strings: not equal"
    (not (shallow_equal {v= 1.; s= s 1; i= 1} {v= 1.; s= s 2; i= 1})) ;
  check "different ints: not equal"
    (not (shallow_equal {v= 1.; s= ""; i= 1} {v= 1.; s= ""; i= 2})) ;
  check "a closure field: not equal"
    (not (shallow_equal (1, fun x -> x + 1) (1, fun x -> x + 1))) ;
  let mk n = fun x -> x + n in
  check "top-level closures: not equal" (not (shallow_equal (mk 1) (mk 2))) ;
  check "the same closure: equal"
    (let g = mk 3 in
     shallow_equal g g ) ;
  check "top-level string built two times: equal" (shallow_equal (s 3) (s 3)) ;
  check "top-level 0. and -0.: not equal"
    (not (shallow_equal (Sys.opaque_identity 0.) (Sys.opaque_identity (-0.)))) ;
  extra check ;
  if !failures > 0 then exit 1
