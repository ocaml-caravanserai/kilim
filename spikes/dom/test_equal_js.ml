(* The browser builds: Object.is under js_of_ocaml, and JS values. *)
open Eq_spike.Eq

let () =
  ( match Sys.backend_type with
  | Sys.Other "js_of_ocaml" ->
      let is = Jv.get (Jv.get Jv.global "Object") "is" in
      field_equal_hook :=
        fun a b -> Jv.to_bool (Jv.apply is [|Obj.magic a; Obj.magic b|])
  | _ ->
      () ) ;
  run ~extra:(fun check ->
      check "a JS object field (Jv.t): not equal"
        (not (shallow_equal (1, Jv.obj [||]) (1, Jv.obj [||]))) ;
      check "the same JS object: equal"
        (let o = Jv.obj [||] in
         shallow_equal (1, o) (1, o) ) )
