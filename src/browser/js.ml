(* The JS values that kilim.browser makes one time. They are lazy: the module
   initialization reads no [document], thus node can load the library. *)

let document = lazy (Jv.get Jv.global "document")

let console = lazy (Jv.get Jv.global "console")

let object_ = lazy (Jv.get Jv.global "Object")

let performance = lazy (Jv.get Jv.global "performance")

(* The identity of two nodes. Never [==] on a [Jv.t]: under wasm_of_ocaml, it is
   not the JS identity. *)
let same_node = Jv.strict_equal

(* The text of an exception with no Printexc, because Printexc links Printf. A
   JS error goes to the console as a value, with its stack. *)
let message = function
  | Jv.Error e ->
      Jv.of_error e
  | Failure s | Invalid_argument s ->
      Jv.of_string s
  | e ->
      Jv.of_string
        (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val e))

let error e =
  ignore
    (Jv.call (Lazy.force console) "error" [|Jv.of_string "kilim:"; message e|])

let warn s = ignore (Jv.call (Lazy.force console) "warn" [|Jv.of_string s|])
