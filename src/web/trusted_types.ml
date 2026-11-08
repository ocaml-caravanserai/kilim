let exists () = not (Jv.is_none (Jv.get Jv.global "trustedTypes"))

let function_js = Jstr.v "function"

(* In a browser with no Trusted Types, an element with the id [trustedTypes] is
   the global [trustedTypes], with no [isHTML]: thus the check of the JS type. A
   strict compare with [true], not a conversion: a polyfill can give a value
   that is not a boolean. *)
let is_html v =
  let tt = Jv.get Jv.global "trustedTypes" in
  (not (Jv.is_none tt))
  && Jstr.equal (Jv.typeof (Jv.get tt "isHTML")) function_js
  && Jv.strict_equal (Jv.call tt "isHTML" [|v|]) Jv.true'
