(* The TrustedHTML values of Unsafe_html (design section 13). *)

type Kilim.Private.Unsafe_html.native += Trusted of Jv.t

(* The text of a value of the page through [String(v)], with no cast. *)
let string_of v = Jv.to_string (Jv.call Jv.global "String" [|v|])

let of_trusted v =
  let tt = Jv.get Jv.global "trustedTypes" in
  if Jv.is_none tt then Kilim.Unsafe_html.of_string (string_of v)
  else if Jv.strict_equal (Jv.call tt "isHTML" [|v|]) Jv.true' then
    Kilim.Private.Unsafe_html.of_native (string_of v) (Trusted v)
  else invalid_arg "Unsafe_html.of_trusted: not a TrustedHTML"
