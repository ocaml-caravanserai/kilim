(* A program that uses each module of kilim.http. test/link examines the units
   that it links (design section 13). *)

module H = Kilim_http

let () =
  let d =
    let open H.Decode.Syntax in
    let+ a =
      H.Decode.one_of
        [ H.Decode.field "a" (H.Decode.list H.Decode.int)
        ; H.Decode.map (fun _ -> []) (H.Decode.index 0 H.Decode.value) ]
    and+ b = H.Decode.field_opt "b" (H.Decode.nullable H.Decode.float) in
    (a, b)
  in
  ignore (H.Decode.decode_string d (H.Json.to_string (H.Json.Number 1.))) ;
  ignore (H.Decode.error_to_string {path= []; message= ""}) ;
  ignore (H.get_json ~timeout:(Kilim.Span.ms 1) d "/x") ;
  H.Private.set_transport {send= (fun _ -> Kilim.Async.return (Error ""))} ;
  ignore (H.Private.transport ()) ;
  H.Private.clear ()
