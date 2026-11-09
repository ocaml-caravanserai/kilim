(* The fetch transport (design sections 12 and 13). [start] is a top-level
   function: it makes its JS callbacks in its own body (the limit of section
   12), and the future chain calls no function that can raise. *)

module Fetch = Brr_io.Fetch
module H = Kilim_http

let text e =
  Jstr.to_string (Jv.Error.name e) ^ ": " ^ Jstr.to_string (Jv.Error.message e)

let headers h =
  List.rev
    (Fetch.Headers.fold
       (fun k v acc -> (Jstr.to_string k, Jstr.to_string v) :: acc)
       h [] )

let init (r : H.request) signal =
  Fetch.Request.init
    ~method':(Jstr.v (H.string_of_meth r.meth))
    ~headers:
      (Fetch.Headers.of_assoc
         (List.map (fun (k, v) -> (Jstr.v k, Jstr.v v)) r.headers) )
    ?body:(Option.map (fun b -> Fetch.Body.of_jstr (Jstr.v b)) r.body)
    ~signal ()

(* The body in the same step as the headers. *)
let response r =
  Fut.map
    (function
      | Error e ->
          Error (text e)
      | Ok body ->
          Ok
            { H.status= Fetch.Response.status r
            ; url= Jstr.to_string (Fetch.Response.url r)
            ; headers= headers (Fetch.Response.headers r)
            ; body= Jstr.to_string body } )
    (Fetch.Body.text (Fetch.Response.as_body r))

let start (r : H.request) tok : (H.response, string) result Fut.t =
  let c = Brr.Abort.controller () in
  (* The end action runs also at the normal end of the step: then it does not
     abort. *)
  Kilim.Cancel.on_end tok (fun () ->
      if Kilim.Cancel.cancelled tok then Brr.Abort.abort c ) ;
  (* new Headers throws a TypeError for a name that is not valid. *)
  match init r (Brr.Abort.signal c) with
  | exception Jv.Error e ->
      Fut.return (Error (text e))
  | init ->
      Fut.bind
        (Fetch.url ~init (Jstr.v r.url))
        (function Error e -> Fut.return (Error (text e)) | Ok x -> response x)

let transport = {H.send= (fun r -> Kilim_browser.Async.of_fut (start r))}

let install () = H.Private.set_transport (Some transport)
