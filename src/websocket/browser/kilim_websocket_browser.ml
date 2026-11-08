(* The driver of kilim.websocket on the WebSocket API (design sections 12 and
   13). [connect] is a top-level function: it makes its JS callbacks in its own
   body (the limit of section 12), and the release removes them. *)

module W = Brr_io.Websocket
module K = Kilim_websocket

let string_js = Jstr.v "string"

let warn text =
  ignore (Jv.call (Jv.get Jv.global "console") "warn" [|Jv.of_string text|])

(* One literal: bench/size looks for it in the builds (a short literal of the
   release build of wasm_of_ocaml is not a run of bytes in the file). *)
let binary =
  "kilim.websocket: a binary frame: kilim.websocket gets only text frames, \
   thus it drops this frame"

(* [url] against the URL of the page, with the scheme http changed to ws and
   https to wss. Brr.Uri.v throws a TypeError for a URL that is not valid. *)
let resolve url =
  let base = Jv.get (Jv.get Jv.global "location") "href" in
  K.Private.ws_scheme
    (Jstr.to_string
       (Brr.Uri.to_jstr (Brr.Uri.v ~base:(Jv.to_jstr base) (Jstr.v url))) )

(* The constructor is read at each call, not when Brr_io starts: thus a test can
   replace it (test/weak). The constructor throws a SyntaxError for a scheme
   that is not ws or wss. *)
let create url =
  W.of_jv
    (Jv.new' (Jv.get Jv.global "WebSocket") [|Jv.of_string (resolve url)|])

let open_type : Brr.Ev.Type.void Brr.Ev.type' =
  Brr.Ev.Type.create (Jstr.v "open")

let failed (h : K.handlers) =
  h.failed () ;
  h.closed {code= 1006; reason= ""; clean= false}

let connect url (h : K.handlers) =
  match create url with
  | exception Jv.Error e ->
      warn
        ( "kilim.websocket: the browser refused the URL " ^ url ^ ": "
        ^ Jstr.to_string (Jv.Error.message e) ) ;
      (* A later task, as the events of a socket. *)
      ignore
        (Jv.apply
           (Jv.get Jv.global "setTimeout")
           [|Jv.callback ~arity:1 (fun _ -> failed h)|] ) ;
      K.Private.none
  | ws ->
      let t = W.as_target ws in
      let listen typ f = Brr.Ev.listen typ f t in
      let ls =
        [ listen open_type (fun _ -> h.opened ())
        ; listen Brr_io.Message.Ev.message (fun e ->
              let d : Jv.t = Brr_io.Message.Ev.data (Brr.Ev.as_type e) in
              (* Text only: a binary frame gives a Blob. *)
              if Jstr.equal (Jv.typeof d) string_js then
                h.message (Jv.to_string d)
              else warn binary )
        ; listen W.Ev.close (fun e ->
              let e = Brr.Ev.as_type e in
              h.closed
                { code= W.Ev.Close.code e
                ; reason= Jstr.to_string (W.Ev.Close.reason e)
                ; clean= W.Ev.Close.was_clean e } )
        ; listen Brr.Ev.error (fun _ -> h.failed ()) ]
      in
      (* The socket drops a send in the state CLOSING or CLOSED with no error:
         thus the check gives Not_open. *)
      let is_open () = W.ready_state ws = W.Ready_state.open' in
      { send=
          (fun s ->
            is_open ()
            && begin
              W.send_string ws (Jstr.v s) ;
              true
            end )
      ; close= (fun () -> W.close ~code:1000 ws)
      ; release=
          (fun () ->
            List.iter Brr.Ev.unlisten ls ;
            W.close ~code:1000 ws ) }

let driver = {K.connect}

let install () = K.Private.set_driver (Some driver)
