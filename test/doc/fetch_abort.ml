(* The example of Kilim_browser.Async.of_fut (src/browser/kilim_browser.mli): a
   fetch that aborts on a cancel. *)

let fetch_text url : string Kilim_browser.Async.t =
  Kilim_browser.Async.map Jstr.to_string
    (Kilim_browser.Async.of_fut_or_error (fun tok ->
         let ctrl = Brr.Abort.controller () in
         Kilim.Cancel.on_end tok (fun () -> Brr.Abort.abort ctrl) ;
         let init =
           Brr_io.Fetch.Request.init ~signal:(Brr.Abort.signal ctrl) ()
         in
         Fut.bind
           (Brr_io.Fetch.url ~init (Jstr.v url))
           (function
             | Error e ->
                 Fut.error e
             | Ok r ->
                 Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body r) ) ) )
