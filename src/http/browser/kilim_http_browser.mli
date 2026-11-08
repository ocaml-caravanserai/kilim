(** kilim.http.browser: the fetch transport of kilim.http.

    {[
    let () =
      Kilim_http_browser.install () ;
      Kilim_browser.run ~mount:"app" app
    ]} *)

val install : unit -> unit
(** [install ()] makes [fetch] the transport of kilim.http. A second call
    changes nothing.

    Each request reads the body in full in the same step as the headers. A
    cancel of the task calls [abort ()] on the [AbortController] of the request:
    the normal end of a request does not. A rejected [fetch] or body read, and a
    request that the browser refuses at once (for example a header name that is
    not valid), give [Network "NAME: MESSAGE"] with the name and the message of
    the JS error, for example ["TypeError: Failed to fetch"] in Chrome. *)
