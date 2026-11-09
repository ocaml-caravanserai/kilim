(** kilim.websocket.browser: the driver of kilim.websocket on the WebSocket API
    of the browser.

    {[
    let () =
      Kilim_websocket_browser.install () ;
      Kilim_browser.run ~mount:"app" app
    ]} *)

val install : unit -> unit
(** [install ()] makes the driver of this module the driver of kilim.websocket.
    A second call does nothing. The driver:
    - Resolves the URL against [location.href], and changes the scheme [http] to
      [ws] and [https] to [wss]. Then it calls [new WebSocket(url)], with the
      constructor of [globalThis.WebSocket] at the time of the call.
    - Adds listeners for [open], [message], [close] and [error] on the socket. A
      message whose data is not a string (a binary frame) writes a warning to
      the console and gives no message.
    - Sends only if [readyState] is [OPEN]. If not, the send gives [Not_open].
    - At the end of the key, removes the four listeners, then calls
      [close(1000)].

    If the browser refuses the URL (a URL that is not valid, or a scheme that is
    not [ws], [wss], [http] or [https]), the driver writes a warning to the
    console, then gives [Failed] and [Closed] with the code 1006 in a later
    task. *)
