(** kilim.websocket: a WebSocket as a subscription (design section 08).

    {!connect} is a subscription: its key comes from the URL and [~id]. A new
    key opens a socket, and a removed key (a change of the model, an unmount)
    closes it. The events of the socket come as messages: {!Opened} gives the
    connection, and the app keeps it in its model to {!send} text and to
    {!close} the socket. The effects are tasks with a result: the app maps them
    to a message with [Cmd.task].

    In the browser, [main.ml] calls [Kilim_websocket_browser.install ()] (the
    library kilim.websocket.browser) before [Kilim_browser.run]. In the tests of
    kilim.testing, each page has its [Websocket_mock]. With no driver, the
    subscription fails with [Failure]: the error goes to the error scope of the
    instance.

    The library does not connect again by itself. After {!Closed}, the app waits
    (for example with [Async.sleep]), then gives a new [~id]: the new key opens
    a new socket. After {!Closed}, the key stays live until the app changes
    [~id] or removes the subscription. The library sends and gets only text
    frames. The browser driver drops a binary frame with a warning on the
    console. *)

(** The close event of a socket: the code and the reason of the close frame, and
    [clean] if the close handshake completed. A connection that failed gives the
    code 1006 with [clean] false. *)
type close = {code: int; reason: string; clean: bool}

(** A connection. Valid from {!Opened} until {!Closed} or the end of the key.
    Keep it in the model, and remove it at {!Closed}. *)
type conn

type event =
  | Opened of conn  (** The socket is open: {!send} can send text. *)
  | Message of string  (** A text frame from the server. *)
  | Closed of close  (** The socket closed. No event comes after it. *)
  | Failed
      (** The connection failed, for example the server refused it or the URL is
          not valid. {!Closed} comes after it. Failed can also come after
          {!Opened}. Connect again on {!Closed} only. *)

val connect : ?id:string -> string -> (event -> 'msg) -> 'msg Kilim.Sub.t
(** [connect ~id url f] opens a socket to [url] when its key starts, and gives
    [f e] for each event [e] of the socket. The key comes from [url] and [id]
    (default [""]): change [id] to connect again. A removed key closes the
    socket with the code 1000, and no event comes after it. A key that stays
    keeps its socket and uses the newest [f].

    The browser driver resolves a relative [url] against the URL of the page,
    and changes the scheme [http] to [ws] and [https] to [wss]. *)

type error = Not_open  (** The socket is not open. *)

val error_to_string : error -> string
(** ["not open"]. *)

val send : conn -> string -> (unit, error) result Kilim.Async.t
(** [send c text] sends [text] as a text frame. After a close and after the end
    of the key, the task gives [Error Not_open] and sends nothing. *)

val close : conn -> unit Kilim.Async.t
(** [close c] starts the close handshake with the code 1000. Then {!Closed}
    comes, with the code of the server. A close of a socket that is not open
    does nothing. *)

(** The functions of a socket, from a driver. *)
type socket =
  { send: string -> bool
        (** [send text] sends [text] if the socket is open, and gives [true]. If
            not, it gives [false] and sends nothing. *)
  ; close: unit -> unit
        (** [close ()] starts the close handshake with the code 1000. The events
            continue. *)
  ; release: unit -> unit
        (** Stop the events, then close the socket with the code 1000 if it is
            open or connecting. *) }

(** The functions that a driver calls for the events of a socket. *)
type handlers =
  { opened: unit -> unit
  ; message: string -> unit
  ; closed: close -> unit
  ; failed: unit -> unit }

(** A driver: the analog of the host of the core. Plain OCaml functions, with no
    type of Brr. [connect url h] opens a socket to [url], and calls the
    functions of [h] for its events, each one in a later task. *)
type driver = {connect: string -> handlers -> socket}

(** The internals for the drivers and the tests. They can change at any time. *)
module Private : sig
  val set_driver : driver option -> unit
  (** [set_driver (Some d)] makes [d] the driver of the program, and
      [set_driver None] removes the driver. A subscription that started before
      keeps its socket. *)

  val driver : unit -> driver option

  val none : socket
  (** A socket that is not open: [send] gives [false], and [close] and [release]
      do nothing. *)

  val ws_scheme : string -> string
  (** [ws_scheme url] changes the scheme [http:] of [url] to [ws:], and [https:]
      to [wss:]. Another URL does not change. *)

  val live : unit -> int
  (** The live subscriptions of {!connect}, for the leak tests. The count
      decreases at the end of the key, not at the close event. *)
end
