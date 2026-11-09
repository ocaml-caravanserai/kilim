(** The driver of kilim.websocket on the virtual host (design section 14).

    Each page sets this mock as the driver of kilim.websocket when it starts. A
    {!type-server} is the side of the server for one URL: it gets each
    connection to the URL, also the connections that started before the call. A
    new connection stays in the state CONNECTING until {!accept}. Each event
    comes as a raw macrotask of the host (see {!Host.raw}): run the host, for
    example with {!Page.run}, to get it. The phase of a connection changes when
    its event runs.

    URLs: a relative URL resolves against the page URL [http://localhost/], and
    the scheme [http] becomes [ws]. Thus [server page "/chat"] gets the
    connections to ["/chat"] and to ["ws://localhost/chat"].

    The driver of the last host that started a page is the driver of the
    program: thus tests that use WebSocket must not run in parallel domains.
    After {!Page.stop}, the reads of a server ({!received}, {!connections})
    still work. *)

type server

val server : 'out Page.t -> string -> server
(** [server page url] is the server of [url] on the host of [page].

    Raises [Failure] if a page of another host started after [page]. *)

val accept : server -> unit
(** Each connection in the state CONNECTING opens: [Opened]. *)

val push : server -> string -> unit
(** [push s text] sends [text] to each open connection: [Message text]. *)

val drop : server -> Kilim_websocket.close -> unit
(** [drop s c] closes each open connection: [Closed c]. Each connection in the
    state CONNECTING fails: [Failed], then [Closed] with the code 1006. *)

val fail : server -> unit
(** Each connection in the state CONNECTING or OPEN fails, for example by a
    protocol error: [Failed], then [Closed] with the code 1006. *)

val received : server -> string list
(** The texts that the connections sent, the oldest first. *)

val connections : server -> int
(** The connections in the state CONNECTING or OPEN. A close of the client and
    the end of a key close a connection at once, with no wait for an event. *)
