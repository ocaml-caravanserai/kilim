(** The chat example: one room on a WebSocket, with kilim.websocket, kilim.http
    and kilim.storage.

    The app connects to ["/chat"] and shows each message of the room. After each
    open, it gets the last 20 messages from ["/api/history"]. The message of the
    form goes to the room as ["NICK: TEXT"]. The nickname is in localStorage at
    the key ["nick"]: an init task reads it, and each change writes it. When the
    socket closes, the app waits 1 s, 2 s, 4 s and so on, at most 30 s, then
    connects again with a new id. An open sets the wait back to 1 s.

    A live message that comes before the reply of the history stays after the
    history, with no copy of a message that the history has. The draft stays
    until the send works. A send with no open socket shows "The message is not
    sent.". *)

val backoff : int -> int
(** [backoff n] is the wait in seconds after [n] closes with no open between
    them: 1, 2, 4, 8, 16, then 30. *)

val app : Kilim.App.t
