(* The chat example: one room on a WebSocket, with kilim.websocket. After each
   open, the app gets the last messages of the room with kilim.http. The
   nickname is in localStorage, with kilim.storage. An init task reads the
   nickname from localStorage, thus the first render shows it. When the socket
   closes, the app waits, then connects again with a new id. The waits are 1 s,
   2 s, 4 s and so on, at most 30 s. *)

open Kilim
module Ws = Kilim_websocket
module Http = Kilim_http
module Storage = Kilim_storage

type status = Connecting | Open of Ws.conn | Waiting of int

(* [attempt]: the closes since the last open. [id]: the id of the key of the
   socket. [early]: the live messages after the open, until the reply of the
   history. [unsent]: the last send failed. *)
type model =
  { nick: string
  ; draft: string
  ; messages: string list
  ; status: status
  ; attempt: int
  ; id: int
  ; saved: bool
  ; early: string list option
  ; unsent: bool }

type msg =
  | Loaded of (string option, Storage.error) result
  | Nick of string
  | Saved of (unit, Storage.error) result
  | Draft of string
  | Send
  | Sent of (unit, Ws.error) result
  | Socket of Ws.event
  | History of (string list, Http.error) result
  | Retry

(* The wait before the next connection: 1 s, 2 s, 4 s, at most 30 s. *)
let backoff attempt = min 30 (1 lsl min attempt 5)

let init () =
  ( { nick= ""
    ; draft= ""
    ; messages= []
    ; status= Connecting
    ; attempt= 0
    ; id= 0
    ; saved= true
    ; early= None
    ; unsent= false }
  , Cmd.task (Async.map (fun r -> Loaded r) (Storage.get Local "nick")) )

let last n l =
  let k = List.length l - n in
  List.filteri (fun i _ -> i >= k) l

(* The live messages [early] after the history [h]. The server puts a message in
   the history before it sends it to the room. Thus the first live messages can
   be the last messages of the history: the longest start of [early] that is
   also the end of [h] goes. *)
let after h early =
  let rec go k =
    if k = 0 then early
    else if
      List.length h >= k && last k h = List.filteri (fun i _ -> i < k) early
    then List.filteri (fun i _ -> i >= k) early
    else go (k - 1)
  in
  h @ go (List.length early)

let update m = function
  | Loaded (Ok (Some nick)) ->
      ({m with nick}, Cmd.none)
  | Loaded (Ok None | Error _) ->
      (m, Cmd.none)
  | Nick nick ->
      ( {m with nick}
      , Cmd.task (Async.map (fun r -> Saved r) (Storage.set Local "nick" nick))
      )
  | Saved r ->
      ({m with saved= Result.is_ok r}, Cmd.none)
  | Draft draft ->
      ({m with draft}, Cmd.none)
  | Send when m.draft = "" ->
      (m, Cmd.none)
  | Send -> (
    match m.status with
    | Open c ->
        let name = if m.nick = "" then "anonymous" else m.nick in
        ( m
        , Cmd.task
            (Async.map (fun r -> Sent r) (Ws.send c (name ^ ": " ^ m.draft))) )
    | Connecting | Waiting _ ->
        ({m with unsent= true}, Cmd.none) )
  (* The draft stays until the send works. *)
  | Sent (Ok ()) ->
      ({m with draft= ""; unsent= false}, Cmd.none)
  | Sent (Error Not_open) ->
      ({m with unsent= true}, Cmd.none)
  | Socket (Opened c) ->
      ( {m with status= Open c; attempt= 0; early= Some []}
      , Cmd.task
          (Async.map
             (fun r -> History r)
             (Http.get_json Http.Decode.(list string) "/api/history") ) )
  | Socket (Message s) ->
      ( { m with
          messages= last 20 (m.messages @ [s])
        ; early= Option.map (fun l -> l @ [s]) m.early }
      , Cmd.none )
  | Socket (Closed _) ->
      let s = backoff m.attempt in
      ( {m with status= Waiting s; attempt= m.attempt + 1}
      , Cmd.task
          (Async.map (fun () -> Retry) (Async.sleep (Span.ms (s * 1000)))) )
  | Socket Failed ->
      (m, Cmd.none)
  | History (Ok h) ->
      let early = Option.value ~default:[] m.early in
      ({m with messages= last 20 (after h early); early= None}, Cmd.none)
  | History (Error _) ->
      ({m with early= None}, Cmd.none)
  | Retry ->
      ({m with status= Connecting; id= m.id + 1}, Cmd.none)

let subs m = Ws.connect ~id:(string_of_int m.id) "/chat" (fun e -> Socket e)

let status_text = function
  | Connecting ->
      "Connecting"
  | Open _ ->
      "Connected"
  | Waiting s ->
      "Disconnected: a new try in " ^ string_of_int s ^ " s"

let view m =
  El.main
    [ El.h1 [txt "Chat"]
    ; El.p ~at:[At.data "testid" "status"] [txt (status_text m.status)]
    ; El.label
        [ txt "Nickname "
        ; El.input
            ~at:[At.value m.nick; At.aria_label "Nickname"]
            ~ev:[Ev.input (fun s -> Nick s)]
            () ]
    ; ( if m.saved then txt ""
        else El.p ~at:[At.role "alert"] [txt "The nickname is not saved."] )
    ; El.ul
        ~at:[At.aria_label "Messages"]
        (List.map (fun s -> El.li [txt s]) m.messages)
    ; El.form
        ~at:[At.aria_label "Send a message"]
        ~ev:[Ev.submit ~prevent_default:true Send]
        [ El.input
            ~at:[At.value m.draft; At.aria_label "Message"]
            ~ev:[Ev.input (fun s -> Draft s)]
            ()
        ; El.button ~at:[At.type_ "submit"] [txt "Send"] ]
    ; ( if m.unsent then
          El.p ~at:[At.role "alert"] [txt "The message is not sent."]
        else txt "" ) ]

let app = App.make ~init ~update ~view ~subs ()
