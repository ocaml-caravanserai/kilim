type close = {code: int; reason: string; clean: bool}

type socket = {send: string -> bool; close: unit -> unit; release: unit -> unit}

type handlers =
  { opened: unit -> unit
  ; message: string -> unit
  ; closed: close -> unit
  ; failed: unit -> unit }

type driver = {connect: string -> handlers -> socket}

(* The socket of a connection. The cell is set after the driver gives the
   socket: the driver calls no handler before it gives the socket. *)
type conn = {mutable socket: socket}

type event = Opened of conn | Message of string | Closed of close | Failed

type error = Not_open

(* The one driver of the program, as the error handler of the core. *)
let current : driver option ref = ref None

let live = ref 0

let none = {send= (fun _ -> false); close= ignore; release= ignore}

let driver () =
  match !current with
  | Some d ->
      d
  | None ->
      failwith
        "kilim.websocket: no driver: call Kilim_websocket_browser.install () \
         in main.ml, or use Websocket_mock in tests"

let kind : event Kilim.Private.Sub.kind = Kilim.Private.Sub.kind ()

(* The task of a subscription never ends by itself: the cancel of its key
   releases the socket. A top-level function, thus the closures keep only
   [deliver] and the connection. *)
let start url deliver =
  Kilim.Async.make (fun tok ~ok:_ ~error:_ ->
      let d = driver () in
      let c = {socket= none} in
      c.socket <-
        d.connect url
          { opened= (fun () -> deliver (Opened c))
          ; message= (fun m -> deliver (Message m))
          ; closed= (fun x -> deliver (Closed x))
          ; failed= (fun () -> deliver Failed) } ;
      incr live ;
      (* After the end of the key, the connection holds no closure of the
         driver: a connection that the app keeps does not keep the instance, and
         a send gives Not_open. *)
      Kilim.Cancel.on_end tok (fun () ->
          decr live ;
          c.socket.release () ;
          c.socket <- none ) )

(* The length of [id] first: two keys with different [id] or [url] never
   collide. *)
let connect ?(id = "") url f =
  Kilim.Private.Sub.auto kind
    ~params:(string_of_int (String.length id) ^ ":" ^ id ^ url)
    ~start:(start url) f

let send c text =
  Kilim.Async.make (fun _ ~ok ~error:_ ->
      ok (if c.socket.send text then Ok () else Error Not_open) )

let close c = Kilim.Async.make (fun _ ~ok ~error:_ -> c.socket.close () ; ok ())

(* [url] with the scheme http changed to ws and https to wss. *)
let ws_scheme u =
  let swap p r =
    if String.starts_with ~prefix:p u then
      Some
        (r ^ String.sub u (String.length p) (String.length u - String.length p))
    else None
  in
  match swap "http:" "ws:" with
  | Some u ->
      u
  | None ->
      Option.value ~default:u (swap "https:" "wss:")

module Private = struct
  let none = none

  let ws_scheme = ws_scheme

  let set_driver d = current := d

  let driver () = !current

  let live () = !live
end
