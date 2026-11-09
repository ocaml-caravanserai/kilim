(* The state of Websocket_mock, before Page: each page sets the driver of
   kilim.websocket at its creation (install). The driver of the last host wins,
   as the error handler of a host. Each event of a socket is a raw macrotask of
   the host. *)

module W = Kilim_websocket

type phase = Connecting | Open | Closed

type conn = {h: W.handlers; mutable phase: phase}

type server =
  { host: Host.t
  ; url: string
  ; mutable conns: conn list  (** The newest first. *)
  ; mutable received: string list  (** The newest first. *) }

type t = {host: Host.t; mutable servers: server list}

let last = Last_host.make (fun (s : t) -> s.host)

(* The URL against the page URL http://localhost/, with the scheme http changed
   to ws, as the browser driver does. *)
let resolve url = W.Private.ws_scheme (Http_mock_state.resolve url)

let server s url =
  let url = resolve url in
  match
    List.find_opt (fun (x : server) -> String.equal x.url url) s.servers
  with
  | Some x ->
      x
  | None ->
      let x = {host= s.host; url; conns= []; received= []} in
      s.servers <- x :: s.servers ;
      x

(* Each connection of [x] in a phase of [p], the oldest first, in a raw
   macrotask. *)
let each (x : server) p f =
  Host.raw x.host (fun () ->
      List.iter (fun c -> if p c.phase then f c) (List.rev x.conns) )

let accept x =
  each x
    (fun p -> p = Connecting)
    (fun c ->
      c.phase <- Open ;
      c.h.opened () )

let push x text = each x (fun p -> p = Open) (fun c -> c.h.message text)

(* A failed connection: Failed, then Closed with the code 1006. *)
let failure c =
  c.phase <- Closed ;
  c.h.failed () ;
  c.h.closed {code= 1006; reason= ""; clean= false}

let fail x = each x (fun p -> p <> Closed) failure

(* The server refuses a connection that is not open. *)
let drop x close =
  each x
    (fun p -> p <> Closed)
    (fun c ->
      if c.phase = Open then begin
        c.phase <- Closed ;
        c.h.closed close
      end
      else failure c )

let connections (x : server) =
  List.length (List.filter (fun c -> c.phase <> Closed) x.conns)

let connect s url h =
  let x = server s url in
  let c = {h; phase= Connecting} in
  x.conns <- c :: x.conns ;
  { W.send=
      (fun text ->
        c.phase = Open
        && begin
          x.received <- text :: x.received ;
          true
        end )
  ; close=
      (fun () ->
        if c.phase = Open then begin
          c.phase <- Closed ;
          Host.raw x.host (fun () ->
              h.closed {code= 1000; reason= ""; clean= true} )
        end )
  ; release=
      (fun () ->
        c.phase <- Closed ;
        (* The handlers reach the message function, thus the model. *)
        x.conns <- List.filter (fun y -> y != c) x.conns ) }

let install host =
  let s = Last_host.find_or last host (fun () -> {host; servers= []}) in
  W.Private.set_driver (Some {W.connect= connect s})

let of_host host = Last_host.get last host ~mock:"Websocket_mock"
