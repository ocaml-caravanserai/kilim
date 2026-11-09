(* The state of History_mock, before Page: each page sets the history driver of
   kilim.router at its creation (install). The driver of the last page wins, as
   the error handler of a host. *)

module Dlist = Kilim.Private.Dlist

type t =
  { host: Host.t
  ; mutable entries: string list  (** The oldest first. *)
  ; mutable index: int
  ; mutable loads: string list  (** The newest first. *)
  ; mutable scrolls: int
  ; listeners: (unit -> unit) Dlist.t }

let last = Last_host.make (fun s -> s.host)

let entry s = List.nth s.entries s.index

(* The browser changes the URL, then sends popstate, in a later task. *)
let back s =
  Host.raw s.host (fun () ->
      if s.index > 0 then begin
        s.index <- s.index - 1 ;
        Dlist.iter (fun f -> f ()) s.listeners
      end )

let push s url =
  s.entries <- List.filteri (fun i _ -> i <= s.index) s.entries @ [url] ;
  s.index <- s.index + 1 ;
  s.scrolls <- s.scrolls + 1

let replace s url =
  s.entries <- List.mapi (fun i e -> if i = s.index then url else e) s.entries

let listen s f =
  let node = Dlist.push s.listeners f in
  fun () -> Dlist.remove node

let driver s =
  { Kilim_router.location= (fun () -> entry s)
  ; push= push s
  ; replace= replace s
  ; back= (fun () -> back s)
  ; listen= listen s }

(* A new state, and its driver. *)
let fresh host url =
  let s =
    { host
    ; entries= [url]
    ; index= 0
    ; loads= []
    ; scrolls= 0
    ; listeners= Dlist.create () }
  in
  Last_host.set last s ;
  Kilim_router.Private.set_history (Some (driver s))

(* The state of the host changes in place: the live subscriptions keep it. *)
let set_url host url =
  match Last_host.find last host with
  | Some s ->
      s.entries <- [url] ;
      s.index <- 0
  | None ->
      fresh host url

let install host =
  match Last_host.find last host with
  | Some s ->
      Kilim_router.Private.set_history (Some (driver s))
  | None ->
      fresh host "/"

let of_host host = Last_host.get last host ~mock:"History_mock"
