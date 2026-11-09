(* The state of Storage_mock, before Page: each page sets the driver of
   kilim.storage at its creation (install). The driver of the last host wins, as
   the error handler of a host. *)

module S = Kilim_storage

type t =
  { host: Host.t
  ; mutable local: (string * string) list
  ; mutable session: (string * string) list
  ; mutable blocked: bool }

let last = Last_host.make (fun s -> s.host)

let items s = function S.Local -> s.local | S.Session -> s.session

let put s area l =
  match area with S.Local -> s.local <- l | S.Session -> s.session <- l

(* [f] if the storage is not blocked. *)
let use s f = if s.blocked then Error S.Unavailable else Ok (f ())

let driver s =
  { S.get= (fun area key -> use s (fun () -> List.assoc_opt key (items s area)))
  ; set=
      (fun area key value ->
        use s (fun () ->
            put s area ((key, value) :: List.remove_assoc key (items s area)) ) )
  ; remove=
      (fun area key ->
        use s (fun () -> put s area (List.remove_assoc key (items s area))) ) }

let of_host_or_fresh host =
  Last_host.find_or last host (fun () ->
      {host; local= []; session= []; blocked= false} )

let preset host area l = put (of_host_or_fresh host) area l

let install host = S.Private.set_driver (Some (driver (of_host_or_fresh host)))

let of_host host = Last_host.get last host ~mock:"Storage_mock"
