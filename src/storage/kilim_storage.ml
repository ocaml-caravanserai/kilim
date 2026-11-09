type area = Local | Session

type error = Unavailable | Quota_exceeded | Other of string

let error_to_string = function
  | Unavailable ->
      "unavailable"
  | Quota_exceeded ->
      "quota exceeded"
  | Other name ->
      "other " ^ name

type driver =
  { get: area -> string -> (string option, error) result
  ; set: area -> string -> string -> (unit, error) result
  ; remove: area -> string -> (unit, error) result }

(* The one driver of the program, as the error handler of the core. *)
let current : driver option ref = ref None

let driver () =
  match !current with
  | Some d ->
      d
  | None ->
      failwith
        "kilim.storage: no driver: call Kilim_storage_browser.install () in \
         main.ml, or use Storage_mock in tests"

(* The driver is read when the task starts, and the result comes at once. *)
let task f = Kilim.Async.make (fun _ ~ok ~error:_ -> ok (f (driver ())))

let get area key = task (fun d -> d.get area key)

let set area key value = task (fun d -> d.set area key value)

let remove area key = task (fun d -> d.remove area key)

module Private = struct
  let set_driver d = current := d

  let driver () = !current
end
