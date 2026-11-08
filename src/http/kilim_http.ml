module Json = Json
module Decode = Decode
module Async = Kilim.Async

type meth = Get | Post | Put | Delete

let string_of_meth = function
  | Get ->
      "GET"
  | Post ->
      "POST"
  | Put ->
      "PUT"
  | Delete ->
      "DELETE"

type request =
  {meth: meth; url: string; headers: (string * string) list; body: string option}

type response =
  {status: int; url: string; headers: (string * string) list; body: string}

type error =
  | Network of string
  | Status of {status: int; body: string}
  | Decode of string
  | Timeout

let error_to_string = function
  | Network m ->
      "network " ^ m
  | Status {status; body} ->
      "status " ^ string_of_int status ^ " " ^ body
  | Decode m ->
      "decode " ^ m
  | Timeout ->
      "timeout"

type transport = {send: request -> (response, string) result Async.t}

let current : transport option ref = ref None

module Private = struct
  let set_transport t = current := Some t

  let clear () = current := None

  let transport () = !current
end

let no_transport () =
  Failure
    "kilim.http: no transport: call Kilim_http_browser.install () in main.ml, \
     or use Http_mock in tests"

(* The transport is read when the task starts, not when the app makes the
   task. *)
let send r =
  Async.bind (Async.return ()) (fun () ->
      match !current with
      | None ->
          Async.fail (no_transport ())
      | Some t ->
          Async.map
            (function Ok x -> Ok x | Error m -> Error (Network m))
            (t.send r) )

let request ?timeout r =
  match timeout with
  | None ->
      send r
  | Some span ->
      Async.map
        (function Ok x -> x | Error `Timeout -> Error Timeout)
        (Async.timeout span (send r))

let body_2xx (r : response) =
  if r.status >= 200 && r.status < 300 then Ok r.body
  else Error (Status {status= r.status; body= r.body})

let get ?(headers = []) ?timeout url =
  Async.map
    (fun r -> Result.bind r body_2xx)
    (request ?timeout {meth= Get; url; headers; body= None})

let decode d body =
  match Decode.decode_string d body with
  | Ok x ->
      Ok x
  | Error e ->
      Error (Decode (Decode.error_to_string e))

let get_json ?headers ?timeout d url =
  Async.map (fun r -> Result.bind r (decode d)) (get ?headers ?timeout url)
