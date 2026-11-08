(** kilim.http: HTTP requests as tasks, JSON and decoders (design section 13).

    The tasks go through one global transport. In the browser, [main.ml] calls
    [Kilim_http_browser.install ()] (the library kilim.http.browser) before
    [Kilim_browser.run]: then each request is a [fetch]. In the tests of
    kilim.testing, each page has its [Http_mock]. A task is cold: it reads the
    transport and sends its request when it starts, and again at each start. A
    cancel of the task (for example by [Cmd.latest] or an unmount) aborts the
    request. A task gives an HTTP failure as an {!error} in its result: thus the
    app maps it to a message with [Cmd.task]. *)

module Json = Json
module Decode = Decode

type meth = Get | Post | Put | Delete

val string_of_meth : meth -> string
(** ["GET"], ["POST"], ["PUT"] or ["DELETE"]. *)

(** A request. Use ASCII header values: the browsers send a header value as a
    ByteString, and they do not agree for other characters. For U+00E9, Chrome
    and Firefox send one byte 0xE9, and WebKit sends the two bytes of UTF-8. A
    character above U+00FF gives [Network] in the three browsers (HT1 of
    test/dom/test_http.ml). A response header with bytes 0x80 or more probably
    comes back with each byte as one character, encoded again in UTF-8: the
    Fetch standard decodes a header value in this way, but no test examines it.
*)
type request =
  {meth: meth; url: string; headers: (string * string) list; body: string option}

type response =
  { status: int
  ; url: string  (** The URL of the response, after the redirects. *)
  ; headers: (string * string) list  (** The names in lowercase. *)
  ; body: string }

type error =
  | Network of string
      (** The request failed with no response: for example no network, or a
          refused CORS check. The text gives the name and the message of the
          error of the browser, for example ["TypeError: Failed to fetch"]. The
          message depends on the browser. *)
  | Status of {status: int; body: string}  (** A status that is not 2xx. *)
  | Decode of string
      (** The body is not JSON, or not the expected value: the text of
          {!Decode.error_to_string}. *)
  | Timeout  (** The time of [~timeout] ended first. *)

val error_to_string : error -> string
(** ["network TEXT"], ["status N BODY"], ["decode TEXT"] or ["timeout"]. *)

val request :
  ?timeout:Kilim.Span.t -> request -> (response, error) result Kilim.Async.t
(** [request r] sends [r] and gives the response, also for a status that is not
    2xx. The body is read in full. With [~timeout], the end of the time aborts
    the request and gives [Error Timeout]. With no transport, the task fails
    with [Failure]: the error goes to the error scope of the instance. *)

val get :
     ?headers:(string * string) list
  -> ?timeout:Kilim.Span.t
  -> string
  -> (string, error) result Kilim.Async.t
(** [get url] gives the body of a [GET] request to [url]. A status that is not
    2xx gives [Error (Status _)]. *)

val get_json :
     ?headers:(string * string) list
  -> ?timeout:Kilim.Span.t
  -> 'a Decode.t
  -> string
  -> ('a, error) result Kilim.Async.t
(** [get_json d url] is {!get}, then decodes the body with [d]. *)

(** A transport: the browser fetch or a mock. [send] gives [Error text] for a
    request that fails with no response. A cancel of its task aborts the
    request. *)
type transport = {send: request -> (response, string) result Kilim.Async.t}

(** The internals for kilim.http.browser and kilim.testing. They can change at
    any time. *)
module Private : sig
  val set_transport : transport -> unit
  (** The global transport. The last call wins. *)

  val clear : unit -> unit
  (** No transport. *)

  val transport : unit -> transport option
end
