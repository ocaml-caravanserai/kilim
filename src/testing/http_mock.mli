(** The transport of kilim.http on the virtual host (design section 14).

    Each page starts with an empty mock: a request of an init task that comes
    before the test gives its reply goes to {!unmatched}. The test gives a reply
    for a method and a URL. The reply comes as a foreign macrotask of the host
    (see {!Host.raw}) after [~delay] (default 0): thus {!Page.advance} or
    {!Page.run} delivers it. A request with no reply fails at the next
    {!Page.run} with [Network "Http_mock: no reply for GET /x"].

    URLs: a relative URL resolves against the page URL [http://localhost/], for
    the routes and for the requests. Thus the route ["/api/x"] matches a request
    for ["http://localhost/api/x"]. The mock does not remove dot segments.

    The mock of a page stops at {!Page.stop}. After the stop, {!requests},
    {!pending}, {!aborted}, {!unmatched} and {!expect_done} still work: for
    example, a test can make sure that the stop aborted a request in flight.
    kilim.http has one global transport: the mock of the last host that started
    a page gets the requests. Thus a test with requests must not run in parallel
    with another test in a second domain, and two live pages with requests need
    one host. *)

(** A reply. [`Json] and [`Text] give the status 200 with the body.
    [`Status (n, body)] gives the status [n]. [`Network text] fails with
    [Network text]. [`Never] gives no reply: the request stays in flight until a
    cancel. [`Response r] gives [r]. *)
type reply =
  [ `Json of string
  | `Text of string
  | `Status of int * string
  | `Network of string
  | `Never
  | `Response of Kilim_http.response ]

val on :
     'out Page.t
  -> ?delay:Kilim.Span.t
  -> ?once:bool
  -> Kilim_http.meth
  -> string
  -> reply
  -> unit
(** [on page meth url r] gives [r] to each request with the method [meth] and
    the URL [url]. If two replies match a request, the older one gives the
    reply. With [~once:true], the mock removes the reply after its first
    request.

    @raise Invalid_argument
      if the page is stopped, or if a page of another host started after it. *)

val on_get :
  'out Page.t -> ?delay:Kilim.Span.t -> ?once:bool -> string -> reply -> unit
(** {!on} for [GET]. *)

val on_post :
  'out Page.t -> ?delay:Kilim.Span.t -> ?once:bool -> string -> reply -> unit
(** {!on} for [POST]. *)

val requests : 'out Page.t -> Kilim_http.request list
(** The requests that started, the oldest first, also the ones with no reply. *)

val pending : 'out Page.t -> int
(** The requests in flight: not replied and not cancelled. *)

val aborted : 'out Page.t -> int
(** The requests that a cancel aborted in flight. *)

val unmatched : 'out Page.t -> Kilim_http.request list
(** The requests that had no reply, the oldest first. *)

val expect_done : 'out Page.t -> unit
(** @raise Failure if a request is in flight or had no reply. *)
