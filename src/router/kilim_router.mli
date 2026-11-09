(** kilim.router: the routes of an app in the path of the URL, with the History
    API (design section 06).

    The app keeps the route in its model. [parse] gives a route of the app from
    a {!Kilim.Url.location}, and [print] is its inverse. A route that the app
    does not know must be a case of the route type, for example [Unknown]. The
    usual flow:
    - [init] gives [Cmd.task (Async.map routed (Router.current r))]: the start
      route, with no read of the URL in [init].
    - [subs] gives [Router.changes r routed]: a message for each change of the
      URL, also for a push of a nested component.
    - A link is [Router.link r ~on:go route [...]], and [update] gives
      [Cmd.task (Async.map pushed (Router.push r route))] for [go route]. Then
      [changes] gives the new route.

    The effects are tasks with a result ([unit Async.t]): the app maps them to a
    message with [Cmd.task]. The router reads the history driver when a task or
    a subscription starts. [main.ml] sets the browser driver with
    [Kilim_router_browser.install ()], and each page of kilim.testing sets the
    driver of [History_mock]. With no driver, a task fails with [Failure]: the
    error goes to the error scope of the instance. *)

(** The history driver: the analog of the host of the core. Plain OCaml
    functions, with no type of Brr. *)
type history =
  { location: unit -> string
        (** The path, the query and the fragment of the URL of the document, for
            example ["/users/42?tab=a#top"]. *)
  ; push: string -> unit
        (** [push url] adds [url] to the history and makes it the URL of the
            document. Then it scrolls the window to the top. [url] is a path
            from {!href}. *)
  ; replace: string -> unit
        (** [replace url] makes [url] the URL of the document, with no new entry
            in the history and no scroll. *)
  ; back: unit -> unit
        (** One step back in the history. The change of the URL and the
            [popstate] event come later. *)
  ; listen: (unit -> unit) -> unit -> unit
        (** [listen f] calls [f] at each [popstate] event, and gives the
            function that stops the calls. *) }

(** A router for the routes of type ['r]. *)
type 'r t

val make :
     ?base:string
  -> parse:(Kilim.Url.location -> 'r)
  -> print:('r -> Kilim.Url.location)
  -> unit
  -> 'r t
(** [make ~base ~parse ~print ()] is a router for the paths below [base], for
    example ["/app"] (default [""], the root of the site). [base] is a path in
    the form of the URL: it is not encoded again. A URL below [base] gives
    [parse] its location with no [base]: thus ["/app/users/42"] gives the path
    [["users"; "42"]], and ["/app"] and ["/app/"] give the path [[]].

    A URL that is not below [base] gives [parse] its full location, with the
    base in the path. For example, with the base ["/app"], the URL ["/"] gives
    the path [[]], and ["/apple"] gives the path [["apple"]]. Thus [parse] can
    give a route for a URL of another part of the site.

    [print] must give no ["."] or [".."] segment (see {!Kilim.Url.to_string}):
    the browser resolves it, and the URL can go out of [base]. [History_mock]
    does not resolve these segments.

    @raise Invalid_argument
      if [base] is not [""] or ["/"] and it does not start with exactly one [/],
      or it ends with [/], or it has a character outside [A-Za-z0-9-._~/]. *)

val href : 'r t -> 'r -> string
(** [href r route] is [base] and [Kilim.Url.to_string (print route)], for
    example ["/app/users/42"]. The path of the route [[]] is ["/app/"]. *)

val current : 'r t -> 'r Kilim.Async.t
(** [current r] gives the route of the URL of the document. The result is
    synchronous: in [init] of the App, the first render already shows the route.
    In [init] of a nested component, the result comes in the same turn, after
    the first render: thus the turn renders two times. *)

val push : 'r t -> 'r -> unit Kilim.Async.t
(** [push r route] adds [href r route] to the history, makes it the URL of the
    document and scrolls the window to the top. Then each live {!changes}
    subscription gets the new route, in the same turn. A push of the URL of the
    document gives no message. The URL text decides: a push of the route of the
    base root changes ["/app"] to ["/app/"], and gives a second message with the
    same route. *)

val replace : 'r t -> 'r -> unit Kilim.Async.t
(** [replace r route] is as {!val-push}, but with no new entry in the history
    and no scroll. *)

val back : unit -> unit Kilim.Async.t
(** [back ()] is one step back in the history. The task gives its result at
    once. The [popstate] event comes later: then {!changes} gives the route. *)

val changes : 'r t -> ('r -> 'msg) -> 'msg Kilim.Sub.t
(** [changes r f] gives [f route] for each change of the URL of the document:
    each [popstate] event (the buttons back and forward of the browser, and
    {!val-back}), and each {!val-push} and {!val-replace} of a router of the
    page. Each subscription keeps the last URL that it gave, and gives no
    message for the same URL two times. The key comes from [base].

    A subscription starts at a render, thus after the init tasks of the turn.
    When it starts, it gives the route of the URL if a {!val-push} or a
    {!val-replace} changed the URL after the last route that the program got
    (from {!current} or from a message of [changes]), or if the URL is not the
    URL of that route. A subscription that stops and starts with no change of
    the URL gives no message. Thus a redirect in [update] (for example a
    [replace] for an unknown route of [current]) and a push in [init] give a
    message. A new history driver starts with no change.

    A message of a [popstate] event is not discrete: the runtime renders it at
    the next frame, or after 100 ms. A message of a {!val-push} in a click
    renders in the turn of the click. *)

val link :
     'r t
  -> ?at:Kilim.At.t list
  -> on:('r -> 'msg)
  -> 'r
  -> 'msg Kilim.View.t list
  -> 'msg Kilim.View.t
(** [link r ~at ~on route children] is [El.a] with [At.href (href r route)], the
    attributes [at], and [Ev.link_click (on route)]. A plain click gives the
    message [on route], and the browser does not follow the link. A click with a
    modifier key or the middle button gives no message: the browser opens the
    URL, for example in a new tab. Do not put a [href] in [at]. *)

(**/**)

(** The internals for the drivers and the tests. They can change at any time. *)
module Private : sig
  val set_history : history option -> unit
  (** [set_history (Some h)] makes [h] the history driver of the program, and
      [set_history None] removes the driver. A task or a subscription that
      started before keeps its driver. *)

  val history : unit -> history option
  (** The history driver, if there is one. *)

  val listeners : unit -> int
  (** The live {!changes} subscriptions, for the leak tests. *)
end

(**/**)
