(** kilim.browser: the browser part of kilim (docs/design.html).

    This version has the browser host, the real DOM, the listeners, {!run},
    {!Async.of_fut}, {!Unsafe_html.of_trusted} and {!Sub.window}. *)

(** The internals for the tests. They can change at any time. *)
module Private : sig
  val host : Kilim.Private.Host.t
  (** The host of the page: one microtask queue and one [MessageChannel] for all
      apps. A test waits for "one task" with its [post_task]. *)

  val live_timers : unit -> int
  (** The timers of the host that did not run and are not cleared. *)

  val cell : Jv.t -> Kilim.Private.Dom.cell option
  (** The cell of an element, in its property [__kilim]. *)

  (** A started app. *)
  type app

  val start : Brr.El.t -> Kilim.App.t -> app
  (** As {!run}, at once, with the element.

      @raise Invalid_argument if the element has a live app. *)

  val mounted : Brr.El.t -> app option
  (** The live app of an element. A trap of wasm_of_ocaml in the first turn of
      {!start} leaves an app that this finds and that {!stop} can end. *)

  val ops : app -> Jv.t Kilim.Private.Dom.t
  (** The DOM operations of the app. Each app has its own operations and its own
      listener functions. *)

  val runtime : app -> Jv.t Kilim.Private.Runtime.t

  val stop : app -> unit
  (** [Runtime.stop], then removes the listeners of the mount element and the
      [visibilitychange] listener. The other apps of the page do not change. *)

  (** The JS cell of {!Async.of_fut}: a JS object whose property [kilimFut]
      holds a JS function. *)
  module Fut_cell : sig
    type t = Jv.t

    val create : Jv.t -> t
    (** [create k] is a cell that holds the JS function [k]. *)

    val reaction : t -> Jv.t
    (** A JS function [v => ...] that holds only the cell. It takes [k] out of
        the cell, then calls [k(v)]. A cleared cell calls nothing. *)

    val clear : t -> unit

    val is_empty : t -> bool
  end

  val fut_start :
    (Kilim.Cancel.t -> 'a Fut.t) -> Kilim.Cancel.t -> ok:('a -> unit) -> Jv.t
  (** The start function of {!Async.of_fut}: it gives its cell. The end of the
      token clears the cell. *)
end

val run : mount:string -> Kilim.App.t -> unit
(** [run ~mount app] starts [app] in the element with the id [mount], at once if
    the element exists. If not, and the document is "loading", it waits until
    [document.readyState] is not "loading". It removes the children of the
    element, installs the listeners, then renders the app in a first turn (the
    init tasks start at once), and returns. An event that the browser sends
    during the first turn goes to the queue of that turn.

    One listener on the element for each delegated event, and a listener on each
    element with an event of [Ev] that uses one, for example [Ev.mouseenter]. A
    kilim app inside the DOM of another kilim app is not supported. Apps side by
    side work. One element has at most one live app.

    @raise Invalid_argument
      if the element does not exist and the document is not "loading", or if the
      element has a live app. After the wait, a missing element goes to the
      error handler. *)

(** {!Kilim.Unsafe_html}, and the values of Trusted Types (design section 13).
*)
module Unsafe_html : sig
  include module type of struct
    include Kilim.Unsafe_html
  end

  val of_trusted : Jv.t -> t
  (** [of_trusted v] is the [TrustedHTML] value [v] and its HTML text
      [String(v)]. [v] comes from the HTML policy of the app, with its own
      sanitizer: kilim makes no HTML policy. The diff compares the text, and it
      writes [v] to [innerHTML]. Thus a page with
      [require-trusted-types-for 'script'] accepts it. If the page has no
      [trustedTypes] (a browser with no Trusted Types), the value is
      [Unsafe_html.of_string (String(v))]: thus a small polyfill that gives
      strings works.

      @raise Invalid_argument
        if [trustedTypes] exists and [v] is not a [TrustedHTML]. *)
end

(** The tasks of {!Kilim.Async}, and the tasks from Brr futures. *)
module Async : sig
  include module type of struct
    include Kilim.Async
  end

  val of_fut : (Kilim.Cancel.t -> 'a Fut.t) -> 'a t
  (** [of_fut make] is a task that calls [make tok] when it starts, with the
      token [tok] of its step, and gives the value of the future. The task is
      cold: [make] runs at each start of the task, not when you make the value.
      An exception from [make] is the error of the task. Most Brr bindings
      (Fetch, for example) give a [Fut.or_error]: use {!of_fut_or_error}. Use
      [of_fut] only for a [Fut.t] that cannot fail.

      [make] can register an end action on [tok] (see {!Kilim.Async.make}), for
      example the abort of a request:

      {@ocaml in=test/doc/fetch_abort.ml[
      let fetch_text url : string Kilim_browser.Async.t =
        Kilim_browser.Async.map Jstr.to_string
          (Kilim_browser.Async.of_fut_or_error (fun tok ->
               let ctrl = Brr.Abort.controller () in
               Kilim.Cancel.on_end tok (fun () -> Brr.Abort.abort ctrl) ;
               let init =
                 Brr_io.Fetch.Request.init ~signal:(Brr.Abort.signal ctrl) ()
               in
               Fut.bind
                 (Brr_io.Fetch.url ~init (Jstr.v url))
                 (function
                   | Error e ->
                       Fut.error e
                   | Ok r ->
                       Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body r) ) )
          )
      ]}

      [on_end] also runs at the normal end of the step, and an abort after the
      settle has no effect.

      A JS promise cannot be cancelled: after a cancel or an unmount, a future
      that settles later gives nothing. A future that never settles keeps the JS
      cell of the task, which the end of the step empties, and its reaction. In
      the release profile of js_of_ocaml, the reaction can also keep the
      internal continuation of the step (its token, the host and the run queue),
      but not the model: the model is free in the two builds (design section
      12). The value comes in a later microtask through JS promises. Do not
      depend on the order of this value against other promises.

      A raise inside the future chain of [make] (for example in a [Fut.map])
      gives an unhandled rejection in the browser, as in Brr itself, and the
      task stays pending until a cancel or an unmount, which empties the cell.
      Keep code that can raise out of the callbacks of a future, or use a
      [Fut.or_error] with an explicit [Error]. *)

  val of_fut_or_error : (Kilim.Cancel.t -> 'a Fut.or_error) -> 'a t
  (** As {!of_fut}, for a future that can fail: [Error e] fails the task with
      [Jv.Error e]. *)
end

(** The subscriptions of {!Kilim.Sub}, and the subscriptions of the browser. *)
module Sub : sig
  include module type of struct
    include Kilim.Sub
  end

  val window : string -> (Brr.Ev.Type.void Brr.Ev.t -> 'msg) -> 'msg t
  (** [window name f] listens to the event [name] on [window], with a passive
      listener. The key comes from [name]: two with the same name get their
      position among them in the key. Each event is a payload, and [f] makes the
      message when the payload leaves the queue, with the newest [f] of the key.
      The event runs a microtask turn: the render waits for the next frame.

      An app module that uses [window] links only in the browser builds. To test
      such an app with kilim.testing, give the window subscriptions to the app
      as a parameter. *)
end
