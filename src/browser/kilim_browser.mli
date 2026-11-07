(** kilim.browser: the browser part of kilim (docs/design.html).

    This version has the browser host, the real DOM, the listeners and {!run}.
*)

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
