(** An app on the fake DOM and the virtual host (design section 14), with
    simulated events.

    A simulated event finds its target, then calls the handlers from the target
    to the top with {!Kilim.Private.Event.walk}, all in one turn. The turn is
    discrete for a discrete event (for example [click]). After each top-level
    call, the host runs the microtasks. *)

type t

(** A query found no element, or more than one. *)
exception Query of string

val start : ?host:Host.t -> Kilim.App.t -> t
(** Mounts the app in a new [div] element. [host] defaults to a new host. *)

val host : t -> Host.t

val doc : t -> Fake_dom.t

val root : t -> Fake_dom.node
(** The mount element. *)

val html : t -> string
(** [Fake_dom.to_html] of the mount element. *)

val find : t -> By.t -> Fake_dom.node
(** @raise Query if no element or more than one element matches. *)

val click : t -> By.t -> unit
(** A [click] event: a discrete turn. *)

val focus : t -> By.t -> unit
(** A [focusin] event: a [Macro] turn. *)

val set_hidden : t -> bool -> unit
(** Sets the hidden flag of the host, then tells the runtime. *)

val advance : t -> Kilim.Span.t -> unit

val run : t -> unit

val stats : t -> Kilim.Private.Runtime.stats

val inspect : t -> Kilim.Private.Runtime.inspect

val stop : t -> unit

val expect_no_tasks : t -> unit
(** @raise Failure if a timer is live or the token of the root has a child. *)
