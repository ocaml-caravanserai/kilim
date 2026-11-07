(** An app on the fake DOM and the virtual host (design section 14), with
    simulated events.

    A simulated event finds its target, then calls the handlers from the target
    to the top with {!Kilim.Private.Event.walk}, all in one turn. The payload
    comes from the target: its live [value], its live [checked], or the key of
    {!keydown}. The turn is discrete for a discrete event ([click], [input],
    [change], [submit] and [keydown]). A discrete event also gives the runtime
    its restore: after the next render, the restore writes the view values of
    the target, and of the other buttons of its radio group, where the live
    property is different (design section 11). It does nothing for a target that
    is not under the mount element. After each top-level call, the host runs the
    microtasks. *)

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
(** A [click] event: a discrete turn. As in a browser, a click on a checkbox
    first toggles its [checked], and a click on a radio button first checks it
    (and unchecks its group). If a handler prevents the default, the click
    undoes this change after the turn, as a browser does after the dispatch: the
    checkbox toggles back, and the radio button that was checked before becomes
    checked again. *)

val type_ : t -> By.t -> string -> unit
(** [type_ t q s] types each character of [s]: the live value becomes the live
    value and the character, then an [input] event. One turn for each character.
*)

val input : t -> By.t -> string -> unit
(** [input t q v] sets the live value to [v], then sends one [input] event. *)

val change : t -> By.t -> string -> unit
(** [change t q v] sets the live value to [v], then sends a [change] event. *)

val submit : t -> By.t -> unit
(** A [submit] event. *)

val keydown : t -> By.t -> string -> unit
(** [keydown t q key] sends a [keydown] event with this [key], for example
    ["Enter"]. *)

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
