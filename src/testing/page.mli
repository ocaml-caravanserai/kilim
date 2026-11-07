(** An app on the fake DOM and the virtual host (design section 14), with
    simulated events.

    A simulated event finds its target, then calls
    {!Kilim.Private.Event.dispatch}: the same rules as in a browser. The
    handlers run from the target to the top, all in one turn. The payload comes
    from the target: its live [value], its live [checked], or the key of
    {!keydown}. The turn is discrete for a discrete event ([click], [input],
    [change], [submit] and [keydown]). A discrete event also gives the runtime
    its restore: after the next render, the restore writes the view values of
    the target, and of the other buttons of its radio group, where the live
    property is different (design section 11). It does nothing for a target that
    is not under the mount element.

    After each top-level call, the page runs the microtasks, then the undo of a
    prevented click, then the tasks of the dispatch (for example the restore
    task after a click on a checkbox), each with the microtasks after it. This
    is the order of a browser. *)

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
    undoes this change after the turn and the microtasks, as a browser does
    after the dispatch: the checkbox toggles back. The radio button that was
    checked before becomes checked again, if it is still in the group of the
    target. If not, the target stays checked, as in Chrome. Then a task restores
    the target from the view. *)

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

val keydown : t -> ?composing:bool -> By.t -> string -> unit
(** [keydown t q key] sends a [keydown] event with this [key], for example
    ["Enter"]. With [~composing:true] ([isComposing] of the event, default
    [false]), no handler runs. *)

val keyup : t -> ?composing:bool -> By.t -> string -> unit
(** As {!keydown}, for [keyup]: a [Macro] turn. *)

val focus : t -> By.t -> unit
(** Gives the focus of the document to the target (see {!Fake_dom.focus}), then
    sends a [focusin] event: a [Macro] turn. On an element with an IME
    composition, it ends the composition and gives a discrete turn with the
    restore. It sends no [focusout] to the element that had the focus. *)

val blur : t -> By.t -> unit
(** Takes the focus away from the target if it has the focus, then sends a
    [focusout] event: a [Macro] turn. On an element with an IME composition, it
    ends the composition and gives a discrete turn with the restore. *)

val compose : t -> ?order:[`Chrome | `Firefox] -> By.t -> string list -> unit
(** [compose t ~order q texts] is an IME composition. It gives the focus to the
    target with no event, then sends [compositionstart]. For each text, the live
    value becomes the start value and the text, then an [input] event runs. The
    last text is the commit. One task for each event, except the commit of
    [`Firefox]. The two orders:
    - [`Chrome] (the default): an [input] event with [isComposing] for each
      text, then [compositionend].
    - [`Firefox]: an [input] event with [isComposing] for each text before the
      last. Then in one task: the live value of the commit, [compositionend],
      then an [input] event with [isComposing] false.

    During the composition, the diff does not write [value] while the target has
    the focus. [compositionend] runs its handlers in a [Macro] turn, which does
    not render, and it posts a task that restores the target. In the order of
    Firefox, the last [input] event renders first. *)

val event : t -> By.t -> string -> unit
(** [event t q name] sends the event [name] with the payloads of the target and
    the key [""], for example [mouseenter], [scroll], [wheel] or
    [compositionstart]. *)

val set_hidden : t -> bool -> unit
(** Sets the hidden flag of the host, then tells the runtime. *)

val advance : t -> Kilim.Span.t -> unit

val run : t -> unit

val stats : t -> Kilim.Private.Runtime.stats

val inspect : t -> Kilim.Private.Runtime.inspect

val stop : t -> unit

val expect_no_tasks : t -> unit
(** @raise Failure if a timer is live or the token of the root has a child. *)
