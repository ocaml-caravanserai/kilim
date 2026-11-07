(** The event walk (design section 11), generic over the node. The fake DOM of
    kilim.testing and the event delegation of kilim.browser use it. *)

val discrete : string -> bool
(** [true] for click, input, change, submit and keydown: a turn of such an event
    renders at once. *)

val bubbles : string -> bool
(** [false] for the names of {!direct_events}: a listener on the element. *)

val delegated_events : string list
(** The names that use delegation: one listener on the mount element for each
    name. They are the names of {!Kilim.Ev} that bubble, and [compositionstart]
    and [compositionend]. *)

val direct_events : string list
(** The names that use a listener on the element: mouseenter, mouseleave,
    scroll, wheel, touchstart and touchmove. *)

val walk :
     parent:('node -> 'node option)
  -> cell:('node -> Dom.cell option)
  -> 'node
  -> string
  -> (Dom.handler -> unit)
  -> unit
(** [walk ~parent ~cell target name f] calls [f] with the handler for [name] of
    each element from [target] to the top, in this order. It stops after a
    handler with [stop]. If [bubbles name] is [false], it calls only the handler
    of [target]. The caller reads the payload from the target, calls [fire] and
    applies [prevent_default]. *)

(** {1 The dispatch}

    One dispatch for the browser and for kilim.testing: the rules of the events
    are in one place, and the native tests reach them. *)

(** The platform part of the dispatch: kilim.browser and [Page] each give one.
*)
type 'node env =
  { ops: 'node Dom.t
  ; parent: 'node -> 'node option  (** [None] at the mount element. *)
  ; cell: 'node -> Dom.cell option
  ; connected: 'node -> bool
        (** The node is in the document and under the mount element. *)
  ; group: 'node -> 'node list
        (** The other buttons of the radio group of a radio button, else [[]].
        *)
  ; toggles: 'node -> bool  (** An [input] with the type checkbox or radio. *)
  ; post_task: (unit -> unit) -> unit
        (** Runs a function in a new task, after the dispatch. *) }

(** One DOM event. *)
type 'node event =
  { name: string
  ; target: 'node
        (** [ev.target], or for a name of {!direct_events} [ev.currentTarget].
        *)
  ; composing: bool  (** [ev.isComposing]. *)
  ; read: 'a. 'a Dom.payload -> 'a  (** Reads the payload from the target. *)
  ; prevent: unit -> unit  (** [ev.preventDefault ()]. *)
  ; stop: unit -> unit  (** [ev.stopPropagation ()]. *) }

val dispatch : 'node env -> 'node Runtime.t -> 'node event -> bool
(** [dispatch env rt ev] runs the handlers of [ev] in one turn (design section
    11). The result is [true] if a handler prevented the default.

    The rules, in this order:
    + [keydown] or [keyup] with [composing]: nothing. Thus the Enter that
      commits an IME candidate does not submit.
    + [compositionstart] sets [composing] in the cell of the target, with no
      turn.
    + [compositionend] clears [composing], then runs a discrete turn with the
      restore of the target.
    + [focusin] or [focusout] on a target with [composing]: clears it, then runs
      the walk in a discrete turn with the restore of the target. Chrome sends
      no [compositionend] when it moves or removes the composing element.
    + A name of {!direct_events}: nothing if the target is not connected. Else
      the walk of the target only, in a [Macro] turn.
    + A discrete name: the walk in a discrete turn with the restore of the
      target. The other names: the walk in a [Macro] turn.
    + After the turn, a [click] on a target that [toggles] posts a task with a
      discrete turn that only restores the target. The browser undoes a
      prevented toggle after the dispatch, before the task.

    A handler with [prevent_default] calls [prevent], a handler with [stop]
    calls [stop], then the handler gets the payload. The restore of a target
    writes the view values of the target and of its radio group, if the target
    is connected. *)
