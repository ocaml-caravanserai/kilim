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
