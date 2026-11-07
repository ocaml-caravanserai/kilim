(** The event walk (design section 11), generic over the node. The fake DOM of
    kilim.testing and the event delegation of kilim.browser use it. *)

val discrete : string -> bool
(** [true] for click, input, change, submit and keydown: a turn of such an event
    renders at once. *)

val bubbles : string -> bool
(** [false] for mouseenter, mouseleave and scroll. *)

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
