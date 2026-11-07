(** The run queue of Async (design sections 11 and 12): the continuations that
    wait for the drain, and the trampoline. *)

type t

val create : schedule:(unit -> unit) -> t
(** [schedule] asks the host for a turn. *)

val max_depth : int
(** The depth of synchronous steps above which [step] puts the work on the
    queue: 256. *)

val length : t -> int

val is_empty : t -> bool

val clear : t -> unit
(** [clear q] removes all continuations. The depth and the flags do not change.
    Thus a push after [clear] asks for a turn again, because the queue is empty.
*)

val reset : t -> unit
(** [reset q] sets the depth to 0 and clears the flags of a turn and of a drain,
    for {!Runtime.abort}: a JS exception that no OCaml catch sees can leave them
    set. The continuations stay. *)

val with_turn : t -> (unit -> 'a) -> 'a
(** [with_turn q f] runs [f] in a turn: a push inside a turn asks for no turn.
    The turn ends also when [f] raises. *)

val push : t -> (unit -> unit) -> unit
(** A push outside a turn and outside a drain asks for one turn, when the queue
    was empty. *)

val step : t -> (unit -> unit) -> unit
(** The trampoline: run at once while the depth is below [max_depth], or else
    push. *)

val drain : ?over:(unit -> bool) -> t -> unit
(** Run the continuations until the queue is empty. [over] reads the clock: the
    drain asks it before the first continuation and once every 256
    continuations, and it stops when [over] gives [true]. Then the rest stays in
    the queue, and the caller must ask for a turn: a push on a queue that is not
    empty asks for none. A drain inside a drain does nothing. After an
    exception, the drain can run again. *)
