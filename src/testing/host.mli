(** A host with a virtual clock (design section 14). It gives the timers, the
    frames, the tasks and the microtasks of {!Kilim.Private.Host} in a fixed
    order. Nothing runs until the test calls {!step}, {!run}, {!advance} or
    {!idle}.

    The rules:
    - The clock is a float. Only the moves of {!advance} and {!idle} change it.
      The timer delays and the frame times are integers.
    - Microtasks: one FIFO. The host runs all of them before and after each
      macrotask. {!Page} also runs them after each of its top-level calls.
    - Macrotasks: one ready FIFO, with the [post_task] callbacks, the raw
      callbacks and the timers that are due, and the frames. At each step, the
      host first moves the due timers (by due time, then by order of creation),
      then one frame if a frame is due, to the end of the FIFO. Then it runs the
      head.
    - A frame runs at the first multiple of [frame_ms] strictly after the first
      waiting request. One frame runs all the callbacks that wait. No frame runs
      while the page is hidden.
    - A cleared timer does not run, also when it is in the ready FIFO.
    - An exception from a macrotask or a microtask goes to {!errors}, as a
      browser writes it to the console. *)

type t

val create : ?frame_ms:int -> unit -> t
(** [frame_ms] is the time between two frames (default 16). [create] sets the
    error handler and the warning handler of {!Kilim.Private} to the lists of
    the new host.

    @raise Invalid_argument if [frame_ms] is not positive. *)

val host : t -> Kilim.Private.Host.t

val env : t -> Kilim.Private.Runtime.env
(** The env of the runtime: {!host} and the hidden flag. *)

val now : t -> float

val hidden : t -> bool

val set_hidden : t -> bool -> unit
(** Only the flag. {!Page.set_hidden} also tells the runtime. *)

val raw : t -> ?after:int -> (unit -> unit) -> unit
(** [raw t ~after f] is a foreign macrotask (a raw listener, an IndexedDB
    request) at [now + after] ms (default 0). It is not a turn, and it is not a
    timer for {!live_timers}. *)

val raw_microtask : t -> (unit -> unit) -> unit
(** A foreign microtask, for example a promise reaction. *)

val microtasks : t -> unit
(** Run the microtasks that wait, and the microtasks that they add. *)

val step : t -> bool
(** Run the microtasks that wait, then the next macrotask that is due, then the
    microtasks. [false] if no macrotask is due at [now]. *)

val run : t -> unit
(** Run each macrotask that is due at [now]. *)

val advance : t -> Kilim.Span.t -> unit
(** [advance t span] runs the work that is due. Then it moves the clock to the
    next due time (a timer or a frame) if that time is not after the target, and
    continues. At the end, the clock is the target. *)

val idle : ?limit:int -> t -> unit
(** Run until no macrotask, timer or frame remains. A frame that waits while the
    page is hidden does not count.

    @raise Failure after [limit] steps (default 1,000,000). *)

val live_timers : t -> int
(** The timers of [set_timer] that did not run and are not cleared. *)

val frames : t -> int
(** The frame callbacks that wait. *)

val errors : t -> exn list
(** The exceptions of the error handler and of the macrotasks and microtasks,
    the oldest first. *)

val warnings : t -> string list
(** The warnings of the warning handler, the oldest first. *)
