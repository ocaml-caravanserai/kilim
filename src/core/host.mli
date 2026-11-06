(** The host of the runtime (design section 12): the time, the timers, the
    frames and the tasks of the platform. The tests use a virtual clock. *)

type t =
  { now_ms: unit -> float
  ; set_timer: ms:int -> (unit -> unit) -> unit -> unit
        (** [set_timer ~ms f] gives a function that clears the timer. *)
  ; request_frame: (unit -> unit) -> unit
  ; queue_microtask: (unit -> unit) -> unit
  ; post_task: (unit -> unit) -> unit  (** a [MessageChannel] message *) }
