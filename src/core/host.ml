type t =
  { now_ms: unit -> float
  ; set_timer: ms:int -> (unit -> unit) -> unit -> unit
  ; request_frame: (unit -> unit) -> unit
  ; queue_microtask: (unit -> unit) -> unit
  ; post_task: (unit -> unit) -> unit }
