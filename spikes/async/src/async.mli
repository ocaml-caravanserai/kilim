(** Cold async tasks with cancel tokens. Nothing runs until the runtime starts a
    task with a token. *)

type 'a t

val return : 'a -> 'a t

val fail : exn -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val bind : 'a t -> ('a -> 'b t) -> 'b t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val sleep : ms:int -> unit t

val first : 'a t list -> 'a t

val timeout : ms:int -> 'a t -> ('a, [`Timeout]) result t

val make : (Cancel.t -> ok:('a -> unit) -> error:(exn -> unit) -> unit) -> 'a t
(** [make start] binds a callback API. [ok] and [error] work one time only, and
    not after a cancel. The continuation never runs inside [start]. *)

(**/**)

type host =
  { now_ms: unit -> float
  ; set_timer: ms:int -> (unit -> unit) -> unit -> unit
        (** gives a function that clears the timer *) }

module Run_queue = Run_queue

val start :
     host:host
  -> run:Run_queue.t
  -> Cancel.t
  -> 'a t
  -> ok:('a -> unit)
  -> error:(exn -> unit)
  -> unit
(** Internal: the runtime starts a task. *)

val set_error_handler : (exn -> unit) -> unit
(** The function that gets an exception that a callback of [make] raises after
    [ok] or [error]. *)
