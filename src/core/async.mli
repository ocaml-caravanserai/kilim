(** Cold async tasks (design section 12). Nothing runs until the runtime starts
    a task with a token. *)

type 'a t

val return : 'a -> 'a t

val fail : exn -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val bind : 'a t -> ('a -> 'b t) -> 'b t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** The two branches run at the same time. The first error cancels the other
      branch. *)
end

val sleep : Span.t -> unit t

val first : 'a t list -> 'a t
(** See {!Kilim.Async.first}. *)

val timeout : Span.t -> 'a t -> ('a, [`Timeout]) result t
(** See {!Kilim.Async.timeout}. *)

val make : (Cancel.t -> ok:('a -> unit) -> error:(exn -> unit) -> unit) -> 'a t
(** See {!Kilim.Async.make}. *)

val start :
     host:Host.t
  -> run:Run_queue.t
  -> Cancel.t
  -> 'a t
  -> ok:('a -> unit)
  -> error:(exn -> unit)
  -> unit
(** The runtime starts a task under a token. *)
