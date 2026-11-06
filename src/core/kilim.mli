(** kilim: a browser application library for OCaml (docs/design.html).

    This version has the async tasks and the cancel tokens of section 12. *)

module Span = Span

(** The internals for the runtime and the tests. They can change at any time. *)
module Private : sig
  val set_error_handler : (exn -> unit) -> unit
  (** The function that gets an exception with no error scope: from an end
      action, or from a callback of {!Async.make} after [ok] or [error]. The
      default does nothing. *)

  module Host = Host
  module Cancel = Cancel
  module Run_queue = Run_queue
  module Async = Async
end

(** Cancel tokens. *)
module Cancel : sig
  type t = Private.Cancel.t

  val cancelled : t -> bool

  val on_end : t -> (unit -> unit) -> unit
  (** [on_end t f] runs [f] one time: at the cancel of [t], or when the step of
      [t] ends. If [t] is already cancelled or ended, [f] runs at once. An
      exception from [f] goes to the error handler, and the other actions still
      run. *)
end

(** Cold async tasks. Nothing runs until the runtime starts a task with a token.
*)
module Async : sig
  type 'a t = 'a Private.Async.t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax = Private.Async.Syntax

  val sleep : Span.t -> unit t

  val first : 'a t list -> 'a t
  (** The first [ok] wins and cancels the other branches. If all branches fail,
      [first] gives the last error. [first []] fails with [Invalid_argument]
      when the task starts. *)

  val timeout : Span.t -> 'a t -> ('a, [`Timeout]) result t
  (** [timeout span t] gives [Ok x] when [t] gives [x], the error of [t] when
      [t] fails, or [Error `Timeout] after [span]. The end of one cancels the
      other. *)

  val make :
    (Cancel.t -> ok:('a -> unit) -> error:(exn -> unit) -> unit) -> 'a t
  (** [make start] binds a callback API. [start] gets the token of the step.
      [ok] and [error] have an effect one time only, and none after a cancel.
      The continuation never runs inside [start]: it goes on the run queue. An
      exception from [start] before [ok] or [error] is the error of the step.
      After [ok] or [error], it goes to the error handler. After a cancel, the
      runtime drops it.

      An end action of the token runs also when the step ends normally. Thus it
      must not stop work that continues after [ok]: for example, a fetch binding
      reads the body in the same step, and aborts only if the token is
      cancelled. Write each binding as a top-level function: under js_of_ocaml,
      a callback inside [update] can keep the variables of [update] alive. *)
end
