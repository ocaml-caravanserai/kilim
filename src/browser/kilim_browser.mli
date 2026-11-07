(** kilim.browser: the browser part of kilim (docs/design.html).

    This version has the browser host. *)

(** The internals for the tests. They can change at any time. *)
module Private : sig
  val host : unit -> Kilim.Private.Host.t
  (** The host of the page: one microtask queue and one [MessageChannel] for all
      apps. A test waits for "one task" with its [post_task]. *)

  val live_timers : unit -> int
  (** The timers of the host that did not run and are not cleared. *)

  val env : unit -> Kilim.Private.Runtime.env
  (** The env of the runtime: the host, the hidden flag of the document, and the
      model compare ([Object.is] in the js_of_ocaml build, [( == )] in the other
      builds). *)
end
