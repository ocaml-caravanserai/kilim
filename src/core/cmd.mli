(** Commands (design section 08): the work that [update] asks for, as a value.
    The runtime takes the leaves with {!requests}. *)

type 'msg t =
  | None
  | Batch of 'msg t list
  | Task of 'msg Async.t
  | Latest of string * 'msg Async.t
  | Map : string option * ('a -> 'msg) * 'a t -> 'msg t
      (** [Map (scope, f, c)]: the messages of [c] go through [f]. [scope] adds
          a level to the keys of [Latest] in [c]. *)

(** The context of the [update] of a component (design section 09). ['msg] is
    the message type of the component. [emit] puts an output in the queue, as a
    message of the parent. *)
type ('msg, 'out) ctx = {emit: 'out -> unit}

val no_ctx : ('msg, 'out) ctx
(** A context whose [emit] does nothing. The App and, in this version, each
    component get it: the outputs come later. *)

(** A task of a command, with the path of its key and its message function. *)
type 'msg request =
  | Request :
      {key: string list option; task: 'a Async.t; to_msg: 'a -> 'msg}
      -> 'msg request

val requests : 'msg t -> 'msg request list
(** The leaves in the order of the tree (left to right). [key] is the path of
    the scopes, outer first, then the key. [to_msg] applies the functions of
    [Map], inner first. The walk uses an explicit stack, and [to_msg] applies
    the functions in a loop. Thus a deep [Batch] or [Map] does not overflow the
    stack. *)
