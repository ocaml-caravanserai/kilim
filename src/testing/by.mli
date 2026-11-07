(** The queries of {!Page} (design section 14). *)

type t =
  | Node of Fake_dom.node  (** This node. *)
  | Test_id of string  (** The element with this [data-testid]. *)

val node : Fake_dom.node -> t

val test_id : string -> t
(** [test_id s] finds the one element with the attribute [data-testid="s"]. *)
