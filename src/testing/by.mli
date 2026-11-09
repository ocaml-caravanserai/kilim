(** The queries of {!Page} (design section 14). A query must find exactly one
    element of the app, never the mount element: {!Page.find} raises
    {!Page.Query} for no match and for more than one. *)

type t =
  | Node of Fake_dom.node  (** This node. *)
  | Test_id of string  (** The element with this [data-testid]. *)
  | Text of string  (** See {!text}. *)
  | Label of string  (** See {!label}. *)

val node : Fake_dom.node -> t
(** [node n] finds the node [n]. *)

val test_id : string -> t
(** [test_id s] finds the one element with the attribute [data-testid="s"]. *)

val text : string -> t
(** [text s] finds the deepest element whose text is [s]: no child element of it
    has this text. The compare collapses each run of white space to one space,
    and ignores the white space at the start and at the end, in the two texts.
*)

val label : string -> t
(** [label s] finds the control of a [label] element whose text is [s] (with the
    white space collapsed, as {!text} does): the element whose [id] is the [for]
    attribute of the label, else the first [input], [select], [textarea] or
    [button] inside the label. It also finds each element whose [aria-label] is
    [s]. *)
