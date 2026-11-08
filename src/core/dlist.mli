(** An intrusive doubly linked list: the holder of a value keeps its node, and
    removes it in O(1) (design section 11, "Link before init"). The child lists
    of the instances and the lists of the boundaries use it. *)

type 'a t

type 'a node

val create : unit -> 'a t

val push : 'a t -> 'a -> 'a node
(** [push l x] puts [x] first in [l], in O(1), and gives its node. *)

val remove : 'a node -> unit
(** [remove n] removes [n] from its list in O(1). A second call does nothing. *)

val length : 'a t -> int

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f l] calls [f] for each value, the first value first. [f] can remove
    the node of its value, but no other node. *)

val drain : ('b -> 'a -> unit) -> 'b -> 'a t -> unit
(** [drain f x l] removes the first node of [l] and calls [f x] with its value,
    until [l] is empty. [f] can remove any node. If [f] pushes a value, [drain]
    removes it too. *)
