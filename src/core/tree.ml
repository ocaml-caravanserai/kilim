(* The types of the retained tree and of the instance tree (design section 09).
   An instance keeps its retained tree, and a retained tree keeps the instances
   of its component nodes. Thus the types are in one module. Instance and Diff
   give the operations. *)

(* The place of an instance in the render order: the depth, then the order of
   the mount. *)
type pos = {depth: int; serial: int}

(* The work queue of a render: the dirty live instances below the root, and the
   failed boundaries whose fallback is not in the DOM. The render takes the
   smallest place first, thus it goes from the top. *)
module Work = Map.Make (struct
  type t = pos

  let compare a b =
    let c = Int.compare a.depth b.depth in
    if c <> 0 then c else Int.compare a.serial b.serial
end)

module Latest = Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

module Subs = Map.Make (Sub.Key)

type 'msg auto =
  | Auto :
      {token: Cancel.t; kind: 'p Sub.kind; on: ('p -> 'msg) ref}
      -> 'msg auto

(* The state of an instance. A component node keeps the same record: a patch
   compares the ids, and then knows the types of the props. *)
type ('p, 'm, 'msg, 'o) data =
  { mutable spec: ('p, 'm, 'msg, 'o) View.spec
  ; mutable props: 'p
  ; mutable model: 'm
  ; ctx: ('msg, 'o) Cmd.ctx (* The context of [update]. *) }

type 'msg state = State : ('p, 'm, 'msg, 'o) data -> 'msg state

(* A node of the retained tree of the view of an instance. ['msg] is the message
   type of that instance. *)
type ('node, 'msg) node =
  { mutable dom: 'node
        (* The DOM node of a text or an element. A component node has no DOM
           node of its own: Diff.dom follows [child], and [dom] is the DOM
           parent, thus it keeps no old root of the child alive. *)
  ; mutable shape: ('node, 'msg) shape }

and ('node, 'msg) shape =
  | Text of string
  | Element of ('node, 'msg) element
  | Comp :
      { key: string option
      ; on: ('o -> 'msg) ref (* The newest [~on] of the parent. *)
      ; inst: ('node, 'c) inst
      ; data: ('p, 'm, 'c, 'o) data
      ; child: ('node, 'c) node
            (* The root of the view of [inst]. A render of [inst] alone changes
               this record in place. The unmount clears [inst.mount], but the
               parent keeps the DOM node here until the diff removes the
               component node. *) }
      -> ('node, 'msg) shape
  | Boundary of ('node, 'msg) boundary

and ('node, 'msg) element =
  { ns: Dom.ns
  ; name: string
  ; cell: Dom.cell
  ; mutable attrs: (string * string) list
  ; mutable listened: (string * bool) list
        (* the events with a listener on the element, and passive *)
  ; mutable children: ('node, 'msg) children }

and ('node, 'msg) children =
  | Plain of ('node, 'msg) node array
  | Keyed of Key.t array * ('node, 'msg) node array
  | Inner of Unsafe_html.t

and ('node, 'msg) inst =
  { base: Cancel.t (* The runtime token: the parent of the root token. *)
  ; pos: pos
  ; mutable parent: 'node any option (* [None] for the root and the dead. *)
  ; children: 'node any Dlist.t
  ; mutable sibling: 'node any Dlist.node option
        (* The node of the instance in the child list of its parent. *)
  ; mutable mount: ('node, 'msg) mount option
        (* The view of a child. [None] for the root, and after the unmount. *)
  ; mutable alive: bool
  ; mutable token: Cancel.t option
  ; mutable latest: Cancel.t Latest.t
  ; mutable dirty: bool
  ; mutable runs: Cancel.t Subs.t
  ; mutable autos: 'msg auto Subs.t
  ; mutable early_stale: bool
  ; mutable state: 'msg state option
  ; mutable member: 'node any Dlist.node option
        (* The node of the instance in the member list of its boundary. The
           boundary itself is the boundary of the context of [mount]. *)
  ; mutable owned: 'node any_boundary Dlist.t option
        (* The live boundaries in the view of the instance. The list comes at
           the first boundary: an instance with no boundary saves its words. *)
  }

and ('node, 'msg) mount =
  { ctx: ('node, 'msg) ctx
  ; parent: 'node (* The DOM parent. A keyed move stays in one parent. *)
  ; root: ('node, 'msg) node }

and 'node any = Any : ('node, 'msg) inst -> 'node any

(* A boundary node (design section 09). Its [owner] is the owner of [ctx], and
   its outer boundary is the boundary of [ctx]. [ctx] is the context of the diff
   at its place: the fallback gets it. [inner] is the context of its child: the
   same, with this boundary. *)
and ('node, 'msg) boundary =
  { place: pos (* The depth of the owner, and a serial of the instances. *)
  ; key: string option
  ; ctx: ('node, 'msg) ctx
  ; mutable inner: ('node, 'msg) ctx
  ; dom_parent: 'node (* A keyed move stays in one parent. *)
  ; mutable fallback: exn -> 'msg View.t (* The newest. *)
  ; members: 'node any Dlist.t
        (* The live instances whose boundary this is, the newest first. *)
  ; nested: 'node any_boundary Dlist.t
        (* The live boundaries whose outer boundary this is. *)
  ; mutable owned_link: 'node any_boundary Dlist.node option
        (* The node in the owned list of the owner. *)
  ; mutable nested_link: 'node any_boundary Dlist.node option
        (* The node in the nested list of the outer boundary. *)
  ; mutable live: bool
        (* In the lists: the owner is alive and the node is in its view. *)
  ; mutable left: bool
        (* The node left the tree: a removal, a new key, the catch of an outer
           boundary or the unmount of the owner. A boundary of a dead owner (in
           the root fallback) is in no list, but it has not left. *)
  ; mutable failed: exn option
  ; mutable shown: bool (* After an error: the fallback is in the DOM. *)
  ; mutable child: ('node, 'msg) node
        (* The child, or the fallback subtree after an error. *) }

and 'node any_boundary = Bound : ('node, 'msg) boundary -> 'node any_boundary

(* An entry of the work queue: a dirty instance, or a failed boundary whose
   fallback is not in the DOM. *)
and 'node work =
  | Inst : ('node, 'msg) inst -> 'node work
  | Show : {b: ('node, 'msg) boundary; e: exn} -> 'node work

(* The context of the diff of the view of [owner]. [controlled] holds the
   elements with a controlled property in the view, with their boundaries, in
   reverse order: the diff writes their properties after all the other
   operations of the patch. The diff patches and builds the nodes in the order
   of the document, thus the order of [controlled] is the order of the end tags.
   One render shares one list: renders never nest. [boundary] is the nearest
   boundary above the place. *)
and ('node, 'msg) ctx =
  { ops: 'node Dom.t
  ; post: 'msg Dom.post
  ; ns: Dom.ns
  ; controlled: ('node * Dom.cell * 'node any_boundary option) list ref
  ; owner: ('node, 'msg) inst
  ; env: 'node env
  ; boundary: 'node any_boundary option }

(* The parts of the runtime that a mount of an instance needs. *)
and 'node env =
  { post: 'msg. ('node, 'msg) inst -> 'msg Dom.post
        (* The post of the cells of the view of an instance. *)
  ; command: 'msg. ('node, 'msg) inst -> 'msg Cmd.t -> unit
  ; viewed: unit -> unit (* Counts a call of a view. *)
  ; caught: unit -> unit (* Counts a boundary catch. *)
  ; mutable serial: int (* The serial of the next instance or boundary. *)
  ; mutable work: 'node work Work.t
  ; passes: bool
        (* The runtime compares the subscriptions, thus [stale] and [touched]
           record the instances. A diff with no runtime has [false]. *)
  ; mutable stale: 'node any list
        (* The instances below the root whose model changed since their last
           subscription pass, the newest first, one entry for each pass: the
           early call compares their subscriptions. An entry can be dead. *)
  ; mutable touched: 'node any list
        (* The instances below the root whose view ran in this render, the
           newest first: the pass at the end of the render compares their
           subscriptions and starts the new keys. An entry can be dead. *) }
