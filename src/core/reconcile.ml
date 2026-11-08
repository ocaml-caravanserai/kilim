(* The component nodes of the diff (design section 09): the mount of an
   instance, the patch of a component node, the render of an instance alone, the
   removal walk and the work queue. Component.v and View.boundary install these
   functions: thus an app with no component and no boundary does not link them.
   An error of a component leaves the diff: the try of the nearest boundary node
   on the path gets it, or the catch of the render of an instance alone, or the
   root catch. *)

open Tree

(* The view of a live instance. The view runs before the diff writes: an error
   leaves no half-changed DOM of the instance. *)
(* The instance has subscriptions to compare: a [subs] function, or the keys of
   an older spec (two modules can share one id). *)
let subscribes : type msg. ('node, msg) inst -> msg state -> bool =
 fun i (State d) ->
  Option.is_some d.spec.subs
  || not (Subs.is_empty i.runs && Subs.is_empty i.autos)

let view : type msg. 'node env -> ('node, msg) inst -> msg state -> msg View.t =
 fun env i s ->
  env.viewed () ;
  let (State d) = s in
  let v = d.spec.view d.props d.model in
  (* The pass at the end of the render compares the subscriptions of [i]. The
     list has the order of the views: a parent comes before its children. *)
  if env.passes && subscribes i s then env.touched <- Any i :: env.touched ;
  v

(* The render of a child by the patch of its parent, inside the run of the
   parent: the parent writes the controlled properties. The caller first removes
   the instance from the work queue. The render first removes the registrations
   of the instance, as Diff.patch does. *)
let redraw : type msg. ('node, msg) inst -> unit =
 fun i ->
  match (i.state, i.mount) with
  | Some s, Some m ->
      let v = view m.ctx.env i s in
      Instance.forget i ;
      Diff.patch_node m.ctx m.parent m.root v
  | (None | Some _), _ ->
      (* A dead instance: the callers never give one. *)
      ()

(* The mount: a live instance links into the child list of its owner before its
   [init] runs (design section 11, "Link before init"). The commands of [init]
   become task requests: the tasks start in the next settle step. Under a dead
   owner (the root fallback), the instance mounts dead: its commands do not run,
   and its messages are dropped. *)
let build : type msg.
    ('node, msg) ctx -> 'node -> msg View.comp -> ('node, msg) node =
 fun ctx parent (View.C {spec; key; on; props}) ->
  let env = ctx.env in
  let inst = Instance.child env ctx.owner ctx.boundary in
  let cctx = {ctx with post= env.post inst; owner= inst} in
  (* An output is an item of the parent. Its message function reads [on] when
     the item leaves the queue, thus a patch of the parent gives the newest
     [~on]. *)
  let on = ref on in
  let map o = !on o in
  let post = ctx.post in
  let out = {Cmd.emit= (fun o -> post.post o map)} in
  let model, cmd = spec.init props in
  let data = {spec; props; model; ctx= out} in
  let state = State data in
  inst.state <- Some state ;
  (* The runtime makes no request for a dead instance. *)
  env.command inst cmd ;
  let child = Diff.build cctx parent (view env inst state) in
  inst.mount <- Some {ctx= cctx; parent; root= child} ;
  (* The DOM node of a component node is the DOM node of [child], and [dom]
     keeps only the DOM parent: a new root of the child does not stay. *)
  {dom= parent; shape= Comp {key; on; inst; data; child}}

(* The same id, the same [?key] and a live instance keep the instance. Then the
   instance renders if [equal_props] of the new spec gives [false], or if it is
   dirty. If not, the diff builds the new node, puts it in the place of the old
   node, and unmounts the old instance. A dead instance is in the tree only
   after a root catch: the fallback can have a component at its place. *)
let patch : type msg.
    ('node, msg) ctx -> 'node -> ('node, msg) node -> msg View.comp -> unit =
 fun ctx parent r (View.C {spec; key; on; props} as c) ->
  match r.shape with
  | Comp old -> (
    match Type.Id.provably_equal spec.id.tid old.data.spec.id.tid with
    | Some Type.Equal
      when Option.equal String.equal key old.key && old.inst.alive ->
        old.on := on ;
        let d = old.data in
        d.spec <- spec ;
        let fresh = not (spec.equal_props d.props props) in
        if fresh then d.props <- props ;
        (* A dirty child with equal props: a pure shortcut. With no render here,
           the work queue renders it later in the same render. *)
        if fresh || old.inst.dirty then begin
          Instance.clean ctx.env old.inst ;
          redraw old.inst
        end
    | _ ->
        Diff.replace ctx parent r (View.Comp c) )
  | Text _ | Element _ | Boundary _ | Provider _ ->
      (* The diff gives only a component node. *)
      Diff.replace ctx parent r (View.Comp c)

(* A render of a dirty instance alone, with its own run of the controlled
   properties. *)
let render : type msg. ('node, msg) inst -> unit =
 fun i ->
  match (i.state, i.mount) with
  | Some s, Some m ->
      Diff.patch m (view m.ctx.env i s)
  | (None | Some _), _ ->
      (* A dead instance: the work queue has none. *)
      ()

(* A boundary leaves its lists and the work queue. The owner can stay alive. A
   second call does nothing. *)
let leave : 'node 'msg. 'node env -> ('node, 'msg) boundary -> unit =
 fun env b ->
  b.live <- false ;
  b.left <- true ;
  Option.iter Dlist.remove b.owned_link ;
  Option.iter Dlist.remove b.nested_link ;
  env.work <- Work.remove b.place env.work

let unlink env (Bound b) = leave env b

(* The removal walk: each instance in a removed subtree unmounts, and each
   boundary unlinks. The walk stops at a component node: the unmount of its
   instance reaches the instances below. The child of a boundary holds each of
   its members: a stopped diff in the child goes to the boundary, which unmounts
   its members. A provider node has nothing to unlink: its readers are the
   owner, whose render removed its registrations first, and the instances below
   it, which unmount. It allocates nothing. *)
let rec walk : 'node 'msg. 'node env -> ('node, 'msg) node -> unit =
 fun env r ->
  match r.shape with
  | Text _ ->
      ()
  | Element e ->
      walk_children env e.children
  | Comp c ->
      Instance.unmount env c.inst
  | Boundary b ->
      leave env b ; walk env b.child
  | Provider {child; _} ->
      walk env child

and walk_children : 'node 'msg. 'node env -> ('node, 'msg) children -> unit =
 fun env c ->
  match c with
  | Plain a | Keyed (_, a) ->
      for i = 0 to Array.length a - 1 do
        walk env a.(i)
      done
  | Inner _ ->
      ()

(* The dirty instances below the root, from the top: the smallest depth first,
   then the order of the mount. Each renders alone, and [catch] gets its error.
   A render of a parent renders its dirty children and removes them from the
   queue, and the unmount removes an instance. A root catch unmounts each
   instance, thus the queue becomes empty. A loop, not a recursion: many
   instances can be dirty. *)
let flush env catch =
  let wrote = ref false and again = ref true in
  while !again do
    match Work.min_binding_opt env.work with
    | Some (_, Inst i) ->
        let b = Instance.boundary i in
        Instance.clean env i ;
        (try render i with e -> catch b e) ;
        wrote := true
    | Some (_, Show {b; e}) ->
        (* An error in the fallback goes to the outer boundary. *)
        ( try Diff.run b.ctx (fun () -> !Diff.bounds.show env b e)
          with e -> catch b.ctx.boundary e ) ;
        wrote := true
    | None ->
        again := false
  done ;
  !wrote

(* The links of an instance: the work queue, the child list of its parent, the
   member list of its boundary, its view, the boundaries of its view and its
   children. A second call does nothing. *)
let detach : 'node 'msg. 'node env -> ('node, 'msg) inst -> unit =
 fun env i ->
  env.work <- Work.remove i.pos env.work ;
  Option.iter Dlist.remove i.sibling ;
  i.sibling <- None ;
  i.parent <- None ;
  i.mount <- None ;
  Option.iter Dlist.remove i.member ;
  (match i.owned with Some l -> Dlist.drain unlink env l | None -> ()) ;
  if
    ( Dlist.length i.children > 0 )
    [@mutate off "a pure shortcut: no closure for an empty list"]
  then Dlist.iter (fun (Any c) -> Instance.unmount env c) i.children

(* The model of [i] changed: the render renders it, and the early call compares
   its subscriptions. [early_stale] is still the flag of the last pass: thus
   many model changes before a pass give one entry. *)
let enqueue : 'node 'msg. 'node env -> ('node, 'msg) inst -> unit =
 fun env i ->
  env.work <- Work.add i.pos (Inst i) env.work ;
  match i.state with
  | Some s when env.passes && (not i.early_stale) && subscribes i s ->
      env.stale <- Any i :: env.stale
  | Some _ | None ->
      ()

let hooks = {Diff.build; patch; render; walk; walk_children; flush}

let ext = {Instance.detach; enqueue}

let install () =
  Diff.hooks := hooks ;
  Instance.ext := ext
