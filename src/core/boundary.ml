(* The boundary nodes of the diff (design sections 09 and 11): the build and the
   patch of a boundary node, the catch inside the diff and outside the diff, and
   the write of a fallback. View.boundary installs these functions: thus an app
   with no boundary does not link them. *)

open Tree

let unmount_member env (Any i) = Instance.unmount env i

(* The subtree of a boundary unmounts: each member, and each nested boundary
   with its subtree. The unmount of a member can remove other members and nested
   boundaries: [Dlist.drain] gets each value one time. The cost is linear in the
   size of the subtree. *)
let rec kill : 'node 'msg. 'node env -> ('node, 'msg) boundary -> unit =
 fun env b ->
  Dlist.drain unmount_member env b.members ;
  Dlist.drain kill_nested env b.nested

and kill_nested : 'node. 'node env -> 'node any_boundary -> unit =
 fun env (Bound n) -> Reconcile.leave env n ; kill env n

(* The catch: the boundary unmounts its subtree and keeps the error. A boundary
   catch does not call the error handler: the fallback gets the exception. *)
let catch env b e =
  kill env b ;
  b.failed <- Some e ;
  env.caught ()

(* The write of the fallback in the place of the child. The fallback gets the
   context of the place: an error in it goes to the outer boundary. The caller
   writes the controlled properties. The old child holds only dead instances and
   unlinked boundaries. *)
let show : 'node 'msg. 'node env -> ('node, 'msg) boundary -> exn -> unit =
 fun env b e ->
  env.work <- Work.remove b.place env.work ;
  let fresh = Diff.build b.ctx b.dom_parent (b.fallback e) in
  b.ctx.ops.replace b.dom_parent (Diff.dom fresh) (Diff.dom b.child) ;
  b.child <- fresh ;
  b.shown <- true

(* The catch outside the diff: an error of [update], of a task, of a message
   function, of [subs], or of the render of an instance alone. The boundary goes
   into the work queue: thus a render is pending, and the render writes the
   fallback. A boundary fails one time: the callers give no failed boundary,
   thus [shown] is still [false]. Only a live boundary catches: an error after a
   stop goes to the error handler. *)
let fail env (Bound b) e =
  if b.live then begin
    catch env b e ;
    env.work <- Work.add b.place (Show {b; e}) env.work
  end
  else Report.error e

(* The build. The boundary links into the owned list of its owner and into the
   nested list of its outer boundary before its child builds (design section 11,
   "Link before init"). The child builds in [try]. On an error, the fallback
   builds outside the [try]: its error goes to the outer boundary. *)
let build : type msg.
    ('node, msg) ctx -> 'node -> msg View.boundary -> ('node, msg) node =
 fun ctx parent {View.key; fallback; child} ->
  let env = ctx.env and owner = ctx.owner in
  let serial = env.serial in
  env.serial <- serial + 1 ;
  let b =
    { place= {depth= owner.pos.depth; serial}
    ; key
    ; ctx
    ; inner= ctx
    ; dom_parent= parent
    ; fallback
    ; members= Dlist.create ()
    ; nested= Dlist.create ()
    ; owned_link= None
    ; nested_link= None
    ; live= owner.alive
    ; left= false
    ; failed= None
    ; shown= false
    ; child= {dom= parent; shape= Text ""} }
  in
  let a = Bound b in
  b.inner <- {ctx with boundary= Some a} ;
  if owner.alive then begin
    let owned =
      match owner.owned with
      | Some l ->
          l
      | None ->
          let l = Dlist.create () in
          owner.owned <- Some l ;
          l
    in
    b.owned_link <- Some (Dlist.push owned a) ;
    match ctx.boundary with
    | Some (Bound o) ->
        b.nested_link <- Some (Dlist.push o.nested a)
    | None ->
        ()
  end ;
  ( match Diff.build b.inner parent child with
  | c ->
      b.child <- c
  | exception e ->
      catch env b e ;
      b.child <- Diff.build ctx parent (fallback e) ;
      b.shown <- true ) ;
  {dom= parent; shape= Boundary b}

(* The same [?key] keeps a boundary that did not leave the tree: a boundary of
   the dead App left, thus the root fallback builds its own boundary. A boundary
   with no error patches its child in [try]: on an error, it writes its fallback
   at once. A failed boundary calls [fallback] again and patches the fallback
   subtree: it does not try the child again. A failed boundary whose fallback is
   not in the DOM (a catch outside the diff) writes it. A new [?key], or another
   node kind at the place, remounts. *)
let patch : type msg.
    ('node, msg) ctx -> 'node -> ('node, msg) node -> msg View.boundary -> unit
    =
 fun ctx parent r v ->
  match r.shape with
  | Boundary b when Option.equal String.equal v.key b.key && not b.left -> (
      b.fallback <- v.fallback ;
      match b.failed with
      | None -> (
        match Diff.patch_node b.inner parent b.child v.child with
        | () ->
            ()
        | exception e ->
            catch ctx.env b e ; show ctx.env b e )
      | Some e ->
          if b.shown then Diff.patch_node b.ctx parent b.child (v.fallback e)
          else show ctx.env b e )
  | Boundary _ | Text _ | Element _ | Comp _ ->
      Diff.replace ctx parent r (View.Boundary v)

let bounds = {Diff.build_boundary= build; patch_boundary= patch; show; fail}

let install () =
  Reconcile.install () ;
  Diff.bounds := bounds

let v ?key ~fallback child =
  install () ;
  View.Boundary {key; fallback; child}
