module Latest = Tree.Latest
module Subs = Tree.Subs
module Work = Tree.Work

type 'msg auto = 'msg Tree.auto =
  | Auto :
      {token: Cancel.t; kind: 'p Sub.kind; on: ('p -> 'msg) ref}
      -> 'msg auto

type ('p, 'm, 'msg, 'o) data = ('p, 'm, 'msg, 'o) Tree.data =
  { mutable spec: ('p, 'm, 'msg, 'o) View.spec
  ; mutable props: 'p
  ; mutable model: 'm }

type 'msg state = 'msg Tree.state =
  | State : ('p, 'm, 'msg, 'o) data -> 'msg state

type ('node, 'msg) t = ('node, 'msg) Tree.inst

type 'node any = 'node Tree.any = Any : ('node, 'msg) t -> 'node any

type 'node env = 'node Tree.env

let make ~base ~pos ~parent : ('node, 'msg) t =
  { base
  ; pos
  ; parent
  ; children= Dlist.create ()
  ; sibling= None
  ; mount= None
  ; alive= true
  ; token= None
  ; latest= Latest.empty
  ; dirty= false
  ; runs= Subs.empty
  ; autos= Subs.empty
  ; early_stale= false
  ; state= None }

type ext =
  { detach: 'node 'msg. 'node env -> ('node, 'msg) t -> unit
  ; enqueue: 'node 'msg. 'node env -> ('node, 'msg) t -> unit }

let ext = ref {detach= (fun _ _ -> ()); enqueue= (fun _ _ -> ())}

let root base = make ~base ~pos:{depth= 0; serial= 0} ~parent:None

let child (env : _ env) (p : _ t) =
  let serial = env.serial in
  env.serial <- serial + 1 ;
  let i =
    make ~base:p.base ~pos:{depth= p.pos.depth + 1; serial} ~parent:None
  in
  if p.alive then begin
    i.parent <- Some (Any p) ;
    i.sibling <- Some (Dlist.push p.children (Any i))
  end
  else i.alive <- false ;
  i

(* The parent of each token of a dead instance. *)
let dead =
  let t = Cancel.root () in
  Cancel.cancel t ; t

let rec token : 'node 'msg. ('node, 'msg) t -> Cancel.t =
 fun (i : _ t) ->
  match i.token with
  | Some t ->
      t
  | None ->
      let p =
        if not i.alive then dead
        else match i.parent with Some (Any p) -> token p | None -> i.base
      in
      let t = Cancel.child p in
      i.token <- Some t ;
      t

let changed (env : _ env) (i : _ t) =
  i.dirty <- true ;
  i.early_stale <- true ;
  if i.pos.depth > 0 then !ext.enqueue env i

(* The remove of an instance that is not in the queue gives the same map. *)
let clean (env : _ env) (i : _ t) =
  i.dirty <- false ;
  env.work <- Work.remove i.pos env.work

(* Each step does nothing the second time: a cancelled token stays cancelled,
   and the sets are empty. *)
let unmount (env : _ env) (i : _ t) =
  i.alive <- false ;
  i.dirty <- false ;
  !ext.detach env i ;
  Option.iter Cancel.cancel i.token ;
  i.latest <- Latest.empty ;
  (* A task that ended released its token: the cancel of the instance token does
     not reach it. Its cancel sets the cancelled flag, thus its send pushes
     nothing. *)
  Subs.iter (fun _ token -> Cancel.cancel token) i.runs ;
  Subs.iter (fun _ (Auto {token; _}) -> Cancel.cancel token) i.autos ;
  i.runs <- Subs.empty ;
  i.autos <- Subs.empty ;
  i.state <- None
