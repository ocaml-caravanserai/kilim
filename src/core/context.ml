(* The providers and the reads of the diff (design section 10). Context.provide
   and Context.read install these functions: thus an app with no context does
   not link them. *)

open Tree

let create ~default () = {View.cid= Type.Id.make (); default}

(* A new value of a provider marks each reader as dirty. A reader is a live
   instance below the owner of the provider: the render of the owner removed the
   registrations of the owner first. Thus the work queue renders the readers in
   the same render, after the owner. A dirty reader is in the work queue
   already: a reader with two registrations goes in one time. *)
let mark env (Any i) =
  if not i.dirty then begin
    i.dirty <- true ;
    env.work <- Work.add i.pos (Inst i) env.work
  end

(* The registration of the owner of [ctx] with a provider. A dead owner (a
   component of the root fallback) does not register: a dead instance never
   renders again. A read with the provider and the site of the last registration
   does not register again. Other duplicates cost only memory until the next
   render of the owner: [mark] puts a dirty reader in the work queue one time. A
   search of all the registrations would cost O(n^2) for a view with n
   boundaries that each read. The [boundary] of the contexts of one place is one
   value, thus [==] compares the places. *)
let register (ctx : _ ctx) a readers =
  let o = ctx.owner in
  let site = ctx.boundary in
  let again =
    match o.reads with r :: _ -> r.prov == a && r.site == site | [] -> false
  in
  if o.alive && not again then
    o.reads <- {prov= a; site; link= Dlist.push readers (Any o)} :: o.reads

(* The value of the nearest provider of the context in the environment of the
   place, or the default value. An error of [f] leaves the diff: it goes to the
   nearest boundary, or to the root catch. *)
let lookup : type msg. ('node, msg) ctx -> msg View.read -> msg View.t =
 fun ctx (View.R {context; f}) ->
  let rec find = function
    | [] ->
        f context.default
    | (Prov p as a) :: rest -> (
      match Type.Id.provably_equal context.cid p.cid with
      | Some Type.Equal ->
          register ctx a p.readers ; f p.value
      | None ->
          find rest )
  in
  find ctx.providers

(* The provider goes first in the environment of its child. *)
let build : type msg.
    ('node, msg) ctx -> 'node -> msg View.provide -> ('node, msg) node =
 fun ctx parent (View.P {context; value; child}) ->
  let p = {cid= context.cid; value; readers= Dlist.create ()} in
  let inner = {ctx with providers= Prov p :: ctx.providers} in
  {dom= parent; shape= Provider {p; inner; child= Diff.build inner parent child}}

(* The same context keeps the provider. A new value, by Shallow.equal, marks
   each reader. Another context, or another node kind at the place, remounts the
   subtree: thus no environment keeps a removed provider. *)
let patch : type msg.
    ('node, msg) ctx -> 'node -> ('node, msg) node -> msg View.provide -> unit =
 fun ctx parent r (View.P {context; value; child} as v) ->
  match r.shape with
  | Provider {p; inner; child= old} -> (
    match Type.Id.provably_equal context.cid p.cid with
    | Some Type.Equal ->
        if not (Shallow.equal p.value value) then begin
          p.value <- value ;
          Dlist.iter (mark ctx.env) p.readers
        end ;
        Diff.patch_node inner parent old child
    | None ->
        Diff.replace ctx parent r (View.Provide v) )
  | Text _ | Element _ | Comp _ | Boundary _ ->
      (* The diff gives only a provider node. *)
      Diff.replace ctx parent r (View.Provide v)

let contexts = {Diff.build_provider= build; patch_provider= patch; read= lookup}

let unlink l = List.iter (fun r -> Dlist.remove r.link) l

let install () =
  Diff.contexts := contexts ;
  Instance.unlinks := {f= unlink}

let provide context value child =
  install () ;
  View.Provide (View.P {context; value; child})

let read context f =
  install () ;
  View.Read (View.R {context; f})
