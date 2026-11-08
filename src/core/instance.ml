module Latest = Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

module Subs = Map.Make (Sub.Key)

type 'msg auto =
  | Auto :
      {token: Cancel.t; kind: 'p Sub.kind; on: ('p -> 'msg) ref}
      -> 'msg auto

type ('props, 'model, 'msg) spec =
  { update: 'props -> 'model -> 'msg -> 'model * 'msg Cmd.t
  ; view: 'props -> 'model -> 'msg View.t
  ; subs: ('props -> 'model -> 'msg Sub.t) option }

type 'msg state =
  | State :
      { spec: ('props, 'model, 'msg) spec
      ; mutable props: 'props
      ; mutable model: 'model }
      -> 'msg state

type 'msg t =
  { base: Cancel.t
  ; mutable alive: bool
  ; mutable token: Cancel.t option
  ; mutable latest: Cancel.t Latest.t
  ; mutable dirty: bool
  ; mutable runs: Cancel.t Subs.t
  ; mutable autos: 'msg auto Subs.t
  ; mutable early_stale: bool
  ; mutable state: 'msg state option }

let make base =
  { base
  ; alive= true
  ; token= None
  ; latest= Latest.empty
  ; dirty= false
  ; runs= Subs.empty
  ; autos= Subs.empty
  ; early_stale= false
  ; state= None }

let token i =
  match i.token with
  | Some t ->
      t
  | None ->
      let t = Cancel.child i.base in
      i.token <- Some t ;
      t

let changed i =
  i.dirty <- true ;
  i.early_stale <- true

let unmount i =
  i.alive <- false ;
  i.dirty <- false ;
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
