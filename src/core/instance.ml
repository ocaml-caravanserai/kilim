module Latest = Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

type ('model, 'msg) spec =
  {update: 'model -> 'msg -> 'model * 'msg Cmd.t; view: 'model -> 'msg View.t}

type 'msg state =
  | State : {spec: ('model, 'msg) spec; mutable model: 'model} -> 'msg state

type 'msg t =
  { base: Cancel.t
  ; mutable alive: bool
  ; mutable token: Cancel.t option
  ; mutable latest: Cancel.t Latest.t
  ; mutable dirty: bool
  ; mutable state: 'msg state option }

let make base =
  { base
  ; alive= true
  ; token= None
  ; latest= Latest.empty
  ; dirty= false
  ; state= None }

let token i =
  match i.token with
  | Some t ->
      t
  | None ->
      let t = Cancel.child i.base in
      i.token <- Some t ;
      t

let unmount i =
  i.alive <- false ;
  i.dirty <- false ;
  Option.iter Cancel.cancel i.token ;
  i.latest <- Latest.empty ;
  i.state <- None
