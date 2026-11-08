type ('p, 'm, 'msg, 'o) id = ('p, 'm, 'msg, 'o) View.id

let id = View.id

module type S = sig
  type props

  type model

  type msg

  type out

  val id : (props, model, msg, out) id

  val init : props -> model * msg Cmd.t

  val update : (msg, out) Cmd.ctx -> props -> model -> msg -> model * msg Cmd.t

  val view : props -> model -> msg View.t

  val subs : props -> model -> msg Sub.t

  val equal_props : props -> props -> bool
end

(* The spec of [C]: the last spec of the id if it has the same functions. Thus a
   render of a parent makes no spec for each child. *)
let spec (type p m msg o)
    (module C : S
      with type props = p
       and type model = m
       and type msg = msg
       and type out = o ) : (p, m, msg, o) View.spec =
  match C.id.last with
  | Some s
    when ( s.init == C.init && s.update == C.update && s.view == C.view
         && (match s.subs with Some f -> f == C.subs | None -> false)
         && s.equal_props == C.equal_props )
         [@mutate off "a pure shortcut: the new spec has the same fields"] ->
      s
  | Some _ | None ->
      let s =
        { View.id= C.id
        ; init= C.init
        ; update= C.update
        ; view= C.view
        ; subs= Some C.subs
        ; equal_props= C.equal_props }
      in
      C.id.last <- Some s ;
      s

let v (module C : S) ?key ~on props =
  Reconcile.install () ;
  View.Comp (View.C {spec= spec (module C); key; on; props})
