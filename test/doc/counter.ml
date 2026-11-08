open Kilim
include Component.Defaults

type props = {step: int; label: string}

type model = int

type msg = Incr | Decr

type out = Changed of int

let id : (props, model, msg, out) Component.id = Component.id ()

let init _ = (0, Cmd.none)

let update ctx p n = function
  | Incr ->
      let n = n + p.step in
      (n, Ctx.emit ctx (Changed n))
  | Decr ->
      let n = n - p.step in
      (n, Ctx.emit ctx (Changed n))

let view p n =
  El.div
    [ El.span [txt p.label]
    ; El.button ~ev:[Ev.click Decr] [txt "-"]
    ; El.output [txt (string_of_int n)]
    ; El.button ~ev:[Ev.click Incr] [txt "+"] ]
