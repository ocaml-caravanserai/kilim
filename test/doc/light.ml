open Kilim
include Component.Defaults

type props = unit

type model = bool

type msg = Flip

type out = |

let id : (props, model, msg, out) Component.id = Component.id ()

let init () = (false, Cmd.none)

let update _ () on Flip = (not on, Cmd.none)

let view () on = El.button ~ev:[Ev.click Flip] [txt (if on then "on" else "off")]
