open Kilim

type theme = Light | Dark

let theme : theme Context.t = Context.create ~default:Light ()

let theme_class = function Light -> "light" | Dark -> "dark"

(* A component that reads the theme in its view. *)
module Toolbar = struct
  include Component.Defaults

  type props = string

  type model = unit

  type msg = |

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () = function (_ : msg) -> .

  let view title () =
    Context.read theme (fun t ->
        El.nav ~at:[At.class_ (theme_class t)] [txt title] )
end

let view (dark : bool) : unit View.t =
  Context.provide theme
    (if dark then Dark else Light)
    (El.div
       [ Component.v (module Toolbar) ~on:(function _ -> .) "Tools"
       ; Context.read theme (fun t -> El.p [txt (theme_class t)]) ] )
