(* The example of docs/loader.md: a task loads a library that is only an ES
   module. In init or update: Cmd.task load_chart. *)

type msg = Chart_loaded of Jv.t

let load_chart : msg Kilim_browser.Async.t =
  Kilim_browser.Async.map
    (fun m -> Chart_loaded m)
    (Kilim_browser.Async.of_fut_or_error (fun _ ->
         Kilim_web.Es_module.import (Jstr.v "/vendor/chart.mjs") ) )
