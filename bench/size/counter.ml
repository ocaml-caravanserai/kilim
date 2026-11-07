(* The counter of the size budget (docs/budgets.md): one int model, two buttons,
   one text, and Kilim_browser.run. *)

open Kilim

type msg = Inc | Dec

let app =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m -> function
      | Inc -> (m + 1, Cmd.none) | Dec -> (m - 1, Cmd.none) )
    ~view:(fun m ->
      El.div
        [ El.button ~ev:[Ev.click Dec] [txt "-"]
        ; txt (string_of_int m)
        ; El.button ~ev:[Ev.click Inc] [txt "+"] ] )
    ()

let () = Kilim_browser.run ~mount:"app" app
