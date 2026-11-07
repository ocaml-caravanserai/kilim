(* The counter example: the model is an int. The messages change it, and the
   view shows it between two buttons. *)

open Kilim

type msg = Inc | Dec | Reset

let update n = function
  | Inc ->
      (n + 1, Cmd.none)
  | Dec ->
      (n - 1, Cmd.none)
  | Reset ->
      (0, Cmd.none)

let view n =
  El.div
    [ El.button ~at:[At.aria_label "Decrement"] ~ev:[Ev.click Dec] [txt "-"]
    ; El.output ~at:[At.data "testid" "count"] [txt (string_of_int n)]
    ; El.button ~at:[At.aria_label "Increment"] ~ev:[Ev.click Inc] [txt "+"]
    ; El.button ~ev:[Ev.click Reset] [txt "Reset"] ]

let app = App.make ~init:(fun () -> (0, Cmd.none)) ~update ~view ()
