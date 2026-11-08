(* The parent of the examples of the doc comments of src/core/kilim.mli. *)

open Kilim

type model = {resets: int}

type msg = Count of int

let view m : msg View.t =
  El.div
    [ Component.v
        (module Counter)
        ~key:("left/" ^ string_of_int m.resets)
        ~on:(fun (Counter.Changed n) -> Count n)
        {Counter.step= 1; label= "Left"}
    ; Component.v (module Light) ~on:(function _ -> .) () ]
