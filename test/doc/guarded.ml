(* The example of View.boundary in the doc comments of src/core/kilim.mli. *)

open Kilim

type msg = Count of int | Retry

let view retries : msg View.t =
  El.div
    [ View.boundary ~key:(string_of_int retries)
        ~fallback:(fun _ ->
          El.p
            [ txt "The counter failed."
            ; El.button ~ev:[Ev.click Retry] [txt "Retry"] ] )
        (Component.v
           (module Counter)
           ~on:(fun (Counter.Changed n) -> Count n)
           {Counter.step= 1; label= "Safe"} ) ]
