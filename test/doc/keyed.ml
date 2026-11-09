(* The example of View.keyed in the doc comments of src/core/kilim.mli. *)

open Kilim

type item = {id: int; label: string}

let view (items : item list) : unit View.t =
  View.keyed El.ul
    (List.map (fun it -> (Key.int it.id, El.li [txt it.label])) items)
