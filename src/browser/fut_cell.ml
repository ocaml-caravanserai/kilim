(* The JS cell of Async.of_fut (design section 12): a JS object with one
   property, kilimFut, that holds the JS function of the continuation. The
   reaction is a JS function that holds only the cell: it takes the function out
   of the cell, then calls it. Thus it calls the function one time only, and a
   cleared cell calls nothing. A promise that never settles keeps only the
   reaction and the cell: after the clear, it keeps no OCaml value. *)

external pure_js_expr : string -> 'a = "caml_pure_js_expr"

type t = Jv.t

(* A JS expression, not an OCaml closure: an OCaml closure would keep the JS
   context of its function (design section 12). *)
let make_reaction : Jv.t =
  pure_js_expr
    "(function(c){return function(v){var \
     k=c.kilimFut;c.kilimFut=undefined;if(k)k(v)}})"

let create k : t = Jv.obj [|("kilimFut", k)|]

let reaction c = Jv.apply make_reaction [|c|]

let clear c = Jv.set c "kilimFut" Jv.undefined

let is_empty c = Jv.is_undefined (Jv.get c "kilimFut")
