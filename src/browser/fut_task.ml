(* Async.of_fut (design section 12). A top-level start function: [ok] goes only
   into Jv.callback, and the promise keeps only the reaction of the JS cell. The
   cancel and the normal end of the step clear the cell. If js_of_ocaml inlines
   Brr code here, the JS context of [start] holds no continuation after the
   clear: Async.make clears its own cell too. *)

let start make tok ~ok =
  let cell =
    Fut_cell.create (Jv.callback ~arity:1 (fun v -> ok (Jv.Id.of_jv v)))
  in
  Kilim.Cancel.on_end tok (fun () -> Fut_cell.clear cell) ;
  let fut = Fut.map Result.ok (make tok) in
  let p = Fut.to_promise' ~ok:Jv.repr ~error:Jv.repr fut in
  ignore (Jv.call p "then" [|Fut_cell.reaction cell|]) ;
  cell

let of_fut make =
  Kilim.Async.make (fun tok ~ok ~error:_ -> ignore (start make tok ~ok))

let of_fut_or_error make =
  Kilim.Async.bind (of_fut make) (function
    | Ok v ->
        Kilim.Async.return v
    | Error e ->
        Kilim.Async.fail (Jv.Error e) )
