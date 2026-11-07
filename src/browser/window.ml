(* Sub.window (design section 08): a passive listener on window for each key.
   The task never ends by itself: the cancel of its key (a removed key, an
   unmount or a stop) removes the listener. *)

let kind : Brr.Ev.Type.void Brr.Ev.t Kilim.Private.Sub.kind =
  Kilim.Private.Sub.kind ()

(* A top-level function, thus the closure keeps only [name] and [deliver]. The
   message function runs after the dispatch, thus it cannot prevent the default:
   the listener is passive. *)
let start name deliver =
  Kilim.Async.make (fun tok ~ok:_ ~error:_ ->
      let fn = Jv.callback ~arity:1 (fun ev -> deliver (Brr.Ev.of_jv ev)) in
      ignore
        (Jv.call Jv.global "addEventListener"
           [|Jv.of_string name; fn; Jv.obj [|("passive", Jv.true')|]|] ) ;
      Kilim.Cancel.on_end tok (fun () ->
          ignore
            (Jv.call Jv.global "removeEventListener" [|Jv.of_string name; fn|]) ) )

let window name f =
  Kilim.Private.Sub.auto kind ~params:name ~start:(start name) f
