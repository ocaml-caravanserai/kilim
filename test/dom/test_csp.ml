(* WC: Es_module.import and the JS cell of Async.of_fut under a CSP with no
   'unsafe-eval' (run.mjs gives it to the pages test_csp), in the js_of_ocaml
   and wasm_of_ocaml builds. The two compilers put the JS expressions in the
   output, thus they work with no eval. The page does not use Driver.js or the
   spy: they call eval.

   The checks run before the first drive of run.mjs. A drive is a
   Runtime.evaluate of DevTools, and the page accepts eval in its synchronous
   code (in Chrome and WebKit also in its microtasks, measured). A later
   setTimeout, innerHTML and the wasm compile stay refused, and a navigation
   resets it. Thus the app has a mount element that is not in the document: the
   body did not come yet. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private

type msg = Got of int

(* Not a literal text: js_of_ocaml compiles the eval of a literal text. *)
let refused () =
  let code = Jv.of_string (string_of_int (Sys.opaque_identity 1)) in
  match Jv.call Jv.global "eval" [|code|] with
  | _ ->
      false
  | exception Jv.Error _ ->
      true

let wc () =
  check "WC the CSP refuses eval" (refused ()) ;
  let* m = Kilim_web.Es_module.import (Jstr.v "/mod/web_mod.mjs") in
  check "WC import() of a module"
    ( match m with
    | Ok m ->
        Jv.to_string (Jv.get m "name") = "web_mod"
    | Error _ ->
        false ) ;
  let root = Jv.call document "createElement" [|Jv.of_string "div"|] in
  let a =
    B.start (el root)
      (App.make
         ~init:(fun () ->
           ( 0
           , Cmd.task
               (Kilim_browser.Async.map
                  (fun v -> Got v)
                  (Kilim_browser.Async.of_fut (fun _ -> Fut.return 5)) ) ) )
         ~update:(fun _ (Got v) -> (v, Cmd.none))
         ~view:(fun m ->
           El.span ~at:[At.data "testid" "s"] [txt (string_of_int m)] )
         () )
  in
  let* () = until (fun () -> String.equal (text (q root "s")) "5") in
  check "WC Async.of_fut: the reaction of the cell gives the value"
    (String.equal (text (q root "s")) "5") ;
  B.stop a ;
  check "WC the CSP refuses eval after the checks" (refused ()) ;
  Fut.return ()

let () =
  Fut.await
    (let* () = wc () in
     let* () = release () in
     loaded () )
    finish
