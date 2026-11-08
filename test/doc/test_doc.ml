(* The test of the example of the doc comment of
   src/testing/kilim_testing.mli. *)

open Kilim_testing

let () =
  Kilim_testing.mount
    (module Counter)
    {Counter.step= 1; label= "n"}
    (fun page ->
      Page.click page (By.text "+") ;
      assert (Page.outputs page = [Counter.Changed 1]) )
