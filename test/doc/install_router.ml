(* The example of src/router/browser/kilim_router_browser.mli. *)

let () =
  Kilim_router_browser.install () ;
  Kilim_browser.run ~mount:"app" App.app
