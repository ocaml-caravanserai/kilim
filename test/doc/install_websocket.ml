(* The example of src/websocket/browser/kilim_websocket_browser.mli. *)

let app = App.app

let () =
  Kilim_websocket_browser.install () ;
  Kilim_browser.run ~mount:"app" app
