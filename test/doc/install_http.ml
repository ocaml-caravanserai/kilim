(* The example of src/http/browser/kilim_http_browser.mli. *)

let app = App.app

let () =
  Kilim_http_browser.install () ;
  Kilim_browser.run ~mount:"app" app
