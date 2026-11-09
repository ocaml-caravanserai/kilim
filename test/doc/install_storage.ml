(* The example of src/storage/browser/kilim_storage_browser.mli. *)

let app = App.app

let () =
  Kilim_storage_browser.install () ;
  Kilim_browser.run ~mount:"app" app
