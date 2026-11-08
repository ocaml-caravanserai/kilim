(* A program that uses kilim.storage.browser. test/link examines the units that
   it links: the program never runs. *)

let () = if Array.length Sys.argv > 1000 then Kilim_storage_browser.install ()
