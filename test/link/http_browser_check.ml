(* A program that uses kilim.http.browser. test/link examines the units that it
   links: the program never runs. *)

let () =
  if Array.length Sys.argv > 1000 then begin
    Kilim_http_browser.install () ;
    ignore (Kilim_http.get "/x")
  end
