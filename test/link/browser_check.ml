(* A program that uses each module of kilim.browser. test/link examines the
   units that it links: the program never runs. *)

let () =
  let app =
    Kilim.App.make
      ~init:(fun () -> ((), Kilim.Cmd.none))
      ~update:(fun () () -> ((), Kilim.Cmd.none))
      ~view:(fun () -> Kilim.txt "")
      ~subs:(fun () -> Kilim_browser.Sub.window "resize" (fun _ -> ()))
      ()
  in
  if Array.length Sys.argv > 1000 then begin
    Kilim_browser.run ~mount:"app" app ;
    let a = Kilim_browser.Private.start (Brr.El.of_jv Jv.null) app in
    ignore (Kilim_browser.Private.ops a) ;
    ignore (Kilim_browser.Private.runtime a) ;
    Kilim_browser.Private.stop a ;
    ignore Kilim_browser.Private.host ;
    ignore (Kilim_browser.Private.live_timers ()) ;
    ignore (Kilim_browser.Private.cell Jv.null)
  end
