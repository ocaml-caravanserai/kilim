(* A program that uses each function of kilim.router. test/link examines the
   units that it links: the program never runs. *)

let () =
  if Array.length Sys.argv > 1000 then begin
    let r =
      Kilim_router.make ~base:"/a" ~parse:(fun l -> l) ~print:(fun l -> l) ()
    in
    let l = Kilim.Url.parse "/x" in
    ignore (Kilim_router.href r l) ;
    ignore (Kilim_router.current r) ;
    ignore (Kilim_router.push r l) ;
    ignore (Kilim_router.replace r l) ;
    ignore (Kilim_router.back ()) ;
    ignore (Kilim_router.changes r ignore) ;
    ignore (Kilim_router.link r ~on:ignore l []) ;
    ignore (Kilim_router.Private.history ()) ;
    ignore (Kilim_router.Private.listeners ()) ;
    Kilim_router.Private.set_history
      (Some
         { location= (fun () -> "/")
         ; push= ignore
         ; replace= ignore
         ; back= ignore
         ; listen= (fun _ () -> ()) } )
  end
