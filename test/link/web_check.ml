(* A program that uses each module of kilim.web. test/link examines the units
   that it links: the program never runs. *)

let () =
  if Array.length Sys.argv > 1000 then begin
    Kilim_web.Microtask.queue Jv.null ;
    ignore (Kilim_web.Js_object.is Jv.null Jv.null) ;
    ignore (Kilim_web.Trusted_types.exists ()) ;
    ignore (Kilim_web.Trusted_types.is_html Jv.null) ;
    ignore (Kilim_web.Es_module.import (Jstr.v "./m.mjs")) ;
    Kilim_web.Scroll.to_ ~left:0. ~top:0. ()
  end
