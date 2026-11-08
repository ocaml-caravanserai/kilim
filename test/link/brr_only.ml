(* A program that uses the modules of Brr that kilim.browser and kilim.web use:
   Jv, Jstr, Fut, Brr.El and Brr.Ev. Brr already links Printf, thus test/link
   compares the units of browser_check with the units of this program. *)

let () =
  if Array.length Sys.argv > 1000 then begin
    ignore (Jv.get Jv.global "document") ;
    ignore (Brr.El.of_jv Jv.null) ;
    ignore (Brr.Ev.of_jv Jv.null : Brr.Ev.Type.void Brr.Ev.t) ;
    ignore (Jstr.v "") ;
    ignore (Fut.of_promise ~ok:(fun v -> v) Jv.null) ;
    ignore (Fut.to_promise' ~ok:Jv.repr ~error:Jv.repr (Fut.return (Ok ())))
  end
