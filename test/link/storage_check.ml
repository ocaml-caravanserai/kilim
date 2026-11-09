(* A program that uses each function of kilim.storage. test/link examines the
   units that it links: the program never runs. *)

module S = Kilim_storage

let () =
  if Array.length Sys.argv > 1000 then begin
    ignore (S.get Local "k") ;
    ignore (S.set Session "k" "v") ;
    ignore (S.remove Local "k") ;
    ignore (S.error_to_string (Other "x")) ;
    S.Private.set_driver
      (Some
         { get= (fun _ _ -> Ok None)
         ; set= (fun _ _ _ -> Error Quota_exceeded)
         ; remove= (fun _ _ -> Error Unavailable) } ) ;
    ignore (S.Private.driver ())
  end
