(* A program that uses each function of kilim.websocket. test/link examines the
   units that it links: the program never runs. *)

module W = Kilim_websocket

let () =
  if Array.length Sys.argv > 1000 then begin
    let s = W.connect ~id:"1" "/ws" (fun _ -> ()) in
    ignore s ;
    ignore (W.send : W.conn -> string -> (unit, W.error) result Kilim.Async.t) ;
    ignore (W.close : W.conn -> unit Kilim.Async.t) ;
    W.Private.set_driver
      (Some
         { connect=
             (fun _ _ ->
               {send= (fun _ -> false); close= ignore; release= ignore} ) } ) ;
    ignore (W.Private.driver ()) ;
    ignore (W.Private.live ())
  end
