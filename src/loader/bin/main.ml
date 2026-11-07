(* The executable kilim: the commands of Kilim_dist.Cli. An error writes one
   line to the standard error. *)

let () =
  let r = Kilim_dist.Cli.run Sys.argv in
  ( match r with
  | Ok s ->
      print_string s
  | Error (Usage s | Fault s) ->
      prerr_endline ("kilim: " ^ s) ) ;
  exit (Kilim_dist.Cli.exit_code r)
