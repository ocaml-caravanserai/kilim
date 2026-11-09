(* The parser of node --version (no windtrap: this folder has no test
   dependency). *)

let cases =
  [ ("v22.3.0", Some (22, 3, 0))
  ; ("v20.19.6\n", Some (20, 19, 6))
  ; ("v24.0.0-rc.1", Some (24, 0, 0))
  ; ("v25.0.0-nightly20260101abc", Some (25, 0, 0))
  ; ("22.3.0", Some (22, 3, 0))
  ; ("v22.3", None)
  ; ("vx.1.2", None)
  ; ("", None) ]

let () =
  let bad = List.filter (fun (s, want) -> Node_version.parse s <> want) cases in
  List.iter (fun (s, _) -> Printf.printf "FAIL %S\n" s) bad ;
  if bad <> [] then exit 1 ;
  Printf.printf "node_version: %d cases pass\n" (List.length cases)
