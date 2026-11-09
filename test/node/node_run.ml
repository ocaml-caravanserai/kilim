(* The guard of the rules that run node (dune in this folder). *)

let minimum = (22, 3, 0)

let show (a, b, c) = Printf.sprintf "%d.%d.%d" a b c

(* The output of `node --version`. Exit status 127 of the child: no node in
   PATH. *)
let version () =
  match Unix.open_process_args_in "node" [|"node"; "--version"|] with
  | exception Unix.Unix_error _ ->
      `Missing
  | ic -> (
      let line = try Some (input_line ic) with End_of_file -> None in
      match (Unix.close_process_in ic, line) with
      | Unix.WEXITED 0, Some l ->
          `Version l
      | Unix.WEXITED 127, _ ->
          `Missing
      | _ ->
          `No_version
      | exception Unix.Unix_error _ ->
          `No_version )

let copy file =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic ; print_string s

let skip ~or_copy reason =
  prerr_endline ("SKIP: no node (" ^ reason ^ ")") ;
  if Sys.getenv_opt "KILIM_REQUIRE_NODE" = Some "1" then exit 1 ;
  Option.iter copy or_copy ;
  exit 0

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let or_copy, args =
    match args with
    | "--or-copy" :: file :: rest ->
        (Some file, rest)
    | _ ->
        (None, args)
  in
  let need = "node " ^ show minimum ^ " or later is necessary" in
  match version () with
  | `Missing ->
      skip ~or_copy (need ^ ", and node is not in PATH")
  | `No_version ->
      skip ~or_copy (need ^ ", and node gave no version")
  | `Version v -> (
    match Node_version.parse v with
    | Some n when compare n minimum >= 0 ->
        Unix.execvp "node" (Array.of_list ("node" :: args))
    | _ ->
        skip ~or_copy (need ^ ", and PATH has node " ^ String.trim v) )
