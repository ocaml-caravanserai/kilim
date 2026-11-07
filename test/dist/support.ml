(* Files and commands for the tests of kilim_dist. *)

open Windtrap

let read f = In_channel.with_open_bin f In_channel.input_all

let write f s = Out_channel.with_open_bin f (fun oc -> output_string oc s)

let rec remove p =
  if Sys.is_directory p then begin
    Array.iter (fun f -> remove (Filename.concat p f)) (Sys.readdir p) ;
    Sys.rmdir p
  end
  else Sys.remove p

(* [in_dir f] runs [f dir] in a new temporary directory, then removes it. *)
let in_dir f =
  let d = Filename.temp_dir "kilim-dist-" "" in
  Fun.protect ~finally:(fun () -> remove d) (fun () -> f d)

(* The files below [dir], relative and sorted. *)
let rec files ?(rel = "") dir =
  Sys.readdir dir |> Array.to_list
  |> List.concat_map (fun f ->
      let r = if rel = "" then f else rel ^ "/" ^ f in
      let p = Filename.concat dir f in
      if Sys.is_directory p then files ~rel:r p else [r] )
  |> List.sort compare

let kilim args = Kilim_dist.Cli.run (Array.of_list ("kilim" :: args))

let code args = Kilim_dist.Cli.exit_code (kilim args)

(* The command gives exit status [n]. *)
let status n args =
  let r = kilim args in
  let msg =
    String.concat " " args ^ ": "
    ^
    match r with
    | Ok _ ->
        "ok"
    | Error (Usage s) ->
        "usage: " ^ s
    | Error (Fault s) ->
        "fault: " ^ s
  in
  equal int ~msg n (Kilim_dist.Cli.exit_code r)

let error args =
  match kilim args with
  | Error (Usage s | Fault s) ->
      s
  | Ok _ ->
      fail "no error"

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let text = Kilim_dist.Loader_text.text

let template =
  "<!doctype html>\n\
   <html><head><meta charset=\"utf-8\"><!--kilim:head-->\n\
   <title>t</title></head>\n\
   <body><div id=\"a\"></div><!--kilim:start--></body></html>\n"
