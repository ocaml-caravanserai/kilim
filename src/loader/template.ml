let markers name =
  let suffix = match name with None -> "" | Some id -> ":" ^ id in
  ("<!--kilim:head" ^ suffix ^ "-->", "<!--kilim:start" ^ suffix ^ "-->")

(* The positions of [m] in [s], with no overlap, in order. *)
let positions s m =
  let n = String.length s and k = String.length m in
  let rec go i acc =
    if i + k > n then List.rev acc
    else if String.sub s i k = m then go (i + k) (i :: acc)
    else go (i + 1) acc
  in
  go 0 []

let fill ~name ~head ~start page =
  let hm, sm = markers name in
  let once m =
    match positions page m with
    | [i] ->
        Ok i
    | l ->
        Error
          (Printf.sprintf "the marker %s occurs %d times in the template" m
             (List.length l) )
  in
  match (once hm, once sm) with
  | Error e, _ | _, Error e ->
      Error e
  | Ok h, Ok s ->
      (* The two markers do not overlap: each one starts with "<!--kilim:" and
         ends with "-->". *)
      let b = Buffer.create (String.length page) in
      let last =
        List.fold_left
          (fun from (i, m, v) ->
            Buffer.add_string b (String.sub page from (i - from)) ;
            Buffer.add_string b v ;
            i + String.length m )
          0
          (List.sort compare [(h, hm, head); (s, sm, start)])
      in
      Buffer.add_string b (String.sub page last (String.length page - last)) ;
      Ok (Buffer.contents b)
