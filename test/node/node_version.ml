let leading_int s =
  let n = String.length s in
  let rec stop i =
    if i < n && s.[i] >= '0' && s.[i] <= '9' then stop (i + 1) else i
  in
  let k = stop 0 in
  if k = 0 then None else int_of_string_opt (String.sub s 0 k)

let parse s =
  let s = String.trim s in
  let s =
    if String.length s > 0 && s.[0] = 'v' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  match String.split_on_char '.' s with
  | a :: b :: c :: _ -> (
    match (leading_int a, leading_int b, leading_int c) with
    | Some a, Some b, Some c ->
        Some (a, b, c)
    | _ ->
        None )
  | _ ->
      None
