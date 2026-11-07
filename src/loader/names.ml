let hash bytes =
  String.sub (Digest.BLAKE128.to_hex (Digest.BLAKE128.string bytes)) 0 16

let content name bytes =
  let h = hash bytes in
  match String.rindex_opt name '.' with
  | Some i ->
      String.sub name 0 i ^ "." ^ h ^ String.sub name i (String.length name - i)
  | None ->
      name ^ "." ^ h
