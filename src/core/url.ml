let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_scheme_char c =
  is_alpha c || (c >= '0' && c <= '9') || c = '+' || c = '-' || c = '.'

let is_tab_or_newline c = c = '\t' || c = '\n' || c = '\r'

(* The scheme of [s] in lowercase, or [""] for a relative URL. The check does
   not remove the C0 control characters and the spaces at the end: they cannot
   change the scheme. *)
let scheme s =
  let n = String.length s in
  let b = Buffer.create 8 in
  (* [start] skips the C0 control characters and the spaces at the start: all
     characters that are not more than [' ']. *)
  let rec start i = if i < n && s.[i] <= ' ' then start (i + 1) else first i
  and first i =
    if i = n then ""
    else
      let c = s.[i] in
      if is_alpha c then begin
        Buffer.add_char b (Char.lowercase_ascii c) ;
        rest (i + 1)
      end
      else ""
  and rest i =
    if i = n then ""
    else
      let c = s.[i] in
      if is_tab_or_newline c then rest (i + 1)
      else if c = ':' then Buffer.contents b
      else if is_scheme_char c then begin
        Buffer.add_char b (Char.lowercase_ascii c) ;
        rest (i + 1)
      end
      else ""
  in
  start 0

let permitted s =
  match scheme s with
  | "" | "http" | "https" | "mailto" | "tel" ->
      true
  | _ ->
      false

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\x0c' || c = '\r'

(* The end of the sequence from [i] whose characters satisfy [p]. *)
let rec skip p s i =
  if i < String.length s && p s.[i] then skip p s (i + 1) else i

(* The position after the descriptors of a srcset candidate: after the comma
   that ends them, outside parentheses, or at the end. *)
let rec descriptors s i parens =
  if i = String.length s then i
  else
    let c = s.[i] in
    if parens then descriptors s (i + 1) (c <> ')')
    else if c = ',' then i + 1
    else descriptors s (i + 1) (c = '(')

(* The WHATWG algorithm "parse a srcset attribute": each URL is a sequence of
   characters that are not ASCII whitespace. If the URL ends with a comma, no
   descriptors follow it. *)
let rec srcset_ok s i =
  let i = skip (fun c -> is_space c || c = ',') s i in
  if i = String.length s then true
  else
    let j = skip (fun c -> not (is_space c)) s i in
    permitted (String.sub s i (j - i))
    && srcset_ok s (if s.[j - 1] = ',' then j else descriptors s j false)

let rec list_ok s i =
  let i = skip is_space s i in
  if i = String.length s then true
  else
    let j = skip (fun c -> not (is_space c)) s i in
    permitted (String.sub s i (j - i)) && list_ok s j

let attribute_ok name value =
  match name with
  | "href"
  | "src"
  | "action"
  | "formaction"
  | "poster"
  | "cite"
  | "data"
  | "background" ->
      permitted value
  | "srcset" ->
      srcset_ok value 0
  | "ping" ->
      list_ok value 0
  | _ ->
      true
