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

(* The URL functions of Kilim.Url (design section 09): the encoding of
   encodeURIComponent, and the query of URLSearchParams. *)

let hex = "0123456789ABCDEF"

(* The characters that encodeURIComponent does not encode. *)
let unreserved c =
  is_alpha c
  || (c >= '0' && c <= '9')
  ||
  match c with
  | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' ->
      true
  | _ ->
      false

let encode_component s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if unreserved c then Buffer.add_char b c
      else begin
        let n = Char.code c in
        Buffer.add_char b '%' ;
        Buffer.add_char b hex.[n lsr 4] ;
        Buffer.add_char b hex.[n land 15]
      end )
    s ;
  Buffer.contents b

(* The value of a hex digit, or -1. *)
let digit c =
  match c with
  | '0' .. '9' ->
      Char.code c - 48
  | 'a' .. 'f' ->
      Char.code c - 87
  | 'A' .. 'F' ->
      Char.code c - 55
  | _ ->
      -1

(* The bytes of [s] with each %XX decoded. A [%] with no two hex digits after it
   stays. With [plus], a [+] is a space. *)
let decode ~plus s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then
      let c = s.[i] in
      if c = '%' && i + 2 < n && digit s.[i + 1] >= 0 && digit s.[i + 2] >= 0
      then begin
        Buffer.add_char b (Char.chr ((digit s.[i + 1] * 16) + digit s.[i + 2])) ;
        go (i + 3)
      end
      else begin
        Buffer.add_char b (if plus && c = '+' then ' ' else c) ;
        go (i + 1)
      end
  in
  go 0 ; Buffer.contents b

let decode_component s = decode ~plus:false s

(* [s] with U+FFFD for each maximal part that is not valid UTF-8, as the UTF-8
   decode of URLSearchParams. *)
let utf_8 s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d) ;
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0 ; Buffer.contents b

(* The pairs of a query with no [?] at the start. *)
let pairs s =
  let form s = utf_8 (decode ~plus:true s) in
  List.filter_map
    (fun part ->
      match String.index_opt part '=' with
      | _ when String.equal part "" ->
          None
      | Some i ->
          let n = String.length part in
          Some
            ( form (String.sub part 0 i)
            , form (String.sub part (i + 1) (n - i - 1)) )
      | None ->
          Some (form part, "") )
    (String.split_on_char '&' s)

let query s =
  if String.length s > 0 && s.[0] = '?' then
    pairs (String.sub s 1 (String.length s - 1))
  else pairs s

let encode_pairs l =
  String.concat "&"
    (List.map (fun (k, v) -> encode_component k ^ "=" ^ encode_component v) l)

(* [s] cut at the first [c]: the part before, and the part after, if [c]
   occurs. *)
let cut c s =
  match String.index_opt s c with
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))
  | None ->
      (s, None)

let with_query url params =
  match params with
  | [] ->
      url
  | _ -> (
      let url, fragment = cut '#' url in
      let sep =
        match cut '?' url with
        | _, None ->
            "?"
        | _, Some q ->
            if String.equal q "" || q.[String.length q - 1] = '&' then ""
            else "&"
      in
      url ^ sep ^ encode_pairs params
      ^ match fragment with Some f -> "#" ^ f | None -> "" )

type location =
  {path: string list; query: (string * string) list; fragment: string option}

let parse s =
  let s, fragment = cut '#' s in
  let path, q = cut '?' s in
  let path =
    if String.length path > 0 && path.[0] = '/' then
      String.sub path 1 (String.length path - 1)
    else path
  in
  (* A first segment "." before "" comes from to_string: it is not part of the
     path. *)
  let segments =
    match String.split_on_char '/' path with
    | "." :: ("" :: _ as rest) ->
        rest
    | l ->
        l
  in
  { path=
      (if String.equal path "" then [] else List.map decode_component segments)
  ; query= (match q with Some q -> pairs q | None -> [])
  ; fragment= Option.map decode_component fragment }

(* A first segment "" before more segments would give "//host", a URL of another
   host. Thus "/." comes first, as the WHATWG serializer writes it, and parse
   removes it. *)
let to_string l =
  let start, segments =
    match (l.path, List.map encode_component l.path) with
    | "" :: _ :: _, segs ->
        ("/./", segs)
    | _, segs ->
        ("/", segs)
  in
  start ^ String.concat "/" segments
  ^ (match l.query with [] -> "" | q -> "?" ^ encode_pairs q)
  ^ match l.fragment with Some f -> "#" ^ encode_component f | None -> ""
