type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list

let max_depth = 512

(* The error and its offset. *)
exception Error of int * string

type p = {s: string; mutable i: int}

let fail p msg = raise_notrace (Error (p.i, msg))

let at_end p = p.i >= String.length p.s

let rec skip p =
  if not (at_end p) then
    match p.s.[p.i] with
    | ' ' | '\t' | '\n' | '\r' ->
        p.i <- p.i + 1 ;
        skip p
    | _ ->
        ()

(* The character at the offset. The end of the text is an error. *)
let current p = if at_end p then fail p "unexpected end" else p.s.[p.i]

let literal p word v =
  let n = String.length word in
  if p.i + n <= String.length p.s && String.equal (String.sub p.s p.i n) word
  then begin
    p.i <- p.i + n ;
    v
  end
  else fail p "unexpected character"

let is_digit p = (not (at_end p)) && p.s.[p.i] >= '0' && p.s.[p.i] <= '9'

let rec digits p =
  if is_digit p then begin
    p.i <- p.i + 1 ;
    digits p
  end

(* At least one digit. *)
let some_digits p =
  if is_digit p then digits p else fail p "a digit is necessary"

let advance_if p c =
  if (not (at_end p)) && p.s.[p.i] = c then begin
    p.i <- p.i + 1 ;
    true
  end
  else false

(* The grammar of RFC 8259 first, thus float_of_string gets no OCaml form (for
   example "0x1p3" or "1_0"). *)
let number p =
  let start = p.i in
  ignore (advance_if p '-') ;
  if advance_if p '0' then () else some_digits p ;
  if advance_if p '.' then some_digits p ;
  if advance_if p 'e' || advance_if p 'E' then begin
    if not (advance_if p '+') then ignore (advance_if p '-') ;
    some_digits p
  end ;
  Number (float_of_string (String.sub p.s start (p.i - start)))

let hex p =
  match current p with
  | '0' .. '9' as c ->
      Char.code c - 48
  | 'a' .. 'f' as c ->
      Char.code c - 87
  | 'A' .. 'F' as c ->
      Char.code c - 55
  | _ ->
      fail p "a hex digit is necessary"

let hex4 p =
  let n = ref 0 in
  for _ = 1 to 4 do
    n := (!n * 16) + hex p ;
    p.i <- p.i + 1
  done ;
  !n

(* After "\u": a surrogate pair gives one character, a lone surrogate gives
   U+FFFD. *)
let unicode p b =
  let u = hex4 p in
  let c =
    if u >= 0xD800 && u <= 0xDBFF then
      let back = p.i in
      if advance_if p '\\' && advance_if p 'u' then
        let l = hex4 p in
        if l >= 0xDC00 && l <= 0xDFFF then
          0x10000 + ((u - 0xD800) lsl 10) + (l - 0xDC00)
        else begin
          (* The next escape is not a low surrogate: it stays. *)
          p.i <- back ;
          0xFFFD
        end
      else begin
        p.i <- back ;
        0xFFFD
      end
    else if u >= 0xDC00 && u <= 0xDFFF then 0xFFFD
    else u
  in
  Buffer.add_utf_8_uchar b (Uchar.of_int c)

let escape p b =
  let c = current p in
  p.i <- p.i + 1 ;
  match c with
  | '"' | '\\' | '/' ->
      Buffer.add_char b c
  | 'b' ->
      Buffer.add_char b '\b'
  | 'f' ->
      Buffer.add_char b '\012'
  | 'n' ->
      Buffer.add_char b '\n'
  | 'r' ->
      Buffer.add_char b '\r'
  | 't' ->
      Buffer.add_char b '\t'
  | 'u' ->
      unicode p b
  | _ ->
      p.i <- p.i - 1 ;
      fail p "a bad escape"

(* After the opening quote. A run of plain bytes goes to the buffer in one
   copy. *)
let string p =
  let b = Buffer.create 16 in
  let rec go start =
    match current p with
    | '"' ->
        Buffer.add_substring b p.s start (p.i - start) ;
        p.i <- p.i + 1
    | '\\' ->
        Buffer.add_substring b p.s start (p.i - start) ;
        p.i <- p.i + 1 ;
        escape p b ;
        go p.i
    | c when c < ' ' ->
        fail p "a control character in a string"
    | _ ->
        p.i <- p.i + 1 ;
        go start
  in
  go p.i ; Buffer.contents b

let expect p c =
  if current p = c then p.i <- p.i + 1 else fail p "unexpected character"

let rec value p depth =
  skip p ;
  match current p with
  | '{' ->
      let depth = deeper p depth in
      p.i <- p.i + 1 ;
      Object (members p depth)
  | '[' ->
      let depth = deeper p depth in
      p.i <- p.i + 1 ;
      Array (elements p depth)
  | '"' ->
      p.i <- p.i + 1 ;
      String (string p)
  | 't' ->
      literal p "true" (Bool true)
  | 'f' ->
      literal p "false" (Bool false)
  | 'n' ->
      literal p "null" Null
  | '-' | '0' .. '9' ->
      number p
  | _ ->
      fail p "unexpected character"

(* At the "[" or "{". *)
and deeper p depth =
  if depth >= max_depth then fail p "more than 512 levels" else depth + 1

(* After "[". *)
and elements p depth =
  skip p ;
  if advance_if p ']' then [] else elements_next p depth []

(* A value is necessary: after "[" with no "]", or after a ",". *)
and elements_next p depth acc =
  let v = value p depth in
  skip p ;
  if advance_if p ',' then elements_next p depth (v :: acc)
  else begin
    expect p ']' ;
    List.rev (v :: acc)
  end

(* After "{". *)
and members p depth =
  skip p ;
  if advance_if p '}' then [] else member p depth []

and member p depth acc =
  skip p ;
  expect p '"' ;
  let k = string p in
  skip p ;
  expect p ':' ;
  let v = value p depth in
  skip p ;
  if advance_if p ',' then member p depth ((k, v) :: acc)
  else begin
    expect p '}' ;
    List.rev ((k, v) :: acc)
  end

let parse s =
  let p = {s; i= 0} in
  match value p 0 with
  | v ->
      skip p ;
      if at_end p then Ok v
      else Error ("trailing text at offset " ^ string_of_int p.i)
  | exception Error (i, msg) ->
      Error (msg ^ " at offset " ^ string_of_int i)

external format_float : string -> float -> string = "caml_format_float"

let number_text f =
  if Float.is_finite f then
    let s = format_float "%.15g" f in
    if Float.equal (float_of_string s) f then s
    else
      let s = format_float "%.16g" f in
      if Float.equal (float_of_string s) f then s else format_float "%.17g" f
  else "null"

let hexdigit n = "0123456789abcdef".[n]

let add_string b s =
  Buffer.add_char b '"' ;
  String.iter
    (fun c ->
      match c with
      | '"' ->
          Buffer.add_string b "\\\""
      | '\\' ->
          Buffer.add_string b "\\\\"
      | '\n' ->
          Buffer.add_string b "\\n"
      | c when c < ' ' ->
          Buffer.add_string b "\\u00" ;
          Buffer.add_char b (hexdigit (Char.code c lsr 4)) ;
          Buffer.add_char b (hexdigit (Char.code c land 15))
      | c ->
          Buffer.add_char b c )
    s ;
  Buffer.add_char b '"'

let to_string v =
  let b = Buffer.create 64 in
  let rec go = function
    | Null ->
        Buffer.add_string b "null"
    | Bool x ->
        Buffer.add_string b (if x then "true" else "false")
    | Number f ->
        Buffer.add_string b (number_text f)
    | String s ->
        add_string b s
    | Array l ->
        Buffer.add_char b '[' ;
        List.iteri
          (fun i v ->
            if i > 0 then Buffer.add_char b ',' ;
            go v )
          l ;
        Buffer.add_char b ']'
    | Object l ->
        Buffer.add_char b '{' ;
        List.iteri
          (fun i (k, v) ->
            if i > 0 then Buffer.add_char b ',' ;
            add_string b k ;
            Buffer.add_char b ':' ;
            go v )
          l ;
        Buffer.add_char b '}'
  in
  go v ; Buffer.contents b
