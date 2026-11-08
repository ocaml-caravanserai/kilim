(* Kilim.Url (design sections 09 and 13): the encoding of encodeURIComponent,
   the query of URLSearchParams and the location of the router. The corpus is in
   test/url_corpus: U1 of test/dom/test_web.ml compares it with the browsers. *)

open Windtrap
module U = Kilim.Url
module C = Url_corpus

let pairs = list (pair string string)

(* All bytes from 0 to 255: the unreserved characters stay, the others become
   %XX with uppercase digits. *)
let encode_bytes () =
  let all = String.init 256 Char.chr in
  let e = U.encode_component all in
  let expected =
    String.concat ""
      (List.init 256 (fun i ->
           let c = Char.chr i in
           match c with
           | 'a' .. 'z'
           | 'A' .. 'Z'
           | '0' .. '9'
           | '-'
           | '_'
           | '.'
           | '!'
           | '~'
           | '*'
           | '\''
           | '('
           | ')' ->
               String.make 1 c
           | _ ->
               let h = "0123456789ABCDEF" in
               String.init 3 (function
                 | 0 ->
                     '%'
                 | 1 ->
                     h.[i lsr 4]
                 | _ ->
                     h.[i land 15] ) ) )
  in
  equal string expected e ;
  equal string all (U.decode_component e) ;
  equal string "a%20b%2B%26%3D%2F%3F%23%25" (U.encode_component "a b+&=/?#%") ;
  equal string "%C3%A9%E2%82%AC%F0%9F%98%80" (U.encode_component "é€😀") ;
  equal string "" (U.encode_component "")

let decode_corpus () =
  List.iter
    (fun (s, d) -> equal string ~msg:s d (U.decode_component s))
    (C.decodes @ C.escapes)

let query_corpus () =
  List.iter (fun (s, q) -> equal pairs ~msg:s q (U.query s)) C.queries

let with_query_corpus () =
  List.iter
    (fun (url, q, r) -> equal string ~msg:url r (U.with_query url q))
    C.with_queries ;
  (* The query of the result gives the pairs again. *)
  let q = [("a b", "c&d"); ("é=", "+%"); ("", "x")] in
  equal pairs q (U.query (U.with_query "" q))

let location l = (l.U.path, l.U.query, l.U.fragment)

let loc = triple (list string) pairs (option string)

let parse_corpus () =
  List.iter (fun (s, l) -> equal loc ~msg:s l (location (U.parse s))) C.parses

let print_corpus () =
  List.iter
    (fun (l, s) ->
      equal string ~msg:s s (U.to_string l) ;
      (* No URL of the corpus starts with "//": a URL of another host. *)
      is_false ~msg:s (String.starts_with ~prefix:"//" (U.to_string l)) )
    C.prints

(* Random UTF-8 text, with many of the characters that the URL syntax uses. *)
let text =
  let special =
    Gen.map
      (fun i -> Uchar.of_char "%+&=/?# ~'*\x00\x7f".[i])
      (Gen.int_range 0 12)
  in
  Gen.map
    (fun l ->
      let b = Buffer.create 16 in
      List.iter (Buffer.add_utf_8_uchar b) l ;
      Buffer.contents b )
    (Gen.list ~size:(Gen.int_range 0 12)
       (Gen.frequency [(1, special); (2, Gen.uchar)]) )

(* Random bytes, also bytes that are not UTF-8. *)
let raw = Gen.string_of ~size:(Gen.int_range 0 12) Gen.char

let round_trip s =
  let e = U.encode_component s in
  equal string s (U.decode_component e) ;
  equal pairs [("a", s)] (U.query ("a=" ^ e))

let bytes_round_trip s =
  equal string s (U.decode_component (U.encode_component s))

(* A segment of a path: often "", or a dot segment, which a browser removes. *)
let segment =
  Gen.frequency
    [ (2, Gen.constant "")
    ; (1, Gen.constant ".")
    ; (1, Gen.constant "..")
    ; (4, raw) ]

let locations =
  let open Gen in
  let+ path = list ~size:(int_range 0 4) segment
  and+ query = list ~size:(int_range 0 3) (pair text text)
  and+ fragment = option raw in
  {U.path; query; fragment}

let location_round_trip l =
  let s = U.to_string l in
  is_false ~msg:s (String.starts_with ~prefix:"//" s) ;
  (* A browser removes a dot segment: the round trip holds without them. *)
  let dot = List.exists (fun x -> x = "." || x = "..") l.U.path in
  if l.U.path <> [""] && not dot then
    equal loc (location l) (location (U.parse s))

let tests =
  [ group "url"
      [ test "encode_component encodes each byte as encodeURIComponent"
          encode_bytes
      ; test "decode_component decodes %XX, and a bad %XX stays" decode_corpus
      ; test "query gives the pairs of URLSearchParams" query_corpus
      ; test "with_query adds the pairs before the fragment" with_query_corpus
      ; test "parse gives the path, the query and the fragment" parse_corpus
      ; test "to_string gives the URL of a location" print_corpus
      ; prop "random UTF-8: decode, and the query, give the text again" text
          round_trip
      ; prop "random bytes: decode gives the bytes again" raw bytes_round_trip
      ; prop "random locations: parse of to_string gives the location" locations
          location_round_trip ] ]
