(* The JSON corpus, in the style of JSONTestSuite: texts that parse, with their
   values, texts that do not, with their errors, and the texts of to_string.
   [run check] gives each case to [check name ok]. *)

open Kilim_http.Json

let deep n = String.make n '[' ^ String.make n ']'

let rec nest n v = if n = 0 then v else nest (n - 1) (Array [v])

let fffd = "\xef\xbf\xbd"

let accepts =
  [ ("null", Null)
  ; ("true", Bool true)
  ; ("false", Bool false)
  ; (" \t\r\n1 \t\r\n", Number 1.)
  ; ("0", Number 0.)
  ; ("-0", Number (-0.))
  ; ("1.5", Number 1.5)
  ; ("-1.5e3", Number (-1500.))
  ; ("1E2", Number 100.)
  ; ("1e+2", Number 100.)
  ; ("1e-2", Number 0.01)
  ; ("0.1", Number 0.1)
  ; ("10.25e1", Number 102.5)
  ; ("123456789", Number 123456789.)
  ; ("9007199254740993", Number 9007199254740992.)
  ; ("5e-324", Number 5e-324)
  ; ("2.2250738585072014e-308", Number 2.2250738585072014e-308)
  ; ("1.7976931348623157e308", Number Float.max_float)
  ; ("1e400", Number Float.infinity)
  ; ("-1e400", Number Float.neg_infinity)
  ; ("[]", Array [])
  ; ("{}", Object [])
  ; (" [ 1 , [ ] , { } ] ", Array [Number 1.; Array []; Object []])
  ; ( "{\"a\" : 1 , \"b\":[null]}"
    , Object [("a", Number 1.); ("b", Array [Null])] )
  ; ("{\"a\":1,\"a\":2}", Object [("a", Number 1.); ("a", Number 2.)])
  ; ("\"\"", String "")
  ; ("\" a \"", String " a ")
  ; ("\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"", String "\"\\/\b\012\n\r\t")
  ; ("\"a\\u00e9b\"", String "a\xc3\xa9b")
  ; ("\"\\u20AC\"", String "\xe2\x82\xac")
  ; ("\"\\ud83d\\ude00\"", String "\xf0\x9f\x98\x80")
  ; ("\"\\ud800\"", String fffd)
  ; ("\"\\udc00x\"", String (fffd ^ "x"))
  ; ("\"\\ud800\\u0041\"", String (fffd ^ "A"))
  ; ("\"\\ud800\\ud800\\udc00\"", String (fffd ^ "\xf0\x90\x80\x80"))
  ; ("\"\\ud800x\"", String (fffd ^ "x"))
  ; ("\"\\udbff\\udfff\"", String "\xf4\x8f\xbf\xbf")
  ; ("\"\\udbff\\ue000\"", String (fffd ^ "\xee\x80\x80"))
  ; ("\"\\udfff\\ud7ff\"", String (fffd ^ "\xed\x9f\xbf"))
  ; ("\"\\udc00\\udbff\"", String (fffd ^ fffd))
  ; ("\"\xc3\xa9\x7f\"", String "\xc3\xa9\x7f")
  ; (deep 512, nest 511 (Array [])) ]

let rejects =
  [ ("", "unexpected end at offset 0")
  ; (" ", "unexpected end at offset 1")
  ; ("nul", "unexpected character at offset 0")
  ; ("nulL", "unexpected character at offset 0")
  ; ("tru", "unexpected character at offset 0")
  ; ("NaN", "unexpected character at offset 0")
  ; ("Infinity", "unexpected character at offset 0")
  ; ("[", "unexpected end at offset 1")
  ; ("[1,]", "unexpected character at offset 3")
  ; ("[,1]", "unexpected character at offset 1")
  ; ("[1 2]", "unexpected character at offset 3")
  ; ("[1}", "unexpected character at offset 2")
  ; ("{\"a\" 1}", "unexpected character at offset 5")
  ; ("{\"a\":1,}", "unexpected character at offset 7")
  ; ("{\"a\":1 \"b\":2}", "unexpected character at offset 7")
  ; ("{1:2}", "unexpected character at offset 1")
  ; ("{\"a\":1]", "unexpected character at offset 6")
  ; ("01", "trailing text at offset 1")
  ; ("0x10", "trailing text at offset 1")
  ; ("1_0", "trailing text at offset 1")
  ; ("[1] x", "trailing text at offset 4")
  ; ("1.", "a digit is necessary at offset 2")
  ; ("1.e1", "a digit is necessary at offset 2")
  ; ("-", "a digit is necessary at offset 1")
  ; ("-a", "a digit is necessary at offset 1")
  ; (".5", "unexpected character at offset 0")
  ; ("+1", "unexpected character at offset 0")
  ; ("1e", "a digit is necessary at offset 2")
  ; ("1e+", "a digit is necessary at offset 3")
  ; ("1E-x", "a digit is necessary at offset 3")
  ; ("\"a", "unexpected end at offset 2")
  ; ("\"\t\"", "a control character in a string at offset 1")
  ; ("\"\x1f\"", "a control character in a string at offset 1")
  ; ("\"\\x\"", "a bad escape at offset 2")
  ; ("\"\\", "unexpected end at offset 2")
  ; ("\"\\u12G4\"", "a hex digit is necessary at offset 5")
  ; ("\"\\u12", "unexpected end at offset 5")
  ; ("\"\\ud800\\u12\"", "a hex digit is necessary at offset 11")
  ; (deep 513, "more than 512 levels at offset 512")
  ; ( String.make 512 '[' ^ "{" ^ String.make 512 ']'
    , "more than 512 levels at offset 512" ) ]

(* The text of to_string: the shortest of %.15g, %.16g and %.17g. *)
let prints =
  [ (Null, "null")
  ; (String "a b~", "\"a b~\"")
  ; (Array [Bool true; Bool false], "[true,false]")
  ; (Number 0.1, "0.1")
  ; (Number 100., "100")
  ; (Number (-0.), "-0")
  ; (Number (1. /. 3.), "0.3333333333333333")
  ; (Number 0.30000000000000004, "0.30000000000000004")
  ; (Number 9007199254740994., "9007199254740994")
  ; (Number 1e21, "1e+21")
  ; (Number 5e-324, "4.94065645841247e-324")
  ; (Number Float.max_float, "1.7976931348623157e+308")
  ; (Number Float.infinity, "null")
  ; (Number Float.nan, "null")
  ; ( String "a\"b\\c\nd\x01\x1f\xc3\xa9"
    , "\"a\\\"b\\\\c\\nd\\u0001\\u001f\xc3\xa9\"" )
  ; (Object [("a", Array []); ("", Object [])], "{\"a\":[],\"\":{}}") ]

let run check =
  List.iter
    (fun (s, v) -> check ("parse " ^ String.escaped s) (parse s = Ok v))
    accepts ;
  List.iter
    (fun (s, e) -> check ("reject " ^ String.escaped s) (parse s = Error e))
    rejects ;
  List.iter
    (fun (v, s) ->
      check ("to_string " ^ String.escaped s) (String.equal (to_string v) s) )
    prints ;
  (* Each finite value of the corpus parses again from its text. *)
  List.iter
    (fun (s, v) ->
      match v with
      | Number f when not (Float.is_finite f) ->
          ()
      | _ ->
          check ("round trip " ^ String.escaped s) (parse (to_string v) = Ok v) )
    accepts

(* Decode.int: an integral number from min_int to max_int of the build. The
   build gives the range: 63 bits in native code, 32 bits under js_of_ocaml and
   31 bits under wasm_of_ocaml. *)
let int_ranges check =
  let expected =
    match Sys.backend_type with
    | Native | Bytecode ->
        63
    | Other "js_of_ocaml" ->
        32
    | Other _ ->
        31
  in
  check ("Sys.int_size is " ^ string_of_int expected) (Sys.int_size = expected) ;
  let module D = Kilim_http.Decode in
  let top = Float.ldexp 1. (expected - 1) in
  let int f = D.decode D.int (Number f) in
  let text f = D.decode_string D.int (to_string (Number f)) in
  check "Decode.int: min_int" (int (-.top) = Ok min_int) ;
  check "Decode.int: min_int from its text" (text (-.top) = Ok min_int) ;
  (* In native code, the float below 2^62 is 2^62 - 512. *)
  let below, largest =
    if expected = 63 then (Float.pred top, max_int - 511)
    else (top -. 1., max_int)
  in
  check "Decode.int: the largest integral float below 2^(n-1)"
    (int below = Ok largest) ;
  check "Decode.int: 2^(n-1) is an error" (Result.is_error (int top)) ;
  check "Decode.int: -2^(n-1) - 1024 is an error"
    (Result.is_error (int (-.top -. 1024.))) ;
  check "Decode.int: 1.5 is an error" (Result.is_error (int 1.5)) ;
  check "Decode.int: 2^53 in native code only"
    (Result.is_ok (int 9007199254740992.) = (expected = 63))
