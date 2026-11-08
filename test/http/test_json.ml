(* Json and Decode of kilim.http: the corpus of test/json_corpus, the round trip
   of random values, and each decoder with its error path. *)

open Windtrap
module J = Kilim_http.Json
module D = Kilim_http.Decode

let corpus () = Json_corpus.run (fun name ok -> is_true ~msg:name ok)

let int_ranges () = Json_corpus.int_ranges (fun name ok -> is_true ~msg:name ok)

let rec gen depth : J.t Gen.t =
  let leaf =
    Gen.one_of
      [ Gen.constant J.Null
      ; Gen.map (fun b -> J.Bool b) Gen.bool
      ; Gen.map (fun f -> J.Number f) Gen.float
      ; Gen.map (fun s -> J.String s) Gen.string ]
  in
  if depth = 0 then leaf
  else
    let size = Gen.int_range 0 4 in
    Gen.frequency
      [ (3, leaf)
      ; (1, Gen.map (fun l -> J.Array l) (Gen.list ~size (gen (depth - 1))))
      ; ( 1
        , Gen.map
            (fun l -> J.Object l)
            (Gen.list ~size (Gen.pair Gen.string (gen (depth - 1)))) ) ]

let round_trip v =
  let s = J.to_string v in
  is_true ~msg:s (J.parse s = Ok v)

(* Each float parses again from the text of to_string. *)
let number_trip f =
  let s = J.to_string (J.Number f) in
  match J.parse s with
  | Ok (J.Number g) ->
      is_true ~msg:s (Float.equal f g)
  | _ ->
      fail s

let ok d text x =
  match D.decode_string d text with
  | Ok y ->
      is_true ~msg:text (y = x)
  | Error e ->
      fail (text ^ ": " ^ D.error_to_string e)

let err d text path message =
  match D.decode_string d text with
  | Ok _ ->
      fail (text ^ ": no error")
  | Error e ->
      equal
        (pair (list string) string)
        ~msg:text (path, message) (e.path, e.message)

let decoders () =
  let open D in
  ok string "\"a\"" "a" ;
  err string "1" [] "expected a string, got a number" ;
  ok bool "true" true ;
  err bool "null" [] "expected a boolean, got null" ;
  ok float "1.5" 1.5 ;
  err float "\"1\"" [] "expected a number, got a string" ;
  ok int "-3" (-3) ;
  err int "1.5" [] "expected an integer in the range of int" ;
  err int "[]" [] "expected an integer, got an array" ;
  ok (null 7) "null" 7 ;
  err (null 7) "{}" [] "expected null, got an object" ;
  ok (nullable int) "null" None ;
  ok (nullable int) "2" (Some 2) ;
  err (nullable int) "true" [] "expected an integer, got a boolean" ;
  ok (list int) "[1,2,3]" [1; 2; 3] ;
  ok (list int) "[]" [] ;
  err (list int) "[1,2,\"x\"]" ["2"] "expected an integer, got a string" ;
  err (list int) "{}" [] "expected an array, got an object" ;
  ok (field "a" int) "{\"a\":1,\"b\":2}" 1 ;
  ok (field "a" int) "{\"a\":1,\"a\":2}" 2 ;
  err (field "a" int) "{\"b\":1}" [] "no member \"a\"" ;
  err (field "a" int) "[]" [] "expected an object, got an array" ;
  err
    (field "r" (list (field "name" string)))
    "{\"r\":[{\"name\":\"x\"},{\"name\":3}]}" ["r"; "1"; "name"]
    "expected a string, got a number" ;
  ok (field_opt "a" int) "{\"a\":1}" (Some 1) ;
  ok (field_opt "a" int) "{}" None ;
  err (field_opt "a" int) "{\"a\":null}" ["a"] "expected an integer, got null" ;
  err (field_opt "a" int) "1" [] "expected an object, got a number" ;
  ok (index 1 string) "[\"a\",\"b\"]" "b" ;
  ok (index 0 string) "[\"a\",\"b\"]" "a" ;
  err (index 1 int) "[1,\"b\"]" ["1"] "expected an integer, got a string" ;
  err (index 2 int) "[1,2]" [] "no index 2 in an array of 2" ;
  err (index (-1) int) "[1,2]" [] "no index -1 in an array of 2" ;
  err (index 0 int) "{}" [] "expected an array, got an object" ;
  ok value "[1]" (J.Array [J.Number 1.]) ;
  ok (succeed 4) "null" 4 ;
  err (fail "no") "null" [] "no" ;
  ok (map succ int) "1" 2 ;
  err (map succ int) "\"1\"" [] "expected an integer, got a string" ;
  ok (bind (field "k" string) (fun k -> field k int)) "{\"k\":\"v\",\"v\":5}" 5 ;
  err (bind (field "k" string) (fun _ -> int)) "{}" [] "no member \"k\"" ;
  ok (one_of [map string_of_int int; string]) "\"x\"" "x" ;
  ok (one_of [map string_of_int int; string]) "3" "3" ;
  err (one_of [int; null 0]) "\"x\"" [] "expected null, got a string" ;
  err (one_of []) "1" [] "one_of []" ;
  err string "[1" [] "unexpected end at offset 2" ;
  let open Syntax in
  let pair =
    let+ a = field "a" int and+ b = field "b" string in
    (a, b)
  in
  ok pair "{\"a\":1,\"b\":\"x\"}" (1, "x") ;
  err pair "{\"b\":\"x\"}" [] "no member \"a\"" ;
  err pair "{\"a\":1}" [] "no member \"b\""

let to_string () =
  let e path message = D.error_to_string {D.path; message} in
  equal string "m" (e [] "m") ;
  equal string "r.1.name: m" (e ["r"; "1"; "name"] "m")

let tests =
  [ group "json"
      [ test "the corpus parses, rejects and prints" corpus
      ; test "Decode.int gives the range of int of the build" int_ranges
      ; prop "random values: parse of to_string gives the value" (gen 3)
          round_trip
      ; prop "random floats: the shortest text gives the float" Gen.float
          number_trip
      ; test "each decoder, with the path of its error" decoders
      ; test "error_to_string" to_string ] ]
