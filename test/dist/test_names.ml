(* The content names. *)

open Windtrap
module N = Kilim_dist.Names

let tests =
  [ group "names"
      [ test "the hash: 16 hexadecimal characters of BLAKE2b-128" (fun () ->
            (* The values of Python: hashlib.blake2b(b, digest_size=16). *)
            equal string "cae66941d9efbd40" (N.hash "") ;
            equal string "cf4ab791c62b8d2b" (N.hash "abc") ;
            equal string (N.hash "abc") (N.hash "abc") ;
            not_equal string (N.hash "abc") (N.hash "abd") )
      ; test "the hash goes before the last extension" (fun () ->
            equal string "main.bc.cf4ab791c62b8d2b.js"
              (N.content "main.bc.js" "abc") ;
            equal string "main.bc.wasm.cf4ab791c62b8d2b.js"
              (N.content "main.bc.wasm.js" "abc") ;
            equal string "loader.cf4ab791c62b8d2b.js"
              (N.content "loader.js" "abc") ;
            equal string "noext.cf4ab791c62b8d2b" (N.content "noext" "abc") ) ]
  ]
