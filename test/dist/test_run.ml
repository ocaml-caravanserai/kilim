(* kilim dist in a temporary directory, with synthetic builds. *)

open Windtrap
open Support

(* A build directory: main.bc.js, the launcher and its .assets directory with
   [wasm] .wasm files and one .map file. *)
let build ?(wasm = ["code-1.wasm"]) dir =
  let b = Filename.concat dir "build" in
  Sys.mkdir b 0o755 ;
  write (Filename.concat b "main.bc.js") "js" ;
  write (Filename.concat b "main.bc.wasm.js") "launcher" ;
  let a = Filename.concat b "main.bc.wasm.assets" in
  Sys.mkdir a 0o755 ;
  List.iter (fun f -> write (Filename.concat a f) "wasm") wasm ;
  write (Filename.concat a "code-1.wasm.map") "map" ;
  write (Filename.concat b "t.html") template ;
  b

let dist ?(extra = []) b out =
  [ "dist"
  ; "--template"
  ; Filename.concat b "t.html"
  ; "--base"
  ; "/s/"
  ; "--js"
  ; Filename.concat b "main.bc.js" ]
  @ extra @ ["-o"; out]

let wasm b = ["--wasm"; Filename.concat b "main.bc.wasm.js"]

(* The hashes of Python: hashlib.blake2b(b, digest_size=16). "js":
   2b8157941b3bd5bc. "launcher": f1497597930520ab. *)
let js_name = "main.bc.2b8157941b3bd5bc.js"

let launcher_name = "main.bc.wasm.f1497597930520ab.js"

let loader_name = Kilim_dist.Names.content "loader.js" text

let page_of parts =
  "<!doctype html>\n<html><head><meta charset=\"utf-8\">" ^ fst parts
  ^ "\n<title>t</title></head>\n<body><div id=\"a\"></div>" ^ snd parts
  ^ "</body></html>\n"

let tests =
  [ group "dist"
      [ test "the files, their names and the page" (fun () ->
            in_dir (fun d ->
                let b = build d in
                let out = Filename.concat d "out" in
                status 0 (dist ~extra:(wasm b) b out) ;
                equal (list string)
                  [ "index.html"
                  ; js_name
                  ; "main.bc.wasm.assets/code-1.wasm"
                  ; "main.bc.wasm.assets/code-1.wasm.map"
                  ; launcher_name ]
                  (files out) ;
                equal string "js" (read (Filename.concat out js_name)) ;
                equal string "launcher"
                  (read (Filename.concat out launcher_name)) ;
                let a =
                  "data-base=\"/s/\" data-js=\"" ^ js_name ^ "\" data-wasm=\""
                  ^ launcher_name
                  ^ "\" data-wasm-main=\"main.bc.wasm.assets/code-1.wasm\""
                in
                equal string
                  (page_of
                     ( "<script " ^ a ^ ">" ^ text ^ "</script>"
                     , "<script " ^ a ^ " data-start>" ^ text ^ "</script>" ) )
                  (read (Filename.concat out "index.html")) ) )
      ; test "the loader.js form, the page path" (fun () ->
            in_dir (fun d ->
                let b = build d in
                let out = Filename.concat d "out" in
                let page = Filename.concat d "page.html" in
                status 0
                  (dist
                     ~extra:(["--form"; "file"; "--page"; page] @ wasm b)
                     b out ) ;
                equal (list string)
                  [ loader_name
                  ; js_name
                  ; "main.bc.wasm.assets/code-1.wasm"
                  ; "main.bc.wasm.assets/code-1.wasm.map"
                  ; launcher_name ]
                  (files out) ;
                equal string text (read (Filename.concat out loader_name)) ;
                let a =
                  " data-base=\"/s/\" data-js=\"" ^ js_name ^ "\" data-wasm=\""
                  ^ launcher_name
                  ^ "\" data-wasm-main=\"main.bc.wasm.assets/code-1.wasm\""
                in
                let src = " src=\"/s/" ^ loader_name ^ "\"" in
                equal string
                  (page_of
                     ( "<script" ^ src ^ " async" ^ a ^ "></script><script"
                       ^ src ^ " defer" ^ a ^ " data-start></script>"
                     , "" ) )
                  (read page) ) )
      ; test "two runs: the same files, no error, earlier files stay" (fun () ->
            in_dir (fun d ->
                let b = build d in
                let out = Filename.concat d "out" in
                status 0 (dist ~extra:(wasm b) b out) ;
                write (Filename.concat out "main.bc.0000000000000000.js") "old" ;
                let before =
                  List.map
                    (fun f -> (f, read (Filename.concat out f)))
                    (files out)
                in
                status 0 (dist ~extra:(wasm b) b out) ;
                equal
                  (list (pair string string))
                  before
                  (List.map
                     (fun f -> (f, read (Filename.concat out f)))
                     (files out) ) ;
                (* A new build: a new name next to the old one. *)
                write (Filename.concat b "main.bc.js") "js2" ;
                status 0 (dist ~extra:(wasm b) b out) ;
                is_true (Sys.file_exists (Filename.concat out js_name)) ;
                is_true
                  (Sys.file_exists
                     (Filename.concat out "main.bc.f7592330757a60d5.js") ) ) )
      ; test "a name with other bytes: no write, the old file stays" (fun () ->
            in_dir (fun d ->
                let b = build d in
                let out = Filename.concat d "out" in
                Sys.mkdir out 0o755 ;
                write (Filename.concat out launcher_name) "other" ;
                let e = error (dist ~extra:(wasm b) b out) in
                is_true ~msg:e (contains e "exists with other bytes") ;
                status 1 (dist ~extra:(wasm b) b out) ;
                equal string "other" (read (Filename.concat out launcher_name)) ;
                (* The conflict comes before the first write: no js copy, no
                   page. *)
                equal (list string) [launcher_name] (files out) ) )
      ; test "a release build has one .wasm file" (fun () ->
            List.iter
              (fun wasm_files ->
                in_dir (fun d ->
                    let b = build ~wasm:wasm_files d in
                    let out = Filename.concat d "out" in
                    let e = error (dist ~extra:(wasm b) b out) in
                    let n = string_of_int (List.length wasm_files) in
                    is_true ~msg:e
                      (contains e
                         ("needs a release build: " ^ n ^ " .wasm files") ) ;
                    status 1 (dist ~extra:(wasm b) b out) ;
                    is_false (Sys.file_exists out) ) )
              [[]; ["code-1.wasm"; "code-2.wasm"]] )
      ; test "the js_of_ocaml build only" (fun () ->
            in_dir (fun d ->
                let b = build d in
                let out = Filename.concat d "out" in
                status 0 (dist ~extra:["--build"; "js"] b out) ;
                equal (list string) ["index.html"; js_name] (files out) ;
                let a =
                  "data-base=\"/s/\" data-js=\"" ^ js_name
                  ^ "\" data-build=\"js\""
                in
                equal string
                  (page_of
                     ( "<script " ^ a ^ ">" ^ text ^ "</script>"
                     , "<script " ^ a ^ " data-start>" ^ text ^ "</script>" ) )
                  (read (Filename.concat out "index.html")) ) )
      ; test "faults of the files" (fun () ->
            in_dir (fun d ->
                let b = build d in
                let out = Filename.concat d "out" in
                let fault extra = status 1 (dist ~extra b out) in
                fault ["--build"; "js"; "--js"; Filename.concat d "none.js"] ;
                fault ["--wasm"; Filename.concat d "none.js"] ;
                (* A launcher with no .js suffix, and a launcher with no .assets
                   directory. *)
                write (Filename.concat b "x.wasm") "" ;
                fault ["--wasm"; Filename.concat b "x.wasm"] ;
                write (Filename.concat b "y.js") "" ;
                fault ["--wasm"; Filename.concat b "y.js"] ;
                fault (["--template"; Filename.concat d "none.html"] @ wasm b) ;
                write (Filename.concat d "bad.html") "<!--kilim:head-->" ;
                let e =
                  error
                    (dist
                       ~extra:
                         (["--template"; Filename.concat d "bad.html"] @ wasm b)
                       b out )
                in
                is_true ~msg:e (contains e "<!--kilim:start--> occurs 0 times") ;
                status 2 (dist ~extra:(["--base"; "https://x/"] @ wasm b) b out) ;
                status 2 (dist ~extra:(["--name"; "A"] @ wasm b) b out) ;
                Sys.mkdir (Filename.concat b "main.bc.wasm.assets/sub") 0o755 ;
                fault (wasm b) ;
                is_false (Sys.file_exists out) ) ) ] ]
