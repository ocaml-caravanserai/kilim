(* The commands and the checks of their arguments. *)

open Windtrap
open Support

(* A page command with a valid template, and [args] at the end: an option that
   comes later replaces the earlier one. *)
let page_with dir args =
  let t = Filename.concat dir "t.html" in
  write t template ;
  [ "page"
  ; "--template"
  ; t
  ; "-o"
  ; Filename.concat dir "o.html"
  ; "--base"
  ; "/s/"
  ; "--js"
  ; "a.bc.js"
  ; "--wasm"
  ; "a.bc.wasm.js" ]
  @ args

let base_refused =
  [""; "s/"; "/s"; "//cdn/"; "http://"; "https:///"; "ftp://x/"; "http:/x/"]

let base_accepted = ["/"; "/s/"; "http://x/"; "https://cdn.example/app/"]

let name_refused = [""; "/a.js"; ".."; "../a.js"; "d/../a.js"; "d/.."]

let name_accepted = ["a.js"; "d/a.js"; "a..b.js"; ".a.js"]

let id_refused = [""; "A"; "a_b"; "a b"; "a/b"]

let id_accepted = ["a"; "app-2"; "0"; "z-9"]

let tests =
  [ group "commands"
      [ test "the base" (fun () ->
            in_dir (fun d ->
                List.iter
                  (fun b -> status 2 (page_with d ["--base"; b]))
                  base_refused ;
                List.iter
                  (fun b -> status 0 (page_with d ["--base"; b]))
                  base_accepted ) )
      ; test "the names of page" (fun () ->
            in_dir (fun d ->
                List.iter
                  (fun opt ->
                    List.iter
                      (fun n -> status 2 (page_with d [opt; n]))
                      name_refused ;
                    List.iter
                      (fun n -> status 0 (page_with d [opt; n]))
                      name_accepted )
                  ["--js"; "--wasm"; "--loader"] ) )
      ; test "the name of the markers" (fun () ->
            in_dir (fun d ->
                List.iter
                  (fun i -> status 2 (page_with d ["--name"; i]))
                  id_refused ;
                (* The template has no marker of an accepted name: a fault of
                   the template, not of the arguments. *)
                List.iter
                  (fun i -> status 1 (page_with d ["--name"; i]))
                  id_accepted ) )
      ; test "the build options" (fun () ->
            in_dir (fun d ->
                status 2 (page_with d ["--build"; "js"]) ;
                status 2 (page_with d ["--build"; "wasm"]) ;
                let no_wasm =
                  List.filter (fun a -> a <> "--wasm" && a <> "a.bc.wasm.js")
                in
                status 2 (no_wasm (page_with d ["--build"; "wasm"])) ;
                status 0 (no_wasm (page_with d ["--build"; "js"])) ;
                status 0 ["page"; "--template"; "x"; "--help"] ;
                status 2
                  [ "page"
                  ; "--template"
                  ; Filename.concat d "t.html"
                  ; "-o"
                  ; Filename.concat d "o.html"
                  ; "--base"
                  ; "/s/"
                  ; "--js"
                  ; "a.bc.js" ] ;
                status 2 (page_with d ["--form"; "other"]) ;
                status 0 (page_with d ["--form"; "file"]) ;
                status 0 (page_with d ["--form"; "inline"]) ) )
      ; test "the usage errors" (fun () ->
            List.iter (status 2)
              [ []
              ; ["other"]
              ; ["page"]
              ; ["page"; "--bogus"]
              ; ["page"; "extra"]
              ; ["loader"]
              ; ["csp"; "extra"]
              ; ["dist"] ] ;
            status 0 ["--help"] ;
            is_true (contains (error ["page"; "--bogus"]) "--bogus") ;
            is_true (contains (error ["other"]) "unknown command: other") )
      ; test "a missing template is a fault" (fun () ->
            in_dir (fun d ->
                status 1
                  (page_with d ["--template"; Filename.concat d "none.html"]) ) )
      ; test "loader and csp" (fun () ->
            in_dir (fun d ->
                let f = Filename.concat d "loader.js" in
                status 0 ["loader"; "-o"; f] ;
                equal string text (read f) ;
                match kilim ["csp"] with
                | Ok s ->
                    equal string (Kilim_dist.Loader_text.csp ^ "\n") s
                | Error _ ->
                    fail "csp" ) )
      ; test "page: the dev parts in the template" (fun () ->
            in_dir (fun d ->
                status 0 (page_with d []) ;
                let p = read (Filename.concat d "o.html") in
                let a =
                  "data-base=\"/s/\" data-js=\"a.bc.js\" \
                   data-wasm=\"a.bc.wasm.js\" data-dev"
                in
                equal string
                  ( "<!doctype html>\n\
                     <html><head><meta charset=\"utf-8\"><script " ^ a ^ ">"
                  ^ text
                  ^ "</script>\n\
                     <title>t</title></head>\n\
                     <body><div id=\"a\"></div><script " ^ a ^ " data-start>"
                  ^ text ^ "</script></body></html>\n" )
                  p ) )
      ; test "page: two runs give two apps" (fun () ->
            in_dir (fun d ->
                let t = Filename.concat d "t.html" in
                write t
                  "<!--kilim:head:a--><!--kilim:head:b--><!--kilim:start:a--><!--kilim:start:b-->" ;
                let run id js out tmpl =
                  status 0
                    [ "page"
                    ; "--template"
                    ; tmpl
                    ; "-o"
                    ; out
                    ; "--base"
                    ; "/s/"
                    ; "--js"
                    ; js
                    ; "--build"
                    ; "js"
                    ; "--name"
                    ; id ]
                in
                let o1 = Filename.concat d "o1.html"
                and o2 = Filename.concat d "o2.html" in
                run "a" "a.bc.js" o1 t ;
                run "b" "b.bc.js" o2 o1 ;
                let part js start =
                  "<script data-base=\"/s/\" data-js=\"" ^ js
                  ^ "\" data-build=\"js\" data-dev" ^ start ^ ">" ^ text
                  ^ "</script>"
                in
                equal string
                  ( part "a.bc.js" "" ^ part "b.bc.js" ""
                  ^ part "a.bc.js" " data-start"
                  ^ part "b.bc.js" " data-start" )
                  (read o2) ) ) ] ]
