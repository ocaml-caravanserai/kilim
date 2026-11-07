(* The HTML of the parts. *)

open Windtrap
open Support
module P = Kilim_dist.Part

let dev =
  { P.base= "/s/"
  ; js= "a.bc.js"
  ; build= P.Wasm {launcher= "a.bc.wasm.js"; main= None}
  ; dev= true }

let release =
  { dev with
    P.build=
      P.Wasm {launcher= "a.bc.wasm.1.js"; main= Some "a.bc.wasm.assets/m.wasm"}
  ; dev= false }

let js_only = {dev with P.build= P.Js}

let inline name t ~head ~start =
  test name (fun () ->
      equal string ~msg:"head" (head ^ text ^ "</script>") (P.head P.Inline t) ;
      equal string ~msg:"start"
        (start ^ text ^ "</script>")
        (P.start P.Inline t) )

let tests =
  [ group "parts"
      [ inline "inline, dev" dev
          ~head:
            "<script data-base=\"/s/\" data-js=\"a.bc.js\" \
             data-wasm=\"a.bc.wasm.js\" data-dev>"
          ~start:
            "<script data-base=\"/s/\" data-js=\"a.bc.js\" \
             data-wasm=\"a.bc.wasm.js\" data-dev data-start>"
      ; inline "inline, release with the main .wasm" release
          ~head:
            "<script data-base=\"/s/\" data-js=\"a.bc.js\" \
             data-wasm=\"a.bc.wasm.1.js\" \
             data-wasm-main=\"a.bc.wasm.assets/m.wasm\">"
          ~start:
            "<script data-base=\"/s/\" data-js=\"a.bc.js\" \
             data-wasm=\"a.bc.wasm.1.js\" \
             data-wasm-main=\"a.bc.wasm.assets/m.wasm\" data-start>"
      ; inline "inline, the js_of_ocaml build only" js_only
          ~head:
            "<script data-base=\"/s/\" data-js=\"a.bc.js\" data-build=\"js\" \
             data-dev>"
          ~start:
            "<script data-base=\"/s/\" data-js=\"a.bc.js\" data-build=\"js\" \
             data-dev data-start>"
      ; test "the loader.js form: two tags at the head, nothing at the start"
          (fun () ->
            let a =
              " data-base=\"/s/\" data-js=\"a.bc.js\" \
               data-wasm=\"a.bc.wasm.js\" data-dev"
            in
            equal string
              ( "<script src=\"/s/l.js\" async" ^ a
              ^ "></script><script src=\"/s/l.js\" defer" ^ a
              ^ " data-start></script>" )
              (P.head (P.File "l.js") dev) ;
            equal string "" (P.start (P.File "l.js") dev) )
      ; test "the escape of an attribute" (fun () ->
            equal string "a&amp;&quot;&lt;&gt;'b" (P.escape "a&\"<>'b") ;
            let t = {js_only with P.base= "/&\"<>/"; js= "x&.js"} in
            let h = P.head (P.File "l&.js") t in
            is_true ~msg:h
              ( contains h "src=\"/&amp;&quot;&lt;&gt;/l&amp;.js\""
              && contains h
                   "data-base=\"/&amp;&quot;&lt;&gt;/\" data-js=\"x&amp;.js\""
              ) ) ] ]
