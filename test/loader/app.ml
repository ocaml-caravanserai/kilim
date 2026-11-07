(* The app of the loader tests (test/loader/run.mjs). Before the start, it
   records in window.kilimLoaded.<id>: the time, the height of its mount element
   (the first render sees the styled layout), window.slowRan (a slow defer
   script ran) and the build. After the start, it increments [starts].

   The view has a button with a count and a View.unsafe_html node: the
   TrustedHTML window.kilimTestHtml if a page script made one, else the text
   "<i>x</i>". The fallback is the text "fallback". *)

open Kilim

let html () =
  let v = Jv.get Jv.global "kilimTestHtml" in
  if Jv.is_undefined v then Unsafe_html.of_string "<i>x</i>"
  else Kilim_browser.Unsafe_html.of_trusted v

let app =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun n () -> (n + 1, Cmd.none))
    ~view:(fun n ->
      El.div
        [ El.button
            ~at:[At.data "testid" "b"]
            ~ev:[Ev.click ()]
            [txt (string_of_int n)]
        ; View.unsafe_html ~el:(El.span ~at:[At.data "testid" "h"]) (html ()) ] )
    ~fallback:(fun _ -> txt "fallback")
    ()

let main id =
  let w = Jv.global in
  let loaded =
    match Jv.find w "kilimLoaded" with
    | Some l ->
        l
    | None ->
        let l = Jv.obj [||] in
        Jv.set w "kilimLoaded" l ; l
  in
  let mount =
    Jv.call (Jv.get w "document") "getElementById" [|Jv.of_string id|]
  in
  let build =
    match Sys.backend_type with
    | Other "wasm_of_ocaml" ->
        "wasm"
    | Other "js_of_ocaml" ->
        "js"
    | _ ->
        "other"
  in
  let r =
    Jv.obj
      [| ("time", Jv.call (Jv.get w "performance") "now" [||])
       ; ( "height"
         , if Jv.is_null mount then Jv.of_int (-1)
           else Jv.get mount "offsetHeight" )
       ; ("slowRan", Jv.of_bool (Option.is_some (Jv.find w "slowRan")))
       ; ("build", Jv.of_string build)
       ; ("starts", Jv.of_int 0) |]
  in
  Jv.set loaded id r ;
  Kilim_browser.run ~mount:id app ;
  Jv.set r "starts" (Jv.of_int 1)
