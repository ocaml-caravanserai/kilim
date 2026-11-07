type build = Wasm of {launcher: string; main: string option} | Js

type form = Inline | File of string

type t = {base: string; js: string; build: build; dev: bool}

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' ->
          Buffer.add_string b "&amp;"
      | '"' ->
          Buffer.add_string b "&quot;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | c ->
          Buffer.add_char b c )
    s ;
  Buffer.contents b

let attr name v = " " ^ name ^ "=\"" ^ escape v ^ "\""

(* The data- attributes, each with a space before it. *)
let attrs t =
  let build =
    match t.build with
    | Wasm {launcher; main} ->
        attr "data-wasm" launcher
        ^ Option.fold ~none:"" ~some:(attr "data-wasm-main") main
    | Js ->
        attr "data-build" "js"
  in
  attr "data-base" t.base ^ attr "data-js" t.js ^ build
  ^ if t.dev then " data-dev" else ""

let inline t extra =
  "<script" ^ attrs t ^ extra ^ ">" ^ Loader_text.text ^ "</script>"

let head form t =
  match form with
  | Inline ->
      inline t ""
  | File loader ->
      let src = attr "src" (t.base ^ loader) in
      "<script" ^ src ^ " async" ^ attrs t ^ "></script><script" ^ src
      ^ " defer" ^ attrs t ^ " data-start></script>"

let start form t =
  match form with Inline -> inline t " data-start" | File _ -> ""
