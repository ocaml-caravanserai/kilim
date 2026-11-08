(* The tests of Async.of_fut and Kilim_web.Es_module in real browsers: Chrome,
   Firefox and WebKit (design sections 12 and 13), in the js_of_ocaml and
   wasm_of_ocaml builds, in the dev and the release profile. The page has the
   import map of run.mjs: the bare specifier "kilim-bare" is the module
   /mod/web_mod.mjs. test/dom/guards.txt lists the guards with their tests. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private
module BA = Kilim_browser.Async
module FC = Kilim_browser.Private.Fut_cell
module R = Private.Runtime

let stats a = R.stats (B.runtime a)

let ins a = R.inspect (B.runtime a)

let start app =
  let root = container () in
  (root, B.start (el root) app)

let children n = int (Jv.get (Jv.get n "childNodes") "length")

(* W4: the JS cell of of_fut, with a real promise. *)
let w4 () =
  let calls = ref [] in
  let k = Jv.callback ~arity:1 (fun v -> calls := int v :: !calls) in
  let c = FC.create k in
  let p = Jv.call (Jv.get Jv.global "Promise") "resolve" [|Jv.of_int 7|] in
  ignore (Jv.call p "then" [|FC.reaction c|]) ;
  ignore (Jv.call p "then" [|FC.reaction c|]) ;
  let full = not (FC.is_empty c) in
  let cleared = FC.create k in
  ignore (Jv.call p "then" [|FC.reaction cleared|]) ;
  FC.clear cleared ;
  let* () = task () in
  check "W4 the cell holds the function before the call" full ;
  check "W4 two reactions of one cell: one call, with the value" (!calls = [7]) ;
  check "W4 the cell is empty after the call" (FC.is_empty c) ;
  check "W4 a clear before the call: no call" (FC.is_empty cleared) ;
  Fut.return ()

(* W5: Es_module.import. *)
let w5 () =
  let import s = Kilim_web.Es_module.import (Jstr.v s) in
  let name = function
    | Ok m ->
        Jv.to_string (Jv.get m "name")
    | Error e ->
        "error " ^ Jstr.to_string (Jv.Error.name e)
  in
  let* absolute = import "/mod/web_mod.mjs" in
  check "W5 an absolute path" (name absolute = "web_mod") ;
  (* The script is /test_fut.bc.js: against its URL, ./web_mod.mjs is
     /web_mod.mjs, which does not exist. With the <base>, a resolve of the bare
     specifier would give /mod/kilim-bare, which does not exist. *)
  let base = Jv.call document "createElement" [|Jv.of_string "base"|] in
  Jv.set base "href" (Jv.of_string "/mod/") ;
  ignore (Jv.call (Jv.get document "head") "appendChild" [|base|]) ;
  let relative = import "./web_mod.mjs"
  and up = import "../mod/web_mod.mjs"
  and bare = import "kilim-bare" in
  ignore (Jv.call base "remove" [||]) ;
  let* relative = relative in
  let* up = up in
  let* bare = bare in
  check "W5 ./ and ../ resolve against document.baseURI"
    (name relative = "web_mod" && name up = "web_mod") ;
  check "W5 a bare specifier stays as it is (the import map)"
    (name bare = "web_mod") ;
  let* missing = import "/mod/missing.mjs" in
  check "W5 a missing module: a TypeError" (name missing = "error TypeError") ;
  Fut.return ()

(* W6: Async.of_fut in apps. *)
type msg = Got of string

let fetch_text url _ : string Fut.or_error =
  let open Fut.Result_syntax in
  let* r = Brr_io.Fetch.url (Jstr.v url) in
  let+ t = Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body r) in
  Jstr.to_string t

let shows task =
  App.make
    ~init:(fun () -> ("", Cmd.task (BA.map (fun s -> Got s) task)))
    ~update:(fun _ (Got s) -> (s, Cmd.none))
    ~view:(fun s -> El.span ~at:[At.data "testid" "s"] [txt s])
    ~fallback:(function
      | Jv.Error e ->
          El.p
            ~at:[At.data "testid" "fb"]
            [txt ("js " ^ Jstr.to_string (Jv.Error.message e))]
      | Failure m ->
          El.p ~at:[At.data "testid" "fb"] [txt ("failure " ^ m)]
      | _ ->
          El.p ~at:[At.data "testid" "fb"] [txt "other"] )
    ()

let fallback root =
  let n = Jv.call root "querySelector" [|Jv.of_string "[data-testid=fb]"|] in
  if Jv.is_null n then "none" else text n

let w6 () =
  (* (a) A fetch: one message and one render. *)
  let root, a =
    start (shows (BA.of_fut_or_error (fetch_text "/mod/web_mod.mjs")))
  in
  let* () = until (fun () -> (stats a).messages = 1) in
  let* () = frame () in
  let s = text (q root "s") in
  check "W6 a fetch: one message, one render, the text of the file"
    ( (stats a).messages = 1 && (stats a).renders = 2
    && Jstr.find_sub ~sub:(Jstr.v "web_mod") (Jstr.v s) <> None ) ;
  B.stop a ;
  (* (b) A rejected future fails the task with Jv.Error. *)
  let e = console_errors () in
  let root, a =
    start
      (shows
         (BA.of_fut_or_error (fun _ -> Fut.error (Jv.Error.v (Jstr.v "w6")))) )
  in
  let* () = until (fun () -> fallback root <> "none") in
  check "W6 a rejection: the fallback gets Jv.Error, one console error"
    (fallback root = "js w6" && console_errors () = e + 1) ;
  B.stop a ;
  (* (c) An unmount before the future settles: no message, no error. *)
  let fut, set = Fut.create () in
  let e = console_errors () in
  let _, a = start (shows (BA.of_fut (fun _ -> fut))) in
  B.stop a ;
  set "late" ;
  let* () = task () in
  let* () = frame () in
  check "W6 an unmount before the settle: no message, no console error"
    ((stats a).messages = 0 && console_errors () = e) ;
  (* (d) A make function that raises fails the task. *)
  let root, a = start (shows (BA.of_fut (fun _ -> failwith "w6 make"))) in
  let* () = until (fun () -> fallback root <> "none") in
  check "W6 a make that raises: the fallback gets the exception"
    (fallback root = "failure w6 make") ;
  B.stop a ;
  (* (e) Cold: make runs at each start of the task. *)
  let calls = ref 0 in
  let t =
    BA.of_fut (fun _ ->
        incr calls ;
        Fut.return (Got (string_of_int !calls)) )
  in
  let made = !calls in
  let root, a =
    start
      (App.make
         ~init:(fun () -> ("", Cmd.batch [Cmd.task t; Cmd.task t]))
         ~update:(fun m (Got s) -> (m ^ s, Cmd.none))
         ~view:(fun s -> El.span ~at:[At.data "testid" "s"] [txt s])
         () )
  in
  let* () = until (fun () -> (stats a).messages = 2) in
  let* () = frame () in
  check "W6 cold: make runs at each start"
    (made = 0 && !calls = 2 && String.equal (text (q root "s")) "12") ;
  B.stop a ;
  (* (f) 100 starts and stops of an app whose task waits for a future that never
     settles. *)
  let never, _ = Fut.create () in
  let doc_vis = count ~name:"visibilitychange" document in
  let root = container () in
  let clean = ref true in
  for _ = 1 to 100 do
    let a = B.start (el root) (shows (BA.of_fut (fun _ -> never))) in
    let started = (ins a).token_children = 1 in
    B.stop a ;
    clean :=
      !clean && started && (ins a).token_children = 0
      && count root = 0
      && count ~name:"visibilitychange" document = doc_vis
      && B.live_timers () = 0
      && children root = 0
  done ;
  check "W6 100 starts and stops with a future that never settles: each count 0"
    !clean ;
  Fut.return ()

let () =
  ignore (Lazy.force spy) ;
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     let* () = w4 () in
     let* () = w5 () in
     w6 () )
    finish
