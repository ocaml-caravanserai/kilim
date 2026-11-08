(* The tests of kilim.web in real browsers: Chrome, Firefox and WebKit (design
   section 13), in the js_of_ocaml and wasm_of_ocaml builds. One W test for each
   binding. U1 compares Kilim.Url with the URL functions of the browser. The D
   tests of test_dom.ml test the code of kilim.browser that uses the bindings.
   test/dom/guards.txt lists the guards of kilim.web with the test that fails
   when a guard changes. *)

open Driver
open Fut.Syntax
module W = Kilim_web

(* W1: Microtask. The microtasks run in FIFO order, before a task of the host (a
   MessageChannel message) and before a timer of 0 ms. A microtask that throws
   does not stop the next microtask. *)
let w1 () =
  let seen = ref [] in
  let add s = Jv.callback ~arity:1 (fun _ -> seen := s :: !seen) in
  let later = sleep 0 and next = task () in
  W.Microtask.queue (add "m1") ;
  W.Microtask.queue (add "m2") ;
  log "EXPECT EXCEPTION Error: w1" ;
  W.Microtask.queue (js "(function () { throw new Error('w1') })") ;
  W.Microtask.queue (add "m3") ;
  let in_turn = !seen = [] in
  let* () = next in
  let* () = later in
  (* Firefox reports the exception after the console lines of the page: a round
     trip through run.mjs waits for the report. *)
  let* () = drive [] in
  log "EXPECT END" ;
  check "W1 no microtask runs in the turn of the call" in_turn ;
  check "W1 FIFO order, a throw does not stop the next microtask"
    (List.rev !seen = ["m1"; "m2"; "m3"]) ;
  let order = ref [] in
  let add s = order := s :: !order in
  let timer = Fut.map (fun () -> add "timer") (sleep 0) in
  let task = Fut.map (fun () -> add "task") (task ()) in
  W.Microtask.queue (Jv.callback ~arity:1 (fun _ -> add "microtask")) ;
  let* () = timer in
  let* () = task in
  check "W1 a microtask runs before a task and a timer"
    (match List.rev !order with "microtask" :: _ -> true | _ -> false) ;
  Fut.return ()

(* W2: Js_object.is. *)
let w2 () =
  let is = W.Js_object.is in
  let o = Jv.obj [||] in
  check "W2 0 and -0 are different"
    (not (is (Jv.of_float 0.) (Jv.of_float (-0.)))) ;
  check "W2 two NaN values are the same" (is (Jv.of_float Float.nan) (js "NaN")) ;
  check "W2 an object is the same as itself" (is o o) ;
  check "W2 two objects are different" (not (is o (Jv.obj [||]))) ;
  check "W2 two equal JS strings are the same"
    (is (Jv.of_string "ab") (js "'a' + 'b'")) ;
  check "W2 1 and \"1\" are different"
    (not (is (Jv.of_int 1) (Jv.of_string "1"))) ;
  Fut.return ()

(* W3: Trusted_types. The three browsers have trustedTypes (D38 needs it too).
   With no trustedTypes (a browser with no Trusted Types), is_html gives false
   and calls nothing. *)
let w3 () =
  let policy = js "trustedTypes.createPolicy('w3', { createHTML: (s) => s })" in
  let html = Jv.call policy "createHTML" [|Jv.of_string "<i>x</i>"|] in
  check "W3 exists" (W.Trusted_types.exists ()) ;
  check "W3 is_html of a TrustedHTML" (W.Trusted_types.is_html html) ;
  check "W3 is_html of a JS string"
    (not (W.Trusted_types.is_html (Jv.of_string "<i>x</i>"))) ;
  check "W3 is_html of an object" (not (W.Trusted_types.is_html (Jv.obj [||]))) ;
  let restore =
    js
      "(() => { const d = Object.getOwnPropertyDescriptor(window, \
       'trustedTypes'); Object.defineProperty(window, 'trustedTypes', { value: \
       null, configurable: true }); return () => { if (d) \
       Object.defineProperty(window, 'trustedTypes', d); else delete \
       window.trustedTypes; }; })()"
  in
  let shadowed = Jv.is_null (Jv.get Jv.global "trustedTypes") in
  let exists = W.Trusted_types.exists () in
  let is_html =
    match W.Trusted_types.is_html html with v -> v | exception _ -> true
  in
  ignore (Jv.apply restore [||]) ;
  check "W3 no trustedTypes: exists is false" (shadowed && not exists) ;
  check "W3 no trustedTypes: is_html is false" (shadowed && not is_html) ;
  check "W3 trustedTypes again" (W.Trusted_types.exists ()) ;
  (* A browser with no Trusted Types and an element with the id trustedTypes:
     the global trustedTypes is the element. The page removes the property of
     window, or puts the element in it if the property does not go. *)
  let shadow =
    js
      "(() => { const el = document.createElement('div'); el.id = \
       'trustedTypes'; document.body.appendChild(el); const d = \
       Object.getOwnPropertyDescriptor(window, 'trustedTypes'); delete \
       window.trustedTypes; if (window.trustedTypes !== el) \
       Object.defineProperty(window, 'trustedTypes', { value: el, \
       configurable: true }); return { el, restore: () => { el.remove(); if \
       (d) Object.defineProperty(window, 'trustedTypes', d); else delete \
       window.trustedTypes; } }; })()"
  in
  let shadowed =
    Jv.strict_equal (Jv.get Jv.global "trustedTypes") (Jv.get shadow "el")
  in
  let is_html =
    match W.Trusted_types.is_html html with v -> v | exception _ -> true
  in
  ignore (Jv.call shadow "restore" [||]) ;
  check "W3 an element with the id trustedTypes: is_html is false"
    (shadowed && not is_html) ;
  check "W3 trustedTypes after the element" (W.Trusted_types.is_html html) ;
  Fut.return ()

(* U1: Kilim.Url against encodeURIComponent, decodeURIComponent, URLSearchParams
   and URL, on the corpus of test/url_corpus. *)
let u1 () =
  let module U = Kilim.Url in
  let module C = Url_corpus in
  let call f s = Jv.to_string (Jv.call Jv.global f [|Jv.of_string s|]) in
  check "U1 encode_component is encodeURIComponent"
    (List.for_all
       (fun s ->
         String.equal (U.encode_component s) (call "encodeURIComponent" s) )
       C.texts ) ;
  check "U1 decode_component is decodeURIComponent"
    (List.for_all
       (fun (s, d) ->
         String.equal (U.decode_component s) d
         && String.equal (call "decodeURIComponent" s) d )
       C.escapes ) ;
  let entries s =
    let p = Jv.new' (Jv.get Jv.global "URLSearchParams") [|Jv.of_string s|] in
    let pair e =
      (Jv.to_string (Jv.Jarray.get e 0), Jv.to_string (Jv.Jarray.get e 1))
    in
    Array.to_list
      (Jv.to_array pair (Jv.call (Jv.get Jv.global "Array") "from" [|p|]))
  in
  check "U1 query is URLSearchParams"
    (List.for_all (fun (s, q) -> U.query s = q && entries s = q) C.queries) ;
  check "U1 with_query: URLSearchParams gives the pairs"
    (List.for_all
       (fun (url, q, r) ->
         let search =
           match String.index_opt r '?' with
           | Some i ->
               String.sub r i (String.length r - i)
           | None ->
               ""
         in
         let search =
           match String.index_opt search '#' with
           | Some i ->
               String.sub search 0 i
           | None ->
               search
         in
         String.equal (U.with_query url q) r
         && (q = [] || List.for_all (fun p -> List.mem p (entries search)) q) )
       C.with_queries ) ;
  (* Each URL of to_string, with the page as the base, has the host of the
     page. *)
  let href = Jv.get (Jv.get Jv.global "location") "href" in
  let host = Jv.to_string (Jv.get (Jv.get Jv.global "location") "host") in
  check "U1 to_string: new URL(to_string l, page).host is the host of the page"
    (List.for_all
       (fun (l, _) ->
         let u =
           Jv.new' (Jv.get Jv.global "URL")
             [|Jv.of_string (U.to_string l); href|]
         in
         String.equal (Jv.to_string (Jv.get u "host")) host )
       C.prints ) ;
  (* The browser keeps a location with no dot segment: the text of its href
     after the origin parses back to the location. (The hash of URL is "" for an
     empty fragment, thus the check reads href.) *)
  let browser_url l =
    let u =
      Jv.new' (Jv.get Jv.global "URL") [|Jv.of_string (U.to_string l); href|]
    in
    let get k = Jv.to_string (Jv.get u k) in
    let h = get "href" and o = get "origin" in
    String.sub h (String.length o) (String.length h - String.length o)
  in
  let dot l = List.exists (fun x -> x = "." || x = "..") l.U.path in
  check "U1 to_string: the URL of the browser parses back to the location"
    (List.for_all
       (fun (l, _) -> dot l || l.U.path = [""] || U.parse (browser_url l) = l)
       C.prints ) ;
  (* A fact of the three browsers: a ".." segment removes the segment before it,
     thus the location goes to another route. *)
  check "U1 to_string: the browser changes the path a/../b to /b"
    (String.equal
       (browser_url {U.path= ["a"; ".."; "b"]; query= []; fragment= None})
       "/b" ) ;
  Fut.return ()

let () =
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     let* () = w1 () in
     let* () = w2 () in
     let* () = w3 () in
     u1 () )
    finish
