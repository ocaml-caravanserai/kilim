(* The page side of run.mjs: the drive channel, the log and the result. *)

let console = Jv.get Jv.global "console"

let log s = ignore (Jv.call console "log" [|Jv.of_string s|])

let failures = ref 0

let check name ok =
  log ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

(* The last line of the page: run.mjs waits for it. *)
let finish () = log (if !failures = 0 then "DONE ok" else "DONE FAIL")

(* The browser of the page: run.mjs gives it in the query ?browser=NAME. A page
   with no query (for example a page that you open by hand) is in Chrome. *)
type browser = Chrome | Firefox | Webkit

let browser =
  let search = Jv.to_string (Jv.get (Jv.get Jv.global "location") "search") in
  match search with
  | "?browser=firefox" ->
      Firefox
  | "?browser=webkit" ->
      Webkit
  | _ ->
      Chrome

(* The differences of the browsers that the pages expect: the one table of the
   DOM tests. The values are facts of Chrome and of the Playwright browsers of
   flake.lock (Firefox 157, WebKit 27.2). A page uses only these values for a
   difference of the browsers. *)
type facts =
  { ime: bool
        (** The driver has a composition command (Input.imeSetComposition).
            Playwright has none for Firefox and WebKit. *)
  ; insert_composes: bool
        (** Input.insertText is a real composition, and the last input comes
            after compositionend (Firefox). In Chrome and WebKit, it is an input
            with no composition. *)
  ; trap: string
        (** The first line of the exception of an illegal cast in the
            wasm_of_ocaml build. The cast of the pages is a cast of a
            wasm:js-string function: WebKit gives "Illegal argument" for it, and
            "ref.cast failed ..." for a ref.cast. *)
  ; null_traps: bool
        (** A trap gives an error event with a null error (a trap of a
            wasm:js-string function in WebKit). kilim then aborts only an open
            turn, thus two traps in one task give one report. *)
  ; removal_blurs: int
        (** The Ev.blur messages when a render removes the focused input.
            Firefox and WebKit send no blur, no focusout and no change on a
            removal. *)
  ; meta_key: bool
        (** A trusted click with the Meta key down has [metaKey] true. *)
  ; middle_click: bool
        (** A trusted click of the middle button sends a [click] event to
            window, after [auxclick]. *)
  ; network_error: string
        (** The name and the message of the rejection of a fetch to a port that
            the browser blocks (http://127.0.0.1:1/). *)
  ; header_byte: bool
        (** A request header value with U+00E9 goes as one byte 0xE9 (in WebKit,
            as the two bytes of UTF-8). *) }

let facts =
  match browser with
  | Chrome ->
      { ime= true
      ; insert_composes= false
      ; trap= "RuntimeError: illegal cast"
      ; null_traps= false
      ; removal_blurs= 1
      ; meta_key= true
      ; middle_click= false
      ; network_error= "TypeError: Failed to fetch"
      ; header_byte= true }
  | Firefox ->
      { ime= false
      ; insert_composes= true
      ; trap= "RuntimeError: bad cast"
      ; null_traps= false
      ; removal_blurs= 0
      ; meta_key= true
      ; middle_click= false
      ; network_error=
          "TypeError: NetworkError when attempting to fetch resource."
      ; header_byte= true }
  | Webkit ->
      { ime= false
      ; insert_composes= false
      ; trap= "RuntimeError: Illegal argument"
      ; null_traps= true
      ; removal_blurs= 0
      ; meta_key= true
      ; middle_click= false
      ; network_error= "TypeError: Load failed"
      ; header_byte= false }

(* A check that does not run in this browser. run.mjs counts these lines. *)
let skip name reason = log ("SKIP " ^ name ^ ": " ^ reason)

(* The drives that wait for kilimDriven, by id. *)
let next = ref 0

let pending : (int * (unit -> unit)) list ref = ref []

let () =
  Jv.set Jv.global "kilimDriven"
    (Jv.callback ~arity:1 (fun id ->
         let id = Jv.to_int id in
         match List.assoc_opt id !pending with
         | Some ok ->
             pending := List.remove_assoc id !pending ;
             ok ()
         | None ->
             () ) )

let json = Jv.get Jv.global "JSON"

let request fields =
  incr next ;
  let id = !next in
  let fut, set = Fut.create () in
  pending := (id, set) :: !pending ;
  let o = Jv.obj (Array.append [|("id", Jv.of_int id)|] fields) in
  ignore (Jv.call Jv.global "kilimDrive" [|Jv.call json "stringify" [|o|]|]) ;
  fut

(* The server sends the body of the page. *)
let release () = request [|("release", Jv.true')|]

(* DevTools input commands, for example [Input.dispatchMouseEvent]. *)
let drive steps =
  request
    [| ( "steps"
       , Jv.of_list
           (fun (m, params) -> Jv.of_jv_array [|Jv.of_string m; params|])
           steps ) |]

let frame () =
  let fut, set = Fut.create () in
  ignore
    (Jv.call Jv.global "requestAnimationFrame"
       [|Jv.callback ~arity:1 (fun _ -> set ())|] ) ;
  fut

(* One task of the host: the same MessageChannel as the tasks of kilim. *)
let task () =
  let fut, set = Fut.create () in
  Kilim_browser.Private.host.post_task set ;
  fut

let sleep ms =
  let fut, set = Fut.create () in
  ignore
    (Jv.call Jv.global "setTimeout"
       [|Jv.callback ~arity:1 (fun _ -> set ()); Jv.of_int ms|] ) ;
  fut

(* Wait until [ok ()], at most 2 s. *)
let rec until ?(n = 200) ok =
  if ok () || n = 0 then Fut.return ()
  else Fut.bind (sleep 10) (fun () -> until ~n:(n - 1) ok)

(* The document is not "loading". *)
let loaded () =
  let fut, set = Fut.create () in
  let document = Jv.get Jv.global "document" in
  if not (Jv.to_string (Jv.get document "readyState") = "loading") then set ()
  else
    ignore
      (Jv.call document "addEventListener"
         [| Jv.of_string "DOMContentLoaded"
          ; Jv.callback ~arity:1 (fun _ -> set ())
          ; Jv.obj [|("once", Jv.true')|] |] ) ;
  fut

(* JS code of the test, for example the spy. *)
let js code = Jv.call Jv.global "eval" [|Jv.of_string code|]

let document = Jv.get Jv.global "document"

let window = Jv.global

let by_id id = Jv.call document "getElementById" [|Jv.of_string id|]

(* The element with this data-testid below [root]. *)
let q root id =
  let n =
    Jv.call root "querySelector"
      [|Jv.of_string ("[data-testid=\"" ^ id ^ "\"]")|]
  in
  if Jv.is_null n then failwith ("no element " ^ id) else n

let text n = Jv.to_string (Jv.get n "textContent")

let value n = Jv.to_string (Jv.get n "value")

let checked n = Jv.to_bool (Jv.get n "checked")

let int v = Jv.to_int v

(* A new element in the body for the mount of one test, with a size, thus the
   mouse can reach its children. *)
let container () =
  let d = Jv.call document "createElement" [|Jv.of_string "div"|] in
  ignore (Jv.call (Jv.get document "body") "appendChild" [|d|]) ;
  d

let el n = Brr.El.of_jv n

(* A listener on [target] for [name], in the bubble phase or with [~capture] in
   the capture phase. *)
let listen ?(capture = false) target name f =
  let fn = Jv.callback ~arity:1 f in
  ignore
    (Jv.call target "addEventListener"
       [|Jv.of_string name; fn; Jv.of_bool capture|] ) ;
  fn

let unlisten ?(capture = false) target name fn =
  ignore
    (Jv.call target "removeEventListener"
       [|Jv.of_string name; fn; Jv.of_bool capture|] )

(* The spy of the listeners: it counts the live listeners by target and name,
   with the passive flag, and the writes of the properties value and checked of
   the inputs and selects. Install it first. *)
let spy_code =
  {|(() => {
  const P = EventTarget.prototype, add = P.addEventListener, rem = P.removeEventListener;
  const live = new Map();
  const cap = o => typeof o === 'boolean' ? o : !!(o && o.capture);
  P.addEventListener = function (name, fn, o) {
    let l = live.get(this); if (!l) { l = []; live.set(this, l); }
    if (!l.some(e => e.name === name && e.fn === fn && e.capture === cap(o)))
      l.push({ name, fn, capture: cap(o), passive: !!(o && typeof o === 'object' && o.passive) });
    return add.call(this, name, fn, o);
  };
  P.removeEventListener = function (name, fn, o) {
    const l = live.get(this);
    if (l) { const i = l.findIndex(e => e.name === name && e.fn === fn && e.capture === cap(o)); if (i >= 0) l.splice(i, 1); }
    return rem.call(this, name, fn, o);
  };
  const writes = { value: 0, checked: 0 };
  for (const C of [HTMLInputElement, HTMLSelectElement, HTMLTextAreaElement])
    for (const p of ['value', 'checked']) {
      const d = Object.getOwnPropertyDescriptor(C.prototype, p);
      if (!d) continue;
      Object.defineProperty(C.prototype, p, { ...d, set(v) { writes[p]++; d.set.call(this, v); } });
    }
  const errors = [];
  const error = console.error;
  console.error = function (...a) { errors.push(a); return error.apply(this, a); };
  globalThis.kilimSpy = {
    count: (t, name) => (live.get(t) || []).filter(e => name === undefined || e.name === name).length,
    passive: (t, name) => { const e = (live.get(t) || []).find(e => e.name === name); return e ? e.passive : null; },
    writes: p => writes[p],
    errors: () => errors.length,
  };
})()|}

let spy =
  lazy
    ( ignore (js spy_code) ;
      Jv.get Jv.global "kilimSpy" )

let count ?name target =
  let s = Lazy.force spy in
  int
    ( match name with
    | Some n ->
        Jv.call s "count" [|target; Jv.of_string n|]
    | None ->
        Jv.call s "count" [|target|] )

let passive target name =
  Jv.call (Lazy.force spy) "passive" [|target; Jv.of_string name|]

let writes p = int (Jv.call (Lazy.force spy) "writes" [|Jv.of_string p|])

let console_errors () = int (Jv.call (Lazy.force spy) "errors" [||])

(* The trusted input of DevTools. *)
let center n =
  let b = Jv.call n "getBoundingClientRect" [||] in
  let f k = Jv.to_float (Jv.get b k) in
  (f "x" +. (f "width" /. 2.), f "y" +. (f "height" /. 2.))

let mouse kind (x, y) extra =
  ( "Input.dispatchMouseEvent"
  , Jv.obj
      (Array.append
         [| ("type", Jv.of_string kind)
          ; ("x", Jv.of_float x)
          ; ("y", Jv.of_float y) |]
         extra ) )

(* The modifier bits of DevTools: Alt 1, Ctrl 2, Meta 4, Shift 8. *)
let alt = 1

let ctrl = 2

let meta = 4

let shift = 8

(* A click with the modifier keys of [modifiers] down, with the button "left",
   "middle" or "right". *)
let click ?(modifiers = 0) ?(button = "left") n =
  let p = center n in
  let b =
    [| ("button", Jv.of_string button)
     ; ("clickCount", Jv.of_int 1)
     ; ("modifiers", Jv.of_int modifiers) |]
  in
  drive [mouse "mousePressed" p b; mouse "mouseReleased" p b]

let move_to n = drive [mouse "mouseMoved" (center n) [||]]

let move_xy p = drive [mouse "mouseMoved" p [||]]

let wheel n =
  drive
    [ mouse "mouseWheel" (center n)
        [|("deltaX", Jv.of_int 0); ("deltaY", Jv.of_int 50)|] ]

let insert_text s =
  drive [("Input.insertText", Jv.obj [|("text", Jv.of_string s)|])]

(* A key press with the modifier keys of [modifiers] down. With [~text] (for
   example "\r" for Enter), the press gives a character, as a real key does:
   then Enter on a focused link follows the link. *)
let key ?(modifiers = 0) ?text k code =
  let ev kind =
    ( "Input.dispatchKeyEvent"
    , Jv.obj
        (Array.append
           [| ("type", Jv.of_string kind)
            ; ("key", Jv.of_string k)
            ; ("code", Jv.of_string k)
            ; ("windowsVirtualKeyCode", Jv.of_int code)
            ; ("nativeVirtualKeyCode", Jv.of_int code)
            ; ("modifiers", Jv.of_int modifiers) |]
           ( match text with
           | Some t when kind = "keyDown" ->
               [|("text", Jv.of_string t)|]
           | _ ->
               [||] ) ) )
  in
  drive [ev (if text = None then "rawKeyDown" else "keyDown"); ev "keyUp"]

let ime s =
  let n = Jv.of_int (String.length s) in
  drive
    [ ( "Input.imeSetComposition"
      , Jv.obj
          [| ("text", Jv.of_string s)
           ; ("selectionStart", n)
           ; ("selectionEnd", n) |] ) ]

let focus n = ignore (Jv.call n "focus" [||])

(* A synthetic event, for example a keydown with isComposing. *)
let dispatch n ev = ignore (Jv.call n "dispatchEvent" [|ev|])
