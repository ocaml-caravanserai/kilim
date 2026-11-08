(* The tests of kilim.storage.browser in real browsers: Chrome, Firefox and
   WebKit (design section 13), in the js_of_ocaml and wasm_of_ocaml builds, in
   the dev and the release profile. The errors are simulated: a getter of
   localStorage that throws a SecurityError, and a setItem or a getItem of
   Storage.prototype that throws or gives a value that is not a string.
   test/dom/guards.txt lists the guards with their tests. *)

open Kilim
open Driver
open Fut.Syntax
module B = Kilim_browser.Private
module S = Kilim_storage

(* The changes of the page: block and unblock localStorage, and replace setItem
   and getItem of Storage.prototype. *)
let shim_code =
  {|(() => {
  const P = Storage.prototype, setItem = P.setItem, getItem = P.getItem;
  const own = Object.getOwnPropertyDescriptor(window, 'localStorage');
  globalThis.kilimStorage = {
    block() {
      Object.defineProperty(window, 'localStorage', { configurable: true,
        get() { throw new DOMException('blocked', 'SecurityError'); } });
    },
    nullStorage() {
      Object.defineProperty(window, 'localStorage', { configurable: true, get() { return null; } });
    },
    unblock() {
      if (own) Object.defineProperty(window, 'localStorage', own);
      else delete window.localStorage;
    },
    setThrows(name) {
      P.setItem = function () {
        const e = name === 'TypeError' ? new TypeError('t') : new DOMException('q', name);
        throw e;
      };
    },
    setFirefoxQuota() {
      P.setItem = function () { const e = new Error('q'); e.name = 'NS_ERROR_DOM_QUOTA_REACHED'; throw e; };
    },
    getNumber() { P.getItem = function () { return 5; }; },
    restore() { P.setItem = setItem; P.getItem = getItem; },
  };
})()|}

let shim name = ignore (Jv.call (Jv.get Jv.global "kilimStorage") name [||])

let set_throws name =
  ignore
    (Jv.call
       (Jv.get Jv.global "kilimStorage")
       "setThrows"
       [|Jv.of_string name|] )

let show_get = function
  | Ok (Some v) ->
      "some " ^ v
  | Ok None ->
      "none"
  | Error e ->
      S.error_to_string e

let show_unit = function Ok () -> "ok" | Error e -> S.error_to_string e

(* The result of [task] in a new app. The result is synchronous: it comes in the
   first turn. *)
let result task =
  let r = ref None in
  let a =
    B.start
      (el (container ()))
      (App.make
         ~init:(fun () -> ((), Cmd.task (Async.map (fun s -> Some s) task)))
         ~update:(fun () s ->
           r := s ;
           ((), Cmd.none) )
         ~view:(fun () -> txt "")
         () )
  in
  B.stop a ;
  Option.value ~default:"none" !r

let get a k = result (Async.map show_get (S.get a k))

let set a k v = result (Async.map show_unit (S.set a k v))

let remove a k = result (Async.map show_unit (S.remove a k))

let local_item k =
  let v =
    Jv.call (Jv.get Jv.global "localStorage") "getItem" [|Jv.of_string k|]
  in
  if Jv.is_null v then "null" else Jv.to_string v

(* ST1: a round trip in the two areas, and the init task gives its result before
   the first render. *)
let st1 () =
  let k = "kilim-st1" in
  (* In order: OCaml evaluates the items of a list from the last one. *)
  let r =
    List.map
      (fun f -> f ())
      [ (fun () -> set Local k "a \xc3\xa9")
      ; (fun () -> get Local k)
      ; (fun () -> set Session k "s")
      ; (fun () -> get Session k)
      ; (fun () -> remove Local k)
      ; (fun () -> get Local k)
      ; (fun () -> get Session k)
      ; (fun () -> remove Session k)
      ; (fun () -> get Session k) ]
  in
  check "ST1 get, set and remove in Local and Session"
    ( r
    = [ "ok"
      ; "some a \xc3\xa9"
      ; "ok"
      ; "some s"
      ; "ok"
      ; "none"
      ; "some s"
      ; "ok"
      ; "none" ] ) ;
  ignore (set Local k "v") ;
  check "ST1 the value is in localStorage" (local_item k = "v") ;
  ignore (remove Local k) ;
  check "ST1 remove removes the item" (local_item k = "null") ;
  Fut.return ()

(* ST2: a getter of localStorage that throws a SecurityError, and a getter that
   gives null. *)
let st2 () =
  shim "block" ;
  let r =
    List.map
      (fun f -> f ())
      [ (fun () -> get Local "k")
      ; (fun () -> set Local "k" "v")
      ; (fun () -> remove Local "k")
      ; (fun () -> set Session "k" "v") ]
  in
  shim "unblock" ;
  ignore (remove Session "k") ;
  check "ST2 a blocked localStorage: Unavailable, sessionStorage works"
    (r = ["unavailable"; "unavailable"; "unavailable"; "ok"]) ;
  check "ST2 localStorage works again"
    (set Local "k" "v" = "ok" && remove Local "k" = "ok") ;
  shim "nullStorage" ;
  let n =
    List.map
      (fun f -> f ())
      [ (fun () -> get Local "k")
      ; (fun () -> set Local "k" "v")
      ; (fun () -> remove Local "k") ]
  in
  shim "unblock" ;
  check "ST2 a localStorage that is null: Unavailable"
    (n = ["unavailable"; "unavailable"; "unavailable"]) ;
  Fut.return ()

(* ST3: the names of the errors of setItem, and a getItem that gives a
   number. *)
let st3 () =
  set_throws "QuotaExceededError" ;
  let q = set Local "k" "v" in
  shim "setFirefoxQuota" ;
  let f = set Session "k" "v" in
  set_throws "TypeError" ;
  let t = set Local "k" "v" in
  set_throws "InvalidStateError" ;
  let i = set Local "k" "v" in
  shim "getNumber" ;
  let n = get Local "k" in
  shim "restore" ;
  check "ST3 QuotaExceededError and NS_ERROR_DOM_QUOTA_REACHED: Quota_exceeded"
    (q = "quota exceeded" && f = "quota exceeded") ;
  check "ST3 another error: Other with the name"
    (t = "other TypeError" && i = "other InvalidStateError") ;
  check "ST3 a getItem that gives a number: None" (n = "none") ;
  Fut.return ()

let () =
  ignore (js shim_code) ;
  Fut.await
    (let* () = release () in
     let* () = loaded () in
     Kilim_storage_browser.install () ;
     Kilim_storage_browser.install () ;
     let* () = st1 () in
     let* () = st2 () in
     st3 () )
    finish
