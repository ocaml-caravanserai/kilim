(* The weak test of kilim.websocket.browser (design section 12), in the
   js_of_ocaml and wasm_of_ocaml builds, with the rules of weak.ml. A JS shim on
   globalThis gives location and a WebSocket class that never opens. The shim
   keeps each socket in a global array, as a browser keeps a socket that is not
   closed, and keeps the listeners of each socket. Thus a listener that stays
   keeps its callback alive. The callbacks of the driver reach the message
   function of the subscription, which holds the model.

   The instance of slot 0 unmounts. After the unmount, no listener stays, and
   its model is free. Slot 1 is the control: its instance stays mounted, with
   four listeners, and its model is alive. Slot 2 is the sensitivity control:
   the shim ignores the removeEventListener of its unmount, thus its listeners
   stay and its model is alive. Thus the check finds a model that a listener
   keeps. *)

open Kilim
module Page = Kilim_testing.Page
module By = Kilim_testing.By

let global name = Jv.get Jv.global name

let shim =
  {|(() => {
  const sockets = [];
  globalThis.kilimShim = { sockets, leak: false, closes: 0 };
  globalThis.location = { href: 'http://127.0.0.1/' };
  globalThis.WebSocket = class {
    constructor(url) { this.url = url; this.readyState = 0; this.listeners = []; sockets.push(this); }
    addEventListener(name, fn) { this.listeners.push({ name, fn }); }
    removeEventListener(name, fn) {
      if (kilimShim.leak) return;
      const i = this.listeners.findIndex((l) => l.name === name && l.fn === fn);
      if (i >= 0) this.listeners.splice(i, 1);
    }
    close() { this.readyState = 3; kilimShim.closes++; }
  };
})()|}

let shim_int f =
  Jv.to_int (Jv.call Jv.global "eval" [|Jv.of_string ("(() => " ^ f ^ ")()")|])

let listeners () =
  shim_int "kilimShim.sockets.reduce((n, s) => n + s.listeners.length, 0)"

let closes () = shim_int "kilimShim.closes"

let set_leak b = Jv.set (global "kilimShim") "leak" (Jv.of_bool b)

let models = Weak.create 3

module Holder = struct
  include Component.Defaults

  type props = int

  type model = Bytes.t

  type msg = int

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init slot =
    let m = Bytes.make 100_000 'x' in
    Weak.set models slot (Some m) ;
    (m, Cmd.none)

  let update _ _ m _ = (m, Cmd.none)

  let view _ m = txt (string_of_int (Bytes.length m))

  (* The message function holds the model. *)
  let subs _ m = Kilim_websocket.connect "/ws" (fun _ -> Bytes.length m)
end

(* The instance mounts at the first click, and unmounts at the second. *)
let app slot =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun n () -> (n + 1, Cmd.none))
    ~view:(fun n ->
      El.div
        [ El.button ~ev:[Ev.click ()] [txt "Toggle"]
        ; ( if n = 1 then
              Component.v (module Holder) ~on:(function _ -> .) slot
            else txt "" ) ] )
    ()

let later f =
  ignore
    (Jv.apply (global "setTimeout")
       [|Jv.callback ~arity:1 (fun _ -> f ()); Jv.of_int 0|] )

let failures = ref []

let check name ok = if not ok then failures := name :: !failures

let collect () =
  if Jv.is_undefined (global "gc") then begin
    print_endline "FAIL: no globalThis.gc: run node with --expose-gc" ;
    exit 1
  end ;
  ignore (Jv.apply (global "gc") [||])

let () =
  ignore (Jv.call Jv.global "eval" [|Jv.of_string shim|]) ;
  let pages = Array.init 3 (fun slot -> Page.start (app slot)) in
  (* Each page sets Websocket_mock: the browser driver comes after them. *)
  Kilim_websocket_browser.install () ;
  let toggle slot = Page.click pages.(slot) (By.text "Toggle") in
  toggle 0 ;
  toggle 1 ;
  toggle 2 ;
  check "twelve listeners" (listeners () = 12) ;
  check "three live keys" (Kilim_websocket.Private.live () = 3) ;
  toggle 0 ;
  set_leak true ;
  toggle 2 ;
  set_leak false ;
  check "the unmounts" ((Page.tree pages.(0)).instances = 0) ;
  check "slot 0 and 2: no live key" (Kilim_websocket.Private.live () = 1) ;
  check "slot 0 and 2: two closes" (closes () = 2) ;
  check "the listeners of slot 1, and the listeners of slot 2 that leak"
    (listeners () = 8) ;
  later (fun () ->
      collect () ;
      later (fun () ->
          collect () ;
          later (fun () ->
              check "the model of the unmounted instance is free"
                (not (Weak.check models 0)) ;
              check "the model of the mounted instance is alive"
                (Weak.check models 1) ;
              check "the sensitivity control: a listener keeps the model"
                (Weak.check models 2) ;
              check "the pages are alive" ((Page.tree pages.(1)).instances = 1) ;
              match List.rev !failures with
              | [] ->
                  print_endline "ok"
              | l ->
                  List.iter (fun s -> print_endline ("FAIL: " ^ s)) l ;
                  exit 1 ) ) )
