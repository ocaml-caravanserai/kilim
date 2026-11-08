(* The weak test of kilim.router.browser (design section 12), in the js_of_ocaml
   and wasm_of_ocaml builds, with the rules of weak.ml. node has no location, no
   history and no window listeners: a JS shim on globalThis gives them. The shim
   keeps each popstate listener in a global array, thus a listener that stays
   keeps its callback alive. The callback of the router reaches the message
   function of the subscription, which holds the model.

   The instance of slot 0 unmounts. After the unmount, no listener stays, and
   its model is free. Slot 1 is the control: its instance stays mounted, with
   one listener, and its model is alive. Slot 2 is the sensitivity control: the
   shim ignores the removeEventListener of its unmount, thus its listener stays
   and its model is alive. Thus the check finds a model that a listener
   keeps. *)

open Kilim
module Page = Kilim_testing.Page
module By = Kilim_testing.By

let global name = Jv.get Jv.global name

let shim =
  {|(() => {
  const listeners = [];
  globalThis.kilimShim = { listeners, leak: false };
  globalThis.location = { pathname: '/', search: '', hash: '', href: 'http://127.0.0.1/' };
  globalThis.history = {
    pushState(s, t, u) { location.pathname = new URL(u).pathname; },
    replaceState(s, t, u) { location.pathname = new URL(u).pathname; },
    back() {},
  };
  globalThis.scrollTo = () => {};
  globalThis.addEventListener = (name, fn) => { listeners.push({ name, fn }); };
  globalThis.removeEventListener = (name, fn) => {
    if (kilimShim.leak) return;
    const i = listeners.findIndex((l) => l.name === name && l.fn === fn);
    if (i >= 0) listeners.splice(i, 1);
  };
})()|}

let listeners () =
  Jv.to_int (Jv.get (Jv.get (global "kilimShim") "listeners") "length")

let set_leak b = Jv.set (global "kilimShim") "leak" (Jv.of_bool b)

let models = Weak.create 3

let router =
  Kilim_router.make
    ~parse:(fun l -> List.length l.Url.path)
    ~print:(fun _ -> {Url.path= []; query= []; fragment= None})
    ()

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
  let subs _ m = Kilim_router.changes router (fun n -> n + Bytes.length m)
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
  (* Each page sets History_mock: the browser driver comes after them. *)
  Kilim_router_browser.install () ;
  let toggle slot = Page.click pages.(slot) (By.text "Toggle") in
  toggle 0 ;
  toggle 1 ;
  toggle 2 ;
  check "three listeners" (listeners () = 3) ;
  check "three subscriptions" (Kilim_router.Private.listeners () = 3) ;
  toggle 0 ;
  set_leak true ;
  toggle 2 ;
  set_leak false ;
  check "the unmounts" ((Page.tree pages.(0)).instances = 0) ;
  check "slot 0 and 2: no subscription" (Kilim_router.Private.listeners () = 1) ;
  check "the listener of slot 1, and the listener of slot 2 that leaks"
    (listeners () = 2) ;
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
