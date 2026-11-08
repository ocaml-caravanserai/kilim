(* The weak test in the js_of_ocaml and wasm_of_ocaml builds (docs/design.html,
   "Testing", Leak tests). The task of a component waits for a JS promise that
   never resolves. A global JS array keeps the resolve function of each promise,
   thus each promise and its callback stay alive. The continuation of the task
   holds the model of the instance. After the unmount of the instance, the model
   must be free. A control instance that stays mounted keeps its model.

   The two builds implement Weak with a JS WeakRef. A WeakRef keeps its target
   alive until the end of the current task, thus the collections and the check
   run in later tasks. The test calls globalThis.gc, which node gives with
   --expose-gc: Gc.full_major calls it under js_of_ocaml, but it does nothing
   under wasm_of_ocaml. The test writes "ok" and exits with status 0, or writes
   the failures and exits with status 1. make test-release runs the test in the
   release profile too, because js_of_ocaml inlines more there. *)

open Kilim
module Page = Kilim_testing.Page
module By = Kilim_testing.By

let global name = Jv.get Jv.global name

(* The resolve functions of the promises. *)
let held =
  let a = Jv.new' (global "Array") [||] in
  Jv.set Jv.global "kilimHeld" a ;
  a

(* The start of a task that waits for a new promise. The callback of the promise
   keeps only [ok]. Do not make the callback in [never]: js_of_ocaml can inline
   [never] into [measure], and then the callback keeps [m] (design section
   12). *)
let wait _ ~ok ~error:_ =
  let p =
    Jv.new' (global "Promise")
      [| Jv.callback ~arity:2 (fun resolve _ ->
             ignore (Jv.call held "push" [|resolve|]) ) |]
  in
  ignore (Jv.call p "then" [|Jv.callback ~arity:1 (fun v -> ok (Jv.to_int v))|])

let never () : int Async.t = Async.make wait

(* The continuation of the task holds the model [m]: thus a callback that keeps
   the continuation keeps the model. *)
let measure m = Async.map (fun v -> v + Bytes.length m) (never ())

(* The models of the two instances: slot 0 unmounts, slot 1 stays. *)
let models = Weak.create 2

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
    (m, Cmd.task (measure m))

  let update _ _ m _ = (m, Cmd.none)

  let view _ m = txt (string_of_int (Bytes.length m))
end

type msg = Hide

let app slot =
  App.make
    ~init:(fun () -> (true, Cmd.none))
    ~update:(fun _ Hide -> (false, Cmd.none))
    ~view:(fun shown ->
      El.div
        [ El.button ~ev:[Ev.click Hide] [txt "Hide"]
        ; ( if shown then
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
  let gone = Page.start (app 0) and kept = Page.start (app 1) in
  check "two live models" (Weak.check models 0 && Weak.check models 1) ;
  Page.click gone (By.text "Hide") ;
  check "the unmount" ((Page.tree gone).instances = 0) ;
  later (fun () ->
      collect () ;
      later (fun () ->
          collect () ;
          later (fun () ->
              check "the promises wait" (Jv.to_int (Jv.get held "length") = 2) ;
              check "the model of the unmounted instance is free"
                (not (Weak.check models 0)) ;
              check "the model of the mounted instance is alive"
                (Weak.check models 1) ;
              check "the two pages are alive"
                ( (Page.tree gone).instances = 0
                && (Page.tree kept).instances = 1 ) ;
              match List.rev !failures with
              | [] ->
                  print_endline "ok"
              | l ->
                  List.iter (fun s -> print_endline ("FAIL: " ^ s)) l ;
                  exit 1 ) ) )
