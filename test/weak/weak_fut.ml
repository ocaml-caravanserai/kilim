(* The weak test of Kilim_browser.Async.of_fut (design section 12), in the
   js_of_ocaml and wasm_of_ocaml builds, with the rules of weak.ml. The task of
   each component waits for a Brr future that never settles: a global JS array
   keeps its set function, thus the future, its promise and the reaction stay
   alive. After the unmount, the model must be free:

   - Slot 0: a top-level make function, and the continuation holds the model. -
   Slot 1: the make function is a closure in init that holds the model. - Slot
   2: of_fut_or_error. - Slot 3: the control. The instance stays mounted, and
   its model is alive. - Slot 4: the sensitivity control. The make function puts
   the model in the global array on purpose: after the unmount, the model is
   alive. Thus the check finds a model that something keeps.

   The JS cell alone: a JS function that holds a large Bytes value goes into a
   cell, and the reaction goes on a promise that never settles. After the clear
   (slot 5), the value is free. Without the clear (slot 6, the control), it is
   alive.

   The weak tests cannot find a missing clear of the cell of of_fut: Async.make
   clears its own cell, thus the cell of of_fut then holds no model. The H test
   "a cancel empties the cell" of test/browser is the guard of that clear. *)

open Kilim
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module BA = Kilim_browser.Async
module FC = Kilim_browser.Private.Fut_cell

let global name = Jv.get Jv.global name

let held =
  let a = Jv.new' (global "Array") [||] in
  Jv.set Jv.global "kilimHeld" a ;
  a

let keep v = ignore (Jv.call held "push" [|v|])

(* A future that never settles. The global array keeps its set function. *)
let never () : int Fut.t =
  let fut, set = Fut.create () in
  keep (Jv.repr set) ;
  fut

let top_make _ = never ()

let never_or_error _ : int Fut.or_error =
  let fut, set = Fut.create () in
  keep (Jv.repr set) ;
  fut

let models = Weak.create 7

let task slot m : int Async.t =
  match slot with
  | 0 | 3 ->
      BA.map (fun v -> v + Bytes.length m) (BA.of_fut top_make)
  | 1 ->
      BA.of_fut (fun _ ->
          if Bytes.length m < 0 then failwith "never" ;
          never () )
  | 2 ->
      BA.map (fun v -> v + Bytes.length m) (BA.of_fut_or_error never_or_error)
  | _ ->
      BA.of_fut (fun _ ->
          keep (Jv.repr m) ;
          never () )

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
    (m, Cmd.task (task slot m))

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

(* The JS cell alone, with a promise that never settles. *)
let promise_never () =
  Jv.new' (global "Promise")
    [|Jv.callback ~arity:2 (fun resolve _ -> keep resolve)|]

let cell_alone slot ~clear =
  let b = Bytes.make 100_000 'y' in
  Weak.set models slot (Some b) ;
  let c =
    FC.create (Jv.callback ~arity:1 (fun _ -> keep (Jv.of_int (Bytes.length b))))
  in
  ignore (Jv.call (promise_never ()) "then" [|FC.reaction c|]) ;
  if clear then FC.clear c

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
  let pages = List.init 5 (fun slot -> Page.start (app slot)) in
  cell_alone 5 ~clear:true ;
  cell_alone 6 ~clear:false ;
  check "five live models" (List.for_all (Weak.check models) [0; 1; 2; 3; 4]) ;
  List.iteri
    (fun slot p -> if slot <> 3 then Page.click p (By.text "Hide"))
    pages ;
  later (fun () ->
      collect () ;
      later (fun () ->
          collect () ;
          later (fun () ->
              check "the futures and the promises wait"
                (Jv.to_int (Jv.get held "length") = 8) ;
              check "slot 0, a top-level make: the model is free"
                (not (Weak.check models 0)) ;
              check "slot 1, a make closure that holds the model: free"
                (not (Weak.check models 1)) ;
              check "slot 2, of_fut_or_error: the model is free"
                (not (Weak.check models 2)) ;
              check "slot 3, the mounted control: the model is alive"
                (Weak.check models 3) ;
              check "slot 4, a model kept on purpose: alive (sensitivity)"
                (Weak.check models 4) ;
              check "slot 5, a cleared cell: the value is free"
                (not (Weak.check models 5)) ;
              check "slot 6, a cell with no clear: the value is alive"
                (Weak.check models 6) ;
              match List.rev !failures with
              | [] ->
                  print_endline "ok"
              | l ->
                  List.iter (fun s -> print_endline ("FAIL: " ^ s)) l ;
                  exit 1 ) ) )
