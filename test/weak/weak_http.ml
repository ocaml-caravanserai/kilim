(* The weak test of kilim.http.browser (design section 12), in the js_of_ocaml
   and wasm_of_ocaml builds, with the rules of weak.ml. A node server never
   answers. The task of each component sends a request to it with the fetch of
   node, and its continuation holds the model:

   - Slot 0: the instance unmounts. The cancel aborts the request: the model
   must be free. - Slot 1: the control. The instance stays mounted, and its
   model is alive. - Slot 2: the sensitivity control. The instance unmounts, but
   init puts the model in a global array on purpose: the model is alive. Thus
   the check finds a model that something keeps.

   Page.start starts the mock of kilim.testing, thus the test installs the fetch
   transport after the pages start, and a click mounts each component. *)

open Kilim
module Page = Kilim_testing.Page
module By = Kilim_testing.By

let global name = Jv.get Jv.global name

let held =
  let a = Jv.new' (global "Array") [||] in
  Jv.set Jv.global "kilimHeld" a ;
  a

let models = Weak.create 3

let url = ref ""

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
    if slot = 2 then ignore (Jv.call held "push" [|Jv.repr m|]) ;
    ( m
    , Cmd.task
        (Async.map
           (fun r ->
             Result.fold ~ok:String.length ~error:(fun _ -> 0) r
             + Bytes.length m )
           (Kilim_http.get !url) ) )

  let update _ _ m _ = (m, Cmd.none)

  let view _ m = txt (string_of_int (Bytes.length m))
end

type msg = Show | Hide

let app slot =
  App.make
    ~init:(fun () -> (false, Cmd.none))
    ~update:(fun _ -> function
      | Show -> (true, Cmd.none) | Hide -> (false, Cmd.none) )
    ~view:(fun shown ->
      El.div
        [ El.button ~ev:[Ev.click Show] [txt "Show"]
        ; El.button ~ev:[Ev.click Hide] [txt "Hide"]
        ; ( if shown then
              Component.v (module Holder) ~on:(function _ -> .) slot
            else txt "" ) ] )
    ()

let later ms f =
  ignore
    (Jv.apply (global "setTimeout")
       [|Jv.callback ~arity:1 (fun _ -> f ()); Jv.of_int ms|] )

let failures = ref []

let check name ok = if not ok then failures := name :: !failures

let collect () =
  if Jv.is_undefined (global "gc") then begin
    print_endline "FAIL: no globalThis.gc: run node with --expose-gc" ;
    exit 1
  end ;
  ignore (Jv.apply (global "gc") [||])

let requests = ref 0

let run stop =
  let pages = List.init 3 (fun slot -> Page.start (app slot)) in
  Kilim_http_browser.install () ;
  List.iter (fun p -> Page.click p (By.text "Show")) pages ;
  check "three live models" (List.for_all (Weak.check models) [0; 1; 2]) ;
  (* The requests reach the server before the unmount. *)
  later 200 (fun () ->
      check "three requests at the server" (!requests = 3) ;
      List.iteri
        (fun slot p -> if slot <> 1 then Page.click p (By.text "Hide"))
        pages ;
      later 0 (fun () ->
          collect () ;
          later 0 (fun () ->
              collect () ;
              later 0 (fun () ->
                  check "slot 0, an unmount during a request: the model is free"
                    (not (Weak.check models 0)) ;
                  check "slot 1, the mounted control: the model is alive"
                    (Weak.check models 1) ;
                  check "slot 2, a model kept on purpose: alive (sensitivity)"
                    (Weak.check models 2) ;
                  stop () ;
                  match List.rev !failures with
                  | [] ->
                      print_endline "ok" ; exit 0
                  | l ->
                      List.iter (fun s -> print_endline ("FAIL: " ^ s)) l ;
                      exit 1 ) ) ) )

(* A server of node that never answers. *)
let () =
  let http =
    Jv.call (global "process") "getBuiltinModule" [|Jv.of_string "node:http"|]
  in
  let server =
    Jv.call http "createServer"
      [|Jv.callback ~arity:2 (fun _ _ -> incr requests)|]
  in
  ignore
    (Jv.call server "listen"
       [| Jv.of_int 0
        ; Jv.of_string "127.0.0.1"
        ; Jv.callback ~arity:1 (fun _ ->
              let port =
                Jv.to_int (Jv.get (Jv.call server "address" [||]) "port")
              in
              url := "http://127.0.0.1:" ^ string_of_int port ^ "/never" ;
              run (fun () ->
                  ignore (Jv.call server "closeAllConnections" [||]) ;
                  ignore (Jv.call server "close" [||]) ) ) |] )
