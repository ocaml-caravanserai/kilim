(* The async tasks of kilim on the host of kilim.browser (design section 12),
   and the order of its queues. Also Component.shallow_equal with the identity
   that kilim.browser installs (design section 09). *)

open Brr
open Kilim
module C = Private.Cancel
module Q = Private.Run_queue

let failures = ref 0

let pending = ref 0

let check name ok =
  print_endline ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

(* The host of kilim.browser: the checks test the shipped host. *)
let host = Kilim_browser.Private.host

let queue_microtask = host.queue_microtask

let rec run = lazy (Q.create ~schedule:(fun () -> queue_microtask turn_now))

and turn f =
  let r = Lazy.force run in
  Q.with_turn r (fun () -> f () ; Q.drain r)

and turn_now () = turn (fun () -> ())

let start tok t ~ok ~error =
  turn (fun () ->
      Private.Async.start ~host ~run:(Lazy.force run) tok t ~ok ~error )

(* A fetch with an abort. The response and the body are one step: an end action
   also runs when a step ends normally, thus an abort at the end of a step that
   gave only the headers would stop the body. The binding aborts only on a
   cancel. *)
let client_aborts = ref 0

let fetch_text url : string Async.t =
  Async.make (fun tok ~ok ~error ->
      let ctrl = Abort.controller () in
      Cancel.on_end tok (fun () ->
          if Cancel.cancelled tok then begin
            incr client_aborts ; Abort.abort ctrl
          end ) ;
      let init = Brr_io.Fetch.Request.init ~signal:(Abort.signal ctrl) () in
      Fut.await
        (Brr_io.Fetch.url ~init (Jstr.v url))
        (function
          | Error e ->
              error (Jv.Error e)
          | Ok r ->
              Fut.await
                (Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body r))
                (function
                  | Ok t -> ok (Jstr.to_string t) | Error e -> error (Jv.Error e) ) ) )

(* A local HTTP server in node (process.getBuiltinModule), with a slow route. *)
let with_server f =
  let http =
    Jv.call
      (Jv.get Jv.global "process")
      "getBuiltinModule"
      [|Jv.of_string "node:http"|]
  in
  let handler =
    Jv.callback ~arity:2 (fun _req res ->
        ignore
          (G.set_timeout ~ms:150 (fun () ->
               if not (Jv.to_bool (Jv.get res "writableEnded")) then
                 ignore (Jv.call res "end" [|Jv.of_string "slow body"|]) ) ) )
  in
  let server = Jv.call http "createServer" [|handler|] in
  let on_listen () =
    let port = Jv.to_int (Jv.get (Jv.call server "address" [||]) "port") in
    let stop () =
      ignore (Jv.call server "close" [||]) ;
      ignore (Jv.call server "closeAllConnections" [||])
    in
    f ("http://127.0.0.1:" ^ string_of_int port ^ "/slow") stop
  in
  ignore
    (Jv.call server "listen"
       [| Jv.of_int 0
        ; Jv.of_string "127.0.0.1"
        ; Jv.callback ~arity:1 (fun _ -> on_listen ()) |] )

(* Each check finishes later: [wait ()] gives the function that ends the check.
   The program reports when all checks are done. *)
let wait () =
  incr pending ;
  fun f ->
    f () ;
    decr pending ;
    if !pending = 0 then begin
      print_endline
        ( if !failures = 0 then "all browser checks pass"
          else "browser checks FAILED" ) ;
      (* The MessageChannel port of the host keeps node alive. *)
      exit (if !failures > 0 then 1 else 0)
    end

let later ms f =
  let finish = wait () in
  ignore (G.set_timeout ~ms (fun () -> finish f))

(* The queues of the host: three tasks run in order, each in its own task. A
   microtask from a task runs before the next task. Then the count of the live
   timers. The checks run in the callbacks, not after a fixed time: node runs a
   timer that is due before a message of a MessageChannel. [k] starts the other
   checks at the end, thus no other timer is live during these checks. *)
let queues k =
  let log = ref [] in
  let add s = log := s :: !log in
  let finish = wait () in
  let live = Kilim_browser.Private.live_timers in
  let timers () =
    let base = live () in
    let clear = host.set_timer ~ms:10 ignore in
    let one = live () = base + 1 in
    clear () ;
    clear () ;
    let cleared = live () = base in
    let (_ : unit -> unit) =
      host.set_timer ~ms:10 (fun () ->
          finish (fun () ->
              check
                "the live timers: 1 after a set, 0 after a clear, 0 after a run"
                (one && cleared && live () = base) ;
              k () ) )
    in
    check "the live timers: 1 after the second set" (live () = base + 1)
  in
  host.post_task (fun () ->
      add "t1" ;
      host.queue_microtask (fun () -> add "m1") ) ;
  host.post_task (fun () -> add "t2") ;
  host.post_task (fun () ->
      add "t3" ;
      check
        ( "three tasks in order, and a microtask before the next task: "
        ^ String.concat " " (List.rev !log) )
        (List.rev !log = ["t1"; "m1"; "t2"; "t3"]) ;
      timers () )

(* Component.shallow_equal with the identity that kilim.browser installs:
   Object.is under js_of_ocaml, ( == ) under wasm_of_ocaml. [f] makes a new
   float box, and [s] a new string. *)
type fr = {x: float; y: float} [@@warning "-69"]

let shallow () =
  let eq = Component.shallow_equal in
  let name s = "H shallow_equal: " ^ s in
  let f x = Int64.float_of_bits (Int64.bits_of_float (Sys.opaque_identity x)) in
  let s n = "row " ^ string_of_int (Sys.opaque_identity n) in
  let jstr n = Jstr.v (s n) in
  check
    (name "a string built two times is equal")
    (eq (s 1) (s 1) && eq (1, s 1) (1, s 1) && not (eq (1, s 1) (1, s 2))) ;
  (* Under wasm_of_ocaml, ( == ) gives true for two equal JS strings. *)
  check
    (name "a JS string built two times is equal")
    (eq (1, jstr 1) (1, jstr 1) && not (eq (1, jstr 1) (1, jstr 2))) ;
  check
    (name "the JS numbers 0 and -0 are not equal")
    ( (not (eq (Jv.of_float 0.) (Jv.of_float (-0.))))
    && eq (Jv.of_float 2.) (Jv.of_float 2.) ) ;
  check
    (name "0. and -0. are not equal, also in a field, a float record and array")
    ( (not (eq (f 0.) (f (-0.))))
    && (not (eq (1, f 0.) (1, f (-0.))))
    && (not (eq {x= f 0.; y= 1.} {x= f (-0.); y= 1.}))
    && not (eq [|f 0.|] [|f (-0.)|]) ) ;
  check
    (name "NaN and NaN are equal, also in a field and a float record")
    ( eq (f nan) (f nan)
    && eq (1, f nan) (1, f nan)
    && eq {x= f nan; y= 1.} {x= f nan; y= 1.} ) ;
  check
    (name "a float record built two times is equal")
    ( eq {x= f 1.; y= f 2.} {x= f 1.; y= f 2.}
    && not (eq {x= f 1.; y= 2.} {x= f 3.; y= 2.}) ) ;
  let o = Jv.obj [||] in
  check
    (name "two JS objects are not equal")
    ( (not (eq (Jv.obj [||]) (Jv.obj [||])))
    && not (eq (1, Jv.obj [||]) (1, Jv.obj [||])) ) ;
  check (name "the same JS object is equal") (eq o o && eq (1, o) (1, o)) ;
  (* Under js_of_ocaml, the tag of these JS arrays is 253 (a float) and 254 (a
     float array). *)
  let arr l = Jv.of_jv_array (Array.of_list l) in
  let a253 n = arr [Jv.of_int 253; Jv.of_int n] in
  let a254 v = arr [Jv.of_int 254; Jv.of_string v] in
  check
    (name "the JS arrays [253, 1] and [253, 2] are not equal, also in a field")
    ((not (eq (a253 1) (a253 2))) && not (eq (1, a253 1) (1, a253 2))) ;
  check
    (name "the JS arrays [254, \"a\"] and [254, \"b\"] are not equal")
    ((not (eq (a254 "a") (a254 "b"))) && not (eq (1, a254 "a") (1, a254 "b"))) ;
  let mk n x = x + n in
  let g = mk (Sys.opaque_identity 1) in
  check
    (name "a closure field compares by identity")
    ( (not (eq (1, mk (Sys.opaque_identity 1)) (1, mk (Sys.opaque_identity 1))))
    && eq (1, g) (1, g)
    && not (eq (mk (Sys.opaque_identity 1)) (mk (Sys.opaque_identity 1))) ) ;
  (* The property of test/core, with 2,000 lists of numbers. *)
  let rng = Random.State.make [|42|] in
  let errors = ref [] in
  for _ = 1 to 2000 do
    let draws =
      List.init
        (1 + Random.State.int rng 40)
        (fun _ -> Random.State.int rng 1001)
    in
    errors := Shallow_law.check draws @ !errors
  done ;
  List.iter print_endline !errors ;
  check (name "true means the same view (2,000 cases)") (!errors = [])

(* Async.of_fut and of_fut_or_error (design section 12), and the JS cell of
   of_fut. Each result comes in a later microtask. *)
module BA = Kilim_browser.Async
module FC = Kilim_browser.Private.Fut_cell

type rcd = {name: string; n: int}

let of_fut () =
  let got = ref [] in
  let add s = got := s :: !got in
  let keep name t =
    start (C.root ()) t
      ~ok:(fun v -> add (name ^ " ok " ^ v))
      ~error:(function
        | Jv.Error e ->
            add (name ^ " js " ^ Jstr.to_string (Jv.Error.message e))
        | Failure m ->
            add (name ^ " failure " ^ m)
        | _ ->
            add (name ^ " other") )
  in
  keep "int" (BA.map string_of_int (BA.of_fut (fun _ -> Fut.return 42))) ;
  keep "record"
    (BA.map
       (fun r -> r.name ^ string_of_int r.n)
       (BA.of_fut (fun _ -> Fut.return {name= "r"; n= 7})) ) ;
  keep "or_error" (BA.of_fut_or_error (fun _ -> Fut.return (Ok "fine"))) ;
  keep "rejected"
    (BA.of_fut_or_error (fun _ -> Fut.error (Jv.Error.v (Jstr.v "boom")))) ;
  keep "raise" (BA.of_fut (fun _ -> failwith "make")) ;
  (* Cold: make runs at each start, not when the task is made. *)
  let calls = ref 0 in
  let cold =
    BA.of_fut (fun _ ->
        incr calls ;
        Fut.return (string_of_int !calls) )
  in
  let made = !calls in
  keep "cold" cold ;
  keep "cold" cold ;
  (* A cancel before the future settles: no result. *)
  let fut, set = Fut.create () in
  let tok = C.root () in
  start tok
    (BA.of_fut (fun _ -> fut))
    ~ok:(fun () -> add "cancelled ok")
    ~error:(fun _ -> add "cancelled error") ;
  C.cancel tok ;
  set () ;
  (* The cell: the end of the token clears it (the guard of the clear: the weak
     tests do not find it). W4 tests the reaction of the cell. *)
  let never, _ = Fut.create () in
  let tok = C.root () in
  let cell = Kilim_browser.Private.fut_start (fun _ -> never) tok ~ok:ignore in
  let full = not (FC.is_empty cell) in
  C.cancel tok ;
  check "H of_fut: the cell holds the continuation until the cancel" full ;
  check "H of_fut: a cancel empties the cell" (FC.is_empty cell) ;
  later 50 (fun () ->
      let l = List.sort compare !got in
      check
        ("H of_fut: the results: " ^ String.concat ", " l)
        ( l
        = [ "cold ok 1"
          ; "cold ok 2"
          ; "int ok 42"
          ; "or_error ok fine"
          ; "raise failure make"
          ; "record ok r7"
          ; "rejected js boom" ] ) ;
      check "H of_fut: cold, make runs at each start" (made = 0 && !calls = 2) )

(* A raise in the future chain of make: the promise of Brr rejects, and the task
   gives nothing until a cancel, which empties the cell. Node reports each
   unhandled rejection: the test counts them, thus node does not exit. *)
let late_raise () =
  let rejections = ref 0 in
  ignore
    (Jv.call
       (Jv.get Jv.global "process")
       "on"
       [| Jv.of_string "unhandledRejection"
        ; Jv.callback ~arity:1 (fun _ -> incr rejections) |] ) ;
  let late _ = Fut.map (fun () -> failwith "late") (Fut.return ()) in
  let got = ref 0 in
  let tok = C.root () in
  start tok (BA.of_fut late) ~ok:(fun () -> incr got) ~error:(fun _ -> incr got) ;
  let tok' = C.root () in
  let cell =
    Kilim_browser.Private.fut_start late tok' ~ok:(fun () -> incr got)
  in
  let finish = wait () in
  ignore
    (G.set_timeout ~ms:50 (fun () ->
         let full = not (FC.is_empty cell) in
         C.cancel tok ;
         C.cancel tok' ;
         ignore
           (G.set_timeout ~ms:20 (fun () ->
                finish (fun () ->
                    check
                      "H of_fut: a raise in the future chain: no result, two \
                       unhandled rejections"
                      (!got = 0 && !rejections = 2 && full) ;
                    check "H of_fut: after the raise, a cancel empties the cell"
                      (FC.is_empty cell && !got = 0) ) ) ) ) )

(* Es_module.import on node: a data: URL, a bare specifier stays as it is, and a
   throw of the resolve gives an error (node has no document: a fake one). *)
let es_module () =
  let finish = wait () in
  let import s = Kilim_web.Es_module.import (Jstr.v s) in
  Fut.await (import "data:text/javascript,export const x = 42;") (fun data ->
      Fut.await (import "kilim-no-such-package") (fun bare ->
          Jv.set Jv.global "document"
            (Jv.obj [|("baseURI", Jv.of_string "not a URL")|]) ;
          let thrown = import "./x.mjs" in
          Jv.delete Jv.global "document" ;
          let no_document = import "./kilim-missing.mjs" in
          Fut.await thrown (fun thrown ->
              Fut.await no_document (fun no_document ->
                  finish (fun () ->
                      check "H Es_module: a data: URL"
                        ( match data with
                        | Ok m ->
                            Jv.to_int (Jv.get m "x") = 42
                        | Error _ ->
                            false ) ;
                      let message = function
                        | Error e ->
                            Jstr.to_string (Jv.Error.message e)
                        | Ok _ ->
                            "ok"
                      in
                      check "H Es_module: a bare specifier stays as it is"
                        ( Jstr.find_sub
                            ~sub:(Jstr.v "'kilim-no-such-package'")
                            (Jstr.v (message bare))
                        <> None ) ;
                      check "H Es_module: a throw of the resolve is an error"
                        ( match thrown with
                        | Error e ->
                            Jstr.equal (Jv.Error.name e) (Jstr.v "TypeError")
                        | Ok _ ->
                            false ) ;
                      (* With no document, node loads the specifier as it is: a
                         missing module gives the name Error on node. *)
                      check "H Es_module: no document, ./ stays as it is"
                        ( match no_document with
                        | Error e ->
                            Jstr.equal (Jv.Error.name e) (Jstr.v "Error")
                        | Ok _ ->
                            false ) ) ) ) ) )

let () =
  shallow () ;
  of_fut () ;
  late_raise () ;
  es_module () ;
  queues
  @@ fun () ->
  (* A real timer. *)
  let fired = ref false in
  start (C.root ())
    (Async.sleep (Span.ms 30))
    ~ok:(fun () -> fired := true)
    ~error:(fun _ -> ()) ;
  later 100 (fun () -> check "a real timer fires" !fired) ;
  (* A cancel clears the real timer. *)
  let fired = ref false and tok = C.root () in
  start tok
    (Async.sleep (Span.ms 30))
    ~ok:(fun () -> fired := true)
    ~error:(fun _ -> ()) ;
  C.cancel tok ;
  later 100 (fun () -> check "a cancel clears a real timer" (not !fired)) ;
  with_server (fun url stop ->
      (* A fetch that completes: the body arrives, also after the end of the
         step. *)
      let body = ref None in
      start (C.root ()) (fetch_text url)
        ~ok:(fun s -> body := Some s)
        ~error:(fun _ -> body := Some "<error>") ;
      (* A fetch with a cancel: the request is aborted, and no result
         arrives. *)
      let got = ref false and tok = C.root () in
      start tok (fetch_text url)
        ~ok:(fun _ -> got := true)
        ~error:(fun _ -> got := true) ;
      ignore (G.set_timeout ~ms:20 (fun () -> C.cancel tok)) ;
      later 600 (fun () ->
          check "a fetch gives its body" (!body = Some "slow body") ;
          (* undici in node keeps the server side open after a client abort,
             thus the check is on the client side: the abort ran, and no result
             arrived. *)
          check "a cancel aborts the fetch, and no result arrives"
            (!client_aborts = 1 && not !got) ;
          stop () ) )
