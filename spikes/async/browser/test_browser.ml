open Brr
open Kilim_spike_async

let failures = ref 0

let pending = ref 0

let check name ok =
  print_endline ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

let queue_microtask f =
  ignore
    (Jv.call Jv.global "queueMicrotask"
       [|Jv.callback ~arity:1 (fun _ -> f ())|] )

let set_timer ~ms f =
  let id = G.set_timeout ~ms f in
  fun () -> G.stop_timer id

let host =
  {Async.now_ms= (fun () -> Performance.now_ms G.performance); set_timer}

let rec run =
  lazy (Async.Run_queue.create ~schedule:(fun () -> queue_microtask turn_now))

and turn f =
  let r = Lazy.force run in
  r.in_turn <- true ;
  f () ;
  Async.Run_queue.drain r ;
  r.in_turn <- false

and turn_now () = turn (fun () -> ())

let start tok t ~ok ~error =
  turn (fun () -> Async.start ~host ~run:(Lazy.force run) tok t ~ok ~error)

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
    Jv.callback ~arity:2 (fun req res ->
        ignore req ;
        ignore
          (Jv.call (Jv.get req "socket") "on"
             [|Jv.of_string "close"; Jv.callback ~arity:0 (fun () -> ())|] ) ;
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

(* Each check finishes later; the program reports when all are done. *)
let later ms name f =
  incr pending ;
  ignore
    (G.set_timeout ~ms (fun () ->
         f () ;
         decr pending ;
         if !pending = 0 then begin
           print_endline
             ( if !failures = 0 then "all browser checks pass"
               else "browser checks FAILED" ) ;
           if !failures > 0 then exit 1
         end ) ) ;
  ignore name

let () =
  (* A real timer. *)
  let fired = ref false in
  start (Cancel.root ()) (Async.sleep ~ms:30)
    ~ok:(fun () -> fired := true)
    ~error:(fun _ -> ()) ;
  later 100 "sleep" (fun () -> check "a real timer fires" !fired) ;
  (* A cancel clears the real timer. *)
  let fired = ref false and tok = Cancel.root () in
  start tok (Async.sleep ~ms:30)
    ~ok:(fun () -> fired := true)
    ~error:(fun _ -> ()) ;
  Cancel.cancel tok ;
  later 100 "sleep cancel" (fun () ->
      check "a cancel clears a real timer" (not !fired) ) ;
  with_server (fun url stop ->
      (* A fetch that completes: the body arrives, also after the end of the
         step. *)
      let body = ref None in
      start (Cancel.root ()) (fetch_text url)
        ~ok:(fun s -> body := Some s)
        ~error:(fun _ -> body := Some "<error>") ;
      (* A fetch with a cancel: the request is aborted, and no result
         arrives. *)
      let got = ref false and tok = Cancel.root () in
      start tok (fetch_text url)
        ~ok:(fun _ -> got := true)
        ~error:(fun _ -> got := true) ;
      ignore (G.set_timeout ~ms:20 (fun () -> Cancel.cancel tok)) ;
      later 600 "fetch" (fun () ->
          check "a fetch gives its body" (!body = Some "slow body") ;
          (* undici in node keeps the server side open after a client abort,
             thus the check is on the client side: the abort ran, and no result
             arrived. *)
          check "a cancel aborts the fetch, and no result arrives"
            (!client_aborts = 1 && not !got) ;
          stop () ) )
