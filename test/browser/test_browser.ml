(* The async tasks of kilim on the host of kilim.browser (design section 12),
   and the order of its queues. *)

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

let () =
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
