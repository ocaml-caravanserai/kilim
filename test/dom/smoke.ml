(* The smoke page of run.mjs: kilim.browser loads, the body of the page comes on
   request, and each queue of the host runs one time in the expected order. *)

open Driver

let host = Kilim_browser.Private.host

(* The runner serves the page of the program for a deep path under /page/PROG/,
   thus a reload of a path of a router works. The check reads the head and stops
   the read: the server holds the body. *)
let deep () =
  let p =
    js
      "(async () => { const prog = location.pathname.slice(6); const r = await \
       fetch('/page/' + prog + '/a/b?x=1'); const rd = r.body.getReader(); \
       const dec = new TextDecoder(); let t = ''; while \
       (!t.includes('</head>')) { const { value, done } = await rd.read(); if \
       (done) break; t += dec.decode(value); } rd.cancel(); return \
       t.includes('<script src=\"/' + prog + '\"></script>'); })()"
  in
  Fut.map
    (function Ok b -> b | Error _ -> false)
    (Fut.of_promise ~ok:Jv.to_bool p)

let () =
  let log = ref [] in
  let add s = log := s :: !log in
  let document = Jv.get Jv.global "document" in
  check "the document is loading"
    (Jv.to_string (Jv.get document "readyState") = "loading") ;
  Fut.await (release ()) (fun () ->
      Fut.await (loaded ()) (fun () ->
          check "the body arrived"
            (not
               (Jv.is_null
                  (Jv.call document "getElementById" [|Jv.of_string "app"|]) ) ) ;
          Fut.await (deep ()) (fun ok ->
              check "the page of the program for a deep path" ok ;
              host.post_task (fun () ->
                  add "task" ;
                  host.request_frame (fun () ->
                      add "frame" ;
                      let (_ : unit -> unit) =
                        host.set_timer ~ms:10 (fun () ->
                            add "timer" ;
                            check "the order of the queues"
                              ( List.rev !log
                              = ["microtask"; "task"; "frame"; "timer"] ) ;
                            check "no live timer"
                              (Kilim_browser.Private.live_timers () = 0) ;
                            finish () )
                      in
                      () ) ) ;
              host.queue_microtask (fun () -> add "microtask") ) ) )
