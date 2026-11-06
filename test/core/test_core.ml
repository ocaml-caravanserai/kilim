let () =
  exit
    (Windtrap.run "core"
       (Test_cancel.tests @ Test_run_queue.tests @ Test_async.tests) )
