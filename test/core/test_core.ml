let () =
  exit
    (Windtrap.run "core"
       ( Test_cancel.tests @ Test_dlist.tests @ Test_run_queue.tests
       @ Test_async.tests @ Test_shallow.tests @ Test_url.tests ) )
