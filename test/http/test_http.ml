let () = exit (Windtrap.run "http" (Test_json.tests @ Test_tasks.tests))
