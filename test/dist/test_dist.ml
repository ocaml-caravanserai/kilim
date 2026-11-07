let () =
  exit
    (Windtrap.run "dist"
       ( Test_text.tests @ Test_part.tests @ Test_template.tests
       @ Test_names.tests @ Test_cli.tests @ Test_run.tests ) )
