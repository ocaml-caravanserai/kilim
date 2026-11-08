let () =
  exit
    (Windtrap.run "view"
       ( Test_el.tests @ Test_diff.tests @ Test_comp.tests @ Test_budget.tests
       @ Test_props.tests @ Test_fake.tests ) )
