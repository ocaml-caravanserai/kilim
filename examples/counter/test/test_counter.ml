(* The native test of the counter example. The test drives the app only through
   the queries and the events of Page, as a user test does. *)

open Windtrap
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module Host = Kilim_testing.Host
module Fake_dom = Kilim_testing.Fake_dom

let count p = Fake_dom.text (Page.find p (By.test_id "count"))

let no_errors p =
  equal int ~msg:"no error" 0 (List.length (Host.errors (Page.host p))) ;
  equal (list string) ~msg:"no warning" [] (Host.warnings (Page.host p))

let clicks () =
  let p = Page.start Counter.app in
  for _ = 1 to 3 do
    Page.click p (By.label "Increment")
  done ;
  Page.click p (By.label "Decrement") ;
  equal string "2" (count p) ;
  Page.click p (By.text "Reset") ;
  equal string "0" (count p) ;
  Page.stop p ;
  Page.expect_no_tasks p ;
  no_errors p

let first_render () =
  let p = Page.start Counter.app in
  equal string
    "<div><div><button aria-label=\"Decrement\">-</button><output \
     data-testid=\"count\">0</output><button \
     aria-label=\"Increment\">+</button><button>Reset</button></div></div>"
    (Page.html p) ;
  no_errors p

let () =
  exit
    (Windtrap.run "counter"
       [ test "the buttons change the count" clicks
       ; test "the first render" first_render ] )
