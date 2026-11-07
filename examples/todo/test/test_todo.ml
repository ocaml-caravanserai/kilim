(* The native test of the todo example. The test drives the app only through the
   queries and the events of Page, as a user test does.

   The fake DOM does not do the default action of a form: Enter in the input and
   a click on the "Add" button send no submit event. Thus the test sends the
   submit event to the form. The Chrome test (test/examples) presses Enter and
   clicks "Add". *)

open Windtrap
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module Host = Kilim_testing.Host
module Fake_dom = Kilim_testing.Fake_dom

let no_errors p =
  equal int ~msg:"no error" 0 (List.length (Host.errors (Page.host p))) ;
  equal (list string) ~msg:"no warning" [] (Host.warnings (Page.host p))

let draft p = Page.find p (By.label "New item")

let add p label =
  Page.input p (By.label "New item") label ;
  Page.submit p (By.label "Add an item")

(* The li elements of the list, and the label of each row. *)
let items p = Fake_dom.children (Page.find p (By.test_id "items"))

let label li =
  Fake_dom.text
    (List.find (fun n -> Fake_dom.name n = "span") (Fake_dom.children li))

let rows p = List.map label (items p)

let left p = Fake_dom.text (Page.find p (By.test_id "left"))

let pressed p name =
  Fake_dom.attribute (Page.find p (By.text name)) "aria-pressed"

let first_render () =
  let p = Page.start Todo.app in
  equal string
    "<div><section><form aria-label=\"Add an item\"><label for=\"new\">New \
     item</label><input id=\"new\" placeholder=\"What to do?\"><button \
     type=\"submit\">Add</button></form><ul \
     data-testid=\"items\"></ul><footer><span data-testid=\"left\">0 items \
     left</span><button aria-pressed=\"true\">All</button><button \
     aria-pressed=\"false\">Active</button><button \
     aria-pressed=\"false\">Completed</button><button>Clear \
     completed</button></footer></section></div>"
    (Page.html p) ;
  no_errors p

let actions () =
  let p = Page.start Todo.app in
  add p "Buy milk" ;
  equal string ~msg:"the draft after Add" "" (Fake_dom.value (draft p)) ;
  add p "  Walk " ;
  equal (list string) ~msg:"Add trims the label" ["Buy milk"; "Walk"] (rows p) ;
  equal string "2 items left" (left p) ;
  Page.click p (By.label "Done: Buy milk") ;
  is_true ~msg:"the live checked"
    (Fake_dom.checked (Page.find p (By.label "Done: Buy milk"))) ;
  equal
    (list (option string))
    ~msg:"the classes" [Some "done"; None]
    (List.map (fun li -> Fake_dom.attribute li "class") (items p)) ;
  equal string "1 item left" (left p) ;
  let show name expected =
    Page.click p (By.text name) ;
    equal (list string) ~msg:name expected (rows p) ;
    equal (option string) ~msg:(name ^ " pressed") (Some "true")
      (pressed p name)
  in
  show "Active" ["Walk"] ;
  equal (option string) (Some "false") (pressed p "All") ;
  show "Completed" ["Buy milk"] ;
  show "All" ["Buy milk"; "Walk"] ;
  add p "Read" ;
  Page.click p (By.label "Delete Walk") ;
  equal (list string) ~msg:"after the delete" ["Buy milk"; "Read"] (rows p) ;
  Page.click p (By.text "Clear completed") ;
  equal (list string) ~msg:"after the clear" ["Read"] (rows p) ;
  equal string "1 item left" (left p) ;
  Page.stop p ;
  Page.expect_no_tasks p ;
  no_errors p

let blank () =
  let p = Page.start Todo.app in
  add p "   " ;
  equal (list string) ~msg:"no row" [] (rows p) ;
  equal string ~msg:"the draft stays" "   " (Fake_dom.value (draft p)) ;
  no_errors p

let restore () =
  let p = Page.start Todo.app in
  Page.input p (By.label "New item") "abc" ;
  Page.input p (By.label "New item") (String.make 81 'a') ;
  equal string ~msg:"81 bytes: the restore" "abc" (Fake_dom.value (draft p)) ;
  let s80 = String.make 80 'a' in
  Page.input p (By.label "New item") s80 ;
  equal string ~msg:"80 bytes" s80 (Fake_dom.value (draft p)) ;
  Page.submit p (By.label "Add an item") ;
  equal (list string) [s80] (rows p) ;
  no_errors p

let keyed () =
  let p = Page.start Todo.app in
  add p "Buy milk" ;
  add p "Walk" ;
  let walk = List.nth (items p) 1 in
  Page.click p (By.label "Delete Buy milk") ;
  is_true ~msg:"the same li node" (List.hd (items p) == walk) ;
  equal (list string) ["Walk"] (rows p) ;
  no_errors p

let () =
  exit
    (Windtrap.run "todo"
       [ test "the first render" first_render
       ; test "add, toggle, filter, delete and clear" actions
       ; test "a blank draft adds no row" blank
       ; test "a draft of more than 80 bytes: the restore" restore
       ; test "a row keeps its node when a row before it goes" keyed ] )
