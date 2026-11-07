(* The DOM operations of three actions of the todo example, on a list of 100
   items: exact counts on the fake DOM (docs/budgets.md). A change of a count is
   a change of the budget. The counts include the reads of the live properties
   ([get_value] and [get_checked]). *)

open Windtrap
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module Host = Kilim_testing.Host
module Fake_dom = Kilim_testing.Fake_dom

let ops = list (pair string int)

let add p label =
  Page.input p (By.label "New item") label ;
  Page.submit p (By.label "Add an item")

(* A page with 100 items. *)
let page () =
  let p = Page.start Todo.app in
  for i = 1 to 100 do
    add p ("item " ^ string_of_int i)
  done ;
  p

(* [prepare] runs before the counts go to 0. *)
let budget name expected ?(prepare = ignore) action =
  test name (fun () ->
      let p = page () in
      prepare p ;
      Fake_dom.reset (Page.doc p) ;
      action p ;
      equal ops expected (Fake_dom.counts (Page.doc p)) ;
      equal int ~msg:"no error" 0 (List.length (Host.errors (Page.host p))) )

let () =
  exit
    (Windtrap.run "todo counts"
       [ (* The check event reads the live checked of the target. The patch
            reads the 100 checkboxes and the draft, and it sets the class of the
            row and the text of the footer. Then the restore reads the
            target. *)
         budget "toggle 1 item of 100"
           [ ("get_checked", 102)
           ; ("get_value", 1)
           ; ("set_attribute", 1)
           ; ("set_text", 1) ]
           (fun p -> Page.click p (By.label "Done: item 50"))
       ; (* The new row: 4 elements, 2 text nodes and 3 attributes. The patch
            reads the 101 checkboxes and the draft, and it writes the empty
            draft. *)
         budget "add 1 item to 100"
           [ ("attach", 4)
           ; ("create_element", 4)
           ; ("create_text", 2)
           ; ("get_checked", 101)
           ; ("get_value", 1)
           ; ("insert_before", 6)
           ; ("set_attribute", 3)
           ; ("set_property", 1)
           ; ("set_text", 1) ]
           ~prepare:(fun p -> Page.input p (By.label "New item") "item 101")
           (fun p -> Page.submit p (By.label "Add an item"))
       ; (* The patch reads the 99 checkboxes and the draft, removes the row and
            sets the text of the footer. *)
         budget "delete 1 item of 100"
           [ ("get_checked", 99)
           ; ("get_value", 1)
           ; ("remove", 1)
           ; ("set_text", 1) ]
           (fun p -> Page.click p (By.label "Delete item 50")) ] )
