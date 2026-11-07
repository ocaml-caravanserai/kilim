(* The budgets of DOM operations (design section 16): exact counts on the fake
   DOM. A change of a count is a change of the budget. *)

open Windtrap
open Kilim
open Support

let ops = list (pair string int)

let row k label = (Key.int k, El.li ~at:[At.class_ "row"] [txt label])

let table labels = View.keyed El.ul (List.mapi row labels)

let labels n = List.init n (fun i -> "row " ^ string_of_int i)

let budget name expected ?moves f =
  test name (fun () ->
      let p = f () in
      equal ops expected (counts p) ;
      Option.iter (fun m -> equal int ~msg:"moves" m (F.moves p.doc)) moves ;
      check_tree p )

let rows1000 () = mount (table (labels 1000))

let tests =
  [ group "budgets"
      [ budget "create 1,000 rows"
          [ ("attach", 1000)
          ; ("create_element", 1000)
          ; ("create_text", 1000)
          ; ("insert_before", 2000)
          ; ("set_attribute", 1000) ]
          (fun () ->
            let p = mount (table []) in
            patch p (table (labels 1000)) ;
            p )
      ; budget "change 1 row of 1,000"
          [("set_text", 1)]
          (fun () ->
            let p = rows1000 () in
            patch p
              (table
                 (List.mapi
                    (fun i l -> if i = 500 then "x" else l)
                    (labels 1000) ) ) ;
            p )
      ; budget "swap 2 rows of 1,000"
          [("insert_before", 2)]
          ~moves:2
          (fun () ->
            let p = rows1000 () in
            let a = Array.of_list (List.mapi row (labels 1000)) in
            let t = a.(1) in
            a.(1) <- a.(998) ;
            a.(998) <- t ;
            patch p (View.keyed El.ul (Array.to_list a)) ;
            p )
      ; budget "remove all rows of 1,000"
          [("clear", 1)]
          (fun () ->
            let p = rows1000 () in
            patch p (table []) ;
            p )
      ; budget "reverse 1,000 rows"
          [("insert_before", 999)]
          ~moves:999
          (fun () ->
            let p = rows1000 () in
            patch p (View.keyed El.ul (List.rev (List.mapi row (labels 1000)))) ;
            p ) ] ]
