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

(* The partial update budget: 1,000 keyed component rows. The props of a row are
   its index and a mark, and its model is a count. A click on a row adds 1 to
   its count. *)
module P = Kilim_testing.Page
module By = Kilim_testing.By

module Row = struct
  type props = int * bool

  type model = int

  type msg = unit

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let task = ref false

  let init _ =
    ( 0
    , if !task then Cmd.task (Async.make (fun _ ~ok:_ ~error:_ -> ()))
      else Cmd.none )

  let update _ _ m () = (m + 1, Cmd.none)

  let view (i, mark) m =
    El.li
      ~at:[At.data "testid" ("r" ^ string_of_int i)]
      ~ev:[Ev.click ()]
      [ txt
          (string_of_int i ^ (if mark then "!" else "") ^ " " ^ string_of_int m)
      ]

  let subs _ _ = Sub.none

  let equal_props (i, a) (j, b) = Int.equal i j && Bool.equal a b
end

let row_view marked i =
  (Key.int i, Component.v (module Row) ~on:(function _ -> .) (i, i = marked))

let control id msg = El.button ~at:[At.data "testid" id] ~ev:[Ev.click msg] []

(* The model of the parent: the header count, the marked row and the number of
   rows. *)
let component_rows ?(task = false) n =
  Row.task := task ;
  P.start
    (App.make
       ~init:(fun () -> ((0, -1, n), Cmd.none))
       ~update:(fun (h, r, n) -> function
         | `Header ->
             ((h + 1, r, n), Cmd.none)
         | `Mark ->
             ((h, 500, n), Cmd.none)
         | `Clear ->
             ((h, r, 0), Cmd.none) )
       ~view:(fun (h, r, n) ->
         El.div
           [ El.h1 [txt (string_of_int h)]
           ; control "header" `Header
           ; control "mark" `Mark
           ; control "clear" `Clear
           ; View.keyed El.ul (List.init n (row_view r)) ] )
       () )

(* The views and the DOM operations of a click. *)
let partial name ~views expected id =
  test name (fun () ->
      let p = component_rows 1000 in
      let v = (P.stats p).views in
      F.reset (P.doc p) ;
      P.click p (By.test_id id) ;
      equal int ~msg:"views" views ((P.stats p).views - v) ;
      equal ops expected (F.counts (P.doc p)) ;
      equal int 1000 (P.tree p).instances )

(* The token release budget: the operations on the lists of Cancel when the
   parent removes [n] rows, each with a token. *)
let release n =
  let links = Private.Cancel.list_ops () in
  let p = component_rows ~task:true n in
  equal int ~msg:"the links of the mount: the root, then 3 for each row"
    ((3 * n) + 1)
    (Private.Cancel.list_ops () - links) ;
  equal int ~msg:"a token, a task and a step for each row" (3 * n)
    (P.tree p).tokens ;
  let before = Private.Cancel.list_ops () in
  P.click p (By.test_id "clear") ;
  equal int ~msg:"no token stays" 0 (P.tree p).tokens ;
  Private.Cancel.list_ops () - before

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
            p )
      ; partial "1 component row of 1,000 changes its model" ~views:1
          [("set_text", 1)]
          "r500"
      ; partial "the parent changes a text outside 1,000 component rows"
          ~views:1
          [("set_text", 1)]
          "header"
      ; partial "the parent changes the props of 1 component row of 1,000"
          ~views:2
          [("set_text", 1)]
          "mark"
      ; test "remove 1,000 and 2,000 component rows with a token: linear"
          (fun () ->
            let a = release 1000 and b = release 2000 in
            equal int ~msg:"1,000 rows" 1000 a ;
            equal int ~msg:"2,000 rows" 2000 b ) ] ]
