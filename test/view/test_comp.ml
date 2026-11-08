(* Component nodes in the diff (design sections 09 and 11), with no runtime: a
   component node has no DOM node of its own. *)

open Windtrap
open Kilim
open Support
module T = Private.Tree

let ops = list (pair string int)

let row k = (Key.int k, wrap (El.li [txt (string_of_int k)]))

let rows l = View.keyed El.ul (List.map row l)

(* The instance of each retained row, alive or not. *)
let alive p =
  match Diff.children (Diff.root p.tree) with
  | Some l ->
      List.map
        (fun (Diff.Child r) ->
          match r.T.shape with T.Comp c -> c.inst.alive | _ -> false )
        l
  | None ->
      []

let moved () =
  let p = mount (rows [1; 2; 3]) in
  let before = kids p in
  patch p (rows [3; 1; 2]) ;
  equal ops [("insert_before", 1)] (counts p) ;
  equal int 1 (F.moves p.doc) ;
  is_true ~msg:"the DOM nodes stay"
    (same_nodes (kids p) [List.nth before 2; List.hd before; List.nth before 1]) ;
  check_tree p

let middle () =
  let p = mount (rows [1; 2; 3; 4; 5]) in
  let before = Array.of_list (kids p) in
  patch p (rows [1; 4; 3; 6; 2; 5]) ;
  equal (list string) ["1"; "4"; "3"; "6"; "2"; "5"] (texts p) ;
  is_true ~msg:"the common start and end stay"
    (List.hd (kids p) == before.(0) && List.nth (kids p) 5 == before.(4)) ;
  is_true ~msg:"the middle moves"
    ( List.nth (kids p) 1 == before.(3)
    && List.nth (kids p) 2 == before.(2)
    && List.nth (kids p) 4 == before.(1) ) ;
  equal (list bool) [true; true; true; true; true; true] (alive p) ;
  check_tree p ;
  patch p (rows [4; 6]) ;
  equal (list string) ["4"; "6"] (texts p) ;
  check_tree p

let to_element () =
  let p = mount (El.div [wrap (El.p [txt "a"]); txt "b"]) in
  let inst =
    match Diff.children (Diff.root p.tree) with
    | Some (Diff.Child r :: _) -> (
      match r.T.shape with
      | T.Comp c ->
          fun () -> c.inst.alive
      | _ ->
          fail "a component node" )
    | _ ->
        fail "children"
  in
  is_true ~msg:"alive" (inst ()) ;
  patch p (El.div [El.p [txt "a"]; txt "b"]) ;
  is_false ~msg:"the replace unmounts" (inst ()) ;
  equal ops
    [ ("attach", 1)
    ; ("create_element", 1)
    ; ("create_text", 1)
    ; ("insert_before", 1)
    ; ("replace", 1) ]
    (counts p) ;
  check_tree p

let key_change () =
  let p = mount (El.div [wrap ~key:"a" (El.p [txt "a"])]) in
  let before = kids p in
  patch p (El.div [wrap ~key:"a" (El.p [txt "b"])]) ;
  is_true ~msg:"the same key keeps the node" (same_nodes before (kids p)) ;
  patch p (El.div [wrap ~key:"b" (El.p [txt "b"])]) ;
  is_false ~msg:"a new key replaces the node" (same_nodes before (kids p)) ;
  equal (list string) ["b"] (texts p) ;
  check_tree p

(* A patch keeps the newest [~on] of the parent in the component node. *)
let newest_on () =
  let v f = Private.Component.v (module Wrap) ~on:f (txt "a") in
  let p = mount (v (fun s -> "1" ^ s)) in
  patch p (v (fun s -> "2" ^ s)) ;
  match (Diff.root p.tree).T.shape with
  | T.Comp c -> (
    match Type.Id.provably_equal c.data.spec.id.tid Wrap.id.tid with
    | Some Type.Equal ->
        equal string "2x" (!(c.on) "x")
    | None ->
        fail "another component" )
  | _ ->
      fail "a component node"

(* A diff with no runtime has no subscription pass: its env records no instance,
   thus it keeps no unmounted instance. *)
let no_passes () =
  let p = mount (rows [1; 2; 3]) in
  patch p (rows [4; 2; 3]) ;
  let env = p.tree.ctx.T.env in
  equal int ~msg:"touched" 0 (List.length env.T.touched) ;
  equal int ~msg:"stale" 0 (List.length env.T.stale)

let tests =
  [ group "diff components"
      [ test "a keyed move of a component moves its DOM node" moved
      ; test "a component in the common start, the middle and the common end"
          middle
      ; test "a component node that becomes an element unmounts" to_element
      ; test "a new key of a component replaces its node" key_change
      ; test "a patch keeps the newest on function" newest_on
      ; test "a diff with no runtime records no instance for the subscriptions"
          no_passes ] ]
