(* The intrusive doubly linked list of the instance tree (design section 11,
   "Link before init"). *)

open Windtrap
module D = Kilim.Private.Dlist

let values l =
  let r = ref [] in
  D.iter (fun x -> r := x :: !r) l ;
  List.rev !r

let push_first () =
  let l = D.create () in
  let a = D.push l 1 in
  let b = D.push l 2 in
  equal (list int) [2; 1] (values l) ;
  equal int 2 (D.length l) ;
  D.remove a ;
  equal (list int) ~msg:"the node of 1" [2] (values l) ;
  D.remove b ;
  equal (list int) ~msg:"the node of 2" [] (values l)

(* The list [4; 3; 2; 1] and its nodes [n1 .. n4]. *)
let four () =
  let l = D.create () in
  let ns = List.map (D.push l) [1; 2; 3; 4] in
  (l, Array.of_list ns)

let remove_each () =
  let l, n = four () in
  D.remove n.(3) ;
  equal (list int) ~msg:"the first" [3; 2; 1] (values l) ;
  D.remove n.(1) ;
  equal (list int) ~msg:"a middle node" [3; 1] (values l) ;
  D.remove n.(0) ;
  equal (list int) ~msg:"the last" [3] (values l) ;
  D.remove n.(2) ;
  equal (list int) ~msg:"the only node" [] (values l) ;
  equal int 0 (D.length l) ;
  let x = D.push l 5 in
  equal (list int) ~msg:"a push after the removals" [5] (values l) ;
  D.remove x ;
  equal (list int) [] (values l)

let remove_twice () =
  let l, n = four () in
  D.remove n.(1) ;
  D.remove n.(1) ;
  equal (list int) [4; 3; 1] (values l) ;
  equal int 3 (D.length l)

let iter_remove () =
  let l, n = four () in
  let seen = ref [] in
  D.iter
    (fun x ->
      seen := x :: !seen ;
      D.remove n.(x - 1) )
    l ;
  equal (list int) [4; 3; 2; 1] (List.rev !seen) ;
  equal int 0 (D.length l)

(* Each call removes the next node too: drain gets each value one time. A value
   that [f] pushes is drained too. *)
let drain () =
  let l, n = four () in
  let seen = ref [] in
  D.drain
    (fun k x ->
      seen := x :: !seen ;
      if x = 4 then D.remove n.(2) ;
      if x = 2 then ignore (D.push l (k + x)) )
    10 l ;
  equal (list int) [4; 2; 12; 1] (List.rev !seen) ;
  equal int 0 (D.length l) ;
  D.drain (fun () _ -> fail "an empty list") () l

(* A random sequence of push and remove against a list model. *)
type op = Push | Remove of int

let pp_op ppf = function
  | Push ->
      Format.pp_print_string ppf "Push"
  | Remove i ->
      Format.fprintf ppf "Remove %d" i

let ops =
  Gen.list ~size:(Gen.int_range 0 60)
    (Gen.with_pp pp_op
       (Gen.frequency
          [ (3, Gen.constant Push)
          ; (2, Gen.map (fun i -> Remove i) (Gen.int_range 0 40)) ] ) )

let model_law ops =
  let l = D.create () in
  let nodes = ref [] and model = ref [] and next = ref 0 in
  List.iter
    (function
      | Push ->
          nodes := !nodes @ [(!next, D.push l !next)] ;
          model := !next :: !model ;
          incr next
      | Remove i ->
          if !nodes <> [] then begin
            let k, n = List.nth !nodes (i mod List.length !nodes) in
            D.remove n ;
            model := List.filter (fun x -> x <> k) !model
          end )
    ops ;
  equal (list int) !model (values l) ;
  equal int (List.length !model) (D.length l)

let tests =
  [ group "dlist"
      [ test "push puts the value first" push_first
      ; test "remove unlinks the first, a middle and the last node" remove_each
      ; test "a second remove does nothing" remove_twice
      ; test "iter permits the remove of the current node" iter_remove
      ; test "drain removes each node, also the nodes that f removes" drain
      ; prop "a random sequence of push and remove agrees with a list model" ops
          model_law ] ]
