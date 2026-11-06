(* The cancel tokens (design section 12). *)

open Windtrap
open Kilim
module C = Private.Cancel

let order () =
  let b = Buffer.create 16 in
  let add s () = Buffer.add_string b s in
  (b, add)

let cancel_order () =
  let b, add = order () in
  let r = C.root () in
  let a = C.child r in
  let c = C.child r in
  Cancel.on_end r (add "r1 ") ;
  Cancel.on_end r (add "r2 ") ;
  Cancel.on_end a (add "a ") ;
  Cancel.on_end c (add "c ") ;
  C.cancel r ;
  equal string "c a r2 r1 " (Buffer.contents b) ;
  is_true (Cancel.cancelled r && Cancel.cancelled a && Cancel.cancelled c)

let grandchildren () =
  let b, add = order () in
  let r = C.root () in
  let a = C.child r in
  let a1 = C.child a in
  Cancel.on_end a1 (add "a1 ") ;
  Cancel.on_end a (add "a ") ;
  C.cancel r ;
  equal string "a1 a " (Buffer.contents b) ;
  is_true (Cancel.cancelled a1)

let child_of_cancelled () =
  let r = C.root () in
  C.cancel r ;
  let c = C.child r in
  is_true (Cancel.cancelled c) ;
  equal int 0 (C.children r)

let on_end_now () =
  let n = ref 0 in
  let r = C.root () in
  C.cancel r ;
  Cancel.on_end r (fun () -> incr n) ;
  equal int 1 !n ;
  let e = C.root () in
  C.release e ;
  Cancel.on_end e (fun () -> incr n) ;
  equal int 2 !n

let actions_once () =
  let n = ref 0 in
  let r = C.root () in
  Cancel.on_end r (fun () -> incr n) ;
  C.cancel r ;
  C.cancel r ;
  C.release r ;
  equal int 1 !n

let release_then_cancel () =
  let n = ref 0 in
  let r = C.root () in
  let t = C.child r in
  Cancel.on_end t (fun () -> incr n) ;
  C.release t ;
  equal int 1 !n ;
  is_true (not (Cancel.cancelled t)) ;
  C.cancel t ;
  is_true (Cancel.cancelled t) ;
  equal int 1 !n

let release_detaches () =
  let r = C.root () in
  let a = C.child r in
  let b = C.child r in
  let c = C.child r in
  equal int 3 (C.children r) ;
  C.release b ;
  equal int 2 (C.children r) ;
  C.release c ;
  equal int 1 (C.children r) ;
  C.release a ;
  equal int 0 (C.children r) ;
  (* A second release or a cancel of a detached token keeps the list *)
  let d = C.child r in
  let e = C.child r in
  C.release d ;
  C.release d ;
  C.cancel d ;
  equal int 1 (C.children r) ;
  C.cancel e ;
  equal int 0 (C.children r) ;
  is_true (not (Cancel.cancelled r))

let release_keeps_children () =
  let r = C.root () in
  let t = C.child r in
  let k = C.child t in
  C.release t ;
  is_true (not (Cancel.cancelled k)) ;
  C.cancel t ;
  is_true (Cancel.cancelled k)

let release_after_parent_cancel () =
  let r = C.root () in
  let a = C.child r in
  let b = C.child r in
  C.cancel r ;
  C.release a ;
  C.release b ;
  equal int 0 (C.children r)

let cancel_detaches () =
  let r = C.root () in
  let a = C.child r in
  let _b = C.child r in
  C.cancel a ;
  equal int 1 (C.children r) ;
  is_true (not (Cancel.cancelled r))

let raising_action () =
  let errors = ref [] and n = ref 0 in
  Private.set_error_handler (fun e -> errors := e :: !errors) ;
  let r = C.root () in
  Cancel.on_end r (fun () -> incr n) ;
  Cancel.on_end r (fun () -> raise Exit) ;
  Cancel.on_end r (fun () -> incr n) ;
  C.cancel r ;
  equal int 2 !n ;
  equal int 1 (List.length !errors) ;
  let c = C.root () in
  C.cancel c ;
  Cancel.on_end c (fun () -> raise Not_found) ;
  equal int 2 (List.length !errors) ;
  Private.set_error_handler (fun _ -> ())

let deep () =
  let r = C.root () in
  let rec chain t n = if n = 0 then t else chain (C.child t) (n - 1) in
  let leaf = chain r 200_000 in
  C.cancel r ;
  is_true (Cancel.cancelled leaf)

(* A random tree: a model of the parents, the cancelled and the ended flags. *)
type op = Child of int | Release of int | Cancel_op of int | On_end of int

let pp_op ppf = function
  | Child i ->
      Format.fprintf ppf "Child %d" i
  | Release i ->
      Format.fprintf ppf "Release %d" i
  | Cancel_op i ->
      Format.fprintf ppf "Cancel %d" i
  | On_end i ->
      Format.fprintf ppf "On_end %d" i

let ops =
  Gen.list ~size:(Gen.int_range 0 60)
    (Gen.with_pp pp_op
       (Gen.frequency
          [ (4, Gen.map (fun i -> Child i) (Gen.int_range 0 40))
          ; (2, Gen.map (fun i -> Release i) (Gen.int_range 0 40))
          ; (2, Gen.map (fun i -> Cancel_op i) (Gen.int_range 0 40))
          ; (3, Gen.map (fun i -> On_end i) (Gen.int_range 0 40)) ] ) )

type node =
  { tok: Cancel.t
  ; mutable parent: int option
  ; mutable m_cancelled: bool
  ; mutable m_ended: bool }

let tree_law ops =
  let nodes =
    ref [|{tok= C.root (); parent= None; m_cancelled= false; m_ended= false}|]
  in
  let counts = ref [] in
  let index i = i mod Array.length !nodes in
  let rec cancel_model i =
    let n = !nodes.(i) in
    if not n.m_cancelled then begin
      n.m_cancelled <- true ;
      Array.iteri
        (fun j m ->
          if m.parent = Some i then begin
            m.parent <- None ;
            cancel_model j
          end )
        !nodes
    end
  in
  List.iter
    (function
      | Child i ->
          let p = index i in
          let pn = !nodes.(p) in
          let tok = C.child pn.tok in
          let n =
            if pn.m_cancelled then
              {tok; parent= None; m_cancelled= true; m_ended= false}
            else {tok; parent= Some p; m_cancelled= false; m_ended= false}
          in
          nodes := Array.append !nodes [|n|]
      | Release i ->
          let k = index i in
          let n = !nodes.(k) in
          C.release n.tok ;
          n.m_ended <- true ;
          n.parent <- None
      | Cancel_op i ->
          let k = index i in
          C.cancel !nodes.(k).tok ;
          !nodes.(k).parent <- None ;
          cancel_model k
      | On_end i ->
          let k = index i in
          let c = ref 0 in
          Cancel.on_end !nodes.(k).tok (fun () -> incr c) ;
          counts := (k, c) :: !counts )
    ops ;
  Array.iteri
    (fun i n ->
      equal bool n.m_cancelled (Cancel.cancelled n.tok) ;
      let kids =
        Array.fold_left
          (fun acc m -> if m.parent = Some i then acc + 1 else acc)
          0 !nodes
      in
      equal int kids (C.children n.tok) )
    !nodes ;
  List.iter
    (fun (k, c) ->
      let n = !nodes.(k) in
      equal int (if n.m_cancelled || n.m_ended then 1 else 0) !c )
    !counts

let tests =
  [ group "cancel"
      [ test "a cancel runs the children first, the newest first" cancel_order
      ; test "a cancel reaches the grandchildren first" grandchildren
      ; test "a child of a cancelled token is cancelled and in no list"
          child_of_cancelled
      ; test "on_end on a cancelled or ended token runs at once" on_end_now
      ; test "each end action runs one time" actions_once
      ; test "a cancel of an ended token sets the flag and runs no action again"
          release_then_cancel
      ; test "a release detaches the token in O(1)" release_detaches
      ; test "a release keeps the children of the token" release_keeps_children
      ; test "a cancel detaches the token from its parent" cancel_detaches
      ; test "a release after the cancel of the parent leaves no entry"
          release_after_parent_cancel
      ; test "an action that raises does not stop the others" raising_action
      ; test "a cancel of a chain of 200,000 tokens" deep
      ; prop "random trees: flags, lists and end actions agree with a model" ops
          tree_law ] ]
