(* Property tests of the diff (design section 14): a patch gives the DOM of a
   fresh mount, keeps the nodes it must keep, makes the least moves, raises for
   a duplicate key before any operation, and keeps the retained tree in the DOM
   when an operation raises. *)

open Windtrap
open Kilim
open Support

(* The first element of each key. *)
let dedupe key l =
  let rec go seen = function
    | [] ->
        []
    | x :: rest ->
        let k = key x in
        if List.mem k seen then go seen rest else x :: go (k :: seen) rest
  in
  go [] l

let gen_at =
  Gen.map
    (fun l ->
      List.map
        (fun (n, v) -> At.v n v)
        (dedupe (fun (n, _) -> String.lowercase_ascii n) l) )
    (Gen.list ~size:(Gen.int_range 0 3)
       (Gen.pair
          (Gen.of_list
             ["class"; "id"; "title"; "type"; "data-k"; "viewBox"; "viewbox"] )
          (Gen.of_list ["x"; "y"]) ) )

let is_type (a : V.at) =
  match a with V.Attr a -> String.equal a.key "type" | _ -> false

let ev (i, b) =
  match i with
  | 0 ->
      Ev.click ~stop:b "c"
  | 1 ->
      Ev.input (fun s -> s)
  | 2 ->
      Ev.mouseenter ~prevent_default:b "m"
  | 3 ->
      Ev.scroll ~prevent_default:b "s"
  | _ ->
      Ev.keydown (fun k -> k)

let gen_ev =
  Gen.map
    (fun l -> List.map ev (dedupe fst l))
    (Gen.list ~size:(Gen.int_range 0 2) (Gen.pair (Gen.int_range 0 4) Gen.bool))

let names =
  [ "div"
  ; "span"
  ; "p"
  ; "svg"
  ; "circle"
  ; "foreignObject"
  ; "input"
  ; "select"
  ; "form" ]

let gen_text = Gen.map txt (Gen.of_list ["a"; "b"; "c"])

let rec gen_view d =
  if d = 0 then gen_text else Gen.frequency [(1, gen_text); (3, gen_element d)]

and gen_element d =
  let open Gen in
  let* name = of_list names in
  let* at = gen_at in
  let* ev = gen_ev in
  if String.equal name "input" then
    let* ty =
      of_list
        [ []
        ; [At.type_ "range"]
        ; [At.type_ "text"]
        ; [At.type_ "radio"]
        ; [At.type_ "checkbox"] ]
    in
    let* name = of_list [[]; [At.name "g"]; [At.name "h"]] in
    let* lo = of_list [[]; [At.v "min" "60"]] in
    let* hi = of_list [[]; [At.v "max" "40"]; [At.v "max" "100"]] in
    let* v = of_list [""; "u"; "50"; "70"] in
    let* c = bool in
    let at =
      if ty = [] then at else List.filter (fun a -> not (is_type a)) at
    in
    constant
      (El.input
         ~at:((At.value v :: At.checked c :: ty) @ name @ lo @ hi @ at)
         ~ev () )
  else if String.equal name "select" then gen_select at ev
  else
    let el = El.v name ~at ~ev in
    let* kind = int_range 0 5 in
    match kind with
    | 0 ->
        map
          (fun h -> View.unsafe_html ~el (Unsafe_html.of_string h))
          (of_list ["<b>x</b>"; "<i>y</i>"])
    | 1 | 2 ->
        map
          (fun l ->
            View.keyed el
              (List.map (fun (k, v) -> (Key.int k, v)) (dedupe fst l)) )
          (list ~size:(int_range 0 8) (pair (int_range 0 9) (gen_view (d - 1))))
    | _ ->
        map (El.v name ~at ~ev) (list ~size:(int_range 0 4) (gen_view (d - 1)))

(* A select with options, in its children or in an optgroup. *)
and gen_select at ev =
  let open Gen in
  let* v = of_list ["a"; "b"; "c"] in
  let at = if at = [] then [At.value "b"] else At.value v :: at in
  let el = El.select ~at ~ev in
  let options = list ~size:(int_range 0 4) gen_option in
  let* kind = int_range 0 2 in
  match kind with
  | 0 ->
      map
        (fun l ->
          View.keyed el (List.map (fun (k, o) -> (Key.int k, o)) (dedupe fst l)) )
        (list ~size:(int_range 0 4) (pair (int_range 0 5) gen_option))
  | 1 ->
      map (fun (a, b) -> el (a @ [El.optgroup b])) (pair options options)
  | _ ->
      map el options

and gen_option =
  let open Gen in
  let* value = of_list [[]; [At.v "value" "a"]; [At.v "value" "b"]] in
  let* t = of_list ["a"; "b"; "c"] in
  constant (El.option ~at:value [txt t])

(* The root is an element, usually with some children. *)
let view =
  Gen.with_pp pp_view
  @@ Gen.frequency
       [ (1, gen_element 3)
       ; ( 3
         , Gen.map
             (fun (keyed, l) ->
               if keyed then
                 View.keyed El.div
                   (List.map (fun (k, v) -> (Key.int k, v)) (dedupe fst l))
               else El.div (List.map snd l) )
             (Gen.pair Gen.bool
                (Gen.list ~size:(Gen.int_range 1 8)
                   (Gen.pair (Gen.int_range 0 9) (gen_view 3)) ) ) ) ]

(* A select and an input with a fixed view value: only the options and the
   attributes change. *)
let form =
  let open Gen in
  let input =
    let* ty = of_list [[]; [At.type_ "range"]; [At.type_ "text"]] in
    let* lo = of_list [[]; [At.v "min" "60"]] in
    let* hi = of_list [[]; [At.v "max" "40"]; [At.v "max" "100"]] in
    constant (El.input ~at:((At.value "50" :: ty) @ lo @ hi) ())
  in
  Gen.with_pp pp_view
    (map (fun (s, i) -> El.div [s; i]) (pair (gen_select [] []) input))

(* Radio buttons and checkboxes in forms and in plain and keyed lists. A group
   can have two checked buttons: then the last button in the document is
   checked, after a patch and after a mount. *)
let radios =
  let open Gen in
  let input =
    let* ty = of_list ["radio"; "radio"; "checkbox"] in
    let* name = of_list ["g"; "h"; ""] in
    let* c = bool in
    constant (El.input ~at:[At.type_ ty; At.name name; At.checked c] ())
  in
  let group =
    let* wrap = of_list ["div"; "form"] in
    let* keyed = bool in
    let* l = list ~size:(int_range 0 4) (pair (int_range 0 5) input) in
    if keyed then
      constant
        (View.keyed (El.v wrap ~at:[])
           (List.map (fun (k, v) -> (Key.int k, v)) (dedupe fst l)) )
    else constant (El.v wrap (List.map snd l))
  in
  Gen.with_pp pp_view (map El.div (list ~size:(int_range 1 3) group))

(* The DOM nodes before a patch. *)
type shadow = Sh of F.node * shadow list

let rec capture n = Sh (n, List.map capture (F.children n))

let index k l =
  let rec go i = function
    | [] ->
        None
    | (k', _) :: rest ->
        if Private.Key.equal k k' then Some i else go (i + 1) rest
  in
  go 0 l

(* A text node, or an element with the same name, at the same place or with the
   same key, keeps its DOM node. *)
let rec identity (a : string View.t) (b : string View.t) (Sh (n, kids)) n' =
  match (a, b) with
  | V.Text _, V.Text _ ->
      is_true ~msg:"a text node stays" (n == n')
  | V.Element ea, V.Element eb when String.equal ea.name eb.name -> (
      is_true ~msg:"an element stays" (n == n') ;
      let kids' = F.children n' in
      match (ea.children, eb.children) with
      | V.Plain la, V.Plain lb ->
          List.iteri
            (fun i vb ->
              if i < List.length la then
                identity (List.nth la i) vb (List.nth kids i) (List.nth kids' i) )
            lb
      | V.Keyed la, V.Keyed lb ->
          List.iteri
            (fun j (k, vb) ->
              match index k la with
              | Some i ->
                  identity
                    (snd (List.nth la i))
                    vb (List.nth kids i) (List.nth kids' j)
              | None ->
                  () )
            lb
      | _ ->
          () )
  | _ ->
      ()

let step p a b =
  let sh = capture (node p) in
  patch p b ;
  equal string ~msg:"a patch gives the DOM of a fresh mount" (fresh b) (dump p) ;
  check_tree p ;
  identity a b sh (node p)

let rec size (v : string View.t) =
  match v with
  | V.Text _ ->
      1
  | V.Element e -> (
      1
      +
      match e.children with
      | V.Plain l ->
          List.fold_left (fun n v -> n + size v) 0 l
      | V.Keyed l ->
          List.fold_left (fun n (_, v) -> n + size v) 0 l
      | V.Inner _ ->
          1 )

let patch_law (a, b) =
  collect
    ( if size a < 5 then "size < 5"
      else if size a < 20 then "size < 20"
      else "size >= 20" ) ;
  step (mount a) a b

let chain_law = function
  | [] ->
      ()
  | v :: rest ->
      let p = mount v in
      ignore (List.fold_left (fun a b -> step p a b ; b) v rest)

let keys =
  Gen.map (dedupe Fun.id)
    (Gen.list ~size:(Gen.int_range 0 20) (Gen.int_range 0 25))

(* The length of a longest strictly increasing subsequence, O(n^2). *)
let lis_length l =
  let a = Array.of_list l in
  let n = Array.length a in
  let best = Array.make n 1 in
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      if a.(j) < a.(i) && best.(j) + 1 > best.(i) then best.(i) <- best.(j) + 1
    done
  done ;
  Array.fold_left max 0 best

let position k l =
  let rec go i = function
    | [] ->
        -1
    | x :: r ->
        if x = k then i else go (i + 1) r
  in
  go 0 l

(* The nodes that do not move are an increasing subsequence of the old order,
   thus the least number of moves is the number of kept keys minus the length of
   a longest increasing subsequence. Each new row costs 5 operations and each
   removed row 1. *)
let moves_law ?key (a, b) =
  let p = mount (ul ?key a) in
  patch p (ul ?key b) ;
  equal (list string) (List.map string_of_int b) (texts p) ;
  let kept = List.filter (fun k -> List.mem k a) b in
  let moves =
    List.length kept - lis_length (List.map (fun k -> position k a) kept)
  in
  equal int ~msg:"moves" moves (F.moves p.doc) ;
  let added = List.length b - List.length kept in
  let removed = List.length a - List.length kept in
  let total =
    match (a, b) with
    | [], [] ->
        0
    | _, [] ->
        1
    | _ ->
        (5 * added) + removed + moves
  in
  equal int ~msg:"total" total (F.total p.doc) ;
  check_tree p

(* 60 keys that start at slot 0 of each table of 128 slots or fewer: a new
   middle of more than 32 keys goes to the map. *)
let clash = colliding 60 7

let clash_keys =
  Gen.map (dedupe Fun.id)
    (Gen.list ~size:(Gen.int_range 0 60) (Gen.int_range 0 59))

let small = Gen.list ~size:(Gen.int_range 0 6) (Gen.int_range 0 3)

let titled l =
  View.keyed (El.ul ~at:[At.title (string_of_int (List.length l))]) (rows l)

let duplicate_law (a, b) =
  let a = dedupe Fun.id a in
  let p = mount (titled a) in
  let before = dump p and kids_before = retained_kids p in
  if List.length (dedupe Fun.id b) <> List.length b then begin
    F.reset p.doc ;
    raises_match (Exn.invalid_arg ~substring:"duplicate") (fun () ->
        Diff.patch p.tree (titled b) ) ;
    equal int ~msg:"no operation" 0 (F.total p.doc) ;
    equal string ~msg:"the same DOM" before (dump p) ;
    is_true ~msg:"the same retained tree"
      (same_nodes kids_before (retained_kids p)) ;
    check_tree p
  end ;
  let b = dedupe Fun.id b in
  patch p (titled b) ;
  equal string (fresh (titled b)) (dump p) ;
  check_tree p

(* Each operation of the patch in turn raises. With [~again], a new patch
   follows the fault, and [again] checks the page. The diff does not keep the
   attributes and the listeners of a failed patch exact, thus [~again] is for
   the keyed lists only. After the fault, the order of the retained children can
   be different from the DOM, thus the new patch gives the right nodes, but not
   always in the right order. *)
let fault_law ?again (a, b) =
  let p = mount a in
  patch p b ;
  for k = 1 to F.total p.doc do
    let p = mount a in
    F.reset p.doc ;
    F.fail_at p.doc k ;
    ( match Diff.patch p.tree b with
    | () ->
        fail "no fault"
    | exception F.Fault ->
        () ) ;
    check_tree ~order:false p ;
    Option.iter
      (fun again -> Diff.patch p.tree b ; check_tree ~order:false p ; again p)
      again
  done

(* The retained keys stay with their nodes: a new patch gives each row one
   time. *)
let keyed_fault_law (a, b) =
  fault_law
    ~again:(fun p ->
      equal (list string)
        (List.sort compare (List.map string_of_int b))
        (List.sort compare (texts p)) )
    (ul a, ul b)

let tests =
  [ group "diff laws"
      [ prop ~count:300
          "a patch gives the DOM of a fresh mount and keeps the nodes"
          (Gen.pair view view) patch_law
      ; prop ~count:300 "a patch of the options or of the range of a value"
          (Gen.pair form form) patch_law
      ; prop ~count:300 "a patch of radio buttons in groups"
          (Gen.pair radios radios) patch_law
      ; prop "chains of patches"
          (Gen.list ~size:(Gen.int_range 2 5) view)
          chain_law
      ; prop "keyed lists: the least moves and O(k) operations"
          (Gen.pair keys keys) (moves_law ?key:None)
      ; prop "keyed lists with colliding keys: the table and the map"
          (Gen.pair clash_keys clash_keys)
          (moves_law ~key:(fun i -> Key.int clash.(i)))
      ; prop "a duplicate key: no operation, the same tree, then a valid patch"
          (Gen.pair small small) duplicate_law
      ; prop "a fault at each operation leaves the retained tree in the DOM"
          (Gen.pair view view) fault_law
      ; prop "a fault in a keyed patch leaves the retained tree in the DOM"
          (Gen.pair keys keys) keyed_fault_law ] ]
