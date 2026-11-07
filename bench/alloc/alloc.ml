(* The allocation budget (docs/budgets.md): the words that the diff allocates in
   the four actions of the DOM operation budget (test/view/test_budget.ml),
   against the ceilings of the file budget.

   The DOM operations do nothing and allocate nothing: the count is the count of
   kilim, not of a fake DOM. The test makes the new view before the first read
   of the counters, thus the count is the patch only, not the view code.

   A ceiling is the measured count plus 5 %, plus 10 words, rounded up to a
   multiple of 10. Each line of the output gives the count, its ceiling, and the
   new ceiling of the count. The count finds regressions only: it has no target.

   The count depends on the compiler. The first line of the budget file is the
   version of OCaml of the counts, and another version is a failure. The dune
   file runs the test only in the dev profile, and with no flambda. *)

open Kilim
module Diff = Private.Diff

(* The DOM operations on the node [()]. *)
let null : unit Private.Dom.t =
  { create_element= (fun _ _ -> ())
  ; create_text= (fun _ -> ())
  ; set_text= (fun _ _ -> ())
  ; set_attribute= (fun _ _ _ -> ())
  ; remove_attribute= (fun _ _ -> ())
  ; set_property= (fun _ _ -> ())
  ; get_value= (fun _ -> "")
  ; get_checked= (fun _ -> false)
  ; insert_before= (fun _ _ _ -> ())
  ; remove= (fun _ _ -> ())
  ; replace= (fun _ _ _ -> ())
  ; clear= (fun _ -> ())
  ; set_inner_html= (fun _ _ -> ())
  ; attach= (fun _ _ -> ())
  ; listen= (fun _ _ ~passive:_ -> ())
  ; unlisten= (fun _ _ -> ()) }

(* All the allocated words: the minor heap, and the major heap with no promoted
   words. A large array goes directly to the major heap. *)
let words () =
  let minor, promoted, major = Gc.counters () in
  minor +. major -. promoted

let row k label = (Key.int k, El.li ~at:[At.class_ "row"] [txt label])

let table rows = View.keyed El.ul rows

let rows1000 = List.init 1000 (fun i -> row i ("row " ^ string_of_int i))

let swapped =
  let a = Array.of_list rows1000 in
  let t = a.(1) in
  a.(1) <- a.(998) ;
  a.(998) <- t ;
  Array.to_list a

let changed = List.mapi (fun i r -> if i = 500 then row 500 "x" else r) rows1000

let actions =
  [ ("create", [], rows1000)
  ; ("change", rows1000, changed)
  ; ("swap", rows1000, swapped)
  ; ("remove", rows1000, []) ]

let measure (before, after) =
  let tree = Diff.mount null ~parent:() ~dispatch:ignore (table before) in
  let view = table after in
  let w0 = words () in
  Diff.patch tree view ;
  let w1 = words () in
  int_of_float (w1 -. w0)

(* The ceiling of a count: plus 5 %, plus 10 words, rounded up to a multiple of
   10. *)
let ceiling n = ((105 * n) + 1000 + 999) / 1000 * 10

let read_budget file =
  let ic = open_in file in
  let lines = In_channel.input_lines ic in
  close_in ic ;
  let fields l = String.split_on_char ' ' l |> List.filter (( <> ) "") in
  match List.map fields lines with
  | ["ocaml"; version] :: rest ->
      ( version
      , List.map
          (function
            | [action; n] ->
                (action, int_of_string n)
            | l ->
                failwith ("budget: " ^ String.concat " " l) )
          rest )
  | _ ->
      failwith "budget: the first line must be: ocaml VERSION"

let () =
  let version, ceilings = read_budget Sys.argv.(1) in
  let failures = ref 0 in
  if version <> Sys.ocaml_version then (
    incr failures ;
    print_endline
      ( "FAIL the counts are for OCaml " ^ version ^ ", this is OCaml "
      ^ Sys.ocaml_version
      ^ ": write this version and the new ceilings of this run" ) ) ;
  List.iter
    (fun (name, before, after) ->
      let n = measure (before, after) in
      match List.assoc_opt name ceilings with
      | None ->
          incr failures ;
          Printf.printf "FAIL %s: %d words, no ceiling (new ceiling %d)\n" name
            n (ceiling n)
      | Some c ->
          if n > c then incr failures ;
          Printf.printf "%s %s: %d words (ceiling %d, new ceiling %d)\n"
            (if n > c then "FAIL" else "ok  ")
            name n c (ceiling n) )
    actions ;
  List.iter
    (fun (name, _) ->
      if not (List.exists (fun (a, _, _) -> a = name) actions) then (
        incr failures ;
        Printf.printf "FAIL %s: a ceiling with no action\n" name ) )
    ceilings ;
  exit (if !failures = 0 then 0 else 1)
