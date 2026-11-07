(* The todo example. It shows a form with a controlled input, a keyed list, a
   controlled checkbox in each row, and three filters. The state is local: no
   subscription and no task. *)

open Kilim

type filter = All | Active | Completed

type item = {id: int; label: string; done_: bool}

(* [next] is the id of the next item. An id is never used again, thus each row
   keeps its key. *)
type model = {items: item list; draft: string; filter: filter; next: int}

type msg =
  | Edit of string
  | Add
  | Toggle of int * bool
  | Delete of int
  | Show of filter
  | Clear_completed

(* The maximum length of a draft, in bytes. *)
let max_length = 80

let init () = ({items= []; draft= ""; filter= All; next= 1}, Cmd.none)

let update m = function
  | Edit s ->
      (* A longer draft gives back the old model. The input is controlled, thus
         the restore writes the old draft back into the input. *)
      if String.length s > max_length then (m, Cmd.none)
      else ({m with draft= s}, Cmd.none)
  | Add -> (
    match String.trim m.draft with
    | "" ->
        (m, Cmd.none)
    | label ->
        let it = {id= m.next; label; done_= false} in
        ({m with items= m.items @ [it]; draft= ""; next= m.next + 1}, Cmd.none)
    )
  | Toggle (id, d) ->
      let set it = if it.id = id then {it with done_= d} else it in
      ({m with items= List.map set m.items}, Cmd.none)
  | Delete id ->
      ({m with items= List.filter (fun it -> it.id <> id) m.items}, Cmd.none)
  | Show f ->
      ({m with filter= f}, Cmd.none)
  | Clear_completed ->
      ({m with items= List.filter (fun it -> not it.done_) m.items}, Cmd.none)

let visible filter it =
  match filter with
  | All ->
      true
  | Active ->
      not it.done_
  | Completed ->
      it.done_

let row it =
  ( Key.int it.id
  , El.li
      ~at:(if it.done_ then [At.class_ "done"] else [])
      [ El.input
          ~at:
            [ At.type_ "checkbox"
            ; At.checked it.done_
            ; At.aria_label ("Done: " ^ it.label) ]
          ~ev:[Ev.check (fun d -> Toggle (it.id, d))]
          ()
      ; El.span [txt it.label]
      ; El.button
          ~at:[At.aria_label ("Delete " ^ it.label)]
          ~ev:[Ev.click (Delete it.id)]
          [txt "x"] ] )

let filter_button current f label =
  El.button
    ~at:[At.aria "pressed" (if current = f then "true" else "false")]
    ~ev:[Ev.click (Show f)]
    [txt label]

let view m =
  let left = List.length (List.filter (fun it -> not it.done_) m.items) in
  let rows = List.filter (visible m.filter) m.items in
  El.section
    [ El.form
        ~at:[At.aria_label "Add an item"]
        ~ev:[Ev.submit ~prevent_default:true Add]
        [ El.label ~at:[At.for_ "new"] [txt "New item"]
        ; El.input
            ~at:[At.id "new"; At.value m.draft; At.placeholder "What to do?"]
            ~ev:[Ev.input (fun s -> Edit s)]
            ()
        ; El.button ~at:[At.type_ "submit"] [txt "Add"] ]
    ; View.keyed (El.ul ~at:[At.data "testid" "items"]) (List.map row rows)
    ; El.footer
        [ El.span
            ~at:[At.data "testid" "left"]
            [ txt
                ( string_of_int left
                ^ if left = 1 then " item left" else " items left" ) ]
        ; filter_button m.filter All "All"
        ; filter_button m.filter Active "Active"
        ; filter_button m.filter Completed "Completed"
        ; El.button ~ev:[Ev.click Clear_completed] [txt "Clear completed"] ] ]

let app = App.make ~init ~update ~view ()
