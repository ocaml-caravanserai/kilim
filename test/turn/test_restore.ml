(* The restore of the controlled properties after a discrete event (design
   section 11), and the events of Page. *)

open Windtrap
open Kilim
open Support

exception Boom

(* A text input whose value is the model. [accept] decides if a value replaces
   the model. With [steps], "a" starts a task of 300 synchronous steps. *)
type tmsg = Typed of string | Done

let rec steps n =
  if n = 0 then Async.return Done
  else Async.bind (sync ()) (fun () -> steps (n - 1))

let text_app ?(accept = fun _ -> true) ?(steps_on = "") () =
  App.make
    ~init:(fun () -> ("", Cmd.none))
    ~update:(fun m -> function
      | Typed s ->
          if accept s then
            ( s
            , if String.equal s steps_on then Cmd.task (steps 300) else Cmd.none
            )
          else (m, Cmd.none)
      | Done ->
          (m, Cmd.none) )
    ~view:(fun m ->
      El.div
        [ El.input
            ~at:[At.data "testid" "i"; At.value m]
            ~ev:[Ev.input (fun s -> Typed s)]
            ()
        ; txt m ] )
    ()

let field = By.test_id "i"

let live p = F.value (P.find p field)

(* A yield with no restore, with a cost of 20 ms. The turn of "a" processes
   Typed "a" (at 20 ms), drains 256 continuations (clock reads at 40 and 60 ms)
   and stops. The restore waits. "b" then comes before the continuation: the
   turn drains the last 44 continuations (80 ms), processes Typed "ab" (100 ms)
   and Done (120 ms), and renders. Its two restores find the live value of the
   view. If the first turn restored, the model would be "b". *)
let yield_no_restore () =
  let h = H.create ~cost:20. () in
  let p = P.start ~host:h (text_app ~steps_on:"a" ()) in
  P.type_ p field "a" ;
  equal int 1 (stats p).stops ;
  equal float_exact 60. (H.now h) ;
  equal int 1 (stats p).renders ;
  equal int 0 (stats p).restores ;
  equal int 1 (ins p).restores_left ;
  equal int 1 (ins p).continuations_left ;
  equal string "a" (live p) ;
  F.reset (P.doc p) ;
  P.type_ p field "b" ;
  equal string "ab" (live p) ;
  equal float_exact 120. (H.now h) ;
  equal int 2 (stats p).renders ;
  equal int 2 (stats p).restores ;
  equal int 0 (ins p).restores_left ;
  equal int 0 (ins p).continuations_left ;
  is_false (List.mem_assoc "set_property" (F.counts (P.doc p))) ;
  equal string "ab" (F.text (P.root p)) ;
  quiet p ;
  no_errors p ;
  (* With no second key: the continuation runs the rest, and the frame renders
     and restores. *)
  let h = H.create ~cost:20. () in
  let p = P.start ~host:h (text_app ~steps_on:"a" ()) in
  P.type_ p field "a" ;
  P.run p ;
  P.advance p (Span.ms 20) ;
  equal int 2 (stats p).renders ;
  equal int 1 (stats p).restores ;
  equal string "a" (live p) ;
  equal string "a" (F.text (P.root p)) ;
  quiet p ;
  no_errors p

(* A rejected input: the turn has no dirty instance, and its render step only
   restores. *)
let rejected () =
  let p = P.start (text_app ~accept:(fun _ -> false) ()) in
  F.reset (P.doc p) ;
  P.input p field "x" ;
  equal string "" (live p) ;
  equal int 1 (stats p).restores ;
  equal int 1 (stats p).renders ;
  equal int 1 (stats p).max_renders ;
  equal
    (list (pair string int))
    [("get_value", 1); ("set_property", 1)]
    (F.counts (P.doc p)) ;
  quiet p ;
  no_errors p

(* A controlled checkbox: a click toggles it first, and a rejected change
   unchecks it again. *)
type cmsg = Checked of bool | Pick of string

let checkbox ~accept =
  App.make
    ~init:(fun () -> (false, Cmd.none))
    ~update:(fun m -> function
      | Checked b ->
          ((if accept then b else m), Cmd.none)
      | Pick _ ->
          (m, Cmd.none) )
    ~view:(fun m ->
      El.input
        ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked m]
        ~ev:[Ev.check (fun b -> Checked b)]
        () )
    ()

let check_box () =
  let p = P.start (checkbox ~accept:false) in
  P.click p (By.test_id "c") ;
  is_false (F.checked (P.find p (By.test_id "c"))) ;
  equal int 1 (stats p).restores ;
  let p = P.start (checkbox ~accept:true) in
  F.reset (P.doc p) ;
  P.click p (By.test_id "c") ;
  is_true (F.checked (P.find p (By.test_id "c"))) ;
  equal int 2 (stats p).renders ;
  is_false (List.mem_assoc "set_property" (F.counts (P.doc p))) ;
  P.click p (By.test_id "c") ;
  is_false (F.checked (P.find p (By.test_id "c"))) ;
  no_errors p

(* Only an HTML input with the type checkbox toggles: a button and an SVG
   element with this type do not. *)
let no_toggle () =
  let got = ref [] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () b ->
           got := b :: !got ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             [ El.button
                 ~at:[At.data "testid" "b"; At.type_ "checkbox"]
                 ~ev:[Ev.check Fun.id]
                 []
             ; El.svg
                 [ El.input
                     ~at:[At.data "testid" "s"; At.type_ "checkbox"]
                     ~ev:[Ev.check Fun.id]
                     () ] ] )
         () )
  in
  P.click p (By.test_id "b") ;
  P.click p (By.test_id "s") ;
  equal (list bool) [false; false] !got ;
  is_false (F.checked (P.find p (By.test_id "b"))) ;
  is_false (F.checked (P.find p (By.test_id "s"))) ;
  no_errors p

(* A checkbox with the name of a radio group is not in the group: its click does
   not uncheck the radio button. *)
let not_in_group () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (false, Cmd.none))
         ~update:(fun _ b -> (b, Cmd.none))
         ~view:(fun b ->
           El.div
             [ El.input
                 ~at:
                   [ At.data "testid" "r"
                   ; At.type_ "radio"
                   ; At.name "g"
                   ; At.checked true ]
                 ()
             ; El.input
                 ~at:
                   [ At.data "testid" "c"
                   ; At.type_ "checkbox"
                   ; At.name "g"
                   ; At.checked b ]
                 ~ev:[Ev.check Fun.id]
                 () ] )
         () )
  in
  equal int 0 (List.length (F.group (P.find p (By.test_id "c")))) ;
  F.reset (P.doc p) ;
  P.click p (By.test_id "c") ;
  is_true (F.checked (P.find p (By.test_id "r"))) ;
  is_true (F.checked (P.find p (By.test_id "c"))) ;
  is_false (List.mem_assoc "set_property" (F.counts (P.doc p))) ;
  no_errors p

(* A handler that prevents the default undoes the change of a click, as a
   browser does after the dispatch: the checkbox toggles back, and the radio
   button that was checked before becomes checked again. *)
let prevented () =
  let got = ref [] in
  let box v =
    El.input
      ~at:[At.data "testid" v; At.type_ "checkbox"]
      ~ev:[Ev.check ~prevent_default:true (fun b -> (v, b))]
      ()
  in
  let radio v =
    El.input
      ~at:[At.data "testid" v; At.type_ "radio"; At.name "g"]
      ~ev:[Ev.check ~prevent_default:true (fun b -> (v, b))]
      ()
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           got := m :: !got ;
           ((), Cmd.none) )
         ~view:(fun () -> El.div [box "c"; radio "a"; radio "b"; radio "d"])
         () )
  in
  let checked v = F.checked (P.find p (By.test_id v)) in
  P.click p (By.test_id "c") ;
  is_false (checked "c") ;
  (* No button of the group is checked: the click checks none. *)
  P.click p (By.test_id "a") ;
  is_false (checked "a") ;
  F.browser_set (P.find p (By.test_id "b")) (Checked true) ;
  P.click p (By.test_id "d") ;
  is_true (checked "b") ;
  is_false (checked "d") ;
  is_false (checked "a") ;
  (* A click on the checked button changes nothing. *)
  P.click p (By.test_id "b") ;
  is_true (checked "b") ;
  equal
    (list (pair string bool))
    [("b", true); ("d", true); ("a", true); ("c", true)]
    !got ;
  no_errors p

(* A radio group: a rejected click on b checks a again, and unchecks b. *)
let radios ~accept =
  App.make
    ~init:(fun () -> ("a", Cmd.none))
    ~update:(fun m -> function
      | Pick s ->
          ((if accept then s else m), Cmd.none)
      | Checked _ ->
          (m, Cmd.none) )
    ~view:(fun m ->
      let radio v =
        El.input
          ~at:
            [ At.data "testid" v
            ; At.type_ "radio"
            ; At.name "g"
            ; At.checked (String.equal m v) ]
          ~ev:[Ev.check (fun _ -> Pick v)]
          ()
      in
      El.div [radio "a"; radio "b"] )
    ()

let radio_group () =
  let checked p v = F.checked (P.find p (By.test_id v)) in
  let p = P.start (radios ~accept:false) in
  is_true (checked p "a") ;
  P.click p (By.test_id "b") ;
  is_true (checked p "a") ;
  is_false (checked p "b") ;
  let p = P.start (radios ~accept:true) in
  F.reset (P.doc p) ;
  P.click p (By.test_id "b") ;
  is_false (checked p "a") ;
  is_true (checked p "b") ;
  is_false (List.mem_assoc "set_property" (F.counts (P.doc p))) ;
  no_errors p

(* A focus is not discrete: no restore. *)
type fmsg = Focused | Typed_f of string | Hide

let focus_app ?(page = ref None) () =
  App.make
    ~init:(fun () -> (true, Cmd.none))
    ~update:(fun m -> function
      | Focused ->
          (* Two discrete events inside the Macro turn, rejected. *)
          Option.iter
            (fun p ->
              P.input p (By.test_id "x") "q" ;
              P.input p (By.test_id "y") "q" )
            !page ;
          (m, Cmd.none)
      | Typed_f _ ->
          (m, Cmd.none)
      | Hide ->
          (false, Cmd.none) )
    ~view:(fun shown ->
      El.div
        ( [ El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus Focused] ()
          ; El.input
              ~at:[At.data "testid" "x"; At.value ""]
              ~ev:[Ev.input (fun s -> Typed_f s)]
              ()
          ; El.input
              ~at:[At.data "testid" "y"; At.value ""]
              ~ev:[Ev.input (fun s -> Typed_f s)]
              () ]
        @
        if shown then
          [ El.input
              ~at:[At.data "testid" "h"; At.value "v"]
              ~ev:[Ev.click Hide]
              () ]
        else [] ) )
    ()

let no_restore_focus () =
  let p = P.start (focus_app ()) in
  P.focus p (By.test_id "f") ;
  equal int 0 (stats p).restores ;
  equal int 0 (ins p).restores_left ;
  quiet p ;
  no_errors p

(* Two rejected inputs inside a Macro turn: the restores wait, and the render of
   the frame runs them, with no model change. *)
let nested_restore () =
  let page = ref None in
  let p = P.start (focus_app ~page ()) in
  page := Some p ;
  let value p v = F.value (P.find p (By.test_id v)) in
  P.focus p (By.test_id "f") ;
  equal string "q" (value p "x") ;
  equal int 2 (ins p).restores_left ;
  is_true (ins p).pending ;
  quiet p ;
  equal string "" (value p "x") ;
  equal string "" (value p "y") ;
  equal int 2 (stats p).restores ;
  equal int ~msg:"no view" 1 (stats p).renders ;
  no_errors p ;
  (* The first restore raises (a DOM fault): the error goes to the error
     handler, and the second restore still runs. *)
  let page = ref None in
  let p = P.start (focus_app ~page ()) in
  page := Some p ;
  P.focus p (By.test_id "f") ;
  F.reset (P.doc p) ;
  F.fail_at (P.doc p) 1 ;
  P.advance p (Span.ms 16) ;
  equal string "q" (value p "x") ;
  equal string "" (value p "y") ;
  equal int 2 (stats p).restores ;
  is_true (match errors p with [F.Fault] -> true | _ -> false) ;
  settled p ;
  quiet p

(* The click removes its own input: the restore does nothing for it. *)
let removed_target () =
  let p = P.start (focus_app ()) in
  let h = P.find p (By.test_id "h") in
  F.reset (P.doc p) ;
  P.click p (By.node h) ;
  equal int 1 (stats p).restores ;
  (* The patch reads the live values of the two other inputs. *)
  equal
    (list (pair string int))
    [("get_value", 2); ("remove", 1)]
    (F.counts (P.doc p)) ;
  quiet p ;
  no_errors p

(* The click removes the parent of its input: the input is not under the mount
   element, and the restore does nothing for it. *)
type wmsg = Unwrap

let removed_parent () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (true, Cmd.none))
         ~update:(fun _ Unwrap -> (false, Cmd.none))
         ~view:(fun wrapped ->
           El.div
             ( if wrapped then
                 [ El.span
                     [ El.input
                         ~at:[At.data "testid" "w"; At.value "v"]
                         ~ev:[Ev.click Unwrap]
                         () ] ]
               else [] ) )
         () )
  in
  F.reset (P.doc p) ;
  P.click p (By.test_id "w") ;
  equal int 1 (stats p).restores ;
  (* The patch clears the div: no operation on the input. *)
  equal (list (pair string int)) [("clear", 1)] (F.counts (P.doc p)) ;
  no_errors p

(* After a stop, no restore runs and none waits. *)
let stop_clears () =
  let h = H.create ~cost:20. () in
  let p = P.start ~host:h (text_app ~steps_on:"a" ()) in
  let n = P.find p field in
  P.type_ p field "a" ;
  equal int 1 (ins p).restores_left ;
  F.reset (P.doc p) ;
  P.stop p ;
  equal int 0 (ins p).restores_left ;
  P.input p (By.node n) "z" ;
  equal int 0 (ins p).restores_left ;
  quiet p ;
  equal int 0 (stats p).restores ;
  equal (list (pair string int)) [("remove", 1)] (F.counts (P.doc p)) ;
  no_errors p

(* The update of an input raises: the render writes the fallback, and the
   restore of the removed input does nothing. *)
let after_catch () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> ("", Cmd.none))
         ~update:(fun _ _ -> raise Boom)
         ~view:(fun m ->
           El.input
             ~at:[At.data "testid" "i"; At.value m]
             ~ev:[Ev.input Fun.id]
             () )
         ~fallback:(fun _ -> txt "fallback")
         () )
  in
  P.input p field "x" ;
  equal string "<div>fallback</div>" (P.html p) ;
  equal int 1 (stats p).restores ;
  equal int 0 (ins p).restores_left ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  quiet p

(* change, submit and keydown each give one discrete turn that renders at once,
   with the payload of the target. type_ gives one turn for each character. *)
type emsg = Changed of string | Submitted | Key of string | Input of string

let events () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> ([], Cmd.none))
         ~update:(fun log m ->
           ( ( match m with
             | Changed s ->
                 "change " ^ s
             | Submitted ->
                 "submit"
             | Key k ->
                 "key " ^ k
             | Input s ->
                 "input " ^ s )
             :: log
           , Cmd.none ) )
         ~view:(fun log ->
           El.form
             ~at:[At.data "testid" "form"]
             ~ev:[Ev.submit Submitted]
             [ El.input
                 ~at:[At.data "testid" "i"]
                 ~ev:
                   [ Ev.change (fun s -> Changed s)
                   ; Ev.keydown (fun k -> Key k)
                   ; Ev.input (fun s -> Input s) ]
                 ()
             ; txt (string_of_int (List.length log)) ] )
         () )
  in
  P.change p field "c" ;
  equal int 2 (stats p).renders ;
  P.submit p (By.test_id "i") ;
  equal int 3 (stats p).renders ;
  P.keydown p field "Enter" ;
  equal int 4 (stats p).renders ;
  equal int 3 (stats p).restores ;
  let turns = (stats p).turns in
  P.type_ p field "ab" ;
  equal int (turns + 2) (stats p).turns ;
  equal int 6 (stats p).renders ;
  equal string "cab" (live p) ;
  equal string "5" (F.text (P.root p)) ;
  equal int 0 (H.frames (P.host p)) ;
  no_errors p

let type_text () =
  let p = P.start (text_app ()) in
  let turns = (stats p).turns in
  P.type_ p field "abc" ;
  equal int (turns + 3) (stats p).turns ;
  equal string "abc" (F.text (P.root p)) ;
  equal string "abc" (live p) ;
  equal int 3 (stats p).restores ;
  quiet p ;
  no_errors p

(* Two controlled inputs and a checkbox on random schedules. An input accepts a
   value with no "x", and the checkbox accepts a change only when the model
   allows it (a focus switches it). A value of two characters starts a task of
   300 synchronous steps, thus some turns stop at the limit. After idle, the
   live property of each controlled element is its view value, and no restore
   waits. *)
type raction =
  | R_type of int * string
  | R_input of int * string
  | R_check
  | R_focus
  | R_wait of int
  | R_radio of int
  | R_nested of string

type rmodel = {v1: string; v2: string; box: bool; allow: bool; radio: int}

type rmsg =
  | R_typed of int * string
  | R_box of bool
  | R_switch
  | R_done
  | R_pick of int
  | R_nest

let pp_raction = function
  | R_type (i, s) ->
      "type " ^ string_of_int i ^ " " ^ s
  | R_input (i, s) ->
      "input " ^ string_of_int i ^ " " ^ s
  | R_check ->
      "check"
  | R_focus ->
      "focus"
  | R_wait n ->
      "wait " ^ string_of_int n
  | R_radio i ->
      "radio " ^ string_of_int i
  | R_nested s ->
      "nested input " ^ s

let gen_text =
  Gen.map
    (fun l -> String.concat "" l)
    (Gen.list ~size:(Gen.int_range 1 3) (Gen.of_list ["a"; "b"; "x"]))

let gen_raction =
  Gen.frequency
    [ ( 3
      , Gen.map
          (fun (i, s) -> R_type (i, s))
          (Gen.pair (Gen.int_range 1 2) gen_text) )
    ; ( 2
      , Gen.map
          (fun (i, s) -> R_input (i, s))
          (Gen.pair (Gen.int_range 1 2) gen_text) )
    ; (2, Gen.constant R_check)
    ; (1, Gen.constant R_focus)
    ; (2, Gen.map (fun n -> R_wait n) (Gen.int_range 1 40))
    ; (2, Gen.map (fun i -> R_radio i) (Gen.int_range 0 2))
    ; (1, Gen.map (fun s -> R_nested s) gen_text) ]

let rgen =
  Gen.with_pp
    (fun ppf (c, l) ->
      Format.pp_print_string ppf
        ( "cost " ^ string_of_float c ^ ": "
        ^ String.concat ", " (List.map pp_raction l) ) )
    (Gen.pair
       (Gen.of_list [1. /. 128.; 1.; 20.])
       (Gen.list ~size:(Gen.int_range 1 20) gen_raction) )

let rec controlled n acc =
  let acc = List.fold_left (fun acc c -> controlled c acc) acc (F.children n) in
  match F.cell n with
  | Some ({value= Some _; _} as c) | Some ({checked= Some _; _} as c) ->
      (n, c) :: acc
  | Some _ | None ->
      acc

(* The radio group accepts a change only when the model allows it. A nested
   input is a discrete event inside the Macro turn of a focus. After each
   action, if no render is pending and no restore waits, the live property of
   each controlled element is its view value. *)
let restore_law (cost, actions) =
  let h = H.create ~cost () in
  let page = ref None and nested = ref "" in
  let id i = By.test_id ("i" ^ string_of_int i) in
  let update m = function
    | R_typed (i, s) ->
        if String.contains s 'x' then (m, Cmd.none)
        else
          ( (if i = 1 then {m with v1= s} else {m with v2= s})
          , if String.length s = 2 then
              Cmd.task (Async.map (fun _ -> R_done) (steps 300))
            else Cmd.none )
    | R_box b ->
        ((if m.allow then {m with box= b} else m), Cmd.none)
    | R_switch ->
        ({m with allow= not m.allow}, Cmd.none)
    | R_done ->
        (m, Cmd.none)
    | R_pick i ->
        ((if m.allow then {m with radio= i} else m), Cmd.none)
    | R_nest ->
        Option.iter (fun p -> P.input p (id 1) !nested) !page ;
        (m, Cmd.none)
  in
  let input i v =
    El.input
      ~at:[At.data "testid" ("i" ^ string_of_int i); At.value v]
      ~ev:[Ev.input (fun s -> R_typed (i, s))]
      ()
  in
  let view m =
    El.div
      [ input 1 m.v1
      ; input 2 m.v2
      ; El.input
          ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked m.box]
          ~ev:[Ev.check (fun b -> R_box b); Ev.focus R_switch]
          ()
      ; El.input ~at:[At.data "testid" "n"] ~ev:[Ev.focus R_nest] ()
      ; El.div
          (List.init 3 (fun i ->
               El.input
                 ~at:
                   [ At.data "testid" ("r" ^ string_of_int i)
                   ; At.type_ "radio"
                   ; At.name "g"
                   ; At.checked (m.radio = i) ]
                 ~ev:[Ev.check (fun _ -> R_pick i)]
                 () ) ) ]
  in
  let live_is_view where =
    List.iter
      (fun (n, (c : Private.Dom.cell)) ->
        Option.iter
          (fun v -> equal string ~msg:("the live value " ^ where) v (F.value n))
          c.value ;
        Option.iter
          (fun b ->
            equal bool ~msg:("the live checked " ^ where) b (F.checked n) )
          c.checked )
      (controlled (P.root (Option.get !page)) [])
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () ->
           ({v1= ""; v2= ""; box= false; allow= true; radio= 0}, Cmd.none) )
         ~update ~view () )
  in
  page := Some p ;
  List.iter
    (fun a ->
      ( match a with
      | R_type (i, s) ->
          P.type_ p (id i) s
      | R_input (i, s) ->
          P.input p (id i) s
      | R_check ->
          P.click p (By.test_id "c")
      | R_focus ->
          P.focus p (By.test_id "c")
      | R_wait n ->
          P.advance p (Span.ms n)
      | R_radio i ->
          P.click p (By.test_id ("r" ^ string_of_int i))
      | R_nested s ->
          nested := s ;
          P.focus p (By.test_id "n") ) ;
      settled p ;
      let i = ins p in
      if (not i.pending) && i.restores_left = 0 then
        live_is_view "after an action" )
    actions ;
  quiet p ;
  equal int 0 (ins p).restores_left ;
  live_is_view "at the end" ;
  equal int 6 (List.length (controlled (P.root p) [])) ;
  no_errors p

let tests =
  [ group "restore"
      [ test "a yield with no restore" yield_no_restore
      ; test "a rejected input is restored" rejected
      ; test "a controlled checkbox" check_box
      ; test "only an HTML checkbox toggles" no_toggle
      ; test "a checkbox is not in a radio group" not_in_group
      ; test "a handler that prevents the default undoes the click" prevented
      ; test "a radio group with a rejected change" radio_group
      ; test "a focus gives no restore" no_restore_focus
      ; test "the restores of nested events wait for the frame" nested_restore
      ; test "the restore of a removed target does nothing" removed_target
      ; test "the restore of a target with a removed parent does nothing"
          removed_parent
      ; test "a stop clears the restores" stop_clears
      ; test "a restore after a root catch" after_catch
      ; test "change, submit and keydown render at once" events
      ; test "type_ gives one turn for each character" type_text
      ; prop ~count:200 "the controlled inputs on random schedules" rgen
          restore_law ] ]
