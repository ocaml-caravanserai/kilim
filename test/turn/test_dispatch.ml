(* The rules of Event.dispatch through Page (design section 11): the IME
   composition, the keys during a composition, the restore task after a toggle
   click, the undo of a prevented radio click, the events with a listener on the
   element, and a focusout inside a turn. *)

open Windtrap
open Kilim
open Support

let field = By.test_id "i"

(* The set_property operations since the last reset. *)
let writes p =
  Option.value ~default:0 (List.assoc_opt "set_property" (F.counts (P.doc p)))

(* An input whose update gives [f] of the value. A click on "t" adds one to a
   count in the view, and "k" gives the keys. *)
type imsg = Typed of string | Tick | Key of string

let ime_app ?(f = String.uppercase_ascii) ?(every = false) () =
  App.make
    ~init:(fun () -> (("", 0, []), Cmd.none))
    ~update:(fun (s, n, keys) -> function
      | Typed v ->
          ((f v, n, keys), Cmd.none)
      | Tick ->
          ((s, n + 1, keys), Cmd.none)
      | Key k ->
          ((s, n, k :: keys), Cmd.none) )
    ~view:(fun (s, n, _) ->
      El.div
        [ El.input
            ~at:[At.data "testid" "i"; At.value s]
            ~ev:
              [ Ev.input (fun v -> Typed v)
              ; Ev.keydown (fun k -> Key k)
              ; Ev.keyup (fun k -> Key ("up " ^ k)) ]
            ()
        ; button ~id:"t" Tick (string_of_int n) ] )
    ?subs:(if every then Some (fun _ -> Sub.every (Span.ms 10) Tick) else None)
    ()

let cell p = Option.get (F.cell (P.find p field))

(* One message for each text, no value write during the composition, one write
   in the task after compositionend. *)
let compose () =
  let p = P.start (ime_app ()) in
  F.reset (P.doc p) ;
  P.compose p field ["k"; "ka"] ;
  equal int 2 (stats p).messages ;
  equal int ~msg:"one write, in the task after compositionend" 1 (writes p) ;
  equal string "KA" (F.value (P.find p field)) ;
  is_false (cell p).composing ;
  settled p ;
  (* An accepted value: no write. *)
  let p = P.start (ime_app ~f:Fun.id ()) in
  F.reset (P.doc p) ;
  P.compose p field ["k"; "ka"] ;
  equal int 0 (writes p) ;
  equal string "ka" (F.value (P.find p field)) ;
  (* The start value stays before the texts. *)
  P.compose p field ["b"] ;
  equal string "kab" (F.value (P.find p field)) ;
  quiet p ;
  no_errors p

(* The two orders of a composition, with a commit text that is not the last text
   with isComposing in Firefox. Firefox sends the last input after
   compositionend. In each order: one message for each text, one write, and the
   value is the view of the commit. *)
let compose_order () =
  List.iter
    (fun order ->
      let p = P.start (ime_app ()) in
      F.reset (P.doc p) ;
      P.compose p ~order field ["k"; "ka"; "kai"] ;
      equal int 3 (stats p).messages ;
      equal int ~msg:"one write" 1 (writes p) ;
      equal string "KAI" (F.value (P.find p field)) ;
      is_false (cell p).composing ;
      settled p ;
      quiet p ;
      no_errors p )
    [`Chrome; `Firefox]

(* The order of Firefox, with a render that is pending at compositionend: the
   keyup runs a Macro turn that changes the model. A render at compositionend
   writes the old view value over the commit. *)
let compose_pending () =
  let p = P.start (ime_app ()) in
  P.keyup p field "a" ;
  is_true ~msg:"a pending render" (ins p).pending ;
  F.reset (P.doc p) ;
  P.compose p ~order:`Firefox field ["kai"] ;
  equal int 2 (stats p).messages ;
  equal int ~msg:"one write" 1 (writes p) ;
  equal string "KAI" (F.value (P.find p field)) ;
  settled p ;
  quiet p ;
  no_errors p

(* Two compositions in a chain: Chrome can send compositionend, then a new
   compositionstart in the same task. The restore task finds the flag set again
   and writes nothing. The end of the last composition writes the view value one
   time. The test calls Event.dispatch, thus the two events are in one task. *)
let chained () =
  let h = H.create () in
  let doc = F.create () in
  let root = F.element "div" in
  let ops = F.ops doc in
  let rt = Private.Runtime.start (H.env h) ops ~parent:root (ime_app ()) in
  H.microtasks h ;
  let i = List.hd (F.children (List.hd (F.children root))) in
  let tasks = Queue.create () in
  let env =
    { Private.Event.ops
    ; parent= F.parent
    ; cell= F.cell
    ; connected= (fun _ -> true)
    ; group= (fun _ -> [])
    ; toggles= (fun _ -> false)
    ; post_task= (fun f -> Queue.push f tasks) }
  in
  let read : type a. a Private.Dom.payload -> a = function
    | Private.Dom.Unit ->
        ()
    | Private.Dom.String ->
        F.value i
    | Private.Dom.Key ->
        ""
    | Private.Dom.Bool ->
        false
    | Private.Dom.Mouse ->
        { Private.Dom.button= 0
        ; alt= false
        ; ctrl= false
        ; meta= false
        ; shift= false
        ; default_prevented= false }
  in
  let send ?(composing = false) name =
    ignore
      (Private.Event.dispatch env rt
         {name; target= i; composing; read; prevent= ignore; stop= ignore} )
  in
  (* The end of a task: the microtasks, then each task of the dispatch. *)
  let finish () =
    H.microtasks h ;
    while not (Queue.is_empty tasks) do
      (Queue.pop tasks) () ;
      H.microtasks h
    done
  in
  let writes () =
    Option.value ~default:0 (List.assoc_opt "set_property" (F.counts doc))
  in
  let composing () = (Option.get (F.cell i)).composing in
  F.focus doc (Some i) ;
  send "compositionstart" ;
  finish () ;
  F.browser_set i (Value "k") ;
  send ~composing:true "input" ;
  finish () ;
  F.reset doc ;
  send "compositionend" ;
  send "compositionstart" ;
  finish () ;
  equal int ~msg:"no write in the restore task" 0 (writes ()) ;
  equal string "k" (F.value i) ;
  is_true (composing ()) ;
  F.browser_set i (Value "kb") ;
  send ~composing:true "input" ;
  finish () ;
  send "compositionend" ;
  finish () ;
  equal int ~msg:"one write at the last end" 1 (writes ()) ;
  equal string "KB" (F.value i) ;
  is_false (composing ()) ;
  equal int 0 (List.length (H.errors h))

(* The handlers of compositionend run in their turn. The restore task then
   renders the view of their message. *)
type emsg_c = Ended

let end_handler () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun n Ended -> (n + 1, Cmd.none))
         ~view:(fun n ->
           El.input
             ~at:[At.data "testid" "i"; At.value (string_of_int n)]
             ~ev:[{(Ev.click Ended) with event= "compositionend"}]
             () )
         () )
  in
  P.compose p field ["a"] ;
  equal int 1 (stats p).messages ;
  equal string "1" (F.value (P.find p field)) ;
  settled p ;
  quiet p ;
  no_errors p

(* Page.compose sends the events in the order of the browser. A handler of
   compositionend reads the live value. *)
type omsg = Typed_o of string | End_o of string

let compose_events () =
  List.iter
    (fun (order, texts, expected) ->
      let log = ref [] in
      let p =
        P.start
          (App.make
             ~init:(fun () -> ((), Cmd.none))
             ~update:(fun () m ->
               log := m :: !log ;
               ((), Cmd.none) )
             ~view:(fun () ->
               El.input
                 ~at:[At.data "testid" "i"]
                 ~ev:
                   [ Ev.input (fun v -> Typed_o v)
                   ; {(Ev.input (fun v -> End_o v)) with event= "compositionend"}
                   ]
                 () )
             () )
      in
      P.compose p ~order field texts ;
      is_true (List.rev !log = expected) ;
      no_errors p )
    [ (`Chrome, ["k"; "ka"], [Typed_o "k"; Typed_o "ka"; End_o "ka"])
    ; (`Firefox, ["k"; "ka"], [Typed_o "k"; End_o "ka"; Typed_o "ka"])
    ; (`Firefox, [], [End_o ""]) ]

(* A render of Sub.every during the composition writes no value. *)
let compose_every () =
  let p = P.start (ime_app ~every:true ()) in
  F.reset (P.doc p) ;
  P.focus p field ;
  P.event p field "compositionstart" ;
  is_true (cell p).composing ;
  P.input p field "a" ;
  P.advance p (Span.ms 50) ;
  is_true ((stats p).renders >= 4) ;
  equal int ~msg:"no write during the composition" 0 (writes p) ;
  equal string "a" (F.value (P.find p field)) ;
  P.event p field "compositionend" ;
  equal int 1 (writes p) ;
  equal string "A" (F.value (P.find p field)) ;
  no_errors p

(* With no compositionend: a focusout or a focusin ends the composition, and its
   restore writes the view value. An input with isComposing false does not end
   it. *)
let no_end () =
  let check finish =
    let p = P.start (ime_app ()) in
    P.focus p field ;
    P.event p field "compositionstart" ;
    P.input p field "a" ;
    P.input p field "ab" ;
    is_true (cell p).composing ;
    equal string "ab" (F.value (P.find p field)) ;
    F.reset (P.doc p) ;
    finish p ;
    is_false (cell p).composing ;
    equal int 1 (writes p) ;
    equal string "AB" (F.value (P.find p field)) ;
    (* The next input writes. *)
    P.input p field "abc" ;
    equal string "ABC" (F.value (P.find p field)) ;
    no_errors p
  in
  check (fun p -> P.blur p field) ;
  check (fun p -> P.focus p field)

(* A keyed swap of the composing input and a button: no compositionend, then the
   focusout of Chrome. The next model change writes the value. The diff moves
   the button, thus the input keeps the focus, and the flag stays until the
   focusout. *)
type mmsg = Swap | Typed_m of string

let moved () =
  let app =
    App.make
      ~init:(fun () -> ((false, ""), Cmd.none))
      ~update:(fun (sw, s) -> function
        | Swap ->
            ((not sw, s), Cmd.none)
        | Typed_m v ->
            ((sw, String.uppercase_ascii v), Cmd.none) )
      ~view:(fun (sw, s) ->
        let rows =
          [ ( Key.int 1
            , El.input
                ~at:[At.data "testid" "i"; At.value s]
                ~ev:[Ev.input (fun v -> Typed_m v)]
                () )
          ; (Key.int 2, button Swap "swap") ]
        in
        View.keyed El.div (if sw then List.rev rows else rows) )
      ()
  in
  let p = P.start app in
  let n = P.find p field in
  P.focus p field ;
  P.event p field "compositionstart" ;
  P.input p field "a" ;
  click p ;
  is_true (n == P.find p field) ;
  is_true ~msg:"the input keeps the focus"
    (match F.focused (P.doc p) with Some f -> f == n | None -> false) ;
  is_true (cell p).composing ;
  equal string "a" (F.value n) ;
  P.blur p field ;
  is_false (cell p).composing ;
  equal string "A" (F.value n) ;
  P.input p field "ab" ;
  equal string "AB" (F.value n) ;
  no_errors p

(* A composition stops a value write only while its element has the focus. A
   keyed move takes the focus away from the input, and Firefox and WebKit then
   send no compositionend and no focusout. The render of the move clears the
   flag and writes the value, and a later model change writes too. *)
type fmmsg = Swap_f | Typed_f of string | More_f

let focus_app () =
  App.make
    ~init:(fun () -> ((false, ""), Cmd.none))
    ~update:(fun (sw, s) -> function
      | Swap_f ->
          ((not sw, s), Cmd.none)
      | Typed_f v ->
          ((sw, String.uppercase_ascii v), Cmd.none)
      | More_f ->
          ((sw, s ^ "!"), Cmd.none) )
    ~view:(fun (sw, s) ->
      let rows =
        [ ( Key.int 1
          , El.input
              ~at:[At.data "testid" "i"; At.value s]
              ~ev:[Ev.input (fun v -> Typed_f v)]
              () )
        ; (Key.int 2, button ~id:"sw" Swap_f "swap")
        ; (Key.int 3, button ~id:"more" More_f "more") ]
      in
      (* The swap moves only the input: the two buttons keep their order. *)
      View.keyed El.div (if sw then List.tl rows @ [List.hd rows] else rows) )
    ()

let focus_lost () =
  let p = P.start (focus_app ()) in
  let n = P.find p field in
  P.focus p field ;
  P.event p field "compositionstart" ;
  P.input p field "ka" ;
  F.reset (P.doc p) ;
  P.click p (By.test_id "more") ;
  is_true ~msg:"composing with the focus" (cell p).composing ;
  equal int ~msg:"no write with the focus" 0 (writes p) ;
  equal string "ka" (F.value n) ;
  P.click p (By.test_id "sw") ;
  is_true (F.focused (P.doc p) = None) ;
  is_false ~msg:"the move ends the composition" (cell p).composing ;
  equal int ~msg:"one write" 1 (writes p) ;
  equal string "KA!" (F.value n) ;
  P.click p (By.test_id "more") ;
  equal string "KA!!" (F.value n) ;
  settled p ;
  quiet p ;
  no_errors p

(* A blur with no focusout: the next render writes the value. *)
let focus_moved () =
  let p = P.start (focus_app ()) in
  let n = P.find p field in
  P.focus p field ;
  P.event p field "compositionstart" ;
  P.input p field "ka" ;
  equal string "ka" (F.value n) ;
  P.focus p (By.test_id "more") ;
  is_true ~msg:"the focusin of another element" (cell p).composing ;
  P.click p (By.test_id "more") ;
  is_false (cell p).composing ;
  equal string "KA!" (F.value n) ;
  no_errors p

(* Page.focus gives the focus to its target, and Page.blur takes it away only
   from its target. *)
let page_focus () =
  let p = P.start (focus_app ()) in
  let n = P.find p field in
  let has n =
    match F.focused (P.doc p) with Some f -> f == n | None -> false
  in
  is_false (has n) ;
  P.focus p field ;
  is_true (has n) ;
  P.blur p (By.test_id "more") ;
  is_true ~msg:"the blur of another element" (has n) ;
  P.blur p field ;
  is_true (F.focused (P.doc p) = None) ;
  no_errors p

(* A keydown or a keyup with isComposing: no message and no turn. *)
let keys () =
  let p = P.start (ime_app ()) in
  let turns = (stats p).turns in
  P.keydown p ~composing:true field "Enter" ;
  P.keyup p ~composing:true field "Enter" ;
  equal int turns (stats p).turns ;
  equal int 0 (stats p).messages ;
  P.keydown p ~composing:false field "Enter" ;
  P.keyup p field "a" ;
  equal int (turns + 2) (stats p).turns ;
  equal int 2 (stats p).messages ;
  no_errors p

(* A click on a checkbox posts the restore task, also with no prevent. A
   prevented check whose update sets the model: the box is checked after the
   click. *)
type cmsg = Checked of bool

let box ~prevent =
  App.make
    ~init:(fun () -> (false, Cmd.none))
    ~update:(fun _ (Checked b) -> (b, Cmd.none))
    ~view:(fun m ->
      El.input
        ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked m]
        ~ev:[Ev.check ~prevent_default:prevent (fun b -> Checked b)]
        () )
    ()

let toggle_task () =
  let p = P.start (box ~prevent:false) in
  P.click p (By.test_id "c") ;
  equal int ~msg:"the turn and the task" 2 (stats p).restores ;
  is_true (F.checked (P.find p (By.test_id "c"))) ;
  (* Only a click posts the task: not a focus or a change. *)
  P.focus p (By.test_id "c") ;
  P.change p (By.test_id "c") "on" ;
  equal int ~msg:"the restore of the change only" 3 (stats p).restores ;
  let p = P.start (box ~prevent:true) in
  F.reset (P.doc p) ;
  P.click p (By.test_id "c") ;
  is_true (F.checked (P.find p (By.test_id "c"))) ;
  (* The undo unchecks the box, and the task checks it again. *)
  equal int 1 (writes p) ;
  equal int 2 (stats p).restores ;
  settled p ;
  quiet p ;
  no_errors p

(* A prevented click on r2 whose render removes the checked r1. r1 is not in the
   group after the render, thus the undo does not change r2 (Chrome). The
   restore task then writes the view value of r2. *)
type rmsg = Pick

let pr3b ~keep =
  App.make
    ~init:(fun () -> (false, Cmd.none))
    ~update:(fun _ Pick -> (true, Cmd.none))
    ~view:(fun picked ->
      let radio id checked =
        El.input
          ~at:
            [ At.data "testid" id
            ; At.type_ "radio"
            ; At.name "g"
            ; At.checked checked ]
          ~ev:[Ev.check ~prevent_default:true (fun _ -> Pick)]
          ()
      in
      (* r2 comes first: the patch keeps its node, and removes r1. *)
      El.div
        ( radio "r2" (picked && keep)
        :: (if picked then [] else [radio "r1" true]) ) )
    ()

let radio_removed () =
  let p = P.start (pr3b ~keep:true) in
  let r1 = P.find p (By.test_id "r1") in
  is_true (F.checked r1) ;
  F.reset (P.doc p) ;
  P.click p (By.test_id "r2") ;
  is_true (F.checked (P.find p (By.test_id "r2"))) ;
  is_false ~msg:"the removed r1 stays unchecked" (F.checked r1) ;
  equal int ~msg:"the undo does not uncheck r2" 0 (writes p) ;
  let p = P.start (pr3b ~keep:false) in
  P.click p (By.test_id "r2") ;
  is_false (F.checked (P.find p (By.test_id "r2"))) ;
  quiet p ;
  no_errors p

(* An event with a listener on the element: nothing for a removed node, and only
   the handler of the node for a connected node. *)
type emsg = Enter of string | Hide

let direct () =
  let app =
    App.make
      ~init:(fun () -> ((true, []), Cmd.none))
      ~update:(fun (shown, log) -> function
        | Enter s ->
            ((shown, s :: log), Cmd.none)
        | Hide ->
            ((false, log), Cmd.none) )
      ~view:(fun (shown, log) ->
        El.div
          ~ev:[Ev.mouseenter (Enter "outer")]
          ( [button Hide (string_of_int (List.length log))]
          @
          if shown then
            [ El.span
                ~at:[At.data "testid" "s"]
                ~ev:[Ev.mouseenter (Enter "inner")]
                [] ]
          else [] ) )
      ()
  in
  let p = P.start app in
  let s = P.find p (By.test_id "s") in
  P.event p (By.node s) "mouseenter" ;
  equal int ~msg:"only the handler of the node" 1 (stats p).messages ;
  click p ;
  equal string "1" (F.text (P.find p (By.test_id "b"))) ;
  let turns = (stats p).turns and messages = (stats p).messages in
  P.event p (By.node s) "mouseenter" ;
  equal int turns (stats p).turns ;
  equal int messages (stats p).messages ;
  quiet p ;
  equal string "1" (F.text (P.find p (By.test_id "b"))) ;
  no_errors p

(* A focusout inside a turn (from update) on an input that the render removes:
   no nested turn, and the turn processes its message. The input was composing,
   thus its restore waits, and finds the node not connected. *)
type fmsg = Go | Left

let blur_in_turn () =
  let page = ref None in
  let app =
    App.make
      ~init:(fun () -> ((true, 0), Cmd.none))
      ~update:(fun (shown, n) -> function
        | Go ->
            Option.iter (fun p -> P.blur p field) !page ;
            ((false, n), Cmd.none)
        | Left ->
            ((shown, n + 1), Cmd.none) )
      ~view:(fun (shown, n) ->
        El.div
          ( button Go (string_of_int n)
          ::
          ( if shown then
              [ El.input
                  ~at:[At.data "testid" "i"; At.value "v"]
                  ~ev:[Ev.blur Left]
                  () ]
            else [] ) ) )
      ()
  in
  let p = P.start app in
  page := Some p ;
  P.focus p field ;
  P.event p field "compositionstart" ;
  let turns = (stats p).turns in
  F.reset (P.doc p) ;
  click p ;
  equal int ~msg:"one turn" (turns + 1) (stats p).turns ;
  equal int 2 (stats p).messages ;
  equal string "1" (F.text (P.find p (By.test_id "b"))) ;
  equal int 0 (writes p) ;
  equal int 0 (ins p).restores_left ;
  settled p ;
  quiet p ;
  no_errors p

(* The dispatch calls [prevent] for a handler with prevent_default and [stop]
   for a handler with stop, and gives [true] if a handler prevented. *)
type dmsg = Inner | Outer

let calls () =
  let h = H.create () in
  let root = F.element "div" in
  let ops = F.ops (F.create ()) in
  let got = ref [] in
  let rt =
    Private.Runtime.start (H.env h) ops ~parent:root
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           got := m :: !got ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             ~ev:[Ev.click ~stop:true Outer; Ev.keyup (fun _ -> Outer)]
             [ El.button
                 ~ev:
                   [ Ev.click ~prevent_default:true Inner
                   ; Ev.keyup (fun _ -> Inner) ]
                 [] ] )
         () )
  in
  let div = List.hd (F.children root) in
  let button = List.hd (F.children div) in
  let env =
    { Private.Event.ops
    ; parent= (fun n -> if n == root then None else F.parent n)
    ; cell= F.cell
    ; connected= (fun _ -> true)
    ; group= (fun _ -> [])
    ; toggles= (fun _ -> false)
    ; post_task= (fun _ -> fail "no task") }
  in
  let prevents = ref 0 and stops = ref 0 in
  let read : type a. a Private.Dom.payload -> a = function
    | Private.Dom.Unit ->
        ()
    | Private.Dom.String ->
        ""
    | Private.Dom.Key ->
        "k"
    | Private.Dom.Bool ->
        false
    | Private.Dom.Mouse ->
        { Private.Dom.button= 0
        ; alt= false
        ; ctrl= false
        ; meta= false
        ; shift= false
        ; default_prevented= false }
  in
  let ev name =
    { Private.Event.name
    ; target= button
    ; composing= false
    ; read
    ; prevent= (fun () -> incr prevents)
    ; stop= (fun () -> incr stops) }
  in
  is_true (Private.Event.dispatch env rt (ev "click")) ;
  equal int 1 !prevents ;
  equal int 1 !stops ;
  is_false (Private.Event.dispatch env rt (ev "keyup")) ;
  equal int 1 !prevents ;
  equal int 1 !stops ;
  is_true ([Outer; Inner; Outer; Inner] = !got)

(* A focus event on a node with no cell: a Macro turn with no restore. *)
let no_cell () =
  let p = P.start (ime_app ()) in
  P.blur p (By.node (P.root p)) ;
  P.focus p (By.node (P.root p)) ;
  equal int 0 (stats p).restores ;
  equal int 0 (ins p).restores_left ;
  is_false (ins p).pending ;
  no_errors p

(* A nested call (from update, inside a turn) runs no microtask and no task: the
   top-level call runs them after the turn. *)
type nmsg = Nest | Box of bool

let nested () =
  let h = H.create () in
  let page = ref None and ran = ref false and seen = ref [] in
  let app =
    App.make
      ~init:(fun () -> (false, Cmd.none))
      ~update:(fun m -> function
        | Nest ->
            H.raw_microtask h (fun () -> ran := true) ;
            Option.iter (fun p -> P.click p (By.test_id "c")) !page ;
            seen := (!ran, (stats (Option.get !page)).restores) :: !seen ;
            (m, Cmd.none)
        | Box b ->
            (b, Cmd.none) )
      ~view:(fun m ->
        El.div
          [ button Nest "nest"
          ; El.input
              ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked m]
              ~ev:[Ev.check (fun b -> Box b)]
              () ] )
      ()
  in
  let p = P.start ~host:h app in
  page := Some p ;
  click p ;
  is_true ~msg:"no microtask and no restore in update" ([(false, 0)] = !seen) ;
  is_true ~msg:"the microtask after the turn" !ran ;
  (* The restores of the two clicks, and the restore task. *)
  equal int 3 (stats p).restores ;
  is_true (F.checked (P.find p (By.test_id "c"))) ;
  no_errors p

(* The order of a prevented click: the dispatch, the microtasks, the undo, then
   the restore task. A microtask of the update sees the box toggled. *)
let order () =
  let h = H.create () in
  let page = ref None and log = ref [] in
  let app =
    App.make
      ~init:(fun () -> (false, Cmd.none))
      ~update:(fun _ (Checked b) ->
        H.raw_microtask h (fun () ->
            let p = Option.get !page in
            log := F.checked (P.find p (By.test_id "c")) :: !log ) ;
        (b, Cmd.none) )
      ~view:(fun m ->
        El.input
          ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked m]
          ~ev:[Ev.check ~prevent_default:true (fun b -> Checked b)]
          () )
      ()
  in
  let p = P.start ~host:h app in
  page := Some p ;
  P.click p (By.test_id "c") ;
  is_true ~msg:"the microtask before the undo" ([true] = !log) ;
  is_true (F.checked (P.find p (By.test_id "c"))) ;
  no_errors p

let tests =
  [ group "dispatch"
      [ test "an IME composition writes the value at its end" compose
      ; test "the orders of Chrome and Firefox give one write" compose_order
      ; test "Firefox order with a pending render at compositionend"
          compose_pending
      ; test "two compositions in one task" chained
      ; test "the handlers of compositionend" end_handler
      ; test "the events of Page.compose in each order" compose_events
      ; test "a render during a composition writes no value" compose_every
      ; test "a focusout or a focusin ends a composition" no_end
      ; test "a moved composing input, then its focusout" moved
      ; test "a move takes the focus away from a composition" focus_lost
      ; test "a composition in an element with no focus" focus_moved
      ; test "Page.focus and Page.blur move the focus" page_focus
      ; test "keydown and keyup during a composition" keys
      ; test "the restore task after a checkbox click" toggle_task
      ; test "a prevented radio click that removes the checked button"
          radio_removed
      ; test "a direct event on a removed node does nothing" direct
      ; test "a focusout inside a turn" blur_in_turn
      ; test "prevent and stop of the dispatch" calls
      ; test "a focus event on a node with no cell" no_cell
      ; test "a nested call runs no microtask and no task" nested
      ; test "a microtask runs before the undo of a click" order ] ]
