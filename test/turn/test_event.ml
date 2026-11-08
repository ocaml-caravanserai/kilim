(* The event walk (design section 11): the discrete events, the order from the
   target up, the stop, and the events that do not bubble. *)

open Windtrap
open Kilim
open Support
module E = Private.Event

let discrete () =
  List.iter
    (fun n -> is_true ~msg:n (E.discrete n))
    ["click"; "input"; "change"; "submit"; "keydown"] ;
  List.iter
    (fun n -> is_false ~msg:n (E.discrete n))
    ["keyup"; "focusin"; "focusout"; "mouseenter"; "mouseleave"; "scroll"] ;
  List.iter
    (fun n -> is_false ~msg:n (E.bubbles n))
    ["mouseenter"; "mouseleave"; "scroll"] ;
  List.iter
    (fun n -> is_true ~msg:n (E.bubbles n))
    ["click"; "focusin"; "keydown"; "input"] ;
  (* Ev uses the same list. Each name of Ev is in exactly one of the two lists,
     and [bubbles] is false exactly for the direct names. *)
  let evs : unit Ev.t list =
    [ Ev.click ()
    ; Ev.check ignore
    ; Ev.input ignore
    ; Ev.change ignore
    ; Ev.submit ()
    ; Ev.keydown ignore
    ; Ev.keyup ignore
    ; Ev.focus ()
    ; Ev.blur ()
    ; Ev.mouseenter ()
    ; Ev.mouseleave ()
    ; Ev.scroll ()
    ; Ev.wheel ()
    ; Ev.touchstart ()
    ; Ev.touchmove () ]
  in
  List.iter
    (fun (h : unit Ev.t) ->
      let d = List.mem h.event E.delegated_events
      and r = List.mem h.event E.direct_events in
      is_true ~msg:h.event (d <> r) ;
      equal bool ~msg:h.event (not r) h.bubbles ;
      equal bool ~msg:h.event (not r) (E.bubbles h.event) )
    evs ;
  List.iter
    (fun n -> is_false ~msg:n (E.bubbles n))
    ["wheel"; "touchstart"; "touchmove"] ;
  equal int 10 (List.length E.delegated_events) ;
  equal int 6 (List.length E.direct_events) ;
  List.iter
    (fun n -> is_true ~msg:n (E.bubbles n && not (E.discrete n)))
    ["keyup"; "focusin"; "focusout"; "compositionstart"; "compositionend"]

(* A tree of three elements: the outer one, the middle one and the target. *)
let tree name =
  let root = F.element "div" in
  let got = ref [] in
  let h tag = {(Ev.click tag) with event= name} in
  let v =
    El.div
      ~ev:[h "outer"]
      [ El.p
          [El.span ~ev:[h "middle"; Ev.blur "other"] [El.b ~ev:[h "target"] []]]
      ]
  in
  let (_ : (F.node, string) Private.Diff.t) =
    Private.Diff.mount
      (F.ops (F.create ()))
      ~parent:root
      ~post:(Private.Dom.post_now (fun m -> got := m :: !got))
      v
  in
  let rec deepest n = match F.children n with [c] -> deepest c | _ -> n in
  (deepest root, got)

let fire (Private.Dom.Handler h) =
  match h.payload with Private.Dom.Unit -> h.fire () | _ -> fail "a payload"

let walk () =
  let target, got = tree "click" in
  E.walk ~parent:F.parent ~cell:F.cell target "click" fire ;
  equal (list string) ["target"; "middle"; "outer"] (List.rev !got) ;
  (* A stop ends the walk after its handler. *)
  let root = F.element "div" in
  let got = ref [] in
  let v =
    El.div
      ~ev:[Ev.click "outer"]
      [El.p ~ev:[Ev.click ~stop:true "middle"] [El.b ~ev:[Ev.click "target"] []]]
  in
  let (_ : (F.node, string) Private.Diff.t) =
    Private.Diff.mount
      (F.ops (F.create ()))
      ~parent:root
      ~post:(Private.Dom.post_now (fun m -> got := m :: !got))
      v
  in
  let rec deepest n = match F.children n with [c] -> deepest c | _ -> n in
  E.walk ~parent:F.parent ~cell:F.cell (deepest root) "click" fire ;
  equal (list string) ["target"; "middle"] (List.rev !got) ;
  (* An event that does not bubble: only the target. *)
  let target, got = tree "scroll" in
  E.walk ~parent:F.parent ~cell:F.cell target "scroll" fire ;
  equal (list string) ["target"] !got ;
  (* No handler on the target: the walk goes up. *)
  let target, got = tree "click" in
  E.walk ~parent:F.parent ~cell:F.cell
    (Option.get (F.parent target))
    "click" fire ;
  equal (list string) ["middle"; "outer"] (List.rev !got)

type msg = Inner | Outer | Focus

let page () =
  let seen = ref [] in
  let app =
    App.make
      ~init:(fun () -> ([], Cmd.none))
      ~update:(fun m msg ->
        seen := msg :: !seen ;
        (msg :: m, Cmd.none) )
      ~view:(fun m ->
        El.div
          ~ev:[Ev.click Outer]
          [ El.button
              ~at:[At.data "testid" "b"]
              ~ev:[Ev.click Inner; Ev.focus Focus]
              [txt (string_of_int (List.length m))] ] )
      ()
  in
  let p = P.start app in
  click p ;
  equal int ~msg:"one turn" 2 (stats p).turns ;
  equal int 2 (stats p).messages ;
  equal int 2 (stats p).renders ;
  (* The order of the walk: the target first. *)
  is_true ([Inner; Outer] = List.rev !seen) ;
  equal string "2" (F.text (P.find p (By.test_id "b"))) ;
  (* A focusin: a Macro turn, thus no render at once. *)
  P.focus p (By.test_id "b") ;
  equal int 3 (stats p).turns ;
  equal int 2 (stats p).renders ;
  is_true (ins p).pending ;
  P.advance p (Span.ms 16) ;
  equal int 3 (stats p).renders ;
  equal string "3" (F.text (P.find p (By.test_id "b"))) ;
  (* The queries. *)
  is_true (P.find p (By.node (P.root p)) == P.root p) ;
  raises_match
    (function
      | P.Query s -> String.equal s "no element with data-testid x" | _ -> false )
    (fun () -> P.find p (By.test_id "x")) ;
  let twice =
    App.make
      ~init:(fun () -> ((), Cmd.none))
      ~update:(fun m _ -> (m, Cmd.none))
      ~view:(fun () -> El.div [button Inner "1"; button Inner "2"])
      ()
  in
  let p2 = P.start twice in
  raises_match
    (function
      | P.Query s ->
          String.equal s "more than one element with data-testid b"
      | _ ->
          false )
    (fun () -> click p2) ;
  no_errors p

(* A top-level call that raises: the next call runs the microtasks. *)
let depth () =
  let h = H.create () in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m -> function
        | Inner ->
            ( m
            , Cmd.task
                (Async.make (fun _ ~ok ~error:_ ->
                     H.raw_microtask h (fun () -> ok Outer) ) ) )
        | _ ->
            (m + 1, Cmd.none) )
      ~view:(fun m -> button Inner (string_of_int m))
      ()
  in
  let p = P.start ~host:h app in
  raises_match
    (function P.Query _ -> true | _ -> false)
    (fun () -> P.click p (By.test_id "x")) ;
  click p ;
  equal int 2 (stats p).messages ;
  equal int 0 (ins p).continuations_left

(* The message function of a handler runs when its item leaves the queue (design
   section 09), not in the event. A re-entrant click in update only puts its
   item in the queue: its message function runs after the update. *)
type qmsg = First | Second | Mapped

let at_dequeue () =
  let log = ref [] and page = ref None in
  let mapped () =
    log := "map" :: !log ;
    Mapped
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () -> function
           | First ->
               log := "first" :: !log ;
               Option.iter (fun p -> P.click p (By.test_id "m")) !page ;
               log := "first end" :: !log ;
               ((), Cmd.none)
           | Second | Mapped ->
               log := "update" :: !log ;
               ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             [ button First "f"
             ; El.button
                 ~at:[At.data "testid" "m"]
                 ~ev:[Ev.check (fun _ -> mapped ())]
                 [] ] )
         () )
  in
  page := Some p ;
  click p ;
  equal (list string) ["first"; "first end"; "map"; "update"] (List.rev !log) ;
  (* Outside a turn, the function runs in the turn of the event. *)
  log := [] ;
  P.click p (By.test_id "m") ;
  equal (list string) ["map"; "update"] (List.rev !log) ;
  equal int 3 (stats p).messages ;
  no_errors p

(* Two handlers on the path: the message function of the outer handler raises.
   The message of the target reaches update first, then the raise leaves the
   queue and goes to the root catch. *)
exception Boom

let raise_at_dequeue () =
  let got = ref [] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           got := m :: !got ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div ~ev:[Ev.check (fun _ -> raise Boom)] [button Second "b"] )
         ~fallback:(fun _ -> txt "fallback")
         () )
  in
  click p ;
  is_true ([Second] = !got) ;
  equal int 1 (stats p).messages ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  is_false (ins p).alive ;
  equal string "<div>fallback</div>" (P.html p) ;
  quiet p

(* The handlers of the root fallback post to the dead root: the item is dropped,
   and the message function never runs. *)
let dead_root () =
  let calls = ref 0 in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () _ -> raise Boom)
         ~view:(fun () -> button First "b")
         ~fallback:(fun _ ->
           El.button
             ~at:[At.data "testid" "fb"]
             ~ev:[Ev.check (fun _ -> incr calls ; Second)]
             [txt "fb"] )
         () )
  in
  click p ;
  is_false (ins p).alive ;
  P.click p (By.test_id "fb") ;
  P.click p (By.test_id "fb") ;
  equal int 0 !calls ;
  equal int 2 (stats p).dropped ;
  equal int 1 (List.length (errors p)) ;
  quiet p

(* Ev.link_click (design section 08): the filter Plain_click. The app has an
   outer div with Ev.click, the link with Ev.link_click, and an element [in] in
   the link with the handlers [inner]. *)
let link_page ?(stop = false) inner =
  let got = ref [] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           got := m :: !got ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             ~ev:[Ev.click "out"]
             [ El.a
                 ~at:[At.href "/x"; At.data "testid" "a"]
                 ~ev:[Ev.link_click ~stop "link"]
                 [El.b ~at:[At.data "testid" "in"] ~ev:inner [txt "x"]] ] )
         () )
  in
  (p, got)

(* [f p] clicks, then the messages and the prevent of the click. *)
let link ?stop ?(inner = []) ?(id = "a") f =
  let p, got = link_page ?stop inner in
  is_false ~msg:"no event yet" (P.prevented p) ;
  f p (By.test_id id) ;
  let r = (List.rev !got, P.prevented p) in
  no_errors p ; r

let result = pair (list string) bool

let plain_click () =
  equal result (["link"; "out"], true) (link (fun p by -> P.click p by)) ;
  (* An event with no mouse values (Page.event) is a plain click. *)
  equal result (["link"; "out"], true) (link (fun p by -> P.event p by "click")) ;
  (* A click on an element in the link. *)
  equal result (["link"; "out"], true) (link ~id:"in" (fun p by -> P.click p by)) ;
  equal result
    (["in"; "link"; "out"], true)
    (link ~id:"in" ~inner:[Ev.click "in"] (fun p by -> P.click p by)) ;
  (* The stop of a plain click ends the walk. *)
  equal result (["link"], true) (link ~stop:true (fun p by -> P.click p by))

let skipped () =
  let skip ~msg f =
    equal result ~msg (["out"], false) (link f) ;
    (* The stop of a skipped handler applies: the click handler above does not
       run. *)
    equal result ~msg ([], false) (link ~stop:true f)
  in
  skip ~msg:"alt" (fun p by -> P.click p ~alt:true by) ;
  skip ~msg:"ctrl" (fun p by -> P.click p ~ctrl:true by) ;
  skip ~msg:"meta" (fun p by -> P.click p ~meta:true by) ;
  skip ~msg:"shift" (fun p by -> P.click p ~shift:true by) ;
  skip ~msg:"button 1" (fun p by -> P.click p ~button:1 by) ;
  skip ~msg:"button 2" (fun p by -> P.click p ~button:2 by) ;
  (* A listener of the page prevented the default: the page tells it. *)
  equal result (["out"], true) (link (fun p by -> P.click p ~prevented:true by)) ;
  equal result ([], true)
    (link ~stop:true (fun p by -> P.click p ~prevented:true by)) ;
  (* A handler below prevents the default. *)
  equal result
    (["in"; "out"], true)
    (link ~id:"in"
       ~inner:[Ev.click ~prevent_default:true "in"]
       (fun p by -> P.click p by) )

let link_handler () =
  let h : unit Ev.t = Ev.link_click () in
  equal string "click" h.event ;
  is_true h.bubbles ;
  is_true h.prevent_default ;
  is_false h.stop ;
  is_true (Ev.link_click ~stop:true ()).stop ;
  is_true (h.filter = Private.Dom.Plain_click) ;
  is_true ((Ev.click ()).filter = Private.Dom.Any) ;
  ( match h.decode with
  | Private.View.Decode (Private.Dom.Unit, f) ->
      f ()
  | _ ->
      fail "the payload of link_click is Unit" ) ;
  (* One handler for each DOM event name. *)
  raises_match
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> El.a ~ev:[Ev.click (); Ev.link_click ()] [])

(* Page.click ~prevented on a checkbox: the browser undoes the toggle. *)
let prevented_toggle () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () ->
           El.input ~at:[At.type_ "checkbox"; At.data "testid" "c"] () )
         () )
  in
  let c = By.test_id "c" in
  P.click p ~prevented:true c ;
  is_false (F.checked (P.find p c)) ;
  is_true (P.prevented p) ;
  P.click p c ;
  is_true (F.checked (P.find p c)) ;
  is_false (P.prevented p)

let tests =
  [ group "event"
      [ test "the discrete events and the events that do not bubble" discrete
      ; test "the walk: from the target up, the stop, no bubble" walk
      ; test "Page: two handlers in one turn, and a focus" page
      ; test "Page: a call that raises" depth
      ; test "a message function runs when its message leaves the queue"
          at_dequeue
      ; test
          "a message function that raises goes to the root catch at the dequeue"
          raise_at_dequeue
      ; test "the message function of a dead root does not run" dead_root
      ; test "link_click: a plain click prevents and gives the message"
          plain_click
      ; test "link_click: another click gives no message and no prevent" skipped
      ; test "link_click: the fields of the handler" link_handler
      ; test "Page.click ~prevented undoes the toggle of a checkbox"
          prevented_toggle ] ]
