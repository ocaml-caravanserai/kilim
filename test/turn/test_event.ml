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
  (* Ev uses the same list. *)
  is_false (Ev.scroll ()).bubbles ;
  is_false (Ev.mouseenter ()).bubbles ;
  is_true (Ev.focus ()).bubbles

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
      ~dispatch:(fun m -> got := m :: !got)
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
      ~dispatch:(fun m -> got := m :: !got)
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
  let app =
    App.make
      ~init:(fun () -> ([], Cmd.none))
      ~update:(fun m msg -> (msg :: m, Cmd.none))
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

(* Page reads only the payloads of click and focus in PR 3. *)
let payload () =
  let app =
    App.make
      ~init:(fun () -> (false, Cmd.none))
      ~update:(fun _ b -> (b, Cmd.none))
      ~view:(fun _ ->
        El.input ~at:[At.data "testid" "b"] ~ev:[Ev.check Fun.id] () )
      ()
  in
  let p = P.start app in
  click p ;
  is_true
    ( match errors p with
    | [Invalid_argument s] ->
        String.equal s "Kilim_testing.Page: no payload for the event click"
    | _ ->
        false )

let tests =
  [ group "event"
      [ test "the discrete events and the events that do not bubble" discrete
      ; test "the walk: from the target up, the stop, no bubble" walk
      ; test "Page: two handlers in one turn, and a focus" page
      ; test "Page: no payload of a value" payload
      ; test "Page: a call that raises" depth ] ]
