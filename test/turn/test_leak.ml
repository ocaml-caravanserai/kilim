(* The leak tests of the turn (design section 14): the latest table, the task
   tokens, the subscription keys, the timers, the model of a root that failed,
   the instance links and the lists of the boundaries. *)

open Windtrap
open Kilim
open Support

type msg = Save | Saved of int | Many | Tick

let saver ~task =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m -> function
      | Save ->
          (m + 1, Cmd.latest ~key:("save/" ^ string_of_int m) (task m))
      | Many ->
          ( m + 1000
          , Cmd.batch
              (List.init 1000 (fun i ->
                   Cmd.latest ~key:("save/" ^ string_of_int i) (task i) ) ) )
      | Saved _ | Tick ->
          (m, Cmd.none) )
    ~view:(fun m ->
      El.div [button Save (string_of_int m); button ~id:"m" Many "m"] )
    ()

(* 1,000 distinct keys with synchronous results: an empty table after each
   turn. *)
let sync_keys () =
  let p = P.start (saver ~task:(fun i -> Async.return (Saved i))) in
  for _ = 1 to 1000 do
    click p ;
    equal int 0 (ins p).latest_keys ;
    equal int 0 (ins p).token_children
  done ;
  equal int 2000 (stats p).messages ;
  no_errors p

(* 1,000 distinct keys that wait for raw results. *)
let pending_keys () =
  let h = H.create () in
  let task i =
    Async.make (fun _ ~ok ~error:_ ->
        H.raw h ~after:10 (fun () -> ok (Saved i)) )
  in
  let p = P.start ~host:h (saver ~task) in
  P.click p (By.test_id "m") ;
  equal int 1000 (ins p).latest_keys ;
  equal int 1000 (ins p).token_children ;
  H.idle h ;
  equal int 0 (ins p).latest_keys ;
  equal int 0 (ins p).token_children ;
  equal int 1001 (stats p).messages ;
  no_errors p

(* 1,000 times a start, a click and a stop on one host. *)
let starts () =
  let h = H.create () in
  let app =
    App.make
      ~init:(fun () ->
        (0, Cmd.task (Async.map (fun () -> Tick) (Async.sleep (Span.s 1)))) )
      ~update:(fun m _ ->
        (m + 1, Cmd.task (Async.map (fun () -> Tick) (Async.sleep (Span.s 1)))) )
      ~view:(fun m -> button Save (string_of_int m))
      ()
  in
  for _ = 1 to 1000 do
    let p = P.start ~host:h app in
    click p ; P.stop p
  done ;
  equal int 0 (H.live_timers h) ;
  equal int 0 (List.length (H.errors h)) ;
  H.idle h ;
  equal int 0 (H.live_timers h)

(* 1,000 changes of the key of a Sub.run: one key and one token at the end. *)
let sub_keys () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m _ -> (m + 1, Cmd.none))
         ~view:(fun m -> button Save (string_of_int m))
         ~subs:(fun m ->
           Sub.run ~key:(string_of_int m) (fun _ ->
               Async.make (fun _ ~ok:_ ~error:_ -> ()) ) )
         () )
  in
  for _ = 1 to 1000 do
    click p
  done ;
  equal int 1 (ins p).token_children ;
  equal int 1 (ins p).sub_keys ;
  P.stop p ;
  equal int 0 (ins p).token_children ;
  no_errors p

(* 1,000 times a start, a click and a stop with a Sub.every on one host. *)
let every_starts () =
  let h = H.create () in
  let app =
    App.make
      ~init:(fun () -> (0, Cmd.none))
      ~update:(fun m _ -> (m + 1, Cmd.none))
      ~view:(fun m -> button Save (string_of_int m))
      ~subs:(fun m -> Sub.every (Span.ms (1 + (m mod 2))) Tick)
      ()
  in
  for _ = 1 to 1000 do
    let p = P.start ~host:h app in
    click p ; P.stop p
  done ;
  equal int 0 (H.live_timers h) ;
  equal int 0 (List.length (H.errors h)) ;
  H.idle h ;
  equal int 0 (H.live_timers h)

exception Boom

(* The model of a root that failed is free. *)
let model_free () =
  let w = Weak.create 1 in
  let p =
    P.start
      (App.make
         ~init:(fun () ->
           let m = Bytes.make 64 'x' in
           Weak.set w 0 (Some m) ; (m, Cmd.none) )
         ~update:(fun _ _ -> raise Boom)
         ~view:(fun m -> button Save (string_of_int (Bytes.length m)))
         () )
  in
  Gc.full_major () ;
  is_true ~msg:"live before" (Weak.check w 0) ;
  click p ;
  is_false (ins p).alive ;
  Gc.full_major () ;
  is_false ~msg:"free after the catch" (Weak.check w 0) ;
  equal int 1 (List.length (errors p))

(* A child with a task of 1 s and a subscription of 2 s: two timers. *)
module Timed = struct
  type props = int

  type model = int

  type msg = unit

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init n = (n, Cmd.task (Async.sleep (Span.s 1)))

  let update _ _ m () = (m + 1, Cmd.none)

  let view _ m = El.span [txt (string_of_int m)]

  let subs _ _ = Sub.every (Span.s 2) ()

  let equal_props = Int.equal
end

let timed n = Component.v (module Timed) ~on:(function _ -> .) n

let toggle_app =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m _ -> (m + 1, Cmd.none))
    ~view:(fun m ->
      El.div
        [ button Save (string_of_int m)
        ; (if m mod 2 = 0 then timed m else txt "") ] )
    ()

(* 100 mounts and unmounts of a child with two timers. *)
let child_mounts () =
  let h = H.create () in
  let p = P.start ~host:h toggle_app in
  for _ = 1 to 200 do
    click p
  done ;
  let t = P.tree p in
  equal int ~msg:"the last mount" 1 t.instances ;
  equal int ~msg:"its timers" 2 (H.live_timers h) ;
  click p ;
  let t = P.tree p in
  equal int ~msg:"timers" 0 (H.live_timers h) ;
  equal int ~msg:"tokens" 0 t.tokens ;
  equal int ~msg:"instances" 0 t.instances ;
  equal int ~msg:"child links" 0 t.child_links ;
  equal int ~msg:"queued" 0 t.queued ;
  no_errors p

(* A live parent replaces 10 rows with new keys 10 times. *)
let rows_app =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m _ -> (m + 1, Cmd.none))
    ~view:(fun m ->
      El.div
        [ button Save (string_of_int m)
        ; View.keyed El.ul
            (List.init 10 (fun i -> (Key.int ((m * 10) + i), timed i))) ] )
    ()

let row_links () =
  let h = H.create () in
  let p = P.start ~host:h rows_app in
  for _ = 1 to 10 do
    click p ;
    let t = P.tree p in
    equal int ~msg:"instances" 10 t.instances ;
    equal int ~msg:"no dead child link" 10 t.child_links ;
    equal int
      ~msg:"tokens: an instance, a task, a key and their steps for each row" 50
      t.tokens
  done ;
  equal int 20 (H.live_timers h) ;
  P.stop p ;
  equal int 0 (H.live_timers h) ;
  no_errors p

(* A live owner replaces 100 boundary rows 10 times, in a boundary that stays:
   each list has no dead entry. With [own], each row has its own boundary. *)
let boundary_rows own () =
  let h = H.create () in
  let row i =
    if own then View.boundary ~fallback:(fun _ -> txt "") (timed i) else timed i
  in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m _ -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             [ button Save (string_of_int m)
             ; View.boundary
                 ~fallback:(fun _ -> txt "")
                 (View.keyed El.ul
                    (List.init 100 (fun i -> (Key.int ((m * 100) + i), row i))) )
             ] )
         () )
  in
  for _ = 1 to 10 do
    click p ;
    let t = P.tree p in
    equal int ~msg:"instances" 100 t.instances ;
    equal int ~msg:"child links" 100 t.child_links ;
    equal int ~msg:"owned links" (if own then 101 else 1) t.owned_links ;
    equal int ~msg:"member links" 100 t.member_links ;
    equal int ~msg:"nested links" (if own then 100 else 0) t.nested_links
  done ;
  P.stop p ;
  equal int 0 (H.live_timers h) ;
  no_errors p

(* A page of 20 cards, each with its own boundary, in a boundary that stays. *)
module Cards = struct
  type props = int

  type model = unit

  type msg = unit

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () () = ((), Cmd.none)

  let view n () =
    El.div
      (List.init 20 (fun i ->
           View.boundary ~fallback:(fun _ -> txt "") (timed ((n * 20) + i)) ) )

  let subs _ _ = Sub.none

  let equal_props = Int.equal
end

(* 10 pages of 20 cards: the unmount of a page unlinks its boundaries from the
   nested list of the outer boundary. *)
let card_pages () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m _ -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             [ button Save (string_of_int m)
             ; View.boundary
                 ~fallback:(fun _ -> txt "")
                 ( if m <= 10 then
                     Component.v
                       (module Cards)
                       ~key:(string_of_int m)
                       ~on:(function _ -> .)
                       m
                   else txt "" ) ] )
         () )
  in
  for _ = 1 to 10 do
    click p ;
    let t = P.tree p in
    equal int ~msg:"nested links" 20 t.nested_links ;
    equal int ~msg:"owned links" 21 t.owned_links ;
    equal int ~msg:"instances" 21 t.instances
  done ;
  click p ;
  let t = P.tree p in
  equal int ~msg:"no nested entry" 0 t.nested_links ;
  equal int ~msg:"one boundary" 1 t.owned_links ;
  equal int 0 t.member_links ;
  equal int 0 t.instances ;
  no_errors p

(* A child with a button. *)
module Waits = struct
  type props = unit

  type model = unit

  type msg = unit

  type out = |

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init () = ((), Cmd.none)

  let update _ () () () = ((), Cmd.none)

  let view () () = El.button ~at:[At.data "testid" "w"] ~ev:[Ev.click ()] []

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

(* A dead instance that a DOM node keeps (through the post of its cell) does not
   keep its boundary: the fallback of the boundary is free. *)
let boundary_free () =
  let w = Weak.create 1 in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m _ -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             [ button Save (string_of_int m)
             ; ( if m = 0 then (
                   let b = Bytes.make 64 'x' in
                   Weak.set w 0 (Some b) ;
                   View.boundary
                     ~fallback:(fun _ -> txt (Bytes.to_string b))
                     (Component.v (module Waits) ~on:(function _ -> .) ()) )
                 else txt "" ) ] )
         () )
  in
  let node = P.find p (By.test_id "w") in
  Gc.full_major () ;
  is_true ~msg:"live before" (Weak.check w 0) ;
  click p ;
  Gc.full_major () ;
  is_false ~msg:"free after the removal" (Weak.check w 0) ;
  is_true ~msg:"the node stays"
    (Option.is_none (F.parent (Sys.opaque_identity node))) ;
  no_errors p

let tests =
  [ group "leaks"
      [ test "1,000 keys with synchronous results leave an empty table" sync_keys
      ; test "1,000 keys that wait leave an empty table after idle" pending_keys
      ; test "1,000 starts, clicks and stops leave no timer" starts
      ; test "1,000 changes of a Sub.run key leave one key" sub_keys
      ; test "1,000 starts, clicks and stops with Sub.every leave no timer"
          every_starts
      ; test "the model of a root that failed is free" model_free
      ; test "100 mounts and unmounts of a child with timers leave nothing"
          child_mounts
      ; test "a live parent replaces 10 rows 10 times: no dead child link"
          row_links
      ; test "a live owner replaces 100 boundary rows 10 times: no dead entry"
          (boundary_rows true)
      ; test "a live boundary replaces 100 rows 10 times: no dead member"
          (boundary_rows false)
      ; test "10 pages of 20 cards, each with a boundary: 0 nested entries"
          card_pages
      ; test "a dead instance does not keep its boundary" boundary_free ] ]
