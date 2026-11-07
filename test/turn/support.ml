(* Helpers for the tests of the turn: small apps on the fake DOM and the virtual
   host. *)

open Windtrap
open Kilim
module H = Kilim_testing.Host
module P = Kilim_testing.Page
module F = Kilim_testing.Fake_dom
module By = Kilim_testing.By
module R = Private.Runtime

let stats = P.stats

let ins = P.inspect

let errors p = H.errors (P.host p)

let no_errors p = equal int ~msg:"no error" 0 (List.length (errors p))

let button ?(id = "b") msg label =
  El.button ~at:[At.data "testid" id] ~ev:[Ev.click msg] [txt label]

(* A task that waits: the test keeps its [ok]. A raw send is a raw callback that
   calls such an [ok] with the message. *)
let waiting (q : ('a -> unit) Queue.t) : 'a Async.t =
  Async.make (fun _ ~ok ~error:_ -> Queue.push ok q)

(* A task with a synchronous result from a callback. *)
let sync x = Async.make (fun _ ~ok ~error:_ -> ok x)

(* The counter: a click starts a task, and its result adds to the model. *)
type cmsg = Clicked | Got of int

let counter ?(task = fun () -> Async.return 1) () =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m -> function
      | Clicked ->
          (m, Cmd.task (Async.map (fun n -> Got n) (task ())))
      | Got n ->
          (m + n, Cmd.none) )
    ~view:(fun m -> button Clicked (string_of_int m))
    ()

let click p = P.click p (By.test_id "b")

(* The checks after each top-level action of a test. *)
let settled p =
  let i = ins p in
  is_false ~msg:"in_turn" i.in_turn ;
  is_true ~msg:"pending implies scheduled" ((not i.pending) || i.scheduled) ;
  at_most int ~msg:"max renders" ~than:3 (stats p).max_renders

(* The host reaches idle, and no render stays pending: a later frame renders
   nothing. *)
let quiet p =
  let h = P.host p in
  ( match H.idle ~limit:100_000 h with
  | () ->
      ()
  | exception Failure _ ->
      fail "idle does not end" ) ;
  is_false ~msg:"no pending render" (ins p).pending ;
  is_false ~msg:"no scheduled render" (ins p).scheduled ;
  let renders = (stats p).renders and turns = (stats p).turns in
  P.advance p (Span.ms 200) ;
  equal int ~msg:"no render after idle" renders (stats p).renders ;
  equal int ~msg:"no turn after idle" turns (stats p).turns

(* The DOM of a fresh mount of [v]. *)
let fresh v =
  let root = F.element "div" in
  let (_ : (F.node, _) Private.Diff.t) =
    Private.Diff.mount (F.ops (F.create ())) ~parent:root ~dispatch:ignore v
  in
  F.dump root
