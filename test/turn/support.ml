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
let quiet_with h ~inspect ~stats =
  ( match H.idle ~limit:100_000 h with
  | () ->
      ()
  | exception Failure _ ->
      fail "idle does not end" ) ;
  is_false ~msg:"no pending render" (inspect ()).R.pending ;
  is_false ~msg:"no scheduled render" (inspect ()).scheduled ;
  let renders = (stats ()).R.renders and turns = (stats ()).turns in
  H.advance h (Span.ms 200) ;
  equal int ~msg:"no render after idle" renders (stats ()).renders ;
  equal int ~msg:"no turn after idle" turns (stats ()).turns

let quiet p =
  quiet_with (P.host p) ~inspect:(fun () -> ins p) ~stats:(fun () -> stats p)

(* The DOM of a fresh mount of [v]. *)
let fresh v =
  let root = F.element "div" in
  let (_ : (F.node, _) Private.Diff.t) =
    Private.Diff.mount
      (F.ops (F.create ()))
      ~parent:root
      ~post:(Private.Dom.post_now ignore)
      v
  in
  F.dump root

(* A fault of the platform in a re-entrant event (see Runtime.catch): the
   function of Runtime.event raises inside a turn, for example a payload read
   that throws. The root catch comes at once, inside the user code that sends
   the event. Page has no such fault, thus these tests use the runtime alone.
   [update], [view] and [subs] get the function that sends the fault. *)
exception Fault

type faulty = {h: H.t; root: F.node; rt: F.node R.t}

let faulty ?subs ~update ~view ?fallback () =
  let h = H.create () and root = F.element "div" in
  let self = ref None in
  let fault () =
    Option.iter (fun rt -> R.event rt R.Discrete (fun () -> raise Fault)) !self
  in
  let rt =
    R.start (H.env h)
      (F.ops (F.create ()))
      ~parent:root
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(update fault) ~view:(view fault)
         ?subs:(Option.map (fun f -> f fault) subs)
         ?fallback () )
  in
  self := Some rt ;
  {h; root; rt}

(* A click on the first element of the view. *)
let fclick f =
  let b = List.hd (F.children f.root) in
  R.event f.rt R.Discrete (fun () -> F.fire b "click" Private.Dom.Unit ())

(* As [quiet], for the runtime alone. *)
let fquiet f =
  quiet_with f.h
    ~inspect:(fun () -> R.inspect f.rt)
    ~stats:(fun () -> R.stats f.rt)

let same_nodes a b = List.length a = List.length b && List.for_all2 ( == ) a b

(* The outputs of a component with no outputs. *)
type none = |

let never : none -> 'a = function _ -> .

(* A leaf component with no model and no message: its props, the commands of its
   [init] and its view. *)
module type LEAF = sig
  type props

  val init : props -> unit Cmd.t

  val view : props -> unit View.t
end

module Leaf (L : LEAF) = struct
  include Component.Defaults

  type props = L.props

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init p = ((), L.init p)

  let update _ _ () () = ((), Cmd.none)

  let view p () = L.view p
end

(* [one_render p f] runs [f], and checks that [f] made one render step. *)
let one_render p f =
  let r = (stats p).renders in
  f () ;
  equal int ~msg:"one render" (r + 1) (stats p).renders
