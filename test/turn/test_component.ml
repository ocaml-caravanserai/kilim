(* The instance tree (design sections 09 and 11): identity, keys, props, the
   dirty order, the tokens, the alive flag, the unmount and the root catch of a
   component. *)

open Windtrap
open Kilim
open Support
module C = Private.Component
module V = Private.View
module I = Private.Instance

type none = |

let never : none -> 'a = function _ -> .

(* A counter. A click adds [step], and [Set] sets the model. The label is the
   test id of its button. *)
module Counter = struct
  type props = {label: string; step: int}

  type model = int

  type msg = Incr | Set of int

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let inits = ref 0

  let init _ = incr inits ; (0, Cmd.none)

  let update _ p m = function
    | Incr ->
        (m + p.step, Cmd.none)
    | Set n ->
        (n, Cmd.none)

  let view p m = button ~id:p.label Incr (p.label ^ "=" ^ string_of_int m)

  let subs _ _ = Sub.none

  let equal_props = Component.shallow_equal
end

let counter ?key ?(step = 1) label =
  C.v (module Counter) ?key ~on:never {Counter.label; step}

let views p = (stats p).views

let text p id = F.text (P.find p (By.test_id id))

let tree = P.tree

type pmsg = Bump | Other

(* A parent with an int model: a click on "p" adds 1. *)
let with_int ?fallback view =
  P.start
    (App.make
       ~init:(fun () -> (0, Cmd.none))
       ~update:(fun m -> function
         | Bump -> (m + 1, Cmd.none) | Other -> (m, Cmd.none) )
       ~view:(fun m -> El.div [button ~id:"p" Bump (string_of_int m); view m])
       ?fallback () )

let bump p = P.click p (By.test_id "p")

(* The DOM node of the view of the parent after its button. *)
let second p = List.nth (F.children (List.hd (F.children (P.root p)))) 1

(* The hooks before the first call of Component.v of the tests. *)
let default_hooks = !Private.Diff.hooks

let default_ext = !Private.Instance.ext

let inside () =
  (* Component.v installs the hooks of the component nodes. *)
  Private.Diff.hooks := default_hooks ;
  Private.Instance.ext := default_ext ;
  let p = with_int (fun _ -> counter "a") in
  equal int ~msg:"the view of the parent and of the child" 2 (views p) ;
  equal string
    "<div><div><button data-testid=\"p\">0</button><button \
     data-testid=\"a\">a=0</button></div></div>"
    (P.html p) ;
  equal int 1 (tree p).instances ;
  equal int 1 (tree p).child_links ;
  settled p ;
  no_errors p

(* With no component, the hooks stay the default: the root never goes into the
   work queue. *)
let no_hooks () =
  Private.Diff.hooks := default_hooks ;
  Private.Instance.ext := default_ext ;
  let p =
    P.start
      (App.make
         ~init:(fun () -> (2, Cmd.none))
         ~update:(fun m -> function
           | Bump -> (m - 1, Cmd.none) | Other -> (m, Cmd.none) )
         ~view:(fun m ->
           El.div
             [ button ~id:"p" Bump (string_of_int m)
             ; button ~id:"o" Other "o"
             ; El.ul (List.init m (fun i -> El.li [txt (string_of_int i)])) ] )
         () )
  in
  bump p ;
  bump p ;
  equal string "0" (text p "p") ;
  let r = (stats p).renders in
  P.click p (By.test_id "o") ;
  equal int ~msg:"a render step with no write" r (stats p).renders ;
  equal int 0 (tree p).queued ;
  no_errors p ;
  quiet p ;
  P.stop p ;
  equal string "<div></div>" (P.html p)

(* A change of the root in a turn that does not render: the root renders through
   its own path, and the work queue has only the instances below it. *)
let root_not_queued () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             [ El.span
                 ~at:[At.data "testid" "r"]
                 ~ev:[Ev.mouseenter ()]
                 [txt (string_of_int m)]
             ; counter "a" ] )
         () )
  in
  P.event p (By.test_id "r") "mouseenter" ;
  is_true ~msg:"a pending render" (ins p).pending ;
  equal int ~msg:"the root is not in the queue" 0 (tree p).queued ;
  P.advance p (Span.ms 20) ;
  equal string "1" (text p "r")

let separate () =
  let p = with_int (fun _ -> El.div [counter "a"; counter ~step:10 "b"]) in
  P.click p (By.test_id "a") ;
  P.click p (By.test_id "b") ;
  P.click p (By.test_id "a") ;
  equal string "a=2" (text p "a") ;
  equal string "b=10" (text p "b") ;
  no_errors p

let alone () =
  let p = with_int (fun _ -> El.div [counter "a"; counter "b"]) in
  let v = views p and r = (stats p).renders in
  P.click p (By.test_id "a") ;
  equal int ~msg:"only the view of the child" (v + 1) (views p) ;
  equal int ~msg:"one render" (r + 1) (stats p).renders ;
  equal string "a=1" (text p "a") ;
  equal int 0 (tree p).queued ;
  settled p ;
  quiet p

let skip () =
  let p = with_int (fun _ -> counter "a") in
  let v = views p in
  bump p ;
  equal int ~msg:"the parent view only" (v + 1) (views p) ;
  equal string "1" (text p "p")

let new_props () =
  let p = with_int (fun m -> counter ~step:(m + 1) "a") in
  P.click p (By.test_id "a") ;
  let v = views p in
  bump p ;
  equal int ~msg:"the parent and the child" (v + 2) (views p) ;
  P.click p (By.test_id "a") ;
  equal string ~msg:"the model stays, the new step" "a=3" (text p "a")

(* [equal_props] compares only the parity of the props. *)
module Parity = struct
  type props = int

  type model = int

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update _ _ m () = (m + 1, Cmd.none)

  let view n _ =
    El.span ~at:[At.data "testid" "n"] ~ev:[Ev.click ()] [txt (string_of_int n)]

  let subs _ _ = Sub.none

  let equal_props a b = a mod 2 = b mod 2
end

(* The props at each value of the parent model. *)
let parities = [|0; 2; 1; 3; 5; 4; 6|]

let custom_equal () =
  let p = with_int (fun m -> C.v (module Parity) ~on:never parities.(m)) in
  let shown () = List.init 5 (fun _ -> bump p ; text p "n") in
  equal (list string) ["0"; "1"; "1"; "1"; "4"] (shown ()) ;
  (* Equal props stay out of the instance: a render alone uses the props of the
     last patch that found them different. *)
  bump p ;
  equal string "4" (text p "n") ;
  P.click p (By.test_id "n") ;
  equal string "4" (text p "n")

let key_reset () =
  let p = with_int (fun m -> counter ~key:(string_of_int (m / 2)) "a") in
  let n = !Counter.inits in
  P.click p (By.test_id "a") ;
  bump p ;
  equal string ~msg:"the same key" "a=1" (text p "a") ;
  bump p ;
  equal string ~msg:"a new key: a new model" "a=0" (text p "a") ;
  equal int ~msg:"init runs again" (n + 1) !Counter.inits ;
  equal int 1 (tree p).instances ;
  equal int 1 (tree p).child_links

let rows m =
  let l = [("a", 1); ("b", 2); ("c", 3)] in
  View.keyed El.ul
    (List.map
       (fun (s, k) -> (Key.int k, counter s))
       (if m = 0 then l else List.rev l) )

let keyed_move () =
  let p = with_int rows in
  P.click p (By.test_id "b") ;
  P.click p (By.test_id "c") ;
  P.click p (By.test_id "c") ;
  let n = !Counter.inits in
  let before = F.children (second p) in
  bump p ;
  equal int ~msg:"no remount" n !Counter.inits ;
  equal (list string) ["c=2"; "b=1"; "a=0"]
    (List.map F.text (F.children (second p))) ;
  is_true ~msg:"the DOM nodes move"
    (same_nodes (List.rev before) (F.children (second p))) ;
  no_errors p

module Counter2 = struct
  include Counter

  let id : (props, model, msg, out) C.id = C.id ()
end

let module_change () =
  let p =
    with_int (fun m ->
        if m = 0 then counter "a"
        else C.v (module Counter2) ~on:never {Counter.label= "a"; step= 1} )
  in
  P.click p (By.test_id "a") ;
  let n = !Counter.inits in
  bump p ;
  equal int ~msg:"init of the new module" (n + 1) !Counter.inits ;
  equal string "a=0" (text p "a") ;
  equal int 1 (tree p).instances

let position_key () =
  let p = with_int (fun m -> El.div [counter ~key:(string_of_int m) "a"]) in
  P.click p (By.test_id "a") ;
  bump p ;
  equal string ~msg:"a new key outside View.keyed remounts" "a=0" (text p "a") ;
  equal int 1 (tree p).instances

let local_module () =
  let p =
    with_int (fun _ ->
        let module L = struct
          include Counter

          let id : (props, model, msg, out) C.id = C.id ()
        end in
        C.v (module L) ~on:never {Counter.label= "a"; step= 1} )
  in
  P.click p (By.test_id "a") ;
  equal string ~msg:"a click renders the child alone" "a=1" (text p "a") ;
  bump p ;
  equal string ~msg:"a new id at each render: a remount" "a=0" (text p "a")

(* Two modules with one id and different views. *)
module Shared = struct
  include Counter

  let view p m = button ~id:p.label Incr (p.label ^ ":" ^ string_of_int m)
end

let shared () = C.v (module Shared) ~on:never {Counter.label= "a"; step= 1}

let shared_id () =
  let p = with_int (fun m -> if m mod 2 = 0 then counter "a" else shared ()) in
  P.click p (By.test_id "a") ;
  let n = !Counter.inits in
  bump p ;
  P.click p (By.test_id "a") ;
  equal string ~msg:"the view of the new module" "a:2" (text p "a") ;
  bump p ;
  P.click p (By.test_id "a") ;
  equal string ~msg:"the view of the first module" "a=3" (text p "a") ;
  equal int ~msg:"no remount" n !Counter.inits

(* The spec of the last call of Component.v: the same module gives the same
   spec, another module with the same id a new spec. *)
let spec_cache () =
  match (counter "a", counter "b", shared ()) with
  | V.Comp (V.C a), V.Comp (V.C b), V.Comp (V.C c) -> (
    match
      ( Type.Id.provably_equal a.spec.id.tid b.spec.id.tid
      , Type.Id.provably_equal a.spec.id.tid c.spec.id.tid )
    with
    | Some Type.Equal, Some Type.Equal ->
        is_true ~msg:"the same module" (a.spec == b.spec) ;
        is_false ~msg:"another module" (a.spec == c.spec)
    | _ ->
        fail "one id" )
  | _ ->
      fail "component nodes"

let element_change () =
  let p =
    with_int (fun m ->
        if m = 0 then El.div [El.p [counter "a"]]
        else El.div [El.span [counter "a"]] )
  in
  P.click p (By.test_id "a") ;
  bump p ;
  equal string "a=0" (text p "a") ;
  equal int 1 (tree p).instances ;
  equal int 1 (tree p).child_links

(* A child with a task that waits: it has a token. *)
module Waiter = struct
  type props = unit

  type model = int

  type msg = Got of int

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let pending : (msg -> unit) Queue.t = Queue.create ()

  let init () = (0, Cmd.task (waiting pending))

  let update _ () _ (Got n) = (n, Cmd.none)

  let view () m = El.span [txt ("w" ^ string_of_int m)]

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let waiter () = C.v (module Waiter) ~on:never ()

let removed () =
  Queue.clear Waiter.pending ;
  let p = with_int (fun m -> if m = 0 then waiter () else txt "none") in
  equal int ~msg:"the tokens of the instance, its task and its step" 3
    (tree p).tokens ;
  equal int ~msg:"the child token is under the root token" 1
    (ins p).token_children ;
  let ok = Queue.pop Waiter.pending in
  bump p ;
  equal int ~msg:"cancelled in the render" 0 (tree p).tokens ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).child_links ;
  let n = (stats p).messages in
  ok (Waiter.Got 5) ;
  P.run p ;
  equal int ~msg:"a late result does nothing" n (stats p).messages ;
  equal string "none" (F.text (second p)) ;
  quiet p

let no_token () =
  let p = with_int (fun m -> if m = 0 then counter "a" else txt "x") in
  let b = P.find p (By.test_id "a") in
  bump p ;
  let d = (stats p).dropped and n = (stats p).messages in
  (* The cell of the removed button keeps the post of the dead instance. *)
  F.fire b "click" Private.Dom.Unit () ;
  P.run p ;
  equal int ~msg:"dropped" (d + 1) (stats p).dropped ;
  equal int ~msg:"no update" n (stats p).messages

(* A child whose init task has a synchronous result. *)
module Sync = struct
  type props = unit

  type model = string

  type msg = Done

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ("init", Cmd.task (sync Done))

  let update _ () _ Done = ("done", Cmd.none)

  let view () m = El.span ~at:[At.data "testid" "s"] [txt m]

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let init_tasks () =
  let p =
    with_int (fun m -> if m = 0 then txt "" else C.v (module Sync) ~on:never ())
  in
  let r = (stats p).renders and n = (stats p).messages in
  bump p ;
  equal int ~msg:"the click and the result" (n + 2) (stats p).messages ;
  equal int ~msg:"the mount, then the render of the result" (r + 2)
    (stats p).renders ;
  equal string "done" (text p "s") ;
  settled p ;
  quiet p

let dead_token () =
  let env : F.node I.env =
    { post= (fun _ -> Private.Dom.post_now ignore)
    ; command= (fun _ _ -> ())
    ; viewed= ignore
    ; serial= 1
    ; work= I.Work.empty
    ; passes= true
    ; stale= []
    ; touched= [] }
  in
  let root : (F.node, unit) I.t = I.root (Private.Cancel.root ()) in
  let a : (F.node, unit) I.t = I.child env root in
  let b : (F.node, unit) I.t = I.child env a in
  is_false ~msg:"a live child" (Cancel.cancelled (I.token b)) ;
  equal int ~msg:"the ancestors first" 1
    (Private.Cancel.children (I.token root)) ;
  let c : (F.node, unit) I.t = I.child env a in
  I.unmount env a ;
  is_false ~msg:"no child stays" (Private.Cancel.children (I.token root) > 0) ;
  is_true ~msg:"a dead child" (Cancel.cancelled (I.token c)) ;
  let d : (F.node, unit) I.t = I.child env root in
  I.unmount env d ;
  is_true ~msg:"a dead instance with no token" (Cancel.cancelled (I.token d)) ;
  let e : (F.node, unit) I.t = I.child env d in
  is_true ~msg:"a child of a dead instance" (Cancel.cancelled (I.token e))

exception Boom

(* A component in the root fallback: its owner is the dead root. *)
module Late = struct
  type props = unit

  type model = int

  type msg = Got

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (0, Cmd.task (Async.return Got))

  let update _ () m Got = (m + 1, Cmd.none)

  let view () m = El.span ~at:[At.data "testid" "late"] [txt (string_of_int m)]

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let late () = C.v (module Late) ~on:never ()

let fallback_component () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun _ _ -> raise Boom)
         ~view:(fun m -> El.div [button ~id:"p" () (string_of_int m); late ()])
         ~fallback:(fun _ -> El.div [txt "failed"; late ()])
         () )
  in
  equal string ~msg:"the task of the live view" "1" (text p "late") ;
  bump p ;
  equal string ~msg:"a new instance in the fallback" "0" (text p "late") ;
  equal int ~msg:"its task has a cancelled token" 0 (tree p).tokens ;
  P.run p ;
  equal string ~msg:"its task never starts" "0" (text p "late") ;
  equal int 1 (List.length (errors p))

(* A component in the root fallback mounts dead: a click does not run its
   update, and it is in no list. *)
let fallback_click () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun _ _ -> raise Boom)
         ~view:(fun m -> button ~id:"p" () (string_of_int m))
         ~fallback:(fun _ -> counter "fb")
         () )
  in
  bump p ;
  equal string "fb=0" (text p "fb") ;
  let n = (stats p).messages in
  P.click p (By.test_id "fb") ;
  equal int ~msg:"no update" n (stats p).messages ;
  equal string "fb=0" (text p "fb") ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).child_links ;
  P.stop p ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).child_links ;
  equal int 1 (List.length (errors p))

let rootless_fallback () =
  (* The root has no token at the catch: the dead root gives a cancelled
     token. *)
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun _ _ -> raise Boom)
         ~view:(fun m -> button ~id:"p" () (string_of_int m))
         ~fallback:(fun _ -> late ())
         () )
  in
  equal int 0 (ins p).token_children ;
  bump p ;
  P.run p ;
  equal string "0" (text p "late") ;
  equal int 0 (tree p).tokens

(* The unmount clears the model of the instance and its DOM parent, also while a
   task keeps the instance. *)
module Heavy = struct
  type props = unit

  type model = Bytes.t

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let model = Weak.create 1

  let pending : (unit -> unit) Queue.t = Queue.create ()

  let init () =
    let m = Bytes.make 64 'x' in
    Weak.set model 0 (Some m) ;
    (m, Cmd.task (waiting pending))

  let update _ () m () = (m, Cmd.none)

  let view () m = El.span [txt (string_of_int (Bytes.length m))]

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let weak_unmount () =
  Queue.clear Heavy.pending ;
  let box = Weak.create 1 in
  let p =
    with_int (fun m ->
        if m = 0 then
          El.div ~at:[At.data "testid" "box"] [C.v (module Heavy) ~on:never ()]
        else txt "" )
  in
  Weak.set box 0 (Some (P.find p (By.test_id "box"))) ;
  Gc.full_major () ;
  is_true ~msg:"live before" (Weak.check Heavy.model 0 && Weak.check box 0) ;
  bump p ;
  equal int ~msg:"the task waits" 1 (Queue.length Heavy.pending) ;
  Gc.full_major () ;
  is_false ~msg:"the model is free" (Weak.check Heavy.model 0) ;
  is_false ~msg:"the DOM parent is free" (Weak.check box 0) ;
  no_errors p

(* A cancelled task with a pending callback: the continuation of the task, and
   thus the data of the instance that it holds, are free. *)
let weak_task () =
  let pending : (unit -> unit) Queue.t = Queue.create () in
  let data = Weak.create 1 in
  let module Holder = struct
    type props = unit

    type model = unit

    type msg = int

    type out = none

    let id : (props, model, msg, out) C.id = C.id ()

    let init () =
      let big = Bytes.make 64 'x' in
      Weak.set data 0 (Some big) ;
      ((), Cmd.task (Async.map (fun () -> Bytes.length big) (waiting pending)))

    let update _ () () _ = ((), Cmd.none)

    let view () () = txt "holder"

    let subs _ _ = Sub.none

    let equal_props _ _ = true
  end in
  let p =
    with_int (fun m ->
        if m = 0 then C.v (module Holder) ~on:never () else txt "" )
  in
  Gc.full_major () ;
  is_true ~msg:"live before" (Weak.check data 0) ;
  bump p ;
  equal int ~msg:"the callback waits" 1 (Queue.length pending) ;
  Gc.full_major () ;
  is_false ~msg:"free after the unmount" (Weak.check data 0) ;
  no_errors p

(* A child with a controlled value: a render alone writes the property. *)
module Typed = struct
  type props = unit

  type model = string

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ("a", Cmd.none)

  let update _ () m () = (m ^ "b", Cmd.none)

  let view () m =
    El.div
      [El.input ~at:[At.data "testid" "i"; At.value m] (); button ~id:"t" () "t"]

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let controlled_alone () =
  let p = with_int (fun _ -> C.v (module Typed) ~on:never ()) in
  P.click p (By.test_id "t") ;
  equal string "ab" (F.value (P.find p (By.test_id "i")))

(* The parent and the child are dirty in one turn. *)
let dirty_both () =
  let send = Queue.create () in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m -> function
           | `Both ->
               (m + 1, Cmd.task (Async.return `Child))
           | `Child ->
               Queue.iter (fun f -> f ()) send ;
               (m, Cmd.none) )
         ~view:(fun m ->
           El.div [button ~id:"p" `Both (string_of_int m); counter "a"] )
         () )
  in
  (* The click of the child, then the parent: the child is dirty when the parent
     renders. *)
  let child = P.find p (By.test_id "a") in
  Queue.push (fun () -> F.fire child "click" Private.Dom.Unit ()) send ;
  let v = views p in
  bump p ;
  equal string "a=1" (text p "a") ;
  equal int ~msg:"one view of the child" (v + 2) (views p) ;
  equal int 0 (tree p).queued ;
  quiet p

(* An instance that records the order of its views. *)
let order = ref []

(* The component of the child of a tracer: a cycle with no recursive module. *)
let below : (string -> unit View.t) ref = ref (fun _ -> txt "")

module Tracer = struct
  type props = string

  type model = int

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update _ _ m () = (m + 1, Cmd.none)

  let view name m =
    order := name :: !order ;
    El.div
      [ button ~id:name () (name ^ string_of_int m)
      ; (if String.length name < 3 then !below (name ^ "x") else txt "") ]

  let subs _ _ = Sub.none

  let equal_props = String.equal
end

let () = below := fun name -> C.v (module Tracer) ~on:never name

(* The click of "p" fires the clicks of [ids] inside its update: they only go to
   the queue. *)
let fire_in_turn ids =
  let self = ref None in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () ->
           let p = Option.get !self in
           List.iter
             (fun id ->
               F.fire (P.find p (By.test_id id)) "click" Private.Dom.Unit () )
             !ids ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             [ button ~id:"p" () "p"
             ; C.v (module Tracer) ~on:never "a"
             ; C.v (module Tracer) ~on:never "b" ] )
         () )
  in
  self := Some p ;
  p

let from_top () =
  let ids = ref [] in
  order := [] ;
  let p = fire_in_turn ids in
  equal (list string) ~msg:"the mount"
    ["a"; "ax"; "axx"; "b"; "bx"; "bxx"]
    (List.rev !order) ;
  let run l =
    ids := l ;
    order := [] ;
    bump p ;
    List.rev !order
  in
  equal (list string) ~msg:"the depth first" ["b"; "axx"] (run ["axx"; "b"]) ;
  equal (list string) ~msg:"then the order of the mount" ["ax"; "bx"]
    (run ["bx"; "ax"]) ;
  equal (list string) ~msg:"a parent renders its dirty child" ["a"; "ax"; "bx"]
    (run ["ax"; "bx"; "a"]) ;
  equal string "bx2" (text p "bx") ;
  equal int 0 (tree p).queued ;
  quiet p

(* A child whose root element changes with its model: a click toggles a [p] and
   a [span]. [Ref] makes a chain: the view of a [Ref] is a [Morph]. *)
module Morph = struct
  type props = string

  type model = bool

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (false, Cmd.none)

  let update _ _ m () = (not m, Cmd.none)

  let view name m = (if m then El.span else El.p) [button ~id:name () name]

  let subs _ _ = Sub.none

  let equal_props = String.equal
end

let morph name = C.v (module Morph) ~on:never name

module Ref = struct
  type props = string

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () () = ((), Cmd.none)

  let view name () = morph name

  let subs _ _ = Sub.none

  let equal_props = String.equal
end

let chain name = C.v (module Ref) ~on:never name

let names l = List.map F.name l

let replace_root () =
  let p = with_int (fun _ -> El.div [morph "a"; txt "end"]) in
  let box = second p in
  P.click p (By.test_id "a") ;
  equal (list string) ["span"; "#text"] (names (F.children box)) ;
  P.click p (By.test_id "a") ;
  equal (list string) ["p"; "#text"] (names (F.children box)) ;
  no_errors p

(* Decision 17: the parent finds the new root of a child that rendered alone,
   for a keyed move, an insert before it and a removal. *)
let transparent ~per make () =
  let lists = [|[1; 2; 3]; [3; 1; 2]; [0; 3; 1; 2]; [0; 1; 2]; []|] in
  let p =
    with_int (fun m ->
        View.keyed El.ul
          (List.map
             (fun k -> (Key.int k, make (string_of_int k)))
             lists.(min m 4) ) )
  in
  let check l =
    equal (list string) (List.map string_of_int l)
      (List.map F.text (F.children (second p))) ;
    List.iter
      (fun k ->
        let b = P.find p (By.test_id (string_of_int k)) in
        is_true ~msg:"the button is in the row"
          ( match Option.bind (F.parent b) F.parent with
          | Some r ->
              r == second p
          | None ->
              false ) )
      l
  in
  P.click p (By.test_id "3") ;
  P.click p (By.test_id "1") ;
  equal (list string) ["span"; "p"; "span"] (names (F.children (second p))) ;
  bump p ;
  check [3; 1; 2] ;
  bump p ;
  check [0; 3; 1; 2] ;
  bump p ;
  check [0; 1; 2] ;
  equal int ~msg:"the instances" (3 * per) (tree p).instances ;
  bump p ;
  check [] ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).child_links ;
  no_errors p

let nested () =
  let p = with_int (fun m -> if m = 0 then chain "a" else txt "x") in
  equal int ~msg:"two levels" 2 (tree p).instances ;
  P.click p (By.test_id "a") ;
  equal string "span" (F.name (second p)) ;
  bump p ;
  equal int ~msg:"the unmount reaches the inner instance" 0 (tree p).instances ;
  equal int 0 (tree p).child_links

(* Two instances with the same key of Cmd.latest. *)
module Latest = struct
  type props = string

  type model = string list

  type msg = Go | Got of string

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let pending : (string -> unit) Queue.t = Queue.create ()

  let init _ = ([], Cmd.none)

  let update _ name m = function
    | Go ->
        ( m
        , Cmd.latest ~key:"k"
            (Async.map (fun s -> Got (name ^ s)) (waiting pending)) )
    | Got s ->
        (s :: m, Cmd.none)

  let view name m = button ~id:name Go (String.concat "," (List.rev m))

  let subs _ _ = Sub.none

  let equal_props = String.equal
end

let latest_local () =
  Queue.clear Latest.pending ;
  let p =
    with_int (fun _ ->
        El.div
          [C.v (module Latest) ~on:never "a"; C.v (module Latest) ~on:never "b"] )
  in
  P.click p (By.test_id "a") ;
  P.click p (By.test_id "b") ;
  equal int ~msg:"the key of b does not cancel the key of a" 2
    (Queue.length Latest.pending) ;
  let a = Queue.pop Latest.pending and b = Queue.pop Latest.pending in
  b "1" ;
  a "1" ;
  P.advance p (Span.ms 20) ;
  equal string "a1" (text p "a") ;
  equal string "b1" (text p "b") ;
  P.click p (By.test_id "a") ;
  P.click p (By.test_id "a") ;
  let old = Queue.pop Latest.pending and now = Queue.pop Latest.pending in
  old "2" ;
  now "3" ;
  P.advance p (Span.ms 20) ;
  equal string ~msg:"the old task of a is cancelled" "a1,a3" (text p "a") ;
  no_errors p

(* Five levels of mounts, each with a synchronous init task: a level mounts the
   level below it after the result of its task. *)
type level = {level: 'msg. int -> 'msg View.t}

let level = ref {level= (fun _ -> txt "")}

module Level = struct
  type props = int

  type model = bool

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (false, Cmd.task (sync ()))

  let update _ _ _ () = (true, Cmd.none)

  let view n ready =
    if ready && n > 1 then !level.level (n - 1)
    else
      El.span
        ~at:[At.data "testid" ("l" ^ string_of_int n)]
        [txt (string_of_bool ready)]

  let subs _ _ = Sub.none

  let equal_props = Int.equal
end

let () = level := {level= (fun n -> C.v (module Level) ~on:never n)}

let five_levels () =
  let p = with_int (fun m -> if m = 0 then txt "" else !level.level 5) in
  let r = (stats p).renders in
  bump p ;
  equal int ~msg:"3 renders in the click turn" (r + 3) (stats p).renders ;
  is_true ~msg:"pending" (ins p).pending ;
  P.advance p (Span.ms 16) ;
  equal int ~msg:"3 renders in the next frame" (r + 6) (stats p).renders ;
  equal string "true" (text p "l1") ;
  equal int 5 (tree p).instances ;
  settled p ;
  quiet p

(* A child with a model of any type, and the function of its next model. *)
let any_model (type m) (init : m) (next : m -> m) (show : m -> string) =
  let module M = struct
    type props = unit

    type model = m

    type msg = unit

    type out = none

    let id : (props, model, msg, out) C.id = C.id ()

    let init () = (init, Cmd.none)

    let update _ () m () = (next m, Cmd.none)

    let view () m = button ~id:"m" () (show m)

    let subs _ _ = Sub.none

    let equal_props _ _ = true
  end in
  let c = C.v (module M) ~on:never () in
  with_int (fun _ -> c)

let same_model () =
  let p = any_model 1 Fun.id string_of_int in
  let r = (stats p).renders in
  P.click p (By.test_id "m") ;
  equal int ~msg:"no render" r (stats p).renders

let closure_model () =
  let p =
    any_model (fun x -> x + 1) (fun f x -> f x) (fun f -> string_of_int (f 1))
  in
  let r = (stats p).renders in
  P.click p (By.test_id "m") ;
  equal int ~msg:"a new closure renders" (r + 1) (stats p).renders ;
  no_errors p

let negative_zero () =
  let p = any_model 0. (fun x -> -.x) (fun x -> string_of_float (1. /. x)) in
  P.click p (By.test_id "m") ;
  equal string "-inf" (text p "m")

(* A child whose functions raise on demand. *)
module Fragile = struct
  type props = {at: string; n: int}

  type model = int

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init p = if p.at = "init" then raise Boom else (0, Cmd.none)

  let update _ p m () = if p.at = "update" then raise Boom else (m + 1, Cmd.none)

  let view p m =
    if p.at = "view" && (m > 0 || p.n > 0) then raise Boom
    else button ~id:"f" () (string_of_int m)

  let subs _ _ = Sub.none

  let equal_props a b = if b.at = "equal" then raise Boom else a = b
end

let fragile ?(stop = false) at =
  let p =
    with_int
      ~fallback:(fun _ -> txt "fallback")
      (fun m ->
        if m = 0 || not stop then
          C.v (module Fragile) ~on:never {Fragile.at; n= m}
        else txt "" )
  in
  p

let caught p =
  equal string "<div>fallback</div>" (P.html p) ;
  is_false ~msg:"the root is dead" (ins p).alive ;
  equal int 0 (tree p).instances ;
  equal int 1 (List.length (errors p))

let error_update () =
  let p = fragile "update" in
  P.click p (By.test_id "f") ;
  caught p

let error_init () =
  let p = fragile "init" in
  caught p

let error_view_mount () =
  let p = fragile "view" in
  bump p ; caught p

let error_view_alone () =
  let p = fragile "view" in
  P.click p (By.test_id "f") ;
  caught p

let error_equal () =
  let p = fragile "equal" in
  bump p ; caught p

(* A child that stops the page in its update. *)
module Stopper = struct
  type props = P.t option ref

  type model = int

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update _ page m () =
    P.stop (Option.get !page) ;
    (m + 1, Cmd.task (Async.return ()))

  let view _ m = button ~id:"s" () (string_of_int m)

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let stop_update () =
  let page = ref None in
  let p = with_int (fun _ -> C.v (module Stopper) ~on:never page) in
  page := Some p ;
  P.click p (By.test_id "s") ;
  equal string "<div></div>" (P.html p) ;
  equal int 0 (tree p).instances ;
  equal int ~msg:"no request" 0 (ins p).requests_left ;
  no_errors p

(* A child init that stops the page during the first mount: no DOM stays. *)
let stop_mount () =
  let h = H.create () and root = F.element "div" in
  let self = ref None in
  let module Halt = struct
    type props = unit

    type model = unit

    type msg = unit

    type out = none

    let id : (props, model, msg, out) C.id = C.id ()

    let init () =
      R.stop (Option.get !self) ;
      ((), Cmd.none)

    let update _ () () () = ((), Cmd.none)

    let view () () = txt "child"

    let subs _ _ = Sub.none

    let equal_props _ _ = true
  end in
  let rt =
    R.create (H.env h)
      (F.ops (F.create ()))
      ~parent:root
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () -> El.div [C.v (module Halt) ~on:never ()])
         () )
  in
  self := Some rt ;
  R.init rt ;
  equal int ~msg:"no DOM" 0 (List.length (F.children root)) ;
  equal int 0 (R.tree rt).instances

(* The parent and the child have a handler for one click. *)
module Inner = struct
  type props = bool

  type model = int

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update _ _ m () = (m + 1, Cmd.none)

  let view stop m =
    El.button
      ~at:[At.data "testid" "c"]
      ~ev:[Ev.click ~stop ()]
      [txt (string_of_int m)]

  let subs _ _ = Sub.none

  let equal_props = Bool.equal
end

let walk_event stop () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             ~ev:[Ev.click ()]
             [El.span [txt (string_of_int m)]; C.v (module Inner) ~on:never stop] )
         () )
  in
  P.click p (By.test_id "c") ;
  equal string "1" (text p "c") ;
  equal string
    (if stop then "0" else "1")
    (F.text (List.hd (F.children (List.hd (F.children (P.root p)))))) ;
  no_errors p

(* A child in an svg element: its elements are SVG, also after a render
   alone. *)
module Shape = struct
  type props = unit

  type model = bool

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (false, Cmd.none)

  let update _ () m () = (not m, Cmd.none)

  let view () m =
    (if m then El.v "rect" else El.circle)
      ~at:[At.data "testid" "shape"]
      ~ev:[Ev.click ()]
      []

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let svg () =
  let p = with_int (fun _ -> El.svg [C.v (module Shape) ~on:never ()]) in
  let shape () = P.find p (By.test_id "shape") in
  is_true ~msg:"svg at the mount" (F.ns (shape ()) = Some Private.Dom.Svg) ;
  P.click p (By.test_id "shape") ;
  equal string "rect" (F.name (shape ())) ;
  is_true ~msg:"svg after a render alone"
    (F.ns (shape ()) = Some Private.Dom.Svg)

(* A child with a controlled input that rejects each change. *)
module Fixed = struct
  type props = unit

  type model = string

  type msg = string

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ("x", Cmd.none)

  let update _ () m _ = (m, Cmd.none)

  let view () m =
    El.input ~at:[At.data "testid" "i"; At.value m] ~ev:[Ev.input Fun.id] ()

  let subs _ _ = Sub.none

  let equal_props _ _ = true
end

let restore () =
  let p = with_int (fun _ -> C.v (module Fixed) ~on:never ()) in
  P.input p (By.test_id "i") "y" ;
  equal string "x" (F.value (P.find p (By.test_id "i"))) ;
  equal int 1 (stats p).restores

(* Decision 13: a change to View.unsafe_html removes the children. *)
let to_inner () =
  let p =
    with_int (fun m ->
        if m = 0 then El.div [counter "a"]
        else View.unsafe_html ~el:El.div (Unsafe_html.of_string "<b>x</b>") )
  in
  bump p ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).child_links

(* The removal walk: a shrink of plain children, a clear, a change from keyed to
   plain children, and a removal deep in an element. *)
let walks () =
  let views =
    [| El.div [counter "a"; El.p [El.span [counter "b"]]; counter "c"]
     ; El.div [counter "a"]
     ; View.keyed El.div
         [(Key.int 1, counter "x"); (Key.int 2, El.p [counter "y"])]
     ; El.div [txt "plain"; counter "z"]
     ; View.keyed El.div []
     ; El.div [El.p [counter "d"]]
     ; El.div [] |]
  in
  let counts = [|3; 1; 2; 1; 0; 1; 0|] in
  let p = with_int (fun m -> views.(m)) in
  Array.iteri
    (fun i n ->
      if i > 0 then bump p ;
      equal int ~msg:(string_of_int i) n (tree p).instances ;
      equal int ~msg:(string_of_int i) n (tree p).child_links )
    counts ;
  no_errors p

(* A child that changes its root element: the old root is free after a Gc. The
   control has no component. *)
let old_root ~component ~parent () =
  let box = Weak.create 1 in
  let view on = (if on then El.span else El.p) [button ~id:"m" `Toggle "m"] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((0, false), Cmd.none))
         ~update:(fun (n, on) -> function
           | `Bump ->
               ((n + 1, on), Cmd.none)
           | `Toggle ->
               ((n, not on), Cmd.none) )
         ~view:(fun (n, on) ->
           El.div
             [ button ~id:"p" `Bump (string_of_int n)
             ; (if component then morph "m" else El.div [view on]) ] )
         () )
  in
  let first () = Option.get (F.parent (P.find p (By.test_id "m"))) in
  Weak.set box 0 (Some (first ())) ;
  P.click p (By.test_id "m") ;
  if parent then begin
    bump p ;
    P.click p (By.test_id "m") ;
    P.click p (By.test_id "m")
  end ;
  is_false ~msg:"a new root" (Weak.get box 0 == Some (first ())) ;
  Gc.full_major () ;
  is_false ~msg:"the old root is free" (Weak.check box 0) ;
  no_errors p

let tests =
  [ group "component"
      [ test "a child renders inside the view of its parent" inside
      ; test "an app with no component needs no hooks" no_hooks
      ; test "the root never goes into the work queue" root_not_queued
      ; test "two instances of one module keep separate models" separate
      ; test "a click in a child renders only the child" alone
      ; test "equal props skip the view of the child" skip
      ; test "new props render the child and keep its model" new_props
      ; test "a custom equal_props decides the render" custom_equal
      ; test "a key change remounts the child and runs init again" key_reset
      ; test "a key in View.keyed moves the instance with no remount" keyed_move
      ; test "a change of the module at a place remounts" module_change
      ; test "a new key at the same position outside View.keyed remounts"
          position_key
      ; test "a module made inside view remounts at each render" local_module
      ; test "two modules that share one id do not remount" shared_id
      ; test "Component.v keeps the spec of the last call" spec_cache
      ; test "an element that changes its name remounts the components below it"
          element_change
      ; test "a removed child is dead and its token is cancelled in the render"
          removed
      ; test "a message to an unmounted instance with no token is dropped"
          no_token
      ; test
          "the init tasks of a new child start in the settle after the render"
          init_tasks
      ; test "tokens: the ancestors first, a dead instance gets a cancelled one"
          dead_token
      ; test "a component in the root fallback gets a cancelled token"
          fallback_component
      ; test "a component in the fallback of a root with no token"
          rootless_fallback
      ; test "a component in the root fallback is dead" fallback_click
      ; test "the unmount frees the model and the DOM parent" weak_unmount
      ; test "a cancelled task with a pending callback keeps no data" weak_task
      ; test "the old root of a child that renders alone is free"
          (old_root ~component:true ~parent:false)
      ; test "the old root is free after renders of the parent"
          (old_root ~component:true ~parent:true)
      ; test "the old root is free with no component"
          (old_root ~component:false ~parent:false)
      ; test "a render alone writes the controlled properties" controlled_alone
      ; test "a dirty parent and a dirty child: the child view runs one time"
          dirty_both
      ; test "dirty instances render from the top" from_top
      ; test "a child that changes its root element replaces its DOM node"
          replace_root
      ; test "a child renders alone, then the parent moves, inserts and removes"
          (transparent ~per:1 morph)
      ; test "the same through a chain of two components"
          (transparent ~per:2 chain)
      ; test "a child whose view is a component" nested
      ; test "Cmd.latest keys are local to each instance" latest_local
      ; test "five levels of mounts with synchronous init tasks" five_levels
      ; test "a child update that gives back the same model: no render"
          same_model
      ; test "a child model with a closure: no exception" closure_model
      ; test "a child model from 0. to -0.: one render" negative_zero
      ; test "an error in a child update goes to the root catch" error_update
      ; test "a child init that raises goes to the root catch" error_init
      ; test "a child view that raises at the mount goes to the root catch"
          error_view_mount
      ; test "a child view that raises alone goes to the root catch"
          error_view_alone
      ; test "an equal_props that raises goes to the root catch" error_equal
      ; test "a stop in a child update ends the turn" stop_update
      ; test "a stop in a child init at the first mount leaves no DOM"
          stop_mount
      ; test "the walk of an event crosses a component" (walk_event false)
      ; test "Ev.stop in a child stops the handler of the parent"
          (walk_event true)
      ; test "a child in an svg element keeps the SVG namespace" svg
      ; test "a rejected input in a child is restored" restore
      ; test
          "a component under an element that changes to View.unsafe_html \
           unmounts"
          to_inner
      ; test "the removal walk finds the components of each removed subtree"
          walks ] ]
