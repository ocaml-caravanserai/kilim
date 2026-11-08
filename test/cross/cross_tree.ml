(* The big loops of the instance tree in each build (design section 14): the
   leak counts after 1,000 repetitions, the supported nesting depth of the
   components (decision 15 of the Phase 2 plan review), and the lists of the
   boundaries. *)

open Kilim
module H = Kilim_testing.Host
module P = Kilim_testing.Page
module C = Component

type none = |

let never : none -> 'a = function _ -> .

let button = El.button ~at:[At.data "testid" "b"] ~ev:[Ev.click ()] []

let click p = P.click p (Kilim_testing.By.test_id "b")

(* A child with a task of 1 s and a subscription of 2 s: two timers. *)
module Timed = struct
  type props = int

  type model = int

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init n = (n, Cmd.task (Async.sleep (Span.s 1)))

  let update _ _ m () = (m + 1, Cmd.none)

  let view _ m = El.span [txt (string_of_int m)]

  let subs _ _ = Sub.every (Span.s 2) ()

  let equal_props = Int.equal
end

let timed n = C.v (module Timed) ~on:never n

let start view =
  let h = H.create () in
  let p =
    P.start ~host:h
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m () -> (m + 1, Cmd.none))
         ~view:(fun m -> El.div [button; view m])
         () )
  in
  (h, p)

let empty p =
  let t = P.tree p in
  t.instances = 0 && t.tokens = 0 && t.child_links = 0 && t.queued = 0

let mounts check =
  let h, p = start (fun m -> if m mod 2 = 0 then timed m else txt "") in
  for _ = 1 to 2_000 do
    click p
  done ;
  check "1,000 mounts and unmounts of a child with two timers: 1 left"
    ((P.tree p).instances = 1 && H.live_timers h = 2) ;
  click p ;
  check "then no timer, token, instance or child link"
    (H.live_timers h = 0 && empty p)

let rows check =
  let h, p =
    start (fun m ->
        View.keyed El.ul
          (List.init 100 (fun i -> (Key.int ((m * 100) + i), timed i))) )
  in
  let ok = ref true in
  for _ = 1 to 1_000 do
    click p ;
    let t = P.tree p in
    if t.instances <> 100 || t.child_links <> 100 then ok := false
  done ;
  check "1,000 replacements of 100 component rows: no dead child link" !ok ;
  P.stop p ;
  check "no timer after the stop" (H.live_timers h = 0)

(* A live owner replaces 100 rows, each with its own boundary, 1,000 times, in a
   boundary that stays: each list has no dead entry, and each list is empty at
   the end. *)
let boundary_rows check =
  let row i = View.boundary ~fallback:(fun _ -> txt "") (timed i) in
  let _, p =
    start (fun m ->
        View.boundary
          ~fallback:(fun _ -> txt "")
          (View.keyed El.ul
             (List.init
                (if m <= 1_000 then 100 else 0)
                (fun i -> (Key.int ((m * 100) + i), row i)) ) ) )
  in
  let ok = ref true in
  for _ = 1 to 1_000 do
    click p ;
    let t = P.tree p in
    if t.owned_links <> 101 || t.member_links <> 100 || t.nested_links <> 100
    then ok := false
  done ;
  check "1,000 replacements of 100 boundary rows: no dead entry" !ok ;
  click p ;
  let t = P.tree p in
  check "then each list is empty"
    (t.owned_links = 1 && t.member_links = 0 && t.nested_links = 0 && empty p)

(* A page of 20 cards, each with its own boundary. *)
module Cards = struct
  type props = int

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = ((), Cmd.none)

  let update _ _ () () = ((), Cmd.none)

  let view n () =
    El.div
      (List.init 20 (fun i ->
           View.boundary ~fallback:(fun _ -> txt "") (timed ((n * 20) + i)) ) )

  let subs _ _ = Sub.none

  let equal_props = Int.equal
end

let card_pages check =
  let _, p =
    start (fun m ->
        View.boundary
          ~fallback:(fun _ -> txt "")
          ( if m <= 1_000 then
              C.v (module Cards) ~key:(string_of_int m) ~on:never m
            else txt "" ) )
  in
  let ok = ref true in
  for _ = 1 to 1_000 do
    click p ;
    if (P.tree p).nested_links <> 20 then ok := false
  done ;
  click p ;
  check "1,000 pages of 20 cards, each with a boundary: 0 nested entries"
    (!ok && (P.tree p).nested_links = 0 && empty p)

(* A row whose init raises. *)
module Failing = struct
  type props = int

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = raise Exit

  let update _ _ () () = ((), Cmd.none)

  let view _ () = txt ""

  let subs _ _ = Sub.none

  let equal_props = Int.equal
end

(* 100,000 rows that each fail in their own boundary: one render, and the cost
   of each catch is linear in its subtree (no time check). *)
let failing_rows check =
  let n = 100_000 in
  let _, p =
    start (fun m ->
        View.keyed El.ul
          (List.init
             (if m = 1 then n else 0)
             (fun i ->
               ( Key.int i
               , View.boundary
                   ~fallback:(fun _ -> El.li [txt "x"])
                   (C.v (module Failing) ~on:never i) ) ) ) )
  in
  let r = (P.stats p).renders in
  click p ;
  let ul =
    List.hd
      (List.tl
         (Kilim_testing.Fake_dom.children
            (List.hd (Kilim_testing.Fake_dom.children (P.root p))) ) )
  in
  check "100,000 rows that fail in their own boundary: one render"
    ( (P.stats p).renders = r + 1
    && (P.stats p).caught = n
    && List.length (Kilim_testing.Fake_dom.children ul) = n
    && (P.tree p).owned_links = n
    && (P.tree p).member_links = 0
    && (P.inspect p).alive ) ;
  click p ;
  check "then the removal unlinks each boundary" ((P.tree p).owned_links = 0)

(* A chain of components: the view of level [n] has the level [n - 1] in a
   [div], and the last level has a task. The props carry a value: a change at
   the top renders each level. *)
type chain = {chain: 'msg. int * int -> 'msg View.t}

let below = ref {chain= (fun _ -> txt "")}

module Level = struct
  type props = int * int

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init (n, _) =
    ((), if n = 0 then Cmd.task (Async.sleep (Span.s 1)) else Cmd.none)

  let update _ _ () () = ((), Cmd.none)

  let view (n, v) () =
    if n = 0 then txt (string_of_int v) else El.div [!below.chain (n - 1, v)]

  let subs _ _ = Sub.none

  let equal_props = ( = )
end

let () = below := {chain= (fun p -> C.v (module Level) ~on:never p)}

let level n v = !below.chain (n, v)

let nesting check depth =
  let h, p = start (fun m -> if m < 2 then level depth m else txt "") in
  let name s =
    "a chain of " ^ string_of_int depth ^ " nested instances: " ^ s
  in
  check (name "the mount")
    ((P.tree p).instances = depth + 1 && H.live_timers h = 1) ;
  click p ;
  check
    (name "a patch of each level")
    ( (P.tree p).instances = depth + 1
    && Kilim_testing.Fake_dom.text (P.root p) = "1" ) ;
  click p ;
  check (name "the unmount") (empty p && H.live_timers h = 0)

let siblings check =
  let _, p =
    start (fun m ->
        if m = 0 then
          View.keyed El.ul (List.init 10_000 (fun i -> (Key.int i, timed i)))
        else txt "" )
  in
  check "10,000 sibling instances: the mount" ((P.tree p).instances = 10_000) ;
  click p ;
  check "10,000 sibling instances: the unmount" (empty p)

(* The supported depth: under js_of_ocaml on node 24 with the default stack, a
   first patch of a chain of 650 levels overflows the stack, and 600 levels
   work. *)
let run check =
  mounts check ;
  rows check ;
  siblings check ;
  nesting check 500 ;
  boundary_rows check ;
  card_pages check ;
  failing_rows check
