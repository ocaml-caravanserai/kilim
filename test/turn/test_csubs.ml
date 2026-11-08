(* The subscriptions of each instance (design sections 08, 09 and 11): the pass
   at the end of the render for the new instances and for the instances with a
   new model or new props, the early call for each instance whose model changed,
   and the root catch of an error in [subs]. *)

open Windtrap
open Kilim
open Support
module C = Private.Component

type none = |

let never : none -> 'a = function _ -> .

exception Boom

let log = ref []

let say s = log := s :: !log

let take () =
  let l = List.rev !log in
  log := [] ;
  l

(* A task that never ends. The log gets its start and the end of its token. *)
let hold name =
  Async.make (fun tok ~ok:_ ~error:_ ->
      say ("start " ^ name) ;
      Cancel.on_end tok (fun () -> say ("stop " ^ name)) )

(* A child with one Sub.run while its model is [true]. The key is the props. A
   keyup or a click on its span toggles the model. A keyup on its input toggles
   it two times: the input and the span have a handler. *)
module Room = struct
  type props = string

  type model = bool

  type msg = Toggle

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let raises = ref false

  let init _ = (true, Cmd.none)

  let update _ _ m Toggle = (not m, Cmd.none)

  let view name _ =
    say ("view " ^ name) ;
    El.span
      ~at:[At.data "testid" ("s" ^ name)]
      ~ev:[Ev.keyup (fun _ -> Toggle); Ev.click Toggle]
      [El.input ~at:[At.data "testid" name] ~ev:[Ev.keyup (fun _ -> Toggle)] ()]

  let subs name m =
    say ("subs " ^ name) ;
    if !raises then raise Boom ;
    if m then Sub.run ~key:name (fun _ -> hold name) else Sub.none

  let equal_props = Component.shallow_equal
end

let room ?key name = C.v (module Room) ?key ~on:never name

(* A parent whose model is a list of names. A click on "p" gives [next] of the
   model. *)
let parent ?(next = fun l -> l) ?fallback ?(view = fun l -> List.map room l)
    names =
  Room.raises := false ;
  let p =
    P.start
      (App.make
         ~init:(fun () -> (names, Cmd.none))
         ~update:(fun l () -> (next l, Cmd.none))
         ~view:(fun l -> El.div (button ~id:"p" () "p" :: view l))
         ?fallback () )
  in
  ignore (take ()) ;
  p

let keyup p id = P.keyup p (By.test_id id) "x"

(* The subscriptions of the new children start after all views of the render, in
   the order of the views. *)
let start_end () =
  Room.raises := false ;
  ignore (take ()) ;
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () -> El.div [room "a"; El.p [room "b"]])
         () )
  in
  equal (list string)
    ["view a"; "view b"; "subs a"; "start a"; "subs b"; "start b"]
    (take ()) ;
  equal int ~msg:"two instances, two keys and their steps" 6 (P.tree p).tokens ;
  settled p ;
  no_errors p ;
  quiet p

let bump p = P.click p (By.test_id "p")

(* New props change the key: the pass of the render stops the old key and starts
   the new key. *)
let new_props () =
  let p = parent ~next:(fun _ -> ["b"]) ["a"] in
  bump p ;
  equal (list string) ["view b"; "subs b"; "stop a"; "start b"] (take ()) ;
  equal int ~msg:"the instance, the key and its step" 3 (P.tree p).tokens ;
  no_errors p

(* A keyup is a Macro turn: the render is pending. The early call stops the
   removed key of the child before the render, and the render starts nothing. A
   second keyup starts the key again at the render. *)
let early () =
  let p = parent ["a"] in
  keyup p "sa" ;
  is_true ~msg:"a pending render" (ins p).pending ;
  equal (list string) ["subs a"; "stop a"] (take ()) ;
  P.advance p (Span.ms 20) ;
  equal (list string) ["view a"; "subs a"] (take ()) ;
  keyup p "sa" ;
  equal (list string) ["subs a"] (take ()) ;
  P.advance p (Span.ms 20) ;
  equal (list string) ["view a"; "subs a"; "start a"] (take ()) ;
  settled p ;
  no_errors p ;
  quiet p

(* A keyup on the input gives two messages to the child: the early call runs
   [subs] one time. The model is [true] again, thus the key stays. *)
let early_once () =
  let p = parent ["a"] in
  keyup p "a" ;
  equal (list string) ["subs a"] (take ()) ;
  equal int ~msg:"the early call empties its list" 0 (P.tree p).stale ;
  P.advance p (Span.ms 20) ;
  equal (list string) ["view a"; "subs a"] (take ()) ;
  no_errors p

(* A click is a discrete turn: the render makes the pass, then the early call at
   the end of the turn finds no change since that pass. *)
let discrete () =
  let p = parent ["a"] in
  P.click p (By.test_id "sa") ;
  equal (list string) ["view a"; "subs a"; "stop a"] (take ()) ;
  P.click p (By.test_id "sa") ;
  equal (list string) ["view a"; "subs a"; "start a"] (take ()) ;
  no_errors p

(* The child toggles in a Macro turn, then the parent removes it at the render:
   the key that the child wants never starts. *)
let removed () =
  let p = parent ~next:(fun _ -> []) ["a"] in
  keyup p "sa" ;
  equal (list string) ["subs a"; "stop a"] (take ()) ;
  keyup p "sa" ;
  equal (list string) ["subs a"] (take ()) ;
  bump p ;
  equal (list string) [] (take ()) ;
  equal int 0 (P.tree p).instances ;
  equal int 0 (P.tree p).tokens ;
  no_errors p ;
  quiet p

(* A child whose view raises. *)
module Bad = struct
  type props = unit

  type model = unit

  type msg = unit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ((), Cmd.none)

  let update _ () () () = ((), Cmd.none)

  let view () () = raise Boom

  let subs () () = Sub.none

  let equal_props = Component.shallow_equal
end

(* A new child that the root catch of the same render unmounts starts no
   subscription. A component of the root fallback is dead: the pass calls no
   [subs] for it. *)
let root_catch () =
  let p =
    parent
      ~next:(fun _ -> ["n"; "bad"])
      ~view:
        (List.map (fun n ->
             if String.equal n "bad" then C.v (module Bad) ~on:never ()
             else room n ) )
      ~fallback:(fun _ -> room "f")
      []
  in
  bump p ;
  equal (list string) ["view n"; "view f"] (take ()) ;
  equal int 1 (List.length (errors p)) ;
  equal int 0 (P.tree p).instances ;
  equal int 0 (P.tree p).tokens

(* An error in [subs] of a child goes to the root catch: in the pass of the
   render, and in the early call. *)
let error_pass () =
  let p = parent ~next:(fun _ -> ["b"]) ~fallback:(fun _ -> txt "f") ["a"] in
  Room.raises := true ;
  bump p ;
  equal (list string) ["view b"; "subs b"; "stop a"] (take ()) ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal string "f" (F.text (P.root p)) ;
  equal int 0 (P.tree p).tokens ;
  settled p ;
  quiet p

let error_early () =
  let p = parent ~fallback:(fun _ -> txt "f") ["a"] in
  Room.raises := true ;
  keyup p "sa" ;
  equal (list string) ["subs a"; "stop a"] (take ()) ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  P.advance p (Span.ms 20) ;
  equal string "f" (F.text (P.root p)) ;
  settled p ;
  quiet p

(* A child with Sub.every: its message comes from its props. A key that stays
   gives the newest message function. *)
module Tick = struct
  type props = int

  type model = int list

  type msg = Got of int

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = ([], Cmd.none)

  let update _ _ l (Got n) = (n :: l, Cmd.none)

  let view _ l =
    El.span
      ~at:[At.data "testid" "t"]
      ~ev:[Ev.click (Got 1)]
      [txt (String.concat "," (List.map string_of_int (List.rev l)))]

  let subs n _ = Sub.every (Span.ms 100) (Got n)

  let equal_props = Component.shallow_equal
end

let newest () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (1, Cmd.none))
         ~update:(fun m () -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             [ button ~id:"p" () "p"
             ; (if m < 3 then C.v (module Tick) ~on:never m else txt "none") ] )
         () )
  in
  P.advance p (Span.ms 120) ;
  bump p ;
  P.advance p (Span.ms 120) ;
  equal string "1,2" (F.text (P.find p (By.test_id "t"))) ;
  equal int 1 (H.live_timers (P.host p)) ;
  bump p ;
  equal int ~msg:"the unmount stops the timer" 0 (H.live_timers (P.host p)) ;
  equal int 0 (P.tree p).tokens ;
  no_errors p ;
  quiet p

(* The clicks in a child are discrete turns: each renders, then the early call
   at the end of the turn empties its list. *)
let clicks () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () -> C.v (module Tick) ~on:never 1)
         () )
  in
  for _ = 1 to 10 do
    P.click p (By.test_id "t")
  done ;
  equal string "1,1,1,1,1,1,1,1,1,1" (F.text (P.find p (By.test_id "t"))) ;
  equal int ~msg:"the early call empties its list" 0 (P.tree p).stale ;
  no_errors p

(* A click gives [Go 5]: each [Go n] changes the model and gives [Go (n - 1)]
   through a task with a synchronous result. Thus six model changes in one turn.
   Each update records the entries of the list of the early call. *)
module Many = struct
  type props = unit

  type model = int

  type msg = Go of int

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let page : P.t option ref = ref None

  let seen = ref []

  let init () = (0, Cmd.none)

  let update _ () m (Go n) =
    Option.iter (fun p -> seen := (P.tree p).stale :: !seen) !page ;
    (m + 1, if n > 0 then Cmd.task (Async.return (Go (n - 1))) else Cmd.none)

  let view () m =
    El.span
      ~at:[At.data "testid" "go"]
      ~ev:[Ev.click (Go 5)]
      [txt (string_of_int m)]

  let subs () _ = Sub.none

  let equal_props = Component.shallow_equal
end

let one_entry () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () () -> ((), Cmd.none))
         ~view:(fun () -> C.v (module Many) ~on:never ())
         () )
  in
  Many.page := Some p ;
  Many.seen := [] ;
  P.click p (By.test_id "go") ;
  equal (list int) ~msg:"one entry for six model changes" [0; 1; 1; 1; 1; 1]
    (List.rev !Many.seen) ;
  equal string "6" (F.text (P.find p (By.test_id "go"))) ;
  equal int 0 (P.tree p).stale ;
  no_errors p

let tests =
  [ group "component subscriptions"
      [ test "the subscriptions of a new child start at the end of the render"
          start_end
      ; test "new props that change the subscriptions" new_props
      ; test "the early call stops a removed key of a child before the render"
          early
      ; test "two messages to a child in one turn: one early call" early_once
      ; test "a click in a child: one subs call in the turn" discrete
      ; test
          "a subscription of an instance that the render removes never starts"
          removed
      ; test "a root catch in the render: no subscription starts" root_catch
      ; test "an error in subs of a child in the render: the root catch"
          error_pass
      ; test "an error in subs of a child in the early call: the root catch"
          error_early
      ; test "a key of a child that stays gets the newest message" newest
      ; test "clicks in a child leave no entry for the early call" clicks
      ; test "many model changes of a child in one turn give one entry"
          one_entry ] ]
