(* The boundaries (design sections 09 and 11): one test for each row of the
   error table, the state of a boundary, the catch inside and outside the diff,
   the lists of a boundary and the order of the render. *)

open Windtrap
open Kilim
open Support
module C = Component

exception Boom

(* A task that never ends. The log gets its start and the end of its token. *)
let hold name =
  Async.make (fun tok ~ok:_ ~error:_ ->
      say ("start " ^ name) ;
      Cancel.on_end tok (fun () -> say ("stop " ^ name)) )

(* Where a probe raises. *)
type at =
  | Init
  | Update
  | View
  | Subs
  | Equal
  | Task
  | Make
  | Handler
  | Feed
  | After
  | Never

let feed : int Private.Sub.kind = Private.Sub.kind ()

(* A child that raises at [at]. A click or a keyup adds 1. [View] raises when
   the model or [n] is not 0, and [Subs] when the model is not 0. [Equal] raises
   when [n] changes. *)
module Probe = struct
  type props = {id: string; at: at; n: int}

  type model = int

  type msg = Incr | Key of string | Fed of int

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init p =
    say ("init " ^ p.id) ;
    if p.at = Init then raise Boom ;
    (0, Cmd.none)

  let update _ p m = function
    | Incr -> (
        if p.at = Update then raise Boom ;
        ( m + 1
        , match p.at with
          | Task ->
              Cmd.task (Async.map (fun () -> raise Boom) (Async.return ()))
          | Make ->
              Cmd.task (Async.make (fun _ ~ok:_ ~error:_ -> raise Boom))
          | After ->
              Cmd.task
                (Async.make (fun _ ~ok ~error:_ -> ok (Fed 1) ; raise Boom))
          | Init | Update | View | Subs | Equal | Handler | Feed | Never ->
              Cmd.none ) )
    | Key _ | Fed _ ->
        (m + 1, Cmd.none)

  let view p m =
    say ("view " ^ p.id) ;
    if p.at = View && m + p.n > 0 then raise Boom ;
    El.button
      ~at:[At.data "testid" p.id]
      ~ev:
        [ Ev.click Incr
        ; Ev.keyup (fun _ -> Incr)
        ; Ev.keydown (fun k -> if p.at = Handler then raise Boom else Key k) ]
      [txt (p.id ^ "=" ^ string_of_int m)]

  let subs p m =
    if p.at = Subs && m > 0 then raise Boom ;
    if p.at = Feed then
      Private.Sub.auto feed ~params:p.id
        ~start:(fun deliver -> deliver 1 ; Async.return ())
        (fun _ -> raise Boom)
    else Sub.none

  let equal_props a b =
    if b.at = Equal && a.n <> b.n then raise Boom ;
    a = b
end

let probe ?(at = Never) ?(n = 0) id =
  C.v (module Probe) ~on:never {Probe.id; at; n}

(* A boundary whose fallback shows the exception. *)
let fb ?key ?(id = "fb") child =
  View.boundary ?key
    ~fallback:(fun e ->
      say ("fallback " ^ id) ;
      El.span
        ~at:[At.data "testid" id]
        [txt (match e with Boom -> "boom" | _ -> "other")] )
    child

(* After a boundary catch: the fallback shows, the root stays alive, the error
   handler gets nothing. *)
let shows ?(caught = 1) ?(text = "boom") ?(id = "fb") p =
  equal string ~msg:"the fallback" text (F.text (P.find p (By.test_id id))) ;
  is_true ~msg:"the root is alive" (ins p).alive ;
  equal int ~msg:"the catches" caught (stats p).caught ;
  no_errors p ;
  settled p

(* Row "init": the instance links before its init, and the catch unmounts it. *)
let row_init () =
  let p = page (fun _ -> El.div [probe "a"; fb (probe ~at:Init "x")]) in
  shows p ;
  let n = P.find p (By.test_id "fb") in
  bump p ;
  is_true ~msg:"a patch keeps the fallback" (P.find p (By.test_id "fb") == n) ;
  equal int ~msg:"only a" 1 (tree p).instances ;
  equal int 1 (tree p).child_links ;
  equal int ~msg:"no member" 0 (tree p).member_links ;
  equal int 1 (tree p).owned_links ;
  P.click p (By.test_id "a") ;
  equal string "a=1" (text p "a")

(* Row "update": the render of the click writes the fallback. *)
let row_update () =
  let p = page (fun _ -> fb (probe ~at:Update "x")) in
  one_render p (fun () -> P.click p (By.test_id "x")) ;
  shows p ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).queued

(* Row "view": the view of an instance alone runs before any DOM write of the
   instance. At the mount, the catch is inside the diff. *)
let row_view () =
  let p = page (fun _ -> fb (probe ~at:View "x")) in
  F.reset (P.doc p) ;
  P.click p (By.test_id "x") ;
  shows p ;
  is_false ~msg:"no write of the child"
    (List.mem_assoc "set_text" (F.counts (P.doc p))) ;
  equal int ~msg:"one replace" 1 (List.assoc "replace" (F.counts (P.doc p))) ;
  let p = page (fun _ -> fb (probe ~at:View ~n:1 "x")) in
  shows p ;
  equal (list string) ["init x"; "view x"; "fallback fb"] (take ())

(* Row "subs", in the pass at the end of the render: the fallback in the same
   render. *)
let row_subs_pass () =
  let p = page (fun _ -> fb (probe ~at:Subs "x")) in
  one_render p (fun () -> P.click p (By.test_id "x")) ;
  shows p

(* Row "subs", in the early call: a render is pending, and it writes the
   fallback. *)
let row_subs_early () =
  let p = page (fun _ -> fb (probe ~at:Subs "x")) in
  let r = (stats p).renders in
  P.keyup p (By.test_id "x") "k" ;
  equal int ~msg:"no render yet" r (stats p).renders ;
  equal string "x=0" (text p "x") ;
  equal int ~msg:"the catch" 1 (stats p).caught ;
  is_true ~msg:"a pending render" (ins p).pending ;
  equal int ~msg:"the boundary in the work queue" 1 (tree p).queued ;
  P.advance p (Span.ms 16) ;
  shows p ;
  equal int 0 (tree p).queued ;
  quiet p

(* Row "equal_props": the patch of the parent, inside the diff. *)
let row_equal () =
  let p = page (fun m -> fb (probe ~at:Equal ~n:m "x")) in
  bump p ;
  shows p ;
  equal string "1" (text p "p")

(* Rows "a step of a task" and "a callback of Async.make". *)
let row_task () =
  List.iter
    (fun at ->
      let p = page (fun _ -> fb (probe ~at "x")) in
      P.click p (By.test_id "x") ;
      shows p ;
      equal int 0 (tree p).tokens ;
      P.expect_no_tasks p )
    [Task; Make]

(* Row "a message function of an Ev handler": it runs when the item leaves the
   queue. *)
let row_handler () =
  let p = page (fun _ -> fb (probe ~at:Handler "x")) in
  P.keydown p (By.test_id "x") "k" ;
  shows p

(* Row "a message function of a subscription". *)
let row_feed () =
  let p = page (fun _ -> fb (probe ~at:Feed "x")) in
  shows p ;
  equal int 0 (tree p).tokens

(* A child that gives an output on a click. With [crash], its div has a handler
   for the same click that raises: the click gives two messages. *)
module Emitter = struct
  include Component.Defaults

  type props = {id: string; crash: bool}

  type model = int

  type msg = Go | Crash

  type out = Sent of int

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update ctx _ m = function
    | Go ->
        (m + 1, Ctx.emit ctx (Sent (m + 1)))
    | Crash ->
        raise Boom

  let view p m =
    El.div
      ~ev:(if p.crash then [Ev.click Crash] else [])
      [button ~id:p.id Go (string_of_int m)]
end

(* A child whose view has an emitter in a boundary. Its [~on] raises if
   [props]. *)
module Middle = struct
  include Component.Defaults

  type props = {raises: bool; crash: bool}

  type model = int list

  type msg = Got of int

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = ([], Cmd.none)

  let update _ _ l (Got n) = (n :: l, Cmd.none)

  let view p l =
    El.div
      [ El.span
          ~at:[At.data "testid" "got"]
          [txt (String.concat "," (List.map string_of_int l))]
      ; fb ~id:"inner"
          (C.v
             (module Emitter)
             ~on:(fun (Emitter.Sent n) -> if p.raises then raise Boom else Got n)
             {Emitter.id= "e"; crash= p.crash} ) ]
end

let middle ?(raises = false) ?(crash = false) () =
  C.v (module Middle) ~on:never {Middle.raises; crash}

(* Row "the ~on function of a parent": the boundary of the parent. *)
let row_on () =
  let p = page (fun _ -> fb (middle ~raises:true ())) in
  P.click p (By.test_id "e") ;
  shows p ;
  is_false ~msg:"not the boundary of the child" (has p "inner") ;
  equal int 0 (tree p).instances

(* The decision of question 6 of the Phase 2 plan: an output of a child that
   dies before the dequeue goes to the live parent. The emitter gives its
   output, then its second message raises in the same turn. *)
let output_of_dead () =
  let p = page (fun _ -> middle ~crash:true ()) in
  P.click p (By.test_id "e") ;
  shows ~id:"inner" p ;
  equal string ~msg:"the parent got the output" "1" (text p "got") ;
  equal int ~msg:"the parent" 1 (tree p).instances

(* A child whose task has an end action that raises. *)
module Ender = Leaf (struct
  type props = unit

  let init () =
    Cmd.task
      (Async.make (fun tok ~ok:_ ~error:_ ->
           Cancel.on_end tok (fun () -> raise Boom) ) )

  let view () = txt "ender"
end)

(* Row "an end action of a token": only the error handler. *)
let row_end_action () =
  let p =
    page (fun m ->
        if m = 0 then fb (C.v (module Ender) ~on:never ()) else txt "" )
  in
  bump p ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int ~msg:"no catch" 0 (stats p).caught ;
  is_true (ins p).alive ;
  equal int 0 (tree p).tokens

(* Row "a callback of Async.make that raises after ok": only the error
   handler. *)
let row_after_ok () =
  let p = page (fun _ -> fb (probe ~at:After "x")) in
  P.click p (By.test_id "x") ;
  equal string "x=2" (text p "x") ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int ~msg:"no catch" 0 (stats p).caught

(* A child with a task that keeps its [error]. *)
let errors_q : (exn -> unit) Queue.t = Queue.create ()

module Waiter = Leaf (struct
  type props = unit

  let init () =
    Cmd.task (Async.make (fun _ ~ok:_ ~error -> Queue.push error errors_q))

  let view () = txt "waiter"
end)

(* Row "a task whose token is cancelled": the error is dropped. *)
let row_cancelled () =
  Queue.clear errors_q ;
  let p =
    page (fun m ->
        if m = 0 then fb (C.v (module Waiter) ~on:never ()) else txt "" )
  in
  bump p ;
  H.raw (P.host p) (fun () -> (Queue.pop errors_q) Boom) ;
  P.run p ;
  no_errors p ;
  equal int ~msg:"no catch" 0 (stats p).caught ;
  is_true (ins p).alive

(* A child that holds a task, with a holder below it. *)
module Holder = Leaf (struct
  type props = string

  let init name = Cmd.task (hold name)

  let view name = El.span [txt name]
end)

let holder name = C.v (module Holder) ~on:never name

module Pair = Leaf (struct
  type props = unit

  let init () = Cmd.task (hold "pair")

  let view () = El.div [holder "h"; probe ~at:Update "x"]
end)

(* The catch unmounts the whole subtree, and cancels its tokens. *)
let subtree () =
  let p = page (fun _ -> fb (C.v (module Pair) ~on:never ())) in
  equal int 3 (tree p).instances ;
  equal int ~msg:"the members: the pair and its children" 3
    (tree p).member_links ;
  ignore (take ()) ;
  P.click p (By.test_id "x") ;
  shows p ;
  equal (list string) ["stop h"; "stop pair"; "fallback fb"] (take ()) ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).tokens ;
  equal int 0 (tree p).member_links ;
  equal int 0 (tree p).child_links

(* A render of the parent calls the fallback again and patches it. It does not
   try the child again. The patch of a fallback has the DOM operations of a
   patch. A catch after a render of the parent uses the newest fallback. *)
let fallback_again () =
  let p =
    page (fun m ->
        View.boundary
          ~fallback:(fun _ ->
            say "fallback" ;
            El.p ~at:[At.data "testid" "fb"] [txt ("fb" ^ string_of_int m)] )
          (probe ~at:Update "x") )
  in
  bump p ;
  P.click p (By.test_id "x") ;
  shows ~text:"fb1" p ;
  ignore (take ()) ;
  F.reset (P.doc p) ;
  bump p ;
  shows ~text:"fb2" p ;
  equal (list string) ["fallback"] (take ()) ;
  equal (list (pair string int)) [("set_text", 2)] (F.counts (P.doc p))

(* A new key of the boundary tries the child again. *)
let key_retry () =
  let p = page (fun m -> fb ~key:(string_of_int m) (probe ~at:Update "x")) in
  P.click p (By.test_id "x") ;
  shows p ;
  ignore (take ()) ;
  bump p ;
  equal (list string) ["init x"; "view x"] (take ()) ;
  equal string "x=0" (text p "x") ;
  equal int 1 (tree p).instances ;
  equal int 1 (tree p).member_links

(* An error in the fallback goes to the outer boundary: a catch outside the diff
   (the fallback of the render) and inside the diff (the fallback of the
   mount). *)
let fallback_raises () =
  let inner at =
    fb ~id:"outer"
      (El.div
         [ View.boundary
             ~fallback:(fun _ -> say "inner" ; raise Boom)
             (probe ~at "x") ] )
  in
  let p = page (fun _ -> inner Update) in
  P.click p (By.test_id "x") ;
  shows ~caught:2 ~id:"outer" p ;
  equal int 0 (tree p).instances ;
  let p = page (fun _ -> inner Init) in
  shows ~caught:2 ~id:"outer" p ;
  equal (list string) ["init x"; "inner"; "fallback outer"] (take ())

(* The instances of a fallback have the outer boundary. *)
let fallback_instances () =
  let p =
    page (fun _ ->
        fb ~id:"outer"
          (El.div
             [ View.boundary
                 ~fallback:(fun _ -> probe ~at:Update "y")
                 (probe ~at:Update "x") ] ) )
  in
  P.click p (By.test_id "x") ;
  equal string "y=0" (text p "y") ;
  equal int ~msg:"y" 1 (tree p).instances ;
  equal int ~msg:"y is a member of the outer boundary" 1 (tree p).member_links ;
  P.click p (By.test_id "y") ;
  shows ~caught:2 ~id:"outer" p ;
  equal int 0 (tree p).instances

(* A child with a Sub.run: the log gets its start and its end. *)
module Ticker = struct
  include Leaf (struct
    type props = unit

    let init () = Cmd.none

    let view () = El.span ~at:[At.data "testid" "fb"] [txt "ticker"]
  end)

  let subs () () = Sub.run ~key:"t" (fun _ -> hold "sub")
end

let ticking at =
  page (fun _ ->
      View.boundary
        ~fallback:(fun _ -> C.v (module Ticker) ~on:never ())
        (probe ~at "x") )

(* A catch outside the diff marks a render as pending: the fallback and its
   subscriptions start at that render. *)
let outside_pending () =
  let p = ticking Update in
  let r = (stats p).renders in
  P.keyup p (By.test_id "x") "k" ;
  equal int ~msg:"no render yet" r (stats p).renders ;
  is_true (ins p).pending ;
  is_true ~msg:"the old child stays until the render" (has p "x") ;
  equal (list string) ["init x"; "view x"] (take ()) ;
  P.advance p (Span.ms 16) ;
  shows ~text:"ticker" p ;
  equal (list string) ["start sub"] (take ()) ;
  quiet p

(* A catch in the subscription pass of the render: the fallback and its
   subscriptions start in the same render. *)
let pass_starts () =
  let p = ticking Subs in
  ignore (take ()) ;
  one_render p (fun () -> P.click p (By.test_id "x")) ;
  shows ~text:"ticker" p ;
  equal (list string) ["view x"; "start sub"] (take ()) ;
  is_false (ins p).pending

(* A child whose div has a handler for the clicks of its content. *)
module Wrapper = struct
  include Component.Defaults

  type props = unit

  type model = int

  type msg = Hit

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (0, Cmd.none)

  let update _ () m Hit = (m + 1, Cmd.none)

  let view () m =
    El.div
      ~ev:[Ev.click Hit]
      [ El.span ~at:[At.data "testid" "w"] [txt (string_of_int m)]
      ; fb ~id:"fbx" (probe ~at:Update "x") ]
end

(* One click gives a message to two widgets: X raises, and Y changes its model.
   Y renders in the same turn. Its patch writes the fallback of X: the render
   does not write it again. *)
let two_widgets () =
  let p = page (fun _ -> C.v (module Wrapper) ~on:never ()) in
  ignore (take ()) ;
  one_render p (fun () -> P.click p (By.test_id "x")) ;
  equal string "1" (text p "w") ;
  shows ~id:"fbx" p ;
  equal (list string) ["fallback fbx"] (take ()) ;
  equal int 0 (tree p).queued

(* A card with a task that waits: the test keeps its [ok]. A bad card raises in
   its view after the first message. *)
let card_oks : (unit -> unit) Queue.t = Queue.create ()

module Card = struct
  include Component.Defaults

  type props = string * bool

  type model = int

  type msg = Incr

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ =
    ( 0
    , Cmd.task
        (Async.make (fun _ ~ok ~error:_ ->
             Queue.push (fun () -> ok Incr) card_oks ) ) )

  let update _ _ m Incr = (m + 1, Cmd.none)

  let view (name, bad) m =
    if bad && m > 0 then raise Boom ;
    El.span ~at:[At.data "testid" name] [txt (string_of_int m)]
end

(* A card that renders alone and raises goes to its own boundary, and the other
   dirty card renders. *)
let cards () =
  Queue.clear card_oks ;
  let card name bad =
    fb ~id:("fb" ^ name) (C.v (module Card) ~on:never (name, bad))
  in
  let p = page (fun _ -> El.div [card "a" true; card "b" false]) in
  H.raw (P.host p) (fun () -> Queue.iter (fun ok -> ok ()) card_oks) ;
  P.advance p (Span.ms 16) ;
  shows ~id:"fba" p ;
  equal string ~msg:"the other card" "1" (text p "b") ;
  equal int 1 (tree p).instances

(* A row with a Sub.run. The init of row 6 raises. *)
module Row = struct
  include Leaf (struct
    type props = int

    let init k = if k = 6 then raise Boom else Cmd.none

    let view k = El.li [txt (string_of_int k)]
  end)

  let subs k () = Sub.run ~key:"r" (fun _ -> hold (string_of_int k))
end

(* Rows 4 and 5 mount with a Sub.run, then the init of row 6 raises: no live
   instance and no token stay. *)
let rows_456 () =
  let p =
    page (fun m ->
        fb
          (View.keyed El.ul
             (List.map
                (fun k -> (Key.int k, C.v (module Row) ~on:never k))
                (List.init (if m = 0 then 3 else 6) (fun i -> i + 1)) ) ) )
  in
  equal int 3 (tree p).instances ;
  ignore (take ()) ;
  bump p ;
  shows p ;
  equal (list string) ["stop 3"; "stop 2"; "stop 1"; "fallback fb"] (take ()) ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).tokens ;
  equal int 0 (tree p).member_links

(* A nested boundary in the same view: the outer catch also unmounts the inner
   subtree, and unlinks the inner boundary. *)
let nested_same_view () =
  let p =
    page (fun _ ->
        fb (El.div [probe ~at:Update "x"; fb ~id:"inner" (holder "h")]) )
  in
  equal int 2 (tree p).owned_links ;
  equal int 1 (tree p).nested_links ;
  ignore (take ()) ;
  P.click p (By.test_id "x") ;
  shows p ;
  equal (list string) ["stop h"; "fallback fb"] (take ()) ;
  equal int 0 (tree p).instances ;
  equal int 0 (tree p).tokens ;
  equal int ~msg:"the failed boundary" 1 (tree p).owned_links ;
  equal int 1 (tree p).owned_links ;
  equal int 0 (tree p).nested_links

(* A row with a task. The view of row 2 raises when the parent changes. *)
module Krow = Leaf (struct
  type props = int * int

  let init (k, _) = Cmd.task (hold (string_of_int k))

  let view (k, m) =
    if k = 2 && m > 0 then raise Boom ;
    El.li [txt (string_of_int k)]
end)

(* A keyed diff from [1; 2; 3] to [3; 2; 4] where row 2 raises: the DOM is [3; 2
   (fallback); 4], row 1 is cancelled, and the task of row 4 starts. *)
let keyed_rows () =
  let row m k =
    ( Key.int k
    , View.boundary
        ~fallback:(fun _ -> El.li [txt (string_of_int k ^ " (fallback)")])
        (C.v (module Krow) ~on:never (k, m)) )
  in
  let p =
    page (fun m ->
        View.keyed
          (El.ul ~at:[At.data "testid" "l"])
          (List.map (row m) (if m = 0 then [1; 2; 3] else [3; 2; 4])) )
  in
  ignore (take ()) ;
  bump p ;
  equal (list string) ["3"; "2 (fallback)"; "4"]
    (List.map F.text (F.children (P.find p (By.test_id "l")))) ;
  equal (list string) ["stop 2"; "stop 1"; "start 4"] (take ()) ;
  equal int ~msg:"the catch" 1 (stats p).caught ;
  equal int 2 (tree p).instances ;
  equal int 3 (tree p).owned_links ;
  no_errors p

(* A fault of a DOM operation goes to the boundary: the fault of the patch of
   the child, inside the diff, and the fault of a controlled property, outside
   the diff. *)
let dom_fault () =
  let p = page (fun m -> fb (El.p [txt (string_of_int m)])) in
  F.reset (P.doc p) ;
  (* The first operation is the text of the button of the root. *)
  F.fail_at (P.doc p) 2 ;
  bump p ;
  shows ~text:"other" p ;
  equal string "1" (text p "p") ;
  let p =
    page (fun m ->
        if m = 0 then txt "" else fb (El.input ~at:[At.value "v"] ()) )
  in
  F.reset (P.doc p) ;
  bump p ;
  let total = F.total (P.doc p) in
  let p =
    page (fun m ->
        if m = 0 then txt "" else fb (El.input ~at:[At.value "v"] ()) )
  in
  F.reset (P.doc p) ;
  (* The last operation is the write of [value]. *)
  F.fail_at (P.doc p) total ;
  bump p ;
  shows ~text:"other" p ;
  quiet p

(* An element of a subtree that a catch unmounted in the same render gets no
   property write: an element of the failed boundary, and an element of a nested
   boundary that the catch unlinked. *)
let dead_property () =
  let p =
    page (fun m ->
        if m = 0 then txt ""
        else
          fb
            (El.div
               [ El.input ~at:[At.value "v"] ()
               ; fb ~id:"in" (El.input ~at:[At.value "w"] ())
               ; probe ~at:Init "x" ] ) )
  in
  F.reset (P.doc p) ;
  bump p ;
  shows p ;
  let counts = F.counts (P.doc p) in
  is_false ~msg:"no read" (List.mem_assoc "get_value" counts) ;
  is_false ~msg:"no write" (List.mem_assoc "set_property" counts)

let input id v = El.input ~at:[At.data "testid" id; At.value v] ()

(* A fault of the property write of a child in a boundary: the fallback in the
   same render. The other element of the failed boundary gets no write, and the
   other controlled values of the app get their writes. *)
let property_fault () =
  let view m =
    if m = 0 then txt ""
    else
      El.div
        [ fb ~id:"f1" (El.div [input "a1" "x"; input "a2" "y"])
        ; input "b" "z"
        ; fb ~id:"f2" (input "c" "w") ]
  in
  let p = page view in
  F.reset (P.doc p) ;
  bump p ;
  let total = F.total (P.doc p) in
  let p = page view in
  F.reset (P.doc p) ;
  (* The last 8 operations read and write a1, a2, b and c. *)
  F.fail_at (P.doc p) (total - 6) ;
  one_render p (fun () -> bump p) ;
  shows ~text:"other" ~id:"f1" p ;
  equal string "z" (F.value (P.find p (By.test_id "b"))) ;
  equal string "w" (F.value (P.find p (By.test_id "c"))) ;
  let counts = F.counts (P.doc p) in
  equal int ~msg:"the reads of a1, b and c" 3 (List.assoc "get_value" counts) ;
  equal int ~msg:"the writes of b and c" 2 (List.assoc "set_property" counts) ;
  quiet p

(* The radio group case: the diff writes the properties after all the other
   operations, also in a boundary. The name of B changes after the boundary of
   A, thus a write of A before that change would be lost. *)
let radio_order () =
  let radio id name c =
    El.input
      ~at:[At.data "testid" id; At.type_ "radio"; At.name name; At.checked c]
      ()
  in
  let p =
    page (fun m ->
        if m = 0 then El.div [fb (radio "A" "g" false); radio "B" "h" true]
        else El.div [fb (radio "A" "g" true); radio "B" "g" false] )
  in
  is_true (F.checked (P.find p (By.test_id "B"))) ;
  bump p ;
  is_true ~msg:"A" (F.checked (P.find p (By.test_id "A"))) ;
  is_false ~msg:"B" (F.checked (P.find p (By.test_id "B"))) ;
  no_errors p

(* A fault of the property write of an element with no boundary: the root
   catch. *)
let property_root () =
  let view m = if m = 0 then txt "" else input "i" "v" in
  let p = page view in
  F.reset (P.doc p) ;
  bump p ;
  let total = F.total (P.doc p) in
  let p = page ~fallback:(fun _ -> txt "root") view in
  F.reset (P.doc p) ;
  F.fail_at (P.doc p) total ;
  bump p ;
  is_false (ins p).alive ;
  equal string "<div>root</div>" (P.html p) ;
  is_true (match errors p with [F.Fault] -> true | _ -> false) ;
  equal int 0 (stats p).caught

(* A boundary that appears or disappears at a place remounts the subtree. *)
let appears () =
  let p = page (fun m -> if m mod 2 = 0 then probe "a" else fb (probe "a")) in
  P.click p (By.test_id "a") ;
  ignore (take ()) ;
  bump p ;
  equal (list string) ["init a"; "view a"] (take ()) ;
  equal string "a=0" (text p "a") ;
  equal int 1 (tree p).owned_links ;
  equal int 1 (tree p).member_links ;
  P.click p (By.test_id "a") ;
  bump p ;
  equal (list string) ["view a"; "init a"; "view a"] (take ()) ;
  equal string "a=0" (text p "a") ;
  equal int 0 (tree p).owned_links ;
  equal int 0 (tree p).owned_links ;
  equal int 1 (tree p).instances ;
  (* A boundary with no component: the removal unlinks it. *)
  let p = page (fun m -> if m = 0 then fb (txt "a") else txt "b") in
  equal int 1 (tree p).owned_links ;
  bump p ;
  equal int 0 (tree p).owned_links ;
  no_errors p

(* The messages of a fallback go to the owner of the boundary. *)
let fallback_messages () =
  let p =
    page (fun _ ->
        View.boundary
          ~fallback:(fun _ -> button ~id:"again" Bump "again")
          (probe ~at:Update "x") )
  in
  P.click p (By.test_id "x") ;
  P.click p (By.test_id "again") ;
  equal string "1" (text p "p") ;
  equal string "again" (text p "again") ;
  no_errors p

(* A child whose div has a handler that raises, around a probe. *)
module Outer = struct
  include Component.Defaults

  type props = unit

  type model = unit

  type msg = Crash

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = ((), Cmd.none)

  let update _ () () Crash = raise Boom

  let view () () = El.div ~ev:[Ev.click Crash] [probe "y"]
end

(* The render skips the instances that a catch unmounted: y changes its model,
   then its parent raises in the same turn. *)
let skips_dead () =
  let p = page (fun _ -> fb (C.v (module Outer) ~on:never ())) in
  ignore (take ()) ;
  let v = (stats p).views in
  P.click p (By.test_id "y") ;
  shows p ;
  equal int ~msg:"no view" v (stats p).views ;
  equal (list string) ["fallback fb"] (take ())

(* A boundary with no error has no DOM node: the DOM of its child. *)
let no_node () =
  let p = page (fun _ -> El.div [fb (txt "a"); fb (El.b [txt "c"])]) in
  equal string
    "<div><div><button \
     data-testid=\"p\">0</button><div>a<b>c</b></div></div></div>"
    (P.html p) ;
  equal int 2 (tree p).owned_links ;
  equal int 0 (stats p).caught

(* A boundary catch does not call the error handler (question 4 of the Phase 2
   plan). The root catch does. *)
let no_handler () =
  let p = page (fun _ -> fb (probe ~at:Update "x")) in
  P.click p (By.test_id "x") ;
  shows p ;
  let p =
    page ~fallback:(fun _ -> txt "root") (fun _ -> probe ~at:Update "x")
  in
  P.click p (By.test_id "x") ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int 0 (stats p).caught

(* A failed boundary that the render removes before it writes its fallback: the
   removal takes it out of the work queue. *)
let removed_before () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m (_ : pmsg) -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             ~ev:[Ev.keyup (fun _ -> Bump)]
             [(if m = 0 then fb (probe ~at:Update "x") else txt "gone")] )
         () )
  in
  log := [] ;
  P.keyup p (By.test_id "x") "k" ;
  equal int ~msg:"the catch" 1 (stats p).caught ;
  P.advance p (Span.ms 16) ;
  equal string "<div><div>gone</div></div>" (P.html p) ;
  equal (list string) [] (take ()) ;
  equal int 0 (tree p).owned_links ;
  no_errors p ;
  quiet p

(* A stop in a fallback that raises after it: the outer boundary is not alive,
   thus it does not catch, and the error goes to the error handler. *)
let stop_in_fallback () =
  let page_ref = ref None in
  let p =
    page (fun _ ->
        fb ~id:"outer"
          (El.div
             [ View.boundary
                 ~fallback:(fun _ ->
                   Option.iter P.stop !page_ref ;
                   raise Boom )
                 (probe ~at:Update "x") ] ) )
  in
  page_ref := Some p ;
  P.click p (By.test_id "x") ;
  equal string "<div></div>" (P.html p) ;
  equal int ~msg:"only the inner catch" 1 (stats p).caught ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int 0 (tree p).queued

(* A boundary in the root fallback: its owner is the dead root, thus it is in no
   list, and its child is dead. *)
let in_root_fallback () =
  let p =
    page ~fallback:(fun _ -> fb (probe "y")) (fun _ -> probe ~at:Update "x")
  in
  P.click p (By.test_id "x") ;
  is_false (ins p).alive ;
  equal string "y=0" (text p "y") ;
  let t = tree p in
  equal int 0 t.owned_links ;
  equal int 0 t.owned_links ;
  equal int 0 t.member_links ;
  equal int 0 (stats p).caught

(* The hooks before the first call of View.boundary or Component.v: each hook
   test starts from them. *)
let reset_hooks () =
  let no _ = invalid_arg "no hook" in
  Private.Diff.hooks :=
    { build= (fun _ _ -> no)
    ; patch= (fun _ _ _ -> no)
    ; render= no
    ; walk= (fun _ _ -> ())
    ; walk_children= (fun _ _ -> ())
    ; flush= (fun _ _ -> false) } ;
  Private.Instance.ext := {detach= (fun _ _ -> ()); enqueue= (fun _ _ -> ())} ;
  Private.Diff.bounds :=
    { build_boundary= (fun _ _ -> no)
    ; patch_boundary= (fun _ _ _ -> no)
    ; show= (fun _ _ _ -> ())
    ; fail= (fun _ _ _ -> ())
    ; write= (fun _ _ _ _ _ -> ()) }

(* View.boundary installs the functions of the boundary nodes, and of the
   component nodes: the removal walk unlinks a boundary with no component, and
   the write of a controlled property in a boundary runs. *)
let installs () =
  reset_hooks () ;
  let p =
    page (fun m ->
        if m = 0 then fb (El.input ~at:[At.data "testid" "i"; At.value "v"] ())
        else txt "b" )
  in
  equal string "v" (F.value (P.find p (By.test_id "i"))) ;
  equal int 1 (tree p).owned_links ;
  bump p ;
  equal int ~msg:"the walk unlinks the boundary" 0 (tree p).owned_links ;
  no_errors p

(* The first boundary of the app comes in a later render. *)
let installs_later () =
  reset_hooks () ;
  let p =
    page (fun m -> if m = 0 then txt "a" else fb (probe ~at:Update "x"))
  in
  bump p ;
  P.click p (By.test_id "x") ;
  shows p

(* The first boundary of the app comes in the root fallback, in an app with no
   component and no boundary before. *)
let installs_root_fallback () =
  reset_hooks () ;
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun _ (_ : pmsg) -> raise Boom)
         ~view:(fun m -> El.div [button ~id:"p" Bump (string_of_int m)])
         ~fallback:(fun _ -> fb (El.div [txt "rf"]))
         () )
  in
  bump p ;
  equal string "<div><div>rf</div></div>" (P.html p)

(* A controlled value and a controlled checked in a boundary in the root
   fallback: a boundary of a dead owner is in no list, but its elements get
   their writes. *)
let root_fallback_controlled () =
  let p =
    page
      ~fallback:(fun _ -> El.div [input "plain" "v"; fb (input "inb" "w")])
      (fun _ -> probe ~at:Update "x")
  in
  P.click p (By.test_id "x") ;
  equal string ~msg:"no boundary" "v" (F.value (P.find p (By.test_id "plain"))) ;
  equal string ~msg:"in a boundary" "w" (F.value (P.find p (By.test_id "inb"))) ;
  let p =
    page
      ~fallback:(fun _ ->
        fb
          (El.input
             ~at:[At.data "testid" "c"; At.type_ "checkbox"; At.checked true]
             () ) )
      (fun _ -> probe ~at:Update "x")
  in
  P.click p (By.test_id "x") ;
  is_true ~msg:"checked in a boundary" (F.checked (P.find p (By.test_id "c")))

(* A boundary failed in the App, then a root catch: the root fallback has a
   boundary at the same place. The boundary of the dead App left the tree, thus
   the root fallback builds its own boundary and shows its child. *)
let root_fallback_new () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun _ (_ : pmsg) -> raise Boom)
         ~view:(fun m ->
           El.div
             [ button ~id:"p" Bump (string_of_int m)
             ; fb ~id:"sec" (probe ~at:Update "x") ] )
         ~fallback:(fun _ ->
           El.div
             [ txt "app"
             ; View.boundary
                 ~fallback:(fun _ -> txt "failed")
                 (El.span
                    ~at:[At.data "testid" "rf"]
                    [txt "Something went wrong"] ) ] )
         () )
  in
  P.click p (By.test_id "x") ;
  equal string "boom" (text p "sec") ;
  bump p ;
  is_false (ins p).alive ;
  equal string "Something went wrong" (text p "rf")

(* A live owner replaces 20 boundary rows 30 times. Some inner boundaries and
   some rows fail at their mount. *)
let churn () =
  let p =
    page (fun m ->
        El.div
          (List.init 20 (fun i ->
               fb
                 ~key:(string_of_int ((m * 100) + i))
                 (El.div
                    [ fb ~id:"in"
                        (probe
                           ~at:(if i mod 3 = 0 then Init else Never)
                           (string_of_int i) )
                    ; probe
                        ~at:(if i mod 4 = 0 then Init else Never)
                        ("q" ^ string_of_int i) ] ) ) ) )
  in
  for _ = 1 to 30 do
    bump p
  done ;
  let t = tree p in
  equal int 35 t.owned_links ;
  equal int 25 t.member_links ;
  no_errors p

(* A catch outside the diff, then the owner renders before the render of the
   work queue, and the new fallback raises at that patch: the outer boundary. *)
let pending_then_owner () =
  let p =
    page (fun m ->
        fb ~id:"outer"
          (El.div
             [ View.boundary
                 ~fallback:(fun _ -> if m > 0 then raise Boom else txt "inner")
                 (probe ~at:Update "x") ] ) )
  in
  P.click p (By.test_id "x") ;
  equal int 1 (stats p).caught ;
  bump p ;
  shows ~caught:2 ~id:"outer" p ;
  equal int 0 (tree p).nested_links ;
  equal int 0 (tree p).queued

(* A component with a controlled value in a boundary and one outside it. *)
module Inp = struct
  include Component.Defaults

  type props = unit

  type model = int

  type msg = Incr

  type out = none

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (0, Cmd.none)

  let update _ () m Incr = (m + 1, Cmd.none)

  let view () m =
    El.div
      [ button ~id:"inc" Incr (string_of_int m)
      ; fb ~id:"fi" (input "ii" (string_of_int m))
      ; input "jj" (string_of_int m) ]
end

(* A fault of a controlled property in a component that renders alone. *)
let alone_fault () =
  let view _ = fb ~id:"outer" (C.v (module Inp) ~on:never ()) in
  let p = page view in
  F.reset (P.doc p) ;
  P.click p (By.test_id "inc") ;
  let total = F.total (P.doc p) in
  let p = page view in
  F.reset (P.doc p) ;
  (* The last operations read and write ii, then jj. *)
  F.fail_at (P.doc p) (total - 2) ;
  P.click p (By.test_id "inc") ;
  shows ~text:"other" ~id:"fi" p ;
  equal string "1" (F.value (P.find p (By.test_id "jj"))) ;
  P.click p (By.test_id "inc") ;
  equal string "2" (F.value (P.find p (By.test_id "jj"))) ;
  equal string "other" (text p "fi") ;
  quiet p

(* One click, two raising instances of one boundary: one catch. *)
let two_in_one () =
  let p =
    page (fun _ -> fb (El.div [probe ~at:Update "x"; probe ~at:Update "y"]))
  in
  P.click p (By.test_id "x") ;
  shows p ;
  equal int 0 (tree p).instances

(* A new key while the fallback waits for the render: the old fallback never
   shows. *)
let key_while_pending () =
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun m (_ : pmsg) -> (m + 1, Cmd.none))
         ~view:(fun m ->
           El.div
             ~ev:[Ev.keyup (fun _ -> Bump)]
             [fb ~key:(string_of_int m) (probe ~at:Update "x")] )
         () )
  in
  log := [] ;
  P.keyup p (By.test_id "x") "k" ;
  equal int 1 (stats p).caught ;
  P.advance p (Span.ms 16) ;
  equal string "x=0" (text p "x") ;
  equal (list string) ["init x"; "view x"] (take ()) ;
  equal int 1 (tree p).owned_links ;
  equal int 1 (tree p).member_links ;
  equal int 0 (tree p).queued ;
  no_errors p

(* A fallback with a component in a nested boundary, then the outer boundary
   fails: the catch also unmounts the fallback of the inner boundary. *)
let fallback_in_outer () =
  let p =
    page (fun _ ->
        fb ~id:"outer"
          (El.div
             [ probe ~at:Update "o"
             ; View.boundary
                 ~fallback:(fun _ -> fb ~id:"deep" (holder "hf"))
                 (probe ~at:Update "x") ] ) )
  in
  P.click p (By.test_id "x") ;
  equal int 2 (tree p).instances ;
  equal int 3 (tree p).owned_links ;
  ignore (take ()) ;
  P.click p (By.test_id "o") ;
  shows ~caught:2 ~id:"outer" p ;
  equal (list string) ["stop hf"; "fallback outer"] (take ()) ;
  let t = tree p in
  equal int 0 t.instances ;
  equal int 1 t.owned_links ;
  equal int 0 t.nested_links ;
  equal int 0 t.member_links ;
  equal int 0 t.tokens

(* Rows replaced 40 times, with catches inside and outside the diff in each
   render, then an empty view: no list keeps an entry. *)
let catch_leaks () =
  let p =
    page (fun m ->
        if m >= 40 then txt "end"
        else
          El.div
            (List.init 10 (fun i ->
                 let s = string_of_int i in
                 fb
                   ~key:(string_of_int ((m / 2 * 100) + i))
                   (El.div
                      [ fb ~id:("in" ^ s)
                          (El.div
                             [probe ~at:Update ("u" ^ s); fb (holder ("h" ^ s))] )
                      ; probe
                          ~at:
                            (if i mod 4 = 0 && m mod 2 = 1 then Equal else Never)
                          ~n:m ("q" ^ s) ] ) ) ) )
  in
  for _ = 1 to 40 do
    if has p "u3" then P.click p (By.test_id "u3") ;
    bump p
  done ;
  let t = tree p in
  equal int 0 t.owned_links ;
  equal int 0 t.member_links ;
  equal int 0 t.nested_links ;
  equal int 0 t.instances ;
  equal int 0 t.tokens ;
  equal int 0 t.child_links ;
  no_errors p

(* A child with a subs function that raises. *)
module Badsubs = struct
  include Leaf (struct
    type props = unit

    let init () = Cmd.none

    let view () = El.span [txt "bad"]
  end)

  let subs () () = say "badsubs" ; raise Boom
end

(* The subscriptions of a fallback raise in the pass of the render: the outer
   boundary, in the same render. *)
let fallback_subs_raise () =
  let p =
    page (fun _ ->
        fb ~id:"outer"
          (El.div
             [ View.boundary
                 ~fallback:(fun _ -> C.v (module Badsubs) ~on:never ())
                 (probe ~at:Update "x") ] ) )
  in
  ignore (take ()) ;
  one_render p (fun () -> P.click p (By.test_id "x")) ;
  shows ~caught:2 ~id:"outer" p ;
  equal (list string) ["badsubs"; "fallback outer"] (take ()) ;
  equal int 0 (tree p).instances

let tests =
  [ group "boundary"
      [ test "init raises: the nearest boundary shows its fallback" row_init
      ; test "update raises" row_update
      ; test "view raises before any DOM write of the child" row_view
      ; test "subs raises in the pass of the render" row_subs_pass
      ; test "subs raises in the early call: a render is pending" row_subs_early
      ; test "equal_props raises" row_equal
      ; test "a task step or a callback of Async.make raises" row_task
      ; test "an Ev message function raises" row_handler
      ; test "a Sub message function raises" row_feed
      ; test "the on function raises: the boundary of the parent" row_on
      ; test "an end action raises: only the error handler" row_end_action
      ; test "a callback that raises after ok: only the error handler"
          row_after_ok
      ; test "a cancelled task drops its last error" row_cancelled
      ; test "an output of a child that dies before the dequeue" output_of_dead
      ; test "the catch unmounts the whole subtree and cancels its tokens"
          subtree
      ; test "a render of the parent calls fallback again and keeps it"
          fallback_again
      ; test "a key change of the boundary tries the child again" key_retry
      ; test "an error in the fallback goes to the outer boundary"
          fallback_raises
      ; test "the instances of a fallback have the outer boundary"
          fallback_instances
      ; test
          "a catch outside the diff marks a render as pending, and the \
           fallback subscriptions start at that render"
          outside_pending
      ; test "a catch in the subs pass starts the fallback subscriptions"
          pass_starts
      ; test "one event gives messages to two widgets: Y renders, X raises"
          two_widgets
      ; test "a card that renders alone and raises: the other card renders"
          cards
      ; test "rows 4 and 5 mount with Sub.run, row 6 raises: nothing stays"
          rows_456
      ; test "a nested boundary in the same view: the outer catch"
          nested_same_view
      ; test "a keyed diff from [1; 2; 3] to [3; 2; 4] where row 2 raises"
          keyed_rows
      ; test "an exception of a DOM operation goes to the boundary" dom_fault
      ; test "an element of a failed subtree gets no property write"
          dead_property
      ; test "a property fault in a boundary: the other values get their writes"
          property_fault
      ; test "radio buttons in a boundary: the order of the property writes"
          radio_order
      ; test "a property fault with no boundary goes to the root catch"
          property_root
      ; test "a boundary that appears or disappears remounts the subtree"
          appears
      ; test "the messages of a fallback go to the owner" fallback_messages
      ; test "the render skips the instances that a catch unmounted" skips_dead
      ; test "a boundary with no error has no DOM node" no_node
      ; test "a boundary catch does not call the error handler" no_handler
      ; test "a failed boundary that the render removes" removed_before
      ; test "a stop in a fallback" stop_in_fallback
      ; test "a boundary in the root fallback is in no list" in_root_fallback
      ; test "View.boundary installs its functions" installs
      ; test "the first boundary in a later render" installs_later
      ; test "the first boundary in the root fallback" installs_root_fallback
      ; test "controlled values in a boundary in the root fallback"
          root_fallback_controlled
      ; test "the root fallback builds its own boundary" root_fallback_new
      ; test "a live owner replaces failing boundary rows" churn
      ; test
          "a pending fallback, then the owner renders and its fallback raises"
          pending_then_owner
      ; test "a property fault in a component that renders alone" alone_fault
      ; test "two catches of one boundary in one turn" two_in_one
      ; test "a new key while the fallback waits" key_while_pending
      ; test "a fallback with a nested boundary, then the outer catch"
          fallback_in_outer
      ; test "catches inside and outside the diff leave no link" catch_leaks
      ; test "the subscriptions of a fallback raise" fallback_subs_raise ] ]
