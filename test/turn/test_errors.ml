(* The errors of the root (design section 09): the root catch unmounts the App,
   and the root shows the fallback. *)

open Windtrap
open Kilim
open Support

exception Boom

type msg = Click | Tick | Fail

let fallback e =
  button ~id:"fb" Click (match e with Boom -> "fallback" | _ -> "other")

(* The view raises for the model 2. A task waits on a timer. *)
let app
    ?(view =
      fun m -> if m = 2 then raise Boom else button Click (string_of_int m))
    ?fallback
    ?(init =
      fun () ->
        (0, Cmd.task (Async.map (fun () -> Tick) (Async.sleep (Span.s 1))))) ()
    =
  App.make ~init
    ~update:(fun m -> function
      | Click ->
          (m + 1, Cmd.none)
      | Tick ->
          (m, Cmd.none)
      | Fail ->
          (m, Cmd.task (Async.fail Boom)) )
    ~view ?fallback ()

let in_view () =
  let p = P.start (app ~fallback ()) in
  equal int 1 (H.live_timers (P.host p)) ;
  equal int 1 (ins p).token_children ;
  click p ;
  click p ;
  equal string "<div><button data-testid=\"fb\">fallback</button></div>"
    (P.html p) ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  is_false (ins p).alive ;
  equal int 0 (H.live_timers (P.host p)) ;
  equal int 0 (ins p).token_children ;
  equal int 3 (stats p).views ;
  equal int 3 (stats p).renders ;
  (* The messages of the fallback do nothing. *)
  P.click p (By.test_id "fb") ;
  equal int 1 (stats p).dropped ;
  equal int 2 (stats p).messages ;
  equal int 1 (List.length (errors p)) ;
  equal int 3 (stats p).renders ;
  P.expect_no_tasks p ;
  settled p

(* A fault of a DOM operation in the patch: the fallback replaces the view, and
   the next turns run. *)
let in_dom () =
  let p = P.start (app ~fallback ()) in
  F.fail_at (P.doc p) 1 ;
  click p ;
  is_true (match errors p with [F.Fault] -> true | _ -> false) ;
  equal string "<div><button data-testid=\"fb\">other</button></div>" (P.html p) ;
  is_false (ins p).alive ;
  let turns = (stats p).turns in
  P.click p (By.test_id "fb") ;
  equal int (turns + 1) (stats p).turns ;
  equal int 1 (stats p).dropped ;
  P.advance p (Span.s 2) ;
  settled p ;
  P.expect_no_tasks p

(* init raises: the start mounts the fallback, and no view runs. *)
let in_init () =
  let init () = raise Boom in
  let p = P.start (app ~init ~fallback ()) in
  equal string "<div><button data-testid=\"fb\">fallback</button></div>"
    (P.html p) ;
  equal int 0 (stats p).views ;
  equal int 1 (stats p).renders ;
  equal int 1 (List.length (errors p)) ;
  (* No fallback: an empty text. *)
  let p = P.start (app ~init ()) in
  equal string "<div></div>" (P.html p) ;
  equal int 1 (List.length (errors p)) ;
  equal int 1 (List.length (F.children (P.root p))) ;
  (* A fallback that raises: two errors, and an empty text. *)
  let p = P.start (app ~init ~fallback:(fun _ -> raise Exit) ()) in
  equal string "<div></div>" (P.html p) ;
  is_true (match errors p with [Boom; Exit] -> true | _ -> false) ;
  equal int 1 (List.length (F.children (P.root p))) ;
  settled p

(* A task error leaves the queue, then the fallback shows. *)
let in_task () =
  let fail_button m =
    El.div [button Click (string_of_int m); button ~id:"x" Fail "x"]
  in
  let p = P.start (app ~view:fail_button ~fallback ()) in
  P.click p (By.test_id "x") ;
  equal string "<div><button data-testid=\"fb\">fallback</button></div>"
    (P.html p) ;
  equal int 1 (List.length (errors p)) ;
  equal int 1 (stats p).messages ;
  is_false (ins p).alive ;
  (* A message function of a handler that raises: the root catch. *)
  let raising : msg Ev.t =
    { Private.View.event= "click"
    ; bubbles= true
    ; prevent_default= false
    ; stop= false
    ; decode= Private.View.Decode (Private.Dom.Unit, fun () -> raise Boom) }
  in
  let view _ = El.button ~at:[At.data "testid" "b"] ~ev:[raising] [txt "r"] in
  let p = P.start (app ~view ~fallback ()) in
  click p ;
  equal string "<div><button data-testid=\"fb\">fallback</button></div>"
    (P.html p) ;
  equal int 1 (List.length (errors p)) ;
  equal int 0 (stats p).messages ;
  equal int 0 (H.live_timers (P.host p)) ;
  settled p

(* An update that raises: the next message of the turn is dropped, and the next
   render shows the fallback. *)
let in_update () =
  let update m = function
    | Click ->
        if m = 1 then raise Boom else (m + 1, Cmd.none)
    | Tick | Fail ->
        (m, Cmd.none)
  in
  let q = Queue.create () in
  let p =
    P.start
      (App.make
         ~init:(fun () ->
           (0, Cmd.batch [Cmd.task (waiting q); Cmd.task (waiting q)]) )
         ~update
         ~view:(fun m -> button Click (string_of_int m))
         ~fallback () )
  in
  click p ;
  H.raw (P.host p) (fun () ->
      (Queue.pop q) Click ;
      (Queue.pop q) Click ) ;
  P.run p ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int 1 (stats p).dropped ;
  equal int 2 (stats p).messages ;
  is_true (ins p).pending ;
  equal string "<div><button data-testid=\"b\">1</button></div>" (P.html p) ;
  P.advance p (Span.ms 16) ;
  equal string "<div><button data-testid=\"fb\">fallback</button></div>"
    (P.html p) ;
  P.expect_no_tasks p ;
  settled p

(* The end of a task releases its token at once, before its error leaves the
   queue. The step of a second task in the same drain sees it. *)
let error_release () =
  let h = H.create () in
  let page = ref None in
  let err = ref (fun _ -> ()) and ok = ref (fun _ -> ()) and seen = ref (-1) in
  let second =
    Async.bind
      (Async.make (fun _ ~ok:k ~error:_ -> ok := k))
      (fun () ->
        Option.iter (fun p -> seen := (ins p).token_children) !page ;
        Async.return Tick )
  in
  let p =
    P.start ~host:h
      (app
         ~init:(fun () ->
           ( 0
           , Cmd.batch
               [ Cmd.task (Async.make (fun _ ~ok:_ ~error -> err := error))
               ; Cmd.task second ] ) )
         () )
  in
  page := Some p ;
  equal int 2 (ins p).token_children ;
  H.raw h (fun () -> !err Boom ; !ok ()) ;
  P.run p ;
  equal int 1 !seen ;
  is_false (ins p).alive

(* init raises, and a DOM fault in the mount of the fallback: the root shows an
   empty text. If the fallback has a duplicate key and the mount of the empty
   text has a DOM fault, the root has no tree. The stop then has no DOM to
   remove. *)
let no_tree () =
  let start fallback =
    let h = H.create () in
    let doc = F.create () in
    let parent = F.element "div" in
    F.fail_at doc 1 ;
    let rt =
      R.start (H.env h) (F.ops doc) ~parent
        (app ~init:(fun () -> raise Boom) ~fallback ())
    in
    (h, parent, rt)
  in
  let h, parent, _ = start fallback in
  is_true (match H.errors h with [Boom; F.Fault] -> true | _ -> false) ;
  equal (list string) ["#text"] (List.map F.name (F.children parent)) ;
  let h, parent, rt =
    start (fun _ ->
        View.keyed El.ul [(Key.int 1, txt "a"); (Key.int 1, txt "b")] )
  in
  is_true
    ( match H.errors h with
    | [Boom; Invalid_argument _; F.Fault] ->
        true
    | _ ->
        false ) ;
  equal (list string) [] (List.map F.name (F.children parent)) ;
  R.stop rt ;
  equal int 3 (List.length (H.errors h)) ;
  is_false (R.inspect rt).alive

(* The catch with a dirty instance: the fallback write is the last render. *)
let fb_html = "<div><button data-testid=\"fb\">fallback</button></div>"

(* An init task that fails at once: the start turn is dirty. *)
let dirty_init () =
  let p =
    P.start (app ~init:(fun () -> (0, Cmd.task (Async.fail Boom))) ~fallback ())
  in
  equal string fb_html (P.html p) ;
  equal int 1 (stats p).renders ;
  quiet p ;
  equal int 1 (stats p).renders

let fail_button m =
  El.div [button Click (string_of_int m); button ~id:"x" Fail "x"]

(* A click that changes the model and gives a task that fails at once. *)
let dirty_click () =
  let update m = function
    | Fail ->
        (m + 1, Cmd.task (Async.fail Boom))
    | _ ->
        (m, Cmd.none)
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update ~view:fail_button ~fallback () )
  in
  P.click p (By.test_id "x") ;
  equal string fb_html (P.html p) ;
  equal int 2 (stats p).renders ;
  quiet p ;
  equal int 2 (stats p).renders

(* A model change, then an update that raises, in one Micro turn. *)
let dirty_update () =
  let q = Queue.create () in
  let update m = function
    | Click ->
        (m + 1, Cmd.none)
    | Fail ->
        raise Boom
    | Tick ->
        (m, Cmd.none)
  in
  let p =
    P.start
      (App.make
         ~init:(fun () ->
           (0, Cmd.batch [Cmd.task (waiting q); Cmd.task (waiting q)]) )
         ~update ~view:fail_button ~fallback () )
  in
  H.raw (P.host p) (fun () ->
      (Queue.pop q) Click ;
      (Queue.pop q) Fail ) ;
  P.run p ;
  is_true (ins p).pending ;
  quiet p ;
  equal string fb_html (P.html p) ;
  equal int 2 (stats p).renders ;
  equal int 1 (List.length (errors p))

(* The function of Cmd.map runs when the result leaves the queue. If it raises,
   the root catch shows the fallback. *)
let in_map () =
  let update m = function
    | Fail ->
        (m, Cmd.map (fun _ -> raise Boom) (Cmd.task (Async.return 1)))
    | _ ->
        (m, Cmd.none)
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update ~view:fail_button ~fallback () )
  in
  P.click p (By.test_id "x") ;
  equal string fb_html (P.html p) ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal int ~msg:"the result does not reach update" 1 (stats p).messages ;
  quiet p

(* A fault inside update: the root catch comes during the update. The model of
   the dead instance does not change, and the instance does not become dirty.
   Thus no render runs after the fallback. *)
let fault_in_update () =
  let f =
    faulty
      ~update:(fun fault m () ->
        fault () ;
        (m + 1, Cmd.none) )
      ~view:(fun _ m -> button () (string_of_int m))
      ()
  in
  fclick f ;
  is_true (match H.errors f.h with [Fault] -> true | _ -> false) ;
  is_false (R.inspect f.rt).alive ;
  equal string "<div></div>" (F.to_html f.root) ;
  equal int 2 (R.stats f.rt).renders ;
  fquiet f ;
  equal int 2 (R.stats f.rt).renders

(* A fault in view, then the view raises: the fallback gets the first error, and
   the second error only goes to the error handler. *)
let fault_in_view () =
  let f =
    faulty
      ~update:(fun _ m () -> (m + 1, Cmd.none))
      ~view:(fun fault m ->
        if m = 1 then (fault () ; raise Boom) ;
        button () (string_of_int m) )
      ~fallback:(fun e -> txt (match e with Fault -> "first" | _ -> "other"))
      ()
  in
  fclick f ;
  equal string "<div>first</div>" (F.to_html f.root) ;
  is_true (match H.errors f.h with [Fault; Boom] -> true | _ -> false) ;
  equal int 2 (R.stats f.rt).renders ;
  fquiet f

(* The write of the root fallback raises (a duplicate key): the root shows an
   empty text. *)
type kmsg = Inc | Kill

let kill_app ~fallback =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update:(fun m -> function Inc -> (m + 1, Cmd.none) | Kill -> raise Boom)
    ~view:(fun m ->
      El.div [button Inc (string_of_int m); button ~id:"k" Kill "kill"] )
    ~fallback ()

let dup_keys _ = View.keyed El.ul [(Key.int 1, txt "a"); (Key.int 1, txt "b")]

let fallback_write () =
  let p = P.start (kill_app ~fallback:dup_keys) in
  P.click p (By.test_id "k") ;
  equal string "<div></div>" (P.html p) ;
  is_true (match errors p with [Boom; Invalid_argument _] -> true | _ -> false) ;
  is_false (ins p).alive ;
  quiet p ;
  (* The write of the empty text raises too (a DOM fault): the root removes its
     DOM. *)
  let p = P.start (kill_app ~fallback:dup_keys) in
  F.reset (P.doc p) ;
  (* The failed patch of the fallback makes no DOM operation. *)
  F.fail_at (P.doc p) 1 ;
  P.click p (By.test_id "k") ;
  equal string "<div></div>" (P.html p) ;
  equal int 0 (List.length (F.children (P.root p))) ;
  is_true
    ( match errors p with
    | [Boom; Invalid_argument _; F.Fault] ->
        true
    | _ ->
        false ) ;
  quiet p

(* An app with a button "b". The test keeps the page in [page], thus user code
   can stop it. *)
type pmsg = Go | Got of int

let page_app ~page ~update ?(view = fun _ -> ()) ?fallback () =
  App.make
    ~init:(fun () -> (0, Cmd.none))
    ~update
    ~view:(fun m ->
      view m ;
      El.div [button Go (string_of_int m)] )
    ?fallback ()
  |> fun app ->
  let p = P.start app in
  page := Some p ;
  p

let stop page = Option.iter P.stop !page

(* The function of Cmd.map stops the page: its message does not reach update,
   and it counts as dropped. *)
let map_stops () =
  let page = ref None and got = ref 0 in
  let update m = function
    | Go ->
        (m, Cmd.map (fun x -> stop page ; x) (Cmd.task (Async.return (Got 5))))
    | Got n ->
        incr got ;
        (m + n, Cmd.none)
  in
  let p = page_app ~page ~update () in
  click p ;
  quiet p ;
  equal int ~msg:"no update after the death" 0 !got ;
  equal int 1 (stats p).messages ;
  equal int 1 (stats p).dropped ;
  is_false (ins p).alive

(* A stop in view: the render writes no DOM. *)
let stop_in_view () =
  let page = ref None in
  let p =
    page_app ~page
      ~update:(fun m _ -> (m + 1, Cmd.none))
      ~view:(fun m -> if m = 1 then stop page)
      ()
  in
  click p ;
  equal string "<div></div>" (P.html p) ;
  quiet p ;
  equal string "<div></div>" (P.html p) ;
  no_errors p

(* A stop in the fallback function: the root writes no DOM. *)
let stop_in_fallback () =
  let page = ref None in
  let p =
    page_app ~page
      ~update:(fun _ _ -> raise Boom)
      ~fallback:(fun _ -> stop page ; txt "fallback")
      ()
  in
  click p ;
  equal string "<div></div>" (P.html p) ;
  quiet p ;
  equal string "<div></div>" (P.html p)

(* A stop in update, which then gives a task: the dead root gets no request. *)
let no_request () =
  let page = ref None and q = Queue.create () in
  let update m = function
    | Go ->
        stop page ;
        (m + 1, Cmd.task (Async.map (fun () -> Got 1) (waiting q)))
    | Got _ ->
        (m, Cmd.none)
  in
  let p = page_app ~page ~update () in
  click p ;
  quiet p ;
  equal int ~msg:"no task for a dead root" 0 (Queue.length q) ;
  equal int 0 (ins p).token_children ;
  equal int 0 (ins p).requests_left ;
  no_errors p

(* An end action of the old token of a Cmd.latest key stops the page, in the
   middle of a batch: the rest of the batch adds no key and no request. *)
type lmsg = First | Second

let end_action_in_batch () =
  let page = ref None in
  let hang =
    Async.make (fun tok ~ok:_ ~error:_ ->
        Cancel.on_end tok (fun () -> stop page) )
  in
  let never = Async.make (fun _ ~ok:_ ~error:_ -> ()) in
  let update n = function
    | First ->
        (n + 1, Cmd.latest ~key:"k" hang)
    | Second ->
        ( n + 1
        , Cmd.batch
            [ Cmd.latest ~key:"k" never
            ; Cmd.latest ~key:"j" never
            ; Cmd.task never ] )
  in
  let view n =
    El.div [button ~id:"a" First (string_of_int n); button Second "b"]
  in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update ~view
         ~fallback:(fun _ -> txt "fallback")
         () )
  in
  page := Some p ;
  P.click p (By.test_id "a") ;
  click p ;
  quiet p ;
  is_false (ins p).alive ;
  equal int 0 (ins p).latest_keys ;
  equal int 0 (ins p).requests_left ;
  equal int 0 (ins p).token_children

(* An end action stops the page while a task gives its result (outside a turn):
   no continuation stays in the run queue. *)
let end_action_at_ok () =
  let page = ref None and k = ref ignore in
  let t =
    Async.make (fun tok ~ok ~error:_ ->
        Cancel.on_end tok (fun () -> Option.iter P.stop !page) ;
        k := fun () -> ok Tick )
  in
  let p = P.start (app ~init:(fun () -> (0, Cmd.task t)) ()) in
  page := Some p ;
  H.raw (P.host p) (fun () -> !k ()) ;
  P.run p ;
  is_false (ins p).alive ;
  equal int 0 (ins p).continuations_left ;
  equal string "<div></div>" (P.html p) ;
  no_errors p

let tests =
  [ group "errors"
      [ test "a view that raises: the fallback, and no task stays" in_view
      ; test "a DOM fault in the patch: the fallback, and turns go on" in_dom
      ; test "init raises: the fallback at the start" in_init
      ; test "a task error and a message function that raises" in_task
      ; test "an update that raises: the fallback at the next render" in_update
      ; test "a task error releases its token" error_release
      ; test "a fault in the mount of the fallback" no_tree
      ; test "an init task that fails at once: one fallback render" dirty_init
      ; test "a click with a model change and a failed task: one render"
          dirty_click
      ; test "a model change, then an update that raises: one render"
          dirty_update
      ; test "a function of Cmd.map that raises" in_map
      ; test "a fault in a re-entrant event inside update: no render after"
          fault_in_update
      ; test "the write of the fallback raises: an empty text" fallback_write
      ; test "a function of Cmd.map that stops the page: no update" map_stops
      ; test "a stop in view: no DOM write" stop_in_view
      ; test "a stop in the fallback: no DOM write" stop_in_fallback
      ; test "a fault and an error in view: the fallback gets the first"
          fault_in_view
      ; test "a stop in update: no request" no_request
      ; test "an end action in a batch stops the page" end_action_in_batch
      ; test "an end action stops the page at a result" end_action_at_ok ] ]
