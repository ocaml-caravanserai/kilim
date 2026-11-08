(* The outputs of a component (design sections 08 and 09): Ctx.emit puts the
   output at the back of the queue as an item of the parent, the newest ~on maps
   it when it leaves the queue, and its errors go to the parent. *)

open Windtrap
open Kilim
open Support
module C = Private.Component

exception Boom

(* A counter with an output. A click on its button adds 1 and gives the new
   count. A click on its second button gives two outputs around a task: the task
   result adds 1 later in the turn. *)
module Count = struct
  type props = string

  type model = int

  type msg = Incr | Twice

  type out = Changed of int

  let id : (props, model, msg, out) C.id = C.id ()

  let init _ = (0, Cmd.none)

  let update ctx _ n = function
    | Incr ->
        (n + 1, Ctx.emit ctx (Changed (n + 1)))
    | Twice ->
        ( n
        , Cmd.batch
            [ Ctx.emit ctx (Changed 10)
            ; Cmd.task (Async.return Incr)
            ; Cmd.map (fun m -> m) (Ctx.emit ctx (Changed 20)) ] )

  let view label n =
    El.span
      [ button ~id:label Incr (string_of_int n)
      ; button ~id:(label ^ "2") Twice "t" ]

  let subs _ _ = Sub.none

  let equal_props = Component.shallow_equal
end

type rmsg = Got of int * int | Bump | Outer | Raise

(* The root. Its model is a count. [on] gives [Got (count, n)]. A click on the
   div of the root gives [Outer]. *)
let app ?(on = fun k (Count.Changed n) -> Got (k, n)) ?fallback () =
  let got = ref [] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> (0, Cmd.none))
         ~update:(fun k m ->
           got := m :: !got ;
           match m with
           | Bump ->
               (k + 1, Cmd.none)
           | Raise ->
               raise Boom
           | Got _ | Outer ->
               (k, Cmd.none) )
         ~view:(fun k ->
           El.div
             ~ev:[Ev.click Outer]
             [ button ~id:"p" Bump (string_of_int k)
             ; button ~id:"r" Raise "r"
             ; C.v (module Count) ~on:(on k) "c" ] )
         ?fallback () )
  in
  let take () =
    let l = List.rev !got in
    got := [] ;
    l
  in
  (p, take)

let show = function
  | Got (k, n) ->
      "got " ^ string_of_int k ^ " " ^ string_of_int n
  | Bump ->
      "bump"
  | Outer ->
      "outer"
  | Raise ->
      "raise"

let shown take = List.map show (take ())

(* The click gives the message of the child, then [Outer] of the root. The
   output comes after [Outer], in the same turn, with one render. *)
let back () =
  let p, take = app () in
  let v = (stats p).views and r = (stats p).renders in
  P.click p (By.test_id "c") ;
  equal (list string) ["outer"; "got 0 1"] (shown take) ;
  equal int ~msg:"one render" (r + 1) (stats p).renders ;
  equal int ~msg:"the model of the root stays: the view of the child only"
    (v + 1) (stats p).views ;
  equal string "1" (F.text (P.find p (By.test_id "c"))) ;
  settled p ;
  no_errors p ;
  quiet p

(* The outputs go to the queue at once, in the order of the command. The task
   result comes later in the settle step. *)
let order () =
  let p, take = app () in
  P.click p (By.test_id "c2") ;
  equal (list string) ["outer"; "got 0 10"; "got 0 20"; "got 0 1"] (shown take) ;
  no_errors p

(* A render of the root gives a new ~on, with equal props: the child does not
   render, and the newest ~on maps its output. *)
let newest () =
  let p, take = app () in
  P.click p (By.test_id "p") ;
  ignore (take ()) ;
  let v = (stats p).views in
  P.click p (By.test_id "c") ;
  equal (list string) ["outer"; "got 1 1"] (shown take) ;
  equal int ~msg:"the view of the child only" (v + 1) (stats p).views ;
  no_errors p

(* A click on the child gives its message, then [Raise] of a handler of the root
   around it. The root catch unmounts the root before the output leaves the
   queue: the item is dropped, and ~on does not run. *)
let dead_parent () =
  let ons = ref 0 in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () m ->
           match m with Raise -> raise Boom | _ -> ((), Cmd.none) )
         ~view:(fun () ->
           El.div
             ~ev:[Ev.click Raise]
             [ C.v
                 (module Count)
                 ~on:(fun (Count.Changed n) ->
                   incr ons ;
                   Got (0, n) )
                 "c" ] )
         ~fallback:(fun _ -> txt "f")
         () )
  in
  let d = (stats p).dropped in
  P.click p (By.test_id "c") ;
  equal int ~msg:"~on does not run" 0 !ons ;
  equal int ~msg:"the output is dropped" (d + 1) (stats p).dropped ;
  equal string "f" (F.text (P.root p)) ;
  equal int 1 (List.length (errors p)) ;
  quiet p

(* An error in ~on goes to the error scope of the parent: here the root
   catch. *)
let on_raises () =
  let p, _ = app ~on:(fun _ _ -> raise Boom) ~fallback:(fun _ -> txt "f") () in
  P.click p (By.test_id "c") ;
  is_true (match errors p with [Boom] -> true | _ -> false) ;
  equal string "f" (F.text (P.root p)) ;
  equal int 0 (P.tree p).instances ;
  settled p ;
  quiet p

(* The middle level maps each output of its child to its own output. *)
module Mid = struct
  type props = unit

  type model = int

  type msg = From of int

  type out = Total of int

  let id : (props, model, msg, out) C.id = C.id ()

  let init () = (0, Cmd.none)

  let update ctx () t (From n) = (t + n, Ctx.emit ctx (Total (t + n)))

  let view () t =
    El.div
      [ El.output [txt (string_of_int t)]
      ; C.v (module Count) ~on:(fun (Count.Changed n) -> From n) "c" ]

  let subs _ _ = Sub.none

  let equal_props = Component.shallow_equal
end

(* An output of a nested child reaches the root through its parent, in the same
   turn. The parent renders one time in the turn, with its child. *)
let nested () =
  let got = ref [] in
  let p =
    P.start
      (App.make
         ~init:(fun () -> ((), Cmd.none))
         ~update:(fun () n ->
           got := n :: !got ;
           ((), Cmd.none) )
         ~view:(fun () ->
           El.div [C.v (module Mid) ~on:(fun (Mid.Total n) -> n) ()] )
         () )
  in
  let v = (stats p).views and r = (stats p).renders in
  P.click p (By.test_id "c") ;
  P.click p (By.test_id "c") ;
  equal (list int) [1; 3] (List.rev !got) ;
  equal int ~msg:"one render for each click" (r + 2) (stats p).renders ;
  equal int ~msg:"the views of the child and of its parent" (v + 4)
    (stats p).views ;
  equal string "32t" (F.text (P.root p)) ;
  no_errors p ;
  quiet p

let tests =
  [ group "outputs"
      [ test
          "Ctx.emit puts the output at the back of the queue in the same turn"
          back
      ; test "the outputs keep the order of the command" order
      ; test "the newest on function maps an output" newest
      ; test "an output to a dead parent is dropped" dead_parent
      ; test "an error in on goes to the root catch" on_raises
      ; test "an output of a nested child reaches the root" nested ] ]
