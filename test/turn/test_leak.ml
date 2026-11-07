(* The leak tests of the turn (design section 14): the latest table, the task
   tokens, the timers and the model of a root that failed. *)

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

let tests =
  [ group "leaks"
      [ test "1,000 keys with synchronous results leave an empty table" sync_keys
      ; test "1,000 keys that wait leave an empty table after idle" pending_keys
      ; test "1,000 starts, clicks and stops leave no timer" starts
      ; test "the model of a root that failed is free" model_free ] ]
