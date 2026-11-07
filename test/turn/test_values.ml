(* The commands as values (design section 08): the order of the requests, the
   key paths and the maps. *)

open Windtrap
open Kilim
module C = Private.Cmd

let host =
  { Private.Host.now_ms= (fun () -> 0.)
  ; set_timer= (fun ~ms:_ _ () -> ())
  ; request_frame= ignore
  ; queue_microtask= ignore
  ; post_task= ignore }

(* The key and the message of each request. Each task here has a synchronous
   result. *)
let results (c : 'msg Cmd.t) =
  List.map
    (fun (C.Request {key; task; to_msg}) ->
      let r = ref None in
      Private.Async.start ~host
        ~run:(Private.Run_queue.create ~schedule:ignore)
        (Private.Cancel.root ()) task
        ~ok:(fun x -> r := Some (to_msg x))
        ~error:raise ;
      match !r with Some m -> (key, m) | None -> fail "no result" )
    (C.requests c)

let key = option (list string)

let rs = list (pair key int)

let order () =
  let t n = Async.return n in
  equal rs [] (results Cmd.none) ;
  equal rs [] (results (Cmd.batch [Cmd.none; Cmd.batch []])) ;
  equal rs
    [(None, 1); (None, 20); (Some ["k"], 30); (Some ["x"], 4); (None, 5)]
    (results
       (Cmd.batch
          [ Cmd.task (t 1)
          ; Cmd.map
              (fun x -> x * 10)
              (Cmd.batch [Cmd.task (t 2); Cmd.latest ~key:"k" (t 3)])
          ; Cmd.latest ~key:"x" (t 4)
          ; Cmd.none
          ; Cmd.task (t 5) ] ) )

let paths () =
  let t = Async.return 0 in
  let keys c = List.map fst (results c) in
  equal (list key)
    [Some ["a"; "b"]]
    (keys (Cmd.map ~scope:"a" Fun.id (Cmd.latest ~key:"b" t))) ;
  equal (list key) [Some ["a/b"]] (keys (Cmd.latest ~key:"a/b" t)) ;
  equal (list key)
    [Some ["a"; "b"; "k"]; Some ["a"; "k"]; None]
    (keys
       (Cmd.map ~scope:"a" Fun.id
          (Cmd.batch
             [ Cmd.map Fun.id (Cmd.map ~scope:"b" Fun.id (Cmd.latest ~key:"k" t))
             ; Cmd.latest ~key:"k" t
             ; Cmd.task t ] ) ) )

(* The inner function first. *)
let maps () =
  equal rs
    [(None, 70)]
    (results
       (Cmd.map
          (fun x -> x * 10)
          (Cmd.map (fun x -> x + 2) (Cmd.task (Async.return 5))) ) ) ;
  equal
    (list (pair key string))
    [(None, "6")]
    (results
       (Cmd.map string_of_int
          (Cmd.map (fun x -> x + 1) (Cmd.task (Async.return 5))) ) )

let deep () =
  let n = 10_000 in
  let left =
    List.fold_left
      (fun acc i -> Cmd.batch [acc; Cmd.task (Async.return i)])
      Cmd.none
      (List.init n (fun i -> i))
  in
  equal (list int) (List.init n (fun i -> i)) (List.map snd (results left)) ;
  let nested =
    List.fold_left
      (fun acc _ -> Cmd.map (fun x -> x + 1) acc)
      (Cmd.task (Async.return 0))
      (List.init n (fun i -> i))
  in
  equal rs [(None, n)] (results nested)

(* A tree of commands, its value, and a naive recursive flatten (the oracle). *)
type d =
  | D_none
  | D_batch of d list
  | D_task of int
  | D_latest of string * int
  | D_map of string option * int * d

let rec build = function
  | D_none ->
      Cmd.none
  | D_batch l ->
      Cmd.batch (List.map build l)
  | D_task n ->
      Cmd.task (Async.return n)
  | D_latest (k, n) ->
      Cmd.latest ~key:k (Async.return n)
  | D_map (scope, a, d) ->
      Cmd.map ?scope (fun x -> (x * 3) + a) (build d)

let rec oracle = function
  | D_none ->
      []
  | D_batch l ->
      List.concat_map oracle l
  | D_task n ->
      [(None, n)]
  | D_latest (k, n) ->
      [(Some [k], n)]
  | D_map (scope, a, d) ->
      List.map
        (fun (key, x) ->
          let key =
            match scope with
            | Some s ->
                Option.map (fun p -> s :: p) key
            | None ->
                key
          in
          (key, (x * 3) + a) )
        (oracle d)

let rec gen_d depth =
  let leaf =
    Gen.one_of
      [ Gen.constant D_none
      ; Gen.map (fun n -> D_task n) (Gen.int_range 0 9)
      ; Gen.map
          (fun (k, n) -> D_latest (k, n))
          (Gen.pair (Gen.of_list ["a"; "b"; "a/b"]) (Gen.int_range 0 9)) ]
  in
  if depth = 0 then leaf
  else
    let sub = gen_d (depth - 1) in
    Gen.frequency
      [ (2, leaf)
      ; ( 2
        , Gen.map (fun l -> D_batch l) (Gen.list ~size:(Gen.int_range 0 4) sub)
        )
      ; ( 2
        , Gen.map
            (fun (s, a, d) -> D_map (s, a, d))
            (Gen.triple
               (Gen.option (Gen.of_list ["a"; "b"]))
               (Gen.int_range 0 5) sub ) ) ]

let flatten_law d = equal rs (oracle d) (results (build d))

let tests =
  [ group "values"
      [ test "the requests in the order of the tree" order
      ; test "the key paths: the scopes, outer first, then the key" paths
      ; test "the maps compose, inner first" maps
      ; test "a deep batch and a deep map do not overflow the stack" deep
      ; prop ~count:300 "the requests are a naive flatten of the tree" (gen_d 6)
          flatten_law ] ]
