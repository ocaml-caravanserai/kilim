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

(* The subscriptions as values: the leaves, their keys and the duplicate keys of
   Sub.run. *)
module S = Private.Sub

let other : int S.kind = S.kind ()

(* The kind, the params and the position of each leaf: "run", or "other" for the
   kind [other], else "every". The key of an auto leaf has the uid of its
   witness. *)
let keys (s : 'msg Sub.t) =
  List.map
    (function
      | S.Run_leaf (k, _) ->
          ("run", k, 0)
      | S.Auto_leaf (k, kind, _, _) ->
          equal int ~msg:"the uid of an auto key" (Type.Id.uid kind) k.uid ;
          ( (if Type.Id.uid kind = Type.Id.uid other then "other" else "every")
          , k.params
          , k.nth ) )
    (S.leaves s)

let ks = list (triple string string int)

let run k : unit Sub.t = Sub.run ~key:k (fun _ -> Async.return ())

let leaves () =
  equal ks [] (keys Sub.none) ;
  equal ks [] (keys (Sub.batch [Sub.none; Sub.batch []])) ;
  equal ks
    [ ("every", "100", 0)
    ; ("run", "100", 0)
    ; ("every", "200", 0)
    ; ("other", "100", 0)
    ; ("every", "100", 1)
    ; ("other", "100", 1)
    ; ("other", "200", 0) ]
    (keys
       (Sub.batch
          [ Sub.every (Span.ms 100) ()
          ; run "100"
          ; Sub.none
          ; Sub.batch
              [ Sub.every (Span.ms 200) ()
              ; S.auto other ~params:"100"
                  ~start:(fun _ -> Async.return ())
                  ignore
              ; Sub.every (Span.ms 100) () ]
          ; S.auto other ~params:"100" ~start:(fun _ -> Async.return ()) ignore
          ; S.auto other ~params:"200" ~start:(fun _ -> Async.return ()) ignore
          ] ) ) ;
  raises_match
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> S.leaves (Sub.batch [run "a"; run "b"; Sub.batch [run "a"]])) ;
  raises_match
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Sub.every Span.zero ()) ;
  equal ks [("every", "1", 0)] (keys (Sub.every (Span.ms 1) ()))

(* Two keys are equal only if their kind, params and position are equal. *)
let key_compare () =
  let a uid params nth = S.Key.Auto {uid; params; nth} in
  let sign k1 k2 = Int.compare (S.Key.compare k1 k2) 0 in
  equal int 0 (sign (S.Key.Run "a") (S.Key.Run "a")) ;
  equal int (-1) (sign (S.Key.Run "a") (S.Key.Run "b")) ;
  equal int 1 (sign (S.Key.Run "b") (S.Key.Run "a")) ;
  equal int (-1) (sign (S.Key.Run "z") (a 0 "a" 0)) ;
  equal int 1 (sign (a 0 "a" 0) (S.Key.Run "z")) ;
  equal int 0 (sign (a 1 "a" 0) (a 1 "a" 0)) ;
  equal int (-1) (sign (a 0 "b" 1) (a 1 "a" 0)) ;
  equal int 1 (sign (a 1 "a" 0) (a 0 "b" 1)) ;
  equal int (-1) (sign (a 1 "a" 1) (a 1 "b" 0)) ;
  equal int 1 (sign (a 1 "b" 0) (a 1 "a" 1)) ;
  equal int (-1) (sign (a 1 "a" 0) (a 1 "a" 1)) ;
  equal int 1 (sign (a 1 "a" 1) (a 1 "a" 0))

let deep_subs () =
  let n = 10_000 in
  let left =
    List.fold_left
      (fun acc i -> Sub.batch [acc; run (string_of_int i)])
      Sub.none
      (List.init n (fun i -> i))
  in
  equal (list string)
    (List.init n string_of_int)
    (List.map (fun (_, k, _) -> k) (keys left))

(* A tree of subscriptions, its value, and a naive recursive flatten (the
   oracle). The oracle finds the position of each auto leaf and the duplicate
   keys of run with lists. *)
type sd =
  | Sd_none
  | Sd_batch of sd list
  | Sd_run of string
  | Sd_every of int
  | Sd_other of string

let rec sbuild = function
  | Sd_none ->
      Sub.none
  | Sd_batch l ->
      Sub.batch (List.map sbuild l)
  | Sd_run k ->
      run k
  | Sd_every n ->
      Sub.every (Span.ms n) ()
  | Sd_other p ->
      S.auto other ~params:p ~start:(fun _ -> Async.return ()) ignore

let rec sflat = function
  | Sd_none ->
      []
  | Sd_batch l ->
      List.concat_map sflat l
  | Sd_run k ->
      [("run", k)]
  | Sd_every n ->
      [("every", string_of_int n)]
  | Sd_other p ->
      [("other", p)]

let soracle d =
  let flat = sflat d in
  let runs = List.filter (fun (n, _) -> String.equal n "run") flat in
  let dup =
    List.exists
      (fun (_, k) ->
        List.length (List.filter (fun (_, k') -> String.equal k k') runs) > 1 )
      runs
  in
  if dup then None
  else
    let rec number seen = function
      | [] ->
          []
      | ((n, p) as x) :: rest ->
          let nth =
            if String.equal n "run" then 0
            else List.length (List.filter (( = ) x) seen)
          in
          (n, p, nth) :: number (x :: seen) rest
    in
    Some (number [] flat)

let rec gen_sd depth =
  let leaf =
    Gen.one_of
      [ Gen.constant Sd_none
      ; Gen.map (fun k -> Sd_run k) (Gen.of_list ["a"; "b"; "c"; "100"])
      ; Gen.map (fun n -> Sd_every n) (Gen.of_list [100; 200])
      ; Gen.map (fun p -> Sd_other p) (Gen.of_list ["100"; "x"]) ]
  in
  if depth = 0 then leaf
  else
    Gen.frequency
      [ (3, leaf)
      ; ( 2
        , Gen.map
            (fun l -> Sd_batch l)
            (Gen.list ~size:(Gen.int_range 0 4) (gen_sd (depth - 1))) ) ]

let leaves_law d =
  let got =
    match keys (sbuild d) with
    | l ->
        Some l
    | exception Invalid_argument _ ->
        None
  in
  equal (option ks) (soracle d) got

let tests =
  [ group "values"
      [ test "the requests in the order of the tree" order
      ; test "the key paths: the scopes, outer first, then the key" paths
      ; test "the maps compose, inner first" maps
      ; test "a deep batch and a deep map do not overflow the stack" deep
      ; prop ~count:300 "the requests are a naive flatten of the tree" (gen_d 6)
          flatten_law
      ; test "the leaves of a subscription and their keys" leaves
      ; test "the order of the keys" key_compare
      ; test "a deep batch of subscriptions does not overflow the stack"
          deep_subs
      ; prop ~count:300 "the leaves are a naive flatten of the tree" (gen_sd 6)
          leaves_law ] ]
