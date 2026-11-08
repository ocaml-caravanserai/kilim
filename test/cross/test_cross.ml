(* Stack depth and the once rule in each build (design section 12). Under
   js_of_ocaml, a chain of 2,000 synchronous steps overflowed the stack before
   the trampoline. Also keyed diffs of 10,000 rows on the fake DOM (design
   section 14), the hash of the keys, and the big loops of the turn
   (Cross_turn), and the JSON corpus of kilim.http with the range of Decode.int
   (Json_corpus). *)

open Kilim
open Async.Syntax
module C = Private.Cancel
module Q = Private.Run_queue

let failures = ref 0

let check name ok =
  print_endline ((if ok then "ok   " else "FAIL ") ^ name) ;
  if not ok then incr failures

let host =
  { Private.Host.now_ms= (fun () -> 0.)
  ; set_timer= (fun ~ms:_ _ () -> ())
  ; request_frame= ignore
  ; queue_microtask= ignore
  ; post_task= ignore }

let result ?(tok = C.root ()) t =
  let q = Q.create ~schedule:ignore in
  let r = ref None in
  ( match
      Q.with_turn q (fun () ->
          Private.Async.start ~host ~run:q tok t
            ~ok:(fun x -> r := Some (Ok x))
            ~error:(fun e -> r := Some (Error e)) ;
          Q.drain q )
    with
  | () ->
      ()
  | exception Stack_overflow ->
      r := Some (Error Stack_overflow) ) ;
  !r

module F = Kilim_testing.Fake_dom

let rows keys =
  View.keyed El.ul
    (List.map (fun k -> (Key.int k, El.li [txt (string_of_int k)])) keys)

(* A shuffle with a fixed seed, with no Random: a small linear congruential
   generator whose values stay below 2^30 in each build. *)
let shuffle l =
  let a = Array.of_list l in
  let x = ref 12345 in
  for i = Array.length a - 1 downto 1 do
    x := ((!x * 75) + 74) mod 65537 ;
    let j = !x mod (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- t
  done ;
  Array.to_list a

let keyed_diffs () =
  let n = 10_000 in
  let keys = List.init n (fun i -> i) in
  let doc = F.create () in
  let root = F.element "div" in
  let tree =
    Private.Diff.mount (F.ops doc) ~parent:root
      ~post:(Private.Dom.post_now ignore)
      (rows keys)
  in
  let ul = Private.Diff.dom (Private.Diff.root tree) in
  let texts () = List.map F.text (F.children ul) in
  let same keys = texts () = List.map string_of_int keys in
  let patch name keys ~moves =
    F.reset doc ;
    match Private.Diff.patch tree (rows keys) with
    | () ->
        check name (same keys && moves (F.moves doc))
    | exception Stack_overflow ->
        check (name ^ ": stack overflow") false
  in
  check "a mount of 10,000 keyed rows" (same keys) ;
  patch "a reverse of 10,000 keyed rows" (List.rev keys) ~moves:(fun m ->
      m = n - 1 ) ;
  let mixed = shuffle keys in
  check "the shuffle is a permutation" (List.sort compare mixed = keys) ;
  patch "a shuffle of 10,000 keyed rows" mixed ~moves:(fun m -> m > 0 && m < n) ;
  patch "a shuffle back to the order" keys ~moves:(fun m -> m > 0 && m < n) ;
  patch "a replace of 10,000 keyed rows with the keys 20000.."
    (List.map (fun k -> k + 20_000) keys)
    ~moves:(fun m -> m = 0) ;
  patch "a removal of 10,000 keyed rows" [] ~moves:(fun m -> m = 0)

(* The hash of a key is the same in the three builds, for an int below 2^30 and
   for a string. Thus the table reads the same slots. *)
let hashes () =
  let h = Private.Key.hash in
  check "the hash of an int"
    ( List.map (fun x -> h (Key.int x)) [0; 1; 2; 7; 1000; 0x3fff_ffff]
    = [0; 749930575; 719903339; 450586307; 789315851; 586705084] ) ;
  check "the hash of a string"
    ( List.map
        (fun x -> h (Key.string x))
        [""; "a"; "row-1"; "Aa"; "BB"; "\255\255"]
    = [493303829; 122118257; 232771046; 110798886; 566909467; 923304462] ) ;
  let n = 10_000 in
  let ints f = Array.init n (fun i -> Key.int (f i)) in
  let probes name nk ok =
    check name
      ( match Private.Diff.probes nk ok with
      | Some c ->
          c < 4 * n
      | None ->
          false )
  in
  probes "the probes of 10,000 keys shifted by 20000"
    (ints (fun i -> i + 20_000))
    (ints (fun i -> i)) ;
  probes "the probes of 10,000 keys with a stride of 20001"
    (ints (fun i -> i * 20_001))
    (ints (fun i -> (n - 1 - i) * 20_001)) ;
  probes "the probes of 10,000 string keys"
    (Array.init n (fun i -> Key.string ("row-" ^ string_of_int i)))
    (Array.init n (fun i -> Key.string ("row-" ^ string_of_int (n - 1 - i)))) ;
  (* A table with half of the slots full: the plan uses no map. *)
  List.iter
    (fun k ->
      let fit = ref true in
      for t = 0 to 19 do
        let off = t * 1_000_003 in
        match
          Private.Diff.probes
            (Array.init k (fun i -> Key.int (off + i)))
            (Array.init k (fun i -> Key.int (off + k + i)))
        with
        | Some c ->
            if c >= 6 * k then fit := false
        | None ->
            fit := false
      done ;
      check
        ("the probes of " ^ string_of_int k ^ " keys, 20 replaces with offsets")
        !fit )
    [16_384; 65_536]

let () =
  let n = 1_000_000 in
  let rec loop k =
    if k = 0 then Async.return 0
    else
      let* () = Async.return () in
      loop (k - 1)
  in
  check "a right-nested chain of 1,000,000 steps" (result (loop n) = Some (Ok 0)) ;
  let left =
    List.fold_left
      (fun acc _ -> Async.bind acc (fun x -> Async.return (x + 1)))
      (Async.return 0)
      (List.init n (fun i -> i))
  in
  check "a left-nested chain of 1,000,000 steps" (result left = Some (Ok n)) ;
  let count = ref 0 in
  let r =
    result
      (let* x = Async.make (fun _ ~ok ~error:_ -> ok 1 ; ok 2) in
       incr count ; Async.return x )
  in
  check "once: the second ok does nothing" (r = Some (Ok 1) && !count = 1) ;
  let m = 100_000 in
  let never = Async.make (fun _ ~ok:_ ~error:_ -> ()) in
  let t =
    List.fold_left
      (fun acc _ ->
        let+ x, _ =
          let+ x = acc and+ y = never in
          (x, y)
        in
        x )
      (Async.return 0)
      (List.init m (fun i -> i))
  in
  let tok = C.root () in
  check "100,000 nested and+: the start" (result ~tok t = None) ;
  check "100,000 nested and+: the cancel"
    (match C.cancel tok with () -> true | exception Stack_overflow -> false) ;
  (* An error through 100,000 nested and+, and an ok through 100,000 nested
     first: the results go through the trampoline. *)
  let t =
    List.fold_left
      (fun acc _ ->
        let+ x, _ =
          let+ x = acc and+ y = Async.return () in
          (x, y)
        in
        x )
      (Async.fail Exit : int Async.t)
      (List.init m (fun i -> i))
  in
  check "an error through 100,000 nested and+" (result t = Some (Error Exit)) ;
  let t =
    List.fold_left
      (fun acc _ -> Async.first [acc])
      (Async.return 7)
      (List.init m (fun i -> i))
  in
  check "an ok through 100,000 nested first" (result t = Some (Ok 7)) ;
  let t =
    List.fold_left
      (fun acc _ -> Async.first [acc])
      (Async.fail Exit : int Async.t)
      (List.init m (fun i -> i))
  in
  check "an error through 100,000 nested first" (result t = Some (Error Exit)) ;
  (* An ok and an error through 100,000 nested timeout, and an ok through
     100,000 nested and+ with no let+ around a level: each result goes through
     the trampoline. *)
  let rec nest_timeout : type a. int -> a Async.t -> unit Async.t =
   fun k t ->
    if k = 0 then Async.map ignore t
    else
      nest_timeout (k - 1)
        (Async.map (fun _ -> ()) (Async.timeout (Span.s 1) t))
  in
  check "an ok through 100,000 nested timeout"
    (result (nest_timeout m (Async.return 7)) = Some (Ok ())) ;
  check "an error through 100,000 nested timeout"
    (result (nest_timeout m (Async.fail Exit : int Async.t)) = Some (Error Exit)) ;
  let rec nest_both : type a. int -> a Async.t -> unit Async.t =
   fun k t ->
    if k = 0 then Async.map ignore t
    else nest_both (k - 1) (Async.Syntax.( and+ ) t (Async.return ()))
  in
  check "an ok through 100,000 nested and+ with no let+"
    (result (nest_both m (Async.return 7)) = Some (Ok ())) ;
  keyed_diffs () ;
  hashes () ;
  Cross_turn.run check ;
  Cross_tree.run check ;
  Json_corpus.run check ;
  Json_corpus.int_ranges check ;
  if !failures > 0 then exit 1
