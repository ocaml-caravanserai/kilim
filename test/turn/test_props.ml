(* The turn on random schedules (design section 14): after each action no turn
   runs, a pending render has a callback, and no stale latest result reaches
   update. After idle, all queues are empty, and the DOM is a fresh mount of the
   view of the model. *)

open Windtrap
open Kilim
open Support

type how = Now | Raw of int | Micro | Sleep of int

type task = {how: how; key: string option}

type action =
  | Click of task list
  | Focus
  | Send
  | Advance of int
  | Hidden of bool

type msg =
  | Clicked
  | Focused
  | Sent
  | Got of string option * int  (** The key and the generation. *)

type model =
  { count: int
  ; results: int  (** The results with no key. *)
  ; last: (string * int) list  (** The last generation of each key. *) }

let pp_how = function
  | Now ->
      "now"
  | Raw d ->
      "raw " ^ string_of_int d
  | Micro ->
      "micro"
  | Sleep d ->
      "sleep " ^ string_of_int d

let pp_action = function
  | Click l ->
      "click ["
      ^ String.concat "; "
          (List.map
             (fun t ->
               pp_how t.how ^ match t.key with Some k -> " @" ^ k | None -> "" )
             l )
      ^ "]"
  | Focus ->
      "focus"
  | Send ->
      "send"
  | Advance n ->
      "advance " ^ string_of_int n
  | Hidden b ->
      "hidden " ^ string_of_bool b

let gen_task =
  Gen.map
    (fun (how, key) -> {how; key})
    (Gen.pair
       (Gen.one_of
          [ Gen.constant Now
          ; Gen.map (fun d -> Raw d) (Gen.int_range 0 30)
          ; Gen.constant Micro
          ; Gen.map (fun d -> Sleep d) (Gen.int_range 0 30) ] )
       (Gen.of_list [None; Some "a"; Some "b"]) )

let gen_action =
  Gen.frequency
    [ ( 4
      , Gen.map (fun l -> Click l) (Gen.list ~size:(Gen.int_range 0 3) gen_task)
      )
    ; (1, Gen.constant Focus)
    ; (2, Gen.constant Send)
    ; (2, Gen.map (fun n -> Advance n) (Gen.int_range 1 40))
    ; (1, Gen.map (fun b -> Hidden b) Gen.bool) ]

let gen =
  Gen.with_pp
    (fun ppf (c, l) ->
      Format.pp_print_string ppf
        ( "cost " ^ string_of_float c ^ ": "
        ^ String.concat ", " (List.map pp_action l) ) )
    (Gen.pair
       (Gen.of_list [1. /. 128.; 1.; 12.5])
       (Gen.list ~size:(Gen.int_range 1 30) gen_action) )

let senders = 40

let view m =
  El.div
    [ button Clicked (string_of_int m.count)
    ; El.input ~at:[At.data "testid" "f"] ~ev:[Ev.focus Focused] ()
    ; txt (string_of_int m.results)
    ; El.ul
        (List.map
           (fun (k, g) -> El.li [txt (k ^ string_of_int g)])
           (List.sort compare m.last) ) ]

let law (cost, actions) =
  let h = H.create ~cost () in
  let plans = Queue.create () in
  let q = Queue.create () in
  (* The newest generation of each key, and the tasks with no key. *)
  let newest = ref [] and keyless = ref 0 in
  let last_model = ref None in
  let task {how; key} =
    let g =
      match key with
      | Some k ->
          let g = 1 + Option.value ~default:0 (List.assoc_opt k !newest) in
          newest := (k, g) :: List.remove_assoc k !newest ;
          g
      | None ->
          incr keyless ; 0
    in
    let v = Got (key, g) in
    let t =
      match how with
      | Now ->
          sync v
      | Raw d ->
          Async.make (fun _ ~ok ~error:_ -> H.raw h ~after:d (fun () -> ok v))
      | Micro ->
          Async.make (fun _ ~ok ~error:_ -> H.raw_microtask h (fun () -> ok v))
      | Sleep d ->
          Async.map (fun () -> v) (Async.sleep (Span.ms d))
    in
    match key with Some k -> Cmd.latest ~key:k t | None -> Cmd.task t
  in
  let update m msg =
    let m, cmd =
      match msg with
      | Clicked ->
          ( {m with count= m.count + 1}
          , Cmd.batch (List.map task (Queue.pop plans)) )
      | Focused | Sent ->
          ({m with count= m.count + 1}, Cmd.none)
      | Got (None, _) ->
          ({m with results= m.results + 1}, Cmd.none)
      | Got (Some k, g) ->
          equal int ~msg:"the newest generation of the key"
            (List.assoc k !newest) g ;
          ({m with last= (k, g) :: List.remove_assoc k m.last}, Cmd.none)
    in
    last_model := Some m ;
    (m, cmd)
  in
  let init () =
    let m = {count= 0; results= 0; last= []} in
    last_model := Some m ;
    (m, Cmd.batch (List.init senders (fun _ -> Cmd.task (waiting q))))
  in
  let p = P.start ~host:h (App.make ~init ~update ~view ()) in
  let send () = match Queue.take_opt q with Some ok -> ok Sent | None -> () in
  List.iter
    (fun a ->
      ( match a with
      | Click l ->
          Queue.push l plans ; click p
      | Focus ->
          P.focus p (By.test_id "f")
      | Send ->
          H.raw h send ; P.run p
      | Advance n ->
          P.advance p (Span.ms n)
      | Hidden b ->
          P.set_hidden p b ) ;
      settled p )
    actions ;
  H.raw h (fun () -> Queue.iter (fun ok -> ok Sent) q) ;
  H.idle h ;
  P.set_hidden p false ;
  H.idle h ;
  settled p ;
  let i = ins p in
  equal int 0 i.messages_left ;
  equal int 0 i.continuations_left ;
  equal int 0 i.requests_left ;
  is_false i.turn_requested ;
  is_false i.pending ;
  is_false i.scheduled ;
  equal int 0 i.latest_keys ;
  equal int 0 i.token_children ;
  no_errors p ;
  let m = Option.get !last_model in
  equal string ~msg:"a fresh mount" (fresh (view m)) (F.dump (P.root p)) ;
  equal int ~msg:"each result with no key" !keyless m.results ;
  equal
    (list (pair string int))
    ~msg:"the last task of each key"
    (List.sort compare !newest)
    (List.sort compare m.last)

(* The turn with errors: updates, views, message functions and tasks that raise
   or fail, held results, re-entrant events (a click, a raising event or a stop)
   from update, from view and from a message function, and a stop. Each message
   reaches update at most one time, no update runs after a root error or a stop,
   and at the end nothing stays. The checks inside update and view only record a
   violation, because an exception there is a root catch. *)

exception Boom

type ehow =
  | E_now
  | E_micro
  | E_raw of int
  | E_sleep of int
  | E_fail_now
  | E_fail_raw of int
  | E_held

(* A re-entrant action: a click, a raising event, a stop, or a raising event and
   then a raise. *)
type reenter = R_none | R_click | R_raise | R_stop | R_raise_then_raise

type etask =
  {ehow: ehow; ekey: string option; escope: string option; emap: reenter}

type eaction =
  | E_click of etask list
  | E_raise_click
  | E_raise_got
  | E_raise_view
  | E_release
  | E_advance of int
  | E_hidden of bool
  | E_stop
  | E_in_update of reenter
  | E_in_view of reenter

type emsg = E_clicked | E_got of int

let pp_reenter = function
  | R_none ->
      "none"
  | R_click ->
      "click"
  | R_raise ->
      "raise"
  | R_stop ->
      "stop"
  | R_raise_then_raise ->
      "raise, raise"

let pp_ehow = function
  | E_now ->
      "now"
  | E_micro ->
      "micro"
  | E_raw d ->
      "raw " ^ string_of_int d
  | E_sleep d ->
      "sleep " ^ string_of_int d
  | E_fail_now ->
      "fail now"
  | E_fail_raw d ->
      "fail raw " ^ string_of_int d
  | E_held ->
      "held"

(* The source of the task, its key, its scope, and the re-entrant action of its
   message function. *)
let pp_etask t =
  pp_ehow t.ehow
  ^ (match t.ekey with Some k -> " @" ^ k | None -> "")
  ^ (match t.escope with Some s -> " in " ^ s | None -> "")
  ^ match t.emap with R_none -> "" | r -> " map " ^ pp_reenter r

let pp_eaction = function
  | E_click l ->
      "click [" ^ String.concat "; " (List.map pp_etask l) ^ "]"
  | E_raise_click ->
      "raise click"
  | E_raise_got ->
      "raise got"
  | E_raise_view ->
      "raise view"
  | E_release ->
      "release"
  | E_advance n ->
      "advance " ^ string_of_int n
  | E_hidden b ->
      "hidden " ^ string_of_bool b
  | E_stop ->
      "stop"
  | E_in_update r ->
      "in update " ^ pp_reenter r
  | E_in_view r ->
      "in view " ^ pp_reenter r

let gen_reenter =
  Gen.frequency
    [ (8, Gen.constant R_none)
    ; (1, Gen.constant R_click)
    ; (1, Gen.constant R_raise)
    ; (1, Gen.constant R_stop)
    ; (1, Gen.constant R_raise_then_raise) ]

let gen_etask =
  Gen.map
    (fun ((ehow, ekey, escope), emap) -> {ehow; ekey; escope; emap})
    (Gen.pair
       (Gen.triple
          (Gen.frequency
             [ (3, Gen.constant E_now)
             ; (2, Gen.constant E_micro)
             ; (2, Gen.map (fun d -> E_raw d) (Gen.int_range 0 30))
             ; (2, Gen.map (fun d -> E_sleep d) (Gen.int_range 0 30))
             ; (1, Gen.constant E_fail_now)
             ; (1, Gen.map (fun d -> E_fail_raw d) (Gen.int_range 0 20))
             ; (2, Gen.constant E_held) ] )
          (Gen.of_list [None; Some "a"; Some "b"])
          (Gen.of_list [None; Some "s"]) )
       gen_reenter )

let gen_eaction =
  Gen.frequency
    [ ( 6
      , Gen.map
          (fun l -> E_click l)
          (Gen.list ~size:(Gen.int_range 0 3) gen_etask) )
    ; (1, Gen.constant E_raise_click)
    ; (1, Gen.constant E_raise_got)
    ; (1, Gen.constant E_raise_view)
    ; (2, Gen.constant E_release)
    ; (2, Gen.map (fun n -> E_advance n) (Gen.int_range 1 40))
    ; (1, Gen.map (fun b -> E_hidden b) Gen.bool)
    ; (1, Gen.constant E_stop)
    ; (1, Gen.map (fun r -> E_in_update r) gen_reenter)
    ; (1, Gen.map (fun r -> E_in_view r) gen_reenter) ]

let egen =
  Gen.with_pp
    (fun ppf l ->
      Format.pp_print_string ppf (String.concat ", " (List.map pp_eaction l)) )
    (Gen.list ~size:(Gen.int_range 1 25) gen_eaction)

let error_law actions =
  let h = H.create () in
  let page = ref None in
  let plans = Queue.create () and held = Queue.create () in
  let next = ref 0 and processed = ref [] and newest = ref [] in
  let keyless = ref [] and violations = ref [] in
  let note s = violations := s :: !violations in
  let raise_click = ref false
  and raise_got = ref false
  and raise_view = ref false
  and stopped = ref false in
  let in_update = ref R_none and in_view = ref R_none in
  let live () = match !page with Some p -> (ins p).alive | None -> true in
  (* A re-entrant action inside a turn. The message function of the input "in"
     raises inside its event: a root catch. *)
  let reenter r =
    match !page with
    | None ->
        ()
    | Some p -> (
        let query f = try f () with P.Query _ -> () in
        match r with
        | R_none ->
            ()
        | R_click ->
            query (fun () -> P.click p (By.test_id "b"))
        | R_raise ->
            query (fun () -> P.click p (By.test_id "in"))
        | R_stop ->
            P.stop p ;
            stopped := true
        | R_raise_then_raise ->
            query (fun () -> P.click p (By.test_id "in")) ;
            raise Boom )
  in
  let task t =
    incr next ;
    let id = !next in
    let k = Option.map (fun k -> (t.escope, k)) t.ekey in
    ( match k with
    | Some k ->
        newest := (k, id) :: List.remove_assoc k !newest
    | None ->
        keyless := id :: !keyless ) ;
    let v = E_got id in
    let a =
      match t.ehow with
      | E_now ->
          sync v
      | E_micro ->
          Async.make (fun _ ~ok ~error:_ -> H.raw_microtask h (fun () -> ok v))
      | E_raw d ->
          Async.make (fun _ ~ok ~error:_ -> H.raw h ~after:d (fun () -> ok v))
      | E_sleep d ->
          Async.map (fun () -> v) (Async.sleep (Span.ms d))
      | E_fail_now ->
          Async.fail Boom
      | E_fail_raw d ->
          Async.make (fun _ ~ok:_ ~error ->
              H.raw h ~after:d (fun () -> error Boom) )
      | E_held ->
          Async.make (fun _ ~ok ~error:_ -> Queue.push (fun () -> ok v) held)
    in
    let c =
      match t.ekey with Some key -> Cmd.latest ~key a | None -> Cmd.task a
    in
    Cmd.map ?scope:t.escope (fun x -> reenter t.emap ; x) c
  in
  let update m msg =
    if not (live ()) then note "an update after a root error" ;
    if !stopped then note "an update after a stop" ;
    let r = !in_update in
    in_update := R_none ;
    reenter r ;
    match msg with
    | E_clicked ->
        if !raise_click then begin
          raise_click := false ;
          raise Boom
        end ;
        (* A re-entrant click has no plan. *)
        let l = Option.value ~default:[] (Queue.take_opt plans) in
        (m + 1, Cmd.batch (List.map task l))
    | E_got id ->
        if List.mem id !processed then note "a message two times" ;
        processed := id :: !processed ;
        if !raise_got then begin
          raise_got := false ;
          raise Boom
        end ;
        (m + 1, Cmd.none)
  in
  let view m =
    if !raise_view then begin
      raise_view := false ;
      raise Boom
    end ;
    let r = !in_view in
    in_view := R_none ;
    reenter r ;
    El.div
      [ button E_clicked (string_of_int m)
      ; El.input
          ~at:[At.data "testid" "in"]
          ~ev:[Ev.check (fun _ -> invalid_arg "a raise")]
          () ]
  in
  let fallback _ = button E_clicked "fallback" in
  let p =
    P.start ~host:h
      (App.make ~init:(fun () -> (0, Cmd.none)) ~update ~view ~fallback ())
  in
  page := Some p ;
  List.iter
    (fun a ->
      ( match a with
      | E_click l ->
          if not !stopped then begin
            Queue.push l plans ;
            click p ;
            (* A dead root drops the click: its plan stays. *)
            if not (live ()) then Queue.clear plans
          end
      | E_raise_click ->
          raise_click := true
      | E_raise_got ->
          raise_got := true
      | E_raise_view ->
          raise_view := true
      | E_release ->
          H.raw h (fun () ->
              match Queue.take_opt held with Some f -> f () | None -> () ) ;
          P.run p
      | E_advance n ->
          P.advance p (Span.ms n)
      | E_hidden b ->
          P.set_hidden p b
      | E_stop ->
          P.stop p ;
          stopped := true
      | E_in_update r ->
          in_update := r
      | E_in_view r ->
          in_view := r ) ;
      settled p )
    actions ;
  raise_view := false ;
  in_update := R_none ;
  in_view := R_none ;
  H.raw h (fun () -> Queue.iter (fun f -> f ()) held) ;
  P.set_hidden p false ;
  quiet p ;
  settled p ;
  equal (list string) [] !violations ;
  if !stopped then
    equal string ~msg:"no DOM after a stop" "<div></div>" (P.html p) ;
  let i = ins p in
  equal int 0 i.messages_left ;
  equal int 0 i.continuations_left ;
  equal int 0 i.requests_left ;
  is_false i.turn_requested ;
  is_false i.continuing ;
  is_false i.tick_pending ;
  equal int 0 i.latest_keys ;
  equal int 0 i.token_children ;
  equal int 0 (H.live_timers h) ;
  if i.alive then begin
    no_errors p ;
    (* Each result of the newest task of a key, and each result with no key,
       reached update. *)
    List.iter
      (fun (_, id) -> is_true ~msg:"the newest result" (List.mem id !processed))
      !newest ;
    List.iter
      (fun id -> is_true ~msg:"a result with no key" (List.mem id !processed))
      !keyless
  end

let tests =
  [ group "turn laws"
      [ prop ~count:200 "the turn on random schedules" gen law
      ; prop ~count:300 "the turn with errors and a stop" egen error_law ] ]
