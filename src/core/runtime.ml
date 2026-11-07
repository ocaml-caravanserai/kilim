type env = {host: Host.t; hidden: unit -> bool}

type kind = Discrete | Frame | Macro | Micro

type stats =
  { mutable turns: int
  ; mutable messages: int
  ; mutable dropped: int
  ; mutable views: int
  ; mutable renders: int
  ; mutable max_renders: int
  ; mutable requests: int }

type inspect =
  { messages_left: int
  ; continuations_left: int
  ; requests_left: int
  ; turn_requested: bool
  ; pending: bool
  ; scheduled: bool
  ; in_turn: bool
  ; latest_keys: int
  ; token_children: int
  ; alive: bool }

(* A message maps when it leaves the queue. *)
type 'msg payload =
  | Msg of 'msg
  | Result : 'a * ('a -> 'msg) -> 'msg payload
  | Error of exn

type item =
  | Item :
      { inst: 'msg Instance.t
      ; token: Cancel.t option  (** The task token, [None] for the DOM. *)
      ; key: string list option  (** The key of [Cmd.latest]. *)
      ; payload: 'msg payload }
      -> item

type request =
  | Req :
      { inst: 'msg Instance.t
      ; token: Cancel.t
      ; key: string list option
      ; task: 'a Async.t
      ; to_msg: 'a -> 'msg }
      -> request

type 'node root =
  | Root :
      { inst: 'msg Instance.t
      ; fallback: (exn -> 'msg View.t) option
      ; mutable tree: ('node, 'msg) Diff.t option
      ; mutable failed: exn option
      ; mutable shown: bool  (** The fallback is in the DOM. *) }
      -> 'node root

type 'node t =
  { env: env
  ; dom: 'node Dom.t
  ; parent: 'node
  ; mutable wrapped: Host.t  (** The host of the tasks. *)
  ; run: Run_queue.t
  ; messages: item Queue.t
  ; requests: request Queue.t
  ; root: 'node root
  ; stats: stats
  ; mutable in_turn: bool
  ; mutable turn_requested: bool
  ; mutable pending: bool
  ; mutable scheduled: bool
  ; mutable gen: int  (** The render count: an older callback does nothing. *)
  ; mutable timer: unit -> unit  (** Clears the 100 ms render timer. *)
  ; mutable renders_in_turn: int
  ; mutable stopped: bool }

let max_renders = 3

let renders_now = function Discrete | Frame -> true | Macro | Micro -> false

let has_work rt =
  not
    ( Queue.is_empty rt.messages && Run_queue.is_empty rt.run
    && Queue.is_empty rt.requests )

(* A dead instance never becomes dirty. After a stop, no render: a stop can come
   between a root catch and the write of its fallback. *)
let wants rt =
  let (Root r) = rt.root in
  (not rt.stopped)
  && (rt.pending || r.inst.dirty || (Option.is_some r.failed && not r.shown))

let clear_timer rt =
  let clear = rt.timer in
  rt.timer <- ignore ;
  clear ()

(* The root catch (design section 09): the error unmounts the App, and the next
   render shows the fallback. An error after the unmount only goes to the error
   handler. *)
let catch rt e =
  Report.error e ;
  let (Root r) = rt.root in
  if r.inst.alive then begin
    Instance.unmount r.inst ;
    r.failed <- Some e ;
    r.shown <- false
  end

let rec push rt item =
  if not rt.stopped then begin
    Queue.push item rt.messages ;
    if not rt.in_turn then request_turn rt
  end

(* The host first, then the flag: a fault of the host leaves no flag set with no
   callback. *)
and request_turn rt =
  if not rt.turn_requested then
    match
      rt.env.host.queue_microtask (fun () ->
          rt.turn_requested <- false ;
          turn rt Micro ignore )
    with
    | () ->
        rt.turn_requested <- true ;
        rt.stats.requests <- rt.stats.requests + 1
    | exception e ->
        Report.error e

and turn rt kind f =
  if rt.stopped then ()
  else if rt.in_turn then f ()
  else begin
    rt.in_turn <- true ;
    match
      Run_queue.with_turn rt.run (fun () -> body rt kind f) ;
      finish rt
    with
    | () ->
        ()
    | exception e ->
        recover rt kind e
  end

and body rt kind f =
  rt.stats.turns <- rt.stats.turns + 1 ;
  rt.renders_in_turn <- 0 ;
  f () ;
  let again = ref true in
  while !again do
    settle rt ;
    let want = wants rt in
    if renders_now kind && rt.renders_in_turn < max_renders && want then
      render rt
    else begin
      if want then rt.pending <- true ;
      again := false
    end
  done

(* A loop, not a recursion: js_of_ocaml has no general tail calls. *)
and settle rt =
  while has_work rt do
    if not (Run_queue.is_empty rt.run) then Run_queue.drain rt.run
    else if not (Queue.is_empty rt.messages) then
      process rt (Queue.pop rt.messages)
    else start_requests rt
  done

and process rt (Item {inst; token; key; payload}) =
  ( match (key, token) with
  | Some k, Some t -> (
    match Instance.Latest.find_opt k inst.latest with
    | Some t' when t' == t ->
        inst.latest <- Instance.Latest.remove k inst.latest
    | _ ->
        () )
  | _ ->
      () ) ;
  let stale =
    (not inst.alive)
    || match token with Some t -> Cancel.cancelled t | None -> false
  in
  match inst.state with
  | Some st when not stale -> (
    match payload with
    | Msg m ->
        count rt ; update rt inst st m
    | Result (x, f) -> (
      match f x with
      | m ->
          (* The function of [Cmd.map] can stop the page or make a root catch (a
             re-entrant event): then the message is dropped. *)
          if inst.alive then begin
            count rt ; update rt inst st m
          end
          else rt.stats.dropped <- rt.stats.dropped + 1
      | exception e ->
          catch rt e )
    | Error e ->
        catch rt e )
  | _ ->
      rt.stats.dropped <- rt.stats.dropped + 1

and count rt = rt.stats.messages <- rt.stats.messages + 1

(* A call of [update]. [Cmd.none] makes no request. *)
and update : type msg. _ -> msg Instance.t -> msg Instance.state -> msg -> unit
    =
 fun rt inst (State s) m ->
  match s.spec.update s.model m with
  | model, cmd -> (
      (* A root catch or a stop during [update] (a re-entrant event) unmounts
         the instance: then the model does not change. *)
      if model != s.model && inst.alive then begin
        s.model <- model ;
        inst.dirty <- true
      end ;
      match cmd with
      | (Cmd.None [@mutate off "a shortcut: the requests of Cmd.None are []"])
        ->
          ()
      | _ ->
          command rt inst cmd )
  | exception e ->
      catch rt e

(* Each request gets its token at once, under the token of the instance (made at
   the first task). A key of [Cmd.latest] cancels the old token of the key, also
   if its task has not started. An instance that a root catch or a stop
   unmounted in its [update] gets no request. *)
and command : type msg. _ -> msg Instance.t -> msg Cmd.t -> unit =
 fun rt inst cmd ->
  List.iter
    (fun (Cmd.Request {key; task; to_msg}) ->
      (* The cancel of the old token runs its end actions: they can stop the
         page or make a root catch, also in the middle of a batch. *)
      Option.iter
        (fun k ->
          Option.iter Cancel.cancel (Instance.Latest.find_opt k inst.latest) )
        key ;
      if inst.alive then begin
        let token = Cancel.child (Instance.token inst) in
        Option.iter
          (fun k -> inst.latest <- Instance.Latest.add k token inst.latest)
          key ;
        Queue.push (Req {inst; token; key; task; to_msg}) rt.requests
      end )
    (Cmd.requests cmd)

(* All requests, in the order of their creation. A request whose token is
   cancelled does not start. The end of a task releases its token. *)
and start_requests rt =
  let reqs = Queue.create () in
  Queue.transfer rt.requests reqs ;
  Queue.iter
    (fun (Req {inst; token; key; task; to_msg}) ->
      if not (Cancel.cancelled token) then
        Async.start ~host:rt.wrapped ~run:rt.run token task
          ~ok:(fun x ->
            Cancel.release token ;
            push rt
              (Item {inst; token= Some token; key; payload= Result (x, to_msg)}) )
          ~error:(fun e ->
            Cancel.release token ;
            push rt (Item {inst; token= Some token; key; payload= Error e}) ) )
    reqs

and render rt =
  rt.pending <- false ;
  rt.scheduled <- false ;
  rt.gen <- rt.gen + 1 ;
  clear_timer rt ;
  rt.renders_in_turn <- rt.renders_in_turn + 1 ;
  rt.stats.max_renders <- max rt.stats.max_renders rt.renders_in_turn ;
  let (Root r) = rt.root in
  let wrote =
    match (r.failed, r.inst.state) with
    | Some e, _ ->
        if r.shown then false else (write_fallback rt e ; true)
    | None, Some (State s) when r.inst.dirty ->
        r.inst.dirty <- false ;
        rt.stats.views <- rt.stats.views + 1 ;
        (* The view runs before the diff writes: an error in the view leaves no
           half-changed DOM. A stop or a root catch in the view (a re-entrant
           event) unmounts the root: then no write. *)
        ( match s.spec.view s.model with
        | v -> (
            if r.inst.alive then
              try r.tree <- write rt r.inst r.tree v
              with e -> catch_in_render rt e )
        | exception e ->
            catch_in_render rt e ) ;
        true
    | None, _ ->
        false
  in
  if wrote then rt.stats.renders <- rt.stats.renders + 1

and dispatch : type msg. _ -> msg Instance.t -> msg -> unit =
 fun rt inst m -> push rt (Item {inst; token= None; key= None; payload= Msg m})

(* The patch of the retained tree, or the mount at the first render. It can
   raise: each caller has its own error rule. *)
and write : type msg.
       _
    -> msg Instance.t
    -> (_, msg) Diff.t option
    -> msg View.t
    -> (_, msg) Diff.t option =
 fun rt inst tree v ->
  match tree with
  | Some t ->
      Diff.patch t v ; tree
  | None ->
      Some (Diff.mount rt.dom ~parent:rt.parent ~dispatch:(dispatch rt inst) v)

(* The view and the diff run only for a live root, thus the catch marks the root
   as failed. *)
and catch_in_render rt e =
  catch rt e ;
  (* A root catch can come first, in a re-entrant event of the view: the
     fallback gets the first error. *)
  let (Root r) = rt.root in
  write_fallback rt (Option.value r.failed ~default:e)

(* The fallback goes in the place of the view. Its handlers dispatch to the dead
   root, thus their messages are dropped. *)
and write_fallback rt e =
  let (Root r) = rt.root in
  let v =
    match r.fallback with
    | Some f -> (
      try f e with e' -> Report.error e' ; View.txt "" )
    | None ->
        View.txt ""
  in
  r.shown <- true ;
  (* The fallback function can stop the page: then no write. *)
  if not rt.stopped then
    try r.tree <- write rt r.inst r.tree v
    with e' -> (
      (* An error in the write of the fallback: an empty text. If that write
         raises too (a DOM fault), the root removes its DOM. A fault of the
         remove goes to the catch of the turn. *)
      Report.error e' ;
      try r.tree <- write rt r.inst r.tree (View.txt "")
      with e'' -> (
        Report.error e'' ;
        match r.tree with
        | Some tree ->
            r.tree <- None ;
            rt.dom.remove rt.parent (Diff.dom (Diff.root tree))
        | None ->
            () ) )

(* A frame and a 100 ms timer, or a task in a hidden page. The first callback
   renders. The render changes [gen], thus the other callback does nothing. *)
and schedule rt =
  let g = rt.gen in
  let cb () =
    if g = rt.gen then begin
      rt.scheduled <- false ;
      clear_timer rt ;
      turn rt Frame ignore
    end
  in
  if rt.env.hidden () then rt.env.host.post_task cb
  else begin
    rt.env.host.request_frame cb ;
    rt.timer <- rt.env.host.set_timer ~ms:100 cb
  end ;
  rt.scheduled <- true

and finish rt =
  if rt.pending && not rt.scheduled then schedule rt ;
  rt.in_turn <- false

(* The catch of the turn, for an error outside each scope. *)
and recover rt kind e =
  Report.error e ;
  rt.in_turn <- false ;
  if has_work rt then request_turn rt ;
  if wants rt then begin
    rt.pending <- true ;
    match kind with
    | Discrete ->
        (* The recovery turn is a frame turn: its own catch does not run a turn
           again. *)
        turn rt Frame ignore
    | Frame | Macro | Micro ->
        ()
  end ;
  if rt.pending && not rt.scheduled then
    try schedule rt with e -> Report.error e

(* Each callback of the host of the tasks runs in a turn. *)
let wrap rt =
  let h = rt.env.host in
  { Host.now_ms= h.now_ms
  ; set_timer= (fun ~ms f -> h.set_timer ~ms (fun () -> turn rt Macro f))
  ; request_frame= (fun f -> h.request_frame (fun () -> turn rt Frame f))
  ; queue_microtask= (fun f -> h.queue_microtask (fun () -> turn rt Micro f))
  ; post_task= (fun f -> h.post_task (fun () -> turn rt Macro f)) }

let make env dom parent root =
  let self = ref None in
  let run =
    Run_queue.create ~schedule:(fun () -> Option.iter request_turn !self)
  in
  let rt =
    { env
    ; dom
    ; parent
    ; wrapped= env.host
    ; run
    ; messages= Queue.create ()
    ; requests= Queue.create ()
    ; root
    ; stats=
        { turns= 0
        ; messages= 0
        ; dropped= 0
        ; views= 0
        ; renders= 0
        ; max_renders= 0
        ; requests= 0 }
    ; in_turn= false
    ; turn_requested= false
    ; pending= false
    ; scheduled= false
    ; gen= 0
    ; timer= ignore
    ; renders_in_turn= 0
    ; stopped= false }
  in
  self := Some rt ;
  rt.wrapped <- wrap rt ;
  rt

let start env dom ~parent (App.App {init; update; view; fallback}) =
  let inst = Instance.make (Cancel.root ()) in
  let rt =
    make env dom parent
      (Root {inst; fallback; tree= None; failed= None; shown= false})
  in
  turn rt Discrete (fun () ->
      match init () with
      | model, cmd ->
          (* init cannot reach the runtime, thus the instance is alive here. *)
          inst.state <- Some (State {spec= {update; view}; model}) ;
          inst.dirty <- true ;
          command rt inst cmd
      | exception e ->
          catch rt e ) ;
  rt

let event rt kind f = turn rt kind (fun () -> try f () with e -> catch rt e)

let visibility_changed rt =
  if rt.env.hidden () && rt.pending then begin
    rt.scheduled <- false ;
    clear_timer rt ;
    rt.gen <- rt.gen + 1 ;
    turn rt Frame ignore
  end

(* The unmount cancels the token of the root, and thus all tasks. Later
   callbacks do nothing, because a turn after the stop does nothing. *)
let stop rt =
  if not rt.stopped then begin
    rt.stopped <- true ;
    let (Root r) = rt.root in
    Instance.unmount r.inst ;
    ( match r.tree with
    | Some tree -> (
        r.tree <- None ;
        try rt.dom.remove rt.parent (Diff.dom (Diff.root tree))
        with e -> Report.error e )
    | None ->
        () ) ;
    Queue.clear rt.messages ;
    Queue.clear rt.requests ;
    Run_queue.clear rt.run ;
    clear_timer rt ;
    rt.pending <- false ;
    rt.scheduled <- false
  end

let stats rt = {rt.stats with turns= rt.stats.turns}

let inspect rt =
  let (Root r) = rt.root in
  { messages_left= Queue.length rt.messages
  ; continuations_left= Run_queue.length rt.run
  ; requests_left= Queue.length rt.requests
  ; turn_requested= rt.turn_requested
  ; pending= rt.pending
  ; scheduled= rt.scheduled
  ; in_turn= rt.in_turn
  ; latest_keys= Instance.Latest.cardinal r.inst.latest
  ; token_children=
      (match r.inst.token with Some t -> Cancel.children t | None -> 0)
  ; alive= r.inst.alive }
