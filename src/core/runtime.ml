type cost = Update | Drain

type env =
  { host: Host.t
  ; hidden: unit -> bool
  ; cost: cost -> unit
  ; same: 'a. 'a -> 'a -> bool }

let limit_ms = 50.

let wall_ms = 1000.

type kind = Discrete | Frame | Macro | Micro

type stats =
  { mutable turns: int
  ; mutable clocks: int
  ; mutable messages: int
  ; mutable dropped: int
  ; mutable views: int
  ; mutable renders: int
  ; mutable max_renders: int
  ; mutable stops: int
  ; mutable yields: int
  ; mutable warnings: int
  ; mutable requests: int
  ; mutable restores: int }

type inspect =
  { messages_left: int
  ; continuations_left: int
  ; requests_left: int
  ; turn_requested: bool
  ; continuing: bool
  ; pending: bool
  ; scheduled: bool
  ; tick_pending: bool
  ; in_turn: bool
  ; latest_keys: int
  ; sub_keys: int
  ; restores_left: int
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
  ; restores: (unit -> unit) Queue.t
        (** The restores of the discrete events, for the next render. *)
  ; root: 'node root
  ; stats: stats
  ; mutable in_turn: bool
  ; mutable turn_requested: bool
  ; mutable continuing: bool  (** A continuation is posted. *)
  ; mutable pending: bool
  ; mutable scheduled: bool
  ; mutable gen: int  (** The render count: an older callback does nothing. *)
  ; mutable timer: unit -> unit  (** Clears the 100 ms render timer. *)
  ; mutable tick_pending: bool
  ; mutable busy: float  (** The busy sum of the turns since the tick. *)
  ; mutable turn_start: float
  ; mutable window_start: float  (** The start of the wall-time window. *)
  ; mutable clocked: bool  (** The first clock read of the turn worked. *)
  ; mutable accounted: bool  (** The turn added its time to [busy]. *)
  ; mutable last_now: float  (** The last clock read of [over]. *)
  ; stop_times: float Queue.t  (** The last 10 stops at the limit. *)
  ; mutable warned: bool
  ; mutable renders_in_turn: int
  ; mutable stopped: bool
  ; mutable first: (unit -> unit) option
        (** The function of the first turn, until it runs. *)
  ; mutable abort: unit -> unit
        (** Ends a turn that a JS exception left (see [active]). *) }

type settled = Done | Yield

(* The aborts of the runtimes in a turn, the newest first. Under wasm_of_ocaml,
   a trap (for example an illegal cast) is a JS exception that no OCaml catch
   sees: it leaves [in_turn] set, and the app stops. The browser then calls
   [abort], which ends these turns. *)
let active : (unit -> unit) list ref = ref []

let max_renders = 3

let renders_now = function Discrete | Frame -> true | Macro | Micro -> false

let has_work rt =
  not
    ( Queue.is_empty rt.messages && Run_queue.is_empty rt.run
    && Queue.is_empty rt.requests )

(* A dead instance never becomes dirty. After a stop, no render: a stop can come
   between a root catch and the write of its fallback. A restore that waits
   needs a render, also with no model change: for example a discrete event
   inside a Macro turn. *)
let wants rt =
  let (Root r) = rt.root in
  (not rt.stopped)
  && ( rt.pending || r.inst.dirty
     || (Option.is_some r.failed && not r.shown)
     || not (Queue.is_empty rt.restores) )

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
    active := rt.abort :: !active ;
    ( match
        Run_queue.with_turn rt.run (fun () -> body rt kind f) ;
        finish rt
      with
    | () ->
        ()
    | exception e ->
        recover rt kind e ) ;
    (* The entry of this runtime, not the head: a dead nested turn of another
       runtime can be above it. After an abort, the entry is gone. *)
    active := List.filter (fun f -> f != rt.abort) !active
  end

and body rt kind f =
  rt.clocked <- false ;
  rt.accounted <- false ;
  let now = rt.env.host.now_ms () in
  (* A pending tick means that no browser task ran since the last turn. *)
  if kind <> Micro || not rt.tick_pending then begin
    rt.busy <- 0. ;
    rt.window_start <- now ;
    rt.stats.clocks <- rt.stats.clocks + 1
  end ;
  rt.turn_start <- now ;
  rt.clocked <- true ;
  rt.stats.turns <- rt.stats.turns + 1 ;
  rt.renders_in_turn <- 0 ;
  f () ;
  let again = ref true in
  while !again do
    match settle rt with
    | Yield ->
        stop_at_limit rt ;
        (* The continuation settles the work of the end actions. *)
        early rt ;
        again := false
    | Done ->
        let want = wants rt in
        if renders_now kind && rt.renders_in_turn < max_renders && want then
          render rt
        else begin
          if want then rt.pending <- true ;
          (* The end actions of the early call can push work: the loop settles
             it in this turn. *)
          early rt ;
          again := has_work rt
        end
  done

(* The busy sum of the turns since the tick, and the wall time since the new
   clock. The check uses a strict [>], thus a turn with no work never stops. *)
and over rt =
  let now = rt.env.host.now_ms () in
  rt.last_now <- now ;
  rt.busy +. (now -. rt.turn_start) > limit_ms
  || now -. rt.window_start > wall_ms

(* A loop, not a recursion: js_of_ocaml has no general tail calls. *)
and settle rt =
  let r = ref None in
  while Option.is_none !r do
    if not (has_work rt) then r := Some Done
    else if over rt then r := Some Yield
    else if not (Run_queue.is_empty rt.run) then begin
      Run_queue.drain ~over:(fun () -> rt.env.cost Drain ; over rt) rt.run ;
      if not (Run_queue.is_empty rt.run) then r := Some Yield
    end
    else if not (Queue.is_empty rt.messages) then
      process rt (Queue.pop rt.messages)
    else start_requests rt
  done ;
  Option.get !r

(* The rest waits in the queues, and a continuation in a new task goes on. Each
   stop goes into the warning ring, also a stop while a continuation is
   posted. *)
and stop_at_limit rt =
  let now = rt.last_now in
  rt.pending <- true ;
  rt.stats.stops <- rt.stats.stops + 1 ;
  Queue.push now rt.stop_times ;
  if Queue.length rt.stop_times > 10 then ignore (Queue.pop rt.stop_times) ;
  if Queue.length rt.stop_times = 10 && now -. Queue.peek rt.stop_times <= 1000.
  then
    begin if not rt.warned then begin
      Report.warn
        "kilim: 10 stops at the limit in 1 s. A loop of messages or tasks \
         blocks the page." ;
      rt.stats.warnings <- rt.stats.warnings + 1 ;
      rt.warned <- true
    end
    end
  else rt.warned <- false ;
  if not rt.continuing then begin
    rt.env.host.post_task (fun () ->
        rt.continuing <- false ;
        turn rt Macro ignore ) ;
    rt.continuing <- true ;
    rt.stats.yields <- rt.stats.yields + 1
  end

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

and count rt =
  rt.env.cost Update ;
  rt.stats.messages <- rt.stats.messages + 1

(* A call of [update]. [Cmd.none] makes no request. *)
and update : type msg. _ -> msg Instance.t -> msg Instance.state -> msg -> unit
    =
 fun rt inst (State s) m ->
  match s.spec.update s.props s.model m with
  | model, cmd -> (
      (* A root catch or a stop during [update] (a re-entrant event) unmounts
         the instance: then the model does not change. *)
      if (not (rt.env.same model s.model)) && inst.alive then begin
        s.model <- model ;
        Instance.changed inst
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
        ( match s.spec.view s.props s.model with
        | v -> (
            if r.inst.alive then
              try r.tree <- write rt r.inst r.tree v
              with e -> catch_in_render rt e )
        | exception e ->
            catch_in_render rt e ) ;
        (* The subscriptions of the new model. An error in [subs] writes the
           fallback in this render. A root that died in the view has no state:
           then the pass does nothing. *)
        (try subs_pass rt r.inst ~start:true with e -> catch_in_render rt e) ;
        true
    | None, _ ->
        false
  in
  if wrote then rt.stats.renders <- rt.stats.renders + 1 ;
  restore rt

(* The restores run after the write, thus they compare with the newest view. A
   restore that a restore pushes waits for the next render. A stop clears the
   list. *)
and restore rt =
  let todo = Queue.create () in
  Queue.transfer rt.restores todo ;
  Queue.iter
    (fun f ->
      rt.stats.restores <- rt.stats.restores + 1 ;
      try f () with e -> Report.error e )
    todo

(* The early call: a turn that leaves a render pending stops the removed keys
   before the render. It runs only if the model changed since the last pass. A
   dead instance has no state, thus the pass does nothing for it. *)
and early rt =
  let (Root r) = rt.root in
  if r.inst.early_stale then
    try subs_pass rt r.inst ~start:false with e -> catch rt e

(* The subscription pass of a live instance. [subs] and the end actions of the
   removed keys are user code: they can stop the page or make a root catch, thus
   each leaf examines [alive] again. The new keys start only if [start]. *)
and subs_pass : type msg. _ -> msg Instance.t -> start:bool -> unit =
 fun rt inst ~start ->
  match inst.state with
  | Some (State {spec= {subs= Some f; _}; props; model}) ->
      let leaves = Sub.leaves (f props model) in
      let wanted =
        List.fold_left
          (fun m -> function
            | Sub.Run_leaf (k, _) ->
                Instance.Subs.add (Sub.Key.Run k) () m
            | Sub.Auto_leaf (k, _, _, _) ->
                Instance.Subs.add (Sub.Key.Auto k) () m )
          Instance.Subs.empty leaves
      in
      (* The key leaves the set before its cancel: an end action cannot find it
         again. *)
      Instance.Subs.iter
        (fun k token ->
          if not (Instance.Subs.mem k wanted) then begin
            inst.runs <- Instance.Subs.remove k inst.runs ;
            Cancel.cancel token
          end )
        inst.runs ;
      Instance.Subs.iter
        (fun k (Instance.Auto {token; _}) ->
          if not (Instance.Subs.mem k wanted) then begin
            inst.autos <- Instance.Subs.remove k inst.autos ;
            Cancel.cancel token
          end )
        inst.autos ;
      List.iter (fun leaf -> if inst.alive then keep rt inst ~start leaf) leaves ;
      inst.early_stale <- false
  | _ ->
      ()

(* A key that stays gets the newest message function. A new key starts if
   [start]. *)
and keep : type msg. _ -> msg Instance.t -> start:bool -> msg Sub.leaf -> unit =
 fun rt inst ~start leaf ->
  match leaf with
  | Sub.Run_leaf (k, f) ->
      let k = Sub.Key.Run k in
      if start && not (Instance.Subs.mem k inst.runs) then start_run rt inst k f
  | Sub.Auto_leaf (k, kind, task, on) -> (
      let k = Sub.Key.Auto k in
      match Instance.Subs.find_opt k inst.autos with
      | Some (Instance.Auto x) -> (
        match Type.Id.provably_equal x.kind kind with
        | Some Type.Equal ->
            x.on := on
        | None ->
            (* This does not occur: the key has the uid of the witness, thus one
               key has one witness. *)
            () )
      | None ->
          if start then start_auto rt inst k kind task on )

(* The key goes into the set first, then the task starts. A [send] after the
   cancel of the key pushes nothing. *)
and start_run : type msg.
    _ -> msg Instance.t -> Sub.Key.t -> ((msg -> unit) -> unit Async.t) -> unit
    =
 fun rt inst k f ->
  let token = Cancel.child (Instance.token inst) in
  inst.runs <- Instance.Subs.add k token inst.runs ;
  let send m =
    if not (Cancel.cancelled token) then
      push rt (Item {inst; token= Some token; key= None; payload= Msg m})
  in
  launch rt inst token (f send)

(* A payload takes the newest message function when it leaves the queue. *)
and start_auto : type msg p.
       _
    -> msg Instance.t
    -> Sub.Key.t
    -> p Sub.kind
    -> ((p -> unit) -> unit Async.t)
    -> (p -> msg)
    -> unit =
 fun rt inst k kind task on ->
  let token = Cancel.child (Instance.token inst) in
  let on = ref on in
  inst.autos <- Instance.Subs.add k (Instance.Auto {token; kind; on}) inst.autos ;
  let map p = !on p in
  let deliver p =
    if not (Cancel.cancelled token) then
      push rt
        (Item {inst; token= Some token; key= None; payload= Result (p, map)})
  in
  launch rt inst token (task deliver)

(* The function of [Sub.run] is user code: it can stop the page or make a root
   catch, and then the token is cancelled. A task under a cancelled token does
   not start, because [Async.return] gives its result also then. The end of a
   task releases its token, and its key stays. An error unmounts the instance
   when it leaves the queue, and the unmount cancels the token. *)
and launch : type msg. _ -> msg Instance.t -> Cancel.t -> unit Async.t -> unit =
 fun rt inst token task ->
  if not (Cancel.cancelled token) then
    Async.start ~host:rt.wrapped ~run:rt.run token task
      ~ok:(fun () -> Cancel.release token)
      ~error:(fun e ->
        push rt (Item {inst; token= Some token; key= None; payload= Error e}) )

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

and account rt =
  let now = rt.env.host.now_ms () in
  rt.busy <- rt.busy +. (now -. rt.turn_start) ;
  rt.accounted <- true

(* The end of each turn posts one tick. The tick is not a turn: it sets the busy
   sum to 0 and posts nothing. *)
and tick rt =
  if not rt.tick_pending then begin
    rt.env.host.post_task (fun () ->
        rt.tick_pending <- false ;
        rt.busy <- 0. ) ;
    rt.tick_pending <- true
  end

and finish rt =
  if rt.pending && not rt.scheduled then schedule rt ;
  account rt ;
  tick rt ;
  rt.in_turn <- false

(* The catch of the turn, for an error outside each scope. It adds the time of
   the turn only one time, and only if the first clock read of the turn
   worked. *)
and recover rt kind e = Report.error e ; resume rt kind

(* The rest of the catch of the turn, after the report of its error. *)
and resume rt kind =
  rt.in_turn <- false ;
  ( try if rt.clocked && not rt.accounted then account rt
    with e -> Report.error e ) ;
  (try tick rt with e -> Report.error e) ;
  if (not rt.continuing) && has_work rt then request_turn rt ;
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
    ; restores= Queue.create ()
    ; root
    ; stats=
        { turns= 0
        ; clocks= 0
        ; messages= 0
        ; dropped= 0
        ; views= 0
        ; renders= 0
        ; max_renders= 0
        ; stops= 0
        ; yields= 0
        ; warnings= 0
        ; requests= 0
        ; restores= 0 }
    ; in_turn= false
    ; turn_requested= false
    ; continuing= false
    ; pending= false
    ; scheduled= false
    ; gen= 0
    ; timer= ignore
    ; tick_pending= false
    ; busy= 0.
    ; turn_start= 0.
    ; window_start= 0.
    ; clocked= false
    ; accounted= false
    ; last_now= 0.
    ; stop_times= Queue.create ()
    ; warned= false
    ; renders_in_turn= 0
    ; stopped= false
    ; first= None
    ; abort= ignore }
  in
  self := Some rt ;
  rt.wrapped <- wrap rt ;
  (* As the catch of the turn, for a turn of no known kind. *)
  rt.abort <-
    (fun () ->
      Run_queue.reset rt.run
      [@mutate
        off
          "native code always leaves the turn of the run queue; test/dom D31 \
           tests the reset after a trap"] ;
      resume rt Macro ) ;
  rt

let create env dom ~parent (App.App {init; update; view; subs; fallback}) =
  let inst = Instance.make (Cancel.root ()) in
  let rt =
    make env dom parent
      (Root {inst; fallback; tree= None; failed= None; shown= false})
  in
  rt.first <-
    Some
      (fun () ->
        match init () with
        | model, cmd ->
            (* init cannot reach the runtime, thus the instance is alive
               here. *)
            let spec =
              { Instance.update= (fun () m msg -> update m msg)
              ; view= (fun () m -> view m)
              ; subs= Option.map (fun f () m -> f m) subs }
            in
            inst.state <- Some (State {spec; props= (); model}) ;
            Instance.changed inst ;
            command rt inst cmd
        | exception e ->
            catch rt e ) ;
  rt

(* The function of the first turn runs one time. *)
let init rt =
  match rt.first with
  | Some f ->
      rt.first <- None ;
      turn rt Discrete f
  | None ->
      ()

let start env dom ~parent app =
  let rt = create env dom ~parent app in
  init rt ; rt

(* The restore goes into the list before the turn, also for an event inside a
   turn. After a stop, no restore. *)
let event rt kind ?restore f =
  ( match restore with
  | Some g when not rt.stopped ->
      Queue.push g rt.restores
  | Some _ | None ->
      () ) ;
  turn rt kind (fun () -> try f () with e -> catch rt e)

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
    Queue.clear rt.restores ;
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
  ; continuing= rt.continuing
  ; pending= rt.pending
  ; scheduled= rt.scheduled
  ; tick_pending= rt.tick_pending
  ; in_turn= rt.in_turn
  ; latest_keys= Instance.Latest.cardinal r.inst.latest
  ; sub_keys=
      Instance.Subs.cardinal r.inst.runs + Instance.Subs.cardinal r.inst.autos
  ; restores_left= Queue.length rt.restores
  ; token_children=
      (match r.inst.token with Some t -> Cancel.children t | None -> 0)
  ; alive= r.inst.alive }

(* One report for each abort: an abort can end several turns, or none when an
   earlier abort of the same task ended them. *)
let abort e =
  Report.error e ;
  let l = !active in
  active := [] ;
  List.iter (fun f -> f ()) l

let abort_open e = match !active with [] -> () | _ :: _ -> abort e
