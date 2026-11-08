(** The runtime of design section 11: the turn, the settle step, the render and
    its timing, the [Cmd.latest] table, the drop rules, and the error rules of
    the root.

    A turn runs the work of one host callback or one DOM event:
    + The callback or the handlers of the event. Their messages go to the queue.
    + The settle step: drain the run queue, take one message, or start the task
      requests (when no message waits), again and again until the three queues
      are empty.
    + The render, at once in a [Discrete] or [Frame] turn and in the first turn
      of {!start}, at most 3 times in one turn. In the other turns, the render
      is pending: it runs at the next frame, or after 100 ms. In a hidden page,
      it runs in a new task. After the view of a new model, the render calls
      [subs] (the subscription pass): it stops the removed keys, gives the new
      message functions to the keys that stay, and starts the new keys. A [send]
      at a start is new work: the turn settles again. At its end, the render
      runs the restores of the discrete events.
    + The early call: if the turn ends with a render that is pending, and the
      model changed since the last pass, the turn calls [subs] again. It stops
      the removed keys and gives the new message functions to the keys that
      stay. The new keys start at the render. The turn settles the work that the
      end actions of the removed keys push.

    A turn never nests: an entry inside a turn only puts its messages in the
    queue. A push outside a turn asks the host for one turn in a microtask.

    The time limit: the settle step stops when the busy sum of the turns since
    the last tick passes 50 ms, or when the wall time since the last new clock
    passes 1,000 ms. The check comes before each settle action and at each clock
    read of the drain. Then the rest waits in the queues, the render is pending,
    and a continuation in a new task goes on. The end of each turn posts one
    tick, which sets the busy sum to 0. A turn gets a new clock if it is not a
    [Micro] turn, or if no tick is pending. After 10 stops in 1 s, the runtime
    writes one warning.

    An error in [init], [update], [view], [subs], a task, a subscription, a
    message function or a DOM operation unmounts the root instance: its tasks
    and its subscriptions stop, and the root shows the fallback. One more catch
    around the turn handles the errors outside these scopes, for example a fault
    of the host. *)

(** A virtual cost for the test host. *)
type cost =
  | Update  (** A call of [update]. *)
  | Drain  (** A clock read of the drain. *)

type env =
  { host: Host.t
  ; hidden: unit -> bool  (** [document.visibilityState = "hidden"]. *)
  ; cost: cost -> unit
        (** The test host moves its virtual clock. A browser does nothing. *)
  ; same: 'a. 'a -> 'a -> bool
        (** The identity of the platform. An instance is dirty after [update] if
            [same model old_model] is [false]. [( == )] in native code and in
            the wasm_of_ocaml build, [Object.is] in the js_of_ocaml build. *) }

(** The kind of a turn. *)
type kind =
  | Discrete
      (** A click, input, change, submit or keydown event: renders at once. *)
  | Frame
      (** The frame, the 100 ms timer, the render task of a hidden page,
          [visibilitychange], a frame of a task: renders if a render is pending
          or if the turn changed the model. *)
  | Macro
      (** Another DOM event, a timer or a [post_task] callback of a task, or the
          continuation after a stop at the limit. *)
  | Micro  (** A push outside a turn, or a microtask of a task. *)

type stats =
  { mutable turns: int
  ; mutable clocks: int  (** The new clocks. *)
  ; mutable messages: int  (** The calls of [update]. *)
  ; mutable dropped: int
  ; mutable views: int  (** The calls of [view]. *)
  ; mutable renders: int
        (** The render steps that called a view or wrote a fallback. *)
  ; mutable max_renders: int  (** The most renders in one turn. *)
  ; mutable stops: int  (** The stops at the limit. *)
  ; mutable yields: int  (** The posted continuations. *)
  ; mutable warnings: int
  ; mutable requests: int  (** The microtask requests. *)
  ; mutable restores: int  (** The restore functions that ran. *) }

type inspect =
  { messages_left: int
  ; continuations_left: int
  ; requests_left: int
  ; turn_requested: bool
  ; continuing: bool  (** A continuation is posted. *)
  ; pending: bool  (** A render is pending. *)
  ; scheduled: bool  (** A frame, a timer or a task waits for the render. *)
  ; tick_pending: bool
  ; in_turn: bool
  ; latest_keys: int  (** The keys in the [Cmd.latest] table of the root. *)
  ; sub_keys: int  (** The recorded subscription keys of the root. *)
  ; restores_left: int  (** The restore functions that wait. *)
  ; token_children: int  (** The children of the token of the root. *)
  ; alive: bool  (** The root instance is alive. *) }

type 'node t

val start : env -> 'node Dom.t -> parent:'node -> App.t -> 'node t
(** Runs the first turn: [init], the settle step, then the render that mounts
    the view as the last child of [parent]. The tasks get a host whose callbacks
    each run in a turn. [start] is {!create}, then {!init}. *)

val create : env -> 'node Dom.t -> parent:'node -> App.t -> 'node t
(** The runtime of the app, with no turn: nothing of the app runs. Thus a
    platform can record the runtime before the first turn, and a trap in the
    first turn leaves a runtime that it can stop. *)

val init : 'node t -> unit
(** Runs the first turn, one time: a second call does nothing. *)

val event : 'node t -> kind -> ?restore:(unit -> unit) -> (unit -> unit) -> unit
(** [event rt kind ~restore f] runs [f] (the handlers of one DOM event) in a
    turn. An exception from [f] goes to the root catch. Inside a turn, it only
    runs [f]: the messages go to the queue.

    A discrete event gives [restore]: it restores the controlled properties of
    the target from the view (design section 11). It goes into the restore list,
    which runs at the end of the next render step, in this turn or in a later
    one. Thus a turn that stops at the limit before its render does not restore
    from an older view. A turn renders also with no model change if a restore
    waits. A restore must do nothing for a node that is not in the document. An
    exception from it goes to the error handler, and the other restores still
    run. A stop clears the list. *)

val visibility_changed : 'node t -> unit
(** If the page is hidden and a render is pending, a turn renders at once. *)

val stop : 'node t -> unit
(** Cancels all tasks, removes the DOM of the root, and empties the queues.
    Later entries and callbacks do nothing. *)

val stats : 'node t -> stats
(** A copy of the counts. *)

val inspect : 'node t -> inspect
(** The flags and counts of the runtime and of the root instance, for the tests.
*)

(** The counts of the instance tree, for the leak tests. *)
type tree =
  { instances: int  (** The live instances below the root. *)
  ; tokens: int
        (** The live tokens below the token of the root, at all levels: the
            tokens of the instances and of their tasks. *)
  ; child_links: int
        (** The entries of the child lists, from the root. A dead instance
            leaves the list of its parent: thus [child_links] is [instances]
            with no leak. *)
  ; queued: int  (** The instances in the work queue of the render. *) }

val tree : 'node t -> tree

val abort : exn -> unit
(** [abort e] gives [e] to the error handler one time, then ends each turn that
    did not end, as the catch of a turn does. If work stays in a queue, it asks
    for a turn. Under wasm_of_ocaml, a trap (for example an illegal cast) is a
    JS exception that no OCaml catch sees: it leaves the turn open, and the app
    stops. Call [abort] only when no OCaml code of a turn runs. kilim.browser
    calls it from a microtask after the [error] event of such an error: a
    microtask runs only when the JS stack is empty, thus each open turn is then
    dead. A turn that ends normally removes only its own entry from the list of
    the open turns, thus a dead nested turn of another runtime stays in the list
    for [abort]. *)

val abort_open : exn -> unit
(** [abort_open e] is [abort e] if a turn did not end, else it does nothing (no
    report). kilim.browser calls it from a microtask after an [error] event
    whose [error] is null: WebKit 27.2 gives a trap of a [wasm:js-string]
    function so. In a microtask, each open turn is dead, thus the abort is
    correct for each such event. With no open turn, the event is not a trap of a
    turn (for example a "Script error." of a script of another origin), and it
    gives no report. *)
