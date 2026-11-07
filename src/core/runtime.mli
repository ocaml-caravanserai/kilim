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
      it runs in a new task.

    A turn never nests: an entry inside a turn only puts its messages in the
    queue. A push outside a turn asks the host for one turn in a microtask.

    An error in [init], [update], [view], a task, a message function or a DOM
    operation unmounts the root instance: its tasks stop, and the root shows the
    fallback. One more catch around the turn handles the errors outside these
    scopes, for example a fault of the host. *)

type env =
  { host: Host.t
  ; hidden: unit -> bool  (** [document.visibilityState = "hidden"]. *) }

(** The kind of a turn. *)
type kind =
  | Discrete
      (** A click, input, change, submit or keydown event: renders at once. *)
  | Frame
      (** The frame, the 100 ms timer, the render task of a hidden page,
          [visibilitychange], a frame of a task: renders if a render is pending
          or if the turn changed the model. *)
  | Macro
      (** Another DOM event, or a timer or a [post_task] callback of a task. *)
  | Micro  (** A push outside a turn, or a microtask of a task. *)

type stats =
  { mutable turns: int
  ; mutable messages: int  (** The calls of [update]. *)
  ; mutable dropped: int
  ; mutable views: int  (** The calls of [view]. *)
  ; mutable renders: int
        (** The render steps that called a view or wrote a fallback. *)
  ; mutable max_renders: int  (** The most renders in one turn. *)
  ; mutable requests: int  (** The microtask requests. *) }

type inspect =
  { messages_left: int
  ; continuations_left: int
  ; requests_left: int
  ; turn_requested: bool
  ; pending: bool  (** A render is pending. *)
  ; scheduled: bool  (** A frame, a timer or a task waits for the render. *)
  ; in_turn: bool
  ; latest_keys: int  (** The keys in the [Cmd.latest] table of the root. *)
  ; token_children: int  (** The children of the token of the root. *)
  ; alive: bool  (** The root instance is alive. *) }

type 'node t

val start : env -> 'node Dom.t -> parent:'node -> App.t -> 'node t
(** Runs the first turn: [init], the settle step, then the render that mounts
    the view as the last child of [parent]. The tasks get a host whose callbacks
    each run in a turn. *)

val event : 'node t -> kind -> (unit -> unit) -> unit
(** [event rt kind f] runs [f] (the handlers of one DOM event) in a turn. An
    exception from [f] goes to the root catch. Inside a turn, it only runs [f]:
    the messages go to the queue. *)

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
