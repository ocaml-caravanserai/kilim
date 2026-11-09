(** An app on the fake DOM and the virtual host (design section 14), with
    simulated events.

    A simulated event finds its target, then calls
    {!Kilim.Private.Event.dispatch}: the same rules as in a browser. The
    handlers run from the target to the top, all in one turn. The payload comes
    from the target: its live [value], its live [checked], or the key of
    {!keydown}. The turn is discrete for a discrete event ([click], [input],
    [change], [submit] and [keydown]). A discrete event also gives the runtime
    its restore: after the next render, the restore writes the view values of
    the target, and of the other buttons of its radio group, where the live
    property is different (design section 11). It does nothing for a target that
    is not under the mount element.

    After each top-level call, the page runs the microtasks, then the undo of a
    prevented click, then the tasks of the dispatch (for example the restore
    task after a click on a checkbox), each with the microtasks after it. This
    is the order of a browser.

    The docs do not show [stats], [inspect] and [tree]: these are helpers for
    the budget tests, and they can change. *)

(** A page. ['out] is the type of the outputs that {!outputs} gives. *)
type 'out t

(** The type of the outputs of a page that has no outputs. *)
type none = |

(** A query found no element, or more than one. *)
exception Query of string

val start : ?host:Host.t -> Kilim.App.t -> none t
(** Mounts the app in a new [div] element. [host] defaults to a new host. Before
    the first turn, the page starts an empty [Http_mock] for its host, and sets
    [History_mock], [Websocket_mock] and [Storage_mock] as the drivers of
    kilim.router, kilim.websocket and kilim.storage. *)

val mount :
  (module C : Kilim.COMPONENT)
  -> ?host:Host.t -> C.props -> (C.out t -> 'a) -> 'a
(** [mount (module C) ~host props f] mounts one instance of [C] with [props] in
    a new [div] element, as the only child of an App, and gives the page to [f].
    The result is the result of [f]. The page gives the outputs of the instance
    with their type: see {!outputs}. An error of the instance goes to the root
    catch: the page then shows an empty text. After [f] returns or raises,
    [mount] stops the page: thus all tasks and subscriptions of the instance
    stop, and the next test starts with no timer of this page.
    {!Kilim_testing.mount} is this function. *)

val outputs : 'out t -> 'out list
(** The outputs of the component of the page, the oldest first. A page of
    {!start} has none. *)

val host : 'out t -> Host.t
(** The host of the page. *)

(**/**)

val http_mock : 'out t -> Http_mock_state.t
(** The state of [Http_mock] (internal). *)

(**/**)

val doc : 'out t -> Fake_dom.t
(** The fake document, with the counts of the DOM operations. *)

val root : 'out t -> Fake_dom.node
(** The mount element. *)

val html : 'out t -> string
(** [Fake_dom.to_html] of the mount element. *)

val find : 'out t -> By.t -> Fake_dom.node
(** @raise Query if no element or more than one element matches. *)

val click :
     'out t
  -> ?button:int
  -> ?alt:bool
  -> ?ctrl:bool
  -> ?meta:bool
  -> ?shift:bool
  -> ?prevented:bool
  -> By.t
  -> unit
(** A [click] event: a discrete turn. As in a browser, a click on a checkbox
    first toggles its [checked], and a click on a radio button first checks it
    (and unchecks its group). If a handler prevents the default, the click
    undoes this change after the turn and the microtasks, as a browser does
    after the dispatch: the checkbox toggles back. The radio button that was
    checked before becomes checked again, if it is still in the group of the
    target. If not, the target stays checked, as in Chrome. Then a task restores
    the target from the view.

    [~button] (default 0, the main button) and the modifier keys [~alt],
    [~ctrl], [~meta] and [~shift] (default [false]) are the values of the event
    for {!Kilim.Ev.link_click}. [~prevented:true] is a click whose default a
    listener of the page prevented before the dispatch reached the app. A
    browser sends [auxclick], not [click], for the middle button: the page sends
    [click] with each button, thus a test can examine the rule of the link. *)

val prevented : 'out t -> bool
(** [prevented t] is [true] if the default of the last event of the page is
    prevented, by a handler or with [~prevented]. For a click on a link, [false]
    tells that the browser follows the link. *)

val type_ : 'out t -> By.t -> string -> unit
(** [type_ t q s] types each character of [s]: the live value becomes the live
    value and the character, then an [input] event. One turn for each character.
*)

val input : 'out t -> By.t -> string -> unit
(** [input t q v] sets the live value to [v], then sends one [input] event. *)

val change : 'out t -> By.t -> string -> unit
(** [change t q v] sets the live value to [v], then sends a [change] event. *)

val submit : 'out t -> By.t -> unit
(** A [submit] event. *)

val keydown : 'out t -> ?composing:bool -> By.t -> string -> unit
(** [keydown t q key] sends a [keydown] event with this [key], for example
    ["Enter"]. With [~composing:true] ([isComposing] of the event, default
    [false]), no handler runs. *)

val keyup : 'out t -> ?composing:bool -> By.t -> string -> unit
(** As {!keydown}, for [keyup]: a [Macro] turn. *)

val focus : 'out t -> By.t -> unit
(** Gives the focus of the document to the target (see {!Fake_dom.focus}), then
    sends a [focusin] event: a [Macro] turn. On an element with an IME
    composition, it ends the composition and gives a discrete turn with the
    restore. It sends no [focusout] to the element that had the focus. *)

val blur : 'out t -> By.t -> unit
(** Takes the focus away from the target if it has the focus, then sends a
    [focusout] event: a [Macro] turn. On an element with an IME composition, it
    ends the composition and gives a discrete turn with the restore. *)

val compose :
  'out t -> ?order:[`Chrome | `Firefox] -> By.t -> string list -> unit
(** [compose t ~order q texts] is an IME composition. It gives the focus to the
    target with no event, then sends [compositionstart]. For each text, the live
    value becomes the start value and the text, then an [input] event runs. The
    last text is the commit. One task for each event, except the commit of
    [`Firefox]. The two orders:
    - [`Chrome] (the default): an [input] event with [isComposing] for each
      text, then [compositionend].
    - [`Firefox]: an [input] event with [isComposing] for each text before the
      last. Then in one task: the live value of the commit, [compositionend],
      then an [input] event with [isComposing] false.

    During the composition, the diff does not write [value] while the target has
    the focus. [compositionend] runs its handlers in a [Macro] turn, which does
    not render, and it posts a task that restores the target. In the order of
    Firefox, the last [input] event renders first. *)

val event : 'out t -> By.t -> string -> unit
(** [event t q name] sends the event [name] with the payloads of the target and
    the key [""], for example [mouseenter], [scroll], [wheel] or
    [compositionstart]. *)

val set_hidden : 'out t -> bool -> unit
(** Sets the hidden flag of the host, then tells the runtime. *)

val advance : 'out t -> Kilim.Span.t -> unit
(** [advance t span] is {!Host.advance} on the host of the page. *)

val run : 'out t -> unit
(** [run t] is {!Host.run} on the host of the page. *)

(**/**)

val stats : 'out t -> Kilim.Private.Runtime.stats
(** The counts of the runtime, for example turns, messages and renders. The
    budget tests use these counts. The fields can change. *)

val inspect : 'out t -> Kilim.Private.Runtime.inspect
(** The state of the queues of the runtime. The budget tests use these counts.
    The fields can change. *)

val tree : 'out t -> Kilim.Private.Runtime.tree
(** The counts of the instance tree. The budget tests use these counts. The
    fields can change. *)

(**/**)

val stop : 'out t -> unit
(** Stops the runtime, then the [Http_mock] of the page. *)

val expect_no_tasks : 'out t -> unit
(** Raises [Failure] if a timer is live or the token of the root has a child. *)
