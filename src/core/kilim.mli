(** kilim: a browser application library for OCaml (docs/design.html).

    An app is a model, an [update] function and a [view] function. [update]
    gives a new model and a command (the tasks to start). [view] gives a virtual
    DOM tree, and the runtime writes the changes to the DOM. This library has no
    browser code: kilim.browser starts an app in a page, and kilim.testing runs
    the same app as native code.

    The parts:
    - {!Span}: durations
    - {!Cancel} and {!Async}: cold tasks and their cancel tokens
    - {!View}, {!At}, {!Ev} and {!El}: the virtual DOM tree of a view
    - {!Key}: the keys of a keyed list
    - {!Unsafe_html}: HTML text with no check
    - {!Cmd} and {!Sub}: the tasks of [update], and the event sources of a model
    - {!App}: the root instance, which [Kilim_browser.run] starts
    - {!Component}, {!Ctx} and {!COMPONENT}: components with their own model and
      outputs
    - {!View.boundary}: an error scope in the view
    - {!Context}: shared state that a provider gives to the views below it
    - {!Url}: the URL check, the encoding and the locations of the router.

    The README, section "Getting started", shows a first app. *)

module Span = Span

(**/**)

(** The internals for the runtime, the platform libraries and the tests. They
    can change at any time. *)
module Private : sig
  val set_error_handler : (exn -> unit) -> unit
  (** The function that gets each error that the runtime writes to the console:
      - An exception with no error scope: from an end action, or from a callback
        of {!Async.make} after [ok] or [error].
      - The error of each root catch. A boundary catch does not call the error
        handler.
      - An error that reaches a boundary that is not alive, for example after a
        stop in a fallback.
      - Each error of the root fallback: of its function or of its write.
      - Each fault in the catch of the turn, for example a fault of the host.
      - An error of a platform library with {!error}, for example a missing
        mount element after the wait of [Kilim_browser.run].

      The default does nothing. *)

  val error : exn -> unit
  (** [error e] gives [e] to the error handler. *)

  val set_warning_handler : (string -> unit) -> unit
  (** The function that gets a warning of the runtime, for example 10 stops at
      the time limit in 1 s. The default does nothing. kilim.browser gives
      [console.warn]. *)

  module Host = Host
  module Cancel = Cancel
  module Dlist = Dlist
  module Run_queue = Run_queue
  module Async = Async
  module Key = Key
  module Unsafe_html = Unsafe_html
  module Names = Names
  module View = View
  module Dom = Dom
  module Tree = Tree
  module Instance = Instance
  module Diff = Diff
  module Reconcile = Reconcile
  module Boundary = Boundary
  module Context = Context
  module Cmd = Cmd
  module Sub = Sub
  module App = App
  module Event = Event
  module Runtime = Runtime
  module Shallow = Shallow
end

(**/**)

(** Cancel tokens. *)
module Cancel : sig
  (** A cancel token. Each task that the runtime starts has a new token. *)
  type t = Private.Cancel.t

  val cancelled : t -> bool
  (** [cancelled t] is [true] after the cancel of [t] or of a token above it. *)

  val on_end : t -> (unit -> unit) -> unit
  (** [on_end t f] runs [f] one time: at the cancel of [t], or when the step of
      [t] ends. If [t] is already cancelled or ended, [f] runs at once. An
      exception from [f] goes to the error handler, and the other actions still
      run. *)
end

(** Cold async tasks. Nothing runs until the runtime starts a task with a token.
*)
module Async : sig
  (** A task that gives a value of type ['a], or fails with an exception. *)
  type 'a t = 'a Private.Async.t

  val return : 'a -> 'a t
  (** [return x] is a task that gives [x]. *)

  val fail : exn -> 'a t
  (** [fail e] is a task that fails with [e]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] gives [f x] when [t] gives [x]. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind t f] runs [t], then runs the task [f x] when [t] gives [x]. *)

  (** The binding operators of the tasks. *)
  module Syntax = Private.Async.Syntax

  val sleep : Span.t -> unit t
  (** [sleep span] gives [()] after [span]. A cancel clears its timer. *)

  val first : 'a t list -> 'a t
  (** The first [ok] wins and cancels the other branches. If all branches fail,
      [first] gives the last error. [first []] fails with [Invalid_argument]
      when the task starts. *)

  val timeout : Span.t -> 'a t -> ('a, [`Timeout]) result t
  (** [timeout span t] gives [Ok x] when [t] gives [x], the error of [t] when
      [t] fails, or [Error `Timeout] after [span]. The end of one cancels the
      other. *)

  val make :
    (Cancel.t -> ok:('a -> unit) -> error:(exn -> unit) -> unit) -> 'a t
  (** [make start] binds a callback API. [start] gets the token of the step.
      [ok] and [error] have an effect one time only, and none after a cancel.
      The continuation never runs inside [start]: it goes on the run queue. An
      exception from [start] before [ok] or [error] is the error of the step.
      After [ok] or [error], it goes to the error handler. After a cancel, the
      runtime drops it.

      An end action of the token runs also when the step ends normally. Thus it
      must not stop work that continues after [ok]: for example, a fetch binding
      reads the body in the same step, and aborts only if the token is
      cancelled. Write each binding as a top-level function: under js_of_ocaml,
      a callback inside [update] can keep the variables of [update] alive. *)
end

(** The keys of {!View.keyed}: an int or a string. *)
module Key : sig
  (** A key. *)
  type t = Private.Key.t

  val int : int -> t
  (** [int n] is the key [n]. *)

  val string : string -> t
  (** [string s] is the key [s]. [int 1] and [string "1"] are different keys. *)
end

(** HTML text for {!Kilim.View.unsafe_html}, with no check (design section 13).
    Do not make a value from data of a user. *)
module Unsafe_html : sig
  (** HTML text. *)
  type t = Private.Unsafe_html.t

  val of_string : string -> t
  (** [of_string html] is [html], for a page with no Trusted Types. *)

  val to_string : t -> string
  (** [to_string h] is the HTML text of [h]. *)
end

(** Virtual DOM trees (design section 08). A view is an immutable value. *)
module View : sig
  (** A view that sends messages of type ['msg]. *)
  type 'msg t = 'msg Private.View.t

  val txt : string -> 'msg t
  (** [txt s] is a text node. An escape is not necessary in the DOM. *)

  val keyed : ('msg t list -> 'msg t) -> (Key.t * 'msg t) list -> 'msg t
  (** [keyed el rows] is the element [el []] with children that the diff
      compares by key, not by position. A row that keeps its key keeps its DOM
      node, also when it moves. For example:

      {@ocaml in=test/doc/keyed.ml[
      View.keyed El.ul
        (List.map (fun it -> (Key.int it.id, El.li [txt it.label])) items)
      ]}

      Raises [Invalid_argument] if [el []] is not an element, or is a void
      element. If [rows] has a duplicate key, the diff raises [Invalid_argument]
      before it writes the DOM of the element. *)

  val unsafe_html : el:('msg t list -> 'msg t) -> Unsafe_html.t -> 'msg t
  (** [unsafe_html ~el h] is the element [el []] with the content [h], which the
      diff writes with [innerHTML]. The diff compares the HTML text of [h]: if
      it does not change, the diff writes nothing. Thus the selection, the focus
      and the scroll inside the content stay.

      Raises [Invalid_argument] as {!keyed} does. *)

  val boundary : ?key:string -> fallback:(exn -> 'msg t) -> 'msg t -> 'msg t
  (** [boundary ~key ~fallback child] is [child] in an error scope (design
      section 09). The node has one child and no DOM node of its own: its DOM
      node is the DOM node of [child], or of the fallback.

      An error of a component goes to the nearest boundary above the instance:
      an error in [init], [update], [view], [subs] or [equal_props], in a task,
      a subscription or a message function of the instance, or in a DOM
      operation of its view. An error in the [~on] function of a parent goes to
      the boundary of the parent. The boundary then unmounts its whole subtree:
      the tasks and the subscriptions of each instance in it stop, and their
      messages are dropped. Then it shows [fallback exn] in the place of
      [child]. A boundary catch does not call the error handler. With no
      boundary above it, the error goes to the root catch of {!App.make}.

      The boundary keeps the error and the fallback until its place remounts: a
      new [key], or another node kind at its place. [fallback] runs again at
      each render of the parent, and the runtime patches the fallback: thus
      [fallback] must have no side effects. The boundary does not try [child]
      again: to try again, give a new [key]. The messages of the fallback go to
      the component or app whose view holds the boundary, as the messages of
      [child] do. An error in the fallback goes to the next boundary above, thus
      a fallback that raises cannot cause a loop. A boundary catch does not call
      the error handler.

      For example, a button of the fallback tries the counter again:
      {@ocaml in=test/doc/guarded.ml[
      View.boundary ~key:(string_of_int retries)
        ~fallback:(fun _ ->
          El.p
            [ txt "The counter failed."
            ; El.button ~ev:[Ev.click Retry] [txt "Retry"] ] )
        (Component.v
           (module Counter)
           ~on:(fun (Counter.Changed n) -> Count n)
           {Counter.step= 1; label= "Safe"} )
      ]} *)
end

val txt : string -> 'msg View.t
(** [txt] is {!View.txt}. *)

(** Attributes and controlled properties (design sections 08 and 13).

    An element joins the values of all of its [class] attributes, in order, with
    one space, and drops the empty values. For each other name, a duplicate
    raises [Invalid_argument] when the code builds the element. The names
    compare with the ASCII case ignored. *)
module At : sig
  (** An attribute or a controlled property. *)
  type t = Private.View.at

  val v : string -> string -> t
  (** [v name value] is the attribute [name="value"]. The name stays as you give
      it, and the checks ignore its ASCII case.

      A URL attribute ([href], [src], [action], [formaction], [poster], [cite],
      [data], [background], [srcset] and [ping]) takes only relative URLs and
      the schemes [http], [https], [mailto] and [tel]. The check finds the
      scheme as the URL parser of the browser does: it ignores the C0 control
      characters and the spaces at the start, and all tab, LF and CR characters.
      Thus [" javascript:"] and ["java\tscript:"] are refused. It checks each
      URL of [srcset], as the srcset parser of the browser finds them, and each
      URL of [ping].

      Raises [Invalid_argument] if [name] is not an ASCII letter or [_], then
      letters, digits, [_], [-] or [.] (thus a name with [:] is refused: use
      [href] in SVG), if [name] starts with [on] or is [srcdoc], or if a URL has
      a refused scheme. *)

  val none : t
  (** [none] adds nothing, for example
      [if editable then At.none else At.readonly]. *)

  val bool : string -> t
  (** [bool name] is [v name ""], for example [disabled]. *)

  val data : string -> string -> t
  (** [data name value] is [v ("data-" ^ name) value].

      Raises [Invalid_argument] as {!v} does, and if [name] is empty. *)

  val aria : string -> string -> t
  (** [aria name value] is [v ("aria-" ^ name) value].

      Raises [Invalid_argument] as {!v} does, and if [name] is empty. *)

  val class_ : string -> t
  (** [class_ c] is the attribute [class="c"]. An element joins all of its
      [class] attributes. *)

  val classes : string list -> t
  (** [classes l] is [class_] of the non-empty strings of [l], joined with one
      space. *)

  (** {1 Attributes with a value}

      Each one is [v] with its name. [href], [src] and [action] have the URL
      check. *)

  val id : string -> t

  val href : string -> t

  val src : string -> t

  val action : string -> t

  val method_ : string -> t

  val name : string -> t

  val type_ : string -> t

  val for_ : string -> t

  val alt : string -> t

  val title : string -> t

  val lang : string -> t

  val rel : string -> t

  val target : string -> t

  val placeholder : string -> t

  val style : string -> t

  val role : string -> t

  val aria_label : string -> t

  val tabindex : int -> t

  val width : string -> t

  val height : string -> t

  (** {1 Boolean attributes} *)

  val disabled : t

  val hidden : t

  val required : t

  val readonly : t

  val multiple : t

  val autofocus : t

  val selected : t

  (** {1 Controlled properties}

      The diff writes these values as properties, after all the other DOM
      operations of the mount or the patch. It compares the view value with the
      live DOM property, as React does, and writes only a different value. If a
      view has no [value] or [checked], the diff writes nothing, and the element
      keeps its value.

      The browser can change a value with no write of the diff: the user types,
      a radio button unchecks the other buttons of its group, a change of the
      options of a [select] can select a different option, and a range input
      keeps its value between [min] and [max]. The compare with the live
      property finds each of these changes. *)

  val value : string -> t

  val checked : bool -> t
end

(** Event handlers (design sections 08 and 11).

    The options [~prevent_default] and [~stop] are static: the listener applies
    them synchronously, before [update]. The default of each one is [false]. An
    element can have one handler for each DOM event name, thus not {!Ev.click}
    and {!Ev.check} together.

    The listener reads the payload synchronously. The message function (for
    example [f] of {!Ev.input}) runs when the message leaves the queue, thus its
    error goes to the instance, as for a task result. *)
module Ev : sig
  (** An event handler that sends messages of type ['msg]. *)
  type 'msg t = 'msg Private.View.handler

  val click : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [click msg] gives [msg] for each [click] event. *)

  val link_click : ?stop:bool -> 'msg -> 'msg t
  (** [link_click msg] is for a link ([El.a] with [At.href]) that the app
      follows itself, for example with a router. A plain click prevents the
      default and gives [msg]. A plain click uses the main button with no Alt,
      Ctrl, Meta or Shift key. Also, no handler below and no listener of the
      page prevented its default. For another click, the handler does not
      prevent the default and gives no message, thus the browser follows the
      link (for example, in a new tab). The [~stop] of the handler applies to
      each click: with [~stop], a click handler above (for example, of a card
      around the link) does not run for a Ctrl click on the link.

      The handler takes each plain click, also on an [a] with [target],
      [download] or a link to another origin. Thus use it only for the links
      that the app follows itself. The Enter key on a focused link sends a plain
      click. Ctrl+Enter sends a click with Ctrl, which the handler skips. A
      browser sends no [click] for the middle button (it sends [auxclick]): thus
      the middle button opens the link with no message. [link_click] uses the
      [click] event. Thus do not put {!click} and [link_click] on one element.
  *)

  val check : ?prevent_default:bool -> ?stop:bool -> (bool -> 'msg) -> 'msg t
  (** [check f] is for a checkbox or a radio button. It uses the [click] event,
      because the browser sends [input] and [change] after the restore of the
      controlled properties. The listener gives [f] the [checked] value before
      the restore. *)

  val input : ?prevent_default:bool -> ?stop:bool -> (string -> 'msg) -> 'msg t
  (** [input f] gives [f] the [value] of the target. *)

  val change : ?prevent_default:bool -> ?stop:bool -> (string -> 'msg) -> 'msg t
  (** [change f] gives [f] the [value] of the target. *)

  val submit : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [submit msg] gives [msg] for each [submit] event. To stop the browser
      submit, give [~prevent_default:true]. *)

  val keydown :
    ?prevent_default:bool -> ?stop:bool -> (string -> 'msg) -> 'msg t
  (** [keydown f] gives [f] the [key] of the event. *)

  val keyup : ?prevent_default:bool -> ?stop:bool -> (string -> 'msg) -> 'msg t
  (** [keyup f] gives [f] the [key] of the event. *)

  val focus : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [focus] uses [focusin], which bubbles. *)

  val blur : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [blur] uses [focusout], which bubbles. *)

  (** {1 Events with a listener on the element}

      These events use a listener on their element, not delegation at the root.
      The listener is passive if the handler has no [~prevent_default]: a
      passive listener cannot prevent the default, and the browser does not wait
      for it before it scrolls. A listener does nothing for an element that is
      not in the document. *)

  val mouseenter : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t

  val mouseleave : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t

  val scroll : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t

  val wheel : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [wheel] has no payload in this version. *)

  val touchstart : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [touchstart] has no payload in this version. *)

  val touchmove : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [touchmove] has no payload in this version. *)
end

(** Elements (design sections 08 and 13).

    A function of this module gives an element of the allowlist of HTML and SVG
    elements. The list does not contain [script], [iframe], [object], [embed],
    [base], [meta], [link], and the SVG animation elements [set], [animate],
    [animateMotion] and [animateTransform]. A camelCase SVG name becomes a
    snake_case function name, for example [linear_gradient] for
    [linearGradient].

    The namespace of an element comes from its place: [svg] starts the SVG
    namespace, and the children of [foreign_object] are HTML. Thus {!El.a},
    {!El.title} and {!El.style} in an [svg] element are SVG elements.

    Each function raises [Invalid_argument] for a duplicate attribute or handler
    (see {!At} and {!Ev}). *)
module El : sig
  (** An element with children. *)
  type 'msg el =
    ?at:At.t list -> ?ev:'msg Ev.t list -> 'msg View.t list -> 'msg View.t

  (** A void element, with no children. *)
  type 'msg void = ?at:At.t list -> ?ev:'msg Ev.t list -> unit -> 'msg View.t

  val v : string -> 'msg el
  (** [v name] is the element [name]. The check ignores the ASCII case, and the
      element gets the usual spelling of the name. For example, [v "DIV"] is
      [div] and [v "foreignobject"] is [foreignObject].

      Raises [Invalid_argument] if [name] (with the ASCII case ignored) is not
      on the allowlist, or if [name] is a void element and the children are not
      empty. *)

  (** {1 Void HTML elements} *)

  val area : 'msg void

  val br : 'msg void

  val col : 'msg void

  val hr : 'msg void

  val img : 'msg void

  val input : 'msg void

  val source : 'msg void

  val track : 'msg void

  val wbr : 'msg void

  (** {1 HTML elements} *)

  val a : 'msg el

  val abbr : 'msg el

  val address : 'msg el

  val article : 'msg el

  val aside : 'msg el

  val audio : 'msg el

  val b : 'msg el

  val bdi : 'msg el

  val bdo : 'msg el

  val blockquote : 'msg el

  val button : 'msg el

  val canvas : 'msg el

  val caption : 'msg el

  val cite : 'msg el

  val code : 'msg el

  val colgroup : 'msg el

  val data : 'msg el

  val datalist : 'msg el

  val dd : 'msg el

  val del : 'msg el

  val details : 'msg el

  val dfn : 'msg el

  val dialog : 'msg el

  val div : 'msg el

  val dl : 'msg el

  val dt : 'msg el

  val em : 'msg el

  val fieldset : 'msg el

  val figcaption : 'msg el

  val figure : 'msg el

  val footer : 'msg el

  val form : 'msg el

  val h1 : 'msg el

  val h2 : 'msg el

  val h3 : 'msg el

  val h4 : 'msg el

  val h5 : 'msg el

  val h6 : 'msg el

  val header : 'msg el

  val hgroup : 'msg el

  val i : 'msg el

  val ins : 'msg el

  val kbd : 'msg el

  val label : 'msg el

  val legend : 'msg el

  val li : 'msg el

  val main : 'msg el

  val map : 'msg el

  val mark : 'msg el

  val menu : 'msg el

  val meter : 'msg el

  val nav : 'msg el

  val ol : 'msg el

  val optgroup : 'msg el

  val option : 'msg el

  val output : 'msg el

  val p : 'msg el

  val picture : 'msg el

  val pre : 'msg el

  val progress : 'msg el

  val q : 'msg el

  val rp : 'msg el

  val rt : 'msg el

  val ruby : 'msg el

  val s : 'msg el

  val samp : 'msg el

  val search : 'msg el

  val section : 'msg el

  val select : 'msg el

  val slot : 'msg el

  val small : 'msg el

  val span : 'msg el

  val strong : 'msg el

  val style : 'msg el

  val sub : 'msg el

  val summary : 'msg el

  val sup : 'msg el

  val table : 'msg el

  val tbody : 'msg el

  val td : 'msg el

  val textarea : 'msg el

  val tfoot : 'msg el

  val th : 'msg el

  val thead : 'msg el

  val time : 'msg el

  val title : 'msg el

  val tr : 'msg el

  val u : 'msg el

  val ul : 'msg el

  val var : 'msg el

  val video : 'msg el

  (** {1 SVG elements} *)

  val svg : 'msg el

  val g : 'msg el

  val defs : 'msg el

  val symbol : 'msg el

  val use : 'msg el

  val image : 'msg el

  val switch : 'msg el

  val desc : 'msg el

  val metadata : 'msg el

  val path : 'msg el

  val rect : 'msg el

  val circle : 'msg el

  val ellipse : 'msg el

  val line : 'msg el

  val polyline : 'msg el

  val polygon : 'msg el

  val text : 'msg el

  val tspan : 'msg el

  val text_path : 'msg el

  val marker : 'msg el

  val pattern : 'msg el

  val clip_path : 'msg el

  val mask : 'msg el

  val linear_gradient : 'msg el

  val radial_gradient : 'msg el

  val stop : 'msg el

  val filter : 'msg el

  val fe_blend : 'msg el

  val fe_color_matrix : 'msg el

  val fe_component_transfer : 'msg el

  val fe_composite : 'msg el

  val fe_convolve_matrix : 'msg el

  val fe_diffuse_lighting : 'msg el

  val fe_displacement_map : 'msg el

  val fe_distant_light : 'msg el

  val fe_drop_shadow : 'msg el

  val fe_flood : 'msg el

  val fe_func_a : 'msg el

  val fe_func_b : 'msg el

  val fe_func_g : 'msg el

  val fe_func_r : 'msg el

  val fe_gaussian_blur : 'msg el

  val fe_image : 'msg el

  val fe_merge : 'msg el

  val fe_merge_node : 'msg el

  val fe_morphology : 'msg el

  val fe_offset : 'msg el

  val fe_point_light : 'msg el

  val fe_specular_lighting : 'msg el

  val fe_spot_light : 'msg el

  val fe_tile : 'msg el

  val fe_turbulence : 'msg el

  val foreign_object : 'msg el

  val view : 'msg el
end

(** Commands: the work that [update] asks for, as a value. *)
module Cmd : sig
  (** A command that gives messages of type ['msg]. *)
  type 'msg t = 'msg Private.Cmd.t

  val none : 'msg t
  (** [none] starts no task. *)

  val batch : 'msg t list -> 'msg t
  (** The tasks start in the order of the list. *)

  val task : 'msg Async.t -> 'msg t
  (** The runtime starts the task in the settle step, after the messages, under
      a new token. Its result is a message. An error goes to the error scope of
      the instance. *)

  val latest : key:string -> 'msg Async.t -> 'msg t
  (** As [task], but a new task with the same key cancels the old one, also if
      the old one has not started. A result of the old task that waits in the
      queue does nothing. The keys are local to the instance. *)

  val map : ?scope:string -> ('a -> 'msg) -> 'a t -> 'msg t
  (** [scope] adds a level to the keys of [latest] in the command. The key is a
      path: [map ~scope:"a" (latest ~key:"b" t)] and [latest ~key:"a/b" t] do
      not collide. *)
end

(** Subscriptions: the event sources that the model needs, with keys (design
    section 08). [subs] of {!Kilim.App.make} gives a set of subscriptions for a
    model. After a render, the runtime compares the keys of the set with the
    keys of the last set. A new key starts its subscription, a removed key stops
    it, and a key that stays keeps it. *)
module Sub : sig
  (** A set of subscriptions that give messages of type ['msg]. *)
  type 'msg t = 'msg Private.Sub.t

  val none : 'msg t
  (** [none] is the empty set. *)

  val batch : 'msg t list -> 'msg t
  (** [batch l] has all the subscriptions of the sets of [l]. *)

  val every : Span.t -> 'msg -> 'msg t
  (** [every span msg] sends [msg] [span] after its start, then [span] after
      each delivery (a fixed delay, not a fixed rate). The key comes from
      [span]: two [every] with the same span get their position among them in
      the key. A key that stays sends the newest [msg].

      Raises [Invalid_argument] if [span] is zero. In [subs], this goes to the
      error scope of the instance. *)

  val run : key:string -> (('msg -> unit) -> unit Async.t) -> 'msg t
  (** [run ~key f] starts the task [f send] at a render. [send m] sends the
      message [m]. Put each parameter of [f] in [key], for example the room of a
      chat. A key that stays keeps its task. A removed key cancels it, and then
      [send] has no effect. If the task ends, the key stays and the task does
      not start again. An error of the task goes to the error scope of the
      instance.

      Raises [Invalid_argument] for a duplicate key, in [subs]. *)
end

(** The context of the [update] of a component (design section 09). *)
module Ctx : sig
  (** The context of a component with the message type ['msg] and the output
      type ['out]. *)
  type ('msg, 'out) t = ('msg, 'out) Private.Cmd.ctx

  val emit : ('msg, 'out) t -> 'out -> 'msg Cmd.t
  (** [emit ctx o] is a command that gives the output [o] to the parent. The
      runtime puts [o] at the back of the message queue as an item of the
      parent, in the same turn. When the item leaves the queue, the newest [~on]
      function of the parent makes the message of the parent, and an error in
      [~on] goes to the error scope of the parent. If the parent is not alive
      then, the item is dropped. {!Cmd.map} does not change an output. *)
end

(** An application: the root instance. *)
module App : sig
  (** An app that {!make} gives, for [Kilim_browser.run] or a page of
      kilim.testing. *)
  type t = Private.App.t

  val make :
       init:(unit -> 'model * 'msg Cmd.t)
    -> update:('model -> 'msg -> 'model * 'msg Cmd.t)
    -> view:('model -> 'msg View.t)
    -> ?subs:('model -> 'msg Sub.t)
    -> ?fallback:(exn -> 'msg View.t)
    -> unit
    -> t
  (** After each render of a new model, the runtime calls [subs] and starts the
      new keys. If a turn ends with a render that is pending, and the model
      changed since the last call of [subs], the runtime calls [subs] at the end
      of the turn, stops the removed keys, and gives the new message functions
      to the keys that stay. The new keys start at the render.

      An error in [init], [update], [view], [subs], a task, a subscription, a
      message function or a DOM operation of the app unmounts the app: its tasks
      and its subscriptions stop. Then the root shows [fallback exn] (default:
      an empty text). An error of a component with no {!View.boundary} above it
      goes to this catch too. The messages of the fallback do nothing, and their
      message functions do not run. The error goes to the error handler. An
      error in [fallback] goes to the error handler, and the root then shows an
      empty text. *)
end

(** Components (design section 09). A component is a module with its own model,
    messages, tasks and subscriptions. Its interface is [props] (from the
    parent) and [out] (to the parent). Its [.mli] file keeps [model] and [msg]
    abstract: the parent cannot read the state or send a private message.

    A counter with an output, in [counter.ml]:
    {@ocaml file=test/doc/counter.ml[
    open Kilim
    include Component.Defaults

    type props = {step: int; label: string}

    type model = int

    type msg = Incr | Decr

    type out = Changed of int

    let id : (props, model, msg, out) Component.id = Component.id ()

    let init _ = (0, Cmd.none)

    let update ctx p n = function
      | Incr ->
          let n = n + p.step in
          (n, Ctx.emit ctx (Changed n))
      | Decr ->
          let n = n - p.step in
          (n, Ctx.emit ctx (Changed n))

    let view p n =
      El.div
        [ El.span [txt p.label]
        ; El.button ~ev:[Ev.click Decr] [txt "-"]
        ; El.output [txt (string_of_int n)]
        ; El.button ~ev:[Ev.click Incr] [txt "+"] ]
    ]}

    Its interface, in [counter.mli]:
    {@ocaml file=test/doc/counter.mli[
      type props = {step: int; label: string}

      type out = Changed of int

      include Kilim.COMPONENT with type props := props and type out := out
    ]}

    In the view of the parent, a new key resets the counter:
    {@ocaml in=test/doc/parent.ml[
    Component.v
      (module Counter)
      ~key:("left/" ^ string_of_int m.resets)
      ~on:(fun (Counter.Changed n) -> Count n)
      {Counter.step= 1; label= "Left"}
    ]}

    A component with no outputs has the empty type [out], in [light.ml]:
    {@ocaml file=test/doc/light.ml[
    open Kilim
    include Component.Defaults

    type props = unit

    type model = bool

    type msg = Flip

    type out = |

    let id : (props, model, msg, out) Component.id = Component.id ()

    let init () = (false, Cmd.none)

    let update _ () on Flip = (not on, Cmd.none)

    let view () on =
      El.button ~ev:[Ev.click Flip] [txt (if on then "on" else "off")]
    ]}

    Its [~on] function has no case:
    {@ocaml in=test/doc/parent.ml[
    Component.v (module Light) ~on:(function _ -> .) ()
    ]}

    An error of a component goes to the nearest {!View.boundary} above it, or to
    the root catch of {!App.make}: an error in [init], [update], [view], [subs],
    [equal_props], a task, a subscription or a message function of the
    component. An error in the [~on] function of a parent goes to the boundary
    of the parent. *)
module Component : sig
  (** The typed identity of a component: it witnesses the types of the props,
      the model, the messages and the outputs. *)
  type ('p, 'm, 'msg, 'o) id = ('p, 'm, 'msg, 'o) Private.View.id

  val id : unit -> ('p, 'm, 'msg, 'o) id
  (** [id ()] makes a new identity. Make it one time for each component, at the
      top level: each call gives a different identity. A module that includes a
      component and changes its props does not compile, because its id has the
      old props type. *)

  val shallow_equal : 'a -> 'a -> bool
  (** The default props compare of a component. It compares two records one
      field at a time. Its contract: if it gives [true], the two values give the
      same view. A [false] result for equal values costs only one more render.

      [shallow_equal a b] is [true] if the field compare finds [a] and [b]
      equal. If not, and [a] and [b] are two records, two tuples, two values of
      a variant with the same constructor, or two arrays, it compares their
      fields with the field compare. A closure, a lazy value that is not forced,
      an object and a custom block (for example an [int64]) have no scan. In
      native code, a forced lazy value can become its value, and then it
      compares as its value.

      The field compare depends on the build:
      - Native code and wasm_of_ocaml: [x == y], or two strings with the same
        characters, or two floats with the same bits, also in a record or an
        array of floats.
      - js_of_ocaml with kilim.browser: [Object.is].

      Thus [0.] and [-0.] are not equal in these builds, also in a field. A
      js_of_ocaml program without kilim.browser (for example a test with
      kilim.testing) keeps the default compare: [==] and the compare by tag.
      There [==] is [===], thus [0.] and [-0.] can compare equal. Also a JS
      array that starts with an unsigned integer can take the compare by tag:
      for example the JS arrays [[253, 1]] and [[253, 2]] compare equal, as two
      NaN floats. Two closures are equal only if they are the same closure. Some
      results differ by build, but only in the safe direction: for example two
      equal [int32] values are equal under js_of_ocaml only, and two NaN values
      with different bits are equal under js_of_ocaml only.

      The props must be immutable: a mutable field that changes in place
      compares as equal. *)

  (** The signature of a component. *)
  module type S = sig
    (** The input from the parent. *)
    type props

    (** The state of the instance. Keep it immutable: the runtime compares the
        old and the new model by physical equality. *)
    type model

    (** The messages of the instance: from its view, its tasks and its
        subscriptions. *)
    type msg

    (** The outputs for the parent. Give the empty type [|] for a component with
        no outputs. *)
    type out

    val id : (props, model, msg, out) id
    (** The identity of the component: make it one time, at the top level, with
        {!Component.val-id}. *)

    val init : props -> model * msg Cmd.t
    (** The mount calls [init]. Its commands become task requests: the tasks
        start in the settle step after the render. *)

    val update : (msg, out) Ctx.t -> props -> model -> msg -> model * msg Cmd.t
    (** [update ctx props model msg]. [props] are the props of the last render
        of the instance. The instance renders again if the new model is not
        physically the same as the old model. {!Ctx.emit} gives an output to the
        parent. *)

    val view : props -> model -> msg View.t
    (** The view of the instance. The runtime calls it at the mount, when the
        model changed, and when [equal_props] gives [false]. It runs before the
        diff writes the DOM of the instance. *)

    val subs : props -> model -> msg Sub.t
    (** The subscriptions of the instance, as [?subs] of {!App.make}. At the end
        of each render, the runtime calls [subs] for each new instance and for
        each instance with a new model or new props, and starts the new keys. If
        a turn ends with a render that is pending, the runtime calls [subs] for
        each instance whose model changed, stops the removed keys, and gives the
        new message functions to the keys that stay. An instance that the render
        removes starts no subscription. The unmount stops each subscription of
        the instance. *)

    val equal_props : props -> props -> bool
    (** [equal_props old_props new_props]. If it gives [false], the instance
        renders with the new props. If it gives [true], the instance keeps its
        old props and renders only if its model changed. *)
  end

  (** Defaults for [include]: [subs] gives {!Sub.none}, and [equal_props] is
      {!shallow_equal}. Include it first, before your own [subs] and
      [equal_props]: a later [include] replaces your functions, with no warning.
      With [Defaults.subs], the instance has no subscription pass. *)
  module Defaults : sig
    val subs : 'p -> 'm -> 'msg Sub.t
    (** [subs _ _] is {!Sub.none}. *)

    val equal_props : 'p -> 'p -> bool
    (** [equal_props] is {!shallow_equal}. *)
  end

  val v :
    (module C : S)
    -> ?key:string -> on:(C.out -> 'msg) -> C.props -> 'msg View.t
  (** [v (module C) ~key ~on props] is a view node with an instance of [C]. The
      node has no DOM node of its own: its DOM node is the DOM node of the view
      of the instance.

      The identity of an instance is its parent instance, its place and [C.id].
      The place is the position among the siblings, or the key in {!View.keyed}:
      thus a keyed move does not remount. If the id or [key] at a place changes,
      the runtime unmounts the old instance and mounts a new one: thus a new
      [key] resets the instance, and [init] runs again. The unmount stops all
      tasks and subscriptions of the instance and of its children, and drops
      their messages.

      A render of the parent compares the props with [C.equal_props], and always
      takes the new [on]. [on] makes a message of the parent from an output,
      when the output leaves the queue.

      Two edge cases: a module made inside [view] (a local module or a functor
      application) gets a new id at each render, thus it remounts each time. Two
      modules that share one id (for example by [include]) do not remount when
      they change places. Do not do these things. *)
end

(** The signature of a component: {!Component.S}. *)
module type COMPONENT = Component.S

(** Shared state (design section 10). A context is a typed key. A provider in
    the view gives a value to the views below it, also to the views of the
    components below it. A read gets the value of the nearest provider above it,
    or the default value of the context.

    The theme of a page, in [themed.ml]:
    {@ocaml file=test/doc/themed.ml[
    open Kilim

    type theme = Light | Dark

    let theme : theme Context.t = Context.create ~default:Light ()

    let theme_class = function Light -> "light" | Dark -> "dark"

    (* A component that reads the theme in its view. *)
    module Toolbar = struct
      include Component.Defaults

      type props = string

      type model = unit

      type msg = |

      type out = |

      let id : (props, model, msg, out) Component.id = Component.id ()

      let init _ = ((), Cmd.none)

      let update _ _ () = function (_ : msg) -> .

      let view title () =
        Context.read theme (fun t ->
            El.nav ~at:[At.class_ (theme_class t)] [txt title] )
    end

    let view (dark : bool) : unit View.t =
      Context.provide theme
        (if dark then Dark else Light)
        (El.div
           [ Component.v (module Toolbar) ~on:(function _ -> .) "Tools"
           ; Context.read theme (fun t -> El.p [txt (theme_class t)]) ] )
    ]} *)
module Context : sig
  (** A context with values of type ['a]. *)
  type 'a t

  val create : default:'a -> unit -> 'a t
  (** [create ~default ()] makes a new context. Make it one time, at the top
      level: each call gives a different context. A read with no provider of the
      context above it gets [default]. *)

  val provide : 'a t -> 'a -> 'msg View.t -> 'msg View.t
  (** [provide c v child] gives the value [v] of [c] to [child] and to the views
      below it. The node has one child and no DOM node of its own: its DOM node
      is the DOM node of [child]. It keeps its last value and its readers, also
      in a keyed move.

      A render of the view compares [v] with the last value by
      {!Component.shallow_equal}. If the value changed, the readers render in
      the same render, also a reader below a component that [equal_props] skips.
      The other components below the provider do not render. If the two values
      are equal, the provider keeps the old value, as a component keeps its old
      props: the readers get the old value.

      A provider is a node kind in identity: if a provider appears or disappears
      at a place, or if its context changes, the subtree at that place remounts,
      as for {!View.boundary}. *)

  val read : 'a t -> ('a -> 'msg View.t) -> 'msg View.t
  (** [read c f] is the view [f v], where [v] is the value of the nearest
      provider of [c] above the read, or the default value of [c]. A provider in
      the same view counts. The read is not a node: the diff calls [f] during
      the render, and puts [f v] in the place of the read.

      The read registers the component (or the app) whose view the diff renders:
      a new value of the provider renders that component again. This is not
      always the component whose code made the read: a read in the props of a
      child, which the view of the child gives, registers the child. Each render
      removes the old registrations of the component, thus a read can be
      conditional. Many reads of one provider in one view render the component
      one time.

      [f] runs in the diff. An error in [f] goes to the nearest {!View.boundary}
      above the read, whose fallback replaces the subtree, or to the root catch
      of {!App.make}. [f] must have no side effects. *)
end

(** URLs (design sections 09 and 13).

    The functions work on the bytes of OCaml strings, in UTF-8. *)
module Url : sig
  val permitted : string -> bool
  (** [permitted url] is [true] if [url] is relative, or if its scheme is
      [http], [https], [mailto] or [tel]. This is the URL check of {!At}: for
      example, [At.href] raises for a URL that is not permitted. The check finds
      the scheme as the WHATWG URL parser does. *)

  val encode_component : string -> string
  (** [encode_component s] encodes [s] as [encodeURIComponent] does: each byte
      that is not an ASCII letter, a digit or one of [- _ . ! ~ * ' ( )] becomes
      [%XX], with uppercase hex digits. *)

  val decode_component : string -> string
  (** [decode_component s] decodes each [%XX] of [s] to its byte. It does not
      raise: a [%] with no two hex digits after it stays. The result is not
      always valid UTF-8, because the bytes of [%XX] are not examined. For a
      string from {!encode_component}, it gives the original string. *)

  val query : string -> (string * string) list
  (** [query s] gives the pairs of the query [s], as [URLSearchParams] does. [s]
      can start with [?]. The pairs have the order of [s]. A [&] separates the
      pairs, and the first [=] of a pair separates the name and the value. A
      pair with no [=] has the value [""]. An empty pair is not in the result. A
      [+] is a space, and each [%XX] is a byte, as {!decode_component} does.
      Then each part that is not valid UTF-8 becomes U+FFFD. *)

  val with_query : string -> (string * string) list -> string
  (** [with_query url pairs] adds [pairs] to the query of [url], before the
      fragment. Each name and each value is encoded with {!encode_component}. If
      [url] has no [?], a [?] comes first. If the query of [url] is not empty
      and does not end with [&], a [&] comes first. With no pair, the result is
      [url]. For example, [with_query "/s" ["q", "a b"]] is ["/s?q=a%20b"]. *)

  (** A location of the router: the path, the query and the fragment of a URL,
      decoded. *)
  type location = Url.location =
    {path: string list; query: (string * string) list; fragment: string option}

  val parse : string -> location
  (** [parse s] gives the location of [s], the path, the query and the fragment
      of a URL (for example ["/users/42?tab=a#top"]). The path has no [/] at the
      start. Each segment of the path between two [/] is decoded with
      {!decode_component}. Thus ["/"] and [""] have the path [[]], and ["/a/"]
      has the path [["a"; ""]]. A first segment ["."] before an empty segment is
      not in the path: ["/.//a"] has the path [[""; "a"]] (see {!to_string}).
      The query comes from the text between the first [?] and the first [#], as
      {!val-query} gives it. The fragment is the decoded text after the first
      [#], or [None] with no [#]. The path and the fragment keep bytes that are
      not valid UTF-8. The query replaces them with U+FFFD.

      A browser removes each ["."] segment, and each [".."] segment with the
      segment before it, also in the form ["%2E"] (see {!to_string}). Thus the
      path of [location] in a browser has no such segment. *)

  val to_string : location -> string
  (** [to_string l] is the URL of [l]: [/], then the segments encoded with
      {!encode_component} and separated by [/], then the query with
      {!with_query} if it has a pair, then the encoded fragment after [#].

      If the first segment is empty and more segments follow, the URL starts
      with ["/./"], as the WHATWG URL serializer does. Without it, the path
      [[""; "evil.example"; "x"]] gives ["//evil.example/x"], a URL of another
      host, which {!permitted} accepts. Then [parse (to_string l)] is [l],
      except for the path [[""]], which gives ["/"], for a first segment ["."]
      before an empty segment, and for a query with text that is not valid
      UTF-8.

      A ["."] or [".."] segment does not survive a browser: the browser removes
      a ["."] segment, and a [".."] segment with the segment before it, also in
      the form ["%2E"]. For example, the browser changes the URL of
      [["a"; ".."; "b"]] to ["/b"], another route of the same app. Thus an app
      that puts user data in a segment of a path must refuse or change ["."] and
      [".."]. *)
end
