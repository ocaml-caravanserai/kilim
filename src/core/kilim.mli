(** kilim: a browser application library for OCaml (docs/design.html).

    This version has the async tasks and the cancel tokens of section 12, the
    view, the commands and the app of section 08, and the diff of section 11. *)

module Span = Span

(** The internals for the runtime and the tests. They can change at any time. *)
module Private : sig
  val set_error_handler : (exn -> unit) -> unit
  (** The function that gets an exception with no error scope: from an end
      action, or from a callback of {!Async.make} after [ok] or [error]. The
      default does nothing. *)

  val set_warning_handler : (string -> unit) -> unit
  (** The function that gets a warning of the runtime, for example 10 stops at
      the time limit in 1 s. The default does nothing. kilim.browser gives
      [console.warn]. *)

  module Host = Host
  module Cancel = Cancel
  module Run_queue = Run_queue
  module Async = Async
  module Key = Key
  module Unsafe_html = Unsafe_html
  module Names = Names
  module View = View
  module Dom = Dom
  module Diff = Diff
  module Cmd = Cmd
  module App = App
end

(** Cancel tokens. *)
module Cancel : sig
  type t = Private.Cancel.t

  val cancelled : t -> bool

  val on_end : t -> (unit -> unit) -> unit
  (** [on_end t f] runs [f] one time: at the cancel of [t], or when the step of
      [t] ends. If [t] is already cancelled or ended, [f] runs at once. An
      exception from [f] goes to the error handler, and the other actions still
      run. *)
end

(** Cold async tasks. Nothing runs until the runtime starts a task with a token.
*)
module Async : sig
  type 'a t = 'a Private.Async.t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax = Private.Async.Syntax

  val sleep : Span.t -> unit t

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
  type t = Private.Key.t

  val int : int -> t

  val string : string -> t
  (** [string s] is the key [s]. [int 1] and [string "1"] are different keys. *)
end

(** HTML text for {!View.unsafe_html}, with no check (design section 13). Do not
    make a value from data of a user. *)
module Unsafe_html : sig
  type t = Private.Unsafe_html.t

  val of_string : string -> t
  (** [of_string html] is [html], for a page with no Trusted Types. *)

  val to_string : t -> string
  (** [to_string h] is the HTML text of [h]. *)
end

(** Virtual DOM trees (design section 08). A view is an immutable value. *)
module View : sig
  type 'msg t = 'msg Private.View.t

  val txt : string -> 'msg t
  (** [txt s] is a text node. An escape is not necessary in the DOM. *)

  val keyed : ('msg t list -> 'msg t) -> (Key.t * 'msg t) list -> 'msg t
  (** [keyed el rows] is the element [el []] with children that the diff
      compares by key, not by position. A row that keeps its key keeps its DOM
      node, also when it moves. For example:

      {[
      View.keyed El.ul
        (List.map (fun it -> (Key.int it.id, El.li [txt it.label])) items)
      ]}

      @raise Invalid_argument
        if [el []] is not an element, or is a void element. If [rows] has a
        duplicate key, the diff raises [Invalid_argument] before it writes the
        DOM of the element. *)

  val unsafe_html : el:('msg t list -> 'msg t) -> Unsafe_html.t -> 'msg t
  (** [unsafe_html ~el h] is the element [el []] with the content [h], which the
      diff writes with [innerHTML]. The diff compares the HTML text of [h]: if
      it does not change, the diff writes nothing. Thus the selection, the focus
      and the scroll inside the content stay.

      @raise Invalid_argument as {!keyed}. *)
end

val txt : string -> 'msg View.t
(** [txt] is {!View.txt}. *)

(** Attributes and controlled properties (design sections 08 and 13).

    An element joins the values of all of its [class] attributes, in order, with
    one space, and drops the empty values. For each other name, a duplicate
    raises [Invalid_argument] when the code builds the element. The names
    compare with the ASCII case ignored. *)
module At : sig
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

      @raise Invalid_argument
        if [name] is not an ASCII letter or [_], then letters, digits, [_], [-]
        or [.] (thus a name with [:] is refused: use [href] in SVG), if [name]
        starts with [on] or is [srcdoc], or if a URL has a refused scheme. *)

  val none : t
  (** [none] adds nothing, for example
      [if editable then At.none else At.readonly]. *)

  val bool : string -> t
  (** [bool name] is [v name ""], for example [disabled]. *)

  val data : string -> string -> t
  (** [data name value] is [v ("data-" ^ name) value].

      @raise Invalid_argument as {!v}, and if [name] is empty. *)

  val aria : string -> string -> t
  (** [aria name value] is [v ("aria-" ^ name) value].

      @raise Invalid_argument as {!v}, and if [name] is empty. *)

  val class_ : string -> t

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
    element can have one handler for each DOM event name, thus not {!click} and
    {!check} together. *)
module Ev : sig
  type 'msg t = 'msg Private.View.handler

  val click : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t

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

  val keydown :
    ?prevent_default:bool -> ?stop:bool -> (string -> 'msg) -> 'msg t
  (** [keydown f] gives [f] the [key] of the event. *)

  val keyup : ?prevent_default:bool -> ?stop:bool -> (string -> 'msg) -> 'msg t

  val focus : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [focus] uses [focusin], which bubbles. *)

  val blur : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
  (** [blur] uses [focusout], which bubbles. *)

  (** {1 Events that do not bubble}

      These events use a listener on their element, not delegation at the root.
      The listener is passive if the handler has no [~prevent_default]. *)

  val mouseenter : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t

  val mouseleave : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t

  val scroll : ?prevent_default:bool -> ?stop:bool -> 'msg -> 'msg t
end

(** Elements (design sections 08 and 13).

    A function of this module gives an element of the allowlist of HTML and SVG
    elements. The list does not contain [script], [iframe], [object], [embed],
    [base], [meta], [link], and the SVG animation elements [set], [animate],
    [animateMotion] and [animateTransform]. A camelCase SVG name becomes a
    snake_case function name, for example [linear_gradient] for
    [linearGradient].

    The namespace of an element comes from its place: [svg] starts the SVG
    namespace, and the children of [foreign_object] are HTML. Thus {!a},
    {!title} and {!style} in an [svg] element are SVG elements.

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

      @raise Invalid_argument
        if [name] (with the ASCII case ignored) is not on the allowlist, or if
        [name] is a void element and the children are not empty. *)

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
  type 'msg t = 'msg Private.Cmd.t

  val none : 'msg t

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

(** An application: the root instance. *)
module App : sig
  type t = Private.App.t

  val make :
       init:(unit -> 'model * 'msg Cmd.t)
    -> update:('model -> 'msg -> 'model * 'msg Cmd.t)
    -> view:('model -> 'msg View.t)
    -> ?fallback:(exn -> 'msg View.t)
    -> unit
    -> t
  (** An error in [init], [update], [view], a task, a message function or a DOM
      operation of the app unmounts the app: its tasks stop. Then the root shows
      [fallback exn] (default: an empty text). The messages of the fallback do
      nothing. The error goes to the error handler. An error in [fallback] goes
      to the error handler, and the root then shows an empty text. *)
end
