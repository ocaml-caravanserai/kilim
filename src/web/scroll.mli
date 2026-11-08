(** [window.scrollTo].

    Web API: [window.scrollTo(options)] of CSSOM View. Brr 0.0.9 has no binding
    for it: [Brr.Window.scroll_x] and [scroll_y] only read the position.
    kilim.router.browser scrolls to the top after a push with it. *)

val to_ : ?smooth:bool -> left:float -> top:float -> unit -> unit
(** [to_ ~smooth ~left ~top ()] calls [window.scrollTo({left, top, behavior})].
    [left] and [top] are in CSS pixels, from the start of the document. The
    behavior is ["smooth"] if [smooth] is [true], else ["auto"] (default): then
    the CSS property [scroll-behavior] of the root element decides, and with no
    such property the position changes at once. The browser limits the position
    to the scroll range of the document. *)
