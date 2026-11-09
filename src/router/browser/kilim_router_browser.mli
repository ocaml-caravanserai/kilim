(** kilim.router.browser: the history driver of kilim.router on the History API
    of the browser (design section 06).

    {@ocaml in=test/doc/install_router.ml[
    let () =
      Kilim_router_browser.install () ;
      Kilim_browser.run ~mount:"app" App.app
    ]} *)

val install : unit -> unit
(** [install ()] makes the driver of this module the history driver of
    kilim.router. A second call does nothing. The driver:
    - [location] reads [pathname], [search] and [hash] of [location].
    - [push] calls [history.pushState] with a null state and the URL that it
      resolves against the URL of the document ([location.href]). Then it
      scrolls the window to the top with [Kilim_web.Scroll]. [replace] calls
      [history.replaceState] in the same way, with no scroll.
    - If the browser refuses the URL (for example a SecurityError for a URL of
      another origin), [push] and [replace] write a warning to the console, and
      the URL and the history do not change.
    - [back] calls [history.back].
    - [listen] adds a [popstate] listener on [window], and gives the function
      that removes it.

    The history state stays null: an OCaml value does not survive the structured
    clone of the browser. The scroll restoration of the browser stays ["auto"].
*)
