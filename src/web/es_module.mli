(** Dynamic [import()].

    Web API: the [import(specifier)] expression of ECMAScript. Brr 0.0.9 has no
    binding for it, and OCaml code cannot write it: this module makes it with a
    JS expression that the compilers put in the output. No [eval] runs, thus a
    page with a strict CSP accepts it. Use it in a task to load a library that
    is only an ES module ([Kilim_browser.Async.of_fut_or_error]). *)

val import : Jstr.t -> Jv.t Fut.or_error
(** [import spec] is [import(url)]: the namespace object of the module, or the
    JS error of the load (a [TypeError] for a missing module in a browser).

    If [spec] starts with [./], [../] or [/], [url] is [spec] resolved against
    [document.baseURI]. Thus the js_of_ocaml script and the wasm_of_ocaml
    launcher give the same URL, which does not depend on the URL of the script.
    Other specifiers, for example a bare name for an import map or a full URL,
    stay as they are. With no [document] (node), [url] is [spec]. If the
    resolution throws a JS exception, the future gives that error. *)
