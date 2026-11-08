(** Trusted Types: [trustedTypes] and [isHTML].

    Web API: [globalThis.trustedTypes] and [trustedTypes.isHTML(v)] of the
    Trusted Types specification. Brr 0.0.9 has no binding for them.
    [Kilim_browser.Unsafe_html.of_trusted] uses them. *)

val exists : unit -> bool
(** [exists ()] is true if [globalThis.trustedTypes] is not [null] or
    [undefined]. Each call reads the global again. *)

val is_html : Jv.t -> bool
(** [is_html v] is true if [trustedTypes.isHTML(v)] gives [true]: [v] is a
    [TrustedHTML] value. If [trustedTypes.isHTML] is not a function (no
    [trustedTypes], or an element with the id [trustedTypes] in a browser with
    no Trusted Types), [is_html v] is false and calls nothing. *)
