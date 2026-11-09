(** kilim.storage.browser: the driver of kilim.storage on the Web Storage API.

    {@ocaml in=test/doc/install_storage.ml[
    let () =
      Kilim_storage_browser.install () ;
      Kilim_browser.run ~mount:"app" app
    ]} *)

val install : unit -> unit
(** [install ()] makes the driver of this module the driver of kilim.storage. A
    second call does nothing. Each call of the driver reads
    [window.localStorage] or [window.sessionStorage] again, then calls
    [getItem], [setItem] or [removeItem]. A value of [getItem] that is not a
    string gives [None].

    A storage object that is [null] or [undefined] (for example in a WebView
    with no storage) gives [Unavailable]. The driver catches each JS error, also
    the error of the getter of [localStorage], and maps its name:
    [SecurityError] gives [Unavailable], [QuotaExceededError] (and
    [NS_ERROR_DOM_QUOTA_REACHED] of older Firefox versions) gives
    [Quota_exceeded], and another name gives [Other name]. *)
