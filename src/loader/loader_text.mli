(** The loader text and its CSP hash. gen/embed.ml generates the module from
    loader.js and loader.csp. *)

val text : string
(** The loader text: the same bytes in each part of each page, and in the
    loader.js file. It is ASCII, and it has no "//", "/*", "</" or "<!--". *)

val csp : string
(** The CSP source expression of {!text}, for example ['sha256-AAAA...='] with
    the quotes. *)
