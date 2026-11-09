(** The URL check of design section 13, and the functions of Kilim.Url. *)

val permitted : string -> bool
(** [permitted url] is [true] if [url] is relative, or if its scheme is [http],
    [https], [mailto] or [tel]. The check finds the scheme as the WHATWG URL
    parser does: it ignores the C0 control characters and the spaces at the
    start, and all tab, LF and CR characters. Then a scheme is an ASCII letter,
    then letters, digits, [+], [-] or [.], then [:]. The compare ignores case. A
    URL with no scheme is relative. The check does not decode entities, because
    [setAttribute] does not decode them. *)

val attribute_ok : string -> string -> bool
(** [attribute_ok name value] is [true] if the attribute [name] (in lowercase)
    is not a URL attribute, or if each URL of [value] is permitted. The URL
    attributes are [href], [src], [action], [formaction], [poster], [cite],
    [data] and [background] (one URL), [srcset] (the URLs that the WHATWG srcset
    parser finds) and [ping] (a list of URLs that ASCII whitespace separates).
*)

(** {1 Kilim.Url}

    The text of each function is in kilim.mli. *)

val encode_component : string -> string

val decode_component : string -> string

val query : string -> (string * string) list

val with_query : string -> (string * string) list -> string

type location =
  {path: string list; query: (string * string) list; fragment: string option}

val parse : string -> location

val to_string : location -> string
