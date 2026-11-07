(** The template of a page (docs/design.html, "The loader"). The user gives an
    HTML page with two markers: [<!--kilim:head-->] directly after
    [<meta charset>] and each CSP [<meta>], and [<!--kilim:start-->] as the last
    element of the body. With a name [ID], the markers are
    [<!--kilim:head:ID-->] and [<!--kilim:start:ID-->]: thus a page can have two
    apps. *)

val markers : string option -> string * string
(** [markers name] is the head marker and the start marker of [name]. *)

val fill :
     name:string option
  -> head:string
  -> start:string
  -> string
  -> (string, string) result
(** [fill ~name ~head ~start page] replaces the head marker with [head] and the
    start marker with [start]. The other bytes of [page] do not change. The
    error tells which marker does not occur exactly one time, and its count. *)
