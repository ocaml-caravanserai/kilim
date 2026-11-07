(** The name checks of design section 13. Each check takes a name in lowercase.
*)

val all : string list
(** The allowlist of HTML and SVG elements, with the usual spelling of each
    name. The list does not contain [script], [iframe], [object], [embed],
    [base], [meta], [link], and the SVG animation elements [set], [animate],
    [animateMotion] and [animateTransform]. *)

val element : string -> string option
(** [element key] is the usual spelling of the element [key] of {!all}, for
    example [Some "foreignObject"] for ["foreignobject"], or [None] if [key] is
    not on the allowlist. *)

val is_void : string -> bool
(** [is_void name] is [true] for an HTML element with no children: [area], [br],
    [col], [hr], [img], [input], [source], [track] and [wbr]. *)

val attribute_ok : string -> bool
(** [attribute_ok name] is [true] if [name] is a valid attribute name and its
    value does not run as script. A valid name is a lowercase ASCII letter or
    [_], then these letters, digits, [_], [-] or [.]. Thus a name with [:] is
    refused: use [href] in SVG, not [xlink:href]. The check refuses the names
    that start with [on] and [srcdoc]. *)
