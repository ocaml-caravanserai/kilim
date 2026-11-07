(** The content names of the release files (docs/design.html, "The loader"). *)

val hash : string -> string
(** [hash bytes] is the first 16 hexadecimal characters of the BLAKE2b-128
    digest of [bytes] (64 bits: enough for the files of one site). *)

val content : string -> string -> string
(** [content name bytes] puts the hash of [bytes] before the last extension of
    [name]: [content "main.bc.js" b] is ["main.bc.<hash>.js"]. A name with no
    extension gets the hash as its extension. *)
