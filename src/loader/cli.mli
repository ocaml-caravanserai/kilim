(** The commands of the executable [kilim] (docs/loader.md):

    {v
    kilim page --template FILE -o FILE --base BASE --js NAME
               (--wasm NAME | --build js) [--form inline|file]
               [--loader NAME] [--name ID]
    kilim loader -o FILE
    kilim csp
    kilim dist --template FILE --base BASE --js PATH
               (--wasm PATH | --build js) [--form inline|file]
               [--name ID] [--page FILE] -o DIR
    v}

    [page] writes a page for the dev profile: fixed names and [data-dev].
    [loader] writes the loader text (the loader.js form). [csp] writes the CSP
    source expression of the loader text. [dist] writes the release files
    ({!Dist.run}). *)

(** [Usage]: an argument is not valid. [Fault]: a fault of a file, a conflict or
    a marker. *)
type error = Usage of string | Fault of string

val run : string array -> (string, error) result
(** [run argv] runs the command of [argv] ([argv.(0)] is the name of the
    program). The result is the text for the standard output. The checks of the
    arguments:
    - A base starts with "/" (but not "//"), "http://" or "https://" with a
      host, and ends with "/".
    - A name of [page] is not empty, does not start with "/", and has no ".."
      segment.
    - An [ID] has only the characters [a-z], [0-9] and "-", and at least one.
    - [--wasm] and [--build js] are not permitted together, and one of them is
      necessary. *)

val exit_code : (string, error) result -> int
(** 0 for [Ok], 1 for [Fault], 2 for [Usage]. *)
