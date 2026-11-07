(** [kilim dist]: the release files (docs/design.html, "The loader"). It runs
    after [dune build --profile release]. It copies the builds into a directory
    with content names, and it fills the template of the page. *)

(** The builds: the path of the launcher of wasm_of_ocaml (its [.assets]
    directory is next to it), or the js_of_ocaml build only. *)
type build = Wasm of string | Js

type t =
  { base: string
  ; js: string  (** The path of the js_of_ocaml build. *)
  ; build: build
  ; file_form: bool  (** The loader.js form, else the inline form. *)
  ; name: string option  (** The name of the markers. *)
  ; template: string  (** The path of the template. *)
  ; page: string option  (** The path of the page. Default: [dir/index.html]. *)
  ; dir: string }

val run : t -> (unit, string) result
(** [run t] writes into [t.dir] (made if it does not exist, but not its parent):
    - The js_of_ocaml build, as [Names.content].
    - With [Wasm], the launcher as [Names.content], and each file of its
      [.assets] directory into a directory with the same name. The launcher
      finds its files next to its own URL. The directory must have exactly one
      [.wasm] file (a release build): it is the main file that the loader
      preloads.
    - In the loader.js form, the loader text as [loader.<hash>.js].

    A file that exists with the same bytes stays. A file that exists with other
    bytes is an error, and [run] then writes no file. [run] never removes a
    file: the files of the last deploy stay. Then it writes the page each time.
    The error is the text of a fault of a file, a conflict or a marker. *)
