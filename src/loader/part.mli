(** The HTML of the two parts of the loader (docs/design.html, "The loader").
    The values go into [data-] attributes, in a fixed order. The loader text is
    {!Loader_text.text}, with no change. *)

(** The builds of the app. [Wasm] has the name of the launcher of wasm_of_ocaml
    and, for a release, the name of its main [.wasm] file (the loader preloads
    it). [Js] is the js_of_ocaml build only: the loader runs no Wasm probe
    ([data-build="js"]). *)
type build = Wasm of {launcher: string; main: string option} | Js

(** [Inline]: the loader text in the two script elements. [File loader]: two
    script elements with the source [base ^ loader]. *)
type form = Inline | File of string

type t =
  { base: string  (** An absolute path or an http(s) URL that ends with "/". *)
  ; js: string  (** The js_of_ocaml build, relative to [base]. *)
  ; build: build
  ; dev: bool  (** The dev profile: [data-dev]. *) }

val escape : string -> string
(** [escape s] is [s] with the ampersand, the double quote, "<" and ">" as
    character references, for the value of an attribute in double quotes. *)

val head : form -> t -> string
(** The HTML at the head marker. In the [File] form, it has the two script
    elements: the [async] head part, then the [defer] start part. *)

val start : form -> t -> string
(** The HTML at the start marker. In the [File] form, it is empty. *)
