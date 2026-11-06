(lang dune 3.17)

(name kilim)

(generate_opam_files true)

(source
 (github ocaml-caravanserai/kilim))

(authors "Bikal Gurung <gbikal@gmail.com>")

(maintainers "Bikal Gurung <gbikal@gmail.com>")

(license MPL-2.0)

(package
 (name kilim)
 (synopsis
  "A browser application library for OCaml: the Elm architecture with encapsulated components")
 (description
  "kilim is a browser application library for OCaml 5.5. It uses the Elm architecture: a model, typed messages, a pure update and a view. Components keep their own model, messages, tasks and subscriptions. Async tasks are values with cancel tokens, and an unmount cancels all tasks of a component. kilim performs no OCaml effects. It builds with js_of_ocaml and wasm_of_ocaml in their default modes.")
 (depends
  (ocaml
   (>= 5.5.0))
  (brr
   (>= 0.0.8))
  (js_of_ocaml-compiler
   (>= 6.4))
  (wasm_of_ocaml-compiler
   (and
    :with-test
    (>= 6.4)))
  (windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (odoc :with-doc)))
