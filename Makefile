.PHONY: build fmt test

build:
	dune build

fmt:
	dune fmt

# The tests run in the three builds: native, js_of_ocaml and wasm_of_ocaml.
# The two browser builds run on node.
test:
	dune test --force
