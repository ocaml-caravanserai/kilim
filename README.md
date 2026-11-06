# kilim

kilim is a browser application library for OCaml 5.5. It is a part of the
[ocaml-caravanserai](https://github.com/ocaml-caravanserai) organization.

kilim has three main ideas:

- The Elm architecture: a model, typed messages, a pure `update` and a `view`.
- Encapsulated components. A component keeps its own model, messages, tasks
  and subscriptions. The parent sees only typed props and typed outputs.
- No OCaml effects. Async tasks are values with cancel tokens. An unmount
  cancels all tasks of a component.

kilim builds with js_of_ocaml and wasm_of_ocaml in their default modes.

## Status

This repository is in Phase 0: spikes and budget targets. No library is ready
for use at this time.

The design is in [docs/design.html](docs/design.html). The results of the spikes are in [docs/phase0.md](docs/phase0.md).

## Development

The development environment is a Nix flake:

```sh
nix develop
make build
make test
```

## License

MPL-2.0. See [LICENSE](LICENSE).
