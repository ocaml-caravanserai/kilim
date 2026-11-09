## 0.1.0 (date TBD)

The first release.

The libraries and the executable:

- `kilim`: the Elm architecture (a model, typed messages, a pure `update`
  and a `view`), components with their own model, messages, tasks and
  subscriptions, async tasks with cancel tokens, and no OCaml effects.
- `kilim.browser`: the browser host and `Kilim_browser.run`.
- `kilim.testing`: a fake DOM that counts each operation, a host with a
  virtual clock, pages with simulated events, and the mocks of the other
  libraries, for native tests.
- `kilim.http` and `kilim.http.browser`: HTTP requests as tasks, JSON and
  decoders, with fetch.
- `kilim.router` and `kilim.router.browser`: the routes of an app in the
  path of the URL, with the History API.
- `kilim.websocket` and `kilim.websocket.browser`: a WebSocket as a
  subscription.
- `kilim.storage` and `kilim.storage.browser`: localStorage and
  sessionStorage as tasks.
- `kilim.web`: the bindings for the Web APIs that Brr does not have.
- The executable `kilim`: the page and the loader of an app, the CSP hash
  of the loader, and the release files with content names.

The supported versions:

- OCaml 5.5.
- js_of_ocaml and wasm_of_ocaml 6.4 and 6.5, in their default modes.
- Chrome, and Firefox and WebKit through Playwright.

The known limitations:

- Nobody checked kilim in Safari. The tests use Playwright WebKit. If your
  users have Safari, use the inline form of the loader (docs/loader.md).
- The CSP hash of the loader can change in a 0.x version. Take it from
  `kilim csp` after each update of kilim.
- wasm_of_ocaml 6.4.1 emits the legacy Wasm exception handling (`try`).
  Firefox 157 marks it as deprecated. If a later Firefox removes it, the
  wasm_of_ocaml build breaks in Firefox after the probe of the loader
  passes, with no fallback to the js_of_ocaml build.
- The JSON parser is OCaml code. It is about 5 to 6 times slower than
  `JSON.parse` (docs/budgets.md).
- The timing budgets wait for fixed hardware. CI checks the size, the
  allocations, the DOM operations and the leaks.
- Some items come after version 1 (the "Later" list of the design): the
  Resize, Intersection and Mutation observers, the hash mode of the router,
  the `storage` event, binary WebSocket frames, element handles, server
  rendering and devtools.
