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

kilim has the parts of Phase 1 of the design: the view, the keyed diff, the
app, the commands, the subscriptions, the async tasks, the browser host, the
loader and `kilim.testing`. The browser tests run in Chrome, Firefox and
WebKit. kilim also has the parts of Phase 2: components with props, outputs
and subscriptions (`Kilim.Component`), error boundaries (`View.boundary`),
shared state (`Kilim.Context`) and the test of one component
(`Kilim_testing.mount`). Phase 4 is complete: it adds `kilim.http` (HTTP
requests and JSON), `kilim.router` (routes in the URL), `kilim.websocket`
(a WebSocket as a subscription) and `kilim.storage` (localStorage and
sessionStorage), each with a mock in `kilim.testing`. There is no release
yet, and the API can change.

The design is in [docs/design.html](docs/design.html). The results of the
spikes are in [docs/phase0.md](docs/phase0.md), and the budgets are in
[docs/budgets.md](docs/budgets.md).

## Getting started

### Run the examples

The examples are in [examples](examples): a counter, a todo list, an app
with components, a search box with the query in the URL, and a chat. Build
them, then serve the dev files of an example:

```sh
nix develop
dune build
node examples/serve.mjs _build/default/examples/todo
```

Then open `http://127.0.0.1:8000/`. `examples/serve.mjs` serves the files
with `cache-control: no-cache`, as [docs/loader.md](docs/loader.md) tells.
It also gives the API of the search example: `/api/cities?q=TEXT` gives the
cities whose names start with `TEXT`, and `&delay=MS` sends the answer MS ms
later. For the chat example, `/chat` is a WebSocket room, `/api/history`
gives the last 20 messages, and `/chat/drop` closes each socket of the
room. `examples/ws.mjs` is the WebSocket server. It uses only the modules of
node.

### An app

An app is a model, messages, a pure `update` and a `view`. This is
`examples/counter/counter.ml`:

```ocaml file=examples/counter/counter.ml
(* The counter example: the model is an int. The messages change it, and the
   view shows it between two buttons. *)

open Kilim

type msg = Inc | Dec | Reset

let update n = function
  | Inc ->
      (n + 1, Cmd.none)
  | Dec ->
      (n - 1, Cmd.none)
  | Reset ->
      (0, Cmd.none)

let view n =
  El.div
    [ El.button ~at:[At.aria_label "Decrement"] ~ev:[Ev.click Dec] [txt "-"]
    ; El.output ~at:[At.data "testid" "count"] [txt (string_of_int n)]
    ; El.button ~at:[At.aria_label "Increment"] ~ev:[Ev.click Inc] [txt "+"]
    ; El.button ~ev:[Ev.click Reset] [txt "Reset"] ]

let app = App.make ~init:(fun () -> (0, Cmd.none)) ~update ~view ()
```

`main.ml` starts the app in the element with the id `app`:

```ocaml file=examples/counter/main.ml
let () = Kilim_browser.run ~mount:"app" Counter.app
```

The template `page.html` has the two markers of the loader:

```html file=examples/counter/page.html
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<!--kilim:head-->
<title>kilim counter</title>
</head>
<body>
<div id="app"></div>
<!--kilim:start-->
</body>
</html>
```

The `dune` file builds the app with js_of_ocaml and wasm_of_ocaml, and the
dev page `index.html` with the rule of the loader:

```dune in=examples/counter/dune
(library
 (name counter)
 (modules counter)
 (libraries kilim))

(executable
 (name main)
 (modules main)
 (modes js wasm)
 (libraries counter kilim.browser))

(rule
 (targets index.html)
 (deps page.html main.bc.js main.bc.wasm.js (glob_files main.bc.wasm.assets/*))
 (action
  (run kilim page --template page.html --base / --js main.bc.js
       --wasm main.bc.wasm.js -o index.html)))
```

[docs/loader.md](docs/loader.md) tells how the loader picks a build, and
how to write the CSP.

### A test

The app has no browser dependency, thus its test runs native, with the fake
DOM and the virtual clock of `kilim.testing`. The test finds the elements by
label, text and test id, and sends the events, as a user does. This is
`examples/counter/test/test_counter.ml`:

```ocaml file=examples/counter/test/test_counter.ml
(* The native test of the counter example. The test drives the app only through
   the queries and the events of Page, as a user test does. *)

open Windtrap
module Page = Kilim_testing.Page
module By = Kilim_testing.By
module Host = Kilim_testing.Host
module Fake_dom = Kilim_testing.Fake_dom

let count p = Fake_dom.text (Page.find p (By.test_id "count"))

let no_errors p =
  equal int ~msg:"no error" 0 (List.length (Host.errors (Page.host p))) ;
  equal (list string) ~msg:"no warning" [] (Host.warnings (Page.host p))

let clicks () =
  let p = Page.start Counter.app in
  for _ = 1 to 3 do
    Page.click p (By.label "Increment")
  done ;
  Page.click p (By.label "Decrement") ;
  equal string "2" (count p) ;
  Page.click p (By.text "Reset") ;
  equal string "0" (count p) ;
  Page.stop p ;
  Page.expect_no_tasks p ;
  no_errors p

let first_render () =
  let p = Page.start Counter.app in
  equal string
    "<div><div><button aria-label=\"Decrement\">-</button><output \
     data-testid=\"count\">0</output><button \
     aria-label=\"Increment\">+</button><button>Reset</button></div></div>"
    (Page.html p) ;
  no_errors p

let () =
  exit
    (Windtrap.run "counter"
       [ test "the buttons change the count" clicks
       ; test "the first render" first_render ] )
```

The test stanza:

```dune in=examples/counter/test/dune
(test
 (name test_counter)
 (libraries counter kilim.testing windtrap))
```

`dune test` runs the tests.

### A component

A component is a module with its own model, messages, tasks and
subscriptions. The parent gives it props and gets its outputs. This counter
of `examples/components/components.ml` gives its new count to the parent:

```ocaml in=examples/components/components.ml
module Counter = struct
  include Component.Defaults

  type props = {label: string; step: int}

  type model = int

  type msg = Incr | Decr

  type out = Changed of int

  let id : (props, model, msg, out) Component.id = Component.id ()

  let init _ = (0, Cmd.none)

  let update ctx p n msg =
    let n = match msg with Incr -> n + p.step | Decr -> n - p.step in
    (n, Ctx.emit ctx (Changed n))

  let view p n =
    Context.read theme (fun t ->
        El.div
          ~at:[At.classes ["counter"; theme_class t]]
          [ El.span [txt p.label]
          ; El.button
              ~at:[At.aria_label ("Decrement " ^ p.label)]
              ~ev:[Ev.click Decr]
              [txt "-"]
          ; El.output
              ~at:[At.aria_label (p.label ^ " count")]
              [txt (string_of_int n)]
          ; El.button
              ~at:[At.aria_label ("Increment " ^ p.label)]
              ~ev:[Ev.click Incr]
              [txt "+"] ] )
end
```

`include Component.Defaults` gives the default `subs` and `equal_props`.
`Component.id ()` gives the identity of the component.

`Component.v` puts an instance in the view of the parent. `~on` makes a
parent message from an output. A new `~key` resets the instance:

```ocaml in=examples/components/components.ml
Component.v
  (module Counter)
  ~key:(key "left")
  ~on:(fun (Counter.Changed n) -> Left n)
  {Counter.label= "Ones"; step= 1}
```

`Kilim_testing.mount` tests one component, and `Page.outputs` gives its
outputs:

```ocaml in=examples/components/test/test_components.ml
  Kilim_testing.mount
    (module Components.Counter)
    {Components.Counter.label= "n"; step= 2}
    (fun p ->
      Page.click p (By.label "Increment n") ;
      Page.click p (By.label "Increment n") ;
      Page.click p (By.label "Decrement n") ;
      equal string "2" (Fake_dom.text (Page.find p (By.label "n count"))) ;
      is_true ~msg:"the outputs"
        (Page.outputs p = Components.Counter.[Changed 2; Changed 4; Changed 2]) )
```

The example also has a filter box with a debounce (`Cmd.latest` and
`Async.sleep`), a theme context (`Kilim.Context`) and an error boundary
(`View.boundary`).

### HTTP

`kilim.http` sends requests as tasks. The search box of
`examples/search/search.ml` waits 250 ms after each key press, then gets the
cities as JSON and decodes them. `Cmd.latest` cancels the task before it: its
timer stops, or the browser aborts its request.

```ocaml in=examples/search/search.ml
  let fetch p q =
    Cmd.latest ~key:"fetch"
      (let* () = Async.sleep (Span.ms 250) in
       let+ r =
         Http.get_json
           Http.Decode.(list string)
           (Url.with_query p.endpoint [("q", q)])
       in
       Got (q, r) )
```

An HTTP failure is an `Http.error` in the result, not an exception.
`main.ml` installs the browser parts of the libraries before the start:

```ocaml file=examples/search/main.ml
let () =
  Kilim_http_browser.install () ;
  Kilim_router_browser.install () ;
  Kilim_browser.run ~mount:"app" Search.app
```

In a test, `Http_mock` gives the replies on the virtual clock:

```ocaml in=examples/search/test/test_search.ml
    Http_mock.on_get page "/api/cities?q=par" (`Json {|["Paris"]|});
    Page.type_ page (By.label "City") "par";
    Page.advance page (Span.ms 300);           (* virtual clock: no real wait *)
```

### Routing

`kilim.router` keeps a route of the app in the path and the query of the URL.
The app gives `parse` and `print`. In the search example, the route is the
query:

```ocaml in=examples/search/search.ml
let router =
  Router.make
    ~parse:(fun (l : Url.location) ->
      Option.value ~default:"" (List.assoc_opt "q" l.query) )
    ~print:(fun q ->
      {Url.path= []; query= (if q = "" then [] else [("q", q)]); fragment= None} )
    ()
```

The example has `module Router = Kilim_router`. `Router.current` gives the
route at the start: thus a deep link (`/?q=par`) loads its results. After
each result, the box writes the query with `Router.replace`. `Router.link`
makes a link, and `Router.push` adds its URL to the history.
`Router.changes` gives each change of the URL. The back button also makes a
change:

```ocaml in=examples/search/search.ml
  let subs _ _ = Router.changes router (fun q -> Url q)
```

In a test, `History_mock` gives the URL of the page and the back button.
This test starts at a deep link:

```ocaml in=examples/search/test/test_search.ml
  let host = Host.create () in
  History_mock.set_url host "/?q=par" ;
  Kilim_testing.mount ~host
    (module Search)
    {Search.endpoint= "/api/cities"}
    (fun p ->
      equal string "par" (value p) ;
      Http_mock.on_get p "/api/cities?q=par" (`Json {|["Paris","Parma"]|}) ;
      Page.advance p (Span.ms 300) ;
      equal (list string) ["Paris"; "Parma"] (cities p) ;
```

### Storage

`kilim.storage` reads and writes localStorage and sessionStorage with tasks.
The result is synchronous: the chat example reads the nickname in an init
task, thus the first render shows it.

```ocaml in=examples/chat/chat.ml
  , Cmd.task (Async.map (fun r -> Loaded r) (Storage.get Local "nick")) )
```

An error is a `Storage.error` in the result, for example `Unavailable` when
the user blocks the storage of the site. `main.ml` calls
`Kilim_storage_browser.install ()` (see the next section).

### WebSocket

`kilim.websocket` gives a socket as a subscription. The key comes from the
URL and `~id`. `Opened` gives the connection, and `Ws.send` sends text on
it:

```ocaml in=examples/chat/chat.ml
let subs m = Ws.connect ~id:(string_of_int m.id) "/chat" (fun e -> Socket e)
```

The library does not connect again by itself. On `Closed`, the chat example
waits (1 s, 2 s, 4 s, at most 30 s), then gives a new `~id`:

```ocaml in=examples/chat/chat.ml
  | Socket (Closed _) ->
      let s = backoff m.attempt in
      ( {m with status= Waiting s; attempt= m.attempt + 1}
      , Cmd.task
          (Async.map (fun () -> Retry) (Async.sleep (Span.ms (s * 1000)))) )
```

`main.ml` installs the three browser parts:

```ocaml file=examples/chat/main.ml
let () =
  Kilim_http_browser.install () ;
  Kilim_websocket_browser.install () ;
  Kilim_storage_browser.install () ;
  Kilim_browser.run ~mount:"app" Chat.app
```

### Test with mocks

Each page of `kilim.testing` has a mock of each library: `Http_mock`,
`History_mock`, `Websocket_mock` and `Storage_mock`. The events of a mock
come on the virtual clock. The chat test opens the room and gives the reply
of `/api/history`:

```ocaml in=examples/chat/test/test_chat.ml
let open_room p history =
  Http_mock.on_get p ~once:true "/api/history" (`Json history) ;
  Websocket_mock.accept (Websocket_mock.server p "/chat") ;
  frame p
```

`Websocket_mock.drop` closes the socket from the server. The test then
checks the wait at exact times on the virtual clock (`at p t ms` runs the
host until `ms` after the drop):

```ocaml in=examples/chat/test/test_chat.ml
      let t = drop () in
      frame p ;
      equal string "Disconnected: a new try in 1 s" (status p) ;
      at p t 999 ;
      equal int ~msg:"999 ms" 0 (Websocket_mock.connections room) ;
      at p t 1016 ;
      equal int ~msg:"1 s and a frame" 1 (Websocket_mock.connections room) ;
```

A value in the storage before the mount (`History_mock` is in the section
"Routing"):

```ocaml in=examples/chat/test/test_chat.ml
  let host = Host.create () in
  Storage_mock.preset host Local [("nick", "ann")] ;
  with_page ~host (fun p ->
      equal string ~msg:"the restore" "ann" (nick p) ;
```

### A release

Build in the release profile, then make the release files with
`kilim dist`. In this repository, the executable is
`_build/install/default/bin/kilim`. A project that installs kilim with opam
runs `kilim`.

```sh
dune build --profile release
_build/install/default/bin/kilim dist --template examples/counter/page.html \
  --base /static/ --js _build/default/examples/counter/main.bc.js \
  --wasm _build/default/examples/counter/main.bc.wasm.js -o dist
node examples/serve.mjs --base /static/ dist
```

The build files get content names, and `examples/serve.mjs` serves them as
immutable files. The page is `dist/index.html`: open
`http://127.0.0.1:8000/` again.

## Development

The development environment is a Nix flake:

```sh
nix develop
make build
make test
```

`make test` runs the browser tests in Chrome (the Chrome of the system).
The same tests in Firefox and WebKit use Playwright, from the second dev
shell `browsers` (Linux only):

```sh
nix develop .#browsers
make browsers
```

This shell adds Playwright, Firefox, WebKit and the EGL driver of mesa to
the default shell: a download of about 680 MiB. On another system, `make
browsers` writes "SKIP: no Playwright".

## License

MPL-2.0. See [LICENSE](LICENSE).
