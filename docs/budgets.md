# Budgets

The budgets of kilim (section 16 of [design.html](design.html)). `make
budgets` is the budget gate. CI runs it on each push to main and on each
pull request.

## The budgets and their checks

| Budget | Check in CI | Where | State |
|---|---|---|---|
| Bundle size (gzip), for each build | Yes, each change | `bench/size`: the counter of `bench/size`, the todo, components, search and chat examples | Done |
| DOM operations for each action | Yes, each change | `test/view/test_budget.ml`: the four actions, exact counts. `examples/todo/test/test_counts.ml`: three actions of the todo example | Done |
| Allocations for each action | Yes, each change | `bench/alloc`: the allocated words of the four actions | Done |
| Leaks after 1,000 mounts | Yes, each change | `test/turn/test_leak.ml` (tokens, timers, keys, instances, child links, the lists of the boundaries and the reader registrations, 100 times), `test/cross` (the same, 1,000 times, in the three builds), tests D25, D40 and D44 of `test/dom` (listeners, timers, DOM, in Chrome, Firefox and WebKit), HT9 of `test/dom` (20 times, with a request in flight), WS6 of `test/dom` (20 times, with an open socket), and the weak tests (native, and `test/weak` in the js_of_ocaml and wasm_of_ocaml builds) | Done |
| Partial update | Yes, each change | `test/view/test_budget.ml`: three actions on 1,000 keyed component rows, exact counts of the views and of the DOM operations. `bench/alloc`: the allocated words of two actions | Done |
| Token release | Yes, each change | `test/view/test_budget.ml`: the operations on the token lists when the parent removes 1,000 and 2,000 component rows | Done |
| Linked modules | Yes, each change | `test/link` (kilim, kilim.browser, kilim.web, kilim.http, kilim.http.browser, kilim.router, kilim.router.browser, kilim.websocket, kilim.websocket.browser, kilim.storage, kilim.storage.browser) | Done |
| Time of the actions, wasm start-up time | No | Each night on fixed hardware | Waits: no fixed hardware yet |

The leak tests and the link check run in the CI step "Test", with the other
tests. `make budgets` runs the size, the allocation, the DOM operation, the
partial update and the token release checks.

## Size

`make size` (and `make budgets`) builds five programs in the release
profile and runs `bench/size/size.mjs`:

- `counter`: `bench/size/counter.ml`, one int model, two buttons, one text,
  and `Kilim_browser.run`. This program is the fixed reference of the size
  budget. The open reference sizes compare with it.
- `example-todo`: the todo example (`examples/todo`), a real app.
- `example-components`: the components example (`examples/components`).
  It is the only program of the budget that links the code of the
  components, the boundaries and the contexts. The dead code elimination
  removes this code from the counter and the todo example. Without this
  program, the budget cannot find a regression in this code.
- `example-search`: the search example (`examples/search`), with
  `kilim.http` and `kilim.router`, and their browser parts.
- `example-chat`: the chat example (`examples/chat`), with `kilim.http`,
  `kilim.websocket` and `kilim.storage`, and their browser parts.

The script gives the gzip size (level 9, the `zlib` module of node) of each
file, and compares it with `bench/size/budget`. Each line of the budget file
is `PROGRAM FILE BYTES`. A larger file is a failure. A budget line with no
measured file, and a measured file with no budget line, are failures too.
`kilim dist` copies the build files with no change, thus these sizes are also
the sizes of the release files.

The budget is the measured size plus 5 %, rounded up to 500 bytes. When a
change of kilim makes a file larger, examine the linked modules first. Change
the budget only for a known cost, and write the cost here.

Measured with OCaml 5.5.1, js_of_ocaml and wasm_of_ocaml 6.4.1, Brr 0.0.9:

| Program | File | Measured | Budget |
|---|---|---|---|
| `counter` | `counter.bc.js` (js_of_ocaml) | 37,753 bytes | 38,500 bytes |
| `counter` | The main `.wasm` file (wasm_of_ocaml) | 37,802 bytes | 38,300 bytes |
| `counter` | `counter.bc.wasm.js` (the launcher) | 6,941 bytes | 7,500 bytes |
| `example-todo` | `main.bc.js` (js_of_ocaml) | 39,570 bytes | 41,000 bytes |
| `example-todo` | The main `.wasm` file (wasm_of_ocaml) | 40,220 bytes | 40,700 bytes |
| `example-todo` | `main.bc.wasm.js` (the launcher) | 6,937 bytes | 7,500 bytes |
| `example-components` | `main.bc.js` (js_of_ocaml) | 41,805 bytes | 44,000 bytes |
| `example-components` | The main `.wasm` file (wasm_of_ocaml) | 43,918 bytes | 46,000 bytes |
| `example-components` | `main.bc.wasm.js` (the launcher) | 6,938 bytes | 7,500 bytes |
| `example-search` | `main.bc.js` (js_of_ocaml) | 46,854 bytes | 49,500 bytes |
| `example-search` | The main `.wasm` file (wasm_of_ocaml) | 51,031 bytes | 54,000 bytes |
| `example-search` | `main.bc.wasm.js` (the launcher) | 7,546 bytes | 8,000 bytes |

The components example adds about 4.0 KB (js_of_ocaml) and 6.1 KB
(wasm_of_ocaml) to the counter. This includes the code of the four
components of the example, the instance tree, the boundaries and the
contexts. Its budget is the measured size plus 5 %, as for the other
programs.

The chat example adds about 6.3 KB (js_of_ocaml) and 9.5 KB
(wasm_of_ocaml) to the counter, and its launcher is about 0.6 KB larger: one
App, `kilim.http` with the JSON parser and the fetch transport, and the
drivers of `kilim.websocket` and `kilim.storage`. Its budget is the measured
size plus 5 %, rounded up to 500 bytes.

The search example adds about 9.1 KB (js_of_ocaml) and 13.2 KB
(wasm_of_ocaml) to the counter: one component, `kilim.http` with the JSON
parser and the fetch transport, and `kilim.router` with the history driver.
Its launcher is about 0.6 KB larger than the launcher of the counter: it has
the JS code of more primitives. The check of the names finds
`AbortController` and `pushState` in its builds, and no `localStorage`. The
builds contain `WebSocket`, because `Brr_io` reads `globalThis.WebSocket`
when it starts: thus this name cannot show that an app does not link
`kilim.websocket`.

The instance tree of Phase 2 adds about 0.4 KB (js_of_ocaml) and 0.6 KB
(wasm_of_ocaml) to the counter, which has no component: before it, the
counter was 36,871 bytes (js_of_ocaml) and 36,376 bytes (the main `.wasm`
file). The cost is the code that each app links: the instance record and its
operations, the transparent component node, and the calls of the hooks. The
budget of the main `.wasm` file of the counter went from 37,000 to 38,000
bytes for this known cost. The code that only components need is in
`Reconcile`: the mount, the patch and the render of a component node, the
removal walk, the render of the work queue, and the links of an instance
(`detach` and `enqueue`). `Component.v` installs these functions in the
diff and in `Instance`, thus the dead code elimination removes them from an
app with no component, in the two builds. With an install at the start of
the runtime and the walk in the diff, the counter was 38,110 bytes
(js_of_ocaml) and 38,453 bytes (wasm_of_ocaml).

The subscriptions and the outputs of each instance add about 0.1 KB to the
counter in the two builds: before them, the counter was 37,268 bytes
(js_of_ocaml) and 37,010 bytes (the main `.wasm` file). The cost is in the
runtime, thus each app links it: the early call and the pass at the end of
the render walk the lists of the instances below the root, and a command
can have an output. The budgets do not change. A component with
`Component.Defaults.subs` has no subscription pass: on the fake DOM, a
click on 1 of 1,000 such component rows allocates 353 words in its turn
(with the dispatch of the fake DOM), and 389 words if each row has its own
`subs`.

The boundaries add about 0.1 KB (js_of_ocaml) and 0.3 KB (wasm_of_ocaml) to
the counter, which has no boundary: before them, the counter was 37,400 bytes
(js_of_ocaml) and 37,118 bytes (the main `.wasm` file). The cost is the code
that each app links: the boundary node in the diff, the boundary of each
controlled property, the routing of an error to a boundary in the runtime,
the loop of the render, and two fields of the instance record. The code that
only boundaries need is in `Boundary`: the build and the patch of a boundary
node, the catch inside and outside the diff, the write of a fallback, and the
write of a controlled property in a boundary. The instance record gets its
list of owned boundaries only at its first boundary: a component row with no
boundary allocates 222 words at its mount, against 219 words before the
boundaries (on a null DOM, with no runtime). `View.boundary` installs these functions in the diff, thus the dead
code elimination removes them from an app with no boundary, in the two builds
(a test string in `Boundary` was not in the release files of the counter and
of the todo example). The budgets do not change.

Context adds about 0.1 KB to the counter in the two builds, which has no
context: before it, the counter was 37,522 bytes (js_of_ocaml) and 37,400
bytes (the main `.wasm` file), and after it 37,612 and 37,477 bytes. The
cost is the code that each app links: the provider node and the read in the
diff, the default functions of the hooks, and the check for registrations of
an instance at each render and at its unmount. The code that only contexts
need is in `Context`: the build and the patch of a provider node, the read
and its registration, the marks of the readers, and the unlink of the
registrations. `Context.provide` and `Context.read` install these functions
in the diff and in `Instance`, thus the dead code elimination removes them
from an app with no context, in the two builds (a test string in `Context`
was not in the release files of the counter and of the todo example). The
registrations of each instance and the environment of each context of the
diff add one word each: the mount of the 1,000 keyed component rows of
`bench/alloc` (on a null DOM, with no runtime) allocates 239.4 words for
each row, against 237.4 words before Context. The budgets do not change.

The mouse reads of `Ev.link_click` (Phase 4) add about 0.14 KB
(js_of_ocaml), 0.28 KB (the main `.wasm` file) and 0.05 KB (the launcher)
to each app. Before them, the counter was 37,612 bytes (js_of_ocaml), 37,518
bytes (the main `.wasm` file) and 6,891 bytes (the launcher). The 37,612
bytes include 4 bytes of `Kilim.Url`: the counter of Phase 3 was 37,608
bytes. The other code of `Kilim.Url` costs nothing in an app that does not
use it. The cost of the mouse reads is in the listener of kilim.browser,
thus each app links it: the read of `button`, `altKey`, `ctrlKey`,
`metaKey`, `shiftKey` and `defaultPrevented`, and the filter in
`Event.dispatch`. The dead code elimination cannot remove it, because the
listener reads the payload of each handler. For this known cost, the budget
of the main `.wasm` file went up by 300 bytes: from 38,000 to 38,300 bytes
(the counter, 498 bytes of headroom) and from 40,500 to 40,700 bytes (the
todo example, 480 bytes of headroom).

The baseline is a minimal Brr program, because Brr links `Fun` and thus
`Printf` (design section 13): 23,845 bytes (js_of_ocaml), and 17,217 bytes
for the main `.wasm` file plus 6,482 bytes for the launcher (wasm_of_ocaml).
Thus kilim adds about 12.9 KB (js_of_ocaml) and 18.8 KB (wasm_of_ocaml) to
the counter. The Brr pull request that replaces `Fun.id` would make the baseline
smaller by about 10 KB. The todo example adds about 2 KB to the counter.

The same rule checks the generated code: the release `counter.bc.js` contains
`.insertBefore(`, `.setAttribute(`, `.__kilim` and `.addEventListener(` (the
DOM operations are direct calls), and the launcher contains
`"meth_call_2_insertBefore"` and `"get___kilim"`.

The rule also checks that an app does not pay for a binding that it does
not use. The counter does not use `Unsafe_html.of_trusted`,
`Async.of_fut`, `Kilim_web.Es_module`, kilim.router.browser and
`Kilim_web.Scroll`. Thus its two release builds (the `.js` file, and the
launcher with the main `.wasm` file, read as bytes) do not contain
`trustedTypes`, `isHTML`, `kilimFut` (the property of the JS cell of
`of_fut`), `import(`, `pushState` and `scrollTo`. The positive controls: the same read finds
each name in the same build of `bench/size/control.ml`, a program that uses
these bindings and has no budget, and finds `queueMicrotask` (the binding
`Kilim_web.Microtask`, which the counter uses) in each file of the
counter.

The counter also does not use the fetch transport of `kilim.http.browser`:
its builds do not contain `AbortController`, and the control program
installs the transport.

No program of the budget uses the driver of `kilim.websocket.browser`: the
builds of the counter and of the todo, components and search examples do not
contain `a binary frame`, a part of the text of its warning for a binary
frame, and the control program installs the driver. Each app with `Brr_io`
contains `WebSocket` (`Brr_io` reads `globalThis.WebSocket` when it starts),
thus this name cannot be the check. A short name of the driver, for example
`wasClean`, is not a good check either: the release build of wasm_of_ocaml
keeps it in no run of bytes of the launcher or of the main `.wasm` file
(measured: the control program contained it only in the `.js` file).

The same check for the driver of `kilim.storage.browser`: no build of the
counter and of the todo, components and search examples contains
`localStorage`, and each build of the control program contains it (in the
wasm_of_ocaml build, in the launcher and in the main `.wasm` file).

## kilim.web

kilim.browser calls `queueMicrotask`, `Object.is` and Trusted Types through
`kilim.web` (Phase 3). The counter, the todo example and the components
example change by 4 bytes or less in the `.js` file and in the launcher. The
main `.wasm` file is 32 to 51 bytes larger: before the change, the counter
was 37,612 bytes (js_of_ocaml) and 37,477 bytes (the main `.wasm` file).
wasm_of_ocaml does not inline the call of `Kilim_web.Microtask.queue` into
the browser host. The release `.js` file of the counter has the same code
for `Object.is` as before (the same direct call `.is(a,b)`), thus the model
compare and `Component.shallow_equal` do not get slower or larger under
js_of_ocaml. The budgets do not change.

The known cost of each binding, measured one time with a program that uses
only that binding, against the same program with no binding (gzip -9, the
release profile):

| Binding | `.js` file | Main `.wasm` file | Launcher |
|---|---|---|---|
| `Microtask.queue` | 27 bytes | 68 bytes | 43 bytes |
| `Js_object.is` | 221 bytes | 248 bytes | 42 bytes |
| `Trusted_types.exists` and `is_html` | 75 bytes | 112 bytes | 69 bytes |

The cost of `Js_object.is` includes `CamlinternalLazy`, which each kilim app
links already.

## kilim.http

`kilim.http` is not in the budget programs: a program that does not use it
does not link it. Its cost, measured one time with the counter and a
`get_json` of a list of strings, against the counter (gzip -9, the release
profile):

| Part | `.js` file | Main `.wasm` file | Launcher |
|---|---|---|---|
| `Kilim_http` (`Json`, `Decode`, the tasks), with no transport | 2,579 bytes | 3,338 bytes | 2 bytes |
| With `Kilim_http_browser.install ()` too (total) | 3,816 bytes | 5,681 bytes | 487 bytes |

The JSON parser is OCaml code in all builds (design section 13): one
behavior in the native tests and in the browsers, and no JS exception to
catch. It is slower than `JSON.parse`. A parse of a text of 1,000,001
bytes (an array of small objects with numbers, strings, an escape and a
boolean), the median of 15 runs on node 24:

| Build | `Json.parse` | `JSON.parse` |
|---|---|---|
| js_of_ocaml | 25 to 27 ms | 4.1 ms |
| wasm_of_ocaml | 21 to 23 ms | 4.1 ms |

Thus a parse is about 5 to 6 times slower than `JSON.parse`, and a response
of about 1.5 to 2 MB takes more than 50 ms, the limit of a busy turn
(design section 16). The review of the Phase 4 plan measured about 9 times
slower, with 27 to 30 ms for 1 MB, on another text. The time is in the
parse only: the decoders come after it. If users need larger responses, a
later version can add a parse with `JSON.parse`.

## kilim.websocket

`kilim.websocket` is not in the budget programs. Its cost, measured one time
with the counter and one `connect` subscription, against the counter (gzip
-9, the release profile):

| Part | `.js` file | Main `.wasm` file | Launcher |
|---|---|---|---|
| `Kilim_websocket`, with no driver | 383 bytes | 506 bytes | 1 byte |
| With `Kilim_websocket_browser.install ()` too (total) | 1,264 bytes | 2,146 bytes | 318 bytes |

## DOM operations

`test/view/test_budget.ml` gives the exact count of each DOM operation on
the fake DOM of `kilim.testing`, for the four actions of design section 16:
create 1,000 rows, change 1 row of 1,000, swap 2 rows of 1,000, and remove
all rows of 1,000. A change of a count is a change of the budget.

`examples/todo/test/test_counts.ml` gives the counts of three actions of the
todo example on a list of 100 items. The counts include the reads of the
live properties:

| Action | DOM operations |
|---|---|
| Toggle 1 item | `get_checked` 102, `get_value` 1, `set_attribute` 1, `set_text` 1 |
| Add 1 item (the submit) | `attach` 4, `create_element` 4, `create_text` 2, `get_checked` 101, `get_value` 1, `insert_before` 6, `set_attribute` 3, `set_property` 1, `set_text` 1 |
| Delete 1 item | `get_checked` 99, `get_value` 1, `remove` 1, `set_text` 1 |

## Allocations

`bench/alloc` counts the words that the diff allocates in the four actions
of the DOM operation budget. The count is all the allocated words:
`Gc.counters` gives the minor words, the promoted words and the major words,
and the count is minor plus major, less promoted. A large array goes
directly to the major heap, thus the minor words alone are not sufficient.
The DOM operations of the test do nothing and allocate nothing, thus the
count is the count of kilim. The test makes the new view before the first
read, thus the count is the patch only.

The count finds regressions only. It has no target, because it does not show
the cost under js_of_ocaml. The ceiling is the measured count plus 5 %, plus
10 words, rounded up to a multiple of 10. Each line of the output gives the
count, its ceiling and the new ceiling of the count.

The count depends on the compiler. The first line of `bench/alloc/budget` is
the version of OCaml of the counts (`ocaml 5.5.1`). With another version, the
test fails and tells you to write the version and the new ceilings. The test
runs only in the dev profile and with no flambda. It is not in `test/view`,
because `make mutate` builds `test/view` with the mutation instrumentation,
and the instrumentation changes the counts.

Measured with OCaml 5.5.1, no flambda:

| Action | Allocated words | Ceiling |
|---|---|---|
| Create 1,000 rows | 62,116 | 65,240 |
| Change 1 row of 1,000 | 11,057 | 11,620 |
| Swap 2 rows of 1,000 | 20,114 | 21,130 |
| Remove all rows of 1,000 | 1,033 | 1,100 |
| 1 component row of 1,000 changes its model (`row-model`) | 96 | 110 |
| The parent changes a text outside 1,000 component rows (`header`) | 9,061 | 9,530 |
| The parent removes 1,000 component rows (`remove-rows`) | 1,037 | 1,100 |

The counts are the same in each run. The two component actions mount 1,000
keyed component rows with the diff and no runtime. `row-model` counts the
render of the row alone, with its view: the model changes before the first
read. The row has `Component.Defaults.subs`, thus no subscription pass,
and a diff with no runtime records no instance for the pass. `header` counts the patch of the parent: each row skips its view, and
the parent makes its new view before the first read. `remove-rows` counts
the patch that removes all rows: the unmount of an instance with no child
makes no closure for the walk of its child list.

## Partial update

`test/view/test_budget.ml` mounts 1,000 keyed component rows in a parent on
the fake DOM and the virtual host. Each row has its index and a mark in its
props, and a count in its model. Each action is one click: a discrete turn
with its render. The counts include the reads of the live properties:

| Action | Views | DOM operations |
|---|---|---|
| 1 row changes its model | 1 | `set_text` 1 |
| The parent changes a text outside the rows | 1 | `set_text` 1 |
| The parent changes the props of 1 row | 2 | `set_text` 1 |

The parent view calls `Component.v` 1,000 times, and each call finds the spec
of its module in the cache of the id: thus the view makes no spec.

## Token release

`Private.Cancel.list_ops` counts the operations on the lists of children of
the tokens: each link and each unlink. In `test/view/test_budget.ml`, each
row has a token, with a task and a step below it. The removal of all rows
unlinks only the token of each row: the cancel takes the tokens below it out
of their lists with no unlink.

| Action | List operations |
|---|---|
| Mount 1,000 rows | 3,001 (the token of the root, and 3 for each row) |
| Remove 1,000 rows | 1,000 |
| Remove 2,000 rows | 2,000 |

## The loader text

`test/dist` checks that the loader text (`Kilim_dist.Loader_text.text`) has
at most 2,560 bytes. The inline form has the text two times in each page,
thus the size counts. Measured: 2,555 bytes, and 1,180 bytes with gzip
(level 9, the `zlib` module of node). The text has no comments and no
leading spaces (`src/loader/gen/embed.ml`).

## Open reference sizes

The targets against references are open: the gzip size of an Elm counter app,
and the js-framework-benchmark results of Elm and Preact.

## Known costs

- A controlled value (`At.value`) of 1 MB costs about 0.5 ms (js_of_ocaml)
  and 1.9 ms (wasm_of_ocaml) at each patch of its element: the diff reads the
  live property, converts it to an OCaml string, and compares it with the
  view value. kilim.browser keeps no cache of the value. A cache can come
  later, with a measurement that shows the need.
- A list of N controlled checkboxes reads N live `checked` properties at
  each patch of the list, also for an event in another field. In the todo
  example with 100 items, one `input` event in the draft reads 100 `checked`
  properties and 2 `value` properties, and writes nothing. Components (Phase
  2) and `View.lazy_` (after version 1) can skip the list.
