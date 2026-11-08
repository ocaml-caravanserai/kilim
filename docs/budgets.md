# Budgets

The budgets of kilim (section 16 of [design.html](design.html)). `make
budgets` is the budget gate. CI runs it on each push to main and on each
pull request.

## The budgets and their checks

| Budget | Check in CI | Where | State |
|---|---|---|---|
| Bundle size (gzip), for each build | Yes, each change | `bench/size`: the counter of `bench/size` and the todo example | Done |
| DOM operations for each action | Yes, each change | `test/view/test_budget.ml`: the four actions, exact counts. `examples/todo/test/test_counts.ml`: three actions of the todo example | Done |
| Allocations for each action | Yes, each change | `bench/alloc`: the allocated words of the four actions | Done |
| Leaks after 1,000 mounts | Yes, each change | `test/turn/test_leak.ml` (tokens, timers, keys, instances and child links, 100 times), `test/cross` (the same, 1,000 times, in the three builds), and test D25 of `test/dom` (listeners, timers, DOM, in Chrome) | Done |
| Partial update | Yes, each change | `test/view/test_budget.ml`: three actions on 1,000 keyed component rows, exact counts of the views and of the DOM operations. `bench/alloc`: the allocated words of two actions | Done |
| Token release | Yes, each change | `test/view/test_budget.ml`: the operations on the token lists when the parent removes 1,000 and 2,000 component rows | Done |
| Linked modules | Yes, each change | `test/link` (kilim, kilim.browser) | Done |
| Time of the actions, wasm start-up time | No | Each night on fixed hardware | Waits: no fixed hardware yet |

The leak tests and the link check run in the CI step "Test", with the other
tests. `make budgets` runs the size, the allocation, the DOM operation, the
partial update and the token release checks.

## Size

`make size` (and `make budgets`) builds two programs in the release profile
and runs `bench/size/size.mjs`:

- `counter`: `bench/size/counter.ml`, one int model, two buttons, one text,
  and `Kilim_browser.run`. This program is the fixed reference of the size
  budget. The open reference sizes compare with it.
- `example-todo`: the todo example (`examples/todo`), a real app.

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
| `counter` | `counter.bc.js` (js_of_ocaml) | 37,268 bytes | 38,500 bytes |
| `counter` | The main `.wasm` file (wasm_of_ocaml) | 37,010 bytes | 38,000 bytes |
| `counter` | `counter.bc.wasm.js` (the launcher) | 6,889 bytes | 7,500 bytes |
| `example-todo` | `main.bc.js` (js_of_ocaml) | 39,092 bytes | 41,000 bytes |
| `example-todo` | The main `.wasm` file (wasm_of_ocaml) | 39,417 bytes | 40,500 bytes |
| `example-todo` | `main.bc.wasm.js` (the launcher) | 6,886 bytes | 7,500 bytes |

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
read. `header` counts the patch of the parent: each row skips its view, and
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
