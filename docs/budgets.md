# Budgets

The budgets of kilim (section 16 of [design.html](design.html)). CI checks
the size budget on each push to main and on each pull request.

## Size

`make size` builds `bench/size/counter.ml` in the release profile and runs
`bench/size/size.mjs`. The program is a counter: one int model, two buttons,
one text, and `Kilim_browser.run`. The script gives the gzip size (level 9,
the `zlib` module of node) of each file, and compares it with
`bench/size/budget`. A larger file is a failure.

The budget is the measured size plus 5 %, rounded up to 500 bytes. When a
change of kilim makes a file larger, examine the linked modules first. Change
the budget only for a known cost, and write the cost here.

Measured with OCaml 5.5.1, js_of_ocaml and wasm_of_ocaml 6.4.1, Brr 0.0.9:

| File | Measured | Budget |
|---|---|---|
| `counter.bc.js` (js_of_ocaml) | 36,647 bytes | 38,500 bytes |
| The main `.wasm` file (wasm_of_ocaml) | 35,916 bytes | 37,000 bytes |
| `counter.bc.wasm.js` (the launcher) | 6,846 bytes | 7,500 bytes |

The baseline is a minimal Brr program, because Brr links `Fun` and thus
`Printf` (design section 13): 23,845 bytes (js_of_ocaml), and 17,217 bytes
for the main `.wasm` file plus 6,482 bytes for the launcher (wasm_of_ocaml).
Thus kilim adds about 12.8 KB (js_of_ocaml) and 18.7 KB (wasm_of_ocaml) to
the counter. The Brr pull request that replaces `Fun.id` would make the baseline
smaller by about 10 KB.

The same rule checks the generated code: the release `counter.bc.js` contains
`.insertBefore(`, `.setAttribute(`, `.__kilim` and `.addEventListener(` (the
DOM operations are direct calls), and the launcher contains
`"meth_call_2_insertBefore"` and `"get___kilim"`.

## Open reference sizes

The targets against references are open: the gzip size of an Elm counter app,
and the js-framework-benchmark results of Elm and Preact.

## Known costs

- A controlled value (`At.value`) of 1 MB costs about 0.5 ms (js_of_ocaml)
  and 1.9 ms (wasm_of_ocaml) at each patch of its element: the diff reads the
  live property, converts it to an OCaml string, and compares it with the
  view value. kilim.browser keeps no cache of the value. A cache can come
  later, with a measurement that shows the need.
