# Phase 0 results

Phase 0 builds three spikes before the real work (section 16 of
[design.html](design.html)). Each spike runs in the builds that it targets:
native code, js_of_ocaml 6.4.1 and wasm_of_ocaml 6.4.1, with OCaml 5.5.1 on
node 24. `make test` runs all of them.

## Spike 1: Async and Cancel

The library `kilim` (src/core) took the place of this spike. Its tests are in
test/core (windtrap), test/cross (the three builds) and test/browser (a real
host on Brr). The results that follow are from the spike.

The library uses only the standard library. A test with a fake host and a
virtual clock passes 16 checks in the three builds:

- A chain of 1,000,000 synchronous steps, right-nested and left-nested.
- The once rule, an error of a later step, and an error before a cancel.
- An `ok` from a callback that kilim does not wrap.
- End actions after `ok`, `on_end` after a release, and the cancel order.
- A cancel of an ended token sets its cancelled flag.
- 100,000 nested `and+`: the start and the cancel do not overflow the stack.
- `sleep`, a cancel that clears its timer, `first` and `timeout`.

A second test uses a real host on Brr, in the js_of_ocaml and wasm_of_ocaml
builds. It passes 4 checks: a real timer, a cancel of a real timer, a fetch,
and a fetch with a cancel (a local HTTP server on node).

Finding: an end action runs also when a step ends normally. Thus a fetch
binding must read the body in the same step as the response, and its end
action must abort only if the token is cancelled. The design now has this
rule (section 12).

Note: node crashes when a fetch of a `data:` URL is aborted. This is a fault
of undici in node, not of kilim. The test uses a local HTTP server.

## Spike 2: components (`spikes/component`)

A small model of the component runtime, with no DOM. `Component.v` is an
OCaml 5.5 module-dependent function, and `Type.Id` gives back the types of a
stored instance with no `Obj.magic`. 7 checks pass in the three builds:

- A mount of an App with two Counters.
- An output goes to the parent through `Ctx.emit` and `~on`.
- Equal props: the children do not render.
- New props: the child renders.
- `update` gives the same model: no render (physical identity).
- A key change resets the child, and the other child keeps its model.

## Spike 3: the DOM hot path and the props compare (`spikes/dom`)

The generated code has direct calls, in the two builds:

- js_of_ocaml: `e.setAttribute(a, v)`.
- wasm_of_ocaml: the launcher has fixed-arity JS fragments, for example
  `"meth_call_2_setAttribute":(a,b,c)=>a.setAttribute(b,c)`.

The cost of one call on a fake element on node:

| Operation | js_of_ocaml | wasm_of_ocaml |
|---|---|---|
| `setAttribute` (two strings) | 17 ns | 92 ns |
| `textContent` (one string) | 8 ns | 46 ns |
| `appendChild` | 68 ns | 43 ns |

Under wasm_of_ocaml, each string is copied at the boundary. Thus 1,000
attribute writes cost about 0.1 ms in that build.

The props compare passes its cases: 12 natively, 12 and 14 under
wasm_of_ocaml (compare by tag), and 14 under js_of_ocaml (`Object.is`). The
cases include `0.` against `-0.`, floats, strings, closures (top level and in
a field) and JS objects. Before Phase 0, the result under wasm_of_ocaml came
only from the source.

## Sizes (release profile, gzip -9)

| Program | Size |
|---|---|
| A minimal Brr program, js_of_ocaml | 24,157 bytes |
| The same program, wasm_of_ocaml: the one `code-<hash>.wasm` | 17,574 bytes |
| The same program, wasm_of_ocaml: the launcher | 6,497 bytes |

The js_of_ocaml size agrees with the Brr baseline of about 24 KB in the design
(Brr links `Fun`, and thus `Printf`). The release build of wasm_of_ocaml gives
one main `.wasm` file, as the design expects for the preload.

## Open items of Phase 0

- The `JSTag` check in a real Safari. This system has no Safari.
- Target numbers for the budgets against references (the gzip size of an Elm
  counter app, and the js-framework-benchmark results of Elm and Preact).
- The pull request to Brr that replaces `Fun.id` and `Fun.const`.
