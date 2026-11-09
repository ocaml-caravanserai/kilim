# The loader

A kilim app starts through a small loader. The loader picks the
wasm_of_ocaml build or the js_of_ocaml build one time, before a build
starts. The design is in [design.html](design.html), section 11, "The
loader". This page tells how to use it.

The package installs the executable `kilim`:

| Command | Result |
|---|---|
| `kilim page` | A page for the dev profile, from your template. |
| `kilim loader -o FILE` | The loader text, for the loader.js form. |
| `kilim csp` | The CSP source expression of the loader text. |
| `kilim dist` | The release files: content names, and the page. |

A usage error gives exit status 2. A fault of a file, a marker or a
conflict gives exit status 1. `kilim COMMAND --help` lists the options, and
`kilim --version` writes the version of the package.

## The template

You write the HTML page. Put two markers in it:

- `<!--kilim:head-->` directly after `<meta charset>` and each CSP
  `<meta>`, and before each stylesheet.
- `<!--kilim:start-->` as the last element of the `body`.

```html
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<!--kilim:head-->
<link rel="stylesheet" href="/static/app.css">
<title>My app</title>
</head>
<body>
<div id="app"></div>
<!--kilim:start-->
</body>
</html>
```

Each marker must occur exactly one time. `kilim` changes no other byte of
the template. The app calls `Kilim_browser.run ~mount:"app" app`.

## The two forms

- The inline form (the default): the two markers get two inline scripts
  with the same text. Add the hash of `kilim csp` to `script-src`.
- The loader.js form (`--form file`): the head marker gets
  `<script src=".../loader.js" async>` and
  `<script src=".../loader.js" defer data-start>`, and the start marker
  goes. Use this form for a CSP with no hash. It costs one more round trip
  before the download of the build starts.

In WebKit (measured in Playwright WebKit 27.2, not yet in Safari), the
loader.js form has two differences:

- A `defer` script does not wait for the stylesheets. Thus the app can
  start before the CSS loads, and a layout that the app reads at the
  start has no CSS.
- An `async` script can run after the `defer` scripts (measured in some
  runs). Then a slow `defer` script of another party delays the start.

Use the inline form if your users have Safari. The inline form does not
have these differences.

## The base and the names

`--base` is the base of all the URLs: a path that ends with `/`, for
example `/static/`. The loader resolves each URL against the base, never
against the URL of the page. Thus with an absolute path, a deep link of a
router app (for example `/users/42`) loads the build. A relative base
(`static/`) is permitted, but a deep link then does not work.

The loader loads only from the origin of the page. `kilim` refuses a base
with a scheme, `//` or `\`, and the loader refuses a URL of another origin
(an error in the console, and no start). Thus a build on a CDN of another
origin is not possible in this version.

The names of `kilim page` (`--js`, `--wasm`, `--loader`) are relative to
the base. A name is not empty, does not start with `/`, has no scheme, no
`//`, no `\` and no `..` segment.

`--build js` in place of `--wasm` gives only the js_of_ocaml build. The
loader then runs no Wasm probe. Use it for a site that never permits
`'wasm-unsafe-eval'`.

## The dev profile

`kilim page` writes the page with fixed names and the attribute
`data-dev`. Copy this rule into the `dune` file of your app:

```dune in=examples/counter/dune
(rule
 (targets index.html)
 (deps page.html main.bc.js main.bc.wasm.js (glob_files main.bc.wasm.assets/*))
 (action
  (run kilim page --template page.html --base / --js main.bc.js
       --wasm main.bc.wasm.js -o index.html)))
```

For the loader.js form, add `--form file` to `kilim page`, and add a second
rule:

```
(rule
 (targets loader.js)
 (action (run kilim loader -o loader.js)))
```

Serve the files with `cache-control: no-cache`. With `no-store`, the
browser can request loader.js two times in the loader.js form.

In the dev profile, the loader writes a console warning, with the prefix
`kilim loader:`, when the document is parsed and the two parts ran:

- A stylesheet comes before the head part.
- A CSP `<meta>` comes after the head part.
- The start part is not in its place: the last element of the `body` in
  the inline form, or directly after the `async` tag in the loader.js form.

The loader also records the part that started the build in the attribute
`data-kilim-started` of the `html` element (`head` or `start`).

## The release files

After `dune build --profile release`, run `kilim dist`:

```sh
kilim dist --template page.html --base /static/ \
  --js _build/default/app/main.bc.js \
  --wasm _build/default/app/main.bc.wasm.js -o dist
```

`kilim dist`:

- Copies the js_of_ocaml build and the launcher of wasm_of_ocaml to
  content names, for example `main.bc.0123456789abcdef.js`. The hash is
  16 hexadecimal characters of BLAKE2b-128.
- Copies the `.assets` directory of the launcher, with its name, next to
  the launcher. The directory must have exactly one `.wasm` file (a
  release build). The loader preloads this file. Playwright WebKit 27.2
  does not use this preload for the fetch of the launcher: it downloads
  the file two times, and it can log that the preload was not used.
- Writes `loader.<hash>.js` in the loader.js form.
- Fills the template, and writes the page to `--page` (default
  `dist/index.html`). The page has no `data-dev`.

A file that exists with the same bytes stays. A file with the same name
and other bytes is an error, and then `kilim dist` writes no file.
`kilim dist` never removes a file.

Serve the files:

- Each file with a content name, and the files of the `.assets`
  directory: `cache-control: public, max-age=31536000, immutable`.
- The page: `cache-control: no-cache`.
- Keep the files of the last deploy for one more deploy. During a deploy
  that is not atomic, an old page can then load its old files.

A repeat visit then starts with no request for the loader and the builds.

## The CSP

- Add the hash of `kilim csp` to `script-src`, for the inline form:
  `script-src 'self' 'sha256-...'`. The hash is the same in the dev and the
  release profiles, and for each app. It changes only when a version of
  kilim changes the loader. In the 0.x versions, a new version can change
  the loader. Thus take the hash from `kilim csp` after each update of
  kilim.
- Do not use `'strict-dynamic'` with kilim. The hash does not cover the
  `data-` attributes: a copy of the loader that an attacker injects into
  the page has the same hash, and with `'strict-dynamic'` the scripts that
  it inserts run. The loader refuses each URL of another origin, but use
  `script-src 'self' 'sha256-...'`: then each script comes from the origin
  of the page.
- Add `'wasm-unsafe-eval'` for the wasm_of_ocaml build. With no
  `'wasm-unsafe-eval'`, the loader picks the js_of_ocaml build. Then the
  probe gives one CSP violation report for each page view in Chrome and
  Firefox. Playwright WebKit 27.2 gives no report.

## Trusted Types

- Add `kilim-loader` to the `trusted-types` directive. The loader sets the
  `src` of the build through a policy with this name. The policy makes
  script URLs only for the build URLs that the loader records. A script of
  the page that runs code can also add to that list: thus the policy is not
  a defense against a script that already runs. It prevents the use of the
  name by another script before the loader. If the directive does not list
  the name, the loader sets a plain URL: with
  `require-trusted-types-for 'script'`, the browser then refuses the build.
- `View.unsafe_html` with Trusted Types: make an HTML policy of the app
  (with its sanitizer), and give its `TrustedHTML` to
  `Kilim_browser.Unsafe_html.of_trusted`. Add the name of that policy to
  `trusted-types` too. kilim makes no HTML policy.

## Other scripts

The build can run before each `defer` script of the page. Thus:

- Link a JS library that the app uses into the build, or load it with a
  blocking script. Put that script after the head part. In the inline
  form, put it also before the start part. A blocking script before the
  head part delays the download of the build.
- A library that is only an ES module: load it with
  `Kilim_web.Es_module.import` in a task (the example that follows).

A specifier that starts with `./`, `../` or `/` resolves against
`document.baseURI`, thus the two builds give the same URL. A bare specifier
stays as it is, for an import map. The import is a JS expression in the
build, not `eval`. Thus a strict CSP accepts it, if its `script-src`
permits the origin of the module. The server must send the
module with a JS MIME type. An error of the import (for example a
`TypeError` for a missing module) fails the task:

```ocaml in=test/doc/es_load.ml
type msg = Chart_loaded of Jv.t

let load_chart : msg Kilim_browser.Async.t =
  Kilim_browser.Async.map
    (fun m -> Chart_loaded m)
    (Kilim_browser.Async.of_fut_or_error (fun _ ->
         Kilim_web.Es_module.import (Jstr.v "/vendor/chart.mjs") ) )
```

## Two apps on one page

Give each app a name with `--name ID` (the characters `a-z`, `0-9` and
`-`). The markers of an app are then `<!--kilim:head:ID-->` and
`<!--kilim:start:ID-->`. Run `kilim page` (or `kilim dist`) one time for
each app: the output of the first run is the template of the second. Each
app starts one time.

## No fallback after a start

The loader picks one build before a build starts. If the build fails
after its start (for example a network fault), the loader does not try
the other build.
