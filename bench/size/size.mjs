// The size budget of kilim (docs/budgets.md). It gives the gzip size (level 9)
// of each program in the release profile, and compares it with the budget. It
// also checks the generated code of the counter: the DOM operations are direct
// calls, and the counter contains no binding that it does not use.
//
//   node size.mjs budget --control JS LAUNCHER ASSETS
//     NAME JS LAUNCHER ASSETS [NAME JS LAUNCHER ASSETS ...]
//
// The budget file has one line for each file: PROGRAM FILE BYTES, with FILE
// js, wasm (the main .wasm file) or launcher. A file over its budget is a
// failure. A budget line with no measured file, and a measured file with no
// budget line, are failures too. The program "counter" is the counter of
// bench/size, and its files get the check of the generated code. The control
// program (bench/size/control.ml) uses each binding: it is the positive
// control of the names that the counter must not contain. It has no budget.

import { readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { gzipSync } from 'node:zlib';

const [budgetFile, flag, ...args] = process.argv.slice(2);
const control = args.slice(0, 3);
const rest = args.slice(3);
if (!budgetFile || flag !== '--control' || rest.length === 0 || rest.length % 4 !== 0) {
  console.log('usage: node size.mjs budget --control JS LAUNCHER ASSETS NAME JS LAUNCHER ASSETS ...');
  process.exit(2);
}
const budget = new Map(
  readFileSync(budgetFile, 'utf8').trim().split('\n').map((l) => {
    const [program, file, bytes] = l.trim().split(/\s+/);
    return [`${program} ${file}`, Number(bytes)];
  }));
const gz = (f) => gzipSync(readFileSync(f), { level: 9 }).length;
// The main .wasm file is the largest asset.
const mainWasm = (dir) =>
  readdirSync(dir).filter((f) => f.endsWith('.wasm')).map((f) => join(dir, f))
    .reduce((a, b) => (gz(a) >= gz(b) ? a : b));
const text = (files) => files.map((f) => readFileSync(f, 'latin1')).join('\n');
// The names of the bindings that the counter does not use: Trusted_types
// (Unsafe_html.of_trusted), the JS cell of Async.of_fut and Es_module.import.
// Then kilim.router.browser (pushState), Kilim_web.Scroll (scrollTo) and the
// fetch transport of kilim.http.browser (AbortController).
const unused = ['trustedTypes', 'isHTML', 'kilimFut', 'import(', 'pushState', 'scrollTo',
  'AbortController'];

let failures = 0;
const measured = new Set();
for (let i = 0; i < rest.length; i += 4) {
  const [name, js, launcher, dir] = rest.slice(i, i + 4);
  const assets = readdirSync(dir).filter((f) => f.endsWith('.wasm')).map((f) => join(dir, f));
  // The main .wasm file is the largest asset.
  const sizes = { js: gz(js), wasm: Math.max(...assets.map(gz)), launcher: gz(launcher) };
  for (const file of ['js', 'wasm', 'launcher']) {
    const key = `${name} ${file}`;
    measured.add(key);
    const b = budget.get(key);
    const ok = b !== undefined && sizes[file] <= b;
    console.log(`${ok ? 'ok  ' : 'FAIL'} ${key}: ${sizes[file]} bytes (budget ${b ?? 'none'})`);
    if (!ok) failures++;
  }
  if (name === 'counter') {
    const has = (file, texts) => {
      const s = readFileSync(file, 'utf8');
      for (const t of texts) {
        const ok = s.includes(t);
        console.log(`${ok ? 'ok  ' : 'FAIL'} ${file} contains ${t}`);
        if (!ok) failures++;
      }
    };
    has(js, ['.insertBefore(', '.setAttribute(', '.__kilim', '.addEventListener(']);
    has(launcher, ['"meth_call_2_insertBefore"', '"get___kilim"']);
    // An app does not pay for a binding that it does not use: no build of the
    // counter contains the names of the bindings that it does not use. The
    // positive controls: the same read finds each name in the same build of
    // the control program, and the name of Kilim_web.Microtask, which the
    // counter uses, in each file of the counter. A build is the .js file, or
    // the launcher and the main .wasm file (read as bytes).
    const [cjs, claunch, cdir] = control;
    const builds = [
      ['js', [js], [cjs]],
      ['wasm', [launcher, mainWasm(dir)], [claunch, mainWasm(cdir)]],
    ];
    const found = (what, s, t, want) => {
      const ok = s.includes(t) === want;
      console.log(`${ok ? 'ok  ' : 'FAIL'} ${what} ${want ? 'contains' : 'lacks'} ${t}`);
      if (!ok) failures++;
    };
    for (const [build, files, cfiles] of builds) {
      const s = text(files), c = text(cfiles);
      for (const t of unused) {
        found(`control ${build}`, c, t, true);
        found(`counter ${build}`, s, t, false);
      }
    }
    for (const file of [js, launcher, mainWasm(dir)])
      found(file, text([file]), 'queueMicrotask', true);
  }
}
for (const key of budget.keys())
  if (!measured.has(key)) {
    console.log(`FAIL ${key}: a budget line with no measured file`);
    failures++;
  }
process.exit(failures === 0 ? 0 : 1);
