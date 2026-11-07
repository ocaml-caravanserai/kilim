// The size budget of kilim (docs/budgets.md). It gives the gzip size (level 9)
// of the counter in the release profile, and compares it with the budget. It
// also checks the generated code: the DOM operations are direct calls.
//
//   node size.mjs budget counter.bc.js counter.bc.wasm.js counter.bc.wasm.assets

import { readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { gzipSync } from 'node:zlib';

const [budgetFile, js, launcher, dir] = process.argv.slice(2);
const assets = readdirSync(dir).filter((f) => f.endsWith('.wasm')).map((f) => join(dir, f));
const budget = Object.fromEntries(
  readFileSync(budgetFile, 'utf8').trim().split('\n').map((l) => {
    const [k, v] = l.trim().split(/\s+/);
    return [k, Number(v)];
  }));
const gz = (f) => gzipSync(readFileSync(f), { level: 9 }).length;
// The main .wasm file is the largest asset.
const wasm = Math.max(...assets.map(gz));
const sizes = { js: gz(js), wasm, launcher: gz(launcher) };
let failures = 0;
for (const k of ['js', 'wasm', 'launcher']) {
  const ok = sizes[k] <= budget[k];
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${k}: ${sizes[k]} bytes (budget ${budget[k]})`);
  if (!ok) failures++;
}
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
process.exit(failures === 0 ? 0 : 1);
