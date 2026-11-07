// The size budget of kilim (docs/budgets.md). It gives the gzip size (level 9)
// of each program in the release profile, and compares it with the budget. It
// also checks the generated code of the counter: the DOM operations are direct
// calls.
//
//   node size.mjs budget NAME JS LAUNCHER ASSETS [NAME JS LAUNCHER ASSETS ...]
//
// The budget file has one line for each file: PROGRAM FILE BYTES, with FILE
// js, wasm (the main .wasm file) or launcher. A file over its budget is a
// failure. A budget line with no measured file, and a measured file with no
// budget line, are failures too. The program "counter" is the counter of
// bench/size, and its files get the check of the generated code.

import { readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { gzipSync } from 'node:zlib';

const [budgetFile, ...rest] = process.argv.slice(2);
if (!budgetFile || rest.length === 0 || rest.length % 4 !== 0) {
  console.log('usage: node size.mjs budget NAME JS LAUNCHER ASSETS [NAME JS LAUNCHER ASSETS ...]');
  process.exit(2);
}
const budget = new Map(
  readFileSync(budgetFile, 'utf8').trim().split('\n').map((l) => {
    const [program, file, bytes] = l.trim().split(/\s+/);
    return [`${program} ${file}`, Number(bytes)];
  }));
const gz = (f) => gzipSync(readFileSync(f), { level: 9 }).length;

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
  }
}
for (const key of budget.keys())
  if (!measured.has(key)) {
    console.log(`FAIL ${key}: a budget line with no measured file`);
    failures++;
  }
process.exit(failures === 0 ? 0 : 1);
