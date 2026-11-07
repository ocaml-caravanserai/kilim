// The Chrome test of the examples (examples/counter and examples/todo). It
// uses only the built-in modules of node, test/dom/chrome.mjs and
// examples/serve.mjs.
//
//   node run.mjs --kilim PATH --profile dev|release DIR...
//
// DIR is the build directory of an example. Its name selects the script of
// the example. For each example, the runner does two runs: a default Chrome
// (the wasm_of_ocaml build) and a Chrome with --js-flags=--jitless (no
// WebAssembly: the js_of_ocaml build). Each run has a new target and a time
// limit of 30 s.
//
// dev: examples/serve.mjs serves DIR at /, with "no-cache". The page is the
// index.html of the rule of docs/loader.md. release: the runner runs the
// command of docs/loader.md, `kilim dist --base /static/`, into a temporary
// directory, and serves it at /static/ with the CSP of docs/loader.md
// (Trusted Types included).
//
// The input is trusted input of DevTools: a click is a mouse press and a
// release at the center of the element, text is Input.insertText after a
// click on the input, and Enter is a key down with the text "\r" and a key
// up. After each action, the runner polls the DOM until the expected state
// comes, with a time limit.
//
// After the script, the runner waits 200 ms for late errors and warnings.
// Chrome logs a preload that the page does not use about 3 s after the load,
// thus a release run lasts at least 4 s. In dev, a self-test run of the first
// example makes the page write a warning 50 ms after the script: the run must
// see it.
//
// The checks of each run, after the wait:
//   - The build: the default Chrome requests a .wasm file and not the
//     js_of_ocaml build. The Chrome with no JIT requests the js_of_ocaml
//     build, and not the launcher.
//   - Each URL gets at most one request (the build uses the preload).
//   - dev: data-kilim-started is "start" (the inline form). release: the
//     attribute is not set.
//   - No uncaught exception, no console error, no console warning, no
//     securitypolicyviolation event, no Chrome log message about a preload,
//     and no Chrome log error or warning other than a 404 for /favicon.ico.
//
// Chrome: see chrome.mjs. With no Chrome, the runner writes "SKIP: no Chrome"
// and gives exit status 0, but 1 if KILIM_REQUIRE_CHROME is 1 (CI).

import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, join, resolve } from 'node:path';
import { findChrome, launch, until, watch, withPage } from '../dom/chrome.mjs';
import { serve } from '../../examples/serve.mjs';

const argv = process.argv.slice(2);
const opt = (name) => {
  const i = argv.indexOf(name);
  return i < 0 ? undefined : argv[i + 1];
};
const kilim = opt('--kilim');
const profile = opt('--profile');
const release = profile === 'release';
const dirs = argv.filter((a, i) => !a.startsWith('--') && !argv[i - 1]?.startsWith('--')).map((d) => resolve(d));
if (!kilim || !(release || profile === 'dev') || dirs.length === 0) {
  console.log('usage: node run.mjs --kilim PATH --profile dev|release DIR...');
  process.exit(2);
}

const runMs = 30000;
const startMs = 10000;
const stepMs = 5000;
const afterMs = 200;
const releaseMinMs = 4000;

// The queries: JS expressions that give one element, or a false value.
const q = {
  label: (s) => `(document.querySelector('[aria-label=' + ${JSON.stringify(JSON.stringify(s))} + ']') || ` +
    `[...document.querySelectorAll('label')].find((l) => l.textContent.trim() === ${JSON.stringify(s)})?.control)`,
  text: (s) => `[...document.querySelectorAll('button')].find((b) => b.textContent.trim() === ${JSON.stringify(s)})`,
  testId: (s) => `document.querySelector('[data-testid=${s}]')`,
};

// The scripts of the examples. ui.until(what, expr) polls until expr is true.
const scripts = {
  counter: async (ui) => {
    const count = (n) => ui.until(`the count ${n}`, `${q.testId('count')}?.textContent === '${n}'`);
    await ui.until('the start', `${q.testId('count')}?.textContent === '0'`, startMs);
    for (const n of [1, 2, 3]) {
      await ui.click(q.label('Increment'));
      await count(n);
    }
    await ui.click(q.label('Decrement'));
    await count(2);
    await ui.click(q.text('Reset'));
    await count(0);
  },
  todo: async (ui) => {
    const draft = q.label('New item');
    const rows = (labels) => ui.until(`the rows ${JSON.stringify(labels)}`,
      `JSON.stringify([...document.querySelectorAll('[data-testid=items] li span')].map((s) => s.textContent)) === ` +
      JSON.stringify(JSON.stringify(labels)));
    const left = (s) => ui.until(`"${s}"`, `${q.testId('left')}.textContent === '${s}'`);
    const li = (label) => `[...document.querySelectorAll('[data-testid=items] li')]` +
      `.find((li) => li.querySelector('span').textContent === ${JSON.stringify(label)})`;
    await ui.until('the start', `${q.testId('left')}?.textContent === '0 items left'`, startMs);
    // 1. Enter submits the form.
    await ui.type(draft, 'Buy milk');
    await ui.until('the draft "Buy milk"', `${draft}.value === 'Buy milk'`);
    await ui.enter();
    await rows(['Buy milk']);
    await ui.until('an empty draft', `${draft}.value === ''`);
    // 2. The "Add" button submits the form.
    await ui.type(draft, 'Walk');
    await ui.until('the draft "Walk"', `${draft}.value === 'Walk'`);
    await ui.click(q.text('Add'));
    await rows(['Buy milk', 'Walk']);
    await left('2 items left');
    // 3. The checkbox.
    await ui.click(q.label('Done: Buy milk'));
    await ui.until('a checked row with the class done',
      `${q.label('Done: Buy milk')}.checked && ${li('Buy milk')}.className === 'done'`);
    await left('1 item left');
    // 4. The filters.
    await ui.click(q.text('Active'));
    await rows(['Walk']);
    await ui.click(q.text('Completed'));
    await rows(['Buy milk']);
    await ui.click(q.text('All'));
    await rows(['Buy milk', 'Walk']);
    // 5. The keyed row of "Walk" keeps its node.
    await ui.read(`${li('Walk')}.kilimMark = 1`);
    await ui.click(q.label('Delete Buy milk'));
    await rows(['Walk']);
    await ui.until('the same li node for "Walk"', `${li('Walk')}.kilimMark === 1`);
    // 6. The restore: the input gets 84 characters, then "abc" again.
    await ui.type(draft, 'abc');
    await ui.until('the draft "abc"', `${draft}.value === 'abc'`);
    await ui.insert('x'.repeat(81));
    await ui.until('an input event with 84 characters', 'window.kilimTest.longest === 84');
    await ui.until('the draft "abc" again', `${draft}.value === 'abc'`);
  },
};
for (const d of dirs)
  if (!scripts[basename(d)]) {
    console.log(`FAIL: no script for ${d}`);
    process.exit(1);
  }

let chrome;
try {
  chrome = findChrome();
} catch (e) {
  console.log(`FAIL: ${e.message}`);
  process.exit(1);
}
if (!chrome) {
  console.log('SKIP: no Chrome');
  process.exit(process.env.KILIM_REQUIRE_CHROME === '1' ? 1 : 0);
}

const csp = release
  ? `script-src 'self' 'wasm-unsafe-eval' ${execFileSync(kilim, ['csp']).toString().trim()}; ` +
    "require-trusted-types-for 'script'; trusted-types kilim-loader"
  : null;

// Runs before each script of the page: the CSP reports, and the length of
// the longest value of an input event (a capture listener runs before the
// handlers of the app, and before the restore).
const probe = `window.kilimTest = { reports: [], longest: 0 };
document.addEventListener('securitypolicyviolation', (e) => {
  window.kilimTest.reports.push(e.violatedDirective + ' ' + e.blockedURI);
});
document.addEventListener('input', (e) => {
  window.kilimTest.longest = Math.max(window.kilimTest.longest, e.target.value.length);
}, true);`;

const work = mkdtempSync(join(tmpdir(), 'kilim-examples-'));

// The release files of an example: `kilim dist` of docs/loader.md.
const dist = (dir) => {
  mkdirSync(join(work, basename(dir)), { recursive: true });
  const out = join(work, basename(dir), 'dist');
  execFileSync(kilim, ['dist', '--template', join(dir, 'page.html'), '--base', '/static/',
    '--js', join(dir, 'main.bc.js'), '--wasm', join(dir, 'main.bc.wasm.js'), '-o', out]);
  return out;
};

const sleep = (ms) => new Promise((ok) => setTimeout(ok, ms));

// selfTest: after the script, the page writes a console warning 50 ms late.
// The run must see it: thus the runner waits after the script.
const runExample = async (browser, dir, jitless, selfTest = false) => {
  const name = basename(dir);
  const faults = [];
  const fault = (s) => faults.push(s);
  const log = [];
  const site = await serve({
    dir: release ? dist(dir) : dir,
    base: release ? '/static/' : null,
    csp,
    port: 0,
    onRequest: (path, status) => log.push({ path, status }),
  });
  let events;
  try {
    await withPage(browser, runMs, async (page) => {
      events = await watch(page);
      await page.send('Page.addScriptToEvaluateOnNewDocument', { source: probe });
      const t0 = Date.now();
      await page.send('Page.navigate', { url: `${site.origin}/` });
      const read = async (expr) => {
        const r = await page.send('Runtime.evaluate', { expression: expr, returnByValue: true });
        if (r.exceptionDetails) throw new Error(`${expr}: ${r.exceptionDetails.exception?.description}`);
        return r.result.value;
      };
      // A read during the navigation can fail: then the poll goes on.
      const poll = async (what, expr, limit = stepMs) => {
        if (!(await until(() => read(`!!(${expr})`), limit)))
          throw new Error(`the state "${what}" did not come in ${limit} ms`);
      };
      const click = async (el) => {
        await poll(`element ${el}`, el);
        const [x, y] = await read(`((e) => {
          e.scrollIntoView({ block: 'center', inline: 'center' });
          const r = e.getBoundingClientRect();
          return [r.left + r.width / 2, r.top + r.height / 2];
        })(${el})`);
        for (const type of ['mousePressed', 'mouseReleased'])
          await page.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 });
      };
      const insert = (s) => page.send('Input.insertText', { text: s });
      const key = { key: 'Enter', code: 'Enter', windowsVirtualKeyCode: 13, nativeVirtualKeyCode: 13 };
      const ui = {
        read,
        until: poll,
        click,
        insert,
        type: async (el, s) => {
          await click(el);
          await poll(`the focus on ${el}`, `document.activeElement === ${el}`);
          await insert(s);
        },
        enter: async () => {
          await page.send('Input.dispatchKeyEvent', { type: 'keyDown', ...key, text: '\r', unmodifiedText: '\r' });
          await page.send('Input.dispatchKeyEvent', { type: 'keyUp', ...key });
        },
      };
      await scripts[name](ui);
      if (selfTest) await read("setTimeout(() => console.warn('kilim self-test'), 50), 0");
      // Late errors and warnings.
      await sleep(Math.max(afterMs, release ? releaseMinMs - (Date.now() - t0) : 0));
      const started = await read("document.documentElement.getAttribute('data-kilim-started')");
      if (started !== (release ? null : 'start')) fault(`data-kilim-started: ${started}`);
      const reports = await read('window.kilimTest.reports');
      if (reports.length) fault(`CSP reports: ${JSON.stringify(reports)}`);
    });
  } catch (e) {
    fault(e.message);
  } finally {
    site.server.close();
    site.server.closeAllConnections();
  }
  // The build of the run, from the request log.
  const paths = log.map((r) => r.path);
  const has = (re) => paths.some((p) => re.test(p));
  const jsBuild = /\/main\.bc\.([0-9a-f]{16}\.)?js$/;
  const launcher = /\/main\.bc\.wasm\.([0-9a-f]{16}\.)?js$/;
  if (jitless) {
    if (!has(jsBuild)) fault('no request for the js_of_ocaml build');
    if (has(launcher) || has(/\.wasm$/)) fault('a request for the wasm_of_ocaml build');
  } else {
    if (!has(/\.wasm$/)) fault('no request for a .wasm file');
    if (has(jsBuild)) fault('a request for the js_of_ocaml build');
  }
  const counts = {};
  for (const p of paths) counts[p] = (counts[p] || 0) + 1;
  for (const [p, c] of Object.entries(counts)) if (c > 1) fault(`${c} requests for ${p}`);
  if (events) {
    if (events.warnings.length) fault(`console warnings: ${JSON.stringify(events.warnings)}`);
    if (events.errors.length) fault(`console errors: ${JSON.stringify(events.errors)}`);
    if (events.exceptions.length) fault(`exceptions: ${JSON.stringify(events.exceptions)}`);
    const logs = events.logs.filter((e) => {
      const favicon = /\/favicon\.ico$/.test(e.url || '') && / 404 /.test(e.text);
      return /preload/i.test(e.text) || ((e.level === 'error' || e.level === 'warning') && !favicon);
    });
    if (logs.length) fault(`log entries: ${JSON.stringify(logs.map((e) => `${e.level}: ${e.text} (${e.url})`))}`);
  }
  return { faults, log };
};

let failures = 0;
let runs = 0;
try {
  for (const jitless of [false, true]) {
    const browser = launch(chrome, jitless ? ['--js-flags=--jitless'] : []);
    try {
      for (const dir of dirs) {
        const label = `${basename(dir)} (${profile}, ${jitless ? 'no JIT: js_of_ocaml' : 'wasm_of_ocaml'})`;
        runs++;
        const r = await runExample(browser, dir, jitless);
        if (r.faults.length) {
          failures++;
          console.log(`FAIL ${label}: ${r.faults.join('; ')}`);
          console.log(`  requests: ${r.log.map((x) => `${x.path} ${x.status}`).join(', ')}`);
        } else console.log(`ok   ${label}`);
      }
      // The self-test of the runner: a warning after the script is a fault.
      if (!jitless && !release) {
        runs++;
        const r = await runExample(browser, dirs[0], false, true);
        const expected = 'console warnings: ["kilim self-test"]';
        if (r.faults.length === 1 && r.faults[0] === expected) console.log('ok   self-test: a late warning is a fault');
        else {
          failures++;
          console.log(`FAIL self-test: faults ${JSON.stringify(r.faults)}, expected only ${expected}`);
        }
      }
    } finally {
      browser.close();
    }
  }
} finally {
  rmSync(work, { recursive: true, force: true });
}
console.log(failures === 0 ? `all ${runs} runs pass` : `${failures} runs FAILED`);
process.exit(failures === 0 ? 0 : 1);
