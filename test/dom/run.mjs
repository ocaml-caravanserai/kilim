// The Chrome runner of the DOM tests (design section 14). It uses only the
// built-in modules of node: Chrome over the DevTools pipe (chrome.mjs), and an
// HTTP server for the build directory.
//
//   node run.mjs [--chrome-flag ...] prog.bc.js prog.bc.wasm.js ...
//
// For each program, the runner opens a new page. The server sends the head of
// the page at once: a <meta charset> and a synchronous <script> of the program.
// It sends the body only when the program asks for it: thus the document is
// "loading" when the program starts, in the two builds. The program calls the
// binding kilimDrive with a JSON text:
//   {"id": n, "release": true}           the server sends the body
//   {"id": n, "steps": [[method, params], ...]}
//                                        DevTools input commands (trusted)
// Then the runner calls window.kilimDriven(n). The program writes its result
// to the console: "DONE ok" is a success. "DONE FAIL", an uncaught exception
// or 60 s with no DONE is a failure. The console lines "EXPECT EXCEPTION <text>"
// and "EXPECT END" permit the uncaught exceptions whose first line is <text>
// between them, and need at least one (a test of an error that no OCaml catch
// sees; Chrome can report one such exception two times).
//
// chrome.mjs finds Chrome. A KILIM_CHROME that is not an executable is a
// failure. With no Chrome, the runner writes "SKIP: no Chrome" and gives exit
// status 0, but 1 if KILIM_REQUIRE_CHROME is 1 (CI).

import { readFileSync } from 'node:fs';
import { extname, join, normalize } from 'node:path';
import http from 'node:http';
import { findChrome, launch, text, withPage } from './chrome.mjs';

// An argument that starts with -- is a flag for Chrome, for example
// --js-flags=--jitless (a Chrome with no WebAssembly).
const args = process.argv.slice(2);
const flags = args.filter((a) => a.startsWith('--'));
const programs = args.filter((a) => !a.startsWith('--'));
const limitMs = 60000;

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

// The server of the build directory. /page/<prog> is the page of a program.
// The launcher of wasm_of_ocaml fetches its assets, thus no file: URL.
const types = { '.js': 'text/javascript', '.wasm': 'application/wasm', '.map': 'application/json' };
const held = new Map();
const body =
  '<body><div id="wrap"><div id="app"><p>placeholder</p></div></div>' +
  '<div id="other"></div></body>';
const server = http.createServer((req, res) => {
  const path = decodeURIComponent(req.url.split('?')[0]);
  if (path.startsWith('/page/')) {
    const prog = path.slice('/page/'.length);
    res.writeHead(200, { 'content-type': 'text/html; charset=utf-8', 'cache-control': 'no-store' });
    res.write(`<!doctype html><html><head><meta charset="utf-8"><title>${prog}</title>` +
      `<script src="/${prog}"></script></head>`);
    held.set(prog, res);
    return;
  }
  const file = normalize(join(process.cwd(), path));
  if (!file.startsWith(process.cwd())) {
    res.writeHead(403);
    res.end();
    return;
  }
  try {
    const data = readFileSync(file);
    res.writeHead(200, { 'content-type': types[extname(file)] || 'application/octet-stream' });
    res.end(data);
  } catch {
    res.writeHead(404);
    res.end();
  }
});
await new Promise((ok) => server.listen(0, '127.0.0.1', ok));
const port = server.address().port;

const browser = launch(chrome, flags);
const cleanup = () => {
  browser.close();
  server.close();
  server.closeAllConnections();
};

// The run of one page, with one time limit from the creation of its target.
const runPage = async (prog) => {
  let done;
  const finished = new Promise((ok) => { done = ok; });
  let expected = null;
  let seen = 0;
  const open = async (page) => {
    page.on(async (m) => {
      if (m.method === 'Runtime.consoleAPICalled') {
        const line = m.params.args.map(text).join(' ');
        console.log(`${prog}: ${line}`);
        if (line.startsWith('EXPECT EXCEPTION ')) {
          expected = line.slice('EXPECT EXCEPTION '.length);
          seen = 0;
        } else if (line === 'EXPECT END') {
          if (seen === 0) done('DONE FAIL (the expected exception did not come)');
          expected = null;
        } else if (line.startsWith('DONE')) done(expected === null ? line : 'DONE FAIL (no EXPECT END)');
      } else if (m.method === 'Runtime.exceptionThrown') {
        const d = m.params.exceptionDetails;
        const what = d.exception?.description ?? d.text;
        if (expected !== null && what.split('\n')[0] === expected) {
          seen++;
          console.log(`${prog}: expected exception: ${expected}`);
        } else {
          console.log(`${prog}: EXCEPTION ${what}`);
          done('DONE FAIL');
        }
      } else if (m.method === 'Runtime.bindingCalled' && m.params.name === 'kilimDrive') {
        try {
          const req = JSON.parse(m.params.payload);
          if (req.release) {
            const res = held.get(prog);
            held.delete(prog);
            if (res) res.end(body);
          }
          for (const [method, params] of req.steps || []) await page.send(method, params);
          await page.send('Runtime.evaluate', { expression: `window.kilimDriven(${req.id})` });
        } catch (e) {
          console.log(`${prog}: DRIVER ERROR ${e.message}`);
          done('DONE FAIL');
        }
      }
    });
    await page.send('Runtime.enable');
    await page.send('Page.enable');
    await page.send('Runtime.addBinding', { name: 'kilimDrive' });
    await page.send('Page.navigate', { url: `http://127.0.0.1:${port}/page/${prog}` });
    return finished;
  };
  try {
    return await withPage(browser, limitMs, open);
  } catch (e) {
    console.log(`${prog}: ${e.message}`);
    return 'DONE FAIL';
  } finally {
    const res = held.get(prog);
    held.delete(prog);
    if (res) res.end(body);
  }
};

let failures = 0;
try {
  for (const prog of programs) {
    const r = await runPage(prog);
    if (r !== 'DONE ok') {
      console.log(`${prog}: FAIL (${r})`);
      failures++;
    }
  }
} catch (e) {
  console.log(`FAIL: ${e.message}`);
  failures++;
}
cleanup();
console.log(failures === 0 ? `all ${programs.length} pages pass` : `${failures} pages FAILED`);
process.exit(failures === 0 ? 0 : 1);
