// The runner of the DOM tests (design section 14). It uses only the built-in
// modules of node: the browser interface (browser.mjs), and an HTTP server for
// the build directory.
//
//   node run.mjs [--browser NAME] [--no-wasm] prog.bc.js prog.bc.wasm.js ...
//
// --browser NAME: chrome (the default), firefox or webkit. --no-wasm: a
// browser with no WebAssembly (browser.mjs). A page of a program whose name
// starts with "test_csp" has a CSP with no 'unsafe-eval' (the header of
// cspPolicy). The head of each other page has an import map: the bare
// specifier "kilim-bare" is /mod/web_mod.mjs.
//
// For each program, the runner opens a new page, with the query
// ?browser=NAME: thus a page knows its browser. The server sends the head of the
// page at once: a <meta charset>, a comment of 1,024 spaces and a synchronous
// <script> of the program. It sends the body only when the program asks for
// it: thus the document is "loading" when the program starts, in the two
// builds. WebKit runs the script of a short head only when more bytes come
// (measured: the script ran with 600 bytes more), thus the comment. The
// program calls the binding kilimDrive with a JSON text:
//   {"id": n, "release": true}           the server sends the body
//   {"id": n, "steps": [[method, params], ...]}
//                                        DevTools input commands (trusted)
// Then the runner calls window.kilimDriven(n). The program writes its result
// to the console: "DONE ok" is a success. "DONE FAIL", an uncaught exception
// or 60 s with no DONE is a failure. The console lines "EXPECT EXCEPTION <text>"
// and "EXPECT END" permit the uncaught exceptions whose first line is <text>
// between them, and need at least one (a test of an error that no OCaml catch
// sees; Chrome and Firefox can report one such exception two times). A line
// "SKIP <name>: <reason>" is a check that does not run in this browser (no
// failure): the runner writes the count of these lines at the end.
//
// A browser configuration that is not valid is a failure. With no browser,
// the runner writes the skip line of browser.mjs.

import { readFileSync } from 'node:fs';
import { extname, join, normalize } from 'node:path';
import http from 'node:http';
import { open } from './browser.mjs';

const args = process.argv.slice(2);
const opt = (name) => {
  const i = args.indexOf(name);
  return i < 0 ? undefined : args[i + 1];
};
const name = opt('--browser') ?? 'chrome';
const noWasm = args.includes('--no-wasm');
const programs = args.filter((a, i) => !a.startsWith('--') && args[i - 1] !== '--browser');
const flags = args.filter((a, i) => a.startsWith('--') && !['--browser', '--no-wasm'].includes(a));
if (flags.length || programs.length === 0) {
  console.log('usage: node run.mjs [--browser NAME] [--no-wasm] PROGRAM...');
  process.exit(2);
}
const limitMs = 60000;

const backend = await open(name);

// The server of the build directory. /page/<prog> is the page of a program,
// and also each path under /page/<prog>/.
// The launcher of wasm_of_ocaml fetches its assets, thus no file: URL.
const types = { '.js': 'text/javascript', '.mjs': 'text/javascript', '.wasm': 'application/wasm', '.map': 'application/json' };
const held = new Map();
const cspPolicy = "script-src 'self' 'wasm-unsafe-eval'";
// The head of a page of a program whose name starts with "test_http" wraps
// fetch before the program starts (Brr reads fetch at its start): the
// wrapper records the signal of each call for test_http.
const fetchSpy = `<script>(() => {
  const fetch = globalThis.fetch, signals = [];
  globalThis.fetch = function (url, init) {
    signals.push(init && init.signal ? init.signal : null);
    return fetch.apply(this, arguments);
  };
  globalThis.kilimFetches = () => signals.length;
  globalThis.kilimSignals = (from) => signals.slice(from)
    .map((s) => (s === null ? 'N' : s.aborted ? 'A' : 'P')).join('');
})()</script>`;
const body =
  '<body><div id="wrap"><div id="app"><p>placeholder</p></div></div>' +
  '<div id="other"></div></body>';
// The endpoints of test_http (kilim.http). /http/echo gives the method, the
// headers and the body of the request as JSON. /http/status/N gives the
// status N. /http/never gives no answer: the abort of the client or the end of
// the runner closes the connection. /http/body-slow sends the headers and a
// part of the body at once, and the rest after 300 ms. /http/body-broken
// sends the headers and a part of the body, then closes the connection.
// /http/bad-json gives a body that is not JSON.
const httpEndpoint = (req, res, path) => {
  if (path === '/http/echo') {
    let body = '';
    req.setEncoding('utf8');
    req.on('data', (c) => { body += c; });
    req.on('end', () => {
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ method: req.method, headers: req.headers, body }));
    });
  } else if (path.startsWith('/http/status/')) {
    const n = Number(path.slice('/http/status/'.length));
    res.writeHead(n, { 'content-type': 'text/plain' });
    res.end(`status ${n}`);
  } else if (path === '/http/body-slow') {
    res.writeHead(200, { 'content-type': 'text/plain' });
    res.write('slow ');
    setTimeout(() => res.end('body'), 300);
  } else if (path === '/http/body-broken') {
    res.writeHead(200, { 'content-type': 'text/plain', 'content-length': '100' });
    res.write('part');
    setTimeout(() => res.destroy(), 100);
  } else if (path === '/http/bad-json') {
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end('{"a":');
  } else if (path !== '/http/never') {
    res.writeHead(404);
    res.end();
  }
};
const server = http.createServer((req, res) => {
  const path = decodeURIComponent(req.url.split('?')[0]);
  if (path.startsWith('/http/')) return httpEndpoint(req, res, path);
  if (path.startsWith('/page/')) {
    // /page/<prog>/<deep path> is the page of <prog> too: a reload of a path
    // of a router works.
    const prog = path.slice('/page/'.length).split('/')[0];
    const headers = { 'content-type': 'text/html; charset=utf-8', 'cache-control': 'no-store' };
    const csp = prog.startsWith('test_csp');
    if (csp) headers['content-security-policy'] = cspPolicy;
    const importMap = csp ? '' :
      '<script type="importmap">{"imports":{"kilim-bare":"/mod/web_mod.mjs"}}</script>';
    res.writeHead(200, headers);
    res.write(`<!doctype html><html><head><meta charset="utf-8"><title>${prog}</title>` +
      `<!--${' '.repeat(1024)}-->${importMap}` +
      `${prog.startsWith('test_http') ? fetchSpy : ''}<script src="/${prog}"></script></head>`);
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

const browser = await backend.launch({ noWasm });
const cleanup = async () => {
  await browser.close();
  server.close();
  server.closeAllConnections();
};

// The run of one page, with one time limit from the creation of its page.
const runPage = async (prog) => {
  let done;
  const finished = new Promise((ok) => { done = ok; });
  let expected = null;
  let seen = 0;
  const run = async (page) => {
    page.on((e) => {
      if (e.kind === 'console') {
        console.log(`${prog}: ${e.text}`);
        if (e.text.startsWith('SKIP ')) skips++;
        else if (e.text.startsWith('EXPECT EXCEPTION ')) {
          expected = e.text.slice('EXPECT EXCEPTION '.length);
          seen = 0;
        } else if (e.text === 'EXPECT END') {
          if (seen === 0) done('DONE FAIL (the expected exception did not come)');
          expected = null;
        } else if (e.text.startsWith('DONE')) done(expected === null ? e.text : 'DONE FAIL (no EXPECT END)');
      } else if (e.kind === 'exception') {
        if (expected !== null && e.text.split('\n')[0] === expected) {
          seen++;
          console.log(`${prog}: expected exception: ${expected}`);
        } else {
          console.log(`${prog}: EXCEPTION ${e.text}`);
          done('DONE FAIL');
        }
      }
    });
    await page.bind('kilimDrive', async (payload) => {
      try {
        const req = JSON.parse(payload);
        if (req.release) {
          const res = held.get(prog);
          held.delete(prog);
          if (res) res.end(body);
        }
        for (const [method, params] of req.steps || []) await page.input(method, params);
        await page.evaluate(`window.kilimDriven(${req.id})`);
      } catch (e) {
        console.log(`${prog}: DRIVER ERROR ${e.message}`);
        done('DONE FAIL');
      }
    });
    await page.goto(`http://127.0.0.1:${port}/page/${prog}?browser=${name}`);
    return finished;
  };
  try {
    return await browser.withPage(limitMs, run);
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
let skips = 0;
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
await cleanup();
console.log((failures === 0 ? `all ${programs.length} pages pass` : `${failures} pages FAILED`) +
  ` in ${name}${skips ? `, ${skips} checks skipped` : ''}`);
process.exit(failures === 0 ? 0 : 1);
