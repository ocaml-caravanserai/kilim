// The Chrome runner of the DOM tests (design section 14). It uses only the
// built-in modules of node: Chrome over the DevTools pipe, with no port and no
// WebSocket, and an HTTP server for the build directory.
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
// Chrome: $KILIM_CHROME if it is set (a failure if it is not an executable),
// else google-chrome, google-chrome-stable, chromium or chromium-browser in
// PATH, else /usr/bin/google-chrome. With no Chrome, the runner writes
// "SKIP: no Chrome" and gives exit status 0, but 1 if KILIM_REQUIRE_CHROME is
// 1 (CI).

import { spawn } from 'node:child_process';
import { accessSync, constants, mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { delimiter, extname, join, normalize } from 'node:path';
import http from 'node:http';

// An argument that starts with -- is a flag for Chrome, for example
// --js-flags=--jitless (a Chrome with no WebAssembly).
const args = process.argv.slice(2);
const flags = args.filter((a) => a.startsWith('--'));
const programs = args.filter((a) => !a.startsWith('--'));
const limitMs = 60000;

const executable = (f) => {
  try {
    accessSync(f, constants.X_OK);
    return true;
  } catch {
    return false;
  }
};

const findChrome = () => {
  const given = process.env.KILIM_CHROME;
  if (given) {
    if (executable(given)) return given;
    console.log(`FAIL: KILIM_CHROME is not an executable: ${given}`);
    process.exit(1);
  }
  const dirs = (process.env.PATH || '').split(delimiter).filter((d) => d);
  for (const name of ['google-chrome', 'google-chrome-stable', 'chromium', 'chromium-browser'])
    for (const d of dirs) if (executable(join(d, name))) return join(d, name);
  return executable('/usr/bin/google-chrome') ? '/usr/bin/google-chrome' : null;
};

const chrome = findChrome();
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

// Chrome gets a minimal environment: the system Chrome can break with the
// libraries of the nix store.
const home = mkdtempSync(join(tmpdir(), 'kilim-chrome-'));
const env = { HOME: home, TMPDIR: tmpdir(), PATH: '/usr/bin:/bin', LANG: 'C.UTF-8' };
const proc = spawn(
  chrome,
  ['--headless', '--no-sandbox', '--remote-debugging-pipe', `--user-data-dir=${join(home, 'profile')}`,
    '--no-first-run', '--no-default-browser-check', '--disable-gpu', ...flags, 'about:blank'],
  { env, stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe'] });
proc.on('error', (e) => {
  console.log(`FAIL: Chrome does not start: ${e.message}`);
  process.exit(1);
});
let stderr = '';
proc.stderr.on('data', (d) => { stderr += d.toString(); });

const cleanup = () => {
  proc.kill();
  server.close();
  server.closeAllConnections();
  try { rmSync(home, { recursive: true, force: true }); } catch {}
};

// The DevTools protocol: NUL-separated JSON on fds 3 (to Chrome) and 4. When
// Chrome exits, each command that waits and each later command fails at once.
const toChrome = proc.stdio[3];
const fromChrome = proc.stdio[4];
let nextId = 0;
const waiting = new Map();
const listeners = new Set();
let buffer = '';
let gone = null;
let pageDone = null;
const fail = (why) => {
  if (gone) return;
  gone = new Error(`${why}. The end of the stderr of Chrome:\n${stderr.slice(-2000)}`);
  for (const [, ko] of waiting.values()) ko(gone);
  waiting.clear();
  if (pageDone) {
    console.log(gone.message);
    pageDone('DONE FAIL');
  }
};
toChrome.on('error', (e) => fail(`the pipe to Chrome: ${e.message}`));
fromChrome.on('data', (d) => {
  buffer += d.toString();
  let i;
  while ((i = buffer.indexOf('\0')) >= 0) {
    const m = JSON.parse(buffer.slice(0, i));
    buffer = buffer.slice(i + 1);
    if (m.id !== undefined && waiting.has(m.id)) {
      const [ok, ko] = waiting.get(m.id);
      waiting.delete(m.id);
      if (m.error) ko(new Error(JSON.stringify(m.error)));
      else ok(m.result);
    } else for (const l of listeners) l(m);
  }
});
const send = (method, params = {}, sessionId) =>
  new Promise((ok, ko) => {
    if (gone) {
      ko(gone);
      return;
    }
    const m = { id: ++nextId, method, params };
    if (sessionId) m.sessionId = sessionId;
    waiting.set(m.id, [ok, ko]);
    toChrome.write(JSON.stringify(m) + '\0');
  });
proc.on('exit', (code) => fail(`Chrome exited (${code})`));

const text = (a) => (a.value !== undefined ? String(a.value) : a.description ?? a.type);

// One time limit for the whole run of a page, from the creation of its target.
const runPage = async (prog) => {
  let done;
  const finished = new Promise((ok) => { done = ok; });
  pageDone = done;
  const timer = setTimeout(() => done('DONE TIMEOUT'), limitMs);
  let expected = null;
  let seen = 0;
  let sessionId;
  const s = (method, params) => send(method, params, sessionId);
  const listener = async (m) => {
    if (!sessionId || m.sessionId !== sessionId) return;
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
        for (const [method, params] of req.steps || []) await s(method, params);
        await s('Runtime.evaluate', { expression: `window.kilimDriven(${req.id})` });
      } catch (e) {
        console.log(`${prog}: DRIVER ERROR ${e.message}`);
        done('DONE FAIL');
      }
    }
  };
  listeners.add(listener);
  let targetId;
  const setup = async () => {
    ({ targetId } = await send('Target.createTarget', { url: 'about:blank' }));
    ({ sessionId } = await send('Target.attachToTarget', { targetId, flatten: true }));
    await s('Runtime.enable');
    await s('Page.enable');
    await s('Runtime.addBinding', { name: 'kilimDrive' });
    await s('Page.navigate', { url: `http://127.0.0.1:${port}/page/${prog}` });
  };
  try {
    // The setup races with the time limit. A failure of the setup after the
    // limit is handled, thus node does not stop.
    const ready = setup();
    ready.catch(() => {});
    await Promise.race([ready, finished]);
    return await finished;
  } catch (e) {
    console.log(`${prog}: ${e.message}`);
    return 'DONE FAIL';
  } finally {
    clearTimeout(timer);
    pageDone = null;
    listeners.delete(listener);
    const res = held.get(prog);
    held.delete(prog);
    if (res) res.end(body);
    if (targetId && !gone) await send('Target.closeTarget', { targetId }).catch(() => {});
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
