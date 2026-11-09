// The Chrome back end of the browser interface (browser.mjs): find Chrome,
// start it with a minimal environment, and talk to it over the DevTools pipe
// (no port, no WebSocket). The import has no side effect: the caller decides
// what a missing Chrome gives (the skip rules of browser.mjs).

import { spawn } from 'node:child_process';
import { accessSync, constants, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { delimiter, join } from 'node:path';

const executable = (f) => {
  try {
    accessSync(f, constants.X_OK);
    return true;
  } catch {
    return false;
  }
};

// $KILIM_CHROME if it is set, else google-chrome, google-chrome-stable,
// chromium or chromium-browser in PATH, else /usr/bin/google-chrome. The result
// is null if there is no Chrome. A KILIM_CHROME that is not an executable
// throws: it is a fault of the configuration, not a missing Chrome.
export const findChrome = () => {
  const given = process.env.KILIM_CHROME;
  if (given) {
    if (executable(given)) return given;
    throw new Error(`KILIM_CHROME is not an executable: ${given}`);
  }
  const dirs = (process.env.PATH || '').split(delimiter).filter((d) => d);
  for (const name of ['google-chrome', 'google-chrome-stable', 'chromium', 'chromium-browser'])
    for (const d of dirs) if (executable(join(d, name))) return join(d, name);
  return executable('/usr/bin/google-chrome') ? '/usr/bin/google-chrome' : null;
};

// Starts a headless Chrome with the extra flags (for example
// --js-flags=--jitless). Chrome gets a minimal environment: the system Chrome
// can break with the libraries of the nix store. The result:
//   send(method, params, sessionId)   a DevTools command, a promise
//   listen(f)                         f gets each event; gives the remove function
//   whenGone                          a promise that rejects when Chrome exits,
//                                     with the end of the stderr of Chrome
//   gone()                            that error, or null
//   close()                           stops Chrome, waits for its exit, then
//                                     removes its directory (a promise)
const launch = (chrome, flags = []) => {
  const home = mkdtempSync(join(tmpdir(), 'kilim-chrome-'));
  const env = { HOME: home, TMPDIR: tmpdir(), PATH: '/usr/bin:/bin', LANG: 'C.UTF-8' };
  const proc = spawn(
    chrome,
    ['--headless', '--no-sandbox', '--remote-debugging-pipe', `--user-data-dir=${join(home, 'profile')}`,
      '--no-first-run', '--no-default-browser-check', '--disable-gpu', ...flags, 'about:blank'],
    { env, stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe'] });
  let stderr = '';
  proc.stderr.on('data', (d) => { stderr += d.toString(); });

  // NUL-separated JSON on fds 3 (to Chrome) and 4. When Chrome exits, each
  // command that waits and each later command fails at once.
  const toChrome = proc.stdio[3];
  const fromChrome = proc.stdio[4];
  let nextId = 0;
  const waiting = new Map();
  const listeners = new Set();
  let buffer = '';
  let gone = null;
  let goneKo;
  const whenGone = new Promise((_, ko) => { goneKo = ko; });
  whenGone.catch(() => {});
  const fail = (why) => {
    if (gone) return;
    gone = new Error(`${why}. The end of the stderr of Chrome:\n${stderr.slice(-2000)}`);
    for (const [, ko] of waiting.values()) ko(gone);
    waiting.clear();
    goneKo(gone);
  };
  proc.on('error', (e) => fail(`Chrome does not start: ${e.message}`));
  proc.on('exit', (code) => fail(`Chrome exited (${code})`));
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
  const listen = (f) => {
    listeners.add(f);
    return () => listeners.delete(f);
  };
  // Chrome can write to its directory until it exits. Thus close waits for
  // the exit (2 s, then SIGKILL and 2 s more) before it removes the directory.
  const exited = new Promise((ok) => {
    if (proc.exitCode !== null || proc.signalCode !== null) ok();
    else proc.once('exit', ok);
  });
  const waitExit = (ms) => {
    let timer;
    const late = new Promise((ok) => { timer = setTimeout(() => ok(false), ms); });
    return Promise.race([exited.then(() => true), late]).finally(() => clearTimeout(timer));
  };
  const close = async () => {
    proc.kill();
    if (!(await waitExit(2000))) {
      proc.kill('SIGKILL');
      await waitExit(2000);
    }
    try { rmSync(home, { recursive: true, force: true }); } catch {}
  };
  return { send, listen, whenGone, gone: () => gone, close };
};

// The text of a console argument of the protocol.
const text = (a) => (a.value !== undefined ? String(a.value) : a.description ?? a.type);

// Runs body(page) in a new target, with one time limit for the whole run, from
// the creation of the target. page is the page of browser.mjs: the Runtime,
// Log and Page domains of the target are on before body starts. The result is
// the result of body. It rejects at the time limit, when Chrome exits, or when
// body rejects. At the end, the target closes.
const withPage = async (browser, limitMs, body) => {
  let sessionId;
  let targetId;
  let timer;
  const handlers = [];
  const bindings = new Map();
  const send = (method, params) => browser.send(method, params, sessionId);
  const emit = (e) => {
    for (const h of handlers) h(e);
  };
  const remove = browser.listen((m) => {
    if (!sessionId || m.sessionId !== sessionId) return;
    const p = m.params;
    if (m.method === 'Runtime.consoleAPICalled') emit({ kind: 'console', type: p.type, text: p.args.map(text).join(' ') });
    else if (m.method === 'Runtime.exceptionThrown') {
      const d = p.exceptionDetails;
      emit({ kind: 'exception', text: d.exception?.description ?? d.text });
    } else if (m.method === 'Log.entryAdded') emit({ kind: 'log', level: p.entry.level, text: p.entry.text, url: p.entry.url });
    else if (m.method === 'Runtime.bindingCalled' && bindings.has(p.name)) bindings.get(p.name)(p.payload);
  });
  const page = {
    goto: (url) => send('Page.navigate', { url }),
    evaluate: async (expression) => {
      const r = await send('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
      if (r.exceptionDetails) {
        const d = r.exceptionDetails;
        throw new Error(d.exception?.description ?? d.text);
      }
      return r.result.value;
    },
    input: (method, params) => send(method, params),
    bind: (name, f) => {
      bindings.set(name, f);
      return send('Runtime.addBinding', { name });
    },
    initScript: (source) => send('Page.addScriptToEvaluateOnNewDocument', { source }),
    on: (f) => handlers.push(f),
  };
  const limit = new Promise((_, ko) => {
    timer = setTimeout(() => ko(new Error(`no result in ${limitMs} ms`)), limitMs);
  });
  const run = (async () => {
    ({ targetId } = await browser.send('Target.createTarget', { url: 'about:blank' }));
    ({ sessionId } = await browser.send('Target.attachToTarget', { targetId, flatten: true }));
    await send('Runtime.enable');
    await send('Log.enable');
    await send('Page.enable');
    return body(page);
  })();
  // A failure of the run after the limit is handled, thus node does not stop.
  run.catch(() => {});
  try {
    return await Promise.race([run, limit, browser.whenGone]);
  } finally {
    clearTimeout(timer);
    remove();
    if (targetId && !browser.gone()) await browser.send('Target.closeTarget', { targetId }).catch(() => {});
  }
};

// The browser of browser.mjs: Chrome at the path exe. options.noWasm gives
// --js-flags=--jitless: with no JIT, Chrome has no WebAssembly (as Chrome with
// "JavaScript optimization" off).
export const start = async (exe, options = {}) => {
  const browser = launch(exe, options.noWasm ? ['--js-flags=--jitless'] : []);
  return {
    withPage: (limitMs, body) => withPage(browser, limitMs, body),
    close: () => browser.close(),
  };
};
