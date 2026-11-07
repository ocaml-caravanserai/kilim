// The Chrome part of the runners of test/dom, test/loader and test/examples:
// find Chrome, start it with a minimal environment, and talk to it over the
// DevTools pipe (no port, no WebSocket). It also collects the events of a
// target and polls a condition. The import has no side effect: the caller
// decides what a missing Chrome gives (the skip rules of
// KILIM_REQUIRE_CHROME).

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
//   close()                           stops Chrome and removes its directory
export const launch = (chrome, flags = []) => {
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
  const close = () => {
    proc.kill();
    try { rmSync(home, { recursive: true, force: true }); } catch {}
  };
  return { send, listen, whenGone, gone: () => gone, close };
};

// Runs body(page) in a new target, with one time limit for the whole run, from
// the creation of the target. page.send sends a command to the target, and
// page.on(f) gives each event of the target to f. The result is the result of
// body. It rejects at the time limit, when Chrome exits, or when body rejects.
// At the end, the target closes.
export const withPage = async (browser, limitMs, body) => {
  let sessionId;
  let targetId;
  let timer;
  const handlers = new Set();
  const page = {
    send: (method, params) => browser.send(method, params, sessionId),
    on: (f) => handlers.add(f),
  };
  const remove = browser.listen((m) => {
    if (sessionId && m.sessionId === sessionId) for (const h of handlers) h(m);
  });
  const limit = new Promise((_, ko) => {
    timer = setTimeout(() => ko(new Error(`no result in ${limitMs} ms`)), limitMs);
  });
  const run = (async () => {
    ({ targetId } = await browser.send('Target.createTarget', { url: 'about:blank' }));
    ({ sessionId } = await browser.send('Target.attachToTarget', { targetId, flatten: true }));
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

// The text of a console argument of the protocol.
export const text = (a) => (a.value !== undefined ? String(a.value) : a.description ?? a.type);

// Enables the Runtime, Log and Page domains of a target of withPage, and
// collects its events. The result has these lists, which grow while the
// target lives:
//   warnings, errors   the text of each console.warn and console.error call
//   exceptions         each uncaught exception
//   logs               each Chrome log entry: { level, text, url }
export const watch = async (page) => {
  const events = { warnings: [], errors: [], exceptions: [], logs: [] };
  page.on((m) => {
    if (m.method === 'Runtime.consoleAPICalled') {
      const line = m.params.args.map(text).join(' ');
      if (m.params.type === 'warning') events.warnings.push(line);
      else if (m.params.type === 'error') events.errors.push(line);
    } else if (m.method === 'Runtime.exceptionThrown') {
      const d = m.params.exceptionDetails;
      events.exceptions.push(d.exception?.description ?? d.text);
    } else if (m.method === 'Log.entryAdded') {
      const e = m.params.entry;
      events.logs.push({ level: e.level, text: e.text, url: e.url });
    }
  });
  await page.send('Runtime.enable');
  await page.send('Log.enable');
  await page.send('Page.enable');
  return events;
};

// Calls test every 20 ms until it gives a true value. The result is true, or
// false after limitMs. A test that throws or rejects gives false for that
// call: for example, a read of the page during a navigation.
export const until = async (test, limitMs) => {
  const end = Date.now() + limitMs;
  for (;;) {
    if (await Promise.resolve().then(test).catch(() => false)) return true;
    if (Date.now() > end) return false;
    await new Promise((ok) => setTimeout(ok, 20));
  }
};
