// The Playwright back end of the browser interface (browser.mjs): Firefox and
// WebKit. The package playwright-core and the two browsers come from nixpkgs
// (the dev shell `browsers` of flake.nix). KILIM_PLAYWRIGHT is the directory
// of playwright-core, and PLAYWRIGHT_BROWSERS_PATH the directory of the
// browsers. No npm, and no network at test time. The nix browsers do not use
// the libraries of the system, thus the back end does not change their
// environment. The import has no side effect.
//
// Playwright translates the DevTools input commands of the pages (input).
// Neither Firefox nor WebKit has a composition command in Playwright, thus
// Input.imeSetComposition is an error.

import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

// The module playwright-core, or null with no KILIM_PLAYWRIGHT. A
// KILIM_PLAYWRIGHT with no index.mjs throws: it is a fault of the
// configuration, not a missing Playwright.
export const load = async () => {
  const dir = process.env.KILIM_PLAYWRIGHT;
  if (!dir) return null;
  const index = join(dir, 'index.mjs');
  if (!existsSync(index)) throw new Error(`KILIM_PLAYWRIGHT has no index.mjs: ${dir}`);
  return import(pathToFileURL(index).href);
};

// A console message is a call of the page if it has at least one argument
// and its text does not start with "[JavaScript Error:" or "[JavaScript
// Warning:". Else it is a message of the browser, as Log.entryAdded of
// Chrome. Firefox 157 gives its own messages as console messages with one
// argument and this prefix, and WebKit 27.2 with no argument. Thus a call of
// the page with this prefix, for example console.error('[JavaScript Error:
// ...'), or with no argument, for example console.log(), is a message of the
// browser in Firefox. WebKit gives no event for a console.log() with no
// argument. No page makes such a call. Firefox 157 also gives no message for
// a preload that the page does not use, and no message for a 404 of a
// stylesheet or an image: there, only the count of the requests finds a
// preload fault.
const browserMessage = (m) => m.args().length === 0 || /^\[JavaScript (Error|Warning):/.test(m.text());

// The trusted input of the DevTools commands of the pages.
const input = async (page, name, method, p) => {
  const mouse = page.mouse;
  if (method === 'Input.dispatchMouseEvent') {
    await mouse.move(p.x, p.y);
    const options = { button: p.button ?? 'left', clickCount: p.clickCount ?? 1 };
    if (p.type === 'mousePressed') await mouse.down(options);
    else if (p.type === 'mouseReleased') await mouse.up(options);
    else if (p.type === 'mouseWheel') await mouse.wheel(p.deltaX ?? 0, p.deltaY ?? 0);
    else if (p.type !== 'mouseMoved') throw new Error(`the mouse event ${p.type} is not supported in ${name}`);
  } else if (method === 'Input.dispatchKeyEvent') {
    // The key names of DevTools (Enter, ArrowDown) are the key names of
    // Playwright.
    if (p.type === 'rawKeyDown' || p.type === 'keyDown') await page.keyboard.down(p.key);
    else if (p.type === 'keyUp') await page.keyboard.up(p.key);
    else throw new Error(`the key event ${p.type} is not supported in ${name}`);
  } else if (method === 'Input.insertText') await page.keyboard.insertText(p.text);
  else throw new Error(`${method} is not supported in ${name}`);
};

// The page of browser.mjs.
const adapt = async (page, name) => {
  const handlers = [];
  const emit = (e) => {
    for (const h of handlers) h(e);
  };
  page.on('console', (m) => {
    if (browserMessage(m)) {
      const level = ['error', 'warning'].includes(m.type()) ? m.type() : 'info';
      emit({ kind: 'log', level, text: m.text(), url: m.location().url });
    } else emit({ kind: 'console', type: m.type(), text: m.text() });
  });
  // WebKit 27.2 gives an unhandled rejection with the name "Unhandled Promise
  // Rejection" and the text of the reason as the message.
  page.on('pageerror', (e) =>
    emit({ kind: 'exception', text: e.name === 'Unhandled Promise Rejection' ? e.message : `${e.name}: ${e.message}` }));
  return {
    goto: (url) => page.goto(url, { waitUntil: 'commit' }),
    evaluate: (expression) => page.evaluate(expression),
    input: (method, params) => input(page, name, method, params),
    bind: (binding, f) => page.exposeBinding(binding, (_source, payload) => f(payload)),
    initScript: (source) => page.addInitScript(source),
    on: (f) => handlers.push(f),
  };
};

// The browser of browser.mjs. options.noWasm: in Firefox, the preference
// javascript.options.wasm = false (a real switch). WebKit has no such switch:
// an init script deletes WebAssembly (simulated).
export const start = async (pw, name, options = {}) => {
  const prefs = name === 'firefox' && options.noWasm ? { 'javascript.options.wasm': false } : undefined;
  const browser = await pw[name].launch(prefs ? { firefoxUserPrefs: prefs } : {});
  let gone = null;
  browser.on('disconnected', () => {
    gone = new Error(`${name} exited`);
  });
  // Runs body(page) in a new context, with one time limit for the whole run.
  // It rejects at the time limit, when the browser exits or the page
  // crashes, or when body rejects. At the end, the context closes.
  const withPage = async (limitMs, body) => {
    if (gone) throw gone;
    let timer;
    let context;
    let stop;
    const failed = new Promise((_, ko) => {
      stop = ko;
      timer = setTimeout(() => ko(new Error(`no result in ${limitMs} ms`)), limitMs);
    });
    const onGone = () => stop(gone);
    browser.on('disconnected', onGone);
    const run = (async () => {
      context = await browser.newContext();
      if (name === 'webkit' && options.noWasm) await context.addInitScript('delete globalThis.WebAssembly;');
      const page = await context.newPage();
      page.on('crash', () => stop(new Error(`the page crashed in ${name}`)));
      return body(await adapt(page, name));
    })();
    // A failure of the run after the limit is handled, thus node does not stop.
    run.catch(() => {});
    try {
      return await Promise.race([run, failed]);
    } finally {
      clearTimeout(timer);
      browser.off('disconnected', onGone);
      if (context && !gone) await context.close().catch(() => {});
    }
  };
  return { withPage, close: () => browser.close().catch(() => {}) };
};
