// The browser interface of the runners of test/dom, test/loader and
// test/examples. The runners use only this interface. It has two back ends:
// chrome.mjs (Chrome, over the DevTools pipe) and playwright.mjs (Firefox and
// WebKit). The import has no side effect.
//
//   pick(name)                 the back end of the browser NAME (chrome,
//                              firefox or webkit), or null if the browser is
//                              not on this machine. A configuration that is
//                              not valid (for example a KILIM_CHROME that is
//                              not an executable) throws.
//   backend.launch(options)    starts the browser, a promise of a browser.
//                              options.noWasm: a browser with no WebAssembly
//                              (Chrome: --js-flags=--jitless, Firefox: a
//                              preference, WebKit: simulated, see
//                              playwright.mjs).
//   browser.withPage(ms, body) runs body(page) in a new page, with one time
//                              limit for the whole run. At the end, the page
//                              closes.
//   browser.close()            stops the browser.
//   page.goto(url)             starts a navigation. The promise does not wait
//                              for the body of the page.
//   page.evaluate(expr)        the value of a JS expression. The promise waits
//                              for a promise that the expression gives. An
//                              exception rejects.
//   page.input(method, params) a trusted input command, with the method and
//                              the parameters of DevTools (Input.*).
//   page.bind(name, f)         a function window.NAME of the page: a call
//                              with a string calls f with that string.
//   page.initScript(source)    a script that runs before each script of the
//                              next documents.
//   page.on(f)                 f gets each event of the page:
//                                {kind: 'console', type, text}   a console
//                                  call of the page (type: log, warning,
//                                  error, ...)
//                                {kind: 'exception', text}       an uncaught
//                                  exception: its first line is "Name: message"
//                                {kind: 'log', level, text, url} a message of
//                                  the browser
//
// skip(name) writes the skip line of a missing browser, then the process
// exits. watch(page) collects the events of a page. until(test, ms) polls a
// condition.

import * as chrome from './chrome.mjs';
import * as playwright from './playwright.mjs';

export const names = ['chrome', 'firefox', 'webkit'];

export const pick = async (name) => {
  if (name === 'chrome') {
    const exe = chrome.findChrome();
    return exe && { name, launch: (options = {}) => chrome.start(exe, options) };
  }
  if (names.includes(name)) {
    const pw = await playwright.load();
    return pw && { name, launch: (options = {}) => playwright.start(pw, name, options) };
  }
  throw new Error(`unknown browser ${name}: give one of ${names.join(', ')}`);
};

// "SKIP: no Chrome" or "SKIP: no Playwright", and exit status 0, but 1 if
// KILIM_REQUIRE_CHROME or KILIM_REQUIRE_PLAYWRIGHT is 1 (CI).
export const skip = (name) => {
  const [what, variable] = name === 'chrome' ? ['Chrome', 'KILIM_REQUIRE_CHROME'] : ['Playwright', 'KILIM_REQUIRE_PLAYWRIGHT'];
  console.log(`SKIP: no ${what}`);
  process.exit(process.env[variable] === '1' ? 1 : 0);
};

// The back end of the browser NAME. A configuration that is not valid writes
// FAIL and gives exit status 1. A missing browser is a skip. cleanup runs
// before the process exits.
export const open = async (name, cleanup = () => {}) => {
  let backend;
  try {
    backend = await pick(name);
  } catch (e) {
    cleanup();
    console.log(`FAIL: ${e.message}`);
    process.exit(1);
  }
  if (!backend) {
    cleanup();
    skip(name);
  }
  return backend;
};

// Collects the events of a page of withPage. The result has these lists,
// which grow while the page lives:
//   warnings, errors   the text of each console.warn and console.error call
//   exceptions         each uncaught exception
//   logs               each message of the browser: { level, text, url }
export const watch = (page) => {
  const events = { warnings: [], errors: [], exceptions: [], logs: [] };
  page.on((e) => {
    if (e.kind === 'console') {
      if (e.type === 'warning') events.warnings.push(e.text);
      else if (e.type === 'error') events.errors.push(e.text);
    } else if (e.kind === 'exception') events.exceptions.push(e.text);
    else if (e.kind === 'log') events.logs.push({ level: e.level, text: e.text, url: e.url });
  });
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
