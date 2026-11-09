// The checks of the Chrome back end (chrome.mjs), with no page of a program:
//
//   C1  a crash of the renderer while an evaluate waits rejects withPage at
//       once with "the renderer of the page crashed", not at the time limit
//   C2  the message of the time limit gives the command with no reply
//   C3  after C1 and C2, a new page of the same Chrome works
//
// With no Chrome, the skip line of browser.mjs.

import { open } from './browser.mjs';

const backend = await open('chrome');
const browser = await backend.launch();
let failures = 0;
const check = (name, ok, detail) => {
  console.log(`${name}: ${ok ? 'ok' : `FAIL (${detail})`}`);
  if (!ok) failures++;
};
const outcome = (p) => p.then((value) => ({ value }), (e) => ({ error: e.message }));

try {
  // C1. Chrome does not reply to the evaluate or to Page.crash. The time
  // limit (10 s) is longer than the permitted time (5 s).
  const t0 = Date.now();
  const c1 = await outcome(browser.withPage(10000, async (page) => {
    const result = page.evaluate('new Promise(() => {})');
    page.input('Page.crash').catch(() => {});
    return result;
  }));
  const ms = Date.now() - t0;
  check('C1', c1.error === 'the renderer of the page crashed' && ms < 5000, `${JSON.stringify(c1)} in ${ms} ms`);

  // C2
  const c2 = await outcome(browser.withPage(500, (page) => page.evaluate('new Promise(() => {})')));
  check('C2', /^no result in 500 ms \(no reply: Runtime\.evaluate, sent at \d+ ms\)$/.test(c2.error ?? ''),
    JSON.stringify(c2));

  // C3
  const c3 = await outcome(browser.withPage(10000, (page) => page.evaluate('1 + 1')));
  check('C3', c3.value === 2, JSON.stringify(c3));
} finally {
  await browser.close();
}
console.log(failures === 0 ? 'all 3 checks pass' : `${failures} checks FAILED`);
process.exit(failures === 0 ? 0 : 1);
