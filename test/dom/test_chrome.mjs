// The checks of the Chrome back end (chrome.mjs), with no page of a program:
//
//   C1  a crash of the renderer while an evaluate waits rejects withPage
//       with "the renderer of the page crashed" in less than 5 s after
//       Page.crash, not at the time limit
//   C2  the message of the time limit gives the commands with no reply
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
  // C1. Chrome does not reply to the evaluate or to Page.crash. The setup
  // of a page can take some seconds on a slow machine (9 s on a CI runner),
  // thus the time limit is 60 s and the permitted time (5 s) starts when the
  // runner sends Page.crash.
  let crashSent;
  const c1 = await outcome(browser.withPage(60000, async (page) => {
    const result = page.evaluate('new Promise(() => {})');
    crashSent = Date.now();
    page.input('Page.crash').catch(() => {});
    return result;
  }));
  const ms = crashSent === undefined ? undefined : Date.now() - crashSent;
  check('C1', c1.error === 'the renderer of the page crashed' && ms < 5000,
    `${JSON.stringify(c1)}, ${ms} ms after Page.crash`);

  // C2. The time limit counts from the start of the page. If the setup of
  // the page is longer than the limit, the command with no reply is a
  // command of the setup. Thus the check expects Runtime.evaluate only when
  // the body started.
  let started = false;
  const c2 = await outcome(browser.withPage(3000, (page) => {
    started = true;
    return page.evaluate('new Promise(() => {})');
  }));
  const form = /^no result in 3000 ms \(no reply: [A-Za-z]+\.[A-Za-z]+, sent at \d+ ms(; [A-Za-z]+\.[A-Za-z]+, sent at \d+ ms)*\)$/;
  const e2 = c2.error ?? '';
  check('C2', form.test(e2) && (!started || e2.includes('(no reply: Runtime.evaluate, sent at ')), JSON.stringify(c2));

  // C3
  const c3 = await outcome(browser.withPage(10000, (page) => page.evaluate('1 + 1')));
  check('C3', c3.value === 2, JSON.stringify(c3));
} finally {
  await browser.close();
}
console.log(failures === 0 ? 'all 3 checks pass' : `${failures} checks FAILED`);
process.exit(failures === 0 ? 0 : 1);
