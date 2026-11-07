// The Chrome runner of the loader tests (docs/design.html, "The loader"). It
// uses only the built-in modules of node, and test/dom/chrome.mjs.
//
//   node run.mjs --kilim PATH --profile dev [--only a,b] [-- chrome flags]
//
// For each scenario, the runner writes a template to a temporary directory,
// and runs `kilim page` with the base /sN/ (N: the number of the scenario).
// Thus each scenario also tests the commands. The server answers:
//   /sN/x/NAME      a fixture of the scenario (CSS, page scripts), with an
//                   optional delay
//   /sN/loader.js   the output of `kilim loader` (the loader.js form)
//   /sN/FILE        a file of the build directory (the current directory)
//   /sN/...         else the page (a deep link)
// The server records each request with its time from the page request. The
// page can send its head at once and its body later, and its CSP in a header.
//
// Each response has "cache-control: no-cache", as a dev server.
//
// Page scripts before the head part: the fixtures of the scenario, then
// count.js. count.js counts the securitypolicyviolation events, the calls of
// WebAssembly.validate (the probes) and the calls of
// trustedTypes.createPolicy("kilim-loader"). A fixture that replaces
// WebAssembly.validate comes before count.js, thus count.js counts the calls.
//
// A scenario waits until window.kilimLoaded has each app of the scenario
// (test/loader/app.ml), or 2 s for a scenario with no start. Then it clicks
// the button of each app and reads its count. "Once": each app starts one
// time, and each URL gets at most one request (thus the inserted build used
// the preload). Each scenario expects no uncaught exception, no warning and no
// console error, except where it says so.
//
// Chrome: see chrome.mjs. With no Chrome, the runner writes "SKIP: no Chrome"
// and gives exit status 0, but 1 if KILIM_REQUIRE_CHROME is 1 (CI).

import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, normalize } from 'node:path';
import http from 'node:http';
import { findChrome, launch, text, withPage } from '../dom/chrome.mjs';

const argv = process.argv.slice(2);
const dash = argv.indexOf('--');
const own = dash < 0 ? argv : argv.slice(0, dash);
const flags = dash < 0 ? [] : argv.slice(dash + 1);
const opt = (name) => {
  const i = own.indexOf(name);
  return i < 0 ? undefined : own[i + 1];
};
const kilim = opt('--kilim');
const profile = opt('--profile');
const only = opt('--only')?.split(',');
if (!kilim || profile !== 'dev') {
  console.log('usage: node run.mjs --kilim PATH --profile dev [--only a,b] [-- chrome flags]');
  process.exit(2);
}

// The CSP source expression of the loader text.
const hash = execFileSync(kilim, ['csp']).toString().trim();
const ttCsp = `script-src 'self' 'wasm-unsafe-eval' ${hash}; require-trusted-types-for 'script'; ` +
  'trusted-types kilim-loader app-html';
const cspNoWasm = `script-src 'self' ${hash}`;

// The fixtures: page scripts and stylesheets.
const js = (body, delay = 0) => ({ type: 'text/javascript', body, delay });
const css = (body, delay = 0) => ({ type: 'text/css', body, delay });
const fixtures = {
  'count.js': js(`(function () {
  var w = window;
  w.kilimReports = [];
  document.addEventListener('securitypolicyviolation', function (e) { w.kilimReports.push(e.violatedDirective); });
  w.kilimProbes = 0;
  if (typeof WebAssembly == 'object') {
    var v = WebAssembly.validate;
    WebAssembly.validate = function () { w.kilimProbes++; return v.apply(this, arguments); };
  }
  w.kilimPolicies = 0;
  if (w.trustedTypes) {
    var c = w.trustedTypes.createPolicy;
    w.trustedTypes.createPolicy = function (name) {
      if (name == 'kilim-loader') w.kilimPolicies++;
      return c.apply(this, arguments);
    };
  }
})();`),
  'no-jstag.js': js('delete WebAssembly.JSTag;'),
  'no-wasm.js': js('delete window.WebAssembly;'),
  'no-gc.js': js('WebAssembly.validate = function () { return false; };'),
  'no-tt.js': js("Object.defineProperty(window, 'trustedTypes', { value: undefined, configurable: true });"),
  'app-html.js': js("window.kilimTestHtml = trustedTypes.createPolicy('app-html', " +
    "{ createHTML: function (s) { return s; } }).createHTML('<i>x</i>');"),
  'slow.js': js('window.slowRan = true;', 2000),
  'slow.css': css('#a { height: 300px; }', 1500),
  'fast.css': css('#a { height: 300px; }'),
  'import.css': css('@import url(slow.css);'),
  'other.css': css('#a { height: 10px; }', 2000),
  'insert.js': js("var l = document.createElement('link'); l.rel = 'stylesheet'; " +
    "l.href = document.currentScript.src.replace('insert.js', 'other.css'); document.head.appendChild(l);"),
};

// The scenarios. The fields:
//   apps      the parts: [app, marker name] (default: app a, no name)
//   form      'inline' (default) or 'file'
//   build     'js': --build js
//   pre       fixtures before count.js
//   csp, cspIn  the CSP, in a 'header' (default) or a 'meta'
//   before    HTML after count.js, before the head marker
//   after     HTML after the head marker
//   between   HTML between the two tags of the loader.js form
//   end       HTML after the start marker
//   nest      the start marker in a div at the end of the body
//   noStart   the page has no start part
//   headDelay the async tag loads loader.js?head, 500 ms late
//   bodyDelay the server sends the body this many ms after the head
//   url       the path of the page after /sN/
//   expect    the checks (see check)
const tt = { csp: ttCsp, after: '<script src="x/app-html.js"></script>' };
const scenarios = [
  { name: 'inline', expect: { build: 'wasm', path: 'start', probes: 1, reports: 0 } },
  { name: 'file', form: 'file', expect: { build: 'wasm', path: 'start', requests: { 'loader.js': 1 } } },
  { name: 'no-jstag', pre: ['no-jstag.js'], expect: { build: 'js', none: /wasm/ } },
  { name: 'no-wasm', pre: ['no-wasm.js'], expect: { build: 'js' } },
  { name: 'no-gc', pre: ['no-gc.js'], expect: { build: 'js', probes: 1 } },
  { name: 'csp-header', csp: cspNoWasm, expect: { build: 'js', reports: 1 } },
  { name: 'csp-meta', csp: cspNoWasm, cspIn: 'meta', expect: { build: 'js', reports: 1 } },
  { name: 'csp-wasm', csp: `script-src 'self' 'wasm-unsafe-eval' ${hash}`, expect: { build: 'wasm', reports: 0 } },
  { name: 'csp-build-js', csp: cspNoWasm, build: 'js', expect: { build: 'js', reports: 0, probes: 0 } },
  { name: 'csp-bad-hash', csp: "script-src 'self' 'sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU='",
    expect: { start: false } },
  { name: 'tt-inline', ...tt, expect: { build: 'wasm', path: 'start', html: '<i>x</i>', reports: 0 } },
  { name: 'tt-handoff', ...tt, form: 'file', headDelay: true,
    expect: { build: 'wasm', path: 'head', html: '<i>x</i>', probes: 1, reports: 0 } },
  { name: 'tt-plain', csp: ttCsp, expect: { build: 'wasm', text: 'fallback', errors: 1, reports: 1 } },
  { name: 'tt-names', csp: `script-src 'self' 'wasm-unsafe-eval' ${hash}; trusted-types other`,
    expect: { build: 'wasm', reports: 1, policies: 1 } },
  { name: 'tt-gadget', ...tt, expect: { build: 'wasm', gadget: true } },
  { name: 'no-tt', pre: ['no-tt.js'], expect: { build: 'wasm', policies: 0 } },
  { name: 'two-inline', apps: [['a', 'a'], ['b', 'b']], expect: { build: 'wasm' } },
  { name: 'two-tt', ...tt, apps: [['a', 'a'], ['b', 'b']],
    expect: { build: 'wasm', html: '<i>x</i>', reports: 0, policies: 1 } },
  { name: 'deep-inline', url: 'users/42', expect: { build: 'wasm' } },
  { name: 'late-inline', bodyDelay: 300, expect: { build: 'wasm' } },
  { name: 'defer-inline', after: '<script src="x/slow.js" defer></script>', expect: { startBefore: 1000, slowRan: false } },
  { name: 'defer-file', form: 'file', after: '<script src="x/slow.js" defer></script>',
    expect: { startBefore: 1000, slowRan: false } },
  { name: 'css-inline', after: '<link rel="stylesheet" href="x/slow.css">',
    expect: { height: 300, buildBefore: 300 } },
  { name: 'css-file', form: 'file', after: '<link rel="stylesheet" href="x/slow.css">',
    expect: { height: 300, buildBefore: 300 } },
  { name: 'css-import', after: '<link rel="stylesheet" href="x/import.css">', expect: { height: 300 } },
  { name: 'css-failed', after: '<link rel="stylesheet" href="x/none.css"><link rel="stylesheet" href="x/fast.css">',
    expect: { height: 300, startBefore: 1000 } },
  { name: 'css-charset', after: '<link rel="stylesheet" type="text/css; charset=utf-8" href="x/slow.css">',
    expect: { height: 300 } },
  { name: 'css-other',
    after: '<link rel="stylesheet" media="print" href="x/other.css?print">' +
      '<link rel="alternate stylesheet" title="alt" href="x/other.css?alternate">' +
      '<link rel="stylesheet" disabled href="x/other.css?disabled"><link rel="stylesheet" href="">' +
      '<script src="x/insert.js"></script>',
    expect: { startBefore: 1000 } },
  { name: 'warn-css', before: '<link rel="stylesheet" href="x/fast.css">', expect: { warnings: 1 } },
  { name: 'warn-meta', before: '<meta http-equiv="content-security-policy" content="img-src *">',
    after: '<meta http-equiv="Content-Security-Policy" content="img-src *">', expect: { warnings: 1 } },
  { name: 'warn-start', end: '<div></div>', expect: { warnings: 1 } },
  { name: 'warn-nest', nest: true, expect: { warnings: 1 } },
  { name: 'warn-none', noStart: true, expect: { start: false, warnings: 1 } },
  { name: 'warn-file', form: 'file', between: '<meta name="x">', expect: { warnings: 1 } },
  { name: 'warn-late', form: 'file', between: '<meta name="x">', headDelay: true,
    expect: { path: 'head', warnings: 1 } },
  { name: 'dup', apps: [['a', 'x'], ['a', 'y']], expect: { build: 'wasm' } },
  { name: 'rule', prebuilt: true, expect: { build: 'wasm', path: 'start' } },
  { name: 'no-wasm-real', alone: true, expect: { build: 'js' } },
];

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

const builds = process.cwd();
const work = mkdtempSync(join(tmpdir(), 'kilim-loader-'));

// The template of a scenario, with its markers.
const marker = (kind, name) => `<!--kilim:${kind}${name ? ':' + name : ''}-->`;
const template = (sc, prefix) => {
  const apps = sc.apps || [['a']];
  const mounts = [...new Set(apps.map(([a]) => a))];
  const cspMeta = sc.csp && sc.cspIn === 'meta' ? `<meta http-equiv="content-security-policy" content="${sc.csp}">` : '';
  const pre = (sc.pre || []).concat(['count.js']).map((f) => `<script src="x/${f}"></script>`).join('');
  // The fixtures are relative to the base, also on a deep link.
  const x = (h) => h.replaceAll('"x/', `"${prefix}x/`);
  return '<!doctype html><html><head><meta charset="utf-8">' + cspMeta + x(pre + (sc.before || '')) +
    apps.map(([, n]) => marker('head', n)).join('') + x(sc.after || '') + `<title>${sc.name}</title></head>` +
    '<body>' + mounts.map((m) => `<div id="${m}"></div>`).join('') +
    (sc.nest ? '<div>' : '') + apps.map(([, n]) => marker('start', n)).join('') + (sc.nest ? '</div>' : '') +
    (sc.end || '') + '</body></html>';
};

// The page of a scenario: one run of `kilim page` for each app, each on the
// output of the run before it.
const makePage = (sc, prefix, dir) => {
  if (sc.prebuilt) return readFileSync(join(builds, 'index.html'), 'utf8');
  let input = join(dir, 'template.html');
  writeFileSync(input, template(sc, prefix));
  (sc.apps || [['a']]).forEach(([app, name], i) => {
    const out = join(dir, `page${i}.html`);
    const build = sc.build === 'js' ? ['--build', 'js'] : ['--wasm', `app_${app}.bc.wasm.js`];
    execFileSync(kilim, ['page', '--template', input, '-o', out, '--base', prefix, '--js', `app_${app}.bc.js`,
      ...build, ...(sc.form === 'file' ? ['--form', 'file'] : []), ...(name ? ['--name', name] : [])]);
    input = out;
  });
  let page = readFileSync(input, 'utf8');
  if (sc.noStart) page = page.replace(/<script[^>]* data-start>[\s\S]*?<\/script>/, '');
  if (sc.form === 'file') {
    execFileSync(kilim, ['loader', '-o', join(dir, 'loader.js')]);
    const asyncTag = `<script src="${prefix}loader.js" async`;
    if (sc.headDelay) page = page.replace(asyncTag, `<script src="${prefix}loader.js?head" async`);
    const deferTag = `<script src="${prefix}loader.js" defer`;
    if (sc.between) page = page.replace(deferTag, sc.between + deferTag);
  }
  return page;
};

// The server. Each scenario has a state: its page, its directory, its
// request log and the time of its page request.
const sites = new Map();
const types = { '.js': 'text/javascript', '.wasm': 'application/wasm', '.map': 'application/json' };
const server = http.createServer(async (req, res) => {
  const url = new URL(req.url, 'http://x');
  const m = url.pathname.match(/^\/(s\d+|rule)\/(.*)$/);
  const site = m && sites.get(m[1]);
  if (!site) {
    res.writeHead(404);
    res.end();
    return;
  }
  const rest = m[2];
  const sc = site.sc;
  const send = (status, type, body) => {
    res.writeHead(status, { 'content-type': type, 'cache-control': 'no-cache' });
    res.end(body);
  };
  const file = (base, name) => {
    const f = normalize(join(base, name));
    if (!f.startsWith(base + '/')) return null;
    try {
      return statSync(f).isFile() ? f : null;
    } catch {
      return null;
    }
  };
  const fixture = rest.startsWith('x/') && fixtures[rest.slice(2)];
  const loader = rest === 'loader.js' && sc.form === 'file' ? join(site.dir, 'loader.js') : null;
  const built = !fixture && !loader && rest !== '' && file(builds, rest);
  if (!fixture && !loader && !built) {
    if (rest.startsWith('x/')) {
      site.log.push({ path: rest + url.search, t: Date.now() - site.t0 });
      send(404, 'text/plain', '');
      return;
    }
    // The page.
    site.t0 = Date.now();
    const headers = { 'content-type': 'text/html; charset=utf-8', 'cache-control': 'no-cache' };
    if (sc.csp && sc.cspIn !== 'meta') headers['content-security-policy'] = sc.csp;
    res.writeHead(200, headers);
    if (sc.bodyDelay) {
      const i = site.page.indexOf('<body');
      res.write(site.page.slice(0, i));
      await new Promise((ok) => setTimeout(ok, sc.bodyDelay));
      res.end(site.page.slice(i));
    } else res.end(site.page);
    return;
  }
  site.log.push({ path: rest + url.search, t: Date.now() - site.t0 });
  if (fixture) {
    if (fixture.delay) await new Promise((ok) => setTimeout(ok, fixture.delay));
    send(200, fixture.type, fixture.body);
  } else if (loader) {
    if (url.search === '?head') await new Promise((ok) => setTimeout(ok, 500));
    send(200, 'text/javascript', readFileSync(loader));
  } else {
    const ext = built.slice(built.lastIndexOf('.'));
    send(200, types[ext] || 'application/octet-stream', readFileSync(built));
  }
});
await new Promise((ok) => server.listen(0, '127.0.0.1', ok));
const origin = `http://127.0.0.1:${server.address().port}`;

const browser = launch(chrome, flags);
const sleep = (ms) => new Promise((ok) => setTimeout(ok, ms));

// The state of the page, as JSON.
const stateJs = `JSON.stringify({
  loaded: window.kilimLoaded || {},
  probes: window.kilimProbes,
  reports: window.kilimReports,
  policies: window.kilimPolicies,
  path: document.documentElement.getAttribute('data-kilim-started'),
})`;

// The click on the button of each app, then the count and the content of the
// HTML node of each app (or the text of the mount element).
const clickJs = (ids) => `new Promise((ok) => {
  const ids = ${JSON.stringify(ids)};
  for (const id of ids) document.querySelector('#' + id + ' [data-testid=b]')?.click();
  setTimeout(() => ok(JSON.stringify(ids.map((id) => {
    const m = document.getElementById(id);
    return { count: m.querySelector('[data-testid=b]')?.textContent, html: m.querySelector('[data-testid=h]')?.innerHTML, text: m.textContent };
  }))), 50);
})`;

const gadgetJs = `(() => {
  try {
    document.documentElement.__kilimLoader.policy.createScriptURL('https://example.invalid/x.js');
    return 'accepted';
  } catch (e) {
    return 'refused';
  }
})()`;

const runScenario = async (sc, n) => {
  const key = sc.prebuilt ? 'rule' : `s${n}`;
  const prefix = `/${key}/`;
  const dir = join(work, key);
  mkdirSync(dir, { recursive: true });
  const site = { sc, dir, log: [], t0: Date.now(), page: makePage(sc, prefix, dir) };
  sites.set(key, site);
  const ids = [...new Set((sc.apps || [['a']]).map(([a]) => a))];
  const e = sc.expect;
  const events = { warnings: [], errors: [], exceptions: [] };
  const faults = [];
  const fault = (s) => faults.push(s);
  // The start time of each app, for the log: the time bounds have a margin.
  let starts = '';
  await withPage(browser, 30000, async (page) => {
    page.on((m) => {
      if (m.method === 'Runtime.consoleAPICalled') {
        const line = m.params.args.map(text).join(' ');
        if (m.params.type === 'warning') events.warnings.push(line);
        else if (m.params.type === 'error') events.errors.push(line);
      } else if (m.method === 'Runtime.exceptionThrown') {
        const d = m.params.exceptionDetails;
        events.exceptions.push(d.exception?.description ?? d.text);
      }
    });
    await page.send('Runtime.enable');
    await page.send('Page.enable');
    await page.send('Page.navigate', { url: origin + prefix + (sc.url || '') });
    const read = async (expr, awaitPromise = false) =>
      (await page.send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise })).result.value;
    let st;
    if (e.start === false) {
      await sleep(2000);
      st = JSON.parse(await read(stateJs));
      if (Object.keys(st.loaded).length) fault('an app started');
      return;
    }
    const until = Date.now() + 10000;
    for (;;) {
      // A read during the navigation can fail: then the poll goes on.
      st = JSON.parse((await read(stateJs).catch(() => undefined)) ?? '{"loaded":{}}');
      if (ids.every((id) => st.loaded[id]?.starts === 1)) break;
      if (Date.now() > until) {
        fault(`no start in 10 s: ${JSON.stringify(st.loaded)}`);
        return;
      }
      await sleep(20);
    }
    // Late requests and late errors.
    await sleep(100);
    st = JSON.parse(await read(stateJs));
    starts = ids.map((id) => `${id} ${Math.round(st.loaded[id].time)} ms`).join(', ');
    const views = JSON.parse(await read(clickJs(ids), true));
    ids.forEach((id, i) => {
      const r = st.loaded[id];
      const v = views[i];
      if (e.build && r.build !== e.build) fault(`${id}: build ${r.build}, expected ${e.build}`);
      if (r.starts !== 1) fault(`${id}: ${r.starts} starts`);
      if (e.text !== undefined) {
        if (v.text !== e.text) fault(`${id}: text ${JSON.stringify(v.text)}, expected ${e.text}`);
      } else {
        if (v.count !== '1') fault(`${id}: count ${v.count} after a click, expected 1`);
        if (v.html !== (e.html ?? '<i>x</i>')) fault(`${id}: html ${v.html}`);
      }
      if (e.height !== undefined && r.height !== e.height) fault(`${id}: height ${r.height}, expected ${e.height}`);
      if (e.startBefore !== undefined && !(r.time < e.startBefore))
        fault(`${id}: start at ${Math.round(r.time)} ms, expected before ${e.startBefore} ms`);
      if (e.slowRan !== undefined && r.slowRan !== e.slowRan) fault(`${id}: slowRan ${r.slowRan}`);
    });
    for (const k of ['probes', 'policies', 'path'])
      if (e[k] !== undefined && st[k] !== e[k]) fault(`${k}: ${st[k]}, expected ${e[k]}`);
    if (e.reports !== undefined && st.reports.length !== e.reports)
      fault(`reports: ${JSON.stringify(st.reports)}, expected ${e.reports}`);
    if (e.gadget && (await read(gadgetJs)) !== 'refused') fault('the policy accepts another URL');
  });
  // The requests: each URL at most one time, and the expected counts.
  const counts = {};
  for (const r of site.log) counts[r.path] = (counts[r.path] || 0) + 1;
  if (e.start !== false) {
    for (const [p, c] of Object.entries(counts)) if (c > 1) fault(`${c} requests for ${p}`);
    for (const [p, c] of Object.entries(e.requests || {}))
      if ((counts[p] || 0) !== c) fault(`${counts[p] || 0} requests for ${p}, expected ${c}`);
    if (e.none) for (const p of Object.keys(counts)) if (e.none.test(p)) fault(`a request for ${p}`);
    if (e.buildBefore !== undefined) {
      const b = site.log.find((r) => /^app_a\.bc\.(wasm\.)?js$/.test(r.path));
      if (!b || !(b.t < e.buildBefore)) fault(`the build request at ${b?.t} ms, expected before ${e.buildBefore} ms`);
    }
  }
  const warnings = events.warnings.length;
  if (warnings !== (e.warnings ?? 0)) fault(`${warnings} warnings: ${JSON.stringify(events.warnings)}`);
  if (events.errors.length !== (e.errors ?? 0)) fault(`${events.errors.length} console errors: ${JSON.stringify(events.errors)}`);
  if (events.exceptions.length) fault(`exceptions: ${JSON.stringify(events.exceptions)}`);
  sites.delete(key);
  return { faults, log: site.log, starts };
};

const chosen = scenarios.filter((sc) => (only ? only.includes(sc.name) : !sc.alone));
if (only) for (const o of only) if (!scenarios.some((sc) => sc.name === o)) {
  console.log(`FAIL: no scenario ${o}`);
  process.exit(1);
}
let failures = 0;
try {
  for (const sc of chosen) {
    const n = scenarios.indexOf(sc);
    let r;
    try {
      r = await runScenario(sc, n);
    } catch (e) {
      r = { faults: [e.message], log: [] };
    }
    if (r.faults.length) {
      failures++;
      console.log(`FAIL ${sc.name}: ${r.faults.join('; ')}`);
      console.log(`  requests: ${r.log.map((x) => `${x.path}@${x.t}`).join(' ')}`);
    } else console.log(`ok   ${sc.name}${r.starts ? ` (start: ${r.starts})` : ''}`);
  }
} finally {
  browser.close();
  server.close();
  server.closeAllConnections();
  rmSync(work, { recursive: true, force: true });
}
console.log(failures === 0 ? `all ${chosen.length} scenarios pass` : `${failures} scenarios FAILED`);
process.exit(failures === 0 ? 0 : 1);
