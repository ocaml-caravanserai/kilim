// The runner of the loader tests (docs/design.html, "The loader"). It uses
// only the built-in modules of node, and the browser interface
// test/dom/browser.mjs.
//
//   node run.mjs --kilim PATH --profile dev|release [--browser NAME]
//                [--no-wasm] [--only a,b] [--mutant FILE] [-- chrome flags]
//
// --browser NAME: chrome (the default). --no-wasm: a browser with no
// WebAssembly (browser.mjs). The flags after -- go to Chrome.
//
// --mutant FILE: the text of FILE replaces the loader text in each page and
// in loader.js, and the CSP has the hash of that text (node:crypto). Thus
// mutate.mjs can test a hand mutant of the loader.
//
// For each scenario, the runner writes a template to a temporary directory.
// In the dev profile, it runs `kilim page` with the base /sN/ (N: the number
// of the scenario), and `kilim loader` for the loader.js form. In the release
// profile, it runs `kilim dist` on the release builds into a directory of the
// scenario. Thus each scenario also tests the commands. The server answers:
//   /sN/x/NAME      a fixture of the scenario (CSS, page scripts), with an
//                   optional delay
//   /sN/loader.js   the output of `kilim loader` (dev, the loader.js form)
//   /sN/FILE        a file of the build directory (the current directory) in
//                   dev, or of the directory of `kilim dist` in release
//   /sN/...         else the page (a deep link)
// The server records each request with its time from the page request. The
// page can send its head at once and its body later, and its CSP in a header.
//
// In dev, each response has "cache-control: no-cache", as a dev server. In
// release, a file with a content name is immutable, and the rest has
// "no-cache".
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
// time, each URL gets at most one request (thus the inserted build used the
// preload), and Chrome logs no message about a preload. Each scenario expects
// no uncaught exception, no warning and no console error, except where it
// says so.
//
// A browser configuration that is not valid is a failure. With no browser,
// the runner writes the skip line of browser.mjs.

import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, normalize } from 'node:path';
import http from 'node:http';
import { open, until, watch } from '../dom/browser.mjs';

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
const name = opt('--browser') ?? 'chrome';
const noWasm = own.includes('--no-wasm');
const release = profile === 'release';
if (!kilim || !(release || profile === 'dev') || (flags.length && name !== 'chrome')) {
  console.log('usage: node run.mjs --kilim PATH --profile dev|release [--browser NAME] [--no-wasm] [--only a,b] ' +
    '[--mutant FILE] [-- chrome flags]');
  process.exit(2);
}

// @HASH@ in a CSP is the CSP source expression of the loader text.
const ttCsp = "script-src 'self' 'wasm-unsafe-eval' @HASH@; require-trusted-types-for 'script'; " +
  'trusted-types kilim-loader app-html';
const cspNoWasm = "script-src 'self' @HASH@";

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
//   inject    HTML at the start of the body
//   nest      the start marker in a div at the end of the body
//   headDelay the async tag loads the loader with "?head", 500 ms late
//   bodyDelay the server sends the body this many ms after the head
//   url       the path of the page after /sN/
//   release   a scenario of the release profile (the others are of dev)
//   repeat    a second visit of the page in the same target
//   expect    the checks, in runScenario
//
// In the loader.js form, the async head part can run before or after the
// defer start part: the order depends on the time of the requests. Thus a
// scenario of this form accepts the two paths, or forces the head path with
// headDelay. In the inline form, the head part always runs first.
const tt = { csp: ttCsp, after: '<script src="x/app-html.js"></script>' };
// The two parts of a copy of the loader, with no data-dev. @TEXT@ is the
// loader text, @OTHER@ the origin of the second server, and @PREFIX@ the
// base of the scenario.
const copy = (base, js) => ['', ' data-start'].map((start) =>
  `<script data-base="${base}" data-js="${js}" data-build="js"${start}>@TEXT@</script>`).join('');
const injected = copy('@OTHER@/', 'x.js') + copy('@PREFIX@', '@OTHER@/x.js');
const scenarios = [
  { name: 'inline', expect: { build: 'wasm', path: 'start', probes: 1, reports: 0, none: /undefined/ } },
  { name: 'file', form: 'file', expect: { build: 'wasm', path: ['head', 'start'], requests: { 'loader.js': 1 } } },
  { name: 'no-jstag', pre: ['no-jstag.js'], expect: { build: 'js', none: /wasm/ } },
  { name: 'no-wasm', pre: ['no-wasm.js'], expect: { build: 'js' } },
  { name: 'no-gc', pre: ['no-gc.js'], expect: { build: 'js', probes: 1 } },
  { name: 'csp-header', csp: cspNoWasm, expect: { build: 'js', reports: 1 } },
  { name: 'csp-meta', csp: cspNoWasm, cspIn: 'meta', expect: { build: 'js', reports: 1 } },
  { name: 'csp-wasm', csp: "script-src 'self' 'wasm-unsafe-eval' @HASH@", expect: { build: 'wasm', reports: 0 } },
  { name: 'csp-build-js', csp: cspNoWasm, build: 'js', expect: { build: 'js', reports: 0, probes: 0 } },
  { name: 'csp-bad-hash', csp: "script-src 'self' 'sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU='",
    expect: { start: false } },
  { name: 'tt-inline', ...tt, expect: { build: 'wasm', path: 'start', html: '<i>x</i>', reports: 0 } },
  { name: 'tt-handoff', ...tt, form: 'file', headDelay: true,
    expect: { build: 'wasm', path: 'head', html: '<i>x</i>', probes: 1, reports: 0 } },
  { name: 'tt-plain', csp: ttCsp, expect: { build: 'wasm', text: 'fallback', errors: 1, reports: 1 } },
  { name: 'tt-names', csp: "script-src 'self' 'wasm-unsafe-eval' @HASH@; trusted-types other",
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
  { name: 'warn-file', form: 'file', between: '<meta name="x">', expect: { warnings: 1 } },
  // The head part runs before the start part, and a slow stylesheet holds
  // the defer start part: no warning.
  { name: 'css-late', form: 'file', headDelay: true, after: '<link rel="stylesheet" href="x/slow.css">',
    expect: { path: 'start', height: 300 } },
  { name: 'warn-late', form: 'file', between: '<meta name="x">', headDelay: true,
    expect: { path: 'head', warnings: 1 } },
  // Two copies of the loader that a page injects, with a base or a build of
  // another origin: no request to that origin, and an error in each of the 4
  // parts (the head part throws at the preload, the start part at the start).
  { name: 'origin', inject: injected, expect: { build: 'wasm', exceptions: 4, exception: /another origin/ } },
  { name: 'origin-strict', inject: injected, csp: "script-src 'strict-dynamic' @HASH@",
    expect: { build: 'js', exceptions: 4, exception: /another origin/ } },
  { name: 'dup', apps: [['a', 'x'], ['a', 'y']], expect: { build: 'wasm' } },
  { name: 'rule', prebuilt: true, expect: { build: 'wasm', path: 'start' } },
  { name: 'no-wasm-real', alone: true, expect: { build: 'js' } },
  // The release profile. A page with the js_of_ocaml build makes no request
  // for a Wasm file: thus the loader preloads the main .wasm only for the
  // wasm_of_ocaml build.
  { name: 'rel-csp-header', release: true, csp: cspNoWasm, expect: { build: 'js', reports: 1, none: /wasm/ } },
  { name: 'rel-no-jstag', release: true, pre: ['no-jstag.js'], expect: { build: 'js', none: /wasm/ } },
  { name: 'rel-no-gc', release: true, pre: ['no-gc.js'], expect: { build: 'js', probes: 1, none: /wasm/ } },
  { name: 'rel-preload', release: true, after: '<link rel="stylesheet" href="x/slow.css">',
    expect: { build: 'wasm', height: 300, wasmBefore: 300 } },
  { name: 'rel-repeat', release: true, form: 'file', repeat: true, expect: { build: 'wasm', probes: 1 } },
  { name: 'rel-silent', release: true, before: '<link rel="stylesheet" href="x/fast.css">',
    expect: { build: 'wasm', path: null } },
  { name: 'rel-two', release: true, apps: [['a', 'a'], ['b', 'b']], expect: { build: 'wasm' } },
  { name: 'rel-names', release: true, names: true, expect: { build: 'wasm' } },
  // The head part runs second, after the defer start part: no dev check.
  { name: 'rel-late', release: true, form: 'file', headDelay: true, before: '<link rel="stylesheet" href="x/fast.css">',
    expect: { build: 'wasm', path: null } },
  // Chrome logs a preload that the page does not use about 3 s after the
  // load: the scenario waits 4 s.
  { name: 'rel-wait', release: true, expect: { build: 'wasm', wait: 4000 } },
];

const mine = scenarios.filter((sc) => !sc.release === !release);
const chosen = mine.filter((sc) => (only ? only.includes(sc.name) : !sc.alone));
if (only) for (const o of only) if (!mine.some((sc) => sc.name === o)) {
  console.log(`FAIL: no scenario ${o} in the ${profile} profile`);
  process.exit(1);
}

const work = mkdtempSync(join(tmpdir(), 'kilim-loader-'));

// The loader text and its CSP source expression. A mutant replaces the text
// of `kilim loader` in each page.
execFileSync(kilim, ['loader', '-o', join(work, 'loader.js')]);
const realText = readFileSync(join(work, 'loader.js'), 'utf8');
const mutant = opt('--mutant');
const loaderText = mutant ? readFileSync(mutant, 'utf8') : realText;
const hash = mutant
  ? `'sha256-${createHash('sha256').update(loaderText).digest('base64')}'`
  : execFileSync(kilim, ['csp']).toString().trim();
const withText = (page) => page.split(realText).join(loaderText);

const backend = await open(name, () => rmSync(work, { recursive: true, force: true }));

const builds = process.cwd();

const csp = (sc) => sc.csp.replaceAll('@HASH@', hash);

// The template of a scenario, with its markers.
const marker = (kind, name) => `<!--kilim:${kind}${name ? ':' + name : ''}-->`;
const template = (sc, prefix) => {
  const apps = sc.apps || [['a']];
  const mounts = [...new Set(apps.map(([a]) => a))];
  const cspMeta = sc.csp && sc.cspIn === 'meta' ? `<meta http-equiv="content-security-policy" content="${csp(sc)}">` : '';
  const pre = (sc.pre || []).concat(['count.js']).map((f) => `<script src="x/${f}"></script>`).join('');
  // The fixtures are relative to the base, also on a deep link.
  const x = (h) => h.replaceAll('"x/', `"${prefix}x/`);
  return '<!doctype html><html><head><meta charset="utf-8">' + cspMeta + x(pre + (sc.before || '')) +
    apps.map(([, n]) => marker('head', n)).join('') + x(sc.after || '') + `<title>${sc.name}</title></head>` +
    '<body>' + (sc.inject || '').replaceAll('@TEXT@', loaderText).replaceAll('@OTHER@', otherOrigin)
      .replaceAll('@PREFIX@', prefix) + mounts.map((m) => `<div id="${m}"></div>`).join('') +
    (sc.nest ? '<div>' : '') + apps.map(([, n]) => marker('start', n)).join('') + (sc.nest ? '</div>' : '') +
    (sc.end || '') + '</body></html>';
};

// The page of a scenario: one run of `kilim page` (dev) or `kilim dist`
// (release) for each app, each on the output of the run before it. The runs
// of `kilim dist` write into one directory.
const makePage = (sc, prefix, dir) => {
  if (sc.prebuilt) return withText(readFileSync(join(builds, 'index.html'), 'utf8'));
  let input = join(dir, 'template.html');
  writeFileSync(input, template(sc, prefix));
  (sc.apps || [['a']]).forEach(([app, name], i) => {
    const out = join(dir, `page${i}.html`);
    const at = (f) => (release ? join(builds, f) : f);
    const build = sc.build === 'js' ? ['--build', 'js'] : ['--wasm', at(`app_${app}.bc.wasm.js`)];
    const common = ['--template', input, '--base', prefix, '--js', at(`app_${app}.bc.js`), ...build,
      ...(sc.form === 'file' ? ['--form', 'file'] : []), ...(name ? ['--name', name] : [])];
    if (release) execFileSync(kilim, ['dist', ...common, '--page', out, '-o', join(dir, 'dist')]);
    else execFileSync(kilim, ['page', ...common, '-o', out]);
    input = out;
  });
  let page = withText(readFileSync(input, 'utf8'));
  // A mutant replaces the text of loader.<hash>.js too: the name stays.
  if (sc.form === 'file' && release)
    for (const f of readdirSync(join(dir, 'dist'))) if (/^loader\./.test(f)) writeFileSync(join(dir, 'dist', f), loaderText);
  if (sc.form === 'file' && !release) writeFileSync(join(dir, 'loader.js'), loaderText);
  if (sc.form === 'file') {
    const src = page.match(/<script src="([^"]*)" async/)[1];
    if (sc.headDelay) page = page.replace(`<script src="${src}" async`, `<script src="${src}?head" async`);
    const deferTag = `<script src="${src}" defer`;
    if (sc.between) page = page.replace(deferTag, sc.between + deferTag);
  }
  return page;
};

// A content name of `kilim dist` (16 hexadecimal characters before the last
// extension), or a file of an .assets directory of wasm_of_ocaml (its names
// already have a hash).
const contentName = (p) => /\.[0-9a-f]{16}\.js$/.test(p) || /\.assets\//.test(p);

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
  const send = (status, type, body, cache = 'no-cache') => {
    res.writeHead(status, { 'content-type': type, 'cache-control': cache });
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
  const loader = rest === 'loader.js' && sc.form === 'file' && !release ? join(site.dir, 'loader.js') : null;
  const built = !fixture && !loader && rest !== '' && file(release ? join(site.dir, 'dist') : builds, rest);
  if (!fixture && !loader && !built) {
    if (rest.startsWith('x/')) {
      site.log.push({ path: rest + url.search, t: Date.now() - site.t0, visit: site.visit });
      send(404, 'text/plain', '');
      return;
    }
    // The page. A navigation starts a visit. Another request of a path that
    // is not a file (for example a preload of a wrong URL) goes to the log.
    if (req.headers['sec-fetch-dest'] === 'document') {
      site.t0 = Date.now();
      site.visit++;
    } else site.log.push({ path: rest + url.search, t: Date.now() - site.t0, visit: site.visit });
    const headers = { 'content-type': 'text/html; charset=utf-8', 'cache-control': 'no-cache' };
    if (sc.csp && sc.cspIn !== 'meta') headers['content-security-policy'] = csp(sc);
    res.writeHead(200, headers);
    if (sc.bodyDelay) {
      const i = site.page.indexOf('<body');
      res.write(site.page.slice(0, i));
      await new Promise((ok) => setTimeout(ok, sc.bodyDelay));
      res.end(site.page.slice(i));
    } else res.end(site.page);
    return;
  }
  site.log.push({ path: rest + url.search, t: Date.now() - site.t0, visit: site.visit });
  // The async tag of a scenario with headDelay.
  if (url.search === '?head') await new Promise((ok) => setTimeout(ok, 500));
  if (fixture) {
    if (fixture.delay) await new Promise((ok) => setTimeout(ok, fixture.delay));
    send(200, fixture.type, fixture.body);
  } else if (loader) {
    send(200, 'text/javascript', readFileSync(loader));
  } else {
    const ext = built.slice(built.lastIndexOf('.'));
    const cache = release && contentName(rest) ? 'public, max-age=31536000, immutable' : 'no-cache';
    send(200, types[ext] || 'application/octet-stream', readFileSync(built), cache);
  }
});
await new Promise((ok) => server.listen(0, '127.0.0.1', ok));
const origin = `http://127.0.0.1:${server.address().port}`;

// A server on another port: another origin. It records each request.
const otherLog = [];
const other = http.createServer((req, res) => {
  otherLog.push(req.url);
  res.writeHead(200, { 'content-type': 'text/javascript', 'access-control-allow-origin': '*' });
  res.end('window.otherRan = true;');
});
await new Promise((ok) => other.listen(0, '127.0.0.1', ok));
const otherOrigin = `http://127.0.0.1:${other.address().port}`;

const browser = await backend.launch({ noWasm, flags });
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
  otherLog.length = 0;
  const key = sc.prebuilt ? 'rule' : `s${n}`;
  const prefix = `/${key}/`;
  const dir = join(work, key);
  mkdirSync(dir, { recursive: true });
  const site = { sc, dir, log: [], t0: Date.now(), visit: 0, page: makePage(sc, prefix, dir) };
  sites.set(key, site);
  const ids = [...new Set((sc.apps || [['a']]).map(([a]) => a))];
  const e = sc.expect;
  let events;
  const faults = [];
  const fault = (s) => faults.push(s);
  // The start time of each app, for the log: the time bounds have a margin.
  let starts = '';
  await browser.withPage(30000, async (page) => {
    events = watch(page);
    await page.goto(origin + prefix + (sc.url || ''));
    const read = (expr) => page.evaluate(expr);
    let st;
    if (e.start === false) {
      await sleep(2000);
      st = JSON.parse(await read(stateJs));
      if (Object.keys(st.loaded).length) fault('an app started');
      return;
    }
    // A read during the navigation can fail: then the poll goes on.
    const started = async () => {
      const ok = await until(async () => {
        st = JSON.parse(await read(stateJs));
        return ids.every((id) => st.loaded[id]?.starts === 1);
      }, 10000);
      if (!ok) fault(`no start in 10 s: ${JSON.stringify(st?.loaded ?? {})}`);
      return ok;
    };
    if (!(await started())) return;
    // Late requests and late errors.
    await sleep(e.wait ?? 100);
    st = JSON.parse(await read(stateJs));
    starts = ids.map((id) => `${id} ${Math.round(st.loaded[id].time)} ms`).join(', ');
    const views = JSON.parse(await read(clickJs(ids)));
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
      if (e[k] !== undefined && ![e[k]].flat().includes(st[k])) fault(`${k}: ${st[k]}, expected ${e[k]}`);
    if (e.reports !== undefined && st.reports.length !== e.reports)
      fault(`reports: ${JSON.stringify(st.reports)}, expected ${e.reports}`);
    if (e.gadget && (await read(gadgetJs)) !== 'refused') fault('the policy accepts another URL');
    if (sc.inject) {
      if (await read('window.otherRan')) fault('a script of another origin ran');
      if (otherLog.length) fault(`requests to another origin: ${otherLog.join(' ')}`);
    }
    if (sc.repeat) {
      // The second visit: the files with a content name come from the cache.
      await page.goto(origin + prefix);
      await sleep(50);
      if (!(await started())) return;
      await sleep(100);
      st = JSON.parse(await read(stateJs));
      if (st.probes !== 1) fault(`second visit: probes ${st.probes}, expected 1`);
      if (st.loaded.a.build !== e.build) fault(`second visit: build ${st.loaded.a.build}`);
      for (const r of site.log)
        if (r.visit === 2 && /^(loader|app_)|\.wasm$/.test(r.path)) fault(`second visit: a request for ${r.path}`);
    }
  });
  // The requests of the first visit: each URL at most one time, and the
  // expected counts.
  const first = site.log.filter((r) => r.visit === 1);
  const counts = {};
  for (const r of first) counts[r.path] = (counts[r.path] || 0) + 1;
  if (e.start !== false) {
    for (const [p, c] of Object.entries(counts)) if (c > 1) fault(`${c} requests for ${p}`);
    for (const [p, c] of Object.entries(e.requests || {}))
      if ((counts[p] || 0) !== c) fault(`${counts[p] || 0} requests for ${p}, expected ${c}`);
    if (e.none) for (const p of Object.keys(counts)) if (e.none.test(p)) fault(`a request for ${p}`);
    if (e.buildBefore !== undefined) {
      const b = first.find((r) => /^app_a\.bc\.(wasm\.)?js$/.test(r.path));
      if (!b || !(b.t < e.buildBefore)) fault(`the build request at ${b?.t} ms, expected before ${e.buildBefore} ms`);
    }
    if (e.wasmBefore !== undefined) {
      const w = first.find((r) => /\.wasm$/.test(r.path));
      if (!w || !(w.t < e.wasmBefore)) fault(`the .wasm request at ${w?.t} ms, expected before ${e.wasmBefore} ms`);
    }
    // Each build file has a content name, and the bytes of the build.
    if (sc.names)
      for (const r of first.filter((x) => /^app_/.test(x.path))) {
        const orig = r.path.replace(/\.[0-9a-f]{16}\.js$/, '.js');
        if (!contentName(r.path)) fault(`${r.path}: not a content name`);
        else if (!readFileSync(join(dir, 'dist', r.path)).equals(readFileSync(join(builds, orig))))
          fault(`${r.path}: not the bytes of ${orig}`);
      }
  }
  const warnings = events.warnings.length;
  if (warnings !== (e.warnings ?? 0)) fault(`${warnings} warnings: ${JSON.stringify(events.warnings)}`);
  if (events.errors.length !== (e.errors ?? 0)) fault(`${events.errors.length} console errors: ${JSON.stringify(events.errors)}`);
  const thrown = events.exceptions.filter((x) => !(e.exception && e.exception.test(x)));
  if (thrown.length) fault(`exceptions: ${JSON.stringify(thrown)}`);
  if (e.exceptions !== undefined && events.exceptions.length !== e.exceptions)
    fault(`${events.exceptions.length} exceptions, expected ${e.exceptions}: ${JSON.stringify(events.exceptions)}`);
  // For example a preload that the request does not use, because its
  // credentials mode is not the same. A file from the cache then gives no
  // second request.
  const preloads = events.logs.filter((l) => /preload/i.test(l.text)).map((l) => l.text);
  if (preloads.length) fault(`preload messages: ${JSON.stringify(preloads)}`);
  sites.delete(key);
  return { faults, log: site.log, starts };
};

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
  await browser.close();
  server.close();
  other.close();
  server.closeAllConnections();
  rmSync(work, { recursive: true, force: true });
}
console.log(failures === 0 ? `all ${chosen.length} scenarios pass` : `${failures} scenarios FAILED`);
process.exit(failures === 0 ? 0 : 1);
