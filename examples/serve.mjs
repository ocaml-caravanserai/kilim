// A static server for the kilim examples, with only node. docs/loader.md
// tells you to serve the dev files with "cache-control: no-cache", and a plain
// static server does not send this header.
//
//   node examples/serve.mjs [--port N] DIR
//       The dev files: DIR at /. Each response has "no-cache".
//   node examples/serve.mjs [--port N] --base /static/ [--csp TEXT] DIR
//       The output of kilim dist: the files of DIR at the base. A content
//       name and a file of an .assets directory are immutable, the other
//       files have "no-cache".
//
// The base and index.html at the base give DIR/index.html, the page. A request
// for a page (GET, with text/html in the accept header) that is not a file
// also gets the page, thus a deep link of an app works. --csp TEXT
// puts TEXT in the content-security-policy header of the page. Each other
// request that is not a file gets 404, and a path outside DIR gets 403. The
// server listens only on 127.0.0.1 (default port 8000).
//
// The API of the search example: /api/cities?q=TEXT gives a JSON array of the
// cities of a fixed list whose names start with TEXT (the ASCII case is
// ignored). An empty TEXT gives []. &delay=MS sends the answer MS ms later.
//
// The Chrome test of the examples (test/examples/run.mjs) imports serve().

import { readFileSync, statSync } from 'node:fs';
import http from 'node:http';
import { extname, join, normalize, resolve, sep } from 'node:path';
import { pathToFileURL } from 'node:url';

const types = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript',
  '.wasm': 'application/wasm',
  '.css': 'text/css',
  '.json': 'application/json',
  '.map': 'application/json',
};
const immutable = 'public, max-age=31536000, immutable';

// A content name of kilim dist (16 hexadecimal characters before the last
// extension), or a file of an .assets directory of wasm_of_ocaml.
const contentName = (p) => /\.[0-9a-f]{16}\.[^./]+$/.test(p) || /\.assets\//.test(p);

const cities = ['Berlin', 'Bern', 'Lisbon', 'London', 'Lyon', 'Madrid', 'Malaga', 'Milan',
  'Munich', 'Paris', 'Parma', 'Rome', 'Vienna'];

const api = (url, send) => {
  const q = (url.searchParams.get('q') ?? '').toLowerCase();
  const found = q === '' ? [] : cities.filter((c) => c.toLowerCase().startsWith(q));
  const delay = Number(url.searchParams.get('delay') ?? 0);
  const answer = () => send(200, { 'content-type': types['.json'], 'cache-control': 'no-store' },
    JSON.stringify(found));
  if (delay > 0) setTimeout(answer, delay);
  else answer();
};

const isFile = (f) => {
  try {
    return statSync(f).isFile();
  } catch {
    return false;
  }
};

// Starts the server. dir: the directory. base: a path that ends with /, for
// the output of kilim dist (null: the dev files at /). csp: the CSP of the
// page, or null. onRequest(path, status): called for each request. port: 0
// for a free port. The result is a promise of { server, origin }.
export const serve = ({ dir, base = null, csp = null, onRequest = () => {}, port = 8000 }) => {
  const root = resolve(dir);
  const prefix = base ?? '/';
  const server = http.createServer((req, res) => {
    const url = new URL(req.url, 'http://x');
    const send = (status, headers, body) => {
      onRequest(url.pathname, status);
      res.writeHead(status, headers);
      res.end(body);
    };
    let path;
    try {
      path = decodeURIComponent(url.pathname);
    } catch {
      send(400, {}, '');
      return;
    }
    const page = () => {
      const headers = { 'content-type': types['.html'], 'cache-control': 'no-cache' };
      if (csp) headers['content-security-policy'] = csp;
      send(200, headers, readFileSync(join(root, 'index.html')));
    };
    if (path === '/api/cities') {
      api(url, send);
      return;
    }
    if (path.startsWith(prefix)) {
      const rest = path.slice(prefix.length);
      const file = normalize(join(root, rest));
      if (file !== root && !file.startsWith(root + sep)) {
        send(403, {}, '');
        return;
      }
      if ((rest === '' || rest === 'index.html') && isFile(join(root, 'index.html'))) {
        page();
        return;
      }
      if (isFile(file)) {
        const cache = base !== null && contentName(rest) ? immutable : 'no-cache';
        send(200, { 'content-type': types[extname(file)] || 'application/octet-stream', 'cache-control': cache },
          readFileSync(file));
        return;
      }
    }
    if (req.method === 'GET' && /text\/html/.test(req.headers.accept || '') && isFile(join(root, 'index.html')))
      page();
    else send(404, {}, '');
  });
  return new Promise((ok, ko) => {
    server.on('error', ko);
    server.listen(port, '127.0.0.1', () => ok({ server, origin: `http://127.0.0.1:${server.address().port}` }));
  });
};

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  const args = process.argv.slice(2);
  const opts = {};
  let dir;
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === '--port') opts.port = Number(args[++i]);
    else if (a === '--base') opts.base = args[++i];
    else if (a === '--csp') opts.csp = args[++i];
    else if (!a.startsWith('--') && dir === undefined) dir = a;
    else dir = null;
  }
  const valid = dir && (opts.base === undefined || /^\/(.*\/)?$/.test(opts.base)) &&
    (opts.port === undefined || Number.isInteger(opts.port));
  if (!valid) {
    console.log('usage: node examples/serve.mjs [--port N] [--base /PATH/] [--csp TEXT] DIR');
    process.exit(2);
  }
  const { origin } = await serve({ dir, ...opts });
  console.log(`${dir} at ${origin}/${opts.base ? `, the files at ${opts.base}` : ''}`);
}
