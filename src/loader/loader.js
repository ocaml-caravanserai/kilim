// The kilim loader (docs/design.html, "The loader"; docs/loader.md). It picks
// the wasm_of_ocaml or the js_of_ocaml build one time, before a build starts.
//
// One text runs two times: the head part (after <meta charset> and each CSP
// <meta>, before each stylesheet) and the start part (data-start: the last
// element of the body, or directly after the async tag in the loader.js
// form). The values are in the data- attributes of the script element, thus
// the text and its CSP hash are the same in each page.
//
// The form of this file: one function in ES5 syntax, with whole-line comments
// only. The generator (gen/embed.ml) removes the comment lines, the leading
// spaces and the empty lines. It refuses a text that is not ASCII, or that has
// a slash-slash, a slash-star, a "</" or a "<!--". A change of this file
// changes the CSP hash: loader.csp holds it, and `dune runtest` compares it.
(function () {
var doc = document;
var s = doc.currentScript;
var d = s.dataset;
var root = doc.documentElement;
// The state that the parts and the apps of the page share. The name is not
// the cell property of kilim.browser.
var L = root.__kilimLoader || (root.__kilimLoader = {apps: {}, urls: Object.create(null)});
// One record for each app: build, flag, started, head and startEl. Two builds
// in one directory have the same base, thus the key also has the name of the
// js_of_ocaml build.
var key = d.base + " " + d.js;
var st = L.apps[key] || (L.apps[key] = {});
var dev = "dev" in d;
// Each URL comes from the base, never from the URL of the document: thus a
// deep link of a router app loads the build. A URL of another origin throws:
// the CSP hash does not cover the data- attributes, thus a copy of the loader
// that a page injects must not load a script of another origin (for example
// under 'strict-dynamic').
var base = new URL(d.base, location.href);
function url(n) {
var u = new URL(n, base);
if (u.origin != location.origin) {
throw Error("kilim loader: another origin: " + u.href);
}
return u.href;
}
function src(b) {
return url(b == "wasm" ? d.wasm : d.js);
}
function preload(h, as, cors) {
var l = doc.createElement("link");
l.rel = "preload";
l.as = as;
if (cors) {
l.crossOrigin = "anonymous";
}
l.href = h;
doc.head.appendChild(l);
}
// The start of the build, one time for each app. With Trusted Types, the src
// goes through the policy "kilim-loader". The policy accepts only the build
// URLs that a part started, thus it gives no TrustedScriptURL for other URLs.
// The call throws when the trusted-types directive does not list the name,
// or when the page has no trustedTypes: then the src is a plain URL.
function start(b, part) {
if (st.started) {
return;
}
st.started = true;
var u = src(b);
L.urls[u] = true;
if (!L.policy) {
try {
L.policy = window.trustedTypes.createPolicy("kilim-loader", {createScriptURL: function (x) {
if (L.urls[x]) {
return x;
}
throw Error("kilim-loader: " + x);
}});
} catch (x) {
}
}
var e = doc.createElement("script");
e.src = L.policy ? L.policy.createScriptURL(u) : u;
doc.head.appendChild(e);
if (dev) {
root.setAttribute("data-kilim-started", part);
}
}
// The checks of the dev profile, when the whole document is parsed. The part
// that runs second calls them: then the head part and the start part exist.
// The first stylesheet and the last CSP meta of the document give the
// result. Bit 4 of compareDocumentPosition: the argument comes after the
// node.
function warn(t) {
console.warn("kilim loader: " + t);
}
function check() {
var h = st.head;
var n = doc.querySelector("link[rel~=stylesheet i], style");
if (n && n.compareDocumentPosition(h) & 4) {
warn("a stylesheet is before the head part");
}
n = doc.querySelectorAll("meta[http-equiv=content-security-policy i]");
n = n[n.length - 1];
if (n && h.compareDocumentPosition(n) & 4) {
warn("a CSP meta is after the head part");
}
// The inline form: the start part is a child of the body, and only start
// parts come after it (two apps). The loader.js form: the start part comes
// directly after the head part.
n = st.startEl;
var ok = h.src ? h.nextElementSibling == n : n.parentNode == doc.body;
while (ok && !h.src && (n = n.nextElementSibling)) {
ok = n.matches("script[data-start]");
}
if (!ok) {
warn("the start part is not in its place");
}
}
// The start part: the part that runs second starts the build. Each start
// part runs before DOMContentLoaded, and the dev checks wait for it.
if ("start" in d) {
st.startEl = s;
if (st.build) {
start(st.build, "start");
if (dev) {
doc.addEventListener("DOMContentLoaded", check);
}
} else {
st.flag = true;
}
return;
}
// The head part: the probes, in the order of their cost. The compile throws
// under a CSP with no 'wasm-unsafe-eval' (one CSP report). The validate
// probe is a module with one GC struct type.
st.head = s;
var b = "js";
var m = [0, 97, 115, 109, 1, 0, 0, 0];
if (d.build != "js" && typeof WebAssembly == "object" && WebAssembly.JSTag) {
try {
new WebAssembly.Module(new Uint8Array(m));
if (WebAssembly.validate(new Uint8Array(m.concat([1, 5, 1, 95, 1, 127, 0])))) {
b = "wasm";
}
} catch (x) {
}
}
st.build = b;
preload(src(b), "script");
if (b == "wasm" && d.wasmMain) {
preload(url(d.wasmMain), "fetch", true);
}
// The head part runs second only in the loader.js form, after the defer
// start part: the document is parsed.
if (st.flag) {
start(b, "head");
if (dev) {
check();
}
}
})();
