// The check of the code in the documentation: the code of a tagged block is
// the code of a file of the repository. It uses only node.
//
//   node check.mjs ROOT DOC MIN [DOC MIN ...]
//
// DOC is a Markdown file, relative to ROOT. A fenced block with the info
// string "LANG file=PATH" must be equal to the file PATH (relative to ROOT).
// A block with "LANG in=PATH" must be a part of the file PATH. The compare
// changes each run of white space to one space, and ignores the white space
// at the start and at the end. Thus a new line break or a new indent (dune
// fmt, ocamlformat) does not fail the check, but a change of a token does. A
// layout change that adds or removes a space between two tokens also fails
// it, for example "[At.x" in place of "[ At.x": then copy the new layout into
// the block. GitHub uses only LANG for the highlight.
//
// A failure: a tag that is not file= or in=, a missing file, a block that is
// not equal to its file or not a part of it, and a DOC with fewer than MIN
// tagged blocks (thus a removed tag does not make the check empty).

import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';

const [root, ...pairs] = process.argv.slice(2);
if (!root || pairs.length === 0 || pairs.length % 2 !== 0) {
  console.log('usage: node check.mjs ROOT DOC MIN [DOC MIN ...]');
  process.exit(2);
}

const squash = (s) => s.replace(/\s+/g, ' ').trim();

// The fenced blocks of a Markdown text: { line, info, code }.
const blocks = (md) => {
  const lines = md.split('\n');
  const found = [];
  for (let i = 0; i < lines.length; i++) {
    const open = lines[i].match(/^```(.*)$/);
    if (!open) continue;
    const start = i;
    const code = [];
    for (i++; i < lines.length && lines[i].trim() !== '```'; i++) code.push(lines[i]);
    found.push({ line: start + 1, info: open[1].trim(), code: code.join('\n') });
  }
  return found;
};

let failures = 0;
const fail = (s) => {
  console.log(`FAIL ${s}`);
  failures++;
};
for (let i = 0; i < pairs.length; i += 2) {
  const doc = pairs[i];
  const min = Number(pairs[i + 1]);
  let tagged = 0;
  for (const b of blocks(readFileSync(join(root, doc), 'utf8'))) {
    const tags = b.info.split(/\s+/).slice(1);
    for (const tag of tags) {
      const where = `${doc}:${b.line}`;
      const m = tag.match(/^(file|in)=(.+)$/);
      if (!m) {
        fail(`${where}: the tag ${tag} is not file=PATH or in=PATH`);
        continue;
      }
      tagged++;
      const [, kind, path] = m;
      if (!existsSync(join(root, path))) {
        fail(`${where}: no file ${path}`);
        continue;
      }
      const file = squash(readFileSync(join(root, path), 'utf8'));
      const code = squash(b.code);
      const ok = kind === 'file' ? file === code : code !== '' && file.includes(code);
      if (ok) console.log(`ok   ${where}: ${kind}=${path}`);
      else fail(`${where}: the block is not ${kind === 'file' ? 'equal to' : 'a part of'} ${path}`);
    }
  }
  if (tagged < min) fail(`${doc}: ${tagged} tagged blocks, expected at least ${min}`);
}
process.exit(failures === 0 ? 0 : 1);
