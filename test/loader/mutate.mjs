// The hand mutants of the loader text (mutants.txt).
//
//   node mutate.mjs --kilim PATH --profile dev|release
//
// For each mutant of the profile, the script applies the change to the output
// of `kilim loader`, runs run.mjs with --mutant and --only the scenarios of the
// line, and expects exit status 1. A mutant whose scenarios pass is a
// survivor: the script writes it and gives exit status 1. A line that is not
// valid (a text to find that does not occur exactly one time, a line with the
// two profiles, an unknown scenario) gives exit status 2. With no Chrome, the
// script writes "SKIP: no Chrome", as run.mjs.

import { execFileSync, spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { findChrome } from '../dom/chrome.mjs';

const argv = process.argv.slice(2);
const opt = (name) => {
  const i = argv.indexOf(name);
  return i < 0 ? undefined : argv[i + 1];
};
const kilim = opt('--kilim');
const profile = opt('--profile');
if (!kilim || !['dev', 'release'].includes(profile)) {
  console.log('usage: node mutate.mjs --kilim PATH --profile dev|release');
  process.exit(2);
}

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

const here = dirname(fileURLToPath(import.meta.url));
const work = mkdtempSync(join(tmpdir(), 'kilim-mutants-'));
const broken = (why) => {
  rmSync(work, { recursive: true, force: true });
  console.log(`FAIL: mutants.txt: ${why}`);
  process.exit(2);
};

execFileSync(kilim, ['loader', '-o', join(work, 'loader.js')]);
const text = readFileSync(join(work, 'loader.js'), 'utf8');

// The lines: slug | find | replacement | scenarios. The separator is " | ",
// thus a text can start or end with a space.
const mutants = readFileSync(join(here, 'mutants.txt'), 'utf8')
  .split('\n')
  .filter((l) => l.trim() !== '' && !l.startsWith('#'))
  .map((l) => {
    const f = l.split(' | ');
    if (f.length !== 4) broken(`not 4 fields: ${l}`);
    const [slug, find, replace, scenarios] = f.map((x) => x.replaceAll('\\n', '\n'));
    const only = scenarios.trim().split(',');
    const release = only.map((s) => s.startsWith('rel-'));
    if (release.some((r) => r !== release[0])) broken(`${slug}: scenarios of the two profiles`);
    const n = text.split(find).length - 1;
    if (n !== 1) broken(`${slug}: the text to find occurs ${n} times`);
    return { slug: slug.trim(), find, replace, only, profile: release[0] ? 'release' : 'dev' };
  });
const slugs = mutants.map((m) => m.slug);
for (const s of slugs) if (slugs.indexOf(s) !== slugs.lastIndexOf(s)) broken(`${s}: two lines`);

const mine = mutants.filter((m) => m.profile === profile);
const survivors = [];
for (const m of mine) {
  const f = join(work, `${m.slug}.js`);
  writeFileSync(f, text.replace(m.find, m.replace));
  const r = spawnSync('node', [join(here, 'run.mjs'), '--kilim', kilim, '--profile', profile,
    '--only', m.only.join(','), '--mutant', f], { encoding: 'utf8' });
  const lines = r.stdout.split('\n');
  if (r.status === 1 && lines.some((l) => l.startsWith('FAIL ')))
    console.log(`killed   ${m.slug}: ${lines.find((l) => l.startsWith('FAIL ')).slice(0, 160)}`);
  else if (r.status === 0) {
    survivors.push(m.slug);
    console.log(`SURVIVED ${m.slug}: ${m.only.join(', ')} pass`);
  } else {
    rmSync(work, { recursive: true, force: true });
    console.log(`FAIL: ${m.slug}: run.mjs gives exit status ${r.status}:\n${r.stdout}${r.stderr}`);
    process.exit(2);
  }
}
rmSync(work, { recursive: true, force: true });
console.log(survivors.length === 0
  ? `all ${mine.length} mutants of the ${profile} profile killed`
  : `${survivors.length} of ${mine.length} mutants survived: ${survivors.join(', ')}`);
process.exit(survivors.length === 0 ? 0 : 1);
