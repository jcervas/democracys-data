// ---------------------------------------------------------------------------
// collect.mjs -- ask three platforms how big each member of Congress's
// audience is, and write down what each one says.
//
// WHY A BROWSER. Nobody publishes this. There is no follower-count file to
// download. X charges $200 a month for the API that would answer it, and the
// old free endpoint (cdn.syndication.twimg.com followbutton) now returns HTTP
// 200 with an empty body. So two of the three platforms are read the way a
// person reads them: load the public profile page, logged out, and take the
// number off the screen. Bluesky is different and needs no browser -- it has a
// free public API with no key and no account.
//
// WHAT THIS PRODUCES. raw/followers-<date>.csv, one row per member per
// platform, INCLUDING the misses. A member with no handle on file is a row. A
// handle that 404s is a row. The misses are half the chapter, so they are
// recorded rather than dropped.
//
// THE HANDLES COME FROM TWO DIFFERENT PLACES, and that is the point.
//   X and Instagram -- handles are read out of unitedstates/congress-legislators,
//     a roster volunteers maintain by hand. 505 members have an X handle on
//     file, 407 an Instagram one. Nobody at either company published that list.
//   Bluesky -- there are no Bluesky handles in the roster, so this script
//     GUESSES them from the member's name against their chamber's domain
//     (schiff.senate.gov, ocasio-cortez.house.gov). A guess either resolves or
//     it does not. It cannot resolve to the wrong person, because Bluesky
//     verifies a domain handle against DNS for that domain, and only the
//     Senate controls senate.gov.
//
// FROZEN, NOT LIVE. This runs once and the CSV it writes is committed. Follower
// counts change every minute; a chapter whose numbers move under it cannot be
// checked by anyone. build-data.R reads the frozen file and never the network,
// so the chapter rebuilds offline and a student re-running it in 2030 gets the
// same numbers this text describes.
//
// RATE. One profile at a time with a pause between, roughly 900 page loads
// across half an hour, once. These are public pages served to a logged-out
// reader. Nothing is logged into and no account is used.
//
// Run from this directory:
//     node collect.mjs             # all three platforms
//     node collect.mjs bluesky     # one platform
// Resumable: rows already in the output file are skipped.
// ---------------------------------------------------------------------------

import { chromium } from '../../../_lib/node_modules/playwright-core/index.mjs';
import fs from 'fs';
import path from 'path';

const DATE = process.env.DD_SCAN_DATE || new Date().toISOString().slice(0, 10);
const OUT = `raw/followers-${DATE}.csv`;
const COLS = ['bioguide_id', 'last_name', 'first_name', 'party', 'state', 'chamber',
  'platform', 'handle', 'handle_from', 'status', 'displayed', 'exact',
  'following', 'posts', 'fetched_at'];

const only = process.argv[2] || null;
const PAUSE = 900;
const sleep = ms => new Promise(r => setTimeout(r, ms));

// --- the roster -------------------------------------------------------------

function parseCSV(text) {
  const rows = []; let row = [], cur = '', q = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (q) {
      if (c === '"' && text[i + 1] === '"') { cur += '"'; i++; }
      else if (c === '"') q = false;
      else cur += c;
    } else if (c === '"') q = true;
    else if (c === ',') { row.push(cur); cur = ''; }
    else if (c === '\n') { row.push(cur); rows.push(row); row = []; cur = ''; }
    else if (c !== '\r') cur += c;
  }
  if (cur.length || row.length) { row.push(cur); rows.push(row); }
  const hdr = rows.shift();
  return rows.filter(r => r.length === hdr.length)
    .map(r => Object.fromEntries(hdr.map((h, i) => [h, r[i]])));
}

const roster = parseCSV(fs.readFileSync('raw/legislators-current.csv', 'utf8'))
  .filter(r => r.bioguide_id);
const social = JSON.parse(fs.readFileSync('raw/legislators-social-media.json', 'utf8'));
const socialBy = Object.fromEntries(social.map(s => [s.id.bioguide, s.social]));

const csvq = v => {
  const s = v === null || v === undefined ? '' : String(v);
  return /[",\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
};

// resume: which (bioguide, platform) pairs are already written
const done = new Set();
if (fs.existsSync(OUT)) {
  for (const r of parseCSV(fs.readFileSync(OUT, 'utf8'))) done.add(r.bioguide_id + '\t' + r.platform);
} else {
  fs.mkdirSync('raw', { recursive: true });
  fs.writeFileSync(OUT, COLS.join(',') + '\n');
}

function write(row) {
  fs.appendFileSync(OUT, COLS.map(c => csvq(row[c])).join(',') + '\n');
  done.add(row.bioguide_id + '\t' + row.platform);
}

const base = r => ({
  bioguide_id: r.bioguide_id, last_name: r.last_name, first_name: r.first_name,
  party: r.party, state: r.state, chamber: r.type === 'sen' ? 'Senate' : 'House',
  fetched_at: new Date().toISOString(),
});

// --- Bluesky: no browser, no key, and the handle is guessed -----------------
//
// Hyphens and apostrophes are KEPT where the domain allows a hyphen. An earlier
// version stripped every non-letter and so never tried ocasio-cortez.house.gov,
// which exists. That was a bug in the collector being read as a fact about the
// world, which is exactly the failure this chapter is about.

const slug = s => s.toLowerCase().replace(/['.,]/g, '').replace(/\s+/g, '');
const slugHyphen = s => s.toLowerCase().replace(/['.,]/g, '').replace(/\s+/g, '-');
// André -> andre, Luján -> lujan. A handle may only hold ASCII letters, digits
// and hyphens, so an accented name has to be folded before it can be guessed at
// all -- and the fold is a second guess about the person, not a fact about them.
const fold = s => s.normalize('NFD').replace(/[̀-ͯ]/g, '');
const VALID_HANDLE = /^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$/;

function blueskyCandidates(r) {
  const dom = r.type === 'sen' ? 'senate.gov' : 'house.gov';
  const pre = r.type === 'sen' ? ['', 'sen', 'senator'] : ['', 'rep', 'congressman', 'congresswoman'];
  const variants = s => [...new Set([slug(s), slugHyphen(s), fold(slug(s)), fold(slugHyphen(s))])];
  const lasts = variants(r.last_name);
  const firsts = variants(r.first_name);
  const stems = [...new Set([...lasts, ...firsts.flatMap(f => lasts.map(l => f + l))])];
  // Anything still not a legal handle is dropped HERE rather than sent. One
  // malformed identifier makes the API reject the whole batch of 25 with a 400,
  // and a rejected batch looks exactly like 25 members having no account.
  return [...new Set(stems.flatMap(s => pre.map(p => `${p}${s}.${dom}`)))].filter(h => VALID_HANDLE.test(h));
}

async function bluesky() {
  const todo = roster.filter(r => !done.has(r.bioguide_id + '\tbluesky'));
  console.log(`bluesky: ${todo.length} members to resolve`);
  const cand2bio = new Map();
  for (const r of todo) for (const c of blueskyCandidates(r)) if (!cand2bio.has(c)) cand2bio.set(c, r.bioguide_id);
  const all = [...cand2bio.keys()];
  console.log(`  ${all.length} candidate handles, ${Math.ceil(all.length / 25)} batched requests`);

  const hits = new Map(); // bioguide -> profile

  // A batch that is rate-limited comes back with FEWER profiles than it had
  // hits, and there is no way to tell a throttled batch from one where nobody
  // had an account -- both look like silence. So sweep repeatedly over the
  // candidates of members not yet resolved, and stop only when a whole pass
  // finds nothing new. The first version ran one pass and reported 120 of 537;
  // the true figure was higher, and the eleven it dropped included a member
  // with 566,000 followers.
  let round = 0;
  while (round < 5) {
    round++;
    const pending = all.filter(c => !hits.has(cand2bio.get(c)));
    if (!pending.length) break;
    const before = hits.size;
    const pace = 120 + (round - 1) * 300;
    console.log(`  round ${round}: ${pending.length} candidates outstanding (${pace}ms pace)`);
    for (let i = 0; i < pending.length; i += 25) {
      await askBatch(pending.slice(i, i + 25), pace);
      await sleep(pace);
    }

    // Ask for up to 25 handles at once. On a 400 the request is rejected whole,
    // so split it and ask again -- that isolates the one bad identifier instead
    // of losing the 24 beside it. Recursion bottoms out at a single handle,
    // which is reported rather than swallowed.
    async function askBatch(batch, pace) {
      if (!batch.length) return;
      const q = batch.map(a => 'actors=' + encodeURIComponent(a)).join('&');
      let res, j;
      try {
        res = await fetch(`https://public.api.bsky.app/xrpc/app.bsky.actor.getProfiles?${q}`,
          { headers: { 'User-Agent': 'democracys-data-84355/1.0' } });
      } catch (e) { console.log(`    request failed: ${e.message}`); return; }
      if (res.status === 429) { await sleep(5000); return askBatch(batch, pace); }
      if (!res.ok) {
        if (batch.length === 1) { console.log(`    rejected handle: ${batch[0]} (HTTP ${res.status})`); return; }
        const h = Math.ceil(batch.length / 2);
        await askBatch(batch.slice(0, h), pace); await sleep(pace);
        await askBatch(batch.slice(h), pace);
        return;
      }
      try { j = await res.json(); } catch { return; }
      for (const p of j.profiles || []) {
        const bio = cand2bio.get(p.handle);
        if (bio && !hits.has(bio)) hits.set(bio, p);
      }
    }
    console.log(`  round ${round}: ${hits.size} found (+${hits.size - before})`);
    if (hits.size === before) break; // a full pass added nobody: this is the floor
  }

  for (const r of todo) {
    const p = hits.get(r.bioguide_id);
    write({
      ...base(r), platform: 'bluesky',
      handle: p ? p.handle : '', handle_from: 'domain-guess',
      status: p ? 'ok' : 'no-account-found',
      displayed: '', exact: p ? p.followersCount : '',
      following: p ? p.followsCount : '', posts: p ? p.postsCount : '',
    });
  }
  console.log(`bluesky: ${hits.size} of ${todo.length} resolved`);
}

// --- X and Instagram: read the public profile page --------------------------

async function scrape(platform) {
  const key = platform === 'x' ? 'twitter' : 'instagram';
  const todo = roster.filter(r => !done.has(r.bioguide_id + '\t' + platform));
  console.log(`${platform}: ${todo.length} members`);

  const browser = await chromium.launch({ channel: 'chrome', headless: true });
  const ctx = await browser.newContext({ viewport: { width: 1280, height: 900 }, locale: 'en-US' });
  const page = await ctx.newPage();
  let n = 0, ok = 0;

  for (const r of todo) {
    n++;
    const handle = (socialBy[r.bioguide_id] || {})[key] || '';
    if (!handle) {
      write({ ...base(r), platform, handle: '', handle_from: 'congress-legislators',
        status: 'no-handle-on-file', displayed: '', exact: '', following: '', posts: '' });
      continue;
    }
    let out = { status: 'error', displayed: '', exact: '', following: '', posts: '' };
    try {
      if (platform === 'x') {
        await page.goto(`https://x.com/${handle}`, { waitUntil: 'domcontentloaded', timeout: 45000 });
        await page.waitForSelector('[href$="/verified_followers"], [href$="/followers"]', { timeout: 20000 });
        out = await page.evaluate(() => {
          const g = sel => document.querySelector(sel);
          const f = g('[href$="/verified_followers"], [href$="/followers"]');
          const fw = g('[href$="/following"]');
          const txt = e => e ? e.innerText.replace(/\n/g, ' ') : '';
          const num = t => (t.match(/^([\d.,]+[KM]?)/) || [, ''])[1];
          return { status: 'ok', displayed: num(txt(f)), exact: '',
                   following: num(txt(fw)), posts: '' };
        });
      } else {
        await page.goto(`https://www.instagram.com/${handle}/`, { waitUntil: 'domcontentloaded', timeout: 45000 });
        await page.waitForSelector('meta[property="og:description"]', { state: 'attached', timeout: 20000 });
        await page.waitForTimeout(2200);
        out = await page.evaluate(() => {
          const meta = document.querySelector('meta[property="og:description"]')?.content || '';
          const m = meta.match(/([\d.,]+[KM]?)\s+Followers,\s+([\d.,]+[KM]?)\s+Following,\s+([\d.,]+[KM]?)\s+Posts/i);
          // the exact count sits in a title attribute while the page displays a rounded one
          const exact = [...document.querySelectorAll('[title]')]
            .map(e => e.getAttribute('title')).filter(t => /^\d{1,3}(,\d{3})+$|^\d+$/.test(t));
          return { status: m ? 'ok' : 'no-count-on-page',
                   displayed: m ? m[1] : '', exact: exact.length ? exact[0].replace(/,/g, '') : '',
                   following: m ? m[2] : '', posts: m ? m[3] : '' };
        });
      }
      if (out.status === 'ok') ok++;
    } catch (e) {
      const title = await page.title().catch(() => '');
      out.status = /Profile|not found|Page N/i.test(title) ? 'handle-not-found' : 'blocked-or-timeout';
    }
    write({ ...base(r), platform, handle, handle_from: 'congress-legislators', ...out });
    if (n % 25 === 0) console.log(`  ${n}/${todo.length}  ok=${ok}`);
    await sleep(PAUSE);
  }
  await browser.close();
  console.log(`${platform}: ${ok} counts read of ${todo.length} members`);
}

// ---------------------------------------------------------------------------

if (!only || only === 'bluesky') await bluesky();
if (!only || only === 'x') await scrape('x');
if (!only || only === 'instagram') await scrape('instagram');
console.log(`\nwrote ${OUT}`);
