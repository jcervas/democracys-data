#!/usr/bin/env node
/*
 * Is any figure text unreadable on the dark page?
 *
 * WHY THIS CANNOT BE DONE IN THE SOURCE. Two of the three ways a figure text
 * gets its colour are invisible to a source scan: a fill set through a CSS
 * class, and a fill built into a template string. A scan of the .Rmd files
 * predicts 13 dim texts in this corpus. The browser finds 112.
 *
 * And the part that matters most is not the colour at all, it is what the text
 * is sitting ON. brief.css lifts dark fills for the dark page, which is right
 * over the page background and wrong over a light patch drawn inside the
 * figure. residual-votes draws a cream ballot; lifting its candidate names put
 * near-white text on cream at 1.2:1. Nothing but a render knows that, so this
 * check opens each brief in a real browser, runs the figures, and hit-tests the
 * point under every text to find the paint actually behind it.
 *
 * WHAT IT DOES NOT MEASURE. Whether a colour means the right thing, whether two
 * series are distinguishable, or anything at all about the static twin that
 * goes to print. It measures one number: text against its own background.
 *
 * WHY IT IS NOT IN THE DEFAULT SUITE. check-all.sh is meant to take seconds,
 * and render-brief.R runs it before every single render. This one drives a
 * browser over ~106 pages and takes minutes. It is opt-in:
 *
 *     node _lib/check-contrast.js                 # whole corpus
 *     node _lib/check-contrast.js rpv             # one chapter
 *     node _lib/check-contrast.js --light         # the light page instead
 *     sh _lib/check-all.sh --contrast             # as part of the suite
 *
 * Findings already read and accepted live in check-contrast-reviewed.tsv, the
 * same convention check-tables.py uses. Only new ones are reported. Exit is 1
 * if any new finding survives, so it can gate a commit when run directly.
 *
 * Needs playwright-core (in _lib/node_modules) and a Chrome. Set DD_CHROME to
 * override the path. If either is missing it says so and exits 0 rather than
 * failing a suite over a missing tool.
 */
"use strict";
const fs = require("fs");
const path = require("path");

const HERE = __dirname;
const LABS = path.dirname(HERE);
const REVIEWED = path.join(HERE, "check-contrast-reviewed.tsv");

// WCAG 1.4.11: graphical objects and large text want 3:1. Figure labels are
// small, so this is a floor, not a target.
const MIN = 3.0;

// The measuring viewport. WIDTH is the one thing that must not vary: a figure
// scales to its container, so measuring at a different width measures a
// different figure. MAX_TALL only bounds a runaway page; the tallest brief in
// the corpus is around 20,000px.
const WIDTH = 1280;
const MAX_TALL = 40000;

const CHROMES = [
  process.env.DD_CHROME,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Chromium.app/Contents/MacOS/Chromium",
  "/usr/bin/google-chrome",
  "/usr/bin/chromium",
].filter(Boolean);

// Exit 0 on every one of these. A missing tool or a missing render is not a
// contrast defect, and this check must never fail the suite for its own
// prerequisites.
function giveUp(why, hint = "this check needs a browser") {
  console.log(why);
  console.log(`   skipped, not failed - ${hint}`);
  process.exit(0);
}

let chromium;
try {
  ({ chromium } = require(path.join(HERE, "node_modules", "playwright-core")));
} catch (e) {
  giveUp("playwright-core is not installed (npm i --prefix _lib playwright-core)");
}
// An override that is quietly ignored when it is wrong is worse than no
// override: it reports on a browser you did not choose.
if (process.env.DD_CHROME && !fs.existsSync(process.env.DD_CHROME)) {
  giveUp(`DD_CHROME is set to ${process.env.DD_CHROME}, which does not exist`);
}
const CHROME = CHROMES.find(p => fs.existsSync(p));
if (!CHROME) giveUp("no Chrome found; set DD_CHROME to its path");

// ---------------------------------------------------------------- the probe
// Runs inside the page. Returns one entry per text below the threshold.
function probe(MIN) {
  const lum = (r, g, b) => {
    const f = c => (c /= 255) <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    return 0.2126 * f(r) + 0.7152 * f(g) + 0.0722 * f(b);
  };
  const rgb = s => (String(s).match(/[\d.]+/g) || []).slice(0, 3).map(Number);
  const clear = s => /rgba\(0,\s*0,\s*0,\s*0\)|^none$|^transparent$/.test(String(s).trim());
  const con = (a, b) => {
    const x = lum(...a), y = lum(...b);
    return (Math.max(x, y) + 0.05) / (Math.min(x, y) + 0.05);
  };
  const hex = v => "#" + v.map(n => Math.round(n).toString(16).padStart(2, "0")).join("");

  const out = [], unmeasured = [];
  for (const t of document.querySelectorAll("svg text")) {
    const label = t.textContent.trim();
    if (!label) continue;
    const r = t.getBoundingClientRect();
    if (!r.width || !r.height) continue;                 // never painted
    if (getComputedStyle(t).visibility === "hidden") continue;
    if (parseFloat(getComputedStyle(t).fillOpacity) === 0) continue;
    const fg = rgb(getComputedStyle(t).fill);
    if (fg.length < 3) continue;

    // What is painted behind it. elementsFromPoint gives the stack at that
    // pixel; take the first thing under the text that actually paints.
    //
    // It only sees INSIDE the viewport. Off-screen it returns an empty stack,
    // and an empty stack that quietly became "the page background" is what
    // made the first version of this check report 797 phantom findings on the
    // light page: white bar labels below the fold, compared against the paper
    // instead of against their bar. The caller grows the viewport to the whole
    // document so this cannot happen; if it happens anyway, say so rather than
    // guess.
    const stack = document.elementsFromPoint(r.left + r.width / 2,
                                             r.top + r.height / 2);
    if (!stack.length) { unmeasured.push(label.slice(0, 40)); continue; }
    let bg = null;
    for (const el of stack) {
      if (el === t || el.contains(t)) {
        const c = getComputedStyle(el).backgroundColor;
        if (!clear(c)) { const v = rgb(c); if (v.length === 3) { bg = v; break; } }
        continue;                                        // keep descending
      }
      // Another text is not a background. Figures commonly draw a label twice,
      // once with a fat stroke as a halo, and taking the twin's fill as the
      // backdrop scores the label against itself: contrast 1.0, a finding that
      // looks alarming and is not real. Keep descending to the actual paint.
      if (el.tagName === "text" || el.tagName === "tspan") continue;
      const paint = el.ownerSVGElement ? getComputedStyle(el).fill
                                       : getComputedStyle(el).backgroundColor;
      if (clear(paint)) continue;
      const v = rgb(paint);
      if (v.length === 3) { bg = v; break; }
    }
    if (!bg) bg = rgb(getComputedStyle(document.body).backgroundColor);

    // A halo changes what the glyph is actually against. With paint-order:stroke
    // the outline is painted first and the fill on top, so the colour touching
    // every letter is the stroke, not whatever is further behind it. Measuring
    // against the mark would report a label that is perfectly legible -- this is
    // the standard fix for a label over a busy ground, and the check has to be
    // able to see it or it argues against the repair it should be recommending.
    const cs = getComputedStyle(t);
    if (/stroke/.test(cs.paintOrder) && !clear(cs.stroke)
        && parseFloat(cs.strokeWidth) > 0) {
      const halo = rgb(cs.stroke);
      if (halo.length === 3) bg = halo;
    }

    const c = con(fg, bg);
    if (c < MIN) {
      out.push({ text: label.slice(0, 40), fg: hex(fg), bg: hex(bg),
                 contrast: Math.round(c * 100) / 100 });
    }
  }
  return { out, unmeasured };
}

// ------------------------------------------------------------------ reviewed
// key = slug + fill + background. Deliberately NOT the text itself: the label
// wording changes when a chapter is rewritten, and the finding is about the
// colour pair, not the words.
function loadReviewed() {
  const seen = new Set();
  if (!fs.existsSync(REVIEWED)) return seen;
  for (const line of fs.readFileSync(REVIEWED, "utf8").split("\n")) {
    if (!line.trim() || line.startsWith("#")) continue;
    const f = line.split("\t");
    if (f.length >= 3) seen.add(`${f[0]}\t${f[1].toLowerCase()}\t${f[2].toLowerCase()}`);
  }
  return seen;
}

(async () => {
  const args = process.argv.slice(2);
  const scheme = args.includes("--light") ? "light" : "dark";
  const only = args.find(a => !a.startsWith("--"));

  const briefs = [];
  for (const part of fs.readdirSync(LABS).sort()) {
    if (part.startsWith("_")) continue;
    const pd = path.join(LABS, part);
    if (!fs.statSync(pd).isDirectory()) continue;
    for (const ch of fs.readdirSync(pd).sort()) {
      const f = path.join(pd, ch, `${ch}-brief.html`);
      if (fs.existsSync(f) && (!only || ch === only)) briefs.push([ch, part, f]);
    }
  }
  if (!briefs.length) {
    giveUp(only ? `no render found for ${only}` : "no rendered briefs found",
           "render first, then check");
  }

  const reviewed = loadReviewed();
  const browser = await chromium.launch({ executablePath: CHROME });
  const page = await (await browser.newContext({ colorScheme: scheme })).newPage();
  page.on("pageerror", () => {});                        // a broken figure is check-figures' job

  const fresh = [], known = [], blind = [];
  for (const [slug, part, file] of briefs) {
    let found = [];
    try {
      await page.setViewportSize({ width: WIDTH, height: 900 });
      await page.goto("file://" + encodeURI(file), { waitUntil: "load", timeout: 60000 });
      // Long enough for a d3 transition to finish. cast-vote-records delays
      // one to 700ms and runs it for 500, and at a shorter wait this check
      // returned different findings run to run -- a gate that is not
      // deterministic is worse than no gate.
      await page.waitForTimeout(1500);

      // Grow the viewport to the whole page, because elementsFromPoint is
      // blind outside it. Width is held fixed: figures scale to their
      // container width, and changing it would re-lay-out what we measure.
      const tall = Math.min(await page.evaluate(
        () => document.documentElement.scrollHeight), MAX_TALL);
      await page.setViewportSize({ width: WIDTH, height: tall });
      await page.waitForTimeout(600);                    // reflow, then redraw

      const r = await page.evaluate(probe, MIN);
      found = r.out;
      if (r.unmeasured.length) blind.push([slug, r.unmeasured.length]);
    } catch (e) {
      console.log(`   ${slug}: could not be opened - ${e.message.split("\n")[0]}`);
      continue;
    }
    for (const f of found) {
      const key = `${slug}\t${f.fg.toLowerCase()}\t${f.bg.toLowerCase()}`;
      (reviewed.has(key) ? known : fresh).push({ slug, part, ...f });
    }
  }
  await browser.close();

  // --baseline prints rows for check-contrast-reviewed.tsv on stdout. It does
  // not write the file. Blessing a finding is a decision, so it stays a
  // deliberate paste rather than something a flag does quietly.
  if (args.includes("--baseline")) {
    const rows = new Map();
    for (const f of fresh.concat(known)) {
      rows.set(`${f.slug}\t${f.fg}\t${f.bg}`,
               `${f.slug}\t${f.fg}\t${f.bg}\t${f.contrast}:1 - `);
    }
    for (const r of [...rows.values()].sort()) console.log(r);
    process.exit(0);
  }

  console.log(`text under ${MIN}:1 against what is painted behind it `
              + `(${scheme} page, ${briefs.length} briefs)`);
  console.log();
  if (fresh.length) {
    let last = "";
    for (const f of fresh.sort((a, b) => a.slug.localeCompare(b.slug) || a.contrast - b.contrast)) {
      if (f.slug !== last) { console.log(`   ${f.part}/${f.slug}`); last = f.slug; }
      console.log(`      ${String(f.contrast).padStart(5)}:1  ${f.fg} on ${f.bg}   ${JSON.stringify(f.text)}`);
    }
    console.log();
    console.log(`   ${fresh.length} new finding(s). A text on a LIGHT background here means an`);
    console.log("   ink rule lifted something drawn on a light patch - fix the figure, not");
    console.log("   the rule. Accept a finding by adding it to check-contrast-reviewed.tsv.");
  } else {
    console.log("   clean");
  }
  if (known.length) {
    console.log();
    console.log(`(${known.length} finding(s) previously read and recorded in `
                + "check-contrast-reviewed.tsv)");
  }
  // Never let this be silent. A text that could not be measured is a hole in
  // the check, and a hole that reports nothing reads exactly like a pass.
  if (blind.length) {
    const n = blind.reduce((a, b) => a + b[1], 0);
    console.log();
    console.log(`WARNING: ${n} text(s) in ${blind.length} brief(s) could not be `
                + "measured - no paint found beneath them.");
    console.log("   " + blind.slice(0, 6).map(b => `${b[0]} (${b[1]})`).join(", "));
    console.log("   These are NOT counted above. The usual cause is a page "
                + `taller than MAX_TALL (${MAX_TALL}px).`);
  }
  process.exit(fresh.length ? 1 : 0);
})();
