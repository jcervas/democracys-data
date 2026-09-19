/* build-map-solve.js -- the solver half of this chapter's seat-shift map.
 *
 * Runs the cartogram studio's own solver headless, with one deliberate
 * change from _lib/geo/build-apportionment.js: the seat count each state is
 * drawn at, and carved into, is max(seats 1960, seats 2020) rather than a
 * single census. That is what makes every cell in the country the same size
 * while still giving a state room for the seats it used to have -- New York
 * is drawn at 41 cells, not 26, and 15 of them are marked as lost.
 *
 * Inputs
 *   ../../../_lib/geo/raw/solver.js        placement, carving, relaxation
 *   ../../../_lib/geo/raw/studio.worker.js the pipeline that drives them
 *   ../../../_lib/geo/raw/studio.json      geometry + seats, from the R build
 *   derived/seatchange.csv                 this chapter's 1960 and 2020 seats
 *
 * Output
 *   derived/.seatshift-solve.json   an INTERMEDIATE, deleted by build-map.R.
 *   Cells here are the solver's raw power cells: convex, and cut to the
 *   state's bounding box rather than to its outline. build-map.R clips them.
 *
 * RUN FROM INSIDE data/:  node build-map-solve.js
 */
"use strict";

const fs = require("fs");
const path = require("path");
const { performance } = require("perf_hooks");

const RAW = path.join(__dirname, "..", "..", "..", "_lib", "geo", "raw");
const Y1 = 1960, Y2 = 2020;

/* The one parameter that is not the studio's default. The hand-drawn slot
 * layout -- a person placed every state, and it is the only layout that still
 * looks like the United States -- was drawn for 435 seats at areaDivisor 2.9.
 * This map draws 510 cells, so at 2.9 New York no longer fits the slot drawn
 * for it, the solver gives up on the slots and falls back to its free layout,
 * and the map stops being recognizable. 3.2 is the largest setting on a sweep
 * of the range at which the slots still converge; the run below refuses to
 * write anything if they ever stop. */
const AREA_DIVISOR = 3.2;

/* ---- the same just-enough Web Worker shim build-apportionment.js uses, so
 * the studio's worker file runs unmodified. new Function() evaluates in
 * global scope, so `self` has to be a true global. */
globalThis.self = globalThis;
self.performance = performance;
self.importScripts = (f) => {
  new Function(fs.readFileSync(path.join(RAW, f), "utf8")).call(self);
};

let result = null, error = null;
self.postMessage = (m) => {
  if (m.type === "error") error = m.message;
  if (m.type === "done") result = m.result;
};

new Function(fs.readFileSync(path.join(RAW, "studio.worker.js"), "utf8")).call(self);

/* ---- seats, from this chapter's own derived table ------------------------
 * seatchange.csv is quoted CSV with no embedded commas in the fields that
 * matter, so a split on "," is honest here; the header tells us which
 * columns to read rather than the position doing it. */
function readCsv(file) {
  const lines = fs.readFileSync(file, "utf8").trim().split(/\r?\n/);
  const head = lines[0].split(",").map((s) => s.replace(/^"|"$/g, ""));
  return lines.slice(1).map((ln) => {
    const cells = ln.split(",").map((s) => s.replace(/^"|"$/g, ""));
    return Object.fromEntries(head.map((h, i) => [h, cells[i]]));
  });
}

const payload = JSON.parse(fs.readFileSync(path.join(RAW, "studio.json"), "utf8"));
const byName = Object.fromEntries(payload.states.map((s) => [s.name, s]));

const seats = {};           // st -> { from, to, max }
for (const row of readCsv(path.join(__dirname, "derived", "seatchange.csv"))) {
  const s = byName[row.name];
  if (!s) continue;         // District of Columbia: in the table, no seat
  const from = +row["reps_" + Y1], to = +row["reps_" + Y2];
  if (!Number.isFinite(from) || !Number.isFinite(to))
    throw new Error("seatchange.csv: no seats for " + row.name);
  seats[s.st] = { from, to, max: Math.max(from, to) };
}
if (Object.keys(seats).length !== 50)
  throw new Error("expected 50 states, matched " + Object.keys(seats).length);

const sum = (f) => Object.values(seats).reduce((a, s) => a + f(s), 0);
if (sum((s) => s.from) !== 435 || sum((s) => s.to) !== 435)
  throw new Error("seats do not sum to 435 in both years");

/* ---- solve: carve at the peak count, place at the peak count ------------- */
const customSeats = Object.fromEntries(
  Object.entries(seats).map(([st, s]) => [st, s.max]));

self.onmessage({ data: { type: "init", payload } });
self.onmessage({
  data: {
    type: "solve",
    id: 1,
    params: {
      seatKey: "custom",
      customSeats,
      areaDivisor: AREA_DIVISOR,                   // not the studio default: see above
      padding: payload.defaults.padding,           // 2 px
      tweaks: true,
      groupNE: true,
      placement: "auto",                           // the hand-drawn slots
      cellMode: "balanced",                        // equal-area cells
      seed: payload.defaults.seed,
      pointsPerSeat: payload.defaults.pointsPerSeat,
      balanceIters: payload.defaults.balanceIters,
    },
  },
});

if (error) throw new Error("solver: " + error);
if (!result) throw new Error("solver returned nothing");
if (!result.cells) throw new Error("solver returned no cells");
const st = result.place;
if (!st.converged)
  throw new Error("placement did not converge (unmet " + st.unmet + " px)");
if (st.mode !== "slots")
  throw new Error(
    "the solver fell back to its '" + st.mode + "' layout: the hand-drawn slots " +
    "no longer fit these seat counts at areaDivisor " + AREA_DIVISOR + ". " +
    "Sweep the divisor and pin a value that still places on the slots.");
console.log(
  `placed ${result.bodies.length} states at ${sum((s) => s.max)} cells, mode=${st.mode}: ` +
  `${st.iterations} iterations, max drift ${st.moved.toFixed(1)} px ` +
  `(cap ${st.maxShift.toFixed(0)}), fidelity error ${st.fidelity.error.toFixed(1)}%, ` +
  `worst cell ratio ${result.cells.worstRatio.toFixed(2)} (${result.cells.worstSt})`
);

/* ---- hand the pieces to R ------------------------------------------------ */
const bodyOf = Object.fromEntries(result.bodies.map((b) => [b.st, b]));
const cellOf = Object.fromEntries(result.cells.states.map((c) => [c.st, c]));

const out = {
  frame: { width: payload.design.width, height: payload.design.height },
  // the hand-drawn layout's own units, which the label anchors are in
  design: { w: payload.design.w, h: payload.design.h },
  from: Y1, to: Y2,
  states: payload.states.map((s) => {
    const b = bodyOf[s.st], c = cellOf[s.st], k = seats[s.st];
    if (c.k !== k.max) throw new Error(s.st + ": carved " + c.k + " of " + k.max);
    return {
      st: s.st, fips: s.fips, name: s.name,
      seatsFrom: k.from, seatsTo: k.to, change: k.to - k.from, cells: k.max,
      centroid: s.centroid, label: s.label,
      cartogram: { scale: b.scale, tx: b.tx, ty: b.ty },
      outline: s.outline,
      // the solver drops a degenerate cell as null; none should appear, but
      // an empty ring travelling to R as `null` would be a silent hole
      cellPolys: c.cells.map((ring, i) => {
        if (!ring || ring.length < 3)
          throw new Error(s.st + ": cell " + i + " came back empty");
        return ring;
      }),
    };
  }),
  solve: {
    areaDivisor: AREA_DIVISOR, mode: st.mode,
    iterations: st.iterations, moved: st.moved, fidelity: st.fidelity.error,
    worstCellRatio: result.cells.worstRatio, worstCellState: result.cells.worstSt,
  },
};

fs.writeFileSync(path.join(__dirname, "derived", ".seatshift-solve.json"),
                 JSON.stringify(out));
console.log("wrote derived/.seatshift-solve.json");
