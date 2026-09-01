#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the tween-ready district geometry for Florida's mid-decade map.
# Companion chapter to ../mid-decade/, same method, following its own
# build-geo.py almost line for line -- see that file for the parts of the
# method common to every state (projection, resampling, canonical start).
#
# SOURCE: district boundary files exported from Dave's Redistricting
#   (https://davesredistricting.org), one per plan:
#     raw/FL-2022.geojson   the map enacted in 2022, used 2022-2024
#     raw/FL-2026.geojson   the map enacted in 2026, first used in 2026
#   Exports are manual browser downloads; the committed copies are the archive.
#
# One file comes out:
#
#   derived/fl-districts-tween.json   both plans' district outlines in one
#       file, projected to Florida's own plane grid and resampled so that
#       each pair's old ring and new ring have the same number of points.
#
# THE FLORIDA-ONLY WRINKLE: RENUMBERING. Texas kept its district numbers
# when it redrew; Florida's 2026 map renumbered three districts as part of
# the redraw (see CROSSWALK below), so "new district N's old ring" is not
# always "the old ring also numbered N." Get this pairing wrong and the
# 2022-lines panel comes up with holes where an unpaired old district never
# gets drawn, and doubled borders where two new districts both claim the
# same old one -- that is what an earlier, area-based version of this
# crosswalk actually did; see fl_overlap_pop.csv in this folder for why
# population, not land, is the right thing to weight by.
#
# Run from inside data/. Needs the mapshaper CLI for the projection step; if
# missing, the committed derived file is kept and the fact is said out loud.
# ---------------------------------------------------------------------------
import json, math, os, shutil, subprocess, sys, tempfile

EPSG_FL = 2777   # Florida GDL Albers, the plane grid Daily District uses

# Population-weighted, one-to-one crosswalk: new district id -> the old
# district id contributing the most of its POPULATION (not land area).
# Found once by build-block-crosswalk.R, which assigns all 390,066 of
# Florida's 2020 census blocks into both plans and solves the assignment
# problem (Hungarian algorithm) that maximizes total population carried
# across, subject to every district being used exactly once on each side --
# see derived/fl_crosswalk_pop.csv for the full result and
# derived/fl_overlap_pop.csv for every (old, new) pair's population share,
# not just the winning one. Recompute by rerunning that script if the
# committed FL-2022/FL-2026 plan files ever change.
#
# Only three districts move: population, unlike land, mostly did NOT follow
# Florida's renumbering. The redraw visibly reshuffled district lines across
# a much wider swath of the state (an area-weighted match once found seven
# districts "renumbered"), but most of that reshuffling nibbled at lightly
# populated edges. Where people actually live, all but a tight three-way
# rotation in South Florida kept its old number.
CROSSWALK = {22: 25, 23: 22, 25: 23}

OLD_SRC, NEW_SRC, DST = "raw/FL-2022.geojson", "raw/FL-2026.geojson", "derived/fl-districts-tween.json"

W = 800.0            # output frame width; height follows the state's shape
STEP = 2.0           # target spacing between resampled points, frame units
NMIN, NMAX = 96, 640 # points per ring: floor for tiny districts, cap for big


def project(src, epsg, tmpdir):
    """Reproject a plan to its state plane via mapshaper; drop label points."""
    dst = os.path.join(tmpdir, os.path.basename(src) + ".proj.json")
    subprocess.run(
        ["mapshaper", src,
         "-filter", 'this.geometryType !== "Point"',
         "-proj", "crs=epsg:%d" % epsg,
         "-filter-fields", "id",
         "-o", dst, "format=geojson", "precision=0.1"],
        check=True, capture_output=True)
    return dst


def load_rings(path):
    gj = json.load(open(path))
    out = {}
    for ft in gj["features"]:
        g = ft["geometry"]
        polys = g["coordinates"] if g["type"] == "MultiPolygon" else [g["coordinates"]]
        rings = [r for poly in polys for r in poly]
        # Most districts here are one ring, no holes, no islands. District 28
        # (the Keys) carries a small barrier island as a second, disconnected
        # ring in both plans; this schematic map keeps only the largest ring
        # per district and drops the rest -- an 800-unit-wide outline has no
        # room for an 83-point island anyway.
        did = ft["properties"]["id"]
        if len(rings) > 1:
            dropped = sum(len(r) for r in rings) - max(len(r) for r in rings)
            print("  .. district %s: %d rings, keeping the largest, "
                  "dropping %d points" % (did, len(rings), dropped))
        out[int(did)] = max(rings, key=len)
    return out


def signed_area(pts):
    s = 0.0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        x2, y2 = pts[(i + 1) % n]
        s += x1 * y2 - x2 * y1
    return s / 2


def perimeter(pts):
    n = len(pts)
    return sum(math.dist(pts[i], pts[(i + 1) % n]) for i in range(n))


def resample(pts, n):
    """Walk the closed ring and emit exactly n evenly spaced points."""
    per = perimeter(pts)
    step = per / n
    out = [pts[0]]
    acc, i, a = 0.0, 0, pts[0]
    target = step
    m = len(pts)
    while len(out) < n:
        b = pts[(i + 1) % m]
        seg = math.dist(a, b)
        if acc + seg >= target:
            t = (target - acc) / seg
            p = (a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t)
            out.append(p)
            a, acc, target = p, 0.0, step
        else:
            acc += seg
            a = b
            i += 1
    return out


def canonical_start(pts):
    """Rotate so index 0 is the topmost, then leftmost, vertex."""
    i0 = min(range(len(pts)), key=lambda i: (pts[i][1], pts[i][0]))
    return pts[i0:] + pts[:i0]


def build():
    with tempfile.TemporaryDirectory() as tmpdir:
        old = load_rings(project(OLD_SRC, EPSG_FL, tmpdir))
        new = load_rings(project(NEW_SRC, EPSG_FL, tmpdir))

    # a proper bijection by construction (solve_LSAP): every new id pairs
    # with a distinct old id, and every old id is used exactly once
    pairs = [(CROSSWALK.get(d, d), d) for d in sorted(new)]
    old_ids_used = sorted(oid for oid, _ in pairs)
    assert old_ids_used == sorted(old), "crosswalk is not a bijection: %s" % old_ids_used

    xs = [p[0] for rings in (old, new) for r in rings.values() for p in r]
    ys = [p[1] for rings in (old, new) for r in rings.values() for p in r]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    k = W / (x1 - x0)
    H = round((y1 - y0) * k, 1)

    def prep(ring):
        # into the frame (y flipped for SVG), closing vertex dropped,
        # wound clockwise so old and new always turn the same way
        pts = [((p[0] - x0) * k, (y1 - p[1]) * k) for p in ring]
        if pts[0] == pts[-1]:
            pts = pts[:-1]
        if signed_area(pts) < 0:
            pts = pts[::-1]
        return pts

    districts = []
    for old_id, new_id in pairs:
        a, b = prep(old[old_id]), prep(new[new_id])
        n = max(NMIN, min(NMAX, int(max(perimeter(a), perimeter(b)) / STEP)))
        ra = canonical_start(resample(a, n))
        rb = canonical_start(resample(b, n))
        flat = lambda pts: [round(v, 1) for p in pts for v in p]
        entry = {"id": new_id, "old": flat(ra), "new": flat(rb)}
        if old_id != new_id:
            entry["old_id"] = old_id
        districts.append(entry)

    out = {"state": "FL", "epsg": EPSG_FL, "viewBox": [int(W), H],
           "districts": districts}
    json.dump(out, open(DST, "w"), separators=(",", ":"))
    npts = sum(len(d["old"]) // 2 for d in districts)
    print("  FL: %d districts, %d points/plan, viewBox %s, %d KB" % (
        len(districts), npts, out["viewBox"], os.path.getsize(DST) // 1024))


os.makedirs("derived", exist_ok=True)

if shutil.which("mapshaper") is None:
    if not os.path.exists(DST):
        sys.exit("mapshaper is not installed and %s does not exist yet; "
                 "install mapshaper (npm install -g mapshaper) and rerun." % DST)
    print("  !! mapshaper is not installed; keeping the committed derived\n"
          "     geometry as it stands. Only the projection step needs it.")
else:
    build()

print("done.")
