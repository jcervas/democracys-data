#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the tween-ready district geometry for the mid-decade chapter.
#
# SOURCE: district boundary files exported from Dave's Redistricting
#   (https://davesredistricting.org), one per plan:
#     raw/TX-2022.geojson   the map enacted in 2021, used 2022-2024
#     raw/TX-2026.geojson   the map enacted in 2025, first used in 2026
#   Exports are manual browser downloads; the committed copies are the archive.
#
# One file comes out:
#
#   derived/tx-districts-tween.json   both plans' district outlines in one
#       file, projected to the state's own plane grid and resampled so that
#       district N's old ring and new ring have the same number of points,
#       the same winding, and the same starting vertex. The brief's map can
#       then slide every point from its old position to its new one, which
#       is what makes the boundary change visible as motion.
#
# Each state is projected to its own plane grid (the same per-state EPSG
# codes the Daily District site uses at build time), so shapes arrive
# undistorted and nothing in the browser projects anything. Texas is the
# first state; adding another is one line in PLANS plus its two raw files.
#
# Run from inside data/. Needs the mapshaper CLI for the projection step;
# if it is missing, the committed derived file is kept and the fact is said
# out loud, so the rest of the chapter still builds.
# ---------------------------------------------------------------------------
import json, math, os, shutil, subprocess, sys, tempfile

# State-plane / state-Albers EPSG per state, as used by Daily District.
EPSG_STATE = dict(
    AL=2759, AK=3338, AZ=2762, AR=2764, CA=3311, CO=2773, CT=2775, DE=2776,
    FL=2777, GA=2780, HI=2784, ID=2788, IL=2790, IN=2792, IA=2794, KS=2796,
    KY=2798, LA=2800, ME=2802, MD=2804, MA=2805, MI=2808, MN=2811, MS=2813,
    MO=2816, MT=2818, NE=2819, NV=2821, NH=2823, NJ=2824, NM=2826, NY=2829,
    NC=3358, ND=2832, OH=2834, OK=2836, OR=2838, PA=3362, RI=2840, SC=3360,
    SD=2841, TN=2843, TX=2845, UT=2850, VT=2852, VA=2853, WA=2855, WV=2857,
    WI=2860, WY=2863)

# (state, old plan file, new plan file, output file)
PLANS = [("TX", "raw/TX-2022.geojson", "raw/TX-2026.geojson",
          "derived/tx-districts-tween.json")]

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
        # Both Texas plans are one ring per district, no holes, no islands;
        # this fails loudly the day a plan arrives for which that is false.
        assert len(rings) == 1, "district %s has %d rings" % (
            ft["properties"]["id"], len(rings))
        out[int(ft["properties"]["id"])] = rings[0]
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


def build_state(st, old_src, new_src, dst):
    epsg = EPSG_STATE[st]
    with tempfile.TemporaryDirectory() as tmpdir:
        old = load_rings(project(old_src, epsg, tmpdir))
        new = load_rings(project(new_src, epsg, tmpdir))
    assert sorted(old) == sorted(new), "plans disagree on district numbers"

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
    for d in sorted(old):
        a, b = prep(old[d]), prep(new[d])
        n = max(NMIN, min(NMAX, int(max(perimeter(a), perimeter(b)) / STEP)))
        ra = canonical_start(resample(a, n))
        rb = canonical_start(resample(b, n))
        flat = lambda pts: [round(v, 1) for p in pts for v in p]
        districts.append({"id": d, "old": flat(ra), "new": flat(rb)})

    out = {"state": st, "epsg": epsg, "viewBox": [int(W), H],
           "districts": districts}
    json.dump(out, open(dst, "w"), separators=(",", ":"))
    npts = sum(len(d["old"]) // 2 for d in districts)
    print("  %s: %d districts, %d points/plan, viewBox %s, %d KB" % (
        st, len(districts), npts, out["viewBox"], os.path.getsize(dst) // 1024))


os.makedirs("derived", exist_ok=True)

if shutil.which("mapshaper") is None:
    missing = [d for _, _, _, d in PLANS if not os.path.exists(d)]
    if missing:
        sys.exit("mapshaper is not installed and %s does not exist yet; "
                 "install mapshaper (npm install -g mapshaper) and rerun."
                 % ", ".join(missing))
    print("  !! mapshaper is not installed; keeping the committed derived\n"
          "     geometry as it stands. Only the projection step needs it.")
else:
    for st, old_src, new_src, dst in PLANS:
        build_state(st, old_src, new_src, dst)

print("done.")
