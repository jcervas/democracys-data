# The shared base maps

GeoJSON base maps, one frame convention, used by every chapter that draws the
United States or one of the promoted states. A chapter never builds its own
state outlines: it reads one of these, so the national map, the equal-weight
map and the apportionment map look the same in chapter 3 as they do in
chapter 19.

**The promotion rule.** Geometry that at least two chapters draw, or any
statewide or nationwide base layer, lives here. Geometry that one chapter
uses analytically — the residual-votes chapter's full-resolution TIGER
shapefiles, the mid-decade chapters' tween geometry, the GA crosswalk blocks
— stays in that chapter's `data/`, and stays the source of record: promoted
copies here are *derived from* the chapter raw, read in place, never the
other way round.

## The nationwide set

| file | what it shows | units |
|---|---|---|
| `us-albers.geojson` | land | 50 states + DC (51 features) |
| `us-grid.geojson` | states equally | 50 states + DC (51 squares) |
| `us-apportionment.geojson` | House seats | 50 states (no DC — no voting seat) |
| `us-counties.geojson` | 2020 counties | 3,143 (50 states + DC) |
| `us-cd-119.geojson` | congressional districts, 119th Congress | 436 (435 seats + DC delegate) |

## The per-state set, `states/`

| file | what it shows | units |
|---|---|---|
| `states/TX-2022.geojson` | TX congressional plan enacted 2021 | 38 districts |
| `states/TX-2026.geojson` | TX plan enacted 2025, first used 2026 | 38 districts |
| `states/FL-2022.geojson` | FL plan used 2022–2024 | 28 districts |
| `states/FL-2026.geojson` | FL plan first used 2026 | 28 districts |
| `states/GA-vtd-2020.geojson` | GA precincts (VTDs), Nov 2020 general | 2,655 |
| `states/GA-vtd-2024.geojson` | GA precincts, Nov 2024 general | 2,698 |

Each state file is re-projected so the STATE fills the standard plane — a
per-state frame, recorded in the registry (below). The two files of a pair
(TX-2022/TX-2026, FL-2022/FL-2026, GA-vtd-2020/GA-vtd-2024) share one fit,
computed over the union of both vintages' bounding boxes, so old and new
overlay exactly and a figure can tween between them without refitting.

## The frame

All files use the same coordinate space: **1152 × 748.8, y down**, the SVG
convention, coordinates rounded to 0.1 (0.01 for the cartogram — see the
repair note below). Each file records this in a top-level `frame` member.
The coordinates are **pre-projected plane units** — nothing that reads these
files projects anything, which is how a D3 figure and a base-R figure are
guaranteed to draw the same map.

D3:

```js
const path = d3.geoPath(d3.geoIdentity());
// svg viewBox: 0 0 1152 748.8
```

Base R:

```r
plot(NULL, xlim = c(0, 1152), ylim = c(748.8, 0), asp = 1, axes = FALSE)
# then polygon() over each ring
```

Geometry uses `Polygon` where a feature has one polygon and `MultiPolygon`
otherwise — a reader should branch on `geometry.type` (d3.geoPath and the
snippet in `verify-geo.R` both do).

## Properties

**State features** (`us-albers`, `us-grid`, `us-apportionment`) carry `st`
(USPS), `fips`, `name`, and `label_x`/`label_y` — an anchor that falls inside
the shape (point-on-surface for the Albers map, the square's centre for the
grid, the scaled centroid for the cartogram). The grid adds `col`/`row`; the
cartogram adds `seats` (= `seats_2020`) plus `seats_2000`, `seats_2010`,
`seats_2020`, `seats_2030` for the reapportionment chapters.

**`us-counties.geojson`**: `GEOID` (5-digit FIPS), `NAME`, `STATEFP`. No
label anchors — 3,143 of them would be a third of the file and nothing draws
them at national scale.

**`us-cd-119.geojson`**: `GEOID` (4-digit state+district), `STATEFP`, `CD`
(the district number as the Census writes it: `"01"`…, `"00"` = at-large,
`"98"` = the DC delegate seat), `st`, `label_x`/`label_y`.

**TX/FL plan files**: `st`, `district` (integer), `name` (`"TX-01"`),
`label_x`/`label_y`. Only identity — the vote shares and demographics in the
Dave's Redistricting exports stay in the chapters that analyze them.

**GA VTD files**: `id` (CTYSOSID — county FIPS + precinct, the SoS key),
`fips2` (3-digit county FIPS), `county`, `precinct`, `name`,
`label_x`/`label_y`. The raw shapefiles carry duplicate CTYSOSID rows
(noncontiguous precincts split into parts); the build dissolves on the id so
it is a real key, which is why the counts (2,655 / 2,698) are below the raw
row counts (2,684 / 2,724). Election returns stay in the ga-precinct-returns
chapter.

## us-frame.json — the frame registry

Two things in one file, for compatibility.

**The top level** is the original single frame record, written by
`build-geo.R` and read by the wind-map and mapping chapters: the three
projection strings, the km-to-frame fit (`fit_*`), and `conus_box` — the
CONUS states' own box on the frame, which is the crop a CONUS-only figure
wants for its viewBox. These keys will not move. A chapter that projects its
own geometry applies

```
frame_x = fit_ox + (X - fit_xmin) * fit_s
frame_y = fit_oy + (fit_ymax - Y) * fit_s
```

to coordinates projected with `conus_proj`, and its drawing then overlays
these files exactly. The mapping chapter does this at `frame_scale` 2 — two
canvas units to the frame pixel — so its vertices stay integers; divide by
two to come back to frame pixels.

**`frames`** is the registry: one entry per shared file, keyed by filename.
Each entry records `file`, `kind` (`nation`/`state`), `frame`, the projection
(`proj`), the fit (`fit`: `xmin`/`ymax`/`s`/`ox`/`oy`, same formula as
above), the drawn `bounds` on the frame, `features`, `id_property`, `source`,
`vintage` and `built`. Nation entries add `insets` — the Alaska and Hawaii
affines (`X' = scale·X + dx`, in projected km, applied *before* the fit),
which build-geo.R derives from bounding boxes and nothing else records.
State entries add `st` and `shares_frame_with`. The two abstract cartograms
(`us-grid`, `us-apportionment`) have no `proj`/`fit` — their coordinates are
drawn on the frame, not projected onto it.

## Where each map comes from

**`us-albers.geojson`** — built by `build-geo.R` from the Census cartographic
boundary file (the generalized TIGER/Line), 1:5,000,000, 2024 vintage — the
same vintage the county maps use. Composite Albers: CONUS in Albers
29.5/45.5 centred on −96 (the wind-map chapter's parameters), Alaska in
Albers 55/65 at 0.35 scale, Hawaii in Albers 8/18, both moved into the ocean
south of the mainland. Simplified once with mapshaper *before* projecting, so
shared borders keep shared vertices — never per-ring thinning, which opens
triangular holes at every state junction (the wind-map build records the
incident).

**`us-grid.geojson`** — built by `build-geo.R`. One 84 px square per state,
DC included, in the tile-grid positions the electoral-map chapter uses.

**`us-apportionment.geojson`** — built by `build-apportionment.js`, which runs
the real cartogram-studio solver (jcervas.github.io/maps/cartogram-studio/)
headless in Node: each state scaled so drawn area is proportional to its
2020-census House seats, placed on the hand-drawn slots, relaxed to a 2 px
gap. Solver defaults throughout (areaDivisor 2.9, tweaks on, New England
grouped); the placement converges on the slots with < 2 px of drift. The
solver, its driver and its geometry are committed in `raw/` so this build
does not depend on the cartograms repository being present.

The node build finishes by running `repair-apportionment.R`: the studio's
outlines were quantised to 0.5 px for browser drawing, which left
self-touching rings in 30 states — invisible when filled, fatal to planar
operations. `st_make_valid` repairs them (no state's area moves more than
0.008 px²), and the result is re-rounded to 0.01, the coarsest rounding that
stays valid. All three files pass `st_is_valid`, and the cartogram's claim
holds in the shipped geometry: drawn area per seat is uniform to 0.3%.

**`us-counties.geojson`** — built by `build-layers.R` from the TIGER/Line
2020 county shapefile the residual-votes chapter committed
(`labs/03-elections/residual-votes/data/raw/tiger/us/`), read in place.
Same composite Albers, same frame, same inset placement — the build re-runs
the state pipeline to recover the inset affines and *asserts* the recomputed
fit matches `us-frame.json` before writing anything. Simplified hard with
the mapshaper CLI (weighted Visvalingam, keep-shapes, 0.15% of removable
vertices) — at national scale a county is a few pixels across. Two caveats a
consumer should know: TIGER county boundaries are legal boundaries and run
~3 nm offshore, so a thin water fringe sits outside the `us-albers`
coastline (draw states on top, or fill and let the coast mask it); and
Honolulu County is cropped to the main islands — its legal extent runs to
Kure Atoll, 25° west of Oahu, which the HI inset was never sized for.
Intended consumers: the mapping chapter's national choropleths, residual
votes, any county-level nationwide figure.

**`us-cd-119.geojson`** — built by `build-layers.R` from the Census
cartographic boundary file `cb_2024_us_cd119_500k.zip`
(www2.census.gov/geo/tiger/GENZ2024/shp/), fetched at run time into a temp
directory (set `GEO_TMP` to choose where) and recorded in `PROVENANCE.tsv` —
the zip itself is not committed. Territories' delegate districts are dropped
(no inset in the composite); the DC delegate seat is kept. Same projection,
insets and simplification doctrine as the counties. Intended consumers: the
redistricting and apportionment chapters, any district-level national
figure.

**`states/`** — built by `build-states.R`, all sources read in place from
their chapters. TX and FL plans are the Dave's Redistricting exports
committed by the mid-decade and mid-decade-florida chapters (manual browser
downloads; the committed copies are the archive). GA precincts are the
Georgia SoS shapefiles committed by the ga-precinct-returns chapter (a
manual, gated download — see that chapter's `data/README.md`); the 2020
file's bare GRS-1980 CRS is assigned NAD83, which is what it is in
everything but name. All are simplified once with mapshaper before
projection (plans at 10%, VTDs at 3% of removable vertices), projected with
the shared `conus_proj`, and fitted per state as described above. If a
source is missing (GA's gated raw, say) the build says so and skips that
state rather than failing the rest. Intended consumers: mid-decade,
mid-decade-florida, ga-precinct-returns, redistricting — any figure that
draws one of these states wall to wall.

## raw/

| file | source | sha256 (first 12) |
|---|---|---|
| `cb_2024_us_state_5m.zip` | www2.census.gov/geo/tiger/GENZ2024/shp/ | `c9db0e395c11` |
| `studio.json` | cartograms repo, `web/` (generated by its R build) | `dc2514045a84` |
| `solver.js` | cartograms repo, `web/` | `3684047df80d` |
| `studio.worker.js` | cartograms repo, `web/` | `0b8aa95fed78` |

The three studio files were copied 2026-08-17 from the reconstructed
cartograms repository (`~/…/GitHub/cartograms`), which is itself a faithful
copy of what is published at jcervas.github.io. The walkthrough file
`placement/placement.json` in that repo was **not** used: its seat counts sum
to 448 across mixed years — it is a demo state of the interactive page, not
an apportionment.

The later builds add nothing to `raw/`: the county shapefile is read from
the residual-votes chapter, the CD zip is fetched to a temp dir, and the
state sources are read from their chapters. `PROVENANCE.tsv` records all of
them — downloads by URL, promoted local sources by `file:` path — and
`BUILD-STAMP.tsv` records which script wrote which output, when, at what
hash (the chapter data contract, mirrored here).

## Rebuilding

```
Rscript build-geo.R          # us-albers, us-grid   (re-fetches the zip if raw/ is empty)
node build-apportionment.js  # us-apportionment     (offline, reads raw/ only)
Rscript build-layers.R       # us-counties, us-cd-119, registry  (needs the mapshaper CLI)
Rscript build-states.R       # states/*, registry               (needs the mapshaper CLI)
Rscript verify-geo.R         # reads everything back, checks bounds/counts/ids
```

Rebuilds are deterministic: the solver seeds from `studio.json`'s recorded
defaults, the R builds re-download files whose vintage is pinned in their
names, and `build-layers.R` refuses to run if the state pipeline no longer
reproduces the recorded frame fit. `PROVENANCE.tsv` is written by
`prov_fetch()` when the build runs where `../provenance.R` resolves (i.e.
installed at `labs/_lib/geo/`). `geo-common.R` holds the shared frame
arithmetic; `build-geo.R` predates it and is self-contained.
