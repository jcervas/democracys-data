# Officeholder age by calendar year

A companion series to this lab's main Voteview-based files, not a replacement.
The lab's `data/derived/age_by_congress.csv` is organized **by Congress** and
built from `HSall_members.csv`, which records `born` as a **year** — so ages
there carry about a year of slop. This folder is organized **by calendar year**
and built from exact **birthdates**.

| | this folder | `../data/derived/` |
|---|---|---|
| source | @unitedstates/congress-legislators | Voteview `HSall_members.csv` |
| birth field | full date | year only |
| unit | calendar year, 1789–2026 | Congress, 1–119 |
| presidents | `executive.json` | rows in the same member file |

## Files

- `build-ages.py` — fetches the three source files into `data/raw/` (gitignored
  repo-wide) and writes `ages.json`. Run it from anywhere: `python3 build-ages.py`.
  No dependencies beyond the standard library.
- `ages.json` — the derived series, 140 KB.
- `ages-page.html` — a self-contained page (data inlined, no network calls):
  median age per chamber across all 238 years, click any year for that year's
  full age distribution. Also published at
  https://claude.ai/artifact/LSE6xNH78PtcKfUMFBV618

## Shape of ages.json

`year → chamber → stats`, chambers keyed `rep`, `sen`, `prez`:

```json
"2026": {"sen": {
  "n": 102, "min": 39, "c": [1,1,0,0,1,"..."],
  "med": 66.0, "mean": 64.9,
  "yng": ["Jon Ossoff", 39], "old": ["Chuck Grassley", 92], "nb": 0 }}
```

`c` is the histogram in single years of age: `c[i]` is the count aged `min + i`,
and the counts sum to `n`. `nb` is the number serving that year whose birthdate
is unknown — counted here, excluded from everything else.

`prez` carries one extra key, `all`: every president who held office that year,
in the order they held it, as `[name, age]`. `yng`/`old` alone do not name them
all — three presidents held office in 1841 (Van Buren, Harrison, Tyler) and again
in 1881 (Hayes, Garfield, Arthur).

## Method and known gaps

Anyone holding a seat at any point in year Y is counted in Y, aged as of 1 July Y
— or, if their service that year ended earlier, as of their last day. Someone who
switches chamber counts once in each. House counts include non-voting delegates,
so they run above 435; mid-year turnover pushes them higher still (445 in 2026).

Two caveats worth stating in any classroom use:

1. **542 members have no recorded birthdate**, nearly all antebellum. They are
   excluded from the median, mean and histogram, and counted in `nb`. The gap is
   worst in the early Republic — 28 of 87 representatives in 1800 — and closes
   entirely after 1981. Early-Republic medians rest on partial data.
2. **Six records carry impossible birthdates** (implying an age under 22 at
   swearing-in; three of them imply a birth year at or after the term began).
   They are dropped. The cut is set at 22 because the youngest person ever
   actually seated, William C. C. Claiborne, was 22.

Source data is CC0. Totals: 12,770 members plus 45 presidents, 112,497
person-years. Fetched 2026-09-15.
