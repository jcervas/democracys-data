# Officeholder age by calendar year

`derived/ages.json` is the same Voteview ages as `derived/age_by_congress.csv`,
reshaped from **Congress** to **calendar year** so the page can plot 238 yearly
points instead of 119 steps. Section 9 of `build-data.R` writes it; there is no
second source and no second script.

An earlier version of this file was built from
[@unitedstates/congress-legislators](https://github.com/unitedstates/congress-legislators),
which carries exact birth *dates*. It was dropped on 2026-09-16: the exact dates
bought nothing this lab asks for, and that source has no birthdate at all for
542 members — 23% of the early Republic, against 2% missing in Voteview. Every
figure on the page got better by switching.

## Files

- `derived/ages.json` — the calendar-year series, 145 KB.
- `derived/ages-page.html` — a self-contained page (data inlined, no network
  calls): median age per chamber across all 238 years, click any year for that
  year's full age distribution. Also published at
  https://claude.ai/artifact/LSE6xNH78PtcKfUMFBV618

## Shape of derived/ages.json

`year → chamber → stats`, chambers keyed `rep`, `sen`, `prez`:

```json
"2026": {"sen": {
  "n": 103, "nb": 1, "min": 39, "c": [1,0,1,1,"..."],
  "med": 66, "mean": 65.2,
  "yng": ["Ossoff, Thomas Jonathan (Jon)", 39],
  "old": ["Grassley, Charles Ernest", 93] }}
```

`n` counts members with a birth year and `nb` those without; `c` is the
histogram in single years of age, where `c[i]` is the count aged `min + i` and
the counts sum to `n`. A chamber with no usable age keeps `n: 0` and its `nb`
rather than disappearing.

`prez` carries two extra keys. `all` lists every president of that Congress as
`[name, age]` in the order they took office — `yng`/`old` alone would not name
all three in a year like 1841. `unk` names presidents with no birth year, so the
page can say which president it cannot age instead of showing a hole.

## Method and known gaps

Congress *n* occupies calendar years 1787+2*n* and 1788+2*n*, and a member's age
in year Y is `Y - born`. Three consequences worth stating in class:

1. **Ages are year differences, not exact ages.** Voteview records a birth year,
   so an age here is the age a member *turns* during that year: correct for
   anyone whose birthday has passed, a year high for everyone else, about half a
   year high on average. Fine for medians and trends; useless for a claim about
   one person on one date.
2. **A year shows its Congress's whole roster.** A seat that changed hands
   mid-term shows both holders in both of the Congress's years, and a president
   who left in March belongs to the previous Congress — 1841 seated three
   presidents, but Van Buren sits with the 26th and only Harrison and Tyler
   appear under 1841.
3. **Some birth years are missing**: 93 of 12,633 legislators, who are left
   out of the medians and counted beside them as `nb`. Territorial delegates
   enter only with the 113th Congress, which is how Voteview carries them, so
   House counts sit near 435 throughout.

## The presidential birth years

Voteview has no birth year on any row for four presidents, and Cleveland carries
one on his first-term rows but not his second — the "birth year on some rows and
not others" line in `checks.csv`. Between them they left the page unable to age a
president for 23 years, Washington included, on a page named after him.

Section 9 repairs both, **and only for `ages.json`**. `members.csv`,
`presidents.csv` and `checks.csv` keep Voteview's `NA` exactly as it is, so the
lab's own validation output still reports four presidents with no birth year and
the brief's account of that gap stays true.

- **Cleveland** is filled from his own first-term rows. Not an override: it is
  Voteview's own value, reached through a different row.
- **Washington (1732), John Adams (1735), Taylor (1784), Hoover (1874)** come
  from the standard presidential record, entered as the `PREZ_BORN` table in
  section 9. Every president sourced this way carries a third element `true` in
  its `all` entry, and the page marks it †, so a patched age never passes as a
  Voteview value.

Washington and Adams are also the sharpest illustration of why a birth *year* is
slipperier than it looks: both were born under the Julian calendar. Washington's
22 February 1732 New Style was 11 February 1731/2 Old Style — a different year,
from the same birth.

Verification: for every one of the 119 Congress-start years, `ages.json`
reproduces the `n_age`, median and mean in `age_by_congress.csv` exactly —
238 chamber-years, zero mismatches.
