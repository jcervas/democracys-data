# panhandle-claim — where the data came from

The chapter tests one claim: **Panhandle voters would not vote for the Black
candidate.** Byron Donalds won the Republican nomination for governor on
18 August 2026 and ran far behind his statewide share across north Florida.
`build-data.R` turns `raw/` into `derived/`; this file says what is in `raw/`
and what is wrong with it.

## The sources

### `raw/fldos/` — the 2026 primary

The state's own results file, all 67 counties, every office, from
`flelectionfiles.floridados.gov`, linked off the Downloads page at
**floridaelectionwatch.gov**. That address matters: the Division's older
results site is still live, still answers, and its election list stops at
November 2024. Ask it for 8/18/2026 and it returns a header row and no data,
which reads like "the state has published nothing" and is wrong.

Read it as **latin-1**. At least one candidate name is not ASCII and a UTF-8
read mangles it without complaining.

### `raw/fldos-past/` — 2018 and 2014

The benchmark, and the chapter's second test. These come from the results
archive's extract utility at `results.elections.myflorida.com`, which takes a
POST with the election date and returns tab-delimited text. 2018 is DeSantis
against Putnam — the last contested Republican primary for this office, and
the comparison the chapter rests on. 2014 is included because Rick Scott's
near-uncontested primary shows what a *narrow* county spread looks like.

### `raw/registration/` — who was actually eligible to vote in it

Two book-closing workbooks for this election, from the Division's voter
registration statistics page. They are the reason the chapter can count the
electorate rather than guess at it from census population.

One reports party registration **by precinct**, with no race; the other reports
party registration **by race**, only by county. Florida does not publish the two
crossed *in these summary tables*, and an earlier draft of this chapter wrongly
concluded from that the cross does not exist. It does — see the RPV section
below. What these two files give cheaply, without any request, is the county
composition the chapter's fourth test and its bounds both run on.

### `raw/enr-precinct/` — 46 counties' precinct returns

Kept, and used to show what the standard method would and would not add.
17 of the 29 north
Florida counties are here. Nothing indexes the per-county election ids behind
these addresses; they were found by scanning and are recorded in
`raw/enr_county_manifest.tsv`, whose `source_url` column holds the exact file
each came from. Flagler and Nassau published reports of the right shape with
every vote zero, and are kept that way, because a source that says nothing is
a different fact from a source that is absent.

## Why there is no RPV estimate

**This section was wrong in the first draft and the correction is the point.**
It claimed the racial composition of a precinct's Republican electorate "is not
a public number anywhere in the state." That is false, and it was reasoning
from the published summary tables to the underlying record — which is the
mistake this course spends a whole part warning about.

Florida's voter file is public by law, free, and carries **race, party and
precinct on every record**. Precinct-level Republican composition can therefore
be counted directly. No boundaries and no address matching are required. The
one real obstacle is a written request to the Division of Elections rather than
a download link, which `voter-file-access` measures: Florida scores open, no
login, no fee, application required.

So the method is available. It is not run here for a different reason, and a
better one.

**Where an electorate is nearly all white, the aggregate result nearly is the
white result.** 91.3% of the Panhandle's registered Republicans are white.
Duncan and Davis's method of bounds — the assumption-free arithmetic
`levels-of-aggregation` teaches — then pins white support for Donalds between
34.6% and 44.2% from the composition alone, a 9.5-point window. Statewide the
same calculation gives 28 points and is useless. The uniformity that makes an
estimate for *Black* Republicans hopeless is exactly what makes the *white*
one nearly exact, and the claim is about white voters.

`derived/bounds.csv` carries this, and the build asserts that the Panhandle
window stays under half the statewide one, because the argument depends on it.

The residual limit is attribution, not measurement. No file at any resolution
records why a voter chose as they did.

**The variation point still stands, for the group it applies to.** Across all
67 counties the Black share of registered Republicans runs 0.4% to 4.7%. An
estimate of how Black Republicans voted, drawn from that, is extrapolation.
The build asserts the range so a rebuild that changed it would fail rather
than quietly weaken the section.

## Traps in this data

**Field sizes are not comparable across contests.** Eleven candidates ran for
governor and two for agriculture commissioner. Raw shares mean different
things; each winner's deviation from their own statewide share does not. Every
cross-contest comparison in `derived/` is built on deviations for this reason.

**The candidates are tickets.** Governor and lieutenant governor run as a pair
in the general but are listed by the governor's surname in these primary
returns. The precinct exports in `raw/enr-precinct/` write the full ticket, so
a surname split there returns the running mate.

**Two spellings of one county.** The registration workbook writes `DeSoto`
where the results file writes `Desoto`, and the workbook ends with a statewide
`Total` row. Both are handled by joining on letters alone, and the join asserts
67 counties so that a third case fails loudly.

**The regions are a judgment.** `PANHANDLE` and `BIGBEND` are written out in
the build script, not inferred. The claim under test names one of them, and
keeping the Panhandle apart from the Big Bend is most of what the third test
does — the two behave very differently and are routinely lumped together as
"north Florida."

## Registration

- `panhandle-claim` is in `FROZEN` in `_lib/check-ai-prompt.py`; the brief
  calls `ai_prompt(tone = "frozen")`.
- It sits in `PARTS` in `_lib/make-index.py`, in part III after
  `levels-of-aggregation`, and appears in `INDEX.md` and `index.html`.
- `course-map.md` schedules it in Week 6.
- The chapter was built first under the slug `official-vs-ap`, as a comparison
  between the state's file and the Associated Press count. That comparison is
  now one paragraph of the source list. The AP files are still in
  `raw/npr-ap/`, unused by the build, kept because they are a frozen
  election-night snapshot that cannot be fetched again.
