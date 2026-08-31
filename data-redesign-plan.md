# Redesign plan — master data and book structure

Written Aug 12 2026, before any files were changed. Nothing in this document
has been executed.

> ### Addendum, Aug 16 2026 — the move, the tiers, and what "publish" means
>
> Three decisions taken after this plan was written; each touches text below.
>
> 1. **The corpus moved.** `F26/` was renamed to `book/` at the root of
>    `_Democracy's Data/`, one atomic rename, internal structure unchanged.
>    Every `F26/...` path in this document now reads `book/...`. Depth is
>    identical, so the five-level relative paths to `_syllabus-template/`
>    still resolve.
>
> 2. **A third data tier exists: `book/tables/`.** The audience-facing name
>    for the student tier — cleaned, analyzable, self-contained spreadsheets.
>    Per-chapter `raw/` and `derived/` are unchanged and stay workshop-only.
>    `tables/` is the distributable face of §4's master layer; its README
>    carries the rules. §4.1's `F26/data/` remains the workshop (build
>    scripts, cache, dictionary).
>
> 3. **"Publish" means enrolled students, not the internet.** Distribution
>    is Canvas or a private repository. This re-settles the licence posture
>    of §4.5.3 a second time: the wall there was about *public republication*
>    (GitHub Pages), which is not the plan. Course distribution to enrolled
>    students at a member institution is a different licence question, and a
>    friendlier one — the ICPSR door in particular is no longer closed by
>    §4.5.3's reasoning, though the four non-licence objections to the long
>    county series still stand. The gate for what enters `tables/` is the
>    Phase 0 audit read against *course* distribution, not publication. The
>    standing exclusions (ANES cumulative, Jacobson roll calls) are
>    unaffected: those are not ours to hand out in any channel.

The corpus was surveyed first; every number below is measured from disk, not
estimated.

---

## 1. What is actually there

**59 briefs** in `F26/labs/`, **1.3 GB**, **322 CSVs**, and roughly **50
independent build scripts**. Each chapter is fully self-contained: it fetches
its own raw data, derives its own tables, and commits them next to its own
`.Rmd`. `_lib/provenance.R` and `_lib/structure.R` are the only shared code.

### The footprint, by kind

| Kind | Size | Note |
|---|---:|---|
| Shapefiles (`.shp`, `.dbf`) | 607 MB | TIGER, downloaded separately by 7 chapters |
| Archives (`.zip`) | 433 MB | block, precinct and PL 94-171 downloads |
| JSON | 43 MB | GA SoS exports, Census API captures |
| **CSV** | **72 MB** | *the only part anyone reads* |
| Rendered `.html` + `.pdf` | 44 MB | build output |

**1,040 MB of 1,300 MB is raw spatial data that no chapter reads directly.**
It exists only as input to a build script. That single fact is what makes the
storage decision easy.

### The duplication, measured

| Symptom | Evidence |
|---|---|
| Byte-identical copies | `pres2024_states.csv` has md5 `05279948e…` in **four** chapters — `electoral-map`, `data-sources`, `campaign-visits`, `poll-simulation` |
| One source, nine fetchers | tonmcg / jaytimm county returns are pulled independently by `data-sources`, `electoral-map`, `election-night`, `historical-campaigns`, `mapping`, `poll-simulation`, `redistricting`, `wind-map` (×2 scripts) |
| One census file, four schemas | 2020 PL 94-171 county race is `census_counties.csv` (4 cols), `pl94171_counties.csv` (26 cols, **504 rows only**), `demographics/counties.csv` (28 cols), `surnames/county_race.csv` (10 cols) — same file upstream, four vocabularies, no way to tell |
| TIGER seven times | `residual-votes` 226 MB · `redlining` 214 MB · `areal-units` 81 MB · plus `mapping`, `migration`, `census-geography`, `demographics` |
| The ladder is not one dataset | state returns in one chapter, county in another, congressional district in a third, precinct in a fourth — four schemas for one election |

### What is already right, and must survive

- **The thesis exists and is written.** `introduction-brief.Rmd`: *"why every
  chapter begins with the file rather than the question."* The book is about
  provenance. That is the through-line; it does not need inventing.
- **The SOURCE-chapter pattern exists.** Four chapters already do the job of a
  part opener: `returns-source`, `census-source`, `surveys-source`,
  `voter-files-source`. Each asks what its instrument can establish that
  nothing else can, and what it can never establish at any level of detail.
- **The ten-step brief form**, one numbering across brief/lab/key, HTML+PDF
  from one file.
- **`_lib/provenance.R`**, which already caught the AP tracker going stale.
- **The introduction computes itself from the corpus** — it counts chapters by
  opening them. Adding a chapter changes its prose with no editing.

### The actual diagnosis

The through-line problem and the master-data problem are one problem. A student
cannot see that Session 1's county file and Session 14's precinct file are the
same election, because they are different files, with different column names,
built by different scripts, that were never introduced to each other. The book
argues that provenance is everything, and then ships 322 files whose provenance
relationships are invisible.

**The introduction already reaches across chapters** — it reads
`data-sources/data/pres2020_counties.csv`, `policing/data/by_race.csv`,
`false-matches/data/facts.csv`, `demographics/data/facts.csv` — which proves
the shared layer is wanted, and that it is currently done by relative path into
someone else's folder.

---

## 2. Decisions taken

Settled before drafting:

1. **Spine** — provenance parts, with the unit-of-analysis ladder as a
   recurring motif inside every part.
2. **Scope** — canonical core first. Master files for the datasets that drive
   most reuse; singular chapters stay standalone but adopt the conventions.
3. **Storage** — commit derived tables, fetch raw into one shared ignored
   cache.
4. **Anchors** — Georgia primary, Pennsylvania as the local secondary.

---

## 3. The book

### 3.1 One amendment to the five parts

The five parts as chosen put **25 of 59 chapters in Part I**. That is not a
part, it is a second book. The state keeps two different kinds of record and
they teach different lessons, so Part I splits along the seam that is already
there:

- a record of **an outcome** — who won, complete, official, and permanently
  silent about who did it;
- a record of **a person** — a status held by a named individual, kept to
  administer something, and reused for purposes it was never built for.

`returns-source` and `voter-files-source` already exist as the two openers.
**Six parts.**

### 3.2 The parts

Each part opens with its SOURCE chapter, which introduces the instrument and
the master table. Every chapter after it is a question asked of that same
table.

| Part | Opens with | Chapters | What the part is about |
|---|---|---:|---|
| **I — The record of an outcome** | `returns-source` ★ | 18 | Certified returns at every rung: nation, state, district, county, precinct, ballot. Complete, official, and silent about who voted how. |
| **II — The record of a person** | `voter-files-source` ★ | 7 | Registration, vote history, and other administrative files kept about named individuals — including three kept by institutions other than the election system. |
| **III — Counting people** | `census-source` ★ | 11 | Decennial, ACS, CPS, and the geography that carries them. Enumeration rather than elicitation, and the classification decisions inside it. |
| **IV — Asking people** | `surveys-source` ★ | 9 | ANES, GSS, CES, public polls. What a sample can establish that no record can, and what no sample size will buy. |
| **V — Scores and constructs** | `scores-source` ✚ **new** | 7 + 1 | DW-NOMINATE, SCDB ideal points, BISG, ecological inference, algorithmic districting. A score is a model, not a measurement. |
| **VI — Compelled and commercial** | `disclosure-source` ✚ **new** | 6 + 1 | FEC filings, lobbying disclosures, prediction markets, pageviews, press trackers. Data produced because someone was required to file it, or because someone sells it. |

Plus `introduction` as front matter. **All 59 existing chapters placed; 61
with the two new source chapters.**

Part I is still the heaviest at 18, and three of those are arguable — see
**open question A** in §7.

### 3.3 Full placement

**Part I — The record of an outcome** (18)
`returns-source` ★ · `data-sources` · `levels-of-aggregation` ·
`electoral-map` · `mapping` · `historical-campaigns` · `wind-map` ·
`house-competition` · `midterm-loss` · `primary-defeats` · `retirements` ·
`vote-targeting` · `election-night` · `ga-precinct-returns` ·
`precinct-geography` · `cast-vote-records` · `residual-votes` · `eavs`

**Part II — The record of a person** (7)
`voter-files-source` ★ · `voter-files` · `false-matches` ·
`disenfranchisement` · `policing` · `jury-selection` · `redlining`

The last three are the DC-designation justice chapters. They belong here on the
argument the part makes: a police stop log, a court's strike record and a 1939
HOLC appraisal are all records kept about people by an institution, for its own
administrative purpose, and then read for a purpose their makers never
intended. That is the same lesson as the voter file, in three other rooms.

**Part III — Counting people** (11)
`census-source` ★ · `census` · `census-api` · `apportionment` ·
`regional-shift` · `demographics` · `census-geography` · `areal-units` ·
`migration` · `section-203` · `surnames`

**Part IV — Asking people** (9)
`surveys-source` ★ · `surveys` · `anes` · `gss-confidence` · `ces-class` ·
`party-id` · `ideology` · `abortion-opinion` · `poll-simulation`

**Part V — Scores and constructs** (7 existing + `scores-source`)
`scores-source` ✚ · `dw-nominate` · `scdb` · `officeholder-age` ·
`bisg-check` · `rpv` · `vote-dilution` · `redistricting`

**Part VI — Compelled and commercial** (6 existing + `disclosure-source`)
`disclosure-source` ✚ · `campaign-finance` · `independent-expenditures` ·
`lobbying` · `models-markets` · `media-attention` · `campaign-visits`

### 3.4 The two new source chapters

Both are short and both complete a pattern the book already established.

**`scores-source` — "A Number Somebody Built"**
The instrument is a *model*. DW-NOMINATE, Martin–Quinn, BISG and ecological
inference all output numbers that look like measurements and are not: they have
no unit, they cannot be audited against a truth in general, and they change
when the estimation changes. The chapter's turn is that the course already
holds two cases where the truth *is* available — BISG graded against Georgia's
self-reported race (40.3% right for Black voters) and ecological inference
graded against Georgia's cast vote records (194.0% against a truth of 64.5%) —
so the part can do the thing the literature usually cannot: grade the score.
`m-q-2024.csv` in the course root is the Martin–Quinn series and belongs here.

**`disclosure-source` — "Filed Because Somebody Had To"**
The instrument is a *compliance artifact*. A disclosure exists because a
statute compelled it, which fixes what it contains, what it rounds to, and what
it is built not to record. Three findings already in the corpus are the
chapter: lobbying reported to the nearest ten thousand with one dollar figure
across 17 issues; 16 of 73,449 FEC rows carrying 90.4% of the dollars, and
reversing support/oppose from 94.5/5.5 to 42.4/57.6; and the AP tracker that
silently stopped three days before the election. Prediction markets and
pageviews sit beside them as the commercial counterpart — data nobody was
required to produce and somebody chose to sell.

### 3.5 The ladder as recurring motif

Every part carries a short, identically-formed section placing its source on
the aggregation ladder. Part I runs the ladder end to end; the others say where
their instrument sits and what it cannot see below that rung.

```
one ballot → precinct → county → congressional district → state → nation
```

This is what makes the master returns table pay off pedagogically: it is
literally one table, and the reader can watch the same election answer
differently at each rung.

---

## 4. The data layer

### 4.1 Location and shape

```
F26/data/
  README.md               how to use it, how to add to it
  dd.R                    the accessor — the only way chapters read master data
  DICTIONARY.tsv          every table, every column: type, level, source, note
  PROVENANCE.tsv          rolled up from all build scripts
  build/                  one script per master table, named for the table
  _cache/                 raw downloads — GITIGNORED, fetched on demand

  returns/                census/                 congress/
  geography/              opinion/                money/
  admin/
```

### 4.2 The master tables

Names are proposals. Coverage reflects what the corpus already has on disk;
nothing here requires a new source.

**`returns/`**

| Table | Grain | Coverage |
|---|---|---|
| `pres_state` | year × state × party | 1864–2024, 41 elections (from `historical-campaigns`, via jaytimm) |
| `pres_county` | year × county × party | **Target 1952–2024** — ICPSR 13 → MEDSL → tonmcg, with 1992/96 open. Only 2020 and 2024 on disk today. See §4.5.2 |
| `pres_cd` | year × district × party | 1952–2024 (from `house-competition`, `redistricting`) |
| `house_cd` | year × district × candidate | 1946–2024 (the Clerk series, 40 elections) |
| `state_office_county` | year × county × office × party | NC gov (`vote-targeting`), GA (`levels-of-aggregation`) |
| `precinct_ga` | year × precinct × candidate × vote method | 2020, 2020 recount, 2024 |
| `cvr_ak` | ballot × ranking | 2022 AK special |

**`census/`**

| Table | Grain | Coverage |
|---|---|---|
| `decennial_county` | county × race/ethnicity | 2020 PL 94-171, all 3,144 |
| `decennial_tract_ga`, `decennial_block_ga` | tract / block | 2020, Georgia |
| `apportionment` | decade × state | 1910–2020 |
| `acs_county` | county × variable × estimate + MOE | 5-year, current vintage |
| `cps_voting` | year × group | 1964–2024 turnout supplement |
| `cps_mobility` | year | 1948– |
| `surnames` | name × race probability | 2010 Census surname list |

**`geography/`**

| Table | Note |
|---|---|
| `states` | fips, abbrev, name, region, EV, cartogram row/col |
| `counties` | geoid, name, state, land area, centroid, **vintage** |
| `cd` | district, state, congress number |
| `xwalk_block_precinct_ga` | population-weighted, both directions |
| `tiger/` | **cache only** — one copy, replacing seven |

**`congress/`** — `members` (1789–), `nominate`, `scdb`, `martin_quinn`
**`opinion/`** — `anes_cumulative`, `gss`, `ces`, `polls_2024`
**`money/`** — `fec_candidates`, `fec_ie`, `lda_filings`
**`admin/`** — `eavs`, `voterfile_ga`

### 4.3 Six rules

1. **One vocabulary.** The same thing has the same column name everywhere:
   `year`, `geoid`, `state_fips`, `state_abbr`, `county_fips`, `cd`,
   `precinct_id`, `office`, `party`, `votes`, `votes_total`. The current
   `fips` / `county_fips` / `GEOID` / `stcd` / `geoid` spread ends.

2. **Every geographic identifier is character, always.** Enforced by the
   loader, not by each chapter remembering. `NBAD` in the introduction — the
   count of rows a numeric read would break — stays true and stays quotable,
   because the introduction will demonstrate it *against the loader's own
   guarantee*.

3. **Long, not wide.** Returns tables are long on year and party, with a
   `dd_wide()` helper. `pres2020_counties.csv` and `pres2024_counties.csv` stop
   being separate files.

4. **Geographic vintage is a column, never a filename.** `counties` carries
   `vintage`, and the DC 11001 problem becomes a documented, joinable field
   instead of a trap only one chapter knows about. **This makes the
   introduction's opening exhibit stronger, not weaker:** the reader is shown
   the field that would have caught it, sitting in the table, unused by the
   naive join.

5. **Every table has a `DICTIONARY.tsv` row and a `PROVENANCE.tsv` entry.** A
   table with no dictionary row is a build error, not a warning.

6. **Chapters never write to master data.** A chapter's `data/` holds only
   figure-ready derivatives — the existing `facts.csv` / `fig_*.csv` pattern,
   which already works well and should become universal.

   **Done, Aug 12 2026, ahead of the rest of this plan.** Every chapter's
   `data/` is now `raw/` + `derived/` with the build script between them, and
   the split is enforced by `labs/_lib/check-layout.py`. The convention is
   written down in `labs/DATA-LAYOUT.md`. This does not depend on the master
   tables below and did not wait for them: it is the same distinction one
   level down, and it makes the master layer a smaller move when it comes,
   because every chapter already separates what arrived from what it made.

7. **Provenance is per row, not per table.** Added after the Aug 12 decisions.
   `pres_county` will span **three or four upstreams** — ICPSR 13 (1952–1990),
   whatever resolves 1992/96, MEDSL (2000–2004), tonmcg (2008–2024) — so a
   table-level citation would be a lie about most of its rows. Every returns
   row carries:

   | Column | Holds |
   |---|---|
   | `source_id` | key into `PROVENANCE.tsv` — which upstream this row came from |
   | `coverage` | `full` · `partial` · `absent`, so gaps are stated, never implied |
   | `unit_kind` | `county` · `house_district` · `city` — **Alaska is never silently a county** |
   | `third_party` | whether `votes_total` includes non-major candidates |

   That last column exists because of what the 2024 file did: third-party votes
   present in `total_votes` for all 51 states in 2020 and absent for two of them
   in 2024, from the same compiler, with no warning. **A boolean makes the
   defect queryable instead of fatal**, and `dd_check()` can then refuse to
   compare two vintages that disagree on it.

   This is the single most important schema consequence of taking historical
   county returns seriously, and it is worth more than the extra coverage.

### 4.4 The accessor

```r
source("../../data/dd.R")

r <- dd_load("returns/pres_county", years = c(2020, 2024))
g <- dd_load("geography/counties", vintage = 2024)
dd_dict("returns/pres_county")     # the dictionary rows, as a table
dd_cite("returns/pres_county")     # the provenance line, for the Sources section
```

`dd_load()` resolves paths from any depth, sets `colClasses` so identifiers
stay character, and fails loudly with the table's dictionary entry if a chapter
asks for a column that does not exist. `dd_cite()` means the "Sources" section
at the foot of every brief is generated from provenance rather than typed —
which is the same discipline the introduction already applies to its own counts.

### 4.5 Historical county returns, and why `pres_state` stays separate

Two questions raised Aug 12, answered here because both change the returns
family.

#### ICPSR series 00059 as the source for historical `pres_county`

The **United States Historical Election Returns Series**: ICPSR 1 (1824–1968),
ICPSR 13 (1950–1990), ICPSR 79 (1788–1823). County-level returns for president,
governor, senator and representative, over 90% of all elections, all parties and
candidates. It would extend county coverage from 2 elections to roughly 45.

**Adopt it, with three costs written down rather than discovered later.**

1. **It does not close the gap it appears to close.** ICPSR ends **1990**;
   tonmcg begins **2008**. **1992–2006 is a hole of four presidential
   elections.** MEDSL's *County Presidential Election Returns 2000–2020*
   (Harvard Dataverse) covers 2000 and 2004 and needs no key. **1992 and 1996
   come from state canvasses — decided Aug 12, see below.** A 1824–2024 table
   with an undeclared hole is worse than one carrying a `coverage` flag; follow
   the `split_coverage` precedent from `house-competition` and mask rather than
   imply.

2. **It re-opens the guestbook, deliberately.** ICPSR data is free "to data
   users at ICPSR member institutions" — CMU qualifies, but **no build script
   can fetch it.** `course-map.md` presently ends on *"No guestbooks left, down
   from three"* and draws the lesson that the gated convenient copy usually has
   an ungated primary source behind it. This is the exception, and it is
   defensible for a reason that does not generalize: **1788–1990 returns are
   frozen history and will never revise**, so a single authenticated download,
   committed once with a provenance entry, is a different object from a
   recurring per-lab fetch. Record it in the README as a knowing exception, not
   an oversight.

3. **It is a parsing project.** Fixed-format within states, more than one file
   per state, distributed with SAS/SPSS setup files rather than CSVs. Effort is
   comparable to `parse-clerk.py`. ICPSR further warns that **its own county
   identification codes contain errors** and advises verifying against county
   names — which, across 166 years of counties created, merged and renamed, and
   Virginia's independent cities, makes the `vintage` column of rule 4 load-
   bearing rather than decorative. Budget a Newberry Atlas crosswalk.

> ### ⚠ REVISED TWICE on Aug 12 — read §4.5.2 for where this landed
>
> **The canvass build is cut**, and 1992/1996 are not built from state
> canvasses. But `pres_county` does **not** stay at 2000–2024 — a second
> revision reinstates a long series from 1952, on a corrected reading of what
> ICPSR actually distributes. See **§4.5.2**, which supersedes this box and the
> §4.5 costs below.
>
> The retrievability scan (`canvass-scan-1992-1996.md`) found that **5 of 51
> jurisdictions** serve county presidential returns at a verifiable address,
> **7 hold them only on paper**, and the effort is 51 navigation problems
> before it is 102 documents. But the decisive objection is not effort — it is
> that **the long series would not measure one thing even if it were
> assembled**: counties change identity, how people vote changes underneath the
> unit, no source is definitive, and it is not clear what the series would be a
> series *of*.
>
> **The scan becomes a chapter instead of a dataset** — see §4.5.1. The
> material below is kept as the record of what was decided and why it was
> undone; it is the evidence the chapter is built from.

#### 1992 and 1996 from state canvasses — proposed Aug 12, cut the same day

The gap ICPSR leaves is closed the same way `parse-clerk.py` closed the House
series: **go upstream to the document the compilations were compiled from.**
Each state's chief election officer certifies a canvass. That canvass is the
primary source, it is ungated, and it is complete by law.

**Scope.** 51 jurisdictions × 2 elections = **102 documents**, for county-level
presidential returns by candidate.

**Why this is the right call and not merely the free one.** 1992 is the worst
imaginable year to take third-party votes on faith: **Perot took 18.9%
nationally**, the largest non-major-party share since 1912, and 8.4% again in
1996. The exact failure mode found in the 2024 file — third-party votes
silently absent from `total_votes` for Alaska and New York — would be
catastrophic rather than cosmetic here. **A canvass cannot have that defect**;
it is the certification of every vote cast. A compilation can, and demonstrably
does.

**What it will cost.** This is the largest single build task in the plan —
comparable to `parse-clerk.py`, but against roughly 51 document formats instead
of one. Expect PDFs, scanned images needing OCR, a few HTML tables, and states
where the 1992 canvass exists only in print. **Alaska will again not be
county-level**, consistent with `ladder.csv`; it gets the same house-district
treatment as 2020 and 2024, recorded rather than papered over.

**The gate, and the fallback.** Before any parsing, run a **retrievability
scan**: for each of the 102, record whether a machine-readable, image-only, or
no online canvass exists. Then:

- retrievable → parse from the canvass;
- **not retrievable → the state-year carries a coverage flag and stays empty.**
  It is never quietly filled from a compilation.

The scan is worth publishing on its own. *Which states can you still obtain
1992 returns from, and in what form* is a finding about American election
administration and record retention, not merely a project status table — and it
is the kind of finding this book is built to make.

### 4.5.2 Where this landed: a county series from 1952 — decided Aug 12

**Build the file for the period the records are good, and make the difficulty
of the *full* history the chapter's subject rather than the project's.**

#### Correction: ICPSR 13 is not a parsing project

§4.5 cost #3 called the ICPSR adoption "a parsing project — fixed-format within
states, more than one file per state, SAS/SPSS setup files." **That is true of
ICPSR 1 (1824–1968) and false of ICPSR 13 (1950–1990)**, which is the one this
series actually needs. ICPSR 13 is:

- **national files, not per-state** — county-level returns for president,
  senator, representative and governor;
- distributed as a **tab-delimited data file, an R data file, and a PDF
  codebook** (v2, DOI `10.3886/ICPSR00013.v2`).

A tab-delimited file with an R data file beside it is a **download and read**,
not a parse. The objection that carried the most weight against the historical
extension does not apply to the part of it we want.

#### Counties are stable over this window, and that is checkable

The premise holds and is quantifiable. The pace of county change **slowed
sharply after 1920 and major boundary changes have been extremely rare since
1970** — essentially two new counties in that span (La Paz AZ 1983, Broomfield
CO 2001), plus a short list of renames and Virginia independent-city
reversions.

**And the exceptions land where the schema already flags trouble:** Alaska
(borough and census-area reorganisations, repeatedly) and Virginia (independent
cities) are the two jurisdictions §4.5's `unit_kind` column already exists for.
The counties that move are the ones that were never plain counties.

**So 1952 is not an arbitrary cut. It is where the unit stops moving** — which
is a far better answer to "why does the series start here" than a round number.

#### The series, and its one hole

| Years | Source | Status |
|---|---|---|
| 1952–1990 | **ICPSR 13** — tab-delimited, national files | one authenticated download |
| **1992, 1996** | **— nothing identified —** | **the open problem** |
| 2000–2004 | MEDSL, Harvard Dataverse | ungated |
| 2008–2024 | tonmcg | ungated |

**The gap did not go away; it moved.** It was a truncation at the start of the
series and is now **a hole in the middle of an otherwise continuous 1952–2024
file**, which is worse. It is the single remaining decision — see open
question G.

**One argument that no longer applies.** §4.5 said never to "quietly fill from
a compilation." **ICPSR 13 is itself a compilation**, so adopting it settles
that question: the rule was never purity, it was *disclosure*. Rule 7's
per-row `source_id` is what enforces it, and with that column present a
licensed compilation for 1992/96 is the same class of object as ICPSR for
1952–1990.

> ### ✅ SETTLED Aug 12 — classroom use, cite the source
>
> **Decision: this material is used to teach a class, not republished.** Every
> chapter cites its source; if the book is ever published, licences get sorted
> then. §4.5.3 below stands as the record of what each source permits, but it
> **no longer gates any build decision.**
>
> **Both halves of that position were checked and already hold:**
>
> - **The public directory contains no data.** `…/jcervas.github.io/teaching/
>   2026-2027/class-cmu-2026-84-355/` holds `readme.html`, `readme.md`,
>   `readme.pdf` and `readme_files` — **zero CSV, TSV or ZIP files.** Only the
>   syllabus is published. Briefs render *in place* in Drive, beside their
>   `.Rmd`, and never reach the public repo. The §4.5.3 worry was about a
>   pipeline that does not exist.
> - **All 62 briefs already carry a Sources section.** Checked by grep; not one
>   is missing. The citation requirement is already met structurally, and
>   `dd_cite()` (§4.4) makes it generated rather than typed.
>
> **One consequence worth banking:** licensing no longer rules out **ICPSR 13**
> for the 1952–1990 county series. CMU is a full member and enrolled students
> are authorized users. If the long series is ever wanted, that door is open —
> though the *other* four objections (the unit moves, the vote method changes,
> no definitive source, unclear what is measured) were never about licensing
> and still stand.
>
> **Still worth doing, now as hygiene rather than exposure:** ask jaytimm and
> APM Reports to declare a licence, and skim the 16 unread terms. Both are
> cheap, and both matter the day publication is considered.

### 4.5.3 The licence wall — checked Aug 12; kept as reference, not a gate

I checked what CMU licenses. **The access question turned out to be the wrong
question.** CMU has the access. What it does not have — and cannot get by
membership — is the right to *republish*.

**The constraint I had missed is in the course's own YAML.** `readme.Rmd` line
30 writes output to `…/GitHub/jcervas.github.io/teaching/2026-2027/…`. **The
book is published to a public GitHub Pages site.** Committing a master table is
therefore redistribution to third parties, unambiguously and by design.

| Source | Years | Licence | Publishable in the book? |
|---|---|---|---|
| **MEDSL** county pres | 2000–2020 | **CC0 1.0** — public domain | ✅ unrestricted |
| **tonmcg** | 2008–2024 | **MIT** | ✅ yes |
| **ICPSR 13** | 1950–1990 | Terms of Use: **redistribution prohibited** absent written agreement | ❌ **no** |
| **Leip Atlas** | 1912– | purchase; republication "requires contract"; institutional licences bar sharing outside the institution | ❌ no |
| **CQ V&E** | 1789–2016 | CMU had a **three-month trial only**, and of *Local Stats / State Stats / U.S. Political Stats* — **not** the Voting and Elections Collection | ❌ not licensed |
| **State canvasses** | any | state government public records | ✅ yes |

**CMU is a full ICPSR member** — free downloads, account created on campus,
no usage restriction mentioned in the LibGuide. That entitles *a CMU user* to
the data. ICPSR's Terms of Use separately prohibit redistribution without
written agreement, with sanctions running to referral to the institution's
Research Integrity Officer. **Membership is a right to receive, not a right to
republish.**

**So §4.5.2's recommendation does not survive.** ICPSR 13 cannot be the basis
of a committed master table in a publicly published book.

**And the irony is worth putting in the chapter:** the only fully
redistributable source for pre-2000 county returns is **the state canvasses** —
the option cut for expense. The wall in front of the long series is not only
boundary change and file format. **The good compilations are property.** For a
book whose subject is who made a dataset and why, that is not an obstacle to
work around; it is the finding.

#### Where this leaves `pres_county`

**Recommended: 2000–2024.** MEDSL (CC0) for 2000–2020, tonmcg (MIT) for
2024. Both public-domain-safe, both already ungated, and the whole table is
publishable without a single permission. The 1992/96 hole and the 1952 ambition
both dissolve into the chapter, which is where they now do more good.

Alternatives, if a longer series is wanted:

1. **Written permission from ICPSR** to publish a derived aggregate. They do
   grant agreements. Slow, uncertain, and worth exactly one email.
2. **Canvasses for a few pre-2000 years**, public domain, restricted to the ten
   tractable states — a subset built for the chapter rather than for coverage.
3. **ICPSR 13 as a student-side download.** Rejected: it breaks "every lab
   ships working," which is the course's governing design rule.

> #### ⚠ A live issue in the existing corpus, unrelated to this decision
>
> **`jaytimm/PresElectionResults` declares no licence.** GitHub's API returns
> no licence object, which by default means all rights reserved. It is the
> source of `pres_state` 1864–2024 in `historical-campaigns` — **already built,
> already committed, already published to the public site.**
>
> Two mitigating points, neither of which is legal advice: the underlying
> election returns are *facts*, and under *Feist* an unoriginal compilation of
> facts is not copyrightable in the US; and what binds in the ICPSR and Leip
> cases above is **contract**, not copyright — no contract was entered into
> here. But "probably fine" is a weaker position than the rest of this corpus
> occupies, and the fix is cheap.
>
> **Action:** either ask jaytimm to declare a licence, or rebuild `pres_state`
> from a source that has one. Worth doing regardless of the county decision.

### 4.5.1 The chapter — now about sufficiency, not impossibility

**`county-returns-history` — "How Far Back Is Far Enough"**
*A county file that starts in 1952, and why going further is possible,
expensive, and mostly pointless.*

The chapter's subject changed with the decision. It is no longer *"the long
series cannot be built."* It can — people have built it. The question is
**where a series should start, and how you would justify the answer.**

**The turn (around Step 5):** the obvious instinct is that further back is
strictly better, and that a start date is a budget compromise to apologise for.
It is not. **A series should start where its unit stops moving**, and for the
American county that is roughly 1970 — with 1952 reachable because the records
either side of that line are good. Extending to 1824 would not add thirty more
elections to the same series; it would silently splice a different one.

Four reasons, each demonstrable from material already on disk:

1. **The unit changes identity.** Counties are created, merged and renamed;
   Virginia mixes counties with independent cities; ICPSR warns that **its own
   county identification codes contain errors** and tells users to match on
   names instead. And the unit is not even "county" in **nine** jurisdictions —
   town in six New England states, locality in Virginia, parish in Louisiana,
   **State House District in Alaska**, which the 2024 file the course already
   ships mislabels with county FIPS codes.

2. **How people vote changes underneath the unit.** A 1992 county total is
   overwhelmingly election-day precinct voting; a 2024 county total is largely
   mail and early votes. The `ga-precinct-returns` data already carries
   **precinct × candidate × vote method**, so the course can show this rather
   than assert it — the same county, the same office, a different mixture of
   instruments.

3. **No source is definitive.** `returns-source/data/sources.csv` already
   records that a national compilation is *"obliged to: **Nothing** — it is a
   voluntary act."* The scan shows what the alternative costs, and
   `national.csv` already shows the compilations disagreeing with the certified
   national total by **10,874 Democratic** and **7,781 Republican** votes.

4. **It is unclear what would be measured.** Third-party handling alone
   defeats it: **Perot took 18.9% in 1992**, and the 2024 file the course ships
   omits third-party votes from `total_votes` for **Alaska and New York** while
   including them for all 51 states in 2020. A "county Democratic share" is not
   one quantity across thirty years.

5. **The good compilations are property.** Added Aug 12 after the licence
   check, and the strongest of the five for this book's purposes. The long
   series *has* been assembled — by ICPSR, by CQ, by Dave Leip — and each of
   those assemblies is **licensed rather than published**: ICPSR forbids
   redistribution absent written agreement, Leip requires a contract to
   republish, CQ is a subscription CMU does not hold. The public-domain source
   is the one nobody has aggregated, because aggregating it is the expensive
   part. **That is why the compilations exist, why they are not free, and why
   this chapter cannot simply hand the reader the file it is about** — a
   constraint the chapter should state plainly rather than route around.

**The scan's own numbers are the exhibit**: 5 of 51 states hand you the data ·
7 have it on paper only · Delaware's own published link returns 245 bytes ·
5 block you for looking like a robot · the oldest, Alaska's, is a photograph of
a document, 134 pages with zero extractable characters.

**And the closing move, which the earlier framing could not make:** the chapter
ends by *building the file anyway* — 1952 to 2024, from the sources that are
good — and stating the start date as a claim with a reason behind it rather
than an apology. **Most questions anyone asks of county returns are about the
period since the New Deal coalition broke up**; a 1952 series covers all of
them. The chapter's job is to make the student able to defend a start date,
which is a more useful skill than admiring a long one.

**Placement:** Part I, beside `returns-source`. It explains why the
compilations everyone uses exist at all — and it costs a fraction of what the
full-history build would have, because the research is already done and sits in
`canvass-scan-1992-1996.md`.

#### `pres_state` is **not** derived from `pres_county`

Tempting, and wrong. The evidence is already in the corpus, and more was found
testing the idea.

**Already built.** `returns-source/data/national.csv` records that county sums
miss the certified national total by **−10,874 Democratic** and **−7,781
Republican** votes. `ladder.csv` records county as published "every state
**except Alaska**."

**Found Aug 12, summing the 2024 county file to state level:**

| State | County-summed Dem % | State file | Gap |
|---|---:|---:|---:|
| Alaska | 43.10 | 41.41 | **+1.69** |
| New York | 56.34 | 55.12 | **+1.22** |
| Vermont | 64.36 | 63.83 | +0.53 |
| Pennsylvania | 48.66 | 48.66 | 0.00 |

Two causes, both chapter-worthy:

- **Alaska's "county" rows are State House Districts wearing county FIPS
  codes.** `02001` is labeled *"State House District 1"*. This is the DC
  `11001` pathology of the introduction, undiscovered, in a file the course
  already ships.
- **Third-party votes are absent from `total_votes` for Alaska and New York in
  2024, and for no state at all in 2020.** Same compiler, same repository,
  behavior changed between vintages, nothing warned.

**The decisive argument is pedagogical, not technical.** Define `pres_state` as
`sum(pres_county)` and the two rungs agree *by construction* — at which point
`national.csv`'s finding cannot be demonstrated, because the disagreement it
exists to show has been engineered away. The ladder motif of §3.5 requires that
rungs be independently sourced and therefore *able* to disagree.

**Resolution.** Keep both tables, independently sourced. Add to the accessor:

```r
dd_check("returns/pres_state")   # recompute from counties, report gap by state-year
```

This turns a silent modeling choice into a published diagnostic, and promotes
the Alaska and New York discoveries from landmines to exhibits.

### 4.6 What this does to the footprint

| | Now | After |
|---|---:|---:|
| Tracked in Drive | 1,300 MB | **~120 MB** |
| Raw spatial | 1,040 MB, seven copies | one ignored cache |
| CSVs | 72 MB, 322 files | ~45 MB, master + per-chapter figure tables |
| Scripts fetching county returns | 9 | 1 |
| Schemas for 2020 county race | 4 | 1 |

---

## 5. Anchors

**Georgia is the canonical worked jurisdiction.** It already is in practice —
593 MB of precinct returns, blocks, the crosswalk, `areal-units`,
`vote-dilution`, `levels-of-aggregation`, `bisg-check` — and it is the only
state in the corpus where the full ladder exists *and* an answer key exists
(cast vote records reporting how mail voters actually voted). Every rung of the
ladder motif can be demonstrated on one state.

**Pennsylvania is the local secondary**, for the chapters where "this is your
county" does real work.

**Migrations to consider, none of them free:**

| Chapter | Currently | Note |
|---|---|---|
| `vote-targeting` | NC gubernatorial, county | NC returns reproduce the textbook's Tables 5.1–5.3 exactly, *and found two errors in them.* **Recommend keeping NC** — the finding is the chapter. |
| `rpv`, `bisg-check` | Houston / Harris County TX | `bisg-check` is already Georgia (46,000 voters). `rpv` is Houston. Moving `rpv` to Georgia would let it share the precinct table and the CVR answer key — **the strongest argument for any migration here**, and worth costing. |
| `demographics` | Wayne County MI / Detroit | Eight scales, transects, rings. Heavy build. **Low priority** — the Detroit gradient is the illustration. |
| `cast-vote-records` | Alaska | RCV; no Georgia equivalent exists. Stays. |

The rule for the rest: **standardize the schema, not the state.** A chapter
changes jurisdiction only when doing so buys a shared table or an answer key.

---

## 6. Sequence

Each phase ends with every affected brief re-knitting. Nothing merges that does
not knit.

**Phase 0 — inventory and baseline. ✅ DONE Aug 12.** *No file changed.*
Outputs and scripts in `data/_phase0/`; full report in its `README.md`.
**383 data files · 2,672 columns · 279 schemas · 36 sources licence-audited ·
18,592 numbers frozen from 62 rendered chapters.**

Three results that change what follows:

- **Deduplication is not the prize.** Exact duplicates total **12,909 bytes**.
  The real finding is **279 distinct column schemas across 383 files** — a
  vocabulary problem, not a storage one. §1 of this plan led with the four
  copies of `pres2024_states.csv`; that is 2.7 KB and was the wrong emphasis.
- **Two sources cannot legally be republished** — `jaytimm/PresElectionResults`
  and `APM-Reports/jury-data`, both declaring no licence, together feeding four
  chapters. **16 more have terms nobody has read.**
- **The baseline exists**, so every later phase can be proved not to have
  changed what the book says.

**Phase 1 — `returns/`.** Highest reuse, nine fetchers to one. Build
`pres_state`, `pres_county`, `pres_cd`, `house_cd`. Write `dd.R` and the
dictionary against this one family first, since it is the hardest case.

**Phase 2 — `census/` and `geography/`.** Reconcile the four county-race
schemas into one. Collapse seven TIGER downloads to one cache. **The largest
single reduction in the project.**

**Phase 3 — remaining masters.** `congress/`, `opinion/`, `money/`, `admin/`.

**Phase 3b — `pres_county` 2000–2024.** Now small. **MEDSL** (CC0) for
2000–2020, **tonmcg** (MIT) for 2024, reconciled onto one schema with per-row
`source_id`, `unit_kind` and `third_party`. Both sources are ungated and
publishable; no permission is needed for any part of it.

Three larger builds were considered and dropped in one day — 102 state
canvasses, the ICPSR 1 fixed-format parse, and finally ICPSR 13 — the last on
licensing rather than effort. **Everything learned doing that goes into
`county-returns-history` (§4.5.1), which is now the strongest chapter to come
out of this redesign.**

**Phase 4 — migrate chapters, part by part.** Part I first (most chapters,
most duplication), then II–VI. Re-knit each part before starting the next.
Repoint the introduction's four cross-chapter reads to master tables.

**Phase 5 — write the two new source chapters** and the six part openers.

**Phase 6 — assemble the book.** Index `.Rmd`, part title pages, a
cross-reference pass in the established convention (data session by number and
name, never by week), and a regenerated lab-inventory artifact.

Phases 1–3 are mechanical and safe. Phase 4 is where the work is. Phase 5 is
writing. **Phases 0–2 alone deliver most of the consistency benefit** and are a
sensible stopping point if the semester intervenes.

---

## 7. Open questions

**A. Three chapters whose part is arguable.** `residual-votes` and `eavs` are
election *administration* — the machinery around the outcome rather than the
outcome. `cast-vote-records` is the ballot itself, the bottom rung. Options:
leave all three in Part I; move the two administration chapters to Part II
(records kept about the process); or give administration its own short part.
Part I is the heaviest either way.

**B. `rpv` to Georgia?** The only migration that clearly buys something — the
shared precinct table plus an answer key. Costs a rebuild of a working chapter.

**C. Census API key policy**, still undecided from the earlier map and now
scheduled: Session 2 teaches the API. Own keys, or keyless demo endpoints.
Independent of this redesign but blocked on the same date.

**D. The live Census API key** in `F25/Assignments/labs/surnames/surnames.md`
was distributed to students and should be revoked regardless of anything here.

**E. `_archive/`** holds 46 old lab/key pairs pointing at the current paths.
Migrate them, or freeze them and say so in a README. Recommend freezing.

**F. Retired chapters.** `gotv` and `campaign-visits` are marked retired in
`course-map.md` but both still knit and both are placed above (`campaign-visits`
in Part VI). Confirm whether they are in the book or out of it.

**G / I. Pre-2000 county returns. — CLOSED by §4.5.3, on licensing.** The
library check was done: **CMU is a full ICPSR member and CQ V&E was never
licensed** (a three-month trial of three other CQ products). But ICPSR forbids
redistribution and the book publishes to a public site, so ICPSR 13 cannot
ship. `pres_county` = **2000–2024**, MEDSL (CC0) + tonmcg (MIT). One email to
ICPSR asking about a derived-aggregate agreement is the only cheap upside left.

**J / K. Licences. — DOWNGRADED to hygiene, Aug 12.** The corpus is taught
from, not republished, and the public directory was verified to contain no data
at all. Phase 0 audited all 36 sources anyway (`data/_phase0/licences.md`), so
the answer exists whenever it is needed. Two follow-ups, neither urgent: ask
`jaytimm/PresElectionResults` and `APM-Reports/jury-data` to declare a licence,
and skim the 16 sources whose terms are unread — ANES and Mapping Inequality
(CC BY-NC-SA) first.

**H. Does the ICPSR exception get taught?** If §4.5 is adopted, the book will
contain one gated source in a corpus that otherwise makes a point of having
none. That is either an embarrassment to bury or the best available example of
*why* the distinction between a compilation and a primary source matters — and
`sources.csv` in `returns-source` already has the row it would slot into
("A university or newsroom compilation … obliged to: **Nothing — it is a
voluntary act**"). Recommend teaching it.

---

## 8. What is not proposed

- No change to the ten-step brief form, or to any chapter's argument or finding.
- No student-facing code. The no-coding rule is unaffected; `dd.R` runs in
  build scripts and in chapter setup chunks, both of which students never edit.
- No new data sources. Every master table is assembled from files already on
  disk or already fetched by an existing script.
- No renumbering of the 14 data sessions. The book's parts and the semester's
  sessions are different objects and can stay that way.
