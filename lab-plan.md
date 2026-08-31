# Lab operations — Fall 2026

**This file used to be a build tracker. The build is finished, so it is now the
maintenance document**: what still needs a human, what breaks if you rebuild,
how to add a chapter, and the two dated actions the semester requires.

Three files, three jobs, and they should not repeat each other:

| File | Answers |
|---|---|
| `labs/INDEX.md` | *What is in the book?* — generated, never edited by hand |
| `course-map.md` | *What gets taught when, and what is the finding?* |
| **this file** | *What could break, and what do I have to do about it?* |

**Classes begin Tue Aug 25, 2026.**

> **Design rule (set after reviewing the F25 evaluations), still governing.**
> 84-355 is a gen-ed with no coding prerequisite. **Students never write code.**
> Every chapter arrives working; exercises are runnable chunks with one
> parameter to change; the graded deliverable is written interpretation. A
> chapter that asks students to author code is out of spec.
>
> This is not a lowering of demand. F25 students reported **4.4 hrs/week against
> a 6.75 CST norm**, and the one substantive comment asked for *more* structure.
> The bar goes up on writing and interpretation as the coding bar comes down.

---

## Where things stand

Verified 14 Aug 2026 by counting the files and running the checkers, after the
re-architecture.

| | |
|---|---:|
| Chapter folders | **97** |
| — of which scheduled case and source chapters | 89 |
| — part openers (`part-1-…` … `part-6-…`) | 6 |
| — front matter (`introduction`) | 1 |
| — deliberately unassigned (`gotv`) | 1 |
| Chapters carrying a `*-brief.Rmd` | **97 — all of them** |
| Briefs knitting to HTML | 97 |
| Briefs knitting to PDF | 97 |
| Chapters with their own build script | 96 — all but `introduction` |
| — of which carry the standard `data/build-data.R` | 85 |
| Build scripts in total (7 chapters have several) | 106 |
| `sh _lib/check-all.sh` | **clean, exit 0** |

Per part, counting the part opener: **I 22 · II 12 · III 33 · IV 15 · V 5 ·
VI 8**, plus front matter and `gotv`. `labs/INDEX.md` is the authority and
`course-map.md` schedules what it lists.

## The book was re-architected on 14 Aug

It is now organised by **where the data came from**, and the six parts are:

| | Part | Ch. | Directory |
|---|---|---:|---|
| I | The Census Bureau | 21 | `01-census-bureau` |
| II | Surveys | 11 | `02-surveys` |
| III | Elections | 32 | `03-elections` |
| IV | Records of Political Actors | 14 | `04-political-actors` |
| V | Records of Ordinary People | 4 | `05-ordinary-people` |
| VI | Putting Data Together | 7 | `06-putting-data-together` |

Every chapter moved directory. Cross-chapter paths were **recomputed against the
new placement** rather than string-replaced, because chapters changed part as
well as part name and a substitution would have produced plausible wrong paths.

**Two parts carry two instrument families**, and their second run opens with its
own source chapter partway down: `voter-files-source` in III, `finance-source`
in IV. Those stay out of the beat map deliberately.

**Part V is four chapters and does not yet earn its name.** The commercial half
— phone-ping location data — is not built. Its opener says so.

**Part VI has no source chapter**, correctly: it introduces no data.

### What this replaced, and why it kept moving

The parts were reorganised three times in one day, which is worth recording so
the next person does not assume the current shape is arbitrary. First by
declining collector authority; then by *how you came to be in the data*, which
put surveys third; then by source, which is the current one. The first two both
described the **collector**. The current one describes **where a reader goes to
understand a number**, which is what the book is for.

### Two things every reorganisation of this corpus needs

- **`labs/_lib/check-tables-reviewed.tsv` keys accepted findings on file
  paths.** Move a chapter and its findings re-report as new. Repoint the ledger.
- **Rebuild every opener and re-knit**, then check for briefs whose `.Rmd` or
  `data/derived/` is newer than their renders. A moved chapter's HTML keeps the
  old links until it is knitted again.

**Nothing is left to build.** The two items this file used to carry as
outstanding — House results through 2024, and precinct returns with geography —
both closed; the resolutions and the bugs they turned up are written up in
`course-map.md`, which is where they belong. The August pass through the six
parts closed the last gaps of a different kind — missing *introductions* rather
than missing data — and found no others.

**The Duchin PUBPOL 2130 port list is closed too.** Every notebook we wanted has
a chapter that supersedes it: Week 1–2 → `census-decennial` / `census-race`,
Week 3 → `census-geography`, Week 4 → `areal-units`, Week 6 → `precinct-geography`,
Week 14 → `redistricting`. None of them is a translation any more; they were
rebuilt around a source rather than around her notebook, and `maup` — the one
genuine package gap — never had to be replaced because the chapters that would
have called it do the assignment explicitly and show the arithmetic.

---

## The two dated actions

### 1. Before Thu Aug 27 — decide the Census API key policy

**Session 1 teaches the API.** Part I still opens the semester, so this is due
before Aug 27.

Two workable answers: students each register their own key (free, instant), or
the chapters use the keyless routes only. **The corpus already works keyless** —
`census-access` exists precisely to walk the four doors, and it establishes that
a keyless API data request now returns **HTTP 302 with an empty body**,
redirecting to a page titled *Missing Key*. Follow the redirect and you parse
HTML as JSON; don't, and you record a success-shaped status and zero rows. That
is a finding, not an obstacle, so "keyless" is the lower-friction choice.

> **Security, still open and still worth doing.** A live **Census API key in
> plain text** sits in the F25 handout at
> `F25/Assignments/labs/surnames/surnames.md`, which was distributed to
> students. Revoke it at <https://api.census.gov/data/key_signup.html>. No book
> chapter needs a key.

### 2. Morning of Thu Nov 5 — put the results in `election-night`

This is the only chapter whose date is not ours to choose, and the only one that
needs an action during the semester.

1. Fill **`labs/02-record-of-an-outcome/election-night/data/derived/results_senate_2026_TEMPLATE.csv`**
   — 33 rows, columns `state`, `winner_party`, `dem_pct`. It will most likely be
   filled by hand from a news site, which takes ten minutes and is entirely
   defensible. **The brief already says so out loud**, on purpose: a called race
   is a judgment, not a count, and states differ enormously in how much is still
   outstanding on Thursday morning.
2. Re-knit the brief through the shared renderer (never a bare
   `rmarkdown::render`, which silently breaks embedded images). It runs
   `check-all.sh` first, so a malformed results file stops the render rather than
   shipping.
3. **Check `senator_last` against who is actually on the ballot.** The roster was
   built in **August 2026** and does not know about retirements, primary defeats
   or appointments since. The brief flags this as a real defect and tells students
   that spotting a name which is not on the ballot is a genuine finding — so this
   is a check, not necessarily a fix.

The rehearsal is already done and committed: the rule is graded one level down on
**433 House districts in 2024**, because a rule cannot be graded on the election
it was written to predict. That is what runs if Nov 5 goes sideways.

---

## What cannot rebuild unattended

Every chapter renders from committed `derived/` tables, so **none of this is
visible until somebody tries to rebuild** — and that day is usually the day the
data has to change. Three chapters need a file placed in `raw/` by a human:

| Chapter | What blocks it | What to do |
|---|---|---|
| `bellwether` | Harvard Dataverse sits behind an **AWS WAF**. A scripted request returns **HTTP 202 with an empty body** and `x-amzn-waf-action: challenge` — not 403, a success-shaped status and zero bytes. The challenge is JavaScript, so only a real browser can answer it. | Open the DOI in a browser, download the one file, put it in `raw/` under the name the script names. **The script refuses to run rather than pretend.** |
| `surnames` | The Census 2010 surname list now answers **HTTP 200 with a 247-byte HTML page reading "Request Rejected"**. A WAF block, not a move; a browser user-agent does not get through either. | Raw file is committed. `bisg-check` and `sweet-spot` read its output, so all three fail together if it is lost. |
| `ga-precinct-returns` | **Cloudflare blocks scripts on sos.ga.gov**, so the outer ZIP for the 2020 and 2024 archives is a browser download. | One download each; everything after that is automatic. See the chapter's `README.md`. |

**This is exactly the failure `_lib/provenance.R` exists to catch**: still 200,
still parses as *something*, no longer the data. The general fix is the one that
worked for the Jacobson House file — **commit the raw file** — and it has been
applied everywhere it can be.

One citation is stale rather than broken: the `lobbying` chapter cites
`lda.senate.gov/api/`, which now 301-redirects to `lda.gov`. Following the
redirect works, so the build is fine and only the brief's citation is out of date.

### A fourth chapter could not rebuild at all — now fixed

It was the one chapter with **no `data/` folder at all**: seven figure tables
sat loose at the chapter root beside a `build-houston-figures.R` also at the
root. `check-layout.py` did not catch it, and **that hole is now closed too** —
see below.

**The layout was the visible half; the broken build was the real one.** The
script read its shapefiles from `../ga-precinct-returns/data/` and unzipped them
itself if they were missing. The sibling has since moved its sources into
`raw/`, so both the read paths and the fallback pointed at files that are not
there. Committed CSVs meant the brief kept knitting and nothing said otherwise —
the same failure the part openers had, found the same way.

What it is now:

```
precinct-geography/
  precinct-geography-brief.Rmd
  data/build-data.R            was build-houston-figures.R at the chapter root
  data/derived/fig_*.csv       seven tables, were at the chapter root
```

- Reads `../../ga-precinct-returns/data/raw/{shp2020,shp2024,blocks}` and that
  chapter's `derived/`, by full path.
- **It no longer unzips anything.** Extracting into a sibling's `raw/` is a
  write into another chapter's folder, which rule 4 forbids. If a directory is
  missing the script now **stops and names the sibling build to run** — the
  sibling's `build-block-crosswalk.R` is what creates them.
- Writes through `dd_write_csv()` instead of `write.csv()`.

**All seven tables rebuild byte-identical to the committed ones**, before and
after the precision change, so the chapter's numbers did not move; the brief
re-knits to HTML and PDF and `check-all.sh` is clean. This chapter draws no data
of its own — it does the spatial work for one county on the sibling's files —
which is why it looked like it did not need the folder.

### The checker had the same shape of bug as the thing it failed to find

**`check-layout.py` was skipping any chapter with no `data/`.** All three of its
checks iterated over chapters that have one, so a chapter with none was not a
chapter with findings — it was a chapter the checker never opened, and the run
reported clean. This is the second time in a month a checker in this corpus has
been unable to fail: `check-vacuous.R` was examining zero chapters because an
absolute path containing `/_teaching/` matched its own exclusion pattern.

Both had the same tell — **a "clean" that never says how much it looked at.**
That is the thing to distrust.

What changed:

- The traversal starts from **what makes a chapter a chapter**, a
  `<slug>-brief.Rmd`, which is the same definition the part openers use, so the
  two cannot disagree about what exists.
- **A chapter with no `data/` is now a finding**, and it names what the chapter
  is holding instead. Exemption requires being listed in `NO_DATA_OK` — one
  entry, `introduction`, which computes nothing. A name in that set that is no
  longer a chapter is itself reported, so the allowlist cannot rot.
- **Loose files at the chapter root are a finding.** Only `data/` and `img/`
  belong beside the chapter, and only the brief and its two renders as files.
- **The clean line states its coverage**: `95 chapters, layout clean (94 with
  data/, 1 exempt: introduction)`. A number smaller than the corpus is now
  visible instead of reassuring.

**It found four strays on the first run**, all of them at chapter roots the
checker had never looked at: a stale 1 MB `apportionment.html` from before the
brief rename, two `*-brief.Rmd.bak` editor backups, and an `Rplots.pdf` that a
base-R device dropped during a build. **There is no version control on this
corpus**, so all four were moved to the session scratchpad rather than deleted;
retrieve them from there if any turns out to be wanted.

---

## Adding a chapter

The corpus is generated from itself, so the order of operations matters.

1. **Write `<slug>/<slug>-brief.Rmd`** and knit it through the shared renderer at
   `Academic/_teaching/_syllabus-template/render-brief.R` — every `knit:` field
   points four levels up at it, and `book/_render-template/` is only a copy kept
   for the record. Never a bare `rmarkdown::render`: the shared renderer repairs
   the base64 data URIs pandoc writes in a form browsers refuse to decode, and
   without that pass every embedded figure is silently broken. One document per
   chapter — the separate instructor key is retired.
2. **Check it against the four house conventions**, none of which is optional and
   three of which are easy to miss: a closing **`## Sources`** heading; a
   **prediction prompt** before the chapter's main turn; a
   **`## What this chapter cannot tell you`** section (or its subject-specific
   idiom); and a **`data/build-data.R`** that regenerates every file the brief
   reads, asserts its expectations, and writes `checks.csv`.
3. **Split `data/` into `raw/` and `derived/`** per `labs/DATA-LAYOUT.md`. Scripts
   run with the working directory at `data/`; briefs knit from the chapter folder
   and read `data/derived/…`. **Every chapter follows this**, as of the
   `precinct-geography` fix below.
4. **Write derived tables through `_lib/precision.R`** — `dd_write_csv()` at the
   write boundary, `dd_num()` where a number becomes a string.
5. **Run `sh _lib/check-all.sh`.** Seconds, no network, exit 0 only if clean. You
   do not have to remember this one — **the renderer runs it before rendering
   anything, so a failed check stops the render** — but running it directly gives
   you the findings without waiting for a knit.
6. **Add the slug to `PARTS` in `_lib/make-index.py`, then run it:**

   ```
   python3 _lib/make-index.py      # writes INDEX.md and index.html
   ```

   **A chapter not in `PARTS` lands in the index as `unassigned`.** A genuinely
   new dataset may also want a line in `TAGS`. The "Data source" column is
   scraped from the **first line** of the build script's `# SOURCES` heading, so
   put a real citation there rather than a lead-in sentence.

   **The position inside `PARTS` is the teaching order**, and it is the only
   place that order is stated — the openers read it back out of `INDEX.md` and
   test it. Moving a slug up or down that list is therefore a real edit: it is
   how `finance-network` came to follow `campaign-visits`, and how
   `house-competition` and `distributions` came to sit with `clerk-source`.
   Rebuild every affected opener afterwards, not just the index.

7. **Give it a beat in its part opener's `data/build-data.R`.** The `BEAT` map
   is an explicit judgment about what the chapter is *for*, and anything
   unlisted defaults to `4 use`. The openers assert that beats form
   **contiguous blocks**, so a chapter placed inside an earlier beat's run will
   fail the build.

   | Beat | For |
   |---|---|
   | `1 source` | what this kind of data is, and what it structurally cannot say |
   | `2 instrument` / `3 instrument` | one chapter per instrument that produces it |
   | `2 access` / `3 access` | how you get hold of it, and what each route hands you |
   | `4 use` | using the data to answer a question |
   | `5 critique` | turning on the instrument after it has been used |

   The number is the beat's **position in that part**, which is why instrument
   and access each appear at two numbers: where a part's finding is that you
   cannot get the file, access runs first (Parts III and IV). Not every part
   runs every beat — V and VI have no access beat on purpose, and only III and
   VI run `5 critique`. Add a new label to the `WHAT` lookup in **all six**
   openers, not just the one you are editing; the `stopifnot` on `names(WHAT)`
   is what makes an unregistered beat an error rather than a blank cell.

   **`5 critique` is not `4 use` with a sharper tone.** A use chapter applies
   the data to a question about politics; a critique chapter takes a number the
   reader has just been asked to trust and grades it against something else.
   `bisg-check` against known race, `validated-turnout` against counted
   ballots, `rpv` against a published truth.

   **If it is a display or technique chapter that draws no data of its own**,
   add it to `COMPANION` in the same file. Companions sit beside the chapter
   they serve rather than at the end of the part, which would otherwise break
   contiguity; the flag exempts them from the ordering test and marks them in
   the opener's contents as `4 use · companion`. Chapters that merely reuse a
   sibling's file to answer their own question are substantive, not companions.
   Then rebuild the opener and re-knit it, or its tables will disagree with the
   part they open.

## Adding a whole instrument, not just a chapter

If the new chapter reads a file no chapter in its part has introduced, the
chapter is not the whole job — **the part needs a source chapter for it.** That
is what the three August additions were: `clerk-source`, `rosters-source` and
`admin-records-source` each closed a family that had chapters using it and
nothing saying what it was made out of.

The source chapter is a normal chapter in every other respect — same four house
conventions, same `build-data.R`, same checks — and it goes into
`SOURCE_CHAPTERS` in `_lib/make-index.py` as well as `PARTS`.

**Where it goes depends on the part's shape**, and both shapes are legitimate:

- **Block** (Parts I, II, V) — the part's instruments are *alternatives* that
  invite comparison (decennial vs ACS vs estimates; ANES vs GSS vs CES), so they
  are introduced together at the top and the beat map covers the whole part.
- **Runs** (Parts III and IV) — the part covers more than one instrument family,
  and introducing them all at the top would hand the reader the second one before
  the first has been used. Each run opens with its own source chapter, the beat
  map covers the first run only, and the later source chapter stays out of `BEAT`
  deliberately — labelling it would drop a `1 source` into the middle of the
  applied chapters and fail the contiguity assertion.
- **Neither** (Part VI) — it introduces no data at all, so it has no source
  chapter and runs a single beat.

Whichever it is, **argue it in the opener's build script at the line where it
happens.** Every deviation from Part I's template is currently commented at the
point of deviation, and that is the convention that keeps the openers honest.

**Refer to chapters by part and slug, never by session or week number** — "Part I,
`surnames`", not "Session 3." Session numbers moved twice during the August
restructure alone, which is the argument. Three numbering schemes were in
circulation simultaneously and they disagreed; that is cleaned up now and should
not be reintroduced.

---

## Deliberately out

| Chapter | Status |
|---|---|
| `gotv` | **Out, and visibly so.** A findings table from a literature rather than a dataset anyone goes and gets — the least data-source-ish chapter in the book. It is unassigned in `PARTS` on purpose, so `INDEX.md` shows it sitting outside the book rather than silently omitting it. It still knits and is on disk if wanted back. |
| `campaign-visits` | **Back in.** The part restructure reinstated it: a wire-service tracker is data nobody was compelled to file and somebody chose to publish, which is a record a political actor left behind — Part IV. Brief-only reading in the money session. |

---

## Two findings that came out of building, and are worth keeping

**When the convenient copy is gated, the primary source often is not.** The House
series was unblocked by going *upstream* — past the tidy academic compilation
behind a guestbook, to the Clerk of the House's own published document. The
`lobbying` and `disenfranchisement` chapters hit the same wall in different forms
(a 25-record rate limit, and a PDF with no machine-readable version), and the PDF
was solved the same way. **No guestbooks remain, down from three.**

**Checking one source against another is what finds the errors.** Chasing eleven
districts that disagreed with Jacobson turned up three real parsing bugs in
`parse-clerk.py`, two of Jacobson's own errors, and one case that is nobody's
error and is flagged rather than fixed. Rebuilding the book's own North Carolina
table found **two errors in Sides et al. Table 5.3**, which `build-data.R`
re-checks on every run. The full write-ups are in `course-map.md`.
