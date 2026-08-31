# How a chapter's data folder is arranged

Every chapter keeps its data in the same three places, and the difference
between them is the difference the course is about: **what arrived** versus
**what we did to it**.

```
labs/<NN-part>/<chapter>/
  <chapter>-brief.Rmd          the chapter
  <chapter>-brief.html/.pdf    rendered in place
  img/                         only if the brief include_graphics() a figure
  data/
    build-data.R               the script that turns raw/ into derived/
    PROVENANCE.tsv             written by _lib/provenance.R, if the lab uses it
    ai-prompt.txt              hand-written; the AI prompt box in ## Sources
    raw/                       the sources exactly as they arrived
    derived/                   everything the build script wrote
```

`ai-prompt.txt` is the text of the chapter's AI prompt box — the prompt a
reader pastes into an AI assistant to rebuild the chapter's data from its
original source, or, where the source cannot be fetched again, the honest
explanation of why not. It is hand-maintained, it lives at the top of
`data/` because it is a record *about* the data, and it is checked by
`_lib/check-ai-prompt.py`: every fetching chapter must have one, every
number in its verification block must still appear in the chapter's own
data, and a chapter whose source is in that checker's FROZEN table must
render the box with `tone = "frozen"`.

Chapters sit under a part directory (`01-counting-people`, …), which is why a
sibling is `../../<chapter>/data/` from inside a script and `../<chapter>/data/`
from inside a brief. **`data/` and `img/` are the only directories that belong
beside the chapter**, and the only files are the brief and its two renders. A
CSV or a build script at the chapter root is misfiled, not a variant.

## raw/

The file as the publisher released it. Not renamed, not re-encoded, not
opened in Excel and saved again. If the Secretary of State ships a 32 MB
JSON export with a mixed-case key called `localResults`, that is what sits
in `raw/`.

`raw/` exists only when the source is actually committed. Many chapters
fetch at run time into a temporary directory instead — a 226 MB TIGER
shapefile has no business in a course repository — and those chapters have
no `raw/` folder at all. That is fine and it is not a lapse: the build
script records where the file came from, and `_lib/provenance.R` notices
when the URL starts returning something different.

Some raw files can never be committed because they are not ours to
redistribute. The ANES cumulative file is the standing example: five
chapters read it, it is 163 MB, and ANES asks that people take it from
ANES. Those builds look for it in `raw/` and say so if it is missing.

## derived/

Everything a build script produced: the figure-ready tables, the intermediate
extractions, the small CSVs the brief reads. **The brief reads from here and
nowhere else** — with one deliberate exception below.

The test for `derived/` is simple: delete the whole folder, run the build
script, and get it back. Nothing in `derived/` should be hand-edited, and
nothing should be in `derived/` that the build cannot rebuild.

Since 29 Aug 2026 these files are also **what the reader is handed**: every
brief closes its `## Sources` with a *The data itself* block linking the
derived tables its figures rest on. Two consequences for anyone renaming one.
`check-layout.py` resolves those link targets, so a rename that misses the
brief is a "dead link" failure rather than a silent 404. And the names are now
read by students, so a derived table is worth naming for what it holds —
`flows.csv`, not `tmp2.csv`.

## The exception, which is the point of the course

A brief may read a file out of `raw/` when the raw file **is** the exhibit —
when the chapter's argument is about what the source actually looks like.
`anes-brief.Rmd` prints the first line of the ANES file plus five real rows
straight from `data/raw/anes-head.txt`, because a chapter about codes that
are not measurements has to show the codes. That is reading raw data on
purpose, in full view, and it is different from reading it by accident.

## The third tier: `book/tables/`

Added 2026-08-16. The two folders above are workshop words — they describe
the chapter's own supply chain. Neither is meant for a reader. The tier that
leaves the corpus lives at the corpus root, in `book/tables/`: cleaned,
self-contained spreadsheets a student can analyze without ever opening a
brief. That folder is the **only** part of the corpus that is distributed —
to enrolled students via Canvas or a private repository, never the open
internet — and its own README carries the rules, including the licence gate
(`data/_phase0/licences.md`) and the standing exclusions (ANES cumulative,
Jacobson roll calls).

Nothing about a chapter's `data/` changes because of this: `raw/` and
`derived/` stay exactly as described here, and a chapter never writes into
`tables/`. Tables get there through the master-table build described in
`data-redesign-plan.md` §4.

## The rules

1. **One build script per chapter**, in `data/`, named `build-data.R` (or
   `build-data.py`). A chapter that needs several passes may have several
   scripts; they all live in `data/` and they all write to `derived/`.
2. **Scripts run with the working directory set to `data/`.** Paths inside
   them are `raw/thing.zip` and `derived/thing.csv`, never absolute.
3. **Briefs knit with the working directory set to the chapter folder.**
   Paths inside them are `data/derived/facts.csv`.
4. **A chapter never writes into another chapter's folder.** Reading across
   is allowed and several chapters do it — always by the full path,
   `../../<chapter>/data/derived/<file>.csv`, so the dependency is visible in
   the source rather than implied.
5. **Nothing moves in or out of `data/` by hand.** If a file cannot be
   explained as either "this is what arrived" or "the script made this", that
   is a question about the chapter, not a filing problem.

## Which raw files can be replaced, and which cannot

The corpus is about 2.0 GB and roughly 1.4 GB of that is acquired source data.
Most of it a build script can fetch again from a URL the script states. Some of
it cannot be fetched again by anyone, and **there is no version control on this
corpus**, so those files exist in exactly one place.

**Irreplaceable — do not delete, and back up before any bulk operation:**

- `rollcalls-source/data/raw/` — Jacobson's file circulates between researchers
  and is at no public URL.
- `lobbying/data/raw/` — the LDA API pages were captured because the ordering
  that made them recoverable was luck, not a documented guarantee.
- `anes/data/raw/anes-head.txt` — an exhibit the brief prints. The full ANES
  cumulative file is *not* here and is not ours to redistribute; it is a gated
  download, which is why `party-id` cannot rebuild from a clean checkout.

**Replaceable, each from a URL its build script names** — the shapefile trees
(`tiger/`, `blocks/`, `shp2020/`, `shp2024/`), every `.zip`, the unzipped
P.L. 94-171 tree under `areal-units/data/raw/pl/` (342 MB, re-extractable from
the zip beside it), and the large single files in `ga-precinct-returns`,
`migration`, `sweet-spot`, `redlining`, `county-returns`, `retirements`,
`officeholder-age` and `house-competition`.

This distinction used to live in `F26/.gitignore`, which was removed along with
the repository. It is a fact about the data rather than about git, so it lives
here now.

## Checking

```bash
sh book/labs/_lib/check-all.sh
```

A few seconds, no network and no builds. Exit status is 0 only if the blocking
ones are clean.

**`check-layout.py`** — is every file where this document says, and does every
path in every brief and build script point at something that exists. Catches
the loose file, the brief reading outside `derived/`, and the cross-chapter
path that stopped resolving.

It also asks, **first**, whether each chapter has a `data/` at all and whether
anything is loose at the chapter root. That question exists because its absence
was a hole: the other checks iterate over chapters that have a `data/`, so a
chapter with none was not a chapter with findings — it was one the checker
never opened, and the run said clean. `precinct-geography` sat that way with
seven CSVs and a build script at its root. A chapter may be exempt only by
being named in `NO_DATA_OK` (currently just `introduction`), and the clean line
now prints how many chapters were covered and how many were exempt, so a number
smaller than the corpus is visible rather than reassuring.

**`check-vacuous.R`** — is any check in the corpus unable to fail. A count over
a condition the data cannot violate is worse than no check at all: it looks
like verification and passes forever. Three passes — a filter and a
`stopifnot()` in one script, a table narrowed on its way to disk against a
condition wherever that CSV is read, and assertions with no data in them. Run
`Rscript _lib/check-vacuous.R --self-test` to confirm the checker itself still
works; it plants known bugs, including one that would execute `system()` if the
constant-folding sandbox ever leaked.

**`check-tables.py`** — what is wrong *inside* the derived tables, all of which
survives every other check here: a fixed-width cut through a name, a ranked
table quietly truncated to a round number of rows, a share given to thirteen
decimal places. Findings already read and judged fine are recorded in
`_lib/check-tables-reviewed.tsv`, so only new ones are reported; delete a line
to hear about it again.

**`check-figures.py`** — what is wrong in the *rendered* HTML and nowhere else.
Two things. A figure emitted more than once, which happens when a value
interpolated into a `cat(paste0(...))` figure is not length one: `paste0()`
vectorises over it instead of failing, and the chart comes out once per element
with duplicate element ids. A one-row data frame has length equal to its number
of columns, which is how `oral-argument` shipped Figure 2 four times with a
caption that read correctly. And a JavaScript figure with no static fallback in
its render, which is a chapter that will arrive with holes wherever `<script>`
is stripped.

This one reads the last render rather than the working tree, so it is advisory
and does not gate. A brief you have edited and not yet re-rendered is reporting
on the old file.

What none of them can do is tell you the numbers are right. That still takes
rebuilding a chapter and reading what changed.

### They run before every render

`_syllabus-template/render-brief.R` looks for `_lib/check-all.sh` above the
brief and runs it once before rendering anything. If the checks fail, nothing
is rendered and the failure is printed — a check that runs after the artifact
is published is a check nobody reads.

Once per *invocation*, not once per brief, so hand the renderer all the briefs
at once rather than looping in the shell:

```bash
Rscript ../../_syllabus-template/render-brief.R book/labs/*/*-brief.Rmd
```

`DD_SKIP_CHECKS=1` renders anyway, for when the brief you are fixing is the
thing the checks are complaining about.
