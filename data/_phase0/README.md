# Phase 0 — complete

Run Aug 12 2026. **No file under `labs/` was read for anything but measurement,
and none was modified.** Three scripts, re-runnable at any time:

```bash
python3 inventory.py     # what every data file is
python3 licences.py      # who owns every source
python3 baseline.py      # what the book currently says
```

| Output | What it holds |
|---|---|
| `inventory.tsv` | 383 data files: size, hash, rows, cols, schema |
| `columns.tsv` | **2,672 columns** — the draft dictionary, with inferred level |
| `duplicates.md` | exact-hash duplicates; schemas shared across labs |
| `licences.tsv` / `.md` | 36 sources, each with a publish/no-publish verdict |
| `baseline.tsv` + `numbers/` | **18,592 numbers** frozen from 62 rendered chapters |

---

## 1. Inventory

**383 data files · 2,672 columns · 279 distinct column schemas.**

## 2. Duplication — and a correction to how this was framed

**The byte saving from deduplication is negligible: 12,909 bytes across 5
exact-duplicate groups.** The plan led with `pres2024_states.csv` appearing in
four chapters, which is true and which is 2.7 KB. That was the wrong thing to
emphasise.

**The real finding is 279 distinct schemas across 383 files.** Nearly every
file in the corpus has a column vocabulary of its own. That is what makes the
book's relationships invisible, and it is not a storage problem — it is the
reason a reader cannot tell that two chapters are holding the same election.

**20 schemas are shared across two or more labs** — those are the free
consolidations, where files already agree on structure and only need a common
home.

Two exact duplicates worth noting because they are *not* about returns:
`primary-defeats` and `retirements` ship identical `checks.csv` and
`compare.csv`. Those chapters are more entangled than the map suggests.

## 3. Licences — 36 sources audited, none previously checked

> **Settled Aug 12: the corpus is taught from, not republished.** Chapters cite
> their sources; licences get sorted if publication is ever considered. The
> audit below is therefore **reference, not a blocker.**
>
> Two things were checked rather than assumed, and both hold:
> **the public directory contains no data** — only `readme.*` and
> `readme_files`, zero CSV/TSV/ZIP — because briefs render in place in Drive
> and never reach the public repo; and **all 62 briefs already carry a Sources
> section.**
>
> The verdict column below therefore reads as *"what this would permit if
> published"*, not *"what is wrong today."*

| Verdict | Sources |
|---|---:|
| **NO — cannot be republished** | **2** |
| **CHECK — terms unread** | 16 |
| LIKELY fine | 2 |
| YES — public domain or permissive | 16 |

### The two that cannot be republished

| Source | Licence | Feeds |
|---|---|---|
| `jaytimm/PresElectionResults` | **none declared** | `electoral-map`, `historical-campaigns`, `redistricting` |
| `APM-Reports/jury-data` | **none declared** | `jury-selection` |

No licence file means all rights reserved by default. **Correction to an
earlier draft of this file: these do *not* feed a public site** — the published
directory holds only the syllabus, and no chapter data leaves Drive. For
classroom use the position is comfortable; the underlying returns and court
records are *facts*, and under *Feist* an unoriginal compilation of facts is not
copyrightable in the US, while what binds ICPSR and Leip is **contract**, which
was never entered into here. The fix is still an email, and it is worth sending
before any thought of publishing.

**16 sources have terms nobody has read**, including ANES (5 chapters),
Richmond's Mapping Inequality (**CC BY-NC-SA** — the non-commercial and
share-alike clauses are worth actually reading), two Google Sheets of unknown
ownership, an AP embed, and the Polymarket API.

**`tonmcg` is MIT** and clean — worth recording, since `county-returns` is
currently replacing it for reasons of custodianship rather than licence.

## 4. Baseline

**18,592 numbers frozen from 62 rendered chapters**, stored per chapter in
`numbers/`. Every chapter has both HTML and PDF; none is missing output.

This is the safety net. After a chapter is repointed at a master table,
re-extract and diff. **A migration that changes no number is safe. A migration
that changes one has to name it** — and sometimes the honest answer will be
that the old number was wrong, which is a finding rather than a regression.

Numbers are read from rendered HTML rather than by re-knitting, because the
rendered file is what students read and because re-knitting 62 chapters is slow
enough that nobody would do it twice.

---

## 5. The corpus moved while Phase 0 ran

Worth recording, because the plan is now slightly stale:

- **59 → 62 chapters.** `finance-source` and `rollcalls-source` were written
  today — these are the two new SOURCE chapters the plan proposed as
  `disclosure-source` and `scores-source`. **That item is largely done.**
  §3.3's placement table needs a refresh against the current 62.
- **`county-returns/` is under construction** and its `SPEC.md` independently
  reaches the same conclusions this planning did: official state sources only,
  no GitHub compilation and no MEDSL, FIPS as character, and **do not force
  Alaska, DC or Connecticut into a county shape**. 26 states are done, with
  per-jurisdiction provenance including the certified/unofficial distinction.
- It also found one the plan had not: **Connecticut moved from eight counties
  to nine planning regions between 2020 and 2024.** A third unit discontinuity
  beside Alaska's house districts and DC's wards.
- Its route — the states' own certified publications — is the **public-domain**
  one, which is the only route §4.5.3 found that can actually be republished.

## 6. What Phase 1 should do first

1. **Send two emails** (jaytimm, APM Reports) asking for a licence. Cheapest
   item in the project; unblocks four chapters.
2. **Read the 16 unread terms**, starting with ANES and Mapping Inequality.
3. **Build `geography/` before `returns/`.** 279 schemas is a vocabulary
   problem, and the shared vocabulary is geography: `geoid`, `state_fips`,
   `county_fips`, `vintage`. Everything else joins to it.
4. **Adopt `county-returns/SPEC.md` as the template** for master-table specs.
   It is already doing what §4.3's rules describe, in a form that has survived
   contact with 26 states.
