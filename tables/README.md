# tables/ — the data that leaves the corpus

This folder is the third tier of the corpus's data layout, and the only one
that is ever distributed. Everything in it is a cleaned, analyzable
spreadsheet a student can open and work with on its own: real column names,
one table per dataset, no fragments.

The three tiers, and who each one is for:

| Tier | Where | For | Leaves the corpus? |
|---|---|---|---|
| raw | `labs/<part>/<chapter>/data/raw/` | replication — the file exactly as the publisher released it | **never** |
| derived | `labs/<part>/<chapter>/data/derived/` | the chapter itself — figure-ready fragments its brief reads | **in place**, linked from its own brief |
| **tables** | `tables/` | **students** — clean data to analyze on their own | **yes — Canvas or the course's private GitHub** |

"Distributed" means sent to enrolled students, on Canvas or in a private
repository. Nothing here is published to the open internet. That distinction
matters because it is what the licence gate below is calibrated to.

## The middle row changed on 29 Aug 2026

Every brief now closes its `## Sources` with a **The data itself** block
linking the derived tables its figures rest on, so a student can open one and
take it further. That is a real change to the row above, and it is worth being
exact about what it does and does not mean.

Derived tables are handed over **in place** — linked from the brief that
explains what the rows are, travelling with the chapter, never lifted out into
a folder of loose CSVs. The licence gate below still applies: a chapter whose
source is not ours to redistribute keeps that file in `raw/` and links nothing.

So the distinction between this folder and `derived/` is no longer "which one a
student may have". It is **whether the file can stand on its own.** A derived
table means what it means because a brief is wrapped around it. A table in here
has to make sense to someone who has never read that brief, which is what the
admission rules below are about, and it is a much higher bar.

## What may go in

1. **Clean and self-contained.** A student who has never read the brief can
   open the file and know what the rows and columns are. Follow the plan's
   vocabulary rules (`data-redesign-plan.md` §4.3): `year`, `geoid`,
   `state_fips`, `party`, `votes`, …; identifiers are text, not numbers.
2. **Licence-cleared for course distribution.** The Phase 0 audit
   (`data/_phase0/licences.md`) is the gate. CC0, MIT, public-domain
   government data: yes. Sources whose terms allow use by enrolled students
   at a member institution: yes, with the source cited. Files that are not
   ours to hand out — the ANES cumulative file, Jacobson's roll-call file —
   **never**, regardless of how useful they would be.
3. **Provenance stated.** Every table gets a row in the dictionary when the
   master-table build (`data-redesign-plan.md` §4) lands; until then, a
   one-line note in this README naming the table's source is the minimum.

## What stays out

- Anything from `raw/` — replication material, and in a few cases files that
  are not ours to redistribute at all.
- Anything from a chapter's `derived/` — those are figure fragments, shaped
  for one paragraph of one brief, and meaningless *here*, stripped of it.
  Students reach them through the brief instead (see above). A derived table
  earns a place in this folder only by being rebuilt to stand alone.
- Anything whose licence row says no or has not been read.

## Layout

Flat for now. When the master tables are built, this folder adopts the
plan's families (`returns/`, `census/`, `geography/`, …) and the tables here
become the distributable face of that layer.

## Tables

*(none yet — the folder was created 2026-08-16 ahead of the master-table
build; add a line here for every table added)*
