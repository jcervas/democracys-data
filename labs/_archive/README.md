# Frozen. Do not repoint, do not re-knit.

These are the retired student-lab and instructor-key pairs from the version of
the course that ran on **week numbers**, before the corpus became a book with
provenance parts. They are kept because they record how a session was actually
taught, which the briefs do not.

**They are deliberately excluded from every maintenance pass**, including:

- the cross-reference pass, which rewrote ~47 references in the live chapters to
  name a chapter rather than a session number;
- the `surveys` / `surveys-source` rename;
- `_lib/make-index.py`, which skips this directory, so nothing here appears in
  `INDEX.md`.

So the references inside these files point at week and session numbers that no
longer mean anything, and at two folder names that no longer exist
(`surveys/`, and `surveys-source/` in its old sense). **That is expected.**
Reading one of these files, treat every "Week N", "Session N" and "Lab N" as a
historical note about the F25 calendar, not as a pointer into the current
corpus.

## The one thing to know if you revive a file from here

Two folder names changed meaning rather than merely moving, so a naive path fix
will silently point at the wrong chapter:

| Old | Now | What it is |
|---|---|---|
| `surveys/` | `surveys-source/` | the Part V opener — what a survey can establish that no record can |
| `surveys-source/` | `validated-turnout/` | a case chapter — counted ballots against what people said they did |
| `poll-simulation/surveys-brief.Rmd` | `poll-simulation/poll-simulation-brief.Rmd` | a third file that was also called `surveys-brief` |
| `ga-precinct-returns/precincts-brief.Rmd` | `ga-precinct-returns/ga-precinct-returns-brief.Rmd` | stem now matches its folder |

The live corpus is `F26/labs/`; its structure is `labs/INDEX.md` and its
schedule is `F26/course-map.md`.
