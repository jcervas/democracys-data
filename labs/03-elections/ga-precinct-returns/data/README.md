# Getting the Georgia precinct file

**One manual download, once.** After that `build-data.R` runs unattended.

There are two routes. **Prefer the first.**

---

## Route A (preferred) — the Georgia Secretary of State

The SoS publishes **one ZIP per election covering all 159 counties**, going back
to 2012:

<https://sos.ga.gov/page/historical-elections-results>

For this lab you want **November 3, 2020 - General Election**. The direct link,
and nine other useful elections, are in `ga-sos-elections.tsv` next to this file.

This is the **primary source** — the certified state record, not a
reconstruction of it.

## Route B (fallback) — VEST

<https://doi.org/10.7910/DVN/NT66Z3> → `2020-ga-precinct-general.tab` (~15 MB).

VEST is a volunteer academic project that reads county returns, reconciles them,
and matches precincts to boundaries. Its column convention (`G20PREDBID`) is
itself teachable, and the lab makes students decode a column name before using
one. Use this if Route A's format turns out to be unusable.

---

## Why either way is manual

**Both are gated, in different ways.**

Dataverse asks for a **guestbook response** — a form naming who you are and what
you want it for. Every scripted attempt returns:

```
{"status":"ERROR","message":"You may not download this file without the
 required Guestbook response for guestbookID 458."}
```

The Secretary of State's files sit behind **Cloudflare bot protection**. The URL
is stable and public and a browser fetches it fine; a script gets `403`, even
with browser headers and a referer. Verified, not assumed.

**This is not a defect to work around. It is the lab's opening fact**, and the
county level makes it worse rather than better:

- **Houston County** publishes its Statement of Votes Cast at
  <https://www.houstoncountyga.gov/residents/election-results.cms> — **as PDFs.**
- There are **159 counties**, each with its own website, its own file naming and
  its own format. There is no county-level route that scales.
- The machine-readable precinct file that researchers actually use comes from a
  volunteer project, and it asks you to sign a form.

**The finest-grained public record of American voting is published by 159
separate county governments in PDF, aggregated by volunteers, and served from
behind a bot check.** That is the first thing the lab says.

## Steps

1. Download the ZIP (Route A) or the `.tab` (Route B) **in a browser**.
2. Save it **into this folder**.
3. From this folder, run:

   ```
   Rscript build-data.R
   ```

`build-data.R` prints these instructions and stops if no source file is here, so
nothing fails silently.

## What gets committed

The raw file is **not** committed — it is large and it is not ours to
redistribute. What is committed sits in `derived/`; anything downloaded by hand
goes in `raw/` (see `../../DATA-LAYOUT.md`), and `.gitignore` excludes the
source.

## Format — verified, and it replaces VEST

Opened and confirmed on the Nov 3 2020 file. The outer ZIP contains:

```
info.txt                     election id, name, date, county versions
summary/<County>_*.zip       countywide totals, one ZIP per county
detail/txt/<County>_*.zip    precinct detail, fixed-width text
detail/xls/<County>_*.zip    the same, as spreadsheets
detail/xml/<County>_*.zip    the same, as XML   <-- use this
```

**Use the XML.** One file per county, 159 of them, structured as:

```
ElectionResult
  VoterTurnout / Precincts / Precinct   name, totalVoters, ballotsCast
  Contest (27 of them)                  text="President of the United States"
    Choice                              text="Joseph R. Biden (Dem)" totalVotes=...
      VoteType                          "Election Day" | "Advanced Voting"
                                        | "Absentee by Mail" | "Provisional"
        Precinct                        name, votes
```

**This is strictly better than VEST for Georgia**, and not only because it is the
primary source. VEST gives precinct x candidate. This gives **precinct x
candidate x vote method** — and in 2020 the split between election-day,
early and mail voting *is* the story. VEST cannot answer that question at all.

Verified on Houston County: 16 precincts, 27 contests, Trump 41,534 /
Biden 32,232 countywide, and the four vote types sum to each candidate's total.

## Still open

- **The 2024 general is not in this archive.** The list ends at June 2024; the
  November 2024 general appears to live on the newer `results.sos.ga.gov`
  platform. So a Biden-2020 vs Harris-2024 comparison is not available here.
- **To compare the same candidates twice**, the two elections have to pair:

  | General | Runoff | Same candidates |
  |---|---|---|
  | Nov 3 2020 (Perdue vs Ossoff) | **Jan 5 2021 Federal Runoff** | yes |
  | **Nov 8 2022** (Warnock vs Walker) | Dec 6 2022 Runoff | yes |

  Nov 3 2020 and Dec 6 2022 do **not** pair. One more download completes either
  row; the 2020/2021 pair is the better lab, because the electorate changed
  between a presidential general and a January runoff and the result flipped.
