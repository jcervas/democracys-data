# Official county-level presidential returns, 2020 and 2024

The states' own certified publications, assembled as a **counterweight** to the
GitHub compilation — not as a replacement for it.

> ### ⚠ The title of this document used to say "replacing", and that is wrong
>
> **Status, Aug 2026: the assembly is complete** — 51 jurisdictions × 2
> elections, 102 documents, 11 formats — **and the three dependent chapters
> already read it.** They were not repointed, and they should not be:
>
> | Chapter | Reads this folder | Its section |
> |---|---|---|
> | `data-sources` | yes | *What the states actually published* |
> | `mapping` | yes | *The file the states published* |
> | `wind-map` | yes | *The same election, as the states published it* |
>
> **In all three the compilation is the specimen, and swapping it out would
> delete the argument.** `data-sources` is a chapter *about* a stapled file —
> "So who stapled this one together", "What a stapled file can testify to". Its
> setup says so in as many words: *this chapter is ABOUT the compilation, so the
> compilation stays as the specimen.* `mapping`'s returns section is titled "the
> returns, and what is wrong with them". `wind-map` uses the compilation's
> Connecticut and DC breakages as its worked examples of "three ways a unit
> stops being the same unit".
>
> So the right relationship is the one that exists: **the compilation is what
> the chapter examines, and this folder is what it is examined against.** Anyone
> reading the "Why" below as an instruction to swap the inputs will destroy
> three working chapters. The `county-returns` chapter is where this assembly
> speaks in its own voice.

## Why

The corpus reads county-level presidential returns from a GitHub repository
(`tonmcg/US_County_Level_Election_Results_08-24`). It is careful work and it is
under no obligation to anybody: no correction path, no custodian, no guarantee
it will exist next year. Three chapters depend on it (`data-sources`, `mapping`,
`wind-map`), and the `returns-source` chapter makes the argument against exactly
this kind of dependency.

There is no official national county file to check it against. No federal agency
counts votes. County returns exist only as fifty-one separate state
publications, so that is what this folder assembles.

## Target schema

One CSV per jurisdiction in `derived/states/`, named `<postal>_<year>.csv`, with
exactly these columns and no others:

    state_name    character, full name, e.g. "Alabama"
    county_fips   character, 5 digits, LEADING ZEROS PRESERVED
    county_name   character, as the state publishes it
    votes_dem     integer, the Democratic nominee's votes
    votes_gop     integer, the Republican nominee's votes
    total_votes   integer, ALL votes cast for president, including
                  third parties and write-ins

`votes_dem + votes_gop <= total_votes` must hold for every row.

## Provenance, recorded per jurisdiction

Append one row to `provenance.csv` for every file produced:

    state, year, url, fetched_on, format, rows, certified,
    notes

`format` is what actually arrived: csv, xlsx, xls, pdf, html, json, txt.
`certified` is yes/no/unknown -- whether the page says these are certified or
official results rather than election-night or unofficial returns.
`notes` records anything a later reader needs: a PDF that had to be parsed, a
county whose name differs from the Census name, a race with a recount.

## The three jurisdictions where "county" does not apply

Do not force these into a county shape. Produce the file the state actually
publishes and record what it is.

- **Alaska** publishes by State House District (40 of them), not by borough or
  census area. There is no county-level certified return. Produce
  `AK_<year>.csv` at the level Alaska publishes, use the state's own unit
  identifiers, and set `county_fips` to the state's district identifier
  prefixed `02`. Say plainly in `notes` that these are not counties.
- **Connecticut** moved from eight counties to nine planning regions between
  2020 and 2024. Produce each year at the level that year was reported and
  record the change. Do not crosswalk.
- **District of Columbia** reported one citywide row in 2020 and eight wards in
  2024. Same rule: report what was published.

## Rules

1. **Official source only.** The Secretary of State, State Board of Elections,
   or equivalent chief election office. A state archive is fine. A newspaper,
   Wikipedia, MEDSL, or another GitHub compilation is NOT, and if the official
   source cannot be obtained, say so and leave the file unwritten rather than
   substituting.
2. **Certified over election-night** wherever both exist. Note which you got.
3. **Never redistribute a bulk file** larger than a few MB. Commit the parsed
   CSV and record the URL.
4. **FIPS are character.** `01001` is not 1001. Verify every file has
   5-character codes before writing it.
5. **Do not invent a county.** If a state publishes returns for a unit with no
   Census FIPS -- an independent city, a township, a planning region -- record
   the state's own name and leave `county_fips` empty rather than guessing.
6. **Say what you could not do.** An honest gap is worth more than a filled
   cell nobody can trace.
