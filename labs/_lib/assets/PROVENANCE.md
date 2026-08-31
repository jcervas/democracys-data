# Shared figure assets

Images of real objects that briefs embed with `knitr::include_graphics()`. Each
is a tight crop of a public government document, kept here so that more than one
brief can use the same file and so that no brief needs a network at knit time.

Reference them relative to the brief, e.g. from `cast-vote-records/`:

```r
knitr::include_graphics("../_lib/assets/alaska-rcv-sample-ballot.png")
```

Set `out.width` in the chunk header so the same file is sized sensibly in both
HTML and PDF, and put the caption in ordinary markdown under the chunk rather
than in a `cat()`-ed HTML block, so that it survives into the PDF.

| file | what it is | source | retrieved |
|---|---|---|---|
| `alaska-rcv-sample-ballot.png` | Ranked-choice instructions and the U.S. Representative contest grid (candidates x 1st-5th choice), cropped from page 1 of Alaska's official demonstration ballot | State of Alaska, Division of Elections, *Sample Ranked Choice Voting Ballot*, <https://www.elections.alaska.gov/doc/GenRCVsampleBallot11.9.21.pdf> | 2026-08-10 |
| `eavs-2024-items-c1-c9.png` | Survey items C1a/C1b (mail ballots transmitted and returned) and C9a-C9e (mail ballots rejected, by reason), cropped from pages 30 and 34 of the 2024 EAVS instrument | U.S. Election Assistance Commission, *2024 Election Administration and Voting Survey* questionnaire, <https://www.eac.gov/sites/default/files/2024-04/2024_EAVS_FINAL_508c.pdf> | 2026-08-10 |

Used by: `cast-vote-records/cast-vote-records-brief.Rmd` (ballot; also suitable
for `levels-of-aggregation`, whose brief is likewise ballot-less), and
`eavs/eavs-brief.Rmd` (survey items).
