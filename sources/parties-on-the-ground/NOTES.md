# Parties on the Ground — access note and data assessment

Bawn, K., Brown, K., Ocampo, A. X., Patterson, S. Jr., Ray, J. L. and Zaller, J.
(2026). *Parties on the Ground: A Study of Nominations for the House of
Representatives*. University of Chicago Press. Chicago Studies in American
Politics. eBook ISBN 9780226853314. doi:10.7208/chicago/9780226853314

## What is actually public

The book is **not** open access. On De Gruyter Brill it carries a `Licensed`
badge, and the full text renders only because Carnegie Mellon University
Libraries subscribes. Of the twenty sections in the table of contents, the
platform labels exactly three `Publicly Available`:

| Section | Pages | File here |
|---|---|---|
| Frontmatter | i–iv | `00-frontmatter.pdf` |
| Contents | v | `01-contents.pdf` |
| Preface | vii–x | `02-preface.pdf` |

Everything else — all eleven chapters, the Epilogue, the **Online Appendix**
(pp. 321–322), Notes, References, Index — is `Licensed`. Those three files are
what is in this folder. The rest was deliberately not pulled: bulk-downloading a
licensed monograph is the specific behaviour that gets an institution's De
Gruyter access suspended.

The PDF endpoints sit behind a bot check that returns HTTP 202 with an empty
body to `curl` and to in-page `fetch()`. Only a real top-level browser
navigation to `.../9780226853314-<id>/pdf` returns the file.

## What the Preface establishes about the study's data

- Field study begun **spring 2013**; team of six (two senior authors, four UCLA
  graduate students from a parties seminar).
- **346 interviews across 55 contests** — potentially winnable open-seat House
  races in the 2013–14 cycle. (The 2019 conference version of the paper says
  fifty-three; the book says fifty-five.)
- Some small **exit surveys of primary voters**, supported by Vanderbilt grants.
- Total budget about **$75,000**, roughly half from the senior authors' own
  research funds. No foundation support — which the authors note was
  occasionally an advantage when Republican sources asked who was paying.
- Earlier results appeared in Bawn et al. 2019 and Bawn et al. 2023, "used here
  with permission." The 2023 item is "Groups, Parties, and Policy Demands in
  House Nominations," ch. 5 of *Accountability Reconsidered* (Cambridge). The
  2019 item circulated as a Harvard PIEP working paper, "A Congress of
  Champions: Principal Agent Relationships in US House Nominations" — the
  Harvard-hosted PDF is currently dead (Akamai error).

**There IS an OSF deposit, and it is not open.** The two-page Online Appendix
(pp. 321–322) is a single paragraph, and it gives the address:

> <https://osf.io/s5982> — "interviews for which we have the necessary
> permissions, key spreadsheet examples, UCLA undergraduate papers on which we
> drew, statistical analysis to chapter 9, and some supplemental discussion."

That project is **private** as of 15 August 2026. The web address redirects to
an OSF sign-in page, and the public API answers `{"detail": "Authentication
credentials were not provided."}` for `nodes/s5982`. It is not a broken link
and it is not a permissions tier that a CMU login would open — the node exists
and has not been made public.

This is worth an email to the authors. A book published in 2026 whose data
availability statement points at a closed project is usually an oversight in
the weeks after release, not a decision. If it opens, the "key spreadsheet
examples" and the chapter 9 analysis are the pieces most likely to be usable.

Even opened, expect the core evidence to stay closed. It is interview
transcripts gathered under confidentiality from local actors who were nervous
about being identified — the Preface closes with a Tea Party leader cancelling
by email overnight — and the appendix's own wording limits it to the interviews
"for which we have the necessary permissions."

## The usable public source on the same subject

**Brookings Primaries Project**, staged in `../brookings-primaries-project/`.
Same cycle as the book's fieldwork, and the whole universe rather than 55 cases.

- `PrimariesDatabase_Clean.xls` — 1,662 candidates × 48 columns, every contested
  2014 congressional primary. Compiled by Elaine Kamarck and Alexander Podkul;
  saved 19 Dec 2014.
- `PrimariesCodeBook_clean.docx` — Stata-generated codebook with full value
  labels and per-variable missingness.

Fourteen columns record the candidate's position on a named issue —
immigration, the ACA, Benghazi, taxes, the minimum wage, gun control, abortion,
climate, the NSA, same-sex marriage, the deficit, Keystone, regulation, defense
spending. Each is coded 1–4, and **code 4 is "Candidate Provides No
Information."** The coding was done from candidates' own websites.

That makes silence the modal value of the file:

- Median candidate: states a position on **6 of 14** issues, silent on 8.
- **222 candidates (13.4%)** say nothing about any of the fourteen.
- **12 candidates (0.7%)** address all fourteen.
- Silence by issue runs from 29.3% (ACA) to 84.5% (Benghazi).
- House primary winners are silent on 7.3 issues on average, losers on 9.3.

The file measures what candidates chose to publish, not what they believe — a
website is a campaign document, and a blank is a decision. It is a record of
self-presentation that has been widely read as a record of positions.

Two provenance defects worth keeping rather than cleaning:

- `RaceContested` has its value labels **inverted** in the codebook: `0
  contested`, `1 unontested` [sic, including the typo].
- `district` carries the string `SEN`, so the "congressional primaries" file
  includes 219 Senate candidates alongside 1,443 House ones. A separate
  `Senate` flag exists; the district column does not tell you unless you look.
- `CandidateEducation` is missing for 415 candidates and `CandidateMaritalStatus`
  for 639 — biographical fields the coders could not fill from a website either.

## Why the two go together

The book's finding is that primary voters mostly cannot use the opportunity
reformers built for them, and that intense policy demanders do the vetting
instead. The Brookings file is the public half of that argument: the record a
voter would have to consult is, for most candidates in most races, blank. The
book explains who fills the gap. The dataset shows the gap.

Nothing in the corpus covered this. `03-elections/primary-defeats` is about
incumbents losing primaries and `03-elections/house-competition` about
uncontested general elections; both are outcome files.

**Built, 15 August 2026:** `03-elections/primary-positions`, "What Candidates
Would Not Say" — the first chapter on who *enters* a primary and what they are
willing to say. It sits directly after `primary-defeats` in Part III. Both raw
files are committed beside it.
