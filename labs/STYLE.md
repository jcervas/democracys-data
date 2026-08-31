# How this book is written

Third edition, 31 August 2026. The first edition measured sentences and never
asked what the sentences were about. The second put subject matter first, and
the corpus read better for it. The third fixes the shape of the book: there
are now two kinds of document, each with a fixed skeleton, so a reader always
knows what they are holding and where it will end.

The reader is a **first- or second-year undergraduate, or a lay reader** who
cares about democracy, has not written code, and has not taken statistics.
They are clever and they are not trained. They will read one document, alone,
on a laptop or in an email.

**The book's stance is affirmative and empirical.** Its two governing
questions are *what data is available* and *what it says about the country's
people and democracy* — in that order, and led by the second wherever
possible. A document opens with what its data can show, not with what goes
wrong; failure modes and limits are load-bearing, but they serve the
measurement rather than replacing it. And the book measures rather than
asserts or argues: where a textbook states a regularity as fact ("the
president's party loses seats at the midterm") or a theory text states a
requirement ("democracy requires equality"), a document here turns the
sentence into a question, opens the record, and answers it with data and a
chart the reader can check. Assertions become questions; questions get
answered from the file.

`sh _lib/check-all.sh` runs `check-language.py`, which measures Part Two, and
`check-layout.py`, which measures Parts Three and Four where it can. Part One
cannot be measured and matters more.

---

# Part One — what a document is about

## 1. The subject is the data, not the handling of it

**A document is about where a number came from and what it can bear. It is not
about how the file was obtained, parsed, or stored.** The reader does not care
that a page answered with a status code, that a column arrived as text, that
the download was 163 MB, or that a request carried one header rather than
another. None of that changes what the source can tell them.

> **Was:** On 2026-08-13 every one of them was requested the way a browser
> would.
>
> **Is:** On 2026-08-13 all 51 of those pages were opened, one after another,
> and what happened at each one was written down.

The test is a question, asked of every sentence describing method: **if this
were false, would the reader trust the data differently?** Georgia's
registration numbers carry leading zeros that vanish when the column is read
as a number — that passes, because a reader who does not know it will
mis-join the file and get a wrong answer. A byte count does not pass. A
content type does not pass. The word "parsed" almost never passes.

This rule has teeth: at the second edition's writing, 145 sentences across 57
chapters described machinery, and the phrase "the script" appeared in prose
meant for people who will never see one.

**Where the machinery genuinely is the subject, say so and make it the
subject.** A chapter about a file that lies to `head()` is entitled to talk
about `head()`. A chapter about turnout is not.

## 2. Say what you measured and what you took on trust

Every document mixes two kinds of claim, and a reader cannot tell them apart
unless you do it for them. **Facts you measured are yours and must be
reproducible. Facts you took from somebody else are theirs and must be cited
where they appear**, not only in the sources.

> The legal facts in this chapter are NCSL's, and are cited to NCSL. The
> retrievability facts are measured here, and can be re-measured by anyone.

Never restate a statute, a definition or a finding on your own authority.

## 3. The point goes first

Open the paragraph with the claim, then support it. A reader who stops after
the first sentence should still have the point.

> **Was:** The Bureau publishes the margin beside the estimate in every table,
> nothing is hidden, and yet a number that arrives formatted like a count is
> read as a count — so a quarter of these are not distinguishable from a
> rather different number.
>
> **Is:** A quarter of these numbers cannot be told apart from a very
> different number. Nothing is hidden: the Bureau prints the margin next to
> every estimate. But a number that looks like a count gets read as a count.

**A chapter may open with a story instead.** If a person, a case or a moment
makes the data worth caring about, tell it, in the same plain language as
everything else. What is forbidden is the *sentence* that withholds its point.

## 4. One figure, one caption

**A figure gets one caption, and it goes underneath.** Say what the reader is
looking at, then what to notice. Do not caption the figure twice in different
words, and do not repeat the caption as a paragraph.

At the second edition's writing, 48 chapters carried both a small caption under
the figure and a bold **Figure N** paragraph restating it — about 175 figures
captioned twice. Pick one. If the point needs a paragraph of its own, it is an
argument, not a caption, and it belongs in the prose without the figure's
number attached.

## 5. Every number is computed, never typed

Inline `` `r ` `` expressions, `dd_write_csv()`, `checks.csv`, and the
assertions in every build script. A typed figure goes stale silently; a
computed one cannot. This applies to the closing bullets and the exercises
too: "take the biggest gap" never goes stale, "fifteen counties disagree"
does.

---

# Part Two — how a document reads

These are measured by `_lib/check-language.py`. A chapter can pass all of them
and still be unreadable, but it cannot fail them and be fine.

## 6. One idea per sentence — **no sentence over 35 words**

A sentence that long is three sentences glued together, and the reader has to
take it apart before they can read it. When you catch yourself writing a
semicolon, it is usually a full stop.

## 7. Gloss every technical word the first time the chapter uses it

**To gloss a word is to explain it in plain language right where it first
appears, inside the sentence** — not in a footnote, not in a glossary. The
reader should never have to leave the sentence to find out what a word means.

| Word | Say it like this |
|---|---|
| denominator | the bottom number of a fraction — what you are dividing by |
| numerator | the top number of a fraction |
| margin of error | how far off the estimate could be, and still be doing its job |
| weighting | counting some answers more than others, to fix a sample that does not look like the country |
| residual | what is left over after you subtract |
| regression | a way of drawing the best line through a scatter of points |
| ecological inference | guessing what individuals did from group totals |
| impute | fill in a value nobody recorded, by guessing it from other things |
| crosswalk | a table that translates one set of areas into another |

A glossed word may then be used freely. The gloss is a courtesy on first
contact, not a tax on every sentence.

## 8. No word doing work it has not earned

Cut: *estimand, covariate, areal, disaggregate, provenance, endogeneity,
orthogonal, heteroscedastic.* Say **the thing being measured**, **another
column**, **area-based**, **break apart**, **where the data came from**.

Some terms are worth teaching by name — *unit of analysis*, *the modifiable
areal unit problem* — because a student who meets them elsewhere should
recognise them. Teach them once, in plain words, under rule 7.

## 9. Short paragraphs — three to five sentences, never more than six

A paragraph that runs past six is two paragraphs, and the reader has already
lost the thread.

## 10. Em-dashes are a seasoning — at most one per paragraph

More than that and every claim becomes an aside.

## 11. Second person, active voice

Say *you* to the reader and *the Bureau publishes* rather than *it is
published*. The exception is where nobody in particular did the thing, which
in this book is often the honest description and should stay.

## 12. Plain openings

Do not open a chapter or a section with a subordinate clause, an inversion or
an aphorism. Open with a statement. This is about the sentence, not the
subject: a chapter that opens by telling a story is following the rule as long
as the story is told in statements.

---

# Part Three — two kinds of document

## 13. Decide what you are writing before you write it

Every `.Rmd` declares itself in its YAML front matter: `type: chapter` or
`type: brief`. Absent means brief. The skeletons below are what
`check-layout.py` checks — advisory until the rewrite completes, gating under
`DD_STRICT_TEMPLATE=1`.

**A chapter is a reading about a kind of data.** 2,500–4,000 words. It teaches
what a source is, who made it and why, and what it can bear — so that every
brief leaning on that source can stay short. Its shape:

1. An opening that makes the data worth caring about
2. `## Where the data comes from, and what it is for` — the data biography,
   in its existing form: one opening paragraph, then bold-led paragraphs in
   fixed order — **Who is in it, and how they got there.** / **Who it was
   made for.** / **What it costs to get.** / **What has already been done to
   it.**
3. How the data is published — the files, the cadence, where they actually live
4. One worked example, with one figure
5. `## What this data cannot tell you`
6. `## What you should have learned` — 3–5 bullets
7. `## Where this data appears in this book` — the briefs that use it
8. `## Sources`

A chapter carries **no prediction prompt**. It is a reading, not a lab.

**A brief is a lab.** 1,500–2,500 words. It takes a claim, turns it into a
question, and answers it from the record. Its shape (the running example is
the midterm-penalty brief):

1. **The question** (no heading; plain opening). A claim as the reader has
   heard it, then the turn: *"Political scientists have frequently observed
   that the president's party loses seats in the midterm elections. Is that
   true?"*
2. `## The test` — how we will answer it: the data named with years and
   source, linked; and why this source, of all sources, can answer this
   question, with a link to the section's data-type chapter. *"Using data
   from US House elections, 1858–2024, obtained from official sources
   [link]."*
3. `## The data` — what the reader is actually holding: how the table was
   constructed (*"built by combining the records of each race in each
   election"*), what cleaning was done and what it decided, and the
   variables it contains, by name.
4. The prediction prompt.
5. `## What it says about democracy` — the finding, in figures and prose.
   The first figure carries one or two sentences saying what the chart shows
   and why this form — a slope, a scatter, a map — fits the question. The
   exceptions get their story: *"With few exceptions, the observation holds.
   2002 is one: President Bush's approval was extraordinarily high, and the
   districts had just been redrawn after the 2000 census."*
6. `## What this brief cannot tell you` — short; the limit built into the
   file, not into the analysis
7. `## What you should have learned` — 3–5 bullets, every number in them
   computed inline, never typed
8. `## Extensions` — additional questions and ideas for future research:
   spreadsheet-answerable questions naming file and column, then one or two
   stretch ideas beyond the spreadsheet
9. `## Sources` — data citations, then **The data itself**, then the AI
   prompt box, last

## 14. The title names the data and the question

The evocative sentence goes in the subtitle, where it earns its keep under a
title that says what the document is about. Two title shapes are banned: a
full sentence that withholds its resolution, and a bare number-teaser that
names no dataset and no concept.

> **Good:** "The Perception Gap: What Each Party Thinks the Other Looks
> Like" · "Bellwether Counties, 1960–2024" · "Party Identification and the
> Leaner Problem"
>
> **Bad:** "Everyone Guesses Forty" · "Everybody Moved, Almost Nobody
> Left" · "Free, Public, and Unreachable"

The bad ones are good sentences in the wrong slot. Demote them: "Free,
Public, and Unreachable" survives happily as a subtitle under a title that
names state election sites and retrievability.

## 15. Every document travels alone, to a lay reader

Strip course machinery from anything the reader sees: no "84-355 Democracy's
Data" author line, no "this course", "this session", "this week". A brief's
only required dependency is **one link to its section's data-type chapter**,
in the Why-this-data paragraph. Everything else it needs, it carries.

## 16. Figures come from the shared chart library

New and rewritten figures use the DD chart library — `labs/_lib/dd-charts.js`,
reached through `dd_fig()` in `labs/_lib/dd-charts.R` — with class-based
colors, so the corpus's figures look like one book and restyle in one place.
Hand-written D3 is reserved for designated showpieces, and a showpiece is
designated, not drifted into.

---

# Part Four — what every document must contain

The structural conventions, each enforced by a checker:

- **A closing `## Sources`**, giving the publisher's address for every source
  the chapter reads, written as `<https://…>` after the citation it belongs
  to. Naming the agency is not enough, and pointing at another chapter is a
  dead end: a document travels alone. A source with no address — a printed
  book — is cited by ISBN and named in `check-sources.py`.
- **A prediction prompt, in briefs only**, before the brief's main turn,
  asking the reader to commit to a number before they see it.
- **A section saying what the document cannot tell you** — `## What this data
  cannot tell you` in a chapter, `## What this chapter cannot tell you` in a
  brief. Not hedging: the specific questions this source cannot answer, and
  why.
- **`## What you should have learned`** — 3 to 5 bullets a reader could
  repeat a week later. Any number in them is computed inline (rule 5).
- **A `data/build-data.R`** that rebuilds everything the chapter prints.

Three conventions carried forward from the second edition, one of them moved:

- **The AI prompt box.** Every chapter that fetches external data closes its
  sources with `ai_prompt(readLines("data/ai-prompt.txt"))`: a prompt a reader
  can paste into an assistant to rebuild the data. Fixed shape — THE SOURCE,
  HOW TO FETCH IT, WHAT TO BUILD, CHECK YOUR WORK. A source that cannot be
  fetched again uses `tone = "frozen"` and a different shape — WHY NOT, WHAT
  EXISTS INSTEAD, WHAT YOU CAN STILL CHECK. Every number in the check block is
  taken verbatim from the chapter's own tables; `check-ai-prompt.py` holds it
  there.
- **The data biography** now lives in the data-type chapter (rule 13), under
  `## Where the data comes from, and what it is for`. One dataset, one
  biography; every brief reading the source links there rather than carrying
  its own.
- **The tables, and `## Extensions`.** Sources closes with **The data
  itself**, linking the CSVs the figures rest on — the tables, not the
  folder, and not the chapter's own bookkeeping. `## Extensions` absorbs what
  "Your turn" used to hold: three or four open questions the tables can
  answer and the brief did not ask, each naming the file *and the column*,
  each answerable by sorting or grouping in a spreadsheet — an exercise that
  needs R is an exercise nobody does. Then one or two **stretch extensions**
  that point past the spreadsheet: another year to fetch, another state to
  compare, a claim to check against a second source. A question whose answer
  is printed above is a quiz, and this is not a quiz.

---

Plain language is not a lower bar. The reading gets easier; the thinking the
document asks for does not.
