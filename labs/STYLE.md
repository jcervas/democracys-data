# How this book is written

Second edition, 30 August 2026. The first edition measured sentences and never
asked what the sentences were about, so the corpus passed every check and still
read badly. What follows puts subject matter first and mechanics second.

The reader is a **first-year undergraduate** who cares about democracy, has not
written code, and has not taken statistics. They are clever and they are not
trained. They will read one brief, alone, on a laptop or in an email.

`sh _lib/check-all.sh` runs `check-language.py`, which measures Part Two. Part
One cannot be measured and matters more.

---

# Part One — what a brief is about

## 1. The subject is the data, not the handling of it

**A brief is about where a number came from and what it can bear. It is not
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

Every brief mixes two kinds of claim, and a reader cannot tell them apart
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
computed one cannot. This applies to the exercises too: "take the biggest gap"
never goes stale, "fifteen counties disagree" does.

---

# Part Two — how a brief reads

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

# Part Three — what every brief must contain

Four structural conventions, each enforced by a checker:

- **A closing `## Sources`**, giving the publisher's address for every source
  the chapter reads, written as `<https://…>` after the citation it belongs
  to. Naming the agency is not enough, and pointing at another chapter is a
  dead end: a brief travels alone. A source with no address — a printed book —
  is cited by ISBN and named in `check-sources.py`.
- **A prediction prompt** before the chapter's main turn, asking the reader to
  commit to a number before they see it.
- **A section saying what the chapter cannot tell you.** Not hedging: the
  specific questions this source cannot answer, and why.
- **A `data/build-data.R`** that rebuilds everything the chapter prints.

Three younger conventions sit beside those:

- **The AI prompt box.** Every chapter that fetches external data closes its
  sources with `ai_prompt(readLines("data/ai-prompt.txt"))`: a prompt a reader
  can paste into an assistant to rebuild the data. Fixed shape — THE SOURCE,
  HOW TO FETCH IT, WHAT TO BUILD, CHECK YOUR WORK. A source that cannot be
  fetched again uses `tone = "frozen"` and a different shape — WHY NOT, WHAT
  EXISTS INSTEAD, WHAT YOU CAN STILL CHECK. Every number in the check block is
  taken verbatim from the chapter's own tables; `check-ai-prompt.py` holds it
  there.
- **The data biography.** The chapter that introduces a dataset carries
  `## Where the file comes from, and what it is for`, placed after the
  prediction prompt: one opening paragraph, then bold-led paragraphs in fixed
  order — **Who is in it, and how they got there.** / **Who it was made
  for.** / **What it costs to get.** / **What has already been done to it.**
  One dataset, one biography; every other chapter linking the source links
  there.
- **The tables, and `Your turn`.** Sources closes with **The data itself**,
  linking the CSVs the figures rest on — the tables, not the folder, and not
  the chapter's own bookkeeping. Then three or four open questions the tables
  can answer and the chapter did not ask. Name the file and the column. Ask
  something genuinely open: a question whose answer is printed above is a
  quiz, and this is not a quiz. Every question must be answerable by sorting
  or grouping in a spreadsheet — students in this course are not asked to
  write code, and an exercise that needs R is an exercise nobody does.

---

Plain language is not a lower bar. The reading gets easier; the thinking the
brief asks for does not.
