# Course map — the fourteen data sessions, F26

Rewritten to the six provenance parts, which are now the book's structure and
the semester's at once. Working reference for building and maintaining labs.
**All 108 chapters are scheduled**, and with the six `part-N-*` openers and the
introduction that is 115 of the 116 rows in `INDEX.md`. The 116th is `gotv`, the
only chapter deliberately out. The book was re-architected on 14 Aug around
**where the data came from** — see *The six parts* below.

**Legend.** ★ = built and knitting · ✦ = built, needs extending · ✚ = to build ·
★ = the session's headline finding · **(Tue)** = a Tuesday companion.

**The spine.** Thursday is one source of data. The order is cumulative: nothing
is inferred from a source students have not already met. Tuesday is the textbook
and the argument; the three tests cover the textbook and nothing else.

**The parts are the spine, and they are the book's parts.** Each part opens with
a `part-N-*` opener that argues the part, then a `*-source` chapter that says what
the kind of data is, and every chapter after that asks a question of the same
instrument. The order is set by the cumulative rule rather than by taste: **Part I
depends on nothing** — it is the only part that doesn't — and each part after it
can be taught because the ones before it have been. The authority for the
assignment is `labs/_lib/make-index.py`, which generates `labs/INDEX.md` in this
same order; this file schedules what that one lists.

| Part | Source chapter | Ch. | Sessions |
|---|---|---:|---|
| **I** The Census Bureau | `census-source` | 21 | 1–3 |
| **II** Surveys | `surveys-source` | 17 | 4–5 |
| **III** Elections | `returns-source` | 42 | 6–8, 10, 11 |
| **IV** Records of Political Actors | `rollcalls-source` | 17 | 9, 12 |
| **V** Records of Ordinary People | `admin-records-source` | 4 | 12 |
| **VI** Putting Data Together | *(no source chapter)* | 7 | 13–14 |

The count excludes each part's `part-N-*` opener, which is a different thing
from the source chapter in the column beside it. `INDEX.md` counts both, and it
is the authority.

**The book is organised by where the data came from**, not by what it is about.
Turnout appears in four different parts and means something slightly different
in each, because a different organisation produced it for a different reason.
Grouping by topic would hide that; grouping by source is what makes it visible.

**I — The Census Bureau.** One agency that counts people and places. It goes
first because almost every other part borrows a denominator from it: 16 of the
95 chapters elsewhere in the book go to the Bureau for a number.

**II — Surveys.** Asking a sample of people questions when nobody is obliged to
answer. Part I already taught a survey — the ACS — and filed it under counting,
because answering the ACS is required by law under 13 U.S.C. § 221 and answering
the ANES is not. Putting the two parts a session apart is what turns that from a
filing quirk into the lesson.

**III — Elections**, in two runs. The **result** — certified, signed for, and
published in fifty-one different formats, which is why no national file exists
and every national number was assembled by somebody. Then the **machinery**:
registration, removals, rejected ballots, precinct boundaries. Both get called
election data, and swapping one for the other is a common published error.

**IV — Records of Political Actors.** What somebody who chose to run for office
leaves behind: roll calls, filings, disclosures, attention. Opens on the voting
record because it is the cleanest data in the book — the vote *is* the event —
and then turns to money, where every number is a claim its subject made about
itself. The thinnest substantive part, and the one most worth building out.

**V — Records of Ordinary People.** The same kind of institutional record with
one difference that changes everything: nobody in it volunteered. A stop, a jury
summons, a lending grade. Four chapters, and the modern case — commercial
location data — is not built yet.

**VI — Putting Data Together.** No new data; every file was met earlier. What is
new is combination, which is where the interesting questions and nearly all the
wrong answers live.

> **This replaced two earlier claims, and both were weaker.** The order was once
> declining collector authority, then "how you came to be in the data." Both
> described the *collector*. This one describes where a reader should go to
> understand a number, which is what a book organised by source is for.

**Money runs the week before the election, out of part order**, and that is
possible because Part IV depends on nothing in Part III. It also puts polls
(Session 5) and the forecast material ahead of election night, so Session 10
grades work the class has already done.

**One exception, and one session protected.**

**`election-night` runs Nov 5 regardless.** It is a Part II chapter and it cannot
travel with its part: it is built around live 2026 returns, with a `REHEARSAL`
flag flipped on the morning of the session. Session 10 is therefore Part II
arriving five weeks after the rest of Part II, and the session is better for it —
`midterm-loss` was met in Session 6 and is graded here against what actually
happened. This is the only chapter in the book whose date is not ours to choose.

**Part III carries a third of the book on five Thursdays, and it can.** Six
parts at an even density want more Thursdays than the term has. Elections is the
largest part at forty-two chapters, and also the one with the most
brief-friendly material: `sparklines`, `distributions`, `wind-map` and `mapping`
read in twenty minutes, and `careers`, `primary-defeats` and `bellwether` are
short arguments rather than sessions. Folding those into the Tuesday slot is what
lets the machinery run fit into Session 11 without any part losing its argument.

**Parts IV and V share Session 12, and the pairing is deliberate.** One is what
people who chose to run for office leave behind; the other is what people who
never volunteered leave behind. Those are the same kind of record with one
difference, and teaching them in one session is the clearest way to make that
difference land.

> **What this protects.** Part VI holds **both** of the last two Thursdays, so
> `rpv` and `vote-dilution` are reached even if the semester slips by a session.
> That was the governing constraint on this tail, and everything else was
> arranged around it.
>
> **If time runs short, cut in this order:** the second half of Session 13
> (`bisg-check` and `uncertainty` become briefs — both are graded again inside
> `rpv`, so the lesson survives) → then Session 6's career cluster
> (`retirements`, `primary-defeats`, `careers`) → **never Session 14.** The
> districting material is the payoff the whole book builds toward, and it is the
> only place a score is graded against a known truth.

**Tuesday companions — twelve of them.** The textbook does not need all eighty
minutes, so the twelve labs marked **(Tue)** below are read as briefs on
students' own time and taken up for twenty to thirty minutes. This is capacity the course already had — it keeps
Thursdays to one source of data each and costs no data session. **The derived
chapters are what this slot is for**: `overplotting`, `chord`, `sparklines`,
`distributions`, `pie-radar`, `streamgraph` and `rank-size` each read a chapter
the class has already met and ask what a chart does to it, which is exactly a
twenty-minute conversation and not a session.

---

## Brought current — 29 August 2026

This file had drifted behind the corpus by ten chapters and one broken build.
What that pass changed, so the next reader knows what was checked:

- **Ten chapters were built and never scheduled here.** `gender-gap` and
  `partisan-economy` into Session 4, `poll-weighting` into Session 5,
  `crossover` and `nationalization` into Session 7, `media-ideology` and
  `follower-counts` into Session 9, `senate-2026` into Session 10, and
  `oral-argument` into Session 12. Each session's finding names what the new
  chapter adds.
- **Every count in this file was recomputed rather than adjusted.** The chapter
  totals come from `INDEX.md`, the beat table from the openers' own
  `derived/beats.csv`, and the census-reuse figure from
  `part-1-census-bureau/data/derived/reuse.csv`. The parts table said 89
  chapters; there are 108, plus six openers and the introduction.
- **Part II's opener would not build.** Its beat map ran
  `validated-turnout · ces-class · [three use chapters] · poll-simulation`, and
  the contiguity assertion failed — correctly. `ces-states` and `poll-weighting`
  are now labelled `5 critique`, which is what they are: one grades a published
  margin against the error it made, the other turns the weighting dial until the
  winner changes. `gender-gap` stays `4 use` and moved up beside `party-id`,
  where it belongs anyway — it takes apart a published statistic, as `party-id`
  does, rather than turning on the instrument.
- **Two placement bullets named sessions and parts that no longer exist.**
  `turnout-denominator` is Session 11, not 8; `bellwether` is Session 8 and
  Part III, not Session 6 and Part II.
- **The one folder that was not a chapter is now one.** `perception-gap` had a
  benchmark table with no build script behind it and no brief. Built the same
  day into Session 5, `5 critique`, which takes Part II to 17 chapters.

**The lesson for the next person who adds a chapter.** `make-index.py` and the
part openers catch a missing `PARTS` entry and a broken beat order. Nothing
catches a chapter that is built, indexed and absent from this file. Adding the
slug to a session here is the last step of building a chapter, not an
afterthought.

---

## The sessions

| # | Date | Wk | Pt | Session | Labs | ★ Finding |
|---|---|---|---|---|---|---|
| — | Tue Aug 25 | 1 | — | *Intro: the three layers* | ★ `introduction` · ★ `electoral-map` · ★ `levels-of-aggregation` **(Tue)** · **CES + census replication surveys taken in class** | The vote-weighted Democratic mean is **48.90% across 2,684 Georgia precincts, 48.90% across 159 counties, 48.89% for the state** — identical at every rung while **30.1% of the variation disappears**; and Goodman's regression on those same precincts says **194.0% of Georgia's mail voters chose Biden** against a published truth of **64.5%**, getting *worse* at county level (**233.4%**) |
| **1** | Thu Aug 27 | 1 | **I** | **What a census is, and how you get it** | ★ `census-source` · ★ `census-decennial` *(new)* · ★ `census-pep` *(new)* · ★ `census-race` · ★ `census-access` *(new)* · ★ `census-api` · ★ `census-coverage` *(new)* | The one thing nothing else can do — the whole decennial is **seven questions and six tables**, and its 2020 undercount ran **3.30% for Black and 4.99% for Hispanic residents against a 1.64% *over*count for non-Hispanic White residents**, both gaps *wider* than 2010; **94.5%** of "Some Other Race" respondents are Hispanic, answering neither question with the box the form provides; **73 of the API's 338 variables exist because the VRA needs them**; and three official routes to one county's population give **1,250,578 / 1,240,476 / 1,231,814**, with the survey's margin of error published as **−555555555**, the code for *this estimate was controlled* — controlled to the estimates program, whose own arithmetic closes to the person on all **3,144 counties** and says **84.3% of 2024 growth was international migration** while **two-thirds of counties buried more people than they delivered** |
| **2** | Thu Sep 3 | 2 | **I** | **Geography, and the scales it comes at** | ★ `census-geography` · ★ `demographics` · ★ `zip-codes` · ★ `areal-units` **(Tue)** · ★ `overplotting` **(Tue)** | Race at eight scales, and only the smallest describes anyone; a ZIP is a routing key for mail trucks; and the same **11,748 Fulton blocks** cut into 6 equal-population units by rules that never look at race give **0 to 4 majority-Black units** — the unit itself decides |
| **3** | Thu Sep 10 | 3 | **I** | **What the count is used for** | ★ `census-acs` *(new)* · ★ `apportionment` · ★ `section-203` · ★ `migration` · ★ `names` *(new)* · ★ `surnames` · ★ `regional-shift` **(Tue)** · ★ `chord` **(Tue)** · `age-structure` *(self-study)* | The one-year ACS covers **854 counties and the five-year all 3,222** — for **73.5% of the country there is no annual estimate at all** — and the three-year series **404s from 2014 on**; NY missed seat 435 by **89 people**; Philadelphia covered at 10,150±450 and Solano not at 9,943±377 — **the intervals overlap**; **938 of 2,652 migration pairs (35%) cannot be told apart from zero** yet hold only **2.21%** of movers; over six decades **75 seats changed states**. And a name is asked to do three jobs at once — identify a person, stand in for their sex, stand in for their race — while being a distribution rather than a label |
| **4** | Thu Sep 17 | 4 | **II** | **What a survey can establish, and how you get one** | ★ `surveys-source` · ★ `survey-access` *(new)* · ★ `validated-turnout` · ★ `anes` · ★ `party-id` · ★ `gender-gap` *(new)* · ★ `ideology` · ★ `partisan-economy` *(new)* · ★ `thermometers` *(new)* · ★ `abortion-opinion` | What no sample size will buy; the survey overstated the 2016 electorate by **8.6 million**; **one coding decision moves the most repeated number in American politics by a factor of five**. And of six survey archives, all free, **four will not answer a program** — one of them tells `curl` 403 and Python 200 from the same address with identical headers. Then the same file measured over fifty years: warmth toward your own party went **70.63 → 70.11** while warmth toward the other party fell **47.98 → 20.62**, so **98.1% of all the movement is on the out-party side** and the share rating the other party at absolute zero went **5.4% → 33.2%** — on a scale coded **0–97, where 98 and 99 are "don't know" and "not ascertained" and no value of 100 exists anywhere to give them away**. Then the most-quoted subtraction in American politics: the gender gap is **14.0 points** in 2024 because men moved **34.8** points since 1972 and women **22.1**, and the pair underneath it is never printed. And a question with a fact behind it — **28.3%** of Democrats and **82.5%** of Republicans said the economy had got worse in 2024, a **54.2**-point gap about one country, which four years earlier ran **40.6** points the other way |
| **5** | Thu Sep 24 | 5 | **II** | **Weighting, polls, and what a margin hides** | ★ `poll-weighting` *(new)* · ★ `ces-class` · ★ `ces-states` *(new)* · ★ `gss-confidence` · ★ `poll-simulation` · ★ `perception-gap` *(new)* · ★ `models-markets` | CES weighting moves "registered to vote" from **91.5% to 72.5%**; at n=100,000 the poll reports **±0.31 and is wrong by 4.5**; **746 resolved markets, Brier 0.064**. Then the margin itself: a 60,000-person survey is **3,618 Texans and 68 Vermonters**, state margins run **2.71 to 15.81 points (median 6.20)** after the weights cost a **design effect of 2.78**, and sample size predicts a state's margin of error at **−0.75 but its actual error at only −0.25** — precision, not accuracy — so **45 of 51 states** fall inside their own margin where sampling error alone predicts **48.4**. And the one poll that was handed over respondent by respondent: **867 Florida voters**, four outside pollsters given the same file, **five points** of spread and no agreement on who was ahead. And the most-reprinted misperception in American politics, rebuilt to five decimal places and then split by who was answering: every one of eight groups is overestimated, by **17.7 points** about your own party and **24.6** about the other one, worst of all Democrats guessing how many Republicans earn over $250,000 (**41.9** points out against a true **2.2%**). Underneath it, **61.1%** of the 8,000 guesses end in a zero and **8.9%** are exactly fifty, so measured off the middle rather than the mean the gap goes **19.8 → 14.0** |
| **6** | Sep 29/Oct 1 | 6 | **III** | **What a return is, and where you get one** *(self-study)* | ★ `returns-source` · ★ `county-returns` *(new)* · ★ `data-sources` · ★ `panhandle-claim` *(new)* · ★ `historical-campaigns` · ★ `mapping` · ★ `sparklines` **(Tue)** | Complete, official, and permanently silent about who did it; **DC is coded 11001 in both 2020 and 2024 and means different things**; Solid South gap +41.7 (1924) → −7.4 (2024). And assembling the file no agency publishes takes **102 documents in 11 formats from 51 officers** — after which the certified record and the compilation everyone uses agree on the Democratic vote in all but **42** counties and disagree on **how many people voted in 1,588 of 3,138**. And the Republican nominee for governor, the first Black nominee either party has had, ran from **18.0%** in Lafayette to **65.8%** in Collier — a **47.8**-point spread that the white winner of the last contested primary **beat**, at 51.9, on a map correlating **0.72** with his |
| **7** | Thu Oct 8 | 7 | **III** | **Returns over time, and the careers in them** | ★ `clerk-source` *(new)* · ★ `house-competition` · ★ `crossover` *(new)* · ★ `nationalization` *(new)* · ★ `rosters-source` *(new)* · ★ `midterm-loss` · ★ `vote-targeting` · ★ `retirements` · ★ `primary-defeats` · ★ `primary-positions` *(new)* · ★ `nomination-anchors` *(new)* · ★ `nomination-rules` *(new)* · ★ `careers` · ★ `distributions` **(Tue)** · ★ `neutral-maps` *(new)* | The Clerk publishes the official returns as a typeset **document** — 11 PDFs, 43,917 lines of text, no table anywhere — and three parsing bugs were invisible in the aggregate (**Ralph Abraham recorded with 2 votes instead of 134,616**); a roster is not a return, and **40,432 people have served in 38,088 seats**, with **64.2%** of modern departures decided by no voter; president's party lost seats in **36 of 41** midterms (88%), median −28; one name on the ballot is ordinary; ranking by contribution vs % share has **zero overlap** in the top 5; four hundred districts drawn four ways give an average that describes almost nothing. And 5,000 computer-drawn maps per state say the country could be holding **109 competitive districts instead of 79**, while the twenty states one party controlled after 2020 could cut Black-plurality representation from **14 seats to 2** under a maximized gerrymander — the Senate, drawing no lines at all, has no such lever. Of **1,662** primary candidates coded on fourteen issues, the typical one stayed silent on **8**, quietest on Benghazi (**84.5%** silent) and loudest on the ACA (**29.3%**); of 55 nominations studied, **34** candidates drew coordinated support and **30** of those won — but the codebook that would explain how was never released; and **8** states hold a real runoff while one in five outright primary winners took under **50%** of the vote, a threshold nowhere written into statute. And most of 2024 was already written down in 2020: split outcomes ran **27 of 50 states** in 1979 and **4** in 2025, split districts **40.3%** in 1984 and **4.0%** in 2024 — a fall the count itself cannot explain, because 1984's districts with today's candidates give **4.6%** and today's districts with 1984's candidates give **14.1%** |
| — | Tue Oct 20 | 9 | — | *Test 1* | — | **Sides et al., Ch. 1, 2, 13, 6** |
| **8** | Thu Oct 22 | 9 | **III** | **The bottom rungs: precincts and ballots** | ★ `whole-foods-cracker-barrel` *(new)* · ★ `ga-precinct-returns` · ★ `precinct-geography` · ★ `cast-vote-records` · ★ `mid-decade` *(new)* · ★ `mid-decade-florida` *(new)* · ★ `wind-map` · ★ `bellwether` **(Tue)** | Only **84.5% of precinct names survive 2020→2024**, and areal vs population weighting disagree about whether **78.3% or 18.6%** of precincts were split; given 4 rankings **65% of Alaskans used one**; **zero** perfect bellwether records over seventeen elections. Reconstructed precinct returns replayed under Texas's 2025 mid-decade map elect more Republicans in every one of seventeen elections already on the books, a gain of **2 to 5** seats depending on the year; Florida's own redraw does the same on **13** replayed elections, one district's presidential margin moving from **51% to 45.4%** Democratic |
| **9** | Thu Oct 29 | 10 | **IV** | **Filed because somebody had to** | ★ `finance-source` · ★ `campaign-finance` · ★ `pie-radar` **(Tue)** · ★ `independent-expenditures` · ★ `lobbying` · ★ `media-attention` · ★ `media-ideology` *(new)* · ★ `streamgraph` **(Tue)** · `campaign-visits` *(brief)* · ★ `follower-counts` *(new)* · ★ `finance-network` · ★ `rank-size` **(Tue)** | The only source in this book written by the people it is about — **16 of 73,449 FEC rows carry 90.4% of the dollars**, inflating outside spending from $4.75bn to $49.7bn **and reversing support/oppose from 94.5/5.5 to 42.4/57.6**; and one dollar figure across 17 issues, with no division. Two more ways to place an actor without asking anybody: **375,979 shared links to 184 news domains** put outlets on the same scale as the members who link to them, and **1,611 profile looks** in one day give three platforms describing three different Congresses |
| **10** | Thu Nov 5 | 11 | **III** ⚑ | **Live returns** *(fixed date)* | ★ `election-night` · ★ `seat-forecast` *(new)* · ★ `senate-2026` *(new)* · *revisit* `midterm-loss` | Baseline right in **418/433** districts (96.5%); did anyone beat the median? The forecast built the week before reproduces the 2024 House's **215** Democratic seats exactly, and sharing one national error term instead of 51 independent state ones moves the odds of the Democrats taking both chambers from **53.2% to 58.7%**. And the sheet that has to be signed in August: **35 seats** on the 2026 ballot while **34** of the Senate's other 65 are already Democratic |
| — | Tue Nov 10 | 12 | — | *Test 2* | — | **Sides et al., Ch. 3, 11, 5, 10** |
| **11** | Thu Nov 12 | 12 | **III** | **The machinery behind the result** | ★ `voter-files-source` · ★ `voter-file-access` *(new)* · ★ `voter-files` · ★ `false-matches` · ★ `disenfranchisement` · ★ `turnout-denominator` · ★ `eavs` · ★ `residual-votes` | The only instrument that can tell you whether a survey is lying — **21 of 53 columns** just route you to a ballot, 70% of the file has no party signal, and **13,803 people voted in 2020 and are gone from the 2026 file**. And the list is public by law in every state while **exactly 1 of 51 publishes it for download**: 33 addresses answer, 26 of those hand you a form; and **584,463 mail ballots rejected** (1.22%), **Oklahoma 4.35% against Vermont 0.20% — a factor of 22** |
| **12** | Thu Nov 19 | 13 | **IV**+**V** | **Who volunteered, and who did not** | ★ `rollcalls-source` · ★ `dw-nominate` · ★ `scdb` · ★ `oral-argument` *(new)* · ★ `officeholder-age` **(Tue)** · ★ `admin-records-source` · **choose one:** ★ `policing` / ★ `jury-selection` / ★ `redlining` | Every vote since 1789, and no note of what most of them were about; Senate polarized as much as House (0.587→0.917). Then the mirror: the same kind of record about people who never ran for anything — and an administrative record arrives as a **numerator**, the same stops over three published denominators giving disparities from **3.07× to 4.79×**. The same Court is also measured twice — one file made of a coder's judgments, one made of a clock — and the clock gives **six defensible answers** to who talks the most |
| **13** | Tue Nov 24 *(remote)* | 14 | **VI** | **A number somebody built** | ★ `uncertainty` · ★ `cost-of-voting` · ★ `bisg-check` | Nobody recorded any of these. **BISG is right 40.3% of the time** on people whose race the state actually records; fifty-six election laws compressed into one number give **four different answers from one file**; and a confidence interval covers one kind of error out of several |
| **14** | Tue Dec 1 | 15 | **VI** 🔒 | **Grading a score against a known truth** | ★ `rpv` · ★ `vote-dilution` · ★ `sweet-spot` · ★ `redistricting` | **Goodman: 102.1% of Black voters (R² 0.982). Truth: 92.7%.** Bounds [70.1, 100]. Then § 2: at the only setting *Callais* still allows (`reach = 1`) the **306 admissible seeds span 22.64 points, 27.21% to 49.85% Black adults, and 0 of 306 clear 50%** — while Houston County's **enacted District 4 already sits at 53.7% at a Polsby-Popper of 0.266**, beating all 15 algorithm districts on both measures at once. And **91 of 434 districts (21%) sit within 5 pts of even; MA +22.4 and not gerrymandered** |
| — | Thu Dec 3 | 15 | — | *Test 3, then the wrap* | **V-Dem** *(discussion)* | **Test 3: Ch. 4, 7, 8, 9, 12.** Where American election data sits against the cross-national indices |

🔒 = **protected.** Session 14 is the one session that does not get cut, moved or
compressed; the tail was built around reaching it.
⚑ = the only chapter whose date is not ours to choose. `election-night` is Part II
arriving five weeks after the rest of Part II, because the election is on Nov 3.

**The reorder closed the one chapter taught away from its part.**
`models-markets` is a Part V chapter — a forecast and a prediction market are
commercial products, not survey instruments — and it used to be taught in the
surveys session because that is where the question felt live. It now sits in
Session 9 with the rest of Part V, one week before the election, which is a
better home for it than either. **Every chapter is now taught inside its own
part**, with `election-night` the single exception, and that one is a date the
calendar chooses.

---

## The five beats — how a part is built on the inside

Part I set the template and the other five were rebuilt against it in August.
A part opens with a **source** chapter, introduces its **instruments**, says how
you **access** them, and then spends the rest of itself **using** them — and,
where the material earns it, **turning on** them.

A chapter's beat is a judgment about what the chapter is *for*, and it is not
recoverable from its files, so each opener states it as an explicit `BEAT` map
in `data/build-data.R` rather than inferring it. Each opener then asserts that
the beats are **contiguous**: an access chapter sitting in the middle of the
applied ones is a part in the wrong order, and the build fails rather than
rendering it.

| Beat | What it does |
|---|---|
| **1 source** | What this kind of data is, and what it structurally cannot say |
| **2 / 3 instrument** | One chapter per instrument that produces it |
| **2 / 3 access** | How you actually get hold of it, and what each route hands you |
| **4 use** | Chapters that use the data to answer a question |
| **5 critique** | Chapters that turn on the instrument after it has been used |

The number in the label is the beat's *position in that part*, which is why
instrument and access each appear twice. Where a part's finding is that you
cannot get the file, access runs first.

| Part | source | instrument | access | use | critique |
|---|---:|---:|---:|---:|---:|
| **I** The Census Bureau | 1 | 4 | 2 | 14 | — |
| **II** Surveys | 1 | 1 | 1 | 8 | 6 |
| **III** Elections | 1 | 1 | 1 | 39 | — |
| **IV** Records of Political Actors | 1 | 2 | — | 14 | — |
| **V** Records of Ordinary People | 1 | — | — | 3 | — |
| **VI** Putting Data Together | — | — | — | — | 7 |

Every cell is read from the openers' own `derived/beats.csv`, not typed here.

**Two parts cover two instrument families each, and their beat maps cover the
first run only.** Part III runs the result and then the machinery; Part IV runs
the voting record and then money. In both, the second run opens with its own
source chapter partway down the part — `voter-files-source` and `finance-source`
— and those stay deliberately out of the beat map, because labelling them would
drop a `1 source` into the middle of the applied chapters and fail the
contiguity assertion for an order that is deliberately right.

**Part VI runs one beat and no others, which is the honest description of it.**
It introduces no instrument because it introduces no data: every file it touches
came from Parts I–V. Every chapter takes a number somebody built and grades it.

**No access beat in Parts IV, V or VI.** A disclosure filing is compelled into a
public bulk download and a roll call is published by the clerk who recorded it,
so *can you get it* is not a question those parts turn on. Where it is the
question — the survey archives, the voter file — access runs early and the beat
is numbered 2.

**`5 critique` is new, and it is named from the corpus's own words** — `PARTS`
already said the surveys part goes in two halves, "first USE them… only then TURN ON
them," and the beat is that sentence made checkable. Seven chapters in
Part VI and three in Part II had been labelled `4 use`, and that was wrong: they
do not apply the data to a question about politics, they take a number the reader
has just been asked to trust and grade it. `bisg-check` against known race,
`validated-turnout` against counted ballots, `rpv` against a published truth,
`poll-simulation` against what a margin of error does not cover. Part VI is now
**critique and nothing else** — seven chapters, no use chapter at all — which is
the honest description of a part about numbers nobody recorded.

**Technique companions are marked and exempt.** `overplotting`, `chord`,
`sparklines`, `distributions`, `pie-radar`, `streamgraph` and `rank-size` are
display chapters that draw no data of their own and are taught beside the chapter
whose data they draw, which drops a `4 use` chapter inside an earlier beat's
block. Two rules were colliding — beats contiguous, companions beside parents —
and both are right, so a `COMPANION` set in each opener marks them and the
contiguity assertion skips them. A chapter that merely reuses a sibling's file to
ask its own question is *not* a companion and stays in the test.

## The source chapters, and the two parts that carry two

Surveying the six parts against the beat vocabulary found three instrument
families with chapters using them and nothing introducing them. Each gap is now
closed by a source chapter built to house convention — `build-data.R`,
`checks.csv`, prediction prompt, limits section — and knitting to HTML and PDF.

| Chapter | Part | Session | The gap it closed |
|---|---|---|---|
| `clerk-source` | III | 7 | The official House returns arrive as a **typeset document**: 11 PDFs, 43,917 lines, no table anywhere. Everything downstream of `parse-clerk.py` assumed a file; nothing said what the file had to be made out of. |
| `rosters-source` | III | 7 | A roster is not a return. **40,432 people have served in 38,088 seats**, and **64.2%** of modern departures were decided by no voter at all — the careers cluster was reading rosters as if they recorded elections. |
| `admin-records-source` | V | 12 | An administrative record arrives as a **numerator with no denominator**. The same traffic stops over three published denominators give disparities from **3.07× to 4.79×**; `policing`, `jury-selection` and `redlining` each hit this and none introduced it. |

**Two parts carry two source chapters each, and that is the design.** Part III
covers the result and then the machinery, so `voter-files-source` opens a second
run partway down. Part IV covers the voting record and then money, so
`finance-source` does the same. In both cases the later source chapter stays out
of the beat map on purpose — see the beats section above.

There are now **nine** `*-source` chapters and six part openers, and they are
different things: an opener is named `part-N-*` and argues the part, a source
chapter says what a kind of data is.

**Part VI has no source chapter at all**, and that is correct rather than an
omission: it introduces no data. Every file it touches was met in Parts I–V.

## Known forward references

The book's rule is that nothing is worked out from a source the reader has not
met. Six chapters break it — all of them by *reading another chapter's built
file*, none by assuming the reader has read that chapter — and they are recorded
here rather than hidden.

| Chapter | Reads | Which sits in |
|---|---|---|
| `census-geography`, `areal-units` | `ga-precinct-returns` | Part III |
| `demographics` | `residual-votes` | Part III |
| `demographics` | `redlining` | Part V |
| `validated-turnout` | `historical-campaigns` | Part III |
| `retirements` | `officeholder-age` | Part IV |

The first three predate this restructure. The last two are new: putting surveys
before elections means `validated-turnout` checks self-reported turnout against
a returns file the class meets two sessions later, and moving roll calls into
Part IV puts `officeholder-age` after the chapter that reads it. Neither is
load-bearing for a reader — both chapters state what the borrowed number is —
but a rebuild-from-scratch has to run them in dependency order rather than part
order.

## Placed by the part restructure — nothing is now unassigned

Every chapter that had no session has one, because the parts assign them rather
than the calendar. Three that were open in the previous version of this file:

- **`turnout-denominator/`** → **Session 11**, Part III. It sits with `eavs` and
  `residual-votes` under the argument that all three are records the election
  system keeps about its own operation. It also reads `disenfranchisement`
  directly and cross-references it twice, and that chapter is now one session
  earlier rather than in a different part.
- **`bellwether/`** → **Session 8** *(Tue)*, Part III, where it argues with
  `midterm-loss` from Session 7: one rule is certain about the sign and useless
  about the size, the other looks predictive and is not a rule at all.
- **`sweet-spot/`** → **Session 14**, Part VI, beside `vote-dilution` — both ask
  how many majority-Black districts there are and get incompatible answers from
  one file.

`gotv/` remains **out**, and is now deliberately unassigned in `PARTS` so the
decision stays visible in `INDEX.md` rather than being absorbed into a part.
`campaign-visits/` is back **in**, as a Part IV brief in Session 9 — a wire
service tracker is data nobody was compelled to file and somebody chose to
publish, which is the commercial half of that part's argument.

## To build — nothing outstanding

**`perception-gap/` is built** (29 Aug 2026) and every directory under `labs/`
is now a chapter. It had been a folder holding a survey-item sheet, a benchmark
table with no script behind it, and no brief. The benchmarks are now regenerated
from the Ahler and Sood replication archive on Harvard Dataverse, the item sheet
is printed in the brief itself rather than sitting loose beside it, and the
chapter closes Part II's critique block.

The acquisition questions this section used to hold are all closed:

| Needed by | What | Source verdict |
|---|---|---|
| — | **House results through 2024** | ✅ **Done, no manual step.** `parse-clerk.py` downloads and parses the Clerk of the House's official *Statistics of the Presidential and Congressional Election* for 2004–2024; `fetch-pres-by-cd.R` pulls The Downballot's presidential-by-district figures; `extend-clerk.R` splices. All keyless. **The series is now 1946–2024, 40 elections, 435 districts a year.** Validated against Jacobson on the 2004–2014 overlap: median difference **0.03 points**, **8 of 2,260** districts off by more than 2, none of them a parsing error. 2024 split districts come out at **16**, matching the published count. The MEDSL guestbook route is retained in `extend-to-2024.R` as an unused alternative. |
| — | **Precinct returns and geography** | ✅ **Done, VEST dropped.** `parse-ga-sos.py` reads the Georgia SoS's own publication — county tabulation exports, all 159 counties, **precinct × candidate × vote method**, which VEST does not carry. The 2020/2024 archives are one browser download each (Cloudflare); the 2024 JSON fetches itself. Precinct **shapefiles** (7 vintages, 2012–2024) and census blocks are fully script-fetchable. `precinct-geography` is built on top: blocks → precincts by interior point, population-weighted crosswalk, **22 votes lost of 13.8 million**. |

**Pattern worth noting.** No guestbooks left, down from three. The House series
was unblocked by going *upstream* — past the tidy academic compilation behind
the guestbook, to the official document it was compiled from. That is worth
saying to the class: **when the convenient copy is gated, the primary source
often is not.** The `lobbying` and `disenfranchisement` labs hit the same wall in
different forms (a 25-record rate limit, and a PDF with no machine-readable
version), and the PDF was solved the same way this one was.


### On scraping Wikipedia for House results

Asked and answered: **no, use the Clerk of the House.** Wikipedia has these
results and they are largely accurate, but it is *downstream of the Clerk* —
its House election articles cite exactly the documents `parse-clerk.py` now
reads. Scraping the encyclopaedia to reach a number the primary source
publishes directly adds a transcription step, removes the ability to say where
the number came from, and produces a dataset that changes when somebody edits a
page. Its article structure also varies by year and state, so a scraper is a
maintenance commitment rather than a script.

Wikipedia remains genuinely useful for **checking a surprising row by hand**,
and that use needs no scraper. (The eleven outliers in the validation were found
by comparing the two sources against each other, not by consulting it.)

### Chasing the outliers found three real parsing bugs

The eleven districts that disagreed with Jacobson were not noise, and running
them down changed the data. All three fixes are in `parse-clerk.py`:

1. **Footnote markers were being read as vote totals.** The Clerk prints
   footnote references as superscripts; `pdftotext` drops them to the end of the
   candidate's line and pushes the real total onto its own line. **Ralph Abraham
   was recorded with 2 votes instead of 134,616.** 24 rows across five years.
2. **Fusion lines listing several parties were unattributed.** New York prints
   cross-endorsements under a candidate with no name, often as a list
   ("Conservative, Libertarian"). Matching one party name missed those. Fixing
   it resolved all four New York outliers exactly.
3. **Unopposed candidates with no printed total were dropped.** Florida keeps
   unopposed candidates off the ballot, so the PDF shows `(1)`. Those districts
   vanished — **undercounting uncontested races, the headline measure, in the
   wrong direction.**

**Then every remaining disagreement above a point was run down, and they did not
resolve the same way.** Two more were ours: "Constitution" was missing from the
fusion list (SC-06 2004, now exact), and Louisiana was taking the leading
candidate of each party where the Clerk's own Recapitulation *sums* them — the
sum matches Jacobson at LA-01 2014 (19.57 vs 19.6).

**Two are Jacobson's errors.** NE-03 in 2004 and UT-01 in 2006: the candidate
blocks *and* the Recapitulation table both give our figures (10.78 and 33.98)
against his 8.6 and 32.1. Two independent presentations inside the official
document agree with each other and disagree with him. We keep ours.

**Five Connecticut districts in 2008 are also his**, arguably: he counts fusion
ballot lines in New York *and South Carolina* but not in Connecticut. We count
them everywhere, which is what the Recapitulation does.

**Louisiana is nobody's error and is flagged rather than fixed.** An open primary
plus December runoffs means the Clerk substitutes runoff totals for the two
finalists while leaving eliminated candidates' November numbers in place — one
row, two elections. An all-Democratic runoff (LA-02 2006) makes a two-party
share meaningless outright. Every Louisiana row carries `la_primary` and
`runoff_mixed`. Texas 22 and 23 in 2006 are the same class: a write-in campaign
and a court-ordered runoff.

**Result: 2020 and 2024 now both give exactly 16 split districts, matching the
published counts.** 2016 gives 38 against 35.

### ⚠ Corrected: the split-district series was measuring the wrong thing

The Jacobson file has `dv` (Democratic House vote), **`dvp` (previous election's
House vote)** and **`dpres` (district presidential vote)**, and ships with empty
Stata variable labels. The build read `dvp` as "presidential" and computed split
districts as `(dv > 50) != (dvp > 50)` — which is **a seat changing party**, not
a split district — then blamed the file's own `split` column for disagreeing.

Fixed. Three checks establish it: `dvp` tracks the previous election's `dv` at
**r = 0.968** (mean gap 0.72 points, 12,226 rows); recomputed with `dpres` the
flag matches the file's `split` column **100%** of the time; and `dpres` for
AL-01 in 2012 is **37.70** against The Downballot's independent **37.70161**.

**The corrected series is a much stronger finding.** Split districts peak at
**54.8% in 1974** — Watergate, with Democrats winning House seats all over
Nixon's 1972 map — stay above 40% through the Reagan landslides (43.7% in 1984,
45.3% in 1986), and collapse to **6.0% in 2012**, exactly the published 26 of
435. The old series showed a flat wander between 2.5% and 23%. Defined
1952–2014.

Also added: `split_coverage`, because presidential-by-district figures do not
exist for **1946, 1948, 1950, 1962 and 1966** — no government publishes them and
nobody calculated the 1940s. Those years are now masked rather than reported as
100%. The key has a section on the column-name trap; the lab's Option B makes
students walk into it.

## Briefs — the student-facing front document

**All 95 chapters now carry a `*-brief.Rmd`** — the form has gone from an
experiment on nine labs to the corpus default and then to universal, and the
table below lists the ten that established it. **The brief is a
walkthrough, not a summary.** It goes from one record to a conclusion in ten
numbered steps, showing a little more at each one, and several of the steps are
about why the obvious answer is wrong. The final number arrives at Step 7 or
later, after the reader has been shown what would have to be true for it to mean
anything.

Each knits to **HTML** (interactive D3) and **PDF** (base-R static equivalents)
from one file, switching on `knitr::is_html_output()`. Every figure is computed
from the committed data; every citation was verified against the published
record before use.

| Brief | pp | The turn | Interactive figure |
|---|---|---|---|
| `policing/` | 8 | Step 4: the denominator cannot exist, so change the question | Bubble scatter + 10-year hit-rate lines |
| `lobbying/` | 8 | Step 4: one dollar figure, 17 issues, no division | Ranked bars animating issues ⇄ institutions |
| `jury-selection/` | 7 | Step 5: the control group is already in the file | Mirror bars + 211-trial *p*-value scatter |
| `house-competition/` | 7 | Step 6: a flat national line is two opposite trends | 5 series, crosshair readout, click to hide |
| `gss-confidence/` | 7 | Step 6: eleven fell, two did not | 13-line chart, hover-isolate + legend toggle |
| `redlining/` | 7 | Step 5: the pooled comparison is the wrong unit | City slope chart A→D, reversals in blue |
| `disenfranchisement/` | 7 | Step 5: one column is 39.9% and exists in 10 states | Tile cartogram with post-sentence readout |
| `residual-votes/` | 6 | Step 5: the measure has a floor, so use it as a test | Rate histogram, hover + median marker |
| `census-geography/` | 6 | Step 5: two tests could not be run at all | Nesting diagram with crosscutting overlay |
| `precinct-geography/` | 7 | Step 6: two methods disagree 78.3% vs 18.6% | Areal vs population weight scatter, agreement diagonal |

**The six added in August**, in the same form. The four marked **(Tue)** are the
Tuesday companions, and for those the brief *is* the assigned reading:

| Brief | pp | The turn |
|---|---|---|
| `turnout-denominator/` | 12 | Step 4: the same ballots, two denominators, and the decline is −0.006 points per election |
| `migration/` | 11 | Step 5: apply the published margin and 35% of the map disappears — carrying 2.21% of the people |
| `vote-dilution/` | 13 | Step 7: the map the algorithm could not find has been enacted law for years |
| `areal-units/` **(Tue)** | 11 | Step 6: 360 race-blind partitions of the same blocks, 0 to 4 majority-Black units |
| `regional-shift/` **(Tue)** | 11 | Step 5: six unremarkable decades added together come to 75 seats |
| `officeholder-age/` **(Tue)** | 11 | Step 4: change one row of the life table and the conclusion reverses |
| `levels-of-aggregation/` **(Tue)** | 9 | Step 6: a method used in federal voting-rights litigation returns 194% |

**The design rule:** the brief is what a student reads to understand the issue;
the lab is what they run to see where the number came from. Neither asks them to
write code.

**All three files now share one numbering.** Brief, lab and key run the same
steps — the brief in prose with D3, the lab with the code exposed, the key with
a `Walking the steps` table giving minutes and the pivot for each. A student who
read Step 6 of the brief and a student running Step 6 of the lab are looking at
the same thing. All 18 files re-knit.

**Anchoring literature** (all verified): Uggen & Manza (2002) · Citrin & Stoker
(2018) · Mayhew (1974) · Knowles, Persico & Todd (2001) and Pierson et al.
(2020) · *Batson* (1986) and *Flowers* (2019) · Aaronson, Hartley & Mazumder
(2021) and Rothstein (2017) · Blanes i Vidal, Draca & Fons-Rosen (2012) and
Baumgartner et al. (2009) · Ansolabehere & Stewart · Openshaw (1984) and
Robinson (1950).

## Retired

| Lab | Why |
|---|---|
| `gotv` | Cut. A findings table from a literature, not a dataset anyone goes and gets — the least data-source-ish lab in the course. Unassigned in `PARTS`, so `INDEX.md` shows it out of the book rather than silently omitting it. |
| ~~`campaign-visits`~~ | **Reinstated** by the part restructure. It is a wire-service tracker — data nobody was compelled to file and somebody chose to publish — which is exactly Part V's second half. Brief-only reading in Session 9, with `finance-network` now following it. |

`gotv` still knits and is on disk if wanted back.

---

## Lab cross-references — repointed to chapter names ✔

**Done, and this time it should not need doing again.** Every reference in the
live corpus now names a **chapter**, so no calendar change can invalidate one.

**The scope was smaller and different from what this file previously claimed.**
The "~130 references across 31 instructor keys" are **in `_archive/`**, which is
frozen — those keys are not part of the book. In the live corpus the count was
**47**, all of them in `data/build-*.R` and `build-*.py` headers and comments,
and **not one live brief contained a session reference at all.** The briefs were
already citing each other by name and link, which is the convention this pass
adopted everywhere else.

- **Three numbering schemes were in circulation simultaneously** — `Week N`,
  `Session N` and `Data Session N` — and they disagreed. The build scripts were
  still on the oldest of the three, predating the August pass entirely. So each
  reference was resolved by its *topic* and never by arithmetic on the old
  number.
- **29 were headers** (`# Build the Week 14b dataset…`) where the number carried
  nothing the folder name did not. They now name the chapter.
- **18 were real cross-references**, now pointing at a chapter: `Week 14's BISG
  lab` → `` `bisg-check` ``, `as Week 2 explained` → `` `data-sources` ``,
  `Session 8 will show` → `` `validated-turnout` ``, and so on.
- **`_archive/` now carries a README** stating that it is frozen, that its week
  and session numbers are historical, and that two folder names changed
  *meaning* rather than merely moving — so a naive path fix inside an archived
  file would point at the wrong chapter.

**Convention going forward:** refer to **the part and the chapter**, not the
session number and not the week — "Part I, `surnames`" rather than "Session 3."
The parts are stable because they are a claim about the instrument. Session
numbers moved **twice during this restructure alone**, which is the argument.

## The survey chapters — renamed ✔

The `*-source` suffix marks a part opener, and two chapters contradicted it.
`surveys/` was the surveys-part opener without the suffix, while `surveys-source/` was
a case chapter wearing it, and the two sat adjacent in the index. A third file in
a third folder was also called `surveys-brief.Rmd`.

| Was | Is | Why |
|---|---|---|
| `surveys/` | **`surveys-source/`** | it is the surveys-part opener (Part V then, Part III now) — *What a Survey Can Establish That No Record Can* |
| `surveys-source/` | **`validated-turnout/`** | a case chapter — counted ballots against what people said they did |
| `poll-simulation/surveys-brief.Rmd` | `poll-simulation/poll-simulation-brief.Rmd` | third file of that name, in a folder not about surveys |
| `ga-precinct-returns/precincts-brief.Rmd` | `ga-precinct-returns/ga-precinct-returns-brief.Rmd` | stem now matches its folder |

Every inbound link was repointed **by meaning, not by string** — the old slug
`surveys-source` named the chapter that is now `validated-turnout`, so a blind
find-and-replace would have inverted six links. The four briefs whose text
changed were re-knit, and every `../`-style cross-chapter link in all 78 briefs
was checked to resolve on disk.

**Every chapter ending in `-source` is a source chapter**, and no other chapter
does. There are now **nine**, not six: the six that open a part, plus
`clerk-source`, `rosters-source` and `admin-records-source`, which open an
instrument family inside a part. `SOURCE_CHAPTERS` in `make-index.py` stays an
explicit set anyway, so a future `foo-source` that is really a case chapter is a
decision rather than an accident. The part *opener* is a separate thing and is
named `part-N-*`.

---

## What the course now covers

Grouped by part, which is now the same grouping the book uses.

### Sources of data

| Source | Part | Session |
|---|---|---|
| Decennial census, PL 94-171 | I | 1 |
| data.census.gov, the Census API, and the bulk directory | I | 1 |
| Population Estimates Program, and what "controlled" means | I | 1 |
| TIGER/Line, legal boundaries vs cartographic ones | I | 1 |
| Census geography, block equivalency | I | 2 |
| American Community Survey, with margins | I | 3 |
| ACS state-to-state migration flows; CPS mobility back to 1948 | I | 3 |
| Decennial apportionment time series, 1910–2020 | I | 3 |
| Census surname list | I | 3 |
| Census first- and last-name tabulations, 2020 (released April 2026) | I | 3 |
| Certified election returns, county and state | III | 6 |
| The Clerk of the House's *Statistics of the Election*, as a typeset document | III | 7 |
| Officeholder rosters, 1789– (Congress, and how a seat is actually vacated) | III | 7 |
| State canvasses, all 51 jurisdictions, as each officer publishes them | III | 6 |
| A GitHub compilation, and what it costs to check one | III | 6 |
| Historical returns, 1868–present | III | 6 |
| Precinct-level returns | III | 8 |
| Cast vote records / RCV ballots | III | 8 |
| Published race ratings, committed to before the fact | III | 10 |
| Live returns on the night | III | 10 ⚑ |
| Voter registration files and vote history | III | 11 |
| State voter-list access pages, all 51, scanned | III | 11 |
| Election administration (EAVS) | III | 11 |
| Administrative records as numerators: court, police and HOLC | V | 12 |
| FEC filings, bulk and itemized | IV | 9 |
| FEC independent expenditures | IV | 9 |
| Lobbying disclosures (LDA) | IV | 9 |
| Wikipedia pageviews | IV | 9 |
| Wire-service campaign tracker | IV | 9 *(brief)* |
| Shared-link counts: politicians against the outlets they post | IV | 9 |
| Social platform profile pages, read by hand on one day | IV | 9 |
| Long-running academic surveys (ANES, GSS, CES, CPS) | II | 4–5 |
| Public polls and aggregates | II | 5 |
| A respondent-level poll file, released by the pollster | II | 5 |
| Forecasting model output; prediction markets | II | 5 |
| Congressional roll calls (DW-NOMINATE) | IV | 12 |
| Supreme Court votes, 1946– (SCDB) | IV | 12 |
| Supreme Court oral argument, transcript aligned to audio (Oyez) | IV | 12 |
| Precinct returns + CVR as an answer key for inference | VI | 14 🔒 |
| Cross-national democracy indices (V-Dem) | — | wrap |

**The acquisition gap is closed.** `census-access` (Session 1) walks all four
routes — data.census.gov, the API, the bulk directory and TIGER/Line — and shows
what each one hands back for a single question. It does not teach the doors as a
how-to: the chapter's finding is that **the route decides the number**, and that
the one field which would warn you reads as missing data.

Three things it establishes that no other chapter does:

- **A keyless API data request now returns HTTP 302 with an empty body**,
  redirecting to a page titled *Missing Key*. Follow the redirect and you parse
  HTML as JSON; don't, and you record a success-shaped status and zero rows.
  `census-api` recorded a different failure mode for the same request in an
  earlier year — both were true, at different times.
- **`data.census.gov` cannot be scripted at all.** The URL of a table returns
  2,928 bytes of JavaScript application.
- **3,090 of 3,222 county rows in the ACS carry a margin of error of
  −555555555**, the code for *controlled* — the survey did not do the
  estimating. The counties that kept a real margin have a median population of
  1,489. Strip negative margins as missing, as every tutorial says to, and the
  file stops telling you.

### Methods and data literacy

| Skill | Session |
|---|---|
| What one row is; units of analysis | Aug 25, and every session after |
| Which rung of the ladder answers which question | Aug 25 *(Tue)*, 4, 6 |
| The modifiable areal unit problem: scale vs zoning | 2 *(Tue)* |
| Choosing a denominator, and how one reverses a conclusion | 3, 8, 13 *(Tue)* |
| Replicating a published finding from the published table | 8, `turnout-denominator` |
| Joins, FIPS codes, geography crosswalks | 2, 4, 6 |
| Seats vs people; "per what?" | 3, 5 |
| Margins of error on official estimates | 3 |
| Thresholds applied to estimated quantities | 3 |
| Sampling error vs bias | 11, 12 |
| Weighting, and what it cannot repair | 12 |
| Checking a survey against a public record | 7, 11 |
| A record written by its own subject | 9 |
| A score is a model, not a measurement | 13, 14 |
| Inference about individuals from aggregates | 3, 13, 14 |
| Grading a method against a known answer | 13, 14 |
| Base rates, and how hard they are to beat | 5, 6, 10 |
| Turning an estimate into a probability | 12 |
| Missing data as information | 8 |
| What a dataset cannot tell you | every lab has a section |

### Substantive topics (Tuesdays, from the textbook)

**The textbook is not read in the book's order, and it is not tied to the
parts.** Each chapter is placed next to the data session it explains. The
sequence below is the authority; `readme.Rmd` schedules it.

| Tue | Chapter | Placed there because |
|---|---|---|
| Sep 1 | **Ch. 1** | the four standards, before anything is judged against them |
| Sep 8 | **Ch. 2** + the Electoral College section of **Ch. 9** | Session 3 is apportionment and § 203 |
| Sep 15 | **Ch. 13** | Session 4 is what a survey can establish, and Ch. 13 is how voters decide — the thing no record holds |
| Sep 22 | **Ch. 6** | Session 5 measures party identification, which is Ch. 6's subject and a survey construct |
| Sep 29 | **Ch. 3** + **Ch. 11** | Session 6 is a century of returns; Ch. 11 lands before Session 8's precincts, because Session 8's own Tuesday is Test 1 |
| Oct 6 | **Ch. 5** + **Ch. 10** | Session 7 *is* both — `vote-targeting` is Ch. 5's worked example, `house-competition`/`careers`/`midterm-loss` is Ch. 10's argument |
| **Oct 20** | — | **Test 1: Ch. 1, 2, 13, 6** |
| Oct 27 | **Ch. 4** + **Ch. 7** | Session 9 is money, one week before the vote |
| Nov 3 *(no class)* | **Ch. 8** + **Ch. 9** in full | election week; Session 10 grades the forecast Sessions 5 and 9 built |
| **Nov 10** | — | **Test 2: Ch. 3, 11, 5, 10** |
| Nov 17 | **Ch. 12** | Sessions 11–12 are registration, turnout and the records institutions keep about people |
| Nov 24 | *none* | remote data session; break reading is the Session 14 brief |
| **Dec 3** | — | **Test 3: Ch. 4, 7, 8, 9, 12** |

**Every chapter is read once and tested once.** Two forward references are
knowingly accepted and flagged in the syllabus: Ch. 10 and Ch. 11 refer back to
Ch. 4 (read five weeks later), and several chapters refer to Ch. 6 (read in
November). Neither is load-bearing.

**Why not the book's order?** Because the money session moved to Oct 29. Reading
Ch. 4 in September and opening the FEC bulk files in late October is the one
pairing worth breaking the book's sequence to fix, and once that moves the rest
follows: Ch. 10 wants the careers session, Ch. 12 wants the voter file, Ch. 13
wants the polls, Ch. 6 wants DW-NOMINATE. Nine of the twelve teaching Tuesdays
now sit beside the session they explain, against three under book order.

**Two chapters swapped when the parts reordered**, and they swapped with each
other: **Ch. 13 (Voter Choice)** moved up to Oct 6 to meet the surveys session,
and **Ch. 12 (Voter Participation)** moved back to Nov 17 to meet the voter file
and turnout. The tests follow them — Test 2 takes 13, Test 3 takes 12 — so
**every chapter is still read once and tested once**, and no test covers a
chapter read after it.

> **Still to check.** The forward-reference audit was done against the old
> pairing. Ch. 13 is now read six weeks earlier, so whether it leans on Ch. 12
> (now read after it) needs one pass through `textbook-chapter-notes.md` before
> the syllabus is final. The two previously-flagged forward references — Ch. 10
> and 11 onto Ch. 4, and several chapters onto Ch. 6 — are unchanged.

Plus, from outside the book: census classification and race, the Voting Rights
Act after *Callais*, language access, ballot initiatives and mid-decade
redistricting, and ideology measurement.

---

## Security note, carried over and now more urgent

A live **Census API key in plain text** sits in the F25 handout at
`F25/Assignments/labs/surnames/surnames.md`, which was distributed to students.
It should be revoked at <https://api.census.gov/data/key_signup.html>.

**Session 1 now teaches the API** — it moved a week earlier when Part I opened
the semester, so the decision is due **before Aug 27, not Sep 3**. The course
needs a key policy regardless:
either students each register their own (free, instant) or the lab uses the
keyless demo endpoints. Decide before Sep 3.
