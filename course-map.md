# Course map — the fourteen data sessions, F26

Working reference for teaching and maintaining the book. **`labs/_lib/make-index.py`
is the authority** for what the book contains and in what order: its `SECTIONS`
structure generates `labs/INDEX.md`, and the generator fails if the structure and
the filesystem disagree in either direction. **This file schedules what that one
lists** — it assigns no content and invents no order of its own.

**All 112 docs are scheduled.** One is read for the Tuesday introduction; the
other 111 are spread across the fourteen data sessions below. Nothing is
deliberately out.

---

## What changed — 31 August 2026

The book was restructured from **six provenance parts** to **four sections built
on kinds of data**. Every claim in the previous version of this file that named a
part, a part opener, a beat map or a session number was invalidated by that pass,
so this file was rewritten rather than patched. What a reader needs to know:

- **Four sections, not six parts.** I. Data About the Population · II. Survey
  Data · III. Administrative Data · IV. Putting Data Together. Each section is
  still a claim about provenance, but the unit inside a section is now a
  **cluster** — one data-type chapter and the briefs that use that data.
- **Two document types, and the difference is pedagogically load-bearing.** A
  **chapter** is a reading about a kind of data (2,500–4,000 words, median 2,728);
  a **brief** is a lab that uses it (1,500–2,500 words, median 2,377). The Type
  column in `INDEX.md` says which each doc is.
- **`gotv` is back in the schedule**, in Section III's Voter Files cluster
  (Session 11). It is no longer unassigned.
- **Two merges.** `clerk-source` folded into `house-competition`;
  `mid-decade-florida` folded into `mid-decade`. Both source directories are in
  `labs/_archive/` and neither is scheduled. `parse-clerk.py` and the Clerk
  parsing forensics now live in `house-competition/data/`.
- **Fourteen docs changed section.** `age-structure` to II; `names` to III;
  `surnames`, `validated-turnout`, `models-markets`, `seat-forecast`,
  `senate-2026`, `election-night`, `neutral-maps`, `mid-decade`,
  `whole-foods-cracker-barrel`, `turnout-denominator`, `data-sources` and
  `rank-size` to IV.
- **Everything is built.** All 112 docs render to HTML and PDF, and all 112 pass
  the third-edition template test. The old ★ / ✦ / ✚ build marks described a
  corpus that no longer exists and are gone.
- **The lab/key pair is gone.** A doc is now one `<slug>-brief.Rmd`, one
  `<slug>-code.R`, and a `data/` directory. There is no separate lab file and no
  instructor key on disk, so the old "brief, lab and key share one numbering"
  convention has no subject.

The six `labs/NN-*/` directories persist from the old layout and no longer
correspond to sections — Section III spans `03-elections`, `04-political-actors`
and `05-ordinary-people`. `make-index.py` names no directory; it discovers each
slug's folder. Do not read a directory name as a section.

---

## The spine

**Thursday is one kind of data.** Tuesday is the textbook and the argument; the
three tests cover the textbook and nothing else.

**The order is cumulative: nothing is inferred from a source students have not
already met.** That rule, not taste, sets the section order:

- **Section I depends on nothing**, and it is the only section that doesn't.
  Everything downstream borrows a denominator from it — **17 of the 93 docs
  outside Section I read Census data of their own.** So it anchors the early
  sessions.
- **Section II follows**, because the sample-versus-enumeration contrast is the
  bridge. Section I has already taught a survey — the ACS — and filed it under
  counting, because answering the ACS is compelled by 13 U.S.C. § 221 and
  answering the ANES is not. Teaching the two a session apart is what turns that
  from a filing quirk into the lesson.
- **Section III is the largest block**, six of the fourteen sessions. It is
  half the book.
- **Section IV comes last** because it combines what the other three taught. It
  introduces no data at all: every file it touches was met earlier.

**Two sessions meet on a Tuesday** (13 and 14), and one runs as self-study across
a Tuesday and a Thursday (6). The calendar, not the book, decides that.

## The four sections

| Section | Clusters | Chapters | Briefs | Intro | Docs | Sessions |
|---|---:|---:|---:|---:|---:|---|
| Front matter | — | 0 | 0 | 1 | 1 | Aug 25 |
| **I** Data About the Population | 5 | 5 | 13 | 1 | 19 | 1–3 |
| **II** Survey Data | 4 | 3 | 13 | 1 | 17 | 4–5 |
| **III** Administrative Data | 7 | 6 | 48 | 1 | 55 | 6–9, 11, 12 |
| **IV** Putting Data Together | 6 | 1 | 18 | 1 | 20 | 10 ⚑, 13, 14 |
| **Total** | **22** | **15** | **92** | **5** | **112** | |

Counted from `labs/INDEX.md`, which `make-index.py` regenerates from the doc
files. Section III's 55 docs are 49.1% of the book, which is why it holds six
Thursdays. Section IV holds three, but one of them (Session 10) is fixed by the
election calendar rather than by the book — see below.

**The clusters, in reading order.** A cluster is one data-type chapter (sometimes
zero, occasionally two) followed by the briefs that use that data.

- **I** — The Census and Its Products · The Decennial Census · The American
  Community Survey · Population Estimates · Census Geography
- **II** — What a Survey Can Establish · Weighting · Political Polls ·
  Public Opinion
- **III** — Election Returns · Rosters of Officeholders · Voter Files ·
  Roll Calls and Courts · Campaign Finance · Media and Attention ·
  Records of Ordinary People
- **IV** — Joining Files · Survey Meets Record · Race Inference · Districts ·
  Forecasting · Proxies and Indices

**Legend.** **Bold** = a data-type chapter · plain = a brief · *italic* = a
section intro · **(Tue)** = read on students' own time and taken up in the
Tuesday slot · ⚑ = date fixed by the world, not by us · 🔒 = protected, not cut.

---

## The sessions

| # | Date | Wk | Sec | Session | Docs | Finding |
|---|---|---|---|---|---|---|
| — | Tue Aug 25 | 1 | — | *Intro: what this book is for* | *`introduction`* · **CES + census replication surveys taken in class** | What data exists about American democracy, and what it says about the country |
| **1** | Thu Aug 27 | 1 | I | **What a census is, and how you get at one** | *`part-1-census-bureau`* · **`census-source`** · `census-access` · `census-api` · `census-coverage` · `census-race` | An enumeration resolves where no sample can: the 2020 census breaks Georgia into **232,717 blocks**, and 2,457 of them hold exactly one person. But "the census says" almost always means one of three instruments — a count, a survey with a margin, or arithmetic carried forward — and they answer different questions. One county's population is a shelf, not a number: **19 figures currently in print, spanning 27,233 people**, and 1 July 2021 alone has been published five times, 7,355 people apart. The only door a program can use is the only one requiring a key, and a gated request answers politely rather than failing. The count grades itself and mostly declines to say: the Bureau marks **14 of 51 states**, while undercounting Black residents **3.30%** and Hispanic residents **4.99%** against a **1.64% overcount** of non-Hispanic White residents. And race and Hispanic origin are two questions, so "how white is New Mexico?" has two correct answers, **51.0% and 36.5%** |
| **2** | Thu Sep 3 | 2 | I | **The count itself, and the arithmetic that carries it forward** | **`census-decennial`** · `apportionment` · `regional-shift` **(Tue)** · `demographics` · `overplotting` **(Tue)** · **`census-pep`** | The whole instrument is **seven questions and six tables** — 301 numbers per place, 288 of them from the race question alone. A one-line formula turns it into the House: the 2020 count moved **7 seats across 13 states**, and New York missed the last one by **89 people** while the next state in line was 11,462 short. Over sixty years **75 seats changed states**, 66 of them out of the Northeast and Midwest, with no vote taken anywhere. The same 331 million people score **60.4 on the mixing scale as one country and 43.1 as a set of tracts**. Then the third instrument: a population estimate is arithmetic, not measurement — an identity that closes to the person on all **3,144 counties**, saying **84.3% of 2024 growth was international migration** while **2,076 counties (66.0%)** buried more people than they delivered |
| **3** | Thu Sep 10 | 3 | I | **The survey inside the Bureau, and the geography everything is published on** | **`census-acs`** · `migration` · `chord` **(Tue)** · `section-203` · **`census-geography`** · `areal-units` **(Tue)** · `zip-codes` | The ACS is a survey that lives in the counting section because answering it is compelled by the same statute as the count. Pooling reaches small places: the one-year file covers **854 counties and the five-year all 3,222**, and for **2,368 counties the five-year window is the only window there is** — with margins running ±1.9% in the largest counties and **±20.8% in the smallest**, widest exactly where the five-year file is all there is. **35% of 2,652 migration pairs fail their own margins** while carrying **2.21%** of the movers. A civil right switches on at an estimate: **59 of 391 language determinations** turn on where inside its own margin a number landed. Then the boxes themselves — geography has a spine and three systems that cut across it, and rotating shapes at fixed scale moves the answer from **0 to 4 majority-Black units** across 360 race-blind plans of the same Fulton blocks. A ZIP is a routing key: **7,846 of 41,488 codes have no area at all** |
| **4** | Thu Sep 17 | 4 | II | **What a survey can establish, and the dial that decides what it says** | *`part-2-surveys`* · **`surveys-source`** · `survey-access` · `age-structure` · **`ces-class`** · `ces-states` · `poll-weighting` · `poll-simulation` | Only a survey reaches opinion, which no record anywhere contains — and precision improves with the square root of n, so the whole ANES resolves ±1.3 points and its median state **±19.2**. Of six free public archives, **two answered a program and four put a bot wall in front of it**, two of those with a success code attached to a challenge page. A weight is the method that makes a sample representative: the CES adjustment moves registration **19.0 points** and the partisan margin from **D+13.5 collected to D+1.9 published**. Weighting accounted for **8.5 points of margin** on 720 people who never changed their answers, and the choice of which variables to correct **spans 9.7 points and crosses zero**. A 60,000-person survey is **68 Vermonters**, a **2.78** design effect, and a sample size that predicts the margin of error at −0.75 and the actual error at only **−0.25**. Past about **464 respondents a biased poll's own interval never contains the truth again** |
| **5** | Thu Sep 24 | 5 | II | **What the long surveys measured** | **`anes`** · `party-id` · `gender-gap` · `ideology` · `partisan-economy` · `abortion-opinion` · `gss-confidence` · `thermometers` · `perception-gap` | The cumulative file is a reconciliation, not a pile of surveys: **73,745 interviews** recoded so one column means one thing across seventy-six years, with non-answer codes living **inside** the data columns at **20.80%** of one variable's values. The share of independents in 2024 is **6.9% or 33.0%** on one coding decision headline numbers never state. Both sexes moved away from the Democrats — **men 34.9 points and women 22.2** — and a difference has no memory of its halves. The ideology scale has **two middles**, and they are different people. The partisan gap on the economy has matched the president's party in **18 of 18** studies, at **54.2 points** in 2024 and 40.6 the other way in 2020. Eleven of thirteen institutions lost confidence, steepest in **medicine at 28.6 points**, not Congress. Affective polarization is **49.5 degrees**, and **98.1% of the movement is the out-party half**. And every one of eight groups is overestimated by **19.8 points**, on guesses of which **61.1% end in a zero** |
| **6** | Sep 29 / Oct 1 | 6 | III | **What a return is, and where you get one** *(self-study)* | *`admin-records-source`* · **`returns-source`** · **`levels-of-aggregation`** · `county-returns` · `panhandle-claim` · `electoral-map` · `mapping` · `wind-map` · `sparklines` **(Tue)** · `historical-campaigns` · `distributions` **(Tue)** | Complete, official, and permanently silent about who did it. The ladder runs ballot → precinct → county → state, and each step up is irreversible: Georgia's Democratic share is **48.90% at every rung** while **30.1% of the precinct variation vanishes** by the county rung, and ecological regression puts mail voters at **194.0% Democratic against a counted 64.5%** — *worse* from 3,144 counties than from 177,708 precincts. **No national agency publishes American returns**: the record is 51 publications in 11 formats, and two private assemblies of one election agree on the Democratic vote in all but **42 of 3,138** counties while disagreeing about **how many people voted in 1,588**. A county choropleth encodes land — Trump's counties are **85.7% of the area and 51.1% of the votes** — and **161 of 3,109 counties cast half the ballots**. **88.5% of counties moved right** 2020→2024, median R+3.1 against a national R+6.0. No winner has reached ten points in **10 straight elections**, the longest run in the record |
| **7** | Thu Oct 8 | 7 | III | **The bottom rungs: precincts, ballots and administration** | `crossover` · `nationalization` · `bellwether` **(Tue)** · `vote-targeting` · `ga-precinct-returns` · `precinct-geography` · `cast-vote-records` · `residual-votes` · `eavs` | Counties persist at **0.9950** between 2020 and 2024, and all **86** that switched switched the same way. Split House districts fell **40.3% in 1984 to 4.0% in 2024**, and swapping in today's candidates alone takes 1984 to 4.6%. Across 3,081 counties and 17 elections **exactly 0** have a perfect bellwether record, against 0.02 from coin-flipping. Ranking counties by contribution and by percentage gives top-five lists **sharing 0 counties**. Georgia publishes **159 county exports, not one state file**, and the four vote methods were different electorates — **38.5% to 65.3%** Democratic in a state decided by 12,670 votes. Only **84.5% of precinct names survive** 2020→2024, and land-vs-people weighting disagrees by ten points on 9.5% of pairs. A CVR is the only American election data where a row is a ballot: **340,778 ballots collapse to 926 patterns** and **65.2%** used one of four rankings. Residual votes have a floor, and **103 of 2,993 counties fall below it**. And **584,463 mail ballots rejected**, Oklahoma **4.35%** against Vermont **0.20%** |
| **8** | Thu Oct 22 | 9 | III | **Rosters, careers and nominations** | **`rosters-source`** · `house-competition` · `midterm-loss` · `retirements` · `primary-defeats` · `primary-positions` · `nomination-anchors` · `nomination-rules` · `careers` | A roster is not a return. **40,432 people have served in 38,088 House seat-terms**, and over the ten most recent Congresses **64.2% of departures were decided by no voter at all**. In **14.8% of 17,403** district-elections since 1946 the voter had no choice, and the flat national line is two regions moving in opposite directions. The president's party lost seats in **36 of 41** midterms (88%), median −28, but only **10 of 41** land within ten seats of that median. Between 2004 and 2022 **790 members left the House and voters removed 283**. A sitting member seeking renomination is refused about **1.5%** of the time, and the spikes are a redistricting effect. Of **1,662** primary candidates the median stated a position on **6 of 14** issues, from 29.3% silent on the ACA to **84.5% on Benghazi**. **61 of 173** runoffs reversed the first round, and **593 nominations** were settled outright under half the vote. The median career is **3 Congresses**, and its apparent collapse after the 1980s is a censoring artifact |
| **9** | Thu Oct 29 | 10 | III | **Money and attention** *(out of book order — see below)* | **`finance-source`** · `campaign-finance` · `pie-radar` **(Tue)** · `independent-expenditures` · `lobbying` · `finance-network` · `media-attention` · `media-ideology` · `streamgraph` **(Tue)** · `campaign-visits` **(Tue)** · `follower-counts` | The only source in this book written by the people it is about: the subject writes the record and the FEC publishes it unverified. **986 of 3,856** filed candidates raised nothing, so the mean runs **83.7 times the median**. Individuals supply most of every kind of campaign's money, and PAC money is allocated by office held rather than by contest — **37.6%** of a serious incumbent's receipts against **1.4%** of an open-seat candidate's. **16 filings out of 73,449** inflate 2024 outside spending from **$4.75bn to $49.67bn** and move the file from **94.5% supportive to 57.6% attacking**; the dangerous error is the plausible one. **47.4%** of paid lobbying filings cover several issues with one undivided dollar figure. Then attention: half a year's reading about one candidate arrived in **18 days**, **112 outlets** are placed left-to-right on shared-link counts alone (recovering roll-call positions at **0.94**), **seven states took 83.8% of 377 logged stops**, and the ten largest accounts hold **43.3%** of all followers |
| **10** | Thu Nov 5 | 11 | IV ⚑ | **Live returns** *(fixed date)* | `models-markets` · `seat-forecast` · `senate-2026` · **`election-night`** · *revisit* `midterm-loss` | A single probability cannot be refuted by a single outcome, so grading a forecaster takes many claims with answers attached — **746** of them, Brier **0.0644** against 0.1944 for answering "no" to everything. A forecast claims a distribution: **244 seats in the middle, 218–275 at the 80% interval**, and switching off the national error collapses that interval from 44 seats to **11**. Averaging expert ratings means inventing a number line for words nobody attached numbers to. Then the night itself: returns arrive in an order that is not random, so the defence is a baseline written down in advance. A rule with no candidates in it called **418 of 433** districts (**96.5%**) on one two-year-old input — right in **all 357** districts beyond four points of an even presidential split, and **all 15 misses within 3.7 points** of one. Trustworthy where nothing is at stake, worthless where the drama is |
| **11** | Thu Nov 12 | 12 | III | **The machinery behind the result** | **`voter-files-source`** · `voter-file-access` · `voter-files` · `false-matches` · `disenfranchisement` · `gotv` | A voter file is machinery for running an election, and every research use of it is a borrowing — but only a record can check a survey: **83.2%** of ANES respondents said they voted in 2024, against **61.4%** of one county's registrants recorded as voting. **21 of 53 columns** exist to hand one person the right ballot, **70.0%** of the file carries no party signal, and **13,803 of 2020's voters (18.6%)** are already gone from the 2026 file. Public by law in all 51 jurisdictions and downloadable in **1**. Coincidences count pairs: matching on birth year instead of full date multiplies collisions by **124**, and where ground truth existed **99.5% of apparent double votes were two different people**. **4,049,978 adults** are barred by a felony conviction and nobody in government counts them — **39.9% have completed their sentences**. And an effect is a difference between randomized groups: **7 of 13 GOTV tactics show no measurable effect**, and effectiveness and cost rank the tactics at a rank correlation of **−1** |
| **12** | Thu Nov 19 | 13 | III | **Roll calls, courts, and the people who never volunteered** | **`rollcalls-source`** · `dw-nominate` · `scdb` · `oral-argument` · `officeholder-age` **(Tue)** · `policing` · `jury-selection` · `redlining` · `names` | The only source where named individuals' behaviour is recorded completely, by law, over two centuries — and still a sample, because the Constitution requires a recorded vote only on demand. **69.0% of 113,512 recorded votes carry no note of what was being voted on.** House party medians ran **0.584 in 1967 to 0.925 today**; the Senate went 0.587 to 0.917 **in a chamber whose boundaries nobody draws**. A left–right ordering of the Court falls out of 44 agreement rates carrying no ideological information, at **0.975**. Six reasonable ways to ask who talks the most give **3 different answers**. Then the mirror — the same kind of institutional record about people who never ran for anything. A rate needs a bottom number and for traffic stops it does not exist: the same **905,070 stops** give **0.86 or 1.01** times the white rate depending on the denominator. The state struck **49.8% of eligible Black jurors against 11.2% of white ones**, and the defense struck the reverse from the same panels. Within a city, D-graded tracts are **14.5 points more Black today**; the pooled national comparison understates it. And a name is asked to identify a person, stand in for sex and stand in for race, while being a distribution: **97.51% right and 7,042,013 people wrong** |
| **13** | Tue Nov 24 *(remote)* | 14 | IV | **Joining files, and the numbers built out of them** | *`part-6-putting-data-together`* · **`data-sources`** · `validated-turnout` · `surnames` · `bisg-check` · `uncertainty` · `turnout-denominator` · `cost-of-voting` · `whole-foods-cracker-barrel` · `rank-size` **(Tue)** | Nobody collected any of these. A join fails three ways, and only one of them — a key that changed meaning — escapes every row count; **328 of 3,160** county codes lose a leading zero on a naive read, and they are the alphabetically first states. Self-reported turnout has exceeded counted ballots in **15 of 16** presidential elections, by **8,642,778** in 2016, and the excess **fell from 10.1% to 3.4%** with no correction announced by anybody, which rules out any explanation that is a constant of human nature. A surname removes **44.6%** of the uncertainty about race and a first name 27.7%; BISG then scores **67.3% overall against a 51.5% free floor**, **90.6% for white voters and 40.7% for Black**, with errors running toward the locally largest group. A 95% interval answers one question, and the same estimate over ten neighbourhoods runs **3.1% to 91.6%** while the widest interval is 4.0 points. The same **156,766,239 ballots** give turnout of **59.2% or 64.3%** — and 1972–2000 falls 0.56 points per election on one denominator and **−0.006** on the other, so a literature was explaining a denominator. An index is a weighting rule wearing a number's clothing. And a famous proxy replicates, then gives **50 points in counties and 10.8 in votes** from one file |
| **14** | Tue Dec 1 | 15 | IV 🔒 | **Districts, and grading a score against a known truth** | `rpv` · `neutral-maps` · `mid-decade` · `redistricting` · `vote-dilution` · `sweet-spot` | **Goodman: 102.1% of Black voters at R² 0.982. Recorded truth: 92.7%** — 9.4 points off and past the ceiling of possibility, with bounds of **[70.1, 100.0]** that cannot be wrong and cannot decide anything. A map's shape is not evidence; its position in a distribution of maps is. Neutral ensembles produce **108.8 competitive districts against the enacted 79**, and the gap is concentrated — Texas runs **2 against a neutral 8.7**. Holding the voters still and moving only the lines, Texas's and Florida's redraws elect more Republicans in **every** election available: **2–5** extra seats across 17 Texas races, **1–4** across 13 Florida ones. A plan can be identified as biased from a printed column with no map consulted, and three statistics from that same column rank the states differently. Malapportionment is subtraction; the first *Gingles* precondition is not. And "how many majority-Black districts are there" has **six defensible answers from one file, 8 to 14**, because three population bases cross two definitions of who counts as Black |
| — | Thu Dec 3 | 15 | — | *Test 3, then the wrap* | **V-Dem** *(discussion)* | Where American election data sits against the cross-national indices |

Test dates: **Tue Oct 20** (Test 1), **Tue Nov 10** (Test 2), **Thu Dec 3**
(Test 3). See the textbook table below for coverage.

---

## Where the calendar overrides the book

Two sessions do not sit where the book would put them, and both exceptions are
about a real-world date rather than a teaching judgment.

**⚑ `election-night` runs Thursday 5 November regardless.** It is written to be
used against live 2026 returns, two days after the vote on Tuesday 3 November,
with the rehearsal flag flipped on the morning of the session. It is a
**Section IV** doc — a forecast is built by joining files nobody collected for
that purpose — so Session 10 is Section IV arriving three sessions early, in the
middle of Section III. That is the only date in the book that is not ours to
choose, and the session is better for it: `midterm-loss` was met in Session 8 and
is graded here against what actually happened. Its cluster mates come with it
(`models-markets`, `seat-forecast`, `senate-2026`), so the session is coherent
rather than a single orphaned doc.

**Money runs the week before the election, out of book order.** Campaign Finance
is Section III's fifth cluster and Media and Attention its sixth, which would put
both in Session 12. They run in **Session 9 (Oct 29)** instead, one week before
the vote, when the material is live and the filings are current. This costs
nothing: neither cluster depends on Voter Files or Roll Calls, so Sessions 11 and
12 can take them up afterward without a gap. It also puts polls (Session 4) and
the finance material ahead of Session 10, so election night grades work the class
has already done.

**Everything else is taught in section order**, and within a section in
`INDEX.md` order, with one small local swap: **Population Estimates (I.4) is
taught in Session 2 beside the Decennial Census rather than after the ACS.** The
estimates program is controlled to the decennial count, and putting the two
instruments that do not sample in one session is what makes the ACS session
land.

**🔒 Session 14 is protected.** It is the one session that does not get cut,
moved or compressed. The districting material is the payoff the whole book builds
toward, and it is the only place a method is graded against a known truth.
Section IV holds both of the last two Thursdays so that `rpv` and `vote-dilution`
are reached even if the semester slips a session.

**If time runs short, cut in this order:** the second half of Session 13
(`bisg-check` and `uncertainty` become reading — both are graded again inside
`rpv`, so the lesson survives) → then Session 8's career cluster (`retirements`,
`primary-defeats`, `careers`) → **never Session 14.**

## Tuesday companions — twelve of them

The textbook does not need all eighty minutes, so the twelve docs marked
**(Tue)** above are read on students' own time and taken up for twenty to thirty
minutes. This is capacity the course already had: it keeps Thursdays to one kind
of data each and costs no data session.

`overplotting` · `regional-shift` · `chord` · `areal-units` · `sparklines` ·
`distributions` · `bellwether` · `pie-radar` · `streamgraph` ·
`campaign-visits` · `officeholder-age` · `rank-size`

**The display docs are what this slot is for.** `overplotting`, `chord`,
`sparklines`, `distributions`, `pie-radar`, `streamgraph` and `rank-size` each
read data the class has already met and ask what a chart does to it, which is
exactly a twenty-minute conversation and not a session. The other five are short
arguments rather than sessions, and each sits beside the Thursday it argues with.
They also let the two eleven-doc sessions (6 and 9) breathe.

## Known forward references

The rule is that nothing is worked out from a source the reader has not met.
Fifteen docs break it, all of them by *reading another doc's built file* and none
by assuming the reader has read that doc. Recorded here rather than hidden,
because a rebuild-from-scratch has to run in dependency order rather than session
order.

| Session | Doc | Reads | Which sits in |
|---|---|---|---|
| 1 | `census-source` | `areal-units`, `migration` | Session 3 |
| 1 | `census-coverage` | `apportionment`, `migration` | Sessions 2, 3 |
| 1 | `census-race` | `data-sources` | Session 13 |
| 2 | `census-decennial`, `overplotting` | `areal-units` | Session 3 |
| 4 | `poll-simulation` | `electoral-map` | Session 6 |
| 6 | `admin-records-source` | `policing`, `jury-selection`, `redlining` | Session 12 |
| 6 | `county-returns`, `mapping`, `wind-map` | `data-sources` | Session 13 |
| 6 | `wind-map` | `ga-precinct-returns` | Session 7 |
| 6 | `distributions` | `house-competition` | Session 8 |
| 7 | `crossover`, `nationalization` | `house-competition` | Session 8 |
| 7 | `residual-votes` | `data-sources` | Session 13 |
| 10 | `election-night` | `redistricting` | Session 14 |

Three patterns, and only one is a problem worth watching:

- **A cluster's chapter reads its own cluster's briefs**, which is what a chapter
  that summarizes a kind of data is supposed to do. `admin-records-source` is the
  Section III intro and reads its section's last cluster; `census-source` and
  `census-decennial` read Section I briefs one session ahead. None of these is
  load-bearing for a reader.
- **Five docs read `data-sources`**, the Joining Files chapter in Session 13. It
  supplies crosswalks the returns docs use. Building the book from scratch means
  running it early.
- **`election-night` reads `redistricting`** and is pulled forward to a fixed
  date, so this one is a genuine ordering conflict rather than a filing quirk:
  the file has to be built before 5 November even though the session that teaches
  it is 1 December.

## Before a session can run

**Two live docs need a Census API key set in the environment as
`CENSUS_API_KEY`:** `zip-codes` (Session 3) and `policing` (Session 12). Both
stop with a clear error if it is absent. `census-api` itself is keyless by design
— it is built from open metadata addresses and makes no live data query — and it
teaches the incident: a live key sat in plain text in a teaching handout for a
year. **That key is no longer in the F25 handout** (checked; the file now carries
a placeholder), so the revocation item is closed. The policy is not: either
students each register their own key, free and instant, or those two docs run
from a committed capture. **Decide before Sep 10.**

**The acquisition questions are all closed.** House results 1946–2024 come from
the Clerk of the House's own typeset volumes via `house-competition/data/parse-clerk.py`,
keyless and with no manual step; precinct returns and geography come from the
Georgia Secretary of State's own county tabulation exports via
`ga-precinct-returns/data/parse-ga-sos.py`, with two browser downloads for the
archived years. No guestbooks remain, down from three.

**The pattern worth saying to the class:** the House series was unblocked by
going *upstream* — past the tidy academic compilation behind the guestbook, to
the official document it was compiled from. **When the convenient copy is gated,
the primary source often is not.** `lobbying` and `disenfranchisement` hit the
same wall in different forms and were solved the same way.

The parsing forensics that used to live in this file — the footnote-marker bug
that recorded Ralph Abraham with 2 votes instead of 134,616, the fusion-line and
unopposed-candidate fixes, the Louisiana runoff flag, and the `dvp`/`dpres`
column-name trap that had the split-district series measuring seats changing
party — now live where the code does, in `house-competition/data/build-data.R`
and the brief itself. They are not scheduling facts and are not repeated here.

---

## Substantive topics — Tuesdays, from the textbook

**The textbook is not read in its own order, and it is not tied to the sections.**
Each chapter is placed next to the data session it explains. The sequence below
is the authority; `readme.Rmd` schedules it.

| Tue | Chapter | Placed there because |
|---|---|---|
| Sep 1 | **Ch. 1** | the four standards, before anything is judged against them |
| Sep 8 | **Ch. 2** + the Electoral College section of **Ch. 9** | Session 2 is apportionment and Session 3 is § 203 |
| Sep 15 | **Ch. 13** | Session 4 is what a survey can establish, and Ch. 13 is how voters decide — the thing no record holds |
| Sep 22 | **Ch. 6** | Session 5 measures party identification, which is Ch. 6's subject and a survey construct |
| Sep 29 | **Ch. 3** + **Ch. 11** | Session 6 is a century of returns; Ch. 11 lands before Session 7's precincts, because Session 8's own Tuesday is Test 1 |
| Oct 6 | **Ch. 5** + **Ch. 10** | Sessions 7–8 are both — `vote-targeting` is Ch. 5's worked example, `house-competition`/`careers`/`midterm-loss` is Ch. 10's argument |
| **Oct 20** | — | **Test 1: Ch. 1, 2, 13, 6** |
| Oct 27 | **Ch. 4** + **Ch. 7** | Session 9 is money and attention, one week before the vote |
| Nov 3 *(no class — Election Day)* | **Ch. 8** + **Ch. 9** in full | election week; Session 10 grades the forecast Sessions 4 and 9 built |
| **Nov 10** | — | **Test 2: Ch. 3, 11, 5, 10** |
| Nov 17 | **Ch. 12** | Sessions 11–12 are registration, turnout and the records institutions keep about people |
| Nov 24 | *none* | remote data session; break reading is the Session 14 material |
| **Dec 3** | — | **Test 3: Ch. 4, 7, 8, 9, 12** |

**Every chapter is read once and tested once**, and no test covers a chapter read
after it. Two forward references are knowingly accepted and flagged in the
syllabus: Ch. 10 and Ch. 11 refer back to Ch. 4 (read five weeks later), and
several chapters refer to Ch. 6 (read in September, so this one is now closed).

**Why not the book's order?** Because the money session moved to Oct 29. Reading
Ch. 4 in September and opening the FEC bulk files in late October is the one
pairing worth breaking the sequence to fix, and once that moves the rest follows:
Ch. 10 wants the careers session, Ch. 12 wants the voter file, Ch. 13 wants the
surveys session, Ch. 6 wants public opinion.

> **Still to check.** The forward-reference audit was done against an older
> pairing. Ch. 13 is read six weeks earlier than it once was, so whether it leans
> on Ch. 12 (still read after it) needs one pass through
> `textbook-chapter-notes.md` before the syllabus is final.

Plus, from outside the book: census classification and race, the Voting Rights
Act after *Callais*, language access, ballot initiatives and mid-decade
redistricting, and ideology measurement.

---

## Conventions for whoever changes the book next

**Refer to the section and the doc, not the session number.** "Section III,
`house-competition`" rather than "Session 8." Sections are stable because they
are a claim about provenance; session numbers have moved with every restructure,
which is the argument.

**Adding a doc is four steps, and the fourth is this file.** Create the folder
and its `<slug>-brief.Rmd`; place the slug in `SECTIONS` in `make-index.py`
(the generator fails if the filesystem and the structure disagree in either
direction); re-run `make-index.py`; **add the slug to a session here.** Nothing
in the build catches a doc that is built, indexed and absent from this file.

**`INDEX.md` is generated — do not edit it by hand.** Re-run
`python3 labs/_lib/make-index.py` instead. As of this writing the committed
`INDEX.md` is one run behind on the third-edition mark for seven docs (`rpv`,
`models-markets`, `seat-forecast`, `senate-2026`, `election-night`,
`whole-foods-cracker-barrel`, `rank-size`) — all seven pass the template test on
disk, so a re-run clears it.

**One open inconsistency, flagged rather than fixed.** `senate-2026` counts
**35** Senate seats on the 2026 ballot; `election-night` says **33**. Both are in
Session 10 and a student will see them side by side. Somebody should settle which
is right before 5 November.
