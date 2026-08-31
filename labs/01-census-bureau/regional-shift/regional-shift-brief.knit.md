---
title: "Regional Population Shift and House Seats"
type: brief
subtitle: "Nobody proposed it, nobody voted on it, and no single decade announced it"
author: "Democracy's Data"
date: "Last updated 31 August 2026"
toc_depth: 2
knit: (function(input, ...) { source(file.path(dirname(input), "../../../../../_syllabus-template/render-brief.R")); render_brief(input) })
---





Everyone says American political power is moving south and west. The Sun Belt rises, the Rust Belt empties; every election-night broadcast carries some version of the claim. This brief asks whether it is true, how fast it has run, and since when, and it answers from the one record in which the movement of power is not a metaphor.

That record exists because of a procedure. Every ten years the country is counted, and the 435 seats of the House of Representatives are divided among the states by a formula fixed in law. States that grew faster than the country take seats from states that did not. No bill is introduced, no legislature approves the transfer, and no voter anywhere casts a ballot on the question. A seat is a vote on legislation, and because a state's electors are its seats plus two, it is an Electoral College vote as well. Gaines and Jenkins (2009) point out that the choices buried in this hand-out attract almost no public attention, because they arrive looking like arithmetic.

How many seats the procedure has moved altogether is the number this brief is built around, and it is held back on purpose. You will be asked to guess it first.

## The test

The answer comes from the Census Bureau's own apportionment time series: every state's population and House seats at every census from 1910 to 2020, in one published file at <https://www.census.gov/data/tables/time-series/dec/apportionment-data-text.html>. Seats are compared from 1960 to 2020, because 1960 is the first census after which the United States had fifty states. Population shares are traced back to 1910 and labelled as such. The House itself has been fixed at 435 seats since 1913, made permanent by the Permanent Apportionment Act of 1929, so seats can only move between states, never multiply.

This is the right source because a House seat is not an indicator of political power. It **is** political power, in the currency the Constitution uses: employment series and housing starts would describe the rise of the Sun Belt better, but none of them is what a state actually received. The file is also self-checking in a way that is rare. The Bureau publishes its own region and nation totals beside the state rows, so the state-to-region assignment that carries the whole argument below was verified against the source rather than trusted.

The count behind every number here is the decennial census. [The decennial census chapter](../census-decennial/census-decennial-brief.html) is that data's biography: what the count asks, who it finds, and who it misses.

## The data

The download is a single CSV of about a hundred kilobytes with one row per geography per census. A `Geography Type` column stacks three kinds of row, State, Region, and Nation, so a reader who summed the population column unfiltered would count the country three times. The build splits the stack into the tables this brief reads: `states.csv`, `regions.csv`, `nation.csv`, plus `seatchange.csv` and `southdefs.csv` built from them.

Three cleaning decisions matter. Every population arrives as a quoted string with commas in it, like `"4,951,560"`, which the obvious read turns into a missing value for every number over 999; the build strips the separators first. Puerto Rico is typed `State` and sits *outside* the national total. The District of Columbia is typed `State`, sits *inside* the total, and has no seat, so it belongs in every population figure below and in none of the seat figures.

`states.csv` carries, for each state and year: `pop`, `reps`, `repchg` (the Bureau's own change-this-decade column, carried through untouched), `region` and `division` from the Bureau's published hierarchy, and four rival definitions of the South (`south_census`, `south_confed`, `sunbelt`, `border_south`), side by side rather than resolved. `regions.csv` carries each region's `share` of population and `seat_share`. `seatchange.csv` reduces the window to one row per state: `reps_1960`, `reps_2020`, `change`. Here is one state, twice, six decades apart.


|State   | Year| Resident population| Seats| Change this decade| Region|       Division|
|:-------|----:|-------------------:|-----:|------------------:|------:|--------------:|
|Florida | 1960|           4,951,560|    12|                  4|  South| South Atlantic|
|Florida | 2020|          21,538,187|    28|                  1|  South| South Atlantic|

The column headed `change this decade` is the number the news reports on apportionment day, and it is the least interesting number in the file. Here is the most recent reapportionment in its entirety, every state whose delegation changed size, and then all six of them at once.


|State          |    Region| Seats after| Change|
|:--------------|---------:|-----------:|------:|
|Texas          |     South|          38|      2|
|Colorado       |      West|           8|      1|
|Florida        |     South|          28|      1|
|Montana        |      West|           2|      1|
|North Carolina |     South|          14|      1|
|Oregon         |      West|           6|      1|
|California     |      West|          52|     -1|
|Illinois       |   Midwest|          17|     -1|
|Michigan       |   Midwest|          13|     -1|
|New York       | Northeast|          26|     -1|
|Ohio           |   Midwest|          15|     -1|
|Pennsylvania   | Northeast|          17|     -1|
|West Virginia  |     South|           2|     -1|


|Reapportionment | Seats changing states| States affected|
|:---------------|---------------------:|---------------:|
|1970            |                    11|              14|
|1980            |                    17|              21|
|1990            |                    19|              21|
|2000            |                    12|              18|
|2010            |                    12|              18|
|2020            |                     7|              13|

The busiest reapportionment in this window moved 19 seats out of 435, under five percent of the House; the quietest moved 7. New York's worst single decade was 5 seats. Taken alone, each of these tables reads as maintenance, and each was reported that way at the time.

> **Stop here and write down two numbers.** You have just read New York's worst
> single decade. **How many seats do you think New York lost in total between
> 1960 and 2020?** And, adding down the middle column of the table above:
> **how many seats do you think changed states altogether?** Written down, not
> thought about. What follows is a comparison between the two numbers you just
> wrote and the two numbers in the file.

## What it says about democracy


|Quantity                                                 | Value|
|:--------------------------------------------------------|-----:|
|Seats that changed states, 1960 to 2020                  |    75|
|States that gained                                       |    14|
|States that lost                                         |    21|
|States unchanged                                         |    15|
|New York's total change                                  |   -15|
|States whose entire delegation is smaller than that loss |    44|

**Most people who put a number on paper put down twenty or thirty. It is 75.** Six unremarkable decades come to 75 seats, and no single one of them announced it. By 2020, 17.2% of the House sat in a different state from the one it sat in sixty years earlier.

The state that carried the cost is the one that had the most to lose. **New York fell by 15 seats**, from 41 in 1960 to 26 now. 44 of the 50 states do not have 15 seats in total: what New York lost would today be the 7th-largest delegation in the country.

![](regional-shift-brief_files/figure-latex/bars-static-1.pdf)<!-- --> 



**Figure 1. The 35 states whose delegation changed size between 1960 and 2020.** One signed bar per state fits the question because the question is who gained and who paid: bars right of the zero line took seats, bars left of it gave them up, and color is Census region. Four states take most of the gains: Florida +16, Texas +15, California +14 and Arizona +6. Four take most of the losses: New York -15, Pennsylvania -10, Ohio -9 and Illinois -7.

**Both lists are geographically pure, and nobody arranged that.** The gainers are Southern and Western, the losers Northeastern and Midwestern, and no step in the procedure that produced them knows what a region is. The formula sees fifty population counts.


|Region              | Seats 1960| Seats 2020| Change| Share of the House|
|:-------------------|----------:|----------:|------:|------------------:|
|Northeast           |        108|         76|    -32|      24.8% → 17.5%|
|Midwest             |        125|         91|    -34|      28.7% → 20.9%|
|South               |        133|        164|    +31|      30.6% → 37.7%|
|West                |         69|        104|    +35|      15.9% → 23.9%|
|Northeast + Midwest |        233|        167|    -66|      53.6% → 38.4%|
|South + West        |        202|        268|    +66|      46.4% → 61.6%|

The last column is worth reading slowly. **In 1960 the Northeast and Midwest together held a majority of the House** — 233 seats of 435. They now hold 167, which is 38.4% of it. A bloc that could pass a bill on its own votes now cannot, and the loss of that majority was never on any agenda. The presidential counterpart follows automatically, since electors are seats plus two.


|Quantity                                  |               Value|
|:-----------------------------------------|-------------------:|
|Northeast + Midwest electoral votes, 1960 | 275 of 535  (51.4%)|
|Northeast + Midwest electoral votes, 2020 | 209 of 535  (39.1%)|
|South + West electoral votes, 1960        | 260 of 535  (48.6%)|
|South + West electoral votes, 2020        | 326 of 535  (60.9%)|
|Electoral votes that changed region       |                  66|

The electoral-vote table is arithmetic about states, not a claim about parties: this file has no election results in it, so nothing here says who benefited.

### A ratchet, not a slosh

There is an obvious objection. Seats bounce, and a state that gains three in one decade and returns three in the next has moved nothing in the end. If that is the story, 75 is churn rather than direction. The file answers cleanly. Sum the per-decade columns, counting every seat any state gained at any reapportionment: 78. The sixty-year total is 75. The two can only differ by a state that gains a seat and later hands it back, so the gap is a count of reversals, and it is 3.


|State      |Seats, 1960 to 2020 by decade    | Net change| Seats given back|
|:----------|:--------------------------------|----------:|----------------:|
|California |38 → 43 → 45 → 52 → 53 → 53 → 52 |        +14|                1|
|Montana    |2 → 2 → 2 → 1 → 1 → 1 → 2        |          0|                1|
|Tennessee  |9 → 8 → 9 → 9 → 9 → 9 → 9        |          0|                1|

47 of the 50 states moved in one direction only across the whole period; the 3 exceptions are the rows above, each accounting for exactly one seat. This is not a cycle. It is a ratchet, and a ratchet does not have to move quickly to end up somewhere a cycle never could.

### Since when

Seats arrive in whole-number jumps, so they lag the thing they measure. Population share does not, and it reaches back to 1910.

![](regional-shift-brief_files/figure-latex/lines-static-1.pdf)<!-- --> 





**Figure 2. Each Census region's share of the national population, 1910 to 2020.** A line per region fits the question because the question is *when*: a share that can be read at every census shows the crossings that seat counts only echo a decade late. The dotted rule marks 1960, the first apportionment with fifty states. The Northeast falls the whole way, from 28.0% to 17.4%, the only region with no reversal across 110 years. The West is the fastest thing on the chart, rising from 7.7% to 23.7%; it passes the Northeast in 1990 and the Midwest in 2010, and the South passes the Midwest in 1940.

The surprise is the South's timing. **The rise of the South does not begin when the phrase suggests.** Between 1910 and 1960 the Census South's share of the country actually *fell*, from 31.9% to 30.7%, and the eleven-state Confederate South was flat. Everything the phrase refers to happens after 1960. The window chosen because the fifty-state House begins there turns out to be the right cut on the substance as well, which is luck, and worth noticing as luck.

### Whether the South is eleven states or seventeen

The most common objection to a regional finding is that the region is a choice, and here it is easy to press, because there is no agreed answer to what the South is. Political history usually means the eleven former Confederate states. The Census Bureau means sixteen states plus the District of Columbia. So compute the rise every way, with the disputed states as a category of their own.


|Definition                                 | States| Share 1960| Share 2020|    Change|
|:------------------------------------------|------:|----------:|----------:|---------:|
|Border South only (DC, DE, KY, MD, OK, WV) |      6|       6.4%|       5.5%|  -1.0 pts|
|Census South (16 states + DC)              |     17|      30.7%|      38.1%|  +7.4 pts|
|Confederate South (11 states)              |     11|      24.2%|      32.6%|  +8.4 pts|
|Northeast                                  |      9|      24.9%|      17.4%|  -7.5 pts|
|Sun Belt (South + West)                    |     30|      46.3%|      61.8%| +15.5 pts|

**The narrower definition gives the larger rise.** The eleven Confederate states gain 8.4 points of national population share; the Census South, which contains all eleven and six units more, gains only 7.4. That is arithmetically possible only if the extra states are a drag, and they are: Maryland, Delaware, Kentucky, Oklahoma, West Virginia and the District went from 6.4% of the country to 5.5%. Demographically they behave like the Northeast, whatever they are called. The finding survives every version of the South in the table, and the broadest version is the one that understates it.

## What this brief cannot tell you

**This file cannot say why anybody moved.** It has population and seats and nothing else: no births, no migration, no incomes, no housing costs, no party labels. It cannot distinguish a family arriving in Phoenix from Michigan from a family arriving from abroad, or either from Arizona families simply being larger.

Nor can it certify the counts. The populations are printed as exact whole numbers, and the Supreme Court's 1999 sampling decision makes them exact as a matter of law, while the Bureau's own published coverage estimates say they are not exact in fact. The margins here are large enough that coverage error does not threaten them, since 15 seats is not an error bar.

And the file is coarse. Twelve observations, one per decade, so anything that happened between two censuses is invisible; four regions for fifty states, with the South the most contested of them. Testing three definitions of the South measures how much the finding depends on that choice. It does not make the category natural.

## What you should have learned

- 75 House seats changed states between 1960 and 2020, 66 of them crossing from the Northeast and Midwest to the South and West, with no vote taken anywhere.
- The Northeast and Midwest held 233 of 435 seats in 1960, a majority of the House; they now hold 38.4% of it.
- The movement is a ratchet, not churn: 47 of 50 states travelled in one direction only, and the whole gap between the per-decade sums and the sixty-year total is 3 returned seats.
- The claim survives every definition of the South, and the broadest definition produces the smallest rise, because the border states it adds shrank while the states below them grew.

## Extensions

- `southdefs.csv` defines the South four different ways and gives each a population and a share. Follow one definition across the years, then another. How much of "the South is growing" is a choice about which states you meant?
- `seatchange.csv` has `reps_1960` and `reps_2020` for every state alongside `pop_growth`. Sort by `change`. Do the states that gained the most seats always have the fastest growth?
- `regions.csv` carries both `share` of population and `seat_share`. Take the difference for each region and year. Does representation track population immediately, or lag behind it?
- `states.csv` flags each state as `south_census`, `south_confed`, `sunbelt` or `border_south`. Find the states the labels disagree about. What is a regional label actually measuring?
- **Stretch.** The Bureau publishes yearly components of population change — births, deaths, and movement in and out — for recent decades. Fetch one state's series and ask *why* it grew, the question this file cannot answer.
- **Stretch.** The Bureau also publishes the priority values behind each apportionment. Re-run the 2020 hand-out under a different divisor rule and ask how much of the shift is formula rather than migration.

## Sources

**Data**

- U.S. Census Bureau, *Apportionment Data (Text Version)*, decennial
  apportionment time series 1910–2020.
  <https://www.census.gov/data/tables/time-series/dec/apportionment-data-text.html>
  Direct file:
  <https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/apportionment.csv>
- U.S. Census Bureau, *Statistical Groupings of States and Counties*
  (Geographic Areas Reference Manual, ch. 6), for the four regions and nine
  divisions.

**The data itself**

Every figure above is drawn from these tables. They are plain CSV, they
open in a spreadsheet, and they are yours to take further.

[`nation.csv`](data/derived/nation.csv), [`regions.csv`](data/derived/regions.csv), [`seatchange.csv`](data/derived/seatchange.csv), [`southdefs.csv`](data/derived/southdefs.csv), [`states.csv`](data/derived/states.csv)

**Cases, statutes and literature**

- *Department of Commerce v. United States House of Representatives*, 525 U.S.
  316 (1999), holding that the Census Act forbids the use of statistical
  sampling to produce the population figures used for apportionment.
- Permanent Apportionment Act of 1929, 46 Stat. 21.
- Gaines, B. J. and Jenkins, J. A. (2009). "Apportionment Matters: Fair
  Representation in the US House and Electoral College." *Perspectives on
  Politics* 7(4): 849–857.

\begin{ddaiprompt}{rebuild}{Rebuild this data yourself, with an AI assistant}
I want to rebuild a public dataset from its original source, without any
starting files. Please do the following and show your work.

THE SOURCE. U.S. Census Bureau, the apportionment time series: every
state's population and House seats at every census from 1910 to
2020, in one CSV of about 40 KB:
https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/apportionment.csv
No account or key is needed. One row is one geography in one census
year, and a "Geography Type" column says which kind: State (52 per
year -- the 50 states, DC and Puerto Rico), Region, or Nation. Three
things to survive. Every numeric column is a quoted string with
thousands separators, like "2,138,093" -- strip the commas before
any arithmetic, or every value over 999 silently becomes missing.
Alaska and Hawaii appear from 1910 with blank seat counts until
1960, because they were territories. And Puerto Rico is a State-type
row that is NOT in the national total, while DC is in the total and
has no seats.

WHAT TO BUILD.
1. A state-by-decade table of population and seats -- and a check:
   the State rows, summed with the cautions above, should reproduce
   the file's own Region and Nation rows exactly.
2. The nation per decade: population, House size, and people per
   seat.
3. Seat change from 1960 to 2020, state by state: who gained, who
   paid.

CHECK YOUR WORK. The copy this book used was fetched in August 2026.
If your rebuild matches it, you should find:
- 684 data rows
- the 2020 Nation row: 331,449,281 people, 435 seats, 761,169 people
  per seat
- 433 seats in the 1910 row, then 435 in every decade after
- from 1960 to 2020: Florida +16 seats, Texas +15, California +14
This series gains one decade per census; the next rows arrive with
the 2030 count, and nothing already in the file changes.
\end{ddaiprompt}
