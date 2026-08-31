# Get Out the Vote — reading notes

Green, D. P. and Gerber, A. S. (2024). *Get Out the Vote: How to Increase Voter
Turnout*, 5th ed. Brookings Institution Press. ISBN 9780815742111.

Read from the VitalSource edition (238 screens; the reader serves encrypted page
images, so there is no text layer — every page here was read off the rendered
page). Printed page numbers are used throughout. In that reader, VitalSource
screen number = printed page + 8.

These are notes for building out the `gotv` lab, not a transcription. Numbers,
table cells and definitions are recorded verbatim where the lab depends on them;
argument is summarized.

**Status: complete.** All twelve chapters (pp. 1–188) and all three appendices
(A, canvassing, pp. 189–193; B, direct mail, pp. 195–199; C, phone calls,
pp. 201–204), including all three study-level tables in full. The endnotes
(pp. 205–228) were surveyed rather than transcribed — they are citations, and the
source behind each claim used here is recorded inline. **The index in this
electronic edition is truncated: it ends mid-"C" on p. 230.**

Chapter 12 was read first, because the lab's data file comes from pp. 172–73.

**Note that the book has three meta-analytic appendices, not one.** The lab's
current Sources section cites only the book; Appendices A, B and C between them
publish standard errors for **229 distinct experiments** (59 canvassing, 130 mail,
40 phone-call rows across three call types) and confidence intervals for every
pooled estimate that feeds Table 12-1.

Sections headed *"For the lab"* are my commentary, not the book. Everything else is
the book's content, with quotation marks reserved for verbatim text.

---

## Contents (as printed)

| | Chapter | Page |
|---|---|---|
| | Preface | vii |
| 1 | Introduction: Why Voter Mobilization Matters | 1 |
| 2 | Evidence versus Received Wisdom | 9 |
| 3 | Door-to-Door Canvassing | 21 |
| 4 | Leaflets and Signage | 41 |
| 5 | Direct Mail | 55 |
| 6 | Commercial Phone Banks, Volunteer Phone Banks, and Robocalls | 69 |
| 7 | Electronic Mail, Social Media, and Text Messaging | 93 |
| 8 | Using Events to Draw Voters to the Polls | 113 |
| 9 | Using Mass Media to Mobilize Voters | 123 |
| 10 | Voter Registration and Voter Turnout | 139 |
| 11 | Strategies for Effective Messaging | 151 |
| 12 | What Works, What Doesn't, and What's Next | 167 |
| A | Appendix A: Meta-Analysis of Door-to-Door Canvassing Experiments | 189 |
| B | Appendix B: Meta-Analysis of Direct Mail Experiments | 195 |
| C | Appendix C: Meta-Analysis of Phone-Call Experiments | 201 |
| | Notes | 205 |
| | Index | 229 |

---

## Chapter 1. Introduction: Why Voter Mobilization Matters (pp. 1–7)

**The framing.** The U.S. has "the busiest election calendar on earth" — Americans
have more opportunities to vote each decade than Britons, Germans or Japanese
have in their lifetimes. Writing about elections focuses on high-visibility
races, which distorts the picture: those races are professionalized, mass-
communication-heavy, and press-covered. The typical election is smaller and more
personal; candidates for state representative or probate judge cannot afford
television.

**The book's two questions,** posed for every tactic: (1) what steps are needed to
put it into place, and (2) **how many votes will be produced for each dollar
spent?**

**Does mobilization matter? (p. 2)** Close elections produce real policy shifts —
narrow Republican presidential wins in 2000 and 2016 led to tax-code changes;
an analysis of hundreds of close gubernatorial elections finds Democrats who won
narrowly presided over increased spending on education, health, public safety and
public pensions; a study of nearly a thousand close mayoral elections finds
narrowly elected Democrats presided over sizable increases in municipal spending.

> "89 percent of Americans live in jurisdictions where at least one close election
> for some federal or state office occurs over the course of six years."

GOTV campaigns are "sometimes derided as 'field goal units,' adding only a few
percentage points to a candidate's vote share. Although few GOTV campaigns are
capable of reversing the fortunes of an overmatched candidate, **field goals do
win close games.**"

**Why the existing advice is untrustworthy (pp. 3–4).** Conventional advice — "one
part mailings to three parts phone calls for an incumbent race" — is "conjecture
drawn from experience, perhaps, but conjecture nonetheless."

The authors' stated claim to objectivity: **"we are not in the business of selling
campaign services."** Many campaign consultants have financial interests in direct
mail companies, phone banks or media consultancies, and "when they cite
scientific evidence (such as the studies we have conducted), they do so
selectively to portray what they are selling in a positive light."

Their method, stated on p. 4: **"we make a concerted effort to incorporate the
results of every experimental study in the public domain, whether published or
unpublished,"** then pool with meta-analysis — "systematic and reproducible."

*For the lab:* this is the authors' own answer to the brief's question about
inclusion criteria, and it is a claim about **scope** (every study in the public
domain, published or not) rather than a passive selection from journals. It
should be quoted in the brief rather than inferred, and it partly answers — while
not eliminating — the publication-bias worry.

**Deliberate scope limits (p. 4).** The book is "concerned with factors that
affect turnout over the course of a few days or weeks." It does *not* address how
participation is shaped by fundamental features of the political, social and
economic system, though it agrees structural and psychological barriers are
worthy of study. "With six weeks until an election, even the most dedicated
campaign team cannot reshape the country's culture, party system, or election
laws."

**Evidence versus war stories (pp. 5–6).** Three misguided assumptions are named
and answered:

1. *Experts know best.* — Experts "rarely measure effectiveness." Hal Malchow, one
   of the first campaign professionals to embrace experimentation, reports his
   calls for rigorous evaluation often go unheeded despite the money at stake.
2. *Nobody can know, because you can't rerun an election.* — Experts adduce
   dubious statistics and, "lacking a background in research design or statistical
   inference, they frequently misrepresent correlation as causation." The worked
   example: claiming a radio GOTV campaign raised the Latino vote by pointing to
   lower Latino turnout in a previous election or a neighboring media market —
   "proof-by-anecdote is potentially misleading." The answer is random assignment
   of households, precincts or media markets, "used hundreds of times."
3. *If everybody is doing it, it must work.* — "Just because everybody is doing it
   does not necessarily mean that it works. **Large sums of money are routinely
   wasted on ineffective GOTV tactics.**"

> "The recurrent theme of this book is the importance of adopting a skeptical
> scientific attitude when evaluating campaign tactics."

**Preview of the findings (p. 6)** — the book's thesis in one sentence:

> "the more personal the interaction between campaign and potential voter, the
> more it raises a person's chances of voting."

Strongest effects come from authentic personal appeals by someone the voter knows
— friend, family member, co-worker, neighbor. Door-to-door canvassing by
enthusiastic volunteers is effective; "chatty, unhurried phone calls seem to work
well, too." Automatically dialed, prerecorded calls "are utterly impersonal and
rarely get people to vote."

**And the central trade-off, stated at the outset:**

> "the more personal the interaction, the harder it is to reproduce on a large
> scale."

Hence the book is "a shoppers' guide" — no campaign manager can read it and find
*the* answer without considering their own resources, goals and situation.

*For the lab:* this is the book's own version of the brief's argument that the
cheapest tactic is a capacity rather than a price. It is stated on page 6, before
any table, and it is the reason Table 12-1 should never be read as a ranking.

**Structure of the book (p. 7).** Ch. 2, why experiments; chs. 3–9, the tactics
(canvassing, literature and signage, mail, phone, e-mail, social media, events,
mass media); ch. 10, registration drives; ch. 11, messages; ch. 12, open
questions, relational organizing, mobilization versus persuasion.

---

## Chapter 2. Evidence versus Received Wisdom (pp. 9–19)

**The question restated (p. 9).** "The question is not 'What are some helpful
campaign tactics?' but rather '**What are the most cost-effective campaign
tactics?**'" Campaign professionals "know a great deal about the inputs, but they
seldom possess reliable information about the outputs: the number of votes that
these tactics produce."

### Box 2-1. Thinking about Cost-Effectiveness (p. 10) — verbatim caveats

This box is the book's own warning label on every number in Table 12-1, and the
lab should quote it directly.

> "When thinking about the cost-effectiveness of a get-out-the-vote tactic, it is
> helpful to ask, 'How many dollars will it take to produce one additional
> vote?'"

Three caveats, as printed:

1. **Scale dependence.** "some tactics, such as text messages, generate votes
   cheaply insofar as they give a tiny nudge to vast numbers of people. **If your
   constituency does not have vast numbers of people, these tactics might be
   useless to you.**"
2. **The costs are from particular campaigns, and do not extrapolate.** "A
   campaign that goes door-to-door to mobilize voters might successfully contact
   every fourth person and generate votes at a rate of $57 per vote. This finding
   says something about the efficiency of that type of campaign, but it might not
   provide an accurate assessment of what would happen if your canvassers returned
   repeatedly to each house in an effort to contact three out of four voters. **Be
   cautious when extrapolating to campaigns that are very different from the ones
   we have studied.**"
3. **Campaign finance law and personal connections change the arithmetic.** You
   may have "special financial incentives to spend dollars on one campaign tactic
   rather than another," or know someone in printing or telemarketing.

*For the lab:* caveat 2 names the exact operation the brief performs — spending a
$250,000 budget at the table's rates — and warns against it, in the book, on
page 10. The brief's "votes bought" arithmetic is not an over-reading the lab
invented; it is the over-reading the authors anticipated. Quoting this makes the
chapter's argument the book's argument. Note also that the $57 canvassing figure
assumes contacting **every fourth person** — a contact rate the file does not
carry.

**Why anecdote and observational data fail (pp. 10–12).** "It is sometimes quipped
that the word 'data' is plural for 'anecdote.'" Worked examples of confounding:
turnout is higher where campaigns spend heavily on phone banks, but tight races
attract both money and voter interest; turnout is high in union-canvassed areas,
but unions may deploy canvassers where turnout was already high.

Survey self-reports fail twice over: respondents "inaccurately recall" both
contact and voting, and those who wish to appear politically involved
over-report both; and **campaigns target likely voters**, so "even if canvassing
had no effect, your survey would still reveal higher voting rates among the folks
who were contacted. That is *why* they were contacted."

> "Complex statistical analysis creates a fog that is too often regarded as
> authoritative. When confronted with reams of impressive-sounding numbers, it is
> easy to lose sight of the weak research design that produced them."

The illustration: calls that merely encourage voters to "buckle up for safety
when driving" appear to produce huge turnout increases, because **people who
answer calls — regardless of message — vote at higher rates than those who do
not.**

*For the lab:* this is a ready-made teaching example of selection on the
dependent variable, and it is about the same voter file the course uses
elsewhere.

**The six components of these experiments (pp. 13–14),** as printed:

1. A **population of observations** is defined — usually drawn from lists of
   registered voters, sometimes lists of streets, precincts or media markets.
2. Observations are **randomly divided** into treatment and control. Random
   assignment "does not refer to haphazard or arbitrary assignment"; it means
   assignment with a **known probability**, usually by computerized random number
   generator.
3. An **intervention is applied** to the treatment group.
4. The **outcome is measured** — "voting is measured by examining public records,
   not by asking people whether they voted."
5. The difference in voting rates is **subjected to statistical analysis**, to
   determine how much uncertainty remains. Larger studies leave less uncertainty.
   Contact rates matter: "If researchers know the rate at which people were
   contacted, they can calculate how much influence the experimental intervention
   had on those who were reachable."
6. The experiment is **replicated in other times and places.**

**Scope of the evidence (p. 13).** Hundreds of experiments since 1998, across
presidential, midterm, off-year, municipal, runoff and primary elections;
traditional-voting states (Virginia), early-voting states (Texas), vote-by-mail
states (Oregon); Detroit, Eugene, Houston, rural Fresno — "not to mention France,
Pakistan, and Uganda."

**Candour about failure (p. 15).** "If canvassers are allowed to choose which
houses to visit, they may inadvertently administer the treatment to people in the
control group. … We have orchestrated dozens of successful experiments but also
**several fiascos that had to be discarded because the experimental plan was not
followed.**" And on disclosure: "**We believe in open science.** … all of the
experiments we conduct are arranged with the clear understanding that the results
will be made public in a form that permits the accumulation of knowledge."

*For the lab:* discarded fiascos are a form of file-drawer the brief does not
currently discuss — studies removed for protocol failure rather than for null
results. Worth distinguishing from publication bias, since the two look identical
from outside.

### Four illustrative studies (pp. 16–17)

- **Federal midterms, 1998 (New Haven).** Under the League of Women Voters, a
  citywide nonpartisan campaign: canvassing spoke with more than **1,600 people**;
  one, two or three pieces of mail to more than **11,000 households**; thousands
  more called by a commercial phone bank; some received combinations. Messages
  were nonpartisan but varied — civic duty, close election, "voting makes elected
  officials pay attention to your neighborhood." Findings that "have stood the
  test of time": canvassing raises turnout substantially **provided canvassers
  catch people at home**; nonpartisan mail boosts turnout slightly; commercial
  phone banks disappoint; **minor variations in message make little difference**;
  and no special gain from combining tactics.
- **Gubernatorial, 2017 (Virginia, Plus3).** Each volunteer responsible for
  turning out three specific voters chosen by geographic proximity to the
  volunteer's home; contact by any means; voters contacted an average of **1.4
  times**. Roughly **one-third of volunteers never downloaded their list**, yet
  assignment produced a **2.3 point** increase — **7.1 points** for voters at the
  top of the volunteer's assigned list.
- **Municipal primary and general, 2019 (Philadelphia).** Election officials
  partnered with researchers on postcards. Some voters got postcards only before
  the general; others were contacted before the primary and then thanked for
  voting, or told "Sorry we missed you." Standard postcard: **+0.8 points**.
  Postcard plus earlier primary encouragement and follow-up note: **+1.5 points**
  in the general — "the primary mailings had persistent effects."
- **Presidential, 2020 (billboards).** Nationwide evaluation across **155
  metropolitan areas** randomly assigned; **298 billboards** purchased in
  treatment locations. Weak effects overall, but possibly a **1 point** boost
  among voters living within **half a mile** of a GOTV billboard.

### The three-star rating system (p. 18)

**The book grades its own confidence on a three-point scale.** This is the single
most useful thing in chapter 2 for the lab.

| Rating | Meaning, as printed |
|---|---|
| ★★★ | "the finding is based on experiments involving large numbers of voters and … the GOTV tactic has been tested by different groups in a variety of sites" |
| ★★ | "based on just one or two experiments. We have a reasonable level of confidence in the results but harbor some reservations because they have not been replicated across a wide array of demographic groups or political conditions" |
| ★ | "suggested by experimental evidence but subject to a great deal of uncertainty. The conclusion may rest on a single study, or the evidence across studies may be contradictory" |

Worked examples given: commercial phone banks — ★★★ ("several truly massive
experiments … with a great deal of precision"). Election Day festivals — ★★
("fewer in number … less precise estimates"). Radio advertising — ★
("suggestive but remain statistically inconclusive").

*For the lab — this changes the brief's sharpest paragraph.* The brief currently
says: "The book reports which tactics have a statistically significant effect in
prose. `effective` turns that into `TRUE` or `FALSE`, which is tidy, and which
throws away every gradation between a well-established finding and a marginal
one."

That is right in substance but wrong about the source. **The gradation is not
buried in prose — it is an explicit ordinal scale the authors built and applied
throughout the book.** The transcription did not flatten a continuous quantity
into a boolean; it flattened *a three-level scale the authors had already
published* into a boolean, and discarded a column that existed. That is a
materially stronger version of the same argument, and it gives the lab an obvious
repair: add a `stars` column.

**A closing caution and a self-test (p. 19).** "Our research — like all research —
is provisional and incomplete."

> "How many votes would you realistically expect to generate as a result of
> 256,000 robocalls? How about 38,000 nonpartisan mailers? Or 2,500 conversations
> at voters' doorsteps? By the time you finish this book, you will understand why
> the answer is approximately 200."

*For the lab:* this is a better prediction prompt than the one the brief
currently uses, and it is the authors'. Three wildly different-looking campaign
efforts that buy the same 200 votes — 256,000 ÷ 425 ≈ 602, 38,000 ÷ 260 ≈ 146,
2,500 ÷ 17 ≈ 147. (The robocall arithmetic does not land on 200 the way the other
two do; worth checking against chapter 6 before using it in the brief.)

---

## Chapter 3. Door-to-Door Canvassing (pp. 21–40)

**The historical opening (p. 21).** "Visiting voters at their homes was once the
bread and butter of party mobilization, particularly in urban areas. Ward leaders
made special efforts to canvass their neighborhoods, occasionally calling in favors
or offering small financial incentives to ensure that their constituents delivered
their votes on Election Day. **Petty corruption was rife, but turnout rates were
high, even in relatively poor neighborhoods.**"

The decline is attributed to incentives, not evidence:

> "With the decline of patronage politics and the rise of technologies that sharply
> reduced the cost of phone calls and mass mailings, shoe leather politics
> gradually faded away. **The shift away from door-to-door canvassing occurred not
> because this type of mobilization was discovered to be ineffective, but rather
> because the economic and political incentives facing parties, candidates, and
> campaign professionals changed over time.**"

Local party organizations still favor face-to-face mobilization; **national** party
leaders "typically prefer campaign tactics that afford them centralized control
over the deployment of campaign resources. The decentralized network of local ward
heelers was replaced by phone banks and direct mail firms, whose messages could be
standardized and whose operations could be started with very short lead time and
deployed virtually anywhere on an enormous scale." National parties have invested
more in "ground operations" recently, "but these activities still account for a
relatively small share of total campaign outlays."

*For the lab:* p. 21 states the book's causal story about **why the cheapest
effective tactic is the least used** — centralized control and lead time, not
effectiveness. Read with p. 22's account of consultants' profit incentives, this is
the fullest answer the book gives to the brief's question of who the table is aimed
at and why it needed to exist.

**Why the advice you hear is conflicting (p. 22).** Campaigns drifted away from
door knocking because impersonal campaigning has short lead times and minimal
start-up costs, and because buying vendor services made candidates "less beholden
to local party activists." Then: "a class of professional campaign consultants
emerged to take advantage of the profits that could be made brokering direct
mail, e-mail, phone banks, text messaging, and mass media. **Less money was to be
made from door-to-door canvassing**, and campaign professionals had little
incentive to invest in the on-the-ground infrastructure of local volunteers."
Meanwhile local party officials "swear by it" because they are in a tug-of-war
with the national party for resources. Campaigns also "exaggerate the size of
their ground game to create the impression that they have the enthusiastic
support of local activists."

*For the lab:* a concrete account of **why a cost-per-vote table is a political
document**. The tactic that wins on the book's own numbers is the one with no
vendor constituency behind it. That is a better answer to "who decided what went
into it" than the brief currently has.

### Operations (pp. 23–31)

Five discrete tasks: **targeting, recruiting, scheduling, training, supervising.**

- **Box 3-1, How to Get Lists (p. 24).** Registered voter lists are public,
  from local registrars, county clerks, secretaries of state. "**The costs of
  these lists vary wildly across jurisdictions. You may pay $5 or $500.**" Lists
  always contain addresses and sometimes party registration, sex, birth date, and
  turnout in previous elections. Private vendors add phone numbers and e-mail.
- **Box 3-2, Refining the List (p. 24).** Three ways to pare a list. Census data
  at **census-block** level identifies neighborhoods by homeownership, ethnicity
  or poverty. Vendors do ethnic targeting for a price — "**If you are handy with
  the software package R, you can find free open-source code to conduct ethnic
  surname matching yourself by googling 'R code for ethnic surname matching.'**"
  And: "**Although the voter file does not say how a person voted**, it often
  contains information about each person's party registration and record of
  voting in previous elections. Voting in closed primaries usually provides good
  clues about a person's partisan leanings."
- **Scheduling (p. 25).** "An experienced canvasser working in an area with
  accessible apartments or other densely packed housing may be able to speak with
  members of **eight households per hour**. This rate may drop by a factor of two
  when it is difficult, dangerous, or time-consuming to reach voters' doors.
  **Splitting the difference, we assume, for purposes of making some rough
  calculations, that canvassers on average speak with voters at six households
  each hour.**" Canvassing hours: 5–7 p.m. weeknights, 10 a.m.–5 p.m. Saturdays.
- **Recruiting (p. 25).** "**Beware of the fact that canvassers tend to be more
  ideologically extreme than the average voter.**"
- **Training (p. 27).** Half-hour session; scripts are "a rough guideline than a
  script to be read verbatim"; "the goal is not to create an army of automatons
  mindlessly parroting the same words."
- **Deep canvassing (p. 30).** "an unhurried, empathetic, two-way conversation" —
  a ten-minute conversation leading voters to reflect on why elections matter to
  them. "the extra investment of time per voter conversation makes this kind of
  canvassing more difficult to conduct at scale."
- **Supervising (p. 30).** "problems may range from bashfulness to drunkenness."
  Reports "conceivably could be faked (claims to have contacted an unusually large
  number of people should raise a red flag)."

Two full canvassing scripts are printed: **Box 3-3** (nonpartisan, Latino voters,
Fresno — source: Michelson, *Annals of the AAPSS* 601 (2005): 85–101) and
**Box 3-4** (advocacy, American Renewal Project, evangelical Christian
registrants — source: Shaw, Dun and Heise, *American Politics Research* 50, no. 5
(2022): 587–602).

### The experiments (pp. 31–33)

Dozens since 1998, grouped into **four categories**: (1) nonpartisan canvassing
orchestrated by college professors — Dos Palos, Fresno, New Haven, South Bend,
Brownsville, River Heights; (2) campaigns run by nonpartisan groups (Youth Vote)
and advocacy groups (ACORN, SCOPE, PIRG) — Boulder, Bridgeport, Columbus,
Detroit, Eugene, Minneapolis, Phoenix, Raleigh, St. Paul, several California
sites, plus Working America 2016 (NC, MO), One Arizona, Plus3 2017 Virginia,
American Renewal Project 2018, AltaMed 2020, and a 2021 progressive group running
**parallel tests of a paid canvassing firm using a standard script against paid
volunteers using deep canvassing**; (3) partisan precinct walking — Michigan
Democratic Party 2002, a 2004 Election Day effort in battleground inner cities,
Young Democrats of America 2005, a Kentucky Republican, and an RNC-funded 2014
study of "precinct captains" in three contested Senate states; (4) **candidates
themselves canvassing** — Arceneaux's 2004 New Mexico primary study (precincts
assigned to candidate-walked, volunteer-walked, or control) and Barton, Castillo
and Petrie's 2010 county legislature study.

Contexts deliberately varied: Detroit precincts largely African American;
Columbus and Eugene rarely encountered nonwhites; Bridgeport, Brownsville and
Fresno heavily Latino; Los Angeles, Minneapolis and St. Paul multiethnic;
suburban Raleigh and rural Dos Palos against urban Detroit and St. Paul.

### Lessons Learned, with the book's star ratings (pp. 33–37)

| Rating | Lesson |
|---|---|
| ★★★ | **Contacting eligible voters can be challenging.** Multiple attempts needed for contact rates over 30 percent. The 2014 Republican precinct captains in Arkansas, Colorado and Iowa contacted **29.8 percent on the first attempt, 7.8 percent on the second, 1 percent on the third.** "don't expect to contact more than half of your targets." The Dos Palos study, combing a town for two weeks with multiple attempts, "met up with **three out of four** voters" — the practical maximum. |
| ★★★ | **When canvassers are able to reach voters, canvassing generates votes.** "**In fifty of fifty-eight studies, canvassing was found to increase turnout.** (The odds of obtaining such a lopsided distribution of experimental results purely by chance are less than one in one million.)" |
| ★★★ | **Effectiveness varies by type of election and type of voter.** Baseline turnout **30–50 percent → about seventeen contacts per vote**; **below 30 percent → nineteen**; **50–70 percent (typical of presidential elections) → fifty-four.** "In a low-salience election, canvassing has the biggest impact on high-propensity voters, whereas in high-salience elections, canvassing has the biggest effect on low-propensity voters." Caution: don't skip a door just because an infrequent voter lives there — the setup cost is already paid, and "cost-efficiency is not everything." |
| ★★★ | **Canvassing is effective even in uncompetitive electoral settings.** Big effects in landslides where many candidates ran unopposed. "Canvassing evidently makes voters feel that their civic participation is valued." |
| ★ | **A GOTV canvassing effort may be less effective in areas being canvassed by other campaigns.** Duplication; also hard to retain canvassers when well-funded partisan campaigns "lure them away with higher wages." |
| ★ | **Canvass close to Election Day.** Two largest of three timing studies favored the last week. But where early voting exists, put canvassers out earlier. |
| ★ | **The messenger matters.** Ethnic matching unclear (Dos Palos found larger effects for Latino Democrats; Fresno 2002 showed no consistent pattern). But **local canvassers appear more effective**: a large Los Angeles study found canvassers working in their own zip code "significantly more effective … than those canvassing outside their home turf." |
| ★★ | **The message does not seem to matter much, but the quality of the conversation does.** Scripts have emphasized neighborhood solidarity, ethnic solidarity, civic duty, closeness of the election. Effects "so small that none of the studies were able to detect them reliably." Strictly nonpartisan campaigns were slightly but not significantly more effective than issue- or candidate-based ones. "**The only thing that seems to be required of a GOTV appeal is that the canvasser must urge voters to turn out.**" Advocacy groups "have repeatedly found that canvassing door-to-door has **no effect on turnout when the discussion is solely about issues, candidates, or voters' concerns.**" Small possible boosts from asking for a verbal commitment to vote and giving polling place location. |
| ★★ | **Canvassing influences people incidentally and indirectly.** Nickerson's placebo-design experiment: canvassers spoke only to whoever answered; half got GOTV appeals, half recycling reminders. **Other registered voters in GOTV households voted at higher rates**, though they received no message. Markovits's 2021 replication found the paid canvassing firm produced **the same two-thirds rate of spillover**, but **deep canvassing produced no apparent spillovers.** Consequently "**the one-for-seventeen rule … understates the effectiveness of door-to-door canvassing because about 40 percent of the direct impact of canvassing appears to be transmitted to voters' housemates.**" |

### Cost-effectiveness — the full derivation of $57 (pp. 38–39)

Explicit caveats first: canvassing has **start-up costs** (list, walking routes,
supervisor, T-shirts, clipboards, printed material, buttons, magnets, handheld
computers) that the per-vote figure excludes; and "what counts as a 'benefit'
depends on your goals. **The accounting we perform in this section considers only
one goal: getting out votes.**" Uncounted collateral benefits: feedback from
voters, publicity from lawn signs, cleaning an outdated list, registering new
voters, building databases of sympathizers. "**The cost-benefit analysis that
follows is admittedly narrow in focus.**"

The arithmetic, as printed:

> "the wage rate for canvassers has risen quite a bit and is now **between $20 and
> $25 per hour**. In order to err on the side of caution, let's assume **$24**. If
> your canvasser speaks with voters at **six households per hour**, and each
> household contains an average of **1.5 voters**, you are in effect getting **six
> direct contacts and three indirect contacts per hour**. Applying the
> **one-for-seventeen rule for the direct contacts and a one-for-forty rule for
> the indirect contacts** implies that it takes **$57 worth of labor to produce
> one additional vote**."

Check: 6/17 + 3/40 = 0.353 + 0.075 = 0.428 votes per hour; $24 ÷ 0.428 = $56.1.

Also: "Contacting six households per hour produces approximately **one additional
vote every two hours.**" And costs move both ways — pizza and beer instead of
wages cuts them; training and supervision drive them up, "so if you are hiring
staff to manage your canvassing campaign, you might encounter substantially
higher costs per vote."

**This is the single most valuable page in the book for the lab.** The brief's
section "What is inside the dollar sign" says: "A cost column with no definition
of cost attached to it is an invitation to compare things that were not costed
the same way — and the file, as you have it, contains no such definition. To find
one you would have to go back to the book."

Page 38 *is* that page, and it shows the $57 is built from **five separate
assumptions** — a $24 wage chosen from a $20–25 range, six households an hour
(itself "splitting the difference" between eight and four, per p. 25), 1.5 voters
per household, one-for-seventeen direct, one-for-forty indirect. None survive into
`gotv_tactics.csv`, where the whole thing arrives as the integer `57`. The brief's
sensitivity analysis on volunteer wages can now be run on the source's own
parameters instead of invented ones, and every one of the five is a defensible
lab exercise.

Note too that the canvassing figure **already includes household spillover** via
the one-for-forty rule, while the volunteer-phone figure does not. The lab's
`cost_per_contact` column divides `cost_per_vote` by `contacts_per_vote` and gets
$57/17 = $3.35 for canvassing — but the book's own labor cost per direct contact
is $24/6 = **$4.00**. The derived column is arithmetically consistent with the
file and inconsistent with the book, precisely because the file dropped the
indirect contacts.

### Assessment and conclusions (pp. 39–40)

"there no longer is any doubt that face-to-face contact with voters raises
turnout." Open questions the authors name: whether **candidates** are more
effective at the door than volunteers (only two small studies, mixed); the value
and optimal timing of **multiple visits**; whether **local versus outside
canvassers** differ (needs random assignment of walk lists, "often difficult to
pull off in practice because canvassers often prefer to work in neighborhoods
close to where they live").

**The scale problem, stated plainly:**

> "Perhaps the biggest challenge is bringing a door-to-door campaign 'to scale.'
> It is one thing to canvass 3,600 voters; quite another to canvass 36,000 or
> 360,000. It is rare for a campaign to inspire (or hire) a workforce sufficient
> to canvass a significant portion of a U.S. congressional district."

But: "A million dollars is not a particularly large sum by the standards of
federal elections; media campaigns gobble up this amount in the production and
distribution of a single ad that airs for only a few days. But a million dollars
will hire an army of canvassers."

Closing extrapolation, flagged as unevidenced: face-to-face tactics like shaking
hands at a supermarket, house parties, church bingo night "share much in common
with conversations on a voter's doorstep. **We do not have direct evidence about
the effectiveness of these time-honored campaign tactics.**"

---

## Chapter 4. Leaflets and Signage (pp. 41–54)

Leafleting shares canvassing's logistics — walk lists, weather, dogs — but "is
easier, faster, and considerably less demanding … Just about anyone can do it,
even those too shy to knock on doors." New to the 5th edition: "**researchers have
conducted the first large-scale experimental evaluations of the effects of GOTV
billboards.**"

### Operations (pp. 42–46)

Leaflet design: visually engaging, clear large print, plus detailed information
for credibility. **Door hangers** (perforated, hung on the knob) beat leaflets
left on the doormat. Box 4-1 reproduces a three-color Loeffler/Perdue door hanger
from the January 2021 Georgia Senate runoff, with a **QR code** for polling place
lookup (source: democracyinaction.us, accessed July 13, 2023).

**"Blind canvassing"** — dropping at every household without a target list — is
efficient in high-registration neighborhoods and wasteful in urban apartment
complexes. "It is a bad idea to place leaflets in mailboxes, which legally are the
special domain of the postal service."

Signage costs: **~$4.50 per sign** printed and shipped, for orders of at least a
thousand; campaigns in the studies **placed forty signs per targeted precinct**;
rule of thumb, **one hour of labor and one gallon of gas per precinct**. "No
scientific studies have evaluated what kinds of signs work best." Sign rustling is
common — "judging from the sheer number of scandals that Google returns when one
searches the phrase 'caught stealing yard signs.'"

### The experiments (pp. 46–50)

**Leaflets: ten experimental campaigns** — one partisan urging candidate support,
two by interest groups backing a presidential candidate, seven strictly
nonpartisan. Hamden, CT 1998 (2,021 registered voters, streets randomized,
8.5″×11″ postcards, patriotic imagery, no polling location); New Haven mayoral
1999 (3,011 voters on 76 streets); **Michigan 2002** (Nickerson, Friedrichs and
King; Michigan Democratic Party; thirteen assembly districts, ~2,500 voters each;
door hanger with partisan message **plus polling place customized per precinct**);
**Florida 2004** (Azari and Washington; Dade and Duval counties, predominantly
African American precincts, "election protection" message with a hotline);
Philadelphia 2006 special election (Frey and Suárez; 15,550 voters split three
ways — control, standard GOTV leaflet, leaflet noting bilingual ballots); four
California church-based studies 2006 (Michelson, García Bedolla and Green; Orange
County with polling locations, Long Beach with voter guides, then Fresno and Long
Beach without either; **7,655 registered voters combined**).

**Signage: six small-format experiments** — four roadside, one yard, one handheld.
NY congressional 2012 (97 election districts, forty signs each, nonadjacency
constraint); FreedomWorks vs. McAuliffe, 2013 Virginia gubernatorial (131 Northern
Virginia precincts, "For Sale" parody signs); Cumberland County PA Republican
primary (88 precincts, only ten signs each); **MoveOn 2018** (296 Colorado
precincts, "I'm [blank] and I vote. So should you!"); a 2013 mayoral primary yard
sign study (71 election districts, signs to the candidate's known supporters);
handheld signs outside 14 NYC polling locations, "VOTE TOMORROW," 7 a.m.–6 p.m.
the day before the 2005 municipal election, against 14 control sites.

**Billboards.** A small 2007 study (3 treated cities, 21 controls) had a margin of
error of ±8 points. **Vote.org 2019**: 349 eligible billboards across metros in
Louisiana, Mississippi, Kentucky and Virginia; **207 randomly assigned** to
display "VOTE" plus the election date; Minkoff and Mann measured turnout in
three-mile squares — **treated squares 2.3 points higher** than controls averaging
18.4 percent. **2020 presidential**: 298 signs across 155 metro areas; among those
within five miles, "turnout rates were scarcely affected"; new registrations
nearly identical; but **~1 point** among those within a **half mile**.

### Lessons Learned, with star ratings (pp. 50–52)

| Rating | Lesson |
|---|---|
| ★★★ | **Leaflets and door hangers typically have weak effects on turnout.** Weighted average of all ten studies: **one additional vote per 189 registered voters** whose doors receive hangers. Based on **more than 65,000 registered voters**, but "the results fall short of statistical significance because there is about an **11 percent chance** that we would observe an estimated effect this large even if leaflets were ineffective." Still strong enough to rule out big effects: **the upper bound is one vote per seventy-two recipients.** |
| ★ | **Partisan door hangers appear to have a slight edge, but the difference is not statistically reliable.** Michigan: **one vote per 78** recipients. Florida (less overt partisan message, no polling place info) failed to increase turnout. Michigan and Florida combined: **one vote per 127**. Strictly nonpartisan leaflets: **more than 500 recipients** per vote. No head-to-head experiment exists. |
| ★ | **Nonpartisan leaflets seem to be equally (in)effective across segments of the electorate.** The early hypothesis that undeclared or young voters would respond more was supported in Michigan but not in later studies. "recipients are about equally responsive to leaflets, regardless of party or past rates of voter participation." |
| ★ | **Door hangers with polling locations and local candidate information may be more effective.** The two most successful campaigns were Michigan (polling locations) and Long Beach (voter guides). |
| ★★ | **Signage on private property or along roadsides has weak positive effects.** Two roadside candidate-sign experiments gave one small positive and one small negative estimate; the yard sign study was weakly positive; MoveOn signs had a small positive effect in treated precincts and on adjacent ones. **None statistically distinguishable from zero.** Pooled: **precincts receiving signs attract six additional voters** — "even this estimate is statistically equivocal, and the true effect could well be zero." |
| ★ | **Handheld signs advertising an upcoming election appear to boost turnout.** The sole study found effects "on the order of 3 percentage points," but it is small. |
| ★★ | **Billboards appear to mobilize voters living in the immediate vicinity.** 2.3 points in the low-salience 2019 context; ~1 point within a half mile in 2020. |

### Cost-effectiveness (pp. 52–53) — three prices the summary table does not print

**Leaflets: ~$69 per vote.** Printing **$0.15 per leaflet** (what the Michigan
Democrats paid for 100,000 door hangers, converted to 2023 dollars). Leafleteers
at **$18 per hour**, dropping at **forty-five addresses per hour**, 1.5 voters per
address → **67.5 voters reached per hour**. At one vote per 189 voters, labor
alone is **~$50 per vote**; printing brings the total to **~$69 per vote**. More
if a voter guide is printed.

**Yard/roadside signs: ~$36 per vote.** Taking the pooled estimate at face value —
six votes per targeted precinct — with forty 18″×24″ signs at $4.50 each plus $35
for labor and gas: **$36 per vote**. (40 × $4.50 + $35 = $215; ÷ 6 = $35.83.)
"further experimentation is needed before we can say with confidence whether signs
boost turnout."

**Billboards: ~$140 per vote.** The 2020 evaluation cost **~$333,000**; **198,324
voters** lived within a half mile of a GOTV billboard; at an average effect of
**1.2 percentage points**, that is **2,380 additional votes at $140 per vote**.
Effects outside the half-mile radius "seem to be quite faint (indeed, many of the
estimated effects are weakly negative)." Stronger case in off-year elections,
where 2019 effects "seemed to be twice as large."

**This is a direct contradiction of Table 12-1's asterisk convention, and it is
the best single finding for the lab after Appendix A.** The footnote to Table 12-1
says: "*Cost-effectiveness is not calculated for tactics that are not proven to
raise turnout.*" Leafleting carries that asterisk in the table. But **chapter 4
calculates it anyway — $69 per vote — and prints two further prices, $36 for
signage and $140 for billboards, for tactics that do not appear in the table at
all.**

So the blank cells in `gotv_tactics.csv` are not "the book declined to price
this." They are "the book priced it in the chapter and left the price out of the
summary table, under a disclosed rule about statistical significance." (See p. 169,
recorded under chapter 12: the authors state the rule explicitly and tell readers
to "look back at previous chapters for our speculations about the
cost-effectiveness of these unproven tactics." Nothing is hidden inside the book —
the pointer is lost in the transcription to the file.) The brief's section "The
blank cells are the finding" is correct that the cells are results rather than
missing data; the fuller story is sharper: **a number exists, was computed by the
authors, and did not survive the summarization step, because a significance
threshold governs the price column.** That is the same mechanism as the `effective`
boolean operating on a different column, and it is what the course's standing note
on not teaching statistical significance is about — a publisher's uncertainty
convention deciding what reaches the reader.

Note also that $69 for leaflets would rank **second cheapest** in the lab's
current eight-row table, between volunteer phones ($45/46) and door-to-door ($57)
— no, third, but well above the $130 and $106 rows. A tactic the lab currently
records as having no price is cheaper per vote than four of the five it prices.

### Assessment and conclusions (pp. 53–54)

"Leafleting operates on the principle that votes can be produced efficiently if
political communication has even a small impact on a large number of people. …
**if your jurisdiction is small enough to allow you to canvass the entire target
population face-to-face, you should do so** because that will generate the most
votes. Leafleting becomes an attractive option when vast numbers of voters
otherwise would receive no contact."

Open questions named: whether door hangers work regardless of polling-location
content; whether door hangers beat other leaflets; and the broader one — "**Or are
leaflets just direct mail delivered by volunteers?** As experimental evidence
about the effects of leaflets and direct mail has accumulated, their apparent
effects have converged, but the definitive experiments on leafleting have yet to
be conducted."

On signage: "The lack of experimentation in this area is ironic given the
ubiquitous use of signage in American elections." Possible mechanism — "**the
public nature of signage conveys a social norm that one's fellow voters are
engaged in the upcoming election**," picked up again in chapter 11.

---

## Chapter 5. Direct Mail (pp. 55–68)

**Costs (p. 55–56).** Postage is "about half of the final cost" for large mailings.
"A typical direct mail campaign will cost somewhere in the neighborhood of **$0.75
per piece**. If you were to mail 25,000 households three pieces of mail apiece,
the final cost would be approximately **$56,250**." Cost per mailer drops at
larger scale or on small cardstock; some nonprofits get a postage discount.

**The bottom line, stated up front (p. 56):** "conventional nonpartisan mail has,
on average, a small positive effect on voter turnout, whereas **advocacy mail
appears to have no effect** on whether people cast ballots. Over the past ten
years, dozens of studies including **more than a million voters** have measured
the effects of mail. Apart from special forms of messaging that exert social
pressure, express gratitude, offer financial inducements, or provide official
reassurances about ballot secrecy, the experimental literature as a whole
indicates that **direct mail is usually more costly than other methods of raising
turnout.**"

### Operations (pp. 57–60)

Box 5-1, obtaining mailing lists: from the most current list, "expect **less than
5 percent** of mail sent to registered voters to be returned as undeliverable."
Freshen a list against the national change of address registry, or send a
postcard to find dead addresses.

On design: two schools — eye-grabbing graphics versus homely mail that "looks like
something from a local organization." "One of the most effective pieces of mail
tested was nothing more than **plain text on a folded sheet of light blue
paper**." The experiments "include many instances of stylish but ineffective
direct mail."

**Box 5-2, handwritten postcards (pp. 60–61).** Letter-writing "has grown
enormously in scale over the past decade, attracting tens of thousands of
volunteers, who not only write notes but also pay for postage," and can be
"banked" in advance. **A meta-analysis of fifteen public-facing studies: handwritten
postcards generate an average of one vote per seventy-one postcards, about three
times as effective as conventional nonpartisan GOTV mail.** First postcard beats
the second, "so it may make more sense to spread the cards around to other voters
than to send multiple cards to the same recipient." A 2022 study targeting
millions tested early-voting, plan-making, mobilize-your-friends, and
thank-you messages — all similar, the thank-you "a bit better." A 2020 study of
30,000 voters: **in-state postmarks +2 points versus 1.3 for out-of-state**, "not
precise enough to be definitive."

### The experiments (pp. 60–62)

**More than one hundred experiments since 1998.** Illustrative designs: varying
quantity (Green and Zelizer, 2014 New Hampshire, ~100,000 Republican women sent
none, one, three, five or ten mailings); varying language (Mann, Davis and
Michelson, 2015 New Jersey and Virginia — NJ 26,900 English / 26,898 bilingual /
125,597 control; VA 24,041 / 23,999 / 23,978); testing spillover (Chiao, 2020,
seven mailings to **more than 16 million households** with two, three or four
registered voters); varying combinations (Ramírez with NALEO, 2002, four states,
two to four bilingual mailings, ~300,000 mailed and 60,000 control, targeting
low-propensity Latino-surname voters whose turnout ranged from 3 percent to more
than 40 percent across sites).

### Lessons Learned, with star ratings (pp. 63–66)

| Rating | Lesson |
|---|---|
| ★★★ | **Mail that merely reminds voters of an election and urges them to vote has no effect.** Five experiments; overall estimate "essentially zero." Used by researchers as a **placebo**. |
| ★★★ | **GOTV mailers increase turnout when messages emphasize civic duty or making one's voice heard.** Meta-analysis of **sixty-five studies** of nonpartisan GOTV mailers (including reminders, excluding social-pressure and cash-incentive mail): **just under 0.4 percentage points**. Confrontational messages that scold: **1 to 2 points**. |
| ★★★ | **Weaker effects in presidential elections.** **0.09 points** in presidential versus **0.58** in other elections. |
| ★ | **Effects taper off after five or six mailings per address.** New Haven 1999: highest turnout at six mailings (vs none, two, four, eight). New Hampshire 2014: highest at five (vs none, one, three, ten). |
| ★★ | **Weak effects among very low-propensity voters.** 2002 NALEO Colorado (base rate 3 percent) negligible; likewise California low-propensity minority voters and a 2012 Virginia experiment (base rate 4 percent). Even social pressure mail is weak here. Possible explanation: **they never received the mail** — vendors advise against mailing registrants who did not vote in the last presidential election, since they may have moved. |
| ★★★ | **Mail sent by an official source, such as a registrar of voters, is roughly twice as effective as ordinary nonpartisan mail.** A letter from a public official **assuring voters their ballots are secret** works best — better than a reminder from the same official, and better than secrecy reassurance from a nongovernment source. |
| ★★★ | **Advocacy mailings typically have negligible effects.** **Twenty-four separate studies**: the average advocacy mailer raises turnout by **less than one-tenth of a percentage point**. Campaigns sending up to twelve mailings failed to produce effects, "with no apparent 'sweet spot' between zero and twelve." The rare exceptions focused on the importance of turning out. |
| ★★ | **Effort or expense does not buy turnout.** Expensively produced mailings are "not especially good at raising turnout." Mailed voter guides (California, Chicago), some in recipients' first language, "raised turnout by **less than one additional voter per one hundred recipients**." Handwritten notes: **one vote per seventy-one mailings**. |
| ★★ | **Bilingual mailings are not more effective than English-only.** "If anything … mailings written solely in English have done a slightly better job of mobilizing Latino voters." |
| ★★★ | **Unconventional messages enhance mail's effect.** Within conventional categories, "subtle variations in messaging seem to make little difference" — negative ≈ positive advocacy; ethnic solidarity ≈ hope ≈ closeness of election; extra information about the voting system adds little. But: 2006 Michigan primary, **180,000 households** — showing voters **their own voting record produced a jump of 5 percentage points (one vote per twenty recipients)**; showing **their own and their neighbors' records boosted it to 8 points.** Smaller in high-turnout elections (2012 Wisconsin recall) but still above conventional mail. Panagopoulos's **gratitude** messages generate above-average effects with few complaints. **Note: shaming and gratitude mailers are excluded from the average-effect calculations for nonpartisan mail.** |
| ★★ | **Reminding a recipient of an earlier pledge to vote is especially effective.** "Most people who are asked to pledge to vote will do so." |
| ★★★ | **No evidence of synergy between mail and other tactics.** Only Cardy's 2002 gubernatorial primary study hinted at it, "offset by an overwhelming array of studies" — 1998 New Haven, 2002 NALEO, 2006 California Votes Initiative, nonpartisan studies in 2010, large advocacy studies in 2006 and 2014. |
| ★★ | **Little household spillover from mail.** Chiao's 2020 study found "relatively weak indications"; a 2007 social pressure study found **no effect on housemates**. |

### Cost-effectiveness (pp. 66–67) — and where the lab's $130 comes from

The derivation, verbatim in substance:

> "One additional vote is generated for every **260 people** who receive a
> conventional nonadvocacy GOTV mailer. The typical address on a voter
> registration file contains **1.5 voters** on average, so if mail is addressed to
> all voters at a household, voter turnout increases by **one vote for every 173
> pieces of mail**. At **$0.75 per piece**, it takes **$130 of mail to produce one
> additional vote.**"

(260 ÷ 1.5 = 173.3; × $0.75 = $130.)

**A second, cheaper price for mail that the summary table does not carry:**
gratitude mailings produce **one vote per ninety-one recipients**, "implying a cost
per vote of **$46 ($0.75 × 91/1.5)**." That is **a third the cost of the mail row
in Table 12-1**, and identical to the volunteer-phone rate, for a tactic that is
still direct mail.

**A warning against the highest-effect tactic in the book (p. 67).** On social
pressure mail: "we would **warn readers against using this tactic**. Social
pressure mail is like lightning in a bottle, an interesting but dangerous curio."
Survey research shows the public takes a dim view of it, and "**one in every three
hundred recipients lodges a complaint** by e-mail or phone. If you send out
thousands of mailers, you will quickly be inundated with complaints, not to
mention calls from journalists eager to investigate accusations of voter
intimidation."

*For the lab:* the brief cites Gerber, Green and Larimer (2008) for the eight-point
social pressure effect and asks whether it is "mobilization or coercion." **The
authors answer that question themselves, in their own advice, and side with the
worry.** Quoting the warning is stronger than posing the question rhetorically —
and the one-in-three-hundred complaint rate is a measured quantity to put beside
the eight points.

### Assessment (pp. 67–68)

Mail "makes sense for certain types of campaigns" — those lacking people power for
door hangers, or needing centralized control of very large programs. "**Appendix B
provides a useful statistical summary of how advocacy and GOTV mail campaigns
have fared.**" *(Note: there is an Appendix B — to be read with the back matter.)*

---

## Chapter 6. Commercial Phone Banks, Volunteer Phone Banks, and Robocalls (pp. 69–91)

**Thesis (p. 69–70):** "phone banks work to the extent that they establish an
**authentic personal connection** with voters. Prerecorded messages are rarely
effective. Commercial phone banks that plow through get-out-the-vote scripts
without much conviction do little to increase turnout. Phone banks staffed by
enthusiastic volunteers are typically effective … When commercial phone banks are
carefully coached — **and paid a premium to slow their callers down** — they can be
as effective as the average volunteer phone bank."

### Operations (pp. 70–76)

**Commercial phone banks.** Little lead time — "with the script and target list in
hand, a telemarketing firm typically requires only a matter of hours." Hence "at
the end of a campaign the remaining contents of the war chest are often dumped
into a commercial phone bank." Practical advice: be clear **what counts as a
completion** before signing ("if the respondent hangs up immediately, is that
billed as a contact?"); put your own name on the target list to track the vendor;
listen in on calls.

**Robocalls and the law (pp. 72–73).** FCC: all prerecorded messages must state
the responsible entity at the beginning. Autodialed calls and prerecorded messages
(including texts) **cannot be directed at cell phones**, with only two exceptions —
emergency purposes and prior express consent. "**Back in 2004, when the first
edition of this book was published … just 5 percent of all households had no phone
service other than cell phones. By 2022, that number exceeded 73 percent** and is
climbing rapidly. The vast majority of voters under thirty live in cell
phone–only households. The net effect of these trends is to **skew the targeting
of robocalls toward a diminishing and older subset of the electorate.**" Some
states effectively prohibit robocalls. Price: "**in the neighborhood of $0.05
apiece or less, which is roughly one-tenth the cost of live calls.**"

**Volunteer phone banks (pp. 73–76).** Because autodialers cannot call cells,
"to speak with a large share of the electorate, a campaign must run **two** phone
banking operations: its commercial phone bank calls landlines while the volunteer
phone bank dials cell phones by hand."

> **Work shifts (p. 74): "With an up-to-date list of phone numbers and a chatty
> script, a competent caller will complete sixteen calls per hour with the
> intended targets."** Example given: 16,000 completed calls needs 1,000
> person-hours.

**Beware the automatic dialer (pp. 74–75).** Predictive dialers make the person who
picks up hear "the telltale silence of a telemarketing call about to commence,"
and they hang up. "**Hand-dialed phones have completion rates in the 40 percent
range, but an automated dialer can lower those rates to 12 percent or less.**"
The incentives diverge: "From the standpoint of a commercial phone bank, which is
paid by the completed call, automated dialers make good business sense. When
managing your own phone bank, your political incentives are to contact a large
share of the voters on your list even if it means more time spent per completed
call."

Also: conversational calls beat tightly scripted ones. Box 6-1, phone lists:
voter files "are often loaded with outdated phone numbers"; hire a phone-matching
firm or run robocalls first to weed out nonworking numbers. Box 6-2 reprints a
full "conversational" Youth Vote 2002 script (source: Nickerson, *AJPS* 51 (2007):
269–82).

### The experiments (pp. 77–86)

**Robocalls.** Seattle 2001, ten thousand calls, message from the local registrar
reminding people to vote the next day — **turnout no higher than control**, for
in-person and absentee alike. "**'Forgetting' Election Day is not why registered
voters fail to cast ballots. Low voter turnout reflects low motivation, not
forgetfulness.**" NALEO 2002, five states, two celebrity calls in Spanish, **250,000
treatment / 100,000 control** — effects too weak to distinguish from zero. Vanessa
Williams GOTV and "election protection" messages, 2004 NC and MO — no effect on
either racial group in either state. Priests recording bilingual robocalls — no
effect. **Exceptions:** Governor Rick Perry's endorsement robocall, 2006 Texas
Republican primary, hundreds of thousands microtargeted as Perry supporters —
**+0.4 points**. A 2014 PAC calling six states, up to six calls — **six votes per
1,000 people who answered at least one call**; heavy doses worse than moderate.
A 2008 primary social-pressure robocall naming the recipient's own record —
**more than 2 points, statistically significant**; weaker in the November 2008
general. Texas Home School Coalition 2016 primary, zero to seven calls — **+0.6
points overall, one vote per 1,000 attempted calls**. A 2016 presidential
social-pressure robocall to **1.76 million registered Republicans in fourteen
states**, four scripts, **live response rate 39 percent** — the blended
advocacy/social-pressure script raised turnout **0.38 points**, "rather good for a
single robocall in a high-salience election."

**Commercial phone banks.** 1998 New Haven and West Haven — no effect from any of
three scripts. 2002 Iowa and Michigan replication: 60,000 called, **more than one
million in control** — **one vote per 280 contacts**, not distinguishable from
zero. Illinois 2004 — **one per fifty-five**. NC and MO — **one per five hundred**.
2002, a phone bank **paid top dollar** to deliver a chatty, unhurried appeal to
young voters — impressive results. 2014 primary, three states, survey firm —
**one vote per forty-eight contacts**. The 2004 script-length experiment (NC, MO):
standard script **one per eighty-three**; medium "do you know your polling
location" script **one per thirty**; the longer recruit-your-friends script an
unexpectedly weak **one per sixty-nine** — "coming up with the right chatty script
is still more art than science." **Mann and Klofstad 2010** head-to-head, four
phone banks, **more than 100,000 voters**, identical chatty script: the two
low-quality banks **one vote per 500 contacts**; the two high-quality banks **one
per 111 and one per 71**. "Ironically, the lower-quality phone banks also reported
a higher rate of contacts, which meant that they ended up being more expensive on
a cost-per-vote basis." Advocacy calls fare no better — McNulty's San Francisco
Proposition D study, ~30,000 calls, **one vote per two hundred successful
contacts**; Cardy's 2002 pro-choice gubernatorial primary, **one per 250**; an
Albany head-to-head of partisan versus nonpartisan scripts found neither had
appreciable effect, "if anything, the nonpartisan script worked slightly better."

Industry pushback is quoted directly: Grenzke and Watts denounced nonpartisan
scripts as betraying "basic misunderstandings about why and how GOTV efforts are
conducted." The authors' reply: "That's a testable proposition. Let's see what the
evidence says" — and then report the advocacy results above.

**Volunteer phone banks.** Youth Vote 2000, four sites — **one vote per twenty-two
contacts** (large in Albany and Stony Brook, weak in Boulder and Eugene). Youth
Vote 2002, scaled up with paid temps, high turnover, thin supervision — **one per
fifty-nine calls**. Michigan Democratic Party 2002, 10,550 targeted, 5,319
completed — **one per twenty-nine**. NALEO Los Angeles/Orange counties — **one per
twenty-two**; NALEO California 2014 — **one per twenty-seven** for single-voter
households. MoveOn 2006 special congressional — **one per twenty-six**.
Recontacting people who said they intended to vote: Southwest Voter Registration
2006 Los Angeles — **one per eleven**; Orange County Asian and Pacific Islander
Community Alliance, June 2008, multilanguage — **one per nine**.

> "**one additional voter is produced for every thirty-six contacts.** The real
> question for volunteer phone banks is not whether they work under optimal
> conditions, but rather whether they can achieve a sufficient volume of calls to
> make a difference. It is the rare volunteer phone bank that can complete the
> 36,000 calls needed to generate 1,000 votes without compromising quality."

### Lessons Learned, with star ratings (pp. 87–88)

| Rating | Lesson |
|---|---|
| ★★★ | **It is increasingly difficult to reach voters by phone.** In the 1990s a phone bank could reliably contact half the people it called; rates have plummeted. Phone banking is shifting to **opt-in lists**. |
| ★★★ | **Robocalls have weak positive effects.** Best guess from **nine large experiments**: **one vote per 425 contacts**, "with several experiments showing no effect at all." May be cost-effective at scale given the low price, "**but do not expect to notice the effects** … it takes roughly **425,000 contacted landlines to produce 1,000 votes.**" |
| ★★★ | **Live calls from professional phone banks produce modest effects.** A standard ~30-second script raises turnout **0.9 points** among those contacted. High-quality banks charge a premium and are more influential per contact, but even they only reached **1.4 points**, i.e. **one vote per seventy-one contacts**. |
| ★★ | **Script content barely matters for professional phone banks.** The three 1998 New Haven/West Haven scripts were equally ineffective, as were the three 2002 Youth Vote scripts; direct advocacy-versus-nonpartisan comparisons (Albany 2005, Missouri 2006) found no difference. |
| ★★★ | **Volunteer phone banks are often effective, but quality varies and capacity is limited or unpredictable.** Average **one vote per thirty-six completed calls**, but "**roughly one-third of the experimental studies of volunteer phone banks reveal effects of less than one vote per fifty contacts.**" |
| ★★ | **Brief-reminder volunteer scripts produce votes at the low end.** |
| ★★ | **Recontacting people who expressed an intention to vote boosts effectiveness** — for volunteers. "Unfortunately, commercial phone banks seem unable to re-create the magic of an ongoing conversation." Of three large tests of recontacting, only one found follow-up useful. |

### Cost-effectiveness (pp. 89–90) — the exact source of three rows in the lab's file

Prices, as printed: a commercial phone bank "might charge **$1 per completed
call** when contracting for brief scripts" (plus a possible **$0.30 surcharge per
attempt to call a cell phone**); "top-of-the-line commercial phone banks cost **$2
per completed call**."

> "Suppose you hire a commercial phone bank that is not charging top dollar.
> Several experiments, many of them quite large, suggest that **one vote is
> generated for every 106 completed calls. At $1 per completed call, that comes to
> $106 per vote.** Bear in mind that nowadays, commercial phone banks typically
> **reach only one-eighth of the voters they call**, so your target list will have
> to be large."

> "**Volunteer phone banks staffed by activists produce votes at an average rate of
> approximately one per thirty-six contacts (contacts here exclude messages left
> with housemates). If volunteers are paid at a rate of $20 per hour and make
> sixteen such contacts per hour, then one additional vote costs $45.** If you have
> the good fortune of working with talented volunteers who generate votes at a rate
> of one vote per twenty completed calls, **this rate falls to $25 per vote**.
> Enthusiastic unpaid volunteers mean better cost-efficiency, but higher
> recruitment, supervision, and training costs work in the opposite direction."

**This page settles four things for the lab:**

1. **The number is $45, not $46.** Confirmed twice — Table 12-1 and p. 89. The
   CSV is wrong.
2. **16 contacts per hour is the book's figure**, stated on both p. 74 and p. 89.
   The brief's `CALLS <- 25` should become 16, which moves the crossover
   substantially (fewer contacts per hour → more volunteer hours needed → the
   money-binding threshold rises).
3. **The $45 already assumes volunteers are paid $20/hour.** This is the most
   important correction to the brief's argument. The brief says the figure
   "prices the phones, the list and the office — everything except the labor,
   which arrives or does not arrive for reasons that have nothing to do with the
   budget," and then runs a sensitivity analysis valuing volunteer time at $0,
   $10, $15 and $25 an hour. **But $20/hour of labor is already inside the $45.**
   The brief's break-even calculation is therefore double-counting: it adds a wage
   to a figure that is almost entirely wage. The real structure is different and
   more interesting — the file's cheapest row is *not* free labor priced at zero,
   it is **paid** labor at $20/hour, and the genuinely-free-volunteer case would
   be *cheaper* than $45, not more expensive.
4. **There is a second volunteer-phone rate in the book — $25 per vote** for
   talented volunteers at one vote per twenty calls. So the range for a single row
   of the lab's table is $25 to $45 depending on caller quality, and the file
   carries a point estimate with no indication that quality is the dominant
   variable.

Also worth adding: high-quality commercial phone banks at **one vote per 71
contacts** and **$2 per completed call** work out to **$142 per vote** — more
expensive than the $106 row, despite being three times as effective per contact.
That is a clean, self-contained illustration of the brief's "two denominators"
argument, drawn from the book.

### Assessment (pp. 90–91)

"phone-based GOTV campaigns are a **hit-or-miss affair**." Cell phones are
off-limits to autodialers, "eliminating more than half of the electorate."
Do-not-call lists do not apply to live political calls but may discourage vendors
from gathering numbers. "it is reasonable to suppose that campaigning by phone
will become considerably more difficult." Open questions: whether volunteer
efforts improve by assigning people to mobilize friends and neighbors (see
chapter 12 on relational organizing); whether calls have household spillover; and
whether crowdsourced calling can supply enough callers over time.

---

## Chapter 7. Electronic Mail, Social Media, and Text Messaging (pp. 93–112)

Context: 93 percent of adults used the internet in 2021, up from 61 percent in
2003; social networking sites attract three-quarters of adults. Three attractive
properties in principle — instant reach, forwarding, flexible content. The
obstacle: "half the battle is getting your message through to your audience,
which is why there is such a premium on messaging to audiences with which you
have some sort of connection."

**Lists (p. 95).** Three kinds. **Opt-in** — high-quality lists contain only people
who actively consented; low-quality ones "contain names of people who did not
object when offered the opportunity to receive political spam. This type of opt-in
list might be called a '**neglected to opt out**' list." **Administrative** databases
(college directories, party loyalist lists) — advantage is they link to mailing
addresses. **Generic** vendor lists, harvested by crawling web pages — "dirt cheap,
but it is anybody's guess as to whether they contain any registered voters in your
jurisdiction."

On subject lines: "There are, of course, all sorts of sleazy tricks to get past
these filters, such as using deceptive subject lines. **Do not resort to these
tactics.**" Concise subject lines about voting work best, and they carry the
message even to those who never open the mail.

### E-mail experiments (pp. 97–101)

Votes For Students, 2002 midterms — colleges nationwide, e-mails sent in varying
**densities** (90 percent of students on some campuses, one-third on others) to
detect forwarding. **20 percent open rate**, "incredibly high" by 2023 standards.
Nickerson matched names to voter rolls: **more than 50,000 subjects at five
sites**, and **no effect on either registration or turnout.** Eastern Michigan
example: registration 48.4 percent treatment vs 49.7 control; turnout 25.3 vs
26.4.

Youth Vote Houston 2003, opt-in list of 12,000+ young registrants, three e-mails —
control turnout 9.2 percent, treatment 9.0. Working Assets 2004, **161,633 people
who had requested registration information**, seven states — those assigned to
treatment were **slightly less likely to register**; turnout unaffected.
Stollwerk with the DNC, 2005 NYC mayoral, three e-mails, 41,900 treatment
(13 percent opened at least one) — **58.7 percent treatment vs 59.7 percent
control.** Stollwerk 2013 replication: partisan e-mails and mixed partisan/gratitude
had no effect; **gratitude-only produced +1.2 points**, though "a skeptic might say
that when that many tests are conducted, some positive results are bound to pop up
by chance." Environmental 501(c)(4) 2006 bond measure, N = 18,818 — 52 percent
treatment vs 52.4 control. Oakland Rising, three e-mails with voter-guide links,
**+1.1 points** but large margin of error. **Vote.org 2016**: four GOTV e-mails to
254,992 registrants, 255,087 control — **identical turnout, 75.1 percent.** A 2016
single late e-mail from a UT professor to 300,000+ Floridians: **all four variations
produced turnout below control.** Democracy Works/TurboVote, **1.2 million
subscribers**, four message types — "none showed significantly positive effects,
and overall those sent e-mails voted at slightly lower rates than the control
group." Jaffe, 2020 Florida primary: 17.9 percent among 76,601 e-mailed vs 17.2
among 76,515 control.

**The one exception (p. 101).** Malhotra, Michelson and Valenzuela, San Mateo,
2009–10: identical messages sent by the **registrar of voters** and by a
nonpartisan group (PAVE). PAVE's **55,000 e-mails generated a meager twenty-eight
votes.** The registrar's 55,000 e-mails **raised turnout by 0.56 points,
approximately 308 votes** — "just barely exceeds conventional levels of statistical
significance." Box 7-1 reprints both messages (source: Malhotra, Michelson and
Valenzuela, *QJPS* 7 (2012): 321–32).

### Text messaging (pp. 101–106)

Regulatory note: bulk texting is almost always via a commercial interface; a
growing niche matches the voter file to **activists' own phone contact lists**;
"**one-to-one**" campaigns have volunteers text strangers and converse with
repliers — this "grew dramatically in scale in 2018."

Dale and Strauss, 8,500+ numbers of new registrants, November 2006 — **+3.1
points**; read with two prior experiments, **~2.6 points**. San Mateo 2009 and
2010 — small but clearly positive; personalizing by name did not help. Rock the
Vote 2012, **180,000+ people**, five arms — reminder messages **+0.6 points**;
Election Day follow-up failed, and the "drop what you're doing and GO VOTE NOW!"
group voted **slightly below control**; any text overall **+0.5 points**. García
Bedolla, Abrajano and Junn, four California grassroots organizations, 2014 —
**+3.1 points**.

Social pressure texting: nationwide 2016 study, 20,015 voters, five messages
("If you don't vote, YOUR FRIENDS will find out!") — **turnout identical at 80.6
percent**. Vote.org head-to-head in twenty-seven states, three groups of ~108,000:
ten messages **with** reference to personal vote history **+0.35 points**; ten
messages **without** it **+0.65 points** — i.e. the social pressure version did
*worse*.

Polling place information: 188,486 Illinois voters under forty, **+0.25 points**
(one vote per four hundred texted); a study of **more than 1.2 million** nonwhite
or single women voters — plan-making group 52.1 percent vs 52.2 control (no
effect), polling-location group 52.4 percent; the 2017 Virginia gubernatorial
primary analogue **+0.62 points**. Mann and Haenschen with Vote.org, 2018:
Election Day registration states **+0.18**; late campaign to nonwhites in four
states **+0.41**; two experiments on nonwhite voters at 70–80 percent propensity
found nothing. **GoVoteNYC 2022**, low-propensity voters, control turnout never
above 14 percent — "Neither SMS nor MMS proved effective; the average effect was
just under one-tenth of a percentage point."

Summary judgment: effects smaller in presidential contests; larger from a public
official; **social pressure "seem[s] not to be worth the trouble"**; polling place
information helps. "Although the effectiveness of text messaging may be declining
over time, it remains a more reliable way to increase turnout than e-mail."

### Social media (pp. 106–110)

**The 61-million-user Facebook experiment, 2010 general.** Three conditions —
control; **information** (banner, "I Voted" button, polling place link, running
counter); **social** (same, plus profile photos of up to six friends who had voted
and a count). Turnout validated for about one in ten subjects. **The information
treatment had precisely zero effect. The social treatment raised turnout 0.39
points** — small, but decisive given the size. 2012 replication: **+0.35 points**
among those who logged in on Election Day.

"The idea of putting an 'I voted' widget on Facebook users' news feeds is a
creative one, but **this intervention is not something that those outside Facebook
are at liberty to do, even for a fee.**"

**Paid ads do not work.** Collins, Keane and Kalla, Rock the Vote 2012: ~365,000
treatment / 365,000 control — **identical 56.5 percent**. 2013, fourteen states,
~46,500 each — 14.6 percent control vs **14.0** treatment. Together: average effect
on the eligible "probably no greater than zero and at most 0.15 percentage
points." Rock the Vote and Mi Familia Vota, California 2014, ~1.9 million each —
**+0.1 points** in both. Haenschen and Jennings with the *Dallas Morning News*,
70,000+ voters — control 5.83 percent, only the both-messages group higher at 5.95.
**Kalla, 2016 presidential**, 1.2 million voters aged 18–35 in NH, NV, PA, ads
urging support for Hillary Clinton: **+0.14 points, not statistically
distinguishable from zero.** "This advertising campaign cost **$691,005** and
produced **1,459 votes at a cost of $474 per vote.**"

**Friend-to-friend does work.** Teresi's university experiment: ~600 students
friended her; treatment received fourteen election-related updates. Matched to the
Georgia voter file (only 344 matched): **31.2 percent treatment vs 23 percent
control**, statistically significant. Haenschen matched seven people's friends
lists to the Dallas County file, 293 registered voters, 2014 early voting:
**"shame" 76 percent, "pride" 68 percent, "civic duty" 48 percent, control 52
percent.** Scolding worked "only when it is directed at the target voter" — a
parallel experiment where accomplices scolded each other, not the subjects,
produced muted effects.

### Lessons Learned, with star ratings (pp. 110–11)

| Rating | Lesson |
|---|---|
| ★★★ | **E-mail has negligible effects on voter registration.** "Absent these e-mail campaigns, the people who registered online would have registered anyway." |
| ★★★ | **E-mail messages rarely increase voter turnout.** Holds across target groups and message styles. **Exception: e-mail from a local registrar of voters — one vote per 180 e-mails sent.** |
| ★★★ | **Text messages from public officials or grassroots organizations increase turnout.** Smaller in presidential contests. Averaging across all studies including the large 2016 ones: **0.26 points, or one vote per 381 targeted phone numbers.** |
| ★★ | **Social media banner ads and "I voted" widgets have no effect; such campaigns raise turnout only when users are shown which of their friends have voted.** The social prime yields **one vote per 271 users** — "when administered to 60 million Facebook users, this intervention translates into more than 220,000 votes." |
| ★★ | **Advertisements on social media platforms produce little or no apparent increase in turnout**, sidebar or news feed alike. |
| ★ | **Friend-to-friend communication about an upcoming election can raise turnout.** The two studies found large effects — "**on par with the effect of face-to-face canvassing**" — though provisional pending replication. |

### Cost-effectiveness (pp. 111–12)

E-mail: "the cost assessment of mass e-mail is rather **dismal**" — substantial
start-up costs, minimal marginal cost, minimal effects.

**Texting, worked in full:** list vendor **$2 per one hundred eligible cell phone
numbers**, texting service **$7 per one hundred messages sent**. Texting **one
million voters five times apiece costs $370,000**, before recruiting and
supervising volunteers. At **three votes per thousand targeted** (the typical
result, including the final days of presidential elections), that is **3,000 votes
at $123 apiece**. At **ten votes per thousand** — "akin to the effects that are
often observed in nonpresidential elections" — it implies **$37 per vote**.

*Note a discrepancy worth flagging in the lab.* Table 12-1 prices texting at
**$133 per vote** using "$0.35 per target number" and one vote per 381. Chapter 7
prices the same tactic at **$123** using a different cost build-up ($0.02 + 5 ×
$0.07 = $0.37 per voter over five messages) and three votes per thousand. **Both
numbers are in the same book, for the same tactic, and differ by 8 percent.** And
the same page gives a third figure — $37 — for nonpresidential elections. This is a
cleaner demonstration than anything currently in the brief that a single cell in
the summary table is a choice among several defensible numbers, not a measurement.

The chapter's own closing note on this: "**Differences in effectiveness can have
profound implications**, which is why research and development in this area is
currently so vigorous."

**And the sharpest contrast in the chapter:** Facebook advertising at **$474 per
vote** (Kalla 2016, a real invoice for a real campaign — $691,005 for 1,459 votes)
against friend-to-friend contact on the same platform performing "on par with
face-to-face canvassing." Same medium, two orders of magnitude apart, and the
difference is whether the message comes from a person the voter knows. That is
chapter 1's thesis stated in dollars.

---

## Chapter 8. Using Events to Draw Voters to the Polls (pp. 113–122)

**The historical argument (pp. 113–15).** Before the 1880s reforms, voting was
public: parties supplied printed ballots of varying colors; in Connecticut voters
placed the filled-out ballot atop the box to be challenged. Polling places were
sometimes in saloons; the jurisprudence of the era required only that "a man of
ordinary courage" be able to cast his ballot. "Nonetheless, voting rates were much
higher than anything seen in the twentieth century." After the reforms — state
ballots, party workers seventy-five feet back, secret booths — "**Election Day
whiskey ceased to flow, and crowds no longer congregated at the polls. Turnout
abruptly declined.**" The 1884 Hartford procession is quoted from the *Hartford
Daily Courant* as an illustration of what campaigning looked like when 71 percent
voted.

The chapter's thesis: this is "**at least to a limited extent, reversible.**"
Modern festivals are patterned on the nineteenth-century poll party "minus the
liquor," and must be advertised and run so as not to link casting a ballot with
receiving food — vote buying and quid pro quo inducements are illegal.

### The experiments (pp. 115–21)

- **Addonizio, Green and Glaser, 2005–06 — sixteen festivals.** Pilot: Hooksett
  and Hanover NH, matched on population and voting rates, coin flipped. A week of
  publicity, newspaper flyer, three dozen lawn signs, two robocalls to 3,000
  households, cotton candy machine "expertly staffed by political science
  professors." Scaled with Working Assets in 2006. **Across all thirty-eight
  precincts, +2 percentage points; approximately 960 additional votes at a cost of
  $40,133 (2022 dollars) — $42 per vote**, "which puts festivals on the same
  general plane as other face-to-face tactics."
- **Civic Nation, 2016 presidential** — nine festivals in battleground (NC, OH)
  and nonbattleground (CA, TN, TX) states; free food, live radio broadcasts, dance
  troupes, photo booths, arts and crafts, lawn games, puppies. Total cost
  ~$34,000. Green and McClellan's design: each organization nominated more than one
  feasible site, then sites were randomized. **Eighteen voting sites (nine/nine),
  +3.8 points, odds of a result that large by chance about one in forty-five —
  $41 per vote**, "quite good given the challenges of increasing turnout in a
  presidential election."
- **A 501(c)(4) targeting Pennsylvania college students, 2016** — five evaluated
  pairs; festivals "popped up suddenly" with free food and shuttles rather than
  advance community outreach. Turnout higher in three of five pairs, **average
  effect almost exactly zero.**
- **Civic Nation, 2017** — decentralized model, small grants, highly variable
  advance work (some canvassing, most relying on flyers, robocalls or mail).
  **Rain afflicted 85 percent of sites. +0.9 points, cost per vote four times
  2016's** — but the no-rain sites performed on par with 2016.
- **#VoteTogether, 2018 midterms** — 1,946 events encouraged or assisted; **56
  randomized treatment sites** each paired with a control. **Rainfall in 77
  percent of sites. Election Day festival sites came in 0.2 points *lower* than
  controls.** But a parallel experiment in **fourteen early voting locations showed
  +3.5 points**, statistically distinguishable from zero.
- **When We All Vote, 2022** — festivals near early voting sites in PA, NC, MI
  targeting people of color and under-35s, publicized by postcards featuring
  Michelle Obama; **sixty-one locations randomized**; not all organizers followed
  through. After adjusting for that, **+2.7 points** among the targeted
  demographic.
- **Party at the Mailbox (pp. 120–21)** — COVID-era party kits (local snacks,
  signs, sports paraphernalia) mailed to hosts in Black communities. Results
  ambiguous. Baltimore June 2020 primary: hosts 87.4 percent vs 89 percent control
  (lower), but **housemates 43.8 vs 41.1**. Baltimore general: hosts 94 vs 92.5,
  housemates 70.3 → 71. Detroit: lower on both. Philadelphia: hosts +0.2 points,
  **housemates 6.6 points lower.**

### Lessons Learned, with star ratings (pp. 121–22)

| Rating | Lesson |
|---|---|
| ★★ | **Election festivals increase voter turnout** — "if the weather cooperates." Pooling *all* Election Day festival studies, including bad weather and limited outreach: **+1 percentage point. If the average precinct includes 2,700 voters and the average festival costs $2,300, the cost per vote comes to $85.** |
| ★★ | **Festivals at early voting sites increase turnout** — and are "**roughly three times as effective** as festivals held at a conventional polling location on Election Day," perhaps from economies of scale and weekend timing. |
| ★★ | **Party kits for at-home use seem to have limited effects on turnout**, though they may affect attitudes about voting. |

Closing argument: "**experiments continually turn up evidence that old-fashioned
campaign tactics work**… the distant past ought to be mined more frequently for
ideas." The Progressive Era's insistence on sober elections produced what Bensel
calls a "morgue-like atmosphere," and "how much of the precipitous drop in voter
turnout rates between the 1880s and 1920s was due to this transformation remains
unclear."

*For the lab:* the festival row is the clearest case in the book of a **cost per
vote that moves with the weather**. The same tactic is priced at $42 (2005–06),
$41 (2016), four times that in 2017 when it rained on 85 percent of sites,
negative in 2018 when it rained on 77 percent, and $85 in the pooled figure that
reaches Table 12-1. The published number is an average over a variable — rainfall
— that no campaign controls and the file does not record. That is a more vivid
version of the brief's "averages across heterogeneous experiments are applied as
though they were constants" than anything currently in it, and it is a single row.

---

## Chapter 9. Using Mass Media to Mobilize Voters (pp. 123–138)

**A methodological caveat unique to this chapter (p. 125):** "Ordinarily, we would
exclude from our review any nonexperimental study, but **in this chapter we
discuss a small number of quasi-experiments**, or studies in which the treatments
vary in a haphazard but not strictly random fashion. In future editions of this
book, we hope that these studies will be superseded by randomized experiments."

The Fox News example (p. 124) is the chapter's teaching case for confounding, and
Broockman and Kalla's paid-switching study (Fox viewers paid to watch CNN before
the 2020 election) is the fix.

### Do persuasive campaign ads mobilize? (pp. 126–27)

The theory: ad volume signals that an important election is coming, so ads
mobilize even when they never mention voting. Tested by quasi-experiments
exploiting media-market geography — western Maryland receives Pittsburgh
broadcasts, central Maryland almost none; likewise Indiana and Georgia bordering
battlegrounds.

Krasno and Green, **128 geographic zones**, 2000: no evidence ads raised turnout.
"The largest volume of advertising exposure — **enough advertising to expose each
member of the television audience approximately 132 times — raised turnout by less
than 1 percentage point.**" Ashworth and Clinton, New Jersey regions receiving
Philadelphia ads vs the NYC broadcast area: same null. Clinton and Lapinski
randomly exposed large numbers of viewers to positive and negative ads: **no
effect on turnout.** "**Apparently, attack ads do not demobilize voters, and more
upbeat ads do not mobilize voters. Regardless of their tone, campaign ads have
little effect on turnout.**"

### Conveying social norms (pp. 127–29)

Vavreck and Green's two post-9/11 patriotism ads, made by UCLA film students —
"Shape your country, vote this Tuesday" and a darker "Stand up for democracy, vote
this Tuesday." **156 cable television systems** across four states in 2003 (KY, LA,
NJ, VA) randomized to four groups; thirty spots per system (ninety-six in
Louisiana). Result: **approximately half a percentage point**, falling short of
significance; a follow-up automated survey of **25,000+ registered voters** to
identify likely viewers again showed "weak and statistically insignificant positive
effects." Conclusion: ads "merely affirming the norm of participating in an
election do not generate higher turnout rates in low- and medium-salience
elections," and the null "cannot be blamed on low rates of exposure."

### Emphasizing what's at stake (pp. 129–33)

**Rock the Vote 2004** — two ads on the military draft and the cost of education,
both closing "It's up to you." Vavreck and Green, twelve nonbattleground states,
**forty-two cable systems treatment / forty-three control**, four airings a night
in prime time on USA, Lifetime, TNT, TBS over the last eight days. Focus:
**23,869 eighteen- and nineteen-year-olds. +3 percentage points in that age
bracket**, statistically significant; strong up to age twenty-two, small for older
viewers. "**This experiment provides the first clear evidence that televised ads
can increase turnout within a targeted audience.**"

**Native American radio**, de Rooij and Green, 2008 and 2010 — dozens of stations
randomized to air PSAs. **+2.3 points among Native Americans in treated coverage
areas, at a cost per vote of $23**, short of conventional significance.

**Mayoral radio**, Panagopoulos and Green, **seventy-eight mayoral elections**,
Nov 2005 and 2006 — thirty-nine treatment municipalities, 50–90 GRPs each. (A GRP
exposes 1 percent of the market once; fifty GRPs = 50 percent of the market once,
or 1 percent fifty times.) The Syracuse script is reproduced in full — strictly
nonpartisan, naming both candidates, paid for by the Institution for Social and
Policy Studies. **Turnout was higher in twenty-three of thirty-nine pairs, average
+1 point — "a one-in-six chance of coming up with at least twenty-three heads if
thirty-nine coins were flipped."** The 2006 Spanish-language radio follow-up
across **204 congressional districts** boosted turnout among Hispanic-surname
voters and, as predicted, had no effect on non-Hispanic turnout — "on the border
of statistical significance."

**Registration radio, 2020** — Cohen and co-authors randomly assigned **186 radio
stations** with predominantly Black or Latino listeners to a week of
celebrity-voiced registration ads. Hints that ads hastened registrations during the
week they aired, "but there appeared to be no net gain in registrations a few weeks
later."

### Newspapers and product placement (pp. 133–35)

Gerber, Karlan and Bergan, 2005 Virginia — several hundred Washington-area
households given free subscriptions to the *Washington Times* or *Washington Post*,
or nothing. Among **2,571 matched to turnout records**: control **56.3 percent**,
*Post* **57.4**, *Times* **56.1** — overall **a half-point gain that persisted
through the 2006 midterms.** Panagopoulos's four paired towns, half-page ad the
Sunday before a 2005 municipal election: higher in three of four pairs, suggesting
**1.5 points**, but "flip four coins, and about one-third of the time you will get
three or more heads."

Paluck and colleagues wove voter-registration storylines into **three
Spanish-language soap operas** at a randomly chosen point in May 2012 — **no
corresponding surge in Latino registrations**; at best a modest increase in visits
to the Spanish-language Rock the Vote site. "**It may be that product placements
are overrated.**"

### Lessons Learned (p. 136) — every finding rated one star

The chapter states this explicitly: "each of the conclusions is rated with **one
star**, indicating that the findings are suggestive but not conclusive."

- ★ The sheer volume of political ads is a poor predictor of turnout.
- ★ Television ads that specifically urge turnout can mobilize **targeted**
  viewers (Rock the Vote: significant among 18–19s, minimal above 22).
- ★ Broad-based television ads encouraging turnout have weak positive effects —
  the "shape your country"/"stand up for democracy" pair raised turnout **less
  than a quarter of a percentage point**.
- ★ Radio campaigns stressing the importance of upcoming **local** elections
  appear to have some positive effect.
- ★ Radio campaigns urging **registration** before a presidential election have
  little effect.
- ★ Daily exposure to newspapers raises turnout to a small extent among those who
  do not ordinarily receive a paper.
- ★ Newspaper ads urging local participation may increase turnout, "subject to a
  great deal of statistical uncertainty."
- ★ Product placements dramatizing registration on soap operas do not induce
  viewers to register.

### Cost-effectiveness (pp. 137–38) — the cheapest numbers in the entire book

All flagged as tentative, "back-of-the-envelope," and "interpreted with extreme
caution, given that the estimated effects are subject to a great deal of
statistical uncertainty."

| Campaign | Effect | Cost (2022 dollars) | Votes | **Cost per vote** |
|---|---|---|---|---|
| 2003 televised PSAs | +0.22 points over 2.3M+ registered voters | $112,288 | just over 5,000 | **~$22** |
| Rock the Vote TV ads | +0.56 points across the age spectrum, 350,000+ voters | $44,921 (airing only; production cost unknown) | ~2,000 | **~$22** |
| Mayoral radio | +0.79 points across 39 towns averaging 50,000 voters | $173,265 | ~15,400 | **~$11** |
| Spanish-language radio 2006 | — | $236,155 | — | **~$16** |
| Native American radio 2008/2010 | +2.3 points | — | — | **~$23** |
| Radio registration 2020 | negligible | $400,000+ | — | — |

> "when television reaches vast audiences, even a small effect goes a long way."

And the closing scepticism: "Until more experimental results come in, **we cannot
rule out the possibility that the effects are zero, in which case cost-effectiveness
goes out the window.** Those in the advertising business like to joke that half of
all money spent on television is wasted; the problem is that you don't know which
half it is. **Our view is more skeptical: How do they know that only half is
wasted?**"

**For the lab — this is the strongest instance of the asterisk problem.** Table
12-1 prints **Television GOTV** and **Radio GOTV** with an asterisk, meaning
"cost-effectiveness is not calculated for tactics that are not proven to raise
turnout." But chapter 9 calculates cost per vote for both, on the same page, from
real invoices: **$22 for television, $11 to $23 for radio.** (Television is the
very example the authors use on p. 169 when explaining the asterisk rule, and they
point readers back to this chapter for exactly these figures — so the book is
consistent with itself. The loss happens when the table is transcribed without the
chapters.)

If those numbers were in the lab's table they would be **the two cheapest rows in
the entire book** — cheaper than volunteer phone banking at $45, cheaper than
canvassing at $57, cheaper than everything. They are excluded not because they are
expensive but because their *effects* fail a significance threshold, and the price
column is where that exclusion is enforced.

That is the whole argument of the brief's "blank cells are the finding" section,
turned up considerably: **the tactics with no price are not the ones that cost the
most. On the book's own arithmetic, several of them cost the least.** A student
reading only `gotv_tactics.csv` would conclude television is unpriceable; a student
reading p. 137 would conclude it is the best buy in the book and highly uncertain.
Neither is quite right, and the gap between them is exactly what a chapter on
provenance is for.

Also note the asymmetry the lab should not gloss: the book's stated reason for
withholding is defensible — an effect indistinguishable from zero makes the
denominator of "dollars per vote" meaningless, and dividing by a number that might
be zero produces an arbitrarily small price. That is *why* the $11 and $22 figures
are dangerous, and the authors say so. The teachable point is not that the authors
were wrong to suppress them; it is that **a significance convention silently
converted "we are unsure" into "no entry," and the file then converted "no entry"
into `NA`, and `NA` reads as "not measured."** Three steps, each locally
reasonable, ending in a false impression.

---

## Chapter 10. Voter Registration and Voter Turnout (pp. 139–150)

**The debate (pp. 139–40).** One side holds registration requirements suppress
turnout; the other is skeptical, and more skeptical still of easing rules "in ways
that open the door to voter fraud." Political scientists have staked out a middle
position — same-day registration raises turnout modestly without materially
affecting outcomes. "**All sides of this debate have relied on rather flimsy
evidence**": correlations between registration and voting, aggregate comparisons
of permissive states, trends after requirements were introduced. "The first
edition of this book said next to nothing about registration because the body of
experimental evidence was so thin."

### The gray literature — a disclosure the lab should quote (p. 140)

> "Such experiments have become increasingly common in recent years, although
> **many remain outside the public domain because they were commissioned by
> partisan or advocacy groups and never posted on a public website or presented
> in a public forum. This 'gray literature' poses a problem for scholars who seek
> to provide a comprehensive assessment** of what is known about registration.
> **Our policy is that we do not reveal the results of proprietary studies, but we
> also take care not to give undue weight to public studies if we know their
> results are contradicted by proprietary research.** In other words, we are
> careful to neither disclose secret information nor adduce evidence that we know
> to be misleading."

**This is the single most important provenance passage in the book, and it belongs
in the brief.** The authors state plainly that their summary judgments are informed
by evidence the reader **cannot see, cannot cite, and cannot check** — and that
they will silently downweight a published finding they know to be contradicted by
a proprietary study. That is an honest and defensible editorial policy. It also
means a cell in Table 12-1 can differ from what the public literature would
support, for reasons that are by design unrecoverable.

The brief currently frames the compilation's weakness as publication bias — the
file drawer, Rosenthal, Franco/Malhotra/Simonovits. **This is a different and
sharper problem: not studies that vanished, but studies the compilers read and the
reader may not.** It also complicates the brief's claim that "the only way to
interrogate it is to read a book and then read the experiments the book cites" —
for some rows, reading the cited experiments would give you a *different* answer
than the authors reached, and the divergence is deliberate. Worth pairing with the
open-science commitment quoted at p. 15, which applies to the authors' *own*
experiments; the gray literature is other people's.

### Practical considerations (pp. 141–42)

**234 million Americans eligible in 2022, 161 million registered — 69 percent**
(including everyone in North Dakota, which does not require registration).
Registration rates are lowest among **under-25s (49 percent), Asian Americans (60
percent), and Hispanics (58 percent)**. Movers are a second vein; experiments that
tracked people randomly encouraged to move out of public housing found a sharp
drop in registration.

Four approaches: **site-based** (clipboards outside supermarkets, campuses,
concerts, arenas); **blind canvassing** door-to-door — "in an ordinary residential
neighborhood, it would not be uncommon for each canvasser to register just **one
such person per hour**"; **targeting specific individuals** by mail, phone or
e-mail (driver's licence records for soon-to-be eighteen-year-olds, change-of-
address data for movers); and **mass media appeals**.

A recurring warning: registration drives "often **re-register the already
registered**." Box 10-1 covers legal requirements — certification, training,
reporting, deadlines for turning in forms; "registration drives sponsored by a
political group **must not 'lose' the forms** completed by voters seeking to
register with the opposing party." (Source: Diana Kasdan, Brennan Center, 2012.)

### The experiments (pp. 142–49)

- **First-Time Voter Program** (Addonizio), high school seniors, CT/IN/KY/NE/NH/NJ,
  2003–04, **840 eligible students** randomized. Young volunteer leads a
  forty-minute informal seminar, students practise registering and cast a practice
  ballot on the local machine type (~90 percent do). **Turnout approximately 10
  percentage points higher**, holding across electoral and socioeconomic settings.
- **Bennion and Nickerson classroom study**, 2006 midterm, **1,026 classrooms in
  sixteen colleges**, three arms (control; forms on every desk plus instructor
  encouragement; forms plus fellow-student encouragement). Both worked about
  equally: **registration +6 points, turnout +2.6 points**; "roughly **40 percent**
  of those who are induced to register in the classroom go on to vote."
- **Bennion and Nickerson e-mail study**, **250,000+ students at twenty-six
  colleges**, varying sender, length, personalization and number of e-mails: "all
  led to the same dismal conclusion" — **registration rates declined by 0.3
  points**. "you can talk to students in person about registration and get them to
  fill out a form right then and there, but if you set them down the path of doing
  so online, **they get lost in some sort of vortex due to confusion or
  procrastination.**"
- **Text reminders** to those who visited Rock the Vote's online system and opted
  into updates, 2008: **+4 points** on registration.
- **Nickerson's six site-based/canvassing evaluations**, 2004–07 — Denver, Memphis,
  Louisville, Detroit, Tampa, Kalamazoo; presidential, gubernatorial and mayoral.
  Streets randomized; canvassers knocked every door at least twice, contacting
  one-third to one-half of residents, providing and collecting forms. The
  researcher **prevented the organization from recontacting the newly registered**,
  so turnout effects are attributable to registration itself. Four results:
  **ten new registrants per street** (about one per sixteen households); those ten
  translated into **2.6 additional votes**; new registrants per street were **about
  five times greater on low-income streets**; but **20 percent of new registrants
  voted in low-income areas versus 50 percent in high-income areas**. Net:
  "registration drives produce **twice as many votes on low-income streets** as on
  high-income streets."
- **The microtargeted Texas failure, October 2005.** A pro-Republican group slated
  **300,000+ unregistered adults** for a first-class mailing with a note from a
  little girl urging registration, plus a robocall from the same girl to those with
  listed numbers; targets selected by microtargeting algorithms using income,
  magazine subscriptions and precinct voting patterns. ~1,600 precincts treatment,
  250 control. **Expenditure exceeded $474,000 (2022 dollars) and produced just 713
  new registrants.** (A similar program a year earlier "achieved a kind of mythic
  status based on its reputed success, although no rigorous evaluation had been
  conducted.")
- **Birthday mailings**, thirteen states before 2008, to young people identified as
  likely ethnic/racial minorities: registration **+1.5 points**, voting **+0.4** —
  both distinguishable from zero, implying **each additional registration generates
  0.26 votes**. Registration packets to minority movers across county lines, 2008
  and 2010: registration **+1.8**, turnout **+1** — **0.55 votes per new
  registrant**. Two 2010 follow-ups: +1.2 registration with no turnout gain; +2.8
  registration with +1.7 turnout. "**All told, approximately 0.4 additional votes
  are generated for every newly registered minority mover.**"
- **State agency mailings** (Mann and Bryant with Oregon and Delaware). Letters
  reading "Our records indicate you may be eligible to vote, but do not appear to
  be registered to vote." Delaware 2012: registration **6.8 → 9 percent**, turnout
  **6.8 → 8.8 percent** — "**almost everyone who became registered due to the
  treatment went on to vote in the presidential election.**" Oregon 2014:
  registration **4.6 → 6.8**, turnout **2 → 3.7 percent** (about three-quarters
  voted). Pennsylvania 2016, **2.2 million+ eligible but unregistered**: **+1 point**
  registration, and again almost everyone induced to register voted.
- **Vote.org texting**, 512,083 unregistered nonwhites under forty and unmarried
  women, 341,400 control, four messages: registration **+0.3 points**, turnout
  **+0.3 points** — smaller than the state mailings, "but it appears that everyone
  induced to register also went on to vote."

### Lessons Learned, with star ratings (p. 149)

| Rating | Lesson |
|---|---|
| ★★★ | **Mass e-mail is a poor method for generating new registrants** — regardless of number, format, or sender. |
| ★★★ | **Personal appeals that accompany the distribution and collection of forms increase registration rates.** |
| ★★ | **Generic mailings, automated calls, radio messages, and web advertisements have little effect on registration rates.** |
| ★★★ | **Mailings sent by government agencies raise registration rates significantly** in both midterm and presidential years, "with the biggest turnout effects coming in presidential elections." |
| ★★ | **Mailing registration materials to those turning eighteen or to recent movers increases registration** — "on the order of **one new registrant per sixty-six mailings**." |
| ★★★ | **Registering the unregistered increases turnout.** Pooling several experiments, **one new vote per three or four new registrants** — but the translation "varies markedly across elections and the socioeconomic status of those encouraged to register. Registration in advance of a presidential election yields the most votes." |

### Cost-effectiveness (p. 150) — four more prices absent from Table 12-1

- **House-to-house search in low-income neighborhoods:** one new registrant per
  hour; at **$24 per canvassing hour** and **one in four new registrants voting**,
  **each vote costs $96**.
- **Site-based at community centers, churches, high schools** in the same
  neighborhoods: three or more registrants per hour → **$32 per vote or less**.
- **Mailings to eighteen-year-olds:** +0.5 points on the average recipient;
  addressed to individuals, so **two hundred mailings per vote**; at $0.75 apiece,
  **$150 per vote**.
- **Mailings to movers:** addressable to households, bringing it to **about $100
  per vote** — "not particularly attractive compared to other GOTV tactics."

**Conclusion (p. 150):** "**For many people, registration is not an impediment to
voting because they won't vote even if registered; for others, registration leads
to turnout.**" Both sides of the long-running debate are "partly right."

*For the lab:* registration is a whole category of vote-buying that Table 12-1
omits entirely, with prices spanning **$32 to $150** — straddling most of the
table. And the low-income/high-income split (five times the registrants, 20 versus
50 percent of them voting, netting twice the votes) is the most direct evidence in
the book on the brief's question of whether mobilization widens or narrows
participation gaps. It cuts *against* the brief's current framing: on these
numbers registration drives are more productive in poor neighborhoods, not less.

---

## Chapter 11. Strategies for Effective Messaging (pp. 151–166)

**The chapter's turn (p. 151).** "In previous chapters, we organized our discussion
around the **channels** through which your campaign might communicate with voters —
door-to-door canvassing, phone calls, mail, and so forth. In this chapter, we
consider **what you might say** to voters once you have reached them. **What kinds
of appeals get people to vote?**" Answers draw on social psychology and behavioral
economics. The chapter covers, in order: visualizing and planning; social pressure;
and other strategies such as thanking voters or reminding them of a pledge — with
prototypical messages reproduced "which you can adapt to suit your own campaign's
needs."

*For the lab:* this is the book's own statement that **channel and message are
separate axes**, and Table 12-1 varies only the first. Every row prices a channel
carrying a conventional message; chapter 11 shows the same channel can differ by a
factor of four or more depending on what it says (conventional mail ~0.4 points
versus "Neighbors" at 8). The table's unit of analysis — the tactic — holds message
constant without saying so.

### Self-prophecy and implementation intentions (pp. 152–54)

**Self-prophecy** — asking people whether they intend to vote. "The experimental
literature started off with a bang": Greenwald and colleagues, before the 1984
presidential election, asked a few dozen college undergraduates by phone whether
they intended to vote — **turnout rose 25 percentage points**. "**Unfortunately,
that whopping effect has never been reproduced in any subsequent study.**" The
original authors found much weaker effects in 1986 and 1987. Later work suggests
self-prophecy effects, "if they exist at all, have approximately the same modest
effect as a GOTV call from a commercial phone bank." Smith and colleagues, 2000
presidential primary: control **43.4 percent**, self-prophecy group **43.3**. Cho,
2008 primary: **+2.2 points**. Nickerson and Rogers: **+2 points**. Gerber, Hill
and Huber, 2010 midterms: **+2 points**. But the effect weakens when the question
is part of a longer conversation — Mann found *Washington Post* pre-election polls
had **no effect on turnout**, and canvassers who merely ask *how* residents intend
to vote produce no increase.

**Implementation intentions / plan making.** Rogers and Ternovski's 2010 mailer
included a checkbox: "Voting takes a plan" — "What time will you vote?" and "How
will you get to your polling place?" The phone version and Cho's "repeat after me"
commitment script are both reproduced. Results are mixed: **Cho found turnout
*declined* slightly, 59.1 → 58.9 percent**; **Nickerson and Rogers found +4.1
points**; **Gerber, Hill and Huber found +0.6**; a nonpartisan mailer combining
plan making with gratitude in competitive 2010 House districts gave **+0.5
points**. Asking for *reasons* for voting (Smith and colleagues) produced no gains.

Summary: "turnout does not rise much when people are encouraged to predict whether
they will vote, visualize the process by which they will vote, or explain why
voting is a worthwhile endeavor."

### Social pressure (pp. 155–62)

Definition: communications playing on the drive to win praise and avoid
chastisement. Three ingredients — **admonish** the receiver to adhere to a norm,
indicate compliance will be **monitored**, and warn it will be **disclosed**.

**Mark Grebner, a campaign consultant — not an academic — invented the tactic**,
and his own explanation is quoted at length: he never met a voter who needed
reminding, but noticed the large fraction who falsely claimed to have voted. "Ten
days didn't seem like enough time to have genuinely forgotten; it seemed more like
intentional lying. … voters weren't unaware of the message, but deliberately
evading it. I decided to try to test that model, by seeing if the threat of public
exposure would force at least some of them to abandon their pose."

"After reading the first edition of this book, Grebner brought his in-house
experiments to our attention," and the authors collaborated on a **four-mailer
experiment across 80,000 households** in a low-salience Michigan primary:
1. **Civic Duty** — admonition only. → **+1.8 points** (replicated in municipal
   elections a year later).
2. **Hawthorne** — adds surveillance: you are part of an academic study and your
   participation will be monitored.
3. **Self** — reports whether each voter in the household voted recently, promises
   an updated mailing. → **+4 to 5 points**.
4. **Neighbors** — adds whether others on the block voted. → **more than 8
   percentage points**, "an astonishingly large effect that eclipses even the
   effect of face-to-face contact with a canvasser."

For comparison: "a single piece of direct mail, partisan or nonpartisan, rarely
increases turnout by more than 1 percentage point, and the effect is usually much
weaker."

**Box 11-1 reproduces the "Self" mailer in full**, including the Homer/Marge
Simpson specimen chart with "Did Not Vote" and "Voted" and a blank column for the
coming election.

**The hazard, restated (p. 157).** "Like the early airplane, Grebner's invention
was both intriguing and hazardous." The second edition warned readers off it,
predicting "Your phone will ring off the hook with calls from people demanding an
explanation." When "Neighbors" was used in the 2012 Wisconsin gubernatorial
recall, **thousands of complaints were received — roughly one for every three
hundred households targeted — and journalists swarmed the office of the
organization that had sent the mail.**

**Table 11-1. The Effects of the "Self" Mailer across Multiple Studies (p. 158)**

Entries are percent voting; parentheses are Ns; * = significant at p < .01,
one-tailed.

| # | Election | Setting | Softened language? | Control | Self | % increase |
|---|---|---|---|---|---|---|
| 1 | 2006 August primary | Michigan | No | 29.7 (191,243) | 34.5 (38,218) | 16%* |
| 2 | 2007 municipal | Michigan | No | 27.7 (772,479) | 32.4 (27,609) | 17%* |
| 3 | 2007 gubernatorial general (previous nonvoters) | Kentucky | Yes | 6.8 (19,561) | 8.9 (13,689) | 31%* |
| 3 | 2007 gubernatorial general (previous voters) | Kentucky | Yes | 13.2 (25,037) | 16.3 (17,731) | 23%* |
| 4 | 2009 municipal special | New York City | No | 3.1 (3,445) | 4.2 (3,486) | 36%* |
| 5 | 2010 general | Texas | Yes | 40.5 (63,531) | 43.1 (1,200) | 6% |
| 5 | 2010 general | Wisconsin | Yes | 49.0 (43,797) | 50.8 (801) | 4% |
| 6 | 2011 municipal | California | No | 10.6 (13,482) | 12.0 (1,000) | 13% |
| 7 | 2014 midterm | 17 states | Yes | 31.2 (128,008) | 31.9 (1,969,899) | 2% |

Sources are printed in full (Gerber/Green/Larimer *APSR* 2008 and *Political
Behavior* 2010; Mann *Political Behavior* 2010; Abrajano and Panagopoulos
*American Politics Research* 2011; Matland and Murray *PRQ* 2013; Panagopoulos,
Larimer and Condon *Political Behavior* 2014; Gerber and others, unpublished
manuscript, 2016).

*Note for the lab:* **percentages here are relative increases, not percentage
points** — the table's own note says so, and the 2 percent in row 7 is 0.7 points.
This is a clean example of a units trap in a published table, and the caption is
the only thing that prevents misreading. Note too that **item 7 is an unpublished
manuscript covering 1.96 million citizens** — the largest study in the table is
not in a journal, which supports the authors' claim to reach past the published
record.

**Softening it.** Three tested approaches:
- Dial back the neighbors element, keep the recipient's own record. The **16
  percent increase from 2006 replicates**. But softened variants are weaker,
  "especially in the context of closely contested elections": a 2014 experiment in
  seventeen states replaced scolding with "a helpful summary of how often you vote
  and how your participation compares with others in your state" — **+0.7 points,
  or 2 percent**. Offering a ride to the polls instead of scolding, or inviting the
  recipient to explain why they do not vote, produce "sizable and statistically
  significant effects but weaker than those of the stern 'Self' mailer."
  Grebner's own ominous variant ("when you skip an election, we worry that it could
  become a habit — a bad habit we want you to break. We'll be looking for you at
  the polls Tuesday") raised 2010 Michigan turnout **24 → 28.5 percent**.
- **Honor Roll** — positive framing, 2009 New Jersey gubernatorial, targeting
  unmarried women, African American and Hispanic voters, presenting ten neighbors'
  perfect vote histories. **+2.3 points among African Americans and Hispanics,
  +1.3 among women** (the first two distinguishable from zero). Grebner's 2022
  version *without* any "Self" content managed only **+0.2 points**, "suggesting
  that the key ingredient is information about the recipient's own voting record."
- **Implied future accountability** — Rogers and Ternovski's "You may be called
  after the election to discuss your experience at the polls" added **a quarter of
  a percentage point**. DellaVigna's door hangers ("researchers will contact you
  within three weeks of the Election … to conduct a survey on your voter
  participation") gave **+1.4 points in 2010 but no increase in 2012**.

**Three stated limits on social pressure (pp. 161–62):**
1. **Far fewer votes in high-salience elections.** The "Neighbors" mailing in the
   contested Wisconsin recall, on a list averaging **65.4 percent** base turnout,
   raised turnout **just 1 point** — versus **3.3 points** among those at roughly
   30 percent base rate, and **essentially no effect above 60 percent**. "the
   people who might be moved by the 'Neighbors' mailing in a low-salience election
   are already voting in a high-salience election."
2. **Hit or miss by phone or text.** A script telling Election Day non-voters their
   failure had been noticed by poll watchers "fared no better than a conventional
   GOTV appeal," though recorded messages scolding voters have tended to work.
   Texting warnings that "your friends will know whether you voted" were not
   especially influential.
3. **Falls flat when partisan argumentation predominates.** A "Self" mailing that
   presented vote history and then enumerated "Republican misdeeds" produced **no
   apparent increase** — "the text left recipients thinking that this was just
   another advocacy mailer."

### Gratitude (pp. 162–63)

Panagopoulos's thank-you mailer (Box 11-2 reproduces the Vote Georgia Project
letter in full; source: *Journal of Politics* 73 (2013): 707–17) raised turnout in
**three different electoral settings, with effects approximately two-thirds as
large as the "Self" mailer's**.

The unexpected detail: **when different thank-you mailers are compared head to
head, they all work — even when recipients are praised merely for being concerned
about public affairs, with no mention of their voting record.** "Evidently, the
gratitude effect is distinct from the effects of social pressure."

Recommendation: campaigns seeking "an effective, unobjectionable, and inexpensive
GOTV message should consider some version of the thank-you mailer" — it needs no
database management or customization, "and the sender may even win some points for
politely thanking voters." Caveat: "if everyone starts thanking voters, recipients
will quickly become inured."

### Pledges and descriptive norms (p. 164)

**Pledges work.** Costa and colleagues, 2016 primary and general: Environmental
Defense Fund campus organizers spent four-hour shifts either encouraging pledges
to vote or encouraging people to request a reminder. Roughly equal compliance and
similar background attributes. The pledge group was mailed a **pledge reminder**;
the control a **nonspecific reminder**. **+4.1 points in the primary, +3.5 in the
general.** "One could readily imagine much stronger versions of this effect in
settings where people pledge to vote in the context of community gatherings."

**Descriptive norms do not.** Telling people "everyone else is voting" — framing
the election as likely to attract high or low turnout — "has little effect. Those
who receive either message vote at similar rates."

### Conclusion (pp. 165–66)

The **"door-in-the-face"** hypothesis fails: McCabe and Michelson's phone bank
either made a straight GOTV appeal or first invited the voter to work an
eight-hour Election Day phone shift (**just one of 543 people agreed**). The
straight appeal significantly increased turnout; **the door-in-the-face appeal
performed significantly worse than the standard message.**

The organizing distinction:
- **Useful:** hypotheses rooted in **prescriptive** social norms. "Turnout rises
  when this norm is asserted forcefully and when compliance with the norm is
  subject to monitoring and possible disclosure to others." Consistent with survey
  evidence that voters regard voting as something citizens ought to do, and with
  experimental evidence that "all else being equal, people who vote are more highly
  regarded than those who do not."
- **Limited practical value:** interventions that merely increase the cognitive
  accessibility of the election — reminders and the like.
- **In between:** predicting, reasoning, plan making — "sometimes reveal a sizable
  effect, but the results have been mixed."

**And a warning against composite messages:** "too many cooks may spoil the broth
… A committee of chefs might decide to add a dash of social pressure, a dash of
gratitude, and a dash of implementation intentions. The problem is that those on
the receiving end … may simply ignore a jumble of messages. … it may be better to
narrow the presentation to a single message that is brief and memorable."

*For the lab:* chapter 11 is where the brief's "mobilization or coercion" question
gets its fullest answer, and the answer is more textured than either the brief or
Table 12-1 suggests. The eight-point effect is real, replicated, and **invented by
a campaign consultant rather than a researcher**; the authors collaborated with him
after he wrote to them about the first edition. It carries a measured complaint
rate of one in three hundred, it stops working in exactly the high-salience
elections campaigns care most about, and the book's practical recommendation is to
use **gratitude** instead — two-thirds the effect, no database customization, no
complaints. That last point matters for the lab's cost argument: gratitude mail is
the **$46-per-vote** row from chapter 5, cheaper than anything in the file except
the corrected $45 volunteer phone figure, and it is absent from Table 12-1
entirely.

---

## Chapter 12. What Works, What Doesn't, and What's Next (pp. 167–188)

### The chapter opens on publication bias (pp. 167–68)

This matters for the brief, which currently introduces publication bias as an
external critique supported by Gerber/Green/Nickerson (2001) and Rosenthal (1979).
**The authors open their own concluding chapter with it, unprompted:**

> "When we began our experimental research in 1998, we were struck by the fact
> that even people running very expensive campaigns were operating on little more
> than their own intuition about what worked. The academic literature in existence
> at that time was little help. Experimental studies were rare, and the ones that
> found their way into print reported what we now know to be **outlandish
> findings. One study, based on a few dozen voters, purported to show that partisan
> mail increased turnout by 19 percentage points.**"
>
> "Looking back, it seems clear that these sensational findings can be attributed
> to something called '**publication bias.**' Academic journals are reluctant to
> publish statistically insignificant findings, which means that **smaller studies
> must report larger results if they are to find their way into print. As a result,
> the experimental studies that appear in academic journals tend to give a
> misleading picture of what works.**"

So the compilation is not a passive inheritor of publication bias — **it is
explicitly presented as a corrective to it**, which is consistent with the
appendix A policy of gleaning results "whether published or unpublished" and with
the p. 4 claim to incorporate every study in the public domain. The brief's
current sentence — "which means the surviving estimates are, if anything, the
optimistic ones" — is the reasonable prior for a literature compilation in
general, but it is the *opposite* of what these compilers say they were doing. The
honest version has more tension in it and is more interesting: the authors
identified the bias, built the table to counter it, and the reader still cannot
verify how well they succeeded.

A second red-herring example follows (voter guides): focus groups found people
*reported* increased enthusiasm; survey research found people in areas with voter
guides were more likely to vote even controlling for age and education, "leading
the authors to recommend that states routinely mail voters sample ballots."
**Tested experimentally, no effect on turnout was found.**

**What does not work, as summarized on p. 168:**
- Mobilizing is not merely reminding. Commercial phone reminders, e-mail, Facebook,
  mail and text reminders all have weak effects.
- Mobilizing is not putting information in front of people. Leafleting and voter
  guide mail "have produced disappointing results."
- Telling people why to vote for a candidate or cause "does not, in itself, lead
  people to vote at higher rates. Putting a partisan edge on a mailing or a phone
  call does not seem to enhance its effectiveness."

**What does (pp. 168–69):**
- **"To mobilize voters, make them feel wanted at the polls. Mobilizing voters is
  like inviting them to a social occasion. Personal invitations convey the most
  warmth and work best."** Next best are conversational phone calls; "mailed
  invitations typically don't work very well."
- Build on preexisting motivation — recontacting someone who pledged to vote.
- Many nonvoters think of themselves as voters and feel voting is a civic
  obligation; "they will vote if urged to do their civic duty and shown that
  whether they vote is a matter of public record."

> "the decision to vote is strongly shaped by one's social environment. One may be
> able to nudge turnout upward slightly by making voting more convenient, supplying
> voters with information, and reminding them about an imminent election; these
> effects, however, are small in comparison to what happens when voters are placed
> in a social milieu that urges their participation. That said, **providing social
> inducements to vote is neither easy nor cheap.**"

### The asterisk rule, stated by the authors (p. 169)

> "In constructing our dollars-per-vote estimates, **we err on the side of caution
> and provide figures only for those tactics that have been shown to work.** In
> other words, table 12-1 reports the cost-effectiveness of tactics whose average
> impact has been demonstrated to be greater than zero. Tactics such as television
> advertising, whose average mobilizing effect has yet to be distinguished
> statistically from zero, are excluded from this segment of the table.
> **Interested readers may look back at previous chapters for our speculations
> about the cost-effectiveness of these unproven tactics.**"

**This corrects the framing I used in the chapter 4 and chapter 9 notes below.**
The chapter-level prices ($69 leaflets, $36 signage, $140 billboards, $22
television, $11–23 radio, $46 gratitude mail, $32–150 registration) are **not
suppressed** — the authors say plainly that they exist and tell the reader where to
find them. The blank cells are a deliberate, disclosed editorial rule, and the
book signposts the workaround in the same paragraph that applies it.

That makes the point for the lab cleaner rather than weaker. **The information loss
happens downstream of the book, not inside it.** The book says "look back at
previous chapters"; the transcription into `gotv_tactics.csv` kept the table and
dropped the chapters, so the pointer was lost and the asterisk became `NA`. This is
the most precise available statement of what the lab's file is: **a summary table
severed from the apparatus that made it readable.** The brief's existing insight —
that the risk moved rather than disappeared when the numbers were hand-keyed — is
exactly right, and this is the concrete instance.

### Scale, stated bluntly (p. 170)

> "**Canvassing one hundred registered voters at their doorsteps will not generate
> one hundred votes. A more realistic estimate is four additional votes among those
> you speak to and perhaps an extra one or two if word spreads to others in the
> household.** Even this figure may be high if the canvassing targets are very
> likely or very unlikely voters. Similarly, addressing a piece of conventional
> GOTV mail to ten thousand registered voters will not bring ten thousand people to
> the polls. The number of additional voters you should expect from this type of
> direct mail campaign is around **thirty-eight**."

Note the arithmetic: **four votes per hundred contacts is one vote per twenty-five**
— the appendix A pooled figure, not the seventeen printed in Table 12-1. The prose
and the table disagree, and the prose is the conservative one. (Mail checks out
exactly: 10,000 ÷ 260 = 38.5.)

Also: "even if your efforts were twice as effective as the most effective
door-to-door canvassing campaign, you would still need to contact several thousand
registered voters to produce one thousand votes. **To sway election outcomes,
high-quality campaigns must also be high-quantity campaigns.**"

### Mobilizing over the long haul — votes have a residual value (pp. 170–71)

Mobilization has **enduring effects**. The New Haven residents randomly assigned to
mail or canvassing in 1998 were more likely to vote in November 1998 *and* in the
November 1999 mayoral election. Replicated many times: the Michigan social pressure
subjects voted at higher rates in the August 2006 primary **and** significantly more
in the August primaries of 2008, 2010 and 2012. **The "Self" mailing generated
approximately 1,850 votes in August 2006, plus an additional 900 votes over the
next three August primaries.** The pattern holds for other large social pressure
studies, García Bedolla and Michelson's multielection minority mobilization work,
and Ternovski's nine canvassing studies.

Carryover depends on the elections: "boosting turnout in primary elections has a
bigger effect on subsequent primary elections than on subsequent general
elections, suggesting that voters get into the habit of voting in particular types
of elections or at certain times of the year."

Two implications, both stated:

1. **Voting is habit-forming.** "America's low turnout rates may reflect the fact
   that we have the most frequent elections on earth. One might liken sleepy
   municipal elections to **gateway drugs**: by enticing so many people to abstain
   from voting, they weaken voting habits."
2. **Cost per vote is overstated by ignoring the future.** "If your campaign
   generates one thousand additional votes at a cost of $40,000, this price amounts
   to **$40 per vote** for the current election. But if your campaign generates an
   extra four hundred votes in future elections, the price falls to $40,000/1,400 =
   **$29 per vote.**"

*For the lab:* **every cost-per-vote figure in Table 12-1 is a single-election
price, and the book says on the facing page that the true price is roughly 30
percent lower** once carryover is counted — while noting the carryover rate itself
varies by election type. So the file's `cost_per_vote` column has an unstated time
horizon of exactly one election. That is a missing column of a different kind from
the missing standard error: not uncertainty, but scope. It also gives the brief's
budget exercise a second, better question — a field director spending $250,000 is
buying votes in this election *and* a fraction of a vote in each subsequent one,
and the tactics differ in how much of that residual they produce.

### Table 12-1. Cost-Effectiveness of Get-Out-the-Vote Tactics (pp. 172–73)

Columns as printed: *GOTV effort · Start-up and overhead costs · Ongoing
management · Effectiveness per contact · Is effect statistically reliable? ·
Dollar cost per vote (excluding start-up and management costs)*.

**The table has twelve rows.** The lab's `gotv_tactics.csv` currently carries
eight of them.

| GOTV effort | Start-up / overhead | Ongoing management | Effectiveness per contact | Statistically reliable? | Dollar cost per vote |
|---|---|---|---|---|---|
| Door-to-door | Recruit, prepare walk lists | Substantial ongoing training and supervision | One vote per 17 contacts, plus effects of spillover on housemates | Yes, large number of studies | At $24 per hour and six contacts per hour, one vote costs **$57** |
| Leafleting | Recruit, prepare walk lists and leaflets | Monitor walkers, check work | One vote per **189** voters reached by leaflets | **Not significantly greater than zero** | * |
| Direct mail, advocacy | Design, print, distribute | Intensive during start-up, then postal service takes over | **No detectable effect** | Yes, large number of studies | * |
| Direct mail, nonpartisan (conventional message)ᶜ | Design, print, distribute | Intensive during start-up, then postal service takes over | One vote per 260 recipients (unconventional messages tend to be more productive) | Yes, large number of studies | At $0.75 per piece and 1.5 recipients per household, one vote costs **$130** |
| Phone, volunteer | Recruit enthusiastic callers | Ongoing training and supervision | One vote per 36 contacts | Yes, large number of studies | At $20 an hour and **16 contacts per hour**, one vote costs **$45** |
| Commercial live calls | Obtain phone list | Requires monitoring to ensure quality | One vote per 106 contacts | Yes, large number of studies | At $1 per contact, one vote costs **$106** |
| Robocalls | Obtain phone list, recording talent | Due diligence to check legal requirements | One vote per 425 landlines targeted, **without social pressure messages** | **Yes, large number of studies** | At $0.15 per targeted number for a series of three calls, one vote costs **$64** |
| E-mail | Amass e-mail list, compose message(s), distribute | Most of the work is in the start-up | Few detectable effects, except when sent by registrar | Large number of studies show average effect cannot be large | * |
| Text messages | Amass target list, compose message(s), distribute | One-to-one messaging requires large staff of volunteers | One vote per **381** voters targeted | Yes, several large studies, including five in presidential elections | Lists and texting services average $0.35 per target number, implying **$133 per vote** |
| Election festivals | Find site, organize event, advertise | Requires staff on hand to host and supervise events | Raises precinct-wide turnout by **1 percentage point**, more for early voting sites | Results vary widely across seven studies, two of which focus on early voting | Assuming precincts of 2,700 voters and $2,300 per festival, cost per vote is **$85** for festivals on Election Day and roughly **$40** for festivals at early voting sites |
| Television GOTV | Produce and place ads | None | Raises turnout by **0.5 percentage points** | Not significantly greater than zero | * |
| Radio GOTV | Produce and place ads | None | Raises turnout by **1 percentage point** | Not significantly greater than zero | * |

**Footnotes, verbatim:**

> a. Costs may vary due to local circumstances and market conditions.
>
> b. "Contact" is defined as follows: for door-to-door canvassing, talking to
> target voter; for commercial and volunteer phone calls, talking to target
> voter; for robocalls, attempting to reach a target voter (since robocalls
> typically leave voicemail); for mail, mail sent; for leaflets, leaflet dropped
> at door. For direct mail, leafleting, and door-to-door canvassing,
> calculations assume that the average household has 1.5 voters. No further
> within-household spillover effects are assumed to apply to direct mail. For
> canvassing, it is assumed that nonpartisan messaging is directed at voters
> whose baseline probability of voting is between 30 and 50 percent and that 40
> percent of the effect on the directly contacted spills over to housemates.
> Across all canvassing studies, the average cost per vote is approximately $83,
> including spillovers. See appendix A. For festivals, TV, and radio, contact
> means targeting a precinct or media market.
>
> c. Unconventional messages, such as those that apply social pressure, are
> often substantially more effective. See chapter 11.
>
> * Cost-effectiveness is not calculated for tactics that are not proven to
> raise turnout.

### What this settles for the lab

**The robocalls flag is faithful to the book.** The brief currently says the row
is "currently unaudited" and that the literature generally finds no effect. The
book's own reliability column reads *"Yes, large number of studies"* — so
`effective = TRUE` transcribes the source correctly. The qualification is in the
effectiveness cell, not the reliability cell: *one vote per 425 landlines
targeted, **without social pressure messages***. So the disagreement, if there is
one, is between the book and the wider literature, not between the file and the
book. The brief's paragraph needs rewriting on that basis.

**One transcription error.** Volunteer phone calls are **$45** per vote in the
book; `gotv_tactics.csv` has `46`. Every derived figure in the brief that uses
the cheapest rate — votes bought, the crossover, the break-even wage — is
computed off the wrong number.

**The contact-rate assumption is in the book.** The brief assumes `CALLS <- 25`
completed conversations an hour and flags it as an assumption rather than a
measurement. The book states **16 contacts per hour** for volunteer phones (and
six per hour for canvassing). The lab can use the source's own figure.

**The cost definition exists.** The brief says a cost column arrives "with no
definition of cost attached to it" and that "the file, as you have it, contains
no such definition." Footnote b is that definition — what counts as a contact for
each tactic, the 1.5-voters-per-household assumption, the 30–50 percent baseline
turnout window for canvassing targets, and the 40 percent within-household
spillover. This is the strongest single addition available to the lab.

**There are two canvassing cost-per-vote figures.** $57 is the headline row;
**$83** is the average across all canvassing studies *including spillovers*
(footnote b, pointing to appendix A). The lab currently carries only $57.

**Leafleting has a number.** The book prints *one vote per 189 voters reached*
alongside *not significantly greater than zero*. The CSV records `NA`. That is
defensible, but it is a decision — the point estimate exists and was dropped,
which is a sharper version of the argument the brief already makes about
blank cells.

**Four tactics are missing from the file entirely:** text messages, election
festivals, television GOTV, radio GOTV. Two of them (festivals, at $85 and ~$40)
are *cheaper per vote than anything currently in the lab's table* — the file's
cheapest row is volunteer phones at $45, but festivals at early voting sites come
in around $40. Adding them changes the ranking the brief is built around.

**Two rows are not per-contact at all.** Festivals, TV and radio are measured as
percentage-point shifts in precinct or media-market turnout, not votes per
contact — footnote b says "contact means targeting a precinct or media market."
So the table mixes two units of analysis in one column, which the eight-row
version of the file conceals.

### Synergy? (p. 174)

The claim tested is not "more is better" — a mailer plus a call obviously beats
either alone. The claim is that recipients of mail are *especially* responsive to
the ensuing call: if mail lifts turnout 1 point and phone 2 points, an
"integrated" campaign should deliver more than 3.

**The results decisively reject the synergy hypothesis.** Cited: 1998 New Haven
(mail did not increase phone effectiveness; phone did not enhance canvassing);
2002 NALEO (robocalls did not enhance mail, or vice versa; neither mail nor
robocalls amplified live calls); 2006 Asian American mobilization (nonpartisan
live calls + mail); California Latino neighborhoods (live calls, robocalls, mail,
canvassing); three environmental-organization studies in 2004 and 2006. Partial
exceptions: Emily Cardy's gubernatorial mail-plus-phone study showed
statistically insignificant signs of synergy, and a 2014 study of calls and mail
targeting Republican women showed hints — but a much larger 2005 study of
partisan mail, phone and canvassing in another gubernatorial contest found none.

> "experimental researchers searched long and hard for the El Dorado of synergy
> only to conclude that combinations of GOTV appeals do not appear to deliver a
> bonus of votes."

p. 175 gives the mechanism: mail rarely creates a personal sense of belonging, so
the later call has nothing to build on. **This is why the book is organized one
tactic per chapter rather than by combination** — which retroactively justifies
the structure of Table 12-1, and is worth knowing before treating its rows as
independently additive.

*For the lab:* the brief's budget arithmetic is linear and additive across
tactics. That is not a simplification the lab imposed — it is the book's own
finding. Worth saying so.

### Frontiers: the "when" and "who" questions (pp. 175–77)

**Timing ("when") is murky.** It takes a very large experiment to separate early
from late effects. Panagopoulos, 2005 Rochester municipal: calls at four weeks,
two weeks, three days — all raised turnout slightly, but only 2,000 per arm, so
the variation gives little guidance. Two larger nonpartisan experiments (2008,
2010), 15,000 households to early calls (a week out) and 15,000 to late (one to
two days out): 2008 gave a slight edge to late, both weak and statistically
indistinguishable; 2010 found both equally ineffective. A large 2014 study gave
the edge to *early* calls. Mail: social pressure mailings eight vs. four days out
in 2010 favored later, just shy of significance; Broockman and Green's 2014
evaluation of a union mail campaign found four-weeks-out ineffective, nineteen
days slightly better than twelve. Text messaging: early ≈ late, no special payoff
for Election Day urgency. Registration by mail (2016) and radio (2020), early vs.
late September: no clear winner.

> "these experiments hint that late communications tend to work better, but the
> many exceptions to this rule suggest that the role of timing is subtle or
> context dependent."

**Targeting ("who").** The rule of thumb: it is easier to generate votes when
targeting voters whose baseline probability of voting falls **roughly between 20
and 80 percent**. (Note this is the general rule; footnote b to Table 12-1 uses a
narrower **30 to 50 percent** window for the canvassing cost calculation
specifically.) Beyond the rule of thumb, researchers now apply data-mining
methods with cross-validation to find responsive subgroups.

Worked illustration (p. 177): a data-mining program called **BART** was trained on
the 2007 Michigan social pressure experiment ("Self" mailer) using variables
common to it and the 2009 Illinois study — gender, age, household size, turnout
in the 2004 and 2006 primary and general elections — then used to partition the
2009 subject pool. Had the 2009 mailer gone only to those predicted to be
especially responsive, the average lift would have been **5.5 percentage points,
versus 2 points among those predicted to be less responsive**. Since mailing cost
is identical for both groups, targeting alone changes cost per vote profoundly.

*For the lab:* this is the sharpest available answer to the brief's question
about whether mobilization narrows or widens participation gaps. It shows the
price in Table 12-1 is not a property of the tactic — it is a property of the
tactic *and* the list. The same mailer costs radically different amounts per vote
depending on who receives it, and the table has no column for that.

### In search of supertreatments; relational organizing (pp. 177–80)

Supertreatments are unlikely to come from combining conventional treatments (see
synergy). They come from harnessing social-psychological forces — the canonical
case being that **a single mailing can produce an increase of 8 percentage points
in turnout by revealing a person's past voting record to the neighbors.**

**Relational organizing** — mobilization by people who already know the target:

- **PICO, 2016 presidential.** Organizers compiled voters they had prior contact
  with; randomized to relational calls (from the organizer with a personal
  connection), paid-staff live calls, or control. Relational calls: **4.2 points
  above control, 2 points above conventional live calls.**
- **Turnout Nation, 2019 municipal.** Forty-four volunteer "captains" mobilizing
  friends, family, co-workers, classmates; evaluated by Green and McClellan, each
  captain's target list randomized. Control (N = 386) turnout **33 percent**;
  treatment (N = 387) **13.2 points higher** — "a massive effect … estimated with
  a relatively small margin of error." A 2021 follow-up again showed the captain
  model highly effective.
- **But it does not scale.** PICO planned 200 organizers and settled for 77. When
  Turnout Nation scaled for the 2022 midterms by *hiring* paid volunteers, ninety-six
  captains produced a treatment effect of just **1.3 points** over a 31 percent
  control — paid captains had many low-propensity friends but were less motivated.
  A 2023 attempt using community-college students compensated with course credit
  showed **no apparent effect**.
- **Cohen and Green, 2020 general.** Fifty volunteer captains recruited by
  classmates at colleges and high schools were so diligent that some mobilized
  friends assigned to control. Control voted at **84.8 percent — 2.2 points
  higher than treatment**. Sixteen of the fifty captains had 100 percent turnout
  in both arms.

The lesson stated: highly motivated volunteers are extremely effective and hard
to come by; the magic is not replicated by captains who are compensated. The
scaling challenge is to attract committed volunteers whose networks contain
*unlikely* voters — volunteers tend to be high-propensity people whose friends
are high-propensity too.

*For the lab:* this is a second, independent confirmation of the brief's
"cheapest tactic is not for sale" argument — and a stronger one, because it is
the book's own evidence that motivation cannot be purchased. Turnout Nation's
13.2 points collapsing to 1.3 points the moment the captains were paid is a
cleaner demonstration than the volunteer-hours model the brief currently builds.
The Cohen and Green study is also a rare published case of **control-group
contamination**, which is a teachable failure mode in its own right.

### Box 12-1. Conducting Your Own Experiment (p. 179)

Points readers to a free step-by-step guide to running GOTV experiments, and to
the authors' two textbooks: *Social Science Experiments: A Hands-on Introduction*
(no statistics background needed) and *Field Experiments: Design, Analysis, and
Interpretation*.

### Strategies for increasing electoral participation (pp. 181–84)

Baseline: about two-thirds of the eligible electorate votes in presidential
elections, roughly half in federal midterms; municipal and special elections
"more often than not attract less than one-third of eligible voters."

Three categories of proposal, all treated skeptically:

1. **Constitutional overhaul** (proportional representation, direct electoral
   control over policy, consolidating all elections to once every two years).
   Implausible; New Zealand's switch to PR "calls this assumption into question."
   Consolidating the election calendar is the most realistic of these, but it is
   no accident that jurisdictions hold elections at odd times — that is a
   calculated move by parties and interest groups to diminish national tides.
2. **Procedural changes** (online ballots, three-week voting periods, universal
   vote-by-mail, automatic national registration, same-day registration). Track
   record "mixed": same-day registration in Idaho, Maine, Minnesota, New
   Hampshire, Wisconsin and Wyoming was associated with **only modest gains**;
   vote-by-mail and early in-person voting boosted turnout "only to a small
   extent"; Motor Voter and automatic registration had **small positive effects**.
   "These innovations seem to nudge turnout upward, but not markedly so."
3. **Voter education** (hotlines, information websites, candidate debates, better
   journalism). "Unlikely to increase voter turnout appreciably" — debates attract
   "appreciative but tiny audiences," public interest websites little traffic.
   "Politics does not interest most people."

The authors' own perspective is different: find out which tactics are effective
and cost-efficient, and the market for campaign services will eventually
reallocate. **Emphasis on *eventually*.** Campaign managers are justifiably
skeptical of research, and there is a conflict of interest — managers sometimes
profit from services they sell or expect ongoing subcontractor relationships,
so they can "have their cake and eat it too by running 'credible' campaigns whose
activities make for handsome profits."

*For the lab:* this is the book's own account of **why the price list exists and
who it is aimed at**. It is a market-correction device pointed at an industry with
a financial incentive to ignore it. That is a direct, citable answer to the
brief's question "who decided what went into it, and why."

### Box 12-2. Further reading on turnout in the U.S. (p. 181)

Anzia, *Timing and Turnout* (Chicago, 2013); Campbell, *Why We Vote* (Princeton,
2006); Highton, "Voter Registration and Turnout in the United States,"
*Perspectives on Politics* 2 (2004): 507–15; Leighley and Nagler, *Who Votes Now?*
(Princeton, 2013); Piven and Cloward, *Why Americans Still Don't Vote* (Beacon,
2000); Rosenstone and Hansen, *Mobilization, Participation, and Democracy in
America* (Macmillan, 1993).

### Cost-effectiveness of turnout versus persuasion (pp. 184–87)

The book's experiments measure **only whether people vote, not how they vote.**
The arithmetic of the distinction (Box 12-3): persuading a voter who would
otherwise vote against you changes the margin by **two votes**; mobilizing a
nonvoter who supports you changes it by **one**. Gains from mobilization are
diluted further if the campaign is unsure the nonvoter truly supports it. So "if
persuasion were half as easy as mobilization, it might be more attractive."

On the evidence, from Kalla and Broockman's systematic review, three points:

1. In general elections, **the average effect of persuasion on candidate choice is
   very close to zero** — TV ads, digital ads, direct mail alike. Even messages
   shown to work early in the season cease to work down the stretch.
2. Persuasion **does** affect vote preference on **ballot measures and races where
   party labels are absent** — voters know less, so information and argument help
   form preferences.
3. Persuasive effects **can** be amplified by careful field testing before launch,
   but campaigns seldom do this, deferring instead to consultants who "rarely
   conduct rigorous field tests."

Why the persuasion literature lags: the secret ballot. Researchers must randomize
at precinct level (where vote choice is counted in aggregate) or run a
postelection survey. Most campaigns will do neither, so much of the literature is
unpublished.

Conclusion: mobilizing supporters "may turn out to be the most dependable and
cost-effective way to influence elections, especially when one takes into account
the fact that each additional voter will cast votes for many offices and ballot
measures."

*For the lab:* the brief already cites Kalla and Broockman (2018) for near-zero
persuasion effects. The book adds the *asymmetry* — one vote versus two — which
is the piece that makes the comparison arithmetic rather than rhetorical, and the
ballot-measure exception, which the brief does not currently mention.

### Box 12-3. Generating Votes: Mobilization versus Persuasion (p. 185)

A worked hypothetical, useful as a lab exercise in its own right. Republican
candidate, 8,000 registered voters:

| Intent | Voters: reg. Republicans | Voters: others | Nonvoters: reg. Republicans | Nonvoters: others |
|---|---|---|---|---|
| Intend to vote for you | 800 | 650 | 800 | 1,300 |
| Intend to vote for your opponent | 200 | 1,350 | 200 | 2,700 |

Setup: 2,000 registered Republicans, 80 percent favor you, half normally vote.
The other 6,000 favor your opponent 67.5 to 32.5, and one-third vote. You lose
1,450 to 1,550 and need at least 100 additional votes.

- **GOTV route:** identify the 2,100 abstainers who favor you and turn out 100.
  Voter ID programs use brief polls to find them, which costs planning and money.
- **Simpler GOTV route:** target Republicans indiscriminately — but you must
  mobilize **at least 167**, because you gain only **60 net votes per 100
  Republicans mobilized** (80 for, 20 against).
- **Persuasion route:** converting 50 of the 1,550 opposing voters makes it a dead
  heat — conversions "rapidly close the margin."

### Scientific frontiers (pp. 187–88)

Field experimentation as a measurement technology, analogous to the telescope and
microscope: for the first time social scientists can measure the causal effect of
a treatment in a real-world setting, which constrains theorizing that was
previously "largely unconstrained by hard facts."

Parting example: the moral-foundations hypothesis — that care/fairness appeals
mobilize liberals and loyalty/authority/sanctity appeals mobilize conservatives.
Murray and Matland ran direct mail experiments in Texas and Wisconsin doing
exactly that and **found no support whatsoever**. Moral arguments "worked equally
well, or badly, regardless of who received them." Is the theory wrong, are moral
arguments diluted by mail, or did the mail reach the wrong targets because
liberals and conservatives are hard to identify in the voter file?

> "When tests are conducted under real-world conditions, politics is a humbling
> laboratory from which few theories emerge unscathed."

*Still to read: pp. 167–171 (opening of chapter 12).*

---

## Appendix A. Meta-Analysis of Door-to-Door Canvassing Experiments (p. 189)

**This is the per-row provenance the brief says does not exist.** Table A-1 lists
the results of *each distinct study* in which door-to-door canvassing was
evaluated experimentally.

Method, as stated on p. 189:

- Entries "were in most cases gleaned from the results presented by the research
  teams, whether published or unpublished" — so the appendix is **not** restricted
  to published work, which bears directly on the brief's publication-bias section.
- In some cases the authors re-did the analysis to account for methodological
  nuances such as **clustered assignment**.
- A "distinct study" means a study in a given election year involving a single
  organization. Families of experiments (e.g. the dozens of PICO canvassing
  experiments reported in García Bedolla and Michelson's *Mobilizing Inclusion*
  for November 2008) are **combined into a single weighted average, weights being
  the inverse of each experiment's squared standard error.**
- Where an experiment tested two or more versions of canvassing (different
  scripts), all treatment arms are **combined into a single treatment**.

### Table A-1. Results of Door-to-Door Canvassing Experiments (pp. 190–91)

Columns: *Context · Study · CACE · SE · Control turnout · Advocacy?*

Notes as printed: Context is election year and type — **G = general, M =
municipal, P = primary, PP = presidential primary, R = runoff**. **CACE = the
estimated complier average causal effect** (the average treatment effect among
compliers). **SE = standard error of the estimated CACE.** Advocacy refers to
appeals that urge support for candidates or causes. Organizations: AACU =
African-American Churches United; CARECEN = Central American Resource Center;
CCAEJ = Center for Community Action and Environmental Justice; PICO = People
Improving Communities through Organizing; SCOPE = Strategic Concepts in
Organizing and Policy Education.

| Context | Study | CACE | SE | Control turnout | Advocacy? |
|---|---|---|---|---|---|
| 1998G | Gerber & Green (New Haven) | 8.4 | 2.6 | 30% to 50% | |
| 2000G | Green & Gerber (OR) | 8.4 | 4.5 | 50% to 70% | |
| 2001G | Green et al. (Bridgeport) | 14.4 | 5.3 | Under 30% | |
| 2001G | Green et al. (Columbus) | 9.7 | 7.9 | Under 30% | |
| 2001G | Green et al. (Detroit) | 7.8 | 4.5 | 30% to 50% | |
| 2001G | Green et al. (Minneapolis) | 10.1 | 8.7 | Under 30% | |
| 2001G | Green et al. (Raleigh) | 0.2 | 3.2 | Under 30% | |
| 2001G | Green et al. (St. Paul) | 14.4 | 6.4 | 30% to 50% | |
| 2001G | Michelson (Dos Palos) | 4.1 | 2.2 | Under 30% | |
| 2002G | Bennion (IN) | 0.6 | 5.1 | 30% to 50% | |
| 2002G | Gillespie (St. Louis) | 0.8 | 1.0 | Under 30% | |
| 2002G | Michelson (Fresno) | 3.5 | 1.6 | Under 30% | |
| 2002G | Nickerson et al. (MI) | 16.8 | 15.9 | 30% to 50% | X |
| 2002M | Gillespie (Newark) | −7.9 | 27.9 | 30% to 50% | X |
| 2002P | Nickerson (Denver) | 8.6 | 4.2 | 30% to 50% | |
| 2002P | Nickerson (Minneapolis) | 10.9 | 4.1 | Under 30% | |
| 2002R | Gillespie (Newark) | 1.2 | 7.3 | Under 30% | X |
| 2003G | Arceneaux (Kansas City) | 7.0 | 3.9 | Under 30% | X |
| 2003G | Michelson (Phoenix) | 12.9 | 1.8 | Under 30% | X |
| 2004G | LeVan (Bakersfield) | 24.2 | 7.5 | 30% to 50% | |
| 2004G | Matland & Murray (TX) | 7.4 | 4.3 | 30% to 50% | |
| 2005G | Anonymous (VA) | 3.5 | 2.4 | 30% to 50% | X |
| 2005G | Nickerson (VA) | 27 | 15.4 | Under 30% | X |
| 2006G | Bedolla & Michelson (AACU) | −3.4 | 8.1 | 30% to 50% | |
| 2006G | Bedolla & Michelson (CARECEN) | −0.5 | 2.9 | 50% to 70% | |
| 2006G | Bedolla & Michelson (CCAEJ) | 4.4 | 5.9 | 50% to 70% | |
| 2006G | Bedolla & Michelson (PICO) | 3.1 | 3.9 | 30% to 50% | |
| 2006G | Bedolla & Michelson (SCOPE) | 6.6 | 2.1 | 30% to 50% | |
| 2006G | Nickerson (Dearborn) | 8.7 | 3.8 | 30% to 50% | |
| 2006G | Nickerson (Grand Rapids) | −0.4 | 4.3 | Under 30% | X |
| 2006P | Bedolla & Michelson (CARECEN) | 2.2 | 1.8 | Under 30% | |
| 2006P | Bedolla & Michelson (CCAEJ) | 43.1 | 12.5 | Under 30% | |
| 2006P | Bedolla & Michelson (SCOPE) | 2.6 | 3.3 | Under 30% | |
| 2007G | Davenport (Boston) | 13.4 | 7.0 | Under 30% | |
| 2007P | Bedolla & Michelson (AACU) | −1.4 | 2 | Under 30% | |
| 2008G | Arceneaux et al. (CA) | 10.7 | 10.2 | Above 70% | |
| 2008G | Bedolla & Michelson (CARECEN) | 0.7 | 6.0 | 50% to 70% | |
| 2008G | Bedolla & Michelson (CCAEJ) | 0.3 | 4.5 | 50% to 70% | |
| 2008G | Bedolla & Michelson (PICO) | 1.2 | 1.7 | 50% to 70% | |
| 2008G | Bedolla & Michelson (SCOPE) | 0.5 | 1.1 | Above 70% | |
| 2008P | Bedolla & Michelson (CARECEN) | 4.0 | 2.6 | Under 30% | |
| 2008P | Bedolla & Michelson (CCAEJ) | 3.9 | 2.8 | Under 30% | |
| 2008P | Bedolla & Michelson (PICO) | 1.0 | 1.3 | Under 30% | |
| 2008PP | Bedolla & Michelson (CARECEN) | 0.9 | 3.2 | 50% to 70% | |
| 2008PP | Bedolla & Michelson (PICO) | 9.0 | 3.4 | 30% to 50% | |
| 2008PP | Bedolla & Michelson (SCOPE) | 3.4 | 2.3 | 50% to 70% | |
| 2008P | Bailey et al. (WI) | 1.5 | 2.0 | 50% to 70% | X |
| 2010G | Barton et al. (Midwest) | −7.7 | 3.8 | 50% to 70% | X |
| 2010G | Bryant (San Francisco) | −32.9 | 21.6 | 50% to 70% | |
| 2010G | Cann et al. (UT) | 8.2 | 4.6 | 50% to 70% | |
| 2010G | Hill & Lachelier (FL) | 1.8 | 9.3 | Under 30% | |
| 2014R | Green et al. (TX) | 3.1 | 1.8 | 30% to 50% | |
| 2015M | Michelson (WA) | 11.0 | 5.6 | Under 30% | X |
| 2016P | Michelson (WA) | 1.0 | 2.9 | Above 70% | X |
| 2016G | Kalla & Broockman (NC) | 2.3 | 1.0 | 50% to 70% | X |
| 2016P | Broockman & Green (AZ) | 2.6 | 1.7 | 50% to 70% | |
| 2021M | Markovits (MA) | 4 | 3.5 | Under 30% | |
| 2021M | Markovits (MN) | −1.8 | 2.2 | Above 70% | |
| 2021M | Markovits (PA) | 4.8 | 2.5 | 30% to 50% | |

### How the CACEs were estimated (p. 192)

CACE is the effect among **compliers** — those reachable if assigned to
treatment. Because these experiments have **one-sided noncompliance** (some
assigned to treatment go untreated, but nobody in control inadvertently receives
treatment), the estimates also describe the average treatment effect among those
actually treated.

Estimation: (1) difference between the voting rate in the *assigned* treatment
group and the *assigned* control group, (2) divided by the proportion of the
treatment group contacted by canvassers. **No control variables** were used
(except, in some instances, block indicators marking randomization blocks) — "the
most straightforward reading of the results," avoiding biases from post hoc
adjustment.

Exclusions, stated: studies assigning the treatment group to a *package* that
includes canvassing (Alvarez and colleagues evaluated a campaign combining phone,
e-mail and mail with canvassing), and early studies that **did not measure contact
rates** (Middleton's evaluation of Election Day canvassing in battleground urban
areas, 2004 presidential).

Summarized with a **random effects model**, allowing effects to vary by type of
canvasser, target population, or electoral context.

> **For all studies combined, the average estimate is 4.0, with a 95 percent
> confidence interval from 2.8 to 5.2. That estimate implies one vote per
> twenty-five contacts.**

### Table A-2. Meta-Analysis Results by Base Rate of Turnout (p. 193)

95 percent confidence intervals in brackets; number of distinct experiments in
parentheses.

| Turnout rate in the control group | All canvassing: average CACE | Nonadvocacy canvassing only: average CACE |
|---|---|---|
| Under 30% | 5.1 [2.8, 7.5] (23) | 3.7 [1.7, 5.8] (17) |
| 30% to 50% | **6.0** [4.2, 7.8] (17) | **6.4** [4.4, 8.5] (14) |
| 50% to 70% | 1.8 [0.4, 3.3] (14) | 2.2 [0.5, 3.9] (11) |
| 70% and higher | 0.2 [−1.6, 2.1] (4) | 0.1 [−1.8, 2.1] (4) |

Effects are weaker when turnout is especially low *or* high. The nonadvocacy
figure peaks at **6.4 points among the fourteen studies targeting populations
whose turnout was between 30 and 50 percent**; estimated CACEs are "less than
half as large" for the thirty-one nonpartisan studies aimed at higher- or
lower-turnout populations.

> "A CACE of 6.0 implies that seventeen contacts are required to produce one
> vote. Note that this contact-per-vote figure does not take into account
> possible within-household spillover."

### What Appendix A does to the lab's central argument

This is the most consequential thing in the book for the brief, and it cuts
against the chapter as currently written in one place and strongly for it in
another.

**The "17 contacts" figure is conditional, and the book says so.** Table 12-1
prints *one vote per 17 contacts* as though it were the canvassing effect. It is
not the average. **Averaged over all 59 studies the CACE is 4.0, which implies one
vote per twenty-five contacts.** The 17 comes from a CACE of 6.0 — the estimate
for target populations whose baseline turnout is between 30 and 50 percent. So
the headline number in the lab's data file is the effect *in the most favorable
targeting band*, and the entire cost-per-vote column inherits that. A campaign
canvassing an average list is buying votes at roughly 25 contacts each, not 17.

**The uncertainty exists, and it is published.** The brief's strongest claim is
that "nothing carries an interval … the file has no column in which that could
have been written down, which is the most consequential thing about it and the
one thing no amount of careful handling downstream can repair." That is true of
the *file*, and it remains a fair criticism of Table 12-1 — but it is not true of
the *book*. Appendix A publishes a standard error for every one of the 59
canvassing studies and a 95 percent confidence interval for every pooled
estimate. The interval was not unavailable; it was **dropped at the transcription
step**, which is a sharper and more teachable claim than "the source has no
uncertainty in it." The brief needs rewording here, and it gets a better argument
in exchange: this is a documented case of uncertainty surviving in the appendix
and vanishing from the summary table that everyone actually uses.

**The variation across studies is enormous and now visible.** Individual CACEs
run from **−32.9 to +43.1**. Standard errors run from 1.0 to 27.9. Eight of the
59 estimates are negative. The brief currently says "how far apart their answers
were … are not recorded anywhere in it" — for canvassing, they are recorded, two
pages later.

**Publication bias looks different now.** The brief argues the surviving
estimates are "if anything, the optimistic ones," citing Gerber, Green and
Nickerson (2001). Appendix A states the entries were gleaned from research teams
"whether published or unpublished" — so the authors deliberately reached past the
published record. That does not eliminate the file-drawer problem, but it means
the compilation was built against it, and the brief should say so rather than
treating the compilation as a passive inheritor of publication bias.

**Two exclusion rules exist and are stated** — package treatments, and studies
that did not measure contact rates. The brief says "what decided whether an
experiment counted?" is "not recorded." For canvassing, it is recorded on p. 192.

**The $57 / $83 gap is now explicable.** $57 is computed from 17 contacts per
vote (the 30–50 percent band, no spillover). $83 is the average cost per vote
across all canvassing studies *including* spillovers (footnote b to Table 12-1).
The two numbers differ both in which studies they average and in whether
housemates count.

---

## Appendix B. Meta-Analysis of Direct Mail Experiments (pp. 195 onward)

Same method as Appendix A, stated in the same terms: entries "gleaned from the
results presented by the research teams, **whether published or unpublished**";
analysis updated for methodological nuances such as clustered assignment; "distinct
study" = a study in a given election year involving a single organization; families
of experiments combined into a weighted average using inverse-squared-standard-error
weights (the worked example is García Bedolla and Michelson's eight PICO mail
experiments from November 2006, reported in *Mobilizing Inclusion*). **"In cases
where the experimental treatment comprised multiple mailings, the entry in the table
reflects the increase in turnout per mailer."** Package treatments are excluded (the
Alvarez campaign combining phone, e-mail and canvassing with mail). **No control
variables** except block indicators.

Mailers are categorized as **social pressure** versus **conventional**, and
conventional is subdivided into **nonpartisan** and **advocacy**.

### The pooled estimates, with confidence intervals (p. 196)

**A total of 130 distinct studies** provides the database. Random effects
meta-analysis, per mailer:

| Category | Average effect (points per mailer) | 95% CI | Studies |
|---|---|---|---|
| Nonpartisan, no social pressure | **0.541** | 0.301 to 0.781 | 80 |
| Nonpartisan, excluding gratitude, pledge reminders and the "Civic Duty" mailer | **0.384** | 0.117 to 0.650 | 65 |
| Advocacy, no social pressure | **0.085** | **−0.065 to 0.235** | — |
| Social pressure (pooled) | **1.843** | 1.289 to 2.397 | — |

### Table B-1. Results of Direct Mailing Experiments (p. 197 onward)

Columns: *Context · Study · Average treatment effect · SE · Advocacy? · Social
pressure?* Context codes as in Appendix A (G general, M municipal, P primary,
PP presidential primary, **S special**).

First page of the table, transcribed (the table continues past p. 197 — remaining
rows not yet read):

| Context | Study | Effect | SE | Advocacy? | Social pressure? |
|---|---|---|---|---|---|
| 1998G | Gerber & Green (New Haven) | 0.5 | 0.3 | | |
| 1999G | Gerber & Green (New Haven) | 0.3 | 0.2 | | |
| 1999G | Gerber et al. (CT and NJ) | 0 | 0.1 | X | |
| 2000G | Green (NAACP) | 0 | 0.5 | X | |
| 2002G | Ramirez (NALEO) | 0.1 | 0.1 | | |
| 2002G | Wong (Los Angeles County) | 1.3 | 1 | | |
| 2002M | Gillespie (Newark) | −1.1 | 2.5 | X | |
| 2002P | Cardy (PA) | −0.2 | 0.5 | X | |
| 2002P | Gerber (PA) | −0.1 | 0.3 | X | |
| 2002S | Gillespie (Newark) | −1.6 | 2 | X | |
| 2003M | Niven (West Palm Beach) | 1.4 | 2.1 | | |
| 2004G | Anonymous (MN) | −0.9 | 0.7 | | |
| 2004G | Matland & Murray (Brownsville) | 2.9 | 1.1 | | |
| 2004G | Trivedi (Queens County) | 1.1 | 1.7 | | |
| 2005G | Anonymous (VA) | 0 | 0.1 | X | |
| 2006G | Barabas et al. (FL) | 0.3 | 0.6 | | |
| 2006G | Bedolla & Michelson (APALC) | 1.1 | 0.5 | | |
| 2006G | Bedolla & Michelson (OCAPICA) | −0.5 | 0.8 | | |
| 2006G | Bedolla & Michelson (PICO) | −3.2 | 1 | | |
| 2006G | Gray & Potter (Franklin County) | −2.9 | 2.7 | X | |
| 2006G | Mann (MO) | −0.1 | 0 | X | |
| 2006G | Anonymous (MD) | −0.4 | 0.3 | X | |
| 2006P | Bedolla & Michelson (APALC) | 0 | 0.3 | | |
| 2006P | Bedolla & Michelson (PICO) | 1.1 | 0.8 | | |
| 2006P | Gerber et al. (MI) | 1.8 | 0.3 | | |
| 2006P | Gerber et al. (MI) | 5.2 | 0.2 | | X |
| 2007G | Gerber et al. (MI) | 1.8 | 0.9 | | |
| 2007G | Gerber et al. (MI) | 5.1 | 0.5 | | X |
| 2007G | Mann (KY) | 2.7 | 0.2 | | X |
| 2007G | Panagopoulos (Gilroy) | −0.3 | 1.4 | | |
| 2007G | Panagopoulos (IA and MI) | 2.2 | 0.8 | | X |
| 2008G | Keane & Nickerson (CO) | −0.7 | 0.3 | | |
| 2008G | Nickerson (APIAVote) | −1.2 | 0.6 | | |
| 2008G | Nickerson (FRESC) | −0.2 | 0.7 | | |
| 2008G | Nickerson (Latina Initiative) | 0.2 | 0.3 | | |
| 2008G | Nickerson (NCL) | 1.5 | 0.6 | | |
| 2008G | Nickerson (Voto Latino) | −0.6 | 0.3 | | |
| 2008G | Rogers & Middleton (OR) | −0.1 | 0.5 | X | |
| 2008P | Enos (Los Angeles County) | 2 | 1.1 | | |
| 2008PP | Barabas et al. (FL) | −2.7 | 0.6 | | |
| 2008PP | Nickerson & White (NC) | 0.8 | 0.7 | | |
| 2008PP | Nickerson & White (NC) | 1 | 0.3 | | X |
| 2009G | Larimer & Condon (Cedar Falls) | 0.7 | 2.4 | | X |
| 2009G | Mann (Houston) | 1.2 | 0.6 | | |
| 2009G | Panagopoulos (NJ) | 2.5 | 0.5 | | |
| 2009G | Panagopoulos (NJ) | 2 | 0.5 | | X |
| 2009S | Abrajano & Panagopoulos (Queens) | 1.1 | 0.4 | | X |
| 2009S | Mann (Houston) | 1.1 | 0.5 | | |
| 2009S | Panagopoulos (Staten Island) | 2 | 1 | | |
| 2009S | Sinclair et al. (Chicago) | 4.4 | 0.6 | | X |
| 2010G | Anonymous (NV) | 0.2 | 0.5 | | X |
| 2010G | Barton et al. (unknown state) | −2.2 | 1.6 | X | |
| 2010G | Bryant (San Francisco) | 1.7 | 2 | | |
| 2010G | Gerber et al. (CT) | 2 | 0.5 | X | |
| 2010G | Gerber et al. (CT) | 0.4 | 0.6 | | |
| 2010G | Gerber et al. (CT) | 0.9 | 0.2 | | |
| 2010G | Herrnson et al. (MD) | 0.1 | 0.3 | | |
| 2010G | Mann & Mayhew (ME) | 1 | 0.4 | | |
| 2010G | Mann & Mayhew (ID, MD, NC, and OH) | 2 | 0.4 | | |
| 2010G | Murray & Matland (TX and WI) | 1.7 | 0.7 | | |
| 2010G | Murray & Matland (TX and WI) | 1.5 | 0.7 | | X |
| 2010G | Rogers et al. (17 states) | 0.6 | 0.1 | | |
| 2010M | Panagopoulos (Lancaster) | −1.1 | 1 | | |
| 2010P | Binder et al. (San Bernardino County) | −0.1 | 0.5 | X | |
| 2010P | Binder et al. (CA) | −0.1 | 0.5 | | |
| 2010P | Panagopoulos (GA) | 2.5 | 0.6 | | |
| 2011G | Mann & Kalla (ME) | 2.4 | 0.6 | | |
| 2011G | Panagopoulos (Lexington) | 1 | 0.8 | | |
| 2011G | Panagopoulos et al. (Hawthorne) | −0.4 | 0.7 | | |
| 2011G | Panagopoulos et al. (Hawthorne) | 2.2 | 0.6 | | X |
| 2011M | Panagopoulos (Key West) | 1.1 | 0.5 | | X |
| 2011M | Panagopoulos (Key West) | −0.1 | 0.4 | | |
| 2011S | Mann (NV) | 0.9 | 0.3 | | |
| 2011S | Panagopoulos (Charlestown) | −0.3 | 0.5 | | |
| 2012G | Citrin et al. (VA and TN) | 0.7 | 0.4 | | |
| 2012G | Doherty & Adler (battleground state) | 0.1 | 0.2 | X | |
| 2012G | Levine & Mann (GA and OH) | 0.2 | 0.3 | | |
| 2012G | Mann & Bryant (DE) | 2 | 0.2 | | |
| 2012G | Mann et al. (FL) | 0.0 | 0.0 | | |
| 2012M | Panagopoulos (VA) | 0 | 0.6 | | |
| 2012P | Condon et al. (IA) | 2.8 | 0.6 | | X |
| 2012P | Condon et al. (IA) | 0.4 | 0.9 | X | |
| 2012P | Condon et al. (IA) | 2.7 | 0.9 | | |
| 2012P | Shi (NC) | −0.7 | 0.5 | X | |
| 2012R | Gerber et al. (WI) | 1.1 | 0.7 | | |
| 2012R | Rogers et al. (WI) | 1 | 0.3 | | X |
| 2013G | Biggers (VA) | 0.1 | 0.2 | | |
| 2013G | Connors et al. (CO) | 0.9 | 0.2 | | |
| 2013G | Connors et al. (CO) | 0.8 | 0.5 | | |
| 2013G | Matland & Murray (TX, OH) | 0.4 | 0.5 | | |
| 2013G | Matland & Murray (TX, OH) | 0.8 | 0.4 | | X |
| 2013G | Matland & Murray (MN, OH, TX, and VA) | 0.4 | 0.3 | | |
| 2013M | Huber et al. (CT) | 1.6 | 0.4 | | |
| 2013M | Matland & Murray (El Paso) | 0.1 | 0.4 | | |
| 2013M | Murray & Matland (WI and TX) | 0.4 | 0.2 | | |
| 2014G | Broockman & Green (CA) | 0.3 | 0.1 | X | |
| 2014G | Cubbison (NC) | −0.1 | 0.1 | X | |
| 2014G | Gerber et al. (17 states) | 0.7 | 0.1 | | X |
| 2014G | Gerber et al. (MS) | 3.4 | 0.4 | | X |
| 2014G | Gerber et al. (AR, FL, GA, KS, MA, MI, and WI) | 0.4 | 0.1 | | |
| 2014G | Gerber et al. (AK, GA, LA, MI, NC, and TX) | 0.8 | 0.2 | | X |
| 2014G | Gerber et al. (13 States) | 0.2 | 0.1 | | |
| 2014G | Green & Zelizer (NH) | 0.1 | 0.05 | | |
| 2014G | Mann & Bryant (OR) | 1.7 | 0.005 | | |
| 2014G | Panagopoulos & Bailey (PA) | 0.9 | 0.8 | X | |
| 2014P | Green et al. (TX) | 0.1 | 0.5 | X | |
| 2014P | Hill & Kousser (CA) | 0.5 | 0.1 | | |
| 2014P | Hughes et al. (CA [information]) | 0.5 | 0.1 | | |
| 2014P | Hughes et al. (CA [partisan]) | 0.5 | 0.1 | X | |
| 2015G | Gerber et al. (KY, LA, MS, TX) | −0.6 | 1.6 | | |
| 2015G | Mann et al. (NJ, VA) | 2.2 | 0.1 | | X |
| 2015M | Michelson (WA) | 6.4 | 1.6 | X | |
| 2016G | Mann & Fischer (NC) | 1.3 | 0.5 | | X |
| 2016G | Mann et al. (NC) | 1.0 | 0.4 | | X |
| 2016G | Costa et al. (CO) | 3.5 | 1.3 | | |
| 2016P | Bryant et al. (PA) | 0.9 | 0.03 | | |
| 2016P | Costa et al. (PA) | 4.5 | 2.2 | | |
| 2016P | Sweeney (IL) | 1.7 | 1.1 | | X |
| 2016S | Hassell (MI) | 0.4 | 0.8 | X | |
| 2017G | Endres & Panagopoulos (VA) | 0 | 0.5 | | |
| 2018G | ideas42 (MN) | 0.3 | 0.1 | | |
| 2018G | ideas42 (MN) | 0.5 | 0.2 | | X |
| 2018G | Mann (NC) | 1.6 | 0.5 | | X |
| 2018G | Schumacher (NY) | 0.4 | 0.8 | | |
| 2019G | Miller and Judd (VA) | 0.5 | 0.03 | | |
| 2019M | Hopkins et al. (PA) | 0.4 | 0.1 | | |
| 2019P | Hopkins et al. (PA) | 0.3 | 0.1 | | |
| 2020G | Biggers (CA) | 0.03 | 0.09 | | |
| 2020G | Chiao (22 States) | 0.4 | 0.1 | | X |
| 2020G | Doleac et al. (NC) | −0.03 | 0.2 | | |
| 2022G | Grebner (MI) | 0.3 | 0.3 | | X |

Notes as printed: context is election year and type, **G = general, M = municipal,
P = primary, PP = presidential primary, R = runoff** (and **S = special**, used in
the table though not listed in the note). **"SE = standard error of the estimated
CACE."** Advocacy = appeals urging support for candidates or causes; social
pressure = appeals emphasizing compliance with the social norm of civic
participation. Organizations: APALC = Asian Pacific American Legal Center;
APIAVote = Asian and Pacific Islander American Vote; FRESC = Front Range Economic
Strategy Center; NAACP; NALEO = National Association of Latino Elected Officials;
NCL = National Council of La Raza; OCAPICA = Orange County Asian and Pacific
Islander Community Alliance; PICO; SEIU = Service Employees International Union.

*(Two oddities worth noting if the lab quotes this table. The note calls the SE
column a standard error "of the estimated CACE," but mail has no noncompliance to
speak of — everyone assigned a mailer is sent one — so these are effectively
intent-to-treat estimates; the wording looks carried over from Appendix A. And the
reported precision is inconsistent: most SEs are one decimal place, but Mann &
Bryant (OR) 2014 is given as 0.005 and Bryant et al. (PA) 2016 and Miller and Judd
(VA) 2019 as 0.03, alongside a point estimate of 0.0 with SE 0.0 for Mann et al.
(FL) 2012.)*

### What Appendix B adds for the lab

**It confirms the Appendix A pattern on a second tactic, and strengthens it.** The
brief's claim that the source carries no uncertainty is now contradicted twice:
canvassing has 59 studies with standard errors and pooled CIs, mail has **130**.
Between them the two appendices cover the two tactics the lab leans on hardest.

**The advocacy-mail row is the cleanest teaching case in either appendix.** Table
12-1 records advocacy mail as "No detectable effect" and the CSV records it as
`effective = FALSE` with `NA` for both numeric columns. **Appendix B gives it a
number: 0.085 percentage points, CI −0.065 to 0.235.** That is a point estimate
that is positive, small, and straddling zero — three facts, of which the file
preserves none. `FALSE` and `NA` are doing the work of an interval that the book
published.

**And the nonpartisan mail figure in Table 12-1 is a choice among two.** The
appendix reports **0.541** points per mailer across 80 studies, and **0.384** across
the 65 that exclude gratitude, pledge reminders and the soft "Civic Duty" mailer.
The 0.384 is what feeds the "one vote per 260 recipients" in Table 12-1 (1/0.00384
≈ 260). So the headline mail row is the **restricted** estimate — deliberately
conservative, with the more inclusive figure being about 40 percent larger. That is
a documented, principled analytic choice, invisible in the file, and it is exactly
the kind of decision the brief's "What was decided" section is built to surface —
except that here the decision was the *authors'*, not the transcriber's.

---

## Appendix C. Meta-Analysis of Phone-Call Experiments (pp. 201–204)

Same method again. **Table C-1** columns: *Context · Study · CACE for live calls,
ITT for robo · SE · Robo · Volunteer · Professional*. The worked example for
combining families of studies is García Bedolla and Michelson's six OCAPICA
experiments from November 2006.

**The estimand differs by call type, and the appendix is explicit about it:**

> "For phone bank studies involving live callers, the estimates reported in table
> C-1 gauge the treatment effect among compliers … the CACE … **For studies
> involving robocalls, table entries represent intent-to-treat estimates, and no
> adjustment is made for contact rates** (which tend to be very high, as they
> include both direct contacts and voicemail). The one exception is Kling's study
> of the 2016 election, which did not leave messages and had a contact rate of
> approximately 39 percent. To estimate the CACE for this study, divide the ITT
> estimates provided in the table by 0.39."

CACEs computed as in Appendix A: difference in voting rates between assigned
groups, divided by the proportion of the treatment group contacted by callers. No
control variables except block indicators. Package treatments excluded (Alvarez et
al. again).

### Table C-1. Results of Phone-Call Experiments (pp. 202–203)

Effect column is **CACE for live calls, ITT for robocalls**. Last three columns
mark the type of call.

| Context | Study | Effect | SE | Robo | Volunteer | Professional |
|---|---|---|---|---|---|---|
| 1998G | Gerber & Green (New Haven) | −1.9 | 2.4 | | | X |
| 1998G | Gerber & Green (West Haven) | −0.5 | 2 | | | X |
| 2000G | Green (NAACP) | 2.3 | 2.3 | | | X |
| 2000G | Green & Gerber (Youth Vote 2000) | 4.9 | 1.7 | | X | |
| 2000G | Nickerson (Youth Vote) | 2.3 | 2.5 | | X | |
| 2001G | Nickerson (Seattle) | −0.6 | 0.9 | X | | |
| 2002G | Gerber & Green (IA and MI) | 0.4 | 0.5 | | | X |
| 2002G | McNulty (Cal Dems) | −8.5 | 6 | | X | |
| 2002G | McNulty (No on D) | 0.5 | 2.6 | | | X |
| 2002G | McNulty (Youth Vote) | 12.9 | 5.5 | | X | |
| 2002G | Nickerson (Youth Vote Coalition) | 0.5 | 0.6 | | X | |
| 2002G | Nickerson (Youth Vote Coalition) | 3.2 | 0.7 | | | X |
| 2002G | Nickerson et al. (MI) | 3.2 | 1.7 | | X | |
| 2002G | Ramirez (NALEO) | 4.6 | 1.8 | | X | |
| 2002G | Ramirez (NALEO) | 0 | 0.2 | X | | |
| 2002G | Wong (Los Angeles County) | 2.3 | 2.4 | | X | |
| 2002P | Green (PA) | −0.1 | 5.5 | | | X |
| 2003G | Michelson et al. (NJ) | 10.5 | 3.6 | | X | |
| 2003G | Nickerson (MI) | 1.4 | 0.9 | | X | |
| 2003S | McNulty (Cal Dems) | −5.3 | 6.2 | | X | |
| 2004G | Arceneaux et al. (IL) | 2 | 1.2 | | | X |
| 2004G | Green & Karlan (MO and NC) | 0 | 0.3 | X | | |
| 2004G | Ha & Karlan (MO and NC) | 0.8 | 0.6 | | | X |
| 2005G | Panagopoulos (Albany) | 0.1 | 1.3 | | | X |
| 2005G | Panagopoulos (Rochester) | 0.9 | 1.1 | | | X |
| 2006G | Barabas et al. (FL) | −3 | 8 | | X | |
| 2006G | Bedolla & Michelson (APALC) | 5.3 | 2.4 | | X | |
| 2006G | Bedolla & Michelson (APALC) | 3.4 | 1.7 | | X | |
| 2006G | Bedolla & Michelson (NALEO) | 0.7 | 1.5 | | X | |
| 2006G | Bedolla & Michelson (OCAPICA) | 2.8 | 1.9 | | X | |
| 2006G | Bedolla & Michelson (PICO) | −1 | 2.7 | | X | |
| 2006G | Michelson et al. (Los Angeles County) | 9.3 | 3.2 | | X | |
| 2006P | Bedolla & Michelson (APALC) | 2.7 | 1.5 | | X | |
| 2006P | Bedolla & Michelson (NALEO) | 2.1 | 2.4 | | X | |
| 2006P | Shaw et al. (TX) | 0.4 | 0.3 | X | | |
| 2006S | Middleton (CA) | 3.9 | 1.2 | | X | |
| 2008G | Bedolla & Michelson (NALEO) | −1.2 | 2.2 | | X | |
| 2008G | Gerber et al. (ME, MO, and NJ) | 0.1 | 0.6 | | | X |
| 2008P | Bedolla & Michelson (OCAPICA) | 11.1 | 2.1 | | X | |
| 2008P | Bedolla & Michelson (PICO) | −1.9 | 3 | | X | |
| 2008P,G | Green (MI) | 1.0 | 0.3 | X* | | |
| 2008PP | Nickerson & Rogers (PA) | 2.8 | 1.3 | | | X |
| 2009G | Green et al. (IA and MI) | −0.1 | 1.7 | | X | |
| 2010G | Bryant (San Francisco) | −7 | 8.5 | | X | |
| 2010G | Gerber et al. (CA, IA, and NV) | 0.1 | 0.6 | | | X |
| 2010G | Gerber et al. (CO, CT, and FL) | 1.3 | 0.7 | | | X |
| 2010G | Mann & Klofstad (IL, MI, NY, and PA) | 0.4 | 0.3 | | | X |
| 2010G | Mann & Klofstad (FL, IA, IL, ME, MI, MN, NM, NY, OH, PA, and SC) | 0.6 | 0.2 | | | X |
| 2011G | Mann & Kalla (ME) | 7 | 5.8 | | | X |
| 2011G | McCabe & Michelson (San Mateo County) | 8.4 | 5.1 | | X | |
| 2013G | Collins et al. (VA) | 7.8 | 4.3 | | X | |
| 2013G | Mann & Lebron (WA) | 1.7 | 1.7 | | | X |
| 2013S | Pringle et al. (Palo Alto) | 4.4 | 2.2 | | X | |
| 2014P | Gerber et al. (MI, MO, TN) | 2.3 | 0.7 | | | X |
| 2014P | Gerber et al. (MI, MO, TN) | 1.4 | 0.7 | | | X |
| 2014P | Zelizer (TX) | 0.6 | 0.3 | X | | |
| 2014G | Gerber et al. (CO) | 1.2 | 1.4 | | | X |
| 2014P | Kling & Stratmann (GA, NE, NM, OH, PA, VA) | 0.3 | 0.2 | X | | |
| 2014G | Bedolla et al. (NALEO) | 1.2 | 0.5 | | X | |
| 2014G | Bedolla et al. (CoCo) | 14.3 | 9.1 | | X | |
| 2014G | Bedolla et al. (AAAJ) | 2.2 | 4.6 | | X | |
| 2014G | Gerber et al. (CO) | 1 | 3.5 | | | X |
| 2016G | Kling (GA, NE, NM, OH, PA, VA) | −0.01 | .09 | X | | |
| 2016G | Kling (Social Pressure) | 0.1 | .09 | X | | |

\* "This robocall used the social pressure script described in chapter 6."

### The pooled estimates (p. 204)

| Category | Distinct experiments | Average effect (points) | 95% CI |
|---|---|---|---|
| **Volunteer** phone banks (CACE) | 32 | **2.78** | 1.74 to 3.83 |
| **Commercial** phone banks (CACE) | 22 | **0.94** | 0.52 to 1.36 |
| **Robocalls** (ITT) | 9 | **0.235** | 0.037 to 0.433 |
| **Robocalls, excluding the two social-pressure studies** (ITT) | 7 | **0.143** | **−0.037 to 0.323** |

### This resolves the lab's robocall question — and finds a real inconsistency

Take the arithmetic step by step.

- Volunteer: 1 / 0.0278 = **36 contacts per vote**. Matches Table 12-1 exactly.
- Commercial: 1 / 0.0094 = **106 contacts per vote**. Matches Table 12-1 exactly.
- Robocalls: 1 / 0.00235 = **425**. Matches Table 12-1 exactly.

So Table 12-1's robocall cell is computed from the **0.235** estimate — the one
that **includes** the two social-pressure robocall studies.

**But the cell's own text reads: "One vote per 425 landlines targeted, *without
social pressure messages*."**

The estimate that actually excludes social pressure is **0.143**, which implies
**one vote per 699 targeted landlines**, and whose confidence interval
(**−0.037 to 0.323**) **includes zero**.

Two things follow, and they matter for the brief:

**1. Your suspicion about the robocalls row was right, and the reason is sharper
than the one you give.** The brief says the row is "currently unaudited," that the
published literature generally finds no effect, and that "nothing inside a file
with no standard errors in it can settle the question either way." The audit is now
done, and it does settle it: **the number 425 and the phrase "without social
pressure messages" attached to it are inconsistent with each other, by the book's
own appendix.** Either the figure should be 699, or the qualifier should be dropped.

**2. The `effective = TRUE` flag is defensible only on the inclusive estimate.**
At 0.235 the CI excludes zero, so "Yes, large number of studies" is fair. At 0.143
— the estimate matching the cell's stated scope — it does not, and the honest
reliability entry would be the same "not significantly greater than zero" that
leafleting, television and radio receive. **So the robocalls row is `TRUE` or
`FALSE` depending on which of two adjacent sentences in the same appendix you
follow.**

This is a much better teaching example than anything currently in the chapter, and
it is entirely internal to the source — no outside literature required. It is the
brief's argument about the `effective` boolean, demonstrated rather than asserted:
a continuous quantity with an interval around it, converted to a yes/no by a
threshold, where the answer flips depending on an inclusion decision recorded 30
pages away and contradicted by the table's own caption.

*Note for the build script:* the brief's existing header comment recording doubt
about robocalls should be replaced with this, and `gotv_tactics.csv` arguably needs
both numbers — 425 (as printed) and 699 (as the caption's scope implies) — or a
note column. Whichever the lab chooses, **the choice is now documentable**, which
it was not before.

---

## Notes section (pp. 205 onward)

Endnotes are organized **by chapter**, numbered from 1 within each. They are
citations rather than substantive commentary — chapter 1 has four notes, chapter 2
begins with Arceneaux, Gerber and Green's matching-versus-experiment comparison
(*Sociological Methods and Research* 39 (2010): 256–82) and a pointer to Gerber and
Green, *Field Experiments: Design, Analysis, and Interpretation* (Norton, 2012).

Chapter 1's notes, in full, as a sample of the apparatus behind the claims recorded
above:

1. Louis-Philippe Beland and Sara Oloomi, "Party Affiliation and Public Spending:
   Evidence from U.S. Governors," *Economic Inquiry* 55, no. 2 (2017): 982–95;
   Christian Dippel, "Political Parties Do Matter in U.S. Cities … for Their
   Unfunded Pensions," *American Economic Journal: Economic Policy* 14, no. 3
   (2022): 33–54.
2. Justin de Benedictis-Kessner and Christopher Warshaw, "Mayoral Partisanship and
   Municipal Fiscal Policy," *Journal of Politics* 78, no. 4 (October 2016):
   1124–38.
3. Bernard L. Fraga and Eitan D. Hersh, "Are Americans Stuck in Uncompetitive
   Enclaves? An Appraisal of U.S. Electoral Competition," *Quarterly Journal of
   Political Science* 13, no. 3 (2018): 291–311. — *this is the source of the "89
   percent of Americans live in jurisdictions where at least one close election …
   occurs over the course of six years" claim.*
4. Hal Malchow, *The New Political Targeting* (Washington, D.C.: Campaigns &
   Elections Magazine, 2003), 281–82.

**Extent.** Notes run **pp. 205–228**, organized by chapter and then by appendix
(Appendix A and Appendix B each have their own numbered notes, both beginning with
the Alvarez, Hopkins and Sinclair citation that explains the package-treatment
exclusion: "Mobilizing Pasadena Democrats: Measuring the Effects of Partisan
Campaign Contacts," *Journal of Politics* 72 (2010): 31–44). Middleton's excluded
2004 study is note 2 to Appendix A: "Voting Is Power 2004 Mobilization Effort"
(unpublished manuscript, University of California, Berkeley, 2015).

They are citations, not commentary, with occasional substantive asides — e.g. note
22 to chapter 11 explains that Hopkins and colleagues found weaker thank-you
effects "perhaps because these messages were sent during the summer, following the
June primary election," and note 31 to chapter 12 attributes the moral-foundations
quotation to Murray and Matland's unpublished 2017 manuscript.

**One citation is fresher in the notes than in the table.** Table 11-1 lists its
item 7 — the 17-state, 1.96-million-citizen social pressure study — as "Alan S.
Gerber and others … (unpublished manuscript, 2016)." Note 19 to chapter 11 gives
the published version: **Gerber, Huber, Fang and Gooch, "The Generalizability of
Social Pressure Effects on Turnout Across High-Salience Electoral Contexts: Field
Experimental Evidence From 1.96 Million Citizens in 17 States," *American Politics
Research* 45, no. 4 (2017): 533–59.** The table's source note was not updated when
the endnote was. Minor, but it is a concrete instance of the same class of problem
the lab is about: two records of the same fact inside one book, one of them stale.

## Index (pp. 229–230) — and a defect in this edition

**The VitalSource edition is truncated.** The reader reports 238 screens, the last
of which is printed **p. 230**, and that page ends partway through the letter C
("Coalition for San Francisco Neighborhoods, 83"). **The index for D through Z is
not present in this digital copy.** Anyone verifying page references against the
print edition should know the electronic index cannot be used past "C".

What survives is useful for one purpose: the **"cost-effectiveness"** entry is a
complete map of where the book prices things, and it corroborates the reading
above. As printed:

> cost-effectiveness: commercial phone banks, 89, *172*; direct mail campaigns,
> 55–56, 66–67, *172*; door-to-door canvassing, 38–39, *172*; e-mail campaigns,
> *172*; festivals, election, 117, 118, 121, *173*; importance of, 9–10; leaflet
> campaigns, 52–53, *172*; mass media campaigns, 137–38, *173*; mobilizing voters
> over long haul, 170–71; phone campaigns, 89–90, *172*; radio advertising, 131,
> 137, *173*; robocalls, 72–73, *172*; signage campaigns, **45, 53**; summarizing,
> 169–70, *172–73*; television advertising, 137, *173*; text messaging, 112, *173*;
> turnout *versus* persuasion, 184–87; volunteer phone banks, 89, *172*; voter
> registration efforts, 147, 150.

Two things to note. **Italicised page numbers are the table pages (172–73)**; roman
numbers are chapter discussions. Every tactic in Table 12-1 has both — a chapter
price and a table cell — except **signage**, which has *only* chapter pages (45, 53)
and no table entry at all, consistent with signage being absent from Table 12-1
while carrying a $36-per-vote figure in chapter 4.

And the entry confirms the inventory of chapter-level prices recorded above is
complete: canvassing 38–39, mail 55–56 and 66–67, leaflets 52–53, signage 45 and
53, phones 89–90, text 112, festivals 117/118/121, mass media 137–38, radio 131 and
137, television 137, registration 147 and 150, plus the long-haul discount at
170–71 and the mobilization-versus-persuasion arithmetic at 184–87. **Nothing was
missed.**

---

## Reading complete

Every page of this edition has now been read except the body of the endnotes
(pp. 206–228), which were surveyed rather than transcribed — they are bibliographic
apparatus, and the sources behind every claim used above are recorded inline in the
chapter sections. The index is incomplete in the source, as described above.
