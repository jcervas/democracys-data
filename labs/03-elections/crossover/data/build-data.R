# ---------------------------------------------------------------------------
# One election against the next, and one office against another, in 2024.
#
# Seven files end up in derived/:
#
#   derived/counties.csv   one row per county that appears in both the 2020 and
#                          the 2024 presidential file: the Republican share of
#                          the two-party vote in each year, and the move
#   derived/states.csv     one row per state: the presidential vote as the
#                          Clerk of the House published it
#   derived/senate.csv     one row per 2024 Senate contest: who won it, on what
#                          party line, and which presidential candidate carried
#                          the state
#   derived/house.csv      one row per 2024 House district: the party that won
#                          the seat and the party that carried the district
#   derived/split_trend.csv  the share of contested House districts that voted
#                          one way for president and another for the House,
#                          1952 to 2024
#   derived/facts.csv      single numbers the brief quotes
#   derived/checks.csv     what this script verified before it wrote anything
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits

# --- SOURCE ----------------------------------------------------------------
#
# Nothing is downloaded here. Every input is a file this corpus already built
# and committed, and each of those chapters records where its own copy came
# from:
#
#   ../../house-competition/data/derived/clerk_2024.txt
#       The text of Statistics of the Presidential and Congressional Election of
#       November 5, 2024, published by the Office of the Clerk of the U.S. House
#       of Representatives on 10 March 2025, turned into text by pdftotext in
#       the house-competition chapter. This is the official compilation of the
#       state canvasses. The presidential, Senate and House results in this
#       chapter are all read out of it.
#
#   ../../../06-putting-data-together/data-sources/data/derived/pres2020_counties.csv
#   ../../../06-putting-data-together/data-sources/data/derived/pres2024_counties.csv
#       County presidential returns for both elections, from Tony McGovern's
#       compilation. The data-sources chapter records how it was assembled; the
#       county-returns chapter checks it against the fifty-one official state
#       publications. No agency publishes a national county file, which is why
#       the county layer of this chapter cannot come from the Clerk.
#
#   ../../house-competition/data/derived/pres_by_cd.csv
#       The presidential vote recomputed on congressional district lines, from
#       The Downballot. No government agency produces this either.
#
#   ../../house-competition/data/derived/clerk_house.csv
#   ../../house-competition/data/derived/by_year.csv
#       That chapter's own parse of the same Clerk documents, 2004 to 2024, and
#       its House series back to 1946. The first is used here only to check this
#       script's parse against one written independently of it.
#
# WHY THE CLERK. No federal office collects election returns. States run and
# count their own elections and publish them in fifty-one formats, so every
# national file is somebody's assembly made afterwards, and assemblies of the
# same election disagree -- see the county-returns chapter, where two of them
# agree on who won and disagree on how many votes were cast in half the country.
#
# This chapter's measure is a subtraction: the Senate share minus the
# presidential share in the same state. A subtraction is where two
# differently-built files leak their differences into the answer, so both
# numbers are taken off the same page of the same document, compiled by one
# office from one set of canvasses on one date.
# ---------------------------------------------------------------------------

CLERK <- "../../house-competition/data/derived/clerk_2024.txt"
C20   <- "../../../06-putting-data-together/data-sources/data/derived/pres2020_counties.csv"
C24   <- "../../../06-putting-data-together/data-sources/data/derived/pres2024_counties.csv"
PBCD  <- "../../house-competition/data/derived/pres_by_cd.csv"
CHOUS <- "../../house-competition/data/derived/clerk_house.csv"
BYYR  <- "../../house-competition/data/derived/by_year.csv"
stopifnot(file.exists(CLERK), file.exists(C20), file.exists(C24),
          file.exists(PBCD), file.exists(CHOUS), file.exists(BYYR))

say <- function(...) cat(..., "\n", sep = "")

STATE_UPPER <- c(
  "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO",
  "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA",
  "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY",
  "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA",
  "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
  "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA",
  "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND",
  "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT",
  "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
STATE_NAME <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
  "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
  "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
STATE_ABB <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID",
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
  "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
  "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
names(STATE_NAME) <- STATE_UPPER
names(STATE_ABB)  <- STATE_UPPER

# ===========================================================================
# 1. THE COUNTIES
# ===========================================================================
#
# The share is of the two major parties, not of every vote cast. Third-party
# votes were not the same size in the two elections, and a share-of-everything
# comparison would read that difference as movement between the major parties.

a <- read.csv(C20, colClasses = c(county_fips = "character"))
b <- read.csv(C24, colClasses = c(county_fips = "character"))

cty <- merge(a[, c("county_fips", "state_name", "county_name",
                   "votes_dem", "votes_gop", "total_votes")],
             b[, c("county_fips", "votes_dem", "votes_gop", "total_votes")],
             by = "county_fips", suffixes = c("_20", "_24"))
cty <- cty[order(cty$county_fips), ]

# THE UNITS THAT DO NOT JOIN are the three the wind-map chapter documents.
# Alaska reports by state legislative district and renumbered them between the
# two elections. Connecticut replaced its eight counties with nine planning
# regions in 2022. The District of Columbia is one row in 2020 and eight in
# 2024. None of that is a broken key: the ground was recut.
gone20 <- unique(a$state_name[!a$county_fips %in% b$county_fips])
gone24 <- unique(b$state_name[!b$county_fips %in% a$county_fips])
DROP20 <- sum(!a$county_fips %in% b$county_fips)
DROP24 <- sum(!b$county_fips %in% a$county_fips)

two_party <- function(g, d) 100 * g / (g + d)
cty$r20   <- round(two_party(cty$votes_gop_20, cty$votes_dem_20), 2)
cty$r24   <- round(two_party(cty$votes_gop_24, cty$votes_dem_24), 2)
cty$swing <- round(cty$r24 - cty$r20, 2)
cty$flipped <- (cty$r20 > 50) != (cty$r24 > 50)

R_COUNTY   <- cor(cty$r20, cty$r24)
FIT        <- lm(r24 ~ r20, data = cty)
RESID_SD   <- sd(resid(FIT))
MED_SWING  <- median(cty$swing)
WITHIN5    <- 100 * mean(abs(cty$swing) < 5)
FLIPS      <- sum(cty$flipped)
FLIPS_TO_R <- sum(cty$flipped & cty$r24 > 50)
FLIPS_TO_D <- sum(cty$flipped & cty$r24 < 50)
# The counties that changed sides are not the small ones. Their share of the
# votes cast says whether an 86-out-of-3,104 headline is as small as it sounds.
FLIP_VOTES <- round(100 * sum(cty$total_votes_24[cty$flipped]) /
                      sum(cty$total_votes_24), 1)
BIGGEST    <- cty[which.max(abs(cty$swing)), ]

# THE POINT OF THE CHAPTER, IN ONE LINE. A correlation does not change when
# every value moves by the same amount, so a country that swung ten points
# would draw the same figure. This is computed rather than asserted, because it
# is what tells a reader what a high correlation is not saying.
R_SHIFTED <- cor(cty$r20, cty$r24 + 10)

# Counties weighted by the votes cast in them. Counties are not people and they
# are wildly unequal, so the plain figure is a statement about places.
R_WEIGHTED <- cov.wt(cbind(cty$r20, cty$r24),
                     wt = cty$total_votes_24, cor = TRUE)$cor[1, 2]

dd_write_csv(cty[, c("county_fips", "state_name", "county_name",
                     "total_votes_20", "total_votes_24",
                     "r20", "r24", "swing", "flipped")],
             "derived/counties.csv")
say("counties joined: ", nrow(cty),
    "  correlation: ", sprintf("%.4f", R_COUNTY),
    "  flips: ", FLIPS)

# ===========================================================================
# 2. READING THE CLERK'S DOCUMENT
# ===========================================================================
#
# HOW THE PARSE WORKS. The document is organised by state. Under each state a
# heading opens a section, and every line beneath it ends in a vote total:
#
#     FOR PRESIDENTIAL ELECTORS
#     Republican ..................................   1,462,616
#     Democratic ..................................     772,412
#
#     FOR UNITED STATES SENATOR
#     Kari Lake, Republican ......................    1,595,761
#     Ruben Gallego, Democrat ....................    1,676,335
#
#     FOR UNITED STATES REPRESENTATIVE
#      1. Doug LaMalfa, Republican ...............      208,592
#         Rose Penelope Yee, Democrat ............      110,636
#
# The presidential section names parties rather than people, because electors
# are what is on the ballot. The Senate and House sections name the candidate
# and print the party after a comma, which is what makes the winner readable.
#
# FOUR THINGS THE PARSE HAS TO GET RIGHT:
#
#   TWO CONTESTS IN ONE STATE. Nebraska and California each held two Senate
#     elections in 2024, one of them for the last weeks of a term somebody had
#     left early. The document separates them with a line in parentheses.
#   FUSION. New York and Connecticut let one candidate stand on several party
#     lines, and the extra line carries no name. Only those two party names are
#     ever added to the candidate above them; every other nameless line is an
#     aggregate -- blanks, write-ins, spoiled ballots, Nevada's "None of These
#     Candidates" -- and is dropped.
#   AN UNOPPOSED CANDIDATE WITH NO NUMBER. Florida and Oklahoma do not publish
#     a count for a candidate nobody ran against, so the Clerk prints a footnote
#     mark instead. Two districts come that way, and both have a winner.
#   THE RECAPITULATION. Each state closes with a summary table whose lines also
#     end in numbers. Reading it would double every state, so the section is
#     closed when that heading appears.

txt <- readLines(CLERK, warn = FALSE)

FUSION <- c("Working Families", "Conservative")
AGG <- "^(Blank|Blanks|Write-in|Scattering|Over Votes|Under Votes|Void|All Others|Others|None of These Candidates|Total)"

pres  <- list()   # state -> named vector of party totals
sen   <- list()   # "STATE term" -> data frame of candidates
house <- list()   # "STATE district" -> data frame of candidates
cur <- NA; sec <- NA; term <- "full"; dist <- NA

add_cand <- function(store, key, state, name, party, votes) {
  row <- data.frame(state = state, name = name, party = party, votes = votes)
  store[[key]] <- if (is.null(store[[key]])) row else rbind(store[[key]], row)
  store
}

for (ln in txt) {
  s <- trimws(ln)
  bare <- trimws(sub("—Continued$", "", s))
  if (bare %in% STATE_UPPER) {
    cur <- bare; sec <- NA; term <- "full"; dist <- NA; next
  }
  if (is.na(cur) || !nzchar(s)) next
  if (grepl("Recapitulation", s)) { sec <- NA; next }
  if (grepl("FOR PRESIDENTIAL ELECTORS", s)) { sec <- "pres"; next }
  if (grepl("FOR UNITED STATES SENATOR", s)) { sec <- "sen"; term <- "full"; next }
  if (grepl("FOR UNITED STATES REPRESENTATIVE", s)) { sec <- "rep"; dist <- NA; next }
  if (grepl("FOR (DELEGATE|RESIDENT COMMISSIONER)", s)) { sec <- NA; next }
  if (is.na(sec)) next
  if (grepl("^\\(For ", s)) {
    term <- if (grepl("unexpired", s)) "unexpired" else "full"
    next
  }
  body <- s
  if (sec == "rep") {
    if (s == "AT LARGE") { dist <- 1; next }
    d <- regmatches(s, regexpr("^[0-9]+\\.", s))
    if (length(d)) {
      dist <- as.integer(sub("\\.", "", d))
      body <- trimws(sub("^[0-9]+\\.", "", s))
    }
    if (is.na(dist)) next
  }
  # The vote total, or the footnote mark that stands where one would be.
  m <- regexpr("[0-9][0-9,]*$", body)
  unopposed <- grepl("\\(1\\)$", body)
  if (m != -1) {
    votes <- as.numeric(gsub(",", "", regmatches(body, m)))
    label <- trimws(sub("\\s*\\.{2,}.*$", "", substr(body, 1, m - 1)))
  } else if (unopposed) {
    votes <- NA_real_
    label <- trimws(sub("\\s*\\.{2,}.*$", "", sub("\\(1\\)$", "", body)))
  } else next
  if (!nzchar(label) || grepl(AGG, label)) next
  if (sec == "pres") {
    v <- pres[[cur]]
    if (is.null(v)) v <- c()
    v[label] <- sum(c(v[label], votes), na.rm = TRUE)
    pres[[cur]] <- v
    next
  }
  key <- if (sec == "sen") paste(cur, term) else paste(cur, dist)
  store <- if (sec == "sen") sen else house
  if (grepl(",", label)) {
    store <- add_cand(store, key, cur,
                      trimws(sub(",[^,]*$", "", label)),
                      trimws(sub("^.*,\\s*", "", label)), votes)
  } else if (label %in% FUSION && !is.null(store[[key]])) {
    n <- nrow(store[[key]])
    store[[key]]$votes[n] <- store[[key]]$votes[n] + votes
  }
  if (sec == "sen") sen <- store else house <- store
}

stopifnot(length(pres) == 51, length(sen) > 30, length(house) == 435)

# The party as the Clerk printed it. "Democrat" and "Democratic" are both used.
# An independent is neither, and this chapter turns on not quietly filing two
# senators with a party they did not run on.
side <- function(p) ifelse(grepl("^Democrat", p), "D",
                    ifelse(grepl("^Republican", p), "R", "I"))

# --- the presidential vote, state by state ---------------------------------
#
# The nominees' lines are named for the party rather than the person. New York
# prints two extra lines that belong to the same two nominees, and the label
# says so. A write-in line is not a nominee's line and is left out.
REP_LINES <- c("Republican", "Conservative (Republican Party Candidate)")
DEM_LINES <- c("Democratic", "Working Families (Democratic Party Candidate)")
pres_row <- function(st) {
  v <- pres[[st]]
  r <- sum(v[names(v) %in% REP_LINES])
  d <- sum(v[names(v) %in% DEM_LINES])
  data.frame(upper = st, state = unname(STATE_NAME[st]),
             abbrev = unname(STATE_ABB[st]),
             pres_rep = r, pres_dem = d,
             pres_winner = ifelse(r > d, "R", "D"),
             pres_r_two = round(100 * r / (r + d), 2))
}
states <- do.call(rbind, lapply(STATE_UPPER, pres_row))

# --- the Senate contests ----------------------------------------------------
win_row <- function(key) {
  d <- sen[[key]]
  d <- d[order(-d$votes), ]
  w <- d[1, ]; r <- d[2, ]
  data.frame(upper = w$state,
             term = sub("^.* ", "", key),
             winner = w$name, winner_party = w$party,
             winner_side = side(w$party), winner_votes = w$votes,
             runner = r$name, runner_party = r$party,
             runner_side = side(r$party), runner_votes = r$votes)
}
senate <- do.call(rbind, lapply(names(sen), win_row))
senate <- merge(senate,
                states[, c("upper", "state", "abbrev", "pres_winner", "pres_r_two")],
                by = "upper")
senate <- senate[order(senate$abbrev, senate$term), ]
senate$margin <- round(100 * (senate$winner_votes - senate$runner_votes) /
                         (senate$winner_votes + senate$runner_votes), 2)

# THE ONE NUMBER THAT PUTS THE TWO OFFICES ON THE SAME SCALE: the Republican
# share of the vote cast for the two leading candidates in the contest.
#
# WHY THE TOP TWO AND NOT THE TWO MAJOR PARTIES. In three of these contests the
# Republican's real opponent was not a Democrat. Maine and Vermont re-elected
# independents, and in Nebraska's full-term contest no Democrat ran at all, so a
# Republican-versus-Democrat share would report Maine as three-to-one Republican
# and Nebraska as unopposed. Both are false. The top two are Trump and Harris in
# every presidential contest, so for president this is the two-party share under
# another name, and the two axes stay comparable.
r_top2 <- function(ws, wv, rs, rv) {
  r <- ifelse(ws == "R", wv, ifelse(rs == "R", rv, NA))
  o <- ifelse(ws == "R", rv, ifelse(rs == "R", wv, NA))
  round(100 * r / (r + o), 2)
}
senate$sen_r_top2 <- r_top2(senate$winner_side, senate$winner_votes,
                            senate$runner_side, senate$runner_votes)
# The Republican Senate candidate's distance from the Republican presidential
# candidate in the same state, in points. Positive means the Senate candidate
# ran ahead of Trump where they were both on the ballot.
senate$ran_ahead <- round(senate$sen_r_top2 - senate$pres_r_two, 2)
# Whose number that is: the Republican in the contest, who is the winner in some
# of these and the runner-up in the rest.
senate$rep_name <- ifelse(senate$winner_side == "R", senate$winner, senate$runner)
AHEAD <- senate[which.max(senate$ran_ahead), ]
BEHIND <- senate[which.min(senate$ran_ahead), ]
GAP_MED <- median(abs(senate$ran_ahead))
R_OFFICE <- cor(senate$pres_r_two, senate$sen_r_top2)

# TWO WAYS TO COUNT A SPLIT, and the difference between them is a coding
# decision rather than a fact about the votes. The loose count files the two
# independents with the party they sit with in Washington, which is what makes
# the familiar number four. The strict count reads the ballot line the senator
# actually ran on.
senate$crossover <- (senate$winner_side == "R" & senate$pres_winner == "D") |
                    (senate$winner_side == "D" & senate$pres_winner == "R")
senate$crossover_strict <- senate$winner_side != senate$pres_winner

CROSS        <- sum(senate$crossover)
CROSS_STRICT <- sum(senate$crossover_strict)
N_CONTEST    <- nrow(senate)
N_SEN_STATES <- length(unique(senate$upper))
N_INDEP      <- sum(senate$winner_side == "I")

dd_write_csv(states[, setdiff(names(states), "upper")], "derived/states.csv")
dd_write_csv(senate[, setdiff(names(senate), "upper")], "derived/senate.csv")
say("senate contests: ", N_CONTEST, " in ", N_SEN_STATES, " states",
    "  crossovers: ", CROSS, " (", CROSS_STRICT, " counting the independents)")

# ===========================================================================
# 3. THE HOUSE
# ===========================================================================
#
# The winner of each district comes out of the Clerk document above. The
# presidential vote inside the same district lines does not: no agency computes
# it, and the figure everybody uses is The Downballot's, which allocates
# precinct returns to district boundaries. The 2024 sheet is the one drawn on
# the lines the 2024 election was run under.

hwin <- function(key) {
  d <- house[[key]]
  known <- d[!is.na(d$votes), ]
  # An unopposed candidate with no published count is the winner of a district
  # with exactly one candidate in it.
  w <- if (nrow(known)) known[order(-known$votes), ][1, ] else d[1, ]
  data.frame(upper = w$state, district = as.integer(sub("^.* ", "", key)),
             winner = w$name, winner_party = w$party,
             house_winner = side(w$party),
             one_candidate = nrow(d) == 1)
}
hs <- do.call(rbind, lapply(names(house), hwin))
hs$abbrev <- unname(STATE_ABB[hs$upper])
hs$state  <- unname(STATE_NAME[hs$upper])

pb <- read.csv(PBCD)
pb <- pb[pb$pres_year == 2024 & pb$lines == "2024", ]
stopifnot(nrow(pb) == 435)
hs <- merge(hs, pb[, c("state_abb", "cd", "dpres")],
            by.x = c("abbrev", "district"), by.y = c("state_abb", "cd"))
hs$pres_winner <- ifelse(hs$dpres > 50, "D", "R")
hs$crossover   <- hs$house_winner != hs$pres_winner
hs <- hs[order(hs$abbrev, hs$district), ]

N_HOUSE  <- nrow(hs)
H_CROSS  <- sum(hs$crossover)
H_D_IN_R <- sum(hs$crossover & hs$house_winner == "D")
H_R_IN_D <- sum(hs$crossover & hs$house_winner == "R")



# THE PARSE, CHECKED AGAINST ONE WRITTEN INDEPENDENTLY OF IT. The
# house-competition chapter parsed the same document for a different purpose and
# published a Democratic share of the two-party House vote per district. Where
# that share exists, it implies a winner, and the two answers should be the same.
ch <- read.csv(CHOUS)
ch <- ch[ch$year == 2024 & !is.na(ch$dv), ]
ch$abbrev <- unname(STATE_ABB[toupper(ch$state)])
cmp <- merge(hs[, c("abbrev", "district", "house_winner")],
             ch[, c("abbrev", "district", "dv")],
             by = c("abbrev", "district"))
cmp$their <- ifelse(cmp$dv > 50, "D", "R")
AGREE <- sum(cmp$house_winner == cmp$their)
AGREE_N <- nrow(cmp)

# PUTTING THE TWO OFFICES ON ONE SCALE, AS FOR THE SENATE. The Senate figure
# needed a top-two share because in three contests the Republican's real
# opponent was not a Democrat. The House does not need one: the
# house-competition chapter already publishes a Democratic share of the
# two-party House vote per district, cross-checked against the Clerk, and this
# script's own parse agrees with it on every district it defines.
#
# WHAT IT DOES NOT DEFINE, AND WHY THAT MATTERS. That column is empty for the
# districts where a two-party share is not a quantity: nobody ran against the
# incumbent, or the two candidates on the November ballot were of the same
# party. Those districts have a winner and no comparison, so they are in the
# dot figure and out of the scatter, and the count is carried in facts.csv
# rather than left for a reader to notice.
hs <- merge(hs, ch[, c("abbrev", "district", "dv")],
            by = c("abbrev", "district"), all.x = TRUE)
hs$rep_pres  <- round(100 - hs$dpres, 2)
hs$rep_house <- round(100 - hs$dv, 2)
hs$ran_ahead <- round(hs$rep_house - hs$rep_pres, 2)
hs <- hs[order(hs$abbrev, hs$district), ]

sc <- hs[!is.na(hs$rep_house), ]
N_SCATTER  <- nrow(sc)
N_NO_SHARE <- sum(is.na(hs$rep_house))
R_OFFICE_H <- cor(sc$rep_pres, sc$rep_house)
GAP_MED_H  <- median(abs(sc$ran_ahead))
H_AHEAD    <- sc[which.max(sc$ran_ahead), ]
H_BEHIND   <- sc[which.min(sc$ran_ahead), ]
# Every crossover district has to survive into the scatter, or the figure would
# quietly drop the cases the chapter is about.
CROSS_IN_SC <- sum(sc$crossover)

dd_write_csv(hs[, c("state", "abbrev", "district", "winner", "winner_party",
                    "house_winner", "dpres", "rep_pres", "rep_house",
                    "ran_ahead", "pres_winner", "crossover", "one_candidate")],
             "derived/house.csv")

# The long series, and two decisions about which years belong in it.
#
# PRESIDENTIAL YEARS ONLY. In a midterm the House result is compared with the
# presidential vote from two years earlier, because there is no other one to
# compare it with. That is a different question from the one this chapter asks,
# so the midterms are left out and every point on the line is a House election
# held on the same day as the presidential election it is measured against.
#
# COVERAGE BELOW 100 means the presidential result was not known for every
# district that year, and a share computed over part of the House is not
# comparable with one computed over all of it.
#
# The denominator is the districts where both major parties ran, because a
# district with one name on the ballot has no two-party share to compare.
by <- read.csv(BYYR)
tr <- by[!is.na(by$pct_split) & by$split_coverage == 100 & by$year %% 4 == 0,
         c("year", "races", "pct_split")]
dd_write_csv(tr, "derived/split_trend.csv")

TR_HI    <- max(tr$pct_split)
TR_HI_YR <- tr$year[which.max(tr$pct_split)]
TR_24    <- tr$pct_split[tr$year == 2024]
say("house districts: ", N_HOUSE, "  crossovers: ", H_CROSS,
    "  agreement with house-competition: ", AGREE, "/", AGREE_N)

# ===========================================================================
# 4. THE NUMBERS THE BRIEF QUOTES
# ===========================================================================

fact <- function(k, v) data.frame(key = k, value = as.character(v))
cross_abb <- sort(senate$abbrev[senate$crossover])
indep_abb <- sort(senate$abbrev[senate$winner_side == "I"])
facts <- do.call(rbind, list(
  fact("clerk_lines",        length(txt)),
  fact("n_counties",         nrow(cty)),
  fact("drop_2020",          DROP20),
  fact("drop_2024",          DROP24),
  fact("r_county",           sprintf("%.4f", R_COUNTY)),
  fact("r_shifted",          sprintf("%.4f", R_SHIFTED)),
  fact("r_weighted",         sprintf("%.4f", R_WEIGHTED)),
  fact("resid_sd",           sprintf("%.2f", RESID_SD)),
  fact("slope",              sprintf("%.3f", coef(FIT)[2])),
  fact("median_swing",       sprintf("%.2f", MED_SWING)),
  fact("within5",            sprintf("%.1f", WITHIN5)),
  fact("flips",              FLIPS),
  fact("flips_to_r",         FLIPS_TO_R),
  fact("flips_to_d",         FLIPS_TO_D),
  fact("flip_vote_share",    sprintf("%.1f", FLIP_VOTES)),
  fact("biggest_county",     paste0(BIGGEST$county_name, ", ", BIGGEST$state_name)),
  fact("biggest_swing",      sprintf("%.1f", BIGGEST$swing)),
  fact("n_contests",         N_CONTEST),
  fact("n_senate_states",    N_SEN_STATES),
  fact("n_crossover",        CROSS),
  fact("n_crossover_strict", CROSS_STRICT),
  fact("n_independent",      N_INDEP),
  fact("r_office",           sprintf("%.3f", R_OFFICE)),
  fact("gap_median",         sprintf("%.1f", GAP_MED)),
  fact("ahead_state",        AHEAD$state),
  fact("ahead_who",          AHEAD$rep_name),
  fact("ahead_by",           sprintf("%.1f", AHEAD$ran_ahead)),
  fact("behind_state",       BEHIND$state),
  fact("behind_who",         BEHIND$rep_name),
  fact("behind_by",          sprintf("%.1f", abs(BEHIND$ran_ahead))),
  fact("crossover_states",   paste(cross_abb, collapse = ", ")),
  fact("independent_states", paste(indep_abb, collapse = ", ")),
  fact("n_house",            N_HOUSE),
  fact("h_crossover",        H_CROSS),
  fact("h_d_in_r",           H_D_IN_R),
  fact("h_r_in_d",           H_R_IN_D),
  fact("h_pct_all",          sprintf("%.1f", 100 * H_CROSS / N_HOUSE)),
  fact("n_scatter",          N_SCATTER),
  fact("n_no_share",         N_NO_SHARE),
  fact("r_office_house",     sprintf("%.3f", R_OFFICE_H)),
  fact("gap_median_house",   sprintf("%.1f", GAP_MED_H)),
  fact("h_ahead_seat",       paste0(H_AHEAD$abbrev, "-",
                                    sprintf("%02d", H_AHEAD$district))),
  fact("h_ahead_by",         sprintf("%.1f", H_AHEAD$ran_ahead)),
  fact("h_behind_seat",      paste0(H_BEHIND$abbrev, "-",
                                    sprintf("%02d", H_BEHIND$district))),
  fact("h_behind_by",        sprintf("%.1f", abs(H_BEHIND$ran_ahead))),
  fact("h_pct_24",           sprintf("%.1f", TR_24)),
  fact("h_pct_peak",         sprintf("%.1f", TR_HI)),
  fact("h_peak_year",        TR_HI_YR),
  fact("trend_first",        min(tr$year)),
  fact("agree_n",            AGREE_N),
  fact("closest_senate",     senate$state[which.min(senate$margin)]),
  fact("closest_margin",     sprintf("%.2f", min(senate$margin)))))
dd_write_csv(facts, "derived/facts.csv")

# ===========================================================================
# 5. WHAT THIS SCRIPT VERIFIED
# ===========================================================================

nebraska <- sum(senate$upper == "NEBRASKA")
pres_tot <- sum(states$pres_rep) + sum(states$pres_dem)
dem_states <- sum(states$pres_winner == "D")

checks <- rbind(
  data.frame(check = "the Clerk's document covers every state and DC",
             expected = "51 presidential sections",
             got = paste(length(pres), "sections"),
             ok = length(pres) == 51),
  data.frame(check = "the presidential winner by state matches the published map",
             expected = "Harris carried 20 states and DC",
             got = paste(dem_states, "carried"),
             ok = dem_states == 20),
  data.frame(check = "the two-party presidential total is the right size",
             expected = "between 150 and 160 million",
             got = paste(round(pres_tot / 1e6, 1), "million"),
             ok = pres_tot > 150e6 && pres_tot < 160e6),
  data.frame(check = "every Senate contest has a winner and a runner-up",
             expected = "no contest with one candidate",
             got = paste(sum(senate$winner_votes > senate$runner_votes),
                         "of", nrow(senate)),
             ok = all(senate$winner_votes > senate$runner_votes)),
  data.frame(check = "Nebraska held two Senate contests and both are here",
             expected = "2",
             got = as.character(nebraska),
             ok = nebraska == 2),
  data.frame(check = "every Senate contest had a Republican in its top two",
             expected = "35, so the two offices share one scale",
             got = paste(sum(!is.na(senate$sen_r_top2)), "of", nrow(senate)),
             ok = all(!is.na(senate$sen_r_top2))),
  data.frame(check = "the top-two share agrees with the two-party one where both exist",
             expected = "identical wherever the runner-up is a major-party candidate",
             got = {
               m <- senate$winner_side %in% c("D", "R") &
                    senate$runner_side %in% c("D", "R")
               tp <- round(100 * ifelse(senate$winner_side == "R",
                                        senate$winner_votes, senate$runner_votes) /
                             (senate$winner_votes + senate$runner_votes), 2)
               paste(sum(m & abs(tp - senate$sen_r_top2) < 0.005), "of", sum(m))
             },
             ok = {
               m <- senate$winner_side %in% c("D", "R") &
                    senate$runner_side %in% c("D", "R")
               tp <- round(100 * ifelse(senate$winner_side == "R",
                                        senate$winner_votes, senate$runner_votes) /
                             (senate$winner_votes + senate$runner_votes), 2)
               all(abs(tp[m] - senate$sen_r_top2[m]) < 0.005)
             }),
  data.frame(check = "the counties that do not join are the recut ones",
             expected = "Alaska, Connecticut and the District of Columbia only",
             got = paste(sort(unique(c(gone20, gone24))), collapse = ", "),
             ok = setequal(unique(c(gone20, gone24)),
                           c("Alaska", "Connecticut", "District of Columbia"))),
  data.frame(check = "every county that changed sides changed the same way",
             expected = "all of them, and none the other way",
             got = paste(FLIPS_TO_R, "toward the Republicans,", FLIPS_TO_D,
                         "the other way"),
             ok = FLIPS_TO_R + FLIPS_TO_D == FLIPS && FLIPS_TO_D == 0),
  data.frame(check = "no county share is outside 0 to 100",
             expected = "0 to 100 in both years",
             got = sprintf("%.1f to %.1f", min(c(cty$r20, cty$r24)),
                           max(c(cty$r20, cty$r24))),
             ok = min(c(cty$r20, cty$r24)) >= 0 &&
                  max(c(cty$r20, cty$r24)) <= 100),
  data.frame(check = "moving every county ten points leaves the correlation alone",
             expected = "the same to seven decimal places",
             got = sprintf("%.7f", R_SHIFTED),
             ok = abs(R_SHIFTED - R_COUNTY) < 1e-7),
  data.frame(check = "the House parse found a whole chamber",
             expected = "435 districts",
             got = as.character(N_HOUSE),
             ok = N_HOUSE == 435),
  data.frame(check = "this parse agrees with the one in house-competition",
             expected = paste(AGREE_N, "districts, same winner"),
             got = paste(AGREE, "agree"),
             ok = AGREE == AGREE_N),
  data.frame(check = "the long series has no missing presidential year",
             expected = "every fourth year from 1952 to 2024",
             got = paste(nrow(tr), "of",
                         length(seq(min(tr$year), max(tr$year), 4))),
             ok = setequal(tr$year, seq(min(tr$year), max(tr$year), 4))),
  data.frame(check = "no crossover district falls out of the House scatter",
             expected = paste(H_CROSS, "of", H_CROSS),
             got = paste(CROSS_IN_SC, "of", H_CROSS),
             ok = CROSS_IN_SC == H_CROSS),
  data.frame(check = "the districts with no two-party House share are accounted for",
             expected = "435 = drawn plus dropped",
             got = paste(N_SCATTER, "+", N_NO_SHARE, "=", N_SCATTER + N_NO_SHARE),
             ok = N_SCATTER + N_NO_SHARE == N_HOUSE),
  data.frame(check = "crossover districts split into the two directions",
             expected = "the two directions sum to the total",
             got = paste(H_D_IN_R, "+", H_R_IN_D, "=", H_CROSS),
             ok = H_D_IN_R + H_R_IN_D == H_CROSS))
checks$ok <- ifelse(checks$ok, "yes", "NO")
write.csv(checks, "derived/checks.csv", row.names = FALSE)
print(checks)
stopifnot(all(checks$ok == "yes"))

say("\ndone.")

# ---------------------------------------------------------------------------
# Build stamp: what this script produced, hashed, beside the data. See
# ../../../_lib/provenance.R.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
