# ---------------------------------------------------------------------------
# Build the officeholder-roster dataset: who held the seat, which is not the
# same question as who won the election.
#
# The second SOURCE chapter in Part II. Part II is the record of an OUTCOME --
# certified returns, precinct by precinct, ballot by ballot. Four of its
# chapters run on something else entirely: a roster of who was a member of
# Congress. A roster is not a return, nothing in this book says so, and the
# difference turns out to be large enough to count.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# Nothing is downloaded. Every figure is read from the derived tables the four
# roster chapters already built, so the provenance of this chapter is four
# chapters rather than four URLs.
#
#   ../../retirements/data/derived/departures.csv   members and seats, by Congress
#   ../../retirements/data/derived/exits.csv        why each member stopped serving
#   ../../careers/data/derived/careers.csv          one row per career
#   ../../midterm-loss/data/derived/house_midterms.csv  party divisions by election
#   ../../primary-defeats/data/derived/incumbents.csv   incumbents facing a primary
#
# The underlying instruments, cited by those chapters rather than here:
# Voteview's HSall_members.csv (the membership roster), the Office of the
# Historian's Party Divisions tables (the seat counts), and the FEC's
# `federalelections` compendium (the candidate-level returns the roster is
# matched against).
#
# THE ARGUMENT. A certified return records a CONTEST: on one day, in one
# district, these people got these votes. A roster records a MEMBERSHIP: on
# some range of days, this person held this seat. Both are official, both are
# complete, and they answer different questions -- which is invisible until you
# put them beside each other and find that the numbers do not reconcile,
# because they were never counting the same thing.
#
# THE MEASURABLE GAP. More people have served in the House than there have been
# seats to serve in. Every one of the excess arrived mid-Congress, which means
# no general election for that Congress put them there: they were appointed,
# or won a special election, or were seated after a contest was resolved. A
# file of general-election returns cannot explain their presence at all.
#
# THE SILENCE, which mirrors the returns chapter exactly. returns-source's
# limit is that a return is permanently silent about WHO voted. A roster's
# limit is the other half: it is permanently silent about VOTES. Not one of
# these tables carries a vote total for the elections that produced the
# membership it describes. The party-divisions series counts seats, never
# ballots.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Five tables in derived/:
#
#   derived/rosters.csv    the three instruments: publisher, what one row is,
#                          and what each is silent about
#   derived/excess.csv     people who served against seats that existed, by
#                          Congress, and the mid-Congress arrivals
#   derived/exits.csv      why members stopped serving, and whether a voter
#                          decided it
#   derived/careers.csv    careers, and the ones that are not a single interval
#   derived/checks.csv     the validation results the chapter prints verbatim
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

SIB <- function(chapter, file)
  file.path("..", "..", chapter, "data", "derived", file)

need <- c(SIB("retirements", "departures.csv"), SIB("retirements", "exits.csv"),
          SIB("careers", "careers.csv"), SIB("midterm-loss", "house_midterms.csv"),
          SIB("primary-defeats", "incumbents.csv"))
missing <- need[!file.exists(need)]
if (length(missing))
  stop("This chapter reads four siblings and cannot run without them.\n  missing: ",
       paste(missing, collapse = "\n           "))

num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# --- 1. The three instruments -----------------------------------------------

rosters <- data.frame(
  roster = c("Membership roster", "Party divisions", "Candidate returns"),
  published_by = c("Voteview (academic, from official sources)",
                   "Office of the Historian, U.S. House",
                   "Federal Election Commission"),
  one_row_is = c("One member in one Congress",
                 "One Congress",
                 "One candidate in one election"),
  answers = c("Who was seated, and when",
              "How many seats each party held",
              "Who ran, and how many votes they got"),
  silent_about = c("Any vote cast in any election",
                   "Any vote cast in any election",
                   "Anyone who took the seat without running"))
dd_write_csv(rosters, "derived/rosters.csv")

# --- 2. More people than seats ----------------------------------------------

dep <- read.csv(SIB("retirements", "departures.csv"))
for (v in c("members", "seats", "mid_congress", "died"))
  dep[[v]] <- num(dep[[v]])
dep$excess <- dep$members - dep$seats

excess <- data.frame(
  congress = dep$congress, year = dep$year,
  members = dep$members, seats = dep$seats,
  excess = dep$excess, mid_congress = dep$mid_congress, died = dep$died)
dd_write_csv(excess, "derived/excess.csv")

TOT_MEM  <- sum(dep$members)
TOT_SEAT <- sum(dep$seats)
TOT_MID  <- sum(dep$mid_congress)
TOT_DIED <- sum(dep$died)
worst    <- dep[which.max(dep$excess), ]

# The excess IS the mid-Congress arrivals. If that identity ever breaks, the
# two columns are measuring different things and the argument is unsound.
stopifnot(TOT_MEM - TOT_SEAT == TOT_MID)

# --- 3. Why members stop serving --------------------------------------------

ex <- read.csv(SIB("retirements", "exits.csv"))

# A voter decided a departure only when a voter was asked: a general-election
# defeat, or a denied renomination. Retirement, a run for the Senate and death
# are all departures no ballot chose.
DECIDED   <- c("defeated in the general election", "denied renomination")
UNDECIDED <- c("did not seek re-election", "ran for the Senate", "died in office")

tab <- as.data.frame(table(ex$outcome), stringsAsFactors = FALSE)
names(tab) <- c("outcome", "members")
tab$departure    <- tab$outcome != "re-elected"
tab$voter_decided <- ifelse(!tab$departure, NA, tab$outcome %in% DECIDED)
tab <- tab[order(-tab$members), ]
dd_write_csv(tab, "derived/exits.csv")

stopifnot(all(tab$outcome[tab$departure] %in% c(DECIDED, UNDECIDED)))

DEPARTS  <- sum(tab$members[tab$departure])
BY_VOTER <- sum(tab$members[which(tab$voter_decided)])
NO_VOTER <- DEPARTS - BY_VOTER
NCONG_EX <- length(unique(ex$congress))

# --- 4. Careers are not intervals -------------------------------------------

ca <- read.csv(SIB("careers", "careers.csv"))
ca$gap <- as.logical(ca$gap)
careers <- data.frame(
  quantity = c("Careers in the file", "Careers with a gap in service",
               "Share with a gap, %"),
  value = c(nrow(ca), sum(ca$gap), 100 * sum(ca$gap) / nrow(ca)))
dd_write_csv(careers, "derived/careers.csv")

# --- 5. The party-divisions series carries no votes -------------------------

mid <- read.csv(SIB("midterm-loss", "house_midterms.csv"))
VOTE_COLS <- grep("vote|ballot", names(mid), ignore.case = TRUE, value = TRUE)
stopifnot(length(VOTE_COLS) == 0L)   # the silence, asserted rather than claimed

inc <- read.csv(SIB("primary-defeats", "incumbents.csv"))

# --- 6. Checks --------------------------------------------------------------

f1 <- function(x) formatC(x, format = "f", digits = 1)
cm <- function(x) format(round(x), big.mark = ",")

chk <- data.frame(
  check = c(
    "Rosters described",
    "Chapters read, none of them downloaded",
    "Congresses in the membership series",
    "People who have served in the House",
    "Seats that have existed to serve in",
    "Excess of people over seats",
    "Excess as a share of seats, %",
    "Mid-Congress arrivals (identical to the excess, by construction)",
    "Seats emptied by a death in office",
    "Worst single Congress: members",
    "Worst single Congress: seats",
    "Worst single Congress: which, and when",
    "Congresses in the modern exit file",
    "Departures in that file",
    "Departures a voter decided",
    "Departures no voter decided",
    "Share no voter decided, %",
    "Careers in the career file",
    "Careers that are not one continuous interval",
    "Columns in the party-divisions series carrying a vote total",
    "Incumbent-primary rows available for matching"),
  value = c(
    nrow(rosters),
    4,
    cm(nrow(dep)),
    cm(TOT_MEM),
    cm(TOT_SEAT),
    cm(TOT_MEM - TOT_SEAT),
    f1(100 * (TOT_MEM - TOT_SEAT) / TOT_SEAT),
    cm(TOT_MID),
    cm(TOT_DIED),
    cm(worst$members),
    cm(worst$seats),
    paste0(worst$congress, "th (", worst$year, ")"),
    NCONG_EX,
    cm(DEPARTS),
    cm(BY_VOTER),
    cm(NO_VOTER),
    f1(100 * NO_VOTER / DEPARTS),
    cm(nrow(ca)),
    cm(sum(ca$gap)),
    length(VOTE_COLS),
    cm(nrow(inc))))
dd_write_csv(chk, "derived/checks.csv")

# Hard stops. Each is a way this chapter could be quietly wrong.
stopifnot(
  nrow(rosters) == 3L,
  TOT_MEM > TOT_SEAT,                       # the whole argument
  TOT_MID > 0L,
  TOT_DIED > 0L,
  worst$members > worst$seats * 1.2,        # the extreme case is extreme
  DEPARTS == BY_VOTER + NO_VOTER,           # the partition is exhaustive
  NO_VOTER > BY_VOTER,                      # the finding, stated as a test
  sum(ca$gap) > 0L,
  length(VOTE_COLS) == 0L,
  nrow(inc) > 1000L)

cat(sprintf("\nrosters.csv : %d instruments\n", nrow(rosters)))
cat(sprintf("excess.csv  : %s people, %s seats, excess %s (%s%%)\n",
            cm(TOT_MEM), cm(TOT_SEAT), cm(TOT_MEM - TOT_SEAT),
            f1(100 * (TOT_MEM - TOT_SEAT) / TOT_SEAT)))
cat(sprintf("exits.csv   : %s departures, %s decided by a voter, %s not (%s%%)\n",
            cm(DEPARTS), cm(BY_VOTER), cm(NO_VOTER), f1(100 * NO_VOTER / DEPARTS)))
cat(sprintf("careers.csv : %s careers, %s with a gap\n", cm(nrow(ca)), cm(sum(ca$gap))))
cat("\ndone.\n")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
