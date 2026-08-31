# ---------------------------------------------------------------------------
# Build the administrative-record dataset: three institutions, and the
# denominator none of them supplies.
#
# The source chapter for the last run of Part IV -- the three chapters that
# describe people without being voter files at all. Its subject is the
# instrument they share: a record produced by an institution in the course of
# doing something else, about a person who was never asked.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# Nothing is downloaded. Every figure is read from the derived tables the three
# chapters already built, so the provenance of this chapter is three chapters
# rather than three URLs -- and re-running any of them re-runs this argument.
#
#   ../../policing/data/derived/by_race.csv
#   ../../policing/data/derived/acs_denominators.csv
#   ../../jury-selection/data/derived/strikes.csv
#   ../../jury-selection/data/derived/trials.csv
#   ../../redlining/data/derived/cities.csv
#
# THE ARGUMENT. A census enumerates, a survey elicits, an election return
# records an outcome, a voter file records an administrative status about a
# person who at least applied for it. These three record a person who did not
# apply for anything: a driver was stopped, a juror was struck, a neighbourhood
# was graded. Nobody consented, nobody was sampled, and no institution was
# measuring anything -- it was policing, prosecuting, or pricing mortgage risk,
# and the file is the paperwork.
#
# That provenance has one consequence which decides everything downstream.
# AN ADMINISTRATIVE RECORD ARRIVES AS A NUMERATOR. It counts the events the
# institution acted on and is silent about the population those events were
# drawn from, because counting that population was nobody's job. So every
# claim about a rate has to import a denominator from outside the file, and
# the choice of denominator is the analysis.
#
# The three chapters are ordered by how that goes:
#
#   policing        the denominator CANNOT EXIST. No census counts who was
#                   driving on that road at that hour. Three published
#                   denominators are available and they disagree by a lot.
#   jury-selection  the denominator IS INSIDE THE FILE. The defense strikes
#                   from the same pools the state does, so the file carries
#                   its own control group.
#   redlining       the denominator is a CHOICE OF UNIT. Pool the cities and
#                   the comparison is between different cities; hold the city
#                   fixed and it is between neighbourhoods.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Five tables in derived/:
#
#   derived/records.csv       the three records: who wrote it, why, what one row
#                             is, and what it is silent about
#   derived/denominators.csv  policing -- one numerator, three published
#                             denominators, three different disparity ratios
#   derived/control.csv       jury-selection -- the four strike rates, and the
#                             comparison the file makes possible
#   derived/unit.csv          redlining -- pooled against within-city, and the
#                             cities that reverse
#   derived/checks.csv        the validation results the chapter prints verbatim
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

# raw/ holds sources as they arrive; derived/ is what this script writes. There
# is no raw/ here: nothing arrives from outside the corpus.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

SIB <- function(chapter, file)
  file.path("..", "..", chapter, "data", "derived", file)

need <- c(SIB("policing", "by_race.csv"),
          SIB("policing", "acs_denominators.csv"),
          SIB("jury-selection", "strikes.csv"),
          SIB("jury-selection", "trials.csv"),
          SIB("redlining", "cities.csv"))
missing <- need[!file.exists(need)]
if (length(missing))
  stop("This chapter reads three siblings and cannot run without them.\n  missing: ",
       paste(missing, collapse = "\n           "))

# --- 1. The three records ---------------------------------------------------
#
# Stated rather than computed. What each institution was doing, and what the
# file is therefore silent about, is a reading of the three chapters and not a
# quantity in them.

records <- data.frame(
  record      = c("Traffic stops", "Peremptory strikes", "HOLC security maps"),
  chapter     = c("policing", "jury-selection", "redlining"),
  written_by  = c("A police officer, at the roadside",
                  "A court clerk, during jury selection",
                  "A federal agency, grading mortgage risk"),
  in_order_to = c("Record an enforcement action",
                  "Record who was seated and who was removed",
                  "Tell lenders which neighbourhoods were safe to lend in"),
  one_row_is  = c("One stop", "One juror in one trial", "One graded area"),
  silent_about = c("Everyone who was driving and was not stopped",
                   "Why a juror was struck",
                   "Everyone the grade was applied to"))
dd_write_csv(records, "derived/records.csv")

# --- 2. Policing: one numerator, three denominators -------------------------

by_race <- read.csv(SIB("policing", "by_race.csv"))
acs     <- read.csv(SIB("policing", "acs_denominators.csv"))

stops <- setNames(by_race$stops, by_race$race)

# Every denominator the policing chapter committed, applied to the same stops.
# The point is not that one of them is right. It is that all three are
# defensible, all three are published by the Census Bureau, and they do not
# agree about how large the disparity is.
den_names <- unique(acs$denominator)
rows <- lapply(den_names, function(d) {
  m <- acs[acs$denominator == d, ]
  pop <- setNames(m$count, m$race)
  common <- intersect(names(stops), names(pop))
  if (!all(c("black", "white") %in% common)) return(NULL)
  rb <- stops[["black"]] / pop[["black"]]
  rw <- stops[["white"]] / pop[["white"]]
  data.frame(denominator = d,
             black_pop = pop[["black"]], white_pop = pop[["white"]],
             black_stops_per_person = rb, white_stops_per_person = rw,
             disparity_ratio = rb / rw)
})
denominators <- do.call(rbind, rows)
denominators <- denominators[order(denominators$disparity_ratio), ]
dd_write_csv(denominators, "derived/denominators.csv")

# The move that needs no denominator at all. A hit rate is searches divided by
# searches -- both numbers are events the institution recorded, so the
# population never enters.
hit <- data.frame(
  race        = by_race$race,
  searched    = by_race$searched,
  contraband  = by_race$contraband_found,
  search_rate = 100 * by_race$searched / by_race$stops,
  hit_rate    = 100 * by_race$contraband_found / by_race$searched)
hit <- hit[order(-hit$hit_rate), ]
dd_write_csv(hit, "derived/hit_rates.csv")

# --- 3. Jury selection: the control group inside the file -------------------

strikes <- read.csv(SIB("jury-selection", "strikes.csv"))
trials  <- read.csv(SIB("jury-selection", "trials.csv"))

# Recompute the printed percentage rather than trusting it: struck/eligible.
strikes$recomputed <- 100 * strikes$struck / strikes$eligible
stopifnot(max(abs(strikes$recomputed - strikes$pct)) < 0.11)

wide <- function(sd) {
  s <- strikes[strikes$side == sd, ]
  setNames(s$recomputed, s$race)
}
st <- wide("state"); df <- wide("defense")

control <- data.frame(
  comparison = c("State strikes Black jurors",
                 "State strikes White jurors",
                 "Defense strikes White jurors",
                 "Defense strikes Black jurors"),
  rate = c(st[["Black"]], st[["White"]], df[["White"]], df[["Black"]]))
control$ratio_within_side <- c(st[["Black"]] / st[["White"]],
                               NA,
                               df[["White"]] / df[["Black"]],
                               NA)
dd_write_csv(control, "derived/control.csv")

# --- 4. Redlining: the unit is the denominator ------------------------------

cities <- read.csv(SIB("redlining", "cities.csv"))

# Pooled: every A-graded person in the country against every D-graded person,
# which compares Cleveland's D areas with Berkeley's A areas.
pooled_a <- sum(cities$a_pop * cities$a_pct_black / 100) / sum(cities$a_pop) * 100
pooled_d <- sum(cities$d_pop * cities$d_pct_black / 100) / sum(cities$d_pop) * 100

unit <- data.frame(
  quantity = c("Cities", "Pooled A-graded, % Black", "Pooled D-graded, % Black",
               "Pooled gap, points", "Median within-city gap, points",
               "Cities where the gap reverses"),
  value = c(nrow(cities), pooled_a, pooled_d, pooled_d - pooled_a,
            median(cities$gap), sum(cities$gap < 0)))
dd_write_csv(unit, "derived/unit.csv")

# --- 5. Checks --------------------------------------------------------------

f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
cm <- function(x) format(round(x), big.mark = ",")

lo <- denominators[1, ]; hi <- denominators[nrow(denominators), ]

chk <- data.frame(
  check = c(
    "Records described",
    "Chapters read, none of them downloaded",
    "Stops in the policing file",
    "Published denominators available for the same stops",
    "Smallest disparity ratio, and its denominator",
    "Largest disparity ratio, and its denominator",
    "Spread between them, as a multiple of the smallest",
    "Hit rate, White drivers searched, %",
    "Hit rate, Black drivers searched, %",
    "Jurors the state found eligible, Black",
    "Jurors the state found eligible, White",
    "Trials in the jury file",
    "Strike rates recomputed from struck/eligible, max disagreement with printed, pts",
    "Cities in the redlining file",
    "Pooled A-to-D gap, points",
    "Median within-city A-to-D gap, points",
    "Cities in which the gap reverses"),
  value = c(
    nrow(records),
    length(unique(records$chapter)),
    cm(sum(by_race$stops)),
    nrow(denominators),
    paste0(f2(lo$disparity_ratio), "x (", lo$denominator, ")"),
    paste0(f2(hi$disparity_ratio), "x (", hi$denominator, ")"),
    paste0(f2(hi$disparity_ratio / lo$disparity_ratio), "x"),
    f1(hit$hit_rate[hit$race == "white"]),
    f1(hit$hit_rate[hit$race == "black"]),
    cm(strikes$eligible[strikes$side == "state" & strikes$race == "Black"]),
    cm(strikes$eligible[strikes$side == "state" & strikes$race == "White"]),
    cm(nrow(trials)),
    f2(max(abs(strikes$recomputed - strikes$pct))),
    nrow(cities),
    f1(pooled_d - pooled_a),
    f1(median(cities$gap)),
    sum(cities$gap < 0)))
dd_write_csv(chk, "derived/checks.csv")

# Hard stops. Each is a way this chapter could be quietly wrong.
stopifnot(
  nrow(records) == 3L,
  nrow(denominators) >= 3L,                       # the argument needs a spread
  hi$disparity_ratio > lo$disparity_ratio * 1.2,  # and the spread must be real
  all(hit$hit_rate > 0 & hit$hit_rate < 100),     # a rate of a rate is still a rate
  hit$hit_rate[hit$race == "white"] >
    hit$hit_rate[hit$race == "black"],            # the finding, stated as a test
  nrow(trials) > 200L,
  abs(sum(strikes$struck) - (902 + 372 + 127 + 1330)) == 0L,
  nrow(cities) == 33L,
  median(cities$gap) > (pooled_d - pooled_a),     # pooling UNDERSTATES the gap
  sum(cities$gap < 0) > 0L)                       # and hides reversals

cat(sprintf("\nrecords.csv      : %d records\n", nrow(records)))
cat(sprintf("denominators.csv : %d denominators, ratios %s to %s\n",
            nrow(denominators), f2(lo$disparity_ratio), f2(hi$disparity_ratio)))
cat(sprintf("control.csv      : state %s%% vs defense %s%%\n",
            f1(st[["Black"]]), f1(df[["White"]])))
cat(sprintf("unit.csv         : pooled %s, within-city median %s, %d reversals\n",
            f1(pooled_d - pooled_a), f1(median(cities$gap)), sum(cities$gap < 0)))
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
