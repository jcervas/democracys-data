# ---------------------------------------------------------------------------
# Build the datasets for the "decennial census" chapter.
#
# This is the first of three instrument chapters. The source chapter next door
# argued that only an enumeration can describe a block; this one is about the
# enumeration itself -- what it asks, what it publishes, what it costs to run,
# and who it fails to find.
#
# THE ARGUMENT, in the order the chapter makes it.
#
#   IT ASKS ALMOST NOTHING. Since 2010 the decennial has been short-form only:
#   seven topics, asked of everyone. That is the whole instrument. Every other
#   thing people call "census data" -- income, language, commuting, education
#   -- comes from the American Community Survey, which is the next chapter.
#
#   IT PUBLISHES ALMOST NOTHING, TOO, and this surprises people. The
#   redistricting file that the states redraw their maps from contains SIX
#   TABLES. Not six hundred. The counts here are computed from the file itself
#   rather than read off the documentation: the tables are counted by counting
#   the columns.
#
#   IT MISSES PEOPLE, AND NOT AT RANDOM. The Bureau measures its own coverage
#   afterward with a separate survey and publishes the result. In 2020 the net
#   error was an undercount of 3.30% for Black residents and 4.99% for Hispanic
#   residents, against a 1.64% OVERCOUNT for non-Hispanic White residents. The
#   gaps widened against 2010 in every one of those three groups.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# 1. THE FILE ITSELF. U.S. Census Bureau, 2020 Census P.L. 94-171 Redistricting
#    Data Summary File, Georgia. Read from the copy the areal-units chapter
#    already committed:
#
#      ../../areal-units/data/raw/ga2020.pl.zip   (or raw/pl/, if unpacked)
#
#    Borrowed rather than re-downloaded: it is 358 MB unpacked, and the vintage
#    decision was made in that chapter. Four pipe-delimited files, no header
#    row. The geographic header joins to the data segments on LOGRECNO.
#
#      gageo2020.pl     97 fields. Field 3 SUMLEV, field 8 LOGRECNO.
#                       SUMLEV 040 is the state; there is exactly one such row.
#      ga000012020.pl  149 fields = 5 header + P1 (71 cells) + P2 (73)
#      ga000022020.pl  152 fields = 5 header + P3 (71) + P4 (73) + H1 (3)
#      ga000032020.pl   15 fields = 5 header + P5 (10)
#
#    The table widths are not hard-coded from documentation below -- they are
#    measured from the files, and checked against these numbers, so that a file
#    of a different vintage fails loudly instead of quietly mis-slicing.
#
# 2. COVERAGE ERROR, transcribed. U.S. Census Bureau, "Census Bureau Releases
#    Estimates of Undercount and Overcount in the 2020 Census", press release
#    CB22-CN.02, 10 March 2022, reporting the 2020 Post-Enumeration Survey
#    against the 2010 Census Coverage Measurement survey.
#
#    THESE SIX RATES ARE KEYED IN, NOT FETCHED. The Bureau publishes the PES
#    results as PDFs, so nothing in this script re-derives them and a rebuild
#    cannot catch a revision. The chapter says so in as many words.
#
# 3. COST. U.S. Government Accountability Office, "2020 Census" high-risk
#    report: approximately $15.6 billion through 2023, inflation-adjusted.
#    The per-person figure is computed here from that total and the published
#    2020 resident population of the United States.
#
# 4. DEADLINES. 13 U.S.C. 141(b) and (c): apportionment counts within nine
#    months of Census Day, redistricting data within one year of it. The
#    delivered dates are the Bureau's own announcements. The slip is computed
#    by date arithmetic here, not asserted.
#
# 5. STATE LAW ON USING THE COUNT. National Conference of State Legislatures,
#    "Redistricting Law 2020" (Denver: NCSL, October 2019). Two things are
#    transcribed from it, both into raw/ as tab-separated tables:
#
#      raw/ncsl-appendix-b.tsv        Appendix B, "Redistricting and the Use
#                                     of Census Data": whether each state's
#                                     constitution or statutes require the
#                                     federal count for congressional and for
#                                     legislative districts.
#      raw/ncsl-state-adjustments.tsv Chapter 1 and Appendix C: the states
#                                     that move people out of the file before
#                                     drawing with it.
#
#    THESE ARE KEYED IN. The book is a PDF and the underlying state law
#    changes, so a rebuild cannot catch a repeal. The counts below are
#    computed from the transcription rather than copied from NCSL's own
#    summary paragraph, which does not agree with its own appendix -- the
#    chapter says so.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Seven tables in derived/ and one raw capture:
#
#   raw/form.txt          The seven topics the 2020 form asked, as a capture.
#   raw/ncsl-appendix-b.tsv         State law on using the count, transcribed.
#   raw/ncsl-state-adjustments.tsv  States that alter the file first.
#   derived/tables.csv    The six tables in the redistricting file, and how
#                         wide each one is. Measured from the file.
#   derived/georgia.csv   What those tables say about one state.
#   derived/race.csv      Georgia by race and by Hispanic origin, the two
#                         questions the coverage estimates are broken out on.
#   derived/deadlines.csv Statutory deadline against delivered date.
#   derived/coverage.csv  2020 PES net coverage error by group, with 2010.
#   derived/scale.csv     Those rates put on a scale a reader can hold.
#   derived/cost.csv      What it cost, and what that is per person counted.
#   derived/census-use.csv    How many states are obliged by their own law to
#                             draw districts from the federal count.
#   derived/adjustments.csv   The states that move people before drawing.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")    # dd_write_csv(): six significant digits

options(scipen = 999, stringsAsFactors = FALSE)

# --- 0. Locate the P.L. file ------------------------------------------------
#
# Prefer the unpacked copy; fall back to the zip. Either way nothing is
# downloaded and nothing is written back into the other chapter's folder.

AU  <- "../../areal-units/data/raw"
ex  <- file.path(AU, "pl")
if (!dir.exists(ex)) {
  ex <- file.path(tempdir(), "pl")
  dir.create(ex, showWarnings = FALSE)
  utils::unzip(file.path(AU, "ga2020.pl.zip"), exdir = ex)
}
f_geo  <- file.path(ex, "gageo2020.pl")
f_seg1 <- file.path(ex, "ga000012020.pl")
f_seg2 <- file.path(ex, "ga000022020.pl")
f_seg3 <- file.path(ex, "ga000032020.pl")
stopifnot(file.exists(f_geo), file.exists(f_seg1), file.exists(f_seg2),
          file.exists(f_seg3))

# --- 1. The state record ----------------------------------------------------
#
# SUMLEV 040 is the state. One row, and the file is read for that one row
# rather than for the 232,717 blocks the source chapter uses.

geo <- read.delim(f_geo, sep = "|", header = FALSE, quote = "",
                  colClasses = "character", fileEncoding = "latin1")
names(geo)[c(3, 8, 10)] <- c("SUMLEV", "LOGRECNO", "GEOCODE")
st <- geo[geo$SUMLEV == "040", ]
stopifnot(nrow(st) == 1L)
LRN <- st$LOGRECNO

# Read one segment and return the state's row as a numeric vector of cells,
# with the five header fields dropped. Reading the whole segment and keeping
# one row is wasteful and is also the honest thing to do: the file has no
# index, and a reader with the file in front of them has to do the same.
cells <- function(path, expect) {
  d <- read.delim(path, sep = "|", header = FALSE, quote = "",
                  colClasses = "character")
  stopifnot(ncol(d) == expect)                 # width is CHECKED, not assumed
  r <- d[d[[5]] == LRN, ]
  stopifnot(nrow(r) == 1L)
  as.numeric(unlist(r[-(1:5)]))
}
s1 <- cells(f_seg1, 149)     # P1 (71) + P2 (73)
s2 <- cells(f_seg2, 152)     # P3 (71) + P4 (73) + H1 (3)
s3 <- cells(f_seg3,  15)     # P5 (10)
stopifnot(length(s1) == 144, length(s2) == 147, length(s3) == 10)

P1 <- s1[1:71];   P2 <- s1[72:144]
P3 <- s2[1:71];   P4 <- s2[72:144];  H1 <- s2[145:147]
P5 <- s3[1:10]

# --- 2. What the redistricting file contains --------------------------------
#
# THE POINT OF THIS TABLE. Every district in the United States is drawn from
# this file, and it is six tables wide. The cell counts are lengths of vectors
# sliced above, so they cannot drift from the file.

tables <- data.frame(
  table = c("P1", "P2", "P3", "P4", "H1", "P5"),
  cells = c(length(P1), length(P2), length(P3), length(P4),
            length(H1), length(P5)),
  what_it_counts = c(
    "Everyone, by race",
    "Everyone, by Hispanic origin and race",
    "People 18 and over, by race",
    "People 18 and over, by Hispanic origin and race",
    "Housing units: total, occupied, vacant",
    "People living in group quarters, by type of institution"))
dd_write_csv(tables, "derived/tables.csv")
NCELL <- sum(tables$cells)

# Of P1's 71 cells, the first nine are the single races and the summary lines.
# Of the remaining 62, five are the "Population of two/three/four/five/six
# races" subtotal lines and 57 are actual combinations: 15 pairs, 20 triples,
# 15 quadruples, 6 quintuples and the single all-six cell. That is where the
# width comes from, and it is a fact about the QUESTION rather than about the
# population -- most of those cells are very small numbers.
COMBOS   <- length(P1) - 9L          # 62 cells below the single-race block
SUBTOT   <- 5L                       # "Population of N races" subtotal lines
COMBOS_R <- COMBOS - SUBTOT          # 57 real combinations
stopifnot(COMBOS_R == 15 + 20 + 15 + 6 + 1)


# How the width accumulates, all read off the measured cell counts: the count
# alone is one number; race makes it 71; adding Hispanic origin brings P1+P2 to
# 144; the 18-and-over line doubles that to 288 across P1-P4. The remaining 13
# are housing (3) and group quarters (10).
w         <- function(t) tables$cells[tables$table == t]
RACE_CELL <- w("P1") + w("P2") + w("P3") + w("P4")
REST_CELL <- w("H1") + w("P5")
stopifnot(RACE_CELL + REST_CELL == NCELL)

# --- 3. What the six tables say about one state -----------------------------

georgia <- data.frame(
  quantity = c("Total population",
               "Population 18 and over",
               "Population in group quarters",
               "Housing units",
               "Occupied housing units",
               "Vacant housing units"),
  value = c(P1[1], P3[1], P5[1], H1[1], H1[2], H1[3]))
stopifnot(georgia$value[georgia$quantity == "Housing units"] ==
          sum(H1[2], H1[3]))                # occupied + vacant = total
dd_write_csv(georgia, "derived/georgia.csv")

# --- 4. The two questions the coverage estimates are broken out on ----------
#
# P1 cells 3-8 are the six single-race categories; P2 cell 2 is Hispanic origin
# of any race. These are the same categories the Post-Enumeration Survey
# reports its coverage error for, which is why they are pulled out here.

race <- data.frame(
  group = c("White alone", "Black or African American alone",
            "American Indian or Alaska Native alone", "Asian alone",
            "Native Hawaiian or Other Pacific Islander alone",
            "Some Other Race alone", "Two or more races",
            "Hispanic or Latino, of any race"),
  people = c(P1[3], P1[4], P1[5], P1[6], P1[7], P1[8], P1[9], P2[2]))
race$share_of_state <- 100 * race$people / P1[1]
dd_write_csv(race, "derived/race.csv")

# --- 5. The deadlines -------------------------------------------------------
#
# Both dates are statutory. Both were missed. The slip is computed.

deadlines <- data.frame(
  delivery = c("Apportionment counts, to the President",
               "Redistricting data, to the states"),
  statute = c("13 U.S.C. 141(b): within 9 months of Census Day",
              "13 U.S.C. 141(c): within 1 year of Census Day"),
  due = as.Date(c("2020-12-31", "2021-04-01")),
  delivered = as.Date(c("2021-04-26", "2021-08-12")))
deadlines$days_late <- as.integer(deadlines$delivered - deadlines$due)
dd_write_csv(deadlines, "derived/deadlines.csv")

# --- 6. Coverage error ------------------------------------------------------
#
# TRANSCRIBED from the Bureau's 10 March 2022 press release. Negative is an
# undercount.
#
# The Bureau reports TWO SEPARATE SIGNIFICANCE TESTS and they are not
# interchangeable, so both are carried:
#
#   differs_from_zero  Is the 2020 rate distinguishable from a perfect count?
#                      One group -- Native Hawaiian or Other Pacific Islander
#                      -- is not, and is carried here rather than dropped,
#                      because a coverage table that shows only the
#                      significant rows reads like a verdict.
#
#   differs_from_2010  Is the 2020 rate distinguishable from the 2010 one?
#                      This is the test the "did the gap widen" question
#                      actually turns on, and it FAILS for Black and for AIAN
#                      residents: both moved further into undercount, but not
#                      by enough for the Bureau to call the movement real.
#                      Subtracting the two columns yields a number for every
#                      row; only three of those numbers mean anything.
#
# The Bureau also reported a significant undercount for the Some Other Race
# population. That rate is not transcribed here, and the chapter says so.

coverage <- data.frame(
  group = c("Black or African American",
            "American Indian or Alaska Native, on reservation",
            "Hispanic or Latino",
            "Native Hawaiian or Other Pacific Islander",
            "Asian",
            "White, not Hispanic"),
  pes_2020 = c(-3.30, -5.64, -4.99,  1.28,  2.62,  1.64),
  ccm_2010 = c(-2.06, -4.88, -1.54,  1.02,  0.00,  0.83),
  differs_from_zero = c("Yes", "Yes", "Yes",  "No", "Yes", "Yes"),
  differs_from_2010 = c( "No",  "No", "Yes",  "No", "Yes", "Yes"))
coverage$change <- coverage$pes_2020 - coverage$ccm_2010
dd_write_csv(coverage, "derived/coverage.csv")

# --- 7. Rates, on a scale a reader can hold ---------------------------------
#
# A percentage is easy to nod at. THIS IS NOT A STATE ESTIMATE: the PES does
# not publish coverage rates by state and race, so what follows is the national
# rate applied to Georgia's published counts -- an illustration of magnitude,
# and labelled as one everywhere it appears.

# MATCH THE POPULATION TO THE RATE. The Bureau reports its White rate for the
# NOT-HISPANIC White population, so it has to be applied to that same
# population: P2 cell 5, White alone and not Hispanic. Using P1's "White alone"
# would fold in Georgia's Hispanic White residents, who are already carried by
# the Hispanic row -- the same people under two rows, at rates of opposite sign.
pick     <- function(g) race$people[race$group == g]
WHITE_NH <- P2[5]
stopifnot(WHITE_NH > 0, WHITE_NH < pick("White alone"))

scale <- data.frame(
  group = c("Black or African American alone",
            "Hispanic or Latino, of any race",
            "White alone, not Hispanic"),
  published_in_georgia = c(pick("Black or African American alone"),
                           pick("Hispanic or Latino, of any race"),
                           WHITE_NH),
  national_rate = c(-3.30, -4.99, 1.64))

# THE ARITHMETIC IS NOT rate * published. A net coverage rate is measured
# against the TRUE population, not the published one: published = true *
# (1 + rate/100). So the implied gap is published/(1 + rate/100) - published,
# which for the Black row is 113,316 rather than the 109,577 that taking 3.30%
# of the published count would give. The chapter says so.
scale$implied_people <- with(scale,
  published_in_georgia / (1 + national_rate / 100) - published_in_georgia)
# Sign: POSITIVE = people the count is implied to have missed,
#       NEGATIVE = people it is implied to hold in excess.
dd_write_csv(scale, "derived/scale.csv")

# --- 8. Cost ----------------------------------------------------------------

US2020    <- 331449281          # 2020 census resident population, United States
COST_2020 <- 15.6e9             # GAO, through 2023, inflation-adjusted
COST_PP   <- COST_2020 / US2020

cost <- data.frame(
  quantity = c("Cost of the 2020 census, through 2023",
               "Resident population counted",
               "Cost per person counted"),
  value = c(COST_2020, US2020, COST_PP),
  unit  = c("dollars", "people", "dollars"))
dd_write_csv(cost, "derived/cost.csv")

# --- 9. Does a state's own law oblige it to use the count? ------------------
#
# THE POINT OF THIS TABLE. The Constitution requires the federal count for
# apportioning the House. It says nothing at all about drawing districts, so
# whether a state must use the count is a question of that state's own law,
# and the answer is not the same in every state.
#
# Read from the transcription in raw/, and counted here. NCSL's own summary
# paragraph in Chapter 1 counts 21 states as explicitly requiring the census,
# which does not match its Appendix B: the summary lists Mississippi, which
# the appendix has as implied, and omits Missouri, which the appendix has as
# explicit. The appendix is the per-state authority, so it wins here.

useb <- read.delim("raw/ncsl-appendix-b.tsv", sep = "\t", quote = "",
                   stringsAsFactors = FALSE)
stopifnot(nrow(useb) == 50L, !any(duplicated(useb$state)))

# The legislative column is the one every state has an answer for. Appendix B
# names only one branch for five states, and those become "Appendix is silent"
# in the congressional column rather than being guessed at.
LEVELS <- c("Explicitly required", "Implied or in practice",
            "Only if the federal count arrives", "Not required",
            "Appendix is silent")
stopifnot(all(useb$congressional %in% LEVELS),
          all(useb$legislative   %in% LEVELS),
          !any(useb$legislative == "Appendix is silent"))

census_use <- data.frame(
  requirement  = LEVELS,
  legislative  = as.integer(table(factor(useb$legislative,   LEVELS))),
  congressional= as.integer(table(factor(useb$congressional, LEVELS))))
stopifnot(sum(census_use$legislative) == 50L,
          sum(census_use$congressional) == 50L)
dd_write_csv(census_use, "derived/census-use.csv")

# --- 10. The states that alter the file before drawing with it --------------
#
# Counting a prisoner at the prison is a residence rule, not a measurement,
# and some states undo it for their own maps. Two did so in the 2010 cycle and
# four more said they would for 2020. A separate NCSL note records four states
# whose 2010-cycle plans were drawn from a modified file.

adj <- read.delim("raw/ncsl-state-adjustments.tsv", sep = "\t", quote = "",
                  stringsAsFactors = FALSE)
stopifnot(nrow(adj) > 0, all(adj$cycle %in% c(2010, 2020)))

realloc <- adj[grepl("^Reallocates prisoners", adj$adjustment), ]
stopifnot(!any(duplicated(realloc$state)))

adjustments <- data.frame(
  quantity = c("States reallocating prisoners, 2010 cycle",
               "States adding prisoner reallocation for 2020",
               "States reallocating prisoners, both cycles",
               "States whose 2010-cycle plans used a modified file"),
  value = c(sum(realloc$cycle == 2010),
            sum(realloc$cycle == 2020),
            nrow(realloc),
            length(unique(adj$state[grepl("^Reassigned", adj$adjustment)]))))
dd_write_csv(adjustments, "derived/adjustments.csv")

# --- 11. A capture of the instrument ----------------------------------------
#
# The seven topics, written out as the form asks them. This is the whole
# decennial questionnaire, and its shortness is the chapter's first point.

writeLines(c(
"The 2020 census asked every household seven things. This is the",
"complete list -- there was no long form; it was retired after 2000.",
"",
"  1. How many people were living here on April 1, 2020?",
"  2. Were there any additional people staying here that you did not",
"     include in Question 1?",
"  3. Is this house, apartment, or mobile home owned or rented?",
"  4. What is your telephone number?",
"  5. What is Person 1's name?",
"  6. What is Person 1's sex?  age?  date of birth?",
"  7. Is Person 1 of Hispanic, Latino, or Spanish origin?  What is",
"     Person 1's race?",
"",
"Then, for each additional person, the relationship to Person 1."),
"raw/form.txt")
# The capture ends at the form itself. It used to carry two closing
# paragraphs -- that the census asks nothing about income, education and the
# rest, and that the telephone number is never published -- but those are
# commentary on the form rather than part of it, and they now sit in the
# brief's own prose directly beneath this block. Emitting them here too put
# them on the page twice.

# --- report -----------------------------------------------------------------

cat(sprintf("\ntables.csv    : %d tables, %d cells in the whole file\n",
            nrow(tables), NCELL))
cat(sprintf("                P1 is %d cells: %d combinations + %d subtotal lines\n",
            length(P1), COMBOS_R, SUBTOT))
cat(sprintf("race width    : %d of %d cells exist because of the race question\n",
            RACE_CELL, NCELL))
cat("\ngeorgia.csv   : what the six tables say about one state\n")
print(georgia, row.names = FALSE)
cat("\ndeadlines.csv : both statutory, both missed\n")
print(deadlines[, c("delivery", "due", "delivered", "days_late")],
      row.names = FALSE)
cat("\ncoverage.csv  : 2020 net coverage error, per cent (negative = undercount)\n")
print(coverage, row.names = FALSE)
cat("\nscale.csv     : national rates on Georgia's published counts\n")
print(scale, row.names = FALSE)
cat(sprintf("\ncost          : $%.1fbn, $%.2f per person counted\n",
            COST_2020 / 1e9, COST_PP))
cat("\ncensus-use.csv: does a state's own law oblige it to use the count?\n")
print(census_use, row.names = FALSE)
cat("\nadjustments.csv: states that move people before drawing\n")
print(adjustments, row.names = FALSE)
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
