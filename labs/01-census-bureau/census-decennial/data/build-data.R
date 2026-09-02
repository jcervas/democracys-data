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
#    Data Summary File, read for the whole country through the Census data API
#    as the dataset `2020/dec/pl`:
#
#      https://api.census.gov/data/2020/dec/pl
#
#    A KEY IS REQUIRED. The API has needed one since 2025; an unkeyed request
#    302s to missing_key.html. Free from
#    https://api.census.gov/data/key_signup.html; put it in ~/.Renviron as
#      CENSUS_API_KEY='...'
#    It is never written into this file, and the committed derived/ output means
#    the LAB needs no key and no network -- only this rebuild does.
#
#    THE FILE IS DELIVERED STATE BY STATE and the Bureau publishes no national
#    archive of it: fifty-one legacy zips, about 1.3 GB, is the only other route
#    to the same numbers. This chapter used to read one of those zips -- 358 MB
#    unpacked, borrowed from the areal-units chapter, which keeps it in a raw/
#    folder that is not under version control. So the old build could not run
#    for anyone who did not already have that other chapter's download.
#
#    THE SWAP IS CHECKED, NOT ASSUMED. derived/legacy_georgia.csv holds the
#    fifteen quantities the previous build parsed out of the Georgia zip by
#    field position. Every run re-fetches Georgia from the API and compares all
#    fifteen, and stops if one differs.
#
#    TABLE WIDTHS are still measured rather than copied from documentation:
#    they are counted from the API's own variables.json, which lists every cell
#    of every table, and checked against the widths the legacy file was
#    measured at, so a vintage change fails loudly instead of quietly
#    mis-slicing.
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
#   derived/national.csv  What those tables say about the whole country.
#   derived/race.csv      The nation by race and by Hispanic origin, the two
#                         questions the coverage estimates are broken out on.
#   derived/legacy_georgia.csv  Frozen. The fifteen quantities the previous
#                         build read out of the Georgia legacy zip, kept as the
#                         check that the API serves the same file.
#   derived/deadlines.csv Statutory deadline against delivered date.
#   derived/coverage.csv  2020 PES net coverage error by group, with 2010.
#   derived/scale.csv     Those rates put on a scale a reader can hold.
#   derived/cost.csv      What it cost, and what that is per person counted.
#   derived/census-use.csv    How many states are obliged by their own law to
#                             draw districts from the federal count.
#   derived/adjustments.csv   The states that move people before drawing.
#
# Run from this directory:  Rscript build-data.R      (needs CENSUS_API_KEY)
# ---------------------------------------------------------------------------

dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")    # dd_write_csv(): six significant digits

options(scipen = 999, stringsAsFactors = FALSE)

# --- 0. Read the file from the API ------------------------------------------
#
# Two requests: the nation, which is what this chapter reports, and Georgia,
# which is only fetched so that it can be checked against the numbers the
# previous build parsed out of the legacy zip.

KEY <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(KEY)) stop("set CENSUS_API_KEY in ~/.Renviron -- see the header")

BASE <- "https://api.census.gov/data/2020/dec/pl"

# The cells this chapter needs, named as the published technical specification
# names them. P1 is race, P2 race by Hispanic origin, P3 the same as P1 for
# adults, H1 housing, P5 group quarters.
vars <- c(
  total        = "P1_001N",   # everyone
  adults       = "P3_001N",   # 18 and over
  gq           = "P5_001N",   # in group quarters
  hu           = "H1_001N",   # housing units
  hu_occupied  = "H1_002N",
  hu_vacant    = "H1_003N",
  white        = "P1_003N",   # the six single-race cells, P1 3-8
  black        = "P1_004N",
  aian         = "P1_005N",
  asian        = "P1_006N",
  nhpi         = "P1_007N",
  other_race   = "P1_008N",
  two_or_more  = "P1_009N",
  hispanic     = "P2_002N",   # Hispanic origin, any race
  white_nh     = "P2_005N")   # White alone AND not Hispanic

# The API answers with a JSON array of arrays, header row first. Rows are cut
# on the brackets and cells pulled out by their quotes -- no jsonlite, the way
# the other API-reading chapters in this corpus do it, and quote-based because
# NAME can carry a comma.
get_pl <- function(geo) {
  u <- paste0(BASE, "?get=NAME,", paste(vars, collapse = ","), "&for=", geo,
              "&key=", KEY)
  txt <- paste(readLines(url(u), warn = FALSE), collapse = "")
  if (grepl("missing_key", txt, fixed = TRUE))
    stop("the API refused the key in CENSUS_API_KEY -- see the header")
  rows <- regmatches(txt, gregexpr("\\[[^][]*\\]", txt))[[1]]
  cell <- function(r) gsub('"', "", regmatches(r, gregexpr('"[^"]*"', r))[[1]])
  hdr <- cell(rows[1]); got <- cell(rows[2])
  stopifnot(length(hdr) == length(got), all(vars %in% hdr))
  setNames(as.numeric(got[match(vars, hdr)]), names(vars))
}

cat("reading 2020/dec/pl from the Census API --\n")
US <- get_pl("us:*")
GA <- get_pl("state:13")
cat("  national total:", format(US[["total"]], big.mark = ","), "\n")

# --- 1. The API against the legacy file -------------------------------------
#
# The reason this build no longer needs a 358 MB zip out of another chapter.
# legacy_georgia.csv is frozen: it is what the previous build measured out of
# the Georgia P.L. zip by field position. If the API is the same file, every
# one of the fifteen quantities matches.

fx <- read.csv("derived/legacy_georgia.csv", stringsAsFactors = FALSE)
fx_key <- c("Total population" = "total",
            "Population 18 and over" = "adults",
            "Population in group quarters" = "gq",
            "Housing units" = "hu",
            "Occupied housing units" = "hu_occupied",
            "Vacant housing units" = "hu_vacant",
            "White alone" = "white",
            "Black or African American alone" = "black",
            "American Indian or Alaska Native alone" = "aian",
            "Asian alone" = "asian",
            "Native Hawaiian or Other Pacific Islander alone" = "nhpi",
            "Some Other Race alone" = "other_race",
            "Two or more races" = "two_or_more",
            "Hispanic or Latino, of any race" = "hispanic",
            "White alone, not Hispanic" = "white_nh")
stopifnot(setequal(fx$quantity, names(fx_key)))
bad <- fx$quantity[fx$value != GA[fx_key[fx$quantity]]]
cat("legacy-vs-API, Georgia:", nrow(fx), "quantities --", length(bad),
    "mismatches\n")
if (length(bad)) cat("  differ:", paste(bad, collapse = "; "), "\n")
stopifnot(length(bad) == 0)

# --- 2. What the redistricting file contains --------------------------------
#
# THE POINT OF THIS TABLE. Every district in the United States is drawn from
# this file, and it is six tables wide. Not six hundred.
#
# The widths are still MEASURED rather than copied out of documentation -- the
# API publishes a machine-readable list of every variable it serves, so the
# cells of each table are counted by counting them there. The widths the legacy
# Georgia file was measured at are asserted against that count, so a vintage
# that changed a table would fail here instead of quietly mis-slicing.

vj <- paste(readLines(url(paste0(BASE, "/variables.json")), warn = FALSE),
            collapse = "")
# {"P1_001N":{...,"group":"P1",...},...} -- count the cells claiming each group
grp <- regmatches(vj, gregexpr('"group"[[:space:]]*:[[:space:]]*"[^"]*"', vj))[[1]]
grp <- sub('.*"([^"]*)"$', "\\1", grp)
wide <- function(t) sum(grp == t)

tables <- data.frame(
  table = c("P1", "P2", "P3", "P4", "H1", "P5"),
  cells = vapply(c("P1", "P2", "P3", "P4", "H1", "P5"), wide, numeric(1),
                 USE.NAMES = FALSE),
  what_it_counts = c(
    "Everyone, by race",
    "Everyone, by Hispanic origin and race",
    "People 18 and over, by race",
    "People 18 and over, by Hispanic origin and race",
    "Housing units: total, occupied, vacant",
    "People living in group quarters, by type of institution"))
# the widths the legacy file was measured at, in the same order
stopifnot(identical(tables$cells, c(71, 73, 71, 73, 3, 10)))
dd_write_csv(tables, "derived/tables.csv")
NCELL <- sum(tables$cells)

# Of P1's 71 cells, the first nine are the single races and the summary lines.
# Of the remaining 62, five are the "Population of two/three/four/five/six
# races" subtotal lines and 57 are actual combinations: 15 pairs, 20 triples,
# 15 quadruples, 6 quintuples and the single all-six cell. That is where the
# width comes from, and it is a fact about the QUESTION rather than about the
# population -- most of those cells are very small numbers.
COMBOS   <- tables$cells[tables$table == "P1"] - 9L
SUBTOT   <- 5L                       # "Population of N races" subtotal lines
COMBOS_R <- COMBOS - SUBTOT          # 57 real combinations
stopifnot(COMBOS_R == 15 + 20 + 15 + 6 + 1)

# How the width accumulates: the count alone is one number; race makes it 71;
# adding Hispanic origin brings P1+P2 to 144; the 18-and-over line doubles that
# to 288 across P1-P4. The remaining 13 are housing (3) and group quarters (10).
w         <- function(t) tables$cells[tables$table == t]
RACE_CELL <- w("P1") + w("P2") + w("P3") + w("P4")
REST_CELL <- w("H1") + w("P5")
stopifnot(RACE_CELL + REST_CELL == NCELL)

# --- 3. What the six tables say about the country ---------------------------

national <- data.frame(
  quantity = c("Total population",
               "Population 18 and over",
               "Population in group quarters",
               "Housing units",
               "Occupied housing units",
               "Vacant housing units"),
  value = as.numeric(US[c("total", "adults", "gq", "hu",
                          "hu_occupied", "hu_vacant")]))
stopifnot(national$value[national$quantity == "Housing units"] ==
          sum(US[["hu_occupied"]], US[["hu_vacant"]]))   # occupied + vacant
# the published 2020 resident population, to the person
stopifnot(US[["total"]] == 331449281)
dd_write_csv(national, "derived/national.csv")

# --- 4. The two questions the coverage estimates are broken out on ----------
#
# P1 cells 3-8 are the six single-race categories, P1 cell 9 is two or more
# races, and P2 cell 2 is Hispanic origin of any race. These are the same
# categories the Post-Enumeration Survey reports its coverage error for, which
# is why they are pulled out here.

race <- data.frame(
  group = c("White alone", "Black or African American alone",
            "American Indian or Alaska Native alone", "Asian alone",
            "Native Hawaiian or Other Pacific Islander alone",
            "Some Other Race alone", "Two or more races",
            "Hispanic or Latino, of any race"),
  people = as.numeric(US[c("white", "black", "aian", "asian", "nhpi",
                           "other_race", "two_or_more", "hispanic")]))
race$share_of_us <- 100 * race$people / US[["total"]]
# the seven race answers are a partition of everyone; Hispanic origin is not
stopifnot(abs(sum(race$people[1:7]) - US[["total"]]) < 0.5)
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
# A percentage is easy to nod at. The PES rates are national and the counts
# they are applied to are now national too, so this is the population the rate
# was actually measured on rather than an illustration borrowed from one state.

# MATCH THE POPULATION TO THE RATE. The Bureau reports its White rate for the
# NOT-HISPANIC White population, so it has to be applied to that same
# population: P2 cell 5, White alone and not Hispanic. Using P1's "White alone"
# would fold in Hispanic White residents, who are already carried by the
# Hispanic row -- the same people under two rows, at rates of opposite sign.
pick     <- function(g) race$people[race$group == g]
WHITE_NH <- US[["white_nh"]]
stopifnot(WHITE_NH > 0, WHITE_NH < pick("White alone"))

scale <- data.frame(
  group = c("Black or African American alone",
            "Hispanic or Latino, of any race",
            "White alone, not Hispanic"),
  published = c(pick("Black or African American alone"),
                pick("Hispanic or Latino, of any race"),
                WHITE_NH),
  national_rate = c(-3.30, -4.99, 1.64))

# THE ARITHMETIC IS NOT rate * published. A net coverage rate is measured
# against the TRUE population, not the published one: published = true *
# (1 + rate/100). So the implied gap is published/(1 + rate/100) - published,
# which is a larger number than taking the rate off the published count. The
# chapter says so.
scale$implied_people <- with(scale,
  published / (1 + national_rate / 100) - published)
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
            w("P1"), COMBOS_R, SUBTOT))
cat(sprintf("race width    : %d of %d cells exist because of the race question\n",
            RACE_CELL, NCELL))
cat("\nnational.csv  : what the six tables say about the whole country\n")
print(national, row.names = FALSE)
cat("\ndeadlines.csv : both statutory, both missed\n")
print(deadlines[, c("delivery", "due", "delivered", "days_late")],
      row.names = FALSE)
cat("\ncoverage.csv  : 2020 net coverage error, per cent (negative = undercount)\n")
print(coverage, row.names = FALSE)
cat("\nscale.csv     : national rates on the national published counts\n")
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
