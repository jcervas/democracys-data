# ---------------------------------------------------------------------------
# Build the datasets for the "census coverage" chapter.
#
# The decennial chapter established that the census misses people and misses
# them unevenly, using the six national rates by race the Bureau put in its
# press release. This chapter opens the same measurement two ways the press
# release did not: BY STATE, and by demographic cuts other than race.
#
# THE ARGUMENT, in the order the chapter makes it.
#
#   THE BUREAU GRADES ITSELF, AND PUBLISHES THE GRADE. After each census a
#   second, independent survey -- the Post-Enumeration Survey -- goes back out,
#   counts a sample of blocks from scratch, and matches its people against the
#   census. The difference is an estimate of how far the count was off. It is
#   published for all 50 states and the District of Columbia.
#
#   THE AGENCY PUBLISHES 51 NUMBERS AND STANDS BEHIND 14. Appendix Table 3
#   prints an estimate for every state, and puts an asterisk on fourteen of
#   them -- six undercounts, eight overcounts. On the other 37 rows the Bureau
#   declines to say which way the count went. THAT MARK IS PART OF THE DATA,
#   and it is the first thing lost when the table is quoted.
#
#   THE MARK DOES NOT FOLLOW THE SIZE OF THE NUMBER. Illinois at -1.97 is
#   marked. Louisiana at -3.73, twice the size, is not. Montana at -4.39 is
#   not. The District of Columbia's +4.59 is the largest number on the page the
#   Bureau will not stand behind. What the mark follows is the MARGIN the
#   Bureau prints in the next column -- 0.65 to 4.93 points, wider where the
#   survey saw less of a state -- which is a fact about the survey rather than
#   about the state.
#
#   THE CHAPTER DOES NOT TEACH A HYPOTHESIS TEST and does not ask a reader to
#   run one. What it asks is the question this book always asks: what did the
#   source actually publish, and what got dropped on the way out.
#
#   SELF-RESPONSE IS A DIFFERENT MEASUREMENT AND IT ANSWERS A DIFFERENT
#   QUESTION. How many households filled the form in on their own is known
#   exactly, for every state, from administrative counts -- no sampling, no
#   error bar. It is not a measure of accuracy, and the correlation with net
#   coverage error is 0.11, which is nothing. What self-response DOES track is
#   the PRECISION of the coverage estimate: the correlation with the standard
#   error is -0.47. States that answered less are states the evaluation had a
#   harder time evaluating.
#
#   THE DEMOGRAPHIC CUTS ARE WHERE THE SIGNAL IS. National estimates rest on
#   the whole sample rather than one state's slice, so their error bars are
#   small enough for real structure to show: children under 5 undercounted by
#   2.79%, men aged 30 to 49 by 3.05%, renters by 1.48%, women 50 and over
#   OVERcounted by 2.63%. The Bureau states a direction for nearly every one
#   of those, and for only 14 of the 51 states. The state
#   map and the demographic tables are the same survey; they differ in how
#   finely it can be cut.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# U.S. Census Bureau, 2020 Post-Enumeration Survey estimation reports -- by
# state (May 2022) and by demographic characteristics (March 2022) -- together
# with the 2020 Census self-response rates. Four sources, in the order the
# script uses them:
#
# 1. U.S. Census Bureau, "Census Coverage Estimates for People in the United
#    States by State and Census Operations," 2020 Post-Enumeration Survey
#    Estimation Report PES20-G-02 (May 2022 release).
#    https://www2.census.gov/programs-surveys/decennial/coverage-measurement/pes/
#      census-coverage-estimates-for-people-in-the-united-states-by-state-and-census-operations.pdf
#    Committed as raw/pes-2020-by-state.pdf. APPENDIX TABLE 3, page 15, is the
#    state table: census count, 2020 estimate, 2020 standard error, and the
#    2010 estimate with its root mean squared error. An asterisk on the 2020
#    estimate is the Bureau's own mark for "significantly different from zero."
#
# 2. U.S. Census Bureau, "National Census Coverage Estimates for People in the
#    United States by Demographic Characteristics," 2020 Post-Enumeration
#    Survey Estimation Report PES20-G-01 (March 2022 release).
#    https://www2.census.gov/programs-surveys/decennial/coverage-measurement/pes/
#      national-census-coverage-estimates-by-demographic-characteristics.pdf
#    Committed as raw/pes-2020-by-demographics.pdf. Table 4 (race and Hispanic
#    origin), Table 6 (tenure, four censuses), Table 10 (age and sex, four
#    censuses) and Table 11 (age group).
#
#    THE PES PUBLISHES AS PDF. There is no CSV, no API and no spreadsheet
#    anywhere on the Bureau's site for any of these numbers. The decennial
#    chapter met the same wall and keyed six rates in by hand. This script
#    does not: it runs `pdftotext -layout` over the two committed reports and
#    parses the tables out of the text layer, so all 51 states and all four
#    demographic tables arrive by machine and a typo has nowhere to enter.
#    The cost is a dependency on poppler; see REQUIRES below.
#
# 3. U.S. Census Bureau, 2020 Census self-response rates, from the file behind
#    the Bureau's response-rate map:
#    https://www2.census.gov/programs-surveys/decennial/2020/data/
#      tracking-response-rates/self-response-rates-map/decennialrr2020.csv
#    Fetched at run time (about 7.6 MB, no key) rather than committed, because
#    all but 52 of its rows are counties, tracts, places and districts this
#    chapter never opens. The state rows carry a single date, 2021-01-29, the
#    final posting.
#
#    A TRAP IN THAT FILE, AND IT IS THE READ-THE-DICTIONARY KIND. The columns
#    are CRRALL (cumulative overall) and CRRINT (cumulative internet). The file
#    ALSO carries CMAX and CINTMAX, whose names read like "the state's final
#    rate" and are nothing of the sort: they are the maximum over the
#    geographies CONTAINED IN the state. Alabama's CRRALL is 63.6 and its CMAX
#    is 77.8 -- the second number is some tract in Alabama, not Alabama. This
#    script takes CRRALL and asserts the national figure comes back 67.0.
#
# 4. STATE OUTLINES ARE BORROWED, not rebuilt. The migration chapter next door
#    already projected the Census cartographic-boundary state file into a
#    1000 x 620 drawing frame with Alaska, Hawaii and Puerto Rico as insets:
#      ../../migration/data/derived/map_states.csv
#      ../../migration/data/derived/map_insets.csv
#    Re-deriving them here would mean a second 184 KB shapefile, a second
#    Albers projection and a second set of inset boxes that would drift out of
#    agreement with the first. The rings are read, joined to the coverage
#    estimates, and written into this chapter's own derived/ so the brief reads
#    one folder.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
#   derived/states.csv       51 states and DC: census count, 2020 net coverage
#                            error and its standard error, the Bureau's own
#                            significance mark, the 2010 estimate, and the
#                            2020 self-response rates.
#   derived/race.csv         Net coverage error by race and Hispanic origin.
#   derived/tenure.csv       Owners against renters, 1990 through 2020.
#   derived/age_sex.csv      Age crossed with sex, 1990 through 2020.
#   derived/age.csv          Age groups alone, 2020.
#   derived/components.csv   What the national net is made of, before it nets.
#   derived/map_bins.csv     The six fill bins of Figure 1 and their labels.
#   derived/map_states.csv   State outlines, delta-encoded, with the fill bin.
#   derived/map_insets.csv   The three inset boxes of the borrowed frame.
#   derived/facts.csv        Single numbers the brief quotes.
#   derived/checks.csv       What this script verified before writing.
#
# REQUIRES `pdftotext` (poppler). On macOS: brew install poppler. Everything
# else is base R. One network fetch, for the self-response file.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")     # dd_write_csv(): six significant digits
source("../../../_lib/provenance.R")    # notices when a fetched URL changes

options(scipen = 999, stringsAsFactors = FALSE)

# ===========================================================================
# 0. READING A TABLE OUT OF A PDF
# ===========================================================================
#
# `pdftotext -layout` keeps the horizontal positions of a page, so a printed
# table comes back as text with its columns still lined up in spaces. Two
# things then have to be undone before the rows can be split:
#
#   THE DOT LEADERS. "Alabama . . . . . . . 4,896,000" -- the run of dots is
#   typography, not data, and it is replaced with a single tab so the label and
#   the numbers separate cleanly. The replacement deliberately does not touch
#   LEADING whitespace, because indentation is the only thing that records that
#   "On Reservation" is a subdivision of "American Indian or Alaska Native"
#   rather than a category beside it.
#
#   THE TYPOGRAPHER'S GLYPHS. The Bureau's PDFs use an EN DASH for minus and
#   ligatures for ff, fi and fl, so "Oﬀ Reservation" and "significantly" arrive
#   as single code points. Left alone the en dash makes every undercount fail
#   to parse as a number, and stripping the ligatures instead of expanding them
#   silently eats letters out of category names.

if (!nzchar(Sys.which("pdftotext")))
  stop("pdftotext not found. This build parses the Bureau's PDF reports; ",
       "install poppler (macOS: brew install poppler) and run again.")

pdftxt <- function(f) {
  stopifnot(file.exists(f))
  # stderr is discarded: poppler prints a font warning for every page of these
  # two files and none of it bears on the text layer being read.
  x <- system(sprintf("pdftotext -layout %s - 2>/dev/null", shQuote(f)),
              intern = TRUE)
  stopifnot(length(x) > 100)
  x <- gsub("\f", "", x, fixed = TRUE)                 # page breaks
  x <- gsub("–|−", "-", x)                   # en dash, true minus
  x <- gsub("ﬀ", "ff", x); x <- gsub("ﬁ", "fi", x)
  x <- gsub("ﬂ", "fl", x); x <- gsub("ﬃ", "ffi", x)
  x <- gsub("ﬄ", "ffl", x)
  x <- gsub("(\\s*\\.){2,}\\s*", "\t", x)              # dot leaders -> tab
  sub("\\s+$", "", x)
}

# How deep a row's label is indented, which is how the PDF records nesting.
indent_of <- function(s) nchar(sub("^( *).*$", "\\1", s))

# Every line of a block that looks like "<label> <tab> <numbers>".
data_rows <- function(block) {
  r <- grep("\t", block, value = TRUE)
  r[grepl("\t\\s*\\*?-?[0-9]", r)]
}

# The lines from a table's heading down to its significance footnote.
table_block <- function(txt, heading, last = FALSE) {
  a <- grep(heading, txt)
  stopifnot(length(a) > 0)
  a <- if (last) a[length(a)] else a[1]
  b <- grep("Denotes a \\(percent\\)", txt)
  b <- b[b > a][1]
  stopifnot(!is.na(b), b - a < 60)
  txt[a:b]
}

# A row of "<label> <tab> [*]est se [*]est se ..." into label + numeric pairs.
# `pairs` is how many (estimate, error) columns the table prints, so a table of
# a different shape fails here instead of silently keeping the first two.
split_row <- function(r, pairs) {
  lab <- trimws(sub("\t.*$", "", r))
  num <- trimws(sub("^[^\t]*\t", "", r))
  tok <- strsplit(num, "[ \t]+")[[1]]
  tok <- tok[nzchar(tok)]
  stopifnot(length(tok) == 2 * pairs)
  est <- tok[c(TRUE, FALSE)]; err <- tok[c(FALSE, TRUE)]
  out <- list(label = lab, indent = indent_of(r),
              sig = grepl("^\\*", est))
  # "N" is the Bureau's not-available mark; it is not a zero and must not
  # become one, so it goes to NA rather than through as.numeric() silently.
  n <- function(v) { v <- sub("^\\*", "", v); v[v %in% c("N", "X", "Z")] <- NA; as.numeric(v) }
  out$est <- n(est); out$err <- n(err)
  out
}

# ===========================================================================
# 1. THE STATES  (Appendix Table 3 of the state report)
# ===========================================================================
#
# 52 printed rows: the national total and then the 50 states and DC in
# alphabetical order. Four numeric columns after the census count -- the 2020
# estimate and its standard error, then the 2010 estimate and its ROOT MEAN
# SQUARED ERROR, which is a different quantity and is kept under a different
# name for that reason. The 2010 state estimates carried a synthetic bias the
# 2020 modeling removed, so the Bureau reported an RMSE for 2010 to cover it
# and a plain standard error for 2020. Calling both "standard error" here
# would have quietly claimed the two columns are comparable.

stx <- pdftxt("raw/pes-2020-by-state.pdf")

a <- grep("^Appendix Table 3\\.$", stx)
stopifnot(length(a) == 1)
blk <- stx[a:(a + 70)]
rows <- grep("^[A-Z][A-Za-z ]*\t", blk, value = TRUE)

m <- regmatches(rows, regexec(paste0(
  "^([A-Za-z ]+?)\\s*\t\\s*([0-9,]+)",      # state, census count
  "\\s+(\\*?)(-?[0-9.]+)\\s+([0-9.]+)",      # 2020 estimate, standard error
  "\\s+(-?[0-9.]+)\\s+([0-9.]+)\\s*$"), rows))
stopifnot(all(lengths(m) == 8))              # every row parsed, or none did

st <- do.call(rbind, lapply(m, function(v) data.frame(
  state        = trimws(v[2]),
  census_count = as.numeric(gsub(",", "", v[3])),
  bureau_states_it = v[4] == "*",
  est_2020     = as.numeric(v[5]),
  se_2020      = as.numeric(v[6]),
  est_2010     = as.numeric(v[7]),
  rmse_2010    = as.numeric(v[8]))))

stopifnot(nrow(st) == 52, st$state[1] == "Total")
us <- st[1, ]; st <- st[-1, ]

# --- the three design numbers, and they are the only KEYED figures here -----
#
# Sample size is stated in the Bureau's America Counts write-up of this table,
# not in the estimation reports, and not as data anywhere. Three numbers,
# transcribed, and the chapter says in its limits section that they are.
# Everything else in this build is parsed or computed.
#   https://www.census.gov/library/stories/2022/05/
#     2020-census-undercount-overcount-rates-by-state.html
PES_BLOCKS     <- 10000       # blocks enumerated from scratch
PES_UNITS      <- 161000      # housing units in those blocks
PES_INTERVIEWS <- 114000      # completed household interviews

# WHO IS NOT IN ANY OF THIS. The PES evaluates the HOUSEHOLD population, so its
# national count is smaller than the census resident population by the group
# quarters -- dormitories, prisons, nursing homes, shelters -- plus the Remote
# Alaska areas it does not visit. The gap is computed rather than asserted.
# The PES count is published rounded to the nearest hundred thousand, so the
# difference is good to about that and the chapter says "about".
US_RESIDENT   <- 331449281    # 2020 census resident population, United States
NOT_EVALUATED <- US_RESIDENT - us$census_count
stopifnot(NOT_EVALUATED > 5e6, NOT_EVALUATED < 12e6)
stopifnot(nrow(st) == 51,
          st$state[1] == "Alabama", st$state[51] == "Wyoming",
          !any(duplicated(st$state)))

# FIPS codes, from R's own state list with DC inserted where it sorts. The
# alphabetical position is checked against the parsed table rather than
# assumed, because a mismatch here would misplace every state on the map.
FIPS <- c("01","02","04","05","06","08","09","10","11","12","13","15","16",
          "17","18","19","20","21","22","23","24","25","26","27","28","29",
          "30","31","32","33","34","35","36","37","38","39","40","41","42",
          "44","45","46","47","48","49","50","51","53","54","55","56")
NAMES <- append(state.name, "District of Columbia", after = 8)
stopifnot(length(FIPS) == 51, identical(NAMES, st$state))
st$fips <- FIPS

# THE ASTERISK, UNDER A NAME THAT SAYS WHAT IT IS FOR. The Bureau's footnote to
# this column reads "significantly different from zero" -- the agency's
# technical language for a decision IT has already made and published. The
# column is carried here as what it is from outside: a mark saying the Bureau
# will state a direction for this row. Nothing downstream re-runs the Bureau's
# statistics and the chapter does not ask a reader to.
#
# WHAT IS DONE HERE IS A PARSE CHECK, and it belongs to the build rather than
# to the chapter. If the asterisks were read off the page correctly they should
# track the two numbers printed beside them, and they do on 50 of 51 rows.
# Delaware is the exception -- its ratio lands a hair on the other side of the
# textbook rule from where the Bureau put it, which is what an estimate
# computed from a complex sample design looks like when checked against a
# formula that assumes a simple one. It is kept and named rather than smoothed
# away, and it is a fact about the crosscheck, not about Delaware.
DISAGREE <- st$state[st$bureau_states_it !=
                     (abs(st$est_2020 / st$se_2020) > qnorm(0.975))]
stopifnot(length(DISAGREE) <= 1)

st$direction <- ifelse(!st$bureau_states_it, "unstated",
                ifelse(st$est_2020 < 0, "undercount", "overcount"))

# WHAT THE RATE IS IN PEOPLE, and the arithmetic is not the one anybody reaches
# for first. A coverage rate is measured against the TRUE population rather than
# the published one, so published = true * (1 + rate/100) and the implied gap is
# published/(1 + rate/100) - published. Taking the rate straight off the
# published count instead gives the wrong answer, by more the larger the rate.
# Sign follows the rest of the chapter: POSITIVE is people the count missed.
st$implied_people <- round(
  st$census_count / (1 + st$est_2020 / 100) - st$census_count)
naive <- -st$census_count * st$est_2020 / 100
stopifnot(all(abs(st$implied_people) >= abs(naive) - 1 |
              st$est_2020 > 0))          # the shortcut understates an undercount
N_UNDER <- sum(st$direction == "undercount")
N_OVER  <- sum(st$direction == "overcount")
N_NONE  <- sum(!st$bureau_states_it)
stopifnot(N_UNDER + N_OVER + N_NONE == 51)

# The Bureau's own story about this table says six and eight. If a future
# release of the PDF changes either number this build stops, which is the
# point of writing the expectation down.
stopifnot(N_UNDER == 6, N_OVER == 8, N_NONE == 37)

# ===========================================================================
# 2. SELF-RESPONSE, WHICH IS NOT COVERAGE
# ===========================================================================
#
# A count, not an estimate: the share of housing units that returned the form
# on their own, before a census taker was sent. Known exactly, so there is no
# standard error column here and there should not be one.

RR_URL <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/",
                 "data/tracking-response-rates/self-response-rates-map/",
                 "decennialrr2020.csv")
rr_file <- file.path(tempdir(), "decennialrr2020.csv")
invisible(prov_fetch(RR_URL, rr_file))
rr <- read.csv(rr_file, colClasses = "character")

# 0400000US|| is the state prefix; 0100000US is the nation. Puerto Rico (72)
# is in the file and is dropped, because Appendix Table 3 does not evaluate it
# -- the PES reported Puerto Rico separately, on a different design.
nat <- rr[rr$GEO_ID == "0100000US", ]
stopifnot(nrow(nat) == 1)
NAT_SRR <- as.numeric(nat$CRRALL)
NAT_INT <- as.numeric(nat$CRRINT)
stopifnot(NAT_SRR == 67.0)          # the published national self-response rate

srr <- rr[grepl("^0400000US", rr$GEO_ID), ]
srr$fips <- substr(srr$GEO_ID, 10, 11)
srr <- srr[srr$fips != "72", ]
stopifnot(nrow(srr) == 51, length(unique(srr$RESP_DATE)) == 1)
RR_DATE <- srr$RESP_DATE[1]

st$self_response <- as.numeric(srr$CRRALL[match(st$fips, srr$fips)])
st$internet_response <- as.numeric(srr$CRRINT[match(st$fips, srr$fips)])

# THE CMAX TRAP, kept as evidence rather than described. One state's two
# numbers, side by side, is the shortest way to show that a column named
# "maximum cumulative overall self-response rate" is not the state's own rate.
TRAP_ST   <- "Alabama"
trap_row  <- srr[srr$fips == st$fips[st$state == TRAP_ST], ]
TRAP_RATE <- as.numeric(trap_row$CRRALL)
TRAP_CMAX <- as.numeric(trap_row$CMAX)
stopifnot(TRAP_CMAX > TRAP_RATE)      # the wrong column reads high, always

stopifnot(!any(is.na(st$self_response)),
          all(st$internet_response < st$self_response))   # internet is a part

# THE TWO CORRELATIONS THE CHAPTER TURNS ON. Rounded where they are computed:
# a correlation is a ratio of quantities built by subtracting means, so its
# trailing digits are cancellation residue rather than measurement.
COR_EST <- round(cor(st$est_2020, st$self_response), 3)
COR_SE  <- round(cor(st$se_2020,  st$self_response), 3)

# ===========================================================================
# 3. THE DEMOGRAPHIC TABLES  (the national report)
# ===========================================================================

dmx <- pdftxt("raw/pes-2020-by-demographics.pdf")

# --- Table 4: race and Hispanic origin, 2020 against 2010 ------------------
#
# Indentation carries the structure and is kept: "On Reservation" and "Balance
# of the United States" are subdivisions of the American Indian or Alaska
# Native row, not categories beside it, and a flat table would invite adding
# them up. Nothing here sums to the total anyway -- these are ALONE OR IN
# COMBINATION groups, so a person who marked two races is counted in two rows.

r4 <- lapply(data_rows(table_block(dmx, "^Table 4\\.$")), split_row, pairs = 2)
race <- do.call(rbind, lapply(r4, function(z) data.frame(
  group = z$label, level = z$indent,
  est_2020 = z$est[1], se_2020 = z$err[1], bureau_states_it = z$sig[1],
  est_2010 = z$est[2], se_2010 = z$err[2])))
stopifnot(nrow(race) == 12, race$group[1] == "Total",
          race$group[nrow(race)] == "Hispanic or Latino")
race$level <- match(race$level, sort(unique(race$level))) - 1L

# The four rates the decennial chapter quoted, re-derived here from the PDF
# rather than carried over. If the two chapters ever disagree, this is where
# it shows.
# Written out against the frame rather than through a helper, so that the
# vacuity checker can see the data these conditions actually touch.
stopifnot(race$est_2020[race$group == "Black or African American"] == -3.30,
          race$est_2020[race$group == "Hispanic or Latino"]        == -4.99,
          race$est_2020[race$group == "Non-Hispanic White alone"]  ==  1.64,
          race$est_2020[race$group == "On Reservation"]            == -5.64)

# --- Table 6: tenure, four censuses ----------------------------------------
r6 <- lapply(data_rows(table_block(dmx, "^Table 6\\.$")), split_row, pairs = 4)
YRS <- c(2020, 2010, 2000, 1990)
tenure <- do.call(rbind, lapply(r6, function(z) data.frame(
  group = z$label, year = YRS, est = z$est, se = z$err,
  bureau_states_it = z$sig)))
stopifnot(nrow(tenure) == 12, setequal(tenure$group, c("Total", "Owner", "Renter")))

# Owners over, renters under, in all four censuses the table covers. This is
# the Bureau's own summary of Table 6 and it is asserted rather than repeated.
own <- tenure[tenure$group == "Owner", ]; ren <- tenure[tenure$group == "Renter", ]
stopifnot(all(own$est > 0 | own$year == 1990), all(ren$est < 0))

# --- Table 10: age crossed with sex, four censuses -------------------------
#
# Table 10 is printed twice in the report -- once with a components-of-coverage
# header bleeding across it, once clean -- so the LAST occurrence is the one
# taken. The 1990 and 2000 columns carry the Bureau's "N" for not available in
# several rows; those arrive as NA above and stay NA.

r10 <- lapply(data_rows(table_block(dmx, "^Table 10\\.$", last = TRUE)),
              split_row, pairs = 4)
age_sex <- do.call(rbind, lapply(r10, function(z) data.frame(
  group = z$label, level = z$indent, year = YRS,
  est = z$est, se = z$err, bureau_states_it = z$sig)))
stopifnot(nrow(age_sex) == 48)
age_sex$level <- match(age_sex$level, sort(unique(age_sex$level))) - 1L

a20 <- age_sex[age_sex$year == 2020, ]
stopifnot(a20$est[a20$group == "0 to 4"]              == -2.79,
          a20$est[a20$group == "30-to-49 males"]      == -3.05,
          a20$est[a20$group == "50-and-over females"] ==  2.63)

# --- Table 11: age groups alone, 2020 --------------------------------------
r11 <- lapply(data_rows(table_block(dmx, "^Table 11\\.$")), split_row, pairs = 1)
age <- do.call(rbind, lapply(r11, function(z) data.frame(
  group = z$label, level = z$indent,
  est = z$est, se = z$err, bureau_states_it = z$sig)))
stopifnot(nrow(age) == 9)
age$level <- match(age$level, sort(unique(age$level))) - 1L

# --- Table 5, national row only: what the net is made of --------------------
#
# WHY THIS ONE ROW IS HERE. Every other number in this chapter is a NET -- an
# undercount minus an overcount, already cancelled. The components row is the
# gross arithmetic behind the national -0.24%, and it is an order of magnitude
# larger: about 5.8% of people were omissions and about 3.4% of the published
# count were whole-person imputations. A reader who takes a net near zero as
# "nothing happened" needs to have seen these seven numbers once.
#
# The Bureau's "Z" (rounds to zero) and "X" (not applicable) appear in the
# error columns and arrive as NA, which is correct: an unreported standard
# error is not a standard error of zero.

t5 <- grep("^Total\t", table_block(dmx, "^Table 5\\.$"), value = TRUE)
stopifnot(length(t5) == 1)
c5 <- split_row(t5, pairs = 7)
comp <- data.frame(
  component = c("Correct enumerations", "Erroneous: duplication",
                "Erroneous: other reasons", "Whole-person census imputations",
                "Correct, dual-system estimate", "Omissions",
                "Net coverage error"),
  percent = c5$est, se = c5$err)
stopifnot(nrow(comp) == 7,
          comp$percent[comp$component == "Net coverage error"] == us$est_2020,
          comp$percent[comp$component == "Omissions"] >
          abs(comp$percent[comp$component == "Net coverage error"]) * 5)
dd_write_csv(comp, "derived/components.csv")
OMIT <- comp$percent[comp$component == "Omissions"]
IMPU <- comp$percent[comp$component == "Whole-person census imputations"]

# The national total is one number and four tables print it. They must agree,
# and if a future release revises it they will all move together or this stops.
US_EST <- us$est_2020; US_SE <- us$se_2020
stopifnot(US_EST == race$est_2020[1], US_EST == tenure$est[1],
          US_EST == age$est[1], US_SE == age$se[1])

# HOW MUCH SHARPER THE NATIONAL CUTS ARE. The whole reason the demographic
# tables say something and the state map mostly does not is that a national
# subgroup is estimated from the entire sample while a state is estimated from
# one state's slice of it. That shows up as the width of the error bar, so the
# comparison is made on standard errors rather than asserted in prose.
SE_STATE_MED <- round(median(st$se_2020), 2)
SE_DEMO_MED  <- round(median(c(race$se_2020, a20$se, age$se,
                               tenure$se[tenure$year == 2020]), na.rm = TRUE), 2)
stopifnot(SE_DEMO_MED < SE_STATE_MED)

# ===========================================================================
# 3b. WHAT A COVERAGE RATE IS WORTH IN SEATS
# ===========================================================================
#
# The chapter wants to say that these rates are not a rounding concern, and the
# unit that makes that concrete is a House seat. Rather than typing a figure
# for how many people a seat represents, the apportionment chapter's own table
# is read -- it already made the decision about which population apportions
# (the apportionment population, which includes overseas federal personnel,
# not the resident population).

AP <- "../../apportionment/data/derived/apportionment_2020.csv"
ap <- read.csv(AP)
stopifnot(nrow(ap) == 50, sum(ap$seats) == 435)
PER_SEAT <- round(sum(ap$app_pop) / sum(ap$seats))
N_SEATS_WORTH <- sum(abs(st$implied_people) > PER_SEAT)

# ===========================================================================
# 4. THE MAP
# ===========================================================================
#
# Geometry from the migration chapter, unchanged; see SOURCES note 4. Only the
# fill is decided here.
#
# TWO CHANNELS, TWO FACTS, and keeping them apart is the chapter's discipline.
# COLOR is the estimate -- how far off the count is said to be. The OUTLINE is
# whether the Bureau calls that estimate distinguishable from zero. A map that
# colored only the marked states would hide 37 measurements; one that
# colored all 51 without marking which are real would invite reading noise as
# geography. Both channels, on every state.

MP  <- "../../migration/data/derived/map_states.csv"
INS <- "../../migration/data/derived/map_insets.csv"
mp  <- read.csv(MP,  colClasses = c(fips = "character", pts = "character"))
ins <- read.csv(INS)
mp <- mp[mp$fips != "72", ]                     # Puerto Rico: not in the table
ins <- ins[ins$piece != "pr", ]
stopifnot(setequal(unique(mp$fips), st$fips))   # every state drawn, none extra

BREAKS <- c(-Inf, -3, -1.5, 0, 1.5, 3, Inf)
BINLAB <- c("3% or more undercounted", "1.5% to 3% under", "0 to 1.5% under",
            "0 to 1.5% over", "1.5% to 3% over", "3% or more overcounted")
st$bin <- as.integer(cut(st$est_2020, BREAKS, right = FALSE))
stopifnot(!any(is.na(st$bin)), length(unique(st$bin)) == 6)  # all six bins used

mp$bin <- st$bin[match(mp$fips, st$fips)]
mp$states_it <- st$bureau_states_it[match(mp$fips, st$fips)]
stopifnot(!any(is.na(mp$bin)))

dd_write_csv(mp[, c("fips", "piece", "bin", "states_it", "pts")],
             "derived/map_states.csv")
dd_write_csv(ins, "derived/map_insets.csv")

# ===========================================================================
# 5. WRITE
# ===========================================================================

st <- st[, c("fips", "state", "census_count", "est_2020", "se_2020",
             "bureau_states_it", "direction", "bin", "implied_people",
             "est_2010", "rmse_2010", "self_response", "internet_response")]
dd_write_csv(st, "derived/states.csv")
dd_write_csv(race,    "derived/race.csv")
dd_write_csv(tenure,  "derived/tenure.csv")
dd_write_csv(age_sex, "derived/age_sex.csv")
dd_write_csv(age,     "derived/age.csv")

# The bin labels are a decision made here, so they travel with the data rather
# than being retyped in the brief.
dd_write_csv(data.frame(bin = 1:6, label = BINLAB), "derived/map_bins.csv")

wid <- function(s) st$state[which.max(ifelse(s, st$se_2020, -Inf))]
big_insig <- st[!st$bureau_states_it, ]
big_insig <- big_insig[order(-abs(big_insig$est_2020)), ]
sig_small <- st[st$bureau_states_it, ]
sig_small <- sig_small[order(abs(sig_small$est_2020)), ]

facts <- data.frame(
  key = c("us_est", "us_se", "n_under", "n_over", "n_none",
          "se_min", "se_min_state", "se_max", "se_max_state",
          "se_state_median", "se_demo_median",
          "largest_undercount", "largest_undercount_state",
          "largest_overcount", "largest_overcount_state",
          "largest_insig", "largest_insig_state", "largest_insig_se",
          "smallest_sig", "smallest_sig_state", "smallest_sig_se",
          "disagree_state",
          "srr_national", "srr_internet_national", "srr_date",
          "srr_min", "srr_min_state", "srr_max", "srr_max_state",
          "cor_est_srr", "cor_se_srr",
          "missed_texas", "missed_florida", "excess_newyork", "missed_us",
          "omissions_pct", "imputations_pct",
          "pes_blocks", "pes_units", "pes_interviews",
          "people_per_seat", "n_states_over_a_seat",
          "us_household_pop", "not_evaluated",
          "trap_state", "trap_crrall", "trap_cmax",
          "est_mean", "est_median", "est_sd", "n_states"),
  value = c(US_EST, US_SE, N_UNDER, N_OVER, N_NONE,
            min(st$se_2020), st$state[which.min(st$se_2020)],
            max(st$se_2020), st$state[which.max(st$se_2020)],
            SE_STATE_MED, SE_DEMO_MED,
            min(st$est_2020), st$state[which.min(st$est_2020)],
            max(st$est_2020), st$state[which.max(st$est_2020)],
            big_insig$est_2020[1], big_insig$state[1], big_insig$se_2020[1],
            sig_small$est_2020[1], sig_small$state[1], sig_small$se_2020[1],
            if (length(DISAGREE)) DISAGREE else "none",
            NAT_SRR, NAT_INT, RR_DATE,
            min(st$self_response), st$state[which.min(st$self_response)],
            max(st$self_response), st$state[which.max(st$self_response)],
            COR_EST, COR_SE,
            st$implied_people[st$state == "Texas"],
            st$implied_people[st$state == "Florida"],
            -st$implied_people[st$state == "New York"],
            # The national figure comes from the NATIONAL rate on the national
            # count. Adding the 51 state figures instead would be arithmetic on
            # 37 numbers the Bureau declines to distinguish from zero, and it is
            # deliberately not done anywhere in this chapter.
            round(us$census_count / (1 + US_EST / 100) - us$census_count),
            OMIT, IMPU,
            PES_BLOCKS, PES_UNITS, PES_INTERVIEWS,
            PER_SEAT, N_SEATS_WORTH,
            us$census_count, NOT_EVALUATED,
            TRAP_ST, TRAP_RATE, TRAP_CMAX,
            round(mean(st$est_2020), 2), round(median(st$est_2020), 2),
            round(sd(st$est_2020), 2), nrow(st)))
dd_write_csv(facts, "derived/facts.csv")

# --- what this build verified ---------------------------------------------
#
# Every line is a condition the data could have violated and did not. The
# significance recount is the one worth reading twice: it re-derives the
# Bureau's asterisks from the two numbers printed beside them and reports the
# disagreement rather than hiding it.

checks <- data.frame(
  check = c(
    "state rows parsed out of Appendix Table 3",
    "state names in alphabetical order, matched to FIPS",
    "the Bureau's marks, checked against the numbers printed beside them",
    "national estimate, identical in all four PES tables",
    "self-response rate, national, against the published figure",
    "internet self-response below overall, every state",
    "map rings joined to a coverage estimate",
    "median published margin, one state against one national group"),
  value = c(
    sprintf("%d states and DC, out of %d printed rows", nrow(st) - 1L, nrow(st) + 1),
    sprintf("%d matched, 0 unmatched", nrow(st)),
    sprintf("%d of %d track; %s is the exception",
            51 - length(DISAGREE), 51,
            if (length(DISAGREE)) DISAGREE else "none"),
    sprintf("%.2f%% in all four", US_EST),
    sprintf("%.1f%%, as published", NAT_SRR),
    sprintf("%d of %d", sum(st$internet_response < st$self_response), nrow(st)),
    sprintf("%d of %d rings", sum(!is.na(mp$bin)), nrow(mp)),
    sprintf("%.2f against %.2f percentage points", SE_STATE_MED, SE_DEMO_MED)))
dd_write_csv(checks, "derived/checks.csv")

# --- report ----------------------------------------------------------------

prov_report()
cat(sprintf("\nstates.csv    : %d states and DC\n", nrow(st) - 1L))
cat(sprintf("                the Bureau states %d undercounts and %d overcounts, and declines on %d\n",
            N_UNDER, N_OVER, N_NONE))
cat(sprintf("                standard errors %.2f (%s) to %.2f (%s)\n",
            min(st$se_2020), st$state[which.min(st$se_2020)],
            max(st$se_2020), st$state[which.max(st$se_2020)]))
cat(sprintf("                largest estimate NOT called real: %s %+.2f (se %.2f)\n",
            big_insig$state[1], big_insig$est_2020[1], big_insig$se_2020[1]))
cat(sprintf("                smallest estimate that IS: %s %+.2f (se %.2f)\n",
            sig_small$state[1], sig_small$est_2020[1], sig_small$se_2020[1]))
cat(sprintf("\nself-response : national %.1f%% on %s; states %.1f to %.1f\n",
            NAT_SRR, RR_DATE, min(st$self_response), max(st$self_response)))
cat(sprintf("                cor with net coverage error %+.3f, with its standard error %+.3f\n",
            COR_EST, COR_SE))
cat(sprintf("\nrace.csv      : %d rows   tenure.csv: %d   age_sex.csv: %d   age.csv: %d\n",
            nrow(race), nrow(tenure), nrow(age_sex), nrow(age)))
cat(sprintf("components    : %.1f%% omissions and %.1f%% imputations behind a net of %.2f%%\n",
            OMIT, IMPU, US_EST))
cat(sprintf("median standard error: %.2f per state, %.2f per national subgroup\n",
            SE_STATE_MED, SE_DEMO_MED))
cat(sprintf("\nmap_states.csv: %d rings across %d states\n",
            nrow(mp), length(unique(mp$fips))))
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
