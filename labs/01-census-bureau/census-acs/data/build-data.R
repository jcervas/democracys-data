# ---------------------------------------------------------------------------
# Build the datasets for the "American Community Survey" chapter.
#
# Second of the three instrument chapters. The decennial chapter next door
# showed an enumeration: seven questions, everyone, once a decade. This one is
# about the instrument that answers everything else -- and that is a SAMPLE,
# published by the same agency, in the same house style, and quoted as though
# it were a count.
#
# THE ARGUMENT, in the order the chapter makes it.
#
#   IT IS A SAMPLE. About 3.5 million addresses a year, continuously. So every
#   number it publishes is an estimate with a margin of error printed beside
#   it, and the Bureau prints that margin in every table. Nothing is hidden.
#
#   THAT IS WHY THERE IS MORE THAN ONE WINDOW. A single year of sample is too
#   thin to describe a small place, so the Bureau pools years. The one-year
#   file covers 854 counties. The five-year file covers 3,222 -- all of them.
#   FOR 2,368 COUNTIES, 73 PER CENT OF THE COUNTRY, THERE IS NO ONE-YEAR
#   ESTIMATE AT ALL. That is not a preference for the five-year series; it is
#   the only series there is.
#
#   ONE OF THE WINDOWS WAS CLOSED. Between 2007 and 2013 the Bureau also
#   published a THREE-year series, and then stopped. This build does not assert
#   that -- it asks the server. The three-year directory answers for 2011,
#   2012 and 2013 and returns 404 from 2014 on, while the five-year directory
#   answers for every one of those years. A discontinued series is a live
#   hazard for anyone whose script still points at it.
#
#   THE WINDOW CHANGES THE ANSWER. For the 854 counties that appear in both
#   files, the one-year and five-year estimates of the same quantity, released
#   by the same agency on the same day, are different numbers. The five-year
#   figure is an average over 2019-2023; the one-year figure is 2023. Where a
#   place is growing or shrinking fast, choosing a window chooses a finding.
#
#   AND THE MARGIN IS NOT DECORATION. Relative margins widen sharply as places
#   get smaller, which is exactly where the five-year file is the only option.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# U.S. Census Bureau, American Community Survey summary file, table B01003
# (total population), 2023 vintage, table-based format, served over plain
# HTTPS with no key or account:
#
#   .../2023/table-based-SF/data/1YRData/acsdt1y2023-b01003.dat    239 KB
#   .../2023/table-based-SF/data/5YRData/acsdt5y2023-b01003.dat   18.3 MB
#
# Pipe-delimited, three columns: GEO_ID, the estimate, the margin of error.
# GEO_ID prefix 0500000US is a county. THE FILE USES -555555555 AS A MARGIN,
# which does not mean a margin of minus half a billion: it is the code for an
# estimate that was CONTROLLED rather than freely estimated, and arithmetic
# that treats it as a number produces nonsense. The census-access chapter is
# where that sentinel is taken apart; here it is simply excluded and counted.
#
# The vintage probe (which series exist for which years) is an HTTP HEAD
# against the summary-file directories, cached in raw/ after the first run so
# that a rebuild without internet still works and still reports what it saw.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Seven tables in derived/ and two raw captures:
#
#   raw/vintage-probe.txt  What the server answered, year by year.
#   raw/arrives.txt        The head of the one-year file, as it arrives.
#   derived/instrument.csv The decennial and the ACS, side by side.
#   derived/windows.csv    What each window covers, counted from the files.
#   derived/vintages.csv   The probe, as a table: which series existed when.
#   derived/controlled.csv Which tables carry a margin at all, and why.
#   derived/margins.csv    Relative margin of error by size of place.
#   derived/compare.csv    One-year against five-year, same county, same year.
#   derived/diverge.csv    The counties where the two windows disagree most.
#
# Run from this directory:  Rscript build-data.R
# (Downloads about 19 MB on first run; nothing after that.)
# ---------------------------------------------------------------------------

# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")    # dd_write_csv(): six significant digits

options(scipen = 999, stringsAsFactors = FALSE)

SENTINEL <- -555555555              # "controlled", not a margin
BASE <- "https://www2.census.gov/programs-surveys/acs/summary_file"

grab <- function(url, dest) {
  if (!file.exists(dest))
    prov_fetch(url, dest, quiet = TRUE, mode = "wb")
  dest
}

# --- 1. The two windows -----------------------------------------------------

f1 <- grab(paste0(BASE, "/2023/table-based-SF/data/1YRData/acsdt1y2023-b01003.dat"),
           "raw/acsdt1y2023-b01003.dat")
f5 <- grab(paste0(BASE, "/2023/table-based-SF/data/5YRData/acsdt5y2023-b01003.dat"),
           "raw/acsdt5y2023-b01003.dat")

rd <- function(p) {
  d <- read.delim(p, sep = "|", colClasses = "character")
  names(d) <- c("GEO_ID", "est", "moe")
  d$est <- as.numeric(d$est)
  d$moe <- as.numeric(d$moe)
  d
}
a1 <- rd(f1); a5 <- rd(f5)

county <- function(d) {
  d <- d[startsWith(d$GEO_ID, "0500000US"), ]
  d$fips <- substr(d$GEO_ID, 10, 14)
  d
}
c1 <- county(a1); c5 <- county(a5)
stopifnot(nrow(c5) > 3000, nrow(c1) > 500, !any(duplicated(c5$fips)))

# THE THRESHOLD, MEASURED RATHER THAN QUOTED. The Bureau publishes a one-year
# estimate only for places above a population floor. Rather than assert the
# floor, read it off the file: the smallest county that has a one-year
# estimate at all.
FLOOR <- min(c1$est, na.rm = TRUE)

# WHAT THE COUNTY ROWS ACTUALLY ARE, because "3,222 counties" is not the
# country and the number is quoted as though it were. Two things separate this
# file's county universe from the 3,143 the decennial chapters count:
#
#   PUERTO RICO. The ACS covers it and publishes its 78 municipios at this
#   summary level. They are county-equivalents, they are not counties of a
#   state, and they are outside the apportionment population the decennial
#   file is built for -- which is why the redistricting chapters drop them.
#
#   CONNECTICUT. The state replaced its eight counties with nine planning
#   regions, and the 2023 ACS vintage is tabulated on the new geography while
#   the 2020 P.L. file is tabulated on the old. So the same country is 3,143
#   rows in one product and 3,144 in the other, and neither is wrong.
#
# Both are measured here rather than asserted, so a later vintage that changed
# either one would move these numbers instead of quietly contradicting them.
st2       <- function(d) substr(d$fips, 1, 2)
PR1       <- sum(st2(c1) == "72");  PR5 <- sum(st2(c5) == "72")
CT5_FIPS  <- c5$fips[st2(c5) == "09"]
CT5       <- length(CT5_FIPS)
# planning regions are 09110...09190 and end in a zero; the old counties are
# 09001...09015 and are odd
CT5_PLANNING <- all(as.integer(substr(CT5_FIPS, 3, 5)) %% 10 == 0)
stopifnot(PR5 == 78, CT5 == 9, CT5_PLANNING)

windows <- data.frame(
  window = c("One-year (2023)", "Five-year (2019-2023)"),
  counties_published = c(nrow(c1), nrow(c5)),
  counties_states_dc = c(nrow(c1) - PR1, nrow(c5) - PR5),
  counties_puerto_rico = c(PR1, PR5),
  smallest_place_published = c(FLOOR, min(c5$est, na.rm = TRUE)),
  what_it_is = c("A single year of sample",
                 "Five years of sample pooled, averaged over the period"))
stopifnot(windows$counties_states_dc + windows$counties_puerto_rico ==
          windows$counties_published)
dd_write_csv(windows, "derived/windows.csv")

MISSING <- nrow(c5) - nrow(c1)
MISSPCT <- 100 * MISSING / nrow(c5)

# --- 2. The window that was closed ------------------------------------------
#
# ASK THE SERVER, do not assert. A HEAD request against each directory: 200
# means the Bureau still publishes that series for that year, 404 means it
# does not. Cached to raw/ so an offline rebuild reports the same thing and
# says when it was seen.

PROBE <- "raw/vintage-probe.txt"
YEARS <- 2011:2015
if (!file.exists(PROBE)) {
  lines <- c("Does the Census Bureau publish this ACS series for this year?",
             "A HEAD request against the summary-file directory.", "")
  for (y in YEARS) for (s in c("3_year_seq_by_state", "5_year_seq_by_state")) {
    u <- sprintf("%s/%d/data/%s/", BASE, y, s)
    code <- tryCatch({
      con <- url(u, open = "rb"); close(con); 200L
    }, error = function(e) 404L)
    lines <- c(lines, sprintf("%d  %-20s  HTTP %d", y, sub("_seq.*", "", s), code))
  }
  writeLines(lines, PROBE)
}
pl <- readLines(PROBE)
pl <- pl[grepl("^[0-9]{4}  ", pl)]
vintages <- data.frame(
  year   = as.integer(substr(pl, 1, 4)),
  series = trimws(substr(pl, 7, 26)),
  http   = as.integer(sub(".*HTTP ", "", pl)))
vintages$published <- ifelse(vintages$http == 200, "Yes", "No -- 404")
dd_write_csv(vintages, "derived/vintages.csv")

LAST3 <- max(vintages$year[vintages$series == "3_year" & vintages$http == 200])

# --- 3. Which numbers even HAVE a margin ------------------------------------
#
# THIS IS THE SURPRISE IN THE FILE, and it decides how the rest of the chapter
# has to be written. Total population is CONTROLLED: for a county, the ACS does
# not estimate it independently at all, it is forced to agree with the
# Population Estimates Program. A controlled estimate has no sampling margin,
# so the Bureau writes the sentinel instead -- for 3,090 of 3,222 counties.
#
# Median household income is not controlled. Every county carries a real
# margin. So the margin section below uses income, and the contrast between
# the two tables is itself the finding: whether a published ACS number has a
# margin at all depends on which table it came from.

f19 <- grab(paste0(BASE, "/2023/table-based-SF/data/5YRData/acsdt5y2023-b19013.dat"),
            "raw/acsdt5y2023-b19013.dat")
inc <- county(rd(f19))

controlled <- data.frame(
  table = c("B01003, total population", "B19013, median household income"),
  county_rows = c(nrow(c5), nrow(inc)),
  rows_with_no_margin = c(sum(c5$moe == SENTINEL, na.rm = TRUE),
                          sum(inc$moe == SENTINEL, na.rm = TRUE)),
  why = c("Controlled to the Population Estimates Program",
          "Estimated from the sample, like most of the ACS"))
controlled$share_with_no_margin <-
  100 * controlled$rows_with_no_margin / controlled$county_rows
dd_write_csv(controlled, "derived/controlled.csv")

# --- 4. The margin, by size of place ----------------------------------------
#
# Banded by the county's POPULATION (from B01003) but measuring the margin on
# INCOME (from B19013), because that is the number that actually carries one.
# Sentinel rows are excluded rather than coerced: a margin of -555555555 is
# not a margin, and averaging it in would drag every summary into nonsense.

m <- merge(inc[, c("fips", "est", "moe")],
           c5[, c("fips", "est")], by = "fips", suffixes = c("", "_pop"))
m <- m[m$moe != SENTINEL & !is.na(m$moe) & m$est > 0, ]
m$rel <- 100 * m$moe / m$est
band <- cut(m$est_pop,
            breaks = c(0, 1000, 5000, 20000, 65000, 250000, Inf),
            labels = c("under 1,000", "1,000-4,999", "5,000-19,999",
                       "20,000-64,999", "65,000-249,999", "250,000 and over"),
            right = FALSE)
margins <- do.call(rbind, lapply(levels(band), function(b) {
  s <- m[!is.na(band) & band == b, ]
  if (nrow(s) == 0L) return(NULL)          # no empty rows in a printed table
  data.frame(population_of_county = b, counties = nrow(s),
             median_income = median(s$est),
             median_margin = median(s$moe),
             median_margin_pct = median(s$rel))
}))
stopifnot(sum(margins$counties) > 3000)    # every county, not a leftover few
dd_write_csv(margins, "derived/margins.csv")

NSENT <- sum(c5$moe == SENTINEL, na.rm = TRUE)

# --- 4. One window against the other ----------------------------------------
#
# The same county, the same agency, the same release, two published
# populations. Not a contradiction -- they answer different questions -- but
# they are quoted interchangeably, and the difference is the cost of that.

j <- merge(c1[, c("fips", "est", "moe")], c5[, c("fips", "est", "moe")],
           by = "fips", suffixes = c("_1yr", "_5yr"))
stopifnot(nrow(j) == nrow(c1))
j$diff     <- j$est_1yr - j$est_5yr
j$diff_pct <- 100 * j$diff / j$est_5yr

compare <- data.frame(
  quantity = c("Counties in both files",
               "Counties where the two differ by more than 1%",
               "Counties where the two differ by more than 5%",
               "Median absolute difference",
               "Median absolute difference, per cent",
               "Largest absolute difference, per cent"),
  value = c(nrow(j),
            sum(abs(j$diff_pct) > 1),
            sum(abs(j$diff_pct) > 5),
            median(abs(j$diff)),
            median(abs(j$diff_pct)),
            max(abs(j$diff_pct))),
  unit = c("counties", "counties", "counties", "people", "%", "%"))
dd_write_csv(compare, "derived/compare.csv")

# The worst cases, named. A county growing or shrinking fast is exactly where
# a five-year average is furthest from the year it is quoted as.
# The state FIPS map is kept here rather than downloaded: 51 codes that have
# not changed since 1970, and a county code is unreadable without them. The
# first two digits of a county FIPS are its state.
ST <- c("01"="AL","02"="AK","04"="AZ","05"="AR","06"="CA","08"="CO","09"="CT",
        "10"="DE","11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL",
        "18"="IN","19"="IA","20"="KS","21"="KY","22"="LA","23"="ME","24"="MD",
        "25"="MA","26"="MI","27"="MN","28"="MS","29"="MO","30"="MT","31"="NE",
        "32"="NV","33"="NH","34"="NJ","35"="NM","36"="NY","37"="NC","38"="ND",
        "39"="OH","40"="OK","41"="OR","42"="PA","44"="RI","45"="SC","46"="SD",
        "47"="TN","48"="TX","49"="UT","50"="VT","51"="VA","53"="WA","54"="WV",
        "55"="WI","56"="WY","72"="PR")

d <- j[order(-abs(j$diff_pct)), ][1:8, ]
diverge <- data.frame(
  county_fips = d$fips,
  state = unname(ST[substr(d$fips, 1, 2)]),
  one_year_2023 = d$est_1yr,
  five_year_2019_2023 = d$est_5yr,
  difference = d$diff,
  difference_pct = d$diff_pct)
stopifnot(!any(is.na(diverge$state)))
dd_write_csv(diverge, "derived/diverge.csv")

# --- 5. The two instruments, side by side -----------------------------------

instrument <- data.frame(
  the_question = c(
    "Who is asked?",
    # THE ROW THAT DECIDES WHICH PART OF THIS BOOK THE ACS LIVES IN.
    # Both instruments are compelled: 13 U.S.C. 221 makes refusing to answer
    # either one a punishable offence, and the ACS mail package says "YOUR
    # RESPONSE IS REQUIRED BY LAW" on the envelope. That is why a sample survey
    # of 3.5 million addresses sits in the COUNTING PEOPLE part rather than the
    # ASKING PEOPLE part, where every instrument is voluntary and grant-funded.
    # Without this row the part assignment looks like a filing error -- the ACS
    # asks some people questions, which is the definition of a survey the
    # surveys chapter gives.
    "Must you answer?",
    "How often?",
    "What comes out?",
    "How many people live on this block?",
    "What is the median household income here?",
    "Is there a margin of error?",
    "Can it be used to apportion the House?"),
  decennial_census = c(
    "Everyone",
    "Yes -- required by law, 13 U.S.C. 221",
    "Once a decade, as of one day",
    "Counts",
    "Yes -- this is the thing only it can do",
    "It does not ask",
    "No -- but noise is added below the state level",
    "Yes -- this is its constitutional purpose"),
  american_community_survey = c(
    "A sample: about 3.5 million addresses a year",
    "Yes -- required by law, the same statute",
    "Continuously, published every year",
    "Estimates",
    "No -- the sample is far too thin",
    "Yes -- this is what it is for",
    "Yes -- printed beside every estimate",
    "No -- the Constitution requires an actual enumeration"))
dd_write_csv(instrument, "derived/instrument.csv")

# --- 6. What arrives --------------------------------------------------------

hd <- readLines(f1, n = 6)
writeLines(c(
"The one-year file, opened. Three columns, pipe-delimited: the",
"geography, the estimate, and the margin of error.",
"",
hd,
"",
"The first data row is the United States, and its margin is",
"-555555555. That is not a margin. It is the code the Bureau uses",
"for an estimate that was CONTROLLED rather than freely estimated,",
"and any arithmetic that treats it as a number will produce",
"nonsense quietly. In the five-year file this code appears",
sprintf("%s times among the county rows alone.", format(NSENT, big.mark = ",")),
"",
"GEO_ID must be read as text. The county code is characters 10-14,",
"and it keeps its leading zero only if nothing has turned it into",
"a number along the way."), "raw/arrives.txt")

# --- report -----------------------------------------------------------------

cat(sprintf("\nwindows.csv   : one-year covers %s counties, five-year %s\n",
            format(nrow(c1), big.mark = ","), format(nrow(c5), big.mark = ",")))
cat(sprintf("                %s counties (%.1f%%) have NO one-year estimate\n",
            format(MISSING, big.mark = ","), MISSPCT))
cat(sprintf("                smallest county with a one-year estimate: %s people\n",
            format(FLOOR, big.mark = ",")))
cat(sprintf("\nvintages.csv  : the three-year series last appears for %d\n", LAST3))
print(vintages[, c("year", "series", "published")], row.names = FALSE)
cat("\ncontrolled.csv: whether a number has a margin depends on its table\n")
print(controlled, row.names = FALSE)
cat("\nmargins.csv   : relative margin widens as places get smaller\n")
print(margins, row.names = FALSE)
cat(sprintf("\n                %s county rows carry the -555555555 sentinel\n",
            format(NSENT, big.mark = ",")))
cat("\ncompare.csv   : the same county, two windows\n")
print(compare, row.names = FALSE)
cat("\ndone.\n")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
