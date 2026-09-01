# ---------------------------------------------------------------------------
# Build the census dataset.
#
# One file ends up in this folder:
#
#   derived/pl94171_counties.csv   county-level 2020 Census redistricting data
#                          for all 50 states and the District of Columbia,
#                          from Tables P1 (race), P2 (Hispanic origin by race)
#                          and P5 (group quarters)
#
# Run this script from inside the data/ folder. It downloads roughly 1.3 GB of
# state files and writes about 400 KB. The committed output means the lab needs
# no network and no Census API key -- which matters more than it used to: the
# api.census.gov `dec/pl` endpoint that would otherwise serve these same counts
# in one small request now answers an unkeyed call with "Missing Key", and the
# Bureau publishes no national P.L. 94-171 archive. Fifty-one state downloads
# is the only keyless route to the whole country.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
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


dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# U.S. Census Bureau, 2020 Census Redistricting Data (P.L. 94-171) Summary File,
# legacy format:
#   https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/
#
# This is the file the Census Bureau is required by law (P.L. 94-171, 1975) to
# deliver to every state within one year of Census Day, so that legislatures can
# draw districts. It is the legal basis of every redistricting cycle.
#
# Each state ships four pipe-delimited files with no headers:
#   xxgeo2020.pl        geographic header, one row per geographic unit
#   xx000012020.pl      segment 1: Table P1 (71 cols) and P2 (73 cols)
#   xx000022020.pl      segment 2: P3, P4 (the same, voting age only)
#   xx000032020.pl      segment 3: Table P5 (group quarters, 10 cols)
#
# Segments join to the geographic header on LOGRECNO.
#
# Fifty states and the District of Columbia. Puerto Rico ships in the same
# directory and is left out: its municipios are not counties of a state, the
# Gazetteer county list this joins against does not carry them, and the
# race-and-origin questions were asked there on a different form.

states <- c(
  AL = "Alabama",        AK = "Alaska",         AZ = "Arizona",
  AR = "Arkansas",       CA = "California",     CO = "Colorado",
  CT = "Connecticut",    DE = "Delaware",       DC = "District_of_Columbia",
  FL = "Florida",        GA = "Georgia",        HI = "Hawaii",
  ID = "Idaho",          IL = "Illinois",       IN = "Indiana",
  IA = "Iowa",           KS = "Kansas",         KY = "Kentucky",
  LA = "Louisiana",      ME = "Maine",          MD = "Maryland",
  MA = "Massachusetts",  MI = "Michigan",       MN = "Minnesota",
  MS = "Mississippi",    MO = "Missouri",       MT = "Montana",
  NE = "Nebraska",       NV = "Nevada",         NH = "New_Hampshire",
  NJ = "New_Jersey",     NM = "New_Mexico",     NY = "New_York",
  NC = "North_Carolina", ND = "North_Dakota",   OH = "Ohio",
  OK = "Oklahoma",       OR = "Oregon",         PA = "Pennsylvania",
  RI = "Rhode_Island",   SC = "South_Carolina", SD = "South_Dakota",
  TN = "Tennessee",      TX = "Texas",          UT = "Utah",
  VT = "Vermont",        VA = "Virginia",       WA = "Washington",
  WV = "West_Virginia",  WI = "Wisconsin",      WY = "Wyoming")

base <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/data/",
               "01-Redistricting_File--PL_94-171/")

# Column positions within each file (1-indexed, pipe-delimited).
#   geo:  3 SUMLEV, 8 LOGRECNO, 10 GEOCODE (the FIPS code), 88 NAME
#   seg1: 5 LOGRECNO, then P1 starts at 6, P2 starts at 77
#   seg3: 5 LOGRECNO, then P5 starts at 6
#
# NOTE: the geo header carries a NAME field at position 88, and for the county
# rows of these files it is populated -- checked against the 2021 vintage on
# 2026-08-11: all 67 Pennsylvania counties and all 5 Hawaii counties have a
# name, and all 72 match the Gazetteer exactly. County names are nonetheless
# joined from the Census Gazetteer, the same authoritative list used in the
# `data-sources` chapter, so that one name list is used across every lab in this
# course. Where the two lists disagree the file's own NAME wins; see the
# Connecticut note below. See data/raw/source-shape.csv for the measurements.
#
# Read only the columns that are used. California's segment 1 is 700,000-odd
# block rows wide by 149 fields; read whole, as character, one state at a time
# is enough to matter. colClasses = "NULL" drops a column at parse time and
# leaves the survivors under their original V-numbers, so the field map below
# stays readable as positions in the published specification rather than
# positions in whatever is left after the drop.
read_pl <- function(path, ncol, keep, ...) {
  cc <- rep("NULL", ncol)
  cc[keep] <- "character"
  read.delim(path, sep = "|", header = FALSE, quote = "", colClasses = cc, ...)
}

grab_state <- function(ab, nm) {
  cat("  ", nm, "... ")
  zurl <- paste0(base, nm, "/", tolower(ab), "2020.pl.zip")
  zf <- tempfile(fileext = ".zip")
  prov_fetch(zurl, zf, mode = "wb", quiet = TRUE)
  fs <- unzip(zf, exdir = tempdir())
  on.exit(unlink(c(zf, fs)), add = TRUE)   # 1.3 GB unpacks to ~13 GB otherwise

  gf <- grep("geo2020\\.pl$", fs, value = TRUE)
  s1 <- grep("000012020\\.pl$", fs, value = TRUE)
  s3 <- grep("000032020\\.pl$", fs, value = TRUE)

  geo <- read_pl(gf, 97, c(3, 8, 10, 88), fileEncoding = "latin1")
  geo <- geo[geo$V3 == "050", ]                        # counties only
  keyg <- data.frame(logrec = geo$V8, fips = geo$V10, state = nm,
                     file_name = trimws(geo$V88))

  d1 <- read_pl(s1, 149, c(5, 6:14, 78, 79, 81:87))
  d1 <- d1[d1$V5 %in% keyg$logrec, ]
  d3 <- read_pl(s3, 15, c(5, 6, 8, 9, 10, 13, 14))
  d3 <- d3[d3$V5 %in% keyg$logrec, ]

  n1 <- function(i) as.numeric(d1[[paste0("V", 5 + i)]])    # P1 field i
  n2 <- function(i) as.numeric(d1[[paste0("V", 76 + i)]])   # P2 field i
  n5 <- function(i) as.numeric(d3[[paste0("V", 5 + i)]])    # P5 field i

  p1 <- data.frame(logrec = d1$V5,
    total        = n1(1),
    one_race     = n1(2),
    white        = n1(3),
    black        = n1(4),
    aian         = n1(5),
    asian        = n1(6),
    nhpi         = n1(7),
    other_race   = n1(8),
    two_or_more  = n1(9))

  p2 <- data.frame(logrec = d1$V5,
    hispanic     = n2(2),
    not_hispanic = n2(3),
    nh_white     = n2(5),
    nh_black     = n2(6),
    nh_aian      = n2(7),
    nh_asian     = n2(8),
    nh_nhpi      = n2(9),
    nh_other     = n2(10),
    nh_two       = n2(11))

  p5 <- data.frame(logrec = d3$V5,
    gq_total       = n5(1),
    gq_correctional = n5(3),
    gq_juvenile    = n5(4),
    gq_nursing     = n5(5),
    gq_college     = n5(8),
    gq_military    = n5(9))

  out <- merge(merge(merge(keyg, p1, by = "logrec"), p2, by = "logrec"),
               p5, by = "logrec")
  cat(nrow(out), "counties\n")
  out
}

cat("downloading 50 states and DC (about 1.3 GB) --\n")
all <- do.call(rbind, mapply(grab_state, names(states), states, SIMPLIFY = FALSE))
all$logrec <- NULL
all$state <- gsub("_", " ", all$state)

# --- County names -----------------------------------------------------------
#
# The Gazetteer is the course's name list, so it is tried first. It does not
# cover this file completely, and the gap is not an error in either source: the
# 2020 P.L. 94-171 files were tabulated on the county geography in force on
# Census Day, and Connecticut has since replaced its eight counties with nine
# planning regions, which is what the current Gazetteer carries. Two vintages of
# a boundary, both correct on their own date. The file being tabulated is the
# 2020 one, so the 2020 name -- the file's own NAME field -- is the right label
# for a 2020 count, and the Gazetteer supplies the other 3,135 rows.
gaz <- read.csv(file.path("..", "..", "..", "06-putting-data-together", "data-sources", "data", "derived",
                          "census_counties.csv"),
                stringsAsFactors = FALSE, colClasses = c(fips = "character"))
all <- merge(all, gaz[, c("fips", "name")], by = "fips", all.x = TRUE)
all$county <- ifelse(is.na(all$name), all$file_name, all$name)

fellback <- all[is.na(all$name), ]
cat("\ncounty names from the Gazetteer:", sum(!is.na(all$name)), "of", nrow(all),
    "\n")
if (nrow(fellback)) {
  cat("names taken from the file's own NAME field (not in the Gazetteer):\n")
  for (s in sort(unique(fellback$state)))
    cat("  ", s, ":", nrow(fellback[fellback$state == s, ]), "rows --",
        paste(sort(fellback$county[fellback$state == s]), collapse = ", "),
        "\n")
}
# Connecticut is the whole of the expected gap. Anything else appearing here is
# a source that moved, not a boundary vintage, and should stop the build.
stopifnot(all(fellback$state == "Connecticut"))

all$name <- NULL; all$file_name <- NULL
all <- all[, c("fips", "county", "state", setdiff(names(all),
                                                  c("fips","county","state")))]
all <- all[order(all$state, all$county), ]
stopifnot(!any(is.na(all$county) | all$county == ""))

# --- Checks the lab depends on ----------------------------------------------

cat("\ntotal counties:", nrow(all), "\n")
cat("states and DC:", length(unique(all$state)), "\n")
stopifnot(nrow(all) == 3143, length(unique(all$state)) == 51)

# P1 must decompose exactly.
d <- with(all, total - (one_race + two_or_more))
cat("P1 one-race + two-or-more equals total in all rows:", all(d == 0), "\n")
stopifnot(all(d == 0))

# P2 must decompose exactly, and must agree with P1's total.
d2 <- with(all, total - (hispanic + not_hispanic))
cat("P2 Hispanic + not-Hispanic equals total in all rows:", all(d2 == 0), "\n")
stopifnot(all(d2 == 0))

# The counties of the 50 states and DC must sum to the published 2020 resident
# population. This is the check the six-state build could not make: it is only
# available once the last state is in, and it is exact because the Bureau's
# privacy noise is applied so as to leave state and national totals untouched.
cat("national total:", sum(all$total), "(published 2020 count: 331449281)\n")
stopifnot(sum(all$total) == 331449281)

# Allegheny County is the anchor example in the lab.
al <- all[all$fips == "42003", ]
cat("Allegheny County total population:", al$total,
    "(published 2020 count: 1250578)\n")
stopifnot(al$total == 1250578)

write.csv(all, "derived/pl94171_counties.csv", row.names = FALSE)
cat("\nwrote pl94171_counties.csv:", nrow(all), "rows,", ncol(all), "columns\n")

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
