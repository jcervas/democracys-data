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
# Run this script from inside the data/ folder. It makes three API requests
# totalling about 240 KB and writes about 400 KB.
#
# WHICH COPY OF THE FILE THIS READS, AND WHY
#
# The 2020 Census Redistricting Data (P.L. 94-171) Summary File is published
# twice over. There is the legacy format -- one zip per state, pipe-delimited,
# no column headers, three segments joining on a record number -- and there is
# the same file behind the Census data API as the dataset `2020/dec/pl`. Same
# tables, same cells, same privacy noise; two deliveries of one file.
#
# This build reads the API. The whole country arrives in one request of a
# quarter of a megabyte. The legacy route is 51 state archives, about 1.3 GB
# compressed, to reach the identical numbers -- and the Bureau publishes no
# national legacy archive, so there is no shortcut through it.
#
# The legacy format is still what the chapter DESCRIBES, and the verbatim
# slices in raw/ are still read by the brief. What changed is only how this
# script gets the counts. The legacy-vs-API check below is what makes that
# safe: derived/legacy_six_states.csv is the previous build's output, parsed
# out of the legacy zips, and every cell of it is compared against what the
# API returns for the same counties on every run.
#
# A KEY IS REQUIRED. The Census data API has required one since 2025; an
# unkeyed request 302s to missing_key.html. Free from
# https://api.census.gov/data/key_signup.html; put it in ~/.Renviron as
#   CENSUS_API_KEY='...'
# and it is picked up below, exactly as zip-codes/data/build-data.R and
# policing/data/build-acs.R do. It is never written into this file, and the
# committed derived/ output means the LAB needs no key and no network -- only
# this rebuild does.
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
CACHE <- tempfile("pl"); dir.create(CACHE)

options(scipen = 999, stringsAsFactors = FALSE)

KEY <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(KEY)) stop("set CENSUS_API_KEY in ~/.Renviron -- see the header")

# --- The cells --------------------------------------------------------------
#
# Twenty-four cells, named as the published technical specification names them.
# The comment on each line is the label the API's own variables.json gives it,
# so the mapping can be checked without leaving this file. These are the same
# cells, in the same order, that the legacy build read by field position out of
# segments 1 and 3.
vars <- c(
  total           = "P1_001N",  # Total
  one_race        = "P1_002N",  # Population of one race
  white           = "P1_003N",  # ... White alone
  black           = "P1_004N",  # ... Black or African American alone
  aian            = "P1_005N",  # ... American Indian and Alaska Native alone
  asian           = "P1_006N",  # ... Asian alone
  nhpi            = "P1_007N",  # ... Native Hawaiian and Other Pacific Islander alone
  other_race      = "P1_008N",  # ... Some Other Race alone
  two_or_more     = "P1_009N",  # Population of two or more races
  hispanic        = "P2_002N",  # Hispanic or Latino
  not_hispanic    = "P2_003N",  # Not Hispanic or Latino
  nh_white        = "P2_005N",  # ... of one race: White alone
  nh_black        = "P2_006N",  # ... Black or African American alone
  nh_aian         = "P2_007N",  # ... American Indian and Alaska Native alone
  nh_asian        = "P2_008N",  # ... Asian alone
  nh_nhpi         = "P2_009N",  # ... Native Hawaiian and Other Pacific Islander alone
  nh_other        = "P2_010N",  # ... Some Other Race alone
  nh_two          = "P2_011N",  # ... of two or more races
  gq_total        = "P5_001N",  # Group quarters total
  gq_correctional = "P5_003N",  # ... correctional facilities for adults
  gq_juvenile     = "P5_004N",  # ... juvenile facilities
  gq_nursing      = "P5_005N",  # ... nursing/skilled-nursing facilities
  gq_college      = "P5_008N",  # ... college/university student housing
  gq_military     = "P5_009N")  # ... military quarters

BASE <- "https://api.census.gov/data/2020/dec/pl"

# The API answers with a JSON array of arrays, first row the header:
#   [["NAME","P1_001N",...,"state","county"],["Autauga County, Alabama",...]]
# Rows are cut on the brackets and cells pulled out of each by their quotes --
# no jsonlite, the way the other API-reading chapters in this corpus do it. The
# quotes are what makes it comma-proof, and NAME carries a comma at county
# level. The header row names its own columns, so nothing here has to know how
# many geography columns a given level returns.
get_pl <- function(geo, file, label) {
  url  <- paste0(BASE, "?get=NAME,", paste(vars, collapse = ","), "&for=", geo)
  path <- file.path(CACHE, file)
  if (!file.exists(path)) prov_fetch(paste0(url, "&key=", KEY), path, label = label)
  txt <- readChar(path, file.size(path))
  if (grepl("missing_key", txt, fixed = TRUE))
    stop("the API refused the key in CENSUS_API_KEY -- see the header")
  rows <- regmatches(txt, gregexpr("\\[[^][]*\\]", txt))[[1]]
  cell <- function(r) gsub('"', "", regmatches(r, gregexpr('"[^"]*"', r))[[1]])
  hdr  <- cell(rows[1])
  body <- lapply(rows[-1], cell)
  stopifnot(length(unique(lengths(body))) == 1L, lengths(body)[1] == length(hdr))
  out <- as.data.frame(do.call(rbind, body), stringsAsFactors = FALSE)
  names(out) <- hdr
  # every requested cell came back, under the name it was requested by
  stopifnot(all(c("NAME", vars) %in% names(out)))
  for (v in vars) out[[v]] <- as.numeric(out[[v]])
  out
}

cat("reading 2020/dec/pl from the Census API --\n")
us  <- get_pl("us:*",     "pl_us.json",     "2020 P.L. 94-171, national")
stt <- get_pl("state:*",  "pl_state.json",  "2020 P.L. 94-171, by state")
cty <- get_pl("county:*", "pl_county.json", "2020 P.L. 94-171, by county")
cat("  national:", nrow(us), "row;  states:", nrow(stt),
    ";  counties:", nrow(cty), "\n")

# Puerto Rico ships in the same dataset and is left out: its municipios are not
# counties of a state, the Gazetteer county list this joins against does not
# carry them, and the race and origin questions were asked there on a different
# form. The national row is the 50 states and DC and already excludes it.
cty <- cty[cty$state != "72", ]
stt <- stt[stt$state != "72", ]

all <- data.frame(fips  = paste0(cty$state, cty$county),
                  state = sub("^.*, ", "", cty$NAME),
                  api_name = sub(",.*$", "", cty$NAME),
                  stringsAsFactors = FALSE)
for (nm in names(vars)) all[[nm]] <- cty[[vars[[nm]]]]

# --- County names -----------------------------------------------------------
#
# The Gazetteer is the course's name list, so it is tried first -- the same
# authoritative list the `data-sources` chapter builds, so that one name list is
# used across every lab. It does not cover this file completely, and the gap is
# not an error in either source: the 2020 P.L. 94-171 tabulation uses the county
# geography in force on Census Day, and Connecticut has since replaced its eight
# counties with nine planning regions, which is what the current Gazetteer
# carries. Two vintages of a boundary, both correct on their own date. What is
# being tabulated is the 2020 count, so the 2020 name -- the one the API returns
# with the row -- is the right label for it.
gaz <- read.csv(file.path("..", "..", "..", "06-putting-data-together", "data-sources", "data", "derived",
                          "census_counties.csv"),
                stringsAsFactors = FALSE, colClasses = c(fips = "character"))
all <- merge(all, gaz[, c("fips", "name")], by = "fips", all.x = TRUE)
all$county <- ifelse(is.na(all$name), all$api_name, all$name)

fellback <- all[is.na(all$name), ]
cat("\ncounty names from the Gazetteer:", sum(!is.na(all$name)), "of", nrow(all), "\n")
if (nrow(fellback)) {
  cat("names taken from the API's own NAME field (not in the Gazetteer):\n")
  for (s in sort(unique(fellback$state)))
    cat("  ", s, ":", nrow(fellback[fellback$state == s, ]), "rows --",
        paste(sort(fellback$county[fellback$state == s]), collapse = ", "), "\n")
}
# Connecticut is the whole of the expected gap. Anything else appearing here is
# a source that moved, not a boundary vintage, and should stop the build.
stopifnot(all(fellback$state == "Connecticut"))

all$name <- NULL; all$api_name <- NULL
all <- all[, c("fips", "county", "state", names(vars))]
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

# The counties must roll up to the state rows and the state rows to the national
# row -- three separate answers from the API, which have no reason to agree
# unless the geography was assembled correctly. Exact, because the Bureau's
# privacy noise is applied so as to leave state and national totals untouched.
roll <- aggregate(total ~ fips_st, data = transform(all, fips_st = substr(fips, 1, 2)),
                  FUN = sum)
roll <- merge(roll, data.frame(fips_st = stt$state, api = stt$P1_001N), by = "fips_st")
cat("counties roll up to every state total:", nrow(roll) == 51 &&
      all(roll$total == roll$api), "\n")
stopifnot(nrow(roll) == 51, all(roll$total == roll$api))

cat("national total:", sum(all$total), "(API's own national row:", us$P1_001N, ")\n")
stopifnot(sum(all$total) == us$P1_001N, sum(all$total) == 331449281)

# Allegheny County is the anchor example in the lab.
al <- all[all$fips == "42003", ]
cat("Allegheny County total population:", al$total,
    "(published 2020 count: 1250578)\n")
stopifnot(al$total == 1250578)

# --- The API against the legacy files ---------------------------------------
#
# The reason this build can stop downloading 1.3 GB of state archives, and the
# check the brief's claim rests on: the two published deliveries of this file
# are compared rather than assumed to agree.
#
# derived/legacy_six_states.csv is the output of the previous build of this lab,
# which parsed the legacy zips by field position for Pennsylvania, Texas, New
# York, Hawaii, Mississippi and New Mexico. It is committed and never rewritten
# by this script, which is the point: comparing against the file this build
# writes would compare its output with itself the moment the first national
# file existed, and pass while testing nothing. It sits in derived/ rather than
# raw/ because raw/ is not under version control and a check nobody else can
# run is not a check.
fixture <- "derived/legacy_six_states.csv"
if (!file.exists(fixture)) stop("missing ", fixture, " -- the legacy fixture ",
                                "is what makes the API safe to read")
old <- read.csv(fixture, stringsAsFactors = FALSE, colClasses = c(fips = "character"))
cells <- intersect(names(vars), names(old))
m <- merge(old[, c("fips", cells)], all[, c("fips", cells)], by = "fips",
           suffixes = c(".legacy", ".api"))
bad <- sum(vapply(cells, function(v)
  sum(m[[paste0(v, ".legacy")]] != m[[paste0(v, ".api")]]), numeric(1)))
cat("\nlegacy-vs-API:", nrow(m), "counties x", length(cells), "cells --",
    bad, "mismatches\n")
stopifnot(nrow(m) == nrow(old), bad == 0)

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
