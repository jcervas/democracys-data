# ---------------------------------------------------------------------------
# build-data.R -- the zip-codes chapter.
#
# THE QUESTION.  A ZIP code is a routing key for mail.  It is used as a place:
# by surveys, by insurers, by health researchers, by people who write about
# "your ZIP code" as though it were a neighbourhood, and -- the case this book
# cares about -- by a proposal to build congressional districts out of them.
# This script assembles the evidence for what the object actually is.
#
# WHAT IT BUILDS  (all in derived/, all read by zip-codes-brief.Rmd)
#
#   lists.csv        the two national lists side by side: postal codes in
#                    circulation, ZIP Code Tabulation Areas, and the overlap
#   digits.csv       one row per ZCTA: code, first digit, three-digit prefix,
#                    interior point, land area, population, state
#   digit_rows.csv   one row per leading digit: how many codes, how many
#                    people, which states -- derived, never typed in
#   no_ground.csv    five postal codes with no tabulation area, named
#   no_ground_kind.csv  where the codes with no ground are, by state
#   nesting.csv      how ZCTA land divides among counties and states
#   churn.csv        what happened to the 2010 areas by 2020
#   cities.csv       Irvine and Pittsburgh: ZIP areas touching, ZIP areas inside
#   cities_big.csv   the same two counts for the 100 largest cities
#   pgh_rings.csv    polygon rings for the two Pittsburgh figures
#   pgh_ids.csv      lev, uid -> the ZCTA code, its land area, its share inside
#                    the city limits
#   pa_split.csv     Pennsylvania ZIP areas against the 119th Congress districts
#   zip_vs_zcta.csv  the two objects side by side, nine questions, with the
#                    evidence for each drawn from the tables above
#   facts.csv        every scalar the brief quotes, name/value/note
#   checks.csv       the checks, printed verbatim in the brief
#
# SOURCES.  GeoNames, Postal Codes United States (https://download.geonames.org/export/zip/US.zip)
#   for the list of codes in circulation, because the Postal Service sells its
#   own; Census Bureau gazetteer; 2020 Census DHC table P1 at summary level 860
#   (the API, which needs a free key); ACS 5-year table B01003; the 2020 ZCTA
#   relationship files; and cartographic boundaries for ZCTAs, places and
#   congressional districts. Eight in all, and the first of them is the point of
#   the chapter.
#
#   GeoNames postal codes, United States      https://download.geonames.org/export/zip/US.zip
#     THE POSTAL SERVICE DOES NOT PUBLISH A FREE LIST OF ITS OWN ZIP CODES.
#     The authoritative file is the City State Product, which USPS sells on
#     subscription.  What is used here instead is GeoNames' compilation, which
#     is CC-BY and is the list most open work ends up using.  It is a
#     compilation, not the register, and the brief says so.  Committed to raw/
#     because it is small and because it moves.
#
#   Census Gazetteer, 2024, ZCTAs             https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_zcta_national.zip
#     The 33,791 tabulation areas, with land area and an interior point.
#     Committed to raw/.
#
#   2020 Census DHC, table P1                 https://api.census.gov/data/2020/dec/dhc
#     The decennial count, tabulated at summary level 860 -- the ZIP Code
#     Tabulation Area.  This is the population used everywhere below.
#
#     A KEY IS REQUIRED.  The Census data API has required one since 2025 (the
#     metadata endpoints are still open).  Free from
#     https://api.census.gov/data/key_signup.html; put it in ~/.Renviron as
#       CENSUS_API_KEY='...'
#     and it is picked up below, exactly as policing/data/build-acs.R does.  It
#     is never written into this file.  Every other source here is keyless.
#
#   ACS 5-year 2023, table B01003             https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData/acsdt5y2023-b01003.dat
#     Total population for every published geography, ZCTAs and places among
#     them.  Read here as a SECOND reading of the same areas, against the
#     decennial count: two published populations for one ZIP area, three years
#     apart, and the size of the gap between them.  Also the only place-level
#     population used, for ranking the hundred largest cities.
#
#   ZCTA-to-county relationship, 2020         https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt
#   ZCTA-to-place relationship, 2020          https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_place20_natl.txt
#   ZCTA 2010-to-2020 relationship            https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta510_zcta520_natl.txt
#     The Bureau's own account of how the areas overlap, in square metres of
#     shared land.  Using these rather than intersecting polygons means the
#     crossing counts are the Bureau's arithmetic and not a by-product of a
#     tolerance chosen here.
#
#   Cartographic boundaries, ZCTAs 2020       https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_zcta520_500k.zip
#   Cartographic boundaries, PA places 2020   https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_42_place_500k.zip
#   Cartographic boundaries, CD 119th         https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_cd119_500k.zip
#     Geometry for the Pittsburgh figures and for the one operation in this
#     script that has no relationship file behind it: ZIP areas against
#     congressional districts.
#
# BUILD SCRIPT -- may use packages.  The brief is base R and reads only the
# CSVs written here.  Run from this directory:
#
#     Rscript build-data.R
#
# Downloads land in a temporary directory and are re-fetched every run.  Set
# DD_ZIP_CACHE to a path outside data/ to keep them between runs while working
# on the script.
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")     # dd_write_csv(): six significant digits
source("../../../_lib/provenance.R")    # notices when a source moves

dir.create("derived", showWarnings = FALSE)
dir.create("raw",     showWarnings = FALSE)

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
options(scipen = 999, stringsAsFactors = FALSE, warn = 1, timeout = 1800)

CACHE <- Sys.getenv("DD_ZIP_CACHE", unset = file.path(tempdir(), "zip-codes"))
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

FETCH_DATE <- "2026-08-13"

n   <- function(x) format(x, big.mark = ",")
say <- function(...) cat(sprintf(...), "\n", sep = "")

FACTS <- list()
fact  <- function(key, value, note) {
  FACTS[[key]] <<- list(value = dd_num(value), note = note)
  invisible(value)
}
CHECKS <- list()
check <- function(label, value) {
  CHECKS[[length(CHECKS) + 1L]] <<- list(check = label, value = value)
  invisible(value)
}

# --- fetching ---------------------------------------------------------------
grab <- function(url, file) {
  p <- file.path(CACHE, file)
  if (!file.exists(p)) {
    say("downloading %s", url)
    prov_fetch(url, p, mode = "wb", quiet = TRUE)
  }
  p
}
# The same, for the sources whose contents are watched: prov_fetch() always
# re-downloads, which is right on a clean run and wrong when DD_ZIP_CACHE has
# been set to work on this script.
grab_watched <- function(url, file) {
  p <- file.path(CACHE, file)
  if (file.exists(p)) return(p)
  prov_fetch(url, p)
}
unzip_to <- function(zipfile) {
  d <- file.path(CACHE, sub("\\.zip$", "", basename(zipfile)))
  if (!dir.exists(d)) utils::unzip(zipfile, exdir = d)
  d
}
shp <- function(url, zipname) {
  d <- unzip_to(grab(url, zipname))
  st_read(list.files(d, "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
}
# The relationship files are pipe-delimited with a UTF-8 byte-order mark on the
# first header name, and every identifier in them is a code that must not be
# read as a number: 00601 is a ZCTA in Puerto Rico, 601 is nothing.
rel <- function(name) {
  u <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/",
              "zcta520/", name)
  d <- read.delim(grab_watched(u, name), sep = "|",
                  colClasses = "character", check.names = FALSE)
  names(d)[1] <- sub("^﻿", "", names(d)[1])
  d$AREALAND_PART <- as.numeric(d$AREALAND_PART)
  d
}

# ===========================================================================
# 1.  THE TWO LISTS
#
#     One is a list of routing keys.  The other is a list of areas.  They are
#     not the same list and they are not the same length, and the difference
#     is the chapter.
# ===========================================================================

# --- the postal codes -------------------------------------------------------
GN_URL <- "https://download.geonames.org/export/zip/US.zip"
GN_RAW <- "raw/geonames-US.zip"
if (!file.exists(GN_RAW)) prov_fetch(GN_URL, GN_RAW, mode = "wb", quiet = TRUE)
gnd <- file.path(CACHE, "geonames"); dir.create(gnd, showWarnings = FALSE)
utils::unzip(GN_RAW, exdir = gnd)
gn <- read.delim(file.path(gnd, "US.txt"), sep = "\t", header = FALSE,
                 quote = "", colClasses = "character",
                 col.names = c("country", "code", "place", "state_name",
                               "state", "county_name", "county_fips",
                               "admin3", "admin3_code", "lat", "lon", "acc"))
gn$lat <- as.numeric(gn$lat); gn$lon <- as.numeric(gn$lon)
stopifnot(all(nchar(gn$code) == 5), all(gn$country == "US"))
# Two codes appear twice, in two states.  A postal code is not required to be
# in one state, which is the first small sign of what is coming.
gn <- gn[!duplicated(gn$code), ]
say("postal codes in the compilation: %s", n(nrow(gn)))

# --- the tabulation areas ---------------------------------------------------
GAZ_URL <- paste0("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/",
                  "2024_Gazetteer/2024_Gaz_zcta_national.zip")
GAZ_RAW <- "raw/2024_Gaz_zcta_national.zip"
if (!file.exists(GAZ_RAW)) prov_fetch(GAZ_URL, GAZ_RAW, mode = "wb", quiet = TRUE)
gz <- read.delim(file.path(unzip_to(GAZ_RAW), "2024_Gaz_zcta_national.txt"),
                 sep = "\t", colClasses = "character", strip.white = TRUE)
names(gz) <- trimws(names(gz))
gz$ALAND <- as.numeric(gz$ALAND)
gz$lat   <- as.numeric(gz$INTPTLAT)
gz$lon   <- as.numeric(gz$INTPTLONG)
stopifnot(all(nchar(gz$GEOID) == 5), !anyDuplicated(gz$GEOID), !anyNA(gz$lon))
say("ZIP Code Tabulation Areas: %s", n(nrow(gz)))

both      <- intersect(gn$code, gz$GEOID)
usps_only <- setdiff(gn$code, gz$GEOID)     # a code with no ground
zcta_only <- setdiff(gz$GEOID, gn$code)     # ground with no code in circulation
stopifnot(length(both) + length(usps_only) == nrow(gn),
          length(both) + length(zcta_only) == nrow(gz))

lists <- data.frame(
  list = c("postal codes in circulation", "ZIP Code Tabulation Areas",
           "in both", "postal code with no tabulation area",
           "tabulation area with no postal code"),
  what_it_is = c("routing keys, compiled from mail addresses",
                 "areas, built from census blocks",
                 "a code that is also an area",
                 "a code that names no ground at all",
                 "ground the compilation does not list"),
  count = c(nrow(gn), nrow(gz), length(both), length(usps_only),
            length(zcta_only)))
dd_write_csv(lists, "derived/lists.csv")
print(lists)

check("postal codes in the compilation", n(nrow(gn)))
check("ZIP Code Tabulation Areas in the gazetteer", n(nrow(gz)))
check("codes in both lists", n(length(both)))
check("postal codes with no tabulation area", n(length(usps_only)))

# ===========================================================================
# 2.  POPULATION, TWICE
#
#     A ZIP area does have a population, and it has two of them.  The decennial
#     census is tabulated at summary level 860 -- the tabulation area -- so
#     there is a COUNT, and that is what everything below is weighted by.  The
#     American Community Survey publishes an ESTIMATE for the same areas three
#     years later.  Both are read, because the gap between them is a fact about
#     the unit rather than an error in either.
# ===========================================================================
KEY <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(KEY)) stop("set CENSUS_API_KEY in ~/.Renviron -- see the header")

DHC <- paste0("https://api.census.gov/data/2020/dec/dhc?get=P1_001N",
              "&for=zip%20code%20tabulation%20area:*")
dhc_path <- file.path(CACHE, "dhc_p1_zcta.json")
if (!file.exists(dhc_path))
  prov_fetch(paste0(DHC, "&key=", KEY), dhc_path, label = "2020 DHC P1 by ZCTA")
# [["P1_001N","zip code tabulation area"],["854","05150"],...] -- parsed without
# jsonlite, the way the other API-reading chapter in this corpus does it.
cells <- regmatches(readChar(dhc_path, file.size(dhc_path)),
                    gregexpr('"[^"]*"', readChar(dhc_path, file.size(dhc_path)))
                    )[[1]]
cells <- gsub('"', "", cells)
stopifnot(cells[1] == "P1_001N", cells[2] == "zip code tabulation area")
cells <- cells[-(1:2)]
dhc <- data.frame(code = cells[seq(2, length(cells), by = 2)],
                  pop  = as.numeric(cells[seq(1, length(cells), by = 2)]))
stopifnot(all(nchar(dhc$code) == 5), !anyDuplicated(dhc$code))
say("2020 census counts by ZCTA: %s areas, %s people",
    n(nrow(dhc)), n(sum(dhc$pop)))

ACS_URL <- paste0("https://www2.census.gov/programs-surveys/acs/summary_file/",
                  "2023/table-based-SF/data/5YRData/acsdt5y2023-b01003.dat")
acs <- read.delim(grab_watched(ACS_URL, "acsdt5y2023-b01003.dat"),
                  sep = "|", colClasses = "character")
names(acs) <- c("GEO_ID", "est", "moe")
acs$est <- as.numeric(acs$est); acs$moe <- as.numeric(acs$moe)
# Suppressed and not-applicable values arrive as large negative sentinels.
acs$moe[acs$moe < 0] <- NA

zpop <- acs[startsWith(acs$GEO_ID, "860Z200US"), ]
zpop$code <- substr(zpop$GEO_ID, 10, 14)
stopifnot(all(nchar(zpop$code) == 5), !anyDuplicated(zpop$code))
ppop <- acs[startsWith(acs$GEO_ID, "1600000US"), ]
ppop$place <- substr(ppop$GEO_ID, 10, 16)
say("ACS ZCTA estimates: %s   place estimates: %s", n(nrow(zpop)), n(nrow(ppop)))

# The COUNT is the population used everywhere below.
gz$pop <- dhc$pop[match(gz$GEOID, dhc$code)]
gz$acs <- zpop$est[match(gz$GEOID, zpop$code)]
gz$moe <- zpop$moe[match(gz$GEOID, zpop$code)]
POP_TOTAL <- sum(gz$pop, na.rm = TRUE)
say("population counted inside a ZCTA in 2020: %s", n(POP_TOTAL))
fact("pop_source", "2020 Census, table P1, summary level 860",
     "the population weighting every count below")
fact("zctas_counted", sum(!is.na(gz$pop)),
     "tabulation areas with a 2020 census count")

# The two published populations for one ZIP area, and the distance between
# them. Three years apart, so this is not error in either -- it is what
# "the population of a ZIP code" means when two instruments answer it.
cmp <- gz[!is.na(gz$pop) & !is.na(gz$acs) & gz$pop >= 100, ]
gap <- abs(cmp$acs - cmp$pop) / cmp$pop
fact("acs_vs_count_median_pct", round(100 * median(gap), 1),
     "median gap between the survey estimate and the census count, ZIP areas of 100+")
fact("acs_vs_count_over_10pct", sum(gap > 0.10),
     "of those ZIP areas where the two differ by more than a tenth")
fact("acs_vs_count_n", nrow(cmp), "ZIP areas of 100+ with both readings")
rel_moe <- gz$moe[!is.na(gz$moe) & !is.na(gz$acs) & gz$acs > 0] /
           gz$acs[!is.na(gz$moe) & !is.na(gz$acs) & gz$acs > 0]
fact("moe_median_pct", round(100 * median(rel_moe), 1),
     "median margin of error as a share of the survey estimate")
check("2020 census population counted inside a tabulation area", n(POP_TOTAL))
check("median gap, ACS estimate against the census count",
      paste0(round(100 * median(gap), 1), "%"))

# ===========================================================================
# 3.  THE FIRST DIGIT IS A MAP
#
#     The reel's claim, and it is true.  A ZIP code is read left to right as a
#     sorting instruction: the first digit picks one of ten national areas,
#     the first three pick one of the sectional centre facilities that sort
#     for a region, and the last two pick the delivery unit.  Drawing the
#     first digit on the country therefore draws the mail system.
#
#     Which states belong to which digit is DERIVED below and not typed in.
# ===========================================================================
zc <- rel("tab20_zcta520_county20_natl.txt")
zcp <- zc[zc$GEOID_ZCTA5_20 != "" & zc$AREALAND_PART > 0, ]
zcp$state  <- substr(zcp$GEOID_COUNTY_20, 1, 2)
stopifnot(all(nchar(zcp$GEOID_ZCTA5_20) == 5), all(nchar(zcp$GEOID_COUNTY_20) == 5))

# A ZCTA's state, for labelling: the state holding the most of its land.  The
# next section is about how often that sentence has to be written this way.
o <- zcp[order(zcp$GEOID_ZCTA5_20, -zcp$AREALAND_PART), ]
main_state <- o$state[!duplicated(o$GEOID_ZCTA5_20)]
names(main_state) <- o$GEOID_ZCTA5_20[!duplicated(o$GEOID_ZCTA5_20)]
gz$state_fips <- unname(main_state[gz$GEOID])

# State FIPS -> postal abbreviation, from the gazetteer's own national file so
# that nothing here is a lookup table typed by hand.
ST_URL <- paste0("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/",
                 "2024_Gazetteer/2024_Gaz_counties_national.zip")
cnty <- read.delim(file.path(unzip_to(grab(ST_URL, "2024_Gaz_counties_national.zip")),
                             "2024_Gaz_counties_national.txt"),
                   sep = "\t", colClasses = "character", strip.white = TRUE)
names(cnty) <- trimws(names(cnty))
ST <- unique(data.frame(fips = substr(cnty$GEOID, 1, 2), ab = cnty$USPS))
ST <- setNames(ST$ab, ST$fips)
gz$state <- unname(ST[gz$state_fips])

digits <- data.frame(code = gz$GEOID, d1 = substr(gz$GEOID, 1, 1),
                     scf = substr(gz$GEOID, 1, 3), lat = gz$lat, lon = gz$lon,
                     aland = gz$ALAND, pop = gz$pop, state = gz$state)
dd_write_csv(digits, "derived/digits.csv")

# One row per leading digit.  `states` is the list of state abbreviations that
# hold at least one per cent of that digit's ZCTAs, largest first -- a rule,
# so that the row for 0 is what the data says and not what anybody remembers.
dr <- do.call(rbind, lapply(as.character(0:9), function(d) {
  s <- digits[digits$d1 == d & !is.na(digits$state), ]
  tb <- sort(table(s$state), decreasing = TRUE)
  tb <- tb[tb >= 0.01 * nrow(s)]
  data.frame(d1 = d, zctas = sum(digits$d1 == d),
             pop = sum(digits$pop[digits$d1 == d], na.rm = TRUE),
             scfs = length(unique(digits$scf[digits$d1 == d])),
             states = paste(names(tb), collapse = " "))
}))
stopifnot(sum(dr$zctas) == nrow(gz))
dd_write_csv(dr, "derived/digit_rows.csv")
print(dr[, c("d1", "zctas", "scfs", "states")])

fact("n_scf", length(unique(digits$scf)),
     "distinct three-digit prefixes among the tabulation areas")

# --- the prefixes, placed -----------------------------------------------
# The figure is drawn at the THREE-digit level rather than the five, because
# three digits is where the sorting happens: a sectional centre facility takes
# everything under one prefix. 896 points instead of 33,791 also means the
# figure is a figure and not a smear.
#
# Each prefix is placed at the population-weighted mean of its areas' interior
# points, projected to Albers equal area (EPSG:5070) so that the shape the
# points make is the shape of the country and not a stretched one.
d <- digits[!is.na(digits$lon) & !is.na(digits$lat), ]
d$w <- ifelse(is.na(d$pop) | d$pop < 1, 1, d$pop)
pt <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
xy <- st_coordinates(st_transform(pt, 5070))
d$px <- xy[, 1] / 1000; d$py <- xy[, 2] / 1000
wm <- function(v, w) sum(v * w) / sum(w)
top_state <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1]
}
scf <- do.call(rbind, lapply(split(d, d$scf), function(s) data.frame(
  scf = s$scf[1], d1 = s$d1[1], zctas = nrow(s),
  pop = sum(s$pop, na.rm = TRUE),
  x = round(wm(s$px, s$w), 1), y = round(wm(s$py, s$w), 1),
  state = top_state(s$state))))
# Alaska, Hawaii and Puerto Rico are 3,000 km outside the frame the other 47
# states fit in; the brief draws the contiguous states and says which prefixes
# it left out rather than moving them somewhere they are not.
scf$in_frame <- as.integer(!scf$state %in% c("AK", "HI", "PR") &
                           !is.na(scf$state))
dd_write_csv(scf, "derived/scf.csv")
say("sectional prefixes: %d, of which %d in the drawn frame",
    nrow(scf), sum(scf$in_frame))
fact("scf_out_of_frame", sum(scf$in_frame == 0),
     "three-digit prefixes outside the drawn frame")
fact("scf_out_of_frame_states",
     paste(sort(unique(scf$state[scf$in_frame == 0])), collapse = ", "),
     "where they are")

# ===========================================================================
# 4.  THE CODES WITH NO GROUND
#
#     A postal code that no tabulation area corresponds to is a code the
#     Census Bureau could not draw, because the addresses it serves do not
#     cover any territory: a single building, a wall of post office boxes, a
#     military mail channel.  Five are named below.  They are CHOSEN, not
#     sampled -- illustrations of the three kinds -- and each choice is
#     asserted against the two lists so a re-vintage cannot leave a dead
#     example standing in the brief.
# ===========================================================================
named <- data.frame(
  code = c("20500", "10118", "12345", "20898", "09096"),
  kind = c("one building", "one building", "one building",
           "post office boxes", "military mail"),
  note = c("the White House", "the Empire State Building",
           "General Electric, Schenectady",
           "boxes rented at one Maryland post office",
           "Army and Air Force post office, Europe"))
g_place <- gn$place[match(named$code, gn$code)]
g_state <- gn$state[match(named$code, gn$code)]
named$listed_as <- ifelse(nzchar(g_state), paste0(g_place, ", ", g_state), g_place)
named$lon <- gn$lon[match(named$code, gn$code)]
named$lat <- gn$lat[match(named$code, gn$code)]
stopifnot(all(named$code %in% gn$code), !any(named$code %in% gz$GEOID))
dd_write_csv(named[, c("code", "listed_as", "kind", "note", "lat", "lon")],
             "derived/no_ground.csv")
print(named[, c("code", "listed_as", "note")])

# 09096 is listed at coordinates in Germany.  That is not an error in the
# file; it is where that mail is handled.
fact("apo_lon", round(gn$lon[gn$code == "09096"], 3),
     "longitude the compilation gives for postal code 09096")
fact("apo_lat", round(gn$lat[gn$code == "09096"], 3),
     "latitude the compilation gives for postal code 09096")

ng <- gn[gn$code %in% usps_only, ]
ng$state[ng$state == ""] <- "(none)"
kind <- as.data.frame(sort(table(ng$state), decreasing = TRUE),
                      stringsAsFactors = FALSE)
names(kind) <- c("state", "codes_with_no_ground")
kind$state <- as.character(kind$state)
allst <- table(ifelse(nzchar(gn$state), gn$state, "(none)"))
kind$codes_in_state  <- as.numeric(allst[kind$state])
kind$share_of_state  <- round(100 * kind$codes_with_no_ground /
                              kind$codes_in_state, 1)
dd_write_csv(kind, "derived/no_ground_kind.csv")

# Military mail carries no state in the compilation at all: the place name is
# the channel -- APO for Army and Air Force, FPO for the fleet, DPO for the
# diplomatic post -- and the state column is empty because there is no state.
mil <- grepl("^(APO|FPO|DPO) ", ng$place)
fact("no_ground_military", sum(mil),
     "codes with no ground that are military or diplomatic mail channels")
fact("no_ground_states", sum(!kind$state %in% "(none)"),
     "states holding at least one postal code with no ground")
fact("no_ground_top_state", kind$state[1],
     "the state with the most postal codes that name no ground")
fact("no_ground_top_state_n", kind$codes_with_no_ground[1],
     "how many it has")

# ===========================================================================
# 5.  IT DOES NOT NEST
#
#     Counted from the Bureau's own relationship file, in square metres of
#     shared land, at three thresholds.  The threshold matters and pretending
#     otherwise would be the same mistake the chapter is about: a relationship
#     file records every overlap, including the few square metres where two
#     boundaries follow the same river bank a little differently.
# ===========================================================================
zland <- setNames(gz$ALAND, gz$GEOID)
zcp$share <- zcp$AREALAND_PART / zland[zcp$GEOID_ZCTA5_20]

span <- function(keys, tol) {
  d <- zcp[zcp$share >= tol, ]
  tapply(d[[keys]], d$GEOID_ZCTA5_20, function(x) length(unique(x)))
}
nesting <- do.call(rbind, lapply(c(0, 0.01, 0.05), function(tol) {
  cs <- span("GEOID_COUNTY_20", tol); ss <- span("state", tol)
  mc <- names(cs)[cs > 1]; ms <- names(ss)[ss > 1]
  data.frame(threshold = paste0(tol * 100, "%"),
             zctas = length(cs),
             in_two_or_more_counties = sum(cs > 1),
             in_two_or_more_states   = sum(ss > 1),
             pop_multi_county = sum(gz$pop[gz$GEOID %in% mc], na.rm = TRUE),
             pop_multi_state  = sum(gz$pop[gz$GEOID %in% ms], na.rm = TRUE))
}))
nesting$pct_multi_county <- round(100 * nesting$in_two_or_more_counties /
                                  nesting$zctas, 1)
nesting$pct_pop_multi_county <- round(100 * nesting$pop_multi_county /
                                      POP_TOTAL, 1)
dd_write_csv(nesting, "derived/nesting.csv")
print(nesting)

cs0 <- span("GEOID_COUNTY_20", 0)
fact("zcta_max_counties", max(cs0), "counties the widest ZIP area reaches into")
fact("zcta_max_counties_code", names(cs0)[which.max(cs0)],
     "the ZIP area that reaches into the most counties")

# Land with no ZIP area over it.  The relationship file carries a row for the
# part of a county that no ZCTA covers; those rows have an empty ZCTA side.
nocov <- zc[zc$GEOID_ZCTA5_20 == "" & zc$AREALAND_PART > 0, ]
covered   <- sum(zcp$AREALAND_PART)
uncovered <- sum(nocov$AREALAND_PART)
# The two sides have to add up to the land area of the counties, or one of them
# is measuring something other than what it says.
cty_land <- sum(as.numeric(unique(zc[, c("GEOID_COUNTY_20", "AREALAND_COUNTY_20")])$AREALAND_COUNTY_20))
stopifnot(abs(covered + uncovered - cty_land) / cty_land < 0.001)
fact("counties_with_uncovered_land", length(unique(nocov$GEOID_COUNTY_20)),
     "counties holding land no tabulation area covers")
fact("uncovered_sq_km", round(uncovered / 1e6),
     "square kilometres of land under no tabulation area")
fact("uncovered_pct", round(100 * uncovered / cty_land, 1),
     "share of the country's land under no tabulation area")

# The Bureau's stated rule for what it leaves out is "uninhabited areas over two
# square miles are potentially left unassigned". That is a testable claim about
# this file, so test it: if the rule is what happened, the uncovered land should
# sit almost entirely in pieces bigger than the threshold.
SQMI <- 2589988.11
big  <- nocov$AREALAND_PART > 2 * SQMI
fact("uncovered_over_2sqmi_pct", round(100 * sum(nocov$AREALAND_PART[big]) /
                                       uncovered, 2),
     "share of the uncovered land lying in pieces larger than two square miles")
fact("uncovered_parts", nrow(nocov), "separate uncovered pieces, one per county")
check("land under no tabulation area",
      sprintf("%s sq km, %s%% of the country",
              n(round(uncovered / 1e6)), round(100 * uncovered / cty_land, 1)))
check("of it in pieces over two square miles, the Bureau's stated threshold",
      paste0(round(100 * sum(nocov$AREALAND_PART[big]) / uncovered, 2), "%"))

check("ZIP areas reaching into two or more counties",
      sprintf("%s of %s", n(nesting$in_two_or_more_counties[1]),
              n(nesting$zctas[1])))
check("ZIP areas reaching into two or more states",
      n(nesting$in_two_or_more_states[1]))

# ===========================================================================
# 6.  IT MOVES
#
#     The 2010 areas against the 2020 areas.  Codes retire, codes appear, and
#     the ones that survive under the same five digits are not the same shape.
# ===========================================================================
zz <- rel("tab20_zcta510_zcta520_natl.txt")
old <- unique(zz$GEOID_ZCTA5_10[zz$GEOID_ZCTA5_10 != ""])
new <- unique(zz$GEOID_ZCTA5_20[zz$GEOID_ZCTA5_20 != ""])
zzp <- zz[zz$GEOID_ZCTA5_10 != "" & zz$GEOID_ZCTA5_20 != "" &
          zz$AREALAND_PART > 0, ]
zzp$AREALAND_ZCTA5_10 <- as.numeric(zzp$AREALAND_ZCTA5_10)

# For a code present in both vintages: how much of its 2010 land is still
# inside the 2020 area of the SAME code.
same <- zzp[zzp$GEOID_ZCTA5_10 == zzp$GEOID_ZCTA5_20, ]
kept <- tapply(same$AREALAND_PART, same$GEOID_ZCTA5_10, sum) /
        tapply(same$AREALAND_ZCTA5_10, same$GEOID_ZCTA5_10, max)
survivors <- intersect(old, new)
kept <- kept[survivors]
kept[is.na(kept)] <- 0

churn <- data.frame(
  what = c("codes with an area in 2010", "codes with an area in 2020",
           "2010 codes gone by 2020", "2020 codes that did not exist in 2010",
           "codes in both, whose area is unchanged",
           "codes in both, that kept less than 95% of their 2010 land"),
  count = c(length(old), length(new), length(setdiff(old, new)),
            length(setdiff(new, old)), sum(kept > 0.9999),
            sum(kept < 0.95)))
dd_write_csv(churn, "derived/churn.csv")
print(churn)

fact("kept_median_pct", round(100 * median(kept), 1),
     "median share of its 2010 land a surviving ZIP area still holds")
check("ZIP areas that changed shape between 2010 and 2020",
      sprintf("%s of %s", n(sum(kept <= 0.9999)), n(length(survivors))))

# ===========================================================================
# 7.  IT DOES NOT FIT A CITY
#
#     Grofman and Cervas (2021) report, from their own reading, that pieces of
#     seventeen ZIP codes are in the City of Irvine and that by their best
#     judgment only two are wholly inside it.  That claim is recomputed here
#     against the relationship file, for Irvine and for Pittsburgh, and then
#     for the hundred largest cities so that neither is taken for typical.
# ===========================================================================
zp <- rel("tab20_zcta520_place20_natl.txt")
zp <- zp[zp$GEOID_ZCTA5_20 != "" & zp$GEOID_PLACE_20 != "" &
         zp$AREALAND_PART > 0, ]
zp$AREALAND_ZCTA5_20 <- as.numeric(zp$AREALAND_ZCTA5_20)
zp$share <- zp$AREALAND_PART / zp$AREALAND_ZCTA5_20

city_row <- function(place, tol) {
  d <- zp[zp$GEOID_PLACE_20 == place, ]
  data.frame(place = place,
             name = sub(" (city|town|borough|village)$", "", d$NAMELSAD_PLACE_20[1]),
             threshold = paste0(tol * 100, "%"),
             touching = sum(d$share >= tol),
             wholly_inside = sum(d$share > 0.9999),
             pop = ppop$est[match(place, ppop$place)])
}
IRVINE <- "0636770"; PITT <- "4261000"
cities <- do.call(rbind, c(
  lapply(c(0, 0.01, 0.05), function(t) city_row(IRVINE, t)),
  lapply(c(0, 0.01, 0.05), function(t) city_row(PITT, t))))
dd_write_csv(cities, "derived/cities.csv")
print(cities)

# The hundred largest cities, so the two named ones can be placed.
big <- ppop[order(-ppop$est), ]
big <- big[big$place %in% zp$GEOID_PLACE_20, ][1:100, ]
cb <- do.call(rbind, lapply(big$place, function(p) city_row(p, 0.01)))
cb$pct_wholly <- round(100 * cb$wholly_inside / cb$touching, 1)
cb <- cb[order(-cb$pop), ]
dd_write_csv(cb, "derived/cities_big.csv")

fact("big_median_touching", median(cb$touching),
     "median ZIP areas with a real piece inside one of the 100 largest cities")
fact("big_median_pct_wholly", round(median(cb$pct_wholly), 1),
     "median share of those ZIP areas that are wholly inside the city")
fact("big_cities_none_wholly", sum(cb$wholly_inside == 0),
     "of the 100 largest cities that contain no ZIP area whole")

# ===========================================================================
# 8.  PITTSBURGH, DRAWN
#
#     Figure 1 of Grofman and Cervas (2021) is a piece of Pittsburgh: ZIP code
#     15260 is a two-piece polygon lying entirely within 15213, which is where
#     this course is taught.  The shapes are read from the Bureau's own
#     cartographic file rather than redrawn from the paper.
# ===========================================================================
zsf <- shp(paste0("https://www2.census.gov/geo/tiger/GENZ2020/shp/",
                  "cb_2020_us_zcta520_500k.zip"), "cb_2020_us_zcta520_500k.zip")
zid <- grep("^ZCTA5CE", names(zsf), value = TRUE)[1]
zsf$code <- as.character(zsf[[zid]])
stopifnot(nrow(zsf) == nrow(gz))

psf <- shp(paste0("https://www2.census.gov/geo/tiger/GENZ2020/shp/",
                  "cb_2020_42_place_500k.zip"), "cb_2020_42_place_500k.zip")
city <- st_transform(st_make_valid(psf[psf$GEOID == PITT, ]), 5070)

# Every ZCTA with a real piece inside the city limits, plus its neighbours, so
# the figure shows the ones that spill out as well as the ones that do not.
in_city <- zp$GEOID_ZCTA5_20[zp$GEOID_PLACE_20 == PITT & zp$share >= 0.01]
near <- unique(c(in_city, zp$GEOID_ZCTA5_20[zp$GEOID_ZCTA5_20 %in%
                 zcp$GEOID_ZCTA5_20[zcp$GEOID_COUNTY_20 == "42003"]]))
pz <- st_transform(st_make_valid(zsf[zsf$code %in% near, ]), 5070)
pz <- pz[st_intersects(pz, st_buffer(city, 1500), sparse = FALSE)[, 1], ]
pz$share_in_city <- zp$share[match(paste(pz$code, PITT),
                                   paste(zp$GEOID_ZCTA5_20, zp$GEOID_PLACE_20))]
pz$share_in_city[is.na(pz$share_in_city)] <- 0
pz <- pz[order(pz$code), ]
say("ZIP areas drawn around Pittsburgh: %d", nrow(pz))

# 15213 and 15260, checked rather than asserted: the two-piece one has to have
# two pieces, and everything it touches has to be the other one.
a <- pz[pz$code == "15213", ]; b <- pz[pz$code == "15260", ]
stopifnot(nrow(a) == 1, nrow(b) == 1)
n_parts_15260 <- length(st_cast(st_geometry(b), "POLYGON"))
touch <- pz$code[st_intersects(b, pz, sparse = FALSE)[1, ]]
touch <- setdiff(touch, "15260")
# Rings, counted through st_cast so that it does not matter whether validation
# left the feature a POLYGON or a MULTIPOLYGON: L1 numbers the rings of each
# piece, and a ring after the first is a hole.
crd <- st_coordinates(st_cast(st_geometry(a), "POLYGON"))
rings_15213 <- nrow(unique(crd[, c("L1", "L2"), drop = FALSE]))
say("15260: %d pieces, touching %s; 15213 has %d rings",
    n_parts_15260, paste(touch, collapse = " "), rings_15213)
stopifnot(n_parts_15260 == 2, identical(touch, "15213"), rings_15213 == 3)

fact("pgh_15260_parts", n_parts_15260, "disconnected pieces of ZIP area 15260")
fact("pgh_15213_rings", rings_15213,
     "rings in the polygon for 15213: one outline and two holes")
fact("pgh_15260_sq_km", round(as.numeric(st_area(b)) / 1e6, 2),
     "land area of 15260")
fact("pgh_15213_sq_km", round(as.numeric(st_area(a)) / 1e6, 2),
     "land area of 15213")
fact("pgh_15213_pop", gz$pop[gz$GEOID == "15213"], "2020 census count for 15213")
fact("pgh_15260_pop", gz$pop[gz$GEOID == "15260"], "2020 census count for 15260")

# --- rings out, in the idiom the other mapped chapters use ------------------
# Kilometres on EPSG:5070, shifted so the drawn window's lower-left corner is
# the origin.  One row per coordinate; `part` separates the rings of a polygon,
# so a hole is drawn as its own outline, which is exactly what these two
# figures need to show.
ORG <- st_bbox(st_buffer(city, 2000))
ringsof <- function(g, lev) {
  gm <- st_geometry(g); out <- vector("list", length(gm))
  for (i in seq_along(gm)) {
    p <- st_coordinates(gm[i])
    part <- as.integer(factor(paste(
      p[, "L1"], if ("L2" %in% colnames(p)) p[, "L2"] else 1,
      if ("L3" %in% colnames(p)) p[, "L3"] else 1)))
    out[[i]] <- data.frame(lev = lev, uid = i, part = part,
                           x = round((p[, "X"] - ORG["xmin"]) / 1000, 3),
                           y = round((p[, "Y"] - ORG["ymin"]) / 1000, 3))
  }
  do.call(rbind, out)
}
thin <- function(g, tol) st_simplify(g, dTolerance = tol, preserveTopology = TRUE)
pr <- rbind(ringsof(thin(pz, 10), "z"), ringsof(thin(city, 10), "c"))
pr <- pr[!is.na(pr$x), ]
np <- ave(pr$x, pr$lev, pr$uid, pr$part, FUN = length)
pr <- pr[np >= 4, ]
dd_write_csv(pr, "derived/pgh_rings.csv")

# --- the blocks a ZIP area is made of ---------------------------------------
# The claim "a ZCTA is a union of whole census blocks" is a claim about these
# two files, so it is tested on them rather than quoted. Every block's own
# interior point decides which ZIP area it is in; then the blocks assigned to a
# ZIP area are dissolved and their outline compared with the published one. If
# the definition is what it says, the two outlines are the same outline.
BLKZ <- grab(paste0("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/",
                    "tl_2020_42_tabblock20.zip"), "tl_2020_42_tabblock20.zip")
BLKD <- unzip_to(BLKZ)
blk <- st_read(list.files(BLKD, "\\.shp$", full.names = TRUE)[1], quiet = TRUE,
               query = paste("SELECT GEOID20, POP20, ALAND20, INTPTLAT20,",
                             "INTPTLON20 FROM tl_2020_42_tabblock20",
                             "WHERE COUNTYFP20 = '003'"))
say("Allegheny County blocks: %s", n(nrow(blk)))
stopifnot(nrow(blk) > 10000, all(nchar(as.character(blk$GEOID20)) == 15))

blk <- st_transform(st_make_valid(blk), 5070)
ip  <- st_transform(st_as_sf(data.frame(
         lon = as.numeric(blk$INTPTLON20), lat = as.numeric(blk$INTPTLAT20)),
         coords = c("lon", "lat"), crs = 4326), 5070)
hit <- st_within(ip, pz, sparse = FALSE)
blk$code <- NA_character_
one <- rowSums(hit) == 1
blk$code[one] <- pz$code[apply(hit[one, , drop = FALSE], 1, which.max)]
say("blocks landing in one of the drawn ZIP areas: %s of %s",
    n(sum(!is.na(blk$code))), n(nrow(blk)))

# THE TEST, and the choice of instrument matters. Comparing the dissolved block
# outline with the published ZIP-area outline measures the wrong thing: the
# blocks are TIGER at full resolution and the ZIP areas here are the generalised
# cartographic file, so the two disagree by a few per cent no matter how true
# the definition is. Compare the QUANTITIES instead. Land area and population
# are attributes the Bureau measured, not properties of a drawn line, so no
# generalisation can move them. If a ZIP area really is a union of whole blocks,
# its blocks' land must sum to its land and its blocks' people to its count.
agree <- function(k) {
  b <- blk[!is.na(blk$code) & blk$code == k, ]
  data.frame(code = k, blocks = nrow(b),
             block_aland = sum(b$ALAND20), zcta_aland = gz$ALAND[gz$GEOID == k],
             block_pop = sum(b$POP20),     zcta_pop  = gz$pop[gz$GEOID == k])
}
ag <- rbind(agree("15213"), agree("15260"))
ag$aland_gap <- ag$block_aland / ag$zcta_aland - 1
ag$pop_gap   <- ag$block_pop - ag$zcta_pop
print(ag)
# THE POPULATION HAS TO BE EXACT. It is the same people counted twice, once by
# block and once by ZIP area, so any difference at all would mean the ZIP area
# is not those blocks.
#
# The land is allowed a per cent, and the reason is worth recording because it
# is the same lesson again. Which ZIP area a block belongs to is decided here by
# dropping the block's interior point into the GENERALISED ZIP-area outline, and
# on a generalised outline a boundary can sit a couple of hundred metres from
# where the full-resolution file puts it. That is enough to put an edge block on
# the wrong side. It happens to 15213 -- its blocks' land runs 0.25% over -- and
# the blocks involved hold nobody, which is why the population still lands
# exactly. Using the Bureau's own block-to-ZCTA relationship file instead of a
# point-in-polygon test would close the gap, at the cost of a one-gigabyte
# download for two numbers.
stopifnot(all(abs(ag$aland_gap) < 0.01), all(ag$pop_gap == 0))
fact("blocks_aland_gap_pct", round(100 * max(abs(ag$aland_gap)), 2),
     "largest land-area gap between a ZIP area and the blocks assigned to it here")
dd_write_csv(ag, "derived/blocks_check.csv")
fact("blocks_15213", ag$blocks[1], "census blocks making up ZIP area 15213")
fact("blocks_15260", ag$blocks[2], "census blocks making up ZIP area 15260")
check("15213 rebuilt from its own census blocks",
      sprintf("%s blocks, %s people -- the published count exactly",
              n(ag$blocks[1]), n(ag$block_pop[1])))

# The blocks of those two areas, on the same origin as the ZIP-area rings, so
# the figure can lay one over the other. Not simplified: the whole point is
# that the block edges and the ZIP-area edge are the same edges.
bsel <- blk[!is.na(blk$code) & blk$code %in% c("15213", "15260"), ]
bsel <- bsel[order(bsel$GEOID20), ]
br <- ringsof(bsel, "b")
br$code <- bsel$code[br$uid]
br <- br[!is.na(br$x), ]
nb <- ave(br$x, br$uid, br$part, FUN = length)
br <- br[nb >= 4, ]
dd_write_csv(br, "derived/pgh_blocks.csv")
say("pgh_blocks.csv: %s blocks, %s coordinate rows", n(nrow(bsel)), n(nrow(br)))

pids <- rbind(
  data.frame(lev = "z", uid = seq_len(nrow(pz)), code = pz$code,
             sq_km = round(as.numeric(st_area(pz)) / 1e6, 3),
             share_in_city = round(pz$share_in_city, 4),
             pop = gz$pop[match(pz$code, gz$GEOID)]),
  data.frame(lev = "c", uid = 1L, code = "4261000",
             sq_km = round(as.numeric(st_area(city)) / 1e6, 3),
             share_in_city = 1, pop = ppop$est[ppop$place == PITT]))
dd_write_csv(pids, "derived/pgh_ids.csv")
say("pgh_rings.csv: %s coordinate rows", n(nrow(pr)))

# ===========================================================================
# 9.  IT DOES NOT FIT A DISTRICT
#
#     The one operation here with no relationship file behind it, because the
#     Bureau publishes none between ZCTAs and congressional districts.  So it
#     is done by intersecting polygons, and the threshold is reported rather
#     than buried: two generalised boundaries that follow the same street can
#     disagree by a few metres, and at a threshold of zero every such
#     disagreement would be counted as a district split.
# ===========================================================================
cds <- shp(paste0("https://www2.census.gov/geo/tiger/GENZ2024/shp/",
                  "cb_2024_us_cd119_500k.zip"), "cb_2024_us_cd119_500k.zip")
CDF <- grep("^CD\\d+FP$", names(cds), value = TRUE)[1]
stopifnot(!is.na(CDF))
cds$cd <- as.character(cds[[CDF]])
PA <- "42"
pacd <- st_transform(st_make_valid(cds[cds$STATEFP == PA, ]), 5070)
say("Pennsylvania districts in the 119th Congress: %d", nrow(pacd))
stopifnot(nrow(pacd) == 17)

pazc <- unique(zcp$GEOID_ZCTA5_20[zcp$state == PA & zcp$share >= 0.01])
paz  <- st_transform(st_make_valid(zsf[zsf$code %in% pazc, ]), 5070)
paz  <- paz[order(paz$code), ]
say("Pennsylvania ZIP areas: %d", nrow(paz))

# Shared area, ZIP area by district, as a share of the ZIP area's own land.
ix <- st_intersection(paz[, "code"], pacd[, "cd"])
ix$a <- as.numeric(st_area(ix))
# The denominator is the ZIP area's land INSIDE Pennsylvania, not its total, so
# that a ZIP area straddling the Ohio line is not recorded as split merely for
# being partly in another state. That is a different fact, counted in §5.
own  <- tapply(ix$a, ix$code, sum)
ix$f <- ix$a / own[ix$code]

split_at <- function(tol) {
  d <- ix[ix$f >= tol, ]
  k <- tapply(d$cd, d$code, function(x) length(unique(x)))
  pop <- gz$pop[match(names(k)[k > 1], gz$GEOID)]
  data.frame(threshold = paste0(tol * 100, "%"),
             zip_areas = length(k), split = sum(k > 1),
             pct_split = round(100 * sum(k > 1) / length(k), 1),
             pop_in_split = sum(pop, na.rm = TRUE),
             most_districts = max(k))
}
pa_split <- do.call(rbind, lapply(c(0.01, 0.02, 0.05), split_at))
pa_split$pct_pop_in_split <- round(100 * pa_split$pop_in_split /
  sum(gz$pop[gz$GEOID %in% paz$code], na.rm = TRUE), 1)
dd_write_csv(pa_split, "derived/pa_split.csv")
print(pa_split)

fact("pa_zip_areas", nrow(paz), "Pennsylvania ZIP areas measured against districts")
fact("pa_districts", nrow(pacd), "Pennsylvania seats in the 119th Congress")
check("Pennsylvania ZIP areas split between districts (2% threshold)",
      sprintf("%s of %s", n(pa_split$split[2]), n(pa_split$zip_areas[2])))

# ===========================================================================
# 10.  THE TWO OBJECTS, SIDE BY SIDE
#
#     The question the chapter is most often asked -- are a ZIP code and a ZCTA
#     different, and how -- answered in one table. The prose columns are the
#     Census Bureau's own description of what it does; the evidence column is
#     computed above, so the table cannot drift from the rest of the chapter.
# ===========================================================================
survivors_n <- length(survivors)
zvz <- data.frame(
  question = c("What is it?",
               "Who defines it?",
               "How does it exist in a file?",
               "How many are there?",
               "How many have no counterpart?",
               "Where does its edge fall?",
               "When does it change?",
               "Does it cover the country?",
               "Does it have a population?"),
  zip = c("a label on a set of delivery addresses",
          "U.S. Postal Service",
          "a field on each address point",
          n(nrow(gn)),
          paste(n(length(usps_only)), "have no tabulation area"),
          "nowhere -- it has no edge",
          "whenever routing changes",
          "only where mail is delivered",
          "no -- nobody counts delivery stops"),
  zcta = c("an area made of whole census blocks",
           "U.S. Census Bureau",
           "a polygon",
           n(nrow(gz)),
           paste(n(length(zcta_only)), "carry a code no longer in circulation"),
           "on a census block boundary, always",
           "once, at each decennial census",
           "inhabited land; empty pieces over 2 sq mi are left out",
           "yes -- the census count of its blocks"),
  evidence = c("", "", "",
               paste0(n(length(both)), " codes are on both lists"),
               "",
               "",
               paste0(n(sum(kept <= 0.9999)), " of ", n(survivors_n),
                      " changed shape between 2010 and 2020"),
               paste0(round(100 * uncovered / cty_land, 1),
                      "% of the country's land is under no tabulation area"),
               paste0(n(POP_TOTAL), " people counted inside one in 2020")))
dd_write_csv(zvz, "derived/zip_vs_zcta.csv")
print(zvz[, c("question", "zip", "zcta")])

# ===========================================================================
# 11.  SCALARS
# ===========================================================================
fact("n_usps", nrow(gn), "postal codes in the compilation")
fact("n_zcta", nrow(gz), "ZIP Code Tabulation Areas")
fact("n_both", length(both), "codes that are also areas")
fact("n_no_ground", length(usps_only), "postal codes with no area")
fact("n_no_code", length(zcta_only), "areas with no code in the compilation")
fact("pct_no_ground", round(100 * length(usps_only) / nrow(gn), 1),
     "share of postal codes with no area")
fact("pop_in_zctas", POP_TOTAL, "people counted inside a tabulation area in 2020")
fact("irvine_touching", cities$touching[cities$place == IRVINE &
                                        cities$threshold == "1%"],
     "ZIP areas with a real piece inside the City of Irvine")
fact("irvine_wholly", cities$wholly_inside[cities$place == IRVINE &
                                           cities$threshold == "1%"],
     "of those wholly inside Irvine")
fact("pgh_touching", cities$touching[cities$place == PITT &
                                     cities$threshold == "1%"],
     "ZIP areas with a real piece inside the City of Pittsburgh")
fact("pgh_wholly", cities$wholly_inside[cities$place == PITT &
                                        cities$threshold == "1%"],
     "of those wholly inside Pittsburgh")
fact("fetch_date", FETCH_DATE, "every source above fetched on this date")

facts <- data.frame(name = names(FACTS),
                    value = vapply(FACTS, function(f) as.character(f$value), ""),
                    note  = vapply(FACTS, function(f) f$note, ""))
dd_write_csv(facts, "derived/facts.csv")
print(facts)

checks <- data.frame(check = vapply(CHECKS, function(c) c$check, ""),
                     value = vapply(CHECKS, function(c) as.character(c$value), ""))
write.csv(checks, "derived/checks.csv", row.names = FALSE)
print(checks)

prov_report()
say("\nwrote: %s", paste(list.files("derived"), collapse = " "))

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
