# ---------------------------------------------------------------------------
# build-data.R  --  the `demographics` lab
#
# What the country looks like, at every scale the census publishes, from the
# 2020 Census Redistricting Data (P.L. 94-171) Summary File.
#
# ---------------------------------------------------------------------------
# SOURCES.  Everything below is keyless and public.  No Census API is used and
# no API key exists anywhere in this lab; the Bureau publishes these as flat
# files over plain HTTPS.
#
#   [1] U.S. Census Bureau, 2020 Census Redistricting Data (P.L. 94-171)
#       Summary File, legacy format.  Tables P1 (race), P2 (Hispanic or Latino
#       origin by race) and P4 (P2 for the voting-age population).
#         national roll-up (nation, region, division, state, metro areas):
#           https://www2.census.gov/programs-surveys/decennial/2020/data/
#             01-Redistricting_File--PL_94-171/National/us2020.npl.zip
#         one file per state, e.g. Michigan:
#           https://www2.census.gov/programs-surveys/decennial/2020/data/
#             01-Redistricting_File--PL_94-171/Michigan/mi2020.pl.zip
#       fetched 2026-08-11.
#       National file: 816,297 bytes, 2,832 geographic records.
#       51 state files (50 states + District of Columbia), 1.3 GB compressed.
#       Puerto Rico is deliberately excluded; it is not in the national total.
#
#   [2] U.S. Census Bureau, TIGER/Line Shapefiles, 2020.  Fetched 2026-08-11.
#         https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_26_tract.zip
#         https://www2.census.gov/geo/tiger/TIGER2020/BG/tl_2020_26_bg.zip
#         https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/
#           tl_2020_26_tabblock20.zip
#       State outlines are read from the copy this course already caches at
#       ../../../03-elections/residual-votes/data/raw/tiger/st/tl_2020_us_state.shp (TIGER 2020),
#       and Michigan tracts from ../../../05-ordinary-people/redlining/data/raw/tiger/26/, so the build
#       downloads only what is not already on disk.
#
#   [3] Office of Management and Budget, Statistical Policy Directive No. 15,
#       1977, 1997 and 2024 versions.  See `derived/categories.csv` and the notes in
#       Section 6; the dates and citations there are transcribed from the
#       Federal Register and are not computed.
#
# ---------------------------------------------------------------------------
# WHAT IT WRITES, all into this folder.  Nothing here is larger than a few
# hundred kilobytes; the raw inputs are processed and discarded.
#
#   derived/nation.csv        1 row     the United States, every category, two ways
#   derived/regions.csv      13 rows    4 census regions and 9 divisions
#   derived/states.csv       51 rows    50 states and DC
#   derived/counties.csv  3,143 rows    every county and county equivalent
#   derived/ladder.csv        8 rows    the measure, computed at each rung
#   derived/tract_dist.csv              binned tract composition, for the density figure
#   derived/metros.csv                  the 50 largest metro/micro areas
#   derived/categories.csv              which category existed in which census
#   wayne_*.csv                 the zoom: Wayne County, Michigan
#   derived/us_rings.csv                state outlines, Albers, for the small multiples
#   derived/facts.csv                   every scalar the documents quote
#
# Run it from inside this folder.  Set DEMOG_CACHE to a directory holding the
# already-downloaded .zip files to skip the downloads.
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


source("../../../_lib/precision.R")   # dd_signif(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE, timeout = 3600)
suppressPackageStartupMessages({
  library(data.table)
})

FETCHED <- "2026-08-11"
CACHE   <- Sys.getenv("DEMOG_CACHE", unset = tempdir())
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
STAGE   <- file.path(CACHE, "stage")
dir.create(STAGE, showWarnings = FALSE, recursive = TRUE)

PLBASE <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/",
                 "data/01-Redistricting_File--PL_94-171/")
TIGER  <- "https://www2.census.gov/geo/tiger/TIGER2020/"

say <- function(...) cat(..., "\n", sep = "")

# A cached download.  The build is re-runnable and the Census servers are slow.
grab <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 1000) return(dest)
  say("  fetching ", basename(dest))
  prov_fetch(url, dest, mode = "wb", quiet = TRUE)
  dest
}

# ===========================================================================
# Section 0 -- the categories, and the arithmetic of the two questions
# ===========================================================================
#
# The 2020 form asks TWO questions.  Question 8 is Hispanic origin; question 9
# is race.  The `census-race` lab in this course is about those two questions and
# their write-in boxes and does not need repeating here.  What this lab needs
# is the tabulation that results, and there are two of them.
#
# TABLE P2 gives EIGHT mutually exclusive, exhaustive categories.  Every
# resident of the United States is in exactly one.  This is the universe every
# figure in this lab counts unless it says otherwise, and it is the universe
# the diversity measure is computed over.

CAT <- c("hispanic", "nh_white", "nh_black", "nh_aian", "nh_asian",
         "nh_nhpi", "nh_sor", "nh_two")
CATLAB <- c(
  hispanic = "Hispanic or Latino (of any race)",
  nh_white = "White alone",
  nh_black = "Black or African American alone",
  nh_aian  = "American Indian and Alaska Native alone",
  nh_asian = "Asian alone",
  nh_nhpi  = "Native Hawaiian and Other Pacific Islander alone",
  nh_sor   = "Some Other Race alone",
  nh_two   = "Two or More Races")
CATSHORT <- c(hispanic = "Hispanic", nh_white = "White", nh_black = "Black",
              nh_aian = "AIAN", nh_asian = "Asian", nh_nhpi = "NHPI",
              nh_sor = "Some Other Race", nh_two = "Two or More")

# TABLE P1 gives race WITHOUT regard to Hispanic origin, and it gives all 63
# combinations.  Since 1997 a respondent may mark more than one race, so each
# race has two counts: ALONE, and ALONE OR IN ANY COMBINATION.  Both are
# correct.  P1 is laid out as 71 fields:
#     1 total; 2 one race; 3-8 the six races alone; 9 two or more races;
#     10 two races; 11-25 the 15 two-race combinations;
#     26 three races; 27-46 the 20 three-race combinations;
#     47 four races; 48-62 the 15 four-race combinations;
#     63 five races; 64-69 the 6 five-race combinations;
#     70 six races; 71 the single six-race combination.
# The combinations run in lexicographic order over 1=White, 2=Black, 3=AIAN,
# 4=Asian, 5=NHPI, 6=Some Other Race, which is exactly what combn() produces,
# so the index is built rather than typed.

RACE6 <- c("white", "black", "aian", "asian", "nhpi", "sor")
combo_fields <- local({
  out <- list(); f <- 9L                       # field 9 is "two or more races"
  for (k in 2:6) {
    f <- f + 1L                                # the "population of k races" line
    cm <- combn(6L, k)
    for (j in seq_len(ncol(cm))) { f <- f + 1L; out[[length(out) + 1L]] <-
      list(field = f, races = cm[, j]) }
  }
  out
})
stopifnot(length(combo_fields) == 15 + 20 + 15 + 6 + 1,          # 57
          max(vapply(combo_fields, `[[`, 0, "field")) == 71L)

# ===========================================================================
# Section 1 -- the diversity measure
# ===========================================================================
#
# WHICH MEASURE, AND WHY.  The zoom in this lab has to produce a number that
# changes, not just a prettier map at each rung.  The choice is the MULTIGROUP
# ENTROPY INDEX, and the reasoning is worth stating because the alternative is
# the one most people reach for first.
#
# The dissimilarity index D is the standard segregation statistic, and it takes
# exactly two groups.  With eight categories that means 28 separate numbers, or
# an arbitrary decision about which single contrast counts -- and deciding that
# "Black versus white" is THE contrast is a substantive claim about the country
# that the data should not be quietly making on our behalf.  Entropy takes all
# eight at once.
#
# For a unit i with shares p_ir over the eight categories:
#     E_i = sum_r p_ir * ln(1 / p_ir)          (0 when everyone matches;
#                                               ln(8) = 2.079 when even)
# For the whole country, E is the same formula on national shares.  Then
#     Ebar_L = sum_i (t_i / T) * E_i           population-weighted mean
#     H_L    = (E - Ebar_L) / E                Theil's information index
#
# Ebar_L answers the question this lab is actually asking: how mixed is the
# place the average American lives in, when "place" means a unit of level L?
# H_L rescales that as the share of national diversity that is missing from the
# average neighbourhood.  H is 0 at the national rung by construction.
#
# LIMITATIONS, all of which the documents state:
#   * H depends entirely on the categories.  Change the category list -- which
#     is exactly what the 2024 revision of OMB Directive 15 does -- and every
#     number here changes.  The measure is not a fact about people; it is a
#     fact about people AS CLASSIFIED.
#   * Finer units always yield higher H, even under random assignment, because
#     small units are noisy.  That is a real artifact and it is why this script
#     computes a null: people shuffled at random into units of the identical
#     sizes.  The gap between observed and null is the part that is sorting.
#   * The 2020 block counts carry differential-privacy noise, so the block rung
#     is reported with that warning attached and is never the headline.
#   * H says nothing about WHY people are where they are.

entropy <- function(M) {
  # M: matrix of counts, one row per unit, one column per category.
  tot <- rowSums(M)
  P <- M / ifelse(tot == 0, 1, tot)
  L <- ifelse(P > 0, P * log(1 / P), 0)
  e <- rowSums(L)
  e[tot == 0] <- NA_real_
  e
}
wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])

# The null: hand out the same total population to units of the identical sizes,
# drawing each person independently from the NATIONAL composition.  Done with
# sequential conditional binomials so it is vectorised over millions of units
# rather than looped.
null_entropy <- function(sizes, p, seed = 20260811) {
  set.seed(seed)
  n <- length(sizes)
  M <- matrix(0, n, length(p))
  rem <- sizes; pr <- 1
  for (j in seq_along(p)) {
    if (j == length(p)) { M[, j] <- rem; break }
    q <- p[j] / pr
    M[, j] <- rbinom(n, rem, min(max(q, 0), 1))
    rem <- rem - M[, j]; pr <- pr - p[j]
  }
  entropy(M)
}

# ===========================================================================
# Section 2 -- the national file: nation, region, division, state, metro
# ===========================================================================

say("Section 2: national roll-up")

npl_zip <- grab(paste0(PLBASE, "National/us2020.npl.zip"),
                file.path(CACHE, "us2020.npl.zip"))
npl_dir <- file.path(CACHE, "npl"); dir.create(npl_dir, showWarnings = FALSE)
unzip(npl_zip, exdir = npl_dir)

# Geographic header.  Column 3 is SUMLEV, 8 is LOGRECNO, 10 the geographic
# code, 11 region, 12 division, 13 state FIPS, 87 the name, 91 population.
# Everything is read as character: a state FIPS is "01", not 1.
rd_geo <- function(f) {
  g <- fread(f, sep = "|", header = FALSE, colClasses = "character",
             quote = "", showProgress = FALSE, encoding = "Latin-1",
             select = c(3, 8, 10, 11, 12, 13, 87, 91))
  setnames(g, c("sumlev", "logrec", "geoid", "region", "division", "st",
                "name", "pop"))
  g
}
# Segment 1 carries P1 (fields 1-71, columns 6-76) and P2 (fields 1-73,
# columns 77-149), keyed on LOGRECNO in column 5.
rd_seg1 <- function(f) {
  d <- fread(f, sep = "|", header = FALSE, quote = "", showProgress = FALSE,
             colClasses = list(character = 5))
  stopifnot(ncol(d) == 149L)
  d
}

geo <- rd_geo(file.path(npl_dir, "usgeo2020.npl"))
s1  <- rd_seg1(file.path(npl_dir, "us000012020.npl"))
setnames(s1, 5, "logrec")

P1 <- function(d, i) as.numeric(d[[5L + i]])       # P1 field i
P2 <- function(d, i) as.numeric(d[[76L + i]])      # P2 field i

# The eight-category table, from P2.
p2_table <- function(d) {
  out <- data.table(logrec = d$logrec,
                    total    = P2(d, 1),
                    hispanic = P2(d, 2),
                    nh_white = P2(d, 5),
                    nh_black = P2(d, 6),
                    nh_aian  = P2(d, 7),
                    nh_asian = P2(d, 8),
                    nh_nhpi  = P2(d, 9),
                    nh_sor   = P2(d, 10),
                    nh_two   = P2(d, 11))
  out
}
# Race alone, and race alone or in combination, from P1.  Ignores ethnicity.
p1_table <- function(d) {
  alone <- lapply(3:8, function(i) P1(d, i))
  names(alone) <- paste0(RACE6, "_alone")
  anyc <- alone
  names(anyc) <- paste0(RACE6, "_any")
  for (cf in combo_fields) {
    v <- P1(d, cf$field)
    for (r in cf$races) anyc[[r]] <- anyc[[r]] + v
  }
  as.data.table(c(list(logrec = d$logrec, p1_total = P1(d, 1),
                       one_race = P1(d, 2), two_plus = P1(d, 9)),
                  alone, anyc))
}

lvl <- function(sl) {
  g <- geo[sumlev == sl]
  m <- merge(g, p2_table(s1), by = "logrec")
  m <- merge(m, p1_table(s1), by = "logrec")
  stopifnot(nrow(m) == nrow(g))
  m
}

nation    <- lvl("010")
regions4  <- lvl("020")
divisions <- lvl("030")
states_np <- lvl("040")
metros    <- lvl("310")

stopifnot(nrow(nation) == 1L, nrow(regions4) == 4L, nrow(divisions) == 9L)
NAT_POP <- nation$total
say("  United States total population: ", format(NAT_POP, big.mark = ","))
stopifnot(NAT_POP == 331449281)                       # the published 2020 count

# The eight categories must partition the population exactly, everywhere.
chk8 <- function(d) all(rowSums(as.matrix(d[, CAT, with = FALSE])) == d$total)
stopifnot(chk8(nation), chk8(regions4), chk8(divisions), chk8(states_np))

NATP <- as.numeric(nation[, CAT, with = FALSE]) / NAT_POP        # national shares
E_US <- entropy(matrix(as.numeric(nation[, CAT, with = FALSE]), nrow = 1))
say("  national entropy E = ", sprintf("%.4f", E_US),
    "  of a maximum ", sprintf("%.4f", log(8)))

# ===========================================================================
# Section 3 -- every state file: county, tract, block group, block
# ===========================================================================
#
# 51 files, roughly 1.3 GB compressed and 15 GB expanded.  Each one is unzipped,
# reduced to the rows and columns this lab needs, and deleted before the next.
# Counties and tracts are kept whole; block groups and blocks are summarised in
# flight, because 8.1 million blocks are not going into a CSV in a course
# repository.

say("Section 3: 51 state files")

STATES <- c(al = "Alabama", ak = "Alaska", az = "Arizona", ar = "Arkansas",
  ca = "California", co = "Colorado", ct = "Connecticut", de = "Delaware",
  dc = "District_of_Columbia", fl = "Florida", ga = "Georgia", hi = "Hawaii",
  id = "Idaho", il = "Illinois", "in" = "Indiana", ia = "Iowa", ks = "Kansas",
  ky = "Kentucky", la = "Louisiana", me = "Maine", md = "Maryland",
  ma = "Massachusetts", mi = "Michigan", mn = "Minnesota", ms = "Mississippi",
  mo = "Missouri", mt = "Montana", ne = "Nebraska", nv = "Nevada",
  nh = "New_Hampshire", nj = "New_Jersey", nm = "New_Mexico", ny = "New_York",
  nc = "North_Carolina", nd = "North_Dakota", oh = "Ohio", ok = "Oklahoma",
  or = "Oregon", pa = "Pennsylvania", ri = "Rhode_Island",
  sc = "South_Carolina", sd = "South_Dakota", tn = "Tennessee", tx = "Texas",
  ut = "Utah", vt = "Vermont", va = "Virginia", wa = "Washington",
  wv = "West_Virginia", wi = "Wisconsin", wy = "Wyoming")
stopifnot(length(STATES) == 51L)

STAGE_F <- file.path(STAGE, "levels.rds")

if (!file.exists(STAGE_F)) {
  co_all <- list(); tr_all <- list(); acc <- list()
  for (ab in names(STATES)) {
    nm <- STATES[[ab]]
    z <- grab(paste0(PLBASE, nm, "/", ab, "2020.pl.zip"),
              file.path(CACHE, paste0(ab, "2020.pl.zip")))
    ex <- file.path(CACHE, paste0("x_", ab)); dir.create(ex, showWarnings = FALSE)
    unzip(z, exdir = ex)
    g  <- rd_geo(file.path(ex, paste0(ab, "geo2020.pl")))
    d1 <- rd_seg1(file.path(ex, paste0(ab, "000012020.pl")))
    setnames(d1, 5, "logrec")
    P <- merge(p2_table(d1), p1_table(d1), by = "logrec")

    take <- function(sl) {
      k <- g[sumlev == sl, .(logrec, geoid, name, region, division, st)]
      m <- merge(k, P, by = "logrec"); m$logrec <- NULL; m
    }
    co <- take("050"); tr <- take("140"); bg <- take("150"); bl <- take("750")
    stopifnot(chk8(co), chk8(tr), chk8(bg), chk8(bl))
    # A state's counties, tracts, block groups and blocks must each sum to the
    # state.  Blocks are the acid test: they are the finest published unit.
    stv <- take("040")
    stopifnot(nrow(stv) == 1L,
              sum(co$total) == stv$total, sum(tr$total) == stv$total,
              sum(bg$total) == stv$total, sum(bl$total) == stv$total)

    co_all[[ab]] <- co
    tr_all[[ab]] <- tr[, c("geoid", "st", CAT), with = FALSE][, total := rowSums(.SD), .SDcols = CAT]
    # For block groups and blocks only the pieces of the ladder are kept.
    summ <- function(d) {
      M <- as.matrix(d[, CAT, with = FALSE]); tot <- d$total
      e <- entropy(M); e0 <- null_entropy(tot, NATP)
      keep <- tot > 0
      list(n = nrow(d), n_pop = sum(keep), pop = sum(tot),
           se = sum(e[keep] * tot[keep]), se0 = sum(e0[keep] * tot[keep]),
           tot_kept = sum(tot[keep]), med = median(tot),
           n_single = sum(keep & apply(M, 1, max) == tot))
    }
    acc[[ab]] <- list(bg = summ(bg), bl = summ(bl))
    say(sprintf("  %-22s %7s counties %6s tracts %8s blocks  %12s people",
                nm, nrow(co), nrow(tr), nrow(bl), format(stv$total, big.mark = ",")))
    unlink(ex, recursive = TRUE)
    rm(g, d1, P, co, tr, bg, bl); gc(FALSE)
  }
  counties <- rbindlist(co_all); tracts <- rbindlist(tr_all)
  saveRDS(list(counties = counties, tracts = tracts, acc = acc), STAGE_F)
}
ST <- readRDS(STAGE_F)
counties <- ST$counties; tracts <- ST$tracts; acc <- ST$acc

stopifnot(sum(counties$total) == NAT_POP, sum(tracts$total) == NAT_POP)
say("  counties: ", nrow(counties), "   tracts: ", nrow(tracts))

# ===========================================================================
# Section 4 -- the ladder
# ===========================================================================

say("Section 4: the ladder")

rung <- function(label, d, note = "") {
  M <- as.matrix(d[, CAT, with = FALSE]); tot <- rowSums(M)
  e  <- entropy(M)
  e0 <- null_entropy(tot, NATP)
  keep <- tot > 0
  data.table(level = label, units = nrow(d), units_with_people = sum(keep),
             median_pop = as.numeric(median(tot)),
             ebar  = wmean(e[keep],  tot[keep]),
             ebar0 = wmean(e0[keep], tot[keep]),
             pct_single_group = 100 * sum(keep & apply(M, 1, max) == tot) /
                                sum(keep),
             note = note)
}
from_acc <- function(label, key, note) {
  a <- lapply(acc, `[[`, key)
  data.table(level = label,
             units = sum(vapply(a, `[[`, 0, "n")),
             units_with_people = sum(vapply(a, `[[`, 0, "n_pop")),
             median_pop = NA_real_,
             ebar  = sum(vapply(a, `[[`, 0, "se"))  / sum(vapply(a, `[[`, 0, "tot_kept")),
             ebar0 = sum(vapply(a, `[[`, 0, "se0")) / sum(vapply(a, `[[`, 0, "tot_kept")),
             pct_single_group = 100 * sum(vapply(a, `[[`, 0, "n_single")) /
                                sum(vapply(a, `[[`, 0, "n_pop")),
             note = note)
}

ladder <- rbindlist(list(
  rung("Nation",      nation,    "1 unit, by construction H = 0"),
  rung("Region",      regions4,  "Northeast, Midwest, South, West"),
  rung("Division",    divisions, "the Bureau's nine divisions"),
  rung("State",       states_np[st != "72"], "50 states and DC"),
  rung("County",      counties,  "counties and county equivalents"),
  rung("Census tract", tracts, "roughly 4,000 people"),
  from_acc("Block group", "bg", "roughly 1,200 people"),
  from_acc("Census block","bl", "the finest unit published; DP noise is worst here")))
ladder[, H     := (E_US - ebar)  / E_US]
ladder[, H0    := (E_US - ebar0) / E_US]
ladder[, H_net := H - H0]
ladder[, diversity := 100 * ebar / log(8)]
ladder[, median_pop := ifelse(is.na(median_pop), NA, median_pop)]
print(ladder[, .(level, units, ebar = round(ebar, 4), H = round(H, 4),
                 H0 = round(H0, 4), H_net = round(H_net, 4))])

# ===========================================================================
# Section 5 -- the zoom: Wayne County, Michigan
# ===========================================================================
#
# WHY WAYNE.  It is the county the national picture is least able to describe.
# Taken whole it looks like a mixed place -- no group is a majority.  Taken
# tract by tract it is two counties, and the line between them is a street.
# It also contains Dearborn, the largest Arab-American concentration in the
# country, whose residents the 2020 form had no category for and counted as
# White; the 2024 revision of Directive 15 adds one.  The definitional argument
# and the geographic one land on the same ground.
#
# Fulton County, Georgia is the worked example in `areal-units` and Houston
# County, Georgia in `census-geography` and `precinct-geography`; Cleveland and
# Philadelphia are `redlining`'s.  Wayne is none of them.

say("Section 5: Wayne County, Michigan")

WAYNE <- "26163"
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

LAB <- normalizePath("../../..", mustWork = TRUE)  # .../book/labs
mi_tract_shp <- file.path(LAB, "05-ordinary-people", "redlining",
                          "data", "raw", "tiger", "26", "tl_2020_26_tract.shp")
us_state_shp <- file.path(LAB, "03-elections", "residual-votes",
                          "data", "raw", "tiger", "st", "tl_2020_us_state.shp")
stopifnot(file.exists(mi_tract_shp), file.exists(us_state_shp))

unz <- function(url, zipname, layer) {
  z <- grab(url, file.path(CACHE, zipname))
  ex <- file.path(CACHE, sub("\\.zip$", "", zipname))
  if (!dir.exists(ex)) { dir.create(ex); unzip(z, exdir = ex) }
  st_read(file.path(ex, layer), quiet = TRUE)
}
mi_bg  <- unz(paste0(TIGER, "BG/tl_2020_26_bg.zip"),
              "tl_2020_26_bg.zip", "tl_2020_26_bg.shp")
mi_blk <- unz(paste0(TIGER, "TABBLOCK20/tl_2020_26_tabblock20.zip"),
              "tl_2020_26_tabblock20.zip", "tl_2020_26_tabblock20.shp")
mi_tr  <- st_read(mi_tract_shp, quiet = TRUE)

# Michigan's own counts, re-read here because the staged tract table does not
# carry block groups or blocks.
mi_z <- grab(paste0(PLBASE, "Michigan/mi2020.pl.zip"),
             file.path(CACHE, "mi2020.pl.zip"))
mi_ex <- file.path(CACHE, "x_mi_zoom"); dir.create(mi_ex, showWarnings = FALSE)
unzip(mi_z, exdir = mi_ex)
mg  <- rd_geo(file.path(mi_ex, "migeo2020.pl"))
md1 <- rd_seg1(file.path(mi_ex, "mi000012020.pl")); setnames(md1, 5, "logrec")
mP  <- merge(p2_table(md1), p1_table(md1), by = "logrec")
mtake <- function(sl) {
  k <- mg[sumlev == sl, .(logrec, geoid, name)]
  m <- merge(k, mP, by = "logrec"); m$logrec <- NULL; m
}
TRI <- c(WAYNE, "26125", "26099")            # Wayne, Oakland, Macomb
w_co   <- mtake("050")[geoid == WAYNE]
tri_co <- mtake("050")[geoid %in% TRI]
w_tr   <- mtake("140")[substr(geoid, 1, 5) == WAYNE]
w_bg   <- mtake("150")[substr(geoid, 1, 5) == WAYNE]
bl_tri <- mtake("750")[substr(geoid, 1, 5) %in% TRI]
w_bl   <- bl_tri[substr(geoid, 1, 5) == WAYNE]
stopifnot(nrow(w_co) == 1L, nrow(tri_co) == 3L,
          sum(w_tr$total) == w_co$total, sum(w_bg$total) == w_co$total,
          sum(w_bl$total) == w_co$total)
say("  Wayne County: ", format(w_co$total, big.mark = ","), " people, ",
    nrow(w_tr), " tracts, ", nrow(w_bg), " block groups, ",
    nrow(w_bl), " blocks")
unlink(mi_ex, recursive = TRUE)

# ---- geometry, projected and expressed in kilometres from a local origin ----
# EPSG:3078 (NAD83 / Michigan Oblique Mercator) is metres and is the state's
# own plane.  Coordinates become kilometres relative to the centre of Wayne
# County, so the figure files stay small and both renderers share one
# coordinate system.
PRJ <- 3078
rings <- function(g, idcol, digits = 3) {
  gg <- st_cast(st_geometry(g), "MULTIPOLYGON")
  out <- vector("list", length(gg))
  for (i in seq_along(gg)) {
    ps <- gg[[i]]; part <- 0L; rr <- list()
    for (poly in ps) {
      part <- part + 1L
      m <- poly[[1]]                       # outer ring; holes are not drawn
      rr[[part]] <- data.table(id = as.character(g[[idcol]][i]), part = part,
                               x = round(m[, 1], digits),
                               y = round(m[, 2], digits))
    }
    out[[i]] <- rbindlist(rr)
  }
  rbindlist(out)
}

w_tr_g <- st_transform(mi_tr[mi_tr$COUNTYFP == "163", ], PRJ)
ctr <- st_coordinates(st_centroid(st_union(st_geometry(w_tr_g))))
OX <- ctr[1]; OY <- ctr[2]
to_km <- function(g) {
  g <- st_transform(g, PRJ)
  gg <- st_geometry(g); st_crs(gg) <- NA
  st_geometry(g) <- (gg - c(OX, OY)) / 1000
  g
}
w_tr_g <- to_km(mi_tr[mi_tr$COUNTYFP == "163", ])
w_bg_g <- to_km(mi_bg[mi_bg$COUNTYFP == "163", ])

# The tract outlines are simplified: 610 tracts at full TIGER resolution is
# several megabytes of coordinates, and this figure is about which side of a
# line a tract falls on, not about the shape of a river bank.  25 m tolerance.
w_tr_rings <- rings(st_simplify(w_tr_g, dTolerance = 0.025,
                                preserveTopology = TRUE), "GEOID")
w_bg_rings <- rings(st_simplify(w_bg_g, dTolerance = 0.030,
                                preserveTopology = TRUE), "GEOID")
say("  tract rings: ", nrow(w_tr_rings), " vertices; block-group rings: ",
    nrow(w_bg_rings))

cty <- st_as_sf(st_sfc(st_union(st_geometry(w_tr_g))))
cty$id <- WAYNE
county_outline <- rings(st_simplify(cty, dTolerance = 0.060,
                                    preserveTopology = TRUE), "id")

# ---- the transect: blocks crossing the northern line of the City of Detroit -
# Eight Mile Road is the northern boundary of the City of Detroit.  It is also,
# along that stretch, the Wayne / Oakland and Wayne / Macomb COUNTY line -- so
# no county-level map of Michigan can show it at all, which is the point of the
# last figure.  The corridor therefore has to reach into two more counties.
#
# The window is defined by GEOGRAPHY, not by outcome: take the City of
# Detroit's own boundary as published by the Bureau, keep the northern edge,
# and take every census block whose interior point lies within 2.5 km of that
# edge across a 12 km stretch of it.  Nothing about who lives there enters the
# selection.
mi_pl <- unz(paste0(TIGER, "PLACE/tl_2020_26_place.zip"),
             "tl_2020_26_place.zip", "tl_2020_26_place.shp")
det <- mi_pl[mi_pl$NAME == "Detroit" & mi_pl$LSAD == "25", ]
stopifnot(nrow(det) == 1L)
det_km <- to_km(det)
det_bb <- st_bbox(det_km)

# The northern edge, as the UPPER ENVELOPE of the city boundary.  Eight Mile
# Road runs east-west on the ground, but Michigan's own map projection rotates
# it a little, so the line is not at a constant y and cannot be found by taking
# the maximum.  Instead: bin the boundary vertices by x, take the highest
# vertex in each bin, and keep that polyline.  Distance from the city line is
# then a vertical offset from the polyline, obtained by interpolation.
bl_line <- st_cast(st_geometry(det_km), "MULTILINESTRING")
edge_pts <- as.data.table(st_coordinates(bl_line))[, .(X, Y)]
edge_pts[, xb := round(X / 0.25) * 0.25]
env <- edge_pts[, .(Y = max(Y)), by = xb][order(xb)]
# the envelope is a straight road; a light monotone smoothing removes the few
# bins where the city boundary steps north around a parcel
env[, Y := stats::runmed(Y, 9, endrule = "median")]
XR <- range(env$xb)
line_y <- function(x) approx(env$xb, env$Y, xout = x, rule = 2)$y
XMID <- mean(XR)
WIN <- list(x0 = XMID - 6, x1 = XMID + 6)
say(sprintf("  Detroit's north edge runs x %.1f..%.1f km, y %.2f..%.2f;",
            XR[1], XR[2], min(env$Y), max(env$Y)))
say(sprintf("  map window: x %.1f..%.1f km", WIN$x0, WIN$x1))

# Blocks: use the Bureau's own interior point (INTPTLAT20/INTPTLON20) rather
# than a computed centroid, the same convention `areal-units` uses.
blk <- mi_blk[mi_blk$COUNTYFP20 %in% c("163", "125", "099"), ]
ip <- st_as_sf(data.frame(
        lon = as.numeric(blk$INTPTLON20), lat = as.numeric(blk$INTPTLAT20),
        GEOID = as.character(blk$GEOID20)),
      coords = c("lon", "lat"), crs = 4269)
ip <- st_transform(ip, PRJ)
xy <- st_coordinates(ip)
ipd <- data.table(GEOID = ip$GEOID,
                  cx = (xy[, 1] - OX) / 1000, cy = (xy[, 2] - OY) / 1000)
ipd[, dist := cy - line_y(cx)]                 # signed km north of the city line
sel <- ipd[cx >= WIN$x0 & cx <= WIN$x1 & abs(dist) <= 2.5]

tw <- merge(sel, bl_tri[, c("geoid", "total", CAT), with = FALSE],
            by.x = "GEOID", by.y = "geoid")
tw[, county := substr(GEOID, 1, 5)]
tw[, side := ifelse(dist >= 0, "north", "south")]
tw[, `:=`(cx = round(cx, 3), cy = round(cy, 3), dist = round(dist, 3))]
say("  transect window: ", nrow(tw), " blocks, ",
    format(sum(tw$total), big.mark = ","), " people, ",
    length(unique(tw$county)), " counties")

# the drawn blocks, simplified only enough to keep the file sane
blk_win <- to_km(blk[as.character(blk$GEOID20) %in% tw$GEOID, ])
blk_win$id <- as.character(blk_win$GEOID20)
blk_rings <- rings(st_simplify(blk_win, dTolerance = 0.008,
                               preserveTopology = TRUE), "id")
det_sf <- st_as_sf(st_sfc(st_geometry(det_km)[[1]])); det_sf$id <- "detroit"
det_rings <- rings(st_simplify(det_sf, dTolerance = 0.020,
                               preserveTopology = TRUE), "id")
say("  block rings: ", nrow(blk_rings), " vertices")

# ---- the profile, computed along the WHOLE northern line, not the window ---
# The map above is a 12 km window, because a map has to be legible.  The
# profile is a statistic, so it uses every block within 2.5 km of any part of
# Detroit's northern boundary -- the full width of the city -- and reports
# composition by distance from that line.  Two different jobs, two different
# extents, and the document says which is which.
near <- ipd[cx >= XR[1] & cx <= XR[2] & abs(dist) <= 2.5]
prof <- merge(near, bl_tri[, c("geoid", "total", CAT), with = FALSE],
              by.x = "GEOID", by.y = "geoid")
BW <- 0.25
prof[, band := pmin(pmax(floor(dist / BW), -10L), 9L)]
profile <- prof[, c(list(blocks = .N, total = sum(total)),
                    lapply(.SD, sum)), by = band, .SDcols = CAT][order(band)]
profile[, mid := (band + 0.5) * BW]
fwrite(dd_signif(as.data.frame(profile)), "derived/eightmile_profile.csv")
say("  profile: ", nrow(prof), " blocks along the whole line, ",
    format(sum(prof$total), big.mark = ","), " people")

# ===========================================================================
# Section 6 -- definitional change, and the one comparison across decades
# ===========================================================================
#
# THE STANDARD IS NOT THE CENSUS BUREAU'S.  The categories on the form are set
# by the Office of Management and Budget in Statistical Policy Directive No.
# 15, and the Bureau implements them.  The directive has been revised twice.
# The dates, citations and quoted provisions below are NOT COMPUTED -- they are
# transcribed from the Federal Register text on govinfo.gov and from OMB's own
# SPD 15 pages, and they are recorded in the build so that the documents quote
# a file rather than a memory.

standards <- data.table(read.csv(text = '
key,issued,citation,title,categories,question_form,first_census,detail
"1977","1977-05-12","Statistical Policy Directive No. 15","Race and Ethnic Standards for Federal Statistics and Administrative Reporting",5,"separate collection preferred, not required","1980","One mark only. A person of mixed origins was to be reported in the category which most closely reflects the individuals recognition in his community."
"1997","1997-10-30","62 FR 58782","Revisions to the Standards for the Classification of Federal Data on Race and Ethnicity",6,"two separate questions preferred","2000","Respondents shall be offered the option of selecting one or more racial designations. Asian or Pacific Islander split into two categories."
"2024","2024-03-29","89 FR 22182","Revisions to OMBs Statistical Policy Directive No. 15: Standards for Maintaining, Collecting, and Presenting Federal Data on Race and Ethnicity",7,"a single combined question required","2030 (planned)","Middle Eastern or North African added as a minimum category, separate from White. Effective 2024-03-28; agency action plans due 2027-03-28 after two extensions; full compliance 2029-09-29."
', stringsAsFactors = FALSE))
# The 2024 full-compliance date is 2029-09-28; stored above as text only.
standards$detail <- sub("2029-09-29", "2029-09-28", standards$detail)

# Which category a respondent could actually be counted in, by census year.
# 1 = offered; 0 = not offered.  2030 is the Bureau's stated plan, not a fact.
categories <- data.table(read.csv(text = '
category,dimension,c1980,c1990,c2000,c2010,c2020,c2030,note
"White",race,1,1,1,1,1,1,"The 1977 standard defined White as origins in any of the original peoples of Europe, North Africa, or the Middle East."
"Black or African American",race,1,1,1,1,1,1,"wording of the label has changed repeatedly; the category has not"
"American Indian or Alaska Native",race,1,1,1,1,1,1,"with a write-in for enrolled or principal tribe"
"Asian or Pacific Islander",race,1,1,0,0,0,0,"one category under the 1977 standard"
"Asian",race,0,0,1,1,1,1,"split out by the 1997 revision"
"Native Hawaiian or Other Pacific Islander",race,0,0,1,1,1,0,"split out by the 1997 revision"
"Native Hawaiian or Pacific Islander",race,0,0,0,0,0,1,"the 2024 revision drops the word Other"
"Some Other Race",race,1,1,1,1,1,0,"never an OMB minimum category; Congress required the Bureau to offer it"
"Two or More Races",race,0,0,1,1,1,1,"arithmetically impossible before 1997 required mark one or more"
"Middle Eastern or North African",race,0,0,0,0,0,1,"added by the 2024 revision; counted as White from 1977 until then"
"Hispanic or Latino",ethnicity,1,1,1,1,1,0,"asked as a separate question, so a Hispanic respondent also answers race"
"Hispanic or Latino",combined,0,0,0,0,0,1,"the 2024 revision moves it into the single combined question"
', stringsAsFactors = FALSE))

# ---- the one cross-decade comparison this data will support ---------------
#
# 2010 and 2020 are the only two decades this lab compares, and they are the
# EASY pair: both ran under the same 1997 standard, with the same categories,
# in the same file format, with the same field layout.  Everything that could
# be held constant was held constant.
#
# The comparison still does not mean what it looks like, and the Census Bureau
# says so itself.  Between the two censuses the Bureau added dedicated write-in
# areas under White and under Black, raised the characters captured per write-in
# from 30 to 200, and coded up to six detailed codes per write-in instead of
# prioritising into two.  Of the resulting jump in Some Other Race the Bureau
# wrote that the changes "could be attributed to a number of factors, including
# demographic change since 2010. But we expect they were largely due to the
# improvements to the design of the two separate questions for race and
# ethnicity, data processing and coding."
#   -- Jones, Marks, Ramirez & Rios-Vargas, "2020 Census Illuminates Racial and
#      Ethnic Composition of the Country", U.S. Census Bureau, 2021-08-12.
#
# So the numbers are computed here, honestly, and the document draws them with
# the discontinuity marked on the figure rather than buried in a caption.

say("Section 6: 2010, for the comparison that does not work")

n10_zip <- grab(paste0("https://www2.census.gov/census_2010/",
                       "redistricting_file--pl_94-171/National/us2010.npl.zip"),
                file.path(CACHE, "us2010.npl.zip"))
n10_dir <- file.path(CACHE, "npl10"); dir.create(n10_dir, showWarnings = FALSE)
unzip(n10_zip, exdir = n10_dir)

# 2010 differs from 2020 in exactly one way that matters here: the geographic
# header is FIXED WIDTH rather than pipe-delimited (SUMLEV at characters 9-11,
# LOGRECNO at 19-25), and the data segments are comma- rather than
# pipe-delimited.  The P1 and P2 field layouts are identical -- 71 and 73
# fields, in the same order -- which is what makes the comparison mechanically
# possible and definitionally treacherous.
g10 <- readLines(file.path(n10_dir, "usgeo2010.npl"), encoding = "latin1")
lr10 <- substr(g10, 19, 25)[substr(g10, 9, 11) == "010"]
d10 <- as.data.table(read.csv(file.path(n10_dir, "us000012010.npl"),
                              header = FALSE, colClasses = list(V5 = "character")))
stopifnot(ncol(d10) == 149L)
setnames(d10, 5, "logrec")
r10 <- d10[logrec %in% lr10]
stopifnot(nrow(r10) == 1L)
n10 <- merge(p2_table(r10), p1_table(r10), by = "logrec")
stopifnot(n10$total == 308745538)                   # the published 2010 count
say("  2010 total population: ", format(n10$total, big.mark = ","))

decades <- rbindlist(list(
  cbind(year = 2010L, n10[, c("total", CAT, paste0(RACE6, "_alone"),
                              paste0(RACE6, "_any"), "one_race", "two_plus"),
                          with = FALSE]),
  cbind(year = 2020L, nation[, c("total", CAT, paste0(RACE6, "_alone"),
                                 paste0(RACE6, "_any"), "one_race", "two_plus"),
                             with = FALSE])))
fwrite(dd_signif(as.data.frame(decades)), "derived/decades.csv")
fwrite(dd_signif(as.data.frame(standards)), "derived/standards.csv")

# ===========================================================================
# Section 7 -- write everything
# ===========================================================================

say("Section 7: writing")

pick <- function(d, extra = character(0))
  d[, c("geoid", "name", extra, "total", CAT,
        paste0(RACE6, "_alone"), paste0(RACE6, "_any"),
        "one_race", "two_plus"), with = FALSE]

nation_out <- pick(nation)
nation_out[, entropy := E_US]
fwrite(dd_signif(as.data.frame(nation_out)), "derived/nation.csv")

reg_out <- rbind(
  cbind(kind = "region",   pick(regions4)),
  cbind(kind = "division", pick(divisions)))
reg_out[, entropy := entropy(as.matrix(reg_out[, CAT, with = FALSE]))]
fwrite(dd_signif(as.data.frame(reg_out)), "derived/regions.csv")

st_out <- states_np[st != "72"]
st_out <- cbind(pick(st_out, c("region", "division", "st")))
st_out[, entropy := entropy(as.matrix(st_out[, CAT, with = FALSE]))]
st_out[, largest := CATSHORT[CAT[max.col(as.matrix(.SD))]], .SDcols = CAT]
setorder(st_out, name)
fwrite(dd_signif(as.data.frame(st_out)), "derived/states.csv")

co_out <- pick(counties, c("st"))
co_out[, entropy := entropy(as.matrix(co_out[, CAT, with = FALSE]))]
co_out[, largest := CATSHORT[CAT[max.col(as.matrix(.SD))]], .SDcols = CAT]
# County names arrive in Latin-1, same as the metro names below. Exactly one
# of the 3,143 carries an accent -- Dona Ana County, New Mexico -- and without
# this the written file is not valid UTF-8, which nothing notices until a tool
# that cares tries to read it.
co_out[, name := iconv(name, "latin1", "UTF-8")]
fwrite(dd_signif(as.data.frame(co_out)), "derived/counties.csv")

fwrite(dd_signif(as.data.frame(ladder)), "derived/ladder.csv")

# The tract distribution, binned, so the density figure does not ship 85,528
# rows.  Two things are binned: the entropy of a tract, and -- because a single
# number hides which way a tract is unmixed -- the share of the tract in its own
# largest group.
TM <- as.matrix(tracts[, CAT, with = FALSE]); Ttot <- rowSums(TM)
te <- entropy(TM); tmax <- 100 * apply(TM, 1, max) / ifelse(Ttot == 0, 1, Ttot)
te0 <- null_entropy(Ttot, NATP)
keep <- Ttot > 0
bins <- seq(0, log(8), length.out = 61)
h  <- hist(te[keep],  breaks = bins, plot = FALSE)
h0 <- hist(te0[keep], breaks = bins, plot = FALSE)
hw <- hist(te[keep],  breaks = bins, plot = FALSE)   # population-weighted below
wsum <- tapply(Ttot[keep], cut(te[keep], bins, include.lowest = TRUE), sum)
wsum0 <- tapply(Ttot[keep], cut(te0[keep], bins, include.lowest = TRUE), sum)
tract_dist <- data.table(
  mid = h$mids, tracts = h$counts, tracts_null = h0$counts,
  people = as.numeric(ifelse(is.na(wsum), 0, wsum)),
  people_null = as.numeric(ifelse(is.na(wsum0), 0, wsum0)))
fwrite(dd_signif(as.data.frame(tract_dist)), "derived/tract_dist.csv")

# largest-group share, in ten-point bins, weighted by people
lg <- data.table(bin = cut(tmax[keep], breaks = seq(10, 100, 10),
                           include.lowest = TRUE),
                 pop = Ttot[keep])[, .(people = sum(pop)), by = bin][order(bin)]
fwrite(dd_signif(as.data.frame(lg)), "derived/tract_largest.csv")

# Metro names arrive in Latin-1 and some carry accents; convert once, and drop
# the Puerto Rico areas, which are outside the national total this lab uses.
metros[, name := iconv(name, "latin1", "UTF-8")]
mt <- metros[!grepl(", PR$", name)][order(-total)][1:50]
fwrite(dd_signif(as.data.frame(cbind(pick(mt), entropy = entropy(as.matrix(mt[, CAT, with = FALSE]))))), "derived/metros.csv")

fwrite(dd_signif(as.data.frame(categories)), "derived/categories.csv")

# ---- the zoom files -------------------------------------------------------
w_tr_out <- w_tr[, c("geoid", "total", CAT), with = FALSE]
w_tr_out[, entropy := entropy(as.matrix(.SD)), .SDcols = CAT]
w_tr_out[, largest := CATSHORT[CAT[max.col(as.matrix(.SD))]], .SDcols = CAT]
w_tr_out[, largest_pct := 100 * apply(as.matrix(.SD), 1, max) /
           ifelse(total == 0, 1, total), .SDcols = CAT]
w_bg_out <- w_bg[, c("geoid", "total", CAT), with = FALSE]
w_bg_out[, largest := CATSHORT[CAT[max.col(as.matrix(.SD))]], .SDcols = CAT]
w_bg_out[, largest_pct := 100 * apply(as.matrix(.SD), 1, max) /
           ifelse(total == 0, 1, total), .SDcols = CAT]
fwrite(dd_signif(as.data.frame(w_tr_out)), "derived/wayne_tracts.csv")
fwrite(dd_signif(as.data.frame(w_tr_rings)), "derived/wayne_tract_rings.csv")
fwrite(dd_signif(as.data.frame(w_bg_out)), "derived/wayne_bg.csv")
fwrite(dd_signif(as.data.frame(w_bg_rings)), "derived/wayne_bg_rings.csv")
fwrite(dd_signif(as.data.frame(county_outline)), "derived/wayne_outline.csv")
fwrite(dd_signif(as.data.frame(tw)), "derived/wayne_transect.csv")
fwrite(dd_signif(as.data.frame(blk_rings)), "derived/wayne_block_rings.csv")
fwrite(dd_signif(as.data.frame(det_rings)), "derived/wayne_detroit_rings.csv")

# ---- the national small-multiple map --------------------------------------
# Albers Equal Area (EPSG:5070), the standard projection for a map of the
# United States, with Alaska and Hawaii moved and scaled the conventional way
# so all 51 units appear.  Heavily simplified: these are 51 thumbnails.
us <- st_read(us_state_shp, quiet = TRUE)
us <- us[us$STATEFP %in% st_out$st, ]
place <- function(g, fp, scale, dx, dy) {
  i <- which(g$STATEFP == fp)
  if (!length(i)) return(g)
  gg <- st_geometry(g)[i]
  ctr <- st_centroid(st_union(gg))
  st_geometry(g)[i] <- (gg - ctr) * scale + ctr + c(dx, dy)
  g
}
us <- st_transform(us, 5070)
us <- place(us, "02", 0.36, 1.30e6, -4.55e6)     # Alaska
us <- place(us, "15", 1.30, 4.10e6, -1.35e6)     # Hawaii
us <- st_simplify(us, dTolerance = 9000, preserveTopology = TRUE)
us$km_x <- 1
us_rings <- rings(us, "STATEFP", digits = 1)
setnames(us_rings, "id", "st")
us_rings[, `:=`(x = round(x / 1000, 1), y = round(y / 1000, 1))]
fwrite(dd_signif(as.data.frame(us_rings)), "derived/us_rings.csv")
say("  state rings: ", nrow(us_rings), " vertices")

# The regions-and-divisions map wants two things the rings alone cannot give:
# the nine division outlines drawn as their own heavier lines, and a label
# anchor that falls inside every state. Both come from the same `us` object,
# so they trace the identical shapes at the identical coordinates. The
# division outline is the border of the union of its states; st_simplify
# above preserves topology, so shared borders share vertices and the union
# closes without slivers. Region and division names are the file's own 020
# and 030 rows, not a hand-typed list.
reg_name <- setNames(regions4$name, regions4$geoid)
div_name <- setNames(divisions$name, divisions$geoid)
i <- match(us$STATEFP, st_out$st)
us$region   <- unname(reg_name[as.character(st_out$region[i])])
us$division <- unname(div_name[as.character(st_out$division[i])])
stopifnot(!anyNA(us$region), !anyNA(us$division))

div_rings <- rbindlist(lapply(sort(unique(us$division)), function(k) {
  u <- st_union(st_geometry(us)[us$division == k])
  m <- st_coordinates(st_cast(u, "MULTILINESTRING"))
  data.table(division = k, part = as.integer(m[, "L1"]),
             x = round(m[, 1] / 1000, 1), y = round(m[, 2] / 1000, 1))
}))
stopifnot(length(unique(div_rings$division)) == 9L)
fwrite(dd_signif(as.data.frame(div_rings)), "derived/us_divisions.csv")

pos <- suppressWarnings(st_point_on_surface(st_geometry(us)))
pm <- st_coordinates(pos)
map_lab <- data.table(kind = "state", name = us$NAME, abbr = us$STUSPS,
                      region = us$region, division = us$division,
                      x = round(pm[, 1] / 1000, 1),
                      y = round(pm[, 2] / 1000, 1),
                      area = round(as.numeric(st_area(us)) / 1e6))
reg_lab <- rbindlist(lapply(unique(us$region), function(r) {
  u <- suppressWarnings(st_point_on_surface(
    st_union(st_geometry(us)[us$region == r & !us$STUSPS %in% c("AK", "HI")])))
  m <- st_coordinates(u)
  data.table(kind = "region", name = r, abbr = "", region = r, division = "",
             x = round(m[1] / 1000, 1), y = round(m[2] / 1000, 1), area = NA)
}))
map_lab <- rbind(map_lab, reg_lab)
fwrite(dd_signif(as.data.frame(map_lab)), "derived/us_maplabels.csv")
say("  division outlines: ", nrow(div_rings), " vertices, ",
    nrow(map_lab), " label anchors")

# ===========================================================================
# Section 8 -- the facts every document quotes
# ===========================================================================

F <- list()
add <- function(name, value, note = "") F[[length(F) + 1L]] <<-
  data.table(name = name, value = as.character(value), note = note)

NV <- function(k) as.numeric(nation_out[[k]])
add("fetched", FETCHED)
add("us_pop", NAT_POP, "2020 Census, 50 states and DC")
for (k in CAT) {
  add(paste0("us_", k), NV(k))
  add(paste0("us_", k, "_pct"), sprintf("%.2f", 100 * NV(k) / NAT_POP))
}
for (r in RACE6) {
  add(paste0("us_", r, "_alone"), NV(paste0(r, "_alone")))
  add(paste0("us_", r, "_any"),   NV(paste0(r, "_any")))
  add(paste0("us_", r, "_alone_pct"),
      sprintf("%.2f", 100 * NV(paste0(r, "_alone")) / NAT_POP))
  add(paste0("us_", r, "_any_pct"),
      sprintf("%.2f", 100 * NV(paste0(r, "_any")) / NAT_POP))
  add(paste0("us_", r, "_gap"),
      NV(paste0(r, "_any")) - NV(paste0(r, "_alone")))
  add(paste0("us_", r, "_ratio"),
      sprintf("%.2f", NV(paste0(r, "_any")) / NV(paste0(r, "_alone"))))
}
add("us_two_plus", NV("two_plus"))
add("us_two_plus_pct", sprintf("%.2f", 100 * NV("two_plus") / NAT_POP))
add("E_us", sprintf("%.4f", E_US))
add("E_max", sprintf("%.4f", log(8)))
add("n_categories", 8L, "mutually exclusive categories in Table P2")

LD <- function(l, cc) ladder[[cc]][ladder$level == l]
for (l in ladder$level) {
  key <- tolower(gsub("[^a-z]", "", tolower(l)))
  add(paste0("H_", key), sprintf("%.4f", LD(l, "H")))
  add(paste0("H0_", key), sprintf("%.4f", LD(l, "H0")))
  add(paste0("div_", key), sprintf("%.1f", LD(l, "diversity")))
  add(paste0("units_", key), LD(l, "units"))
}
# How lopsided is the tract the average American lives in?  The share of people
# whose own tract is at least X per cent one category.
lgp <- lg$people / sum(lg$people)
names(lgp) <- as.character(lg$bin)
add("pop_tract_80plus", sprintf("%.1f", 100 * sum(lgp[c("(80,90]", "(90,100]")])))
add("pop_tract_70plus", sprintf("%.1f", 100 * sum(lgp[c("(70,80]", "(80,90]",
                                                        "(90,100]")])))
add("pop_tract_90plus", sprintf("%.1f", 100 * lgp[["(90,100]"]]))
add("pop_tract_under50", sprintf("%.1f", 100 * sum(lgp[c("[10,20]", "(20,30]",
                                                         "(30,40]", "(40,50]")])))
add("n_counties", nrow(co_out))
add("n_tracts", nrow(tracts))
add("n_blockgroups", ladder$units[ladder$level == "Block group"])
add("n_blocks", ladder$units[ladder$level == "Census block"])

# Wayne
WC <- function(k) as.numeric(w_co[[k]])
add("wayne_pop", WC("total"))
add("wayne_tracts", nrow(w_tr))
add("wayne_blocks", nrow(w_bl))
for (k in CAT) add(paste0("wayne_", k, "_pct"),
                   sprintf("%.1f", 100 * WC(k) / WC("total")))
add("wayne_largest", CATSHORT[[CAT[which.max(as.numeric(w_co[, CAT, with = FALSE]))]]])
add("wayne_largest_pct",
    sprintf("%.1f", 100 * max(as.numeric(w_co[, CAT, with = FALSE])) / WC("total")))
WE <- entropy(matrix(as.numeric(w_co[, CAT, with = FALSE]), nrow = 1))
add("wayne_entropy", sprintf("%.4f", WE))
add("wayne_div", sprintf("%.1f", 100 * WE / log(8)))
wte <- w_tr_out$entropy; wtt <- w_tr_out$total
add("wayne_tract_ebar", sprintf("%.4f", wmean(wte[wtt > 0], wtt[wtt > 0])))
add("wayne_H_tract",
    sprintf("%.4f", (WE - wmean(wte[wtt > 0], wtt[wtt > 0])) / WE))
add("wayne_tracts_80_black",
    sum(w_tr_out$largest == "Black" & w_tr_out$largest_pct >= 80))
add("wayne_tracts_80_white",
    sum(w_tr_out$largest == "White" & w_tr_out$largest_pct >= 80))
add("wayne_pop_80",
    sum(w_tr_out$total[w_tr_out$largest_pct >= 80]))
add("wayne_pop_80_pct",
    sprintf("%.1f", 100 * sum(w_tr_out$total[w_tr_out$largest_pct >= 80]) /
              sum(w_tr_out$total)))
add("wayne_tracts_black_largest", sum(w_tr_out$largest == "Black"))
add("wayne_tracts_white_largest", sum(w_tr_out$largest == "White"))
add("wayne_tracts_pop", sum(w_tr_out$total > 0))
add("wayne_tract_median_largest",
    sprintf("%.1f", median(w_tr_out$largest_pct[w_tr_out$total > 0])))
add("wayne_tracts_90", sum(w_tr_out$largest_pct >= 90 & w_tr_out$total > 0))
add("wayne_pop_90_pct",
    sprintf("%.1f", 100 * sum(w_tr_out$total[w_tr_out$largest_pct >= 90]) /
              sum(w_tr_out$total)))
add("wayne_bg", nrow(w_bg))
add("transect_blocks", nrow(tw))
add("transect_pop", sum(tw$total))
add("transect_counties", length(unique(tw$county)))
add("profile_blocks", nrow(prof))
add("profile_pop", sum(prof$total))
pf <- function(lo, hi, k) {
  z <- profile[mid > lo & mid < hi]
  sprintf("%.1f", 100 * sum(z[[k]]) / sum(z$total))
}
add("prof_south_black", pf(-2.5, 0, "nh_black"))
add("prof_south_white", pf(-2.5, 0, "nh_white"))
add("prof_north_black", pf(0, 2.5, "nh_black"))
add("prof_north_white", pf(0, 2.5, "nh_white"))
add("prof_in500_black", pf(-0.5, 0, "nh_black"))
add("prof_out500_black", pf(0, 0.5, "nh_black"))
add("prof_jump_black", sprintf("%.1f", as.numeric(pf(-0.5, 0, "nh_black")) -
                                       as.numeric(pf(0, 0.5, "nh_black"))))

# ---- the cross-decade numbers, which the chapter draws with a break in it --
DC <- function(y, k) as.numeric(decades[[k]][decades$year == y])
add("pop_2010", DC(2010, "total"))
add("pop_2020", DC(2020, "total"))
for (k in c("two_plus", "sor_alone", "sor_any", "white_alone", "white_any",
            "black_alone", "black_any", "aian_alone", "aian_any",
            "asian_alone", "asian_any")) {
  add(paste0("d10_", k), DC(2010, k))
  add(paste0("d20_", k), DC(2020, k))
  add(paste0("chg_", k),
      sprintf("%+.1f", 100 * (DC(2020, k) / DC(2010, k) - 1)))
}
add("chg_pop", sprintf("%+.1f", 100 * (DC(2020, "total") /
                                       DC(2010, "total") - 1)))
add("d10_hispanic_pct", sprintf("%.1f", 100 * DC(2010, "hispanic") /
                                  DC(2010, "total")))
add("d20_hispanic_pct", sprintf("%.1f", 100 * DC(2020, "hispanic") /
                                  DC(2020, "total")))
add("d10_nh_white_pct", sprintf("%.1f", 100 * DC(2010, "nh_white") /
                                  DC(2010, "total")))
add("d20_nh_white_pct", sprintf("%.1f", 100 * DC(2020, "nh_white") /
                                  DC(2020, "total")))

# ---- the standards ---------------------------------------------------------
for (i in seq_len(nrow(standards))) {
  k <- standards$key[i]
  add(paste0("std", k, "_issued"), standards$issued[i])
  add(paste0("std", k, "_cite"), standards$citation[i])
  add(paste0("std", k, "_n"), standards$categories[i])
  add(paste0("std", k, "_census"), standards$first_census[i])
}

facts <- rbindlist(F)
stopifnot(!any(duplicated(facts$name)))
fwrite(dd_signif(as.data.frame(facts)), "derived/facts.csv")
say("  facts.csv: ", nrow(facts), " rows")

say("\nDONE.  Files written:")
print(data.frame(file = list.files(pattern = "\\.csv$"),
                 kb = round(file.size(list.files(pattern = "\\.csv$")) / 1024, 1)))

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
