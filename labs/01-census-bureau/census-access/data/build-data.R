# ---------------------------------------------------------------------------
# Build the census-access dataset: every published answer to one question.
#
# THE QUESTION is "how many people live in Allegheny County?" and the point of
# this script is that the Census Bureau has published well over a dozen
# answers to it, all of them current, all of them official, and no two of them
# the same. The chapter is a map of which product answers which version of the
# question, and where each product can be picked up.
#
# WHAT IS FETCHED, all of it from www2.census.gov, none of it needing a key:
#
#   1. pa2020.pl.zip                   the 2020 count, tables P1 to P4
#   2. acsdt5y{2021..2024}-b01003.dat  four five-year survey windows
#   3. acsdt1y{2021..2024}-b01003.dat  four single-year survey windows
#   4. co-est{2021..2025}-alldata.csv  five vintages of the estimates model
#   5. co-est2020.csv                  the 2010 count and the 2010s series
#   6. api.census.gov/data.json        the API's own catalog of what it carries
#
# Everything except the catalog is preserved whole in raw/, per labs/DATA-LAYOUT.md,
# because a chapter about where files come from cannot throw the files away.
# About 135 MB. Delete raw/ and the script fetches it all again; leave it and
# the build runs offline.
#
# THREE FINDINGS THE SCRIPT ESTABLISHES, none of which is a claim about bytes:
#
#   A. The redistricting file publishes four tables and TWO populations. P1 and
#      P2 both total everybody; P3 and P4 both total the 18-and-over. "The
#      population of the county" is already ambiguous inside a single file.
#
#   B. The estimates model republishes its whole series every year. Five
#      vintages therefore give five different figures for 1 July 2021, and the
#      spread between them is larger than most people assume a published
#      population can move.
#
#      This is measured for every county, not asserted from one.
#
#   C. The one-year survey figure for total population IS the estimates figure
#      for the same year -- exactly, for every county that has both. Two routes
#      that look independent hand you one number. The five-year figure is not,
#      which is the contrast that shows B is a real test and not a tautology.
#
# WHY ALLEGHENY COUNTY. It is where the students are. The national tables are
# here so that the county is the illustration and the country is the evidence.
#
# Run from this directory:  Rscript build-data.R    (needs internet on first run)
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")     # dd_write_csv(): six significant digits
source("../../../_lib/provenance.R")    # notices when a source moves

dir.create("derived", showWarnings = FALSE)
dir.create("raw",     showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE, warn = 1, timeout = 3600)

FIPS  <- "42003"          # Allegheny County, Pennsylvania
STATE <- "Pennsylvania"
ACS_YEARS <- 2021:2024    # the table-based summary file begins with 2021
PEP_VINT  <- 2021:2025    # every vintage published off the 2020 base

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
  say("  check: %-58s %s", label, value)
  invisible(value)
}

# raw/ is the cache. A file already there is not re-fetched, so a rebuild is
# free and offline. Nothing here lands in a temporary directory.
#
# A cache hit is still recorded in PROVENANCE.tsv. It has to be: otherwise the
# provenance file lists only whatever happened to be missing on the last run,
# and a reader cannot tell which URL any file in raw/ came from.
grab <- function(url, file) {
  p <- file.path("raw", file)
  if (!file.exists(p)) {
    say("downloading %s", url)
    prov_fetch(url, p, mode = "wb", quiet = TRUE)
  } else {
    .prov_record(url, p)
  }
  p
}

# Read a few columns out of a very wide pipe-delimited file. The redistricting
# segments are 150-odd fields and 170 MB each; naming the columns to keep turns
# a two-minute read into a ten-second one.
read_cols <- function(path, keep, nfields, enc = NULL) {
  cc <- rep("NULL", nfields)
  cc[keep] <- "character"
  args <- list(path, sep = "|", header = FALSE, quote = "", colClasses = cc)
  if (!is.null(enc)) args$fileEncoding <- enc
  d <- do.call(read.delim, args)
  names(d) <- paste0("f", keep)
  d
}

# ===========================================================================
# 1. THE COUNT.  2020 Census, P.L. 94-171 redistricting file.
#
# Four tables ship in this file and they are not four populations:
#
#   P1  total population, by race                        segment 1, field 6
#   P2  the same total, by Hispanic origin and race      segment 1, field 77
#   P3  population 18 and over, by race                  segment 2, field 6
#   P4  the same 18-and-over total, by Hispanic origin   segment 2, field 77
#
# So the file answers "how many people" twice and "how many adults" twice. A
# request for "the population" has already chosen between them without saying so.
# ===========================================================================

plz <- grab(paste0("https://www2.census.gov/programs-surveys/decennial/2020/data/",
                   "01-Redistricting_File--PL_94-171/", STATE, "/pa2020.pl.zip"),
            "pa2020.pl.zip")
pld <- file.path(tempdir(), "pa2020.pl")     # the zip is kept; the unpack is not
if (!dir.exists(pld)) utils::unzip(plz, exdir = pld)

# Geography header: 3 SUMLEV, 8 LOGRECNO, 10 GEOCODE, 88 NAME, of 97 fields.
geo <- read_cols(file.path(pld, "pageo2020.pl"), c(3, 8, 10, 88), 97, "latin1")
geo <- geo[geo$f3 == "050", ]                                    # counties only
s1  <- read_cols(file.path(pld, "pa000012020.pl"), c(5, 6, 77), 149)
s2  <- read_cols(file.path(pld, "pa000022020.pl"), c(5, 6, 77), 152)

pl <- data.frame(
  fips = geo$f10,
  name = geo$f88,
  p1   = as.numeric(s1$f6 [match(geo$f8, s1$f5)]),   # everyone
  p2   = as.numeric(s1$f77[match(geo$f8, s1$f5)]),   # everyone again
  p3   = as.numeric(s2$f6 [match(geo$f8, s2$f5)]),   # 18 and over
  p4   = as.numeric(s2$f77[match(geo$f8, s2$f5)]))   # 18 and over again
stopifnot(nrow(pl) == 67, !anyNA(pl$p1), !anyNA(pl$p3))

pl_one <- pl[pl$fips == FIPS, ]
stopifnot(nrow(pl_one) == 1)
say("2020 count: %s  everyone %s  18+ %s",
    pl_one$name, n(pl_one$p1), n(pl_one$p3))

fact("county",     pl_one$name, "the county the chapter follows")
fact("dec_all",    pl_one$p1,   "2020 census count, everybody, 1 April 2020")
fact("dec_adult",  pl_one$p3,   "2020 census count, 18 and over")
fact("dec_adult_share", round(100 * pl_one$p3 / pl_one$p1, 1),
     "the 18-and-over count as a share of the whole")

# The two pairs are the same totals seen through different breakdowns. If that
# ever stopped being true the file's structure would have changed, so it is
# asserted for all 67 counties rather than read off one.
check("P1 and P2 publish the same total for every county",
      all(pl$p1 == pl$p2))
check("P3 and P4 publish the same 18-and-over total for every county",
      all(pl$p3 == pl$p4))
check("the two populations in one file are not the same number",
      all(pl$p1 > pl$p3))
dd_write_csv(pl, "derived/decennial_pa.csv")

# ===========================================================================
# 2. THE SURVEY.  American Community Survey, table B01003, total population.
#
# Eight published figures: four five-year windows and four single years. The
# margin column is kept as it arrives -- for a county total it is almost always
# the Bureau's controlled-estimate sentinel, which the ACS chapter takes apart.
# What matters here is only the estimate.
# ===========================================================================

acs_url <- function(y, w) paste0(
  "https://www2.census.gov/programs-surveys/acs/summary_file/", y,
  "/table-based-SF/data/", w, "YRData/acsdt", w, "y", y, "-b01003.dat")

read_acs <- function(y, w) {
  d <- read.delim(grab(acs_url(y, w), sprintf("acsdt%dy%d-b01003.dat", w, y)),
                  sep = "|", colClasses = "character")
  names(d)[1:3] <- c("geo_id", "est", "moe")
  d <- d[startsWith(d$geo_id, "0500000US"), ]
  data.frame(fips = substr(d$geo_id, 10, 14),
             est  = as.numeric(d$est),
             moe  = as.numeric(d$moe))
}

acs5 <- lapply(ACS_YEARS, read_acs, w = 5); names(acs5) <- ACS_YEARS
acs1 <- lapply(ACS_YEARS, read_acs, w = 1); names(acs1) <- ACS_YEARS

for (y in as.character(ACS_YEARS))
  say("ACS %s: five-year %s   one-year %s", y,
      n(acs5[[y]]$est[acs5[[y]]$fips == FIPS]),
      n(acs1[[y]]$est[acs1[[y]]$fips == FIPS]))

# The single-year file is published only where the county is large enough, and
# the five-year file everywhere. That is why both exist, and it is the first
# thing a reader hits when a county they want is missing from one of them.
fact("acs5_counties", nrow(acs5[["2024"]]),
     "county rows in the newest five-year file")
fact("acs1_counties", nrow(acs1[["2024"]]),
     "county rows in the newest one-year file")
fact("acs_only_5yr", nrow(acs5[["2024"]]) - nrow(acs1[["2024"]]),
     "counties the five-year file covers and the one-year file does not")
check("the one-year file covers far fewer counties than the five-year",
      nrow(acs1[["2024"]]) < nrow(acs5[["2024"]]) / 2)

# There is no 2020 single-year file. The pandemic year's response rates were
# too low for the Bureau to publish the standard series, so it did not. The
# chapter says so, and a chapter does not assert a 404 it has not asked for.
gap_url  <- acs_url(2020, 1)
gap_code <- tryCatch({
  h <- suppressWarnings(system2("curl", c("-sIL", "-o", "/dev/null",
                                          "-w", "%{http_code}", shQuote(gap_url)),
                                stdout = TRUE)); trimws(h[length(h)])
}, error = function(e) NA_character_)
fact("acs1_2020_http", gap_code, "what the 2020 one-year file's address returns")
check("there is no 2020 one-year file to fetch", identical(gap_code, "404"))

# ===========================================================================
# 3. THE MODEL.  Population Estimates Program, county totals.
#
# The vintage is the whole story. Each vintage republishes EVERY year back to
# the 2020 base, revised. So the estimate for 1 July 2022 has been published
# four separate times, with four different values, and the file does not
# present itself as a revision of anything.
# ===========================================================================

pep_of <- function(v) {
  d <- read.csv(grab(paste0("https://www2.census.gov/programs-surveys/popest/",
                            "datasets/2020-", v, "/counties/totals/co-est", v,
                            "-alldata.csv"),
                     sprintf("co-est%d-alldata.csv", v)),
                colClasses = "character")
  d <- d[d$COUNTY != "000", ]
  d$fips <- paste0(d$STATE, d$COUNTY)
  d
}
pep <- lapply(PEP_VINT, pep_of); names(pep) <- PEP_VINT

# One row per (vintage, reference year): what that vintage said about that date.
rev_rows <- do.call(rbind, lapply(PEP_VINT, function(v) {
  d  <- pep[[as.character(v)]]
  ys <- 2020:v
  do.call(rbind, lapply(ys, function(y) data.frame(
    vintage = v, refers_to = y,
    population = as.numeric(d[[paste0("POPESTIMATE", y)]][d$fips == FIPS]))))
}))
dd_write_csv(rev_rows, "derived/pep_revisions.csv")

# The same date, as many times as it has been published. This is the table the
# chapter draws, and it is computed for the county, then checked nationally.
same_date <- do.call(rbind, lapply(2020:max(PEP_VINT), function(y) {
  vs <- PEP_VINT[PEP_VINT >= y]
  v  <- rev_rows$population[rev_rows$refers_to == y & rev_rows$vintage %in% vs]
  data.frame(refers_to = y, times_published = length(v),
             lowest = min(v), highest = max(v), spread = max(v) - min(v))
}))
dd_write_csv(same_date, "derived/pep_same_date.csv")

worst <- same_date[which.max(same_date$spread), ]
fact("pep_vintages",  length(PEP_VINT), "vintages of the estimates published off the 2020 base")
fact("pep_worst_year",   worst$refers_to, "the reference date whose published value moved most")
fact("pep_worst_spread", worst$spread,
     "how far apart its highest and lowest published values are")
fact("pep_worst_times",  worst$times_published, "how many times that date has been published")
fact("pep_newest", rev_rows$population[rev_rows$vintage == max(PEP_VINT) &
                                       rev_rows$refers_to == max(PEP_VINT)],
     "the newest estimate: 1 July of the newest vintage year")
check("no reference date keeps the same value across vintages",
      all(same_date$spread[same_date$times_published > 1] > 0))

# Nationally, so the county is not carrying the claim. For each county, how far
# the published value for one date has moved across the vintages that cover it.
nat_rev <- function(y) {
  vs <- PEP_VINT[PEP_VINT >= y]
  m  <- sapply(vs, function(v) {
    d <- pep[[as.character(v)]]
    as.numeric(d[[paste0("POPESTIMATE", y)]])[match(pep[["2025"]]$fips, d$fips)]
  })
  data.frame(fips = pep[["2025"]]$fips,
             spread_pct = 100 * (apply(m, 1, max) - apply(m, 1, min)) /
                          apply(m, 1, max))
}
rv <- nat_rev(worst$refers_to)
rv <- rv[!is.na(rv$spread_pct), ]
dd_write_csv(rv[order(-rv$spread_pct), ], "derived/pep_revision_national.csv")
fact("rev_counties", nrow(rv), "counties in the national revision table")
fact("rev_median_pct", round(median(rv$spread_pct), 2),
     "median county-level movement in the published value for that one date")
fact("rev_unchanged", sum(rv$spread_pct == 0),
     "counties whose published value for that date never moved")
fact("rev_over_1pct", sum(rv$spread_pct > 1),
     "counties where it moved by more than one per cent")
check("the revision table on disk holds every county, not a top-N",
      nrow(read.csv("derived/pep_revision_national.csv")) == nrow(rv))
check("for most counties the published value for one date did move",
      sum(rv$spread_pct == 0) < nrow(rv) / 2)

# The 2010 count, and the two estimates bases. A count becomes a base by being
# adjusted, so the base is not the count -- a small difference that matters to
# anyone who compares an estimate against "the census figure".
e10 <- read.csv(grab(paste0("https://www2.census.gov/programs-surveys/popest/",
                            "datasets/2010-2020/counties/totals/co-est2020.csv"),
                     "co-est2020.csv"), colClasses = "character")
e10$fips <- paste0(e10$STATE, e10$COUNTY)
e10 <- e10[e10$COUNTY != "000", ]
c10 <- as.numeric(e10$CENSUS2010POP[e10$fips == FIPS])
b20 <- as.numeric(pep[["2025"]]$ESTIMATESBASE2020[pep[["2025"]]$fips == FIPS])
fact("dec_2010", c10, "the 2010 census count for the county")
fact("base_2020", b20, "the 2020 count after adjustment into an estimates base")
fact("base_gap", b20 - pl_one$p1, "how far the base sits from the published count")
check("the estimates base is not identical to the published count",
      b20 != pl_one$p1)

# ===========================================================================
# 4. TWO ROUTES, ONE NUMBER.
#
# The single-year survey estimate of total population for a county is not an
# independent measurement of anything: it is the estimates figure for the same
# year. Checked county by county, for every county that carries both.
#
# The five-year estimate is NOT any published estimates figure, which is what
# makes the one-year result a finding rather than an arithmetic identity.
# ===========================================================================

match_rate <- function(acs_tab, pep_year, pep_vintage) {
  d <- pep[[as.character(pep_vintage)]]
  i <- match(acs_tab$fips, d$fips)
  p <- as.numeric(d[[paste0("POPESTIMATE", pep_year)]])[i]
  ok <- !is.na(p)
  bad <- ok & acs_tab$est != p
  bad[is.na(bad)] <- FALSE
  list(counties = sum(ok), exact = sum(acs_tab$est[ok] == p[ok]),
       off = data.frame(fips = acs_tab$fips[bad],
                        name = paste(d$CTYNAME[i][bad], d$STNAME[i][bad], sep = ", "),
                        gap  = abs(acs_tab$est[bad] - p[bad])))
}

one_res <- lapply(ACS_YEARS, function(y) match_rate(acs1[[as.character(y)]], y, y))
one_vs  <- do.call(rbind, Map(function(y, m) data.frame(
  window = paste0("one-year ", y),
  compared_with = paste0("estimates vintage ", y, ", 1 July ", y),
  counties = m$counties, exact_matches = m$exact), ACS_YEARS, one_res))

# The exceptions, by name. A finding stated as "every county" has to survive
# being asked which ones it is not true of.
odd_all <- do.call(rbind, lapply(one_res, function(m) m$off))
odd     <- unique(odd_all$name)

# Puerto Rico's municipios are in the survey file and not in the county
# estimates file -- the estimates publish them separately. They are dropped
# from the comparison above rather than counted as disagreements, so the
# number of them is recorded here instead of being silently lost.
pr_only <- setdiff(acs1[["2024"]]$fips, pep[["2024"]]$fips)
five_vs <- do.call(rbind, lapply(PEP_VINT, function(v) {
  m <- match_rate(acs5[["2024"]], v, max(PEP_VINT))
  data.frame(window = "five-year 2020-2024",
             compared_with = paste0("estimates vintage ", max(PEP_VINT),
                                    ", 1 July ", v),
             counties = m$counties, exact_matches = m$exact)
}))
dd_write_csv(rbind(one_vs, five_vs), "derived/route_overlap.csv")

fact("one_year_counties", sum(one_vs$counties), "county comparisons across the one-year files")
fact("one_year_exact",    sum(one_vs$exact_matches), "of them where the two figures are identical")
fact("one_year_off",      sum(one_vs$counties) - sum(one_vs$exact_matches),
     "comparisons where they differ")
fact("one_year_odd",      paste(odd, collapse = "; "), "the county they differ in")
fact("one_year_odd_max",  max(odd_all$gap), "the largest of those differences, in people")
fact("pr_only",           length(pr_only),
     "survey counties with no row in the county estimates file")
fact("five_year_best",    max(five_vs$exact_matches),
     "the most any single estimates year matches the five-year figure")
fact("five_year_counties", five_vs$counties[1], "counties in that comparison")
check("the one-year survey figure is the estimates figure, bar one county",
      length(odd) == 1)
check("...which holds in every year, not just the newest",
      all(one_vs$counties - one_vs$exact_matches == 1))
check("the five-year figure matches no published estimates year",
      max(five_vs$exact_matches) < five_vs$counties[1] / 100)

# ===========================================================================
# 5. THE CATALOG.  Every published answer for this one county.
#
# One row per published figure. This is the table the chapter is built around,
# and the ONLY hand-written parts of it are the labels; every population comes
# out of a file fetched above.
# ===========================================================================

g5 <- function(y) acs5[[as.character(y)]]$est[acs5[[as.character(y)]]$fips == FIPS]
g1 <- function(y) acs1[[as.character(y)]]$est[acs1[[as.character(y)]]$fips == FIPS]
gp <- function(v, y) rev_rows$population[rev_rows$vintage == v & rev_rows$refers_to == y]

# refers_mid is the middle of the period each figure describes, as a decimal
# year. A census day is 1 April, an estimate is 1 July, a single survey year
# centres on the middle of that year, and a five-year window centres two and a
# half years before its label. It exists so the catalog can be drawn against
# time, which is what shows that most of the spread between these figures is
# the calendar rather than disagreement.
catalog <- rbind(
  data.frame(program = "Decennial census", product = "2020 P.L. 94-171, table P1",
             counts = "everybody", refers_to = "1 April 2020",
             refers_mid = 2020.25, population = pl_one$p1),
  data.frame(program = "Decennial census", product = "2020 P.L. 94-171, table P3",
             counts = "18 and over", refers_to = "1 April 2020",
             refers_mid = 2020.25, population = pl_one$p3),
  data.frame(program = "Decennial census", product = "2010 P.L. 94-171, table P1",
             counts = "everybody", refers_to = "1 April 2010",
             refers_mid = 2010.25, population = c10),
  do.call(rbind, lapply(rev(ACS_YEARS), function(y) data.frame(
    program = "American Community Survey",
    product = paste0("five-year ", y - 4, "-", y, ", table B01003"),
    counts = "everybody", refers_to = paste0(y - 4, "-", y, " average"),
    refers_mid = y - 1.5, population = g5(y)))),
  do.call(rbind, lapply(rev(ACS_YEARS), function(y) data.frame(
    program = "American Community Survey",
    product = paste0("one-year ", y, ", table B01003"),
    counts = "everybody", refers_to = paste0(y, " average"),
    refers_mid = y + 0.5, population = g1(y)))),
  data.frame(program = "Population Estimates", product = "vintage 2025 base",
             counts = "everybody", refers_to = "1 April 2020",
             refers_mid = 2020.25, population = b20),
  do.call(rbind, lapply(rev(PEP_VINT), function(v) data.frame(
    program = "Population Estimates", product = paste0("vintage ", v),
    counts = "everybody", refers_to = paste0("1 July ", v),
    refers_mid = v + 0.5, population = gp(v, v)))),
  data.frame(program = "Population Estimates", product = "vintage 2024",
             counts = "everybody", refers_to = "1 July 2023",
             refers_mid = 2023.5, population = gp(2024, 2023)),
  data.frame(program = "Population Estimates", product = "vintage 2025",
             counts = "everybody", refers_to = "1 July 2023",
             refers_mid = 2023.5, population = gp(2025, 2023))
)
catalog <- catalog[order(-catalog$population), ]
row.names(catalog) <- NULL
dd_write_csv(catalog, "derived/catalog.csv")

# The spread is taken over the figures that count EVERYBODY. The 18-and-over
# row is a different question and including it would inflate the spread by
# comparing adults with people, which is the error the chapter warns about.
all_people <- catalog$population[catalog$counts == "everybody"]
fact("catalog_rows", nrow(catalog), "published figures the chapter lists for this county")
fact("catalog_all",  length(all_people), "of them that count everybody")
fact("catalog_distinct", length(unique(all_people)), "distinct values among those")
fact("cat_high", max(all_people), "the largest")
fact("cat_low",  min(all_people), "the smallest")
fact("cat_spread", max(all_people) - min(all_people), "the distance between them")
fact("cat_spread_pct", round(100 * (max(all_people) - min(all_people)) / max(all_people), 2),
     "the same distance as a share of the largest")
# The catalog holds fewer distinct values than rows, and that is finding C
# arriving in the table the reader sees: the survey rows and the estimates rows
# that repeat each other are the duplicates.
check("the catalog has fewer distinct values than rows",
      length(unique(all_people)) < length(all_people))
check("...and the repeats are the survey/estimates pairs, nothing else",
      length(all_people) - length(unique(all_people)) == length(ACS_YEARS))
check("every figure in the catalog came from a fetched file",
      all(is.finite(catalog$population)) && nrow(catalog) > 15)

# The figure claims the spread runs with the calendar. That is a claim about
# these numbers, so it is tested rather than asserted: across the figures that
# count everybody and refer to 2020 or later, population falls as the reference
# date advances.
recent <- catalog[catalog$counts == "everybody" & catalog$refers_mid >= 2020, ]
fact("time_corr", round(cor(recent$refers_mid, recent$population), 2),
     "how tightly the published figures track the date they refer to")
check("the later a figure refers to, the smaller it is",
      cor(recent$refers_mid, recent$population) < -0.7)

# The same span for every county, so Allegheny is an instance and not the case.
nat <- data.frame(fips = acs5[["2024"]]$fips)
for (y in ACS_YEARS) {
  nat[[paste0("acs5_", y)]] <- acs5[[as.character(y)]]$est[match(nat$fips, acs5[[as.character(y)]]$fips)]
  nat[[paste0("acs1_", y)]] <- acs1[[as.character(y)]]$est[match(nat$fips, acs1[[as.character(y)]]$fips)]
}
for (v in PEP_VINT) {
  d <- pep[[as.character(v)]]
  nat[[paste0("pep_", v)]] <- as.numeric(d[[paste0("POPESTIMATE", v)]])[match(nat$fips, d$fips)]
}
m <- as.matrix(nat[, -1])
nat$published  <- rowSums(!is.na(m))
nat$distinct   <- apply(m, 1, function(r) length(unique(r[!is.na(r)])))
nat$spread_pct <- 100 * (apply(m, 1, max, na.rm = TRUE) -
                         apply(m, 1, min, na.rm = TRUE)) /
                  apply(m, 1, max, na.rm = TRUE)
nat <- nat[is.finite(nat$spread_pct), ]
dd_write_csv(nat[order(-nat$spread_pct), c("fips", "published", "distinct", "spread_pct")],
             "derived/national_spread.csv")

fact("nat_counties", nrow(nat), "counties with at least one current figure from each route")
fact("nat_median_pct", round(median(nat$spread_pct), 2),
     "median spread across the current figures, as a share of the largest")
fact("nat_agree", sum(nat$spread_pct == 0), "counties where every current figure agrees")
fact("nat_over_5", sum(nat$spread_pct > 5), "counties where they span more than five per cent")
check("the national table on disk holds every county, not a top-N",
      nrow(read.csv("derived/national_spread.csv")) == nrow(nat))
check("agreement across products is rare, not the rule",
      sum(nat$spread_pct == 0) < nrow(nat) / 50)

# ===========================================================================
# 6. WHERE EACH ONE LIVES.
#
# The API's own catalog is a public JSON index, keyless, listing every dataset
# it serves. It is the honest way to say which vintages a route carries,
# because it is the route describing itself.
#
# The finding: the API carries the survey back nearly twenty years and stops
# carrying county population estimates at vintage 2021. The four newest
# vintages -- the ones a reader is most likely to want -- exist only as files.
# ===========================================================================

cat_p <- grab("https://api.census.gov/data.json", "api-catalog.json")
idx <- jsonlite::fromJSON(cat_p, simplifyVector = FALSE)$dataset

# Each record carries a "c_dataset" path -- c("acs","acs1") -- and a
# "c_vintage" year. So the vintages a product offers can be read straight off
# the index, which is the route describing its own holdings.
ds_path <- vapply(idx, function(x) paste(x$c_dataset, collapse = "/"), "")
ds_year <- vapply(idx, function(x) if (is.null(x$c_vintage)) NA_integer_
                                   else as.integer(x$c_vintage), 0L)
vintages_of <- function(path) sort(unique(ds_year[ds_path == path & !is.na(ds_year)]))
API <- c("dec/pl", "acs/acs5", "acs/acs1", "pep/population")
api_v <- lapply(API, vintages_of); names(api_v) <- API

api_tab <- data.frame(
  product = c("Decennial redistricting file", "Survey, five-year",
              "Survey, one-year", "Population estimates"),
  api_path = API,
  vintages = vapply(api_v, length, 0L),
  earliest = vapply(api_v, function(v) if (length(v)) min(v) else NA_integer_, 0L),
  latest   = vapply(api_v, function(v) if (length(v)) max(v) else NA_integer_, 0L))
row.names(api_tab) <- NULL
dd_write_csv(api_tab, "derived/api_vintages.csv")

n_datasets <- length(idx)
fact("api_datasets", n_datasets, "datasets the API's own catalog lists")
fact("api_pep_latest", max(api_v[["pep/population"]]),
     "the newest county estimates vintage the API carries")
fact("api_pep_missing", sum(PEP_VINT > max(api_v[["pep/population"]])),
     "vintages that exist as files and not on the API")
fact("api_acs1_earliest", min(api_v[["acs/acs1"]]),
     "how far back the API carries the one-year survey")
fact("acs1_gap_visible", as.integer(!(2020 %in% api_v[["acs/acs1"]])),
     "whether the missing 2020 one-year file is visible in the API catalog too")
check("the API's newest county estimates vintage is older than the newest file",
      max(api_v[["pep/population"]]) < max(PEP_VINT))
check("the 2020 gap in the one-year survey shows up in the API catalog as well",
      !(2020 %in% api_v[["acs/acs1"]]) && 2019 %in% api_v[["acs/acs1"]])

# Two of the three "carries" cells are read off the index above. The first is
# not: data.census.gov is a browser application, so what it offers cannot be
# asked of it in the same way, and the cell says only what is plainly true of it.
routes <- data.frame(
  route = c("data.census.gov", "api.census.gov", "www2.census.gov"),
  hands_you = c("one table at a time, drawn in a browser",
                "the cells you name, as JSON",
                "the published file, whole"),
  you_need = c("nothing", "a free key, for data, since 2026", "nothing"),
  carries = c("a search over the published tables; it cannot be scripted",
              paste0(n(n_datasets), " datasets; county estimates stop at vintage ",
                     max(api_v[["pep/population"]])),
              paste0("the files themselves, county estimates through vintage ",
                     max(PEP_VINT))),
  best_for = c("looking one number up, once, by hand",
               "a few named numbers inside a program",
               "a whole product, or a vintage the other two have dropped"))
dd_write_csv(routes, "derived/routes.csv")

# ===========================================================================

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
prov_report()
say("\ndone: %d facts, %d checks", nrow(facts), length(CHECKS))

if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
