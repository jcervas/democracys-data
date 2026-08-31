# ---------------------------------------------------------------------------
# Build the surnames datasets.
#
# Eight files end up in this folder:
#
#   derived/census_surnames.csv   every surname held by 100 or more people at the 2010
#                         Census, with the racial composition of its bearers
#   derived/county_race.csv       racial and ethnic composition of every U.S. county,
#                         in the same six categories as the surname file
#   derived/surnames_2020_top100.csv
#                         THE HUNDRED MOST COMMON surnames at the 2020 Census --
#                         a ranking, cut at 100, and cut deliberately: the whole
#                         2020 table is 156,621 rows and the chapter shows a
#                         handful of them
#   derived/surnames_2020_negatives.csv
#                         the dozen largest surnames whose published racial
#                         breakdown disagrees with the Bureau's own
#                         "WithNegatives" copy of the same table
#   derived/surnames_2020_facts.csv
#                         the scalars the 2020 section quotes
#   derived/census_surnames_2010.csv
#                         the 2010 table kept in full beside the 2020 one, so
#                         the two vintages can be compared name by name rather
#                         than in aggregate
#   derived/surnames_2010_vs_2020.csv
#                         what moved between the two vintages, per racial
#                         category: how many names were compared, the median
#                         and mean absolute shift, and how many names moved
#                         more than five points
#   derived/surnames_vintage_facts.csv
#                         the scalars the vintage-comparison section quotes
#
# Run this script from inside the data/ folder. It downloads about 100 MB and
# writes about 10 MB. The committed outputs mean the lab needs no network.
#
# NOTE: this lab deliberately does NOT use the `wru` package or the Census API.
# BISG is implemented by hand, in base R, so that students see Bayes' rule
# rather than calling a function that hides it. No API key is needed anywhere.
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


dir.create("raw", showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- 1. Census surname file -------------------------------------------------
#
# Source: U.S. Census Bureau, "Frequently Occurring Surnames from the 2010
# Census"  https://www2.census.gov/topics/genealogy/2010surnames/names.zip
#
# Every surname reported by 100 or more people. For each, the Bureau gives the
# percentage of bearers in six mutually exclusive categories:
#
#   pctwhite     non-Hispanic White alone
#   pctblack     non-Hispanic Black alone
#   pctapi       non-Hispanic Asian / Native Hawaiian / Pacific Islander alone
#   pctaian      non-Hispanic American Indian / Alaska Native alone
#   pct2prace    non-Hispanic two or more races
#   pcthispanic  Hispanic or Latino, any race
#
# Cells are SUPPRESSED (written "(S)") where publishing them would risk
# identifying individuals. Those become NA, and the lab treats that as
# information rather than as a nuisance.

# THE 2010 FILE DOWNLOADS ONLY SOMETIMES, and the failure is worth knowing
# exactly: on 12 and 14 Aug 2026 the address below answered 200 with a 247-byte
# HTML page reading "Request Rejected"; on 16 Aug 2026 the same address, asked
# the same way, served the real 12,874,389-byte zip. Everything else in the
# same directory was fine on every one of those days -- the Top 1000 extract,
# the documentation PDF -- and so was the whole 2020 release. It is this one
# file, and the block comes and goes. The Bureau still links it from
# <https://www.census.gov/topics/population/genealogy/data/2010_surnames.html>.
#
# So the fetch is attempted, and when it comes back as something other than a
# zip the committed extract is used instead and the fact is said out loud. The
# alternative -- stopping -- would make every downstream chapter unbuildable
# because of one intermittent block at the Census Bureau.
surl <- "https://www2.census.gov/topics/genealogy/2010surnames/names.zip"
zf <- tempfile(fileext = ".zip")
csv <- NULL
try(prov_fetch(surl, zf, mode = "wb", quiet = TRUE), silent = TRUE)
if (file.exists(zf) && file.size(zf) > 1e5) {
  sf  <- try(unzip(zf, exdir = tempdir()), silent = TRUE)
  if (!inherits(sf, "try-error"))
    csv <- grep("\\.csv$", sf, value = TRUE, ignore.case = TRUE)[1]
}
# THE COMMITTED SOURCE. raw/Names_2010Census.csv is the file that URL used to
# serve, taken from inside the zip: all eleven of the Bureau's own columns,
# 162,254 names and the ALL OTHER NAMES residual, unedited. It is committed
# because the URL cannot be counted on and a chapter that cannot be rebuilt
# from its source is not reproducible -- the same fix the Jacobson House file
# got.
#
# The extract stood alone at first because the zip itself could not be had
# when it was committed (14 Aug 2026, mid-block), and a zip assembled here
# would carry a different hash from the Bureau's -- it would sit in raw/
# looking exactly like the artifact it is not, which is the failure this book
# spends ninety-six chapters on. On 16 Aug 2026 the address answered, and the
# Bureau's own zip -- genuine artifact, genuine hash, recorded in
# PROVENANCE.tsv -- was captured and committed beside the extract as
# raw/names.zip. Its CSV is byte-identical to the committed extract. The
# fallback below still reads the extract; the zip is the archival capture.
LOCAL_2010 <- "raw/Names_2010Census.csv"
if ((is.null(csv) || is.na(csv)) && file.exists(LOCAL_2010)) {
  cat("\n  !! the 2010 surname list did not download (",
      if (file.exists(zf)) file.size(zf) else 0, " bytes returned).\n",
      "     Reading the committed copy at ", LOCAL_2010, " instead. Same file,\n",
      "     same eleven columns; only the download is missing.\n", sep = "")
  csv <- LOCAL_2010
}
if (is.null(csv) || is.na(csv)) {
  cat("\n  !! the 2010 surname list did not download, and ", LOCAL_2010, "\n",
      "     is missing too. Falling back to this chapter's OWN derived\n",
      "     extract, which is nine columns of a table it wrote earlier.\n", sep = "")
  # NOT census_surnames.csv -- that name now holds the 2020 table (section 4),
  # so falling back to it would silently compare 2020 with itself.
  stopifnot(file.exists("derived/census_surnames_2010.csv"))
  csv <- "derived/census_surnames_2010.csv"
}

sn <- read.csv(csv, stringsAsFactors = FALSE, na.strings = c("(S)", ""))
names(sn) <- tolower(names(sn))
cat("surname rows:", nrow(sn), "\n")

pctcols <- c("pctwhite", "pctblack", "pctapi", "pctaian", "pct2prace", "pcthispanic")
stopifnot(all(c("name", "rank", "count", pctcols) %in% names(sn)))
for (v in c("rank", "count", pctcols)) sn[[v]] <- suppressWarnings(as.numeric(sn[[v]]))

sn <- sn[!is.na(sn$count), c("name", "rank", "count", pctcols)]
cat("suppressed cells:", sum(is.na(sn[, pctcols])), "of",
    nrow(sn) * length(pctcols), "\n")
# The 2010 table is kept under its own name. It is no longer what BISG runs on
# -- see section 3 -- but its download cannot be counted on, so the copy on
# disk is the one that matters, and the chapter compares the two vintages
# directly.
dd_write_csv(sn, "derived/census_surnames_2010.csv")

# --- 2. County racial composition -------------------------------------------
#
# Source: U.S. Census Bureau, Population Estimates Program, county
# characteristics file
#   https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/asrh/cc-est2024-alldata.csv
#
# YEAR == 6 is the 1 July 2024 estimate; AGEGRP == 0 is all ages. Categories
# are collapsed to match the surname file exactly, so that Bayes' rule can be
# applied without any category mismatch.

curl_ <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
                "2020-2024/counties/asrh/cc-est2024-alldata.csv")
cc <- read.csv(curl_, stringsAsFactors = FALSE,
               colClasses = c(STATE = "character", COUNTY = "character"))

cc <- cc[cc$YEAR == 6 & cc$AGEGRP == 0, ]
cat("\ncounties:", nrow(cc), "\n")

p <- function(a, b) cc[[a]] + cc[[b]]
county <- data.frame(
  fips     = paste0(cc$STATE, cc$COUNTY),
  county   = cc$CTYNAME,
  state    = cc$STNAME,
  pop      = cc$TOT_POP,
  white    = p("NHWA_MALE", "NHWA_FEMALE"),
  black    = p("NHBA_MALE", "NHBA_FEMALE"),
  api      = p("NHAA_MALE", "NHAA_FEMALE") + p("NHNA_MALE", "NHNA_FEMALE"),
  aian     = p("NHIA_MALE", "NHIA_FEMALE"),
  twoplus  = p("NHTOM_MALE", "NHTOM_FEMALE"),
  hispanic = p("H_MALE", "H_FEMALE"))

for (v in c("white", "black", "api", "aian", "twoplus", "hispanic")) {
  county[[v]] <- county[[v]] / county$pop
}

s <- rowSums(county[, c("white","black","api","aian","twoplus","hispanic")])
cat("category shares sum to 1: min", round(min(s), 6),
    "max", round(max(s), 6), "\n")
stopifnot(all(abs(s - 1) < 1e-6))

county <- county[order(county$state, county$county), ]
dd_write_csv(county, "derived/county_race.csv")

# --- 3. Report the facts the lab is built on --------------------------------

big <- sn[sn$count > 1000, ]
cat("\nsurnames held by more than 1,000 people:", nrow(big), "\n")
for (v in pctcols) {
  x <- big[[v]]; x <- x[!is.na(x)]
  cat(sprintf("  more than 90%% %-12s %5d names\n", sub("pct", "", v), sum(x > 90)))
}

cat("\ndone.\n")

# ---------------------------------------------------------------------------
# 3. The 2020 release, and the counts that come out below zero
# ---------------------------------------------------------------------------
#
# The Bureau published the same tabulation from the 2020 Census, and it is NOT
# a drop-in replacement for the 2010 one. Three differences, in rising order of
# interest:
#
#   1. It is a spreadsheet, not a zipped CSV, and it downloads without trouble
#      every time -- where the section above must be ready to fall back to a
#      committed copy.
#   2. It gives COUNTS of people by race, where 2010 gave rounded percentages
#      with the small cells suppressed as "(S)". Counts are the better input to
#      Bayes' rule: a share can be recovered from them exactly.
#   3. IT PUBLISHES TWO VERSIONS OF THE SAME TABLE, and they disagree.
#
# That third one is what this section is about. The 2020 Census was released
# under a disclosure-avoidance system that adds noise to every count before
# publication, and noise added to a small count can push it below zero. The
# Bureau does not hide that. It ships the clean table, and beside it the same
# table "WithNegatives", where a surname is recorded as being held by minus
# three people.
#
# Neither file is wrong. They are two answers to a question the noise makes
# unanswerable -- how many people named FENG are American Indian -- and the
# difference between them is the size of the fiction that makes publication
# safe.
#
# Sources, both fetched without a key or an account:
#   https://www2.census.gov/topics/genealogy/2020surnames/Names2020_LastNames_RaceHispanic.xlsx
#   https://www2.census.gov/topics/genealogy/2020surnames/Names2020_LastNames_RaceHispanic_WithNegatives.xlsx

stopifnot(requireNamespace("readxl", quietly = TRUE))
B20 <- "https://www2.census.gov/topics/genealogy/2020surnames/"
RACE <- c("white", "black", "aian", "asian", "twoplus", "hispanic")

grab <- function(f) {
  p <- file.path("raw", f)
  if (!file.exists(p)) prov_fetch(paste0(B20, f), p, mode = "wb", quiet = TRUE)
  p
}
pub <- readxl::read_excel(grab("Names2020_LastNames_RaceHispanic.xlsx"),
                          skip = 2, col_types = "text")
neg <- readxl::read_excel(grab("Names2020_LastNames_RaceHispanic_WithNegatives.xlsx"),
                          skip = 2, col_types = "text")
names(pub) <- c("name", "rank", "count", "per100k", "cum", RACE)
names(neg) <- c("name", RACE)
num <- function(d, cols) { for (v in cols) d[[v]] <- suppressWarnings(as.numeric(d[[v]])); d }
pub <- num(pub, c("rank", "count", "per100k", "cum", RACE))
neg <- num(neg, RACE)
# THE LAST ROW IS NOT A SURNAME, AND IN 2020 IT IS NOT LABELLED EITHER.
# Both vintages sweep every name held by fewer than 100 people into one line
# called ALL OTHER NAMES -- 36,257,637 people in 2020. The 2010 file flags it
# by giving it rank 0. The 2020 file leaves the rank cell EMPTY, so a filter
# that drops rows without a rank deletes thirty-six million people without
# saying anything. It is given rank 0 here, so that the residual keeps the same
# label it has always had and every chapter downstream sees what it expects.
pub$rank[is.na(pub$rank) & grepl("^ALL OTHER", pub$name)] <- 0
stopifnot(sum(pub$rank == 0, na.rm = TRUE) == 1)
pub <- pub[!is.na(pub$rank), ]
names_only <- pub[pub$rank > 0, ]      # the residual is not a name
neg <- neg[neg$name %in% names_only$name, ]
cat("\n2020 last names:", nrow(names_only), "rows, plus the residual\n")

# --- how much of the table is affected -------------------------------------
NM  <- as.matrix(neg[, RACE])
anyneg <- rowSums(NM < 0, na.rm = TRUE) > 0
N_NEG_ROWS  <- sum(anyneg)
N_NEG_CELLS <- sum(NM < 0, na.rm = TRUE)
WORST <- min(NM, na.rm = TRUE)

# --- and what the published table does about it -----------------------------
# The negative is not deleted. It is absorbed by the cells beside it, so the
# row still adds to the same number of people. That is checked here rather
# than asserted, because it is the whole point of the section.
m <- merge(names_only[, c("name", RACE)], neg, by = "name", suffixes = c("_pub", "_neg"))
tp <- rowSums(m[, paste0(RACE, "_pub")], na.rm = TRUE)
tn <- rowSums(m[, paste0(RACE, "_neg")], na.rm = TRUE)
SAME_TOTAL <- sum(abs(tp - tn) < 0.5)

# --- the top of the table, for the chapter to show --------------------------
top <- names_only[order(names_only$rank), ][1:100, ]
for (v in RACE) top[[paste0("pct_", v)]] <- 100 * top[[v]] / top$count
dd_write_csv(top[, c("name", "rank", "count", paste0("pct_", RACE))],
             "derived/surnames_2020_top100.csv")

# --- a handful of rows where the two files disagree, for the exhibit --------
ex <- m[m$name %in% neg$name[anyneg], ]
ex <- ex[order(-rowSums(ex[, paste0(RACE, "_pub")], na.rm = TRUE)), ][1:12, ]
out <- do.call(rbind, lapply(seq_len(nrow(ex)), function(i) {
  j <- which.min(unlist(ex[i, paste0(RACE, "_neg")]))
  data.frame(name = ex$name[i], category = RACE[j],
             published = ex[[paste0(RACE[j], "_pub")]][i],
             with_negatives = ex[[paste0(RACE[j], "_neg")]][i],
             row_total = rowSums(ex[i, paste0(RACE, "_pub")], na.rm = TRUE),
             stringsAsFactors = FALSE)
}))
dd_write_csv(out, "derived/surnames_2020_negatives.csv")

dd_write_csv(data.frame(
  key = c("rows_2020", "rows_with_negative", "pct_rows_with_negative",
          "negative_cells", "worst_negative", "rows_total_unchanged",
          "rows_2010", "suppressed_cells_2010"),
  value = c(nrow(names_only), N_NEG_ROWS, round(100 * N_NEG_ROWS / nrow(names_only), 1),
            N_NEG_CELLS, WORST, SAME_TOTAL,
            nrow(sn), sum(is.na(sn[, pctcols])))),
  "derived/surnames_2020_facts.csv")

cat(sprintf("  surnames with a negative count: %s of %s (%.1f%%)\n",
            format(N_NEG_ROWS, big.mark = ","), format(nrow(names_only), big.mark = ","),
            100 * N_NEG_ROWS / nrow(names_only)))
cat(sprintf("  most negative cell: %d ; rows whose total is unchanged: %s of %s\n",
            WORST, format(SAME_TOTAL, big.mark = ","), format(nrow(m), big.mark = ",")))

# ---------------------------------------------------------------------------
# 4. The table BISG actually runs on, rebuilt from the 2020 counts
# ---------------------------------------------------------------------------
#
# Same six categories, same meaning, one vintage later. The 2020 release names
# them at greater length -- "NON-HISPANIC OR LATINO ASIAN AND NATIVE HAWAIIAN
# AND OTHER PACIFIC ISLANDER ALONE" is 2010's `pctapi` -- but the groupings are
# identical, so the columns below carry the 2010 names and every chapter
# downstream reads the file it always read.
#
# WHY MOVE. The census the probabilities are applied TO is the 2020 one: every
# block, every county and every voting-age population in this corpus comes from
# 2020. Weighting those with surname probabilities from 2010 asked Bayes' rule
# to combine two different decades and hoped the difference was small. It is
# not always small, and section 3 already showed why the 2020 file is the more
# honest of the two about its own small cells.
#
# WHAT IT COSTS. The 2020 file publishes counts that have been noised, and the
# noise is proportionally largest exactly where a count is smallest. The 2010
# file refused to publish those cells at all. Neither is a clean measurement;
# the difference is whether the uncertainty is visible or hidden, and the
# comparison written below makes it visible.

pct2020 <- pub[, c("name", "rank", "count")]
pct2020$pctwhite    <- 100 * pub$white    / pub$count
pct2020$pctblack    <- 100 * pub$black    / pub$count
pct2020$pctapi      <- 100 * pub$asian    / pub$count
pct2020$pctaian     <- 100 * pub$aian     / pub$count
pct2020$pct2prace   <- 100 * pub$twoplus  / pub$count
pct2020$pcthispanic <- 100 * pub$hispanic / pub$count
pct2020 <- pct2020[!is.na(pct2020$count) & pct2020$count > 0, ]
stopifnot(all(abs(rowSums(pct2020[, pctcols], na.rm = TRUE) - 100) < 0.5))
dd_write_csv(pct2020, "derived/census_surnames.csv")
cat("\nBISG table now 2020:", nrow(pct2020), "surnames,",
    sum(is.na(pct2020[, pctcols])), "missing cells\n")

# --- what the change does, measured rather than asserted --------------------
cmp <- merge(sn[, c("name", "count", pctcols)],
             pct2020[, c("name", "count", pctcols)],
             by = "name", suffixes = c("_2010", "_2020"))
shift <- do.call(rbind, lapply(pctcols, function(v) {
  d <- cmp[[paste0(v, "_2020")]] - cmp[[paste0(v, "_2010")]]
  data.frame(category = v, names_compared = sum(!is.na(d)),
             median_shift = median(d, na.rm = TRUE),
             mean_abs_shift = mean(abs(d), na.rm = TRUE),
             moved_over_5pt = sum(abs(d) > 5, na.rm = TRUE),
             stringsAsFactors = FALSE)
}))
dd_write_csv(shift, "derived/surnames_2010_vs_2020.csv")

# the names each vintage has and the other does not
only10 <- setdiff(sn$name, pct2020$name); only20 <- setdiff(pct2020$name, sn$name)
dd_write_csv(data.frame(
  key = c("names_2010", "names_2020", "in_both", "only_2010", "only_2020",
          "cells_missing_2010", "cells_missing_2020"),
  value = c(nrow(sn), nrow(pct2020), nrow(cmp), length(only10), length(only20),
            sum(is.na(sn[, pctcols])), sum(is.na(pct2020[, pctcols])))),
  "derived/surnames_vintage_facts.csv")
cat("  names in both:", nrow(cmp), "| only 2010:", length(only10),
    "| only 2020:", length(only20), "\n")
print(shift)

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
