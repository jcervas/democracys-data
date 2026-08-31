# ---------------------------------------------------------------------------
# Build the section-203 datasets.
#
# Two files end up in this folder:
#
#   derived/sect203_determined.csv   every jurisdiction the Census Bureau made a
#                            Section 203 determination about in 2021, with the
#                            underlying estimates and the coverage flags
#   derived/sect203_counties.csv     county-level rows for every county-language pair
#                            with any limited-English population at all --
#                            this is what makes the near-miss analysis possible
#
# Run this script from inside the data/ folder. It downloads a 4.3 MB zip that
# expands to about 120 MB, so it needs a network connection and a little
# patience. The committed outputs are much smaller.
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
# U.S. Census Bureau, Redistricting and Voting Rights Data Office,
# "2021 Section 203 Determinations -- Public Use File"
#   https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/voting-rights-determination-file.html
#   file: Sec203_PUF_2021_12_01.zip
#
# This is the actual arithmetic behind a legal determination. The Director of
# the Census publishes coverage determinations under 52 U.S.C. 10503, most
# recently on 8 December 2021 using 2015-2019 American Community Survey data.
# A covered jurisdiction must by law provide ballots and election materials in
# the covered language.
#
# The file carries margins of error alongside every estimate. That is unusual
# and it is the reason this lab exists: the thresholds in the statute are
# sharp, and the data they are applied to is not.

url <- paste0("https://www2.census.gov/programs-surveys/decennial/rdo/datasets/",
              "2021/2021_Section203-Determinations/Sec203_PUF_2021_12_01.zip")

zipf <- tempfile(fileext = ".zip")
prov_fetch(url, zipf, mode = "wb", quiet = TRUE)
exdir <- tempdir()
files <- unzip(zipf, exdir = exdir)
cat("extracted:\n"); print(basename(files))

keep <- c("SUMLVL", "LEVEL", "S203_GEOID", "ST", "CNTY", "NAMELSAD",
          "LANGUAGE", "VACIT", "MVACIT", "VACLEP", "MVACLEP",
          "ILLIT", "MILLIT", "LEPPCT", "MLEPPCT", "ILLRAT", "MILLRAT",
          "FLAG10", "FLAG5", "FLAG_EDU", "FLAG_AIAN", "FLAG_ANRC", "FLAG_COV")

as_num <- function(d) {
  for (v in c("VACIT","MVACIT","VACLEP","MVACLEP","ILLIT","MILLIT",
              "LEPPCT","MLEPPCT","ILLRAT","MILLRAT")) {
    d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
  }
  for (v in c("FLAG10","FLAG5","FLAG_EDU","FLAG_AIAN","FLAG_ANRC","FLAG_COV")) {
    d[[v]] <- ifelse(is.na(d[[v]]) | d[[v]] == "", 0L, as.integer(d[[v]]))
  }
  d
}

# --- 1. Determined areas ----------------------------------------------------

det <- read.csv(grep("Determined_Areas_Only", files, value = TRUE),
                stringsAsFactors = FALSE, colClasses = "character")
stopifnot(all(keep %in% names(det)))
det <- as_num(det[, keep])

cat("\ndetermined rows:", nrow(det), "\n")
cat("covered rows:", sum(det$FLAG_COV == 1),
    "across", length(unique(det$S203_GEOID[det$FLAG_COV == 1])),
    "distinct jurisdictions\n")
write.csv(det, "derived/sect203_determined.csv", row.names = FALSE)

# --- 2. All counties --------------------------------------------------------
#
# The 118 MB all-areas file covers nine geography levels. We keep counties,
# which is the level most students can reason about, and drop county-language
# pairs with no limited-English population at all -- those are the bulk of the
# rows and none of the interest.

all_f <- grep("All_Areas", files, value = TRUE)
cat("\nreading", basename(all_f), "-- this takes a moment\n")
alld <- read.csv(all_f, stringsAsFactors = FALSE, colClasses = "character")
cat("all rows:", nrow(alld), "\n")

cty <- alld[alld$LEVEL == "County", keep]
cty <- as_num(cty)
cty <- cty[!is.na(cty$VACLEP) & cty$VACLEP > 0, ]
cty <- cty[order(-cty$VACLEP), ]

cat("county-language pairs with a limited-English population:", nrow(cty), "\n")
cat("of which covered:", sum(cty$FLAG_COV == 1), "\n")
write.csv(cty, "derived/sect203_counties.csv", row.names = FALSE)

# --- 3. Report the two things the lab is built on ---------------------------

s10 <- cty[cty$VACLEP - cty$MVACLEP < 10000 & cty$VACLEP + cty$MVACLEP > 10000, ]
cat("\ncounties whose margin of error straddles the 10,000 threshold:",
    nrow(s10), "\n")
print(s10[, c("NAMELSAD", "LANGUAGE", "VACLEP", "MVACLEP", "FLAG10", "FLAG_COV")])

n5 <- cty[!is.na(cty$LEPPCT) & cty$LEPPCT - cty$MLEPPCT < 5 &
          cty$LEPPCT + cty$MLEPPCT > 5, ]
cat("\ncounties whose margin of error straddles the 5% threshold:", nrow(n5), "\n")

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
