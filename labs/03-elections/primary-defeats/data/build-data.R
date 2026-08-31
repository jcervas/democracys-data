# ---------------------------------------------------------------------------
# Build the data for the `primary-defeats` chapter.
#
# THERE IS NO SEPARATE BUILD FOR THIS CHAPTER, AND THAT IS DELIBERATE.
#
# `primary-defeats` and `retirements` are two questions about one event. A
# sitting member of the House either comes back after the next election or does
# not, and if not, the route out was retirement, or a lost primary, or a lost
# general election. Deciding which requires exactly one derivation: match the
# membership record to the candidate record and look at what happened on each
# ballot the member appeared on. Splitting that across two scripts would mean
# two copies of three hundred lines of the same parsing, the same name
# matching, and the same Louisiana exception -- and the two copies would drift,
# and the two chapters would then disagree with each other about a number they
# both computed from the same file.
#
# So the build lives in `../../retirements/data/build-data.R`, writes into both
# folders, and this file runs it. The header of that script documents every
# source, every URL, the fetch date and the row counts.
#
# Run from this directory:  Rscript build-data.R
#
# WHAT ARRIVES HERE
#
#   derived/incumbents.csv  one row per House incumbent per cycle, 2004-2022: whether
#                   they stood in a primary, how many rivals, their share of
#                   the primary vote, whether they were renominated
#   derived/by_year.csv     one row per cycle: incumbents, contested primaries,
#                   denials, and the distribution of incumbent primary shares
#   derived/denied.csv      the roster -- every member refused renomination
#   derived/compare.csv     this file's count of primary defeats beside the Brookings
#                   count and the Giroux count, cycle by cycle
#   derived/vsoc.csv        Brookings, Vital Statistics on Congress, Table 2-7,
#                   1946-2024
#   derived/giroux.csv      the Giroux tracker's per-year totals, 1966-2026
#   derived/checks.csv      the validation results
# ---------------------------------------------------------------------------

here <- normalizePath(".")
shared <- normalizePath(file.path("..", "..", "retirements", "data"),
                        mustWork = FALSE)
if (!file.exists(file.path(shared, "build-data.R")))
  stop("shared build not found at:\n  ", shared,
       "\nThis chapter is built by retirements/data/build-data.R.")

cat("running the shared build in", shared, "\n\n")
setwd(shared)
source("build-data.R")
setwd(here)
cat("\nback in", here, "\n")

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
