# ---------------------------------------------------------------------------
# Build the ga-precinct-returns dataset: Georgia precinct returns.
#
# NO GUESTBOOK. This lab used to depend on VEST via Harvard Dataverse. It now
# reads the Georgia Secretary of State's own publication -- the counties'
# tabulation exports -- parsed by `parse-ga-sos.py` in this folder.
#
#   Rscript ../data/... no. Run, in this folder:
#     python3 parse-ga-sos.py --xml "raw/November 3,2020-General Election" \
#                             --out-prefix ga2020_
#     python3 parse-ga-sos.py --summary "raw/November 3rd General Election Recount" \
#                             --out-prefix ga2020rc_
#     python3 parse-ga-sos.py --json-url --out-prefix ga2024_     # no download
#     Rscript build-data.R
#
# Produces:
#   derived/precincts.csv      one row per precinct, 2020 presidential
#   derived/counties.csv       county totals, precinct counts, size spread
#   derived/structure.csv      what the source contains
#   derived/recount.csv        original vs recount, by county   <-- the new one
#
# ---------------------------------------------------------------------------
# WHY THE RECOUNT IS COUNTY-LEVEL AND THE RETURNS ARE PRECINCT-LEVEL
#
# Georgia published the November 2020 count and the November 2020 RECOUNT as
# two separate archives. The original carries precinct detail for all 159
# counties. **The recount carries 159 county summaries and precinct detail for
# exactly one county (Lanier).**
#
# So the two can only be compared at county level. That is a fact about what
# the state published, and the lab says so rather than quietly using one file
# for both purposes.
#
# The comparison is worth having. The recount summary reproduces Georgia's
# CERTIFIED 2020 totals exactly -- Biden 2,473,633, Trump 2,461,854 -- while
# the original precinct file gives Biden 2,474,507 and Trump 2,461,837.
# **The same election, counted twice, 874 Biden votes apart.**
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

need <- c("derived/ga2020_precincts.csv", "derived/ga2020_counties.csv", "derived/ga2020_structure.csv")
miss <- need[!file.exists(need)]
if (length(miss)) {
  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("Missing:", paste(miss, collapse = ", "), "\n\n")
  cat("Run the parser first. See README.md. In this folder:\n")
  cat('  python3 parse-ga-sos.py --xml "raw/November 3,2020-General Election" --out-prefix ga2020_\n')
  cat(strrep("-", 70), "\n\n", sep = "")
  quit(status = 0)
}

pr <- read.csv("derived/ga2020_precincts.csv", stringsAsFactors = FALSE, check.names = FALSE)
co <- read.csv("derived/ga2020_counties.csv",  stringsAsFactors = FALSE, check.names = FALSE)
st <- read.csv("derived/ga2020_structure.csv", stringsAsFactors = FALSE)

cat(sprintf("precincts %s in %d counties\n", format(nrow(pr), big.mark = ","), nrow(co)))
stopifnot(nrow(co) == 159)

dd_write_csv(pr, "derived/precincts.csv")
dd_write_csv(co, "derived/counties.csv")
dd_write_csv(st, "derived/structure.csv")

# ---- original vs recount, by county --------------------------------------
if (file.exists("derived/ga2020rc_counties.csv")) {
  rc <- read.csv("derived/ga2020rc_counties.csv", stringsAsFactors = FALSE)
  rc <- rc[, c("county", "candidate", "votes")]
  names(rc)[3] <- "recount"
  long <- do.call(rbind, lapply(c("Joseph R. Biden", "Donald J. Trump"), function(cd) {
    if (!cd %in% names(co)) return(NULL)
    data.frame(county = co$county, candidate = cd, original = co[[cd]],
               stringsAsFactors = FALSE)
  }))
  cmp <- merge(long, rc, by = c("county", "candidate"))
  cmp$change <- cmp$recount - cmp$original
  cmp$pct_change <- round(100 * cmp$change / pmax(cmp$original, 1), 3)
  dd_write_csv(cmp[order(-abs(cmp$change)), ], "derived/recount.csv")

  tot <- aggregate(cbind(original, recount) ~ candidate, cmp, sum)
  tot$change <- tot$recount - tot$original
  cat("\noriginal count vs recount, statewide:\n")
  print(tot, row.names = FALSE)
  cat(sprintf("\ncounties where the count changed: %d of %d\n",
              length(unique(cmp$county[cmp$change != 0])), length(unique(cmp$county))))
  cat("largest movements:\n")
  print(head(cmp[order(-abs(cmp$change)), c("county","candidate","original","recount","change")], 6),
        row.names = FALSE)
} else {
  cat("\nga2020rc_counties.csv not present -- recount.csv not written.\n")
}

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
