# ---------------------------------------------------------------------------
# Build the residual-vote dataset, 2024.
#
#   derived/counties.csv   One row per county that joins: ballots cast, presidential
#                  votes, residual votes, residual rate.
#   derived/states.csv     State rollups, and how many counties joined.
#   derived/anomalies.csv  Counties where the arithmetic is impossible.
#
# WHAT A RESIDUAL VOTE IS. Subtract the votes cast in the top race from the
# ballots cast. What is left is a ballot that came out of the machine without a
# valid presidential vote on it -- left blank (an undervote), filled in twice
# (an overvote), or marked in a way the scanner would not read. It is the
# oldest measure of how well the voting equipment and the ballot design worked,
# and it is what the Caltech/MIT project used after Florida 2000.
#
# NOBODY PUBLISHES IT. It has to be built from two sources that were never
# meant to meet:
#
#   BALLOTS CAST      EAVS 2024, item F1a, by jurisdiction (U.S. Election
#                     Assistance Commission).
#   PRESIDENTIAL VOTES County returns already used in this course's first lab.
#
# THE JOIN ONLY WORKS WHERE A JURISDICTION IS A COUNTY. In Wisconsin, Michigan
# and most of New England, EAVS reports townships, so their rows are summed to
# the county FIPS prefix -- which is right when townships nest inside counties
# and wrong when they do not. Where the join fails entirely, the county drops
# out. 3,028 of 3,160 counties survive.
#
# WHAT COMES OUT. A median county residual rate of about 0.9% -- roughly one
# ballot in a hundred carries no valid presidential vote.
#
# AND AN IMPOSSIBILITY. In dozens of counties, EAVS reports FEWER ballots cast
# than there were presidential votes counted. That cannot happen: you cannot
# cast more presidential votes than ballots. Those counties are written to
# derived/anomalies.csv rather than dropped quietly, because a disagreement between two
# federal sources about the same election is the most interesting thing in the
# file.
#
# SOURCES
#   EAVS 2024 V2  https://www.eac.gov/sites/default/files/2026-02/2024_EAVS_for_Public_Release_nolabel_V2_csv.zip
#   County returns: ../../data-sources/data/derived/pres2024_counties.csv
#
# Run from this directory:  Rscript build-data.R   (downloads ~2 MB)
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

URL <- paste0("https://www.eac.gov/sites/default/files/2026-02/",
              "2024_EAVS_for_Public_Release_nolabel_V2_csv.zip")

tmp <- tempfile(fileext = ".zip")
prov_fetch(URL, tmp, mode = "wb", quiet = TRUE)
e <- read.csv(unzip(tmp, exdir = tempdir())[1], stringsAsFactors = FALSE,
              check.names = FALSE)
stopifnot(nrow(e) == 6461)

# -99 not reported, -88 not applicable
ballots <- suppressWarnings(as.numeric(e$F1a))
ballots[!is.na(ballots) & ballots < 0] <- NA

e$fips5 <- substr(sprintf("%010s", trimws(e$FIPSCode)), 1, 5)
k <- !is.na(ballots)
agg <- tapply(ballots[k], e$fips5[k], sum)
eavs <- data.frame(fips = names(agg), ballots = as.vector(agg),
                   stringsAsFactors = FALSE)

pres <- read.csv(file.path("..", "..", "data-sources", "data", "derived",
                           "pres2024_counties.csv"), stringsAsFactors = FALSE)
pres$fips <- sprintf("%05d", as.integer(pres$county_fips))

m <- merge(pres[, c("state_name", "county_name", "fips", "total_votes")],
           eavs, by = "fips")
m$residual      <- m$ballots - m$total_votes
m$residual_rate <- round(100 * m$residual / m$ballots, 3)

cat(sprintf("counties in returns: %d   joined to EAVS: %d\n", nrow(pres), nrow(m)))

# --- the impossible ones ----------------------------------------------------
bad <- m[m$residual < 0, ]
bad <- bad[order(bad$residual_rate), ]
write.csv(bad[, c("state_name", "county_name", "fips", "ballots",
                  "total_votes", "residual", "residual_rate")],
          "derived/anomalies.csv", row.names = FALSE)

good <- m[m$residual >= 0, ]

# --- flag states where the measure itself breaks ---------------------------
# EAVS item F1a is meant to be BALLOTS CAST. Some states appear to report
# something else, and it shows up as a residual rate that cannot be right.
# These are identified from the data, not from a list, and are excluded from
# headline figures but kept in counties.csv with a flag.
med <- tapply(good$residual_rate, good$state_name, median)
allzero <- tapply(good$residual_rate, good$state_name, function(x) all(x == 0))
broken <- names(med)[med > 3 | med < 0.05 | allzero]
good$state_usable <- !(good$state_name %in% broken)
cat("\nstates where the measure breaks:", paste(broken, collapse = ", "), "\n")
write.csv(data.frame(state = broken,
                     median_rate = round(as.vector(med[broken]), 2),
                     reason = ifelse(as.vector(allzero[broken]), "every county exactly 0",
                                     "median rate implausible")),
          "derived/unusable_states.csv", row.names = FALSE)

# --- states -----------------------------------------------------------------
write.csv(good[, c("state_name", "county_name", "fips", "ballots",
                   "total_votes", "residual", "residual_rate", "state_usable")],
          "derived/counties.csv", row.names = FALSE)

u <- good[good$state_usable, ]
sb <- tapply(good$ballots,     good$state_name, sum)
sv <- tapply(good$total_votes, good$state_name, sum)
sn <- table(good$state_name)
st <- data.frame(state = names(sb), counties = as.vector(sn),
                 ballots = as.vector(sb), pres_votes = as.vector(sv),
                 stringsAsFactors = FALSE)
st$residual_rate <- round(100 * (st$ballots - st$pres_votes) / st$ballots, 3)
st <- st[order(-st$residual_rate), ]
write.csv(st, "derived/states.csv", row.names = FALSE)

cat(sprintf("usable counties: %d   impossible: %d\n", nrow(good), nrow(bad)))
cat(sprintf("usable states: %d of %d   usable counties: %d\n",
            length(unique(u$state_name)), length(unique(good$state_name)), nrow(u)))
cat(sprintf("median county residual rate (usable states): %.2f%%\n",
            median(u$residual_rate)))
cat(sprintf("across usable states: %s ballots, %s presidential votes, %.2f%% residual\n",
            format(sum(u$ballots), big.mark = ","),
            format(sum(u$total_votes), big.mark = ","),
            100 * (sum(u$ballots) - sum(u$total_votes)) / sum(u$ballots)))
cat("\nhighest and lowest states:\n")
print(head(st[, c("state", "counties", "residual_rate")], 4), row.names = FALSE)
print(tail(st[, c("state", "counties", "residual_rate")], 3), row.names = FALSE)

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
