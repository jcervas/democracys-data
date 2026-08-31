# ---------------------------------------------------------------------------
# Build the validated-turnout dataset: what a survey says about turnout, and what the
# ballots say.
#
# One file ends up in this folder:
#
#   derived/cps_turnout.csv   One row per presidential election, 1964-2024.
#                     CPS reported turnout, derived counts, actual votes cast,
#                     and the gap between them. Plus reported turnout by race.
#
# SOURCES.
#   1. U.S. Census Bureau, Current Population Survey, Voting and Registration
#      Supplement, Table A-1 (historical time series):
#      https://www2.census.gov/programs-surveys/cps/tables/time-series/
#        voting-historical-time-series/hst_vote01.xlsx
#      This is a SURVEY. Roughly 60,000 households are asked, after the
#      election, whether they voted. Nobody checks.
#   2. Actual votes cast for president: the national file already built for the
#      `historical-campaigns` (`../../../03-elections/historical-campaigns/data/derived/pres_national.csv`).
#      These are counted ballots.
#
# WHAT THE TABLE GIVES AND WHAT IT DOES NOT. Table A-1 publishes percentages
# and one count (total voting-age population, in thousands). It does not
# publish the number of people who said they voted. We derive it:
#
#   reported voters = VAP x (percent voted, total-population basis)
#   citizen VAP     = VAP x (pct total basis) / (pct citizen basis)
#
# Both are arithmetic on the Bureau's own published figures, not estimates of
# ours -- but they inherit whatever the Bureau's weighting did.
#
# TWO HONEST CAVEATS, which the lab makes students confront:
#   * "Actual votes" here are votes cast FOR PRESIDENT. Total ballots cast are
#     slightly higher, because some people vote and skip the top of the ticket.
#     That makes the gap below a slight OVERSTATEMENT of over-reporting.
#   * The denominator (citizen VAP) comes from the CPS itself. We are partly
#     checking the survey against arithmetic derived from the same survey.
#
# DEPENDENCY. This script needs `readxl` to open the Bureau's .xlsx. That is an
# instructor-side dependency only -- the lab itself reads the committed CSV and
# uses base R, like every other lab in the course.
#
# Run from this directory:  Rscript build-data.R      (needs internet)
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


source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE, scipen = 999, timeout = 600)
if (!requireNamespace("readxl", quietly = TRUE))
  stop("install.packages('readxl') first -- build script only, not the lab")

url <- paste0("https://www2.census.gov/programs-surveys/cps/tables/time-series/",
              "voting-historical-time-series/hst_vote01.xlsx")
xls <- file.path(tempdir(), "hst_vote01.xlsx")
if (!file.exists(xls)) {
  cat("downloading CPS Table A-1 ...\n")
  prov_fetch(url, xls, mode = "wb", quiet = TRUE, method = "curl",
             extra = '-L -A "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/122 Safari/537.36"')
}

x <- readxl::read_excel(xls, sheet = 1, col_names = FALSE, .name_repair = "minimal")

# The sheet stacks several blocks (Total voted, Total registered, then by age).
# We want the first block only: rows below the "Total, Percent Voted" label,
# stopping at the next label.
lab   <- as.character(unlist(x[, 1]))
start <- which(lab == "Total, Percent Voted")[1] + 1
stop_ <- which(!is.na(lab) & !grepl("^[0-9]{4}$", lab) & seq_along(lab) > start)[1] - 1

blk <- x[start:stop_, c(1, 2, 3, 4, 6, 8, 10, 12, 14)]
names(blk) <- c("year", "vap_thousands", "pct_total_basis", "pct_citizen_basis",
                "pct_white", "pct_white_nh", "pct_black", "pct_asian", "pct_hispanic")
blk <- data.frame(lapply(blk, function(c_) suppressWarnings(as.numeric(as.character(c_)))))
blk <- blk[!is.na(blk$year), ]

blk$reported_voters <- blk$vap_thousands * blk$pct_total_basis * 10
blk$citizen_vap     <- blk$vap_thousands * 1000 * blk$pct_total_basis / blk$pct_citizen_basis

# --- actual ballots for president -------------------------------------------
pres_file <- file.path("..", "..", "historical-campaigns", "data", "derived",
                       "pres_national.csv")
stopifnot(file.exists(pres_file))
pres <- read.csv(pres_file)
actual <- aggregate(pop_votes ~ year, pres, sum)
names(actual)[2] <- "actual_votes"

d <- merge(blk, actual, by = "year")          # presidential years only
d$cps_rate    <- 100 * d$reported_voters / d$citizen_vap
d$actual_rate <- 100 * d$actual_votes    / d$citizen_vap
d$over_report_pp <- d$cps_rate - d$actual_rate
d <- d[order(-d$year), ]

keep <- c("year", "vap_thousands", "pct_citizen_basis", "reported_voters",
          "citizen_vap", "actual_votes", "cps_rate", "actual_rate",
          "over_report_pp", "pct_white_nh", "pct_black", "pct_asian",
          "pct_hispanic")
dd_write_csv(d[, keep], "derived/cps_turnout.csv")
cat("wrote cps_turnout.csv:", nrow(d), "presidential elections,",
    min(d$year), "-", max(d$year), "\n\n")

recent <- d[d$year >= 1980, ]
cat("Mean over-report, 1980-2024:", round(mean(recent$over_report_pp), 1), "points\n")
cat("Years the survey UNDER-reported:",
    paste(recent$year[recent$over_report_pp < 0], collapse = ", "), "\n\n")
print(data.frame(year = recent$year,
                 cps  = round(recent$cps_rate, 1),
                 actual = round(recent$actual_rate, 1),
                 gap  = round(recent$over_report_pp, 1)))
cat("\nThe gap is large in the 1980s and nearly gone by 2020. That trend is the lab.\n")

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
