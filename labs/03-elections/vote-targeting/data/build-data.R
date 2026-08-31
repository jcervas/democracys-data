# ---------------------------------------------------------------------------
# Build the vote-targeting dataset.
#
# One file ends up in this folder:
#
#   derived/nc_gov_county.csv   North Carolina governor's race, by county, for the four
#                       general elections 2012-2024. Columns: year, county,
#                       dem, rep, oth, total.
#
# WHY THIS RACE. Sides et al. Ch. 5 works a vote-targeting example on exactly
# these four elections (Tables 5.1-5.3). We rebuild it from the source they
# cite -- the NC State Board of Elections -- rather than retyping their table,
# which is the whole habit this course is trying to instil. It also lets us
# check their arithmetic, and it does not survive the check (see below).
#
# SOURCE. NCSBE publishes precinct-level results for every general election:
#   https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/<date>/results_pct_<yyyymmdd>.zip
# Landing page:
#   https://www.ncsbe.gov/results-data/election-results/historical-election-results-data
#
# FORMAT WARNING. 2012 is a quoted CSV with lowercase headers; 2016, 2020 and
# 2024 are tab-delimited with a different column set. Any script that assumes
# one layout silently returns zero rows for the other years. This is the `data-sources`
# lesson arriving again in a different costume.
#
# Run from this directory:  Rscript build-data.R      (needs internet, ~110 MB)
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

options(stringsAsFactors = FALSE, scipen = 999, timeout = 900)

years <- data.frame(
  year = c(2012, 2016, 2020, 2024),
  dir  = c("2012_11_06", "2016_11_08", "2020_11_03", "2024_11_05"),
  stamp= c("20121106", "20161108", "20201103", "20241105")
)

get_one <- function(i) {
  y <- years[i, ]
  txt <- file.path(tempdir(), paste0("results_pct_", y$stamp, ".txt"))
  if (!file.exists(txt)) {
    url <- sprintf("https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/%s/results_pct_%s.zip",
                   y$dir, y$stamp)
    zp <- file.path(tempdir(), paste0(y$stamp, ".zip"))
    cat("downloading", y$year, "...\n")
    prov_fetch(url, zp, mode = "wb", quiet = TRUE)
    unzip(zp, exdir = tempdir())
  }

  if (y$year == 2012) {
    x <- read.csv(txt, check.names = FALSE)
    names(x) <- tolower(trimws(names(x)))
    x <- x[toupper(x$contest) == "NC GOVERNOR", ]
    out <- data.frame(year = y$year, county = toupper(trimws(x$county)),
                      party = toupper(trimws(x$party)),
                      votes = as.numeric(x[["total votes"]]))
  } else {
    x <- read.delim(txt, check.names = FALSE, quote = "")
    names(x) <- tolower(trimws(names(x)))
    x <- x[toupper(trimws(x$`contest name`)) == "NC GOVERNOR", ]
    out <- data.frame(year = y$year, county = toupper(trimws(x$county)),
                      party = toupper(trimws(x$`choice party`)),
                      votes = as.numeric(x$`total votes`))
  }
  out$votes[is.na(out$votes)] <- 0
  out
}

all <- do.call(rbind, lapply(seq_len(nrow(years)), get_one))
all$grp <- ifelse(all$party == "DEM", "dem", ifelse(all$party == "REP", "rep", "oth"))

cty <- aggregate(votes ~ year + county + grp, all, sum)
w <- reshape(cty, idvar = c("year", "county"), timevar = "grp", direction = "wide")
names(w) <- sub("votes\\.", "", names(w))
for (v in c("dem", "rep", "oth")) if (is.null(w[[v]])) w[[v]] <- 0
w[is.na(w)] <- 0
w$total <- w$dem + w$rep + w$oth
w <- w[order(w$year, w$county), c("year", "county", "dem", "rep", "oth", "total")]

stopifnot(nrow(w) == 400, all(table(w$year) == 100))
write.csv(w, "derived/nc_gov_county.csv", row.names = FALSE)
cat("wrote nc_gov_county.csv:", nrow(w), "rows,", length(unique(w$county)), "counties\n\n")

# --- Check the textbook's arithmetic ---------------------------------------
st <- aggregate(cbind(dem, rep, oth) ~ year, w, sum)
cat("Statewide totals from the source data:\n")
print(st)

cat("\nAgainst Sides et al. Ch. 5:\n")
cat("  2016 Republican -- Table 5.2 says 2,298,880; Table 5.3 says 2,289,880.\n")
cat("                     Source data says", format(st$rep[st$year == 2016], big.mark = ","),
    "-> Table 5.3 has a\n                     transposed digit.\n")
cat("  2020 Republican -- book says 2,586,605; source data says",
    format(st$rep[st$year == 2020], big.mark = ","), "(off by one).\n")
cat("\nThe five counties the book prints reproduce exactly. Two typos in a\n")
cat("published table is not a scandal -- it is why you check.\n")

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
