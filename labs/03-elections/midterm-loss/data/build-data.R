# ---------------------------------------------------------------------------
# Build the midterm-loss dataset.
#
# One file ends up in this folder:
#
#   derived/house_midterms.csv   One row per House election from 1858 to the present.
#                        Columns: election_year, congress, seats, dem, rep,
#                        pres_party, midterm, pres_party_seats,
#                        pres_party_change.
#
# SOURCE. Office of the Historian, U.S. House of Representatives, "Party
# Divisions of the House of Representatives, 1789 to Present":
#   https://history.house.gov/Institution/Party-Divisions/Party-Divisions/
# This is the House's own count and is the authority on the question. It is a
# U.S. government publication.
#
# WHY NOT VOTEVIEW. The `dw-nominate` chapter already has a file with every House member's
# party by Congress, and counting rows there looks like it would give seat
# totals. It does not: members who die, resign or are replaced mid-term appear
# alongside their replacements, so the 119th Congress "has" 449 members in a
# 435-seat chamber. Seats and people are different units. This is the `data-sources`
# lesson in a new costume.
#
# ONE PARSING TRAP. The Historian's table footnotes some rows, and the footnote
# marker sits inside the cell. Stripping HTML tags concatenates the digits, so
# 435 with footnote 5 reads as "4355". Any seat count above 999 is repaired
# below by keeping the first three digits.
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


dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE, scipen = 999, timeout = 600)

url <- "https://history.house.gov/Institution/Party-Divisions/Party-Divisions/"
html_file <- file.path(tempdir(), "party-divisions.html")

if (!file.exists(html_file)) {
  cat("downloading the House Historian's table ...\n")
  # The site refuses requests without a browser user-agent.
  prov_fetch(url, html_file, quiet = TRUE, method = "curl",
             extra = '-L -A "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/122 Safari/537.36"')
}
raw <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

# --- parse the table --------------------------------------------------------
strip <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;|&#160;", " ", x)
  x <- gsub("&amp;", "&", x)
  trimws(gsub("\\s+", " ", x))
}
tr <- regmatches(raw, gregexpr("<tr[^>]*>.*?</tr>", raw))[[1]]

rows <- lapply(tr, function(r) {
  cells <- regmatches(r, gregexpr("<t[dh][^>]*>.*?</t[dh]>", r))[[1]]
  vapply(cells, strip, character(1), USE.NAMES = FALSE)
})

d_i <- r_i <- NA
out <- list()
for (c_ in rows) {
  if (length(c_) == 0) next
  if (grepl("^Congress", c_[1])) {                 # a header row: parties change by era
    d_i <- if ("Democrats"   %in% c_) match("Democrats",   c_) else NA
    r_i <- if ("Republicans" %in% c_) match("Republicans", c_) else NA
    next
  }
  m <- regmatches(c_[1], regexec("^(\\d+)\\w*\\s*\\((\\d{4})", c_[1]))[[1]]
  if (length(m) == 3 && !is.na(d_i) && !is.na(r_i) && length(c_) >= max(d_i, r_i)) {
    num <- function(x) suppressWarnings(as.integer(gsub("[^0-9]", "", x)))
    seats <- num(c_[2]); dem <- num(c_[d_i]); rep <- num(c_[r_i])
    if (any(is.na(c(seats, dem, rep)))) next
    if (seats > 999) seats <- as.integer(substr(as.character(seats), 1, 3))  # footnote trap
    out[[length(out) + 1]] <- data.frame(
      congress = as.integer(m[2]), start_year = as.integer(m[3]),
      seats = seats, dem = dem, rep = rep)
  }
}
h <- do.call(rbind, out)
h <- h[h$congress >= 36, ]          # 36th (1859-) onward: stable D vs R competition
h <- h[order(h$congress), ]
h$election_year <- h$start_year - 1

# --- president's party at the time of each election -------------------------
# Party of the sitting president in November of the election year.
pres <- rbind(
  data.frame(from = 1857, to = 1860, party = "D"),   # Buchanan
  data.frame(from = 1861, to = 1884, party = "R"),   # Lincoln .. Arthur
  data.frame(from = 1885, to = 1888, party = "D"),   # Cleveland
  data.frame(from = 1889, to = 1892, party = "R"),   # B. Harrison
  data.frame(from = 1893, to = 1896, party = "D"),   # Cleveland
  data.frame(from = 1897, to = 1912, party = "R"),   # McKinley .. Taft
  data.frame(from = 1913, to = 1920, party = "D"),   # Wilson
  data.frame(from = 1921, to = 1932, party = "R"),   # Harding .. Hoover
  data.frame(from = 1933, to = 1952, party = "D"),   # FDR .. Truman
  data.frame(from = 1953, to = 1960, party = "R"),   # Eisenhower
  data.frame(from = 1961, to = 1968, party = "D"),   # Kennedy, Johnson
  data.frame(from = 1969, to = 1976, party = "R"),   # Nixon, Ford
  data.frame(from = 1977, to = 1980, party = "D"),   # Carter
  data.frame(from = 1981, to = 1992, party = "R"),   # Reagan, G.H.W. Bush
  data.frame(from = 1993, to = 2000, party = "D"),   # Clinton
  data.frame(from = 2001, to = 2008, party = "R"),   # G.W. Bush
  data.frame(from = 2009, to = 2016, party = "D"),   # Obama
  data.frame(from = 2017, to = 2020, party = "R"),   # Trump
  data.frame(from = 2021, to = 2024, party = "D"),   # Biden
  data.frame(from = 2025, to = 2032, party = "R")    # Trump
)
h$pres_party <- NA_character_
for (i in seq_len(nrow(pres)))
  h$pres_party[h$election_year >= pres$from[i] & h$election_year <= pres$to[i]] <- pres$party[i]

# --- midterm flag and seat change ------------------------------------------
h$midterm <- h$election_year %% 4 != 0
h$pres_party_seats <- ifelse(h$pres_party == "D", h$dem, h$rep)

# Change in the PRESIDENT'S PARTY's seats from the previous election. The party
# is fixed by who sits in the White House at this election; we then track that
# one party's seat count across the two elections. (Do not require the previous
# election's president to be the same party -- that would throw out every
# midterm in a president's first term, which is most of them.)
h$dem_change <- c(NA, diff(h$dem))
h$rep_change <- c(NA, diff(h$rep))
h$pres_party_change <- ifelse(h$pres_party == "D", h$dem_change, h$rep_change)

keep <- c("election_year", "congress", "seats", "dem", "rep",
          "pres_party", "midterm", "pres_party_seats", "pres_party_change")
write.csv(h[, keep], "derived/house_midterms.csv", row.names = FALSE)
cat("wrote house_midterms.csv:", nrow(h), "elections,",
    min(h$election_year), "-", max(h$election_year), "\n\n")

# --- sanity checks against elections everybody knows ------------------------
mt <- h[h$midterm & !is.na(h$pres_party_change), ]
cat("Midterms in the file:", nrow(mt), "\n")
cat("President's party lost seats in", sum(mt$pres_party_change < 0), "of them",
    sprintf("(%.0f%%)\n", 100 * mean(mt$pres_party_change < 0)))
cat("Mean change:", round(mean(mt$pres_party_change), 1), "seats\n\n")
cat("Checks against the famous ones:\n")
for (y in c(1934, 1938, 1994, 2002, 2010, 2018)) {
  r <- h[h$election_year == y, ]
  if (nrow(r)) cat(sprintf("  %d  president %s  change %+d\n",
                           y, r$pres_party, r$pres_party_change))
}
cat("\nThe exceptions to look for: 1934, 1998, 2002.\n")

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
