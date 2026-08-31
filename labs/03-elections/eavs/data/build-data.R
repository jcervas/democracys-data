# ---------------------------------------------------------------------------
# Build the eavs dataset: election administration (EAVS).
#
# Four small files end up in this folder, all aggregate:
#
#   derived/national.csv     Key national totals, each with the number of jurisdictions
#                    that actually reported it.
#   derived/reasons.csv      All 20 published reasons a mail ballot can be rejected,
#                    with totals and how many jurisdictions reported each.
#   derived/states.csv       One row per state: jurisdictions, voters, mail and
#                    provisional ballots.
#   derived/units.csv        Jurisdictions per state -- the unit-of-observation problem.
#
# SOURCE. U.S. Election Assistance Commission, Election Administration and
# Voting Survey, 2024, **Version 2** (released 12 February 2026). 6,461
# jurisdictions, 535 columns.
#
#   data     https://www.eac.gov/sites/default/files/2026-02/2024_EAVS_for_Public_Release_nolabel_V2_csv.zip
#   codebook https://www.eac.gov/sites/default/files/2025-06/2024_EAVS_Codebook.xlsx
#   errata   https://www.eac.gov/sites/default/files/2026-02/Errata_Note_2024_EAVS_v2.pdf
#
# USE V2, NOT V1. Version 1 was corrected; the errata note above lists what
# changed. A lab built on V1 would disagree with the EAC's own report.
#
# THREE THINGS THAT WILL BITE ANYONE WHO USES THIS FILE
#
#  1. MISSING IS NEGATIVE, NOT BLANK. Non-response is coded -99, and -88 marks
#     items that do not apply. sum() on a raw column silently mixes real counts
#     with these codes. Every total below goes through drop_missing().
#
#  2. THE FILE HAS EMBEDDED NEWLINES. Comment fields contain line breaks, so
#     counting lines gives 17,013 for a file with 6,461 rows. read.csv parses
#     it correctly; `wc -l` does not.
#
#  3. "JURISDICTION" IS NOT ONE THING. It is a county in most states and a
#     township in New England, Wisconsin and Michigan. Wisconsin reports 1,851
#     jurisdictions; Texas reports 254. Any per-jurisdiction average is
#     comparing townships to counties.
#
# Run from this directory:  Rscript build-data.R
# (Downloads ~2 MB and expands to 41 MB in tempdir; nothing large is kept.)
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
csv <- unzip(tmp, exdir = tempdir())
e <- read.csv(csv[1], stringsAsFactors = FALSE, check.names = FALSE)

stopifnot(nrow(e) == 6461)

# -99 = not reported, -88 = not applicable. Both must go before any sum().
drop_missing <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.na(x) & x < 0] <- NA
  x
}
total    <- function(k) sum(drop_missing(e[[k]]), na.rm = TRUE)
reported <- function(k) sum(!is.na(drop_missing(e[[k]])))

# --- national totals --------------------------------------------------------
items <- rbind(
  c("F1a", "Total voters"),
  c("D1a", "Total precincts"),
  c("C1a", "Mail ballots transmitted to voters"),
  c("C1b", "Mail ballots returned by voters"),
  c("C1c", "Mail ballots returned undeliverable"),
  c("C9a", "Mail ballots rejected"),
  c("E1a", "Provisional ballots cast"),
  c("E1b", "Provisional ballots counted in full"),
  c("E1d", "Provisional ballots rejected"))

national <- data.frame(
  variable = items[, 1], item = items[, 2],
  total    = vapply(items[, 1], total, 0, USE.NAMES = FALSE),
  reported_by = vapply(items[, 1], reported, 0L, USE.NAMES = FALSE),
  stringsAsFactors = FALSE)
national$pct_reporting <- round(100 * national$reported_by / nrow(e), 1)
write.csv(national, "derived/national.csv", row.names = FALSE)

# --- every published reason a mail ballot is rejected -----------------------
rc <- c(C9b = "Received after the deadline",
        C9c = "Voter signature missing",
        C9d = "Witness signature missing",
        C9e = "Signature did not match",
        C9f = "Unofficial envelope",
        C9g = "Ballot missing from envelope",
        C9h = "No secrecy envelope",
        C9i = "Multiple ballots in one envelope",
        C9j = "Envelope not sealed",
        C9k = "No postmark",
        C9l = "No resident address on envelope",
        C9m = "Voter deceased",
        C9n = "Voter already voted",
        C9o = "Missing documentation",
        C9p = "Voter not eligible",
        C9q = "No ballot application on file",
        C9r = "Other (1)",
        C9s = "Other (2)",
        C9t = "Other (3)")

reasons <- data.frame(
  variable = names(rc), reason = unname(rc),
  ballots  = vapply(names(rc), total, 0, USE.NAMES = FALSE),
  reported_by = vapply(names(rc), reported, 0L, USE.NAMES = FALSE),
  stringsAsFactors = FALSE)
reasons$pct_of_rejected <- round(100 * reasons$ballots / total("C9a"), 1)
reasons <- reasons[order(-reasons$ballots), ]
write.csv(reasons, "derived/reasons.csv", row.names = FALSE)

# --- states -----------------------------------------------------------------
st_sum <- function(k) tapply(drop_missing(e[[k]]), e$State_Abbr, sum, na.rm = TRUE)
states <- data.frame(
  state         = sort(unique(e$State_Abbr)),
  jurisdictions = as.vector(table(e$State_Abbr)),
  voters        = as.vector(st_sum("F1a")),
  mail_sent     = as.vector(st_sum("C1a")),
  mail_returned = as.vector(st_sum("C1b")),
  mail_rejected = as.vector(st_sum("C9a")),
  prov_cast     = as.vector(st_sum("E1a")),
  prov_rejected = as.vector(st_sum("E1d")),
  stringsAsFactors = FALSE)
write.csv(states, "derived/states.csv", row.names = FALSE)

# --- the unit problem -------------------------------------------------------
units <- states[, c("state", "jurisdictions", "voters")]
units$voters_per_jurisdiction <- round(units$voters / units$jurisdictions)
units <- units[order(units$voters_per_jurisdiction), ]
write.csv(units, "derived/units.csv", row.names = FALSE)

cat(sprintf("jurisdictions: %d in %d states and territories\n",
            nrow(e), length(unique(e$State_Abbr))))
cat(sprintf("mail rejected: %s of %s returned (%.2f%%)\n",
            format(total("C9a"), big.mark = ","),
            format(total("C1b"), big.mark = ","),
            100 * total("C9a") / total("C1b")))
cat(sprintf("provisional rejected: %.1f%%\n", 100 * total("E1d") / total("E1a")))
cat(sprintf("reasons sum to %s against a stated total of %s\n",
            format(sum(reasons$ballots), big.mark = ","),
            format(total("C9a"), big.mark = ",")))

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
