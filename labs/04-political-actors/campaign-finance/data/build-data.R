# ---------------------------------------------------------------------------
# Build the campaign-finance dataset.
#
# One file ends up in this folder:
#
#   derived/fec_candidates_2024.csv   every federal candidate who filed with the FEC
#                             for the 2023-24 cycle (3,856 rows), with their
#                             committee's receipts, disbursements, and the
#                             sources those receipts came from
#
# Run this script from inside the data/ folder. It needs a network connection;
# the whole point of committing the output is that the lab does not.
#
# Nothing is edited by hand. In particular the duplicate candidate records are
# LEFT IN -- Part 6 of the lab is about them.
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
# Federal Election Commission, bulk download "All Candidates" file for the
# 2024 cycle:
#   https://www.fec.gov/files/bulk-downloads/2024/weball24.zip
#
# The file is pipe-delimited with NO header row. The column layout is published
# separately by the FEC ("all candidates file description"). If a future cycle
# changes the layout, the stopifnot() below will catch it.

url <- "https://www.fec.gov/files/bulk-downloads/2024/weball24.zip"
zipf <- tempfile(fileext = ".zip")
prov_fetch(url, zipf, mode = "wb", quiet = TRUE)
txt <- unzip(zipf, exdir = tempdir())
stopifnot(length(txt) == 1)

fec_cols <- c(
  "cand_id", "cand_name", "ici", "pty_cd", "party",
  "ttl_receipts", "trans_from_auth", "ttl_disb", "trans_to_auth",
  "coh_bop", "coh_cop", "cand_contrib", "cand_loans", "other_loans",
  "cand_loan_repay", "other_loan_repay", "debts_owed_by",
  "ttl_indiv_contrib", "office_st", "office_district",
  "spec_election", "prim_election", "run_election", "gen_election",
  "gen_election_pct", "other_pol_cmte_contrib", "pol_pty_contrib",
  "cvg_end_dt", "indiv_refunds", "cmte_refunds")

raw <- read.delim(txt, sep = "|", header = FALSE, quote = "",
                  col.names = fec_cols, stringsAsFactors = FALSE,
                  colClasses = c(office_district = "character"))

stopifnot(ncol(raw) == 30)
cat("downloaded", nrow(raw), "candidate records\n")

# --- Derive and trim --------------------------------------------------------
#
# Office is the first character of the candidate ID: H, S or P.

raw$office <- c(H = "House", S = "Senate", P = "President")[substr(raw$cand_id, 1, 1)]

# Money the candidate put in themselves, direct contributions plus loans.
raw$self_funding <- raw$cand_contrib + raw$cand_loans

out <- raw[, c("cand_id", "cand_name", "office", "office_st", "office_district",
               "ici", "party", "ttl_receipts", "ttl_disb", "ttl_indiv_contrib",
               "other_pol_cmte_contrib", "pol_pty_contrib", "self_funding",
               "cand_contrib", "cand_loans", "coh_cop", "debts_owed_by",
               "trans_from_auth")]

names(out)[names(out) == "other_pol_cmte_contrib"] <- "pac_contrib"
names(out)[names(out) == "ttl_indiv_contrib"]      <- "indiv_contrib"
names(out)[names(out) == "pol_pty_contrib"]        <- "party_contrib"
names(out)[names(out) == "coh_cop"]                <- "cash_on_hand"

out <- out[order(out$office, -out$ttl_receipts), ]

# --- Report what is in it, including what is wrong with it ------------------

cat("\nby office:\n"); print(table(out$office))
cat("\nby incumbency status (I incumbent, C challenger, O open seat):\n")
print(table(out$ici, useNA = "ifany"))

# The win/loss columns exist in the FEC layout and are completely empty. This
# is not an oversight in this script -- the FEC does not populate them. The
# lab makes a point of it.
cat("\ngen_election (win/loss) values present in source:",
    sum(!is.na(raw$gen_election) & raw$gen_election != ""), "of", nrow(raw), "\n")

# Duplicate records. A candidate who moves chamber mid-cycle, or whose committee
# is renamed, gets more than one candidate ID carrying the SAME committee money.
big <- out[out$ttl_receipts > 1e6, ]
dups <- table(big$ttl_receipts); dups <- dups[dups > 1]
cat("\nreceipt totals shared by 2+ candidate IDs (above $1M):",
    length(dups), "values covering", sum(dups), "rows\n")
cat("  -- these are LEFT IN deliberately; see Part 6 of the lab\n")

write.csv(out, "derived/fec_candidates_2024.csv", row.names = FALSE)
cat("\nwrote fec_candidates_2024.csv:", nrow(out), "rows,", ncol(out), "columns\n")

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
