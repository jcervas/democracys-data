# ---------------------------------------------------------------------------
# Build the campaign-finance-instrument dataset: filed, not audited.
#
# The last SOURCE chapter, after surveys, the census, voter files, election
# returns and roll calls. Its subject is the instrument: what a disclosure
# regime records, and the one structural fact that separates it from every
# other source in this book.
#
# Four files end up in this folder:
#
#   derived/filed.csv      What the independent-expenditure file says before and after
#                  sixteen rows are removed. The conclusion reverses.
#   derived/outliers.csv   Those sixteen rows, named.
#   derived/distribution.csv  What the candidate file looks like, and why its average
#                  is useless.
#   derived/regime.csv     Who must file what, and what is not filed at all.
#
# THE ARGUMENT. Every other source in this book is a record made ABOUT its
# subject by somebody else. A census counts you. A state records that you
# voted. A clerk records how a member voted. Campaign finance data is different
# in a way that is easy to miss because it arrives from a federal agency in a
# federal format:
#
#   THE SUBJECT OF THE RECORD WRITES THE RECORD.
#
# Committees file reports about themselves. The Federal Election Commission
# receives them, publishes them, and does not verify them before publication.
# Disclosure is a legal obligation to report; it is not an audit, and the
# distinction is not academic.
#
# HOW NOT ACADEMIC. The 2024 independent-expenditure bulk file, taken as it
# arrives, says that $46.9 BILLION was spent SUPPORTING candidates and $2.7
# billion opposing them -- 94.5% supportive. Sixteen rows account for $44.9
# billion of that. They are filed by committees named THE COURT OF DIVINE
# JUSTICE and THE COMMITTEE OF 300, in amounts like $9,978,412,568, on behalf
# of a candidate who is not a national figure. Remove those sixteen rows and
# the same file says spending was 57.6% OPPOSING.
#
# The junk does not merely inflate a total. IT REVERSES THE FINDING. And it is
# not hidden: it sits in the official bulk download, published by the agency,
# available to anybody who does not look.
#
# WHY THE AVERAGE IS USELESS. The candidate file contains everyone who filed,
# including 986 candidates who raised nothing at all. Mean receipts are
# $1,775,409 and median receipts are $21,200 -- a ratio of about 84. The top
# one per cent of candidates account for 59% of all money raised. "The average
# candidate raised $1.8 million" is arithmetically true and describes nobody.
#
# WHAT IS NOT IN THE FILE AT ALL. Disclosure has thresholds and boundaries, and
# what falls outside them is not merely unreported -- it is invisible in the
# same way an unasked survey question is invisible:
#   - contributions from a person aggregating $200 or less in an election
#     cycle are not itemised, so small donors appear only as a lump sum;
#   - spending by 501(c)(4) organisations need not disclose donors at all;
#   - and the boundary between coordinated and independent spending is a legal
#     question, not a fact recorded in the data.
#
# SOURCES. Read from files the corpus already built, so the provenance is a
# chapter rather than a URL:
#   ../../independent-expenditures/data/derived/ie_summary.csv, ie_outliers.csv
#     -- FEC bulk independent_expenditure_2024.csv
#   ../../campaign-finance/data/derived/fec_candidates_2024.csv
#     -- FEC bulk weball24.zip
# Nothing is fetched. Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

IE <- "../../independent-expenditures/data"
CF <- "../../campaign-finance/data"
stopifnot(file.exists(file.path(IE, "derived/ie_summary.csv")),
          file.exists(file.path(IE, "derived/ie_outliers.csv")),
          file.exists(file.path(CF, "derived/fec_candidates_2024.csv")))

# --- 1. before and after sixteen rows ---------------------------------------

s <- read.csv(file.path(IE, "derived/ie_summary.csv"))
w <- reshape(s[, c("version", "side", "dollars", "pct")],
             idvar = "version", timevar = "side", direction = "wide")
names(w) <- sub("\\.", "_", names(w))
fil <- data.frame(
  the_file = c("As it arrives from the FEC", "After sixteen rows are removed"),
  supporting = w$dollars_support[match(c("raw", "cleaned"), w$version)],
  opposing   = w$dollars_oppose[match(c("raw", "cleaned"), w$version)],
  pct_supporting = w$pct_support[match(c("raw", "cleaned"), w$version)],
  pct_opposing   = w$pct_oppose[match(c("raw", "cleaned"), w$version)])
fil$reads_as <- ifelse(fil$pct_supporting > 50,
                       "mostly money FOR candidates",
                       "mostly money AGAINST candidates")
write.csv(fil, "derived/filed.csv", row.names = FALSE)

# --- 2. the sixteen rows ----------------------------------------------------

o <- read.csv(file.path(IE, "derived/ie_outliers.csv"))
o <- o[order(-o$amount), ]
out <- data.frame(spender = o$spender, on_behalf_of = o$cand_name,
                  amount = o$amount, support_or_oppose = o$sup_opp)
write.csv(out, "derived/outliers.csv", row.names = FALSE)

# --- 3. the shape of the money ----------------------------------------------

d <- read.csv(file.path(CF, "derived/fec_candidates_2024.csv"))
r <- d$ttl_receipts[!is.na(d$ttl_receipts)]
rs <- sort(r, decreasing = TRUE)
dist <- data.frame(
  quantity = c("Candidates in the file",
               "Candidates who raised nothing at all",
               "Mean receipts",
               "Median receipts",
               "Ratio of mean to median",
               "Share of all money raised by the top 1% of candidates",
               "Share raised by the top 10%"),
  value = c(length(r), sum(r == 0), round(mean(r)), round(median(r)),
            round(mean(r) / median(r), 1),
            round(100 * sum(rs[1:ceiling(length(rs) * 0.01)]) / sum(rs), 1),
            round(100 * sum(rs[1:ceiling(length(rs) * 0.10)]) / sum(rs), 1)),
  unit = c("count", "count", "dollars", "dollars", "ratio", "%", "%"))
write.csv(dist, "derived/distribution.csv", row.names = FALSE)

# --- 4. the regime ----------------------------------------------------------
#
# Hand-authored from the statute and the FEC's own filing requirements. Every
# row names what is recorded and what the same rule leaves out.

reg <- data.frame(
  who = c("Candidate committee",
          "Political action committee",
          "Super PAC (independent-expenditure-only committee)",
          "Party committee",
          "501(c)(4) social welfare organisation"),
  must_report = c(
    "Receipts, disbursements, and each contributor over the itemisation threshold",
    "The same, plus contributions made to candidates",
    "Receipts and independent expenditures, including donors",
    "Receipts, disbursements, and coordinated spending",
    "Independent expenditures only, if it makes them"),
  what_is_invisible = c(
    "Contributions aggregating $200 or less -- reported only as a lump sum",
    "The same threshold applies",
    "Nothing about coordination, which is a legal question the data cannot answer",
    "The line between coordinated and independent spending",
    "Its donors -- who need not be disclosed at all"),
  verified_before_publication = rep("No", 5))
write.csv(reg, "derived/regime.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

usd <- function(x) paste0("$", format(round(x / 1e9, 2), big.mark = ","), "b")
cat("\nfiled.csv    : the same file, two ways\n")
print(data.frame(file = fil$the_file,
                 supporting = usd(fil$supporting),
                 opposing = usd(fil$opposing),
                 pct_supporting = fil$pct_supporting,
                 reads_as = fil$reads_as), row.names = FALSE)
cat(sprintf("\n  %d rows, %s, reverse the conclusion.\n",
            nrow(out), usd(sum(out$amount))))
cat("\noutliers.csv : the largest of them\n")
print(head(out, 4), row.names = FALSE)
cat("\ndistribution.csv : the shape of the money\n")
print(dist, row.names = FALSE)
cat(sprintf("\n  Mean receipts are %.0f times the median.\n",
            dist$value[dist$quantity == "Ratio of mean to median"]))
cat("\nregime.csv   :", nrow(reg), "kinds of filer; none verified before publication\n")

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
