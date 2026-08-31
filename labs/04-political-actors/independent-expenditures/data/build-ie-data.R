# ---------------------------------------------------------------------------
# Build the independent-expenditures dataset: independent expenditures, 2024.
#
# An independent expenditure is money spent FOR or AGAINST a candidate by
# somebody the candidate does not control and is legally forbidden from
# coordinating with. It is reported to the FEC separately from the campaign's
# own money, and it is frequently larger.
#
# Four files end up in this folder:
#
#   derived/ie_summary.csv     Totals, raw and cleaned, split support vs oppose.
#   derived/ie_outliers.csv    The 16 rows that break the file. Committed on purpose.
#   derived/ie_committees.csv  Top spending committees, cleaned.
#   derived/ie_candidates.csv  The 30 most-targeted candidates, with support and
#                              oppose separated. A ranking, and cut at 30 -- the
#                              build reports, every run, how much money the cut
#                              hides in variant spellings of the names it keeps.
#
# Dollar columns are rounded to the cent; see MONEY IS ROUNDED, below.
#
# ---------------------------------------------------------------------------
# WHY THIS LAB EXISTS, AND WHAT IT FOUND
#
# The raw file totals $49.7 BILLION. Real 2024 independent expenditure was
# about $4.75 billion. **Sixteen rows out of 73,449 account for 90.4% of the
# raw total**, and they are of two completely different kinds:
#
#   JUNK FILINGS. Six rows, $43.6bn, filed by committees named "THE COMMITTEE
#   OF 300", "THE COURT OF DIVINE JUSTICE" and "The Masonic Illuminati Eye",
#   almost all naming one obscure candidate, with stated purposes including
#   "DEPOSIT FOR WINNING IN ELECTION". A seventh reports $255,000,000 spent on
#   a candidate named Walter White for "funding for waltuh white's campaign".
#   **The FEC publishes what is filed. It does not check whether it is true.**
#
#   A DUPLICATE. Nine rows, all $114,056,874, all Food & Water Action
#   supporting Harris, and all carrying the SAME file number (1848231). One
#   filing, exported nine times. Not a prank -- a plumbing failure.
#
# THE FINDING THIS CHANGES. In the raw file, 94.5% of independent expenditure
# supports candidates and 5.5% opposes them. Remove those sixteen rows and it
# **reverses**: 57.6% opposes, 42.4% supports. The junk does not merely inflate
# the total, it flips the single most important substantive fact about outside
# money -- that most of it is spent attacking somebody.
#
# THE CLEANING RULE IS CRUDE AND STATED OPENLY: drop rows above $100 million.
# It is a threshold, not a diagnosis, and the lab makes students argue with it.
# Every row it removes is listed in ie_outliers.csv so nothing is hidden.
#
# SOURCE. FEC bulk downloads, independent expenditures, 2024 cycle:
#   https://www.fec.gov/files/bulk-downloads/2024/independent_expenditure_2024.csv
# Column layout: https://www.fec.gov/campaign-finance-data/independent-expenditures-file-description/
#
# ONE RAW FILING IS ALSO COMMITTED, so the brief can show what a junk row looks
# like as submitted rather than as summarised:
#
#   raw/ie_filing_1707511.fec   418 bytes, fetched 10 August 2026 from
#                           https://docquery.fec.gov/dcdev/posted/1707511.fec
#
# That is FEC filing number 1707511 -- the file_num on the "Gus Associates" /
# "WHITE, WALTER" row of ie_outliers.csv, $255,000,000 for "funding for waltuh
# white's campaign". It is the FEC's electronic filing format: records
# separated by newlines, fields separated by ASCII 0x1C (the file separator
# character), so read it with readLines() then strsplit(x, "\x1c"). Three
# records: HDR, an F24N cover record, and one Schedule E line. Field positions
# follow the FEC's v8.4 layout. Re-fetch with:
#
#   download.file("https://docquery.fec.gov/dcdev/posted/1707511.fec",
#                 "raw/ie_filing_1707511.fec", mode = "wb")
#   stopifnot(file.size("raw/ie_filing_1707511.fec") == 418)
#
# Run from this directory:  Rscript build-ie-data.R   (downloads ~19 MB)
# ---------------------------------------------------------------------------

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


options(scipen = 999)   # a threshold of 100 million is not written "1e+08"

source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

URL <- paste0("https://www.fec.gov/files/bulk-downloads/2024/",
              "independent_expenditure_2024.csv")

tmp <- tempfile(fileext = ".csv")
prov_fetch(URL, tmp, mode = "wb", quiet = TRUE)
ie <- read.csv(tmp, stringsAsFactors = FALSE, colClasses = "character")

# --- 0. the schema, described from the file rather than from the first row ----
#
# The brief opens on all 23 columns. It used to show the value each column
# happened to hold in ROW ONE, which is a sample of size one dressed as a
# description: row one says ele_type is "G" and the reader concludes the column
# means primary-or-general. It has six values. amndt_ind is not "N or A", it is
# N, A1, A2, A3, A4. fec_election_yr is the same 2024 on every row and carries
# no information at all -- none of which a first row can tell you.
#
# So: for a column with a small closed set of values, list THE WHOLE SET,
# because that set is what the column means. For everything else the set is not
# the point -- 49,153 transaction IDs describe nothing -- so give the count, a
# range where the values are numeric, and one real example. The example is
# quoted so that whitespace shows: `spe_nam` opens with a leading space, and
# that single invisible character splits a committee in two when the file is
# grouped without trimming.
#
# Computed here, before the cleaning below touches cand_name and spe_nam, so
# the table describes the file as it ARRIVED and not as this script left it.
CLOSED_SET <- 8            # at most this many distinct values -> list them all

# A RANGE IS ONLY SHOWN FOR THE TWO COLUMNS THAT HOLD A QUANTITY, and the
# restriction is the point rather than a shortcut. `file_num`, `image_num`,
# `prev_file_num` and `can_office_dis` are all numeric in the sense that R can
# parse them, and none of them is a number: district 8 is not eight of
# anything, and the lowest and highest filing numbers describe the FEC's
# counter rather than the data. Printing a min and a max for those columns
# would perform, in the chapter's own opening table, exactly the arithmetic on
# codes it spends the rest of its length warning against. It would also drop
# the leading zero from district "08", which is the other half of the same
# mistake.
QUANTITY <- c("exp_amo", "agg_amo")

describe_col <- function(x, nm) {
  v  <- x[nzchar(trimws(x))]
  nd <- length(unique(v))
  if (nd == 0) return("(empty in every row)")
  if (nd <= CLOSED_SET) {
    tb <- sort(table(v), decreasing = TRUE)
    return(paste(names(tb), collapse = " · "))
  }
  if (nm %in% QUANTITY) {
    num <- suppressWarnings(as.numeric(v))
    rng <- paste(format(range(num, na.rm = TRUE), big.mark = ",",
                        scientific = FALSE, trim = TRUE), collapse = " to ")
    return(sprintf("%s distinct, %s", format(nd, big.mark = ","), rng))
  }
  sprintf("%s distinct, e.g. %s", format(nd, big.mark = ","),
          paste0('"', v[1], '"'))
}

cols <- data.frame(
  n      = seq_along(names(ie)),
  column = names(ie),
  values = mapply(describe_col, ie, names(ie), USE.NAMES = FALSE),
  blank  = round(100 * vapply(ie, function(x) mean(!nzchar(trimws(x))), 0), 1),
  stringsAsFactors = FALSE)
dd_write_csv(cols, "derived/ie_columns.csv")

ie$amount <- suppressWarnings(as.numeric(ie$exp_amo))
ie$amount[is.na(ie$amount)] <- 0
ie$spender <- trimws(ie$spe_nam)

# A THIRD data problem, smaller than the other two but real: candidate names
# are not normalised. "Harris, Kamala" and "HARRIS, KAMALA" are the same person
# filed by different committees and would otherwise be two rows -- splitting her
# totals, and hiding that the money supporting her and the money attacking her
# were being counted separately. Upper-casing merges them. Spender names have
# the same problem and are NOT merged here, because two committees can share a
# name legitimately (see "THE COMMITTEE OF 300", filed under two different IDs).
ie$cand_name <- toupper(trimws(ie$cand_name))

stopifnot(nrow(ie) > 70000)

CUT <- 1e8                       # the crude rule, stated in the lab
outlier <- ie$amount > CUT
clean   <- ie[!outlier, ]

# --- the shape of the source file, so the brief does not have to be told -----
#
# The 19 MB bulk file is fetched at run time and not committed, so the brief
# cannot count its rows itself -- it used to carry `ROWS <- 73449` typed into
# the setup chunk, a number that was correct on the day somebody looked and had
# no way of noticing when it stopped being. The FEC adds rows to a closed cycle
# for years afterwards. Recorded here instead, at the moment the file is read.
#
# No fetch date: it would change on every build and make a diff out of nothing.
# What the row count is FOR is stated in the chapter, which says plainly that
# its argument is an aggregation over this many rows.
# `rows_half_to_cut` and `largest_kept` are here because the brief argues with
# the threshold, and that argument needs the rows the threshold KEPT -- which
# the brief cannot see. It reads ie_outliers.csv, and those are by definition
# the rows above the cut, so a count of "rows between half the cut and the cut"
# taken from that file is zero however the data looks. It has to be counted
# here, against the whole file, or it is not a check at all.
dd_write_csv(data.frame(
  rows             = nrow(ie),
  columns          = nrow(cols),    # the columns as they arrived, counted above
  outlier_rows     = sum(outlier),
  cut_usd          = CUT,
  rows_half_to_cut = sum(ie$amount > CUT / 2 & ie$amount <= CUT),
  largest_kept     = max(clean$amount),
  source_url       = URL,
  stringsAsFactors = FALSE), "derived/ie_facts.csv")

# MONEY IS ROUNDED TO THE CENT BEFORE IT IS WRITTEN, and the percentages below
# are computed from the rounded figures rather than beside them.
#
# Summing tens of thousands of doubles leaves ordinary floating-point residue:
# the NRCC's total came out of aggregate() as 50576806.6199999, which R then
# wrote at fifteen significant digits. Nothing downstream noticed, because the
# brief formats these as dollars -- but the file said a committee spent a
# hundredth of a cent it did not spend, and the digits CHANGED BETWEEN BUILDS as
# rows arrived at the FEC and the summation order shifted. That makes a
# meaningless diff on every rebuild and invites a reader to believe a precision
# that is not there. An expenditure is filed in dollars and cents; two decimal
# places is the whole of it.
#
# Rounding happens before the shares are taken so that a reader who recomputes
# `pct_against` from the two columns beside it gets the number printed there.
usd <- function(x) round(x, 2)

# --- 1. the sixteen rows that break the file -------------------------------
o <- ie[outlier, c("spender", "cand_name", "amount", "pur", "file_num", "sup_opp")]
o <- o[order(-o$amount), ]
o$amount <- usd(o$amount)
write.csv(o, "derived/ie_outliers.csv", row.names = FALSE)

# --- 2. summary, raw against cleaned ---------------------------------------
side <- function(d, k) sum(d$amount[d$sup_opp == k])
summ <- data.frame(
  version = c("raw", "raw", "cleaned", "cleaned"),
  side    = c("support", "oppose", "support", "oppose"),
  dollars = usd(c(side(ie, "S"), side(ie, "O"), side(clean, "S"), side(clean, "O"))),
  stringsAsFactors = FALSE)
summ$pct <- round(100 * summ$dollars /
                  ave(summ$dollars, summ$version, FUN = sum), 1)
write.csv(summ, "derived/ie_summary.csv", row.names = FALSE)

# --- 3. committees ----------------------------------------------------------
cm <- aggregate(amount ~ spender, data = clean, sum)
cm <- cm[order(-cm$amount), ][1:25, ]
opp <- aggregate(amount ~ spender,
                 data = clean[clean$sup_opp == "O", ], sum)
cm$against <- opp$amount[match(cm$spender, opp$spender)]
cm$against[is.na(cm$against)] <- 0
cm$amount  <- usd(cm$amount)
cm$against <- usd(cm$against)
cm$pct_against <- round(100 * cm$against / cm$amount, 1)
write.csv(cm, "derived/ie_committees.csv", row.names = FALSE)

# --- 4. candidates ----------------------------------------------------------
agg <- function(k) {
  a <- aggregate(amount ~ cand_name, data = clean[clean$sup_opp == k, ], sum)
  setNames(a$amount, a$cand_name)
}
S <- agg("S"); O <- agg("O")
who <- union(names(S), names(O))
cd <- data.frame(
  candidate = who,
  office    = clean$can_office[match(who, clean$cand_name)],
  party     = clean$cand_pty_aff[match(who, clean$cand_name)],
  supporting = as.numeric(S[who]), opposing = as.numeric(O[who]),
  stringsAsFactors = FALSE)
cd[is.na(cd)] <- 0
cd$supporting <- usd(cd$supporting)
cd$opposing   <- usd(cd$opposing)
cd$total <- cd$supporting + cd$opposing
cd_all <- cd[order(-cd$total), ]
TOPN   <- 30
cd     <- cd_all[1:TOPN, ]
cd$pct_against <- round(100 * cd$opposing / cd$total, 1)

# WHAT THE CUT AT THIRTY HIDES, MEASURED EVERY BUILD.
#
# The brief uses this table to show what a middle initial costs: "TRUMP, DONALD"
# and "TRUMP, DONALD J." are one man in two rows, and merging them moves the
# share of money spent against him by tens of points. That demonstration is
# computed from the rows in THIS table -- so it is a demonstration about the
# variants that made the top thirty, and there are always more below.
#
# This was checked rather than assumed, and the answer is worth stating plainly:
# of 972 candidates, the deepest variant of a name in the table sits at rank
# 891. Every headline name has more spellings further down. So the cut cannot be
# raised past the problem -- that would mean keeping the whole file -- and the
# variants cannot be merged automatically, because the chapter's own argument is
# that no rule does that safely without a list of candidates. What is left is to
# MEASURE it, every build, and let the size of the number speak:
#
#     Trump      0.3% of his money below the cut  (the headline demo survives)
#     Casey      4.5%
#     Tester    10.8%   -- one row at rank 82 worth $9.2m
#
# A banner rather than a stop, because this condition is permanent and true: it
# is a property of the source, not a fault in the build. It stops being a
# banner and starts being a correction the day a headline share moves.
surname <- function(x) toupper(trimws(sub(",.*$", "", x)))
key     <- function(d) paste(surname(d$candidate), d$office, d$party, sep = "|")
cd_all$key <- key(cd_all)
below <- cd_all[-(1:TOPN), ]
hidden <- do.call(rbind, lapply(unique(key(cd)), function(k) {
  out <- below[below$key == k, ]
  if (!nrow(out)) return(NULL)
  inn <- cd_all[seq_len(TOPN), ][key(cd) == k, ]
  data.frame(name = sub("\\|.*", "", k), rows_below = nrow(out),
             dollars_below = sum(out$total),
             pct_hidden = 100 * sum(out$total) / (sum(inn$total) + sum(out$total)),
             deepest_rank = max(which(cd_all$key == k)))
}))
if (!is.null(hidden)) {
  hidden <- hidden[order(-hidden$pct_hidden), ]
  cat("\n  [names] the top-", TOPN, " cut hides variant spellings of ",
      nrow(hidden), " names in the table:\n", sep = "")
  for (i in seq_len(nrow(hidden)))
    cat(sprintf("          %-12s %d row(s) below, $%s, %.1f%% of that person\n",
                hidden$name[i], hidden$rows_below[i],
                format(round(hidden$dollars_below[i]), big.mark = ","),
                hidden$pct_hidden[i]))
  cat("          the brief's merge demonstration is about the variants ABOVE",
      "the cut.\n")
}

# The brief looks these four up by their exact spelling. A rename upstream would
# otherwise return numeric(0) and print an empty number into a sentence.
for (nm in c("TRUMP, DONALD", "TRUMP, DONALD J.", "TESTER, R.", "TESTER, R. JON",
             "BROWN, SHERROD", "CASEY, ROBERT")) {
  if (!nm %in% cd$candidate)
    stop("the brief quotes the row '", nm, "' and this build did not produce it")
}

write.csv(cd, "derived/ie_candidates.csv", row.names = FALSE)

cat(sprintf("rows: %s\n", format(nrow(ie), big.mark = ",")))
cat(sprintf("raw total:     $%s\n", format(sum(ie$amount), big.mark = ",")))
cat(sprintf("outliers:      %d rows, $%s (%.1f%% of the raw total)\n",
            sum(outlier), format(sum(ie$amount[outlier]), big.mark = ","),
            100 * sum(ie$amount[outlier]) / sum(ie$amount)))
cat(sprintf("cleaned total: $%s\n", format(sum(clean$amount), big.mark = ",")))
cat(sprintf("\nraw:     %.1f%% support / %.1f%% oppose\n",
            summ$pct[1], summ$pct[2]))
cat(sprintf("cleaned: %.1f%% support / %.1f%% oppose  <-- reversed\n",
            summ$pct[3], summ$pct[4]))

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
