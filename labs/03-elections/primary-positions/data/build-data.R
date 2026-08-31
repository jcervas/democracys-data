# ---------------------------------------------------------------------------
# Build the data for the `primary-positions` chapter.
#
#   raw/PrimariesDatabase_Clean.xls    the Brookings file exactly as released
#   raw/PrimariesCodeBook_clean.docx   its codebook, exactly as released
#
#   derived/candidates.csv    one row per candidate, 1,662 of them, with the
#                             fourteen issue codes carried through unchanged
#   derived/by_issue.csv      one row per issue: how many candidates took each
#                             of the four codes, and the silence rate
#   derived/by_count.csv      the distribution of "how many of the fourteen
#                             issues did this candidate say nothing about"
#   derived/by_outcome.csv    silence beside the primary result
#   derived/factions.csv      the faction codes, and which ones the codebook
#                             actually defines
#   derived/party_labels.csv  every distinct string in the party column, with
#                             its length in characters, because three of them
#                             are the same word
#   derived/checks.csv        the validation results
#
# SOURCE. Brookings Institution, The Primaries Project. Compiled by Elaine C.
# Kamarck and Alexander R. Podkul; the workbook's own metadata says it was last
# saved by Grace Wallack on 5 January 2015, and the codebook is dated December
# 2014.
#
#   https://www.brookings.edu/wp-content/uploads/2016/06/PrimariesDatabase_Clean.xls
#   https://www.brookings.edu/wp-content/uploads/2016/06/PrimariesCodeBook_clean.docx
#
# Keyless: no account, no token, no fee.
#
# WHY BOTH FILES ARE COMMITTED. These sit at `wp-content/uploads/2016/06/`,
# which is a content-management system's dumping ground rather than a data
# archive. The report they belong to is from September 2014 and its own link is
# already dead. Nothing about that address promises to keep answering, and
# there is no version of this file anywhere with a DOI. 690 KB is a cheap
# insurance policy against the chapter becoming unreproducible.
#
# THE CODING IS OF WEBSITES, NOT OF CANDIDATES. This is the fact the whole
# chapter turns on, so it is recorded here as well. Kamarck's team read each
# candidate's own campaign website and coded what it said. A candidate who
# never mentions immigration is coded 4 on immigration. Code 4 is therefore not
# missing data and it is not a neutral position. It is a candidate who did not
# raise the subject, and it is the most common value in the file.
#
# NOTHING IS RECODED HERE. The four issue codes are carried through exactly as
# the workbook holds them, including 4. A build that turned 4 into NA would be
# making the chapter's argument disappear into a missing-value convention, and
# a reader of derived/candidates.csv would never know it had happened.
#
# FOUR THINGS IN THIS FILE WILL MISLEAD SOMEBODY, and all four are carried
# into the chapter rather than cleaned away:
#
#   * The party column is dirty, AND WHETHER IT IS DIRTY DEPENDS ON WHO READS
#     IT. The workbook stores `D` in 716 cells and `D ` -- with a trailing
#     space -- in three more. Python's pandas sees fifteen distinct strings.
#     R's readxl sees fourteen, because `read_excel()` takes an argument called
#     `trim_ws` whose default is TRUE, and it silently repairs those three
#     cells on the way in. Neither reader warns. So this build reads the column
#     TWICE -- once with `trim_ws = FALSE` to record what the file holds, and
#     once with the default to get the column everything else uses -- and
#     writes the difference into checks.csv. `Libertarian`/`libertarian` and
#     `G.O.P.`/`R` survive both readers, because case and punctuation are not
#     whitespace. A cleaned column is added BESIDE the original; the original
#     is not overwritten.
#
#   * The faction variable is mostly undocumented. `CandidatePartyCategory` has
#     ten values and the codebook prints four of them, under the heading
#     "examples". It then says, in its own words, that one nonmissing value is
#      not labeled. That value is 100, and it holds 259 candidates. There is no
#     published key for 11, 33, 55, 66 or 99. This script does NOT guess them.
#     factions.csv marks each code documented or not, and the chapter says so.
#
#   * `CandidateElectoralExperience` is a 0/1 flag whose zero the codebook
#     defines as "No or No information". Never held office and could not be
#     established are the same number. There is no way to separate them.
#
#   * `RaceContested` has its labels backwards in the codebook: `0 contested`,
#     `1 unontested`, typo included. The column is left alone and a note is
#     written into checks.csv, because the chapter does not use it and a reader
#     who reaches for it should be warned.
#
# ONE MORE, which is a shape rather than a defect. The file is called a
# congressional primaries database and it is: 1,443 House candidates and 219
# Senate candidates. The `district` column holds the string `SEN` for the
# latter. A `Senate` flag exists. Anyone who groups by state and district
# without looking will pool two chambers.
#
# Run from inside this data/ folder:  Rscript build-data.R
# ---------------------------------------------------------------------------

options(scipen = 999, stringsAsFactors = FALSE)

dir.create("derived", showWarnings = FALSE)
dir.create("raw",     showWarnings = FALSE)

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

stopifnot(requireNamespace("readxl", quietly = TRUE))

BASE <- "https://www.brookings.edu/wp-content/uploads/2016/06/"
XLS  <- "raw/PrimariesDatabase_Clean.xls"
DOC  <- "raw/PrimariesCodeBook_clean.docx"

# The committed copies are what the chapter reads. Re-fetch deliberately, and
# expect nothing to move: this is a file that stopped changing in 2015.
if (nzchar(Sys.getenv("PRIMARIES_REFRESH")) || !file.exists(XLS))
  invisible(prov_fetch(paste0(BASE, "PrimariesDatabase_Clean.xls"), XLS,
                       label = "Brookings Primaries Project database"))
if (nzchar(Sys.getenv("PRIMARIES_REFRESH")) || !file.exists(DOC))
  invisible(prov_fetch(paste0(BASE, "PrimariesCodeBook_clean.docx"), DOC,
                       label = "Brookings Primaries Project codebook"))

CK <- data.frame(check = character(), value = character())
add <- function(k, v) CK[nrow(CK) + 1L, ] <<- c(k, as.character(v))

# --- 1. the workbook --------------------------------------------------------
raw <- as.data.frame(readxl::read_excel(XLS, sheet = 1))
add("rows in the workbook", format(nrow(raw), big.mark = ","))
add("columns in the workbook", ncol(raw))
stopifnot(nrow(raw) == 1662)

# The fourteen issues, in the order the workbook holds them, with the wording
# the chapter uses. The column name is the workbook's; the label is ours, and
# is only ever used for printing.
ISSUES <- data.frame(
  col = c("CandidateImmigration", "CandidateACA", "CandidateBenghazi",
          "CandidateTax", "CandidateMinWage", "CandidateGunControl",
          "CandidateAbortion", "CandidateClimateChange", "CandidateNSA",
          "CandidateSameSexMarriage", "CandidateDebtDeficit",
          "CandidateKeystone", "CandidateRegulations",
          "CandidateDefenseSpending"),
  issue = c("immigration", "the Affordable Care Act", "Benghazi",
            "taxes", "the minimum wage", "gun control",
            "abortion", "climate change", "the NSA",
            "same-sex marriage", "the debt and deficit",
            "the Keystone pipeline", "regulation", "defense spending"))
stopifnot(all(ISSUES$col %in% names(raw)))

# Every issue column must be 1, 2, 3 or 4 with nothing missing. If a future
# copy of this file ever uses a fifth code or a blank, everything below is
# wrong, so the build stops here rather than there.
for (cc in ISSUES$col) {
  v <- raw[[cc]]
  if (any(is.na(v)) || !all(v %in% 1:4))
    stop("issue column '", cc, "' is not a complete 1-4 coding")
}
add("issue columns, all complete and coded 1-4", nrow(ISSUES))

# --- 2. the party column, read twice ----------------------------------------
# `raw` above came through readxl's default, which is `trim_ws = TRUE`. That
# default edits the data: three cells holding "D " arrive as "D". So the column
# is read a second time with the trimming turned off, and THAT is what
# party_labels.csv reports, because the file's contents are the evidence and
# the reader's repair is the finding.
untrimmed <- as.data.frame(
  readxl::read_excel(XLS, sheet = 1, trim_ws = FALSE))$CandidatePartyLabel
trimmed <- raw$CandidatePartyLabel

pl <- as.data.frame(table(untrimmed), stringsAsFactors = FALSE)
names(pl) <- c("label_as_stored", "candidates")
pl$characters     <- nchar(pl$label_as_stored)
pl$trailing_space <- grepl("[ ]$", pl$label_as_stored)
pl$survives_default_read <- !pl$trailing_space
pl <- pl[order(-pl$candidates, pl$label_as_stored), ]

NMOVED <- sum(untrimmed != trimmed)
NDIRTY <- sum(pl$candidates[pl$trailing_space])
stopifnot(NMOVED == 3, NDIRTY == 3)

add("distinct party strings the file holds", nrow(pl))
add("distinct party strings readxl reports by default",
    length(unique(trimmed)))
add("cells readxl's trim_ws default silently rewrites", NMOVED)
add("candidates whose party label ends in a space", NDIRTY)
add("of the strings the file holds, pairs differing only by case or spacing",
    "D / 'D ', Libertarian / libertarian, R / G.O.P.")

# Clean beside, never over, and touch only what is actually a duplicate. Trim
# the whitespace, then fold the two strings that name a party this file already
# has under another spelling. Nothing else is normalised -- uppercasing the
# whole column would "move" all fifty candidates of the small parties without
# fixing anything, which would make the count of repairs meaningless.
pc <- trimws(untrimmed)
pc[pc == "libertarian"] <- "Libertarian"
pc[pc == "G.O.P."]      <- "R"
raw$party_clean <- pc
add("distinct parties after cleaning", length(unique(pc)))

# --- 3. factions ------------------------------------------------------------
# The four the codebook prints, and nothing else. The rest are left blank on
# purpose: this build has no source for them and will not invent one.
DOCUMENTED <- c("22" = "Tea Party Republican", "44" = "Conservative Republican",
                "77" = "Establishment Democrat", "88" = "Moderate Democrat")
COLLAPSED <- c("0" = "Democrat", "1" = "Republican", "3" = "Third party")

fac <- as.data.frame(table(raw$CandidatePartyCategory), stringsAsFactors = FALSE)
names(fac) <- c("code", "candidates")
fac$codebook_label <- ifelse(fac$code %in% names(DOCUMENTED),
                             DOCUMENTED[fac$code], "")
fac$documented <- fac$codebook_label != ""
# Which side of the aisle each code sits on is recoverable from the file even
# where the label is not, so it is recovered rather than left blank.
side <- tapply(raw$CandidateCollapsedParty, raw$CandidatePartyCategory,
               function(z) paste(sort(unique(COLLAPSED[as.character(z)])),
                                 collapse = " and "))
fac$party <- as.character(side[fac$code])
fac <- fac[order(as.integer(fac$code)), ]

NUNDOC <- sum(fac$candidates[!fac$documented])
add("faction codes in the file", nrow(fac))
add("of those, defined in the codebook", sum(fac$documented))
add("candidates whose faction code the codebook does not define",
    sprintf("%s (%.1f%%)", format(NUNDOC, big.mark = ","),
            100 * NUNDOC / nrow(raw)))
add("candidates in code 100, the value the codebook calls unlabeled",
    sum(raw$CandidatePartyCategory == 100))

# The codebook's own sentence, quoted, because it is the evidence that this is
# a documented gap rather than a file that lost its labels in transit.
add("the codebook's wording for code 100",
    "label: PartyCatLabel, but 1 nonmissing value is not labeled")

# --- 4. the candidate table -------------------------------------------------
d <- data.frame(
  first        = raw$candidatefirstname,
  last         = raw$candidatelastname,
  state        = raw$state,
  district     = raw$district,
  senate       = raw$Senate == 1,
  # The untrimmed string, so candidates.csv carries what the workbook holds
  # rather than what the default reader handed back.
  party_raw    = untrimmed,
  party        = raw$party_clean,
  party_side   = COLLAPSED[as.character(raw$CandidateCollapsedParty)],
  faction_code = raw$CandidatePartyCategory,
  incumbent    = raw$Incumbent == 1,
  open_seat    = raw$OpenSeatElection,          # NA for 220; left as NA
  contested    = raw$Contested == 1,
  prim_result  = c("lost", "won", "runoff")[raw$CandidatePrimaryVoteWinStatus + 1],
  prim_share   = 100 * raw$CandidatePrimaryVotePercent,
  experience   = raw$CandidateElectoralExperience == 1,
  female       = raw$CandidateFemale == 1)
d <- cbind(d, raw[, ISSUES$col])
names(d)[(ncol(d) - nrow(ISSUES) + 1):ncol(d)] <- ISSUES$issue

# How many of the fourteen this candidate said nothing about.
SIL <- as.matrix(raw[, ISSUES$col]) == 4
d$silent_on <- as.integer(rowSums(SIL))
stopifnot(d$silent_on >= 0, d$silent_on <= 14)

add("House candidates", format(sum(!d$senate), big.mark = ","))
add("Senate candidates", sum(d$senate))
add("candidates whose open-seat status the file does not record",
    sum(is.na(d$open_seat)))

# --- 5. the three summary tables --------------------------------------------
by_issue <- do.call(rbind, lapply(seq_len(nrow(ISSUES)), function(i) {
  v <- raw[[ISSUES$col[i]]]
  data.frame(issue = ISSUES$issue[i],
             supports = sum(v == 1), opposes = sum(v == 2),
             unclear  = sum(v == 3), silent  = sum(v == 4),
             # Two decimals. One candidate is 0.06% of the file, so a third
             # decimal would be printing precision the denominator has not got.
             silent_pct = round(100 * mean(v == 4), 2))
}))
by_issue <- by_issue[order(-by_issue$silent_pct), ]

by_count <- data.frame(silent_on = 0:14)
by_count$candidates <- as.integer(table(factor(d$silent_on, levels = 0:14)))
by_count$pct <- round(100 * by_count$candidates / nrow(d), 2)

# Winners against losers, House only. Runoff rows are their own category and
# are kept separate rather than folded into either.
h <- d[!d$senate, ]
by_outcome <- aggregate(cbind(silent_on, n = rep(1, nrow(h))) ~ prim_result, h, sum)
by_outcome$mean_silent <- round(by_outcome$silent_on / by_outcome$n, 2)
by_outcome <- by_outcome[order(-by_outcome$n), c("prim_result", "n", "mean_silent")]

MED  <- median(d$silent_on)
ALL14 <- sum(d$silent_on == 14)
NONE  <- sum(d$silent_on == 0)
add("median issues a candidate is silent on, of 14", MED)
add("candidates silent on all fourteen",
    sprintf("%d (%.1f%%)", ALL14, 100 * ALL14 / nrow(d)))
add("candidates silent on none of the fourteen",
    sprintf("%d (%.1f%%)", NONE, 100 * NONE / nrow(d)))
add("quietest issue", sprintf("%s, %.1f%% silent", by_issue$issue[1],
                              by_issue$silent_pct[1]))
add("loudest issue", sprintf("%s, %.1f%% silent",
                             by_issue$issue[nrow(by_issue)],
                             by_issue$silent_pct[nrow(by_issue)]))
add("mean issues silent, House primary winners",
    sprintf("%.1f", by_outcome$mean_silent[by_outcome$prim_result == "won"]))
add("mean issues silent, House primary losers",
    sprintf("%.1f", by_outcome$mean_silent[by_outcome$prim_result == "lost"]))

# --- 6. the two documentation defects, recorded ------------------------------
add("RaceContested value labels in the codebook",
    "0 contested, 1 unontested [sic] -- inverted, and misspelled; unused here")
add("CandidateElectoralExperience, meaning of 0",
    "No or No information -- never held office and not established are one code")

# --- 7. the checks that could fail ------------------------------------------
# The silence count must be reconstructible from the candidate table alone, or
# derived/candidates.csv and every figure built from it have come apart.
chk <- rowSums(d[, ISSUES$issue] == 4)
stopifnot(identical(as.integer(chk), d$silent_on))

# The four codes must partition every issue exactly.
stopifnot(all(by_issue$supports + by_issue$opposes +
              by_issue$unclear + by_issue$silent == nrow(d)))

# The distribution must sum back to the file.
stopifnot(sum(by_count$candidates) == nrow(d))

# And the party clean-up must not have merged anything real. Four parties go
# in with more than 100 candidates and four must come out.
stopifnot(sum(table(d$party) > 100) == 2)          # D and R only
stopifnot(sum(d$party == "D") == 719, sum(d$party == "R") == 897,
          sum(d$party == "Libertarian") == 8)
add("candidates whose party label the cleaning moves, all causes",
    sum(d$party != d$party_raw))
add("of those, moved by trimming a space", NDIRTY)
add("of those, moved by folding case", sum(d$party_raw == "libertarian"))
add("of those, moved by folding G.O.P. into R", sum(d$party_raw == "G.O.P."))

w <- function(df, f) write.csv(df, file.path("derived", f), row.names = FALSE)
w(d,          "candidates.csv")
w(by_issue,   "by_issue.csv")
w(by_count,   "by_count.csv")
w(by_outcome, "by_outcome.csv")
w(fac,        "factions.csv")
w(pl,         "party_labels.csv")
w(CK,         "checks.csv")

cat(sprintf(paste0(
  "\n  candidates            %s  (%s House, %s Senate)\n",
  "  issues coded          %d\n",
  "  median silent on      %d of %d\n",
  "  silent on all         %d\n",
  "  silent on none        %d\n",
  "  faction codes         %d, of which %d are documented\n",
  "  party strings         %d, cleaning to %d\n"),
  format(nrow(d), big.mark = ","), format(sum(!d$senate), big.mark = ","),
  sum(d$senate), nrow(ISSUES), MED, nrow(ISSUES), ALL14, NONE,
  nrow(fac), sum(fac$documented), nrow(pl), length(unique(d$party))))

invisible(prov_report())

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
