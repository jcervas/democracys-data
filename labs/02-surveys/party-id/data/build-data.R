# ---------------------------------------------------------------------------
# Build the party identification dataset: how many independents are there?
#
# Five files end up in this folder:
#
#   raw/branch.txt    A real capture: the two questions the seven-point scale
#                     is actually made of, and five real rows.
#   derived/sevenpoint.csv    Every study year, the share in each of the seven
#                     categories. The unit is the respondent.
#   derived/independents.csv  The same years, with the share of "independents" computed
#                     three defensible ways. They do not agree.
#   derived/leaners.csv       How leaners actually voted, which is the evidence that
#                     the coding decision is not arbitrary.
#   derived/collapse.csv      What ANES's own three-category column does, and what it
#                     costs to use it without reading its note.
#
# THE QUESTION THIS CHAPTER IS ABOUT. "A third of Americans are independents"
# is one of the most repeated facts in American politics. It is also
# unfalsifiable as stated, because the number depends entirely on a coding
# decision that the sentence never mentions. In 2024 the share of independents
# in this file is 6.9% or 33.0% depending on that decision alone.
#
# WHERE THE SEVEN POINTS COME FROM. VCF0301 looks like one question and is not.
# It is a branching sequence:
#
#   Q1. "Generally speaking, do you usually think of yourself as a Republican,
#        a Democrat, an Independent, or what?"
#   Q2. If Republican or Democrat: "Would you call yourself a STRONG
#        [R/D] or a NOT VERY STRONG [R/D]?"
#   Q3. If Independent or other: "Do you think of yourself as CLOSER to the
#        Republican or Democratic party?"
#
# Categories 3 and 5 -- the LEANERS -- are people who said "independent" to the
# first question and named a party in the third. Whether they are independents
# is not a question the data can answer. It is a question about what the word
# means, and every published independent share has silently answered it.
#
# WHAT ANES ITSELF DOES, and this is the part worth teaching. The file ships a
# convenience column, VCF0303, whose codebook note reads:
#
#   Collapsed from VCF0301, 1-3=1, 4=2, 5-7=3
#   1. Democrats (including leaners)
#   2. Independents
#   3. Republicans (including leaners)
#
# So ANES has already decided: leaners are partisans. Anyone who reaches for
# the three-category column because it is easier has adopted that definition
# without choosing it, and will report an independent share near 11% rather
# than near 34%. Neither is wrong. Only one of them is stated.
#
# THE EVIDENCE THAT THE DECISION MATTERS. leaners.csv reports how each of the
# seven groups voted for president. If leaners voted like independents, the
# coding choice would be cosmetic. They do not: leaners vote about as loyally
# as weak partisans, which is the empirical case for counting them as
# partisans -- and it is a case, not a fact, and the chapter says so.
#
# CODING, from the ANES codebook:
#   VCF0004  study year
#   VCF0301  party identification, 7 point
#            1 strong Dem, 2 weak Dem, 3 independent-Dem (leaner),
#            4 independent-independent, 5 independent-Rep (leaner),
#            6 weak Rep, 7 strong Rep.  0 = DK/NA/other -- NOT a scale point.
#   VCF0303  the three-category collapse described above. 0 = missing.
#   VCF0704  presidential vote, two-party: 1 Democrat, 2 Republican
#   VCF0009z sample weight, present and deliberately unused here
#
# SOURCE. American National Election Studies, Time Series Cumulative Data File
# 1948-2024, version of 5 February 2026.
#   https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
# Requested, not handed over: the download page returns 403 to a script and 200
# to a browser, so a person has to fetch it. Not redistributed here (163 MB).
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

CAND <- path.expand(c(
  "raw/anes_timeseries_cdf_csv_20260205.csv",
  "../../anes/data/raw/anes_timeseries_cdf_csv_20260205.csv",
  "~/Downloads/anes_timeseries_cdf_csv_20260205.csv",
  "~/Downloads/anes_timeseries_cdf_csv_20260205.zip",
  "raw/anes_timeseries_cdf_csv_20260205.zip"))
hit <- CAND[file.exists(CAND)]
if (!length(hit)) stop(
  "ANES cumulative file not found. Download the CSV distribution from\n",
  "  https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/\n",
  "and put it in this folder's raw/ subdirectory.")

src <- hit[1]
if (grepl("\\.zip$", src)) {
  inner <- grep("\\.csv$", unzip(src, list = TRUE)$Name, value = TRUE)[1]
  hdr <- names(read.csv(unz(src, inner), nrows = 1, check.names = FALSE))
  reader <- function(cc) read.csv(unz(src, inner), colClasses = cc,
                                  check.names = FALSE)
} else {
  hdr <- names(read.csv(src, nrows = 1, check.names = FALSE))
  reader <- function(cc) read.csv(src, colClasses = cc, check.names = FALSE)
}
cat("reading:", src, "\n")

KEEP <- c("VCF0004", "VCF0301", "VCF0303", "VCF0704", "VCF0009z")
stopifnot(all(KEEP %in% hdr))
d <- reader(ifelse(hdr %in% KEEP, NA, "NULL"))
stopifnot(nrow(d) > 70000)

LAB <- c("Strong Democrat", "Weak Democrat", "Independent-Democrat",
         "Independent-Independent", "Independent-Republican",
         "Weak Republican", "Strong Republican")

pid <- d$VCF0301
pid[!pid %in% 1:7] <- NA

# --- 1. a real capture ------------------------------------------------------

set.seed(84355)
i <- sort(sample.int(sum(!is.na(pid)), 5))
w <- which(!is.na(pid))[i]
cap <- file("raw/branch.txt", "w")
writeLines(c(
"VCF0301 looks like one column. It is three questions.",
"",
"  Q1  Generally speaking, do you usually think of yourself as a",
"      Republican, a Democrat, an Independent, or what?",
"",
"  Q2  (if Republican or Democrat) Would you call yourself a STRONG",
"      [R/D] or a NOT VERY STRONG [R/D]?",
"",
"  Q3  (if Independent or other) Do you think of yourself as CLOSER",
"      to the Republican or Democratic party?",
"",
"The seven points are the answers combined:",
"",
"  1 strong Dem      2 weak Dem      3 independent-Dem   (leaner)",
"  4 independent-independent",
"  5 independent-Rep (leaner)        6 weak Rep          7 strong Rep",
"",
"Five random rows as they arrive. VCF0303 is the file's own collapse.",
""), cap)
utils::write.table(
  data.frame(year = d$VCF0004[w], VCF0301 = pid[w],
             means = LAB[pid[w]], VCF0303 = d$VCF0303[w]),
  cap, sep = "  ", quote = FALSE, row.names = FALSE)
writeLines(c("",
"Nothing in the seven numbers says that 3 and 5 answered 'Independent'",
"to the first question. The codebook says it. The data does not."), cap)
close(cap)

# --- 2. the seven categories, every year ------------------------------------

yrs <- sort(unique(d$VCF0004[!is.na(pid)]))
sev <- do.call(rbind, lapply(yrs, function(y) {
  v <- pid[d$VCF0004 == y & !is.na(pid)]
  data.frame(year = y, n = length(v),
             t(round(100 * as.vector(table(factor(v, levels = 1:7))) /
                     length(v), 1)))
}))
names(sev)[3:9] <- c("strong_dem", "weak_dem", "lean_dem", "pure_ind",
                     "lean_rep", "weak_rep", "strong_rep")
write.csv(sev, "derived/sevenpoint.csv", row.names = FALSE)

# --- 3. three defensible independent shares ---------------------------------

ind <- do.call(rbind, lapply(yrs, function(y) {
  v <- pid[d$VCF0004 == y & !is.na(pid)]
  data.frame(year = y, n = length(v),
             pure_only        = round(100 * mean(v == 4), 1),
             with_leaners     = round(100 * mean(v %in% 3:5), 1),
             leaners_and_weak = round(100 * mean(v %in% c(2:6)), 1))
}))
ind$spread <- round(ind$with_leaners - ind$pure_only, 1)
write.csv(ind, "derived/independents.csv", row.names = FALSE)

# --- 4. how the leaners actually voted --------------------------------------
#
# Two-party presidential vote only. This is the evidence for treating leaners
# as partisans, and it is evidence rather than proof: it shows they BEHAVE like
# partisans in one respect, not that the word "independent" is wrong for them.

vt <- d$VCF0704
vt[!vt %in% 1:2] <- NA
ok <- !is.na(pid) & !is.na(vt)
lean <- data.frame(
  category = LAB,
  respondents = as.vector(table(factor(pid[ok], levels = 1:7))),
  pct_voted_democratic = round(100 * as.vector(
    tapply(vt[ok] == 1, factor(pid[ok], levels = 1:7), mean)), 1))
lean$loyalty <- pmax(lean$pct_voted_democratic, 100 - lean$pct_voted_democratic)
write.csv(lean, "derived/leaners.csv", row.names = FALSE)

# --- 5. what the convenience column costs -----------------------------------

three <- d$VCF0303
three[!three %in% 1:3] <- NA
k <- !is.na(pid) & !is.na(three)
agree <- table(pid[k], three[k])

coll <- data.frame(
  seven_point = LAB,
  file_puts_it_in = c("Democrat", "Democrat", "Democrat", "Independent",
                      "Republican", "Republican", "Republican"),
  respondents = as.vector(table(factor(pid[k], levels = 1:7))))
coll$share <- round(100 * coll$respondents / sum(coll$respondents), 1)
write.csv(coll, "derived/collapse.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

last <- max(ind$year)
cat(sprintf("\nsevenpoint.csv   : %d study years, %d-%d\n",
            nrow(sev), min(yrs), max(yrs)))
cat("independents.csv : the share of independents, three ways\n")
print(ind[ind$year %in% c(1952, 1972, 1992, 2012, last),
          c("year", "n", "pure_only", "with_leaners", "spread")],
      row.names = FALSE)
cat(sprintf("\n  In %d the answer is %.1f%% or %.1f%% -- a factor of %.1f.\n",
            last, ind$pure_only[ind$year == last],
            ind$with_leaners[ind$year == last],
            ind$with_leaners[ind$year == last] / ind$pure_only[ind$year == last]))
cat("\nleaners.csv      : two-party presidential vote by category\n")
print(lean[, c("category", "respondents", "pct_voted_democratic", "loyalty")],
      row.names = FALSE)
cat(sprintf("\n  Leaning Democrats vote Democratic %.1f%% of the time;\n",
            lean$pct_voted_democratic[3]))
cat(sprintf("  weak Democrats %.1f%%. The leaners are the more loyal group.\n",
            lean$pct_voted_democratic[2]))
cat("\ncollapse.csv     : VCF0303 puts leaners with the parties\n")
cat(sprintf("  independents under the file's own collapse: %.1f%%\n",
            coll$share[4]))
cat(sprintf("  independents if you decide for yourself     : %.1f%%\n",
            sum(coll$share[3:5])))

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
