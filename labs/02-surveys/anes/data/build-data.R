# ---------------------------------------------------------------------------
# Build the ANES chapter dataset: political awareness and the shape of opinion.
#
# Six files end up in this folder:
#
#   raw/anes-head.txt     A real capture: the first line of the file as it
#                         arrives, plus five real rows, columns untouched.
#   derived/coverage.csv          Every variable this chapter uses, and the years it
#                         actually exists. Not all of them run 1948-2024.
#   derived/codes.csv             The naive mean, the correct mean, and the two
#                         half-cleaned means in between. This is the chapter.
#   derived/awareness.csv         The constructed awareness scale, and what it is made
#                         of. Nobody was asked how aware they are.
#   derived/zaller.csv            Opinion by awareness by party, for one consensus
#                         issue and one contested issue.
#   derived/resentment.csv        The four-item racial resentment battery, 1986-2024.
#
# SOURCE. American National Election Studies, Time Series Cumulative Data File
# 1948-2024, version of 5 February 2026.
#   https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
# The CSV distribution is `anes_timeseries_cdf_csv_20260205.zip` (21.8 MB
# zipped; the CSV inside is 163 MB, 73,745 rows, 1,030 columns) and it ships
# with both codebooks, which are the only documents these data may be read
# with -- ANES says so on the page, and it is not a formality: the same VCF
# number means the same question across studies ONLY because staff recoded it
# to make that true.
#
# ACQUISITION. Requested, not handed over. The file sits behind a download page
# that returns 403 to a script and 200 to a browser, so no build script can
# fetch it unattended; a person has to go and get it. It is not redistributed
# here (163 MB, and ANES asks that people take it from ANES). Put the zip
# anywhere below and this script finds it.
#
# THE THREE THINGS THIS CHAPTER IS ABOUT, all visible in the raw file:
#
# 1. THE CODES THAT ARE NOT MEASUREMENTS. VCF0806 is a seven-point scale, and
#    the column also contains -1, 0 and 9. Together that is 20.8% of all
#    non-missing answers. The naive mean is 3.966 and the correct mean is
#    3.829, which sounds like a small problem until you notice WHY it is small:
#    the 5,044 nines push up (9 is larger than the largest real answer) and the
#    5,019 zeros and minus-ones push down, and the two errors nearly cancel.
#    Clean only the nines and you get 3.380. Clean only the zeros and you get
#    4.431. **Half-cleaning is three to four times worse than not cleaning at
#    all**, and the uncleaned number looks perfectly reasonable, which is what
#    makes it dangerous. Every number in codes.csv is computed, not asserted.
#
# 2. THE AXIS NOBODY WAS ASKED ABOUT. The most famous figure in public opinion
#    research plots attitude against POLITICAL AWARENESS, and no respondent was
#    ever asked how aware they were. It is built -- from the interviewer's
#    rating and from campaign interest -- and the builder has choices. Worse for
#    anyone hoping to just look it up: the interviewer rating is not one column.
#    It is VCF0050a (1968-2016) and VCF0050b (1966-2016), and BOTH STOP IN 2016,
#    so the classic measure does not reach the last three studies at all.
#
# 3. WHAT THE FILE ADMITS. ANES includes a question only if it was asked in
#    three or more studies. That rule, and not the researcher, decides which
#    questions can be asked of this file at all.
#
# CODING, from the ANES codebook:
#   VCF0004  study year
#   VCF0301  party identification, 7 point: 1 strong Dem ... 7 strong Rep,
#            0 = DK/NA/other. NOT a scale point.
#   VCF0310  interest in the campaign: 1 not much, 2 somewhat, 3 very much;
#            0 and 9 are not interest levels.
#   VCF0050a interviewer rating of respondent's political information, 1-5
#   VCF0050b interviewer rating, alternate series, 1-5
#   VCF0806  government health insurance scale, 1 (govt plan) - 7 (private);
#            -1, 0, 9 are not positions.
#   VCF0838  abortion, 1-4 with 9 = DK
#   VCF9039-9042  racial resentment battery, 1-5 agree/disagree
#   VCF0009z sample weight
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

# --- locate the file the professor downloaded -------------------------------

CAND <- c("raw/anes_timeseries_cdf_csv_20260205.csv",
          "~/Downloads/anes_timeseries_cdf_csv_20260205.csv",
          "~/Downloads/anes_timeseries_cdf_csv_20260205.zip",
          "raw/anes_timeseries_cdf_csv_20260205.zip")
CAND <- path.expand(CAND)
hit  <- CAND[file.exists(CAND)]
if (!length(hit)) stop(
  "ANES cumulative file not found. Download the CSV distribution from\n",
  "  https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/\n",
  "and put the zip in this folder's raw/ subdirectory.")

src <- hit[1]
if (grepl("\\.zip$", src)) {
  inner <- grep("\\.csv$", unzip(src, list = TRUE)$Name, value = TRUE)[1]
  con <- unz(src, inner)
  hdr <- names(read.csv(con, nrows = 1, check.names = FALSE))
  reader <- function(cc) read.csv(unz(src, inner), colClasses = cc,
                                  check.names = FALSE)
} else {
  hdr <- names(read.csv(src, nrows = 1, check.names = FALSE))
  reader <- function(cc) read.csv(src, colClasses = cc, check.names = FALSE)
}
cat("reading:", src, "\n")

KEEP <- c("VCF0004", "VCF0301", "VCF0310", "VCF0050a", "VCF0050b",
          "VCF0806", "VCF0838", "VCF9039", "VCF9040", "VCF9041", "VCF9042",
          "VCF0009z")
stopifnot(all(KEEP %in% hdr))
d <- reader(ifelse(hdr %in% KEEP, NA, "NULL"))
stopifnot(nrow(d) > 70000)
cat(sprintf("%s rows, %d of %d columns kept\n",
            format(nrow(d), big.mark = ","), ncol(d), length(hdr)))

# --- 1. a real capture of the arriving file ---------------------------------
#
# Five RANDOM rows, not the first five: the file is ordered by study year, so
# head() returns five interviews from 1948 and nothing else.

set.seed(84355)
i <- sort(sample.int(nrow(d), 5))
cap <- file("raw/anes-head.txt", "w")
writeLines(c(
  "ANES Time Series Cumulative Data File, 20260205 CSV distribution",
  sprintf("%s rows x %s columns", format(nrow(d), big.mark = ","),
          format(length(hdr), big.mark = ",")),
  "",
  "The first 14 of 1,030 column names, exactly as they arrive:",
  paste(strwrap(paste(head(hdr, 14), collapse = "  "), 68), collapse = "\n"),
  "",
  "Five random rows, the columns this chapter reads, values untouched:",
  ""), cap)
utils::write.table(format(d[i, KEEP]), cap, sep = "  ", quote = FALSE,
                   row.names = FALSE)
writeLines(c("",
  "Every -1, 0 and 9 above is a code, not a measurement.",
  "The codebook is the only way to know which is which."), cap)
close(cap)

# --- 2. coverage: what actually spans 1948-2024 -----------------------------

cov <- do.call(rbind, lapply(setdiff(KEEP, "VCF0004"), function(v) {
  k <- !is.na(d[[v]])
  y <- sort(unique(d$VCF0004[k]))
  data.frame(variable = v, first = min(y), last = max(y), studies = length(y),
             answers = sum(k))
}))
# Labels taken from the codebook, not from memory. Three of these four
# resentment titles were wrong on the first pass -- VCF9040 is the special
# favors item, not the try harder one -- which is a small demonstration of why
# the codebook is the only thing that can name a VCF number.
cov$label <- c(
  VCF0301  = "Party identification, 7 point",
  VCF0310  = "Interest in the campaign",
  VCF0050a = "Interviewer rating of political information (a)",
  VCF0050b = "Interviewer rating of political information (b)",
  VCF0806  = "Government health insurance, 7 point",
  VCF0838  = "Abortion, 4 point",
  VCF9039  = "Conditions make it difficult for blacks to succeed",
  VCF9040  = "Blacks should not have special favors to succeed",
  VCF9041  = "Blacks must try harder to succeed",
  VCF9042  = "Blacks gotten less than they deserve",
  VCF0009z = "Sample weight")[cov$variable]
write.csv(cov, "derived/coverage.csv", row.names = FALSE)

# --- 3. the codes that are not measurements ---------------------------------

v  <- d$VCF0806
mm <- function(cs) { w <- v; w[w %in% cs] <- NA; mean(w, na.rm = TRUE) }
codes <- data.frame(
  cleaning = c("Nothing removed, the file as it arrives",
               "Only 9 (don't know) removed",
               "Only 0 and -1 (inapplicable) removed",
               "Every non-scale code removed"),
  mean = round(c(mm(integer(0)), mm(9), mm(c(0, -1)), mm(c(-1, 0, 9))), 3),
  n = c(sum(!is.na(v)), sum(!is.na(v) & v != 9), sum(!is.na(v) & !v %in% c(0, -1)),
        sum(!is.na(v) & !v %in% c(-1, 0, 9))))
codes$error <- round(codes$mean - codes$mean[4], 3)
write.csv(codes, "derived/codes.csv", row.names = FALSE)

# --- 4. the constructed axis ------------------------------------------------
#
# Awareness is BUILT. The chapter shows two defensible builds so a reader can
# see the answer move: the interviewer's rating alone, and the rating combined
# with self-reported interest. Both are rescaled to run 0-1 so they can be
# compared at all, which is itself a decision.

rescale <- function(x) (x - min(x, na.rm = TRUE)) /
                       (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# THE SCALE RUNS BACKWARDS, and nothing in the CSV says so. From the codebook:
#   VCF0050a  1. Very high  2. Fairly high  3. Average  4. Fairly low  5. Very low
# So 1 is the MOST informed respondent, not the least. Read it in the intuitive
# ascending direction and the famous figure comes out inverted -- the party gap
# appears to SHRINK with awareness rather than widen -- and the inverted result
# is perfectly readable as a finding ("information moderates partisanship"),
# which is why it does not announce itself as an error. Reversed here so that
# larger always means more aware. This is what ANES means when it says the file
# cannot be used with any codebook but its own.
info <- ifelse(!is.na(d$VCF0050a), d$VCF0050a, d$VCF0050b)
info[!info %in% 1:5] <- NA
info <- 6 - info                                  # now 1 = very low, 5 = very high
inte <- d$VCF0310; inte[!inte %in% 1:3] <- NA                 # 1-3

d$aw_rating <- rescale(info)
d$aw_both   <- rescale(rescale(info) + rescale(inte))

aw <- data.frame(
  build = c("Interviewer rating only (VCF0050a, falling back to VCF0050b)",
            "Interviewer rating plus campaign interest (VCF0310)"),
  respondents = c(sum(!is.na(d$aw_rating)), sum(!is.na(d$aw_both))),
  first_year = c(min(d$VCF0004[!is.na(d$aw_rating)]),
                 min(d$VCF0004[!is.na(d$aw_both)])),
  last_year  = c(max(d$VCF0004[!is.na(d$aw_rating)]),
                 max(d$VCF0004[!is.na(d$aw_both)])),
  levels = c(length(unique(na.omit(d$aw_rating))),
             length(unique(na.omit(d$aw_both)))))
write.csv(aw, "derived/awareness.csv", row.names = FALSE)

# --- 5. the figure: opinion by awareness by party ---------------------------

pid <- d$VCF0301
party <- ifelse(pid %in% 1:3, "Democrat",
         ifelse(pid == 4, "Independent",
         ifelse(pid %in% 5:7, "Republican", NA)))

hi <- d$VCF0806; hi[!hi %in% 1:7] <- NA        # 1 govt plan ... 7 private
ab <- d$VCF0838; ab[!ab %in% 1:4] <- NA        # 1 never ... 4 always permitted

band <- cut(d$aw_both, breaks = seq(0, 1, 0.2), include.lowest = TRUE,
            labels = c("lowest", "low", "middle", "high", "highest"))

mk <- function(y, nm) {
  k <- !is.na(y) & !is.na(band) & !is.na(party)
  ag <- aggregate(y[k], list(awareness = band[k], party = party[k]), mean)
  names(ag)[3] <- "mean"
  n  <- aggregate(y[k], list(awareness = band[k], party = party[k]), length)
  ag$n <- n$x; ag$item <- nm; ag$mean <- round(ag$mean, 3)
  ag[, c("item", "party", "awareness", "mean", "n")]
}
z <- rbind(mk(hi, "Government health insurance"), mk(ab, "Abortion"))
write.csv(z, "derived/zaller.csv", row.names = FALSE)

# --- 6. racial resentment ---------------------------------------------------

# TWO OF THE FOUR RUN THE OTHER WAY, and this is the same trap as VCF0050a in
# a second costume. All four are coded 1 = agree strongly ... 5 = disagree
# strongly, but agreeing does not mean the same thing across them:
#
#   VCF9039  conditions make it difficult to succeed   agreeing = LESS resentment
#   VCF9040  should not have special favors            agreeing = MORE resentment
#   VCF9041  must try harder                           agreeing = MORE resentment
#   VCF9042  gotten less than they deserve             agreeing = LESS resentment
#
# So 9040 and 9041 must be reversed before anything is averaged. Skip that and
# the two directions cancel: every party lands within 0.05 of every other in
# every year, which reads as "no partisan difference in racial attitudes" --
# a striking finding, and an artifact of arithmetic on mixed-direction items.
# The scale runs 1 (least resentful) to 5 (most) after the reversal.
REV  <- c("VCF9040", "VCF9041")
rr <- d[, c("VCF9039", "VCF9040", "VCF9041", "VCF9042")]
for (j in names(rr)) {
  rr[[j]][!rr[[j]] %in% 1:5] <- NA                 # 8 = DK, 9 = NA, both out
  if (j %in% REV) rr[[j]] <- 6 - rr[[j]]
}
res <- data.frame(year = d$VCF0004, party = party,
                  score = rowMeans(rr, na.rm = FALSE))
res <- res[!is.na(res$score) & !is.na(res$party), ]
rag <- aggregate(res$score, list(year = res$year, party = res$party), mean)
names(rag)[3] <- "resentment"
rag$resentment <- round(rag$resentment, 3)
rag$n <- aggregate(res$score, list(year = res$year, party = res$party),
                   length)$x
write.csv(rag[order(rag$year, rag$party), ], "derived/resentment.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

cat("\nraw/anes-head.txt : real capture, 5 random rows\n")
cat(sprintf("coverage.csv      : %d variables; only %d reach 2024\n",
            nrow(cov), sum(cov$last == 2024)))
cat(sprintf("  the interviewer rating stops in %d, %d studies short of the end\n",
            max(cov$last[grepl("VCF0050", cov$variable)]),
            length(unique(d$VCF0004[d$VCF0004 >
              max(cov$last[grepl("VCF0050", cov$variable)])]))))
cat("codes.csv         :\n"); print(codes, row.names = FALSE)
cat(sprintf("\n  half-cleaning is off by %.3f and %.3f; not cleaning at all is off by %.3f\n",
            abs(codes$error[2]), abs(codes$error[3]), abs(codes$error[1])))
cat("\nawareness.csv     :\n"); print(aw[, c("build","respondents","levels")], row.names = FALSE)
cat(sprintf("\nzaller.csv        : %d rows, %d items x %d parties x %d bands\n",
            nrow(z), length(unique(z$item)), length(unique(z$party)),
            length(unique(z$awareness))))
cat(sprintf("resentment.csv    : %d rows, %d-%d\n",
            nrow(rag), min(rag$year), max(rag$year)))

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
