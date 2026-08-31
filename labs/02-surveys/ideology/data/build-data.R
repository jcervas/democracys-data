# ---------------------------------------------------------------------------
# Build the ideology dataset: the two middles.
#
# Four files end up in this folder:
#
#   raw/scale.txt    A real capture: the question as it is read aloud, the
#                    codebook's own Valid block, and five real rows.
#   derived/placement.csv    Every study year, the share in each of the seven points,
#                    plus the share who declined to place themselves at all.
#   derived/middles.csv      Moderates and non-placers compared on everything the
#                    cumulative file can compare them on.
#   derived/collapse.csv     What the file's three-category column does with each
#                    group -- and what it conspicuously does NOT do.
#
# THE QUESTION THIS CHAPTER IS ABOUT. The liberal-conservative scale has a
# midpoint, "moderate, middle of the road", and it has an escape hatch,
# "haven't thought much about it". Both sit in the middle of the distribution
# and they are not the same people. Treating either as the other -- by dropping
# the non-placers as missing, or by folding them into the moderates -- changes
# who "the center" is by about a fifth of the sample.
#
# THE ESCAPE HATCH IS PART OF THE QUESTION, and this is the thing to notice
# first. It is not an interviewer's note or a refusal code. The interviewer
# reads it out:
#
#   "Where would you place yourself on this scale, or haven't you thought
#    much about this?"
#
# The survey offers the exit, and about a fifth of respondents accept it. The
# codebook then lists code 9 under **Valid**, not under Missing. A person who
# drops the nines as missing data is not tidying the file; they are overruling
# its own classification of what counts as an answer.
#
# THE CONTRAST WITH PARTY IDENTIFICATION, which is the companion chapter. Party
# ID also has a hidden decision -- whether leaners are independents -- and the
# file resolves it silently in VCF0303, whose note collapses leaners into the
# parties. Ideology's convenience column, VCF0804, does the opposite: it
# collapses seven points to three and **keeps code 9 as its own category**. Two
# adjacent design choices in the same file, one hiding a judgment call and one
# preserving it. Neither is announced anywhere a casual user would look.
#
# WHAT THE COMPARISON FINDS. Moderates and non-placers differ on nearly
# everything measurable, and the largest gap is education: a quarter of
# moderates hold a college degree against under a tenth of non-placers. The
# result worth arguing about is partisanship. Non-placers are MORE likely to be
# strong partisans than moderates are. Party identity does not require the
# vocabulary of ideology; a person can be a strong Democrat and decline to say
# what liberal means.
#
# CODING, from the ANES codebook:
#   VCF0004  study year
#   VCF0803  liberal-conservative, 7 point.  1 extremely liberal ...
#            4 moderate, middle of the road ... 7 extremely conservative.
#            9 = "DK; haven't thought much about it" -- listed as VALID.
#            0 = NA; no post interview; not administered.  NOT valid.
#   VCF0804  the same, collapsed to 1 liberal / 2 moderate / 3 conservative,
#            with 9 retained.
#   VCF0301  party identification, 7 point (see the party-id chapter)
#   VCF0310  interest in the campaign, 1-3
#   VCF0110  education, 4 categories; 4 = college or advanced degree
#   VCF0703  turnout, 1 did not vote, 2 voted
#   VCF0838  abortion, 1-4 (see the abortion chapter)
#   VCF0806  government health insurance, 1-7
#
# SOURCE. American National Election Studies, Time Series Cumulative Data File
# 1948-2024, version of 5 February 2026.
#   https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
# Requested, not handed over; 403 to a script, 200 to a browser. Not
# redistributed here (163 MB).
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

CAND <- path.expand(c(
  "raw/anes_timeseries_cdf_csv_20260205.csv",
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

KEEP <- c("VCF0004", "VCF0803", "VCF0804", "VCF0301", "VCF0310",
          "VCF0110", "VCF0703", "VCF0838", "VCF0806")
stopifnot(all(KEEP %in% hdr))
d <- reader(ifelse(hdr %in% KEEP, NA, "NULL"))
stopifnot(nrow(d) > 70000)

LAB <- c("Extremely liberal", "Liberal", "Slightly liberal",
         "Moderate, middle of the road", "Slightly conservative",
         "Conservative", "Extremely conservative")

ide <- d$VCF0803
ide[!ide %in% c(1:7, 9)] <- NA          # 0 is genuinely missing; 9 is not
MOD <- which(ide == 4)
DK  <- which(ide == 9)

# --- 1. a real capture ------------------------------------------------------

set.seed(84355)
w <- sort(sample(which(!is.na(ide)), 5))
cap <- file("raw/scale.txt", "w")
writeLines(c(
"The question, as the interviewer reads it:",
"",
"  We hear a lot of talk these days about liberals and conservatives.",
"  Here is a 7-point scale on which the political views that people",
"  might hold are arranged from extremely liberal to extremely",
"  conservative. Where would you place yourself on this scale,",
"  OR HAVEN'T YOU THOUGHT MUCH ABOUT THIS?",
"",
"The codebook's own Valid block -- note where 9 appears:",
"",
"  Valid   1. Extremely liberal",
"          2. Liberal",
"          3. Slightly liberal",
"          4. Moderate, middle of the road",
"          5. Slightly conservative",
"          6. Conservative",
"          7. Extremely conservative",
"          9. DK; haven't thought much about it",
"",
"  Missing 0. NA; no Post IW; form III,IV (1972); R not administered",
"",
"Five random rows as they arrive:",
""), cap)
utils::write.table(
  data.frame(year = d$VCF0004[w], VCF0803 = ide[w],
             means = ifelse(ide[w] == 9, "haven't thought much about it",
                            LAB[pmin(ide[w], 7)]),
             VCF0804 = d$VCF0804[w]),
  cap, sep = "  ", quote = FALSE, row.names = FALSE)
writeLines(c("",
"A 9 is not a gap in the data. It is an answer to the question that",
"was asked, and the codebook files it under Valid."), cap)
close(cap)

# --- 2. placement, every year -----------------------------------------------

yrs <- sort(unique(d$VCF0004[!is.na(ide)]))
pl <- do.call(rbind, lapply(yrs, function(y) {
  v <- ide[d$VCF0004 == y & !is.na(ide)]
  if (length(v) < 100) return(NULL)
  data.frame(year = y, n = length(v),
             liberal      = round(100 * mean(v %in% 1:3), 1),
             moderate     = round(100 * mean(v == 4), 1),
             conservative = round(100 * mean(v %in% 5:7), 1),
             not_placed   = round(100 * mean(v == 9), 1))
}))
write.csv(pl, "derived/placement.csv", row.names = FALSE)

# --- 3. the two middles compared --------------------------------------------

pid <- d$VCF0301; pid[!pid %in% 1:7] <- NA
int <- d$VCF0310; int[!int %in% 1:3] <- NA
edu <- d$VCF0110; edu[!edu %in% 1:4] <- NA
vot <- d$VCF0703; vot[!vot %in% 1:2] <- NA
abo <- d$VCF0838; abo[!abo %in% 1:4] <- NA
hin <- d$VCF0806; hin[!hin %in% 1:7] <- NA

pc  <- function(x, k) round(100 * mean(x[k], na.rm = TRUE), 1)
mid <- data.frame(
  measure = c("Respondents",
              "Holds a college degree",
              "Voted",
              "Very much interested in the campaign",
              "Pure independent, no party at all",
              "Strong partisan, strong Democrat or strong Republican",
              "Abortion should always be permitted",
              "Mean position on government health insurance (1-7)"),
  moderate = c(length(MOD),
               pc(edu == 4, MOD), pc(vot == 2, MOD), pc(int == 3, MOD),
               pc(pid == 4, MOD), pc(pid %in% c(1, 7), MOD),
               pc(abo == 4, MOD), round(mean(hin[MOD], na.rm = TRUE), 2)),
  not_placed = c(length(DK),
               pc(edu == 4, DK), pc(vot == 2, DK), pc(int == 3, DK),
               pc(pid == 4, DK), pc(pid %in% c(1, 7), DK),
               pc(abo == 4, DK), round(mean(hin[DK], na.rm = TRUE), 2)),
  unit = c("count", rep("%", 6), "mean"))
write.csv(mid, "derived/middles.csv", row.names = FALSE)

# --- 4. what the convenience column does ------------------------------------

thr <- d$VCF0804
k <- !is.na(ide) & thr %in% c(1:3, 9)
coll <- as.data.frame(table(ide[k], thr[k]))
names(coll) <- c("seven_point", "three_category", "respondents")
coll <- coll[coll$respondents > 0, ]
coll$seven_point <- ifelse(coll$seven_point == 9, "9 (haven't thought much)",
                           paste0(coll$seven_point, " ",
                                  LAB[pmin(as.integer(as.character(
                                    coll$seven_point)), 7)]))
coll$three_category <- c("1 Liberal", "2 Moderate", "3 Conservative",
                         "9 (haven't thought much)")[
                           match(coll$three_category, c(1, 2, 3, 9))]
write.csv(coll, "derived/collapse.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

last <- max(pl$year)
cat(sprintf("\nplacement.csv : %d study years, %d-%d\n", nrow(pl),
            min(pl$year), max(pl$year)))
print(pl[pl$year %in% c(1972, 1984, 2000, 2016, last), ], row.names = FALSE)
cat(sprintf("\n  Non-placers fall from %.1f%% in %d to %.1f%% in %d.\n",
            pl$not_placed[1], pl$year[1],
            pl$not_placed[nrow(pl)], pl$year[nrow(pl)]))
cat(sprintf("  Moderates over the same span: %.1f%% to %.1f%%.\n",
            pl$moderate[1], pl$moderate[nrow(pl)]))
cat("\nmiddles.csv   : the two middles are not the same people\n")
print(mid[, c("measure", "moderate", "not_placed")], row.names = FALSE)
cat("\ncollapse.csv  : VCF0804 keeps code 9 as its own category --\n")
cat("  unlike VCF0303, which folds party leaners away silently.\n")

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
