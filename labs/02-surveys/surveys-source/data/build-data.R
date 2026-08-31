# ---------------------------------------------------------------------------
# Build the survey-instrument dataset: what a survey is, and what it is for.
#
# This is a SOURCE chapter, not a finding chapter. Its subject is the
# instrument -- what a survey can establish that no administrative record can,
# what it cannot establish at any sample size, and the three design decisions
# that stand between a person's answer and a published number.
#
# Five files end up in this folder:
#
#   raw/wording.txt   Three real question texts, verbatim, chosen because each
#                     one shows the wording doing measurement work.
#   derived/capacity.csv      ANES respondents per state in the most recent study, and
#                     the margin of error that implies. This is the whole
#                     ANES-versus-CES argument in one table.
#   derived/moe.csv           Margin of error at 50% by subgroup size, so a reader can
#                     look up what any n can support.
#   derived/weighting.csv     The largest weighting adjustments in the CES 2024
#                     benchmarks, biggest first.
#   derived/instruments.csv   Census, ANES and CES side by side on what each can and
#                     cannot answer.
#
# THE ARGUMENT. A census ENUMERATES: it tries to count everyone, and what it
# records is what a person IS, in the categories the form allows. A survey
# ELICITS: it asks a sample of people questions, and what it records is what a
# person SAYS, in the categories the question allows. These are different
# operations and they fail in different places.
#
#   The census cannot tell you whether anyone trusts the government, because
#   no administrative record anywhere contains an attitude. There is no
#   register of opinions and there never will be.
#
#   The survey cannot tell you how many people live on a block, and no
#   increase in sample size fixes this in the way people expect -- see
#   derived/capacity.csv, where the median STATE in the most recent ANES has 26
#   respondents and a margin of error of about 19 points.
#
# WHY BOTH ANES AND CES. They are the same kind of instrument with opposite
# investments. ANES buys DEPTH and TIME: the same questions since 1948, long
# interviews, and a design built so that 1972 and 2024 can be compared. CES
# buys BREADTH: about sixty thousand respondents in a single year, which is
# what makes a state-level or small-subgroup estimate possible at all. Neither
# is better. They answer different questions, and the reason is arithmetic that
# this chapter shows rather than asserts.
#
# THE THREE DECISIONS between an answer and a number, each with an exhibit:
#   1. WORDING. The question is not a window onto an opinion; it is the
#      instrument that produces one. raw/wording.txt shows three cases where
#      the wording visibly creates the category being counted.
#   2. WHO ANSWERS. People who take surveys are not a random slice of the
#      country, and the fix -- weighting -- is large. weighting.csv shows the
#      CES 2024 registration item moving 19 points.
#   3. HOW MANY ANSWERED. moe.csv is the arithmetic that decides which
#      questions a given survey may be asked at all.
#
# SOURCES.
#   American National Election Studies, Time Series Cumulative Data File
#   1948-2024, version of 5 February 2026. Requested, not handed over: the
#   download page returns 403 to a script and 200 to a browser. 163 MB, not
#   redistributed here.
#     https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
#   Cooperative Election Study 2024 Common Content, Ansolabehere, Schaffner &
#   Pope, Harvard Dataverse, doi:10.7910/DVN/X11EP6. The weighting figures are
#   read from ces2024_benchmarks.csv, which the ces-class chapter derived from
#   the 184 MB respondent file; that file is not redistributed either.
#
# CODING, from the ANES codebook:
#   VCF0004  study year
#   VCF0900  state of interview (FIPS); 0 and 99 are not states
#   VCF0009z sample weight
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

KEEP <- c("VCF0004", "VCF0900", "VCF0009z")
stopifnot(all(KEEP %in% hdr))
d <- reader(ifelse(hdr %in% KEEP, NA, "NULL"))
stopifnot(nrow(d) > 70000)

LAST <- max(d$VCF0004, na.rm = TRUE)
now  <- !is.na(d$VCF0004) & d$VCF0004 == LAST

# --- 1. the question texts --------------------------------------------------
#
# Verbatim from the ANES codebook. Each is here because the wording is doing
# something a summary of the question would hide.

writeLines(c(
"Three questions, exactly as the interviewer reads them. Each one is",
"chosen because the wording is not a window onto an opinion -- it is",
"the instrument that produces one.",
"",
"---- 1. The escape hatch is inside the question --------------------",
"",
"  We hear a lot of talk these days about liberals and conservatives.",
"  Here is a 7-point scale on which the political views that people",
"  might hold are arranged from extremely liberal to extremely",
"  conservative. Where would you place yourself on this scale,",
"  OR HAVEN'T YOU THOUGHT MUCH ABOUT THIS?",
"",
"  The last clause invites a fifth of respondents to decline, and the",
"  codebook files that answer as VALID. A survey that omitted the",
"  clause would report a country with no such group in it.",
"",
"---- 2. One column, three questions --------------------------------",
"",
"  Generally speaking, do you usually think of yourself as a",
"  Republican, a Democrat, an Independent, or what?",
"    ...if Republican or Democrat:",
"       Would you call yourself a STRONG [R/D] or a NOT VERY",
"       STRONG [R/D]?",
"    ...if Independent or other:",
"       Do you think of yourself as CLOSER to the Republican",
"       or Democratic party?",
"",
"  The famous seven-point scale is the record of a branch. Nothing",
"  in the seven numbers says which branch a respondent came down.",
"",
"---- 3. The options are the opinion --------------------------------",
"",
"  1  By law, abortion should never be permitted.",
"  2  The law should permit abortion only in case of rape, incest,",
"     or when the woman's life is in danger.",
"  3  The law should permit abortion for reasons other than rape,",
"     incest, or danger to the woman's life, but only after the need",
"     for the abortion has been clearly established.",
"  4  By law, a woman should always be able to obtain an abortion",
"     as a matter of personal choice.",
"",
"  There is no fifth option, and a person whose view is not on the",
"  card must pick the nearest one. Whatever share option 3 receives",
"  is a fact about the card as much as about the country."),
"raw/wording.txt")

# --- 2. what the most recent study can support ------------------------------

st <- d$VCF0900[now]
st <- st[!is.na(st) & !st %in% c(0, 99)]
tb <- sort(table(st))

moe_at <- function(n) round(100 * 1.96 * sqrt(0.25 / n), 1)

cap <- data.frame(
  quantity = c("Respondents in the study",
               "States and equivalents represented",
               "Respondents in the median state",
               "Respondents in the smallest state",
               "Respondents in the largest state",
               "Margin of error, median state, at 50%",
               "Margin of error, smallest state, at 50%",
               "Margin of error, whole sample, at 50%"),
  value = c(sum(now), length(tb), as.integer(median(tb)),
            as.integer(min(tb)), as.integer(max(tb)),
            moe_at(median(tb)), moe_at(min(tb)), moe_at(sum(now))),
  unit = c(rep("count", 5), rep("+/- points", 3)))
write.csv(cap, "derived/capacity.csv", row.names = FALSE)

# --- 3. the arithmetic that decides what may be asked -----------------------
#
# A 60,000-respondent survey is put beside ANES using the SAME state
# proportions, so the comparison is about size and nothing else.

med_share <- as.numeric(median(tb)) / sum(tb)
ns <- c(25, 50, 100, 200, 400, 1000, 2500, 5500, 60000)
mo <- data.frame(respondents = ns, margin_of_error = moe_at(ns))
mo$what_it_can_support <- c(
  "nothing anyone should publish",
  "nothing anyone should publish",
  "a large difference, if you are lucky",
  "a large difference",
  "a moderate difference",
  "a national estimate",
  "a national estimate, or a big subgroup",
  "roughly one ANES study",
  "roughly one CES study")
write.csv(mo, "derived/moe.csv", row.names = FALSE)

# --- 4. weighting -----------------------------------------------------------

B <- "../../ces-class/data/derived/ces2024_benchmarks.csv"
if (file.exists(B)) {
  b <- read.csv(B)
  b$adjustment <- round(b$pct_weighted - b$pct_unweighted, 1)
  b <- b[order(-abs(b$adjustment)), ]
  w <- head(b[, c("variable", "category", "n", "pct_unweighted",
                  "pct_weighted", "adjustment")], 10)
  write.csv(w, "derived/weighting.csv", row.names = FALSE)
} else {
  stop("ces2024_benchmarks.csv not found beside the ces-class chapter")
}

# --- 5. the three instruments side by side ----------------------------------
#
# Hand-authored, but every row is a statement about what the instrument does,
# not an opinion about which is better.

inst <- data.frame(
  question = c(
    "How many people live on this block?",
    "What share of adults in Wyoming are registered?",
    "Does this person trust the federal government?",
    "Has the country's view of abortion changed since 1980?",
    "Do Black Republicans differ from white Republicans on immigration?",
    "What is this person's race, as they describe it?"),
  census = c("Yes, by enumeration", "Yes, by enumeration",
             "No -- no record contains an attitude",
             "No -- the question is never asked",
             "No -- neither variable exists",
             "Yes, in the categories the form allows"),
  anes = c("No -- a sample cannot count a block",
           "No -- too few respondents per state",
           "Yes, and since 1958",
           "Yes -- this is what it is for",
           "No -- the subgroup is too small",
           "Yes, in the categories the question allows"),
  ces = c("No -- a sample cannot count a block",
          "Yes, at about +/- 6 points",
          "Yes, but only back to 2006",
          "No -- the series is too short",
          "Yes -- this is what the sample size is for",
          "Yes, in the categories the question allows"))
write.csv(inst, "derived/instruments.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

gv <- function(q) cap$value[cap$quantity == q]
cat(sprintf("\ncapacity.csv  : the %d ANES study\n", LAST))
print(cap, row.names = FALSE)
cat(sprintf("\n  The median state has %d respondents: +/- %.1f points.\n",
            gv("Respondents in the median state"),
            gv("Margin of error, median state, at 50%")))
cat(sprintf("  At 60,000 respondents split the same way it would be %d, +/- %.1f.\n",
            round(60000 * med_share), moe_at(60000 * med_share)))
cat("\nweighting.csv : the largest CES 2024 adjustments\n")
print(w[1:4, ], row.names = FALSE)
cat(sprintf("\n  The registration item moves %.1f points between the raw\n",
            abs(w$adjustment[1])))
cat("  interviews and the published estimate.\n")
cat(sprintf("\nmoe.csv       : %d sizes\ninstruments.csv: %d questions x 3 instruments\n",
            nrow(mo), nrow(inst)))

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
