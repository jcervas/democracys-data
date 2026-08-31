# ---------------------------------------------------------------------------
# Build the partisan-economy dataset: what a question about the economy
# measures, once you know who answered it.
#
# Six files end up in this folder:
#
#   raw/question.txt        A real capture: the two questions this chapter
#                           rests on, in the codebook's own words, and five
#                           real rows as they arrive.
#   derived/national.csv    Every study year the national economy question was
#                           asked, with the share of Democrats and of
#                           Republicans who said the economy had got worse.
#   derived/personal.csv    The same years and the same two groups, on the
#                           question about the respondent's OWN finances.
#   derived/gradient.csv    2020 and 2024, split seven ways instead of three,
#                           which is where the staircase reverses.
#   derived/weighting.csv   Every year's gap computed twice, with and without
#                           the file's full-sample weight. Nothing here turns
#                           on that choice and this file is the evidence.
#   derived/checks.csv      The assertions below, printed in the brief.
#
# THE QUESTION THIS CHAPTER IS ABOUT. There is one American economy. In any
# given year it either grew or it shrank, and the answer does not depend on
# who is asked. So a survey question about whether the national economy has
# got better or worse over the past year looks like a question with a fact
# behind it -- unlike a question about abortion or ideology, where
# disagreement IS the measurement.
#
# It does not behave like one. The share of Democrats who say the economy got
# worse, minus the share of Republicans who say the same, changes SIGN every
# time the White House changes party. Across every study from 1980 to 2024
# there is no exception: under a Republican president Democrats are the more
# negative group, under a Democratic president Republicans are. The question
# is answered as though it were about the president.
#
# THE COMPARISON THAT KEEPS IT HONEST, and the reason personal.csv exists. A
# reader can object that Democrats and Republicans are different people --
# different incomes, regions and ages -- so they might genuinely experience
# different economies. The file answers that objection with a second question
# asked in the same interview: are YOU better or worse off financially than a
# year ago. That question has many true answers, one per household, and the
# groups really do differ. The national question has one true answer for
# everybody. Yet the partisan gap is LARGER on the national question than on
# the personal one in every year but 2008. Whatever the national question is
# picking up, it is not the difference between Democratic and Republican
# household finances, because that difference is smaller.
#
# WHERE THE GAP CLOSES. In 1990 and in 2008 the gap is at its narrowest.
# Those are the two worst economies in the series, and in both years majorities
# in both parties said the economy had got worse. A bad enough economy is
# still visible through partisanship. That is a limit on the finding, and it
# is in the data rather than in a caveat.
#
# CODING, from the ANES codebook:
#   VCF0004  study year
#   VCF0870  BETTER OR WORSE ECONOMY IN PAST YEAR
#            1 better, 3 stayed same, 5 worse. 8 = DK, 0 = NA, blank = not
#            asked that year. Asked 1980 onward.
#   VCF0880  BETTER OR WORSE OFF IN PAST YEAR (the respondent's own finances)
#            1 better now, 2 same, 3 worse now -- NOT the same code set as
#            VCF0870, which is the trap in reading the two together.
#   VCF0301  party identification, 7 point. 1 strong Dem ... 7 strong Rep.
#   VCF0303  the file's own 3-category collapse of VCF0301, leaners folded in
#            with the parties. 1 Democrat, 2 Independent, 3 Republican.
#   VCF0009z sample weight, full sample including the 2012-2024 web cases.
#
# WHY VCF0303 AND NOT VCF0301 FOR THE MAIN SERIES. The three-category column
# puts leaners with the party they lean toward, which is a decision, not a
# fact -- the party-id chapter is about exactly that decision. It is the right
# one here for two reasons. It is the definition under which the leaners
# behave like partisans, which party-id establishes, and gradient.csv shows
# the leaners answering the economy question like partisans too. Using the
# seven-point column and dropping the leaners moves every number in
# national.csv and changes no sign.
#
# WHY THE MAIN SERIES IS UNWEIGHTED. The sibling ANES chapters are unweighted
# and say so, and a comparison between two groups inside one survey is the
# case where it matters least. Rather than repeat the caveat and leave it
# untested, weighting.csv computes every gap both ways. The assertions below
# require that no sign differs.
#
# SOURCE. American National Election Studies, Time Series Cumulative Data File
# 1948-2024, version of 5 February 2026.
#   https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
# Requested, not handed over: the download page returns 403 to a script and 200
# to a browser, so a person has to fetch it. Not redistributed here (163 MB).
# Read from the copy the anes chapter already holds, as the party-id and
# ideology chapters do.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)
source("../../../_lib/precision.R")

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

KEEP <- c("VCF0004", "VCF0301", "VCF0303", "VCF0870", "VCF0880", "VCF0009z")
stopifnot(all(KEEP %in% hdr))
d <- reader(ifelse(hdr %in% KEEP, NA, "NULL"))
stopifnot(nrow(d) > 70000)

# --- 0. the codes, cleaned --------------------------------------------------
#
# Each column keeps only its own valid answers. Everything else -- don't know,
# refused, not asked that year -- becomes NA rather than being folded into a
# category, because a blank is not a "same".

nat <- d$VCF0870                       # 1 better, 3 same, 5 worse
nat[!nat %in% c(1, 3, 5)] <- NA
own <- d$VCF0880                       # 1 better, 2 same, 3 worse
own[!own %in% c(1, 2, 3)] <- NA
p3 <- d$VCF0303                        # 1 Dem, 2 Ind, 3 Rep (leaners in)
p3[!p3 %in% 1:3] <- NA
p7 <- d$VCF0301                        # 1 strong Dem ... 7 strong Rep
p7[!p7 %in% 1:7] <- NA
wt <- suppressWarnings(as.numeric(d$VCF0009z))
yr <- d$VCF0004

# The two questions do NOT share a code for "worse", and reading 5 as worse on
# VCF0880 would silently drop every one of its worse answers.
stopifnot(!any(own %in% 5, na.rm = TRUE))

# --- 1. which party held the White House when each study was fielded --------
#
# The ANES time series is fielded before the November election, so the
# president in office during the interviews is the one elected earlier, not
# whoever won that year. 2020 is Trump and 2024 is Biden for that reason.

PRES <- c("1980" = "D",                                   # Carter
          "1982" = "R", "1984" = "R", "1986" = "R",       # Reagan
          "1988" = "R", "1990" = "R", "1992" = "R",       # Reagan, G.H.W. Bush
          "1994" = "D", "1996" = "D", "1998" = "D",       # Clinton
          "2000" = "D",
          "2002" = "R", "2004" = "R", "2008" = "R",       # G.W. Bush
          "2012" = "D", "2016" = "D",                     # Obama
          "2020" = "R",                                   # Trump
          "2024" = "D")                                   # Biden

# --- 2. the national economy question, by party, every year -----------------

share_worse <- function(keep, worse_code, w = NULL) {
  if (!any(keep)) return(NA_real_)
  if (is.null(w)) return(100 * mean(worse_code[keep]))
  sum(w[keep] * worse_code[keep]) / sum(w[keep]) * 100
}

years <- sort(unique(yr[!is.na(nat) & !is.na(p3)]))
national <- do.call(rbind, lapply(years, function(y) {
  k <- yr == y & !is.na(nat) & !is.na(p3)
  W <- nat == 5
  dem <- k & p3 == 1; rep <- k & p3 == 3; ind <- k & p3 == 2
  data.frame(
    year = y,
    president = unname(PRES[as.character(y)]),
    n_dem = sum(dem), n_ind = sum(ind), n_rep = sum(rep),
    dem_worse = round(share_worse(dem, W), 1),
    ind_worse = round(share_worse(ind, W), 1),
    rep_worse = round(share_worse(rep, W), 1))
}))
# A gap is a subtraction of two near-equal numbers, so it is rounded here
# rather than left to the write boundary.
national$gap <- round(national$dem_worse - national$rep_worse, 1)
dd_write_csv(national, "derived/national.csv")

# --- 3. the same people, asked about their own finances ---------------------

personal <- do.call(rbind, lapply(years, function(y) {
  k <- yr == y & !is.na(own) & !is.na(p3)
  W <- own == 3
  dem <- k & p3 == 1; rep <- k & p3 == 3
  data.frame(
    year = y,
    president = unname(PRES[as.character(y)]),
    n_dem = sum(dem), n_rep = sum(rep),
    dem_worse = round(share_worse(dem, W), 1),
    rep_worse = round(share_worse(rep, W), 1))
}))
personal$gap <- round(personal$dem_worse - personal$rep_worse, 1)
dd_write_csv(personal, "derived/personal.csv")

# --- 4. seven groups instead of three, in the two most recent studies -------
#
# 2020 and 2024 are four years apart, run either side of a change of party,
# and asked the same question. Splitting by the seven-point column shows the
# leaners answering like the partisans they lean toward -- and the whole
# staircase running the other way.

LAB7 <- c("Strong Democrat", "Weak Democrat", "Independent-Democrat",
          "Independent-Independent", "Independent-Republican",
          "Weak Republican", "Strong Republican")

GYEARS <- c(2020, 2024)
gradient <- do.call(rbind, lapply(GYEARS, function(y) {
  do.call(rbind, lapply(1:7, function(i) {
    k <- yr == y & !is.na(nat) & !is.na(p7) & p7 == i
    data.frame(year = y, code = i, category = LAB7[i],
               respondents = sum(k),
               pct_worse = round(share_worse(k, nat == 5), 1))
  }))
}))
dd_write_csv(gradient, "derived/gradient.csv")

# --- 5. does the weight change anything? ------------------------------------

weighting <- do.call(rbind, lapply(years, function(y) {
  k <- yr == y & !is.na(nat) & !is.na(p3)
  kw <- k & !is.na(wt)
  W <- nat == 5
  u <- round(share_worse(k & p3 == 1, W) - share_worse(k & p3 == 3, W), 1)
  g <- round(share_worse(kw & p3 == 1, W, wt) -
             share_worse(kw & p3 == 3, W, wt), 1)
  data.frame(year = y, gap_unweighted = u, gap_weighted = g,
             difference = round(g - u, 1))
}))
dd_write_csv(weighting, "derived/weighting.csv")

# --- 6. a real capture ------------------------------------------------------

set.seed(84355)
pool <- which(!is.na(nat) & !is.na(p3) & !is.na(own))
w5 <- sort(sample(pool, 5))
cap <- file("raw/question.txt", "w")
writeLines(c(
"Two questions, asked in the same interview, in the codebook's words.",
"",
"  VCF0870  BETTER OR WORSE ECONOMY IN PAST YEAR",
"    Now thinking about the economy in the country as a whole, would",
"    you say that over the past year the nation's economy has gotten",
"    better, stayed about the same or gotten worse?",
"",
"      1. Better     3. Stayed same     5. Worse",
"",
"  VCF0880  BETTER OR WORSE OFF IN PAST YEAR",
"    We are interested in how people are getting along financially",
"    these days. Would you say that you (and your family living here)",
"    are better off or worse off financially than you were a year ago?",
"",
"      1. Better Now     2. Same     3. Worse Now",
"",
"The two do not share a code for 'worse'. One uses 5, the other 3.",
"",
"Five real rows. VCF0303 is the file's own three-way party column.",
""), cap)
utils::write.table(
  data.frame(year = yr[w5], VCF0303 = p3[w5],
             party = c("Democrat", "Independent", "Republican")[p3[w5]],
             VCF0870 = nat[w5], VCF0880 = own[w5]),
  cap, sep = "  ", quote = FALSE, row.names = FALSE)
writeLines(c("",
"Nothing in either column records who was president when it was asked.",
"That is the fact the two columns turn out to be about."), cap)
close(cap)

# --- 7. checks --------------------------------------------------------------
#
# Each row is a claim the brief makes, re-derived here. A check that cannot
# fail is worse than no check, so every one of these is a condition the data
# is free to violate: the sign agreement is over 18 independent years, the
# gap comparison counts the years rather than asserting a direction, and the
# weighting row would fail on a single flipped sign.

expected_sign <- ifelse(national$president == "R", 1, -1)
sign_ok <- sum(sign(national$gap) == expected_sign)

both <- merge(national[, c("year", "president", "gap")],
              personal[, c("year", "gap")], by = "year",
              suffixes = c("_nat", "_own"))
smaller <- sum(abs(both$gap_own) < abs(both$gap_nat))
same_dir <- sum(sign(both$gap_own) == sign(both$gap_nat))
exception <- both$year[abs(both$gap_own) >= abs(both$gap_nat)]

g20 <- gradient$pct_worse[gradient$year == 2020]
g24 <- gradient$pct_worse[gradient$year == 2024]
worst <- national$year[order(abs(national$gap))][1:2]

wsign <- sum(sign(weighting$gap_weighted) == sign(weighting$gap_unweighted))

checks <- data.frame(
  check = c(
    "study years carrying the national economy question",
    "years where the sign of the gap matches the president's party",
    "years where the personal-finance gap is smaller than the national one",
    "the only year where it is not",
    "years where the two gaps point the same direction",
    "the two narrowest gaps in the series",
    "2020: strong Democrats minus strong Republicans, points",
    "2024: the same subtraction, points",
    "years where weighting changes the sign of the gap",
    "largest difference weighting makes to any gap, points"),
  value = c(
    nrow(national),
    paste0(sign_ok, " of ", nrow(national)),
    paste0(smaller, " of ", nrow(both)),
    paste(exception, collapse = ", "),
    paste0(same_dir, " of ", nrow(both)),
    paste(sort(worst), collapse = " and "),
    dd_num(round(g20[1] - g20[7], 1)),
    dd_num(round(g24[1] - g24[7], 1)),
    nrow(weighting) - wsign,
    dd_num(max(abs(weighting$difference)))))
write.csv(checks, "derived/checks.csv", row.names = FALSE)

stopifnot(
  nrow(national) == 18,
  sign_ok == nrow(national),                  # every year, no exception
  same_dir == nrow(both),
  smaller == nrow(both) - 1,
  identical(as.integer(exception), 2008L),
  wsign == nrow(weighting),
  g20[1] > g20[7],                            # 2020: Democrats more negative
  g24[1] < g24[7],                            # 2024: the staircase reversed
  all(national$president %in% c("D", "R")),
  !any(is.na(national$gap)), !any(is.na(personal$gap)))

# --- report -----------------------------------------------------------------

cat(sprintf("\nnational.csv  : %d study years, %d-%d\n",
            nrow(national), min(years), max(years)))
print(national[, c("year", "president", "dem_worse", "rep_worse", "gap")],
      row.names = FALSE)
cat(sprintf("\n  The sign of the gap matches the president's party in %d of %d years.\n",
            sign_ok, nrow(national)))
cat("\npersonal.csv  : the same two groups on their OWN finances\n")
print(both, row.names = FALSE)
cat(sprintf("\n  Smaller than the national gap in %d of %d years (exception: %s).\n",
            smaller, nrow(both), paste(exception, collapse = ", ")))
cat("\ngradient.csv  : seven groups, two adjacent studies\n")
print(gradient[, c("year", "category", "respondents", "pct_worse")],
      row.names = FALSE)
cat("\nweighting.csv : the same gaps, weighted and not\n")
cat(sprintf("  no sign differs; largest difference %.1f points\n",
            max(abs(weighting$difference))))

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
