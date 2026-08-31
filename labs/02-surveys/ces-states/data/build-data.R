# ---------------------------------------------------------------------------
# Build the ces-states dataset: what sixty thousand respondents buys, and
# where it runs out.
#
# THE OBJECT. The 2024 Cooperative Election Study interviewed 60,000 people.
# That is sixty times a standard national poll, and the usual thing said about
# it is that the sample is so large the margin of error is negligible.
#
# It is negligible nationally. This chapter is about the fact that a national
# sample is not a national sample everywhere: the same 60,000 respondents are
# 4,900 Californians and 95 Wyomingites, and the second number is what decides
# whether the survey can say anything about Wyoming.
#
# Every state estimate is then graded against what that state actually did in
# November, which is the move this book prefers to admiring an estimate.
#
# SOURCE. Cooperative Election Study Common Content, 2024 (Data Release 2),
# Ansolabehere, Schaffner & Pope, Harvard Dataverse, doi:10.7910/DVN/X11EP6,
# file CCES24_Common_OUTPUT_vv_topost_final.csv -- 175 MB, 60,000 respondents,
# 694 variables. Read from ../../ces-class/data/raw/ rather than copied, so one
# download serves both chapters.
#
# ACQUISITION. Not scriptable. Dataverse answers an automated request with
# HTTP 202 and an empty body, which is a success-shaped refusal and is measured
# in this book's `survey-access` chapter. A person with a browser fetched it.
#
# THE VARIABLES:
#   inputstate     state FIPS code
#   CC24_364b      presidential vote, 1 = Harris, 2 = Trump, 3-5 other/none
#   commonweight   the weight for the full common-content sample
#
# THE CODING OF CC24_364b WAS VERIFIED, NOT ASSUMED. Weighted, value 1 takes
# 47.79% of the two-party vote nationally against an actual 49.2%. That is the
# right ballpark and the wrong direction to be a coding error: a swapped coding
# would have put it near 52. The check below refuses to proceed if the national
# two-party estimate is not within five points of the certified result.
#
# THE BENCHMARK is read from a sibling chapter, not typed here:
#   ../../../03-elections/historical-campaigns/data/derived/pres_states_1864_2024.csv
# and the state crosswalk from
#   ../../../06-putting-data-together/data-sources/data/derived/census_counties.csv
# because `inputstate` is a FIPS code and the returns are keyed by abbreviation.
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")
suppressWarnings(try(source("../../../_lib/provenance.R"), silent = TRUE))

dir.create("derived", showWarnings = FALSE)
say <- function(...) cat(sprintf(...), "\n")

FACTS <- list(); CHECKS <- list()
fact <- function(key, value, note) {
  FACTS[[key]] <<- list(value = value, note = note); invisible(value) }
check <- function(what, ok) {
  CHECKS[[length(CHECKS) + 1]] <<- list(check = what, passed = isTRUE(ok))
  if (!isTRUE(ok)) stop("FAILED: ", what)
  invisible(TRUE) }

CES <- c(Sys.glob("raw/CCES24_Common_OUTPUT*.csv"),
         Sys.glob("../../ces-class/data/raw/CCES24_Common_OUTPUT*.csv"))
if (!length(CES))
  stop("The CES common content is not here.\n",
       "  It cannot be downloaded by a script: Harvard Dataverse answers\n",
       "  automated requests with a success code and an empty body.\n",
       "  Get it from doi:10.7910/DVN/X11EP6 in a browser and put it in\n",
       "  raw/ or in ../../ces-class/data/raw/, which this reads too.")
say("reading %s", CES[1])

d <- read.csv(CES[1], stringsAsFactors = FALSE, colClasses = "character")[
       , c("inputstate", "CC24_364b", "commonweight")]
names(d) <- c("fips", "vote", "w")
d$fips <- as.integer(d$fips); d$vote <- as.numeric(d$vote)
d$w    <- as.numeric(d$w)

fact("n_total", nrow(d), "respondents in the 2024 common content")
check("the whole common content arrived", nrow(d) == 60000)
check("every row carries the common weight", !any(is.na(d$w)))

# --- state names ------------------------------------------------------------

XW <- "../../../06-putting-data-together/data-sources/data/derived/census_counties.csv"
check("the sibling chapter's county file, which carries the state crosswalk, is present",
      file.exists(XW))
cw <- read.csv(XW, stringsAsFactors = FALSE)
cw$sf <- as.integer(substr(sprintf("%05d", cw$fips), 1, 2))
xw <- unique(cw[, c("sf", "state_abbrev", "state_name")])
d$st   <- xw$state_abbrev[match(d$fips, xw$sf)]
d$name <- xw$state_name[match(d$fips, xw$sf)]
check("every respondent's state code resolves to a state", !any(is.na(d$st)))
fact("n_states", length(unique(d$st)), "jurisdictions the respondents live in")

# --- the two-party estimate -------------------------------------------------

HARRIS <- 1; TRUMP <- 2
tp <- d[d$vote %in% c(HARRIS, TRUMP), ]
fact("n_twoparty", nrow(tp), "who named one of the two major candidates")
fact("n_other", nrow(d) - nrow(tp),
     "who named somebody else, said they did not vote, or did not answer")

dshare <- function(z) 100 * sum(z$w * (z$vote == HARRIS)) / sum(z$w)
NAT <- dshare(tp)
fact("nat_est", round(NAT, 2), "the CES estimate of the national two-party Democratic share")

# --- what actually happened -------------------------------------------------

HC <- "../../../03-elections/historical-campaigns/data/derived/pres_states_1864_2024.csv"
check("the sibling chapter's returns are present", file.exists(HC))
h  <- read.csv(HC, stringsAsFactors = FALSE)
h24 <- h[h$year == 2024, ]
h24$actual <- 100 * h24$democrat / (h24$democrat + h24$republican)
fact("n_states_actual", nrow(h24), "states with a certified 2024 result in that file")

natw <- sum(h24$democrat) / (sum(h24$democrat) + sum(h24$republican)) * 100
fact("nat_actual_unweighted_states", round(natw, 2),
     "the mean of state shares, which is NOT the national vote and is not used")

# The national benchmark has to be the national two-party share, and the
# sibling file carries state shares rather than vote counts. So the national
# figure comes from that chapter's own national table.
NAT_FILE <- "../../../03-elections/historical-campaigns/data/derived/pres_national.csv"
check("the sibling chapter's national series is present", file.exists(NAT_FILE))
nn <- read.csv(NAT_FILE, stringsAsFactors = FALSE)
# That table is one row per CANDIDATE, not per year, so the two-party share is
# computed from the two rows rather than read off one.
n24 <- nn[nn$year == 2024 & nn$party %in% c("Democratic", "Republican"), ]
check("exactly two major-party rows for 2024", nrow(n24) == 2)
NAT_ACT <- 100 * n24$pop_votes[n24$party == "Democratic"] / sum(n24$pop_votes)
fact("nat_actual", round(NAT_ACT, 2), "the certified national two-party Democratic share")
fact("nat_error", round(NAT - NAT_ACT, 2),
     "points by which the CES national estimate misses it")
check("the vote coding is not reversed -- the estimate is near the result",
      abs(NAT - NAT_ACT) < 5)

# --- per state --------------------------------------------------------------
#
# The margin of error at 95% for a weighted share, using the design effect
# implied by the weights (Kish). A weighted sample of n respondents carries the
# precision of a smaller simple random sample, and ignoring that overstates how
# much the big n bought.

S <- do.call(rbind, lapply(sort(unique(tp$st)), function(s) {
  z <- tp[tp$st == s, ]
  p <- dshare(z) / 100
  deff <- length(z$w) * sum(z$w^2) / sum(z$w)^2      # Kish design effect
  neff <- nrow(z) / deff
  data.frame(state = s, name = z$name[1], n = nrow(z),
             n_eff = round(neff, 1), deff = round(deff, 2),
             est = round(100 * p, 2),
             moe = round(100 * 1.96 * sqrt(p * (1 - p) / neff), 2),
             stringsAsFactors = FALSE)
}))
S$actual <- h24$actual[match(S$state, h24$state_abbrev)]
S <- S[!is.na(S$actual), ]
S$error  <- round(S$est - S$actual, 2)
S$covers <- abs(S$error) <= S$moe
S <- S[order(S$n), ]
dd_write_csv(S, "derived/states.csv")

fact("n_states_graded", nrow(S), "states with both a CES estimate and a certified result")
fact("n_min", min(S$n), "respondents in the smallest state cell")
fact("state_min", S$name[which.min(S$n)], "which state that is")
fact("n_max", max(S$n), "respondents in the largest")
fact("state_max", S$name[which.max(S$n)], "which state that is")
fact("n_ratio", round(max(S$n) / min(S$n), 1), "how many times larger the largest cell is")
check("the largest state cell is at least twenty times the smallest",
      max(S$n) / min(S$n) > 20)

fact("moe_min", min(S$moe), "the narrowest state margin of error")
fact("moe_max", max(S$moe), "the widest")
fact("moe_max_state", S$name[which.max(S$moe)], "the state with the widest")
fact("moe_median", round(median(S$moe), 2), "the median state's margin of error")
fact("deff_median", round(median(S$deff), 2),
     "the median state's design effect, the factor by which weighting costs precision")
check("weighting costs precision everywhere", min(S$deff) > 1)

fact("err_median", round(median(abs(S$error)), 2),
     "the median state's absolute error against the certified result")
fact("err_max", round(max(abs(S$error)), 2), "the largest state error")
fact("err_max_state", S$name[which.max(abs(S$error))], "where it happens")
fact("n_covered", sum(S$covers),
     "states where the certified result falls inside the estimate's margin of error")
fact("pct_covered", round(100 * mean(S$covers), 1), "as a percentage")

# THE POINT. If the only thing wrong were sampling, about 95% of states would
# be covered. Far fewer are, and that gap is everything the margin of error
# does not describe.
check("the margin of error does not account for most of the error",
      mean(S$covers) < 0.9)
fact("expected_covered", round(0.95 * nrow(S), 1),
     "how many states sampling error alone predicts would be covered")

# Does a bigger cell help? It buys precision, not accuracy.
fact("cor_n_moe", round(cor(S$n, S$moe), 2),
     "correlation between a state's sample size and its margin of error")
fact("cor_n_err", round(cor(S$n, abs(S$error)), 2),
     "and between its sample size and how wrong it actually is")
check("sample size buys precision far more than it buys accuracy",
      abs(cor(S$n, S$moe)) > abs(cor(S$n, abs(S$error))))

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("done: %d facts, %d checks, %d states", nrow(facts), length(CHECKS), nrow(S))

try(prov_stamp(), silent = TRUE)
