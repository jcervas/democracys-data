# ---------------------------------------------------------------------------
# Build the thermometers dataset: warmth toward the two parties, 1978-2024.
#
# THE OBJECT. Since 1964 the ANES has handed respondents a "feeling
# thermometer": rate this group from 0 to 100, where 50 is neither warm nor
# cold. Two of those ratings are the Democratic Party and the Republican Party.
# Subtract one from the other for a partisan and you have the most-used measure
# of AFFECTIVE POLARIZATION -- how much more warmly people feel toward their
# own side than the other one.
#
# That difference has roughly doubled since the 1970s, and this script writes
# both halves of it, because the halves do not behave the same way at all.
#
# SOURCE. American National Election Studies, Time Series Cumulative Data File,
# 1948-2024, CSV distribution `anes_timeseries_cdf_csv_20260205.csv`
# (156 MB, 73,745 respondents, 1,030 variables).
#   https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
#
# ACQUISITION IS THE PART WORTH NOTICING. The file is free and it is not
# downloadable by a script. ANES requires an account and agreement to terms,
# and serves the file only to a logged-in browser session. It was fetched by a
# person and placed in raw/, where it is preserved rather than discarded. A
# reader rebuilding this chapter has to make an account first, which is the
# single largest difference between this source and the GSS.
#
# THE VARIABLES, verified against the codebook PDFs kept beside the data in
# ../../anes/data/raw/ rather than from memory:
#   VCF0004   year of study
#   VCF0218   feeling thermometer, Democratic Party
#   VCF0224   feeling thermometer, Republican Party
#   VCF0301   party identification, 7 points (1-3 Democrat, 4 independent,
#             5-7 Republican)
#   VCF0009z  the weight ANES supplies for the full cumulative file
#
# 98 AND 99 ARE NOT TEMPERATURES, AND THIS IS THE TRAP IN THE FILE. The scale
# everyone describes as "0 to 100" is coded 0-97 in the cumulative file: 97 is
# the top code, and 98 and 99 are missing-value codes ("don't know", "not
# ascertained"). A handful of -8s are a third such code.
#
# Nothing about that is hidden and nothing about it is visible either. There is
# no value of 100 anywhere in the column, so a 98 does not look out of range --
# it looks like somebody who feels very warmly indeed. Averaging without
# dropping them counts every "don't know" as near-maximum warmth, and because
# the codes are more common in some years than others, the error moves the
# trend rather than merely offsetting it.
#
# The verified counts are written out below rather than asserted.
#
# THE FILE IS SHARED WITH THE `anes` CHAPTER and read from that chapter's raw/
# rather than copied. One 156 MB file, two chapters, the same borrowing
# `gss-confidence` and `gender-gap` do with the GSS.
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

CAND <- c(Sys.glob("raw/anes_timeseries_cdf_csv_*.csv"),
          Sys.glob("../../anes/data/raw/anes_timeseries_cdf_csv_*.csv"))
if (!length(CAND))
  stop("The ANES cumulative file is not here.\n",
       "  It cannot be downloaded by a script: ANES requires an account.\n",
       "  Get anes_timeseries_cdf_csv_*.csv from\n",
       "  https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/\n",
       "  and put it in raw/ (or in ../../anes/data/raw/, which this reads too).")
SRC <- CAND[1]
say("reading %s", SRC)

V <- c(year = "VCF0004", tdem = "VCF0218", trep = "VCF0224",
       pid = "VCF0301", w = "VCF0009z")
d <- read.csv(SRC, stringsAsFactors = FALSE, colClasses = "character")[, V]
names(d) <- names(V)
for (v in names(d)) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))

fact("src_rows", nrow(d), "respondents in the cumulative file")
fact("src_first", min(d$year), "the first study year in it")
fact("src_last",  max(d$year), "and the most recent")
check("the whole cumulative file arrived", nrow(d) > 70000)

# --- the codes that are not temperatures ------------------------------------

TOP <- 97                                  # the top of the scale as coded
allt <- c(d$tdem, d$trep)
fact("top_code", TOP, "the highest value that is a temperature")
fact("n_at_top", sum(allt == TOP, na.rm = TRUE), "ratings given at the top of the scale")
fact("n_code98", sum(allt == 98, na.rm = TRUE), "entries coded 98, which means don't know")
fact("n_code99", sum(allt == 99, na.rm = TRUE), "entries coded 99, which means not ascertained")
fact("n_negative", sum(allt < 0, na.rm = TRUE), "entries coded negative, a third missing code")
fact("n_at_100", sum(allt == 100, na.rm = TRUE),
     "ratings of exactly 100, on a scale everybody calls nought to a hundred")
check("there is no value of 100 in a column described as running to 100",
      sum(allt == 100, na.rm = TRUE) == 0)
check("the codes that masquerade as warmth are present and worth removing",
      sum(allt %in% c(98, 99), na.rm = TRUE) > 1000)

# What believing 98 and 99 would cost: the average warmth with and without.
naive <- mean(allt[!is.na(allt) & allt >= 0], na.rm = TRUE)
clean <- mean(allt[!is.na(allt) & allt >= 0 & allt <= TOP], na.rm = TRUE)
fact("mean_naive", round(naive, 2), "average rating if the codes are believed")
fact("mean_clean", round(clean, 2), "and if they are dropped")
fact("mean_inflation", round(naive - clean, 2), "degrees of warmth the codes invent")
check("believing the codes warms the country", naive > clean)

ok <- !is.na(d$tdem) & !is.na(d$trep) &
      d$tdem >= 0 & d$trep >= 0 & d$tdem <= TOP & d$trep <= TOP & !is.na(d$w)
fact("n_both_therms", sum(ok), "respondents who rated both parties and carry a weight")

# --- heaping ----------------------------------------------------------------
#
# A 0-100 scale is not used as a 0-100 scale. People answer in round numbers,
# and three numbers do most of the work. This is computed before any averaging,
# because it is the reason an average of this column is a coarser thing than it
# looks.

th <- c(d$tdem[ok], d$trep[ok])
fact("n_ratings", length(th), "individual party ratings behind everything below")
for (k in c(0, 50, 85, TOP)) {
  fact(paste0("pct_at_", k), round(100 * mean(th == k), 1),
       paste("percent of all ratings given as exactly", k))
}
fact("pct_multiple_10", round(100 * mean(th %% 10 == 0), 1),
     "percent that are multiples of ten")
fact("distinct_used", length(unique(th)), "distinct values used out of the 98 available")
check("the scale is used far more coarsely than its 98 points allow",
      mean(th %% 10 == 0) > 0.7)

# --- the two halves ---------------------------------------------------------
#
# For each partisan, warmth toward their OWN party and toward the OTHER one.
# Independents (pid 4) have no own party and are excluded from the pair; they
# are counted so the exclusion is visible.

s <- d[ok & d$pid %in% c(1, 2, 3, 5, 6, 7), ]
fact("n_partisans", nrow(s), "partisans, including leaners, in the paired series")
fact("n_independents", sum(ok & d$pid == 4),
     "pure independents, who have no in-party and are not in it")

MINN <- 200
S <- do.call(rbind, lapply(sort(unique(s$year)), function(y) {
  z <- s[s$year == y, ]
  if (nrow(z) < MINN) return(NULL)
  dem <- z$pid <= 3
  inn <- c(z$tdem[dem],  z$trep[!dem])
  out <- c(z$trep[dem],  z$tdem[!dem])
  ww  <- c(z$w[dem],     z$w[!dem])
  data.frame(year = y, n = nrow(z),
             in_party  = round(sum(ww * inn) / sum(ww), 2),
             out_party = round(sum(ww * out) / sum(ww), 2),
             stringsAsFactors = FALSE)
}))
S$gap <- round(S$in_party - S$out_party, 2)
dd_write_csv(S, "derived/thermometers_by_year.csv")

fact("min_n", MINN, "the respondent floor a year must clear to be reported")
fact("n_years", nrow(S), "study years that clear it")
fact("first_year", min(S$year), "the first year with both thermometers and enough people")
fact("last_year",  max(S$year), "the last")
check("the series spans at least four decades", max(S$year) - min(S$year) >= 40)

A <- S[which.min(S$year), ]; Z <- S[which.max(S$year), ]
fact("in_first",  A$in_party,  "warmth toward one's own party in the first year")
fact("in_last",   Z$in_party,  "and in the last")
fact("out_first", A$out_party, "warmth toward the other party in the first year")
fact("out_last",  Z$out_party, "and in the last")
fact("gap_first", A$gap, "the gap between them then")
fact("gap_last",  Z$gap, "and now")
fact("in_move",   round(Z$in_party - A$in_party, 2),   "how far in-party warmth moved")
fact("out_move",  round(Z$out_party - A$out_party, 2), "how far out-party warmth moved")
fact("share_from_out",
     round(100 * abs(Z$out_party - A$out_party) /
           (abs(Z$out_party - A$out_party) + abs(Z$in_party - A$in_party)), 1),
     "percent of the total movement contributed by the out-party half")

# THE FINDING. The gap grew and the two halves did not share the work.
check("the gap is wider at the end than at the start", Z$gap > A$gap)
check("out-party warmth moved further than in-party warmth",
      abs(Z$out_party - A$out_party) > abs(Z$in_party - A$in_party))
fact("gap_max", max(S$gap), "the widest the gap ever gets")
fact("gap_max_year", S$year[which.max(S$gap)], "the year it does")
fact("out_min", min(S$out_party), "the coldest the other party is ever rated")
fact("out_min_year", S$year[which.min(S$out_party)], "the year that happens")

# How much of the change is people using the bottom of the scale at all.
below <- do.call(rbind, lapply(sort(unique(s$year)), function(y) {
  z <- s[s$year == y, ]; if (nrow(z) < MINN) return(NULL)
  dem <- z$pid <= 3
  out <- c(z$trep[dem], z$tdem[!dem]); ww <- c(z$w[dem], z$w[!dem])
  data.frame(year = y, pct_zero = round(100 * sum(ww * (out == 0)) / sum(ww), 2),
             stringsAsFactors = FALSE)
}))
dd_write_csv(below, "derived/out_party_zero.csv")
fact("zero_first", below$pct_zero[which.min(below$year)],
     "percent rating the other party exactly 0 in the first year")
fact("zero_last",  below$pct_zero[which.max(below$year)], "and in the last")
check("rating the other party at absolute zero became far more common",
      below$pct_zero[which.max(below$year)] > 3 * below$pct_zero[which.min(below$year)])

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("done: %d facts, %d checks, %d years", nrow(facts), length(CHECKS), nrow(S))

try(prov_stamp(), silent = TRUE)
