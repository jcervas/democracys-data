# ---------------------------------------------------------------------------
# Build the rank-size dataset: six quantities from six other chapters, each
# sorted largest to smallest, plus what happens to a straight-line fit as you
# change how much of the tail you are willing to ignore.
#
# Five files end up in derived/:
#
#   derived/ranks.csv    rank and value for each series, thinned for drawing
#   derived/summary.csv  one row per series: n, median, mean, concentration
#   derived/fits.csv     slope and R-squared as a function of how much of the
#                        data the line is fitted to
#   derived/zipf.csv     what Zipf's rule predicts at ranks 1, 10, 100, 1000
#                        and what is actually there
#   derived/facts.csv    single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Sources ----------------------------------------------------------------
#
# Everything here is the committed output of another chapter in this book. The
# point of the chapter is that six quantities collected for six unrelated
# reasons have the same shape, so they are deliberately not re-derived: each is
# read exactly as its own chapter published it.
#
#   surnames        Census Bureau, 2010surnames genealogy file, via
#                   ../../../01-census-bureau/surnames/data/derived/census_surnames.csv
#   counties        Census Bureau, 2020 decennial apportionment population, via
#                   ../../../03-elections/mapping/data/derived/counties.csv
#   receipts        Federal Election Commission, weball candidate summary, via
#                   ../../campaign-finance/data/derived/fec_candidates_2024.csv
#   IE committees   Federal Election Commission, independent expenditures, via
#                   ../../finance-network/data/derived/committees.csv
#   IE targets      the same FEC independent_expenditure file, by candidate, via
#                   ../../finance-network/data/derived/candidates.csv
#   pageviews       Wikimedia Foundation Pageviews API, English Wikipedia, via
#                   ../../media-attention/data/derived/wiki_attention_2024.csv

SRC <- list(
  list(key = "surnames",  file = "../../../01-census-bureau/surnames/data/derived/census_surnames.csv",
       col = "count", who = function(d) d$name,
       # THE BIGGEST ROW IN THE SURNAME FILE IS NOT A SURNAME. The Census
       # publishes every name held by 100 or more people and sweeps the rest
       # into one line called ALL OTHER NAMES -- 29.3 million people, which
       # would outrank SMITH twelve times over and sit at the top left of the
       # figure as the single most important-looking point in the chapter. It
       # is a residual, not a name, and the file flags it by giving it rank 0.
       drop = function(d) d$rank == 0, drop_n = 1,
       drop_why = "the Census's residual bucket for every name held by fewer than 100 people",
       label = "People sharing a surname", short = "Surnames",
       what = "surname", unit = "people", chapter = "surnames"),
  list(key = "counties",  file = "../../../03-elections/mapping/data/derived/counties.csv",
       col = "pop", who = function(d) paste0(d$name, ", ", d$state),
       label = "County population", short = "Counties",
       what = "county", unit = "people", chapter = "mapping"),
  list(key = "receipts",  file = "../../campaign-finance/data/derived/fec_candidates_2024.csv",
       col = "ttl_receipts", who = function(d) d$cand_name,
       label = "Money a candidate raised", short = "Candidate receipts",
       what = "candidate", unit = "dollars", chapter = "campaign-finance"),
  list(key = "committees", file = "../../finance-network/data/derived/committees.csv",
       col = "total", who = function(d) d$name,
       label = "Outside money a committee spent", short = "Outside spenders",
       what = "committee", unit = "dollars", chapter = "finance-network"),
  list(key = "targets",   file = "../../finance-network/data/derived/candidates.csv",
       col = "total", who = function(d) d$name,
       label = "Outside money spent on a candidate", short = "Outside targets",
       what = "candidate", unit = "dollars", chapter = "finance-network"),
  list(key = "pageviews", file = "../../media-attention/data/derived/wiki_attention_2024.csv",
       col = "views",
       who = function(d) paste0(gsub("_", " ", d$article), ", ", d$date),
       # THIS SERIES WAS WRONG WHEN THIS CHAPTER WAS FIRST BUILT, and looking
       # at its tail is what found the problem. The English Wikipedia article
       # on JD Vance sat at the title "J. D. Vance" until it was renamed in
       # late July 2024, and the Pageviews API reports traffic by TITLE. Asked
       # for the title the article has now, it returned a median of 1 to 6
       # views a day from January to June -- for a sitting United States
       # senator -- which is redirect traffic rather than readership, and
       # which parked roughly 190 article-days at the very bottom of this
       # chapter's smallest series. A chapter about the bottom of a
       # distribution cannot use a fake bottom.
       #
       # It was fixed upstream rather than patched here: media-attention now
       # requests every title the article has held and sums them, so nothing
       # is dropped at this end. See that chapter's section on the rename.
       label = "Readers of one article in one day", short = "Article-days",
       what = "article-day", unit = "views", chapter = "media-attention"))

for (s in SRC) stopifnot(file.exists(s$file))

DROPPED <- list()

get <- function(s) {
  d <- read.csv(s$file, stringsAsFactors = FALSE)
  if (!is.null(s$drop)) {
    k <- which(s$drop(d))
    stopifnot(length(k) == s$drop_n)
    DROPPED[[s$key]] <<- list(n = length(k), who = s$who(d)[k[1]],
                              value = max(d[[s$col]][k]), why = s$drop_why)
    d <- d[-k, , drop = FALSE]
  }
  v <- suppressWarnings(as.numeric(d[[s$col]]))
  w <- s$who(d)
  # ZEROS ARE DROPPED, AND THAT IS A DECISION. A rank-size plot is drawn on
  # log axes and log(0) does not exist, so every quantity here is restricted
  # to the units that have any of it at all. How many were dropped is written
  # to summary.csv rather than left implicit: for candidate receipts it is not
  # a rounding error, it is every candidate who filed and raised nothing.
  ok <- !is.na(v) & v > 0
  o  <- order(v[ok], decreasing = TRUE)
  list(pos = v[ok][o], who = w[ok][o],
       zero = sum(!is.na(v) & v == 0), na = sum(is.na(v)))
}

gini <- function(v) {
  v <- sort(v); n <- length(v)
  sum((2 * seq_len(n) - n - 1) * v) / (n * sum(v))
}

# --- Ranks, thinned ----------------------------------------------------------
#
# 162,254 surnames cannot travel to a browser and do not need to: on a log
# axis, ranks 100,000 and 100,001 land on the same pixel. Keeping points at
# log-spaced ranks is lossless for the figure and two orders of magnitude
# smaller. The first fifty ranks are kept in full, because the head of the
# curve is where the shape is decided.

thin <- function(n, keep = 320) {
  if (n <= keep) return(seq_len(n))
  sort(unique(c(seq_len(min(50, n)),
                round(10^seq(log10(1), log10(n), length.out = keep)), n)))
}

ranks <- list(); summ <- list(); fits <- list()
for (s in SRC) {
  g <- get(s)
  v <- g$pos; n <- length(v); r <- seq_len(n)
  i <- thin(n)
  ranks[[s$key]] <- data.frame(series = s$key, rank = i, value = v[i],
                               who = g$who[i], stringsAsFactors = FALSE)

  top1 <- max(1, round(0.01 * n))
  summ[[s$key]] <- data.frame(
    series = s$key, label = s$label, short = s$short, what = s$what,
    unit = s$unit, chapter = s$chapter,
    n = n, zeros = g$zero,
    median = median(v), mean = round(mean(v), 2),
    max = max(v), min = min(v),
    top_who = g$who[1], bottom_who = g$who[n],
    mean_over_median = round(mean(v) / median(v), 2),
    above_mean = sum(v > mean(v)),
    above_mean_pct = round(100 * sum(v > mean(v)) / n, 1),
    top1_share = round(100 * sum(v[1:top1]) / sum(v), 2),
    top10_share = round(100 * sum(v[1:max(1, round(0.1 * n))]) / sum(v), 2),
    gini = round(gini(v), 4), stringsAsFactors = FALSE)

  # --- The fit, as a function of how much you fit -------------------------
  #
  # THE POINT OF THE CHAPTER. Fit a straight line to log(value) against
  # log(rank), using only the top f of the data, for f from 1% to 100%. A
  # power law would give roughly the same slope at every f. What these six
  # actually do is written out so the brief does not have to assert it.
  #
  # The intercept travels with the slope because the figure has to DRAW the
  # line, not just report it, and a slope without an intercept is not a line.
  fr <- round(10^seq(log10(0.01), log10(1), length.out = 40), 5)
  fr <- sort(unique(c(fr, 0.01, 0.05, 0.10, 0.25, 0.50, 1.00)))
  fits[[s$key]] <- do.call(rbind, lapply(fr, function(f) {
    k <- max(12, round(f * n))
    m <- lm(log10(v[1:k]) ~ log10(r[1:k]))
    data.frame(series = s$key, frac = f, k = k,
               intercept = round(unname(coef(m)[1]), 4),
               slope = round(unname(coef(m)[2]), 4),
               r2 = round(summary(m)$r.squared, 4),
               stringsAsFactors = FALSE)
  }))
}
ranks <- do.call(rbind, ranks); summ <- do.call(rbind, summ)
fits  <- do.call(rbind, fits)

# --- Zipf's arithmetic, done out loud ----------------------------------------
#
# The oldest version of this claim is the simplest: the item at rank k is
# 1/k of the item at rank 1. That is a slope of exactly -1, and it is a
# PREDICTION, so it can be written next to the observed number and read.

zipf <- do.call(rbind, lapply(SRC, function(s) {
  g <- get(s); v <- g$pos
  k <- c(1, 10, 100, 1000)
  k <- k[k <= length(v)]
  data.frame(series = s$key, rank = k, observed = round(v[k]),
             zipf = round(v[1] / k),
             who = g$who[k],
             ratio = round(v[k] / (v[1] / k), 2), stringsAsFactors = FALSE)
}))
write.csv(zipf, "derived/zipf.csv", row.names = FALSE)
write.csv(ranks, "derived/ranks.csv", row.names = FALSE)

write.csv(summ,  "derived/summary.csv", row.names = FALSE)
write.csv(fits,  "derived/fits.csv", row.names = FALSE)

# --- Check the fit by hand ---------------------------------------------------
#
# lm() is doing the arithmetic, so the arithmetic is checked once against the
# closed form for a least-squares slope, on the series with the most points.

chk <- get(SRC[[1]])$pos
x <- log10(seq_along(chk)); y <- log10(chk)
by_hand <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
stopifnot(abs(by_hand - fits$slope[fits$series == "surnames" &
                                   fits$frac == 1]) < 5e-5)

# --- Facts -------------------------------------------------------------------

f10  <- fits[fits$frac == 0.10, ]
fall <- fits[fits$frac == 1.00, ]
cmp  <- merge(f10[, c("series", "slope", "r2")],
              fall[, c("series", "slope", "r2")], by = "series",
              suffixes = c("_top10", "_all"))
cmp  <- merge(cmp, summ[, c("series", "label")], by = "series")
straight <- cmp[which.max(cmp$r2_all), ]
bent     <- cmp[which.min(cmp$r2_all), ]
worst    <- summ[which.max(summ$mean_over_median), ]

sn <- summ[summ$series == "surnames", ]
ct <- summ[summ$series == "counties", ]
rc <- summ[summ$series == "receipts", ]
tg <- summ[summ$series == "targets", ]

facts <- data.frame(
  key = c("series", "values", "min_r2_top10", "max_r2_all", "min_r2_all",
          "max_slope_spread",
          "straight_series", "straight_label", "straight_r2_all",
          "straight_slope_all",
          "bent_series", "bent_label", "bent_r2_all", "bent_r2_top10",
          "bent_slope_all", "bent_slope_top10",
          "worst_series", "worst_label", "worst_ratio", "worst_median",
          "worst_mean", "worst_n", "worst_above_mean", "worst_above_mean_pct",
          "worst_top", "worst_max",
          "receipts_zero", "receipts_n", "receipts_median", "receipts_mean",
          "receipts_top", "receipts_max", "receipts_above_mean_pct",
          "receipts_bottom", "receipts_min",
          "committees_bottom", "committees_min",
          "residual_who", "residual_value", "residual_rank_if_kept",
          "residual_share",
          "surnames_n", "surnames_top", "surnames_max", "surnames_median",
          "counties_top", "counties_max", "counties_median",
          "min_gini", "max_gini", "min_above_mean_pct", "max_above_mean_pct",
          "min_top1_share", "max_top1_share"),
  value = c(nrow(summ), sum(summ$n), min(f10$r2), max(fall$r2), min(fall$r2),
            round(max(abs(cmp$slope_all - cmp$slope_top10)), 2),
            straight$series, straight$label, straight$r2_all,
            straight$slope_all,
            bent$series, bent$label, bent$r2_all, bent$r2_top10,
            bent$slope_all, bent$slope_top10,
            worst$series, worst$label, worst$mean_over_median,
            round(worst$median), round(worst$mean), worst$n,
            worst$above_mean, worst$above_mean_pct, worst$top_who,
            round(worst$max),
            rc$zeros, rc$n, round(rc$median), round(rc$mean), rc$top_who,
            round(rc$max), rc$above_mean_pct,
            rc$bottom_who, rc$min,
            summ$bottom_who[summ$series == "committees"],
            summ$min[summ$series == "committees"],
            DROPPED$surnames$who, DROPPED$surnames$value,
            sum(get(SRC[[1]])$pos >= DROPPED$surnames$value) + 1,
            round(100 * DROPPED$surnames$value /
                    (DROPPED$surnames$value + sum(get(SRC[[1]])$pos)), 1),
            sn$n, sn$top_who, sn$max, sn$median,
            ct$top_who, ct$max, ct$median,
            min(summ$gini), max(summ$gini),
            min(summ$above_mean_pct), max(summ$above_mean_pct),
            min(summ$top1_share), max(summ$top1_share)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("ranks.csv   ->", nrow(ranks), "plotted points from",
    sum(summ$n), "values\n")
cat("summary.csv ->", nrow(summ), "series\n")
cat("fits.csv    ->", nrow(fits), "fits\n\n")
print(cmp[order(-cmp$r2_all), c("label", "slope_top10", "r2_top10",
                                "slope_all", "r2_all")], row.names = FALSE)
cat("\nshare of units above their own series' mean:",
    min(summ$above_mean_pct), "% to", max(summ$above_mean_pct), "%\n")
cat("worst mean/median ratio:", worst$label, worst$mean_over_median, "\n")
cat("done.\n")

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
