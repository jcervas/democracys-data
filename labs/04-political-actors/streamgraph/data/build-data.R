# ---------------------------------------------------------------------------
# Build the streamgraph dataset: twelve articles' daily readership in 2024,
# laid out once so that print and screen draw the same shape.
#
# Five files end up in derived/:
#
#   derived/series.csv   date, article, views -- the complete grid
#   derived/stream.csv   the stacked layout: y0 and y1 per article per day
#   derived/articles.csv one row per article: totals, peak, and stacking order
#   derived/events.csv   the nine dated campaign events, carried through
#   derived/facts.csv    single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Wikipedia pageviews for twelve articles, every day of 2024, from the
# Wikimedia REST API, and nine dated campaign events. Both are committed by
# this corpus's media-attention chapter, which documents the capture:
#
#   ../../media-attention/data/derived/wiki_attention_2024.csv
#   ../../media-attention/data/derived/campaign_events_2024.csv
#
# That chapter draws these series as a zero-baseline stacked area and as lines.
# This one draws them two ways it does not -- a streamgraph and a horizon
# chart -- because the twelve series span three orders of magnitude and those
# are the two forms that answer to that.

W <- "../../media-attention/data/derived/wiki_attention_2024.csv"
E <- "../../media-attention/data/derived/campaign_events_2024.csv"
stopifnot(file.exists(W), file.exists(E))
w  <- read.csv(W, stringsAsFactors = FALSE)
ev <- read.csv(E, stringsAsFactors = FALSE)
w$date <- as.Date(w$date)

ARTS <- sort(unique(w$article))
DAYS <- seq(min(w$date), max(w$date), by = "day")

# --- No missing cells, and that is checked rather than assumed ---------------
#
# A stacked layout cannot leave a hole: every day needs a total, so a single
# absent article-day would either stop the build or get silently imputed. This
# grid is built explicitly and asserted complete.
#
# It was NOT complete until the media-attention chapter fixed its capture. That
# chapter had asked the Pageviews API for the title the JD Vance article has
# now, which it did not have for the first half of 2024, and the series that
# came back was missing 5 May and was three orders of magnitude too small
# everywhere before the rename. This chapter used to fill that one cell with
# the mean of its neighbours and say so. There is nothing left to fill.

grid <- expand.grid(date = DAYS, article = ARTS, stringsAsFactors = FALSE)
g <- merge(grid, w, by = c("date", "article"), all.x = TRUE)
stopifnot(!any(is.na(g$views)), nrow(g) == length(DAYS) * length(ARTS))
g <- g[order(g$date, g$article), ]
write.csv(data.frame(date = as.character(g$date), article = g$article,
                     views = as.integer(g$views)),
          "derived/series.csv", row.names = FALSE)

# --- Per article -------------------------------------------------------------

art <- do.call(rbind, lapply(ARTS, function(a) {
  z <- g[g$article == a, ]
  data.frame(article = a, total = sum(z$views), peak = max(z$views),
             peak_date = as.character(z$date[which.max(z$views)]),
             median_day = median(z$views), stringsAsFactors = FALSE)
}))
art <- art[order(-art$total), ]

# INSIDE-OUT ORDER. A streamgraph puts the largest series in the middle and
# alternates the rest outward, so that the big shapes sit where the band is
# thickest and the thin ones are not asked to survive being bent around them.
ord <- integer(0); left <- TRUE
for (i in seq_len(nrow(art))) {
  if (left) ord <- c(ord, i) else ord <- c(i, ord)
  left <- !left
}
art$stack_order <- match(seq_len(nrow(art)), ord)
art <- art[order(art$stack_order), ]
write.csv(art, "derived/articles.csv", row.names = FALSE)

# --- The layout, computed once ----------------------------------------------
#
# A CENTRED baseline, not d3's wiggle. Both are streamgraphs; the centred one
# is y0 = -total/2 and can be written in a line, which means print and screen
# can be given the same numbers instead of two implementations of the same
# published algorithm that might disagree in the third decimal.

M <- matrix(0, nrow = length(DAYS), ncol = nrow(art),
            dimnames = list(as.character(DAYS), art$article))
for (a in art$article) {
  z <- g[g$article == a, ]; z <- z[order(z$date), ]
  M[, a] <- z$views
}
tot  <- rowSums(M)
cum  <- t(apply(M, 1, cumsum))
base <- -tot / 2
y1 <- sweep(cum, 1, base, "+")
y0 <- y1 - M

stream <- do.call(rbind, lapply(seq_along(art$article), function(j) {
  a <- art$article[j]
  data.frame(date = as.character(DAYS), article = a,
             y0 = round(y0[, j], 1), y1 = round(y1[, j], 1),
             views = as.integer(M[, j]), stringsAsFactors = FALSE)
}))
write.csv(stream, "derived/stream.csv", row.names = FALSE)

ev$date <- as.character(as.Date(ev$date))
write.csv(ev, "derived/events.csv", row.names = FALSE)

# --- Facts -------------------------------------------------------------------

daily <- data.frame(date = DAYS, total = tot, stringsAsFactors = FALSE)
# `art` is now in STACKING order, not size order, so the largest series is not
# row 1. Ask for it by value.
big   <- art[which.max(art$total), ]
small <- art[which.min(art$total), ]
stopifnot(big$article != small$article, big$total > small$total)
peakday <- daily[which.max(daily$total), ]

facts <- data.frame(
  key = c("articles", "days", "rows", "imputed_cells",
          "total_views", "peak_day", "peak_day_total",
          "median_day_total", "day_ratio",
          "biggest", "biggest_total", "smallest", "smallest_total",
          "size_ratio", "walz_peak", "walz_peak_date",
          "events", "span_lo", "span_hi"),
  value = c(nrow(art), length(DAYS), nrow(g), 0,
            sum(M), as.character(peakday$date), peakday$total,
            round(median(daily$total)),
            round(max(daily$total) / median(daily$total), 1),
            big$article, big$total, small$article, small$total,
            round(big$total / small$total),
            art$peak[art$article == "Tim_Walz"],
            art$peak_date[art$article == "Tim_Walz"],
            nrow(ev), min(M), max(M)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("series.csv  ->", nrow(g), "article-days (", length(DAYS), "days x",
    nrow(art), "articles )\n")
cat("stream.csv  ->", nrow(stream), "rows, centred layout\n")
cat("no missing cells; nothing imputed\n\n")
cat("daily total: median", format(round(median(daily$total)), big.mark = ","),
    " peak", format(max(daily$total), big.mark = ","),
    "on", as.character(peakday$date), "\n")
cat("largest series is", round(big$total / small$total),
    "times the smallest\n")
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
