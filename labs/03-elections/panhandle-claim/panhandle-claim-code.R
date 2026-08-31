# panhandle-claim-code.R -- chunk bodies for panhandle-claim-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

# Every figure here is base R, which rasterises and so cannot follow the page
# theme the way a D3 figure does. Two defences, borrowed from the wind-map
# chapter: the paper is transparent in HTML, so the figure sits on whatever
# colour the reader's page is, and the ink is a mid grey that holds against
# white and near-black alike.
HTMLOUT <- knitr::is_html_output()
if (HTMLOUT) knitr::opts_chunk$set(dev.args = list(bg = "transparent"))
INK <- if (HTMLOUT) "#8a8d95" else "#333333"
GRY <- if (HTMLOUT) "#6e7179" else "#777777"

D <- "data/derived"
rd  <- function(f, ...) read.csv(file.path(D, f), stringsAsFactors = FALSE, ...)
resu <- rd("county_results.csv")
dev  <- rd("deviations.csv")
bm   <- rd("benchmark.csv")
rg   <- rd("regions.csv", check.names = FALSE)
cbr  <- rd("candidate_by_region.csv", check.names = FALSE)
bw   <- rd("williams.csv")
el   <- rd("electorate.csv")
cor4 <- rd("correlates.csv")
bnd  <- rd("bounds.csv")
fct  <- rd("facts.csv")
rng  <- rd("county_map.csv", colClasses = c(id = "character"))
mlb  <- rd("county_map_labels.csv", colClasses = c(fips = "character"))

# Every number the prose states comes out of here, so a rebuild that moves a
# number moves the sentence with it.
F <- function(k) {
  v <- fct$value[fct$name == k]
  if (!length(v)) stop("no such fact: ", k)
  v
}
Ft <- function(k) fct$text[fct$name == k]
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
sg <- function(x, k = 1) paste0(ifelse(x >= 0, "+", "−"), pc(abs(x), k))

REG <- c("Panhandle", "Big Bend / north central", "rest of Florida")
BLUE <- "#2C7FB8"; ORANGE <- "#D95F0E"; PURPLE <- "#4F2E83"; GREY <- "#C2C5CC"

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  nm <- names(x); nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- spread-map
# One county, one outline, shaded by how far Donalds ran from his own statewide
# share. Diverging, because the sign is the subject.
op <- par(mar = c(0, 0, 1.2, 0)); on.exit(par(op))
br  <- c(-30, -20, -12, -6, 0, 6, 12, 20)
pal <- c("#8C2D04", "#CC4C02", "#EC7014", "#FEC44F",
         "#C6DBEF", "#6BAED6", "#2171B5")
shade <- function(v) pal[findInterval(v, br, all.inside = TRUE)]
# The shared frame counts y downward, the way a screen does, so the y axis is
# reversed to put north at the top.
plot(NA, xlim = range(rng$x), ylim = rev(range(rng$y)),
     asp = 1, axes = FALSE, xlab = "", ylab = "")
for (id in unique(rng$id)) {
  d <- rng[rng$id == id, ]
  for (pt in unique(d$part)) {
    q <- d[d$part == pt, ]
    polygon(q$x, q$y, col = shade(q$donalds_dev[1]), border = INK, lwd = 0.4)
  }
}
key <- mlb[mlb$county %in% c("Lafayette", "Collier", "Escambia", "Okaloosa",
                             "Leon", "Miami-Dade", "Lee"), ]
for (a in seq(0, 2 * pi, length.out = 18))
  text(key$cx + cos(a) * 1.1, key$cy + sin(a) * 1.1, key$county,
       cex = 0.55, font = 2, col = "#141414")
text(key$cx, key$cy, key$county, cex = 0.55, font = 2, col = "#ffffff")
legend("bottomleft", bty = "n", cex = 0.68, text.col = INK,
       fill = rev(pal), border = INK,
       legend = rev(c("30 to 20 points below", "20 to 12 below", "12 to 6 below",
                      "6 below to even", "even to 6 above", "6 to 12 above",
                      "12 to 20 above")))
title(sprintf("Donalds against his own statewide %s%%", pc(F("donalds_statewide"))),
      cex.main = 0.95, col.main = INK, line = 0.1)

## ---- same-ballot
t <- rg[, c("region", "Donalds", "Moody", "Ingoglia", "Simpson")]
for (j in 2:5) t[[j]] <- sg(t[[j]])
names(t) <- c("Region", "Donalds (Gov)", "Moody (Sen)", "Ingoglia (CFO)", "Simpson (Agr)")
t

## ---- benchmark-scatter
# 67 counties. The horizontal axis is a white candidate's map from eight years
# earlier; the vertical is this one. The line is least squares.
op <- par(mar = c(4.2, 4.4, 2.4, 1.5)); on.exit(par(op))
cols <- ifelse(bm$region == "Panhandle", ORANGE,
        ifelse(bm$region == "Big Bend / north central", PURPLE, GREY))
# A colour vector shorter than the data draws a scatter with no points in it,
# and the figure still comes out looking like a figure -- fitted line, axes,
# labels, nothing plotted. That happened here once. Nothing in the corpus
# checks for an empty figure, so the chapter checks for it.
stopifnot(length(cols) == nrow(bm), !any(is.na(cols)))
plot(bm$desantis_dev, bm$donalds_dev, pch = 19, cex = 0.9, col = cols,
     xlab = "", ylab = "", axes = FALSE,
     xlim = c(min(bm$desantis_dev) - 1, max(bm$desantis_dev) + 9))
axis(1, col = GRY, col.axis = INK, cex.axis = 0.72, lwd = 0.7)
axis(2, col = GRY, col.axis = INK, cex.axis = 0.72, lwd = 0.7, las = 1)
abline(h = 0, v = 0, col = GRY, lwd = 0.6, lty = 3)
abline(lm(donalds_dev ~ desantis_dev, data = bm), col = INK, lwd = 2)
lab <- bm[bm$county %in% c("Okaloosa", "Santa Rosa", "Leon", "Lafayette",
                           "Collier", "Escambia", "Miami-Dade"), ]
text(lab$desantis_dev, lab$donalds_dev, lab$county, pos = 4, cex = 0.62,
     col = INK, offset = 0.35)
mtext("DeSantis 2018, points from his statewide share", 1, line = 2.4,
      cex = 0.72, col = INK)
mtext("Donalds 2026", 2, line = 2.6, cex = 0.72, col = INK)
legend("topleft", bty = "n", cex = 0.72, text.col = INK, pch = 19,
       col = c(ORANGE, PURPLE, GREY),
       legend = c("Panhandle", "Big Bend / north central", "rest of Florida"))
title(sprintf("The same map, eight years and one white candidate earlier (r = %s)",
              pc(F("benchmark_r"), 2)), cex.main = 0.92, col.main = INK, line = 0.9)

## ---- williams-map
# Where a four-per-cent candidate found his vote. The point of the figure is
# that the dark counties are not one region.
op <- par(mar = c(0, 0, 1.2, 0)); on.exit(par(op))
wb <- c(0, 4, 8, 12, 16, 30)
wp <- c("#F7F7F7", "#D9D3E8", "#B0A2D0", "#7F63B8", "#4F2E83")
ws <- setNames(bw$share, bw$county)
plot(NA, xlim = range(rng$x), ylim = rev(range(rng$y)),
     asp = 1, axes = FALSE, xlab = "", ylab = "")
for (id in unique(rng$id)) {
  d <- rng[rng$id == id, ]
  v <- ws[[d$county[1]]]
  for (pt in unique(d$part)) {
    q <- d[d$part == pt, ]
    polygon(q$x, q$y, col = wp[findInterval(v, wb, all.inside = TRUE)],
            border = INK, lwd = 0.4)
  }
}
key <- mlb[mlb$county %in% c("Lafayette", "Dixie", "Glades", "Hardee", "Okeechobee"), ]
NUDGE <- rbind(c(-9, -3), c(0, 0), c(0, 4), c(0, 0), c(11, 2))
rownames(NUDGE) <- c("Hardee", "Dixie", "Glades", "Lafayette", "Okeechobee")
key$lx <- key$cx + NUDGE[key$county, 1]
key$ly <- key$cy + NUDGE[key$county, 2]
for (a in seq(0, 2 * pi, length.out = 18))
  text(key$lx + cos(a) * 1.1, key$ly + sin(a) * 1.1, key$county,
       cex = 0.55, font = 2, col = "#141414")
text(key$lx, key$ly, key$county, cex = 0.55, font = 2, col = "#ffffff")
legend("bottomleft", bty = "n", cex = 0.7, text.col = INK, fill = rev(wp),
       border = INK, legend = rev(c("under 4%", "4 to 8%", "8 to 12%",
                                    "12 to 16%", "over 16%")))
title(sprintf("Bobby Williams, %s%% of the state", pc(F("williams_statewide"), 1)),
      cex.main = 0.95, col.main = INK, line = 0.1)

## ---- region-shares
t <- cbr
for (j in 2:5) t[[j]] <- pc(t[[j]])
names(t) <- c("Candidate", "Statewide", "Panhandle", "Big Bend / north central",
              "Rest of Florida")
t

## ---- bounds
t <- bnd
t$donalds_share <- paste0(pc(t$donalds_share), "%")
t$white_pct_of_registered <- paste0(pc(t$white_pct_of_registered), "%")
t$range <- sprintf("%s%% to %s%%", pc(t$white_rate_low), pc(t$white_rate_high))
t$width <- paste0(pc(t$width), " points")
t <- t[, c("region", "white_pct_of_registered", "donalds_share", "range", "width")]
names(t) <- c("Region", "White share of registered Republicans", "Donalds",
              "So white Republicans gave him", "Window")
t

## ---- electorate-range
# The closing argument, drawn rather than asserted: one covariate has range and
# the other does not. Same axis, same 67 counties.
op <- par(mar = c(4.2, 6, 2.4, 2)); on.exit(par(op))
plot(NA, xlim = c(0, 100), ylim = c(0.4, 2.6), axes = FALSE, xlab = "", ylab = "")
axis(1, col = GRY, col.axis = INK, cex.axis = 0.75, lwd = 0.7)
# Short labels: the axis title below already says whose share this is, and a
# two-line label here ran off the panel however wide the margin was set.
rows <- list(list(2, el$hispanic_pct, BLUE,   "Hispanic"),
             list(1, el$black_pct,    ORANGE, "Black"))
for (r in rows) {
  y <- r[[1]]; v <- r[[2]]
  segments(min(v), y, max(v), y, col = r[[3]], lwd = 3)
  points(v, rep(y, length(v)), pch = 124, cex = 0.8, col = r[[3]])
  text(max(v) + 2, y, sprintf("%s to %s points", pc(min(v)), pc(max(v))),
       adj = 0, cex = 0.72, col = r[[3]], font = 2)
  # Right-aligned into the margin with text() rather than mtext(): mtext
  # centres on `at`, so half of a label this long runs off the left edge
  # however wide the margin is.
  text(-2, y, r[[4]], adj = 1, cex = 0.8, col = INK, font = 2, xpd = NA)
}
mtext("per cent of a county's registered Republicans", 1, line = 2.4,
      cex = 0.72, col = INK)
title("One of these can carry an estimate. The other cannot.",
      cex.main = 0.92, col.main = INK, line = 0.9)

## ---- correlates
t <- cor4
t$vs_deviation <- pc(t$vs_deviation, 2)
t$vs_residual  <- pc(t$vs_residual, 2)
t$range_points <- paste0(pc(t$range_points), " points")
names(t) <- c("County characteristic", "With his geography",
              "With what 2018 leaves", "Range across the 67 counties")
t

## ---- worst-counties
t <- bm[order(bm$donalds_2026), ][1:10, c("county", "county_votes",
                                          "donalds_2026", "donalds_dev",
                                          "desantis_dev", "residual")]
t$donalds_2026 <- paste0(pc(t$donalds_2026), "%")
for (j in c("donalds_dev", "desantis_dev", "residual")) t[[j]] <- sg(t[[j]])
t$county_votes <- n(t$county_votes)
names(t) <- c("County", "GOP votes", "Donalds", "vs his statewide",
              "DeSantis 2018", "Left unexplained")
t

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
