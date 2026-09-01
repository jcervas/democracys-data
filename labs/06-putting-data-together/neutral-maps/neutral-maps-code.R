# neutral-maps-code.R -- chunk bodies for neutral-maps-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
source("../../_lib/dd-charts.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

comp   <- read.csv("data/derived/competitive_by_state.csv", stringsAsFactors = FALSE)
compN  <- read.csv("data/derived/competitive_national.csv", stringsAsFactors = FALSE)
black  <- read.csv("data/derived/black_by_state.csv", stringsAsFactors = FALSE)
blackN <- read.csv("data/derived/black_national.csv", stringsAsFactors = FALSE)
facts  <- read.csv("data/derived/facts.csv", stringsAsFactors = FALSE)
fx <- function(k) facts$value[facts$key == k]
who    <- read.csv("data/derived/who_draws.csv", stringsAsFactors = FALSE)
whoS   <- read.csv("data/derived/who_draws_states.csv", stringsAsFactors = FALSE)
WD <- function(k) who$states[who$who_draws_it == k]
fxn <- function(k) as.numeric(fx(k))

pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# one palette, used in every figure in this chapter
ENACT <- "#4E5A63"   # the map a legislature actually drew
NEUT  <- "#1C4C5C"   # the mean across 5,000 maps with no one steering them
GERRY <- "#C41230"   # the most adversarial map the simulations contain
GRY   <- "#B9BEC4"

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- fig1-static
# One row per threshold, the enacted count and the neutral average on it.
op <- par(mar = c(4.2, 5.2, 1, 1), mgp = c(2.6, 0.7, 0))
yy <- rev(seq_len(nrow(compN)))
plot(NA, xlim = c(0, max(compN$neutral_mean) * 1.12), ylim = range(yy) + c(-0.6, 0.6),
     axes = FALSE, xlab = "Competitive districts", ylab = "")
axis(1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
segments(compN$enacted, yy, compN$neutral_mean, yy, col = "#999999", lwd = 1.8)
points(compN$enacted, yy, pch = 21, bg = "#FFFFFF", col = ENACT, cex = 1.3)
points(compN$neutral_mean, yy, pch = 19, col = NEUT, cex = 1.3)
text(-1.5, yy, paste0("\u00b1", compN$threshold * 100, " points"),
     xpd = NA, adj = 1, cex = 0.86)
legend("bottomright", c("enacted maps", "neutral maps (mean)"), pch = c(21, 19),
       col = c(ENACT, NEUT), pt.bg = c("#FFFFFF", NA), bty = "n", cex = 0.85)
par(op)

## ---- fig1-d3
# The shared chart library. A dumbbell is the form the question takes: one row
# per threshold, two counts of the same thing, and the gap between them is the
# finding. This is the chapter's first figure, so it loads d3 as well.
f1 <- data.frame(band = paste0("\u00b1", compN$threshold * 100, " points"),
                 enacted = compN$enacted,
                 neutral = round(compN$neutral_mean, 1),
                 stringsAsFactors = FALSE)
dd_fig("f1", "dumbbell", f1,
  size = list(w = 720),
  rowHeight = 34,
  y = list(field = "band"),
  a = list(field = "enacted", label = "enacted maps"),
  b = list(field = "neutral", label = "neutral maps (mean)"),
  aClass = "series-3", bClass = "series-1",
  x = list(ticks = 7, fmt = "d", label = "Competitive districts, out of 429"),
  tip = dd_tip(c(enacted = "enacted maps", neutral = "mean of 5,000 neutral maps"),
               fmt = c(enacted = "d", neutral = "f1"), title = "band"))

## ---- fig2-static
c5 <- comp[comp$threshold == 0.05, ]
c5$gap <- c5$neutral_mean - c5$enacted
top <- head(c5[order(-c5$gap), ], 10)
top <- top[order(top$gap), ]
op <- par(mar = c(4, 4.2, 1, 1), mgp = c(2.6, 0.7, 0))
yy <- seq_len(nrow(top))
plot(NA, xlim = c(0, max(top$neutral_mean) * 1.12), ylim = range(yy) + c(-0.6, 0.6),
     axes = FALSE, xlab = "Competitive districts (±5%)", ylab = "")
axis(1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
segments(top$enacted, yy, top$neutral_mean, yy, col = "#999999", lwd = 1.6)
points(top$enacted, yy, pch = 21, bg = "#FFFFFF", col = ENACT, cex = 1.25)
points(top$neutral_mean, yy, pch = 19, col = NEUT, cex = 1.25)
text(-0.4, yy, top$state, xpd = NA, adj = 1, cex = 0.86)
legend("bottomright", c("enacted", "neutral mean"), pch = c(21, 19),
       col = c(ENACT, NEUT), pt.bg = c("#FFFFFF", NA), bty = "n", cex = 0.85)
par(op)

## ---- fig2-d3
c5 <- comp[comp$threshold == 0.05, ]
c5$gap <- c5$neutral_mean - c5$enacted
top <- head(c5[order(-c5$gap), ], 10)
top <- top[order(-top$gap), ]
f2 <- data.frame(state = top$state, enacted = top$enacted,
                 neutral = round(top$neutral_mean, 1), stringsAsFactors = FALSE)
dd_fig("f2", "dumbbell", f2, d3 = FALSE,
  size = list(w = 700),
  rowHeight = 30,
  y = list(field = "state"),
  a = list(field = "enacted", label = "enacted map"),
  b = list(field = "neutral", label = "neutral maps (mean)"),
  aClass = "series-3", bClass = "series-1",
  x = list(ticks = 7, fmt = "d",
           label = "Competitive districts, within 5 points of even"),
  tip = dd_tip(c(enacted = "enacted map", neutral = "mean of 5,000 neutral maps"),
               fmt = c(enacted = "d", neutral = "f1"), title = "state"))

## ---- fig3-static
b <- black[black$actual_black > 0 | black$neutral_black_mean > 0.01, ]
b <- b[order(b$actual_black), ]
op <- par(mar = c(4.2, 4.2, 1.4, 1), mgp = c(2.6, 0.7, 0))
yy <- seq_len(nrow(b))
plot(NA, xlim = c(-0.15, max(b$actual_black) + 0.3), ylim = range(yy) + c(-0.6, 0.6),
     axes = FALSE, xlab = "Black-plurality districts", ylab = "")
axis(1, at = 0:max(b$actual_black), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
abline(h = yy, col = "#EEEEEE")
points(b$actual_black,       yy, pch = 19, col = ENACT, cex = 1.2)
points(b$neutral_black_mean, yy, pch = 19, col = NEUT,  cex = 1.2)
points(b$gerry_black,        yy, pch = 19, col = GERRY, cex = 1.2)
text(-0.3, yy, b$state, xpd = NA, adj = 1, cex = 0.86)
legend("bottomright", c("enacted", "neutral mean", "maximized gerrymander"),
       pch = 19, col = c(ENACT, NEUT, GERRY), bty = "n", cex = 0.8)
par(op)

## ---- fig3-d3
b <- black[black$actual_black > 0 | black$neutral_black_mean > 0.01, ]
b <- b[order(-b$actual_black), ]
f3 <- rbind(
  data.frame(state = b$state, kind = "enacted",
             seats = b$actual_black),
  data.frame(state = b$state, kind = "neutral mean",
             seats = round(b$neutral_black_mean, 2)),
  data.frame(state = b$state, kind = "maximized gerrymander",
             seats = b$gerry_black))
dd_fig("f3", "dot", f3, d3 = FALSE,
  size = list(w = 720),
  rowHeight = 34, r = 5.5,
  y = list(field = "state"),
  x = list(field = "seats", domain = c(-0.2, max(b$actual_black) + 0.3),
           ticks = 5, fmt = "d", label = "Black-plurality districts"),
  series = list(field = "kind", classes = list(
    "enacted" = "series-3",
    "neutral mean" = "series-1",
    "maximized gerrymander" = "gop")),
  legend = TRUE,
  tip = dd_tip(c(kind = "count", seats = "Black-plurality districts"),
               fmt = c(seats = "f2"), title = "state"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
