# nationalization-code.R -- chunk bodies for nationalization-brief.Rmd
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

del <- read.csv("data/derived/delegations.csv", stringsAsFactors = FALSE)
sy  <- read.csv("data/derived/senate_years.csv", stringsAsFactors = FALSE)
hy  <- read.csv("data/derived/house_years.csv",  stringsAsFactors = FALSE)
swp <- read.csv("data/derived/swap.csv",         stringsAsFactors = FALSE)
fx  <- read.csv("data/derived/facts.csv",        stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)

# The corpus map palette, for the STATIC twins only. The D3 figures are drawn
# with the shared library, whose class-based colors restyle with the page;
# base-R devices cannot swap for the dark page, so they use the light values.
RED <- "#C41230"; BLU <- "#2C7FB8"; ORG <- "#e08214"
DRK <- "#1C4C5C"; MUTE <- "#76838C"; RULE <- "#CBD3D8"

DPEAK <- fn("del_peak_pct"); DLAST <- fn("del_last_pct")
HPEAK <- fn("h_peak_pct");   HLAST <- fn("h_last_pct")
YOLD  <- fn("swap_old_year"); YNEW <- fn("swap_new_year")

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
z <- del[del$year %in% c(1879, 1979, 2025), ]
data.frame(
  Congress = paste0(z$congress, "th"),
  Convened = z$year,
  States = z$states,
  Split_delegations = z$split_states,
  Share = paste0(p1(z$pct_split), "%"))

## ---- fig1-static
op <- par(mar = c(3.6, 4.4, 2.4, 1.6), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(del$year), ylim = c(0, 60), axes = FALSE,
     xlab = "", ylab = "")
abline(h = seq(0, 60, 10), col = RULE, lwd = 0.6)
lines(del$year, del$pct_split_two, col = ORG, lwd = 1.6)
lines(del$year, del$pct_split,     col = DRK, lwd = 2.6)
axis(1, at = seq(1880, 2020, 20), cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 60, 10), las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
mtext("% of states", 2, line = 2.9, cex = 0.86)
legend("topleft", c("Every state", "Only states that sent exactly two senators"),
       col = c(DRK, ORG), lwd = c(2.6, 1.6), bty = "n", cex = 0.72)
# Only the peak is labelled. The last Congress is named in the sentence under
# the figure, and a second label there lands on top of the falling line.
pk <- del[which.max(del$pct_split), ]
text(pk$year, pk$pct_split + 3.4, paste0(pk$year, ": ", pk$split_states, " of ", pk$states),
     cex = 0.7, col = MUTE)
par(op)

## ---- fig1-d3
# Drawn with the shared library (_lib/dd-charts.js); dd_fig() emits the two
# <script src> tags for the document. The two lines are two READINGS of the
# same roster, not two parties, so they take series classes.
m <- del[order(del$year), c("year", "pct_split", "pct_split_two",
                            "split_states", "states")]
dd_fig("dele", "line", m,
  size = list(w = 770, h = 430, m = list(t = 18, r = 24, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 10),
  y = list(field = "pct_split", label = "% of states",
           domain = c(0, 60), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "pct_split", label = "every state", class = "series-1"),
    list(field = "pct_split_two",
         label = "only states that sent exactly two senators",
         class = "series-4"))),
  legend = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> every state: "+
        d.pct_split.toFixed(1)+"% ("+d.split_states+" of "+d.states+")<br>"+
      "<span class=\'series-4-txt\'>&#9632;</span> exactly two senators: "+
        d.pct_split_two.toFixed(1)+"%";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the figure for a Congress-by-Congress readout.</p>')

## ---- senate-table
o <- sy
o$pct_split <- paste0(p1(o$pct_split), "%")
o$wobble <- p2(o$wobble)
names(o) <- c("Election", "Senate contests", "Split outcomes", "Share",
              "Both parties ran", "Distance from the ticket")
o

## ---- fig2-static
op <- par(mfrow = c(2, 1), mar = c(2.4, 4.4, 2.2, 1.6), mgp = c(2.6, 0.7, 0))
plot(hy$year, hy$pct_split, type = "o", pch = 19, cex = 0.7, lwd = 2.4,
     col = DRK, axes = FALSE, xlab = "", ylab = "", ylim = c(0, 45))
abline(h = seq(0, 40, 10), col = RULE, lwd = 0.6)
lines(hy$year, hy$pct_split, col = DRK, lwd = 2.4)
points(hy$year, hy$pct_split, pch = 19, cex = 0.7, col = DRK)
axis(1, at = seq(1952, 2024, 8), cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 40, 10), las = 1, cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
mtext("% of districts", 2, line = 2.9, cex = 0.8)
mtext("Districts that voted one way for president and the other for the House",
      3, line = 0.7, cex = 0.8, adj = 0)

par(mar = c(3.4, 4.4, 2.2, 1.6))
plot(NA, xlim = range(hy$year), ylim = c(0, 15), axes = FALSE, xlab = "", ylab = "")
abline(h = seq(0, 15, 5), col = RULE, lwd = 0.6)
lines(hy$year, hy$wobble, col = RED, lwd = 2.4)
lines(hy$year, hy$spread, col = BLU, lwd = 2.4)
axis(1, at = seq(1952, 2024, 8), cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 15, 5), las = 1, cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
mtext("points of vote share", 2, line = 2.9, cex = 0.8)
mtext("The two ingredients of that count", 3, line = 0.7, cex = 0.8, adj = 0)
legend("topright", c("Distance from the ticket", "Spread of the districts"),
       col = c(RED, BLU), lwd = 2.4, bty = "n", cex = 0.72)
par(op)

## ---- fig2-d3
# Two shared-library panels on one time axis: the count on top, the two
# quantities it is made of underneath. dd_fig() emitted the script tags at
# Figure 1, so these two calls draw and add nothing to the payload.
h <- hy[order(hy$year), ]
dd_fig("hsplit", "line", h[, c("year", "pct_split", "districts")],
  size = list(w = 770, h = 250, m = list(t = 26, r = 24, b = 34, l = 54)),
  x = list(field = "year", fmt = "d", ticks = 10),
  y = list(field = "pct_split", label = "% of districts",
           domain = c(0, 45), fmt = "pct0", ticks = 5),
  series = list(fields = list(
    list(field = "pct_split", label = "districts that split",
         class = "series-1"))),
  points = TRUE,
  annotations = list(dd_annot_text(1952, 44,
    "Districts that voted one way for president and the other for the House",
    class = "sub", size = 12)),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+d.pct_split.toFixed(1)+
      "% of "+d.districts+" districts split";
  }'))
dd_fig("hparts", "line", h[, c("year", "wobble", "spread")],
  size = list(w = 770, h = 250, m = list(t = 26, r = 24, b = 34, l = 54)),
  x = list(field = "year", fmt = "d", ticks = 10),
  y = list(field = "wobble", label = "points of vote share",
           domain = c(0, 15), ticks = 4),
  series = list(fields = list(
    list(field = "wobble", label = "distance from the ticket", class = "series-2"),
    list(field = "spread", label = "spread of the districts", class = "series-1"))),
  legend = TRUE,
  annotations = list(dd_annot_text(1952, 14.6,
    "The two ingredients of that count", class = "sub", size = 12)))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across either panel for an election-by-election readout.</p>')

## ---- fig3-static
lab <- c(paste0(YOLD, " as it was"),
         paste0(YOLD, " districts,\n", YNEW, " candidates"),
         paste0(YNEW, " districts,\n", YOLD, " candidates"),
         paste0(YNEW, " as it was"))
v   <- c(swp$pct_split[swp$case == "actual_old"],
         swp$pct_split[swp$case == "old_with_new_gaps"],
         swp$pct_split[swp$case == "new_with_old_gaps"],
         swp$pct_split[swp$case == "actual_new"])
cl  <- c(DRK, ORG, ORG, DRK)
op <- par(mar = c(4.6, 4.4, 2.4, 1.6), mgp = c(2.6, 0.7, 0))
bp <- barplot(v, col = cl, border = NA, ylim = c(0, 46), axes = FALSE,
              names.arg = rep("", 4), space = 0.5)
abline(h = seq(0, 40, 10), col = RULE, lwd = 0.6)
barplot(v, col = cl, border = NA, ylim = c(0, 46), axes = FALSE,
        names.arg = rep("", 4), space = 0.5, add = TRUE)
axis(2, at = seq(0, 40, 10), las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
text(bp, v + 2.2, paste0(p1(v), "%"), cex = 0.8, col = MUTE)
mtext(lab, side = 1, at = bp, line = 1.6, cex = 0.72)
mtext("% of districts", 2, line = 2.9, cex = 0.86)
par(op)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
