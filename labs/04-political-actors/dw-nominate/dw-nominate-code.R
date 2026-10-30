# dw-nominate-code.R -- chunk bodies for dw-nominate-brief.Rmd
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
                      dpi = 96, fig.retina = 1)   # these figures are point-heavy
options(scipen = 999)

nom <- read.csv("data/derived/nominate_members.csv", stringsAsFactors = FALSE)

series <- function(ch) {
  x  <- nom[nom$chamber == ch, ]
  cg <- sort(unique(x$congress))
  md <- function(c_, p) median(x$dim1[x$congress == c_ & x$party == p])
  ov <- function(c_) {
    y <- x[x$congress == c_, ]
    sum(y$dim1[y$party == "Republican"] < max(y$dim1[y$party == "Democrat"]))
  }
  data.frame(congress = cg,
             year = sapply(cg, function(c_) x$year[x$congress == c_][1]),
             dem  = sapply(cg, md, p = "Democrat"),
             rep  = sapply(cg, md, p = "Republican"),
             overlap = sapply(cg, ov))
}
H <- series("House"); S <- series("Senate")
H$gap <- H$rep - H$dem; S$gap <- S$rep - S$dem

g <- function(d, cg) d$gap[d$congress == cg]
NOWH <- max(H$congress); NOWS <- max(S$congress)
YRNOW <- H$year[H$congress == NOWH]
Y67 <- H$year[H$congress == 90]

DMOVE <- H$dem[H$congress == NOWH] - H$dem[H$congress == 90]
RMOVE <- H$rep[H$congress == NOWH] - H$rep[H$congress == 90]

LASTOVH <- max(H$congress[H$overlap > 0])
LASTOVS <- max(S$congress[S$overlap > 0])
ZEROH   <- H$year[H$overlap == 0]

G1890 <- max(H$gap[H$year >= 1889 & H$year <= 1901])
Y1890 <- H$year[which(H$gap == G1890)]

# Does anybody in this file ever move? The answer decides what a moving party
# median can possibly mean, and the "cannot tell you" section rests on it.
# The two ends of the current House, and the size of each party in it, for
# the worked example beside the one-row table and the median arithmetic.
HN   <- nom[nom$chamber == "House" & nom$congress == NOWH, ]
EXL  <- HN[which.min(HN$dim1), ]; EXR <- HN[which.max(HN$dim1), ]
NDEM <- sum(HN$party == "Democrat"); NREP <- sum(HN$party == "Republican")

hk    <- with(nom[nom$chamber == "House", ], paste(bioname, state, party))
NMEM  <- length(unique(hk))
NMOVE <- sum(tapply(nom$dim1[nom$chamber == "House"], hk,
                    function(v) length(unique(v))) > 1)

nm <- function(x, k = 3) formatC(x, format = "f", digits = k)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- nom[nom$chamber == "House" & nom$congress == NOWH, ]
o <- o[order(o$dim1), ]
o <- rbind(head(o, 2), o[round(nrow(o)/2), ], tail(o, 2))
o <- o[, c("congress", "year", "chamber", "state", "bioname", "party",
           "dim1", "dim2")]
o$dim1 <- nm(o$dim1); o$dim2 <- nm(o$dim2)
names(o) <- c("congress", "year", "chamber", "state", "member", "party",
              "dim 1", "dim 2")
o

## ---- coverage
data.frame(
  quantity = c("Member-Congress records", "House records", "Senate records",
               "Congresses covered", "Years covered"),
  value = c(n(nrow(nom)), n(sum(nom$chamber == "House")),
            n(sum(nom$chamber == "Senate")),
            paste0(min(nom$congress), "th–", max(nom$congress), "th"),
            paste0(min(nom$year), "–", max(nom$year))))

## ---- gap-table
o <- H[H$congress %in% c(90, 100, 110, NOWH), c("congress", "year", "dem", "rep", "gap")]
o$dem <- nm(o$dem); o$rep <- nm(o$rep); o$gap <- nm(o$gap)
names(o) <- c("congress", "year", "median Democrat", "median Republican",
              "distance")
o

## ---- gap-d3
# ---------------------------------------------------------------------------
# Two chambers, one frame, one unit. The whole test is the comparison between
# the lines, so they belong on a single axis -- and drawn with the shared
# library rather than by hand, because nothing here is particular to this
# chapter but the data.
# ---------------------------------------------------------------------------
gp <- merge(H[, c("year", "gap")], S[, c("year", "gap")], by = "year",
            all = TRUE, suffixes = c("_house", "_senate"))
gp <- gp[order(gp$year), ]
dd_fig("gapfig", "line", gp, height = 390,
  size = list(m = list(t = 20, r = 30, b = 42, l = 60)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "gap_house", label = "distance between the party medians",
           domain = c(0.4, 1.0), fmt = "f3", ticks = 6),
  series = list(fields = list(
    list(field = "gap_house",  label = "House",  class = "series-1"),
    list(field = "gap_senate", label = "Senate", class = "series-2"))),
  legend = TRUE)

## ---- gap-static
par(mar = c(3.5, 4.2, 1, 1))
plot(H$year, H$gap, type = "l", lwd = 2.4, col = "#54278F", ylim = c(0.4, 1),
     xlab = "", ylab = "distance between party medians")
lines(S$year, S$gap, lwd = 2.4, col = "#E08214", lty = 2)
legend("topleft", c("House", "Senate"), col = c("#54278F", "#E08214"),
       lwd = 2.4, lty = c(1, 2), bty = "n", cex = 0.9)

## ---- chambers
o <- data.frame(
  chamber = c("House", "Senate"),
  `gap in that year` = c(nm(g(H, 90)), nm(g(S, 90))),
  `gap now` = c(nm(g(H, NOWH)), nm(g(S, NOWS))),
  change = c(paste0("+", nm(g(H, NOWH) - g(H, 90))),
             paste0("+", nm(g(S, NOWS) - g(S, 90)))),
  check.names = FALSE)
o

## ---- strip-prep
DCOL <- "#2166AC"; RCOL <- "#B2182B"          # party, and nothing else
STRIP <- nom[nom$chamber == "House" & nom$congress == NOWH, ]
# A beeswarm: members with similar scores are stacked so no two dots overlap.
# dd_swarm() packs them exactly as the D3 figure's swarm does, in the D3
# frame's own pixels (760 wide, 60 left and 26 right margin, x from -1 to 1,
# dot radius 3), so the PDF and the page draw the same picture.
SW_R <- 3; SW_PX <- (760 - 60 - 26) / 2       # pixels per unit of dimension 1
STRIP$sw <- dd_swarm(60 + (STRIP$dim1 + 1) * SW_PX, SW_R)

## ---- strip-d3
# One band, one dot per member, packed as a beeswarm (swarm = TRUE) so that
# members with similar scores stack rather than hide one another. The band
# grows to fit the tallest stack.
z <- STRIP
z$row <- as.character(YRNOW)
dd_fig("housenow", "dot",
       z[, c("dim1", "row", "party", "bioname", "state")],
       height = 230, swarm = TRUE,
       size = list(m = list(t = 22, r = 26, b = 52, l = 60)),
       x = list(field = "dim1", domain = c(-1, 1), ticks = 9, fmt = "f1",
                label = "DW-NOMINATE dimension 1 (liberal to conservative)"),
       y = list(field = "row"),
       series = list(field = "party",
                     classes = list(Democrat = "dem", Republican = "gop")),
       r = 3, legend = TRUE,
       annotations = list(dd_annot_vline(0)),
       tip = dd_tip(c(bioname = "member", state = "state", dim1 = "dimension 1"),
                    fmt = c(dim1 = "f2"), title = "party"))

## ---- strip-static
# The same swarm as the D3 figure. Its offsets are in D3 pixels, so the y axis
# is set to the same pixels-per-inch as the x axis; then a dot one pixel up is
# as far as a dot one pixel across, and the circles cannot overlap here either.
z <- STRIP
par(mar = c(4, 1, 2.4, 1))
plot.new()
pin <- par("pin")                                    # plot region, inches
yhalf <- pin[2] / 2 * (2 * SW_PX / pin[1])           # D3 pixels in half the height
plot.window(xlim = c(-1, 1), ylim = c(-yhalf, yhalf), xaxs = "i")
axis(1, at = seq(-1, 1, 0.25), cex.axis = 0.75)
title(xlab = "DW-NOMINATE dimension 1 (liberal to conservative)",
      main = paste0("The House, ", YRNOW), cex.main = 0.95)
abline(v = 0, lty = 3, col = "grey60")
symbols(z$dim1, -z$sw, circles = rep(SW_R / SW_PX, nrow(z)),
        inches = FALSE, add = TRUE, fg = NA,
        bg = ifelse(z$party == "Democrat", DCOL, RCOL))
legend("top", c("Democrat", "Republican"), pch = 19, col = c(DCOL, RCOL),
       horiz = TRUE, bty = "n", cex = 0.7)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
