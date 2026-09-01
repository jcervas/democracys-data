# ideology-code.R -- chunk bodies for ideology-brief.Rmd
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

pl   <- read.csv("data/derived/placement.csv", stringsAsFactors = FALSE)
mid  <- read.csv("data/derived/middles.csv",   stringsAsFactors = FALSE)
coll <- read.csv("data/derived/collapse.csv",  stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

mv <- function(m, col) mid[[col]][mid$measure == m]
NMOD <- mv("Respondents", "moderate")
NDK  <- mv("Respondents", "not_placed")
EDM  <- mv("Holds a college degree", "moderate")
EDD  <- mv("Holds a college degree", "not_placed")
VTM  <- mv("Voted", "moderate")
VTD  <- mv("Voted", "not_placed")
SPM  <- mv("Strong partisan, strong Democrat or strong Republican", "moderate")
SPD  <- mv("Strong partisan, strong Democrat or strong Republican", "not_placed")

FIRST <- min(pl$year); LAST <- max(pl$year)
r1 <- pl[pl$year == FIRST, ]; r2 <- pl[pl$year == LAST, ]
DKSHARE <- round(100 * NDK / (NMOD + NDK), 1)

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

LIB <- "#2B5C8A"; CON <- "#A33B2A"; MODC <- "#8A8F94"; DKC <- "#C08A2E"

## ---- raw
cat(paste(readLines("data/raw/scale.txt"), collapse = "\n"))

## ---- tab1
m <- mid[mid$measure != "Respondents", ]
data.frame(Measure = m$measure,
           Moderate = ifelse(m$unit == "%", paste0(p1(m$moderate), "%"),
                             p1(m$moderate)),
           Havent_thought_much = ifelse(m$unit == "%",
                             paste0(p1(m$not_placed), "%"), p1(m$not_placed)))

## ---- fig1-static
op <- par(mar = c(3.4, 4.0, 1.4, 7.4), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = range(pl$year), ylim = c(0, max(pl$conservative) + 5),
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(1972, 2024, 13), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% of respondents", 2, line = 2.7, cex = 0.9)
sers <- list(conservative = CON, liberal = LIB, moderate = MODC,
             not_placed = DKC)
for (s in names(sers)) {
  lines(pl$year, pl[[s]], col = sers[[s]], lwd = 2.4,
        lty = if (s == "not_placed") 2 else 1)
  points(pl$year, pl[[s]], col = sers[[s]], pch = 19, cex = 0.55)
}
labs <- c(conservative = "conservative", liberal = "liberal",
          moderate = "moderate", not_placed = "haven't\nthought much")
for (s in names(sers))
  text(LAST, pl[[s]][pl$year == LAST], paste0(" ", labs[s]),
       col = sers[[s]], pos = 4, cex = 0.7, xpd = NA)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Four series whose whole point is what happens to their sum: when the
# "haven't thought much" line falls, the hover rules across all four at once
# and shows which of the others caught the fall. Drawn with the shared
# library (_lib/dd-charts.js); dd_fig() emits the two <script src> tags for
# the document. Colours are series classes, so the page restyles in one
# place and survives dark mode.
# ---------------------------------------------------------------------------
m <- pl[order(pl$year), ]
dd_fig("ide", "line", m,
  size = list(w = 770, h = 430, m = list(t = 16, r = 170, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 7),
  y = list(field = "conservative", label = "% of respondents",
           domain = c(0, max(c(m$liberal, m$moderate, m$conservative,
                               m$not_placed)) + 5),
           fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "liberal",      label = "liberal",      class = "series-1"),
    list(field = "moderate",     label = "moderate",     class = "series-3"),
    list(field = "conservative", label = "conservative", class = "series-2"),
    list(field = "not_placed",   label = "haven't thought much",
         endLabel = c("haven't", "thought much"), class = "series-4"))),
  points = TRUE, endLabels = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b> &middot; "+DD.fmt.comma(d.n)+" respondents<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> liberal: "+
        d.liberal.toFixed(1)+"%<br>"+
      "<span class=\'series-3-txt\'>&#9632;</span> moderate: "+
        d.moderate.toFixed(1)+"%<br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> conservative: "+
        d.conservative.toFixed(1)+"%<br>"+
      "<span class=\'series-4-txt\'>&#9632;</span> haven\'t thought much: "+
        d.not_placed.toFixed(1)+"%";
  }'))

## ---- tab2
data.frame(Seven_point = coll$seven_point,
           Collapses_to = coll$three_category,
           Respondents = nn(coll$respondents))
