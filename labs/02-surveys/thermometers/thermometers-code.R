# thermometers-code.R -- chunk bodies for thermometers-brief.Rmd
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

S  <- read.csv("data/derived/thermometers_by_year.csv", stringsAsFactors = FALSE)
Z  <- read.csv("data/derived/out_party_zero.csv",       stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",                stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",               stringsAsFactors = FALSE)

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)

knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

INP <- "#1b7837"; OUT <- "#762a83"

## ---- heap-table
data.frame(
  Rating = c("0 — as cold as possible", "50 — the midpoint", "85",
             paste(F("top_code"), "— the top of the scale")),
  `Share of all ratings` = paste0(c(p1(F("pct_at_0")), p1(F("pct_at_50")),
                                    p1(F("pct_at_85")), p1(F("pct_at_97"))), "%"),
  check.names = FALSE)

## ---- halves-d3
# ---------------------------------------------------------------------------
# The subtraction, undone: both halves of the polarization number, with the
# band between them AS the number, so the reader sees the quoted gap and its
# two parents at once. Hovering reports all three for any year.
#
# Drawn with the shared library (_lib/dd-charts.js); dd_fig() emits the two
# <script src> tags for the document. Colours are series classes, so the page
# restyles in one place and survives dark mode.
# ---------------------------------------------------------------------------
m <- S[order(S$year), ]
dd_fig("th", "line", m,
  size = list(w = 770, h = 380, m = list(t = 16, r = 120, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "in_party", label = "degrees of warmth",
           domain = c(0, 80), fmt = "f0", ticks = 5),
  series = list(fields = list(
    list(field = "in_party", label = "one's own party",
         endLabel = c("one's own", "party"), class = "series-1"),
    list(field = "out_party", label = "the other party",
         endLabel = c("the other", "party"), class = "series-2"))),
  band = list(y0 = "out_party", y1 = "in_party"),
  points = TRUE, endLabels = TRUE,
  annotations = list(list(type = "hline", y = 50, class = "rule"),
                     list(type = "text", x = 1979, y = 52,
                          text = "neither warm nor cold", size = 10,
                          class = "lbl")),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b> &middot; "+DD.fmt.comma(d.n)+" partisans<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> own party: "+
        d.in_party.toFixed(1)+"&deg;<br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> other party: "+
        d.out_party.toFixed(1)+"&deg;<br>"+
      "<b>gap = "+d.gap.toFixed(1)+"&deg;</b>";
  }'))

## ---- halves-static
par(mar = c(3.2, 4.0, 1.2, 7.2))
plot(S$year, S$in_party, type = "n", ylim = c(0, 80), axes = FALSE,
     xlab = "", ylab = "degrees")
abline(h = 50, lty = 3, col = "grey65")
lines(S$year, S$in_party,  col = INP, lwd = 2.4)
lines(S$year, S$out_party, col = OUT, lwd = 2.4)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)
L <- nrow(S)
text(S$year[L] + 1, S$in_party[L],  paste("own party", round(S$in_party[L])),
     col = INP, adj = 0, cex = 0.75, xpd = NA)
text(S$year[L] + 1, S$out_party[L], paste("other party", round(S$out_party[L])),
     col = OUT, adj = 0, cex = 0.75, xpd = NA)

## ---- zero-static
par(mar = c(3.2, 4.4, 1.2, 1.2))
plot(Z$year, Z$pct_zero, type = "n", axes = FALSE, xlab = "",
     ylab = "% rating them 0", ylim = c(0, max(Z$pct_zero) * 1.1))
lines(Z$year, Z$pct_zero, col = OUT, lwd = 2.2)
points(Z$year, Z$pct_zero, col = OUT, pch = 19, cex = 0.55)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)

## ---- checks-table
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)
