# gender-gap-code.R -- chunk bodies for gender-gap-brief.Rmd
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

S  <- read.csv("data/derived/partyid_by_sex.csv",         stringsAsFactors = FALSE)
M  <- read.csv("data/derived/partyid_by_sex_marital.csv", stringsAsFactors = FALSE)
D  <- read.csv("data/derived/by_decade.csv",              stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",                  stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",                 stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("by_decade.csv", "checks.csv", "facts.csv", "partyid_by_sex.csv", "partyid_by_sex_marital.csv"))

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
sg <- function(x) sprintf("%+.1f", as.numeric(x))

knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

WOM <- "#6a3d9a"   # women, in the static twins
MEN <- "#ff7f00"   # men
GAP <- "#333333"

## ---- fig1-static
par(mar = c(3.2, 4.2, 1.4, 1.2))
plot(S$year, S$gap, type = "n", xlab = "", ylab = "gap, points",
     ylim = c(-4, 22), axes = FALSE)
abline(h = 0, col = "grey60", lty = 3)
lines(S$year, S$gap, col = GAP, lwd = 2)
points(S$year, S$gap, col = GAP, pch = 19, cex = 0.5)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The gap drawn the way it is usually drawn: one line, because one number per
# year is what gets quoted. Hovering hands back the two numbers the
# subtraction destroyed, which is the brief's whole argument in a tooltip.
# Drawn with the shared library (_lib/dd-charts.js); dd_fig() emits the
# <script src> tags once for the document.
# ---------------------------------------------------------------------------
m <- S[order(S$year), ]
dd_fig("gg-gap", "line", m,
  size = list(w = 770, h = 340, m = list(t = 16, r = 96, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "gap", label = "gap, percentage points",
           domain = c(-4, 22), fmt = "signed0", ticks = 6),
  series = list(fields = list(
    list(field = "gap", label = "the gap", endLabel = "the gap",
         class = "series-1"))),
  points = TRUE, endLabels = TRUE,
  annotations = list(list(type = "hline", y = 0, class = "zero")),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "women "+DD.fmt.signed1(d.women)+" &middot; men "+DD.fmt.signed1(d.men)+"<br>"+
      "<b>gap = "+d.gap.toFixed(1)+" pts</b>";
  }'))

## ---- fig2-static
par(mar = c(3.2, 4.2, 1.4, 6.5))
plot(S$year, S$women, type = "n", xlab = "", ylab = "net Democratic, points",
     ylim = c(-12, 34), axes = FALSE)
abline(h = 0, col = "grey60", lty = 3)
lines(S$year, S$women, col = WOM, lwd = 2.2)
lines(S$year, S$men,   col = MEN, lwd = 2.2)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)
ly <- nrow(S)
text(S$year[ly] + 1, S$women[ly], paste("women", sg(S$women[ly])),
     col = WOM, adj = 0, cex = 0.78, xpd = NA)
text(S$year[ly] + 1, S$men[ly], paste("men", sg(S$men[ly])),
     col = MEN, adj = 0, cex = 0.78, xpd = NA)

## ---- fig2-d3
# The same years with the subtraction undone. The band between the lines IS
# the gap, so the reader sees the quoted number and its two parents at once;
# hovering reports all three for any year.
m <- S[order(S$year), ]
dd_fig("gg-pair", "line", m,
  size = list(w = 770, h = 380, m = list(t = 16, r = 110, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "women", label = "net Democratic, points",
           domain = c(-12, 34), fmt = "signed0", ticks = 6),
  series = list(fields = list(
    list(field = "women", label = "women", class = "series-1"),
    list(field = "men",   label = "men",   class = "series-2"))),
  band = list(y0 = "men", y1 = "women"),
  points = TRUE, endLabels = TRUE,
  annotations = list(list(type = "hline", y = 0, class = "zero")),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b> &middot; "+DD.fmt.comma(d.n_men + d.n_women)+" respondents<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> women: "+DD.fmt.signed1(d.women)+"<br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> men: "+DD.fmt.signed1(d.men)+"<br>"+
      "<b>gap = "+d.gap.toFixed(1)+" pts</b>";
  }'))

## ---- decade-table
data.frame(
  Decade  = D$decade,
  Years   = D$years,
  Men     = sg(D$men),
  Women   = sg(D$women),
  Gap     = p1(D$gap),
  check.names = FALSE)

## ---- marital-table
LM <- M[which.max(M$year), ]
data.frame(
  Group = c("Unmarried women", "Married women", "Unmarried men", "Married men"),
  `Net Democratic` = sg(c(LM$unmarried_women, LM$married_women,
                          LM$unmarried_men, LM$married_men)),
  check.names = FALSE)

## ---- checks-table
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
