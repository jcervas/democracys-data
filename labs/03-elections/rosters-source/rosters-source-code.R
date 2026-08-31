# rosters-source-code.R -- chunk bodies for rosters-source-brief.Rmd
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

ros <- read.csv("data/derived/rosters.csv", stringsAsFactors = FALSE)
exc <- read.csv("data/derived/excess.csv",  stringsAsFactors = FALSE)
ext <- read.csv("data/derived/exits.csv",   stringsAsFactors = FALSE)
car <- read.csv("data/derived/careers.csv", stringsAsFactors = FALSE)
ck  <- read.csv("data/derived/checks.csv",  stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)
cv <- function(k) ck$value[ck$check == k]
cvn <- function(k) as.numeric(gsub(",", "", cv(k)))

MEM   <- cvn("People who have served in the House")
SEATS <- cvn("Seats that have existed to serve in")
XS    <- cvn("Excess of people over seats")
XSP   <- cv("Excess as a share of seats, %")
DIED  <- cvn("Seats emptied by a death in office")
NCONG <- cv("Congresses in the membership series")
WM    <- cv("Worst single Congress: members")
WS    <- cv("Worst single Congress: seats")
WW    <- cv("Worst single Congress: which, and when")

DEP   <- cvn("Departures in that file")
BYV   <- cvn("Departures a voter decided")
NOV   <- cvn("Departures no voter decided")
NOVP  <- cv("Share no voter decided, %")
NCEX  <- cv("Congresses in the modern exit file")

NCAR  <- cvn("Careers in the career file")
NGAP  <- cvn("Careers that are not one continuous interval")
GAPP  <- car$value[car$quantity == "Share with a gap, %"]

# the figure's series: mid-Congress churn as a share of the seats that existed
exc$pct <- 100 * exc$excess / exc$seats
PCTWORST <- max(exc$pct)
YRWORST  <- exc$year[which.max(exc$pct)]
PCTLAST  <- exc$pct[which.max(exc$year)]
YRLAST   <- exc$year[which.max(exc$year)]

ACC <- "#1C4C5C"

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- rostab
data.frame(Roster = ros$roster, Published_by = ros$published_by,
           One_row_is = ros$one_row_is, Answers = ros$answers)

## ---- exctab
h <- exc[order(-exc$excess), ][1:8, ]
data.frame(Congress = h$congress, Year = h$year,
           Members = nn(h$members), Seats = nn(h$seats),
           Excess = nn(h$excess), Deaths_in_office = nn(h$died))

## ---- fig1-static
op <- par(mar = c(3.0, 4.6, 1.0, 1.4), mgp = c(2.8, 0.7, 0))
plot(exc$year, exc$pct, type = "l", lwd = 2.4, col = ACC, las = 1,
     xaxt = "n", xlab = "", ylab = "% more members than seats",
     ylim = c(0, ceiling(PCTWORST) + 4))
axis(1, at = seq(1790, 2020, 30), cex.axis = 0.8)
abline(h = seq(10, 40, 10), col = "#00000015")
w <- exc[which.max(exc$pct), ]
points(w$year, w$pct, pch = 19, cex = 0.8, col = ACC)
text(w$year, w$pct + 2.4, paste0(w$year, ": ", w$members, " members, ",
                                 w$seats, " seats"),
     cex = 0.68, col = "#4E5A63", pos = 4)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# One quantity, 118 Congresses, and the finding is the shape of the series --
# so it is a line, drawn with the shared library. dd_fig() emits the two
# <script src> tags for the document; hovering reads off each Congress's
# members, seats and excess, which the static twin cannot carry.
# ---------------------------------------------------------------------------
ex2 <- exc[order(exc$year),
           c("year", "congress", "members", "seats", "excess", "pct")]
ex2$pct <- round(ex2$pct, 1)
dd_fig("excessline", "line", ex2,
  size = list(w = 770, h = 400, m = list(t = 16, r = 30, b = 40, l = 56)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "pct", label = "% more members than seats",
           domain = c(0, ceiling(PCTWORST) + 4), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "pct", label = "members in excess of seats", class = "series-1"))),
  tip = dd_js('function(d){
    return "<b>Congress "+d.congress+" ("+d.year+")</b><br>"+
      DD.fmt.comma(d.members)+" members, "+DD.fmt.comma(d.seats)+" seats<br>"+
      "<b>"+d.excess+" more people than seats ("+d.pct.toFixed(1)+"%)</b>";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover for each Congress&#39;s members, seats and excess.</p>')

## ---- exttab
data.frame(Outcome = ext$outcome, Members = nn(ext$members),
           Decided_by_a_voter = ifelse(is.na(ext$voter_decided), "—",
                                ifelse(ext$voter_decided, "yes", "no")))

## ---- checks
ck
