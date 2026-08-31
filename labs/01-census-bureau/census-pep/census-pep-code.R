# census-pep-code.R -- chunk bodies for census-pep-brief.Rmd
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

rd <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
idn <- rd("identity.csv");  cmp <- rd("components.csv")
nat <- rd("natural.csv");   grw <- rd("growth.csv")
res <- rd("residual.csv")

# Read the ACS chapter's own table rather than repeating its numbers here, so
# that a rebuild next door cannot leave this paragraph quietly stale.
ctrl <- read.csv("../census-acs/data/derived/controlled.csv",
                 stringsAsFactors = FALSE)
ACSNOM <- ctrl$rows_with_no_margin[1]
ACSROW <- ctrl$county_rows[1]

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
CP <- function(k) cmp$people[cmp$component == k]
NA_ <- function(q) nat$value[nat$quantity == q]
GR <- function(q) grw$value[grw$quantity == q]
RS <- function(q) res$value[res$quantity == q]

NCTY  <- idn$counties_checked[1]
TOTCH <- CP("Total change in 2024")
INTL  <- CP("International migration")
INTLP <- 100 * INTL / TOTCH

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

ACC <- "#1C4C5C"; WARN <- "#8A3B2C"

## ---- idntab
data.frame(Consistency_check = idn$check,
           Counties_checked = nn(idn$counties_checked),
           Violations = nn(idn$violations))

## ---- raw
cat(paste(readLines("data/raw/arrives.txt"), collapse = "\n"))

## ---- comp-static
# The chapter's one figure: the eight terms of the 2024 arithmetic, in
# millions of people, as diverging bars off a shared zero line. The total is
# set apart in the warning colour because it is the sum, not an addend.
# Twin of comp-d3 below; keep the two in step.
cm  <- cmp$people / 1e6
lb  <- cmp$component
cl  <- ifelse(cmp$component == "Total change in 2024", WARN, ACC)
op  <- par(mar = c(4.0, 10.6, 0.6, 3.2), mgp = c(2.4, 0.7, 0))
lim <- c(min(cm) * 1.25, max(cm) * 1.25)
bp  <- barplot(rev(cm), horiz = TRUE, col = rev(cl), border = NA,
               axes = FALSE, names.arg = rev(lb), las = 1,
               cex.names = 0.78, xlim = lim)
axis(1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
abline(v = 0, col = "#76838C")
mtext("Millions of people, 2024", 1, line = 2.4, cex = 0.9)
vl <- sprintf("%+.1f", rev(cm))
vl[rev(cm) == 0] <- "0.0"                 # match the d3 twin: no sign on zero
text(rev(cm), bp, vl,
     pos = ifelse(rev(cm) < 0, 2, 4), cex = 0.72,
     col = "#4E5A63", xpd = NA)
par(op)

## ---- comp-d3
# Same eight bars, drawn with the shared library (_lib/dd-charts.js).
# Hovering a bar gives the exact count of people behind the rounded
# millions. Twin of comp-static above; keep the two in step.
cfig <- data.frame(component = cmp$component,
                   people = cmp$people,
                   m = round(cmp$people / 1e6, 2),
                   cls = ifelse(cmp$component == "Total change in 2024",
                                "series-2", "series-1"))
dd_fig("compfig", "bar", cfig,
  size = list(w = 770, m = list(l = 170, r = 64)),
  rowHeight = 26,
  x = list(field = "m", fmt = "signed1",
           label = "millions of people, 2024"),
  y = list(field = "component", band = TRUE),
  valueLabels = TRUE,
  tip = dd_tip(c(people = "people, 2024"), fmt = c(people = "comma"),
               title = "component"))

## ---- nattab
data.frame(Quantity = nat$quantity,
           Value = ifelse(nat$unit == "%", paste0(p1(nat$value), "%"),
                          nn(nat$value)))

## ---- grwtab
data.frame(Quantity = grw$quantity, Counties = nn(grw$value))

## ---- restab
data.frame(Quantity = res$quantity,
           Value = ifelse(res$unit == "%", paste0(p1(res$value), "%"),
                          nn(res$value)))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
