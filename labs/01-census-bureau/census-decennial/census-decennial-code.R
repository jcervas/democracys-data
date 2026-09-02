# census-decennial-code.R -- chunk bodies for census-decennial-brief.Rmd
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

rd  <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
tab <- rd("tables.csv");   nat  <- rd("national.csv")
race <- rd("race.csv");    dead <- rd("deadlines.csv")
cov  <- rd("coverage.csv");  sc  <- rd("scale.csv")
cost <- rd("cost.csv")
use  <- rd("census-use.csv"); adjm <- rd("adjustments.csv")

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
p4 <- function(x) formatC(as.numeric(x), format = "f", digits = 4)
N  <- function(q) nat$value[nat$quantity == q]
R  <- function(g) race$people[race$group == g]
CV <- function(g, col) cov[[col]][cov$group == g]
CO <- function(q) cost$value[cost$quantity == q]
AD <- function(q) adjm$value[adjm$quantity == q]
UU <- function(r, col) use[[col]][use$requirement == r]

NCELL <- sum(tab$cells)
NTAB  <- nrow(tab)
P1N   <- tab$cells[tab$table == "P1"]
COMBO <- P1N - 9
SUBTOT <- 5                      # the "Population of N races" subtotal lines
COMBOR <- COMBO - SUBTOT         # combinations proper
POP   <- N("Total population")

W        <- function(t) tab$cells[tab$table == t]
RACECELL <- W("P1") + W("P2") + W("P3") + W("P4")
RESTCELL <- W("H1") + W("P5")

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

# Rows for the coverage figure. Short display labels, because the Bureau's
# full group names run to fifty characters; the tooltip carries the full name.
# Ordered with the deepest undercount at the top, and shared by both twins so
# the two formats cannot disagree about which row went where.
cov_rows <- function() {
  short <- c("Black or African American" = "Black",
             "American Indian or Alaska Native, on reservation" =
               "American Indian, on reservation",
             "Hispanic or Latino" = "Hispanic or Latino",
             "Native Hawaiian or Other Pacific Islander" = "Pacific Islander",
             "Asian" = "Asian",
             "White, not Hispanic" = "White, not Hispanic")
  cv <- cov[order(cov$pes_2020), ]
  cv$label <- unname(short[cv$group])
  cv
}

## ---- form
cat(paste(readLines("data/raw/form.txt"), collapse = "\n"))

## ---- tabtab
data.frame(Table = tab$table, Variables = nn(tab$cells),
           What_it_counts = tab$what_it_counts)

## ---- racetab
data.frame(Group = race$group, People = nn(race$people),
           Share_of_the_country = paste0(p1(race$share_of_us), "%"))

## ---- cov-static
cv <- cov_rows()
n_cv <- nrow(cv)
par(mar = c(3.8, 11.5, 0.6, 1.0))
plot(NA, xlim = c(-6.5, 3.5), ylim = c(0.6, n_cv + 0.4), axes = FALSE,
     xlab = "", ylab = "")
abline(v = seq(-6, 3, 1), col = "#00000018", lty = 3)
abline(v = 0, col = "#666")
ypos <- n_cv:1                               # first row (deepest undercount) on top
segments(cv$ccm_2010, ypos, cv$pes_2020, ypos, col = "#bbb", lwd = 2)
points(cv$ccm_2010, ypos, pch = 19, cex = 1.0, col = "#2c7fb8")
points(cv$pes_2020, ypos, pch = 19, cex = 1.0, col = "#C41230")
axis(1, at = seq(-6, 3, 1), labels = sprintf("%+d", seq(-6, 3, 1)),
     cex.axis = 0.7, mgp = c(2.2, 0.7, 0))
axis(2, at = ypos, labels = cv$label, las = 1, tick = FALSE, cex.axis = 0.7,
     line = -0.5)
title(xlab = "net coverage error, per cent of the group (negative = undercount)",
      line = 2.3, cex.lab = 0.8)
legend("bottomright", c("2010", "2020"), pch = 19,
       col = c("#2c7fb8", "#C41230"), bty = "n", cex = 0.75)

## ---- cov-d3
# Drawn with the shared library: a dumbbell, one row per group, the 2010 and
# 2020 measurements as the two ends. The solid rule is a perfect count.
cv <- cov_rows()
dd_fig("coverage", "dumbbell",
       cv[, c("group", "label", "ccm_2010", "pes_2020", "change")],
  y = list(field = "label"),
  a = list(field = "ccm_2010", label = "2010"),
  b = list(field = "pes_2020", label = "2020"),
  x = list(domain = c(-6.5, 3.5), fmt = "signed0",
           label = "net coverage error, per cent of the group (negative = undercount)"),
  aClass = "series-1", bClass = "series-2",
  size = list(m = list(l = 210)),
  annotations = list(dd_annot_vline(0, class = "zero", dash = FALSE)),
  tip = dd_tip(c(ccm_2010 = "2010 net error", pes_2020 = "2020 net error",
                 change = "movement"),
               fmt = c(ccm_2010 = "signed2", pes_2020 = "signed2",
                       change = "signed2"),
               title = "group"))

## ---- covtab
data.frame(Group = cov$group,
           Net_2020 = paste0(ifelse(cov$pes_2020 > 0, "+", ""), p2(cov$pes_2020), "%"),
           Net_2010 = paste0(ifelse(cov$ccm_2010 > 0, "+", ""), p2(cov$ccm_2010), "%"),
           Change = paste0(ifelse(cov$change > 0, "+", ""), p2(cov$change)),
           Differs_from_zero = cov$differs_from_zero,
           Differs_from_2010 = cov$differs_from_2010)

## ---- scaletab
data.frame(Group = sc$group,
           Published_count = nn(sc$published),
           National_rate = paste0(ifelse(sc$national_rate > 0, "+", ""),
                                  p2(sc$national_rate), "%"),
           Implied_people = paste0(nn(abs(sc$implied_people)), " ",
                                   ifelse(sc$implied_people > 0,
                                          "missed", "in excess")))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
