# census-decennial-code.R -- chunk bodies for census-decennial-brief.Rmd
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

rd  <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
tab <- rd("tables.csv");   ga   <- rd("georgia.csv")
race <- rd("race.csv");    dead <- rd("deadlines.csv")
cov  <- rd("coverage.csv");  sc  <- rd("scale.csv")
cost <- rd("cost.csv")
use  <- rd("census-use.csv"); adjm <- rd("adjustments.csv")

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
G  <- function(q) ga$value[ga$quantity == q]
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
POP   <- G("Total population")

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

## ---- form
cat(paste(readLines("data/raw/form.txt"), collapse = "\n"))

## ---- tabtab
data.frame(Table = tab$table, Variables = nn(tab$cells),
           What_it_counts = tab$what_it_counts)

## ---- racetab
data.frame(Group = race$group, People = nn(race$people),
           Share_of_state = paste0(p1(race$share_of_state), "%"))

## ---- deadtab
data.frame(Delivery = dead$delivery, Statute = dead$statute,
           Due = dead$due, Delivered = dead$delivered,
           Days_late = nn(dead$days_late))

## ---- covtab
data.frame(Group = cov$group,
           Net_2020 = paste0(ifelse(cov$pes_2020 > 0, "+", ""), p2(cov$pes_2020), "%"),
           Net_2010 = paste0(ifelse(cov$ccm_2010 > 0, "+", ""), p2(cov$ccm_2010), "%"),
           Change = paste0(ifelse(cov$change > 0, "+", ""), p2(cov$change)),
           Differs_from_zero = cov$differs_from_zero,
           Differs_from_2010 = cov$differs_from_2010)

## ---- usetab
data.frame(What_the_state_law_says = use$requirement,
           Legislative_districts   = use$legislative,
           Congressional_districts = use$congressional)

## ---- scaletab
data.frame(Group = sc$group,
           Published_in_Georgia = nn(sc$published_in_georgia),
           National_rate = paste0(ifelse(sc$national_rate > 0, "+", ""),
                                  p2(sc$national_rate), "%"),
           Implied_people = paste0(nn(abs(sc$implied_people)), " ",
                                   ifelse(sc$implied_people > 0,
                                          "missed", "in excess")))
