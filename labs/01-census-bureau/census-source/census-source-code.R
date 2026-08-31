# census-source-code.R -- chunk bodies for census-source-brief.Rmd
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

gran <- read.csv("data/derived/granularity.csv", stringsAsFactors = FALSE)
chk  <- read.csv("data/derived/consistency.csv", stringsAsFactors = FALSE)
pops <- read.csv("data/derived/populations.csv", stringsAsFactors = FALSE)
acs  <- read.csv("data/derived/acs.csv",         stringsAsFactors = FALSE)
inst <- read.csv("data/derived/instruments.csv", stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)
gg <- function(q) gran$value[gran$quantity == q]
av <- function(q) acs$value[acs$quantity == q]

NBLK  <- gg("Blocks in Georgia")
MEDP  <- gg("Median population of a populated block")
ONE   <- gg("Blocks containing exactly one person")
UND10 <- gg("Blocks containing fewer than ten people")
ZERO  <- gg("Blocks with nobody in them")
TOT   <- gg("Total population, summed from blocks")

RES <- pops$value[pops$population == "Resident population, 2020 census"]
APP <- pops$value[pops$population == "Apportionment population, 2020 census"]
DIF <- pops$value[pops$population == "Difference"]

NFLOW <- av("Published state-to-state flows")
BADF  <- av("Flows whose margin of error exceeds the estimate")
BADP  <- av("Share of flows whose margin exceeds the estimate")

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

## ---- grantab
data.frame(Quantity = gran$quantity,
           Value = ifelse(gran$unit == "%", paste0(p1(gran$value), "%"),
                          nn(gran$value)))

## ---- insttab
data.frame(The_question = inst$question,
           Decennial_census = inst$decennial,
           American_Community_Survey = inst$acs,
           Population_estimates = inst$estimates)

## ---- acstab
data.frame(Quantity = acs$quantity,
           Value = ifelse(acs$unit == "%", paste0(p1(acs$value), "%"),
                          nn(acs$value)))

## ---- poptab
data.frame(Population = pops$population, Value = nn(pops$value),
           What_it_is_for = pops$what_it_is_for)

## ---- chktab
data.frame(Consistency_check = chk$check,
           Violations = nn(chk$violations),
           Blocks_checked = nn(chk$of))

## ---- raw
cat(paste(readLines("data/raw/block.txt"), collapse = "\n"))
