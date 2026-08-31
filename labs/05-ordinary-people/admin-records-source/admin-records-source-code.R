# admin-records-source-code.R -- chunk bodies for admin-records-source-brief.Rmd
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

rec <- read.csv("data/derived/records.csv",      stringsAsFactors = FALSE)
den <- read.csv("data/derived/denominators.csv", stringsAsFactors = FALSE)
hit <- read.csv("data/derived/hit_rates.csv",    stringsAsFactors = FALSE)
con <- read.csv("data/derived/control.csv",      stringsAsFactors = FALSE)
uni <- read.csv("data/derived/unit.csv",         stringsAsFactors = FALSE)
ck  <- read.csv("data/derived/checks.csv",       stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)
p2 <- function(x) formatC(x, format = "f", digits = 2)
uv <- function(k) uni$value[uni$quantity == k]

LO   <- den[1, ]
HI   <- den[nrow(den), ]
SPREAD <- HI$disparity_ratio / LO$disparity_ratio

HW <- hit$hit_rate[hit$race == "white"]
HB <- hit$hit_rate[hit$race == "black"]
SW <- hit$search_rate[hit$race == "white"]
SB <- hit$search_rate[hit$race == "black"]

SB_ <- con$rate[con$comparison == "State strikes Black jurors"]
SW_ <- con$rate[con$comparison == "State strikes White jurors"]
DW_ <- con$rate[con$comparison == "Defense strikes White jurors"]
DB_ <- con$rate[con$comparison == "Defense strikes Black jurors"]

POOL <- uv("Pooled gap, points")
WITH <- uv("Median within-city gap, points")
REV  <- uv("Cities where the gap reverses")
NCIT <- uv("Cities")

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- rectab
data.frame(Record = rec$record, Written_by = rec$written_by,
           In_order_to = rec$in_order_to, One_row_is = rec$one_row_is)

## ---- silenttab
data.frame(Record = rec$record, Silent_about = rec$silent_about)

## ---- dentab
data.frame(Denominator = den$denominator,
           Black_stops_per_person = p2(den$black_stops_per_person),
           White_stops_per_person = p2(den$white_stops_per_person),
           Disparity_ratio = paste0(p2(den$disparity_ratio), "x"))

## ---- hittab
data.frame(Race = hit$race,
           Searched = nn(hit$searched),
           Search_rate = paste0(p1(hit$search_rate), "%"),
           Contraband_found = nn(hit$contraband),
           Hit_rate = paste0(p1(hit$hit_rate), "%"))

## ---- contab
data.frame(Comparison = con$comparison, Rate = paste0(p1(con$rate), "%"))

## ---- unittab
data.frame(Quantity = uni$quantity,
           Value = ifelse(uni$quantity %in% c("Cities", "Cities where the gap reverses"),
                          nn(uni$value), p1(uni$value)))

## ---- checks
ck
