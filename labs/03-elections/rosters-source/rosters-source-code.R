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

## ---- exttab
data.frame(Outcome = ext$outcome, Members = nn(ext$members),
           Decided_by_a_voter = ifelse(is.na(ext$voter_decided), "—",
                                ifelse(ext$voter_decided, "yes", "no")))

## ---- checks
ck
