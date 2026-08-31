# clerk-source-code.R -- chunk bodies for clerk-source-brief.Rmd
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

doc <- read.csv("data/derived/documents.csv", stringsAsFactors = FALSE)
edg <- read.csv("data/derived/edges.csv",     stringsAsFactors = FALSE)
bug <- read.csv("data/derived/bugs.csv",      stringsAsFactors = FALSE)
sur <- read.csv("data/derived/survivors.csv", stringsAsFactors = FALSE)
cov <- read.csv("data/derived/coverage.csv",  stringsAsFactors = FALSE)
ck  <- read.csv("data/derived/checks.csv",    stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)
cv <- function(k) ck$value[ck$check == k]
cvn <- function(k) as.numeric(gsub(",", "", cv(k)))

NDOC  <- cvn("Publications read")
SPAN  <- cv("Elections they cover")
MB    <- cv("Total size of the documents, MB")
LINES <- cv("Lines of text pdftotext produced from them")
NROWP <- cv("District-elections the parse recovered")
NOVER <- cv("Districts in the overlap used to validate the parse")
MEDD  <- cv("Median disagreement in two-party Democratic share, points")
GT1   <- cv("Districts differing by more than 1 point")
GT2   <- cv("Districts differing by more than 2 points")
LAP   <- cv("Rows flagged la_primary")
RUN   <- cv("Rows flagged runoff_mixed")
UNC   <- cv("Uncontested races the parse recovered")
NONEY <- cv("Years with no presidential-by-district figure at all")
NPART <- cv("Years with partial coverage")
NFULL <- cv("Years with complete coverage")
PEAK  <- cv("Split districts at their peak")
Y2012 <- cv("Split districts in 2012")

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- doctab
data.frame(Year = doc$year, Kilobytes = nn(doc$kilobytes),
           Extracted_lines = nn(doc$extracted_lines))

## ---- edgetab
data.frame(Edge_case = edg$edge_case, Where = edg$where,
           What_the_page_does = edg$what_the_page_does,
           If_mishandled = edg$if_mishandled)

## ---- bugtab
data.frame(Bug = bug$bug, Cause = bug$cause, Worst_case = bug$worst_case,
           Scale = bug$scale)

## ---- surtab
data.frame(Disagreement = sur$disagreement, Whose = sur$whose,
           Resolution = sur$resolution)

## ---- covtab
h <- cov[cov$status != "complete", ]
data.frame(Year = h$year, Districts_with_a_figure = nn(h$districts_with),
           Districts = nn(h$districts_total), Status = h$status)

## ---- checks
ck
