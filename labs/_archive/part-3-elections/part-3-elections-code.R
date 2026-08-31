# part-3-elections-code.R -- chunk bodies for part-3-elections-brief.Rmd
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

rd <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
ch <- rd("chapters.csv"); bt <- rd("beats.csv"); ru <- rd("reuse.csv")

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
RU <- function(q) ru$value[ru$quantity == q]

NCH   <- nrow(ch)
OUT   <- RU("Chapters outside this part")
GOES  <- RU("Of those, that go to the Census Bureau themselves")
DEPS  <- RU("Of those, that read this part's own derived files")

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- beattab
data.frame(Beat = bt$beat, Chapters = nn(bt$chapters),
           What_it_does = bt$what_it_does)

## ---- chaptab
data.frame(Beat = ifelse(ch$companion, paste0(ch$beat, " \u00b7 companion"), ch$beat),
           Chapter = ch$chapter, Title = ch$title)

## ---- reusetab
data.frame(Quantity = ru$quantity, Chapters = nn(ru$value))
