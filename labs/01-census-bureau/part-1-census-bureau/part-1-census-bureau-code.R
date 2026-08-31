# part-1-census-bureau-code.R -- chunk bodies for part-1-census-bureau-brief.Rmd
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
ct <- rd("contents.csv"); cl <- rd("clusters.csv"); ru <- rd("reuse.csv")

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
RU <- function(q) ru$value[ru$quantity == q]

NCL   <- nrow(cl)
NCHAP <- sum(ct$type == "chapter")
NBRF  <- sum(ct$type == "brief")
OUT   <- RU("Docs outside this section")
GOES  <- RU("Of those, that go to the Census Bureau themselves")
DEPS  <- RU("Of those, that read this section's own files")

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- clustertab
data.frame(Cluster = paste(cl$cluster, "·", cl$name),
           The_reading = ifelse(cl$reading == "", "—", cl$reading),
           Briefs = nn(cl$briefs))

## ---- contentstab
data.frame(Cluster = ct$cluster, Type = ct$type, Title = ct$title,
           What_it_is_about = ct$topic)

## ---- reusetab
data.frame(Quantity = ru$quantity, Docs = nn(ru$value))
