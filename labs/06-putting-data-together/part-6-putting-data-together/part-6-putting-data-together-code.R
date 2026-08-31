# part-6-putting-data-together-code.R -- chunk bodies for part-6-putting-data-together-brief.Rmd
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

# The section's shape is read from the book's own index -- INDEX.md, written
# by _lib/make-index.py -- so this page cannot disagree with the section it
# opens: reorganise the section and re-render, and every count below follows.
idx  <- readLines("../../INDEX.md", warn = FALSE)
rows <- idx[startsWith(idx, "| ")]
f    <- strsplit(rows, "\\s*\\|\\s*")
cell <- vapply(f, function(x) if (length(x) > 3) x[[2]] else "", "")
typ  <- vapply(f, function(x) if (length(x) > 3) x[[3]] else "", "")

# Cluster rows of Section IV carry labels like "IV.1 Joining Files"; the
# section's intro row (this page) carries the bare section name and is not
# counted among the docs it introduces.
keep <- grepl("^IV\\.[0-9]", cell)
clu  <- cell[keep]
NDOC <- sum(keep)
NCLU <- length(unique(clu))
NCH  <- sum(typ[keep] == "chapter")
stopifnot(NDOC >= 15, NCLU >= 5)   # parsing drift must fail loudly, not quietly

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")

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
tab <- as.data.frame(table(factor(clu, levels = unique(clu))),
                     stringsAsFactors = FALSE)
names(tab) <- c("Cluster", "Docs")
tab
