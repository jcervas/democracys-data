# survey-access-code.R -- chunk bodies for survey-access-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 3.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

status  <- rd("status.csv")
refusal <- rd("refusal.csv")

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("checks.csv", "client_dependent.csv", "facts.csv", "refusal.csv", "status.csv"))
probe   <- read.csv("data/raw/probe-2026-08-13.csv", stringsAsFactors = FALSE)

n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

knit_print.data.frame <- function(x, ...) {
  nm <- names(x); nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- status
s <- status
s$open <- NULL
s$max_bytes <- n(s$max_bytes)
names(s) <- c("archive", "what the address is", "curl", "urllib", "wall", "bytes")
s

## ---- refusal
r <- refusal
names(r) <- c("HTTP", "wall", "archives", "what a program sees")
r

## ---- disagree
d <- probe[probe$archive == FV("disagree_archive"), c("client", "http", "bytes")]
d$bytes <- n(d$bytes)
names(d) <- c("client", "HTTP", "bytes returned")
d

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
