# census-pep-code.R -- chunk bodies for census-pep-brief.Rmd
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
idn <- rd("identity.csv");  cmp <- rd("components.csv")
nat <- rd("natural.csv");   grw <- rd("growth.csv")
res <- rd("residual.csv")

# Read the ACS chapter's own table rather than repeating its numbers here, so
# that a rebuild next door cannot leave this paragraph quietly stale.
ctrl <- read.csv("../census-acs/data/derived/controlled.csv",
                 stringsAsFactors = FALSE)
ACSNOM <- ctrl$rows_with_no_margin[1]
ACSROW <- ctrl$county_rows[1]

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
CP <- function(k) cmp$people[cmp$component == k]
NA_ <- function(q) nat$value[nat$quantity == q]
GR <- function(q) grw$value[grw$quantity == q]
RS <- function(q) res$value[res$quantity == q]

NCTY  <- idn$counties_checked[1]
TOTCH <- CP("Total change in 2024")
INTL  <- CP("International migration")
INTLP <- 100 * INTL / TOTCH

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

## ---- idntab
data.frame(Consistency_check = idn$check,
           Counties_checked = nn(idn$counties_checked),
           Violations = nn(idn$violations))

## ---- raw
cat(paste(readLines("data/raw/arrives.txt"), collapse = "\n"))

## ---- cmptab
data.frame(Component = cmp$component, People = nn(cmp$people))

## ---- nattab
data.frame(Quantity = nat$quantity,
           Value = ifelse(nat$unit == "%", paste0(p1(nat$value), "%"),
                          nn(nat$value)))

## ---- grwtab
data.frame(Quantity = grw$quantity, Counties = nn(grw$value))

## ---- restab
data.frame(Quantity = res$quantity,
           Value = ifelse(res$unit == "%", paste0(p1(res$value), "%"),
                          nn(res$value)))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
