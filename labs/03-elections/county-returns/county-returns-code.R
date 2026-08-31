# county-returns-code.R -- chunk bodies for county-returns-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.0,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

fmt    <- rd("formats.csv")
notcty <- rd("not_counties.csv")
shapes <- rd("unit_shapes.csv")
gaps   <- rd("largest_gaps.csv")
tpz    <- rd("third_party_zero.csv")

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

## ---- formats
fmt

## ---- notcty
notcty

## ---- agree
data.frame(
  quantity = c("Democratic vote", "Republican vote", "total votes cast"),
  counties_disagreeing = c(n(FV("diff_dem")), n(FV("diff_gop")), n(FV("diff_total"))),
  share = c(paste0(pc(100 * FN("diff_dem") / FN("matched"), 1), "%"),
            paste0(pc(100 * FN("diff_gop") / FN("matched"), 1), "%"),
            paste0(pc(FV("diff_total_pct"), 1), "%")),
  stringsAsFactors = FALSE)

## ---- magfig
b <- c(FN("diff_total") - FN("dt_over_100"),
       FN("dt_over_100") - FN("dt_over_1000"),
       FN("dt_over_1000"))
op <- par(mar = c(4.0, 9.6, 1.0, 3.4), cex = 0.86)
bp <- barplot(rev(b), horiz = TRUE, col = c("#8c2d19", "#c47a4a", "#c9d6e3"),
              border = NA, xlab = "counties", las = 1,
              names.arg = rev(c("1-100 votes", "101-1,000 votes",
                                "more than 1,000")),
              xlim = c(0, max(b) * 1.18))
text(rev(b), bp, paste0(" ", n(rev(b))), adj = 0, cex = 0.95)
par(op)

## ---- tpz
tpz

## ---- kc
data.frame(source = c("certified state return", "the compilation"),
           total_votes = c(n(FV("kc_off")), n(FV("kc_com"))),
           stringsAsFactors = FALSE)

## ---- shapes
s <- shapes
names(s) <- c("jurisdiction", "certified rows", "compilation rows",
              "certified unit", "compilation unit")
s

## ---- gaps
g <- gaps
names(g) <- c("fips", "county", "state", "certified", "compilation", "difference")
g$certified <- n(g$certified); g$compilation <- n(g$compilation)
g$difference <- n(g$difference)
g

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
