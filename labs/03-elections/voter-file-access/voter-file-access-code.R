# voter-file-access-code.R -- chunk bodies for voter-file-access-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 3.8,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

funnel <- rd("funnel.csv")
shape  <- rd("page_shape.csv")
claims <- rd("claimed_downloads.csv")
rot    <- rd("link_rot.csv")

# What the two published price tables say, side by side. `bp_price` is kept as
# the prose Ballotpedia prints, because for eight jurisdictions the price is
# not a single amount; `bp_price_usd` is filled in only where it is.
price <- rd("price.csv")
PCMP  <- price[price$bp_price_is_one_number == "yes", ]
PGAP  <- PCMP[PCMP$same_price == "no", ]
PGAP  <- PGAP[order(-abs(PGAP$eac_price_usd - PGAP$bp_price_usd)), ]
usd   <- function(x) paste0("$", format(as.numeric(x), big.mark = ",", trim = TRUE))

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

## ---- funnelfig
f  <- funnel$states
lab <- c("in the survey", "answers a script", "links a data file",
         "the file is the voter list")
op <- par(mar = c(3.6, 12.2, 0.8, 3.2), cex = 0.86)
bp <- barplot(rev(f), horiz = TRUE, col = rev(c("#c9d6e3", "#a9bed2", "#c47a4a", "#8c2d19")),
              border = NA, las = 1, names.arg = rev(lab), xlab = "jurisdictions",
              xlim = c(0, max(f) * 1.16))
text(rev(f), bp, paste0("  ", rev(f)), adj = 0, cex = 0.98)
par(op)

## ---- funnel
funnel

## ---- price-spread
data.frame(
  `What the two tables say` = c(
    "Both give one plain price, and it is the same",
    "Both give one plain price, and it is not",
    "The 2026 price is not a single amount"),
  Jurisdictions = c(sum(PCMP$same_price == "yes"),
                    sum(PCMP$same_price == "no"),
                    sum(price$bp_price_is_one_number == "no")),
  check.names = FALSE)

## ---- price-gaps
g <- head(PGAP, 6)
data.frame(State = g$state,
           `EAC, 2020` = usd(g$eac_price_usd),
           `Ballotpedia, 2026` = usd(g$bp_price_usd),
           check.names = FALSE)

## ---- price-prose
p <- price[price$bp_price_is_one_number == "no",
           c("state", "eac_price_usd", "bp_price")]
data.frame(State = p$state,
           `EAC, 2020` = usd(p$eac_price_usd),
           `What the state actually charges` = p$bp_price,
           check.names = FALSE)

## ---- rot
rot

## ---- shape
sh <- shape
sh$of <- NULL
names(sh) <- c("the page...", paste0("states (of ", FV("n_ok"), ")"))
sh

## ---- claims
cl <- claims
cl$bytes <- ifelse(is.na(cl$bytes), "--", n(cl$bytes))
names(cl) <- c("state", "what the link actually is", "is it the voter list?", "bytes")
cl

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
