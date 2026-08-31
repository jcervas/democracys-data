# follower-counts-code.R -- chunk bodies for follower-counts-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.2,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

cov     <- rd("coverage.csv")
party   <- rd("party_coverage.csv")
leaders <- rd("platform_leaders.csv")
stale   <- rd("stale_handles.csv")
allm    <- rd("all_members.csv")

n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
plat <- c(x = "X", instagram = "Instagram", bluesky = "Bluesky")

knit_print.data.frame <- function(x, ...) {
  nm <- names(x); nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- coverage
c1 <- cov
c1$platform <- plat[c1$platform]
c1 <- c1[, c("platform", "members", "handle_on_file", "count_read",
             "no_handle", "no_account", "other_miss", "pct_covered")]
names(c1) <- c("platform", "members", "handle on file", "count read",
               "no handle known", "no account found", "handle wrong",
               "% of Congress")
c1

## ---- stale
s1 <- head(stale[stale$says_rep_but_sits_in_senate, c("member", "party", "chamber", "handle_on_file")], 6)
names(s1) <- c("member", "party", "now sits in", "handle the roster still lists")
s1

## ---- party
p1 <- party[party$platform == "bluesky", c("party", "members", "covered", "pct")]
names(p1) <- c("party", "members of Congress", "found on Bluesky", "%")
p1

## ---- partyx
p2 <- party[party$platform == "x", c("party", "members", "covered", "pct")]
names(p2) <- c("party", "members of Congress", "found on X", "%")
p2

## ---- leaders
l1 <- leaders
l1$platform <- plat[l1$platform]
names(l1) <- c("platform", "most followed member", "party", "followers", "members covered")
attr(l1, "align") <- "lllrr"
l1

## ---- allmembers
a1 <- allm
names(a1) <- c("state", "chamber", "member", "party", "X", "Instagram", "Bluesky")
attr(a1, "align") <- "lllcrrr"
a1

## ---- checks
ch <- rd("checks.csv")
names(ch) <- c("check", "result")
ch

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
