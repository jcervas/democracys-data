# follower-counts-code.R -- chunk bodies for follower-counts-brief.Rmd
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

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("all_members.csv", "checks.csv", "coverage.csv", "facts.csv", "party_coverage.csv", "platform_leaders.csv", "stale_handles.csv"))

n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
plat <- c(x = "X", instagram = "Instagram", bluesky = "Bluesky")

# ---- the one exact distribution in the file --------------------------------
# Instagram is the only platform that gives a whole number for a large share of
# Congress: X rounds ("11.5M") and Bluesky covers a quarter of the members. So
# every statement about the SHAPE of the distribution is made on the Instagram
# column, and says so.
allm$ig <- suppressWarnings(as.numeric(gsub(",", "", allm$instagram)))
ig  <- allm[!is.na(allm$ig), ]
igo <- ig[order(-ig$ig), ]

IG_N    <- nrow(ig)
IG_MED  <- median(ig$ig)
IG_MEAN <- mean(ig$ig)
IG_TOT  <- sum(ig$ig)
TOP10   <- 100 * sum(igo$ig[1:10]) / IG_TOT
BOT50   <- 100 * sum(igo$ig[(floor(IG_N / 2) + 1):IG_N]) / IG_TOT
RATIO   <- igo$ig[1] / IG_MED
IG_GINI <- local({ x <- sort(ig$ig); k <- length(x)
                   sum((2 * seq_len(k) - k - 1) * x) / (k * sum(x)) })
MED_SEN <- median(ig$ig[ig$chamber == "Senate"])
MED_HSE <- median(ig$ig[ig$chamber == "House"])
SEN_25  <- sum(igo$chamber[1:25] == "Senate")
TOPNAME <- igo$member[1]

# one member with a count on all three platforms, for the prose to read the
# three numbers off as the platforms printed them (X's is a rounded string)
EX <- allm[allm$member == "Schiff, Adam", ][1, ]

knit_print.data.frame <- function(x, ...) {
  nm <- names(x); nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# ---- the figure's data, built once so both versions draw the same thing -----
# Twelve named members, then one more row for the middle of the same column.
# The median row is the whole argument of the figure: at a scale that fits the
# top of Congress, the typical member is a line.
figd <- igo[1:12, c("member", "party", "chamber", "ig")]
figd <- rbind(figd, data.frame(
  member = paste0("median of the ", IG_N), party = "—",
  chamber = "the middle of the column", ig = IG_MED))
names(figd) <- c("member", "party", "chamber", "followers")
FIG_MAX <- max(figd$followers) * 1.22

## ---- dist-static
op <- par(mar = c(3.6, 10.6, 0.6, 2.2), mgp = c(2.3, 0.6, 0))
d  <- figd[rev(seq_len(nrow(figd))), ]
COLS <- c(House = "#2c7fb8", Senate = "#54278F")
cl <- ifelse(d$chamber %in% names(COLS), COLS[d$chamber], "#9aa0a6")
bp <- barplot(d$followers, horiz = TRUE, col = cl, border = NA, axes = FALSE,
              names.arg = rep("", nrow(d)), xlim = c(0, FIG_MAX))
axis(1, at = seq(0, 3e6, 1e6), labels = c("0", "1m", "2m", "3m"),
     col = "grey70", cex.axis = 0.75)
mtext("Instagram followers", side = 1, line = 2.1, cex = 0.78, col = "grey30")
text(par("usr")[1] - FIG_MAX * 0.015, bp, d$member, xpd = NA, adj = 1,
     cex = 0.68, col = "grey20")
text(d$followers + FIG_MAX * 0.012, bp, format(d$followers, big.mark = ","),
     adj = 0, cex = 0.62, col = "grey35", xpd = NA)
legend("bottomright", c("House", "Senate", "the middle of the column"),
       fill = c(COLS[["House"]], COLS[["Senate"]], "#9aa0a6"), border = NA,
       bty = "n", cex = 0.66)
par(op)

## ---- dist-d3
dd_fig("dist", "bar", figd,
  size = list(w = 760, m = list(t = 26, r = 78, b = 8, l = 178)),
  rowHeight = 26,
  x = list(field = "followers", domain = c(0, FIG_MAX), fmt = "comma",
           ticks = 4),
  y = list(field = "member", band = TRUE),
  series = list(field = "chamber",
                classes = list(House = "series-1", Senate = "series-2",
                               `the middle of the column` = "series-8")),
  valueLabels = TRUE, legend = TRUE,
  tip = dd_tip(c(chamber = "chamber", party = "party",
                 followers = "Instagram followers"),
               fmt = c(followers = "comma"), title = "member"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a bar for the exact count.</p>')

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
a1 <- allm[, c("state", "chamber", "member", "party", "x", "instagram", "bluesky")]
names(a1) <- c("state", "chamber", "member", "party", "X", "Instagram", "Bluesky")
attr(a1, "align") <- "lllcrrr"
a1

## ---- checks
ch <- rd("checks.csv")
names(ch) <- c("check", "result")
ch

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
