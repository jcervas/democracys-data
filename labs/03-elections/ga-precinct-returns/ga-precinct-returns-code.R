# ga-precinct-returns-code.R -- chunk bodies for ga-precinct-returns-brief.Rmd
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
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

p  <- read.csv("data/derived/precincts.csv", stringsAsFactors = FALSE, check.names = FALSE)
co <- read.csv("data/derived/counties.csv",  stringsAsFactors = FALSE, check.names = FALSE)
sx <- read.csv("data/derived/structure.csv", stringsAsFactors = FALSE)
rc <- read.csv("data/derived/recount.csv",   stringsAsFactors = FALSE)
vm <- read.csv("data/derived/ga2020_vote_methods.csv", stringsAsFactors = FALSE)

DEM <- "Joseph R. Biden"; REP <- "Donald J. Trump"; LIB <- "Jo Jorgensen"
NPREC <- nrow(p); NCO <- nrow(co)
TOTD <- sum(p[[DEM]]); TOTR <- sum(p[[REP]]); TOTL <- sum(p[[LIB]])
TOTV <- sum(p$total)
MARGIN <- TOTD - TOTR

# vote methods
mm <- tapply(vm$votes, list(vm$candidate, vm$method), sum)
meth <- colnames(mm)
short <- sub(" Votes$", "", meth)
mdem <- mm[DEM, ]; mrep <- mm[REP, ]
mshare <- 100 * mdem / (mdem + mrep)
mtot <- colSums(mm)
ED <- grep("Election Day", meth, value = TRUE)
MB <- grep("Absentee by Mail", meth, value = TRUE)
AV <- grep("Advanced", meth, value = TRUE)

# recount
tot <- aggregate(cbind(original, recount) ~ candidate, rc, sum)
tot$change <- tot$recount - tot$original
RCD <- tot$recount[tot$candidate == DEM]; RCR <- tot$recount[tot$candidate == REP]
RCMARGIN <- RCD - RCR
CHANGED <- length(unique(rc$county[rc$change != 0]))
NCTY <- length(unique(rc$county))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- vintage-setup
s24 <- read.csv("data/derived/ga2024_structure.csv", stringsAsFactors = FALSE)

# Every table this chapter writes is handed over at the head of "The data";
# dd_derived() stops the build if one appears in derived/ without a line there.
dd_derived(c("assign_point.csv", "block_assign.csv", "counties.csv", "crosswalk.csv", "crosswalk_check.csv", "crosswalk_compare.csv", "crosswalk_pop.csv", "fig_forsyth.csv", "fig_forsyth_lab.csv", "fig_wind_blocks.csv", "fig_wind_meta.csv", "fig_wind_outline.csv", "fig_wind_pts.csv", "ga2020_counties.csv", "ga2020_precincts.csv", "ga2020_structure.csv", "ga2020_vote_methods.csv", "ga2020rc_counties.csv", "ga2020rc_precincts.csv", "ga2020rc_structure.csv", "ga2020rc_vote_methods.csv", "ga2024_counties.csv", "ga2024_precincts.csv", "ga2024_structure.csv", "ga2024_vote_methods.csv", "precincts.csv", "precincts_2024_est.csv", "precincts_2024_pop.csv", "recount.csv", "structure.csv"))
g <- function(d, k) d$value[d$item == k]

## ---- vintages
data.frame(
  the_same_thing = c("the county", "the office", "how a ballot was cast",
                     "the candidate", "registration per precinct"),
  in_2020 = c("Baker", g(sx, "contest"), "Election Day Votes",
              "Donald J. Trump (I) (Rep)", "reported"),
  in_2024 = c("Baker County", g(s24, "contest"), "Election Day",
              "Donald J. Trump (Rep)", "absent"))

## ---- one-row
o <- head(p[p$county == "Appling", ], 3)
o <- o[, c("county", "precinct", "registered", "ballots_cast", DEM, REP,
           "total", "dem_two_party_pct")]
o$registered <- n(o$registered); o$ballots_cast <- n(o$ballots_cast)
o[[DEM]] <- n(o[[DEM]]); o[[REP]] <- n(o[[REP]]); o$total <- n(o$total)
names(o) <- c("county", "precinct", "registered", "ballots cast",
              "Biden", "Trump", "presidential votes", "Dem two-party %")
o

## ---- structure
o <- sx
names(o) <- c("what the source contains", "value")
o

## ---- totals
data.frame(
  candidate = c(DEM, REP, LIB, "Total presidential votes",
                "Two-party margin"),
  votes = c(n(TOTD), n(TOTR), n(TOTL), n(TOTV), n(MARGIN)))

## ---- methods
o <- data.frame(method = short,
                votes = n(as.vector(mtot)),
                share = paste0(pc(100 * as.vector(mtot) / sum(mtot)), "%"),
                stringsAsFactors = FALSE)
names(o) <- c("how the ballot was cast", "votes", "% of all votes")
o[order(-mtot), ]

## ---- method-split
o <- data.frame(method = short,
                dem = n(as.vector(mdem)), rep = n(as.vector(mrep)),
                share = paste0(pc(as.vector(mshare)), "%"),
                stringsAsFactors = FALSE)
names(o) <- c("how the ballot was cast", sub(" .*", "", DEM),
              sub(" .*", "", REP), "Dem two-party %")
o[order(-mtot), ]

## ---- d3-methods
# The method split, drawn with the shared library: one dumbbell per vote
# method, ordered by channel size — Biden's dot against Trump's on a single
# vote scale, so channel size and channel disagreement read off one mark.
ord <- order(-mtot)
d <- data.frame(method = short[ord],
                dem = as.vector(mdem[ord]), rep = as.vector(mrep[ord]),
                total = as.vector(mtot[ord]),
                share = round(as.vector(mshare[ord]), 1),
                stringsAsFactors = FALSE)
dd_fig("gam", "dumbbell", d,
  size = list(w = 760, m = list(t = 28, r = 30, b = 44, l = 130)),
  rowHeight = 52,
  y = list(field = "method"),
  a = list(field = "dem", label = "Biden"),
  b = list(field = "rep", label = "Trump"),
  aClass = "dem", bClass = "gop",
  x = list(fmt = "comma", label = "votes", zero = TRUE),
  tip = dd_tip(c(dem = "Biden", rep = "Trump",
                 total = "ballots in this channel",
                 share = "Dem two-party share"),
               fmt = c(dem = "comma", rep = "comma", total = "comma",
                       share = "pct1"),
               title = "method"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a pair of dots for the exact counts and the two-party split.</p>')

## ---- methods-static
ord <- order(-mtot)
par(mar = c(4, 9, 1, 2))
barplot(rbind(rev(mdem[ord]), rev(mrep[ord])) / 1000, beside = TRUE,
        horiz = TRUE, names.arg = rev(short[ord]), las = 1, cex.names = 0.8,
        col = c("#2166AC", "#B2182B"), xlab = "votes (thousands)")
legend("bottomright", c("Biden", "Trump"), fill = c("#2166AC", "#B2182B"),
       bty = "n", cex = 0.85)

## ---- recount-tot
o <- tot
o$original <- n(o$original); o$recount <- n(o$recount)
o$change <- ifelse(tot$change > 0, paste0("+", n(tot$change)), n(tot$change))
names(o) <- c("candidate", "original count", "recount", "change")
o

## ---- recount-summary
data.frame(
  quantity = c("Counties in the comparison", "Counties where the count changed",
               "Share of counties", "Total votes moved (absolute)",
               "Margin, original count", "Margin, recount",
               "Change in the margin"),
  value = c(NCTY, CHANGED, paste0(pc(100 * CHANGED / NCTY), "%"),
            n(sum(abs(rc$change))), n(MARGIN), n(RCMARGIN),
            n(RCMARGIN - MARGIN)))

## ---- recount-detail
o <- head(rc[order(-abs(rc$change)), ], 6)
o$original <- n(o$original); o$recount <- n(o$recount)
o$change <- ifelse(o$change > 0, paste0("+", n(o$change)), n(o$change))
o <- o[, c("county", "candidate", "original", "recount", "change", "pct_change")]
names(o) <- c("county", "candidate", "original", "recount", "change", "% change")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
