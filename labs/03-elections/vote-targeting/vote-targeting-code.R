# vote-targeting-code.R -- chunk bodies for vote-targeting-brief.Rmd
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
nc <- read.csv("data/derived/nc_gov_county.csv", stringsAsFactors = FALSE)

sw <- aggregate(cbind(dem, rep, oth, total) ~ year, nc, sum)
sw$rep_pct <- 100 * sw$rep / sw$total
sw$dem_pct <- 100 * sw$dem / sw$total
sw$oth_pct <- 100 * sw$oth / sw$total

tr <- tapply(nc$rep, nc$year, sum); td <- tapply(nc$dem, nc$year, sum)
nc$rs <- nc$rep / tr[as.character(nc$year)]
nc$ds <- nc$dem / td[as.character(nc$year)]
nc$rp <- nc$rep / nc$total

ar <- sort(tapply(nc$rs, nc$county, mean), decreasing = TRUE)   # contribution, R
ad <- sort(tapply(nc$ds, nc$county, mean), decreasing = TRUE)   # contribution, D
bp <- sort(tapply(nc$rp, nc$county, mean), decreasing = TRUE)   # R percentage

TARGET  <- 3200000
tgt     <- ar * TARGET
best    <- max(sw$rep); best_yr <- sw$year[which.max(sw$rep)]
a24     <- nc[nc$year == 2024, ]; rownames(a24) <- a24$county
a20     <- nc[nc$year == 2020, ]; rownames(a20) <- a20$county   # the worked example

# drop 2024 and recompute
sub  <- nc[nc$year != 2024, ]
tr2  <- tapply(sub$rep, sub$year, sum)
sub$s <- sub$rep / tr2[as.character(sub$year)]
ar2  <- tapply(sub$s, sub$county, mean)

half_r <- which(cumsum(ar) >= .5)[1]
half_d <- which(cumsum(ad) >= .5)[1]

# how much each county's assigned share moves when 2024 is dropped, against
# how big the county is: one election moving the whole plan
csize  <- setNames(a24$total, a24$county)
cshift <- data.frame(county = names(ar),
                     with    = 100 * as.numeric(ar),
                     without = 100 * as.numeric(ar2[names(ar)]),
                     size    = as.numeric(csize[names(ar)]),
                     stringsAsFactors = FALSE)
cshift$rel <- 100 * (cshift$without / cshift$with - 1)
n_gain   <- sum(cshift$rel > 0)
med_gain <- median(cshift$size[cshift$rel > 0])
med_lose <- median(cshift$size[cshift$rel <= 0])

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")
tt <- function(cty) gsub("\\b([a-z])", "\\U\\1", tolower(cty), perl = TRUE)

# ---- the colors, for the STATIC twins only --------------------------------
# The D3 figures are drawn with the shared library, whose gop/dem classes
# restyle with the page; base-R devices cannot swap for the dark page, so the
# static twins use the light values of the same pair.
DEMC <- "#2c7fb8"
REPC <- "#C41230"

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)                      # fails_when -> fails when
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)  # sentence case the first letter
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- nc[nc$county == "WAKE" & nc$year == 2020,
        c("year", "county", "dem", "rep", "oth", "total")]
o$county <- tt(o$county)
o[] <- lapply(o, function(x) if (is.numeric(x)) n(x) else x)
names(o) <- c("year", "county", "Democratic", "Republican", "other", "total")
o

## ---- statewide
o <- data.frame(year = sw$year,
                Democratic = n(sw$dem), Republican = n(sw$rep),
                other = n(sw$oth),
                rep_pct = pc(sw$rep_pct), oth_pct = pc(sw$oth_pct, 2))
names(o) <- c("year", "Democratic", "Republican", "other", "Republican %",
              "other %")
o

## ---- concentration
data.frame(
  measure = c("Top 10 counties supply", "Top 25 counties supply",
              "The bottom 50 counties supply",
              "Counties needed to reach half the party's vote"),
  Republican = c(paste0(pc(100 * sum(ar[1:10])), "%"),
                 paste0(pc(100 * sum(ar[1:25])), "%"),
                 paste0(pc(100 * sum(ar[51:100])), "%"), half_r),
  Democratic = c(paste0(pc(100 * sum(ad[1:10])), "%"),
                 paste0(pc(100 * sum(ad[1:25])), "%"),
                 paste0(pc(100 * sum(ad[51:100])), "%"), half_d))

## ---- conc-static
par(mar = c(4.4, 4.6, 1.0, 1.2))
plot(seq_along(ar), 100 * cumsum(ar), type = "l", lwd = 2.6, col = REPC,
     xlab = "counties, ranked by contribution", ylab = "cumulative % of the party's vote",
     ylim = c(0, 100), las = 1)
lines(seq_along(ad), 100 * cumsum(ad), lwd = 2.6, col = DEMC)
abline(h = 50, lty = 3, col = "grey50")
abline(v = c(half_r, half_d), lty = 3, col = "grey50")
legend("bottomright", c("Republican", "Democratic"),
       col = c(REPC, DEMC), lwd = 2.6, bty = "n", cex = 0.85)

## ---- conc-d3
# Two cumulative curves: exactly what the shared library's line type draws,
# so it is drawn with dd_fig(). This is the document's first D3 figure, so
# dd_fig() emits the d3 and dd-charts.js tags here. The two series really are
# the two parties, so they take the gop/dem classes.
cc <- data.frame(rank = seq_along(ar),
                 rep_cum = 100 * cumsum(as.numeric(ar)),
                 dem_cum = 100 * cumsum(as.numeric(ad)))
dd_fig("conc", "line", cc,
  size = list(w = 760, h = 420, m = list(t = 18, r = 24, b = 44, l = 56)),
  x = list(field = "rank", label = "counties, ranked by contribution",
           domain = c(1, 100), fmt = "d", ticks = 10),
  y = list(field = "rep_cum", label = "cumulative % of the party's vote",
           domain = c(0, 100), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "rep_cum", label = "Republican", class = "gop"),
    list(field = "dem_cum", label = "Democratic", class = "dem"))),
  legend = TRUE,
  annotations = list(dd_annot_hline(50)),
  tip = dd_js('function(d){
    return "<b>top "+d.rank+" counties</b><br>"+
      "<span class=\'gop-txt\'>&#9632;</span> Republican: "+d.rep_cum.toFixed(1)+"%<br>"+
      "<span class=\'dem-txt\'>&#9632;</span> Democratic: "+d.dem_cum.toFixed(1)+"%";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the chart to read the value at any N.</p>')

## ---- targets
o <- data.frame(county = tt(names(tgt)[1:8]),
                share = pc(100 * ar[1:8], 2),
                target = n(tgt[1:8]))
names(o) <- c("county", "average share (%)", "votes needed")
o

## ---- gap-static
par(mar = c(3.6, 4.6, 1.4, 1))
b <- barplot(sw$rep / 1e6, names.arg = sw$year, ylim = c(0, 3.5),
             col = "#bdbdbd", border = "white", las = 1,
             ylab = "Republican votes for governor (millions)")
abline(h = TARGET / 1e6, col = REPC, lwd = 2.4, lty = 2)
text(b[1] - 0.4, TARGET / 1e6 + 0.13, paste0("the plan's target: ", n(TARGET)),
     adj = c(0, 0), cex = 0.78, col = REPC)
arrows(b, sw$rep / 1e6, b, TARGET / 1e6, length = 0.05, code = 3,
       col = REPC, lwd = 1.3)
text(b + 0.12, (sw$rep / 1e6 + TARGET / 1e6) / 2,
     paste0(n(TARGET - sw$rep), "\nshort"), cex = 0.68, col = REPC,
     adj = c(0, 0.5))
text(b, sw$rep / 1e6 - 0.16, n(sw$rep), cex = 0.74, col = "#333333")
text(b[which.max(sw$rep)], max(sw$rep) / 1e6 - 0.34, "best of the four",
     cex = 0.7, col = "#333333")

## ---- gap-d3
# Four bars against a threshold line: the shared library's bar type plus an
# hline annotation, so it is drawn with dd_fig().
gb <- data.frame(year = sw$year, rep = sw$rep, short = TARGET - sw$rep)
dd_fig("gap", "bar", gb,
  size = list(w = 760, h = 400, m = list(t = 26, r = 24, b = 40, l = 86)),
  x = list(field = "year"),
  y = list(field = "rep", label = "Republican votes for governor",
           domain = c(0, 3400000), fmt = "comma", ticks = 7),
  series = list(class = "gop"),
  valueLabels = TRUE,
  annotations = list(
    dd_annot_hline(TARGET, class = "gop"),
    dd_annot_text(2012, 3260000, "the plan's target: 3,200,000",
                  class = "gop-txt", size = 11.5)),
  tip = dd_js('function(d){
    var f = DD.fmt.comma;
    return "<b>"+d.year+"</b><br>"+f(d.rep)+" Republican votes<br>"+
      f(d.short)+" short of the target";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a bar for the shortfall.</p>')

## ---- change-target
o <- do.call(rbind, lapply(c(TARGET, sw$dem[sw$year == 2024], best),
  function(T) {
    t <- ar * T
    data.frame(target = n(T),
               a = n(t[names(ar)[1]]), b = n(t[names(ar)[2]]),
               c = n(t["DURHAM"]),
               same_order = identical(names(sort(t, decreasing = TRUE))[1:10],
                                      names(ar)[1:10])) }))
names(o) <- c("statewide target", tt(names(ar)[1]), tt(names(ar)[2]), "Durham",
              "top ten in the same order?")
o

## ---- rankings
data.frame(rank = 1:5,
           by_contribution = tt(names(ar)[1:5]),
           contribution_pct = pc(100 * ar[1:5], 2),
           by_percentage = tt(names(bp)[1:5]),
           republican_pct = pc(100 * bp[1:5]))

## ---- scatter-static
par(mar = c(4.4, 4.6, 1.0, 1.2))
cn <- names(ar)
plot(100 * bp[cn], 100 * ar[cn], pch = 19, cex = 0.8, col = "#777777",
     xlab = "average Republican share of votes cast in the county (%)",
     ylab = "average share of the party's statewide vote (%)",
     las = 1, xlim = c(20, 80))
hi <- cn[100 * ar[cn] > 2 | 100 * bp[cn] > 70]
points(100 * bp[hi], 100 * ar[hi], pch = 19, cex = 0.9, col = REPC)
lb <- cn[100 * ar[cn] > 2]
text(100 * bp[lb], 100 * ar[lb], tt(lb), pos = 4, cex = 0.62, col = "grey25")
tp <- names(bp)[1]
text(100 * bp[tp], 100 * ar[tp], tt(tp), pos = 2, cex = 0.62, col = "grey25")

## ---- scatter-d3
# One point per county on two axes: the shared library's scatter type. Rows
# with a lbl get named on the panel; the friendliest county takes side="left"
# so its label stays inside the frame.
cn <- names(ar)
sd <- data.frame(county = tt(cn),
                 pct = 100 * as.numeric(bp[cn]),
                 share = 100 * as.numeric(ar[cn]),
                 stringsAsFactors = FALSE)
sd$lbl  <- ifelse(sd$share > 2 | cn == names(bp)[1], sd$county, NA)
sd$side <- ifelse(cn == names(bp)[1], "left", NA)
dd_fig("sc", "scatter", sd,
  size = list(w = 760, h = 430, m = list(t = 18, r = 24, b = 48, l = 58)),
  x = list(field = "pct",
           label = "where the party is liked: Republican % of votes cast",
           domain = c(20, 80), fmt = "pct0", ticks = 7),
  y = list(field = "share",
           label = "where the votes are: % of the statewide party vote",
           domain = c(0, 9), fmt = "pct0", ticks = 6),
  series = list(class = "gop"),
  r = 5, opacity = 0.7,
  tip = dd_js('function(d){
    return "<b>"+d.county+"</b><br>"+d.share.toFixed(2)+
      "% of the statewide party vote<br>"+d.pct.toFixed(1)+"% Republican locally";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover any dot for its two shares.</p>')

## ---- drop24
cm <- data.frame(county = names(ar), with = 100 * ar,
                 without = 100 * ar2[names(ar)])
cm$chg <- cm$without - cm$with
o <- rbind(head(cm[order(-cm$chg), ], 4), head(cm[order(cm$chg), ], 4))
o <- data.frame(county = tt(o$county), with_2024 = pc(o$with, 2),
                without_2024 = pc(o$without, 2), change = pc(o$chg, 2))
names(o) <- c("county", "share with 2024 (%)", "share without 2024 (%)",
              "change")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
