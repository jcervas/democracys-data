# incumbency-advantage-code.R -- chunk bodies for incumbency-advantage-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
source("../../_lib/dd-charts.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

d        <- read.csv("data/derived/races.csv",              stringsAsFactors = FALSE)
rdd      <- read.csv("data/derived/rdd.csv",                 stringsAsFactors = FALSE)
by_dec   <- read.csv("data/derived/by_decade.csv",           stringsAsFactors = FALSE)
binned   <- read.csv("data/derived/binned.csv",              stringsAsFactors = FALSE)
binned_p <- read.csv("data/derived/binned_personal.csv",     stringsAsFactors = FALSE)
by_yr    <- read.csv("data/derived/by_year.csv",             stringsAsFactors = FALSE)
jval     <- read.csv("data/derived/jacobson_validation.csv", stringsAsFactors = FALSE)
head_t   <- read.csv("data/derived/headline.csv",            stringsAsFactors = FALSE)
raw      <- read.csv("data/raw/1976-2024-house.tab", stringsAsFactors = FALSE)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
hv <- function(q) head_t$value[head_t$quantity == q]

Y1 <- min(d$year); Y2 <- max(d$year)
TOTAL   <- hv("district-year races, 1976-2024 (50 states, general, non-special)")
STD     <- hv("standard contested two-party races")
UNC     <- hv("uncontested races")
NONSTD  <- hv("nonstandard same-party pairs (top-two / jungle primary)")
MN_ROWS <- hv("Minnesota 'Independent-Republican' rows fixed")
MN_PHANTOM <- hv("of those, the only Republican row in their race")
RO_FIX  <- hv("runoff district-years resolved to their final round")

vp   <- d[d$valid_pair, ]
RUN  <- sum(vp$personal_incumbent_running)
NVP  <- nrow(vp)
REEL <- sum(vp$personal_incumbent_reelected[vp$personal_incumbent_running], na.rm = TRUE)

std_vp  <- vp[vp$standard, ]
inc_run <- std_vp[std_vp$personal_incumbent_running %in% TRUE, ]
open_st <- std_vp[std_vp$personal_incumbent_running %in% FALSE, ]
NAIVE_INC  <- mean(abs(inc_run$margin), na.rm = TRUE)
NAIVE_OPEN <- mean(abs(open_st$margin), na.rm = TRUE)
SEL_RUN  <- mean(abs(inc_run$lag_margin), na.rm = TRUE)
SEL_OPEN <- mean(abs(open_st$lag_margin), na.rm = TRUE)

fit_jump <- function(dat, h) {
  loc <- dat[abs(dat$lag_margin) <= h, ]
  f <- lm(dv ~ lag_margin * treat, data = loc)
  s <- summary(f)$coefficients
  c(n = nrow(loc), jump = unname(s["treatTRUE", "Estimate"]),
    se = unname(s["treatTRUE", "Std. Error"]))
}
rdd$treat <- rdd$lag_margin >= 0
P20 <- as.list(fit_jump(rdd, 20))
P10 <- as.list(fit_jump(rdd, 10))
P5  <- as.list(fit_jump(rdd, 5))

rdd_p <- rdd[rdd$personal_incumbent_running %in% TRUE, ]
rdd_o <- rdd[rdd$personal_incumbent_running %in% FALSE, ]
PP20 <- as.list(fit_jump(rdd_p, 20))
PP10 <- as.list(fit_jump(rdd_p, 10))
PP5  <- as.list(fit_jump(rdd_p, 5))
OP10 <- as.list(fit_jump(rdd_o, 10))

DEC1 <- by_dec$jump[1]; DECN <- by_dec$jump[nrow(by_dec)]
DECY1 <- by_dec$decade[1]; DECYN <- by_dec$decade[nrow(by_dec)]

yr_real <- by_yr[!is.na(by_yr$jump), ]
YFIRST <- min(yr_real$year); YLAST <- max(yr_real$year)
JAC_LAST    <- max(by_yr$year[by_yr$source == "Jacobson (JOP replication)"])
MEDSL_FIRST <- min(by_yr$year[by_yr$source == "This chapter (MEDSL returns)"])
PEAK    <- yr_real[which.max(yr_real$jump), ]
LOWPT   <- yr_real[yr_real$year == YFIRST, ]
LATEST  <- yr_real[yr_real$year == YLAST, ]
N_GAPS  <- sum(by_yr$redistricting_gap)
AGREE_PCT <- jval$agreement_pct[1]
AGREE_N   <- jval$races_compared[1]
AGREE_YRS <- jval$overlap_years[1]

# render every data.frame as a table, not a "##"-prefixed code block
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- year-static
par(mar = c(3.4, 4.4, 0.8, 0.8), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(by_yr$year), ylim = c(-2, 14), las = 1,
     xlab = "", ylab = "estimated incumbency advantage (points of the two-party vote)")
abline(h = 0, lty = 3, col = "#999999")
lines(by_yr$year[by_yr$source == "Jacobson (JOP replication)"],
      by_yr$jump_jacobson[by_yr$source == "Jacobson (JOP replication)"],
      col = "#6a51a3", lwd = 2.4)
lines(by_yr$year[by_yr$source == "This chapter (MEDSL returns)"],
      by_yr$jump_medsl[by_yr$source == "This chapter (MEDSL returns)"],
      col = "#d95f02", lwd = 2.4)
legend("topleft", c("Jacobson's own data, 1946-2014", "this chapter's construction, 2016-2024"),
       col = c("#6a51a3", "#d95f02"), lwd = 2.4, bty = "n", cex = 0.72)

## ---- year-d3
m <- by_yr[, c("year", "jump_jacobson", "jump_medsl", "n")]
dd_fig("yr", "line", m,
  size = list(w = 780, h = 430, m = list(t = 16, r = 24, b = 34, l = 56)),
  x = list(field = "year", fmt = "d", ticks = 10),
  y = list(field = "jump_jacobson",
           label = "estimated incumbency advantage (points of the two-party vote)",
           domain = c(-2, 14), fmt = "d", ticks = 8),
  series = list(fields = list(
    list(field = "jump_jacobson", label = "Jacobson's own data, 1946-2014",
         class = "series-4", width = 2.4),
    list(field = "jump_medsl", label = "this chapter's construction, 2016-2024",
         class = "series-1", width = 2.4))),
  points = 3.4,
  legend = TRUE,
  annotations = list(dd_annot_hline(0)),
  tip = dd_js('function(d){
    var v = d.jump_jacobson!=null ? d.jump_jacobson : d.jump_medsl;
    if (v==null) return "<b>"+d.year+"</b><br>no estimate -- lines redrawn this year";
    return "<b>"+d.year+"</b><br>"+v.toFixed(1)+" points, n="+d.n;
  }'))
cat(sprintf('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The %d breaks mark the years when nearly every district in the country got new
lines: no valid same-district comparison exists there at all.</p>', N_GAPS))

## ---- panetta-example
pan <- raw[raw$year %in% c(1992, 1994) & raw$state == "CALIFORNIA" & raw$district == 17 &
           raw$stage == "GEN" & !raw$writein, c("year", "candidate", "party", "candidatevotes")]
pan <- pan[order(pan$year, -pan$candidatevotes), ]
names(pan) <- c("year", "candidate", "party", "votes")
pan

## ---- fusion-example
fu <- raw[raw$year == 1976 & raw$state == "NEW YORK" & raw$district == 1 &
          raw$stage == "GEN", c("candidate", "party", "candidatevotes")]
fu <- fu[order(-fu$candidatevotes), ]
tot <- aggregate(candidatevotes ~ candidate, data = fu, sum)
names(tot) <- c("candidate", "combined total")
o <- merge(fu, tot, by = "candidate")
o <- o[order(-o$`combined total`, o$party), ]
names(o) <- c("candidate", "party line", "votes on this line", "combined total")
o

## ---- mn-trap
mn <- raw[raw$year == 1992 & raw$state == "MINNESOTA" & raw$stage == "GEN" &
          !raw$writein, c("district", "candidate", "party", "candidatevotes")]
mn <- mn[order(mn$district, -mn$candidatevotes), ]
names(mn) <- c("district", "candidate", "party (as recorded)", "votes")
mn

## ---- runoff-trap
tx <- raw[raw$year == 1996 & raw$state == "TEXAS" & raw$district == 8 &
          raw$stage == "GEN", c("runoff", "candidate", "party", "candidatevotes")]
tx <- tx[order(tx$candidate, tx$runoff), ]
names(tx) <- c("runoff round?", "candidate", "party", "votes")
tx

## ---- headline
o <- head_t
names(o) <- c("quantity", "count")
o

## ---- rdd-d3
rows <- binned
dd_fig("rdd", "scatter", rows,
  size = list(w = 770, h = 440, m = list(t = 18, r = 24, b = 46, l = 56)),
  x = list(field = "bin", label = "Democratic margin in the SAME district, two years earlier",
           domain = c(-21, 21), fmt = "signed0", ticks = 8),
  y = list(field = "dv", label = "Democratic % of the two-party vote, this election",
           domain = c(25, 75), fmt = "d", ticks = 6),
  series = list(field = "side", classes = list(below = "gop", above = "dem")),
  r = 5.5,
  annotations = list(dd_annot_vline(0), dd_annot_hline(50)),
  tip = dd_js('function(d){
    return "<b>prior margin "+(d.bin>0?"+":"")+d.bin+"</b><br>"+
      "this election: "+d.dv.toFixed(1)+"% Democratic<br>"+
      "n = "+d.n;
  }'))

## ---- rdd-static
par(mar = c(4.0, 4.4, 0.8, 0.8), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(-21, 21), ylim = c(25, 75), las = 1,
     xlab = "Democratic margin in the same district, two years earlier",
     ylab = "Dem % of the two-party vote, this election")
abline(v = 0, lty = 2, col = "#666666"); abline(h = 50, lty = 3, col = "#999999")
below <- binned[binned$side == "below", ]; above <- binned[binned$side == "above", ]
points(below$bin, below$dv, pch = 16, col = "#C41230", cex = 1.1)
points(above$bin, above$dv, pch = 16, col = "#2c7fb8", cex = 1.1)
legend("topleft", c("Republicans barely held it last time", "Democrats barely held it last time"),
       col = c("#C41230", "#2c7fb8"), pch = 16, bty = "n", cex = 0.75)

## ---- rdd-personal-d3
rows <- binned_p
dd_fig("rddp", "scatter", rows,
  size = list(w = 770, h = 440, m = list(t = 18, r = 24, b = 46, l = 56)),
  x = list(field = "bin", label = "Democratic margin in the SAME district, two years earlier",
           domain = c(-21, 21), fmt = "signed0", ticks = 8),
  y = list(field = "dv", label = "Democratic % of the two-party vote, this election",
           domain = c(25, 75), fmt = "d", ticks = 6),
  series = list(field = "side", classes = list(below = "gop", above = "dem")),
  r = 5.5,
  annotations = list(dd_annot_vline(0), dd_annot_hline(50)),
  tip = dd_js('function(d){
    return "<b>prior margin "+(d.bin>0?"+":"")+d.bin+"</b><br>"+
      "this election: "+d.dv.toFixed(1)+"% Democratic<br>"+
      "n = "+d.n;
  }'))

## ---- rdd-personal-static
par(mar = c(4.0, 4.4, 0.8, 0.8), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(-21, 21), ylim = c(25, 75), las = 1,
     xlab = "Democratic margin in the same district, two years earlier",
     ylab = "Dem % of the two-party vote, this election")
abline(v = 0, lty = 2, col = "#666666"); abline(h = 50, lty = 3, col = "#999999")
below_p <- binned_p[binned_p$side == "below", ]; above_p <- binned_p[binned_p$side == "above", ]
points(below_p$bin, below_p$dv, pch = 16, col = "#C41230", cex = 1.1)
points(above_p$bin, above_p$dv, pch = 16, col = "#2c7fb8", cex = 1.1)
legend("topleft", c("Republican incumbent running again", "Democratic incumbent running again"),
       col = c("#C41230", "#2c7fb8"), pch = 16, bty = "n", cex = 0.75)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
