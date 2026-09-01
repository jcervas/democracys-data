# whole-foods-cracker-barrel-code.R -- chunk bodies for whole-foods-cracker-barrel-brief.Rmd
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
                      fig.width = 7.2, fig.height = 4.4,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f, ...) read.csv(file.path("data/derived", f),
                                   stringsAsFactors = FALSE, ...)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

gap   <- rd("gap.csv")
cats  <- rd("categories.csv")
alt   <- rd("alternatives.csv")
p20   <- rd("published_2020.csv")
anach <- rd("anachronism.csv")

n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# --- palette ---------------------------------------------------------------
# The D3 figures take their colours from the shared chart library's classes,
# so nothing here is a hex for the screen. These four are for the print
# twins only, and they are chosen to sit near the library's series colours so
# the printed page and the screen page are recognisably the same figure.
CNTC <- "#1C4C5C"   # anything measured in counties
VOTC <- "#C0625A"   # the same thing measured in votes
NONE <- "#3B3B3B"   # the category the metric leaves out
GRY  <- "#8A8F94"

.hdr <- function(x) sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)

knit_print.data.frame <- function(x, ...) {
  knitr::knit_print(knitr::kable(x, col.names = .hdr(x), row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

nobreak <- function(x) {
  if (knitr::is_latex_output())
    knitr::kable(x, format = "latex", booktabs = TRUE, longtable = FALSE,
                 linesep = "", col.names = .hdr(x), row.names = FALSE,
                 align = table_align(x))
  else x
}

g24 <- gap[gap$year == 2024, ]
g08 <- gap[gap$year == 2008, ]

# the four store categories, in the order the figures and tables read them
ORD <- c("whole foods only", "both", "cracker barrel only", "neither")
CIX <- match(ORD, cats$category)

# values quoted inline, named so the prose reads as prose
VS <- function(k) cats$vote_share[cats$category == k]
CS <- function(k) cats$county_share[cats$category == k]
DC <- function(k) cats$dem_counties_pct[cats$category == k]
MISSING_CB <- as.integer(FN("cb_10k_stores") - FN("n_cb_osm"))
EXCL_SHARE <- VS("whole foods only") + VS("cracker barrel only")
COVERED    <- 100 - as.numeric(FV("neither_county_pct"))

## ---- replicate
o <- data.frame(
  category = p20$category,
  counties = p20$counties,
  published = paste0(pc(p20$published, 0), "%"),
  recomputed = paste0(pc(p20$recomputed, 1), "%"),
  difference = sprintf("%+.1f", p20$drift))
names(o) <- c("counties with", "how many", "Wasserman published",
              "this file recomputes", "difference")
o

## ---- headline
o <- data.frame(
  unit = c("counties carried", "votes cast"),
  wf = c(paste0(pc(g24$wf_counties, 1), "%"), paste0(pc(g24$wf_votes, 1), "%")),
  cb = c(paste0(pc(g24$cb_counties, 1), "%"), paste0(pc(g24$cb_votes, 1), "%")),
  gap = c(paste0(pc(g24$gap_counties, 1), " points"),
          paste0(pc(g24$gap_votes, 1), " points")))
names(o) <- c("2024, measured in", "Whole Foods counties",
              "Cracker Barrel counties", "gap")
nobreak(o)

## ---- gapfig-static
g <- gap[order(gap$year), ]
par(mar = c(3.6, 4.6, 1.4, 8.6), xpd = NA, cex = 0.88)
plot(NA, xlim = range(g$year), ylim = c(0, 60), axes = FALSE,
     xlab = "", ylab = "gap, percentage points")
axis(1, at = g$year, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
grid(nx = NA, ny = NULL, col = "#EEEEEE", lty = 1)
for (s in list(list("gap_counties", CNTC, "measured in counties carried"),
               list("gap_votes",    VOTC, "measured in votes cast"))) {
  v <- g[[s[[1]]]]
  lines(g$year, v, col = s[[2]], lwd = 2.6)
  points(g$year, v, pch = 19, col = s[[2]])
  text(max(g$year) + 0.5, v[length(v)], s[[3]], adj = 0, cex = 0.7,
       col = s[[2]])
}
par(xpd = FALSE)

## ---- gapfig-d3
# The finding, drawn with the shared chart library. Two readings of ONE
# metric over five elections, so a line with two series is the form: the
# reader follows each reading across time and reads the distance between
# them off a single axis. Not two panels -- putting both on one scale is the
# whole point, because the argument is that the two numbers are far apart.
m <- gap[order(gap$year),
         c("year", "gap_counties", "gap_votes", "wf_counties", "cb_counties",
           "wf_votes", "cb_votes")]
dd_fig("wfgap", "line", m,
  size = list(w = 770, h = 400, m = list(t = 20, r = 210, b = 42, l = 58)),
  # ticks = 9 over 2008-2024 is a tick every two years, so every election year
  # this figure draws carries its own label; a smaller count lands d3 on
  # 2010/2015/2020, which are not years this chart has data for
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "gap_counties", label = "gap, percentage points",
           domain = c(0, 60), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "gap_counties", label = "measured in counties carried",
         class = "series-1"),
    list(field = "gap_votes", label = "measured in votes cast",
         class = "series-2"))),
  points = TRUE, legend = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "counties carried: "+d.wf_counties.toFixed(1)+"% vs "+
        d.cb_counties.toFixed(1)+"% \\u2014 gap "+d.gap_counties.toFixed(1)+
        "<br>"+
      "votes cast: "+d.wf_votes.toFixed(1)+"% vs "+
        d.cb_votes.toFixed(1)+"% \\u2014 gap "+d.gap_votes.toFixed(1);
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the figure for both readings of any year.</p>')

## ---- cats
o <- data.frame(
  category = cats$category,
  counties = n(cats$counties),
  county_pct = paste0(pc(cats$county_share, 1), "%"),
  dem_counties = paste0(pc(cats$dem_counties_pct, 1), "%"),
  vote_pct = paste0(pc(cats$vote_share, 1), "%"),
  dem_votes = paste0(pc(cats$dem_votes_pct, 1), "%"))
names(o) <- c("counties with", "how many", "share of all counties",
              "D carried", "share of all votes", "D share of votes")
o

## ---- catfig-static
d <- cats[CIX, ]
par(mar = c(4.2, 9.6, 1.2, 1.4), cex = 0.88)
yy <- seq_len(nrow(d))
plot(NA, xlim = c(0, 90), ylim = c(nrow(d) + 0.5, 0.5), axes = FALSE,
     xlab = "% of the national total", ylab = "")
axis(1, cex.axis = 0.8); axis(2, at = yy, labels = d$category, las = 1,
                              tick = FALSE, cex.axis = 0.8)
abline(v = seq(0, 90, 15), col = "#EEEEEE")
segments(d$county_share, yy, d$vote_share, yy, col = GRY, lwd = 2)
points(d$county_share, yy, pch = 19, col = CNTC, cex = 1.2)
points(d$vote_share,   yy, pch = 19, col = VOTC, cex = 1.2)
legend("bottomright", bty = "n", cex = 0.7, pch = 19,
       col = c(CNTC, VOTC),
       legend = c("share of all counties", "share of all votes"))

## ---- catfig-d3
# A dumbbell, because the question is asked of each category separately and
# the answer is a DISTANCE: how far a category's share of counties sits from
# its share of votes. A stacked bar would put those two numbers on different
# rows and make the reader measure the gap by eye.
d <- data.frame(
  category = ORD,
  county_share = round(cats$county_share[CIX], 1),
  vote_share   = round(cats$vote_share[CIX], 1),
  counties     = cats$counties[CIX],
  dem_counties_pct = round(cats$dem_counties_pct[CIX], 1),
  dem_votes_pct    = round(cats$dem_votes_pct[CIX], 1),
  stringsAsFactors = FALSE)
dd_fig("wfcats", "dumbbell", d, rowHeight = 40,
  size = list(w = 770, m = list(t = 26, r = 34, b = 46, l = 150)),
  y = list(field = "category"),
  a = list(field = "county_share", label = "share of all counties"),
  b = list(field = "vote_share",   label = "share of all votes"),
  aClass = "series-1", bClass = "series-2", r = 5,
  x = list(fmt = "pct0", zero = TRUE, label = "% of the national total"),
  tip = dd_tip(c(counties = "counties", county_share = "share of counties",
                 vote_share = "share of votes",
                 dem_counties_pct = "D carried",
                 dem_votes_pct = "D share of their votes"),
               fmt = c(counties = "comma", county_share = "pct1",
                       vote_share = "pct1", dem_counties_pct = "pct1",
                       dem_votes_pct = "pct1"),
               title = "category"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a pair of dots for the counts behind it.</p>')

## ---- alts
o <- data.frame(
  rule = alt$rule,
  dc = paste0(pc(alt$dem_counties_pct, 1), "%"),
  dv = paste0(pc(alt$dem_votes_pct, 1), "%"),
  ov = paste0(pc(alt$overlap_wf_pct, 0), "%"))
names(o) <- c(paste("the", n(FV("n_wf_counties")), "counties picked by"),
              "D carried", "D share of their votes",
              "that also have a Whole Foods")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
