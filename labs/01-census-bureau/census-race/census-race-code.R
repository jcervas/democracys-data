# census-race-code.R -- chunk bodies for census-race-brief.Rmd
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

d <- read.csv("data/derived/pl94171_counties.csv", stringsAsFactors = FALSE,
              colClasses = c(fips = "character"))

st <- aggregate(cbind(total, white, nh_white, hispanic, other_race, nh_other,
                      two_or_more, nh_two) ~ state, data = d, FUN = sum)
st$white_pct   <- round(100 * st$white       / st$total, 1)
st$nhwhite_pct <- round(100 * st$nh_white    / st$total, 1)
st$gap         <- round(st$white_pct - st$nhwhite_pct, 1)
st$hisp_pct    <- round(100 * st$hispanic    / st$total, 1)
st$other_pct   <- round(100 * st$other_race  / st$total, 1)
st$two_pct     <- round(100 * st$two_or_more / st$total, 1)
# how Hispanic is each of the two residual categories, state by state?
st$other_h     <- 100 * (st$other_race  - st$nh_other) / st$other_race
st$two_h       <- 100 * (st$two_or_more - st$nh_two)   / st$two_or_more
S <- function(s, v) st[[v]][st$state == s]

tot_other  <- sum(d$other_race); nh_other <- sum(d$nh_other)
hisp_other <- tot_other - nh_other
pct_other_h <- 100 * hisp_other / tot_other

tot_two  <- sum(d$two_or_more); nh_two <- sum(d$nh_two)
hisp_two <- tot_two - nh_two
pct_two_h <- 100 * hisp_two / tot_two

d$hisp_pct  <- 100 * d$hispanic    / d$total
d$two_pct   <- 100 * d$two_or_more / d$total
d$other_pct <- 100 * d$other_race  / d$total
cor_two   <- cor(d$hisp_pct, d$two_pct)
cor_other <- cor(d$hisp_pct, d$other_pct)
cor_gap   <- cor(st$hisp_pct, st$gap)

top_two <- head(d[order(-d$two_pct), ], 10)
hi_two  <- d[d$state == "Hawaii", ]
hi_top  <- hi_two[which.max(hi_two$two_pct), ]

p1ok <- all(d$one_race + d$two_or_more == d$total)
p2ok <- all(d$hispanic + d$not_hispanic == d$total)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# ---- the alluvial the two sankey chunks share -------------------------------
# every race answer, split by the ethnicity answer
rc <- data.frame(
  race = c("White alone", "Black alone", "American Indian alone", "Asian alone",
           "Pacific Islander alone", "Some Other Race alone", "Two or more races"),
  tot  = c(sum(d$white), sum(d$black), sum(d$aian), sum(d$asian), sum(d$nhpi),
           sum(d$other_race), sum(d$two_or_more)),
  nonh = c(sum(d$nh_white), sum(d$nh_black), sum(d$nh_aian), sum(d$nh_asian),
           sum(d$nh_nhpi), sum(d$nh_other), sum(d$nh_two)),
  stringsAsFactors = FALSE)
rc$hisp  <- rc$tot - rc$nonh
rc$share <- 100 * rc$hisp / rc$tot
rc <- rc[order(-rc$share), ]
rc$fill <- ifelse(rc$race == "Some Other Race alone", "#C41230",
           ifelse(rc$race == "Two or more races", "#e08214", "#999999"))
gap  <- 0.012 * sum(rc$tot)
rc$y0 <- head(cumsum(c(0, rc$tot + gap)), -1); rc$y1 <- rc$y0 + rc$tot
hgt  <- max(rc$y1)
hsum <- sum(rc$hisp); nsum <- sum(rc$nonh)
rgap <- (hgt - hsum - nsum) / 2
rt   <- data.frame(name = c("Hispanic or Latino", "Not Hispanic or Latino"),
                   tot  = c(hsum, nsum),
                   y0   = c(0, hsum + rgap),
                   fill = c("#8856a7", "#999999"), stringsAsFactors = FALSE)
rt$y1 <- rt$y0 + rt$tot
rc$hy <- head(cumsum(c(rt$y0[1], rc$hisp)), -1)
rc$ny <- head(cumsum(c(rt$y0[2], rc$nonh)), -1)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- file
data.frame(
  item = c("Source", "Reference date", "Counties", "States", "People covered",
           "Share of the U.S. population", "Tables kept"),
  value = c("2020 Census Redistricting Data (P.L. 94-171) Summary File",
            "1 April 2020", nrow(d), length(unique(d$state)),
            n(sum(d$total)),
            paste0(pc(100 * sum(d$total) / 331449281, 0), "%"),
            "P1 (race), P2 (Hispanic origin by race), P5 (group quarters)"))

## ---- checks
data.frame(
  test = c("one race + two or more races = total, in every county",
           "Hispanic + not Hispanic = total, in every county"),
  holds = c(ifelse(p1ok, "TRUE", "FALSE"), ifelse(p2ok, "TRUE", "FALSE")),
  counties_tested = c(nrow(d), nrow(d)))

## ---- two-static
s <- st[order(st$white_pct), ]
yy <- seq_len(nrow(s))
par(mar = c(4.2, 8.6, 1.2, 2))
plot(NA, xlim = c(0, 80), ylim = c(0.6, nrow(s) + 0.4), yaxt = "n", bty = "n",
     xlab = "% of the state's population", ylab = "")
abline(v = seq(0, 80, 20), col = "#eeeeee")
segments(s$nhwhite_pct, yy, s$white_pct, yy, col = "#bbbbbb", lwd = 2.5)
points(s$nhwhite_pct, yy, pch = 19, col = "#2166AC", cex = 1.2)
points(s$white_pct, yy, pch = 19, col = "#92C5DE", cex = 1.2)
axis(2, at = yy, labels = s$state, las = 1, tick = FALSE, cex.axis = 0.9)
legend("bottomright",
       c("non-Hispanic white (both questions)",
         "white by the race question alone"),
       col = c("#2166AC", "#92C5DE"), pch = 19, bty = "n", cex = 0.8)

## ---- two-d3
# Drawn with the shared library: one row per state, two readings of the same
# question joined by a rule. dd_libs() emits d3 and dd-charts.js here, ahead
# of the hand-written alluvial below, which reuses the same d3.
s <- st[order(-st$white_pct), ]
dd_fig("two", "dumbbell",
       s[, c("state", "nhwhite_pct", "white_pct", "gap", "hisp_pct")],
  a = list(field = "nhwhite_pct", label = "non-Hispanic white (both questions)"),
  b = list(field = "white_pct", label = "white by the race question alone"),
  y = list(field = "state"),
  x = list(domain = c(0, 80), fmt = "pct0",
           label = "% of the state's population"),
  rowHeight = 36,
  tip = dd_tip(c(white_pct = "white, race question alone",
                 nhwhite_pct = "non-Hispanic white",
                 gap = "both Hispanic and white",
                 hisp_pct = "Hispanic, any race"),
               fmt = c(white_pct = "pct1", nhwhite_pct = "pct1",
                       gap = "pct1", hisp_pct = "pct1"),
               title = "state"))

## ---- other-who
data.frame(
  group = c("Chose 'Some Other Race'", "  of whom Hispanic",
            "  of whom not Hispanic"),
  people = c(n(tot_other), n(hisp_other), n(nh_other)),
  share = c("", paste0(pc(pct_other_h), "%"),
            paste0(pc(100 - pct_other_h), "%")))

## ---- two-race-who
data.frame(
  group = c("Reported two or more races", "  of whom Hispanic",
            "  of whom not Hispanic"),
  people = c(n(tot_two), n(hisp_two), n(nh_two)),
  share = c("", paste0(pc(pct_two_h), "%"), paste0(pc(100 - pct_two_h), "%")))

## ---- sankey-static
NW <- 0.03
rib <- function(ya0, ya1, yb0, yb1, col) {
  t <- seq(0, 1, length.out = 60); s <- (1 - cos(pi * t)) / 2
  xs <- NW + (1 - 2 * NW) * t
  polygon(c(xs, rev(xs)), c(ya0 + (yb0 - ya0) * s, rev(ya1 + (yb1 - ya1) * s)),
          col = col, border = NA)
}
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(-0.33, 1.30), ylim = c(hgt, 0), axes = FALSE, xlab = "",
     ylab = "", yaxs = "i")
for (i in seq_len(nrow(rc))) {
  a <- paste0(rc$fill[i], if (rc$fill[i] == "#999999") "55" else "99")
  rib(rc$y0[i], rc$y0[i] + rc$hisp[i], rc$hy[i], rc$hy[i] + rc$hisp[i], a)
  rib(rc$y0[i] + rc$hisp[i], rc$y1[i], rc$ny[i], rc$ny[i] + rc$nonh[i],
      paste0(rc$fill[i], "33"))
}
rect(0, rc$y0, NW, rc$y1, col = rc$fill, border = NA)
rect(1 - NW, rt$y0, 1, rt$y1, col = rt$fill, border = NA)
text(-0.012, (rc$y0 + rc$y1) / 2, rc$race, pos = 2, cex = 0.72)
text(-0.012, (rc$y0 + rc$y1) / 2 + 0.022 * hgt,
     paste0(pc(rc$share), "% Hispanic"), pos = 2, cex = 0.62, col = "#666666")
text(1.012, (rt$y0 + rt$y1) / 2, rt$name, pos = 4, cex = 0.78, font = 2)
text(1.012, (rt$y0 + rt$y1) / 2 + 0.022 * hgt, n(rt$tot), pos = 4, cex = 0.66,
     col = "#666666")

## ---- sankey-d3
lr <- paste(sprintf(
  '{"r":"%s","t":%d,"h":%d,"nn":%d,"s":%.1f,"y0":%.0f,"y1":%.0f,"hy":%.0f,"ny":%.0f,"c":"%s"}',
  rc$race, rc$tot, rc$hisp, rc$nonh, rc$share, rc$y0, rc$y1, rc$hy, rc$ny,
  rc$fill), collapse = ",")
rr <- paste(sprintf('{"n":"%s","t":%d,"y0":%.0f,"y1":%.0f,"c":"%s"}',
                    rt$name, rt$tot, rt$y0, rt$y1, rt$fill), collapse = ",")
cat(sprintf('
<div id="snk" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const L=[%s],R=[%s],HG=%.0f;
const W=760,H=470,M={t:10,r:210,b:10,l:200},NW=13;
const svg=d3.select("#snk").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,HG]).range([M.t,H-M.b]);
const xa=M.l,xb=W-M.r,xm=(xa+xb)/2;
const band=(a0,a1,b0,b1)=>
  `M${xa+NW},${y(a0)} C${xm},${y(a0)} ${xm},${y(b0)} ${xb},${y(b0)}`+
  ` L${xb},${y(b1)} C${xm},${y(b1)} ${xm},${y(a1)} ${xa+NW},${y(a1)} Z`;
const fmt=d3.format(",");
const cap=d3.select("#snk").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const base="<b>Hover a band.</b> The top two ribbons carry almost all of their people into the Hispanic node \\u2014 %s%% and %s%% of them.";
const g=svg.append("g");
L.forEach(d=>{
  const dim=d.c==="#999999";
  g.append("path").attr("d",band(d.y0,d.y0+d.h,d.hy,d.hy+d.h))
    .attr("fill",d.c).attr("fill-opacity",dim?0.32:0.6).style("cursor","pointer")
    .on("mousemove",()=>cap.html("<b>"+d.r+" \\u2192 Hispanic:</b> "+fmt(d.h)+
      " people, "+d.s.toFixed(1)+"%% of everyone who chose "+d.r+"."))
    .on("mouseleave",()=>cap.html(base));
  g.append("path").attr("d",band(d.y0+d.h,d.y1,d.ny,d.ny+d.nn))
    .attr("fill",d.c).attr("fill-opacity",dim?0.14:0.22).style("cursor","pointer")
    .on("mousemove",()=>cap.html("<b>"+d.r+" \\u2192 not Hispanic:</b> "+fmt(d.nn)+
      " people, "+(100-d.s).toFixed(1)+"%% of everyone who chose "+d.r+"."))
    .on("mouseleave",()=>cap.html(base));});
svg.append("g").selectAll("rect").data(L).join("rect")
  .attr("x",xa).attr("y",d=>y(d.y0)).attr("width",NW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.c);
svg.append("g").selectAll("rect").data(R).join("rect")
  .attr("x",xb-NW).attr("y",d=>y(d.y0)).attr("width",NW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.c);
const lt=svg.append("g").selectAll("g").data(L).join("g");
lt.append("text").attr("x",xa-8).attr("y",d=>(y(d.y0)+y(d.y1))/2-1)
  .attr("text-anchor","end").attr("font-size","12px").text(d=>d.r);
lt.append("text").attr("x",xa-8).attr("y",d=>(y(d.y0)+y(d.y1))/2+12)
  .attr("text-anchor","end").attr("font-size","10.5px").attr("fill","#777")
  .text(d=>d.s.toFixed(1)+"%% Hispanic");
const rtg=svg.append("g").selectAll("g").data(R).join("g");
rtg.append("text").attr("x",xb+9).attr("y",d=>(y(d.y0)+y(d.y1))/2-1)
  .attr("font-size","12.5px").attr("font-weight","600").text(d=>d.n);
rtg.append("text").attr("x",xb+9).attr("y",d=>(y(d.y0)+y(d.y1))/2+13)
  .attr("font-size","10.5px").attr("fill","#777").text(d=>fmt(d.t)+" people");
cap.html(base);
})();
</script>
', lr, rr, hgt, pc(rc$share[1]), pc(rc$share[2])))

## ---- top-two
o <- top_two[, c("county", "state", "total", "two_pct", "hisp_pct")]
o$total <- n(o$total); o$two_pct <- pc(o$two_pct); o$hisp_pct <- pc(o$hisp_pct)
names(o) <- c("county", "state", "population", "% two or more races",
              "% Hispanic")
o

## ---- scatter-static
plot(d$hisp_pct, d$two_pct, pch = 19, cex = 0.6,
     col = ifelse(d$state == "Hawaii", "#C41230", "#2166AC66"),
     xlab = "% of the county that is Hispanic",
     ylab = "% reporting two or more races", xlim = c(0, 100), ylim = c(0, 50))
points(hi_two$hisp_pct, hi_two$two_pct, pch = 19, col = "#C41230", cex = 1.1)
legend("topleft", c("Hawaii counties", "all other counties"),
       col = c("#C41230", "#2166AC"), pch = 19, bty = "n", cex = 0.85)

## ---- scatter-d3
# The shared library again: one dot per county, Hawaii picked out as its own
# series and drawn last so its five counties sit on top of the cloud.
dd <- d[, c("county", "state", "hisp_pct", "two_pct")]
dd$grp <- ifelse(dd$state == "Hawaii", "Hawaii counties", "all other counties")
dd <- dd[order(dd$state == "Hawaii"), ]
dd_fig("hisp-two", "scatter", dd,
  x = list(field = "hisp_pct", domain = c(0, 100), fmt = "pct0",
           label = "% of the county that is Hispanic"),
  y = list(field = "two_pct", domain = c(0, 50), fmt = "pct0",
           label = "% reporting two or more races"),
  series = list(field = "grp",
                classes = list("Hawaii counties" = "series-2",
                               "all other counties" = "series-1")),
  r = 3.4, opacity = 0.5, legend = TRUE,
  tip = dd_tip(c(state = "state", hisp_pct = "Hispanic",
                 two_pct = "two or more races"),
               fmt = c(hisp_pct = "pct1", two_pct = "pct1"),
               title = "county"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
