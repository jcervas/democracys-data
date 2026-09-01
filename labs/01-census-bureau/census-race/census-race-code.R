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

# the states where the choice between the two definitions crosses 50%: white
# by the race question, not white once the origin question is counted
flip <- st[st$white_pct >= 50 & st$nhwhite_pct < 50, ]
flip <- flip[order(-flip$gap), ]

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
top_two_tx <- sum(top_two$state == "Texas")
hi_two  <- d[d$state == "Hawaii", ]
hi_top  <- hi_two[which.max(hi_two$two_pct), ]
# where Hawaii's counties actually sit once every county in the country is in
# the ranking: high, but not the top -- which is the whole point of Figure 3
d$two_rank <- rank(-d$two_pct, ties.method = "min")
hi_rank <- range(d$two_rank[d$state == "Hawaii"])
hi_state_two <- 100 * sum(hi_two$two_or_more) / sum(hi_two$total)

p1ok <- all(d$one_race + d$two_or_more == d$total)
p2ok <- all(d$hispanic + d$not_hispanic == d$total)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# Counts under eleven are spelled out in prose, which is the book's habit and
# the only way a sentence does not open on a numeral. Still computed, so a
# changed file changes the sentence rather than quietly contradicting it.
wd <- function(k) {
  w <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
         "nine", "ten")
  if (k >= 1 && k <= 10) w[k] else n(k)
}
Wd <- function(k) sub("^(.)", "\\U\\1", wd(k), perl = TRUE)

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
            "1 April 2020", n(nrow(d)),
            paste0(length(unique(d$state)) - 1, " and the District of Columbia"),
            n(sum(d$total)),
            paste0(pc(100 * sum(d$total) / 331449281, 0),
                   "% -- every resident of the 50 states and DC"),
            "P1 (race), P2 (Hispanic origin by race), P5 (group quarters)"))

## ---- checks
data.frame(
  test = c("one race + two or more races = total, in every county",
           "Hispanic + not Hispanic = total, in every county"),
  holds = c(ifelse(p1ok, "TRUE", "FALSE"), ifelse(p2ok, "TRUE", "FALSE")),
  counties_tested = c(nrow(d), nrow(d)))

## ---- two-static
# Fifty-one rows, so the row labels set the type size rather than the other way
# round. The 50% rule is drawn because three states cross it between the two
# readings, which is the fact the figure exists to show.
s <- st[order(st$white_pct), ]
yy <- seq_len(nrow(s))
par(mar = c(4.2, 8.2, 1.2, 2))
plot(NA, xlim = c(0, 100), ylim = c(0.4, nrow(s) + 0.6), yaxt = "n", bty = "n",
     xlab = "% of the state's population", ylab = "")
abline(v = seq(0, 100, 20), col = "#eeeeee")
abline(v = 50, col = "#999999", lty = 2)
segments(s$nhwhite_pct, yy, s$white_pct, yy, col = "#bbbbbb", lwd = 1.8)
points(s$nhwhite_pct, yy, pch = 19, col = "#2166AC", cex = 0.7)
points(s$white_pct, yy, pch = 19, col = "#92C5DE", cex = 0.7)
axis(2, at = yy, labels = s$state, las = 1, tick = FALSE, cex.axis = 0.52)
legend("bottomright",
       c("non-Hispanic white (both questions)",
         "white by the race question alone"),
       col = c("#2166AC", "#92C5DE"), pch = 19, bty = "n", cex = 0.75)

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
  x = list(domain = c(0, 100), fmt = "pct0",
           label = "% of the state's population"),
  # The majority line: three states sit on one side of it by the race question
  # and the other side once the origin question is counted. Classed "zero"
  # rather than taking the default "rule", because the dumbbell's own connector
  # lines are class "rule" and the re-sort below selects them by it -- left on
  # the default, this one annotation would ride up and down with the rows.
  annotations = list(dd_annot_vline(50, class = "zero")),
  rowHeight = 17, r = 3.6,
  tip = dd_tip(c(white_pct = "white, race question alone",
                 nhwhite_pct = "non-Hispanic white",
                 gap = "both Hispanic and white",
                 hisp_pct = "Hispanic, any race"),
               fmt = c(white_pct = "pct1", nhwhite_pct = "pct1",
                       gap = "pct1", hisp_pct = "pct1"),
               title = "state"),
  # ---- the sort toggle ------------------------------------------------------
  # Rank order is an argument, and this figure can make three of them. Sorted
  # by white share it is a league table; sorted by the gap it says which states
  # the choice of definition moves most, which is the chapter's point and is
  # invisible in the default order; alphabetical is the one that plays no
  # rhetorical games and lets a reader find their own state.
  #
  # Done through the library's hook, which hands back the finished figure, so
  # the only thing this adds is a re-scale: the y domain is a list of state
  # names, and reordering that list moves every mark. Nothing is redrawn and no
  # data is re-sent. The buttons are built here rather than cat() as loose HTML
  # so the control cannot outlive or drift from the figure it drives.
  hook = dd_js('function(f){
  const F=f.cfg.y.field, D=620, rows=f.data;
  const by=(k)=>rows.slice().sort(k).map(d=>d[F]);
  const ord={white: by((a,b)=>b.white_pct-a.white_pct),
             gap:   by((a,b)=>b.gap-a.gap),
             az:    by((a,b)=>d3.ascending(a[F],b[F]))};
  // The left axis is the one translated to the left margin; the bottom axis
  // carries the same class. Its ticks are moved rather than the axis re-called:
  // d3 keys axis ticks by POSITION, so re-calling it relabels the ticks where
  // they stand and the names appear to teleport. Each tick is already bound to
  // its own state name, so moving it by that datum carries the label along with
  // the row it belongs to, which is the thing a reader is trying to follow.
  const yAxis=f.svg.selectAll("g.axis").filter(function(){
    return d3.select(this).attr("transform")==="translate("+f.M.l+",0)";});
  // Motion is decoration here; the new order is the information. Honour a
  // reduced-motion preference, and skip the animation on a hidden page, where
  // requestAnimationFrame never fires and a transition would never land --
  // either way the marks must end up in the right place.
  const still=(window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches);
  const move=(sel)=>(still||document.hidden)?sel:sel.transition().duration(D);
  const bar=f.box.insert("div",":first-child").attr("class","dd-sort");
  const btn=bar.selectAll("button")
    .data([["white","most white first"],["gap","widest gap first"],
           ["az","A to Z"]])
    .join("button").attr("type","button").text(d=>d[1])
    .on("click",function(e,d){go(d[0]);});
  function go(k){
    btn.classed("on",d=>d[0]===k);
    f.y.domain(ord[k]);
    // class "rule" is the connector joining the two dots of one state; the 50%
    // annotation is classed "zero" so that it stays put
    move(f.svg.selectAll("line.rule"))
      .attr("y1",d=>f.y(d[F])).attr("y2",d=>f.y(d[F]));
    move(f.svg.selectAll("circle.pt")).attr("cy",d=>f.y(d[F]));
    move(yAxis.selectAll(".tick")).attr("transform",d=>"translate(0,"+f.y(d)+")");
  }
  go("white");
}'))

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
# The alluvial has no dd-charts type -- it is two node columns and fourteen
# ribbons -- so it is drawn by hand. What it does borrow is the library's
# furniture: the .dd-fig container and DD.tip(), so the readout is the same
# hovering box as every other figure in the book, and picks up the same
# dark-mode CSS instead of hard-coding a grey.
#
# The readout used to be a caption under the chart, which meant reading a
# number with your eye in one place and the ribbon under your cursor in
# another. It is a tooltip now, and it carries BOTH percentages a cell of a
# cross-tabulation has: the row share (what fraction of this race answer said
# yes to the origin question) and the column share (what fraction of all
# Hispanics this ribbon is). The row share is the chapter's argument; the
# column share is what stops a reader inferring the argument backwards.
#
# Built with paste0(), not sprintf(): the D3 below is full of % signs, and one
# unescaped one in a format string is a silent corruption.
lr <- paste(sprintf(
  '{"r":"%s","t":%d,"h":%d,"nn":%d,"s":%.1f,"y0":%.0f,"y1":%.0f,"hy":%.0f,"ny":%.0f,"c":"%s"}',
  rc$race, rc$tot, rc$hisp, rc$nonh, rc$share, rc$y0, rc$y1, rc$hy, rc$ny,
  rc$fill), collapse = ",")
rr <- paste(sprintf('{"n":"%s","t":%d,"y0":%.0f,"y1":%.0f,"c":"%s"}',
                    rt$name, rt$tot, rt$y0, rt$y1, rt$fill), collapse = ",")
cat(paste0('
<div class="dd-fig" id="snk" style="margin:1em 0"></div>
<script>
(function(){
const L=[', lr, '],R=[', rr, '],HG=', sprintf("%.0f", hgt), ';
const W=760,H=470,M={t:10,r:210,b:10,l:200},NW=13;
const box=d3.select("#snk");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,HG]).range([M.t,H-M.b]);
const xa=M.l,xb=W-M.r,xm=(xa+xb)/2;
const band=(a0,a1,b0,b1)=>
  `M${xa+NW},${y(a0)} C${xm},${y(a0)} ${xm},${y(b0)} ${xb},${y(b0)}`+
  ` L${xb},${y(b1)} C${xm},${y(b1)} ${xm},${y(a1)} ${xa+NW},${y(a1)} Z`;
const fmt=d3.format(","), TOT=R[0].t+R[1].t;
const pct=(a,b)=>(100*a/b).toFixed(1)+"%";
const tip=DD.tip(box);

// one datum per ribbon: which race answer, and which side of the origin
// question it flows into
const ribs=[];
L.forEach(d=>{ribs.push({d:d,hisp:true},{d:d,hisp:false});});
const op=r=>{const dim=r.d.c==="#999999";
  return r.hisp?(dim?0.32:0.6):(dim?0.14:0.22);};
const paths=svg.append("g").selectAll("path").data(ribs).join("path")
  .attr("d",r=>r.hisp?band(r.d.y0,r.d.y0+r.d.h,r.d.hy,r.d.hy+r.d.h)
                     :band(r.d.y0+r.d.h,r.d.y1,r.d.ny,r.d.ny+r.d.nn))
  .attr("fill",r=>r.d.c).attr("fill-opacity",op).style("cursor","pointer");

function ribTip(r){
  const d=r.d,node=r.hisp?R[0]:R[1],k=r.hisp?d.h:d.nn;
  return "<b>"+d.r+" \\u2192 "+node.n+"<\\/b><br>"+
    fmt(k)+" people<br>"+
    pct(k,d.t)+" of everyone who chose \\u201c"+d.r+"\\u201d<br>"+
    pct(k,node.t)+" of everyone "+(r.hisp?"Hispanic":"not Hispanic");
}
paths.on("mousemove",function(e,r){
    tip.show(ribTip(r),e);
    paths.attr("fill-opacity",q=>q===r?Math.min(0.9,op(q)+0.28):op(q)*0.3);})
  .on("mouseleave",function(){tip.hide();paths.attr("fill-opacity",op);});

// the node bars are hoverable too: the race answer as a whole on the left,
// the origin answer as a whole on the right
svg.append("g").selectAll("rect").data(L).join("rect")
  .attr("x",xa).attr("y",d=>y(d.y0)).attr("width",NW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.c)
  .style("cursor","pointer")
  .on("mousemove",function(e,d){tip.show("<b>"+d.r+"<\\/b><br>"+fmt(d.t)+
    " people, "+pct(d.t,TOT)+" of the country<br>"+
    d.s.toFixed(1)+"% of them Hispanic or Latino",e);})
  .on("mouseleave",()=>tip.hide());
svg.append("g").selectAll("rect").data(R).join("rect")
  .attr("x",xb-NW).attr("y",d=>y(d.y0)).attr("width",NW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.c)
  .style("cursor","pointer")
  .on("mousemove",function(e,d){tip.show("<b>"+d.n+"<\\/b><br>"+fmt(d.t)+
    " people, "+pct(d.t,TOT)+" of the country",e);})
  .on("mouseleave",()=>tip.hide());

const lt=svg.append("g").selectAll("g").data(L).join("g")
  .style("pointer-events","none");
lt.append("text").attr("class","ttl").attr("x",xa-8)
  .attr("y",d=>(y(d.y0)+y(d.y1))/2-1)
  .attr("text-anchor","end").attr("font-size","12px").text(d=>d.r);
lt.append("text").attr("class","foot").attr("x",xa-8)
  .attr("y",d=>(y(d.y0)+y(d.y1))/2+12)
  .attr("text-anchor","end").attr("font-size","10.5px")
  .text(d=>d.s.toFixed(1)+"% Hispanic");
const rtg=svg.append("g").selectAll("g").data(R).join("g")
  .style("pointer-events","none");
rtg.append("text").attr("class","ttl").attr("x",xb+9)
  .attr("y",d=>(y(d.y0)+y(d.y1))/2-1)
  .attr("font-size","12.5px").attr("font-weight","600").text(d=>d.n);
rtg.append("text").attr("class","foot").attr("x",xb+9)
  .attr("y",d=>(y(d.y0)+y(d.y1))/2+13)
  .attr("font-size","10.5px").text(d=>fmt(d.t)+" people");
})();
</script>
<p style="font-size:0.85em;color:var(--ink-2);margin-top:0.2em">Hover any
ribbon for its two counts, or either end bar for the totals.</p>
'))

## ---- top-two
o <- top_two[, c("county", "state", "total", "two_pct", "hisp_pct")]
o$total <- n(o$total); o$two_pct <- pc(o$two_pct); o$hisp_pct <- pc(o$hisp_pct)
names(o) <- c("county", "state", "population", "% two or more races",
              "% Hispanic")
o

## ---- scatter-static
# Three thousand dots rather than five hundred, so the ink per dot comes down:
# smaller marks and more transparency, or the cloud fills in solid and stops
# showing where the counties actually are.
plot(d$hisp_pct, d$two_pct, pch = 19, cex = 0.34,
     col = ifelse(d$state == "Hawaii", "#C41230", "#2166AC44"),
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
  r = 2.3, opacity = 0.35, legend = TRUE,
  # 3,143 dots need less ink each than 503 did, but Hawaii's five are the
  # comparison the figure is making and would vanish at the cloud's weight.
  # The hook is the library's escape hatch: same marks, one series restyled.
  hook = dd_js('function(f){f.svg.selectAll("circle.series-2-fill")
                  .attr("r",4.6).attr("fill-opacity",1).raise();}'),
  tip = dd_tip(c(state = "state", hisp_pct = "Hispanic",
                 two_pct = "two or more races"),
               fmt = c(hisp_pct = "pct1", two_pct = "pct1"),
               title = "county"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
