# distributions-code.R -- chunk bodies for distributions-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

di <- read.csv("data/derived/districts.csv", stringsAsFactors = FALSE)
by <- read.csv("data/derived/by_year.csv",   stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",     stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("by_year.csv", "districts.csv", "facts.csv"))

f  <- function(k) fx$value[fx$key == k]
p1 <- function(x) formatC(x, format = "f", digits = 1)
p2 <- function(x) formatC(x, format = "f", digits = 2)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

LAST <- f("last_year")
V    <- sort(di$dv[di$year == LAST & !is.na(di$dv)])   # the 397 numbers
NV   <- length(V)

MEAN <- f("mean"); MED <- f("median"); SD <- f("sd")
P25  <- f("p25");  P75 <- f("p75")
W5   <- f("within5"); W10 <- f("within10")
PKR  <- f("peak_rep"); PKD <- f("peak_dem")
B40  <- f("band_40_45"); B45 <- f("band_45_50"); B50 <- f("band_50_55")
UNC  <- f("uncontested"); SEATS <- f("seats")

# the years the ridgeline draws, one per era rather than one per election
RY <- c(1976, 1992, 2008, 2016, LAST)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

ACC <- "#1C4C5C"; WARN <- "#C41230"; GRY <- "#8A8F94"
DEM <- "#2c7fb8"; REP <- "#A33B2A"; GLD <- "#C08A2E"

## ---- one-row
z <- di[di$year == LAST & !is.na(di$dv), ]
z <- z[order(-z$dv), ]
o <- rbind(head(z[, c("stcd", "dv")], 2), tail(z[, c("stcd", "dv")], 2))
data.frame(
  District_code = o$stcd,
  Democratic_share = paste0(p1(o$dv), "%"),
  What_it_means = c("the most Democratic contested seat",
                    "the second most", "the second least",
                    "the least Democratic contested seat"))

## ---- varmap
data.frame(
  Column = c("year", "stcd", "dv", "uncontested"),
  What_it_holds = c(
    "the election year",
    "a state-district code — the race's identifier",
    "the Democratic share of the two-party vote, 0 to 100",
    "whether only one candidate stood"),
  Measurement = c("discrete, 1946 to 2024", "categorical, one per race",
                  "continuous", "dichotomous"))

## ---- summ
data.frame(
  Summary = c("Mean", "Median", "Standard deviation",
              "25th percentile", "75th percentile",
              "Within 5 points of even", "Within 10 points of even"),
  Value = c(paste0(p1(MEAN), "%"), paste0(p1(MED), "%"), p1(SD),
            paste0(p1(P25), "%"), paste0(p1(P75), "%"),
            paste0(p1(W5), "% of races"), paste0(p1(W10), "% of races")))

## ---- fig1-static
op <- par(mar = c(4.2, 4.0, 1.2, 1.0), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(15, 92), ylim = c(0.4, 2.6), axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(20, 90, 10), labels = paste0(seq(20, 90, 10), "%"),
     cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("Democratic share of the two-party vote", 1, line = 2.4, cex = 0.9)

# the box
bx <- boxplot.stats(V)
rect(P25, 1.75, P75, 2.25, col = "#E7EFF1", border = ACC, lwd = 1.6)
segments(MED, 1.75, MED, 2.25, col = ACC, lwd = 3)
segments(bx$stats[1], 2.0, P25, 2.0, col = ACC)
segments(P75, 2.0, bx$stats[5], 2.0, col = ACC)
segments(c(bx$stats[1], bx$stats[5]), 1.88, c(bx$stats[1], bx$stats[5]), 2.12,
         col = ACC)
text(15, 2.0, "boxplot", adj = 0, cex = 0.8, col = ACC, font = 2)

# the same numbers as a strip, jittered
set.seed(84355)
points(V, rep(1.05, NV) + runif(NV, -0.16, 0.16), pch = 19, cex = 0.42,
       col = paste0(GRY, "AA"))
text(15, 1.05, "every district", adj = 0, cex = 0.8, col = "#4E5A63", font = 2)
abline(v = MED, col = WARN, lty = 3)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The box and the raw points on one axis. The box is drawn first and the strip
# under it, because the argument is that the second contradicts the first --
# and hovering a point names the district, so the strip is not decorative.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
zz <- di[di$year == LAST & !is.na(di$dv), ]
rows <- paste0('{k:"', zz$stcd, '",v:', formatC(zz$dv, format = "f", digits = 2),
               '}', collapse = ",")
cat(paste0('
<div id="dbox" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const MED=', MED, ', P25=', P25, ', P75=', P75, ';
const ACC="', ACC, '", WARN="', WARN, '", GRY="', GRY, '";
const W=770,H=270,M={t:22,r:24,b:52,l:118};
const box=d3.select("#dbox");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([15,92]).range([M.l,W-M.r]);
const lo=d3.min(D,d=>d.v), hi=d3.max(D,d=>d.v);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(8));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Democratic share of the two-party vote");
// the box, on the upper track
const yb=64;
svg.append("line").attr("x1",x(lo)).attr("x2",x(P25)).attr("y1",yb).attr("y2",yb)
  .attr("stroke",ACC);
svg.append("line").attr("x1",x(P75)).attr("x2",x(hi)).attr("y1",yb).attr("y2",yb)
  .attr("stroke",ACC);
[lo,hi].forEach(function(v){
  svg.append("line").attr("x1",x(v)).attr("x2",x(v)).attr("y1",yb-9).attr("y2",yb+9)
     .attr("stroke",ACC);
});
svg.append("rect").attr("x",x(P25)).attr("y",yb-18).attr("width",x(P75)-x(P25))
  .attr("height",36).attr("fill","#E7EFF1").attr("stroke",ACC).attr("stroke-width",1.6);
svg.append("line").attr("x1",x(MED)).attr("x2",x(MED)).attr("y1",yb-18).attr("y2",yb+18)
  .attr("stroke",ACC).attr("stroke-width",3);
svg.append("text").attr("x",M.l-12).attr("y",yb+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("font-weight","600").attr("fill",ACC).text("boxplot");
// the median, carried down the figure
svg.append("line").attr("x1",x(MED)).attr("x2",x(MED)).attr("y1",yb+18)
  .attr("y2",H-M.b).attr("stroke",WARN).attr("stroke-dasharray","3 3");
// every district, on the lower track
const ys=170;
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:5px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const jitter=d3.randomUniform.source(d3.randomLcg(0.42))(-17,17);
svg.selectAll("circle.d").data(D).join("circle").attr("class","d")
  .attr("cx",d=>x(d.v)).attr("cy",()=>ys+jitter()).attr("r",2.6)
  .attr("fill",GRY).attr("fill-opacity",0.7)
  .on("mousemove",function(e,d){
    d3.select(this).attr("r",5).attr("fill",WARN).attr("fill-opacity",1).raise();
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+13)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.k+"</b><br>"+d.v.toFixed(1)+"% Democratic");
  })
  .on("mouseleave",function(){
    d3.select(this).attr("r",2.6).attr("fill",GRY).attr("fill-opacity",0.7);
    tip.style("opacity",0);
  });
svg.append("text").attr("x",M.l-12).attr("y",ys+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("font-weight","600").attr("fill","#4E5A63")
  .text("every district");
})();
</script>'))

## ---- fig2-static
op <- par(mar = c(4.2, 4.2, 1.2, 1.0), mgp = c(2.7, 0.7, 0))
h <- hist(V, breaks = seq(15, 90, 5), plot = FALSE)
barplot(h$counts, space = 0, col = ifelse(h$mids > 50, DEM, REP), border = "white",
        ylim = c(0, max(h$counts) * 1.15), axes = FALSE)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("districts", 2, line = 2.8, cex = 0.9)
at <- seq(0, length(h$counts), 2)
axis(1, at = at, labels = paste0(seq(15, 90, 10), "%"),
     cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("Democratic share of the two-party vote", 1, line = 2.4, cex = 0.9)
abline(v = (MED - 15) / 5, col = WARN, lty = 3, lwd = 1.8)
text((MED - 15) / 5, max(h$counts) * 1.10, " median", col = WARN, adj = 0,
     cex = 0.74)
par(op)

## ---- fig2-d3
# The bin width is the argument. At two points the dip is jagged noise; at five
# it is unmistakable; at fifteen it is gone and the distribution looks like one
# smooth hill. The reader moves the control and watches a finding appear and
# disappear without a single number changing.
vv <- paste(formatC(V, format = "f", digits = 2), collapse = ",")
cat(paste0('
<div id="dhist" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const V=[', vv, '];
const MED=', MED, ', MEAN=', MEAN, ';
const DEM="', DEM, '", REP="', REP, '", WARN="', WARN, '";
const W=770,H=400,M={t:18,r:20,b:56,l:56};
const box=d3.select("#dhist");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([15,90]).range([M.l,W-M.r]);
const y=d3.scaleLinear().range([H-M.b,M.t]);
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")");
const gy=svg.append("g").attr("transform","translate("+M.l+",0)");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Democratic share of the two-party vote");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("districts");
const gb=svg.append("g");
const rule=svg.append("line").attr("stroke",WARN).attr("stroke-dasharray","3 3")
  .attr("y1",M.t).attr("y2",H-M.b).attr("x1",x(MED)).attr("x2",x(MED));
const rlab=svg.append("text").attr("y",M.t+11).attr("font-size","11px")
  .attr("fill",WARN).attr("x",x(MED)+5).text("median " + MED.toFixed(1) + "%");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:5px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const lab=bar.append("span").attr("style","color:#4E5A63");
const slider=bar.append("input").attr("type","range").attr("min","1").attr("max","15")
  .attr("step","1").attr("value","5")
  .attr("style","flex:1;max-width:280px;accent-color:#1C4C5C");
function draw(){
  const bw=+slider.property("value");
  const lo=15, hi=90;
  const nb=Math.ceil((hi-lo)/bw);
  const counts=new Array(nb).fill(0);
  V.forEach(function(v){
    let i=Math.floor((v-lo)/bw); if(i<0)i=0; if(i>=nb)i=nb-1; counts[i]++;
  });
  y.domain([0,d3.max(counts)*1.12]);
  gy.call(d3.axisLeft(y).ticks(6));
  gx.call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(8));
  const data=counts.map((c,i)=>({c:c,a:lo+i*bw,b:Math.min(lo+(i+1)*bw,hi)}));
  gb.selectAll("rect").data(data).join("rect")
    .attr("x",d=>x(d.a)+0.5).attr("width",d=>Math.max(1,x(d.b)-x(d.a)-1))
    .attr("y",d=>y(d.c)).attr("height",d=>y(0)-y(d.c))
    .attr("fill",d=>(d.a+d.b)/2>50?DEM:REP)
    .on("mousemove",function(e,d){
      const r=box.node().getBoundingClientRect();
      tip.style("opacity",1).style("left",(e.clientX-r.left+13)+"px")
         .style("top",(e.clientY-r.top-8)+"px")
         .html("<b>"+d.a.toFixed(0)+"\\u2013"+d.b.toFixed(0)+"%</b><br>"+
               d.c+" district"+(d.c===1?"":"s"));
    })
    .on("mouseleave",function(){tip.style("opacity",0);});
  lab.text("bin width: "+bw+(bw===1?" point":" points"));
  rlab.raise(); rule.raise();
}
slider.on("input",draw);
draw();
})();
</script>'))

## ---- fig3-static
op <- par(mar = c(4.2, 4.4, 1.2, 1.0), mgp = c(2.8, 0.7, 0))
plot(V, 100 * seq_along(V) / NV, type = "s", col = ACC, lwd = 2.4,
     axes = FALSE, xlab = "", ylab = "", xlim = c(15, 90), ylim = c(0, 100))
axis(1, at = seq(20, 90, 10), labels = paste0(seq(20, 90, 10), "%"),
     cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("Democratic share of the two-party vote", 1, line = 2.4, cex = 0.9)
mtext("% of districts at or below", 2, line = 3.0, cex = 0.9)
rect(45, 0, 55, 100, col = "#F2E7C9", border = NA)
lines(V, 100 * seq_along(V) / NV, type = "s", col = ACC, lwd = 2.4)
lo <- 100 * sum(V <= 45) / NV; hi <- 100 * sum(V <= 55) / NV
segments(45, lo, 55, lo, col = WARN, lty = 3)
segments(55, lo, 55, hi, col = WARN, lwd = 2.4)
text(56, (lo + hi) / 2, paste0(p1(hi - lo), "% of districts\nare in this band"),
     adj = 0, cex = 0.72, col = WARN)
par(op)

## ---- fig3-d3
# The ECDF with a movable band. The reader sets a width around even and reads
# the count straight off the curve, which is the one question the boxplot and
# the histogram both make you estimate by eye.
cat(paste0('
<div id="decdf" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const V=[', vv, '];
const N=V.length;
const ACC="', ACC, '", WARN="', WARN, '", GLD="', GLD, '";
const W=770,H=400,M={t:18,r:24,b:56,l:62};
const box=d3.select("#decdf");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([15,90]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
const band=svg.append("rect").attr("y",M.t).attr("height",H-M.b-M.t)
  .attr("fill","#F2E7C9").attr("fill-opacity",0.75);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Democratic share of the two-party vote");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("% of districts at or below");
const pts=V.map((v,i)=>({v:v,p:100*(i+1)/N}));
svg.append("path").attr("fill","none").attr("stroke",ACC).attr("stroke-width",2.4)
  .attr("d",d3.line().curve(d3.curveStepAfter).x(d=>x(d.v)).y(d=>y(d.p))(pts));
const seg=svg.append("line").attr("stroke",WARN).attr("stroke-width",2.6);
const t1=svg.append("line").attr("stroke",WARN).attr("stroke-dasharray","3 3");
const rd=svg.append("text").attr("font-size","12px").attr("font-weight","600")
  .attr("fill",WARN);
const lab=bar.append("span").attr("style","color:#4E5A63");
const slider=bar.append("input").attr("type","range").attr("min","1").attr("max","25")
  .attr("step","1").attr("value","5")
  .attr("style","flex:1;max-width:280px;accent-color:#1C4C5C");
const below=v=>100*V.filter(z=>z<=v).length/N;
function draw(){
  const w=+slider.property("value");
  const a=50-w, b=50+w;
  band.attr("x",x(a)).attr("width",x(b)-x(a));
  const lo=below(a), hi=below(b);
  t1.attr("x1",x(a)).attr("x2",x(b)).attr("y1",y(lo)).attr("y2",y(lo));
  seg.attr("x1",x(b)).attr("x2",x(b)).attr("y1",y(lo)).attr("y2",y(hi));
  rd.attr("x",x(b)+7).attr("y",(y(lo)+y(hi))/2+4)
    .text((hi-lo).toFixed(1)+"% of districts");
  lab.text("within \\u00b1"+w+" points of even");
}
slider.on("input",draw);
draw();
})();
</script>'))

## ---- fig4-static
op <- par(mar = c(4.2, 5.4, 1.2, 1.0), mgp = c(2.7, 0.7, 0))
plot(NA, xlim = c(15, 90), ylim = c(0.6, length(RY) + 0.9), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(20, 90, 10), labels = paste0(seq(20, 90, 10), "%"),
     cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("Democratic share of the two-party vote", 1, line = 2.4, cex = 0.9)
for (i in seq_along(RY)) {
  v <- di$dv[di$year == RY[i] & !is.na(di$dv)]
  d <- density(v, bw = 3.2, from = 15, to = 90)
  base <- length(RY) - i + 1
  sc <- 1.55 / max(d$y)
  polygon(c(d$x, rev(d$x)), c(base + d$y * sc, rep(base, length(d$x))),
          col = "#E7EFF1", border = ACC, lwd = 1.5)
  segments(50, base, 50, base + 1.55, col = WARN, lty = 3)
  mtext(RY[i], 2, at = base + 0.35, las = 1, line = 0.6, cex = 0.82)
  m <- by$median[by$year == RY[i]]
  points(m, base, pch = 19, col = WARN, cex = 0.8, xpd = NA)
}
par(op)

## ---- fig4-d3
# One density per election, stacked. The densities are computed in R -- the
# same bandwidth for every year, so the years are comparable -- and drawn here,
# because a bandwidth chosen per-year would be the bin-width problem again in
# a form the reader cannot see.
dens <- do.call(rbind, lapply(RY, function(y) {
  v <- di$dv[di$year == y & !is.na(di$dv)]
  k <- density(v, bw = 3.2, from = 15, to = 90, n = 160)
  data.frame(year = y, x = k$x, y = k$y / max(k$y), stringsAsFactors = FALSE)
}))
ser <- vapply(RY, function(y) {
  z <- dens[dens$year == y, ]
  b <- by[by$year == y, ]
  paste0('{yr:', y, ',med:', b$median, ',n:', b$contested,
         ',unc:', b$uncontested, ',w5:', b$within5,
         ',y:[', paste(formatC(z$y, format = "f", digits = 4), collapse = ","),
         ']}')
}, character(1))
xs <- paste(formatC(sort(unique(dens$x)), format = "f", digits = 2), collapse = ",")
cat(paste0('
<div id="dridge" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[', paste(ser, collapse = ","), '];
const X=[', xs, '];
const ACC="', ACC, '", WARN="', WARN, '";
const W=770,H=430,M={t:18,r:24,b:56,l:70};
const box=d3.select("#dridge");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([15,90]).range([M.l,W-M.r]);
const rowH=(H-M.t-M.b)/S.length;
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(8));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Democratic share of the two-party vote");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
S.forEach(function(s,i){
  const base=M.t+(i+1)*rowH;
  const amp=rowH*1.5;
  const pts=X.map((xv,j)=>({x:xv,y:s.y[j]}));
  const area=d3.area().x(d=>x(d.x)).y0(base).y1(d=>base-d.y*amp);
  svg.append("path").attr("d",area(pts)).attr("fill","#E7EFF1")
     .attr("stroke",ACC).attr("stroke-width",1.5);
  svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",base)
     .attr("y2",base-amp).attr("stroke",WARN).attr("stroke-dasharray","3 3");
  svg.append("circle").attr("cx",x(s.med)).attr("cy",base).attr("r",3.6)
     .attr("fill",WARN);
  svg.append("text").attr("x",M.l-12).attr("y",base-4).attr("text-anchor","end")
     .attr("font-size","12px").attr("fill","currentColor").text(s.yr);
  svg.append("rect").attr("x",M.l).attr("y",base-amp).attr("width",W-M.r-M.l)
     .attr("height",amp).attr("fill","transparent")
     .on("mousemove",function(e){
       const r=box.node().getBoundingClientRect();
       tip.style("opacity",1).style("left",(e.clientX-r.left+13)+"px")
          .style("top",(e.clientY-r.top-8)+"px")
          .html("<b>"+s.yr+"</b><br>"+s.n+" contested, "+s.unc+" uncontested<br>"+
                "median "+s.med.toFixed(1)+"%<br>within 5 points: "+
                s.w5.toFixed(1)+"%");
     })
     .on("mouseleave",function(){tip.style("opacity",0);});
});
})();
</script>'))
