# uncertainty-code.R -- chunk bodies for uncertainty-brief.Rmd
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

gp <- read.csv("data/derived/by_group.csv",  stringsAsFactors = FALSE)
de <- read.csv("data/derived/by_decile.csv", stringsAsFactors = FALSE)
th <- read.csv("data/derived/thresh.csv",    stringsAsFactors = FALSE)
cb <- read.csv("data/derived/calib.csv",     stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",     stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

NV <- fn("voters"); ACC <- fn("acc"); ALO <- fn("acc_lo"); AHI <- fn("acc_hi")
AUC <- fn("auc"); PHIT <- fn("pct_hit"); PMISS <- fn("pct_miss")
NMISS <- fn("n_miss")
AIN <- fn("aian_n"); AIR <- fn("aian_recall"); AILO <- fn("aian_lo")
AIHI <- fn("aian_hi"); AIP <- fn("aian_prec")
WR <- fn("white_recall"); BR <- fn("black_recall")
HTPR <- fn("half_tpr"); HFPR <- fn("half_fpr"); HPREC <- fn("half_prec")
DLO <- fn("dec_lo_recall"); DHI <- fn("dec_hi_recall")

de$width <- de$hi - de$lo
NARROW <- max(de$width)

ACC2 <- "#1C4C5C"; WARN <- "#C41230"; GRY <- "#8A8F94"
POOL <- "#54278F"; BAND <- "#E7EFF1"

# ---- why these three figures are hand-written (STYLE.md rule 16) -----------
# Designated showpieces, not drift. All three draw a mark the shared library
# has no vocabulary for, and the mark IS the argument:
#   fig1, fig2  a forest plot -- the interval drawn as a bar with a diamond at
#               the estimate, and the pooled interval carried through as a
#               band. dd_fig() draws points and bars, never an interval, and a
#               brief whose whole subject is interval width cannot imply one.
#   fig3        an ROC curve with a threshold slider that re-reports four
#               counts of real people. The interaction is the point: the
#               reader moves the cut and watches the errors trade.

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- varmap
data.frame(
  Quantity = c("recall", "precision", "the interval", "AUC"),
  What_it_answers = c(
    "of the people who ARE this race, how many did the model find",
    "of the people the model CALLED this race, how many were",
    "how far the estimate would move if you drew the sample again",
    "how well the score separates the two groups, at any threshold"),
  Computed_from = c("truth, then prediction", "prediction, then truth",
                    "the count and the sample size",
                    "the ranking, not the threshold"))

## ---- fig1-static
op <- par(mar = c(4.0, 7.6, 1.2, 1.6), mgp = c(2.5, 0.7, 0))
z <- gp[gp$group != "ALL (pooled)", ]
z <- z[order(z$recall), ]
plot(NA, xlim = c(0, 100), ylim = c(0.4, nrow(z) + 1.4), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("recall: of the people who are this race, the share the model found",
      1, line = 2.4, cex = 0.82)
pooled <- gp[gp$group == "ALL (pooled)", ]
rect(pooled$rec_lo, 0.3, pooled$rec_hi, nrow(z) + 1.5, col = BAND, border = NA)
abline(v = pooled$recall, col = POOL, lty = 2, lwd = 1.6)
for (i in seq_len(nrow(z))) {
  segments(z$rec_lo[i], i, z$rec_hi[i], i, col = ACC2, lwd = 2)
  points(z$recall[i], i, pch = 18, cex = 1.5, col = ACC2)
  mtext(paste0(z$group[i], "  (n=", n(z$n_true[i]), ")"), 2, at = i, las = 1,
        line = 0.4, cex = 0.74)
}
polygon(c(pooled$rec_lo, pooled$recall, pooled$rec_hi, pooled$recall),
        nrow(z) + 1 + c(0, 0.28, 0, -0.28), col = POOL, border = NA)
mtext("pooled", 2, at = nrow(z) + 1, las = 1, line = 0.4, cex = 0.74,
      font = 2)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# A forest plot: one row per estimate, the interval drawn rather than implied,
# and the pooled figure as a diamond and a band behind everything. The band is
# the point of the figure -- it is narrow, and almost nothing lies in it.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
z <- gp[gp$group != "ALL (pooled)", ]; z <- z[order(z$recall), ]
po <- gp[gp$group == "ALL (pooled)", ]
rows <- paste0('{g:"', z$group, '",n:', z$n_true, ',v:', z$recall,
               ',lo:', z$rec_lo, ',hi:', z$rec_hi, ',p:',
               ifelse(is.na(z$precision), 0, z$precision), '}', collapse = ",")
cat(paste0('
<div id="fg" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const PO={v:', po$recall, ',lo:', po$rec_lo, ',hi:', po$rec_hi,
        ',n:', po$n_true, '};
const ACC="', ACC2, '", POOL="', POOL, '", BAND="', BAND, '";
const W=770,H=320,M={t:22,r:120,b:52,l:150};
const box=d3.select("#fg");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(D.map(d=>d.g).concat(["pooled"]))
  .range([H-M.b-18,M.t+10]).padding(0.6);
svg.append("rect").attr("x",x(PO.lo)).attr("y",M.t)
  .attr("width",x(PO.hi)-x(PO.lo)).attr("height",H-M.b-M.t).attr("fill",BAND);
svg.append("line").attr("x1",x(PO.v)).attr("x2",x(PO.v)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke",POOL).attr("stroke-dasharray","4 3");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(5));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#4E5A63")
  .text("recall: of the people who are this race, the share the model found");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const g=svg.append("g").selectAll("g").data(D).join("g")
  .attr("transform",d=>"translate(0,"+y(d.g)+")");
g.append("line").attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi))
  .attr("stroke",ACC).attr("stroke-width",2);
g.append("line").attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.lo)).attr("y1",-4)
  .attr("y2",4).attr("stroke",ACC).attr("stroke-width",1.6);
g.append("line").attr("x1",d=>x(d.hi)).attr("x2",d=>x(d.hi)).attr("y1",-4)
  .attr("y2",4).attr("stroke",ACC).attr("stroke-width",1.6);
g.append("path").attr("d",d3.symbol().type(d3.symbolDiamond).size(70))
  .attr("transform",d=>"translate("+x(d.v)+",0)").attr("fill",ACC);
g.append("text").attr("x",M.l-10).attr("y",4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","currentColor").text(d=>d.g);
g.append("text").attr("x",W-M.r+8).attr("y",4).attr("font-size","11px")
  .attr("fill","#76838C")
  .text(d=>d.v.toFixed(1)+"% ["+d.lo.toFixed(1)+", "+d.hi.toFixed(1)+"]");
g.append("rect").attr("x",M.l).attr("y",-9).attr("width",W-M.r-M.l)
  .attr("height",18).attr("fill","transparent")
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.g+"</b><br>"+d3.format(",")(d.n)+" voters<br>recall "+
             d.v.toFixed(2)+"% ["+d.lo.toFixed(2)+", "+d.hi.toFixed(2)+"]<br>"+
             "precision "+d.p.toFixed(1)+"%");
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
// the pooled estimate as a diamond whose width IS its interval
const yp=y("pooled");
svg.append("path")
  .attr("d","M"+x(PO.lo)+","+yp+"L"+x(PO.v)+","+(yp-7)+"L"+x(PO.hi)+","+yp+
            "L"+x(PO.v)+","+(yp+7)+"Z")
  .attr("fill",POOL);
svg.append("text").attr("x",M.l-10).attr("y",yp+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("font-weight","700").attr("fill",POOL)
  .text("pooled");
svg.append("text").attr("x",W-M.r+8).attr("y",yp+4).attr("font-size","11px")
  .attr("fill",POOL).text(PO.v.toFixed(1)+"% ["+PO.lo.toFixed(1)+", "+
                          PO.hi.toFixed(1)+"]");
})();
</script>'))

## ---- fig2-static
op <- par(mar = c(4.0, 8.4, 1.2, 1.6), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = c(0, 100), ylim = c(nrow(de) + 0.6, 0.4), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("share of this stratum's Black voters the model found", 1, line = 2.4,
      cex = 0.82)
pooled <- gp[gp$group == "black", ]
rect(pooled$rec_lo, 0.3, pooled$rec_hi, nrow(de) + 0.7, col = BAND, border = NA)
abline(v = pooled$recall, col = POOL, lty = 2, lwd = 1.6)
for (i in seq_len(nrow(de))) {
  segments(de$lo[i], i, de$hi[i], i, col = ACC2, lwd = 2)
  points(de$recall[i], i, pch = 18, cex = 1.4, col = ACC2)
  # a plain hyphen: the PDF device has no en dash in its base font and
  # substitutes one with a warning on every label
  mtext(paste0(p1(de$block_black_lo[i]), "-", p1(de$block_black_hi[i]),
               "% Black  (n=", n(de$black_voters[i]), ")"),
        2, at = i, las = 1, line = 0.4, cex = 0.68)
}
par(op)

## ---- fig2-d3
# The same estimator, ten strata. Every interval here is narrow; the estimates
# still run from one end of the axis to the other. That is the figure.
rows <- paste0('{d:', de$decile, ',lo0:', de$block_black_lo, ',hi0:',
               de$block_black_hi, ',n:', de$black_voters, ',v:', de$recall,
               ',lo:', de$lo, ',hi:', de$hi, ',w:', round(de$width, 2), '}',
               collapse = ",")
po <- gp[gp$group == "black", ]
cat(paste0('
<div id="fd" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const PO={v:', po$recall, ',lo:', po$rec_lo, ',hi:', po$rec_hi, '};
const ACC="', ACC2, '", POOL="', POOL, '", BAND="', BAND, '";
const W=770,H=430,M={t:20,r:118,b:52,l:196};
const box=d3.select("#fd");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(D.map(d=>d.d)).range([M.t+10,H-M.b-10]).padding(0.5);
svg.append("rect").attr("x",x(PO.lo)).attr("y",M.t)
  .attr("width",Math.max(1,x(PO.hi)-x(PO.lo))).attr("height",H-M.b-M.t)
  .attr("fill",BAND);
svg.append("line").attr("x1",x(PO.v)).attr("x2",x(PO.v)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke",POOL).attr("stroke-dasharray","4 3");
svg.append("text").attr("x",x(PO.v)+6).attr("y",M.t+12).attr("font-size","11px")
  .attr("fill",POOL).text("pooled "+PO.v.toFixed(1)+"%");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(5));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#4E5A63")
  .text("share of this stratum\\u2019s Black voters the model found");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const g=svg.append("g").selectAll("g").data(D).join("g")
  .attr("transform",d=>"translate(0,"+y(d.d)+")");
g.append("line").attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi))
  .attr("stroke",ACC).attr("stroke-width",2);
[["lo"],["hi"]].forEach(function(k){
  g.append("line").attr("x1",d=>x(d[k[0]])).attr("x2",d=>x(d[k[0]]))
   .attr("y1",-4).attr("y2",4).attr("stroke",ACC).attr("stroke-width",1.6);
});
g.append("path").attr("d",d3.symbol().type(d3.symbolDiamond).size(64))
  .attr("transform",d=>"translate("+x(d.v)+",0)").attr("fill",ACC);
g.append("text").attr("x",M.l-10).attr("y",4).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","currentColor")
  .text(d=>d.lo0.toFixed(0)+"\\u2013"+d.hi0.toFixed(0)+"% Black blocks");
g.append("text").attr("x",W-M.r+8).attr("y",4).attr("font-size","11px")
  .attr("fill","#76838C").text(d=>d.v.toFixed(1)+"%  \\u00b1"+(d.w/2).toFixed(1));
g.append("rect").attr("x",M.l).attr("y",-9).attr("width",W-M.r-M.l)
  .attr("height",18).attr("fill","transparent")
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>blocks "+d.lo0.toFixed(1)+"\\u2013"+d.hi0.toFixed(1)+"% Black</b><br>"+
             d3.format(",")(d.n)+" Black voters<br>found "+d.v.toFixed(2)+
             "% ["+d.lo.toFixed(2)+", "+d.hi.toFixed(2)+"]<br>"+
             "interval width "+d.w.toFixed(1)+" points");
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
})();
</script>'))

## ---- fig3-static
op <- par(mar = c(4.0, 4.4, 1.2, 1.2), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"),
     cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"),
     las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("false positive rate", 1, line = 2.4, cex = 0.9)
mtext("true positive rate", 2, line = 3.0, cex = 0.9)
abline(0, 1, col = GRY, lty = 3)
o <- th[order(th$fpr, th$tpr), ]
lines(o$fpr, o$tpr, col = ACC2, lwd = 2.6)
i5 <- which.min(abs(th$t - 0.5))
points(th$fpr[i5], th$tpr[i5], pch = 19, col = WARN, cex = 1.2)
text(th$fpr[i5] + 0.03, th$tpr[i5] - 0.04, "threshold 0.5", col = WARN,
     adj = 0, cex = 0.74)
text(0.62, 0.22, paste0("AUC = ", p2(AUC)), cex = 0.86, col = ACC2)
par(op)

## ---- fig3-d3
# The ROC with a movable cut. Every point on this curve is the same model; what
# changes is which error the reader has decided to prefer. The confusion counts
# come from the same table the curve does, so the figure cannot disagree with
# itself.
o <- th[order(th$fpr, th$tpr), ]
rows <- paste0('[', o$t, ',', o$fpr, ',', o$tpr, ',', o$tp, ',', o$fp, ',',
               o$fn, ',', o$tn, ']', collapse = ",")
cat(paste0('
<div id="roc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const T=[', rows, '];
const AUC=', AUC, ';
const ACC="', ACC2, '", WARN="', WARN, '", GRY="', GRY, '";
const W=770,H=440,M={t:18,r:250,b:54,l:64};
const box=d3.select("#roc");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,1]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format(".0%")).ticks(5));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d3.format(".0%")).ticks(5));
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(1)).attr("y2",y(1))
  .attr("stroke",GRY).attr("stroke-dasharray","3 3");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("false positive rate \\u2014 white, Hispanic, Asian and AIAN voters called Black");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("true positive rate \\u2014 Black voters found");
svg.append("path").attr("fill","none").attr("stroke",ACC).attr("stroke-width",2.6)
  .attr("d",d3.line().x(d=>x(d[1])).y(d=>y(d[2]))(T));
svg.append("text").attr("x",x(0.55)).attr("y",y(0.22)).attr("font-size","13px")
  .attr("fill",ACC).text("AUC = "+AUC.toFixed(3));
const dot=svg.append("circle").attr("r",6).attr("fill",WARN);
const rd=svg.append("g").attr("transform","translate("+(W-M.r+18)+","+(M.t+18)+")");
const lines=["cut","found","missed","wrongly called","correctly passed","precision"]
  .map((L,i)=>rd.append("text").attr("x",0).attr("y",i*22).attr("font-size","11.5px")
    .attr("fill","currentColor"));
const lab=bar.append("span").attr("style","color:#4E5A63");
const sl=bar.append("input").attr("type","range").attr("min","0")
  .attr("max",String(T.length-1)).attr("step","1")
  .attr("value",String(T.reduce((b,d,i)=>Math.abs(d[0]-0.5)<Math.abs(T[b][0]-0.5)?i:b,0)))
  .attr("style","flex:1;max-width:300px;accent-color:#1C4C5C");
const fmt=d3.format(",");
function draw(){
  const d=T[+sl.property("value")];
  dot.attr("cx",x(d[1])).attr("cy",y(d[2]));
  const prec=d[3]+d[4]>0?100*d[3]/(d[3]+d[4]):0;
  lines[0].text("call it Black when P \\u2265 "+d[0].toFixed(3));
  lines[1].text("found: "+fmt(d[3])+" Black voters");
  lines[2].text("missed: "+fmt(d[5]));
  lines[3].text("wrongly called Black: "+fmt(d[4]));
  lines[4].text("correctly passed over: "+fmt(d[6]));
  lines[5].text("precision: "+prec.toFixed(1)+"%");
  lab.text("threshold");
}
sl.on("input",draw); draw();
})();
</script>'))
