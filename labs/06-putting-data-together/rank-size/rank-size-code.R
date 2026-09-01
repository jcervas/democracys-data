# rank-size-code.R -- chunk bodies for rank-size-brief.Rmd
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

rk <- read.csv("data/derived/ranks.csv",   stringsAsFactors = FALSE)
su <- read.csv("data/derived/summary.csv", stringsAsFactors = FALSE)
ft <- read.csv("data/derived/fits.csv",    stringsAsFactors = FALSE)
zp <- read.csv("data/derived/zipf.csv",    stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",   stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
p3 <- function(x) formatC(as.numeric(x), format = "f", digits = 3)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
# dollars, and the cents survive: the smallest total in the FEC receipts file
# is one cent, which n() would round to $0 and quietly turn into a different
# claim
dl <- function(x) {
  x <- as.numeric(x)
  ifelse(abs(x) < 1, paste0("$", formatC(x, format = "f", digits = 2)),
         paste0("$", n(x)))
}
mn <- function(x) paste0("$", p1(as.numeric(x) / 1e6), "m")

# the display order: three counts of people or readers, then three of money
SER <- c("surnames", "counties", "pageviews",
         "receipts", "committees", "targets")
su  <- su[match(SER, su$series), ]
COL <- c(surnames = "#1C4C5C", counties = "#4E9DB5", pageviews = "#4A7C3F",
         receipts = "#C41230", committees = "#D98324", targets = "#7A5C8F")
GRY <- "#8A8F94"; ACC <- "#1C4C5C"; WARN <- "#C41230"

rk$mx  <- su$max[match(rk$series, su$series)]
rk$rel <- rk$value / rk$mx

NS <- fn("series"); NV <- fn("values")
STRL <- f("straight_label"); STRR <- fn("straight_r2_all")
STRB <- fn("straight_slope_all")
BNTL <- f("bent_label"); BNTRA <- fn("bent_r2_all"); BNTRT <- fn("bent_r2_top10")
BNTBA <- fn("bent_slope_all"); BNTBT <- fn("bent_slope_top10")
MINR10 <- fn("min_r2_top10"); MINRA <- fn("min_r2_all")
WL <- f("worst_label"); WRAT <- fn("worst_ratio"); WMED <- fn("worst_median")
WMEAN <- fn("worst_mean"); WN <- fn("worst_n"); WAM <- fn("worst_above_mean")
WAMP <- fn("worst_above_mean_pct")
RZ <- fn("receipts_zero"); RN <- fn("receipts_n")
RTOP <- f("receipts_top"); RMAX <- fn("receipts_max")
RBOT <- f("receipts_bottom"); RMIN <- fn("receipts_min")
CBOT <- f("committees_bottom"); CMIN <- fn("committees_min")
RESW <- f("residual_who"); RESV <- fn("residual_value")
RESS <- fn("residual_share")
SMAX <- fn("surnames_max"); SMED <- fn("surnames_median"); SN <- fn("surnames_n")
CTOP <- f("counties_top"); CMAX <- fn("counties_max")
AMLO <- fn("min_above_mean_pct"); AMHI <- fn("max_above_mean_pct")
T1LO <- fn("min_top1_share"); T1HI <- fn("max_top1_share")
GLO <- fn("min_gini"); GHI <- fn("max_gini")

# a fit read out of fits.csv by series and fraction
fit <- function(k, fr) {
  z <- ft[ft$series == k, ]
  z[which.min(abs(z$frac - fr)), ]
}

# Values the brief quotes inline, named here so the prose reads as prose
# rather than as a subsetting expression.
zg     <- function(k, r, col) zp[[col]][zp$series == k & zp$rank == r]
Z100S  <- zg("surnames", 100, "who")
Z100SN <- zg("surnames", 100, "observed")
Z100SR <- zg("surnames", 100, "ratio")
Z100RN <- zg("receipts", 100, "observed")
Z100RR <- zg("receipts", 100, "ratio")
SL10   <- sapply(SER, function(k) fit(k, .1)$slope)
RECL   <- tolower(su$short[su$series == "receipts"])
STRLO  <- tolower(STRL)
RC01   <- fit("receipts", .01)$slope
RC1    <- fit("receipts", 1)$slope
SN01   <- fit("surnames", .01)$slope
SN1    <- fit("surnames", 1)$slope
RNSM   <- RN - round(0.1 * RN)

# strings on their way into a <script>: the FEC files carry apostrophes, and
# one committee name carries a doubled one
esc <- function(x) {
  x <- gsub("''", "'", x, fixed = TRUE)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("<", "\\u003c", x, fixed = TRUE)
  x <- gsub(">", "\\u003e", x, fixed = TRUE)
  x
}
jarr <- function(x) paste0("[", paste(x, collapse = ","), "]")
jstr <- function(x) paste0("[", paste0('"', esc(x), '"', collapse = ","), "]")

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
z <- rk[rk$series %in% c("surnames", "counties") & rk$rank %in% c(1, 10), ]
z <- z[order(match(z$series, SER), z$rank), ]
data.frame(
  Series = su$short[match(z$series, su$series)],
  Rank = z$rank,
  Value = n(z$value),
  What_it_is = z$who)

## ---- varmap
data.frame(
  Column = c("series", "rank", "value", "who"),
  What_it_holds = c(
    "which of the six quantities this row belongs to",
    "position in that quantity's sorted list; 1 is the largest",
    "the quantity itself — people, dollars or views",
    "the surname, county, candidate, committee or article-day it belongs to"),
  Measurement = c("categorical", "count", "continuous", "identifier"))

## ---- fig1-static
op <- par(mar = c(3.8, 4.4, 1.2, 7.4), mgp = c(2.5, 0.6, 0))
xr <- c(1, max(rk$rank)); yr <- range(rk$rel)
plot(NA, xlim = xr, ylim = yr, log = "xy", axes = FALSE,
     xlab = "Rank, largest first", ylab = "")
xt <- 10^(0:5)
axis(1, at = xt, labels = format(xt, big.mark = ",", scientific = FALSE,
                                 trim = TRUE), cex.axis = 0.76,
     lwd = 0, lwd.ticks = 1)
yt <- 10^seq(-12, 0, by = 2)
yt <- yt[yt >= yr[1] / 3 & yt <= 1]
axis(2, at = yt, labels = parse(text = paste0("10^", log10(yt))), las = 1,
     cex.axis = 0.76, lwd = 0, lwd.ticks = 1)
mtext("Size, as a share of the largest in its own series", 2, line = 3,
      cex = 0.8)
abline(h = yt, col = "#00000010")
for (k in SER) {
  z <- rk[rk$series == k, ]; z <- z[order(z$rank), ]
  lines(z$rank, z$rel, col = COL[k], lwd = 1.7)
}
for (i in seq_along(SER)) {
  z <- rk[rk$series == SER[i], ]
  text(max(rk$rank) * 1.35, 10^(par("usr")[3] + diff(par("usr")[3:4]) *
                                (0.97 - 0.075 * (i - 1))),
       su$short[su$series == SER[i]], adj = 0, cex = 0.62,
       col = COL[SER[i]], xpd = NA)
}
mtext("Six quantities, each divided by its own largest value", 3, line = -0.2,
      cex = 0.8, adj = 0)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Six rank-size curves on shared log axes, with a switch to linear axes.
#
# Each series is divided by its own largest value so that all six start at
# (1, 1). On log axes dividing by a constant is a vertical SHIFT and nothing
# else, so the slope of every curve -- the only thing being compared here --
# survives the normalisation untouched.
#
# The linear button is the pedagogical control, not a convenience: it draws
# what a spreadsheet's default would draw.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
pay <- vapply(SER, function(k) {
  z <- rk[rk$series == k, ]; z <- z[order(z$rank), ]
  s <- su[su$series == k, ]
  paste0('{k:"', k, '",l:"', esc(s$short), '",u:"', s$unit,
         '",c:"', COL[k], '",n:', s$n, ',mx:', s$max,
         ',r:', jarr(z$rank), ',v:', jarr(z$value), ',w:', jstr(z$who), '}')
}, character(1))
cat(paste0('
<div id="rs1" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', paste(pay, collapse = ","), '];
const W=770,H=560,M={t:16,r:172,b:46,l:64};
const ispow=d=>Math.abs(Math.log10(d)-Math.round(Math.log10(d)))<1e-6;
// The exponent is drawn as a raised tspan rather than with the Unicode
// superscript characters: U+207B, superscript minus, is missing from enough
// screen fonts that half of these labels would read 10 4 for ten to the
// minus four.
const powtext=function(sel){
  sel.selectAll(".tick text").each(function(d){
    const e=Math.round(Math.log10(d));
    const t=d3.select(this).text(null);
    if(e===0){t.text("1");return;}
    t.append("tspan").text("10");
    t.append("tspan").attr("dy","-4.5").attr("font-size","76%")
     .text(String(e));
  });
};
const box=d3.select("#rs1");
const bar=box.append("div").attr("style","margin:0 0 8px;display:flex;'
, 'align-items:center;gap:12px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
S.forEach(function(s){ s.y=s.v.map(q=>q/s.mx); });
const NMAX=d3.max(S,s=>s.n), YMIN=d3.min(S,s=>d3.min(s.y));
let logmode=true;
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")");
const gy=svg.append("g").attr("transform","translate("+M.l+",0)");
const gg=svg.append("g");
const gl=svg.append("g");
const key=svg.append("g").attr("transform","translate("+(W-M.r+14)+","+(M.t+12)+")");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;'
, 'border-radius:3px;padding:6px 8px;font:11.5px inherit;max-width:260px;'
, 'box-shadow:0 1px 4px rgba(0,0,0,.14)");
const cm=d3.format(",");
const fmtv=(s,q)=>(s.u==="dollars"?"$":"")+cm(Math.round(q))+
                  (s.u==="dollars"?"":" "+s.u);
let x,y;
function scales(){
  x=logmode?d3.scaleLog().domain([1,NMAX]).range([M.l,W-M.r])
           :d3.scaleLinear().domain([1,NMAX]).range([M.l,W-M.r]);
  y=logmode?d3.scaleLog().domain([YMIN,1]).range([H-M.b,M.t])
           :d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
}
const line=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
const pts=[];
S.forEach(function(s,si){ s.r.forEach(function(r,i){
  pts.push({s:si,i:i,r:r,y:s.y[i]}); }); });
function draw(anim){
  scales();
  const tx=anim?gx.transition().duration(340):gx;
  const ty=anim?gy.transition().duration(340):gy;
  // an explicit decade grid: the log-scale tickFormat in d3 blanks most
  // labels when given a count, leaving an axis of unlabelled ticks
  const xd=[1,10,100,1000,10000,100000,1000000].filter(d=>d<=NMAX);
  tx.call(logmode?d3.axisBottom(x).tickValues(xd).tickFormat(d3.format("~s"))
                 :d3.axisBottom(x).ticks(6).tickFormat(d3.format("~s")));
  const yd=logmode?y.ticks(12).filter(ispow):y.ticks(6);
  ty.call(logmode?d3.axisLeft(y).tickValues(yd)
                 :d3.axisLeft(y).ticks(6).tickFormat(d3.format(".1f")));
  if(logmode) gy.call(powtext);
  gg.selectAll("line").data(yd).join("line")
    .attr("x1",M.l).attr("x2",W-M.r).attr("y1",d=>y(d)).attr("y2",d=>y(d))
    .attr("stroke","currentColor").attr("stroke-opacity",0.07);
  const sel=gl.selectAll("path").data(S).join("path")
    .attr("fill","none").attr("stroke",s=>s.c).attr("stroke-width",1.8);
  (anim?sel.transition().duration(340):sel)
    .attr("d",s=>line(s.r.map((r,i)=>[r,s.y[i]])));
}
const rows=key.selectAll("g").data(S).join("g")
  .attr("transform",(d,i)=>"translate(0,"+i*30+")");
rows.append("line").attr("x1",0).attr("x2",14).attr("y1",-4).attr("y2",-4)
  .attr("stroke",d=>d.c).attr("stroke-width",2.4);
rows.append("text").attr("x",20).attr("font-size","11px")
  .attr("fill","currentColor").text(d=>d.l);
rows.append("text").attr("x",20).attr("y",13).attr("font-size","9.5px")
  .attr("fill","#8A8F94").text(d=>cm(d.n)+" of them");
svg.append("text").attr("x",M.l).attr("y",H-10).attr("font-size","11px")
  .attr("fill","#8A8F94").text("Rank, largest first");
svg.append("text").attr("transform","translate(14,"+((H-M.b+M.t)/2)+") rotate(-90)")
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#8A8F94")
  .text("Size, as a share of the largest in its series");
const btn=bar.append("button")
  .attr("style","padding:4px 11px;border:1px solid #CBD3D8;border-radius:3px;'
, 'cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text("draw it on ordinary axes");
const note=bar.append("span").attr("style","color:#8A8F94")
  .text("both axes logarithmic");
btn.on("click",function(){
  logmode=!logmode;
  d3.select(this).text(logmode?"draw it on ordinary axes":"put the logarithms back");
  note.text(logmode?"both axes logarithmic":"both axes linear \\u2014 the way a spreadsheet would");
  draw(true);
});
const hit=svg.append("rect").attr("x",M.l).attr("y",M.t)
  .attr("width",W-M.r-M.l).attr("height",H-M.b-M.t).attr("fill","transparent");
const dot=svg.append("circle").attr("r",4).attr("fill","none")
  .attr("stroke-width",2).attr("opacity",0);
hit.on("mousemove",function(e){
  const p=d3.pointer(e,svg.node());
  let best=null,bd=1e9;
  pts.forEach(function(q){
    const dx=x(q.r)-p[0], dy=y(q.y)-p[1], d=dx*dx+dy*dy;
    if(d<bd){bd=d;best=q;}
  });
  if(!best||bd>900){tip.style("opacity",0);dot.attr("opacity",0);return;}
  const s=S[best.s];
  dot.attr("cx",x(best.r)).attr("cy",y(best.y)).attr("stroke",s.c)
     .attr("opacity",1);
  const rr=box.node().getBoundingClientRect();
  tip.style("opacity",1).style("left",(e.clientX-rr.left+14)+"px")
     .style("top",(e.clientY-rr.top-10)+"px")
     .html("<b>"+s.w[best.i]+"</b><br>rank "+cm(best.r)+" of "+cm(s.n)+
           " \\u00b7 "+s.l.toLowerCase()+"<br>"+fmtv(s,s.v[best.i]));
}).on("mouseleave",function(){tip.style("opacity",0);dot.attr("opacity",0);});
draw(false);
})();
</script>'))

## ---- zipftab
z <- zp[zp$series %in% c("surnames", "receipts"), ]
z <- z[order(match(z$series, SER), z$rank), ]
data.frame(
  Series = su$short[match(z$series, su$series)],
  Rank = n(z$rank),
  What_is_there = z$who,
  Observed = ifelse(z$series == "receipts", dl(z$observed), n(z$observed)),
  Zipf_predicts = ifelse(z$series == "receipts", dl(z$zipf), n(z$zipf)),
  Ratio = p2(z$ratio))

## ---- meantab
z <- data.frame(
  Series = su$short,
  Things = n(su$n),
  Median = ifelse(su$unit == "dollars", dl(su$median), n(su$median)),
  Mean = ifelse(su$unit == "dollars", dl(su$mean), n(su$mean)),
  Mean_over_median = p1(su$mean_over_median),
  Above_the_mean = paste0(p1(su$above_mean_pct), "%"),
  x = paste0(p1(su$top1_share), "%"), check.names = FALSE)
names(z)[7] <- "Top 1% holds"
z

## ---- fig2-static
op <- par(mfrow = c(2, 3), mar = c(3.4, 3.6, 2.0, 0.8), mgp = c(2.1, 0.55, 0))
for (k in SER) {
  z <- rk[rk$series == k, ]; z <- z[order(z$rank), ]
  s <- su[su$series == k, ]
  a10 <- fit(k, 0.10); aal <- fit(k, 1.00)
  plot(z$rank, z$value, log = "xy", pch = 16, cex = 0.42, col = "#00000070",
       axes = FALSE, xlab = "Rank", ylab = "")
  box(col = "#CBD3D8")
  xt <- 10^(0:5); xt <- xt[xt <= s$n * 1.3]
  axis(1, at = xt, labels = format(xt, big.mark = ",", scientific = FALSE,
                                   trim = TRUE), cex.axis = 0.66,
       lwd = 0, lwd.ticks = 1)
  yt <- 10^seq(-2, 10, by = 2)
  yt <- yt[yt >= min(z$value) / 3 & yt <= max(z$value) * 3]
  axis(2, at = yt, labels = parse(text = paste0("10^", log10(yt))), las = 1,
       cex.axis = 0.66, lwd = 0, lwd.ticks = 1)
  xs <- 10^seq(0, log10(s$n), length.out = 60)
  # black for the fit to everything, the series colour dashed for the top 10%.
  # Not the series colour for both: one of the six series IS red.
  lines(xs, 10^(aal$intercept + aal$slope * log10(xs)), col = "#12181D",
        lwd = 1.5)
  lines(xs, 10^(a10$intercept + a10$slope * log10(xs)), col = COL[k],
        lwd = 1.8, lty = 2)
  mtext(ifelse(s$unit == "dollars", "Dollars", "Size"), 2, line = 2.3,
        cex = 0.55)
  mtext(s$short, 3, line = 0.7, cex = 0.68, col = COL[k], adj = 0)
  mtext(paste0("all: ", p2(aal$slope), "  R2 ", p3(aal$r2),
               "   top 10%: ", p2(a10$slope), "  R2 ", p3(a10$r2)),
        3, line = -0.1, cex = 0.52, col = "#4E5A63", adj = 0)
}
par(op)

## ---- fig2-d3
# ---------------------------------------------------------------------------
# The chapter's argument, made operable: one series at a time, and a slider
# for how much of it the straight line is fitted to.
#
# The FITS ARE NOT COMPUTED HERE. build-data.R fits log10(value) against
# log10(rank) by least squares on ALL n values at each of 44 cut-offs and
# writes the intercept, slope and R-squared; this chunk only draws them. The
# scatter behind the line is the thinned set, so the line is fitted to more
# points than are drawn -- which is the honest way round.
#
# The strip along the bottom shows the same two numbers as a function of the
# cut-off, so the slider's effect is visible before you move it.
# ---------------------------------------------------------------------------
dpay <- vapply(SER, function(k) {
  z <- rk[rk$series == k, ]; z <- z[order(z$rank), ]
  s <- su[su$series == k, ]
  g <- ft[ft$series == k, ]; g <- g[order(g$frac), ]
  paste0('{k:"', k, '",l:"', esc(s$short), '",u:"', s$unit,
         '",c:"', COL[k], '",n:', s$n,
         ',r:', jarr(z$rank), ',v:', jarr(z$value), ',w:', jstr(z$who),
         ',f:', jarr(g$frac), ',kk:', jarr(g$k),
         ',a:', jarr(g$intercept), ',b:', jarr(g$slope),
         ',q:', jarr(g$r2), '}')
}, character(1))
cat(paste0('
<div id="rs2" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[', paste(dpay, collapse = ","), '];
const W=770,H=512,M={t:18,r:22,l:64};
const SH=96, SY=H-SH-22;       // top of the slope / R-squared strip
const PB=SY-48;                // baseline of the main panel, clear of it
const box=d3.select("#rs2");
const bar=box.append("div").attr("style","margin:0 0 8px;display:flex;'
, 'align-items:center;gap:6px;font:12px inherit;flex-wrap:wrap");
const bar2=box.append("div").attr("style","margin:0 0 6px;display:flex;'
, 'align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cm=d3.format(",");
// every power of ten inside a range, with at most eight labels kept: a log
// axis given only a count leaves most of its ticks unlabelled
// d3.format("~s") renders 0.01 as "10m" for milli, which on an axis that also
// carries "100M" for a hundred million is worse than useless. Money also wants
// B for billion rather than the SI G.
const sfmt=function(d){
  if(d>=1e9) return (d/1e9)+"B";
  if(d>=1e6) return (d/1e6)+"M";
  if(d>=1e3) return (d/1e3)+"k";
  return String(d);
};
const dec=function(lo,hi){
  const a=Math.ceil(Math.log10(lo)-1e-9), b=Math.floor(Math.log10(hi)+1e-9);
  let v=d3.range(a,b+1), step=Math.ceil(v.length/8);
  return v.filter((d,i)=>i%step===0).map(e=>Math.pow(10,e));
};
let cur=0, fi=S[0].f.indexOf(0.1)>=0?S[0].f.indexOf(0.1):20;
const btns=bar.selectAll("button").data(S).join("button")
  .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;'
, 'cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text(d=>d.l)
  .on("click",function(e,d){cur=S.indexOf(d);paint();draw(true);});
bar2.append("span").attr("style","color:#8A8F94").text("fit the line to the top");
const sl=bar2.append("input").attr("type","range").attr("min","0")
  .attr("max",String(S[0].f.length-1)).attr("step","1")
  .attr("value",String(fi))
  .attr("style","flex:0 1 230px;accent-color:#1C4C5C");
const read=bar2.append("span").attr("style","color:#4E5A63;font-variant-numeric:tabular-nums");
const gx=svg.append("g"), gy=svg.append("g"), gg=svg.append("g");
const gp=svg.append("g"), gline=svg.append("g"), gs=svg.append("g");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;'
, 'border-radius:3px;padding:6px 8px;font:11.5px inherit;max-width:260px;'
, 'box-shadow:0 1px 4px rgba(0,0,0,.14)");
let x,y;
function paint(){
  btns.attr("style",(d,i)=>"padding:3px 9px;border:1px solid "+
    (i===cur?d.c:"#CBD3D8")+";border-radius:3px;cursor:pointer;'
, 'font:11.5px inherit;background:"+(i===cur?d.c:"#FAFBFB")+
    ";color:"+(i===cur?"#FFFFFF":"#12181D"));
}
function draw(anim){
  const s=S[cur], k=s.kk[fi];
  x=d3.scaleLog().domain([1,s.n]).range([M.l,W-M.r]);
  y=d3.scaleLog().domain([d3.min(s.v),d3.max(s.v)]).range([PB,M.t]);
  gx.attr("transform","translate(0,"+(PB)+")")
    .call(d3.axisBottom(x).tickValues(dec(1,s.n)).tickFormat(sfmt));
  gy.attr("transform","translate("+M.l+",0)")
    .call(d3.axisLeft(y).tickValues(dec(d3.min(s.v),d3.max(s.v)))
            .tickFormat(d=>(s.u==="dollars"?"$":"")+sfmt(d)));
  gg.selectAll("line").data(dec(d3.min(s.v),d3.max(s.v))).join("line")
    .attr("x1",M.l).attr("x2",W-M.r).attr("y1",d=>y(d)).attr("y2",d=>y(d))
    .attr("stroke","currentColor").attr("stroke-opacity",0.07);
  const dots=gp.selectAll("circle").data(s.r.map((r,i)=>({r:r,v:s.v[i],i:i})),
                                         d=>d.i);
  dots.join(e=>e.append("circle").attr("r",2.1),u=>u,x=>x.remove())
    .attr("cx",d=>x(d.r)).attr("cy",d=>y(d.v))
    .attr("fill",s.c).attr("fill-opacity",d=>d.r<=k?0.62:0.13);
  const a=s.a[fi], b=s.b[fi];
  const x1=1, x2=k, x3=s.n;
  const yy=q=>Math.pow(10,a+b*Math.log10(q));
  const L=[[x1,x2],[x2,x3]];
  // the fitted segment is currentColor, not a fixed hue: the six series
  // include a red one, and a red line over red points is not a line
  const seg=gline.selectAll("line").data(L).join("line")
    .attr("stroke",(d,i)=>i?"#8A8F94":"currentColor")
    .attr("stroke-opacity",(d,i)=>i?0.9:0.85)
    .attr("stroke-width",(d,i)=>i?1.2:2.2)
    .attr("stroke-dasharray",(d,i)=>i?"4 3":null);
  (anim?seg.transition().duration(300):seg)
    .attr("x1",d=>x(d[0])).attr("x2",d=>x(Math.max(d[1],d[0]+1e-9)))
    .attr("y1",d=>y(yy(d[0]))).attr("y2",d=>y(yy(Math.max(d[1],d[0]+1e-9))));
  read.html("<b>"+d3.format(".1%")(s.f[fi])+"</b> \\u2014 "+cm(k)+" of "+
            cm(s.n)+" \\u00b7 slope <b>"+d3.format(".2f")(b)+
            "</b> \\u00b7 R\\u00b2 <b>"+d3.format(".3f")(s.q[fi])+"</b>");
  // --- the strip: slope and R-squared against how much was fitted ---------
  gs.selectAll("*").remove();
  const panels=[{t:"slope",vals:s.b,dom:[d3.min(s.b)-0.15,Math.min(0,d3.max(s.b)+0.15)],fm:d3.format(".1f")},
                {t:"R\\u00b2",vals:s.q,dom:[Math.min(0.6,d3.min(s.q)-0.02),1],fm:d3.format(".2f")}];
  panels.forEach(function(P,pi){
    const px=M.l+pi*((W-M.r-M.l)/2+16), pw=(W-M.r-M.l)/2-16;
    const sx=d3.scaleLog().domain([s.f[0],1]).range([px,px+pw]);
    const sy=d3.scaleLinear().domain(P.dom).range([SY+SH-18,SY+6]);
    const g=gs.append("g");
    g.append("rect").attr("x",px).attr("y",SY+2).attr("width",pw)
     .attr("height",SH-16).attr("fill","currentColor").attr("fill-opacity",0.035);
    g.append("path").attr("fill","none").attr("stroke",s.c)
     .attr("stroke-width",1.6)
     .attr("d",d3.line().x((d,i)=>sx(s.f[i])).y(d=>sy(d))(P.vals));
    g.append("circle").attr("cx",sx(s.f[fi])).attr("cy",sy(P.vals[fi]))
     .attr("r",3.4).attr("fill","#C41230");
    g.append("text").attr("x",px).attr("y",SY-2).attr("font-size","10.5px")
     .attr("fill","#8A8F94").text(P.t+" vs. how much was fitted");
    g.append("g").attr("transform","translate(0,"+(SY+SH-18)+")")
     .call(d3.axisBottom(sx).tickValues([0.01,0.1,1])
             .tickFormat(d3.format(".0%")))
     .attr("font-size","9.5px");
    g.append("g").attr("transform","translate("+px+",0)")
     .call(d3.axisLeft(sy).ticks(3).tickFormat(P.fm)).attr("font-size","9.5px");
  });
  gp.selectAll("circle")
    .on("mousemove",function(e,d){
      const rr=box.node().getBoundingClientRect();
      tip.style("opacity",1).style("left",(e.clientX-rr.left+14)+"px")
         .style("top",(e.clientY-rr.top-10)+"px")
         .html("<b>"+s.w[d.i]+"</b><br>rank "+cm(d.r)+" of "+cm(s.n)+"<br>"+
               (s.u==="dollars"?"$":"")+cm(Math.round(d.v))+
               (s.u==="dollars"?"":" "+s.u));
    })
    .on("mouseleave",function(){tip.style("opacity",0);});
}
sl.on("input",function(){fi=+this.value;draw(false);});
svg.append("text").attr("x",W-M.r).attr("y",PB+32).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#8A8F94")
  .text("Rank, largest first \\u2014 both axes logarithmic");
svg.append("text").attr("transform","translate(14,"+((PB+M.t)/2)+") rotate(-90)")
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#8A8F94")
  .text("Size");
paint(); draw(false);
})();
</script>'))

## ---- fittab
z <- do.call(rbind, lapply(SER, function(k) {
  a <- fit(k, 0.10); b <- fit(k, 1.00)
  data.frame(Series = su$short[su$series == k],
             a = p2(a$slope), b = p3(a$r2),
             c = p2(b$slope), d = p3(b$r2),
             stringsAsFactors = FALSE, check.names = FALSE)
}))
names(z) <- c("Series", "Slope, top 10%", "R², top 10%",
              "Slope, all of it", "R², all of it")
z
