# overplotting-code.R -- chunk bodies for overplotting-brief.Rmd
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

gr <- read.csv("data/derived/grid.csv",     stringsAsFactors = FALSE)
hx <- read.csv("data/derived/hex.csv",      stringsAsFactors = FALSE)
ct <- read.csv("data/derived/contour.csv",  stringsAsFactors = FALSE)
mg <- read.csv("data/derived/marginal.csv", stringsAsFactors = FALSE)
cn <- read.csv("data/derived/counts.csv",   stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",    stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("contour.csv", "counts.csv", "facts.csv", "grid.csv", "hex.csv", "marginal.csv"))

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

NBLK <- fn("blocks"); NEMP <- fn("empty"); PEMP <- fn("pct_empty")
NPT  <- fn("points"); NPIX <- fn("pixels"); NHID <- fn("hidden")
PHID <- fn("pct_hidden"); MAXP <- fn("maxpix")
STATE <- fn("state_share"); MEDP <- fn("med_pop"); MEANP <- fn("mean_pop")
LO5 <- fn("lo5"); PLO5 <- fn("pct_lo5")
HI95 <- fn("hi95"); PHI95 <- fn("pct_hi95")
MID <- fn("mid"); PMID <- fn("pct_mid")
XHI <- fn("xhi")
RC <- fn("hex_coarse"); RF <- fn("hex_fine")
NHC <- fn("n_hex_coarse"); NHF <- fn("n_hex_fine")

GW <- 700; GH <- 450
# grid cell -> data units
gx2l <- function(gx) (gx - 0.5) / GW * XHI          # log10 population
gy2s <- function(gy) (gy - 0.5) / GH * 100          # per cent Black
# the x axis is log population, rescaled to 0-100 so the hexes and the
# contours share one coordinate system with the scatter
POPTICK <- c(1, 10, 100, 1000)
XT <- 100 * log10(POPTICK) / XHI

INK <- "#12181D"; ACC <- "#1C4C5C"; WARN <- "#C41230"; GRY <- "#8A8F94"
RAMP <- c("#F2F6F7", "#CBE0E6", "#8FBFCD", "#4E97AC", "#256E86", "#0F4557")

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
data.frame(
  Column = c("GEOID20", "pop", "black_any", "vap"),
  What_it_holds = c(
    "the block's 15-character census identifier",
    "everybody living in the block on Census Day 2020",
    "of those, everybody who marked Black alone or in combination",
    "the voting-age population"),
  Measurement = c("categorical", "count", "count", "count"))

## ---- counts
data.frame(
  Blocks = n(cn$value),
  Which = cn$what)

## ---- fig1-static
op <- par(mar = c(4.0, 4.4, 1.2, 1.0), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
axis(1, at = XT, labels = n(POPTICK), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("people living in the block (log scale)", 1, line = 2.4, cex = 0.9)
mtext("Black share of the block", 2, line = 3.0, cex = 0.9)
rect(0, 0, 100, 100, col = "#FAFBFB", border = NA)
points(100 * gx2l(gr$gx) / XHI, gy2s(gr$gy), pch = 16, cex = 0.26,
       col = paste0(INK, "66"))
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The scatter, quantised to the grid it is drawn on. That is not a sample: at
# this size, 165,333 points occupy 19,871 pixels, so drawing one dot per
# occupied pixel puts the same ink in the same places. What it buys is the
# COUNT under each dot, which is what the opacity control below is reading.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0("[", gr$gx, ",", gr$gy, ",", gr$n, "]", collapse = ",")
cat(paste0('
<div id="sc" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const G=[', rows, '];
const GW=', GW, ', GH=', GH, ', XHI=', XHI, ', NPT=', NPT, ';
const INK="', INK, '", WARN="', WARN, '";
const W=770,H=470,M={t:16,r:20,b:56,l:62};
const box=d3.select("#sc");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:12px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
const POPT=[1,10,100,1000];
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(POPT.map(p=>100*Math.log10(p)/XHI))
          .tickFormat((d,i)=>d3.format(",")(POPT[i])));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("people living in the block (log scale)");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("Black share of the block");
// An explicit panel. This figure is about how much ink lands where, so it
// cannot be drawn on whatever ground the reader happens to have: dark ink on
// a dark page is not a subtle problem, it is an invisible figure.
svg.insert("rect",":first-child").attr("x",M.l).attr("y",M.t)
  .attr("width",W-M.r-M.l).attr("height",H-M.b-M.t).attr("fill","#FAFBFB");
const g=svg.append("g");
const dots=g.selectAll("circle").data(G).join("circle")
  .attr("cx",d=>x((d[0]-0.5)/GW*100)).attr("cy",d=>y((d[1]-0.5)/GH*100))
  .attr("fill",INK);
const lab=bar.append("span").attr("style","color:#4E5A63");
bar.append("span").attr("style","color:#76838C").text("opacity");
const so=bar.append("input").attr("type","range").attr("min","2").attr("max","100")
  .attr("step","2").attr("value","100")
  .attr("style","flex:0 1 160px;accent-color:#1C4C5C");
bar.append("span").attr("style","color:#76838C").text("dot size");
const sr=bar.append("input").attr("type","range").attr("min","4").attr("max","30")
  .attr("step","1").attr("value","16")
  .attr("style","flex:0 1 160px;accent-color:#1C4C5C");
function draw(){
  const o=+so.property("value")/100, r=+sr.property("value")/10;
  dots.attr("r",r).attr("fill-opacity",o);
  lab.text("each dot is one pixel of the plot area");
}
so.on("input",draw); sr.on("input",draw); draw();
})();
</script>'))

## ---- quant
data.frame(
  Blocks_with = c("1–5 people", "10 or fewer", "26 or fewer (the median)"),
  How_many = c(n(fn("n_le5")), n(fn("n_le10")), n(fn("n_lemed"))),
  Share_of_all = paste0(p1(c(100 * fn("n_le5") / NPT, fn("pct_le10"),
                             fn("pct_lemed"))), "%"),
  Distinct_shares_available = c(n(fn("shares_le5")), "66", "hundreds"))

## ---- fig2-static
op <- par(mar = c(4.0, 4.4, 1.2, 1.0), mgp = c(2.6, 0.7, 0))
z <- hx[hx$r == 3.2, ]
plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
axis(1, at = XT, labels = n(POPTICK), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("people living in the block (log scale)", 1, line = 2.4, cex = 0.9)
mtext("Black share of the block", 2, line = 3.0, cex = 0.9)
rect(0, 0, 100, 100, col = "#FAFBFB", border = NA)
br <- colorRampPalette(RAMP)(100)
cl <- br[pmax(1, ceiling(100 * sqrt(z$n) / sqrt(max(z$n))))]
r  <- 3.2
for (i in seq_len(nrow(z))) {
  a <- seq(pi / 6, 2 * pi + pi / 6, length.out = 7)
  polygon(z$cx[i] + r * cos(a), z$cy[i] + r * sin(a), col = cl[i], border = NA)
}
par(op)

## ---- fig2-d3
# Three bin sizes, computed in build-data.R because d3 does not ship a hexbin
# and this book does not load a second library for one shape. Each size
# conserves the point total exactly; the build checks it.
mkhex <- function(r) {
  z <- hx[hx$r == r, ]
  paste0('{r:', r, ',n:', nrow(z), ',c:[',
         paste0("[", z$cx, ",", z$cy, ",", z$n, "]", collapse = ","), ']}')
}
cat(paste0('
<div id="hb" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const H=[', paste(vapply(sort(unique(hx$r), decreasing = TRUE), mkhex,
                         character(1)), collapse = ","), '];
const XHI=', XHI, ';
const RAMP=', paste0('["', paste(RAMP, collapse = '","'), '"]'), ';
const W=770,H2=470,M={t:16,r:20,b:56,l:62};
const box=d3.select("#hb");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H2)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H2-M.b,M.t]);
const POPT=[1,10,100,1000];
svg.append("g").attr("transform","translate(0,"+(H2-M.b)+")")
  .call(d3.axisBottom(x).tickValues(POPT.map(p=>100*Math.log10(p)/XHI))
          .tickFormat((d,i)=>d3.format(",")(POPT[i])));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H2-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("people living in the block (log scale)");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H2-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("Black share of the block");
// the same panel as Figure 1, so the two are read on the same ground
svg.insert("rect",":first-child").attr("x",M.l).attr("y",M.t)
  .attr("width",W-M.r-M.l).attr("height",H2-M.b-M.t).attr("fill","#FAFBFB");
const gh=svg.append("g");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const lab=bar.append("span").attr("style","color:#4E5A63");
const sl=bar.append("input").attr("type","range").attr("min","0").attr("max","2")
  .attr("step","1").attr("value","1")
  .attr("style","flex:0 1 200px;accent-color:#1C4C5C");
// a hexagon path in SCREEN units, from a radius given in data units
function hexPath(rx,ry){
  let p="";
  for(let i=0;i<6;i++){
    const a=Math.PI/6+i*Math.PI/3;
    p+=(i?"L":"M")+(rx*Math.cos(a)).toFixed(2)+","+(-ry*Math.sin(a)).toFixed(2);
  }
  return p+"Z";
}
function draw(){
  const hh=H[+sl.property("value")];
  const mx=d3.max(hh.c,d=>d[2]);
  const col=d3.scaleSqrt().domain([1,mx]).range([0,RAMP.length-1]).clamp(true);
  const rx=x(hh.r)-x(0), ry=y(0)-y(hh.r);
  gh.selectAll("path").data(hh.c).join("path")
    .attr("transform",d=>"translate("+x(d[0])+","+y(d[1])+")")
    .attr("d",hexPath(rx,ry))
    .attr("fill",d=>RAMP[Math.round(col(d[2]))])
    .on("mousemove",function(e,d){
      const r=box.node().getBoundingClientRect();
      const pop=Math.pow(10,d[0]/100*XHI);
      tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
         .style("top",(e.clientY-r.top-8)+"px")
         .html("<b>"+d3.format(",")(d[2])+" blocks</b><br>around "+
               d3.format(",.0f")(pop)+" people<br>around "+
               d[1].toFixed(0)+"% Black");
    })
    .on("mouseleave",function(){tip.style("opacity",0);});
  lab.text("bin radius "+hh.r+" \\u2014 "+hh.n+" hexagons");
}
sl.on("input",draw); draw();
})();
</script>'))

## ---- fig3-static
op <- par(mar = c(4.0, 4.4, 1.2, 1.0), mgp = c(2.6, 0.7, 0))
mid <- (mg$lo + mg$hi) / 2
plot(NA, xlim = c(0, 100), ylim = c(0, max(mg$n) * 1.05), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Black share of the block", 1, line = 2.4, cex = 0.9)
mtext("blocks", 2, line = 3.2, cex = 0.9)
rect(mg$lo, 0, mg$hi, mg$n, col = ACC, border = "white", lwd = 0.4)
abline(v = STATE, col = WARN, lwd = 2, lty = 2)
text(STATE + 1.5, max(mg$n) * 0.92,
     paste0("Georgia is ", p1(STATE), "% Black"), col = WARN, adj = 0,
     cex = 0.74)
par(op)

## ---- fig3-d3
# The composition axis on its own. The state figure is drawn as a line through
# it because the whole point is where that line falls relative to the mass.
rows <- paste0("[", mg$lo, ",", mg$hi, ",", mg$n, "]", collapse = ",")
cat(paste0('
<div id="mg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const M2=[', rows, '];
const STATE=', STATE, ', ACC="', ACC, '", WARN="', WARN, '";
const W=770,H=380,M={t:20,r:24,b:56,l:66};
const box=d3.select("#mg");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(M2,d=>d[2])*1.05]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(5));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(5).tickFormat(d3.format(",")));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Black share of the block");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("blocks");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.selectAll("rect.b").data(M2).join("rect").attr("class","b")
  .attr("x",d=>x(d[0])).attr("width",d=>x(d[1])-x(d[0])-0.5)
  .attr("y",d=>y(d[2])).attr("height",d=>y(0)-y(d[2])).attr("fill",ACC)
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d[0]+"\\u2013"+d[1]+"% Black</b><br>"+
             d3.format(",")(d[2])+" blocks");
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
svg.append("line").attr("x1",x(STATE)).attr("x2",x(STATE)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke",WARN).attr("stroke-width",2)
  .attr("stroke-dasharray","5 4");
svg.append("text").attr("x",x(STATE)+7).attr("y",M.t+14).attr("font-size","11.5px")
  .attr("fill",WARN).text("Georgia is "+STATE.toFixed(1)+"% Black");
})();
</script>'))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
cat('<style>
#hb text[fill="currentcolor" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
</style>')
