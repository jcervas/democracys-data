# partisan-economy-code.R -- chunk bodies for partisan-economy-brief.Rmd
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

nat  <- read.csv("data/derived/national.csv",  stringsAsFactors = FALSE)
own  <- read.csv("data/derived/personal.csv",  stringsAsFactors = FALSE)
grd  <- read.csv("data/derived/gradient.csv",  stringsAsFactors = FALSE)
wgt  <- read.csv("data/derived/weighting.csv", stringsAsFactors = FALSE)
ck   <- read.csv("data/derived/checks.csv",    stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)
pa <- function(x) formatC(abs(x), format = "f", digits = 1)

FIRST <- min(nat$year)
LAST  <- max(nat$year)
NYR   <- nrow(nat)

gp   <- function(y) nat$gap[nat$year == y]
gpo  <- function(y) own$gap[own$year == y]
G20  <- gp(2020); G24 <- gp(2024)

# The two narrowest gaps, and the two widest. Taken from the data rather than
# named, so a rebuild that moved them would move the sentence too.
NARROW <- nat$year[order(abs(nat$gap))][1:2]
WIDE   <- nat$year[order(-abs(nat$gap))][1]

both <- merge(nat[, c("year", "president", "gap")], own[, c("year", "gap")],
              by = "year", suffixes = c("_nat", "_own"))
SMALLER <- sum(abs(both$gap_own) < abs(both$gap_nat))
# The one year the personal question splits the parties wider than the
# national one. Derived, not named: it is also the year the national question
# runs out of room, and the two sections that discuss it both read it here.
EXC     <- both$year[abs(both$gap_own) >= abs(both$gap_nat)]
CD      <- nat$dem_worse[nat$year == EXC]
CR      <- nat$rep_worse[nat$year == EXC]

g7 <- function(y, i) grd$pct_worse[grd$year == y & grd$code == i]
SD20 <- g7(2020, 1); SR20 <- g7(2020, 7)
SD24 <- g7(2024, 1); SR24 <- g7(2024, 7)

WMAX <- max(abs(wgt$difference))

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

DEM <- "#2B5C8A"; REP <- "#A33B2A"; GRY <- "#8A8F94"
# The pale bands behind Figure 1. Hardcoded fills stay light on the dark page,
# which is why the text drawn ON them below is #707070 rather than #555 or
# #666: brief.css remaps that list of dark hexes to near-white in dark mode,
# and a remapped label on a band that did not move is unreadable.
BAND_R <- "#F4E9E6"; BAND_D <- "#E7EDF3"; BAND_INK <- "#707070"

## ---- question
cat(paste(readLines("data/raw/question.txt"), collapse = "\n"))

## ---- fig1-static
# Bands first, bars over them: the claim is that the sign of the bar agrees
# with the colour of the band it stands in, every time.
op <- par(mar = c(3.6, 4.2, 2.2, 0.8), mgp = c(2.6, 0.7, 0))
yl <- max(abs(nat$gap)) + 8
plot(NA, xlim = c(FIRST - 1.6, LAST + 1.6), ylim = c(-yl, yl),
     axes = FALSE, xlab = "", ylab = "")
runs <- rle(nat$president)
e <- cumsum(runs$lengths); s <- e - runs$lengths + 1
for (i in seq_along(runs$values)) {
  x0 <- nat$year[s[i]] - 1.1; x1 <- nat$year[e[i]] + 1.1
  rect(x0, -yl, x1, yl, col = if (runs$values[i] == "R") BAND_R else BAND_D,
       border = NA)
  text((x0 + x1) / 2, yl - 3,
       if (runs$values[i] == "R") "Republican president" else "Democratic president",
       cex = 0.6, col = BAND_INK)
}
rect(nat$year - 0.85, 0, nat$year + 0.85, nat$gap, border = NA,
     col = ifelse(nat$gap > 0, DEM, REP))
abline(h = 0, col = "#12181D", lwd = 1)
axis(1, at = seq(1980, 2024, 8), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(-60, 60, 20), las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Democrats minus Republicans,\n% saying the economy got worse", 2,
      line = 2.0, cex = 0.82)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The bars are the subtraction; the bands are who was president. On paper the
# reader has to trust that the two underlying shares are what the caption says
# they are. Here hovering gives both of them, plus how many people each rests
# on, so the bar can be taken apart.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('[', nat$year, ',"', nat$president, '",', nat$dem_worse, ',',
               nat$rep_worse, ',', nat$ind_worse, ',', nat$gap, ',',
               nat$n_dem, ',', nat$n_rep, ']', collapse = ",")
cat(paste0('
<div id="pgap" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const R=[', rows, '];
const D=R.map(r=>({y:r[0],p:r[1],dw:r[2],rw:r[3],iw:r[4],g:r[5],nd:r[6],nr:r[7]}));
const DEM="', DEM, '", REP="', REP, '";
const W=770,H=430,M={t:26,r:16,b:38,l:74};
const box=d3.select("#pgap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([', FIRST, '-1.6,', LAST, '+1.6]).range([M.l,W-M.r]);
const ymax=d3.max(D,d=>Math.abs(d.g))+8;
const y=d3.scaleLinear().domain([-ymax,ymax]).range([H-M.b,M.t]);
// Bands: one rectangle per unbroken run of the same party.
const runs=[];
D.forEach(function(d,i){
  if(i===0||d.p!==D[i-1].p) runs.push({p:d.p,a:d.y,b:d.y});
  else runs[runs.length-1].b=d.y;
});
svg.selectAll("rect.bd").data(runs).join("rect").attr("class","bd")
  .attr("x",d=>x(d.a-1.1)).attr("width",d=>x(d.b+1.1)-x(d.a-1.1))
  .attr("y",M.t).attr("height",H-M.b-M.t)
  .attr("fill",d=>d.p==="R"?"', BAND_R, '":"', BAND_D, '");
svg.selectAll("text.bl").data(runs).join("text").attr("class","bl")
  .attr("x",d=>(x(d.a-1.1)+x(d.b+1.1))/2).attr("y",M.t+13)
  .attr("text-anchor","middle").attr("font-size","10px")
  .attr("fill","', BAND_INK, '")
  .text(d=>(x(d.b+1.1)-x(d.a-1.1)<86)?(d.p==="R"?"R":"D"):
            (d.p==="R"?"Republican president":"Democratic president"));
const bw=Math.abs(x(1.7)-x(0));
svg.selectAll("rect.b").data(D).join("rect").attr("class","b")
  .attr("x",d=>x(d.y)-bw/2).attr("width",bw)
  .attr("y",d=>y(Math.max(0,d.g))).attr("height",d=>Math.abs(y(d.g)-y(0)))
  .attr("fill",d=>d.g>0?DEM:REP);
// Mid grey rather than currentColor or a dark literal. This line crosses the
// pale bands, which are pinned light in both themes, AND the bare paper in the
// years with no study. currentColor would vanish on the bands in dark mode and
// a dark literal vanishes on the paper there; #707070 reads on both. No text,
// so check-contrast.js cannot catch this one.
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#707070");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(6));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickValues([-60,-40,-20,0,20,40,60]).tickFormat(d=>d+" pts"));
const yl=svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#4E5A63");
["Democrats minus Republicans,","% saying the economy got worse"].forEach(function(t,i){
  yl.append("tspan").attr("x",-(M.t+(H-M.b))/2).attr("dy",i===0?"0":"1.15em").text(t);
});
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.selectAll("rect.b")
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+d.y+"</b> &middot; "+(d.p==="R"?"Republican":"Democratic")+
         " president<br>"+
         "<span style=\\"color:"+DEM+"\\">&#9632;</span> Democrats: "+
           d.dw.toFixed(1)+"% worse ("+d.nd.toLocaleString()+")<br>"+
         "<span style=\\"color:"+REP+"\\">&#9632;</span> Republicans: "+
           d.rw.toFixed(1)+"% worse ("+d.nr.toLocaleString()+")<br>"+
         "independents: "+d.iw.toFixed(1)+"%<br>"+
         "<b>gap = "+d.g.toFixed(1)+" pts</b>");
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
})();
</script>'))

## ---- tab1
sel <- nat[nat$year %in% c(1980, 1992, 2000, 2008, 2020, LAST), ]
data.frame(Year = sel$year,
           President = ifelse(sel$president == "R", "Republican", "Democratic"),
           Democrats_saying_worse = paste0(p1(sel$dem_worse), "%"),
           Republicans_saying_worse = paste0(p1(sel$rep_worse), "%"),
           Gap = paste0(p1(sel$gap), " pts"))

## ---- fig2-static
# Seven groups, two studies four years apart. The point is the crossing, so
# both years go on one pair of axes rather than side by side.
op <- par(mar = c(6.8, 4.2, 1.2, 0.8), mgp = c(2.6, 0.7, 0))
a <- grd[grd$year == 2020, ]; b <- grd[grd$year == 2024, ]
a <- a[order(a$code), ]; b <- b[order(b$code), ]
plot(NA, xlim = c(0.8, 7.2), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
abline(h = seq(0, 100, 25), col = "#E4E8EA")
lines(1:7, a$pct_worse, col = REP, lwd = 2.5)
lines(1:7, b$pct_worse, col = DEM, lwd = 2.5)
points(1:7, a$pct_worse, col = REP, pch = 19, cex = 0.8)
points(1:7, b$pct_worse, col = DEM, pch = 19, cex = 0.8)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("% saying the economy got worse", 2, line = 2.7, cex = 0.86)
text(1:7, par("usr")[3] - 4, srt = 40, adj = 1, xpd = NA, cex = 0.7,
     labels = a$category)
text(7, a$pct_worse[7] - 6, "2020 (Trump)", col = REP, pos = 2, cex = 0.72)
text(7, b$pct_worse[7] + 6, "2024 (Biden)", col = DEM, pos = 2, cex = 0.72)
par(op)

## ---- fig2-d3
# The static twin has to show both lines at once, which is right for the
# crossing but makes the middle busy. Here the reader can also take one year
# at a time and watch the staircase turn over.
rows <- paste0('{y:', grd$year, ',c:', grd$code, ',k:"', grd$category,
               '",n:', grd$respondents, ',w:', grd$pct_worse, '}',
               collapse = ",")
cat(paste0('
<div id="pstair" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const A=D.filter(d=>d.y===2020).sort((p,q)=>p.c-q.c);
const B=D.filter(d=>d.y===2024).sort((p,q)=>p.c-q.c);
const DEM="', DEM, '", REP="', REP, '";
const SER=[{y:2020,lab:"2020 (Trump)",col:REP,d:A},
           {y:2024,lab:"2024 (Biden)",col:DEM,d:B}];
const W=770,H=440,M={t:16,r:20,b:118,l:56};
const box=d3.select("#pstair");
const bar=box.append("div").attr("style","margin:0 0 6px");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
let show=[true,true];
const x=d3.scalePoint().domain(A.map(d=>d.c)).range([M.l,W-M.r]).padding(0.4);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(c=>A[c-1].k).tickSize(0));
gx.selectAll("text").attr("transform","rotate(-40)").attr("text-anchor","end")
  .attr("dx","-0.6em").attr("dy","0.4em").style("font-size","10.5px");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#4E5A63")
  .text("% saying the economy got worse");
const line=d3.line().x(d=>x(d.c)).y(d=>y(d.w));
const paths=svg.selectAll("path.s").data(SER).join("path").attr("class","s")
  .attr("fill","none").attr("stroke",d=>d.col).attr("stroke-width",2.5)
  .attr("d",d=>line(d.d));
const dots=SER.map(s=>svg.selectAll("circle.c"+s.y).data(s.d).join("circle")
  .attr("class","c"+s.y).attr("cx",d=>x(d.c)).attr("cy",d=>y(d.w))
  .attr("r",3.6).attr("fill",s.col));
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
dots.forEach(function(sel){
  sel.on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.k+"</b>, "+d.y+"<br>"+d.n.toLocaleString()+" respondents<br>"+
             d.w.toFixed(1)+"% said the economy got worse");
  }).on("mouseleave",function(){tip.style("opacity",0);});
});
const btns=bar.selectAll("button").data(SER).join("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text(d=>d.lab)
  .on("click",function(e,d){const i=SER.indexOf(d);
    if(show[i]&&show.filter(Boolean).length===1)return;   // never hide both
    show[i]=!show[i];draw();});
function draw(){
  paths.attr("opacity",(d,i)=>show[i]?1:0.12);
  dots.forEach((s,i)=>s.attr("opacity",show[i]?1:0.12));
  btns.style("background",(d,i)=>show[i]?d.col:"#fff")
      .style("color",(d,i)=>show[i]?"#fff":"#12181D")
      .style("font-weight",(d,i)=>show[i]?"600":"400");
}
draw();
})();
</script>'))

## ---- fig3-static
# Two subtractions on one axis. The national gap is the wider line everywhere
# except 2008, and both cross zero in the same years.
op <- par(mar = c(3.6, 4.2, 1.4, 7.4), mgp = c(2.6, 0.7, 0))
m <- both[order(both$year), ]
yl <- max(abs(c(m$gap_nat, m$gap_own))) + 6
plot(NA, xlim = range(m$year), ylim = c(-yl, yl), axes = FALSE,
     xlab = "", ylab = "")
abline(h = 0, col = "#12181D")
lines(m$year, m$gap_nat, col = REP, lwd = 2.5)
lines(m$year, m$gap_own, col = DEM, lwd = 2.5)
points(m$year, m$gap_nat, col = REP, pch = 19, cex = 0.6)
points(m$year, m$gap_own, col = DEM, pch = 19, cex = 0.6)
axis(1, at = seq(1980, 2024, 8), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(-60, 60, 20), las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Democrats minus Republicans (pts)", 2, line = 2.6, cex = 0.86)
text(LAST, m$gap_nat[nrow(m)], " the national\n economy", col = REP, pos = 4,
     cex = 0.72, xpd = NA)
text(LAST, m$gap_own[nrow(m)], " their own\n finances", col = DEM, pos = 4,
     cex = 0.72, xpd = NA)
par(op)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# Two subtractions on one axis. Hovering reports both and says which is
# wider, because "wider" is the whole claim and it is easier to assert than
# to eyeball in the years where the two lines run close together.
#
# Drawn with the shared library (_lib/dd-charts.js). Figure 1 above is
# hand-written and already loaded d3, so dd_fig() is told d3 = FALSE and
# emits only the dd-charts tag; a second d3 copy would double the payload.
# Colours are series classes, so the labels survive dark mode without the
# per-figure CSS patch the hand-written version needed.
# ---------------------------------------------------------------------------
m <- both[order(both$year), ]
ym <- max(abs(c(m$gap_nat, m$gap_own))) + 6
dd_fig("pown", "line", m, d3 = FALSE,
  size = list(w = 770, h = 400, m = list(t = 16, r = 132, b = 38, l = 64)),
  x = list(field = "year", fmt = "d", ticks = 6),
  y = list(field = "gap_nat", label = "Democrats minus Republicans (pts)",
           domain = c(-ym, ym), fmt = "signed0", ticks = 6),
  series = list(fields = list(
    list(field = "gap_nat", label = "the national economy",
         endLabel = c("the national", "economy"), class = "series-2"),
    list(field = "gap_own", label = "their own finances",
         endLabel = c("their own", "finances"), class = "series-1"))),
  points = TRUE, endLabels = TRUE,
  annotations = list(list(type = "hline", y = 0, class = "zero",
                          dash = FALSE)),
  tip = dd_js('function(d){
    var wider = Math.abs(d.gap_nat) > Math.abs(d.gap_own) ?
      "the national question" : "the personal one";
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> national economy: "+
        d.gap_nat.toFixed(1)+" pts<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> own finances: "+
        d.gap_own.toFixed(1)+" pts<br>wider on "+wider;
  }'))

## ---- tab2
sel <- both[both$year %in% c(1984, 2004, 2008, 2016, 2020, LAST), ]
data.frame(Year = sel$year,
           The_national_economy = paste0(p1(sel$gap_nat), " pts"),
           Their_own_finances = paste0(p1(sel$gap_own), " pts"),
           Wider_on = ifelse(abs(sel$gap_nat) > abs(sel$gap_own),
                             "the national question", "their own finances"))

## ---- checks
ck
