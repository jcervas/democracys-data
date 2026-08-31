# party-id-code.R -- chunk bodies for party-id-brief.Rmd
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

sev  <- read.csv("data/derived/sevenpoint.csv",   stringsAsFactors = FALSE)
ind  <- read.csv("data/derived/independents.csv", stringsAsFactors = FALSE)
lean <- read.csv("data/derived/leaners.csv",      stringsAsFactors = FALSE)
coll <- read.csv("data/derived/collapse.csv",     stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

LAST  <- max(ind$year)
FIRST <- min(ind$year)
PURE  <- ind$pure_only[ind$year == LAST]
WITH  <- ind$with_leaners[ind$year == LAST]
FACTOR <- WITH / PURE

lv <- function(cat, col) lean[[col]][lean$category == cat]
LD <- lv("Independent-Democrat",    "pct_voted_democratic")
WD <- lv("Weak Democrat",           "pct_voted_democratic")
LR <- lv("Independent-Republican",  "loyalty")
WR <- lv("Weak Republican",         "loyalty")
PI <- lv("Independent-Independent", "pct_voted_democratic")

FILE_IND <- coll$share[coll$seven_point == "Independent-Independent"]
OWN_IND  <- sum(coll$share[coll$seven_point %in%
  c("Independent-Democrat", "Independent-Independent", "Independent-Republican")])

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

## ---- raw
cat(paste(readLines("data/raw/branch.txt"), collapse = "\n"))

## ---- fig1-static
op <- par(mar = c(3.4, 4.0, 1.4, 7.6), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = range(ind$year), ylim = c(0, max(ind$with_leaners) + 4),
     axes = FALSE, xlab = "", ylab = "")
polygon(c(ind$year, rev(ind$year)), c(ind$with_leaners, rev(ind$pure_only)),
        col = "#E4E8EA", border = NA)
axis(1, at = seq(1952, 2024, 12), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% of respondents", 2, line = 2.7, cex = 0.9)
lines(ind$year, ind$with_leaners, col = DEM, lwd = 2.5)
lines(ind$year, ind$pure_only,    col = REP, lwd = 2.5)
points(ind$year, ind$with_leaners, col = DEM, pch = 19, cex = 0.6)
points(ind$year, ind$pure_only,    col = REP, pch = 19, cex = 0.6)
text(LAST, ind$with_leaners[ind$year == LAST], " anyone who said\n independent",
     col = DEM, pos = 4, cex = 0.72, xpd = NA)
text(LAST, ind$pure_only[ind$year == LAST], " no party at all",
     col = REP, pos = 4, cex = 0.72, xpd = NA)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The band between the two definitions is the whole argument, and on paper it
# is just grey. Here hovering decomposes it: the upper line is the sum of
# lean_dem + pure_ind + lean_rep, so the reader can see that almost all of the
# disagreement is leaners rather than people with no party at all.
#
# Drawn with the shared library (_lib/dd-charts.js); dd_fig() emits the two
# <script src> tags for the document. The blue and red are series classes,
# not the party pair: the two lines are two DEFINITIONS, and both count the
# same independents.
# ---------------------------------------------------------------------------
m <- merge(ind[, c("year", "n", "pure_only", "with_leaners", "spread")],
           sev[, c("year", "lean_dem", "pure_ind", "lean_rep")], by = "year")
m <- m[order(m$year), ]
dd_fig("pid", "line", m,
  size = list(w = 770, h = 420, m = list(t = 16, r = 150, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 7),
  y = list(field = "with_leaners", label = "% of respondents",
           domain = c(0, max(m$with_leaners) + 4), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "with_leaners", label = "anyone who said independent",
         endLabel = c("anyone who said", "independent"), class = "series-1"),
    list(field = "pure_only", label = "no party at all", class = "series-2"))),
  band = list(y0 = "pure_only", y1 = "with_leaners"),
  points = TRUE, endLabels = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b> &middot; "+DD.fmt.comma(d.n)+" respondents<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> said independent: "+
        d.with_leaners.toFixed(1)+"%<br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> no party at all: "+
        d.pure_only.toFixed(1)+"%<br>"+
      "<b>band = "+d.spread.toFixed(1)+" pts</b><br>"+
      "&nbsp;&nbsp;lean Dem "+d.lean_dem.toFixed(1)+"% &middot; pure "+
        d.pure_ind.toFixed(1)+"% &middot; lean Rep "+d.lean_rep.toFixed(1)+"%";
  }'))

## ---- tab1
sel <- ind[ind$year %in% c(FIRST, 1972, 1992, 2012, LAST), ]
data.frame(Year = sel$year, Respondents = nn(sel$n),
           No_party_at_all = paste0(p1(sel$pure_only), "%"),
           Anyone_who_said_independent = paste0(p1(sel$with_leaners), "%"),
           Gap = paste0(p1(sel$spread), " pts"))

## ---- tab2
data.frame(Category = lean$category, Respondents = nn(lean$respondents),
           Voted_Democratic = paste0(p1(lean$pct_voted_democratic), "%"),
           Loyalty_to_nearer_party = paste0(p1(lean$loyalty), "%"))

## ---- fig2-static
op <- par(mar = c(6.6, 4.0, 1.2, 0.8), mgp = c(2.6, 0.7, 0))
o  <- lean$pct_voted_democratic
cl <- c(DEM, DEM, DEM, GRY, REP, REP, REP)
bp <- barplot(o, col = cl, border = NA, ylim = c(0, 100), axes = FALSE,
              names.arg = rep("", 7))
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% voting Democratic", 2, line = 2.7, cex = 0.9)
abline(h = 50, lty = 3, col = "#76838C")
text(bp, par("usr")[3] - 4, srt = 40, adj = 1, xpd = NA, cex = 0.72,
     labels = lean$category)
text(bp, o + 4, paste0(p1(o), "%"), cex = 0.68, col = "#4E5A63")
par(op)

## ---- fig2-d3
# Two measures of the same seven groups. "Voted Democratic" is a direction and
# makes a staircase; "loyalty to the nearer party" folds the two sides together
# and is the measure on which the leaners overtake the weak partisans. The
# static twin has to pick one; here the reader switches and sees the crossing.
rows <- paste0('{k:"', lean$category, '",n:', lean$respondents,
               ',d:', lean$pct_voted_democratic, ',l:', lean$loyalty, '}',
               collapse = ",")
cols <- paste0('"', c(DEM, DEM, DEM, GRY, REP, REP, REP), '"', collapse = ",")
cat(paste0('
<div id="pln" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '], C=[', cols, '];
const MODE=[{k:"d",lab:"% voting Democratic"},
            {k:"l",lab:"% loyal to the nearer party"}];
const W=770,H=420,M={t:14,r:16,b:104,l:56};
const box=d3.select("#pln");
const bar=box.append("div").attr("style","margin:0 0 6px");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
let sel=0;
const x=d3.scaleBand().domain(D.map(d=>d.k)).range([M.l,W-M.r]).padding(0.24);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickSize(0));
gx.selectAll("text").attr("transform","rotate(-38)").attr("text-anchor","end")
  .attr("dx","-0.6em").attr("dy","0.4em").style("font-size","11px");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#76838C").attr("stroke-dasharray","2 3");
const ylab=svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const rects=svg.selectAll("rect.b").data(D).join("rect").attr("class","b")
  .attr("x",d=>x(d.k)).attr("width",x.bandwidth())
  .attr("fill",(d,i)=>C[i]);
const vals=svg.selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>x(d.k)+x.bandwidth()/2).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63");
const btns=bar.selectAll("button").data(MODE).join("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text(d=>d.lab)
  .on("click",function(e,d){sel=MODE.indexOf(d);draw(true);});
rects.on("mousemove",function(e,d){
  const r=box.node().getBoundingClientRect();
  tip.style("opacity",1)
     .style("left",(e.clientX-r.left+14)+"px")
     .style("top",(e.clientY-r.top-8)+"px")
     .html("<b>"+d.k+"</b><br>"+d.n.toLocaleString()+" respondents<br>"+
       "voted Democratic: "+d.d.toFixed(1)+"%<br>"+
       "loyal to nearer party: "+d.l.toFixed(1)+"%");
}).on("mouseleave",function(){tip.style("opacity",0);});
// First paint is not a transition: rAF is frozen in a background tab.
function draw(anim){
  const k=MODE[sel].k;
  (anim?rects.transition().duration(340):rects)
    .attr("y",d=>y(d[k])).attr("height",d=>y(0)-y(d[k]));
  (anim?vals.transition().duration(340):vals)
    .attr("y",d=>y(d[k])-5).text(d=>d[k].toFixed(1)+"%");
  ylab.text(MODE[sel].lab);
  btns.style("background",(d,i)=>i===sel?"#1C4C5C":"#fff")
      .style("color",(d,i)=>i===sel?"#fff":"#12181D")
      .style("font-weight",(d,i)=>i===sel?"600":"400");
}
draw(false);
})();
</script>'))

## ---- tab3
data.frame(Seven_point_category = coll$seven_point,
           Share = paste0(p1(coll$share), "%"),
           The_file_files_them_under = coll$file_puts_it_in)
