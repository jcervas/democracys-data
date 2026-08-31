# ideology-code.R -- chunk bodies for ideology-brief.Rmd
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

pl   <- read.csv("data/derived/placement.csv", stringsAsFactors = FALSE)
mid  <- read.csv("data/derived/middles.csv",   stringsAsFactors = FALSE)
coll <- read.csv("data/derived/collapse.csv",  stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

mv <- function(m, col) mid[[col]][mid$measure == m]
NMOD <- mv("Respondents", "moderate")
NDK  <- mv("Respondents", "not_placed")
EDM  <- mv("Holds a college degree", "moderate")
EDD  <- mv("Holds a college degree", "not_placed")
VTM  <- mv("Voted", "moderate")
VTD  <- mv("Voted", "not_placed")
SPM  <- mv("Strong partisan, strong Democrat or strong Republican", "moderate")
SPD  <- mv("Strong partisan, strong Democrat or strong Republican", "not_placed")

FIRST <- min(pl$year); LAST <- max(pl$year)
r1 <- pl[pl$year == FIRST, ]; r2 <- pl[pl$year == LAST, ]
DKSHARE <- round(100 * NDK / (NMOD + NDK), 1)

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

LIB <- "#2B5C8A"; CON <- "#A33B2A"; MODC <- "#8A8F94"; DKC <- "#C08A2E"

## ---- raw
cat(paste(readLines("data/raw/scale.txt"), collapse = "\n"))

## ---- tab1
m <- mid[mid$measure != "Respondents", ]
data.frame(Measure = m$measure,
           Moderate = ifelse(m$unit == "%", paste0(p1(m$moderate), "%"),
                             p1(m$moderate)),
           Havent_thought_much = ifelse(m$unit == "%",
                             paste0(p1(m$not_placed), "%"), p1(m$not_placed)))

## ---- fig1-static
op <- par(mar = c(3.4, 4.0, 1.4, 7.4), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = range(pl$year), ylim = c(0, max(pl$conservative) + 5),
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(1972, 2024, 13), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% of respondents", 2, line = 2.7, cex = 0.9)
sers <- list(conservative = CON, liberal = LIB, moderate = MODC,
             not_placed = DKC)
for (s in names(sers)) {
  lines(pl$year, pl[[s]], col = sers[[s]], lwd = 2.4,
        lty = if (s == "not_placed") 2 else 1)
  points(pl$year, pl[[s]], col = sers[[s]], pch = 19, cex = 0.55)
}
labs <- c(conservative = "conservative", liberal = "liberal",
          moderate = "moderate", not_placed = "haven't\nthought much")
for (s in names(sers))
  text(LAST, pl[[s]][pl$year == LAST], paste0(" ", labs[s]),
       col = sers[[s]], pos = 4, cex = 0.7, xpd = NA)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Four series whose whole point is what happens to their sum. Muting a series
# with the buttons is how a reader checks the chapter's claim that the people
# who stopped saying "haven't thought much" did not become moderates.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('[', pl$year, ',', pl$liberal, ',', pl$moderate, ',',
               pl$conservative, ',', pl$not_placed, ']', collapse = ",")
cat(paste0('
<div id="ide" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const R=[', rows, '];
const D=R.map(r=>({y:r[0],v:[r[1],r[2],r[3],r[4]]}));
const K=[{c:"', LIB, '",lab:"liberal",dash:null},
         {c:"', MODC, '",lab:"moderate",dash:null},
         {c:"', CON, '",lab:"conservative",dash:null},
         {c:"', DKC, '",lab:"haven\\u2019t thought much",dash:"5 4"}];
const W=770,H=430,M={t:16,r:170,b:40,l:52};
const box=d3.select("#ide");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d3.max(d.v))+5]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(7));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(6));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("% of respondents");
const on=[true,true,true,true];
const gs=K.map(function(k,i){
  const g=svg.append("g");
  g.append("path").attr("fill","none").attr("stroke",k.c).attr("stroke-width",2.4)
   .attr("stroke-dasharray",k.dash)
   .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.v[i]))(D));
  g.selectAll("circle").data(D).join("circle")
   .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.v[i])).attr("r",2.6).attr("fill",k.c);
  g.append("text").attr("x",W-M.r+8).attr("y",y(D[D.length-1].v[i])+4)
   .attr("font-size","11.5px").attr("font-weight","600").attr("fill",k.c)
   .text(k.lab);
  return g;
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","3 3").attr("opacity",0);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const yr=x.invert(d3.pointer(e,this)[0]+M.l);
    const d=D.reduce((a,b)=>Math.abs(b.y-yr)<Math.abs(a.y-yr)?b:a);
    rule.attr("x1",x(d.y)).attr("x2",x(d.y)).attr("opacity",0.55);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+d.y+"</b><br>"+K.map((k,i)=>
         "<span style=\\"color:"+k.c+"\\">&#9632;</span> "+k.lab+": "+
         d.v[i].toFixed(1)+"%").join("<br>"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
const leg=box.append("div").attr("style","margin-top:6px");
leg.selectAll("button").data(K).join("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .html(d=>"<span style=\\"color:"+d.c+"\\">&#9632;</span> "+d.lab)
  .on("click",function(e,d){
    const i=K.indexOf(d); on[i]=!on[i];
    gs[i].attr("opacity",on[i]?1:0.12);
    d3.select(this).style("opacity",on[i]?1:0.45);
  });
})();
</script>'))

## ---- tab2
data.frame(Seven_point = coll$seven_point,
           Collapses_to = coll$three_category,
           Respondents = nn(coll$respondents))
