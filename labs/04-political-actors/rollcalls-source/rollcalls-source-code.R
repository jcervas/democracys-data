# rollcalls-source-code.R -- chunk bodies for rollcalls-source-brief.Rmd
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

vol <- read.csv("data/derived/volume.csv",    stringsAsFactors = FALSE)
mar <- read.csv("data/derived/margins.csv",   stringsAsFactors = FALSE)
con <- read.csv("data/derived/content.csv",   stringsAsFactors = FALSE)
qs  <- read.csv("data/derived/questions.csv", stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

TOTAL <- con$roll_calls[con$era == "all"]
COVER <- con$question_recorded_pct[con$era == "all"]
NOQ   <- round(TOTAL * (100 - COVER) / 100)
LASTC <- max(vol$congress)

UNAN  <- mar$pct[grepl("^Unanimous", mar$band)]
LOP   <- sum(mar$pct[grepl("^Unanimous|under 10", mar$band)])
CLOSE <- mar$pct[grepl("^Close", mar$band)]

PASS  <- qs$pct_of_all[qs$question == "On Passage"]

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

ACC <- "#1C4C5C"; WARN <- "#8A3B2C"; GRY <- "#8A8F94"

## ---- fig1-static
op <- par(mar = c(3.4, 4.2, 1.2, 6.4), mgp = c(2.7, 0.7, 0))
plot(NA, xlim = range(vol$congress), ylim = c(0, max(vol$house, vol$senate) * 1.05),
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(10, LASTC, 20), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Congress", 1, line = 2.3, cex = 0.9)
mtext("recorded votes", 2, line = 3.0, cex = 0.9)
lines(vol$congress, vol$house,  col = ACC,  lwd = 2.2)
lines(vol$congress, vol$senate, col = WARN, lwd = 2.2)
text(LASTC, vol$house[nrow(vol)],  " House",  col = ACC,  pos = 4, cex = 0.74, xpd = NA)
text(LASTC, vol$senate[nrow(vol)], " Senate", col = WARN, pos = 4, cex = 0.74, xpd = NA)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Two centuries of counts, where the interesting question at any point is "how
# many, and in which chamber" — a question a printed line cannot answer without
# a ruler. The 91st is marked because the chapter's claim is about that break.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('[', vol$congress, ',', vol$house, ',', vol$senate, ']',
               collapse = ",")
cat(paste0('
<div id="rcv" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const R=[', rows, '];
const D=R.map(r=>({c:r[0],h:r[1],s:r[2]}));
const ACC="', ACC, '", WARN="', WARN, '";
const W=770,H=420,M={t:16,r:88,b:44,l:58};
const box=d3.select("#rcv");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.c)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.h,d.s))*1.05])
  .range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d3.format(",")).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Congress");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("recorded votes");
svg.append("line").attr("x1",x(91)).attr("x2",x(91)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#12181D").attr("stroke-dasharray","3 3")
  .attr("opacity",0.5);
svg.append("text").attr("x",x(91)-5).attr("y",M.t+12).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#12181D").text("91st");
[["h",ACC,"House"],["s",WARN,"Senate"]].forEach(function(k){
  svg.append("path").attr("fill","none").attr("stroke",k[1]).attr("stroke-width",2.2)
     .attr("d",d3.line().x(d=>x(d.c)).y(d=>y(d[k[0]]))(D));
  svg.append("text").attr("x",W-M.r+8).attr("y",y(D[D.length-1][k[0]])+4)
     .attr("font-size","12px").attr("font-weight","600").attr("fill",k[1])
     .text(k[2]);
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","2 2").attr("opacity",0);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const fmt=d3.format(",d");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const c=x.invert(d3.pointer(e,this)[0]+M.l);
    const d=D.reduce((a,b)=>Math.abs(b.c-c)<Math.abs(a.c-c)?b:a);
    rule.attr("x1",x(d.c)).attr("x2",x(d.c)).attr("opacity",0.55);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+d.c+"th Congress</b><br>"+
         "<span style=\\"color:"+ACC+"\\">&#9632;</span> House "+fmt(d.h)+"<br>"+
         "<span style=\\"color:"+WARN+"\\">&#9632;</span> Senate "+fmt(d.s));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
})();
</script>'))

## ---- contab
c2 <- con[con$era != "all", ]
data.frame(Congresses = c2$era, Years = c2$years,
           Roll_calls = nn(c2$roll_calls),
           Question_recorded = paste0(p1(c2$question_recorded_pct), "%"))

## ---- qtab
data.frame(Question = qs$question, Roll_calls = nn(qs$roll_calls),
           Share_of_all_roll_calls = paste0(p1(qs$pct_of_all), "%"))

## ---- martab
data.frame(Margin = mar$band, Roll_calls = nn(mar$roll_calls),
           Share = paste0(p1(mar$pct), "%"))

## ---- fig2-static
op <- par(mar = c(2.6, 11.4, 1.0, 3.0), mgp = c(2.4, 0.6, 0))
b <- barplot(rev(mar$pct), horiz = TRUE, col = rev(c(GRY, GRY, ACC, ACC, ACC)),
             border = NA, xlim = c(0, max(mar$pct) + 8), axes = FALSE,
             names.arg = rev(mar$band), las = 1, cex.names = 0.72)
axis(1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
text(rev(mar$pct), b, paste0(" ", p1(rev(mar$pct)), "%"), pos = 4,
     cex = 0.74, col = "#4E5A63", xpd = NA)
par(op)

## ---- fig2-d3
# Five bands, and the number that matters is the running total: how much of the
# record is uninformative about disagreement. Hovering gives the count and the
# cumulative share, which is the quantity the prose asserts.
# mar is ordered most-lopsided first, and scaleBand puts the first band at the
# top -- the same order the static twin draws. So the running total is a plain
# forward cumulative sum: this band plus everything above it.
mm <- mar
mm$cum <- cumsum(mm$pct)
rows <- paste0('{b:"', mm$band, '",n:', mm$roll_calls, ',p:', mm$pct,
               ',cum:', formatC(mm$cum, format = "f", digits = 1), '}',
               collapse = ",")
cols <- paste0('"', c(GRY, GRY, ACC, ACC, ACC), '"', collapse = ",")
cat(paste0('
<div id="rcm" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '], C=[', cols, '];
const W=770,H=300,M={t:12,r:70,b:36,l:230};
const box=d3.select("#rcm");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.p)*1.12]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.b)).range([M.t,H-M.b]).padding(0.3);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(6));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickSize(0))
  .selectAll("text").style("font-size","11px");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const fmt=d3.format(",d");
svg.selectAll("rect.b").data(D).join("rect").attr("class","b")
  .attr("x",M.l).attr("y",d=>y(d.b)).attr("height",y.bandwidth())
  .attr("width",d=>x(d.p)-M.l).attr("fill",(d,i)=>C[i])
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.b+"</b><br>"+fmt(d.n)+" roll calls &middot; "+
             d.p.toFixed(1)+"%<br>this band and everything above it: <b>"+
             d.cum.toFixed(1)+"%</b>");
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
svg.selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>x(d.p)+6).attr("y",d=>y(d.b)+y.bandwidth()/2+4)
  .attr("font-size","11px").attr("fill","#4E5A63")
  .text(d=>d.p.toFixed(1)+"%");
})();
</script>'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
