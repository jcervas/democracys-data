# surveys-source-code.R -- chunk bodies for surveys-source-brief.Rmd
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

cap  <- read.csv("data/derived/capacity.csv",    stringsAsFactors = FALSE)
mo   <- read.csv("data/derived/moe.csv",         stringsAsFactors = FALSE)
wt   <- read.csv("data/derived/weighting.csv",   stringsAsFactors = FALSE)
inst <- read.csv("data/derived/instruments.csv", stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)
gv <- function(q) cap$value[cap$quantity == q]

NRESP  <- gv("Respondents in the study")
NSTATE <- gv("States and equivalents represented")
MEDN   <- gv("Respondents in the median state")
MINN   <- gv("Respondents in the smallest state")
MEDMOE <- gv("Margin of error, median state, at 50%")
MINMOE <- gv("Margin of error, smallest state, at 50%")
ALLMOE <- gv("Margin of error, whole sample, at 50%")

WREG <- wt[wt$variable == "votereg" & wt$category == "Yes", ]
WNO  <- wt[wt$variable == "votereg" & wt$category == "No",  ]
p2   <- function(x) formatC(x, format = "f", digits = 2)

# one respondent's weight, on average, relative to an answer counted once:
# a group's weighted share over its unweighted share
W_NO  <- WNO$pct_weighted  / WNO$pct_unweighted
W_REG <- WREG$pct_weighted / WREG$pct_unweighted

# the margin-of-error arithmetic for the median state, step by step
MED_VAR <- 0.25 / MEDN          # p(1-p)/n at p = 0.5
MED_SE  <- sqrt(MED_VAR)

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

## ---- inst
data.frame(The_question = inst$question, Census = inst$census,
           ANES = inst$anes, CES = inst$ces)

## ---- captab
data.frame(Quantity = cap$quantity,
           Value = ifelse(cap$unit == "count", nn(cap$value),
                          paste0("± ", p1(cap$value), " points")))

## ---- fig1-static
op <- par(mar = c(3.8, 4.2, 1.2, 1.0), mgp = c(2.7, 0.7, 0))
plot(mo$respondents, mo$margin_of_error, log = "x", type = "n",
     axes = FALSE, xlab = "", ylab = "")
abline(h = seq(0, 20, 5), col = "#E4E8EA")
lines(mo$respondents, mo$margin_of_error, col = ACC, lwd = 2.6)
points(mo$respondents, mo$margin_of_error, col = ACC, pch = 19, cex = 0.8)
axis(1, at = c(25, 100, 400, 2500, 60000),
     labels = c("25", "100", "400", "2,500", "60,000"),
     cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("respondents", 1, line = 2.4, cex = 0.9)
mtext("± points at 50%", 2, line = 2.7, cex = 0.9)
points(MEDN, MEDMOE, col = WARN, pch = 19, cex = 1.3)
text(MEDN, MEDMOE, paste0("  a typical state\n  in ANES: ± ", p1(MEDMOE)),
     col = WARN, pos = 4, cex = 0.72)
points(60000, mo$margin_of_error[mo$respondents == 60000], col = "#2B5C8A",
       pch = 19, cex = 1.3)
text(60000, mo$margin_of_error[mo$respondents == 60000], "one CES  ",
     col = "#2B5C8A", pos = 2, cex = 0.72)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The square-root law is easier to believe when you can interrogate it. The
# static twin annotates three points; here the reader moves along the curve and
# reads the margin of error at any sample size, which is the only way to feel
# how flat the right-hand end is.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('[', mo$respondents, ',',
               formatC(mo$margin_of_error, format = "f", digits = 3), ']',
               collapse = ",")
cat(paste0('
<div id="moe" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '].map(r=>({n:r[0],m:r[1]}));
const ACC="', ACC, '", WARN="', WARN, '", CES="#2B5C8A";
const MEDN=', MEDN, ', MEDMOE=', formatC(MEDMOE, format="f", digits=3), ';
const W=770,H=420,M={t:16,r:130,b:44,l:56};
const box=d3.select("#moe");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain(d3.extent(D,d=>d.n)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.m)*1.05]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues([25,100,400,2500,60000])
          .tickFormat(d3.format(",")));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>"\\u00b1"+d).ticks(5));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("respondents (log scale)");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("\\u00b1 points at 50%");
svg.append("path").attr("fill","none").attr("stroke",ACC).attr("stroke-width",2.6)
  .attr("d",d3.line().x(d=>x(d.n)).y(d=>y(d.m))(D));
[[MEDN,MEDMOE,WARN,"a typical state in ANES"],
 [60000,D.find(d=>d.n===60000).m,CES,"one CES"]].forEach(function(p){
  svg.append("circle").attr("cx",x(p[0])).attr("cy",y(p[1])).attr("r",5)
     .attr("fill",p[2]);
  svg.append("text").attr("x",x(p[0])+9).attr("y",y(p[1])-7)
     .attr("font-size","11px").attr("font-weight","600").attr("fill",p[2])
     .text(p[3]);
});
const dot=svg.append("circle").attr("r",4).attr("fill","#12181D").attr("opacity",0);
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","3 3").attr("opacity",0);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const fmt=d3.format(",d");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const n=x.invert(d3.pointer(e,this)[0]+M.l);
    const d=D.reduce((a,b)=>Math.abs(Math.log(b.n)-Math.log(n))<
                            Math.abs(Math.log(a.n)-Math.log(n))?b:a);
    dot.attr("cx",x(d.n)).attr("cy",y(d.m)).attr("opacity",1);
    rule.attr("x1",x(d.n)).attr("x2",x(d.n)).attr("opacity",0.5);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+fmt(d.n)+" respondents</b><br>margin of error \\u00b1"+
             d.m.toFixed(1)+" points at 50%");
  })
  .on("mouseleave",function(){
    tip.style("opacity",0);rule.attr("opacity",0);dot.attr("opacity",0);
  });
})();
</script>'))

## ---- raw
cat(paste(readLines("data/raw/wording.txt"), collapse = "\n"))

## ---- wtab
data.frame(Variable = wt$variable, Category = wt$category,
           Interviews = nn(wt$n),
           Raw = paste0(p1(wt$pct_unweighted), "%"),
           Weighted = paste0(p1(wt$pct_weighted), "%"),
           Adjustment = sprintf("%+.1f", wt$adjustment))
