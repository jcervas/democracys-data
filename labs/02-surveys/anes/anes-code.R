# anes-code.R -- chunk bodies for anes-brief.Rmd
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

cov <- read.csv("data/derived/coverage.csv",   stringsAsFactors = FALSE)
cds <- read.csv("data/derived/codes.csv",      stringsAsFactors = FALSE)
awb <- read.csv("data/derived/awareness.csv",  stringsAsFactors = FALSE)
zal <- read.csv("data/derived/zaller.csv",     stringsAsFactors = FALSE)
res <- read.csv("data/derived/resentment.csv", stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p2 <- function(x) formatC(x, format = "f", digits = 2)
p3 <- function(x) formatC(x, format = "f", digits = 3)

cl <- function(which) cds$mean[cds$cleaning == which]
NAIVE <- cl("Nothing removed, the file as it arrives")
ONLY9 <- cl("Only 9 (don't know) removed")
ONLY0 <- cl("Only 0 and -1 (inapplicable) removed")
RIGHT <- cl("Every non-scale code removed")
NALL  <- cds$n[cds$cleaning == "Nothing removed, the file as it arrives"]
NGOOD <- cds$n[cds$cleaning == "Every non-scale code removed"]
PCTBAD <- 100 * (NALL - NGOOD) / NALL

INFO_LAST <- max(cov$last[grepl("VCF0050", cov$variable)])
REACH24   <- sum(cov$last == 2024)

gap <- function(item, band) {
  s <- zal[zal$item == item & zal$awareness == band, ]
  s$mean[s$party == "Republican"] - s$mean[s$party == "Democrat"]
}
HI_LO <- gap("Government health insurance", "lowest")
HI_HI <- gap("Government health insurance", "highest")
AB_LO <- gap("Abortion", "lowest")
AB_HI <- gap("Abortion", "highest")

rg <- function(y, p) res$resentment[res$year == y & res$party == p]
RGAP <- function(y) rg(y, "Republican") - rg(y, "Democrat")
YR1 <- min(res$year); YR2 <- max(res$year)

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

PAL <- c(Democrat = "#2B5C8A", Independent = "#8A8F94", Republican = "#A33B2A")

## ---- raw
cat(paste(readLines("data/raw/anes-head.txt"), collapse = "\n"))

## ---- codes
data.frame(
  What_was_removed = cds$cleaning,
  Mean = p3(cds$mean),
  Answers_used = nn(cds$n),
  Distance_from_correct = ifelse(cds$error == 0, "—",
                                 sprintf("%+.3f", cds$error)))

## ---- awareness
data.frame(How_it_was_built = awb$build,
           Respondents = nn(awb$respondents),
           Years = paste0(awb$first_year, "–", awb$last_year),
           Distinct_levels = awb$levels)

## ---- rrlabels
r4 <- cov[grepl("VCF904|VCF9039", cov$variable), c("variable", "label")]
r4$agreeing_means <- c("less resentment", "more resentment",
                       "more resentment", "less resentment")[
                         match(r4$variable, c("VCF9039","VCF9040","VCF9041","VCF9042"))]
names(r4) <- c("Variable", "Item", "Agreeing means")
r4

## ---- fig1-static
op <- par(mar = c(3.4, 3.9, 1.6, 6.2), mgp = c(2.4, 0.7, 0))
yr <- sort(unique(res$year))
plot(NA, xlim = range(yr), ylim = range(res$resentment) + c(-0.1, 0.1),
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(1986, 2024, 6), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("mean racial resentment", 2, line = 2.6, cex = 0.9)
for (p in names(PAL)) {
  q <- res[res$party == p, ]
  q <- q[order(q$year), ]
  lines(q$year, q$resentment, col = PAL[p], lwd = 2.5)
  points(q$year, q$resentment, col = PAL[p], pch = 19, cex = 0.7)
  text(max(q$year), q$resentment[which.max(q$year)], paste0(" ", p),
       col = PAL[p], pos = 4, cex = 0.78, xpd = NA)
}
par(op)

## ---- fig1-d3
# The claim under this figure is about a gap and about which side moved, so the
# hover reports the Republican-minus-Democrat gap at each year alongside the
# three levels. Reading that off a printed pair of lines is exactly the sort of
# thing readers get wrong.
#
# This chunk carries the ONE d3 <script src> for the document.
rows <- paste0('{y:', res$year, ',p:"', res$party, '",v:', res$resentment,
               ',n:', res$n, '}', collapse = ",")
pal <- paste0('"', names(PAL), '":"', PAL, '"', collapse = ",")
cat(paste0('
<div id="anr" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const PAL={', pal, '};
const PARTIES=Object.keys(PAL);
const YRS=[...new Set(D.map(d=>d.y))].sort((a,b)=>a-b);
const W=770,H=400,M={t:16,r:120,b:44,l:56};
const box=d3.select("#anr");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(YRS)).range([M.l,W-M.r]);
const ex=d3.extent(D,d=>d.v);
const y=d3.scaleLinear().domain([ex[0]-0.1,ex[1]+0.1]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(7));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("mean racial resentment");
PARTIES.forEach(function(p){
  const q=D.filter(d=>d.p===p).sort((a,b)=>a.y-b.y);
  svg.append("path").attr("fill","none").attr("stroke",PAL[p])
     .attr("stroke-width",2.5)
     .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.v))(q));
  svg.selectAll("c"+p).data(q).join("circle")
     .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.v)).attr("r",3).attr("fill",PAL[p]);
  const last=q[q.length-1];
  svg.append("text").attr("x",x(last.y)+8).attr("y",y(last.v)+4)
     .attr("font-size","12px").attr("font-weight","600").attr("fill",PAL[p])
     .text(p);
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","3 3").attr("opacity",0);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const yr0=x.invert(d3.pointer(e,this)[0]+M.l);
    const yr=YRS.reduce((a,b)=>Math.abs(b-yr0)<Math.abs(a-yr0)?b:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",0.55);
    const at=D.filter(d=>d.y===yr);
    const g=(a,b)=>{const u=at.find(d=>d.p===a),v=at.find(d=>d.p===b);
                    return (u&&v)?(u.v-v.v):null;};
    const gap=g("Republican","Democrat");
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+yr+"</b><br>"+
         PARTIES.map(function(p){const d=at.find(z=>z.p===p);
           return d?("<span style=\\"color:"+PAL[p]+"\\">&#9632;</span> "+p+
             ": "+d.v.toFixed(2)+" <span style=\\"color:#8A8F94\\">(n="+
             d.n.toLocaleString()+")</span>"):"";}).filter(Boolean).join("<br>")+
         (gap!==null?("<br><b>Rep &minus; Dem: "+gap.toFixed(2)+"</b>"):""));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
})();
</script>'))

## ---- coverage
cv <- cov[order(cov$first), ]
data.frame(Variable = cv$variable, Question = cv$label,
           Years = paste0(cv$first, "–", cv$last),
           Studies = cv$studies, Answers = nn(cv$answers))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
