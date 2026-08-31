# gender-gap-code.R -- chunk bodies for gender-gap-brief.Rmd
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

S  <- read.csv("data/derived/partyid_by_sex.csv",         stringsAsFactors = FALSE)
M  <- read.csv("data/derived/partyid_by_sex_marital.csv", stringsAsFactors = FALSE)
D  <- read.csv("data/derived/by_decade.csv",              stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",                  stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",                 stringsAsFactors = FALSE)

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
sg <- function(x) sprintf("%+.1f", as.numeric(x))

knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

WOM <- "#6a3d9a"   # women, everywhere in this chapter
MEN <- "#ff7f00"   # men
GAP <- "#333333"

## ---- gap-only-static
par(mar = c(3.2, 4.2, 1.4, 1.2))
plot(S$year, S$gap, type = "n", xlab = "", ylab = "gap, points",
     ylim = c(-4, 22), axes = FALSE)
abline(h = 0, col = "grey60", lty = 3)
lines(S$year, S$gap, col = GAP, lwd = 2)
points(S$year, S$gap, col = GAP, pch = 19, cex = 0.5)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)

## ---- two-d3
cat(paste0('
<div id="gg" class="wind-fig"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const Y=[', paste(S$year, collapse=","), '];
const MN=[', paste(S$men, collapse=","), '];
const WM=[', paste(S$women, collapse=","), '];
const GP=[', paste(S$gap, collapse=","), '];
const box=d3.select("#gg");
const ctl=box.append("div").attr("style","margin:0.2em 0 0.6em 0;font-size:0.92em");
let mode="both";
[["both","the two series"],["gap","the gap alone"]].forEach(([k,lab])=>{
  ctl.append("button").attr("type","button").text(lab)
    .attr("id","b-"+k)
    .attr("style","margin-right:0.4em;padding:3px 9px;cursor:pointer")
    .on("click",()=>{mode=k;draw();});
});
const w=680,h=300,m={t:16,r:96,b:34,l:46};
const svg=box.append("svg").attr("viewBox","0 0 "+w+" "+h)
  .attr("style","max-width:100%;height:auto;font-family:inherit;font-size:12px");
const x=d3.scaleLinear().domain([d3.min(Y),d3.max(Y)]).range([m.l,w-m.r]);
const y=d3.scaleLinear().domain([-12,34]).range([h-m.b,m.t]);
svg.append("g").attr("transform","translate(0,"+(h-m.b)+")")
  .call(d3.axisBottom(x).ticks(8).tickFormat(d3.format("d")))
  .call(g=>g.select(".domain").remove());
svg.append("g").attr("transform","translate("+m.l+",0)")
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>(d>0?"+":"")+d))
  .call(g=>g.select(".domain").remove());
svg.append("line").attr("x1",m.l).attr("x2",w-m.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("class","rule").attr("stroke-dasharray","3,3");
const ln=d3.line().x((d,i)=>x(Y[i])).y(d=>y(d));
const paths={
  men:  svg.append("path").attr("fill","none").attr("stroke","', MEN, '").attr("stroke-width",2.2),
  women:svg.append("path").attr("fill","none").attr("stroke","', WOM, '").attr("stroke-width",2.2),
  gap:  svg.append("path").attr("fill","none").attr("stroke","', GAP, '").attr("stroke-width",2.2)
};
const labs=svg.append("g");
function draw(){
  d3.selectAll("#gg button").attr("style",function(){
    const on=this.id==="b-"+mode;
    return "margin-right:0.4em;padding:3px 9px;cursor:pointer;"+
      (on?"font-weight:600;background:#eee":"");});
  paths.men.attr("d",mode==="both"?ln(MN):null).attr("opacity",mode==="both"?1:0);
  paths.women.attr("d",mode==="both"?ln(WM):null).attr("opacity",mode==="both"?1:0);
  paths.gap.attr("d",mode==="gap"?ln(GP):null).attr("opacity",mode==="gap"?1:0);
  labs.selectAll("*").remove();
  const last=Y.length-1;
  const put=(v,txt,col)=>labs.append("text").attr("x",x(Y[last])+7).attr("y",y(v)+4)
    .attr("fill",col).attr("font-size","11px").text(txt);
  if(mode==="both"){ put(WM[last],"women "+(WM[last]>0?"+":"")+WM[last].toFixed(1),"', WOM, '");
                     put(MN[last],"men "+(MN[last]>0?"+":"")+MN[last].toFixed(1),"', MEN, '"); }
  else put(GP[last],"gap "+GP[last].toFixed(1),"', GAP, '");
}
draw();
})();
</script>'))

## ---- two-static
par(mar = c(3.2, 4.2, 1.4, 6.5))
plot(S$year, S$women, type = "n", xlab = "", ylab = "net Democratic, points",
     ylim = c(-12, 34), axes = FALSE)
abline(h = 0, col = "grey60", lty = 3)
lines(S$year, S$women, col = WOM, lwd = 2.2)
lines(S$year, S$men,   col = MEN, lwd = 2.2)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)
ly <- nrow(S)
text(S$year[ly] + 1, S$women[ly], paste("women", sg(S$women[ly])),
     col = WOM, adj = 0, cex = 0.78, xpd = NA)
text(S$year[ly] + 1, S$men[ly], paste("men", sg(S$men[ly])),
     col = MEN, adj = 0, cex = 0.78, xpd = NA)

## ---- decade-table
data.frame(
  Decade  = D$decade,
  Years   = D$years,
  Men     = sg(D$men),
  Women   = sg(D$women),
  Gap     = p1(D$gap),
  check.names = FALSE)

## ---- marital-table
LM <- M[which.max(M$year), ]
data.frame(
  Group = c("Unmarried women", "Married women", "Unmarried men", "Married men"),
  `Net Democratic` = sg(c(LM$unmarried_women, LM$married_women,
                          LM$unmarried_men, LM$married_men)),
  check.names = FALSE)

## ---- checks-table
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
