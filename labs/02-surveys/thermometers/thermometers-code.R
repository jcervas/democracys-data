# thermometers-code.R -- chunk bodies for thermometers-brief.Rmd
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

S  <- read.csv("data/derived/thermometers_by_year.csv", stringsAsFactors = FALSE)
Z  <- read.csv("data/derived/out_party_zero.csv",       stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",                stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",               stringsAsFactors = FALSE)

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)

knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

INP <- "#1b7837"; OUT <- "#762a83"

## ---- heap-table
data.frame(
  Rating = c("0 — as cold as possible", "50 — the midpoint", "85",
             paste(F("top_code"), "— the top of the scale")),
  `Share of all ratings` = paste0(c(p1(F("pct_at_0")), p1(F("pct_at_50")),
                                    p1(F("pct_at_85")), p1(F("pct_at_97"))), "%"),
  check.names = FALSE)

## ---- halves-d3
cat(paste0('
<div id="th" class="wind-fig"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const Y=[', paste(S$year, collapse=","), '];
const IN=[', paste(S$in_party, collapse=","), '];
const OU=[', paste(S$out_party, collapse=","), '];
const GP=[', paste(S$gap, collapse=","), '];
const box=d3.select("#th");
const ctl=box.append("div").attr("style","margin:0.2em 0 0.6em 0;font-size:0.92em");
let mode="both";
[["both","the two halves"],["gap","the gap alone"]].forEach(([k,lab])=>{
  ctl.append("button").attr("type","button").text(lab).attr("id","t-"+k)
    .attr("style","margin-right:0.4em;padding:3px 9px;cursor:pointer")
    .on("click",()=>{mode=k;draw();});
});
const w=680,h=300,m={t:16,r:104,b:34,l:44};
const svg=box.append("svg").attr("viewBox","0 0 "+w+" "+h)
  .attr("style","max-width:100%;height:auto;font-family:inherit;font-size:12px");
const x=d3.scaleLinear().domain([d3.min(Y),d3.max(Y)]).range([m.l,w-m.r]);
const y=d3.scaleLinear().domain([0,80]).range([h-m.b,m.t]);
svg.append("g").attr("transform","translate(0,"+(h-m.b)+")")
  .call(d3.axisBottom(x).ticks(8).tickFormat(d3.format("d")))
  .call(g=>g.select(".domain").remove());
svg.append("g").attr("transform","translate("+m.l+",0)")
  .call(d3.axisLeft(y).ticks(5)).call(g=>g.select(".domain").remove());
svg.append("line").attr("x1",m.l).attr("x2",w-m.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("class","rule-2").attr("stroke-dasharray","3,3");
svg.append("text").attr("x",m.l+4).attr("y",y(50)-4).attr("class","lbl")
  .attr("font-size","10px").text("neither warm nor cold");
const ln=d3.line().x((d,i)=>x(Y[i])).y(d=>y(d));
const pIn=svg.append("path").attr("fill","none").attr("stroke","', INP, '").attr("stroke-width",2.4);
const pOu=svg.append("path").attr("fill","none").attr("stroke","', OUT, '").attr("stroke-width",2.4);
const pGp=svg.append("path").attr("fill","none").attr("class","ttl-stroke").attr("stroke-width",2.4);
const labs=svg.append("g");
function draw(){
  d3.selectAll("#th button").attr("style",function(){
    const on=this.id==="t-"+mode;
    return "margin-right:0.4em;padding:3px 9px;cursor:pointer;"+(on?"font-weight:600;background:var(--rule)":"");});
  pIn.attr("d",mode==="both"?ln(IN):null).attr("opacity",mode==="both"?1:0);
  pOu.attr("d",mode==="both"?ln(OU):null).attr("opacity",mode==="both"?1:0);
  pGp.attr("d",mode==="gap"?ln(GP):null).attr("opacity",mode==="gap"?1:0);
  labs.selectAll("*").remove();
  const L=Y.length-1;
  const put=(v,t,c)=>labs.append("text").attr("x",x(Y[L])+7).attr("y",y(v)+4)
    .style("fill",c).attr("font-size","11px").text(t);
  if(mode==="both"){ put(IN[L],"own party "+IN[L].toFixed(0),"', INP, '");
                     put(OU[L],"other party "+OU[L].toFixed(0),"', OUT, '"); }
  else put(GP[L],"gap "+GP[L].toFixed(0),"var(--ink)");
}
draw();
})();
</script>'))

## ---- halves-static
par(mar = c(3.2, 4.0, 1.2, 7.2))
plot(S$year, S$in_party, type = "n", ylim = c(0, 80), axes = FALSE,
     xlab = "", ylab = "degrees")
abline(h = 50, lty = 3, col = "grey65")
lines(S$year, S$in_party,  col = INP, lwd = 2.4)
lines(S$year, S$out_party, col = OUT, lwd = 2.4)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)
L <- nrow(S)
text(S$year[L] + 1, S$in_party[L],  paste("own party", round(S$in_party[L])),
     col = INP, adj = 0, cex = 0.75, xpd = NA)
text(S$year[L] + 1, S$out_party[L], paste("other party", round(S$out_party[L])),
     col = OUT, adj = 0, cex = 0.75, xpd = NA)

## ---- zero-static
par(mar = c(3.2, 4.4, 1.2, 1.2))
plot(Z$year, Z$pct_zero, type = "n", axes = FALSE, xlab = "",
     ylab = "% rating them 0", ylim = c(0, max(Z$pct_zero) * 1.1))
lines(Z$year, Z$pct_zero, col = OUT, lwd = 2.2)
points(Z$year, Z$pct_zero, col = OUT, pch = 19, cex = 0.55)
axis(1, cex.axis = 0.85); axis(2, las = 1, cex.axis = 0.85)

## ---- checks-table
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)
