# poll-weighting-code.R -- chunk bodies for poll-weighting-brief.Rmd
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

R  <- read.csv("data/derived/fl_respondents.csv", stringsAsFactors = FALSE)
TG <- read.csv("data/derived/targets.csv",        stringsAsFactors = FALSE)
SC <- read.csv("data/derived/schemes.csv",        stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",          stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",         stringsAsFactors = FALSE)

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
sg <- function(x) sprintf("%+.1f", as.numeric(x))     # a margin always signed

knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

BLU <- "#2166ac"   # Clinton, everywhere in this chapter
RED <- "#b2182b"   # Trump

## ---- ladder-table
lad <- SC[SC$scheme %in% c("As collected", "Registered voters (Times)",
                           "Likely voters (Times, published)"), ]
data.frame(
  `Weighted how`     = lad$scheme,
  `What it assumes`  = lad$what_it_assumes,
  Margin             = sg(lad$margin),
  Leader             = lad$leader,
  check.names = FALSE)

## ---- ladder-d3
cat(paste0('
<style>
/* Classed rather than filled: an unfilled text defaults to black and
   vanishes on a dark page, and a class degrades better than var() in a
   presentation attribute, which falls back to black if it fails to parse. */
#ladder{color:var(--ink)}
#ladder .lb-zero{stroke:var(--ink-3);stroke-dasharray:3,3}
#ladder .lb-mut{fill:var(--ink-3)}
#ladder .lb-lab{fill:var(--ink)}
#ladder .lb-val{fill:var(--ink-2)}
#ladder line.lb-act{stroke:', RED, '}
#ladder text.lb-act{fill:', RED, '}
</style>
<div id="ladder" style="margin:1.2em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const L=[', paste(sprintf('["%s",%s]', lad$scheme, lad$margin), collapse=","), '];
const ACT=', F("actual_margin"), ';
const w=680,h=180,m={t:44,r:24,b:34,l:210};
const s=d3.select("#ladder").append("svg").attr("viewBox","0 0 "+w+" "+h)
  .attr("style","max-width:100%;height:auto;font-family:inherit;font-size:12px");
const x=d3.scaleLinear().domain([-4,11]).range([m.l,w-m.r]);
const y=d3.scalePoint().domain(L.map(d=>d[0])).range([m.t,h-m.b]).padding(0.6);
// Each rule is capped by its own label. They sit two points apart on the
// axis, so the labels are staggered and each line is drawn up to meet its
// own -- below the axis the caption read as a title for the axis instead.
const foot=h-m.b+4;
s.append("line").attr("class","lb-act").attr("stroke-width",1.5)
  .attr("x1",x(ACT)).attr("x2",x(ACT)).attr("y1",15).attr("y2",foot);
s.append("text").attr("class","lb-act")
  .attr("x",x(ACT)).attr("y",12).attr("text-anchor","middle")
  .text("what happened in November");
s.append("line").attr("class","lb-zero")
  .attr("x1",x(0)).attr("x2",x(0)).attr("y1",34).attr("y2",foot);
s.append("text").attr("class","lb-mut")
  .attr("x",x(0)).attr("y",31).attr("text-anchor","middle").text("tied");
L.forEach(d=>{
  s.append("line").attr("x1",x(0)).attr("x2",x(d[1])).attr("y1",y(d[0])).attr("y2",y(d[0]))
    .attr("stroke",d[1]>0?"', BLU, '":"', RED, '").attr("stroke-width",7)
    .attr("stroke-linecap","round").attr("opacity",0.85);
  s.append("text").attr("class","lb-lab")
    .attr("x",m.l-10).attr("y",y(d[0])+4).attr("text-anchor","end").text(d[0]);
  s.append("text").attr("class","lb-val")
    .attr("x",x(d[1])+(d[1]>0?11:-11)).attr("y",y(d[0])+4)
    .attr("text-anchor",d[1]>0?"start":"end")
    .text((d[1]>0?"+":"")+d[1].toFixed(1));
});
s.append("g").attr("transform","translate(0,"+(h-m.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>(d>0?"+":"")+d))
  .call(g=>g.select(".domain").remove());
})();
</script>'))

## ---- ladder-static
par(mar = c(2.6, 12, 3.0, 3))
plot(NA, xlim = c(-4, 11), ylim = c(0.5, 3.5), axes = FALSE, xlab = "", ylab = "")
abline(v = 0, lty = 3, col = "grey45")
abline(v = FN("actual_margin"), col = RED, lwd = 1.6)
for (i in seq_len(nrow(lad))) {
  yy <- nrow(lad) - i + 1
  segments(0, yy, lad$margin[i], yy, lwd = 7, lend = 1,
           col = ifelse(lad$margin[i] > 0, BLU, RED))
  text(lad$margin[i] + ifelse(lad$margin[i] > 0, 0.45, -0.45), yy,
       sg(lad$margin[i]), cex = 0.8, adj = ifelse(lad$margin[i] > 0, 0, 1))
}
axis(1, at = seq(-4, 10, 2), labels = sprintf("%+d", seq(-4, 10, 2)), cex.axis = 0.8)
mtext(rev(lad$scheme), side = 2, at = seq_len(nrow(lad)), las = 1, cex = 0.75, line = 0.5)
# above its own rule, not under the axis, where it read as the axis title
mtext("what happened in November", side = 3, at = FN("actual_margin"),
      line = 0.2, cex = 0.7, col = RED)
mtext("tied", side = 3, at = 0, line = -1.1, cex = 0.7, col = "grey45")

## ---- four-table
data.frame(
  Pollster = c("Omero, Green and Rosenblatt", "Charles Franklin",
               "The Times and Siena, as published", "Patrick Ruffini",
               "Gelman, Rothschild and Corbett-Davies"),
  Result   = c("Clinton +4", "Clinton +3", "Clinton +1", "Clinton +1", "Trump +1"),
  check.names = FALSE)

## ---- targets-table
tt <- TG[TG$variable %in% c("educ4", "party3"), ]
data.frame(
  Variable      = ifelse(tt$variable == "educ4", "Education", "Party registration"),
  Level         = tt$level,
  `As collected` = p1(tt$as_collected),
  `Registered`   = p1(tt$registered),
  `Likely voters` = p1(tt$likely),
  check.names = FALSE)

## ---- rake-d3
V   <- c("educ4", "race4", "party3", "age4", "turnout3", "region5")
VL  <- c("education", "race", "party registration", "age", "turnout history", "region")
lev <- lapply(V, function(v) sort(unique(R[[v]])))
names(lev) <- V

# Respondent rows as small integers: vote, then the level index of each variable.
code <- sapply(V, function(v) match(R[[v]], lev[[v]]))
rows <- paste0("[", apply(cbind(ifelse(is.na(R$vote), 0,
                                       ifelse(R$vote == "Clinton", 1, 2)), code),
                          1, paste, collapse = ","), "]")

tj <- function(v, col) {
  t <- TG[TG$variable == v, ]
  paste0("[", paste(round(t[[col]][match(lev[[v]], t$level)], 2), collapse = ","), "]")
}
cat(paste0('
<style>
#rk{color:var(--ink)}
#rk .rk-hd{font-weight:600;margin-bottom:0.4em}
#rk .rk-note{color:var(--ink-3);font-size:0.82em;line-height:1.4;margin:0 0 0.8em}
#rk .rk-var{display:flex;align-items:baseline;gap:0.6em;font-size:0.9em;
  font-weight:600;color:var(--ink);margin-bottom:0.15em}
#rk .rk-row{display:flex;align-items:center;gap:0.5em;font-size:0.85em;margin:0.12em 0}
#rk .rk-lv{width:8em;flex:none;color:var(--ink-2)}
#rk .rk-track{position:relative;flex:1;min-width:64px;display:flex;align-items:center}
#rk .rk-track input{width:100%;margin:0;accent-color:var(--accent)}
/* taller than the thumb, so the default stays visible under it */
#rk .rk-tick{position:absolute;top:-3px;bottom:-3px;width:2px;background:var(--ink-3);
  opacity:.75;pointer-events:none}
#rk .rk-val{width:6.9em;flex:none;text-align:right;color:var(--ink);
  white-space:nowrap;font-variant-numeric:tabular-nums}
#rk .rk-d{color:var(--ink-3)}
#rk .rk-mut{color:var(--ink-3)}
#rk button{font:inherit;font-size:0.86em;cursor:pointer}
#rk .rk-rst{font-weight:400;color:var(--ink-3);background:none;border:0;
  border-bottom:1px solid var(--rule);padding:0}
#rk .rk-rst:hover{color:var(--accent);border-bottom-color:var(--accent)}
#rk svg text{fill:currentColor}
#rk .rk-zero{stroke:var(--ink-3);stroke-dasharray:3,3}
#rk line.pub{stroke:var(--ink-2)}
#rk text.pub{fill:var(--ink-2)}
#rk line.act{stroke:', RED, '}
#rk text.act{fill:', RED, '}
</style>
<div id="rk" style="margin:1em 0"></div>
<script>
(function(){
const ROWS=[', paste(rows, collapse = ","), '];
const V=', paste0('["', paste(V, collapse = '","'), '"]'), ';
const VL=', paste0('["', paste(VL, collapse = '","'), '"]'), ';
const LEV=[', paste(sapply(V, function(v)
  paste0('["', paste(lev[[v]], collapse = '","'), '"]')), collapse = ","), '];
const T_COLL=[', paste(sapply(V, tj, col = "as_collected"), collapse = ","), '];
const T_LIKE=[', paste(sapply(V, tj, col = "likely"), collapse = ","), '];
const ACT=', F("actual_margin"), ', PUB=', F("margin_lv"), ';
let on=[true,false,false,false,false,false];
let tgt=T_LIKE.map(a=>a.slice());

const NV=ROWS.filter(r=>r[0]).length;

function margin(w){let c=0,t=0;
  for(let i=0;i<ROWS.length;i++){const v=ROWS[i][0]; if(!v) continue;
    if(v===1) c+=w[i]; else t+=w[i];}
  return 100*(c-t)/(c+t);}

// Kish: how many unweighted answers this set of weights is worth.
function neff(w){let s=0,q=0;
  for(let i=0;i<ROWS.length;i++){if(!ROWS[i][0]) continue; s+=w[i]; q+=w[i]*w[i];}
  return q>0?s*s/q:0;}

function rake(){
  let w=new Array(ROWS.length).fill(1);
  const use=V.map((_,k)=>k).filter(k=>on[k]);
  if(!use.length) return w;
  for(let it=0;it<60;it++) use.forEach(k=>{
    const nl=LEV[k].length, cur=new Array(nl).fill(0); let tot=0;
    for(let i=0;i<ROWS.length;i++){cur[ROWS[i][k+1]-1]+=w[i]; tot+=w[i];}
    const f=cur.map((c,j)=>{const want=tgt[k][j]/100*tot; return c>0?want/c:1;});
    for(let i=0;i<ROWS.length;i++) w[i]*=f[ROWS[i][k+1]-1];
  });
  return w;
}

const box=d3.select("#rk");
const grid=box.append("div").attr("style",
  "display:grid;grid-template-columns:minmax(180px,1fr) minmax(240px,1.5fr);gap:1.1em;align-items:start");
const left=grid.append("div"), right=grid.append("div");
left.append("div").attr("class","rk-hd").text("Correct for…");
right.append("div").attr("class","rk-hd").text("…assuming the electorate looks like this");
right.append("div").attr("class","rk-note")
  .text("Shares of the assumed electorate, not of the ', F("n"), ' respondents, so each "
       +"variable adds to 100%: move one level and the rest give up or take back the "
       +"difference. The grey mark on each track is the Times’ own likely-voter target.");

V.forEach((v,k)=>{
  const row=left.append("label").attr("style",
    "display:block;margin:0.18em 0;cursor:pointer;font-size:0.93em");
  row.append("input").attr("type","checkbox").property("checked",on[k])
    .attr("style","margin-right:0.5em").on("change",function(){on[k]=this.checked;draw();});
  row.append("span").text(VL[k]);
});
const preset=left.append("div").attr("style","margin-top:0.8em;font-size:0.9em");
preset.append("div").attr("class","rk-mut").attr("style","margin-bottom:0.25em")
  .text("Targets:");
[["the Times’ likely voters",()=>T_LIKE],["the sample as collected",()=>T_COLL]]
 .forEach(([lab,f])=>{
  preset.append("button").attr("type","button").text(lab)
    .attr("style","margin:0 0.35em 0.35em 0;padding:2px 7px;font-size:0.88em;cursor:pointer")
    .on("click",()=>{tgt=f().map(a=>a.slice());draw();});
});

const sliders=right.append("div");
const readout=box.append("div").attr("style","margin-top:1.1em");
const svg=readout.append("svg").attr("viewBox","0 0 700 96")
  .attr("style","max-width:100%;height:auto;font-family:inherit;font-size:12px");
const LO=-8, HI=12;
const x=d3.scaleLinear().domain([LO,HI]).range([40,640]);
const xc=v=>x(Math.max(LO,Math.min(HI,v)));   // the axis is fixed; the dial is not
svg.append("g").attr("transform","translate(0,66)")
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>(d>0?"+":"")+d))
  .call(g=>g.select(".domain").remove());
svg.append("line").attr("class","rk-zero")
  .attr("x1",x(0)).attr("x2",x(0)).attr("y1",14).attr("y2",66);
[[PUB,"pub","published"],[ACT,"act","November"]].forEach(([val,cls,lab])=>{
  svg.append("line").attr("class",cls).attr("stroke-width",1.2)
    .attr("x1",x(val)).attr("x2",x(val)).attr("y1",14).attr("y2",66);
  svg.append("text").attr("class",cls).attr("x",x(val)).attr("y",11)
    .attr("text-anchor","middle").attr("font-size","10px").text(lab);
});
const bar=svg.append("rect").attr("y",30).attr("height",18).attr("rx",2);
const lbl=svg.append("text").attr("y",43).attr("font-weight","600");
const foot=readout.append("div").attr("class","rk-note")
  .attr("style","margin:0.35em 0 0");

// A target is a share of the assumed electorate, so the five (or three, or
// four) levels of a variable have to add to 100. Move one and the others
// absorb the difference in proportion -- which also makes the handler
// idempotent, so holding a slider at its maximum stops drifting the number.
function setTarget(k,j,val){
  const a=tgt[k], v=Math.max(0,Math.min(100,val)), rest=100-v;
  const others=a.reduce((s,y,i)=>i===j?s:s+y,0);
  a.forEach((y,i)=>{ if(i!==j) a[i]= others>0 ? y*rest/others : rest/(a.length-1); });
  a[j]=v;
  draw(false);
}

function drawSliders(){
  sliders.selectAll("*").remove();
  if(!on.some(Boolean)){
    sliders.append("div").attr("class","rk-mut").attr("style","font-size:0.9em")
      .text("Nothing is being corrected, so there is no target to set.");return;}
  V.forEach((v,k)=>{ if(!on[k]) return;
    const blk=sliders.append("div").attr("style","margin-bottom:0.7em");
    const hd=blk.append("div").attr("class","rk-var");
    hd.append("span").text(VL[k]);
    hd.append("button").attr("type","button").attr("class","rk-rst")
      .attr("title","back to the Times’ likely-voter target").text("reset")
      .on("click",()=>{tgt[k]=T_LIKE[k].slice();draw(false);});
    LEV[k].forEach((lv,j)=>{
      const r=blk.append("div").attr("class","rk-row");
      r.append("span").attr("class","rk-lv").text(lv);
      const tr=r.append("div").attr("class","rk-track");
      const d=T_LIKE[k][j];
      tr.append("div").attr("class","rk-tick")
        .attr("title","the Times’ target: "+d.toFixed(1)+"%")
        .attr("style","left:calc(7px + "+d+"% - "+(d*0.14).toFixed(2)+"px)");
      tr.append("input").attr("type","range").attr("min",0).attr("max",100)
        .attr("step",0.1).attr("class","s-"+k+"-"+j)
        .on("input",function(){setTarget(k,j,+this.value);});
      r.append("span").attr("class","rk-val v-"+k+"-"+j);
    });
  });
}
function syncSliders(){
  V.forEach((v,k)=>{ if(!on[k]) return;
    LEV[k].forEach((lv,j)=>{
      const t=tgt[k][j], d=t-T_LIKE[k][j];
      d3.select(".s-"+k+"-"+j).property("value",t);
      const cell=d3.select(".v-"+k+"-"+j).text(t.toFixed(1)+"%");
      cell.append("span").attr("class","rk-d")
        .text(Math.abs(d)<0.05?"":(d>0?" +":" -")+Math.abs(d).toFixed(1));
    });
  });
}
function draw(rebuild=true){
  if(rebuild) drawSliders();
  syncSliders();
  const w=rake(), m=margin(w), blu="', BLU, '", red="', RED, '";
  const px=xc(m), col=m>0?blu:red, inside=px>600||px<100;   // else the label clips
  bar.attr("x",Math.min(x(0),px)).attr("width",Math.abs(px-x(0))).attr("fill",col);
  // .style, NOT .attr. The "#rk svg text{fill:currentColor}" rule above keeps
  // the axis legible in both themes, and a stylesheet declaration beats a
  // presentation attribute -- so an .attr("fill") here is silently discarded
  // and the label comes out ink-coloured whatever the bar is doing. White is
  // safe inside the bar in both themes, because the bar colour is fixed.
  lbl.attr("x",m>0?px+(inside?-8:8):px-(inside?-8:8))
     .attr("text-anchor",(m>0)!==inside?"start":"end")
     .style("fill",inside?"#fff":col)
     .text((m>0?"Clinton +":"Trump +")+Math.abs(m).toFixed(1));
  foot.text("Effective sample size "+Math.round(neff(w))+" of the "+NV
    +" respondents who named a candidate: the more the weights vary, the fewer "
    +"independent answers they are worth."
    +((m<LO||m>HI)?" The bar is pinned at the end of the axis.":""));
}
draw();
})();
</script>'))

## ---- rake-static
par(mar = c(3.4, 13, 1.2, 3))
s2 <- SC[order(SC$margin), ]
plot(NA, xlim = c(-3, 11), ylim = c(0.5, nrow(s2) + 0.5), axes = FALSE,
     xlab = "", ylab = "")
abline(v = 0, lty = 3, col = "grey45")
abline(v = FN("actual_margin"), col = RED, lwd = 1.6)
for (i in seq_len(nrow(s2))) {
  segments(0, i, s2$margin[i], i, lwd = 6, lend = 1,
           col = ifelse(s2$margin[i] > 0, BLU, RED))
  text(s2$margin[i] + ifelse(s2$margin[i] > 0, 0.4, -0.4), i,
       sg(s2$margin[i]), cex = 0.72, adj = ifelse(s2$margin[i] > 0, 0, 1))
}
axis(1, at = seq(-2, 10, 2), labels = sprintf("%+d", seq(-2, 10, 2)), cex.axis = 0.78)
mtext(s2$scheme, side = 2, at = seq_len(nrow(s2)), las = 1, cex = 0.7, line = 0.5)

## ---- which-table
w2 <- SC[SC$scheme %in% c("As collected", "Education only", "Party registration only",
                          "Education and race", "Everything at once"), ]
data.frame(
  `Corrected for` = w2$scheme,
  Margin          = sg(w2$margin),
  Leader          = w2$leader,
  check.names = FALSE)

## ---- checks-table
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
