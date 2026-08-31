# mid-decade-code.R -- chunk bodies for mid-decade-brief.Rmd
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

long  <- read.csv("data/derived/district_shares.csv",  stringsAsFactors = FALSE)
seats <- read.csv("data/derived/seats_by_election.csv", stringsAsFactors = FALSE)
summ  <- read.csv("data/derived/district_summary.csv",  stringsAsFactors = FALSE)
facts <- read.csv("data/derived/facts.csv",             stringsAsFactors = FALSE)
fx <- function(k) facts$value[facts$key == k]

# elections in ballot-ish order within year: president first, then senator,
# then the state executive offices, then the rest as they come
OFF_ORDER <- c("pres", "senator", "gov", "ltgov", "ag", "compt", "sc", "trea")
el_office <- sub("-[0-9]{4}$", "", seats$election)
seats <- seats[order(seats$year, match(el_office, OFF_ORDER)), ]
row.names(seats) <- NULL

# Two columns keep the collection's own shorthand ("sc", "trea"); the rest
# get the office spelled out. See the note under Figure 2.
OFFICE <- c(pres = "President", senator = "U.S. Senate", gov = "Governor",
            ltgov = "Lt. Governor", ag = "Attorney General",
            compt = "Comptroller", sc = "Supreme Court", trea = "trea")
el_label <- function(e)
  paste(OFFICE[sub("-[0-9]{4}$", "", e)], sub("^.*-", "", e))
seats$label <- el_label(seats$election)

# one shared color rule for every figure, HTML and PDF alike: Democratic
# two-party share, capped 20 points either side of 50-50
CAP <- 20
pal <- colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(101)
share_fill <- function(s) {
  m <- pmax(pmin(100 * (2 * s - 1), CAP), -CAP)
  pal[round((m + CAP) / (2 * CAP) * 100) + 1]
}

# the tween geometry: both plans' rings, point-for-point aligned
geo <- jsonlite::fromJSON("data/derived/tx-districts-tween.json",
                          simplifyVector = FALSE)
VBW <- geo$viewBox[[1]]; VBH <- geo$viewBox[[2]]
ring_xy <- function(flat) {
  v <- unlist(flat)
  list(x = v[seq(1, length(v), 2)], y = v[seq(2, length(v), 2)])
}

# wide share matrices in the seats-table election order, for the figures
O <- reshape(long[c("district", "election", "share_old")],
             idvar = "district", timevar = "election", direction = "wide")
N <- reshape(long[c("district", "election", "share_new")],
             idvar = "district", timevar = "election", direction = "wide")
O <- O[order(O$district), paste0("share_old.", seats$election)]
N <- N[order(N$district), paste0("share_new.", seats$election)]

# every (old, new) district pair sharing population, from the 2020 census
# block groups assigned into both plans by build-bg-overlap.R
overlap <- read.csv("data/derived/tx_overlap_pop.csv", stringsAsFactors = FALSE)

# Render every data.frame in this document as a TABLE, not as console output.
# Without this the chapter's output depends on WHICH OTHER CHAPTERS rendered
# before it: render-brief.R builds every brief in one R session, and a sibling
# chapter's identical registration lands in knitr's namespace and stays there.
# Nothing in this chapter prints a bare data.frame today, so the registration
# changes no page as it stands -- it is here so that the first one that does
# renders the same way whether the chapter is built alone or with the corpus.
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- mm-static
# print fallback: both plans side by side, colored by the 2024 presidential race
par(mar = c(0.2, 0.2, 1.4, 0.2), mfrow = c(1, 2))
for (plan in c("old", "new")) {
  s <- if (plan == "old") summ$pres2024_old else summ$pres2024_new
  plot(NA, xlim = c(0, VBW), ylim = c(VBH, 0), asp = 1,
       axes = FALSE, ann = FALSE)
  for (i in seq_along(geo$districts)) {
    d <- geo$districts[[i]]
    r <- ring_xy(d[[plan]])
    polypath(r$x, r$y, col = share_fill(s[summ$district == d$id]),
             border = "#FFFFFF", lwd = 0.5)
  }
  mtext(if (plan == "old") "2021 lines" else "2025 lines",
        side = 3, line = 0.2, cex = 0.9)
}
par(mfrow = c(1, 1))
legend("bottomright", c("Republican", "even", "Democratic"),
       fill = c("#B2182B", "#F7F7F7", "#2166AC"), border = NA, bty = "n",
       cex = 0.7, title = "two-party share, president 2024")

## ---- mm-panel-static
# print fallback for the district panel: district 28, the online default
d28 <- long[long$district == 28, ]
d28 <- d28[match(seats$election, d28$election), ]
par(mar = c(4, 10, 2.2, 2))
yy <- rev(seq_len(nrow(d28)))
plot(NA, xlim = c(38, 72), ylim = range(yy) + c(-0.5, 0.5),
     axes = FALSE, ann = FALSE)
abline(v = 50, col = "#777777", lty = 3)
segments(100 * d28$share_old, yy, 100 * d28$share_new, yy,
         col = "#999999", lwd = 1.6)
points(100 * d28$share_old, yy, pch = 21, bg = "#FFFFFF", col = "#4E5A63", cex = 1.1)
points(100 * d28$share_new, yy, pch = 19,
       col = ifelse(d28$share_new > 0.5, "#2C7FB8", "#C41230"), cex = 1.1)
axis(1, at = seq(40, 70, 10), labels = paste0(seq(40, 70, 10), "%"), cex.axis = 0.8)
mtext("Democratic share of the two-party vote, district 28", side = 1,
      line = 2.4, cex = 0.85)
text(37.2, yy, seats$label, xpd = NA, adj = 1, cex = 0.72)
legend("bottomright", c("2021 lines", "2025 lines"), pch = c(21, 19),
       col = c("#4E5A63", "#2C7FB8"), pt.bg = c("#FFFFFF", NA),
       bty = "n", cex = 0.78)

## ---- mm-d3
GEO <- paste(readLines("data/derived/tx-districts-tween.json", warn = FALSE),
             collapse = "")
sh_row <- function(m, i) paste(sprintf("%.4f", as.numeric(m[i, ])), collapse = ",")
SH <- paste(vapply(seq_len(38), function(i)
  sprintf('{"d":%d,"o":[%s],"n":[%s]}', i, sh_row(O, i), sh_row(N, i)),
  character(1)), collapse = ",")
EL <- paste(sprintf('{"k":"%s","t":"%s"}', seats$election, seats$label),
            collapse = ",")
cat(paste0('
<script src="../../_lib/d3.v7.min.js"></script>
<div id="mm" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const G=', GEO, ';
const SH=[', SH, '];
const EL=[', EL, '];
const W=G.viewBox[0],H=G.viewBox[1];
const PAL=d3.interpolateRgbBasis(["#B2182B","#F7F7F7","#2166AC"]),CAP=20;
const fillOf=(s)=>{const m=Math.max(-CAP,Math.min(CAP,100*(2*s-1)));return PAL((m+CAP)/(2*CAP));};
const box=d3.select("#mm");
const bar=box.append("div").attr("style","margin-bottom:6px;font-size:13px;display:flex;gap:8px;flex-wrap:wrap;align-items:center");
const sel=bar.append("select").attr("style","font:inherit;padding:2px 4px");
sel.selectAll("option").data(EL).join("option").attr("value",d=>d.k).text(d=>d.t);
sel.property("value","pres-2024");
const bstyle="font:inherit;padding:2px 10px;border:1px solid #999;background:none;color:inherit;cursor:pointer;border-radius:3px";
const bOld=bar.append("button").attr("style",bstyle).text("2021 lines");
const bNew=bar.append("button").attr("style",bstyle).text("2025 lines");
const bPlay=bar.append("button").attr("style",bstyle).text("\\u25b6 morph");
bar.append("span").attr("style","margin-left:8px").text("District:");
const dsel=bar.append("select").attr("style","font:inherit;padding:2px 4px");
dsel.selectAll("option").data(d3.range(1,39)).join("option").attr("value",d=>d).text(d=>"District "+d);
dsel.property("value","28");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H+56}`)
  .attr("style","max-width:640px;width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:3.3em;margin:0.3em 0 0 0");
const DEF="Color: Democratic share of the two-party vote in the selected race, "+
  "red to blue, capped 20 points either side of even. "+
  "<i>Hover a district for its numbers; click it to load it below.</i>";
cap.html(DEF);
window.DD_SYNC=window.DD_SYNC||{fns:[],select(id){this.fns.forEach(fn=>fn(id));},
  subscribe(fn){this.fns.push(fn);}};
window.__TXGEO=G;
let elx="pres-2024",plan=1,selD=28;   // plan: 0 = 2021 lines, 1 = 2025 lines
const EIX=Object.fromEntries(EL.map((e,i)=>[e.k,i]));
const path1=(d,t)=>{const o=d.old,n=d.new;let p="";
  for(let i=0;i<o.length;i+=2){
    p+=(i?"L":"M")+(o[i]+(n[i]-o[i])*t).toFixed(1)+","+(o[i+1]+(n[i+1]-o[i+1])*t).toFixed(1);}
  return p+"Z";};
G.districts.forEach(d=>{d.t=1;});
const shOf=(id,which)=>SH[id-1][which][EIX[elx]];
const g=svg.append("g");
const paths=g.selectAll("path").data(G.districts).join("path")
  .attr("d",d=>path1(d,1)).attr("stroke","#fff").attr("stroke-width",0.7)
  .attr("cursor","pointer");
const hl=svg.append("path").attr("fill","none").attr("stroke","#111")
  .attr("stroke-width",2).attr("pointer-events","none");
function refill(dur){
  paths.transition("f").duration(dur||0)
    .attr("fill",d=>fillOf(shOf(d.id,plan?"n":"o")));
}
function morph(to){
  plan=to;
  paths.transition("m").duration(1100).ease(d3.easeCubicInOut)
    .attrTween("d",d=>{const s=d.t,e=to;return tt=>{d.t=s+(e-s)*tt;return path1(d,d.t);};});
  refill(1100); drawHl(1100); flag();
}
function flag(){
  bOld.style("border-color",plan?"#999":"#111").style("font-weight",plan?"400":"700");
  bNew.style("border-color",plan?"#111":"#999").style("font-weight",plan?"700":"400");
}
bOld.on("click",()=>morph(0)); bNew.on("click",()=>morph(1));
bPlay.on("click",()=>morph(1-plan));
sel.on("change",function(){elx=this.value;refill(400);panel();});
function drawHl(dur){
  const d=G.districts.find(x=>x.id===selD);
  hl.transition("h").duration(dur||0)
    .attrTween("d",()=>{const s=d.t;return tt=>path1(d,s+(plan-s)*tt);});
}
paths.on("mouseenter",function(e,d){
    d3.select(this).attr("stroke","#111").attr("stroke-width",1.6).raise();hl.raise();
    const o=shOf(d.id,"o"),n=shOf(d.id,"n");
    cap.html("<b>District "+d.id+"</b>, "+EL[EIX[elx]].t+": "+
      (100*o).toFixed(1)+"% Democratic under the 2021 lines, "+
      (100*n).toFixed(1)+"% under the 2025 lines"+
      ((o>0.5)!==(n>0.5)?" \\u2014 <b>the winner changes</b>.":"."));})
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#fff").attr("stroke-width",0.7);cap.html(DEF);})
  .on("click",(e,d)=>{selD=d.id;drawHl(0);panel();dsel.property("value",selD);window.DD_SYNC.select(d.id);});
dsel.on("change",function(){selD=+this.value;drawHl(0);panel();window.DD_SYNC.select(selD);});
window.DD_SYNC.subscribe(id=>{if(id!==selD){selD=id;drawHl(0);panel();dsel.property("value",selD);}});
// ---- color legend: without it the map does not carry its encoding ----
const LX=20,LY=H+18,LW=240;
const lg=svg.append("g");
lg.selectAll("rect").data(d3.range(0,101)).join("rect")
  .attr("x",v=>LX+v*LW/101).attr("y",LY).attr("width",LW/101+0.7).attr("height",10)
  .attr("fill",v=>PAL(v/100));
lg.append("text").attr("x",LX).attr("y",LY+24).attr("font-size","11px")
  .attr("fill","#555").text("Republican");
lg.append("text").attr("x",LX+LW).attr("y",LY+24).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#555").text("Democratic");
lg.append("text").attr("x",LX+LW/2).attr("y",LY+24).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#555").text("even");
// ---------- the district panel ----------
const pw=800,ph=430,pm={t:44,r:250,b:34,l:120};
const psvg=box.append("svg").attr("viewBox",`0 0 ${pw} ${ph}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
function panel(){
  psvg.selectAll("*").remove();
  const d=G.districts.find(x=>x.id===selD);
  const rows=EL.map((e,i)=>({t:e.t,k:e.k,o:SH[selD-1].o[i],n:SH[selD-1].n[i]}));
  const x=d3.scaleLinear().domain([0.2,0.8]).range([pm.l,pw-pm.r]);
  const y=d3.scaleBand().domain(rows.map(r=>r.t)).range([pm.t,ph-pm.b]).padding(0.35);
  psvg.append("text").attr("x",pm.l).attr("y",20).attr("font-weight","700")
    .attr("font-size","14px").attr("fill","#12181D")
    .text("District "+selD+", every election, both maps");
  psvg.append("text").attr("x",pm.l).attr("y",36).attr("font-size","11px").attr("fill","#777")
    .text("open circle: 2021 lines \\u00b7 filled: 2025 lines \\u00b7 right of the line = Democratic candidate ahead");
  psvg.append("line").attr("x1",x(0.5)).attr("x2",x(0.5)).attr("y1",pm.t-4).attr("y2",ph-pm.b)
    .attr("stroke","#777").attr("stroke-dasharray","3 3");
  psvg.append("g").attr("transform",`translate(0,${ph-pm.b})`)
    .call(d3.axisBottom(x).ticks(6).tickFormat(v=>Math.round(100*v)+"%"));
  const rg=psvg.selectAll("g.r").data(rows).join("g");
  rg.append("text").attr("x",pm.l-8).attr("y",r=>y(r.t)+y.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","11px").attr("fill","#4E5A63").text(r=>r.t);
  rg.append("line").attr("x1",r=>x(r.o)).attr("x2",r=>x(r.n))
    .attr("y1",r=>y(r.t)+y.bandwidth()/2).attr("y2",r=>y(r.t)+y.bandwidth()/2)
    .attr("stroke","#999").attr("stroke-width",1.2);
  rg.append("circle").attr("cx",r=>x(r.o)).attr("cy",r=>y(r.t)+y.bandwidth()/2)
    .attr("r",4.5).attr("fill","var(--paper,#fff)").attr("stroke","#4E5A63").attr("stroke-width",1.4);
  rg.append("circle").attr("cx",r=>x(r.n)).attr("cy",r=>y(r.t)+y.bandwidth()/2)
    .attr("r",4.5).attr("fill",r=>r.n>0.5?"#2166AC":"#B2182B");
  // inset: the district itself, old dashed over new solid
  const ins=psvg.append("g"),IS=200,ix=pw-pm.r+30,iy=pm.t+20;
  const pts=[];for(let i=0;i<d.old.length;i+=2){pts.push(d.old[i],d.old[i+1],d.new[i],d.new[i+1]);}
  const xs=pts.filter((_,i)=>i%2===0),ys=pts.filter((_,i)=>i%2===1);
  const bx=[Math.min(...xs),Math.max(...xs)],by=[Math.min(...ys),Math.max(...ys)];
  const k=Math.min(IS/(bx[1]-bx[0]),IS/(by[1]-by[0]));
  const tp=(flat)=>{let p="";for(let i=0;i<flat.length;i+=2){
    p+=(i?"L":"M")+(ix+(flat[i]-bx[0])*k).toFixed(1)+","+(iy+(flat[i+1]-by[0])*k).toFixed(1);}return p+"Z";};
  ins.append("path").attr("d",tp(d.new)).attr("fill",fillOf(SH[selD-1].n[EIX[elx]]))
    .attr("stroke","#4E5A63").attr("stroke-width",1.2);
  ins.append("path").attr("d",tp(d.old)).attr("fill","none")
    .attr("stroke","#12181D").attr("stroke-width",1.2).attr("stroke-dasharray","5 3");
  ins.append("text").attr("x",ix).attr("y",iy+IS+26).attr("font-size","11px").attr("fill","#777")
    .text("dashed: 2021 lines \\u00b7 solid: 2025 lines");
  ins.append("text").attr("x",ix).attr("y",iy+IS+42).attr("font-size","11px").attr("fill","#777")
    .text("fill: the selected race under the 2025 lines");
}
refill(0);drawHl(0);flag();panel();
})();
</script>'))

## ---- sb-static
par(mar = c(4, 11, 2.4, 6))
yy <- rev(seq_len(nrow(seats)))
plot(NA, xlim = c(23, 31), ylim = range(yy) + c(-0.5, 0.5),
     axes = FALSE, ann = FALSE)
abline(v = 23:31, col = "#EEEEEE")
segments(seats$rep_old, yy, seats$rep_new, yy, col = "#999999", lwd = 1.6)
points(seats$rep_old, yy, pch = 21, bg = "#FFFFFF", col = "#4E5A63", cex = 1.15)
points(seats$rep_new, yy, pch = 19, col = "#C41230", cex = 1.15)
axis(1, at = seq(23, 31, 2), cex.axis = 0.8)
mtext("Republican seats out of 38", side = 1, line = 2.4, cex = 0.85)
text(22.7, yy, seats$label, xpd = NA, adj = 1, cex = 0.72)
text(seats$rep_new + 0.35, yy, paste0("+", seats$rep_gain), adj = 0,
     cex = 0.72, col = "#555555", xpd = NA)
legend("topleft", c("2021 lines", "2025 lines"), pch = c(21, 19),
       col = c("#4E5A63", "#C41230"), pt.bg = c("#FFFFFF", NA),
       bty = "n", cex = 0.78)

## ---- sb-d3
SB <- paste(sprintf('{"t":"%s","o":%d,"n":%d,"g":%d}',
                    seats$label, seats$rep_old, seats$rep_new, seats$rep_gain),
            collapse = ",")
cat(paste0('
<div id="sb" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[', SB, '];
const W=760,H=470,M={t:30,r:70,b:40,l:150};
const box=d3.select("#sb");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="Open circle: Republican seats under the 2021 lines. Filled red: the same "+
  "votes under the 2025 lines. <i>Hover a row for the counts.</i>";
cap.html(DEF);
const x=d3.scaleLinear().domain([23,31]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(R.map(r=>r.t)).range([M.t,H-M.b]).padding(0.4);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d3.format("d")));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#555").text("Republican seats out of 38");
const rg=svg.selectAll("g.r").data(R).join("g")
  .on("mouseenter",function(e,r){
    d3.select(this).selectAll("circle").attr("r",6.5);
    cap.html("<b>"+r.t+"</b>: "+r.o+" Republican seats under the 2021 lines, "+
      r.n+" under the 2025 lines \\u2014 a gain of "+r.g+".");})
  .on("mouseleave",function(){
    d3.select(this).selectAll("circle").attr("r",5);cap.html(DEF);});
rg.append("rect").attr("x",M.l-140).attr("width",W-M.r-M.l+200)
  .attr("y",r=>y(r.t)).attr("height",y.bandwidth()+6).attr("fill","transparent");
rg.append("text").attr("x",M.l-10).attr("y",r=>y(r.t)+y.bandwidth()/2+4)
  .attr("text-anchor","end").attr("font-size","11.5px").attr("fill","#4E5A63").text(r=>r.t);
rg.append("line").attr("x1",r=>x(r.o)).attr("x2",r=>x(r.n))
  .attr("y1",r=>y(r.t)+y.bandwidth()/2).attr("y2",r=>y(r.t)+y.bandwidth()/2)
  .attr("stroke","#999").attr("stroke-width",1.6);
rg.append("circle").attr("cx",r=>x(r.o)).attr("cy",r=>y(r.t)+y.bandwidth()/2)
  .attr("r",5).attr("fill","var(--paper,#fff)").attr("stroke","#4E5A63").attr("stroke-width",1.6);
rg.append("circle").attr("cx",r=>x(r.n)).attr("cy",r=>y(r.t)+y.bandwidth()/2)
  .attr("r",5).attr("fill","#C41230");
rg.append("text").attr("x",r=>x(r.n)+12).attr("y",r=>y(r.t)+y.bandwidth()/2+4)
  .attr("font-size","11px").attr("fill","#555").text(r=>"+"+r.g);
})();
</script>'))

## ---- xy-static
par(mar = c(4, 4, 1, 1), pty = "s")
so <- 100 * long$share_old; sn <- 100 * long$share_new
flip <- (long$share_old > 0.5) != (long$share_new > 0.5)
cl <- ifelse(!flip, "#B9BEC4",
             ifelse(long$share_new > 0.5, "#2C7FB8", "#C41230"))
plot(so, sn, xlim = c(15, 85), ylim = c(15, 85), pch = 19, cex = 0.45,
     col = adjustcolor(cl, 0.7), axes = FALSE,
     xlab = "Democratic share, 2021 lines (%)",
     ylab = "Democratic share, 2025 lines (%)")
axis(1, cex.axis = 0.8); axis(2, cex.axis = 0.8)
abline(0, 1, col = "#777777")
abline(h = 50, v = 50, col = "#777777", lty = 3)
text(70, 30, "flipped to Republican", cex = 0.7, col = "#C41230")
text(31, 70, "flipped to Democratic", cex = 0.7, col = "#2C7FB8")

## ---- xy-d3
XY <- paste(sprintf('{"d":%d,"t":"%s","o":%.4f,"n":%.4f}',
                    long$district, el_label(long$election),
                    long$share_old, long$share_new), collapse = ",")
cat(paste0('
<div id="xy" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const P=[', XY, '];
const W=560,H=560,M={t:14,r:14,b:46,l:52};
const box=d3.select("#xy");
const bar=box.append("div").attr("style","margin-bottom:6px;font-size:13px;display:flex;gap:8px;align-items:center");
bar.append("span").text("Highlight:");
const sel=bar.append("select").attr("style","font:inherit;padding:2px 4px");
sel.selectAll("option").data(["All districts",...d3.range(1,39).map(d=>"District "+d)])
  .join("option").attr("value",(d,i)=>i).text(d=>d);
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:560px;width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="One point per district per election. Below the diagonal, the 2025 lines "+
  "made the district more Republican. <i>Hover a point, or highlight one district above.</i>";
cap.html(DEF);
const x=d3.scaleLinear().domain([15,85]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([15,85]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(v=>v+"%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(7).tickFormat(v=>v+"%"));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#555").text("Democratic share, 2021 lines");
svg.append("text").attr("transform",`translate(14,${(M.t+H-M.b)/2}) rotate(-90)`)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#555")
  .text("Democratic share, 2025 lines");
svg.append("line").attr("x1",x(15)).attr("y1",y(15)).attr("x2",x(85)).attr("y2",y(85))
  .attr("stroke","#777");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",y(15)).attr("y2",y(85))
  .attr("stroke","#777").attr("stroke-dasharray","2 3");
svg.append("line").attr("y1",y(50)).attr("y2",y(50)).attr("x1",x(15)).attr("x2",x(85))
  .attr("stroke","#777").attr("stroke-dasharray","2 3");
svg.append("text").attr("x",x(70)).attr("y",y(28)).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230").text("flipped to Republican");
svg.append("text").attr("x",x(30)).attr("y",y(72)).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#2C7FB8").text("flipped to Democratic");
window.DD_SYNC=window.DD_SYNC||{fns:[],select(id){this.fns.forEach(fn=>fn(id));},
  subscribe(fn){this.fns.push(fn);}};
let hi=0;   // 0 = no highlight, else the highlighted district id
const pts=svg.append("g").selectAll("circle").data(P).join("circle")
  .attr("cx",p=>x(100*p.o)).attr("cy",p=>y(100*p.n))
  .attr("fill",p=>((p.o>0.5)!==(p.n>0.5))?(p.n>0.5?"#2C7FB8":"#C41230"):"#B9BEC4")
  .on("mouseenter",function(e,p){
    d3.select(this).attr("r",5).attr("fill-opacity",1);
    cap.html("<b>District "+p.d+"</b>, "+p.t+": "+(100*p.o).toFixed(1)+
      "% Democratic became "+(100*p.n).toFixed(1)+"%"+
      ((p.o>0.5)!==(p.n>0.5)?" \\u2014 <b>the winner changes</b>.":"."));})
  .on("mouseleave",function(){restyle();cap.html(DEF);});
function restyle(){
  pts.attr("r",p=>hi&&p.d===hi?5:2.6)
     .attr("fill-opacity",p=>hi?(p.d===hi?1:0.12):0.7)
     .attr("stroke",p=>hi&&p.d===hi?"#12181D":"none")
     .attr("stroke-width",1);
}
sel.on("change",function(){hi=+this.value;restyle();
  cap.html(hi?("<b>District "+hi+"</b> highlighted: its seventeen elections among all "+P.length+" points."):DEF);
  if(hi)window.DD_SYNC.select(hi);});
window.DD_SYNC.subscribe(id=>{if(id!==hi){hi=id;sel.property("value",id);restyle();}});
restyle();
})();
</script>'))

## ---- ov-prep
# FOCUS + CONTEXT. An even alluvial across 38 districts gives each one
# 1/38th of the height, which buries the only thing worth seeing: the
# handful of districts whose people actually moved. So the ribbons landing
# in the focused district are scaled up to take most of the height, and
# every other ribbon collapses to a sliver. Flow is still conserved -- each
# ribbon has one thickness at both ends, so the columns still balance --
# but the selected district's inflows are large enough to read and label.
SPOT_OV <- 28
NB <- 38
flows <- overlap[overlap$weight > 0.002, ]

ov_layout <- function(fl, spot, focus = 0.62, gapf = 0.004) {
  ns <- 1 - (NB - 1) * gapf                 # height left over for the nodes
  sf <- focus * ns                          # scale for ribbons into `spot`
  so <- (1 - focus) * ns / (NB - 1)         # scale for every other ribbon
  fl$t <- fl$weight * ifelse(fl$new == spot, sf, so)
  fo <- fl[order(fl$old, fl$new), ]
  fn <- fl[order(fl$new, fl$old), ]
  fo$off <- ave(fo$t, fo$old, FUN = function(x) cumsum(x) - x)
  fn$off <- ave(fn$t, fn$new, FUN = function(x) cumsum(x) - x)
  side <- function(v, g) {
    h <- tapply(v$t, g, sum)[as.character(1:NB)]
    h[is.na(h)] <- 0
    list(h = h, top = cumsum(c(0, head(h, -1) + gapf)))
  }
  L <- side(fl, fl$old); R <- side(fl, fl$new)
  k <- function(d) paste(d$old, d$new)
  fl$y1 <- L$top[fl$old] + fo$off[match(k(fl), k(fo))]
  fl$y2 <- R$top[fl$new] + fn$off[match(k(fl), k(fn))]
  stopifnot(!anyNA(fl$y1), !anyNA(fl$y2))
  list(fl = fl, hL = L$h, hR = R$h, topL = L$top, topR = R$top)
}

## ---- ov-static
OV <- ov_layout(flows, SPOT_OV)
NWD <- 0.05; X1 <- NWD; X2 <- 1 - NWD
ribbon <- function(y1a, y1b, y2a, y2b, col) {
  t <- seq(0, 1, length.out = 60); xm <- (X1 + X2) / 2
  bx <- (1-t)^3*X1 + 3*(1-t)^2*t*xm + 3*(1-t)*t^2*xm + t^3*X2
  by <- function(a, b) (1-t)^3*a + 3*(1-t)^2*t*a + 3*(1-t)*t^2*b + t^3*b
  polygon(c(bx, rev(bx)), c(by(y1a, y2a), rev(by(y1b, y2b))), col = col, border = NA)
}
par(mar = c(0.2, 0.2, 1.6, 0.2))
plot(NA, xlim = c(0, 1), ylim = c(1, 0), asp = NA, axes = FALSE, ann = FALSE)
ovf <- OV$fl
for (i in order(ovf$new == SPOT_OV)) {        # focused ribbons drawn last
  r <- ovf[i, ]
  ribbon(r$y1, r$y1 + r$t, r$y2, r$y2 + r$t,
         if (r$new == SPOT_OV) "#7FB07Fdd" else "#DDDDDDbb")
}
for (i in 1:NB) {
  rect(0, OV$topL[i], NWD, OV$topL[i] + OV$hL[i], col = "#EEEEEE", border = "#888888")
  text(NWD/2, OV$topL[i] + OV$hL[i]/2, i,
       cex = if (OV$hL[i] > 0.02) 0.62 else 0.46,
       col = if (OV$hL[i] > 0.02) "#333333" else "#777777")
  hl <- i == SPOT_OV
  rect(1 - NWD, OV$topR[i], 1, OV$topR[i] + OV$hR[i],
       col = if (hl) "#B8D4B8" else "#EEEEEE",
       border = if (hl) "#12181D" else "#888888", lwd = if (hl) 1.8 else 1)
  text(1 - NWD/2, OV$topR[i] + OV$hR[i]/2, i,
       cex = if (hl) 0.72 else if (OV$hR[i] > 0.02) 0.62 else 0.46,
       font = if (hl) 2 else 1,
       col = if (hl || OV$hR[i] > 0.02) "#333333" else "#777777")
}
sp <- ovf[ovf$new == SPOT_OV, ]
for (j in seq_len(nrow(sp))) {
  r <- sp[j, ]
  if (r$t < 0.018) next
  text(NWD + 0.014, r$y1 + r$t/2,
       sprintf("%.0f%% from district %d", 100 * r$weight, r$old),
       adj = 0, cex = 0.66, col = "#24421F", font = 2)
}
mtext("2021 districts", side = 3, at = 0, line = 0.1, cex = 0.72, adj = 0)
mtext("2025 districts", side = 3, at = 1, line = 0.1, cex = 0.72, adj = 1)

## ---- ov-d3
FLOWS <- paste(sprintf('{"o":%d,"n":%d,"w":%.5f}',
                       flows$old, flows$new, flows$weight), collapse = ",")
cat(paste0('
<div id="ov" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const F=[', FLOWS, '];
const NB=38,FOCUS=0.62,GAPF=0.004;
const W=760,NWD=44,TOPM=22,HGT=820;
const X1=NWD,X2=W-NWD,XM=(X1+X2)/2;
const box=d3.select("#ov");
window.DD_SYNC=window.DD_SYNC||{fns:[],select(id){this.fns.forEach(fn=>fn(id));},
  subscribe(fn){this.fns.push(fn);}};
const bar=box.append("div").attr("style","margin-bottom:6px;font-size:13px;display:flex;gap:8px;align-items:center");
bar.append("span").text("District:");
const sel=bar.append("select").attr("style","font:inherit;padding:2px 4px");
sel.selectAll("option").data(d3.range(1,NB+1)).join("option").attr("value",d=>d).text(d=>"District "+d);
sel.property("value","28");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${HGT+TOPM+14}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:3.3em;margin:0.3em 0 0 0");
const DEF="Every ribbon is a group of people: it leaves the 2021 district that held "+
  "them and lands in the 2025 district that holds them now. The selected district is "+
  "opened up to fill the frame; every other district is collapsed to a sliver so it "+
  "stays on the page without crowding out the one you are reading. "+
  "<i>Click any district or ribbon to open it up.</i>";
cap.html(DEF);
let hL=[],hR=[],topL=[],topR=[];
function layout(spot){
  const ns=1-(NB-1)*GAPF, sf=FOCUS*ns, so=(1-FOCUS)*ns/(NB-1);
  F.forEach(f=>{f.t=f.w*(f.n===spot?sf:so);});
  const accO={},accN={};
  F.slice().sort((a,b)=>a.o-b.o||a.n-b.n).forEach(f=>{
    accO[f.o]=accO[f.o]||0; f.eo=accO[f.o]; accO[f.o]+=f.t;});
  F.slice().sort((a,b)=>a.n-b.n||a.o-b.o).forEach(f=>{
    accN[f.n]=accN[f.n]||0; f.en=accN[f.n]; accN[f.n]+=f.t;});
  let cl=0,cr=0;
  for(let i=1;i<=NB;i++){
    hL[i]=accO[i]||0; hR[i]=accN[i]||0;
    topL[i]=cl; cl+=hL[i]+GAPF;
    topR[i]=cr; cr+=hR[i]+GAPF;
  }
  F.forEach(f=>{f.y1=(topL[f.o]+f.eo)*HGT+TOPM; f.y2=(topR[f.n]+f.en)*HGT+TOPM;});
}
const path=(f)=>{
  const y1=f.y1,y2=f.y2,t=f.t*HGT;
  return "M"+X1+","+y1+"C"+XM+","+y1+" "+XM+","+y2+" "+X2+","+y2+
         "L"+X2+","+(y2+t)+"C"+XM+","+(y2+t)+" "+XM+","+(y1+t)+" "+X1+","+(y1+t)+"Z";
};
svg.append("text").attr("x",0).attr("y",14).attr("font-size","11.5px")
  .attr("font-weight","700").attr("fill","#4E5A63").text("2021 districts");
svg.append("text").attr("x",W).attr("y",14).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("font-weight","700").attr("fill","#4E5A63")
  .text("2025 districts");
let selD=28;
layout(selD);
const rib=svg.append("g").selectAll("path").data(F).join("path")
  .attr("d",path).attr("stroke","none").attr("cursor","pointer")
  .on("mouseenter",function(e,f){
    d3.select(this).attr("fill","#4E7A4E").attr("fill-opacity",1).raise();
    cap.html("<b>"+(100*f.w).toFixed(1)+"%</b> of 2021 district "+f.o+
      "\\u2019s people are in 2025 district "+f.n+"."+
      (f.n!==selD?" <i>Click to open district "+f.n+".</i>":""));})
  .on("mouseleave",function(){shade();cap.html(DEF);})
  .on("click",(e,f)=>go(f.n));
const gL=svg.append("g"), gR=svg.append("g"), lab=svg.append("g");
const mkNode=(g,x)=>{
  g.selectAll("rect").data(d3.range(1,NB+1)).join("rect")
    .attr("x",x).attr("width",NWD).attr("cursor","pointer")
    .on("click",(e,d)=>go(d));
  // #2b2b2b / #707070 sit outside the stylesheet ink rule on purpose: the
  // node slabs are pinned light in both themes, so their labels have to
  // stay dark rather than being lifted to near-white on a dark page.
  g.selectAll("text").data(d3.range(1,NB+1)).join("text")
    .attr("x",x+NWD/2).attr("text-anchor","middle").attr("fill","#2b2b2b")
    .attr("pointer-events","none").text(d=>d);
};
mkNode(gL,0); mkNode(gR,W-NWD);
function shade(){
  rib.attr("fill",f=>f.n===selD?"#7FB07F":"#DDDDDD")
     .attr("fill-opacity",f=>f.n===selD?0.9:0.6);
  rib.filter(f=>f.n===selD).raise();
}
function place(t){
  // The animation is decoration, never the thing that makes the figure
  // correct: a hidden tab never fires requestAnimationFrame, so a d3
  // transition started there would leave the diagram frozen mid-story.
  // Snap instead whenever motion cannot or should not run.
  const anim=t&&!document.hidden&&
    !(window.matchMedia&&window.matchMedia("(prefers-reduced-motion: reduce)").matches);
  const R=(s)=>anim?s.transition().duration(650).ease(d3.easeCubicInOut):s;
  R(rib).attr("d",path);
  R(gL.selectAll("rect")).attr("y",d=>topL[d]*HGT+TOPM).attr("height",d=>hL[d]*HGT)
    .attr("fill","#EEEEEE").attr("stroke","#888888").attr("stroke-width",1);
  R(gL.selectAll("text")).attr("y",d=>(topL[d]+hL[d]/2)*HGT+TOPM+4)
    .attr("font-size",d=>hL[d]*HGT>16?"11px":"7.5px")
    .attr("fill",d=>hL[d]*HGT>16?"#2b2b2b":"#707070");
  R(gR.selectAll("rect")).attr("y",d=>topR[d]*HGT+TOPM).attr("height",d=>hR[d]*HGT)
    .attr("fill",d=>d===selD?"#B8D4B8":"#EEEEEE")
    .attr("stroke",d=>d===selD?"#12181D":"#888888")
    .attr("stroke-width",d=>d===selD?1.8:1);
  R(gR.selectAll("text")).attr("y",d=>(topR[d]+hR[d]/2)*HGT+TOPM+4)
    .attr("font-size",d=>d===selD?"13px":hR[d]*HGT>16?"11px":"7.5px")
    .attr("font-weight",d=>d===selD?"700":"400")
    .attr("fill",d=>d===selD||hR[d]*HGT>16?"#2b2b2b":"#707070");
  lab.selectAll("*").remove();
  const draw=()=>F.filter(f=>f.n===selD).forEach(f=>{
    if(f.t*HGT<11)return;
    lab.append("text").attr("x",X1+10).attr("y",f.y1+f.t*HGT/2+4)
      .attr("font-size","11.5px").attr("font-weight","700").attr("fill","#24421F")
      .attr("pointer-events","none")
      .text(Math.round(100*f.w)+"% from district "+f.o);
  });
  if(anim)setTimeout(draw,650); else draw();
}
function go(d){
  if(d===selD)return;
  selD=d; sel.property("value",d);
  layout(selD); shade(); place(true); window.DD_SYNC.select(d);
}
sel.on("change",function(){go(+this.value);});
window.DD_SYNC.subscribe(id=>{
  if(id===selD)return;
  selD=id; sel.property("value",id); layout(selD); shade(); place(true);});
shade(); place(false);
})();
</script>'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
