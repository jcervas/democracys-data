# mid-decade-code.R -- chunk bodies for mid-decade-brief.Rmd
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
# the 2024 presidential race alone, walked through in the prose
s24 <- seats[seats$election == "pres-2024", ]

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

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("district_shares.csv", "district_summary.csv", "facts.csv", "seats_by_election.csv", "tx_overlap_pop.csv"))
# share of one 2021 district's people that the named 2025 district holds
ovw <- function(o, n) {
  w <- overlap$weight[overlap$old == o & overlap$new == n]
  if (length(w)) 100 * w else 0
}

# ---- Florida, the same test run a year later --------------------------------
# The Florida re-count was built as its own chapter and folded into this one on
# 31 Aug 2026. Its derived tables are read where they were written, so the two
# states' headline numbers come from the same arithmetic run twice rather than
# from anything retyped here. The candidates cover the folded chapter both in
# place and archived; the first that exists wins.
FL_DIR <- Filter(dir.exists, c(
  "../mid-decade-florida/data/derived",
  "../../_archive/06-putting-data-together/mid-decade-florida/data/derived",
  "../../_archive/mid-decade-florida/data/derived"))[1]
if (is.na(FL_DIR))
  stop("mid-decade: the Florida derived tables are not where this chapter ",
       "expects them; see the Sources note under 'The same test, run in Florida'")
fl_facts <- read.csv(file.path(FL_DIR, "facts.csv"), stringsAsFactors = FALSE)
fl_seats <- read.csv(file.path(FL_DIR, "seats_by_election.csv"),
                     stringsAsFactors = FALSE)
flx <- function(k) fl_facts$value[fl_facts$key == k]

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
# The shared chart library draws this one: a dumbbell is exactly the form the
# question takes, one row per election with two counts on it. mm-d3 above has
# already put d3 on the page, so only dd-charts.js is added here.
sbd <- seats[, c("label", "rep_old", "rep_new", "rep_gain")]
dd_fig("sb", "dumbbell", sbd, d3 = FALSE,
  size = list(w = 760),
  rowHeight = 24,
  y = list(field = "label"),
  a = list(field = "rep_old", label = "2021 lines"),
  b = list(field = "rep_new", label = "2025 lines"),
  aClass = "series-3", bClass = "gop",
  x = list(domain = c(22, 32), ticks = 6, fmt = "d",
           label = "Republican seats out of 38"),
  tip = dd_tip(c(rep_old = "2021 lines", rep_new = "2025 lines",
                 rep_gain = "Republican gain"),
               fmt = c(rep_old = "d", rep_new = "d", rep_gain = "signed0"),
               title = "label"))

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
# One point per district per election, drawn by the shared library. A point
# that did not move sits on the diagonal; the two shaded quadrants hold the
# districts where the winning party changes.
xyd <- data.frame(
  o   = 100 * long$share_old,
  nw  = 100 * long$share_new,
  lab = paste0("District ", long$district, ", ", el_label(long$election)),
  cls = ifelse((long$share_old > 0.5) != (long$share_new > 0.5),
               ifelse(long$share_new > 0.5, "dem", "gop"), "series-4"),
  stringsAsFactors = FALSE)
dd_fig("xy", "scatter", xyd, d3 = FALSE,
  size = list(w = 620, h = 620),
  r = 2.8, opacity = 0.7,
  x = list(field = "o",  domain = c(15, 85), fmt = "d",
           label = "Democratic share, 2021 lines (%)"),
  y = list(field = "nw", domain = c(15, 85), fmt = "d",
           label = "Democratic share, 2025 lines (%)"),
  annotations = list(
    list(type = "rule",  x1 = 15, y1 = 15, x2 = 85, y2 = 85),
    list(type = "vline", x = 50),
    list(type = "hline", y = 50),
    list(type = "text", x = 70, y = 27, anchor = "middle", `class` = "gop-txt",
         text = "flipped to Republican"),
    list(type = "text", x = 30, y = 73, anchor = "middle", `class` = "dem-txt",
         text = "flipped to Democratic")),
  tip = dd_tip(c(o = "2021 lines", nw = "2025 lines"),
               fmt = c(o = "pct1", nw = "pct1"), title = "lab"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
