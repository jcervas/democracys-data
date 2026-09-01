# abortion-opinion-code.R -- chunk bodies for abortion-opinion-brief.Rmd
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

now <- read.csv("data/derived/now.csv",         stringsAsFactors = FALSE)
bp  <- read.csv("data/derived/by_party.csv",    stringsAsFactors = FALSE)
bi  <- read.csv("data/derived/by_ideology.csv", stringsAsFactors = FALSE)
ot  <- read.csv("data/derived/overtime.csv",    stringsAsFactors = FALSE)
gp  <- read.csv("data/derived/gaps.csv",        stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

LAST <- max(ot$year); FIRST <- min(ot$year)
PREV <- ot$year[which(ot$year == LAST) - 1]
o <- function(y, col) ot[[col]][ot$year == y]
g <- function(y, col) gp[[col]][gp$year == y]

NOWN <- sum(now$respondents)
ALW  <- now$pct[now$code == 4]
NEV  <- now$pct[now$code == 1]
NEED <- now$pct[now$code == 3]

pv <- function(grp) bp$pct_4[bp$group == grp]
iv <- function(grp) bi$pct_4[bi$group == grp]
DK_IDE <- iv("Haven't thought much about it")

PG1 <- g(FIRST, "party_gap"); PG2 <- g(LAST, "party_gap")
IG1 <- g(FIRST, "ideology_gap"); IG2 <- g(LAST, "ideology_gap")
MISS08 <- ot$pct_missing[ot$year == 2008]

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

DEM <- "#2B5C8A"; REP <- "#A33B2A"; GRY <- "#8A8F94"; GLD <- "#C08A2E"

## ---- nowtab
data.frame(The_option_as_read = now$view,
           Respondents = nn(now$respondents),
           Share = paste0(p1(now$pct), "%"))

## ---- raw
cat(paste(readLines("data/raw/item.txt"), collapse = "\n"))

## ---- partytab
data.frame(Party_identification = bp$group, Respondents = nn(bp$respondents),
           Never = paste0(p1(bp$pct_1), "%"),
           Rape_incest_life = paste0(p1(bp$pct_2), "%"),
           Need_established = paste0(p1(bp$pct_3), "%"),
           Always = paste0(p1(bp$pct_4), "%"))

## ---- fig1-static
op <- par(mar = c(6.6, 4.2, 1.2, 0.8), mgp = c(2.7, 0.7, 0))
v <- bp$pct_4
cl <- c(DEM, DEM, DEM, GRY, REP, REP, REP)
b <- barplot(v, col = cl, border = NA, ylim = c(0, 100), axes = FALSE,
             names.arg = rep("", nrow(bp)))
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% saying always permitted", 2, line = 2.8, cex = 0.9)
text(b, par("usr")[3] - 4, srt = 40, adj = 1, xpd = NA, cex = 0.72,
     labels = bp$group)
text(b, v + 4, paste0(p1(v), "%"), cex = 0.7, col = "#4E5A63")
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The static twin can only draw one option at a time, so it draws option 4 and
# the prose has to assert what the other three do. Here the reader switches
# option and watches the gradient invert, which is the chapter's argument.
#
# This chunk carries the ONE d3 <script src> for the document. Later figures
# use the library loaded here; a second copy would double the payload.
# ---------------------------------------------------------------------------
rows <- paste0('{g:"', bp$group, '",n:', bp$respondents,
               ',v:[', bp$pct_1, ',', bp$pct_2, ',', bp$pct_3, ',',
               bp$pct_4, ']}', collapse = ",")
cols <- paste0('"', c(DEM, DEM, DEM, GRY, REP, REP, REP), '"', collapse = ",")
cat(paste0('
<div id="abp" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '], C=[', cols, '];
const OPT=["never permitted","only rape, incest, or life",
           "only after the need is established","always, personal choice"];
const W=770,H=430,M={t:14,r:16,b:108,l:56};
const box=d3.select("#abp");
const bar=box.append("div").attr("style","margin:0 0 6px");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
let sel=3;
const x=d3.scaleBand().domain(D.map(d=>d.g)).range([M.l,W-M.r]).padding(0.24);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickSize(0));
gx.selectAll("text").attr("transform","rotate(-38)")
  .attr("text-anchor","end").attr("dx","-0.6em").attr("dy","0.4em")
  .style("font-size","11px");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("% choosing this option");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const rects=svg.selectAll("rect.b").data(D).join("rect").attr("class","b")
  .attr("x",d=>x(d.g)).attr("width",x.bandwidth())
  .attr("y",y(0)).attr("height",0).attr("fill",(d,i)=>C[i]);
const vals=svg.selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>x(d.g)+x.bandwidth()/2).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63");
const btns=bar.selectAll("button").data(OPT).join("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text((d,i)=>(i+1)+". "+d)
  .on("click",function(e,d){sel=OPT.indexOf(d);draw(true);});
rects.on("mousemove",function(e,d){
  const r=box.node().getBoundingClientRect();
  tip.style("opacity",1)
     .style("left",(e.clientX-r.left+14)+"px")
     .style("top",(e.clientY-r.top-8)+"px")
     .html("<b>"+d.g+"</b><br>"+d.n.toLocaleString()+" respondents<br>"+
       OPT.map((o,i)=>(i===sel?"<b>":"")+o+": "+d.v[i].toFixed(1)+"%"+
       (i===sel?"</b>":"")).join("<br>"));
}).on("mouseleave",function(){tip.style("opacity",0);});
// The first paint is NOT a transition. A d3 transition runs on
// requestAnimationFrame, which browsers freeze in a background tab, so a
// chart animated in from zero stays blank for a reader who opens the
// chapter in a tab they have not looked at yet. Animate only what the
// reader asks for.
function draw(anim){
  (anim?rects.transition().duration(340):rects)
    .attr("y",d=>y(d.v[sel])).attr("height",d=>y(0)-y(d.v[sel]));
  (anim?vals.transition().duration(340):vals)
    .attr("y",d=>y(d.v[sel])-5).text(d=>d.v[sel].toFixed(1)+"%");
  btns.style("background",(d,i)=>i===sel?"#1C4C5C":"#fff")
      .style("color",(d,i)=>i===sel?"#fff":"#12181D")
      .style("font-weight",(d,i)=>i===sel?"600":"400");
}
draw(false);
})();
</script>'))

## ---- ideotab
data.frame(Ideological_self_placement = bi$group,
           Respondents = nn(bi$respondents),
           Never = paste0(p1(bi$pct_1), "%"),
           Need_established = paste0(p1(bi$pct_3), "%"),
           Always = paste0(p1(bi$pct_4), "%"))

## ---- fig2-static
op <- par(mar = c(3.4, 4.2, 1.4, 8.6), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(ot$year), ylim = c(0, max(ot$always) + 6),
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(1980, LAST, 8), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% of respondents", 2, line = 2.9, cex = 0.9)
sers <- list(always = DEM, rape_incest_life = GRY,
             need_established = GLD, never = REP)
labs <- c(always = "always", rape_incest_life = "rape, incest,\nor life",
          need_established = "need\nestablished", never = "never")
for (s in names(sers)) {
  lines(ot$year, ot[[s]], col = sers[[s]], lwd = 2.4)
  pch <- ifelse(ot$year == 2008, 1, 19)
  points(ot$year, ot[[s]], col = sers[[s]], pch = pch, cex = 0.6)
  text(LAST, ot[[s]][ot$year == LAST], paste0(" ", labs[s]),
       col = sers[[s]], pos = 4, cex = 0.7, xpd = NA)
}
par(op)

## ---- fig2-d3
# Four series over 17 study years. The static twin can only label the last
# point of each line; here the reader scrubs across and reads all four at any
# year, which is what it takes to see option 3 rising as option 2 falls.
rows <- paste0('[', ot$year, ',', ot$never, ',', ot$rape_incest_life, ',',
               ot$need_established, ',', ot$always, ',',
               ifelse(ot$year == 2008, 1, 0), ']', collapse = ",")
cat(paste0('
<div id="abt" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[', rows, '];
const D=R.map(r=>({y:r[0],v:[r[1],r[2],r[3],r[4]],hollow:r[5]===1}));
const K=[{c:"', REP, '",lab:"never permitted"},
         {c:"', GRY, '",lab:"only rape, incest, or life"},
         {c:"', GLD, '",lab:"only after the need is established"},
         {c:"', DEM, '",lab:"always, personal choice"}];
const W=770,H=430,M={t:16,r:210,b:40,l:52};
const box=d3.select("#abt");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d3.max(d.v))+6]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(6));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("% of respondents");
const on=[true,true,true,true];
const ln=d3.line().x(d=>x(d.y)).y(d=>y(d.val));
const gs=K.map(function(k,i){
  const g=svg.append("g");
  g.append("path").attr("fill","none").attr("stroke",k.c).attr("stroke-width",2.4)
   .attr("d",ln(D.map(d=>({y:d.y,val:d.v[i]}))));
  g.selectAll("circle").data(D).join("circle")
   .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.v[i])).attr("r",3)
   .attr("fill",d=>d.hollow?"#fff":k.c).attr("stroke",k.c).attr("stroke-width",1.4);
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
    const px=d3.pointer(e,this)[0]+M.l;
    const yr=x.invert(px);
    const d=D.reduce((a,b)=>Math.abs(b.y-yr)<Math.abs(a.y-yr)?b:a);
    rule.attr("x1",x(d.y)).attr("x2",x(d.y)).attr("opacity",0.55);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+d.y+"</b>"+(d.hollow?" &middot; split half-sample":"")+"<br>"+
         K.map((k,i)=>"<span style=\\"color:"+k.c+"\\">&#9632;</span> "+
           k.lab+": "+d.v[i].toFixed(1)+"%").join("<br>"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
const leg=box.append("div").attr("style","margin-top:6px;font:11.5px inherit");
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

## ---- changetab
cc <- c("never", "rape_incest_life", "need_established", "always")
lb <- c("Never permitted", "Only rape, incest, or life",
        "Only after the need is established", "Always, personal choice")
data.frame(Option = lb,
           Then = paste0(p1(sapply(cc, function(z) o(PREV, z))), "%"),
           Now  = paste0(p1(sapply(cc, function(z) o(LAST, z))), "%"),
           Change = sprintf("%+.1f", sapply(cc, function(z) o(LAST, z) - o(PREV, z))))

## ---- fig3-static
op <- par(mar = c(3.4, 4.2, 1.4, 6.4), mgp = c(2.6, 0.7, 0))
k <- !is.na(gp$party_gap) | !is.na(gp$ideology_gap)
plot(NA, xlim = range(gp$year[k]), ylim = c(-5, max(gp$ideology_gap, na.rm = TRUE) + 6),
     axes = FALSE, xlab = "", ylab = "")
abline(h = 0, col = "#CBD3D8")
axis(1, at = seq(1980, LAST, 8), cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("percentage-point gap", 2, line = 2.9, cex = 0.9)
lines(gp$year, gp$ideology_gap, col = GLD, lwd = 2.6)
lines(gp$year, gp$party_gap,    col = DEM, lwd = 2.6)
points(gp$year, gp$ideology_gap, col = GLD, pch = 19, cex = 0.6)
points(gp$year, gp$party_gap,    col = DEM, pch = 19, cex = 0.6)
text(LAST, IG2, " ideology", col = GLD, pos = 4, cex = 0.74, xpd = NA)
text(LAST, PG2, " party",    col = DEM, pos = 4, cex = 0.74, xpd = NA)
par(op)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# A gap is a difference, and a difference hides which side moved. The static
# twin can only draw the two gap lines; hovering here also reports the four
# component shares, so the reader can see that the party gap widened mostly
# because Democrats moved, not because Republicans did.
#
# Drawn with the shared library (_lib/dd-charts.js). Figure 1 above is
# hand-written and already loaded d3, so dd_fig() is told d3 = FALSE and
# emits only the dd-charts tag; a second d3 copy would double the payload.
# ---------------------------------------------------------------------------
m <- gp[order(gp$year), ]
dd_fig("abg", "line", m, d3 = FALSE,
  size = list(w = 770, h = 400, m = list(t = 16, r = 96, b = 40, l = 56)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "ideology_gap", label = "percentage-point gap",
           domain = c(-5, max(m$ideology_gap, na.rm = TRUE) + 6),
           fmt = "d", ticks = 6),
  series = list(fields = list(
    list(field = "ideology_gap", label = "ideology", class = "series-3"),
    list(field = "party_gap",    label = "party",    class = "series-1"))),
  points = TRUE, endLabels = TRUE,
  annotations = list(list(type = "hline", y = 0, class = "zero",
                          dash = FALSE)),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> party gap "+
        d.party_gap.toFixed(1)+"<br>"+
      "&nbsp;&nbsp;Dem "+d.dem_always.toFixed(1)+"% &middot; Rep "+
        d.rep_always.toFixed(1)+"%<br>"+
      "<span class=\'series-3-txt\'>&#9632;</span> ideology gap "+
        d.ideology_gap.toFixed(1)+"<br>"+
      "&nbsp;&nbsp;Lib "+d.lib_always.toFixed(1)+"% &middot; Con "+
        d.con_always.toFixed(1)+"%";
  }'))
