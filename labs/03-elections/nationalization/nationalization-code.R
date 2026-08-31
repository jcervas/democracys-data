# nationalization-code.R -- chunk bodies for nationalization-brief.Rmd
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

del <- read.csv("data/derived/delegations.csv",       stringsAsFactors = FALSE)
dst <- read.csv("data/derived/delegation_states.csv", stringsAsFactors = FALSE)
con <- read.csv("data/derived/senate_contests.csv",   stringsAsFactors = FALSE)
sy  <- read.csv("data/derived/senate_years.csv",      stringsAsFactors = FALSE)
hy  <- read.csv("data/derived/house_years.csv",       stringsAsFactors = FALSE)
swp <- read.csv("data/derived/swap.csv",              stringsAsFactors = FALSE)
fx  <- read.csv("data/derived/facts.csv",             stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
sg <- function(x) sprintf("%+.1f", as.numeric(x))

# The corpus map palette. The base-R fallbacks cannot swap for the dark page,
# so they use the light values.
RED <- "#C41230"; BLU <- "#2C7FB8"; GRN <- "#4d9221"; ORG <- "#e08214"
DRK <- "#1C4C5C"; MUTE <- "#76838C"; RULE <- "#CBD3D8"

DPEAK <- fn("del_peak_pct"); DLAST <- fn("del_last_pct")
HPEAK <- fn("h_peak_pct");   HLAST <- fn("h_last_pct")
YOLD  <- fn("swap_old_year"); YNEW <- fn("swap_new_year")

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
z <- del[del$year %in% c(1879, 1979, 2025), ]
data.frame(
  Congress = paste0(z$congress, "th"),
  Convened = z$year,
  States = z$states,
  Split_delegations = z$split_states,
  Share = paste0(p1(z$pct_split), "%"))

## ---- fig1-static
op <- par(mar = c(3.6, 4.4, 2.4, 1.6), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(del$year), ylim = c(0, 60), axes = FALSE,
     xlab = "", ylab = "")
abline(h = seq(0, 60, 10), col = RULE, lwd = 0.6)
lines(del$year, del$pct_split_two, col = ORG, lwd = 1.6)
lines(del$year, del$pct_split,     col = DRK, lwd = 2.6)
axis(1, at = seq(1880, 2020, 20), cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 60, 10), las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
mtext("% of states", 2, line = 2.9, cex = 0.86)
mtext("States whose two senators belonged to different parties",
      3, line = 0.9, cex = 0.82, adj = 0)
legend("topleft", c("Every state", "Only states that sent exactly two senators"),
       col = c(DRK, ORG), lwd = c(2.6, 1.6), bty = "n", cex = 0.72)
# Only the peak is labelled. The last Congress is named in the sentence under
# the figure, and a second label there lands on top of the falling line.
pk <- del[which.max(del$pct_split), ]
text(pk$year, pk$pct_split + 3.4, paste0(pk$year, ": ", pk$split_states, " of ", pk$states),
     cex = 0.7, col = MUTE)
par(op)

## ---- fig1-d3
ser <- paste0(
  '{"k":"Every state","c":"', DRK, '","w":2.6,"v":[',
  paste0("[", del$year, ",", p2(del$pct_split), "]", collapse = ","), ']},',
  '{"k":"Only states that sent exactly two senators","c":"', ORG, '","w":1.6,"v":[',
  paste0("[", del$year, ",", p2(del$pct_split_two), "]", collapse = ","), ']}')
cat(paste0('
<div id="dele" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', ser, '];
const W=770,H=430,M={t:18,r:24,b:46,l:52};
const svg=d3.select("#dele").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const yrs=S[0].v.map(p=>p[0]);
const x=d3.scaleLinear().domain(d3.extent(yrs)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,60]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(10));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#666")
  .text("% of states");
const ln=d3.line().x(p=>x(p[0])).y(p=>y(p[1]));
svg.append("g").selectAll("path").data(S).join("path")
  .attr("fill","none").attr("stroke",s=>s.c).attr("stroke-width",s=>s.w)
  .attr("d",s=>ln(s.v));
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#bbb").attr("opacity",0);
const dots=svg.append("g");
const tip=d3.select("#dele").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const px=d3.pointer(e,this)[0]+M.l;
    const yr=yrs.reduce((a,c)=>Math.abs(x(c)-px)<Math.abs(x(a)-px)?c:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const rows=S.map(s=>({k:s.k,c:s.c,val:s.v.find(q=>q[0]===yr)[1]}));
    dots.selectAll("circle").data(rows).join("circle")
      .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",4).attr("fill",r=>r.c);
    tip.style("opacity",1).html("<b>"+yr+"</b><br>"+
      rows.map(r=>"<span style=\\"color:"+r.c+"\\">\\u25a0</span> "+r.k+": "+r.val+"%").join("<br>"))
      .style("left",Math.min(x(yr)-M.l+18,W-330)+"px").style("top",(M.t+4)+"px");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);dots.selectAll("circle").remove();tip.style("opacity",0);});
const leg=d3.select("#dele").append("div").attr("style","margin-top:6px;font-size:12px");
leg.selectAll("span").data(S).join("span")
  .attr("style",s=>"display:inline-block;margin-right:14px;color:"+s.c+";font-weight:600")
  .html(s=>"\\u25a0 "+s.k);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the figure for a Congress-by-Congress readout.</p>'))

## ---- senate-table
o <- sy
o$pct_split <- paste0(p1(o$pct_split), "%")
o$wobble <- p2(o$wobble)
names(o) <- c("Election", "Senate contests", "Split outcomes", "Share",
              "Both parties ran", "Distance from the ticket")
o

## ---- runoff-table
r <- con[con$runoff, c("year", "abbrev", "term", "winner", "winner_party")]
r$term <- ifelse(r$term == "unexpired", "unexpired term", "full term")
names(r) <- c("Election", "State", "Seat", "Winner", "Party")
r

## ---- fig2-static
op <- par(mfrow = c(2, 1), mar = c(2.4, 4.4, 2.2, 1.6), mgp = c(2.6, 0.7, 0))
plot(hy$year, hy$pct_split, type = "o", pch = 19, cex = 0.7, lwd = 2.4,
     col = DRK, axes = FALSE, xlab = "", ylab = "", ylim = c(0, 45))
abline(h = seq(0, 40, 10), col = RULE, lwd = 0.6)
lines(hy$year, hy$pct_split, col = DRK, lwd = 2.4)
points(hy$year, hy$pct_split, pch = 19, cex = 0.7, col = DRK)
axis(1, at = seq(1952, 2024, 8), cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 40, 10), las = 1, cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
mtext("% of districts", 2, line = 2.9, cex = 0.8)
mtext("Districts that voted one way for president and the other for the House",
      3, line = 0.7, cex = 0.8, adj = 0)

par(mar = c(3.4, 4.4, 2.2, 1.6))
plot(NA, xlim = range(hy$year), ylim = c(0, 15), axes = FALSE, xlab = "", ylab = "")
abline(h = seq(0, 15, 5), col = RULE, lwd = 0.6)
lines(hy$year, hy$wobble, col = RED, lwd = 2.4)
lines(hy$year, hy$spread, col = BLU, lwd = 2.4)
axis(1, at = seq(1952, 2024, 8), cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 15, 5), las = 1, cex.axis = 0.74, lwd = 0, lwd.ticks = 1)
mtext("points of vote share", 2, line = 2.9, cex = 0.8)
mtext("The two ingredients of that count", 3, line = 0.7, cex = 0.8, adj = 0)
legend("topright", c("Distance from the ticket", "Spread of the districts"),
       col = c(RED, BLU), lwd = 2.4, bty = "n", cex = 0.72)
par(op)

## ---- fig2-d3
top <- paste0('{"k":"Split districts","c":"', DRK, '","v":[',
              paste0("[", hy$year, ",", p2(hy$pct_split), "]", collapse = ","), ']}')
bot <- paste0('{"k":"Distance from the ticket","c":"', RED, '","v":[',
              paste0("[", hy$year, ",", p2(hy$wobble), "]", collapse = ","), ']},',
              '{"k":"Spread of the districts","c":"', BLU, '","v":[',
              paste0("[", hy$year, ",", p2(hy$spread), "]", collapse = ","), ']}')
cat(paste0('
<div id="hs1" style="position:relative;margin:1em 0 0.2em 0"></div>
<div id="hs2" style="position:relative;margin:0 0 1em 0"></div>
<!-- d3 v7 is loaded once, by the first figure above -->
<script>
(function(){
const PANELS=[{id:"#hs1",S:[', top, '],ymax:45,unit:"%",lab:"% of districts",
               title:"Districts that voted one way for president and the other for the House"},
              {id:"#hs2",S:[', bot, '],ymax:15,unit:" pts",lab:"points of vote share",
               title:"The two ingredients of that count"}];
PANELS.forEach(function(P){
  const W=770,H=250,M={t:30,r:24,b:34,l:54};
  const svg=d3.select(P.id).append("svg").attr("viewBox","0 0 "+W+" "+H)
    .attr("style","max-width:100%;height:auto;font:12px inherit");
  const yrs=P.S[0].v.map(p=>p[0]);
  const x=d3.scaleLinear().domain(d3.extent(yrs)).range([M.l,W-M.r]);
  const y=d3.scaleLinear().domain([0,P.ymax]).range([H-M.b,M.t]);
  svg.append("text").attr("x",M.l).attr("y",14).attr("font-size","12px")
    .attr("fill","#666").text(P.title);
  svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
    .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(10));
  svg.append("g").attr("transform","translate("+M.l+",0)")
    .call(d3.axisLeft(y).ticks(4));
  svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
    .attr("y",14).attr("text-anchor","middle").attr("font-size","11px")
    .attr("fill","#666").text(P.lab);
  const ln=d3.line().x(p=>x(p[0])).y(p=>y(p[1]));
  svg.append("g").selectAll("path").data(P.S).join("path")
    .attr("fill","none").attr("stroke",s=>s.c).attr("stroke-width",2.4)
    .attr("d",s=>ln(s.v));
  const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
    .attr("stroke","#bbb").attr("opacity",0);
  const dots=svg.append("g");
  const tip=d3.select(P.id).append("div").attr("style",
   "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
  svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
    .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
    .on("mousemove",function(e){
      const px=d3.pointer(e,this)[0]+M.l;
      const yr=yrs.reduce((a,c)=>Math.abs(x(c)-px)<Math.abs(x(a)-px)?c:a);
      rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
      const rows=P.S.map(s=>({k:s.k,c:s.c,val:s.v.find(q=>q[0]===yr)[1]}));
      dots.selectAll("circle").data(rows).join("circle")
        .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",4).attr("fill",r=>r.c);
      tip.style("opacity",1).html("<b>"+yr+"</b><br>"+
        rows.map(r=>"<span style=\\"color:"+r.c+"\\">\\u25a0</span> "+r.k+": "+r.val+P.unit).join("<br>"))
        .style("left",Math.min(x(yr)-M.l+18,W-320)+"px").style("top",(M.t)+"px");
    })
    .on("mouseleave",()=>{rule.attr("opacity",0);dots.selectAll("circle").remove();tip.style("opacity",0);});
  const leg=d3.select(P.id).append("div").attr("style","margin-top:4px;font-size:12px");
  leg.selectAll("span").data(P.S).join("span")
    .attr("style",s=>"display:inline-block;margin-right:14px;color:"+s.c+";font-weight:600")
    .html(s=>"\\u25a0 "+s.k);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across either panel for an election-by-election readout.</p>'))

## ---- fig3-static
lab <- c(paste0(YOLD, " as it was"),
         paste0(YOLD, " districts,\n", YNEW, " candidates"),
         paste0(YNEW, " districts,\n", YOLD, " candidates"),
         paste0(YNEW, " as it was"))
v   <- c(swp$pct_split[swp$case == "actual_old"],
         swp$pct_split[swp$case == "old_with_new_gaps"],
         swp$pct_split[swp$case == "new_with_old_gaps"],
         swp$pct_split[swp$case == "actual_new"])
cl  <- c(DRK, ORG, ORG, DRK)
op <- par(mar = c(4.6, 4.4, 2.4, 1.6), mgp = c(2.6, 0.7, 0))
bp <- barplot(v, col = cl, border = NA, ylim = c(0, 46), axes = FALSE,
              names.arg = rep("", 4), space = 0.5)
abline(h = seq(0, 40, 10), col = RULE, lwd = 0.6)
barplot(v, col = cl, border = NA, ylim = c(0, 46), axes = FALSE,
        names.arg = rep("", 4), space = 0.5, add = TRUE)
axis(2, at = seq(0, 40, 10), las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
text(bp, v + 2.2, paste0(p1(v), "%"), cex = 0.8, col = MUTE)
mtext(lab, side = 1, at = bp, line = 1.6, cex = 0.72)
mtext("% of districts", 2, line = 2.9, cex = 0.86)
mtext("Split districts, actual and with the candidates swapped between years",
      3, line = 0.9, cex = 0.82, adj = 0)
par(op)

## ---- swap-table
o <- swp
o$case <- c(paste(YOLD, "as it was"), paste(YNEW, "as it was"),
            paste(YOLD, "districts, ", YNEW, "candidates"),
            paste(YNEW, "districts, ", YOLD, "candidates"))
o$pct_split <- paste0(p1(o$pct_split), "%")
names(o) <- c("Run", "Districts from", "Candidate distances from", "Split districts")
o

## ---- last-congress
spell <- function(s) paste(c(D = "Democrat", R = "Republican",
                             I = "independent")[strsplit(s, "")[[1]]], collapse = " and ")
o <- dst[, c("state", "senators", "sides", "split_caucus")]
o$sides <- vapply(o$sides, spell, "")
o$split_caucus <- ifelse(o$split_caucus, "yes", "no")
names(o) <- c("State", "Senators", "Party lines", "Still split read by side")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
