# perception-gap-code.R -- chunk bodies for perception-gap-brief.Rmd
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

it  <- read.csv("data/derived/items.csv",    stringsAsFactors = FALSE)
hp  <- read.csv("data/derived/heaping.csv",  stringsAsFactors = FALSE)
byp <- read.csv("data/derived/by_party.csv", stringsAsFactors = FALSE)
di  <- read.csv("data/derived/dist.csv",     stringsAsFactors = FALSE)
cov <- read.csv("data/derived/coverage.csv", stringsAsFactors = FALSE)
fx  <- read.csv("data/derived/facts.csv",    stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)

RED <- "#C41230"; BLU <- "#2C7FB8"; ORG <- "#e08214"
DRK <- "#1C4C5C"; MUTE <- "#76838C"; RULE <- "#CBD3D8"

# The archive's group strings are terse and two are abbreviations a reader has
# no reason to know.
LAB <- c("Black" = "Black", "Union" = "Union member",
         "Atheist/Agnostic" = "Atheist or agnostic", "LGB" = "Lesbian, gay or bisexual",
         "Evangelical" = "Evangelical", "$250K+ Income" = "Earning $250K+",
         "Age 65+" = "Aged 65 or over", "Southern" = "Southern")
it <- it[order(it$party, -it$gap_mean), ]
it$label  <- unname(LAB[it$group])
byp$label <- unname(LAB[byp$group])

POOL <- hp[hp$item == "ALL", ]
HB   <- di$count[di$item == "ALL"]

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- items-table
o <- it[, c("party", "label", "actual_pct", "perceived_pct", "gap_mean")]
o$actual_pct <- paste0(p1(o$actual_pct), "%")
o$perceived_pct <- paste0(p1(o$perceived_pct), "%")
o$gap_mean <- sprintf("%+.1f", o$gap_mean)
names(o) <- c("Party", "Out of every 100 supporters, how many are…",
              "Really", "People said", "Gap")
o

## ---- fig1-static
op <- par(mar = c(3.8, 12.5, 2.6, 1.6), mgp = c(2.4, 0.6, 0))
k <- nrow(it); y <- rev(seq_len(k))
# the panel runs past the last gridline so the key has empty space to sit in
plot(NA, xlim = c(0, 58), ylim = c(0.4, k + 0.6), axes = FALSE, xlab = "", ylab = "")
abline(v = seq(0, 50, 10), col = RULE, lwd = 0.6)
segments(it$actual_pct, y, it$perceived_pct, y, col = RULE, lwd = 3)
segments(it$actual_pct - 1.96 * it$actual_se, y,
         it$actual_pct + 1.96 * it$actual_se, y, col = DRK, lwd = 1.4)
points(it$actual_pct, y, pch = 19, cex = 0.95, col = DRK)
points(it$perceived_pct, y, pch = 19, cex = 0.95, col = ORG)
axis(1, at = seq(0, 50, 10), labels = paste0(seq(0, 50, 10), "%"),
     cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, at = y, labels = it$label, las = 1, cex.axis = 0.76, lwd = 0, tick = FALSE)
first <- !duplicated(it$party)
mtext(ifelse(it$party == "Republican", "Republicans", "Democrats")[first],
      2, at = y[first], line = 11.4, las = 1, cex = 0.66, col = MUTE, adj = 0)
legend(44, k + 0.5, c("really", "people said"), pch = 19,
       col = c(DRK, ORG), bty = "n", cex = 0.74, y.intersp = 1.1)
mtext("Share of each party's supporters, true and estimated",
      3, line = 1.1, cex = 0.84, adj = 0)
par(op)

## ---- fig1-d3
rows <- paste0('{"g":"', it$label, '","p":"', it$party,
               '","a":', p2(it$actual_pct), ',"se":', p2(it$actual_se),
               ',"q":', p2(it$perceived_pct), '}', collapse = ",")
cat(paste0('
<div id="pgap" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', rows, '];
const W=770,H=380,M={t:34,r:24,b:38,l:190};
const svg=d3.select("#pgap").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,50]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(S.map(d=>d.g)).range([M.t,H-M.b]).padding(0.6);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y));
svg.append("text").attr("x",M.l).attr("y",16).attr("font-size","12px")
  .attr("fill","#666").text("Share of each party\\u2019s supporters, true and estimated");
const g=svg.append("g");
g.selectAll("line.join").data(S).join("line").attr("class","join")
  .attr("x1",d=>x(d.a)).attr("x2",d=>x(d.q)).attr("y1",d=>y(d.g)).attr("y2",d=>y(d.g))
  .attr("stroke","#CBD3D8").attr("stroke-width",3);
g.selectAll("line.ci").data(S).join("line").attr("class","ci")
  .attr("x1",d=>x(d.a-1.96*d.se)).attr("x2",d=>x(d.a+1.96*d.se))
  .attr("y1",d=>y(d.g)).attr("y2",d=>y(d.g))
  .attr("stroke","', DRK, '").attr("stroke-width",1.4);
g.selectAll("circle.a").data(S).join("circle").attr("class","a")
  .attr("cx",d=>x(d.a)).attr("cy",d=>y(d.g)).attr("r",5).attr("fill","', DRK, '");
g.selectAll("circle.q").data(S).join("circle").attr("class","q")
  .attr("cx",d=>x(d.q)).attr("cy",d=>y(d.g)).attr("r",5).attr("fill","', ORG, '");
const tip=d3.select("#pgap").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
g.selectAll("rect.hit").data(S).join("rect").attr("class","hit")
  .attr("x",M.l).attr("y",d=>y(d.g)-12).attr("width",W-M.r-M.l).attr("height",24)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.g+"</b> ("+d.p+")<br>Really: "+d.a.toFixed(1)+
      "% \\u00b1"+(1.96*d.se).toFixed(1)+"<br>People said: "+d.q.toFixed(1)+
      "%<br>Gap: "+(d.q-d.a).toFixed(1)+" points")
      .style("left",Math.min(x(d.q)-M.l+200,W-260)+"px").style("top",(y(d.g)-46)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
const leg=d3.select("#pgap").append("div").attr("style","margin-top:4px;font-size:12px");
leg.html("<span style=\\"color:', DRK, ';font-weight:600\\">\\u25cf really</span>"+
 "<span style=\\"margin-left:14px;color:', ORG, ';font-weight:600\\">\\u25cf people said</span>");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a row for the pair and the gap. The short bar through the dark dot is the
interval on the true share, which is itself a survey estimate.</p>'))

## ---- party-prep
# Two dots per item: what Democratic respondents said, and what Republican
# respondents said. The row order is Figure 1's, so the eye can carry over.
bd <- byp[byp$respondent == "Democrats", ]
br <- byp[byp$respondent == "Republicans", ]
bd <- bd[match(it$group, bd$group), ]
br <- br[match(it$group, br$group), ]

## ---- fig2-static
op <- par(mar = c(3.8, 12.5, 2.6, 1.6), mgp = c(2.4, 0.6, 0))
k <- nrow(it); y <- rev(seq_len(k))
plot(NA, xlim = c(0, 66), ylim = c(0.4, k + 0.6), axes = FALSE, xlab = "", ylab = "")
abline(v = seq(0, 50, 10), col = RULE, lwd = 0.6)
segments(it$actual_pct, y, pmin(bd$guess, br$guess), y, col = RULE, lwd = 1)
segments(pmin(bd$guess, br$guess), y, pmax(bd$guess, br$guess), y,
         col = RULE, lwd = 3)
points(it$actual_pct, y, pch = 124, cex = 1.1, col = MUTE)
points(bd$guess, y, pch = 19, cex = 0.95, col = BLU)
points(br$guess, y, pch = 19, cex = 0.95, col = RED)
axis(1, at = seq(0, 50, 10), labels = paste0(seq(0, 50, 10), "%"),
     cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, at = y, labels = it$label, las = 1, cex.axis = 0.76, lwd = 0, tick = FALSE)
first <- !duplicated(it$party)
mtext(paste("about", ifelse(it$party == "Republican", "Republicans", "Democrats"))[first],
      2, at = y[first], line = 11.4, las = 1, cex = 0.66, col = MUTE, adj = 0)
legend(45, k + 0.6, c("Democrats guessed", "Republicans guessed", "really"),
       pch = c(19, 19, 124), col = c(BLU, RED, MUTE), bty = "n", cex = 0.7,
       y.intersp = 1.1)
mtext("Who was answering, and what they said",
      3, line = 1.1, cex = 0.84, adj = 0)
par(op)

## ---- fig2-d3
rows2 <- paste0('{"g":"', it$label, '","p":"', it$party,
                '","a":', p2(it$actual_pct),
                ',"d":', p2(bd$guess), ',"r":', p2(br$guess), '}', collapse = ",")
cat(paste0('
<div id="pby" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first figure above -->
<script>
(function(){
const S=[', rows2, '];
const W=770,H=380,M={t:34,r:24,b:38,l:190};
const svg=d3.select("#pby").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,50]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(S.map(d=>d.g)).range([M.t,H-M.b]).padding(0.6);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y));
svg.append("text").attr("x",M.l).attr("y",16).attr("font-size","12px")
  .attr("fill","#666").text("Who was answering, and what they said");
const g=svg.append("g");
g.selectAll("line.lead").data(S).join("line").attr("class","lead")
  .attr("x1",d=>x(d.a)).attr("x2",d=>x(Math.min(d.d,d.r)))
  .attr("y1",d=>y(d.g)).attr("y2",d=>y(d.g))
  .attr("stroke","#CBD3D8").attr("stroke-width",1);
g.selectAll("line.sp").data(S).join("line").attr("class","sp")
  .attr("x1",d=>x(Math.min(d.d,d.r))).attr("x2",d=>x(Math.max(d.d,d.r)))
  .attr("y1",d=>y(d.g)).attr("y2",d=>y(d.g))
  .attr("stroke","#CBD3D8").attr("stroke-width",3);
g.selectAll("line.tr").data(S).join("line").attr("class","tr")
  .attr("x1",d=>x(d.a)).attr("x2",d=>x(d.a))
  .attr("y1",d=>y(d.g)-7).attr("y2",d=>y(d.g)+7)
  .attr("stroke","', MUTE, '").attr("stroke-width",2);
g.selectAll("circle.d").data(S).join("circle").attr("class","d")
  .attr("cx",d=>x(d.d)).attr("cy",d=>y(d.g)).attr("r",5).attr("fill","', BLU, '");
g.selectAll("circle.r").data(S).join("circle").attr("class","r")
  .attr("cx",d=>x(d.r)).attr("cy",d=>y(d.g)).attr("r",5).attr("fill","', RED, '");
const tip=d3.select("#pby").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
g.selectAll("rect.hit").data(S).join("rect").attr("class","hit")
  .attr("x",M.l).attr("y",d=>y(d.g)-12).attr("width",W-M.r-M.l).attr("height",24)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.g+"</b><br>a group of "+d.p+"s"+
      "<br>Really: "+d.a.toFixed(1)+"%"+
      "<br>Democrats said: "+d.d.toFixed(1)+"%"+
      "<br>Republicans said: "+d.r.toFixed(1)+"%")
      .style("left",Math.min(x(Math.max(d.d,d.r))-M.l+200,W-270)+"px")
      .style("top",(y(d.g)-56)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
const leg=d3.select("#pby").append("div").attr("style","margin-top:4px;font-size:12px");
leg.html("<span style=\\"color:', BLU, ';font-weight:600\\">\\u25cf Democrats guessed</span>"+
 "<span style=\\"margin-left:14px;color:', RED, ';font-weight:600\\">\\u25cf Republicans guessed</span>"+
 "<span style=\\"margin-left:14px;color:', MUTE, ';font-weight:600\\">| really</span>");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a row for all three numbers. The top four rows are groups of Democrats and
the bottom four are groups of Republicans, so the far dot changes colour
halfway down.</p>'))

## ---- party-table
o <- data.frame(
  who = c("Democrats", "Republicans", "Democrats", "Republicans"),
  ab  = c("their own party", "their own party",
          "the other party", "the other party"),
  gap = sprintf("%+.1f", c(
    mean(byp$gap[byp$respondent == "Democrats"   & byp$relation == "own party"]),
    mean(byp$gap[byp$respondent == "Republicans" & byp$relation == "own party"]),
    mean(byp$gap[byp$respondent == "Democrats"   & byp$relation == "other party"]),
    mean(byp$gap[byp$respondent == "Republicans" & byp$relation == "other party"]))))
names(o) <- c("Who was answering", "Asked about", "Average overestimate (pts)")
o

## ---- fig3-static
op <- par(mar = c(3.8, 4.4, 2.6, 1.6), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(-1, 101), ylim = c(0, max(HB) * 1.08), axes = FALSE,
     xlab = "", ylab = "")
abline(h = seq(0, max(HB), 200), col = RULE, lwd = 0.6)
cols <- ifelse(0:100 %% 10 == 0, RED, ifelse(0:100 %% 5 == 0, ORG, BLU))
segments(0:100, 0, 0:100, HB, col = cols, lwd = 2.4, lend = 1)
axis(1, at = seq(0, 100, 10), cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
mtext("answers", 2, line = 3.0, cex = 0.86)
mtext("the answer given, out of 100", 1, line = 2.2, cex = 0.82)
mtext(paste0("All ", n(fn("n_answers")), " answers, at one-point bins"),
      3, line = 1.1, cex = 0.84, adj = 0)
legend("topright", c("ends in 0", "ends in 5", "anything else"), fill = c(RED, ORG, BLU),
       border = NA, bty = "n", cex = 0.72)
text(50, HB[51] + max(HB) * 0.05, "50", cex = 0.72, col = MUTE)
par(op)

## ---- fig3-d3
# One series per item plus the pooled one, so the reader can put any single
# item's thousand answers beside the true share for that item.
ser <- paste0('{"k":"', c(it$label, "all eight pooled"), '","a":',
              c(p2(it$actual_pct), "null"), ',"v":[',
              vapply(c(it$item, "ALL"), function(v)
                paste(di$count[di$item == v], collapse = ","), ""), ']}',
              collapse = ",")
cat(paste0('
<div id="dexp" style="position:relative;margin:1em 0"></div>
<div id="dbtn" style="margin:0 0 0.6em 0;font-size:12px"></div>
<script>
(function(){
const S=[', ser, '];
const W=770,H=360,M={t:34,r:20,b:44,l:56};
const svg=d3.select("#dexp").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-1,101]).range([M.l,W-M.r]);
const y=d3.scaleLinear().range([H-M.b,M.t]);
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")");
const gy=svg.append("g").attr("transform","translate("+M.l+",0)");
const title=svg.append("text").attr("x",M.l).attr("y",16)
  .attr("font-size","12px").attr("fill","#666");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#666").text("answers");
const bars=svg.append("g");
const truth=svg.append("g");
const col=i=>(i%10===0)?"', RED, '":((i%5===0)?"', ORG, '":"', BLU, '");
let cur=S.length-1;
function style(i){ return "display:inline-block;margin:0 10px 4px 0;cursor:pointer;"+
  "border-bottom:2px solid "+(i===cur?"', RED, '":"transparent")+";"+
  (i===cur?"font-weight:600":"color:#666"); }
function draw(){
  const s=S[cur];
  y.domain([0,d3.max(s.v)*1.1]);
  gx.call(d3.axisBottom(x).ticks(11).tickFormat(d3.format("d")));
  gy.call(d3.axisLeft(y).ticks(5));
  title.text(s.a===null
    ? "All ', n(fn("n_answers")), ' answers, at one-point bins"
    : "\\u201c\\u2026how many are "+s.k+"?\\u201d \\u2014 ', n(fn("n_resp")), ' answers");
  bars.selectAll("line").data(s.v).join("line")
    .attr("x1",(d,i)=>x(i)).attr("x2",(d,i)=>x(i))
    .attr("y1",y(0)).attr("y2",d=>y(d))
    .attr("stroke",(d,i)=>col(i)).attr("stroke-width",4);
  const t=(s.a===null)?[]:[s.a];
  truth.selectAll("line").data(t).join("line")
    .attr("x1",d=>x(d)).attr("x2",d=>x(d)).attr("y1",M.t).attr("y2",y(0))
    .attr("stroke","#111").attr("stroke-width",2).attr("stroke-dasharray","4 3");
  truth.selectAll("text").data(t).join("text")
    .attr("x",d=>x(d)+6).attr("y",M.t+11).attr("font-size","11px").attr("fill","#111")
    .text(d=>"really "+d.toFixed(1)+"%");
}
const tip=d3.select("#dexp").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const i=Math.max(0,Math.min(100,Math.round(x.invert(d3.pointer(e,this)[0]+M.l))));
    tip.style("opacity",1).html("<b>"+i+"</b> was given "+S[cur].v[i]+" times")
      .style("left",Math.min(x(i)-M.l+18,W-230)+"px").style("top",(M.t+4)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
const btn=d3.select("#dbtn");
btn.selectAll("span").data(S).join("span")
  .attr("style",(d,i)=>style(i)).text(d=>d.k)
  .on("click",function(e,d){ cur=S.indexOf(d);
    btn.selectAll("span").attr("style",(dd,i)=>style(i)); draw(); });
draw();
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Click an item to see the thousand answers to that question on their own, with
the true share marked. Hover for the count at any value.</p>'))

## ---- heap-table
o <- hp[, c("item", "group", "mult10", "mult5", "eq50")]
o$name <- ifelse(o$item == "ALL", "all eight pooled", unname(LAB[o$group]))
o <- o[, c("name", "mult10", "mult5", "eq50")]
o$mult10 <- paste0(p1(o$mult10), "%")
o$mult5  <- paste0(p1(o$mult5), "%")
o$eq50   <- paste0(p1(o$eq50), "%")
names(o) <- c("Item", "Ends in a zero", "Ends in a zero or a five", "Exactly 50")
o

## ---- fig4-static
o <- it[order(-it$gap_mean), ]
op <- par(mar = c(3.8, 12.5, 2.6, 3.2), mgp = c(2.4, 0.6, 0))
k <- nrow(o); y <- rev(seq_len(k))
plot(NA, xlim = c(0, 40), ylim = c(0.4, k + 0.6), axes = FALSE, xlab = "", ylab = "")
abline(v = seq(0, 40, 10), col = RULE, lwd = 0.6)
arrows(o$gap_mean, y, o$gap_median, y, length = 0.06, col = MUTE, lwd = 1.6)
points(o$gap_mean, y, pch = 19, cex = 0.95, col = RED)
points(o$gap_median, y, pch = 19, cex = 0.95, col = BLU)
axis(1, at = seq(0, 40, 10), cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, at = y, labels = o$label, las = 1, cex.axis = 0.76, lwd = 0, tick = FALSE)
mtext("the gap, in points", 1, line = 2.2, cex = 0.82)
mtext("The same gap, measured off the mean and off the middle",
      3, line = 1.1, cex = 0.84, adj = 0)
legend("topleft", c("gap from the mean", "gap from the median"), pch = 19,
       col = c(RED, BLU), bty = "n", cex = 0.74)
par(op)

## ---- gap-table
o <- it[order(-it$gap_mean), c("label", "actual_pct", "perceived_pct", "median",
                               "gap_mean", "gap_median")]
o$actual_pct <- p1(o$actual_pct)
o$perceived_pct <- p1(o$perceived_pct)
o$gap_mean <- sprintf("%+.1f", o$gap_mean)
o$gap_median <- sprintf("%+.1f", o$gap_median)
names(o) <- c("Group", "Really", "Mean guess", "Middle guess",
              "Gap from the mean", "Gap from the middle")
o

## ---- coverage-table
o <- cov
o$where <- ifelse(o$where == "codebook only",
                  "described, and not in the file", "in the file, and not described")
names(o) <- c("Column", "Which side it falls on")
o

## ---- form-table
o <- it[order(it$party, it$field), c("field", "question")]
names(o) <- c("Field", "Question")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
