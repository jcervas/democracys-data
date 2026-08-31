# election-night-code.R -- chunk bodies for election-night-brief.Rmd
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
sen <- read.csv("data/derived/senate_2026_landscape.csv", stringsAsFactors = FALSE)
reh <- read.csv("data/derived/rehearsal_house_2024.csv", stringsAsFactors = FALSE)

sen$baseline <- ifelse(sen$pres24_winner == "Trump", "Republican", "Democrat")
sen$flips    <- sen$baseline != sen$party

reh$dev  <- abs(reh$dem_share - 50)
miss     <- reh[!reh$as_expected, ]
acc      <- function(x) 100 * mean(x)

band_tab <- do.call(rbind, lapply(c(2, 5, 10, 15), function(b) {
  i <- reh$dev <= b
  data.frame(band = paste0("within ", b, " points of even"),
             districts = sum(i), correct = sum(reh$as_expected[i]),
             pct = acc(reh$as_expected[i]))
}))

safe <- sen[sen$abs_margin >= 15, ]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# ---- a square-tile layout of the fifty states, for the map of the class ----
# Column and row only; nothing here is a finding, it is where a box is drawn.
tile <- do.call(rbind, lapply(strsplit(strsplit(paste(
  "AK:1,1 ME:11,1 VT:10,2 NH:11,2 WA:1,3 ID:2,3 MT:3,3 ND:4,3 MN:5,3 WI:6,3",
  "MI:7,3 NY:9,3 MA:10,3 RI:11,3 OR:1,4 NV:2,4 WY:3,4 SD:4,4 IA:5,4 IL:6,4",
  "IN:7,4 OH:8,4 PA:9,4 NJ:10,4 CT:11,4 CA:1,5 UT:2,5 CO:3,5 NE:4,5 MO:5,5",
  "KY:6,5 WV:7,5 VA:8,5 MD:9,5 DE:10,5 AZ:2,6 NM:3,6 KS:4,6 AR:5,6 TN:6,6",
  "NC:7,6 SC:8,6 OK:4,7 LA:5,7 MS:6,7 AL:7,7 GA:8,7 HI:1,8 TX:4,8 FL:9,8"),
  " ")[[1]], "[:,]"), function(z)
  data.frame(state = z[1], col = as.integer(z[2]), row = as.integer(z[3]),
             stringsAsFactors = FALSE)))
tile        <- merge(tile, sen, by = "state", all.x = TRUE)
tile$up     <- !is.na(tile$party)
tile$letter <- ifelse(is.na(tile$party), "", substr(tile$party, 1, 1))
tile$lab    <- ifelse(is.na(tile$pres24_margin), "",
                      paste0(ifelse(tile$pres24_margin > 0, "+", ""),
                             pc(tile$pres24_margin)))

# ---- accuracy of the rule as the competitiveness window widens ----
bs   <- seq(0.5, 25, by = 0.5)
accin  <- sapply(bs, function(b) 100 * mean(reh$as_expected[reh$dev <= b]))
accout <- sapply(bs, function(b) 100 * mean(reh$as_expected[reh$dev >  b]))
b100 <- min(bs[accout >= 100])
miss$dir <- ifelse(miss$pres_party == "republican",
                   "Trump district, Democratic member",
                   "Harris district, Republican member")

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)                       # fails_when -> fails when
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)   # sentence case the first letter
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- clean-en
o <- sen[sen$state == "GA", c("state", "senator_last", "party",
                              "pres24_margin", "hostile_turf")]
names(o) <- c("state", "incumbent", "party", "2024 margin", "hostile turf?")
o

## ---- shape-en
data.frame(
  stage = c("Members of Congress in the roster", "Senators",
            "Senators whose class is up in 2026"),
  rows = c("537", "100", nrow(sen)))

## ---- one-row
o <- sen[sen$state == "GA", c("state", "senator_last", "party", "pres24_winner",
                              "pres24_margin", "hostile_turf")]
names(o) <- c("state", "incumbent", "party", "2024 presidential winner",
              "2024 margin", "hostile turf?")
o

## ---- map
data.frame(
  quantity = c("Seats up", "Republican seats being defended",
               "Democratic seats being defended",
               "Seats where the other party's presidential candidate won in 2024"),
  value = c(nrow(sen), sum(sen$party == "Republican"),
            sum(sen$party == "Democrat"), sum(sen$hostile_turf)))

## ---- hostile
o <- sen[sen$hostile_turf, c("state", "senator_last", "party", "pres24_winner",
                             "pres24_margin")]
names(o) <- c("state", "incumbent", "party", "2024 winner", "2024 margin")
o

## ---- tile-static
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0.5, max(tile$col) + 0.5), ylim = c(max(tile$row) + 0.6, 0.05),
     axes = FALSE, xlab = "", ylab = "")
fill <- ifelse(!tile$up, "#EDEDED",
        ifelse(tile$party == "Republican", "#C41230", "#2c7fb8"))
rect(tile$col - 0.46, tile$row - 0.46, tile$col + 0.46, tile$row + 0.46,
     col = fill, border = "white", lwd = 1.2)
ht <- which(tile$up & tile$hostile_turf)
rect(tile$col[ht] - 0.46, tile$row[ht] - 0.46,
     tile$col[ht] + 0.46, tile$row[ht] + 0.46, border = "#111111", lwd = 2.4)
text(tile$col, tile$row - 0.14, tile$state, cex = 0.72, font = 2,
     col = ifelse(tile$up, "white", "#AAAAAA"))
text(tile$col[tile$up], tile$row[tile$up] + 0.22,
     paste(tile$letter[tile$up], tile$lab[tile$up]), cex = 0.5, col = "white")
kt <- c(paste0("Republican seat up (", sum(sen$party == "Republican"), ")"),
        paste0("Democratic seat up (", sum(sen$party == "Democrat"), ")"),
        "no seat up in 2026")
kx <- 0.5 + c(0, cumsum(head(strwidth(kt, cex = 0.7) + 0.42, -1)))
rect(kx, 0.16, kx + 0.16, 0.34, col = c("#C41230", "#2c7fb8", "#EDEDED"),
     border = NA)
text(kx + 0.22, 0.25, kt, adj = 0, cex = 0.7, col = "#444444")
text(max(tile$col) + 0.5, 0.25, adj = 1, cex = 0.7, col = "#111111",
     labels = paste0("heavy outline: hostile turf (", sum(sen$hostile_turf), ")"))

## ---- tile-d3
rows <- paste(sprintf(
  '{"st":"%s","c":%d,"r":%d,"up":%s,"p":"%s","nm":"%s","m":%s,"lab":"%s","h":%s}',
  tile$state, tile$col, tile$row, tolower(as.character(tile$up)),
  ifelse(is.na(tile$party), "", tile$party),
  ifelse(is.na(tile$senator_last), "", tile$senator_last),
  ifelse(is.na(tile$pres24_margin), "null", sprintf("%.2f", tile$pres24_margin)),
  tile$lab, tolower(as.character(!is.na(tile$hostile_turf) & tile$hostile_turf))),
  collapse = ",")
cat(paste0('
<div id="tl" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const NC=', max(tile$col), ', NR=', max(tile$row), ';
const W=760,M={t:26,r:8,b:8,l:8},cell=Math.min((W-M.l-M.r)/NC,64);
const H=M.t+M.b+NR*cell;
const box=d3.select("#tl");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const px=d=>M.l+(d.c-1)*cell, py=d=>M.t+(d.r-1)*cell;
const fill=d=>!d.up?"#EDEDED":(d.p==="Republican"?"#C41230":"#2c7fb8");
const key=[["#C41230","Republican seat up (', sum(sen$party == "Republican"), ')"],
           ["#2c7fb8","Democratic seat up (', sum(sen$party == "Democrat"), ')"],
           ["#EDEDED","no seat up in 2026"]];
let kx=M.l;
key.forEach(k=>{
  svg.append("rect").attr("x",kx).attr("y",4).attr("width",12).attr("height",12)
    .attr("fill",k[0]);
  svg.append("text").attr("x",kx+16).attr("y",14).attr("font-size","11px")
    .attr("fill","#444").text(k[1]);
  kx+=k[1].length*6.0+30;
});
svg.append("text").attr("x",W-M.r).attr("y",14).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#111")
  .text("heavy outline: hostile turf (', sum(sen$hostile_turf), ')");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
g.selectAll("rect").data(D).join("rect")
  .attr("x",d=>px(d)+1.5).attr("y",d=>py(d)+1.5)
  .attr("width",cell-3).attr("height",cell-3)
  .attr("fill",fill).attr("stroke",d=>d.h?"#111":"none").attr("stroke-width",2.4)
  .on("mousemove",function(ev,d){
    if(!d.up){ tip.style("opacity",0); return; }
    tip.style("opacity",1).html(
      `<b>${d.st} — ${d.nm} (${d.p})</b><br>2024 margin ${d.lab}`+
      (d.h?"<br><i>hostile turf</i>":""))
      .style("left",Math.min(ev.offsetX+14,W-250)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
g.selectAll("text.s").data(D).join("text").attr("class","s")
  .attr("x",d=>px(d)+cell/2).attr("y",d=>py(d)+cell/2+1)
  .attr("text-anchor","middle").attr("font-size","13px").attr("font-weight","700")
  .attr("pointer-events","none")
  .attr("fill",d=>d.up?"#fff":"#AAA").text(d=>d.st);
g.selectAll("text.m").data(D.filter(d=>d.up)).join("text").attr("class","m")
  .attr("x",d=>px(d)+cell/2).attr("y",d=>py(d)+cell/2+15)
  .attr("text-anchor","middle").attr("font-size","10px").attr("fill","#fff")
  .attr("pointer-events","none").text(d=>d.p.charAt(0)+" "+d.lab);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One square per state. The letter is the party defending the seat and the number
is that state&#39;s 2024 presidential margin. Hover for the incumbent.</p>
'))

## ---- safe
data.frame(
  quantity = c("Seats where the 2024 margin was 15 points or more",
               "Share of the seats up", "Median 2024 margin, absolute value",
               "Seats where the 2024 margin was under 5 points"),
  value = c(nrow(safe), paste0(pc(100 * nrow(safe) / nrow(sen), 0), "%"),
            paste0(pc(median(sen$abs_margin)), " points"),
            sum(sen$abs_margin < 5)))

## ---- sen-static
s <- sen[order(sen$pres24_margin), ]
par(mar = c(4.2, 4.5, 1, 1))
plot(s$pres24_margin, seq_len(nrow(s)), pch = 19, cex = 1.1,
     col = ifelse(s$party == "Republican", "#B2182B", "#2166AC"),
     xlab = "2024 presidential margin in the state (Trump minus Harris)",
     ylab = "", yaxt = "n", xlim = c(-50, 50))
axis(2, at = seq_len(nrow(s)), labels = s$state, las = 1, cex.axis = 0.6)
abline(v = 0, lty = 2)
abline(v = c(-5, 5), lty = 3, col = "gray60")
legend("topleft", c("Republican seat", "Democratic seat"), pch = 19,
       col = c("#B2182B", "#2166AC"), bty = "n", cex = 0.75)

## ---- sen-d3
s <- sen[order(sen$pres24_margin), ]
rows <- paste(sprintf('{"st":"%s","nm":"%s","p":"%s","m":%.2f,"h":%s}',
                      s$state, s$senator_last, s$party, s$pres24_margin,
                      tolower(as.character(s$hostile_turf))), collapse = ",")
cat(sprintf('
<div id="sen" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=560,M={t:26,r:24,b:44,l:44};
const box=d3.select("#sen");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-50,50]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.st)).range([M.t,H-M.b]).padding(0.25);
svg.append("rect").attr("x",x(-5)).attr("y",M.t).attr("width",x(5)-x(-5))
  .attr("height",H-M.b-M.t).attr("fill","#888").attr("opacity",0.09);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>(d>0?"+":"")+d));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0));
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#666").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("2024 presidential margin in the state (Trump minus Harris)");
svg.append("text").attr("x",x(-5)-6).attr("y",M.t-10).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#2166AC").text("Harris states");
svg.append("text").attr("x",x(5)+6).attr("y",M.t-10)
  .attr("font-size","11px").attr("fill","#B2182B").text("Trump states");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.m)).attr("cy",d=>y(d.st)+y.bandwidth()/2)
  .attr("r",d=>d.h?7:5)
  .attr("fill",d=>d.p==="Republican"?"#B2182B":"#2166AC")
  .attr("stroke",d=>d.h?"#111":"none").attr("stroke-width",1.6)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.st} — ${d.nm} (${d.p})</b><br>2024 margin ${(d.m>0?"+":"")+d.m}`+
      (d.h?"<br><i>hostile turf</i>":""))
      .style("left",Math.min(ev.offsetX+14,W-250)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Color is the party defending the seat; ringed points sit on hostile turf. The
shaded band is five points either side of even.</p>
', rows))

## ---- baseline
data.frame(
  outcome = c("Seats the baseline gives Republicans",
              "Seats the baseline gives Democrats",
              "Seats where the baseline implies a party change"),
  value = c(sum(sen$baseline == "Republican"), sum(sen$baseline == "Democrat"),
            sum(sen$flips)))

## ---- rehearsal
data.frame(
  quantity = c("Districts", "Where the presidential winner's party won the seat",
               "Accuracy"),
  value = c(nrow(reh), sum(reh$as_expected),
            paste0(pc(acc(reh$as_expected)), "%")))

## ---- bands
o <- band_tab
o$pct <- pc(o$pct)
names(o) <- c("districts...", "how many", "rule correct in", "accuracy (%)")
o

## ---- outside
o <- do.call(rbind, lapply(c(2, 5, 10, 15), function(b) {
  i <- reh$dev > b
  data.frame(a = paste0("more than ", b, " points from even"),
             b = sum(i), c = sum(reh$as_expected[i]),
             d = pc(acc(reh$as_expected[i]))) }))
names(o) <- c("districts...", "how many", "rule correct in", "accuracy (%)")
o

## ---- regimes-static
par(mar = c(4.4, 4.4, 1.0, 1))
plot(NA, xlim = c(0, max(bs)), ylim = c(60, 101), las = 1,
     xlab = "cut-off: distance from an even split, in points",
     ylab = "accuracy of the rule (%)")
abline(h = seq(60, 100, 10), col = "#EEEEEE")
segments(b100, 60, b100, 100, col = "#999999", lty = 3)
lines(bs, accout, type = "s", col = "#2c7fb8", lwd = 2.6)
lines(bs, accin,  type = "s", col = "#C41230", lwd = 2.6)
points(b100, 100, pch = 19, cex = 0.9, col = "#2c7fb8")
text(max(bs), 98.0, "districts FURTHER than the cut-off from even",
     pos = 2, offset = 0.3, cex = 0.72, col = "#2c7fb8")
text(max(bs), 92.8, "districts WITHIN the cut-off of even",
     pos = 2, offset = 0.3, cex = 0.72, col = "#C41230")
text(b100 + 0.4, 66, paste0("beyond ", pc(b100), " points the rule\nnever missed once"),
     adj = 0, cex = 0.72, col = "#555555")

## ---- regimes-d3
rows <- paste(sprintf('{"b":%.1f,"i":%.2f,"o":%.2f,"ni":%d,"no":%d}',
                      bs, accin, accout,
                      sapply(bs, function(b) sum(reh$dev <= b)),
                      sapply(bs, function(b) sum(reh$dev >  b))), collapse = ",")
cat(paste0('
<div id="rg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '], B100=', b100, ';
const W=760,H=400,M={t:16,r:24,b:48,l:56};
const box=d3.select("#rg");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.b)]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([60,101]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(10));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(5).tickFormat(d=>d+"%"));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("cut-off: distance from an even split, in points");
svg.append("line").attr("x1",x(B100)).attr("x2",x(B100)).attr("y1",y(60)).attr("y2",y(100))
  .attr("stroke","#999").attr("stroke-dasharray","3,3");
const ln=d3.line().curve(d3.curveStepAfter).x(d=>x(d.b));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#2c7fb8")
  .attr("stroke-width",2.6).attr("d",ln.y(d=>y(d.o)));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.6).attr("d",ln.y(d=>y(d.i)));
svg.append("text").attr("x",W-M.r-4).attr("y",y(98.0))
  .attr("text-anchor","end").attr("font-size","11px").attr("fill","#2c7fb8")
  .text("districts FURTHER than the cut-off from even");
svg.append("text").attr("x",W-M.r-4).attr("y",y(92.8))
  .attr("text-anchor","end").attr("font-size","11px").attr("fill","#C41230")
  .text("districts WITHIN the cut-off of even");
svg.append("text").attr("x",x(B100)+6).attr("y",y(66)).attr("font-size","11px")
  .attr("fill","#555").text("beyond ', pc(b100), ' points the rule never missed once");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const b=x.invert(d3.pointer(ev,this)[0]);
    const d=D.reduce((a,c)=>Math.abs(c.b-b)<Math.abs(a.b-b)?c:a);
    tip.style("opacity",1).html(
      `<b>cut-off ${d.b.toFixed(1)} points</b><br>`+
      `within: ${d.i.toFixed(1)}% of ${d.ni} districts<br>`+
      `beyond: ${d.o.toFixed(1)}% of ${d.no} districts`)
      .style("left",Math.min(ev.offsetX+14,W-240)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The same rule, scored twice at every cut-off. Hover for the district counts
behind each pair of percentages.</p>
'))

## ---- misses
o <- miss[order(miss$dem_share), c("district", "dem_share", "pres_party",
                                   "house_rep", "house_rep_party")]
o$dem_share <- pc(o$dem_share, 2)
names(o) <- c("district", "Democratic % (president)", "presidential winner",
              "member elected", "member's party")
o

## ---- strip-static
s  <- miss[order(miss$dem_share), ]
up <- s$pres_party == "republican"
par(mar = c(4.4, 0.4, 1.6, 0.4))
plot(NA, xlim = c(43.5, 55), ylim = c(-1.1, nrow(s) * 0.5 + 1.2), yaxt = "n",
     xlab = "Democratic share of the two-party presidential vote (%)", ylab = "",
     bty = "n")
segments(50, -0.9, 50, nrow(s) * 0.5 + 0.6, lty = 2, col = "#666666")
rug(reh$dem_share[reh$as_expected], side = 1, ticksize = 0.03, col = "#CCCCCC")
yy <- seq_len(nrow(s)) * 0.5 - 0.2
points(s$dem_share, yy, pch = ifelse(up, 24, 25), cex = 1.05,
       bg = ifelse(up, "#2c7fb8", "#C41230"),
       col = ifelse(up, "#2c7fb8", "#C41230"))
text(s$dem_share, yy, s$district, pos = ifelse(s$dem_share < 50, 2, 4),
     cex = 0.62, col = "#333333")
text(43.5, nrow(s) * 0.5 + 1.0, adj = 0, cex = 0.72, col = "#2c7fb8",
     labels = paste0("up-triangle: Trump district, Democratic member (",
                     sum(up), ")"))
text(55, nrow(s) * 0.5 + 1.0, adj = 1, cex = 0.72, col = "#C41230",
     labels = paste0("down-triangle: Harris district, Republican member (",
                     sum(!up), ")"))
text(49.9, -0.5, paste0("gray ticks below the axis: the ", sum(reh$as_expected),
                        " districts the rule got right"),
     cex = 0.68, col = "#777777")

## ---- strip-d3
s <- miss[order(miss$dem_share), ]
rows <- paste(sprintf('{"d":"%s","v":%.2f,"pp":"%s","hp":"%s","mb":"%s","i":%d}',
                      s$district, s$dem_share, s$pres_party, s$house_rep_party,
                      s$house_rep, seq_len(nrow(s))), collapse = ",")
ok <- paste(sprintf('%.2f', reh$dem_share[reh$as_expected]), collapse = ",")
cat(paste0('
<div id="st" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '], OK=[', ok, '];
const W=760,H=380,M={t:34,r:24,b:46,l:24};
const box=d3.select("#st");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([43.5,55]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,D.length+1]).range([H-M.b-16,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>d+"%"));
svg.append("g").selectAll("line").data(OK).join("line")
  .attr("x1",d=>x(d)).attr("x2",d=>x(d)).attr("y1",H-M.b).attr("y2",H-M.b-7)
  .attr("stroke","#CCCCCC");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t-6).attr("y2",H-M.b)
  .attr("stroke","#666").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party presidential vote (%)");
svg.append("text").attr("x",M.l).attr("y",16).attr("font-size","11px")
  .attr("fill","#2c7fb8")
  .text("\\u25B2 Trump district, Democratic member (', sum(miss$pres_party == "republican"), ')");
svg.append("text").attr("x",W-M.r).attr("y",16).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("\\u25BC Harris district, Republican member (', sum(miss$pres_party != "republican"), ')");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const sym=d3.symbol().type(d3.symbolTriangle).size(95)();
svg.append("g").selectAll("path").data(D).join("path")
  .attr("d",sym)
  .attr("transform",d=>`translate(${x(d.v)},${y(d.i)}) rotate(${d.pp==="republican"?0:180})`)
  .attr("fill",d=>d.pp==="republican"?"#2c7fb8":"#C41230")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.d}</b>: ${d.v.toFixed(2)}% Democratic<br>`+
      `presidential winner: ${d.pp}<br>member elected: ${d.mb} (${d.hp})`)
      .style("left",Math.min(ev.offsetX+14,W-280)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text").data(D).join("text")
  .attr("x",d=>x(d.v)+(d.v<50?-9:9)).attr("y",d=>y(d.i)+4)
  .attr("text-anchor",d=>d.v<50?"end":"start")
  .attr("font-size","10.5px").attr("fill","#333").attr("pointer-events","none")
  .text(d=>d.d);
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",32).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#777")
  .text("gray ticks along the axis: the ', sum(reh$as_expected), ' districts the rule got right");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Every district the rule missed, on the same axis as the ', sum(reh$as_expected), '
it did not. Hover for the member elected.</p>
'))

## ---- hist-static
h <- hist(reh$dem_share, breaks = seq(0, 100, 2.5), plot = FALSE)
plot(h, col = "#CCCCCC", border = "white", main = "",
     xlab = "Democratic share of the two-party presidential vote (%)",
     ylab = "districts")
hm <- hist(miss$dem_share, breaks = seq(0, 100, 2.5), plot = FALSE)
plot(hm, col = "#C41230", border = "white", add = TRUE)
abline(v = c(45, 55), lty = 3, col = "gray40")
legend("topright", c("all districts", "rule was wrong"),
       fill = c("#CCCCCC", "#C41230"), border = NA, bty = "n", cex = 0.8)

## ---- hist-d3
br <- seq(0, 100, 2.5)
ha <- hist(reh$dem_share, breaks = br, plot = FALSE)
hm <- hist(miss$dem_share, breaks = br, plot = FALSE)
rows <- paste(sprintf('{"x":%.2f,"a":%d,"m":%d}', ha$mids, ha$counts, hm$counts),
              collapse = ",")
cat(sprintf('
<div id="hd" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const B=[%s];
const W=760,H=400,M={t:20,r:24,b:46,l:52};
const box=d3.select("#hd");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(B,d=>d.a)*1.06]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(10).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party presidential vote in the district");
svg.append("text").attr("x",M.l).attr("y",M.t-6).attr("font-size","11px")
  .attr("fill","#666").text("districts");
const w=(x(2.5)-x(0))-1;
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
g.selectAll("rect.a").data(B).join("rect").attr("class","a")
  .attr("x",d=>x(d.x)-w/2).attr("y",d=>y(d.a)).attr("width",w)
  .attr("height",d=>y(0)-y(d.a)).attr("fill","#CCCCCC")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${(d.x-1.25).toFixed(1)}–${(d.x+1.25).toFixed(1)}%%</b><br>${d.a} districts<br>`+
      `rule wrong in ${d.m}`)
      .style("left",Math.min(ev.offsetX+12,W-200)+"px").style("top",(ev.offsetY-8)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
g.selectAll("rect.m").data(B).join("rect").attr("class","m")
  .attr("x",d=>x(d.x)-w/2).attr("y",d=>y(d.m)).attr("width",w)
  .attr("height",d=>y(0)-y(d.m)).attr("fill","#C41230").attr("pointer-events","none");
[45,55].forEach(v=>svg.append("line").attr("x1",x(v)).attr("x2",x(v))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#999").attr("stroke-dasharray","3,3"));
svg.append("text").attr("x",W-M.r-4).attr("y",M.t+12).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("red: the rule was wrong");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Every failure of the rule sits in the middle of the distribution. Hover for
counts.</p>
', rows))

## ---- apply
data.frame(
  zone = c("More than 15 points from even in 2024",
           "Between 5 and 15 points", "Within 5 points"),
  seats = c(sum(sen$abs_margin > 15),
            sum(sen$abs_margin >= 5 & sen$abs_margin <= 15),
            sum(sen$abs_margin < 5)),
  what_the_rehearsal_says = c(
    paste0("rule was ", pc(acc(reh$as_expected[reh$dev > 15])), "% right here"),
    paste0("rule was ",
           pc(acc(reh$as_expected[reh$dev >= 5 & reh$dev <= 15])), "% right here"),
    paste0("rule was ", pc(acc(reh$as_expected[reh$dev < 5])), "% right here")))

## ---- close-seats
o <- sen[sen$abs_margin < 5, c("state", "senator_last", "party",
                               "pres24_winner", "pres24_margin")]
o <- o[order(abs(o$pres24_margin)), ]
names(o) <- c("state", "incumbent", "party", "2024 winner", "2024 margin")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
