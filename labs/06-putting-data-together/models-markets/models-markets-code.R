# models-markets-code.R -- chunk bodies for models-markets-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.8,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

d      <- read.csv("data/derived/polymarket_resolved.csv", stringsAsFactors = FALSE)
d$p30  <- suppressWarnings(as.numeric(d$p30))
bands  <- cut(d$p7, breaks = seq(0, 1, 0.1), include.lowest = TRUE)

pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
p3  <- function(x) formatC(x, format = "f", digits = 3)
p4  <- function(x) formatC(x, format = "f", digits = 4)
cnt <- function(x) format(round(x), big.mark = ",")
usd <- function(x) paste0("$", format(round(x / 1e6), big.mark = ","), "m")

tab <- data.frame(band     = levels(bands),
                  n        = as.vector(table(bands)),
                  priced   = as.vector(tapply(d$p7,      bands, mean)),
                  happened = as.vector(tapply(d$outcome, bands, mean)))
tab$gap <- tab$happened - tab$priced
gb <- function(lv, col) tab[[col]][tab$band == lv]

brier    <- mean((d$p7 - d$outcome)^2)
# one market's score, for the worked example in the brief: the largest market
o1     <- d[order(-d$volume), ][1, ]
B1     <- (o1$p7 - o1$outcome)^2
B1_ALT <- (o1$p7 - (1 - o1$outcome))^2      # had it gone the other way
brier50  <- mean((0.5 - d$outcome)^2)
brierbr  <- mean((mean(d$outcome) - d$outcome)^2)

# per-band exact tests
bt <- do.call(rbind, lapply(levels(bands), function(lv) {
  k <- bands == lv; n <- sum(k)
  if (n < 5) return(NULL)
  data.frame(band = lv, n = n, expected = sum(d$p7[k]), observed = sum(d$outcome[k]),
             p = binom.test(sum(d$outcome[k]), n, mean(d$p7[k]))$p.value)
}))
k_pool <- d$p7 > 0.1 & d$p7 <= 0.3
p_pool <- binom.test(sum(d$outcome[k_pool]), sum(k_pool), mean(d$p7[k_pool]))$p.value

# volume split, restricted to the mid-range where calibration bites
mid  <- d[d$p7 > 0.1 & d$p7 < 0.9, ]
vmed <- median(mid$volume)
mhi  <- mid[mid$volume >  vmed, ]
mlo  <- mid[mid$volume <= vmed, ]
dmed <- median(d$volume)

# seven days out versus thirty
k30 <- !is.na(d$p30)
b7  <- mean((d$p7[k30]  - d$outcome[k30])^2)
b30 <- mean((d$p30[k30] - d$outcome[k30])^2)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- d3-load
# the one and only copy of d3 in this document
cat('<script src="../../_lib/d3.v7.min.js"></script>\n')

## ---- one-record
o <- d[order(-d$volume), ][1, ]
data.frame(
  field = c("question", "p7", "p30", "outcome", "volume", "end_date"),
  value = c(o$question, p4(o$p7), p4(o$p30), o$outcome,
            cnt(o$volume), o$end_date),
  what_it_is = c("the contract's wording",
                 "price seven days before resolution",
                 "price thirty days before resolution",
                 "1 if it happened, 0 if it did not",
                 "traded volume in dollars",
                 "the day it settled"),
  check.names = FALSE)

## ---- scope
data.frame(
  quantity = c("Resolved markets", "Resolving between",
               "Markets that resolved 'yes'", "Markets priced under 0.10",
               "Median traded volume", "With a price thirty days out"),
  value = c(cnt(nrow(d)), paste(min(d$end_date), "to", max(d$end_date)),
            paste0(cnt(sum(d$outcome)), " (", pc(100 * mean(d$outcome)), "%)"),
            paste0(cnt(sum(d$p7 < 0.10)), " (", pc(100 * mean(d$p7 < 0.10)), "%)"),
            usd(dmed), cnt(sum(k30))))

## ---- swarm-prep
set.seed(11)
sw       <- d[order(-d$volume), ]
sw$jit   <- runif(nrow(sw), -0.40, 0.40)
# Radius is the square root of volume with NO additive constant, so dot AREA is
# genuinely proportional to money traded and the caption is literally true. The
# constant that used to sit here was larger than the signal for all but a
# handful of markets, which flattened exactly the variation the split by money
# is about.
sw$rad   <- sqrt(sw$volume / max(sw$volume))
lo_share <- mean(sw$p7 < 0.10)

# reference volumes for the size legend both formats now carry
LEGV <- c(25e6, 250e6, 1500e6)
LEGR <- sqrt(LEGV / max(sw$volume))
LEGL <- paste0("$", format(round(LEGV / 1e6), big.mark = ",", trim = TRUE), "m")
D3R  <- 9.0                      # d3 radius, px, at the largest market
STR  <- 3.4                      # base-R cex at the largest market
cap_sw <- sprintf(paste(
  "%s dots, one per market, area proportional to traded volume: the largest",
  "market traded %s and the median %s. The lower strip is crowded against the",
  "left wall, %s%% of every question asked here was priced under 0.10, and",
  "%s%% of the file resolved no."),
  cnt(nrow(sw)), usd(max(sw$volume)), usd(median(sw$volume)),
  pc(100 * lo_share, 0), pc(100 * (1 - mean(d$outcome)), 0))

## ---- swarm-static
par(mar = c(8.0, 8.6, 1.6, 1.6))
plot(NA, xlim = c(-0.02, 1.02), ylim = c(-0.75, 1.75), yaxt = "n", bty = "n",
     las = 1, ylab = "", xlab = "price seven days before resolution")
abline(v = seq(0, 1, 0.1), col = "grey94")
segments(0, 1.55, 1, 1.55, col = "grey85")
for (o in 0:1) {
  k <- sw$outcome == o
  points(sw$p7[k], o + sw$jit[k], pch = 19, cex = STR * sw$rad[k],
         col = adjustcolor(if (o == 1) "#2c7fb8" else "#999999", 0.55))
  m <- mean(sw$p7[k])
  segments(m, o - 0.52, m, o + 0.52, lwd = 2.4,
           col = if (o == 1) "#2c7fb8" else "#666666")
}
axis(2, at = c(1, 0),
     labels = c(paste0("it happened\n(", sum(sw$outcome == 1), " markets)"),
                paste0("it did not\n(", sum(sw$outcome == 0), " markets)")),
     las = 1, tick = FALSE, cex.axis = 0.8)
# the size legend: without it a reader cannot turn a dot back into a number
lgx <- c(0.60, 0.72, 0.86)
points(lgx, rep(-0.60, 3), pch = 19, cex = STR * LEGR,
       col = adjustcolor("#999999", 0.75))
text(lgx, rep(-0.72, 3), LEGL, cex = 0.58, col = "#666666")
text(0.55, -0.60, "traded volume:", cex = 0.62, col = "#666666", pos = 2)
mtext(paste0("Dot area is proportional to traded volume. Vertical rules ",
             "are each strip's average price."),
      side = 3, line = 0.2, cex = 0.74, adj = 0)
cw <- strwrap(cap_sw, width = 100)
mtext(cw, side = 1, line = 4.4 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.66, col = "#555555")

## ---- swarm-d3
rows <- paste(sprintf('[%.4f,%.3f,%d,%.4f,%.0f]', sw$p7, sw$jit, sw$outcome,
                      sw$rad, sw$volume / 1e6), collapse = ",")
qs <- gsub('"', "", substr(sw$question, 1, 62))
qs <- paste(sprintf('"%s"', gsub("\\\\", "", qs)), collapse = ",")
cat(sprintf('
<div id="swarm" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], Q=[%s];
const W=780,H=360,M={t:34,r:26,b:46,l:132};
const box=d3.select("#swarm");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-0.02,1.02]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([-0.75,1.75]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6,d3.format(".1f")));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("price seven days before resolution");
svg.append("line").attr("x1",x(0)).attr("x2",x(1)).attr("y1",y(1.55)).attr("y2",y(1.55))
  .attr("stroke","#ddd");
const n1=D.filter(p=>p[2]===1).length, n0=D.length-n1;
[[1,"it happened","#2c7fb8",n1],[0,"it did not","#666666",n0]].forEach(r=>{
  svg.append("text").attr("x",M.l-12).attr("y",y(r[0])-2).attr("text-anchor","end")
    .attr("font-size","12.5px").attr("fill",r[2]).text(r[1]);
  svg.append("text").attr("x",M.l-12).attr("y",y(r[0])+14).attr("text-anchor","end")
    .attr("font-size","11px").attr("fill","#888").text(r[3]+" markets");
  const m=d3.mean(D.filter(p=>p[2]===r[0]),p=>p[0]);
  svg.append("line").attr("x1",x(m)).attr("x2",x(m))
    .attr("y1",y(r[0]-0.52)).attr("y2",y(r[0]+0.52))
    .attr("stroke",r[2]).attr("stroke-width",2.6);
});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;max-width:330px");
tip.style("opacity",0);
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",p=>x(p[0])).attr("cy",p=>y(p[2]+p[1]))
  .attr("r",p=>%.2f*p[3])
  .attr("fill",p=>p[2]===1?"#2c7fb8":"#999999").attr("fill-opacity",0.55)
  .on("mousemove",function(e,p){
    const i=D.indexOf(p);
    tip.style("opacity",1).html(`<b>${Q[i]}</b><br>priced ${p[0].toFixed(4)}<br>`+
      `${p[2]===1?"happened":"did not happen"}<br>$${d3.format(",.0f")(p[4])}m traded`)
      .style("left",Math.min(e.offsetX+14,W-340)+"px").style("top",(e.offsetY-8)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
// size legend: without it a reader cannot turn a dot back into a number
const LG=[%s], LL=[%s];
const lg=svg.append("g").attr("transform",`translate(${W-M.r-150},${M.t-6})`);
lg.append("text").attr("x",-10).attr("y",4).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#666").text("traded volume");
LG.forEach((r,i)=>{
  const cx=i*52+12;
  lg.append("circle").attr("cx",cx).attr("cy",0).attr("r",%.2f*r)
    .attr("fill","#999999").attr("fill-opacity",0.75);
  lg.append("text").attr("x",cx).attr("y",20).attr("text-anchor","middle")
    .attr("font-size","10px").attr("fill","#666").text(LL[i]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover any dot for
the question behind it.</p>
', rows, qs, D3R,
   paste(sprintf("%.4f", LEGR), collapse = ","),
   paste(sprintf('"%s"', LEGL), collapse = ","), D3R, cap_sw))

## ---- one-forecast
p <- 0.72
data.frame(
  what_happened = c("It happened", "It did not happen"),
  is_the_forecast_refuted = c("No", "No"),
  the_reply = c("72% is the larger side; this is the expected case",
                "28% events occur roughly three times in ten"),
  check.names = FALSE)

## ---- calibration
o <- data.frame(band = tab$band, n = tab$n,
                priced = p3(tab$priced), happened = p3(tab$happened),
                gap = sprintf("%+.3f", tab$gap))
names(o) <- c("price band", "markets", "average price", "share that happened",
              "gap")
o

## ---- calib-static
plot(tab$priced, tab$happened, pch = 19, cex = sqrt(tab$n) / 3.6,
     col = "#2c7fb8", xlim = c(0, 1), ylim = c(0, 1), las = 1,
     xlab = "price seven days out",
     ylab = "share that actually happened")
abline(0, 1, lty = 2, col = "grey40")
text(tab$priced, tab$happened, tab$n, pos = 3, cex = 0.7, col = "grey25")

## ---- calib-d3
rows <- paste(sprintf('{"b":"%s","n":%d,"p":%.4f,"h":%.4f}',
                      tab$band, tab$n, tab$priced, tab$happened), collapse = ",")
cat(sprintf('
<div id="cal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=700,H=520,M={t:20,r:26,b:48,l:60};
const svg=d3.select("#cal").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,1]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6,d3.format(".1f")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6,d3.format(".1f")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("price seven days before resolution");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("share that actually happened");
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(1)).attr("y2",y(1))
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(0.78)).attr("y",y(0.60)).attr("font-size","11px")
  .attr("fill","#888").attr("text-anchor","middle")
  .attr("transform",`rotate(-40,${x(0.78)},${y(0.60)})`)
  .text("perfect calibration");
const r=d3.scaleSqrt().domain([0,d3.max(D,d=>d.n)]).range([0,44]);
const tip=d3.select("#cal").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.p)).attr("cy",d=>y(d.h)).attr("r",d=>Math.max(r(d.n),4))
  .attr("fill","#2c7fb8").attr("fill-opacity",0.3)
  .attr("stroke","#2c7fb8").attr("stroke-width",1.8)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(`<b>${d.b}</b><br>${d.n} markets<br>`+
      `average price ${d3.format(".3f")(d.p)}<br>happened ${d3.format(".1%%")(d.h)}`)
      .style("left",Math.min(e.offsetX+14,W-210)+"px").style("top",(e.offsetY-10)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Circle area is the number of markets in the band. Points on the dashed line are
perfectly calibrated. Hover for detail.</p>
', rows))

## ---- brier
data.frame(
  forecaster = c("The market, seven days out", "Always say 50%",
                 "Always say the overall base rate",
                 "Always say 0% (nothing will happen)"),
  brier = p4(c(brier, brier50, brierbr, mean((0 - d$outcome)^2))))

## ---- low-bands
o <- data.frame(band = tab$band[1:3], n = tab$n[1:3],
                priced = p3(tab$priced[1:3]), happened = p3(tab$happened[1:3]),
                ratio = pc(tab$happened[1:3] / tab$priced[1:3], 1))
names(o) <- c("price band", "markets", "average price", "share that happened",
              "happened ÷ priced")
o

## ---- sig
o <- data.frame(band = bt$band, n = bt$n, expected = pc(bt$expected),
                observed = bt$observed, p = p3(bt$p),
                flag = ifelse(bt$p < 0.05, "*", ""))
names(o) <- c("price band", "markets", "expected 'yes'", "observed 'yes'",
              "p", "")
o

## ---- volume-split
data.frame(
  markets = c("Above median volume", "Below median volume"),
  n = c(nrow(mhi), nrow(mlo)),
  average_price = p3(c(mean(mhi$p7), mean(mlo$p7))),
  share_happened = p3(c(mean(mhi$outcome), mean(mlo$outcome))),
  gap_points = sprintf("%+.1f", 100 * c(mean(mhi$outcome) - mean(mhi$p7),
                                        mean(mlo$outcome) - mean(mlo$p7))),
  check.names = FALSE)

## ---- vbell-prep
vb <- data.frame(
  lab = c("mid-range, above median volume", "mid-range, below median volume",
          "all markets, above median volume", "all markets, below median volume"),
  n = c(nrow(mhi), nrow(mlo), sum(d$volume > dmed), sum(d$volume <= dmed)),
  priced = c(mean(mhi$p7), mean(mlo$p7),
             mean(d$p7[d$volume > dmed]), mean(d$p7[d$volume <= dmed])),
  happened = c(mean(mhi$outcome), mean(mlo$outcome),
               mean(d$outcome[d$volume > dmed]), mean(d$outcome[d$volume <= dmed])),
  stringsAsFactors = FALSE)
vb$gap <- 100 * (vb$happened - vb$priced)

## ---- vbell-static
yy <- rev(seq_len(nrow(vb)))
par(mar = c(4.2, 12.6, 1.2, 4.4))
plot(NA, xlim = c(0, max(vb$priced, vb$happened) * 1.14),
     ylim = c(0.5, nrow(vb) + 0.5), yaxt = "n", bty = "n", las = 1, ylab = "",
     xlab = "probability")
abline(v = pretty(c(0, max(vb$priced, vb$happened))), col = "grey94")
cl <- ifelse(abs(vb$gap) > 3, "#C41230", "#999999")
segments(vb$priced, yy, vb$happened, yy, col = cl, lwd = 3)
points(vb$priced, yy, pch = 21, bg = "white", col = "#2c7fb8", cex = 1.4,
       lwd = 2.2)
points(vb$happened, yy, pch = 19, col = cl, cex = 1.4)
axis(2, at = yy, labels = paste0(vb$lab, "\nn = ", vb$n), las = 1, tick = FALSE,
     cex.axis = 0.7)
text(pmax(vb$priced, vb$happened), yy, paste0(" ", sprintf("%+.1f", vb$gap)),
     pos = 4, cex = 0.72, col = cl, xpd = NA)
legend("bottomright", c("priced", "happened"), pch = c(21, 19),
       col = c("#2c7fb8", "#999999"), pt.bg = "white", bty = "n", cex = 0.72)

## ---- vbell-d3
rows <- paste(sprintf('{"l":"%s","n":%d,"p":%.4f,"h":%.4f,"g":%.2f}',
                      vb$lab, vb$n, vb$priced, vb$happened, vb$gap),
              collapse = ",")
cat(sprintf('
<div id="vbell" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=780,H=300,M={t:34,r:64,b:46,l:236};
const svg=d3.select("#vbell").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.p,d.h))*1.1])
  .range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.l)).range([M.t,H-M.b]).padding(0.46);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("probability");
const cy=d=>y(d.l)+y.bandwidth()/2, cl=d=>Math.abs(d.g)>3?"#C41230":"#999999";
D.forEach(d=>{
  svg.append("text").attr("x",M.l-12).attr("y",cy(d)-2).attr("text-anchor","end")
    .attr("font-size","11.5px").attr("fill","#333").text(d.l);
  svg.append("text").attr("x",M.l-12).attr("y",cy(d)+13).attr("text-anchor","end")
    .attr("font-size","10.5px").attr("fill","#888").text("n = "+d.n);
});
const g=svg.append("g");
g.selectAll("line").data(D).join("line")
  .attr("x1",d=>x(d.p)).attr("x2",d=>x(d.h)).attr("y1",cy).attr("y2",cy)
  .attr("stroke",cl).attr("stroke-width",4);
g.selectAll("circle.p").data(D).join("circle")
  .attr("cx",d=>x(d.p)).attr("cy",cy).attr("r",6).attr("fill","#fff")
  .attr("stroke","#2c7fb8").attr("stroke-width",2.4);
g.selectAll("circle.h").data(D).join("circle")
  .attr("cx",d=>x(d.h)).attr("cy",cy).attr("r",6).attr("fill",cl);
g.selectAll("text.g").data(D).join("text")
  .attr("x",d=>Math.max(x(d.p),x(d.h))+11).attr("y",d=>cy(d)+4)
  .attr("font-size","11.5px").attr("fill",cl).text(d=>d3.format("+.1f")(d.g));
const key=svg.append("g").attr("font-size","11px");
key.append("circle").attr("cx",W-M.r-150).attr("cy",M.t-16).attr("r",6)
  .attr("fill","#fff").attr("stroke","#2c7fb8").attr("stroke-width",2.4);
key.append("text").attr("x",W-M.r-138).attr("y",M.t-12).attr("fill","#2c7fb8").text("priced");
key.append("circle").attr("cx",W-M.r-84).attr("cy",M.t-16).attr("r",6).attr("fill","#999999");
key.append("text").attr("x",W-M.r-72).attr("y",M.t-12).attr("fill","#666").text("happened");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Gap in percentage points at the right-hand end. In the mid-range the busy
markets are off by %.1f and the thin ones by %.1f — %.0f times as far, on
%d and %d markets respectively. Across all prices the two halves look almost
identical, which is why the split only shows up once the near-zero markets are
set aside.</p>
', rows, abs(vb$gap[1]), abs(vb$gap[2]), abs(vb$gap[2]) / abs(vb$gap[1]),
   vb$n[1], vb$n[2]))

## ---- timing
data.frame(
  snapshot = c("Thirty days before resolution", "Seven days before resolution"),
  markets = c(sum(k30), sum(k30)),
  brier = p4(c(b30, b7)))

## ---- slope-static
sl <- d[k30, ]
sl <- sl[order(sl$outcome), ]
par(mar = c(3.0, 4.4, 2.2, 8.2))
plot(NA, xlim = c(0.86, 2.14), ylim = c(0, 1), xaxt = "n", las = 1, bty = "n",
     xlab = "", ylab = "market price")
abline(h = seq(0, 1, 0.2), col = "grey95")
segments(1, sl$p30, 2, sl$p7, lwd = 1,
         col = ifelse(sl$outcome == 1, adjustcolor("#2c7fb8", 0.30),
                                       adjustcolor("#999999", 0.16)))
for (o in 0:1) {
  k <- sl$outcome == o
  segments(1, mean(sl$p30[k]), 2, mean(sl$p7[k]), lwd = 3.4,
           col = if (o == 1) "#2c7fb8" else "#666666")
  points(c(1, 2), c(mean(sl$p30[k]), mean(sl$p7[k])), pch = 19, cex = 1.2,
         col = if (o == 1) "#2c7fb8" else "#666666")
  text(2.06, mean(sl$p7[k]),
       paste0(if (o == 1) "happened" else "did not", ", average ",
              p3(mean(sl$p7[k]))),
       pos = 4, cex = 0.72, xpd = NA,
       col = if (o == 1) "#2c7fb8" else "#666666")
}
axis(1, at = c(1, 2), labels = c("30 days out", "7 days out"), tick = FALSE,
     line = -0.6)
mtext(paste0("Brier ", p4(b30), " to ", p4(b7), " on the ", sum(k30),
             " markets with both prices"), side = 3, line = 0.6, cex = 0.76,
      adj = 0)

## ---- slope-d3
sl   <- d[k30, ]
sl   <- sl[order(sl$outcome), ]
rows <- paste(sprintf('[%.4f,%.4f,%d]', sl$p30, sl$p7, sl$outcome),
              collapse = ",")
mv <- sapply(0:1, function(o) c(mean(sl$p30[sl$outcome == o]),
                                mean(sl$p7[sl$outcome == o])))
cat(sprintf('
<div id="slope" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MV=[[%.4f,%.4f],[%.4f,%.4f]];
const W=700,H=440,M={t:34,r:196,b:40,l:56};
const svg=d3.select("#slope").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(["30 days out","7 days out"]).range([M.l,W-M.r]).padding(0.12);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6,d3.format(".1f")));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("market price");
x.domain().forEach(t=>svg.append("text").attr("x",x(t)).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444").text(t));
const x1=x("30 days out"), x2=x("7 days out");
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",x1).attr("x2",x2).attr("y1",p=>y(p[0])).attr("y2",p=>y(p[1]))
  .attr("stroke",p=>p[2]===1?"#2c7fb8":"#999999")
  .attr("stroke-opacity",p=>p[2]===1?0.30:0.16).attr("stroke-width",1);
[[0,"#666666","did not happen"],[1,"#2c7fb8","happened"]].forEach(r=>{
  const m=MV[r[0]];
  svg.append("line").attr("x1",x1).attr("x2",x2).attr("y1",y(m[0])).attr("y2",y(m[1]))
    .attr("stroke",r[1]).attr("stroke-width",3.4);
  [[x1,m[0]],[x2,m[1]]].forEach(q=>svg.append("circle").attr("cx",q[0]).attr("cy",y(q[1]))
    .attr("r",5).attr("fill",r[1]));
  svg.append("text").attr("x",x2+12).attr("y",y(m[1])+4).attr("font-size","12px")
    .attr("fill",r[1]).text(r[2]+", average "+m[1].toFixed(3));
});
svg.append("text").attr("x",M.l).attr("y",M.t-14).attr("font-size","11.5px")
  .attr("fill","#555").text("Brier %s \\u2192 %s across %d markets");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The average market that ended up happening moved from %.3f to %.3f over those
three weeks; the average one that did not moved from %.3f to %.3f. Prices sort
themselves toward the answer as the answer approaches, which is what a forecast
is supposed to do — and it is also why grading a market at one moment
grades the moment as much as the market.</p>
', rows, mv[1, 1], mv[2, 1], mv[1, 2], mv[2, 2], p4(b30), p4(b7), sum(k30),
   mv[1, 2], mv[2, 2], mv[1, 1], mv[2, 1]))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
