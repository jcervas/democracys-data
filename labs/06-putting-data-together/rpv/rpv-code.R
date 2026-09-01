# rpv-code.R -- chunk bodies for rpv-brief.Rmd
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

p  <- read.csv("data/derived/houston_primary.csv", stringsAsFactors = FALSE)
jp <- read.csv("data/derived/join_peek.csv",        stringsAsFactors = FALSE)
jc <- read.csv("data/derived/join_counts.csv",      stringsAsFactors = FALSE)
p$pct_black <- p$black / p$voters
p$pct_dem   <- p$dem   / p$voters

fit <- lm(pct_dem ~ pct_black, data = p, weights = voters)
b   <- coef(fit); r2 <- summary(fit)$r.squared
est_white <- 100 * b[[1]]; est_black <- 100 * (b[[1]] + b[[2]])

lo <- pmax(0, (p$pct_dem - (1 - p$pct_black)) / p$pct_black)
hi <- pmin(1, p$pct_dem / p$pct_black)
bd_lo <- 100 * max(lo); bd_hi <- 100 * min(hi)
bd_who_lo <- p$precinct[which.max(lo)]
p$bd_lo <- 100 * lo; p$bd_hi <- 100 * hi

truth_black <- 100 * sum(p$black_dem) / sum(p$black)
truth_white <- 100 * sum(p$white_dem) / sum(p$white)

hb <- p[p$pct_black >= 0.80, ]; hw <- p[p$pct_black <= 0.20, ]
hw_dem <- 100 * sum(hw$dem) / sum(hw$voters)

p$true_blk <- 100 * p$black_dem / p$black
p$true_wht <- 100 * p$white_dem / p$white
p$fit_dem  <- 100 * fitted(fit)
p$act_dem  <- 100 * p$pct_dem
max_resid  <- max(abs(p$fit_dem - p$act_dem))
cor_blk    <- cor(p$pct_black, p$true_blk)

# the true group rates, against the variable the regression assumes they ignore
tb  <- lm(true_blk ~ pct_black, data = p, weights = black)
tw  <- lm(true_wht ~ pct_black, data = p, weights = white)
gx  <- seq(min(p$pct_black), max(p$pct_black), length.out = 40)
tby <- predict(tb, data.frame(pct_black = gx))
twy <- predict(tw, data.frame(pct_black = gx))

top <- p[which.max(p$pct_black), ]; bot <- p[which.min(p$pct_black), ]

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)

# ---- figure data ----------------------------------------------------------

# Refit the line seventeen times, leaving one precinct out each time. The
# figure this used to feed was cut in the 3rd-edition condense; the finding it
# carried -- that the estimate is a property of one polling place -- is
# reported in prose, so the arithmetic stays here rather than being typed.
loo <- vapply(seq_len(nrow(p)), function(i) {
  f <- lm(pct_dem ~ pct_black, data = p[-i, ], weights = voters)
  100 * sum(coef(f))
}, numeric(1))
names(loo) <- p$precinct
loo_i    <- order(loo)
loo_span <- diff(range(loo))
loo_out  <- sum(loo > 100)


# ---- one palette for this document ----------------------------------------
# Three figures survive the 3rd-edition condense, and each color below names
# exactly one thing across all of them: a precinct, the recorded truth, an
# estimator, or one of the two racial groups. Nothing here is a party colour,
# because no figure's categories are the parties.
PREC   <- "#5b6b7a"   # one precinct: the raw observations, in every figure
TRUTHC <- "#4d9221"   # the recorded truth: black_dem / white_dem, the count of
                      # each group who asked for a Democratic ballot. Georgia
                      # puts self-reported race on the registration form and the
                      # party of the ballot request in the public record, so this
                      # column is observed, not modeled. A handful of states
                      # make that possible; that is why this county is gradeable
GOODC  <- "#762a83"   # Goodman's ecological regression estimate
HOMC   <- "#e08214"   # the homogeneous-precinct route: its zone and its answer
BNDC   <- "#8c8c8c"   # what the bounds allow: a range, not an estimate
BNDF   <- "#eeeeee"   # the same range as a background wash
BLKC   <- "#01665e"   # Black voters
WHTC   <- "#8c510a"   # white voters

# The d3 twins are emitted as text, so the palette above is substituted into
# them by token just before they are printed. One definition, two formats.
PALTOK <- c(PREC_ = PREC, TRUTHC_ = TRUTHC, GOODC_ = GOODC, HOMC_ = HOMC,
            BNDC_ = BNDC, BNDF_ = BNDF, BLKC_ = BLKC, WHTC_ = WHTC)
pal  <- function(x) {
  for (k in names(PALTOK)) x <- gsub(k, PALTOK[[k]], x, fixed = TRUE)
  x
}
pcat <- function(x) cat(pal(x))

# every static figure ends with this, so print carries what the screen carries
subcap <- function(txt, width = 92, line = 4.4, cex = 0.66) {
  cw <- strwrap(txt, width = width)
  mtext(cw, side = 1, line = line + (seq_along(cw) - 1) * 0.95, adj = 0,
        cex = cex, col = "#555555")
}

# ---- one caption per figure, written once, printed in BOTH formats ---------
# Each of these lived only inside its d3 chunk, so the PDF, which is the format
# students print, was carrying its figures with no caption at all.
cap_good <- paste(
  "The two large purple points are the estimates. The right-hand one sits in",
  "the gray band, where the county contains no precincts at all.")
cap_score <- paste(
  "Every estimate for both groups on one axis, against the recorded truth.",
  "The regression's Black estimate sits in the gray zone past 100%.")
cap_truth <- paste(
  "Circles: how Black voters in each precinct actually voted. Triangles: how",
  "white voters in the same precinct actually voted. The regression assumes",
  "each is one number (the dashed lines); both slope upward with the x-axis",
  "instead.")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- joincounts
jc

## ---- joinpeek
jp

## ---- file
data.frame(
  quantity = c("Precincts", "Primary voters", "Black voters", "white voters",
               "Democratic ballots requested", "Republican ballots requested",
               "Least Black precinct", "Most Black precinct"),
  value = c(nrow(p), n(sum(p$voters)), n(sum(p$black)), n(sum(p$white)),
            n(sum(p$dem)), n(sum(p$rep)),
            paste0(bot$precinct, " (", pc(100 * bot$pct_black), "% Black)"),
            paste0(top$precinct, " (", pc(100 * top$pct_black), "% Black)")))

## ---- good-static
par(mar = c(7.6, 4.4, 1.6, 1.4))
plot(p$pct_black, p$pct_dem, pch = 19, cex = sqrt(p$voters) / 22,
     col = PREC, xlim = c(0, 1), ylim = c(0, 1.12),
     xlab = "share of the precinct's primary voters who are Black",
     ylab = "share of ballots that were Democratic")
rect(max(p$pct_black), -1, 1.2, 2, col = adjustcolor("grey70", alpha.f = 0.25),
     border = NA)
abline(fit, col = GOODC, lwd = 2)
abline(h = 1, lty = 2, col = "grey40")
points(c(0, 1), c(b[[1]], b[[1]] + b[[2]]), pch = 21, bg = GOODC, cex = 1.6)
text(0.88, 0.42, "no data\nout here", col = "grey30", cex = 0.9)
text(1, 1.06, pc(est_black, 1), col = GOODC, cex = 0.95, pos = 2)
subcap(cap_good)

## ---- good-d3
# The first html figure in the document, so this chunk carries the one d3
# <script src> the page needs; the two hand-written figures below ride on it.
# It also publishes the precinct rows on window.__rpv, which Figure 3 reads.
rows <- paste(sprintf('{"p":"%s","x":%.4f,"y":%.4f,"v":%d,"t":%.1f}',
                      gsub('"', "", p$precinct), p$pct_black, p$pct_dem,
                      p$voters, p$true_blk), collapse = ",")
pcat(sprintf('
<div id="gd" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
window.__rpv=[%s];
(function(){
const D=window.__rpv, B0=%.5f, B1=%.5f, MX=%.4f;
const W=740,H=440,M={t:24,r:22,b:52,l:62};
const svg=d3.select("#gd").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,1]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1.12]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(MX)).attr("y",M.t).attr("width",x(1)-x(MX))
  .attr("height",H-M.b-M.t).attr("fill","#999").attr("fill-opacity",0.16);
svg.append("text").attr("x",(x(MX)+x(1))/2).attr("y",y(0.42))
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#555")
  .text("no data out here");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d3.format(".0%%")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d3.format(".0%%")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("share of the precinct\\u2019s primary voters who are Black");
svg.append("line").attr("x1",x(0)).attr("x2",x(1)).attr("y1",y(1)).attr("y2",y(1))
  .attr("stroke","#666").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(0.03)).attr("y",y(1)-6).attr("font-size","11.5px")
  .attr("fill","#666").text("100%% \\u2014 the ceiling of possibility");
svg.append("line").attr("x1",x(0)).attr("y1",y(B0)).attr("x2",x(1)).attr("y2",y(B0+B1))
  .attr("stroke","GOODC_").attr("stroke-width",2.4);
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",d=>Math.sqrt(d.v)/5)
  .attr("fill","PREC_").attr("fill-opacity",0.62);
[[0,B0],[1,B0+B1]].forEach(pt=>{
  svg.append("circle").attr("cx",x(pt[0])).attr("cy",y(pt[1])).attr("r",7)
    .attr("fill","GOODC_").attr("stroke","#fff").attr("stroke-width",2);
  svg.append("text").attr("x",x(pt[0])+(pt[0]?-12:12)).attr("y",y(pt[1])-11)
    .attr("text-anchor",pt[0]?"end":"start").attr("font-size","13px")
    .attr("font-weight","600").attr("fill","GOODC_")
    .text((100*pt[1]).toFixed(1)+"%%");});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s</p>
', rows, b[[1]], b[[2]], max(p$pct_black), cap_good))

## ---- truth
data.frame(
  method = c("Homogeneous precincts", "Goodman's regression",
             "Bounds, lower", "Bounds, upper", "THE TRUTH"),
  black_voters = c("cannot be computed", paste0(pc(est_black), "%"),
                   paste0(pc(bd_lo), "%"), paste0(pc(bd_hi), "%"),
                   paste0(pc(truth_black), "%")),
  white_voters = c(paste0(pc(hw_dem), "%"), paste0(pc(est_white), "%"),
                   "", "", paste0(pc(truth_white), "%")))

## ---- score-static
par(mar = c(7.8, 7.2, 0.8, 1.2))
plot(NA, xlim = c(0, 112), ylim = c(0.45, 2.95), yaxt = "n", bty = "n", las = 1,
     xlab = "Democratic share of the group's ballots (%)", ylab = "")
rect(100, 0.4, 114, 2.35, col = "#f2f2f2", border = NA)
abline(v = 100, lty = 2, col = "grey40")
text(101, 0.6, "impossible", adj = c(0, 0), cex = 0.72, col = "grey35")
axis(2, at = 2:1, labels = c("Black voters", "white voters"), las = 1,
     tick = FALSE, cex.axis = 0.92)
segments(bd_lo, 2, bd_hi, 2, lwd = 9, col = BNDC, lend = 1)
segments(c(truth_black, truth_white), c(1.8, 0.8),
         c(truth_black, truth_white), c(2.2, 1.2), lwd = 3.4, col = TRUTHC)
points(c(est_black, est_white), c(2, 1), pch = 19, cex = 1.5, col = GOODC)
points(hw_dem, 1, pch = 17, cex = 1.4, col = HOMC)
text(truth_black, 2.24, pc(truth_black), pos = 3, cex = 0.75, col = TRUTHC,
     offset = 0.1)
text(truth_white, 1.24, pc(truth_white), pos = 3, cex = 0.75, col = TRUTHC,
     offset = 0.1)
text(est_black, 2, paste0(pc(est_black), " "), pos = 1, cex = 0.75, col = GOODC)
text(est_white, 1, pc(est_white), pos = 1, cex = 0.75, col = GOODC)
text(hw_dem, 1, pc(hw_dem), pos = 3, cex = 0.75, col = HOMC, offset = 0.5)
text(1, 2, "homogeneous precincts:\nno estimate exists", adj = c(0, 0.5),
     cex = 0.75, col = HOMC)
legend(20, 2.95, c("the truth", "Goodman's regression", "homogeneous precincts",
                   "what the bounds allow"),
       pch = c(NA, 19, 17, NA), lty = c(1, NA, NA, 1), lwd = c(3.4, NA, NA, 9),
       col = c(TRUTHC, GOODC, HOMC, BNDC), bty = "n",
       cex = 0.75, ncol = 2)
subcap(cap_score, width = 84)

## ---- score-d3
pcat(sprintf('
<div id="scr" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const TB=%.2f,TW=%.2f,GB=%.2f,GW=%.2f,HW=%.2f,LO=%.2f,HI=%.2f;
const W=740,H=340,M={t:64,r:24,b:50,l:110};
const svg=d3.select("#scr").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,112]).range([M.l,W-M.r]);
const yB=M.t+34,yW=H-M.b-34;
svg.append("rect").attr("x",x(100)).attr("y",M.t-10).attr("width",x(112)-x(100))
  .attr("height",H-M.b-M.t+10).attr("fill","#f0f0f0");
svg.append("text").attr("x",x(100)+5).attr("y",H-M.b-8).attr("font-size","11px")
  .attr("fill","#666").text("impossible");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the group\\u2019s ballots");
svg.append("line").attr("x1",x(100)).attr("x2",x(100)).attr("y1",M.t-10).attr("y2",H-M.b)
  .attr("stroke","#888").attr("stroke-dasharray","5,4");
[["Black voters",yB],["white voters",yW]].forEach(r=>{
  svg.append("text").attr("x",M.l-12).attr("y",r[1]+4).attr("text-anchor","end")
    .attr("font-size","12.5px").attr("fill","#222").text(r[0]);});
svg.append("line").attr("x1",x(LO)).attr("x2",x(HI)).attr("y1",yB).attr("y2",yB)
  .attr("stroke","BNDC_").attr("stroke-width",11);
[[TB,yB],[TW,yW]].forEach(t=>{
  svg.append("line").attr("x1",x(t[0])).attr("x2",x(t[0])).attr("y1",t[1]-16)
    .attr("y2",t[1]+16).attr("stroke","TRUTHC_").attr("stroke-width",3.4);
  svg.append("text").attr("x",x(t[0])).attr("y",t[1]-21).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("fill","TRUTHC_").text(t[0].toFixed(1)+"%%");});
[[GB,yB],[GW,yW]].forEach(t=>{
  svg.append("circle").attr("cx",x(t[0])).attr("cy",t[1]).attr("r",7)
    .attr("fill","GOODC_").attr("stroke","#fff").attr("stroke-width",1.5);
  svg.append("text").attr("x",x(t[0])).attr("y",t[1]+22).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("fill","GOODC_").text(t[0].toFixed(1)+"%%");});
svg.append("path").attr("d",d3.symbol().type(d3.symbolTriangle).size(90)())
  .attr("transform",`translate(${x(HW)},${yW})`).attr("fill","HOMC_");
svg.append("text").attr("x",x(HW)).attr("y",yW-14).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","HOMC_").text(HW.toFixed(1)+"%%");
svg.append("text").attr("x",M.l+4).attr("y",yB+4).attr("font-size","11.5px")
  .attr("fill","HOMC_").text("homogeneous precincts: no estimate exists");
const key=[["the truth","TRUTHC_"],["Goodman\\u2019s regression","GOODC_"],
           ["homogeneous precincts","HOMC_"],["what the bounds allow","BNDC_"]];
key.forEach((k,i)=>{
  const kx=M.l+(i%%2)*270,ky=16+Math.floor(i/2)*18;
  svg.append("rect").attr("x",kx).attr("y",ky-8).attr("width",16).attr("height",8)
    .attr("fill",k[1]);
  svg.append("text").attr("x",kx+22).attr("y",ky).attr("font-size","11.5px")
    .attr("fill","#333").text(k[0]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s</p>
', truth_black, truth_white, est_black, est_white, hw_dem, bd_lo, bd_hi,
   cap_score))

## ---- truth-static
par(mar = c(7.8, 4.6, 0.8, 1))
plot(100 * p$pct_black, p$true_blk, pch = 19, col = BLKC,
     cex = sqrt(p$voters) / 24, xlim = c(5, 86), ylim = c(0, 108), las = 1,
     xlab = "precinct's Black share of primary voters (%)",
     ylab = "Democratic share of the group's ballots (%)")
points(100 * p$pct_black, p$true_wht, pch = 17, col = WHTC,
       cex = sqrt(p$voters) / 24)
lines(100 * gx, tby, col = BLKC, lwd = 2)
lines(100 * gx, twy, col = WHTC, lwd = 2)
abline(h = c(est_black, est_white), lty = 2, col = GOODC, lwd = 1.8)
text(5, est_black + 3.5, paste0("what the regression assumes for Black voters: ",
     pc(est_black), "% in every precinct"), adj = c(0, 0), cex = 0.72,
     col = GOODC)
text(5, est_white - 3.5, paste0("and for white voters: ", pc(est_white),
     "% in every precinct"), adj = c(0, 1), cex = 0.72, col = GOODC)
text(86, p$true_blk[which.max(p$pct_black)] - 7, "Black voters, actual", pos = 2,
     cex = 0.78, col = BLKC)
text(86, p$true_wht[which.max(p$pct_black)] + 7, "white voters, actual", pos = 2,
     cex = 0.78, col = WHTC)
text(44, 55, paste0("both rise with the x-axis:\nBlack ", pc(min(p$true_blk)),
     "% to ", pc(max(p$true_blk)), "%, white ", pc(min(p$true_wht)), "% to ",
     pc(max(p$true_wht)), "%"), adj = c(0.5, 0.5), cex = 0.75, col = "#555")
subcap(cap_truth)

## ---- truth-d3
rows <- paste(sprintf('{"p":"%s","x":%.2f,"b":%.2f,"w":%.2f,"v":%d}',
                      gsub('"', "", p$precinct), 100 * p$pct_black, p$true_blk,
                      p$true_wht, p$voters), collapse = ",")
ltb <- paste(sprintf('[%.2f,%.2f]', 100 * gx, tby), collapse = ",")
ltw <- paste(sprintf('[%.2f,%.2f]', 100 * gx, twy), collapse = ",")
pcat(sprintf('
<div id="tru" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],LB=[%s],LW=[%s],GB=%.2f,GW=%.2f;
const W=740,H=440,M={t:18,r:24,b:52,l:62};
const box=d3.select("#tru");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([5,86]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,108]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("precinct\\u2019s Black share of primary voters");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the group\\u2019s ballots");
[[GB,"what the regression assumes for Black voters: "],
 [GW,"and for white voters: "]].forEach(g=>{
  svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(g[0])).attr("y2",y(g[0]))
    .attr("stroke","GOODC_").attr("stroke-dasharray","6,4").attr("stroke-width",1.8);
  svg.append("text").attr("x",M.l+6).attr("y",y(g[0])-6).attr("font-size","11.5px")
    .attr("fill","GOODC_").text(g[1]+g[0].toFixed(1)+"%% in every precinct");});
const ln=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
svg.append("path").attr("d",ln(LB)).attr("fill","none").attr("stroke","BLKC_")
  .attr("stroke-width",2);
svg.append("path").attr("d",ln(LW)).attr("fill","none").attr("stroke","WHTC_")
  .attr("stroke-width",2);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
function tips(sel){sel.on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.p}</b><br>${d.x.toFixed(1)}%% Black<br>`+
     `Black voters ${d.b.toFixed(1)}%% Democratic<br>`+
     `white voters ${d.w.toFixed(1)}%% Democratic`)
     .style("left",Math.min(e.offsetX+14,W-280)+"px").style("top",(e.offsetY-10)+"px");})
   .on("mouseleave",()=>tip.style("opacity",0));}
tips(svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.b)).attr("r",d=>Math.sqrt(d.v)/6)
  .attr("fill","BLKC_").attr("fill-opacity",0.75));
tips(svg.append("g").selectAll("path.t").data(D).join("path")
  .attr("d",d=>d3.symbol().type(d3.symbolTriangle).size(d.v/12)())
  .attr("transform",d=>`translate(${x(d.x)},${y(d.w)})`)
  .attr("fill","WHTC_").attr("fill-opacity",0.75));
const last=D[d3.maxIndex(D,d=>d.x)];
svg.append("text").attr("x",x(85)).attr("y",y(last.b)-14).attr("text-anchor","end")
  .attr("font-size","12px").attr("fill","BLKC_").text("Black voters, actual");
svg.append("text").attr("x",x(85)).attr("y",y(last.w)+20).attr("text-anchor","end")
  .attr("font-size","12px").attr("fill","WHTC_").text("white voters, actual");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s</p>
', rows, ltb, ltw, est_black, est_white, cap_truth))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#gd text[fill="#555" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: the on-mark chunk above pins #gd dark for the dark page, so
# a --paper stroke there would sit dark behind a dark ink, and the checker
# scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#gd text[fill="#555" i],
#tru text[fill="#762a83" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
