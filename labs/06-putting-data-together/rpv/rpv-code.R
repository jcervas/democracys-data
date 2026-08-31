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
uninf <- sum(p$bd_lo <= 0 & p$bd_hi >= 100)   # precincts that rule out nothing

truth_black <- 100 * sum(p$black_dem) / sum(p$black)
truth_white <- 100 * sum(p$white_dem) / sum(p$white)

hb <- p[p$pct_black >= 0.80, ]; hw <- p[p$pct_black <= 0.20, ]
hw_dem <- 100 * sum(hw$dem) / sum(hw$voters)

thr <- do.call(rbind, lapply(c(.80, .70, .60, .50), function(t) {
  h <- p[p$pct_black >= t, ]
  data.frame(threshold = paste0(round(100 * t), "% or more Black"),
             precincts = nrow(h),
             estimate = if (nrow(h)) 100 * sum(h$dem) / sum(h$voters) else NA,
             error = if (nrow(h)) 100 * sum(h$dem) / sum(h$voters) - truth_black
                     else NA)
}))

p2 <- p[p$precinct != "WELLSTON CENTER", ]
f2 <- lm(pct_dem ~ pct_black, data = p2, weights = voters); b2 <- coef(f2)

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

# where on the x-axis the county's precincts actually sit
dbrk <- seq(0, 100, by = 10)
dcnt <- as.integer(table(cut(100 * p$pct_black, breaks = dbrk,
                            include.lowest = TRUE)))
dmid <- dbrk[-1] - 5
d_empty <- sum(dcnt[dmid > 50] == 0)          # empty decades above 50%

# every Black and white primary voter, from race to ballot
sk_bd <- sum(p$black_dem); sk_br <- sum(p$black) - sk_bd
sk_wd <- sum(p$white_dem); sk_wr <- sum(p$white) - sk_wd
sk_bl <- sk_bd + sk_br;    sk_wh <- sk_wd + sk_wr
sk_dm <- sk_bd + sk_wd;    sk_rp <- sk_br + sk_wr
sk_tot <- sk_bl + sk_wh

# refit the line seventeen times, leaving one precinct out each time
loo <- vapply(seq_len(nrow(p)), function(i) {
  f <- lm(pct_dem ~ pct_black, data = p[-i, ], weights = voters)
  100 * sum(coef(f))
}, numeric(1))
names(loo) <- p$precinct
loo_i    <- order(loo)
loo_span <- diff(range(loo))
loo_out  <- sum(loo > 100)


# ---- one palette for this document ----------------------------------------
# Eight figures, and before this block a single color could mean the
# homogeneity zone in one of them, an estimator in the next and a Republican
# ballot in the third; blue meant "a Democratic ballot" in one figure and
# "Black voters" a paragraph later. Each color below now names exactly one
# thing, and the only place red and blue appear is the one figure whose
# categories really are the two parties.
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
DEMC   <- "#2c7fb8"   # a Democratic ballot
REPC   <- "#C41230"   # a Republican ballot

# The d3 twins are emitted as text, so the palette above is substituted into
# them by token just before they are printed. One definition, two formats.
PALTOK <- c(PREC_ = PREC, TRUTHC_ = TRUTHC, GOODC_ = GOODC, HOMC_ = HOMC,
            BNDC_ = BNDC, BNDF_ = BNDF, BLKC_ = BLKC, WHTC_ = WHTC,
            DEMC_ = DEMC, REPC_ = REPC)
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
# Every one of these lived only inside the d3 chunk, so the PDF, which is the
# format students print, was carrying eight figures with no caption at all.
cap_good <- paste(
  "The two large purple points are the estimates. The right-hand one sits in",
  "the gray band, where the county contains no precincts at all.")
cap_score <- paste(
  "Every estimate for both groups on one axis, against the recorded truth.",
  "The regression's Black estimate sits in the gray zone past 100%.")
cap_bounds <- sprintf(paste(
  "One bar per precinct: the values of Black Democratic support that",
  "precinct's own totals still permit. %d of the %d precincts rule nothing out",
  "at all, and the shaded strip is what is left after every one of them."),
  uninf, nrow(p))
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

## ---- cleanprecinct
p[p$precinct %in% c("WELLSTON CENTER", "ROZR"),
  c("precinct", "voters", "black", "white", "dem", "rep")]

## ---- one-row
o <- p[p$precinct == "WELLSTON CENTER",
       c("precinct", "voters", "black", "white", "dem", "rep")]
names(o) <- c("precinct", "primary voters", "Black", "white",
              "Democratic ballots", "Republican ballots")
o

## ---- file
data.frame(
  quantity = c("Precincts", "Primary voters", "Black voters", "white voters",
               "Democratic ballots requested", "Republican ballots requested",
               "Least Black precinct", "Most Black precinct"),
  value = c(nrow(p), n(sum(p$voters)), n(sum(p$black)), n(sum(p$white)),
            n(sum(p$dem)), n(sum(p$rep)),
            paste0(bot$precinct, " (", pc(100 * bot$pct_black), "% Black)"),
            paste0(top$precinct, " (", pc(100 * top$pct_black), "% Black)")))

## ---- scatter-static
par(mar = c(7.6, 4.4, 1.6, 1.4))
plot(p$pct_black, p$pct_dem, pch = 19, cex = sqrt(p$voters) / 22,
     col = PREC, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "share of the precinct's primary voters who are Black",
     ylab = "share of ballots that were Democratic")
abline(0, 1, lty = 3, col = "grey60"); grid()

## ---- scatter-d3
rows <- paste(sprintf('{"p":"%s","x":%.4f,"y":%.4f,"v":%d,"t":%.1f}',
                      gsub('"', "", p$precinct), p$pct_black, p$pct_dem,
                      p$voters, p$true_blk), collapse = ",")
pcat(sprintf('
<div id="sc" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
window.__rpv=[%s];
(function(){
const D=window.__rpv;
const W=740,H=430,M={t:18,r:22,b:52,l:62};
const svg=d3.select("#sc").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,1]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d3.format(".0%%")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d3.format(".0%%")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("share of the precinct\\u2019s primary voters who are Black");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("share of ballots that were Democratic");
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(1)).attr("y2",y(1))
  .attr("stroke","#aaa").attr("stroke-dasharray","4,4");
const tip=d3.select("#sc").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",d=>Math.sqrt(d.v)/5)
  .attr("fill","PREC_").attr("fill-opacity",0.65)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.p}</b><br>${d3.format(",")(d.v)} voters<br>`+
     `${(100*d.x).toFixed(1)}%% Black &middot; ${(100*d.y).toFixed(1)}%% Democratic`)
     .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', rows))

## ---- homog
data.frame(
  quantity = c("Precincts at least 80% Black",
               "Precincts at most 20% Black",
               "Democratic share in those white-homogeneous precincts"),
  value = c(nrow(hb), nrow(hw), paste0(pc(hw_dem), "%")))

## ---- decade-static
par(mar = c(8.0, 4.6, 1.6, 1.4))
bp <- barplot(dcnt, space = 0, col = PREC,
              border = "white", las = 1, ylim = c(-1.4, max(dcnt) + 1.2),
              ylab = "precincts", xlab = "precinct's Black share of primary voters (%)")
axis(1, at = seq(0, 10, by = 1), labels = seq(0, 100, by = 10))
rect(8, -1.4, 10, max(dcnt) + 1.2, col = adjustcolor(HOMC, alpha.f = 0.12),
     border = NA)
abline(v = 8, lty = 2, col = HOMC, lwd = 1.8)
text(7.85, max(dcnt) * 0.92, "the 80% homogeneity\nthreshold", pos = 2,
     cex = 0.76, col = HOMC)
text(9, max(dcnt) * 0.45, "no\nprecincts", cex = 0.78, col = HOMC)
points(100 * p$pct_black / 10, rep(-0.75, nrow(p)), pch = "|", cex = 0.8,
       col = PREC)
text(0.1, max(dcnt) + 1.0, paste0("one tick per precinct, all ", nrow(p),
     " of them"), adj = c(0, 1), cex = 0.72, col = "#555")

## ---- decade-d3
rows <- paste(sprintf('{"m":%.0f,"c":%d}', dmid, dcnt), collapse = ",")
rug  <- paste(sprintf('{"p":"%s","x":%.2f,"v":%d}', gsub('"', "", p$precinct),
                      100 * p$pct_black, p$voters), collapse = ",")
pcat(sprintf('
<div id="dec" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], R=[%s];
const W=740,H=350,M={t:24,r:22,b:66,l:56};
const box=d3.select("#dec");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.c)+1]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(80)).attr("y",M.t).attr("width",x(100)-x(80))
  .attr("height",H-M.b-M.t).attr("fill","HOMC_").attr("fill-opacity",0.12);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(4));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("precinct\\u2019s Black share of primary voters");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("precincts");
const bw=x(10)-x(0);
svg.append("g").selectAll("rect.b").data(D).join("rect")
  .attr("x",d=>x(d.m-5)+0.8).attr("width",bw-1.6)
  .attr("fill",()=>"PREC_")
  .attr("y",y(0)).attr("height",0)
  .transition().duration(650).delay((d,i)=>i*35)
  .attr("y",d=>y(d.c)).attr("height",d=>y(0)-y(d.c));
svg.append("line").attr("x1",x(80)).attr("x2",x(80)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","HOMC_").attr("stroke-dasharray","5,4").attr("stroke-width",1.8);
svg.append("text").attr("x",x(79)).attr("y",M.t+14).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","HOMC_")
  .text("the 80%% homogeneity threshold");
svg.append("text").attr("x",x(90)).attr("y",y(0)-60).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","HOMC_").text("no precincts");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("line.r").data(R).join("line")
  .attr("x1",d=>x(d.x)).attr("x2",d=>x(d.x)).attr("y1",H-M.b+2).attr("y2",H-M.b+13)
  .attr("stroke","PREC_").attr("stroke-width",2)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.p}</b><br>${d.x.toFixed(1)}%% Black<br>${d3.format(",")(d.v)} voters`)
     .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-40)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', rows, rug))

## ---- goodman
data.frame(
  quantity = c("R-squared", "Estimated Democratic support, white voters",
               "Estimated Democratic support, Black voters"),
  value = c(pc(r2, 3), paste0(pc(est_white), "%"), paste0(pc(est_black), "%")))

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
pcat(sprintf('
<div id="gd" style="position:relative;margin:1em 0"></div>
<script>
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
', b[[1]], b[[2]], max(p$pct_black), cap_good))

## ---- bounds
data.frame(
  quantity = c("Lower bound on Black Democratic support",
               "Upper bound", "Width of the interval",
               "Which precinct produces the binding lower bound"),
  value = c(paste0(pc(bd_lo), "%"), paste0(pc(bd_hi), "%"),
            paste0(pc(bd_hi - bd_lo), " points"), bd_who_lo))

## ---- bounds-static
par(mar = c(7.8, 4.6, 0.8, 1))
o <- p[order(p$pct_black), ]
plot(NA, xlim = c(0, 100), ylim = c(5, 84), las = 1,
     xlab = "values of Black Democratic support this precinct still allows (%)",
     ylab = "precinct's Black share of primary voters (%)")
rect(bd_lo, 0, bd_hi, 95, col = BNDF, border = NA)
abline(v = bd_lo, lty = 2, col = BNDC, lwd = 1.8)
segments(o$bd_lo, 100 * o$pct_black, o$bd_hi, 100 * o$pct_black, lwd = 2.6,
         col = PREC, lend = 1)
points(o$bd_lo, 100 * o$pct_black, pch = 19, cex = 0.65, col = PREC)
i <- which(o$bd_lo > 0)
text(o$bd_lo[i], 100 * o$pct_black[i], paste0(o$precinct[i], " "), pos = 2,
     cex = 0.7, col = "#333")
text(bd_lo + 2, 66, paste0("survives every\nprecinct: ", pc(bd_lo), "%\nto ",
     pc(bd_hi), "%"), adj = c(0, 1), cex = 0.75, col = BNDC)
text(2, 62, paste0(uninf, " of ", nrow(p), " precincts allow\nanything from 0% to 100%"),
     adj = c(0, 0.5), cex = 0.75, col = "#555")
box()
subcap(cap_bounds)

## ---- bounds-d3
o <- p[order(p$pct_black), ]
rows <- paste(sprintf('{"p":"%s","b":%.2f,"lo":%.2f,"hi":%.2f}',
                      gsub('"', "", o$precinct), 100 * o$pct_black,
                      o$bd_lo, o$bd_hi), collapse = ",")
pcat(sprintf('
<div id="bnd" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],LO=%.2f,HI=%.2f;
const W=740,H=420,M={t:18,r:24,b:52,l:62};
const box=d3.select("#bnd");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([5,84]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(LO)).attr("y",M.t).attr("width",x(HI)-x(LO))
  .attr("height",H-M.b-M.t).attr("fill","BNDF_");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("values of Black Democratic support this precinct still allows");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("precinct\\u2019s Black share of primary voters");
svg.append("line").attr("x1",x(LO)).attr("x2",x(LO)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","BNDC_").attr("stroke-dasharray","5,4").attr("stroke-width",1.8);
svg.append("text").attr("x",x(LO)+6).attr("y",y(70)).attr("font-size","11.5px")
  .attr("fill","BNDC_").text("survives every precinct: "+LO.toFixed(1)+"%%\\u2013"+HI.toFixed(1)+"%%");
svg.append("text").attr("x",x(2)).attr("y",y(62)).attr("font-size","11.5px")
  .attr("fill","#555").text("%d of %d precincts allow anything from 0%% to 100%%");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("line.b").data(D).join("line")
  .attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi)).attr("y1",d=>y(d.b)).attr("y2",d=>y(d.b))
  .attr("stroke","PREC_").attr("stroke-width",3).attr("stroke-linecap","butt")
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.p}</b><br>${d.b.toFixed(1)}%% Black<br>`+
     `allows ${d.lo.toFixed(1)}%%\\u2013${d.hi.toFixed(1)}%%`)
     .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.lo)).attr("cy",d=>y(d.b)).attr("r",3).attr("fill","PREC_");
svg.append("g").selectAll("text.n").data(D.filter(d=>d.lo>0)).join("text")
  .attr("x",d=>x(d.lo)-6).attr("y",d=>y(d.b)+4).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#333").text(d=>d.p);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s</p>
', rows, bd_lo, bd_hi, uninf, nrow(p), cap_bounds))

## ---- truth
data.frame(
  method = c("Homogeneous precincts", "Goodman's regression",
             "Bounds, lower", "Bounds, upper", "THE TRUTH"),
  black_voters = c("cannot be computed", paste0(pc(est_black), "%"),
                   paste0(pc(bd_lo), "%"), paste0(pc(bd_hi), "%"),
                   paste0(pc(truth_black), "%")),
  white_voters = c(paste0(pc(hw_dem), "%"), paste0(pc(est_white), "%"),
                   "", "", paste0(pc(truth_white), "%")))

## ---- sankey-static
par(mar = c(7.6, 5.2, 0.8, 6.4))
plot(NA, xlim = c(0, 1), ylim = c(1.03, -0.06), axes = FALSE,
     xlab = "", ylab = "")
gp <- 0.05
hL <- c(sk_bl, sk_wh) / sk_tot * (1 - gp); hR <- c(sk_dm, sk_rp) / sk_tot * (1 - gp)
yL <- c(0, hL[1] + gp); yR <- c(0, hR[1] + gp)
rib <- function(x0, x1, a0, a1, w0, w1, col) {
  t <- seq(0, 1, length.out = 80); s <- 0.5 - 0.5 * cos(pi * t)
  polygon(c(x0 + (x1 - x0) * t, rev(x0 + (x1 - x0) * t)),
          c(a0 + (a1 - a0) * s, rev(a0 + w0 + (a1 + w1 - a0 - w0) * s)),
          col = col, border = NA)
}
oL <- yL; oR <- yR
for (k in 1:4) {
  i <- (k - 1) %/% 2 + 1; j <- (k - 1) %% 2 + 1
  vv <- c(sk_bd, sk_br, sk_wd, sk_wr)[k] / sk_tot * (1 - gp)
  rib(0.10, 0.90, oL[i], oR[j], vv, vv,
      adjustcolor(c(BLKC, WHTC)[i], alpha.f = 0.42))
  oL[i] <- oL[i] + vv; oR[j] <- oR[j] + vv
}
rect(0.03, yL, 0.10, yL + hL, col = c(BLKC, WHTC), border = NA)
rect(0.90, yR, 0.97, yR + hR, col = c(DEMC, REPC), border = NA)
text(0.01, yL + hL / 2, paste0(c("Black\n", "white\n"), n(c(sk_bl, sk_wh))),
     pos = 2, cex = 0.82, xpd = NA)
text(0.99, yR + hR / 2, paste0(c("Democratic\nballot\n", "Republican\nballot\n"),
     n(c(sk_dm, sk_rp))), pos = 4, cex = 0.82, xpd = NA)
text(0.5, c(sk_bd / 2, sk_bd + sk_br / 2) / sk_tot * (1 - gp),
     paste0(pc(100 * c(sk_bd, sk_br) / sk_bl), "%"), cex = 0.78, col = BLKC)
text(0.5, yL[2] + c(sk_wd / 2, sk_wd + sk_wr / 2) / sk_tot * (1 - gp),
     paste0(pc(100 * c(sk_wd, sk_wr) / sk_wh), "%"), cex = 0.78, col = "#444")

## ---- sankey-d3
pcat(sprintf('
<div id="snk" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const F=[{s:0,t:0,v:%d},{s:0,t:1,v:%d},{s:1,t:0,v:%d},{s:1,t:1,v:%d}];
const SL=[{n:"Black voters",v:%d,c:"BLKC_"},{n:"white voters",v:%d,c:"WHTC_"}];
const TL=[{n:"Democratic ballot",v:%d,c:"DEMC_"},{n:"Republican ballot",v:%d,c:"REPC_"}];
const TOT=%d;
const W=740,H=400,M={t:18,r:150,b:22,l:130};
const box=d3.select("#snk");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const IH=H-M.t-M.b, GAP=26, sc=(IH-GAP)/TOT;
const x0=M.l,x1=W-M.r,bw=15;
function lay(N){let a=M.t,o=[];N.forEach(d=>{o.push({y:a,h:d.v*sc});a+=d.v*sc+GAP;});return o;}
const L=lay(SL), T=lay(TL);
const offL=L.map(d=>d.y), offT=T.map(d=>d.y);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
F.forEach(f=>{
  const h=f.v*sc, ya=offL[f.s], yb=offT[f.t];
  offL[f.s]+=h; offT[f.t]+=h;
  const xm=(x0+bw+x1)/2;
  const d=`M${x0+bw},${ya} C${xm},${ya} ${xm},${yb} ${x1},${yb} `+
          `L${x1},${yb+h} C${xm},${yb+h} ${xm},${ya+h} ${x0+bw},${ya+h} Z`;
  svg.append("path").attr("d",d).attr("fill",SL[f.s].c).attr("fill-opacity",0.4)
    .on("mousemove",function(e){tip.style("opacity",1).html(
       `<b>${SL[f.s].n} \\u2192 ${TL[f.t].n}</b><br>`+
       `${d3.format(",")(f.v)} voters<br>`+
       `${(100*f.v/SL[f.s].v).toFixed(1)}%% of ${SL[f.s].n}`)
       .style("left",Math.min(e.offsetX+14,W-330)+"px").style("top",(e.offsetY-10)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0));
  svg.append("text").attr("x",xm).attr("y",(ya+yb)/2+h/2+4).attr("text-anchor","middle")
    .attr("font-size","12px").attr("font-weight","600").attr("fill","#333")
    .text((100*f.v/SL[f.s].v).toFixed(1)+"%%");
});
SL.forEach((d,i)=>{
  svg.append("rect").attr("x",x0).attr("y",L[i].y).attr("width",bw)
    .attr("height",L[i].h).attr("fill",d.c);
  svg.append("text").attr("x",x0-10).attr("y",L[i].y+L[i].h/2-2).attr("text-anchor","end")
    .attr("font-size","12.5px").attr("fill","#222").text(d.n);
  svg.append("text").attr("x",x0-10).attr("y",L[i].y+L[i].h/2+14).attr("text-anchor","end")
    .attr("font-size","11.5px").attr("fill","#666").text(d3.format(",")(d.v));});
TL.forEach((d,i)=>{
  svg.append("rect").attr("x",x1).attr("y",T[i].y).attr("width",bw)
    .attr("height",T[i].h).attr("fill",d.c);
  svg.append("text").attr("x",x1+bw+10).attr("y",T[i].y+T[i].h/2-2)
    .attr("font-size","12.5px").attr("fill","#222").text(d.n);
  svg.append("text").attr("x",x1+bw+10).attr("y",T[i].y+T[i].h/2+14)
    .attr("font-size","11.5px").attr("fill","#666").text(d3.format(",")(d.v));});
})();
</script>
', sk_bd, sk_br, sk_wd, sk_wr, sk_bl, sk_wh, sk_dm, sk_rp, sk_tot))

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

## ---- mechanism
o <- p[order(p$pct_black),
       c("precinct", "pct_black", "act_dem", "fit_dem", "true_blk", "true_wht")]
o$pct_black <- pc(100 * o$pct_black); o$act_dem <- pc(o$act_dem)
o$fit_dem <- pc(o$fit_dem); o$true_blk <- pc(o$true_blk)
o$true_wht <- pc(o$true_wht)
names(o) <- c("precinct", "% Black", "% Dem, actual", "% Dem, predicted",
              "Black Dem rate (true)", "white Dem rate (true)")
o

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

## ---- drop
data.frame(
  fit = c("All 17 precincts", paste("Without", top$precinct)),
  black_estimate = paste0(pc(c(est_black, 100 * (b2[[1]] + b2[[2]]))), "%"),
  white_estimate = paste0(pc(c(est_white, 100 * b2[[1]])), "%"),
  r_squared = pc(c(r2, summary(f2)$r.squared), 3),
  most_black_precinct = paste0(pc(100 * c(max(p$pct_black),
                                          max(p2$pct_black))), "%"))

## ---- loo-static
par(mar = c(8.0, 12.6, 0.8, 1.6))
plot(NA, xlim = range(c(loo, truth_black, 100)) + c(-4, 4),
     ylim = c(0.4, nrow(p) + 0.6), yaxt = "n", bty = "n",
     xlab = "Black Democratic support the refitted line reports (%)", ylab = "")
axis(2, at = seq_len(nrow(p)), labels = names(loo)[loo_i], las = 1,
     tick = FALSE, cex.axis = 0.62)
abline(v = 100, lty = 2, col = "grey40")
abline(v = truth_black, col = TRUTHC, lwd = 2.4)
abline(v = est_black, lty = 3, col = GOODC, lwd = 1.8)
segments(est_black, seq_len(nrow(p)), loo[loo_i], seq_len(nrow(p)),
         col = "grey80", lwd = 1.4)
points(loo[loo_i], seq_len(nrow(p)), pch = 19, cex = 1.05,
       col = PREC)
text(truth_black, nrow(p) + 0.55, paste0("truth ", pc(truth_black), "%"),
     pos = 2, cex = 0.72, col = TRUTHC)
text(100, nrow(p) + 0.55, "100%", pos = 4, cex = 0.72, col = "grey35")
text(est_black, 0.55, paste0("all ", nrow(p), ": ", pc(est_black), "%"), pos = 4,
     cex = 0.72, col = GOODC)

## ---- loo-d3
rows <- paste(sprintf('{"p":"%s","v":%.2f,"b":%.2f}',
                      gsub('"', "", names(loo)[loo_i]), loo[loo_i],
                      100 * p$pct_black[loo_i]), collapse = ",")
pcat(sprintf('
<div id="loo" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], FULL=%.2f, TRUTH=%.2f, LO=%.2f, HI=%.2f;
const W=740,H=480,M={t:26,r:26,b:52,l:158};
const box=d3.select("#loo");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([LO,HI]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.p)).range([H-M.b,M.t]).padding(0.34);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0))
  .selectAll("text").attr("font-size","10.5px");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Black Democratic support the refitted line reports");
svg.append("rect").attr("x",x(100)).attr("y",M.t).attr("width",Math.max(0,x(HI)-x(100)))
  .attr("height",H-M.b-M.t).attr("fill","#999").attr("fill-opacity",0.14);
[[100,"#888","5,4","100%%"],[TRUTH,"TRUTHC_",null,"truth "+TRUTH.toFixed(1)+"%%"],
 [FULL,"GOODC_","2,3","all precincts "+FULL.toFixed(1)+"%%"]].forEach(g=>{
  const l=svg.append("line").attr("x1",x(g[0])).attr("x2",x(g[0]))
    .attr("y1",M.t-8).attr("y2",H-M.b).attr("stroke",g[1]).attr("stroke-width",2);
  if(g[2]) l.attr("stroke-dasharray",g[2]);
  svg.append("text").attr("x",x(g[0])).attr("y",M.t-13).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill",g[1]).text(g[3]);});
const yc=d=>y(d.p)+y.bandwidth()/2;
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("line.c").data(D).join("line")
  .attr("x1",x(FULL)).attr("x2",d=>x(d.v)).attr("y1",yc).attr("y2",yc)
  .attr("stroke","#ccc").attr("stroke-width",1.4);
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.v)).attr("cy",yc).attr("r",5)
  .attr("fill",()=>"PREC_")
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>drop ${d.p}</b> (${d.b.toFixed(1)}%% Black)<br>`+
     `estimate becomes ${d.v.toFixed(1)}%%<br>`+
     `${(d.v-FULL>0?"+":"")}${(d.v-FULL).toFixed(1)} against the full fit`)
     .style("left",Math.min(e.offsetX+14,W-320)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', rows, est_black, truth_black,
   min(c(loo, truth_black)) - 4, max(c(loo, 100)) + 4))

## ---- thresholds
o <- thr
o$estimate <- ifelse(is.na(o$estimate), "cannot be computed",
                     paste0(pc(o$estimate), "%"))
o$error <- ifelse(is.na(o$error), "", sprintf("%+.1f", o$error))
names(o) <- c("precincts defined as homogeneous", "how many qualify",
              "Black Democratic estimate", "error against the truth")
o

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#gd text[fill="#555" i],
#snk text[fill="#333" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe. The #dec
# label carries its series own orange and its darkened light-page text weight
# lands on the mark itself, which is what the halo separates it from.
# LIGHT PAGE ONLY: the on-mark chunk above pins #gd and #snk dark for the
# dark page, so a --paper stroke there would sit dark behind a dark ink, and
# the checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#dec text[fill="#e08214" i],
#gd text[fill="#555" i],
#snk text[fill="#333" i],
#tru text[fill="#762a83" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
