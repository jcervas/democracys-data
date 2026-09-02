# vote-dilution-code.R -- chunk bodies for vote-dilution-brief.Rmd
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

b  <- read.csv("data/derived/blocks.csv",     colClasses = c(GEOID20 = "character"))
d  <- read.csv("data/derived/districts.csv",  stringsAsFactors = FALSE)
tr <- read.csv("data/derived/tradeoff.csv",   stringsAsFactors = FALSE)
sw <- read.csv("data/derived/seed_sweep.csv", stringsAsFactors = FALSE)
cu <- read.csv("data/derived/ga_county_units.csv", colClasses = c(fips = "character"))
sg <- read.csv("data/derived/segregation.csv", stringsAsFactors = FALSE)
bs <- read.csv("data/derived/block_shapes.csv", colClasses = c(id = "character"))
ps <- read.csv("data/derived/plan_shapes.csv",  stringsAsFactors = FALSE)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
sv <- function(nm, col) d[[col]][match(nm, d$plan)]

# ---- the county ------------------------------------------------------------
POP <- sum(b$pop); VAP <- sum(b$vap); REG <- sum(b$reg)
sh_pop <- 100 * sum(b$pop_black) / POP
sh_vap <- 100 * sum(b$vap_black) / VAP
sh_reg <- 100 * sum(b$reg_black) / REG
DISS   <- sg$value[1]

# ---- the enacted plan, and what equal population means ---------------------
ag <- aggregate(cbind(pop, vap, vap_black) ~ boe5, b, sum)
IDEAL <- mean(ag$pop)
ag$dev <- 100 * (ag$pop - IDEAL) / IDEAL
DEV_RANGE <- max(ag$dev) - min(ag$dev)
WORTH <- max(ag$pop) / min(ag$pop)
BIG <- ag$boe5[which.max(ag$pop)]; SML <- ag$boe5[which.min(ag$pop)]
POP_BIG <- max(ag$pop); POP_SML <- min(ag$pop)   # the worked example in the brief
DEV_BIG <- max(ag$dev); DEV_SML <- min(ag$dev)

# ---- Georgia's county unit rule, applied to 2020 population ---------------
UNITS  <- sum(cu$units); NEEDED <- floor(UNITS / 2) + 1
o      <- cu[order(cu$people_per_unit), ]
K      <- which(cumsum(o$units) >= NEEDED)[1]
K_POP  <- sum(o$pop[seq_len(K)])
K_SHR  <- 100 * K_POP / sum(cu$pop)
CU_RAT <- max(cu$people_per_unit) / min(cu$people_per_unit)

# ---- "sufficiently large", as arithmetic ----------------------------------
D_VAP   <- VAP / 5                       # voting-age people per district
NEED_BL <- D_VAP / 2                     # Black adults a majority would take
HAVE_BL <- sum(b$vap_black)
PROP5   <- 5 * sh_vap / 100              # districts proportionality suggests
PROP4   <- 4 * sh_vap / 100

# where the Black adults are
hi   <- b$vap >= 20 & b$vap_black / pmax(b$vap, 1) > 0.5
N_HI <- sum(hi); S_HI <- 100 * sum(b$vap_black[hi]) / HAVE_BL
EMPTY <- sum(b$pop == 0)

# ---- the algorithm's frontier ---------------------------------------------
t5 <- tr[tr$districts == 5, ]; t4 <- tr[tr$districts == 4, ]
t5$reach[!is.finite(t5$reach)] <- Inf; t4$reach[!is.finite(t4$reach)] <- Inf
R1_5  <- t5[t5$reach == 1, ]; RI_5 <- t5[!is.finite(t5$reach), ]
CROSS <- t5[t5$vap_black_pct > 50, ][1, ]           # first setting over 50% VAP
R1_4  <- t4[t4$reach == 1, ]
WOB   <- t5[which.min(t5$vap_black_pct), ]          # the non-monotone setting

# the four-district configuration, over every finite setting of the dial
f4 <- t4[is.finite(t4$reach) & t4$reach <= 100, ]
F4_REG_MIN <- min(f4$reg_black_pct); F4_VAP_MAX <- max(f4$vap_black_pct)

# ---- the enacted district, on the same axes -------------------------------
E4_VAP <- sv("Enacted BOE district 4", "vap_black_pct")
E4_POP <- sv("Enacted BOE district 4", "pop_black_pct")
E4_REG <- sv("Enacted BOE district 4", "reg_black_pct")
E4_PP  <- sv("Enacted BOE district 4", "pp")
E4_RK  <- sv("Enacted BOE district 4", "reock")
CEIL5  <- sv("Ceiling, 5 districts", "vap_black_pct")
CEIL_PP <- sv("Ceiling, 5 districts", "pp")
# does any point on the algorithm's curve match the enacted district?
BEAT <- sum(t5$vap_black_pct >= E4_VAP & t5$pp >= E4_PP)

# ---- the seed sweep -------------------------------------------------------
SW_LO <- min(sw$d5_vap_black_pct); SW_HI <- max(sw$d5_vap_black_pct)
SW_SD <- sd(sw$d5_vap_black_pct);  SW_N  <- nrow(sw)

# ---- geometry helpers ------------------------------------------------------
# Every ring of a long-format polygon table, as one SVG path.
#
# Written as a relative path ("M x,y l dx dy,dx dy ... Z") rather than a list of
# absolute points. The county has ~31,000 vertices; absolute coordinates cost
# about five characters each and the deltas between neighboring vertices cost
# one or two, which is worth roughly 120 KB of the rendered HTML. Precision is
# unchanged: coordinates are rounded to 0.1 SVG units first, so each delta is an
# exact difference of two rounded values and the browser's running sum
# reproduces them. Zero-length segments are dropped.
# No backreference in the replacement: sub() writes a literal \001 byte when the
# captured group is empty, which silently corrupts the path data.
num <- function(v) {
  s <- sub("\\.0$", "", sprintf("%.1f", v))
  s <- sub("^0\\.", ".", s)
  sub("^-0\\.", "-.", s)
}
paths <- function(z, sx, sy) {
  k <- interaction(z$id, z$part, drop = TRUE)
  vapply(split(z, k), function(r) {
    x <- round(sx(r$x), 1); y <- round(sy(r$y), 1)
    dx <- round(diff(x), 1); dy <- round(diff(y), 1)
    k <- dx != 0 | dy != 0
    seg <- if (any(k))
      paste0("l", paste(num(dx[k]), num(dy[k]), sep = " ", collapse = ",")) else ""
    paste0("M", num(x[1]), ",", num(y[1]), seg, "Z")
  }, character(1))
}
XR <- range(bs$x); YR <- range(bs$y)

# Black share of voting-age population, per block, for the choropleth
b$bshare <- ifelse(b$vap > 0, b$vap_black / b$vap, NA)
# "No adults recorded" is not a low value of the thing being shaded; it is the
# absence of the thing, and this brief reports how many such blocks there are as
# a finding in its own right. It therefore leaves the blue ramp entirely and is
# drawn hatched, so that in print it can never be mistaken for the palest data
# class. The palest data class is a blue, not another off-white.
NODATA <- "#ffffff"
HATCH  <- "#9a9a9a"
shade <- function(s, v) {
  z <- ifelse(is.na(s), NODATA,
       ifelse(s < .10, "#e8f1f8", ifelse(s < .25, "#d9e6f2",
       ifelse(s < .40, "#a9c9e2", ifelse(s < .50, "#6fa8d0",
       ifelse(s < .65, "#2c7fb8", "#17527a"))))))
  ifelse(is.na(v) | v == 0, NODATA, z)
}
b$fill <- shade(b$bshare, b$vap)
BRK <- c("no adults recorded", "under 10%", "10-25%", "25-40%",
         "40-50%", "50-65%", "over 65%")
BCOL <- c(NODATA, "#e8f1f8", "#d9e6f2", "#a9c9e2", "#6fa8d0", "#2c7fb8", "#17527a")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- b[order(-b$vap_black), ][1, c("GEOID20", "boe5", "pop", "vap",
                                   "vap_black", "reg", "reg_black")]
names(o) <- c("census block", "enacted district", "population", "voting age",
              "Black adults", "registered", "Black registered")
o

## ---- dev-static
par(mar = c(4.4, 5.6, 2.2, 1.4))
y <- 5:1
plot(NA, xlim = c(-5.6, 5.6), ylim = c(0.5, 5.5), yaxt = "n", bty = "n", las = 1,
     xlab = "deviation from equal population (%)", ylab = "")
rect(-5, 0.3, 5, 5.7, col = "#f0f5ea", border = NA)
abline(v = 0, col = "grey45")
abline(v = c(-5, 5), lty = 2, col = "#4d9221")
axis(2, at = y, labels = paste("District", ag$boe5[order(ag$boe5)]), las = 1,
     tick = FALSE, cex.axis = 0.92)
dv <- ag$dev[order(ag$boe5)]
segments(0, y, dv, y, col = "#2c7fb8", lwd = 3)
points(dv, y, pch = 19, cex = 1.5, col = "#2c7fb8")
text(dv, y, sprintf(" %+.2f", dv), pos = ifelse(dv > 0, 4, 2), cex = 0.78,
     col = "#1a5c88")
text(5, 5.35, " the 10% band a\n legislative plan gets", pos = 4, cex = 0.7,
     col = "#4d9221", xpd = NA)
mtext(sprintf("total spread %.2f points; a legislature is allowed 10",
              DEV_RANGE), 3, line = 0.4, cex = 0.82, col = "#555")

## ---- dev-d3
# The shared chart library draws this one: five rows, one signed value, a
# shaded allowance. Nothing here is particular enough to hand-write, and this
# chunk is the document's first D3 output, so dd_fig() emits the script tags.
o <- ag[order(ag$boe5), ]
o$district <- paste("District", o$boe5)
dd_fig("dev", "bar", o[, c("district", "pop", "dev")],
  rowHeight = 44, size = list(m = list(t = 54, b = 16)),
  x = list(field = "dev", fmt = "signed2", domain = c(-5.6, 5.6), ticks = 7),
  y = list(field = "district", band = TRUE),
  valueLabels = TRUE,
  annotations = list(
    list(type = "band", axis = "x", from = -5, to = 5),
    list(type = "vline", x = -5),
    list(type = "vline", x = 5),
    list(type = "text", px = TRUE, x = 380, y = 16, anchor = "middle",
         size = 12, text = "deviation from equal population (%)")),
  tip = dd_tip(c(pop = "people", dev = "from the ideal"),
               fmt = c(pop = "comma", dev = "signed2"), title = "district"))
cat(sprintf('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Total spread %.2f points; the shaded band is the 10 points a legislature
drawing state or local districts is presumed to be within.</p>
', DEV_RANGE))

## ---- county-units
data.frame(
  quantity = c("People per unit vote, largest county",
               "People per unit vote, smallest county",
               "Ratio between them",
               "Counties that together hold a unit-vote majority",
               "Share of Georgians living in them"),
  value = c(n(max(cu$people_per_unit)), n(min(cu$people_per_unit)),
            paste0(pc(CU_RAT, 0), " to 1"), paste0(K, " of 159"),
            paste0(pc(K_SHR, 1), "%")))

## ---- map-static
par(mar = c(0.4, 0.4, 2.2, 0.4))
plot(NA, xlim = XR, ylim = YR, asp = 1, axes = FALSE, xlab = "", ylab = "")
k <- interaction(bs$id, bs$part, drop = TRUE)
for (r in split(bs, k)) {
  f <- b$fill[match(r$id[1], b$GEOID20)]
  if (is.na(f)) f <- NODATA
  nd <- f == NODATA
  polygon(r$x, r$y, col = f, border = if (nd) "#bfbfbf" else NA, lwd = 0.3)
  if (nd) polygon(r$x, r$y, density = 15, angle = 45, col = HATCH,
                  border = NA, lwd = 0.4)
}
for (dd in 1:5) {
  z <- ps[ps$id == paste("Enacted BOE district", dd), ]
  for (r in split(z, z$part)) lines(r$x, r$y, col = "#C41230", lwd = 1.4)
}
# the no-data class is the LAST entry once reversed, and it is hatched here too
legend("topleft", rev(BRK), fill = c(rev(BCOL)[-7], HATCH),
       density = c(rep(NA, 6), 22), angle = 45,
       border = "grey70", bty = "n",
       cex = 0.68, title = "Black share of a block's adults", title.adj = 0)
mtext(sprintf("Houston County: %s blocks, %s Black adults, index of dissimilarity %.1f",
              n(nrow(b)), n(HAVE_BL), DISS), 3, line = 0.5, cex = 0.8, col = "#555")

## ---- map-d3
SX <- function(v) 40 + (v - XR[1]) / diff(XR) * 500
SY <- function(v) 600 - 30 - (v - YR[1]) / diff(YR) * 540
pb <- paths(bs, SX, SY)
fill <- b$fill[match(sub("\\..*$", "", names(pb)), b$GEOID20)]
fill[is.na(fill)] <- NODATA
# One <path> per shade rather than one per block. The blocks do not overlap and
# nothing on this figure is interactive, so the picture is identical; it saves
# the per-block color string and about 3,400 array wrappers.
grp <- tapply(pb, fill, paste, collapse = "")
blk <- paste(sprintf('["%s","%s"]', grp, names(grp)), collapse = ",")
pl <- ps[grepl("^Enacted", ps$id), ]
lin <- paste(sprintf('"%s"', paths(pl, SX, SY)), collapse = ",")
leg <- paste(sprintf('["%s","%s"]', BRK, BCOL), collapse = ",")
cat(sprintf('
<div id="mp" style="position:relative;margin:1em 0"></div>
<script>
window.__blk=[%s]; window.__SX=%f; window.__SY=%f;
(function(){
const B=window.__blk, L=[%s], K=[%s];
const svg=d3.select("#mp").append("svg").attr("viewBox","0 0 580 600")
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
// "no adults recorded" is hatched, not shaded: it is the absence of the
// quantity, not a low value of it, and it must not read as the palest class
const ND="%s";
const pat=svg.append("defs").append("pattern").attr("id","nodata")
  .attr("width",6).attr("height",6).attr("patternUnits","userSpaceOnUse")
  .attr("patternTransform","rotate(45)");
pat.append("rect").attr("width",6).attr("height",6).attr("fill","#fff");
pat.append("line").attr("x1",0).attr("y1",0).attr("x2",0).attr("y2",6)
  .attr("stroke","%s").attr("stroke-width",1.1);
const g=svg.append("g");
g.selectAll("path.b").data(B).join("path").attr("d",d=>d[0])
  .attr("fill",d=>d[1]===ND?"url(#nodata)":d[1])
  .attr("stroke",d=>d[1]===ND?"#bfbfbf":"none").attr("stroke-width",0.3);
g.selectAll("path.l").data(L).join("path").attr("d",d=>d).attr("fill","none")
  .attr("stroke","#C41230").attr("stroke-width",1.5);
const lg=svg.append("g").attr("transform","translate(14,16)");
lg.append("text").attr("x",0).attr("y",0).attr("font-size","11.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Black share of a block\\u2019s adults");
K.forEach((k,i)=>{
  lg.append("rect").attr("x",0).attr("y",8+i*15).attr("width",13).attr("height",11)
    .attr("fill",k[1]===ND?"url(#nodata)":k[1])
    .attr("stroke","#bbb").attr("stroke-width",0.5);
  lg.append("text").attr("x",19).attr("y",17.5+i*15).attr("font-size","11px")
    .attr("fill","#444").text(k[0]);});
lg.append("line").attr("x1",0).attr("x2",13).attr("y1",14+K.length*15)
  .attr("y2",14+K.length*15).attr("stroke","#C41230").attr("stroke-width",1.8);
lg.append("text").attr("x",19).attr("y",18+K.length*15).attr("font-size","11px")
  .attr("fill","#444").text("enacted district boundary");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
All %s census blocks, shaded by the Black share of their voting-age population;
the %s hatched blocks contain nobody at all. Red lines are the five enacted
districts. The county’s index of dissimilarity is %.1f.</p>
', blk, 0, 0, lin, leg, NODATA, HATCH, n(nrow(b)), n(EMPTY), DISS))

## ---- front-static
par(mar = c(4.4, 4.6, 2.4, 1.2))
plot(t5$pp, t5$vap_black_pct, type = "n", xlim = c(0, 0.36), ylim = c(38, 70),
     las = 1, xlab = "Polsby-Popper compactness (1 is a circle)",
     ylab = "Black share of the district's adults (%)")
rect(-1, 50, 1, 100, col = "#f0f5ea", border = NA)
abline(h = 50, lty = 2, col = "#4d9221", lwd = 1.6)
z <- t5[order(t5$pp), ]
lines(z$pp, z$vap_black_pct, col = "#2c7fb8", lwd = 2)
points(t5$pp, t5$vap_black_pct, pch = 19, cex = 1.05, col = "#2c7fb8")
lb <- t5$reach %in% c(1, 8, 24, 40, 100, 200) | !is.finite(t5$reach)
text(t5$pp[lb], t5$vap_black_pct[lb],
     ifelse(is.finite(t5$reach[lb]), t5$reach[lb], "unlim"),
     pos = c(4, 4, 4, 4, 4, 1, 4)[seq_len(sum(lb))], cex = 0.68, col = "#1a5c88")
points(CEIL_PP, CEIL5, pch = 17, cex = 1.3, col = "#8856a7")
text(CEIL_PP, CEIL5, " ceiling: contiguity dropped", pos = 4, cex = 0.72,
     col = "#8856a7")
text(0.355, 51.4, "majority-Black ", pos = 2, cex = 0.74, col = "#4d9221")
mtext("each point is one setting of the dial; label is reach", 3, line = 0.5,
      cex = 0.82, col = "#555")

## ---- front-d3
rows <- paste(sprintf('{"r":"%s","x":%.5f,"y":%.3f,"g":%.3f,"n":%d}',
                      ifelse(is.finite(t5$reach), t5$reach, "unlimited"),
                      t5$pp, t5$vap_black_pct, t5$reock, t5$blocks),
              collapse = ",")
cat(sprintf('
<div id="fr" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],CP=%.5f,CV=%.3f;
const W=740,H=440,M={t:22,r:130,b:52,l:62};
const box=d3.select("#fr");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,0.36]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([38,70]).range([H-M.b,M.t]);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",y(50)-M.t).attr("fill","#4d9221").attr("fill-opacity",0.08);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#4d9221").attr("stroke-dasharray","5,4").attr("stroke-width",1.8);
svg.append("text").attr("x",W-M.r-6).attr("y",y(50)-6).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#4d9221").text("majority-Black");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Polsby-Popper compactness (1 is a circle)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("Black share of the district\\u2019s adults");
const S=D.slice().sort((a,b)=>a.x-b.x);
svg.append("path").attr("d",d3.line().x(d=>x(d.x)).y(d=>y(d.y))(S))
  .attr("fill","none").attr("stroke","#2c7fb8").attr("stroke-width",2);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",6).attr("fill","#2c7fb8")
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>reach ${d.r}</b><br>${d.n} blocks<br>${d.y.toFixed(1)}%% Black adults<br>`+
     `Polsby-Popper ${d.x.toFixed(3)} &middot; Reock ${d.g.toFixed(3)}`)
     .style("left",Math.min(e.offsetX+14,W-320)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.r").data(D.filter(d=>["1","8","24","40","100","200","unlimited"].includes(d.r)))
  .join("text").attr("x",d=>x(d.x)+10).attr("y",d=>y(d.y)+4)
  .attr("font-size","11px").attr("fill","#1a5c88")
  .text(d=>d.r==="unlimited"?"\\u221e":d.r);
svg.append("path").attr("d",d3.symbol().type(d3.symbolTriangle).size(110)())
  .attr("transform",`translate(${x(CP)},${y(CV)})`).attr("fill","#8856a7");
svg.append("text").attr("x",x(CP)+12).attr("y",y(CV)+4).attr("font-size","11.5px")
  .attr("fill","#8856a7").text("ceiling: contiguity dropped");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Each point is one setting of the dial. Hover for the district it produced.</p>
', rows, CEIL_PP, CEIL5))

## ---- pivot
data.frame(
  district = c(paste0("Algorithm, reach ", R1_5$reach),
               paste0("Algorithm, reach ", CROSS$reach),
               "Algorithm, reach unlimited",
               "Ceiling (contiguity dropped)",
               "ENACTED District 4"),
  black_of_adults = paste0(pc(c(R1_5$vap_black_pct, CROSS$vap_black_pct,
                                RI_5$vap_black_pct, CEIL5, E4_VAP)), "%"),
  polsby_popper = pc(c(R1_5$pp, CROSS$pp, RI_5$pp, CEIL_PP, E4_PP), 3),
  reock = pc(c(R1_5$reock, CROSS$reock, RI_5$reock,
               sv("Ceiling, 5 districts", "reock"), E4_RK), 3))

## ---- cmp-static
par(mfrow = c(1, 3), mar = c(0.3, 0.3, 2.0, 0.3))
draw <- function(nm, col, ttl, sub) {
  plot(NA, xlim = XR, ylim = YR, asp = 1, axes = FALSE, xlab = "", ylab = "")
  z <- ps[ps$id == "COUNTY", ]
  for (r in split(z, z$part)) polygon(r$x, r$y, col = "#f2f2f2", border = "grey65")
  z <- ps[ps$id == nm, ]
  for (r in split(z, z$part)) polygon(r$x, r$y, col = col, border = col)
  mtext(ttl, 3, line = 0.85, cex = 0.74, col = "#333")
  mtext(sub, 3, line = 0.05, cex = 0.64, col = "#666")
}
draw("Reach 1, 5 districts", "#2c7fb8", "Algorithm, reach 1",
     sprintf("%.1f%% Black adults, PP %.3f", R1_5$vap_black_pct, R1_5$pp))
draw("Reach unlimited, 5 districts", "#e08214", "Algorithm, reach unlimited",
     sprintf("%.1f%% Black adults, PP %.3f", RI_5$vap_black_pct, RI_5$pp))
draw("Enacted BOE district 4", "#C41230", "Enacted District 4",
     sprintf("%.1f%% Black adults, PP %.3f", E4_VAP, E4_PP))

## ---- cmp-d3
SX <- function(v) 8 + (v - XR[1]) / diff(XR) * 210
SY <- function(v) 250 - 26 - (v - YR[1]) / diff(YR) * 200
cty <- paste(sprintf('"%s"', paths(ps[ps$id == "COUNTY", ], SX, SY)),
             collapse = ",")
pan <- c("Reach 1, 5 districts", "Reach unlimited, 5 districts",
         "Enacted BOE district 4")
col <- c("#2c7fb8", "#e08214", "#C41230")
ttl <- c("Algorithm, reach 1", "Algorithm, reach unlimited", "Enacted District 4")
sub <- sprintf("%.1f%% Black adults &middot; PP %.3f",
               c(R1_5$vap_black_pct, RI_5$vap_black_pct, E4_VAP),
               c(R1_5$pp, RI_5$pp, E4_PP))
cells <- paste(vapply(seq_along(pan), function(i) sprintf(
  '{"p":[%s],"c":"%s","t":"%s","s":"%s"}',
  paste(sprintf('"%s"', paths(ps[ps$id == pan[i], ], SX, SY)), collapse = ","),
  col[i], ttl[i], sub[i]), character(1)), collapse = ",")
cat(sprintf('
<div id="cmp" style="display:flex;gap:6px;flex-wrap:wrap;margin:1em 0"></div>
<script>
(function(){
const C=[%s], CT=[%s];
const box=d3.select("#cmp");
C.forEach(c=>{
  const w=box.append("div").attr("style","flex:1 1 200px;min-width:180px");
  const svg=w.append("svg").attr("viewBox","0 0 226 250")
    .attr("style","width:100%%;height:auto;font:12px inherit");
  svg.append("text").attr("x",113).attr("y",14).attr("text-anchor","middle")
    .attr("font-size","12px").attr("font-weight","600").attr("fill","#333")
    .text(c.t);
  svg.append("text").attr("x",113).attr("y",245).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#666").html(c.s);
  svg.append("g").selectAll("path.c").data(CT).join("path").attr("d",d=>d)
    .attr("fill","#f2f2f2").attr("stroke","#bbb");
  svg.append("g").selectAll("path.d").data(c.p).join("path").attr("d",d=>d)
    .attr("fill",c.c).attr("stroke",c.c);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The same county three times. The enacted district is both more Black and more
compact than the algorithm’s unlimited-reach attempt.</p>
', cells, cty))

## ---- denominator
o <- f4[f4$reach %in% c(1, 4, 12, 24, 60, 100), ]
data.frame(
  reach = as.character(o$reach),
  of_population = paste0(pc(o$pop_black_pct), "%"),
  of_adults = paste0(pc(o$vap_black_pct), "%"),
  of_registered_voters = paste0(pc(o$reg_black_pct), "%"),
  majority = ifelse(o$vap_black_pct > 50,
                    ifelse(o$reg_black_pct > 50, "on both", "on adults only"),
                    ifelse(o$reg_black_pct > 50, "on registered voters only",
                           "on neither")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: these fills are not lifted on the dark page, so a --paper
# stroke there would sit dark behind a dark ink, and the checker scores the
# fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
# The deviation figure is no longer here: it comes from the shared chart
# library now, which colours by class rather than by fill, so brief.css
# already handles it in both themes.
cat('<style>
@media (prefers-color-scheme: light) {
#fr text[fill="#1a5c88" i],
#fr text[fill="#4d9221" i],
#fr text[fill="#8856a7" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
