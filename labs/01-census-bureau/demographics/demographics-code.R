# demographics-code.R -- chunk bodies for demographics-brief.Rmd
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
D <- "data"

# GEOIDs are CHARACTER everywhere in this document. A tract identifier is 11
# digits and a block identifier 15; read as numbers, the block becomes
# 2.6163e+14 and every identifier in a state whose FIPS code begins with a zero
# quietly loses that zero. The sibling lab that got this wrong lost the leading
# digit on 1,549 of 4,489 tracts.
CH <- c(geoid = "character", id = "character", st = "character",
        GEOID = "character")
rd <- function(f, ...) read.csv(file.path(D, f), stringsAsFactors = FALSE, ...)

fc  <- rd("derived/facts.csv")
nat <- rd("derived/nation.csv")
reg <- rd("derived/regions.csv")
sta <- rd("derived/states.csv",   colClasses = c(st = "character", geoid = "character"))
cou <- rd("derived/counties.csv", colClasses = c(geoid = "character", st = "character"))
lad <- rd("derived/ladder.csv")
dec <- rd("derived/decades.csv")
cts <- rd("derived/categories.csv")
std <- rd("derived/standards.csv")
tld <- rd("derived/tract_largest.csv")
usr <- rd("derived/us_rings.csv", colClasses = c(st = "character"))
usr$id <- usr$st           # the ring helpers below all key on `id`
dvr <- rd("derived/us_divisions.csv")
mlb <- rd("derived/us_maplabels.csv")
wtr <- rd("derived/wayne_tracts.csv",      colClasses = c(geoid = "character"))
wtg <- rd("derived/wayne_tract_rings.csv", colClasses = c(id = "character"))
wbg <- rd("derived/wayne_bg.csv",          colClasses = c(geoid = "character"))
wbr <- rd("derived/wayne_bg_rings.csv",    colClasses = c(id = "character"))
wou <- rd("derived/wayne_outline.csv",     colClasses = c(id = "character"))
wtx <- rd("derived/wayne_transect.csv",    colClasses = c(GEOID = "character"))
wbk <- rd("derived/wayne_block_rings.csv", colClasses = c(id = "character"))
wdt <- rd("derived/wayne_detroit_rings.csv")
pro <- rd("derived/eightmile_profile.csv")

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("categories.csv", "counties.csv", "decades.csv", "eightmile_profile.csv", "facts.csv", "ladder.csv", "metros.csv", "nation.csv", "regions.csv", "standards.csv", "states.csv", "tract_dist.csv", "tract_largest.csv", "us_divisions.csv", "us_maplabels.csv", "us_rings.csv", "wayne_bg.csv", "wayne_bg_rings.csv", "wayne_block_rings.csv", "wayne_detroit_rings.csv", "wayne_outline.csv", "wayne_tract_rings.csv", "wayne_tracts.csv", "wayne_transect.csv"))

FV <- function(k) { v <- fc$value[fc$name == k]
                    if (!length(v)) stop("no such fact: ", k); v }
FN <- function(k) as.numeric(FV(k))
n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# ---- the download itself, for the raw-to-clean section ----------------------
# Eight rows of the national roll-up, verbatim, plus the counts measured from
# the full file at capture time. See data/raw/README.txt.
RSH <- read.csv(file.path(D, "raw", "source-shape.csv"), stringsAsFactors = FALSE)
RS  <- function(k) RSH$value[RSH$name == k]

# Which fields of Table P1 have to be added to get "White alone or in any
# combination". Built the same way the build script builds it -- from the
# combinations, not from a typed list -- and then checked against the number
# the build wrote out.
.RAWA  <- strsplit(readLines(file.path(D, "raw", "us000012020-head.txt"),
                            warn = FALSE)[1], "|", fixed = TRUE)[[1]]
.combo <- local({
  out <- list(); f <- 9L
  for (k in 2:6) {
    f <- f + 1L; cm <- combn(6L, k)
    for (j in seq_len(ncol(cm))) { f <- f + 1L
      out[[length(out) + 1L]] <- list(field = f, races = cm[, j]) }
  }
  out
})
WFLD <- c(5L + 3L,                                   # White alone: P1 field 3
          5L + vapply(Filter(function(z) 1L %in% z$races, .combo),
                      function(z) z$field, integer(1)))
NW   <- length(WFLD)
stopifnot(sum(as.numeric(.RAWA[WFLD])) == nat$white_any,
          as.numeric(.RAWA[6])  == nat$total,
          as.numeric(.RAWA[78]) == nat$hispanic,
          as.numeric(.RAWA[81]) == nat$nh_white)
LD <- function(l, cc) lad[[cc]][lad$level == l]
SD <- function(y, cc) { v <- std[[cc]][std$key == y]
                        if (!length(v)) stop("no such column: ", cc); v }
# The change columns are stored signed, because the figures need the sign. In
# prose the direction is carried by the verb ("falls", "rises"), so quoting the
# signed value would produce "falls -8.6%". mag() is the magnitude alone.
mag <- function(k) sub("^[+-]", "", FV(k))
# A minimum-gap spreader, for figures whose right-hand labels would otherwise
# collide. Keeps the order and the center of mass; moves nothing that fits.
spread <- function(y, gap) {
  o <- order(y); v <- y[o]
  for (i in 2:length(v)) if (v[i] - v[i - 1] < gap) v[i] <- v[i - 1] + gap
  sh <- (mean(y) - mean(v))
  v <- v + sh
  out <- numeric(length(y)); out[o] <- v; out
}

# ---- the eight categories, in the Bureau's own words and one fixed order ----
# Every figure in this chapter that shows categories shows them in THIS order
# and THESE colors, so a reader who learns the palette once can read every
# later figure without the legend.
CAT <- c("hispanic", "nh_white", "nh_black", "nh_asian", "nh_two",
         "nh_aian", "nh_sor", "nh_nhpi")
LAB <- c(hispanic = "Hispanic or Latino", nh_white = "White",
         nh_black = "Black or African American", nh_asian = "Asian",
         nh_two = "Two or More Races",
         nh_aian = "American Indian and Alaska Native",
         nh_sor = "Some Other Race",
         nh_nhpi = "Native Hawaiian and Other Pacific Islander")
SHORT <- c(hispanic = "Hispanic", nh_white = "White", nh_black = "Black",
           nh_asian = "Asian", nh_two = "Two or More", nh_aian = "AIAN",
           nh_sor = "Some Other Race", nh_nhpi = "NHPI")
COL <- c(hispanic = "#E08214", nh_white = "#7F9BB3", nh_black = "#C41230",
         nh_asian = "#4D9221", nh_two = "#8073AC", nh_aian = "#B35806",
         nh_sor = "#999999", nh_nhpi = "#2C7FB8")
NP  <- setNames(as.numeric(nat[CAT]), CAT)
NPC <- 100 * NP / nat$total

RACE6 <- c("white", "black", "aian", "asian", "nhpi", "sor")
RLAB  <- c(white = "White", black = "Black or African American",
           aian = "American Indian and Alaska Native", asian = "Asian",
           nhpi = "Native Hawaiian and Other Pacific Islander",
           sor = "Some Other Race")

# ---- geometry helpers, shared by both renderers ----------------------------
# The same approach `areal-units` and `census-geography` use: one SVG path per
# ring, integer pixel coordinates, RELATIVE line commands. Rounding to whole
# pixels is what keeps several thousand block outlines inside a document of
# ordinary size.
onepath <- function(X, Y) {
  dx <- X[-1] - X[-length(X)]; dy <- Y[-1] - Y[-length(Y)]
  seg <- paste0(dx, ifelse(dy < 0, "", ","), dy)
  sep <- c("", ifelse(substr(seg[-1], 1, 1) == "-", "", " "))
  paste0("M", X[1], ",", Y[1], "l", paste0(sep, seg, collapse = ""), "Z")
}
ringpaths <- function(d, sx, sy) {
  if (!nrow(d)) return(character(0))
  k <- interaction(d$id, d$part, drop = TRUE)
  p <- vapply(split(d, k), function(z) {
    X <- round(sx(z$x)); Y <- round(sy(z$y))
    keep <- c(TRUE, X[-1] != X[-length(X)] | Y[-1] != Y[-length(Y)])
    X <- X[keep]; Y <- Y[keep]
    if (length(X) < 3) return(paste0("M", X[1], ",", Y[1], "h1v1h-1Z"))
    onepath(X, Y)
  }, character(1))
  p[nzchar(p)]
}
ringids <- function(d) {
  k <- interaction(d$id, d$part, drop = TRUE)
  vapply(split(d, k), function(z) z$id[1], character(1))
}
drawrings <- function(d, fill, border = NA, lwd = 0.3) {
  if (!nrow(d)) return(invisible())
  k <- interaction(d$id, d$part, drop = TRUE)
  for (l in levels(k)) {
    z <- d[k == l, ]
    polygon(z$x, z$y, col = fill[[as.character(z$id[1])]], border = border,
            lwd = lwd)
  }
}
jstr <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")
jnum <- function(x) paste0("[", paste0(x, collapse = ","), "]")
win <- function(d, pad = 0.04) {
  rx <- range(d$x); ry <- range(d$y)
  s <- max(diff(rx), diff(ry)) * (1 + pad) / 2
  list(x = mean(rx) + c(-s, s), y = mean(ry) + c(-s, s), s = 2 * s)
}

# ---- render every data.frame in this document as a TABLE, not code output ---
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- fig1-d3
# ---------------------------------------------------------------------------
# FIGURE 1. A staircase, not a line chart: the rungs are not equally spaced in
# any real quantity, so joining them with a slope would invent an interpolation
# nobody can defend. Steps say "these are eight separate calculations."
#
# THE NULL IS THE POINT OF THE FIGURE. Finer units always look more segregated,
# even under random assignment, because small units are noisy -- that is a real
# artifact and the reason this chapter cannot just assert the staircase means
# sorting. So the same people are shuffled at random into units of the IDENTICAL
# sizes and the measure is recomputed. The pale line is that null. Where the
# two lines separate is the part that is people; where they converge is the part
# that is arithmetic.
# ---------------------------------------------------------------------------
LV <- lad$level
d0 <- 100 * lad$ebar0 / log(8)
d1 <- lad$diversity
W6 <- 760; L6 <- 62; R6M <- 154; T6 <- 46; B6 <- 62; H6 <- 330
SW <- (W6 - L6 - R6M) / length(LV)
yl <- c(30, 63)
ys <- function(v) T6 + (yl[2] - v) / diff(yl) * (H6 - T6 - B6)
steps <- function(v) {
  s <- character(0)
  for (i in seq_along(v)) {
    x0 <- L6 + (i - 1) * SW; x1 <- L6 + i * SW
    s <- c(s, sprintf("%s%.1f,%.1f L%.1f,%.1f", if (i == 1) "M" else "L",
                      x0, ys(v[i]), x1, ys(v[i])))
    if (i < length(v)) s <- c(s, sprintf("L%.1f,%.1f", x1, ys(v[i + 1])))
  }
  paste(s, collapse = " ")
}
labs <- paste(vapply(seq_along(LV), function(i) sprintf(
  '{"l":"%s","u":"%s","x":%.1f,"y":%.1f,"y0":%.1f,"d":"%s","h":"%s"}',
  LV[i], n(lad$units[i]), L6 + (i - 0.5) * SW, ys(d1[i]), ys(d0[i]),
  pc(d1[i]), pc(lad$H[i], 3)), character(1)), collapse = ",")
cat(paste0('
<div id="f1" style="margin:1.1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const L=[', labs, '];
const P1="', steps(d1), '",P0="', steps(d0), '";
const W=', W6, ',H=', H6, ',LX=', L6, ',T=', T6, ',B=', B6, ',SW=', SW, ';
const svg=d3.select("#f1").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
[35,40,45,50,55,60].forEach(v=>{
  const y=', sprintf("%.4f", T6), '+(', yl[2], '-v)/', diff(yl),
  '*(H-T-B);
  svg.append("line").attr("x1",LX).attr("x2",W-154).attr("y1",y).attr("y2",y)
    .attr("stroke","#f0f0f0");
  svg.append("text").attr("x",LX-8).attr("y",y+4).attr("text-anchor","end")
    .attr("font-size","10px").attr("fill","#aaa").text(v);
});
svg.append("path").attr("d",P0).attr("fill","none").attr("stroke","#bbb")
  .attr("stroke-width",1.6).attr("stroke-dasharray","5,3");
svg.append("path").attr("d",P1).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.6);
L.forEach((d,i)=>{
  svg.append("circle").attr("cx",d.x).attr("cy",d.y).attr("r",3.6)
    .attr("fill","#C41230");
  svg.append("text").attr("x",d.x).attr("y",d.y-10).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#C41230")
    .text(d.d);
  svg.append("text").attr("x",d.x).attr("y",H-B+16).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#333").text(d.l);
  svg.append("text").attr("x",d.x).attr("y",H-B+29).attr("text-anchor","middle")
    .attr("font-size","9.5px").attr("fill","#999").text(d.u);
});
svg.append("text").attr("x",LX-8).attr("y",T-16).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#888").text("diversity");
svg.append("text").attr("x",LX).attr("y",T-16).attr("font-size","11.5px")
  .attr("fill","#C41230").attr("font-weight","600")
  .text("the score of the unit the average American lives in");
const lx=W-148;
svg.append("line").attr("x1",lx).attr("x2",lx+22).attr("y1",T+8).attr("y2",T+8)
  .attr("stroke","#C41230").attr("stroke-width",2.6);
svg.append("text").attr("x",lx+28).attr("y",T+12).attr("font-size","11px")
  .attr("fill","#333").text("observed");
svg.append("line").attr("x1",lx).attr("x2",lx+22).attr("y1",T+26).attr("y2",T+26)
  .attr("stroke","#bbb").attr("stroke-width",1.6).attr("stroke-dasharray","5,3");
svg.append("text").attr("x",lx+28).attr("y",T+30).attr("font-size","11px")
  .attr("fill","#333").text("same people,");
svg.append("text").attr("x",lx+28).attr("y",T+43).attr("font-size","11px")
  .attr("fill","#333").text("shuffled at random");
})();
</script>'))

## ---- fig1-static
LV <- lad$level
d0 <- 100 * lad$ebar0 / log(8)
d1 <- lad$diversity
par(mar = c(4.0, 3.4, 2.6, 0.8))
plot(NA, xlim = c(0.5, length(LV) + 0.5), ylim = c(30, 63), axes = FALSE,
     ann = FALSE)
abline(h = seq(35, 60, 5), col = "#f0f0f0")
axis(2, seq(35, 60, 5), cex.axis = 0.6, col = "#dddddd", col.axis = "#aaa",
     tck = -0.015, mgp = c(2, 0.4, 0), las = 1)
stepx <- rep(seq_along(LV), each = 2) + c(-0.5, 0.5)
lines(stepx, rep(d0, each = 2), col = "#bbbbbb", lwd = 1.6, lty = 2)
lines(stepx, rep(d1, each = 2), col = "#C41230", lwd = 2.4)
points(seq_along(LV), d1, pch = 19, col = "#C41230", cex = 0.8)
text(seq_along(LV), d1 + 1.6, pc(d1), cex = 0.6, font = 2, col = "#C41230")
axis(1, seq_along(LV), LV, tick = FALSE, cex.axis = 0.6, line = -0.4,
     col.axis = "#333")
mtext(n(lad$units), side = 1, at = seq_along(LV), line = 1.1, cex = 0.5,
      col = "#999")
title("The diversity score of the unit the average American lives in",
      cex.main = 0.85, adj = 0, col.main = "#C41230", line = 1.2)
legend("bottomleft", c("observed", "same people, shuffled at random"),
       col = c("#C41230", "#bbbbbb"), lwd = c(2.4, 1.6), lty = c(1, 2),
       bty = "n", cex = 0.6)

## ---- fig2-d3
# ---------------------------------------------------------------------------
# FIGURE 2. Three maps of one county, and the encoding is CATEGORICAL rather
# than a ramp, on purpose. A ramp of "share Black" would answer a question the
# reader already knows the answer to. Coloring each unit by WHICH category is
# largest asks a different one -- how many different places is this county? --
# and the answer changes with the size of the unit while the ground does not
# move at all. Opacity carries how lopsided the unit is, so a 51% plurality and
# a 95% supermajority do not read the same.
# ---------------------------------------------------------------------------
PWs <- 236L; GAPs <- 12L; TOPs <- 34L; BOTs <- 40L
w <- win(wou, pad = 0.03)
sc <- PWs / w$s
lev <- list(
  list(t = "the county", sub = "1 unit", rings = wou,
       val = data.frame(id = "26163",
                        largest = FV("wayne_largest"),
                        pct = FN("wayne_largest_pct"),
                        stringsAsFactors = FALSE)),
  list(t = "its tracts", sub = paste(n(FV("wayne_tracts")), "units"),
       rings = wtg,
       val = data.frame(id = wtr$geoid, largest = wtr$largest,
                        pct = wtr$largest_pct, stringsAsFactors = FALSE)),
  list(t = "its block groups", sub = paste(n(FV("wayne_bg")), "units"),
       rings = wbr,
       val = data.frame(id = wbg$geoid, largest = wbg$largest,
                        pct = wbg$largest_pct, stringsAsFactors = FALSE)))
KEYC <- c(White = "#7F9BB3", Black = "#C41230", Hispanic = "#E08214",
          Asian = "#4D9221", `Two or More` = "#8073AC", AIAN = "#B35806",
          `Some Other Race` = "#999999", NHPI = "#2C7FB8")
mkw <- function(i) {
  L <- lev[[i]]
  off <- (i - 1L) * (PWs + GAPs)
  fx <- function(x) off + (x - w$x[1]) * sc
  fy <- function(y) TOPs + (w$y[2] - y) * sc
  P <- ringpaths(L$rings, fx, fy); ids <- ringids(L$rings)
  m <- match(ids, L$val$id)
  cl <- KEYC[L$val$largest[m]]; cl[is.na(cl)] <- "#eeeeee"
  op <- 0.22 + 0.78 * pmin(pmax((L$val$pct[m] - 30) / 60, 0), 1)
  op[is.na(op)] <- 0.1
  paste0("[", jstr(P), ",", jstr(unname(cl)), ",",
         jnum(sprintf("%.2f", op)), ",", off, ',"', L$t, '","', L$sub, '"]')
}
PANW <- vapply(1:3, mkw, character(1))
KL <- names(KEYC)[names(KEYC) %in% unique(c(wtr$largest, wbg$largest))]
keyj <- paste(sprintf('{"n":"%s","c":"%s"}', KL, unname(KEYC[KL])),
              collapse = ",")
Ww <- 3L * PWs + 2L * GAPs; Hw <- TOPs + PWs + BOTs
cat(paste0('
<div id="f2" style="margin:1.1em 0"></div>
<script>
(function(){
const P=[', paste(PANW, collapse = ","), '],K=[', keyj, '];
const W=', Ww, ',H=', Hw, ',PW=', PWs, ',TOP=', TOPs, ';
const svg=d3.select("#f2").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
P.forEach(p=>{
  const paths=p[0],cl=p[1],op=p[2],off=p[3],t=p[4],s=p[5];
  const g=svg.append("g");
  g.selectAll("path").data(paths).join("path").attr("d",d=>d)
    .attr("fill",(d,j)=>cl[j]).attr("fill-opacity",(d,j)=>op[j])
    .attr("stroke","#ffffff").attr("stroke-width",0.25);
  svg.append("text").attr("x",off+PW/2).attr("y",15).attr("text-anchor","middle")
    .attr("font-size","12.5px").attr("font-weight","600").attr("fill","#222")
    .text(t);
  svg.append("text").attr("x",off+PW/2).attr("y",27).attr("text-anchor","middle")
    .attr("font-size","10px").attr("fill","#888").text(s);
});
let kx=8;
K.forEach(k=>{
  svg.append("rect").attr("x",kx).attr("y",H-24).attr("width",11)
    .attr("height",11).attr("fill",k.c);
  svg.append("text").attr("x",kx+15).attr("y",H-15).attr("font-size","10.5px")
    .attr("fill","#555").text(k.n);
  kx+=26+k.n.length*5.9;
});
svg.append("text").attr("x",8).attr("y",H-3).attr("font-size","10px")
  .attr("fill","#999")
  .text("color = the largest category in the unit; stronger color = a larger share");
})();
</script>'))

## ---- fig2-static
w <- win(wou, pad = 0.03)
KEYC <- c(White = "#7F9BB3", Black = "#C41230", Hispanic = "#E08214",
          Asian = "#4D9221", `Two or More` = "#8073AC", AIAN = "#B35806",
          `Some Other Race` = "#999999", NHPI = "#2C7FB8")
lev <- list(
  list(t = "the county", sub = "1 unit", rings = wou,
       id = "26163", largest = FV("wayne_largest"),
       pct = FN("wayne_largest_pct")),
  list(t = "its tracts", sub = paste(n(FV("wayne_tracts")), "units"),
       rings = wtg, id = wtr$geoid, largest = wtr$largest,
       pct = wtr$largest_pct),
  list(t = "its block groups", sub = paste(n(FV("wayne_bg")), "units"),
       rings = wbr, id = wbg$geoid, largest = wbg$largest,
       pct = wbg$largest_pct))
layout(matrix(1:3, 1, 3))
op <- par(mar = c(0.4, 0.2, 2.6, 0.2), oma = c(2.6, 0, 0, 0))
for (L in lev) {
  cl <- KEYC[L$largest]; cl[is.na(cl)] <- "#eeeeee"
  al <- 0.22 + 0.78 * pmin(pmax((L$pct - 30) / 60, 0), 1)
  al[is.na(al)] <- 0.1
  fill <- as.list(mapply(adjustcolor, unname(cl), al))
  names(fill) <- L$id
  plot(NA, xlim = w$x, ylim = w$y, asp = 1, axes = FALSE, ann = FALSE)
  drawrings(L$rings, fill, border = "#ffffff", lwd = 0.15)
  title(L$t, cex.main = 0.95, line = 1.1)
  mtext(L$sub, side = 3, line = 0.25, cex = 0.55, col = "#888")
}
KL <- names(KEYC)[names(KEYC) %in% unique(c(wtr$largest, wbg$largest))]
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
legend("bottom", KL, fill = unname(KEYC[KL]), border = NA, bty = "n",
       cex = 0.55, horiz = TRUE, inset = c(0, 0.005))
text(0.5, 0.055,
     "color = the largest category in the unit; stronger color = a larger share",
     cex = 0.5, col = "#999")
par(op); layout(1)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# FIGURE 3. A map and a profile, side by side, sharing one vertical axis: a
# block's position on the map and its position on the profile are the same
# distance from the same line. Nothing else in this document uses a shared
# axis across two panels, and nothing else needed it -- here it is the whole
# claim, because the point is that the profile's step and the map's line are
# the same object seen twice.
#
# The map's color is share Black; a single-variable ramp, unlike Figure 8's
# categorical fill, because at block level "which group is largest" is mostly
# noise: 18 percent of populated blocks in the country contain exactly one
# category, often because they contain four people.
# ---------------------------------------------------------------------------
MW <- 400L; PWp <- 300L; GP <- 34L
rxk <- range(wbk$x); ryk <- range(wbk$y)
scm <- min(MW / diff(rxk), 300 / diff(ryk))
MH <- round(diff(ryk) * scm)
T8 <- 44L; B8 <- 44L
W8 <- MW + GP + PWp + 76; H8 <- T8 + MH + B8
fx <- function(x) 8 + (x - rxk[1]) * scm
fy <- function(y) T8 + (ryk[2] - y) * scm
BP <- ringpaths(wbk, fx, fy); bid <- ringids(wbk)
m <- match(bid, wtx$GEOID)
pb <- 100 * wtx$nh_black[m] / ifelse(wtx$total[m] == 0, 1, wtx$total[m])
pb[is.na(pb) | wtx$total[m] == 0] <- NA
DP <- ringpaths(wdt, fx, fy)
# profile panel
PX <- 8 + MW + GP
pro$pw <- 100 * pro$nh_white / pro$total
pro$pb <- 100 * pro$nh_black / pro$total
prj <- paste(sprintf('{"y":%.1f,"b":%.2f,"w":%.2f}',
                     T8 + (ryk[2] - (mean(ryk) + pro$mid)) * scm,
                     pro$pb, pro$pw), collapse = ",")
cat(paste0('
<div id="f3" style="margin:1.1em 0"></div>
<script>
(function(){
const BP=', jstr(BP), ',V=', jnum(ifelse(is.na(pb), -1, round(pb, 1))), ';
const DP=', jstr(DP), ',PR=[', prj, '];
const W=', W8, ',H=', H8, ',PX=', PX, ',PW=', PWp, ',T=', T8, ',MH=', MH, ';
const svg=d3.select("#f3").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const ramp=d3.interpolateRgb("#f7f7f7","#C41230");
svg.append("g").selectAll("path").data(BP).join("path").attr("d",d=>d)
  .attr("fill",(d,j)=>V[j]<0?"#e8e8e8":ramp(0.06+0.94*V[j]/100))
  .attr("stroke","#ffffff").attr("stroke-width",0.15);
svg.append("g").selectAll("path").data(DP).join("path").attr("d",d=>d)
  .attr("fill","none").attr("stroke","#111").attr("stroke-width",1.8);
svg.append("text").attr("x",8).attr("y",16).attr("font-size","12.5px")
  .attr("font-weight","600").text("blocks along Eight Mile Road");
svg.append("text").attr("x",8).attr("y",30).attr("font-size","10px")
  .attr("fill","#888")
  .text("shading = share Black; the black line is Detroit\\u2019s city limit");
// profile
const x=v=>PX+v/100*PW;
[0,25,50,75,100].forEach(v=>{
  svg.append("line").attr("x1",x(v)).attr("x2",x(v)).attr("y1",T)
    .attr("y2",T+MH).attr("stroke","#eee");
  svg.append("text").attr("x",x(v)).attr("y",T+MH+15).attr("text-anchor","middle")
    .attr("font-size","10px").attr("fill","#aaa").text(v+"%");
});
const ln=k=>d3.line().x(d=>x(d[k])).y(d=>d.y).curve(d3.curveStepBefore)(PR);
svg.append("path").attr("d",ln("b")).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.4);
svg.append("path").attr("d",ln("w")).attr("fill","none").attr("stroke","#7F9BB3")
  .attr("stroke-width",2.4);
const yline=', sprintf("%.1f", T8 + (ryk[2] - mean(ryk)) * scm), ';
svg.append("line").attr("x1",PX).attr("x2",PX+PW).attr("y1",yline).attr("y2",yline)
  .attr("stroke","#111").attr("stroke-width",1.4).attr("stroke-dasharray","4,3");
svg.append("text").attr("x",PX+PW).attr("y",yline-6).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#111").text("the city limit");
svg.append("text").attr("x",PX).attr("y",16).attr("font-size","12.5px")
  .attr("font-weight","600").text("composition, by distance from it");
svg.append("text").attr("x",PX).attr("y",30).attr("font-size","10px")
  .attr("fill","#888")
  .text("', n(FV("profile_blocks")), ' blocks, ', n(FV("profile_pop")),
  ' people, whole length of the line");
svg.append("text").attr("x",PX+PW+6).attr("y",T+18).attr("font-size","10.5px")
  .attr("fill","#888").text("2.5 km");
svg.append("text").attr("x",PX+PW+6).attr("y",T+MH-8).attr("font-size","10.5px")
  .attr("fill","#888").text("2.5 km");
svg.append("text").attr("x",PX+8).attr("y",T+MH-10).attr("font-size","11px")
  .attr("fill","#C41230").attr("font-weight","600").text("Black");
svg.append("text").attr("x",PX+8).attr("y",T+22).attr("font-size","11px")
  .attr("fill","#7F9BB3").attr("font-weight","600").text("White");
})();
</script>'))

## ---- fig3-static
rxk <- range(wbk$x); ryk <- range(wbk$y)
m <- match(unique(wbk$id), wtx$GEOID)
layout(matrix(1:2, 1, 2), widths = c(1.25, 1))
op <- par(mar = c(2.6, 0.4, 2.6, 0.4))
ramp <- colorRampPalette(c("#f7f7f7", "#C41230"))(101)
pbv <- 100 * wtx$nh_black / ifelse(wtx$total == 0, 1, wtx$total)
pbv[wtx$total == 0] <- NA
fill <- as.list(ifelse(is.na(pbv[match(wtx$GEOID, wtx$GEOID)]), "#e8e8e8",
                       ramp[round(6 + 0.94 * pbv) + 1]))
names(fill) <- wtx$GEOID
plot(NA, xlim = rxk, ylim = ryk, asp = 1, axes = FALSE, ann = FALSE)
drawrings(wbk, fill, border = "#ffffff", lwd = 0.1)
for (p in unique(wdt$part)) {
  z <- wdt[wdt$part == p, ]
  lines(z$x, z$y, col = "#111111", lwd = 1.4)
}
title("blocks along Eight Mile Road", cex.main = 0.85, adj = 0, line = 1.2)
mtext("shading = share Black; black line = Detroit city limit", side = 3,
      line = 0.2, cex = 0.5, col = "#888", adj = 0)
# profile
pro$pw <- 100 * pro$nh_white / pro$total
pro$pb <- 100 * pro$nh_black / pro$total
par(mar = c(2.6, 2.6, 2.6, 1.6))
plot(NA, xlim = c(0, 100), ylim = c(-2.5, 2.5), axes = FALSE, ann = FALSE)
abline(v = seq(0, 100, 25), col = "#eeeeee")
lines(rep(pro$pb, each = 2), c(pro$mid[1] - 0.125,
      rep(pro$mid, each = 2)[-c(1, 2 * nrow(pro))],
      pro$mid[nrow(pro)] + 0.125), col = "#C41230", lwd = 2.2)
lines(rep(pro$pw, each = 2), c(pro$mid[1] - 0.125,
      rep(pro$mid, each = 2)[-c(1, 2 * nrow(pro))],
      pro$mid[nrow(pro)] + 0.125), col = "#7F9BB3", lwd = 2.2)
abline(h = 0, col = "#111111", lty = 2, lwd = 1.2)
axis(1, seq(0, 100, 25), paste0(seq(0, 100, 25), "%"), cex.axis = 0.55,
     col = "#dddddd", col.axis = "#aaa", tck = -0.015, mgp = c(2, 0.3, 0))
axis(2, c(-2.5, 0, 2.5), c("2.5 km south", "the city limit", "2.5 km north"),
     cex.axis = 0.5, col = "#dddddd", col.axis = "#888", tck = -0.01,
     mgp = c(2, 0.4, 0), las = 1)
text(70, -2.0, "Black", col = "#C41230", cex = 0.7, font = 2)
text(70, 2.0, "White", col = "#7F9BB3", cex = 0.7, font = 2)
title("composition, by distance from it", cex.main = 0.85, adj = 0, line = 1.2)
mtext(paste0(n(FV("profile_blocks")), " blocks, ", n(FV("profile_pop")),
             " people, whole length of the line"), side = 3, line = 0.2,
      cex = 0.5, col = "#888", adj = 0)
par(op); layout(1)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
