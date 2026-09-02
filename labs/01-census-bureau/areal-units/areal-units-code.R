# areal-units-code.R -- chunk bodies for areal-units-brief.Rmd
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

D  <- "data"
fc <- read.csv(file.path(D, "derived/facts.csv"),        stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("county_outline.csv", "dens_fulton.csv", "dots_fulton.csv", "facts.csv", "ga_block_race.csv", "ladder_fulton.csv", "ladder_state.csv", "map_units.csv", "map_vals.csv", "zoning_plan_geo.csv", "zoning_plan_units.csv", "zoning_summary.csv", "zoning_sweep.csv"))
ls_ga <- read.csv(file.path(D, "derived/ladder_state.csv"),  stringsAsFactors = FALSE)
lf <- read.csv(file.path(D, "derived/ladder_fulton.csv"), stringsAsFactors = FALSE)
mu <- read.csv(file.path(D, "derived/map_units.csv"),    stringsAsFactors = FALSE)
sm <- read.csv(file.path(D, "derived/zoning_summary.csv"), stringsAsFactors = FALSE)
sw <- read.csv(file.path(D, "derived/zoning_sweep.csv"), stringsAsFactors = FALSE)
pg <- read.csv(file.path(D, "derived/zoning_plan_geo.csv"),   stringsAsFactors = FALSE)
dt <- read.csv(file.path(D, "derived/dots_fulton.csv"),  stringsAsFactors = FALSE)
co <- read.csv(file.path(D, "derived/county_outline.csv"), stringsAsFactors = FALSE)

# GEOIDs are CHARACTER. Read as numbers, a 15-digit block id becomes
# 1.312101e+14 and a tract id loses its leading zero. Both files below carry
# one, so both are read with the column pinned.
mv <- read.csv(file.path(D, "derived/map_vals.csv"),
               colClasses = c(unit = "character"), stringsAsFactors = FALSE)
pu <- read.csv(file.path(D, "derived/zoning_plan_units.csv"), stringsAsFactors = FALSE)

# ---- every number in this document comes out of the built files -------------
FV <- function(k) {
  v <- fc$value[fc$name == k]
  if (!length(v)) stop("no such fact: ", k)
  v
}
FN <- function(k) as.numeric(FV(k))
n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
LG <- function(l, cc) ls_ga[[cc]][ls_ga$level == l]      # statewide ladder
LF <- function(l, cc) lf[[cc]][lf$level == l]            # Fulton ladder
LEVS <- lf$level                                          # block .. county

# the ramp the build used, rebuilt from the same call, so the legend in this
# document and the fills in the CSVs cannot drift apart
RAMP <- colorRampPalette(c("#f7f7f7", "#d9e6f2", "#8fbedd", "#3f8dc0",
                           "#1f5d8c", "#12395a"))(101)

# ---- geometry helpers -------------------------------------------------------
# One SVG path per ring. Three things keep the knitted HTML small enough to
# carry 10,000 polygons: INTEGER coordinates in a deliberately large viewBox
# (so the browser still has sub-pixel precision after rounding), removal of
# consecutive duplicate points, and RELATIVE line commands, which turn
# "L328,148" into "2,0". Together they cut this figure by about two thirds.
# A ring that rounds away to fewer than three distinct points -- 1,086 of
# Fulton's blocks do, holding 5% of its people -- is drawn as a one-unit
# square instead of dropped, so the HTML and the PDF draw the same units in
# the same colors rather than leaving gray specks where the small blocks are.
onepath <- function(X, Y) {
  dx <- X[-1] - X[-length(X)]; dy <- Y[-1] - Y[-length(Y)]
  seg <- paste0(dx, ifelse(dy < 0, "", ","), dy)   # "-2-3" needs no comma
  sep <- c("", ifelse(substr(seg[-1], 1, 1) == "-", "", " "))
  paste0("M", X[1], ",", Y[1], "l", paste0(sep, seg, collapse = ""), "Z")
}
ringpaths <- function(d, by, sx, sy) {
  k <- interaction(d[[by]], d$part, drop = TRUE)
  p <- vapply(split(d, k), function(z) {
    X <- round(sx(z$x)); Y <- round(sy(z$y))
    keep <- c(TRUE, X[-1] != X[-length(X)] | Y[-1] != Y[-length(Y)])
    X <- X[keep]; Y <- Y[keep]
    if (length(X) < 3) return(paste0("M", X[1], ",", Y[1], "h1v1h-1Z"))
    onepath(X, Y)
  }, character(1))
  p[nzchar(p)]
}
# a JSON array of quoted strings, and one of bare numbers
jstr <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")
jnum <- function(x) paste0("[", paste0(x, collapse = ","), "]")
# A dissolved plan unit comes back as one real polygon plus a few dozen rings
# of river bed and interstate median, each a fraction of a square kilometer.
# They are geometry, not districts, and they turn the boundary into a smear.
bigrings <- function(d, by, min_km2 = 2) {
  k <- interaction(d[[by]], d$part, drop = TRUE)
  a <- tapply(seq_len(nrow(d)), k, function(i)
    abs(sum(d$x[i] * c(d$y[i][-1], d$y[i][1]) -
            c(d$x[i][-1], d$x[i][1]) * d$y[i])) / 2)
  d[k %in% names(a)[a >= min_km2], ]
}
# The dissolved boundary staircases block by block, which at this size reads as
# a smear rather than a line. Keep a vertex only when it is at least `tol` km
# from the last one kept -- a radial-distance thin, the cheapest simplification
# there is, and enough because the staircase is the size of a city block.
smoothrings <- function(d, by, tol = 0.9) {
  k <- interaction(d[[by]], d$part, drop = TRUE)
  do.call(rbind, lapply(split(d, k), function(z) {
    keep <- logical(nrow(z)); keep[1] <- TRUE
    lx <- z$x[1]; ly <- z$y[1]
    for (i in seq_len(nrow(z))[-1]) {
      if ((z$x[i] - lx)^2 + (z$y[i] - ly)^2 >= tol^2) {
        keep[i] <- TRUE; lx <- z$x[i]; ly <- z$y[i]
      }
    }
    if (sum(keep) < 4) keep[] <- TRUE
    z[keep, ]
  }))
}
# draw one long-format polygon table with base R
drawrings <- function(d, by, fill, border = NA, lwd = 0.4) {
  k <- interaction(d[[by]], d$part, drop = TRUE)
  for (lv in levels(k)) {
    z <- d[k == lv, ]
    polygon(z$x, z$y, col = fill[[as.character(z[[by]][1])]],
            border = border, lwd = lwd)
  }
}
RX <- range(mu$x); RY <- range(mu$y)

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)                        # median_pop -> median pop
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)    # sentence case
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- p1-layout
# The same reconstruction the build does: every combination of the six race
# categories, in publication order, and the ones that include Black.
combos  <- unlist(lapply(1:6, function(k) combn(6, k, simplify = FALSE)),
                  recursive = FALSE)
n_cells <- length(combos)
n_black <- sum(vapply(combos, function(z) 2L %in% z, TRUE))

## ---- step1
data.frame(
  quantity = c("Census blocks in Georgia", "People in Georgia",
               "Georgians of any part Black",
               "Census blocks in Fulton County",
               "Fulton blocks with anyone living on them",
               "People in Fulton County",
               "Fulton residents of any part Black",
               "Fulton share, any part Black (%)",
               "Fulton share of voting-age population (%)"),
  value = c(n(FV("ga_blocks")), n(FV("ga_pop")), n(FV("ga_black_any")),
            n(FV("ful_blocks")), n(FV("ful_blocks_pop")), n(FV("ful_pop")),
            n(FV("ful_black")), pc(FV("ful_black_pct"), 2),
            pc(FV("ful_bvap_pct"), 2)))

## ---- scale-d3
PW <- 208L; GAP <- 10L; TOP <- 42L; BOTPAD <- 40L
s  <- PW / diff(RX)
PH <- round(diff(RY) * s)
W  <- 4L * PW + 3L * GAP
H  <- TOP + PH + BOTPAD
KEYS <- c("b", "g", "t", "c")

# Each panel ships as two parallel flat arrays -- the ring paths, and the index
# of each ring's color in the ramp. The index is looked up in the build's own
# hex strings, so the HTML and the PDF cannot end up painted differently.
# The county silhouette goes underneath every panel in the no-data gray, so a
# block with nobody on it reads as "nobody", not as a hole.
mkpanel <- function(i) {
  lv <- KEYS[i]
  off <- (i - 1L) * (PW + GAP)
  fx <- function(x) off + (x - RX[1]) * s
  fy <- function(y) TOP + (RY[2] - y) * s
  v  <- mv[mv$lev == lv, ]
  P  <- ringpaths(mu[mu$lev == lv, ], "uid", fx, fy)
  uid <- as.integer(sub("\\.[0-9]+$", "", names(P)))
  ci  <- match(v$fill[match(uid, v$uid)], RAMP) - 1L   # NA = the no-data gray
  ci[is.na(ci)] <- -1L
  base <- ringpaths(mu[mu$lev == "c", ], "uid", fx, fy)
  paste0("[", jstr(c(base, P)), ",", jnum(c(rep(-1L, length(base)), ci)), "]")
}
PAN <- vapply(seq_along(KEYS), mkpanel, character(1))

# titles, computed here so the two renderings cannot disagree
ttl <- LEVS
sub1 <- paste0(vapply(LEVS, function(l) n(LF(l, "units")), character(1)),
               ifelse(LEVS == "County", " unit", " units"))
sub2 <- paste0("median ", vapply(LEVS, function(l) n(LF(l, "median_pop")),
                                 character(1)), " people")
sub3 <- paste0("spread ", vapply(LEVS, function(l) pc(LF(l, "sd_share")),
                                 character(1)), " pts")
lab <- paste0("[", paste0('{"t":"', ttl, '","a":"', sub1, '","b":"', sub2,
                          '","c":"', sub3, '"}', collapse = ","), "]")
ramp <- paste0("[", paste0('"', RAMP, '"', collapse = ","), "]")

cat(paste0('
<div id="scl" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first figure above -->
<script>
(function(){
const PAN=[', paste(PAN, collapse = ","), '],L=', lab, ',R=', ramp, ';
const W=', W, ',H=', H, ',PW=', PW, ',GAP=', GAP, ',TOP=', TOP, ',PH=', PH, ';
const svg=d3.select("#scl").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const col=i=>i<0?"#e6e6e6":R[i];
PAN.forEach((P,i)=>{
  const off=i*(PW+GAP);
  const g=svg.append("g");
  g.selectAll("path").data(P[0]).join("path").attr("d",d=>d)
    .attr("fill",(d,j)=>col(P[1][j])).attr("stroke","none");
  svg.append("text").attr("x",off+PW/2).attr("y",15).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600").attr("fill","#222")
    .text(L[i].t);
  [[L[i].a,27],[L[i].b,37]].forEach(([s,y])=>
    svg.append("text").attr("x",off+PW/2).attr("y",y).attr("text-anchor","middle")
      .attr("font-size","9.5px").attr("fill","#777").text(s));
  svg.append("text").attr("x",off+PW/2).attr("y",TOP+PH+12).attr("text-anchor","middle")
    .attr("font-size","9.5px").attr("fill","#777").text(L[i].c);
});
// color key
const KW=210,KX=(W-KW)/2,KY=TOP+PH+21;
R.forEach((c,i)=>svg.append("rect").attr("x",KX+i*KW/101).attr("y",KY)
  .attr("width",KW/101+0.5).attr("height",7).attr("fill",c));
[[0,"0%"],[50,"50%"],[100,"100%"]].forEach(([v,t])=>
  svg.append("text").attr("x",KX+v*KW/100).attr("y",KY+8)
    .attr("text-anchor","middle").attr("dy","0.9em").attr("font-size","8px")
    .attr("fill","#888").text(t));
svg.append("text").attr("x",KX-8).attr("y",KY+6).attr("text-anchor","end")
  .attr("font-size","8.5px").attr("fill","#888").text("share of the unit that is Black");
})();
</script>
'))

## ---- scale-static
layout(matrix(c(1, 2, 3, 4, 5, 5, 5, 5), 2, 4, byrow = TRUE), heights = c(8, 1))
op <- par(mar = c(1.6, 0.2, 3.0, 0.2), xpd = NA)
for (i in seq_along(LEVS)) {
  lv <- c("b", "g", "t", "c")[i]
  d  <- mu[mu$lev == lv, ]
  v  <- mv[mv$lev == lv, ]
  fill <- as.list(v$fill); names(fill) <- as.character(v$uid)
  plot(NA, xlim = RX, ylim = RY, asp = 1, axes = FALSE, ann = FALSE)
  polygon(co$x, co$y, col = "#e6e6e6", border = NA)   # nobody-lives-here gray
  drawrings(d, "uid", fill, border = NA)
  title(LEVS[i], cex.main = 1.0, line = 1.9)
  mtext(paste0(n(LF(LEVS[i], "units")),
               if (LEVS[i] == "County") " unit" else " units"),
        side = 3, line = 0.95, cex = 0.55, col = "#777")
  mtext(paste0("median ", n(LF(LEVS[i], "median_pop")), " people"),
        side = 3, line = 0.25, cex = 0.55, col = "#777")
  mtext(paste0("spread ", pc(LF(LEVS[i], "sd_share")), " pts"),
        side = 1, line = 0.2, cex = 0.55, col = "#777")
}
par(mar = c(0, 0.2, 0, 0.2))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
bx <- seq(0.40, 0.62, length.out = 102)
rect(bx[-102], 0.42, bx[-1], 0.72, col = RAMP, border = NA)
text(c(0.40, 0.51, 0.62), 0.42, c("0%", "50%", "100%"), pos = 1,
     cex = 0.52, col = "#888", offset = 0.25)
text(0.385, 0.57, "share of the unit that is Black", pos = 2, cex = 0.52, col = "#888")
par(op)
layout(1)

## ---- ladder-state
o <- data.frame(
  level = ls_ga$level,
  units = n(ls_ga$units),
  `median people` = n(ls_ga$median_pop),
  `weighted SD (points)` = pc(ls_ga$sd_share),
  `most Black unit (%)` = pc(ls_ga$max_share),
  `corr with under-18 share` = pc(ls_ga$corr_child, 3),
  check.names = FALSE)
o

## ---- rules
data.frame(
  rule = c("Strip", "Split"),
  how = c(paste(FV("K"), "parallel slabs perpendicular to a chosen direction, cut so each slab holds a sixth of the county"),
          "Recursive bisection: halve the county, halve each half, halve again, turning 90 degrees each time"),
  `plans it makes` = c(paste(FV("n_angles"), "rotations"), paste(FV("n_angles"), "rotations")),
  check.names = FALSE)

## ---- zone-d3
ord <- order(sm$n_majority, sm$max_share)
key <- paste(sm$family[ord], sm$angle[ord])
swk <- paste(sw$family, sw$angle)
rows <- vapply(seq_along(key), function(i) {
  v <- sort(sw$share[swk == key[i]])
  paste0("[", i - 1L, ",", paste0(formatC(v, format = "f", digits = 1),
                                  collapse = ","), "]")
}, character(1))
i_lo <- which(key == paste(FV("lo_family"), FV("lo_angle"))) - 1L
i_hi <- which(key == paste(FV("hi_family"), FV("hi_angle"))) - 1L
lab_lo <- paste0(FV("lo_maj"), " majority-Black units · ", FV("lo_family"),
                 " rule at ", FV("lo_angle"), "°")
lab_hi <- paste0(FV("hi_maj"), " majority-Black units · ", FV("hi_family"),
                 " rule at ", FV("hi_angle"), "°")
brk <- cumsum(table(factor(sm$n_majority[ord], levels = sort(unique(sm$n_majority)))))
blab <- paste0("[", paste0('{"y":', as.integer(brk), ',"t":"',
        names(brk), ifelse(names(brk) == "1", " majority-Black unit",
                           " majority-Black units"), '","n":',
        as.integer(table(sm$n_majority)[names(brk)]), '}', collapse = ","), "]")

cat(paste0('
<div id="zon" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first figure above -->
<script>
(function(){
const P=[', paste(rows, collapse = ","), '],BR=', blab, ';
const NR=P.length,LO=', i_lo, ',HI=', i_hi, ';
const RH=1.55,L=228,R=32,T0=54,B=48;
const W=900,H=Math.round(T0+NR*RH+B);
const svg=d3.select("#zon").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([L,W-R]);
const yr=i=>T0+i*RH;
svg.append("rect").attr("x",x(50)).attr("y",T0-6).attr("width",W-R-x(50))
  .attr("height",NR*RH+6).attr("fill","#C41230").attr("fill-opacity",0.045);
BR.forEach(b=>{
  svg.append("line").attr("x1",L-6).attr("x2",W-R).attr("y1",yr(b.y)).attr("y2",yr(b.y))
    .attr("stroke","#ddd");
  svg.append("text").attr("x",L-12).attr("y",yr(b.y)-b.n*RH/2+4).attr("text-anchor","end")
    .attr("font-size","13px").attr("fill","#555")
    .text(b.n+" plans → "+b.t);});
const FLAT=[];P.forEach(p=>{for(let j=1;j<7;j++)FLAT.push([p[0],p[j]]);});
svg.append("g").selectAll("circle").data(FLAT).join("circle")
  .attr("cx",d=>x(d[1])).attr("cy",d=>yr(d[0])+RH/2).attr("r",1.7)
  .attr("fill",d=>d[1]>50?"#C41230":"#3f8dc0").attr("fill-opacity",0.55);
[[LO,"#111","', lab_lo, '"],[HI,"#111","', lab_hi, '"]].forEach(([i,c,t])=>{
  svg.append("line").attr("x1",L-6).attr("x2",W-R).attr("y1",yr(i)+RH/2)
    .attr("y2",yr(i)+RH/2).attr("stroke",c).attr("stroke-width",1);
  const p=P[i];
  for(let j=1;j<7;j++)
    svg.append("circle").attr("cx",x(p[j])).attr("cy",yr(i)+RH/2).attr("r",3.6)
      .attr("fill",p[j]>50?"#C41230":"#1f5d8c").attr("stroke","#fff").attr("stroke-width",1);
  svg.append("text").attr("x",W-R).attr("y",yr(i)-7).attr("text-anchor","end")
    .attr("font-size","13px").attr("font-weight","600").attr("fill","#111").text(t);});
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",T0-14)
  .attr("y2",T0+NR*RH).attr("stroke","#C41230").attr("stroke-width",1.4);
svg.append("text").attr("x",x(50)).attr("y",T0-20).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("fill","#C41230").text("50%");
d3.range(0,101,10).forEach(v=>svg.append("text").attr("x",x(v))
  .attr("y",T0+NR*RH+20).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#aaa").text(v+"%"));
svg.append("text").attr("x",L).attr("y",T0+NR*RH+40).attr("font-size","13px")
  .attr("fill","#888").text("Black share of a unit · every unit of every plan · "
   +(6*NR).toLocaleString()+" units in "+NR+" plans");
})();
</script>
'))

## ---- zone-static
ord <- order(sm$n_majority, sm$max_share)
key <- paste(sm$family[ord], sm$angle[ord])
swk <- paste(sw$family, sw$angle)
par(mar = c(3.6, 9.6, 2.6, 1.0))
plot(NA, xlim = c(0, 100), ylim = c(nrow(sm), 0), axes = FALSE, ann = FALSE)
rect(50, 0, 100, nrow(sm), col = adjustcolor("#C41230", 0.045), border = NA)
for (i in seq_along(key)) {
  v <- sw$share[swk == key[i]]
  points(v, rep(i, length(v)), pch = 19, cex = 0.24,
         col = adjustcolor(ifelse(v > 50, "#C41230", "#3f8dc0"), 0.55))
}
brk <- cumsum(table(factor(sm$n_majority[ord], levels = sort(unique(sm$n_majority)))))
cnt <- as.integer(table(sm$n_majority)[names(brk)])
segments(0, brk, 100, brk, col = "#dddddd")
text(-2, brk - cnt / 2, paste0(cnt, " plans -> ", names(brk),
     ifelse(names(brk) == "1", " majority-Black unit", " majority-Black units")),
     adj = 1, cex = 0.6, col = "#555", xpd = NA)
abline(v = 50, col = "#C41230", lwd = 1.4)
for (tg in c("lo", "hi")) {
  i <- which(key == paste(FV(paste0(tg, "_family")), FV(paste0(tg, "_angle"))))
  v <- sw$share[swk == key[i]]
  segments(0, i, 100, i, col = "#111111", lwd = 0.7)
  points(v, rep(i, length(v)), pch = 21, cex = 0.62, lwd = 0.5,
         bg = ifelse(v > 50, "#C41230", "#1f5d8c"), col = "white")
  lb <- paste0(FV(paste0(tg, "_maj")), " majority-Black units - ",
               FV(paste0(tg, "_family")), " rule at ",
               FV(paste0(tg, "_angle")), " deg")
  yy <- if (tg == "lo") i - 5 else i - 6
  rect(100 - strwidth(lb, cex = 0.58) - 1, yy - strheight(lb, cex = 0.58) * 0.9,
       100.5, yy + strheight(lb, cex = 0.58) * 0.9, col = "white", border = NA)
  text(100, yy, lb, adj = 1, cex = 0.58, font = 2, col = "#111111")
}
axis(1, seq(0, 100, 10), paste0(seq(0, 100, 10), "%"), cex.axis = 0.6,
     col = "#ddd", col.axis = "#999", tck = -0.012, mgp = c(2, 0.3, 0))
mtext(sprintf("Black share of a unit - every unit of every plan - %s units in %d plans",
              n(6 * nrow(sm)), nrow(sm)), side = 1, line = 1.9, cex = 0.6, col = "#888")
title(sprintf("%s equal-population partitions of the same %s blocks",
              n(nrow(sm)), n(FV("ful_blocks"))), cex.main = 0.9, adj = 0, line = 0.8)

## ---- plans-d3
PW <- 236L; GAP <- 26L; TOP <- 44L; BOT <- 50L
s  <- PW / diff(RX)
PH <- round(diff(RY) * s)
W  <- 2L * PW + GAP
H  <- TOP + PH + BOT
TAGS <- c("fewest", "most")
# boundaries: one panel each, in that panel's own coordinates
mkp <- function(i) {
  off <- (i - 1L) * (PW + GAP)
  jstr(ringpaths(smoothrings(bigrings(pg[pg$plan == TAGS[i], ], "unit"), "unit"), "unit",
                 function(x) off + (x - RX[1]) * s,
                 function(y) TOP + (RY[2] - y) * s))
}
# dots: the SAME census in both panels, so they are sent once, in panel-0
# coordinates, and the second panel just shifts them
mkd <- function(black) {
  z <- dt[(dt$kind == "Black") == black, ]
  jnum(as.vector(rbind(round((z$x - RX[1]) * s), round(TOP + (RY[2] - z$y) * s))))
}
mklab <- function(tag) jstr(paste0(pc(pu$share[pu$plan == tag], 1), "%"))
hdr <- vapply(c("lo", "hi"), function(tg) paste0(
  '{"a":"', FV(paste0(tg, "_family")), " rule at ", FV(paste0(tg, "_angle")),
  '°","b":"', FV(paste0(tg, "_maj")), " of ", FV("K"),
  ' units majority-Black","c":"units run ', pc(FV(paste0(tg, "_min"))), "% to ",
  pc(FV(paste0(tg, "_max"))), '%"}'), character(1))

cat(paste0('
<div id="pln" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const B=[', mkp(1), ',', mkp(2), '],BK=', mkd(TRUE), ',OT=', mkd(FALSE), ';
const SH=[', mklab("fewest"), ',', mklab("most"), '],HD=[',
  paste(hdr, collapse = ","), '];
const W=', W, ',H=', H, ',PW=', PW, ',GAP=', GAP, ',TOP=', TOP, ',PH=', PH, ';
const svg=d3.select("#pln").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const dots=(g,A,off,c,o)=>{const n=A.length/2,I=d3.range(n);
  g.selectAll("circle").data(I).join("circle").attr("cx",k=>A[2*k]+off)
   .attr("cy",k=>A[2*k+1]).attr("r",1.2).attr("fill",c).attr("fill-opacity",o);};
[0,1].forEach(i=>{
  const off=i*(PW+GAP);
  svg.append("text").attr("x",off+PW/2).attr("y",16).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600").attr("fill","#222").text(HD[i].a);
  svg.append("text").attr("x",off+PW/2).attr("y",29).attr("text-anchor","middle")
    .attr("font-size","11px").attr("font-weight","600")
    .attr("fill",i?"#C41230":"#1f5d8c").text(HD[i].b);
  svg.append("text").attr("x",off+PW/2).attr("y",40).attr("text-anchor","middle")
    .attr("font-size","9px").attr("fill","#777").text(HD[i].c);
  dots(svg.append("g"),OT,off,"#9aa7b0",0.5);
  dots(svg.append("g"),BK,off,"#C41230",0.8);
  svg.append("g").selectAll("path").data(B[i]).join("path").attr("d",d=>d)
    .attr("fill","none").attr("stroke","#111").attr("stroke-width",0.9)
    .attr("stroke-opacity",0.85);
  svg.append("text").attr("x",off+PW/2).attr("y",TOP+PH+15).attr("text-anchor","middle")
    .attr("font-size","9px").attr("fill","#444")
    .text("units: "+SH[i].join("  ·  "));
});
const lg=svg.append("g").attr("transform","translate("+(W/2-95)+","+(TOP+PH+32)+")");
[["#C41230","any part Black"],["#9aa7b0","everyone else"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",i*110).attr("cy",-4).attr("r",2.8).attr("fill",r[0]);
  lg.append("text").attr("x",i*110+7).attr("y",0).attr("font-size","9px")
    .attr("fill","#666").text(r[1]);});
})();
</script>
'))

## ---- plans-static
op <- par(mfrow = c(1, 2), mar = c(3.0, 0.2, 3.4, 0.2), xpd = NA)
for (i in 1:2) {
  tg  <- c("lo", "hi")[i]
  tag <- c("fewest", "most")[i]
  plot(NA, xlim = RX, ylim = RY, asp = 1, axes = FALSE, ann = FALSE)
  points(dt$x, dt$y, pch = 19, cex = 0.16,
         col = ifelse(dt$kind == "Black", adjustcolor("#C41230", 0.8),
                      adjustcolor("#9aa7b0", 0.5)))
  z <- smoothrings(bigrings(pg[pg$plan == tag, ], "unit"), "unit")
  k <- interaction(z$unit, z$part, drop = TRUE)
  for (lv in levels(k)) {
    q <- z[k == lv, ]
    polygon(q$x, q$y, col = NA, border = adjustcolor("#111111", 0.85), lwd = 0.6)
  }
  title(paste0(FV(paste0(tg, "_family")), " rule at ",
               FV(paste0(tg, "_angle")), " deg"), cex.main = 0.92, line = 2.3)
  mtext(paste0(FV(paste0(tg, "_maj")), " of ", FV("K"), " units majority-Black"),
        side = 3, line = 1.25, cex = 0.62, font = 2,
        col = if (i == 2) "#C41230" else "#1f5d8c")
  mtext(paste0("units run ", pc(FV(paste0(tg, "_min"))), "% to ",
               pc(FV(paste0(tg, "_max"))), "%"),
        side = 3, line = 0.35, cex = 0.55, col = "#777")
  mtext(paste(pc(pu$share[pu$plan == tag]), collapse = "  "),
        side = 1, line = 0.4, cex = 0.52, col = "#444")
}
par(op)
legend("bottom", c("any part Black", "everyone else"), pch = 19, horiz = TRUE,
       col = c("#C41230", "#9aa7b0"), bty = "n", cex = 0.6, pt.cex = 0.8,
       inset = c(0, -0.02))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: on the dark page the fill is lifted and already passes,
# and a --paper stroke would sit dark behind a dark ink there, because the
# checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#zon text[fill="#111" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
