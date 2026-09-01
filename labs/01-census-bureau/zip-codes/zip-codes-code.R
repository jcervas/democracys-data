# zip-codes-code.R -- chunk bodies for zip-codes-brief.Rmd
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
# Every five-digit code in this document is CHARACTER. Read as a number, 00601
# becomes 601 and 09096 becomes 9096, and the two codes this chapter opens and
# closes with would both stop existing. This is a lab about a five-digit string
# that is not a quantity; getting that wrong here would be a special kind of
# failure.
rd <- function(f, ...) read.csv(file.path(D, "derived", f), stringsAsFactors = FALSE, ...)
lists  <- rd("lists.csv")
dr     <- rd("digit_rows.csv",  colClasses = c(d1 = "character"))
scf    <- rd("scf.csv",         colClasses = c(scf = "character", d1 = "character"))
ng     <- rd("no_ground.csv",   colClasses = c(code = "character"))
ngk    <- rd("no_ground_kind.csv")
nest   <- rd("nesting.csv")
churn  <- rd("churn.csv")
cities <- rd("cities.csv",      colClasses = c(place = "character"))
cbig   <- rd("cities_big.csv",  colClasses = c(place = "character"))
pas    <- rd("pa_split.csv")
pr     <- rd("pgh_rings.csv")
pid    <- rd("pgh_ids.csv",     colClasses = c(code = "character"))
pbl    <- rd("pgh_blocks.csv",  colClasses = c(code = "character"))
zvz    <- rd("zip_vs_zcta.csv")
chk    <- rd("checks.csv")
fv     <- rd("facts.csv")

FV <- function(k) fv$value[fv$name == k]
FN <- function(k) as.numeric(FV(k))
n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
# Counts in the tables get thousands separators. table_align() sends short
# character columns right, so the alignment survives the conversion.
fmt <- function(d, cols) { for (k in cols) d[[k]] <- n(d[[k]]); d }
CI <- function(nm, col, tol = "1%") cities[[col]][cities$name == nm &
                                                 cities$threshold == tol]
PS <- function(col, tol = "2%") pas[[col]][pas$threshold == tol]
NS <- function(col, tol = "0%") nest[[col]][nest$threshold == tol]
CH <- function(k) churn$count[churn$what == k]

# ---- the ten colours -------------------------------------------------------
# The leading digit is ORDINAL, not categorical: 0 is the north-east corner and
# 9 is the Pacific, and the whole point of the figure is that neighbouring
# digits are neighbours on the ground. So the ramp is sequential and its
# lightness falls monotonically from 0 to 9 rather than being ten unrelated
# hues. Identity never rests on colour alone: each band carries its digit
# drawn on it, and every mark has a dark ring so the pale end still reads.
DIG <- c("#C6E9B3", "#99D6B8", "#6DC5BE", "#40B6C4", "#2C9DC1",
         "#227FB8", "#215EA7", "#26429A", "#1B2C7F", "#071D58")
ACC <- "#C41230"     # the house accent, used only for the thing under discussion
GRY <- "#8a8a8a"

# ---- geometry helpers ------------------------------------------------------
# The same approach the mapped chapters use: one SVG path per ring, integer
# pixel coordinates, relative line commands. `featpath` concatenates all the
# rings of one unit into a single path so that a polygon with holes in it can
# be filled with the even-odd rule -- which is the entire subject of Figure 2.
onepath <- function(X, Y) {
  dx <- X[-1] - X[-length(X)]; dy <- Y[-1] - Y[-length(Y)]
  seg <- paste0(dx, ifelse(dy < 0, "", ","), dy)
  sep <- c("", ifelse(substr(seg[-1], 1, 1) == "-", "", " "))
  paste0("M", X[1], ",", Y[1], "l", paste0(sep, seg, collapse = ""), "Z")
}
.ring <- function(z, sx, sy) {
  X <- round(sx(z$x)); Y <- round(sy(z$y))
  keep <- c(TRUE, X[-1] != X[-length(X)] | Y[-1] != Y[-length(Y)])
  X <- X[keep]; Y <- Y[keep]
  if (length(X) < 3) return("")
  onepath(X, Y)
}
featpaths <- function(d, sx, sy) {
  if (!nrow(d)) return(character(0))
  p <- vapply(split(d, d$uid), function(u)
    paste(vapply(split(u, u$part), .ring, character(1), sx = sx, sy = sy),
          collapse = ""), character(1))
  p[nzchar(p)]
}
jstr <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")

# rows of pgh_rings.csv for a level, optionally for particular units
G <- function(lev, uid = NULL) {
  d <- pr[pr$lev == lev, ]
  if (!is.null(uid)) d <- d[d$uid %in% uid, ]
  d
}
Z <- function(code) pid$uid[pid$lev == "z" & pid$code %in% code]
win <- function(d, pad = 0.06) {
  rx <- range(d$x); ry <- range(d$y)
  s <- max(diff(rx), diff(ry)) * (1 + pad) / 2
  list(x = mean(rx) + c(-s, s), y = mean(ry) + c(-s, s), s = 2 * s)
}
# Base R has no even-odd fill. It does not need one here: ZCTAs tile without
# overlapping, so an enclosed area drawn after the one enclosing it produces
# the same picture. Largest first, and the holes fill themselves.
drawz <- function(uids, col, border = "#9a9a9a", lwd = 0.6) {
  pz <- pid[pid$lev == "z", ]
  a <- pz$sq_km[match(uids, pz$uid)]
  for (u in uids[order(-a)]) {
    d <- G("z", u)
    for (p in split(d, d$part))
      polygon(p$x, p$y, col = if (length(col) > 1) col[match(u, uids)] else col,
              border = border, lwd = lwd)
  }
}

# ---- render every data.frame as a TABLE, not code output -------------------
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- lists
fmt(lists, "count")

## ---- digits-d3
W <- 720L; H <- 430L; PADT <- 16L; PADB <- 46L
f <- scf[scf$in_frame == 1, ]
sx <- function(x) (x - min(f$x)) / diff(range(f$x)) * (W - 24) + 12
sy <- function(y) PADT + (max(f$y) - y) / diff(range(f$y)) * (H - PADT - PADB)
f$px <- round(sx(f$x), 1); f$py <- round(sy(f$y), 1)
f$r  <- round(1.6 + 2.6 * sqrt(f$zctas / max(f$zctas)), 2)
# the digit label goes at the median point of its own band
lab <- do.call(rbind, lapply(split(f, f$d1), function(s) data.frame(
  d1 = s$d1[1], x = round(median(s$px), 1), y = round(median(s$py), 1))))
PTS <- paste0("[", paste(sprintf('[%s,%s,%s,%s]', f$px, f$py, f$r, f$d1),
                         collapse = ","), "]")
cat(paste0('
<div id="dg" style="margin:1.1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const P=', PTS, ';
const L=', paste0("[", paste(sprintf('[%s,%s,"%s"]', lab$x, lab$y, lab$d1),
  collapse = ","), "]"), ';
const C=', jstr(DIG), ',W=', W, ',H=', H, ';
const svg=d3.select("#dg").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
svg.selectAll("circle.p").data(P).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",d=>d[2])
  .attr("fill",d=>C[d[3]]).attr("stroke","#33333366").attr("stroke-width",0.5);
svg.selectAll("text.l").data(L).join("text")
  .attr("x",d=>d[0]).attr("y",d=>d[1]+9).attr("text-anchor","middle")
  .attr("font-size","25px").attr("font-weight","700").attr("fill","#ffffff")
  .attr("stroke","#333333").attr("stroke-width",2.6)
  .attr("paint-order","stroke").text(d=>d[2]);
const lx=W/2-155, ly=H-26;
svg.append("text").attr("x",lx-6).attr("y",ly+10).attr("text-anchor","end")
  .attr("font-size","10px").attr("fill","#777").text("first digit");
C.forEach((c,i)=>{
  svg.append("rect").attr("x",lx+i*31).attr("y",ly).attr("width",29)
    .attr("height",12).attr("fill",c).attr("stroke","#33333366")
    .attr("stroke-width",0.5);
  svg.append("text").attr("x",lx+i*31+14.5).attr("y",ly+23)
    .attr("text-anchor","middle").attr("font-size","10px").attr("fill","#777")
    .text(i);
});
})();
</script>
'))

## ---- digits-static
f <- scf[scf$in_frame == 1, ]
RX <- range(f$x); RY <- range(f$y)
op <- par(mar = rep(0.4, 4))
plot(NA, xlim = RX, ylim = RY, asp = 1, axes = FALSE, ann = FALSE)
points(f$x, f$y, pch = 21, bg = DIG[as.integer(f$d1) + 1], col = "#33333366",
       lwd = 0.4, cex = 0.5 + 1.1 * sqrt(f$zctas / max(f$zctas)))
# A white halo, drawn as eight offset copies, so the digit reads over both ends
# of the ramp. Base R has no paint-order.
halo <- function(x, y, s, cex) {
  d <- diff(RX) / 320
  for (i in 0:7) text(x + d * cos(i * pi / 4), y + d * sin(i * pi / 4), s,
                      cex = cex, font = 2, col = "white")
  text(x, y, s, cex = cex, font = 2, col = "#333333")
}
for (s in split(f, f$d1)) halo(median(s$x), median(s$y), s$d1[1], 1.9)
# The legend sits in the empty south-west of the frame -- ocean and northern
# Mexico -- rather than under the panel, where the margin would have to grow.
bw <- diff(RX) / 34; bh <- diff(RY) / 26
x0 <- RX[1] + diff(RX) * 0.02; y0 <- RY[1] + diff(RY) * 0.03
rect(x0 + (0:9) * bw, y0, x0 + (1:10) * bw, y0 + bh, col = DIG,
     border = "#33333366", lwd = 0.4)
text(x0 + (0:9 + 0.5) * bw, y0 - bh * 0.55, 0:9, cex = 0.6, col = "#777")
text(x0, y0 + bh * 1.7, "first digit", adj = 0, cex = 0.6, col = "#777")
par(op)

## ---- no-ground
o <- ng[, c("code", "listed_as", "note")]
names(o) <- c("ZIP code", "listed as", "what it actually is")
o

## ---- zip-vs-zcta
o <- zvz[, c("question", "zip", "zcta")]
names(o) <- c("", "ZIP code", "ZCTA")
o

## ---- pgh-d3
A <- Z("15213"); B <- Z("15260")
w <- win(G("z", c(A, B)), pad = 0.28)
PW <- 620L; PH <- 380L
s  <- min((PW - 20) / diff(w$x), (PH - 20) / diff(w$y))
fx <- function(x) 10 + (x - w$x[1]) * s
fy <- function(y) 10 + (w$y[2] - y) * s
near <- unique(pr$uid[pr$lev == "z" &
  pr$x > w$x[1] - 1 & pr$x < w$x[2] + 1 &
  pr$y > w$y[1] - 1 & pr$y < w$y[2] + 1])
oth <- setdiff(near, c(A, B))
ctr <- function(u) { d <- G("z", u); c(mean(range(d$x)), mean(range(d$y))) }
# 15260 sits at the centre of 15213, so the two labels would land on top of each
# other. 15213 is labelled at the top of its own outline; 15260 is labelled off
# to the side with a leader, because the shape itself is too small to hold text.
cA <- ctr(A); cB <- ctr(B)
dA <- G("z", A)
LA <- c(round(fx(cA[1]), 1), round(fy(max(dA$y)) + 26, 1))
LB <- c(round(fx(cB[1]) + 96, 1), round(fy(cB[2]) + 5, 1))
LEAD <- c(round(fx(cB[1]) + 12, 1), round(fy(cB[2]), 1),
          round(fx(cB[1]) + 84, 1), round(fy(cB[2]), 1))
cat(paste0('
<div id="pg" style="margin:1.1em 0"></div>
<script>
(function(){
const O=', jstr(featpaths(G("z", oth), fx, fy)), ';
const A=', jstr(featpaths(G("z", A), fx, fy)), ';
const B=', jstr(featpaths(G("z", B), fx, fy)), ';
const W=', PW, ',H=', PH, ';
const svg=d3.select("#pg").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const clip=svg.append("defs").append("clipPath").attr("id","pgc");
clip.append("rect").attr("x",0).attr("y",0).attr("width",W).attr("height",H);
const g=svg.append("g").attr("clip-path","url(#pgc)");
g.selectAll("path.o").data(O).join("path").attr("d",d=>d)
  .attr("fill","#f2f2f2").attr("fill-rule","evenodd")
  .attr("stroke","#c8c8c8").attr("stroke-width",0.8);
g.selectAll("path.a").data(A).join("path").attr("d",d=>d)
  .attr("fill","#dce8f2").attr("fill-rule","evenodd")
  .attr("stroke","#2c7fb8").attr("stroke-width",1.6);
g.selectAll("path.b").data(B).join("path").attr("d",d=>d)
  .attr("fill","', ACC, '").attr("fill-rule","evenodd")
  .attr("stroke","', ACC, '").attr("stroke-width",1.2);
svg.append("line").attr("x1",', LEAD[1], ').attr("y1",', LEAD[2],
  ').attr("x2",', LEAD[3], ').attr("y2",', LEAD[4], ')
  .attr("stroke","', ACC, '").attr("stroke-width",1);
const T=[[', paste(LA, collapse = ","), ',"15213","#2c7fb8","middle",15],
         [', paste(LB, collapse = ","), ',"15260","', ACC, '","start",13]];
svg.selectAll("text.t").data(T).join("text")
  .attr("x",d=>d[0]).attr("y",d=>d[1]).attr("text-anchor",d=>d[4])
  .attr("font-size",d=>d[5]+"px").attr("font-weight","700").attr("fill",d=>d[3])
  .attr("class","halo")
  .text(d=>d[2]);
})();
</script>
'))

## ---- pgh-static
A <- Z("15213"); B <- Z("15260")
w <- win(G("z", c(A, B)), pad = 0.28)
near <- unique(pr$uid[pr$lev == "z" &
  pr$x > w$x[1] - 1 & pr$x < w$x[2] + 1 &
  pr$y > w$y[1] - 1 & pr$y < w$y[2] + 1])
op <- par(mar = rep(0.3, 4))
plot(NA, xlim = w$x, ylim = w$y, asp = 1, axes = FALSE, ann = FALSE)
drawz(setdiff(near, c(A, B)), "#f2f2f2", "#c8c8c8", 0.7)
drawz(A, "#dce8f2", "#2c7fb8", 1.4)
drawz(B, ACC, ACC, 1.1)
# 15260 sits at the centre of 15213, so one label is placed above the outline
# and the other led out to the side.
dA <- G("z", A); dB <- G("z", B)
cB <- c(mean(range(dB$x)), mean(range(dB$y)))
text(mean(range(dA$x)), max(dA$y) + diff(w$y) * 0.035, "15213",
     cex = 0.95, font = 2, col = "#2c7fb8")
segments(cB[1] + diff(w$x) * 0.02, cB[2], cB[1] + diff(w$x) * 0.15, cB[2],
         col = ACC, lwd = 0.9)
text(cB[1] + diff(w$x) * 0.16, cB[2], "15260", adj = 0, cex = 0.9, font = 2,
     col = ACC)
par(op)

## ---- nesting
o <- fmt(nest[, c("threshold", "in_two_or_more_counties", "pct_multi_county",
                  "in_two_or_more_states", "pct_pop_multi_county")],
         c("in_two_or_more_counties", "in_two_or_more_states"))
names(o) <- c("counted above", "in 2+ counties", "% of ZIP areas",
              "in 2+ states", "% of people")
o

## ---- pgh-city-d3
CU <- pid$uid[pid$lev == "c"]
w <- win(rbind(G("c"), G("z", pid$uid[pid$lev == "z" &
                                      pid$share_in_city >= 0.01])), pad = 0.05)
PW <- 640L; PH <- 430L
s  <- min((PW - 16) / diff(w$x), (PH - 40) / diff(w$y))
fx <- function(x) 8 + (x - w$x[1]) * s
fy <- function(y) 8 + (w$y[2] - y) * s
zu <- pid$uid[pid$lev == "z"]
sh <- pid$share_in_city[pid$lev == "z"]
# three states, and the middle one is the point: wholly inside, partly inside,
# outside. Colour carries it and the caption names the counts.
grp <- ifelse(sh > 0.9999, 2L, ifelse(sh >= 0.01, 1L, 0L))
CLS <- c("#f4f4f4", "#f6d7dd", "#a9cbe4")
paths <- lapply(0:2, function(k) featpaths(G("z", zu[grp == k]), fx, fy))
cat(paste0('
<div id="pc" style="margin:1.1em 0"></div>
<script>
(function(){
const P=[', paste(vapply(paths, jstr, character(1)), collapse = ","), '];
const CT=', jstr(featpaths(G("c", CU), fx, fy)), ';
const C=', jstr(CLS), ',W=', PW, ',H=', PH, ';
const svg=d3.select("#pc").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
P.forEach((set,k)=>{
  svg.selectAll("path.z"+k).data(set).join("path").attr("d",d=>d)
    .attr("fill",C[k]).attr("fill-rule","evenodd")
    .attr("stroke","#b9b9b9").attr("stroke-width",0.7);
});
svg.selectAll("path.c").data(CT).join("path").attr("d",d=>d)
  .attr("fill","none").attr("fill-rule","evenodd")
  .attr("stroke","', ACC, '").attr("stroke-width",2.2);
const K=[["', ACC, '","the city limits",1],[C[2],"ZIP area wholly inside",0],
         [C[1],"ZIP area partly inside",0],[C[0],"ZIP area outside",0]];
K.forEach((k,i)=>{
  const x=14+i*158, y=H-18;
  if(k[2]) svg.append("line").attr("x1",x).attr("y1",y+6).attr("x2",x+12)
    .attr("y2",y+6).attr("stroke",k[0]).attr("stroke-width",2.2);
  else svg.append("rect").attr("x",x).attr("y",y).attr("width",12)
    .attr("height",12).attr("fill",k[0]).attr("stroke","#b9b9b9")
    .attr("stroke-width",0.7);
  svg.append("text").attr("x",x+17).attr("y",y+10).attr("font-size","10.5px")
    .attr("fill","#777").text(k[1]);
});
})();
</script>
'))

## ---- pgh-city-static
CU <- pid$uid[pid$lev == "c"]
w <- win(rbind(G("c"), G("z", pid$uid[pid$lev == "z" &
                                      pid$share_in_city >= 0.01])), pad = 0.05)
zu <- pid$uid[pid$lev == "z"]
sh <- pid$share_in_city[match(zu, pid$uid[pid$lev == "z"])]
grp <- ifelse(sh > 0.9999, 2L, ifelse(sh >= 0.01, 1L, 0L))
CLS <- c("#f4f4f4", "#f6d7dd", "#a9cbe4")
op <- par(mar = c(2.0, 0.3, 0.3, 0.3))
plot(NA, xlim = w$x, ylim = w$y, asp = 1, axes = FALSE, ann = FALSE)
for (k in 0:2) drawz(zu[grp == k], CLS[k + 1], "#b9b9b9", 0.6)
for (p in split(G("c", CU), G("c", CU)$part))
  polygon(p$x, p$y, border = ACC, lwd = 1.8)
# Drawn by hand rather than through legend(), which cannot put a line key and
# three fill keys in one horizontal row without leaving empty boxes behind.
kx <- w$x[1] + diff(w$x) * c(0.02, 0.28, 0.53, 0.78)
ky <- w$y[1] - diff(w$y) * 0.035
kw <- diff(w$x) * 0.025; kh <- diff(w$y) * 0.018
segments(kx[1], ky + kh / 2, kx[1] + kw, ky + kh / 2, col = ACC, lwd = 1.8,
         xpd = NA)
rect(kx[-1], ky, kx[-1] + kw, ky + kh, col = CLS[3:1], border = "#b9b9b9",
     lwd = 0.6, xpd = NA)
text(kx + kw * 1.4, ky + kh / 2, adj = 0, cex = 0.62, col = "#777", xpd = NA,
     labels = c("city limits", "wholly inside", "partly inside", "outside"))
par(op)

## ---- pa-split
o <- fmt(pas[, c("threshold", "zip_areas", "split", "pct_split",
                 "pct_pop_in_split", "most_districts")],
         c("zip_areas", "split"))
names(o) <- c("counted above", "PA ZIP areas", "split", "% of areas",
              "% of people", "most districts")
o

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#pg text[fill="#2c7fb8" i],
#pg text[fill="#c41230" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
