# census-geography-code.R -- chunk bodies for census-geography-brief.Rmd
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
lv <- read.csv(file.path(D, "derived/levels.csv"),  stringsAsFactors = FALSE)
ne <- read.csv(file.path(D, "derived/nesting.csv"), stringsAsFactors = FALSE)
lv$nests <- lv$nests_into %in% c("state", "county")
g  <- function(x, v) lv[[v]][lv$geography == x]
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
n  <- function(x) format(as.numeric(x), big.mark = ",")

# ---- the mapped county ------------------------------------------------------
# GEOIDs are CHARACTER, always. Read as numbers, the 15-digit block identifier
# this whole document is about becomes 1.3153e+14, and every tract in a state
# whose code begins with a zero silently loses that zero. This is the lab about
# identifiers; getting it wrong here would be a special kind of failure.
CH <- c(geoid = "character", digits = "character", lev = "character")
fc  <- read.csv(file.path(D, "derived/facts.csv"),      colClasses = c(value = "character"))
sp  <- read.csv(file.path(D, "derived/splits.csv"),     stringsAsFactors = FALSE)
ch  <- read.csv(file.path(D, "derived/chain.csv"),      colClasses = CH)
mu  <- read.csv(file.path(D, "derived/map_units.csv"),  stringsAsFactors = FALSE)
mi  <- read.csv(file.path(D, "derived/map_ids.csv"),    colClasses = c(geoid = "character"))
tsp <- read.csv(file.path(D, "derived/tract_split.csv"), colClasses = c(geoid = "character"))

FV <- function(k) {
  v <- fc$value[fc$name == k]
  if (!length(v)) stop("no such fact: ", k)
  v
}
FN <- function(k) as.numeric(FV(k))
SP <- function(o, cc) sp[[cc]][sp$overlay == o]
CHV <- function(l, cc) ch[[cc]][ch$level == l]

# The five levels of the spine, with the color each keeps in every figure.
LEVN <- c("state", "county", "tract", "block group", "block")
LEVK <- c("s", "c", "t", "g", "b")
CL   <- c("#555555", "#2c7fb8", "#4d9221", "#e08214", "#C41230")

# ---- geometry helpers -------------------------------------------------------
# The same approach the areal-units lab uses, for the same reason: one SVG path
# per ring, integer coordinates, and RELATIVE line commands, which turn
# "L328,148" into "2,0". Rounding to whole pixels is what keeps 3,269 block
# outlines inside a document of ordinary size.
onepath <- function(X, Y) {
  dx <- X[-1] - X[-length(X)]; dy <- Y[-1] - Y[-length(Y)]
  seg <- paste0(dx, ifelse(dy < 0, "", ","), dy)
  sep <- c("", ifelse(substr(seg[-1], 1, 1) == "-", "", " "))
  paste0("M", X[1], ",", Y[1], "l", paste0(sep, seg, collapse = ""), "Z")
}
ringpaths <- function(d, sx, sy) {
  if (!nrow(d)) return(character(0))
  k <- interaction(d$uid, d$part, drop = TRUE)
  p <- vapply(split(d, k), function(z) {
    X <- round(sx(z$x)); Y <- round(sy(z$y))
    keep <- c(TRUE, X[-1] != X[-length(X)] | Y[-1] != Y[-length(Y)])
    X <- X[keep]; Y <- Y[keep]
    if (length(X) < 3) return(paste0("M", X[1], ",", Y[1], "h1v1h-1Z"))
    onepath(X, Y)
  }, character(1))
  p[nzchar(p)]
}
drawrings <- function(d, col = NA, border = "#999", lwd = 0.4) {
  if (!nrow(d)) return(invisible())
  k <- interaction(d$uid, d$part, drop = TRUE)
  for (l in levels(k)) {
    z <- d[k == l, ]
    polygon(z$x, z$y, col = col, border = border, lwd = lwd)
  }
}
jstr <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")

# rows of map_units.csv for a level, optionally for particular units
G <- function(lev, uid = NULL) {
  d <- mu[mu$lev == lev, ]
  if (!is.null(uid)) d <- d[d$uid %in% uid, ]
  d
}
# the units of `lev` inside a given ancestor, found by GEOID PREFIX -- the same
# operation the whole lab is about, done here to decide what to draw
kids <- function(lev, parent_geoid) {
  k <- mi[mi$lev == lev, ]
  k$uid[substr(k$geoid, 1, nchar(parent_geoid)) == parent_geoid]
}
# a square window on a set of rings, so every panel keeps the true shape
win <- function(d, pad = 0.06) {
  rx <- range(d$x); ry <- range(d$y)
  s <- max(diff(rx), diff(ry)) * (1 + pad) / 2
  list(x = mean(rx) + c(-s, s), y = mean(ry) + c(-s, s), s = 2 * s)
}

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

## ---- geoid
o <- ch[, c("digits", "level", "name", "id_digits")]
names(o) <- c("digits", "what they name", "which is", "digits so far")
o

## ---- zoom-d3
PW <- 138L; GAP <- 12L; TOP <- 74L; CAP <- 34L
W <- 5L * PW + 4L * GAP; H <- TOP + PW + CAP

# Panel k is a window on the unit named by prefix k-1, with the unit named by
# prefix k shaded inside it and its equals drawn thin around it. So each panel
# is the shaded shape of the panel before it, enlarged. Everything drawn is
# selected by GEOID prefix -- the figure is made the way the claim says the
# country is put together.
CTX <- function(k) if (k > 1) G(LEVK[k - 1], CHV(LEVN[k - 1], "uid")) else
                   G(LEVK[1], CHV(LEVN[1], "uid"))
panel <- function(k) {
  lev <- LEVK[k]
  self <- G(lev, CHV(LEVN[k], "uid"))
  ctx  <- CTX(k)
  w <- win(ctx, pad = 0.07)
  s <- PW / w$s
  off <- (k - 1L) * (PW + GAP)
  fx <- function(x) off + (x - w$x[1]) * s
  fy <- function(y) TOP + (w$y[2] - y) * s
  sib <- if (k > 1) G(lev, kids(lev, CHV(LEVN[k - 1], "geoid"))) else self[0, ]
  paste0("[", jstr(ringpaths(ctx, fx, fy)), ",",
         jstr(ringpaths(sib, fx, fy)), ",",
         jstr(ringpaths(self, fx, fy)), ",", off, "]")
}
PAN <- vapply(1:5, panel, character(1))
sofar <- vapply(1:5, function(k) paste(ch$digits[1:k], collapse = ""), character(1))

cat(paste0('
<div id="zm" style="margin:1.1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const P=[', paste(PAN, collapse = ","), '];
const SEG=', jstr(ch$digits), ',LVL=', jstr(ch$level), ',SF=', jstr(sofar), ';
const NM=', jstr(ch$name), ',CL=', jstr(CL), ';
const W=', W, ',H=', H, ',PW=', PW, ',GAP=', GAP, ',TOP=', TOP, ';
const svg=d3.select("#zm").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const MONO="ui-monospace,SFMono-Regular,Menlo,monospace";
// the identifier itself, one colored cell per prefix
const CW=[13,13,13,13,13];
let x=0; const cells=[];
SEG.forEach((s,k)=>{const w=s.length*CW[k];cells.push([x,w]);x+=w;});
const X0=(W-x)/2;
svg.append("text").attr("x",X0).attr("y",14).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("one 15-digit census BLOCK identifier");
SEG.forEach((s,k)=>{
  const a=X0+cells[k][0],w=cells[k][1];
  svg.append("rect").attr("x",a).attr("y",22).attr("width",w).attr("height",26)
    .attr("fill",CL[k]).attr("fill-opacity",0.13).attr("stroke",CL[k]).attr("rx",2);
  svg.append("text").attr("x",a+w/2).attr("y",40).attr("text-anchor","middle")
    .attr("font-family",MONO).attr("font-size","15px").attr("font-weight","600")
    .attr("fill",CL[k]).text(s);
  svg.append("line").attr("x1",a+w/2).attr("y1",50)
    .attr("x2",k*(PW+GAP)+PW/2).attr("y2",TOP-4)
    .attr("stroke",CL[k]).attr("stroke-width",1).attr("stroke-opacity",0.45);
});
// the five maps, each clipped to its own panel
const defs=svg.append("defs");
P.forEach((p,k)=>{
  defs.append("clipPath").attr("id","zc"+k).append("rect")
    .attr("x",p[3]).attr("y",TOP).attr("width",PW).attr("height",PW);
  const g=svg.append("g").attr("clip-path","url(#zc"+k+")");
  g.selectAll("path.c").data(p[0]).join("path").attr("d",d=>d)
    .attr("fill","#f4f4f4").attr("stroke","#bbb").attr("stroke-width",0.7);
  g.selectAll("path.s").data(p[1]).join("path").attr("d",d=>d)
    .attr("fill","none").attr("stroke","#ccc").attr("stroke-width",0.5);
  g.selectAll("path.m").data(p[2]).join("path").attr("d",d=>d)
    .attr("fill",CL[k]).attr("fill-opacity",0.85).attr("stroke",CL[k])
    .attr("stroke-width",0.8);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",TOP+PW+13)
    .attr("text-anchor","middle").attr("font-size","11px")
    .attr("font-weight","600").attr("fill",CL[k]).text(LVL[k]);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",TOP+PW+25)
    .attr("text-anchor","middle").attr("font-family",MONO)
    .attr("font-size","9.5px").attr("fill","#777").text(SF[k]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Every shape here was selected by slicing digits off the identifier above. Each
panel is the shaded shape of the panel to its left, enlarged.</p>
'))

## ---- zoom-static
# xpd stays FALSE here. Georgia is ten times the width of the county window it
# is drawn in on the second panel, and without clipping it floods the device.
layout(matrix(1:5, 1, 5))
op <- par(mar = c(2.4, 0.2, 2.2, 0.2), xpd = FALSE)
CTX <- function(k) if (k > 1) G(LEVK[k - 1], CHV(LEVN[k - 1], "uid")) else
                   G(LEVK[1], CHV(LEVN[1], "uid"))
for (k in 1:5) {
  lev <- LEVK[k]
  self <- G(lev, CHV(LEVN[k], "uid"))
  ctx  <- CTX(k)
  w <- win(ctx, pad = 0.07)
  plot(NA, xlim = w$x, ylim = w$y, asp = 1, axes = FALSE, ann = FALSE)
  drawrings(ctx, col = "#f4f4f4", border = "#bbb", lwd = 0.7)
  if (k > 1)
    drawrings(G(lev, kids(lev, CHV(LEVN[k - 1], "geoid"))), col = NA,
              border = "#cccccc", lwd = 0.5)
  drawrings(self, col = adjustcolor(CL[k], 0.85), border = CL[k], lwd = 0.8)
  title(ch$level[k], cex.main = 0.85, col.main = CL[k], line = 0.4)
  mtext(paste(ch$digits[1:k], collapse = ""), side = 1, line = 0.3,
        cex = 0.52, col = "#777")
}
par(op); layout(1)

## ---- regions-prep
um <- read.csv("data/derived/us_map.csv",       stringsAsFactors = FALSE)
ud <- read.csv("data/derived/us_divisions.csv", stringsAsFactors = FALSE)
ui <- read.csv("data/derived/us_ids.csv",       stringsAsFactors = FALSE,
               colClasses = c(uid = "character", fips = "character"))
# The four hues that stand for the four regions wherever this book draws
# them (regional-shift sets the palette), mixed toward white so a dark label
# can sit on the fill -- and so they read as fills, not as the saturated
# line colours the spine levels keep in every other figure here.
RCOL <- c(Northeast = "#2c7fb8", Midwest = "#4d9221",
          South = "#C41230", West = "#e08214")
MPAL <- sapply(RCOL, function(k) colorRampPalette(c("#ffffff", k))(100)[45])
uslab <- ui[ui$area > 1150 | ui$uid == "HI", ]
usreg <- data.frame(name = c("WEST", "MIDWEST", "NORTHEAST", "SOUTH"),
                    col  = unname(RCOL[c("West", "Midwest", "Northeast", "South")]),
                    x = c(290, 600, 958, 730), y = c(32, 36, 40, 618))
u_xl <- c(70, 1085); u_yl <- c(0, 745)      # the base-map frame, cropped
u_sc <- 760 / diff(u_xl)
u_fx <- function(x) (x - u_xl[1]) * u_sc
u_fy <- function(y) (y - u_yl[1]) * u_sc    # frame y already runs down
UH   <- round(diff(u_yl) * u_sc)

## ---- regions-d3
# ---------------------------------------------------------------------------
# FIGURE 2. A key, not a data figure: which states group into which division
# and region, on the course's standard base map. Pale region fills so the
# figure cannot be confused with the saturated spine colours; heavy open
# paths are the division borders, drawn once from the dissolved unions.
# ---------------------------------------------------------------------------
P   <- ringpaths(um, u_fx, u_fy)
ids <- vapply(split(um, interaction(um$uid, um$part, drop = TRUE)),
              function(z) z$uid[1], character(1))
stp <- tapply(P, ids, paste, collapse = "")
uj  <- ui[match(names(stp), ui$uid), ]
STJ <- paste0('{"n":"', uj$name, '","f":"', uj$fips, '","r":"', uj$region,
              '","dv":"', uj$division, '","c":"', MPAL[uj$region],
              '","p":"', stp, '"}', collapse = ",")
dvp <- vapply(split(ud, interaction(ud$division, ud$part, drop = TRUE)),
              function(z) paste0("M", paste0(round(u_fx(z$x)), ",",
                                             round(u_fy(z$y)), collapse = "L")),
              character(1))
DVJ <- paste0('"', dvp, '"', collapse = ",")
SLJ <- paste0('{"x":', round(u_fx(uslab$label_x)), ',"y":',
              round(u_fy(uslab$label_y)), ',"t":"', uslab$uid, '"}',
              collapse = ",")
RLJ <- paste0('{"x":', round(u_fx(usreg$x)), ',"y":', round(u_fy(usreg$y)),
              ',"t":"', usreg$name, '","c":"', usreg$col, '"}', collapse = ",")
cat(paste0('
<div id="rg" style="position:relative;margin:1.1em 0"></div>
<script>
(function(){
const ST=[', STJ, '];
const DV=[', DVJ, '];
const SL=[', SLJ, '];
const RL=[', RLJ, '];
const W=760,H=', UH, ';
const wrap=d3.select("#rg");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const tip=wrap.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("path").data(ST).join("path")
  .attr("d",d=>d.p).attr("fill",d=>d.c)
  .attr("stroke","#fff").attr("stroke-width",0.6)
  .on("mousemove",function(e,d){
    d3.select(this).attr("stroke","#111").attr("stroke-width",1.2).raise();
    const m=d3.pointer(e,wrap.node());
    tip.style("opacity",1)
      .html("<b>"+d.n+"</b><br>state FIPS "+d.f+" \\u00b7 "+d.dv+
            " division, "+d.r+" region")
      .style("left",Math.min(m[0]+16,wrap.node().clientWidth-250)+"px")
      .style("top",(m[1]+10)+"px");
  })
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#fff").attr("stroke-width",0.6);
    tip.style("opacity",0);
  });
svg.append("g").selectAll("path").data(DV).join("path")
  .attr("d",p=>p).attr("fill","none").attr("stroke","#4a4a4a")
  .attr("stroke-width",1.5).attr("pointer-events","none");
svg.append("g").selectAll("text").data(SL).join("text")
  .attr("class","on-mark")
  .attr("x",d=>d.x).attr("y",d=>d.y).attr("text-anchor","middle")
  .attr("font-size","9.5px").attr("fill","#333")
  .attr("pointer-events","none").text(d=>d.t);
svg.append("g").selectAll("text").data(RL).join("text")
  .attr("x",d=>d.x).attr("y",d=>d.y).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight",700).attr("fill",d=>d.c)
  .text(d=>d.t);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a state: the FIPS code is in every identifier on the spine; the
division and region are in none.</p>'))

## ---- regions-static
op <- par(mar = c(0, 0, 0, 0))
plot(NA, xlim = u_xl, ylim = rev(u_yl), asp = 1, axes = FALSE, ann = FALSE)
ufill <- setNames(MPAL[ui$region], ui$uid)
uk <- interaction(um$uid, um$part, drop = TRUE)
for (l in levels(uk)) {
  z <- um[uk == l, ]
  polygon(z$x, z$y, col = ufill[[z$uid[1]]], border = "#ffffff", lwd = 0.35)
}
for (z in split(ud, interaction(ud$division, ud$part, drop = TRUE)))
  lines(z$x, z$y, col = "#4a4a4a", lwd = 1.4)
text(uslab$label_x, uslab$label_y, uslab$uid, cex = 0.45, col = "#333333")
text(usreg$x, usreg$y, usreg$name, cex = 0.95, font = 2, col = usreg$col)
par(op)

## ---- source
data.frame(
  item = c("Counts and areas", "Vintage", "Geography types pulled",
           "Units described", "Boundaries", "Mapped here",
           "Access", "What each row carries"),
  value = c("Census Bureau Gazetteer Files", "2024", nrow(lv),
            n(sum(lv$count)), "Census Bureau TIGER/Line, 2020",
            paste0(FV("county_name"), " — ", n(FV("n_blocks")), " blocks"),
            "Open — no key, no registration, no rate limit",
            "identifier, name, land and water area, and for TIGER a polygon"))

## ---- geo-raw-setup
RD  <- file.path(D, "raw")
gsh <- read.csv(file.path(RD, "source-shape.csv"), stringsAsFactors = FALSE)
GS  <- function(k) as.numeric(gsh$value[gsh$name == k])
ghd <- read.csv(file.path(RD, "gaz-headers.csv"), stringsAsFactors = FALSE)
ghd$kind <- sub("^2024_Gaz_(.*)_national\\.txt$", "\\1", ghd$file)

# The gazetteer files are tab-separated with padded fields. One column per
# field, one row per column of the file, so the three can be compared.
gaztab <- function(f) {
  L <- readLines(file.path(RD, f), warn = FALSE)
  p <- lapply(L[1:3], function(x) trimws(strsplit(x, "\t")[[1]]))
  k <- min(lengths(p))
  data.frame(Column = p[[1]][seq_len(k)],
             Row_1  = p[[2]][seq_len(k)],
             Row_2  = p[[3]][seq_len(k)],
             stringsAsFactors = FALSE)
}

## ---- geo-raw-counties
gaztab("gaz-counties-excerpt.txt")

## ---- geo-raw-tracts
gaztab("gaz-tracts-excerpt.txt")

## ---- geo-raw-zcta
gaztab("gaz-zcta-excerpt.txt")

## ---- geo-headergrid
KIND <- c("counties", "tracts", "cousubs", "place", "elsd", "unsd", "zcta")
KLAB <- c("counties", "census tracts", "county subdivisions", "places",
          "elementary school districts", "unified school districts",
          "ZCTAs (ZIP approximations)")
FLD  <- c("USPS", "GEOID", "NAME", "ANSICODE", "LSAD", "FUNCSTAT",
          "LOGRADE", "HIGRADE", "ALAND", "AWATER", "ALAND_SQMI",
          "AWATER_SQMI", "INTPTLAT", "INTPTLONG")
has <- outer(KIND, FLD, Vectorize(function(k, f)
  any(ghd$kind == k & ghd$field == f)))
stopifnot(all(FLD %in% ghd$field), all(KIND %in% ghd$kind))
par(mar = c(0.4, 11.6, 5.4, 0.4))
plot(NA, xlim = c(0.4, length(FLD) + 0.6),
     ylim = c(length(KIND) + 0.6, 0.4), axes = FALSE, xlab = "", ylab = "")
for (i in seq_along(KIND)) for (j in seq_along(FLD)) {
  st <- if (!has[i, j] && FLD[j] == "USPS") "gone" else
        if (has[i, j] && FLD[j] == "USPS") "state" else
        if (has[i, j]) "yes" else "no"
  rect(j - 0.42, i - 0.40, j + 0.42, i + 0.40,
       col = c(yes = "#C8C8C8", no = "#F4F4F4", state = "#54278F",
               gone = "#C41230")[st],
       border = "white", lwd = 1.2)
  if (st == "gone") text(j, i, "none", cex = 0.52, col = "white")
}
text(rep(0.3, length(KIND)), seq_along(KIND), KLAB, adj = c(1, 0.5),
     cex = 0.72, xpd = NA)
text(seq_along(FLD), rep(0.35, length(FLD)), FLD, adj = c(0, 0.5), srt = 55,
     cex = 0.62, col = "grey25", xpd = NA)

## ---- geo-clean
o <- lv[lv$geography %in% c("county", "census tract",
                            "ZCTA (ZIP approximation)"),
        c("geography", "count", "id_length", "id_encodes", "nests_into")]
o$count <- n(o$count)
names(o) <- c("geography", "how many", "ID digits", "the ID encodes",
              "nests into")
o

## ---- levels
o <- lv[order(-lv$count), c("geography", "count", "id_length", "median_sq_mi")]
o$count <- n(o$count)
names(o) <- c("geography", "how many", "digits in the ID", "median sq mi")
o

## ---- counts-d3
o <- lv[order(lv$count), ]
BW <- 560; BH <- 22; TOPB <- 26; LEFT <- 176
HB <- TOPB + nrow(o) * BH + 30
lo <- log10(min(o$count)); hi <- log10(max(o$count))
xs <- function(v) LEFT + (log10(v) - 3) / (log10(200000) - 3) * (BW - LEFT - 16)
bars <- paste0("[", paste0(sprintf(
  '{"g":"%s","n":%d,"a":%s,"x":%.1f,"y":%d,"c":"%s"}',
  o$geography, o$count, o$median_sq_mi, xs(o$count),
  TOPB + (seq_len(nrow(o)) - 1) * BH,
  ifelse(o$nests, "#2c7fb8", "#C41230")), collapse = ","), "]")
cat(paste0('
<div id="cnt" style="margin:0.9em 0"></div>
<script>
(function(){
const B=', bars, ',W=', BW, ',H=', HB, ',L=', LEFT, ',BH=', BH, ';
const svg=d3.select("#cnt").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
[1000,10000,100000].forEach(v=>{
  const x=L+(Math.log10(v)-3)/(Math.log10(200000)-3)*(W-L-16);
  svg.append("line").attr("x1",x).attr("y1",18).attr("x2",x).attr("y2",H-24)
    .attr("stroke","#e4e4e4");
  svg.append("text").attr("x",x).attr("y",14).attr("text-anchor","middle")
    .attr("font-size","9px").attr("fill","#999")
    .text(v.toLocaleString());});
B.forEach(d=>{
  svg.append("rect").attr("x",L).attr("y",d.y+3).attr("width",Math.max(1,d.x-L))
    .attr("height",BH-9).attr("fill",d.c).attr("rx",1.5);
  svg.append("text").attr("x",L-7).attr("y",d.y+BH/2+1).attr("text-anchor","end")
    .attr("font-size","10.5px").attr("fill","#333").text(d.g);
  // the longest bar reaches the frame, so its label goes inside it
  const s=d.n.toLocaleString()+"  \\u00b7  median "+d.a+" sq mi";
  const inside=d.x+s.length*5.1+8>W;
  svg.append("text").attr("x",d.x+(inside?-6:5)).attr("y",d.y+BH/2+1)
    .attr("text-anchor",inside?"end":"start")
    .attr("font-size","9.5px").attr("fill",inside?"#fff":"#777").text(s);});
svg.append("text").attr("x",L).attr("y",H-8).attr("font-size","9.5px")
  .attr("fill","#777").text("number of units, log scale");
[["nests into the spine","#2c7fb8",300],["cuts across it","#C41230",440]]
 .forEach(([t,c,x])=>{
  svg.append("rect").attr("x",x).attr("y",H-16).attr("width",9).attr("height",9)
    .attr("fill",c);
  svg.append("text").attr("x",x+13).attr("y",H-8).attr("font-size","9.5px")
    .attr("fill","#777").text(t);});
})();
</script>
'))

## ---- counts-static
o <- lv[order(lv$count), ]
par(mar = c(3.4, 11.2, 0.6, 1.2))
b <- barplot(o$count, horiz = TRUE, names.arg = substr(o$geography, 1, 26),
             las = 1, cex.names = 0.72, log = "x", xlim = c(1000, 200000),
             col = ifelse(o$nests, "#2c7fb8", "#C41230"), border = NA,
             xlab = "", cex.axis = 0.7)
mtext("number of units (log scale)", side = 1, line = 2.1, cex = 0.7)
text(o$count, b, paste0("  ", format(o$count, big.mark = ",")), adj = 0,
     cex = 0.6, col = "#777", xpd = NA)
legend("bottomright", c("nests into the spine", "cuts across it"),
       fill = c("#2c7fb8", "#C41230"), border = NA, bty = "n", cex = 0.7)

## ---- tests
o <- ne[1:2, c("test", "result", "n_pass", "n_total")]
names(o) <- c("tested", "holds", "units passing", "units tested")
o

## ---- scales-d3
PW <- 168L; GAP <- 10L; TOPS <- 40L; BOT <- 30L
Ws <- 4L * PW + 3L * GAP; Hs <- TOPS + PW + BOT
KEY <- c("b", "g", "t", "c")
w <- win(G("c"), pad = 0.04)
s <- PW / w$s

# The county silhouette goes under every panel, so the four panels are visibly
# the same piece of ground and not four different maps.
mk <- function(i) {
  off <- (i - 1L) * (PW + GAP)
  fx <- function(x) off + (x - w$x[1]) * s
  fy <- function(y) TOPS + (w$y[2] - y) * s
  hit <- CHV(if (KEY[i] == "c") "county" else
             LEVN[match(KEY[i], LEVK)], "uid")
  paste0("[", jstr(ringpaths(G("c"), fx, fy)), ",",
         jstr(ringpaths(G(KEY[i]), fx, fy)), ",",
         jstr(ringpaths(G(KEY[i], hit), fx, fy)), ",", off, "]")
}
PAN <- vapply(seq_along(KEY), mk, character(1))
ttl <- c(paste(n(FV("n_blocks")), "blocks"),
         paste(n(FV("n_bg")), "block groups"),
         paste(n(FV("n_tracts")), "tracts"), "1 county")
sub <- paste(c(15, 12, 11, 5), "digits in the ID")
lwd <- c(0.25, 0.6, 0.8, 1.4)

cat(paste0('
<div id="scl" style="margin:1.1em 0"></div>
<script>
(function(){
const P=[', paste(PAN, collapse = ","), '],T=', jstr(ttl), ',S=', jstr(sub), ';
const LW=[', paste(lwd, collapse = ","), '];
const W=', Ws, ',H=', Hs, ',PW=', PW, ',TOP=', TOPS, ';
const svg=d3.select("#scl").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
P.forEach((p,i)=>{
  const g=svg.append("g");
  g.selectAll("path.b").data(p[0]).join("path").attr("d",d=>d)
    .attr("fill","#eef1f4").attr("stroke","none");
  g.selectAll("path.u").data(p[1]).join("path").attr("d",d=>d)
    .attr("fill","none").attr("stroke","#4d6a86").attr("stroke-width",LW[i]);
  g.selectAll("path.h").data(p[2]).join("path").attr("d",d=>d)
    .attr("fill","#C41230").attr("fill-opacity",0.9)
    .attr("stroke","#C41230").attr("stroke-width",1.2);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",16).attr("text-anchor","middle")
    .attr("font-size","12.5px").attr("font-weight","600").attr("fill","#222")
    .text(T[i]);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",28).attr("text-anchor","middle")
    .attr("font-size","9.5px").attr("fill","#888").text(S[i]);
});
svg.append("text").attr("x",W/2).attr("y",H-9).attr("text-anchor","middle")
  .attr("font-size","10px").attr("fill","#C41230")
  .text("in red: one block, then the block group, tract and county containing it");
})();
</script>
'))

## ---- scales-static
KEY <- c("b", "g", "t", "c")
ttl <- c(paste(n(FV("n_blocks")), "blocks"),
         paste(n(FV("n_bg")), "block groups"),
         paste(n(FV("n_tracts")), "tracts"), "1 county")
sub <- paste(c(15, 12, 11, 5), "digits in the ID")
lwd <- c(0.12, 0.3, 0.45, 0.8)
w <- win(G("c"), pad = 0.04)
layout(matrix(1:4, 1, 4))
op <- par(mar = c(0.4, 0.2, 2.6, 0.2), oma = c(1.7, 0, 0, 0), xpd = FALSE)
for (i in seq_along(KEY)) {
  plot(NA, xlim = w$x, ylim = w$y, asp = 1, axes = FALSE, ann = FALSE)
  drawrings(G("c"), col = "#eef1f4", border = NA)
  drawrings(G(KEY[i]), col = NA, border = "#4d6a86", lwd = lwd[i])
  hit <- CHV(if (KEY[i] == "c") "county" else LEVN[match(KEY[i], LEVK)], "uid")
  drawrings(G(KEY[i], hit), col = "#C41230", border = "#C41230", lwd = 1)
  title(ttl[i], cex.main = 0.9, line = 1.1)
  mtext(sub[i], side = 3, line = 0.25, cex = 0.55, col = "#888")
}
mtext(paste("in red: one block, then the block group, tract and county",
            "containing it"), side = 1, outer = TRUE, line = 0.3, cex = 0.6,
      col = "#C41230")
par(op); layout(1)

## ---- fails
o <- ne[3:4, c("test", "result", "n_pass", "n_total")]
names(o) <- c("tested", "holds", "units passing", "units tested")
o

## ---- encodes
o <- lv[order(-lv$nests, -lv$count), c("geography", "id_encodes", "nests_into")]
names(o) <- c("geography", "the ID is built from", "nests into")
o

## ---- nest-d3
PW <- 214L; GAP <- 22L; TOPN <- 40L; BOTN <- 40L
Wn <- 3L * PW + 2L * GAP; Hn <- TOPN + PW + BOTN
w  <- win(G("c"), pad = 0.04)
s  <- PW / w$s
OV  <- c("g", "p", "z")
OCL <- c("#2c7fb8", "#C41230", "#e08214")
FLG <- c("split_bg", "split_place", "split_zcta")
ROW <- c("block group", "census place (city limits)", "ZCTA (ZIP approximation)")

mk <- function(i) {
  off <- (i - 1L) * (PW + GAP)
  fx <- function(x) off + (x - w$x[1]) * s
  fy <- function(y) TOPN + (w$y[2] - y) * s
  cut <- tsp$uid[tsp[[FLG[i]]] == 1]
  paste0("[", jstr(ringpaths(G("t"), fx, fy)), ",",
         jstr(ringpaths(G("t", cut), fx, fy)), ",",
         jstr(ringpaths(G(OV[i]), fx, fy)), ",", off, "]")
}
PAN <- vapply(1:3, mk, character(1))
ttl <- c("block groups", "city limits", "ZCTAs")
sub <- sprintf("%s of them", vapply(ROW, function(r) n(SP(r, "units")),
                                    character(1)))
cnt <- sprintf("%s of %s tracts cut",
               vapply(ROW, function(r) n(SP(r, "tracts_split")), character(1)),
               n(FV("n_tracts")))
rol <- vapply(ROW, function(r) SP(r, "role"), character(1))

cat(paste0('
<div id="nst" style="margin:1.1em 0"></div>
<script>
(function(){
const P=[', paste(PAN, collapse = ","), '],T=', jstr(ttl), ',S=', jstr(sub), ';
const C=', jstr(cnt), ',R=', jstr(rol), ',OC=', jstr(OCL), ';
const W=', Wn, ',H=', Hn, ',PW=', PW, ',TOP=', TOPN, ';
const svg=d3.select("#nst").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
// ZCTAs run off the edge of the county, so each panel clips to its own frame
const defs=svg.append("defs");
P.forEach((p,i)=>{
  defs.append("clipPath").attr("id","nc"+i).append("rect")
    .attr("x",p[3]).attr("y",TOP).attr("width",PW).attr("height",PW);
  const g=svg.append("g").attr("clip-path","url(#nc"+i+")");
  g.selectAll("path.t").data(p[0]).join("path").attr("d",d=>d)
    .attr("fill","#f7f7f7").attr("stroke","#b6b6b6").attr("stroke-width",0.6);
  g.selectAll("path.c").data(p[1]).join("path").attr("d",d=>d)
    .attr("fill","#f4d9dd").attr("stroke","#b6b6b6").attr("stroke-width",0.6);
  g.selectAll("path.o").data(p[2]).join("path").attr("d",d=>d)
    .attr("fill","none").attr("stroke",OC[i]).attr("stroke-width",1.4);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",16).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600").attr("fill",OC[i])
    .text(T[i]);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",28).attr("text-anchor","middle")
    .attr("font-size","9.5px").attr("fill","#888").text(S[i]+"  \\u00b7  "+R[i]);
  svg.append("text").attr("x",p[3]+PW/2).attr("y",TOP+PW+18)
    .attr("text-anchor","middle").attr("font-size","13px")
    .attr("font-weight","600").attr("fill",i?"#C41230":"#2c7fb8").text(C[i]);
});
svg.append("rect").attr("x",W/2-108).attr("y",H-14).attr("width",10)
  .attr("height",10).attr("fill","#f4d9dd").attr("stroke","#b6b6b6");
svg.append("text").attr("x",W/2-94).attr("y",H-5).attr("font-size","9.5px")
  .attr("fill","#777").text("tract with a boundary running through it");
})();
</script>
'))

## ---- nest-static
OV  <- c("g", "p", "z")
OCL <- c("#2c7fb8", "#C41230", "#e08214")
FLG <- c("split_bg", "split_place", "split_zcta")
ROW <- c("block group", "census place (city limits)", "ZCTA (ZIP approximation)")
ttl <- c("block groups", "city limits", "ZCTAs")
w <- win(G("c"), pad = 0.04)
layout(matrix(1:3, 1, 3))
op <- par(mar = c(2.4, 0.3, 3.0, 0.3), oma = c(1.4, 0, 0, 0), xpd = FALSE)
for (i in 1:3) {
  plot(NA, xlim = w$x, ylim = w$y, asp = 1, axes = FALSE, ann = FALSE)
  drawrings(G("t"), col = "#f7f7f7", border = "#b6b6b6", lwd = 0.5)
  drawrings(G("t", tsp$uid[tsp[[FLG[i]]] == 1]), col = "#f4d9dd",
            border = "#b6b6b6", lwd = 0.5)
  drawrings(G(OV[i]), col = NA, border = OCL[i], lwd = 1)
  title(ttl[i], cex.main = 1, col.main = OCL[i], line = 1.4)
  mtext(paste0(n(SP(ROW[i], "units")), " of them, ", SP(ROW[i], "role")),
        side = 3, line = 0.5, cex = 0.55, col = "#888")
  mtext(paste0(n(SP(ROW[i], "tracts_split")), " of ", n(FV("n_tracts")),
               " tracts cut"), side = 1, line = 0.9, cex = 0.72, font = 2,
        col = if (i == 1) "#2c7fb8" else "#C41230")
}
mtext("shaded: a tract with a boundary running through it", side = 1,
      outer = TRUE, line = 0.1, cex = 0.6, col = "#777")
par(op); layout(1)

## ---- zcta
data.frame(
  question = c("What is a ZIP code?", "Who maintains it?", "What is it for?",
               "When does it change?", "What does a ZCTA ID encode?",
               "Which state is a ZCTA in?"),
  answer = c("A set of mail delivery stops — not an area",
             "The U.S. Postal Service", "Routing efficiency",
             "Whenever routing changes", g("ZCTA (ZIP approximation)", "id_encodes"),
             "The file does not say"))

## ---- recap
data.frame(
  `where it bit` = c("Precinct returns", "Redlining", "Election administration",
                     "Residual votes"),
  `the mismatch` = c("Precincts do not nest into census blocks",
                     "1930s HOLC polygons do not nest into 2020 tracts",
                     "\"Jurisdiction\" is a township in WI, a county in TX",
                     "Chicago runs elections separately from Cook County"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: on the dark page those fills are lifted or pinned and
# already pass, and a --paper stroke would sit dark behind a dark ink there,
# because the checker scores the fill against the stroke it touches. Note the
# six-digit #555555 -- this figure spells it long, and [fill="#555" i] would
# not match it.
cat('<style>
#zm text[fill="#2c7fb8" i],
#zm text[fill="#c41230" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#zm text[fill="#555555" i],
#zm text[fill="#e08214" i],
#zm text[fill="#4d9221" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
