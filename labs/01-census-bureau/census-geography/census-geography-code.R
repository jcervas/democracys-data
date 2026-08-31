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

## ---- tests
o <- ne[1:2, c("test", "result", "n_pass", "n_total")]
names(o) <- c("tested", "holds", "units passing", "units tested")
o

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
<script src="../../_lib/d3.v7.min.js"></script>
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

