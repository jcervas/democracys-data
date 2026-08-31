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

## ---- pl-raw-geo
GL <- readLines(file.path(D, "raw", "usgeo2020-head.txt"), warn = FALSE)
SL <- readLines(file.path(D, "raw", "us000012020-head.txt"), warn = FALSE)
sp <- function(x) strsplit(x, "|", fixed = TRUE)[[1]]
G  <- sp(GL[1]); A <- sp(SL[1])

# The fields that carry a value, one per row. The empty ones are counted
# instead of listed: there are more of them than there are filled ones, and
# that imbalance is the paragraph below.
GNE <- which(nzchar(G))
mg <- rep("—", length(G))
mg[3]  <- "summary level — 010 means the whole nation"
mg[8]  <- "logical record number — the join key"
mg[88] <- "the name of the thing this row describes"
data.frame(
  Field = c(as.character(GNE),
            paste0("the other ", length(G) - length(GNE))),
  Value = c(G[GNE], "(empty)"),
  What_it_means = c(mg[GNE], "kinds of geography a country is not"))

## ---- pl-raw-levels
LV <- vapply(GL, function(l) sp(l)[3], character(1))
NM <- c("010" = "the whole nation", "020" = "a region",
        "030" = "a division", "040" = "a state")
data.frame(
  Summary_level = unname(LV),
  What_that_code_means = unname(ifelse(LV %in% names(NM), NM[LV], "—")),
  Logical_record = unname(vapply(GL, function(l) sp(l)[8], character(1))),
  Name_in_field_88 = unname(vapply(GL, function(l) sp(l)[88], character(1))))

## ---- pl-raw-seg
# Every one of the 149 fields carries a number, so the three the chapter reads
# are named and the rest are given as the ranges they belong to.
data.frame(
  Field = c("1", "2", "3", "4", "5", "6", "7", "8", "9–77", "78", "79–149"),
  Value = c(A[1:8], "counts", A[78], "counts"),
  What_it_means = c("file identifier", "state abbreviation",
                    "characteristic iteration", "file sequence number",
                    "logical record number — the join key",
                    "population of the United States",
                    "population of one race",
                    "White alone", "the rest of Table P1",
                    "Hispanic population", "Table P2"))

## ---- pl-anyfig
par(mar = c(2.2, 0.6, 0.4, 0.6))
plot(NA, xlim = c(0.5, 71.5), ylim = c(0, 1), axes = FALSE, xlab = "",
     ylab = "", xaxs = "i")
gcol <- rep("#EDEDED", 71)
gcol[WFLD[-1] - 5] <- "#B9A7D6"
gcol[WFLD[1]  - 5] <- "#54278F"
rect(seq_len(71) - 0.44, 0.36, seq_len(71) + 0.44, 0.80, col = gcol,
     border = NA)
seg <- function(a, b, lab, ctr = TRUE) {
  segments(a - 0.44, 0.29, b + 0.44, 0.29, lwd = 1.1, col = "grey35")
  segments(c(a - 0.44, b + 0.44), 0.29, c(a - 0.44, b + 0.44), 0.25,
           lwd = 1.1, col = "grey35")
  text(if (ctr) (a + b) / 2 else a - 0.44, 0.15, lab, cex = 0.66,
       col = "grey25", adj = c(if (ctr) 0.5 else 0, 0.5), xpd = NA)
}
seg(3, 8, "the six races alone", ctr = FALSE)
seg(9, 71, "the 63 multiple-race combinations")
text(1, 0.88, "field 1: the total", cex = 0.6, col = "grey45",
     adj = c(0, 0.5), xpd = NA)

## ---- pl-clean-nation
o <- data.frame(
  column = c("total", "hispanic", "nh_white", "nh_two", "white_alone",
             "white_any", "entropy", "geoid"),
  value = c(n(nat$total), n(nat$hispanic), n(nat$nh_white), n(nat$nh_two),
            n(nat$white_alone), n(nat$white_any),
            formatC(nat$entropy, format = "f", digits = 3),
            "(empty)"),
  `where it came from` = c(
    "segment 1, field 6", "segment 1, field 78", "segment 1, field 81",
    "segment 1, field 87", "segment 1, field 8",
    paste(NW, "fields of segment 1, added"),
    "computed from the eight P2 shares",
    "the United States has no FIPS code"),
  check.names = FALSE)
o

## ---- fig1-d3
# ---------------------------------------------------------------------------
# FIGURE 1. A proportional bar has an honesty problem here: five of the eight
# categories are under six percent and three are under one, so on a linear
# scale they are slivers a reader cannot see, let alone compare. The fix is not
# a log scale (which would misstate the proportions this figure exists to show)
# but a magnifier: the full bar at true proportion, and beneath it the last
# stretch of it enlarged, with the wedge that connects them drawn.
#
# This chunk carries the ONE d3 <script src> for the document. Every later
# figure uses the library loaded here; a second copy would silently double the
# payload.
# ---------------------------------------------------------------------------
W <- 760; PAD <- 8; BH <- 44; TOPB <- 30; GAPB <- 66; MAGH <- 44
H <- TOPB + BH + GAPB + MAGH + 92
CUT <- 4                      # the first CUT categories stay in the main bar
tail_share <- sum(NPC[CAT[-(1:CUT)]])
segs <- paste(vapply(CAT, function(k) sprintf(
  '{"k":"%s","l":"%s","s":"%s","v":%.4f,"n":"%s","p":"%s","c":"%s"}',
  k, LAB[[k]], SHORT[[k]], NPC[[k]], n(NP[[k]]), pc(NPC[[k]], 2), COL[[k]]),
  character(1)), collapse = ",")
cat(paste0('
<div id="f1" style="margin:1.1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', segs, '],CUT=', CUT, ',TS=', sprintf("%.4f", tail_share), ';
const W=', W, ',H=', H, ',PAD=', PAD, ',BH=', BH, ',TOP=', TOPB,
  ',GAP=', GAPB, ',MAGH=', MAGH, ';
const IW=W-2*PAD;
const svg=d3.select("#f1").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
// -- the true bar --
let x=PAD;
const pos={};
S.forEach(d=>{const w=IW*d.v/100;pos[d.k]=[x,w];x+=w;});
S.forEach(d=>{
  const [a,w]=pos[d.k];
  svg.append("rect").attr("x",a).attr("y",TOP).attr("width",Math.max(w,0.4))
    .attr("height",BH).attr("fill",d.c);
});
S.slice(0,CUT).forEach(d=>{
  const [a,w]=pos[d.k];
  svg.append("text").attr("x",a+w/2).attr("y",TOP+19).attr("text-anchor","middle")
    .attr("font-size","12.5px").attr("font-weight","600").attr("fill","#fff")
    .text(d.s);
  svg.append("text").attr("x",a+w/2).attr("y",TOP+34).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("fill","#fff").attr("fill-opacity",0.92)
    .text(d.p+"%");
});
svg.append("text").attr("x",PAD).attr("y",TOP-10).attr("font-size","11.5px")
  .attr("fill","#666")
  .text("every resident of the United States, one category each");
// -- the magnifier --
const X0=pos[S[CUT].k][0], MY=TOP+BH+GAP;
svg.append("path")
  .attr("d","M"+X0+","+(TOP+BH)+"L"+(W-PAD)+","+(TOP+BH)+"L"+(W-PAD)+","+MY+
            "L"+PAD+","+MY+"Z")
  .attr("fill","#f0f0f0").attr("stroke","none");
// The four smallest categories are still uneven after magnifying -- Two or
// More Races is three quarters of the enlarged bar -- so the labels are
// staggered on two rows with a leader line back to the segment. Centering all
// four on one row overlaps them into unreadability.
let mx=PAD;
S.slice(CUT).forEach((d,i)=>{
  const w=IW*(d.v/TS);
  svg.append("rect").attr("x",mx).attr("y",MY).attr("width",Math.max(w,0.6))
    .attr("height",MAGH).attr("fill",d.c);
  // One label per row, name and value on the same line. Even magnified, the
  // last three segments are narrow and adjacent; sharing a row in any
  // arrangement makes them overprint, so each gets its own and a leader line
  // back to its segment.
  const cxm=mx+w/2, ly=MY+MAGH+18+i*17;
  const lx=Math.min(Math.max(cxm,150),W-150);
  svg.append("path")
    .attr("d","M"+cxm+","+(MY+MAGH+1)+"L"+cxm+","+(ly-11)+"L"+lx+","+(ly-4))
    .attr("fill","none").attr("stroke",d.c).attr("stroke-width",0.8)
    .attr("stroke-opacity",0.55);
  // No single quotes anywhere in this block: the whole script is emitted from
  // R inside a single-quoted string, and one apostrophe would end it.
  svg.append("text").attr("x",lx).attr("y",ly).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("fill",d.c)
    .text(d.s+"   "+d.p+"%   "+d.n);
  mx+=w;
});
// The share is formatted ONCE, in R, and passed through as a string. R rounds
// half to even and JavaScript half up, so letting each renderer format this for
// itself is how the browser and the PDF end up disagreeing in the last digit.
svg.append("text").attr("x",PAD).attr("y",MY-8).attr("font-size","11.5px")
  .attr("fill","#666").text("the shaded wedge, enlarged: the last ',
  pc(tail_share), '% of the bar");
svg.append("text").attr("x",W-PAD).attr("y",H-10).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#888")
  .text("2020 Census, Table P2 · total population · ', n(FV("us_pop")), ' people");
})();
</script>'))

## ---- fig1-static
CUT <- 4
tail_share <- sum(NPC[CAT[-(1:CUT)]])
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, ann = FALSE,
     xaxs = "i", yaxs = "i")
# the true bar
x <- 0; pos <- list()
for (k in CAT) { w <- NPC[[k]]; pos[[k]] <- c(x, w); x <- x + w }
for (k in CAT) {
  p <- pos[[k]]
  rect(p[1], 66, p[1] + p[2], 88, col = COL[[k]], border = NA)
}
for (k in CAT[1:CUT]) {
  p <- pos[[k]]
  text(p[1] + p[2] / 2, 80, SHORT[[k]], col = "white", cex = 0.62, font = 2)
  text(p[1] + p[2] / 2, 72, paste0(pc(NPC[[k]], 2), "%"), col = "white",
       cex = 0.55)
}
text(0, 92, "every resident of the United States, one category each",
     adj = 0, cex = 0.55, col = "#666")
# the magnifier
X0 <- pos[[CAT[CUT + 1]]][1]
polygon(c(X0, 100, 100, 0), c(66, 66, 46, 46), col = "#f0f0f0", border = NA)
# Staggered on two rows with leader lines, for the reason given in the D3
# chunk: Two or More Races is three quarters of the enlarged bar and the other
# three labels would sit on top of each other.
mx <- 0; i <- 0
for (k in CAT[-(1:CUT)]) {
  w <- 100 * NPC[[k]] / tail_share
  rect(mx, 26, mx + w, 46, col = COL[[k]], border = NA)
  # One label per row, name and value on the same line. Even magnified, the
  # last three segments are narrow and adjacent; sharing a row in any
  # arrangement makes them overprint, so each gets its own and a leader line
  # back to its segment.
  cxm <- mx + w / 2
  ly <- 21 - i * 6
  lx <- min(max(cxm, 22), 78)
  lines(c(cxm, cxm, lx), c(25, ly + 5, ly + 2.2),
        col = adjustcolor(COL[[k]], 0.55), lwd = 0.7)
  text(lx, ly, paste0(SHORT[[k]], "   ", pc(NPC[[k]], 2), "%   ", n(NP[[k]])),
       col = COL[[k]], cex = 0.52)
  mx <- mx + w; i <- i + 1
}
text(0, 51, paste0("the shaded wedge, enlarged: the last ",
                   pc(tail_share), "% of the bar"),
     adj = 0, cex = 0.55, col = "#666")
text(100, 92, paste0("2020 Census, Table P2 - total population - ",
                     n(FV("us_pop")), " people"),
     adj = 1, cex = 0.5, col = "#888")

## ---- standards-table
o <- data.frame(
  issued  = format(as.Date(std$issued), "%d %b %Y"),
  what    = c("Directive No. 15", paste0("Revised, ", std$citation[2]),
              paste0("Revised, ", std$citation[3])),
  categories = std$categories,
  `question form` = c("separate collection preferred",
                      "two separate questions preferred",
                      "one combined question required"),
  `first census` = std$first_census,
  check.names = FALSE)
o

## ---- fig2-d3
# ---------------------------------------------------------------------------
# FIGURE 2. Not a chart of quantities -- a chart of AVAILABILITY. Each row is a
# category and each cell is a census; the cell is filled if a respondent could
# be counted in that category that year. The two OMB revisions are drawn as
# rules BETWEEN the census columns they separate, because that is where they
# sit in time, and every appearance and disappearance in the figure lines up
# with one of them. Nothing here is a count, so nothing here can be misread as
# a trend.
# ---------------------------------------------------------------------------
YRS <- c(1980, 1990, 2000, 2010, 2020, 2030)
YC  <- paste0("c", YRS)
ct  <- cts[cts$dimension != "combined", ]
ct2 <- cts[cts$dimension == "combined", ]
LW <- 268; CW <- 68; TOPT <- 52; RH <- 21
WT <- LW + length(YRS) * CW + 16
HT <- TOPT + (nrow(ct) + 1) * RH + 54
rows <- paste(vapply(seq_len(nrow(ct)), function(i) paste0(
  '{"n":"', ct$category[i], '","d":"', ct$dimension[i], '","v":',
  jnum(as.integer(ct[i, YC])), '}'), character(1)), collapse = ",")
# the combined-question row for 2030 is drawn as a continuation of the
# ethnicity row, so the reader sees the question itself move
comb <- jnum(as.integer(ct2[1, YC]))
cat(paste0('
<div id="f2" style="margin:1.1em 0"></div>
<script>
(function(){
const R=[', rows, '],CB=', comb, ',Y=', jnum(YRS), ';
const LW=', LW, ',CW=', CW, ',TOP=', TOPT, ',RH=', RH, ',W=', WT, ',H=', HT, ';
const svg=d3.select("#f2").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cx=j=>LW+j*CW+CW/2;
Y.forEach((y,j)=>{
  svg.append("text").attr("x",cx(j)).attr("y",TOP-10).attr("text-anchor","middle")
    .attr("font-size","12.5px").attr("font-weight","600")
    .attr("fill",y===2030?"#888":"#333").text(y);
});
svg.append("text").attr("x",cx(5)).attr("y",TOP-24).attr("text-anchor","middle")
  .attr("font-size","10px").attr("fill","#888").text("planned");
// the two revisions, as rules between columns
[[2,"1997 revision","one or more marks;  Asian / Pacific Islander split"],
 [5,"2024 revision","one combined question;  MENA added"]].forEach(([j,t,s])=>{
  const x=LW+j*CW;
  svg.append("line").attr("x1",x).attr("x2",x).attr("y1",TOP-40)
    .attr("y2",TOP+R.length*RH+RH+6).attr("stroke","#C41230")
    .attr("stroke-width",1.6);
  svg.append("text").attr("x",x-5).attr("y",TOP+R.length*RH+RH+22)
    .attr("text-anchor","end").attr("font-size","11px")
    .attr("font-weight","600").attr("fill","#C41230").text(t);
  svg.append("text").attr("x",x-5).attr("y",TOP+R.length*RH+RH+35)
    .attr("text-anchor","end").attr("font-size","10px").attr("fill","#C41230")
    .attr("fill-opacity",0.85).text(s);
});
R.forEach((r,i)=>{
  const y=TOP+i*RH;
  svg.append("text").attr("x",LW-10).attr("y",y+RH/2+4).attr("text-anchor","end")
    .attr("font-size","11.5px")
    .attr("fill",r.d==="ethnicity"?"#E08214":"#333").text(r.n);
  r.v.forEach((v,j)=>{
    if(!v) return;
    svg.append("rect").attr("x",LW+j*CW+3).attr("y",y+3)
      .attr("width",CW-6).attr("height",RH-6).attr("rx",2)
      .attr("fill",r.d==="ethnicity"?"#E08214":"#2C7FB8").attr("fill-opacity",0.8);
  });
});
// the 2030 combined-question cell, hatched to show it is the same question
const yl=TOP+(R.length-1)*RH;
CB.forEach((v,j)=>{ if(!v) return;
  svg.append("rect").attr("x",LW+j*CW+3).attr("y",yl+3)
    .attr("width",CW-6).attr("height",RH-6).attr("rx",2)
    .attr("fill","#E08214").attr("fill-opacity",0.35)
    .attr("stroke","#E08214").attr("stroke-dasharray","3,2");
});
svg.append("text").attr("x",LW).attr("y",H-8).attr("font-size","10.5px")
  .attr("fill","#888")
  .text("filled = a respondent could be counted in that category that year");
})();
</script>'))

## ---- fig2-static
YRS <- c(1980, 1990, 2000, 2010, 2020, 2030)
YC  <- paste0("c", YRS)
ct  <- cts[cts$dimension != "combined", ]
ct2 <- cts[cts$dimension == "combined", ]
par(mar = c(3.6, 15.4, 2.2, 0.6))
plot(NA, xlim = c(0.5, 6.5), ylim = c(nrow(ct) + 0.5, 0.1), axes = FALSE,
     ann = FALSE)
for (i in seq_len(nrow(ct))) {
  v <- as.integer(ct[i, YC])
  cl <- if (ct$dimension[i] == "ethnicity") "#E08214" else "#2C7FB8"
  for (j in which(v == 1))
    rect(j - 0.42, i - 0.36, j + 0.42, i + 0.36,
         col = adjustcolor(cl, 0.8), border = NA)
  mtext(ct$category[i], side = 2, at = i, las = 1, line = 0.3, cex = 0.55,
        col = if (ct$dimension[i] == "ethnicity") "#E08214" else "#333")
}
v <- as.integer(ct2[1, YC])
for (j in which(v == 1))
  rect(j - 0.42, nrow(ct) - 0.36, j + 0.42, nrow(ct) + 0.36,
       col = adjustcolor("#E08214", 0.35), border = "#E08214", lty = 2)
for (jj in c(2, 5)) abline(v = jj + 0.5, col = "#C41230", lwd = 1.6)
axis(3, 1:6, YRS, tick = FALSE, cex.axis = 0.7, line = -1.0,
     col.axis = "#333")
text(6, 0.28, "planned", cex = 0.5, col = "#888")
mtext("1997 revision", side = 1, at = 2.45, line = 0.4, cex = 0.55,
      adj = 1, col = "#C41230", font = 2)
mtext("2024 revision", side = 1, at = 5.45, line = 0.4, cex = 0.55,
      adj = 1, col = "#C41230", font = 2)
mtext("filled = a respondent could be counted in that category that year",
      side = 1, line = 2.1, cex = 0.55, col = "#888", adj = 0, at = 0.5)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# FIGURE 3. A dumbbell, because the quantity of interest is a GAP between two
# numbers for the same thing, and a dumbbell is the only common form that makes
# a gap the visual object rather than an inference from two bar lengths. The x
# axis is a square root: on a linear axis White (204 million) flattens the other
# five into the origin, and on a log axis the gaps become ratios and the reader
# loses the population sizes the figure also has to carry.
# ---------------------------------------------------------------------------
R6 <- data.frame(k = RACE6, lab = RLAB[RACE6],
                 a = vapply(RACE6, function(r) FN(paste0("us_", r, "_alone")),
                            0),
                 b = vapply(RACE6, function(r) FN(paste0("us_", r, "_any")), 0),
                 stringsAsFactors = FALSE)
R6$ratio <- R6$b / R6$a
R6 <- R6[order(-R6$ratio), ]
sq <- function(v) sqrt(v)
W3 <- 760; L3 <- 232; R3 <- 128; T3 <- 44; B3 <- 42; RH3 <- 34
H3 <- T3 + nrow(R6) * RH3 + B3
xmax <- sq(max(R6$b) * 1.02)
rows <- paste(vapply(seq_len(nrow(R6)), function(i) sprintf(
  '{"l":"%s","a":%.4f,"b":%.4f,"an":"%s","bn":"%s","r":"%s","g":"%s"}',
  R6$lab[i], sq(R6$a[i]) / xmax, sq(R6$b[i]) / xmax,
  n(R6$a[i]), n(R6$b[i]), FV(paste0("us_", R6$k[i], "_ratio")),
  n(R6$b[i] - R6$a[i])), character(1)), collapse = ",")
tk <- c(1e6, 1e7, 5e7, 1e8, 2e8)
tks <- paste(sprintf('{"x":%.4f,"t":"%s"}', sq(tk) / xmax,
                     c("1m", "10m", "50m", "100m", "200m")), collapse = ",")
cat(paste0('
<div id="f3" style="margin:1.1em 0"></div>
<script>
(function(){
const R=[', rows, '],TK=[', tks, '];
const W=', W3, ',H=', H3, ',L=', L3, ',RR=', R3, ',T=', T3, ',RH=', RH3, ';
const svg=d3.select("#f3").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=v=>L+v*(W-L-RR);
TK.forEach(t=>{
  svg.append("line").attr("x1",x(t.x)).attr("x2",x(t.x)).attr("y1",T-14)
    .attr("y2",T+R.length*RH-8).attr("stroke","#eee");
  svg.append("text").attr("x",x(t.x)).attr("y",T-20).attr("text-anchor","middle")
    .attr("font-size","10px").attr("fill","#aaa").text(t.t);
});
R.forEach((d,i)=>{
  const y=T+i*RH;
  svg.append("text").attr("x",L-12).attr("y",y+4).attr("text-anchor","end")
    .attr("font-size","11.5px").attr("fill","#333").text(d.l);
  svg.append("line").attr("x1",x(d.a)).attr("x2",x(d.b)).attr("y1",y)
    .attr("y2",y).attr("stroke","#C41230").attr("stroke-width",3)
    .attr("stroke-opacity",0.5);
  svg.append("circle").attr("cx",x(d.a)).attr("cy",y).attr("r",5.4)
    .attr("fill","#fff").attr("stroke","#555").attr("stroke-width",1.6);
  svg.append("circle").attr("cx",x(d.b)).attr("cy",y).attr("r",5.4)
    .attr("fill","#C41230");
  svg.append("text").attr("x",W-RR+10).attr("y",y+4).attr("font-size","12px")
    .attr("font-weight","600").attr("fill","#C41230").text("x "+d.r);
  svg.append("text").attr("x",W-RR+52).attr("y",y+4).attr("font-size","10.5px")
    .attr("fill","#888").text("+"+d.g);
});
const lg=svg.append("g").attr("transform","translate("+L+","+(H-16)+")");
lg.append("circle").attr("r",5.4).attr("fill","#fff").attr("stroke","#555")
  .attr("stroke-width",1.6);
lg.append("text").attr("x",11).attr("y",4).attr("font-size","11px")
  .attr("fill","#666").text("alone");
lg.append("circle").attr("cx",92).attr("r",5.4).attr("fill","#C41230");
lg.append("text").attr("x",103).attr("y",4).attr("font-size","11px")
  .attr("fill","#666").text("alone or in any combination");
svg.append("text").attr("x",W-8).attr("y",H-16).attr("text-anchor","end")
  .attr("font-size","10px").attr("fill","#aaa")
  .text("horizontal position on a square-root scale");
})();
</script>'))

## ---- fig3-static
R6 <- data.frame(k = RACE6, lab = RLAB[RACE6],
                 a = vapply(RACE6, function(r) FN(paste0("us_", r, "_alone")), 0),
                 b = vapply(RACE6, function(r) FN(paste0("us_", r, "_any")), 0),
                 stringsAsFactors = FALSE)
R6$ratio <- R6$b / R6$a
R6 <- R6[order(R6$ratio), ]
xmax <- sqrt(max(R6$b) * 1.02)
par(mar = c(2.6, 14.0, 1.8, 5.4))
plot(NA, xlim = c(0, 1), ylim = c(0.5, nrow(R6) + 0.5), axes = FALSE,
     ann = FALSE)
tk <- c(1e6, 1e7, 5e7, 1e8, 2e8)
abline(v = sqrt(tk) / xmax, col = "#eeeeee")
axis(3, sqrt(tk) / xmax, c("1m", "10m", "50m", "100m", "200m"), tick = FALSE,
     cex.axis = 0.55, col.axis = "#aaa", line = -1.2)
for (i in seq_len(nrow(R6))) {
  a <- sqrt(R6$a[i]) / xmax; b <- sqrt(R6$b[i]) / xmax
  segments(a, i, b, i, col = adjustcolor("#C41230", 0.5), lwd = 3)
  points(a, i, pch = 21, bg = "white", col = "#555555", cex = 1.1, lwd = 1.4)
  points(b, i, pch = 19, col = "#C41230", cex = 1.1)
  mtext(R6$lab[i], side = 2, at = i, las = 1, line = 0.3, cex = 0.55)
  text(1.02, i, paste0("x ", FV(paste0("us_", R6$k[i], "_ratio"))), adj = 0,
       cex = 0.6, font = 2, col = "#C41230", xpd = NA)
}
legend("bottomleft", c("alone", "alone or in any combination"),
       pch = c(21, 19), col = c("#555555", "#C41230"),
       pt.bg = c("white", "#C41230"), bty = "n", cex = 0.6, horiz = TRUE,
       inset = c(0, -0.10), xpd = NA)

## ---- fig4-d3
# ---------------------------------------------------------------------------
# FIGURE 4. A slope chart, with the discontinuity drawn ON the figure. The band
# between the two columns is not decoration: it is the statement that the two
# endpoints were produced by different processing, and that a line crossing it
# is not measuring only the population. Anything else -- a caption, a footnote
# -- lets a reader take the slope at face value, which is precisely the mistake
# the Census Bureau itself warns against.
# ---------------------------------------------------------------------------
KEYS <- c("white_alone", "white_any", "black_alone", "black_any",
          "aian_any", "sor_any", "two_plus")
KL <- c(white_alone = "White alone", white_any = "White, any combination",
        black_alone = "Black alone", black_any = "Black, any combination",
        aian_any = "AIAN, any combination", sor_any = "Some Other Race, any",
        two_plus = "Two or More Races")
KC <- c(white_alone = "#7F9BB3", white_any = "#7F9BB3",
        black_alone = "#C41230", black_any = "#C41230",
        aian_any = "#B35806", sor_any = "#999999", two_plus = "#8073AC")
sl <- data.frame(k = KEYS, lab = KL[KEYS], col = KC[KEYS],
                 a = vapply(KEYS, function(k) FN(paste0("d10_", k)), 0),
                 b = vapply(KEYS, function(k) FN(paste0("d20_", k)), 0),
                 ch = vapply(KEYS, function(k) FV(paste0("chg_", k)), ""),
                 stringsAsFactors = FALSE)
sl$idx <- 100 * sl$b / sl$a
W4 <- 760; L4 <- 190; R4 <- 214; T4 <- 52; B4 <- 60
H4 <- 316
yr <- range(c(100, sl$idx)) + c(-14, 14)
ys <- function(v) T4 + (yr[2] - v) / diff(yr) * (H4 - T4 - B4)
# Four of the seven lines land within four points of each other at the 2020
# end, so their labels are pushed apart to a minimum spacing and joined to
# their own line's endpoint by a short connector.
sl$ylab <- spread(ys(sl$idx), 14)
rows <- paste(vapply(seq_len(nrow(sl)), function(i) sprintf(
  '{"l":"%s","c":"%s","y0":%.1f,"y1":%.1f,"yl":%.1f,"ch":"%s"}',
  sl$lab[i], sl$col[i], ys(100), ys(sl$idx[i]), sl$ylab[i], sl$ch[i]),
  character(1)), collapse = ",")
cat(paste0('
<div id="f4" style="margin:1.1em 0"></div>
<script>
(function(){
const S=[', rows, '];
const W=', W4, ',H=', H4, ',L=', L4, ',R=', R4, ',T=', T4, ',B=', B4, ';
const X0=L,X1=W-R;
const svg=d3.select("#f4").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
// the discontinuity band
svg.append("rect").attr("x",X0+8).attr("y",T-26).attr("width",X1-X0-16)
  .attr("height",H-B-T+34).attr("fill","#C41230").attr("fill-opacity",0.055);
svg.append("text").attr("x",(X0+X1)/2).attr("y",T-32).attr("text-anchor","middle")
  .attr("font-size","11px").attr("font-weight","600").attr("fill","#C41230")
  .text("write-in areas added \\u00b7 30 \\u2192 200 characters captured \\u00b7 recoded");
svg.append("text").attr("x",X0).attr("y",T-14).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600").text("2010");
svg.append("text").attr("x",X1).attr("y",T-14).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600").text("2020");
const y100=', sprintf("%.1f", ys(100)), ';
svg.append("line").attr("x1",X0).attr("x2",X1).attr("y1",y100).attr("y2",y100)
  .attr("stroke","#ccc").attr("stroke-dasharray","4,3");
S.forEach(d=>{
  svg.append("line").attr("x1",X0).attr("x2",X1).attr("y1",d.y0).attr("y2",d.y1)
    .attr("stroke",d.c).attr("stroke-width",2.2).attr("stroke-opacity",0.85);
  svg.append("circle").attr("cx",X1).attr("cy",d.y1).attr("r",3.4).attr("fill",d.c);
  svg.append("path")
    .attr("d","M"+X1+","+d.y1+"L"+(X1+7)+","+d.yl+"L"+(X1+13)+","+d.yl)
    .attr("fill","none").attr("stroke",d.c).attr("stroke-width",0.7)
    .attr("stroke-opacity",0.55);
  svg.append("text").attr("x",X1+17).attr("y",d.yl+4).attr("font-size","11.5px")
    .attr("fill",d.c).attr("font-weight","600").text(d.ch+"%");
  svg.append("text").attr("x",X1+68).attr("y",d.yl+4).attr("font-size","11px")
    .attr("fill","#666").text(d.l);
});
svg.append("circle").attr("cx",X0).attr("cy",y100).attr("r",3.4).attr("fill","#555");
svg.append("text").attr("x",X0-10).attr("y",y100+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#555")
  .text("each category = 100 in 2010");
svg.append("text").attr("x",8).attr("y",H-10).attr("font-size","10.5px")
  .attr("fill","#888")
  .text("2010 and 2020 Censuses, P.L. 94-171, national totals");
})();
</script>'))

## ---- fig4-static
KEYS <- c("white_alone", "white_any", "black_alone", "black_any",
          "aian_any", "sor_any", "two_plus")
KL <- c(white_alone = "White alone", white_any = "White, any combination",
        black_alone = "Black alone", black_any = "Black, any combination",
        aian_any = "AIAN, any combination", sor_any = "Some Other Race, any",
        two_plus = "Two or More Races")
KC <- c(white_alone = "#7F9BB3", white_any = "#7F9BB3",
        black_alone = "#C41230", black_any = "#C41230",
        aian_any = "#B35806", sor_any = "#999999", two_plus = "#8073AC")
sl <- data.frame(k = KEYS, lab = KL[KEYS], col = KC[KEYS],
                 a = vapply(KEYS, function(k) FN(paste0("d10_", k)), 0),
                 b = vapply(KEYS, function(k) FN(paste0("d20_", k)), 0),
                 ch = vapply(KEYS, function(k) FV(paste0("chg_", k)), ""),
                 stringsAsFactors = FALSE)
sl$idx <- 100 * sl$b / sl$a
par(mar = c(2.4, 7.4, 3.4, 14.6))
yl <- range(c(100, sl$idx)) + c(-16, 16)
# Same spreader as the D3 version: four of the seven endpoints are within four
# points of one another and their labels would otherwise overprint.
sl$ylab <- spread(sl$idx, diff(yl) * 0.052)
plot(NA, xlim = c(0, 1), ylim = yl, axes = FALSE, ann = FALSE)
rect(0.03, yl[1], 0.97, yl[2], col = adjustcolor("#C41230", 0.055),
     border = NA)
abline(h = 100, col = "#cccccc", lty = 2)
for (i in seq_len(nrow(sl))) {
  segments(0, 100, 1, sl$idx[i], col = sl$col[i], lwd = 2.2)
  points(1, sl$idx[i], pch = 19, col = sl$col[i], cex = 0.7)
  lines(c(1, 1.03, 1.055), c(sl$idx[i], sl$ylab[i], sl$ylab[i]),
        col = adjustcolor(sl$col[i], 0.55), lwd = 0.7, xpd = NA)
  text(1.07, sl$ylab[i], paste0(sl$ch[i], "%"), adj = 0, cex = 0.6, font = 2,
       col = sl$col[i], xpd = NA)
  text(1.25, sl$ylab[i], sl$lab[i], adj = 0, cex = 0.55, col = "#666",
       xpd = NA)
}
points(0, 100, pch = 19, col = "#555555", cex = 0.7)
mtext("each category = 100 in 2010", side = 2, at = 100, las = 1, line = 0.3,
      cex = 0.55, col = "#555")
mtext(c("2010", "2020"), side = 3, at = c(0, 1), line = 1.2, cex = 0.75,
      font = 2)
mtext(paste("write-in areas added - 30 to 200 characters captured - recoded"),
      side = 3, line = 0.1, cex = 0.55, col = "#C41230", font = 2)
mtext("2010 and 2020 Censuses, P.L. 94-171, national totals", side = 1,
      line = 0.8, cex = 0.55, col = "#888", adj = 0, at = 0)

## ---- fig5-prep
# The four hues that stand for the four regions across this book, mixed
# toward white so the fills can carry dark labels and the heavy division
# borders on top of them.
RCOL <- c(Northeast = "#2c7fb8", Midwest = "#4d9221",
          South = "#C41230", West = "#e08214")
MPAL <- sapply(RCOL, function(k) colorRampPalette(c("#ffffff", k))(100)[45])
ms <- mlb[mlb$kind == "state", ]
i  <- match(ms$name, sta$name)
ms$st <- sta$st[i]; ms$total <- sta$total[i]; ms$entropy <- sta$entropy[i]
# room for two letters; the seven smallest states read by color, and by hover
slab <- ms[ms$area > 26000 | ms$abbr == "HI", ]
rlab <- data.frame(name = c("WEST", "MIDWEST", "NORTHEAST", "SOUTH"),
                   col  = RCOL[c("West", "Midwest", "Northeast", "South")],
                   x = c(-1700, 140, 1800, 450), y = c(3320, 3320, 3320, 260))
m_rx <- range(usr$x)
m_yl <- c(min(usr$y) - 30, max(usr$y) + 280)   # headroom for the region names
m_sc <- 760 / diff(m_rx)
m_fx <- function(x) (x - m_rx[1]) * m_sc
m_fy <- function(y) (m_yl[2] - y) * m_sc
MH   <- round(diff(m_yl) * m_sc)

## ---- fig5-d3
# ---------------------------------------------------------------------------
# FIGURE 5. Not a data figure but a key: the region and division rows of the
# file, drawn from the same rings as Figure 6 so the two trace identical
# shapes. Division borders ride on top as open paths.
# ---------------------------------------------------------------------------
P   <- ringpaths(usr, m_fx, m_fy)
ids <- ringids(usr)
stp <- tapply(P, ids, function(p) paste(p, collapse = ""))
mj  <- ms[match(names(stp), ms$st), ]
STJ <- paste0('{"n":"', mj$name, '","r":"', mj$region, '","dv":"', mj$division,
              '","pp":"', n(mj$total), '","e":"', pc(100 * mj$entropy / log(8)),
              '","c":"', MPAL[mj$region], '","p":"', stp, '"}', collapse = ",")
dvp <- vapply(split(dvr, interaction(dvr$division, dvr$part, drop = TRUE)),
              function(z) paste0("M", paste0(round(m_fx(z$x)), ",",
                                             round(m_fy(z$y)), collapse = "L")),
              character(1))
DVJ <- paste0('"', dvp, '"', collapse = ",")
SLJ <- paste0('{"x":', round(m_fx(slab$x)), ',"y":', round(m_fy(slab$y)),
              ',"t":"', slab$abbr, '"}', collapse = ",")
RLJ <- paste0('{"x":', round(m_fx(rlab$x)), ',"y":', round(m_fy(rlab$y)),
              ',"t":"', rlab$name, '","c":"', rlab$col, '"}', collapse = ",")
cat(paste0('
<div id="f5" style="position:relative;margin:1.1em 0"></div>
<script>
(function(){
const ST=[', STJ, '];
const DV=[', DVJ, '];
const SL=[', SLJ, '];
const RL=[', RLJ, '];
const W=760,H=', MH, ';
const wrap=d3.select("#f5");
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
      .html("<b>"+d.n+"</b><br>"+d.dv+" division, "+d.r+" region<br>"+
            d.pp+" counted in 2020, diversity "+d.e)
      .style("left",Math.min(m[0]+16,wrap.node().clientWidth-260)+"px")
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
Hover a state for its division, region, and 2020 count.</p>'))

## ---- fig5-static
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = m_rx, ylim = m_yl, asp = 1, axes = FALSE, ann = FALSE)
fill <- as.list(MPAL[ms$region]); names(fill) <- ms$st
drawrings(usr, fill, border = "#ffffff", lwd = 0.35)
for (z in split(dvr, interaction(dvr$division, dvr$part, drop = TRUE)))
  lines(z$x, z$y, col = "#4a4a4a", lwd = 1.4)
text(slab$x, slab$y, slab$abbr, cex = 0.45, col = "#333333")
text(rlab$x, rlab$y, rlab$name, cex = 0.95, font = 2, col = rlab$col)

## ---- region-table
o <- reg[reg$kind == "region", ]
o <- o[order(-o$total), ]
d <- data.frame(region = o$name, population = n(o$total))
for (k in c("hispanic", "nh_white", "nh_black", "nh_asian"))
  d[[SHORT[[k]]]] <- paste0(pc(100 * o[[k]] / o$total), "%")
d$`diversity score` <- pc(100 * o$entropy / log(8))
d

## ---- fig6-d3
# ---------------------------------------------------------------------------
# FIGURE 6. Six small-multiple choropleths on one shared Albers projection.
# Each panel has its OWN color ramp maximum, printed in the panel, because a
# shared scale would render five of the six panels blank -- White runs to 89%
# and NHPI to 0.4%. That is a real trade-off and the figure states it: the
# panels show WHERE a group is concentrated, and cannot be compared with each
# other for size. The bar in Figure 1 is where size lives.
# ---------------------------------------------------------------------------
PANEL <- c("nh_white", "hispanic", "nh_black", "nh_asian", "nh_two", "nh_aian")
PW <- 244L; GAPX <- 8L; TOPP <- 30L; BOTP <- 20L
rx <- range(usr$x); ry <- range(usr$y)
sc <- PW / diff(rx); PH <- round(diff(ry) * sc)
W5 <- 3L * PW + 2L * GAPX
H5 <- 2L * (TOPP + PH + BOTP)
mkp <- function(i) {
  k <- PANEL[i]
  cc <- (i - 1L) %% 3L; rr <- (i - 1L) %/% 3L
  offx <- cc * (PW + GAPX); offy <- rr * (TOPP + PH + BOTP)
  fx <- function(x) offx + (x - rx[1]) * sc
  fy <- function(y) offy + TOPP + (ry[2] - y) * sc
  P <- ringpaths(usr, fx, fy)
  ids <- ringids(usr)
  v <- 100 * sta[[k]] / sta$total
  vv <- v[match(ids, sta$st)]
  mx <- max(v)
  paste0("[", jstr(P), ",", jnum(round(pmin(vv / mx, 1), 3)), ",",
         offx, ",", offy, ',"', LAB[[k]], '","', COL[[k]], '","',
         pc(mx), '%","', sta$name[which.max(v)], '"]')
}
PANS <- vapply(seq_along(PANEL), mkp, character(1))
cat(paste0('
<div id="f6" style="margin:1.1em 0"></div>
<script>
(function(){
const P=[', paste(PANS, collapse = ","), '];
const W=', W5, ',H=', H5, ',PW=', PW, ',TOP=', TOPP, ',PH=', PH, ';
const svg=d3.select("#f6").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
P.forEach(p=>{
  const paths=p[0],v=p[1],ox=p[2],oy=p[3],lab=p[4],col=p[5],mx=p[6],who=p[7];
  const g=svg.append("g");
  g.selectAll("path").data(paths).join("path").attr("d",d=>d)
    .attr("fill",(d,j)=>d3.interpolate("#f4f4f4",col)(0.10+0.90*v[j]))
    .attr("stroke","#fff").attr("stroke-width",0.35);
  svg.append("text").attr("x",ox+6).attr("y",oy+13).attr("font-size","12.5px")
    .attr("font-weight","600").attr("fill",col).text(lab);
  svg.append("text").attr("x",ox+6).attr("y",oy+25).attr("font-size","10px")
    .attr("fill","#888").text("darkest = "+mx+", "+who);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Each panel is scaled to its own maximum, printed under the title. The panels
show where a group is concentrated; they cannot be compared with each other for
size.</p>'))

## ---- fig6-static
PANEL <- c("nh_white", "hispanic", "nh_black", "nh_asian", "nh_two", "nh_aian")
rx <- range(usr$x); ry <- range(usr$y)
layout(matrix(1:6, 2, 3, byrow = TRUE))
op <- par(mar = c(0.2, 0.2, 2.4, 0.2))
for (k in PANEL) {
  v <- 100 * sta[[k]] / sta$total
  mx <- max(v)
  ramp <- colorRampPalette(c("#f4f4f4", COL[[k]]))(101)
  fill <- as.list(ramp[round(100 * pmin(v / mx, 1) * 0.90 + 10) + 1])
  names(fill) <- sta$st
  plot(NA, xlim = rx, ylim = ry, asp = 1, axes = FALSE, ann = FALSE)
  drawrings(usr, fill, border = "#ffffff", lwd = 0.25)
  title(LAB[[k]], cex.main = 0.72, col.main = COL[[k]], line = 1.0)
  mtext(paste0("darkest = ", pc(mx), "%, ", sta$name[which.max(v)]),
        side = 3, line = 0.2, cex = 0.48, col = "#888")
}
par(op); layout(1)

## ---- fig7-d3
# ---------------------------------------------------------------------------
# FIGURE 7. A staircase, not a line chart: the rungs are not equally spaced in
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
<div id="f7" style="margin:1.1em 0"></div>
<script>
(function(){
const L=[', labs, '];
const P1="', steps(d1), '",P0="', steps(d0), '";
const W=', W6, ',H=', H6, ',LX=', L6, ',T=', T6, ',B=', B6, ',SW=', SW, ';
const svg=d3.select("#f7").append("svg").attr("viewBox","0 0 "+W+" "+H)
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

## ---- fig7-static
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

## ---- fig8-d3
# ---------------------------------------------------------------------------
# FIGURE 8. Three maps of one county, and the encoding is CATEGORICAL rather
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
<div id="f8" style="margin:1.1em 0"></div>
<script>
(function(){
const P=[', paste(PANW, collapse = ","), '],K=[', keyj, '];
const W=', Ww, ',H=', Hw, ',PW=', PWs, ',TOP=', TOPs, ';
const svg=d3.select("#f8").append("svg").attr("viewBox","0 0 "+W+" "+H)
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

## ---- fig8-static
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

## ---- fig9-d3
# ---------------------------------------------------------------------------
# FIGURE 9. A map and a profile, side by side, sharing one vertical axis: a
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
<div id="f9" style="margin:1.1em 0"></div>
<script>
(function(){
const BP=', jstr(BP), ',V=', jnum(ifelse(is.na(pb), -1, round(pb, 1))), ';
const DP=', jstr(DP), ',PR=[', prj, '];
const W=', W8, ',H=', H8, ',PX=', PX, ',PW=', PWp, ',T=', T8, ',MH=', MH, ';
const svg=d3.select("#f9").append("svg").attr("viewBox","0 0 "+W+" "+H)
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

## ---- fig9-static
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

## ---- on-mark-halo
# White labels drawn on pale and mid-toned marks, under 3:1 in BOTH themes.
# A halo fixes them, but the stroke must be dark against a white glyph in
# both themes, and no single token is: on the light page that is var(--ink),
# and on the dark page it is var(--paper). A --paper stroke on the light page
# would make white text worse, not better.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
#f1 text[fill="#fff" i],
#f1 text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#f1 text[fill="#fff" i],
#f1 text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
