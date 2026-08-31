# precinct-geography-code.R -- chunk bodies for precinct-geography-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6, dpi = 96, fig.retina = 1)
options(scipen = 999)
D  <- "../ga-precinct-returns/data"
HC <- "HOUSTON"                    # the running example: one county, knowable
a  <- read.csv(file.path(D, "derived/assign_point.csv"),      stringsAsFactors = FALSE)
ba <- read.csv(file.path(D, "derived/block_assign.csv"),      stringsAsFactors = FALSE)
ar <- read.csv(file.path(D, "derived/crosswalk.csv"),         stringsAsFactors = FALSE)
po <- read.csv(file.path(D, "derived/crosswalk_pop.csv"),     stringsAsFactors = FALSE)
cp <- read.csv(file.path(D, "derived/crosswalk_compare.csv"), stringsAsFactors = FALSE)
ck <- read.csv(file.path(D, "derived/crosswalk_check.csv"),   stringsAsFactors = FALSE)
ts <- read.csv("data/derived/fig_twosource.csv",                   stringsAsFactors = FALSE)

# 30 rows of the 2020 shapefile and 27 of the 2024 shapefile carry no county and
# no precinct name, so the build script's key collapses all of them to the single
# pseudo-precinct "NA|NA". It is not a precinct and must not be counted as one:
# left in, it contributes a spurious "split", and because its area weights and
# population weights describe different bundles of ground it also supplies the
# single largest area-vs-population disagreement in the file. Drop it everywhere.
ok   <- function(x) !is.na(x) & x != "NA|NA"
junk <- function(d) d[ok(d$from_2020) & ok(d$to_2024), ]
a <- junk(a); ar <- junk(ar); po <- junk(po); cp <- junk(cp)

cty <- function(x) sub("\\|.*$", "", x)      # "HOUSTON|ROZR" -> "HOUSTON"
nm  <- function(x) sub("^[^|]*\\|", "", x)   # "HOUSTON|ROZR" -> "ROZR"
pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
n   <- function(x) format(round(x), big.mark = ",")

# ---- statewide, and the same thing for one county -------------------------
p20 <- unique(a$from_2020); p24 <- unique(a$to_2024)
h20 <- sort(unique(a$from_2020[cty(a$from_2020) == HC]))
h24 <- sort(unique(a$to_2024[cty(a$to_2024)   == HC]))
aH  <- a[cty(a$from_2020) == HC, ]; aH <- aH[order(aH$from_2020), ]
arH <- ar[cty(ar$from_2020) == HC, ]
poH <- po[cty(po$from_2020) == HC, ]
cpH <- cp[cty(cp$from_2020) == HC, ]
baH <- ba[!is.na(ba$precinct_2020) & cty(ba$precinct_2020) == HC, ]

surv  <- function(A, B) 100 * length(intersect(A, B)) / length(A)
sp    <- function(d) 100 * mean(table(d$from_2020) > 1)
sp1   <- function(d) sp(d[d$weight >= 0.01, ])     # the same rule, 1% floor
sp_ar <- sp(ar);  sp_po <- sp(po)
sp_arH<- sp(arH); sp_poH<- sp(poH)

# every 2024 precinct in the county, including any the interior-point table
# cannot see because it contains no 2020 precinct's interior point
h24all <- sort(unique(ba$precinct_2024[!is.na(ba$precinct_2024) &
                                       cty(ba$precinct_2024) == HC]))
unseen <- setdiff(nm(h24all), nm(h24))

# a 2020 precinct is a RENAME if population weighting sends every one of its
# people to a single 2024 precinct that carries a different name; it is REDRAWN
# if its people end up in more than one. Computed, not asserted.
ren    <- poH[poH$weight > 0.999 & nm(poH$from_2020) != nm(poH$to_2024), ]
REN20  <- nm(ren$from_2020); REN24 <- nm(ren$to_2024)
splsrc <- names(which(table(poH$from_2020) > 1))
RDR20  <- nm(splsrc); RDR24 <- nm(poH$to_2024[poH$from_2020 %in% splsrc])

# ---- geometry for the maps, precomputed by data/build-data.R ---------
# Long-format coordinate tables in kilometers, small enough to keep the knitted
# document light and simple enough that base R can draw them for the PDF.
fg <- read.csv("data/derived/fig_houston.csv",      stringsAsFactors = FALSE)
fl <- read.csv("data/derived/fig_houston_lab.csv",  stringsAsFactors = FALSE)
wb <- read.csv("data/derived/fig_rozr_blocks.csv",  stringsAsFactors = FALSE)
wp <- read.csv("data/derived/fig_rozr_pts.csv",     stringsAsFactors = FALSE)
wo <- read.csv("data/derived/fig_rozr_outline.csv", stringsAsFactors = FALSE)
wm <- read.csv("data/derived/fig_rozr_meta.csv",    stringsAsFactors = FALSE)

fl$cat <- ifelse(fl$yr == 2020,
           ifelse(fl$name %in% RDR20, "redrawn",
           ifelse(fl$name %in% REN20, "renamed", "same")),
           ifelse(fl$name %in% RDR24, "redrawn",
           ifelse(fl$name %in% REN24, "renamed", "same")))
CAT  <- c(same = "#f2f2f2", renamed = "#fbe3c2", redrawn = "#cfe3f1")
CATB <- c(same = "#9a9a9a", renamed = "#c07d20", redrawn = "#2c7fb8")

WCOL <- c("ROZR" = "#2c7fb8", "PEC" = "#e08214")
HPOP <- sum(baH$pop)
WBAL <- wm$ballots_total[1]                       # ballots cast in ROZR, 2020
w2   <- wm[match(names(WCOL), wm$target), ]
wcol <- function(t) { z <- unname(WCOL[t]); ifelse(is.na(z), "#a8a8a8", z) }
PECA <- wm$w_area[wm$target == "PEC"]             # PEC's share of ROZR, by land
PECP <- wm$w_pop[wm$target == "PEC"]              # ...and by people
PECB <- wm$ballots_area[wm$target == "PEC"] - wm$ballots_pop[wm$target == "PEC"]

# The disaggregate/reaggregate schematic is a sketch, not a map, but its two
# renderings must not disagree. The sixteen block populations live here once,
# both versions read them, and the two percentages it prints are formatted once,
# in R, and passed through as strings.
POPG <- matrix(c(2, 1, 4, 1,  6, 3, 9, 5,  22, 34, 18, 26,  30, 41, 27, 33),
               4, 4, byrow = TRUE)
SKA  <- 100 * sum(POPG[1:2, ]) / sum(POPG)
SKB  <- 100 - SKA

# every ring of a long-format polygon table, as one SVG path each
paths <- function(d, by, sx, sy) {
  k <- interaction(d[[by]], d$part, drop = TRUE)
  vapply(split(d, k), function(z)
    paste0("M", paste(sprintf("%.1f,%.1f", sx(z$x), sy(z$y)), collapse = "L"), "Z"),
    character(1))
}
scalebar <- function(km, x, y, lab) {
  lines(c(x, x + km), c(y, y), lwd = 2.2)
  text(x + km/2, y, lab, pos = 3, cex = 0.6, offset = 0.2)
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

## ---- raw20
# Verbatim transcripts, captured from the committed shapefiles in
# ../ga-precinct-returns/data/. Everything the prose counts is read back out of
# this text at knit time rather than asserted beside it.
R20 <- c(
"> dim(d)",
"[1] 2684  115",
"",
"> names(d)[1:20]",
" [1] \"ID\"         \"AREA\"       \"DATA\"       \"DISTRICT\"   \"CTYSOSID\"  ",
" [6] \"PRECINCT_I\" \"PRECINCT_N\" \"CNTY\"       \"FIPS2\"      \"CTYNAME\"   ",
"[11] \"REG20\"      \"VOTED20\"    \"VOTED201\"   \"TRUMP20\"    \"BIDEN20\"   ",
"[16] \"JORGENSON2\" \"PURDUE20\"   \"OSSOFF20\"   \"HAZEL20\"    \"LOEFFLER20\"",
"",
"> k <- c(\"CTYNAME\", \"PRECINCT_I\", \"PRECINCT_N\", \"TRUMP20\", \"BIDEN20\")",
"> head(d[, k], 3)",
"   CTYNAME PRECINCT_I           PRECINCT_N TRUMP20 BIDEN20",
"1 COLUMBIA        131 JOURNEY COMM. CHURCH     808     238",
"2 COLUMBIA        064 GRACE BAPTIST CHURCH    1526    1171",
"3 COLUMBIA        061      GREENBRIER HIGH    1871     793")
R24 <- c(
"> dim(e)",
"[1] 2724   13",
"",
"> names(e)",
" [1] \"ID\"         \"AREA\"       \"DATA\"       \"DATA1\"      \"DISTRICT\"  ",
" [6] \"CTYSOSID\"   \"FIPS\"       \"FIPS2\"      \"CTYNAME\"    \"CONTY\"     ",
"[11] \"COUNTY\"     \"PRECINCT_I\" \"PRECINCT_N\"")
RBL <- c(
"> dim(b)",
"[1] 232717     17",
"",
"> kb <- c(\"GEOID20\", \"POP20\", \"ALAND20\", \"INTPTLAT20\", \"INTPTLON20\")",
"> head(b[, kb], 3)",
"          GEOID20 POP20 ALAND20  INTPTLAT20   INTPTLON20",
"1 132532001001042     7   12678 +30.8017174 -084.8413455",
"2 132532002003085    11   32388 +31.0257042 -084.8925507",
"3 130990905001082     2 2977358 +31.1995307 -085.0500314")
dimof <- function(x) {
  s <- sub("^\\[[0-9]+\\] *", "", x[2])          # drop R's own "[1] " index
  as.integer(regmatches(s, gregexpr("[0-9]+", s))[[1]])
}
D20 <- dimof(R20); D24 <- dimof(R24); DBL <- dimof(RBL)

# The transcripts hold two different things -- a list of column names and a few
# sample rows -- so they become two kinds of table rather than one block of
# console output.
namesof <- function(x) {
  ln <- grep('^\\s*\\[[0-9]+\\]', x, value = TRUE)
  nm <- unlist(regmatches(ln, gregexpr('"[^"]+"', ln)))
  gsub('"', "", nm)
}
namestab <- function(x) {
  nm <- namesof(x)
  # No codebook ships with a DBF, so these readings are inferred from the
  # values in the column and from what the rest of the file is doing. Where
  # the name is truncated past recovery the entry says so rather than guess.
  d <- c(
    ID = "row number inside the shapefile",
    AREA = "the polygon's area, in the shapefile's own units",
    DATA = "unlabelled — the name says nothing and the values do not either",
    DATA1 = "a second unlabelled column",
    DISTRICT = "a district number, which district is not stated",
    CTYSOSID = "the Secretary of State's county identifier",
    FIPS = "county FIPS code",
    FIPS2 = "a second FIPS-like code, differently padded",
    CTYNAME = "county name — half of the key this chapter has to manufacture",
    CONTY = "sits immediately before COUNTY; appears to be a typo that shipped",
    COUNTY = "county",
    PRECINCT_I = "precinct identifier",
    PRECINCT_N = "precinct name — the other half of the manufactured key",
    CNTY = "county code",
    REG20 = "registered voters, 2020",
    VOTED20 = "ballots cast, 2020",
    VOTED201 = "a second turnout column, distinguished only by the digit DBF appended",
    TRUMP20 = "votes for Trump, 2020",
    BIDEN20 = "votes for Biden, 2020",
    JORGENSON2 = "votes for Jorgensen — the name truncated at ten characters",
    PURDUE20 = "votes for Purdue, 2020 Senate",
    OSSOFF20 = "votes for Ossoff, 2020 Senate",
    HAZEL20 = "votes for Hazel, 2020 Senate",
    LOEFFLER20 = "votes for Loeffler, 2020 Senate special")
  data.frame(Position = seq_along(nm),
             Column_name_in_the_file = nm,
             What_it_appears_to_hold = ifelse(nm %in% names(d), d[nm], "—"))
}
namestab(R20)

## ---- raw20-rows
# Transcribed from the same capture. A printed data.frame cannot be split on
# whitespace -- "JOURNEY COMM. CHURCH" is one value -- so the three rows are
# written out as the values they are.
data.frame(
  CTYNAME    = c("COLUMBIA", "COLUMBIA", "COLUMBIA"),
  PRECINCT_I = c("131", "064", "061"),
  PRECINCT_N = c("JOURNEY COMM. CHURCH", "GRACE BAPTIST CHURCH",
                 "GREENBRIER HIGH"),
  TRUMP20    = c(808, 1526, 1871),
  BIDEN20    = c(238, 1171, 793))

## ---- raw24
namestab(R24)

## ---- rawbl
# Three of the block file's rows, on the five columns the chapter uses.
data.frame(
  GEOID20    = c("132532001001042", "132532002003085", "130990905001082"),
  POP20      = c(7, 11, 2),
  ALAND20    = c(12678, 32388, 2977358),
  INTPTLAT20 = c("+30.8017174", "+31.0257042", "+31.1995307"),
  INTPTLON20 = c("-084.8413455", "-084.8925507", "-085.0500314"))

## ---- cleanblocks
z <- baH[nm(baH$precinct_2020) == "ROZR", ]
z <- z[order(z$GEOID20), ]
o <- rbind(head(z[nm(z$precinct_2024) == "ROZR", ], 2),
           head(z[nm(z$precinct_2024) == "PEC",  ], 1))
o$GEOID20 <- sprintf("%.0f", o$GEOID20)
o

## ---- cleanxwalk
poH[nm(poH$from_2020) == "ROZR", c("from_2020", "to_2024", "weight", "pop")]

## ---- ownership
data.frame(
  fact = c("Who holds the official boundary", "Who may change it, and when",
           "What the statewide file is", "What it is fitted to"),
  answer = c("Each of 159 counties, separately",
             "County election officials, at any time",
             "A compilation of 159 county maps",
             "Census geography — 'best fit', not exact"),
  check.names = FALSE)

## ---- moved
data.frame(
  quantity = c("Precinct names, 2020", "Precinct names, 2024",
               "Names present in both", "Names gone by 2024", "Names new in 2024",
               "Share of names surviving (%)"),
  Houston = c(n(length(h20)), n(length(h24)), n(length(intersect(h20, h24))),
              n(length(setdiff(h20, h24))), n(length(setdiff(h24, h20))),
              pc(surv(h20, h24))),
  Georgia = c(n(length(p20)), n(length(p24)), n(length(intersect(p20, p24))),
              n(length(setdiff(p20, p24))), n(length(setdiff(p24, p20))),
              pc(surv(p20, p24))),
  check.names = FALSE)

## ---- inventory
INV <- data.frame(
  `2020 precinct` = nm(aH$from_2020),
  `became, in 2024` = nm(aH$to_2024),
  outcome = ifelse(nm(aH$from_2020) == nm(aH$to_2024),
                   ifelse(nm(aH$from_2020) %in% RDR20,
                          "kept its name, lost part of its ground", "unchanged"),
                   "same ground, new name"),
  check.names = FALSE)
INV

## ---- inventory-new
newnames <- setdiff(nm(h24all), nm(h20))
src <- vapply(newnames, function(t) {
  z <- poH[nm(poH$to_2024) == t, ]
  if (!nrow(z)) return("—")
  z <- z[order(-z$pop), ]
  paste0(nm(z$from_2020[1]), " supplied ", pc(100 * z$pop[1] / sum(z$pop)), "%")
}, character(1))
data.frame(`2024 name that did not exist in 2020` = newnames,
           `who its people used to belong to` = unname(src),
           check.names = FALSE)

## ---- houston-d3
W <- 760; PH <- 400; PW <- 368; PAD <- 10; GAP <- 24
rx <- range(fg$x); ry <- range(fg$y)
s  <- min((PW - 2*PAD) / diff(rx), (PH - 2*PAD) / diff(ry))
mkx <- function(off) function(x) off + (PW - diff(rx)*s)/2 + (x - rx[1]) * s
mky <- function(y) PAD + 22 + (ry[2] - y) * s

J <- c(); LAB <- c()
for (i in 1:2) {
  Y <- c(2020, 2024)[i]
  d <- fg[fg$yr == Y, ]; l <- fl[fl$yr == Y, ]
  fx <- mkx((i - 1) * (PW + GAP))
  P  <- paths(d, "name", fx, mky)
  nn <- sub("\\.[0-9]+$", "", names(P))
  J  <- c(J, sprintf('{"y":%d,"n":"%s","c":"%s","d":"%s"}', Y,
                     gsub('"', "", nn), l$cat[match(nn, l$name)], P))
  LAB <- c(LAB, sprintf('{"y":%d,"x":%.1f,"yy":%.1f,"t":"%s"}',
                        Y, fx(l$x), mky(l$y), l$code))
}
# where each 2020 precinct's PEOPLE went
f <- poH[poH$weight >= 0.005, ]
f <- f[order(f$from_2020, -f$weight), ]
K <- vapply(split(f, f$from_2020), function(z)
       sprintf('"%s":[%s]', nm(z$from_2020[1]),
               paste(sprintf('{"t":"%s","w":%.3f}', nm(z$to_2024), z$weight),
                     collapse = ",")), character(1))
cat(sprintf('
<div id="hou" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const G=[%s], L=[%s], X={%s};
const W=%d,H=%d;
const FILL={same:"#f2f2f2",renamed:"#fbe3c2",redrawn:"#cfe3f1"};
const EDGE={same:"#9a9a9a",renamed:"#c07d20",redrawn:"#2c7fb8"};
const svg=d3.select("#hou").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
[[0,"2020 \\u2014 %d precincts"],[1,"2024 \\u2014 %d precincts"]].forEach(([i,t])=>
  svg.append("text").attr("x",i*%d+%d).attr("y",14).attr("font-size","13px")
     .attr("font-weight","600").attr("fill","#333").text(t));
const g=svg.append("g");
const sel=g.selectAll("path").data(G).join("path")
  .attr("d",d=>d.d).attr("stroke",d=>EDGE[d.c]).attr("stroke-width",0.8)
  .attr("fill",d=>FILL[d.c]);
g.selectAll("text").data(L).join("text").attr("x",d=>d.x).attr("y",d=>d.yy)
  .attr("text-anchor","middle").attr("font-size","8.5px").attr("fill","#444")
  .attr("pointer-events","none").text(d=>d.t);
const cap=d3.select("#hou").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:3.4em;margin:0.4em 0 0 0");
const DEF="<b style=\\"color:#c07d20\\">Amber</b>: the name changed but the ground "+
  "did not \\u2014 %d polling places, renamed. <b style=\\"color:#2c7fb8\\">Blue</b>: "+
  "the ground itself changed \\u2014 ROZR split, and its western half became the new "+
  "precinct PEC. Everything gray is the same precinct under the same name in both "+
  "years. <i>Hover a 2020 precinct to see where its people went.</i>";
cap.html(DEF);
function clear(){sel.attr("fill",d=>FILL[d.c]).attr("stroke",d=>EDGE[d.c])
  .attr("stroke-width",0.8);cap.html(DEF);}
sel.filter(d=>d.y===2020).style("cursor","pointer")
 .on("mouseenter",function(e,d){
   const tg=X[d.n]; if(!tg) return;
   const m={}; tg.forEach(t=>m[t.t]=t.w);
   sel.attr("fill",q=>{
     if(q.y===2020) return q.n===d.n?"#2c7fb8":"#f4f4f4";
     return m[q.n]!==undefined?d3.interpolateOranges(0.25+0.6*m[q.n]):"#f4f4f4";
   }).attr("stroke",q=>(q.y===2020&&q.n===d.n)?"#15476b":"#9a9a9a")
     .attr("stroke-width",q=>(q.y===2020&&q.n===d.n)?2:0.8);
   cap.html(`<b style="color:#2c7fb8">${d.n}</b> sent its people to `+
     `<b>${tg.length}</b> 2024 precinct${tg.length>1?"s":""}: `+
     tg.map(t=>`${t.t} <b>${(100*t.w).toFixed(0)}%%</b>`).join(" \\u00b7 "));
 }).on("mouseleave",clear);
})();
</script>
', paste(J, collapse = ","), paste(LAB, collapse = ","), paste(K, collapse = ","),
   W, PH + 22, length(h20), length(h24all), PW + GAP, PAD, length(REN20)))

## ---- houston-static
op <- par(mfrow = c(1, 2), mar = c(0.2, 0.2, 1.8, 0.2))
for (Y in c(2020, 2024)) {
  d <- fg[fg$yr == Y, ]; l <- fl[fl$yr == Y, ]
  plot(NA, xlim = range(fg$x), ylim = range(fg$y) + c(-2.4, 0), asp = 1,
       axes = FALSE, ann = FALSE)
  k <- interaction(d$name, d$part, drop = TRUE)
  for (i in seq_along(levels(k))) {
    z <- d[k == levels(k)[i], ]
    j <- match(z$name[1], l$name)
    polygon(z$x, z$y, col = CAT[l$cat[j]], border = CATB[l$cat[j]], lwd = 0.7)
  }
  text(l$x, l$y, l$code, cex = 0.42, col = "#333333")
  title(sprintf("%d - %d precincts", Y, nrow(l)), cex.main = 0.95, line = 0.3)
  if (Y == 2020) scalebar(5, min(fg$x) + 1, min(fg$y) - 1.6, "5 km")
}
par(op)

## ---- houston-static-legend
par(mar = rep(0, 4))
plot(NA, xlim = c(0, 100), ylim = c(0, 10), axes = FALSE, ann = FALSE)
legend(0, 10, horiz = TRUE, bty = "n", cex = 0.62, pt.cex = 1.3, pch = 22,
       pt.bg = unname(CAT[c("same", "renamed", "redrawn")]),
       col = unname(CATB[c("same", "renamed", "redrawn")]),
       legend = c("same name, same ground", "renamed, same ground",
                  "the ground itself changed"))

## ---- drop
dropped <- setdiff(h20, h24)
data.frame(
  approach = c("Match on name, drop the rest", "Put both years on one map"),
  `keeps, in Houston` = c(paste0(n(length(intersect(h20, h24))), " of ",
                                 n(length(h20)), " precincts"), "everything"),
  `keeps, statewide` = c(paste0(pc(100 * length(intersect(p20, p24)) /
                                   length(union(p20, p24))), "% of names"),
                         "everything"),
  `what it biases` = c("Removes changing areas preferentially",
                       "Nothing, if the carry is checked"),
  check.names = FALSE)

## ---- blocks
data.frame(
  quantity = c("Census blocks", "Population they carry",
               "Placed in a 2020 precinct", "Placed in a 2024 precinct"),
  Houston = c(n(nrow(baH)), n(sum(baH$pop, na.rm = TRUE)),
              paste0(n(sum(!is.na(baH$precinct_2020))), " (",
                     pc(100*mean(!is.na(baH$precinct_2020)), 2), "%)"),
              paste0(n(sum(!is.na(baH$precinct_2024))), " (",
                     pc(100*mean(!is.na(baH$precinct_2024)), 2), "%)")),
  Georgia = c(n(nrow(ba)), n(sum(ba$pop, na.rm = TRUE)),
              paste0(n(sum(!is.na(ba$precinct_2020))), " (",
                     pc(100*mean(!is.na(ba$precinct_2020)), 2), "%)"),
              paste0(n(sum(!is.na(ba$precinct_2024))), " (",
                     pc(100*mean(!is.na(ba$precinct_2024)), 2), "%)")),
  check.names = FALSE)

## ---- method
data.frame(
  method = c("Intersect polygons", "Interior point in polygon"),
  produces = c("Fragments, including slivers", "One answer per block"),
  fails_when = c("Two agencies draw the same line differently",
                 "A block genuinely straddles a boundary"),
  check.names = FALSE)

## ---- recipe-d3
# The block populations and the two percentages come from POPG in the setup
# chunk, so this figure and the base-R one below cannot drift apart.
POPJS <- paste0("[", paste(apply(POPG, 1, function(r)
  paste0("[", paste(r, collapse = ","), "]")), collapse = ","), "]")
cat(paste0('
<div id="rcp" style="margin:1.2em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const W=760,H=340;
const BLU="#2c7fb8",ORA="#b3651a",GRN="#4d9221",RED="#C41230",GRY="#8c8c8c";
const svg=d3.select("#rcp").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const T=(x,y,s,o)=>{const t=svg.append("text").attr("x",x).attr("y",y)
  .attr("text-anchor",(o&&o.a)||"middle").attr("font-size",(o&&o.s)||"12px")
  .attr("fill",(o&&o.c)||"#333");
  if(o&&o.b)t.attr("font-weight","600");t.text(s);return t;};
// stage headers
[[88,"1","THE OLD UNIT","2020 precinct",BLU],
 [380,"2","THE COMMON UNIT","census blocks",GRN],
 [660,"3","THE NEW UNIT","2024 precincts","#666"]].forEach(([x,n,a,b,c])=>{
  T(x,20,n,{b:1,c:RED,s:"15px"});T(x,40,a,{b:1,c:RED,s:"11px"});
  T(x,58,b,{b:1,c:c,s:"13px"});});
// stage 1
svg.append("rect").attr("x",10).attr("y",70).attr("width",156).attr("height",170)
  .attr("fill","#eaf2f8").attr("stroke",BLU).attr("stroke-width",2).attr("rx",4);
T(88,140,"votes",{b:1,c:BLU,s:"20px"});
T(88,163,"counted here",{c:"#4a6b83",s:"12px"});
T(88,205,"one total,",{c:GRY,s:"10.5px"});T(88,219,"no interior detail",{c:GRY,s:"10.5px"});
// stage 2: blocks
svg.append("rect").attr("x",300).attr("y",70).attr("width",160).attr("height",170)
  .attr("fill","#fcfcfc").attr("stroke",GRY).attr("stroke-width",2).attr("rx",4);
const POP=', POPJS, ';
for(let i=0;i<4;i++)for(let j=0;j<4;j++){
  const x=306+j*38,y=76+i*40;
  svg.append("rect").attr("x",x).attr("y",y).attr("width",34).attr("height",36)
    .attr("fill",i<2?"#fbeedd":"#e7f0f7").attr("stroke","#d2d2d2").attr("rx",2);
  svg.append("circle").attr("cx",x+17).attr("cy",y+18)
    .attr("r",1.35*Math.sqrt(POP[i][j])).attr("fill",GRN).attr("fill-opacity",0.8);}
T(380,258,"each dot is that block\\u2019s population",{c:"#555",s:"10.5px"});
// stage 3
svg.append("rect").attr("x",580).attr("y",70).attr("width",160).attr("height",74)
  .attr("fill","#fbeedd").attr("stroke",ORA).attr("stroke-width",2).attr("rx",4);
T(660,113,"2024 precinct A",{b:1,c:ORA,s:"13px"});
svg.append("rect").attr("x",580).attr("y",166).attr("width",160).attr("height",74)
  .attr("fill","#e7f0f7").attr("stroke",BLU).attr("stroke-width",2).attr("rx",4);
T(660,209,"2024 precinct B",{b:1,c:BLU,s:"13px"});
// arrows
svg.append("defs").append("marker").attr("id","ah").attr("viewBox","0 0 10 10")
  .attr("refX",9).attr("refY",5).attr("markerWidth",6).attr("markerHeight",6)
  .attr("orient","auto").append("path").attr("d","M0,0L10,5L0,10Z").attr("fill",RED);
const AR=(x1,y1,x2,y2)=>svg.append("line").attr("x1",x1).attr("y1",y1)
  .attr("x2",x2).attr("y2",y2).attr("stroke",RED).attr("stroke-width",2.2)
  .attr("marker-end","url(#ah)");
AR(172,155,294,155);AR(466,130,574,107);AR(466,180,574,203);
T(233,143,"DISAGGREGATE",{b:1,c:RED,s:"11px"});
T(233,176,"split the votes",{c:"#666",s:"10px"});
T(233,189,"across the blocks",{c:"#666",s:"10px"});
T(520,166,"REAGGREGATE",{b:1,c:RED,s:"11px"});
T(478,92,"half the land,",{c:ORA,s:"10px",a:"start"});
T(478,105,"', pc(SKA, 0), '% of the people",{c:ORA,s:"10px",a:"start"});
T(478,222,"half the land,",{c:BLU,s:"10px",a:"start"});
T(478,235,"', pc(SKB, 0), '% of the people",{c:BLU,s:"10px",a:"start"});
// the arithmetic
svg.append("line").attr("x1",10).attr("y1",282).attr("x2",750).attr("y2",282)
  .attr("stroke","#ddd");
T(408,313,"votes carried into A  =  2020 votes  \\u00d7",{b:1,s:"13px",a:"end"});
T(535,305,"people in the blocks that land in A",{s:"12px"});
svg.append("line").attr("x1",418).attr("y1",310).attr("x2",652).attr("y2",310)
  .attr("stroke","#333");
T(535,326,"people in all the blocks",{s:"12px"});
})();
</script>'))

## ---- recipe-static
par(mar = rep(0.1, 4))
plot(NA, xlim = c(0, 100), ylim = c(0, 56), asp = NA, axes = FALSE, ann = FALSE)
BLU <- "#2c7fb8"; ORA <- "#b3651a"; GRN <- "#4d9221"; RED <- "#C41230"; GRY <- "#8c8c8c"

rect(0.5, 16, 22, 42, col = "#eaf2f8", border = BLU, lwd = 1.8)
text(11.2, 51.5, "1", cex = 0.85, font = 2, col = RED)
text(11.2, 46.6, "THE OLD UNIT", cex = 0.62, font = 2, col = RED)
text(11.2, 43.6, "2020 precinct", cex = 0.72, font = 2, col = BLU)
text(11.2, 32, "votes", cex = 0.95, font = 2, col = BLU)
text(11.2, 27.5, "counted here", cex = 0.66, col = "#4a6b83")
text(11.2, 20.5, "one total,\nno interior detail", cex = 0.56, col = GRY)

BX <- 35.5; BY <- 16; BW <- 29; BH <- 26
rect(BX, BY, BX + BW, BY + BH, col = "#fcfcfc", border = GRY, lwd = 1.8)
text(BX + BW/2, 51.5, "2", cex = 0.85, font = 2, col = RED)
text(BX + BW/2, 46.6, "THE COMMON UNIT", cex = 0.62, font = 2, col = RED)
text(BX + BW/2, 43.6, "census blocks", cex = 0.72, font = 2, col = GRN)
cw <- (BW - 2.4) / 4; chh <- (BH - 2.4) / 4
for (i in 1:4) for (j in 1:4) {
  x0 <- BX + 1.2 + (j - 1) * cw; y0 <- BY + 1.2 + (4 - i) * chh
  rect(x0, y0, x0 + cw - 0.5, y0 + chh - 0.5,
       col = if (i <= 2) "#fbeedd" else "#e7f0f7", border = "#d2d2d2", lwd = 0.6)
  points(x0 + cw/2 - 0.25, y0 + chh/2 - 0.25, pch = 19,
         cex = 0.33 * sqrt(POPG[i, j]), col = adjustcolor(GRN, 0.8))
}
text(BX + BW/2, BY - 2.6, "each dot is that block's population", cex = 0.6, col = "#555")

rect(78, 31, 99.5, 42, col = "#fbeedd", border = ORA, lwd = 1.8)
text(88.7, 36.8, "2024 precinct A", cex = 0.72, font = 2, col = ORA)
rect(78, 16, 99.5, 27, col = "#e7f0f7", border = BLU, lwd = 1.8)
text(88.7, 21.8, "2024 precinct B", cex = 0.72, font = 2, col = BLU)
text(88.7, 51.5, "3", cex = 0.85, font = 2, col = RED)
text(88.7, 46.6, "THE NEW UNIT", cex = 0.62, font = 2, col = RED)
text(88.7, 43.6, "2024 precincts", cex = 0.72, font = 2, col = "#666")

arrows(23.0, 29, 34.5, 29, length = 0.09, lwd = 2, col = RED)
text(28.7, 32.4, "DISAGGREGATE", cex = 0.56, font = 2, col = RED)
text(28.7, 25.6, "split the votes\nacross the blocks", cex = 0.52, col = "#666")
arrows(65.5, 32, 77.2, 35.5, length = 0.09, lwd = 2, col = RED)
arrows(65.5, 26, 77.2, 22.5, length = 0.09, lwd = 2, col = RED)
text(71.3, 29, "REAGGREGATE", cex = 0.56, font = 2, col = RED)
text(71.3, 38.8, paste0("half the land,\n", pc(SKA, 0), "% of the people"),
     cex = 0.44, col = ORA)
text(71.3, 17.2, paste0("half the land,\n", pc(SKB, 0), "% of the people"),
     cex = 0.44, col = BLU)

segments(0.5, 10.5, 99.5, 10.5, col = "#dddddd")
text(50, 6.2, expression(bold("votes carried into A") == bold("2020 votes") %*%
     frac("people in the blocks that land in A", "people in all the blocks")), cex = 0.72)

## ---- weights
data.frame(
  method = c("By area", "By population"),
  weight_is = c("Share of the precinct's land in each target",
                "Share of the precinct's people in each target"),
  `Houston precincts it calls split` =
    c(paste0(n(sum(table(arH$from_2020) > 1)), " of ", n(length(h20)),
             "  (", pc(sp_arH), "%)"),
      paste0(n(sum(table(poH$from_2020) > 1)), " of ", n(length(h20)),
             "  (", pc(sp_poH), "%)")),
  `Georgia (%)` = c(pc(sp_ar), pc(sp_po)),
  check.names = FALSE)

## ---- sliver
sec <- do.call(rbind, lapply(split(arH, arH$from_2020), function(z) {
  z <- z[order(-z$weight), ]; if (nrow(z) > 1) z[2, ] else NULL }))
data.frame(
  quantity = c("Precincts area calls split",
               "...whose second-largest piece is under 1% of the precinct",
               "Largest such piece, other than ROZR (%)",
               "Precincts area calls split at a 1% floor (%)",
               "Precincts population calls split at a 1% floor (%)"),
  Houston = c(n(sum(table(arH$from_2020) > 1)),
              n(sum(sec$weight < 0.01)),
              pc(100 * max(sec$weight[nm(sec$from_2020) != "ROZR"]), 2),
              pc(sp1(arH)), pc(sp1(poH))),
  Georgia = c(n(sum(table(ar$from_2020) > 1)), "—", "—",
              pc(sp1(ar)), pc(sp1(po))),
  check.names = FALSE)

## ---- rozr-d3
MW <- 470; MH <- 400; MP <- 14
bx <- range(wb$x); by <- range(wb$y)
ss <- min((MW - 2*MP) / diff(bx), (MH - 2*MP - 16) / diff(by))
sx <- function(x) MP + (x - bx[1]) * ss
sy <- function(y) MP + 16 + (by[2] - y) * ss
BP <- paths(wb, "id", sx, sy)
bid <- as.integer(sub("\\.[0-9]+$", "", names(BP)))
OP <- paths(wo, "tgt", sx, sy)
oid <- sub("\\.[0-9]+$", "", names(OP))

blk <- paste(sprintf('{"d":"%s","t":"%s","p":%d}', BP,
                     wb$tgt[match(bid, wb$id)], wb$pop[match(bid, wb$id)]),
             collapse = ",")
otl <- paste(sprintf('{"d":"%s","t":"%s"}', OP, oid), collapse = ",")
pts <- paste(sprintf('{"x":%.1f,"y":%.1f,"r":%.2f,"t":"%s","p":%d}',
                     sx(wp$x), sy(wp$y), 9 * sqrt(wp$pop) / sqrt(max(wp$pop)),
                     wp$tgt, wp$pop), collapse = ",")
# Format the shared percentages ONCE, here, and pass the strings. R rounds half
# to even and JavaScript rounds half up, so 0.4445 formatted on each side gives
# 44.4 in the PDF and 44.5 in the HTML -- the two paths would print different
# numbers for the same weight. The bar heights still use the full-precision
# values; only the printed labels come from R.
bar <- paste(sprintf('{"t":"%s","a":%.6f,"p":%.6f,"la":"%s","lp":"%s","ba":%d,"bp":%d}',
                     w2$target, w2$w_area, w2$w_pop,
                     pc(100 * w2$w_area), pc(100 * w2$w_pop),
                     w2$ballots_area, w2$ballots_pop),
             collapse = ",")
cat(sprintf('
<div id="wnd" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const B=[%s],O=[%s],P=[%s],BAR=[%s];
const W=760,H=%d;
const C={"ROZR":"#2c7fb8","PEC":"#e08214"};
const cc=t=>C[t]||"#a8a8a8";
const svg=d3.select("#wnd").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",12).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("font-weight","600").attr("fill","#333")
  .text("%s census blocks inside one 2020 precinct");
const map=svg.append("g");
map.selectAll("path.b").data(B).join("path").attr("class","b").attr("d",d=>d.d)
  .attr("fill",d=>cc(d.t)).attr("fill-opacity",0.16).attr("stroke","#fff")
  .attr("stroke-width",0.4);
map.selectAll("path.o").data(O).join("path").attr("class","o").attr("d",d=>d.d)
  .attr("fill","none").attr("stroke",d=>cc(d.t)).attr("stroke-width",2);
const tip=d3.select("#wnd").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
map.selectAll("circle").data(P).join("circle").attr("cx",d=>d.x).attr("cy",d=>d.y)
  .attr("r",d=>d.r).attr("fill",d=>cc(d.t)).attr("fill-opacity",0.85)
  .on("mousemove",function(e,d){tip.style("opacity",1)
    .html(`<b>${d3.format(",")(d.p)}</b> people<br>goes to ${d.t}`)
    .style("left",Math.min(e.offsetX+14,W-180)+"px").style("top",(e.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
// legend
const lg=svg.append("g").attr("transform","translate(14,%d)");
[["#2c7fb8","stays in ROZR"],["#e08214","goes to the new precinct PEC"]]
 .forEach((r,i)=>{
  lg.append("circle").attr("cy",i*16).attr("r",4.5).attr("fill",r[0]);
  lg.append("text").attr("x",11).attr("y",i*16+4).attr("font-size","11px").text(r[1]);});
// the two stacked bars
const BX=560,BW=64,BT=46,BH=280;
svg.append("text").attr("x",645).attr("y",12).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("font-weight","600").attr("fill","#333")
  .text("how its %s ballots get split");
[["a","by AREA",0],["p","by POPULATION",1]].forEach(([k,lab,i])=>{
  const x=BX+i*98; let cum=0;
  // drawn top-down, so reverse the rows: the static path stacks from y=0 upward
  // and both must end up with the same target at the bottom
  BAR.slice().reverse().forEach(d=>{
    const h=d[k]*BH;
    svg.append("rect").attr("x",x).attr("y",BT+cum).attr("width",BW).attr("height",h)
      .attr("fill",cc(d.t)).attr("fill-opacity",0.9).attr("stroke","#fff");
    svg.append("text").attr("x",x+BW/2).attr("y",BT+cum+h/2-2).attr("text-anchor","middle")
      .attr("font-size","12px").attr("font-weight","600").attr("fill","#fff")
      .text(d.t+" "+(k==="a"?d.la:d.lp)+"%%");
    svg.append("text").attr("x",x+BW/2).attr("y",BT+cum+h/2+13).attr("text-anchor","middle")
      .attr("font-size","10px").attr("fill","#fff")
      .text(d3.format(",")(d[k==="a"?"ba":"bp"])+" ballots");
    cum+=h;});
  if(cum<BH){svg.append("rect").attr("x",x).attr("y",BT+cum).attr("width",BW)
    .attr("height",BH-cum).attr("fill","#dcdcdc").attr("stroke","#fff");}
  svg.append("text").attr("x",x+BW/2).attr("y",BT+BH+16).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#444").text(lab);});
})();
</script>
', blk, otl, pts, bar, MH, round(MW/2), n(nrow(wp)), MH - 44, n(WBAL)))

## ---- rozr-static
layout(matrix(c(1, 2), 1), widths = c(2.05, 1))
par(mar = c(0.2, 0.2, 1.8, 0.2))
plot(NA, xlim = range(wb$x), ylim = range(wb$y) + c(-1.4, 0), asp = 1,
     axes = FALSE, ann = FALSE)
k <- interaction(wb$id, wb$part, drop = TRUE)
for (i in seq_along(levels(k))) {
  z <- wb[k == levels(k)[i], ]
  polygon(z$x, z$y, col = adjustcolor(wcol(z$tgt[1]), 0.16), border = "#fff", lwd = 0.3)
}
ko <- interaction(wo$tgt, wo$part, drop = TRUE)
for (i in seq_along(levels(ko))) {
  z <- wo[ko == levels(ko)[i], ]
  polygon(z$x, z$y, col = NA, border = wcol(z$tgt[1]), lwd = 1.8)
}
points(wp$x, wp$y, pch = 19, cex = 1.5 * sqrt(wp$pop) / sqrt(max(wp$pop)),
       col = adjustcolor(wcol(wp$tgt), 0.85))
title(sprintf("%s census blocks inside one 2020 precinct", n(nrow(wp))),
      cex.main = 0.9, line = 0.3)
scalebar(2, min(wb$x) + 0.1, min(wb$y) - 0.9, "2 km")
legend("bottomright", c("stays in ROZR", "goes to the new precinct PEC"),
       pch = 19, col = unname(WCOL), bty = "n", cex = 0.6, pt.cex = 0.9)

par(mar = c(2.2, 0.4, 1.8, 0.4))
plot(NA, xlim = c(0, 2.4), ylim = c(-0.06, 1.02), axes = FALSE, ann = FALSE)
for (j in 1:2) {
  v <- if (j == 1) w2$w_area else w2$w_pop
  bl <- if (j == 1) w2$ballots_area else w2$ballots_pop
  b <- 0.32 + (j - 1) * 1.16; wd <- 0.62; cum <- 0
  for (i in seq_along(v)) {
    rect(b, cum, b + wd, cum + v[i], col = adjustcolor(unname(WCOL)[i], 0.9),
         border = "white", lwd = 1.4)
    text(b + wd/2, cum + v[i]/2 + 0.022,
         paste0(w2$target[i], " ", pc(100*v[i]), "%"),
         cex = 0.48, col = "white", font = 2)
    text(b + wd/2, cum + v[i]/2 - 0.030, paste(n(bl[i]), "ballots"),
         cex = 0.5, col = "white")
    cum <- cum + v[i]
  }
  if (cum < 1) rect(b, cum, b + wd, 1, col = "#dcdcdc", border = "white", lwd = 1.4)
  text(b + wd/2, -0.05, c("by AREA", "by POPULATION")[j], cex = 0.62, font = 2, xpd = NA)
}
title(sprintf("how its %s ballots get split", n(WBAL)), cex.main = 0.9, line = 0.3)

## ---- compare
data.frame(
  quantity = c("Precinct pairs both methods produce",
               "Median absolute difference in weight",
               "Pairs differing by more than 10 points of weight",
               "Largest single disagreement"),
  Houston = c(n(nrow(cpH)), pc(median(abs(cpH$diff)), 3),
              paste0(n(sum(abs(cpH$diff) > 0.10)), "  (",
                     pc(100*mean(abs(cpH$diff) > 0.10)), "%)"),
              pc(max(abs(cpH$diff)), 3)),
  Georgia = c(n(nrow(cp)), pc(median(abs(cp$diff)), 3),
              paste0(n(sum(abs(cp$diff) > 0.10)), "  (",
                     pc(100*mean(abs(cp$diff) > 0.10)), "%)"),
              pc(max(abs(cp$diff)), 3)),
  check.names = FALSE)

## ---- d3
set.seed(84355)
s <- cp[sample(nrow(cp), min(1800, nrow(cp))), ]
s <- s[cty(s$from_2020) != HC, ]
rows <- paste(sprintf('{"a":%.4f,"p":%.4f,"d":%.4f}', s$weight_area, s$weight_pop, s$diff),
              collapse = ",")
hrow <- paste(sprintf('{"a":%.4f,"p":%.4f,"d":%.4f,"n":"%s"}',
                      cpH$weight_area, cpH$weight_pop, cpH$diff, nm(cpH$from_2020)),
              collapse = ",")
xw <- cp[which.max(abs(cp$diff)), ]
cat(sprintf('
<div id="wt" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s],H=[%s];
const W=740,HT=440,M={t:20,r:24,b:52,l:60};
const svg=d3.select("#wt").append("svg").attr("viewBox",`0 0 ${W} ${HT}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,1]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([HT-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${HT-M.b})`).call(d3.axisBottom(x).ticks(6));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",HT-14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("weight by AREA");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(HT-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("weight by POPULATION");
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(1)).attr("y2",y(1))
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(0.62)).attr("y",y(0.66)).attr("font-size","11px")
  .attr("fill","#777").attr("transform",`rotate(-38,${x(0.62)},${y(0.66)})`)
  .text("the two methods agree");
const tip=d3.select("#wt").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const show=(e,d,who)=>{tip.style("opacity",1).html(
  (who?`<b>${who}</b><br>`:"")+
  `area ${(100*d.a).toFixed(1)}%%<br>population ${(100*d.p).toFixed(1)}%%<br>`+
  `<b>gap ${(100*Math.abs(d.d)).toFixed(1)} points</b>`)
  .style("left",Math.min(e.offsetX+14,W-170)+"px").style("top",(e.offsetY-10)+"px");};
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.a)).attr("cy",d=>y(d.p)).attr("r",3)
  .attr("fill",d=>Math.abs(d.d)>0.10?"#C41230":"#2c7fb8")
  .attr("fill-opacity",d=>Math.abs(d.d)>0.10?0.6:0.22)
  .on("mousemove",(e,d)=>show(e,d,null)).on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("circle").data(H).join("circle")
  .attr("cx",d=>x(d.a)).attr("cy",d=>y(d.p)).attr("r",5)
  .attr("fill","#1a9641").attr("stroke","#0b5d24").attr("stroke-width",1.2)
  .on("mousemove",(e,d)=>show(e,d,"HOUSTON "+d.n))
  .on("mouseleave",()=>tip.style("opacity",0));
// the statewide extreme, called out
const XA=%.4f,XP=%.4f;
svg.append("text").attr("x",x(XA)+12).attr("y",y(XP)+4).attr("font-size","11px")
  .attr("fill","#C41230").attr("font-weight","600")
  .text("%s \\u2014 the worst in Georgia");
const lg=svg.append("g").attr("transform",`translate(${M.l+12},${M.t+4})`);
[["#1a9641","HOUSTON pairs"],["#C41230","differ by more than 10 points"],
 ["#2c7fb8","agree within 10 points"]]
 .forEach((r,i)=>{lg.append("circle").attr("cy",i*17).attr("r",4).attr("fill",r[0]);
  lg.append("text").attr("x",10).attr("y",i*17+4).attr("font-size","11.5px").text(r[1]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
A sample of %s precinct pairs from the rest of Georgia, with all %s of Houston\'s
in green. Points on the dashed line are pairs where land and people give the same
answer; points far off it are where they do not.</p>
', rows, hrow, xw$weight_area, xw$weight_pop, nm(xw$from_2020),
   n(nrow(s)), n(nrow(cpH))))

## ---- scatter-static
par(mar = c(4.1, 4.1, 0.6, 0.6))
plot(cp$weight_area, cp$weight_pop, pch = 19, cex = 0.4,
     col = ifelse(abs(cp$diff) > 0.10, adjustcolor("#C41230", 0.6),
                  adjustcolor("#2c7fb8", 0.3)),
     xlab = "weight by area", ylab = "weight by population", xlim = c(0,1), ylim = c(0,1))
abline(0, 1, lty = 2, col = "grey50")
points(cpH$weight_area, cpH$weight_pop, pch = 21, cex = 0.95,
       bg = "#1a9641", col = "#0b5d24", lwd = 1.1)
xw <- cp[which.max(abs(cp$diff)), ]
text(xw$weight_area + 0.02, xw$weight_pop - 0.03,
     paste(nm(xw$from_2020), "- the worst in Georgia"), pos = 4, cex = 0.6,
     col = "#C41230", font = 2)
legend("bottomright", c("HOUSTON pairs", "differ by >10 points",
                        "agree within 10 points"),
       col = c("#0b5d24", "#C41230", "#2c7fb8"), pt.bg = c("#1a9641", NA, NA),
       pch = c(21, 19, 19), bty = "n", cex = 0.7)

## ---- carry
est <- read.csv(file.path(D, "derived/precincts_2024_pop.csv"), stringsAsFactors = FALSE)
eH  <- est[cty(est$to_2024) == HC, ]
eH  <- eH[order(-eH$VOTED20), ]
data.frame(`2024 precinct` = nm(eH$to_2024),
           TRUMP20 = n(eH$TRUMP20), BIDEN20 = n(eH$BIDEN20),
           REG20 = n(eH$REG20), VOTED20 = n(eH$VOTED20),
           check.names = FALSE)

## ---- check
ck

## ---- twosource
data.frame(
  scope = ifelse(ts$scope == HC, "Houston County", ts$scope),
  `precincts the name join reached` = n(ts$matched),
  `of those, vote totals identical` = n(ts$identical),
  `of those, differing` = n(ts$differing),
  check.names = FALSE)

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#hou text[fill="#444" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The white labels: under 3:1 in BOTH themes, and their stroke must be dark
# against a white glyph in both, which no single token is -- var(--ink) on
# the light page, var(--paper) on the dark one. A --paper stroke on the
# light page would make white text worse, not better.
cat('<style>
#rcp text[fill="#2c7fb8" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
#wnd text[fill="#fff" i],
#wnd text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#wnd text[fill="#fff" i],
#wnd text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
