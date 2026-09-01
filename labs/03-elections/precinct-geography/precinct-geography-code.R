# precinct-geography-code.R -- chunk bodies for precinct-geography-brief.Rmd
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

# what a match-on-names repair would discard in Houston
dropped <- setdiff(h20, h24)

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
dimof <- function(x) {
  s <- sub("^\\[[0-9]+\\] *", "", x[2])          # drop R's own "[1] " index
  as.integer(regmatches(s, gregexpr("[0-9]+", s))[[1]])
}
D20 <- dimof(R20); D24 <- dimof(R24)

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

## ---- raw24
namestab(R24)

## ---- cleanblocks
z <- baH[nm(baH$precinct_2020) == "ROZR", ]
z <- z[order(z$GEOID20), ]
o <- rbind(head(z[nm(z$precinct_2024) == "ROZR", ], 2),
           head(z[nm(z$precinct_2024) == "PEC",  ], 1))
o$GEOID20 <- sprintf("%.0f", o$GEOID20)
o

## ---- cleanxwalk
poH[nm(poH$from_2020) == "ROZR", c("from_2020", "to_2024", "weight", "pop")]

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

## ---- d3
# Land against people, one dot per precinct pair, drawn with the shared
# library (_lib/dd-charts.js). A sample keeps the payload light; every one of
# Houston's pairs rides along, and the statewide extreme carries its label.
mkpts <- function(d, grp) data.frame(
  area = round(d$weight_area, 4), pop = round(d$weight_pop, 4),
  gap = round(100 * abs(d$diff), 1),
  precinct = nm(d$from_2020), county = cty(d$from_2020),
  grp = grp, stringsAsFactors = FALSE)
set.seed(84355)
sm <- cp[sample(nrow(cp), min(1400, nrow(cp))), ]
sm <- sm[cty(sm$from_2020) != HC, ]
xw <- cp[which.max(abs(cp$diff)), ]
sm <- sm[!(sm$from_2020 == xw$from_2020 & sm$to_2024 == xw$to_2024), ]
dsc <- rbind(mkpts(sm, ifelse(abs(sm$diff) > 0.10,
                              "differ by more than 10 points",
                              "agree within 10 points")),
             mkpts(cpH, "Houston pairs"),
             mkpts(xw, "differ by more than 10 points"))
dsc$lbl <- ""
dsc$lbl[nrow(dsc)] <- paste(nm(xw$from_2020), "\u2014 the worst in Georgia")
dsc$side <- ifelse(dsc$area > 0.55, "left", "")
dd_fig("wt", "scatter", dsc, d3 = FALSE,
  size = list(w = 740, h = 460, m = list(t = 20, r = 26, b = 54, l = 60)),
  x = list(field = "area", domain = c(0, 1), ticks = 6, fmt = "f1",
           label = "weight by area"),
  y = list(field = "pop", domain = c(0, 1), ticks = 6, fmt = "f1",
           label = "weight by population"),
  series = list(field = "grp",
                classes = list("Houston pairs" = "series-3",
                               "differ by more than 10 points" = "series-2",
                               "agree within 10 points" = "series-1")),
  r = 3.2, opacity = 0.45, legend = TRUE,
  annotations = list(dd_annot_rule(0, 0, 1, 1, class = "zero")),
  tip = dd_tip(c(county = "county", area = "weight by area",
                 pop = "weight by population", gap = "gap, points"),
               fmt = c(area = "f2", pop = "f2", gap = "f1"),
               title = "precinct"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Dots on the dashed diagonal are pairs where land and people give the same
answer. Hover a dot for the pair and the gap.</p>')

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
