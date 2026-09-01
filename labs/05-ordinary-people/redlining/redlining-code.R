# redlining-code.R -- chunk bodies for redlining-brief.Rmd
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
# GEOID must be read as text. A tract identifier is 11 digits and 1,549 of the
# 4,489 here begin with a zero (California is state 06, Alabama 01). Read as a
# number, those lose the leading digit and print as 10-digit identifiers that no
# longer name a state -- the exact failure that would show up in the three-row
# table of individual tracts further down.
tr <- read.csv("data/derived/tracts.csv", stringsAsFactors = FALSE,
               colClasses = c(GEOID = "character"))
ct <- read.csv("data/derived/cities.csv", stringsAsFactors = FALSE)
gr <- aggregate(cbind(total, black) ~ grade, tr, sum)
gr$pct <- 100 * gr$black / gr$total
gr$n   <- as.vector(table(tr$grade))
gv <- function(g, v) gr[[v]][gr$grade == g]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
cv <- function(city, v) ct[[v]][ct$city == city]

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

# ---- the 1937 maps, and the modern tracts drawn over them -------------------
# All six files are written by data/build-brief-figures.R from Mapping
# Inequality's digitized HOLC polygons (fetched 2026-08-10) and TIGER/Line 2020
# tracts. Coordinates are local kilometers from one shared origin per city, so
# the 1939 layer and the 2020 layer can be drawn over each other. GEOID is text
# here for the same reason it is text above.
HR <- read.csv("data/derived/fig_holc_rings.csv",  stringsAsFactors = FALSE)
HA <- read.csv("data/derived/fig_holc_attr.csv",   stringsAsFactors = FALSE)
HM <- read.csv("data/derived/fig_holc_meta.csv",   stringsAsFactors = FALSE)
ZH <- read.csv("data/derived/fig_zoom_holc.csv",   stringsAsFactors = FALSE)
ZT <- read.csv("data/derived/fig_zoom_tracts.csv", stringsAsFactors = FALSE)
ZW <- read.csv("data/derived/fig_zoom_win.csv",    stringsAsFactors = FALSE)
ZA <- read.csv("data/derived/fig_zoom_attr.csv",   stringsAsFactors = FALSE,
               colClasses = c(GEOID = "character"))
MX <- read.csv("data/derived/fig_mix.csv",         stringsAsFactors = FALSE,
               colClasses = c(GEOID = "character"))
QT <- read.csv("data/derived/fig_quote.csv",       stringsAsFactors = FALSE)
SR <- read.csv("data/derived/fig_source.csv",      stringsAsFactors = FALSE)

# The four colors are the HOLC's own, read out of the source file's `fill`
# field rather than invented here: A green, B blue, C yellow, D red.
GC <- setNames(HA$fill[match(c("A", "B", "C", "D"), HA$grade)], c("A", "B", "C", "D"))
GL <- c(A = "Best", B = "Still Desirable",
        C = "Definitely Declining", D = "Hazardous")

cl  <- function(d) d[d$city == "Cleveland", ]
hm  <- cl(HM)
asg <- MX[MX$centre_grade != "", ]                   # tracts the rule graded
mis <- asg[asg$centre_grade != asg$modal_grade, ]    # ...and graded as a minority
drp <- MX[MX$centre_grade == "" & MX$graded > 0, ]   # ...and threw away
fo  <- ZA[ZA$focus == 1, ]                           # the tract the close-up outlines
# Re-running the rule on the polygons is also a check on the file this chapter
# draws from: it should return the grade tracts.csv already carries.
agree <- merge(MX[MX$in_csv == 1, c("GEOID", "centre_grade")],
               tr[, c("GEOID", "grade")], by = "GEOID")

rx <- range(HR$x); ry <- range(HR$y)                 # the drawn city, in km
mixstr <- function(r) {                              # "63% C, 35% D, 1% A"
  v <- c(A = r$pA, B = r$pB, C = r$pC, D = r$pD, ungraded = r$pU)
  v <- sort(v[v >= 0.5], decreasing = TRUE)
  paste(sprintf("%.0f%% %s", v, names(v)), collapse = ", ")
}

## ---- grades

data.frame(
  grade = c("A", "B", "C", "D"),
  label = c("Best", "Still Desirable", "Definitely Declining", "Hazardous"),
  color = c("green", "blue", "yellow", "red"),
  check.names = FALSE)

## ---- holc-map-d3

# ---------------------------------------------------------------------------
# THE OBJECT THE CHAPTER IS ABOUT. Real HOLC areas, real 1939 boundaries, the
# source's own fills. Pixel coordinates are computed HERE, in R, and handed to
# D3 as finished path strings, so this figure and the base-R one below draw the
# same shapes from the same numbers; the only difference is that this one can be
# hovered. The outlines are not simplified -- the 1930s sheets were digitized
# into a few thousand vertices to begin with.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
WD <- 760; LG <- 186; PD <- 8
SC <- (WD - LG - 2 * PD) / diff(rx)
HT <- round(diff(ry) * SC + 2 * PD)
px <- function(x) PD + (x - rx[1]) * SC
py <- function(y) HT - PD - (y - ry[1]) * SC
d_of <- function(d) paste(vapply(split(d, d$part), function(z)
  paste0("M", paste(sprintf("%.1f,%.1f", px(z$x), py(z$y)), collapse = "L"), "Z"),
  character(1)), collapse = "")
areas <- paste(vapply(HA$id, function(i) paste0(
  '{"d":"', d_of(HR[HR$id == i, ]),
  '","g":"', HA$grade[HA$id == i],
  '","l":"', HA$label[HA$id == i],
  '","a":"', pc(HA$area_km2[HA$id == i], 2), '"}'), character(1)), collapse = ",")
key <- paste(vapply(c("A", "B", "C", "D"), function(g) paste0(
  '{"g":"', g, '","c":"', GC[[g]], '","n":"', GL[[g]],
  '","k":"', hm$n[hm$grade == g],
  '","p":"', pc(hm$pct_area[hm$grade == g], 0), '"}'), character(1)), collapse = ",")
cat(paste0('
<div id="holcmap" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const A=[', areas, '],K=[', key, '];
const GC={A:"', GC[["A"]], '",B:"', GC[["B"]], '",C:"', GC[["C"]], '",D:"', GC[["D"]], '"};
const W=', WD, ',H=', HT, ',LX=', WD - LG + 6, ';
const svg=d3.select("#holcmap").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=d3.select("#holcmap").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.4em;margin:0.3em 0 0 0");
const DEF="Home Owners’ Loan Corporation, Cleveland, 1939. "+
  "<i>Hover an area for the grade and the label the appraiser gave it.</i>";
cap.html(DEF);
const pa=svg.selectAll("path.a").data(A).join("path").attr("class","a")
  .attr("d",d=>d.d).attr("fill",d=>GC[d.g])
  .attr("stroke","#fff").attr("stroke-width",0.6).style("cursor","pointer")
  .on("mouseenter",function(e,d){
    pa.attr("stroke","#fff").attr("stroke-width",0.6);
    d3.select(this).attr("stroke","#111").attr("stroke-width",2).raise();
    const nm=({A:"Best",B:"Still Desirable",C:"Definitely Declining",D:"Hazardous"})[d.g];
    cap.html("Area <b>"+d.l+"</b> — grade <b>"+d.g+"</b>, “"+nm+
             "”, "+d.a+" km².");
  }).on("mouseleave",()=>{pa.attr("stroke","#fff").attr("stroke-width",0.6);cap.html(DEF);});
svg.append("rect").attr("x",', sprintf("%.1f", px(ZW$x0)), ')
  .attr("y",', sprintf("%.1f", py(ZW$y1)), ')
  .attr("width",', sprintf("%.1f", (ZW$x1 - ZW$x0) * SC), ')
  .attr("height",', sprintf("%.1f", (ZW$y1 - ZW$y0) * SC), ')
  .attr("fill","none").attr("stroke","#111").attr("stroke-width",1.6)
  .attr("stroke-dasharray","5,3").attr("pointer-events","none");
svg.append("text").attr("x",', sprintf("%.1f", px(ZW$x0) - 4), ')
  .attr("y",', sprintf("%.1f", py(ZW$y1) - 6), ')
  .attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#111").text("enlarged in Figure 2");
const lg=svg.append("g").attr("transform","translate("+LX+",26)");
lg.append("text").attr("y",-9).attr("font-size","11px").attr("font-weight","600")
  .text("HOLC grade, 1939");
K.forEach((r,i)=>{
  lg.append("rect").attr("y",i*30).attr("width",14).attr("height",14)
    .attr("fill",r.c).attr("stroke","#999").attr("stroke-width",0.5);
  lg.append("text").attr("x",21).attr("y",i*30+11).attr("font-size","11.5px")
    .attr("font-weight","600").text(r.g+"  "+r.n);
  lg.append("text").attr("x",21).attr("y",i*30+23).attr("font-size","10.5px")
    .attr("fill","#666").text(r.k+" areas, "+r.p+"% of graded land");
});
})();
</script>'))

## ---- holc-map-static

# The same polygons, the same colors, the same window marked: base R for the
# PDF device, D3 above for the browser. Neither is a redrawing of the other --
# both read fig_holc_rings.csv.
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(NA, xlim = rx, ylim = ry, asp = 1, axes = FALSE, ann = FALSE)
for (i in HA$id) {
  d <- HR[HR$id == i, ]
  for (q in unique(d$part)) {
    z <- d[d$part == q, ]
    polygon(z$x, z$y, col = GC[[HA$grade[HA$id == i]]], border = "#ffffff", lwd = 0.4)
  }
}
rect(ZW$x0, ZW$y0, ZW$x1, ZW$y1, border = "#111", lwd = 1.6, lty = 2)
text(ZW$x0 - 0.2, ZW$y1 + 1.0, "enlarged in Figure 2", adj = 1, cex = 0.62)
legend(rx[1], ry[2], sprintf("%s  %s (%d areas, %s%% of graded land)",
       names(GL), GL, hm$n[match(names(GL), hm$grade)],
       pc(hm$pct_area[match(names(GL), hm$grade)], 0)),
       fill = GC[names(GL)], border = "#999", bty = "n", cex = 0.62,
       title = "HOLC grade, 1939", title.adj = 0)

## ---- join

data.frame(
  step = c("HOLC areas digitized", "of them graded A to D", "Rule applied",
           "Tracts matched to a grade", "Cities represented", "States"),
  value = c(n(SR$features), n(SR$graded), "tract center point inside a graded polygon",
            n(nrow(tr)), length(unique(tr$city)), length(unique(tr$state))))

## ---- zoom-d3

# ---------------------------------------------------------------------------
# TWO LAYERS, EIGHTY-ONE YEARS APART, AT FULL RESOLUTION. Neither layer is
# simplified. This figure exists to show where a 1939 boundary and a 2020
# boundary fail to meet, so thinning either one would be inventing part of the
# answer. Everything quoted in the hover text was computed in the build script
# from the same unsimplified geometry, formatted once in R, and passed through
# as a string -- R and JS round differently and the reader should not be able to
# tell which one drew the figure.
# ---------------------------------------------------------------------------
WZ <- 700; PZ <- 6
SZ <- (WZ - 2 * PZ) / (ZW$x1 - ZW$x0)
HZt <- round((ZW$y1 - ZW$y0) * SZ + 2 * PZ)
qx <- function(x) PZ + (x - ZW$x0) * SZ
qy <- function(y) HZt - PZ - (y - ZW$y0) * SZ
ring <- function(d, fx, fy) paste(vapply(split(d, d$part), function(z)
  paste0("M", paste(sprintf("%.1f,%.1f", fx(z$x), fy(z$y)), collapse = "L"), "Z"),
  character(1)), collapse = "")
hz <- paste(vapply(split(ZH, ZH$id), function(d) paste0(
  '{"d":"', ring(d, qx, qy), '","g":"', d$grade[1], '"}'), character(1)),
  collapse = ",")
tz <- paste(vapply(ZA$id, function(i) {
  a <- ZA[ZA$id == i, ]
  paste0('{"d":"', ring(ZT[ZT$id == i, ], qx, qy),
         '","c":"', a$centre_grade, '","m":"', a$modal_grade,
         '","x":', sprintf("%.1f", qx(a$cx)), ',"y":', sprintf("%.1f", qy(a$cy)),
         ',"in":', ifelse(a$cx >= ZW$x0 & a$cx <= ZW$x1 &
                          a$cy >= ZW$y0 & a$cy <= ZW$y1, 1, 0),
         ',"f":', a$focus, ',"mix":"', mixstr(a), '"}')
}, character(1)), collapse = ",")
cat(paste0('
<div id="zoom" style="margin:1em 0"></div>
<script>
(function(){
const H=[', hz, '],T=[', tz, '];
const GC={A:"', GC[["A"]], '",B:"', GC[["B"]], '",C:"', GC[["C"]], '",D:"', GC[["D"]], '"};
const NM={A:"Best",B:"Still Desirable",C:"Definitely Declining",D:"Hazardous"};
const W=', WZ, ',Ht=', HZt, ';
const svg=d3.select("#zoom").append("svg").attr("viewBox","0 0 "+W+" "+Ht)
  .attr("style","max-width:100%;height:auto;font:12px inherit;border:1px solid #ddd");
svg.selectAll("path.h").data(H).join("path").attr("class","h")
  .attr("d",d=>d.d).attr("fill",d=>GC[d.g]).attr("stroke","none");
const cap=d3.select("#zoom").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:3.2em;margin:0.3em 0 0 0");
const DEF="Colored surfaces: HOLC areas, Cleveland 1939. Outlines: census "+
  "tracts, 2020. Dots: the center point the rule tests, colored by the grade "+
  "it returns; gray where it returns nothing. <i>Hover a tract.</i>";
cap.html(DEF);
const tp=svg.selectAll("path.t").data(T).join("path").attr("class","t")
  .attr("d",d=>d.d).attr("fill","transparent")
  .attr("stroke",d=>d.f?"#111":"#333").attr("stroke-width",d=>d.f?3:0.9)
  .style("cursor","pointer")
  .on("mouseenter",function(e,d){
    tp.attr("stroke-width",x=>x.f?3:0.9);
    d3.select(this).attr("stroke-width",3.4).attr("stroke","#111").raise();
    cap.html(d.c===""
      ? "<b>This tract’s center point falls outside every graded area</b>, so the "+
        "tract is dropped from the analysis entirely. Its land is "+d.mix+"."
      : "Center point lands in grade <b>"+d.c+"</b> (“"+NM[d.c]+"”), so the "+
        "whole tract is coded <b>"+d.c+"</b>. Its land is "+d.mix+".");
  })
  .on("mouseleave",function(){
    tp.attr("stroke-width",x=>x.f?3:0.9).attr("stroke",x=>x.f?"#111":"#333");
    cap.html(DEF);});
svg.selectAll("circle.d").data(T.filter(d=>d["in"])).join("circle").attr("class","d")
  .attr("cx",d=>d.x).attr("cy",d=>d.y).attr("r",4.6).attr("pointer-events","none")
  .attr("fill",d=>d.c===""?"#bbbbbb":GC[d.c]).attr("stroke","#111").attr("stroke-width",1);
const lg=svg.append("g").attr("transform","translate(10,10)");
lg.append("rect").attr("width",214).attr("height",62).attr("fill","#fff")
  .attr("fill-opacity",0.95).attr("stroke","#ccc");
lg.append("rect").attr("x",12).attr("y",11).attr("width",16).attr("height",10)
  .attr("fill","none").attr("stroke","#333");
// on-mark: the three legend labels sit on the white panel drawn above.
lg.append("text").attr("x",36).attr("y",20).attr("class","on-mark").attr("font-size","11.5px")
  .text("2020 census tract");
lg.append("circle").attr("cx",20).attr("cy",33).attr("r",4.6).attr("fill",GC.D)
  .attr("stroke","#111");
lg.append("text").attr("x",36).attr("y",37).attr("class","on-mark").attr("font-size","11.5px")
  .text("its center point");
lg.append("circle").attr("cx",20).attr("cy",49).attr("r",4.6).attr("fill","#bbbbbb")
  .attr("stroke","#111");
lg.append("text").attr("x",36).attr("y",53).attr("class","on-mark").attr("font-size","11.5px")
  .text("center outside every area");
})();
</script>'))

## ---- zoom-static

# The same two layers, the same window, the same numbers. ASCII only in the
# annotation: the PDF device drops non-Latin-1 glyphs from plot text.
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(NA, xlim = c(ZW$x0, ZW$x1), ylim = c(ZW$y0, ZW$y1), asp = 1,
     axes = FALSE, ann = FALSE)
for (i in unique(ZH$id)) {
  d <- ZH[ZH$id == i, ]
  for (q in unique(d$part)) {
    z <- d[d$part == q, ]
    polygon(z$x, z$y, col = GC[[d$grade[1]]], border = NA)
  }
}
for (i in ZA$id) {
  d <- ZT[ZT$id == i, ]
  for (q in unique(d$part)) {
    z <- d[d$part == q, ]
    polygon(z$x, z$y, col = NA, border = "#333333", lwd = 0.8)
  }
}
inw <- ZA$cx >= ZW$x0 & ZA$cx <= ZW$x1 & ZA$cy >= ZW$y0 & ZA$cy <= ZW$y1
points(ZA$cx[inw], ZA$cy[inw], pch = 21, cex = 0.95, lwd = 0.9, col = "#111111",
       bg = ifelse(ZA$centre_grade[inw] == "", "#bbbbbb", GC[ZA$centre_grade[inw]]))
for (q in unique(ZT$part[ZT$id == fo$id])) {
  z <- ZT[ZT$id == fo$id & ZT$part == q, ]
  polygon(z$x, z$y, col = NA, border = "#111111", lwd = 2.6)
}
t1 <- sprintf("center point in %s: the whole tract is coded %s.",
              fo$centre_grade, fo$centre_grade)
t2 <- sprintf("its land is %s.", mixstr(fo))
CX <- 0.5
bw <- max(strwidth(t1, cex = CX, font = 2), strwidth(t2, cex = CX)) + 0.16
lh <- 1.7 * strheight("Ag", cex = CX)
bh <- 2.4 * lh
tx <- fo$cx - bw - 0.5; ty <- fo$cy - 0.55
rect(tx, ty - bh, tx + bw, ty, col = "#ffffff", border = "#111111", lwd = 0.7)
text(tx + 0.08, ty - 0.75 * lh, t1, cex = CX, adj = 0, font = 2)
text(tx + 0.08, ty - 1.75 * lh, t2, cex = CX, adj = 0)
arrows(tx + bw, ty - bh / 2, fo$cx - 0.07, fo$cy - 0.06, length = 0.05, lwd = 1.1,
       col = "#111111")
legend(ZW$x0 + 0.08, ZW$y1 - 0.08,
       c("2020 census tract", "its center point", "center outside every area"),
       lty = c(1, NA, NA), pch = c(NA, 21, 21), pt.bg = c(NA, GC[["D"]], "#bbbbbb"),
       col = c("#333333", "#111111", "#111111"), bty = "o", bg = "#ffffff",
       box.col = "#cccccc", cex = 0.58)

## ---- one-record

o <- tr[1:3, c("GEOID", "state", "city", "grade", "total", "black", "pct_black")]
names(o) <- c("tract GEOID", "state", "city", "1930s HOLC grade",
              "2020 population", "Black residents", "% Black")
o

## ---- national

o <- gr[, c("grade", "n", "total", "black", "pct")]
o$total <- n(o$total); o$black <- n(o$black); o$pct <- pc(o$pct)
names(o) <- c("HOLC grade", "tracts", "2020 population", "Black residents", "% Black")
o

## ---- within

o <- head(ct[order(-ct$gap), c("city", "a_tracts", "d_tracts",
                               "a_pct_black", "d_pct_black", "gap")], 8)
names(o) <- c("city", "A tracts", "D tracts", "A areas % Black",
              "D areas % Black", "gap")
o

## ---- slope-static

o <- ct[order(ct$gap), ]
plot(NA, xlim = c(0.8, 2.2), ylim = c(0, 75), xaxt = "n", las = 1,
     xlab = "", ylab = "% Black, 2020")
axis(1, at = c(1, 2), labels = c("HOLC grade A", "HOLC grade D"))
for (i in seq_len(nrow(o))) {
  cl <- if (o$gap[i] < 0) "#2c7fb8" else "#C41230"
  lines(c(1, 2), c(o$a_pct_black[i], o$d_pct_black[i]), col = cl, lwd = 1.8)
  points(c(1, 2), c(o$a_pct_black[i], o$d_pct_black[i]), pch = 19, col = cl, cex = 0.6)
}
legend("topleft", c("D more Black than A", "reversed"),
       col = c("#C41230", "#2c7fb8"), lwd = 2, bty = "n", cex = 0.8)

## ---- d3-slope

rows <- paste(sprintf('{"c":"%s","a":%.1f,"d":%.1f,"g":%.1f,"an":%d,"dn":%d}',
                      gsub('"', "", ct$city), ct$a_pct_black, ct$d_pct_black,
                      ct$gap, ct$a_tracts, ct$d_tracts), collapse = ",")
cat(sprintf('
<div id="red" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=470,M={t:26,r:150,b:40,l:56};
const svg=d3.select("#red").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(["A","D"]).range([M.l+70,W-M.r-70]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.a,d.d))*1.08]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% Black, 2020 Census");
["A","D"].forEach(g=>{
  svg.append("text").attr("x",x(g)).attr("y",H-16).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600")
    .text(g==="A"?"HOLC grade A (\\u201cBest\\u201d)":"HOLC grade D (\\u201cHazardous\\u201d)");
  svg.append("line").attr("x1",x(g)).attr("x2",x(g)).attr("y1",M.t).attr("y2",H-M.b)
    .attr("stroke","#ddd");
});
const col=d=>d.g<0?"#2c7fb8":"#C41230";
const tip=d3.select("#red").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
g.selectAll("path").data(D).join("path")
  .attr("d",d=>`M${x("A")},${y(d.a)} L${x("D")},${y(d.d)}`)
  .attr("stroke",col).attr("stroke-width",1.9).attr("fill","none").attr("opacity",0.72)
  .on("mousemove",function(e,d){
    g.selectAll("path").attr("opacity",0.12);
    d3.select(this).attr("opacity",1).attr("stroke-width",3.4);
    tip.style("opacity",1).html(
      `<b>${d.c}</b><br>A areas: ${d.a}%% Black (${d.an} tracts)<br>`+
      `D areas: ${d.d}%% Black (${d.dn} tracts)<br>gap: ${d.g>0?"+":""}${d.g} pts`)
      .style("left",Math.min(e.offsetX+14,W-240)+"px").style("top",(e.offsetY-10)+"px");
  })
  .on("mouseleave",function(){
    g.selectAll("path").attr("opacity",0.72).attr("stroke-width",1.9);
    tip.style("opacity",0);
  });
D.filter(d=>d.g<0||d.g>32).forEach(d=>{
  svg.append("text").attr("x",x("D")+8).attr("y",y(d.d)+4).attr("font-size","11px")
    .attr("fill",col(d)).text(d.c);
});
const lg=svg.append("g").attr("transform",`translate(${M.l+8},${M.t-10})`);
[["#C41230","D areas more Black than A"],["#2c7fb8","reversed"]].forEach((r,i)=>{
  lg.append("line").attr("x1",0).attr("x2",16).attr("y1",i*16).attr("y2",i*16)
    .attr("stroke",r[0]).attr("stroke-width",3);
  lg.append("text").attr("x",21).attr("y",i*16+4).attr("font-size","11.5px").text(r[1]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One line per city. Hover to isolate. Labeled cities are the reversals and the
largest gaps.</p>
', rows))

## ---- reversals

o <- ct[ct$gap < 0, c("city", "a_tracts", "d_tracts", "a_pop", "d_pop",
                      "a_pct_black", "d_pct_black", "gap")]
o <- o[order(o$gap), ]
o$a_pop <- n(o$a_pop); o$d_pop <- n(o$d_pop)
names(o) <- c("city", "A tracts", "D tracts", "A population", "D population",
              "A % Black", "D % Black", "gap")
o

## ---- on-mark

# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#holcmap text[fill="#111" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt

cat(ai_prompt(readLines("data/ai-prompt.txt")))
