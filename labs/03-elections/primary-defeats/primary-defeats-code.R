# primary-defeats-code.R -- chunk bodies for primary-defeats-brief.Rmd
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

# All of this is read from data/, which is written by the shared build in
# ../retirements/data/build-data.R. That script parses ten Federal Election
# Commission workbooks, joins them to Voteview's membership file, and writes
# the same derivation into both chapters. Nothing here re-derives anything.
IN <- read.csv("data/derived/incumbents.csv", stringsAsFactors = FALSE)
BY <- read.csv("data/derived/by_year.csv",    stringsAsFactors = FALSE)
DN <- read.csv("data/derived/denied.csv",     stringsAsFactors = FALSE)
VS <- read.csv("data/derived/vsoc.csv",       stringsAsFactors = FALSE)
GG <- read.csv("data/derived/giroux.csv",     stringsAsFactors = FALSE)
CM <- read.csv("data/derived/compare.csv",    stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",     stringsAsFactors = FALSE)
# The exit table from the sibling chapter, for the top band of the funnel and
# for the one sentence that quotes its headline share. Read once here rather
# than re-read inline four times.
RE <- read.csv("../retirements/data/derived/exits_by_year.csv", stringsAsFactors = FALSE)
NHELD <- sum(RE$members)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)
ck <- function(k) {
  v <- CK$value[CK$check == k]
  if (length(v) != 1L) stop("checks.csv has no single value for '", k, "'")
  v
}
by <- function(yr, col) BY[[col]][BY$year == yr]

Y1 <- min(BY$year); Y2 <- max(BY$year)
NINC  <- sum(BY$incumbents)      # incumbent candidacies in the FEC files
NRAN  <- sum(BY$ran_primary)     # of those, stood on a primary ballot
NCON  <- sum(BY$contested)       # of those, had at least one rival
NDEN  <- sum(BY$denied)          # of those, were not renominated
RATE  <- 100 * NDEN / NRAN

# The Brookings series is the long view: forty elections rather than ten.
VS$redist <- VS$year %% 10 == 2
RD <- aggregate(lost_primary ~ redist, VS, mean)
VSR <- 100 * sum(VS$lost_primary) / sum(VS$seeking)

# The Giroux tracker counts the House and the Senate separately; only the
# House column is comparable with anything here.
GH <- GG[GG$year >= Y1 & GG$year <= Y2, ]

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- scope
data.frame(
  quantity = c("Elections covered", "Years",
               "Incumbent candidacies in the FEC files",
               "Of those, on a primary ballot",
               "Of those, with at least one rival",
               "Of those, denied renomination"),
  value = c(nrow(BY), paste(Y1, "to", Y2), n(NINC), n(NRAN), n(NCON), NDEN))

## ---- rawfec
# A verbatim capture from ../retirements/data/raw/federalelections2014.xls,
# the copy this corpus's build script downloaded and kept. Printed transposed --
# one field per line -- because the sheet is 23 columns wide.
# The sheet's own field names down the page, the two candidates beside them.
# NA is what the workbook holds -- an absent cell, not a zero -- and the
# chapter's inference is built on exactly those absences, so they are shown.
data.frame(
  Field_as_the_sheet_names_it = c(
    "STATE ABBREVIATION", "D", "FEC ID#", "(I)", "CANDIDATE NAME (Last)",
    "PARTY", "PRIMARY VOTES", "PRIMARY %", "GENERAL VOTES",
    "GE WINNER INDICATOR"),
  Column_1 = c("VA", "07 - FULL TERM", "H0VA07042", "(I)", "Cantor", "R",
               "28912", "0.44468369810972513", "NA", "NA"),
  Column_2 = c("VA", "07 - FULL TERM", "H4VA07143", "NA", "Brat", "R",
               "36105", "0.55531630189027481", "148026", "W"))

## ---- cleaninc
IN[IN$last == "Cantor", c("year", "stab", "dnum", "party", "unopposed",
                          "prim_votes", "rivals", "won_nom", "denied")]

## ---- rule
data.frame(
  step = c("Did the member stand in a primary?",
           "Did anyone stand against them?",
           "Did they win the nomination?"),
  `how it is decided` = c(
    "A primary vote total, or the word \"Unopposed\"",
    "Another candidate of the same party with a vote total",
    "The FEC's own winner flag from 2020; before that, the runoff winner, or the leader of the primary"),
  check.names = FALSE)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# THE CASCADE. Five nested totals, pooled across ten elections, drawn as a
# funnel because the argument is about survival at each stage rather than about
# a trend. Widths are proportional to the counts and are computed in R; D3 only
# draws the trapezoids.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
ST <- data.frame(
  lab = c("held a House seat", "sought re-election to the House",
          "stood on a primary ballot", "faced a primary rival",
          "denied renomination"),
  v = c(sum(BY$incumbents) + 0, NINC, NRAN, NCON, NDEN))
ST$v[1] <- NHELD
rows <- paste(sprintf('{"l":"%s","v":%d}', ST$lab, ST$v), collapse = ",")
cat(paste0('
<div id="fun" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', rows, '];
const W=760,H=360,M={t:16,r:250,b:16,l:16},CX=(W-M.r)/2;
const svg=d3.select("#fun").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
// Width on a square-root scale: a linear one makes the last band invisible and
// a log one makes it look bigger than it is.
const mx=S[0].v, wmax=(W-M.r)-2*M.l;
const wof=v=>Math.max(wmax*Math.sqrt(v/mx),2.5);
const bh=(H-M.t-M.b)/S.length;
const CO=["#dbe6ef","#c2d5e6","#a9c3dc","#8fb1d2","#C41230"];
S.forEach((s,i)=>{
  const w0=wof(s.v), w1=wof(i+1<S.length?S[i+1].v:s.v*0.45);
  const y0=M.t+i*bh, y1=y0+bh-6;
  svg.append("path").attr("d","M"+(CX-w0/2)+","+y0+"L"+(CX+w0/2)+","+y0+
      "L"+(CX+w1/2)+","+y1+"L"+(CX-w1/2)+","+y1+"Z")
    .attr("fill",CO[i]).attr("stroke","#fff").attr("stroke-width",1.2);
  svg.append("text").attr("x",CX).attr("y",y0+bh/2)
    .attr("text-anchor","middle").attr("font-size","13px").attr("font-weight","600")
    .attr("fill",i===4?"#fff":"#123").text(s.v.toLocaleString());
  svg.append("text").attr("x",W-M.r+18).attr("y",y0+bh/2-4)
    .attr("font-size","12.5px").attr("font-weight","600").text(s.l);
  if(i>0) svg.append("text").attr("x",W-M.r+18).attr("y",y0+bh/2+11)
    .attr("font-size","11px").attr("fill","#666")
    .text((100*s.v/S[i-1].v).toFixed(1)+"% of the row above");
});
})();
</script>'))

## ---- fig1-static
# The same five totals, the same square-root widths, base R for the PDF device.
ST <- data.frame(
  lab = c("held a House seat", "sought re-election to the House",
          "stood on a primary ballot", "faced a primary rival",
          "denied renomination"),
  v = c(NHELD, NINC, NRAN, NCON, NDEN))
CO <- c("#dbe6ef", "#c2d5e6", "#a9c3dc", "#8fb1d2", "#C41230")
wof <- function(v) pmax(sqrt(v / ST$v[1]), 0.006)
par(mar = c(0.4, 0.4, 0.4, 0.4))
plot(NA, xlim = c(-0.55, 1.55), ylim = c(0, 5), axes = FALSE, ann = FALSE)
for (i in 1:5) {
  w0 <- wof(ST$v[i]) / 2
  w1 <- wof(if (i < 5) ST$v[i + 1] else ST$v[i] * 0.45) / 2
  yt <- 5 - (i - 1) * 1; yb <- yt - 0.86
  polygon(c(-w0, w0, w1, -w1), c(yt, yt, yb, yb), col = CO[i], border = "#ffffff")
  text(0, (yt + yb) / 2, n(ST$v[i]), cex = 0.72, font = 2,
       col = if (i == 5) "#ffffff" else "#112233")
  text(0.60, (yt + yb) / 2 + 0.10, ST$lab[i], adj = 0, cex = 0.66, font = 2)
  if (i > 1)
    text(0.60, (yt + yb) / 2 - 0.14,
         sprintf("%s%% of the row above", pc(100 * ST$v[i] / ST$v[i - 1])),
         adj = 0, cex = 0.58, col = "#666666")
}

## ---- fig2-d3
# ---------------------------------------------------------------------------
# ONE TICK PER INCUMBENT. Ten rows, one per cycle; each tick is a sitting
# member placed at the share of the primary vote they took, drawn only for the
# members who actually had a rival. Members refused renomination are drawn on
# top in red. The form is a barcode rather than a histogram because the point
# is the individual members in the left tail, not the density.
# ---------------------------------------------------------------------------
C <- IN[IN$ran_prim & IN$rivals > 0 & !is.na(IN$prim_share), ]
rows <- paste(sprintf('{"y":%d,"s":%.2f,"d":%d,"n":"%s"}', C$year, C$prim_share,
                      as.integer(C$denied),
                      gsub('"', "", paste0(C$last, " (", C$stab, "-", C$dnum, ")"))),
              collapse = ",")
meds <- paste(sprintf('{"y":%d,"m":%.2f,"n":%d,"t":%d}', BY$year, BY$median_share,
                      BY$contested, BY$ran_primary), collapse = ",")
cat(paste0('
<div id="bar" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '],M=[', meds, '];
const W=760,H=400,MG={t:24,r:14,b:38,l:64};
const svg=d3.select("#bar").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([MG.l,W-MG.r]);
const yb=d3.scaleBand().domain(M.map(d=>d.y)).range([MG.t,H-MG.b]).padding(0.3);
svg.append("g").attr("transform","translate(0,"+(H-MG.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+MG.l+",0)").call(d3.axisLeft(yb));
svg.append("text").attr("x",(W+MG.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("the incumbent\\u2019s share of the primary vote");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",MG.t)
  .attr("y2",H-MG.b).attr("stroke","#999").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(50)+4).attr("y",MG.t+9).attr("font-size","10.5px")
  .attr("fill","#666").text("half the primary vote");
const tip=d3.select("#bar").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("line.t").data(D.filter(d=>!d.d)).join("line").attr("class","t")
  .attr("x1",d=>x(d.s)).attr("x2",d=>x(d.s)).attr("y1",d=>yb(d.y))
  .attr("y2",d=>yb(d.y)+yb.bandwidth()).attr("stroke","#4a6b8a")
  .attr("stroke-width",1).attr("stroke-opacity",0.5);
svg.selectAll("line.d").data(D.filter(d=>d.d)).join("line").attr("class","d")
  .attr("x1",d=>x(d.s)).attr("x2",d=>x(d.s)).attr("y1",d=>yb(d.y)-3)
  .attr("y2",d=>yb(d.y)+yb.bandwidth()+3).attr("stroke","#C41230")
  .attr("stroke-width",2.2).style("cursor","pointer")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.n+"</b>, "+d.y+"<br>"+d.s.toFixed(1)+
      "% of the primary vote, and not renominated")
      .style("left",Math.min(e.offsetX+14,W-280)+"px").style("top",(e.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.selectAll("circle.m").data(M).join("circle").attr("class","m")
  .attr("cx",d=>x(d.m)).attr("cy",d=>yb(d.y)+yb.bandwidth()/2).attr("r",3.6)
  .attr("fill","#111");
M.forEach(d=>{
  svg.append("text").attr("x",W-MG.r).attr("y",yb(d.y)+yb.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","10.5px").attr("fill","#666")
    .text(d.n+" of "+d.t+" contested");
});
const lg=d3.select("#bar").append("div").attr("style",
  "margin-top:4px;font-size:12px;color:#444");
lg.html("<span style=\\"color:#4a6b8a\\">\\u2758</span> one incumbent with a "+
  "primary rival &nbsp; <span style=\\"color:#C41230\\">\\u2758</span> refused "+
  "renomination &nbsp; \\u25cf median for the cycle");
})();
</script>'))

## ---- fig2-static
C <- IN[IN$ran_prim & IN$rivals > 0 & !is.na(IN$prim_share), ]
par(mar = c(3.2, 3.6, 0.6, 5.2), mgp = c(2.1, 0.6, 0))
yrs <- sort(unique(BY$year))
# The extra room at the bottom is for the legend, which otherwise lands on the
# 2022 row, where the ticks are densest.
plot(NA, xlim = c(0, 100), ylim = c(-0.5, length(yrs) + 0.6), yaxt = "n",
     xlab = "the incumbent's share of the primary vote", ylab = "", las = 1)
axis(2, at = seq_along(yrs), labels = rev(yrs), las = 1, cex.axis = 0.72)
abline(v = 50, col = "#999999", lty = 2)
for (i in seq_along(yrs)) {
  yy <- length(yrs) - i + 1
  z <- C[C$year == yrs[i], ]
  segments(z$prim_share[!z$denied], yy - 0.32, z$prim_share[!z$denied], yy + 0.32,
           col = "#4a6b8a80", lwd = 0.8)
  segments(z$prim_share[z$denied], yy - 0.40, z$prim_share[z$denied], yy + 0.40,
           col = "#C41230", lwd = 1.8)
  points(BY$median_share[BY$year == yrs[i]], yy, pch = 19, cex = 0.6)
  text(103, yy, sprintf("%d of %d", BY$contested[BY$year == yrs[i]],
                        BY$ran_primary[BY$year == yrs[i]]),
       adj = 0, cex = 0.55, col = "#666666", xpd = NA)
}
legend(0, 0.15, c("one incumbent with a primary rival",
                  "refused renomination", "median for the cycle"),
       horiz = TRUE, lty = c(1, 1, NA), pch = c(NA, NA, 19),
       lwd = c(1, 1.8, NA), col = c("#4a6b8a", "#C41230", "#111111"),
       bty = "n", cex = 0.54, x.intersp = 0.6)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# THE LONG VIEW, from a source that is not the FEC. One column per election,
# 1946 to 2024, from the Brookings series; the first election held on a new
# districting map is drawn in the accent color. Columns rather than a line
# because these are counts of a rare event, and a line between two of them
# implies values that never existed.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"y":%d,"v":%d,"r":%d,"s":%d}', VS$year, VS$lost_primary,
                      as.integer(VS$redist), VS$seeking), collapse = ",")
cat(paste0('
<div id="lng" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const W=760,H=310,M={t:20,r:14,b:42,l:46};
const svg=d3.select("#lng").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(D.map(d=>d.y)).range([M.l,W-M.r]).padding(0.22);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.v)+2]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(D.filter(d=>d.y%10===0).map(d=>d.y)));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",13).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#444").text("House incumbents denied renomination");
const tip=d3.select("#lng").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.y)).attr("y",d=>y(d.v)).attr("width",x.bandwidth())
  .attr("height",d=>y(0)-y(d.v)).attr("fill",d=>d.r?"#C41230":"#9fb6c9")
  .style("cursor","pointer")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.y+"</b><br>"+d.v+" of "+d.s+
      " seeking re-election"+(d.r?"<br><i>first election on a new map</i>":""))
      .style("left",Math.min(e.offsetX+14,W-250)+"px").style("top",(e.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
const lg=svg.append("g").attr("transform","translate("+(W-M.r-230)+","+(M.t)+")");
[["#C41230","first election on a new districting map"],
 ["#9fb6c9","every other election"]].forEach((r,i)=>{
  lg.append("rect").attr("y",i*16).attr("width",11).attr("height",11).attr("fill",r[0]);
  lg.append("text").attr("x",16).attr("y",i*16+10).attr("font-size","11px").text(r[1]);
});
})();
</script>'))

## ---- fig3-static
par(mar = c(2.8, 3.6, 0.6, 0.6), mgp = c(2.3, 0.6, 0))
b <- barplot(VS$lost_primary, border = NA, las = 1,
             col = ifelse(VS$redist, "#C41230", "#9fb6c9"),
             ylab = "House incumbents denied renomination", ylim = c(0, 21))
axis(1, at = b[VS$year %% 10 == 0], labels = VS$year[VS$year %% 10 == 0],
     cex.axis = 0.72, tick = FALSE, line = -0.6)
legend("topright", c("first election on a new districting map",
                     "every other election"),
       fill = c("#C41230", "#9fb6c9"), border = NA, bty = "n", cex = 0.66)

## ---- fig4-d3
# ---------------------------------------------------------------------------
# THIS BUILD AGAINST TWO INDEPENDENT COUNTS. A scatter with a 45-degree line,
# so a point off the diagonal is a disagreement and the reader can see how far.
# Brookings and Giroux agree with each other exactly in every year here, so
# they are drawn as one point per election with both labels.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"y":%d,"a":%d,"b":%d,"g":%d}', CM$year, CM$fec_derived,
                      CM$brookings, CM$giroux), collapse = ",")
cat(paste0('
<div id="cmp" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const W=760,H=360,M={t:18,r:18,b:42,l:52};
const svg=d3.select("#cmp").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const mx=d3.max(D,d=>Math.max(d.a,d.b,d.g))+2;
const x=d3.scaleLinear().domain([0,mx]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,mx]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(8));
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(mx)).attr("y2",y(mx))
  .attr("stroke","#bbb").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("counted by Brookings and by the Giroux tracker (identical in every year)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#444").text("derived here from the FEC workbooks");
svg.selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.b)).attr("cy",d=>y(d.a)).attr("r",5.4)
  .attr("fill",d=>d.a===d.b?"#2f6f5e":"#C41230").attr("fill-opacity",0.85);
D.forEach(d=>{
  svg.append("text").attr("x",x(d.b)+9).attr("y",y(d.a)+4).attr("font-size","11px")
    .attr("fill",d.a===d.b?"#2f6f5e":"#C41230").text(d.y);
});
const lg=svg.append("g").attr("transform","translate("+(M.l+12)+","+(M.t+8)+")");
[["#2f6f5e","the three counts agree exactly"],
 ["#C41230","one short, for a named reason"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",5).attr("cy",i*17).attr("r",5.4).attr("fill",r[0]);
  lg.append("text").attr("x",16).attr("y",i*17+4).attr("font-size","11.5px").text(r[1]);
});
})();
</script>'))

## ---- fig4-static
par(mar = c(3.4, 3.8, 0.6, 0.6), mgp = c(2.3, 0.6, 0))
mx <- max(CM$fec_derived, CM$brookings) + 2
plot(CM$brookings, CM$fec_derived, xlim = c(0, mx), ylim = c(0, mx), las = 1,
     pch = 19, cex = 1.1,
     col = ifelse(CM$fec_derived == CM$brookings, "#2f6f5e", "#C41230"),
     xlab = "counted by Brookings and by the Giroux tracker",
     ylab = "derived here from the FEC workbooks")
abline(0, 1, col = "#bbbbbb", lty = 2)
# Several elections land on exactly the same pair of counts, so their labels
# would print on top of each other. Stack the duplicates instead.
o  <- order(CM$brookings, CM$fec_derived)
k  <- paste(CM$brookings, CM$fec_derived)[o]
dy <- ave(seq_along(k), k, FUN = seq_along) - 1
text(CM$brookings[o] + 0.25, CM$fec_derived[o] - dy * 0.62, CM$year[o],
     adj = 0, cex = 0.6,
     col = ifelse(CM$fec_derived[o] == CM$brookings[o], "#2f6f5e", "#C41230"))
legend("topleft", c("the three counts agree exactly", "one short, for a named reason"),
       pch = 19, col = c("#2f6f5e", "#C41230"), bty = "n", cex = 0.68)

## ---- resid
data.frame(
  election = c(2006, 2014, 2020),
  member = c("Joe Schwarz, Michigan 7th", "Vance McAllister, Louisiana 5th",
             "Denver Riggleman, Virginia 5th"),
  `why the FEC file cannot show it` = c(
    "The incumbent marker is blank for every candidate in the district",
    "Louisiana's blanket primary is recorded as a general election",
    "Nominated at a party convention, so there is no primary to lose"),
  check.names = FALSE)
