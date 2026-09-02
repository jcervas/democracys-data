# policing-code.R -- chunk bodies for policing-brief.Rmd
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
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)
d <- read.csv("data/derived/by_race.csv",  stringsAsFactors = FALSE)
y <- read.csv("data/derived/by_year.csv",  stringsAsFactors = FALSE)
d$share       <- 100 * d$stops / sum(d$stops)
d$search_rate <- 100 * d$searched / d$stops
d$hit_rate    <- 100 * d$contraband_found / d$searched
y$search_rate <- 100 * y$searched / y$stops
y$hit_rate    <- 100 * y$contraband_found / y$searched
r  <- function(x, v) d[[v]][d$race == x]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# ---- the totals the prose quotes --------------------------------------------
# Computed once here so no sentence in the brief carries a typed number, and so
# the two renderers of every figure below read the same values.
ST <- sum(d$stops); SE <- sum(d$searched); FD <- sum(d$contraband_found)

# the horizontal line in the search-rate/hit-rate figure: one weighted mean,
# used by both renderers rather than recomputed in each
WM <- 100 * FD / SE

# ---- does the ordering hold in every year? ----------------------------------
# The by-year table exists to answer one question in the prose: is the hit-rate
# gap the work of one or two unusual years? Counted here rather than drawn,
# because the answer is a count and not a shape.
.w <- y[y$race == "white", c("year", "hit_rate")]
.b <- y[y$race == "black", c("year", "hit_rate")]
.m <- merge(.w, .b, by = "year", suffixes = c("_white", "_black"))
NYR <- nrow(.m)
WHI <- sum(.m$hit_rate_white > .m$hit_rate_black)

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

# ---- the same stops over three real denominators -----------------------------
# American Community Survey counts for San Francisco County, one row per group
# per denominator, each with the ACS table it came from. The fourth column is a
# different question and is built separately below, which is the point of the
# figure.
acs <- read.csv("data/derived/acs_denominators.csv", stringsAsFactors = FALSE,
                colClasses = c(state = "character", county = "character"))
DEN <- c("residents", "adults 18+", "drives to work")
# only the groups the ACS can support at every denominator; "other" has no
# clean ACS counterpart beyond residents, so it is left out rather than faked
BR  <- c("white", "black", "hispanic", "asian/pacific islander")
bench <- do.call(rbind, lapply(DEN, function(dn) {
  s <- acs[acs$denominator == dn, ]
  m <- merge(d[d$race %in% BR, c("race", "stops", "search_rate")], s, by = "race")
  m$rate  <- m$stops / m$count
  m$ratio <- m$rate / m$rate[m$race == "white"]
  m$den   <- dn
  m[, c("race", "den", "ratio")]
}))
# the fourth column is a different question: given a stop, how often a search
srr <- data.frame(race = BR, den = "per stop",
                  ratio = d$search_rate[match(BR, d$race)] /
                          d$search_rate[d$race == "white"])
bench <- rbind(bench, srr)
BCOL <- c(white = "#2c7fb8", black = "#C41230", hispanic = "#e08214",
          `asian/pacific islander` = "#4d9221")
BX   <- c(DEN, "per stop")
bfmt <- function(x) formatC(x, format = "f", digits = 2)
bget <- function(r, dn) bench$ratio[bench$race == r & bench$den == dn]

# ---- the worked example in the prose ----------------------------------------
# One group's stops over one census count, so the reader can see a ratio in
# Figure 1 built by hand: the count behind a denominator, and stops per person.
aget  <- function(rc, dn) acs$count[acs$race == rc & acs$denominator == dn]
srate <- function(rc, dn) r(rc, "stops") / aget(rc, dn)

## ---- schema
data.frame(
  field = c("date, time", "location", "subject_race", "subject_sex, subject_age",
            "reason_for_stop", "search_conducted", "search_basis",
            "contraband_found", "outcome"),
  what_it_holds = c("when the stop happened", "district or intersection",
                    "the officer's perception, entered on the form",
                    "as recorded by the officer",
                    "the violation cited", "TRUE / FALSE",
                    "consent, probable cause, incident to arrest",
                    "TRUE / FALSE, recorded only if a search happened",
                    "warning, citation, arrest"),
  check.names = FALSE)

## ---- cleanrace
d[order(-d$stops), c("race", "stops", "searched", "contraband_found", "arrests")]

## ---- denominator
data.frame(
  `you would need` = c("Who was driving", "Where and when they were driving",
                       "Who committed a violation", "Who was carrying contraband"),
  `does it exist?` = c("No", "No", "No", "Only for those searched"),
  check.names = FALSE)

## ---- bench-d3
# ---------------------------------------------------------------------------
# A designated showpiece, and the only hand-written D3 left in this brief. No
# library type draws a panel of real denominators beside a marked-out empty one,
# and the empty panel is the argument.
#
# Every ratio drawn here was computed in R, in setup, from acs_denominators.csv
# and by_race.csv, and its printed label was formatted there too. Nothing in
# this figure is recomputed in JavaScript.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the dd_fig() scatter below rides on the
# library loaded here and passes d3 = FALSE.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"r":"%s","d":"%s","v":%.4f,"lab":"%s"}',
                      bench$race, bench$den, bench$ratio, bfmt(bench$ratio)),
              collapse = ",")
cat(sprintf('
<div id="bench" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s],XS=%s,COL=%s;
const W=760,H=440,M={t:26,r:150,b:64,l:62};
const box=d3.select("#bench");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(XS).range([M.l,W-M.r]).padding(0.55);
const y=d3.scaleLog().domain([0.45,5.6]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([0.5,1,2,3,5]).tickFormat(d=>d+"x"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("stop rate relative to white drivers");
// the band between the last real denominator and the one that does not exist
const gapL=(x(XS[2])+x(XS[3]))/2;
svg.append("rect").attr("x",gapL-46).attr("y",M.t).attr("width",92)
  .attr("height",H-M.b-M.t).attr("fill","#f4f4f4").attr("stroke","#bbb")
  .attr("stroke-dasharray","6,4");
svg.append("text").attr("x",gapL).attr("y",M.t+24).attr("text-anchor","middle")
  .attr("font-size","26px").attr("font-weight","600").attr("fill","#aaa").text("?");
["who was actually","driving, and who","committed a","violation"].forEach((t,i)=>
  svg.append("text").attr("x",gapL).attr("y",M.t+48+i*13).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#888").text(t));
svg.append("text").attr("x",gapL).attr("y",H-M.b-8).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("font-style","italic").attr("fill","#999")
  .text("never counted");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(1)).attr("y2",y(1))
  .attr("stroke","#555").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",M.l+4).attr("y",y(1.30)).attr("font-size","11px")
  .attr("fill","#555").text("same rate as white drivers");
XS.forEach(s=>{
  svg.append("text").attr("x",x(s)).attr("y",H-M.b+18).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("fill","#444").text("per "+s.replace("per ",""));});
svg.append("text").attr("x",(x(XS[0])+x(XS[2]))/2).attr("y",H-M.b+40)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#777")
  .text("denominators that exist");
svg.append("text").attr("x",x(XS[3])).attr("y",H-M.b+40).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#777").text("conditioning instead");
const byR=d3.group(D,d=>d.r);
const ln=d3.line().x(d=>x(d.d)).y(d=>y(d.v));
byR.forEach((v,k)=>{
  const real=v.filter(d=>d.d!=="per stop"), cond=v.filter(d=>d.d==="per stop");
  svg.append("path").attr("d",ln(real)).attr("fill","none").attr("stroke",COL[k])
    .attr("stroke-width",2.4);
  svg.append("path").attr("d",ln([real[real.length-1],cond[0]])).attr("fill","none")
    .attr("stroke",COL[k]).attr("stroke-width",1.4).attr("stroke-dasharray","3,3")
    .attr("opacity",0.55);
  svg.append("g").selectAll("circle").data(v).join("circle")
    .attr("cx",d=>x(d.d)).attr("cy",d=>y(d.v)).attr("r",4.5).attr("fill",COL[k]);
  svg.append("text").attr("x",x(XS[3])+10).attr("y",y(cond[0].v)+4)
    .attr("font-size","11.5px").attr("fill",COL[k]).text(k);
  v.filter(d=>k!=="white"||d.d==="per stop").forEach(d=>
    svg.append("text").attr("x",x(d.d)).attr("y",y(d.v)-9)
      .attr("text-anchor","middle").attr("font-size","10px").attr("fill",COL[k])
      .text(d.lab));
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Log scale, so equal distances are equal ratios.</p>
', rows,
   paste0('["', paste(BX, collapse = '","'), '"]'),
   paste0('{', paste(paste0('"', names(BCOL), '":"', BCOL, '"'),
                     collapse = ","), '}')))

## ---- bench-static
# The same ratios, the same log axis, the same labels: base R for the PDF
# device, D3 above for the browser. Both read `bench`, built once in setup.
par(mar = c(4.9, 4.6, 0.8, 8.6))
xp <- c(1, 2, 3, 4.6)                       # gap before the fourth column
plot(NA, xlim = c(0.75, 4.95), ylim = c(0.45, 5.6), log = "y", axes = FALSE,
     xlab = "", ylab = "stop rate relative to white drivers")
rect(3.55, 0.45, 4.05, 5.6, col = "#f4f4f4", border = "#bbb", lty = 2)
text(3.8, 3.6, "?", cex = 2.1, font = 2, col = "#aaa")
text(3.8, 2.15, "who was actually\ndriving, and who\ncommitted a violation",
     cex = 0.56, col = "#888")
text(3.8, 0.52, "never counted", cex = 0.56, col = "#999", font = 3)
axis(2, at = c(0.5, 1, 2, 3, 5), labels = paste0(c(0.5, 1, 2, 3, 5), "x"),
     las = 1, cex.axis = 0.8)
axis(1, at = xp, labels = paste0("per ", sub("^per ", "", BX)), tick = FALSE,
     cex.axis = 0.76, mgp = c(3, 0.5, 0))
abline(h = 1, lty = 2, col = "#555")
text(0.78, 1.30, "same rate as white drivers", adj = c(0, 0), cex = 0.62,
     col = "#555")
mtext("denominators that exist", side = 1, line = 2.1, at = 2, cex = 0.68,
      col = "#777")
mtext("conditioning instead", side = 1, line = 2.1, at = 4.6, cex = 0.68,
      col = "#777")
# NB: not `for (r in ...)` -- `r()` is this document's global accessor helper
for (rc in BR) {
  v <- sapply(BX, function(z) bget(rc, z))
  lines(xp[1:3], v[1:3], col = BCOL[[rc]], lwd = 2.4)
  lines(xp[3:4], v[3:4], col = BCOL[[rc]], lwd = 1.3, lty = 3)
  points(xp, v, pch = 19, col = BCOL[[rc]], cex = 0.95)
  if (rc != "white")            # white is 1.00 by construction; labels collide
    text(xp, v, bfmt(v), pos = 3, cex = 0.58, col = BCOL[[rc]], offset = 0.42)
  else
    text(xp[4], v[4], bfmt(v[4]), pos = 3, cex = 0.58, col = BCOL[[rc]],
         offset = 0.42)
  text(4.78, v[4], rc, adj = c(0, 0.5), cex = 0.66, col = BCOL[[rc]], xpd = NA)
}

## ---- search-rate
o <- d[order(-d$search_rate), c("race", "stops", "searched", "search_rate")]
o$stops <- n(o$stops); o$searched <- n(o$searched)
o$search_rate <- pc(as.numeric(o$search_rate), 2)
names(o) <- c("race", "stops", "searches", "% of stops searched")
o

## ---- hit-rate
o <- d[order(-d$search_rate), c("race", "searched", "contraband_found", "hit_rate")]
o$searched <- n(o$searched); o$contraband_found <- n(o$contraband_found)
o$hit_rate <- pc(as.numeric(o$hit_rate), 1)
names(o) <- c("race", "searches", "contraband found", "% of searches finding contraband")
o

## ---- scatter-static
# WM, the overall share of searches that found contraband, is computed once in
# setup; the dd_fig() version below is handed the same number rather than
# summing the columns again. Point size is constant in both, so neither
# renderer can imply a size encoding the other does not draw.
par(mar = c(4.4, 4.6, 0.8, 1.4))
plot(d$search_rate, d$hit_rate, pch = 19, col = "#C41230", cex = 1.1,
     xlim = c(0, 18), ylim = c(0, 42), las = 1,
     xlab = "% of stops that led to a search",
     ylab = "% of searches that found contraband")
text(d$search_rate, d$hit_rate, d$race, pos = 4, cex = 0.75)
abline(h = WM, lty = 3, col = "grey50")
# the HTML version labels this line; without the label the PDF reader has no way
# to know what it means
text(17.6, WM + 1.4, "equal thresholds would put every group on one horizontal line",
     adj = c(1, 0), cex = 0.62, col = "#777")

## ---- d3-scatter
# The shared chart library draws this one: five points, two rates, one
# reference line. d3 = FALSE because the showpiece above already loaded d3.
sc <- d[, c("race", "stops", "searched", "contraband_found",
            "search_rate", "hit_rate")]
sc$lbl <- sc$race
dd_fig("pol", "scatter", sc,
  size = list(w = 760, h = 430),
  x = list(field = "search_rate", label = "share of stops that led to a search",
           domain = c(0, 18), fmt = "pct0"),
  y = list(field = "hit_rate", label = "share of searches that found contraband",
           domain = c(0, 42), fmt = "pct0"),
  r = 6.5,
  annotations = list(
    list(type = "hline", y = WM),
    list(type = "text", x = 17.6, y = WM + 1.4, anchor = "end",
         text = "equal thresholds would put every group on this line")),
  tip = dd_tip(c(stops = "stops", searched = "searches",
                 search_rate = "% of stops searched",
                 hit_rate = "% of searches finding contraband"),
               fmt = c(stops = "comma", searched = "comma",
                       search_rate = "pct2", hit_rate = "pct1"),
               title = "race"),
  d3 = FALSE)
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a point for that group\'s counts.</p>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
