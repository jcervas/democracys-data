# surnames-code.R -- chunk bodies for surnames-brief.Rmd
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

# `sn` is the CURRENT surname table -- 2020 -- because that is what BISG runs
# on everywhere in this book. `sn10` is the 2010 tabulation it replaced, kept
# because the cleaning section is about what the 2010 file refused to publish,
# and that refusal is a fact about the file rather than about surnames.
sn   <- read.csv("data/derived/census_surnames.csv", stringsAsFactors = FALSE)
sn10 <- read.csv("data/derived/census_surnames_2010.csv", stringsAsFactors = FALSE)
s20n <- read.csv("data/derived/surnames_2020_negatives.csv", stringsAsFactors = FALSE)
f20  <- read.csv("data/derived/surnames_2020_facts.csv",   stringsAsFactors = FALSE)
F20  <- function(k) f20$value[f20$key == k]
co <- read.csv("data/derived/county_race.csv", stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))

grp  <- c("pctwhite", "pctblack", "pctapi", "pctaian", "pct2prace", "pcthispanic")
grp6 <- c("white", "black", "api", "aian", "twoplus", "hispanic")
pretty6 <- c("white", "Black", "Asian/PI", "Am. Indian", "two or more",
             "Hispanic")

# ---- one palette for this document ----------------------------------------
# Two different things get colored here, and each gets its own channel so that
# no color carries two meanings. Green and red in particular mean "white" and
# "Black" everywhere in this document and nothing else.
#   GCOL  a group. Categorical, six hues, fixed for the whole document.
#   ACOL  what adding geography does. A before/after pair, in a hue used
#         nowhere else. The two hues are the shared library's tan and teal
#         (--dd-s7, --dd-s6), because Figure 3 is drawn with dd_fig() and the
#         static twin has to print what the screen shows.
GCOL <- c("#4d9221", "#C41230", "#2c7fb8", "#e08214", "#999999", "#8856a7")
names(GCOL) <- pretty6
ACOL <- c(before = "#8A5B1B", after = "#00857C")

natl <- colSums(co[, grp6] * co$pop) / sum(co$pop)
# which(), not a bare logical: the surname list contains the real American
# surname NA, which read.csv turns into a missing value. A bare `==` then
# returns NA for that row, subsetting adds a row of NAs to every lookup, and
# every inline figure in this brief printed as "43.8, NA%".
look <- function(nm) sn[which(sn$name == toupper(nm)), ]
bisg <- function(name, fips) {
  s <- look(name)
  p <- c(s$pctwhite, s$pctblack, s$pctapi, s$pctaian, s$pct2prace,
         s$pcthispanic) / 100
  p[is.na(p)] <- 0
  g <- as.numeric(co[co$fips == fips, grp6])
  u <- p * g / natl
  setNames(round(100 * u / sum(u), 1), grp6)
}

sn$top <- apply(sn[, grp], 1, function(x) max(x, na.rm = TRUE))
ok  <- is.finite(sn$top) & !is.na(sn$count)
pop_covered <- sum(sn$count[ok])
p_decisive  <- 100 * sum(sn$count[ok & sn$top > 90]) / pop_covered
n_useless   <- sum(sn$count[ok & sn$top < 60])
p_useless   <- 100 * n_useless / pop_covered

big  <- sn[sn$count > 1000, ]
n90  <- sapply(grp, function(v) sum(big[[v]] > 90, na.rm = TRUE))
names(n90) <- pretty6

# Suppression is a 2010 phenomenon: the 2020 file publishes every cell, having
# noised them instead. Both numbers describe the older table.
cells <- nrow(sn10) * length(grp)
miss  <- sum(is.na(sn10[, grp]))

gains <- t(sapply(c("SMITH","WILLIAMS","JOHNSON","LEE","NGUYEN","GARCIA"),
  function(nm) { s <- look(nm)
    b <- max(c(s$pctwhite, s$pctblack, s$pctapi, s$pcthispanic), na.rm = TRUE)
    a <- max(bisg(nm, "36005")); c(before = b, after = a, change = a - b) }))
# ordered here rather than inside a figure chunk, because the prose after
# Figure 3 reads the ends of it and only one of the two twins ever runs
gd <- gains[order(gains[, "change"]), , drop = FALSE]

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
G  <- function(nm, v) look(nm)[[v]]

# ---- figure data ----------------------------------------------------------

# the thousand most common surnames, on the two shares they share
k1  <- sn[!is.na(sn$rank) & sn$rank >= 1 & sn$rank <= 1000, ]
k1$pctblack[is.na(k1$pctblack)] <- 0
k1_share  <- 100 * sum(k1$count) / pop_covered
k1_w90    <- sum(k1$pctwhite > 90, na.rm = TRUE)
k1_b90    <- sum(k1$pctblack > 90, na.rm = TRUE)
k1_maxb   <- k1[which.max(k1$pctblack), ]
mark      <- c("SMITH", "WILLIAMS", "JOHNSON", "WASHINGTON", "NGUYEN",
               "GARCIA", "LEE", "YODER")


# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- look("SMITH")[, c("name", "rank", "count", grp)]
o$count <- n(o$count)
names(o) <- c("surname", "rank", "people holding it", pretty6)
o

## ---- neg2020
o <- head(s20n, 6)
o$row_total <- n(o$row_total)
names(o) <- c("last name", "category", "published", "with negatives",
              "people with that name")
o

## ---- ambiguous
o <- rbind(look("SMITH"), look("JOHNSON"), look("WILLIAMS"))[, c("name", "rank", "count", grp)]
o$count <- n(o$count); names(o) <- c("surname", "rank", "people", pretty6)
o

## ---- kilo-static
par(mar = c(4.4, 4.6, 1.0, 1.2))
plot(k1$pctwhite, k1$pctblack, xlim = c(0, 100), ylim = c(0, 100), las = 1,
     pch = 16, cex = 0.28 + sqrt(k1$count) / 900,
     col = adjustcolor("#2c7fb8", alpha.f = 0.45),
     xlab = "% of the people holding this surname who are white",
     ylab = "% who are Black")
abline(a = 100, b = -1, lty = 3, col = "grey55")
abline(h = 90, v = 90, lty = 2, col = "#999")
text(45, 93, paste0("more than 90% Black: ", k1_b90, " of the thousand"),
     adj = c(0.5, 0), cex = 0.72, col = "#C41230")
text(87, 52, paste0("more than 90% white:\n", k1_w90), adj = c(1, 0.5),
     cex = 0.72, col = "#4d9221")
mk  <- k1[k1$name %in% mark, ]
lpos <- c(SMITH = 4, WILLIAMS = 4, JOHNSON = 4, WASHINGTON = 4, NGUYEN = 3,
          GARCIA = 4, LEE = 3, YODER = 2)
points(mk$pctwhite, mk$pctblack, pch = 21, cex = 1.25, bg = "#C41230",
       col = "white", lwd = 1.2)
text(mk$pctwhite, mk$pctblack, mk$name, pos = lpos[mk$name], cex = 0.68,
     col = "#333", offset = 0.45)

## ---- kilo-d3
rows <- paste(sprintf('{"x":%.2f,"y":%.2f,"c":%d,"n":"%s"}',
                      k1$pctwhite, k1$pctblack, k1$count,
                      ifelse(k1$name %in% mark, k1$name, "")),
              collapse = ",")
cat(sprintf('
<div id="kilo" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=740,H=470,M={t:16,r:20,b:52,l:60};
const box=d3.select("#kilo");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("%% of the people holding this surname who are white");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% who are Black");
svg.append("line").attr("x1",x(0)).attr("y1",y(100)).attr("x2",x(100)).attr("y2",y(0))
  .attr("stroke","#aaa").attr("stroke-dasharray","3,3");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(90)).attr("y2",y(90))
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("line").attr("x1",x(90)).attr("x2",x(90)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(3)).attr("y",y(92)).attr("font-size","11.5px")
  .attr("fill","#C41230").text("more than 90%% Black: %d of the thousand");
svg.append("text").attr("x",x(91)).attr("y",y(18)).attr("font-size","11.5px")
  .attr("fill","#4d9221").text("more than");
svg.append("text").attr("x",x(91)).attr("y",y(13)).attr("font-size","11.5px")
  .attr("fill","#4d9221").text("90%% white: %d");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",d=>Math.max(1.6,Math.sqrt(d.c)/220))
  .attr("fill",d=>d.n?"#C41230":"#2c7fb8").attr("fill-opacity",d=>d.n?0.95:0.42)
  .attr("stroke",d=>d.n?"#fff":"none").attr("stroke-width",1.2)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.n||"one of the thousand"}</b><br>${d3.format(",")(d.c)} people<br>`+
     `${d.x.toFixed(1)}%% white &middot; ${d.y.toFixed(1)}%% Black`)
     .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.l").data(D.filter(d=>d.n)).join("text")
  .attr("x",d=>x(d.x)).attr("y",d=>y(d.y)-10).attr("text-anchor","middle")
  .attr("font-size","11px").attr("font-weight","600").attr("fill","#333")
  .text(d=>d.n);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a circle for the name and the counts behind it.</i></p>
', rows, k1_b90, k1_w90, k1_share, k1_maxb$name, k1_maxb$pctblack))

## ---- asym-prep
# DOTS, not bars. A bar says "compare my length"; on a logarithmic axis that
# length is the logarithm, so a 99-to-1 ratio would be drawn as roughly 2.5
# to 1 and the reader would take the wrong number away from the one figure in
# this document whose entire point is the ratio. A dot says "read my
# position", which is what a log axis is for. The ratio is then stated in
# words next to the two dots it belongs to.
av    <- sort(n90)
azero <- av == 0                      # a true zero has no place on a log axis
aratio <- n90[["white"]] / max(n90[["Black"]], 1)
alab  <- sprintf("%s to 1", format(round(aratio), big.mark = ",", trim = TRUE))
acap  <- sprintf(paste(
  "Surnames held by more than a thousand people. The axis is logarithmic, so",
  "each gridline is ten times the one before it and equal distances are equal",
  "MULTIPLES, not equal counts: white to Black is %s. %s is a true zero and",
  "cannot be placed on a logarithmic axis at all, so it is marked with an",
  "open circle at the left edge."),
  alab, names(av)[azero][1])

## ---- asym-static
par(mar = c(7.6, 8.5, 1.2, 4.2))
plot(NA, xlim = c(1, 40000), ylim = c(0.5, length(av) + 0.5), log = "x",
     axes = FALSE, xlab = "", ylab = "")
abline(v = 10^(0:4), col = "grey92")
axis(1, at = 10^(0:4), labels = c("1", "10", "100", "1,000", "10,000"),
     cex.axis = 0.8, col = "grey70", col.axis = "#666666")
axis(2, at = seq_along(av), labels = names(av), las = 1, tick = FALSE,
     cex.axis = 0.9)
mtext("surnames more than 90% this group (log scale)", side = 1, line = 2.4,
      cex = 0.85)
segments(1, seq_along(av), pmax(av, 1), seq_along(av), col = "grey78",
         lwd = 1.6)
points(pmax(av, 1)[!azero], which(!azero), pch = 19, cex = 1.5,
       col = GCOL[names(av)][!azero])
points(1, which(azero), pch = 21, cex = 1.5, bg = "white",
       col = GCOL[names(av)][azero], lwd = 2)
text(pmax(av, 1), seq_along(av), paste0("  ", format(av, big.mark = ",",
     trim = TRUE)), pos = 4, cex = 0.8, xpd = NA, col = "#333333")
cw <- strwrap(acap, width = 96)
mtext(cw, side = 1, line = 3.9 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.66, col = "#555555")

## ---- asym-d3
v <- sort(n90, decreasing = TRUE)
rows <- paste(sprintf('{"g":"%s","v":%d,"t":"%s","c":"%s"}',
                      names(v), as.integer(v),
                      format(as.integer(v), big.mark = ",", trim = TRUE),
                      GCOL[names(v)]),
              collapse = ",")
cat(sprintf('
<div id="asym" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=290,M={t:16,r:74,b:46,l:110};
const svg=d3.select("#asym").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
// dots, not bars: see the comment in the asym-prep chunk
const x=d3.scaleLog().domain([1,40000]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.g)).range([M.t,H-M.b]).padding(0.24);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickValues([1,10,100,1000,10000])
    .tickFormat(d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("surnames more than 90%% this group (log scale)");
[1,10,100,1000,10000].forEach(g=>svg.append("line").attr("x1",x(g)).attr("x2",x(g))
  .attr("y1",M.t-4).attr("y2",H-M.b).attr("stroke","#eee"));
const cy=d=>y(d.g)+y.bandwidth()/2;
svg.append("g").selectAll("line.s").data(D).join("line")
  .attr("x1",x(1)).attr("x2",x(1)).attr("y1",cy).attr("y2",cy)
  .attr("stroke","#c9c9c9").attr("stroke-width",1.8)
  .transition().duration(750).attr("x2",d=>x(Math.max(d.v,1)));
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",x(1)).attr("cy",cy).attr("r",6)
  .attr("fill",d=>d.v>0?d.c:"#fff")
  .attr("stroke",d=>d.c).attr("stroke-width",d=>d.v>0?0:2)
  .transition().duration(750).attr("cx",d=>x(Math.max(d.v,1)));
svg.append("g").selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(Math.max(d.v,1))+11).attr("y",d=>cy(d)+4)
  .attr("font-size","11.5px").attr("fill","#333").attr("opacity",0)
  .text(d=>d.t)
  .transition().delay(750).duration(300).attr("opacity",1);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s</p>
', rows, acap))

## ---- flip
o <- rbind(`SMITH in Allegheny County, PA` = bisg("SMITH", "42003"),
           `SMITH in the Bronx, NY`        = bisg("SMITH", "36005"),
           `LEE in Allegheny County, PA`   = bisg("LEE", "42003"),
           `LEE in Honolulu County, HI`    = bisg("LEE", "15003"))
o <- data.frame(name_and_place = rownames(o), o, check.names = FALSE)
names(o) <- c("surname and place", pretty6)
o

## ---- gains-static
par(mar = c(4.4, 7.0, 1.6, 3.2))
plot(NA, xlim = c(0, 100), ylim = c(0.6, nrow(gd) + 0.4), yaxt = "n", bty = "n",
     las = 1, ylab = "",
     xlab = "strongest guess about a person's race (%)")
axis(2, at = seq_len(nrow(gd)), labels = rownames(gd), las = 1, tick = FALSE,
     cex.axis = 0.9)
abline(v = seq(0, 100, 20), col = "grey92")
segments(gd[, "before"], seq_len(nrow(gd)), gd[, "after"], seq_len(nrow(gd)),
         lwd = 3.4, col = "grey70", lend = 1)
points(gd[, "before"], seq_len(nrow(gd)), pch = 21, bg = "white",
       col = ACOL[["before"]], cex = 1.5, lwd = 2)
points(gd[, "after"], seq_len(nrow(gd)), pch = 19, col = ACOL[["after"]],
       cex = 1.5)
text(pmax(gd[, "before"], gd[, "after"]), seq_len(nrow(gd)),
     sprintf(" %+.1f", gd[, "change"]), pos = 4, cex = 0.78,
     col = ACOL[["after"]], xpd = NA)
legend("top", c("from the name alone", "after adding the Bronx"),
       pch = c(21, 19), col = unname(ACOL), pt.bg = "white",
       bty = "n", horiz = TRUE, cex = 0.78, inset = c(0, -0.06), xpd = NA)

## ---- gains-d3
# Drawn with the shared library (_lib/dd-charts.js), as the house convention asks: a
# before/after rod per row is exactly dd_fig("dumbbell"), and nothing about
# this figure needs hand-written D3. d3 = FALSE because the kilo figure above
# is a designated showpiece and has already put the d3 tag on the page; a
# second copy would double the payload once pandoc inlines it.
dd_fig("gns", "dumbbell",
  data.frame(surname = rownames(gd), before = gd[, "before"],
             after = gd[, "after"], change = gd[, "change"],
             stringsAsFactors = FALSE),
  height = 320, d3 = FALSE,
  y = list(field = "surname"),
  a = list(field = "before", label = "from the name alone"),
  b = list(field = "after",  label = "after adding the Bronx"),
  x = list(domain = c(0, 100), fmt = "pct0",
           label = "strongest guess about a person\u2019s race"),
  aClass = "series-7", bClass = "series-6",
  tip = dd_tip(c(before = "from the name alone",
                 after  = "after adding the Bronx",
                 change = "change"),
               fmt = c(before = "pct1", after = "pct1", change = "signed1"),
               title = "surname"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>The tan end is the name alone; the teal end is the name plus one county.
Hover a rod for both figures and the change between them.</i></p>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
