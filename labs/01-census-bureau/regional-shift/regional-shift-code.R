# regional-shift-code.R -- chunk bodies for regional-shift-brief.Rmd
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

st  <- read.csv("data/derived/states.csv",     stringsAsFactors = FALSE)
reg <- read.csv("data/derived/regions.csv",    stringsAsFactors = FALSE)
nat <- read.csv("data/derived/nation.csv",     stringsAsFactors = FALSE)
sc  <- read.csv("data/derived/seatchange.csv", stringsAsFactors = FALSE)
sd  <- read.csv("data/derived/southdefs.csv",  stringsAsFactors = FALSE)

Y1 <- 1960; Y2 <- 2020
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
sh <- function(r, y) reg$share[reg$region == r & reg$year == y]
sg <- function(r, y) sum(sc[[paste0("reps_", y)]][sc$region == r])
dv <- function(d, y) sd$share[sd$definition == d & sd$year == y]

REGS <- c("Northeast", "Midwest", "South", "West")
RCOL <- c(Northeast = "#2c7fb8", Midwest = "#4d9221",
          South = "#C41230", West = "#e08214")
RLTY <- c(Northeast = 1, Midwest = 2, South = 1, West = 4)

sc      <- sc[order(sc$change, sc$reps_2020), ]
moved   <- sc[sc$change != 0, ]
gross   <- sum(sc$change[sc$change > 0])
netreg  <- tapply(sc$change, sc$region, sum)
blk     <- ifelse(sc$region %in% c("Northeast", "Midwest"), "NE+MW", "S+W")
b60     <- tapply(sc$reps_1960, blk, sum); b20 <- tapply(sc$reps_2020, blk, sum)
ev60    <- b60 + 2 * table(blk); ev20 <- b20 + 2 * table(blk)
ny      <- sc[sc$name == "New York", ]
smaller <- sum(sc$reps_2020 <= abs(ny$change))

# decade-by-decade: how many seats changed hands at each reapportionment
dec <- st[st$year > Y1 & st$name != "District of Columbia" & !is.na(st$repchg), ]
byd <- tapply(dec$repchg, dec$year, function(x) sum(x[x > 0]))
nst <- tapply(dec$repchg, dec$year, function(x) sum(x != 0))
sumdec <- sum(byd)

# Does any state ever move in both directions? And -- the same question asked
# arithmetically -- why the per-decade gains do not equal the sixty-year total.
# For a state that only ever gains, the two are identical. A state that gains a
# seat and later hands it back appears in the per-decade sum twice and in the
# sixty-year sum once, so it contributes exactly one seat of the discrepancy.
# GAPS is that per-state contribution; it is the whole of the difference.
z  <- st[st$year >= Y1 & st$name != "District of Columbia", ]
sp <- split(z[order(z$year), ], z$name[order(z$year)])
mono <- sapply(sp, function(x) { d <- diff(x$reps[order(x$year)])
                                 all(d >= 0) || all(d <= 0) })
NONMONO <- names(mono)[!mono]
POSDEC <- sapply(sp, function(x) { d <- diff(x$reps[order(x$year)]); sum(d[d > 0]) })
NETCHG <- sapply(sp, function(x) { r <- x$reps[order(x$year)]; r[length(r)] - r[1] })
GAPS   <- POSDEC - pmax(NETCHG, 0)
GAPST  <- names(GAPS)[GAPS > 0]

# people per seat: national (from the file) and the interstate spread (computed)
ppr <- st[st$year >= Y1 & st$name != "District of Columbia" & !is.na(st$reps), ]
ppr$per <- ppr$pop / ppr$reps
spread <- do.call(rbind, lapply(split(ppr, ppr$year), function(x)
  data.frame(year = x$year[1], ratio = max(x$per) / min(x$per),
             lo = x$name[which.min(x$per)], hi = x$name[which.max(x$per)])))
P1 <- nat$pop_per_rep[nat$year == 1910]; P2 <- nat$pop_per_rep[nat$year == Y2]

# ---- captions -------------------------------------------------------------
# Every figure ships twice: D3 for the browser, base R for the PDF. Its caption
# ships ONCE, as the numbered paragraph of Markdown that follows the chunk pair,
# so the two formats cannot drift apart and the printed copy cannot end up
# carrying a figure with no caption at all. Nothing that belongs to a caption
# goes inside a chunk. The only text that stays inside the D3 chunks is the
# invitation to hover, which has no meaning on paper.

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- rawapp
# Read from the committed download, not quoted here: this file is small enough
# to keep, so the excerpt is the file rather than a copy of it.
RL <- readLines("data/raw/apportionment_raw.csv", warn = FALSE)
pick <- c(1L,
          grep('^Florida,State,1960,', RL)[1],
          grep('^Florida,State,2020,', RL)[1],
          grep('^Puerto Rico,State,2020,', RL)[1],
          grep('^District of Columbia,State,2020,', RL)[1])
NRAW <- length(RL) - 1L
NCRAW <- 1L + sum(strsplit(RL[1], "", fixed = TRUE)[[1]] == ",")
# The header and four rows, read as a table rather than folded across the page.
# `Geography Type` is the column the paragraph below is about, so it is easier
# to compare four values of it down a column than across four wrapped lines.
read.csv(text = paste(RL[pick], collapse = "\n"), stringsAsFactors = FALSE,
         check.names = FALSE, colClasses = "character")

## ---- cleanstate
st[st$name == "Florida" & st$year %in% c(Y1, Y2),
   c("year", "pop", "region", "south_census", "south_confed",
     "sunbelt", "border_south")]

## ---- one-record
o <- st[st$name == "Florida" & st$year %in% c(Y1, Y2),
        c("name", "year", "pop", "reps", "repchg", "region", "division")]
o$pop <- n(o$pop)
names(o) <- c("state", "year", "resident population", "seats",
              "change this decade", "region", "division")
o

## ---- file
raw   <- read.csv("data/raw/apportionment_raw.csv", stringsAsFactors = FALSE,
                  check.names = FALSE)
gt    <- table(trimws(raw[["Geography Type"]])) / length(unique(nat$year))
gt    <- gt[c("State", "Region", "Nation")]
RAWN  <- nrow(raw)
KB    <- round(file.size("data/raw/apportionment_raw.csv") / 1024)

data.frame(
  item = c("Source", "Rows", "Years", "Kinds of row", "Seats allocated",
           "Key required", "Size"),
  value = c("U.S. Census Bureau, apportionment time series",
            paste(n(RAWN), "data rows"),
            paste(min(nat$year), "to", max(nat$year), "by decade"),
            paste(sprintf("%s (%d/year)", names(gt), as.integer(gt)),
                  collapse = ", "),
            n(sum(sc$reps_2020)), "none", paste("about", n(KB), "KB")))

## ---- map-prep
mp   <- read.csv("data/derived/statemap.csv",  stringsAsFactors = FALSE)
dvl  <- read.csv("data/derived/divmap.csv",    stringsAsFactors = FALSE)
mlab <- read.csv("data/derived/maplabels.csv", stringsAsFactors = FALSE)
MH   <- ceiling(max(mp$y))                      # drawing height at width 760
# the region hues of every figure in this chapter, mixed toward white so the
# fills stay light enough to carry labels and heavy division borders
MPAL <- sapply(RCOL, function(k) colorRampPalette(c("#ffffff", k))(100)[45])
# label the states with room for two letters; the rest are readable by color,
# and by hover in the browser
slab <- mlab[mlab$kind == "state" & (mlab$area > 700 | mlab$abbr == "HI"), ]
rlab <- data.frame(name = c("West", "Midwest", "Northeast", "South"),
                   x = c(232, 445, 645, 548), y = c(20, 24, 32, 442))

## ---- map-static
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 760), ylim = c(MH, 0), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
for (k in split(mp, interaction(mp$name, mp$piece, drop = TRUE)))
  polygon(k$x, k$y, col = MPAL[k$region[1]], border = "#ffffff", lwd = 0.5)
for (k in split(dvl, interaction(dvl$division, dvl$piece, drop = TRUE)))
  lines(k$x, k$y, col = "#4a4a4a", lwd = 1.4)
text(slab$x, slab$y, slab$abbr, cex = 0.45, col = "#333333")
text(rlab$x, rlab$y, toupper(rlab$name), cex = 0.95, font = 2,
     col = RCOL[rlab$name])

## ---- map-d3
# The one d3 <script src> in this document lives here, on the first figure that
# renders in HTML. Every later figure uses the library loaded by this chunk; a
# second copy would silently double the payload.
pth <- function(df, close = TRUE) paste(vapply(split(df, df$piece), function(k)
  paste0("M", paste(sprintf("%.1f,%.1f", k$x, k$y), collapse = "L"),
         if (close) "Z" else ""), ""), collapse = "")
stj <- paste(vapply(unique(mp$name), function(nm) {
  d <- mp[mp$name == nm, ]
  i <- match(nm, sc$name)
  seat <- if (is.na(i)) "null" else
    sprintf('[%d,%d,%d]', sc$reps_1960[i], sc$reps_2020[i], sc$change[i])
  sprintf('{"n":"%s","r":"%s","dv":"%s","s":%s,"p":"%s"}',
          nm, d$region[1], d$division[1], seat, pth(d))
}, ""), collapse = ",")
dvj <- paste(vapply(split(dvl, dvl$division), function(d)
  sprintf('"%s"', pth(d, close = FALSE)), ""), collapse = ",")
slj <- paste(sprintf('{"x":%.1f,"y":%.1f,"t":"%s"}', slab$x, slab$y, slab$abbr),
             collapse = ",")
rlj <- paste(sprintf('{"x":%d,"y":%d,"t":"%s","c":"%s"}', rlab$x, rlab$y,
                     toupper(rlab$name), RCOL[rlab$name]), collapse = ",")
plj <- paste(sprintf('"%s":"%s"', names(MPAL), MPAL), collapse = ",")
cat(sprintf('
<div id="usmap" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const ST=[%s];
const DV=[%s];
const SL=[%s];
const RL=[%s];
const PAL={%s};
const W=760,H=%d;
const wrap=d3.select("#usmap");
const svg=wrap.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const tip=wrap.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("path").data(ST).join("path")
  .attr("d",d=>d.p).attr("fill",d=>PAL[d.r])
  .attr("stroke","#ffffff").attr("stroke-width",0.6)
  .on("mousemove",function(e,d){
    d3.select(this).attr("stroke","#111").attr("stroke-width",1.2).raise();
    const m=d3.pointer(e,wrap.node());
    const seat=d.s===null
      ? "counted every decade, no seat in the House"
      : `${d.s[0]} seats in 1960 \\u2192 ${d.s[1]} in 2020`+
        (d.s[2]===0?"":` (${d.s[2]>0?"+":""}${d.s[2]})`);
    tip.style("opacity",1).html(`<b>${d.n}</b><br>${d.dv} division, ${d.r} region<br>${seat}`)
      .style("left",Math.min(m[0]+16,wrap.node().clientWidth-230)+"px")
      .style("top",(m[1]+10)+"px");
  })
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#ffffff").attr("stroke-width",0.6);
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
Hover a state for its division, region, and seats.</p>
', stj, dvj, slj, rlj, plj, MH))

## ---- ak-hi
o <- st[st$name %in% c("Alaska", "Hawaii") & st$year %in% c(1940, 1950, 1960),
        c("name", "year", "pop", "reps")]
o$pop <- n(o$pop); o$reps[is.na(o$reps)] <- "—"
names(o) <- c("state", "year", "resident population", "seats")
o

## ---- one-decade
o <- st[st$year == Y2 & !is.na(st$repchg) & st$repchg != 0,
        c("name", "region", "reps", "repchg")]
o <- o[order(-o$repchg, o$name), ]
names(o) <- c("state", "region", "seats after", "change")
o

## ---- decades
data.frame(reapportionment = names(byd),
           `seats changing states` = as.integer(byd),
           `states affected` = as.integer(nst),
           check.names = FALSE)

## ---- pivot
data.frame(
  quantity = c(paste("Seats that changed states,", Y1, "to", Y2),
               "States that gained", "States that lost", "States unchanged",
               "New York's total change",
               "States whose entire delegation is smaller than that loss"),
  value = c(n(gross), sum(sc$change > 0), sum(sc$change < 0),
            sum(sc$change == 0), n(ny$change), n(smaller)))

## ---- bars-static
m <- moved[order(moved$change), ]
par(mar = c(3.6, 8.0, 0.4, 1.0))
bp <- barplot(m$change, horiz = TRUE, las = 1, names.arg = m$name,
              col = RCOL[m$region], border = NA, cex.names = 0.55,
              mgp = c(2.2, 0.7, 0), xaxt = "n",
              xlim = c(-19, 19), xlab = paste("seats gained or lost,", Y1, "to", Y2))
axis(1, at = seq(-15, 15, 5), labels = c("-15", "-10", "-5", "0", "+5", "+10", "+15"))
abline(v = 0, col = "#666")
abline(v = seq(-15, 15, 5), col = "#00000018", lty = 3)
text(m$change + ifelse(m$change > 0, 0.5, -0.5), bp,
     ifelse(m$change > 0, paste0("+", m$change), m$change),
     cex = 0.55, pos = ifelse(m$change > 0, 4, 2), offset = 0, col = "#333")
legend("bottomright", names(RCOL), fill = RCOL, border = NA, bty = "n", cex = 0.7)

## ---- bars-d3
# Drawn with the shared library. d3 itself is loaded once, by the map figure
# above, so dd_fig() is told not to emit it a second time; it still emits
# dd-charts.js, which rides beside whatever loaded d3 first. The four region
# hues are the series classes the whole chapter maps onto RCOL.
m <- moved[order(-moved$change), ]
dd_fig("bars", "bar", m[, c("name", "change", "region")], d3 = FALSE,
  x = list(field = "change", domain = c(-17, 17), fmt = "signed0", ticks = 7),
  y = list(field = "name", band = TRUE),
  series = list(field = "region",
                classes = list(Northeast = "series-1", Midwest = "series-3",
                               South = "series-2", West = "series-4")),
  catLabels = "inline", valueLabels = TRUE, legend = TRUE,
  tip = dd_tip(c(region = "region", change = "seat change"),
               fmt = c(change = "signed0"), title = "name"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a bar for the exact figure.</p>')

## ---- slope
tp <- sc[order(-pmax(sc$reps_1960, sc$reps_2020)), ][1:12, ]
tp <- tp[order(-tp$reps_1960, -tp$reps_2020), ]
# push overlapping labels apart without moving the points they belong to
dodge <- function(v, gap) {
  o <- order(v); s <- sort(v)
  for (i in seq_along(s)[-1]) if (s[i] - s[i - 1] < gap) s[i] <- s[i - 1] + gap
  s <- s - (mean(s) - mean(v))          # re-center so nothing drifts far
  out <- numeric(length(v)); out[o] <- s; out
}
tp$lab60 <- dodge(tp$reps_1960, 1.5)
tp$lab20 <- dodge(tp$reps_2020, 1.5)

## ---- slope-static
par(mar = c(0.8, 6.4, 2.2, 6.4))
plot(NA, xlim = c(0, 1), ylim = c(0, 58), axes = FALSE, xlab = "", ylab = "")
mtext(c(Y1, Y2), side = 3, at = c(0, 1), line = 0.2, cex = 0.9, font = 2)
segments(0, 0, 0, 56, col = "#ccc"); segments(1, 0, 1, 56, col = "#ccc")
for (i in seq_len(nrow(tp))) {
  cl <- RCOL[tp$region[i]]
  segments(0, tp$reps_1960[i], 1, tp$reps_2020[i], col = cl, lwd = 2,
           lty = ifelse(tp$change[i] >= 0, 1, 2))
  points(c(0, 1), c(tp$reps_1960[i], tp$reps_2020[i]), pch = 19, cex = 0.8, col = cl)
  segments(-0.015, tp$lab60[i], 0, tp$reps_1960[i], col = cl, lwd = 0.5, xpd = NA)
  segments(1.015, tp$lab20[i], 1, tp$reps_2020[i], col = cl, lwd = 0.5, xpd = NA)
  text(-0.02, tp$lab60[i], paste0(tp$name[i], " ", tp$reps_1960[i]),
       pos = 2, cex = 0.62, col = cl, xpd = NA)
  text(1.02, tp$lab20[i], paste0(tp$reps_2020[i], " ", tp$name[i]),
       pos = 4, cex = 0.62, col = cl, xpd = NA)
}

## ---- slope-d3
rows <- paste(sprintf('{"s":"%s","a":%d,"b":%d,"la":%.2f,"lb":%.2f,"c":"%s"}',
                      tp$name, tp$reps_1960, tp$reps_2020, tp$lab60, tp$lab20,
                      RCOL[tp$region]), collapse = ",")
cat(sprintf('
<div id="slope" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=700,H=430,M={t:34,r:150,b:14,l:150};
const svg=d3.select("#slope").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,56]).range([H-M.b,M.t]);
const xa=M.l,xb=W-M.r;
svg.append("text").attr("x",xa).attr("y",20).attr("text-anchor","middle")
  .attr("font-weight",600).text("%d");
svg.append("text").attr("x",xb).attr("y",20).attr("text-anchor","middle")
  .attr("font-weight",600).text("%d");
[xa,xb].forEach(v=>svg.append("line").attr("x1",v).attr("x2",v)
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#ddd"));
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("line").attr("x1",xa).attr("x2",xb).attr("y1",d=>y(d.a))
  .attr("y2",d=>y(d.b)).attr("stroke",d=>d.c).attr("stroke-width",2.2)
  .attr("stroke-dasharray",d=>d.b>=d.a?null:"5,3");
g.append("circle").attr("cx",xa).attr("cy",d=>y(d.a)).attr("r",4).attr("fill",d=>d.c);
g.append("circle").attr("cx",xb).attr("cy",d=>y(d.b)).attr("r",4).attr("fill",d=>d.c);
g.append("line").attr("x1",xa-8).attr("x2",xa).attr("y1",d=>y(d.la))
  .attr("y2",d=>y(d.a)).attr("stroke",d=>d.c).attr("stroke-width",0.7);
g.append("line").attr("x1",xb+8).attr("x2",xb).attr("y1",d=>y(d.lb))
  .attr("y2",d=>y(d.b)).attr("stroke",d=>d.c).attr("stroke-width",0.7);
g.append("text").attr("x",xa-12).attr("y",d=>y(d.la)+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill",d=>d.c).text(d=>`${d.s} ${d.a}`);
g.append("text").attr("x",xb+12).attr("y",d=>y(d.lb)+4)
  .attr("font-size","11.5px").attr("fill",d=>d.c).text(d=>`${d.b} ${d.s}`);
})();
</script>
', rows, Y1, Y2))

## ---- regions-seats
o <- data.frame(g = c(REGS, "Northeast + Midwest", "South + West"),
                s1 = c(sapply(REGS, sg, y = 1960), b60["NE+MW"], b60["S+W"]),
                s2 = c(sapply(REGS, sg, y = 2020), b20["NE+MW"], b20["S+W"]))
o$change <- ifelse(o$s2 - o$s1 > 0, paste0("+", o$s2 - o$s1), o$s2 - o$s1)
o$share  <- paste0(pc(100 * o$s1 / 435), "% → ", pc(100 * o$s2 / 435), "%")
names(o) <- c("region", paste("seats", Y1), paste("seats", Y2), "change",
              "share of the House")
o

## ---- reversals
o <- data.frame(state = GAPST,
                path = sapply(GAPST, function(k)
                  paste(sp[[k]]$reps[order(sp[[k]]$year)], collapse = " → ")),
                net = ifelse(NETCHG[GAPST] > 0, paste0("+", NETCHG[GAPST]),
                             NETCHG[GAPST]),
                gap = GAPS[GAPST])
names(o) <- c("state", paste("seats,", Y1, "to", Y2, "by decade"),
              "net change", "seats given back")
o

## ---- lines-static
par(mar = c(3.0, 4.2, 0.8, 6.4))
plot(NA, xlim = c(1910, 2020), ylim = c(5, 42), las = 1, xaxt = "n",
     xlab = "", ylab = "% of U.S. resident population")
axis(1, at = seq(1910, 2020, 10), cex.axis = 0.8)
abline(h = seq(10, 40, 10), col = "#00000015")
abline(v = Y1, col = "#999", lty = 3)
text(Y1, 41.4, " 50 states from here", col = "#666", cex = 0.68, pos = 4)
for (r in REGS) {
  q <- reg[reg$region == r, ]; q <- q[order(q$year), ]
  lines(q$year, q$share, col = RCOL[r], lwd = 2.4, lty = RLTY[r])
  text(2021, q$share[q$year == 2020], paste0(" ", r), col = RCOL[r],
       cex = 0.72, pos = 4, xpd = NA)
}

## ---- lines-d3
ser <- paste(sapply(REGS, function(r) {
  q <- reg[reg$region == r, ]; q <- q[order(q$year), ]
  sprintf('{"k":"%s","c":"%s","v":[%s]}', r, RCOL[r],
          paste(sprintf('[%d,%.2f]', q$year, q$share), collapse = ","))
}), collapse = ",")
cat(sprintf('
<div id="lines" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[%s];
const W=770,H=420,M={t:18,r:96,b:38,l:56};
const svg=d3.select("#lines").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const yrs=S[0].v.map(p=>p[0]);
const x=d3.scaleLinear().domain([1910,2020]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([5,42]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).tickValues(yrs));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>d+"%%").ticks(6))
  .call(g=>g.selectAll(".tick line").clone()
    .attr("x2",W-M.r-M.l).attr("stroke","#00000012"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("%% of U.S. resident population");
svg.append("line").attr("x1",x(%d)).attr("x2",x(%d)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#999").attr("stroke-dasharray","3,3");
svg.append("text").attr("x",x(%d)+5).attr("y",M.t+11).attr("font-size","11px")
  .attr("fill","#666").text("50 states from here");
const ln=d3.line().x(p=>x(p[0])).y(p=>y(p[1]));
svg.append("g").selectAll("path").data(S).join("path")
  .attr("fill","none").attr("stroke",s=>s.c).attr("stroke-width",2.5)
  .attr("d",s=>ln(s.v));
svg.append("g").selectAll("text").data(S).join("text")
  .attr("x",W-M.r+6).attr("y",s=>y(s.v[s.v.length-1][1])+4)
  .attr("font-size","12px").attr("font-weight",600).attr("fill",s=>s.c)
  .text(s=>s.k);
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#bbb").attr("opacity",0);
const dots=svg.append("g");
const tip=d3.select("#lines").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const px=d3.pointer(e,this)[0]+M.l;
    const yr=yrs.reduce((a,c)=>Math.abs(x(c)-px)<Math.abs(x(a)-px)?c:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const rw=S.map(s=>({k:s.k,c:s.c,val:s.v.find(p=>p[0]===yr)[1]}))
              .sort((a,b)=>b.val-a.val);
    dots.selectAll("circle").data(rw).join("circle")
      .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",4).attr("fill",r=>r.c);
    tip.style("opacity",1).html(`<b>${yr}</b><br>`+
      rw.map(r=>`<span style="color:${r.c}">\\u25a0</span> ${r.k}: ${r.val}%%`)
        .join("<br>"))
      .style("left",Math.min(x(yr)-M.l+18,W-250)+"px").style("top",(M.t+4)+"px");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);
    dots.selectAll("circle").remove();tip.style("opacity",0);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across for a decade-by-decade readout.</p>
', ser, Y1, Y1, Y1))

## ---- cross
wide <- reshape(reg[, c("region", "year", "share")], idvar = "year",
                timevar = "region", direction = "wide")
wide <- wide[order(wide$year), ]
first_over <- function(a, b) wide$year[which(wide[[a]] > wide[[b]])[1]]
c_sm <- first_over("share.South", "share.Midwest")
c_wn <- first_over("share.West",  "share.Northeast")
c_wm <- first_over("share.West",  "share.Midwest")

## ---- southdefs
o <- do.call(rbind, lapply(unique(sd$definition), function(k)
  data.frame(definition = k,
             n_states = sd$n_states[sd$definition == k][1],
             s60 = dv(k, Y1), s20 = dv(k, Y2))))
o$change <- o$s20 - o$s60
o$s60 <- paste0(pc(o$s60), "%"); o$s20 <- paste0(pc(o$s20), "%")
o$change <- paste0(ifelse(o$change > 0, "+", ""), pc(o$change), " pts")
names(o) <- c("definition", "states", paste("share", Y1), paste("share", Y2),
              "change")
o

## ---- border
BOR <- st[st$border_south & st$year %in% c(Y1, Y2), c("name", "year", "pop")]
BOR$share <- 100 * BOR$pop / nat$pop[match(BOR$year, nat$year)]
bw <- reshape(BOR[, c("name", "year", "share")], idvar = "name",
              timevar = "year", direction = "wide")
names(bw) <- c("name", "s1", "s2")
bw$change <- bw$s2 - bw$s1
bw <- bw[order(-bw$change), ]
CONF <- dv("Confederate South (11 states)", Y2) -
        dv("Confederate South (11 states)", Y1)
bg <- function(k, v) bw[[v]][bw$name == k]

## ---- bordertab
o <- data.frame(unit = bw$name,
                s1 = paste0(pc(bw$s1, 2), "%"),
                s2 = paste0(pc(bw$s2, 2), "%"),
                eff = paste0(ifelse(bw$change > 0, "+", ""), pc(bw$change, 2),
                             " pts"))
names(o) <- c("unit added", paste("share", Y1), paste("share", Y2),
              "effect on the measured rise")
o

## ---- perseat
data.frame(
  quantity = c("People per representative, 1910",
               paste("People per representative,", Y2), "Multiple",
               "U.S. population, 1910 → 2020", "Multiple",
               "House seats, 1910 → 2020"),
  value = c(n(P1), n(P2), paste0(pc(P2 / P1, 2), "×"),
            paste(n(nat$pop[nat$year == 1910]), "→", n(nat$pop[nat$year == Y2])),
            paste0(pc(nat$pop[nat$year == Y2] / nat$pop[nat$year == 1910], 2), "×"),
            paste(n(nat$reps[nat$year == 1910]), "→",
                  n(nat$reps[nat$year == Y2]))))

## ---- step-static
par(mar = c(3.0, 4.6, 0.8, 1.2))
plot(nat$year, nat$pop_per_rep / 1000, type = "s", lwd = 2.6, col = "#8856a7",
     xlim = c(1910, 2024), ylim = c(0, 830), las = 1, xaxt = "n", xlab = "",
     ylab = "thousands of people per representative")
axis(1, at = seq(1910, 2020, 10), cex.axis = 0.8)
abline(h = seq(200, 800, 200), col = "#00000015")
points(nat$year, nat$pop_per_rep / 1000, pch = 19, cex = 0.7, col = "#8856a7")
segments(1910, P1 / 1000, 2020, P1 / 1000, col = "#999", lty = 2)
text(1962, P1 / 1000 + 30, "the 1910 district", col = "#666", cex = 0.72)
text(2020, P2 / 1000, paste0(n(P2), " "), pos = 2, cex = 0.72, col = "#8856a7")
# below the dashed 1910 rule, not on it, so the two do not overprint
text(1910, P1 / 1000 - 34, paste0(" ", n(P1)), pos = 4, cex = 0.72, col = "#8856a7")

## ---- step-d3
pts <- paste(sprintf('[%d,%d]', nat$year, nat$pop_per_rep), collapse = ",")
cat(sprintf('
<div id="perrep" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=360,M={t:18,r:70,b:38,l:64};
const svg=d3.select("#perrep").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1910,2020]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,830000]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).tickValues(D.map(d=>d[0])));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(5).tickFormat(d=>(d/1000)+"k"))
  .call(g=>g.selectAll(".tick line").clone()
    .attr("x2",W-M.r-M.l).attr("stroke","#00000012"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("people per representative");
const ln=d3.line().curve(d3.curveStepAfter).x(d=>x(d[0])).y(d=>y(d[1]));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#8856a7")
  .attr("stroke-width",2.6).attr("d",ln);
svg.append("line").attr("x1",x(1910)).attr("x2",x(2020))
  .attr("y1",y(D[0][1])).attr("y2",y(D[0][1]))
  .attr("stroke","#999").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(1962)).attr("y",y(D[0][1])-8)
  .attr("font-size","11px").attr("fill","#666").text("the 1910 district");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",4).attr("fill","#8856a7")
  .append("title").text(d=>`${d[0]}: ${d3.format(",")(d[1])} people per seat`);
svg.append("text").attr("x",x(2020)+6).attr("y",y(D[D.length-1][1])+4)
  .attr("font-size","11.5px").attr("fill","#8856a7")
  .text(d3.format(",")(D[D.length-1][1]));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover for exact values.</p>
', pts))

## ---- spread
o <- spread[spread$year %in% c(Y1, Y2), ]
o$ratio <- pc(o$ratio, 2)
names(o) <- c("year", "largest ÷ smallest", "smallest state district",
              "largest state district")
o

## ---- ec
data.frame(
  quantity = c(paste("Northeast + Midwest electoral votes,", Y1),
               paste("Northeast + Midwest electoral votes,", Y2),
               paste("South + West electoral votes,", Y1),
               paste("South + West electoral votes,", Y2),
               "Electoral votes that changed region"),
  value = c(paste0(n(ev60["NE+MW"]), " of ", n(sum(ev60)),
                   "  (", pc(100 * ev60["NE+MW"] / sum(ev60)), "%)"),
            paste0(n(ev20["NE+MW"]), " of ", n(sum(ev20)),
                   "  (", pc(100 * ev20["NE+MW"] / sum(ev20)), "%)"),
            paste0(n(ev60["S+W"]), " of ", n(sum(ev60)),
                   "  (", pc(100 * ev60["S+W"] / sum(ev60)), "%)"),
            paste0(n(ev20["S+W"]), " of ", n(sum(ev20)),
                   "  (", pc(100 * ev20["S+W"] / sum(ev20)), "%)"),
            n(netreg["South"] + netreg["West"])))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
