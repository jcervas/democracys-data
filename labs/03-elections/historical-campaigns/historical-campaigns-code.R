# historical-campaigns-code.R -- chunk bodies for historical-campaigns-brief.Rmd
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

p <- read.csv("data/derived/pres_states_1864_2024.csv", stringsAsFactors = FALSE)
p$d2 <- 100 * p$democrat / (p$democrat + p$republican)
absent <- p[is.na(p$d2), ]

# the national two-party margin, one row per election, from the national file
nat <- read.csv("data/derived/pres_national.csv", stringsAsFactors = FALSE)
dr  <- nat[nat$party %in% c("Democratic", "Republican"), ]
agp <- aggregate(pop_votes ~ year + party, data = dr, FUN = sum)
nm  <- reshape(agp, idvar = "year", timevar = "party", direction = "wide")
names(nm) <- c("year", "dem", "rep")
nm <- nm[order(nm$year), ]
nm$margin <- 100 * abs(nm$dem - nm$rep) / (nm$dem + nm$rep)
dr2  <- dr[order(dr$year, -dr$pop_votes), ]
lead <- dr2[!duplicated(dr2$year), c("year", "candidate")]
nm   <- merge(nm, lead, by = "year")
mg   <- function(y) nm$margin[match(y, nm$year)]

# the most recent election, walked through by hand in the prose so a reader
# can see how a two-party margin is made from two vote totals
nn  <- function(x) format(round(x), big.mark = ",")
t24 <- nm[nm$year == max(nm$year), ]
t24_rep_share <- 100 * t24$rep / (t24$rep + t24$dem)
t24_dem_share <- 100 * t24$dem / (t24$rep + t24$dem)

# the streaks the argument turns on: runs of elections without a
# double-digit winner, and the count of sub-five-point elections
runs <- rle(nm$margin < 10)
RUN_NOW  <- tail(runs$lengths, 1)                       # ends at 2024
RUN_PREV <- max(head(runs$lengths[runs$values], -1))    # the Gilded Age run
LAST_BLOWOUT <- max(nm$year[nm$margin >= 10])
N5_SINCE00 <- sum(nm$margin < 5 & nm$year >= 2000)
GILD5   <- sum(nm$margin < 5 & nm$year >= 1876 & nm$year <= 1896)
MEANMID <- mean(nm$margin[nm$year >= 1904 & nm$year <= 1984])
MEANNOW <- mean(nm$margin[nm$year >= 1988])

# the Solid South gap, for the closing paragraph of the findings
c11 <- c("AL","AR","FL","GA","LA","MS","NC","SC","TN","TX","VA")
p$r <- ifelse(p$state_abbrev %in% c11, "S", "R")
a   <- aggregate(d2 ~ year + r, data = p, FUN = mean, na.rm = TRUE)
g11 <- reshape(a, idvar = "year", timevar = "r", direction = "wide")
names(g11) <- c("year", "rest", "south")
g11$gap <- round(g11$south - g11$rest, 1)
g11 <- g11[order(g11$year), ]
first_neg <- function(w) min(w$year[!is.na(w$gap) & w$gap < 0])
perm_neg  <- function(w) {
  y <- w$year[!is.na(w$gap)]
  min(y[sapply(y, function(yy) all(w$gap[w$year >= yy & !is.na(w$gap)] < 0))])
}

# competitive states: within five points of an even two-party split
p$close <- abs(p$d2 - 50) < 5
ncl <- aggregate(close ~ year, data = p, FUN = sum, na.rm = TRUE)
tot <- as.data.frame(table(p$year)); names(tot) <- c("year", "n")
tot$year <- as.numeric(as.character(tot$year))
ncl <- merge(ncl, tot, by = "year"); ncl$pct <- 100 * ncl$close / ncl$n
cv <- function(y, v) ncl[[v]][ncl$year == y]

# how lopsided was the election overall? (unweighted mean state, the only
# national figure the STATE file can support -- it carries no vote counts)
nt <- aggregate(d2 ~ year, data = p, FUN = mean, na.rm = TRUE)
names(nt)[2] <- "natl"
ncl <- merge(ncl, nt, by = "year")
ncl$tilt <- abs(ncl$natl - 50)
ncl$even <- ncl$tilt < 3            # "the election itself was close"
ev_pre  <- mean(ncl$pct[ncl$even & ncl$year <  2000])
ev_post <- mean(ncl$pct[ncl$even & ncl$year >= 2000])

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)

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

## ---- sample-rows
o <- p[(p$year == 1864 & p$state_abbrev == "CA") |
       (p$year == 1948 & p$state_abbrev == "AL"),
       c("year", "state_abbrev", "democrat", "republican", "other", "winner")]
names(o) <- c("year", "state", "Dem %", "Rep %", "other %", "winner")
o

## ---- natl-static
op <- par(mar = c(3.6, 4.4, 1.0, 1.0), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(nm$year), ylim = c(0, 32), las = 1, xlab = "",
     ylab = "winning margin of the two-party vote (points)")
rect(min(nm$year) - 4, 0, max(nm$year) + 4, 5, col = "#F2F4F5", border = NA)
lines(nm$year, nm$margin, lwd = 2.2, col = "#1C4C5C")
points(nm$year, nm$margin, pch = 19, cex = 0.55, col = "#1C4C5C")
for (yr in c(1880, 1924, 1964, 1984, 2024))
  text(yr, mg(yr) + 1.6, yr, cex = 0.7, col = "#76838C")
text(min(nm$year), 4.1, "decided by less than five points", adj = c(0, 1),
     cex = 0.72, col = "#76838C")
par(op)

## ---- natl-d3
# Drawn with the shared library; dd_fig() emits d3 and dd-charts.js here, and
# the hand-written battleground figure below rides on the same d3 tag. The
# tooltip names the popular-vote leader rather than "the winner", because in
# 1876, 1888, 2000 and 2016 those were different people.
d <- nm[, c("year", "margin", "candidate")]
d$margin <- round(d$margin, 2)
dd_fig("natl", "line", d,
  size = list(w = 770, h = 400, m = list(t = 18, r = 24, b = 40, l = 56)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "margin", label = "winning margin of the two-party vote (points)",
           domain = c(0, 32), fmt = "f0", ticks = 6),
  series = list(fields = list(
    list(field = "margin", label = "margin", class = "series-1"))),
  points = TRUE,
  annotations = list(
    dd_annot_band(0, 5, axis = "y"),
    dd_annot_text(1866, 4.2, "decided by less than five points",
                  class = "foot", size = 11)),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+d.candidate+" led the two-party vote<br>by "+
      d.margin.toFixed(1)+" points";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the chart for each election\'s margin and who led it.</p>')

## ---- close
yy <- c(1896, 1924, 1960, 1976, 2000, 2024)
o <- data.frame(year = yy,
                n = sapply(yy, function(y) cv(y, "close")),
                tot = sapply(yy, function(y) cv(y, "n")),
                pct = pc(sapply(yy, function(y) cv(y, "pct"))))
names(o) <- c("year", "states within 5 points", "states in the union",
              "% competitive")
o

## ---- battle-static
par(mar = c(4, 4.4, 0.6, 1))
plot(ncl$year, ncl$pct, type = "l", col = "#bbbbbb", lwd = 1.6, las = 1,
     xlab = "", ylab = "% of states within 5 points of an even split",
     ylim = c(0, 72))
e_yr <- ncl$year[ncl$even]
segments(min(e_yr), ev_pre, max(e_yr[e_yr < 2000]), ev_pre, col = "#C41230",
         lwd = 2.4, lty = 2)
segments(min(e_yr[e_yr >= 2000]), ev_post, max(e_yr), ev_post, col = "#C41230",
         lwd = 2.4, lty = 2)
points(ncl$year, ncl$pct, pch = 1, cex = 0.7, col = "#999999")
points(ncl$year[ncl$even], ncl$pct[ncl$even], pch = 19, cex = 1.05,
       col = "#C41230")
text(1900, ev_pre + 2.5, paste0("close elections before 2000 averaged ",
     pc(ev_pre), "%"), adj = c(0, 0), cex = 0.76, col = "#C41230")
text(1998, ev_post - 4.5, paste0("since 2000, ", pc(ev_post), "%"),
     adj = c(1, 1), cex = 0.76, col = "#C41230")
for (yr in c(1960, 1976)) text(yr, ncl$pct[ncl$year == yr] + 2.6, yr,
                               cex = 0.72, col = "#C41230")
legend("topleft", c("filled: the election itself was close",
                    "open: a landslide year"),
       pch = c(19, 1), col = c("#C41230", "#999999"), bty = "n", cex = 0.76)

## ---- battle-d3
# Hand-written: the filled/hollow distinction, the two era-average rules and
# the per-election tooltip have no library form. Rides on the d3 tag the
# dd_fig() call above already emitted.
rows <- paste(sprintf('{"y":%d,"p":%.1f,"n":%d,"t":%.1f,"e":%s}',
        ncl$year, ncl$pct, ncl$close, ncl$tilt,
        ifelse(ncl$even, "true", "false")), collapse = ",")
cat(sprintf('
<div id="btl" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],PRE=%.1f,POST=%.1f;
const W=760,H=430,M={t:22,r:24,b:40,l:56};
const box=d3.select("#btl");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1864,2024]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,72]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(7).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% of states within 5 points of an even split");
svg.append("path").attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.p))(D))
  .attr("fill","none").attr("stroke","#c9c9c9").attr("stroke-width",1.8);
const ev=D.filter(d=>d.e),eA=ev.filter(d=>d.y<2000),eB=ev.filter(d=>d.y>=2000);
[[eA[0].y,eA[eA.length-1].y,PRE],[eB[0].y,eB[eB.length-1].y,POST]].forEach(s=>{
  svg.append("line").attr("x1",x(s[0])).attr("x2",x(s[1])).attr("y1",y(s[2]))
    .attr("y2",y(s[2])).attr("stroke","#C41230").attr("stroke-width",2.2)
    .attr("stroke-dasharray","7,4");});
svg.append("text").attr("x",x(1900)).attr("y",y(PRE)-7).attr("font-size","11.5px")
  .attr("fill","#C41230").text("close elections before 2000 averaged "+PRE.toFixed(1)+"%%");
svg.append("text").attr("x",x(2024)).attr("y",y(POST)-9).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#C41230").text("since 2000, "+POST.toFixed(1)+"%%");
D.filter(d=>d.y===1960||d.y===1976).forEach(d=>{
  svg.append("text").attr("x",x(d.y)).attr("y",y(d.p)-11).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#C41230").text(d.y);});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.p)).attr("r",d=>d.e?5:3.4)
  .attr("fill",d=>d.e?"#C41230":"#fff").attr("stroke",d=>d.e?"#C41230":"#999")
  .attr("stroke-width",1.4)
  .on("mousemove",function(ev2,d){
    tip.style("opacity",1).html(`<b>${d.y}</b><br>${d.n} competitive states `+
      `(${d.p.toFixed(1)}%%)<br>average state was ${d.t.toFixed(1)} points off 50<br>`+
      (d.e?"a close election":"a landslide"))
      .style("left",Math.min(ev2.offsetX+14,W-250)+"px").style("top",(ev2.offsetY-46)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Filled points are the %d elections in which the average state sat within three
points of an even split. Hollow points are landslides, where few states are
close anywhere. Hover for the count.</p>
', rows, ev_pre, ev_post, sum(ncl$even)))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
