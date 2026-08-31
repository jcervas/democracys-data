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

## ---- one-record
o <- st[st$name == "Florida" & st$year %in% c(Y1, Y2),
        c("name", "year", "pop", "reps", "repchg", "region", "division")]
o$pop <- n(o$pop)
names(o) <- c("state", "year", "resident population", "seats",
              "change this decade", "region", "division")
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
# Drawn with the shared library. This is the first figure in the document, so
# dd_fig() emits d3 and dd-charts.js here; the hand-written figure below rides
# on the same d3 tag. The four region hues are the series classes the whole
# chapter maps onto RCOL.
m <- moved[order(-moved$change), ]
dd_fig("bars", "bar", m[, c("name", "change", "region")],
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

## ---- regions-seats
o <- data.frame(g = c(REGS, "Northeast + Midwest", "South + West"),
                s1 = c(sapply(REGS, sg, y = 1960), b60["NE+MW"], b60["S+W"]),
                s2 = c(sapply(REGS, sg, y = 2020), b20["NE+MW"], b20["S+W"]))
o$change <- ifelse(o$s2 - o$s1 > 0, paste0("+", o$s2 - o$s1), o$s2 - o$s1)
o$share  <- paste0(pc(100 * o$s1 / 435), "% → ", pc(100 * o$s2 / 435), "%")
names(o) <- c("region", paste("seats", Y1), paste("seats", Y2), "change",
              "share of the House")
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

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
