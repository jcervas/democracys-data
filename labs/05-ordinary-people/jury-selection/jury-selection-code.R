# jury-selection-code.R -- chunk bodies for jury-selection-brief.Rmd
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
s <- read.csv("data/derived/strikes.csv", stringsAsFactors = FALSE)
t <- read.csv("data/derived/trials.csv",  stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("flowers.csv", "strikes.csv", "trials.csv"))
t$eligible <- t$black_eligible + t$white_eligible
t$struck   <- t$black_struck   + t$white_struck
t$p <- phyper(t$black_struck - 1, t$black_eligible, t$white_eligible,
              t$struck, lower.tail = FALSE)
g  <- function(sd, rc, v) s[[v]][s$side == sd & s$race == rc]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# the years the file covers, read off the trial identifiers rather than typed
YRS <- as.integer(substr(t$trial, 1, 4))
YR0 <- min(YRS); YR1 <- max(YRS)

# ---- the trial the brief opens on -------------------------------------------
# Row 1 of the file, and the only trial named in the opening paragraphs.
# Everything said about it is read out of these five numbers, so the opening
# and the limit stated near the end cannot drift apart.
o1 <- t[1, ]
NB <- o1$black_eligible; NW <- o1$white_eligible
KS <- o1$struck;         SB <- o1$black_struck
PV <- o1$p
ONE_IN <- round(1 / PV)                     # "about one chance in twenty-three"

# the single most lopsided trial in the file, quoted by name in the prose
FL <- t[which.min(t$p), ]

# ---- how many trials would look significant if nothing were going on? --------
# The tempting answer is 0.05 * 211 = 10.6, but that assumes p-values spread
# evenly over [0,1]. These are hypergeometric tests on small integer counts, so
# their p-values are DISCRETE and land in a handful of coarse steps; a test that
# cannot produce a value between 0.03 and 0.11 simply never fires at 0.05. The
# tests are therefore conservative, and the honest null is simulated, not
# assumed: redraw each trial's strikes from its own margins and re-count.
set.seed(20260810)                     # fixed so the PDF is identical each knit
NSIM <- 2000
THR  <- seq(0.005, 0.20, by = 0.005)
sim1 <- function(i) {
  bs <- rhyper(nrow(t), t$black_eligible, t$white_eligible, t$struck)
  pp <- phyper(bs - 1, t$black_eligible, t$white_eligible, t$struck,
               lower.tail = FALSE)
  colSums(outer(pp, THR, "<"))
}
simM   <- sapply(seq_len(NSIM), sim1)          # length(THR) x NSIM
obs_c  <- colSums(outer(t$p, THR, "<"))
nul_m  <- rowMeans(simM)
nul_lo <- apply(simM, 1, quantile, 0.025)
nul_hi <- apply(simM, 1, quantile, 0.975)
i05    <- which(abs(THR - 0.05) < 1e-9)        # the conventional threshold
exp05  <- nul_m[i05]                           # simulated, not 0.05 * n
obs05  <- obs_c[i05]
ratio05 <- obs05 / exp05
naive05 <- 0.05 * nrow(t)
naive_ratio <- obs05 / naive05                 # the ratio the shortcut gives

# ---- the four bars of Figure 1 ----------------------------------------------
# Built once, read by both renderers, so the two cannot disagree about the
# order of the bars, the values on them or the top of the axis.
mir <- data.frame(
  bar  = c("state, Black jurors", "state, white jurors",
           "defense, Black jurors", "defense, white jurors"),
  race = c("Black jurors", "white jurors", "Black jurors", "white jurors"),
  pct  = c(g("state", "Black", "pct"),   g("state", "White", "pct"),
           g("defense", "Black", "pct"), g("defense", "White", "pct")),
  stringsAsFactors = FALSE)
MIRY <- 66                          # one y limit, used by both renderers
MCOL <- c(`Black jurors` = "#C41230", `white jurors` = "#2c7fb8")

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

## ---- clean-jury
o <- s[, c("side", "race", "eligible", "struck", "pct")]
o$eligible <- n(o$eligible); o$struck <- n(o$struck)
names(o) <- c("side", "race", "could be struck", "struck", "%")
o

## ---- counts
data.frame(
  quantity = c("Trials in this file", "Prospective jurors eligible to be struck",
               "Struck by either side", "Prosecutor's office", "Period"),
  value = c(nrow(t), n(sum(t$eligible)), n(sum(t$struck)),
            "Mississippi's Fifth Circuit District", paste(YR0, "to", YR1)))

## ---- state-rates
o <- s[s$side == "state", c("race", "eligible", "struck", "pct")]
o$eligible <- n(o$eligible); o$struck <- n(o$struck)
names(o) <- c("race", "eligible to be struck", "struck by the state", "% struck")
o

## ---- mirror-static
# The four rates in strikes.csv, in the order the D3 twin below draws them:
# state first, defense second, Black bar then white bar inside each. Both
# renderers read `mir` and the same y limit, so neither can pick a scale or an
# order the other does not use.
par(mar = c(4.6, 4.6, 2.6, 1.2))
bp <- barplot(mir$pct, names.arg = NA, col = MCOL[mir$race], border = NA,
              ylim = c(0, MIRY), las = 1, space = c(0.4, 0.12, 0.7, 0.12),
              ylab = "% of eligible jurors struck")
text(bp, mir$pct + MIRY * 0.028, paste0(pc(mir$pct), "%"),
     cex = 0.78, font = 2, col = MCOL[mir$race])
text(bp, -MIRY * 0.055, sub(", ", ",\n", mir$bar), cex = 0.7, xpd = NA)
legend(x = mean(bp), y = MIRY * 1.10, xjust = 0.5, horiz = TRUE,
       legend = names(MCOL), fill = unname(MCOL), border = NA,
       bty = "n", cex = 0.88, xpd = NA)

## ---- d3-mirror
# ---------------------------------------------------------------------------
# The four rates in strikes.csv, drawn by the shared chart library: one bar per
# side-and-race, coloured by the juror's race. `mir` is built once in setup and
# read by the static twin above, so the two cannot disagree.
#
# This chunk carries the ONE d3 <script src> for the document, emitted by
# dd_libs() inside dd_fig(). A second copy would silently double the payload;
# the threshold figure below uses the library loaded here.
# ---------------------------------------------------------------------------
dd_fig("mir", "bar", mir,
  size = list(w = 760, h = 360, m = list(t = 16, r = 20, b = 62, l = 58)),
  x = list(field = "bar"),
  y = list(field = "pct", label = "% of eligible jurors struck",
           domain = c(0, MIRY), fmt = "pct1"),
  series = list(field = "race",
                classes = list(`Black jurors` = "series-1",
                               `white jurors` = "series-2")),
  valueLabels = TRUE, legend = TRUE, tiltLabels = TRUE,
  tip = dd_tip(c(pct = "struck"), fmt = c(pct = "pct1"), title = "bar"))
## ---- many-trials
data.frame(
  quantity = c("Trials significant at p < 0.05",
               "Expected by chance: the textbook shortcut",
               "Expected by chance: simulated from each trial's own margins",
               "Trials significant at p < 0.01",
               "Trials where every eligible Black juror was struck"),
  value = c(obs05, pc(naive05, 1), pc(exp05, 1), sum(t$p < 0.01),
            sum(t$black_eligible > 0 & t$black_struck == t$black_eligible)))

## ---- thresh-static
par(mar = c(4.3, 4.4, 0.8, 1.2))
plot(NA, xlim = c(0, max(THR)), ylim = c(0, max(obs_c) * 1.08), las = 1,
     xlab = "significance threshold applied to every trial",
     ylab = "trials called significant")
polygon(c(THR, rev(THR)), c(nul_lo, rev(nul_hi)),
        col = adjustcolor("#8899aa", 0.30), border = NA)
lines(THR, nul_m, type = "s", col = "#55606c", lwd = 2, lty = 2)
lines(THR, obs_c, type = "s", col = "#C41230", lwd = 2.6)
abline(v = 0.05, col = "grey55", lty = 3)
points(0.05, obs05, pch = 19, col = "#C41230", cex = 1.1)
points(0.05, exp05, pch = 19, col = "#55606c", cex = 1.1)
text(0.054, obs05, paste0(obs05, " observed"), adj = c(0, 0.5), cex = 0.76,
     col = "#C41230", font = 2)
text(0.054, exp05 + max(obs_c) * 0.055, paste0(pc(exp05, 1), " expected"),
     adj = c(0, 0.5), cex = 0.76, col = "#55606c")
text(0.05, max(obs_c) * 1.06, "p = 0.05", adj = c(0.5, 1), cex = 0.7,
     col = "grey40")
legend("topleft", bty = "n", cex = 0.76, inset = c(0, 0.02),
       legend = c("what happened", "chance alone (mean)",
                  "chance alone (95% of simulations)"),
       col = c("#C41230", "#55606c", adjustcolor("#8899aa", 0.55)),
       lwd = c(2.6, 2, 8), lty = c(1, 2, 1))

## ---- thresh-d3
rows <- paste(sprintf('{"th":%.3f,"o":%d,"m":%.3f,"lo":%.1f,"hi":%.1f}',
                      THR, obs_c, nul_m, nul_lo, nul_hi), collapse = ",")
cat(sprintf('
<div id="thr" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s],OBS=%d,EXP=%s,NS="%s";
const W=760,H=430,M={t:22,r:26,b:50,l:58};
const box=d3.select("#thr");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.th)]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.o)*1.08]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("significance threshold applied to every trial");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("trials called significant");
svg.append("path").datum(D).attr("fill","#8899aa").attr("fill-opacity",0.30)
  .attr("d",d3.area().curve(d3.curveStepAfter).x(d=>x(d.th))
    .y0(d=>y(d.lo)).y1(d=>y(d.hi)));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#55606c")
  .attr("stroke-width",2).attr("stroke-dasharray","6,4")
  .attr("d",d3.line().curve(d3.curveStepAfter).x(d=>x(d.th)).y(d=>y(d.m)));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.6)
  .attr("d",d3.line().curve(d3.curveStepAfter).x(d=>x(d.th)).y(d=>y(d.o)));
svg.append("line").attr("x1",x(0.05)).attr("x2",x(0.05)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#999").attr("stroke-dasharray","3,3");
svg.append("text").attr("x",x(0.05)).attr("y",M.t-6).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#777").text("p = 0.05");
[[OBS,"#C41230",OBS+" observed",1],[+EXP,"#55606c",EXP+" expected",0]].forEach(a=>{
  svg.append("circle").attr("cx",x(0.05)).attr("cy",y(a[0])).attr("r",5)
    .attr("fill",a[1]);
  svg.append("text").attr("x",x(0.05)+9).attr("y",y(a[0])+(a[3]?4:-9))
    .attr("font-size","11.5px").attr("fill",a[1]).attr("font-weight",a[3]?"600":"400")
    .text(a[2]);});
const lg=svg.append("g").attr("transform",`translate(${W-M.r-232},${H-M.b-64})`);
[["#C41230","what happened",1,2.6],["#55606c","chance alone (mean)",1,2],
 ["#8899aa","chance alone (95%% of simulations)",0.35,8]].forEach((r,i)=>{
  lg.append("line").attr("x1",0).attr("x2",26).attr("y1",i*17).attr("y2",i*17)
    .attr("stroke",r[0]).attr("stroke-width",r[3]).attr("stroke-opacity",r[2])
    .attr("stroke-dasharray",i===1?"6,4":null);
  lg.append("text").attr("x",32).attr("y",i*17+4).attr("font-size","11.5px")
    .attr("fill","#444").text(r[1]);});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const th=x.invert(d3.pointer(ev,this)[0]);
    const d=D.reduce((a,b)=>Math.abs(b.th-th)<Math.abs(a.th-th)?b:a);
    tip.style("opacity",1).html(`<b>threshold ${d.th.toFixed(3)}</b><br>`+
      `observed: ${d.o} trials<br>chance alone: ${d.m.toFixed(1)} `+
      `(95%% of runs ${d.lo}-${d.hi})`)
      .style("left",Math.min(ev.offsetX+14,W-260)+"px").style("top",(M.t+4)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover for the count at any threshold.</p>
', rows, obs05, pc(exp05, 1), n(NSIM)))

## ---- flowers
o <- t[order(t$p), ][1:3, c("trial", "black_eligible", "black_struck",
                            "white_eligible", "white_struck", "p")]
o$p <- formatC(o$p, format = "e", digits = 1)
names(o) <- c("trial", "Black eligible", "Black struck", "white eligible",
              "white struck", "p")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
cat('<style>
#thr text[fill="#444" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
</style>')
