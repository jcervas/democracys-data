# jury-selection-code.R -- chunk bodies for jury-selection-brief.Rmd
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
s <- read.csv("data/derived/strikes.csv", stringsAsFactors = FALSE)
t <- read.csv("data/derived/trials.csv",  stringsAsFactors = FALSE)
t$eligible <- t$black_eligible + t$white_eligible
t$struck   <- t$black_struck   + t$white_struck
t$p <- phyper(t$black_struck - 1, t$black_eligible, t$white_eligible,
              t$struck, lower.tail = FALSE)
g  <- function(sd, rc, v) s[[v]][s$side == sd & s$race == rc]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# ---- the trial the chapter opens on -----------------------------------------
# Row 1 of the file, and the only trial named in the opening pages. Everything
# said about it below is read out of these five numbers, so the paragraph and
# the urn figure further down cannot drift apart.
o1 <- t[1, ]
NB <- o1$black_eligible; NW <- o1$white_eligible
KS <- o1$struck;         SB <- o1$black_struck
PV <- o1$p
ONE_IN <- round(1 / PV)                     # "about one chance in twenty-three"
kk <- 0:min(NB, KS); pm <- dhyper(kk, NB, NW, KS)   # the whole null distribution
BLK <- "#C41230"; WHT <- "#cfd6dc"; GRY <- "#666666"

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

# one y limit for the mirror chart, used by BOTH renderers so they cannot drift
MIRY <- 66

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

## ---- one-trial
o <- t[1, c("trial", "black_eligible", "black_struck", "white_eligible", "white_struck")]
names(o) <- c("trial", "Black jurors eligible", "struck by the state",
              "white jurors eligible", "struck by the state")
o

## ---- clean-jury
o <- s[, c("side", "race", "eligible", "struck", "pct")]
o$eligible <- n(o$eligible); o$struck <- n(o$struck)
names(o) <- c("side", "race", "could be struck", "struck", "%")
o

## ---- counts
data.frame(
  quantity = c("Trials in this file", "Prospective jurors eligible to be struck",
               "Struck by the state", "Prosecutor's office", "Period"),
  value = c(nrow(t), n(sum(t$eligible)), n(sum(t$struck)),
            "Mississippi's Fifth Circuit District", "26 years"))

## ---- state-rates
o <- s[s$side == "state", c("race", "eligible", "struck", "pct")]
o$eligible <- n(o$eligible); o$struck <- n(o$struck)
names(o) <- c("race", "eligible to be struck", "struck by the state", "% struck")
o

## ---- both-sides
o <- s[, c("side", "race", "eligible", "struck", "pct")]
o$eligible <- n(o$eligible); o$struck <- n(o$struck)
names(o) <- c("striking side", "juror's race", "eligible", "struck", "% struck")
o

## ---- mirror-static
m <- matrix(c(g("state","Black","pct"), g("state","White","pct"),
              g("defense","Black","pct"), g("defense","White","pct")), nrow = 2)
par(mar = c(3.4, 4.6, 2.6, 1.2))
bp <- barplot(m, beside = TRUE, names.arg = c("state strikes", "defense strikes"),
              col = c("#C41230", "#2c7fb8"), ylim = c(0, MIRY), las = 1,
              ylab = "% of eligible jurors struck")
# value labels, so the PDF carries the same numbers the HTML version prints
text(as.vector(bp), as.vector(m) + MIRY * 0.028, paste0(pc(as.vector(m)), "%"),
     cex = 0.78, font = 2, col = rep(c("#C41230", "#2c7fb8"), 2))
# legend above the bars: at "topright" it lands on the 47.3% defense bar
legend(x = mean(bp), y = MIRY * 1.10, xjust = 0.5, horiz = TRUE,
       legend = c("Black jurors", "white jurors"),
       fill = c("#C41230", "#2c7fb8"), bty = "n", cex = 0.88, xpd = NA)

## ---- d3-mirror
# ---------------------------------------------------------------------------
# The four rates in strikes.csv, drawn once. Both renderers read the same four
# numbers and share the same y limit (MIRY, set in setup), so neither can pick
# a scale the other does not use.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
cat(sprintf('
<div id="mir" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[{s:"state strikes",b:%.1f,w:%.1f},{s:"defense strikes",b:%.1f,w:%.1f}];
const W=720,H=340,M={t:20,r:20,b:44,l:56};
const svg=d3.select("#mir").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x0=d3.scaleBand().domain(D.map(d=>d.s)).range([M.l,W-M.r]).padding(0.3);
const x1=d3.scaleBand().domain(["b","w"]).range([0,x0.bandwidth()]).padding(0.12);
const y=d3.scaleLinear().domain([0,%d]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x0).tickSize(0))
  .selectAll("text").attr("font-size","13px");
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% of eligible jurors struck");
const col={b:"#C41230",w:"#2c7fb8"}, nm={b:"Black jurors",w:"white jurors"};
D.forEach(d=>{
  ["b","w"].forEach(k=>{
    svg.append("rect").attr("x",x0(d.s)+x1(k)).attr("width",x1.bandwidth())
      .attr("y",y(0)).attr("height",0).attr("fill",col[k]).attr("rx",2)
      .transition().duration(700).attr("y",y(d[k])).attr("height",y(0)-y(d[k]));
    svg.append("text").attr("x",x0(d.s)+x1(k)+x1.bandwidth()/2).attr("y",y(d[k])-6)
      .attr("text-anchor","middle").attr("font-size","12.5px").attr("font-weight","600")
      .attr("fill",col[k]).attr("opacity",0).text(d[k]+"%%")
      .transition().delay(700).duration(300).attr("opacity",1);
  });
});
const lg=svg.append("g").attr("transform",`translate(${M.l+8},${M.t})`);
["b","w"].forEach((k,i)=>{
  lg.append("rect").attr("y",i*17).attr("width",11).attr("height",11).attr("fill",col[k]);
  lg.append("text").attr("x",16).attr("y",i*17+10).attr("font-size","12px").text(nm[k]);
});
})();
</script>
', g("state","Black","pct"), g("state","White","pct"),
   g("defense","Black","pct"), g("defense","White","pct"), MIRY))

## ---- urn-d3
bars <- paste(sprintf('{"k":%d,"p":%.5f}', kk, pm), collapse = ",")
cat(sprintf('
<div id="urn" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const NB=%d,NW=%d,KS=%d,SB=%d,PV=%f,B=[%s];
const BLK="#C41230",WHT="#cfd6dc";
const W=760,H=380;
const svg=d3.select("#urn").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const T=(x,y,s,o)=>{const t=svg.append("text").attr("x",x).attr("y",y)
  .attr("text-anchor",(o&&o.a)||"start").attr("font-size",(o&&o.s)||"12px")
  .attr("fill",(o&&o.c)||"#333");if(o&&o.b)t.attr("font-weight","600");return t.text(s);};
const dot=(x,y,black)=>svg.append("circle").attr("cx",x).attr("cy",y).attr("r",11)
  .attr("fill",black?BLK:WHT).attr("stroke","#333");
T(8,18,`the venire: ${NB+NW} eligible jurors`,{b:1,s:"12.5px"});
for(let i=0;i<NB+NW;i++) dot(20+(i%%5)*30, 44+Math.floor(i/5)*30, i<NB);
T(8,168,`${NB} Black, ${NW} white`,{c:"#555",s:"11px"});
svg.append("defs").append("marker").attr("id","um").attr("viewBox","0 0 10 10")
  .attr("refX",9).attr("refY",5).attr("markerWidth",6).attr("markerHeight",6)
  .attr("orient","auto").append("path").attr("d","M0,0L10,5L0,10Z").attr("fill","#666");
svg.append("line").attr("x1",178).attr("y1",74).attr("x2",228).attr("y2",74)
  .attr("stroke","#666").attr("stroke-width",2).attr("marker-end","url(#um)");
T(203,58,`${KS} peremptory`,{a:"middle",c:"#666",b:1,s:"11px"});
T(203,94,"strikes",{a:"middle",c:"#666",b:1,s:"11px"});
T(248,18,"what the prosecution actually struck",{b:1,s:"12.5px"});
for(let i=0;i<KS;i++) dot(262+i*30, 44, i<SB);
T(250,80,`all ${SB} of the ${NB} Black jurors,`,{c:BLK,b:1,s:"11px"});
T(250,98,`and ${KS-SB} of the ${NW} white ones`,{c:"#555",s:"11px"});
T(248,140,`if the ${KS} strikes had been drawn at random`,{b:1,s:"12.5px"});
const X0=270,X1=690,Y0=330,BH=150,mx=d3.max(B,d=>d.p);
const bw=(X1-X0)/B.length*0.68, gap=(X1-X0)/B.length;
B.forEach((d,i)=>{const x=X0+i*gap,h=d.p/mx*BH;
  svg.append("rect").attr("x",x).attr("y",Y0-h).attr("width",bw).attr("height",h)
    .attr("fill",d.k>=SB?BLK:"#2c7fb8").attr("fill-opacity",d.k>=SB?1:0.55);
  T(x+bw/2,Y0-h-6,d3.format(".1%%")(d.p),{a:"middle",s:"11px",b:d.k>=SB?1:0,
    c:d.k>=SB?BLK:"#446"});
  T(x+bw/2,Y0+16,d.k,{a:"middle",b:1,s:"12px"});});
svg.append("line").attr("x1",X0-6).attr("y1",Y0).attr("x2",X0+(B.length-1)*gap+bw+6)
  .attr("y2",Y0).attr("stroke","#333");
T(X0+((B.length-1)*gap+bw)/2,Y0+36,"number of Black jurors among the strikes",
  {a:"middle",c:"#555",s:"11px"});
const xl=X0+(B.length-1)*gap+bw+16;
T(xl,Y0-30,"what happened,",{c:BLK,b:1,s:"11px"});
T(xl,Y0-14,`in ${d3.format(".1%%")(PV)} of draws`,{c:BLK,b:1,s:"11px"});
})();
</script>
', NB, NW, KS, SB, PV, bars))

## ---- urn-static
# The same venire, the same strikes, the same null distribution: base R for the
# PDF device, D3 above for the browser. Both read NB, NW, KS, SB and pm, all
# computed once in setup from row 1 of trials.csv.
par(mar = rep(0.2, 4))
plot(NA, xlim = c(0, 100), ylim = c(-6, 50), asp = NA, axes = FALSE, ann = FALSE)
r <- 1.55
text(1, 47, sprintf("the venire: %d eligible jurors", NB + NW), cex = 0.7, font = 2,
     col = "#333", adj = 0)
for (i in seq_len(NB + NW))
  symbols(2.6 + ((i-1) %% 5) * 4.3, 39 - ((i-1) %/% 5) * 5.0, circles = r,
          inches = FALSE, add = TRUE, bg = if (i <= NB) BLK else WHT, fg = "#333")
text(2.6, 22.5, sprintf("%d Black, %d white", NB, NW), cex = 0.6, col = "#555", adj = 0)

arrows(24, 32, 33, 32, length = 0.08, lwd = 2, col = GRY)
text(28.5, 35.4, sprintf("%d peremptory", KS), cex = 0.6, font = 2, col = GRY)
text(28.5, 29.0, "strikes", cex = 0.6, font = 2, col = GRY)

text(35, 47, "what the prosecution actually struck", cex = 0.7, font = 2,
     col = "#333", adj = 0)
for (i in seq_len(KS))
  symbols(37 + (i-1) * 4.3, 39, circles = r, inches = FALSE, add = TRUE,
          bg = if (i <= SB) BLK else WHT, fg = "#333")
text(37, 33.6, sprintf("all %d of the %d Black jurors,", SB, NB), cex = 0.6,
     col = BLK, adj = 0, font = 2)
text(37, 30.6, sprintf("and %d of the %d white ones", KS - SB, NW), cex = 0.6,
     col = "#555", adj = 0)

BX <- 37; BY <- 4; BW <- 50
text(BX, 26.6, sprintf("if the %d strikes had been drawn at random", KS), cex = 0.7,
     font = 2, col = "#333", adj = 0)
gap <- BW / length(kk); bw <- gap * 0.68; BH <- 17
for (i in seq_along(kk)) {
  x <- BX + (i-1) * gap; h <- pm[i] / max(pm) * BH; hit <- kk[i] >= SB
  rect(x, BY, x + bw, BY + h, col = if (hit) BLK else adjustcolor("#2c7fb8", 0.55),
       border = NA)
  text(x + bw/2, BY + h + 1.4, sprintf("%.1f%%", 100*pm[i]), cex = 0.54,
       col = if (hit) BLK else "#446", font = if (hit) 2 else 1)
  text(x + bw/2, BY - 2.2, kk[i], cex = 0.6, font = 2)
}
segments(BX - 1, BY, BX + (length(kk)-1) * gap + bw + 1, BY, col = "#333")
text(BX + ((length(kk)-1) * gap + bw)/2, BY - 4.8,
     "number of Black jurors among the strikes", cex = 0.56, col = "#555")
text(BX + (length(kk)-1) * gap + bw + 3, BY + 5.2,
     sprintf("what happened,\nin %s%% of draws", formatC(100*PV, format = "f", digits = 1)),
     cex = 0.56, col = BLK, font = 2, adj = 0)

## ---- one-trial-p
data.frame(
  quantity = c("Eligible jurors", "Black among them", "Strikes used",
               "Black jurors struck", "Chance of a split this lopsided or worse"),
  value = c(o1$eligible, o1$black_eligible, o1$struck, o1$black_struck,
            paste0(pc(100 * PV, 1), "%")))

## ---- many-trials-naive
data.frame(
  quantity = c("Trials examined",
               "Trials where the split is significant at p < 0.05",
               "Expected by chance, on the textbook shortcut of 0.05 x 211"),
  value = c(nrow(t), obs05, pc(naive05, 1)))

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

## ---- scatter-static
# Same square domain as the HTML version, so the y = x diagonal -- the line that
# means "every eligible Black juror struck" -- sits at the same 45 degrees in
# both. Auto axes here would tilt it and make the two versions disagree.
JMX <- max(t$black_eligible) + 1
# the HTML version's index-based jitter, reproduced exactly so the two renderers
# place the same trial in the same spot, and so the PDF does not move each knit
jit <- function(v) v + (((seq_along(v) - 1) * 2654435761) %% 100 / 100 - 0.5) * 0.34
par(mar = c(4.3, 4.4, 0.8, 1.2))
plot(jit(t$black_eligible), jit(t$black_struck), pch = 19,
     xlim = c(0, JMX), ylim = c(0, JMX), las = 1, xaxs = "i", yaxs = "i",
     col = ifelse(t$p < 0.01, "#C41230", ifelse(t$p < 0.05, "#e08214", "#88a")),
     xlab = "Black jurors eligible to be struck", ylab = "Black jurors struck",
     cex = 0.9)
abline(0, 1, lty = 2, col = "grey40")
text(JMX, JMX - 0.5, "every eligible Black juror struck", adj = c(1, 1),
     cex = 0.72, col = "#777")
legend("topleft", c("p < 0.01", "p < 0.05", "not significant"),
       col = c("#C41230", "#e08214", "#88a"), pch = 19, bty = "n", cex = 0.85)

## ---- d3-scatter
rows <- paste(sprintf('{"t":"%s","be":%d,"bs":%d,"we":%d,"ws":%d,"p":%.3g}',
                      gsub('"', "", t$trial), t$black_eligible, t$black_struck,
                      t$white_eligible, t$white_struck, t$p), collapse = ",")
cat(sprintf('
<div id="jur" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=440,M={t:20,r:24,b:48,l:56};
const svg=d3.select("#jur").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const mx=d3.max(D,d=>d.be)+1;
const x=d3.scaleLinear().domain([0,mx]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,mx]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(8));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("Black jurors eligible to be struck");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("Black jurors struck");
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(mx)).attr("y2",y(mx))
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(mx)-6).attr("y",y(mx)+16).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#777").text("every eligible Black juror struck");
const col=d=>d.p<0.01?"#C41230":(d.p<0.05?"#e08214":"#8899aa");
const jit=(v,i)=>v+((i*2654435761%%100)/100-0.5)*0.34;
const tip=d3.select("#jur").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",(d,i)=>x(jit(d.be,i))).attr("cy",(d,i)=>y(jit(d.bs,i+7)))
  .attr("r",4.6).attr("fill",col).attr("fill-opacity",0.72)
  .on("mousemove",function(e,d){
    d3.select(this).attr("r",7);
    tip.style("opacity",1).html(
      `<b>${d.t}</b><br>Black: ${d.bs} struck of ${d.be}<br>`+
      `white: ${d.ws} struck of ${d.we}<br>p = ${d.p<0.001?d.p.toExponential(1):d.p.toFixed(3)}`)
      .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-10)+"px");
  })
  .on("mouseleave",function(){d3.select(this).attr("r",4.6);tip.style("opacity",0);});
const lg=svg.append("g").attr("transform",`translate(${M.l+10},${M.t})`);
[["#C41230","p < 0.01"],["#e08214","p < 0.05"],["#8899aa","not significant"]]
 .forEach((r,i)=>{lg.append("circle").attr("cy",i*17).attr("r",5).attr("fill",r[0]);
   lg.append("text").attr("x",11).attr("y",i*17+4).attr("font-size","12px").text(r[1]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover for the trial.</p>
', rows))

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
