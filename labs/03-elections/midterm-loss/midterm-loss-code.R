# midterm-loss-code.R -- chunk bodies for midterm-loss-brief.Rmd
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
h <- read.csv("data/derived/house_midterms.csv", stringsAsFactors = FALSE)
h <- h[order(h$election_year), ]
h$pct <- 100 * h$pres_party_change / h$seats
h$pp_seats  <- ifelse(h$pres_party == "D", h$dem, h$rep)
h$maj       <- h$pp_seats > h$seats / 2
h$prev_maj  <- c(NA, head(h$maj, -1))

m <- h[h$midterm & !is.na(h$pres_party_change), ]
med <- median(m$pres_party_change)

# the same sample with Reconstruction and everything before it removed,
# for the one-sentence robustness check in the prose
a78 <- m[m$election_year >= 1878, ]

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)

# ---- quantities the figures draw ----
inner <- sum(abs(m$pres_party_change - med) <= 10)
q1 <- as.numeric(quantile(m$pres_party_change, .25))
q3 <- as.numeric(quantile(m$pres_party_change, .75))
held  <- sum(m$prev_maj, na.rm = TRUE)
lostm <- sum(m$prev_maj & !m$maj, na.rm = TRUE)
m$fate <- ifelse(is.na(m$prev_maj) | !m$prev_maj, "no majority to lose",
          ifelse(m$maj, "held it and kept it", "held it and lost it"))
fates <- c("no majority to lose", "held it and kept it", "held it and lost it")

# shared geometry for the majority-fate chart (identical in HTML and PDF)
WK <- 12
wf <- do.call(rbind, lapply(seq_along(fates), function(g) {
  yr <- sort(m$election_year[m$fate == fates[g]])
  data.frame(g = g, year = yr, cx = (seq_along(yr) - 1) %% WK,
             ry = (seq_along(yr) - 1) %/% WK)
}))
wfh    <- as.integer(tapply(wf$ry, wf$g, max)) + 1
wfbase <- cumsum(c(0, head(wfh + 0.8, -1)))
wf$y   <- wfbase[wf$g] + wf$ry
wfrows <- max(wf$y) + 1
wfn    <- as.integer(table(factor(m$fate, fates)))
# ---- one palette for this document ----------------------------------------
# Every signed quantity here is "the president's party lost seats / gained
# them", which is NOT a party: the president's party is Democratic in half
# these midterms and Republican in the other half. Red and blue would be read
# as party regardless of the legend, so losses and gains get a diverging pair
# that carries no partisan reading at all. Red is kept for one job only, the
# reference marks: the median, the cases singled out in text.
LOSSC <- "#b35806"   # the president's party lost seats, or lost the majority
GAINC <- "#542788"   # gained seats, or held the majority
NEUTC <- "#999999"   # no majority to lose; also the plain-count bars
REFC  <- "#C41230"   # a reference mark, never a group
wfcol  <- c(NEUTC, GAINC, LOSSC)

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

## ---- one-row
o <- h[h$election_year == 2018,
       c("election_year", "congress", "seats", "dem", "rep", "pres_party",
         "midterm", "pres_party_change")]
names(o) <- c("year", "congress", "seats in the chamber", "Democrats",
              "Republicans", "president's party", "midterm?",
              "seat change for the president's party")
o

## ---- footnote-repair
r18 <- h[h$election_year == 2018, ]
data.frame(
  quantity = c("What a reader sees in the cell",
               "After the tags are thrown away", "After the repair",
               "The largest the House has ever been"),
  value = c("435, then a raised 5", "4355", format(r18$seats),
            format(max(h$seats))))

## ---- reliability
data.frame(
  quantity = c("Midterm elections in the data", "President's party lost seats in",
               "As a percentage", "Mean change", "Median change"),
  value = c(nrow(m), sum(m$pres_party_change < 0),
            paste0(pc(100 * mean(m$pres_party_change < 0), 0), "%"),
            paste0(pc(mean(m$pres_party_change)), " seats"),
            paste0(med, " seats")))

## ---- exceptions
o <- m[m$pres_party_change >= 0,
       c("election_year", "pres_party", "pres_party_change", "seats")]
names(o) <- c("year", "president's party", "seat change", "chamber size")
o

## ---- hist-static
par(mar = c(4.2, 4.4, 1.0, 1.2))
cl <- ifelse(m$pres_party_change < 0, LOSSC, GAINC)
plot(m$election_year, m$pres_party_change, type = "h", lwd = 3, col = cl,
     xlab = "", ylab = "seat change, president's party", las = 1)
abline(h = 0, lwd = 1)
abline(h = med, lty = 3, col = "grey45")
points(m$election_year, m$pres_party_change, pch = 19, cex = 0.6, col = cl)
text(min(m$election_year), med, paste0("  median ", med), adj = c(0, -0.4),
     cex = 0.68, col = "#555555")
legend("bottomleft", c("the president's party lost seats",
                       "the president's party gained seats"),
       col = c(LOSSC, GAINC), lwd = 3, bty = "n", cex = 0.72)

## ---- hist-d3
rows <- paste(sprintf('{"y":%d,"s":%d,"p":%.2f,"n":%d,"pt":"%s"}',
                      m$election_year, m$pres_party_change, m$pct, m$seats,
                      m$pres_party), collapse = ",")
cat(sprintf('
<div id="mt" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:18,r:24,b:44,l:58};
const box=d3.select("#mt");
const bar=box.append("div").attr("style","margin-bottom:6px;font-size:12px");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1855,2028]).range([M.l,W-M.r]);
const ys=d3.scaleLinear().domain([-130,45]).range([H-M.b,M.t]);
const yp=d3.scaleLinear().domain([-38,20]).range([H-M.b,M.t]);
let mode="s";
const yax=svg.append("g").attr("transform",`translate(${M.l},0)`);
const ylab=svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(H-M.b+M.t)/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
const zero=svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("stroke","#666");
const g=svg.append("g");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
function val(d){ return mode==="s"?d.s:d.p; }
function sc(){ return mode==="s"?ys:yp; }
function draw(){
  const y=sc();
  yax.transition().duration(400).call(d3.axisLeft(y).ticks(7));
  ylab.text(mode==="s"?"seat change, president’s party"
                      :"seat change as %% of the chamber");
  zero.attr("y1",y(0)).attr("y2",y(0));
  g.selectAll("line.b").data(D).join("line").attr("class","b")
    .attr("stroke",d=>val(d)<0?"%s":"%s").attr("stroke-width",3)
    .attr("x1",d=>x(d.y)).attr("x2",d=>x(d.y))
    .transition().duration(400).attr("y1",y(0)).attr("y2",d=>y(val(d)));
  g.selectAll("circle").data(D).join("circle")
    .attr("cx",d=>x(d.y)).attr("r",3.4)
    .attr("fill",d=>val(d)<0?"%s":"%s")
    .on("mousemove",function(ev,d){
      tip.style("opacity",1).html(
        `<b>${d.y}</b> (president&#39;s party: ${d.pt})<br>`+
        `${d.s>0?"+":""}${d.s} seats of ${d.n}<br>`+
        `${d.p>0?"+":""}${d.p.toFixed(1)}%% of the chamber`)
        .style("left",Math.min(ev.offsetX+14,W-260)+"px").style("top",(ev.offsetY-10)+"px"); })
    .on("mouseleave",()=>tip.style("opacity",0))
    .transition().duration(400).attr("cy",d=>y(val(d)));
}
["seats","%% of the chamber"].forEach((lab,i)=>{
  bar.append("button").text(lab)
    .attr("style","margin-right:6px;padding:3px 9px;font:inherit;font-size:12px;cursor:pointer")
    .on("click",function(){ mode=i===0?"s":"p"; draw(); });
});
draw();
})();
</script>
', rows, LOSSC, GAINC, LOSSC, GAINC))

## ---- spread
inner <- sum(abs(m$pres_party_change - med) <= 10)
data.frame(
  statistic = c("Median loss", "Within 10 seats of the median",
                "Middle half of outcomes (25th to 75th percentile)",
                "Standard deviation", "Full range"),
  value = c(paste(med, "seats"),
            paste0(inner, " of ", nrow(m), " midterms (",
                   pc(100 * inner / nrow(m), 0), "%)"),
            paste(quantile(m$pres_party_change, .25), "to",
                  quantile(m$pres_party_change, .75), "seats"),
            paste(pc(sd(m$pres_party_change)), "seats"),
            sprintf("%d to +%d seats", min(m$pres_party_change),
                    max(m$pres_party_change))))

## ---- spread-hist-static
br <- seq(-130, 40, 10)
hh <- hist(m$pres_party_change, breaks = br, plot = FALSE)
ym <- max(hh$counts) * 1.32
par(mar = c(4.4, 4.2, 1.0, 1))
plot(hh, col = NA, border = NA, main = "", ylim = c(0, ym), xlim = c(-135, 45),
     xlab = "seat change for the president's party", ylab = "midterms")
rect(q1, 0, q3, ym, col = "#EDEDED", border = NA)
rect(med - 10, 0, med + 10, ym, col = "#F4D5DA", border = NA)
plot(hh, col = "#999999", border = "white", add = TRUE)
segments(med, 0, med, ym * 0.94, col = "#C41230", lwd = 2)
text(med, ym * 0.97, paste0("median ", med), col = "#C41230", cex = 0.8)
text(-133, ym * 0.80, paste0("shaded pink: within 10 seats of the median: ",
                             inner, " of ", nrow(m), " midterms"),
     col = "#C41230", cex = 0.72, adj = 0)
text(-133, ym * 0.68, paste0("shaded gray: the middle half, ", q1, " to ", q3,
                             " seats"), col = "#555555", cex = 0.72, adj = 0)
text(-133, ym * 0.56, paste0("full range: ", min(m$pres_party_change), " to +",
                             max(m$pres_party_change), " seats"),
     col = "#555555", cex = 0.72, adj = 0)

## ---- spread-hist-d3
br <- seq(-130, 40, 10)
hh <- hist(m$pres_party_change, breaks = br, plot = FALSE)
lab <- sapply(seq_along(hh$counts), function(i)
  paste(m$election_year[m$pres_party_change >  hh$breaks[i] &
                        m$pres_party_change <= hh$breaks[i + 1]], collapse = ", "))
rows <- paste(sprintf('{"x":%.1f,"lo":%d,"hi":%d,"c":%d,"y":"%s"}',
                      hh$mids, hh$breaks[-length(hh$breaks)], hh$breaks[-1],
                      hh$counts, lab), collapse = ",")
cat(paste0('
<div id="sh" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const B=[', rows, '];
const MED=', med, ', Q1=', q1, ', Q3=', q3, ', INNER=', inner, ', N=', nrow(m), ';
const W=760,H=400,M={t:20,r:24,b:46,l:52};
const box=d3.select("#sh");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-135,45]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(B,d=>d.c)*1.12]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(Q3)).attr("width",x(Q1)-x(Q3))
  .attr("y",M.t).attr("height",H-M.b-M.t).attr("fill","#EDEDED");
svg.append("rect").attr("x",x(MED-10)).attr("width",x(MED+10)-x(MED-10))
  .attr("y",M.t).attr("height",H-M.b-M.t).attr("fill","#C41230").attr("opacity",0.14);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(5));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("seat change for the president’s party");
svg.append("text").attr("x",M.l).attr("y",M.t-6).attr("font-size","11px")
  .attr("fill","#666").text("midterms");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;max-width:300px");
const w=(x(10)-x(0))-1.5;
svg.append("g").selectAll("rect").data(B).join("rect")
  .attr("x",d=>x(d.lo)+0.75).attr("width",w)
  .attr("y",d=>y(d.c)).attr("height",d=>y(0)-y(d.c)).attr("fill","#999999")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.lo} to ${d.hi} seats</b><br>${d.c} midterm${d.c===1?"":"s"}`+
      (d.y?`<br>${d.y}`:""))
      .style("left",Math.min(ev.offsetX+14,W-320)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("line").attr("x1",x(MED)).attr("x2",x(MED)).attr("y1",M.t+14).attr("y2",H-M.b)
  .attr("stroke","#C41230").attr("stroke-width",2);
svg.append("text").attr("x",x(MED)).attr("y",M.t+8).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230").text("median "+MED);
svg.append("text").attr("x",x(-133)).attr("y",M.t+34).attr("font-size","11px")
  .attr("fill","#C41230")
  .text("pink band: within 10 seats of the median — "+INNER+" of "+N+" midterms");
svg.append("text").attr("x",x(-133)).attr("y",M.t+50).attr("font-size","11px")
  .attr("fill","#555").text("gray band: the middle half, "+Q1+" to "+Q3+" seats");
})();
</script>
'))

## ---- majority
held  <- sum(m$prev_maj, na.rm = TRUE)
lostm <- sum(m$prev_maj & !m$maj, na.rm = TRUE)
data.frame(
  quantity = c("Midterms where the president's party held a House majority going in",
               "... of those, midterms where it lost the majority",
               "As a percentage"),
  value = c(held, lostm, paste0(pc(100 * lostm / held, 0), "%")))

## ---- fate-static
par(mar = c(1.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0, WK), ylim = c(wfrows, -0.75), axes = FALSE,
     xlab = "", ylab = "")
text(0, wfbase - 0.22, paste0(fates, ": ", wfn, " midterms"),
     adj = 0, cex = 0.8, font = 2, col = wfcol)
rect(wf$cx + 0.03, wf$y + 0.06, wf$cx + 0.97, wf$y + 0.94,
     col = wfcol[wf$g], border = NA)
text(wf$cx + 0.5, wf$y + 0.56, wf$year, col = "white", cex = 0.6)

## ---- fate-d3
w <- wf
w$ch <- m$pres_party_change[match(w$year, m$election_year)]
w$pt <- m$pres_party[match(w$year, m$election_year)]
cells <- paste(sprintf('{"g":%d,"yr":%d,"cx":%d,"cy":%.1f,"ch":%d,"pt":"%s"}',
                       w$g, w$year, w$cx, w$y, w$ch, w$pt), collapse = ",")
heads <- paste(sprintf('{"g":%d,"y":%.1f,"t":"%s — %d midterms"}',
                       seq_along(fates), wfbase, fates, wfn), collapse = ",")
cat(paste0('
<div id="wf" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[', cells, '], Hd=[', heads, '];
const K=', WK, ', ROWS=', wfrows, ', COL=["', paste(wfcol, collapse='","'), '"];
const W=760,M={l:8,r:16},cell=(W-M.l-M.r)/K,top=cell*0.55;
const H=top+ROWS*cell+4;
const box=d3.select("#wf");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
svg.append("g").selectAll("text").data(Hd).join("text")
  .attr("x",M.l).attr("y",d=>top+(d.y-0.22)*cell)
  .attr("font-size","13px").attr("font-weight","600")
  .attr("fill",d=>COL[d.g-1]).text(d=>d.t);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
g.selectAll("rect").data(C).join("rect")
  .attr("x",d=>M.l+d.cx*cell+1).attr("y",d=>top+d.cy*cell+1)
  .attr("width",cell-2).attr("height",cell-2)
  .attr("fill",d=>COL[d.g-1])
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.yr}</b> (president&#39;s party: ${d.pt})<br>`+
      `${d.ch>0?"+":""}${d.ch} seats`)
      .style("left",Math.min(ev.offsetX+14,W-230)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
g.selectAll("text").data(C).join("text")
  .attr("x",d=>M.l+d.cx*cell+cell/2).attr("y",d=>top+d.cy*cell+cell/2+4)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#fff")
  .attr("pointer-events","none").text(d=>d.yr);
})();
</script>
'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

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
#sh text[fill="#c41230" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
#wf text[fill="#fff" i],
#wf text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#wf text[fill="#fff" i],
#wf text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
