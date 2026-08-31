# migration-code.R -- chunk bodies for migration-brief.Rmd
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

# Everything in this document is read from CSVs written by data/build-data.R,
# which parses five Census files. Nothing here re-derives a number from a
# spreadsheet and nothing here is typed in by hand.
D  <- "data"
fl <- read.csv(file.path(D, "derived/flows.csv"),        stringsAsFactors = FALSE)
st <- read.csv(file.path(D, "derived/states.csv"),       stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
ar <- read.csv(file.path(D, "derived/arcs.csv"),         stringsAsFactors = FALSE)
mp <- read.csv(file.path(D, "derived/map_states.csv"),   stringsAsFactors = FALSE,
               colClasses = c(fips = "character", pts = "character"))
ins <- read.csv(file.path(D, "derived/map_insets.csv"),  stringsAsFactors = FALSE)
mo <- read.csv(file.path(D, "derived/mobility.csv"),     stringsAsFactors = FALSE)
cy <- read.csv(file.path(D, "derived/county_focus.csv"), stringsAsFactors = FALSE)
mm <- read.csv(file.path(D, "derived/meta.csv"),         stringsAsFactors = FALSE)

mv <- function(k) mm$value[mm$key == k]
mn <- function(k) as.numeric(mv(k))
n  <- function(x) format(round(x), big.mark = ",")
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)

FOCUS <- mv("focus_state"); CONTRAST <- mv("contrast_state")
CTY   <- mv("focus_county")
S     <- st[st$is_state, ]                    # 50 states + DC; PR flagged out
F1    <- st[st$state == FOCUS, ]
C1    <- st[st$state == CONTRAST, ]
NOR   <- nrow(st) - 1     # origins available to any one state: everywhere else

# Whether an estimate clears its own 90 percent margin of error is the question
# this brief turns on, so the helper that prints an estimate together with its
# margin is defined once here and used everywhere below.
pm <- function(e, m) sprintf("%s ± %s", n(e), n(m))

# ---- palette -------------------------------------------------------------
RED <- "#C41230"; BLU <- "#2c7fb8"; GRY <- "#999999"
INK <- "#12161c"; LAND <- "#1e2732"; EDGE <- "#2f3d4d"
ARC <- "#5fb0e5"; KEYTX <- "#9fb3c8"

# ---- how an arc is drawn -------------------------------------------------
# WIDTH carries the number of people and nothing else carries it. The scale is
# ABSOLUTE -- WLWD line units at WREF people, in every panel and in the D3
# version -- so the same thickness always means the same number of movers and
# the key in the corner of each map can be read off. The transform is a square
# root: these flows span roughly 300 to 1, and drawn linearly the small arcs
# vanish while the big ones become blobs.
WREF <- 50000     # a flow of this many people ...
WLWD <- 5.0       # ... is drawn this thick
WMIN <- 0.40      # and nothing is drawn thinner than this, or it disappears
awid <- function(e) pmax(WMIN, WLWD * sqrt(e / WREF))
WKEY <- c(1000, 10000, 50000)   # the reference arcs drawn in the corner key

# COLOR is not a second copy of the width. It carries an entirely different
# fact: whether the estimate clears its own margin of error, i.e. whether the
# survey can tell this flow apart from no one at all.

MW <- mn("frame_w"); MH <- mn("frame_h")
# "x0 y0 dx dy ..." -> absolute coordinates
mdec <- function(p) {
  v <- as.integer(strsplit(p, " ", fixed = TRUE)[[1]])
  list(x = cumsum(v[c(TRUE, FALSE)]), y = cumsum(v[c(FALSE, TRUE)]))
}
# a quadratic Bezier, sampled. Both renderers use the same control point, so the
# PDF and the HTML draw the same curve.
bez <- function(a, t = seq(0, 1, length.out = 40))
  list(x = (1-t)^2 * a$x0 + 2*(1-t)*t * a$cx + t^2 * a$x1,
       y = (1-t)^2 * a$y0 + 2*(1-t)*t * a$cy + t^2 * a$y1)

# the corner key: three reference arcs at round numbers of people, drawn with
# exactly the function that draws the map, so it cannot drift out of date
arckey <- function(cexs = 1) {
  cx <- 0.50 * cexs
  x2 <- MW - 22; x1 <- x2 - 78; xt <- x1 - 12
  yy <- c(34, 84, 142)
  text(x2, 186, "migrants per arc", col = KEYTX, cex = cx, adj = 1)
  for (i in seq_along(WKEY)) {
    lines(c(x1, x2), rep(yy[i], 2), col = adjustcolor(ARC, 0.85),
          lwd = awid(WKEY[i]))
    text(xt, yy[i], format(WKEY[i], big.mark = ","), col = KEYTX,
         cex = cx, adj = 1)
  }
}

# one arc panel, base R -- used for the PDF build of the arc figure
arcpanel <- function(hub, dir, title, grey_insig = FALSE, cexs = 1) {
  d <- ar[ar$hub == hub & ar$dir == dir, ]
  d <- d[order(d$est), ]
  par(bg = INK, mar = c(0.2, 0.2, 1.4, 0.2))
  plot(NA, xlim = c(0, MW), ylim = c(0, MH), asp = 1, axes = FALSE, ann = FALSE)
  for (i in seq_len(nrow(mp))) {
    z <- mdec(mp$pts[i]); polygon(z$x, z$y, col = LAND, border = EDGE, lwd = 0.4)
  }
  rect(ins$x0, ins$y0, ins$x1, ins$y1, border = EDGE, lwd = 0.5)
  text(ins$x0, ins$y1 + 9, ins$label, col = "#5d7288", cex = 0.42 * cexs, adj = 0)
  if (nrow(d)) {
    lw <- awid(d$est)
    for (i in seq_len(nrow(d))) {
      b <- bez(d[i, ])
      if (!d$sig[i] && grey_insig)          # asking how much survives: hairlines
        lines(b$x, b$y, col = adjustcolor(GRY, 0.55), lwd = 0.7, lty = 3)
      else if (!d$sig[i])                   # elsewhere: true width, gray
        lines(b$x, b$y, col = adjustcolor(GRY, 0.70), lwd = lw[i])
      else
        lines(b$x, b$y, col = adjustcolor(ARC, 0.85), lwd = lw[i])
    }
  }
  arckey(cexs)
  points(st$mx, st$my, pch = 19, cex = 0.22 * cexs, col = "#7f93a8")
  h <- st[st$state == hub, ]
  points(h$mx, h$my, pch = 21, cex = 0.75 * cexs, col = "#ffffff", bg = RED, lwd = 0.8)
  mtext(title, side = 3, line = 0.1, col = "#dbe4ee", cex = 0.68 * cexs, adj = 0.02)
}

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- clean-mig
o <- head(fl[fl$to_state == "Alabama", c("from_state", "to_state", "est",
                                         "moe", "lo", "hi", "sig")], 3)
names(o) <- c("from", "to", "estimate", "margin", "low", "high",
              "differs from zero")
o

## ---- shape-mig
data.frame(
  stage = c("Ordered pairs of states", "Pairs carrying an estimate",
            "Pairs suppressed or blank"),
  rows = c(mv("n_pairs"), mv("n_reported"), mv("n_suppressed")))

## ---- what-it-is
data.frame(
  quantity = c("Moved within their own state", "Moved to a different state",
               "Moved to the U.S. from abroad", "Population 1 year and over"),
  people = c(n(mn("us_movers_within_state")), n(mn("us_movers_between_states")),
             n(mn("us_movers_from_abroad")), n(mn("us_pop1"))))

## ---- focus-row
data.frame(
  quantity = c(sprintf("Moved INTO %s from another state", FOCUS),
               sprintf("Moved OUT of %s to another state", FOCUS),
               "Net (in minus out)",
               sprintf("Moved into %s from abroad", FOCUS)),
  estimate = c(pm(F1$in_est, F1$in_moe), pm(F1$out_est, F1$out_moe),
               pm(F1$net, F1$net_moe), pm(F1$abroad_est, F1$abroad_moe)))

## ---- gainers
G <- head(S[order(-S$net), ], 2)          # the two biggest net gainers

## ---- arcwidth-facts
aw    <- ar[ar$hub == FOCUS & ar$dir == "in", ]
ao    <- ar[ar$hub == FOCUS & ar$dir == "out", ]
w_est <- max(aw$est) / min(aw$est)                    # spread of the flows
w_lwd <- max(awid(aw$est)) / min(awid(aw$est))        # spread of the widths
# the heaviest exchange in the country, named and measured once so the caption
# and the paragraph beneath it cannot disagree
PART  <- ao$other[which.max(ao$est)]
OUTMX <- max(ao$est); INMX <- max(aw$est)

## ---- arcmap-d3
mk <- function(hub) {
  d <- ar[ar$hub == hub, ]
  paste(sprintf('{"d":"%s","o":"%s","e":%d,"m":%d,"s":%d,"p":"M%.1f,%.1fQ%.1f,%.1f %.1f,%.1f"}',
        d$dir, gsub('"', "", d$other), d$est, d$moe, as.integer(d$sig),
        d$x0, d$y0, d$cx, d$cy, d$x1, d$y1), collapse = ",")
}
paths <- paste(sprintf('"%s"', mp$pts), collapse = ",")
inbx  <- paste(sprintf('{"l":"%s","x":%d,"y":%d,"w":%d,"h":%d}', ins$label,
                       ins$x0, ins$y0, ins$x1 - ins$x0, ins$y1 - ins$y0),
               collapse = ",")
hubxy <- paste(sprintf('{"n":"%s","x":%.1f,"y":%.1f}',
                       FOCUS, F1$mx, F1$my),
               collapse = ",")
cat(sprintf('
<div id="arcwrap" style="margin:1em 0">
 <div style="margin:0 0 .5em 0;font:13px/1.4 inherit">
  <b>Direction:</b>
  <label><input type="radio" name="dir" value="in" checked> arriving in %s</label>
  <label style="margin-left:.8em"><input type="radio" name="dir" value="out"> leaving %s</label>
  <label style="margin-left:1.6em"><input type="checkbox" id="onlysig">
   hide flows not distinguishable from zero</label>
 </div>
 <div id="arc" style="position:relative"></div>
 <p id="arccap" style="font-size:0.85em;color:#666;margin:.35em 0 0 0"></p>
</div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const A={"%s":[%s]};
const PATHS=[%s], BX=[%s], HUB=[%s];
const W=%d,H=%d;
const svg=d3.select("#arc").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;display:block;background:%s");
const flip=s=>s.replace(/(-?[\\d.]+),(-?[\\d.]+)/g,(m,a,b)=>a+","+(H-(+b)));
svg.append("g").selectAll("path").data(PATHS).join("path")
  .attr("d",d=>{let v=d.split(" "),x=0,y=0,s="";
    for(let i=0;i<v.length;i+=2){x+=+v[i];y+=+v[i+1];s+=(i?"L":"M")+x+","+(H-y);}
    return s+"Z";})
  .attr("fill","%s").attr("stroke","%s").attr("stroke-width",0.5);
BX.forEach(b=>{
  svg.append("rect").attr("x",b.x).attr("y",H-b.y-b.h).attr("width",b.w)
    .attr("height",b.h).attr("fill","none").attr("stroke","%s").attr("stroke-width",0.7);
  svg.append("text").attr("x",b.x).attr("y",H-b.y-b.h-4).attr("font-size","10px")
    .attr("fill","#5d7288").text(b.l);
});
// the same width scale the static build uses: WLWD units at WREF people, on a
// square root, floored at WMIN. SW converts an R line width to a stroke width
// in this 1000-unit frame, so the two renderings land at the same thickness on
// screen; every other constant is shared.
const ARC="%s", DIM="%s", KEYTX="%s";
const WREF=%d, WLWD=%.2f, WMIN=%.2f, SW=1.35, WKEY=[%s];
const wid=e=>Math.max(WMIN,WLWD*Math.sqrt(e/WREF))*SW;
const key=svg.append("g");
const kx2=W-22, kx1=kx2-78;
key.append("text").attr("x",kx2).attr("y",H-186).attr("text-anchor","end")
  .attr("font-size","13px").attr("fill",KEYTX).text("migrants per arc");
WKEY.forEach((v,i)=>{
  const ky=H-[34,84,142][i];
  key.append("line").attr("x1",kx1).attr("x2",kx2).attr("y1",ky).attr("y2",ky)
    .attr("stroke",ARC).attr("stroke-opacity",0.85).attr("stroke-width",wid(v));
  key.append("text").attr("x",kx1-12).attr("y",ky+4.5).attr("text-anchor","end")
    .attr("font-size","13px").attr("fill",KEYTX).text(d3.format(",")(v));
});
const g=svg.append("g");
const hubg=svg.append("g");
const tip=d3.select("#arc").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#000;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const cap=d3.select("#arccap");
let HUBN="%s";
function draw(){
  const dir=d3.select(\'input[name="dir"]:checked\').property("value");
  const only=d3.select("#onlysig").property("checked");
  let d=A[HUBN].filter(a=>a.d===dir);
  const total=d3.sum(d,a=>a.e), nAll=d.length;
  if(only) d=d.filter(a=>a.s===1);
  d=d.slice().sort((a,b)=>a.e-b.e);
  g.selectAll("path").data(d,a=>a.o+a.d).join("path")
    .attr("d",a=>flip(a.p)).attr("fill","none")
    .attr("stroke",a=>a.s?ARC:DIM)
    .attr("stroke-opacity",a=>a.s?0.85:0.7)
    .attr("stroke-dasharray",a=>a.s?null:"4,3")
    .attr("stroke-width",a=>wid(a.e))
    .on("mousemove",function(e,a){
      const w=d3.select("#arc").node().clientWidth;
      tip.style("opacity",1).html(
        `<b>${a.o}</b><br>${d3.format(",")(a.e)} &plusmn; ${d3.format(",")(a.m)}`+
        (a.s?"":`<br><span style="color:#ffb0bd">not distinguishable from zero</span>`))
       .style("left",Math.min(e.offsetX+14,w-190)+"px").style("top",(e.offsetY-8)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0));
  const shown=d3.sum(d,a=>a.e);
  cap.html(`<b>${HUBN}</b> &mdash; ${dir==="in"?"arrivals from":"departures to"} `+
    `each other state. Showing ${d.length} of ${nAll} arcs, `+
    `${d3.format(".1%%")(shown/total)} of the people. Hover any arc.`);
}
function hubdots(){
  const h=HUB.find(z=>z.n===HUBN);
  hubg.selectAll("circle").data([h]).join("circle")
    .attr("cx",z=>z.x).attr("cy",z=>H-z.y).attr("r",4.5)
    .attr("fill","%s").attr("stroke","#fff").attr("stroke-width",1.2);
}
d3.selectAll(\'input[name="dir"]\').on("change",draw);
d3.select("#onlysig").on("change",draw);
hubdots(); draw();
})();
</script>
', FOCUS, FOCUS, FOCUS, mk(FOCUS), paths, inbx, hubxy,
   MW, MH, INK, LAND, EDGE, EDGE,
   ARC, GRY, KEYTX, WREF, WLWD, WMIN, paste(WKEY, collapse = ","),
   FOCUS, RED))

## ---- arcmap-static
par(mfrow = c(1, 2))
arcpanel(FOCUS, "in",  sprintf("Arriving in %s", FOCUS), cexs = 1.5)
arcpanel(FOCUS, "out", sprintf("Leaving %s", FOCUS),     cexs = 1.5)

## ---- moe-accounting
data.frame(
  outcome = c("Ordered pairs of states in the table",
              "Pairs the ACS will not report at all (flagged N)",
              "Pairs reported as exactly zero",
              "Pairs whose estimate is smaller than its own margin of error",
              "Pairs that survive: a flow distinguishable from zero",
              "Share of ALL interstate movers in the pairs that fail (%)"),
  count = c(n(mn("n_pairs")), n(mn("n_suppressed")), n(mn("n_zero_est")),
            n(mn("n_fail") - mn("n_suppressed") - mn("n_zero_est")),
            n(mn("n_sig")), pc(mn("pct_volume_in_failed_pairs"), 2)))

## ---- cater-setup
ex <- fl[!is.na(fl$est), ]
ex <- ex[order(-ex$est), ]
sel <- rbind(head(ex, 4),
             ex[!ex$sig, ][order(-ex$est[!ex$sig]), ][1:8, ])
sel$lab <- paste(abbreviate(sel$from_state, 12), "->", abbreviate(sel$to_state, 12))
sel <- sel[order(sel$est), ]

## ---- cater-static
par(mar = c(3.6, 9.4, 1.0, 1.4))
yy <- seq_len(nrow(sel))
plot(NA, xlim = c(min(0, min(sel$est - sel$moe)), max(sel$est + sel$moe) * 1.04),
     ylim = c(0.4, nrow(sel) + 0.6), axes = FALSE, ann = FALSE)
abline(v = 0, col = "#333", lwd = 1.2)
cl <- ifelse(sel$sig, BLU, RED)
segments(sel$est - sel$moe, yy, sel$est + sel$moe, yy, col = cl, lwd = 3)
points(sel$est, yy, pch = 19, cex = 0.8, col = cl)
axis(1, cex.axis = 0.68)
axis(2, at = yy, labels = sel$lab, las = 1, cex.axis = 0.6, tick = FALSE, line = -0.5)
mtext("people who moved, with the published 90% interval", side = 1, line = 2.3,
      cex = 0.72)
legend("bottomright", bty = "n", cex = 0.62, lwd = 3, col = c(BLU, RED),
       legend = c("interval clears zero", "interval touches zero"))

## ---- cater-d3
rows <- paste(sprintf('{"l":"%s","e":%d,"m":%d,"s":%d}',
        gsub('"', "", paste(sel$from_state, "→", sel$to_state)),
        sel$est, sel$moe, as.integer(sel$sig)), collapse = ",")
cat(sprintf('
<div id="cat" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=330,M={t:14,r:26,b:44,l:190};
const svg=d3.select("#cat").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([Math.min(0,d3.min(D,d=>d.e-d.m)),
  d3.max(D,d=>d.e+d.m)*1.03]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.l)).range([H-M.b,M.t]).padding(0.3);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0))
  .call(g=>g.select(".domain").remove()).selectAll("text").attr("font-size","10px");
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#333").attr("stroke-width",1.2);
const c=d=>d.s?"%s":"%s";
svg.append("g").selectAll("line.e").data(D).join("line")
  .attr("x1",d=>x(d.e-d.m)).attr("x2",d=>x(d.e+d.m))
  .attr("y1",d=>y(d.l)+y.bandwidth()/2).attr("y2",d=>y(d.l)+y.bandwidth()/2)
  .attr("stroke",c).attr("stroke-width",4).attr("stroke-linecap","round");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.e)).attr("cy",d=>y(d.l)+y.bandwidth()/2).attr("r",3.6)
  .attr("fill","#fff").attr("stroke",c).attr("stroke-width",2);
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("people who moved, with the published 90%% interval");
})();
</script>
', rows, BLU, RED))

## ---- mob-facts
f <- mo[1, ]; l <- mo[nrow(mo), ]

## ---- mobility-static
par(mar = c(3.8, 4.0, 1.6, 9.2))
plot(NA, xlim = range(mo$year), ylim = c(0, max(mo$movers) * 1.04),
     xlab = "", ylab = "% who moved in the previous year", axes = FALSE)
axis(1, cex.axis = 0.78); axis(2, las = 1, cex.axis = 0.78); box(col = "#ccc")
V <- list(c("movers", GRY, "all movers"),
          c("same_county", BLU, "within the same county"),
          c("same_state_diff_county", "#4d9221", "same state, new county"),
          c("diff_state", RED, "to a different state"))
for (v in V) lines(mo$year, mo[[v[1]]], col = v[2], lwd = 2.2)
for (v in V) text(max(mo$year) + 1.2, mo[[v[1]]][nrow(mo)], v[3], pos = 4,
                  cex = 0.55, col = v[2], xpd = NA)

## ---- mobility-d3
# Drawn with the shared chart library rather than hand-written D3: this is a
# plain multi-series line chart, which is exactly what the library is for.
# d3 itself was loaded once by the arc map above, so dd_fig() is told not to
# emit it a second time; it still emits dd-charts.js.
dd_fig("mobility", "line",
  mo[, c("year", "movers", "same_county", "same_state_diff_county",
         "diff_state")],
  d3 = FALSE,
  x = list(field = "year", fmt = "d"),
  y = list(domain = c(0, ceiling(max(mo$movers) * 1.05)),
           label = "% who moved in the previous year", fmt = "f1"),
  series = list(fields = list(
    list(field = "movers", label = "all movers"),
    list(field = "same_county", label = "within the same county"),
    list(field = "same_state_diff_county", label = "same state, new county"),
    list(field = "diff_state", label = "to a different state"))),
  endLabels = TRUE,
  size = list(h = 380, m = list(r = 170)))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
