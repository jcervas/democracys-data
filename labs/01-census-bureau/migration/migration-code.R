# migration-code.R -- chunk bodies for migration-brief.Rmd
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
# this chapter turns on, so the helper that prints an estimate together with its
# margin is defined once here and used everywhere below.
pm <- function(e, m) sprintf("%s ± %s", n(e), n(m))

# ---- palette -------------------------------------------------------------
RED <- "#C41230"; BLU <- "#2c7fb8"; GRN <- "#4d9221"
ORG <- "#e08214"; PUR <- "#8856a7"; GRY <- "#999999"
INK <- "#12161c"; LAND <- "#1e2732"; EDGE <- "#2f3d4d"
ARC <- "#5fb0e5"; KEYTX <- "#9fb3c8"

# ---- how an arc is drawn -------------------------------------------------
# WIDTH carries the number of people and nothing else carries it. The scale is
# ABSOLUTE -- WLWD line units at WREF people, in every panel and in the D3
# version -- so the same thickness always means the same number of movers and
# the key in the corner of each map can be read off. The transform is a square
# root: these flows span roughly 300 to 1, and drawn linearly the small arcs
# vanish while the big ones become blobs. Square root is the standard
# perceptual compromise for line thickness, and it still delivers a much wider
# span of widths than the flattened version this replaces.
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

# one arc panel, base R -- used for the PDF build of every arc figure
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
                       c(FOCUS, CONTRAST), c(F1$mx, C1$mx), c(F1$my, C1$my)),
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
const A={"%s":[%s],"%s":[%s]};
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
const dots=svg.append("g");
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
dots.selectAll("circle").data(HUB).join("circle");
function hubdots(){
  const h=HUB.find(z=>z.n===HUBN);
  hubg.selectAll("circle").data([h]).join("circle")
    .attr("cx",z=>z.x).attr("cy",z=>H-z.y).attr("r",4.5)
    .attr("fill","%s").attr("stroke","#fff").attr("stroke-width",1.2);
}
d3.selectAll(\'input[name="dir"]\').on("change",draw);
d3.select("#onlysig").on("change",draw);
hubdots(); draw();
window.__setHub=function(nm){HUBN=nm;hubdots();draw();};
})();
</script>
', FOCUS, FOCUS, FOCUS, mk(FOCUS), CONTRAST, mk(CONTRAST), paths, inbx, hubxy,
   MW, MH, INK, LAND, EDGE, EDGE,
   ARC, GRY, KEYTX, WREF, WLWD, WMIN, paste(WKEY, collapse = ","),
   FOCUS, RED))

## ---- arcmap-static
par(mfrow = c(1, 2))
arcpanel(FOCUS, "in",  sprintf("Arriving in %s", FOCUS), cexs = 1.5)
arcpanel(FOCUS, "out", sprintf("Leaving %s", FOCUS),     cexs = 1.5)

## ---- diverge-setup
# the margin of error on a rate is the margin on the count, divided by the same
# population the rate was divided by
S$rate_moe <- 1000 * S$net_moe / S$pop1
S$rate_lo  <- S$net_per1k - S$rate_moe
S$rate_hi  <- S$net_per1k + S$rate_moe

TOPR <- which.max(S$net_per1k)     # highest rate
TOPN <- which.max(S$net)           # highest raw count
# every other jurisdiction whose rate interval reaches into the leader's
OVL  <- which(S$rate_hi >= S$rate_lo[TOPR] & seq_len(nrow(S)) != TOPR)
RKN  <- rank(-S$net); RKR <- rank(-S$net_per1k)
SPR  <- cor(S$net, S$net_per1k, method = "spearman")

## ---- diverge-static
# The HTML build of this figure is one chart with a toggle. Print cannot toggle,
# so it gets both panels at once: same 51 jurisdictions, two orderings.
divpanel <- function(v, m, ttl, xlab) {
  o <- order(v); vv <- v[o]; mm <- m[o]
  par(mar = c(4.2, 5.0, 1.8, 1.2))
  bp <- barplot(vv, horiz = TRUE, border = NA, names.arg = rep("", length(vv)),
                col = ifelse(!S$net_sig[o], GRY, ifelse(vv > 0, GRN, RED)),
                xlim = range(c(vv - mm, vv + mm)) * 1.04, xlab = xlab)
  axis(2, at = bp, labels = S$state[o], las = 1, cex.axis = 0.36,
       tick = FALSE, line = -0.7)
  segments(vv - mm, bp, vv + mm, bp, col = "#00000099", lwd = 0.6)
  segments(0, min(bp) - 0.5, 0, max(bp) + 0.5, col = "#333", lwd = 1)
  mtext(ttl, side = 3, line = 0.4, cex = 0.66, adj = 0)
}
par(mfrow = c(1, 2))
divpanel(S$net_per1k, S$rate_moe, "per 1,000 residents",
         "net per 1,000 residents")
divpanel(S$net / 1000, S$net_moe / 1000, "raw net people",
         "net people (thousands)")
legend("topleft", bty = "n", cex = 0.5, fill = c(GRN, RED, GRY), border = NA,
       legend = c("net gain", "net loss", "not distinguishable from zero"))

## ---- diverge-d3
rows <- paste(sprintf('{"s":"%s","v":%.2f,"r":%.2f,"n":%d,"m":%d,"g":%d}',
        gsub('"', "", S$state), S$net_per1k, S$rate_moe, S$net, round(S$net_moe),
        as.integer(S$net_sig)), collapse = ",")
cat(sprintf('
<div id="dvwrap" style="margin:1em 0">
 <div style="margin:0 0 .5em 0;font:13px/1.4 inherit"><b>Measure:</b>
  <label><input type="radio" name="dvm" value="rate" checked>
   net per 1,000 residents</label>
  <label style="margin-left:.9em"><input type="radio" name="dvm" value="raw">
   raw net people</label>
 </div>
 <div id="dv" style="position:relative"></div>
 <p id="dvcap" style="font-size:0.85em;color:#666;margin:.35em 0 0 0"></p>
</div>
<script>
(function(){
const D=[%s];
const W=760,H=720,M={t:26,r:30,b:40,l:132};
const svg=d3.select("#dv").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const gx=svg.append("g").attr("transform",`translate(0,${H-M.b})`);
const gy=svg.append("g").attr("transform",`translate(${M.l},0)`);
const gb=svg.append("g"), ge=svg.append("g");
const zero=svg.append("line").attr("stroke","#333");
const xlab=svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#444");
const tip=d3.select("#dv").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const cap=d3.select("#dvcap");
[["%s","net gain"],["%s","net loss"],["%s","not distinguishable from zero"]]
 .forEach((c,i)=>{
  svg.append("rect").attr("x",M.l+8+i*168).attr("y",6).attr("width",11).attr("height",11).attr("fill",c[0]);
  svg.append("text").attr("x",M.l+24+i*168).attr("y",15.5).attr("font-size","10.5px")
    .attr("fill","#555").text(c[1]);});
function draw(){
  const raw=d3.select(\'input[name="dvm"]:checked\').property("value")==="raw";
  const V=d=>raw?d.n:d.v, E=d=>raw?d.m:d.r;
  const A=D.slice().sort((a,b)=>V(a)-V(b));
  const x=d3.scaleLinear().domain([d3.min(A,d=>V(d)-E(d)),d3.max(A,d=>V(d)+E(d))])
    .nice().range([M.l,W-M.r]);
  const y=d3.scaleBand().domain(A.map(d=>d.s)).range([M.t,H-M.b]).padding(0.18);
  const T=svg.transition().duration(500);
  gx.transition(T).call(d3.axisBottom(x).ticks(8)
    .tickFormat(raw?d3.format("~s"):null));
  gy.transition(T).call(d3.axisLeft(y).tickSize(0))
    .call(g=>g.select(".domain").remove());
  gy.selectAll("text").attr("font-size","9.5px");
  gb.selectAll("rect").data(A,d=>d.s).join("rect")
    .attr("fill",d=>!d.g?"%s":(V(d)>0?"%s":"%s"))
    .on("mousemove",function(e,d){tip.style("opacity",1).html(
       `<b>${d.s}</b><br>${d3.format("+,")(d.n)} &plusmn; ${d3.format(",")(d.m)} people`+
       `<br>${d3.format("+.2f")(d.v)} &plusmn; ${d3.format(".2f")(d.r)} per 1,000`+
       (d.g?"":`<br><span style="color:#ffb0bd">not distinguishable from zero</span>`))
     .style("left",Math.min(e.offsetX+14,W-230)+"px").style("top",(e.offsetY-6)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0))
    .transition(T)
    .attr("x",d=>Math.min(x(0),x(V(d)))).attr("y",d=>y(d.s))
    .attr("width",d=>Math.abs(x(V(d))-x(0))).attr("height",y.bandwidth());
  ge.selectAll("line").data(A,d=>d.s).join("line")
    .attr("stroke","#00000099").attr("stroke-width",1).transition(T)
    .attr("x1",d=>x(V(d)-E(d))).attr("x2",d=>x(V(d)+E(d)))
    .attr("y1",d=>y(d.s)+y.bandwidth()/2).attr("y2",d=>y(d.s)+y.bandwidth()/2);
  zero.transition(T).attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b);
  xlab.text(raw?"net interstate migration, people":
                "net interstate migration per 1,000 residents");
  // the two leaders are named in R and passed in as strings, so this caption
  // and the prose around the figure cannot pick different states
  cap.html(`Sorted by ${raw?"the raw count":"the rate"}. `+
    `<b>%s</b> leads per 1,000 residents; <b>%s</b> leads on the raw `+
    `count. The black line on each bar is the 90 percent interval published `+
    `with the estimate: where two of them overlap, the survey does not `+
    `establish which of those two states is higher.`);
}
d3.selectAll(\'input[name="dvm"]\').on("change",draw);
draw();
})();
</script>
', rows, GRN, RED, GRY, GRY, GRN, RED, S$state[TOPR], S$state[TOPN]))

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

## ---- contrast-static
par(mfrow = c(1, 2))
arcpanel(FOCUS, "in", sprintf("%s: %d of %d usable", FOCUS, F1$sig_in_n, NOR),
         grey_insig = TRUE, cexs = 1.5)
arcpanel(CONTRAST, "in", sprintf("%s: %d of %d usable", CONTRAST, C1$sig_in_n, NOR),
         grey_insig = TRUE, cexs = 1.5)

## ---- contrast-d3
cat(sprintf('
<div id="arcwrap2" style="margin:1em 0">
 <div style="margin:0 0 .5em 0;font:13px/1.4 inherit"><b>Hub state:</b>
  <label><input type="radio" name="hub2" value="%s" checked> %s (%d of %d usable)</label>
  <label style="margin-left:.9em"><input type="radio" name="hub2" value="%s"> %s (%d of %d usable)</label>
 </div>
</div>
<p style="font-size:0.85em;color:#666;margin:.2em 0 0 0">
These buttons redraw the map above. Dotted gray arcs are flows the survey cannot
distinguish from zero; switch on "hide flows not distinguishable from zero" to
see what is left.</p>
<script>
(function(){
d3.selectAll(\'input[name="hub2"]\').on("change",function(){
  window.__setHub(this.value);
  document.getElementById("arc").scrollIntoView({behavior:"smooth",block:"center"});
});
})();
</script>
', FOCUS, FOCUS, F1$sig_in_n, NOR, CONTRAST, CONTRAST, C1$sig_in_n, NOR))

## ---- sigscatter-facts
# how tightly usable-inflow count tracks size. Computed once here so the caption
# and the sentence beneath it quote the same correlation.
SIGCOR <- cor(log(S$pop1), S$sig_in_n)
SMALL  <- S[which.min(S$pop1), ]
BIG    <- S[which.max(S$pop1), ]

## ---- sigscatter-static
par(mar = c(4.0, 4.2, 1.0, 1.0))
plot(S$pop1, S$sig_in_n, log = "x", pch = 19, cex = 0.9,
     col = adjustcolor(BLU, 0.75), xaxt = "n",
     ylim = c(min(S$sig_in_n) - 2, max(S$sig_in_n) + 4),
     xlab = "state population aged 1 and over (log scale)",
     ylab = sprintf("usable inflows (of %d)", NOR))
at <- c(6e5, 1e6, 3e6, 1e7, 3e7)
axis(1, at = at, labels = c("600k", "1m", "3m", "10m", "30m"), cex.axis = 0.75)
lab <- S$state %in% c(FOCUS, CONTRAST, "Texas", "Florida", "Wyoming", "Vermont",
                      "New York", "North Dakota")
text(S$pop1[lab], S$sig_in_n[lab], S$state[lab], pos = 3, cex = 0.55, col = "#444")
points(S$pop1[S$state %in% c(FOCUS, CONTRAST)], S$sig_in_n[S$state %in% c(FOCUS, CONTRAST)],
       pch = 21, cex = 1.3, col = RED, lwd = 1.6)

## ---- sigscatter-d3
rows <- paste(sprintf('{"s":"%s","p":%d,"k":%d,"h":%d}', gsub('"', "", S$state),
       S$pop1, S$sig_in_n, as.integer(S$state %in% c(FOCUS, CONTRAST))),
       collapse = ",")
cat(sprintf('
<div id="sc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=380,M={t:16,r:24,b:46,l:60};
const svg=d3.select("#sc").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain(d3.extent(D,d=>d.p)).nice().range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,52]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6,"~s"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("state population aged 1 and over (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#444").text("inflows distinguishable from zero (of %d)");
const tip=d3.select("#sc").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.p)).attr("cy",d=>y(d.k)).attr("r",d=>d.h?6:4.5)
  .attr("fill",d=>d.h?"%s":"%s").attr("fill-opacity",0.8)
  .attr("stroke",d=>d.h?"#fff":"none").attr("stroke-width",1.3)
  .on("mousemove",function(e,d){tip.style("opacity",1)
    .html(`<b>${d.s}</b><br>${d3.format(",")(d.p)} people<br>${d.k} of %d inflows usable`)
    .style("left",Math.min(e.offsetX+14,W-190)+"px").style("top",(e.offsetY-6)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', rows, NOR, RED, BLU, NOR))

## ---- born-extremes
o <- S[order(-S$born_state_pct), c("state", "born_state_pct")]
hi <- head(o, 5); lo <- tail(o, 5)
data.frame(
  `most born in state` = sprintf("%s (%s%%)", hi$state, pc(hi$born_state_pct)),
  `fewest born in state` = sprintf("%s (%s%%)", rev(lo$state), pc(rev(lo$born_state_pct))),
  check.names = FALSE)

## ---- stack-setup
K <- rbind(head(S[order(-S$born_state_pct), ], 6),
           head(S[order( S$born_state_pct), ], 6))
K <- K[order(K$born_state_pct), ]
K$other_us <- K$born_otherstate_pct + K$born_pr_pct
SEG <- c(BLU, GRN, ORG)
SLAB <- c("born in this state", "born in another U.S. state or Puerto Rico",
          "born in a foreign country")

## ---- stack-static
par(mar = c(4.2, 7.2, 2.2, 1.0))
m <- t(as.matrix(K[, c("born_state_pct", "other_us", "born_foreign_pct")]))
bp <- barplot(m, horiz = TRUE, col = SEG, border = "white", names.arg = rep("", ncol(m)),
              xlim = c(0, 100), xlab = "% of residents")
axis(2, at = bp, labels = K$state, las = 1, cex.axis = 0.62, tick = FALSE, line = -0.6)
legend("top", inset = c(0, -0.13), xpd = NA, bty = "n", cex = 0.58, ncol = 3,
       fill = SEG, border = "white", legend = SLAB)

## ---- stack-d3
rows <- paste(sprintf('{"s":"%s","a":%.2f,"b":%.2f,"c":%.2f}', gsub('"', "", K$state),
       K$born_state_pct, K$other_us, K$born_foreign_pct), collapse = ",")
cat(sprintf('
<div id="sk" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], SEG=["%s","%s","%s"],
 LAB=["born in this state","born in another U.S. state or Puerto Rico","born in a foreign country"];
const W=760,H=390,M={t:34,r:20,b:40,l:132};
const svg=d3.select("#sk").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([H-M.b,M.t]).padding(0.22);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0))
  .call(g=>g.select(".domain").remove()).selectAll("text").attr("font-size","10px");
const tip=d3.select("#sk").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach(d=>{
  const v=[d.a,d.b,d.c]; let acc=0;
  v.forEach((q,i)=>{
    svg.append("rect").attr("x",x(acc)).attr("y",y(d.s))
      .attr("width",x(q)-x(0)).attr("height",y.bandwidth())
      .attr("fill",SEG[i]).attr("stroke","#fff").attr("stroke-width",0.8)
      .on("mousemove",function(e){const w=d3.select("#sk").node().clientWidth;
        tip.style("opacity",1).html(`<b>${d.s}</b><br>${LAB[i]}<br>${q.toFixed(1)}%%`)
         .style("left",Math.min(e.offsetX+14,w-230)+"px").style("top",(e.offsetY-6)+"px");})
      .on("mouseleave",()=>tip.style("opacity",0));
    acc+=q;});
});
LAB.forEach((l,i)=>{
  svg.append("rect").attr("x",M.l+i*208).attr("y",10).attr("width",11).attr("height",11)
    .attr("fill",SEG[i]);
  svg.append("text").attr("x",M.l+16+i*208).attr("y",19.5).attr("font-size","10px")
    .attr("fill","#555").text(l);});
})();
</script>
', rows, SEG[1], SEG[2], SEG[3]))

## ---- abroad-table
o <- S[order(-S$abroad_per1k), ]
h <- head(o, 6)
data.frame(state = h$state,
           `arrivals from abroad` = pm(h$abroad_est, h$abroad_moe),
           `per 1,000 residents` = pc(h$abroad_per1k),
           `net interstate migration` = sprintf("%s%s", ifelse(h$net > 0, "+", ""), n(h$net)),
           check.names = FALSE)

## ---- county-table
h <- head(cy, 6)
data.frame(
  `moved from` = sprintf("%s, %s", h$county_a, h$state_a),
  `people, with margin` = pm(h$into_b, h$into_b_moe),
  `distinguishable from zero` = ifelse(h$sig_in, "yes", "no"),
  check.names = FALSE)

## ---- county-accounting
data.frame(
  quantity = c(sprintf("Counties with a reported inflow to %s", CTY),
               "Of those, distinguishable from zero",
               "Share that survive (%)",
               "Share of state pairs that survived, for comparison (%)"),
  value = c(n(mn("county_all_pairs")), n(mn("county_all_sig")),
            pc(100 * mn("county_all_sig") / mn("county_all_pairs")),
            pc(mn("pct_sig_of_pairs"))))

## ---- mobility-static
par(mar = c(3.8, 4.0, 1.6, 9.2))
plot(NA, xlim = range(mo$year), ylim = c(0, max(mo$movers) * 1.04),
     xlab = "", ylab = "% who moved in the previous year", axes = FALSE)
axis(1, cex.axis = 0.78); axis(2, las = 1, cex.axis = 0.78); box(col = "#ccc")
V <- list(c("movers", GRY, "all movers"),
          c("same_county", BLU, "within the same county"),
          c("same_state_diff_county", GRN, "same state, new county"),
          c("diff_state", RED, "to a different state"))
for (v in V) lines(mo$year, mo[[v[1]]], col = v[2], lwd = 2.2)
for (v in V) text(max(mo$year) + 1.2, mo[[v[1]]][nrow(mo)], v[3], pos = 4,
                  cex = 0.55, col = v[2], xpd = NA)

## ---- mobility-d3
rows <- paste(sprintf('{"y":%d,"a":%.1f,"b":%.1f,"c":%.1f,"d":%.1f}', mo$year,
       mo$movers, mo$same_county, mo$same_state_diff_county, mo$diff_state),
       collapse = ",")
cat(sprintf('
<div id="ts" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const K=[["a","%s","all movers"],["b","%s","within the same county"],
         ["c","%s","same state, new county"],["d","%s","to a different state"]];
const W=760,H=380,M={t:16,r:150,b:42,l:52};
const svg=d3.select("#ts").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.a)*1.05]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
K.forEach(k=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",k[1]).attr("stroke-width",2.2)
    .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d[k[0]])));
  const last=D[D.length-1];
  svg.append("text").attr("x",W-M.r+8).attr("y",y(last[k[0]])+3).attr("font-size","10.5px")
    .attr("fill",k[1]).text(k[2]);
});
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",13)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#444")
  .text("%% who moved in the previous year");
const tip=d3.select("#ts").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l).attr("height",H-M.b-M.t)
 .attr("fill","none").attr("pointer-events","all")
 .on("mousemove",function(e){
   const yr=Math.round(x.invert(d3.pointer(e,this)[0]+M.l));
   const d=D.reduce((p,q)=>Math.abs(q.y-yr)<Math.abs(p.y-yr)?q:p);
   tip.style("opacity",1).html(`<b>${d.y}</b><br>all movers ${d.a}%%`+
     `<br>different state ${d.d}%%`)
    .style("left",Math.min(e.offsetX+14,W-170)+"px").style("top",(e.offsetY-6)+"px");})
 .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', rows, GRY, BLU, GRN, RED))

## ---- mobility-numbers
f <- mo[1, ]; l <- mo[nrow(mo), ]
data.frame(
  measure = c("Share who moved at all (%)", "Share who moved to a different state (%)"),
  `first year` = c(pc(f$movers), pc(f$diff_state)),
  `last year`  = c(pc(l$movers), pc(l$diff_state)),
  check.names = FALSE)

## ---- gainers
G <- head(S[order(-S$net), ], 2)          # the two biggest net gainers

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
