# chord-code.R -- chunk bodies for chord-brief.Rmd
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

mx <- read.csv("data/derived/matrix.csv", stringsAsFactors = FALSE)
gr <- read.csv("data/derived/groups.csv", stringsAsFactors = FALSE)
ar <- read.csv("data/derived/arcs.csv",   stringsAsFactors = FALSE)
pr <- read.csv("data/derived/pairs.csv",  stringsAsFactors = FALSE)
sa <- read.csv("data/derived/states.csv", stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",  stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
mil <- function(x) paste0(p1(as.numeric(x) / 1e6), " million")

DIVS <- c("New England", "Middle Atlantic", "East North Central",
          "West North Central", "South Atlantic", "East South Central",
          "West South Central", "Mountain", "Pacific", "Puerto Rico")
REGS <- c("Northeast", "Midwest", "South", "West", "Puerto Rico")

# Hue by region, shade by division inside it, so the nesting the Bureau built
# into these names is visible without a second legend.
COL <- c("New England"        = "#1C4C5C", "Middle Atlantic"    = "#4E9DB5",
         "East North Central" = "#2F6B3C", "West North Central" = "#77AC6E",
         "South Atlantic"     = "#C41230", "East South Central" = "#D9622B",
         "West South Central" = "#E0A030",
         "Mountain"           = "#6B4A87", "Pacific"            = "#A08CC0",
         "Puerto Rico"        = "#8A8F94",
         "Northeast" = "#1C4C5C", "Midwest" = "#2F6B3C",
         "South" = "#C41230", "West" = "#6B4A87")
GRY <- "#8A8F94"; WARN <- "#C41230"; ACC <- "#1C4C5C"

# short labels for the rim, where the full names do not fit
SHORT <- c("New England" = "New Eng.", "Middle Atlantic" = "Mid. Atlantic",
           "East North Central" = "E.N. Central",
           "West North Central" = "W.N. Central",
           "South Atlantic" = "S. Atlantic",
           "East South Central" = "E.S. Central",
           "West South Central" = "W.S. Central",
           "Mountain" = "Mountain", "Pacific" = "Pacific",
           "Puerto Rico" = "Puerto Rico",
           "Northeast" = "Northeast", "Midwest" = "Midwest",
           "South" = "South", "West" = "West")

NUNIT <- fn("units"); NPAIR <- fn("ordered_pairs")
NSUPP <- fn("suppressed_pairs"); NSIG <- fn("sig_pairs")
BETWEEN <- fn("movers_between_states"); WITHIN_ST <- fn("movers_within_state")
ABROAD <- fn("movers_abroad"); YR <- f("acs_year")
NDIV <- fn("divisions"); NREG <- fn("regions")
DRIB <- fn("div_ribbons"); RRIB <- fn("reg_ribbons")
WD <- fn("within_div"); BD <- fn("between_div"); WPCT <- fn("within_pct")
BA <- f("big_a"); BB <- f("big_b"); BG <- fn("big_gross")
BAB <- fn("big_ab"); BBA <- fn("big_ba"); BN <- fn("big_net")
BNP <- fn("big_net_pct")
TA <- f("tiny_a"); TB <- f("tiny_b"); TG <- fn("tiny_gross")
TN <- fn("tiny_net"); TM <- fn("tiny_net_moe")
CA <- f("canc_a"); CB <- f("canc_b"); CG <- fn("canc_gross")
CN <- fn("canc_net"); CM <- fn("canc_net_moe")
DP <- fn("div_pairs"); DPS <- fn("div_pairs_sig")
RP <- fn("reg_pairs"); RPS <- fn("reg_pairs_sig")
MNP <- fn("median_net_pct"); ONP <- fn("overall_net_pct")
DCELL <- fn("div_cells"); DSUPP <- fn("div_cells_supp")
DZERO <- fn("div_cells_zero")
LSEAT <- fn("loser_seats"); WSEAT <- fn("winner_seats")
LDIV <- fn("loser_divs"); WDIV <- fn("winner_divs")
TGAIN <- f("top_gainer"); TGP <- fn("top_gainer_per1k")
TLOSE <- f("top_loser"); TLP <- fn("top_loser_per1k")
NESN <- fn("ne_south_net"); NESG <- fn("ne_south_gross")
SWN <- fn("south_west_net"); SWG <- fn("south_west_gross")
PROUT <- fn("pr_out"); PRIN <- fn("pr_in")

gd <- gr[gr$grouping == "division" & gr$within, ]
gd <- gd[match(DIVS, gd$group), ]
gd$pop <- as.numeric(tapply(sa$pop1, sa$division, sum)[gd$group])
gd$seats <- as.numeric(tapply(sa$seats_2020, sa$division, sum)[gd$group])
gd$per1k <- 1000 * gd$net / gd$pop
pd <- pr[pr$grouping == "division", ]

# --- one chord ribbon, as a polygon ------------------------------------------
#
# The same path d3.ribbon draws, and the same one the HTML figure draws: an arc
# along the sending sub-arc, a quadratic Bezier through the CENTRE of the circle
# to the receiving sub-arc, an arc along that, and a Bezier back. Angles are
# measured from twelve o'clock, clockwise, which is what d3.arc does, so the
# two figures place every ribbon in the same place.
px <- function(a, r) r * sin(a)
py <- function(a, r) r * cos(a)
qbez <- function(p0, p1, k = 24) {
  t <- seq(0, 1, length.out = k)
  cbind((1 - t)^2 * p0[1] + t^2 * p1[1], (1 - t)^2 * p0[2] + t^2 * p1[2])
}
arcpts <- function(a0, a1, r, k = 24) {
  a <- seq(a0, a1, length.out = max(2, k))
  cbind(px(a, r), py(a, r))
}
ribbon <- function(a0, a1, b0, b1, r) {
  s <- arcpts(a0, a1, r); t <- arcpts(b0, b1, r)
  rbind(s,
        qbez(s[nrow(s), ], t[1, ]),
        t,
        qbez(t[nrow(t), ], s[1, ]))
}

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
z <- pd[pd$a == BA & pd$b == BB, ]
data.frame(
  From = c(z$a, z$b),
  To   = c(z$b, z$a),
  People = n(c(z$a_to_b, z$b_to_a)),
  Note = c("the larger direction", "the smaller direction"))

## ---- varmap
data.frame(
  Column = c("from", "to", "est", "moe", "cells", "suppressed"),
  What_it_holds = c(
    "the group people left",
    "the group they arrived in; when it equals `from`, they changed state without leaving the group",
    "how many people the ACS estimates made that move in the year",
    "the margin of error on that estimate, at 90% confidence",
    "how many state-to-state pairs were added together to make this cell",
    "how many of those pairs the Census would not release"),
  Measurement = c("categorical", "categorical", "count", "continuous",
                  "count", "count"))

## ---- grouptab
z <- do.call(rbind, lapply(REGS, function(r) {
  d <- unique(sa$division[sa$region == r])
  data.frame(Region = r, Division = d,
             States = sapply(d, function(k) sum(sa$division == k)),
             Population = n(sapply(d, function(k) sum(sa$pop1[sa$division == k]))),
             House_seats = sapply(d, function(k) sum(sa$seats_2020[sa$division == k])),
             stringsAsFactors = FALSE)
}))
rownames(z) <- NULL
z

## ---- fig1-static
op <- par(mar = c(0.3, 0.3, 0.3, 0.3))
R <- 1; RO <- 1.055; LB <- 1.10
plot(NA, xlim = c(-1.62, 1.62), ylim = c(-1.42, 1.42), asp = 1,
     axes = FALSE, xlab = "", ylab = "")
a <- ar[ar$grouping == "division" & ar$within, ]
g <- gd
# ribbons first, widest ends underneath, so the small exchanges stay visible
key <- function(x, y) paste(pmin(x, y), pmax(x, y), sep = "\r")
seen <- character(0)
ord <- a[order(-a$est), ]
for (i in seq_len(nrow(ord))) {
  s <- ord[i, ]; k <- key(s$from, s$to)
  if (k %in% seen) next
  seen <- c(seen, k)
  t <- a[a$from == s$to & a$to == s$from, ]
  if (!nrow(t)) t <- s                    # a within-group ribbon meets itself
  # the ribbon takes the colour of whichever group sent more people
  cl <- COL[if (s$est >= t$est) s$from else t$from]
  p <- ribbon(s$ang0, s$ang1, t$ang0, t$ang1, R)
  polygon(p[, 1], p[, 2], col = adjustcolor(cl, alpha.f = 0.42), border = NA)
}
for (i in seq_len(nrow(g))) {
  p <- rbind(arcpts(g$ang0[i], g$ang1[i], R),
             arcpts(g$ang1[i], g$ang0[i], RO))
  polygon(p[, 1], p[, 2], col = COL[g$group[i]], border = NA)
  am <- (g$ang0[i] + g$ang1[i]) / 2
  sd <- if (sin(am) >= 0) 4 else 2
  text(px(am, LB), py(am, LB), SHORT[g$group[i]], adj = if (sd == 4) 0 else 1,
       cex = 0.62, col = COL[g$group[i]],
       srt = 0)
}
text(0, 1.36, paste0("Interstate moves, ", YR, ", between and within the nine ",
                     "Census divisions"), cex = 0.72)
text(0, -1.36, paste0("Ribbon width at each rim is how many people that group ",
                      "SENT; the difference between a ribbon's two ends is the net."),
     cex = 0.6, col = "#4E5A63")
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The chord diagram. THE LAYOUT IS NOT COMPUTED HERE: build-data.R writes the
# start and end angle of every sub-arc for all four combinations of grouping
# (divisions or regions) and within-group ribbons (drawn or not), so the PDF
# and this figure place every ribbon identically instead of relying on two
# implementations of the same layout agreeing to six decimal places.
#
# Angles run clockwise from twelve o'clock, which is d3.arc's convention.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)
key <- function(x, y) paste(pmin(x, y), pmax(x, y), sep = "|")
lay <- function(kk, wi) {
  a <- ar[ar$grouping == kk & ar$within == wi, ]
  g <- gr[gr$grouping == kk & gr$within == wi, ]
  ord <- if (kk == "division") DIVS else REGS
  g <- g[match(ord, g$group), ]
  seen <- character(0); rows <- character(0)
  z <- a[order(-a$est), ]
  for (i in seq_len(nrow(z))) {
    s <- z[i, ]; k <- key(s$from, s$to)
    if (k %in% seen) next
    seen <- c(seen, k)
    t <- a[a$from == s$to & a$to == s$from, ]
    if (!nrow(t)) t <- s
    cl <- COL[if (s$est >= t$est) s$from else t$from]
    rows <- c(rows, paste0(
      '{s:"', esc(s$from), '",t:"', esc(s$to), '",sv:', s$est, ',tv:', t$est,
      ',c:"', cl, '",a:[', s$ang0, ',', s$ang1, '],b:[', t$ang0, ',', t$ang1,
      ']}'))
  }
  grows <- paste0('{g:"', esc(g$group), '",l:"', esc(SHORT[g$group]),
                  '",c:"', COL[g$group], '",o:', g$out_est, ',i:', g$in_est,
                  ',a:[', g$ang0, ',', g$ang1, ']}', collapse = ",")
  paste0('{rib:[', paste(rows, collapse = ","), '],grp:[', grows, ']}')
}
cat(paste0('
<div id="cd" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const L={division:{on:', lay("division", TRUE), ',off:', lay("division", FALSE), '},
         region:{on:', lay("region", TRUE), ',off:', lay("region", FALSE), '}};
const W=760,H=620,R=228,RO=243,LB=252;
const box=d3.select("#cd");
const bar=box.append("div").attr("style","margin:0 0 8px;display:flex;align-items:center;gap:8px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const G=svg.append("g").attr("transform","translate("+(W/2)+","+(H/2+6)+")");
const gRib=G.append("g"), gArc=G.append("g"), gLab=G.append("g");
const cm=d3.format(",");
const P=(a,r)=>[r*Math.sin(a),-r*Math.cos(a)];
const arc=d3.arc().innerRadius(R+2).outerRadius(RO);
function ribPath(a,b,r){
  const A=d3.path();
  const p0=P(a[0],r), p1=P(a[1],r), q0=P(b[0],r), q1=P(b[1],r);
  A.moveTo(p0[0],p0[1]);
  // angles here run clockwise from twelve o\u2019clock; a canvas arc measures
  // from three o\u2019clock, so every angle is shifted by a quarter turn
  A.arc(0,0,r,a[0]-Math.PI/2,a[1]-Math.PI/2,false);
  A.quadraticCurveTo(0,0,q0[0],q0[1]);
  A.arc(0,0,r,b[0]-Math.PI/2,b[1]-Math.PI/2,false);
  A.quadraticCurveTo(0,0,p0[0],p0[1]);
  A.closePath();
  return A+"";
}
let mode="division", within=true, focus=null;
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;border-radius:3px;padding:7px 9px;font:11.5px inherit;max-width:280px;box-shadow:0 1px 4px rgba(0,0,0,.14)");
function show(e,html){
  const r=box.node().getBoundingClientRect();
  tip.style("opacity",1).style("left",Math.min(e.clientX-r.left+14,W-290)+"px")
     .style("top",(e.clientY-r.top-10)+"px").html(html);
}
function draw(){
  const D=L[mode][within?"on":"off"];
  const rb=gRib.selectAll("path").data(D.rib,d=>d.s+"|"+d.t);
  rb.join(en=>en.append("path"),u=>u,x=>x.remove())
    .attr("d",d=>ribPath(d.a,d.b,R))
    .attr("fill",d=>d.c)
    .attr("fill-opacity",d=>focus&&focus!==d.s&&focus!==d.t?0.05:0.44)
    .attr("stroke",d=>d.c).attr("stroke-opacity",d=>focus&&focus!==d.s&&focus!==d.t?0.05:0.28)
    .on("mousemove",function(e,d){
      const net=d.sv-d.tv, big=net>=0?d.s:d.t, sml=net>=0?d.t:d.s;
      show(e, d.s===d.t
        ? "<b>"+d.s+"</b><br>"+cm(d.sv)+" people changed state without leaving it"
        : "<b>"+d.s+" \\u2194 "+d.t+"</b><br>"+
          d.s+" \\u2192 "+d.t+": "+cm(d.sv)+"<br>"+
          d.t+" \\u2192 "+d.s+": "+cm(d.tv)+"<br><b>net "+cm(Math.abs(net))+
          " to "+big+"</b> \\u00b7 "+
          d3.format(".1%")(Math.abs(net)/(d.sv+d.tv))+" of the exchange");
    })
    .on("mouseleave",function(){tip.style("opacity",0);});
  const ac=gArc.selectAll("path").data(D.grp,d=>d.g);
  ac.join(en=>en.append("path"),u=>u,x=>x.remove())
    .attr("d",d=>arc({startAngle:d.a[0],endAngle:d.a[1]}))
    .attr("fill",d=>d.c)
    .attr("fill-opacity",d=>focus&&focus!==d.g?0.25:1)
    .attr("cursor","pointer")
    .on("mousemove",function(e,d){
      show(e,"<b>"+d.g+"</b><br>left: "+cm(d.o)+"<br>arrived: "+cm(d.i)+
             "<br><b>net "+(d.i-d.o>=0?"+":"")+cm(d.i-d.o)+"</b>");
    })
    .on("mouseleave",function(){tip.style("opacity",0);})
    .on("click",function(e,d){focus=focus===d.g?null:d.g;draw();});
  const lb=gLab.selectAll("text").data(D.grp,d=>d.g);
  lb.join(en=>en.append("text"),u=>u,x=>x.remove())
    .attr("x",d=>P((d.a[0]+d.a[1])/2,LB)[0])
    .attr("y",d=>P((d.a[0]+d.a[1])/2,LB)[1])
    .attr("dy","0.34em")
    .attr("text-anchor",d=>Math.sin((d.a[0]+d.a[1])/2)>=0?"start":"end")
    .attr("font-size","11px")
    // currentColor, not the group colour: several of these hues are dark
    // enough to vanish on a dark ground, and the coloured arc is right there
    .attr("fill","currentColor")
    .attr("fill-opacity",d=>focus&&focus!==d.g?0.3:0.85)
    .text(d=>d.l);
}
function btn(label,fn){
  return bar.append("button")
    .attr("style","padding:4px 10px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
    .text(label).on("click",fn);
}
const b1=btn("group by region instead",function(){
  mode=mode==="division"?"region":"division"; focus=null;
  d3.select(this).text(mode==="division"?"group by region instead"
                                        :"group by division instead");
  draw();
});
const b2=btn("hide moves inside a group",function(){
  within=!within;
  d3.select(this).text(within?"hide moves inside a group"
                             :"show moves inside a group");
  draw();
});
bar.append("span").attr("style","color:#8A8F94")
   .text("click a rim segment to isolate it");
draw();
})();
</script>'))

## ---- pairtab
z <- pd[order(-pd$gross), ][1:8, ]
data.frame(
  # an en dash, not a double-headed arrow: the PDF font has no U+2194 and
  # silently sets a tofu box where the arrow should be
  Exchange = paste(z$a, "–", z$b),
  A_to_B = n(z$a_to_b), B_to_A = n(z$b_to_a),
  Gross = n(z$gross),
  Net = n(abs(z$net)),
  Net_margin = n(z$net_moe),
  Net_share = paste0(p1(z$net_pct_of_gross), "%"),
  Clears_margin = ifelse(z$net_sig, "yes", "no"))

## ---- fig2-static
op <- par(mar = c(0.4, 7.6, 7.0, 0.4))
M <- matrix(0, length(DIVS), length(DIVS), dimnames = list(DIVS, DIVS))
z <- mx[mx$grouping == "division", ]
M[cbind(match(z$from, DIVS), match(z$to, DIVS))] <- z$est
lv <- log10(M + 1); mxl <- max(lv)
ramp <- colorRampPalette(c("#F4F6F7", "#BBD4DC", "#4E9DB5", "#12414F"))(100)
plot(NA, xlim = c(0.5, length(DIVS) + 0.5), ylim = c(length(DIVS) + 0.5, 0.5),
     axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
for (i in seq_along(DIVS)) for (j in seq_along(DIVS)) {
  v <- M[i, j]
  cl <- if (i == j) "#E8E4DC" else ramp[max(1, ceiling(100 * lv[i, j] / mxl))]
  rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, col = cl, border = "#FFFFFF",
       lwd = 0.6)
  if (v > 0) {
    dark <- i != j && lv[i, j] / mxl > 0.72
    text(j, i, formatC(round(v / 1000), format = "d"),
         cex = 0.52, col = if (dark) "#FFFFFF" else "#12181D")
  } else if (i != j) {
    # an estimate of zero, not a missing cell -- said out loud, because a pale
    # square and an absent square look identical
    text(j, i, "0", cex = 0.52, col = "#8A8F94")
  }
}
axis(2, at = seq_along(DIVS), labels = SHORT[DIVS], las = 1, tick = FALSE,
     cex.axis = 0.58, line = -0.6)
axis(3, at = seq_along(DIVS), labels = SHORT[DIVS], las = 2, tick = FALSE,
     cex.axis = 0.58, line = -0.6)
mtext(paste0("Thousands of people, from the row to the column, shaded on a log ",
             "scale. Grey 0: an estimate of zero, not a missing cell."),
      3, line = 5.2, cex = 0.62, adj = 0)
par(op)

## ---- fig2-d3
# The matrix the chord was drawn from, plus the thing the chord cannot show at
# all: which of these cells survive their own margin of error. In net mode the
# blanked cells are the pairs whose difference is smaller than the uncertainty
# in it -- most of the matrix.
z <- mx[mx$grouping == "division", ]
cells <- paste0('{i:', match(z$from, DIVS) - 1, ',j:', match(z$to, DIVS) - 1,
                ',v:', z$est, ',m:', z$moe, ',s:', z$suppressed, '}',
                collapse = ",")
nets <- paste0('{a:', match(pd$a, DIVS) - 1, ',b:', match(pd$b, DIVS) - 1,
               ',n:', pd$net, ',m:', pd$net_moe, ',g:', pd$gross,
               ',sig:', tolower(as.character(pd$net_sig)), '}', collapse = ",")
cat(paste0('
<div id="mtx" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[', cells, '], N=[', nets, '];
const L=[', paste0('"', SHORT[DIVS], '"', collapse = ","), '];
const F=[', paste0('"', DIVS, '"', collapse = ","), '];
const n=L.length;
const W=760,M={t:104,r:16,b:26,l:112};
const CS=(W-M.l-M.r)/n, H=M.t+M.b+n*CS;
const box=d3.select("#mtx");
const bar=box.append("div").attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cm=d3.format(",");
const mxv=d3.max(C,d=>d.v), mxn=d3.max(N,d=>Math.abs(d.n));
const shade=d3.scaleSequential(d3.interpolateRgbBasis(
  ["#F4F6F7","#BBD4DC","#4E9DB5","#12414F"])).domain([0,1]);
const diverge=d3.scaleDiverging(d3.interpolateRgbBasis(
  ["#12414F","#DCE6E9","#8C1024"])).domain([-mxn,0,mxn]);
const g=svg.append("g");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;border-radius:3px;padding:7px 9px;font:11.5px inherit;max-width:280px;box-shadow:0 1px 4px rgba(0,0,0,.14)");
let net=false;
const netAt=(i,j)=>N.find(d=>(d.a===i&&d.b===j)||(d.a===j&&d.b===i));
function draw(){
  g.selectAll("*").remove();
  L.forEach(function(s,k){
    g.append("text").attr("x",M.l-8).attr("y",M.t+k*CS+CS/2+4)
     .attr("text-anchor","end").attr("font-size","10.5px")
     .attr("fill","currentColor").attr("fill-opacity",0.85).text(s);
    g.append("text")
     .attr("transform","translate("+(M.l+k*CS+CS/2+4)+","+(M.t-8)+") rotate(-90)")
     .attr("font-size","10.5px").attr("fill","currentColor")
     .attr("fill-opacity",0.85).text(s);
  });
  for (let i=0;i<n;i++) for (let j=0;j<n;j++){
    const c=C.find(d=>d.i===i&&d.j===j);
    const x=M.l+j*CS, y=M.t+i*CS;
    let fill="#EFEFEF", label="", netdark=false;
    if (!net){
      if (c){ fill=shade(Math.log10(c.v+1)/Math.log10(mxv+1));
              label=c.v>0?Math.round(c.v/1000):(i!==j?"0":""); }
      if (i===j) fill="#E8E4DC";
    } else {
      if (i===j){ fill="#E8E4DC"; }
      else {
        const q=netAt(i,j);
        if (q && q.sig){
          const v=(q.a===i)?q.n:-q.n;      // positive = i sent more to j
          fill=diverge(v); label=Math.round(Math.abs(v)/1000);
          if (Math.abs(v)/mxn>0.5) netdark=true;
        } else { fill="#F3F3F3"; label=""; }
      }
    }
    const dark=(!net&&i!==j)?(Math.log10((c?c.v:0)+1)/Math.log10(mxv+1)>0.72):false;
    g.append("rect").attr("x",x).attr("y",y).attr("width",CS-1)
     .attr("height",CS-1).attr("fill",fill)
     .on("mousemove",function(e){
       const q=netAt(i,j);
       const r=box.node().getBoundingClientRect();
       let h="<b>"+F[i]+" \\u2192 "+F[j]+"</b>";
       if (i===j) h+="<br>"+(c?cm(c.v):"0")+" changed state without leaving the group";
       else if (c) h+="<br>"+cm(c.v)+" people \\u00b1 "+cm(c.m)+
         (c.s>0?"<br><span style=\\"color:#8A8F94\\">"+c.s+
          " state pair(s) in this cell were suppressed</span>":"");
       if (q && i!==j){
         const v=(q.a===i)?q.n:-q.n;
         h+="<br><b>net "+cm(Math.abs(v))+" "+(v>=0?"out":"in")+"</b> \\u00b1 "+
            cm(q.m)+(q.sig?"":" \\u2014 <i>inside its own margin</i>");
       }
       tip.style("opacity",1).style("left",Math.min(e.clientX-r.left+14,W-290)+"px")
          .style("top",(e.clientY-r.top-10)+"px").html(h);
     })
     .on("mouseleave",function(){tip.style("opacity",0);});
    if (label!=="") g.append("text").attr("x",x+CS/2-0.5).attr("y",y+CS/2+3)
      .attr("text-anchor","middle").attr("font-size","9.5px")
      .attr("pointer-events","none")
      .attr("fill",(net?netdark:dark)?"#FFFFFF":"#12181D").text(label);
  }
}
bar.append("button")
  .attr("style","padding:4px 10px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text("show the net instead")
  .on("click",function(){net=!net;
    d3.select(this).text(net?"show the gross flows again":"show the net instead");
    note.text(net?"net flow, blank where it is smaller than its own margin of error"
                 :"thousands of people, row to column, log shading");
    draw();});
const note=bar.append("span").attr("style","color:#8A8F94")
  .text("thousands of people, row to column, log shading");
draw();
})();
</script>'))

## ---- nettab
z <- gd[order(-gd$per1k), ]
data.frame(
  Division = z$group,
  Arrived = n(z$in_est), Left = n(z$out_est),
  Net = paste0(ifelse(z$net > 0, "+", ""), n(z$net)),
  Net_per_1000 = formatC(z$per1k, format = "f", digits = 2),
  House_seats_2020 = z$seats)

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#mtx text[fill="#12181d" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')
