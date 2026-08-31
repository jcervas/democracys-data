# pie-radar-code.R -- chunk bodies for pie-radar-brief.Rmd
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

ca <- read.csv("data/derived/candidates.csv", stringsAsFactors = FALSE)
du <- read.csv("data/derived/duplicates.csv", stringsAsFactors = FALSE)
ft <- read.csv("data/derived/featured.csv",   stringsAsFactors = FALSE)
ar <- read.csv("data/derived/areas.csv",      stringsAsFactors = FALSE)
lk <- read.csv("data/derived/lookalikes.csv", stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",      stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
dl <- function(x) paste0("$", n(x))
mn <- function(x) paste0("$", p1(as.numeric(x) / 1e6), "m")
bn <- function(x) paste0("$", p2(as.numeric(x) / 1e9), " billion")

KEY <- c("indiv_contrib", "pac_contrib", "party_contrib", "self_funding",
         "trans_from_auth", "other")
LAB <- c("From individuals", "From PACs", "From the party",
         "The candidate's own money", "Transferred in", "Everything else")
names(LAB) <- KEY
COL <- c(indiv_contrib = "#1C4C5C", pac_contrib = "#C41230",
         party_contrib = "#D98324", self_funding = "#6B4A87",
         trans_from_auth = "#4E9DB5", other = "#8A8F94")
GRY <- "#8A8F94"; WARN <- "#C41230"; ACC <- "#1C4C5C"

# a shorter name for a figure label: the surname only, title-cased. The FEC
# files are upper case throughout and some surnames are two words, so this
# capitalises every word rather than only the first letter of the string.
nice <- function(x) {
  x <- sub(",.*$", "", x)
  gsub("(^|[ -])(\\w)", "\\1\\U\\2", tolower(x), perl = TRUE)
}

NROWS <- fn("cycle_rows"); NKEPT <- fn("kept_rows")
NCAT <- fn("categories"); NORD <- fn("orderings"); NCAND <- fn("candidates")
DG <- fn("dup_groups"); DR <- fn("dup_rows"); DX <- fn("dup_extra")
DBIG <- f("dup_biggest"); DBIGT <- fn("dup_biggest_total")
DA <- f("dup_a"); DB <- f("dup_b")
DONCE <- fn("dup_money_once"); DFILED <- fn("dup_money_as_filed")
DOVER <- fn("dup_overcount")
DW <- f("dup_widest"); DWI <- fn("dup_widest_ids"); DWT <- fn("dup_widest_total")
NEG <- fn("negative_indiv"); NEGMIN <- fn("negative_indiv_min")
MINR <- fn("min_receipts")
LA <- f("look_a"); LAT <- fn("look_a_total")
LB <- f("look_b"); LBT <- fn("look_b_total")
LRAT <- fn("look_ratio"); LGAP <- fn("look_gap")
EVEN <- f("even_name"); ERAT <- fn("even_ratio")
EMIN <- fn("even_min"); EMAX <- fn("even_max")
MAXR <- fn("max_ratio"); MAXRN <- f("max_ratio_name")
MEDOTH <- fn("median_other_pct"); MAXOTH <- fn("max_other_pct")
MAXOTHN <- f("max_other_name")

shares <- function(name) {
  z <- ft[ft$cand_name == name, ]
  if (!nrow(z)) z <- lk[lk$cand_name == name, ]
  setNames(as.numeric(z[, paste0("p_", KEY)]), KEY)
}
dollars <- function(name) {
  z <- ft[ft$cand_name == name, ]
  if (!nrow(z)) z <- lk[lk$cand_name == name, ]
  setNames(as.numeric(z[, KEY]), KEY)
}

# --- radar geometry, shared by both figures ----------------------------------
#
# n axes at equal angles, clockwise from twelve o'clock. The area of the
# polygon is the identity the chapter argues from; build-data.R checks it
# against the shoelace formula on 200 random radii before writing areas.csv.
axpt <- function(i, n, r) {
  a <- 2 * pi * (i - 1) / n
  c(r * sin(a), r * cos(a))
}
poly_area <- function(r) {
  m <- length(r)
  0.5 * sin(2 * pi / m) * sum(r * r[c(2:m, 1)])
}

# Strings on their way into a <script>. The category labels carry an apostrophe
# and the R strings around the payload are single-quoted, so the apostrophe is
# replaced by the typographic one -- as an actual character, not as a "\u2019"
# escape, which the backslash rule on the next line would then double into a
# visible \\u2019 in the figure.
esc <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("'", "\u2019", x, fixed = TRUE)
  gsub('"', '\\"', x, fixed = TRUE)
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
z <- lk[1, ]
data.frame(
  Source = unname(LAB[KEY]),
  Dollars = dl(as.numeric(z[, KEY])),
  Share = paste0(p1(as.numeric(z[, paste0("p_", KEY)])), "%"))

## ---- varmap
data.frame(
  Column = c("cand_id", "cand_name", "ttl_receipts", "indiv_contrib",
             "pac_contrib", "party_contrib", "self_funding",
             "trans_from_auth", "other"),
  What_it_holds = c(
    "the FEC's identifier for a candidacy, not for a person",
    "the candidate's name as filed, in whatever form the committee used",
    "everything the committee took in during the cycle",
    "money from people, in their own names",
    "money from political action committees",
    "money from the candidate's own party committee",
    "the candidate's own money: contributions plus loans, added here",
    "money moved in from another committee the candidate controls",
    "the receipts left over, computed here as the total minus the five above"),
  Measurement = c("identifier", "text", "continuous", "continuous",
                  "continuous", "continuous", "continuous", "continuous",
                  "continuous"))

## ---- duptab
z <- du[du$group %in% du$group[du$ttl_receipts >= 5e6], ]
z <- z[order(-z$ttl_receipts, z$cand_name), ]
z <- z[z$group %in% unique(z$group)[1:5], ]
data.frame(
  Candidate_ID = z$cand_id, Name = z$cand_name, Office = z$office,
  State = z$office_st, Receipts = dl(z$ttl_receipts))

## ---- fig1-static
op <- par(mfrow = c(2, 1), mar = c(0.4, 0.4, 1.6, 0.4))
# --- the two pies -----------------------------------------------------------
plot(NA, xlim = c(-1, 5.2), ylim = c(-1.35, 1.35), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
for (q in 1:2) {
  s <- shares(lk$cand_name[q]) / 100
  cx <- (q - 1) * 2.6
  a0 <- 0
  for (k in KEY) {
    a1 <- a0 + 2 * pi * s[k]
    aa <- seq(a0, a1, length.out = 64)
    polygon(c(cx, cx + sin(aa)), c(0, cos(aa)), col = COL[k], border = "#FFFFFF",
            lwd = 0.6)
    a0 <- a1
  }
  text(cx, 1.22, nice(lk$cand_name[q]), cex = 0.72)
}
legend(4.0, 1.0, unname(LAB[KEY]), fill = COL[KEY], border = NA, bty = "n",
       cex = 0.56, xpd = NA)
mtext("The same numbers as pies", 3, line = 0.2, cex = 0.78, adj = 0)
# --- the same numbers as bars -----------------------------------------------
par(mar = c(3.6, 8.6, 2.0, 1.4), mgp = c(2.1, 0.55, 0))
m <- rbind(dollars(lk$cand_name[1]), dollars(lk$cand_name[2])) / 1e6
# barplot recycles `col` across the bars in drawing order -- within a group,
# then on to the next group -- so a length-12 vector colours by CATEGORY and
# distinguishes the two campaigns by shade within each pair.
cols <- as.vector(rbind(COL[KEY], adjustcolor(COL[KEY], alpha.f = 0.38)))
bp <- barplot(m, beside = TRUE, horiz = TRUE, col = cols, border = NA,
              axes = FALSE, names.arg = rep("", ncol(m)), xlim = c(0, max(m) * 1.02))
axis(1, cex.axis = 0.72, lwd = 0, lwd.ticks = 1)
mtext("Millions of dollars", 1, line = 1.9, cex = 0.7)
axis(2, at = colMeans(bp), labels = unname(LAB[KEY]), las = 1, tick = FALSE,
     cex.axis = 0.62, line = -0.4)
mtext(paste0("The same numbers as bars, in dollars. Solid: ",
             nice(lk$cand_name[1]), ", ", mn(lk$ttl_receipts[1]),
             ". Pale: ", nice(lk$cand_name[2]), ", ",
             mn(lk$ttl_receipts[2]), "."),
      3, line = 0.4, cex = 0.66, adj = 0)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Two pies, and the same numbers as bars on a shared dollar axis. The button
# is the argument: the pies are drawn from SHARES and are therefore identical,
# while the bars are drawn from DOLLARS and are not.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
pay <- vapply(seq_len(nrow(lk)), function(q) {
  paste0('{n:"', esc(nice(lk$cand_name[q])), '",full:"', esc(lk$cand_name[q]),
         '",tot:', lk$ttl_receipts[q],
         ',v:[', paste(lk[q, KEY], collapse = ","),
         '],s:[', paste(round(lk[q, paste0("p_", KEY)], 3), collapse = ","), ']}')
}, character(1))
cat(paste0('
<div id="pie" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const C=[', paste(pay, collapse = ","), '];
const L=[', paste0('"', esc(LAB[KEY]), '"', collapse = ","), '];
const K=[', paste0('"', COL[KEY], '"', collapse = ","), '];
const W=760,H=380;
const box=d3.select("#pie");
const bar=box.append("div").attr("style","margin:0 0 8px;display:flex;align-items:center;gap:12px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const g=svg.append("g");
const cm=d3.format(",");
const usd=v=>"$"+cm(Math.round(v));
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;border-radius:3px;padding:7px 9px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
function show(e,html){
  const r=box.node().getBoundingClientRect();
  tip.style("opacity",1).style("left",Math.min(e.clientX-r.left+14,W-250)+"px")
     .style("top",(e.clientY-r.top-10)+"px").html(html);
}
let mode="pie";
function drawPie(){
  const R=118, CY=176;
  C.forEach(function(c,q){
    const cx=200+q*330;
    const arc=d3.arc().innerRadius(0).outerRadius(R);
    const pie=d3.pie().sort(null).value(d=>d)(c.s);
    g.append("g").attr("transform","translate("+cx+","+CY+")")
     .selectAll("path").data(pie).join("path")
     .attr("d",arc).attr("fill",(d,i)=>K[i])
     .attr("stroke","#FAFBFB").attr("stroke-width",1)
     .on("mousemove",function(e,d){
       show(e,"<b>"+c.n+"</b><br>"+L[d.index]+"<br>"+
              d3.format(".1f")(c.s[d.index])+"% of receipts");
     }).on("mouseleave",function(){tip.style("opacity",0);});
    g.append("text").attr("x",cx).attr("y",CY-R-14).attr("text-anchor","middle")
     .attr("font-size","13px").attr("fill","currentColor").text(c.n);
  });
  g.append("text").attr("x",20).attr("y",H-14).attr("font-size","11px")
   .attr("fill","#8A8F94")
   .text("Drawn from shares. Neither circle carries a total.");
}
function drawBars(){
  const M={t:26,r:22,b:38,l:186};
  const x=d3.scaleLinear().domain([0,d3.max(C,c=>d3.max(c.v))])
            .range([M.l,W-M.r]);
  const y=d3.scaleBand().domain(d3.range(L.length)).range([M.t,H-M.b])
            .paddingInner(0.32);
  const yy=d3.scaleBand().domain([0,1]).range([0,y.bandwidth()]).paddingInner(0.18);
  g.append("g").attr("transform","translate(0,"+(H-M.b)+")")
   .call(d3.axisBottom(x).ticks(5).tickFormat(d=>"$"+d3.format("~s")(d)));
  L.forEach(function(s,i){
    g.append("text").attr("x",M.l-10).attr("y",y(i)+y.bandwidth()/2+4)
     .attr("text-anchor","end").attr("font-size","11px")
     .attr("fill","currentColor").text(s);
    C.forEach(function(c,q){
      g.append("rect").attr("x",M.l).attr("y",y(i)+yy(q))
       .attr("width",Math.max(0,x(c.v[i])-M.l)).attr("height",yy.bandwidth())
       .attr("fill",K[i]).attr("fill-opacity",q===0?1:0.42)
       .on("mousemove",function(e){
         show(e,"<b>"+c.n+"</b><br>"+s+"<br>"+usd(c.v[i])+
                "<br><span style=\\"color:#8A8F94\\">"+
                d3.format(".1f")(c.s[i])+"% of "+usd(c.tot)+"</span>");
       }).on("mouseleave",function(){tip.style("opacity",0);});
    });
  });
  const key=g.append("g").attr("transform","translate("+(M.l+8)+","+14+")");
  C.forEach(function(c,q){
    key.append("rect").attr("x",q*180).attr("y",-9).attr("width",9).attr("height",9)
       .attr("fill","#12181D").attr("fill-opacity",q===0?0.8:0.34);
    key.append("text").attr("x",q*180+14).attr("font-size","11px")
       .attr("fill","currentColor").text(c.n+" \\u00b7 "+usd(c.tot));
  });
}
function draw(){ g.selectAll("*").remove(); if(mode==="pie") drawPie(); else drawBars(); }
bar.append("button")
  .attr("style","padding:4px 11px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text("draw the same numbers as bars")
  .on("click",function(){mode=mode==="pie"?"bar":"pie";
    d3.select(this).text(mode==="pie"?"draw the same numbers as bars"
                                     :"put the pies back");
    note.text(mode==="pie"?"two campaigns, as shares of themselves"
                          :"the same two campaigns, in dollars");
    draw();});
const note=bar.append("span").attr("style","color:#8A8F94")
  .text("two campaigns, as shares of themselves");
draw();
})();
</script>'))

## ---- fig2-static
w <- EVEN
r <- shares(w) / 100
ordset <- ar[ar$cand_name == w, ]
ordset <- ordset[order(ordset$area), ]
pick <- ordset[round(seq(1, nrow(ordset), length.out = 6)), ]
op <- par(mfrow = c(2, 3), mar = c(0.6, 0.6, 2.2, 0.6))
for (i in seq_len(nrow(pick))) {
  o <- match(strsplit(pick$axes[i], " ")[[1]], KEY)
  rr <- r[o]
  plot(NA, xlim = c(-1.28, 1.28), ylim = c(-1.28, 1.28), asp = 1, axes = FALSE,
       xlab = "", ylab = "")
  for (g in c(0.25, 0.5, 0.75, 1)) {
    p <- t(sapply(seq_along(rr), function(k) axpt(k, length(rr), g)))
    polygon(p[, 1], p[, 2], border = "#00000018", lwd = 0.6)
  }
  for (k in seq_along(rr)) {
    e <- axpt(k, length(rr), 1)
    segments(0, 0, e[1], e[2], col = "#00000020", lwd = 0.6)
  }
  p <- t(sapply(seq_along(rr), function(k) axpt(k, length(rr), rr[k] / max(r))))
  polygon(p[, 1], p[, 2], col = adjustcolor(ACC, alpha.f = 0.34),
          border = ACC, lwd = 1.4)
  # vertices coloured by CATEGORY, so a reader can see the same six sources
  # landing on different spokes from panel to panel
  points(p[, 1], p[, 2], pch = 16, cex = 0.8, col = COL[KEY][o])
  mtext(paste0("area ", formatC(pick$area[i], format = "f", digits = 4)),
        3, line = 0.5, cex = 0.66)
  mtext(paste0("ordering ", pick$ordering[i], " of ", NORD), 3, line = -0.3,
        cex = 0.54, col = "#4E5A63")
}
par(op)

## ---- fig2-d3
# ---------------------------------------------------------------------------
# One candidate, six axes, and a button that reorders the axes.
#
# THE AREAS ARE NOT COMPUTED HERE. build-data.R enumerates all 60 distinct
# cyclic orderings of six axes for each featured candidate and writes the area
# of every one, having first checked the closed form
#
#     A = 1/2 sin(2 pi / n) sum_i r_i r_(i+1)
#
# against the shoelace formula on 200 random radii. This chunk reads that file,
# so the number under the polygon and the strip along the bottom come from the
# same place and cannot drift apart.
# ---------------------------------------------------------------------------
cpay <- vapply(seq_len(nrow(ft)), function(q) {
  z <- ar[ar$cand_name == ft$cand_name[q], ]
  z <- z[order(z$ordering), ]
  ords <- vapply(z$axes, function(s)
    paste0("[", paste(match(strsplit(s, " ")[[1]], KEY) - 1, collapse = ","), "]"),
    character(1))
  paste0('{n:"', esc(nice(ft$cand_name[q])), '",role:"', esc(ft$role[q]),
         '",tot:', ft$ttl_receipts[q],
         ',v:[', paste(ft[q, KEY], collapse = ","),
         '],s:[', paste(round(ft[q, paste0("p_", KEY)], 4), collapse = ","),
         '],ord:[', paste(ords, collapse = ","),
         '],area:[', paste(z$area, collapse = ","), ']}')
}, character(1))
cat(paste0('
<div id="rad" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[', paste(cpay, collapse = ","), '];
const L=[', paste0('"', esc(LAB[KEY]), '"', collapse = ","), '];
const K=[', paste0('"', COL[KEY], '"', collapse = ","), '];
const W=760,H=470,CX=250,CY=220,R=150,SY=392;
const box=d3.select("#rad");
const bar=box.append("div").attr("style","margin:0 0 6px;display:flex;align-items:center;gap:6px;font:12px inherit;flex-wrap:wrap");
const bar2=box.append("div").attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const g=svg.append("g");
const cm=d3.format(",");
const P=(i,n,r)=>[CX+R*r*Math.sin(2*Math.PI*i/n),CY-R*r*Math.cos(2*Math.PI*i/n)];
// the sparse profiles have areas near 1e-5, which four decimal places print as
// 0.0000 -- a number the figure would then appear to be claiming
const fmtA=v=>v<0.001?d3.format(".1e")(v):d3.format(".4f")(v);
let cur=0, oi=0, byMax=true;
const btns=bar.selectAll("button").data(C).join("button")
  .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text(d=>d.n).on("click",function(e,d){cur=C.indexOf(d);oi=0;paint();draw();});
const shuffle=bar2.append("button")
  .attr("style","padding:4px 11px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text("reorder the axes")
  .on("click",function(){oi=(oi+1)%C[cur].ord.length;draw();});
const scaleb=bar2.append("button")
  .attr("style","padding:4px 11px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#FAFBFB;color:#12181D")
  .text("put every axis on one dollar scale")
  .on("click",function(){byMax=!byMax;
    d3.select(this).text(byMax?"put every axis on one dollar scale"
                              :"scale each axis to its own maximum");
    draw();});
const read=bar2.append("span").attr("style","color:#4E5A63;font-variant-numeric:tabular-nums");
function paint(){
  btns.attr("style",(d,i)=>"padding:3px 9px;border:1px solid "+
    (i===cur?"#1C4C5C":"#CBD3D8")+";border-radius:3px;cursor:pointer;font:11.5px inherit;background:"+
    (i===cur?"#1C4C5C":"#FAFBFB")+";color:"+(i===cur?"#FFFFFF":"#12181D"));
}
function draw(){
  g.selectAll("*").remove();
  const c=C[cur], o=c.ord[oi], n=o.length;
  const denom=byMax?Math.max.apply(null,c.s):100;
  [0.25,0.5,0.75,1].forEach(function(q){
    const pts=d3.range(n).map(i=>P(i,n,q));
    g.append("polygon").attr("points",pts.map(p=>p.join(",")).join(" "))
     .attr("fill","none").attr("stroke","currentColor").attr("stroke-opacity",0.12);
  });
  d3.range(n).forEach(function(i){
    const e=P(i,n,1);
    g.append("line").attr("x1",CX).attr("y1",CY).attr("x2",e[0]).attr("y2",e[1])
     .attr("stroke","currentColor").attr("stroke-opacity",0.16);
    const t=P(i,n,1.16);
    g.append("text").attr("x",t[0]).attr("y",t[1]).attr("dy","0.34em")
     .attr("text-anchor",Math.sin(2*Math.PI*i/n)>0.2?"start":
                        (Math.sin(2*Math.PI*i/n)<-0.2?"end":"middle"))
     .attr("font-size","10.5px").attr("fill",K[o[i]]).text(L[o[i]]);
  });
  const pts=d3.range(n).map(i=>P(i,n,c.s[o[i]]/denom));
  g.append("polygon").attr("points",pts.map(p=>p.join(",")).join(" "))
   .attr("fill","#1C4C5C").attr("fill-opacity",0.32)
   .attr("stroke","#1C4C5C").attr("stroke-width",1.6);
  d3.range(n).forEach(function(i){
    g.append("circle").attr("cx",pts[i][0]).attr("cy",pts[i][1]).attr("r",3.6)
     .attr("fill",K[o[i]])
     .on("mousemove",function(e){
       const r=box.node().getBoundingClientRect();
       tip.style("opacity",1).style("left",Math.min(e.clientX-r.left+14,W-250)+"px")
          .style("top",(e.clientY-r.top-10)+"px")
          .html("<b>"+L[o[i]]+"</b><br>$"+cm(Math.round(c.v[o[i]]))+"<br>"+
                d3.format(".1f")(c.s[o[i]])+"% of receipts");
     }).on("mouseleave",function(){tip.style("opacity",0);});
  });
  // --- every ordering, as a strip -----------------------------------------
  const lo=Math.min.apply(null,c.area), hi=Math.max.apply(null,c.area);
  const sx=d3.scaleLinear().domain([lo,hi]).range([520,W-30]);
  g.append("text").attr("x",520).attr("y",CY-96).attr("font-size","11px")
   .attr("fill","currentColor").text("the area of this polygon");
  g.append("text").attr("x",520).attr("y",CY-80).attr("font-size","10.5px")
   .attr("fill","#8A8F94").text("under all "+c.area.length+" orderings");
  c.area.forEach(function(a,i){
    g.append("line").attr("x1",sx(a)).attr("x2",sx(a))
     .attr("y1",CY-66).attr("y2",CY-46)
     .attr("stroke",i===oi?"#C41230":"currentColor")
     .attr("stroke-opacity",i===oi?1:0.22)
     .attr("stroke-width",i===oi?2:1);
  });
  [[lo,"start"],[hi,"end"]].forEach(function(q,i){
    g.append("text").attr("x",sx(q[0])).attr("y",CY-32)
     .attr("text-anchor",q[1]).attr("font-size","10px").attr("fill","#8A8F94")
     .text(fmtA(q[0]));
  });
  g.append("text").attr("x",520).attr("y",CY+6).attr("font-size","12px")
   .attr("fill","currentColor")
   .text("area "+fmtA(c.area[oi]));
  g.append("text").attr("x",520).attr("y",CY+24).attr("font-size","10.5px")
   .attr("fill","#8A8F94").text("ordering "+(oi+1)+" of "+c.area.length);
  g.append("text").attr("x",520).attr("y",CY+48).attr("font-size","10.5px")
   .attr("fill","#8A8F94").text("largest \\u00f7 smallest = "+
     d3.format(".2f")(hi/lo)+"\\u00d7");
  g.append("text").attr("x",520).attr("y",CY+76).attr("font-size","11px")
   .attr("fill","currentColor").text("$"+cm(Math.round(c.tot))+" raised");
  g.append("text").attr("x",520).attr("y",CY+92).attr("font-size","10.5px")
   .attr("fill","#8A8F94").text(c.role);
  read.html("<b>"+c.n+"</b> \\u00b7 "+(byMax?"each axis to its own maximum"
                                            :"one dollar scale for every axis"));
}
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;border-radius:3px;padding:7px 9px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
paint(); draw();
})();
</script>'))

## ---- arearange
z <- do.call(rbind, lapply(ft$cand_name, function(w) {
  a <- ar$area[ar$cand_name == w]
  s <- shares(w)
  data.frame(Campaign = nice(w),
             Profile = ft$role[ft$cand_name == w],
             Smallest_area = formatC(min(a), format = "f", digits = 4),
             Largest_area = formatC(max(a), format = "f", digits = 4),
             Ratio = paste0(formatC(max(a) / min(a), format = "f",
                                    digits = if (max(a) / min(a) > 100) 0 else 2),
                            "×"),
             stringsAsFactors = FALSE)
}))
z
