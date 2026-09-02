# streamgraph-code.R -- chunk bodies for streamgraph-brief.Rmd
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

se <- read.csv("data/derived/series.csv",   stringsAsFactors = FALSE)
sm <- read.csv("data/derived/stream.csv",   stringsAsFactors = FALSE)
ar <- read.csv("data/derived/articles.csv", stringsAsFactors = FALSE)
ev <- read.csv("data/derived/events.csv",   stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",    stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
mn <- function(x) paste0(p1(as.numeric(x) / 1e6), "m")

se$date <- as.Date(se$date); sm$date <- as.Date(sm$date)
ev$date <- as.Date(ev$date)
DAYS <- sort(unique(se$date))

NART <- fn("articles"); NDAY <- fn("days"); NROW <- fn("rows")
TOTV <- fn("total_views"); PKD <- f("peak_day"); PKT <- fn("peak_day_total")
MEDT <- fn("median_day_total"); DRATIO <- fn("day_ratio")
BIG <- f("biggest"); BIGT <- fn("biggest_total")
SMALL <- f("smallest"); SMALLT <- fn("smallest_total"); SRATIO <- fn("size_ratio")
IMPC <- fn("imputed_cells")
WPK <- fn("walz_peak"); WPKD <- f("walz_peak_date"); NEV <- fn("events")

# a readable label for each article, in stacking order
nice <- function(a) gsub("_", " ", sub("_\\(United_States\\)", "", a))
ar$label <- nice(ar$article)
ORD <- ar$article

# a sequential ramp with twelve steps, so the bands read as an ordered set
RAMP <- colorRampPalette(c("#C9E3EA", "#6FAFC2", "#2E7C96", "#14495C",
                           "#8A3B2C", "#C08A2E"))(NART)
names(RAMP) <- ORD
ACC <- "#1C4C5C"; WARN <- "#C41230"; GRY <- "#8A8F94"

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
z <- se[se$date == as.Date(WPKD) &
        se$article %in% c("Tim_Walz", "Kamala_Harris", "Opinion_poll"), ]
data.frame(
  Date = as.character(z$date), Article = nice(z$article),
  Views = n(z$views))

## ---- varmap
data.frame(
  Column = c("date", "article", "views"),
  What_it_holds = c("the day, in 2024",
                    "one of twelve English Wikipedia articles",
                    "how many human readers opened it that day"),
  Measurement = c("discrete", "categorical", "count"))

## ---- arttab
z <- ar[order(-ar$total), ]
data.frame(
  Article = z$label,
  Total_views = n(z$total),
  Busiest_day = z$peak_date,
  Views_that_day = n(z$peak))

## ---- fig1-static
op <- par(mar = c(3.2, 0.6, 1.0, 8.8), mgp = c(2.2, 0.6, 0))
yr <- range(c(sm$y0, sm$y1))
plot(NA, xlim = range(DAYS), ylim = yr, axes = FALSE, xlab = "", ylab = "")
m1 <- as.Date(paste0("2024-", sprintf("%02d", seq(1, 12, 2)), "-01"))
axis(1, at = m1, labels = format(m1, "%b"), cex.axis = 0.76, lwd = 0,
     lwd.ticks = 1)
for (a in ORD) {
  z <- sm[sm$article == a, ]; z <- z[order(z$date), ]
  polygon(c(z$date, rev(z$date)), c(z$y1, rev(z$y0)), col = RAMP[a],
          border = NA)
}
for (i in seq_len(nrow(ev))) {
  segments(ev$date[i], yr[1], ev$date[i], yr[2], col = "#FFFFFF88", lwd = 0.7)
}
lab <- ar[order(-ar$total), ][1:5, ]
for (i in seq_len(nrow(lab))) {
  z <- sm[sm$article == lab$article[i] & sm$date == as.Date(PKD), ]
  text(max(DAYS) + 6, (z$y0 + z$y1) / 2, lab$label[i], adj = 0, cex = 0.6,
       col = RAMP[lab$article[i]], xpd = NA)
}
mtext("Wikipedia pageviews, twelve articles, 2024", 3, line = -0.2, cex = 0.8,
      adj = 0)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The layout is NOT computed here. build-data.R writes y0 and y1 for every
# article-day, so print and screen draw the same polygons. The baseline is
# CENTRED rather than d3's wiggle: both are streamgraphs, and a centred one is
# y0 = -total/2, which can be written identically in two languages.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
paths <- vapply(ORD, function(a) {
  z <- sm[sm$article == a, ]; z <- z[order(z$date), ]
  i <- as.integer(z$date - min(DAYS))
  paste0('{a:"', a, '",l:"', nice(a), '",c:"', RAMP[a],
         '",y0:[', paste(z$y0, collapse = ","),
         '],y1:[', paste(z$y1, collapse = ","),
         '],v:[', paste(z$views, collapse = ","), ']}')
}, character(1))
evs <- paste0('{d:', as.integer(ev$date - min(DAYS)), ',t:"',
              gsub('"', "'", ev$event, fixed = TRUE), '"}', collapse = ",")
cat(paste0('
<div id="stg" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', paste(paths, collapse = ","), '];
const EV=[', evs, '];
const ND=', NDAY, ', D0=new Date("2024-01-01T00:00:00Z");
const W=770,H=430,M={t:26,r:186,b:44,l:16};
const box=d3.select("#stg");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,ND-1]).range([M.l,W-M.r]);
const lo=d3.min(S,s=>d3.min(s.y0)), hi=d3.max(S,s=>d3.max(s.y1));
const y=d3.scaleLinear().domain([lo,hi]).range([H-M.b,M.t]);
const dayOf=i=>new Date(D0.getTime()+i*86400000);
const fmt=d3.utcFormat("%d %b");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(d3.range(0,ND,61))
          .tickFormat(i=>d3.utcFormat("%b")(dayOf(i))));
const area=d3.area().x((d,i)=>x(i)).y0((d,i)=>y(d[0])).y1((d,i)=>y(d[1]));
const bands=svg.append("g").selectAll("path").data(S).join("path")
  .attr("d",s=>area(s.y0.map((q,i)=>[q,s.y1[i]])))
  .attr("fill",s=>s.c);
svg.append("g").selectAll("line").data(EV).join("line")
  .attr("x1",d=>x(d.d)).attr("x2",d=>x(d.d)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#FFFFFF").attr("stroke-opacity",0.45).attr("stroke-width",0.8);
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-width",1).attr("opacity",0);
const key=svg.append("g").attr("transform","translate("+(W-M.r+12)+","+(M.t+4)+")");
const sorted=S.slice().sort((a,b)=>d3.sum(b.v)-d3.sum(a.v));
const rows=key.selectAll("g").data(sorted).join("g")
  .attr("transform",(d,i)=>"translate(0,"+i*15+")");
rows.append("rect").attr("width",9).attr("height",9).attr("y",-8)
  .attr("fill",d=>d.c);
const rlab=rows.append("text").attr("x",14).attr("font-size","10.5px")
  .attr("fill","currentColor").text(d=>d.l.length>20?d.l.slice(0,19)+"\\u2026":d.l);
const rval=rows.append("text").attr("x",W-M.r-26).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#76838C");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const cm=d3.format(",");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const i=Math.max(0,Math.min(ND-1,Math.round(x.invert(d3.pointer(e,this)[0]+M.l))));
    rule.attr("x1",x(i)).attr("x2",x(i)).attr("opacity",0.6);
    rval.text(d=>cm(d.v[i]));
    const near=EV.find(q=>Math.abs(q.d-i)<=1);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+fmt(dayOf(i))+"</b><br>"+
             cm(d3.sum(S,s=>s.v[i]))+" views that day"+
             (near?"<br><i>"+near.t+"</i>":""));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);
                              rval.text("");});
})();
</script>'))

## ---- fig2-static
op <- par(mar = c(3.2, 8.2, 1.2, 1.0), mgp = c(2.2, 0.6, 0))
o <- ar[order(-ar$total), ]
NB <- 4
plot(NA, xlim = range(DAYS), ylim = c(nrow(o) + 0.4, 0.2), axes = FALSE,
     xlab = "", ylab = "")
m1 <- as.Date(paste0("2024-", sprintf("%02d", seq(1, 12, 2)), "-01"))
axis(1, at = m1, labels = format(m1, "%b"), cex.axis = 0.76, lwd = 0,
     lwd.ticks = 1)
cols <- colorRampPalette(c("#D6E8EE", "#0F4557"))(NB)
for (i in seq_len(nrow(o))) {
  z <- se[se$article == o$article[i], ]; z <- z[order(z$date), ]
  h <- max(z$views) / NB                      # per-series band height
  for (b in seq_len(NB)) {
    v <- pmin(pmax(z$views - (b - 1) * h, 0), h) / h        # 0..1
    yb <- i + 0.34
    polygon(c(z$date, rev(z$date)), c(yb - v * 0.68, rep(yb, nrow(z))),
            col = cols[b], border = NA)
  }
  mtext(o$label[i], 2, at = i, las = 1, line = 0.4, cex = 0.62)
  text(max(DAYS), i - 0.28, n(max(z$views)), adj = 1, cex = 0.55,
       col = "#76838C")
}
par(op)

## ---- fig2-d3
# A horizon chart: each series gets one row, its own band height, and a colour
# ramp that folds the tall part of the series back down over the short part.
# The two controls are the two decisions -- how many bands, and whether every
# row is measured against its own maximum or against the largest series.
rows <- vapply(ar$article[order(-ar$total)], function(a) {
  z <- se[se$article == a, ]; z <- z[order(z$date), ]
  paste0('{a:"', a, '",l:"', nice(a), '",mx:', max(z$views),
         ',v:[', paste(z$views, collapse = ","), ']}')
}, character(1))
cat(paste0('
<div id="hz" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[', paste(rows, collapse = ","), '];
const ND=', NDAY, ', D0=new Date("2024-01-01T00:00:00Z");
const GMAX=d3.max(R,r=>r.mx);
const W=770,RH=34,M={t:16,r:64,b:40,l:150};
const H=M.t+M.b+R.length*RH;
const box=d3.select("#hz");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,ND-1]).range([M.l,W-M.r]);
const dayOf=i=>new Date(D0.getTime()+i*86400000);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(d3.range(0,ND,61))
          .tickFormat(i=>d3.utcFormat("%b")(dayOf(i))));
const g=svg.append("g");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const cm=d3.format(",");
const fmt=d3.utcFormat("%d %b");
let NB=4, own=true;
const lab=bar.append("span").attr("style","color:#4E5A63;min-width:64px");
bar.append("span").attr("style","color:#76838C").text("bands");
const sl=bar.append("input").attr("type","range").attr("min","1").attr("max","8")
  .attr("step","1").attr("value","4")
  .attr("style","flex:0 1 150px;accent-color:#1C4C5C");
const btn=bar.append("button")
  .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text("measure every row against the largest");
function draw(){
  NB=+sl.property("value");
  const ramp=d3.scaleLinear().domain([0,NB-1])
    .range(["#D6E8EE","#0F4557"]).interpolate(d3.interpolateRgb);
  g.selectAll("*").remove();
  R.forEach(function(r,i){
    const top=M.t+i*RH, base=top+RH-6;
    const h=(own?r.mx:GMAX)/NB;
    for (let b=0;b<NB;b++){
      const a=d3.area().x((d,j)=>x(j))
        .y0(base)
        .y1(d=>base-(Math.min(Math.max(d-b*h,0),h)/h)*(RH-8));
      g.append("path").attr("d",a(r.v)).attr("fill",ramp(b));
    }
    g.append("text").attr("x",M.l-10).attr("y",base-2).attr("text-anchor","end")
     .attr("font-size","11px").attr("fill","currentColor")
     .text(r.l.length>22?r.l.slice(0,21)+"\\u2026":r.l);
    g.append("text").attr("x",W-M.r+6).attr("y",base-2).attr("font-size","10px")
     .attr("fill","#76838C").text(d3.format(".2s")(own?r.mx:GMAX));
    g.append("rect").attr("x",M.l).attr("y",top).attr("width",W-M.r-M.l)
     .attr("height",RH-2).attr("fill","transparent")
     .on("mousemove",function(e){
       const j=Math.max(0,Math.min(ND-1,Math.round(x.invert(d3.pointer(e,this)[0]+M.l))));
       const rr=box.node().getBoundingClientRect();
       tip.style("opacity",1).style("left",(e.clientX-rr.left+14)+"px")
          .style("top",(e.clientY-rr.top-8)+"px")
          .html("<b>"+r.l+"</b><br>"+fmt(dayOf(j))+"<br>"+cm(r.v[j])+" views"+
                "<br><span style=\\"color:#76838C\\">band height "+
                cm(Math.round(h))+"</span>");
     })
     .on("mouseleave",function(){tip.style("opacity",0);});
  });
  lab.text(NB+(NB===1?" band":" bands"));
}
sl.on("input",draw);
btn.on("click",function(){
  own=!own;
  d3.select(this).text(own?"measure every row against the largest"
                          :"measure every row against itself");
  draw();
});
draw();
})();
</script>'))

## ---- band-read
# One band read at one date, by the numbers the figure was drawn from: the
# debate day, Harris's band edges and thickness, how many bands sit beneath
# hers, and the smallest series' thickness the same day. Also the total on
# the day Walz was named, for his band's share of the ribbon.
RD <- "2024-09-10"
rd <- sm[sm$date == as.Date(RD), ]; rd <- rd[order(rd$y0), ]
RD_TOT <- sum(se$views[se$date == as.Date(RD)])
hz <- rd[rd$article == "Kamala_Harris", ]
RD_Y0 <- hz$y0; RD_Y1 <- hz$y1; RD_V <- hz$views
RD_BELOW <- sum(rd$y0 < hz$y0)
RD_SV <- rd$views[rd$article == SMALL]
WD_TOT <- sum(se$views[se$date == as.Date(WPKD)])
