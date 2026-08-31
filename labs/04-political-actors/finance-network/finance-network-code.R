# finance-network-code.R -- chunk bodies for finance-network-brief.Rmd
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

ed <- read.csv("data/derived/edges.csv",       stringsAsFactors = FALSE)
cm <- read.csv("data/derived/committees.csv",  stringsAsFactors = FALSE)
cd <- read.csv("data/derived/candidates.csv",  stringsAsFactors = FALSE)
gn <- read.csv("data/derived/graph_nodes.csv", stringsAsFactors = FALSE)
ge <- read.csv("data/derived/graph_edges.csv", stringsAsFactors = FALSE)
id <- read.csv("data/derived/identity.csv",    stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",       stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
usd <- function(x) paste0("$", n(x))
bn  <- function(x) paste0("$", formatC(as.numeric(x) / 1e9, format = "f",
                                       digits = 2), "bn")
mn  <- function(x) paste0("$", formatC(as.numeric(x) / 1e6, format = "f",
                                       digits = 1), "m")

NEDGE <- fn("edges"); NCOMM <- fn("committees"); NCAND <- fn("candidates")
NOID  <- fn("noid_rows"); NOIDU <- fn("noid_usd"); NOIDP <- fn("noid_pct")
CLEAN <- fn("clean_rows"); CLEANU <- fn("clean_usd"); DROP <- fn("dropped")
WIDE  <- f("widest_committee"); WIDEN <- fn("widest_n")
SLN   <- fn("slice_nodes"); SLE <- fn("slice_edges"); SLP <- fn("slice_pct")
BIGC  <- fn("big_committees"); BIGA <- fn("big_attack")

COMM <- "#54278F"; CAND <- "#2c7fb8"; OPP <- "#C41230"; SUP <- "#4d9221"
GRY  <- "#8A8F94"; ACC <- "#1C4C5C"

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
o <- ed[order(-ed$amount), ][1:3, ]
data.frame(
  Spender_id = o$sid,
  Candidate_id = o$cid,
  Side = ifelse(o$side == "O", "against", "for"),
  Amount = usd(o$amount))

## ---- varmap
data.frame(
  Column = c("sid", "cid", "side", "amount"),
  What_it_holds = c(
    "the FEC identifier of the committee that spent the money",
    "the FEC identifier of the candidate it was spent on",
    "S for supporting, O for opposing — written by the filer",
    "dollars, summed over every filing for that pair and side"),
  Measurement = c("categorical", "categorical", "dichotomous", "continuous"))

## ---- idtab
data.frame(
  Rule = id$rule,
  Side = id$side,
  Nodes_it_produces = n(id$nodes))

## ---- harris
h <- cd[grepl("^HARRIS, KAMAL", cd$name) & cd$office == "P", ]
h <- h[order(-h$total), ]
data.frame(
  FEC_candidate_id = h$cid,
  Name_as_filed = h$name,
  Committees = h$committees,
  Money_for_or_against = usd(h$total))

## ---- noid
data.frame(
  Rows = c(n(CLEAN), n(NOID)),
  Share = c("100%", paste0(p1(NOIDP), "%")),
  Dollars = c(bn(CLEANU), mn(NOIDU)),
  Which = c("expenditures after the sixteen junk filings are removed",
            "of those, the ones naming no candidate identifier"))

## ---- fig1-static
op <- par(mar = c(0.4, 0.4, 0.4, 0.4))
plot(NA, xlim = c(140, 900), ylim = c(610, 10), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
ix <- setNames(seq_len(nrow(gn)), gn$id)
for (i in seq_len(nrow(ge))) {
  a <- ix[ge$source[i]]; b <- ix[ge$target[i]]
  segments(gn$x[a], gn$y[a], gn$x[b], gn$y[b],
           col = paste0(ifelse(ge$mostly_against[i], OPP, SUP), "44"),
           lwd = 0.4 + 2.2 * sqrt(ge$amount[i] / max(ge$amount)))
}
r <- 2.6 + 7 * sqrt(gn$total / max(gn$total))
points(gn$x[gn$kind == "candidate"], gn$y[gn$kind == "candidate"],
       pch = 19, cex = r[gn$kind == "candidate"] / 3.2, col = paste0(CAND, "CC"))
points(gn$x[gn$kind == "committee"], gn$y[gn$kind == "committee"],
       pch = 15, cex = r[gn$kind == "committee"] / 3.2, col = paste0(COMM, "DD"))
lab <- gn[gn$kind == "committee", ]
lab <- lab[order(-lab$total), ][1:8, ]
text(lab$x, lab$y - 12, substr(lab$name, 1, 22), cex = 0.52, col = "#12181D")
legend(140, 40, c("committee", "candidate", "mostly against", "mostly for"),
       pch = c(15, 19, NA, NA), lty = c(NA, NA, 1, 1), lwd = c(NA, NA, 2, 2),
       col = c(COMM, CAND, OPP, SUP), bty = "n", cex = 0.68, ncol = 2)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The layout is NOT computed here. build-data.R solves it once with a fixed
# seed and writes the coordinates, so print and screen show the same
# arrangement -- and so that a reader who opens this twice sees the same
# picture. The simulation below starts from those positions with almost no
# energy, which means it does nothing until a node is dragged, and then it
# behaves like the physical object the layout implies.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
nd <- paste0('{id:"', gn$id, '",k:"', substr(gn$kind, 1, 4), '",nm:"',
             gsub('"', "'", gn$name, fixed = TRUE), '",t:', round(gn$total),
             ',pa:', ifelse(is.na(gn$pct_against), 0, gn$pct_against),
             ',d:', gn$degree, ',x:', gn$x, ',y:', gn$y, '}', collapse = ",")
lk <- paste0('{source:"', ge$source, '",target:"', ge$target, '",v:',
             round(ge$amount), ',o:', ifelse(ge$mostly_against, 1, 0), '}',
             collapse = ",")
cat(paste0('
<div id="net" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const N=[', nd, '];
const L=[', lk, '];
const COMM="', COMM, '", CAND="', CAND, '", OPP="', OPP, '", SUP="', SUP, '";
const W=960,H=640;
const box=d3.select("#net");
const bar=box.append("div")
  .attr("style","margin:0 0 6px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit;cursor:grab");
const maxT=d3.max(N,d=>d.t), maxV=d3.max(L,d=>d.v);
const rad=d=>2.5+11*Math.sqrt(d.t/maxT);
const byId={}; N.forEach(d=>byId[d.id]=d);
L.forEach(l=>{l.source=byId[l.source]; l.target=byId[l.target];});
const link=svg.append("g").selectAll("line").data(L).join("line")
  .attr("stroke",d=>d.o?OPP:SUP).attr("stroke-opacity",0.3)
  .attr("stroke-width",d=>0.5+3*Math.sqrt(d.v/maxV));
const node=svg.append("g").selectAll("g").data(N).join("g");
node.append("path")
  .attr("d",d=>d.k==="comm"
    ? d3.symbol().type(d3.symbolSquare).size(Math.PI*rad(d)*rad(d)*1.1)()
    : d3.symbol().type(d3.symbolCircle).size(Math.PI*rad(d)*rad(d))())
  .attr("fill",d=>d.k==="comm"?COMM:CAND).attr("fill-opacity",0.85)
  .attr("stroke","#fff").attr("stroke-width",0.8);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;max-width:240px;'
, 'box-shadow:0 1px 4px rgba(0,0,0,.14)");
const usd=v=>v>=1e6?("$"+(v/1e6).toFixed(1)+"m"):("$"+d3.format(",.0f")(v));
function highlight(d){
  const keep=new Set([d.id]);
  L.forEach(l=>{ if(l.source.id===d.id) keep.add(l.target.id);
                 if(l.target.id===d.id) keep.add(l.source.id); });
  node.attr("opacity",q=>keep.has(q.id)?1:0.12);
  link.attr("stroke-opacity",l=>
    (l.source.id===d.id||l.target.id===d.id)?0.85:0.04);
}
function clear(){ node.attr("opacity",1); link.attr("stroke-opacity",0.3); }
node.on("mousemove",function(e,d){
    highlight(d);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.nm+"</b><br>"+(d.k==="comm"?"committee":"candidate")+
             "<br>"+usd(d.t)+" total<br>"+d.d+" connection"+(d.d===1?"":"s")+
             "<br>"+d.pa.toFixed(0)+"% against");
  })
  .on("mouseleave",function(){clear();tip.style("opacity",0);});
// the simulation exists so that dragging behaves like the physics the layout
// came from; it starts cold, so nothing moves until the reader moves it
const sim=d3.forceSimulation(N)
  .force("link",d3.forceLink(L).id(d=>d.id).distance(46).strength(0.35))
  .force("charge",d3.forceManyBody().strength(-90))
  .force("collide",d3.forceCollide().radius(d=>rad(d)+2))
  .alpha(0).alphaTarget(0).stop();
function paint(){
  link.attr("x1",d=>d.source.x).attr("y1",d=>d.source.y)
      .attr("x2",d=>d.target.x).attr("y2",d=>d.target.y);
  node.attr("transform",d=>"translate("+d.x+","+d.y+")");
}
sim.on("tick",paint); paint();
node.style("cursor","grab").call(d3.drag()
  .on("start",function(e,d){ if(!e.active) sim.alphaTarget(0.25).restart();
                             d.fx=d.x; d.fy=d.y; })
  .on("drag", function(e,d){ d.fx=e.x; d.fy=e.y; })
  .on("end",  function(e,d){ if(!e.active) sim.alphaTarget(0);
                             d.fx=null; d.fy=null; }));
// labels for the largest committees, drawn last so they sit on top
const lab=N.filter(d=>d.k==="comm").sort((a,b)=>b.t-a.t).slice(0,8);
svg.append("g").selectAll("text").data(lab).join("text")
  .attr("x",d=>d.x).attr("y",d=>d.y-rad(d)-4).attr("text-anchor","middle")
  // currentColor, not a literal: this label has to stay legible whether the
  // reader is on a light ground or a dark one
  .attr("font-size","10.5px").attr("fill","currentColor")
  .attr("pointer-events","none")
  .text(d=>d.nm.length>24?d.nm.slice(0,22)+"\\u2026":d.nm);
sim.on("tick.lab",function(){
  svg.selectAll("text").filter(function(){return d3.select(this).datum()&&d3.select(this).datum().k==="comm";})
     .attr("x",d=>d.x).attr("y",d=>d.y-rad(d)-4);
});
[["committee",COMM],["candidate",CAND]].forEach(function(s,i){
  bar.append("span").html("<span style=\\"color:"+s[1]+"\\">&#9632;</span> "+s[0]);
});
[["mostly against",OPP],["mostly for",SUP]].forEach(function(s){
  bar.append("span").html("<span style=\\"color:"+s[1]+"\\">&#9473;</span> "+s[0]);
});
bar.append("span").attr("style","color:#76838C").text("drag a node");
})();
</script>'))

## ---- fig2-static
op <- par(mfrow = c(1, 2), mar = c(4.0, 4.2, 2.4, 0.8), mgp = c(2.5, 0.7, 0))
for (z in list(list(cm$candidates, COMM, "committees", "candidates it spent on"),
               list(cd$committees, CAND, "candidates", "committees that spent on it"))) {
  tb <- table(pmin(z[[1]], 60))
  plot(as.numeric(names(tb)), as.numeric(tb), log = "xy", pch = 19, cex = 0.6,
       col = z[[2]], axes = FALSE, xlab = "", ylab = "")
  axis(1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
  axis(2, las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
  mtext(z[[4]], 1, line = 2.3, cex = 0.8)
  mtext(paste("number of", z[[3]]), 2, line = 2.6, cex = 0.8)
  mtext(paste(z[[3]], "by degree"), 3, line = 0.8, cex = 0.86, font = 2, adj = 0)
}
par(op)

## ---- fig2-d3
# Degree on both sides of the graph, on log axes because the tail is the point.
# Hovering names the committees or candidates that sit at a given degree, which
# is the thing a bare power-law plot never tells you.
mkdeg <- function(v, nm) {
  tb <- table(v)
  paste0('{d:', names(tb), ',n:', as.integer(tb), ',ex:"',
         vapply(as.integer(names(tb)), function(k) {
           z <- head(nm[v == k], 3)
           gsub('"', "'", paste(substr(z, 1, 34), collapse = "; "), fixed = TRUE)
         }, character(1)), '"}', collapse = ",")
}
cat(paste0('
<div id="deg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const A=[', mkdeg(cm$candidates, cm$name), '];
const B=[', mkdeg(cd$committees, cd$name), '];
const COMM="', COMM, '", CAND="', CAND, '";
const W=770,H=380,PW=W/2,M={t:36,r:20,b:52,l:58};
const box=d3.select("#deg");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;max-width:250px;'
, 'box-shadow:0 1px 4px rgba(0,0,0,.14)");
[[A,COMM,"committees","candidates it spent on"],
 [B,CAND,"candidates","committees that spent on it"]].forEach(function(P,pi){
  const D=P[0], col=P[1], ox=pi*PW;
  const g=svg.append("g").attr("transform","translate("+ox+",0)");
  const x=d3.scaleLog().domain([1,d3.max(D,d=>d.d)*1.15]).range([M.l,PW-M.r]);
  const y=d3.scaleLog().domain([0.8,d3.max(D,d=>d.n)*1.3]).range([H-M.b,M.t]);
  g.append("g").attr("transform","translate(0,"+(H-M.b)+")")
   .call(d3.axisBottom(x).ticks(4,"~s"));
  g.append("g").attr("transform","translate("+M.l+",0)")
   .call(d3.axisLeft(y).ticks(4,"~s"));
  g.append("text").attr("x",M.l).attr("y",20).attr("font-size","12px")
   .attr("font-weight","700").text(P[2]+" by degree");
  g.append("text").attr("x",(M.l+PW-M.r)/2).attr("y",H-14)
   .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
   .text(P[3]);
  g.selectAll("circle").data(D).join("circle")
   .attr("cx",d=>x(d.d)).attr("cy",d=>y(d.n)).attr("r",3.4)
   .attr("fill",col).attr("fill-opacity",0.8)
   .on("mousemove",function(e,d){
     d3.select(this).attr("r",6);
     const r=box.node().getBoundingClientRect();
     tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
        .style("top",(e.clientY-r.top-8)+"px")
        .html("<b>"+d.n+" "+P[2]+"</b> with "+d.d+" connection"+(d.d===1?"":"s")+
              "<br><span style=\\"color:#76838C\\">"+d.ex+"</span>");
   })
   .on("mouseleave",function(){d3.select(this).attr("r",3.4);tip.style("opacity",0);});
});
})();
</script>'))

## ---- attack
b <- cm[cm$total > 5e6, ]
b <- b[order(-b$pct_against), ][1:8, ]
data.frame(
  Committee = substr(b$name, 1, 44),
  Total = usd(b$total),
  Against = paste0(p1(b$pct_against), "%"),
  Candidates = b$candidates)

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe. The
# network labels never chose a colour, so both spellings of that are listed:
# no fill attribute at all, and a literal currentColor.
# LIGHT PAGE ONLY: on the dark page these labels ride currentColor to the
# light ink and already pass; a --paper stroke is only needed on the light
# page, where three of them cross dark nodes.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#net text:not([fill]),
#net text[fill="currentcolor" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
