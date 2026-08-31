# age-structure-code.R -- chunk bodies for age-structure-brief.Rmd
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

bd <- read.csv("data/derived/bands.csv",  stringsAsFactors = FALSE)
sh <- read.csv("data/derived/shares.csv", stringsAsFactors = FALSE)
ck <- read.csv("data/derived/checks.csv", stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",  stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
mil <- function(x) paste0(p1(as.numeric(x) / 1000), "m")

BANDS <- c("18 to 24 Years", "25 to 44 Years", "45 to 64 Years",
           "65 Years and Over")
SHORT <- c("18–24", "25–44", "45–64", "65+")
# The PDF device has no en dash in its base font and substitutes one with a
# warning on every label, so the static figures use hyphens and the HTML ones
# keep the typographic dash.
SHORTP <- c("18-24", "25-44", "45-64", "65+")
LP    <- fn("last_pres"); FIRST <- fn("first"); LAST <- fn("last")
NEL   <- fn("elections")
YPOP  <- fn("y_pop"); YVOTE <- fn("y_vote"); YGAP <- fn("y_gap")
OPOP  <- fn("o_pop"); OVOTE <- fn("o_vote"); OGAP <- fn("o_gap")
YTURN <- fn("y_turn"); OTURN <- fn("o_turn"); RATIO <- fn("turn_ratio")
OPOP1 <- fn("o_pop_first"); OVOTE1 <- fn("o_vote_first")
VAPL  <- fn("vap_last"); VOTL <- fn("voters_last"); TURNL <- fn("turn_last")
YREG  <- fn("y_reg"); OREG <- fn("o_reg")

bd$band <- factor(bd$band, levels = c("Total", BANDS))
sh$band <- factor(sh$band, levels = BANDS)
PRES <- sort(unique(sh$year[sh$year %% 4 == 0]))
OGAP1 <- sh$gap[sh$year == min(PRES) & sh$band == "65 Years and Over"]

POPC <- "#8A8F94"; VOTC <- "#1C4C5C"; WARN <- "#C41230"
BCOL <- c("#C08A2E", "#4d9221", "#2c7fb8", "#54278F")

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
z <- bd[bd$year == LP & bd$band != "Total", ]
data.frame(
  Age_band = SHORT,
  Old_enough_to_vote = mil(z$vap),
  Said_they_voted = mil(z$voters),
  Turnout = paste0(p1(z$pct_voted), "%"),
  Registered = paste0(p1(z$pct_registered), "%"))

## ---- varmap
data.frame(
  Column = c("year", "band", "vap", "pct_voted", "voters"),
  What_it_holds = c(
    "the federal election year, 1966 to 2024",
    "one of the Bureau's four age bands",
    "voting-age population, in thousands",
    "the share who told the survey they voted",
    "vap multiplied by that share — a count this chapter derives"),
  Measurement = c("discrete", "ordered categorical", "count",
                  "continuous", "count"))

## ---- checks
data.frame(Check = ck$check, Value = ck$value)

## ---- fig1-static
op <- par(mar = c(3.8, 5.6, 2.0, 1.2), mgp = c(2.4, 0.7, 0))
z <- sh[sh$year == LP, ]
z <- z[match(BANDS, z$band), ]
M <- ceiling(max(z$pop_share, z$vote_share) / 5) * 5
plot(NA, xlim = c(-M, M), ylim = c(0.4, length(BANDS) + 0.8), axes = FALSE,
     xlab = "", ylab = "")
at <- seq(-M, M, 10)
axis(1, at = at, labels = paste0(abs(at), "%"), cex.axis = 0.78, lwd = 0,
     lwd.ticks = 1)
for (i in seq_along(BANDS)) {
  y <- length(BANDS) - i + 1
  rect(-z$pop_share[i], y - 0.32, 0, y + 0.32, col = POPC, border = NA)
  rect(0, y - 0.32, z$vote_share[i], y + 0.32, col = VOTC, border = NA)
  mtext(SHORTP[i], 2, at = y, las = 1, line = 0.4, cex = 0.8)
  text(-z$pop_share[i] - 0.6, y, paste0(p1(z$pop_share[i]), "%"), adj = 1,
       cex = 0.68, col = "#4E5A63")
  text(z$vote_share[i] + 0.6, y, paste0(p1(z$vote_share[i]), "%"), adj = 0,
       cex = 0.68, col = "#4E5A63")
}
abline(v = 0, col = "#FFFFFF", lwd = 1.4)
mtext("share of adults", 3, at = -M / 2, line = 0.2, cex = 0.82, col = POPC,
      font = 2)
mtext("share of voters", 3, at = M / 2, line = 0.2, cex = 0.82, col = VOTC,
      font = 2)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The two pyramids, with a year to drag. Shares by default because the question
# is about composition; the counts button is there because a share hides that
# the right-hand pyramid is smaller than the left one in every year, which is
# turnout and is a different fact.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
w <- merge(sh, bd[, c("year", "band", "vap", "voters", "pct_voted")],
           by = c("year", "band"))
w <- w[order(w$year, match(w$band, BANDS)), ]
rows <- paste0('{y:', w$year, ',b:"', w$band, '",ps:', w$pop_share,
               ',vs:', w$vote_share, ',pn:', round(w$vap),
               ',vn:', round(w$voters), ',t:', w$pct_voted, '}',
               collapse = ",")
cat(paste0('
<div id="pyr" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const BANDS=', paste0('["', paste(BANDS, collapse = '","'), '"]'), ';
const SHORT=', paste0('["', paste(SHORT, collapse = '","'), '"]'), ';
const POPC="', POPC, '", VOTC="', VOTC, '";
const YEARS=[...new Set(D.map(d=>d.y))].sort((a,b)=>a-b);
const W=770,H=330,M={t:44,r:30,b:52,l:74};
const box=d3.select("#pyr");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
let mode="share";
const mid=(M.l+W-M.r)/2;
const x=d3.scaleLinear().range([M.l,W-M.r]);
const y=d3.scaleBand().domain(BANDS.slice().reverse())
  .range([H-M.b,M.t]).padding(0.28);
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")");
svg.append("g").attr("transform","translate("+mid+",0)")
  .call(d3.axisLeft(y).tickSize(0).tickFormat(b=>SHORT[BANDS.indexOf(b)]))
  .selectAll("text").remove();
BANDS.forEach(function(b,i){
  svg.append("text").attr("x",M.l-12).attr("y",y(b)+y.bandwidth()/2+4)
     .attr("text-anchor","end").attr("font-size","12px")
     .attr("fill","currentColor").text(SHORT[i]);
});
svg.append("text").attr("x",M.l).attr("y",26).attr("font-size","12px")
  .attr("font-weight","700").attr("fill",POPC).text("old enough to vote");
svg.append("text").attr("x",W-M.r).attr("y",26).attr("text-anchor","end")
  .attr("font-size","12px").attr("font-weight","700").attr("fill",VOTC)
  .text("said they voted");
const gp=svg.append("g"), gv=svg.append("g"), gl=svg.append("g");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const lab=bar.append("span").attr("style","color:#4E5A63;min-width:96px");
const sl=bar.append("input").attr("type","range")
  .attr("min","0").attr("max",String(YEARS.length-1)).attr("step","1")
  .attr("value",String(YEARS.indexOf(', LP, ')))
  .attr("style","flex:1;max-width:300px;accent-color:#1C4C5C");
const btn=bar.append("button")
  .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text("show counts");
btn.on("click",function(){
  mode = mode==="share" ? "count" : "share";
  d3.select(this).text(mode==="share"?"show counts":"show shares");
  draw();
});
const fmtm=v=>(v/1000).toFixed(1)+"m";
function draw(){
  const yr=YEARS[+sl.property("value")];
  const z=BANDS.map(b=>D.find(d=>d.y===yr&&d.b===b));
  const pk=mode==="share"?"ps":"pn", vk=mode==="share"?"vs":"vn";
  const mx=d3.max(z,d=>Math.max(d[pk],d[vk]))*1.08;
  x.domain([-mx,mx]);
  gx.call(d3.axisBottom(x).ticks(7)
      .tickFormat(v=>mode==="share"?Math.abs(v).toFixed(0)+"%":fmtm(Math.abs(v))));
  const mk=(g,key,col,side)=>{
    g.selectAll("rect").data(z).join("rect")
     .attr("y",d=>y(d.b)).attr("height",y.bandwidth())
     .attr("x",d=>side<0?x(-d[key]):x(0))
     .attr("width",d=>Math.abs(x(d[key])-x(0)))
     .attr("fill",col)
     .on("mousemove",function(e,d){
       const r=box.node().getBoundingClientRect();
       tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
          .style("top",(e.clientY-r.top-8)+"px")
          .html("<b>"+SHORT[BANDS.indexOf(d.b)]+" in "+d.y+"</b><br>"+
                fmtm(d.pn)+" old enough ("+d.ps.toFixed(1)+"% of adults)<br>"+
                fmtm(d.vn)+" said they voted ("+d.vs.toFixed(1)+"% of voters)<br>"+
                "turnout "+d.t.toFixed(1)+"%");
     })
     .on("mouseleave",function(){tip.style("opacity",0);});
  };
  mk(gp,pk,POPC,-1); mk(gv,vk,VOTC,1);
  gl.selectAll("line").data([0]).join("line")
    .attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t-6).attr("y2",H-M.b)
    .attr("stroke","#FAFBFB").attr("stroke-width",1.6);
  lab.text(yr);
}
sl.on("input",draw); draw();
})();
</script>'))

## ---- fig2-static
op <- par(mar = c(3.8, 4.6, 1.4, 6.8), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = range(PRES), ylim = range(sh$gap) + c(-0.6, 0.6), axes = FALSE,
     xlab = "", ylab = "")
abline(h = 0, col = "#CBD3D8", lwd = 1.4)
axis(1, at = seq(1968, LP, 8), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("share of voters minus share of adults, in points", 2, line = 2.8,
      cex = 0.86)
for (i in seq_along(BANDS)) {
  z <- sh[sh$band == BANDS[i] & sh$year %in% PRES, ]
  z <- z[order(z$year), ]
  lines(z$year, z$gap, col = BCOL[i], lwd = 2.4)
  points(z$year, z$gap, col = BCOL[i], pch = 19, cex = 0.55)
  text(LP, z$gap[nrow(z)], paste0(" ", SHORTP[i]), col = BCOL[i], pos = 4,
       cex = 0.76, xpd = NA)
}
par(op)

## ---- fig2-d3
# Over-representation as one number per band per election: share of voters
# minus share of adults. Zero means a band votes exactly in proportion to its
# size, which is the only interesting reference line here.
mk <- function(i) {
  z <- sh[sh$band == BANDS[i] & sh$year %in% PRES, ]
  z <- z[order(z$year), ]
  paste0('{b:"', SHORT[i], '",c:"', BCOL[i], '",p:[',
         paste0("[", z$year, ",", z$gap, ",", z$pop_share, ",",
                z$vote_share, "]", collapse = ","), ']}')
}
cat(paste0('
<div id="gap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[', paste(vapply(seq_along(BANDS), mk, character(1)), collapse = ","), '];
const W=770,H=400,M={t:18,r:78,b:52,l:66};
const box=d3.select("#gap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const all=S.flatMap(s=>s.p);
const x=d3.scaleLinear().domain(d3.extent(all,p=>p[0])).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([d3.min(all,p=>p[1])-0.6,
                                 d3.max(all,p=>p[1])+0.6]).range([H-M.b,M.t]);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#CBD3D8").attr("stroke-width",1.4);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>(d>0?"+":"")+d));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("share of voters minus share of adults, in points");
svg.append("text").attr("x",M.l+6).attr("y",y(0)-6).attr("font-size","11px")
  .attr("fill","#76838C").text("votes exactly in proportion to its size");
const ln=d3.line().x(p=>x(p[0])).y(p=>y(p[1]));
S.forEach(function(s){
  svg.append("path").attr("fill","none").attr("stroke",s.c).attr("stroke-width",2.4)
     .attr("d",ln(s.p));
  // selectAll() takes a CSS selector, and the band labels do not make valid
  // ones: "c"+"65+" is "c65+", which throws, so the markers for every band
  // were silently never drawn. selectAll(null) is the d3 idiom for joining
  // into a fresh empty selection, which is what this always meant.
  svg.selectAll(null).data(s.p).join("circle")
     .attr("cx",p=>x(p[0])).attr("cy",p=>y(p[1])).attr("r",2.8).attr("fill",s.c);
  const last=s.p[s.p.length-1];
  svg.append("text").attr("x",x(last[0])+7).attr("y",y(last[1])+4)
     .attr("font-size","12px").attr("font-weight","600").attr("fill",s.c)
     .text(s.b);
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","3 3").attr("opacity",0);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const yr0=x.invert(d3.pointer(e,this)[0]+M.l);
    const yrs=S[0].p.map(p=>p[0]);
    const yr=yrs.reduce((a,b)=>Math.abs(b-yr0)<Math.abs(a-yr0)?b:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",0.5);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+yr+"</b><br>"+S.map(function(s){
          const p=s.p.find(q=>q[0]===yr); if(!p) return "";
          return "<span style=\\"color:"+s.c+"\\">&#9632;</span> "+s.b+": "+
                 (p[1]>0?"+":"")+p[1].toFixed(1)+
                 " <span style=\\"color:#8A8F94\\">("+p[2].toFixed(1)+
                 "% of adults, "+p[3].toFixed(1)+"% of voters)</span>";
       }).filter(Boolean).join("<br>"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
})();
</script>'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
