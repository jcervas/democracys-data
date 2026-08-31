# careers-code.R -- chunk bodies for careers-brief.Rmd
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

ca <- read.csv("data/derived/careers.csv",  stringsAsFactors = FALSE)
co <- read.csv("data/derived/cohorts.csv",  stringsAsFactors = FALSE)
km <- read.csv("data/derived/km.csv",       stringsAsFactors = FALSE)
nv <- read.csv("data/derived/naive.csv",    stringsAsFactors = FALSE)
tl <- read.csv("data/derived/timeline.csv", stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",    stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

LASTC  <- fn("last_congress"); NCAR <- fn("careers"); NON <- fn("ongoing")
PCTON  <- fn("pct_ongoing");   NMEM <- fn("members"); NGAP <- fn("gaps")
MKM    <- fn("median_km");     MH <- fn("median_house"); MS <- fn("median_senate")
MODKM  <- fn("median_modern_km"); OLDKM <- fn("median_old_km")
MODN   <- fn("mod_n"); MODON <- fn("mod_ongoing"); MODPCT <- fn("mod_pct")
MODALL <- fn("mod_median_all"); MODEND <- fn("mod_median_ended")
ENTRY  <- fn("entry_congress"); ENTN <- fn("entry_n"); ENTON <- fn("entry_ongoing")
MAXT   <- fn("max_tenure"); MAXN <- f("max_name")

# the year a Congress convened, for labelling only
yr <- function(cg) 1789 + 2 * (cg - 1)

LAST_CO <- co[nrow(co), ]      # the newest entering cohort

ACC <- "#1C4C5C"; ENDC <- "#2c7fb8"; ONC <- "#C41230"
GRY <- "#8A8F94"; GLD <- "#C08A2E"; GRN <- "#4d9221"

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
e <- tl[!tl$ongoing, ][1, ]; g <- tl[tl$ongoing, ][1, ]
data.frame(
  Member = c(e$bioname, g$bioname),
  Entered = paste0(e$first[1], "th"),
  Last_seen = c(paste0(e$last, "th"), paste0(g$last, "th")),
  Congresses_so_far = c(e$tenure, g$tenure),
  Career = c("ended", "still running"))

## ---- varmap
data.frame(
  Column = c("icpsr", "chamber", "first", "last", "tenure", "ongoing"),
  What_it_holds = c(
    "the member's permanent Voteview identifier",
    "House or Senate — a career here is a member in one chamber",
    "the first Congress in which they served",
    "the last Congress in which they served",
    "how many Congresses they served, counted not spanned",
    paste0("TRUE if they were still there in the ", LASTC, "th")),
  Measurement = c("categorical", "dichotomous", "discrete", "discrete",
                  "count", "dichotomous"))

## ---- fig1-static
op <- par(mar = c(3.6, 0.6, 1.6, 0.6), mgp = c(2.3, 0.6, 0))
plot(NA, xlim = c(0, max(tl$tenure) + 2.2), ylim = c(nrow(tl) + 1, 0),
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = 0:max(tl$tenure), cex.axis = 0.76, lwd = 0, lwd.ticks = 1)
mtext("Congresses served", 1, line = 2.1, cex = 0.88)
for (i in seq_len(nrow(tl))) {
  col <- if (tl$ongoing[i]) ONC else ENDC
  segments(0, i, tl$tenure[i], i, col = col, lwd = 3.2, lend = 1)
  if (tl$ongoing[i])
    arrows(tl$tenure[i], i, tl$tenure[i] + 1.5, i, length = 0.05,
           col = ONC, lwd = 1.6)
  else
    points(tl$tenure[i], i, pch = 124, col = col, cex = 0.62)
}
legend("bottomright",
       c(paste0("ended (", sum(!tl$ongoing), ")"),
         paste0("still serving (", sum(tl$ongoing), ")")),
       col = c(ENDC, ONC), lwd = 3, bty = "n", cex = 0.76)
mtext(paste0("Everyone who first entered the House in the ", ENTRY,
             "th Congress (", yr(ENTRY), ")"),
      3, line = 0.4, cex = 0.86, font = 2, adj = 0)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# One bar per career. A bar that ends in a tick is a career; a bar that ends in
# an arrow is a career the record has not seen the end of. That distinction is
# the entire chapter and it is worth one figure before any estimator appears.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('{i:', tl$row, ',nm:"', gsub('"', "'", tl$bioname, fixed = TRUE),
               '",p:"', substr(tl$party, 1, 1), '",st:"', tl$state_abbrev,
               '",t:', tl$tenure, ',on:', ifelse(tl$ongoing, 1, 0),
               ',last:', tl$last, '}', collapse = ",")
cat(paste0('
<div id="tl" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const ENDC="', ENDC, '", ONC="', ONC, '";
const W=770,H=', 90 + 7.4 * nrow(tl), ',M={t:36,r:120,b:44,l:26};
const box=d3.select("#tl");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const maxT=d3.max(D,d=>d.t);
const x=d3.scaleLinear().domain([0,maxT+1.6]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,D.length+1]).range([M.t,H-M.b]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(maxT).tickFormat(d3.format("d")));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-12)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Congresses served");
svg.append("text").attr("x",M.l).attr("y",18).attr("font-size","12px")
  .attr("font-weight","700").attr("fill","currentColor")
  .text("Everyone who first entered the House in the ', ENTRY, 'th Congress (', yr(ENTRY), ')");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const g=svg.append("g").selectAll("g").data(D).join("g")
  .attr("transform",d=>"translate(0,"+y(d.i)+")");
g.append("line").attr("x1",x(0)).attr("x2",d=>x(d.t)).attr("y1",0).attr("y2",0)
  .attr("stroke",d=>d.on?ONC:ENDC).attr("stroke-width",3.4)
  .attr("stroke-linecap","butt");
// a tick closes a finished career; an arrow says the record simply stops
g.filter(d=>!d.on).append("line")
  .attr("x1",d=>x(d.t)).attr("x2",d=>x(d.t)).attr("y1",-3.4).attr("y2",3.4)
  .attr("stroke",ENDC).attr("stroke-width",1.6);
g.filter(d=>d.on).append("path")
  .attr("d",d=>"M"+x(d.t)+",-3.6L"+(x(d.t)+9)+",0L"+x(d.t)+",3.6Z")
  .attr("fill",ONC);
g.append("rect").attr("x",M.l).attr("y",-3.7).attr("width",W-M.r-M.l)
  .attr("height",7.4).attr("fill","transparent")
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.nm+"</b> ("+d.p+"-"+d.st+")<br>"+
             d.t+" Congress"+(d.t===1?"":"es")+
             (d.on?", <b>still serving</b>":", left after the "+d.last+"th"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
const leg=svg.append("g").attr("transform","translate("+(W-M.r+14)+","+(M.t+6)+")");
[["ended ("+D.filter(d=>!d.on).length+")",ENDC],
 ["still serving ("+D.filter(d=>d.on).length+")",ONC]].forEach(function(s,i){
  leg.append("line").attr("x1",0).attr("x2",18).attr("y1",i*18).attr("y2",i*18)
     .attr("stroke",s[1]).attr("stroke-width",3.4);
  leg.append("text").attr("x",24).attr("y",i*18+4).attr("font-size","11px")
     .attr("fill","#4E5A63").text(s[0]);
});
})();
</script>'))

## ---- naivetab
data.frame(
  Method = nv$method,
  What_it_assumes = nv$what_it_assumes,
  Median_Congresses = nv$median)

## ---- cohorttab
z <- co[co$cohort_lo >= 41, ]
data.frame(
  Entered = paste0(z$cohort_lo, "th–", z$cohort_hi, "th"),
  Years = paste0(yr(z$cohort_lo), "–", yr(z$cohort_hi)),
  Careers = n(z$n),
  Still_running = paste0(p1(z$pct_ongoing), "%"),
  Mean_of_the_ended = p1(z$mean_ended))

## ---- fig2-static
op <- par(mar = c(4.0, 4.4, 1.4, 6.4), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(0, 22), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(0, 22, 2), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"),
     las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Congresses served", 1, line = 2.4, cex = 0.9)
mtext("share of careers still running", 2, line = 3.0, cex = 0.9)
abline(h = 0.5, col = GRY, lty = 3)
cols <- c(All = ACC, House = ENDC, Senate = GLD)
for (s in names(cols)) {
  k <- km[km$stratum == s, ]
  lines(c(0, k$t), c(1, k$surv), type = "s", col = cols[s], lwd = 2.4)
  text(22, k$surv[which.min(abs(k$t - 22))], paste0(" ", s), col = cols[s],
       pos = 4, cex = 0.74, xpd = NA)
}
par(op)

## ---- fig2-d3
# The curve, with the risk set on the hover. A survival curve read without its
# risk set invites the reader to trust the long tail, where the estimate rests
# on a handful of careers -- so the number at risk travels with the estimate.
mk <- function(s) {
  k <- km[km$stratum == s, ]
  paste0('{s:"', s, '",n:', k$n_total[1], ',ong:', k$n_ongoing[1],
         ',t:[', paste(k$t, collapse = ","),
         '],v:[', paste(formatC(k$surv, format = "f", digits = 5),
                        collapse = ","),
         '],r:[', paste(k$at_risk, collapse = ","),
         '],d:[', paste(k$ended, collapse = ","), ']}')
}
cat(paste0('
<div id="km" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[', paste(vapply(c("All", "House", "Senate"), mk, character(1)),
                  collapse = ","), '];
const COL={All:"', ACC, '",House:"', ENDC, '",Senate:"', GLD, '"};
const W=770,H=420,M={t:18,r:110,b:52,l:64};
const box=d3.select("#km");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,22]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(11));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d3.format(".0%")).ticks(5));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0.5)).attr("y2",y(0.5))
  .attr("stroke","#8A8F94").attr("stroke-dasharray","3 3");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Congresses served");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("share of careers still running");
const line=d3.line().curve(d3.curveStepAfter).x(d=>x(d.t)).y(d=>y(d.v));
S.forEach(function(s){
  const pts=[{t:0,v:1}].concat(s.t.map((t,i)=>({t:t,v:s.v[i]})));
  svg.append("path").attr("fill","none").attr("stroke",COL[s.s])
     .attr("stroke-width",2.4).attr("d",line(pts.filter(p=>p.t<=22)));
  const at22=s.v[s.t.filter(t=>t<=22).length-1];
  svg.append("text").attr("x",W-M.r+7).attr("y",y(at22)+4)
     .attr("font-size","12px").attr("font-weight","600").attr("fill",COL[s.s])
     .text(s.s);
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","2 2").attr("opacity",0);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const t=Math.max(1,Math.round(x.invert(d3.pointer(e,this)[0]+M.l)));
    rule.attr("x1",x(t)).attr("x2",x(t)).attr("opacity",0.5);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>after "+t+" Congress"+(t===1?"":"es")+"</b><br>"+
         S.map(function(s){
           let i=-1; for(let k=0;k<s.t.length;k++){ if(s.t[k]<=t) i=k; }
           if(i<0) return "";
           return "<span style=\\"color:"+COL[s.s]+"\\">&#9632;</span> "+s.s+
                  ": "+(100*s.v[i]).toFixed(1)+"% still serving"+
                  " <span style=\\"color:#8A8F94\\">("+s.r[i]+" at risk)</span>";
         }).filter(Boolean).join("<br>"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
})();
</script>'))

## ---- fig3-static
op <- par(mar = c(4.0, 4.4, 1.4, 7.4), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(0, 22), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(0, 22, 2), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"),
     las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Congresses served", 1, line = 2.4, cex = 0.9)
mtext("share of careers still running", 2, line = 3.0, cex = 0.9)
abline(h = 0.5, col = GRY, lty = 3)
ers <- c("1st-49th", "50th-79th", "80th-103rd", "104th-119th")
ecol <- c("#CBD3D8", GRY, GRN, ONC)
for (i in seq_along(ers)) {
  k <- km[km$stratum == ers[i], ]
  lines(c(0, k$t), c(1, k$surv), type = "s", col = ecol[i], lwd = 2.4)
}
legend(22.4, 0.95, ers, col = ecol, lwd = 2.4, bty = "n", cex = 0.7, xpd = NA)
par(op)

## ---- fig3-d3
cat(paste0('
<div id="kmera" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[', paste(vapply(c("1st-49th", "50th-79th", "80th-103rd", "104th-119th"),
                         mk, character(1)), collapse = ","), '];
const COL=["#CBD3D8","', GRY, '","', GRN, '","', ONC, '"];
const W=770,H=420,M={t:18,r:132,b:52,l:64};
const box=d3.select("#kmera");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,22]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(11));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d3.format(".0%")).ticks(5));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0.5)).attr("y2",y(0.5))
  .attr("stroke","#8A8F94").attr("stroke-dasharray","3 3");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4E5A63")
  .text("Congresses served");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("share of careers still running");
const line=d3.line().curve(d3.curveStepAfter).x(d=>x(d.t)).y(d=>y(d.v));
S.forEach(function(s,i){
  const pts=[{t:0,v:1}].concat(s.t.map((t,j)=>({t:t,v:s.v[j]})));
  svg.append("path").attr("fill","none").attr("stroke",COL[i])
     .attr("stroke-width",2.4).attr("d",line(pts.filter(p=>p.t<=22)));
});
const leg=svg.append("g").attr("transform","translate("+(W-M.r+12)+","+(M.t+10)+")");
S.forEach(function(s,i){
  leg.append("line").attr("x1",0).attr("x2",16).attr("y1",i*20).attr("y2",i*20)
     .attr("stroke",COL[i]).attr("stroke-width",2.6);
  leg.append("text").attr("x",22).attr("y",i*20+4).attr("font-size","11px")
     .attr("fill","#4E5A63").text(s.s);
  leg.append("text").attr("x",22).attr("y",i*20+15).attr("font-size","9.5px")
     .attr("fill","#8A8F94").text(s.n+" careers, "+s.ong+" unfinished");
});
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#12181D").attr("stroke-dasharray","2 2").attr("opacity",0);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const t=Math.max(1,Math.round(x.invert(d3.pointer(e,this)[0]+M.l)));
    rule.attr("x1",x(t)).attr("x2",x(t)).attr("opacity",0.5);
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>after "+t+" Congress"+(t===1?"":"es")+"</b><br>"+
         S.map(function(s,i){
           let j=-1; for(let k=0;k<s.t.length;k++){ if(s.t[k]<=t) j=k; }
           if(j<0) return "";
           return "<span style=\\"color:"+COL[i]+"\\">&#9632;</span> "+s.s+
                  ": "+(100*s.v[j]).toFixed(1)+"%";
         }).filter(Boolean).join("<br>"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);rule.attr("opacity",0);});
})();
</script>'))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#km text[fill="#2c7fb8" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')
