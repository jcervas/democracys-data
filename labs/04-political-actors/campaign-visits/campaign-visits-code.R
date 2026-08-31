# campaign-visits-code.R -- chunk bodies for campaign-visits-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.2,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

v  <- read.csv("data/derived/campaign_visits_2024.csv", stringsAsFactors = FALSE)
v$date <- as.Date(v$date)
st <- read.csv("data/derived/pres2024_states.csv", stringsAsFactors = FALSE)
ap <- read.csv("data/derived/ap_snapshots.csv",    stringsAsFactors = FALSE)

vis <- as.data.frame(table(v$state))
names(vis) <- c("state", "visits")
vis$state <- as.character(vis$state)

m <- merge(st, vis, by = "state", all.x = TRUE)
m$visits[is.na(m$visits)] <- 0
m$absm   <- abs(m$margin)
m$per_ev <- m$visits / m$ev

mn  <- m[m$state != "District of Columbia", ]   # same table, DC removed
top <- m[order(-m$visits), ]
t7  <- head(top, 7)
z   <- m[m$visits == 0, ]
cl  <- m[m$absm < 5, ]                # the close states
lg  <- head(m[order(-m$ev), ], 10)    # the ten largest

OLD <- ap[ap$version == 50, ]         # the capture this lab used to be built on
NEW <- ap[ap$version == 54, ]         # the capture it is built on now
LAST_OLD <- as.Date(OLD$last_event)
added_late  <- sum(v$date > LAST_OLD)
added_back  <- NEW$rows_in_shared_window - OLD$rows_in_shared_window

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)
r2 <- function(x) formatC(x, format = "f", digits = 2)
per <- function(s) m$per_ev[m$state == s]

# ---- Lorenz curve of attention against electoral weight -------------------
# States ordered from least to most attention per electoral vote. x is the
# cumulative share of the electoral college, y the cumulative share of stops.
lz <- m[order(m$per_ev, m$visits), ]
lx <- c(0, cumsum(lz$ev)     / sum(lz$ev))
ly <- c(0, cumsum(lz$visits) / sum(lz$visits))
gini    <- 1 - sum(diff(lx) * (ly[-1] + ly[-length(ly)]))
zero_ec <- lx[nrow(z) + 1]                 # EC share held by the zero-stop bloc
t7_ec   <- 1 - sum(t7$ev) / sum(m$ev)      # where the top seven begin on the x axis

# ---- The campaign calendar ------------------------------------------------
# One record per candidate per day, so a lane chart can stack same-day stops.
cd <- as.data.frame(table(v$candidate, v$date), stringsAsFactors = FALSE)
names(cd) <- c("cand", "date", "k")
cd <- cd[cd$k > 0, ]
cd$date <- as.Date(cd$date)
LANE <- c("Trump", "Vance", "Biden", "Harris", "Walz")
cd$lane <- match(cd$cand, LANE)
cd$rep  <- cd$cand %in% c("Trump", "Vance")
GAP0 <- max(v$date[v$candidate == "Biden"])   # last Biden stop
GAP1 <- min(v$date[v$candidate == "Harris"])  # first Harris stop
D0   <- min(v$date); D1 <- max(v$date)

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

## ---- one-record
o <- v[c(1, 150, 330), c("date", "city", "state", "candidate", "event_type")]
names(o) <- c("date", "city", "state", "candidate", "event type")
o

## ---- scope
data.frame(
  quantity = c("Campaign stops logged", "People tracked", "First stop",
               "Last stop", "States with at least one stop", "Distinct cities",
               "Source"),
  value = c(n(nrow(v)), length(unique(v$candidate)),
            format(min(v$date), "%d %B %Y"), format(max(v$date), "%d %B %Y"),
            length(unique(v$state)), n(length(unique(v$city))),
            paste("AP campaign trail tracker, version", NEW$version)))

## ---- ap-raw
rawv <- read.csv("data/raw/ap-dataset-54.csv", stringsAsFactors = FALSE,
                 check.names = FALSE, colClasses = "character")
kept <- c("date", "city", "county", "state", "postal", "lat", "long", "fips",
          "candidate", "event-type", "notes")
r1 <- rawv[1, ]

# Column, what it holds, value, and whether it survives. The descriptions are
# read off the names and values -- AP published no codebook with the capture --
# and the ones that describe the GRAPHIC rather than the event are marked,
# because that distinction is the whole section.
.ap <- c(
  date = "the date of the event", city = "the city it was held in",
  county = "the county", state = "the state, spelled out",
  postal = "the state's two-letter code — the same fact again",
  lat = "latitude", long = "longitude",
  fips = "county FIPS code", candidate = "who appeared",
  `event-type` = "what kind of event it was",
  notes = "free text about the event",
  `biden-total-visits-city` = "tooltip: Biden's running total for this city",
  `biden-total-visits-county` = "tooltip: his running total for the county",
  `biden-total-visits-state` = "tooltip: his running total for the state",
  `trump-total-visits-city` = "tooltip: Trump's running total for this city",
  `trump-total-visits-county` = "tooltip: his running total for the county",
  `trump-total-visits-state` = "tooltip: his running total for the state",
  `total-state-visits` = "tooltip: both candidates, this state",
  `total-county-visits` = "tooltip: both candidates, this county",
  `total-city-visits` = "tooltip: both candidates, this city",
  `biden-date` = "tooltip: date formatted for display",
  `trump-date` = "tooltip: date formatted for display",
  `city-visits-status` = "tooltip: text describing the city's visit count",
  `county-text` = "tooltip: the county sentence shown to a reader",
  `reg-voters` = "tooltip: registered voters in the county",
  `reg-voters-as-of-date` = "tooltip: the date that count was taken")
data.frame(
  Column_as_it_arrives = names(rawv),
  What_it_holds = ifelse(names(rawv) %in% names(.ap),
                         unname(.ap[names(rawv)]), "—"),
  Value_in_record_1 = ifelse(nchar(as.character(r1)) == 0, "(empty)",
                             as.character(r1)),
  This_chapter = ifelse(names(rawv) %in% kept, "kept", "—"))

## ---- ap-clean
o <- v[1:2, c("date", "city", "state", "candidate", "event_type", "notes")]
o$notes <- paste0(substr(o$notes, 1, 34), "...")
names(o) <- c("date", "city", "state", "candidate", "event type", "notes")
o

## ---- top-states
o <- head(top[, c("state", "ev", "absm", "visits")], 6)
names(o) <- c("state", "electoral votes", "margin (pts)", "stops")
o

## ---- pointmap-static
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(v$long, v$lat, type = "n", asp = 1 / cos(38 * pi / 180),
     axes = FALSE, xlab = "", ylab = "",
     xlim = c(-125, -66), ylim = c(24, 50))
abline(v = seq(-120, -70, 10), h = seq(25, 50, 5), col = "grey93")
dem <- v$candidate %in% c("Biden", "Harris", "Walz")
# plot in file order, not one ticket then the other, so neither side is
# systematically drawn on top of the other
points(v$long, v$lat, pch = 19, cex = 0.8,
       col = ifelse(dem, "#2c7fb899", "#C4123099"))
legend("bottomleft", c("Democratic ticket", "Republican ticket"),
       pch = 19, col = c("#2c7fb8", "#C41230"), bty = "n", cex = 0.7)

## ---- pointmap-d3
tk <- ifelse(v$candidate %in% c("Biden", "Harris", "Walz"), "D", "R")
pts <- paste(sprintf('{"x":%.3f,"y":%.3f,"c":"%s","t":"%s","s":"%s","d":"%s","k":"%s"}',
                     v$long, v$lat, gsub('"', "", v$city), tk,
                     v$postal, format(v$date, "%d %b"), v$candidate),
             collapse = ",")
cat(sprintf('
<div id="pm" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px;font:12px inherit">
  <button id="pAll" style="font:12px inherit;padding:3px 9px;cursor:pointer">Both tickets</button>
  <button id="pD" style="font:12px inherit;padding:3px 9px;cursor:pointer;color:#2c7fb8">Democratic only</button>
  <button id="pR" style="font:12px inherit;padding:3px 9px;cursor:pointer;color:#C41230">Republican only</button>
  <span id="pn" style="margin-left:8px;color:#444"></span>
 </div>
</div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const P=[%s];
const W=760,M=14;
const lat0=Math.cos(38*Math.PI/180);
const xs=d3.scaleLinear().domain([-125,-66]).range([M,W-M]);
const yr=(W-2*M)/(59*lat0)*26;
const ys=d3.scaleLinear().domain([24,50]).range([yr+M,M]);
const svg=d3.select("#pm").append("svg").attr("viewBox",`0 0 ${W} ${yr+2*M}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit;background:#fbfbfc");
const gg=svg.append("g");
for(let lo=-120;lo<=-70;lo+=10) gg.append("line").attr("x1",xs(lo)).attr("x2",xs(lo))
  .attr("y1",M).attr("y2",yr+M).attr("stroke","#eceef1");
for(let la=25;la<=50;la+=5) gg.append("line").attr("y1",ys(la)).attr("y2",ys(la))
  .attr("x1",M).attr("x2",W-M).attr("stroke","#eceef1");
const tip=d3.select("#pm").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
function draw(f){
  const D=P.filter(f);
  d3.select("#pn").text(D.length+" stops shown");
  g.selectAll("circle").data(D).join("circle")
   .attr("cx",p=>xs(p.x)).attr("cy",p=>ys(p.y)).attr("r",4.2)
   .attr("fill",p=>p.t==="D"?"#2c7fb8":"#C41230").attr("fill-opacity",0.55)
   .attr("stroke",p=>p.t==="D"?"#2c7fb8":"#C41230").attr("stroke-opacity",0.9)
   .on("mousemove",function(e,p){
     tip.style("opacity",1).html(`<b>${p.c}, ${p.s}</b><br>${p.k}, ${p.d}`)
        .style("left",Math.min(xs(p.x)+12,W-180)+"px").style("top",(ys(p.y)-8)+"px");})
   .on("mouseleave",()=>tip.style("opacity",0));
}
const lg=svg.append("g").attr("transform",`translate(${M+6},${yr+M-34})`);
[["#2c7fb8","Democratic ticket"],["#C41230","Republican ticket"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",6).attr("cy",i*18).attr("r",4.2).attr("fill",r[0])
    .attr("fill-opacity",0.55).attr("stroke",r[0]);
  lg.append("text").attr("x",17).attr("y",i*18+4).attr("font-size","11.5px")
    .attr("fill","#333").text(r[1]);
});
draw(()=>true);
d3.select("#pAll").on("click",()=>draw(()=>true));
d3.select("#pD").on("click",()=>draw(p=>p.t==="D"));
d3.select("#pR").on("click",()=>draw(p=>p.t==="R"));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Every logged stop at its own coordinates — no state boundaries, only the events.
Hover for the city and the date.</p>
', pts))

## ---- conc
data.frame(
  quantity = c("Top seven states' share of all stops",
               "Top seven states' share of the electoral college",
               "Stops in states decided by under 5 points"),
  value = c(paste0(pc(100 * sum(t7$visits) / nrow(v)), "%"),
            paste0(pc(100 * sum(t7$ev) / sum(m$ev)), "%"),
            paste0(pc(100 * sum(m$visits[m$absm < 5]) / nrow(v)), "%")))

## ---- zeros
data.frame(
  quantity = c("States and DC in the results file", "With at least one stop",
               "With none", "Electoral votes held by the zero-stop states",
               "Their share of the electoral college"),
  value = c(nrow(m), sum(m$visits > 0), nrow(z), sum(z$ev),
            paste0(pc(100 * sum(z$ev) / sum(m$ev)), "%")))

## ---- cartogram-static
par(mar = c(1.8, 0.3, 0.3, 0.3))
plot(NA, xlim = c(0.8, max(m$col) + 1.1), ylim = c(max(m$row) + 1.1, 0.8),
     axes = FALSE, xlab = "", ylab = "")
br  <- c(-0.5, 0.5, 4.5, 14.5, 29.5, 49.5, 1000)
pal <- c("#f4f4f6", "#dbe6ef", "#a8c6df", "#5a95c4", "#2c7fb8", "#123f5e")
cl2 <- pal[as.integer(cut(m$visits, br))]
rect(m$col, m$row, m$col + 0.92, m$row + 0.92, col = cl2, border = "white")
text(m$col + 0.46, m$row + 0.38, m$abbrev, cex = 0.62,
     col = ifelse(m$visits > 14, "white", "grey25"))
text(m$col + 0.46, m$row + 0.72, m$visits, cex = 0.55,
     col = ifelse(m$visits > 14, "white", "grey45"))
legend("bottom", c("0", "1-4", "5-14", "15-29", "30-49", "50+"),
       fill = pal, bty = "n", cex = 0.66, horiz = TRUE, border = "white")

## ---- cartogram-d3
rows <- paste(sprintf('{"s":"%s","a":"%s","c":%d,"r":%d,"v":%d,"e":%d,"m":%.2f}',
                      m$state, m$abbrev, m$col, m$row, m$visits, m$ev, m$absm),
              collapse = ",")
cat(sprintf('
<div id="cg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const CW=58,W=11*CW+30,H=8*CW+42;
const svg=d3.select("#cg").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const col=d3.scaleThreshold().domain([1,5,15,30,50])
  .range(["#f4f4f6","#dbe6ef","#a8c6df","#5a95c4","#2c7fb8","#123f5e"]);
const tip=d3.select("#cg").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:8px 11px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g").attr("transform",`translate(${-CW+14},${-CW+10})`);
const cell=g.selectAll("g").data(D).join("g")
  .attr("transform",d=>`translate(${d.c*CW},${d.r*CW})`);
cell.append("rect").attr("width",CW-5).attr("height",CW-5).attr("rx",3)
  .attr("fill",d=>col(d.v)).attr("stroke","#fff").attr("stroke-width",2);
cell.append("text").attr("x",(CW-5)/2).attr("y",(CW-5)/2-1).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600")
  .attr("fill",d=>d.v>=15?"#fff":"#333").text(d=>d.a);
cell.append("text").attr("x",(CW-5)/2).attr("y",(CW-5)/2+14).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("fill",d=>d.v>=15?"#fff":"#777").text(d=>d.v);
cell.on("mousemove",function(e,d){
    d3.select(this).select("rect").attr("stroke","#111");
    tip.style("opacity",1).html(
      `<b>${d.s}</b><br>${d.v} campaign stops<br>${d.e} electoral votes<br>`+
      `decided by ${d.m.toFixed(2)} points`)
      .style("left",Math.min((d.c-1)*CW+20,W-230)+"px").style("top",((d.r-1)*CW+18)+"px");})
  .on("mouseleave",function(){d3.select(this).select("rect").attr("stroke","#fff");
    tip.style("opacity",0);});
const lg=svg.append("g").attr("transform",`translate(16,${H-24})`);
[["#f4f4f6","0"],["#dbe6ef","1-4"],["#a8c6df","5-14"],["#5a95c4","15-29"],
 ["#2c7fb8","30-49"],["#123f5e","50+"]].forEach((r,i)=>{
  lg.append("rect").attr("x",i*74).attr("width",16).attr("height",13).attr("fill",r[0])
    .attr("stroke","#ccc");
  lg.append("text").attr("x",i*74+21).attr("y",11).attr("font-size","11.5px").text(r[1]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One tile per state and DC, laid out roughly geographically, shaded by campaign
stops. Hover for electoral votes and margin.</p>
', rows))

## ---- lorenz-static
HY <- approx(lx, ly, 0.5)$y
par(mar = c(4.4, 4.6, 0.6, 0.9))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i", axes = FALSE,
     xlab = "cumulative share of the electoral college",
     ylab = "cumulative share of all campaign stops")
abline(h = seq(0.2, 0.8, 0.2), v = seq(0.2, 0.8, 0.2), col = "grey93")
polygon(c(0, 1, rev(lx)), c(0, 1, rev(ly)), col = "#efeaf6", border = NA)
lines(c(0, 1), c(0, 1), lty = 2, col = "grey45")
lines(lx, ly, col = "#54278F", lwd = 2.2)
segments(0, 0, zero_ec, 0, col = "#C41230", lwd = 4)
segments(0.5, 0, 0.5, HY, col = "grey35", lty = 3)
points(0.5, HY, pch = 19, cex = 0.85, col = "grey20")
axis(1, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"),
     col = "grey70", cex.axis = 0.8)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), las = 1,
     col = "grey70", cex.axis = 0.8)
box(col = "grey70")
# kept clear of the dashed diagonal, which would otherwise strike through it
text(0.30, 0.085, paste0(nrow(z), " states got no stops at all\n(",
     pc(100 * zero_ec), "% of the electoral college)"),
     adj = 0, cex = 0.66, col = "#C41230")
text(0.52, 0.275, paste0("the least-visited half of the\nelectoral college got ",
     pc(100 * HY), "% of all stops"), adj = 0, cex = 0.66, col = "grey20")
text(0.40, 0.68, "if attention followed\nelectoral weight", adj = 0,
     cex = 0.66, col = "grey45")
text(0.985, 0.055, paste0("Gini = ", r2(gini)), adj = 1, cex = 0.72,
     col = "#54278F")

## ---- lorenz-d3
# d3 is loaded once, by the first D3 figure above
HY <- approx(lx, ly, 0.5)$y
f5 <- function(x) formatC(x, format = "f", digits = 5)
J <- paste0('{"s":"', lz$state, '","e":', lz$ev, ',"v":', lz$visits,
            ',"x":', f5(lx[-1]), ',"y":', f5(ly[-1]), '}', collapse = ",")
cat(paste0('
<div id="lz" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', J, '];
const ZERO=', f5(zero_ec), ', HY=', f5(HY), ';
const NZ="', nrow(z), '", ZP="', pc(100 * zero_ec), '", HP="', pc(100 * HY),
'", G="', r2(gini), '";
const W=760,H=470,M={t:18,r:22,b:52,l:66};
const svg=d3.select("#lz").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,1]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
const pct=d=>Math.round(d*100)+"%";
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(pct));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(6).tickFormat(pct));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("cumulative share of the electoral college");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("cumulative share of all campaign stops");
const P=[{x:0,y:0}].concat(D);
const area=d3.area().x(p=>x(p.x)).y0(p=>y(p.x)).y1(p=>y(p.y));
svg.append("path").datum(P).attr("d",area).attr("fill","#efeaf6");
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(1)).attr("y2",y(1))
  .attr("stroke","#888").attr("stroke-dasharray","5 4");
svg.append("path").datum(P).attr("fill","none").attr("stroke","#54278F")
  .attr("stroke-width",2.2)
  .attr("d",d3.line().x(p=>x(p.x)).y(p=>y(p.y)));
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(ZERO)).attr("y2",y(0))
  .attr("stroke","#C41230").attr("stroke-width",4);
svg.append("line").attr("x1",x(0.5)).attr("y1",y(0)).attr("x2",x(0.5)).attr("y2",y(HY))
  .attr("stroke","#555").attr("stroke-dasharray","2 3");
svg.append("circle").attr("cx",x(0.5)).attr("cy",y(HY)).attr("r",3.4).attr("fill","#333");
function note(px,py,col,lines,anchor){
  const t=svg.append("text").attr("x",x(px)).attr("y",y(py)).attr("fill",col)
    .attr("font-size","11.5px").attr("text-anchor",anchor||"start");
  lines.forEach((L,i)=>t.append("tspan").attr("x",x(px)).attr("dy",i?14:0).text(L));
}
note(0.30,0.105,"#C41230",[NZ+" states got no stops at all",
  "("+ZP+"% of the electoral college)"]);
note(0.52,0.30,"#333",["the least-visited half of the",
  "electoral college got "+HP+"% of all stops"]);
note(0.40,0.72,"#888",["if attention followed","electoral weight"]);
svg.append("text").attr("x",x(0.985)).attr("y",y(0.05)).attr("text-anchor","end")
  .attr("font-size","12.5px").attr("fill","#54278F").text("Gini = "+G);
const tip=d3.select("#lz").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle.v").data(D).join("circle").attr("class","v")
  .attr("cx",p=>x(p.x)).attr("cy",p=>y(p.y)).attr("r",4.5)
  .attr("fill",p=>p.v===0?"#C41230":"#54278F").attr("fill-opacity",p=>p.v===0?0.5:0.75)
  .on("mousemove",function(e,p){
     tip.style("opacity",1).html("<b>"+p.s+"</b><br>"+p.v+" stops, "+p.e+
       " electoral votes<br>running total: "+Math.round(p.y*1000)/10+
       "% of stops, "+Math.round(p.x*1000)/10+"% of the college")
       .style("left",Math.min(x(p.x)+12,W-250)+"px").style("top",(y(p.y)-6)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
'))

## ---- per-ev
o <- m[m$state %in% c("Wisconsin", "Nevada", "Pennsylvania", "Texas", "California"),
       c("state", "ev", "absm", "visits", "per_ev")]
o <- o[order(-o$per_ev), ]
o$per_ev <- r2(o$per_ev)
names(o) <- c("state", "electoral votes", "margin (pts)", "stops",
              "stops per electoral vote")
o

## ---- size
data.frame(
  quantity = c("Correlation, stops and electoral votes (all 51)",
               "Three largest states' electoral votes",
               "Their combined stops"),
  value = c(r2(cor(m$visits, m$ev)), sum(lg$ev[1:3]), sum(lg$visits[1:3])))

## ---- close
data.frame(
  quantity = c("Correlation, stops and margin of victory (all 51)",
               "States decided by under 5 points",
               "Of those, how many got fewer than 5 stops"),
  value = c(r2(cor(m$visits, m$absm)), nrow(cl), sum(cl$visits < 5)))

## ---- close-tab
o <- cl[order(cl$visits), c("state", "ev", "absm", "visits")]
o$absm <- pc(o$absm, 2)
names(o) <- c("state", "electoral votes", "margin (pts)", "stops")
o

## ---- interaction
data.frame(
  question = c("Among all 51 jurisdictions: do stops track size?",
               "Among all 51: do stops track closeness?",
               paste0("Among the ", nrow(cl), " states within 5 points: do stops track size?"),
               "Among the 10 largest states: do stops track closeness?"),
  correlation = c(r2(cor(m$visits, m$ev)), r2(cor(m$visits, m$absm)),
                  r2(cor(cl$visits, cl$ev)), r2(cor(lg$visits, lg$absm))))

## ---- index
m$prize <- m$ev * (m$absm < 5)
data.frame(
  measure = c("Electoral votes alone", "Margin alone",
              "Electoral votes x (within 5 points)"),
  `correlation with stops` = c(r2(cor(m$visits, m$ev)),
                               r2(cor(m$visits, m$absm)),
                               r2(cor(m$visits, m$prize))),
  check.names = FALSE)

## ---- d3-scatter
# d3 is loaded once, by the first D3 figure above
pts <- paste(sprintf('{"s":"%s","x":%.2f,"y":%d,"e":%d}',
                     m$state, m$absm, m$visits, m$ev), collapse = ",")
YMAX <- ceiling(max(m$visits) / 10) * 10 + 5   # 85 while the maximum is 80
EVK  <- c(3, 20, max(m$ev))                    # the size key: smallest, mid, largest
cat(sprintf('
<div id="cv" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="bAll" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">All 51</button>
  <button id="bNoDC" style="font:12px inherit;padding:4px 10px;cursor:pointer">Drop the District of Columbia</button>
  <span id="rr" style="font:12px inherit;margin-left:10px;color:#444"></span>
 </div>
</div>
<script>
(function(){
const ALL=[%s];
const W=770,H=430,M={t:16,r:24,b:44,l:56};
const svg=d3.select("#cv").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,%d]).range([H-M.b,M.t]);
const rr=d3.scaleSqrt().domain([0,%d]).range([0,20]);
const gx=svg.append("g").attr("transform",`translate(0,${H-M.b})`);
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("margin of victory (percentage points)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444").text("campaign stops");
const g=svg.append("g"), lab=svg.append("g");
const tip=d3.select("#cv").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
function corr(a,b){const ma=d3.mean(a),mb=d3.mean(b);
  let sn=0,da=0,db=0;
  for(let i=0;i<a.length;i++){sn+=(a[i]-ma)*(b[i]-mb);da+=(a[i]-ma)**2;db+=(b[i]-mb)**2;}
  return sn/Math.sqrt(da*db);}
function draw(D){
  x.domain([0,d3.max(D,p=>p.x)*1.05]);
  gx.transition().duration(500).call(d3.axisBottom(x).ticks(8));
  g.selectAll("circle").data(D,p=>p.s).join(
    en=>en.append("circle").attr("cx",p=>x(p.x)).attr("cy",p=>y(p.y)).attr("r",0)
      .attr("fill","#54278F").attr("fill-opacity",0.45).attr("stroke","#54278F"),
    u=>u, ex=>ex.transition().duration(300).attr("r",0).remove())
   .on("mousemove",function(e,p){
      tip.style("opacity",1).html(`<b>${p.s}</b><br>${p.y} stops<br>${p.e} electoral votes<br>margin ${p.x.toFixed(2)} pts`)
         .style("left",Math.min(x(p.x)+14,W-190)+"px").style("top",(y(p.y)-10)+"px");})
   .on("mouseleave",()=>tip.style("opacity",0))
   .transition().duration(600)
    .attr("cx",p=>x(p.x)).attr("cy",p=>y(p.y)).attr("r",p=>rr(p.e));
  const big=D.filter(p=>p.y>=20||p.e>=28||p.x>60);
  lab.selectAll("text").data(big,p=>p.s).join(
    en=>en.append("text").attr("font-size","10.5px").attr("fill","#333").attr("opacity",0),
    u=>u, ex=>ex.remove())
   .transition().duration(600)
    .attr("x",p=>x(p.x)>W-190?x(p.x)-rr(p.e)-3:x(p.x)+rr(p.e)+3)
    .attr("text-anchor",p=>x(p.x)>W-190?"end":"start")
    .attr("y",p=>y(p.y)+4).attr("opacity",1).text(p=>p.s);
  d3.select("#rr").text("correlation between stops and margin: "
    + corr(D.map(p=>p.x),D.map(p=>p.y)).toFixed(2) + "   (n = " + D.length + ")");
}
draw(ALL);
d3.select("#bAll").on("click",()=>draw(ALL));
d3.select("#bNoDC").on("click",()=>draw(ALL.filter(p=>p.s!=="District of Columbia")));
const key=svg.append("g").attr("transform",`translate(${W-190},${M.t+18})`);
key.append("text").attr("x",0).attr("y",-8).attr("font-size","11px").attr("fill","#666")
   .text("circle area = electoral votes");
let kx=6;
[%d,%d,%d].forEach(e=>{
  key.append("circle").attr("cx",kx+rr(e)).attr("cy",22).attr("r",rr(e))
     .attr("fill","#54278F").attr("fill-opacity",0.2).attr("stroke","#54278F");
  key.append("text").attr("x",kx+rr(e)).attr("y",46).attr("text-anchor","middle")
     .attr("font-size","10.5px").attr("fill","#666").text(e);
  kx+=2*rr(e)+16;
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Circle area is electoral votes. Hover for a state. The second button removes one
row and recomputes the correlation; the next section is about why.</p>
', pts, YMAX, max(m$ev), EVK[1], EVK[2], EVK[3]))

## ---- static-scatter
YMAX <- ceiling(max(m$visits) / 10) * 10 + 5   # 85 while the maximum is 80
XMAX <- max(m$absm) * 1.05                     # the same limits the HTML version uses
par(mar = c(4.2, 4.2, 0.6, 1))
symbols(m$absm, m$visits, circles = sqrt(m$ev), inches = 0.22,
        bg = "#54278F55", fg = "#54278F", las = 1,
        xlim = c(0, XMAX), ylim = c(0, YMAX),
        xlab = "margin of victory (percentage points)", ylab = "campaign stops")
big <- m[m$visits >= 20 | m$ev >= 28 | m$absm > 60, ]
# labels near the right edge flip to the left, as they do in the HTML version
text(big$absm, big$visits, big$state, cex = 0.65, col = "grey20",
     pos = ifelse(big$absm > XMAX * 0.75, 2, 4))
# size key: circle AREA is electoral votes, so the radii go as sqrt.
# inches = 0.22 scales to the largest value in the vector passed, and the same
# vector maximum is used here as in the plot above, so the two agree exactly.
kev <- c(3, 20, max(m$ev))
kr  <- 0.22 * sqrt(kev) / sqrt(max(m$ev))                  # radii in inches
kin <- cumsum(c(kr[1], kr[1] + kr[2] + 0.11, kr[2] + kr[3] + 0.11))
u   <- XMAX / par("pin")[1]                                # user units per inch
kx  <- XMAX * 0.40 + kin * u
ky  <- YMAX * 0.87
symbols(kx, rep(ky, 3), circles = sqrt(kev), inches = 0.22,
        add = TRUE, fg = "#54278F", bg = "#54278F22")
text(kx, ky - (kr + 0.10) * (YMAX / par("pin")[2]), kev, cex = 0.6,
     col = "grey35")
text(XMAX * 0.40, YMAX * 0.995, "circle area = electoral votes",
     adj = 0, cex = 0.66, col = "grey35")

## ---- dc
o <- v[v$state == "District of Columbia", c("date", "candidate", "notes")]
o <- head(o[order(o$date), ], 4)
names(o) <- c("date", "candidate", "what the event was")
o

## ---- dc-effect
data.frame(
  sample = c("All 51 jurisdictions", "The 50 states, DC removed"),
  `correlation, stops and margin` = c(r2(cor(m$visits, m$absm)),
                                      r2(cor(mn$visits, mn$absm))),
  check.names = FALSE)

## ---- nebraska
o <- v[v$state == "Nebraska", c("date", "city", "candidate")]
names(o) <- c("date", "city", "candidate")
o

## ---- snapshots
o <- ap[, c("version", "rows", "last_event", "states", "rows_in_shared_window")]
names(o) <- c("AP version", "stops", "last event", "states",
              paste("stops dated on or before", format(LAST_OLD, "%d %b")))
o

## ---- edges
data.frame(
  quantity = c("Last stop logged", "Election day",
               "Biden's last stop, and Harris's first",
               "Democratic ticket stops, Republican ticket stops"),
  value = c(format(max(v$date), "%d %B %Y"), "05 November 2024",
            paste(format(max(v$date[v$candidate == "Biden"]), "%d %B"), "/",
                  format(min(v$date[v$candidate == "Harris"]), "%d %B")),
            paste(sum(v$candidate %in% c("Biden", "Harris", "Walz")), "/",
                  sum(v$candidate %in% c("Trump", "Vance")))))

## ---- calendar-static
X0 <- as.Date("2024-03-01"); X1 <- D1 + 6
par(mar = c(2.4, 5.0, 0.8, 0.8))
plot(NA, xlim = c(X0, X1), ylim = c(6.7, -0.35), axes = FALSE,
     xlab = "", ylab = "")
rect(GAP0 + 0.5, 2.48, GAP1 - 0.5, 5.6, col = "#ededf1", border = NA)
rect(LAST_OLD + 0.5, -0.2, X1, 5.6, col = "#fbe8eb", border = NA)
segments(X0, 1:5, X1, 1:5, col = "grey85")
segments(cd$date, cd$lane, cd$date, cd$lane - cd$k * 0.075,
         col = ifelse(cd$rep, "#C41230", "#2c7fb8"), lwd = 1.6)
mt <- seq(X0, as.Date("2024-11-01"), by = "month")
axis(1, at = mt, labels = format(mt, "%b"), cex.axis = 0.72, col = "grey70",
     col.axis = "grey30", tck = -0.018, mgp = c(3, 0.3, 0))
for (i in seq_along(LANE))
  mtext(LANE[i], side = 2, at = i, las = 1, line = 0.3, cex = 0.72,
        col = if (LANE[i] %in% c("Trump", "Vance")) "#C41230" else "#2c7fb8")
text(X1, -0.05, paste0("after the last day version ", OLD$version, " saw: ",
     added_late, " stops in 3 days, ", pc(100 * added_late / nrow(v)),
     "% of the campaign"), adj = 1, cex = 0.62, col = "#C41230")
gm <- GAP0 + (GAP1 - GAP0) / 2
segments(gm, 5.65, gm, 5.95, col = "grey55")
text(gm, 6.28, paste0("no Democratic nominee\non the trail for ",
     as.numeric(GAP1 - GAP0), " days"), cex = 0.62, col = "grey35")

## ---- calendar-d3
# d3 is loaded once, by the first D3 figure above
X0 <- as.Date("2024-03-01"); X1 <- D1 + 6
kk  <- paste(v$candidate, v$date)
who <- tapply(paste0(gsub('"', "", v$city), ", ", v$postal), kk,
              function(zz) paste(zz, collapse = "; "))
cd$lbl <- who[paste(cd$cand, cd$date)]
J <- paste0('{"c":"', cd$cand, '","l":', cd$lane, ',"k":', cd$k,
            ',"r":', ifelse(cd$rep, 1, 0),
            ',"d":', as.numeric(cd$date - X0),
            ',"t":"', format(cd$date, "%d %b"),
            '","w":"', cd$lbl, '"}', collapse = ",")
mt <- seq(X0, as.Date("2024-11-01"), by = "month")
TK <- paste0('{"p":', as.numeric(mt - X0), ',"l":"', format(mt, "%b"), '"}',
             collapse = ",")
cat(paste0('
<div id="cal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', J, '], TK=[', TK, '];
const SPAN=', as.numeric(X1 - X0), ', G0=', as.numeric(GAP0 - X0),
', G1=', as.numeric(GAP1 - X0), ', V50=', as.numeric(LAST_OLD - X0), ';
const NAMES=["Trump","Vance","Biden","Harris","Walz"];
const W=780,H=300,M={t:42,r:14,b:58,l:74};
const svg=d3.select("#cal").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,SPAN]).range([M.l,W-M.r]);
const lane=i=>M.t+(i-1)*34;
svg.append("rect").attr("x",x(G0+0.5)).attr("y",lane(3)-26)
  .attr("width",x(G1-0.5)-x(G0+0.5)).attr("height",lane(5)-lane(3)+26)
  .attr("fill","#ededf1");
svg.append("rect").attr("x",x(V50+0.5)).attr("y",lane(1)-28)
  .attr("width",W-M.r-x(V50+0.5)).attr("height",lane(5)-lane(1)+28)
  .attr("fill","#fbe8eb");
NAMES.forEach((nm,i)=>{
  svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",lane(i+1))
    .attr("y2",lane(i+1)).attr("stroke","#e2e2e6");
  svg.append("text").attr("x",M.l-10).attr("y",lane(i+1)+4).attr("text-anchor","end")
    .attr("font-size","12px").attr("font-weight","600")
    .attr("fill",i<2?"#C41230":"#2c7fb8").text(nm);
});
TK.forEach(t=>{
  svg.append("line").attr("x1",x(t.p)).attr("x2",x(t.p)).attr("y1",H-M.b+6)
    .attr("y2",H-M.b+11).attr("stroke","#aaa");
  svg.append("text").attr("x",x(t.p)).attr("y",H-M.b+24).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#555").text(t.l);
});
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",H-M.b+6)
  .attr("y2",H-M.b+6).attr("stroke","#aaa");
const tip=d3.select("#cal").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;max-width:280px");
svg.append("g").selectAll("line.s").data(D).join("line").attr("class","s")
  .attr("x1",d=>x(d.d)).attr("x2",d=>x(d.d))
  .attr("y1",d=>lane(d.l)).attr("y2",d=>lane(d.l)-d.k*5.2)
  .attr("stroke",d=>d.r?"#C41230":"#2c7fb8").attr("stroke-width",2.2)
  .attr("stroke-linecap","round")
  .on("mousemove",function(e,d){
     tip.style("opacity",1).html("<b>"+d.c+", "+d.t+"</b><br>"+d.k+
       (d.k>1?" stops: ":" stop: ")+d.w)
       .style("left",Math.min(x(d.d)+12,W-300)+"px")
       .style("top",(lane(d.l)-46)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("text").attr("x",W-M.r).attr("y",11).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#C41230")
  .text("after the last day version ', OLD$version, ' saw: ', added_late,
      ' stops in 3 days, ', pc(100 * added_late / nrow(v)), '% of the campaign");
const gm=(G0+G1)/2;
svg.append("line").attr("x1",x(gm)).attr("x2",x(gm)).attr("y1",lane(5)+6)
  .attr("y2",lane(5)+18).attr("stroke","#888");
const gt=svg.append("text").attr("x",x(gm)).attr("y",lane(5)+32)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#555");
gt.append("tspan").attr("x",x(gm)).text("no Democratic nominee");
gt.append("tspan").attr("x",x(gm)).attr("dy",13)
  .text("on the trail for ', as.numeric(GAP1 - GAP0), ' days");
})();
</script>
'))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#cg text[fill="#333" i],
#cg text[fill="#777" i],
#lz text[fill="#333" i],
#lz text[fill="#c41230" i],
#pm text[fill="#333" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: on the dark page those fills are lifted or pinned and
# already pass, and a --paper stroke would sit dark behind a dark ink there,
# because the checker scores the fill against the stroke it touches.
cat('<style>
#cv text[fill="#666" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#cg text[fill="#777" i],
#cv text[fill="#333" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
