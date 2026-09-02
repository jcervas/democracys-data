# sparklines-code.R -- chunk bodies for sparklines-brief.Rmd
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
su <- read.csv("data/derived/states.csv",   stringsAsFactors = FALSE)
nt <- read.csv("data/derived/national.csv", stringsAsFactors = FALSE)
fx <- read.csv("data/derived/facts.csv",    stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("facts.csv", "national.csv", "series.csv", "states.csv"))

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
sg <- function(x) sprintf("%+.1f", as.numeric(x))
n_fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

# Perot's national share, read from the file rather than remembered, because
# the sentence that quotes it is an argument for the two-party denominator.
pn <- read.csv("../historical-campaigns/data/derived/pres_national.csv",
               stringsAsFactors = FALSE)
PEROT <- pn$pop_per[pn$year == 1992 & pn$party == "Independent"]

FROM <- fn("from"); TO <- fn("to"); NY <- fn("elections"); NS <- fn("states")
UP <- f("up_state"); UPC <- fn("up_change"); UPF <- fn("up_first"); UPL <- fn("up_last")
DN <- f("dn_state"); DNC <- fn("dn_change"); DNF <- fn("dn_first"); DNL <- fn("dn_last")
STB <- f("stable_state"); STBR <- fn("stable_range")
NLO <- fn("nat_lo"); NLOY <- fn("nat_lo_year")
NHI <- fn("nat_hi"); NHIY <- fn("nat_hi_year")
MSAME <- fn("median_same"); MINSAME <- fn("min_same")
FLIPMAX <- fn("flips_max"); FLIPST <- f("flips_max_state")
NEVER <- fn("never_flipped")
RELDN <- f("reldn_state"); RELDNC <- fn("reldn_change")

# The state that fell furthest, walked through by hand in the prose: its own
# share and the country's in the first and last elections, read off its rows
# so the arithmetic on the page is the arithmetic in the table.
dz <- se[se$state_abbrev == DN & se$year %in% c(FROM, TO), ]
dz <- dz[order(dz$year), ]
stopifnot(nrow(dz) == 2)

YEARS <- sort(unique(se$year))
ACC <- "#1C4C5C"; DEMC <- "#2c7fb8"; REPC <- "#A33B2A"
GRY <- "#8A8F94"; WARN <- "#C41230"

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
z <- se[se$state_abbrev == "PA", ][1:3, ]
data.frame(
  Year = z$year, State = z$state_abbrev,
  Democratic_share_of_two_party = paste0(p1(z$two), "%"),
  National_that_year = paste0(p1(z$national_two), "%"),
  State_minus_national = sg(z$rel))

## ---- varmap
data.frame(
  Column = c("year", "state_abbrev", "two", "national_two", "rel"),
  What_it_holds = c(
    "the presidential election year",
    "the state, or DC",
    "the Democratic share of the votes cast for the two major parties",
    "the same quantity for the country, from the national popular vote",
    "the state's share minus the national one, in points"),
  Measurement = c("discrete", "categorical", "continuous", "continuous",
                  "continuous"))

## ---- fig1-static
op <- par(mar = c(3.8, 4.4, 1.2, 1.2), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(YEARS), ylim = range(se$two), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(1976, TO, 8), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Democratic share of the two-party vote", 2, line = 2.9, cex = 0.88)
abline(h = 50, col = "#CBD3D8")
for (s in unique(se$state_abbrev)) {
  z <- se[se$state_abbrev == s, ]
  lines(z$year, z$two, col = paste0(GRY, "88"), lwd = 1)
}
lines(nt$year, nt$national_two, col = WARN, lwd = 2.6)
text(TO, nt$national_two[nrow(nt)], " national", col = WARN, pos = 4,
     cex = 0.76, xpd = NA)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Fifty-one lines on one axis. Hovering isolates one, which is the only way to
# read anything here -- and needing to hover is the argument for the next
# figure rather than a defect of this one.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
ser <- vapply(sort(unique(se$state_abbrev)), function(s) {
  z <- se[se$state_abbrev == s, ]
  z <- z[order(z$year), ]
  paste0('{s:"', s, '",v:[', paste(round(z$two, 2), collapse = ","), ']}')
}, character(1))
cat(paste0('
<div id="sp" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', paste(ser, collapse = ","), '];
const NAT=[', paste(round(nt$national_two, 2), collapse = ","), '];
const YR=[', paste(YEARS, collapse = ","), '];
const GRY="', GRY, '", WARN="', WARN, '";
const W=770,H=420,M={t:18,r:78,b:50,l:60};
const box=d3.select("#sp");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const all=S.flatMap(s=>s.v);
const x=d3.scaleLinear().domain(d3.extent(YR)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([d3.min(all)-2,d3.max(all)+2]).range([H-M.b,M.t]);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#CBD3D8");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(7));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63")
  .text("Democratic share of the two-party vote");
const ln=d3.line().x((d,i)=>x(YR[i])).y(d=>y(d));
const paths=svg.append("g").selectAll("path").data(S).join("path")
  .attr("fill","none").attr("stroke",GRY).attr("stroke-opacity",0.5)
  .attr("stroke-width",1).attr("d",d=>ln(d.v));
svg.append("path").attr("fill","none").attr("stroke",WARN).attr("stroke-width",2.6)
  .attr("d",ln(NAT));
svg.append("text").attr("x",x(YR[YR.length-1])+7).attr("y",y(NAT[NAT.length-1])+4)
  .attr("font-size","12px").attr("font-weight","600").attr("fill",WARN)
  .text("national");
const nm=svg.append("text").attr("x",M.l+6).attr("y",M.t+12)
  .attr("font-size","13px").attr("font-weight","700").attr("fill","currentColor");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const p=d3.pointer(e,this), px=p[0]+M.l, py=p[1]+M.t;
    const yr=YR.reduce((a,b)=>Math.abs(x(b)-px)<Math.abs(x(a)-px)?b:a);
    const i=YR.indexOf(yr);
    let best=null,bd=1e9;
    S.forEach(function(s){ const d=Math.abs(y(s.v[i])-py);
                           if(d<bd){bd=d;best=s;} });
    paths.attr("stroke",d=>d===best?"#1C4C5C":GRY)
         .attr("stroke-opacity",d=>d===best?1:0.16)
         .attr("stroke-width",d=>d===best?2.4:1);
    nm.text(best.s+" \\u00b7 "+yr+" \\u00b7 "+best.v[i].toFixed(1)+"%");
  })
  .on("mouseleave",function(){
    paths.attr("stroke",GRY).attr("stroke-opacity",0.5).attr("stroke-width",1);
    nm.text("");
  });
})();
</script>'))

## ---- fig2-static
op <- par(mar = c(0.4, 0.4, 1.4, 0.4))
o <- su[order(-su$change), ]
NC <- 3; NR <- ceiling(nrow(o) / NC)
plot(NA, xlim = c(0, NC), ylim = c(NR + 0.5, -0.6), axes = FALSE,
     xlab = "", ylab = "")
rng <- range(se$two)
for (i in seq_len(nrow(o))) {
  cx <- (i - 1) %/% NR; cy <- (i - 1) %% NR + 1
  z <- se[se$state_abbrev == o$state[i], ]
  z <- z[order(z$year), ]
  x0 <- cx + 0.20; x1 <- cx + 0.56
  yy <- cy + 0.30 - 0.60 * (z$two - rng[1]) / diff(rng)
  xx <- x0 + (x1 - x0) * (z$year - min(YEARS)) / diff(range(YEARS))
  # the 50% line, so a reader can see which side of it the line sits
  y50 <- cy + 0.30 - 0.60 * (50 - rng[1]) / diff(rng)
  segments(x0, y50, x1, y50, col = "#DDE3E6", lwd = 0.7)
  lines(xx, yy, col = ACC, lwd = 1.1)
  points(xx[length(xx)], yy[length(yy)],
         col = ifelse(o$last[i] > 50, DEMC, REPC), pch = 19, cex = 0.42)
  text(cx + 0.17, cy, o$state[i], adj = 1, cex = 0.62)
  text(cx + 0.60, cy, sprintf("%.0f", o$first[i]), adj = 0, cex = 0.56,
       col = "#76838C")
  text(cx + 0.72, cy, sprintf("%.0f", o$last[i]), adj = 0, cex = 0.56,
       col = "#76838C")
  text(cx + 0.86, cy, sg(o$change[i]), adj = 0, cex = 0.56,
       col = ifelse(o$change[i] > 0, DEMC, REPC))
}
# a plain hyphen, not an en dash: the PDF device has no U+2013 and substitutes
# one silently, so the label would differ between print and screen
mtext(paste0("Democratic share of the two-party vote, ", FROM, "-", TO,
             "  ·  each line spans the same years and the same scale"),
      3, line = 0.2, cex = 0.74, adj = 0)
par(op)

## ---- fig2-d3
# The sparkline table. Every line is drawn on the SAME vertical scale -- if
# each were scaled to its own range the shapes would be incomparable, which is
# the commonest way to get this form wrong.
#
# The sort control is the reason the table beats the small-multiple grid: the
# rows carry an order, and changing it asks a different question of the same
# ink.
rows <- vapply(su$state, function(s) {
  z <- se[se$state_abbrev == s, ]; z <- z[order(z$year), ]
  r <- su[su$state == s, ]
  paste0('{s:"', s, '",v:[', paste(round(z$two, 2), collapse = ","),
         '],r:[', paste(round(z$rel, 2), collapse = ","),
         '],f:', r$first, ',l:', r$last, ',c:', r$change,
         ',rc:', r$rel_change, ',rg:', r$range, ',fl:', r$flips, '}')
}, character(1))
cat(paste0('
<div id="tbl" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', paste(rows, collapse = ","), '];
const YR=[', paste(YEARS, collapse = ","), '];
const ACC="', ACC, '", DEMC="', DEMC, '", REPC="', REPC, '";
const box=d3.select("#tbl");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:8px;font:12px inherit;flex-wrap:wrap");
let mode="two", sortk="c";
const wrap=box.append("div")
  .attr("style","display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:2px 18px");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const SW=76, SH=17;
function draw(){
  const key = mode==="two" ? "v" : "r";
  // ONE scale for every row, so the shapes can be compared
  const all=D.flatMap(d=>d[key]);
  const lo=Math.min(...all), hi=Math.max(...all);
  const x=d3.scaleLinear().domain([0,YR.length-1]).range([1,SW-1]);
  const y=d3.scaleLinear().domain([lo,hi]).range([SH-2,2]);
  const ref = mode==="two" ? 50 : 0;
  const sorted=D.slice().sort((a,b)=>
    sortk==="s" ? d3.ascending(a.s,b.s) : d3.descending(a[sortk],b[sortk]));
  const row=wrap.selectAll("div.row").data(sorted,d=>d.s)
    .join("div").attr("class","row")
    .attr("style","display:flex;align-items:center;gap:7px;padding:1px 0;font:11.5px inherit");
  row.html("");
  row.append("span").attr("style","width:26px;font-weight:600").text(d=>d.s);
  row.append("span").html(function(d){
    const v=d[key];
    const pts=v.map((q,i)=>x(i).toFixed(1)+","+y(q).toFixed(1)).join(" ");
    const last=v[v.length-1];
    // Template literals with double-quoted attributes: a single quote here
    // would close the R string this whole script is being pasted into.
    const yr0=y(ref).toFixed(1);
    return `<svg width="${SW}" height="${SH}" style="vertical-align:middle">` +
      `<line x1="1" x2="${SW-1}" y1="${yr0}" y2="${yr0}" ` +
        `stroke="#DDE3E6" stroke-width="0.8"/>` +
      `<polyline points="${pts}" fill="none" stroke="${ACC}" ` +
        `stroke-width="1.2"/>` +
      `<circle cx="${x(v.length-1).toFixed(1)}" cy="${y(last).toFixed(1)}" ` +
        `r="2" fill="${last>ref?DEMC:REPC}"/></svg>`;
  });
  row.append("span").attr("style","width:30px;text-align:right;color:#76838C")
     .text(d=>d[key][0].toFixed(0));
  row.append("span").attr("style","width:30px;text-align:right;color:#76838C")
     .text(d=>d[key][d[key].length-1].toFixed(0));
  row.append("span")
     .attr("style",d=>"width:38px;text-align:right;font-weight:600;color:"+
           ((mode==="two"?d.c:d.rc)>0?DEMC:REPC))
     .text(d=>d3.format("+.0f")(mode==="two"?d.c:d.rc));
  row.on("mousemove",function(e,d){
      const r=box.node().getBoundingClientRect();
      tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
         .style("top",(e.clientY-r.top-8)+"px")
         .html("<b>"+d.s+"</b><br>"+
           YR.map((yy,i)=>yy+": "+d.v[i].toFixed(1)+"%").join("<br>")+
           "<br><br>range "+d.rg.toFixed(1)+" points, changed side "+
           d.fl+" time"+(d.fl===1?"":"s"));
    })
    .on("mouseleave",function(){tip.style("opacity",0);});
}
bar.append("span").attr("style","color:#76838C").text("sort:");
[["c","by change"],["rg","by how far it wandered"],["l","by where it ended"],
 ["s","alphabetical"]].forEach(function(o){
  bar.append("button")
    .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
    .text(o[1]).on("click",function(){ sortk=o[0]; draw(); });
});
bar.append("button")
  .attr("style","margin-left:6px;padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text("show against the national")
  .on("click",function(){
    mode = mode==="two" ? "rel" : "two";
    d3.select(this).text(mode==="two"?"show against the national"
                                     :"show the raw share");
    draw();
  });
draw();
})();
</script>'))

## ---- fig3-static
op <- par(mar = c(3.8, 4.4, 1.2, 1.2), mgp = c(2.6, 0.7, 0))
plot(nt$year, nt$national_two, type = "o", pch = 19, cex = 0.7, col = WARN,
     lwd = 2.2, axes = FALSE, xlab = "", ylab = "", ylim = c(38, 58))
axis(1, at = seq(1976, TO, 8), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
abline(h = 50, col = "#CBD3D8")
mtext("national Democratic share of the two-party vote", 2, line = 2.9,
      cex = 0.86)
text(NLOY, NLO - 1.6, paste0(NLOY, ": ", p1(NLO), "%"), cex = 0.7, col = WARN)
text(NHIY, NHI + 1.6, paste0(NHIY, ": ", p1(NHI), "%"), cex = 0.7, col = WARN)
par(op)
