# retirements-code.R -- chunk bodies for retirements-brief.Rmd
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

# Everything here is read from data/, which data/build-data.R wrote from
# Voteview's membership file, ten Federal Election Commission compilations and
# Brookings' Vital Statistics on Congress. Nothing below re-derives anything;
# every figure and every number in the prose is computed from these files at
# the moment this document is rendered.
DP <- read.csv("data/derived/departures.csv",    stringsAsFactors = FALSE)
EY <- read.csv("data/derived/exits_by_year.csv", stringsAsFactors = FALSE)
EX <- read.csv("data/derived/exits.csv",         stringsAsFactors = FALSE)
VS <- read.csv("data/derived/vsoc.csv",          stringsAsFactors = FALSE)
VP <- read.csv("data/derived/vsoc_party.csv",    stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",        stringsAsFactors = FALSE)
CM <- read.csv("data/derived/compare.csv",       stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("checks.csv", "compare.csv", "departures.csv", "dropped.csv", "exits.csv", "exits_by_year.csv", "recovered.csv", "unmatched.csv", "vsoc.csv", "vsoc_party.csv"))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)
ck <- function(k) {
  v <- CK$value[CK$check == k]
  if (length(v) != 1L) stop("checks.csv has no single value for '", k, "'")
  v
}
dy <- function(yr, col) DP[[col]][DP$year == yr]
era <- function(a, b) mean(DP$pct_left[DP$year >= a & DP$year <= b])

# The two ends of the range, named once so the prose and the captions cannot
# drift apart from each other.
HI <- DP[which.max(DP$pct_left), ]

# The one member the prose walks through: Mike Rogers of Michigan, re-elected
# in 2012 and absent from the 2014 candidate file. Two rows, kept in one place
# so the table and the sentences about it cannot disagree.
RG <- EX[EX$bioname == "ROGERS, Mike" & EX$state_abbrev == "MI" &
         EX$year %in% c(2011, 2013), ]
RG <- RG[order(RG$year), ]
LO <- DP[DP$year >= 1900, ][which.min(DP$pct_left[DP$year >= 1900]), ]

# Pooled 2004-2022. The four routes out collapse into two: a member either
# stood on a ballot somebody could beat them on, or did not.
TOTM <- sum(EY$members); TOTL <- sum(EY$left)
BEAT <- sum(EY$lost_general) + sum(EY$denied)
QUIT <- TOTL - BEAT

# The Brookings series, which reaches back to 1946 and counts the same events
# by hand. Early and late windows for the sentence that turns this chapter.
E1 <- VS[VS$year <= 1970, ]; E2 <- VS[VS$year >= 2000, ]

# Retirements against what happened to that party's seats at the same election.
# `vsoc_party` gives House retirements by party; `departures` gives the party's
# seat count in each Congress, so the change across an election is available
# without any outside source. A Congress convening in year Y-1 is the one whose
# members faced the election of year Y.
sc <- DP[, c("year", "dem", "rep")]
sc$next_dem <- c(sc$dem[-1], NA); sc$next_rep <- c(sc$rep[-1], NA)
sc$election <- sc$year + 1
AN <- merge(VP, sc, by.x = "year", by.y = "election")
AN <- AN[AN$year >= 1946 & !is.na(AN$next_dem), ]
AN <- data.frame(
  year = rep(AN$year, 2),
  party = rep(c("D", "R"), each = nrow(AN)),
  retired = c(AN$dem.x, AN$rep.x),
  change  = c(AN$next_dem - AN$dem.y, AN$next_rep - AN$rep.y))
ANQ <- aggregate(change ~ q, transform(AN,
  q = cut(retired, quantile(retired, 0:4 / 4), include.lowest = TRUE)), mean)
ANF <- lm(change ~ retired, AN)

# Redistricting cycles. The first election on a new map is the one held in a
# year ending in 2, and the question is whether members treat it differently.
VS$redist <- VS$year %% 10 == 2
RD <- aggregate(cbind(retired, lost_primary, lost_general) ~ redist, VS, mean)

# The `occupancy` column's coverage, from a verbatim capture of the committed
# Voteview download: the share of House rows with the column filled, by
# Congress. The cliff between two adjacent Congresses is the finding, and
# LASTC is the last Congress on the filled side of it.
SH <- c(
"share of House rows with `occupancy` filled, by Congress:",
"  110   112   113   114   115   116   117   118   119 ",
"0.989 1.000 1.000 1.000 0.000 0.000 0.000 0.000 0.000 ")
sv <- as.numeric(strsplit(trimws(SH[3]), " +")[[1]])
cg <- as.integer(strsplit(trimws(SH[2]), " +")[[1]])
LASTC <- max(cg[sv > 0.5])            # last Congress with the column populated

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- sources
data.frame(
  source = c("Voteview member file", "FEC, *Federal Elections*",
             "Brookings, *Vital Statistics on Congress*"),
  what = c("Every member of every Congress",
           "Every House candidate, with primary and general votes",
           "Retirements, primary defeats, general defeats, counted"),
  covering = c("1789 to 2025", "2004 to 2022", "1946 to 2024"),
  kind = c("Compiled by researchers, out of roll calls",
           "A federal agency's own publication",
           "Compiled from other people's counts"),
  check.names = FALSE)

## ---- routes
data.frame(
  `what the files show` = c(
    "Not on any House ballot",
    "On the primary ballot, not nominated",
    "Nominated, lost in November",
    "Nominated, won"),
  `what it is called` = c("Did not seek re-election", "Denied renomination",
                          "Defeated in the general election", "Re-elected"),
  check.names = FALSE)

## ---- rawocc2
# The share of House rows with `occupancy` filled, one Congress per row. The
# cliff between two adjacent rows is the finding, and a row is where it reads.
data.frame(Congress = cg,
           Share_of_House_rows_with_occupancy_filled = sprintf("%.3f", sv))

## ---- cleanexit
RG[, c("congress", "state_abbrev", "district_code", "election_year",
       "on_house_ballot", "denied", "ge_win", "outcome")]

## ---- era-table
E <- data.frame(
  period = c("1789–1830", "1831–1870", "1871–1900",
             "1901–1940", "1941–1980", "1981–2024"),
  `share who did not return` = c(
    paste0(pc(era(1789, 1830)), "%"), paste0(pc(era(1831, 1870)), "%"),
    paste0(pc(era(1871, 1900)), "%"), paste0(pc(era(1901, 1940)), "%"),
    paste0(pc(era(1941, 1980)), "%"), paste0(pc(era(1981, 2024)), "%")),
  check.names = FALSE)
E

## ---- fig1-d3
# ---------------------------------------------------------------------------
# THE WHOLE SERIES, 1789 TO THE PRESENT. An area rather than a line, because
# the quantity is a share of a chamber and the region under it is meaningful.
# Pixel coordinates are computed HERE, in R, and handed to D3 as a finished
# path, so this figure and the base-R one below draw the same curve from the
# same numbers and the only difference is that this one can be hovered.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('[%d,%.2f,%d,%d]', DP$year, DP$pct_left, DP$left,
                      DP$members), collapse = ",")
mk <- rbind(
  data.frame(y = 1841, t = "1841: three in four gone"),
  data.frame(y = 1913, t = "direct election of senators"),
  data.frame(y = 1987, t = paste0("1987: ", pc(LO$pct_left), "%")))
anns <- paste(sprintf('{"y":%d,"t":"%s","v":%.2f}', mk$y, mk$t,
                      DP$pct_left[match(mk$y, DP$year)]), collapse = ",")
cat(paste0('
<div id="turn" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '].map(r=>({y:r[0],p:r[1],l:r[2],m:r[3]})),A=[', anns, '];
const W=760,H=380,M={t:22,r:14,b:36,l:48};
const svg=d3.select("#turn").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,80]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickValues([0,20,40,60,80]).tickFormat(d=>d+"%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",13).attr("text-anchor","middle").attr("font-size","11px")
  .attr("fill","#444").text("share of the House that did not return");
svg.append("path").datum(D).attr("fill","#C41230").attr("fill-opacity",0.16)
  .attr("d",d3.area().x(d=>x(d.y)).y0(y(0)).y1(d=>y(d.p)));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",1.7).attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.p)));
A.forEach(a=>{
  svg.append("circle").attr("cx",x(a.y)).attr("cy",y(a.v)).attr("r",3.4)
    .attr("fill","#111");
  svg.append("text").attr("x",x(a.y)+6).attr("y",y(a.v)-7)
    .attr("font-size","10.5px").attr("fill","#111").text(a.t);
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("opacity",0);
const dot=svg.append("circle").attr("r",4).attr("fill","#C41230").attr("opacity",0);
const cap=d3.select("#turn").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="<i>Move across the figure for a Congress-by-Congress readout.</i>";
cap.html(DEF);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const px=d3.pointer(e,this)[0];
    const d=D.reduce((a,c)=>Math.abs(x(c.y)-px)<Math.abs(x(a.y)-px)?c:a);
    rule.attr("x1",x(d.y)).attr("x2",x(d.y)).attr("opacity",1);
    dot.attr("cx",x(d.y)).attr("cy",y(d.p)).attr("opacity",1);
    cap.html("<b>"+d.y+"</b>: "+d.l+" of "+d.m+
      " members did not serve in the next Congress \\u2014 <b>"+
      d.p.toFixed(1)+"%</b>.");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);dot.attr("opacity",0);cap.html(DEF);});
})();
</script>'))

## ---- fig1-static
# Same series, same numbers, base R for the PDF device. ASCII only in the
# annotations: the PDF device drops glyphs outside Latin-1 from plot text.
par(mar = c(3.0, 4.0, 0.8, 0.8), mgp = c(2.4, 0.6, 0))
plot(NA, xlim = range(DP$year), ylim = c(0, 82), xlab = "", las = 1,
     ylab = "share of the House that did not return", yaxt = "n")
axis(2, at = seq(0, 80, 20), labels = paste0(seq(0, 80, 20), "%"), las = 1)
polygon(c(DP$year, rev(DP$year)), c(DP$pct_left, rep(0, nrow(DP))),
        col = "#C4123029", border = NA)
lines(DP$year, DP$pct_left, col = "#C41230", lwd = 1.6)
for (i in 1:2) {
  yy <- c(1841, 1987)[i]
  v  <- DP$pct_left[DP$year == yy]
  points(yy, v, pch = 19, cex = 0.7)
  # 1841 is the peak and its label sits above it; 1987 is the floor and its
  # label has to sit above and to the left, clear of the line.
  text(yy + c(0, -6)[i], v + c(6, 9)[i], sprintf("%d: %s%%", yy, pc(v)),
       cex = 0.62, adj = c(0.3, 1)[i])
  if (i == 2) segments(yy, v + 1.4, yy - 1.2, v + 7.4, col = "#666666", lwd = 0.6)
}

## ---- pooled
o <- data.frame(
  outcome = c("Re-elected", "Did not seek re-election",
              "Defeated in the general election", "Ran for the Senate",
              "Denied renomination", "Died in office"),
  members = c(sum(EY$reelected), sum(EY$not_running), sum(EY$lost_general),
              sum(EY$senate_run), sum(EY$denied), sum(EY$died)))
o$share <- paste0(pc(100 * o$members / TOTM), "%")
o$members <- n(o$members)
o

## ---- fig2-d3
# ---------------------------------------------------------------------------
# BACK TO BACK. Every departure in one cycle, split at a center line: left for
# the members who were never on a ballot anybody could beat them on, right for
# the members somebody beat. The form is chosen because the argument is a
# comparison of two totals, not a trend in one.
# ---------------------------------------------------------------------------
rows <- paste(sprintf(
  '{"y":%d,"nr":%d,"sen":%d,"died":%d,"gen":%d,"pri":%d,"tot":%d}',
  EY$year, EY$not_running, EY$senate_run, EY$died, EY$lost_general,
  EY$denied, EY$left), collapse = ",")
cat(paste0('
<div id="tor" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const W=760,H=340,M={t:46,r:16,b:30,l:16},CX=W/2;
const svg=d3.select("#tor").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const mx=d3.max(D,d=>Math.max(d.nr+d.sen+d.died,d.gen+d.pri));
const s=d3.scaleLinear().domain([0,mx]).range([0,(W/2)-M.l-46]);
const yb=d3.scaleBand().domain(D.map(d=>d.y)).range([M.t,H-M.b]).padding(0.22);
const CL={nr:"#7fa8c9",sen:"#b8cfe0",died:"#dfe9f1",gen:"#C41230",pri:"#f0a0a8"};
const NM={nr:"did not seek re-election",sen:"ran for the Senate",
          died:"died in office",gen:"defeated in November",
          pri:"denied renomination"};
const tip=d3.select("#tor").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
function bar(d,keys,dir){
  let acc=0;
  keys.forEach(k=>{
    const w=s(d[k]);
    svg.append("rect").attr("x",dir<0?CX-acc-w:CX+acc).attr("y",yb(d.y))
      .attr("width",Math.max(w,0)).attr("height",yb.bandwidth())
      .attr("fill",CL[k]).style("cursor","pointer")
      .on("mousemove",function(e){
        tip.style("opacity",1).html("<b>"+d.y+"</b><br>"+d[k]+" "+NM[k])
          .style("left",Math.min(e.offsetX+14,W-230)+"px")
          .style("top",(e.offsetY-6)+"px");})
      .on("mouseleave",()=>tip.style("opacity",0));
    acc+=w;
  });
  return acc;
}
D.forEach(d=>{
  const a=bar(d,["nr","sen","died"],-1), b=bar(d,["gen","pri"],1);
  svg.append("text").attr("x",CX-a-6).attr("y",yb(d.y)+yb.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","11px")
    .text(d.nr+d.sen+d.died);
  svg.append("text").attr("x",CX+b+6).attr("y",yb(d.y)+yb.bandwidth()/2+4)
    .attr("font-size","11px").text(d.gen+d.pri);
});
svg.append("line").attr("x1",CX).attr("x2",CX).attr("y1",M.t-6).attr("y2",H-M.b)
  .attr("stroke","#111").attr("stroke-width",1);
D.forEach(d=>svg.append("text").attr("x",CX).attr("y",yb(d.y)+yb.bandwidth()/2+4)
  .attr("text-anchor","middle").attr("font-size","11px").attr("font-weight","600")
  .attr("fill","#fff").attr("stroke","#111").attr("stroke-width",3)
  .attr("paint-order","stroke").text(d.y));
svg.append("text").attr("x",CX-14).attr("y",26).attr("text-anchor","end")
  .attr("font-size","12.5px").attr("font-weight","600")
  .text("left without facing a voter");
svg.append("text").attr("x",CX+14).attr("y",26).attr("font-size","12.5px")
  .attr("font-weight","600").text("beaten");
const lg=d3.select("#tor").append("div").attr("style",
  "margin-top:4px;font-size:12px;color:#444");
lg.html(Object.keys(NM).map(k=>
  "<span style=\\"display:inline-block;margin-right:12px\\">"+
  "<span style=\\"display:inline-block;width:11px;height:11px;background:"+CL[k]+
  ";border:1px solid #999\\"></span> "+NM[k]+"</span>").join(""));
})();
</script>'))

## ---- fig2-static
# The same split, the same numbers, base R. No em dashes in plot text: the PDF
# device renders them as a missing glyph.
par(mar = c(2.6, 0.6, 2.6, 0.6), mgp = c(2.2, 0.6, 0))
CL <- c(nr = "#7fa8c9", sen = "#b8cfe0", died = "#dfe9f1",
        gen = "#C41230", pri = "#f0a0a8")
L <- EY$not_running + EY$senate_run + EY$died
R <- EY$lost_general + EY$denied
mx <- max(L, R)
plot(NA, xlim = c(-mx * 1.28, mx * 1.28), ylim = c(0.4, nrow(EY) + 0.9),
     axes = FALSE, xlab = "", ylab = "")
for (i in seq_len(nrow(EY))) {
  yy <- nrow(EY) - i + 1
  a <- 0
  for (k in c("nr", "sen", "died")) {
    v <- EY[[c(nr = "not_running", sen = "senate_run", died = "died")[k]]][i]
    rect(-a - v, yy - 0.36, -a, yy + 0.36, col = CL[[k]], border = "#ffffff")
    a <- a + v
  }
  b <- 0
  for (k in c("gen", "pri")) {
    v <- EY[[c(gen = "lost_general", pri = "denied")[k]]][i]
    rect(b, yy - 0.36, b + v, yy + 0.36, col = CL[[k]], border = "#ffffff")
    b <- b + v
  }
  text(-a - mx * 0.05, yy, L[i], cex = 0.62, adj = 1)
  text(b + mx * 0.05, yy, R[i], cex = 0.62, adj = 0)
  text(0, yy, EY$year[i], cex = 0.6, font = 2, col = "#ffffff")
}
abline(v = 0, col = "#111111")
mtext("left without facing a voter", 3, line = 0.6, at = -mx * 0.5, cex = 0.7,
      font = 2)
mtext("beaten", 3, line = 0.6, at = mx * 0.5, cex = 0.7, font = 2)
legend("bottom", horiz = TRUE, bty = "n", cex = 0.52, inset = -0.02,
       legend = c("did not seek re-election", "ran for the Senate",
                  "died in office", "defeated in November",
                  "denied renomination"),
       fill = CL, border = "#999999")

## ---- thenandnow
data.frame(
  ` ` = c("Retired", "Defeated in a primary", "Defeated in November"),
  `1946–1970, per election` = c(pc(mean(E1$retired)), pc(mean(E1$lost_primary)),
                                pc(mean(E1$lost_general))),
  `2000–2024, per election` = c(pc(mean(E2$retired)), pc(mean(E2$lost_primary)),
                                pc(mean(E2$lost_general))),
  check.names = FALSE)

## ---- redist
data.frame(
  election = c("First on a new map (years ending in 2)", "All other years"),
  `retirements` = c(pc(RD$retired[RD$redist]), pc(RD$retired[!RD$redist])),
  `primary defeats` = c(pc(RD$lost_primary[RD$redist]),
                        pc(RD$lost_primary[!RD$redist])),
  `general defeats` = c(pc(RD$lost_general[RD$redist]),
                        pc(RD$lost_general[!RD$redist])),
  check.names = FALSE)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# ONE POINT PER PARTY PER ELECTION. Retirements against that party's seat
# change at the same election, 1946 to 2022, with the quartile means drawn as
# a step so the reader can see that the relationship is a tail rather than a
# trend. Both series are computed in R; D3 draws them.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"y":%d,"p":"%s","r":%d,"c":%d}', AN$year, AN$party,
                      AN$retired, AN$change), collapse = ",")
qs <- quantile(AN$retired, 0:4 / 4)
qr <- paste(sprintf('{"a":%.1f,"b":%.1f,"m":%.2f}', qs[-5], qs[-1], ANQ$change),
            collapse = ",")
cat(paste0('
<div id="ant" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '],Q=[', qr, '];
const W=760,H=390,M={t:18,r:16,b:38,l:52};
const svg=d3.select("#ant").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.r)+2]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain(d3.extent(D,d=>d.c)).nice().range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#bbb").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("that party\\u2019s House retirements at this election");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#444").text("seats that party gained or lost");
Q.forEach(q=>{
  svg.append("line").attr("x1",x(q.a)).attr("x2",x(q.b)).attr("y1",y(q.m))
    .attr("y2",y(q.m)).attr("stroke","#111").attr("stroke-width",2.6);
});
const tip=d3.select("#ant").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.r)).attr("cy",d=>y(d.c)).attr("r",4.2)
  .attr("fill",d=>d.p==="D"?"#2c7fb8":"#C41230").attr("fill-opacity",0.72)
  .attr("stroke","#fff").attr("stroke-width",0.7).style("cursor","pointer")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.y+"</b>, "+(d.p==="D"?"Democrats":"Republicans")+
      "<br>"+d.r+" retirements<br>"+(d.c>0?"+":"")+d.c+" seats")
      .style("left",Math.min(e.offsetX+14,W-210)+"px").style("top",(e.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
const lg=svg.append("g").attr("transform","translate("+(M.l+10)+","+(M.t+8)+")");
[["#2c7fb8","Democrats"],["#C41230","Republicans"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",5).attr("cy",i*16).attr("r",4.2).attr("fill",r[0]);
  lg.append("text").attr("x",15).attr("y",i*16+4).attr("font-size","11.5px").text(r[1]);
});
lg.append("line").attr("x1",0).attr("x2",12).attr("y1",34).attr("y2",34)
  .attr("stroke","#111").attr("stroke-width",2.6);
lg.append("text").attr("x",17).attr("y",38).attr("font-size","11.5px")
  .text("mean within each quarter of the range");
})();
</script>'))

## ---- fig3-static
par(mar = c(3.4, 3.8, 0.8, 0.8), mgp = c(2.3, 0.6, 0))
plot(AN$retired, AN$change, pch = 19, cex = 0.8, las = 1,
     col = ifelse(AN$party == "D", "#2c7fb8bb", "#C41230bb"),
     xlab = "that party's House retirements at this election",
     ylab = "seats that party gained or lost")
abline(h = 0, col = "#bbbbbb", lty = 2)
qs <- quantile(AN$retired, 0:4 / 4)
for (i in 1:4) segments(qs[i], ANQ$change[i], qs[i + 1], ANQ$change[i],
                        lwd = 2.4, col = "#111111")
legend("topright", c("Democrats", "Republicans",
                     "mean within each quarter of the range"),
       pch = c(19, 19, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 2.4),
       col = c("#2c7fb8", "#C41230", "#111111"), bty = "o", bg = "#ffffff",
       box.col = "#dddddd", cex = 0.66)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
