# vote-targeting-code.R -- chunk bodies for vote-targeting-brief.Rmd
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
nc <- read.csv("data/derived/nc_gov_county.csv", stringsAsFactors = FALSE)

sw <- aggregate(cbind(dem, rep, oth, total) ~ year, nc, sum)
sw$rep_pct <- 100 * sw$rep / sw$total
sw$dem_pct <- 100 * sw$dem / sw$total
sw$oth_pct <- 100 * sw$oth / sw$total

tr <- tapply(nc$rep, nc$year, sum); td <- tapply(nc$dem, nc$year, sum)
nc$rs <- nc$rep / tr[as.character(nc$year)]
nc$ds <- nc$dem / td[as.character(nc$year)]
nc$rp <- nc$rep / nc$total

ar <- sort(tapply(nc$rs, nc$county, mean), decreasing = TRUE)   # contribution, R
ad <- sort(tapply(nc$ds, nc$county, mean), decreasing = TRUE)   # contribution, D
bp <- sort(tapply(nc$rp, nc$county, mean), decreasing = TRUE)   # R percentage

TARGET  <- 3200000
tgt     <- ar * TARGET
best    <- max(sw$rep); best_yr <- sw$year[which.max(sw$rep)]
rep24   <- sw$rep[sw$year == 2024]
a24     <- nc[nc$year == 2024, ]; rownames(a24) <- a24$county

# drop 2024 and recompute
sub  <- nc[nc$year != 2024, ]
tr2  <- tapply(sub$rep, sub$year, sum)
sub$s <- sub$rep / tr2[as.character(sub$year)]
ar2  <- tapply(sub$s, sub$county, mean)

half_r <- which(cumsum(ar) >= .5)[1]
half_d <- which(cumsum(ad) >= .5)[1]

# how much each county's assigned share moves when 2024 is dropped, against
# how big the county is: one election moving the whole plan
csize  <- setNames(a24$total, a24$county)
cshift <- data.frame(county = names(ar),
                     with    = 100 * as.numeric(ar),
                     without = 100 * as.numeric(ar2[names(ar)]),
                     size    = as.numeric(csize[names(ar)]),
                     stringsAsFactors = FALSE)
cshift$rel <- 100 * (cshift$without / cshift$with - 1)
n_gain   <- sum(cshift$rel > 0)
med_gain <- median(cshift$size[cshift$rel > 0])
med_lose <- median(cshift$size[cshift$rel <= 0])

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")
tt <- function(cty) gsub("\\b([a-z])", "\\U\\1", tolower(cty), perl = TRUE)


# ---- the colors, fixed once ----------------------------------------------
# Democratic and Republican were being drawn with one blue/red pair in the
# first figure and a different blue/red pair in the second. One encoding, one
# pair. Here the two categories really are the two parties, so party colors
# are the honest choice; they are simply the SAME two everywhere.
DEMC <- "#2c7fb8"
REPC <- "#C41230"
OTHC <- "#4d9221"    # everyone else

# ---- one caption per figure, written once, printed in BOTH formats ---------
# All five of these lived only inside the d3 chunks, so the PDF, which is the
# format students print, was the weaker half of every pair.
cap_conc <- paste(
  "How much of each party's statewide vote the top N counties supply.",
  "The two curves are close: concentration is a fact about where people live,",
  "not about which party they vote for.")
cap_gap <- sprintf(paste(
  "Four elections, none of them within %s votes of the target. The tallest bar",
  "is %d."), n(TARGET - best), best_yr)
cap_shift <- sprintf(paste(
  "One dot per county. Only %d of the 100 counties gain when 2024 is dropped,",
  "and they are the large ones: a typical gainer cast %s votes in 2024 against",
  "%s for a county that loses."), n_gain, n(med_gain), n(med_lose))

# every static figure ends with this, so print carries what the screen carries
subcap <- function(txt, width = 92, line = 4.4, cex = 0.66) {
  cw <- strwrap(txt, width = width)
  mtext(cw, side = 1, line = line + (seq_along(cw) - 1) * 0.95, adj = 0,
        cex = cex, col = "#555555")
}

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

## ---- rawnc
# Verbatim captures of the first lines of two vintages of the same series.
# In the second block the file's TAB separators are printed as " | " so the
# field boundaries are visible on the page; nothing else is changed.
R12 <- c(
'"county","precinct","contest_type","runoff_status ","recount_status",',
'"contest","choice","winner_status","party","Election Day","One Stop",',
'"Absentee by Mail","Provisional","total votes","district"',
'"ALAMANCE","01_PATTERSON","S","0","0","PRESIDENT AND VICE PRESIDENT OF',
'THE UNITED STATES","Obama/Biden","0","DEM",225,237,21,5,488,"Not Found"')
R24 <- c(
"County | Election Date | Precinct | Contest Group ID | Contest Type |",
"Contest Name | Choice | Choice Party | Vote For | Election Day |",
"Early Voting | Absentee by Mail | Provisional | Total Votes |",
"Real Precinct |",
"BUNCOMBE | 11/05/2024 | 01.1 | 7 | C | CITY OF ASHEVILLE CITY COUNCIL |",
"Write-In (Miscellaneous) |  | 3 | 6 | 14 | 1 | 0 | 21 | Y |")
# The column names down the page against the values they carry. The comparison
# the next paragraph asks for is name against name between two vintages, and
# that reads down a column, not across two blocks of wrapped text.
data.frame(
  Position = seq_len(15),
  Column_as_it_arrives = c("county", "precinct", "contest_type",
    "runoff_status ", "recount_status", "contest", "choice", "winner_status",
    "party", "Election Day", "One Stop", "Absentee by Mail", "Provisional",
    "total votes", "district"),
  What_it_holds = c("the county reporting", "the precinct within it",
    "the kind of contest — S for statewide",
    "whether this is a runoff — note the trailing space in the name",
    "whether this is a recount", "the office, spelled out",
    "the candidate or option", "whether this choice won",
    "the candidate's party", "votes cast on election day",
    "votes cast early — North Carolina's old name for early voting",
    "votes cast by mail", "provisional ballots",
    "the four vote columns added up", "district, where the contest has one"),
  Value_in_row_1 = c("ALAMANCE", "01_PATTERSON", "S", "0", "0",
    "PRESIDENT AND VICE PRESIDENT OF THE UNITED STATES", "Obama/Biden", "0",
    "DEM", "225", "237", "21", "5", "488", "Not Found"))

## ---- rawnc2
# The 2024 file is tab-separated; the capture prints its tabs as " | ".
data.frame(
  Position = seq_len(15),
  Column_as_it_arrives = c("County", "Election Date", "Precinct",
    "Contest Group ID", "Contest Type", "Contest Name", "Choice",
    "Choice Party", "Vote For", "Election Day", "Early Voting",
    "Absentee by Mail", "Provisional", "Total Votes", "Real Precinct"),
  What_it_holds = c("the county reporting", "the date of the election — new in this vintage",
    "the precinct within it", "an identifier grouping related contests — new",
    "the kind of contest — C for county", "the office, spelled out",
    "the candidate or option", "the candidate's party",
    "how many may be elected — new",
    "votes cast on election day",
    "votes cast early — the same quantity 2012 called One Stop",
    "votes cast by mail", "provisional ballots", "the four vote columns added up",
    "whether this row is a precinct or an aggregate — new"),
  Value_in_row_1 = c("BUNCOMBE", "11/05/2024", "01.1", "7", "C",
    "CITY OF ASHEVILLE CITY COUNCIL", "Write-In (Miscellaneous)", "(empty)",
    "3", "6", "14", "1", "0", "21", "Y"))

## ---- cleannc
nc[nc$county == "ALAMANCE", ]

## ---- one-row
o <- nc[nc$county == "WAKE" & nc$year == 2020,
        c("year", "county", "dem", "rep", "oth", "total")]
o$county <- tt(o$county)
o[] <- lapply(o, function(x) if (is.numeric(x)) n(x) else x)
names(o) <- c("year", "county", "Democratic", "Republican", "other", "total")
o

## ---- statewide
o <- data.frame(year = sw$year,
                Democratic = n(sw$dem), Republican = n(sw$rep),
                other = n(sw$oth),
                rep_pct = pc(sw$rep_pct), oth_pct = pc(sw$oth_pct, 2))
names(o) <- c("year", "Democratic", "Republican", "other", "Republican %",
              "other %")
o

## ---- elect-static
par(mar = c(7.2, 4.4, 0.8, 6.6))
plot(NA, xlim = c(2012, 2024), ylim = c(0, 60), las = 1, xaxt = "n",
     xlab = "", ylab = "% of all votes cast for governor")
axis(1, at = sw$year)
abline(h = 50, lty = 3, col = "grey65")
ser <- list(list(v = sw$dem_pct, c = "#2c7fb8", p = 19, l = 1, k = "Democratic"),
            list(v = sw$rep_pct, c = "#C41230", p = 17, l = 2, k = "Republican"),
            list(v = sw$oth_pct, c = "#4d9221", p = 15, l = 4, k = "everyone else"))
for (s in ser) {
  lines(sw$year, s$v, col = s$c, lwd = 2.6, lty = s$l)
  points(sw$year, s$v, col = s$c, pch = s$p, cex = 1.05)
  text(2024.3, s$v[length(s$v)], s$k, col = s$c, adj = c(0, 0.5), cex = 0.8,
       xpd = NA)
}
text(sw$year, sw$oth_pct + 3.4, paste0(pc(sw$oth_pct, 1), "%"), cex = 0.68,
     col = "#4d9221")

## ---- elect-d3
ser <- sprintf(
  '[{"k":"Democratic","c":"#2c7fb8","d":null,"v":[%s]},
    {"k":"Republican","c":"#C41230","d":"7,4","v":[%s]},
    {"k":"everyone else","c":"#4d9221","d":"2,3","v":[%s]}]',
  paste(sprintf('[%d,%.2f]', sw$year, sw$dem_pct), collapse = ","),
  paste(sprintf('[%d,%.2f]', sw$year, sw$rep_pct), collapse = ","),
  paste(sprintf('[%d,%.2f]', sw$year, sw$oth_pct), collapse = ","))
cat(sprintf('
<div id="elec" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=%s;
const W=760,H=380,M={t:18,r:120,b:38,l:56};
const box=d3.select("#elec");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain([2012,2016,2020,2024]).range([M.l+10,W-M.r-10]);
const y=d3.scaleLinear().domain([0,60]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% of all votes cast for governor");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#bbb").attr("stroke-dasharray","3,3");
const ln=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
const sym=[d3.symbolCircle,d3.symbolTriangle,d3.symbolSquare];
S.forEach((s,i)=>{
  svg.append("path").attr("d",ln(s.v)).attr("fill","none").attr("stroke",s.c)
    .attr("stroke-width",2.5).attr("stroke-dasharray",s.d);
  svg.append("g").selectAll("path.m").data(s.v).join("path")
    .attr("d",d3.symbol().type(sym[i]).size(46))
    .attr("transform",d=>`translate(${x(d[0])},${y(d[1])})`).attr("fill",s.c);
  const last=s.v[s.v.length-1];
  svg.append("text").attr("x",W-M.r+4).attr("y",y(last[1])+4)
    .attr("font-size","11.5px").attr("fill",s.c).text(s.k);
});
S[2].v.forEach(d=>{
  svg.append("text").attr("x",x(d[0])).attr("y",y(d[1])-12).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#4d9221").text(d[1].toFixed(1)+"%%");
});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#ccc").attr("opacity",0);
svg.append("g").selectAll("rect").data([2012,2016,2020,2024]).join("rect")
  .attr("x",d=>x(d)-28).attr("y",M.t).attr("width",56).attr("height",H-M.b-M.t)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev,yr){
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    tip.style("opacity",1).html(`<b>${yr}</b><br>`+
      S.map(s=>{const f=s.v.find(v=>v[0]===yr);
        return `<span style="color:${s.c}">&#9632;</span> ${s.k}: ${f[1].toFixed(1)}%%`;}).join("<br>"))
      .style("left",Math.min(x(yr)-M.l+18,W-240)+"px").style("top",(M.t+6)+"px"); })
  .on("mouseleave",()=>{rule.attr("opacity",0);tip.style("opacity",0);});
})();
</script>
', ser))

## ---- shares
o <- data.frame(county = tt(names(ar)[1:10]),
                avg_share_pct = pc(100 * ar[1:10], 2))
names(o) <- c("county", "average share of the Republican statewide vote (%)")
o

## ---- concentration
data.frame(
  measure = c("Top 10 counties supply", "Top 25 counties supply",
              "The bottom 50 counties supply",
              "Counties needed to reach half the party's vote"),
  Republican = c(paste0(pc(100 * sum(ar[1:10])), "%"),
                 paste0(pc(100 * sum(ar[1:25])), "%"),
                 paste0(pc(100 * sum(ar[51:100])), "%"), half_r),
  Democratic = c(paste0(pc(100 * sum(ad[1:10])), "%"),
                 paste0(pc(100 * sum(ad[1:25])), "%"),
                 paste0(pc(100 * sum(ad[51:100])), "%"), half_d))

## ---- conc-static
par(mar = c(7.8, 4.6, 1.0, 1.2))
plot(seq_along(ar), 100 * cumsum(ar), type = "l", lwd = 2.6, col = REPC,
     xlab = "counties, ranked by contribution", ylab = "cumulative % of the party's vote",
     ylim = c(0, 100), las = 1)
lines(seq_along(ad), 100 * cumsum(ad), lwd = 2.6, col = DEMC)
abline(h = 50, lty = 3, col = "grey50")
abline(v = c(half_r, half_d), lty = 3, col = "grey50")
legend("bottomright", c("Republican", "Democratic"),
       col = c(REPC, DEMC), lwd = 2.6, bty = "n", cex = 0.85)
subcap(cap_conc)

## ---- conc-d3
rr <- paste(sprintf('[%d,%.2f]', seq_along(ar), 100 * cumsum(ar)), collapse = ",")
dd <- paste(sprintf('[%d,%.2f]', seq_along(ad), 100 * cumsum(ad)), collapse = ",")
cat(sprintf('
<div id="conc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[{"k":"Republican","c":"%s","v":[%s]},{"k":"Democratic","c":"%s","v":[%s]}];
const W=760,H=420,M={t:18,r:110,b:44,l:56};
const box=d3.select("#conc");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(10));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("counties, ranked by contribution");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("cumulative share of the party vote");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#bbb").attr("stroke-dasharray","4,4");
const ln=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
S.forEach(s=>{ svg.append("path").attr("d",ln(s.v)).attr("fill","none")
  .attr("stroke",s.c).attr("stroke-width",2.6);
  svg.append("text").attr("x",W-M.r+8).attr("y",y(s.v[s.v.length-1][1])+4)
    .attr("font-size","11.5px").attr("fill",s.c).text(s.k); });
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const k=Math.max(1,Math.min(100,Math.round(x.invert(d3.pointer(ev,this)[0]+M.l))));
    tip.style("opacity",1).html(`<b>top ${k} counties</b><br>`+
      S.map(s=>`<span style="color:${s.c}">&#9632;</span> ${s.k}: ${s.v[k-1][1].toFixed(1)}%%`).join("<br>"))
      .style("left",Math.min(x(k)-M.l+18,W-240)+"px").style("top",(M.t+8)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Move across the chart to read the value at any N.</p>
', REPC, rr, DEMC, dd, cap_conc))

## ---- targets
o <- data.frame(county = tt(names(tgt)[1:8]),
                share = pc(100 * ar[1:8], 2),
                target = n(tgt[1:8]))
names(o) <- c("county", "average share (%)", "votes needed")
o

## ---- reality
o <- data.frame(year = sw$year, rep_votes = n(sw$rep),
                short = n(TARGET - sw$rep))
names(o) <- c("year", "Republican votes cast", "short of the target")
o

## ---- gap-static
par(mar = c(7.2, 4.6, 1.4, 1))
b <- barplot(sw$rep / 1e6, names.arg = sw$year, ylim = c(0, 3.5),
             col = "#bdbdbd", border = "white", las = 1,
             ylab = "Republican votes for governor (millions)")
abline(h = TARGET / 1e6, col = "#C41230", lwd = 2.4, lty = 2)
text(b[1] - 0.4, TARGET / 1e6 + 0.13, paste0("the plan's target: ", n(TARGET)),
     adj = c(0, 0), cex = 0.78, col = "#C41230")
arrows(b, sw$rep / 1e6, b, TARGET / 1e6, length = 0.05, code = 3,
       col = "#C41230", lwd = 1.3)
text(b + 0.12, (sw$rep / 1e6 + TARGET / 1e6) / 2,
     paste0(n(TARGET - sw$rep), "\nshort"), cex = 0.68, col = "#C41230",
     adj = c(0, 0.5))
text(b, sw$rep / 1e6 - 0.16, n(sw$rep), cex = 0.74, col = "#333333")
text(b[which.max(sw$rep)], max(sw$rep) / 1e6 - 0.34, "best of the four",
     cex = 0.7, col = "#333333")
subcap(cap_gap)

## ---- gap-d3
rows <- paste(sprintf('{"y":%d,"v":%d,"s":%d}', sw$year, sw$rep,
                      TARGET - sw$rep), collapse = ",")
cat(sprintf('
<div id="gap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],T=%d;
const W=760,H=400,M={t:26,r:24,b:40,l:64};
const box=d3.select("#gap");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(D.map(d=>d.y)).range([M.l,W-M.r]).padding(0.32);
const y=d3.scaleLinear().domain([0,3.5]).range([H-M.b,M.t]);
const f=d3.format(",");
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("Republican votes for governor (millions)");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.y)).attr("width",x.bandwidth())
  .attr("y",d=>y(d.v/1e6)).attr("height",d=>y(0)-y(d.v/1e6)).attr("fill","#bdbdbd")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.y}</b><br>${f(d.v)} Republican votes<br>`+
      `${f(d.s)} short of the target`)
      .style("left",Math.min(ev.offsetX+14,W-230)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>x(d.y)+x.bandwidth()/2).attr("y",d=>y(d.v/1e6)+16)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#333")
  .text(d=>f(d.v));
svg.append("g").selectAll("line.a").data(D).join("line").attr("class","a")
  .attr("x1",d=>x(d.y)+x.bandwidth()/2).attr("x2",d=>x(d.y)+x.bandwidth()/2)
  .attr("y1",d=>y(d.v/1e6)).attr("y2",y(T/1e6))
  .attr("stroke","#C41230").attr("stroke-width",1.4);
svg.append("g").selectAll("text.s").data(D).join("text").attr("class","s")
  .attr("x",d=>x(d.y)+x.bandwidth()/2+6)
  .attr("y",d=>(y(d.v/1e6)+y(T/1e6))/2)
  .attr("font-size","11px").attr("fill","#C41230").text(d=>f(d.s)+" short");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(T/1e6)).attr("y2",y(T/1e6))
  .attr("stroke","#C41230").attr("stroke-width",2.2).attr("stroke-dasharray","8,5");
svg.append("text").attr("x",M.l+4).attr("y",y(T/1e6)-7).attr("font-size","11.5px")
  .attr("fill","#C41230").text("the plan&#39;s target: "+f(T));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover for the shortfall.</p>
', rows, TARGET, cap_gap))

## ---- change-target
o <- do.call(rbind, lapply(c(TARGET, sw$dem[sw$year == 2024], best),
  function(T) {
    t <- ar * T
    data.frame(target = n(T),
               a = n(t[names(ar)[1]]), b = n(t[names(ar)[2]]),
               c = n(t["DURHAM"]),
               same_order = identical(names(sort(t, decreasing = TRUE))[1:10],
                                      names(ar)[1:10])) }))
names(o) <- c("statewide target", tt(names(ar)[1]), tt(names(ar)[2]), "Durham",
              "top ten in the same order?")
o

## ---- both-lists
data.frame(rank = 1:6,
           Republican = tt(names(ar)[1:6]),
           Democratic = tt(names(ad)[1:6]))

## ---- rankings
data.frame(rank = 1:5,
           by_contribution = tt(names(ar)[1:5]),
           contribution_pct = pc(100 * ar[1:5], 2),
           by_percentage = tt(names(bp)[1:5]),
           republican_pct = pc(100 * bp[1:5]))

## ---- scatter-static
par(mar = c(7.8, 4.6, 1.0, 1.2))
cn <- names(ar)
plot(100 * bp[cn], 100 * ar[cn], pch = 19, cex = 0.8, col = "#777777",
     xlab = "average Republican share of votes cast in the county (%)",
     ylab = "average share of the party's statewide vote (%)",
     las = 1, xlim = c(20, 80))
hi <- cn[100 * ar[cn] > 2 | 100 * bp[cn] > 70]
points(100 * bp[hi], 100 * ar[hi], pch = 19, cex = 0.9, col = "#C41230")
lb <- cn[100 * ar[cn] > 2]
text(100 * bp[lb], 100 * ar[lb], tt(lb), pos = 4, cex = 0.62, col = "grey25")
tp <- names(bp)[1]
text(100 * bp[tp], 100 * ar[tp], tt(tp), pos = 2, cex = 0.62, col = "grey25")

## ---- scatter-d3
cn <- names(ar)
rows <- paste(sprintf('{"c":"%s","x":%.2f,"y":%.3f}',
                      tt(cn), 100 * bp[cn], 100 * ar[cn]), collapse = ",")
cat(sprintf('
<div id="sc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:18,r:24,b:48,l:58};
const box=d3.select("#sc");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([20,80]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,9]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("where the party is liked: Republican %% of votes cast in the county");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("where the votes are: %% of the statewide party vote");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",5)
  .attr("fill",d=>(d.y>2||d.x>70)?"#C41230":"#999").attr("fill-opacity",0.75)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.c}</b><br>${d.y.toFixed(2)}%% of the party&#39;s statewide vote<br>`+
      `${d.x.toFixed(1)}%% Republican locally`)
      .style("left",Math.min(ev.offsetX+14,W-250)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.l").data(D.filter(d=>d.y>2)).join("text")
  .attr("class","l").attr("x",d=>x(d.x)+8).attr("y",d=>y(d.y)+4)
  .attr("font-size","10px").attr("fill","#555").text(d=>d.c);
svg.append("g").selectAll("text.t").data(D.filter(d=>d.x>=%.2f)).join("text")
  .attr("class","t").attr("x",d=>x(d.x)-8).attr("y",d=>y(d.y)+4)
  .attr("text-anchor","end").attr("font-size","10px").attr("fill","#555").text(d=>d.c);
})();
</script>
', rows, 100 * bp[1]))

## ---- drop24
cm <- data.frame(county = names(ar), with = 100 * ar,
                 without = 100 * ar2[names(ar)])
cm$chg <- cm$without - cm$with
o <- rbind(head(cm[order(-cm$chg), ], 4), head(cm[order(cm$chg), ], 4))
o <- data.frame(county = tt(o$county), with_2024 = pc(o$with, 2),
                without_2024 = pc(o$without, 2), change = pc(o$chg, 2))
names(o) <- c("county", "share with 2024 (%)", "share without 2024 (%)",
              "change")
o

## ---- shift-static
par(mar = c(8.0, 4.4, 0.8, 1))
plot(cshift$size, cshift$rel, log = "x", pch = 1, cex = 0.8, col = "#999999",
     las = 1, xlab = "votes cast in the county, 2024 (log scale)",
     ylab = "change in the county's share (% of itself)", xaxt = "n",
     xlim = c(1500, 1.4e6))
axis(1, at = c(2000, 5000, 20000, 50000, 200000, 600000),
     labels = c("2k", "5k", "20k", "50k", "200k", "600k"))
abline(h = 0, col = "grey35")
gi <- cshift$rel > 0
points(cshift$size[gi], cshift$rel[gi], pch = 19, cex = 0.95, col = "#C41230")
li <- cshift$size > 150000 | cshift$rel < quantile(cshift$rel, 0.03)
text(cshift$size[li], cshift$rel[li], tt(cshift$county[li]), pos = 4,
     cex = 0.62, col = "grey25")
text(1500, max(cshift$rel) + 0.4, "gains when 2024 is dropped", adj = c(0, 1),
     cex = 0.76, col = "#C41230")
text(1.4e6, min(cshift$rel), "loses when\n2024 is dropped", adj = c(1, 0),
     cex = 0.76, col = "#555555")
subcap(cap_shift)

## ---- shift-d3
rows <- paste(sprintf('{"c":"%s","s":%d,"r":%.2f,"w":%.3f,"o":%.3f}',
                      tt(cshift$county), round(cshift$size), cshift$rel,
                      cshift$with, cshift$without), collapse = ",")
cat(sprintf('
<div id="shf" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:20,r:26,b:48,l:60};
const box=d3.select("#shf");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([1500,1400000]).range([M.l,W-M.r]);
const ex=d3.extent(D,d=>d.r);
const y=d3.scaleLinear().domain([ex[0]-1,ex[1]+1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickValues([2000,5000,20000,50000,200000,600000])
    .tickFormat(d=>d>=1000?(d/1000)+"k":d));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("votes cast in the county, 2024 (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("change in the county&#39;s share (%% of itself)");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#666");
svg.append("text").attr("x",M.l+4).attr("y",M.t+12).attr("font-size","11.5px")
  .attr("fill","#C41230").text("gains when 2024 is dropped");
svg.append("text").attr("x",W-M.r-4).attr("y",H-M.b-8).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#555").text("loses when 2024 is dropped");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const f=d3.format(",");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.s)).attr("cy",d=>y(d.r)).attr("r",d=>d.r>0?5:4)
  .attr("fill",d=>d.r>0?"#C41230":"none").attr("stroke",d=>d.r>0?"#C41230":"#999")
  .attr("stroke-width",1.3)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.c}</b> (${f(d.s)} votes cast)<br>`+
      `share with 2024: ${d.w.toFixed(2)}%%<br>without: ${d.o.toFixed(2)}%%<br>`+
      `${d.r>0?"+":""}${d.r.toFixed(1)}%% of itself`)
      .style("left",Math.min(ev.offsetX+14,W-250)+"px").style("top",(ev.offsetY-46)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.l").data(D.filter(d=>d.s>150000)).join("text")
  .attr("class","l").attr("x",d=>x(d.s)+8).attr("y",d=>y(d.r)+4)
  .attr("font-size","10.5px").attr("fill","#555").text(d=>d.c);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover any dot for its two shares.</p>
', rows, cap_shift))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#conc text[fill="#2c7fb8" i],
#conc text[fill="#c41230" i],
#gap text[fill="#333" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
