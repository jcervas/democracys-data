# nomination-rules-code.R -- chunk bodies for nomination-rules-brief.Rmd
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

# All of this is read from data/, written by data/build-data.R out of the ten
# FEC workbooks the retirements chapter committed. Nothing here re-derives
# anything.
CT <- read.csv("data/derived/contests.csv",           stringsAsFactors = FALSE)
RO <- read.csv("data/derived/runoffs.csv",            stringsAsFactors = FALSE)
BY <- read.csv("data/derived/by_year.csv",            stringsAsFactors = FALSE)
BR <- read.csv("data/derived/brackets.csv",           stringsAsFactors = FALSE)
NC <- read.csv("data/derived/nonclosing_by_year.csv", stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",             stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("brackets.csv", "by_year.csv", "checks.csv", "contests.csv", "nonclosing_by_year.csv", "runoffs.csv"))

pc <- function(x, k = 0) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)
ck <- function(k) {
  v <- CK$value[CK$check == k]
  if (length(v) != 1L) stop("checks.csv has no single value for '", k, "'")
  v
}

P     <- CT[!CT$top_two_state, ]          # party primaries in ordinary states
NOR   <- P[!P$runoff, ]                   # settled without a runoff
NRUN  <- nrow(RO)
NOVER <- sum(!RO$leader_won_runoff)
Y1 <- min(BY$year); Y2 <- max(BY$year)
OK <- BR[BR$closes, ]; OK <- OK[order(OK$bracket_width), ]
BAD <- BR[!BR$closes, ]
# North Carolina, cycle by cycle, with the width of each year's own bracket.
NCE <- NC[NC$state == "NC", ]
NCE$w <- NCE$lowest_share_no_runoff - NCE$highest_share_still_runoff
# The opening case and the worked example: Ralph Hall's 2014 runoff, and
# Georgia, the state whose outcomes pin its rule most tightly.
HALL <- RO[RO$year == 2014 & RO$st == "TX" & RO$district == 4, ]
GA   <- BR[BR$state == "GA", ]

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

## ---- scope
data.frame(
  quantity = c("Elections covered", "Years",
               "House candidate rows", "Of those, on a major-party line",
               "Contested primaries with usable vote totals",
               "Of those, in all-party states, set aside",
               "Runoffs"),
  value = c(nrow(BY), paste(Y1, "to", Y2),
            ck("House candidate rows, 2004-2022"),
            ck("of those, on a Democratic or Republican line"),
            ck("contested House primaries with usable vote totals"),
            ck("of those, in all-party (top-two) states, shares not comparable"),
            NRUN))

## ---- fig1-d3
# ---------------------------------------------------------------------------
# THE BRACKET, PER STATE. One row per state that held at least one House
# runoff; a bar spanning from the highest leader share that still triggered a
# runoff to the lowest that did not. The rule is somewhere inside the bar.
# Two states are drawn in the accent colour because their bracket runs
# backwards, which is impossible for a fixed rule and is the finding.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figure uses the library loaded
# here.
# ---------------------------------------------------------------------------
B <- BR[order(BR$closes, -BR$bracket_width), ]
rows <- paste(sprintf('{"s":"%s","hi":%.1f,"lo":%.1f,"ok":%d,"n":%d}',
  B$state, B$highest_share_still_runoff, B$lowest_share_no_runoff,
  as.integer(B$closes), B$runoffs), collapse = ",")
cat(paste0('
<div id="brk" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const W=760,H=300,M={t:22,r:150,b:40,l:52};
const svg=d3.select("#brk").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([15,60]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([M.t,H-M.b]).padding(0.34);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("the leading candidate\\u2019s share of the primary vote");
[40,50].forEach(v=>{
  svg.append("line").attr("x1",x(v)).attr("x2",x(v)).attr("y1",M.t).attr("y2",H-M.b)
    .attr("stroke","#bbb").attr("stroke-dasharray","4,3");
  svg.append("text").attr("x",x(v)).attr("y",M.t-6).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#777").text(v+"%");
});
D.forEach(d=>{
  const a=Math.min(d.hi,d.lo), b=Math.max(d.hi,d.lo);
  svg.append("rect").attr("x",x(a)).attr("y",y(d.s)).attr("height",y.bandwidth())
    .attr("width",x(b)-x(a)).attr("fill",d.ok?"#9fb6c9":"#C41230")
    .attr("fill-opacity",d.ok?0.85:0.28);
  [[d.hi,"#123"],[d.lo,"#123"]].forEach(([v,c])=>{
    svg.append("line").attr("x1",x(v)).attr("x2",x(v)).attr("y1",y(d.s)-2)
      .attr("y2",y(d.s)+y.bandwidth()+2).attr("stroke",d.ok?"#4a6b8a":"#C41230")
      .attr("stroke-width",2);
  });
  svg.append("text").attr("x",W-M.r+8).attr("y",y(d.s)+y.bandwidth()/2+4)
    .attr("font-size","11px").attr("fill",d.ok?"#444":"#C41230")
    .text(d.ok?(d.hi.toFixed(1)+"% to "+d.lo.toFixed(1)+"%"):"runs backwards");
});
})();
</script>'))

## ---- fig1-static
B <- BR[order(BR$closes, -BR$bracket_width), ]
par(mar = c(3.2, 3.6, 1.4, 7.0), mgp = c(2.0, 0.6, 0))
plot(NA, xlim = c(15, 60), ylim = c(nrow(B) + 0.5, 0.5), yaxt = "n", las = 1,
     xlab = "the leading candidate's share of the primary vote", ylab = "",
     cex.axis = 0.7, cex.lab = 0.72)
axis(2, at = seq_len(nrow(B)), labels = B$state, las = 1, cex.axis = 0.7)
abline(v = c(40, 50), col = "#bbbbbb", lty = 2)
for (i in seq_len(nrow(B))) {
  a <- min(B$highest_share_still_runoff[i], B$lowest_share_no_runoff[i])
  b <- max(B$highest_share_still_runoff[i], B$lowest_share_no_runoff[i])
  rect(a, i - 0.3, b, i + 0.3, border = NA,
       col = if (B$closes[i]) "#9fb6c9" else "#C4123048")
  segments(c(a, b), i - 0.34, c(a, b), i + 0.34, lwd = 2,
           col = if (B$closes[i]) "#4a6b8a" else "#C41230")
  text(61, i, if (B$closes[i])
         sprintf("%.1f%% to %.1f%%", B$highest_share_still_runoff[i],
                 B$lowest_share_no_runoff[i]) else "runs backwards",
       adj = 0, cex = 0.58, xpd = NA,
       col = if (B$closes[i]) "#444444" else "#C41230")
}

## ---- bad
BB <- BAD[, c("state", "runoffs", "contests", "highest_share_still_runoff",
              "lowest_share_no_runoff")]
names(BB) <- c("state", "runoffs", "contests",
               "highest share that still went to a runoff",
               "lowest share that did not")
BB

## ---- era
E <- NC[NC$state == "NC", c("year", "contests_under_40", "went_to_runoff",
                            "highest_share_still_runoff", "lowest_share_no_runoff")]
E$highest_share_still_runoff[is.na(E$highest_share_still_runoff)] <- NA
names(E) <- c("cycle", "contests under 40%", "of those, went to a runoff",
              "highest share that still went to a runoff",
              "lowest share that did not")
E

## ---- fig2-d3
# ---------------------------------------------------------------------------
# EVERY RUNOFF, ONE TICK EACH, placed at the leader's primary share and split
# by what the runoff then did. A barcode rather than a histogram because the
# question is about individual contests near the threshold, and because a
# histogram of 173 events invites a smooth reading the data cannot support.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"s":%.2f,"o":%d,"y":%d,"n":"%s"}', RO$leader_share,
  as.integer(!RO$leader_won_runoff), RO$year,
  gsub('"', "", paste0(RO$st, "-", RO$district, RO$party, " ", RO$year))),
  collapse = ",")
cat(paste0('
<div id="run" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const W=760,H=250,M={t:24,r:16,b:44,l:120};
const svg=d3.select("#run").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([20,55]).range([M.l,W-M.r]);
const lanes=["leader went on to win","leader was beaten"];
const y=d3.scaleBand().domain(lanes).range([M.t,H-M.b]).padding(0.35);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y))
  .selectAll("text").attr("font-size","11px");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("the leader\\u2019s share of the primary vote");
const tip=d3.select("#run").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("line.t").data(D).join("line")
  .attr("x1",d=>x(d.s)).attr("x2",d=>x(d.s))
  .attr("y1",d=>y(lanes[d.o])).attr("y2",d=>y(lanes[d.o])+y.bandwidth())
  .attr("stroke",d=>d.o?"#C41230":"#4a6b8a").attr("stroke-width",1.6)
  .attr("stroke-opacity",0.75).style("cursor","pointer")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.n+"</b><br>leader took "+d.s.toFixed(1)+
      "% of the primary vote<br>"+(d.o?"and lost the runoff":"and won the runoff"))
      .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
lanes.forEach((l,i)=>{
  const k=D.filter(d=>d.o===i).length;
  svg.append("text").attr("x",W-M.r).attr("y",y(l)-4).attr("text-anchor","end")
    .attr("font-size","11px").attr("font-weight","600")
    .attr("fill",i?"#C41230":"#4a6b8a").text(k+" runoffs");
});
})();
</script>'))

## ---- fig2-static
par(mar = c(3.4, 8.6, 1.6, 0.8), mgp = c(2.1, 0.6, 0))
plot(NA, xlim = c(20, 55), ylim = c(0.4, 2.6), yaxt = "n", las = 1,
     xlab = "the leader's share of the primary vote", ylab = "",
     cex.axis = 0.7, cex.lab = 0.72)
axis(2, at = 1:2, labels = c("leader was beaten", "leader went on to win"),
     las = 1, cex.axis = 0.62)
for (k in 0:1) {
  z <- RO[as.integer(!RO$leader_won_runoff) == k, ]
  yy <- if (k == 1) 1 else 2
  segments(z$leader_share, yy - 0.3, z$leader_share, yy + 0.3,
           col = if (k == 1) "#C41230" else "#4a6b8a", lwd = 1.3)
  text(55, yy + 0.44, sprintf("%d runoffs", nrow(z)), adj = 1, cex = 0.58,
       font = 2, col = if (k == 1) "#C41230" else "#4a6b8a", xpd = NA)
}

## ---- plurality
data.frame(
  `the winner took` = c("under half the vote", "under 40 percent",
                        "under 35 percent"),
  primaries = c(ck("primaries settled outright whose winner took under 50%"),
                ck("primaries settled outright whose winner took under 40%"),
                ck("primaries settled outright whose winner took under 35%")),
  check.names = FALSE)
