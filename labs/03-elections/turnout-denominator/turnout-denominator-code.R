# turnout-denominator-code.R -- chunk bodies for turnout-denominator-brief.Rmd
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

mp <- read.csv("data/derived/mp1948.csv",     stringsAsFactors = FALSE)
na <- read.csv("data/derived/national.csv",   stringsAsFactors = FALSE)
st <- read.csv("data/derived/states.csv",     stringsAsFactors = FALSE)
tr <- read.csv("data/derived/state_trends.csv", stringsAsFactors = FALSE)
pr <- read.csv("data/derived/pairs2024.csv",  stringsAsFactors = FALSE)
ov <- read.csv("data/derived/overlap.csv",    stringsAsFactors = FALSE)
ck <- read.csv("data/derived/checks.csv",     stringsAsFactors = FALSE)

cv <- function(k) ck$value[ck$check == k]
n  <- function(x) format(round(x), big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)
sg <- function(x, k = 2) sprintf("%+.*f", k, x)

# ---- the paper's own regression, on the paper's own numbers -----------------
# Reported per ELECTION, not per year: that is the unit the article quotes,
# and the only unit in which the two series are comparable to the published
# figure. Presidential years only -- pooling presidential and midterm turnout
# makes the slope a function of how many of each happen to fall in the window.
PRES <- mp[mp$presidential, ]
W72  <- PRES[PRES$year >= 1972, ]
fit  <- function(y, x) summary(lm(y ~ x))$coefficients[2, ] * 4
B_VAP <- fit(W72$vap_rate, W72$year)
B_VEP <- fit(W72$vep_rate, W72$year)
RATIO <- abs(B_VAP[1] / B_VEP[1])

# ---- the size of the correction, before and after 1972 ---------------------
GAP_EARLY <- mean(mp$gap[mp$year <= 1970])
GAP_LATE  <- mean(mp$gap[mp$year >= 1972])

# ---- 2024, one row, taken apart --------------------------------------------
N24  <- na[na$YEAR == 2024, ]
NC24 <- N24$VAP * N24$NONCITIZEN_PCT / 100
GAP24 <- N24$rate_vep_tb - N24$rate_vap_tb
EXTRA <- N24$VAP * N24$rate_vep_tb / 100 - N24$TOTAL_BALLOTS_COUNTED

# ---- the five states, and the one the section is written around ------------
FLIP  <- tr[tr$sign_flip, ]
FLIP  <- FLIP[order(FLIP$b_vap), ]
TX    <- tr[tr$state == "Texas", ]
# The smallest p-value among the TEN trends in the sign-flip table -- not
# across all 51 states, where plenty of trends are well determined. The five
# states that flip are exactly the states where neither slope is.
P_MIN  <- min(c(FLIP$p_vap, FLIP$p_vep))
P_SIG  <- sum(c(tr$p_vap, tr$p_vep) < 0.05)      # over all 51 states
P_SIGF <- sum(c(FLIP$p_vap, FLIP$p_vep) < 0.05)  # over the five that flip

# ---- the pair the 2024 section is written around ---------------------------
TOP <- pr[1, ]

# ---- the two panels the figures draw ---------------------------------------
# Defined once, here, rather than inside the figure chunks: only one of each
# HTML/PDF pair ever evaluates, and prose after the figure refers to both.
TXD <- st[st$STATE == "Texas" & st$presidential == TRUE &
            st$YEAR >= 1980 & st$YEAR <= 2016, ]
TXD <- TXD[order(TXD$YEAR), ]
S <- st[st$YEAR == 2024, ]
S$hl <- S$STATE %in% c(TOP$state_a, TOP$state_b)
S$lift <- S$rate_vep_tb - S$rate_vap_tb

# ---- the disenfranchisement chapter's number, borrowed ---------------------
# Read rather than re-derived: that chapter's build script parses the five
# categories out of a PDF and has already decided what they mean.
DIS <- read.csv("../disenfranchisement/data/derived/national.csv", stringsAsFactors = FALSE)

# ---- the ineligible share of the voting-age population ---------------------
# The one quantity here with no numerator in it, which is what makes it safe
# to draw across two sources that count votes differently. Both series are
# kept whole and drawn separately; they are never averaged or spliced.
SH_MP <- mp[, c("year", "inelig_pct")]
SH_EP <- data.frame(year = na$YEAR, inelig_pct = na$inelig_pct)

# ---- render every data.frame in this document as a TABLE, not code output --
# These are front-facing documents. A data.frame printed the ordinary way
# comes out as a "##"-prefixed code block, which reads as machinery rather
# than as a result. Registering knit_print for data.frame turns all of them
# into real tables in HTML and PDF alike. The envir argument is required:
# without it the registration silently fails.
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- raw-capture
RAW <- readLines("data/raw/mp2001-table1.txt", warn = FALSE)
hdr <- RAW[1:4]
pick <- function(y) RAW[grep(paste0("^\\s*", y, "\\s"), RAW)][1]
blk <- c(hdr, unlist(lapply(c(1948, 1960, 1972, 1984, 2000), pick)))
blk <- substr(blk, 1, 78)
cat("```\n", paste(blk, collapse = "\n"), "\n```\n", sep = "")

## ---- vep-def
data.frame(
  term = c("Voting-age population", "less noncitizens", "less ineligible felons",
           "plus eligible overseas", "Voting-eligible population"),
  who_that_is = c("everyone aged 18 and over living in the country",
                  "in the country, cannot vote",
                  "citizens, barred by their state over a conviction",
                  "citizens who may vote and do not live here",
                  "the people who could have cast a ballot"),
  check.names = FALSE)

## ---- recon-check
data.frame(
  check = c("The test",
            "Election years where the rebuilt rate matches the printed one",
            "Largest disagreement anywhere, percentage points",
            "Second test",
            "Years where the printed adjustments sum to the printed rate"),
  result = c("(vote for highest office) / (VAP - noncitizens - felons + overseas)",
             paste(cv("rows where the rebuilt VEP rate matches the printed one within 0.1 pts"),
                   "of", cv("Table 1 data rows parsed")),
             cv("largest disagreement between rebuilt and printed VEP rate, pts"),
             "VAP rate + each printed adjustment = VEP rate",
             paste(cv("rows where the printed adjustments sum to the printed VEP rate within 0.2 pts"),
                   "of", cv("Table 1 data rows parsed"))))

## ---- trend-tab
data.frame(
  denominator = c("Voting-age population (VAP)", "Voting-eligible population (VEP)"),
  `1972` = c(f1(W72$vap_rate[W72$year == 1972]), f1(W72$vep_rate[W72$year == 1972])),
  `2000` = c(f1(W72$vap_rate[W72$year == 2000]), f1(W72$vep_rate[W72$year == 2000])),
  trend_per_election = c(sg(B_VAP[1], 3), sg(B_VEP[1], 3)),
  standard_error = c(f2(B_VAP[2]), f2(B_VEP[2])),
  check.names = FALSE)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The two series, presidential years, 1948-2000, with 1972 marked. Both this
# figure and the base-R equivalent below read the same `PRES` rows and the
# same formatted strings, so the interactive and static versions cannot drift.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
J <- paste(sprintf('{"y":%d,"vap":%s,"vep":%s}', PRES$year,
                   f1(PRES$vap_rate), f1(PRES$vep_rate)), collapse = ",")
cat(sprintf('
<div id="f1" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:52,r:120,b:46,l:52};
const svg=d3.select("#f1").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1946,2002]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([44,66]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",8).attr("y",22).attr("font-size","13px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Presidential turnout, 1948-2000: the same ballots, two denominators");
svg.append("text").attr("x",8).attr("y",39).attr("font-size","11.5px")
  .attr("fill","#666").text("After 1972 the two lines stop agreeing about the direction of travel");
// the 1972 divide
svg.append("line").attr("x1",x(1972)).attr("x2",x(1972)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#bbb").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(1972)+5).attr("y",M.t+12).attr("font-size","11px")
  .attr("fill","#888").text("1972");
const mk=(k,c,dash)=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",c)
    .attr("stroke-width",2).attr("stroke-dasharray",dash)
    .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d[k])));
  svg.selectAll("circle."+k).data(D).join("circle").attr("class",k)
    .attr("cx",d=>x(d.y)).attr("cy",d=>y(d[k])).attr("r",3.4).attr("fill",c);
};
mk("vep","#2c7fb8",null); mk("vap","#C41230","5,3");
const lab=(k,c,t)=>svg.append("text").attr("x",x(2000)+9)
  .attr("y",y(D[D.length-1][k])+4).attr("font-size","12px")
  .attr("font-weight","600").attr("fill",c).text(t);
lab("vep","#2c7fb8","VEP  eligible"); lab("vap","#C41230","VAP  voting-age");
// hover readout
const rd=svg.append("text").attr("x",W-M.r+9).attr("y",M.t+2)
  .attr("font-size","11.5px").attr("fill","#333");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const yr=x.invert(d3.pointer(e,this)[0]);
    const d=D.reduce((a,b)=>Math.abs(b.y-yr)<Math.abs(a.y-yr)?b:a);
    rd.text(d.y+":  "+d.vap+"  vs  "+d.vep);
  }).on("mouseleave",()=>rd.text(""));
})();
</script>
', J))

## ---- fig1-static
par(mar = c(3.6, 4.0, 3.0, 6.6))
plot(PRES$year, PRES$vep_rate, type = "n", ylim = c(44, 66),
     xlim = c(1946, 2002), xlab = "", ylab = "", axes = FALSE)
abline(v = 1972, lty = 2, col = "#bbbbbb")
axis(1, at = seq(1948, 2000, 8), cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
lines(PRES$year, PRES$vep_rate, col = "#2c7fb8", lwd = 2)
lines(PRES$year, PRES$vap_rate, col = "#C41230", lwd = 2, lty = 2)
points(PRES$year, PRES$vep_rate, pch = 19, col = "#2c7fb8", cex = 0.7)
points(PRES$year, PRES$vap_rate, pch = 19, col = "#C41230", cex = 0.7)
text(2001, tail(PRES$vep_rate, 1), "VEP", col = "#2c7fb8", adj = 0,
     font = 2, cex = 0.8, xpd = NA)
text(2001, tail(PRES$vap_rate, 1), "VAP", col = "#C41230", adj = 0,
     font = 2, cex = 0.8, xpd = NA)
text(1973, 65, "1972", col = "#888888", adj = 0, cex = 0.75)
mtext("Presidential turnout, 1948-2000: the same ballots, two denominators",
      3, line = 1.4, adj = 0, cex = 0.9, font = 2)
mtext("After 1972 the two lines stop agreeing about the direction of travel",
      3, line = 0.3, adj = 0, cex = 0.75, col = "#666666")

## ---- gap-tab
data.frame(
  period = c("1948-1970", "1972-2000", "2024"),
  mean_gap_between_the_two_rates = c(paste0(f2(GAP_EARLY), " pts"),
                                     paste0(f2(GAP_LATE), " pts"),
                                     paste0(f2(GAP24), " pts")),
  check.names = FALSE)

## ---- fig2-d3
JA <- paste(sprintf('{"y":%d,"v":%s}', SH_MP$year, f2(SH_MP$inelig_pct)), collapse = ",")
JB <- paste(sprintf('{"y":%d,"v":%s}', SH_EP$year, f2(SH_EP$inelig_pct)), collapse = ",")
cat(sprintf('
<div id="f2" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const A=[%s],B=[%s];
const W=760,H=340,M={t:52,r:118,b:44,l:50};
const svg=d3.select("#f2").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1946,2026]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,10]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(5).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",8).attr("y",22).attr("font-size","13px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Share of the voting-age population that cannot vote, 1948-2024");
svg.append("text").attr("x",8).attr("y",39).attr("font-size","11.5px")
  .attr("fill","#666").text("No ballots in this figure at all - it is only a statement about who lives here");
const ln=d3.line().x(d=>x(d.y)).y(d=>y(d.v));
svg.append("path").datum(A).attr("fill","none").attr("stroke","#8c8c8c")
  .attr("stroke-width",2).attr("d",ln);
svg.append("path").datum(B).attr("fill","none").attr("stroke","#54278F")
  .attr("stroke-width",2).attr("d",ln);
svg.selectAll("circle.a").data(A).join("circle").attr("class","a")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.v)).attr("r",2.6).attr("fill","#8c8c8c");
svg.selectAll("circle.b").data(B).join("circle").attr("class","b")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.v)).attr("r",2.6).attr("fill","#54278F");
svg.append("text").attr("x",x(A[A.length-1].y)-6).attr("y",y(A[A.length-1].v)-11)
  .attr("text-anchor","end")
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#8c8c8c")
  .text("the 2001 article");
svg.append("text").attr("x",W-M.r+8).attr("y",y(B[B.length-1].v)+4)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#54278F")
  .text("the current file");
svg.append("line").attr("x1",x(1972)).attr("x2",x(1972)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#bbb").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(1972)+5).attr("y",M.t+12).attr("font-size","11px")
  .attr("fill","#888").text("1972");
})();
</script>
', JA, JB))

## ---- fig2-static
par(mar = c(3.6, 4.0, 3.0, 7.4))
plot(SH_MP$year, SH_MP$inelig_pct, type = "n", ylim = c(0, 10),
     xlim = c(1946, 2026), xlab = "", ylab = "", axes = FALSE)
abline(v = 1972, lty = 2, col = "#bbbbbb")
axis(1, at = seq(1950, 2020, 10), cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
lines(SH_MP$year, SH_MP$inelig_pct, col = "#8c8c8c", lwd = 2)
lines(SH_EP$year, SH_EP$inelig_pct, col = "#54278F", lwd = 2)
points(SH_MP$year, SH_MP$inelig_pct, pch = 19, col = "#8c8c8c", cex = 0.5)
points(SH_EP$year, SH_EP$inelig_pct, pch = 19, col = "#54278F", cex = 0.5)
text(tail(SH_MP$year, 1) - 1, tail(SH_MP$inelig_pct, 1) + 1.1,
     "the 2001 article", col = "#8c8c8c", adj = 1, font = 2, cex = 0.7)
text(2027, tail(SH_EP$inelig_pct, 1), "the\ncurrent\nfile", col = "#54278F",
     adj = 0, font = 2, cex = 0.7, xpd = NA)
mtext("Share of the voting-age population that cannot vote, 1948-2024",
      3, line = 1.4, adj = 0, cex = 0.9, font = 2)
mtext("No ballots in this figure at all", 3, line = 0.3, adj = 0,
      cex = 0.75, col = "#666666")

## ---- decomp
data.frame(
  step = c("Voting-age population, November 2024",
           "less noncitizens",
           "less people in prison, on probation or on parole where that bars voting",
           "plus citizens living overseas who may vote",
           "Voting-eligible population"),
  people = c(n(N24$VAP), paste0("-", n(NC24)),
             paste0("-", n(N24$INELIGIBLE_FELONS_TOTAL)),
             paste0("+", n(N24$ELIGIBLE_OVERSEAS)), n(N24$VEP)))

## ---- ep-check
data.frame(
  check = c("Rows in the file",
            "Rows where VEP = VAP x (1 - noncitizen%) - felons + overseas",
            "Largest disagreement on that identity",
            "Rows carrying a published turnout rate",
            "Of those, rows this chapter reproduces within 0.05 points",
            "Largest disagreement with a published rate"),
  result = c(cv("Elections Project rows, 1980-2024"),
             paste(cv("rows where VEP equals VAP*(1-noncitizen%) - felons + overseas within 0.1%"),
                   "of", cv("Elections Project rows, 1980-2024")),
             paste0(cv("largest disagreement on that identity, %"), "%"),
             cv("rows with a published turnout rate to check against"),
             paste(cv("of those, rows reproduced within 0.05 pts"), "of",
                   cv("rows with a published turnout rate to check against")),
             paste0(cv("largest disagreement with a published rate, pts"), " pts")))

## ---- numerator-cov
cov <- function(v) {
  yy <- na$YEAR[!is.na(na[[v]])]
  paste0(min(yy), "-", max(yy), "  (", length(yy), " elections)")
}
data.frame(
  numerator = c("VOTE_FOR_HIGHEST_OFFICE", "TOTAL_BALLOTS_COUNTED"),
  what_it_counts = c("votes for the top statewide office",
                     "every ballot, including undervotes"),
  years_available_nationally = c(cov("VOTE_FOR_HIGHEST_OFFICE"),
                                 cov("TOTAL_BALLOTS_COUNTED")),
  check.names = FALSE)

## ---- overlap-tab
data.frame(
  quantity = c("Numerator: votes for the highest office",
               "Denominator: voting-age population",
               "Result: the published VAP turnout rate"),
  mean_change_since_2001 = c(
    paste0(cv("mean revision to the numerator since 2001, %"), "%"),
    paste0(cv("mean revision to the denominator since 2001, %"), "%"),
    paste0(f2(mean(abs(ov$vap_rate_move))), " pts")),
  largest_single_change = c(
    paste0(f3(max(abs(ov$num_move_pct))), "%"),
    paste0(f2(max(abs(ov$vap_move_pct))), "%"),
    paste0(cv("largest revision to a published VAP turnout rate, pts"), " pts")),
  check.names = FALSE)

## ---- flip-tab
data.frame(
  state = FLIP$state,
  noncitizen_share = paste0(f1(FLIP$nc_first), "% to ", f1(FLIP$nc_last), "%"),
  fitted_change_on_VAP = paste0(sg(FLIP$fit_vap, 1), " pts"),
  fitted_change_on_VEP = paste0(sg(FLIP$fit_vep, 1), " pts"),
  check.names = FALSE)

## ---- fig3-d3
JT <- paste(sprintf('{"y":%d,"vap":%s,"vep":%s}', TXD$YEAR,
                    f1(TXD$rate_vap_hi), f1(TXD$rate_vep_hi)), collapse = ",")
cat(sprintf('
<div id="f3" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=340,M={t:52,r:126,b:44,l:50};
const svg=d3.select("#f3").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1978,2018]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([38,58]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(6));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(5).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",8).attr("y",22).attr("font-size","13px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Texas presidential turnout, 1980-2016");
svg.append("text").attr("x",8).attr("y",39).attr("font-size","11.5px")
  .attr("fill","#666").text("Falling and rising, at the same time, from the same ballots");
const fitline=(k,c)=>{
  const n=D.length,sx=d3.sum(D,d=>d.y),sy=d3.sum(D,d=>d[k]);
  const sxy=d3.sum(D,d=>d.y*d[k]),sxx=d3.sum(D,d=>d.y*d.y);
  const b=(n*sxy-sx*sy)/(n*sxx-sx*sx),a=(sy-b*sx)/n;
  svg.append("line").attr("x1",x(1980)).attr("x2",x(2016))
    .attr("y1",y(a+b*1980)).attr("y2",y(a+b*2016))
    .attr("stroke",c).attr("stroke-width",2.4).attr("opacity",0.9);
};
const mk=(k,c)=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",c)
    .attr("stroke-width",1).attr("opacity",0.35)
    .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d[k])));
  svg.selectAll("circle."+k).data(D).join("circle").attr("class",k)
    .attr("cx",d=>x(d.y)).attr("cy",d=>y(d[k])).attr("r",3.2)
    .attr("fill",c).attr("opacity",0.55);
  fitline(k,c);
};
mk("vep","#2c7fb8"); mk("vap","#C41230");
svg.append("text").attr("x",x(2016)+9).attr("y",y(D[D.length-1].vep)+4)
  .attr("font-size","12px").attr("font-weight","600").attr("fill","#2c7fb8")
  .text("VEP  rising");
svg.append("text").attr("x",x(2016)+9).attr("y",y(D[D.length-1].vap)+4)
  .attr("font-size","12px").attr("font-weight","600").attr("fill","#C41230")
  .text("VAP  falling");
})();
</script>
', JT))

## ---- fig3-static
par(mar = c(3.6, 4.0, 3.0, 7.0))
plot(TXD$YEAR, TXD$rate_vep_hi, type = "n", ylim = c(38, 58),
     xlim = c(1978, 2018), xlab = "", ylab = "", axes = FALSE)
axis(1, at = seq(1980, 2016, 8), cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
for (k in list(c("rate_vep_hi", "#2c7fb8"), c("rate_vap_hi", "#C41230"))) {
  lines(TXD$YEAR, TXD[[k[1]]], col = adjustcolor(k[2], 0.35), lwd = 1)
  points(TXD$YEAR, TXD[[k[1]]], pch = 19, col = adjustcolor(k[2], 0.55), cex = 0.7)
  abline(lm(TXD[[k[1]]] ~ TXD$YEAR), col = k[2], lwd = 2.4)
}
text(2018.5, tail(TXD$rate_vep_hi, 1), "VEP\nrising", col = "#2c7fb8",
     adj = 0, font = 2, cex = 0.72, xpd = NA)
text(2018.5, tail(TXD$rate_vap_hi, 1), "VAP\nfalling", col = "#C41230",
     adj = 0, font = 2, cex = 0.72, xpd = NA)
mtext("Texas presidential turnout, 1980-2016", 3, line = 1.4, adj = 0,
      cex = 0.9, font = 2)
mtext("Falling and rising, at the same time, from the same ballots",
      3, line = 0.3, adj = 0, cex = 0.75, col = "#666666")

## ---- pair-tab
data.frame(
  denominator = c("Voting-age population", "Voting-eligible population"),
  a = c(paste0(f1(TOP$vap_a), "%"), paste0(f1(TOP$vep_a), "%")),
  b = c(paste0(f1(TOP$vap_b), "%"), paste0(f1(TOP$vep_b), "%")),
  `turned out more` = c(TOP$state_b, TOP$state_a),
  check.names = FALSE) |>
  setNames(c("Denominator", TOP$state_a, TOP$state_b, "Turned out more"))

## ---- fig4-d3
JS <- paste(sprintf('{"s":"%s","x":%s,"y":%s,"h":%s}', S$STATE,
                    f2(S$rate_vap_tb), f2(S$rate_vep_tb),
                    tolower(as.character(S$hl))), collapse = ",")
cat(sprintf('
<div id="f4" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:52,r:26,b:52,l:56};
const svg=d3.select("#f4").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([42,74]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([42,74]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",8).attr("y",22).attr("font-size","13px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Every state in 2024, measured both ways");
svg.append("text").attr("x",8).attr("y",39).attr("font-size","11.5px")
  .attr("fill","#666")
  .text("Distance above the diagonal is how much the denominator is doing");
svg.append("line").attr("x1",x(42)).attr("y1",y(42)).attr("x2",x(74))
  .attr("y2",y(74)).attr("stroke","#ccc").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#555")
  .text("turnout on the voting-age population");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#555").text("turnout on the voting-eligible population");
const tip=svg.append("text").attr("x",M.l+8).attr("y",H-M.b-10)
  .attr("font-size","12px").attr("font-weight","600").attr("fill","#333");
svg.selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",d=>d.h?6:4)
  .attr("fill",d=>d.h?"#C41230":"#54278F").attr("fill-opacity",d=>d.h?0.95:0.42)
  .style("cursor","pointer")
  .on("mouseover",(e,d)=>tip.text(d.s+":  "+d.x+"%% of voting-age,  "+d.y+"%% of eligible"))
  .on("mouseout",()=>tip.text(""));
D.filter(d=>d.h).forEach(d=>{
  svg.append("text").attr("x",x(d.x)+10).attr("y",y(d.y)+4)
    .attr("font-size","12px").attr("font-weight","600").attr("fill","#C41230")
    .text(d.s);
});
})();
</script>
', JS))

## ---- fig4-static
par(mar = c(4.0, 4.2, 3.0, 1.4))
plot(S$rate_vap_tb, S$rate_vep_tb, type = "n", xlim = c(42, 74),
     ylim = c(42, 74), xlab = "", ylab = "", axes = FALSE)
abline(0, 1, lty = 2, col = "#cccccc")
axis(1, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
points(S$rate_vap_tb[!S$hl], S$rate_vep_tb[!S$hl], pch = 19,
       col = adjustcolor("#54278F", 0.42), cex = 0.9)
points(S$rate_vap_tb[S$hl], S$rate_vep_tb[S$hl], pch = 19,
       col = "#C41230", cex = 1.4)
text(S$rate_vap_tb[S$hl] + 0.7, S$rate_vep_tb[S$hl], S$STATE[S$hl],
     adj = 0, cex = 0.72, font = 2, col = "#C41230")
mtext("turnout on the voting-age population", 1, line = 2.4, cex = 0.8)
mtext("turnout on the voting-eligible population", 2, line = 2.8, cex = 0.8)
mtext("Every state in 2024, measured both ways", 3, line = 1.4, adj = 0,
      cex = 0.9, font = 2)
mtext("Distance above the diagonal is how much the denominator is doing",
      3, line = 0.3, adj = 0, cex = 0.75, col = "#666666")

## ---- still-in
data.frame(
  population = c("Voting-eligible population, 2024",
                 "of which: post-sentence and still barred from voting",
                 "share of the VEP that cannot actually vote"),
  figure = c(n(N24$VEP), n(DIS$post_sentence),
             paste0(f2(100 * DIS$post_sentence / N24$VEP), "%")))

## ---- checks
ck

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
