# poll-simulation-code.R -- chunk bodies for poll-simulation-brief.Rmd
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
set.seed(2026)

st <- read.csv("data/derived/pres2024_states.csv", stringsAsFactors = FALSE)
st$abs_margin <- abs(st$margin)

pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
sg  <- function(x, k = 2) formatC(x, format = "f", digits = k, flag = "+")
cnt <- function(x) format(round(x), big.mark = ",")
moe <- function(n) 1.96 * sqrt(0.25 / n) * 100

# ---- one palette for this document, and deliberately not red/blue ----------
# This chapter is about Harris and Trump, so a red line and a blue line
# would be read as party no matter what the legend says. None of the
# quantities plotted here is a party. Each color below means exactly one
# thing, in every figure, in both output formats.
CT  <- "#111111"   # the truth: the fact the simulation is built on
CS  <- "#999999"   # sampling error alone: honest polls, the advertised margin
CSF <- "#dcdcdc"   # the same idea, as a fill
CG  <- "#e08214"   # the response gap, and every distortion it produces
CGF <- "#f6d3a8"   # the same idea, as a fill
CU  <- "#8073ac"   # the mirror of CG: under-represented in the poll
CGD <- "#8a4a05"   # dark CG and dark CU, for text on a pale fill
CUD <- "#4a3b73"
CM  <- "#00666e"   # how close a state's margin was: a sequential ramp, and the
                   # one place in this document where a color means a size

# ---- the simulated country -------------------------------------------------
truth <- 0.4925                 # stipulated Harris share of the two-party vote
true_margin <- 100 * (1 - 2 * truth)   # the margin the simulation is built on
N     <- 1000000
population <- c(rep("Harris", round(N * truth)), rep("Trump", N - round(N * truth)))
GAP     <- 0.20                 # Harris voters 20% likelier to answer
answers <- ifelse(population == "Harris", 1 + GAP, 1)

# asymptotic value of a poll drawn with that response gap: algebra, not a draw
asym <- function(g) 100 * (truth * (1 + g)) / (truth * (1 + g) + (1 - truth))

honest_1000 <- replicate(1000, 100 * mean(sample(population, 1000) == "Harris"))
biased_1000 <- replicate(1000, 100 * mean(
  sample(population, 1000, prob = answers, replace = TRUE) == "Harris"))

# Distribution labels and caption: formatted ONCE here, in R, and handed to both
# base-R figure and the d3 figure, so print and screen cannot disagree.
avg_biased <- mean(biased_1000)
lab_truth  <- paste0("the truth ", pc(100 * truth, 2), "%")
lab_avgb   <- paste0("average biased poll ", pc(avg_biased, 2), "%")
cap_step5  <- sprintf(paste(
  "Gray: 1,000 honest polls of 1,000 people. Orange: the same 1,000 polls",
  "when Harris voters are %s%% likelier to answer.",
  "Both are tight; only one is right."), pc(100 * GAP, 0))

# The bias, and the sample size past which the advertised interval can
# no longer reach the truth. Computed once, used by both figure formats.
bias   <- asym(GAP) - 100 * truth
ncross <- 0.25 * (1.96 / (bias / 100))^2
cap_fan <- sprintf(paste(
  "Gray band: where 95%% of honest polls land. Orange band: where 95%% of polls",
  "land once Harris voters are %s%% likelier to answer, and the orange dots are",
  "the three polls in the table above. Past about %s respondents the orange band",
  "is narrower than the %s-point bias, so from there on the poll's own interval",
  "never contains the truth again, and it gets worse, not better, with every",
  "extra respondent."), pc(100 * GAP, 0), cnt(ncross), pc(bias, 2))

# Response gaps small enough to be undetectable, and the bias each one
# buys. Caption written once, in ASCII, for both formats.
gs  <- c(0.02, 0.05, 0.10, 0.20, 0.40)
bi  <- sapply(gs, asym) - 100 * truth
mm1 <- moe(1000)
cap_gap <- sprintf(paste(
  "Hollow gray dot: the ±%s a poll of 1,000 advertises, the same on every row,",
  "because sample size is all it responds to. Filled orange dot: the bias the",
  "response gap actually produces. %d of the %d gaps put the bias past the",
  "advertised margin, and %d of %d put it past the %s-point margin of the",
  "election itself."),
  pc(mm1, 2), sum(bi > mm1), length(bi), sum(bi > true_margin), length(bi),
  pc(true_margin, 2))

ns   <- c(1000, 10000, 100000)
draw <- sapply(ns, function(k)
  100 * mean(sample(population, k, prob = answers, replace = TRUE) == "Harris"))

close_n  <- sum(st$abs_margin < moe(1000))
close_ev <- sum(st$ev[st$abs_margin < moe(1000)])

# ---- weighting demonstration ----------------------------------------------
N2         <- 200000
harris     <- rbinom(N2, 1, truth)
trust      <- rbinom(N2, 1, ifelse(harris == 1, 0.75, 0.45))
true_share <- 100 * mean(harris)
responds   <- ifelse(trust == 1, 1.00, 0.35)
i          <- sample(N2, 5000, prob = responds, replace = TRUE)
s          <- data.frame(harris = harris[i], trust = trust[i])
raw_est    <- 100 * mean(s$harris)
tgt        <- mean(trust)
w_right    <- ifelse(s$trust == 1, tgt / mean(s$trust),
                                   (1 - tgt) / (1 - mean(s$trust)))
est_right  <- 100 * weighted.mean(s$harris, w_right)
irrelevant <- rbinom(nrow(s), 1, 0.5)
w_wrong    <- ifelse(irrelevant == 1, 0.5 / mean(irrelevant),
                                      0.5 / (1 - mean(irrelevant)))
est_wrong  <- 100 * weighted.mean(s$harris, w_wrong)

# Weighting-grid caption, ASCII, shared by both formats.
cap_wmx <- sprintf(paste(
  "Orange cells are over-represented in the poll, purple under-represented.",
  "Nothing in this grid is about the vote directly: the poll over-samples",
  "people who trust institutions, and it is only because trust and vote are",
  "correlated that Harris comes out at %s%% of the poll against %s%% of the",
  "country. Weight the two rows back to their true sizes and the error falls",
  "to %s points."),
  pc(raw_est, 1), pc(true_share, 1), sg(est_right - true_share))

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- d3-load
# the one and only copy of d3 in this document
cat('<script src="../../_lib/d3.v7.min.js"></script>\n')

## ---- rawrda
# Two captures, taken when the file was fetched: R's own description of the
# loaded object, and the first four 2024 rows of it. Both are quoted verbatim.
# The row and column counts used in the prose are parsed back out of the first
# line at knit time rather than asserted beside it.
RAWSTR <- c(
"'data.frame':\t1913 obs. of  7 variables:",
" $ year        : num  1864 1864 ...",
" $ state_abbrev: chr  \"CA\" \"CT\" ...",
" $ winner      : chr  \"Abraham Lincoln\" \"Abraham Lincoln\" ...",
" $ party_win   : chr  \"republican\" \"republican\" ...",
" $ democrat    : num  41.4 48.6 ...",
" $ other       : num  NA NA NA NA NA ...",
" $ republican  : num  58.6 51.4 ...")
RAWHEAD <- c(
"     year state_abbrev       winner  party_win democrat other republican",
"1863 2024           AK Donald Trump republican    41.41  1.68      54.54",
"1864 2024           AL Donald Trump republican    34.10    NA      64.57",
"1865 2024           AR Donald Trump republican    33.56  1.12      64.20",
"1866 2024           AZ Donald Trump republican    46.70  0.50      52.20")
RN <- as.integer(sub(".*:\\D*([0-9]+) obs.*", "\\1", RAWSTR[1]))
RC <- as.integer(sub(".*of *([0-9]+) variables.*", "\\1", RAWSTR[1]))

# R's own description of the object, parsed into the three things it actually
# says about each column: its name, its storage type, and the first values.
.v <- RAWSTR[-1]
.m <- regmatches(.v, regexec("^ \\$ ([A-Za-z_]+)\\s*: (\\w+)\\s+(.*)$", .v))
.ps <- c(
  year = "the presidential election year",
  state_abbrev = "the state's two-letter code",
  winner = "who carried the state",
  party_win = "that winner's party",
  democrat = "the Democratic share of the vote, as a percentage",
  other = "the share going to everyone else",
  republican = "the Republican share")
.nm <- vapply(.m, function(z) z[2], character(1))
data.frame(
  Column       = .nm,
  What_it_holds = unname(.ps[.nm]),
  Stored_as    = c(num = "number", chr = "text")[
                    vapply(.m, function(z) z[3], character(1))],
  First_values = trimws(vapply(.m, function(z) z[4], character(1))))

## ---- rawrda2
# The four rows as a table. The row numbers on the left are R's, not the
# file's, and they are kept because the paragraph below refers to them.
.h <- strsplit(trimws(RAWHEAD[1]), " +")[[1]]
.b <- lapply(RAWHEAD[-1], function(x) strsplit(trimws(x), " +")[[1]])
.b <- lapply(.b, function(z) c(z[1], z[2], z[3], paste(z[4], z[5]),
                               z[6], z[7], z[8], z[9]))
.d <- as.data.frame(do.call(rbind, .b), stringsAsFactors = FALSE)
names(.d) <- c("row", .h)
.d

## ---- cleanstates
st[st$abbrev %in% c("AK", "AL", "AR", "AZ"),
   c("state", "abbrev", "ev", "harris", "trump", "other", "winner", "margin")]

## ---- one-record
o <- st[st$state == "Pennsylvania",
        c("state", "abbrev", "ev", "harris", "trump", "other", "winner", "margin")]
names(o) <- c("state", "code", "electoral votes", "Harris (%)", "Trump (%)",
              "other (%)", "winner", "margin (Trump − Harris)")
o

## ---- close-states
o <- st[st$abs_margin < moe(1000), ]
o <- o[order(o$abs_margin), c("state", "ev", "harris", "trump", "margin", "winner")]
names(o) <- c("state", "electoral votes", "Harris (%)", "Trump (%)",
              "margin (Trump − Harris)", "winner")
o

## ---- gridmap-static
mm    <- moe(1000)
cap40 <- pmin(st$abs_margin, 40)
ramp  <- colorRampPalette(c(CM, "#9fd0d3", "#f2f2f2"))(101)
fillc <- ramp[round(100 * cap40 / 40) + 1]
tight <- st$abs_margin < mm
par(mar = c(0.3, 0.3, 2.6, 0.3))
plot(NA, xlim = c(0.4, 11.6), ylim = c(8.6, 0.4), axes = FALSE, ann = FALSE,
     asp = 1)
rect(st$col - 0.46, st$row - 0.46, st$col + 0.46, st$row + 0.46,
     col = fillc, border = "white", lwd = 1.1)
rect(st$col[tight] - 0.46, st$row[tight] - 0.46,
     st$col[tight] + 0.46, st$row[tight] + 0.46,
     col = NA, border = "#111111", lwd = 2.8)
tcol <- ifelse(cap40 < 15, "white", "#333333")
text(st$col, st$row - 0.08, st$abbrev, cex = 0.60, font = 2, col = tcol)
text(st$col, st$row + 0.26, pc(st$abs_margin, 1), cex = 0.46, col = tcol)
lx <- seq(2.1, 5.1, length.out = 101)
rect(lx, 0.72, lx + 0.031, 0.92, col = ramp, border = NA)
text(2.1, 1.10, "dead heat", cex = 0.58, adj = 0)
text(5.13, 1.10, "40+ point margin", cex = 0.58, adj = 1)
rect(6.10, 0.72, 6.42, 0.92, col = NA, border = "#111111", lwd = 2.4)
text(6.55, 0.83, paste0("closer than \u00b1", pc(mm, 2), " points"), cex = 0.58,
     adj = 0)
mtext(paste0("One square per state, shaded by how close it was. Black outline: ",
             "decided by less than the \u00b1", pc(mm, 2),
             " a poll of 1,000 claims."),
      side = 3, line = 1.4, cex = 0.76, adj = 0)
mtext(paste0(close_n, " of the ", nrow(st), " squares are outlined, together ",
             close_ev, " electoral votes. The tightest was ",
             st$state[which.min(st$abs_margin)], " at ",
             pc(min(st$abs_margin), 2), " points."),
      side = 3, line = 0.4, cex = 0.68, adj = 0, col = "#555555")

## ---- gridmap-d3
mm   <- moe(1000)
rows <- paste(sprintf('{"a":"%s","s":"%s","c":%d,"r":%d,"m":%.2f,"e":%d,"w":"%s","t":%d}',
                      st$abbrev, st$state, st$col, st$row, st$abs_margin,
                      st$ev, st$winner, as.integer(st$abs_margin < mm)),
              collapse = ",")
cat(sprintf('
<div id="gmap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MM=%.4f;
const cell=58, W=11*cell+40, H=8*cell+74, M={t:52,l:20};
const box=d3.select("#gmap");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const col=d3.scaleLinear().domain([0,40]).clamp(true)
  .range(["%s","#f2f2f2"]).interpolate(d3.interpolateRgb);
const gx=d=>M.l+(d.c-1)*cell, gy=d=>M.t+(d.r-1)*cell;
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
const g=svg.append("g").selectAll("g").data(D).join("g")
  .attr("transform",d=>`translate(${gx(d)},${gy(d)})`)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(`<b>${d.s}</b><br>${d.w} by ${d.m.toFixed(2)} points`+
      `<br>${d.e} electoral vote${d.e===1?"":"s"}`+
      (d.t?`<br><i>closer than \\u00b1${MM.toFixed(2)}</i>`:""))
      .style("left",Math.min(gx(d)+cell+6,W-230)+"px").style("top",(gy(d)-4)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
g.append("rect").attr("width",cell-4).attr("height",cell-4)
  .attr("fill",d=>col(d.m)).attr("stroke",d=>d.t?"#111":"#fff")
  .attr("stroke-width",d=>d.t?3:1.4);
// on-mark: these two sit inside the cell, so the fill is chosen against the
// cell and must not follow the page into dark. The caption below the grid uses
// the same #333 and is on the page, which is why this is classed per text.
g.append("text").attr("x",(cell-4)/2).attr("y",(cell-4)/2+1).attr("class","on-mark")
  .attr("text-anchor","middle").attr("font-size","14px").attr("font-weight","600")
  .attr("fill",d=>d.m<15?"#fff":"#333").text(d=>d.a);
g.append("text").attr("x",(cell-4)/2).attr("y",(cell-4)/2+17).attr("class","on-mark")
  .attr("text-anchor","middle").attr("font-size","10.5px")
  .attr("fill",d=>d.m<15?"#fff":"#666").text(d=>d.m.toFixed(1));
const lg=svg.append("g").attr("transform",`translate(${M.l},14)`);
const nstop=60;
lg.selectAll("rect").data(d3.range(nstop)).join("rect")
  .attr("x",d=>d*3.4).attr("width",3.6).attr("height",11)
  .attr("fill",d=>col(d/(nstop-1)*40));
lg.append("text").attr("x",0).attr("y",25).attr("font-size","11px")
  .attr("fill","#555").text("dead heat");
lg.append("text").attr("x",nstop*3.4).attr("y",25).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#555").text("40+ point margin");
lg.append("rect").attr("x",250).attr("y",0).attr("width",11).attr("height",11)
  .attr("fill","none").attr("stroke","#111").attr("stroke-width",3);
lg.append("text").attr("x",268).attr("y",10).attr("font-size","11px")
  .attr("fill","#333").text("closer than \\u00b1"+MM.toFixed(2)+" points");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%d of the %d squares carry a black outline: %s, together %d electoral votes,
came in closer than the ±%.2f a poll of 1,000 advertises. The tightest was
%s at %.2f points. Hover any square.</p>
', rows, mm, CM, close_n, nrow(st),
   paste(st$abbrev[st$abs_margin < mm][order(st$abs_margin[st$abs_margin < mm])],
         collapse = ", "),
   close_ev, mm, st$state[which.min(st$abs_margin)], min(st$abs_margin)))

## ---- population
data.frame(candidate = c("Harris", "Trump"),
           voters = cnt(as.vector(table(population))[c(1, 2)]),
           share  = pc(100 * as.vector(table(population))[c(1, 2)] / N, 2))

## ---- three-polls
o <- data.frame(poll = 1:3,
                estimate = pc(replicate(3, 100 * mean(sample(population, 1000) == "Harris")), 2),
                truth = pc(100 * truth, 2))
names(o) <- c("poll", "estimate of Harris's share (%)", "truth (%)")
o

## ---- honest-static
brk <- seq(43, 61, 0.25)
hh  <- hist(honest_1000, breaks = brk, plot = FALSE)   # honest polls
hb  <- hist(biased_1000, breaks = brk, plot = FALSE)   # the same polls, gap
par(mar = c(7.0, 4.4, 2.2, 1.2))
plot(NA, xlim = c(44, 60), ylim = c(0, max(hh$counts, hb$counts) * 1.06),
     las = 1, bty = "n", xlab = "poll estimate of Harris's share (%)",
     ylab = "polls")
plot(hh, add = TRUE, col = CS, border = "white")
plot(hb, add = TRUE, col = CG, border = "white")
abline(v = 100 * truth, lwd = 3, col = CT)
abline(v = avg_biased,  lwd = 3, col = CG)
# both rules are labeled in the top margin, where nothing can collide with a bar
mtext(lab_truth, side = 3, at = 100 * truth, line = 0.25, cex = 0.7, col = CT)
mtext(lab_avgb,  side = 3, at = avg_biased,  line = 0.25, cex = 0.7, col = CG)
legend("topleft", c("honest polls", "polls with the response gap"),
       fill = c(CS, CG), border = "white", bty = "n", cex = 0.74)
cw <- strwrap(cap_step5, width = 96)
mtext(cw, side = 1, line = 4.3 + (seq_along(cw) - 1) * 0.95, at = 44, adj = 0,
      cex = 0.68, col = "#555555")

## ---- honest-d3
mk <- function(x) {
  h <- hist(x, breaks = seq(43, 61, 0.25), plot = FALSE)
  paste(sprintf('[%.3f,%d]', h$mids, h$counts), collapse = ",")
}
cat(sprintf('
<div id="polls" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const H=[%s], B=[%s], TRUTH=%.4f, AVG=%.4f;
const LT="%s", LA="%s";
const W=770,H2=400,M={t:20,r:24,b:44,l:52};
const svg=d3.select("#polls").append("svg").attr("viewBox",`0 0 ${W} ${H2}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([44,60]).range([M.l,W-M.r]);
const mx=d3.max(H.concat(B),d=>d[1]);
const y=d3.scaleLinear().domain([0,mx]).range([H2-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H2-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H2-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("poll estimate of Harris\\u2019s share");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H2-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444").text("polls");
const bw=x(0.25)-x(0);
function bars(D,col,op,cls){
  svg.append("g").attr("class",cls).selectAll("rect").data(D).join("rect")
   .attr("x",d=>x(d[0])-bw/2).attr("y",d=>y(d[1])).attr("width",Math.max(bw-0.6,1))
   .attr("height",d=>y(0)-y(d[1])).attr("fill",col).attr("fill-opacity",op);
}
bars(H,"%s",0.85,"h"); bars(B,"%s",0.85,"b");
function rule(v,col,txt,dy){
  svg.append("line").attr("x1",x(v)).attr("x2",x(v)).attr("y1",M.t).attr("y2",H2-M.b)
    .attr("stroke",col).attr("stroke-width",2.6);
  svg.append("text").attr("x",x(v)+6).attr("y",M.t+dy).attr("font-size","11.5px")
    .attr("fill",col).text(txt);
}
rule(TRUTH,"%s",LT,12);
rule(AVG,"%s",LA,28);
const lg=svg.append("g").attr("transform",`translate(${M.l+8},${M.t+4})`);
[["%s","honest polls"],["%s","polls with the response gap"]].forEach((v,i)=>{
  lg.append("rect").attr("x",0).attr("y",i*17).attr("width",11).attr("height",11)
    .attr("fill",v[0]);
  lg.append("text").attr("x",17).attr("y",i*17+10).attr("font-size","11.5px")
    .attr("fill","#444").text(v[1]);
});
})();
</script>
', mk(honest_1000), mk(biased_1000), 100 * truth, avg_biased,
   lab_truth, lab_avgb, CS, CG, CT, CG, CS, CG))

## ---- coverage
data.frame(
  quantity = c("Margin of error claimed at n = 1,000",
               "Polls landing inside it", "Average of the thousand polls",
               "The truth"),
  value = c(paste0("±", pc(moe(1000), 2), " points"),
            paste0(pc(100 * mean(abs(honest_1000 - 100 * truth) <= moe(1000))), "%"),
            paste0(pc(mean(honest_1000), 2), "%"),
            paste0(pc(100 * truth, 2), "%")))

## ---- moe-table
o <- data.frame(respondents = cnt(c(500, 1000, 5000, 20000, 100000)),
                margin = paste0("±", pc(moe(c(500, 1000, 5000, 20000, 100000)), 2)))
names(o) <- c("people polled", "margin of error (points)")
o

## ---- the-pivot
o <- data.frame(
  n = cnt(ns),
  estimate = pc(draw, 2),
  claimed = paste0("±", pc(moe(ns), 2)),
  wrong_by = sg(draw - 100 * truth),
  multiple = paste0(pc(abs(draw - 100 * truth) / moe(ns)), "×"))
names(o) <- c("people polled", "estimate (%)", "advertised margin",
              "actually wrong by", "as a multiple of its own margin")
o

## ---- asymptote
data.frame(
  quantity = c("The truth", "What the poll converges on as n → ∞",
               "The bias", "Margin of error at n = 100,000"),
  value = c(paste0(pc(100 * truth, 2), "%"), paste0(pc(asym(GAP), 2), "%"),
            paste0(sg(asym(GAP) - 100 * truth), " points"),
            paste0("±", pc(moe(1e5), 2), " points")))

## ---- fan-static
nf <- unique(round(exp(seq(log(100), log(1e6), length.out = 200))))
par(mar = c(9.4, 4.4, 1.2, 8.6))
plot(NA, xlim = c(100, 1e6), ylim = c(42, 64), log = "x", las = 1, bty = "n",
     xlab = "people polled (log scale)",
     ylab = "estimate of Harris's share (%)", xaxt = "n")
axis(1, at = 10^(2:6), labels = c("100", "1,000", "10k", "100k", "1m"))
polygon(c(nf, rev(nf)),
        c(100 * truth + moe(nf), rev(100 * truth - moe(nf))),
        col = CSF, border = NA)
polygon(c(nf, rev(nf)),
        c(asym(GAP) + moe(nf), rev(asym(GAP) - moe(nf))),
        col = CGF, border = NA)
abline(v = ncross, lty = 3, col = "#555555")
lines(range(nf), rep(100 * truth, 2), col = CT, lwd = 2.8)
lines(range(nf), rep(asym(GAP), 2), col = CG, lwd = 2.6, lty = 2)
points(ns, draw, pch = 21, bg = CG, col = "white", lwd = 1.4, cex = 1.5)
text(1e6, 100 * truth - 1.1, lab_truth, col = CT, cex = 0.72, pos = 4,
     xpd = NA)
text(1e6, asym(GAP) + 1.1, paste0("what biased polls\nconverge on ",
     pc(asym(GAP), 2), "%"), col = CG, cex = 0.72, pos = 4, xpd = NA)
text(ncross, 63.4, paste0("n = ", cnt(ncross)), cex = 0.68, col = "#555555",
     pos = 4)
legend(10^3.15, 47.6, c("honest polling, 95% of draws",
                        "polls with the response gap, 95% of draws",
                        "the three polls in the table above"),
       fill = c(CSF, CGF, NA), border = NA,
       pch = c(NA, NA, 21), pt.bg = c(NA, NA, CG), col = c(NA, NA, "white"),
       bty = "n", cex = 0.68)
cw <- strwrap(cap_fan, width = 100)
# no `at`: on a log axis mtext reads `at` as a data value, so adj alone is the
# safe way to pin the caption to the left edge of the plot region
mtext(cw, side = 1, line = 3.6 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.66, col = "#555555")

## ---- fan-d3
nf     <- unique(round(exp(seq(log(100), log(1e6), length.out = 200))))
band   <- paste(sprintf('[%d,%.3f,%.3f,%.3f,%.3f]', nf,
                        100 * truth - moe(nf), 100 * truth + moe(nf),
                        asym(GAP) - moe(nf), asym(GAP) + moe(nf)),
                collapse = ",")
obs <- paste(sprintf('[%d,%.3f]', ns, draw), collapse = ",")
cat(sprintf('
<div id="fan" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], O=[%s], TRUTH=%.4f, ASY=%.4f, NC=%.1f;
const W=780,H=420,M={t:16,r:184,b:46,l:56};
const box=d3.select("#fan");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([100,1e6]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([42,64]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickValues([100,1e3,1e4,1e5,1e6])
    .tickFormat(d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("people polled (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("estimate of Harris\\u2019s share");
const area=(a,b)=>d3.area().x(d=>x(d[0])).y0(d=>y(d[a])).y1(d=>y(d[b]));
svg.append("path").datum(D).attr("fill","%s").attr("d",area(1,2));
svg.append("path").datum(D).attr("fill","%s").attr("d",area(3,4));
svg.append("line").attr("x1",x(NC)).attr("x2",x(NC)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#555").attr("stroke-dasharray","2,3");
svg.append("text").attr("x",x(NC)+5).attr("y",M.t+11).attr("font-size","11px")
  .attr("fill","#555").text("n = "+Math.round(NC));
function rule(v,c,dash,txt,dy){
  svg.append("line").attr("x1",x(100)).attr("x2",x(1e6)).attr("y1",y(v)).attr("y2",y(v))
    .attr("stroke",c).attr("stroke-width",2.6).attr("stroke-dasharray",dash);
  svg.append("text").attr("x",W-M.r+8).attr("y",y(v)+dy).attr("font-size","11.5px")
    .attr("fill",c).text(txt);
}
rule(TRUTH,"%s",null,"%s",14);
rule(ASY,"%s","6,4","biased polls converge here",-6);
svg.append("text").attr("x",W-M.r+8).attr("y",y(ASY)+8).attr("font-size","11.5px")
  .attr("fill","%s").text("%s");
svg.append("g").selectAll("circle").data(O).join("circle")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",5.5)
  .attr("fill","%s").attr("stroke","#fff").attr("stroke-width",1.4);
const lg=svg.append("g").attr("transform",`translate(${M.l+10},${M.t+6})`);
[["%s","honest polling, 95%% of draws"],
 ["%s","polls with the response gap, 95%% of draws"]].forEach((v,i)=>{
  lg.append("rect").attr("x",0).attr("y",i*17).attr("width",11).attr("height",11)
    .attr("fill",v[0]);
  lg.append("text").attr("x",17).attr("y",i*17+10).attr("font-size","11px")
    .attr("fill","#555").text(v[1]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s</p>
', band, obs, 100 * truth, asym(GAP), ncross,
   CSF, CGF, CT, lab_truth, CG, CG, paste0(pc(asym(GAP), 2), "%"), CG,
   CSF, CGF, cap_fan))

## ---- gaps
o <- data.frame(
  gap = paste0(pc(100 * gs, 0), "%"),
  bias = sg(sapply(gs, asym) - 100 * truth),
  moe = paste0("±", pc(moe(1000), 2)),
  vs_margin = pc((sapply(gs, asym) - 100 * truth) / true_margin, 1))
names(o) <- c("response gap", "bias (points)", "advertised margin at n = 1,000",
              "as a multiple of the simulated election margin")
o

## ---- gapbell-static
mm <- moe(1000)
yy <- rev(seq_along(gs))
par(mar = c(8.8, 7.4, 2.4, 2.2))
plot(NA, xlim = c(0, max(bi) * 1.16), ylim = c(0.5, length(gs) + 0.5),
     yaxt = "n", bty = "n", las = 1, ylab = "",
     xlab = "points")
abline(v = pretty(c(0, max(bi))), col = "grey93")
abline(v = true_margin, lty = 2, col = CT, lwd = 1.8)
abline(v = mm, lty = 2, col = CS, lwd = 1.8)
# One variable, one channel: which side of the advertised margin a bias falls
# on is already carried by position, so color does not repeat it. Gray is the
# advertised margin, orange is the bias the response gap produces.
segments(mm, yy, bi, yy, col = CG, lwd = 2.6)
points(rep(mm, length(gs)), yy, pch = 21, bg = "white", col = CS,
       cex = 1.4, lwd = 2.2)
points(bi, yy, pch = 19, col = CG, cex = 1.4)
axis(2, at = yy, labels = paste0(pc(100 * gs, 0), "% response gap"), las = 1,
     tick = FALSE, cex.axis = 0.8)
text(bi, yy, pc(bi, 2), pos = ifelse(bi > mm, 4, 2), cex = 0.72, xpd = NA,
     col = CGD)
text(mm, length(gs) + 0.30, paste0("advertised \u00b1", pc(mm, 2)), cex = 0.7,
     col = "#666666", pos = 4, xpd = NA)
text(true_margin, length(gs) + 0.62,
     paste0("the election margin ", pc(true_margin, 2)),
     cex = 0.7, col = CT, pos = 2, xpd = NA)
cw <- strwrap(cap_gap, width = 92)
mtext(cw, side = 1, line = 4.3 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.66, col = "#555555")

## ---- gapbell-d3
mm   <- moe(1000)
rows <- paste(sprintf('{"g":"%s%%","b":%.4f,"v":"%s"}', pc(100 * gs, 0), bi,
                      pc(bi, 2)), collapse = ",")
cat(sprintf('
<div id="gbell" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MM=%.4f, EM=%.4f;
const W=760,H=310,M={t:44,r:78,b:44,l:158};
const svg=d3.select("#gbell").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.b)*1.1]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.g)).range([M.t,H-M.b]).padding(0.42);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(7));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).select(".domain").remove();
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("points");
[[EM,"%s","the election margin %s"],
 [MM,"%s","advertised \\u00b1%s"]].forEach((v,i)=>{
  svg.append("line").attr("x1",x(v[0])).attr("x2",x(v[0])).attr("y1",M.t-6)
    .attr("y2",H-M.b).attr("stroke",v[1]).attr("stroke-width",1.8)
    .attr("stroke-dasharray","5,4");
  svg.append("text").attr("x",x(v[0])).attr("y",M.t-14-i*15).attr("font-size","11px")
    .attr("fill",v[1]).attr("text-anchor","middle").text(v[2]);
});
// One variable, one channel: position already says which side of the
// advertised margin a bias falls on, so color is not a second copy of it.
const cy=d=>y(d.g)+y.bandwidth()/2;
const g=svg.append("g");
g.selectAll("line").data(D).join("line")
  .attr("x1",x(MM)).attr("x2",d=>x(d.b)).attr("y1",cy).attr("y2",cy)
  .attr("stroke","%s").attr("stroke-width",3);
g.selectAll("circle.m").data(D).join("circle")
  .attr("cx",x(MM)).attr("cy",cy).attr("r",6).attr("fill","#fff")
  .attr("stroke","%s").attr("stroke-width",2.4);
g.selectAll("circle.b").data(D).join("circle")
  .attr("cx",d=>x(d.b)).attr("cy",cy).attr("r",6).attr("fill","%s");
g.selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(d.b)+(d.b>MM?11:-11)).attr("y",d=>cy(d)+4)
  .attr("text-anchor",d=>d.b>MM?"start":"end")
  .attr("font-size","11.5px").attr("fill","%s").text(d=>d.v);
})();
</script>
', rows, mm, true_margin,
   CT, pc(true_margin, 2), CS, pc(mm, 2),
   CG, CS, CG, CGD))

## ---- weighting
o <- data.frame(
  estimate = c("The truth", "Raw poll", "Weighted on trust",
               "Weighted on a variable unrelated to response"),
  value = pc(c(true_share, raw_est, est_right, est_wrong), 2),
  error = c("—", sg(raw_est - true_share), sg(est_right - true_share),
            sg(est_wrong - true_share)))
names(o) <- c("estimate", "Harris share (%)", "error (points)")
o

## ---- wmatrix-static
popt <- table(factor(trust, 0:1), factor(harris, 0:1))
smpt <- table(factor(s$trust, 0:1), factor(s$harris, 0:1))
pp   <- 100 * popt / sum(popt)
psm  <- 100 * smpt / sum(smpt)
dif  <- psm - pp
mxd  <- max(abs(dif))
# Orange over, purple under: the columns of this grid ARE the two candidates,
# so a red/blue diverging scale would be read as party rather than as
# over- and under-representation.
dv   <- colorRampPalette(c(CU, "#f7f7f7", CG))(101)
cix  <- function(v) round(50 * (v / mxd) + 51)
par(mar = c(7.6, 6.6, 3.4, 1.4))
plot(NA, xlim = c(0, 2), ylim = c(-0.42, 2), axes = FALSE, ann = FALSE)
for (r in 1:2) for (cc in 1:2) {
  tr <- c(1, 0)[r]; hv <- c(1, 0)[cc]
  ri <- as.character(tr); ci <- as.character(hv)
  x0 <- cc - 1; y0 <- 2 - r
  rect(x0, y0, x0 + 1, y0 + 1, col = dv[cix(dif[ri, ci])], border = "white",
       lwd = 3)
  strong <- abs(dif[ri, ci]) > 0.62 * mxd
  tc <- if (strong) "white" else "#333333"
  text(x0 + 0.5, y0 + 0.70, paste0("population ", pc(pp[ri, ci], 1), "%"),
       cex = 0.80, col = tc)
  text(x0 + 0.5, y0 + 0.52, paste0("poll ", pc(psm[ri, ci], 1), "%"),
       cex = 0.80, font = 2, col = tc)
  text(x0 + 0.5, y0 + 0.28, sg(dif[ri, ci], 1), cex = 1.05, font = 2,
       col = if (strong) "white" else
             if (dif[ri, ci] > 0) CGD else CUD)
}
axis(3, at = c(0.5, 1.5), labels = c("voted Harris", "voted Trump"),
     tick = FALSE, line = -0.6, cex.axis = 0.9)
axis(2, at = c(1.5, 0.5), labels = c("trusts\ninstitutions",
                                     "does not\ntrust"),
     tick = FALSE, las = 1, line = -0.4, cex.axis = 0.85)
# the color key the print twin was missing entirely
lx <- seq(0.42, 1.58, length.out = 101)
rect(lx, -0.30, lx + diff(lx)[1], -0.17, col = dv, border = NA)
text(0.40, -0.235, paste0("under-represented ", sg(-mxd, 1)), cex = 0.62,
     adj = 1, col = CUD, xpd = NA)
text(1.60, -0.235, paste0("over-represented ", sg(mxd, 1)), cex = 0.62,
     adj = 0, col = CGD, xpd = NA)
text(1.0, -0.40, "poll share minus population share, in points", cex = 0.62,
     col = "#555555")
mtext(paste0("Poll share minus population share. Harris is ",
             pc(true_share, 1), "% of the country and ",
             pc(raw_est, 1), "% of the raw poll."),
      side = 1, line = 1.0, cex = 0.76, adj = 0)
cw <- strwrap(cap_wmx, width = 92)
mtext(cw, side = 1, line = 2.4 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.66, col = "#555555")

## ---- wmatrix-d3
popt <- table(factor(trust, 0:1), factor(harris, 0:1))
smpt <- table(factor(s$trust, 0:1), factor(s$harris, 0:1))
pp   <- 100 * popt / sum(popt)
psm  <- 100 * smpt / sum(smpt)
cells <- do.call(rbind, lapply(1:2, function(r) do.call(rbind, lapply(1:2, function(cc) {
  ri <- as.character(c(1, 0)[r]); ci <- as.character(c(1, 0)[cc])
  data.frame(r = r - 1, c = cc - 1, p = pp[ri, ci], s = psm[ri, ci],
             d = psm[ri, ci] - pp[ri, ci])
}))))
rows <- paste(sprintf('{"r":%d,"c":%d,"p":%.2f,"s":%.2f,"d":%.2f,"v":"%s"}',
                      cells$r, cells$c, cells$p, cells$s, cells$d,
                      sg(cells$d, 1)),
              collapse = ",")
cat(sprintf('
<div id="wmx" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MXD=%.3f;
const cw=246, ch=132, W=2*cw+186, H=2*ch+146, M={t:56,l:180};
const svg=d3.select("#wmx").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
// orange over, purple under: the columns here are the two candidates, so a
// red/blue diverging scale would be read as party
const col=d3.scaleLinear().domain([-MXD,0,MXD])
  .range(["%s","#f7f7f7","%s"]).interpolate(d3.interpolateRgb);
["voted Harris","voted Trump"].forEach((t,i)=>
  svg.append("text").attr("x",M.l+i*cw+cw/2).attr("y",M.t-14)
    .attr("text-anchor","middle").attr("font-size","13px").attr("fill","#333").text(t));
["trusts institutions","does not trust"].forEach((t,i)=>
  svg.append("text").attr("x",M.l-14).attr("y",M.t+i*ch+ch/2+4)
    .attr("text-anchor","end").attr("font-size","13px").attr("fill","#333").text(t));
const g=svg.append("g").selectAll("g").data(D).join("g")
  .attr("transform",d=>`translate(${M.l+d.c*cw},${M.t+d.r*ch})`);
g.append("rect").attr("width",cw-5).attr("height",ch-5).attr("fill",d=>col(d.d))
  .attr("stroke","#fff").attr("stroke-width",3);
const strong=d=>Math.abs(d.d)>0.62*MXD;
g.append("text").attr("x",(cw-5)/2).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("fill",d=>strong(d)?"#fff":"#444")
  .text(d=>"population "+d.p.toFixed(1)+"%%");
g.append("text").attr("x",(cw-5)/2).attr("y",56).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("font-weight","600")
  .attr("fill",d=>strong(d)?"#fff":"#222")
  .text(d=>"poll "+d.s.toFixed(1)+"%%");
g.append("text").attr("x",(cw-5)/2).attr("y",92).attr("text-anchor","middle")
  .attr("font-size","20px").attr("font-weight","700")
  .attr("fill",d=>strong(d)?"#fff":(d.d>0?"%s":"%s"))
  .text(d=>d.v);
const lg=svg.append("g").attr("transform",`translate(${M.l+cw/2},${H-70})`);
d3.range(101).forEach(i=>{
  lg.append("rect").attr("x",i*2.0).attr("y",0).attr("width",2.2).attr("height",12)
    .attr("fill",col(-MXD+2*MXD*i/100));
});
lg.append("text").attr("x",-8).attr("y",10).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","%s").text("under-represented %s");
lg.append("text").attr("x",101*2.0+8).attr("y",10)
  .attr("font-size","11px").attr("fill","%s").text("over-represented %s");
lg.append("text").attr("x",101).attr("y",30).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#555")
  .text("poll share minus population share, in points");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s</p>
', rows, max(abs(cells$d)), CU, CG, CGD, CUD,
   CUD, sg(-max(abs(cells$d)), 1), CGD, sg(max(abs(cells$d)), 1), cap_wmx))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#gmap text[fill="#666" i],
#wmx text[fill="#222" i],
#wmx text[fill="#444" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: the on-mark class pins #gmap labels dark for the dark
# page, so a --paper stroke there would sit dark behind a dark ink, and the
# checker scores the fill against the stroke it touches.
cat('<style>
#fan text[fill="currentcolor" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#gmap text[fill="#666" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
#wmx text[fill="#fff" i],
#wmx text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#wmx text[fill="#fff" i],
#wmx text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
