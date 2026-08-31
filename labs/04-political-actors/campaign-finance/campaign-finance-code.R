# campaign-finance-code.R -- chunk bodies for campaign-finance-brief.Rmd
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

fec <- read.csv("data/derived/fec_candidates_2024.csv", stringsAsFactors = FALSE)

h   <- fec[fec$office == "House", ]
hs  <- h[h$ici %in% c("I", "C", "O"), ]
ser <- hs[hs$ttl_receipts > 50000, ]
ser$pac_share <- 100 * ser$pac_contrib / ser$ttl_receipts

sen <- fec[fec$office == "Senate" & fec$ici %in% c("I", "C", "O") &
           fec$ttl_receipts > 50000, ]
sen$pac_share <- 100 * sen$pac_contrib / sen$ttl_receipts

MEDH <- median(h$ttl_receipts); MNH <- mean(h$ttl_receipts)
r    <- sort(h$ttl_receipts, decreasing = TRUE)
TOP10 <- 100 * sum(r[1:round(0.10 * length(r))]) / sum(r)
BOT50 <- 100 * sum(r[(round(0.5 * length(r)) + 1):length(r)]) / sum(r)

med <- function(d, k) median(d$ttl_receipts[d$ici == k])
pacm <- function(d, k) median(d$pac_share[d$ici == k])
LAB <- c(C = "Challenger", O = "Open seat", I = "Incumbent")
RATIO <- med(hs, "I") / med(hs, "C")

src <- sapply(c("C", "O", "I"), function(k) {
  x <- ser[ser$ici == k, ]
  c(Individuals = sum(x$indiv_contrib), PACs = sum(x$pac_contrib),
    Party = sum(x$party_contrib), Self = sum(x$self_funding)) /
    sum(x$ttl_receipts) * 100
})

NAIVE <- sum(fec$ttl_receipts)
nodup <- fec[!duplicated(fec$ttl_receipts) | fec$ttl_receipts == 0, ]
DEDUP <- sum(nodup$ttl_receipts)
BH <- fec[grepl("HARRIS, KAMALA|BIDEN, JOSEPH", fec$cand_name), ]

bigc <- fec[fec$ttl_receipts > 100000, ]
SELF <- sum(100 * bigc$self_funding / bigc$ttl_receipts > 50)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
d  <- function(x) paste0("$", n(x))
dm <- function(x) paste0("$", n(x / 1e6), "m")

# ---- Figure 1: Lorenz curve of House receipts ------------------------------
# Every House candidate, sorted from worst-funded to best-funded. LX is the
# cumulative share of candidates, LY the cumulative share of all House money.
lz   <- sort(h$ttl_receipts)
NH   <- length(lz)
NB   <- NH - round(0.5 * NH)          # candidates in the worst-funded half
NT   <- round(0.10 * NH)              # candidates in the best-funded tenth
LX   <- 100 * seq_len(NH) / NH
LY   <- 100 * cumsum(lz) / sum(lz)
BX   <- LX[NB];      BY <- LY[NB]           # BY is BOT50
AX   <- LX[NH - NT]; AY <- LY[NH - NT]      # 100 - AY is TOP10
ZERO <- sum(h$ttl_receipts == 0)
# thin the 3,000-point polyline for the browser, keeping the two marked points
KI <- sort(unique(c(1, round(seq(1, NH, length.out = 220)), NB, NH - NT, NH)))

LAB_A <- paste0("best-funded 10% (", n(NT), " candidates)")
LAB_A2 <- paste0("raised ", pc(TOP10), "% of all House money")
LAB_B <- paste0("worst-funded 50% (", n(NB), " candidates)")
LAB_B2 <- paste0("raised ", pc(BOT50), "%")

# ---- Figure 3: mosaic of sources by status ---------------------------------
# Bar width is the group's aggregate dollars; segment height is that source's
# share of them. The four named sources do not exhaust receipts, so the
# remainder is carried explicitly.
GT   <- sapply(c("C", "O", "I"), function(k) sum(ser$ttl_receipts[ser$ici == k]))
GW   <- 100 * GT / sum(GT)
MOS  <- rbind(PACs = src["PACs", ], Individuals = src["Individuals", ],
              Party = src["Party", ], Self = src["Self", ],
              Other = 100 - colSums(src))
MCOL <- c(PACs = "#2166AC", Individuals = "#4d9221", Party = "#999999",
          Self = "#C41230", Other = "#e2e2e2")
MBOR <- ifelse(names(MCOL) == "Other", "#b0b0b0", NA)
MLABEL <- 5      # label a segment directly when it is at least this many %

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- fec[fec$office == "House" & fec$ici == "I", ]
o <- o[order(-o$ttl_receipts), ][2:4, ]
o <- o[, c("cand_name", "office_st", "ici", "ttl_receipts", "indiv_contrib",
           "pac_contrib")]
o$ttl_receipts <- d(o$ttl_receipts); o$indiv_contrib <- d(o$indiv_contrib)
o$pac_contrib <- d(o$pac_contrib)
names(o) <- c("candidate", "state", "I/C/O", "total receipts",
              "from individuals", "from PACs")
o

## ---- counts
o <- as.data.frame(table(office = fec$office), stringsAsFactors = FALSE)
names(o) <- c("office sought", "candidates")
o

## ---- fec-raw
Z  <- "data/raw/weball24.zip"
rl <- local({
  con <- unz(Z, "weball24.txt", open = "rb"); on.exit(close(con))
  readLines(con, warn = FALSE)
})
nraw <- length(rl)

# a candidate who both gave and lent to their own campaign, so the two
# columns the build adds together are both visible in the raw line
sf   <- fec[fec$cand_contrib > 0 & fec$cand_loans > 0, ]
pick <- sf$cand_id[which.max(sf$self_funding)]
line <- rl[startsWith(rl, paste0(pick, "|"))][1]
f    <- strsplit(line, "|", fixed = TRUE)[[1]]

# One field per row, numbered. The file states no names, so there is no name
# column to give: the position IS the identifier, and that is the problem the
# section goes on to describe.
data.frame(
  Field = seq_along(f),
  Value = ifelse(nzchar(f), f, "(empty)"))

## ---- fec-fields
data.frame(
  position = c(1, 2, 3, 6, 12, 13, 18, 26),
  value    = c(f[1], f[2], f[3], f[6], f[12], f[13], f[18], f[26]),
  becomes  = c("cand_id", "cand_name", "ici", "ttl_receipts", "cand_contrib",
               "cand_loans", "indiv_contrib", "pac_contrib"),
  what_it_is = c("the Commission's permanent ID for this candidate",
                 "the candidate's name, as filed",
                 "incumbent, challenger, or open seat — one letter",
                 "everything the committee raised this cycle",
                 "money the candidate gave their own campaign",
                 "money the candidate lent it, which is not the same thing",
                 "money from individual people",
                 "money from political action committees"))

## ---- fec-clean
o <- fec[fec$cand_id == pick,
         c("cand_name", "office", "ici", "ttl_receipts",
           "indiv_contrib", "pac_contrib", "self_funding")]
o$ttl_receipts <- d(o$ttl_receipts); o$indiv_contrib <- d(o$indiv_contrib)
o$pac_contrib  <- d(o$pac_contrib);  o$self_funding  <- d(o$self_funding)
names(o) <- c("candidate", "office", "I/C/O", "total receipts",
              "from individuals", "from PACs", "own money")
o

## ---- typical
data.frame(
  quantity = c("House candidates", "Median total raised", "Mean total raised",
               "Mean ÷ median"),
  value = c(n(nrow(h)), d(MEDH), d(MNH), paste0(pc(MNH / MEDH), "×")))

## ---- concentration
data.frame(
  group = c("Best-funded 10% of House candidates",
            "Worst-funded 50% of House candidates"),
  candidates = c(n(round(0.10 * nrow(h))), n(nrow(h) - round(0.5 * nrow(h)))),
  `share of all House money` = c(paste0(pc(TOP10), "%"), paste0(pc(BOT50), "%")),
  check.names = FALSE)

## ---- d3-lorenz
# ---------------------------------------------------------------------------
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here. Every coordinate and every label string is computed in R above and
# handed across finished, so this figure and the base-R one below are drawn
# from identical numbers.
# ---------------------------------------------------------------------------
pts <- paste0("[", pc(LX[KI], 3), ",", pc(LY[KI], 3), "]", collapse = ",")
cat(paste0('
<div id="lorenz" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const P=[', pts, '];
const AX=', pc(AX, 3), ', AY=', pc(AY, 3), ', BX=', pc(BX, 3), ', BY=', pc(BY, 3), ';
const W=720,H=440,M={t:24,r:26,b:54,l:64};
const svg=d3.select("#lorenz").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
const curve=P.map(p=>x(p[0])+","+y(p[1])).join("L");
svg.append("path").attr("d","M"+curve+"L"+x(100)+","+y(100)+"L"+x(0)+","+y(0)+"Z")
  .attr("fill","#ededed");
svg.append("line").attr("x1",x(0)).attr("y1",y(0)).attr("x2",x(100)).attr("y2",y(100))
  .attr("stroke","#999999").attr("stroke-width",1).attr("stroke-dasharray","5 4");
svg.append("path").attr("d","M"+curve).attr("fill","none")
  .attr("stroke","#333333").attr("stroke-width",2);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(5).tickFormat(v=>v+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(5).tickFormat(v=>v+"%"));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#666666")
  .text("cumulative share of House candidates, worst-funded first");
svg.append("text").attr("transform","translate(16,"+(M.t+(H-M.b-M.t)/2)+") rotate(-90)")
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#666666")
  .text("cumulative share of all House money");
// --paper, not #ffffff: on the dark page the fills lift and a white outline
// makes the label disappear into its own halo.
const halo="paint-order:stroke;stroke:var(--paper);stroke-width:3.5px;stroke-linejoin:round";
svg.append("text").attr("x",x(30)).attr("y",y(37)).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#666666").attr("style",halo)
  .text("line of equality");
svg.append("path").attr("d","M"+x(86.5)+","+y(61)+"L"+x(89)+","+y(31))
  .attr("stroke","#666666").attr("fill","none");
svg.append("path").attr("d","M"+x(46.5)+","+y(8)+"L"+x(49.4)+","+y(2))
  .attr("stroke","#666666").attr("fill","none");
[[86,72,', paste0('"', LAB_A, '"'), ',"end",400],
 [86,65,', paste0('"', LAB_A2, '"'), ',"end",700],
 [47,18,', paste0('"', LAB_B, '"'), ',"end",400],
 [47,11,', paste0('"', LAB_B2, '"'), ',"end",700]].forEach(function(q){
  // These are the Lorenz annotations. They carry the halo because they can
  // cross the curve, but they sit on the PAGE, so they must follow the theme.
  // They were briefly classed on-mark on the assumption that they were the
  // mosaic block labels; that put near-black text on the dark page at 1.03:1.
  svg.append("text").attr("x",x(q[0])).attr("y",y(q[1])+4)
    .attr("text-anchor",q[3]).attr("font-size","12px").attr("font-weight",q[4])
    .attr("fill","#333333").attr("style",halo).text(q[2]);
});
[[AX,AY],[BX,BY]].forEach(function(q){
  svg.append("circle").attr("cx",x(q[0])).attr("cy",y(q[1])).attr("r",5)
    .attr("fill","#333333").attr("stroke","#ffffff").attr("stroke-width",2);
});
})();
</script>
'))

## ---- lorenz-static
# The same curve, the same two marked points, the same four label strings:
# base R for the PDF device, D3 above for the browser.
par(mar = c(4.2, 4.4, 1.0, 1.2), mgp = c(2.6, 0.7, 0), cex = 0.9)
plot(NA, xlim = c(0, 100), ylim = c(0, 100), xaxs = "i", yaxs = "i",
     xlab = "cumulative share of House candidates, worst-funded first",
     ylab = "cumulative share of all House money", axes = FALSE)
polygon(c(LX[KI], 100, 0), c(LY[KI], 100, 0), col = "#ededed", border = NA)
abline(0, 1, col = "#999999", lty = 2)
lines(LX[KI], LY[KI], col = "#333333", lwd = 2)
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"))
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"), las = 1)
box(col = "#cccccc")
segments(86.5, 61, 89, 31, col = "#666666")
segments(46.5, 8, 49.4, 2, col = "#666666")
halo <- function(px, py, txt, cex = 0.72, font = 1, col = "#333333") {
  w <- strwidth(txt, cex = cex, font = font)
  hh <- strheight(txt, cex = cex, font = font)
  rect(px - w - 0.8, py - hh * 0.9, px + 0.8, py + hh * 0.9,
       col = "white", border = NA)
  text(px, py, txt, adj = c(1, 0.5), cex = cex, font = font, col = col)
}
halo(30, 37, "line of equality", cex = 0.68, col = "#666666")
halo(86, 72, LAB_A);  halo(86, 65, LAB_A2, font = 2)
halo(47, 18, LAB_B);  halo(47, 11, LAB_B2, font = 2)
points(c(AX, BX), c(AY, BY), pch = 21, bg = "#333333", col = "#ffffff",
       cex = 1.2, lwd = 1.6)

## ---- by-ici
o <- data.frame(
  status = unname(LAB[c("C", "O", "I")]),
  candidates = n(sapply(c("C","O","I"), function(k) sum(hs$ici == k))),
  `median raised` = d(sapply(c("C","O","I"), function(k) med(hs, k))),
  check.names = FALSE)
o

## ---- two-stories
o <- data.frame(
  story = c("Money makes incumbents", "Incumbents attract money"),
  claim = c("Contributions buy the advantages that keep members in office",
            "Strong candidates raise money and also win; the money is a symptom"),
  fits = c("Yes", "Yes"),
  check.names = FALSE)
names(o)[3] <- paste0("predicts the ", n(RATIO), ":1 gap?")
o

## ---- pac-share
o <- data.frame(
  status = unname(LAB[c("C", "O", "I")]),
  candidates = n(sapply(c("C","O","I"), function(k) sum(ser$ici == k))),
  `median PAC share of receipts` =
    paste0(pc(sapply(c("C","O","I"), function(k) pacm(ser, k))), "%"),
  check.names = FALSE)
o

## ---- pac-prep
# The three bar heights and the three printed labels are computed once, here,
# and handed to both renderers below. Formatting the same median twice -- once
# inside the D3 chunk and once inside the base-R chunk -- is how two versions of
# the same figure begin to disagree in the last decimal place.
PGRP <- unname(LAB[c("C", "O", "I")])
pacrow <- function(dd) data.frame(
  k = PGRP,
  v = sapply(c("C", "O", "I"), function(k) pacm(dd, k)),
  n = sapply(c("C", "O", "I"), function(k) sum(dd$ici == k)),
  stringsAsFactors = FALSE)
PH <- pacrow(ser); PS <- pacrow(sen)
PH$lab <- paste0(pc(PH$v), "%"); PS$lab <- paste0(pc(PS$v), "%")
pacjson <- function(p) paste(sprintf('{"k":"%s","v":%s,"n":%d,"l":"%s"}',
                                     p$k, pc(p$v, 3), p$n, p$lab), collapse = ",")

## ---- d3-pac
# One axis, two chambers, a button to swap between them. The bar heights and the
# printed labels both arrive from pac-prep; nothing numeric is computed here.
# d3 v7 is already loaded by Figure 1.
cat(sprintf('
<div id="pacs" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="pH" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">House</button>
  <button id="pS" style="font:12px inherit;padding:4px 10px;cursor:pointer">Senate</button>
 </div>
</div>
<script>
(function(){
const H=[%s], S=[%s];
const W=720,H2=330,M={t:20,r:30,b:40,l:120};
const svg=d3.select("#pacs").append("svg").attr("viewBox",`0 0 ${W} ${H2}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(H.map(q=>q.k)).range([M.l,W-M.r]).padding(0.32);
const y=d3.scaleLinear().domain([0,42]).range([H2-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H2-M.b})`).call(d3.axisBottom(x));
const gy=svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(v=>v+"%%"));
svg.append("text").attr("x",M.l).attr("y",M.t-6).attr("font-size","11px")
  .attr("fill","#666").text("median PAC share of the candidate\'s receipts");
const bars=svg.append("g"), labs=svg.append("g");
function draw(d,color){
  bars.selectAll("rect").data(d,q=>q.k).join(
    e=>e.append("rect").attr("x",q=>x(q.k)).attr("width",x.bandwidth())
        .attr("y",y(0)).attr("height",0).attr("rx",2),
    u=>u)
    .transition().duration(650)
    .attr("y",q=>y(q.v)).attr("height",q=>y(0)-y(q.v)).attr("fill",color);
  labs.selectAll("text").data(d,q=>q.k).join(
    e=>e.append("text").attr("text-anchor","middle").attr("font-size","12px").attr("fill","#333"),
    u=>u)
    .transition().duration(650)
    .attr("x",q=>x(q.k)+x.bandwidth()/2).attr("y",q=>y(q.v)-7)
    .text(q=>q.l);
}
draw(H,"#2166AC");
d3.select("#pH").on("click",()=>draw(H,"#2166AC"));
d3.select("#pS").on("click",()=>draw(S,"#54278F"));
})();
</script>
', pacjson(PH), pacjson(PS)))

## ---- pac-static
par(mfrow = c(1, 2), mar = c(3.5, 4, 2.5, 1))
for (P in list(list(PH, "House", "#2166AC"), list(PS, "Senate", "#54278F"))) {
  p <- P[[1]]
  bp <- barplot(p$v, names.arg = c("Chal.", "Open", "Inc."), col = P[[3]],
                border = NA, ylim = c(0, 42), main = P[[2]],
                ylab = "median PAC share (%)")
  text(bp, p$v, p$lab, pos = 3, xpd = TRUE, cex = 0.85)
}
par(mfrow = c(1, 1))

## ---- senate
o <- data.frame(
  status = unname(LAB[c("C", "O", "I")]),
  `House PAC share` = paste0(pc(sapply(c("C","O","I"), function(k) pacm(ser, k))), "%"),
  `Senate PAC share` = paste0(pc(sapply(c("C","O","I"), function(k) pacm(sen, k))), "%"),
  `Senate median raised` = d(sapply(c("C","O","I"), function(k) med(sen, k))),
  check.names = FALSE)
o

## ---- sources
o <- as.data.frame(round(src, 1))
names(o) <- unname(LAB[c("C", "O", "I")])
o <- cbind(source = rownames(o), o)
rownames(o) <- NULL
o

## ---- mosaic-prep
# geometry shared by both versions of the mosaic
GAP  <- 1.8                                   # blank axis between bars, %
WID  <- GW * (100 - 2 * GAP) / 100            # bar widths, % of the axis
X0   <- cumsum(c(0, WID + GAP))[1:3]          # bar left edges
GNM  <- unname(LAB[c("C", "O", "I")])
Y0   <- apply(MOS, 2, function(v) cumsum(c(0, v))[1:nrow(MOS)])
MTX  <- ifelse(rownames(MOS) == "Other", "#333333", "#ffffff")
MLAB <- sapply(1:3, function(j) {
  # narrow bars have room for the number but not for the source name
  full <- if (WID[j] >= 16) paste0(rownames(MOS), " ", pc(MOS[, j]), "%")
          else paste0(pc(MOS[, j]), "%")
  ifelse(MOS[, j] < MLABEL, "", full)
})

## ---- d3-mosaic
# Widths, heights, colors and every printed label come from mosaic-prep, so
# this figure and the base-R one below cannot drift apart. d3 v7 is already
# loaded by Figure 1.
bor <- ifelse(is.na(MBOR), "none", MBOR)
seg <- function(j) paste0(sapply(seq_len(nrow(MOS)), function(i) paste0(
  '{"y0":', pc(Y0[i, j], 3), ',"h":', pc(MOS[i, j], 3),
  ',"c":"', MCOL[rownames(MOS)[i]], '","b":"', bor[i], '","t":"', MTX[i],
  '","l":"', MLAB[i, j], '"}')), collapse = ",")
gj <- paste0(sapply(1:3, function(j) paste0(
  '{"k":"', GNM[j], '","x0":', pc(X0[j], 3), ',"w":', pc(WID[j], 3),
  ',"d":"', dm(GT[j]), ' raised","segs":[', seg(j), ']}')), collapse = ",")
lj <- paste0(sapply(seq_along(MCOL), function(i) paste0(
  '{"n":"', names(MCOL)[i], '","c":"', MCOL[i], '","b":"', bor[i], '"}')),
  collapse = ",")
cat(paste0('
<div id="mosaic" style="margin:1em 0"></div>
<script>
(function(){
const G=[', gj, '], L=[', lj, '];
const W=720,H=430,M={t:44,r:132,b:58,l:52};
const svg=d3.select("#mosaic").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(5).tickFormat(v=>v+"%"));
svg.append("text").attr("transform","translate(14,"+(M.t+(H-M.b-M.t)/2)+") rotate(-90)")
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#666666")
  .text("share of the group\'s receipts");
G.forEach(function(g){
  const gx=x(g.x0), gw=x(g.w)-x(0);
  svg.selectAll(null).data(g.segs).join("rect")
    .attr("x",gx).attr("width",gw)
    .attr("y",s=>y(s.y0+s.h)).attr("height",s=>y(0)-y(s.h))
    .attr("fill",s=>s.c).attr("stroke",s=>s.b);
  svg.selectAll(null).data(g.segs.filter(s=>s.l!=="")).join("text")
    .attr("x",gx+gw/2).attr("y",s=>y(s.y0+s.h/2)+4)
    .attr("text-anchor","middle").attr("font-size","11.5px")
    .attr("fill",s=>s.t).text(s=>s.l);
  svg.append("text").attr("x",gx+gw/2).attr("y",M.t-22).attr("text-anchor","middle")
    .attr("font-size","12.5px").attr("font-weight",700).attr("fill","#333333").text(g.k);
  svg.append("text").attr("x",gx+gw/2).attr("y",M.t-8).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#666666").text(g.d);
});
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-16).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#666666")
  .text("bar width = total dollars the group raised");
L.forEach(function(e,i){
  svg.append("rect").attr("x",W-M.r+16).attr("y",M.t+i*22).attr("width",13)
    .attr("height",13).attr("fill",e.c).attr("stroke",e.b);
  svg.append("text").attr("x",W-M.r+34).attr("y",M.t+i*22+11)
    .attr("font-size","11.5px").attr("fill","#333333").text(e.n);
});
})();
</script>
'))

## ---- mosaic-static
par(mar = c(3.4, 4.2, 5.4, 1.0), mgp = c(2.5, 0.7, 0), cex = 0.9)
plot(NA, xlim = c(0, 100), ylim = c(0, 100), xaxs = "i", yaxs = "i",
     axes = FALSE, xlab = "", ylab = "share of the group's receipts")
for (j in 1:3) {
  for (i in seq_len(nrow(MOS))) {
    rect(X0[j], Y0[i, j], X0[j] + WID[j], Y0[i, j] + MOS[i, j],
         col = MCOL[rownames(MOS)[i]], border = MBOR[i])
    if (nzchar(MLAB[i, j]))
      text(X0[j] + WID[j] / 2, Y0[i, j] + MOS[i, j] / 2, MLAB[i, j],
           col = MTX[i], cex = 0.66)
  }
  text(X0[j] + WID[j] / 2, 107, GNM[j], font = 2, cex = 0.78, xpd = TRUE)
  text(X0[j] + WID[j] / 2, 102, paste(dm(GT[j]), "raised"), col = "#666666",
       cex = 0.68, xpd = TRUE)
}
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"), las = 1)
mtext("bar width = total dollars the group raised", side = 1, line = 0.8,
      cex = 0.62, col = "#666666")
legend(0, 121, legend = rownames(MOS), fill = MCOL[rownames(MOS)],
       border = MBOR, horiz = TRUE, bty = "n", cex = 0.68, xpd = TRUE,
       x.intersp = 0.5)

## ---- self
data.frame(
  quantity = c("Candidates raising more than $100,000",
               "Of those, funding more than half of it themselves",
               "Share"),
  value = c(n(nrow(bigc)), n(SELF), paste0(pc(100 * SELF / nrow(bigc)), "%")))

## ---- biden-harris
o <- BH[, c("cand_id", "cand_name", "office", "ttl_receipts")]
o$ttl_receipts <- d(o$ttl_receipts)
names(o) <- c("FEC candidate ID", "candidate", "office", "total receipts")
o

## ---- dup-cost
data.frame(
  quantity = c("Total receipts, summing the column naively",
               "After removing repeated totals",
               "Difference"),
  value = c(paste0("$", pc(NAIVE / 1e9, 2), " billion"),
            paste0("$", pc(DEDUP / 1e9, 2), " billion"),
            paste0("$", pc((NAIVE - DEDUP) / 1e9, 2), " billion")))

## ---- no-outcome
data.frame(
  question = c("Who did each candidate raise money from?",
               "How much did they spend?",
               "Did they win?"),
  `answerable from this file` = c("Yes", "Yes",
                                  "No — the column is empty in the source"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# #lorenz was first given the token pin that #mosaic-style figures get, and
# that was wrong: only some of its #333333 is on a mark, and pinning the rest
# put near-black labels on the dark page at 1.03:1. A halo has no such
# failure mode -- it does nothing where the text is already on the page.
cat('<style>
#lorenz text[fill="#333333" i],
#mosaic text[fill="#333333" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
</style>')
