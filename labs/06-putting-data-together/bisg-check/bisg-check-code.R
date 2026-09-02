# bisg-check-code.R -- chunk bodies for bisg-check-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
source("../../_lib/dd-charts.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

v0 <- read.csv("data/derived/houston_voters.csv", colClasses = c(GEOID20 = "character"))
g  <- read.csv("data/derived/houston_blocks.csv",  colClasses = c(GEOID20 = "character"))
sn <- read.csv("../../06-putting-data-together/surnames/data/derived/census_surnames.csv",
               stringsAsFactors = FALSE)

R  <- c("white", "black", "hispanic", "asian", "aian")
PR <- c("white", "Black", "Hispanic", "Asian", "Am. Indian")
sc <- c(white = "pctwhite", black = "pctblack", hispanic = "pcthispanic",
        asian = "pctapi",   aian  = "pctaian")

i        <- match(v0$surname, sn$name)
matched  <- !is.na(i)
p_match  <- 100 * mean(matched)
drop     <- v0[!matched, ]
mix_all  <- 100 * prop.table(table(factor(v0$race,   levels = R)))
mix_drop <- 100 * prop.table(table(factor(drop$race, levels = R)))

v <- v0[matched, ]
S <- as.matrix(sn[i[matched], sc]); S[is.na(S)] <- 0
keep <- rowSums(S) > 0; v <- v[keep, ]; S <- S[keep, , drop = FALSE]
S <- S / rowSums(S)                                     # P(race | surname)

P <- as.matrix(g[, R]); rownames(P) <- g$GEOID20
M <- sweep(P, 2, colSums(P), "/")[v$GEOID20, , drop = FALSE]
empty <- rowSums(M) == 0
M[empty, ] <- matrix(colSums(P) / sum(P), sum(empty), 5, byrow = TRUE)
post <- S * M
degen <- rowSums(post) == 0
post[degen, ] <- S[degen, ]
post <- post / rowSums(post)

sur <- R[max.col(S,    ties.method = "first")]
bis <- R[max.col(post, ties.method = "first")]

acc <- function(pred) sapply(R, function(r) 100 * mean(pred[v$race == r] == r))
a_sur <- acc(sur); a_bis <- acc(bis)
o_sur <- 100 * mean(sur == v$race); o_bis <- 100 * mean(bis == v$race)
base_white <- 100 * mean(v$race == "white")
chg <- a_bis - a_sur

fallback <- empty | degen
fb_black <- sum(v$race[fallback] == "black")

tp   <- post[cbind(seq_len(nrow(post)), match(v$race, R))]
cali <- sapply(R, function(r) mean(tp[v$race == r]))

Mc  <- matrix(colSums(P) / sum(P), nrow(v), 5, byrow = TRUE)
b2  <- R[max.col(S * Mc, ties.method = "first")]
o_c <- 100 * mean(b2 == v$race); b_c <- 100 * mean(b2[v$race == "black"] == "black")

amb   <- apply(S, 1, max) < 0.6
a_amb <- c(surname = 100 * mean(sur[amb] == v$race[amb]),
           bisg    = 100 * mean(bis[amb] == v$race[amb]))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)

# The shape of the source files, and the counts the cleaning threw away. These
# used to live inside a chunk that printed all 53 column names as a table; the
# table went in the 3rd-edition cut, the numbers the prose quotes did not.
hdr <- scan("data/raw/voter-reg-header.txt", what = "", sep = ",", quiet = TRUE)
shp <- read.csv("data/raw/source-shape.csv", stringsAsFactors = FALSE)
SV  <- function(k) shp$value[shp$name == k]

# ---- figure data ----------------------------------------------------------
CLR <- c("#4d9221", "#C41230", "#8856a7", "#2c7fb8", "#e08214")
names(CLR) <- R
# The five groups as shared-library classes, so Figure 1 on screen carries the
# same hues the static twin prints: green, red, purple, blue, orange, and clay
# for the whole electorate. See _syllabus-template/brief.css, --dd-s1..s8.
SER <- list(white = "series-3", `Black` = "series-2", Hispanic = "series-5",
            Asian = "series-1", `Am. Indian` = "series-4",
            `ALL VOTERS` = "series-8")

# Figure 3 · the full confusion matrix
cm  <- table(factor(v$race, levels = R), factor(bis, levels = R))
cmp <- 100 * prop.table(cm, 1)
cm_worst <- which(cmp == max(cmp[row(cmp) != col(cmp)]), arr.ind = TRUE)[1, ]

# Figure 2 · the whole probability the model put on the truth, group by group
cbrk  <- seq(0, 1, by = 0.05)
chist <- lapply(R, function(r) {
  h <- hist(tp[v$race == r], breaks = cbrk, plot = FALSE)
  100 * h$counts / sum(h$counts)
})
names(chist) <- R
cmid  <- cbrk[-1] - 0.025
cmax  <- max(unlist(chist))

# Figure 1 · every accuracy against the floor available for free
bar9   <- c(a_bis, overall = o_bis)
bar9nm <- c(PR, "ALL VOTERS")
bar9n  <- c(as.integer(table(factor(v$race, levels = R))), nrow(v))

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- drop
data.frame(
  quantity = c("Voters in the file",
               "Whose surname appears in the Census surname list",
               "Whose surname does not",
               "Voters the method is scored on"),
  value = c(n(nrow(v0)), paste0(n(sum(matched)), " (", pc(p_match), "%)"),
            n(nrow(drop)), n(nrow(v))))

## ---- bisg
o <- data.frame(group = PR,
                n = n(as.integer(table(factor(v$race, levels = R)))),
                s = paste0(pc(a_sur), "%"), b = paste0(pc(a_bis), "%"),
                d = sprintf("%+.1f", chg))
names(o) <- c("the voter actually is", "how many", "surname only",
              "surname + block", "change")
o

## ---- conf-static
par(mar = c(1.2, 7.4, 4.4, 1.2))
K <- length(R)
plot(NA, xlim = c(0.5, K + 0.5), ylim = c(K + 0.5, 0.5), axes = FALSE,
     xlab = "", ylab = "", asp = 1)
ramp <- colorRampPalette(c("#ffffff", "#2c7fb8"))(101)
for (i in 1:K) for (j in 1:K) {
  val <- cmp[i, j]
  rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5,
       col = ramp[round(val) + 1], border = "white", lwd = 2)
  text(j, i, pc(val), cex = 0.78, col = if (val > 55) "white" else "#333",
       font = if (i == j) 2 else 1)
}
axis(3, at = 1:K, labels = PR, tick = FALSE, line = -0.6, cex.axis = 0.82)
axis(2, at = 1:K, labels = PR, tick = FALSE, las = 1, cex.axis = 0.82)
mtext("BISG guessed", side = 3, line = 2.2, cex = 0.92)
mtext("the voter actually is", side = 2, line = 5.6, cex = 0.92)

## ---- conf-d3
rows <- paste(apply(expand.grid(i = seq_along(R), j = seq_along(R)), 1,
  function(k) sprintf('{"a":"%s","g":"%s","p":%.1f,"n":%d}',
                      PR[k[["i"]]], PR[k[["j"]]], cmp[k[["i"]], k[["j"]]],
                      cm[k[["i"]], k[["j"]]])), collapse = ",")
cat(sprintf('
<div id="cfm" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], L=[%s];
const W=740,H=430,M={t:64,r:24,b:26,l:130};
const box=d3.select("#cfm");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(L).range([M.l,W-M.r]).padding(0.045);
const y=d3.scaleBand().domain(L).range([M.t,H-M.b]).padding(0.045);
const c=d3.scaleLinear().domain([0,100]).range(["#ffffff","#2c7fb8"]);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.g)).attr("y",d=>y(d.a)).attr("width",x.bandwidth())
  .attr("height",y.bandwidth()).attr("fill",d=>c(d.p))
  .attr("stroke",d=>d.a===d.g?"#222":"#fff").attr("stroke-width",d=>d.a===d.g?2:1.5)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.a} voters guessed ${d.g}</b><br>${d.p.toFixed(1)}%% of them<br>`+
     `${d3.format(",")(d.n)} people`)
     .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
// on-mark: the number sits inside its confusion-matrix cell and is coloured
// against the cell, not the page. The row and column labels below use the same
// #333 on the page and do want to follow the theme.
svg.append("g").selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(d.g)+x.bandwidth()/2).attr("y",d=>y(d.a)+y.bandwidth()/2+4)
  .attr("text-anchor","middle").attr("font-size","12px").attr("class","on-mark")
  .attr("font-weight",d=>d.a===d.g?"700":"400")
  .attr("fill",d=>d.p>55?"#fff":"#333").text(d=>d.p.toFixed(1)+"%%");
L.forEach(l=>{
  svg.append("text").attr("x",x(l)+x.bandwidth()/2).attr("y",M.t-10)
    .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#333").text(l);
  svg.append("text").attr("x",M.l-10).attr("y",y(l)+y.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","12px").attr("fill","#333").text(l);});
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",22).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("fill","#444").text("BISG guessed");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+H-M.b)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12.5px").attr("fill","#444")
  .text("the voter actually is");
})();
</script>
', rows, paste(sprintf('"%s"', PR), collapse = ",")))

## ---- calibration
o <- data.frame(group = PR, p = pc(cali, 2), acc = paste0(pc(a_bis), "%"))
names(o) <- c("the voter actually is",
              "mean probability BISG placed on the truth",
              "how often BISG's top guess was right")
o

## ---- cali-static
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 5), mar = c(3.6, 1.0, 2.4, 0.4), oma = c(1.6, 3.4, 0.4, 0.4))
for (k in seq_along(R)) {
  r <- R[k]
  plot(NA, xlim = c(0, 1), ylim = c(0, cmax * 1.06), axes = FALSE,
       xlab = "", ylab = "")
  rect(cbrk[-length(cbrk)], 0, cbrk[-1], chist[[r]], col = CLR[[r]],
       border = "white", lwd = 0.7)
  abline(v = cali[[r]], lwd = 2, col = "#222")
  axis(1, at = c(0, 0.5, 1), labels = c("0", ".5", "1"), cex.axis = 0.95)
  if (k == 1) axis(2, las = 1, cex.axis = 0.95)
  mtext(PR[k], side = 3, line = 0.7, cex = 0.78)
  mtext(paste0("mean ", pc(cali[[r]], 2)), side = 3, line = -0.2, cex = 0.66,
        col = "#555")
}
mtext("probability BISG placed on the truth", side = 1, outer = TRUE,
      line = 0.3, cex = 0.8)
mtext("% of the group's voters", side = 2, outer = TRUE, line = 1.9, cex = 0.8)
par(op)

## ---- cali-d3
ser <- paste(mapply(function(r, pr) sprintf('{"g":"%s","m":%.4f,"h":[%s]}',
    pr, cali[[r]], paste(sprintf("%.2f", chist[[r]]), collapse = ",")),
    R, PR), collapse = ",")
cat(sprintf('
<div id="cal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MX=%.2f;
const CL={"white":"#4d9221","Black":"#C41230","Hispanic":"#8856a7",
          "Asian":"#2c7fb8","Am. Indian":"#e08214"};
const W=760,H=250,M={t:34,r:14,b:52,l:44},GAP=14;
const box=d3.select("#cal");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const pw=(W-M.l-M.r-GAP*(D.length-1))/D.length;
const y=d3.scaleLinear().domain([0,MX*1.06]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(4).tickFormat(d=>d+"%%"));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach((s,k)=>{
  const ox=M.l+k*(pw+GAP);
  const x=d3.scaleLinear().domain([0,1]).range([ox,ox+pw]);
  const g=svg.append("g");
  g.selectAll("rect").data(s.h).join("rect")
    .attr("x",(d,i)=>x(i*0.05)+0.4).attr("width",x(0.05)-x(0)-0.8)
    .attr("fill",CL[s.g]).attr("y",y(0)).attr("height",0)
    .on("mousemove",function(e,d,i){tip.style("opacity",1).html(
       `<b>${s.g}</b><br>${d.toFixed(1)}%% of them`)
       .style("left",Math.min(e.offsetX+14,W-220)+"px").style("top",(e.offsetY-10)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0))
    .transition().duration(600).delay((d,i)=>i*12)
    .attr("y",d=>y(d)).attr("height",d=>y(0)-y(d));
  g.append("g").attr("transform",`translate(0,${H-M.b})`)
    .call(d3.axisBottom(x).tickValues([0,0.5,1]).tickFormat(d=>d===0?"0":(d===1?"1":".5")));
  g.append("line").attr("x1",x(s.m)).attr("x2",x(s.m)).attr("y1",M.t).attr("y2",y(0))
    .attr("stroke","#222").attr("stroke-width",2);
  g.append("text").attr("x",ox+pw/2).attr("y",M.t-16).attr("text-anchor","middle")
    .attr("font-size","12px").attr("fill","#222").text(s.g);
  g.append("text").attr("x",ox+pw/2).attr("y",M.t-4).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#666").text("mean "+s.m.toFixed(2));
});
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("probability BISG placed on the truth");
})();
</script>
', ser, cmax))

## ---- floor-static
par(mar = c(4.6, 4.8, 1.2, 1.4))
bp <- barplot(bar9, names.arg = bar9nm, las = 1, ylim = c(0, 100),
              col = c(CLR, "#555"), border = NA, cex.names = 0.84,
              ylab = "BISG identifies this group correctly (%)")
abline(h = base_white, lty = 2, lwd = 2, col = "#222")
text(bp[1], base_white + 4.5, paste0("guess \"white\" every time: ",
     pc(base_white), "%"), adj = c(0, 0), cex = 0.78, col = "#222")
text(bp, bar9 + 3.2, paste0(pc(bar9), "%"), cex = 0.8)
box(bty = "l")

## ---- floor-d3
# Drawn with the shared library (_lib/dd-charts.js), as the house convention asks: a
# plain categorical bar chart with one reference line is exactly what dd_fig()
# is for, and this is the document's FIRST html figure, so dd_fig() emits the
# d3 and dd-charts.js tags that the two hand-written figures below ride on.
# The classes carry the same five hues the static twin prints.
fb <- data.frame(g = bar9nm, v = as.numeric(bar9), n = bar9n,
                 stringsAsFactors = FALSE)
dd_fig("flr", "bar", fb,
  height = 380,
  x = list(field = "g"),
  y = list(field = "v", label = "BISG identifies this group correctly",
           domain = c(0, 100), fmt = "pct0", ticks = 6),
  series = list(field = "g", classes = SER),
  valueLabels = TRUE,
  annotations = list(
    dd_annot_hline(base_white, class = "zero"),
    dd_annot_text(bar9nm[1], base_white + 4, sprintf(
      "guess “white” every time: %s%%", pc(base_white)), size = 12)),
  tip = dd_tip(c(v = "identified correctly", n = "voters"),
               fmt = c(v = "pct1", n = "comma"), title = "g"))
cat(sprintf('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The dashed line is free: it is what a method with no method scores. %d of the
five groups sit below it, and so would the whole electorate if it were not
%.1f%% white. Hover a bar for the group size.</p>
', sum(a_bis < base_white), base_white))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
#cfm text[fill="#fff" i],
#cfm text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#cfm text[fill="#fff" i],
#cfm text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
