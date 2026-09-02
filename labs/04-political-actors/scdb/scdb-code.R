# scdb-code.R -- chunk bodies for scdb-brief.Rmd
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

ag <- read.csv("data/derived/agreement.csv", stringsAsFactors = FALSE)
jj <- read.csv("data/derived/justices.csv",  stringsAsFactors = FALSE)
bt <- read.csv("data/derived/by_term.csv",   stringsAsFactors = FALSE)

cc <- read.csv("data/derived/close_cases.csv", stringsAsFactors = FALSE)

js   <- sort(unique(c(ag$a, ag$b)))
NJ   <- length(js); NPAIR <- nrow(ag)
HOLE <- ag[ag$cases == 0, ]

# --- the scaling, with the hole filled -------------------------------------
mat <- function(a) {
  m <- matrix(NA_real_, NJ, NJ, dimnames = list(js, js)); diag(m) <- 1
  for (i in seq_len(nrow(a))) {
    if (a$cases[i] == 0) next
    r <- a$agree[i] / a$cases[i]
    m[a$a[i], a$b[i]] <- r; m[a$b[i], a$a[i]] <- r
  }
  m
}
M    <- mat(ag)
FILL <- mean(M[upper.tri(M)], na.rm = TRUE)
M[is.na(M)] <- FILL
score <- cmdscale(as.dist(1 - M), k = 1)[, 1]
ord   <- sort(score)
R <- cor(score[jj$justice], jj$pct_conservative)

# --- the scaling, refusing to guess ----------------------------------------
OUT <- c(HOLE$a, HOLE$b)
a2  <- ag[!(ag$a %in% OUT | ag$b %in% OUT), ]
js2 <- sort(unique(c(a2$a, a2$b)))
M2  <- matrix(NA_real_, length(js2), length(js2), dimnames = list(js2, js2))
diag(M2) <- 1
for (i in seq_len(nrow(a2))) {
  r <- a2$agree[i] / a2$cases[i]
  M2[a2$a[i], a2$b[i]] <- r; M2[a2$b[i], a2$a[i]] <- r
}
score2 <- cmdscale(as.dist(1 - M2), k = 1)[, 1]
ord2   <- sort(score2)
R2 <- cor(score2[js2], jj$pct_conservative[match(js2, jj$justice)])

# which adjacent pairs swapped
keep  <- names(ord)[names(ord) %in% js2]
swaps <- which(keep != names(ord2))

# blocs
CONS  <- names(ord)[1:6]; LIBS  <- names(ord)[7:NJ]
SPAN  <- ord[6] - ord[1]
CHASM <- ord[7] - ord[6]

nm <- function(x, k = 3) formatC(x, format = "f", digits = k)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
nice <- function(x) {                        # ACBarrett -> Barrett
  sub("^[A-Z]+(?=[A-Z][a-z])", "", x, perl = TRUE)
}

# One case read through the coding rule: Borden, a 5-4 criminal-procedure
# decision for the accused, so each majority vote is coded liberal.
BOR     <- cc[grepl("^BORDEN v. UNITED STATES", cc$caseName), ][1, ]
BOR_MAJ <- nice(strsplit(BOR$majority_bloc, " ")[[1]])

# Three justices for the walk from agreement to distance: the pair that agrees
# most, and one of them against a justice from the other bloc.
agp <- function(x, y) ag$pct[(ag$a == x & ag$b == y) | (ag$a == y & ag$b == x)]
PA  <- ag$a[which.max(ag$pct)]; PB <- ag$b[which.max(ag$pct)]
PC  <- "SSotomayor"

# describe the swaps as adjacent pairs rather than as loose positions
swap_txt <- local({
  out <- character(0); i <- 1
  while (i < length(keep)) {
    if (keep[i] != names(ord2)[i] && keep[i] == names(ord2)[i + 1]) {
      out <- c(out, paste(nice(keep[i]), "and", nice(keep[i + 1]))); i <- i + 2
    } else i <- i + 1
  }
  out
})

# the two runs side by side, for Figure 2 and its static twin: one row per
# justice who appears in both, in the order the filled scaling put them
SW <- data.frame(justice = nice(keep),
                 filled  = as.vector(ord[keep]),
                 refused = as.vector(ord2[keep]),
                 stringsAsFactors = FALSE)
SW$moved <- ifelse(keep == names(ord2), "stayed", "swapped")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- justices
o <- jj[order(-jj$pct_conservative),
        c("justice", "cases", "pct_conservative", "pct_in_majority")]
o$justice <- nice(o$justice)
names(o) <- c("justice", "cases with a coded direction",
              "% of votes coded conservative", "% of the time in the majority")
o

## ---- agreement
o <- rbind(head(ag[order(-ag$pct), ], 3), head(ag[order(ag$pct), ], 3))
o <- o[, c("a", "b", "agree", "cases", "pct")]
o$a <- nice(o$a); o$b <- nice(o$b)
names(o) <- c("justice", "justice", "voted the same way", "shared cases",
              "% agreement")
o

## ---- heat-prep
RAW  <- mat(ag)                    # NA where the pair never sat together
LO   <- min(RAW[upper.tri(RAW)], na.rm = TRUE)
HI   <- max(RAW[upper.tri(RAW)], na.rm = TRUE)
SHADE <- colorRampPalette(c("#f7f7f7", "#252525"))(100)
gcol  <- function(v) SHADE[pmax(1, pmin(100, round(1 + 99 * (v - LO) / (HI - LO))))]
FILE_ORD <- js                     # the order the file happens to be in
SCAL_ORD <- names(ord)             # the order the scaling below will produce
HOLEA <- HOLE$a; HOLEB <- HOLE$b

## ---- heat-d3
# ---------------------------------------------------------------------------
# A DESIGNATED SHOWPIECE. The shared library has no matrix type, and the whole
# argument of this figure is that the same 45 numbers re-sorted look like a
# finding -- which needs the toggle. Colors are computed in R by gcol() and
# passed through as hex, so the browser and the PDF device shade the identical
# cell identically.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the dd_fig() figure below is emitted with
# d3 = FALSE for that reason.
# ---------------------------------------------------------------------------
cells <- character(0)
for (i in seq_len(NJ)) for (k in seq_len(NJ)) {
  v <- RAW[FILE_ORD[i], FILE_ORD[k]]
  cells <- c(cells, paste0('{"a":"', nice(FILE_ORD[i]), '","b":"',
    nice(FILE_ORD[k]), '","v":', ifelse(is.na(v), "null",
      formatC(100 * v, format = "f", digits = 1)),
    ',"c":"', ifelse(is.na(v), "#ffffff", gcol(v)), '"}'))
}
key <- paste0('{"p":', formatC(seq(0, 1, length.out = 40), format = "f", digits = 3),
              ',"c":"', SHADE[round(seq(1, 100, length.out = 40))], '"}',
              collapse = ",")
cat(paste0('
<div id="hm" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="hO" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">the order the file is in</button>
  <button id="hS" style="font:12px inherit;padding:4px 10px;cursor:pointer">re-sorted by the recovered order</button>
 </div>
</div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const C=[', paste(cells, collapse = ","), '];
const KEY=[', key, '];
const A=[', paste0('"', nice(FILE_ORD), '"', collapse = ","), '];
const B=[', paste0('"', nice(SCAL_ORD), '"', collapse = ","), '];
const LO=', formatC(100 * LO, format = "f", digits = 1),
    ', HI=', formatC(100 * HI, format = "f", digits = 1), ';
const W=770,H=430,M={t:96,r:196,b:24,l:104};
const svg=d3.select("#hm").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const s=d3.scaleBand().range([M.l,W-M.r]).padding(0.06);
const sy=d3.scaleBand().range([M.t,H-M.b]).padding(0.06);
const g=svg.append("g"), gx=svg.append("g"), gy=svg.append("g");
const tip=d3.select("#hm").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
function draw(ord){
  s.domain(ord); sy.domain(ord);
  g.selectAll("rect").data(C,d=>d.a+"|"+d.b).join(
    e=>e.append("rect").attr("stroke","#fff"),u=>u,x=>x.remove())
   .attr("x",d=>s(d.a)).attr("y",d=>sy(d.b))
   .attr("width",s.bandwidth()).attr("height",sy.bandwidth())
   .attr("fill",d=>d.a===d.b?"#e8e8e8":d.c)
   .attr("stroke",d=>d.v===null?"#C41230":"#fff")
   .attr("stroke-width",d=>d.v===null?2:1)
   .on("mousemove",function(ev,d){ tip.style("opacity",1)
     .html(d.a===d.b?("<b>"+d.a+"</b>"):
       (d.a+" and "+d.b+"<br>"+(d.v===null?"never sat together":d.v+"% agreement")))
     .style("left",Math.min(ev.offsetX+14,W-210)+"px").style("top",(ev.offsetY-6)+"px");})
   .on("mouseleave",()=>tip.style("opacity",0));
  gx.selectAll("text").data(ord,d=>d).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#333"),u=>u,x=>x.remove())
   .attr("transform",d=>"translate("+(s(d)+s.bandwidth()/2+4)+","+(M.t-8)+") rotate(-55)")
   .text(d=>d);
  gy.selectAll("text").data(ord,d=>d).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#333").attr("text-anchor","end"),
    u=>u,x=>x.remove())
   .attr("x",M.l-8).attr("y",d=>sy(d)+sy.bandwidth()/2+4).text(d=>d);
}
draw(A);
d3.select("#hO").on("click",()=>draw(A));
d3.select("#hS").on("click",()=>draw(B));
const kx=W-M.r+42, kt=M.t+18, kh=200;
svg.selectAll("rect.k").data(KEY).join("rect").attr("class","k")
  .attr("x",kx).attr("y",(d,i)=>kt+kh-(i+1)*(kh/KEY.length))
  .attr("width",16).attr("height",kh/KEY.length+0.6).attr("fill",d=>d.c);
svg.append("text").attr("x",kx+22).attr("y",kt+9).attr("font-size","11px").text(HI+"% agreement");
svg.append("text").attr("x",kx+22).attr("y",kt+kh).attr("font-size","11px").text(LO+"%");
svg.append("rect").attr("x",kx).attr("y",kt+kh+22).attr("width",16).attr("height",16)
  .attr("fill","#fff").attr("stroke","#C41230").attr("stroke-width",2);
svg.append("text").attr("x",kx+22).attr("y",kt+kh+34).attr("font-size","11px")
  .attr("fill","#C41230").text("never sat together");
svg.append("rect").attr("x",kx).attr("y",kt+kh+48).attr("width",16).attr("height",16)
  .attr("fill","#e8e8e8").attr("stroke","#fff");
svg.append("text").attr("x",kx+22).attr("y",kt+kh+60).attr("font-size","11px")
  .attr("fill","#555").text("a justice with herself");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover any cell for the pair and the number. The second button re-sorts the same
', NPAIR, ' numbers into the order recovered further down.</p>'))

## ---- heat-static
panel <- function(o, main) {
  plot(NA, xlim = c(0.5, NJ + 0.5), ylim = c(NJ + 0.5, 0.5), axes = FALSE,
       xlab = "", ylab = "")
  mtext(main, side = 3, line = 3.4, cex = 0.8, font = 2)
  for (i in seq_len(NJ)) for (k in seq_len(NJ)) {
    v <- RAW[o[i], o[k]]
    rect(i - 0.5, k - 0.5, i + 0.5, k + 0.5,
         col = if (i == k) "#e8e8e8" else if (is.na(v)) "#ffffff" else gcol(v),
         border = if (is.na(v)) "#C41230" else "#ffffff",
         lwd = if (is.na(v)) 2 else 1)
  }
  axis(2, at = seq_len(NJ), labels = nice(o), las = 1, tick = FALSE,
       cex.axis = 0.58, line = -0.8)
  axis(3, at = seq_len(NJ), labels = nice(o), las = 2, tick = FALSE,
       cex.axis = 0.58, line = -0.8)
}
par(mfrow = c(1, 2), mar = c(2.6, 4.2, 5.4, 0.6))
panel(FILE_ORD, "the order the file is in")
panel(SCAL_ORD, "re-sorted by the recovered order")
par(mfrow = c(1, 1))
# legend: an absolute scale, drawn once, in the bottom margin
par(new = TRUE, mar = c(0.4, 4.2, 0, 0.6), fig = c(0, 1, 0, 0.11))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
for (i in 1:100) rect((i - 1) / 100 * 0.30, 0.45, i / 100 * 0.30, 0.85,
                      col = SHADE[i], border = NA)
text(0, 0.28, paste0(pc(100 * LO), "% agreement"), adj = 0, cex = 0.62)
text(0.30, 0.28, paste0(pc(100 * HI), "%"), adj = 1, cex = 0.62)
rect(0.42, 0.45, 0.46, 0.85, col = "white", border = "#C41230", lwd = 2)
text(0.48, 0.65, "never sat together", adj = 0, cex = 0.62, col = "#C41230")
rect(0.72, 0.45, 0.76, 0.85, col = "#e8e8e8", border = "white")
text(0.78, 0.65, "a justice with herself", adj = 0, cex = 0.62, col = "#555555")

## ---- scaling
o <- data.frame(justice = nice(names(ord)),
                position = nm(as.vector(ord)),
                stringsAsFactors = FALSE)
o

## ---- refuse
o <- data.frame(rank = seq_along(ord2),
                `with the gap filled` = nice(keep),
                `refusing to guess`   = nice(names(ord2)),
                changed = ifelse(keep == names(ord2), "", "swapped"),
                check.names = FALSE)
o

## ---- swap-d3
# One row per justice who appears in both runs, two positions joined by a rule.
# A dumbbell is the form the comparison actually has: the question is asked of
# each person separately, and the reader follows one name across two runs.
# NOT a length: cmdscale fixes neither sign nor scale, so the two runs are on
# scales that are only comparable in their ORDER, which is what the caption
# tells the reader to read. d3 = FALSE: the heat map above already loaded d3.
dd_fig("scdbswap", "dumbbell", SW, d3 = FALSE, rowHeight = 26,
       size = list(m = list(t = 26, r = 30, b = 58, l = 108)),
       y = list(field = "justice"),
       a = list(field = "filled",  label = "with the gap filled"),
       b = list(field = "refused", label = "refusing to guess"),
       aClass = "series-1", bClass = "series-2", r = 5,
       x = list(domain = c(-0.32, 0.34), fmt = "f2",
                label = "position recovered from agreement alone; the direction of this axis is arbitrary"),
       annotations = list(dd_annot_vline(0)),
       tip = dd_tip(c(filled = "with the gap filled",
                      refused = "refusing to guess",
                      moved = "when the guess went"),
                    fmt = c(filled = "f3", refused = "f3"),
                    title = "justice"))

## ---- swap-static
d <- SW
par(mar = c(5.0, 7.4, 1.2, 1.0))
plot(NA, xlim = c(-0.32, 0.34), ylim = c(nrow(d) + 0.5, 0.5), yaxt = "n",
     xlab = "", ylab = "", cex.axis = 0.8)
abline(v = 0, lty = 3, col = "grey70")
segments(d$filled, seq_len(nrow(d)), d$refused, seq_len(nrow(d)),
         col = "#BBBBBB", lwd = 2)
points(d$filled,  seq_len(nrow(d)), pch = 19, col = "#2c7fb8", cex = 1.1)
points(d$refused, seq_len(nrow(d)), pch = 19, col = "#C41230", cex = 1.1)
axis(2, at = seq_len(nrow(d)), labels = d$justice, las = 1, tick = FALSE,
     cex.axis = 0.75)
mtext("position recovered from agreement alone", side = 1, line = 2.4,
      cex = 0.85)
mtext("the direction of this axis is arbitrary: only order and spacing mean anything",
      side = 1, line = 3.5, cex = 0.66, col = "#777777")
legend("topright", c("with the gap filled", "refusing to guess"), pch = 19,
       col = c("#2c7fb8", "#C41230"), bty = "n", cex = 0.7)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
