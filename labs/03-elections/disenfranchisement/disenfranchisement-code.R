# disenfranchisement-code.R -- chunk bodies for disenfranchisement-brief.Rmd
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
st <- read.csv("data/derived/states.csv",   stringsAsFactors = FALSE)
na <- read.csv("data/derived/national.csv", stringsAsFactors = FALSE)
gd <- read.csv("data/derived/grid.csv",     stringsAsFactors = FALSE)
sv <- function(s, v) st[[v]][st$state == s]
pc <- function(x, k = 2) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# the state at the top of the table and the states at exactly zero, named once
# here so the opening paragraph and every later mention cannot disagree
TOP  <- st$state[which.max(st$pct)]
ZERO <- st$state[st$total == 0]
NOTINC <- na$total - na$prison - na$jail

# --- the five categories as one population, drawn one square at a time ------
CAT  <- c("Prison", "Jail", "Parole", "Probation", "Post-sentence")
CVAL <- c(na$prison, na$jail, na$parole, na$probation, na$post_sentence)
names(CVAL) <- CAT
CCOL <- c(Prison = "#54278F", Jail = "#807dba", Parole = "#2c7fb8",
          Probation = "#a6cee3", `Post-sentence` = "#C41230")
PER  <- 25000                       # people per square
NSQ  <- round(na$total / PER)
NR   <- 6                           # rows; squares fill column by column
cum  <- cumsum(CVAL)
sqcat <- CAT[pmin(findInterval(seq_len(NSQ) * PER - PER / 2, cum) + 1L, 5L)]
# the two cut points, in column units, at their exact fractional position
CUT_INC <- (na$prison + na$jail) / PER / NR
CUT_END <- (na$total - na$post_sentence) / PER / NR

# --- the ten states that hold the whole post-sentence category --------------
ps <- st[st$post_sentence > 0, ]
ps <- ps[order(-ps$post_sentence), ]
ps$rest  <- ps$total - ps$post_sentence
ps$share <- 100 * ps$post_sentence / ps$total
PSCOV <- 100 * sum(ps$post_sentence) / na$post_sentence

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

## ---- validate
st$parts <- st$prison + st$parole + st$probation + st$jail + st$post_sentence
st$diff  <- st$parts - st$total
data.frame(
  check = c("The test", "States summing exactly",
            "States off by exactly 1 person", "States off by more than 1",
            "Largest discrepancy anywhere"),
  result = c("prison + parole + probation + jail + post-sentence = total",
             sum(st$diff == 0), sum(abs(st$diff) == 1),
             sum(abs(st$diff) > 1), n(max(abs(st$diff)))))

## ---- one-record
o <- st[st$state %in% c("Florida", "Tennessee", "Pennsylvania"),
        c("state", "prison", "jail", "parole", "probation", "post_sentence",
          "total", "pct")]
for (c_ in c("prison","jail","parole","probation","post_sentence","total")) o[[c_]] <- n(o[[c_]])
names(o) <- c("state", "prison", "jail", "parole", "probation",
              "post-sentence", "total", "% of adult citizens")
o

## ---- national
data.frame(
  quantity = c("Total disenfranchised", "Voting-eligible population",
               "Share of adult citizens", "Currently incarcerated",
               "Not incarcerated"),
  value = c(n(na$total), n(na$voting_eligible_pop), paste0(pc(na$pct, 1), "%"),
            paste0(n(na$prison + na$jail), "  (",
                   pc(100*(na$prison+na$jail)/na$total, 1), "%)"),
            paste0(n(NOTINC), "  (", pc(100*NOTINC/na$total, 1), "%)")))

## ---- categories
o <- data.frame(
  category = c("Prison", "Jail", "Parole", "Probation", "Post-sentence"),
  people = c(na$prison, na$jail, na$parole, na$probation, na$post_sentence))
o$share <- pc(100 * o$people / na$total, 1)
o$people <- n(o$people)
o$incarcerated <- c("yes", "yes", "no", "no", "no")
o$`sentence complete` <- c("no", "no", "no", "no", "yes")
o

## ---- waffle-static
i  <- seq_len(NSQ) - 1L
cx <- i %/% NR; cy <- i %% NR
NCOL_W <- NSQ / NR
par(mar = c(2.4, 0.4, 0.4, 0.4))
plot(NA, xlim = c(-0.4, NCOL_W + 0.4), ylim = c(NR + 2.0, -2.4), asp = 1,
     axes = FALSE, xlab = "", ylab = "")
rect(cx + 0.06, cy + 0.06, cx + 0.94, cy + 0.94, col = CCOL[sqcat],
     border = NA)
cuts <- list(list(CUT_INC, -2.0, "not in prison or jail", NOTINC),
             list(CUT_END, -1.0, "sentence already served", na$post_sentence))
for (q in cuts) {
  segments(q[[1]], q[[2]] + 0.2, q[[1]], NR + 0.1, col = "#333333", lty = 3)
  arrows(q[[1]], q[[2]] + 0.2, NCOL_W, q[[2]] + 0.2, length = 0.05,
         col = "#333333")
  text(q[[1]] + 0.2, q[[2]] - 0.25,
       paste0(q[[3]], ": ", n(q[[4]]), "  (", pc(100 * q[[4]] / na$total, 0),
              "%)"), pos = 4, cex = 0.64, col = "#333333")
}
legend(x = -0.4, y = NR + 2.2, horiz = TRUE, bty = "n", cex = 0.58, pch = 15,
       col = CCOL[CAT], legend = paste0(CAT, " ", n(CVAL)), xpd = NA)
mtext(paste0("One square is ", n(PER), " people; ", n(NSQ), " squares are ",
             n(na$total), ". Squares fill down each column, then rightward."),
      side = 1, line = 1.1, cex = 0.62, col = "#666666")

## ---- waffle-d3
# ---------------------------------------------------------------------------
# The same 162 squares in the same order as the base-R version below, from the
# same `sqcat` vector; the two cut positions and every label are computed and
# formatted in R and passed through as numbers and strings.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
cat(paste0('
<div id="waf" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const CAT=', paste0("[", paste(sprintf('"%s"', CAT), collapse = ","), "]"), ';
const COL=', paste0("[", paste(sprintf('"%s"', CCOL[CAT]), collapse = ","), "]"), ';
const LAB=', paste0("[", paste(sprintf('"%s"', paste0(CAT, " ", n(CVAL))),
                               collapse = ","), "]"), ';
const SQ=', paste0("[", paste(match(sqcat, CAT) - 1, collapse = ","), "]"), ';
const NR=', NR, ',S=22,P=2.4,NC=Math.ceil(SQ.length/NR);
const W=NC*S+30,H=NR*S+96;
const svg=d3.select("#waf").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=d3.select("#waf").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover a square.</b> Each one is ', n(PER),
  ' people. Everything right of the second dotted line has finished its sentence.";
svg.append("g").selectAll("rect").data(SQ).join("rect")
  .attr("x",(d,i)=>15+Math.floor(i/NR)*S).attr("y",(d,i)=>14+(i%NR)*S)
  .attr("width",S-P).attr("height",S-P).attr("fill",d=>COL[d]).attr("rx",1.5)
  .attr("opacity",0).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+CAT[d]+"</b> \\u2014 ', n(PER),
    ' of the "+LAB[d].replace(CAT[d]+" ","")+" people in this category."))
  .on("mouseleave",()=>cap.html(DEF))
  .transition().delay((d,i)=>i*4).duration(200).attr("opacity",1);
[[', sprintf("%.4f", CUT_INC), ',"not in prison or jail: ', n(NOTINC), '  (',
  pc(100 * NOTINC / na$total, 0), '%)"],
 [', sprintf("%.4f", CUT_END), ',"sentence already served: ', n(na$post_sentence),
  '  (', pc(100 * na$post_sentence / na$total, 0), '%)"]].forEach((q,k)=>{
  const X=15+q[0]*S;
  svg.append("line").attr("x1",X).attr("x2",X).attr("y1",8).attr("y2",NR*S+22)
    .attr("stroke","#333").attr("stroke-dasharray","3,3");
  svg.append("text").attr("x",X+6).attr("y",NR*S+34+k*15).attr("font-size","11.5px")
    .attr("fill","#333").text("\\u2192 "+q[1]);});
const lg=svg.append("g").attr("transform",`translate(15,${H-6})`);
LAB.forEach((t,i)=>{
  lg.append("rect").attr("x",i*126).attr("y",-10).attr("width",11).attr("height",11)
    .attr("fill",COL[i]);
  lg.append("text").attr("x",i*126+16).attr("y",0).attr("font-size","11px")
    .attr("fill","#333").text(t);});
cap.html(DEF);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One square is ', n(PER), ' people; ', n(NSQ), ' squares are ', n(na$total),
'. Squares fill down each column, then rightward, so the dotted lines fall at
the exact share.</p>
'))

## ---- post-states
p <- st[st$post_sentence > 0, c("state", "post_sentence", "total")]
p$share_of_state <- pc(100 * p$post_sentence / p$total, 1)
p <- p[order(-p$post_sentence), ]
p$post_sentence <- n(p$post_sentence); p$total <- n(p$total)
names(p) <- c("state", "post-sentence", "state total", "% of state's total")
p

## ---- extremes
o <- rbind(head(st[order(-st$pct), c("state", "total", "pct")], 6),
           head(st[order(st$pct),  c("state", "total", "pct")], 4))
o$total <- n(o$total)
names(o) <- c("state", "disenfranchised", "% of adult citizens")
o

## ---- cartogram-static
m <- merge(gd, st, by = "state")
par(mar = c(3.6, 0.4, 0.4, 0.4))
plot(NA, xlim = c(-0.5, max(m$col) + 1), ylim = c(max(m$row) + 1, -0.5),
     axes = FALSE, xlab = "", ylab = "")
# six classes, and the same six the HTML version uses: zero is its own class
br  <- c(-1e-9, 1e-3, 0.5, 1, 2, 3, 100)
pal <- c("#f7f7f7", "#fde7dc", "#fddbc7", "#f4a582", "#d6604d", "#8c1919")
cl  <- pal[as.integer(cut(m$pct, br))]
rect(m$col, m$row, m$col + .92, m$row + .92, col = cl,
     border = ifelse(m$pct == 0, "#bbbbbb", "white"))
text(m$col + .46, m$row + .46, m$abbr, cex = .62,
     col = ifelse(m$pct > 2, "white", "black"))
legend("bottom", horiz = TRUE, inset = c(0, -0.06), xpd = NA, bty = "n",
       cex = .66, fill = pal,
       legend = c("0%", "<0.5", "0.5-1", "1-2", "2-3", ">3"))
mtext(paste0("Share of adult citizens barred by a felony conviction. The ",
             sum(st$total == 0), " white tiles are the states at exactly zero: ",
             paste(ZERO, collapse = " and "), "."),
      side = 1, line = 2.4, cex = 0.62, col = "#666666")

## ---- d3-cartogram
m <- merge(gd, st, by = "state")
rows <- paste(sprintf('{"s":"%s","a":"%s","c":%d,"r":%d,"p":%.2f,"t":%d,"ps":%d}',
                      m$state, m$abbr, m$col, m$row, m$pct, round(m$total),
                      round(m$post_sentence)), collapse = ",")
cat(sprintf('
<div id="dis" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const CW=58,W=12*CW+40,H=9*CW+90;
const svg=d3.select("#dis").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const col=d3.scaleThreshold().domain([0.001,0.5,1,2,3])
  .range(["#f7f7f7","#fde7dc","#fddbc7","#f4a582","#d6604d","#8c1919"]);
const tip=d3.select("#dis").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:8px 11px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g").attr("transform","translate(14,14)");
const cell=g.selectAll("g").data(D).join("g")
  .attr("transform",d=>`translate(${d.c*CW},${d.r*CW})`);
cell.append("rect").attr("width",CW-5).attr("height",CW-5).attr("rx",3)
  .attr("fill",d=>col(d.p)).attr("stroke","#fff").attr("stroke-width",2);
cell.append("text").attr("x",(CW-5)/2).attr("y",(CW-5)/2-2).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600")
  .attr("fill",d=>d.p>2?"#fff":"#333").text(d=>d.a);
cell.append("text").attr("x",(CW-5)/2).attr("y",(CW-5)/2+13).attr("text-anchor","middle")
  .attr("font-size","10px").attr("fill",d=>d.p>2?"#fff":"#666").text(d=>d.p.toFixed(1)+"%%");
cell.on("mousemove",function(e,d){
    d3.select(this).select("rect").attr("stroke","#111");
    tip.style("opacity",1).html(
      `<b>${d.s}</b><br>${d3.format(",")(d.t)} disenfranchised<br>`+
      `${d.p}%% of adult citizens<br>`+
      (d.ps>0?`${d3.format(",")(d.ps)} have completed their sentence`
             :`none post-sentence`))
      .style("left",Math.min(d.c*CW+30,W-260)+"px").style("top",(d.r*CW+40)+"px");
  })
  .on("mouseleave",function(){d3.select(this).select("rect").attr("stroke","#fff");
    tip.style("opacity",0);});
const lg=svg.append("g").attr("transform",`translate(16,${H-34})`);
[["#f7f7f7","0%%"],["#fde7dc","<0.5"],["#fddbc7","0.5-1"],["#f4a582","1-2"],
 ["#d6604d","2-3"],["#8c1919",">3"]].forEach((r,i)=>{
  lg.append("rect").attr("x",i*72).attr("width",16).attr("height",13).attr("fill",r[0])
    .attr("stroke","#ccc");
  lg.append("text").attr("x",i*72+21).attr("y",11).attr("font-size","11.5px").text(r[1]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One tile per state, laid out roughly geographically. Hover for counts and the
post-sentence figure.</p>
', rows))

## ---- denom
data.frame(
  quantity = c("National voting-eligible population used here",
               "Where it comes from",
               "Is the disenfranchised count subtracted from it?"),
  value = c(n(na$voting_eligible_pop),
            "Estimated adult citizen population",
            "No"))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#dis text[fill="#333" i],
#dis text[fill="#666" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: the on-mark chunk above pins this fill dark for the dark
# page, so a --paper stroke there would sit dark behind a dark ink, and the
# checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#dis text[fill="#666" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
