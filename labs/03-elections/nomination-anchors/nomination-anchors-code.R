# nomination-anchors-code.R -- chunk bodies for nomination-anchors-brief.Rmd
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

# Everything is read from data/, written by data/build-data.R out of the five
# transcribed tables in data/raw/. Nothing here re-derives anything.
NM <- read.csv("data/derived/nominations.csv",   stringsAsFactors = FALSE)
BA <- read.csv("data/derived/by_anchor.csv",     stringsAsFactors = FALSE, check.names = FALSE)
BF <- read.csv("data/derived/by_faction.csv",    stringsAsFactors = FALSE)
PA <- read.csv("data/derived/pac_by_anchor.csv", stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",        stringsAsFactors = FALSE)

pc <- function(x, k = 0) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)
ck <- function(k) {
  v <- CK$value[CK$check == k]
  if (length(v) != 1L) stop("checks.csv has no single value for '", k, "'")
  v
}
fac <- function(f, col) BF[[col]][BF$faction == f]
pac <- function(f, col) PA[[col]][PA$anchor_form == f]

NR   <- nrow(NM)
NRES <- sum(NM$anchor_form_source == "left over: stated in no table and no sentence")
NERR <- sum(nzchar(NM$errata))
NDIS <- sum(NM$tables_disagree)
W1 <- NM[NM$race == "PA-13D", ]           # backing named in table 5.2
W2 <- NM[NM$race == "MI-12D", ]           # backing stated nowhere: left over
INS  <- "insurgent"; PBK <- "establishment (party-backed)"; BBK <- "establishment (business-backed)"

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
  quantity = c("Nominations studied", "Republican", "Democratic",
               "Interviews", "Tables in the book", "Tables that share a key"),
  value = c(NR, ck("Republican nominations"), ck("Democratic nominations"),
            "346", "23", "4"))

## ---- sources
data.frame(
  table = c("Table 10.1", "Table 5.2", "Tables 6.2 and 6.3", "Table 9.1"),
  `what it gives you` = c(
    "Every nomination: who won, which group backed them, and why that group cared",
    "The 32 contests where groups worked together behind one candidate",
    "How many winners each kind of backing produced -- as totals only",
    "For 33 Republicans: their faction, and business PAC money in dollars"),
  check.names = FALSE)

## ---- rebuild
data.frame(
  step = c("Expand the (1st and 2nd) annotations",
           "Move the two winners chapter 6 reassigns",
           "Take the lists chapter 6 prints in prose",
           "Whatever is left"),
  `derived here` = c(ck("of those, who won the nomination"),
                     "28", "19", NRES),
  `printed in the book` = c("30 (chapter 6)", "28 (table 6.3)",
                            "9 + 6 + 4 (table 6.3)", "8 (table 6.2)"),
  check.names = FALSE)

## ---- errata
E <- NM[nzchar(NM$errata), c("race", "candidate", "errata")]
names(E) <- c("race", "candidate", "what the book prints")
E

## ---- disagree
D <- NM[NM$tables_disagree, c("race", "candidate", "anchor", "anchor_form")]
names(D) <- c("race", "candidate", "table 10.1 says the backer was",
              "tables 6.2/6.3 say the backing was")
D

## ---- fig1-d3
# ---------------------------------------------------------------------------
# WHY EACH GROUP GOT INVOLVED, AGAINST WHAT IT SUPPLIED. Eight motives down,
# five kinds of backing across, counts in the cells. A grid rather than a bar
# chart because the interesting thing is which cells are EMPTY -- most of them
# are, and a bar chart would hide that behind totals.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figure uses the library loaded
# here.
# ---------------------------------------------------------------------------
FM   <- names(BA)[-1]
cols <- paste0('["', paste(FM, collapse = '","'), '"]')
rows <- paste(apply(BA, 1, function(r)
  sprintf('{"m":"%s","v":[%s]}', r[[1]], paste(as.integer(r[-1]), collapse = ","))),
  collapse = ",")
cat(paste0('
<div id="grid" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '],C=', cols, ';
const W=760,H=330,M={t:74,r:14,b:14,l:196};
const svg=d3.select("#grid").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(C).range([M.l,W-M.r]).padding(0.06);
const y=d3.scaleBand().domain(D.map(d=>d.m)).range([M.t,H-M.b]).padding(0.06);
const mx=d3.max(D,d=>d3.max(d.v));
const col=d3.scaleLinear().domain([0,mx]).range(["#f4f7fa","#2f5f8f"]);
C.forEach(c=>{
  svg.append("text").attr("transform","translate("+(x(c)+x.bandwidth()/2)+","+(M.t-8)+") rotate(-32)")
    .attr("font-size","11px").attr("fill","#333").text(c);
});
D.forEach(d=>{
  svg.append("text").attr("x",M.l-8).attr("y",y(d.m)+y.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","11.5px").text(d.m);
  d.v.forEach((v,i)=>{
    svg.append("rect").attr("x",x(C[i])).attr("y",y(d.m))
      .attr("width",x.bandwidth()).attr("height",y.bandwidth())
      .attr("fill",v===0?"#fbfbfb":col(v)).attr("stroke","#fff");
    if(v>0) svg.append("text").attr("x",x(C[i])+x.bandwidth()/2)
      .attr("y",y(d.m)+y.bandwidth()/2+4).attr("text-anchor","middle")
      .attr("font-size","12px").attr("font-weight","600")
      .attr("fill",v>mx*0.55?"#fff":"#123").text(v);
  });
});
})();
</script>'))

## ---- fig1-static
FM <- names(BA)[-1]
M  <- as.matrix(BA[, -1]); rownames(M) <- BA[[1]]
par(mar = c(0.4, 11.6, 6.4, 4.6))
plot(NA, xlim = c(0, length(FM)), ylim = c(nrow(M), 0), axes = FALSE, ann = FALSE)
mxv <- max(M)
for (i in seq_len(nrow(M))) for (j in seq_along(FM)) {
  v <- M[i, j]
  rect(j - 1, i - 1, j, i, border = "#ffffff",
       col = if (v == 0) "#fbfbfb" else
         rgb(colorRamp(c("#f4f7fa", "#2f5f8f"))(v / mxv) / 255))
  if (v > 0) text(j - 0.5, i - 0.5, v, cex = 0.62, font = 2,
                  col = if (v > mxv * 0.55) "#ffffff" else "#112233")
}
text(-0.15, seq_len(nrow(M)) - 0.5, rownames(M), adj = 1, cex = 0.56, xpd = NA)
text(seq_along(FM) - 0.35, -0.2, FM, adj = 0, cex = 0.52, srt = 32, xpd = NA)

## ---- fig2-d3
# ---------------------------------------------------------------------------
# COORDINATION BEHIND EACH KIND OF REPUBLICAN NOMINEE. Three rows, each a full
# bar of that faction's nominees, split into coordinated and not. Full bars
# rather than percentages because the groups are 12, 12 and 9 candidates, and a
# percentage on nine cases invites more confidence than nine cases can carry.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"f":"%s","n":%d,"c":%d}', BF$faction, BF$nominees, BF$coordinated),
              collapse = ",")
cat(paste0('
<div id="fac" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const W=760,H=230,M={t:20,r:150,b:34,l:210};
const svg=d3.select("#fac").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const mx=d3.max(D,d=>d.n);
const x=d3.scaleLinear().domain([0,mx]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.f)).range([M.t,H-M.b]).padding(0.3);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d3.format("d")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-4).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#444").text("nominees");
D.forEach(d=>{
  svg.append("text").attr("x",M.l-10).attr("y",y(d.f)+y.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","11.5px").text(d.f);
  svg.append("rect").attr("x",x(0)).attr("y",y(d.f)).attr("height",y.bandwidth())
    .attr("width",x(d.n)-x(0)).attr("fill","#dde5ed");
  svg.append("rect").attr("x",x(0)).attr("y",y(d.f)).attr("height",y.bandwidth())
    .attr("width",x(d.c)-x(0)).attr("fill","#C41230");
  svg.append("text").attr("x",W-M.r+8).attr("y",y(d.f)+y.bandwidth()/2+4)
    .attr("font-size","11.5px").attr("font-weight","600")
    .text(d.c+" of "+d.n+" coordinated");
});
const lg=d3.select("#fac").append("div").attr("style","margin-top:2px;font-size:12px;color:#444");
lg.html("<span style=\\"display:inline-block;width:11px;height:11px;background:#C41230;"+
 "margin-right:4px\\"></span>groups coordinated in this contest &nbsp;&nbsp;"+
 "<span style=\\"display:inline-block;width:11px;height:11px;background:#dde5ed;"+
 "margin-right:4px\\"></span>no coordination observed");
})();
</script>'))

## ---- fig2-static
# Bottom margin carries the axis, its label AND the legend. At 3.0 the legend
# was drawn below the device and silently clipped, so the PDF lost a key the
# HTML version had. Both renders must show the same figure.
par(mar = c(4.6, 12.4, 0.4, 6.4), mgp = c(1.9, 0.5, 0))
b <- barplot(BF$nominees, horiz = TRUE, col = "#dde5ed", border = NA,
             names.arg = BF$faction, las = 1, cex.names = 0.56,
             xlab = "nominees", cex.axis = 0.66, cex.lab = 0.7)
barplot(BF$coordinated, horiz = TRUE, col = "#C41230", border = NA,
        add = TRUE, axes = FALSE, names.arg = rep("", nrow(BF)))
text(max(BF$nominees) + 0.5, b, sprintf("%d of %d coordinated",
     BF$coordinated, BF$nominees), adj = 0, cex = 0.56, font = 2, xpd = NA)
legend(0, -0.9, c("groups coordinated in this contest", "no coordination observed"),
       fill = c("#C41230", "#dde5ed"), border = NA, horiz = TRUE, bty = "n",
       cex = 0.52, xpd = NA)

## ---- pacs
P <- PA[PA$anchor_form != "none or unclassified", ]
P$median_pac <- paste0("$", n(P$median_pac))
P$mean_pac   <- paste0("$", n(P$mean_pac))
names(P) <- c("what backed the winner", "Republican nominees",
              "median business PAC money", "mean")
P
