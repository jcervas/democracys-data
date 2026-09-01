# oral-argument-code.R -- chunk bodies for oral-argument-brief.Rmd
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

ar  <- read.csv("data/derived/arguments.csv", stringsAsFactors = FALSE)
me  <- read.csv("data/derived/measures.csv",  stringsAsFactors = FALSE)
si  <- read.csv("data/derived/silence.csv",   stringsAsFactors = FALSE)
od  <- read.csv("data/derived/order.csv",     stringsAsFactors = FALSE)
ck  <- read.csv("data/derived/checks.csv",    stringsAsFactors = FALSE)
tot <- read.csv("data/derived/totals.csv",    stringsAsFactors = FALSE)
TOT <- function(k) tot$value[tot$quantity == k]

NARG   <- nrow(ar)
NTERM  <- length(unique(ar$term))
T0     <- min(ar$term); T1 <- max(ar$term)
NTURN  <- TOT("turns_read")     # before the two exclusions

# --- per term ---------------------------------------------------------------
has <- ar[!is.na(ar$minutes), ]
agg <- function(f) tapply(seq_len(nrow(has)), has$term, function(i) f(has[i, ]))
bt <- data.frame(term = sort(unique(has$term)))
bt$minutes     <- as.vector(agg(function(d) mean(d$minutes)))
bt$jwords      <- as.vector(agg(function(d) mean(d$justice_words)))
bt$jwords_min  <- as.vector(agg(function(d) sum(d$justice_words) / sum(d$minutes)))

MIN0 <- bt$minutes[bt$term == T0]; MIN1 <- bt$minutes[bt$term == T1]
WRD0 <- bt$jwords[bt$term == T0];  WRD1 <- bt$jwords[bt$term == T1]
RAT0 <- bt$jwords_min[bt$term == T0]; RAT1 <- bt$jwords_min[bt$term == T1]

PRE  <- has[has$regime == "in person, no rounds", ]
POST <- has[has$regime != "in person, no rounds", ]
GROWTH <- 100 * (mean(POST$minutes) / mean(PRE$minutes) - 1)

# --- the six measures -------------------------------------------------------
MEAS  <- unique(me$measure)
NMEAS <- length(MEAS)
JS    <- unique(me$name[me$measure == MEAS[1]])
NJ    <- length(JS)
rk    <- function(j, m) me$rank[me$name == j & me$measure == m]
spread <- sapply(JS, function(j) {
  r <- sapply(MEAS, function(m) rk(j, m)); max(r) - min(r) })
MOVER  <- names(which.max(spread))
MOVE   <- max(spread)
NMOVED <- sum(spread > 0)
BEST   <- rk(MOVER, MEAS[which.min(sapply(MEAS, function(m) rk(MOVER, m)))])
WORST  <- rk(MOVER, MEAS[which.max(sapply(MEAS, function(m) rk(MOVER, m)))])
MOVER_HI <- MEAS[which.min(sapply(MEAS, function(m) rk(MOVER, m)))]
MOVER_LO <- MEAS[which.max(sapply(MEAS, function(m) rk(MOVER, m)))]

# who never moves, and who is always at an end
TOP3  <- names(which(sapply(JS, function(j)
           all(sapply(MEAS, function(m) rk(j, m)) <= 3))))
LAST  <- names(which(sapply(JS, function(j)
           all(sapply(MEAS, function(m) rk(j, m)) == NJ))))
FIRSTS <- unique(me$name[me$rank == 1])
RW <- c(me$term_from[1], me$term_to[1])   # the window the build ranked in

# --- the rule ---------------------------------------------------------------
TEL   <- od[od$regime == "telephone, seniority rounds", ]
# The one term argued by telephone, as a NUMBER. TEL is a data frame, and
# paste0() run over a data frame vectorises across its columns rather than
# failing, so interpolating TEL itself into a figure emits that figure once per
# column. It looked correct, because the first column is the term.
TELT  <- TEL$term
HYB   <- od[od$regime == "in person, seniority round added", ]
# 2019 is the term the rule landed in the middle of: argued in the courtroom
# until March 2020 and by telephone in May, so it belongs to neither side.
SPLIT <- 2019
PREO  <- od[od$regime == "in person, no rounds" & od$term < SPLIT, ]
PREO_MAX <- max(PREO$pct_in_seniority_order)
NZERO <- sum(PREO$pct_in_seniority_order == 0)
ORD <- function(t) od$pct_in_seniority_order[od$term == t]

TH <- si[si$justice == "clarence_thomas", ]
TH_PRE  <- TH[TH$term <= 2018, ]
TH_POST <- TH[TH$term >= 2020, ]
TH_SILENT_PRE <- sum(TH_PRE$arguments_silent)
TH_ON_PRE     <- sum(TH_PRE$arguments_on_bench)
TH_2020 <- TH$pct_silent[TH$term == 2020]
TH_TERMS_100 <- sum(TH_PRE$pct_silent == 100)

# --- what the published report says, for the comparison table ---------------
# These are READ OFF the Epstein-Posner report, not computed here. They are the
# only numbers in this chapter that this book did not produce, and the table
# that uses them labels every one.
EP <- data.frame(
  quantity = c("Arguments in the 2005-2025 terms",
               "Speaking turns, before the two exclusions",
               "Speaking turns, after them",
               "Justice words per argument, 2005 term",
               "Justice words per argument, 2025 term",
               "Arguments with at least one silent justice",
               "Longest argument, in minutes"),
  reported = c("1,425", "368,369", "353,278", "3,866", "5,802", "1,105", "193"),
  stringsAsFactors = FALSE)
QUIET <- TOT("arguments_with_a_silent_justice")
LONG  <- ar[which.max(ar$minutes), ]
EP$here <- c(format(NARG, big.mark = ","),
             format(TOT("turns_read"), big.mark = ","),
             format(TOT("turns_kept"), big.mark = ","),
             format(round(WRD0), big.mark = ","),
             format(round(WRD1), big.mark = ","),
             format(QUIET, big.mark = ","),
             format(round(LONG$minutes)))

nm <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
short <- function(s) sub("^.* ", "", sub(",.*", "", s))   # -> a surname
ord <- function(k) c("first", "second", "third", "fourth", "fifth", "sixth",
                     "seventh", "eighth", "ninth")[k]

knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

RED <- "#C41230"; BLU <- "#2c7fb8"; GRY <- "#8A8F94"

# --- the exhibit ------------------------------------------------------------
# Read out of raw/ on purpose. This chapter's claim is that the record is
# turn-level and timestamped, and a claim like that has to show a turn with the
# timestamps still attached. The first turn by a justice is the one printed,
# because the argument is about what the Court records of ITSELF.
ex <- read.csv("data/derived/exhibit.csv", stringsAsFactors = FALSE)
j <- jsonlite::fromJSON(file.path("data", ex$file), simplifyVector = FALSE)
allturns <- unlist(lapply(j$transcript$sections, function(s) s$turns),
                   recursive = FALSE)
is_justice <- function(t) {
  r <- t$speaker$roles
  !is.null(r) && any(vapply(r, function(z) identical(z$type, "scotus_justice"),
                            logical(1)))
}
# The FIRST justice turn is the Chief gavelling the argument in, which is the
# one kind of turn this chapter throws away. The exhibit is the first justice
# turn long enough to be a question.
nwords <- function(t) lengths(strsplit(gsub("\\s+", " ", trimws(paste(
  vapply(t$text_blocks, function(b) b$text, character(1)), collapse = " "))),
  " +"))
cand <- which(vapply(allturns, is_justice, logical(1)) &
              vapply(allturns, nwords, integer(1)) >= 25)
EXT <- allturns[[cand[1]]]
EXNAME <- EXT$speaker$name
EXROLE <- EXT$speaker$roles[[1]]$role_title
EXTXT  <- gsub("\\s+", " ", trimws(paste(
  vapply(EXT$text_blocks, function(b) b$text, character(1)), collapse = " ")))
EXCASE <- ex$case

## ---- rawturn
data.frame(
  field = c("case", "speaker", "role", "start", "stop", "seconds of floor",
            "words", "text"),
  value = c(paste0(EXCASE, " (", ex$docket, ", ", ex$term, " term)"),
            EXNAME, EXROLE,
            paste(EXT$start, "seconds in"),
            paste(EXT$stop, "seconds in"),
            nm(EXT$stop - EXT$start, 2),
            lengths(strsplit(EXTXT, " +")),
            paste0(substr(EXTXT, 1, 92), "...")),
  stringsAsFactors = FALSE)

## ---- measure-list
data.frame(
  measure = MEAS,
  what_it_counts = c(
    "how often a justice takes the floor",
    "how much a justice says in an argument",
    "how long a justice goes when they do speak",
    "how much of the clock a justice personally holds",
    "a justice's share of everything the justices said",
    "a justice's share of the whole argument, lawyers included"),
  stringsAsFactors = FALSE)

## ---- bump-d3
# ---------------------------------------------------------------------------
# A DESIGNATED SHOWPIECE. Nine justices, six measures, rank 1 at the top. Every
# line is one justice and holds its identity across all six columns, which is
# the entire point: a reader who follows one line watches the answer change
# under them. The shared library has no bump type, and the raising interaction
# is what makes nine overlapping lines readable at all.
#
# COLOUR. Nine series is one more than the number of categorical hues anybody
# can tell apart, so no justice gets a hue. All lines are grey and hovering or
# clicking one raises it in the book's red. Identity is carried by the label at
# both ends, never by colour alone, and the table below the figure holds every
# value the figure plots.
#
# This chunk carries the ONE d3 <script src> for the document; the dd_fig()
# figure below is emitted with d3 = FALSE for that reason.
# ---------------------------------------------------------------------------
rows <- paste0("{\"j\":\"", me$name, "\",\"m\":\"", me$measure,
               "\",\"v\":", me$value, ",\"r\":", me$rank, "}", collapse = ",")
cat(paste0('
<div id="bump" style="position:relative;margin:1.2em 0">
 <p style="font:12px inherit;color:#666;margin:0 0 6px">
  Hover a line to follow one justice. Click to keep them raised.</p>
</div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const M=[', paste0('"', MEAS, '"', collapse = ","), '];
const J=[', paste0('"', JS, '"', collapse = ","), '];
const W=790,H=430,P={t:74,r:184,b:30,l:184};
const svg=d3.select("#bump").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(M).range([P.l,W-P.r]);
const y=d3.scalePoint().domain(d3.range(1,J.length+1)).range([P.t,H-P.b]);
const RED="', RED, '", GRY="', GRY, '";

// column headers, wrapped by hand so a six-word label does not collide
M.forEach(function(m,i){
  const ws=m.split(" "), lines=[]; let cur="";
  ws.forEach(function(w){ if((cur+" "+w).trim().length>13){lines.push(cur);cur=w;}
                          else cur=(cur+" "+w).trim(); });
  lines.push(cur);
  lines.forEach(function(l,k){
    svg.append("text").attr("x",x(m)).attr("y",P.t-46+k*12)
      .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#444")
      .text(l);
  });
});
// A rank gutter on the far left, clear of the name labels. Putting "rank 1"
// beside the first name collided with it, which is why the numbers get their
// own column instead.
svg.selectAll("text.rk").data(d3.range(1,J.length+1)).join("text")
  .attr("class","rk").attr("x",36).attr("y",d=>y(d)+4).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#999").text(d=>d);
svg.append("text").attr("x",36).attr("y",P.t-16).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#999").text("rank");

const line=d3.line().x(d=>x(d.m)).y(d=>y(d.r));
const byJ={};
J.forEach(j=>byJ[j]=M.map(m=>D.find(d=>d.j===j&&d.m===m)));
const g=svg.append("g");
let pinned=null;

const paths=g.selectAll("path").data(J).join("path")
  .attr("d",j=>line(byJ[j])).attr("fill","none")
  .attr("stroke",GRY).attr("stroke-width",1.6).attr("opacity",0.55);
const dots=g.selectAll("g.d").data(D).join("g").attr("class","d");
dots.append("circle").attr("cx",d=>x(d.m)).attr("cy",d=>y(d.r)).attr("r",4)
  .attr("fill","#fff").attr("stroke",GRY).attr("stroke-width",1.6);

// a name at each end of every line, so identity never depends on colour
const ends=[["l",M[0],"end",-12],["r",M[M.length-1],"start",12]];
const labs=[];
ends.forEach(function(e){
  labs.push(svg.selectAll("text.lab"+e[0]).data(J).join("text")
    .attr("class","lab"+e[0])
    .attr("x",x(e[1])+e[3]).attr("y",j=>y(byJ[j].find(d=>d.m===e[1]).r)+4)
    .attr("text-anchor",e[2]).attr("font-size","11px").attr("fill","#555")
    .text(j=>j.replace(/,.*/,"").replace(/^.* /,"")));
});

const tip=d3.select("#bump").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");

function raise(j){
  paths.attr("stroke",p=>p===j?RED:GRY)
       .attr("stroke-width",p=>p===j?2.8:1.6)
       .attr("opacity",p=>j===null?0.55:(p===j?1:0.16));
  dots.select("circle").attr("stroke",d=>d.j===j?RED:GRY)
      .attr("opacity",d=>j===null?1:(d.j===j?1:0.16));
  labs.forEach(L=>L.attr("fill",p=>p===j?RED:"#555")
                   .attr("opacity",p=>j===null?1:(p===j?1:0.3)));
}
// generous hit areas: an invisible fat stroke over each thin line
g.selectAll("path.hit").data(J).join("path").attr("class","hit")
  .attr("d",j=>line(byJ[j])).attr("fill","none").attr("stroke","transparent")
  .attr("stroke-width",22).attr("cursor","pointer")
  .on("mousemove",function(ev,j){
     raise(j);
     const m=M.reduce((a,b)=>Math.abs(x(b)-ev.offsetX)<Math.abs(x(a)-ev.offsetX)?b:a);
     const d=byJ[j].find(q=>q.m===m);
     tip.style("opacity",1).html("<b>"+j+"</b><br>"+m+"<br>"+
        (+d.v).toFixed(1)+(m.indexOf("share")===0?"%":"")+" &middot; rank "+d.r)
        .style("left",Math.min(ev.offsetX+14,W-230)+"px")
        .style("top",(ev.offsetY-4)+"px");})
  .on("mouseleave",function(){ tip.style("opacity",0); raise(pinned); })
  .on("click",function(ev,j){ pinned=(pinned===j?null:j); raise(pinned); });
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Every line is one justice, holding the same identity across all ', NMEAS, '
columns. The table under the figure carries every value plotted here.</p>'))

## ---- bump-static
par(mar = c(1.0, 9.0, 5.6, 7.4))
plot(NA, xlim = c(1, NMEAS), ylim = c(NJ + 0.4, 0.6), axes = FALSE,
     xlab = "", ylab = "")
for (j in JS) {
  r <- sapply(MEAS, function(m) rk(j, m))
  hi <- j %in% c(TOP3, LAST, MOVER)
  lines(seq_len(NMEAS), r, col = if (hi) RED else GRY, lwd = if (hi) 2.4 else 1.4)
  points(seq_len(NMEAS), r, pch = 21, bg = "white",
         col = if (hi) RED else GRY, cex = 0.8)
  text(1 - 0.08, r[1], short(j), adj = 1, cex = 0.62,
       col = if (hi) RED else "#555555", xpd = NA)
  text(NMEAS + 0.08, r[NMEAS], short(j), adj = 0, cex = 0.62,
       col = if (hi) RED else "#555555", xpd = NA)
}
wrapped <- sapply(MEAS, function(m)
  paste(strwrap(m, width = 14), collapse = "\n"))
mtext(wrapped, side = 3, at = seq_len(NMEAS), line = 0.4, cex = 0.55)
# rank numbers get their own column out in the margin, clear of the names
axis(2, at = seq_len(NJ), labels = seq_len(NJ), las = 1, tick = FALSE,
     line = 5.4, cex.axis = 0.6, col.axis = "#999999")
mtext("rank", side = 3, at = 1, line = 0.4, adj = 1, cex = 0.55,
      col = "#999999", padj = 0, outer = FALSE, xpd = NA)

## ---- measure-table
o <- reshape(me[, c("name", "measure", "rank")], idvar = "name",
             timevar = "measure", direction = "wide")
names(o) <- c("justice", MEAS)
o <- o[order(o[[2]]), ]
o$justice <- short(o$justice)   # surnames, to match the labels in Figure 1
o

## ---- order-d3
# ---------------------------------------------------------------------------
# Two series, both a share of the arguments heard in a term, so one axis is not
# only allowed but required: they are the same unit and the comparison between
# them is the point. Drawn with the shared library; d3 = FALSE because the bump
# figure above already loaded d3.
# ---------------------------------------------------------------------------
th <- si[si$justice == "clarence_thomas", c("term", "pct_silent")]
mo <- merge(od[, c("term", "pct_in_seniority_order", "regime")], th, all.x = TRUE)
mo <- mo[order(mo$term), ]
dd_fig("ordfig", "line", mo, height = 360, d3 = FALSE,
  size = list(m = list(t = 44, r = 30, b = 42, l = 54)),
  x = list(field = "term", fmt = "d", ticks = 8),
  y = list(field = "pct_in_seniority_order",
           label = "% of the term's arguments",
           domain = c(0, 100), fmt = "pct0", ticks = 5),
  series = list(fields = list(
    list(field = "pct_in_seniority_order", class = "series-1",
         label = "first questions came in seniority order"),
    list(field = "pct_silent", class = "series-2",
         label = "Thomas asked nothing"))),
  points = TRUE, legend = TRUE,
  annotations = list(
    dd_annot_band(2019.5, 2020.5),
    dd_annot_text(2020, 100, "argued by telephone", anchor = "middle",
                  dy = -24)),
  tip = dd_js('function(d){
    return "<b>"+d.term+" term</b><br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> in seniority order: "+
        d.pct_in_seniority_order.toFixed(1)+"%<br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> Thomas silent: "+
        (d.pct_silent===null?"&mdash;":d.pct_silent.toFixed(1)+"%")+"<br>"+
      d.regime;
  }'))

## ---- order-static
th <- si[si$justice == "clarence_thomas", c("term", "pct_silent")]
mo <- merge(od[, c("term", "pct_in_seniority_order")], th, all.x = TRUE)
par(mar = c(3.0, 4.4, 2.4, 1.2))
plot(NA, xlim = c(T0, T1), ylim = c(0, 100), las = 1, xlab = "",
     ylab = "% of the term's arguments", cex.axis = 0.8)
rect(2019.5, -10, 2020.5, 110, col = "#00000010", border = NA)
text(2020, 106, "telephone", cex = 0.6, col = "#666666", xpd = NA)
lines(mo$term, mo$pct_in_seniority_order, col = BLU, lwd = 2)
points(mo$term, mo$pct_in_seniority_order, pch = 19, col = BLU, cex = 0.6)
lines(mo$term, mo$pct_silent, col = RED, lwd = 2)
points(mo$term, mo$pct_silent, pch = 19, col = RED, cex = 0.6)
# The legend goes in the middle-left, the one empty quarter of this plot. At
# the right-hand end both lines sit at 0% and their labels landed on top of
# each other.
legend(T0 + 0.6, 62, c("first questions came in seniority order",
                       "Thomas asked nothing"),
       col = c(BLU, RED), lwd = 2, pch = 19, pt.cex = 0.6, bty = "n",
       cex = 0.62, text.col = "#444444", y.intersp = 1.4)

## ---- epcompare
names(EP) <- c("quantity", "Epstein and Posner report", "this build, from Oyez")
EP

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
