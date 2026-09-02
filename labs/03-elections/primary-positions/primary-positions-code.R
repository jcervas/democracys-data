# primary-positions-code.R -- chunk bodies for primary-positions-brief.Rmd
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

# Everything below is read from data/, written by data/build-data.R out of the
# two files in data/raw/. Nothing here re-derives anything.
CA <- read.csv("data/derived/candidates.csv",   stringsAsFactors = FALSE)
BI <- read.csv("data/derived/by_issue.csv",     stringsAsFactors = FALSE)
BC <- read.csv("data/derived/by_count.csv",     stringsAsFactors = FALSE)
BO <- read.csv("data/derived/by_outcome.csv",   stringsAsFactors = FALSE)
FA <- read.csv("data/derived/factions.csv",     stringsAsFactors = FALSE)
PL <- read.csv("data/derived/party_labels.csv", stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",       stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("by_count.csv", "by_issue.csv", "by_outcome.csv", "candidates.csv", "checks.csv", "factions.csv", "party_labels.csv"))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)
ck <- function(k) {
  v <- CK$value[CK$check == k]
  if (length(v) != 1L) stop("checks.csv has no single value for '", k, "'")
  v
}

NCAND <- nrow(CA)
NISS  <- nrow(BI)
MED   <- median(CA$silent_on)
ALL14 <- sum(CA$silent_on == NISS)
NONE  <- sum(CA$silent_on == 0)
NHOU  <- sum(!CA$senate)
QUIET <- BI[1, ]
LOUD  <- BI[nrow(BI), ]
WON   <- BO$mean_silent[BO$prim_result == "won"]
LOST  <- BO$mean_silent[BO$prim_result == "lost"]
NUNDOC <- sum(FA$candidates[!FA$documented])

# The one candidate the prose walks through: Dave Brat, the challenger who
# beat Eric Cantor in Virginia's seventh. The issue columns share their names
# with by_issue.csv, so the codes can be read off his row by name.
ISS <- BI$issue
BRT <- CA[CA$last == "Brat" & CA$state == "VA", ]
BRT <- BRT[1, ]
BRC <- unlist(BRT[make.names(ISS)])     # his fourteen codes, by issue
                                        # (read.csv dots the spaces in names)
BRS <- ISS[BRC == 4]                    # the issues he said nothing about

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
  quantity = c("Candidates coded", "House candidates", "Senate candidates",
               "Issues coded per candidate", "Columns in the workbook",
               "Primary election dates covered"),
  value = c(n(NCAND), n(NHOU), sum(CA$senate), NISS,
            ck("columns in the workbook"), "15, from 4 March to 9 September"))

## ---- codes
data.frame(
  code = 1:4,
  `what the codebook calls it` = c(
    "Candidate Supports",
    "Candidate Opposes",
    "Candidate Provides Complicated/Complex/Unclear Position",
    "Candidate Provides No Information"),
  check.names = FALSE)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# FOURTEEN ISSUES, EACH SPLIT FOUR WAYS. Stacked horizontal bars, sorted by the
# silent share, because the comparison the chapter wants is "which subjects did
# candidates stay off" rather than "which side won". Widths are shares of the
# same 1,662 candidates in every row, so the rows are directly comparable.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figure uses the library loaded
# here.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"i":"%s","a":%d,"b":%d,"c":%d,"d":%d}',
                      BI$issue, BI$supports, BI$opposes, BI$unclear, BI$silent),
              collapse = ",")
cat(paste0('
<div id="iss" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '],N=', NCAND, ';
const W=760,H=430,M={t:26,r:60,b:34,l:150};
const svg=d3.select("#iss").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,N]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.i)).range([M.t,H-M.b]).padding(0.26);
const CO={a:"#4a7fb5",b:"#c98a3f",c:"#b8b8b8",d:"#C41230"};
const KEY=[["a","supports"],["b","opposes"],["c","unclear"],["d","did not raise it"]];
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y))
  .selectAll("text").attr("font-size","11.5px");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>Math.round(100*d/N)+"%"));
const tip=d3.select("#iss").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach(d=>{
  let acc=0;
  KEY.forEach(([k,lab])=>{
    svg.append("rect").attr("x",x(acc)).attr("y",y(d.i))
      .attr("width",x(d[k])-x(0)).attr("height",y.bandwidth())
      .attr("fill",CO[k]).style("cursor","pointer")
      .on("mousemove",function(e){
        tip.style("opacity",1).html("<b>"+d.i+"</b><br>"+d[k]+" candidates "+lab+
          " ("+(100*d[k]/N).toFixed(1)+"%)")
          .style("left",Math.min(e.offsetX+14,W-250)+"px").style("top",(e.offsetY-8)+"px");})
      .on("mouseleave",()=>tip.style("opacity",0));
    acc+=d[k];
  });
  svg.append("text").attr("x",W-M.r+6).attr("y",y(d.i)+y.bandwidth()/2+4)
    .attr("font-size","11px").attr("font-weight","600").attr("fill","#C41230")
    .text((100*d.d/N).toFixed(0)+"%");
});
const lg=d3.select("#iss").append("div").attr("style",
  "margin-top:6px;font-size:12px;color:#444");
lg.html(KEY.map(([k,lab])=>"<span style=\\"display:inline-block;width:11px;"+
  "height:11px;background:"+CO[k]+";margin-right:4px\\"></span>"+lab).join("&nbsp;&nbsp; "));
})();
</script>'))

## ---- fig1-static
# The same fourteen rows, base R for the PDF device. BI is sorted with the
# quietest subject FIRST, and a horizontal barplot draws its first row at the
# BOTTOM -- so the rows are reversed here to put Benghazi at the top, which is
# where the d3 version puts it. Without this the two renders of "Figure 1" read
# upside down from each other and the paragraph below, which talks about the
# top row and the bottom row, is right in only one of them.
R  <- BI[rev(seq_len(nrow(BI))), ]
M  <- t(as.matrix(R[, c("supports", "opposes", "unclear", "silent")]))
CO <- c("#4a7fb5", "#c98a3f", "#b8b8b8", "#C41230")
par(mar = c(3.0, 9.4, 0.6, 3.0), mgp = c(2.0, 0.6, 0))
b <- barplot(M, horiz = TRUE, col = CO, border = NA, names.arg = R$issue,
             las = 1, cex.names = 0.62, xaxt = "n", xlab = "")
axis(1, at = seq(0, NCAND, length.out = 6),
     labels = paste0(round(100 * seq(0, 1, length.out = 6)), "%"), cex.axis = 0.66)
text(NCAND + 40, b, paste0(round(R$silent_pct), "%"), adj = 0, cex = 0.58,
     font = 2, col = "#C41230", xpd = NA)
legend("bottom", c("supports", "opposes", "unclear", "did not raise it"),
       fill = CO, border = NA, horiz = TRUE, bty = "n", cex = 0.55,
       inset = c(0, -0.16), xpd = NA)

## ---- fig2-d3
# ---------------------------------------------------------------------------
# THE PER-CANDIDATE DISTRIBUTION, drawn with the shared library
# (_lib/dd-charts.js). Columns rather than a smooth curve, because the
# variable is a count from 0 to 14 and there is no value between 7 and 8.
# The two ends are called out in the figure itself: they are the shapes the
# reader is least likely to have predicted. The hand-written Figure 1 above
# already loaded d3, so dd_fig() adds only the chart library (d3 = FALSE).
B2 <- BC[, c("silent_on", "candidates")]
B2$grp <- ifelse(B2$silent_on %in% c(0, 14), "end", "mid")
dd_fig("dst", "bar", B2,
  size = list(w = 760, h = 330, m = list(t = 24, r = 16, b = 46, l = 52)),
  x = list(field = "silent_on"),
  y = list(field = "candidates", label = "candidates", fmt = "d",
           domain = c(0, max(BC$candidates) * 1.14)),
  series = list(field = "grp",
                classes = list(end = "gop", mid = "series-1")),
  tip = dd_js(paste0('function(d){return "<b>"+d.candidates+
    " candidates</b><br>silent on "+d.silent_on+" of 14 ("+
    (100*d.candidates/', NCAND, ').toFixed(1)+"% of the field)";}')),
  annotations = list(
    list(type = "text", x = MED, y = BC$candidates[BC$silent_on == MED] + 14,
         text = "median", anchor = "middle", weight = 600),
    list(type = "text", x = 0, y = BC$candidates[BC$silent_on == 0] + 14,
         text = paste(NONE, "said something about all 14"),
         anchor = "start", class = "gop-txt", weight = 600, size = 10.5),
    list(type = "text", x = 14, y = BC$candidates[BC$silent_on == 14] + 14,
         text = paste(ALL14, "said nothing about any"),
         anchor = "end", class = "gop-txt", weight = 600, size = 10.5),
    list(type = "text", x = 380, y = 324, px = TRUE, anchor = "middle",
         text = "issues, of 14, the candidate said nothing about")),
  d3 = FALSE)

## ---- fig2-static
par(mar = c(3.4, 3.8, 0.8, 0.8), mgp = c(2.3, 0.6, 0))
b <- barplot(BC$candidates, border = NA, las = 1,
             col = ifelse(BC$silent_on %in% c(0, 14), "#C41230", "#7fa3c4"),
             xlab = "issues, of 14, the candidate said nothing about",
             ylab = "candidates", ylim = c(0, max(BC$candidates) * 1.14))
axis(1, at = b, labels = BC$silent_on, cex.axis = 0.62, tick = FALSE, line = -0.7)
text(b[BC$silent_on == MED], BC$candidates[BC$silent_on == MED] + 14, "median",
     cex = 0.55, font = 2)
for (k in c(0, 14))
  text(b[BC$silent_on == k], BC$candidates[BC$silent_on == k] + 14,
       BC$candidates[BC$silent_on == k], cex = 0.55, font = 2, col = "#C41230")

## ---- outcome
BO$mean_silent <- round(BO$mean_silent, 1)
names(BO) <- c("primary result", "House candidates", "mean issues left unmentioned")
BO

## ---- labels
P <- PL[PL$candidates > 1 | PL$trailing_space | PL$label_as_stored %in% c("G.O.P.", "libertarian"), ]
P$label_as_stored <- ifelse(P$trailing_space,
                            paste0('"', P$label_as_stored, '"  <- ends in a space'),
                            P$label_as_stored)
P <- P[, c("label_as_stored", "candidates", "characters")]
names(P) <- c("the string in the cell", "candidates", "characters")
P

## ---- readers
data.frame(
  `reading the same file with` = c(
    "Python, pandas.read_excel()",
    "R, readxl::read_excel()",
    "R, readxl::read_excel(trim_ws = FALSE)"),
  `distinct party strings` = c(15, 14, 15),
  `what happens to \"D \"` = c("kept", "silently trimmed to \"D\"", "kept"),
  check.names = FALSE)

## ---- factions
F2 <- FA
F2$codebook_label <- ifelse(F2$documented, F2$codebook_label, "— not defined —")
F2 <- F2[, c("code", "candidates", "party", "codebook_label")]
names(F2) <- c("code", "candidates", "party", "what the codebook says it means")
F2

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
