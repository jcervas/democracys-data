# cost-of-voting-code.R -- chunk bodies for cost-of-voting-brief.Rmd
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

d  <- read.csv("data/derived/covi_2024.csv",       stringsAsFactors = FALSE)
ys <- read.csv("data/derived/year_structure.csv",  stringsAsFactors = FALSE)
rb <- read.csv("data/derived/rebuild_2024.csv",    stringsAsFactors = FALSE)
rw <- read.csv("data/derived/reweight_2024.csv",   stringsAsFactors = FALSE)
it <- read.csv("data/derived/items_2024.csv",      stringsAsFactors = FALSE)
tr <- read.csv("data/derived/control_trend.csv",   stringsAsFactors = FALSE)
cc <- read.csv("data/derived/control_check.csv",   stringsAsFactors = FALSE)
fa <- read.csv("data/derived/facts.csv",           stringsAsFactors = FALSE)

fact <- function(k) fa$value[fa$key == k]
pc   <- function(x, k = 2) formatC(as.numeric(x), format = "f", digits = k)
n    <- function(x) format(round(as.numeric(x)), big.mark = ",")

# named once here so no two paragraphs can disagree about them
EASY_NOW <- fact("easiest_2024_current")       # the state the current file ranks first
HARD     <- fact("hardest_2024")
Y96      <- ys[ys$year == 1996, ]
Y24      <- ys[ys$year == 2024, ]

# the election at which the Republican-trifecta mean rank first sits ABOVE the
# Democratic one -- i.e. where the two lines in Figure 4 cross
TRY   <- sort(unique(tr$year))
TRGAP <- vapply(TRY, function(y)
  tr$mean_rank[tr$year == y & tr$control == "Republican trifecta"] -
  tr$mean_rank[tr$year == y & tr$control == "Democratic trifecta"], numeric(1))
CROSS <- TRY[which(TRGAP > 0)[1]]

# the ten issue areas, in the order the source sheet carries them
IA <- c("reg_deadline", "reg_restrictions", "reg_drives", "preregistration",
        "automatic_reg", "inconveniences", "voter_id", "poll_hours",
        "early_voting", "absentee")
IA_LAB <- c("Registration deadline", "Registration restrictions",
            "Registration drives", "Preregistration", "Automatic registration",
            "Voting inconveniences", "Voter ID", "Poll hours",
            "Early voting days", "Absentee voting")

# the component structure of the 2024 index, recomputed here so the spreads
# quoted in the text come from the same arithmetic the build script verified
P24 <- prcomp(d[, IA], scale. = TRUE)
LOAD <- data.frame(area = IA_LAB, sd = sapply(d[, IA], sd), row.names = NULL)

# the hardest state's own row, and a standardized value: how many spreads
# above the fifty-state average that state sits on one issue area
MSR <- d[d$state == HARD, ]
Z   <- function(col, row) (row[[col]] - mean(d[[col]])) / sd(d[[col]])

# The static twins run through base-R devices, which cannot restyle for the
# dark page the way the shared library's classes do. Light values here; the
# D3 twins use the gop/dem/series classes brief.css owns.
CTRL_COL <- c(`Republican trifecta` = "#C41230",
              `Democratic trifecta` = "#2c7fb8",
              `Divided`             = "#9E9E9E",
              `Nonpartisan legislature` = "#4D4D4D")
CTRL_CLS <- list(`Republican trifecta` = "gop",
                 `Democratic trifecta` = "dem",
                 `Divided`             = "series-8",
                 `Nonpartisan legislature` = "series-7")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- d[d$state %in% c("Oregon", "Pennsylvania", HARD),
       c("state", "reg_deadline", "voter_id", "early_voting", "absentee",
         "inconveniences", "final", "final_rank")]
o <- o[order(o$final), ]
names(o) <- c("state", "reg. deadline", "voter ID", "early voting",
              "absentee", "inconveniences", "COVI", "rank")
o

## ---- rebuild-check
data.frame(
  check = c("States rebuilt from the published laws",
            "Largest difference from the published score",
            "States landing in the same rank",
            "Correlation with the published index"),
  result = c(nrow(rb), fact("repro_max_difference"),
             paste0(sum(rb$published_rank == rb$rebuilt_rank), " of 50"),
             pc(cor(rb$published, rb$rebuilt), 6)))

## ---- fig1-static
o <- d[order(d$final), ]
par(mar = c(4.2, 4.4, 0.6, 0.6))
bp <- barplot(rev(o$final), horiz = TRUE, names.arg = rev(o$abbr), las = 1,
              cex.names = 0.5, border = NA, col = rev(CTRL_COL[o$control]),
              xlim = c(-2.8, 2.3), xlab = "")
abline(v = 0, col = "#666666", lwd = 0.8)
# bottom-left: the last bars are the most expensive states, which run rightward
# from zero, so everything left of the axis down there is empty
legend("bottomleft", bty = "n", cex = 0.62, pch = 15, col = CTRL_COL,
       legend = names(CTRL_COL))
mtext("Cost of Voting Index, 2024  (lower = easier to vote)", side = 1,
      line = 2.4, cex = 0.8)
mtext(paste0(EASY_NOW, " is lowest, ", HARD, " highest. Colour is party control ",
             "of state government."),
      side = 1, line = 3.4, cex = 0.62, col = "#666666")

## ---- fig1-d3
# Drawn with the shared library (_lib/dd-charts.js): fifty named cases with one
# value each, sorted, which is what the bar type is for. dd_fig() emits the two
# <script src> tags for the document, and the hand-written figure below uses
# the d3 loaded here.
o <- d[order(d$final),
       c("abbr", "state", "final", "final_rank", "control", "vep_turnout")]
dd_fig("f1", "bar", o,
  size = list(w = 760),
  rowHeight = 14, padding = 0.16, catLabels = "inline",
  y = list(field = "abbr", band = TRUE),
  x = list(field = "final", domain = c(-2.8, 2.3), fmt = "f1", ticks = 7),
  series = list(field = "control", classes = CTRL_CLS),
  legend = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.state+"</b><br>ranked "+d.final_rank+" of 50, score "+
      d.final.toFixed(2)+"<br>"+d.control+"<br>turnout "+
      d.vep_turnout.toFixed(1)+"% of eligible adults";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Cost of Voting Index, 2024 (lower = easier to vote). Hover a bar for the
state, its rank and its turnout.</p>')

## ---- near-universal
o <- it[it$states_scored >= 40 | it$states_scored <= 3, ]
o <- o[order(-o$states_scored), c("item", "states_scored", "sd")]
o$sd <- pc(o$sd, 2)
names(o) <- c("law item", "states scoring it", "standard deviation")
o

## ---- reweight-table
o <- data.frame(
  rule = c("Published index", "Four components weighted equally",
           "First component alone", "Issue areas not standardized"),
  what_changes = c("--", "variance weights dropped",
                   "the other three components dropped",
                   "days and points left on their own scales"),
  mean_move = c("--", pc(mean(abs(rw$equal_move)), 1),
                pc(mean(abs(rw$pc1_move)), 1),
                pc(mean(abs(rw$unstandardized_move)), 1)),
  biggest_move = c("--",
    paste0(max(abs(rw$equal_move)), " (", rw$state[which.max(abs(rw$equal_move))], ")"),
    paste0(max(abs(rw$pc1_move)), " (", rw$state[which.max(abs(rw$pc1_move))], ")"),
    paste0(max(abs(rw$unstandardized_move)), " (",
           rw$state[which.max(abs(rw$unstandardized_move))], ")")))
names(o) <- c("weighting rule", "what changes", "mean rank move", "biggest move")
o

## ---- fig2-static
o <- rw[order(rw$published_rank), ]
o$control <- d$control[match(o$abbr, d$abbr)]
cols <- c("published_rank", "equal_rank", "pc1_rank", "unstandardized_rank")
labs <- c("published", "equal\nweights", "first\ncomponent", "not\nstandardized")
par(mar = c(3.0, 3.4, 1.4, 3.4))
plot(NA, xlim = c(0.86, 4.14), ylim = c(50.6, 0.4), axes = FALSE, xlab = "", ylab = "")
# the four states that travel furthest across the four rules, found rather than
# named, so the highlight cannot drift away from the caption
HL <- o$state[order(-pmax(abs(o$equal_move), abs(o$pc1_move),
                          abs(o$unstandardized_move)))[1:4]]
for (i in seq_len(nrow(o))) {
  hl <- o$state[i] %in% HL
  lines(1:4, as.numeric(o[i, cols]),
        col = if (hl) CTRL_COL[[o$control[i]]] else "#DCDCDC",
        lwd = if (hl) 2.0 else 0.7)
}
for (i in which(o$state %in% HL)) {
  points(1:4, as.numeric(o[i, cols]), pch = 19, cex = 0.5,
         col = CTRL_COL[[o$control[i]]])
  text(4.06, o$unstandardized_rank[i], o$abbr[i], pos = 4, cex = 0.62, xpd = NA,
       col = CTRL_COL[[o$control[i]]])
  text(0.94, o$published_rank[i], o$abbr[i], pos = 2, cex = 0.62, xpd = NA,
       col = CTRL_COL[[o$control[i]]])
}
axis(1, at = 1:4, labels = labs, tick = FALSE, cex.axis = 0.66, line = 0.2, padj = 0.4)
axis(2, at = c(1, 10, 20, 30, 40, 50), las = 1, cex.axis = 0.7)
mtext("rank (1 = easiest to vote)", side = 2, line = 2.3, cex = 0.72)
mtext("Each grey line is a state. The same 2024 laws under four weighting rules.",
      side = 3, line = 0.2, cex = 0.62, col = "#666666")

## ---- fig2-d3
# A DESIGNATED SHOWPIECE, and the one hand-written figure left in this brief.
# Fifty lines carried across four ranking rules is a parallel-coordinates plot,
# which the shared library has no type for, and the interaction -- raise one
# state to the front and read its four ranks -- is the whole point of the
# figure. d3 was loaded by Figure 1 above; nothing new is fetched here.
o <- rw[order(rw$published_rank), ]
o$control <- d$control[match(o$abbr, d$abbr)]
rows <- paste(sprintf('{"s":"%s","a":"%s","c":"%s","r":[%d,%d,%d,%d]}',
                      o$state, o$abbr, o$control, o$published_rank, o$equal_rank,
                      o$pc1_rank, o$unstandardized_rank), collapse = ",")
cat(sprintf('
<div id="f2" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const COL={"Republican trifecta":"%s","Democratic trifecta":"%s","Divided":"%s","Nonpartisan legislature":"%s"};
const LAB=["published","equal weights","first component","not standardized"];
const W=760,H=520,M={t:26,r:56,b:34,l:56};
const svg=d3.select("#f2").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain([0,1,2,3]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([1,50]).range([M.t,H-M.b]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([1,10,20,30,40,50]));
LAB.forEach((t,i)=>svg.append("text").attr("x",x(i)).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#444").text(t));
const line=d3.line().x((v,i)=>x(i)).y(v=>y(v));
const cap=d3.select("#f2").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover a line.</b> Each is one state, carried across four ways of weighting the same laws.";
const g=svg.append("g");
g.selectAll("path").data(D).join("path").attr("d",d=>line(d.r))
  .attr("fill","none").attr("stroke","#DCDCDC").attr("stroke-width",1.1)
  .style("cursor","pointer")
  .on("mousemove",function(e,d){
    d3.select(this).raise().attr("stroke",COL[d.c]).attr("stroke-width",2.6);
    cap.html("<b>"+d.s+"</b> \\u2014 published "+d.r[0]+", equal weights "+d.r[1]+
      ", first component "+d.r[2]+", unstandardized "+d.r[3]+".");})
  .on("mouseleave",function(){d3.select(this).attr("stroke","#DCDCDC").attr("stroke-width",1.1);
    cap.html(DEF);});
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H/2)).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#444")
  .text("rank (1 = easiest to vote)");
cap.html(DEF);
})();
</script>', rows, CTRL_COL[["Republican trifecta"]], CTRL_COL[["Democratic trifecta"]],
   CTRL_COL[["Divided"]], CTRL_COL[["Nonpartisan legislature"]]))

## ---- turnout-cases
o <- d[d$state %in% c("Hawaii", HARD, "Wisconsin", "Oregon", "Texas"),
       c("state", "final_rank", "final", "vep_turnout")]
o <- o[order(o$final_rank), ]
o$final <- pc(o$final); o$vep_turnout <- paste0(pc(o$vep_turnout, 1), "%")
names(o) <- c("state", "cost rank", "COVI", "turnout")
o

## ---- fig3-static
par(mar = c(4.2, 4.2, 2.2, 0.6))
plot(d$final, d$vep_turnout, pch = 21, cex = 1.25, lwd = 0.7,
     bg = CTRL_COL[d$control], col = "white",
     xlab = "", ylab = "", axes = FALSE, xlim = c(-2.8, 2.3),
     ylim = c(48, 79))
axis(1, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
abline(lm(vep_turnout ~ final, data = d), col = "#333333", lty = 2, lwd = 1.2)
LB <- c("Hawaii", HARD, "Wisconsin", "Oregon", "Texas", "West Virginia")
i <- d$state %in% LB
# Hawaii sits at the bottom of the cloud; its label goes below the point so it
# does not land on top of it
text(d$final[i], d$vep_turnout[i], d$abbr[i],
     pos = ifelse(d$state[i] == "Hawaii", 1, 3), cex = 0.62, col = "#333333")
mtext("Cost of Voting Index, 2024  (lower = easier)", side = 1, line = 2.4, cex = 0.8)
mtext("turnout, % of eligible adults", side = 2, line = 2.8, cex = 0.8)
# across the top margin, clear of every point
legend("top", horiz = TRUE, inset = c(0, -0.12), xpd = NA, bty = "n",
       cex = 0.58, pch = 21, pt.bg = CTRL_COL, col = "white",
       legend = names(CTRL_COL))
mtext(paste0("r = ", fact("turnout_correlation"), "; the line explains ",
             pc(100 * as.numeric(fact("turnout_r_squared")), 1),
             "% of the variation between states."),
      side = 1, line = 3.4, cex = 0.62, col = "#666666")

## ---- fig3-d3
# One point per state on the shared library's scatter. The fitted line is an
# annotation rather than a series: it is a summary of the points, not a second
# measurement of them.
fitl <- lm(vep_turnout ~ final, data = d)
LB <- c("Hawaii", HARD, "Wisconsin", "Oregon", "Texas", "West Virginia")
J <- data.frame(state = d$state, abbr = d$abbr, final = round(d$final, 4),
                turnout = round(d$vep_turnout, 2), control = d$control,
                rank = d$final_rank,
                lbl = ifelse(d$state %in% LB, d$abbr, NA_character_),
                stringsAsFactors = FALSE)
dd_fig("f3", "scatter", J,
  size = list(w = 760, h = 470, m = list(t = 18, r = 22, b = 52, l = 60)),
  x = list(field = "final", label = "Cost of Voting Index, 2024 (lower = easier)",
           domain = c(-2.8, 2.3), fmt = "f1", ticks = 7),
  y = list(field = "turnout", label = "turnout, % of eligible adults",
           domain = c(45, 78), fmt = "pct0", ticks = 6),
  series = list(field = "control", classes = CTRL_CLS),
  r = 6, opacity = 0.75, legend = TRUE,
  annotations = list(dd_annot_rule(-2.8, coef(fitl)[[1]] + coef(fitl)[[2]] * -2.8,
                                    2.3, coef(fitl)[[1]] + coef(fitl)[[2]] *  2.3)),
  tip = dd_tip(c(rank = "cost rank of 50", turnout = "turnout",
                 control = "government"),
               fmt = c(rank = "d", turnout = "pct1"), title = "state"))

## ---- control-trend
o <- reshape(tr[, c("year", "control", "mean_rank")], idvar = "year",
             timevar = "control", direction = "wide")
names(o) <- c("year", "Republican trifecta", "Democratic trifecta", "Divided")
o$gap <- o$`Republican trifecta` - o$`Democratic trifecta`
for (k in 2:5) o[[k]] <- pc(o[[k]], 1)
o$states <- cc$source[match(o$year, cc$year)]
names(o) <- c("year", "R trifecta", "D trifecta", "divided",
              "R minus D", "control from")
o

## ---- fig4-static
G <- c("Republican trifecta", "Democratic trifecta", "Divided")
par(mar = c(4.0, 4.4, 1.0, 6.6))
plot(NA, xlim = c(1994, 2026), ylim = c(40, 5), axes = FALSE, xlab = "", ylab = "")
abline(h = seq(10, 40, 10), col = "#EEEEEE")
for (g in G) {
  z <- tr[tr$control == g, ]
  lines(z$year, z$mean_rank, col = CTRL_COL[[g]], lwd = 2.2,
        lty = if (g == "Divided") 3 else 1)
  points(z$year, z$mean_rank, pch = 19, cex = 0.8, col = CTRL_COL[[g]])
  text(2025, z$mean_rank[z$year == 2024], sub(" trifecta", "", g), pos = 4,
       cex = 0.62, col = CTRL_COL[[g]], xpd = NA)
}
axis(1, at = tr$year[tr$control == G[1]], cex.axis = 0.68)
axis(2, at = seq(10, 40, 10), las = 1, cex.axis = 0.75)
mtext("mean rank of the group's states\n(1 = easiest to vote)", side = 2,
      line = 2.0, cex = 0.72)
mtext(paste0("Ranks closer to 1 are more accessible. The two trifecta lines ",
             "cross in the early 2000s."),
      side = 1, line = 2.5, cex = 0.6, col = "#666666")

## ---- fig4-d3
# Three groups across nine elections, on the shared library. The y domain runs
# 40 down to 5 because rank 1 is the accessible end and belongs at the top.
W <- reshape(tr[, c("year", "control", "mean_rank")], idvar = "year",
             timevar = "control", direction = "wide")
names(W) <- c("year", "rep", "dem", "div")
W <- W[order(W$year), ]
for (k in c("rep", "dem", "div")) W[[k]] <- round(W[[k]], 2)
dd_fig("f4", "line", W,
  size = list(w = 760, h = 430, m = list(t = 18, r = 150, b = 46, l = 64)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "rep", label = "mean rank (1 = easiest to vote)",
           domain = c(40, 5), fmt = "d", ticks = 5),
  series = list(fields = list(
    list(field = "rep", label = "Republican trifecta", class = "gop"),
    list(field = "dem", label = "Democratic trifecta", class = "dem"),
    list(field = "div", label = "Divided", class = "series-8"))),
  points = TRUE, endLabels = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'gop-txt\'>&#9632;</span> Republican trifectas: "+
        d.rep.toFixed(1)+"<br>"+
      "<span class=\'dem-txt\'>&#9632;</span> Democratic trifectas: "+
        d.dem.toFixed(1)+"<br>"+
      "<span class=\'series-8-txt\'>&#9632;</span> divided: "+d.div.toFixed(1);
  }'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
cat('<style>
#f1 text:not([fill])
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
</style>')
