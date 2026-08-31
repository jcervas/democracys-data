# cost-of-voting-code.R -- chunk bodies for cost-of-voting-brief.Rmd
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

d  <- read.csv("data/derived/covi_2024.csv",       stringsAsFactors = FALSE)
pn <- read.csv("data/derived/covi_panel.csv",      stringsAsFactors = FALSE)
ys <- read.csv("data/derived/year_structure.csv",  stringsAsFactors = FALSE)
rb <- read.csv("data/derived/rebuild_2024.csv",    stringsAsFactors = FALSE)
rw <- read.csv("data/derived/reweight_2024.csv",   stringsAsFactors = FALSE)
it <- read.csv("data/derived/items_2024.csv",      stringsAsFactors = FALSE)
tr <- read.csv("data/derived/control_trend.csv",   stringsAsFactors = FALSE)
cp <- read.csv("data/derived/control_panel.csv",   stringsAsFactors = FALSE)
cc <- read.csv("data/derived/control_check.csv",   stringsAsFactors = FALSE)
fa <- read.csv("data/derived/facts.csv",           stringsAsFactors = FALSE)
ch <- read.csv("data/derived/checks.csv",          stringsAsFactors = FALSE)

fact <- function(k) fa$value[fa$key == k]
pc   <- function(x, k = 2) formatC(as.numeric(x), format = "f", digits = k)
n    <- function(x) format(round(as.numeric(x)), big.mark = ",")

# named once here so no two paragraphs can disagree about them
EASY_PUB <- fact("easiest_2024_published")     # the state the article ranks first
EASY_NOW <- fact("easiest_2024_current")       # the state the current file ranks first
HARD     <- fact("hardest_2024")
Y96      <- ys[ys$year == 1996, ]
Y24      <- ys[ys$year == 2024, ]
R_TURN   <- as.numeric(fact("turnout_correlation"))

# the election at which the Republican-trifecta mean rank first sits ABOVE the
# Democratic one -- i.e. where the two lines in Figure 5 cross
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

# the component structure of the 2024 index, recomputed here so the loadings
# quoted in the text come from the same arithmetic the build script verified
P24 <- prcomp(d[, IA], scale. = TRUE)
SG  <- sign(colSums(P24$rotation[, 1:4]))
LOAD <- data.frame(area = IA_LAB, sd = sapply(d[, IA], sd),
                   pc1 = P24$rotation[, 1] * SG[1],
                   pc2 = P24$rotation[, 2] * SG[2], row.names = NULL)

CTRL_COL <- c(`Republican trifecta` = "#C41230",
              `Democratic trifecta` = "#2c7fb8",
              `Divided`             = "#9E9E9E",
              `Nonpartisan legislature` = "#4D4D4D")

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

## ---- source-table
data.frame(
  item = c("Who compiles it", "Published as", "Unit", "Coverage",
           "Machine-readable version", "Replication material"),
  value = c("Pomante, Schraufnagel and Li (academic researchers)",
            "A journal article each cycle, plus a project website",
            "State, in an election year",
            paste0(fact("n_years"), " elections, 1996-2024, all 50 states"),
            "Yes -- three spreadsheets on the project site",
            "The laws, the codebook, and the component weights"))

## ---- one-row
o <- d[d$state %in% c("Oregon", "Pennsylvania", HARD),
       c("state", "reg_deadline", "voter_id", "early_voting", "absentee",
         "inconveniences", "final", "final_rank")]
o <- o[order(o$final), ]
names(o) <- c("state", "reg. deadline", "voter ID", "early voting",
              "absentee", "inconveniences", "COVI", "rank")
o

## ---- items-extremes
o <- rbind(
  data.frame(item = it$item[1:4], states = it$states_scored[1:4],
             group = "in only a few states"),
  data.frame(item = tail(it$item, 4), states = tail(it$states_scored, 4),
             group = "in nearly every state"))
names(o) <- c("law item", "states scoring it", "")
o

## ---- weights
o <- data.frame(
  component = c("First", "Second", "Third", "Fourth", "Total"),
  variance_explained = c(pc(P24$sdev[1:4]^2 / sum(P24$sdev^2), 4),
                         pc(sum(P24$sdev[1:4]^2) / sum(P24$sdev^2), 4)),
  weight_in_the_index = c(pc((P24$sdev[1:4]^2 / sum(P24$sdev^2)) /
                             (sum(P24$sdev[1:4]^2) / sum(P24$sdev^2)), 4), "1.0000"))
names(o) <- c("component", "share of variance explained", "weight in the index")
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
o <- d[order(d$final), ]
rows <- paste(sprintf('{"s":"%s","a":"%s","v":%.4f,"r":%d,"c":"%s","t":%.2f}',
                      o$state, o$abbr, o$final, o$final_rank, o$control,
                      o$vep_turnout), collapse = ",")
cat(sprintf('
<div id="f1" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const COL={"Republican trifecta":"%s","Democratic trifecta":"%s","Divided":"%s","Nonpartisan legislature":"%s"};
const W=760,H=720,M={t:12,r:20,b:44,l:52};
const svg=d3.select("#f1").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-2.8,2.3]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.a)).range([M.t,H-M.b]).padding(0.18);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(7));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0))
  .selectAll("text").attr("font-size","9.5px");
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#888").attr("stroke-dasharray","2,2");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Cost of Voting Index, 2024  (lower = easier to vote)");
const cap=d3.select("#f1").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover a state.</b> Colour is party control of state government in 2024.";
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>Math.min(x(0),x(d.v))).attr("y",d=>y(d.a)).attr("height",y.bandwidth())
  .attr("fill",d=>COL[d.c]).attr("width",0).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.s+"</b> \\u2014 ranked "+d.r+" of 50, score "+
    d.v.toFixed(2)+". "+d.c+". Turnout "+d.t.toFixed(1)+"%% of eligible adults."))
  .on("mouseleave",()=>cap.html(DEF))
  .transition().delay((d,i)=>i*12).duration(320)
  .attr("width",d=>Math.abs(x(d.v)-x(0)));
const lg=svg.append("g").attr("transform",`translate(${W-250},${H-150})`);
Object.entries(COL).forEach(([k,v],i)=>{
  lg.append("rect").attr("y",i*17).attr("width",11).attr("height",11).attr("fill",v);
  lg.append("text").attr("x",16).attr("y",i*17+10).attr("font-size","11px").text(k);});
cap.html(DEF);
})();
</script>', rows, CTRL_COL[["Republican trifecta"]], CTRL_COL[["Democratic trifecta"]],
   CTRL_COL[["Divided"]], CTRL_COL[["Nonpartisan legislature"]]))

## ---- near-universal
o <- it[it$states_scored >= 40 | it$states_scored <= 3, ]
o <- o[order(-o$states_scored), c("item", "states_scored", "sd")]
o$sd <- pc(o$sd, 2)
names(o) <- c("law item", "states scoring it", "standard deviation")
o

## ---- loadings
o <- LOAD[order(-abs(LOAD$pc1)), ]
o$sd <- pc(o$sd, 2); o$pc1 <- pc(o$pc1, 3); o$pc2 <- pc(o$pc2, 3)
names(o) <- c("issue area", "sd across states", "loading, 1st component",
              "loading, 2nd")
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
o <- rw[order(rw$published_rank), ]
o$control <- d$control[match(o$abbr, d$abbr)]
rows <- paste(sprintf('{"s":"%s","a":"%s","c":"%s","r":[%d,%d,%d,%d]}',
                      o$state, o$abbr, o$control, o$published_rank, o$equal_rank,
                      o$pc1_rank, o$unstandardized_rank), collapse = ",")
cat(sprintf('
<div id="f2" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
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

## ---- turnout-table
data.frame(
  quantity = c("Correlation, cost against turnout",
               "Variation in turnout explained",
               "p-value", "Turnout in the 3 cheapest states",
               "Turnout in the 3 most expensive"),
  value = c(fact("turnout_correlation"),
            paste0(pc(100 * as.numeric(fact("turnout_r_squared")), 1), "%"),
            fact("turnout_p_value"),
            paste0(pc(mean(d$vep_turnout[order(d$final)][1:3]), 1), "%"),
            paste0(pc(mean(d$vep_turnout[order(-d$final)][1:3]), 1), "%")))

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
fit <- lm(vep_turnout ~ final, data = d)
rows <- paste(sprintf('{"s":"%s","a":"%s","x":%.4f,"y":%.2f,"c":"%s","r":%d}',
                      d$state, d$abbr, d$final, d$vep_turnout, d$control,
                      d$final_rank), collapse = ",")
cat(sprintf('
<div id="f3" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const COL={"Republican trifecta":"%s","Democratic trifecta":"%s","Divided":"%s","Nonpartisan legislature":"%s"};
const W=760,H=470,M={t:16,r:20,b:52,l:60};
const svg=d3.select("#f3").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-2.8,2.3]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([45,78]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(7));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("line").attr("x1",x(-2.8)).attr("y1",y(%.4f+%.4f*-2.8))
  .attr("x2",x(2.3)).attr("y2",y(%.4f+%.4f*2.3))
  .attr("stroke","#333").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Cost of Voting Index, 2024  (lower = easier to vote)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H/2)).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("turnout, %% of eligible adults");
const cap=d3.select("#f3").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover a state.</b> The dashed line is the best straight-line fit; it explains %s%% of the variation.";
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",0)
  .attr("fill",d=>COL[d.c]).attr("stroke","#fff").attr("stroke-width",1.2)
  .style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.s+"</b> \\u2014 cost rank "+d.r+
    " of 50, turnout "+d.y.toFixed(1)+"%% of eligible adults."))
  .on("mouseleave",()=>cap.html(DEF))
  .transition().delay((d,i)=>i*10).duration(300).attr("r",6);
const lg=svg.append("g").attr("transform",`translate(${M.l+10},${H-M.b-76})`);
Object.entries(COL).forEach(([k,v],i)=>{
  lg.append("circle").attr("cy",i*17).attr("r",5).attr("fill",v);
  lg.append("text").attr("x",11).attr("y",i*17+4).attr("font-size","11px").text(k);});
cap.html(DEF);
})();
</script>', rows, CTRL_COL[["Republican trifecta"]], CTRL_COL[["Democratic trifecta"]],
   CTRL_COL[["Divided"]], CTRL_COL[["Nonpartisan legislature"]],
   coef(fit)[1], coef(fit)[2], coef(fit)[1], coef(fit)[2],
   pc(100 * as.numeric(fact("turnout_r_squared")), 1)))

## ---- two-scorings
o <- d[order(d$initial)[1:5], c("state", "initial", "initial_rank",
                                "final", "final_rank")]
o$initial <- pc(o$initial); o$final <- pc(o$final)
names(o) <- c("state", "as published", "rank", "current file", "rank")
o

## ---- structure-table
o <- ys[, c("year", "n_issue_areas", "n_components", "var_explained",
            "cor_init_final", "mean_rank_move", "max_rank_move",
            "max_move_state")]
o$var_explained  <- pc(o$var_explained, 3)
o$cor_init_final <- pc(o$cor_init_final, 3)
o$mean_rank_move <- pc(o$mean_rank_move, 1)
names(o) <- c("year", "issue areas", "components", "variance explained",
              "r, published vs current", "mean rank move", "max", "biggest mover")
o

## ---- moves-1996
p96 <- pn[pn$year == 1996, ]
p96$ir <- rank(p96$initial); p96$fr <- rank(p96$final)
p96$mv <- p96$fr - p96$ir
o <- p96[order(-abs(p96$mv))[1:6], c("state", "ir", "fr", "mv")]
o$ir <- round(o$ir); o$fr <- round(o$fr); o$mv <- round(o$mv)
names(o) <- c("state", "rank as published", "rank in current file", "move")
o

## ---- fig4-static
par(mar = c(4.0, 4.4, 1.0, 4.2))
plot(ys$year, ys$mean_rank_move, type = "n", axes = FALSE, xlab = "", ylab = "",
     ylim = c(0, 15), xlim = c(1994, 2026))
segments(ys$year, 0, ys$year, ys$mean_rank_move, col = "#DCDCDC", lwd = 6)
points(ys$year, ys$mean_rank_move, pch = 19, col = "#C41230", cex = 1.1)
lines(ys$year, ys$mean_rank_move, col = "#C41230", lwd = 1.4)
axis(1, at = ys$year, cex.axis = 0.68); axis(2, las = 1, cex.axis = 0.75)
mtext("mean rank move between\nthe two published scorings", side = 2, line = 2.0,
      cex = 0.72)
par(new = TRUE)
plot(ys$year, ys$n_issue_areas, type = "s", col = "#2c7fb8", lwd = 1.6,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 12), xlim = c(1994, 2026))
axis(4, las = 1, cex.axis = 0.75, col = "#2c7fb8", col.axis = "#2c7fb8")
mtext("issue areas in the index", side = 4, line = 2.4, cex = 0.72, col = "#2c7fb8")
# one line only: the caption below carries the rest, and a second line here
# collided with it
mtext("Red: mean rank move between the two scorings.   Blue: issue areas in the index.",
      side = 1, line = 2.5, cex = 0.6, col = "#666666")

## ---- fig4-d3
rows <- paste(sprintf('{"y":%d,"m":%.2f,"n":%d,"r":%.3f,"s":"%s","x":%d}',
                      ys$year, ys$mean_rank_move, ys$n_issue_areas,
                      ys$cor_init_final, ys$max_move_state, ys$max_rank_move),
              collapse = ",")
cat(sprintf('
<div id="f4" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=400,M={t:18,r:60,b:46,l:62};
const svg=d3.select("#f4").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(D.map(d=>d.y)).range([M.l,W-M.r]).padding(0.5);
const y=d3.scaleLinear().domain([0,15]).range([H-M.b,M.t]);
const y2=d3.scaleLinear().domain([0,12]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("g").attr("transform",`translate(${W-M.r},0)`)
  .call(d3.axisRight(y2).ticks(6)).selectAll("text").attr("fill","#2c7fb8");
svg.append("path").datum(D).attr("fill","none").attr("stroke","#2c7fb8")
  .attr("stroke-width",1.8)
  .attr("d",d3.line().curve(d3.curveStepAfter).x(d=>x(d.y)).y(d=>y2(d.n)));
svg.append("path").datum(D).attr("fill","none").attr("stroke","%s")
  .attr("stroke-width",1.8).attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.m)));
const cap=d3.select("#f4").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover an election.</b> Red: how far the average state moves between the two published scorings of that year. Blue: how many issue areas the index was built from.";
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.m)).attr("r",6).attr("fill","%s")
  .style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.y+"</b> \\u2014 built from "+d.n+
    " issue areas. Between the two scorings the average state moves "+d.m.toFixed(1)+
    " places and "+d.s+" moves "+d.x+"; the two versions correlate "+d.r.toFixed(3)+"."))
  .on("mouseleave",()=>cap.html(DEF));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H/2)).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#444")
  .text("mean rank move between scorings");
svg.append("text").attr("transform","rotate(90)").attr("x",H/2).attr("y",-(W-14))
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#2c7fb8")
  .text("issue areas in the index");
cap.html(DEF);
})();
</script>', rows, CTRL_COL[["Republican trifecta"]], CTRL_COL[["Republican trifecta"]]))

## ---- control-table
o <- do.call(rbind, lapply(c("Democratic trifecta", "Divided",
                             "Republican trifecta"), function(g) {
  s <- d[d$control == g, ]
  data.frame(control = g, states = nrow(s),
             mean_rank = pc(mean(s$final_rank), 1),
             mean_covi = pc(mean(s$final)),
             easiest = s$state[which.min(s$final)],
             hardest = s$state[which.max(s$final)])
}))
names(o) <- c("party control, 2024", "states", "mean rank", "mean COVI",
              "easiest", "hardest")
o

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

## ---- fig5-static
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

## ---- fig5-d3
G <- c("Republican trifecta", "Democratic trifecta", "Divided")
ser <- paste(sapply(G, function(g) {
  z <- tr[tr$control == g, ]
  sprintf('{"g":"%s","c":"%s","p":[%s]}', g, CTRL_COL[[g]],
          paste(sprintf('{"y":%d,"r":%.2f,"n":%d}', z$year, z$mean_rank, z$n),
                collapse = ","))
}), collapse = ",")
cat(sprintf('
<div id="f5" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const S=[%s];
const W=760,H=430,M={t:18,r:150,b:46,l:64};
const svg=d3.select("#f5").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const YRS=S[0].p.map(d=>d.y);
const x=d3.scalePoint().domain(YRS).range([M.l,W-M.r]).padding(0.5);
const y=d3.scaleLinear().domain([40,5]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(5));
const line=d3.line().x(d=>x(d.y)).y(d=>y(d.r));
const cap=d3.select("#f5").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover a point.</b> Each line is the average rank of the states under that kind of government. Lower is easier to vote.";
S.forEach(s=>{
  svg.append("path").datum(s.p).attr("fill","none").attr("stroke",s.c)
    .attr("stroke-width",2.4)
    .attr("stroke-dasharray",s.g==="Divided"?"4,3":null).attr("d",line);
  svg.append("text").attr("x",x(2024)+10).attr("y",y(s.p[s.p.length-1].r)+4)
    .attr("font-size","11.5px").attr("fill",s.c).text(s.g);
  svg.append("g").selectAll("circle").data(s.p).join("circle")
    .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.r)).attr("r",5).attr("fill",s.c)
    .style("cursor","pointer")
    .on("mousemove",(e,d)=>cap.html("<b>"+d.y+"</b> \\u2014 the "+d.n+
      " states with "+s.g.toLowerCase()+" averaged rank "+d.r.toFixed(1)+" of 50."))
    .on("mouseleave",()=>cap.html(DEF));
});
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H/2)).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#444")
  .text("mean rank (1 = easiest to vote)");
cap.html(DEF);
})();
</script>', ser))

## ---- checks-table
ch

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
