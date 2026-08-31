# returns-source-code.R -- chunk bodies for returns-source-brief.Rmd
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

lad <- read.csv("data/derived/ladder.csv",     stringsAsFactors = FALSE)
eco <- read.csv("data/derived/ecological.csv", stringsAsFactors = FALSE)
nat <- read.csv("data/derived/national.csv",   stringsAsFactors = FALSE)
src <- read.csv("data/derived/sources.csv",    stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

BALLOTS <- lad$units_nationally[lad$level == "ballot"]
PRECS   <- lad$units_nationally[lad$level == "precinct"]
COUNTIES<- lad$units_nationally[lad$level == "county"]

EP <- eco$estimate_mail[grepl("precinct", eco$computed_from)]
EC <- eco$estimate_mail[grepl("counties", eco$computed_from)]
TR <- eco$truth_mail[1]

natl <- nat[grepl("^National", nat$comparison), ]
NDIFF <- natl$difference[grepl("Democratic", natl$comparison)]
NTOT  <- natl$their_total[grepl("Democratic", natl$comparison)]
GA11  <- nat$difference[nat$comparison == "Georgia Democratic votes" &
                        nat$difference != 0]

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

ACC <- "#1C4C5C"; WARN <- "#8A3B2C"; GRY <- "#8A8F94"

## ---- ecotab
data.frame(Computed_from = eco$computed_from,
           Estimate_for_mail_voters = paste0(p1(eco$estimate_mail), "%"),
           The_truth = paste0(p1(eco$truth_mail), "%"),
           Error_in_points = sprintf("%+.1f", eco$error),
           Impossible_value = eco$impossible)

## ---- fig1-static
op <- par(mar = c(3.0, 8.6, 1.4, 2.2), mgp = c(2.4, 0.7, 0))
v <- c(EP, EC, TR)
lb <- c("from 2,653 precincts", "from 159 counties", "the truth, from ballots")
cl <- c(WARN, WARN, ACC)
plot(NA, xlim = c(0, max(v) + 20), ylim = c(0.4, 3.6), axes = FALSE,
     xlab = "", ylab = "")
rect(0, 0.3, 100, 3.7, col = "#EAF0F2", border = NA)
segments(0, 3:1, v, 3:1, col = cl, lwd = 9, lend = 1)
axis(1, at = seq(0, 250, 50), labels = paste0(seq(0, 250, 50), "%"),
     cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = 3:1, labels = lb, las = 1, cex.axis = 0.8, lwd = 0, tick = FALSE)
text(v, 3:1, paste0(" ", p1(v), "%"), pos = 4, cex = 0.8, col = cl)
text(50, 3.45, "the range a percentage may occupy", cex = 0.7, col = "#4E5A63")
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The whole figure is one comparison against one boundary: 100%. Everything to
# the right of it is a region no percentage may occupy, so the impossible zone
# is drawn explicitly rather than left to be inferred from an axis.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('{lab:"',
  c("from 2,653 precincts", "from 159 counties", "the truth, from ballots"),
  '",v:', c(EP, EC, TR), ',c:"', c(WARN, WARN, ACC), '",note:"',
  c("ecological inference over precincts",
    "ecological inference over counties",
    "counted directly from cast vote records"), '"}', collapse = ",")
cat(paste0('
<div id="rsb" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const W=770,H=280,M={t:34,r:60,b:40,l:196};
const box=d3.select("#rsb");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.v)+20]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.lab)).range([M.t,H-M.b]).padding(0.42);
// the only region a percentage is allowed to occupy
svg.append("rect").attr("x",x(0)).attr("y",M.t-14)
  .attr("width",x(100)-x(0)).attr("height",H-M.b-M.t+20).attr("fill","#EAF0F2");
svg.append("text").attr("x",(x(0)+x(100))/2).attr("y",M.t-20)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
  .text("the range a percentage may occupy");
svg.append("line").attr("x1",x(100)).attr("x2",x(100)).attr("y1",M.t-14)
  .attr("y2",H-M.b+6).attr("stroke","#12181D").attr("stroke-dasharray","3 3");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(6));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickSize(0)).selectAll("text").style("font-size","11.5px");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.selectAll("rect.b").data(D).join("rect").attr("class","b")
  .attr("x",x(0)).attr("y",d=>y(d.lab)).attr("height",y.bandwidth())
  .attr("width",d=>x(d.v)-x(0)).attr("fill",d=>d.c)
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.lab+"</b><br>"+d.v.toFixed(1)+"%<br>"+
         "<span style=\\"color:#4E5A63\\">"+d.note+"</span>"+
         (d.v>100?"<br><b>impossible: above 100%</b>":""));
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
svg.selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>x(d.v)+7).attr("y",d=>y(d.lab)+y.bandwidth()/2+4)
  .attr("font-size","12px").attr("font-weight","600").attr("fill",d=>d.c)
  .text(d=>d.v.toFixed(1)+"%");
})();
</script>'))

## ---- ladtab
data.frame(Rung = lad$rung, Level = lad$level,
           Units_nationally = ifelse(is.na(lad$units_nationally), "—",
                                     nn(lad$units_nationally)),
           A_row_is = lad$a_row_is)

## ---- srctab
data.frame(Publisher = src$publisher, Publishes = src$what_it_publishes,
           Finest_grain = src$finest_grain, Obliged_to = src$obliged_to,
           If_it_is_wrong = src$correction_path)

## ---- nattab
data.frame(Comparison = nat$comparison, One_source = nat$one_source,
           Its_total = nn(nat$its_total), Other_source = nat$other_source,
           Their_total = nn(nat$their_total),
           Difference = sprintf("%+d", as.integer(nat$difference)))
