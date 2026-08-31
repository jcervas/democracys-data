# finance-source-code.R -- chunk bodies for finance-source-brief.Rmd
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

fil  <- read.csv("data/derived/filed.csv",        stringsAsFactors = FALSE)
out  <- read.csv("data/derived/outliers.csv",     stringsAsFactors = FALSE)
dist <- read.csv("data/derived/distribution.csv", stringsAsFactors = FALSE)
reg  <- read.csv("data/derived/regime.csv",       stringsAsFactors = FALSE)

nn  <- function(x) format(round(x), big.mark = ",")
p1  <- function(x) formatC(x, format = "f", digits = 1)
bn  <- function(x) paste0("$", formatC(x / 1e9, format = "f", digits = 2), " billion")
usd <- function(x) paste0("$", format(round(x), big.mark = ","))

RAWS  <- fil$supporting[1];  RAWP  <- fil$pct_supporting[1]
CLNS  <- fil$supporting[2];  CLNP  <- fil$pct_supporting[2]
OPPP  <- fil$pct_opposing[2]
JUNK  <- sum(out$amount);    NJUNK <- nrow(out)
TOPJ  <- out$amount[1];      TOPS  <- out$spender[1]

dv <- function(q) dist$value[dist$quantity == q]
NCAND <- dv("Candidates in the file")
ZERO  <- dv("Candidates who raised nothing at all")
MEANR <- dv("Mean receipts")
MEDR  <- dv("Median receipts")
RATIO <- dv("Ratio of mean to median")
TOP1  <- dv("Share of all money raised by the top 1% of candidates")

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

## ---- filedtab
data.frame(The_file = fil$the_file,
           Supporting = bn(fil$supporting),
           Opposing = bn(fil$opposing),
           Pct_supporting = paste0(p1(fil$pct_supporting), "%"),
           It_reads_as = fil$reads_as)

## ---- fig1-static
op <- par(mar = c(2.8, 9.0, 1.6, 2.4), mgp = c(2.4, 0.6, 0))
m <- rbind(c(fil$pct_supporting[1], fil$pct_opposing[1]),
           c(fil$pct_supporting[2], fil$pct_opposing[2]))
b <- barplot(t(m), horiz = TRUE, beside = FALSE, col = c(ACC, WARN),
             border = NA, axes = FALSE, names.arg = c("as filed", "16 rows removed"),
             las = 1, cex.names = 0.8, xlim = c(0, 100))
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
text(m[, 1] / 2, b, paste0(p1(m[, 1]), "% for"), cex = 0.74, col = "white")
text(m[, 1] + m[, 2] / 2, b, paste0(p1(m[, 2]), "% against"), cex = 0.74,
     col = "white")
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Two bars whose percentages cross. The dollar amounts behind them are the
# thing that makes the reversal comprehensible -- the opposing total does not
# move at all, and the supporting total collapses -- so they are on the hover
# rather than in a separate table.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0('{lab:"', c("as filed", "16 rows removed"),
               '",sup:', fil$pct_supporting, ',opp:', fil$pct_opposing,
               ',supd:', round(fil$supporting), ',oppd:', round(fil$opposing),
               '}', collapse = ",")
cat(paste0('
<div id="fsb" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const ACC="', ACC, '", WARN="', WARN, '";
const W=770,H=250,M={t:16,r:24,b:40,l:170};
const box=d3.select("#fsb");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.lab)).range([M.t,H-M.b]).padding(0.4);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(5));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickSize(0)).selectAll("text").style("font-size","12px");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t-6)
  .attr("y2",H-M.b+4).attr("stroke","#12181D").attr("stroke-dasharray","3 3")
  .attr("opacity",0.55);
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const usd=v=>"$"+d3.format(",.0f")(v);
const segs=[];
D.forEach(function(d){
  segs.push({d:d,k:"sup",x0:0,   v:d.sup,amt:d.supd,c:ACC, lab:"supporting"});
  segs.push({d:d,k:"opp",x0:d.sup,v:d.opp,amt:d.oppd,c:WARN,lab:"opposing"});
});
svg.selectAll("rect.s").data(segs).join("rect").attr("class","s")
  .attr("x",s=>x(s.x0)).attr("width",s=>x(s.v)-x(0))
  .attr("y",s=>y(s.d.lab)).attr("height",y.bandwidth()).attr("fill",s=>s.c)
  .on("mousemove",function(e,s){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+s.d.lab+" &middot; "+s.lab+"</b><br>"+
             s.v.toFixed(1)+"% of the total<br>"+usd(s.amt));
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
svg.selectAll("text.s").data(segs).join("text").attr("class","s")
  .attr("x",s=>x(s.x0)+(x(s.v)-x(0))/2)
  .attr("y",s=>y(s.d.lab)+y.bandwidth()/2+4).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#fff")
  .text(s=>s.v>=12?s.v.toFixed(1)+"% "+(s.k==="sup"?"for":"against"):"");
})();
</script>'))

## ---- outtab
data.frame(Spender = head(out$spender, 6),
           On_behalf_of = head(out$on_behalf_of, 6),
           Amount = usd(head(out$amount, 6)))

## ---- disttab
data.frame(Quantity = dist$quantity,
           Value = ifelse(dist$unit == "dollars", usd(dist$value),
                   ifelse(dist$unit == "%", paste0(p1(dist$value), "%"),
                   ifelse(dist$unit == "ratio", paste0(p1(dist$value), "×"),
                          nn(dist$value)))))

## ---- regtab
data.frame(Filer = reg$who, Must_report = reg$must_report,
           What_stays_invisible = reg$what_is_invisible,
           Verified_first = reg$verified_before_publication)
