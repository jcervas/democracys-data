# ces-states-code.R -- chunk bodies for ces-states-brief.Rmd
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

S  <- read.csv("data/derived/states.csv", stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",  stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv", stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("checks.csv", "facts.csv", "states.csv"))

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)

knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# the smallest and largest state cells, for the worked margin-of-error arithmetic
VT <- S[S$name == F("state_min"), ][1, ]
TX <- S[S$name == F("state_max"), ][1, ]
VT_P   <- VT$est / 100                 # the estimate as a proportion
VT_PQ  <- VT_P * (1 - VT_P)            # p(1 - p)
VT_VAR <- VT_PQ / VT$n_eff             # over the effective sample
VT_SE  <- sqrt(VT_VAR)
VT_MOE <- 100 * 1.96 * VT_SE           # in points; matches VT$moe

OK  <- "#1b7837"    # the estimate's interval covers the result
BAD <- "#b2182b"    # it does not

## ---- cells-table
sm <- head(S[order(S$n), c("name", "n", "moe", "est", "actual", "error")], 5)
bg <- head(S[order(-S$n), c("name", "n", "moe", "est", "actual", "error")], 3)
data.frame(
  State        = c(sm$name, "…", bg$name),
  Respondents  = c(n(sm$n), "", n(bg$n)),
  `Margin ±`   = c(p1(sm$moe), "", p1(bg$moe)),
  `CES says`   = c(p1(sm$est), "", p1(bg$est)),
  `Result was` = c(p1(sm$actual), "", p1(bg$actual)),
  Error        = c(sprintf("%+.1f", sm$error), "", sprintf("%+.1f", bg$error)),
  check.names = FALSE)

## ---- grade-d3
# Built column-wise, not with apply(), which would coerce the frame to a
# padded character matrix.
rows <- sprintf('["%s",%d,%s,%s,%s]', S$name, S$n, S$moe, S$error,
                ifelse(as.logical(S$covers), "true", "false"))
cat(paste0('
<div id="cs"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', paste(rows, collapse = ","), '];
const box=d3.select("#cs");
const tip=box.append("div").attr("style","min-height:2.2em;font-size:0.9em");
const w=680,h=300,m={t:14,r:24,b:40,l:52};
const svg=box.append("svg").attr("viewBox","0 0 "+w+" "+h)
  .attr("style","max-width:100%;height:auto;font-family:inherit;font-size:12px");
const x=d3.scaleLog().domain([50,4200]).range([m.l,w-m.r]);
const y=d3.scaleLinear().domain([0,22]).range([h-m.b,m.t]);
svg.append("g").attr("transform","translate(0,"+(h-m.b)+")")
  .call(d3.axisBottom(x).ticks(5,"~s")).call(g=>g.select(".domain").remove());
svg.append("g").attr("transform","translate("+m.l+",0)")
  .call(d3.axisLeft(y).ticks(6)).call(g=>g.select(".domain").remove());
svg.append("text").attr("x",(m.l+w-m.r)/2).attr("y",h-4).attr("text-anchor","middle")
  .attr("fill","currentColor").text("respondents in the state (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(h-m.b)/2).attr("y",14)
  .attr("text-anchor","middle").attr("fill","currentColor")
  .text("points, absolute");
// the margin of error each state claims, and the error it actually made
D.forEach(d=>{
  svg.append("circle").attr("cx",x(d[1])).attr("cy",y(d[2])).attr("r",3)
    .attr("fill","none").attr("stroke","currentColor").attr("opacity",0.45);
  svg.append("circle").attr("cx",x(d[1])).attr("cy",y(Math.abs(d[3]))).attr("r",4.2)
    .attr("fill",d[4]?"', OK, '":"', BAD, '").attr("opacity",0.85)
    .style("cursor","pointer")
    .on("mouseover",()=>tip.html("<b>"+d[0]+"</b> &nbsp; "+d[1].toLocaleString()+
      " respondents &nbsp; margin ±"+d[2].toFixed(1)+
      " &nbsp; actual error "+(d[3]>0?"+":"")+d[3].toFixed(1)+
      (d[4]?" &nbsp; inside the margin":" &nbsp; <b>outside it</b>")));
});
svg.append("text").attr("x",w-m.r).attr("y",m.t+10).attr("text-anchor","end")
  .attr("fill","currentColor").attr("font-size","11px")
  .text("hollow = claimed margin,  filled = error actually made");
tip.html("Hover a state.");
})();
</script>'))

## ---- grade-static
par(mar = c(3.8, 4.0, 1.0, 1.0))
plot(S$n, abs(S$error), log = "x", type = "n", axes = FALSE,
     xlab = "", ylab = "", ylim = c(0, 22))
points(S$n, S$moe, pch = 1, cex = 0.7, col = "grey45")
points(S$n, abs(S$error), pch = 19, cex = 0.85,
       col = ifelse(S$covers == "TRUE" | S$covers == TRUE, OK, BAD))
axis(1, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
mtext("respondents in the state (log scale)", 1, line = 2.3, cex = 0.82)
mtext("points, absolute", 2, line = 2.5, cex = 0.82)
legend("topright", bty = "n", cex = 0.72, pch = c(1, 19), col = c("grey45", OK),
       legend = c("claimed margin", "error actually made"))

## ---- checks-table
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)
