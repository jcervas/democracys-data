# media-ideology-code.R -- chunk bodies for media-ideology-brief.Rmd
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

O  <- read.csv("data/derived/outlets.csv",     stringsAsFactors = FALSE)
PL <- read.csv("data/derived/politicians.csv", stringsAsFactors = FALSE)
FA <- read.csv("data/derived/facts.csv",       stringsAsFactors = FALSE)
CK <- read.csv("data/derived/checks.csv",      stringsAsFactors = FALSE)

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(x, big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
r2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
K  <- O[O$kept == "TRUE" | O$kept == TRUE, ]

DEM <- "#2166ac"; REP <- "#b2182b"; GREY <- "#9e9e9e"

## ---- scale-d3
KK <- O[order(O$score), ]
row <- function(r) sprintf('["%s",%s,%s,%s,%s]', r[["domain"]], r[["score"]],
                           r[["shares"]], r[["top_sharer_pct"]],
                           tolower(as.character(r[["kept"]])))
cat(paste0('
<div id="ms"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', paste(apply(KK, 1, row), collapse = ","), '];
const box=d3.select("#ms");
const ctl=box.append("div").attr("style","margin:0.2em 0 0.5em 0;font-size:0.92em");
let hideThin=false, showCut=false;
const cb=(lab,init,fn)=>{const l=ctl.append("label")
   .attr("style","margin-right:1.1em;cursor:pointer");
  l.append("input").attr("type","checkbox").property("checked",init)
   .attr("style","margin-right:0.4em").on("change",function(){fn(this.checked);draw();});
  l.append("span").text(lab);};
cb("hide outlets where one politician is over half the shares",false,v=>hideThin=v);
cb("show the outlets below the share cut",false,v=>showCut=v);
const w=680,h=250,m={t:18,r:20,b:40,l:20};
const svg=box.append("svg").attr("viewBox","0 0 "+w+" "+h)
  .attr("style","max-width:100%;height:auto;font-family:inherit;font-size:12px");
const x=d3.scaleLinear().domain([-0.6,0.85]).range([m.l+18,w-m.r-18]);
const rr=d3.scaleSqrt().domain([1,d3.max(D,d=>d[2])]).range([2,17]);
svg.append("g").attr("transform","translate(0,"+(h-m.b)+")")
  .call(d3.axisBottom(x).ticks(7))
  .call(g=>g.select(".domain").remove());
svg.append("text").attr("x",m.l+18).attr("y",h-m.b+34).attr("fill","', DEM, '")
  .attr("font-size","11px").text("shared by liberals");
svg.append("text").attr("x",w-m.r-18).attr("y",h-m.b+34).attr("text-anchor","end")
  .attr("fill","', REP, '").attr("font-size","11px").text("shared by conservatives");
const tip=box.append("div").attr("style",
  "min-height:2.6em;margin-top:0.3em;font-size:0.9em;color:#333");
const g=svg.append("g");
function draw(){
  let d=D.filter(v=>showCut? true : v[4]);
  if(hideThin) d=d.filter(v=>v[3]<=50);
  const sim=d3.forceSimulation(d.map(v=>({v:v,x:x(v[1]),y:(h-m.b)/2+m.t/2})))
    .force("x",d3.forceX(p=>x(p.v[1])).strength(1))
    .force("y",d3.forceY((h-m.b)/2).strength(0.06))
    .force("c",d3.forceCollide(p=>rr(p.v[2])+1.2))
    .stop();
  for(let i=0;i<140;i++) sim.tick();
  const nodes=sim.nodes();
  const sel=g.selectAll("circle").data(nodes,p=>p.v[0]);
  sel.exit().remove();
  sel.enter().append("circle").merge(sel)
    .attr("cx",p=>p.x).attr("cy",p=>p.y).attr("r",p=>rr(p.v[2]))
    .attr("fill",p=>p.v[4]? (p.v[1]<0?"', DEM, '":"', REP, '") : "', GREY, '")
    .attr("opacity",p=>p.v[3]>50?0.45:0.8)
    .attr("stroke",p=>p.v[3]>50?"#000":"none").attr("stroke-dasharray","2,2")
    .style("cursor","pointer")
    .on("mouseover",(e,p)=>tip.html("<b>"+p.v[0]+"</b> &nbsp; score "+
      p.v[1].toFixed(3)+" &nbsp; "+p.v[2].toLocaleString()+" shares &nbsp; biggest sharer "+
      p.v[3].toFixed(1)+"% of them"+(p.v[4]?"":" &nbsp; <i>below the cut</i>")));
  tip.html(hideThin? "Outlets where a single politician contributes more than half the links are hidden."
                   : "Hover a circle. Dashed outlines rest on one politician for more than half their shares.");
}
draw();
})();
</script>'))

## ---- scale-static
par(mar = c(3.6, 1, 1, 1))
kk <- K[order(K$score), ]
set.seed(84355)
yy <- runif(nrow(kk), -1, 1)
plot(kk$score, yy, type = "n", axes = FALSE, xlab = "", ylab = "",
     xlim = c(-0.6, 0.85), ylim = c(-1.6, 1.6))
abline(v = 0, col = "grey70", lty = 3)
symbols(kk$score, yy, circles = sqrt(kk$shares), inches = 0.17, add = TRUE,
        bg = ifelse(kk$top_sharer_pct > 50, "grey75",
             ifelse(kk$score < 0, DEM, REP)),
        fg = ifelse(kk$top_sharer_pct > 50, "black", NA))
axis(1, cex.axis = 0.85)
lab <- kk[kk$domain %in% c("nytimes.com", "foxnews.com", F("left_most"), F("right_most")), ]
text(lab$score, yy[match(lab$domain, kk$domain)] + 0.55, lab$domain,
     cex = 0.68, col = "#222")
mtext("shared by liberals", side = 1, line = 2.1, adj = 0, cex = 0.72, col = DEM)
mtext("shared by conservatives", side = 1, line = 2.1, adj = 1, cex = 0.72, col = REP)

## ---- check-static
par(mar = c(3.8, 3.8, 1, 1))
plot(PL$nominate, PL$media_score, pch = 19, cex = 0.5,
     col = ifelse(PL$party == "Democrat", paste0(DEM, "cc"),
           ifelse(PL$party == "Republican", paste0(REP, "cc"), "#66666699")),
     xlab = "", ylab = "", axes = FALSE)
axis(1, cex.axis = 0.82); axis(2, las = 1, cex.axis = 0.82)
mtext("DW-NOMINATE, from roll-call votes", 1, line = 2.3, cex = 0.85)
mtext("media score, from shared links", 2, line = 2.6, cex = 0.85)
abline(lm(media_score ~ nominate, data = PL), col = "#333", lwd = 1.4)

## ---- checks-table
knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))
data.frame(Check = CK$check, Passed = ifelse(CK$passed, "yes", "NO"),
           check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- worked
# The arithmetic behind one outlet's score, done in the open. It reads the
# complete file in raw/, because the derived tables carry the answer and not
# the sharers behind it. Same rule as the build: people with no roll-call
# score are left out, and the average is weighted by share count.
e <- new.env(); load("data/raw/PolShares.RData", envir = e)
P <- as.data.frame(get("PolShares", envir = e), stringsAsFactors = FALSE)
wd <- F("worst_domain")
wk <- P[!is.na(P$nominate) & !is.na(P[[wd]]) & P[[wd]] > 0,
        c("name", "party", "nominate", wd)]
names(wk)[4] <- "shares"
wk <- wk[order(-wk$shares, -wk$nominate), ]
WK_TOT   <- sum(wk$shares)
WK_SCORE <- sum(wk$nominate * wk$shares) / WK_TOT
WK_TOP   <- wk$name[1]; WK_TOPN <- wk$shares[1]
WK_TOPNOM <- formatC(wk$nominate[1], format = "f", digits = 3)
WK_SCORE_NOTOP <- with(wk[-1, ], sum(nominate * shares) / sum(shares))
knit_print.data.frame <- function(x, ...) {
  nm <- sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))
data.frame(Politician = wk$name, Party = wk$party,
           Roll_call_score = formatC(wk$nominate, format = "f", digits = 3),
           Shares = as.integer(wk$shares),
           Score_times_shares = formatC(wk$nominate * wk$shares,
                                        format = "f", digits = 1))
