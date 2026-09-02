# residual-votes-code.R -- chunk bodies for residual-votes-brief.Rmd
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
# FIPS must be read as text. A county FIPS is 5 digits and 268 of the 2,890 here
# begin with a zero (Alabama is state 01, California 06). Read as a number those
# lose the leading digit, print as 4-digit codes that name no state, and match no
# county boundary -- which is the exact failure the census-geography lab warns
# about, and a FIPS is printed in a table below.
co <- read.csv("data/derived/counties.csv",  stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
an <- read.csv("data/derived/anomalies.csv", stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
un <- read.csv("data/derived/unusable_states.csv", stringsAsFactors = FALSE)
u  <- co[co$state_usable, ]
an$gap <- an$total_votes - an$ballots
cook <- an[an$fips == "17031", ]     # the one impossible row the prose walks through
pc <- function(x, k = 2) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")
ex <- u[u$state_name == "Pennsylvania" & u$county_name == "Allegheny County", ]

# ---- geometry for the map, precomputed by data/build-brief-figures.R --------
# County outlines from Census TIGER 2020, projected, simplified, and packed as
# integer steps in a 1520-unit frame; see the header of that script. Small enough
# to keep the knitted document light and simple enough that base R draws them.
mc <- read.csv("data/derived/fig_map_counties.csv",    stringsAsFactors = FALSE,
               colClasses = c(fips = "character", pts = "character"))
ma <- read.csv("data/derived/fig_map_attr.csv",        stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
ml <- read.csv("data/derived/fig_map_state_lines.csv", stringsAsFactors = FALSE,
               colClasses = c(st = "character", pts = "character"))
ms <- read.csv("data/derived/fig_map_states.csv",      stringsAsFactors = FALSE,
               colClasses = c(st = "character"))
mf <- read.csv("data/derived/fig_map_frame.csv",       stringsAsFactors = FALSE)
mb <- read.csv("data/derived/fig_map_absent.csv",      stringsAsFactors = FALSE)
mm <- read.csv("data/derived/fig_map_meta.csv",        stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("anomalies.csv", "counties.csv", "fig_map_absent.csv", "fig_map_attr.csv", "fig_map_counties.csv", "fig_map_frame.csv", "fig_map_meta.csv", "fig_map_state_lines.csv", "fig_map_states.csv", "states.csv", "unusable_states.csv"))
mv <- function(k) mm$value[mm$key == k]
mn <- function(k) as.numeric(mv(k))

# "x0 y0 dx dy ..." -> absolute coordinates
mdec <- function(p) {
  v <- as.integer(strsplit(p, " ", fixed = TRUE)[[1]])
  list(x = cumsum(v[c(TRUE, FALSE)]), y = cumsum(v[c(FALSE, TRUE)]))
}
MW <- as.integer(mn("frame_w")); MH <- as.integer(mn("frame_h"))
MTOP <- 150L                                   # legend band above the map
MRAMP <- c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
MBLAB <- c("under 0.5", "0.5-1", "1-1.5", "1.5-2.5", "2.5-5", "5 and over")
MIMP <- "#C41230"; MSET <- "#9c9c9c"; MNOD <- "#e8e8e8"
ma$col <- ifelse(ma$class == "ok",         MRAMP[ma$bin],
          ifelse(ma$class == "impossible", MIMP,
          ifelse(ma$class == "setaside",   MSET, MNOD)))
ma$state <- ms$state[match(substr(ma$fips, 1, 2), ms$st)]
mcol <- ma$col[match(mc$fips, ma$fips)]
mimp <- ma$class[match(mc$fips, ma$fips)] == "impossible"
stopifnot(!any(is.na(mcol)))
# recomputed here rather than trusted from the build script
MOK  <- sum(ma$class == "ok");        MIM <- sum(ma$class == "impossible")
MSE  <- sum(ma$class == "setaside");  MNO <- sum(ma$class == "nodata")
stopifnot(MIM == nrow(an), MSE == sum(!co$state_usable), MOK == nrow(u))

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

## ---- butterfly-d3
cat('
<style>
/* Part of this figure is drawn ON the ballot -- the cream pages and the
   punch-hole boxes -- and that paper stays paper when the page goes dark.
   brief.css lifts dark text fills for the dark page, which is right for
   the annotations underneath but wrong on the paper, where it put
   near-white names on a cream page at 1.2:1.

   So the on-paper marks, and only those, pin the tokens back to their
   light values. The rule in brief.css still fires; it just resolves to
   what was drawn. Scoping this to #bfly as a whole was the first attempt
   and it was too wide: the two lines of commentary below the ballot sit
   on the dark page, not on the paper, and stayed dim. */
#bfly text.on-paper{--ink:#12181D;--ink-2:#4E5A63;--ink-3:#76838C;--map-gop:#C41230}
/* Tertiary ink by name rather than by hex, so it follows the page off the
   paper and resolves to the light value on it. No fill attribute: if this
   rule ever fails to match, brief.css floors the text at currentColor,
   which is readable -- a hex that fails is not. */
#bfly .mut{fill:var(--ink-3)}
</style>
<div id="bfly" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const W=760,H=372;
const svg=d3.select("#bfly").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit;display:block");
svg.append("defs").append("marker").attr("id","bar").attr("viewBox","0 0 10 10")
  .attr("refX",9).attr("refY",5).attr("markerWidth",6).attr("markerHeight",6)
  .attr("orient","auto-start-reverse").append("path").attr("d","M0,1L9,5L0,9Z")
  .attr("fill","#555");
// the two facing pages
[[18,"left"],[426,"right"]].forEach(p=>{
  svg.append("rect").attr("x",p[0]).attr("y",26).attr("width",316).attr("height",250)
    .attr("fill","#fdfcf8").attr("stroke","#bbb").attr("stroke-width",1.2);
});
svg.append("text").attr("x",176).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#888").text("left page");
svg.append("text").attr("x",584).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#888").text("right page");
svg.append("text").attr("x",380).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#888").text("punch holes");
const YS=[70,118,166,214,254];
// the center column of punch holes, numbered by their position down the column
YS.forEach((y,i)=>{
  const hot=(i===1);
  svg.append("rect").attr("x",364).attr("y",y-10).attr("width",32).attr("height",20).attr("rx",3)
    .attr("fill",hot?"#fbeaed":"#f2f2f2").attr("stroke",hot?"#C41230":"#999")
    .attr("stroke-width",hot?2.4:1);
  svg.append("text").attr("x",380).attr("y",y+4).attr("text-anchor","middle")
    .attr("class","on-paper")
    .attr("font-size","11.5px").attr("font-weight",hot?"700":"400")
    .attr("fill",hot?"#C41230":"#666").text(i+1);
});
function row(side,y,ord,party,name,mate,ordCol){
  const L=(side==="L");
  const tx=L?36:724, ax0=L?286:474, ax1=L?358:402;
  // The unhighlighted ordinal was #999 on the cream page: 2.78:1, and dim in
  // print too. It takes the tertiary ink instead, which on paper is 3.8:1.
  if(ord){
    const o=svg.append("text").attr("x",L?26:734).attr("y",y+4)
      .attr("class","on-paper"+(ordCol?"":" mut"))
      .attr("text-anchor",L?"start":"end").attr("font-size","12px")
      .attr("font-weight","700").text(ord);
    if(ordCol) o.attr("fill",ordCol);
  }
  svg.append("text").attr("x",tx).attr("y",y-4).attr("text-anchor",L?"start":"end")
    .attr("class","on-paper")
    .attr("font-size","10.5px").attr("letter-spacing","0.06em").attr("fill","#777").text(party);
  svg.append("text").attr("x",tx).attr("y",y+11).attr("text-anchor",L?"start":"end")
    .attr("class","on-paper")
    .attr("font-size","13px").attr("font-weight","600").attr("fill","#222").text(name);
  svg.append("text").attr("x",tx+(L?1:-1)*(name.length*7.6+8)).attr("y",y+11)
    .attr("text-anchor",L?"start":"end").attr("class","on-paper")
    .attr("font-size","11px").attr("fill","#777").text(mate);
  svg.append("line").attr("x1",ax0).attr("y1",y).attr("x2",ax1).attr("y2",y)
    .attr("stroke","#555").attr("stroke-width",1.2).attr("marker-end","url(#bar)");
}
row("L",YS[0],"1","REPUBLICAN","George W. Bush","Dick Cheney");
row("R",YS[1],null,"REFORM","Pat Buchanan","Ezola Foster");
row("L",YS[2],"2","DEMOCRATIC","Al Gore","Joe Lieberman","#C41230");
[3,4].forEach(i=>{
  [[36,"L"],[724,"R"]].forEach(p=>{
    svg.append("line").attr("x1",p[1]==="L"?36:560).attr("y1",YS[i]+2)
      .attr("x2",p[1]==="L"?200:724).attr("y2",YS[i]+2)
      .attr("stroke","#d6d6d6").attr("stroke-width",7);
    svg.append("line").attr("x1",p[1]==="L"?286:474).attr("y1",YS[i])
      .attr("x2",p[1]==="L"?358:402).attr("y2",YS[i])
      .attr("stroke","#ccc").attr("stroke-width",1.2);
  });
});
svg.append("text").attr("x",176).attr("y",290).attr("text-anchor","middle")
  .attr("font-size","11px").attr("class","mut")
  .text("seven more tickets, still alternating page to page");
// what the geometry does
svg.append("text").attr("x",18).attr("y",324).attr("font-size","13px")
  .attr("font-weight","700").attr("fill","#C41230")
  .text("Gore is 2nd on the left page. The 2nd hole belongs to Buchanan.");
svg.append("text").attr("x",18).attr("y",344).attr("font-size","12px").attr("fill","#444")
  .text("Punch hole 2 and the ballot carries a counted vote — for Buchanan.");
svg.append("text").attr("x",18).attr("y",362).attr("font-size","12px").attr("fill","#444")
  .text("Punch hole 2 and then hole 3, and the ballot carries no presidential vote at all.");
})();
</script>
')

## ---- butterfly-static
par(mar = c(0.2, 0.2, 0.2, 0.2))
W <- 760; H <- 372
YF <- function(y) H - y                       # SVG y (down) -> base R y (up)
plot(NA, xlim = c(0, W), ylim = c(0, H), asp = 1, axes = FALSE, ann = FALSE)
for (x0 in c(18, 426))
  rect(x0, YF(276), x0 + 316, YF(26), col = "#fdfcf8", border = "#bbb", lwd = 1)
text(176, YF(18), "left page",  cex = 0.55, col = "#888")
text(584, YF(18), "right page", cex = 0.55, col = "#888")
text(380, YF(18), "punch holes", cex = 0.55, col = "#888")
YS <- c(70, 118, 166, 214, 254)
for (i in seq_along(YS)) {
  hot <- i == 2
  rect(364, YF(YS[i] + 10), 396, YF(YS[i] - 10),
       col = if (hot) "#fbeaed" else "#f2f2f2",
       border = if (hot) "#C41230" else "#999", lwd = if (hot) 1.8 else 0.7)
  text(380, YF(YS[i]), i, cex = 0.58, font = if (hot) 2 else 1,
       col = if (hot) "#C41230" else "#666")
}
# #76838C is the tertiary ink the screen figure resolves --ink-3 to on paper.
# It replaces #999, which was 2.8:1 on the cream page in both media.
brow <- function(side, y, ord, party, name, mate, ordcol = "#76838C") {
  L <- side == "L"; tx <- if (L) 36 else 724; adj <- if (L) 0 else 1
  if (!is.null(ord))
    text(if (L) 26 else 734, YF(y), ord, cex = 0.6, font = 2, col = ordcol, adj = adj)
  text(tx, YF(y - 6), party, cex = 0.5,  col = "#777", adj = adj)
  text(tx, YF(y + 9), name,  cex = 0.63, font = 2, col = "#222", adj = adj)
  text(tx + (if (L) 1 else -1) * (nchar(name) * 7.6 + 8), YF(y + 9), mate,
       cex = 0.53, col = "#777", adj = adj)
  arrows(if (L) 286 else 474, YF(y), if (L) 358 else 402, YF(y),
         length = 0.05, angle = 22, lwd = 1, col = "#555")
}
brow("L", YS[1], "1", "REPUBLICAN", "George W. Bush", "Dick Cheney")
brow("R", YS[2], NULL, "REFORM", "Pat Buchanan", "Ezola Foster")
brow("L", YS[3], "2", "DEMOCRATIC", "Al Gore", "Joe Lieberman", "#C41230")
for (i in 4:5) {
  segments(c(36, 560), YF(YS[i] + 2), c(200, 724), YF(YS[i] + 2), col = "#d6d6d6", lwd = 4)
  segments(c(286, 474), YF(YS[i]), c(358, 402), YF(YS[i]), col = "#ccc", lwd = 1)
}
text(176, YF(290), "seven more tickets, still alternating page to page",
     cex = 0.5, col = "#76838C")
text(18, YF(324), "Gore is 2nd on the left page. The 2nd hole belongs to Buchanan.",
     cex = 0.62, font = 2, col = "#C41230", adj = 0)
text(18, YF(344), "Punch hole 2 and the ballot carries a counted vote - for Buchanan.",
     cex = 0.57, col = "#444", adj = 0)
text(18, YF(362),
     "Punch hole 2 and then hole 3, and the ballot carries no presidential vote at all.",
     cex = 0.57, col = "#444", adj = 0)

## ---- sources
data.frame(
  `we need` = c("Ballots cast", "Presidential votes counted"),
  `who has it` = c("Election Assistance Commission (EAVS), item F1a",
                   "State canvasses, compiled into county returns"),
  `what it is` = c("An administrative survey of every election office",
                   "The official count, by county"),
  check.names = FALSE)

## ---- one-row
o <- ex[, c("state_name", "county_name", "fips", "ballots", "total_votes",
            "residual", "residual_rate")]
names(o) <- c("state", "county", "FIPS", "ballots cast (EAVS)",
              "presidential votes", "residual", "residual rate (%)")
o

## ---- summary
data.frame(
  statistic = c("Counties with a usable comparison",
                "Median county residual rate (%)",
                "25th percentile (%)", "75th percentile (%)",
                "Aggregate across all usable counties (%)"),
  value = c(n(nrow(u)), pc(median(u$residual_rate)),
            pc(quantile(u$residual_rate, .25)),
            pc(quantile(u$residual_rate, .75)),
            pc(100 * (sum(u$ballots) - sum(u$total_votes)) / sum(u$ballots))))

## ---- anomalies
h <- head(an[order(-an$gap), c("state_name", "county_name", "ballots", "total_votes", "gap")], 6)
h$ballots <- n(h$ballots); h$total_votes <- n(h$total_votes); h$gap <- n(h$gap)
names(h) <- c("state", "county", "ballots (EAVS)", "presidential votes", "excess votes")
h

## ---- floor-setup
FLO <- -8; FHI <- 6
fh   <- hist(u$residual_rate[u$residual_rate <= FHI], breaks = seq(0, FHI, 0.2),
             plot = FALSE)
ffar <- sum(an$residual_rate < FLO)
fwi  <- which.min(an$residual_rate)
set.seed(4); fjit <- runif(nrow(an))          # one jitter, shared by both renderers
fx   <- pmax(an$residual_rate, FLO + 0.15)    # clipped, and the caption says so

## ---- floor-d3
bars <- paste(sprintf('{"a":%.2f,"b":%.2f,"n":%d}', fh$breaks[-length(fh$breaks)],
                      fh$breaks[-1], fh$counts), collapse = ",")
dots <- paste(sprintf('{"x":%.3f,"j":%.3f,"c":"%s","s":"%s","r":%.2f}',
                      fx, fjit, gsub('"', "", an$county_name),
                      gsub('"', "", an$state_name), an$residual_rate), collapse = ",")
cat(sprintf('
<div id="flr" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const B=[%s],A=[%s],LO=%f,HI=%f,MED=%f;
const W=760,H=340,M={t:34,r:20,b:52,l:20};
const svg=d3.select("#flr").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([LO,HI]).range([M.l,W-M.r]);
const YB=H-M.b-72, mx=d3.max(B,d=>d.n);
const y=d3.scaleLinear().domain([0,mx]).range([YB,M.t]);
svg.append("rect").attr("x",x(LO)).attr("y",M.t-22).attr("width",x(0)-x(LO))
  .attr("height",H-M.b-M.t+22).attr("fill","#fbeaed");
svg.selectAll("rect.h").data(B).join("rect").attr("class","h")
  .attr("x",d=>x(d.a)).attr("y",d=>y(d.n)).attr("width",d=>x(d.b)-x(d.a))
  .attr("height",d=>YB-y(d.n)).attr("fill","#2c7fb8").attr("stroke","#fff")
  .attr("stroke-width",0.4);
svg.append("line").attr("x1",x(MED)).attr("y1",M.t-6).attr("x2",x(MED)).attr("y2",YB)
  .attr("stroke","#C41230").attr("stroke-width",2);
svg.append("text").attr("x",x(MED)+6).attr("y",M.t+2).attr("font-size","11px")
  .attr("font-weight","600").attr("fill","#C41230").text("median %s%%");
svg.append("line").attr("x1",x(0)).attr("y1",M.t-22).attr("x2",x(0)).attr("y2",H-M.b)
  .attr("stroke","#111").attr("stroke-width",2.6);
svg.append("text").attr("x",x(0)-6).attr("y",M.t+56).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#111")
  .text("the arithmetic floor");
svg.append("text").attr("x",x(LO)+8).attr("y",M.t+2).attr("font-size","11.5px")
  .attr("font-weight","600").attr("fill","#C41230")
  .text("%d counties report MORE presidential votes than ballots");
svg.append("text").attr("x",x(LO)+8).attr("y",M.t+18).attr("font-size","10.5px")
  .attr("fill","#a04").text("%d of them fall left of this frame; the worst is %s, %s, at %s%%");
const tip=d3.select("#flr").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("circle").data(A).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>H-M.b-14-d.j*30).attr("r",3.4)
  .attr("fill","#C41230").attr("fill-opacity",0.8)
  .on("mousemove",function(e,d){tip.style("opacity",1)
    .html(`<b>${d.c}, ${d.s}</b><br>${d.r.toFixed(1)}%% residual rate`)
    .style("left",Math.min(e.offsetX+14,W-200)+"px").style("top",(e.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#444")
  .text("residual vote rate, %% of ballots cast");
svg.append("text").attr("x",W-M.r-6).attr("y",M.t+40).attr("text-anchor","end")
  .attr("font-size","11px").attr("font-weight","600").attr("fill","#2c7fb8")
  .text("%s counties that pass");
})();
</script>
', bars, dots, FLO, FHI, median(u$residual_rate), pc(median(u$residual_rate)),
   nrow(an), ffar, gsub('"', "", an$county_name[fwi]), gsub('"', "", an$state_name[fwi]),
   pc(an$residual_rate[fwi], 1), n(nrow(u))))

## ---- floor-static
par(mar = c(3.8, 0.6, 2.4, 0.6))
plot(NA, xlim = c(FLO, FHI), ylim = c(-0.42, 1.06), axes = FALSE, ann = FALSE, yaxs = "i")
rect(FLO - 1, -0.42, 0, 1.06, col = "#fbeaed", border = NA)
sc <- 0.86 / max(fh$counts)
for (i in seq_along(fh$counts))
  rect(fh$breaks[i], 0, fh$breaks[i+1], fh$counts[i] * sc,
       col = "#2c7fb8", border = "white", lwd = 0.4)
abline(v = median(u$residual_rate), col = "#C41230", lwd = 2)
text(median(u$residual_rate), 0.94, sprintf("median %s%%", pc(median(u$residual_rate))),
     cex = 0.6, col = "#C41230", font = 2, pos = 4, offset = 0.3)
segments(0, -0.42, 0, 1.06, lwd = 2.6, col = "#111")
text(0, 0.74, "the arithmetic floor", cex = 0.62, font = 2, col = "#111",
     pos = 2, offset = 0.35)
text(FLO + 0.15, 0.98,
     sprintf("%d counties report MORE presidential votes than ballots", nrow(an)),
     cex = 0.6, col = "#C41230", font = 2, pos = 4, offset = 0)
text(FLO + 0.15, 0.89,
     sprintf("%d of them fall left of this frame; the worst is %s, %s, at %s%%",
             ffar, an$county_name[fwi], an$state_name[fwi], pc(an$residual_rate[fwi], 1)),
     cex = 0.52, col = "#a04", pos = 4, offset = 0)
points(fx, -0.10 - fjit * 0.26, pch = 19, cex = 0.55, col = adjustcolor("#C41230", 0.8))
text(FHI, 0.55, sprintf("%s counties\nthat pass", n(nrow(u))), cex = 0.6,
     col = "#2c7fb8", font = 2, pos = 2, offset = 0.4)
axis(1, at = seq(FLO, FHI, 2), labels = paste0(seq(FLO, FHI, 2), "%"), cex.axis = 0.7)
mtext("residual vote rate, % of ballots cast", side = 1, line = 2.2, cex = 0.72)

## ---- map-d3
rg <- paste(sprintf('["%s","%s"]', mc$fips, mc$pts), collapse = ",")
at <- paste(sprintf('"%s":["%s",%d,"%s",%s,%s]',
              ma$fips, gsub('"', "", ma$name),
              match(substr(ma$fips, 1, 2), ms$st) - 1L, ma$class,
              ifelse(is.na(ma$bin), "null", ma$bin),
              ifelse(is.na(ma$rate), "null", sprintf("%.3f", ma$rate))),
            collapse = ",")
sn <- paste(sprintf('"%s"', gsub('"', "", ms$state)), collapse = ",")
sl <- paste(sprintf('"%s"', ml$pts), collapse = ",")
lg <- paste(sprintf('["%s","%s"]', MRAMP, MBLAB), collapse = ",")
bx <- paste(sprintf('{"n":"%s","x":%d,"y":%d,"w":%d,"h":%d}',
                    mf$piece, mf$x, mf$y, mf$w, mf$h), collapse = ",")
cat(sprintf('
<div id="usmap" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const RG=[%s], AT={%s}, SN=[%s], SL=[%s], LG=[%s], BX=[%s];
const W=%d, MH=%d, TOP=%d, H=MH+TOP;
const IMP="%s", SET="%s", NOD="%s";
const svg=d3.select("#usmap").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit;display:block");
function path(p){
  const v=p.split(" ");let x=0,y=0,s="";
  for(let i=0;i<v.length;i+=2){x+=+v[i];y+=+v[i+1];
    s+=(i?"L":"M")+x+","+(H-y);}
  return s+"Z";
}
const colOf=a=>a[2]==="ok"?LG[a[3]-1][0]:a[2]==="impossible"?IMP:a[2]==="setaside"?SET:NOD;
const g=svg.append("g");
const norm=RG.filter(r=>AT[r[0]][2]!=="impossible");
const imp =RG.filter(r=>AT[r[0]][2]==="impossible");
g.selectAll("path.c").data(norm).join("path").attr("class","c")
  .attr("d",r=>path(r[1])).attr("fill",r=>colOf(AT[r[0]])).attr("stroke","none");
svg.append("g").selectAll("path").data(SL).join("path")
  .attr("d",path).attr("fill","none").attr("stroke","#fff").attr("stroke-width",1)
  .attr("pointer-events","none");
const gi=svg.append("g");
gi.selectAll("path").data(imp).join("path")
  .attr("d",r=>path(r[1])).attr("fill",IMP).attr("stroke","#6d0a19").attr("stroke-width",0.8);
BX.forEach(b=>{
  svg.append("rect").attr("x",b.x-6).attr("y",H-b.y-b.h-6).attr("width",b.w+12)
    .attr("height",b.h+12).attr("fill","none").attr("stroke","#c8c8c8").attr("stroke-width",1);
  svg.append("text").attr("x",b.x-6).attr("y",H-b.y-b.h-12).attr("font-size","15px")
    .attr("fill","#777").text(b.n+" \\u2014 inset, not to scale");
});
// ---- legend, drawn in the band above the map ----
const L=svg.append("g");
L.append("text").attr("x",30).attr("y",40).attr("font-size","22px")
  .attr("font-weight","600").attr("fill","#333")
  .text("residual vote rate, %% of ballots cast");
LG.forEach((d,i)=>{
  L.append("rect").attr("x",30+i*104).attr("y",52).attr("width",100).attr("height",24)
    .attr("fill",d[0]).attr("stroke","#fff");
  L.append("text").attr("x",80+i*104).attr("y",96).attr("text-anchor","middle")
    .attr("font-size","17px").attr("fill","#555").text(d[1]);
});
[[IMP,"%s counties: more presidential votes than ballots"],
 [SET,"%s counties in the three states set aside below"],
 [NOD,"%s counties that never entered the join at all"]].forEach((d,i)=>{
  L.append("rect").attr("x",710).attr("y",30+i*34).attr("width",28).attr("height",22)
    .attr("fill",d[0]).attr("stroke",d[0]===IMP?"#6d0a19":"#fff");
  L.append("text").attr("x",750).attr("y",48+i*34).attr("font-size","18px")
    .attr("fill","#444").text(d[1]);
});
const box=d3.select("#usmap");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const TXT={ok:a=>`${a[4].toFixed(2)}%% residual rate`,
  impossible:a=>`<span style="color:#ff9aa8">${a[4].toFixed(1)}%% \\u2014 more `+
    `presidential votes than ballots</span>`,
  setaside:a=>`set aside with the rest of ${SN[a[1]]}`,
  nodata:a=>"no row in either source file"};
function hov(e,r){
  if(!Array.isArray(r)) return;
  const a=AT[r[0]];
  tip.style("opacity",1).html(`<b>${a[0]}, ${SN[a[1]]}</b><br>${TXT[a[2]](a)}`)
    .style("left",Math.min(e.offsetX+14,box.node().clientWidth-240)+"px")
    .style("top",(e.offsetY-8)+"px");
}
g.selectAll("path").on("mousemove",hov).on("mouseleave",()=>tip.style("opacity",0));
gi.selectAll("path").on("mousemove",hov).on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s counties, from Census TIGER 2020 boundaries. Hover any county. County
outlines are simplified to about ten vertices each; nothing in the argument
rests on the shape of a county line.</p>
', rg, at, sn, sl, lg, bx, MW, MH, MTOP, MIMP, MSET, MNOD,
   n(MIM), n(MSE), n(MNO), n(nrow(ma))))

## ---- map-static
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0, MW), ylim = c(0, MH + MTOP), asp = 1, axes = FALSE, ann = FALSE)
for (i in which(!mimp)) { z <- mdec(mc$pts[i]); polygon(z$x, z$y, col = mcol[i], border = NA) }
for (i in seq_len(nrow(ml))) {
  z <- mdec(ml$pts[i]); polygon(z$x, z$y, col = NA, border = "#ffffff", lwd = 0.5)
}
# the impossible counties last and outlined, so a single small county still reads
for (i in which(mimp)) {
  z <- mdec(mc$pts[i]); polygon(z$x, z$y, col = MIMP, border = "#6d0a19", lwd = 0.35)
}
rect(mf$x - 6, mf$y - 6, mf$x + mf$w + 6, mf$y + mf$h + 6, border = "#c8c8c8", lwd = 0.7)
text(mf$x - 6, mf$y + mf$h + 12, paste(mf$piece, "- inset, not to scale"),
     cex = 0.45, col = "#777", adj = 0)
text(30, MH + MTOP - 34, "residual vote rate, % of ballots cast",
     cex = 0.6, font = 2, col = "#333", adj = 0)
for (i in seq_along(MRAMP)) {
  rect(30 + (i-1)*104, MH + MTOP - 76, 130 + (i-1)*104, MH + MTOP - 52,
       col = MRAMP[i], border = "#fff", lwd = 0.5)
  text(80 + (i-1)*104, MH + MTOP - 98, MBLAB[i], cex = 0.47, col = "#555")
}
key <- rbind(
  c(MIMP, sprintf("%s counties: more presidential votes than ballots", n(MIM))),
  c(MSET, sprintf("%s counties in the three states set aside below", n(MSE))),
  c(MNOD, sprintf("%s counties that never entered the join at all",     n(MNO))))
for (i in 1:3) {
  yy <- MH + MTOP - 52 - (i-1)*34
  rect(710, yy, 738, yy + 22, col = key[i, 1],
       border = if (i == 1) "#6d0a19" else "#fff", lwd = 0.5)
  text(750, yy + 11, key[i, 2], cex = 0.5, col = "#444", adj = 0)
}

## ---- unusable
o <- un
names(o) <- c("state", "median rate (%)", "why it was set aside")
o

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#flr text[fill="#111" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
cat('<style>
#flr text[fill="#c41230" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
</style>')
