# whole-foods-cracker-barrel-code.R -- chunk bodies for whole-foods-cracker-barrel-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.4,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f, ...) read.csv(file.path("data/derived", f),
                                   stringsAsFactors = FALSE, ...)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

gap   <- rd("gap.csv")
cats  <- rd("categories.csv")
alt   <- rd("alternatives.csv")
inv   <- rd("inventory_check.csv")
p20   <- rd("published_2020.csv")
anach <- rd("anachronism.csv")

n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# --- palette ---------------------------------------------------------------
# The two brands are the whole subject and a reader will map them onto parties
# within a second of seeing any colour at all. Fighting that would be futile,
# so the pair is a muted blue and a muted red -- close enough to read as the
# parties everybody is already thinking of, far enough from full saturation
# that no figure looks like an election map. The third colour is for the
# category the metric leaves out, and it is deliberately the darkest thing on
# the page, because that is the chapter's argument.
WFC   <- "#4E79A7"   # Whole Foods
CBC   <- "#C0625A"   # Cracker Barrel
NONE  <- "#3B3B3B"   # neither store
BOTHC <- "#8E7CA8"   # both
NEUTC <- "#999999"
REFC  <- "#C41230"   # a published figure, a reference line

subcap <- function(txt, width = 100, line = 3.4, cex = 0.66) {
  cw <- strwrap(txt, width = width)
  mtext(cw, side = 1, line = line + (seq_along(cw) - 1) * 0.95, adj = 0,
        cex = cex, col = "#555555")
}

cap_gap <- paste0(
  "The metric on two different units, 2008-2024. Counties above, votes below.")
cap_cat <- paste0(
  "The four store categories in 2024, by share of counties and share of votes.")

.hdr <- function(x) sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)

knit_print.data.frame <- function(x, ...) {
  knitr::knit_print(knitr::kable(x, col.names = .hdr(x), row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

nobreak <- function(x) {
  if (knitr::is_latex_output())
    knitr::kable(x, format = "latex", booktabs = TRUE, longtable = FALSE,
                 linesep = "", col.names = .hdr(x), row.names = FALSE,
                 align = table_align(x))
  else x
}

g24 <- gap[gap$year == 2024, ]
g08 <- gap[gap$year == 2008, ]

## ---- inventory
o <- inv[, c("source", "brand", "stores", "as_of", "kind")]
names(o) <- c("source", "brand", "stores", "as of", "what it is")
nobreak(o)

## ---- replicate
o <- data.frame(
  category = p20$category,
  counties = p20$counties,
  published = paste0(pc(p20$published, 0), "%"),
  recomputed = paste0(pc(p20$recomputed, 1), "%"),
  difference = sprintf("%+.1f", p20$drift))
names(o) <- c("counties with", "how many", "Wasserman published",
              "this file recomputes", "difference")
o

## ---- headline
o <- data.frame(
  unit = c("counties carried", "votes cast"),
  wf = c(paste0(pc(g24$wf_counties, 1), "%"), paste0(pc(g24$wf_votes, 1), "%")),
  cb = c(paste0(pc(g24$cb_counties, 1), "%"), paste0(pc(g24$cb_votes, 1), "%")),
  gap = c(paste0(pc(g24$gap_counties, 1), " points"),
          paste0(pc(g24$gap_votes, 1), " points")))
names(o) <- c("2024, measured in", "Whole Foods counties",
              "Cracker Barrel counties", "gap")
nobreak(o)

## ---- gapfig-static
par(mfrow = c(2, 1), mar = c(2.4, 4.6, 2.2, 7.6), xpd = NA, cex = 0.88)
for (k in c("counties", "votes")) {
  wfv <- gap[[paste0("wf_", k)]]; cbv <- gap[[paste0("cb_", k)]]
  plot(NA, xlim = c(2008, 2024), ylim = c(0, 100), axes = FALSE, xlab = "",
       ylab = if (k == "counties") "% of counties carried" else "% of votes, D share")
  axis(1, at = gap$year, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
  grid(nx = NA, ny = NULL, col = "#EEEEEE", lty = 1)
  polygon(c(gap$year, rev(gap$year)), c(wfv, rev(cbv)),
          col = "#0000000D", border = NA)
  lines(gap$year, wfv, col = WFC, lwd = 2.6); points(gap$year, wfv, pch = 19, col = WFC)
  lines(gap$year, cbv, col = CBC, lwd = 2.6); points(gap$year, cbv, pch = 19, col = CBC)
  text(2024.5, wfv[nrow(gap)], "Whole Foods", adj = 0, cex = 0.72, col = WFC)
  text(2024.5, cbv[nrow(gap)], "Cracker Barrel", adj = 0, cex = 0.72, col = CBC)
  mtext(sprintf("measured in %s  -  gap %s to %s points", k,
                pc(wfv[1] - cbv[1], 0), pc(wfv[nrow(gap)] - cbv[nrow(gap)], 0)),
        side = 3, line = 0.4, adj = 0, cex = 0.74, col = "#444444")
}
par(mfrow = c(1, 1))

## ---- gapfig-d3
rows <- paste(sprintf(
  '{"y":%d,"wc":%.2f,"cc":%.2f,"wv":%.2f,"cv":%.2f}',
  gap$year, gap$wf_counties, gap$cb_counties, gap$wf_votes, gap$cb_votes),
  collapse = ",")
cat(sprintf('
<div id="gp" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,PH=210,M={t:26,r:118,b:34,l:52};
const box=d3.select("#gp");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${2*PH+34}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const x=d3.scaleLinear().domain([2008,2024]).range([M.l,W-M.r]);
[["counties","wc","cc","%% of counties carried"],
 ["votes","wv","cv","%% of votes, D share"]].forEach((p,pi)=>{
  const oy=pi*(PH+16);
  const y=d3.scaleLinear().domain([0,100]).range([oy+PH-M.b,oy+M.t]);
  const g=svg.append("g");
  g.append("g").attr("transform",`translate(0,${oy+PH-M.b})`)
    .call(d3.axisBottom(x).tickFormat(d3.format("d")).tickValues(D.map(d=>d.y)));
  g.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(4));
  g.append("path").datum(D).attr("fill","#00000010")
    .attr("d",d3.area().x(d=>x(d.y)).y0(d=>y(d[p[2]])).y1(d=>y(d[p[1]])));
  const gapA=(D[0][p[1]]-D[0][p[2]]).toFixed(0),
        gapB=(D[D.length-1][p[1]]-D[D.length-1][p[2]]).toFixed(0);
  g.append("text").attr("x",M.l).attr("y",oy+16).attr("font-size","11.5px")
    .attr("fill","#444").text(`measured in ${p[0]} — gap ${gapA} to ${gapB} points`);
  [[p[1],"%s","Whole Foods"],[p[2],"%s","Cracker Barrel"]].forEach(s=>{
    g.append("path").datum(D).attr("fill","none").attr("stroke",s[1]).attr("stroke-width",2.6)
      .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d[s[0]])));
    g.selectAll("circle."+s[0]).data(D).join("circle").attr("class",s[0])
      .attr("cx",d=>x(d.y)).attr("cy",d=>y(d[s[0]])).attr("r",4).attr("fill",s[1]);
    g.append("text").attr("x",x(2024)+9).attr("y",y(D[D.length-1][s[0]])+4)
      .attr("font-size","11.5px").attr("fill",s[1]).text(s[2]); });
  g.selectAll("rect.h").data(D).join("rect").attr("class","h")
    .attr("x",d=>x(d.y)-16).attr("y",oy+M.t).attr("width",32)
    .attr("height",PH-M.b-M.t).attr("fill","transparent")
    .on("mousemove",function(ev,d){
      tip.style("opacity",1).html(
        `<b>${d.y}</b><br>Whole Foods: ${d[p[1]].toFixed(1)}%%<br>`+
        `Cracker Barrel: ${d[p[2]].toFixed(1)}%%<br>`+
        `gap ${(d[p[1]]-d[p[2]]).toFixed(1)} points`)
        .style("left",Math.min(ev.offsetX+14,W-240)+"px")
        .style("top",(ev.offsetY-10)+"px"); })
    .on("mouseleave",()=>tip.style("opacity",0));
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover any year.</p>
', rows, WFC, CBC, cap_gap))

## ---- cats
o <- data.frame(
  category = cats$category,
  counties = n(cats$counties),
  county_pct = paste0(pc(cats$county_share, 1), "%"),
  dem_counties = paste0(pc(cats$dem_counties_pct, 1), "%"),
  vote_pct = paste0(pc(cats$vote_share, 1), "%"),
  dem_votes = paste0(pc(cats$dem_votes_pct, 1), "%"))
names(o) <- c("counties with", "how many", "share of all counties",
              "D carried", "share of all votes", "D share of votes")
o

## ---- catfig-static
ord <- c("whole foods only", "both", "cracker barrel only", "neither")
i   <- match(ord, cats$category)
COL <- c(WFC, BOTHC, CBC, NONE)
# barplot() stacks down the ROWS and groups across the COLUMNS, so the
# categories have to be the rows and the two measures the columns. The columns
# are supplied in reverse because barplot draws the first at the BOTTOM, and
# the pair should read counties-then-votes down the page.
m   <- cbind("share of votes"    = cats$vote_share[i],
             "share of counties" = cats$county_share[i])
rownames(m) <- ord
par(mar = c(6.8, 8.4, 0.6, 1.6), cex = 0.88)
bp <- barplot(m, horiz = TRUE, col = COL, border = "white",
              las = 1, xlab = "", xlim = c(0, 100))
for (b in 1:2) {
  mid <- cumsum(m[, b]) - m[, b] / 2
  ok  <- m[, b] > 6
  text(mid[ok], bp[b], paste0(pc(m[ok, b], 0), "%"), col = "white",
       cex = 0.78, font = 2)
}
mtext("% of the national total", side = 1, line = 2.3, cex = 0.78)
legend("bottom", inset = c(0, -0.46), xpd = NA, horiz = TRUE, bty = "n",
       cex = 0.62, legend = ord, fill = COL, border = NA)
subcap(cap_cat, line = 5.2)

## ---- catfig-d3
ord <- c("whole foods only", "both", "cracker barrel only", "neither")
i   <- match(ord, cats$category)
rows <- paste(sprintf('{"k":"%s","c":%.2f,"v":%.2f,"n":%d,"dc":%.1f,"dv":%.1f}',
                      ord, cats$county_share[i], cats$vote_share[i],
                      cats$counties[i], cats$dem_counties_pct[i],
                      cats$dem_votes_pct[i]), collapse = ",")
cat(sprintf('
<div id="ct" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], COL=["%s","%s","%s","%s"];
const W=760,H=210,M={t:16,r:20,b:56,l:118};
const box=d3.select("#ct");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(["share of counties","share of votes"])
  .range([M.t,H-M.b]).padding(0.3);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
[["share of counties","c"],["share of votes","v"]].forEach(row=>{
  let acc=0;
  D.forEach((d,j)=>{
    const x0=acc; acc+=d[row[1]];
    svg.append("rect").attr("x",x(x0)).attr("y",y(row[0]))
      .attr("width",x(acc)-x(x0)).attr("height",y.bandwidth())
      .attr("fill",COL[j]).attr("stroke","#fff")
      .on("mousemove",function(ev){
        tip.style("opacity",1).html(
          `<b>${d.k}</b><br>${d.n} counties<br>`+
          `${d.c.toFixed(1)}%% of counties, ${d.v.toFixed(1)}%% of votes<br>`+
          `D carried ${d.dc.toFixed(1)}%% of them<br>`+
          `D won ${d.dv.toFixed(1)}%% of their two-party vote`)
          .style("left",Math.min(ev.offsetX+14,W-270)+"px")
          .style("top",(ev.offsetY-10)+"px"); })
      .on("mouseleave",()=>tip.style("opacity",0));
    if (d[row[1]]>6) svg.append("text").attr("x",x((x0+acc)/2)).attr("y",y(row[0])+y.bandwidth()/2+4)
      .attr("text-anchor","middle").attr("font-size","11.5px").attr("font-weight","600")
      .attr("fill","#fff").attr("pointer-events","none").text(d[row[1]].toFixed(0)+"%%");
  });
});
const lg=svg.append("g").attr("transform",`translate(${M.l},${H-16})`);
D.forEach((d,j)=>{
  lg.append("rect").attr("x",j*150).attr("y",-10).attr("width",12).attr("height",12).attr("fill",COL[j]);
  lg.append("text").attr("x",j*150+17).attr("font-size","11px").attr("fill","#333").text(d.k); });
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover any block.</p>
', rows, WFC, BOTHC, CBC, NONE, cap_cat))

## ---- medians
ord <- c("whole foods only", "both", "cracker barrel only", "neither")
i   <- match(ord, cats$category)
o <- data.frame(
  category = ord,
  pop = n(round(cats$med_pop[i])),
  dens = n(round(cats$med_density[i])),
  ba = paste0(pc(cats$med_ba[i], 0), "%"))
names(o) <- c("counties with", "median population",
              "median people per square mile", "median % with a BA")
nobreak(o)

## ---- alts
o <- data.frame(
  rule = alt$rule,
  dc = paste0(pc(alt$dem_counties_pct, 1), "%"),
  dv = paste0(pc(alt$dem_votes_pct, 1), "%"),
  ov = paste0(pc(alt$overlap_wf_pct, 0), "%"))
names(o) <- c("the 212 counties picked by", "D carried",
              "D share of their votes", "that also have a Whole Foods")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
