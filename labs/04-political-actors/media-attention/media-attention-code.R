# media-attention-code.R -- chunk bodies for media-attention-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.8,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

w  <- read.csv("data/derived/wiki_attention_2024.csv", stringsAsFactors = FALSE)
ev <- read.csv("data/derived/campaign_events_2024.csv", stringsAsFactors = FALSE)
w$date  <- as.Date(w$date)
ev$date <- as.Date(ev$date)

g   <- function(a) { s <- w[w$article == a, ]; s[order(s$date), ] }
med <- function(a) median(g(a)$views)
pk  <- function(a) max(g(a)$views)
pkd <- function(a) { s <- g(a); format(s$date[which.max(s$views)], "%e %B") }
von <- function(a, d) g(a)$views[g(a)$date == as.Date(d)]

pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
cnt <- function(x) format(round(x), big.mark = ",")
nice <- function(a) gsub("_", " ", a)

conc <- function(a) {
  s <- g(a); v <- sort(s$views, decreasing = TRUE)
  cu <- cumsum(v) / sum(v) * 100
  c(top10 = cu[10], top30 = cu[30], half = which(cu >= 50)[1])
}

people <- c("Tim_Walz", "Donald_Trump", "Kamala_Harris", "JD_Vance")
peaks  <- data.frame(article = people, peak = sapply(people, pk),
                     when = sapply(people, pkd),
                     baseline = sapply(people, med),
                     stringsAsFactors = FALSE)
peaks  <- peaks[order(-peaks$peak), ]

h  <- g("Kamala_Harris"); t <- g("Donald_Trump")
mg <- merge(h[, c("date", "views")], t[, c("date", "views")], by = "date",
            suffixes = c("_h", "_t"))
mg$phase <- ifelse(mg$date < as.Date("2024-07-21"), "before 21 July",
                                                    "from 21 July")
lead <- table(mg$phase, ifelse(mg$views_h > mg$views_t, "Harris", "Trump"))

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- d3-load
# the one and only copy of d3 in this document
cat('<script src="../../_lib/d3.v7.min.js"></script>\n')

## ---- wiki-grid
# The complete article-by-day grid, built explicitly so that an absent
# article-day would stay absent (NA) rather than quietly becoming a zero. As of
# the title fix described below the grid is complete, and the code that marks
# holes is kept because the check is what tells you so.
days   <- sort(unique(w$date))
arts_o <- names(sort(tapply(w$views, w$article, sum), decreasing = TRUE))
grid   <- matrix(NA_real_, nrow = length(arts_o), ncol = length(days),
                 dimnames = list(arts_o, format(days)))
grid[cbind(match(w$article, arts_o), match(w$date, days))] <- w$views

miss   <- which(is.na(grid), arr.ind = TRUE)
miss_n <- nrow(miss)
zc     <- which(grid == 0 & !is.na(grid), arr.ind = TRUE)
zero_n <- nrow(zc)

# the two titles the Vance article held, kept separately by the build script
ti <- read.csv("data/derived/wiki_titles_2024.csv", stringsAsFactors = FALSE)
ti$date <- as.Date(ti$date)
TI_OLD <- "J._D._Vance"; TI_NEW <- "JD_Vance"
tiw <- merge(
  setNames(ti[ti$title == TI_OLD, c("date", "views")], c("date", "old")),
  setNames(ti[ti$title == TI_NEW, c("date", "views")], c("date", "new")),
  by = "date", all = TRUE)
tiw <- tiw[order(tiw$date), ]
tiw$old[is.na(tiw$old)] <- 0; tiw$new[is.na(tiw$new)] <- 0
MOVE  <- tiw$date[which(tiw$new > tiw$old)[1]]        # the day the title flipped
NAMED <- as.Date("2024-07-15")                        # named to the ticket
VPRE  <- median(w$views[w$article == "JD_Vance" & w$date < NAMED])
VPEAK <- max(w$views[w$article == "JD_Vance"])
# What a request for the current title alone returned, across the whole year --
# the number this chapter used to print as its median day. Taken from the raw
# by-title table, not from tiw, whose missing day has been zero-filled for
# plotting and would drag the median down by one.
VOLD  <- median(ti$views[ti$title == TI_NEW])
VRESID <- 100 * median(tiw$old[tiw$date > MOVE] /
                       (tiw$old + tiw$new)[tiw$date > MOVE])

pal12 <- c("#C41230", "#2c7fb8", "#4d9221", "#e08214", "#8856a7", "#999999",
           "#7f0b20", "#17557f", "#31661a", "#9c5a0c", "#5c3a7a", "#5f5f5f")
acol  <- setNames(pal12[seq_along(arts_o)], arts_o)

## ---- one-record
o <- g("Kamala_Harris")
o <- o[o$date == as.Date("2024-07-22"), ]
data.frame(field = c("date", "article", "views"),
           value = c(format(o$date), o$article, cnt(o$views)),
           what_it_is = c("one calendar day",
                          "one English Wikipedia article title",
                          "requests from human readers that day"),
           check.names = FALSE)

## ---- clean-media
o <- g("Kamala_Harris")
o <- o[o$date >= as.Date("2024-07-20") & o$date <= as.Date("2024-07-22"), ]
o$views <- cnt(o$views)
o$date <- format(o$date)
names(o) <- c("date", "article", "views")
o

## ---- scope
data.frame(
  quantity = c("Articles tracked", "Titles requested", "Days covered",
               "Rows in the file", "Rows a complete grid would have",
               "Total pageviews", "Article-days with zero human readers"),
  value = c(length(unique(w$article)),
            length(unique(w$article)) + length(unique(ti$title)) - 1,
            length(unique(w$date)), cnt(nrow(w)),
            cnt(length(unique(w$article)) * length(unique(w$date))),
            cnt(sum(w$views)), zero_n))

## ---- hgrid-static
lv   <- log10(pmax(grid, 1) + 1)
mxlv <- max(lv, na.rm = TRUE)
ramp <- colorRampPalette(c("#f4f4f4", "#c6dbef", "#2c7fb8", "#0d3b57"))(100)
par(mar = c(2.6, 11.4, 4.2, 0.8))
plot(NA, xlim = c(0.5, length(days) + 0.5), ylim = c(0.4, nrow(grid) + 0.6),
     axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
for (r in seq_len(nrow(grid))) {
  yv <- nrow(grid) - r + 1
  cl <- ramp[pmax(1, ceiling(100 * (lv[r, ] / mxlv)^1.4))]
  cl[!is.na(grid[r, ]) & grid[r, ] == 0] <- "#e08214"
  cl[is.na(grid[r, ])] <- "#C41230"
  rect(seq_along(days) - 0.5, yv - 0.45, seq_along(days) + 0.5, yv + 0.45,
       col = cl, border = NA)
}
axis(2, at = nrow(grid):1, labels = nice(arts_o), las = 1, tick = FALSE,
     cex.axis = 0.62, line = -0.7)
m1 <- as.Date(paste0("2024-", sprintf("%02d", 1:12), "-01"))
axis(1, at = match(m1, days), labels = format(m1, "%b"), tick = FALSE,
     cex.axis = 0.7, line = -1.0)
mv <- match(MOVE, days)
points(mv, nrow(grid) + 0.72, pch = 25, bg = "#C41230", col = "#C41230",
       cex = 0.7, xpd = NA)
mtext(paste0("Every cell is filled: ", zero_n, " zero days and ", miss_n,
             " missing days in ", cnt(nrow(w)), " article-days."),
      side = 3, line = 2.4, cex = 0.62, adj = 0)
mtext(paste0("Red arrow: ", format(MOVE, "%e %B"),
             ", the day the traffic crossed from the Vance article's old",
             " title to its new one."),
      side = 3, line = 1.5, cex = 0.62, adj = 0)
mtext("Blue: pageviews, darker is more (log scale).", side = 3, line = 0.6,
      cex = 0.62, adj = 0)

## ---- hgrid-d3
ser <- paste(apply(grid, 1, function(v)
  sprintf('[%s]', paste(ifelse(is.na(v), -1, v), collapse = ","))),
  collapse = ",")
labs <- paste(sprintf('"%s"', nice(arts_o)), collapse = ",")
m1   <- as.Date(paste0("2024-", sprintf("%02d", 1:12), "-01"))
mons <- paste(sprintf('[%d,"%s"]', match(m1, days) - 1, format(m1, "%b")),
              collapse = ",")
cat(sprintf('
<div id="hgrid" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const V=[%s], L=[%s], MO=[%s], D0=%d, MISS=%d;
const nd=V[0].length, nr=V.length;
const W=852,H=nr*24+96,M={t:44,r:14,b:26,l:238};
const cw=(W-M.l-M.r)/nd, chh=(H-M.t-M.b)/nr;
const box=d3.select("#hgrid");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const mx=d3.max(V,r=>d3.max(r)), MXLV=Math.log10(mx+1);
const col=d3.scaleSequential(d3.interpolateRgbBasis(
  ["#f4f4f4","#c6dbef","#2c7fb8","#0d3b57"])).domain([0,1]);
const shade=v=>col(Math.pow(Math.log10(v+1)/MXLV,1.4));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
const fmt=d3.utcFormat("%%e %%B");
V.forEach((row,r)=>{
  svg.append("text").attr("x",M.l-10).attr("y",M.t+r*chh+chh/2+4)
    .attr("text-anchor","end").attr("font-size","11px").attr("fill","#333").text(L[r]);
  svg.append("g").selectAll("rect").data(row.map((v,i)=>[i,v])).join("rect")
    .attr("x",p=>M.l+p[0]*cw).attr("y",M.t+r*chh)
    .attr("width",Math.max(cw,0.9)).attr("height",chh-1)
    .attr("fill",p=>p[1]<0?"#C41230":p[1]===0?"#e08214":shade(p[1]))
    .on("mousemove",function(e,p){
      const dt=new Date((D0+p[0])*86400000);
      tip.style("opacity",1).html(`<b>${L[r]}</b><br>${fmt(dt)}<br>`+
        (p[1]<0?"<i>no row in the file</i>":d3.format(",")(p[1])+" pageviews"))
        .style("left",Math.min(M.l+p[0]*cw+10,W-230)+"px")
        .style("top",(M.t+r*chh-6)+"px");
    }).on("mouseleave",()=>tip.style("opacity",0));
});
MO.forEach(m=>svg.append("text").attr("x",M.l+m[0]*cw).attr("y",H-10)
  .attr("font-size","10.5px").attr("fill","#777").text(m[1]));
svg.append("path").attr("d",d3.symbol().type(d3.symbolTriangle).size(46))
  .attr("transform",`translate(${M.l+(MISS+0.5)*cw},${M.t-9}) rotate(180)`)
  .attr("fill","#C41230");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s cells: %d articles by %d days, and every one of them is filled — %d missing,
%d zero. The arrow marks %s, the day the traffic crossed from the Vance article’s
old title to its new one; before the two were summed, that row was almost empty
to the left of it. Hover any
cell.</p>
', ser, labs, mons, as.integer(min(days)), match(MOVE, days) - 1,
   cnt(length(days) * length(arts_o)), length(arts_o), length(days),
   miss_n, zero_n, format(MOVE, "%e %B")))

## ---- fig-move-static
op <- par(mar = c(3.0, 4.6, 2.4, 1.0), mgp = c(3.2, 0.6, 0))
plot(NA, xlim = as.numeric(range(tiw$date)), ylim = c(1, max(tiw$old, tiw$new)),
     log = "y", axes = FALSE, xlab = "", ylab = "")
yt <- 10^(0:6)
axis(2, at = yt, labels = format(yt, big.mark = ",", scientific = FALSE,
                                 trim = TRUE), las = 1, cex.axis = 0.68,
     lwd = 0, lwd.ticks = 1)
m1 <- as.Date(paste0("2024-", sprintf("%02d", seq(1, 12, 2)), "-01"))
axis(1, at = as.numeric(m1), labels = format(m1, "%b"), cex.axis = 0.72,
     lwd = 0, lwd.ticks = 1)
abline(h = yt, col = "#00000010")
abline(v = as.numeric(MOVE), col = "#8A8F94", lty = 3)
lines(tiw$date, pmax(tiw$old, 1), col = "#1C4C5C", lwd = 1.8)
lines(tiw$date, pmax(tiw$new, 1), col = "#C41230", lwd = 1.8)
text(as.Date("2024-02-20"), 4e4, gsub("_", " ", TI_OLD), col = "#1C4C5C",
     cex = 0.68, adj = 0)
text(as.Date("2024-09-01"), 6, gsub("_", " ", TI_NEW), col = "#C41230",
     cex = 0.68, adj = 0)
text(MOVE, max(tiw$old, tiw$new), format(MOVE, "%e %b"), col = "#8A8F94",
     cex = 0.62, adj = c(-0.1, 1))
mtext("Daily pageviews under each title, log scale", 2, line = 3.4, cex = 0.72)
mtext("One article, two titles, one handover", 3, line = 0.6, cex = 0.8,
      adj = 0)
par(op)

## ---- fig-move-d3
# The two title series on one log axis. The whole argument for summing them is
# visual: they cross once, on the day of the move, and neither is a plausible
# readership series on its own.
cat(paste0('
<div id="mv" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const OLD=[', paste(tiw$old, collapse = ","), '];
const NEW=[', paste(tiw$new, collapse = ","), '];
const D0=', as.integer(min(tiw$date)), ', MV=', which(tiw$date == MOVE) - 1, ';
const LO="', gsub("_", " ", TI_OLD), '", LN="', gsub("_", " ", TI_NEW), '";
const nd=OLD.length;
const W=770,H=330,M={t:20,r:126,b:34,l:66};
const box=d3.select("#mv");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,nd-1]).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([1,d3.max(OLD.concat(NEW))]).range([H-M.b,M.t]);
const dec=d3.range(0,7).map(e=>Math.pow(10,e));
const dayOf=i=>new Date((D0+i)*86400000);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues([0,60,121,182,244,305])
          .tickFormat(i=>d3.utcFormat("%b")(dayOf(i))));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickValues(dec).tickFormat(d3.format("~s")));
svg.append("g").selectAll("line").data(dec).join("line")
  .attr("x1",M.l).attr("x2",W-M.r).attr("y1",d=>y(d)).attr("y2",d=>y(d))
  .attr("stroke","currentColor").attr("stroke-opacity",0.07);
svg.append("line").attr("x1",x(MV)).attr("x2",x(MV)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#8A8F94").attr("stroke-dasharray","3 3");
const ln=d3.line().x((d,i)=>x(i)).y(d=>y(Math.max(d,1)));
[["#1C4C5C",OLD,LO],["#C41230",NEW,LN]].forEach(function(s,k){
  svg.append("path").attr("fill","none").attr("stroke",s[0])
     .attr("stroke-width",1.9).attr("d",ln(s[1]));
  svg.append("text").attr("x",W-M.r+8).attr("y",M.t+14+k*18)
     .attr("font-size","11px").attr("fill",s[0]).text(s[2]);
});
svg.append("text").attr("x",x(MV)+5).attr("y",M.t+10).attr("font-size","10.5px")
  .attr("fill","#8A8F94").text("renamed");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;opacity:0;background:#FAFBFB;color:#12181D;border:1px solid #CBD3D8;border-radius:3px;padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const cm=d3.format(",");
const fd=d3.utcFormat("%e %B");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const i=Math.max(0,Math.min(nd-1,Math.round(x.invert(d3.pointer(e,this)[0]))));
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+fd(dayOf(i))+"</b><br>"+
             "<span style=\\"color:#1C4C5C\\">"+LO+"</span> "+cm(OLD[i])+"<br>"+
             "<span style=\\"color:#C41230\\">"+LN+"</span> "+cm(NEW[i])+"<br>"+
             "<b>together "+cm(OLD[i]+NEW[i])+"</b>");
  }).on("mouseleave",function(){tip.style("opacity",0);});
})();
</script>'))

## ---- election-article
elec <- "2024_United_States_presidential_election"
data.frame(
  quantity = c("Median day", "Peak day", "Peak date",
               "Peak as a multiple of the median"),
  value = c(cnt(med(elec)), cnt(pk(elec)), pkd(elec),
            paste0(pc(pk(elec) / med(elec), 0), "×")))

## ---- cal-prep
ce   <- g(elec)
sun0 <- as.Date("2023-12-31")                      # the Sunday before 1 Jan
cwk  <- as.integer(ce$date - sun0) %/% 7
cwd  <- as.integer(format(ce$date, "%w"))          # 0 = Sunday
clv  <- log10(ce$views + 1)
cmon <- as.Date(paste0("2024-", sprintf("%02d", 1:12), "-01"))
cmwk <- as.integer(cmon - sun0) %/% 7
top5 <- order(-ce$views)[1:5]

## ---- cal-static
ramp <- colorRampPalette(c("#f4f4f4", "#c6dbef", "#2c7fb8", "#0d3b57"))(100)
cix  <- pmax(1, ceiling(100 * (clv - min(clv)) / diff(range(clv))))
par(mar = c(3.4, 3.2, 1.6, 0.8))
plot(NA, xlim = c(-0.5, max(cwk) + 1), ylim = c(7.2, -0.4), axes = FALSE,
     ann = FALSE, asp = 1)
rect(cwk, cwd, cwk + 0.88, cwd + 0.88, col = ramp[cix], border = NA)
rect(cwk[top5], cwd[top5], cwk[top5] + 0.88, cwd[top5] + 0.88, col = NA,
     border = "#C41230", lwd = 1.4)
text(-0.6, c(1, 3, 5) + 0.44, c("Mon", "Wed", "Fri"), cex = 0.58, adj = 1,
     xpd = NA)
text(cmwk, -0.5, format(cmon, "%b"), cex = 0.62, adj = 0, col = "#555555",
     xpd = NA)
mtext(paste0(nice(elec), ": median day ", cnt(med(elec)), ", peak ",
             cnt(pk(elec)), " on ", pkd(elec), " (",
             pc(pk(elec) / med(elec), 0), " times the median)."),
      side = 1, line = 0.6, cex = 0.64, adj = 0)
mtext(paste0("Red outlines: the five busiest days, which between them carried ",
             pc(100 * sum(sort(ce$views, decreasing = TRUE)[1:5]) /
                sum(ce$views), 1), "% of the year."),
      side = 1, line = 1.5, cex = 0.64, adj = 0)

## ---- cal-d3
rows <- paste(sprintf('[%d,%d,%d,%d]', cwk, cwd, ce$views,
                      as.integer(seq_len(nrow(ce)) %in% top5)),
              collapse = ",")
mons <- paste(sprintf('[%d,"%s"]', cmwk, format(cmon, "%b")), collapse = ",")
cat(sprintf('
<div id="cal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MO=[%s], D0=%d;
const cell=13.4, W=54*cell+64, H=7*cell+58, M={t:26,l:44};
const box=d3.select("#cal");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const ex=d3.extent(D,d=>Math.log10(d[2]+1));
const col=d3.scaleSequential(d3.interpolateRgbBasis(
  ["#f4f4f4","#c6dbef","#2c7fb8","#0d3b57"])).domain(ex);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
const fmt=d3.utcFormat("%%e %%B %%Y");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>M.l+d[0]*cell).attr("y",d=>M.t+d[1]*cell)
  .attr("width",cell-1.4).attr("height",cell-1.4)
  .attr("fill",d=>col(Math.log10(d[2]+1)))
  .attr("stroke",d=>d[3]?"#C41230":"none").attr("stroke-width",1.6)
  .on("mousemove",function(e,d){
    const dt=new Date((D0+(d[0]*7+d[1]))*86400000);
    tip.style("opacity",1).html(`<b>${fmt(dt)}</b><br>`+
      d3.format(",")(d[2])+" pageviews")
      .style("left",Math.min(M.l+d[0]*cell+10,W-190)+"px")
      .style("top",(M.t+d[1]*cell-4)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
[[1,"Mon"],[3,"Wed"],[5,"Fri"]].forEach(r=>
  svg.append("text").attr("x",M.l-8).attr("y",M.t+r[0]*cell+10)
    .attr("text-anchor","end").attr("font-size","10.5px").attr("fill","#777").text(r[1]));
MO.forEach(m=>svg.append("text").attr("x",M.l+m[0]*cell).attr("y",M.t-8)
  .attr("font-size","10.5px").attr("fill","#777").text(m[1]));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%d squares, one per day. The five outlined in red carried %.1f%% of the whole
year between them; the busiest, %s, drew %s readers against a median day of %s.
Hover any square.</p>
', rows, mons, as.integer(sun0), nrow(ce),
   100 * sum(sort(ce$views, decreasing = TRUE)[1:5]) / sum(ce$views),
   pkd(elec), cnt(pk(elec)), cnt(med(elec))))

## ---- concentration
arts <- c("Donald_Trump", "Kamala_Harris",
          "2024_United_States_presidential_election",
          "Electoral_College_(United_States)", "Opinion_poll",
          "Springfield,_Ohio")
o <- t(sapply(arts, conc))
o <- data.frame(article = nice(arts), top10 = pc(o[, 1]), top30 = pc(o[, 2]),
                half = o[, 3], row.names = NULL)
names(o) <- c("article", "% of the year's views in its 10 busiest days",
              "in its 30 busiest days", "days needed to reach half")
o

## ---- stream-prep
k    <- length(arts_o)
sord <- c(rev(arts_o[seq(1, k, 2)]), arts_o[seq(2, k, 2)])  # biggest in middle
S    <- t(grid[sord, , drop = FALSE])                       # days x series
tot  <- rowSums(S)                                          # NA on the gap day
cum  <- t(apply(S, 1, cumsum))
base <- -tot / 2
sy0  <- cbind(base, base + cum[, -k, drop = FALSE])
sy1  <- base + cum
# runs of consecutive days with a total. The grid is complete, so this is a
# single run -- the machinery is kept because it is what makes the completeness
# visible rather than assumed.
okd  <- which(!is.na(tot))
runs <- split(okd, cumsum(c(1, diff(okd) != 1)))
peak_day <- days[which.max(tot)]
top10    <- sum(sort(tot, decreasing = TRUE)[1:10]) / sum(tot, na.rm = TRUE)

## ---- stream-static
par(mar = c(2.6, 1.0, 2.6, 1.0))
plot(NA, xlim = range(days), ylim = range(c(sy0, sy1), na.rm = TRUE),
     axes = FALSE, ann = FALSE)
for (j in seq_len(k)) for (rr in runs) {
  polygon(c(days[rr], rev(days[rr])), c(sy0[rr, j], rev(sy1[rr, j])),
          col = acol[sord[j]], border = NA)
}
axis.Date(1, at = seq(as.Date("2024-01-01"), as.Date("2024-12-01"),
                      by = "1 month"), format = "%b", tick = FALSE,
          cex.axis = 0.7, line = -1.0)
mtext(paste0("Peak day ", format(peak_day, "%e %B"), ": ",
             cnt(max(tot, na.rm = TRUE)), " pageviews across all ", k,
             " articles. The ten busiest days carried ",
             pc(100 * top10, 1), "% of the year."),
      side = 3, line = 1.2, cex = 0.66, adj = 0)
mtext(paste0("The ribbon is drawn in ", length(runs),
             " unbroken run(s): every day of the year has a total."),
      side = 3, line = 0.3, cex = 0.66, adj = 0)
legend("topleft", nice(sord), fill = acol[sord], border = NA, bty = "n",
       cex = 0.52, ncol = 3)

## ---- stream-d3
bands <- paste(sapply(seq_len(k), function(j) sprintf(
  '{"k":"%s","c":"%s","y0":[%s],"y1":[%s]}', nice(sord[j]), acol[sord[j]],
  paste(ifelse(is.na(sy0[, j]), 0, round(sy0[, j])), collapse = ","),
  paste(ifelse(is.na(sy1[, j]), 0, round(sy1[, j])), collapse = ","))),
  collapse = ",")
rr   <- paste(sprintf('[%d,%d]', sapply(runs, min) - 1, sapply(runs, max) - 1),
              collapse = ",")
evs  <- paste(sprintf('{"i":%d,"t":"%s"}', match(ev$date, days) - 1,
                      gsub('"', "", ev$event)), collapse = ",")
cat(sprintf('
<div id="stream" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const B=[%s], R=[%s], E=[%s], D0=%d, MI=%d;
const nd=B[0].y0.length;
const W=800,H=430,M={t:16,r:16,b:30,l:16};
const box=d3.select("#stream");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,nd-1]).range([M.l,W-M.r]);
const lo=d3.min(B,b=>d3.min(b.y0)), hi=d3.max(B,b=>d3.max(b.y1));
const y=d3.scaleLinear().domain([lo,hi]).range([H-M.b,M.t]);
const area=d3.area().x(d=>x(d.i)).y0(d=>y(d.a)).y1(d=>y(d.b));
let focus=null;
const gg=svg.append("g");
function paths(){
  gg.selectAll("g.band").data(B,b=>b.k).join(
    en=>en.append("g").attr("class","band")).each(function(b){
    const sel=d3.select(this);
    sel.selectAll("path").data(R).join("path")
      .attr("fill",b.c)
      .attr("opacity",focus&&focus!==b.k?0.14:0.95)
      .attr("d",r=>{const pts=[];
        for(let i=r[0];i<=r[1];i++) pts.push({i:i,a:b.y0[i],b:b.y1[i]});
        return area(pts);});
  });
}
paths();
E.forEach(e=>svg.append("line").attr("x1",x(e.i)).attr("x2",x(e.i))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#fff")
  .attr("stroke-opacity",0.55).attr("stroke-dasharray","3,3"));
const fmt=d3.utcFormat("%%e %%B");
[0,31,60,91,121,152,182,213,244,274,305,335].forEach(i=>
  svg.append("text").attr("x",x(i)).attr("y",H-8).attr("font-size","10.5px")
    .attr("fill","#777").text(d3.utcFormat("%%b")(new Date((D0+i)*86400000))));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const i=Math.max(0,Math.min(nd-1,Math.round(x.invert(d3.pointer(e,this)[0]))));
    const ev=E.find(z=>z.i===i);
    const rows=B.map(b=>({k:b.k,c:b.c,v:b.y1[i]-b.y0[i]}))
      .sort((a,b)=>b.v-a.v).slice(0,5);
    const tt=d3.sum(B,b=>b.y1[i]-b.y0[i]);
    tip.style("opacity",1).html(`<b>${fmt(new Date((D0+i)*86400000))}</b>`+
      (ev?`<br><i>${ev.t}</i>`:"")+`<br>all ${B.length}: ${d3.format(",")(Math.round(tt))}<br>`+
      rows.map(r=>`<span style="color:${r.c}">\\u25a0</span> ${r.k}: ${d3.format(",")(Math.round(r.v))}`).join("<br>"))
      .style("left",Math.min(x(i)+12,W-300)+"px").style("top",(M.t+4)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
const leg=box.append("div").attr("style","margin-top:8px;font-size:11px;line-height:1.9");
leg.selectAll("span").data(B.slice().reverse()).join("span")
  .attr("style",b=>`display:inline-block;margin-right:11px;cursor:pointer;color:${b.c};font-weight:600`)
  .html(b=>`\\u25a0 ${b.k}`)
  .on("mouseenter",(e,b)=>{focus=b.k;paths();})
  .on("mouseleave",()=>{focus=null;paths();});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Total thickness is the day’s attention across all %d articles; the peak,
%s, drew %s. The ten busiest days of the year carried %.1f%% of it. The ribbon
runs unbroken across the whole year, which it did not before the two Vance
titles were summed. Hover for a daily readout; hover a label to isolate a
band.</p>
', bands, rr, evs, as.integer(min(days)), match(MOVE, days) - 1,
   k, format(peak_day, "%e %B"), cnt(max(tot, na.rm = TRUE)), 100 * top10))

## ---- july
o <- g("Kamala_Harris")
o <- o[o$date >= as.Date("2024-07-18") & o$date <= as.Date("2024-07-24"),
       c("date", "views")]
o$multiple <- pc(o$views / med("Kamala_Harris"), 0)
o$views <- cnt(o$views)
names(o) <- c("date", "pageviews", "× her own median day")
o

## ---- lines-static
ks <- c("Donald_Trump", "Kamala_Harris", "Joe_Biden", "Tim_Walz", "JD_Vance")
cl <- c("#B2182B", "#2166AC", "#999999", "#4d9221", "#e08214")
plot(NA, xlim = range(w$date), ylim = c(1, 5e6), log = "y", las = 1,
     xlab = "", ylab = "pageviews (log scale)", yaxt = "n", xaxt = "n")
axis(2, at = c(1, 100, 10000, 1e6), labels = c("1", "100", "10k", "1m"), las = 1)
axis.Date(1, at = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "2 months"),
          format = "%b")
abline(v = ev$date, lty = 3, col = "grey75")
for (i in seq_along(ks)) {
  s <- g(ks[i]); lines(s$date, pmax(s$views, 1), col = cl[i], lwd = 1.5)
}
legend("bottomright", nice(ks), col = cl, lwd = 2, bty = "n", cex = 0.75)

## ---- lines-d3
ks <- c("Donald_Trump", "Kamala_Harris", "Joe_Biden", "Tim_Walz", "JD_Vance",
        "Springfield,_Ohio")
cl <- c("#B2182B", "#2166AC", "#999999", "#4d9221", "#e08214", "#8856a7")
ser <- paste(mapply(function(k, col) {
  s <- g(k)
  sprintf('{"k":"%s","c":"%s","v":[%s]}', nice(k), col,
          paste(sprintf('[%d,%d]', as.integer(s$date), pmax(s$views, 1)),
                collapse = ","))
}, ks, cl), collapse = ",")
evs <- paste(sprintf('{"d":%d,"t":"%s"}', as.integer(ev$date),
                     gsub('"', "", ev$event)), collapse = ",")
cat(sprintf('
<div id="att" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[%s], E=[%s];
const W=780,H=470,M={t:18,r:24,b:44,l:58};
const svg=d3.select("#att").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleTime().domain([new Date(2024,0,1),new Date(2024,11,31)]).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([1,5e6]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(12).tickFormat(d3.timeFormat("%%b")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([1,100,1e4,1e6]).tickFormat(d3.format(".0s")));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("pageviews (log scale)");
E.forEach(e=>{
  svg.append("line").attr("x1",x(new Date(e.d*86400000))).attr("x2",x(new Date(e.d*86400000)))
    .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#ccc").attr("stroke-dasharray","3,3");
});
const ln=d3.line().x(p=>x(new Date(p[0]*86400000))).y(p=>y(p[1]));
const on={}; S.forEach(s=>on[s.k]=true);
let focus=null;
const gg=svg.append("g");
function draw(){
  gg.selectAll("path").data(S.filter(s=>on[s.k]),s=>s.k).join("path")
   .attr("fill","none").attr("stroke",s=>s.c).attr("d",s=>ln(s.v))
   .attr("stroke-width",s=>focus===s.k?2.6:1.4)
   .attr("opacity",s=>focus&&focus!==s.k?0.12:0.92);
}
draw();
const tip=d3.select("#att").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#888").attr("opacity",0);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l).attr("height",H-M.b-M.t)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const dt=x.invert(d3.pointer(e,this)[0]+M.l);
    const day=Math.round(dt.getTime()/86400000);
    rule.attr("x1",x(new Date(day*86400000))).attr("x2",x(new Date(day*86400000))).attr("opacity",1);
    const rows=S.filter(s=>on[s.k]).map(s=>{const p=s.v.find(q=>q[0]===day);
      return p?{k:s.k,c:s.c,v:p[1]}:null;}).filter(Boolean).sort((a,b)=>b.v-a.v);
    const evt=E.find(z=>z.d===day);
    tip.style("opacity",1).html(`<b>${d3.timeFormat("%%e %%B")(new Date(day*86400000))}</b>`+
      (evt?`<br><i>${evt.t}</i>`:"")+"<br>"+
      rows.map(r=>`<span style="color:${r.c}">\\u25a0</span> ${r.k}: ${d3.format(",")(r.v)}`).join("<br>"))
      .style("left",Math.min(x(new Date(day*86400000))-M.l+16,W-320)+"px").style("top",(M.t+2)+"px");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);tip.style("opacity",0);});
const leg=d3.select("#att").append("div").attr("style","margin-top:8px;font-size:11.5px;line-height:1.8");
leg.selectAll("span").data(S).join("span")
  .attr("style",s=>`display:inline-block;margin-right:12px;cursor:pointer;color:${s.c};font-weight:600`)
  .html(s=>`\\u25a0 ${s.k}`)
  .on("mouseenter",(e,s)=>{focus=s.k;draw();})
  .on("mouseleave",()=>{focus=null;draw();})
  .on("click",function(e,s){on[s.k]=!on[s.k];
    d3.select(this).style("opacity",on[s.k]?1:0.3);focus=null;draw();});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Log scale, so a straight vertical rise is a hundredfold jump. Dotted lines are
the events in <code>campaign_events_2024.csv</code>. Move across for a daily
readout; hover a label to isolate a line, click to hide it.</p>
', ser, evs))

## ---- vps
BEFORE <- c(JD_Vance = "2024-07-15", Tim_Walz = "2024-08-06")
base_before <- function(a) median(g(a)$views[g(a)$date < as.Date(BEFORE[a])])
o <- data.frame(
  article = nice(names(BEFORE)),
  before = cnt(sapply(names(BEFORE), base_before)),
  median_day = cnt(sapply(names(BEFORE), med)),
  peak = cnt(sapply(names(BEFORE), pk)),
  peak_date = sapply(names(BEFORE), pkd),
  ratio = paste0(cnt(sapply(names(BEFORE),
                            function(a) pk(a) / base_before(a))), "×"),
  row.names = NULL)
names(o) <- c("article", "median day before naming", "median day, all year",
              "peak", "peak date", "peak ÷ before")
o

## ---- spike-prep
sp <- do.call(rbind, lapply(arts_o, function(a) {
  s <- g(a); m <- median(s$views); r <- s$views / m
  kk <- r >= 2
  data.frame(article = a, date = s$date[kk], ratio = r[kk],
             views = s$views[kk], stringsAsFactors = FALSE)
}))
sp_top <- do.call(rbind, lapply(arts_o, function(a) {
  z <- sp[sp$article == a, ]; z[which.max(z$ratio), ]
}))

## ---- spike-static
yy  <- length(arts_o) - match(sp$article, arts_o) + 1
yt  <- length(arts_o) - match(sp_top$article, arts_o) + 1
par(mar = c(4.0, 11.4, 1.2, 2.2))
plot(NA, xlim = range(sp$ratio), ylim = c(0.4, length(arts_o) + 0.6),
     log = "x", yaxt = "n", bty = "n", las = 1, ylab = "",
     xlab = "that day, as a multiple of the article's own median day")
abline(v = c(2, 10, 100, 1000, 10000), col = "grey93")
points(sp$ratio, yy, pch = 19, cex = 0.55,
       col = adjustcolor(acol[sp$article], 0.5))
points(sp_top$ratio, yt, pch = 21, bg = "white", col = acol[sp_top$article],
       cex = 1.15, lwd = 1.8)
text(sp_top$ratio, yt, paste0(" ", format(sp_top$date, "%e %b")), pos = 4,
     cex = 0.58, col = acol[sp_top$article], xpd = NA)
axis(2, at = length(arts_o):1, labels = nice(arts_o), las = 1, tick = FALSE,
     cex.axis = 0.66)

## ---- spike-d3
rows <- paste(sprintf('{"a":"%s","d":%d,"r":%.2f,"v":%d}', nice(sp$article),
                      as.integer(sp$date), sp$ratio, sp$views), collapse = ",")
tops <- paste(sprintf('{"a":"%s","d":%d,"r":%.2f}', nice(sp_top$article),
                      as.integer(sp_top$date), sp_top$ratio), collapse = ",")
labs <- paste(sprintf('"%s"', nice(arts_o)), collapse = ",")
cols <- paste(sprintf('"%s"', acol[arts_o]), collapse = ",")
cat(sprintf('
<div id="spike" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], T=[%s], L=[%s], C=[%s];
const W=800,H=380,M={t:16,r:96,b:48,l:186};
const box=d3.select("#spike");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain(d3.extent(D,d=>d.r)).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(L).range([M.t,H-M.b]).padding(0.3);
const cm={}; L.forEach((l,i)=>cm[l]=C[i]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6,d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).select(".domain").remove();
svg.selectAll("g").selectAll("text").attr("font-size","11px");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("that day, as a multiple of the article\\u2019s own median day");
const cy=d=>y(d.a)+y.bandwidth()/2;
const fmt=d3.utcFormat("%%e %%B");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.r)).attr("cy",cy).attr("r",3)
  .attr("fill",d=>cm[d.a]).attr("fill-opacity",0.5)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(`<b>${d.a}</b><br>${fmt(new Date(d.d*86400000))}`+
      `<br>${d3.format(",")(d.v)} pageviews<br>${d3.format(",.0f")(d.r)}\\u00d7 its median`)
      .style("left",Math.min(e.offsetX+14,W-250)+"px").style("top",(e.offsetY-10)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("circle.t").data(T).join("circle")
  .attr("cx",d=>x(d.r)).attr("cy",cy).attr("r",5.5).attr("fill","#fff")
  .attr("stroke",d=>cm[d.a]).attr("stroke-width",2);
svg.append("g").selectAll("text.t").data(T).join("text")
  .attr("x",d=>x(d.r)+9).attr("y",d=>cy(d)+4).attr("font-size","10.5px")
  .attr("fill",d=>cm[d.a]).text(d=>d3.utcFormat("%%e %%b")(new Date(d.d*86400000)));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s spike-days across %d articles — %.1f%% of all article-days in the
file. The ringed dot on each row is that article’s largest day, dated. The
axis is logarithmic and runs to %s times median, which is %s on %s. Hover any
dot.</p>
', rows, tops, labs, cols, cnt(nrow(sp)), length(arts_o),
   100 * nrow(sp) / sum(!is.na(grid)), cnt(max(sp$ratio)),
   nice(sp$article[which.max(sp$ratio)]),
   format(sp$date[which.max(sp$ratio)], "%e %B")))

## ---- springfield
o <- g("Springfield,_Ohio")
o <- o[o$date >= as.Date("2024-09-07") & o$date <= as.Date("2024-09-14"),
       c("date", "views")]
o$multiple <- pc(o$views / med("Springfield,_Ohio"), 0)
o$views <- cnt(o$views)
names(o) <- c("date", "pageviews", "× the article's median day")
o

## ---- spring-prep
win  <- seq(as.Date("2024-09-01"), as.Date("2024-09-24"), by = "day")
spg  <- grid["Springfield,_Ohio", format(win)]
hai  <- grid["Haitian_Americans", format(win)]
deb  <- as.Date("2024-09-10")
pre  <- as.Date("2024-09-09")
sp_base <- median(g("Springfield,_Ohio")$views)

## ---- spring-static
par(mar = c(3.4, 4.8, 2.6, 1.4))
ylo <- max(1, min(c(spg, hai)) / 1.8)
plot(NA, xlim = range(win), ylim = c(ylo, max(spg, hai) * 1.6), log = "y",
     las = 1, bty = "n", xlab = "", ylab = "pageviews (log scale)", xaxt = "n",
     yaxt = "n")
axis(2, at = c(1, 10, 100, 1000, 10000, 1e5),
     labels = c("1", "10", "100", "1k", "10k", "100k"), las = 1,
     cex.axis = 0.8)
axis(2, at = c(1, 10, 100, 1000, 10000, 1e5), labels = FALSE, tcl = -0.2)
axis.Date(1, at = seq(min(win), max(win), by = "3 days"), format = "%e %b",
          cex.axis = 0.72)
rect(deb - 0.5, ylo, deb + 0.5, max(spg, hai) * 1.6, col = "#f0f0f0",
     border = NA)
segments(pre, ylo, pre, max(spg, hai) * 1.18, lty = 2, col = "#4d9221")
lines(win, pmax(spg, 1), type = "s", col = "#C41230", lwd = 2.2)
lines(win, pmax(hai, 1), type = "s", col = "#8856a7", lwd = 2.2)
points(pre, spg[format(pre)], pch = 19, col = "#4d9221", cex = 1.2)
abline(h = sp_base, lty = 3, col = "#999999")
text(min(win), sp_base * 1.5, paste0("Springfield's median day: ",
     cnt(sp_base)), cex = 0.62, col = "#777777", pos = 4)
text(pre, max(spg, hai) * 1.30,
     paste0("9 Sept: already\n", round(spg[format(pre)] / sp_base),
            "x its median"),
     cex = 0.62, col = "#4d9221", pos = 2)
text(deb, max(spg, hai) * 1.30, "10 Sept:\nthe debate", cex = 0.62,
     col = "#555555", pos = 4)
legend("bottomright", c("Springfield, Ohio", "Haitian Americans"),
       col = c("#C41230", "#8856a7"), lwd = 2.2, bty = "n", cex = 0.72)

## ---- spring-d3
rows <- paste(sprintf('[%d,%d,%d]', as.integer(win), spg, hai), collapse = ",")
cat(sprintf('
<div id="spring" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], DEB=%d, PRE=%d, BASE=%.1f;
const W=780,H=380,M={t:44,r:26,b:44,l:64};
const box=d3.select("#spring");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleUtc().domain([new Date(D[0][0]*86400000),
  new Date(D[D.length-1][0]*86400000)]).range([M.l,W-M.r]);
const mx=d3.max(D,d=>Math.max(d[1],d[2]));
const mn=d3.min(D,d=>Math.min(d[1],d[2]));
const y=d3.scaleLog().domain([Math.max(1,mn/1.8),mx*1.7]).range([H-M.b,M.t]);
const dd=i=>new Date(D[i][0]*86400000);
svg.append("rect").attr("x",x(new Date((DEB-0.5)*86400000))
  ).attr("y",M.t).attr("width",x(new Date((DEB+0.5)*86400000))-x(new Date((DEB-0.5)*86400000)))
  .attr("height",H-M.b-M.t).attr("fill","#f0f0f0");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d3.utcFormat("%%e %%b")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([1,10,100,1000,1e4,1e5]).tickFormat(d3.format("~s")));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("pageviews (log scale)");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(BASE)).attr("y2",y(BASE))
  .attr("stroke","#999").attr("stroke-dasharray","3,3");
svg.append("text").attr("x",M.l+6).attr("y",y(BASE)-6).attr("font-size","11px")
  .attr("fill","#777").text("Springfield\\u2019s median day: "+d3.format(",")(BASE));
svg.append("line").attr("x1",x(new Date(PRE*86400000))).attr("x2",x(new Date(PRE*86400000)))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#4d9221").attr("stroke-dasharray","5,4");
const ln=j=>d3.line().curve(d3.curveStepAfter)
  .x((d,i)=>x(dd(i))).y(d=>y(Math.max(d[j],1)));
[[1,"#C41230","Springfield, Ohio"],[2,"#8856a7","Haitian Americans"]].forEach(s=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",s[1])
    .attr("stroke-width",2.2).attr("d",ln(s[0]));
  const last=D[D.length-1];
  svg.append("text").attr("x",W-M.r-4).attr("y",y(Math.max(last[s[0]],1))-8)
    .attr("text-anchor","end").attr("font-size","11.5px").attr("fill",s[1]).text(s[2]);
});
const pi=D.findIndex(d=>d[0]===PRE);
svg.append("circle").attr("cx",x(dd(pi))).attr("cy",y(D[pi][1])).attr("r",5)
  .attr("fill","#4d9221");
svg.append("text").attr("x",x(dd(pi))-8).attr("y",M.t-22).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#4d9221")
  .text("9 Sept, before the debate");
svg.append("text").attr("x",x(dd(pi))-8).attr("y",M.t-8).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#4d9221")
  .text(d3.format(",")(D[pi][1])+" readers, "+Math.round(D[pi][1]/BASE)+"\\u00d7 median");
svg.append("text").attr("x",x(new Date(DEB*86400000))+8).attr("y",M.t-8)
  .attr("font-size","11.5px").attr("fill","#555").text("10 Sept: the debate");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect.h").data(D).join("rect")
  .attr("x",(d,i)=>x(dd(i))-8).attr("y",M.t).attr("width",16).attr("height",H-M.b-M.t)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(`<b>${d3.utcFormat("%%e %%B")(new Date(d[0]*86400000))}</b>`+
      `<br><span style="color:#C41230">\\u25a0</span> Springfield: ${d3.format(",")(d[1])}`+
      `<br><span style="color:#8856a7">\\u25a0</span> Haitian Americans: ${d3.format(",")(d[2])}`)
      .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(M.t+6)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The step up happens on the 9th, one day before the shaded debate: %s readers
against a median day of %s, %.0f times over. The debate then multiplied it again
to %s on the %s. Whatever started this was already running before the claim
reached a mass audience.</p>
', rows, as.integer(deb), as.integer(pre), sp_base,
   cnt(von("Springfield,_Ohio", "2024-09-09")), cnt(sp_base),
   von("Springfield,_Ohio", "2024-09-09") / sp_base,
   cnt(pk("Springfield,_Ohio")), pkd("Springfield,_Ohio")))

## ---- peaks
o <- data.frame(article = nice(peaks$article), peak = cnt(peaks$peak),
                when = peaks$when, baseline = cnt(peaks$baseline),
                ratio = paste0(pc(peaks$peak / peaks$baseline, 0), "×"))
names(o) <- c("person", "largest single day", "date", "median day",
              "peak ÷ median")
o

## ---- bump-prep
four <- c("Donald_Trump", "Kamala_Harris", "JD_Vance", "Tim_Walz")
# 7-day trailing mean. na.rm is kept from when the grid had a hole in it; with
# the titles summed there is nothing for it to remove, and every window is a
# full seven days.
rm7 <- sapply(four, function(a) {
  v <- grid[a, ]
  sapply(seq_along(days), function(i) mean(v[max(1, i - 6):i], na.rm = TRUE))
})
rk    <- t(apply(rm7, 1, function(z) rank(-z, ties.method = "first")))
bcol  <- c(Donald_Trump = "#C41230", Kamala_Harris = "#2c7fb8",
           JD_Vance = "#e08214", Tim_Walz = "#4d9221")
lead1 <- sapply(four, function(a) sum(rk[, a] == 1))

## ---- bump-static
par(mar = c(2.8, 3.0, 2.6, 8.6))
plot(NA, xlim = range(days), ylim = c(4.5, 0.5), yaxt = "n", xaxt = "n",
     bty = "n", xlab = "", ylab = "")
axis(2, at = 1:4, labels = paste0("#", 1:4), las = 1, tick = FALSE,
     cex.axis = 0.78)
axis.Date(1, at = seq(as.Date("2024-01-01"), as.Date("2024-12-01"),
                      by = "1 month"), format = "%b", tick = FALSE,
          cex.axis = 0.7, line = -1.0)
abline(v = ev$date, lty = 3, col = "grey85")
for (a in four) lines(days, rk[, a], col = bcol[a], lwd = 2, type = "s")
for (a in four) {
  text(max(days) + 4, rk[nrow(rk), a],
       paste0(nice(a), " (", lead1[a], " days at #1)"),
       pos = 4, cex = 0.66, col = bcol[a], xpd = NA)
}
mtext(paste0("Daily rank on a 7-day trailing mean, so one loud day cannot ",
             "flip a line by itself. Every window is a full seven days."),
      side = 3, line = 0.5, cex = 0.62, adj = 0)

## ---- bump-d3
ser <- paste(sapply(four, function(a) sprintf(
  '{"k":"%s","c":"%s","r":[%s],"one":%d}', nice(a), bcol[a],
  paste(rk[, a], collapse = ","), lead1[a])), collapse = ",")
evs <- paste(sprintf('{"i":%d,"t":"%s"}', match(ev$date, days) - 1,
                     gsub('"', "", ev$event)), collapse = ",")
cat(sprintf('
<div id="bump" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[%s], E=[%s], D0=%d;
const nd=S[0].r.length;
const W=800,H=300,M={t:22,r:236,b:34,l:46};
const box=d3.select("#bump");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,nd-1]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain([1,2,3,4]).range([M.t,H-M.b]).padding(0.5);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>"#"+d).tickSize(0)).select(".domain").remove();
E.forEach(e=>svg.append("line").attr("x1",x(e.i)).attr("x2",x(e.i))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#e2e2e2").attr("stroke-dasharray","3,3"));
[0,31,60,91,121,152,182,213,244,274,305,335].forEach(i=>
  svg.append("text").attr("x",x(i)).attr("y",H-10).attr("font-size","10.5px")
    .attr("fill","#888").text(d3.utcFormat("%%b")(new Date((D0+i)*86400000))));
const ln=d3.line().curve(d3.curveStepAfter).x((d,i)=>x(i)).y(d=>y(d));
let focus=null;
const gg=svg.append("g");
function draw(){
  gg.selectAll("path").data(S,s=>s.k).join("path")
    .attr("fill","none").attr("stroke",s=>s.c).attr("d",s=>ln(s.r))
    .attr("stroke-width",s=>focus===s.k?3:2)
    .attr("opacity",s=>focus&&focus!==s.k?0.15:0.95);
}
draw();
S.forEach(s=>{
  const g2=svg.append("g").attr("style","cursor:pointer")
    .on("mouseenter",()=>{focus=s.k;draw();})
    .on("mouseleave",()=>{focus=null;draw();});
  g2.append("text").attr("x",W-M.r+10).attr("y",y(s.r[nd-1])+4)
    .attr("font-size","11.5px").attr("fill",s.c)
    .text(s.k+" \\u2014 "+s.one+" days at #1");
});
const fmt=d3.utcFormat("%%e %%B");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#aaa").attr("opacity",0);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const i=Math.max(0,Math.min(nd-1,Math.round(x.invert(d3.pointer(e,this)[0]))));
    rule.attr("x1",x(i)).attr("x2",x(i)).attr("opacity",1);
    const ev=E.find(z=>z.i===i);
    const rows=S.slice().sort((a,b)=>a.r[i]-b.r[i]);
    tip.style("opacity",1).html(`<b>${fmt(new Date((D0+i)*86400000))}</b>`+
      (ev?`<br><i>${ev.t}</i>`:"")+"<br>"+
      rows.map(s=>`#${s.r[i]} <span style="color:${s.c}">\\u25a0</span> ${s.k}`).join("<br>"))
      .style("left",Math.min(x(i)+12,W-300)+"px").style("top",(M.t+2)+"px");
  }).on("mouseleave",()=>{rule.attr("opacity",0);tip.style("opacity",0);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Rank on a 7-day trailing mean, so a single loud day cannot flip a line by
itself. %s held first place on %d of the %d days and %s on %d. Every window is
a full seven days. Hover for a daily readout.</p>
', ser, evs, as.integer(min(days)), nice(names(which.max(lead1))),
   max(lead1), length(days), nice(names(sort(lead1, decreasing = TRUE))[2]),
   sort(lead1, decreasing = TRUE)[2]))

## ---- lead
o <- data.frame(period = rownames(lead),
                Harris = as.vector(lead[, "Harris"]),
                Trump  = as.vector(lead[, "Trump"]))
names(o) <- c("period", "days Harris led", "days Trump led")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
