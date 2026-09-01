# media-attention-code.R -- chunk bodies for media-attention-brief.Rmd
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

# The article about the election itself, named once so the prose can quote it
# without a chunk of its own.
elec <- "2024_United_States_presidential_election"

# Each running mate's baseline is the median day BEFORE they were named to a
# ticket, not the median day of the year: half of a running mate's annual
# median is the attention the announcement itself created.
BEFORE <- c(JD_Vance = "2024-07-15", Tim_Walz = "2024-08-06")
base_before <- function(a) median(g(a)$views[g(a)$date < as.Date(BEFORE[a])])

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
', ser, labs, mons, as.integer(min(days)), match(MOVE, days) - 1))

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
# Drawn with the shared chart library (_lib/dd-charts.js), reached through
# dd_fig(). Two series, one log axis and four annotations: the plainest kind of
# figure in the book, and the kind the library exists to stop being rewritten.
# d3 = FALSE because Figure 1 already put the d3 tag on the page.
sd <- data.frame(day         = as.integer(format(win, "%d")),
                 springfield = pmax(as.numeric(spg), 1),
                 haitian     = pmax(as.numeric(hai), 1))
SMAX <- max(sd$springfield, sd$haitian)
SMIN <- max(1, min(sd$springfield, sd$haitian) / 1.8)
dd_fig("spring", "step", sd, d3 = FALSE,
  size = list(w = 780, h = 380, m = list(t = 46, r = 148, b = 46, l = 66)),
  x = list(field = "day", fmt = "d", ticks = 8,
           label = "day of September 2024"),
  y = list(field = "springfield", log = TRUE, fmt = "comma", ticks = 5,
           domain = c(SMIN, SMAX * 1.9), label = "pageviews (log scale)"),
  series = list(fields = list(
    list(field = "springfield", label = "Springfield, Ohio",
         class = "series-1"),
    list(field = "haitian", label = "Haitian Americans",
         class = "series-4"))),
  endLabels = TRUE,
  annotations = list(
    dd_annot_band(9.5, 10.5, axis = "x"),
    dd_annot_hline(sp_base),
    dd_annot_vline(9),
    dd_annot_text(1, sp_base, paste0("median day: ", cnt(sp_base)),
                  class = "foot", size = 11, dy = -6),
    dd_annot_text(8.8, SMAX * 1.6, "9 Sept, before the debate",
                  class = "lbl", size = 11, anchor = "end"),
    dd_annot_text(10.7, SMAX * 1.6, "10 Sept: the debate",
                  class = "lbl", size = 11)),
  tip = dd_js('function(d){
    return "<b>"+d.day+" September</b><br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> Springfield, Ohio: "+
        d3.format(",")(d.springfield)+"<br>"+
      "<span class=\'series-4-txt\'>&#9632;</span> Haitian Americans: "+
        d3.format(",")(d.haitian);
  }'))

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
', ser, evs, as.integer(min(days))))

## ---- lead
o <- data.frame(period = rownames(lead),
                Harris = as.vector(lead[, "Harris"]),
                Trump  = as.vector(lead[, "Trump"]))
names(o) <- c("period", "days Harris led", "days Trump led")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
