# ces-class-code.R -- chunk bodies for ces-class-brief.Rmd
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

b <- read.csv("data/derived/ces2024_benchmarks.csv", stringsAsFactors = FALSE)
b$shift <- b$pct_weighted - b$pct_unweighted

cls_file    <- if (file.exists("data/raw/class_responses.csv"))
                 "data/raw/class_responses.csv" else
                 "data/derived/class_responses_EXAMPLE.csv"
PLACEHOLDER <- grepl("EXAMPLE", cls_file)
cls         <- read.csv(cls_file, stringsAsFactors = FALSE)
n_class     <- max(table(cls$variable))

N_CES <- max(tapply(b$n, b$variable, sum))
pc    <- function(x, k = 1) formatC(x, format = "f", digits = k)
cnt   <- function(x) format(round(x), big.mark = ",")
gv    <- function(v, cat_, col) b[[col]][b$variable == v & b$category == cat_]
moe   <- function(n) 100 * 1.96 * sqrt(0.25 / n)

# class shares for one variable, against both CES columns
share <- function(v) {
  x   <- cls$code[cls$variable == v]
  bb  <- b[b$variable == v, ]
  cn  <- as.vector(table(factor(x, levels = bb$code)))
  data.frame(category       = bb$category,
             class_pct      = round(100 * cn / sum(cn), 1),
             ces_unweighted = bb$pct_unweighted,
             ces_weighted   = bb$pct_weighted,
             stringsAsFactors = FALSE)
}

# plain-English name for each question, in file order
qlab <- c(gender4 = "gender", educ = "education", race = "race",
          pid7 = "party ID (7 point)", ideo5 = "ideology (5 point)",
          newsint = "news attention", votereg = "voter registration")

# how much of the sample each question's weights move, in total
# (half the sum of absolute category shifts = share of respondents reallocated)
adj <- data.frame(variable = names(tapply(abs(b$shift), b$variable, sum)),
                  idx = as.vector(tapply(abs(b$shift), b$variable, sum)) / 2,
                  stringsAsFactors = FALSE)
adj$label <- qlab[adj$variable]
adj <- adj[order(adj$idx), ]

# class-versus-CES gap for every answer category of every question
gaps <- do.call(rbind, lapply(names(qlab), function(v) {
  s <- share(v)
  s$variable <- v
  s$label    <- qlab[[v]]
  s$gap      <- s$class_pct - s$ces_weighted
  s
}))
gaps$out <- abs(gaps$gap) > moe(n_class)

# sample size a class would need to reach a +/- 3 point margin
n_for3 <- 0.25 * (1.96 / 0.03)^2

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

## ---- placeholder-warning
if (PLACEHOLDER) cat(
"> **A caveat about the class column, before any of it is read.** The class
> file currently committed to this folder is `class_responses_EXAMPLE.csv`.
> **It is placeholder data. The responses in it are invented and are not
> yours.** They exist so that the tables have the right shape while the real
> first-day survey is being processed. Everything drawn from the CES is real
> and can be checked; nothing in the class column is, until the real file
> replaces the placeholder. Where this document reports a class figure, treat
> it as an illustration of the arithmetic and not as a fact about this room.\n")
if (!PLACEHOLDER) cat(
"> The class file in use is `", basename(cls_file), "` — the real first-day
> responses.\n", sep = "")

## ---- ces-raw-class
CL <- readLines(cls_file, warn = FALSE)
# The file's own header line becomes the header row, and the eight lines under
# it become eight rows. Nothing is renamed or recoded: these are the digits as
# they sit on disk, which is the point the paragraph below makes.
.hdr  <- strsplit(CL[1], ",")[[1]]
.body <- do.call(rbind, lapply(CL[2:9], function(l) strsplit(l, ",")[[1]]))
.raw  <- as.data.frame(.body, stringsAsFactors = FALSE)
names(.raw) <- .hdr
.raw

## ---- ces-shape
RNG <- vapply(names(qlab), function(v) {
  k <- sort(b$code[b$variable == v])
  paste0(min(k), "..", max(k))
}, character(1))
data.frame(Column = names(qlab),
           Codes_that_occur = RNG,
           What_it_asks = unlist(qlab, use.names = FALSE))

## ---- ces-clean-race
o <- b[b$variable == "race", c("code", "category", "n", "pct_unweighted",
                               "pct_weighted")]
o$n <- cnt(o$n)
names(o) <- c("code", "category", "respondents", "% unweighted", "% weighted")
rownames(o) <- NULL
o

## ---- ces-mislabel
r <- b[b$variable == "race", ]
o <- data.frame(
  code = c(6, 8),
  `by the Guide's printed order` = c("Middle Eastern", "Two or more races"),
  `by matching counts` = r$category[match(c(6, 8), r$code)],
  respondents = cnt(r$n[match(c(6, 8), r$code)]),
  check.names = FALSE)
o

## ---- schema
data.frame(
  file = c("ces2024_benchmarks.csv", "class_responses(...).csv"),
  one_row_is = c("one answer category of one question",
                 "one student's answer to one question"),
  columns = c("variable, code, category, n, pct_unweighted, pct_weighted",
              "variable, code"),
  rows = c(nrow(b), nrow(cls)),
  check.names = FALSE)

## ---- one-row
o <- b[b$variable == "votereg" & b$category == "Yes", ]
names(o) <- c("variable", "code", "category", "respondents",
              "% unweighted", "% weighted", "shift")
o[, 1:6]

## ---- scope
o <- data.frame(
  variable = names(tapply(b$n, b$variable, sum)),
  categories = as.vector(table(b$variable)),
  respondents = cnt(as.vector(tapply(b$n, b$variable, sum))))
names(o) <- c("variable", "answer categories", "CES respondents")
o

## ---- pid7
o <- share("pid7")
names(o) <- c("party identification", "class (%)", "CES unweighted (%)",
              "CES weighted (%)")
o

## ---- pid7-caveat
if (PLACEHOLDER) cat(
"**Reminder: the first column is placeholder data, not this class.** The two\n",
"CES columns are real.\n")

## ---- adjust-static
par(mar = c(4, 9.6, 0.8, 2.8))
top <- which.max(adj$idx)
cl  <- ifelse(seq_len(nrow(adj)) == top, "#C41230", "#2c7fb8")
plot(NA, xlim = c(0, max(adj$idx) * 1.14), ylim = c(0.5, nrow(adj) + 0.5),
     yaxt = "n", ylab = "", las = 1, bty = "n",
     xlab = "% of the sample moved from one answer to another")
abline(v = pretty(c(0, max(adj$idx))), col = "grey92")
segments(0, seq_len(nrow(adj)), adj$idx, seq_len(nrow(adj)), col = cl, lwd = 2.4)
points(adj$idx, seq_len(nrow(adj)),
       pch = ifelse(seq_len(nrow(adj)) == top, 19, 21),
       bg = "white", col = cl, cex = 1.25, lwd = 2)
axis(2, at = seq_len(nrow(adj)), labels = adj$label, las = 1, tick = FALSE,
     cex.axis = 0.85)
text(adj$idx, seq_len(nrow(adj)), paste0(" ", pc(adj$idx)), pos = 4,
     cex = 0.76, col = cl, xpd = NA)

## ---- adjust-d3
rows <- paste(sprintf('{"l":"%s","v":"%s","i":%.2f}', adj$label, adj$variable,
                      adj$idx), collapse = ",")
cat(sprintf('
<div id="adj" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=330,M={t:14,r:64,b:44,l:150};
const box=d3.select("#adj");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const mx=d3.max(D,d=>d.i);
const x=d3.scaleLinear().domain([0,mx*1.06]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.l).reverse()).range([M.t,H-M.b]).padding(0.42);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).select(".domain").remove();
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("%% of the sample moved from one answer to another");
const col=d=>d.i===mx?"#C41230":"#2c7fb8";
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",x(0)).attr("x2",d=>x(d.i))
  .attr("y1",d=>y(d.l)+y.bandwidth()/2).attr("y2",d=>y(d.l)+y.bandwidth()/2)
  .attr("stroke",col).attr("stroke-width",2.6);
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.i)).attr("cy",d=>y(d.l)+y.bandwidth()/2).attr("r",5.5)
  .attr("fill",d=>d.i===mx?"#C41230":"#fff").attr("stroke",col)
  .attr("stroke-width",2);
svg.append("g").selectAll("text.val").data(D).join("text")
  .attr("x",d=>x(d.i)+11).attr("y",d=>y(d.l)+y.bandwidth()/2+4)
  .attr("font-size","11.5px").attr("fill",col).text(d=>d.i.toFixed(1));
})();
</script>
', rows))

## ---- shifts
o <- head(b[order(-abs(b$shift)),
            c("variable", "category", "pct_unweighted", "pct_weighted", "shift")], 8)
o$shift <- sprintf("%+.1f", o$shift)
names(o) <- c("variable", "category", "unweighted (%)", "weighted (%)",
              "shift (points)")
o

## ---- slope-prep
sl <- head(b[order(-abs(b$shift)), ], 8)
sl <- sl[order(-sl$pct_unweighted), ]
sl$lab <- paste0(sl$variable, ": ", sl$category)

# push overlapping labels apart, once, so both renderings agree
spread <- function(v, sep) {
  o <- order(v); z <- v[o]
  for (it in 1:400) {
    for (i in seq_along(z)[-1]) {
      d <- sep - (z[i] - z[i - 1])
      if (d > 0) { z[i - 1] <- z[i - 1] - d / 2; z[i] <- z[i] + d / 2 }
    }
    z[1] <- max(z[1], 0)
  }
  out <- numeric(length(v)); out[o] <- z; out
}
sl$luy <- spread(sl$pct_unweighted, 3.4)
sl$lwy <- spread(sl$pct_weighted,   3.4)

## ---- slope-static
par(mar = c(0.6, 11.6, 3.6, 9.4))
ytop <- max(sl$luy, sl$lwy)
plot(NA, xlim = c(0, 1), ylim = c(-2, ytop + 8), axes = FALSE, ann = FALSE)
cl <- ifelse(sl$shift > 0, "#2c7fb8", "#C41230")
segments(0, sl$pct_unweighted, 1, sl$pct_weighted, col = cl, lwd = 2.2)
segments(-0.035, sl$luy, 0, sl$pct_unweighted, col = cl, lwd = 0.7, xpd = NA)
segments(1, sl$pct_weighted, 1.035, sl$lwy, col = cl, lwd = 0.7, xpd = NA)
points(rep(0, nrow(sl)), sl$pct_unweighted, pch = 19, col = cl, cex = 1.05)
points(rep(1, nrow(sl)), sl$pct_weighted, pch = 19, col = cl, cex = 1.05)
text(-0.045, sl$luy, paste0(sl$lab, "  ", pc(sl$pct_unweighted)), pos = 2,
     cex = 0.66, col = cl, xpd = NA)
text(1.045, sl$lwy, paste0(pc(sl$pct_weighted), "  ",
     sprintf("%+.1f", sl$shift)), pos = 4, cex = 0.66, col = cl, xpd = NA)
text(c(0, 1), rep(ytop + 6, 2), c("collected", "published"), cex = 0.84,
     font = 2, col = "#444444", xpd = NA)
mtext(paste0("The eight largest adjustments in the file: ",
             sum(sl$shift > 0), " groups grow, ", sum(sl$shift < 0),
             " shrink."), side = 3, line = 2.2, cex = 0.7, adj = 0)
mtext(paste0("The steepest line is ", sl$lab[which.max(abs(sl$shift))],
             ", ", pc(max(abs(sl$shift))), " points."),
      side = 3, line = 1.3, cex = 0.7, adj = 0)

## ---- slope-d3
rows <- paste(sprintf('{"l":"%s","u":%.1f,"w":%.1f,"s":%.1f,"n":%d,"ly":%.2f,"ry":%.2f}',
                      sl$lab, sl$pct_unweighted, sl$pct_weighted, sl$shift,
                      sl$n, sl$luy, sl$lwy), collapse = ",")
cat(sprintf('
<div id="slp" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:44,r:190,b:20,l:250};
const box=d3.select("#slp");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.ly,d.ry))*1.04])
  .range([H-M.b,M.t]);
const x0=M.l, x1=W-M.r;
const col=d=>d.s>0?"#2c7fb8":"#C41230";
svg.append("text").attr("x",x0).attr("y",M.t-18).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600").attr("fill","#444")
  .text("collected");
svg.append("text").attr("x",x1).attr("y",M.t-18).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600").attr("fill","#444")
  .text("published");
const g=svg.append("g");
g.selectAll("line").data(D).join("line")
  .attr("x1",x0).attr("x2",x1).attr("y1",d=>y(d.u)).attr("y2",d=>y(d.w))
  .attr("stroke",col).attr("stroke-width",2.4);
[["u",x0],["w",x1]].forEach(k=>{
  g.selectAll("circle."+k[0]).data(D).join("circle")
    .attr("cx",k[1]).attr("cy",d=>y(d[k[0]])).attr("r",5).attr("fill",col);
});
g.selectAll("line.ll").data(D).join("line")
  .attr("x1",x0-26).attr("x2",x0).attr("y1",d=>y(d.ly)).attr("y2",d=>y(d.u))
  .attr("stroke",col).attr("stroke-width",0.8);
g.selectAll("line.rl").data(D).join("line")
  .attr("x1",x1).attr("x2",x1+26).attr("y1",d=>y(d.w)).attr("y2",d=>y(d.ry))
  .attr("stroke",col).attr("stroke-width",0.8);
g.selectAll("text.l").data(D).join("text")
  .attr("x",x0-32).attr("y",d=>y(d.ly)+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill",col)
  .text(d=>d.l+"  "+d.u.toFixed(1)+"%%");
g.selectAll("text.r").data(D).join("text")
  .attr("x",x1+32).attr("y",d=>y(d.ry)+4).attr("font-size","11.5px")
  .attr("fill",col)
  .text(d=>d.w.toFixed(1)+"%%  "+d3.format("+.1f")(d.s));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The eight largest adjustments in the file. %d of them grow and %d shrink. The
steepest line is %s at %.1f points; the next steepest, %s at %.1f, is the same
question’s mirror image — the people the weights move out of one
answer have to land in another.</p>
', rows, sum(sl$shift > 0), sum(sl$shift < 0),
   sl$lab[order(-abs(sl$shift))][1], sort(abs(sl$shift), decreasing = TRUE)[1],
   sl$lab[order(-abs(sl$shift))][2], sort(abs(sl$shift), decreasing = TRUE)[2]))

## ---- shift-static
s <- b[order(b$shift), ]
lab <- paste0(substr(s$variable, 1, 8), ": ", substr(s$category, 1, 22))
par(mar = c(4, 13, 1, 2))
barplot(s$shift, horiz = TRUE, names.arg = lab, las = 1, cex.names = 0.52,
        col = ifelse(s$shift > 0, "#2c7fb8", "#C41230"), border = NA,
        xlab = "weighted minus unweighted (percentage points)")
abline(v = 0)

## ---- shift-d3
s <- b[order(b$shift), ]
rows <- paste(sprintf('{"v":"%s","c":"%s","u":%.1f,"w":%.1f,"s":%.1f,"n":%d}',
                      s$variable, gsub('"', "", s$category),
                      s$pct_unweighted, s$pct_weighted, s$shift, s$n),
              collapse = ",")
cat(sprintf('
<div id="ces" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=770,H=760,M={t:16,r:24,b:40,l:250};
const svg=d3.select("#ces").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-20,17]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.v+": "+d.c)).range([M.t,H-M.b]).padding(0.18);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d=>d3.format("+d")(d)));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).selectAll("text").attr("font-size","10.5px");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("weighted minus unweighted (percentage points)");
const tip=d3.select("#ces").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>Math.min(x(0),x(d.s))).attr("y",d=>y(d.v+": "+d.c))
  .attr("width",d=>Math.abs(x(d.s)-x(0))).attr("height",y.bandwidth())
  .attr("fill",d=>d.s>0?"#2c7fb8":"#C41230").attr("fill-opacity",0.85)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(
      `<b>${d.v}: ${d.c}</b><br>collected ${d.u}%%<br>published ${d.w}%%<br>`+
      `shift ${d3.format("+.1f")(d.s)} points<br>${d3.format(",")(d.n)} respondents`)
      .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-10)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#333");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
All %d answer categories. Red bars are groups the panel over-represented; blue
bars are groups it missed. Hover for the underlying counts.</p>
', rows, nrow(b)))

## ---- collapse
p   <- b[b$variable == "pid7", ]
grp <- ifelse(p$code %in% 1:3, "Democrat",
       ifelse(p$code == 4,     "Independent",
       ifelse(p$code %in% 5:7, "Republican", "Not sure")))
a   <- aggregate(cbind(pct_unweighted, pct_weighted) ~ grp,
                 data = cbind(p, grp), sum)
names(a) <- c("group", "CES unweighted (%)", "CES weighted (%)")
a

## ---- margin-calc
dr_raw <- a[a$group == "Democrat", 2] - a[a$group == "Republican", 2]
dr_wtd <- a[a$group == "Democrat", 3] - a[a$group == "Republican", 3]

## ---- alluv-prep
pp   <- b[b$variable == "pid7", ]
ordg <- c("Democrat", "Independent", "Republican", "Not sure")
pg   <- ifelse(pp$code %in% 1:3, "Democrat",
        ifelse(pp$code == 4,     "Independent",
        ifelse(pp$code %in% 5:7, "Republican", "Not sure")))
oo   <- order(match(pg, ordg), pp$code)
pp   <- pp[oo, ]; pg <- pg[oo]
lu1  <- cumsum(pp$pct_unweighted); lu0 <- c(0, head(lu1, -1))
lw1  <- cumsum(pp$pct_weighted);   lw0 <- c(0, head(lw1, -1))
gcol <- c(Democrat = "#2c7fb8", Independent = "#999999",
          Republican = "#C41230", `Not sure` = "#8856a7")
gsum <- sapply(ordg, function(z) c(u = sum(pp$pct_unweighted[pg == z]),
                                   w = sum(pp$pct_weighted[pg == z])))
gu1  <- cumsum(gsum["u", ]); gu0 <- c(0, head(gu1, -1))
gw1  <- cumsum(gsum["w", ]); gw0 <- c(0, head(gw1, -1))
sigy <- function(a, b_, n = 48) {
  tt <- seq(0, 1, length.out = n); a + (b_ - a) * (1 - cos(pi * tt)) / 2
}
sigx <- function(n = 48) seq(0, 1, length.out = n)

## ---- alluv-static
par(mar = c(0.6, 12.4, 2.4, 11.0))
plot(NA, xlim = c(0, 1), ylim = c(100, 0), axes = FALSE, ann = FALSE)
xx <- sigx()
for (i in seq_len(nrow(pp))) {
  polygon(c(xx, rev(xx)),
          c(sigy(lu0[i], lw0[i]), rev(sigy(lu1[i], lw1[i]))),
          col = adjustcolor(gcol[pg[i]], 0.55), border = "white", lwd = 0.6)
}
text(-0.03, (lu0 + lu1) / 2,
     paste0(pp$category, "  ", pc(pp$pct_unweighted)), pos = 2, cex = 0.66,
     col = gcol[pg], xpd = NA)
rect(1.0, gw0, 1.045, gw1, col = gcol[ordg], border = "white", xpd = NA)
text(1.055, (gw0 + gw1) / 2, paste0(ordg, "  ", pc(gsum["w", ])), pos = 4,
     cex = 0.72, col = gcol[ordg], xpd = NA)
text(c(0, 1), c(-3, -3), c("collected", "published"), cex = 0.8, font = 2,
     col = "#444444", xpd = NA)
mtext(paste0("Seven-point party ID collapsed to three groups. Collected: D+",
             pc(dr_raw), ".  Published: D+", pc(dr_wtd), "."),
      side = 3, line = 1.0, cex = 0.72, adj = 0)

## ---- alluv-d3
rib <- paste(sprintf('{"c":"%s","g":"%s","col":"%s","u0":%.2f,"u1":%.2f,"w0":%.2f,"w1":%.2f,"u":%.1f,"w":%.1f}',
                     pp$category, pg, gcol[pg], lu0, lu1, lw0, lw1,
                     pp$pct_unweighted, pp$pct_weighted), collapse = ",")
grp <- paste(sprintf('{"g":"%s","col":"%s","y0":%.2f,"y1":%.2f,"w":%.1f,"u":%.1f}',
                     ordg, gcol[ordg], gw0, gw1, gsum["w", ], gsum["u", ]),
             collapse = ",")
cat(sprintf('
<div id="alv" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[%s], G=[%s];
const W=760,H=460,M={t:44,r:196,b:16,l:212};
const box=d3.select("#alv");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,100]).range([M.t,H-M.b]);
const x0=M.l, x1=W-M.r, N=48;
function ribbon(d){
  const pts=[];
  for(let i=0;i<N;i++){const t=i/(N-1), e=(1-Math.cos(Math.PI*t))/2;
    pts.push([x0+(x1-x0)*t, y(d.u0+(d.w0-d.u0)*e)]);}
  for(let i=N-1;i>=0;i--){const t=i/(N-1), e=(1-Math.cos(Math.PI*t))/2;
    pts.push([x0+(x1-x0)*t, y(d.u1+(d.w1-d.u1)*e)]);}
  return "M"+pts.map(p=>p.join(",")).join("L")+"Z";
}
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("path").data(R).join("path")
  .attr("d",ribbon).attr("fill",d=>d.col).attr("fill-opacity",0.55)
  .attr("stroke","#fff").attr("stroke-width",0.8)
  .on("mousemove",function(e,d){
    d3.select(this).attr("fill-opacity",0.85);
    tip.style("opacity",1).html(`<b>${d.c}</b><br>collected ${d.u}%%<br>`+
      `published ${d.w}%%<br>${d3.format("+.1f")(d.w-d.u)} points<br>`+
      `counted as ${d.g}`)
      .style("left",Math.min(e.offsetX+14,W-230)+"px").style("top",(e.offsetY-10)+"px");
  })
  .on("mouseleave",function(){d3.select(this).attr("fill-opacity",0.55);
    tip.style("opacity",0);});
svg.append("g").selectAll("text").data(R).join("text")
  .attr("x",x0-10).attr("y",d=>y((d.u0+d.u1)/2)+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill",d=>d.col)
  .text(d=>d.c+"  "+d.u.toFixed(1)+"%%");
svg.append("g").selectAll("rect").data(G).join("rect")
  .attr("x",x1).attr("y",d=>y(d.y0)).attr("width",13)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.col);
svg.append("g").selectAll("text.g").data(G).join("text")
  .attr("x",x1+20).attr("y",d=>y((d.y0+d.y1)/2)+4).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill",d=>d.col)
  .text(d=>d.g+"  "+d.w.toFixed(1)+"%%");
[[x0,"collected"],[x1,"published"]].forEach(p=>
  svg.append("text").attr("x",p[0]).attr("y",M.t-16).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600").attr("fill","#444").text(p[1]));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Ribbon width is the share of the sample. The Democratic block enters at %.1f%%
and leaves at %.1f%%, the Republican block at %.1f%% and %.1f%%: a collected
margin of D+%.1f becomes a published margin of D+%.1f. Hover a ribbon for the
answer option behind it.</p>
', rib, grp, gsum["u", "Democrat"], gsum["w", "Democrat"],
   gsum["u", "Republican"], gsum["w", "Republican"], dr_raw, dr_wtd))

## ---- waffle-prep
cx  <- cls$code[cls$variable == "pid7"]
cg  <- ifelse(cx %in% 1:3, "Democrat",
       ifelse(cx == 4,     "Independent",
       ifelse(cx %in% 5:7, "Republican", "Not sure")))
wg  <- c("Democrat", "Independent", "Republican", "Not sure")
cg  <- factor(cg, levels = wg)
cg  <- sort(cg)
wcol <- c(Democrat = "#2c7fb8", Independent = "#999999",
          Republican = "#C41230", `Not sure` = "#8856a7")
ncol_w <- 10
wtab   <- table(cg); wtab <- wtab[wtab > 0]
ratio  <- N_CES / n_class

## ---- waffle-static
nr <- ceiling(length(cg) / ncol_w)
ix <- seq_along(cg) - 1
par(mar = c(3.0, 0.6, 2.2, 0.6))
plot(NA, xlim = c(-0.4, ncol_w + 0.4), ylim = c(nr + 0.4, -0.4), axes = FALSE,
     ann = FALSE, asp = 1)
rect(ix %% ncol_w, ix %/% ncol_w, ix %% ncol_w + 0.86,
     ix %/% ncol_w + 0.86, col = wcol[as.character(cg)], border = "white",
     lwd = 1.4)
mtext(paste0("All ", n_class, " class respondents to the party question, ",
             "one square each: ",
             paste(paste0(as.vector(wtab), " ", names(wtab)),
                   collapse = ",  "), "."),
      side = 3, line = 0.7, cex = 0.7, adj = 0)
mtext(paste0("The CES has ", cnt(N_CES), " of these squares - ",
             cnt(ratio), " times as many. At this size its grid would run to ",
             cnt(ceiling(N_CES / ncol_w)), " rows."),
      side = 1, line = 0.4, cex = 0.7, adj = 0)
if (PLACEHOLDER) mtext("PLACEHOLDER DATA - these responses are invented",
                       side = 1, line = 1.5, cex = 0.74, adj = 0,
                       col = "#C41230", font = 2)

## ---- waffle-d3
cells <- paste(sprintf('{"g":"%s","c":"%s"}', as.character(cg),
                       wcol[as.character(cg)]), collapse = ",")
keys  <- paste(sprintf('{"g":"%s","c":"%s","n":%d}', names(wtab),
                       wcol[names(wtab)], as.vector(wtab)), collapse = ",")
ph    <- if (PLACEHOLDER)
  '<div style="background:#C41230;color:#fff;font-weight:600;font-size:12px;padding:5px 9px;border-radius:3px;display:inline-block;margin-bottom:8px">PLACEHOLDER DATA &mdash; these responses are invented</div>' else ""
cat(sprintf('
<div id="waf" style="position:relative;margin:1em 0">%s</div>
<script>
(function(){
const D=[%s], K=[%s], NC=10, CELL=34;
const nr=Math.ceil(D.length/NC);
const W=NC*CELL+30, H=nr*CELL+56;
const box=d3.select("#waf");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:%dpx;height:auto;font:12px inherit;display:block");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",(d,i)=>(i%%NC)*CELL).attr("y",(d,i)=>Math.floor(i/NC)*CELL)
  .attr("width",CELL-4).attr("height",CELL-4).attr("fill",d=>d.c)
  .append("title").text(d=>d.g);
const key=svg.append("g").attr("transform",`translate(0,${nr*CELL+18})`);
let cx=0;
K.forEach(k=>{
  key.append("rect").attr("x",cx).attr("y",0).attr("width",11).attr("height",11)
    .attr("fill",k.c);
  key.append("text").attr("x",cx+16).attr("y",10).attr("font-size","11.5px")
    .attr("fill",k.c).text(k.n+" "+k.g);
  cx+=32+7.2*(String(k.n).length+k.g.length);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%d squares. The CES is %s of them — %s times as many — which at this
size would be a grid %s rows deep. That ratio is the whole of the difference
between ±%.1f points and ±%.2f.</p>
', ph, cells, keys, 10 * 34 + 30, length(cg), cnt(N_CES), cnt(ratio),
   cnt(ceiling(N_CES / 10)), moe(n_class), moe(N_CES)))

## ---- moe-table
o <- data.frame(
  survey = c("This class", "A typical national poll", "CES 2024"),
  n = c(cnt(n_class), "1,000", cnt(N_CES)),
  margin = c(pc(moe(n_class)), pc(moe(1000)), pc(moe(N_CES), 2)))
names(o) <- c("survey", "respondents", "margin of error (± points)")
o

## ---- pid-static
s  <- share("pid7"); s <- s[nrow(s):1, ]
yy <- seq_len(nrow(s)); mm <- moe(n_class)
lo <- pmax(0, s$class_pct - mm); hi <- pmin(100, s$class_pct + mm)
par(mar = c(4, 11.4, 1.4, 1.4))
plot(NA, xlim = c(0, max(hi, s$ces_weighted) + 2), ylim = c(0.5, nrow(s) + 1.1),
     yaxt = "n", bty = "n", las = 1, ylab = "",
     xlab = "share of respondents (%)")
segments(s$class_pct, yy, s$ces_weighted, yy, col = "grey70", lwd = 1.4)
segments(lo, yy, hi, yy, col = "#C41230", lwd = 1.1)
segments(c(lo, hi), rep(yy, 2) - 0.15, c(lo, hi), rep(yy, 2) + 0.15,
         col = "#C41230", lwd = 1.1)
points(s$ces_weighted, yy, pch = 24, bg = "white", col = "#2c7fb8",
       cex = 1.1, lwd = 1.8)
points(s$class_pct, yy, pch = 19, col = "#C41230", cex = 1.15)
axis(2, at = yy, labels = s$category, las = 1, tick = FALSE, cex.axis = 0.78)
legend("topright", c(paste0("class (n=", n_class, "), ±", pc(mm), " points"),
                     "CES 2024, weighted"),
       pch = c(19, 24), col = c("#C41230", "#2c7fb8"), pt.bg = "white",
       bty = "n", cex = 0.76)

## ---- pid-d3
s    <- share("pid7")
mm   <- moe(n_class)
rows <- paste(sprintf('{"c":"%s","k":%.1f,"w":%.1f,"lo":%.1f,"hi":%.1f}',
                      s$category, s$class_pct, s$ces_weighted,
                      pmax(0, s$class_pct - mm), pmin(100, s$class_pct + mm)),
              collapse = ",")
inside <- sum(abs(s$class_pct - s$ces_weighted) <= mm)
cat(sprintf('
<div id="pid" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=360,M={t:30,r:24,b:44,l:186};
const box=d3.select("#pid");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.hi,d.w))+2]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.c)).range([M.t,H-M.b]).padding(0.34);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).select(".domain").remove();
svg.selectAll("g").selectAll("text").attr("font-size","11px");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("share of respondents");
const cy=d=>y(d.c)+y.bandwidth()/2;
const g=svg.append("g");
g.selectAll("line.j").data(D).join("line")
  .attr("x1",d=>x(d.k)).attr("x2",d=>x(d.w)).attr("y1",cy).attr("y2",cy)
  .attr("stroke","#bbb").attr("stroke-width",1.6);
g.selectAll("line.e").data(D).join("line")
  .attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi)).attr("y1",cy).attr("y2",cy)
  .attr("stroke","#C41230").attr("stroke-width",1.2);
[["lo",-1],["hi",1]].forEach(p=>{
  g.selectAll("line.c"+p[0]).data(D).join("line")
    .attr("x1",d=>x(d[p[0]])).attr("x2",d=>x(d[p[0]]))
    .attr("y1",d=>cy(d)-5).attr("y2",d=>cy(d)+5)
    .attr("stroke","#C41230").attr("stroke-width",1.2);
});
g.selectAll("path.w").data(D).join("path")
  .attr("d",d3.symbol().type(d3.symbolTriangle).size(52))
  .attr("transform",d=>`translate(${x(d.w)},${cy(d)})`)
  .attr("fill","#fff").attr("stroke","#2c7fb8").attr("stroke-width",1.8);
g.selectAll("circle.k").data(D).join("circle")
  .attr("cx",d=>x(d.k)).attr("cy",cy).attr("r",5).attr("fill","#C41230");
const key=svg.append("g").attr("font-size","11px");
key.append("circle").attr("cx",W-M.r-186).attr("cy",M.t-16).attr("r",5).attr("fill","#C41230");
key.append("text").attr("x",W-M.r-176).attr("y",M.t-12).attr("fill","#C41230")
  .text("class (n=%d), \\u00b1%.1f points");
key.append("path").attr("d",d3.symbol().type(d3.symbolTriangle).size(52))
  .attr("transform",`translate(${W-M.r-56},${M.t-16})`)
  .attr("fill","#fff").attr("stroke","#2c7fb8").attr("stroke-width",1.8);
key.append("text").attr("x",W-M.r-46).attr("y",M.t-12).attr("fill","#2c7fb8")
  .text("CES, weighted");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The red bar is what a class of %d can resolve. The CES value falls inside it
for %d of the %d party categories, so on this question the class and the
country are statistically indistinguishable.</p>
', rows, n_class, mm, n_class, inside, nrow(s)))

## ---- moe-static
nn <- seq(10, 2000, by = 5)
plot(nn, moe(nn), type = "l", lwd = 2.4, col = "#54278F", log = "x", las = 1,
     xlab = "respondents (log scale)", ylab = "margin of error (± points)")
abline(h = c(3, moe(n_class)), lty = 3, col = "grey55")
points(n_class, moe(n_class), pch = 19, col = "#C41230", cex = 1.3)
points(1000, moe(1000), pch = 19, col = "#2c7fb8", cex = 1.3)
text(n_class, moe(n_class) - 2.2, paste0("this class (n=", n_class, ")"),
     cex = 0.8, pos = 4)
text(1000, moe(1000) + 2.4, "a national poll (n=1,000)", cex = 0.8, pos = 4)

## ---- moe-d3
nn <- unique(round(exp(seq(log(10), log(3000), length.out = 220))))
pts <- paste(sprintf('[%d,%.2f]', nn, moe(nn)), collapse = ",")
cat(sprintf('
<div id="moe" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=380,M={t:18,r:130,b:44,l:56};
const svg=d3.select("#moe").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([10,3000]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,32]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6,d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>"\\u00b1"+d).ticks(7));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("respondents (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("margin of error (points)");
svg.append("path").datum(D).attr("fill","none").attr("stroke","#54278F")
  .attr("stroke-width",2.6).attr("d",d3.line().x(d=>x(d[0])).y(d=>y(d[1])));
const marks=[{n:%d,c:"#C41230",t:"this class"},{n:1000,c:"#2c7fb8",t:"a national poll"}];
marks.forEach(m=>{
  const v=196*Math.sqrt(0.25/m.n);
  svg.append("circle").attr("cx",x(m.n)).attr("cy",y(v)).attr("r",5.5).attr("fill",m.c);
  svg.append("text").attr("x",x(m.n)+10).attr("y",y(v)-8).attr("font-size","11.5px")
    .attr("fill",m.c).text(`${m.t} (n=${m.n}) \\u00b1${v.toFixed(1)}`);
});
})();
</script>
', pts, n_class))

## ---- two-failures
data.frame(
  problem = c("Bias", "Imprecision"),
  what_goes_wrong = c("the sample is systematically unlike the population",
                      "the sample is too small to resolve anything"),
  where_you_saw_it = c(paste0("CES: registration off by ",
                              pc(abs(gv("votereg", "Yes", "shift"))),
                              " points before weighting"),
                       paste0("this class: ±", pc(moe(n_class)),
                              " points on every estimate")),
  does_a_bigger_sample_fix_it = c("No", "Yes, slowly"),
  does_weighting_fix_it = c("Only for variables you measured", "No"),
  check.names = FALSE)

## ---- allgap-static
mm <- moe(n_class)
yj <- length(qlab) + 1 - match(gaps$variable, names(qlab))
mx <- max(abs(gaps$gap)) * 1.08
par(mar = c(4, 9.6, 1.6, 1.4))
plot(NA, xlim = c(-mx, mx), ylim = c(0.4, length(qlab) + 0.9), yaxt = "n",
     bty = "n", las = 1, ylab = "",
     xlab = "class minus CES weighted (percentage points)")
rect(-mm, 0.3, mm, length(qlab) + 0.7, col = "#eeeeee", border = NA)
segments(0, 0.3, 0, length(qlab) + 0.7, col = "grey40")
segments(c(-mm, mm), 0.3, c(-mm, mm), length(qlab) + 0.7, lty = 3,
         col = "grey45")
points(gaps$gap[!gaps$out], yj[!gaps$out], pch = 21, bg = "white",
       col = "#2c7fb8", cex = 1.05, lwd = 1.6)
points(gaps$gap[gaps$out], yj[gaps$out], pch = 19, col = "#C41230", cex = 1.3)
text(gaps$gap[gaps$out], yj[gaps$out], gaps$category[gaps$out], pos = 3,
     cex = 0.72, col = "#C41230")
axis(2, at = seq_along(qlab), labels = rev(qlab), las = 1, tick = FALSE,
     cex.axis = 0.85)
text(-mx, length(qlab) + 0.82, paste0("gray band = ±", pc(mm),
     " points, the margin of error at n = ", n_class),
     adj = c(0, 0.5), cex = 0.74, col = "#555")

## ---- allgap-d3
mm   <- moe(n_class)
rows <- paste(sprintf('{"l":"%s","c":"%s","g":%.1f,"k":%.1f,"w":%.1f,"o":%d}',
                      gaps$label, gsub('"', "", gaps$category), gaps$gap,
                      gaps$class_pct, gaps$ces_weighted, as.integer(gaps$out)),
              collapse = ",")
ord <- paste(sprintf('"%s"', qlab), collapse = ",")
cat(sprintf('
<div id="agp" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],L=[%s],MM=%.2f;
const W=760,H=360,M={t:34,r:26,b:44,l:150};
const box=d3.select("#agp");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const mx=d3.max(D,d=>Math.abs(d.g))*1.08;
const x=d3.scaleLinear().domain([-mx,mx]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(L).range([M.t,H-M.b]).padding(0.3);
svg.append("rect").attr("x",x(-MM)).attr("y",M.t).attr("width",x(MM)-x(-MM))
  .attr("height",H-M.b-M.t).attr("fill","#eeeeee");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d3.format("+d")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).select(".domain").remove();
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#666");
[-MM,MM].forEach(v=>svg.append("line").attr("x1",x(v)).attr("x2",x(v))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#999").attr("stroke-dasharray","3,3"));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("class minus CES weighted (percentage points)");
svg.append("text").attr("x",M.l).attr("y",M.t-12).attr("font-size","11px")
  .attr("fill","#555")
  .text("gray band = \\u00b1"+MM.toFixed(1)+" points, the margin of error at n = %d");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.g)).attr("cy",d=>y(d.l)+y.bandwidth()/2).attr("r",d=>d.o?6:5)
  .attr("fill",d=>d.o?"#C41230":"#fff").attr("stroke",d=>d.o?"#C41230":"#2c7fb8")
  .attr("stroke-width",1.7)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(
      `<b>${d.c}</b><br>class ${d.k}%%<br>CES weighted ${d.w}%%<br>`+
      `gap ${d3.format("+.1f")(d.g)} points`)
      .style("left",Math.min(e.offsetX+14,W-230)+"px").style("top",(e.offsetY-10)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.o").data(D.filter(d=>d.o)).join("text")
  .attr("x",d=>x(d.g)).attr("y",d=>y(d.l)+y.bandwidth()/2-10)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#C41230")
  .text(d=>d.c);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One circle per answer category. %d of the %d comparisons clear the margin of
error; the other %d sit inside the band, where the class and the country cannot
be told apart. Hover for both underlying percentages.</p>
', rows, ord, mm, n_class, sum(gaps$out), nrow(gaps), sum(!gaps$out)))

## ---- placeholder-close
if (PLACEHOLDER) cat(
"\nNone of that is yet a statement about any particular room, because the class\n",
"file in use is the placeholder. The arithmetic is correct; the responses it\n",
"was applied to are invented.\n")

## ---- weak-placeholder
if (PLACEHOLDER) cat(
"\nTo which one more must be added here: **the class responses in use are\n",
"placeholder data**, invented to give the tables their shape. Every class\n",
"figure in this document is illustrative only.\n")

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
