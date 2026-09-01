# bellwether-code.R -- chunk bodies for bellwether-brief.Rmd
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
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

w   <- read.csv("data/derived/county_winners.csv", colClasses = c(fips = "character"))
nat <- read.csv("data/derived/national.csv")
xc  <- read.csv("data/derived/crosscheck.csv", colClasses = c(fips = "character"))

YRS <- nat$year
K   <- length(YRS)
N   <- nrow(w)
V   <- function(y) w[[paste0("y", y)]]
natwin <- function(kind, y) nat[[kind]][match(y, nat$year)]
hits   <- function(kind, yy = YRS)
  rowSums(sapply(yy, function(y) V(y) == natwin(kind, y)))

h_ec <- hits("ec")
h_pv <- hits("pv")

# --- the Wall Street Journal's nineteen -------------------------------------
WIN80 <- seq(1980, 2016, 4)
h80   <- hits("ec", WIN80)
p19   <- w[h80 == length(WIN80), ]
p19   <- p19[order(p19$state, p19$county), ]
n_fail20 <- sum(p19$y2020 != natwin("ec", 2020))
surv     <- p19[p19$y2020 == natwin("ec", 2020), ]

# --- the counting argument --------------------------------------------------
kexp <- data.frame(k = c(3, 5, 7, 10, 13, 17))
kexp$from <- YRS[K - kexp$k + 1]
kexp$expected <- N / 2^kexp$k
kexp$observed <- sapply(kexp$k, function(k) sum(hits("ec", tail(YRS, k)) == k))

# --- how much the map moves at all ------------------------------------------
flip <- data.frame(
  year = YRS[-1],
  pct  = 100 * sapply(2:K, function(i) mean(V(YRS[i]) != V(YRS[i - 1]))))
r20 <- 100 * mean(V(2020) == "R")

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
# Title-case each word, not just the first: the source county names are all
# upper case, so a naive sub() turns VAN BUREN into "Van buren".
cap <- function(nm) gsub("\\b([a-z])", "\\U\\1", tolower(nm), perl = TRUE)
namef <- function(f) {
  i <- match(f, w$fips)
  paste0(cap(w$county[i]), ", ", w$state[i])
}

# --- palette, for the STATIC twins and the hand-written D3 ------------------
# The quantity nearly every figure carries is "did this county match the
# national winner", which is NOT a party: the national winner is a Democrat in
# eight of these elections and a Republican in nine. Red and blue would be read
# as party whatever the legend said, so matched/missed gets a diverging pair
# that carries no partisan reading.
MATCH <- "#1B7837"   # the county went with the national winner
MISS  <- "#762A83"   # it did not
NEUTC <- "#999999"
REFC  <- "#C41230"   # a reference mark: an expectation, a named case

.hdr <- function(x) sub("^(.)", "\\U\\1", gsub("_", " ", names(x)), perl = TRUE)

knit_print.data.frame <- function(x, ...) {
  knitr::knit_print(knitr::kable(x, col.names = .hdr(x), row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# A table that must not straddle a page break.
#
# Pandoc renders every markdown table as a `longtable`, whose header is set to
# repeat via \endhead. That is right for a table long enough to span pages and
# wrong for a short one: when the table starts near the foot of a page, LaTeX
# prints the header, finds no room for even one row, breaks, and strands a bare
# "Field | Value" strip at the top of the next page.
#
# For a table known to be short, `tabular` is the correct environment -- it
# cannot break at all, so LaTeX moves the whole thing to the next page. HTML is
# unaffected and takes the ordinary path.
nobreak <- function(x) {
  if (knitr::is_latex_output())
    knitr::kable(x, format = "latex", booktabs = TRUE, longtable = FALSE,
                 linesep = "", col.names = .hdr(x), row.names = FALSE,
                 align = table_align(x))
  else x
}

## ---- counting
o <- data.frame(
  elections = paste0("the last ", kexp$k, " (", kexp$from, "-2024)"),
  # More decimals as the expectation shrinks: "0.0" for the seventeen-election
  # row would hide the whole point, which is that it is 0.02 and not 0.
  expected_if_coins = ifelse(kexp$expected >= 10, pc(kexp$expected, 0),
                      ifelse(kexp$expected >= 1,  pc(kexp$expected, 1),
                                                  pc(kexp$expected, 2))),
  observed = kexp$observed)
names(o) <- c("Elections", "Perfect records expected from coin-flipping",
              "Perfect records observed")
o

## ---- dist-static
obs <- as.integer(table(factor(h_ec, levels = 0:K)))
exp <- N * dbinom(0:K, K, 0.5)
par(mar = c(4.4, 4.4, 1.0, 1.2))
bp <- barplot(obs, names.arg = 0:K, col = NEUTC, border = "white",
              ylim = c(0, max(obs, exp) * 1.14), las = 1,
              xlab = "elections called correctly, out of 17",
              ylab = "counties")
lines(bp, exp, col = REFC, lwd = 2, lty = 2)
points(bp, exp, col = REFC, pch = 19, cex = 0.5)
legend("topright", bty = "n", cex = 0.74,
       legend = c("counties (observed)", "if every county were a fair coin"),
       col = c(NEUTC, REFC), lwd = c(6, 2), lty = c(1, 2))
text(bp[K + 1], obs[K + 1], "0", pos = 3, cex = 0.72, col = REFC)

## ---- dist-d3
# A histogram with a benchmark line laid over it -- two kinds of mark on one
# band scale, which the shared library's single-type figures do not draw, so
# this one stays hand-written.
obs <- as.integer(table(factor(h_ec, levels = 0:K)))
expv <- N * dbinom(0:K, K, 0.5)
rows <- paste(sprintf('{"k":%d,"o":%d,"e":%.2f}', 0:K, obs, expv), collapse = ",")
cat(sprintf('
<div id="ds" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,H=420,M={t:20,r:24,b:52,l:60};
const box=d3.select("#ds");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(D.map(d=>d.k)).range([M.l,W-M.r]).padding(0.16);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.o,d.e))*1.12]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("elections called correctly, out of 17");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444").text("counties");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.k)).attr("width",x.bandwidth())
  .attr("y",d=>y(d.o)).attr("height",d=>y(0)-y(d.o)).attr("fill","%s")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.k} of 17 correct</b><br>${d.o} counties<br>`+
      `coin-flipping predicts ${d.e.toFixed(1)}`)
      .style("left",Math.min(ev.offsetX+14,W-240)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
const ln=d3.line().x(d=>x(d.k)+x.bandwidth()/2).y(d=>y(d.e));
svg.append("path").datum(D).attr("fill","none").attr("stroke","%s")
  .attr("stroke-width",2).attr("stroke-dasharray","6,4").attr("d",ln);
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.k)+x.bandwidth()/2).attr("cy",d=>y(d.e)).attr("r",3).attr("fill","%s");
svg.append("text").attr("x",x(17)+x.bandwidth()/2).attr("y",y(0)-6)
  .attr("text-anchor","middle").attr("font-size","12px").attr("font-weight","600")
  .attr("fill","%s").text("0");
const lg=svg.append("g").attr("transform",`translate(${W-M.r-250},${M.t+4})`);
lg.append("rect").attr("width",13).attr("height",13).attr("fill","%s");
lg.append("text").attr("x",19).attr("y",11).attr("font-size","11.5px").attr("fill","#333")
  .text("counties (observed)");
lg.append("line").attr("x1",0).attr("x2",13).attr("y1",26).attr("y2",26)
  .attr("stroke","%s").attr("stroke-width",2).attr("stroke-dasharray","4,3");
lg.append("text").attr("x",19).attr("y",30).attr("font-size","11.5px").attr("fill","#333")
  .text("if every county were a fair coin");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">Hover a bar.</p>
', rows, NEUTC, REFC, REFC, REFC, NEUTC, REFC))

## ---- checks
data.frame(
  check = c("Against MEDSL's independently compiled county returns, 2000-2016",
            "Against 26 states' own certified 2024 returns, from county-returns/",
            "Reproducing the Wall Street Journal's list from scratch"),
  result = c(
    paste0("15,562 county-elections compared, ", nrow(xc),
           " disagreements about the winner (0.077%)"),
    "1,909 counties compared, every vote total identical",
    paste0("searched for counties perfect over 1980-2016: found ", nrow(p19),
           ", and they are the same 19")))

## ---- one-row
i <- match("18167", w$fips)
o <- data.frame(
  field = c("FIPS code", "county", "state",
            "carried by, 1960 through 1976", "1980 through 1996",
            "2000 through 2016", "2020", "2024"),
  value = c(w$fips[i], cap(w$county[i]), w$state[i],
            paste(w[i, paste0("y", seq(1960, 1976, 4))], collapse = " "),
            paste(w[i, paste0("y", seq(1980, 1996, 4))], collapse = " "),
            paste(w[i, paste0("y", seq(2000, 2016, 4))], collapse = " "),
            w$y2020[i], w$y2024[i]))
nobreak(o)

## ---- the-19
o <- data.frame(county = paste0(cap(p19$county), ", ", p19$state),
                record_1980_2016 = "10 of 10",
                v2020 = p19$y2020, v2024 = p19$y2024,
                still_perfect = ifelse(
                  p19$y2020 == natwin("ec", 2020) & p19$y2024 == natwin("ec", 2024),
                  "yes", "no"))
names(o) <- c("county", "record 1980-2016", "2020", "2024", "still perfect")
o

## ---- grid-static
M <- as.matrix(p19[, paste0("y", YRS)])
ok <- t(sapply(seq_len(nrow(M)), function(i) M[i, ] == natwin("ec", YRS)))
nr <- nrow(M)
# Room at the top for two stacked rows of annotation (the bracket labels above
# the years) and at the bottom for the legend.
par(mar = c(2.4, 8.6, 4.6, 0.6))
plot(NA, xlim = c(0.5, K + 0.5), ylim = c(nr + 0.5, 0.5), axes = FALSE,
     xlab = "", ylab = "", xaxs = "i")
for (i in seq_len(nr)) for (j in seq_len(K))
  rect(j - 0.46, i - 0.44, j + 0.46, i + 0.44,
       col = ifelse(ok[i, j], MATCH, MISS), border = NA)
axis(2, at = seq_len(nr), labels = paste0(cap(p19$county), ", ", p19$state),
     las = 1, tick = FALSE, cex.axis = 0.62, line = -0.6)
axis(3, at = seq_len(K), labels = YRS, tick = FALSE, las = 2,
     cex.axis = 0.58, line = -0.8)
abline(v = which(YRS == 1980) - 0.5, col = "white", lwd = 2)
abline(v = which(YRS == 2020) - 0.5, col = "white", lwd = 2)
# Labels for the two stretches, in the top margin ABOVE the rotated year
# labels. mtext(line=) is the only placement here that is reliably outside the
# years: computing a user-space y and hoping lands on top of them.
brk <- function(a, b, txt, col) {
  axis(3, at = c(a - 0.46, b + 0.46), labels = FALSE, line = 2.7,
       tck = 0.010, col = col, lwd = 1.1)
  mtext(txt, side = 3, line = 2.85, at = (a + b) / 2, cex = 0.62, col = col)
}
brk(which(YRS == 1980), which(YRS == 2016), "selected on this stretch", "#555555")
brk(which(YRS == 2020), which(YRS == 2024), "tested here", REFC)
legend(x = 0.5, y = nr + 1.55, xpd = NA, horiz = TRUE, bty = "n", cex = 0.68,
       legend = c("matched the national winner", "did not"),
       fill = c(MATCH, MISS), border = NA)

## ---- grid-d3
# The chapter's showpiece: a per-square annotated grid with bracket labels,
# which no shared-library type draws. It stays hand-written.
M <- as.matrix(p19[, paste0("y", YRS)])
cells <- unlist(lapply(seq_len(nrow(M)), function(i)
  sprintf('{"r":%d,"c":%d,"y":%d,"v":"%s","ok":%s}', i - 1, seq_len(K) - 1, YRS,
          M[i, ], ifelse(M[i, ] == natwin("ec", YRS), "true", "false"))))
labs <- paste(sprintf('"%s"', paste0(cap(p19$county), ", ", p19$state)),
              collapse = ",")
cat(sprintf('
<div id="gr" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[%s], L=[%s], YR=[%s];
const R=L.length, K=YR.length;
const W=760,M={t:34,r:14,b:30,l:150};
const cw=(W-M.l-M.r)/K, ch=17, H=M.t+R*ch+M.b;
const box=d3.select("#gr");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("g").selectAll("text").data(L).join("text")
  .attr("x",M.l-8).attr("y",(d,i)=>M.t+i*ch+12).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#333").text(d=>d);
svg.append("g").selectAll("text").data(YR).join("text")
  .attr("x",(d,i)=>M.l+i*cw+cw/2).attr("y",M.t-16).attr("text-anchor","middle")
  .attr("font-size","9.5px").attr("fill","#666").text(d=>d);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(C).join("rect")
  .attr("x",d=>M.l+d.c*cw+0.8).attr("y",d=>M.t+d.r*ch+0.8)
  .attr("width",cw-1.6).attr("height",ch-1.6)
  .attr("fill",d=>d.ok?"%s":"%s")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${L[d.r]}</b><br>${d.y}: voted ${d.v}<br>`+
      (d.ok?"matched the winner":"<b>missed</b>"))
      .style("left",Math.min(ev.offsetX+14,W-240)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
[YR.indexOf(1980),YR.indexOf(2020)].forEach(i=>{
  svg.append("line").attr("x1",M.l+i*cw).attr("x2",M.l+i*cw)
    .attr("y1",M.t).attr("y2",M.t+R*ch).attr("stroke","#fff").attr("stroke-width",2); });
svg.append("text").attr("x",M.l+(YR.indexOf(1980)+5)*cw).attr("y",M.t-3)
  .attr("text-anchor","middle").attr("font-size","10.5px").attr("fill","#555")
  .text("selected on this stretch");
svg.append("text").attr("x",M.l+(YR.indexOf(2020)+1)*cw).attr("y",M.t-3)
  .attr("text-anchor","middle").attr("font-size","10.5px").attr("fill","%s")
  .text("tested here");
const lg=svg.append("g").attr("transform",`translate(${M.l},${M.t+R*ch+16})`);
[["%s","matched the national winner"],["%s","did not"]].forEach((p,i)=>{
  lg.append("rect").attr("x",i*200).attr("width",12).attr("height",12).attr("y",-10).attr("fill",p[0]);
  lg.append("text").attr("x",i*200+18).attr("font-size","11px").attr("fill","#333").text(p[1]); });
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">Hover any square.</p>
', paste(cells, collapse = ","), labs, paste(YRS, collapse = ","),
   MATCH, MISS, REFC, MATCH, MISS))

## ---- together
data.frame(
  quantity = c("Of the nineteen, the share that voted Republican in 2020",
               "Of all 3,081 counties, the share that voted Republican in 2020",
               "Probability 18 of 19 independent fair coins fail together"),
  value = c(paste0(pc(100 * mean(p19$y2020 == "R"), 0), "%"),
            paste0(pc(r20, 0), "%"),
            sprintf("%.5f", sum(dbinom(18:19, 19, 0.5)))))

## ---- flip-static
par(mar = c(3.6, 4.6, 1.0, 1.2))
plot(flip$year, flip$pct, type = "n", las = 1, ylim = c(0, max(flip$pct) * 1.1),
     xlab = "", ylab = "% of counties changing party")
grid(nx = NA, ny = NULL, col = "#EEEEEE", lty = 1)
lines(flip$year, flip$pct, col = NEUTC, lwd = 2)
points(flip$year, flip$pct, pch = 19, cex = 0.9, col = NEUTC)
ix <- c(which.max(flip$pct), nrow(flip) - 1, nrow(flip))
points(flip$year[ix], flip$pct[ix], pch = 19, cex = 1.15, col = REFC)
text(flip$year[ix], flip$pct[ix], paste0(pc(flip$pct[ix], 0), "%"),
     pos = c(3, 3, 4), cex = 0.72, col = REFC)

## ---- flip-d3
# A single line on a time axis: exactly what the shared library draws, so it
# is drawn with dd_fig(). d3 itself was loaded by the first figure's own tag,
# so only dd-charts.js is emitted here (d3 = FALSE).
dd_fig("fl", "line", flip[order(flip$year), ], d3 = FALSE,
  size = list(w = 760, h = 380, m = list(t = 20, r = 24, b = 40, l = 56)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "pct", label = "% of counties changing party",
           domain = c(0, ceiling(max(flip$pct)) + 2), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "pct", label = "counties changing party", class = "series-1"))),
  points = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+d.pct.toFixed(1)+
      "% of counties changed party";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">Hover a point.</p>')

## ---- definition
o <- data.frame(
  definition = c("Electoral College winner", "National popular-vote winner"),
  mix = c(paste0(sum(nat$ec == "D"), "D / ", sum(nat$ec == "R"), "R"),
          paste0(sum(nat$pv == "D"), "D / ", sum(nat$pv == "R"), "R")),
  perfect = c(sum(h_ec == K), sum(h_pv == K)),
  best = c(max(h_ec), max(h_pv)),
  who = c(paste(namef(w$fips[h_ec == max(h_ec)]), collapse = "; "),
          paste(namef(w$fips[h_pv == max(h_pv)]), collapse = "; ")))
names(o) <- c("“the national winner” means", "elections won",
              "perfect records", "best record", "held by")
o

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
