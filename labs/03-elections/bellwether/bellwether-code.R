# bellwether-code.R -- chunk bodies for bellwether-brief.Rmd
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
n_live20 <- sum(p19$y2020 == natwin("ec", 2020))
surv     <- p19[p19$y2020 == natwin("ec", 2020), ]

# --- the counting argument --------------------------------------------------
kexp <- data.frame(k = c(3, 5, 7, 10, 13, 17))
kexp$from <- YRS[K - kexp$k + 1]
kexp$expected <- N / 2^kexp$k
kexp$observed <- sapply(kexp$k, function(k) sum(hits("ec", tail(YRS, k)) == k))

# --- six-election windows, each measured the same way -----------------------
wl <- list(c(1960, 1980), c(1968, 1988), c(1980, 2000),
           c(1992, 2012), c(2000, 2020), c(2004, 2024))
win <- do.call(rbind, lapply(wl, function(v) {
  yy <- seq(v[1], v[2], 4)
  hh <- hits("ec", yy)
  dy <- yy[natwin("ec", yy) == "D"]; ry <- yy[natwin("ec", yy) == "R"]
  D  <- rowSums(sapply(dy, function(y) V(y) == "D"))
  R  <- rowSums(sapply(ry, function(y) V(y) == "R"))
  data.frame(from = v[1], to = v[2], nd = length(dy), nr = length(ry),
             perfect = sum(hh == 6), pct = 100 * mean(hh == 6),
             pdD = mean(D) / length(dy), prR = mean(R) / length(ry),
             corr = cor(D, R))
}))
win$label <- paste0(win$from, "-", win$to)

# --- how much the map moves at all ------------------------------------------
flip <- data.frame(
  year = YRS[-1],
  pct  = 100 * sapply(2:K, function(i) mean(V(YRS[i]) != V(YRS[i - 1]))))
dshare <- data.frame(year = YRS,
                     pct = 100 * sapply(YRS, function(y) mean(V(y) == "D")))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
# Title-case each word, not just the first: the source county names are all
# upper case, so a naive sub() turns VAN BUREN into "Van buren".
cap <- function(nm) gsub("\\b([a-z])", "\\U\\1", tolower(nm), perl = TRUE)
namef <- function(f) {
  i <- match(f, w$fips)
  paste0(cap(w$county[i]), ", ", w$state[i])
}

# --- palette ----------------------------------------------------------------
# The quantity nearly every figure carries is "did this county match the
# national winner", which is NOT a party: the national winner is a Democrat in
# eight of these elections and a Republican in nine. Red and blue would be read
# as party whatever the legend said, so matched/missed gets a diverging pair
# that carries no partisan reading. Party colours appear exactly once, in the
# figure that really is about party, and they are muted so that the two
# schemes cannot be confused at a glance.
MATCH <- "#1B7837"   # the county went with the national winner
MISS  <- "#762A83"   # it did not
NEUTC <- "#999999"
REFC  <- "#C41230"   # a reference mark: an expectation, a median, a named case
DEMC  <- "#6699CC"
REPC  <- "#CC7766"

subcap <- function(txt, width = 100, line = 3.4, cex = 0.66) {
  cw <- strwrap(txt, width = width)
  mtext(cw, side = 1, line = line + (seq_along(cw) - 1) * 0.95, adj = 0,
        cex = cex, col = "#555555")
}

cap_dist <- paste0(
  "All ", format(N, big.mark = ","), " counties by how many of the ", K,
  " elections they called correctly, against what independent coin-flipping ",
  "would produce.")
cap_19 <- paste0(
  "The nineteen counties the Wall Street Journal published in November 2020, ",
  "one square per election.")
cap_flip <- paste0(
  "The share of counties that changed party from the previous election.")
cap_cond <- paste0(
  "Two conditional probabilities that used to be close together and no longer ",
  "are, measured over six-election windows.")
# Plain quotes, not curly: this string is drawn by base R into the PDF device,
# which substitutes for U+201C/U+201D and warns on every knit.
cap_def <- paste0(
  "The same counties scored against two different definitions of ",
  "\"the national winner\".")

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
# "Field | Value" strip at the top of the next page. \filbreak does not help,
# because the break happens *inside* the table, after \endhead.
#
# For a table known to be short, `tabular` is the correct environment -- it
# cannot break at all, so LaTeX moves the whole thing to the next page. HTML is
# unaffected and takes the ordinary path.
nobreak <- function(x) {
  if (knitr::is_latex_output())
    # linesep = "": kable's latex path inserts \addlinespace every fifth row,
    # which reads as a grouping the table does not have.
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
par(mar = c(5.6, 4.4, 1.0, 1.2))
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
subcap(cap_dist, line = 3.8)

## ---- dist-d3
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
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover a bar.</p>
', rows, NEUTC, REFC, REFC, REFC, NEUTC, REFC, cap_dist))

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

## ---- crosscheck
o <- xc[, c("county", "state", "year", "win_algara", "win_medsl")]
o$county <- cap(o$county)
names(o) <- c("county", "state", "year", "Algara & Amlani say", "MEDSL says")
o

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
# the years) and at the bottom for the legend and the caption, which otherwise
# land on top of each other.
par(mar = c(4.2, 8.6, 4.6, 0.6))
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
subcap(cap_19, line = 2.6, width = 112)

## ---- grid-d3
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
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover any square.</p>
', paste(cells, collapse = ","), labs, paste(YRS, collapse = ","),
   MATCH, MISS, REFC, MATCH, MISS, cap_19))

## ---- together
r20 <- 100 * mean(V(2020) == "R")
data.frame(
  quantity = c("Of the nineteen, the share that voted Republican in 2020",
               "Of all 3,081 counties, the share that voted Republican in 2020",
               "Probability 18 of 19 independent fair coins fail together"),
  value = c(paste0(pc(100 * mean(p19$y2020 == "R"), 0), "%"),
            paste0(pc(r20, 0), "%"),
            sprintf("%.5f", sum(dbinom(18:19, 19, 0.5)))))

## ---- flip-static
par(mar = c(4.6, 4.6, 1.0, 1.2))
plot(flip$year, flip$pct, type = "n", las = 1, ylim = c(0, max(flip$pct) * 1.1),
     xlab = "", ylab = "% of counties changing party")
grid(nx = NA, ny = NULL, col = "#EEEEEE", lty = 1)
lines(flip$year, flip$pct, col = NEUTC, lwd = 2)
points(flip$year, flip$pct, pch = 19, cex = 0.9, col = NEUTC)
ix <- c(which.max(flip$pct), nrow(flip) - 1, nrow(flip))
points(flip$year[ix], flip$pct[ix], pch = 19, cex = 1.15, col = REFC)
text(flip$year[ix], flip$pct[ix], paste0(pc(flip$pct[ix], 0), "%"),
     pos = c(3, 3, 4), cex = 0.72, col = REFC)
subcap(cap_flip, line = 3.0)

## ---- flip-d3
rows <- paste(sprintf('{"y":%d,"p":%.2f}', flip$year, flip$pct), collapse = ",")
cat(sprintf('
<div id="fl" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=380,M={t:20,r:24,b:44,l:58};
const box=d3.select("#fl");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1962,2026]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.p)*1.12]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% of counties changing party");
svg.append("path").datum(D).attr("fill","none").attr("stroke","%s").attr("stroke-width",2)
  .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.p)));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.p)).attr("r",4)
  .attr("fill",d=>(d.y===1964||d.y===1968||d.y===2020||d.y===2024)?"%s":"%s")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.y}</b><br>${d.p.toFixed(1)}%% of counties changed party`)
      .style("left",Math.min(ev.offsetX+14,W-250)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover a point.</p>
', rows, NEUTC, REFC, NEUTC, cap_flip))

## ---- conditional
o <- data.frame(
  window = win$label,
  mix = paste0(win$nd, "D / ", win$nr, "R"),
  pdD = pc(win$pdD, 3), prR = pc(win$prR, 3),
  corr = pc(win$corr, 3),
  perfect = paste0(win$perfect, " (", pc(win$pct, 2), "%)"))
names(o) <- c("six elections", "won by", "p(correct | D wins)",
              "p(correct | R wins)", "correlation", "perfect records")
o

## ---- cond-static
par(mar = c(5.2, 4.6, 1.0, 8.4), xpd = NA)
plot(NA, xlim = c(1, nrow(win)), ylim = c(0, 1), axes = FALSE,
     xlab = "", ylab = "probability a county calls the winner")
axis(1, at = seq_len(nrow(win)), labels = win$label, cex.axis = 0.66, las = 2)
axis(2, las = 1, cex.axis = 0.8)
grid(nx = NA, ny = NULL, col = "#EEEEEE", lty = 1)
lines(win$prR, col = REPC, lwd = 2.4); points(win$prR, pch = 19, col = REPC)
lines(win$pdD, col = DEMC, lwd = 2.4); points(win$pdD, pch = 19, col = DEMC)
text(nrow(win) + 0.14, win$prR[nrow(win)], "when a\nRepublican wins",
     adj = 0, cex = 0.68, col = REPC)
text(nrow(win) + 0.14, win$pdD[nrow(win)], "when a\nDemocrat wins",
     adj = 0, cex = 0.68, col = DEMC)
subcap(cap_cond, line = 4.2, width = 92)

## ---- cond-d3
rows <- paste(sprintf('{"l":"%s","d":%.4f,"r":%.4f,"c":%.3f,"p":%d}',
                      win$label, win$pdD, win$prR, win$corr, win$perfect),
              collapse = ",")
cat(sprintf('
<div id="cd" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=400,M={t:20,r:150,b:64,l:58};
const box=d3.select("#cd");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(D.map(d=>d.l)).range([M.l,W-M.r]).padding(0.4);
const y=d3.scaleLinear().domain([0,1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x))
  .selectAll("text").attr("transform","rotate(-32)").attr("text-anchor","end")
  .attr("font-size","10.5px");
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("probability a county calls the winner");
[["r","%s","when a Republican wins"],["d","%s","when a Democrat wins"]].forEach(s=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",s[1]).attr("stroke-width",2.4)
    .attr("d",d3.line().x(d=>x(d.l)).y(d=>y(d[s[0]])));
  svg.append("g").selectAll("circle."+s[0]).data(D).join("circle").attr("class",s[0])
    .attr("cx",d=>x(d.l)).attr("cy",d=>y(d[s[0]])).attr("r",4.2).attr("fill",s[1]);
  const last=D[D.length-1];
  svg.append("text").attr("x",x(last.l)+10).attr("y",y(last[s[0]])+4)
    .attr("font-size","11.5px").attr("fill",s[1]).text(s[2]); });
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.l)-18).attr("y",M.t).attr("width",36).attr("height",H-M.b-M.t)
  .attr("fill","transparent")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.l}</b><br>when a Democrat wins: ${(100*d.d).toFixed(1)}%%<br>`+
      `when a Republican wins: ${(100*d.r).toFixed(1)}%%<br>`+
      `correlation ${d.c.toFixed(3)}<br>${d.p} perfect records`)
      .style("left",Math.min(ev.offsetX+14,W-260)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover a window.</p>
', rows, REPC, DEMC, cap_cond))

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

## ---- def-static
par(mar = c(6.0, 4.4, 1.0, 1.2))
te <- as.integer(table(factor(h_ec, levels = 0:K)))
tp <- as.integer(table(factor(h_pv, levels = 0:K)))
plot(0:K, te, type = "n", ylim = c(0, max(te, tp) * 1.1), las = 1,
     xlab = "elections called correctly, out of 17", ylab = "counties")
grid(nx = NA, ny = NULL, col = "#EEEEEE", lty = 1)
lines(0:K, te, col = MATCH, lwd = 2.4); points(0:K, te, pch = 19, cex = 0.7, col = MATCH)
lines(0:K, tp, col = MISS,  lwd = 2.4); points(0:K, tp, pch = 19, cex = 0.7, col = MISS)
legend("topleft", bty = "n", cex = 0.74, lwd = 2.4,
       col = c(MATCH, MISS),
       legend = c("scored against the Electoral College",
                  "scored against the popular vote"))
subcap(cap_def, line = 4.4)

## ---- def-d3
te <- as.integer(table(factor(h_ec, levels = 0:K)))
tp <- as.integer(table(factor(h_pv, levels = 0:K)))
rows <- paste(sprintf('{"k":%d,"e":%d,"p":%d}', 0:K, te, tp), collapse = ",")
cat(sprintf('
<div id="df" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=380,M={t:20,r:24,b:48,l:58};
const box=d3.select("#df");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,17]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.e,d.p))*1.1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("elections called correctly, out of 17");
[["e","%s","scored against the Electoral College"],
 ["p","%s","scored against the popular vote"]].forEach((s,i)=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",s[1]).attr("stroke-width",2.4)
    .attr("d",d3.line().x(d=>x(d.k)).y(d=>y(d[s[0]])));
  const lg=svg.append("g").attr("transform",`translate(${M.l+14},${M.t+8+i*18})`);
  lg.append("line").attr("x1",0).attr("x2",16).attr("stroke",s[1]).attr("stroke-width",2.4);
  lg.append("text").attr("x",22).attr("y",4).attr("font-size","11.5px").attr("fill","#333").text(s[2]); });
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.k)-14).attr("y",M.t).attr("width",28).attr("height",H-M.b-M.t)
  .attr("fill","transparent")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.k} of 17 correct</b><br>Electoral College: ${d.e} counties<br>`+
      `popular vote: ${d.p} counties`)
      .style("left",Math.min(ev.offsetX+14,W-250)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s Hover a value.</p>
', rows, MATCH, MISS, cap_def))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
