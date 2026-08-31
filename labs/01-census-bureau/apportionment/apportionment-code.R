# apportionment-code.R -- chunk bodies for apportionment-brief.Rmd
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

ap  <- read.csv("data/derived/apportionment_2020.csv", stringsAsFactors = FALSE)
ap$app_pop <- as.numeric(ap$app_pop)
pop <- setNames(ap$app_pop, ap$state)

# The Bureau's own published queue: which state took each House seat from 51 to
# 445, which of that state's seats it was, and the priority value it was bought
# at. It runs ten seats past the 435 the statute allows.
pv <- read.csv("data/derived/priority_values.csv", stringsAsFactors = FALSE)

# Huntington-Hill: every state starts with one seat, the rest go one at a time
# to whichever state has the highest priority value.
hh <- function(size) {
  s <- setNames(rep(1L, length(pop)), names(pop))
  ord <- character(0); pv <- numeric(0)
  for (i in seq_len(size - length(pop))) {
    p <- pop / sqrt(s * (s + 1))
    w <- names(which.max(p)); ord[i] <- w; pv[i] <- p[[w]]; s[w] <- s[w] + 1L
  }
  list(seats = s, order = ord, priority = pv)
}
b435  <- hh(435); base <- b435$seats
ok435 <- identical(as.integer(base[ap$state]), as.integer(ap$seats))

# and the recomputation against the Bureau's published priority column
pv_ok <- max(abs(pv$priority[pv$house_seat <= 435] - b435$priority)) < 1e-6

mn <- ap[ap$state == "Minnesota", ]
ny <- ap[ap$state == "New York",  ]
win_pri <- mn$app_pop / sqrt(mn$seats * (mn$seats - 1))   # the price of seat 435
need_ny <- ceiling(win_pri * sqrt(ny$seats * (ny$seats + 1)))
margin  <- need_ny - ny$app_pop

# how far every state was from one more seat
pri   <- pop / sqrt(base * (base + 1))
short <- ceiling(win_pri * sqrt(base * (base + 1)) - pop)
ladder <- data.frame(state = names(pri), priority = as.numeric(pri),
                     short = as.numeric(short), stringsAsFactors = FALSE)
ladder <- ladder[order(-ladder$priority), ]
runner <- ladder$short[2]

# --- the map -----------------------------------------------------------------
# state_rings.csv is the fifty state outlines already projected and reduced to
# integer canvas coordinates, so nothing here projects anything.
sr   <- read.csv("data/derived/state_rings.csv", stringsAsFactors = FALSE)
mapd <- merge(ap[, c("state", "seats", "app_pop", "people_per_seat")],
              ladder[, c("state", "short")], by = "state")
NATAVG <- sum(ap$app_pop) / sum(ap$seats)
mapd$dev <- mapd$people_per_seat / NATAVG - 1      # signed, for a diverging ramp
MAXDEV <- max(abs(mapd$dev))
most_over  <- mapd$state[which.min(mapd$people_per_seat)]
most_under <- mapd$state[which.max(mapd$people_per_seat)]
pps_ratio  <- max(mapd$people_per_seat) / min(mapd$people_per_seat)

# the obvious method
ap$quota   <- 435 * ap$app_pop / sum(ap$app_pop)
ap$rounded <- round(ap$quota)
movers <- ap[ap$rounded != ap$seats, c("state", "quota", "rounded", "seats")]

# --- the five divisor methods, and Hamilton --------------------------------
#
# Every divisor method is the same queue with a different answer to one
# question: at what point between n seats and n+1 does a state deserve the
# next one? Adams says n, Jefferson says n+1, and the other three sit between.
# Every state is seated once first, which is how the constitutional floor is
# imposed on the two rules -- Webster and Jefferson -- that would not respect
# it on their own.
DIVISOR <- list(
  "Adams"            = function(k) k,
  "Dean"             = function(k) (2 * k * (k + 1)) / (2 * k + 1),
  "Huntington-Hill"  = function(k) sqrt(k * (k + 1)),
  "Webster"          = function(k) k + 0.5,
  "Jefferson"        = function(k) k + 1)

run_divisor <- function(f, size) {
  s <- setNames(rep(1, length(pop)), names(pop))
  for (i in seq_len(size - length(pop))) {
    p <- pop / f(s); s[names(which.max(p))] <- s[names(which.max(p))] + 1
  }
  s
}

# Hamilton: give every state the whole part of its quota, then hand the
# leftovers to the largest fractions. A state lifted to the constitutional
# floor has already been paid and takes no remainder seat.
run_hamilton <- function(size) {
  q <- size * pop / sum(pop); fl <- floor(q); s <- pmax(fl, 1)
  left <- size - sum(s); fr <- q - fl; fr[fl < 1] <- -Inf
  if (left > 0) { o <- order(-fr); s[o[seq_len(left)]] <- s[o[seq_len(left)]] + 1 }
  s
}

apportion <- function(method, size) {
  if (method == "Hamilton") run_hamilton(size)
  else run_divisor(DIVISOR[[method]], size)
}
METHODS <- c(names(DIVISOR), "Hamilton")

M435 <- sapply(METHODS, apportion, size = 435)[ap$state, ]
mdiff <- M435 - M435[, "Huntington-Hill"]
mmoved <- mdiff[apply(mdiff, 1, function(r) any(r != 0)), , drop = FALSE]
seats_moved <- colSums(abs(mdiff)) / 2

# quota violations: a state above the ceiling or below the floor of its
# fair share. This is the half of Balinski and Young that can be seen.
qv <- vapply(METHODS, function(m)
  paste(rownames(M435)[M435[, m] > ceiling(ap$quota) |
                       M435[, m] < floor(ap$quota)], collapse = ", "),
  character(1))

web_diff <- data.frame(state = ap$state, hh = as.integer(M435[, "Huntington-Hill"]),
                       webster = as.integer(M435[, "Webster"]))
web_diff <- web_diff[web_diff$webster != web_diff$hh, ]
dean_diff <- data.frame(state = ap$state, hh = as.integer(M435[, "Huntington-Hill"]),
                        dean = as.integer(M435[, "Dean"]))
dean_diff <- dean_diff[dean_diff$dean != dean_diff$hh, ]

# --- the Alabama paradox, on the 2020 file ---------------------------------
# Grow the House one seat at a time under Hamilton's method and record every
# state that ends up with FEWER seats in the larger House.
PARA_LO <- 400; PARA_HI <- 700
para <- local({
  prev <- NULL; out <- list()
  for (sz in PARA_LO:PARA_HI) {
    cur <- run_hamilton(sz)
    if (!is.null(prev)) {
      d <- cur - prev
      if (any(d < 0)) out[[length(out) + 1L]] <- data.frame(
        from = sz - 1L, to = sz, state = names(which(d < 0)),
        had = prev[which(d < 0)], now = cur[which(d < 0)],
        gained = paste(names(which(d > 0)), collapse = " and "),
        stringsAsFactors = FALSE)
    }
    prev <- cur
  }
  do.call(rbind, out)
})
alab <- para[para$state == "Alabama", ][1, ]
ALO <- alab$from - 14L; AHI <- alab$to + 14L
atrack <- data.frame(size = ALO:AHI,
  hamilton = vapply(ALO:AHI, function(z) run_hamilton(z)[["Alabama"]], 0),
  hh       = vapply(ALO:AHI, function(z) apportion("Huntington-Hill", z)[["Alabama"]], 0))

# --- the size of the House -------------------------------------------------
wy_house  <- round(sum(pop) / pop[["Wyoming"]])
cube_root <- round(sum(pop) ^ (1 / 3))
spread <- function(sz) { s <- apportion("Huntington-Hill", sz); per <- round(pop / s)
  list(min = min(per), max = max(per), ratio = max(per) / min(per),
       smallest = names(which.min(per)), largest = names(which.max(per))) }
sp435 <- spread(435); sp_wy <- spread(wy_house); sp_cr <- spread(cube_root)
per435 <- round(pop / base); per_wy <- round(pop / apportion("Huntington-Hill", wy_house))
s436  <- apportion("Huntington-Hill", 436)
gain436 <- names(which(s436[ap$state] - base[ap$state] > 0))
pv436 <- pv[pv$house_seat > 435, ]

# The House as it has actually been sized, 1910 to 2020. This is the national
# series built for the regional-shift chapter, borrowed rather than rebuilt:
# ../../01-census-bureau/regional-shift/data/derived/nation.csv
hist_h <- read.csv("../regional-shift/data/derived/nation.csv",
                   stringsAsFactors = FALSE)

# Electoral College: a state's electors are its seats plus two, so the
# senatorial pair is a fixed bonus that a larger House dilutes. The ratio is
# between the state where an elector covers the fewest people and the state
# where it covers the most.
ec_ratio <- function(sz) {
  e <- (apportion("Huntington-Hill", sz) + 2) / (pop / 1e6)
  max(e) / min(e)
}
ECG <- sort(unique(c(seq(435, 1000, by = 15), 435, wy_house, cube_root)))
ecg <- data.frame(size = ECG, ratio = vapply(ECG, ec_ratio, 0))
ec435 <- ecg$ratio[ecg$size == 435]
ec_cr <- ecg$ratio[ecg$size == cube_root]
e435   <- (base + 2) / (pop / 1e6)
ec_top <- names(which.max(e435)); ec_bot <- names(which.min(e435))

# The same ratio for the House itself: the largest district divided by the
# smallest, at every size the Electoral College line is drawn at. Both series
# are one resident's weight divided by another's, so they share an axis.
ecg$house <- vapply(ECG, function(sz) spread(sz)$ratio, 0)
hs435 <- ecg$house[ecg$size == 435]
hs_cr <- ecg$house[ecg$size == cube_root]

# Ladewig and Jasinski (2008) measure interstate malapportionment as the sum of
# squared differences between each district and the national average district.
# That is a statement about the whole distribution; the largest-to-smallest
# ratio is a statement about its two ends. They disagree about how much a bigger
# House would help, which is the reason both are reported below.
ij_index <- function(sz) { s <- apportion("Huntington-Hill", sz)
  sum((rep(pop / s, s) - sum(pop) / sz) ^ 2) }
ij435 <- ij_index(435); ij_wy <- ij_index(wy_house); ij_cr <- ij_index(cube_root)

ordn <- function(k) paste0(k, ifelse(k %% 100 %in% 11:13, "th",
  ifelse(k %% 10 == 1, "st", ifelse(k %% 10 == 2, "nd",
  ifelse(k %% 10 == 3, "rd", "th")))))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  nm <- names(x); nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- ap[ap$state == "California",
        c("state", "resident_pop", "overseas", "app_pop", "seats")]
o$resident_pop <- n(o$resident_pop); o$overseas <- n(o$overseas)
o$app_pop <- n(o$app_pop)
names(o) <- c("state", "people living there", "counted from abroad",
              "apportionment population", "seats")
o

## ---- quota
o <- head(ap[order(-ap$quota), c("state", "app_pop", "quota", "seats")], 6)
o$app_pop <- n(o$app_pop); o$quota <- pc(o$quota, 2)
names(o) <- c("state", "apportionment population", "fair share of 435", "seats")
o

## ---- movers
o <- movers; o$quota <- pc(o$quota, 3)
names(o) <- c("state", "fair share", "seats if we round", "seats actually held")
o

## ---- floor
o <- ap[ap$quota < 1, c("state", "app_pop", "quota", "seats")]
o$app_pop <- n(o$app_pop); o$quota <- pc(o$quota, 3)
names(o) <- c("state", "apportionment population", "fair share", "seats")
o

## ---- ceilingless
u <- ap[ap$seats == 1 & ap$quota >= 1, c("state", "app_pop", "quota", "seats")]
u <- u[order(-u$quota), ]
u$app_pop <- n(u$app_pop); u$quota <- pc(u$quota, 3)
names(u) <- c("state", "apportionment population", "fair share", "seats")
u

## ---- worked-queue
wq <- head(pv, 12)
wq$divisor <- sqrt(wq$state_seat * (wq$state_seat - 1))
out <- data.frame(
  `House seat` = wq$house_seat,
  `goes to`    = wq$state,
  `that state's` = paste(ordn(wq$state_seat), "seat"),
  population   = n(round(pop[wq$state])),
  divisor      = pc(wq$divisor, 5),
  priority     = n(round(wq$priority)),
  check.names  = FALSE, stringsAsFactors = FALSE)
out

## ---- pq-static
# The whole published queue, not just the part the statute pays for. The Bureau
# tabulates ten seats past 435, so the staircase can be drawn past the point
# where it stops mattering -- which is where New York is.
qpv  <- pv$priority
qsq  <- pv$house_seat
STAT <- sum(ap$seats)
par(mar = c(6.6, 6.4, 1.8, 1.6))
plot(qsq, qpv, type = "n", log = "y", yaxt = "n", bty = "n", las = 1,
     xlab = "", ylab = "", xlim = c(min(qsq), max(qsq) + 4))
u <- par("usr")
rect(STAT, 10^u[3], max(qsq), 10^u[4], col = "#F4E3E6", border = NA)
lines(qsq, qpv, type = "s", col = "#2c7fb8", lwd = 2)
qtk <- c(1e6, 2e6, 5e6, 1e7, 2e7)
axis(2, at = qtk, labels = paste0(qtk / 1e6, "m"), las = 1)
mtext("House seat, in the order it was handed out", side = 1, line = 2.5,
      cex = 0.9)
mtext("price paid (priority value)", side = 2, line = 4.2, cex = 0.9)
abline(h = win_pri, col = "#999999", lty = 3)
i435 <- which(qsq == STAT); i436 <- which(qsq == STAT + 1)
points(qsq[i435], qpv[i435], pch = 19, col = "#C41230", cex = 1.3)
text(qsq[i435], qpv[i435],
     paste0("seat ", STAT, " - ", tail(b435$order, 1), "   "),
     pos = 2, cex = 0.75, col = "#C41230")
points(qsq[i436], qpv[i436], pch = 21, bg = "#FFFFFF", col = "#C41230",
       lwd = 1.6, cex = 1.3)
text(qsq[i436], qpv[i436] * 2.4,
     paste0(pv$state[i436], " would\nhave taken ", STAT + 1),
     cex = 0.72, col = "#C41230", adj = c(0.85, 0))
text(qsq[1], qpv[1], paste0("  seat ", qsq[1], " - ", pv$state[1]),
     pos = 4, cex = 0.75, col = "#2c7fb8")
mtext(paste0("A staircase, never a curve: each tread is one seat. It fell from ",
             n(round(qpv[1])), " to ", n(round(qpv[i435])), ", about ",
             pc(qpv[1] / qpv[i435], 0), "-fold."),
      side = 1, line = 3.8, cex = 0.62, col = "#666666")
mtext(paste0("The shaded band is seats ", STAT + 1, " to ", max(qsq),
             ": the Bureau tabulates them, the statute does not pay for them."),
      side = 1, line = 5.5, cex = 0.62, col = "#666666")
mtext(paste0("The dotted line is the price of the final seat - the level ",
             ladder$state[1], " had to reach and did not."),
      side = 1, line = 4.6, cex = 0.62, col = "#666666")

## ---- pq-d3
# The shared engine. Every interactive figure in this chapter runs the same
# fifty populations through the same rules in the browser, so the reader can
# change the rule or the size of the House and watch the seats move. d3 is
# loaded once, here, because this is the first figure that needs it.
cat(sprintf('
<script src="../../_lib/d3.v7.min.js"></script>
<script>
window.DD_AP = window.DD_AP || (function(){
  const ST=[%s], POP=[%s];
  const DIV={ "Adams":k=>k, "Dean":k=>(2*k*(k+1))/(2*k+1),
              "Huntington-Hill":k=>Math.sqrt(k*(k+1)), "Webster":k=>k+0.5,
              "Jefferson":k=>k+1 };
  // Every state is seated once first, which is how the constitutional floor
  // gets imposed on Webster and Jefferson, which would not respect it alone.
  function divisor(rule,size){
    const d=DIV[rule], s=ST.map(()=>1);
    for(let i=ST.length;i<size;i++){
      let b=-1,w=0;
      for(let j=0;j<ST.length;j++){const p=POP[j]/d(s[j]); if(p>b){b=p;w=j;}}
      s[w]++;
    }
    return s;
  }
  function hamilton(size){
    const tot=POP.reduce((a,b)=>a+b,0);
    const q=POP.map(p=>size*p/tot), s=q.map(v=>Math.max(Math.floor(v),1));
    let left=size-s.reduce((a,b)=>a+b,0);
    const fr=q.map((v,i)=>Math.floor(v)<1?-Infinity:v-Math.floor(v));
    const ord=d3.range(ST.length).sort((a,b)=>fr[b]-fr[a]);
    for(let i=0;i<left;i++) s[ord[i]]++;
    return s;
  }
  return { st:ST, pop:POP, rules:Object.keys(DIV).concat("Hamilton"),
           seats:(rule,size)=>rule==="Hamilton"?hamilton(size):divisor(rule,size),
           idx:Object.fromEntries(ST.map((s,i)=>[s,i])) };
})();
</script>', paste0(sprintf('"%s"', ap$state), collapse = ","),
            paste0(sprintf('%.0f', ap$app_pop), collapse = ",")))

## ---- pq-fig
pqrows <- paste(sprintf('[%d,"%s",%d,%.0f]', pv$house_seat, pv$state,
                        pv$state_seat, pv$priority), collapse = ",")
cat(sprintf('
<div id="pq" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const Q=[%s].map(a=>({k:a[0],s:a[1],n:a[2],v:a[3]}));
const AP=window.DD_AP, CUT=%.0f, STAT=%d;
const W=760,H=330,M={t:16,r:26,b:44,l:76};
const box=d3.select("#pq");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([51,d3.max(Q,d=>d.k)]).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([7e5,3.2e7]).range([H-M.b,M.t]);
// the ten seats past 435 exist in the Bureau\'s table but not in the statute
svg.append("rect").attr("x",x(STAT)).attr("width",x(d3.max(Q,d=>d.k))-x(STAT))
  .attr("y",M.t).attr("height",H-M.b-M.t).attr("fill","#C41230")
  .attr("fill-opacity",0.06);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([1e6,2e6,5e6,1e7,2e7]).tickFormat(d=>(d/1e6)+"m"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("House seat, in the order it was handed out");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("price paid (priority value)");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(CUT))
  .attr("y2",y(CUT)).attr("stroke","#999").attr("stroke-dasharray","3,3");
svg.append("path").datum(Q).attr("fill","none").attr("stroke","#2c7fb8")
  .attr("stroke-width",2).attr("d",d3.line().curve(d3.curveStepAfter)
    .x(d=>x(d.k)).y(d=>y(d.v)));
svg.append("text").attr("x",W-M.r).attr("y",11).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#C41230")
  .text("the shaded seats are past the statute");
const mk=svg.append("g");
const mline=mk.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#C41230").attr("stroke-width",1.2);
const mdot=mk.append("circle").attr("r",5).attr("fill","#C41230");
const sl=box.append("div").attr("style","margin:0.4em 0 0.2em");
sl.append("input").attr("type","range").attr("min",51).attr("max",d3.max(Q,d=>d.k))
  .attr("value",51).attr("id","pqsl")
  .attr("style","width:100%%;accent-color:#C41230")
  .on("input",function(){draw(+this.value);});
const say=box.append("div")
  .attr("style","font:0.9em inherit;min-height:5.4em;margin-top:0.3em");
const fmt=d3.format(","), f5=d3.format(".5f");
// how many seats each state holds just before seat k is handed out
function held(k){
  const s=AP.st.map(()=>1);
  for(const q of Q){ if(q.k>=k) break; s[AP.idx[q.s]]++; }
  return s;
}
function draw(k){
  const q=Q[k-51], s=held(k);
  mline.attr("x1",x(q.k)).attr("x2",x(q.k));
  mdot.attr("cx",x(q.k)).attr("cy",y(q.v));
  const bids=AP.st.map((nm,i)=>({s:nm,n:s[i]+1,
      v:AP.pop[i]/Math.sqrt((s[i]+1)*s[i])}))
    .sort((a,b)=>b.v-a.v).slice(0,4);
  const rows=bids.map((b,i)=>"<tr><td style=\\"padding:1px 10px 1px 0\\">"+
      (i?"":"<b>winner</b> ")+b.s+"</td><td style=\\"padding:1px 10px 1px 0\\">"+
      "its "+b.n+(b.n%%10===2&&b.n!==12?"nd":b.n%%10===3&&b.n!==13?"rd":"th")+
      " seat</td><td style=\\"padding:1px 0;text-align:right\\">"+
      fmt(Math.round(b.v))+"</td></tr>").join("");
  say.html("<b>Seat "+q.k+(q.k>STAT?" (past the statute)":"")+" goes to "+q.s+
    "</b>, its "+q.n+(q.n%%10===2&&q.n!==12?"nd":q.n%%10===3&&q.n!==13?"rd":"th")+
    ". "+fmt(AP.pop[AP.idx[q.s]])+" \\u00F7 \\u221A("+q.n+" \\u00D7 "+(q.n-1)+
    ") = "+fmt(Math.round(q.v))+
    "<br><span style=\\"color:#4E5A63\\">the four highest bids at this point"+
    "</span><table style=\\"font:0.92em inherit;border:0;margin:0.2em 0 0\\">"+
    rows+"</table>");
}
draw(51);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<b>Drag the slider.</b> Each tread is one seat, drawn at the price the winning
state paid for it. The line never rises, because winning lowers the winner\'s
own next bid. The dotted line is the price of seat %d, the level %s had to reach
and did not; the shaded band past it is the ten further seats the Bureau
published priority values for and the statute does not allow.</p>
', pqrows, win_pri, sum(ap$seats), sum(ap$seats), ladder$state[1]))

## ---- reproduce
data.frame(
  check = c("Seats handed out one at a time",
            "Total when the queue empties",
            "Does this reproduce the official House, state for state?",
            "Do the recomputed prices match the Bureau's published ones?"),
  result = c(n(sum(ap$seats) - nrow(ap)), n(sum(base)),
             ifelse(ok435, "TRUE", "FALSE"), ifelse(pv_ok, "TRUE", "FALSE")))

## ---- last
o <- data.frame(seat = 430:435, went_to = tail(b435$order, 6))
o

## ---- past435
o <- head(pv436[, c("house_seat", "state", "state_seat", "priority")], 5)
o$priority <- n(round(o$priority))
o$state_seat <- paste("its", ordn(o$state_seat))
names(o) <- c("if the House had this many seats", "the next one would go to",
              "seat number", "at a price of")
o

## ---- waffle-static
tot  <- sum(ap$seats); nfl <- nrow(ap); nc <- 29; nr <- tot / nc
kind <- c(rep(1L, nfl), rep(2L, tot - nfl - 1L), 3L)
cols <- c("#999999", "#2c7fb8", "#C41230")[kind]
i  <- seq_len(tot) - 1L
cx <- i %% nc; cy <- i %/% nc
par(mar = c(5.2, 0.6, 0.6, 0.6))
plot(NA, xlim = c(0, nc), ylim = c(0, nr), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
rect(cx + 0.08, nr - cy - 0.92, cx + 0.92, nr - cy - 0.08, col = cols,
     border = NA)
legend("bottom", horiz = TRUE, inset = c(0, -0.30), xpd = NA, bty = "n",
       cex = 0.72, pch = 15, col = c("#999999", "#2c7fb8", "#C41230"),
       legend = c(paste(nfl, "guaranteed: one per state"),
                  paste(tot - nfl - 1, "allocated by priority"),
                  paste("seat", tot, "-", tail(b435$order, 1))))
mtext(paste0("One square is one seat. The single red square is the seat ",
             ladder$state[1], " missed by ", n(margin), " people."),
      side = 1, line = 3.7, cex = 0.62, col = "#666666")

## ---- waffle-d3
tot <- sum(ap$seats); nfl <- nrow(ap)
cat(sprintf('
<div id="waf" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const TOT=%d,FL=%d,NC=29,NR=TOT/NC,S=22,P=2.6;
const W=NC*S+40,H=NR*S+58;
const svg=d3.select("#waf").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const col=i=>i<FL?"#999999":(i===TOT-1?"#C41230":"#2c7fb8");
const lab=i=>i<FL?"one of the %d seats the Constitution guarantees every state"
  :(i===TOT-1?"seat %d \\u2014 %s, the last one handed out":"seat "+(i+1)+" of %d, won on priority");
const cap=d3.select("#waf").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
// Motion is decoration: rAF is frozen in a hidden tab, so the end state is
// written directly whenever the transition cannot be trusted to run.
const anim=!document.hidden&&!(window.matchMedia&&
  window.matchMedia("(prefers-reduced-motion: reduce)").matches);
const sq=svg.append("g").selectAll("rect").data(d3.range(TOT)).join("rect")
  .attr("x",i=>20+(i%%NC)*S).attr("y",i=>10+Math.floor(i/NC)*S)
  .attr("width",S-P).attr("height",S-P).attr("fill",col).attr("rx",1.5)
  .attr("opacity",anim?0:1)
  .on("mousemove",(e,i)=>cap.html("<b>"+lab(i)+"</b>"));
if(anim) sq.transition().delay(i=>i*1.6).duration(220).attr("opacity",1);
const lg=svg.append("g").attr("transform",`translate(20,${H-18})`);
[["#999999","%d guaranteed: one per state"],
 ["#2c7fb8","%d allocated by priority"],
 ["#C41230","seat %d \\u2014 %s"]].forEach((r,i)=>{
  lg.append("rect").attr("x",i*215).attr("y",-9).attr("width",11).attr("height",11)
    .attr("fill",r[0]);
  lg.append("text").attr("x",i*215+16).attr("y",1).attr("font-size","11.5px")
    .attr("fill","#333").text(r[1]);});
cap.html("<b>Hover a square.</b> The one red square is the seat %s missed by %s people.");
})();
</script>
', tot, nfl, nfl, tot, tail(b435$order, 1), tot,
   nfl, tot - nfl - 1, tot, tail(b435$order, 1),
   ladder$state[1], n(margin)))

## ---- howclose
data.frame(
  quantity = c("What New York was counted at",
               "What New York needed to take that seat",
               "Short by"),
  people = c(n(ny$app_pop), n(need_ny), n(margin)))

## ---- ladder
o <- head(ladder, 6)
o$priority <- pc(o$priority, 0); o$short <- n(o$short)
names(o) <- c("state", "priority for one more seat", "people short of it")
o

## ---- ladder-static
d <- head(ladder, 8); d <- d[order(-d$short), ]
par(mar = c(5.4, 8.5, 1.0, 5.2))
plot(NA, xlim = c(50, 1.2e6), ylim = c(0.5, nrow(d) + 0.5), log = "x",
     axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(100, 1000, 1e4, 1e5, 1e6),
     labels = c("100", "1,000", "10,000", "100,000", "1,000,000"), cex.axis = 0.8)
axis(2, at = seq_len(nrow(d)), labels = d$state, las = 1, tick = FALSE,
     cex.axis = 0.85, line = -0.5)
abline(v = c(100, 1000, 1e4, 1e5, 1e6), col = "#eeeeee")
points(pmax(d$short, 1), seq_len(nrow(d)), pch = 19, cex = 1.5,
       col = ifelse(seq_len(nrow(d)) == nrow(d), "#C41230", "#54278F"))
text(pmax(d$short, 1), seq_len(nrow(d)), labels = n(d$short), pos = 4,
     cex = 0.72, xpd = NA)
mtext("people short of one more seat (log scale)", side = 1, line = 2.4,
      cex = 0.85)
mtext(paste0("A dot, not a bar: on a log scale a bar's length would encode the ",
             "distance from an arbitrary origin, not the quantity."),
      side = 1, line = 3.5, cex = 0.6, col = "#666666")
mtext(paste0("These are the eight states nearest a further seat, ordered by how ",
             "short they fell. ", ladder$state[1], " is about ",
             n(round(runner / margin)), " times closer than the next of them."),
      side = 1, line = 4.3, cex = 0.6, col = "#666666")

## ---- ladder-d3
d <- head(ladder, 8); d <- d[order(d$short), ]
rows <- paste(sprintf('{"s":"%s","v":%d}', d$state, d$short), collapse = ",")
cat(sprintf('
<div id="lad" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=340,M={t:16,r:96,b:44,l:104};
const svg=d3.select("#lad").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([50,1.2e6]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([M.t,H-M.b]).padding(0.24);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(5,"~s"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("people short of one more seat (log scale)");
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",M.l).attr("x2",W-M.r)
  .attr("y1",d=>y(d.s)+y.bandwidth()/2).attr("y2",d=>y(d.s)+y.bandwidth()/2)
  .attr("stroke","#eeeeee");
// As in the waffle: the dots must land even when no frame ever ticks.
const anim=!document.hidden&&!(window.matchMedia&&
  window.matchMedia("(prefers-reduced-motion: reduce)").matches);
const dot=svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cy",d=>y(d.s)+y.bandwidth()/2).attr("r",6)
  .attr("fill",(d,i)=>i?"#54278F":"#C41230")
  .attr("cx",d=>anim?x(50):x(d.v));
if(anim) dot.transition().duration(750).attr("cx",d=>x(d.v));
const val=svg.append("g").selectAll("text").data(D).join("text")
  .attr("x",d=>x(d.v)+11).attr("y",d=>y(d.s)+y.bandwidth()/2+4)
  .attr("font-size","11.5px").attr("fill","#333").attr("opacity",anim?0:1)
  .text(d=>d3.format(",")(d.v));
if(anim) val.transition().delay(750).duration(300).attr("opacity",1);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Each dot is how many more residents a state needed for one additional seat. The
scale is logarithmic, so a dot rather than a bar: bar <em>length</em> on a log
axis would encode the distance from an arbitrary origin rather than the
quantity. These are the eight states nearest a further seat, ordered by how
short they fell; %s is about %s times closer than the next of them.</p>
', rows, ladder$state[1], n(round(runner / margin))))

## ---- overseas
data.frame(
  quantity = c("New York's population counted from overseas",
               "Minnesota's population counted from overseas",
               "The margin that decided the 435th seat"),
  people = c(n(ny$overseas), n(mn$overseas), n(margin)))

## ---- rules
data.frame(
  method = c("Adams", "Dean", "Huntington-Hill", "Webster", "Jefferson",
             "Hamilton"),
  `the divisor for a state's next seat` = c(
    "k — the smallest possible, so every fraction rounds up",
    "the harmonic mean of k and k+1 — which comes to making each state's district as close to the ideal size as it can be, counted in people",
    "the geometric mean of k and k+1: √(k(k+1)) — the same aim, counted in percentages rather than in people",
    "k + 0.5 — the arithmetic mean, which is ordinary rounding",
    "k + 1 — the largest possible, so every fraction rounds down",
    "not a divisor at all: whole shares first, then the largest fractions"),
  `who it favours` = c("small states", "small states", "small states, slightly",
                       "neither, on average", "large states", "neither"),
  check.names = FALSE)

## ---- method-table
mt <- as.data.frame(mmoved)
mt <- data.frame(state = rownames(mmoved),
                 seats = as.integer(M435[rownames(mmoved), "Huntington-Hill"]),
                 mt[, c("Adams", "Dean", "Webster", "Jefferson", "Hamilton")],
                 check.names = FALSE, stringsAsFactors = FALSE)
for (cc in c("Adams", "Dean", "Webster", "Jefferson", "Hamilton"))
  mt[[cc]] <- ifelse(mt[[cc]] == 0, "·", sprintf("%+d", mt[[cc]]))
names(mt)[1:2] <- c("state", "seats under Huntington-Hill")
mt

## ---- quotaviol
data.frame(method = names(qv),
           `states given more or fewer seats than their fair share allows` =
             ifelse(nzchar(qv), qv, "none"),
           check.names = FALSE)

## ---- mex-static
mxm <- c("Adams", "Dean", "Huntington-Hill", "Webster", "Jefferson", "Hamilton")
mxd <- mdiff[rownames(mmoved), mxm, drop = FALSE]
mxo <- order(M435[rownames(mxd), "Huntington-Hill"])
mxd <- mxd[mxo, , drop = FALSE]
par(mar = c(3.4, 8.6, 3.0, 1.0))
plot(NA, xlim = c(0.5, length(mxm) + 0.5), ylim = c(0.5, nrow(mxd) + 0.5),
     axes = FALSE, xlab = "", ylab = "")
for (i in seq_len(nrow(mxd))) for (j in seq_along(mxm)) {
  v <- mxd[i, j]
  rect(j - 0.45, i - 0.42, j + 0.45, i + 0.42, border = "#FFFFFF", lwd = 1.2,
       col = if (v > 0) "#2c7fb8" else if (v < 0) "#C41230" else "#EDEFF1")
  if (v != 0) text(j, i, sprintf("%+d", v), cex = 0.7, col = "#FFFFFF")
}
axis(2, at = seq_len(nrow(mxd)),
     labels = paste0(rownames(mxd), "  (",
                     M435[rownames(mxd), "Huntington-Hill"], ")"),
     las = 1, tick = FALSE, cex.axis = 0.72, line = -0.6)
mtext(mxm, side = 3, at = seq_along(mxm), line = 0.3, cex = 0.68)
mtext(paste0("Seats each rule would add to or take from the official House. ",
             "Every state not listed holds the same number under all six."),
      side = 1, line = 1.2, cex = 0.62, col = "#666666")
mtext("Blue gains a seat, red loses one; the number in brackets is the official count.",
      side = 1, line = 2.0, cex = 0.62, col = "#666666")

## ---- mex-d3
cat(sprintf('
<div id="mex" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const AP=window.DD_AP, OFF=[%s], STAT=%d, TOTPOP=%.0f;
const RULES=AP.rules;
let rule="Huntington-Hill", size=STAT;
const box=d3.select("#mex");
const bar=box.append("div").attr("style","margin:0 0 8px");
const btns=bar.selectAll("button").data(RULES).join("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;"
   +"border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text(d=>d).on("click",(e,d)=>{rule=d;draw();});
const srow=box.append("div")
  .attr("style","display:flex;align-items:center;gap:10px;margin:0 0 8px");
const slab=srow.append("span")
  .attr("style","font:0.9em inherit;min-width:11em;color:#4E5A63");
srow.append("input").attr("type","range").attr("min",300).attr("max",1000)
  .attr("value",STAT).attr("style","flex:1;accent-color:#2c7fb8")
  .on("input",function(){size=+this.value;draw();});
srow.append("button").text("back to " + STAT)
  .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;"
   +"cursor:pointer;font:11.5px inherit;background:#fff")
  .on("click",function(){size=STAT;box.select(\'input[type=range]\').property("value",STAT);draw();});
const head=box.append("p").attr("style","font:0.92em inherit;margin:0 0 0.4em");
const list=box.append("div").attr("style","font:0.92em inherit");
const fmt=d3.format(",");
function draw(){
  btns.style("background",d=>d===rule?"#1C4C5C":"#fff")
      .style("color",d=>d===rule?"#fff":"#12181D")
      .style("font-weight",d=>d===rule?"600":"400");
  slab.text("a House of "+size+" seats");
  const cur=AP.seats(rule,size);
  const selfHH = (rule==="Huntington-Hill");
  const base = selfHH ? OFF : AP.seats("Huntington-Hill",size);
  const baseName = selfHH ? ("the official House of "+STAT)
                          : ("Huntington-Hill at "+size+" seats");
  const rows=[];
  for(let i=0;i<AP.st.length;i++) if(cur[i]!==base[i])
    rows.push({s:AP.st[i],a:base[i],b:cur[i],d:cur[i]-base[i]});
  rows.sort((p,q)=>q.d-p.d||q.b-p.b);
  const q=AP.pop.map(p=>size*p/TOTPOP);
  const viol=AP.st.filter((s,i)=>cur[i]>Math.ceil(q[i])||cur[i]<Math.floor(q[i]));
  const per=AP.pop.map((p,i)=>p/cur[i]);
  head.html("<b>"+rule+", a House of "+size+".</b> "+
    (rows.length?rows.length+" state"+(rows.length===1?"":"s")+
      " hold a different number of seats than under "+baseName+"."
     :"No state\'s seat count differs from "+baseName+".")+
    "<br><span style=\'color:#4E5A63\'>largest district "+
    fmt(Math.round(d3.max(per)))+" people, smallest "+
    fmt(Math.round(d3.min(per)))+", a ratio of "+
    (d3.max(per)/d3.min(per)).toFixed(2)+
    ". More or fewer seats than the fair share allows: "+
    (viol.length?viol.join(", "):"none")+".</span>");
  const li=list.selectAll("div.r").data(rows,d=>d.s);
  li.exit().remove();
  const en=li.enter().append("div").attr("class","r")
    .attr("style","display:flex;align-items:center;gap:8px;padding:1px 0");
  en.append("span").attr("class","nm")
    .attr("style","min-width:11em;text-align:right");
  en.append("span").attr("class","vv");
  const all=en.merge(li);
  all.select("span.nm").text(d=>d.s);
  all.select("span.vv").html(d=>"<span style=\'color:#4E5A63\'>"+d.a+
    "</span> \\u2192 <b style=\'color:"+(d.d>0?"#1a5c88":"#A11026")+"\'>"+d.b+
    "</b> <span style=\'color:"+(d.d>0?"#1a5c88":"#A11026")+"\'>("+
    (d.d>0?"+":"")+d.d+")</span>");
  all.order();
}
draw();
// Every other figure in this book draws an <svg>, and the fallback sweep in
// brief-head.html hides a static twin whenever it finds one nearby. This
// figure draws a list of states instead, so the sweep cannot tell that it
// ran and would leave the twin on the page beside it. Say so directly. With
// scripts stripped -- in a mail client -- none of this runs and the twin is
// what the reader sees, which is the whole point of it.
for (let el = document.getElementById("mex").previousElementSibling, i = 0;
     el && i < 4; el = el.previousElementSibling, i++) {
  if (el.classList && el.classList.contains("dd-fallback")) {
    el.style.display = "none"; break;
  }
}
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.4em">
<b>Pick a rule, then drag the size.</b> Every seat is recomputed from the fifty
apportionment populations while you watch; nothing here is precomputed. With
Huntington&#8211;Hill selected the comparison is against the official House of
%d, so the list shows what growing the House alone would do. With any other rule
selected the comparison is against Huntington&#8211;Hill at the <em>same</em>
size, so the list shows what changing the rule alone would do.</p>
', paste(as.integer(ap$seats), collapse = ","),
   sum(ap$seats), sum(ap$app_pop), sum(ap$seats)))

## ---- alabama
o <- para[c(which(para$state == "Alabama")[1],
            setdiff(seq_len(nrow(para)), which(para$state == "Alabama"))[1:5]), ]
o <- data.frame(
  `growing the House from` = paste(o$from, "to", o$to, "seats"),
  `costs this state a seat` = o$state,
  `it drops from` = paste(o$had, "to", o$now),
  `while these two gain` = o$gained,
  check.names = FALSE, stringsAsFactors = FALSE)
o

## ---- alp-static
par(mar = c(4.2, 4.4, 2.4, 1.2))
plot(NA, xlim = range(atrack$size), ylim = range(c(atrack$hamilton, atrack$hh)) +
       c(-0.4, 0.6), axes = FALSE, xlab = "", ylab = "")
axis(1, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8,
     at = seq(min(atrack$hamilton), max(atrack$hh)))
lines(atrack$size, atrack$hh, type = "s", col = "#9EC4DE", lwd = 7)
lines(atrack$size, atrack$hamilton, type = "s", col = "#C41230", lwd = 2.4)
abline(v = alab$to, col = "#999999", lty = 3)
mtext("total seats in the House", side = 1, line = 2.4, cex = 0.85)
mtext("Alabama's seats", side = 2, line = 2.9, cex = 0.85)
legend("topleft", bty = "n", cex = 0.75, lwd = c(7, 2.4),
       col = c("#9EC4DE", "#C41230"),
       legend = c("Huntington-Hill (wide band): never falls",
                  "Hamilton: falls at this point"))
mtext(paste0("At ", alab$from, " seats Alabama holds ", alab$had,
             " under Hamilton's method; at ", alab$to, " it holds ", alab$now,
             ". Huntington-Hill cannot do this to any state, ever."),
      side = 1, line = 3.4, cex = 0.62, col = "#666666")

## ---- alp-d3
alrows <- paste(sprintf('[%d,%d,%d]', atrack$size, atrack$hamilton, atrack$hh),
                collapse = ",")
cat(sprintf('
<div id="alp" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s].map(a=>({k:a[0],h:a[1],g:a[2]})), DROP=%d;
const W=760,H=300,M={t:26,r:26,b:46,l:64};
const svg=d3.select("#alp").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.k)).range([M.l,W-M.r]);
const lo=d3.min(D,d=>Math.min(d.h,d.g)), hi=d3.max(D,d=>Math.max(d.h,d.g));
const y=d3.scaleLinear().domain([lo-0.5,hi+0.5]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues(d3.range(lo,hi+1)).tickFormat(d3.format("d")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("total seats in the House");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("Alabama\'s seats");
svg.append("line").attr("x1",x(DROP)).attr("x2",x(DROP)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#999").attr("stroke-dasharray","3,3");
const ln=k=>d3.line().curve(d3.curveStepAfter).x(d=>x(d.k)).y(d=>y(d[k]));
// The two rules agree everywhere except the paradox, so the monotone one is
// drawn as a wide pale band underneath rather than a line the other hides.
svg.append("path").datum(D).attr("fill","none").attr("stroke","#9EC4DE")
  .attr("stroke-width",8).attr("stroke-linecap","round").attr("d",ln("g"));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.4).attr("d",ln("h"));
[["#9EC4DE","Huntington\\u2013Hill (band): never falls",0,8],
 ["#C41230","Hamilton: falls here",280,2.4]].forEach(r=>{
  svg.append("line").attr("x1",M.l+r[2]).attr("x2",M.l+r[2]+18).attr("y1",14)
    .attr("y2",14).attr("stroke",r[0]).attr("stroke-width",r[3]);
  svg.append("text").attr("x",M.l+r[2]+24).attr("y",18).attr("font-size","11.5px")
    .attr("fill","#333").text(r[1]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Alabama\'s seat count as the House grows, on the 2020 apportionment populations.
At %d seats Alabama holds %d under Hamilton\'s method; at %d it holds %d, with
nobody\'s population changed. Huntington&#8211;Hill cannot do this to any state,
at any size, ever.</p>
', alrows, alab$to, alab$from, alab$had, alab$to, alab$now))

## ---- hsz-ink
# Figure 6 carries two axes, and each axis label wears its own series' colour so
# the reader can tie the label to the line. That link is the point and must not
# be broken, but a colour good for a 2px line can be too dark for 11px text on
# the dark page: #A11026 measures 2.31:1 against --paper there, #1a5c88 2.58:1.
# Same answer brief.css gives the other series-coloured labels -- keep the hue,
# change only the weight, and only in the theme that needs it.
cat('<style>
@media (prefers-color-scheme: dark) {
#hsz text[fill="#A11026" i] { fill:#E8798C; }
#hsz text[fill="#1a5c88" i] { fill:#7FB3D5; }
}
</style>')

## ---- hsz-static
# Two series, two scales. The axis titles used to carry the whole burden of
# saying which line was which; each line now says so itself, at a year where the
# two are far enough apart that the label cannot be read onto the wrong one.
hs <- hist_h[order(hist_h$year), ]
par(mar = c(4.2, 4.6, 2.6, 4.8))
plot(hs$year, hs$pop_per_rep / 1000, type = "o", pch = 19, cex = 0.8,
     col = "#C41230", lwd = 2.2, axes = FALSE, xlab = "", ylab = "",
     ylim = c(0, max(hs$pop_per_rep) / 1000 * 1.08))
axis(1, at = hs$year, labels = hs$year, cex.axis = 0.7, las = 2)
axis(2, las = 1, cex.axis = 0.8, col.axis = "#C41230")
mtext("thousands per seat", side = 2, line = 3.2, cex = 0.8, col = "#C41230")
ir <- which.min(abs(hs$year - 1990))
text(hs$year[ir], hs$pop_per_rep[ir] / 1000, "people per seat", col = "#C41230",
     cex = 0.78, font = 2, pos = 2, offset = 0.7)
par(new = TRUE)
plot(hs$year, hs$reps, type = "o", pch = 19, cex = 0.8, col = "#2c7fb8",
     lwd = 2.2, axes = FALSE, xlab = "", ylab = "", ylim = c(0, 800))
axis(4, las = 1, cex.axis = 0.8, col.axis = "#2c7fb8")
mtext("seats", side = 4, line = 3.0, cex = 0.8, col = "#2c7fb8")
ib <- which.min(abs(hs$year - 1950))
text(hs$year[ib], hs$reps[ib], "seats in the House", col = "#2c7fb8",
     cex = 0.78, font = 2, pos = 1, offset = 1.1)
abline(h = c(wy_house, cube_root), col = "#4d9221", lty = 3)
text(min(hs$year), wy_house, "  Wyoming Rule", pos = 4, cex = 0.66,
     col = "#4d9221")
text(min(hs$year), cube_root, "  cube-root rule", pos = 4, cex = 0.66,
     col = "#4d9221")
mtext(paste0("Each line is labelled where it runs. ",
             "1910 to 2020. The House has been ", n(sum(ap$seats)),
             " since 1913; the district it represents has grown ",
             pc(max(hs$pop_per_rep) / min(hs$pop_per_rep), 1), "-fold."),
      side = 1, line = 2.6, cex = 0.62, col = "#666666")

## ---- hsz-d3
hs <- hist_h[order(hist_h$year), ]
hrows <- paste(sprintf('[%d,%d,%d]', hs$year, hs$reps, round(hs$pop_per_rep)),
               collapse = ",")
cat(sprintf('
<div id="hsz" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s].map(a=>({y:a[0],r:a[1],p:a[2]})), WY=%d, CR=%d;
const W=760,H=340,M={t:22,r:74,b:52,l:70};
const box=d3.select("#hsz");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(D.map(d=>d.y)).range([M.l,W-M.r]);
const yL=d3.scaleLinear().domain([0,d3.max(D,d=>d.p)*1.08]).range([H-M.b,M.t]);
const yR=d3.scaleLinear().domain([0,800]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x)).selectAll("text").attr("transform","rotate(-40)")
  .attr("text-anchor","end").attr("dx","-4").attr("dy","4");
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(yL).ticks(6).tickFormat(d=>(d/1000)+"k"));
svg.append("g").attr("transform",`translate(${W-M.r},0)`)
  .call(d3.axisRight(yR).ticks(5));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#A11026").text("people per seat");
svg.append("text").attr("transform","rotate(90)").attr("x",(H-M.b+M.t)/2)
  .attr("y",-(W-14)).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#1a5c88").text("seats in the House");
[[WY,"Wyoming Rule"],[CR,"cube-root rule"]].forEach(t=>{
  svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",yR(t[0]))
    .attr("y2",yR(t[0])).attr("stroke","#4d9221").attr("stroke-dasharray","3,3");
  svg.append("text").attr("x",M.l+5).attr("y",yR(t[0])-5)
    .attr("font-size","10.5px").attr("fill","#4d9221").text(t[1]+" ("+t[0]+")");});
const mk=(key,scale,col)=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",col)
    .attr("stroke-width",2.2)
    .attr("d",d3.line().x(d=>x(d.y)).y(d=>scale(d[key])));
  svg.append("g").selectAll("circle").data(D).join("circle")
    .attr("cx",d=>x(d.y)).attr("cy",d=>scale(d[key])).attr("r",3.6).attr("fill",col);
};
mk("p",yL,"#C41230"); mk("r",yR,"#2c7fb8");
const cap=box.append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover a census.</b> Blue is the size of the House, red is the "+
  "number of people one seat represents.";
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.y)-16).attr("y",M.t).attr("width",32).attr("height",H-M.b-M.t)
  .attr("fill","transparent")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.y+"</b>: "+d.r+" seats, "+
    d3.format(",")(d.p)+" people per seat."))
  .on("mouseleave",()=>cap.html(DEF));
cap.html(DEF);
})();
</script>
', hrows, wy_house, cube_root))

## ---- spread-static
d1 <- density(per435, adjust = 0.9); d2 <- density(per_wy, adjust = 0.9)
h  <- max(d1$y, d2$y)
par(mar = c(5.8, 1.2, 1.2, 1.2))
plot(NA, xlim = c(3.2e5, 1.13e6), ylim = c(-0.34, 1.08) * h,
     yaxt = "n", bty = "n", ylab = "", xlab = "", xaxt = "n")
axis(1, at = seq(4e5, 1e6, 1e5), labels = paste0(seq(400, 1000, 100), "k"))
polygon(d2$x, d2$y, col = "#4d922133", border = "#4d9221", lwd = 2)
polygon(d1$x, d1$y, col = "#2c7fb833", border = "#2c7fb8", lwd = 2)
segments(per_wy,  0.005 * h, per_wy,  0.05 * h, col = "#4d9221")
segments(per435, 0.065 * h, per435, 0.11 * h, col = "#2c7fb8")
for (k in list(list(per435, "#2c7fb8", -0.14), list(per_wy, "#4d9221", -0.26))) {
  v <- k[[1]]; cl <- k[[2]]; yy <- k[[3]] * h
  segments(min(v), yy, max(v), yy, col = cl, lwd = 3)
  text(min(v), yy, paste0(names(which.min(v)), " "), pos = 2, cex = 0.7, col = cl)
  text(max(v), yy, paste0(" ", names(which.max(v))), pos = 4, cex = 0.7, col = cl)
}
legend("topright", bty = "n", cex = 0.8, lwd = 2,
       col = c("#2c7fb8", "#4d9221"),
       legend = c(paste("a House of", sum(ap$seats)),
                  paste("a House of", wy_house, "(Wyoming Rule)")))
mtext("people per seat", side = 1, line = 2.5, cex = 0.9)
mtext(paste0("Each tick is one state. Adding ", n(wy_house - sum(ap$seats)),
             " seats pulls the bulk of the distribution left"),
      side = 1, line = 3.9, cex = 0.62, col = "#666666")
mtext(paste0("but leaves the spread almost intact: the largest-to-smallest ",
             "ratio falls only from ", pc(sp435$ratio, 2), " to ",
             pc(sp_wy$ratio, 2), "."),
      side = 1, line = 4.7, cex = 0.62, col = "#666666")

## ---- spread-d3
d1 <- density(per435, adjust = 0.9); d2 <- density(per_wy, adjust = 0.9)
cv <- function(dd) paste(sprintf('[%.0f,%.6f]', dd$x, dd$y), collapse = ",")
rg <- function(v) paste(sprintf('%.0f', sort(as.numeric(v))), collapse = ",")
cat(sprintf('
<div id="spr" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const A={c:[%s],r:[%s],col:"#2c7fb8",lab:"a House of %d",
  lo:"%s",hi:"%s"};
const B={c:[%s],r:[%s],col:"#4d9221",lab:"a House of %d (Wyoming Rule)",
  lo:"%s",hi:"%s"};
const W=760,H=360,M={t:18,r:70,b:96,l:70};
const svg=d3.select("#spr").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([3.4e5,1.11e6]).range([M.l,W-M.r]);
const ymax=d3.max([A,B],s=>d3.max(s.c,p=>p[1]));
const y=d3.scaleLinear().domain([0,ymax*1.05]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickValues(d3.range(4e5,1.05e6,1e5))
    .tickFormat(d=>(d/1000)+"k"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-58).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("people per seat");
const area=d3.area().x(p=>x(p[0])).y0(y(0)).y1(p=>y(p[1]));
[[B,2,11],[A,13,22]].forEach(q=>{
  const s=q[0];
  svg.append("path").datum(s.c).attr("fill",s.col).attr("fill-opacity",0.2)
    .attr("stroke",s.col).attr("stroke-width",2).attr("d",area);
  svg.append("g").selectAll("line").data(s.r).join("line")
    .attr("x1",v=>x(v)).attr("x2",v=>x(v)).attr("y1",y(0)-q[1]).attr("y2",y(0)-q[2])
    .attr("stroke",s.col).attr("stroke-opacity",0.8);});
[[A,26],[B,45]].forEach(q=>{
  const s=q[0],yy=H-M.b+q[1];
  svg.append("line").attr("x1",x(s.r[0])).attr("x2",x(s.r[s.r.length-1]))
    .attr("y1",yy).attr("y2",yy).attr("stroke",s.col).attr("stroke-width",3);
  svg.append("text").attr("x",x(s.r[0])-6).attr("y",yy+4).attr("text-anchor","end")
    .attr("font-size","10.5px").attr("fill",s.col).text(s.lo);
  svg.append("text").attr("x",x(s.r[s.r.length-1])+6).attr("y",yy+4)
    .attr("font-size","10.5px").attr("fill",s.col).text(s.hi);});
const lg=svg.append("g").attr("transform",`translate(${W-M.r-250},${M.t+6})`);
[A,B].forEach((s,i)=>{
  lg.append("line").attr("x1",0).attr("x2",18).attr("y1",i*17).attr("y2",i*17)
    .attr("stroke",s.col).attr("stroke-width",2.5);
  lg.append("text").attr("x",24).attr("y",i*17+4).attr("font-size","11.5px")
    .attr("fill","#333").text(s.lab);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Each tick is one state. Adding %s seats pulls the bulk of the distribution left
but leaves the spread almost intact: the largest-to-smallest ratio falls only
from %s to %s.</p>
', cv(d1), rg(per435), sum(ap$seats), names(which.min(per435)),
   names(which.max(per435)),
   cv(d2), rg(per_wy), wy_house, names(which.min(per_wy)),
   names(which.max(per_wy)),
   n(wy_house - sum(ap$seats)), pc(sp435$ratio, 2), pc(sp_wy$ratio, 2)))

## ---- hsizes
data.frame(
  `House size` = c(n(sum(ap$seats)), n(wy_house), n(cube_root)),
  `named` = c("in force since 1913", "Wyoming Rule", "cube-root rule"),
  `largest district` = c(n(sp435$max), n(sp_wy$max), n(sp_cr$max)),
  `smallest district` = c(n(sp435$min), n(sp_wy$min), n(sp_cr$min)),
  `ratio between them` = c(pc(sp435$ratio, 2), pc(sp_wy$ratio, 2),
                           pc(sp_cr$ratio, 2)),
  check.names = FALSE)

## ---- ecb-ink
# Figure 8 labels each line in its own line's colour, so a reader can tie the
# name to the curve. That link is the point and must not be broken, but a
# colour good for a 2.4px stroke is too dark for 11.5px text on the dark page:
# #54278F measures 1.82:1 against --paper there, #1a5c88 2.58:1. Same answer
# Figure 6 gets -- keep the hue, change only the weight, and only in the theme
# that needs it. The strokes themselves are unchanged in both themes.
cat('<style>
@media (prefers-color-scheme: dark) {
#ecb text[fill="#54278F" i] { fill:#A98BD8; }
#ecb text[fill="#1a5c88" i] { fill:#7FB3D5; }
}
</style>')

## ---- ecb-static
# Two series on one axis, because both are the same quantity: how many times
# more weight the best-served state's resident carries than the worst-served
# state's. Which states those are changes as seats are handed out, so neither
# line is about a fixed pair.
par(mar = c(4.4, 4.8, 1.4, 1.2))
plot(ecg$size, ecg$ratio, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
     ylim = c(1, max(ecg$ratio) * 1.04))
abline(h = 1, col = "#999999", lty = 3)
for (v in c(435, wy_house, cube_root)) {
  abline(v = v, col = "#CBD3D8", lty = 2)
  text(v, max(ecg$ratio) * 1.02, v, cex = 0.66, col = "#4E5A63", pos = 4)
}
lines(ecg$size, ecg$ratio, col = "#54278F", lwd = 2.4)
lines(ecg$size, ecg$house, col = "#1a5c88", lwd = 2.4)
points(435, ec435, pch = 19, col = "#C41230", cex = 1.3)
points(435, hs435, pch = 19, col = "#C41230", cex = 1.3)
# Each line carries its own name, at a size where the two are far apart.
il <- which.min(abs(ecg$size - 660))
text(ecg$size[il], ecg$ratio[il], "for President", col = "#54278F",
     cex = 0.76, font = 2, pos = 3, offset = 0.55)
text(ecg$size[il], ecg$house[il], "for the House", col = "#1a5c88",
     cex = 0.76, font = 2, pos = 3, offset = 0.55)
mtext("seats in the House", side = 1, line = 2.4, cex = 0.85)
mtext("weight of the best-served state,\ndivided by the worst-served",
      side = 2, line = 2.4, cex = 0.8)
mtext(paste0("At ", n(sum(ap$seats)), " seats the presidential ratio is ",
             pc(ec435, 2), " and the House ratio ", pc(hs435, 2),
             ". A House of ", n(cube_root), " would leave them at ",
             pc(ec_cr, 2), " and ", pc(hs_cr, 2), "."),
      side = 1, line = 3.4, cex = 0.62, col = "#666666")

## ---- ecb-d3
ecrows <- paste(sprintf('[%d,%.4f,%.4f]', ecg$size, ecg$ratio, ecg$house),
                collapse = ",")
cat(sprintf('
<div id="ecb" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s].map(a=>({k:a[0],v:a[1],h:a[2]})), MARKS=[[%d,"in force"],[%d,"Wyoming Rule"],[%d,"cube root"]];
const W=760,H=320,M={t:20,r:26,b:48,l:78};
const svg=d3.select("#ecb").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.k)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([1,d3.max(D,d=>d.v)*1.04]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d.toFixed(1)+"\\u00D7"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("seats in the House");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("font-size","11.5px")
  .attr("fill","#444").text("best-served state \\u00F7 worst-served state");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(1)).attr("y2",y(1))
  .attr("stroke","#999").attr("stroke-dasharray","3,3");
MARKS.forEach(m=>{
  svg.append("line").attr("x1",x(m[0])).attr("x2",x(m[0])).attr("y1",M.t)
    .attr("y2",H-M.b).attr("stroke","#CBD3D8").attr("stroke-dasharray","2,3");
  svg.append("text").attr("x",x(m[0])+4).attr("y",M.t+11).attr("font-size","10.5px")
    .attr("fill","#4E5A63").text(m[1]+" ("+m[0]+")");});
[["v","#54278F","for President"],["h","#1a5c88","for the House"]].forEach(q=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",q[1])
    .attr("stroke-width",2.4).attr("d",d3.line().x(d=>x(d.k)).y(d=>y(d[q[0]])));
  // Each line says which it is, rather than sending the reader to a key.
  const a=D.reduce((p,r)=>Math.abs(r.k-660)<Math.abs(p.k-660)?r:p);
  svg.append("text").attr("x",x(a.k)).attr("y",y(a[q[0]])-9)
    .attr("text-anchor","middle").attr("font-size","11.5px")
    .attr("font-weight","700").attr("fill",q[1]).text(q[2]);});
const cap=d3.select("#ecb").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const DEF="<b>Hover the chart.</b> A ratio of 1 would mean every American "+
  "counted the same. Neither line reaches it.";
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const k=Math.round(x.invert(d3.pointer(e,this)[0]+M.l));
    const d=D.reduce((p,q)=>Math.abs(q.k-k)<Math.abs(p.k-k)?q:p);
    cap.html("<b>A House of "+d.k+"</b>: the best-served state carries "+
      d.v.toFixed(2)+" times the presidential weight of the worst-served, and "+
      d.h.toFixed(2)+" times the weight in the House.");})
  .on("mouseleave",()=>cap.html(DEF));
cap.html(DEF);
})();
</script>
', ecrows, sum(ap$seats), wy_house, cube_root))

## ---- pps-map-static
op <- par(mar = c(0.4, 0.4, 0.4, 0.4))
plot(NA, xlim = c(0, 960), ylim = c(600, 0), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
ramp <- colorRampPalette(c("#2c7fb8", "#eeeeee", "#C41230"))(101)
fill <- setNames(ramp[round(50 + 50 * mapd$dev / MAXDEV) + 1], mapd$state)
for (p in unique(sr$part)) {
  g <- sr[sr$part == p, ]
  polygon(g$x, g$y, col = fill[[g$state[1]]], border = "#FFFFFF", lwd = 0.6)
}
rect(660 + (0:100) * 2.4, 556, 660 + (1:101) * 2.4, 572,
     col = ramp, border = NA)
text(660, 550, paste0("-", round(100 * MAXDEV), "%"), cex = 0.6, adj = 0)
text(902, 550, paste0("+", round(100 * MAXDEV), "%"), cex = 0.6, adj = 1)
text(781, 586, "people per seat vs. national average", cex = 0.62, adj = 0.5)
par(op)

## ---- pps-map-d3
# Fifty polygons, drawn from integer canvas coordinates computed in advance.
# Two modes on the same geography: how much a seat costs in each state, and how
# far each state was from buying another one. Alaska and Hawaii are insets --
# the tooltip says so, because a reader should not have to infer from the
# picture that their position is a convention.
rings <- vapply(sort(unique(sr$part)), function(p) {
  g <- sr[sr$part == p, ]
  paste0('{s:"', g$state[1], '",d:"M',
         paste0(g$x, ",", g$y, collapse = "L"), 'Z"}')
}, character(1))
vals <- paste0('{s:"', mapd$state, '",seats:', mapd$seats,
               ',pop:', mapd$app_pop, ',pps:', mapd$people_per_seat,
               ',dev:', round(mapd$dev, 5), ',short:', mapd$short, '}',
               collapse = ",")
cat(paste0('
<div id="ppsmap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const G=[', paste(rings, collapse = ","), '];
const V=[', vals, '];
const MAXDEV=', round(MAXDEV, 5), ', NAT=', round(NATAVG), ';
const INSET={Alaska:1,Hawaii:1};
const byS={}; V.forEach(v=>byS[v.s]=v);
const MODE=[{k:"dev",lab:"people per seat"},
            {k:"short",lab:"how far from one more seat"}];
let sel=0;
const box=d3.select("#ppsmap");
const bar=box.append("div").attr("style","margin:0 0 6px");
const svg=box.append("svg").attr("viewBox","0 0 960 640")
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const div=d3.scaleLinear().domain([-MAXDEV,0,MAXDEV])
  .range(["#2c7fb8","#eeeeee","#C41230"]).clamp(true);
const shortMax=d3.max(V,v=>v.short);
const seq=d3.scaleSqrt().domain([0,shortMax]).range([0,1]).clamp(true);
function colour(v){
  return sel===0 ? div(v.dev)
                 : d3.interpolateRgb("#2c7fb8","#eeeeee")(seq(v.short));
}
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
const fmt=d3.format(",d");
const paths=svg.selectAll("path.st").data(G).join("path").attr("class","st")
  .attr("d",g=>g.d).attr("stroke","#fff").attr("stroke-width",0.7)
  .on("mousemove",function(e,g){
    const v=byS[g.s]; if(!v) return;
    svg.selectAll("path.st").attr("stroke-width",q=>q.s===g.s?2:0.7)
       .attr("stroke",q=>q.s===g.s?"#12181D":"#fff");
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+g.s+"</b>"+(INSET[g.s]?" <i>(inset)</i>":"")+"<br>"+
         v.seats+(v.seats===1?" seat":" seats")+"<br>"+
         fmt(v.pps)+" people per seat<br>"+
         (v.dev>=0?"+":"")+(100*v.dev).toFixed(1)+"% vs. the national "+fmt(NAT)+
         "<br>short of one more seat by <b>"+fmt(v.short)+"</b> people");
  })
  .on("mouseleave",function(){
    tip.style("opacity",0);
    svg.selectAll("path.st").attr("stroke-width",0.7).attr("stroke","#fff");
  });
const lg=svg.append("g").attr("transform","translate(600,600)");
const lw=260;
const stops=d3.range(0,101).map(i=>i/100);
lg.selectAll("rect").data(stops).join("rect")
  .attr("x",d=>d*lw).attr("y",0).attr("width",lw/100+0.6).attr("height",12);
const lgL=lg.append("text").attr("x",0).attr("y",26).attr("font-size","10.5px")
  .attr("fill","#4E5A63");
const lgR=lg.append("text").attr("x",lw).attr("y",26).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#4E5A63");
const lgT=lg.append("text").attr("x",lw/2).attr("y",-6).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63");
const btns=bar.selectAll("button").data(MODE).join("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text(d=>d.lab)
  .on("click",function(e,d){sel=MODE.indexOf(d);draw();});
// No entry transition: rAF is frozen in a background tab, and a map that
// animates in would simply be absent for a reader who opens the chapter there.
function draw(){
  paths.attr("fill",g=>byS[g.s]?colour(byS[g.s]):"#ddd");
  lg.selectAll("rect").attr("fill",d=>sel===0
    ? div(-MAXDEV+d*2*MAXDEV)
    : d3.interpolateRgb("#2c7fb8","#eeeeee")(d));
  lgL.text(sel===0?("-"+Math.round(100*MAXDEV)+"%"):"0 people");
  lgR.text(sel===0?("+"+Math.round(100*MAXDEV)+"%"):fmt(shortMax));
  lgT.text(sel===0?"people per seat vs. the national average"
                  :"people still needed for one more seat");
  btns.style("background",(d,i)=>i===sel?"#1C4C5C":"#fff")
      .style("color",(d,i)=>i===sel?"#fff":"#12181D")
      .style("font-weight",(d,i)=>i===sel?"600":"400");
}
draw();
})();
</script>'))

## ---- scm-static
# One cell is one seat. Every state is carved into as many equal-area cells as
# it held at its peak across the two counts, so the cells are the same size
# nationally and a state that lost a seat has one cell more than it now needs.
# States are drawn apart, each scaled by its seat count -- that transform, and
# the label placement, were done in build-data.R, so nothing here projects or
# positions anything. Which cell carries the mark is presentational: a seat is
# apportioned to a state, not to a place inside it.
scr <- read.csv("data/derived/seat_change_rings.csv", stringsAsFactors = FALSE)
scl <- read.csv("data/derived/seat_change_labels.csv", stringsAsFactors = FALSE)
SCCOL <- c(retained = "#B9BEC4", gained = "#0072B2", lost = "#D55E00")

op <- par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = range(scr$x) + c(-16, 16), ylim = rev(range(scr$y) + c(-16, 34)),
     asp = 1, axes = FALSE, xlab = "", ylab = "")

cells <- scr[scr$status != "outline", ]
for (p in unique(cells$part)) {
  g <- cells[cells$part == p, ]
  polygon(g$x, g$y, col = SCCOL[[g$status[1]]], border = "#FFFFFF", lwd = 0.5)
}
# State borders are drawn twice, as the source draws them: a background-coloured
# casing, then a thin dark line on top. One stroke alone vanishes into the cells.
outl <- scr[scr$status == "outline", ]
for (p in unique(outl$part)) {
  g <- outl[outl$part == p, ]
  polygon(g$x, g$y, col = NA, border = "#FFFFFF", lwd = 2.2)
}
for (p in unique(outl$part)) {
  g <- outl[outl$part == p, ]
  polygon(g$x, g$y, col = NA, border = "#101216", lwd = 1.0)
}

sl  <- scl[scl$kind == "state", ]
# x is an edge of the label's box and anchor says which: "right" means the text
# ends there and hangs to the left of it, "left" that it starts there, "centre"
# that the label is set inside its own state. y is the middle of the cap box in
# every case, so adj[2] = 0.5 and nothing is offset here -- build-data.R
# resolved each position against the shapes and the other labels.
adx <- c(right = 1, left = 0, centre = 0.5)
for (a in unique(sl$anchor)) {
  g <- sl[sl$anchor == a, ]
  text(g$x, g$y, g$label, cex = 0.5, font = 2, col = "#101216",
       adj = c(adx[[a]], 0.5))
}
cl <- scl[scl$kind == "cell", ]
# The source writes U+2212 MINUS SIGN. The pdf device cannot encode it and
# silently substitutes a hyphen with a warning, so do the substitution here
# deliberately -- the data keeps the character it actually arrived with.
text(cl$x, cl$y, sub("\u2212", "-", cl$label), cex = 0.46, font = 2, col = "#FFFFFF")

key <- c(gained = "gained a seat", lost = "lost a seat", retained = "unchanged")
ky  <- max(scr$y) + 26
for (i in seq_along(key)) {
  x0 <- min(scr$x) + 240 + (i - 1) * 175
  rect(x0, ky - 7, x0 + 20, ky + 7, col = SCCOL[[names(key)[i]]], border = "#FFFFFF")
  text(x0 + 27, ky, key[[i]], cex = 0.6, adj = 0)
}
par(op)

## ---- scm-d3
# The same cartogram as the static twin, with the two things a page cannot
# give: the state under the cursor says how many seats it held before and
# after, and the three legend chips filter to the states that gained, lost or
# kept. Nothing is projected or positioned here either -- build-data.R applied
# each state's transform and resolved every label.
#
# Coordinates are written as integers in a space twice the size of the one
# derived/ uses. Whole units in the 960-unit space put visible raggedness on a
# one-seat state's cell, which is only about thirty units across; keeping the
# tenths more than doubles the bytes for detail no reader can see at this size.
scr  <- read.csv("data/derived/seat_change_rings.csv",  stringsAsFactors = FALSE)
scl  <- read.csv("data/derived/seat_change_labels.csv", stringsAsFactors = FALSE)
scst <- read.csv("data/derived/seat_change_states.csv", stringsAsFactors = FALSE)
scj  <- merge(scst, mapd[, c("state", "app_pop", "people_per_seat", "short")],
              by.x = "name", by.y = "state", all.x = TRUE)

SCQ <- 2
scpath <- function(x, y) {
  xi <- as.integer(round(x * SCQ)); yi <- as.integer(round(y * SCQ))
  n  <- length(xi)
  k  <- c(TRUE, xi[-1] != xi[-n] | yi[-1] != yi[-n])
  xi <- xi[k]; yi <- yi[k]; n <- length(xi)
  if (n > 1 && xi[1] == xi[n] && yi[1] == yi[n]) { xi <- xi[-n]; yi <- yi[-n] }
  paste0("M", paste0(xi, ",", yi, collapse = "L"), "Z")
}
scparts <- function(d, extra)
  vapply(split(d, d$part), function(g)
    paste0('{s:"', g$st[1], '",', extra(g), 'd:"', scpath(g$x, g$y), '"}'),
    character(1))

sccell <- scparts(scr[scr$status != "outline", ],
                  function(g) paste0('k:"', g$status[1], '",'))
scoutl <- scparts(scr[scr$status == "outline", ], function(g) "")

# The label positions in derived/ were resolved against the static figure's own
# text metrics, so size the SVG labels to the same cap height: a label that
# clears its state's outline there then clears it here. The left/right anchors
# already point away from the shape, so an approximation errs into empty space
# rather than onto the state.
grDevices::pdf(NULL, width = 7.2, height = 5.6)
scop <- par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = range(scr$x) + c(-16, 16),
     ylim = rev(range(scr$y) + c(-16, 34)), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
SCCAP <- abs(strheight("W", cex = 0.50, font = 2, units = "user"))
SCCEL <- abs(strheight("1", cex = 0.46, font = 2, units = "user"))
par(scop); grDevices::dev.off()
LABFS <- round(SCQ * SCCAP / 0.72, 1)   # a cap height is about 0.72 em
CELFS <- round(SCQ * SCCEL / 0.72, 1)

scs <- scl[scl$kind == "state", ]
sccl <- scl[scl$kind == "cell", ]
slab <- paste0('{s:"', scs$st, '",x:', round(scs$x * SCQ, 1),
               ',y:', round(scs$y * SCQ, 1), ',a:"', scs$anchor, '"}',
               collapse = ",")
clab <- paste0('{t:"', sccl$label, '",x:', round(sccl$x * SCQ, 1),
               ',y:', round(sccl$y * SCQ, 1), '}', collapse = ",")
scv <- paste0('{s:"', scj$st, '",n:"', scj$name, '",f:', scj$seats_2010,
              ',t:', scj$seats_2020, ',c:', scj$change,
              ',pop:', scj$app_pop, ',pps:', scj$people_per_seat,
              ',short:', scj$short, '}', collapse = ",")
SCVB <- round(c(min(scr$x) - 10, min(scr$y) - 10,
              diff(range(scr$x)) + 20, diff(range(scr$y)) + 20) * SCQ)

cat(paste0('
<style>
/* The casing, the outline and the labels that sit on the page follow the
   theme. The cells do not: gained, lost and unchanged are data, and they are
   pinned in both themes -- so the three labels set INSIDE their own state, and
   the +1/-1 marks on the cells, keep their fixed colours. Lifting those with
   the page would put near-white text on near-white grey. */
#scm .cas   { stroke: var(--paper, #ffffff); fill: none; }
#scm .out   { stroke: var(--ink, #101216); fill: none; }
#scm .lab   { fill: var(--ink, #101216); }
#scm .labin { fill: #101216; }
#scm .chip  { margin: 0 6px 4px 0; padding: 3px 9px; cursor: pointer;
  border: 1px solid var(--rule, #CBD3D8); border-radius: 3px;
  font: 11.5px inherit; background: var(--card, #fff); color: var(--ink, #12181D); }
</style>
<div id="scm" style="position:relative;margin:1em 0">
<div id="scmkey" style="margin:0 0 6px"></div>
<svg id="scmsvg" viewBox="', paste(SCVB, collapse = " "), '"
     style="max-width:100%;height:auto;font:', LABFS, 'px inherit"></svg>
</div>
<script>
(function(){
const CELL=[', paste(sccell, collapse = ","), '];
const OUTL=[', paste(scoutl, collapse = ","), '];
const SLAB=[', slab, '];
const CLAB=[', clab, '];
const V=[', scv, '];
const COL={retained:"#B9BEC4",gained:"#0072B2",lost:"#D55E00"};
const CELFS=', CELFS, ';
const KEY=[{k:"gained",lab:"gained a seat"},
           {k:"lost",lab:"lost a seat"},
           {k:"retained",lab:"unchanged"}];
const byS={}; V.forEach(v=>byS[v.s]=v);
const ANCH={left:"start",right:"end",centre:"middle"};
const fmt=d3.format(",d");
let filt=null, hot=null;

const box=d3.select("#scm");
const svg=d3.select("#scmsvg");
const gCell=svg.append("g"), gCas=svg.append("g"),
      gOut=svg.append("g"),  gLab=svg.append("g");

// Cells carry the pointer: they tile the state, so they are a complete hit
// target, and the outline rings would need a fill rule to be one.
const cells=gCell.selectAll("path").data(CELL).join("path")
  .attr("d",c=>c.d).attr("stroke","#FFFFFF").attr("stroke-width",1)
  .attr("fill",c=>COL[c.k]).style("cursor","pointer");

// Borders are drawn twice, as the source draws them: a casing in the page
// colour, then a thin dark line on top. One stroke alone vanishes into the
// cells.
gCas.selectAll("path").data(OUTL).join("path")
  .attr("class","cas").attr("d",o=>o.d).attr("stroke-width",4.4)
  .attr("pointer-events","none");
const outs=gOut.selectAll("path").data(OUTL).join("path").attr("class","out")
  .attr("d",o=>o.d).attr("stroke-width",2).attr("pointer-events","none");

gLab.selectAll("text.s").data(SLAB).join("text")
  .attr("class",l=>l.a==="centre"?"s labin":"s lab")
  .attr("x",l=>l.x).attr("y",l=>l.y).attr("text-anchor",l=>ANCH[l.a])
  .attr("dominant-baseline","central").attr("font-weight","700")
  .attr("pointer-events","none").text(l=>l.s);
gLab.selectAll("text.c").data(CLAB).join("text").attr("class","c")
  .attr("x",l=>l.x).attr("y",l=>l.y).attr("text-anchor","middle")
  .attr("dominant-baseline","central").attr("font-weight","700")
  .attr("font-size",CELFS).attr("fill","#FFFFFF")
  .attr("pointer-events","none").text(l=>l.t);


const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");

const chips=d3.select("#scmkey").selectAll("button").data(KEY).join("button")
  .attr("class","chip")
  .html(d=>{
    const n=V.filter(v=>kindOf(v)===d.k).length;
    return "<span style=\\"display:inline-block;width:9px;height:9px;'
, 'margin-right:6px;background:"+COL[d.k]+"\\"></span>"+d.lab+" ("+n+")";
  })
  .on("click",(e,d)=>{filt=(filt===d.k?null:d.k); draw();});

function kindOf(v){ return v.c>0?"gained":v.c<0?"lost":"retained"; }

function show(e,c){
  const v=byS[c.s]; if(!v) return;
  hot=c.s; draw();
  const r=box.node().getBoundingClientRect();
  const arrow=v.f===v.t?"":" ("+(v.c>0?"+":"\\u2212")+Math.abs(v.c)+")";
  tip.style("opacity",1)
     .style("left",Math.min(e.clientX-r.left+14,r.width-190)+"px")
     .style("top",(e.clientY-r.top-10)+"px")
     .html("<b>"+v.n+"</b><br>"+v.f+" \\u2192 "+v.t+
       (v.t===1?" seat":" seats")+arrow+"<br>"+
       fmt(v.pop)+" people, "+fmt(v.pps)+" per seat<br>"+
       "short of one more seat by "+fmt(v.short));
}
function hide(){ hot=null; tip.style("opacity",0); draw(); }
cells.on("mousemove",show).on("mouseleave",hide);

function draw(){
  const dim=c=>filt&&kindOf(byS[c.s])!==filt&&c.s!==hot;
  cells.attr("opacity",c=>dim(c)?0.22:1);
  outs.attr("stroke-width",o=>o.s===hot?4:2)
      .attr("opacity",o=>{const v=byS[o.s];
        return filt&&kindOf(v)!==filt&&o.s!==hot?0.22:1;});
  chips.style("background",d=>filt===d.k?"#1C4C5C":null)
       .style("color",d=>filt===d.k?"#fff":null)
       .style("font-weight",d=>filt===d.k?"600":"400");
}
draw();
})();
</script>'))


## ---- pen
# THE PENNY ACTIVITY, worked in R.
#
# Nineteen people in four groups and twenty-five pennies, run through the same
# six rules the rest of this chapter runs over fifty states. Four states is
# small enough to hold the whole disagreement in your head, which fifty is not.
# Every number in the Activity section comes from this block, so the prose, the
# static figure and the interactive one cannot drift apart.
#
# THE MANDATED FIRST PENNY. The real House hands every state one seat before
# any rule runs, because the Constitution says so. This does the same, and
# `penF` is the switch: with it on, every group is seated once and the rules
# divide what is left. It is on by default, so the toy models the House rather
# than an abstraction of it.
#
# Two of the rules would impose that floor anyway. Adams, Dean and
# Huntington-Hill divide by a quantity that is zero when a state holds no
# seats, so an empty state has infinite claim on the next penny and can never
# be left out. Webster, Jefferson and Hamilton have no such floor of their own.
# Turning `penF` off is what shows why the Constitution does not leave the
# question to the rule: at four tiny groups against one large one, Jefferson's
# method hands the large one everything.
#
# The numbers are the ones the class actually uses, searched in
# F26/sessions/02-census/data/build-activity-table.R for a split on which the
# rules disagree and no seat rests on a tie-break.
penP <- c(1, 2, 7, 9)
penS <- 25
penF <- TRUE
penL <- c("A", "B", "C", "D")

# Largest remainders: whole shares first, then the largest leftovers. The
# remainders are compared over a common denominator rather than as fractions,
# so nothing here turns on a rounding of a rounding. A group held up to the
# floor is taken out of the queue for leftovers, or it would be paid twice.
pen_ham <- function(P, S, f0) {
  tot <- sum(P); prod <- P * S
  base <- prod %/% tot
  s <- pmax(base, f0)
  rem <- ifelse(base < f0, -Inf, prod %% tot)
  left <- S - sum(s)
  if (left > 0) {
    o <- order(-rem, -P)
    s[o[seq_len(left)]] <- s[o[seq_len(left)]] + 1L
  }
  as.integer(s)
}

# Highest averages, shared by the five divisor rules. `dv` is the divisor for a
# state's next seat and `f0` the number every state starts with.
pen_div <- function(P, S, dv, f0) {
  n <- length(P); extra <- S - f0 * n
  s <- rep(f0, n)
  if (extra > 0) {
    g <- expand.grid(k = f0:(f0 + extra), i = seq_len(n))
    pri <- mapply(function(i, k) P[i] / dv(k), g$i, g$k)
    for (j in order(-pri)[seq_len(extra)]) s[g$i[j]] <- s[g$i[j]] + 1L
  }
  as.integer(s)
}

# Each rule takes the mandated floor and raises it to its own minimum where it
# has one, which is what `pmax(f0, 1L)` is doing in the first three.
PEN <- list(
  Adams             = function(P, S, f0) pen_div(P, S, function(k) k,
                                                 max(f0, 1L)),
  Dean              = function(P, S, f0) pen_div(P, S, function(k)
                                             (2 * k * (k + 1)) / (2 * k + 1),
                                             max(f0, 1L)),
  `Huntington-Hill` = function(P, S, f0) pen_div(P, S, function(k)
                                             sqrt(k * (k + 1)), max(f0, 1L)),
  Webster           = function(P, S, f0) pen_div(P, S, function(k) k + 0.5, f0),
  Jefferson         = function(P, S, f0) pen_div(P, S, function(k) k + 1, f0),
  Hamilton          = function(P, S, f0) pen_ham(P, S, f0))

pen_all <- function(P, S, f0 = as.integer(penF)) {
  m <- t(vapply(PEN, function(f) f(P, S, f0), integer(length(P))))
  colnames(m) <- penL
  m
}

penM <- pen_all(penP, penS)
penQ <- penS * penP / sum(penP)
penD <- length(unique(apply(penM, 1, paste, collapse = " ")))

# The first House size at or above the activity's own penny count at which
# handing out one MORE penny costs some group one. That is the Alabama paradox
# in four states. The search starts at penS rather than at the smallest legal
# House because below it there is a duller version, where the group of one is
# holding nothing to begin with and Hamilton simply never gives it anything.
pen_para <- function(P, from, to = 60, f0 = as.integer(penF)) {
  for (S in from:(to - 1)) for (r in names(PEN)) {
    a <- PEN[[r]](P, S, f0); b <- PEN[[r]](P, S + 1, f0)
    if (any(b < a)) return(list(rule = r, size = S, before = a, after = b,
                                li = which(b < a)[1],
                                loser = penL[which(b < a)[1]]))
  }
  NULL
}
penA <- pen_para(penP, from = penS)

## ---- pen-static
penX <- rbind(penM, penA$before, penA$after)
par(mar = c(5.2, 8.2, 3.4, 1.0))
plot(NA, xlim = c(0.4, length(penP) + 0.6),
     ylim = c(-1.9, nrow(penM) + 1.3), axes = FALSE, xlab = "", ylab = "")
for (i in seq_len(nrow(penM))) {
  y <- nrow(penM) - i + 1
  for (j in seq_along(penP)) {
    d <- penM[i, j] - penM["Huntington-Hill", j]
    rect(j - 0.45, y - 0.42, j + 0.45, y + 0.42, border = "#FFFFFF", lwd = 1.2,
         col = if (d > 0) "#2c7fb8" else if (d < 0) "#C41230" else "#EDEFF1")
    text(j, y, penM[i, j], cex = 0.86, font = 2,
         col = if (d == 0) "#12181D" else "#FFFFFF")
  }
}
axis(2, at = nrow(penM):1, labels = rownames(penM), las = 1, tick = FALSE,
     cex.axis = 0.78, line = -0.6)
mtext(sprintf("%s  (%d %s)", penL, penP,
              ifelse(penP == 1, "person", "people")),
      side = 3, at = seq_along(penP), line = 1.0, cex = 0.74)
mtext(sprintf("fair share %.2f", penQ), side = 3, at = seq_along(penP),
      line = 0.15, cex = 0.62, col = "#666666")

# The paradox, underneath: one more penny, and a group loses one.
for (r in 1:2) {
  y <- -0.5 - (r - 1) * 0.85
  for (j in seq_along(penP)) {
    v <- if (r == 1) penA$before[j] else penA$after[j]
    text(j, y, v, cex = 0.82, font = if (r == 2 && penA$after[j] <
                                         penA$before[j]) 2 else 1,
         col = if (r == 2 && penA$after[j] < penA$before[j]) "#C41230"
               else "#12181D")
  }
  axis(2, at = y, las = 1, tick = FALSE, cex.axis = 0.74, line = -0.6,
       labels = sprintf("%s, %d pennies", penA$rule,
                        penA$size + (r - 1)))
}
mtext(sprintf(paste("Blue is a penny more than Huntington-Hill gives, red a",
                    "penny fewer. The six rules produce %d different splits."),
              penD), side = 1, line = 1.6, cex = 0.62, col = "#666666")
mtext(sprintf(paste("Below the rule: adding the %dth penny takes one away",
                    "from group %s, which is the Alabama paradox."),
              penA$size + 1, penA$loser),
      side = 1, line = 2.5, cex = 0.62, col = "#666666")

## ---- pen-d3
# Four states rather than fifty, so this figure does not use the chapter's
# shared DD_AP engine: that one ships the Bureau's populations and imposes the
# constitutional floor on everything. Here the floor belongs to the rule, and
# the populations are whatever the reader drags them to.
cat(sprintf('
<div id="pen" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const LAB=["A","B","C","D"], DEF=[%s], DEFS=%d, PARA=%d;
const RULES=["Adams","Dean","Huntington-Hill","Webster","Jefferson","Hamilton"];
const DIV={Adams:k=>k, Dean:k=>(2*k*(k+1))/(2*k+1),
           "Huntington-Hill":k=>Math.sqrt(k*(k+1)),
           Webster:k=>k+0.5, Jefferson:k=>k+1};
// Adams, Dean and Huntington-Hill divide by something that is zero at k=0, so
// an empty state would have infinite claim on the next penny: they start every
// group at one whatever the switch says. The other three have no floor of
// their own, and take the mandated one.
const MIN={Adams:1,Dean:1,"Huntington-Hill":1,Webster:0,Jefferson:0,Hamilton:0};
let P=DEF.slice(), S=DEFS, F=1;
const floorOf=r=>Math.max(MIN[r],F);

function divisor(rule,P,S){
  const d=DIV[rule], f=floorOf(rule), n=P.length, s=P.map(()=>f);
  let tied=false;
  for(let i=f*n;i<S;i++){
    let b=-1,w=-1,sec=-1;
    for(let j=0;j<n;j++){ const p=P[j]/d(s[j]);
      if(p>b){sec=b;b=p;w=j;} else if(p>sec){sec=p;} }
    if(w<0) break;
    // Only a tie on the LAST penny can change the answer. A tie earlier is
    // settled by the next round, which hands the other state the next one.
    if(i===S-1 && sec>0 && Math.abs(b-sec)<=1e-9*b) tied=true;
    s[w]++;
  }
  return {s:s,tied:tied};
}
function hamilton(P,S){
  const T=P.reduce((a,b)=>a+b,0), base=P.map(p=>Math.floor(p*S/T));
  // A group held up to the mandated floor leaves the queue for leftovers,
  // or it would be paid twice for the same shortfall.
  const s=base.map(v=>Math.max(v,F));
  const rem=P.map((p,i)=>base[i]<F?-Infinity:p*S-T*base[i]);
  let left=S-s.reduce((a,b)=>a+b,0), tied=false;
  const ord=P.map((p,i)=>i).sort((a,b)=>rem[b]-rem[a]||P[b]-P[a]);
  if(left>0 && left<P.length) tied=rem[ord[left-1]]===rem[ord[left]];
  for(let i=0;i<left;i++) s[ord[i]]++;
  return {s:s,tied:tied};
}
const alloc=(r,p,n)=>r==="Hamilton"?hamilton(p,n):divisor(r,p,n);

const box=d3.select("#pen");
const ctl=box.append("div").attr("style",
  "display:grid;grid-template-columns:repeat(auto-fit,minmax(11em,1fr));"
 +"gap:4px 14px;margin:0 0 10px");
const pins=[];
LAB.forEach(function(L,j){
  const row=ctl.append("div").attr("style",
    "display:flex;align-items:center;gap:7px;font:0.86em inherit");
  const lab=row.append("span").attr("style",
    "min-width:6.6em;color:var(--ink-2)");
  const inp=row.append("input").attr("type","range").attr("min",1).attr("max",12)
    .attr("step",1).attr("value",DEF[j])
    .attr("style","flex:1;min-width:4.5em;accent-color:#2c7fb8")
    .on("input",function(){ P[j]=+this.value; draw(); });
  pins.push({lab:lab,inp:inp});
});
const srow=box.append("div").attr("style",
  "display:flex;align-items:center;gap:10px;margin:0 0 10px;flex-wrap:wrap");
const slab=srow.append("span").attr("style",
  "font:0.86em inherit;min-width:6.6em;color:var(--ink-2)");
const sinp=srow.append("input").attr("type","range").attr("min",8).attr("max",40)
  .attr("step",1).attr("value",DEFS)
  .attr("style","flex:1;min-width:9em;accent-color:#2c7fb8")
  .on("input",function(){ S=+this.value; draw(); });
const BTN="padding:3px 9px;border:1px solid var(--rule);border-radius:3px;"
 +"cursor:pointer;font:11.5px inherit;background:var(--card);color:var(--ink)";
srow.append("button").attr("style",BTN).text("the numbers from class")
  .on("click",function(){ P=DEF.slice(); S=DEFS; sync(); draw(); });
srow.append("button").attr("style",BTN).text("set up the paradox")
  .on("click",function(){ P=DEF.slice(); S=PARA; sync(); draw(); });
const frow=box.append("label").attr("style",
  "display:flex;align-items:center;gap:7px;margin:0 0 10px;font:0.86em inherit;"
 +"color:var(--ink-2);cursor:pointer");
frow.append("input").attr("type","checkbox").property("checked",true)
  .attr("style","accent-color:#2c7fb8")
  .on("change",function(){ F=this.checked?1:0; draw(); });
frow.append("span").text("give every group one penny first, as the Constitution "
 +"does for the states");
function sync(){
  pins.forEach(function(o,j){ o.inp.property("value",P[j]); });
  sinp.property("value",S);
}
const head=box.append("p").attr("style",
  "font:0.92em inherit;margin:0 0 0.6em;color:var(--ink)");
const grid=box.append("div");
const note=box.append("p").attr("style",
  "font:0.88em inherit;margin:0.6em 0 0;color:var(--ink-2)");

function draw(){
  const T=P.reduce((a,b)=>a+b,0);
  const q=P.map(p=>S*p/T);
  const res={}; RULES.forEach(r=>{ res[r]=alloc(r,P,S); });
  // Measured against the method actually in force, as Figure 4 is.
  const ref=res["Huntington-Hill"].s;
  const houses=new Set(RULES.map(r=>res[r].s.join(" ")));
  LAB.forEach(function(L,j){
    pins[j].lab.text("group "+L+": "+P[j]+(P[j]===1?" person":" people"));
  });
  slab.text(S+" pennies");

  let cells="<div style=\\"font:0.78em inherit;color:var(--ink-3);"
   +"text-align:right;padding:0 8px 4px 0\\">rule</div>";
  LAB.forEach(function(L,j){
    cells+="<div style=\\"text-align:center;padding:0 0 4px\\">"
      +"<div style=\\"font:0.95em inherit;font-weight:600;color:var(--ink)\\">"
      +L+"</div><div style=\\"font:0.76em inherit;color:var(--ink-3)\\">"
      +"fair share "+q[j].toFixed(2)+"</div></div>";
  });
  RULES.forEach(function(r){
    cells+="<div style=\\"font:0.86em inherit;color:var(--ink-2);"
      +"text-align:right;padding:3px 8px 3px 0;border-top:1px solid var(--rule)\\">"
      +r+(res[r].tied?" *":"")+"</div>";
    res[r].s.forEach(function(v,j){
      const d=v-ref[j];
      const col=d>0?"var(--map-dem)":(d<0?"var(--map-gop)":"var(--ink)");
      cells+="<div style=\\"text-align:center;padding:3px 0;"
        +"border-top:1px solid var(--rule);font:0.98em inherit;"
        +"font-weight:"+(d?"700":"400")+";color:"+col+"\\">"+v
        +(d?"<span style=\\"font-size:0.72em\\"> "+(d>0?"+":"")+d+"</span>":"")
        +"</div>";
    });
  });
  grid.attr("style","display:grid;grid-template-columns:"
    +"minmax(7.5em,auto) repeat("+LAB.length+",1fr);align-items:center")
    .html(cells);

  head.html("<b>"+S+" pennies, "+T+" people, six rules, "
    +(houses.size===1?"one answer":houses.size+" different answers")+".</b> "
    +(houses.size===1
      ? "Every rule splits them the same way here. Move a slider one step."
      : "Bold numbers differ from Huntington&#8211;Hill, the method the United "
       +"States has used since 1941: blue is a penny more, red a penny fewer."));

  // One more penny: does anyone LOSE one? Only Hamilton can do this.
  const lines=[];
  RULES.forEach(function(r){
    const b=alloc(r,P,S+1).s;
    const lost=[]; b.forEach(function(v,j){ if(v<res[r].s[j]) lost.push(LAB[j]); });
    if(lost.length) lines.push("Under "+r+", a "+(S+1)+"th penny takes one away "
      +"from group "+lost.join(" and ")+".");
  });
  if(RULES.some(r=>res[r].tied))
    lines.push("A star marks a rule whose last penny is a tie between two "
      +"groups, so its answer depends on a coin flip.");
  note.html(lines.length?lines.join(" ")
    :"Adding one more penny takes nothing away from anyone at this setting.");
}
sync(); draw();

// This figure draws HTML rather than an <svg>, so the fallback sweep in
// brief-head.html cannot tell that it ran and would leave the static twin on
// the page beside it. Hide the twin here instead. With scripts stripped -- in
// a mail client -- none of this runs and the twin is what the reader gets.
for (let el=document.getElementById("pen").previousElementSibling, i=0;
     el && i<4; el=el.previousElementSibling, i++) {
  if (el.classList && el.classList.contains("dd-fallback")) {
    el.style.display="none"; break;
  }
}
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.4em">
<b>Drag a group, then drag the pennies.</b> All six rules are re-run in the
browser on every step; nothing is precomputed. The mandated first penny is on
by default, which is what the Constitution does for the states, and at the
opening numbers it changes nothing: every group clears one penny on its own.
Turn it off and set three groups to 1 against a fourth at 9, with 8 pennies, to
see what it is for.</p>
', paste(penP, collapse = ","), penS, penA$size))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
