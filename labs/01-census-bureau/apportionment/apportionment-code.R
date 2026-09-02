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

# The first three seats of the queue, followed by hand in the brief. The prose
# names California, Texas and Florida, so the file had better agree.
stopifnot(pv$state[1:4] == c("California", "Texas", "California", "Florida"))
ca <- pop[["California"]]; tx <- pop[["Texas"]]; fl <- pop[["Florida"]]
d2 <- sqrt(2 * 1); d3 <- sqrt(3 * 2)      # the divisors for a 2nd and a 3rd seat

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

# the seats the 2020 count moved, from Table 1's own comparison column
scs   <- read.csv("data/derived/seat_change_states.csv", stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("apportionment_2020.csv", "priority_values.csv", "seat_change_labels.csv", "seat_change_rings.csv", "seat_change_states.csv", "state_rings.csv"))
SGAIN <- sum(scs$change[scs$change > 0])

# the ten seats the Bureau tabulated past the statute
pv436 <- pv[pv$house_seat > 435, ]

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
# where it stops mattering -- which is where New York is. The caption under the
# figure in the brief carries the explanation; this device draws only the data.
qpv  <- pv$priority
qsq  <- pv$house_seat
STAT <- sum(ap$seats)
par(mar = c(3.8, 6.4, 1.8, 1.6))
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

## ---- pq-d3
# The engine. The figure below replays the same fifty populations through the
# same rule in the browser, so the reader can watch every bid recomputed as the
# queue empties. d3 is loaded once, here, because this is the first figure that
# needs it.
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
<b>Drag the slider</b> to replay the queue: the readout beneath shows the
winning state\'s arithmetic and the four highest bids at every step.</p>
', pqrows, win_pri, sum(ap$seats)))

## ---- reproduce
data.frame(
  check = c("Seats handed out one at a time",
            "Total when the queue empties",
            "Does this reproduce the official House, state for state?",
            "Do the recomputed prices match the Bureau's published ones?"),
  result = c(n(sum(ap$seats) - nrow(ap)), n(sum(base)),
             ifelse(ok435, "TRUE", "FALSE"), ifelse(pv_ok, "TRUE", "FALSE")))

## ---- seatmoves
o <- scs[scs$change != 0, c("name", "seats_2010", "seats_2020", "change")]
o <- o[order(-o$change, o$name), ]
o$change <- sprintf("%+d", o$change)
names(o) <- c("state", "seats after 2010", "seats after 2020", "change")
o

## ---- howclose
data.frame(
  quantity = c("What New York was counted at",
               "What New York needed to take that seat",
               "Short by"),
  people = c(n(ny$app_pop), n(need_ny), n(margin)))

## ---- past435
o <- head(pv436[, c("house_seat", "state", "state_seat", "priority")], 5)
o$priority <- n(round(o$priority))
o$state_seat <- paste("its", ordn(o$state_seat))
names(o) <- c("if the House had this many seats", "the next one would go to",
              "seat number", "at a price of")
o

## ---- ladder-static
d <- head(ladder, 8); d <- d[order(-d$short), ]
par(mar = c(3.6, 8.5, 1.0, 5.2))
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
// The dots must land even when no frame ever ticks (a hidden tab, or a
// reader who has asked for reduced motion), so the end state is written
// directly whenever the transition cannot be trusted to run.
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
', rows))

## ---- overseas
data.frame(
  quantity = c("New York's population counted from overseas",
               "Minnesota's population counted from overseas",
               "The margin that decided the 435th seat"),
  people = c(n(ny$overseas), n(mn$overseas), n(margin)))


## ---- pen
# THE PENNY ACTIVITY, worked in R.
#
# Nineteen people in four groups and twenty-five pennies, run through the six
# rules Congress has used or considered, over four groups instead of fifty
# states. Four is small enough to hold the whole disagreement in your head,
# which fifty is not. Every number in the Activity section comes from this
# block, so the prose, the static figure and the interactive one cannot drift
# apart.
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
# The four group sizes and the penny count were searched for rather than
# picked: a split on which the rules disagree, and on which no penny rests on a
# tie-break. Set all four groups equal and the exercise dies, which is the
# reader's fourth thing to try.
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
', paste(penP, collapse = ","), penS, penA$size))

# The rest carries no substitutions, so it is a plain string rather than a
# format: sprintf() caps a format at 8192 characters and this figure is longer
# than that on its own.
cat('
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
srow.append("button").attr("style",BTN).text("back to the opening numbers")
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
  // Measured against the method actually in force, as the printed twin is.
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
// brief-head.html cannot tell that it ran: that sweep hides a twin only when
// a real <svg> appears somewhere in the same section, and no <svg> ever will
// here. Hide the twin here instead, on the same rule the sweep uses -- the
// enclosing section, not a count of siblings, because the twin sits above the
// prose rather than next to the figure. With scripts stripped -- in a mail
// client -- none of this runs and the twin is what the reader gets.
let sec=document.getElementById("pen");
while(sec && sec!==document.body && !(sec.classList &&
      sec.classList.contains("section"))) sec=sec.parentNode;
if(sec) sec.querySelectorAll(".dd-fallback")
          .forEach(el=>{ el.style.display="none"; });
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.4em">
<b>Drag a group, then drag the pennies.</b> All six rules are re-run in the
browser on every step; nothing is precomputed. The mandated first penny is on
by default, which is what the Constitution does for the states, and at the
opening numbers it changes nothing: every group clears one penny on its own.
Turn it off and set three groups to 1 against a fourth at 9, with 8 pennies, to
see what it is for.</p>
')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
