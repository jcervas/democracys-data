# crossover-code.R -- chunk bodies for crossover-brief.Rmd
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

cty <- read.csv("data/derived/counties.csv", stringsAsFactors = FALSE,
                colClasses = c(county_fips = "character"))
sen <- read.csv("data/derived/senate.csv",  stringsAsFactors = FALSE)
trd <- read.csv("data/derived/split_trend.csv", stringsAsFactors = FALSE)
fx  <- read.csv("data/derived/facts.csv",   stringsAsFactors = FALSE)

f  <- function(k) fx$value[fx$key == k]
fn <- function(k) as.numeric(f(k))
n  <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
p3 <- function(x) formatC(as.numeric(x), format = "f", digits = 3)
p4 <- function(x) formatC(as.numeric(x), format = "f", digits = 4)
sg <- function(x) sprintf("%+.1f", as.numeric(x))

# The two party colours are the corpus map palette. In HTML the figures use the
# .wind-fig classes from brief.css, which swap these for lifted versions on the
# dark page; the base-R fallbacks below cannot, and use the light values.
RED <- "#C41230"; BLU <- "#2C7FB8"; GRY <- "#8A8F94"
RULE <- "#CBD3D8"; MUTE <- "#76838C"

NCTY  <- fn("n_counties")
RCTY  <- fn("r_county")
RSHIFT <- fn("r_shifted")
RESID <- fn("resid_sd")
FLIPS <- fn("flips")
NCON  <- fn("n_contests")
NSST  <- fn("n_senate_states")
CROSS <- fn("n_crossover")
CROSSS <- fn("n_crossover_strict")
HCROSS <- fn("h_crossover")
NHOUSE <- fn("n_house")

# Two cases walked through by hand in the prose, picked by rule rather than by
# eye: the Senate crossover whose winner ran furthest from the presidential
# ticket, and the House crossover whose winner did the same. Every figure
# quoted about them is read off their own rows.
hse <- read.csv("data/derived/house.csv", stringsAsFactors = FALSE)
xs  <- sen[sen$crossover, ]
xs  <- xs[which.max(abs(xs$ran_ahead)), ]
hx  <- hse[hse$crossover, ]
hx  <- hx[which.max(abs(hx$ran_ahead)), ]
hx_pres <- ifelse(hx$pres_winner == "D", "Harris", "Trump")

# Figure 2 is drawn in order of the presidential vote, which is what the label
# placement below alternates along.
sen$key <- ifelse(sen$pres_winner == "R", 0, 1)
sen <- sen[order(sen$key, ifelse(sen$pres_winner == "R",
                                 -sen$pres_r_two, sen$pres_r_two),
                 sen$term), ]
sen$last <- sub("^.* ", "", sub(",.*$", "", sen$winner))
sen$lab  <- paste0(sen$abbrev, ifelse(sen$term == "unexpired", "*", ""),
                   "  ", sen$last)
# Figure 2 is a scatter, so the label beside a point has to be short. The
# asterisk marks the two contests for the last weeks of a term.
sen$lab2 <- paste0(sen$abbrev, ifelse(sen$term == "unexpired", "*", ""))

# WHICH POINTS CARRY A NAME. Half of these contests sit inside a few points of
# the middle of both axes, and thirty-five labels there would be a smudge. The
# rule is stated rather than chosen by eye: a contest is named if it is one the
# chapter argues about, or one of the six furthest from the diagonal.
NLAB <- 6
sen$show_lab <- sen$crossover_strict |
  sen$margin == min(sen$margin) |
  rank(-abs(sen$ran_ahead), ties.method = "first") <= NLAB

# The crowd is in the middle, so Figure 2 carries a second panel that magnifies
# it. The window is stated as a rule rather than picked to look tidy: every
# contest within six points of an even split on BOTH axes. That is the box the
# four crossovers live in, and every point inside it is named.
WIN <- 6
sen$in_win <- abs(sen$pres_r_two - 50) <= WIN & abs(sen$sen_r_top2 - 50) <= WIN

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
z <- cty[cty$county_fips %in% c("42003", "04013", "48427"), ]
data.frame(
  County = paste0(z$county_name, ", ", z$state_name),
  Republican_share_2020 = paste0(p1(z$r20), "%"),
  Republican_share_2024 = paste0(p1(z$r24), "%"),
  Move = sg(z$swing))

## ---- varmap
data.frame(
  Column = c("county_fips", "r20", "r24", "swing", "flipped"),
  What_it_holds = c(
    "the five-digit federal code for the county",
    "the Republican share of the votes cast for the two major parties in 2020",
    "the same quantity in 2024",
    "the second minus the first, in points",
    "whether the county backed a different party in the two elections"),
  Measurement = c("categorical", "continuous", "continuous", "continuous",
                  "yes or no"))

## ---- fig1-static
op <- par(mar = c(4.0, 4.4, 2.8, 1.2), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
axis(1, at = seq(0, 100, 25), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
axis(2, at = seq(0, 100, 25), las = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
mtext("Republican share of the two-party vote, 2020", 1, line = 2.4, cex = 0.86)
mtext("the same share in 2024", 2, line = 2.9, cex = 0.86)
# The two off-diagonal boxes, drawn here and in the figure that follows. A
# county that backed different parties in the two elections can only land in
# one of them, and only one of the two boxes turns out to be occupied.
rect(0, 50, 50, 100, col = "#F2F4F5", border = NA)
rect(50, 0, 100, 50, col = "#F2F4F5", border = NA)
abline(h = 50, v = 50, col = RULE, lwd = 0.8)
abline(0, 1, col = MUTE, lty = 2, lwd = 1)
points(cty$r20, cty$r24, pch = 19, cex = 0.28,
       col = adjustcolor(ifelse(cty$r24 > 50, RED, BLU), 0.32))
fl <- cty[cty$flipped, ]
points(fl$r20, fl$r24, pch = 1, cex = 0.62, col = "#12181D", lwd = 0.6)
text(2, 98, "Republican in 2024,\nDemocratic in 2020", cex = 0.62, col = MUTE,
     adj = c(0, 1))
text(98, 4, "Democratic in 2024,\nRepublican in 2020", cex = 0.62, col = MUTE,
     adj = c(1, 0))
mtext("Every county, twice: how it voted in 2020, and how it voted in 2024",
      3, line = 1.4, cex = 0.76, adj = 0)
mtext("ringed: the county backed a different party in the two elections",
      3, line = 0.5, cex = 0.66, adj = 0, col = MUTE)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The county scatter. Two things it has to do that the static twin cannot:
# name a dot, and move the whole cloud ten points to show what that does to the
# correlation. The second is a demonstration, not the argument -- the number it
# produces is computed in build-data.R and printed in the prose either way.
#
# This chunk carries the ONE d3 <script src> for the document; the dd_fig()
# trend figure below rides on it with d3 = FALSE.
# ---------------------------------------------------------------------------
d <- cty[order(cty$total_votes_24), ]
PT <- paste(sprintf('[%.1f,%.1f,%d,"%s","%s"]',
                    d$r20, d$r24, d$total_votes_24,
                    gsub('"', "", d$county_name), d$state_name),
            collapse = ",")
cat(sprintf('
<style>
/* The quadrant patches in Figures 1 and 2. Painted at full opacity in a
   colour that already IS the composite, rather than as ink at 5.5%% over the
   page. The two render identically; only this one tells the truth about what
   is on the pixel, which matters because a contrast checker reads `fill` and
   cannot see through fill-opacity. */
#sc .quad, #sen .quad { fill: #E3E5E6; }
@media (prefers-color-scheme: dark) {
  #sc .quad, #sen .quad { fill: #1C2024; }
}
</style>
<div id="sc" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const P=[%s], RNOW=%.4f, RUP=%.4f;
const W=760,H=470,M={t:16,r:16,b:46,l:52};
const box=d3.select("#sc");
const bar=box.append("div")
  .attr("style","margin:0 0 8px;display:flex;align-items:center;gap:10px;font:12px inherit;flex-wrap:wrap");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,110]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
   .call(d3.axisBottom(x).ticks(5));
svg.append("g").attr("transform","translate("+M.l+",0)")
   .call(d3.axisLeft(y).ticks(6));
[[0,50,50,100],[50,0,100,50]].forEach(function(q){
  svg.append("rect").attr("x",x(q[0])).attr("y",y(q[3]))
     .attr("width",x(q[2])-x(q[0])).attr("height",y(q[1])-y(q[3]))
     .attr("class","quad").attr("stroke","none");});
svg.append("line").attr("x1",x(0)).attr("x2",x(100))
   .attr("y1",y(50)).attr("y2",y(50)).attr("class","rule");
svg.append("line").attr("x1",x(50)).attr("x2",x(50))
   .attr("y1",y(0)).attr("y2",y(110)).attr("class","rule");
svg.append("line").attr("x1",x(0)).attr("x2",x(100))
   .attr("y1",y(0)).attr("y2",y(100)).attr("stroke","#76838C")
   .attr("stroke-dasharray","4 3");
svg.append("text").attr("transform","rotate(-90)")
   .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
   .attr("font-size","12px").attr("class","lbl")
   .text("the same share in 2024");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8)
   .attr("text-anchor","middle").attr("font-size","12px").attr("class","lbl")
   .text("Republican share of the two-party vote, 2020");
const dots=svg.append("g").selectAll("circle").data(P).join("circle")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",2)
  .attr("class",d=>d[1]>50?"gop-fill":"dem-fill").attr("fill-opacity",0.34);
/* The counties that changed sides, ringed as in the figure that follows.
   They move with the cloud when the button is pressed, and what happens then is
   worth watching: a uniform shift pours hundreds more counties into the box
   that is already occupied and leaves the empty one empty. The boxes measure a
   country moving one way, not places trading with each other. */
const rings=svg.append("g").selectAll("circle")
  .data(P.filter(d=>(d[0]>50)!==(d[1]>50))).join("circle")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",4)
  .attr("fill","none").attr("class","ttl-stroke").attr("stroke-width",0.9);
svg.append("text").attr("x",x(0)+8).attr("y",y(100)+14).attr("font-size","11px")
  .attr("class","foot").text("Republican in 2024, Democratic in 2020");
svg.append("text").attr("x",x(100)-4).attr("y",y(0)-8).attr("text-anchor","end")
  .attr("font-size","11px").attr("class","foot")
  .text("Democratic in 2024, Republican in 2020");
const rd=svg.append("text").attr("x",M.l+10).attr("y",M.t+16)
  .attr("font-size","13px").attr("font-weight","600").attr("class","ttl");
const nm=svg.append("text").attr("x",M.l+10).attr("y",M.t+34)
  .attr("font-size","12px").attr("class","sub");
let up=false;
function draw(){
  dots.transition().duration(600)
      .attr("cy",d=>y(Math.min(d[1]+(up?10:0),110)));
  rings.transition().duration(600)
      .attr("cy",d=>y(Math.min(d[1]+(up?10:0),110)));
  rd.text("correlation with 2020: "+(up?RUP:RNOW).toFixed(4));
}
bar.append("button")
  .attr("style","padding:3px 9px;border:1px solid #CBD3D8;border-radius:3px;cursor:pointer;font:11.5px inherit;background:transparent;color:inherit")
  .text("move every county ten points toward the Republicans")
  .on("click",function(){
    up=!up;
    d3.select(this).text(up?"put them back where they voted"
                           :"move every county ten points toward the Republicans");
    draw();});
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e){
    const p=d3.pointer(e,this), px=p[0]+M.l, py=p[1]+M.t;
    let best=null,bd=1e9;
    P.forEach(function(q){
      const dx=x(q[0])-px, dy=y(q[1]+(up?10:0))-py, dd=dx*dx+dy*dy;
      if(dd<bd){bd=dd;best=q;}});
    if(best) nm.text(best[3]+", "+best[4]+"  \\u00b7  "
      +best[0].toFixed(1)+"%% then "+best[1].toFixed(1)+"%% now");
  })
  .on("mouseleave",function(){nm.text("");});
draw();
})();
</script>', PT, RCTY, RSHIFT))

## ---- fig2-static
layout(matrix(c(1, 2), 1, 2), widths = c(1.9, 1))
col <- ifelse(sen$winner_side == "R", RED,
       ifelse(sen$winner_side == "D", BLU, GRY))

panel <- function(lo, hi, which_lab, tick, cexlab) {
  plot(NA, xlim = c(lo, hi), ylim = c(lo, hi), axes = FALSE, xlab = "",
       ylab = "")
  axis(1, at = seq(lo, hi, tick), cex.axis = 0.72, lwd = 0, lwd.ticks = 1)
  axis(2, at = seq(lo, hi, tick), las = 1, cex.axis = 0.72, lwd = 0,
       lwd.ticks = 1)
  # The two off-diagonal boxes: a state that voted one way for president and
  # the other for the Senate can only land in one of them.
  rect(lo, 50, 50, hi, col = "#F2F4F5", border = NA)
  rect(50, lo, hi, 50, col = "#F2F4F5", border = NA)
  abline(h = 50, v = 50, col = RULE, lwd = 0.8)
  abline(0, 1, col = MUTE, lty = 2, lwd = 1)
  # The vertical gap between a point and the dashed line is the quantity the
  # claim is about, so it is drawn for every contest, named or not.
  segments(sen$pres_r_two, sen$pres_r_two, sen$pres_r_two, sen$sen_r_top2,
           col = RULE, lwd = 1)
  points(sen$pres_r_two, sen$sen_r_top2, pch = 19, cex = 1.05, col = col)
  # LABEL PLACEMENT. Each label takes the first of the four positions round its
  # point that collides with nothing already placed, and the points nearest the
  # crosshair go first because they are the crowded ones.
  lb <- which(which_lab & sen$pres_r_two > lo & sen$pres_r_two < hi &
                sen$sen_r_top2 > lo & sen$sen_r_top2 < hi)
  # Eight directions round the point, tried in order, then pushed outward a step
  # at a time until nothing is in the way. A label that ends up far from its
  # point gets a hairline back to it, because a name that has drifted off its
  # dot is worse than no name.
  pad <- (hi - lo) / 130
  DIRS <- list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0),
               c(0.8, 0.8), c(0.8, -0.8), c(-0.8, 0.8), c(-0.8, -0.8))
  box_at <- function(i, d, k) {
    w <- strwidth(sen$lab2[i], cex = cexlab)
    h <- strheight(sen$lab2[i], cex = cexlab)
    cx <- sen$pres_r_two[i] + d[1] * (w / 2 + pad * k)
    cy <- sen$sen_r_top2[i] + d[2] * (h / 2 + pad * k)
    c(cx - w / 2, cy - h / 2, cx + w / 2, cy + h / 2)
  }
  hits <- function(a, b)
    !(a[3] < b[1] || b[3] < a[1] || a[4] < b[2] || b[4] < a[2])
  rad <- strheight("0", cex = cexlab)
  taken <- lapply(seq_len(nrow(sen)), function(i)
    c(sen$pres_r_two[i] - rad, sen$sen_r_top2[i] - rad,
      sen$pres_r_two[i] + rad, sen$sen_r_top2[i] + rad))
  for (i in lb[order(abs(sen$pres_r_two[lb] - 50) +
                       abs(sen$sen_r_top2[lb] - 50))]) {
    best <- NULL
    for (k in 1:6) {
      for (d in DIRS) {
        b <- box_at(i, d, k)
        if (!any(vapply(taken, hits, logical(1), b = b))) { best <- b; break }
      }
      if (!is.null(best)) break
    }
    if (is.null(best)) best <- box_at(i, DIRS[[1]], 6)
    taken[[length(taken) + 1]] <- best
    cx <- mean(best[c(1, 3)]); cy <- mean(best[c(2, 4)])
    if (k > 2)
      segments(sen$pres_r_two[i], sen$sen_r_top2[i], cx, cy, col = RULE,
               lwd = 0.7)
    text(cx, cy, sen$lab2[i], cex = cexlab, col = "#3A444C", font = 2)
  }
}

op <- par(mar = c(4.0, 3.6, 3.4, 0.8), mgp = c(2.4, 0.6, 0))
LO <- 30; HI <- 80
panel(LO, HI, sen$show_lab & !sen$in_win, 10, 0.6)
rect(50 - WIN, 50 - WIN, 50 + WIN, 50 + WIN, border = MUTE, lwd = 1)
mtext("Republican share of the presidential vote in the state", 1, line = 2.3,
      cex = 0.76)
mtext("the same share in the Senate contest", 2, line = 2.4, cex = 0.76)
text(LO + 0.6, HI - 0.6, "Republican senator,\nHarris state", cex = 0.6,
     col = MUTE, adj = c(0, 1))
text(HI - 0.6, LO + 2.6, "Democratic senator,\nTrump state", cex = 0.6,
     col = MUTE, adj = c(1, 0))
mtext("Every Senate contest, against the presidential vote beside it",
      3, line = 1.5, cex = 0.76, adj = 0)
mtext("the dashed line is running level with the ticket",
      3, line = 0.6, cex = 0.66, adj = 0, col = MUTE)

par(mar = c(4.0, 2.6, 3.4, 1.4))
panel(50 - WIN, 50 + WIN, rep(TRUE, nrow(sen)), 3, 0.66)
box(col = MUTE, lwd = 1)
mtext("the box, magnified", 3, line = 1.5, cex = 0.76, adj = 0)
mtext("within six points of even, both ways", 3, line = 0.6, cex = 0.66,
      adj = 0, col = MUTE)
par(op)
layout(1)

## ---- fig2-d3
# The two offices on one pair of axes, twice: the whole field, and the middle of
# it magnified. Each point is a contest -- how Republican the state voted for
# president, against how Republican it voted for the Senate. The dashed diagonal
# is a Senate candidate running level with their ticket, so the vertical drop
# from it is the thing the textbook's claim is about. Same panels, same window
# and same labelling rule as the base-R twin.
SN <- paste(sprintf('[%.2f,%.2f,"%s","%s","%s","%s",%.2f,"%s","%s",%d,%d,%s,%s,%s]',
                    sen$pres_r_two, sen$sen_r_top2, gsub('"', "", sen$lab2),
                    sen$winner_side, gsub('"', "", sen$winner),
                    gsub('"', "", sen$runner), sen$ran_ahead,
                    gsub('"', "", sen$rep_name), sen$state,
                    sen$winner_votes, sen$runner_votes,
                    tolower(sen$crossover_strict), tolower(sen$show_lab),
                    tolower(sen$in_win)), collapse = ",")
cat(sprintf('
<div id="sen" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[%s], WIN=%d;
const W=780,H=430;
const box=d3.select("#sen");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const tip=box.append("div").attr("class","windtip")
  .attr("style","position:absolute;pointer-events:none;opacity:0;background:var(--card,#fff);border:1px solid #CBD3D8;border-radius:3px;padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
function tipHTML(d){
  const dir = d[6]>=0 ? "ahead of" : "behind";
  return "<b>"+d[8]+"</b><br>"+d[4]+" beat "+d[5]+"<br>"
    +d[9].toLocaleString()+" to "+d[10].toLocaleString()+"<br>"
    +d[7]+" ran "+Math.abs(d[6]).toFixed(1)+" points "+dir+" Trump"
    +(d[11]?"<br><b>the state voted the other way for president</b>":"");
}
/* Labels: eight directions round the point, then pushed outward a step at a
   time until nothing is in the way, with a hairline back when it has drifted.
   The same algorithm the base-R twin uses, so the two agree. */
const DIRS=[[0,-1],[0,1],[1,0],[-1,0],[.8,-.8],[.8,.8],[-.8,-.8],[-.8,.8]];
function place(g,pts,x,y,taken){
  const CH=6.5;
  pts.forEach(function(p){
    const w=p.t.length*6.4, h=11;
    let best=null,k=1;
    for(k=1;k<=6&&!best;k++)
      for(const d of DIRS){
        const cx=x(p.x)+d[0]*(w/2+3*k), cy=y(p.y)+d[1]*(h/2+3*k);
        const b=[cx-w/2,cy-h/2,cx+w/2,cy+h/2];
        if(!taken.some(q=>!(b[2]<q[0]||q[2]<b[0]||b[3]<q[1]||q[3]<b[1]))){
          best=b; break;
        }
      }
    if(!best){const cx=x(p.x),cy=y(p.y)-14;best=[cx-w/2,cy-h/2,cx+w/2,cy+h/2];}
    taken.push(best);
    const cx=(best[0]+best[2])/2, cy=(best[1]+best[3])/2;
    if(k>3) g.append("line").attr("x1",x(p.x)).attr("y1",y(p.y))
              .attr("x2",cx).attr("y2",cy).attr("class","rule");
    g.append("text").attr("x",cx).attr("y",cy+CH/2-1).attr("text-anchor","middle")
     .attr("font-size","10.5px").attr("font-weight","600").attr("class","lbl")
     .text(p.t);
  });
}
function panel(gx,gy,pw,ph,lo,hi,ticks,wants,title,sub){
  const g=svg.append("g").attr("transform","translate("+gx+","+gy+")");
  const M={t:34,r:10,b:34,l:34};
  const x=d3.scaleLinear().domain([lo,hi]).range([M.l,pw-M.r]);
  const y=d3.scaleLinear().domain([lo,hi]).range([ph-M.b,M.t]);
  g.append("text").attr("x",0).attr("y",12).attr("font-size","12.5px")
   .attr("font-weight","600").attr("class","ttl").text(title);
  g.append("text").attr("x",0).attr("y",26).attr("font-size","11px")
   .attr("class","sub").text(sub);
  [[lo,50,50,hi],[50,lo,hi,50]].forEach(function(q){
    g.append("rect").attr("x",x(q[0])).attr("y",y(q[3]))
     .attr("width",x(q[2])-x(q[0])).attr("height",y(q[1])-y(q[3]))
     .attr("class","quad").attr("stroke","none");});
  g.append("g").attr("transform","translate(0,"+(ph-M.b)+")")
   .call(d3.axisBottom(x).ticks(ticks));
  g.append("g").attr("transform","translate("+M.l+",0)")
   .call(d3.axisLeft(y).ticks(ticks));
  g.append("line").attr("x1",x(lo)).attr("x2",x(hi)).attr("y1",y(50))
   .attr("y2",y(50)).attr("class","rule");
  g.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",y(lo))
   .attr("y2",y(hi)).attr("class","rule");
  g.append("line").attr("x1",x(lo)).attr("x2",x(hi)).attr("y1",y(lo))
   .attr("y2",y(hi)).attr("stroke","#76838C").attr("stroke-dasharray","4 3");
  const inside=S.filter(d=>d[0]>lo&&d[0]<hi&&d[1]>lo&&d[1]<hi);
  /* The vertical gap to the diagonal, drawn for every contest whether or not
     it is named. */
  g.append("g").selectAll("line").data(inside).join("line")
   .attr("x1",d=>x(d[0])).attr("x2",d=>x(d[0]))
   .attr("y1",d=>y(d[0])).attr("y2",d=>y(d[1]))
   .attr("class","rule").attr("stroke-width",1);
  const pt=g.append("g").selectAll("circle").data(inside).join("circle")
   .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",4.6)
   .attr("class",d=>d[3]==="R"?"gop-fill":(d[3]==="D"?"dem-fill":null))
   .attr("fill",d=>d[3]==="I"?"#8A8F94":null);
  pt.on("mousemove",function(e,d){
      const r=box.node().getBoundingClientRect();
      tip.style("opacity",1).style("left",(e.clientX-r.left+14)+"px")
         .style("top",(e.clientY-r.top-8)+"px").html(tipHTML(d));})
    .on("mouseleave",function(){tip.style("opacity",0);});
  const taken=inside.map(d=>[x(d[0])-6,y(d[1])-6,x(d[0])+6,y(d[1])+6]);
  const named=inside.filter(wants)
    .sort((a,b)=>(Math.abs(a[0]-50)+Math.abs(a[1]-50))
                -(Math.abs(b[0]-50)+Math.abs(b[1]-50)))
    .map(d=>({x:d[0],y:d[1],t:d[2]}));
  place(g,named,x,y,taken);
  return {g:g,x:x,y:y,M:M,pw:pw,ph:ph};
}
const A=panel(0,0,500,H,30,80,5,d=>d[12]&&!d[13],
  "Every Senate contest, against the presidential vote beside it",
  "the dashed line is running level with the ticket");
A.g.append("rect").attr("x",A.x(50-WIN)).attr("y",A.y(50+WIN))
  .attr("width",A.x(50+WIN)-A.x(50-WIN))
  .attr("height",A.y(50-WIN)-A.y(50+WIN))
  .attr("fill","none").attr("stroke","#76838C");
A.g.append("text").attr("x",A.x(30)+6).attr("y",A.y(80)+14)
  .attr("font-size","10.5px").attr("class","foot")
  .text("Republican senator, Harris state");
A.g.append("text").attr("x",A.x(80)-6).attr("y",A.y(30)-8)
  .attr("text-anchor","end").attr("font-size","10.5px").attr("class","foot")
  .text("Democratic senator, Trump state");
A.g.append("text").attr("x",(A.M.l+500-A.M.r)/2).attr("y",H-6)
  .attr("text-anchor","middle").attr("font-size","11px").attr("class","lbl")
  .text("Republican share of the presidential vote in the state");
A.g.append("text").attr("transform","rotate(-90)")
  .attr("x",-(A.M.t+(H-A.M.b))/2).attr("y",11).attr("text-anchor","middle")
  .attr("font-size","11px").attr("class","lbl")
  .text("the same share in the Senate contest");
const B=panel(505,0,275,H,50-WIN,50+WIN,4,()=>true,
  "the box, magnified", "within six points of even, both ways");
B.g.append("rect").attr("x",B.M.l).attr("y",B.M.t)
  .attr("width",275-B.M.r-B.M.l).attr("height",H-B.M.b-B.M.t)
  .attr("fill","none").attr("stroke","#76838C");
})();
</script>', SN, WIN))

## ---- indep
z <- sen[sen$winner_side == "I", ]
data.frame(
  State = z$state,
  Senator = z$winner,
  Ran_as = z$winner_party,
  President = ifelse(z$pres_winner == "D", "Harris", "Trump"),
  Margin = paste0(p1(z$margin), " pts"))

## ---- trend-static
op <- par(mar = c(3.6, 4.4, 2.0, 1.6), mgp = c(2.6, 0.7, 0))
plot(trd$year, trd$pct_split, type = "o", pch = 19, cex = 0.7, lwd = 2,
     col = "#1C4C5C", axes = FALSE, xlab = "", ylab = "",
     ylim = c(0, max(trd$pct_split) + 4))
axis(1, at = seq(1952, 2024, 8), cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
axis(2, las = 1, cex.axis = 0.78, lwd = 0, lwd.ticks = 1)
mtext("% of contested districts", 2, line = 2.9, cex = 0.86)
mtext("Districts that voted one way for president and the other for the House",
      3, line = 0.6, cex = 0.82, adj = 0)
pk <- trd[which.max(trd$pct_split), ]
text(pk$year, pk$pct_split + 2.6, paste0(pk$year, ": ", p1(pk$pct_split), "%"),
     cex = 0.7, col = MUTE)
lt <- trd[nrow(trd), ]
text(lt$year, lt$pct_split + 8.5, paste0(lt$year, ": ", p1(lt$pct_split), "%"),
     cex = 0.7, col = MUTE, adj = 1)
par(op)

## ---- trend-d3
# Drawn with the shared library (_lib/dd-charts.js): a plain line with points
# and a per-election tooltip needs nothing hand-written. d3 itself was already
# emitted by the county figure above, so only dd-charts.js is added here.
dd_fig("trend", "line", trd[, c("year", "pct_split")],
  size = list(w = 770, h = 360, m = list(t = 16, r = 24, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "pct_split", label = "% of contested districts",
           domain = c(0, 48), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "pct_split", label = "split districts", class = "series-1"))),
  points = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+d.pct_split.toFixed(1)+
      "% of contested districts<br>split their vote";
  }'),
  d3 = FALSE)
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the chart for each election\'s share.</p>')
