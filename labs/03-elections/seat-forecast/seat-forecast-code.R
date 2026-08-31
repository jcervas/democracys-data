# seat-forecast-code.R -- chunk bodies for seat-forecast-brief.Rmd
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

gb    <- read.csv("data/derived/generic_ballot.csv")
base  <- read.csv("data/derived/baseline.csv")
cal   <- read.csv("data/derived/calibration.csv")
curve <- read.csv("data/derived/curve.csv")
sim   <- read.csv("data/derived/simulation.csv")
sd0   <- read.csv("data/derived/sim_default.csv")
mods  <- read.csv("data/derived/models.csv")
spr   <- read.csv("data/derived/spread.csv")
rd    <- read.csv("data/derived/redistricting.csv")
sup   <- read.csv("data/derived/senate_seats.csv")
spar  <- read.csv("data/derived/senate_parse.csv")
scv   <- read.csv("data/derived/senate_curve.csv")
ssd   <- read.csv("data/derived/senate_sd_by_year.csv")
scal  <- read.csv("data/derived/senate_calibration.csv")
ssim  <- read.csv("data/derived/senate_sim.csv")
joint <- read.csv("data/derived/joint.csv")
jcmp  <- read.csv("data/derived/joint_compare.csv")
cloud <- read.csv("data/derived/joint_cloud.csv")

avg     <- gb[gb$source == "Average", ]
aggs    <- gb[gb$source != "Average", ]
POLL_2P <- avg$two_party_dem
NAT24   <- 48.649
DS24    <- sum(base$party_2024 == "D")
RS24    <- 435 - DS24
SWING   <- round(POLL_2P - NAT24, 2)

TIP     <- min(curve$national_2p[curve$seats >= 218])
ordb    <- base[order(-base$base), ]
tipd    <- ordb$district_id[218]
tipv    <- ordb$base[218]
NEAR    <- sum(abs(base$base - 50) <= 5)
SLOPE   <- round(as.numeric(coef(lm(seats ~ national_2p,
             curve[curve$national_2p >= 50 & curve$national_2p <= 56, ]))[2]), 2)

SD_DIST  <- round(mean(cal$sd_dev[cal$to >= 2018]), 2)
SD_MODEL <- round(sd(cal$err) / SLOPE, 2)
SD_POLL  <- 2.0
SD_NAT   <- round(sqrt(SD_MODEL^2 + SD_POLL^2), 2)

fc      <- mods[mods$model == "Uniform swing, simulated", ]
MED     <- fc$point; LO <- fc$lo; HI <- fc$hi
PHOUSE  <- fc$p_house
f51     <- mods[mods$model == "FiftyPlusOne", ]

MAE     <- round(mean(abs(cal$err)), 1)
MAE_NC  <- round(mean(abs(cal$nochange_err)), 1)
RD_ENACT <- sum(rd$enacted)
RD_COMP  <- sum(rd$comp_seats)

# ---- the Senate ----
DEM_FLOOR  <- 34
DEM_TARGET <- 51 - DEM_FLOOR
SEN_HELD_D <- sum(sup$held == "D")
SEN_GAIN   <- DEM_TARGET - SEN_HELD_D
SEN_TIP    <- sort(sup$pres24_2p, decreasing = TRUE)[DEM_TARGET]
SEN_TIP_ST <- sup$state[order(-sup$pres24_2p)][DEM_TARGET]
SEN_TIP_V  <- min(scv$national_2p[scv$seats >= 51])
SD_STATE   <- round(sd(scal$resid[scal$midterm]), 2)
SEN_MED    <- ssim$seats[which.max(cumsum(ssim$share) >= 0.5)]
SEN_LO     <- ssim$seats[which.max(cumsum(ssim$share) >= 0.10)]
SEN_HI     <- ssim$seats[which.max(cumsum(ssim$share) >= 0.90)]
JS <- joint[joint$model == "one national error, shared by both chambers", ]
JI <- joint[joint$model == "a separate national error for each chamber", ]

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
mg <- function(v) sprintf("%s%.1f", ifelse(v >= 50, "D+", "R+"),
                          abs(2 * (v - 50)))

# ---- one palette for this document ----------------------------------------
# The quantity on every axis here is Democratic seats or the Democratic share
# of the vote, which IS a party. That is the one case where red and blue are
# the honest encoding rather than a decoration, so this chapter uses them and
# the rest of the marks stay out of the way in grey.
DEM  <- "#2c7fb8"
GOP  <- "#c41230"
REF  <- "#12181D"   # the 218 line: a threshold, not a party
NEU  <- "#76838C"
BAND <- "#CBD3D8"

subcap <- function(txt, width = 100, line = 3.4, cex = 0.66) {
  cw <- strwrap(txt, width = width)
  mtext(cw, side = 1, line = line + (seq_along(cw) - 1) * 0.95, adj = 0,
        cex = cex, col = "#555555")
}

cap_agg <- paste0(
  "Six aggregators reading the same polls in the same week. The spread ",
  "between them is ", pc(diff(range(aggs$margin))), " points.")
cap_curve <- paste0(
  "Seats the model gives the Democrats at each national vote share, with no ",
  "error term switched on. The step at each district is one seat changing hands.")
cap_dial <- paste0(
  "The distribution of Democratic seats over ", format(sum(sd0$n), big.mark = ","),
  " simulated elections.")
cap_where <- paste0(
  "The same 80% interval, with one error term switched on at a time.")
cap_back <- paste0(
  "Eight pairs of consecutive elections on unchanged district lines. The model ",
  "was told the national vote and asked only for the seats.")
cap_mods <- "Five answers to one question, with the interval each one states."
cap_lad <- paste0(
  "All 33 seats up in 2026, ordered by how their state voted for president in ",
  "2024. Democrats need the top ", DEM_TARGET, ".")
cap_cloud <- paste0(
  "Every simulated election placed by both of its outcomes, ",
  "binned four House seats wide.")

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- agg-table
o <- aggs[order(-aggs$margin),
          c("source", "dem_pct", "rep_pct", "other_pct", "two_party_dem")]
names(o) <- c("aggregator", "Democrats", "Republicans",
              "other or undecided", "two-party Democratic share")
o

## ---- agg-static
a <- aggs[order(aggs$margin), ]
par(mar = c(6.2, 9.4, 0.6, 2))
plot(a$margin, seq_len(nrow(a)), pch = 19, cex = 1.3, col = DEM,
     xlim = c(0, 9), yaxt = "n", ylab = "", xlab = "Democratic margin, points",
     ylim = c(0.4, nrow(a) + 0.6))
abline(v = avg$margin, col = NEU, lty = 3)
axis(2, at = seq_len(nrow(a)), labels = a$source, las = 1, tick = FALSE,
     cex.axis = 0.78)
text(a$margin, seq_len(nrow(a)), sprintf("%+.1f", a$margin), pos = 4,
     cex = 0.68, col = "#555555")
text(avg$margin, nrow(a) + 0.55, paste0("average ", sprintf("%+.1f", avg$margin)),
     cex = 0.68, col = "#555555")
subcap(cap_agg, line = 4.2)

## ---- agg-d3
a <- aggs[order(aggs$margin), ]
rows <- paste(sprintf('{"s":"%s","m":%.1f,"d":%.1f,"r":%.1f,"o":%.1f,"t":%.2f}',
                      a$source, a$margin, a$dem_pct, a$rep_pct, a$other_pct,
                      a$two_party_dem), collapse = ",")
cat(sprintf('
<style>
.fc svg { max-width:100%%; height:auto; font:12px inherit; }
.fc .dem   { fill: var(--map-dem); }
.fc .gop   { fill: var(--map-gop); }
.fc .demS  { stroke: var(--map-dem); }
.fc .gopS  { stroke: var(--map-gop); }
.fc .ink   { fill: var(--ink); }
.fc .ink2  { fill: var(--ink-2); }
.fc .ink3  { fill: var(--ink-3); }
.fc .rule  { stroke: var(--rule); }
.fc .rule2 { stroke: var(--rule-2); }
.fc .refS  { stroke: var(--ink); }
.fc .band  { fill: var(--rule-2); }
.fc .tip   { position:absolute; pointer-events:none; background:var(--ink);
             color:var(--paper); padding:7px 10px; font-size:12px; opacity:0;
             white-space:nowrap; }
.fc .ctl   { display:flex; flex-wrap:wrap; gap:1.1rem; margin:.4rem 0 .8rem; }
.fc .ctl label { font-size:12px; color:var(--ink-2); display:block; }
.fc .ctl input[type=range] { width:190px; accent-color:var(--accent); }
.fc .ctl b { color:var(--ink); font-variant-numeric:tabular-nums; }
.fc .read  { display:flex; flex-wrap:wrap; gap:1.6rem; margin:.2rem 0 .6rem;
             font-size:13px; color:var(--ink-2); }
.fc .read b { display:block; font-size:1.55rem; color:var(--ink);
              font-variant-numeric:tabular-nums; line-height:1.15; }
</style>
<div id="agg" class="fc" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const A=[%s], AVG=%.1f;
const W=760,H=250,M={t:16,r:70,b:44,l:150};
const box=d3.select("#agg");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([0,9]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(A.map(d=>d.s)).range([M.t+10,H-M.b-10]).padding(0.5);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>"D+"+d));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("class","ink2").text("Democratic margin on the generic ballot, points");
svg.append("line").attr("x1",x(AVG)).attr("x2",x(AVG)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("class","rule").attr("stroke-dasharray","3,3");
svg.append("text").attr("x",x(AVG)).attr("y",M.t-2).attr("text-anchor","middle")
  .attr("font-size","11px").attr("class","ink3").text("average D+"+AVG.toFixed(1));
svg.append("g").selectAll("text").data(A).join("text")
  .attr("x",M.l-12).attr("y",d=>y(d.s)+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("class","ink2").text(d=>d.s);
const tip=box.append("div").attr("class","tip");
svg.append("g").selectAll("circle").data(A).join("circle")
  .attr("cx",d=>x(d.m)).attr("cy",d=>y(d.s)).attr("r",6).attr("class","dem")
  .attr("stroke","var(--paper)").attr("stroke-width",2)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.s}</b><br>Democrats ${d.d}%%, Republicans ${d.r}%%<br>`+
      `neither ${d.o}%%<br>two-party Democratic share ${d.t}%%`)
      .style("left",Math.min(ev.offsetX+14,W-280)+"px")
      .style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.v").data(A).join("text").attr("class","v ink3")
  .attr("x",d=>x(d.m)+12).attr("y",d=>y(d.s)+4).attr("font-size","11px")
  .text(d=>"+"+d.m.toFixed(1));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover for each aggregator&#39;s full numbers.</p>
', rows, avg$margin, cap_agg))

## ---- impute-fit
ok <- !base$imputed
fit <- lm(base ~ dpres, base[ok, ])
data.frame(
  quantity = c("Districts with a two-party House result",
               "Districts without one",
               "How closely the presidential vote tracks the House vote",
               "Typical miss when it is used to predict the House vote"),
  value = c(sum(ok), sum(!ok),
            paste0("r-squared ", pc(summary(fit)$r.squared, 3)),
            paste0(pc(summary(fit)$sigma), " points")))

## ---- footnote-seats
o <- base[base$no_totals, c("district_id", "dpres", "party_2024")]
names(o) <- c("district", "Democratic share of the 2024 presidential vote",
              "assigned to")
o

## ---- baseline-check
data.frame(
  quantity = c("Democratic seats the baseline produces",
               "Democratic seats in the 2024 House",
               "Seats within 5 points of the line",
               "Seats more than 5 points from it"),
  value = c(sum(base$base > 50), DS24, NEAR, 435 - NEAR))

## ---- curve-static
cc <- curve[curve$national_2p >= 42 & curve$national_2p <= 58, ]
par(mar = c(6.8, 4.6, 1.0, 1.2))
plot(cc$national_2p, cc$seats, type = "n", las = 1,
     xlab = "national two-party Democratic share of the House vote, %",
     ylab = "Democratic seats")
abline(h = 218, col = REF, lwd = 1.4, lty = 2)
abline(v = 50, col = BAND)
lines(cc$national_2p, cc$seats, lwd = 2.6, col = DEM)
points(TIP, 218, pch = 19, cex = 1.1, col = GOP)
text(TIP, 218, paste0("  218 seats at ", pc(TIP, 1), "% (", mg(TIP), ")"),
     adj = c(0, 1.6), cex = 0.7, col = GOP)
points(POLL_2P, curve$seats[which.min(abs(curve$national_2p - POLL_2P))],
       pch = 19, cex = 1.1, col = REF)
text(POLL_2P, curve$seats[which.min(abs(curve$national_2p - POLL_2P))],
     paste0("what the polls imply  "), adj = c(1, -0.7), cex = 0.7, col = REF)
text(42.2, 224, "218 = a majority", adj = 0, cex = 0.68, col = "#555555")
subcap(cap_curve, line = 4.4)

## ---- curve-d3
cc <- curve[curve$national_2p >= 42 & curve$national_2p <= 58, ]
rows <- paste(sprintf('[%.1f,%d]', cc$national_2p, cc$seats), collapse = ",")
cat(sprintf('
<div id="cur" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[%s], TIP=%.1f, POLL=%.2f, SEATP=%d;
const W=760,H=430,M={t:18,r:26,b:52,l:58};
const box=d3.select("#cur");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([42,58]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([d3.min(C,d=>d[1])-6,d3.max(C,d=>d[1])+6])
  .range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("class","ink2")
  .text("national two-party Democratic share of the House vote");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("class","ink2")
  .text("Democratic seats");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("class","rule");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(218)).attr("y2",y(218))
  .attr("class","refS").attr("stroke-dasharray","5,4").attr("stroke-width",1.4);
svg.append("text").attr("x",M.l+6).attr("y",y(218)-6).attr("font-size","11px")
  .attr("class","ink2").text("218 seats — a majority");
svg.append("path").datum(C).attr("fill","none").attr("class","demS")
  .attr("stroke-width",2.6)
  .attr("d",d3.line().x(d=>x(d[0])).y(d=>y(d[1])));
[[TIP,218,"var(--map-gop)","218 seats at "+TIP.toFixed(1)+"%%"],
 [POLL,SEATP,"var(--ink)","what the polls imply: "+SEATP+" seats"]]
 .forEach(([px,py,col,lab],i)=>{
  svg.append("circle").attr("cx",x(px)).attr("cy",y(py)).attr("r",5.5)
    .attr("fill",col).attr("stroke","var(--paper)").attr("stroke-width",2);
  svg.append("text").attr("x",x(px)+(i?-10:10)).attr("y",y(py)+(i?-12:20))
    .attr("text-anchor",i?"end":"start").attr("font-size","11.5px")
    .attr("fill",col).text(lab);
});
const tip=box.append("div").attr("class","tip");
const hit=svg.append("line").attr("class","rule2").attr("y1",M.t).attr("y2",H-M.b)
  .attr("opacity",0);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const v=x.invert(d3.pointer(ev,this)[0]+M.l-M.l);
    const p=C.reduce((a,b)=>Math.abs(b[0]-v)<Math.abs(a[0]-v)?b:a);
    hit.attr("opacity",1).attr("x1",x(p[0])).attr("x2",x(p[0]));
    const m=(p[0]>=50?"D+":"R+")+(Math.abs(2*(p[0]-50))).toFixed(1);
    tip.style("opacity",1).html(
      `<b>${p[0].toFixed(1)}%% of the national vote</b> (${m})<br>`+
      `${p[1]} Democratic seats, ${435-p[1]} Republican<br>`+
      (p[1]>=218?"Democratic House":"Republican House"))
      .style("left",Math.min(x(p[0])+14,W-260)+"px").style("top",(M.t+8)+"px"); })
  .on("mouseleave",()=>{tip.style("opacity",0);hit.attr("opacity",0);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover anywhere on the plot.</p>
', rows, TIP, POLL_2P,
   curve$seats[which.min(abs(curve$national_2p - POLL_2P))], cap_curve))

## ---- dial-static
par(mar = c(6.8, 6.2, 1.0, 1.2))
b <- barplot(sd0$share, space = 0, border = NA, col = BAND,
             xlab = "Democratic seats", ylab = "", las = 1)
mtext("share of simulations", side = 2, line = 4.4, cex = 0.95)
sel <- sd0$seats >= 218
rect(b[sel] - 0.5, 0, b[sel] + 0.5, sd0$share[sel], col = DEM, border = NA)
rect(b[!sel] - 0.5, 0, b[!sel] + 0.5, sd0$share[!sel], col = GOP, border = NA)
i218 <- which.min(abs(sd0$seats - 218))
abline(v = b[i218] - 0.5, col = REF, lwd = 1.6, lty = 2)
ax <- seq(180, 320, 20)
axis(1, at = b[match(ax, sd0$seats)], labels = ax)
text(b[i218] - 0.5, max(sd0$share) * 0.96, " 218", adj = 0, cex = 0.72, col = REF)
legend("topright", bty = "n", cex = 0.74,
       legend = c(paste0("Democratic House: ", pc(100 * PHOUSE, 0), "% of simulations"),
                  paste0("median ", MED, " seats; 80% interval ", LO, "-", HI)),
       text.col = c(DEM, "#555555"))
subcap(cap_dial, line = 4.4)

## ---- dial-d3
bs <- sort(base$base, decreasing = TRUE)
cat(sprintf('
<div id="dial" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const B=[%s];                       // 435 starting shares, sorted
const NAT=%.3f, SWING0=%.2f, SDN=%.2f, SDD=%.2f, NSIM=4000;
const W=760,H=360,M={t:16,r:24,b:46,l:56};
const box=d3.select("#dial");

// --- controls -----------------------------------------------------------
const ctl=box.append("div").attr("class","ctl");
function slider(id,label,min,max,step,val,fmt){
  const w=ctl.append("div");
  const lab=w.append("label").html(label+" <b id=\'v"+id+"\'></b>");
  const inp=w.append("input").attr("type","range").attr("id",id)
    .attr("min",min).attr("max",max).attr("step",step).attr("value",val);
  return {inp:inp,out:d3.select("#v"+id),fmt:fmt};
}
const S=[
 slider("env","national environment",-4,14,0.5,%.1f,v=>(v>=0?"D+":"R+")+Math.abs(v).toFixed(1)),
 slider("pol","polling error assumed",0,8,0.25,%.1f,v=>"±"+v.toFixed(2)+" pts of margin"),
 slider("dis","district error assumed",0,9,0.05,SDD,v=>"±"+v.toFixed(2)+" pts")
];
const read=box.append("div").attr("class","read");
const rMed=read.append("div").html("<b id=\'rm\'></b>median Democratic seats");
const rInt=read.append("div").html("<b id=\'ri\'></b>80%% of simulations");
const rP  =read.append("div").html("<b id=\'rp\'></b>chance of a Democratic House");

const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([150,320]).range([M.l,W-M.r]);
const y=d3.scaleLinear().range([H-M.b,M.t]);
const gx=svg.append("g").attr("transform",`translate(0,${H-M.b})`);
const gy=svg.append("g").attr("transform",`translate(${M.l},0)`);
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("class","ink2").text("Democratic seats");
const gbar=svg.append("g");
const l218=svg.append("line").attr("class","refS").attr("stroke-width",1.6)
  .attr("stroke-dasharray","5,4").attr("y1",M.t).attr("y2",H-M.b)
  .attr("x1",x(218)).attr("x2",x(218));
svg.append("text").attr("x",x(218)+5).attr("y",M.t+11).attr("font-size","11px")
  .attr("class","ink2").text("218");

// --- the model ----------------------------------------------------------
// One national draw per simulation, then the 435 districts. Each district is
// Democratic with probability Phi((base + shift - 50)/sd_district); the seat
// total is the sum of 435 such coin flips, which is close enough to normal
// that it can be drawn directly instead of flipped one at a time.
function Phi(z){                       // normal CDF, Abramowitz & Stegun 26.2.17
  const s=z<0?-1:1; z=Math.abs(z)/Math.SQRT2;
  const t=1/(1+0.3275911*z);
  const e=1-(((((1.061405429*t-1.453152027)*t)+1.421413741)*t-0.284496736)*t
        +0.254829592)*t*Math.exp(-z*z);
  return 0.5*(1+s*e);
}
let sp=0, spare=0;
function gauss(){                      // Box-Muller
  if(sp){sp=0;return spare;}
  let u=0,v=0,s2=0;
  do{u=Math.random()*2-1;v=Math.random()*2-1;s2=u*u+v*v;}while(s2>=1||s2===0);
  const m=Math.sqrt(-2*Math.log(s2)/s2); spare=v*m; sp=1; return u*m;
}
function run(env,sdPollMargin,sdDist){
  const V=50+env/2;                    // margin -> two-party share
  const shift=V-NAT;
  const sdNat=Math.sqrt(SDN*SDN-%.2f*%.2f + (sdPollMargin/2)*(sdPollMargin/2));
  const out=new Array(NSIM);
  for(let k=0;k<NSIM;k++){
    const en=gauss()*sdNat;
    let mu=0,va=0;
    for(let i=0;i<435;i++){
      const p= sdDist>0 ? Phi((B[i]+shift+en-50)/sdDist)
                        : ((B[i]+shift+en>50)?1:0);
      mu+=p; va+=p*(1-p);
    }
    out[k]=Math.max(0,Math.min(435,Math.round(mu+gauss()*Math.sqrt(va))));
  }
  out.sort((a,b)=>a-b);
  return out;
}
function draw(){
  const env=+S[0].inp.property("value"),
        pol=+S[1].inp.property("value"),
        dis=+S[2].inp.property("value");
  S.forEach(s=>s.out.text(s.fmt(+s.inp.property("value"))));
  const o=run(env,pol,dis);
  const med=o[Math.floor(NSIM*0.5)], lo=o[Math.floor(NSIM*0.10)],
        hi=o[Math.floor(NSIM*0.90)];
  const p=o.filter(v=>v>=218).length/NSIM;
  d3.select("#rm").text(med);
  d3.select("#ri").text(lo+"–"+hi);
  d3.select("#rp").text(Math.round(p*100)+"%%")
    .style("color",p>=0.5?"var(--map-dem)":"var(--map-gop)");
  const bins=d3.bin().domain([150,320]).thresholds(d3.range(150,321,3))(o);
  y.domain([0,d3.max(bins,b=>b.length)/NSIM*1.08]);
  gx.transition().duration(200).call(d3.axisBottom(x).ticks(9));
  gy.transition().duration(200).call(d3.axisTicks=d3.axisLeft(y).ticks(4)
    .tickFormat(d3.format(".0%%")));
  gbar.selectAll("rect").data(bins).join("rect")
    .attr("x",b=>x(b.x0)+0.5).attr("width",b=>Math.max(1,x(b.x1)-x(b.x0)-1.5))
    .attr("fill",b=>b.x0>=218?"var(--map-dem)":"var(--map-gop)")
    .transition().duration(200)
    .attr("y",b=>y(b.length/NSIM)).attr("height",b=>y(0)-y(b.length/NSIM));
}
S.forEach(s=>s.inp.on("input",draw));
draw();
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Move any dial. Everything below it recomputes.</p>
', paste(sprintf("%.2f", bs), collapse = ","), NAT24, SWING, SD_NAT, SD_DIST,
   round(2 * (POLL_2P - 50), 1), SD_POLL * 2, SD_POLL, SD_POLL, cap_dial))

## ---- spread-table
o <- spr[, c("terms", "lo80", "hi80", "width", "p_house")]
names(o) <- c("error terms switched on", "10th percentile", "90th percentile",
              "width, seats", "chance of a Democratic House")
o$`chance of a Democratic House` <- paste0(pc(100 * spr$p_house, 0), "%")
o

## ---- where-static
par(mar = c(6.2, 11.5, 0.6, 7))
plot(NA, xlim = c(min(spr$lo80) - 4, max(spr$hi80) + 4), xpd = NA,
     ylim = c(0.5, nrow(spr) + 0.5), yaxt = "n", ylab = "",
     xlab = "Democratic seats", las = 1)
abline(v = 218, col = REF, lty = 2)
segments(spr$lo80, seq_len(nrow(spr)), spr$hi80, seq_len(nrow(spr)),
         lwd = 7, col = BAND, lend = 1)
points(rep(MED, nrow(spr)), seq_len(nrow(spr)), pch = 19, cex = 0.9, col = DEM)
axis(2, at = seq_len(nrow(spr)), labels = spr$terms, las = 1, tick = FALSE,
     cex.axis = 0.78)
text(spr$hi80, seq_len(nrow(spr)), paste0(" ", spr$width, " seats wide"),
     pos = 4, cex = 0.66, col = "#555555", xpd = NA)
text(218, nrow(spr) + 0.42, "218", cex = 0.68, col = REF)
subcap(cap_where, line = 4.2)

## ---- where-d3
rows <- paste(sprintf('{"t":"%s","lo":%d,"hi":%d,"w":%d,"p":%.3f}',
                      spr$terms, spr$lo80, spr$hi80, spr$width, spr$p_house),
              collapse = ",")
cat(sprintf('
<div id="whr" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[%s], MED=%d;
const W=760,H=210,M={t:20,r:100,b:44,l:170};
const box=d3.select("#whr");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([d3.min(R,d=>d.lo)-5,d3.max(R,d=>d.hi)+5])
  .range([M.l,W-M.r]);
const y=d3.scalePoint().domain(R.map(d=>d.t)).range([M.t+8,H-M.b-8]).padding(0.6);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(8));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("class","ink2").text("Democratic seats");
svg.append("line").attr("x1",x(218)).attr("x2",x(218)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("class","refS").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(218)).attr("y",M.t-4).attr("text-anchor","middle")
  .attr("font-size","11px").attr("class","ink2").text("218");
svg.append("g").selectAll("text").data(R).join("text")
  .attr("x",M.l-12).attr("y",d=>y(d.t)+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("class","ink2").text(d=>d.t);
const tip=box.append("div").attr("class","tip");
svg.append("g").selectAll("line").data(R).join("line")
  .attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi))
  .attr("y1",d=>y(d.t)).attr("y2",d=>y(d.t))
  .attr("stroke","var(--rule)").attr("stroke-width",11).attr("stroke-linecap","butt")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.t}</b><br>80%% interval ${d.lo}–${d.hi} (${d.w} seats)<br>`+
      `chance of a Democratic House ${Math.round(d.p*100)}%%`)
      .style("left",Math.min(ev.offsetX+14,W-300)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("circle").data(R).join("circle")
  .attr("cx",x(MED)).attr("cy",d=>y(d.t)).attr("r",5).attr("class","dem")
  .attr("stroke","var(--paper)").attr("stroke-width",2);
svg.append("g").selectAll("text.w").data(R).join("text").attr("class","w ink3")
  .attr("x",d=>x(d.hi)+10).attr("y",d=>y(d.t)+4).attr("font-size","11px")
  .text(d=>d.w+" seats");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover a bar.</p>
', rows, MED, cap_where))

## ---- backtest
o <- cal[, c("from", "to", "n", "swing", "pred_seats", "actual_seats", "err",
             "nochange_err")]
names(o) <- c("from", "to", "districts", "national swing", "model said",
              "actually", "model missed by", "no-change missed by")
o

## ---- backtest-static
par(mar = c(6.6, 4.6, 1.0, 1.2))
rg <- range(c(cal$pred_seats, cal$actual_seats)) + c(-12, 12)
plot(cal$actual_seats, cal$pred_seats, type = "n", xlim = rg, ylim = rg,
     xlab = "seats the Democrats actually won",
     ylab = "seats the model said", las = 1)
abline(0, 1, col = BAND, lwd = 1.4)
segments(cal$actual_seats, cal$actual_seats, cal$actual_seats, cal$pred_seats,
         col = NEU, lty = 3)
points(cal$actual_seats, cal$pred_seats, pch = 19, cex = 1.3, col = DEM)
# 2020 and 2024 sit almost on top of each other; label one of each such pair to
# the left so the years do not overprint.
near <- abs(outer(cal$actual_seats, cal$actual_seats, "-")) < 10 &
        abs(outer(cal$pred_seats,   cal$pred_seats,   "-")) < 10
side <- ifelse(apply(near, 1, function(z) which(z)[1]) == seq_len(nrow(cal)),
               4, 2)
text(cal$actual_seats, cal$pred_seats, cal$to, pos = side, cex = 0.66,
     col = "#555555")
text(rg[1] + 3, rg[2] - 4, "on this line the model was exactly right",
     adj = 0, cex = 0.68, col = "#555555")
subcap(cap_back, line = 4.2)

## ---- backtest-d3
rows <- paste(sprintf('{"y":%d,"f":%d,"a":%d,"p":%d,"e":%d,"nc":%d,"s":%.2f}',
                      cal$to, cal$from, cal$actual_seats, cal$pred_seats,
                      cal$err, cal$nochange_err, cal$swing), collapse = ",")
lo <- min(c(cal$pred_seats, cal$actual_seats)) - 12
hi <- max(c(cal$pred_seats, cal$actual_seats)) + 12
cat(sprintf('
<div id="bt" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], LO=%d, HI=%d;
const W=760,H=430,M={t:18,r:26,b:50,l:60};
const box=d3.select("#bt");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([LO,HI]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([LO,HI]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(8));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("class","ink2").text("seats the Democrats actually won");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("class","ink2")
  .text("seats the model said");
svg.append("line").attr("x1",x(LO)).attr("y1",y(LO)).attr("x2",x(HI)).attr("y2",y(HI))
  .attr("class","rule").attr("stroke-width",1.4);
svg.append("text").attr("x",x(LO)+10).attr("y",y(HI)+16).attr("font-size","11px")
  .attr("class","ink3").text("on this line the model was exactly right");
svg.append("g").selectAll("line.d").data(D).join("line").attr("class","d rule2")
  .attr("x1",d=>x(d.a)).attr("x2",d=>x(d.a)).attr("y1",d=>y(d.a)).attr("y2",d=>y(d.p))
  .attr("stroke-dasharray","2,3");
const tip=box.append("div").attr("class","tip");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.a)).attr("cy",d=>y(d.p)).attr("r",6).attr("class","dem")
  .attr("stroke","var(--paper)").attr("stroke-width",2)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.y}</b> (from ${d.f})<br>national swing ${d.s>0?"+":""}${d.s} points<br>`+
      `model said ${d.p}, actually ${d.a} — missed by ${d.e>0?"+":""}${d.e}<br>`+
      `assuming no change missed by ${d.nc>0?"+":""}${d.nc}`)
      .style("left",Math.min(ev.offsetX+14,W-320)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.l").data(D).join("text").attr("class","l ink3")
  .attr("x",d=>x(d.a)+11).attr("y",d=>y(d.p)+4).attr("font-size","11px").text(d=>d.y);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover a year.</p>
', rows, lo, hi, cap_back))

## ---- model-table
o <- mods
o$point <- as.character(o$point)
o$interval <- ifelse(is.na(o$lo), "—", paste0(o$lo, " to ", o$hi))
o$chance <- ifelse(is.na(o$p_house), "—", paste0(pc(100 * o$p_house, 0), "%"))
o <- o[, c("model", "what_it_uses", "point", "interval", "chance")]
names(o) <- c("model", "what it uses", "Democratic seats", "80% interval",
              "chance of a Democratic House")
o

## ---- models-static
m <- mods[nrow(mods):1, ]
par(mar = c(6.2, 12.5, 0.6, 6))
plot(NA, xlim = c(200, 292), ylim = c(0.5, nrow(m) + 0.5), yaxt = "n",
     ylab = "", xlab = "Democratic seats", las = 1)
abline(v = 218, col = REF, lty = 2)
has <- !is.na(m$lo)
segments(m$lo[has], which(has), m$hi[has], which(has), lwd = 7, col = BAND,
         lend = 1)
points(m$point, seq_len(nrow(m)), pch = 19, cex = 1.15, col = DEM)
axis(2, at = seq_len(nrow(m)), labels = m$model, las = 1, tick = FALSE,
     cex.axis = 0.76)
lab <- ifelse(is.na(m$p_house), "", paste0(pc(100 * m$p_house, 0), "%"))
text(292, seq_len(nrow(m)), lab, cex = 0.72, col = DEM, adj = 1)
text(292, nrow(m) + 0.45, "P(House)", cex = 0.66, col = "#555555", adj = 1)
text(218, nrow(m) + 0.45, "218", cex = 0.68, col = REF)
subcap(cap_mods, line = 4.2)

## ---- models-d3
m <- mods[nrow(mods):1, ]
rows <- paste(sprintf('{"m":"%s","u":"%s","pt":%d,"lo":%s,"hi":%s,"p":%s}',
                      m$model, gsub('"', "", m$what_it_uses), m$point,
                      ifelse(is.na(m$lo), "null", m$lo),
                      ifelse(is.na(m$hi), "null", m$hi),
                      ifelse(is.na(m$p_house), "null", m$p_house)),
              collapse = ",")
cat(sprintf('
<div id="mdl" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[%s];
const W=760,H=250,M={t:22,r:96,b:44,l:190};
const box=d3.select("#mdl");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([200,292]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(R.map(d=>d.m)).range([M.t+6,H-M.b-6]).padding(0.55);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(8));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("class","ink2").text("Democratic seats");
svg.append("line").attr("x1",x(218)).attr("x2",x(218)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("class","refS").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(218)).attr("y",M.t-6).attr("text-anchor","middle")
  .attr("font-size","11px").attr("class","ink2").text("218");
svg.append("text").attr("x",W-M.r+40).attr("y",M.t-6).attr("text-anchor","end")
  .attr("font-size","11px").attr("class","ink3").text("P(House)");
svg.append("g").selectAll("text").data(R).join("text")
  .attr("x",M.l-12).attr("y",d=>y(d.m)+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("class","ink2").text(d=>d.m);
const tip=box.append("div").attr("class","tip");
svg.append("g").selectAll("line").data(R.filter(d=>d.lo!==null)).join("line")
  .attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi))
  .attr("y1",d=>y(d.m)).attr("y2",d=>y(d.m))
  .attr("stroke","var(--rule)").attr("stroke-width",11);
svg.append("g").selectAll("circle").data(R).join("circle")
  .attr("cx",d=>x(d.pt)).attr("cy",d=>y(d.m)).attr("r",5.5).attr("class","dem")
  .attr("stroke","var(--paper)").attr("stroke-width",2)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.m}</b><br>${d.u}<br>${d.pt} seats`+
      (d.lo!==null?`, 80%% interval ${d.lo}–${d.hi}`:"")+
      (d.p!==null?`<br>chance of a Democratic House ${Math.round(d.p*100)}%%`:""))
      .style("left",Math.min(ev.offsetX+14,W-340)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.p").data(R.filter(d=>d.p!==null)).join("text")
  .attr("class","p").attr("x",W-M.r+40).attr("y",d=>y(d.m)+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("fill","var(--map-dem)")
  .text(d=>Math.round(d.p*100)+"%%");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover for what each model uses.</p>
', rows, cap_mods))

## ---- senate-arithmetic
data.frame(
  quantity = c("Seats up in 2026",
               "... held by Democrats going in",
               "Seats not up, held by Democrats or independents who caucus with them",
               "Seats not up, held by Republicans",
               "Seats a Democratic majority needs",
               "... so seats they must win of the ones up",
               "... which is a net gain of"),
  value = c(nrow(sup), SEN_HELD_D, DEM_FLOOR, 100 - nrow(sup) - DEM_FLOOR,
            51, DEM_TARGET, SEN_GAIN))

## ---- senate-sd
o <- ssd
names(o) <- c("election", "kind", "races measured",
              "points from the state's presidential vote, sd")
o

## ---- ladder-static
s <- sup[order(sup$pres24_2p), ]
par(mar = c(6.6, 5.6, 1.2, 2.4))
cl <- ifelse(s$held == "D", DEM, GOP)
plot(s$pres24_2p, seq_len(nrow(s)), type = "n", yaxt = "n", ylab = "",
     xlab = "Democratic share of the state's 2024 presidential vote, %",
     xlim = c(24, 66), ylim = c(0.4, nrow(s) + 0.6))
abline(v = 50, col = BAND)
cut <- nrow(s) - DEM_TARGET + 0.5
abline(h = cut, col = REF, lty = 2)
segments(50, seq_len(nrow(s)), s$pres24_2p, seq_len(nrow(s)), col = "#CCCCCC")
points(s$pres24_2p, seq_len(nrow(s)), pch = 19, cex = 1.0, col = cl)
axis(2, at = seq_len(nrow(s)), labels = s$state, las = 1, tick = FALSE,
     cex.axis = 0.62)
text(24, cut, paste0(" Democrats need everything above this line (",
                     DEM_TARGET, " seats)"), adj = c(0, -0.5), cex = 0.66,
     col = REF)
legend("bottomright", bty = "n", cex = 0.7,
       legend = c("Democratic-held", "Republican-held"),
       col = c(DEM, GOP), pch = 19)
subcap(cap_lad, line = 4.6)

## ---- ladder-d3
s <- sup[order(-sup$pres24_2p), ]
rows <- paste(sprintf('{"st":"%s","nm":"%s","p":%.2f,"h":"%s","i":%d}',
                      s$state, gsub("'", "", s$last_name), s$pres24_2p,
                      s$held, seq_len(nrow(s))), collapse = ",")
cat(sprintf('
<div id="lad" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const S=[%s], NEED=%d;
const W=760,H=560,M={t:22,r:120,b:44,l:64};
const box=d3.select("#lad");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([24,66]).range([M.l,W-M.r]);
const y=d3.scalePoint().domain(S.map(d=>d.st)).range([M.t,H-M.b]).padding(0.5);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("class","ink2")
  .text("Democratic share of the state’s 2024 presidential vote");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t-8).attr("y2",H-M.b)
  .attr("class","rule");
const cut=(y(S[NEED-1].st)+y(S[NEED].st))/2;
svg.append("line").attr("x1",M.l-50).attr("x2",W-M.r+50).attr("y1",cut).attr("y2",cut)
  .attr("class","refS").attr("stroke-dasharray","5,4").attr("stroke-width",1.4);
svg.append("text").attr("x",M.l-50).attr("y",cut-6).attr("font-size","11px")
  .attr("class","ink2")
  .text("Democrats need everything above this line — "+NEED+" seats");
svg.append("g").selectAll("text").data(S).join("text")
  .attr("x",M.l-10).attr("y",d=>y(d.st)+4).attr("text-anchor","end")
  .attr("font-size","11px").attr("class","ink2").text(d=>d.st);
svg.append("g").selectAll("line.s").data(S).join("line").attr("class","s")
  .attr("x1",x(50)).attr("x2",d=>x(d.p)).attr("y1",d=>y(d.st)).attr("y2",d=>y(d.st))
  .attr("class","rule2").attr("stroke-width",1.5);
const tip=box.append("div").attr("class","tip");
svg.append("g").selectAll("circle").data(S).join("circle")
  .attr("cx",d=>x(d.p)).attr("cy",d=>y(d.st)).attr("r",5)
  .attr("fill",d=>d.h==="D"?"var(--map-dem)":"var(--map-gop)")
  .attr("stroke","var(--paper)").attr("stroke-width",1.5)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.st}</b> — ${d.nm} (${d.h})<br>`+
      `Harris ${d.p.toFixed(1)}%% of the two-party vote<br>`+
      `${d.i<=NEED?"inside":"outside"} the ${NEED} Democrats need`)
      .style("left",Math.min(x(d.p)+14,W-260)+"px").style("top",(y(d.st)-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.n").data(S).join("text").attr("class","n ink3")
  .attr("x",d=>x(d.p)+10).attr("y",d=>y(d.st)+4).attr("font-size","10.5px")
  .text(d=>d.nm);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Hover a seat.</p>
', rows, DEM_TARGET, cap_lad))

## ---- joint-table
o <- data.frame(
  outcome = c("Democratic House", "Democratic Senate",
              "Both chambers", "Neither chamber", "One of the two"),
  shared = pc(100 * c(JS$p_house, JS$p_senate, JS$p_both, JS$p_neither,
                      JS$p_split), 0),
  separate = pc(100 * c(JI$p_house, JI$p_senate, JI$p_both, JI$p_neither,
                        JI$p_split), 0))
o$shared <- paste0(o$shared, "%"); o$separate <- paste0(o$separate, "%")
names(o) <- c("outcome", "one national error, shared",
              "a separate error for each chamber")
o

## ---- cloud-static
par(mfrow = c(1, 2), oma = c(3.0, 0, 0, 0), mar = c(4.6, 4.2, 2.2, 0.8))
for (md in c("shared", "separate")) {
  cd <- cloud[cloud$mode == md, ]
  plot(cd$house, cd$senate, type = "n", xlim = c(160, 320), ylim = c(40, 62),
       xlab = "Democratic House seats", ylab = "Democratic Senate seats",
       las = 1, main = "", cex.axis = 0.8)
  title(ifelse(md == "shared", "one shared national error",
               "a separate error for each"), cex.main = 0.86, font.main = 1)
  points(cd$house, cd$senate, pch = 15,
         cex = 0.28 + 3.2 * sqrt(cd$share), col = "#9CB6C9")
  abline(v = 218, col = REF, lty = 2); abline(h = 51, col = REF, lty = 2)
  jj <- if (md == "shared") JS else JI
  text(316, 61.0, paste0("both ", pc(100 * jj$p_both, 0), "%"), adj = 1,
       cex = 0.72, col = DEM)
  text(316, 42.0, paste0("House only ",
       pc(100 * (jj$p_house - jj$p_both), 0), "%"), adj = 1, cex = 0.72,
       col = "#555555")
  text(163, 42.0, paste0("neither ", pc(100 * jj$p_neither, 0), "%"), adj = 0,
       cex = 0.72, col = GOP)
}
cw <- strwrap(cap_cloud, 108)
mtext(cw, side = 1, line = 0.6 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.62, col = "#555555", outer = TRUE)
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

## ---- cloud-d3
rows <- paste(sprintf('{"m":"%s","h":%d,"s":%d,"v":%.6f}',
                      cloud$mode, cloud$house, cloud$senate, cloud$share),
              collapse = ",")
lab <- paste(sprintf('{"m":"%s","both":%.4f,"h":%.4f,"s":%.4f,"n":%.4f,"ind":%.4f}',
                     c("shared", "separate"),
                     c(JS$p_both, JI$p_both), c(JS$p_house, JI$p_house),
                     c(JS$p_senate, JI$p_senate),
                     c(JS$p_neither, JI$p_neither),
                     c(JS$p_independent, JI$p_independent)), collapse = ",")
cat(sprintf('
<div id="cld" class="fc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[%s], L=[%s];
const W=760,H=430,M={t:34,r:26,b:50,l:58};
const box=d3.select("#cld");
const bar=box.append("div").attr("class","ctl");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`);
const x=d3.scaleLinear().domain([160,320]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([40,62]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("class","ink2").text("Democratic House seats");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("class","ink2")
  .text("Democratic Senate seats");
svg.append("rect").attr("x",x(218)).attr("y",M.t).attr("width",W-M.r-x(218))
  .attr("height",y(51)-M.t).attr("fill","var(--map-dem)").attr("opacity",0.07);
svg.append("line").attr("x1",x(218)).attr("x2",x(218)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("class","refS").attr("stroke-dasharray","5,4");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(51)).attr("y2",y(51))
  .attr("class","refS").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(218)+4).attr("y",H-M.b-6).attr("font-size","11px")
  .attr("class","ink2").text("218");
svg.append("text").attr("x",M.l+4).attr("y",y(51)-5).attr("font-size","11px")
  .attr("class","ink2").text("51");
const g=svg.append("g");
const note=svg.append("text").attr("x",M.l).attr("y",M.t-16)
  .attr("font-size","12px").attr("class","ink");
const corner=svg.append("g");
const tip=box.append("div").attr("class","tip");
let mode="shared";
function draw(){
  const D=C.filter(d=>d.m===mode), L0=L.find(d=>d.m===mode);
  const r=d3.scaleSqrt().domain([0,d3.max(D,d=>d.v)]).range([0.6,7]);
  g.selectAll("rect").data(D,d=>d.h+"_"+d.s).join(
    en=>en.append("rect").attr("fill","var(--map-dem)"),
    up=>up, ex=>ex.remove())
   .attr("x",d=>x(d.h)-r(d.v)/2).attr("y",d=>y(d.s)-r(d.v)/2)
   .attr("width",d=>r(d.v)).attr("height",d=>r(d.v))
   .attr("opacity",0.75)
   .on("mousemove",function(ev,d){
     tip.style("opacity",1).html(
       `${d.h}–${d.h+3} House seats, ${d.s} Senate<br>`+
       `${(d.v*100).toFixed(2)}%% of simulations`)
       .style("left",Math.min(ev.offsetX+14,W-260)+"px")
       .style("top",(ev.offsetY-10)+"px"); })
   .on("mouseleave",()=>tip.style("opacity",0));
  note.text(mode==="shared"
    ? "one national error, given to both chambers — the cloud leans"
    : "a separate national error for each chamber — the cloud is round");
  const items=[[W-M.r-6,M.t+14,"end","both "+Math.round(L0.both*100)+"%%","var(--map-dem)"],
               [W-M.r-6,H-M.b-22,"end","House only "+Math.round((L0.h-L0.both)*100)+"%%","var(--ink-2)"],
               [M.l+6,H-M.b-22,"start","neither "+Math.round(L0.n*100)+"%%","var(--map-gop)"],
               [W-M.r-6,M.t+30,"end","independence would say "+Math.round(L0.ind*100)+"%%","var(--ink-3)"]];
  corner.selectAll("text").data(items).join("text")
    .attr("x",d=>d[0]).attr("y",d=>d[1]).attr("text-anchor",d=>d[2])
    .attr("font-size","12px").attr("fill",d=>d[4]).text(d=>d[3]);
}
["one shared national error","a separate error for each chamber"].forEach((t,i)=>{
  bar.append("button").text(t)
    .attr("style","padding:3px 9px;font:inherit;font-size:12px;cursor:pointer")
    .on("click",function(){ mode=i?"separate":"shared"; draw(); });
});
draw();
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
%s Switch the error structure with the buttons.</p>
', rows, lab, cap_cloud))

## ---- joint-compare
o <- jcmp
o$fiftyplusone <- paste0(pc(100 * o$fiftyplusone, 0), "%")
o$this_model   <- paste0(pc(100 * o$this_model, 0), "%")
o$independence_would_say <- ifelse(is.na(o$independence_would_say), "—",
  paste0(pc(100 * o$independence_would_say, 0), "%"))
names(o) <- c("quantity", "FiftyPlusOne", "this model",
              "FiftyPlusOne, if the chambers were independent")
o

## ---- redistricting-table
o <- rd[rd$enacted, c("state", "dem_seats", "comp_seats", "rep_seats")]
o <- o[order(o$rep_seats - o$dem_seats), ]
names(o) <- c("state", "Democratic-leaning seats", "highly competitive seats",
              "Republican-leaning seats")
o

## ---- sensitivity
sens <- read.csv("data/derived/sensitivity.csv")
o <- data.frame(
  assumed = paste0("±", pc(sens$assumed_poll_error_margin), " points of margin"),
  seats = sens$median,
  interval = paste0(sens$lo80, " to ", sens$hi80),
  width = paste(sens$width, "seats"),
  chance = paste0(pc(100 * sens$p_house, 0), "%"))
names(o) <- c("assumed polling error", "median Democratic seats",
              "80% interval", "width", "chance of a Democratic House")
o

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
#cld text[fill="var(--ink-3)" i],
#cld text[fill="var(--map-dem)" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
</style>')
