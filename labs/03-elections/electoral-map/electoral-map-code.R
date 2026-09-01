# electoral-map-code.R -- chunk bodies for electoral-map-brief.Rmd
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
e <- read.csv("data/derived/pres2024_states.csv", stringsAsFactors = FALSE)

o    <- e[order(-e$ev), ]
cum  <- cumsum(o$ev)
k    <- which(cum >= 270)[1]          # states needed to reach 270
top12 <- o[1:k, ]

e$close  <- abs(e$margin) < 5
cl       <- e[e$close, ]
h_share  <- e$harris / (e$harris + e$trump)
h_prop   <- round(h_share * e$ev)
t_prop   <- e$ev - h_prop

# how far each state's three reported percentages fall short of 100
e$short <- 100 - (e$harris + e$trump + e$other)
sh_rep  <- e$short[e$other >  0]
sh_zero <- e$short[e$other == 0]

# The two ways of measuring the same result. Both are taken from a tie and both
# are signed the same way (positive = the Republican direction), so they can
# share one axis in the dumbbell figure and one color ramp on the maps.
d_marg <- e$margin              # Trump % minus Harris %; 0 is a tie
d_shar <- 50 - e$harris         # how far Harris's share sits below 50

# ---- one capped diverging ramp, shared by every map in this document --------
# Every map below classifies with tile_fill() and is keyed with tile_key(), so
# the HTML and PDF versions, and the margin and share maps, cannot drift apart.
CAP <- 30
pal <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(101)
# smallest state whose area-proportional square is still big enough to label;
# the HTML and PDF versions of the first map both use this cutoff
EVMIN <- ceiling(max(e$ev) * (0.19 / 0.46)^2)
tile_fill <- function(v)
  pal[round((pmax(pmin(v, CAP), -CAP) + CAP) / (2 * CAP) * 100) + 1]

# one tile grid, drawn into the current layout panel
tile_panel <- function(v, ttl, by_ev = FALSE, lab_cex = 0.62, ttl_cex = 0.78) {
  par(mar = c(0.3, 0.3, 1.9, 0.3), cex = 1)
  plot(NULL, xlim = c(0.4, 11.6), ylim = c(8.6, 0.4), asp = 1, axes = FALSE,
       xlab = "", ylab = "", main = "")
  hw <- if (by_ev) 0.46 * sqrt(e$ev / max(e$ev)) else rep(0.46, nrow(e))
  rect(e$col - hw, e$row - hw, e$col + hw, e$row + hw,
       col = tile_fill(v), border = "white", lwd = 1.5)
  # shrink the label with the square, and drop it where it would not fit
  text(e$col, e$row, ifelse(hw > 0.19, e$abbrev, ""),
       cex = lab_cex * sqrt(hw / 0.46))
  mtext(ttl, side = 3, line = 0.2, cex = ttl_cex, font = 2)
}

# the shared color key, drawn as its own layout panel. ASCII only here: this
# text goes to the PDF device.
tile_key <- function(unit, notes = character(0)) {
  par(mar = c(3.4, 0.3, 1.6, 0.3), cex = 1)
  plot(NULL, xlim = c(-74, 74), ylim = c(0, 1), axes = FALSE,
       xlab = "", ylab = "")
  xs <- seq(-CAP, CAP, length.out = 102)
  rect(xs[-102], 0.52, xs[-1], 0.98, col = pal, border = pal)
  rect(-CAP, 0.52, CAP, 0.98, border = "grey40", lwd = 0.8)
  at <- seq(-CAP, CAP, CAP / 2)
  segments(at, 0.52, at, 0.40, col = "grey40", lwd = 0.8)
  text(at, 0.30, ifelse(at == 0, "0", sprintf("%+d", as.integer(at))),
       cex = 0.62, col = "#555")
  text(-CAP - 3, 0.75, "Harris ahead", adj = c(1, 0.5), cex = 0.64,
       col = "#2166AC")
  text( CAP + 3, 0.75, "Trump ahead",  adj = c(0, 0.5), cex = 0.64,
       col = "#B2182B")
  mtext(unit, side = 1, line = 0.55, cex = 0.62, col = "#444")
  for (i in seq_along(notes))
    mtext(notes[i], side = 1, line = 0.55 + 0.72 * i, cex = 0.58, col = "#777")
}

# Which points get a name in the size-against-margin scatter, and on which side.
# Two pairs of close states share an electoral-vote total exactly (so they sit
# at the same height); the closer of each pair is labeled to the left so the
# names do not print on top of one another. Computed once so HTML and PDF agree.
qi <- which(e$ev >= 14 | e$close)
q_left <- vapply(qi, function(i)
  e$close[i] && any(e$close[qi] & e$ev[qi] == e$ev[i] &
                    abs(e$margin[qi]) > abs(e$margin[i])), logical(1))

# electoral votes that move to Harris (+) or to Trump (-) in each state when
# winner-take-all is replaced by proportional allocation
dh   <- h_prop - ifelse(e$winner == "Harris", e$ev, 0)
dord <- order(dh)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
st <- function(a, v) e[[v]][e$abbrev == a]

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)                       # fails_when -> fails when
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)   # sentence case the first letter
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o1 <- e[e$abbrev == "PA", c("state", "abbrev", "ev", "harris", "trump",
                            "other", "winner", "margin")]
names(o1) <- c("state", "abbrev", "electoral votes", "Harris %", "Trump %",
               "other %", "winner", "margin")
o1

## ---- tile-static
over <- abs(e$margin) > CAP
layout(matrix(1:3, ncol = 1), heights = c(1, 1, 0.46))
tile_panel(e$margin, "One square per jurisdiction, all the same size")
tile_panel(e$margin, "The same squares, area proportional to electoral votes",
           by_ev = TRUE)
tile_key("margin: Trump % minus Harris %, in percentage points",
         c(sprintf(paste("Capped at +/-%d: %d of %d jurisdictions lie beyond",
                         "the cap and all read as the same extreme."),
                   CAP, sum(over), nrow(e)),
           sprintf("The largest is %s at %s.",
                   e$state[which.max(abs(e$margin))],
                   pc(e$margin[which.max(abs(e$margin))], 1))))
layout(1)

## ---- tile-d3
rows <- paste(sprintf(
  '{"a":"%s","s":"%s","c":%d,"r":%d,"ev":%d,"m":%.2f,"h":%.2f,"t":%.2f}',
  e$abbrev, e$state, e$col, e$row, e$ev, e$margin, e$harris, e$trump),
  collapse = ",")
cat(sprintf('
<div id="tile" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s],CAP=%d,NOVER=%d,NALL=%d,EVMIN=%d;
const W=760,H=578,M={t:14,r:14,b:64,l:14};
const box=d3.select("#tile");
const bar=box.append("div").attr("style","margin-bottom:6px;font-size:12px");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const nc=d3.max(D,d=>d.c), nr=d3.max(D,d=>d.r);
const cw=Math.min((W-M.l-M.r)/nc,(H-M.t-M.b)/nr);
const x0=(W-cw*nc)/2, y0=M.t;
const col=d3.scaleLinear().domain([-CAP,0,CAP])
  .range(["#2166AC","#F7F7F7","#B2182B"]).clamp(true);
// ---- color legend: without it the map does not carry its encoding ----
const LW=300,LH=12,LX=(W-LW)/2,LY=H-M.b+22;
const grad=svg.append("defs").append("linearGradient").attr("id","tilegrad")
  .attr("x1","0%%").attr("x2","100%%");
d3.range(0,101).forEach(i=>grad.append("stop")
  .attr("offset",i+"%%").attr("stop-color",col(-CAP+i/100*2*CAP)));
svg.append("rect").attr("x",LX).attr("y",LY).attr("width",LW).attr("height",LH)
  .attr("fill","url(#tilegrad)").attr("stroke","#666").attr("stroke-width",0.8);
const lx=d3.scaleLinear().domain([-CAP,CAP]).range([LX,LX+LW]);
d3.range(-CAP,CAP+1,15).forEach(v=>{
  svg.append("line").attr("x1",lx(v)).attr("x2",lx(v)).attr("y1",LY+LH)
    .attr("y2",LY+LH+4).attr("stroke","#666").attr("stroke-width",0.8);
  svg.append("text").attr("x",lx(v)).attr("y",LY+LH+16).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#555")
    .text(v>0?"+"+v:""+v);});
svg.append("text").attr("x",LX-10).attr("y",LY+LH-1).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#2166AC").text("Harris ahead");
svg.append("text").attr("x",LX+LW+10).attr("y",LY+LH-1)
  .attr("font-size","11px").attr("fill","#B2182B").text("Trump ahead");
svg.append("text").attr("x",W/2).attr("y",LY-8).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#555")
  .text("margin: Trump %% minus Harris %% ("+NOVER+" of "+NALL+
        " jurisdictions lie beyond the cap)");
const maxev=d3.max(D,d=>d.ev);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
let mode="equal";
function side(d){ return mode==="equal" ? cw*0.46 : cw*0.46*Math.sqrt(d.ev/maxev); }
function draw(){
  const cells=g.selectAll("rect").data(D).join("rect")
    .attr("fill",d=>col(d.m)).attr("stroke","#fff").attr("stroke-width",1.5)
    .on("mousemove",function(ev,d){
      tip.style("opacity",1).html(
        `<b>${d.s}</b><br>${d.ev} electoral votes<br>`+
        `Harris ${d.h}%%, Trump ${d.t}%%<br>margin ${d.m>0?"+":""}${d.m}`)
        .style("left",Math.min(ev.offsetX+14,W-200)+"px")
        .style("top",(ev.offsetY-10)+"px"); })
    .on("mouseleave",()=>tip.style("opacity",0));
  cells.transition().duration(500)
    .attr("x",d=>x0+(d.c-1)*cw+cw/2-side(d)).attr("y",d=>y0+(d.r-1)*cw+cw/2-side(d))
    .attr("width",d=>2*side(d)).attr("height",d=>2*side(d));
  const labs=g.selectAll("text").data(D).join("text")
    .attr("text-anchor","middle").attr("fill","#222").text(d=>d.a);
  labs.transition().duration(500)
    .attr("x",d=>x0+(d.c-1)*cw+cw/2).attr("y",d=>y0+(d.r-1)*cw+cw/2+4)
    .attr("font-size",d=>Math.min(2*side(d)*0.42,12)+"px")
    .attr("opacity",d=>(mode==="equal"||d.ev>=EVMIN)?1:0);
}
["equal size","sized by electoral votes"].forEach((lab,i)=>{
  bar.append("button").text(lab)
    .attr("style","margin-right:6px;padding:3px 9px;font:inherit;font-size:12px;cursor:pointer")
    .on("click",function(){ mode=i===0?"equal":"ev"; draw(); });
});
draw();
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Color is the margin, capped at plus or minus %d points, so the %d jurisdictions
beyond the cap all read as the same extreme; the largest is %s at %s. Hover for
detail, and use the buttons to give each square an area proportional to its
electoral votes.</p>
', rows, CAP, sum(abs(e$margin) > CAP), nrow(e), EVMIN,
   CAP, sum(abs(e$margin) > CAP),
   e$state[which.max(abs(e$margin))],
   pc(e$margin[which.max(abs(e$margin))], 1)))

## ---- sharemap-static
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(1, 0.42))
tile_panel(d_marg, "colored by margin", lab_cex = 0.55, ttl_cex = 0.70)
tile_panel(d_shar, "colored by Harris vote share", lab_cex = 0.55,
           ttl_cex = 0.70)
tile_key("percentage points from a tie, on one ramp shared by both maps",
         c(sprintf(paste("Left: Trump %% minus Harris %%. Right: 50 minus",
                         "Harris %%. Same ramp, same cap of +/-%d points."),
                   CAP),
           sprintf("Jurisdictions at the cap: %d on the left, %d on the right.",
                   sum(abs(d_marg) > CAP), sum(abs(d_shar) > CAP))))
layout(1)

## ---- sharemap-d3
# Drawn with the shared library, as two choropleths over the equal-weight
# state grid in _lib/geo -- the same squares, read from the same file, that
# the senate chapter draws. Colour is a CLASS, one of the ten shared
# diverging bins, quantised here by dd_ramp_class() on the same +/-CAP the
# static twin caps its ramp at; brief.css owns what each bin looks like in
# each theme. The two maps share one values frame, so they cannot disagree
# about a state -- only about which column they colour by.
gp <- dd_geo_paths("../../_lib/geo/us-grid.geojson", "st")
gp$lab <- gp$id
vals <- data.frame(id = e$abbrev, s = e$state,
                   dm = round(d_marg, 2), ds = round(d_shar, 2),
                   h = e$harris, stringsAsFactors = FALSE)
shmp_tip <- dd_js('function(d){
  return "<b>"+d.s+"</b><br>margin "+DD.fmt.signed2(d.dm)+
    "<br>Harris share "+d.h+"%, i.e. "+DD.fmt.signed2(d.ds)+" from a tie";
}')
cat('<div style="display:flex;gap:12px;margin:1em 0 0.2em">\n<div style="flex:1">\n')
dd_fig("shmp-m", "choropleth", d3 = FALSE,
  geo = dd_geo(gp), title = "colored by margin", titleSize = 22,
  values = cbind(vals, cls = dd_ramp_class(vals$dm, cap = CAP)),
  geoLabels = TRUE, labelSize = 20, labelClass = "lbl halo",
  tip = shmp_tip)
cat('</div>\n<div style="flex:1">\n')
dd_fig("shmp-s", "choropleth", d3 = FALSE,
  geo = dd_geo(gp), title = "colored by Harris vote share", titleSize = 22,
  values = cbind(vals, cls = dd_ramp_class(vals$ds, cap = CAP)),
  geoLabels = TRUE, labelSize = 20, labelClass = "lbl halo",
  tip = shmp_tip)
cat('</div>\n</div>\n')
# The shared key, once, under both maps, from the library's primitives: the
# same DD.rampKey() bins the two maps were quantised onto.
cat(sprintf('
<div class="dd-fig" id="shmp-key"></div>
<script>
(function(){
const CAP=%d,NM=%d,NS=%d;
const f=DD.frame("#shmp-key",{w:760,h:74,m:{t:0,r:0,b:0,l:0}});
f.svg.append("text").attr("class","lbl").attr("x",380).attr("y",12)
  .attr("text-anchor","middle").attr("font-size","11px")
  .text("percentage points from a tie, on one scale shared by both maps");
const it=DD.rampKey(CAP,5,"signed0"),sw=30,gap=2;
const kw=it.length*(sw+gap)-gap,kx=(760-kw)/2,ky=22;
it.forEach(function(d,i){
  f.svg.append("rect").attr("class",d.cls)
    .attr("x",kx+i*(sw+gap)).attr("y",ky).attr("width",sw).attr("height",12);
  f.svg.append("text").attr("class","lbl")
    .attr("x",kx+i*(sw+gap)+(i<5?0:sw)).attr("y",ky+24)
    .attr("text-anchor","middle").attr("font-size","10px").text(d.label);});
f.svg.append("text").attr("class","dem-txt").attr("x",kx-10).attr("y",ky+10)
  .attr("text-anchor","end").attr("font-size","11px").text("Harris ahead");
f.svg.append("text").attr("class","gop-txt").attr("x",kx+kw+10).attr("y",ky+10)
  .attr("font-size","11px").text("Trump ahead");
f.svg.append("text").attr("class","foot").attr("x",380).attr("y",ky+42)
  .attr("text-anchor","middle").attr("font-size","10.5px")
  .text("Jurisdictions at the cap: "+NM+" on the left map, "+NS+" on the right.");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Identical geography, identical data, identical color scale; only the quantity
being colored has changed. The right-hand map is the paler one because a margin
is about %s times the distance of a share from 50. Hover either map.</p>
', CAP, sum(abs(d_marg) > CAP), sum(abs(d_shar) > CAP),
   pc(diff(range(d_marg)) / diff(range(d_shar)), 1)))

## ---- me-ne
o3 <- e[e$abbrev %in% c("ME", "NE"), c("state", "winner", "ev", "margin")]
names(o3) <- c("state", "statewide winner", "electoral votes", "margin")
o3

## ---- close-states
o6 <- cl[order(-cl$ev), c("state", "ev", "harris", "trump", "margin")]
o6$margin <- pc(o6$margin, 2)
names(o6) <- c("state", "electoral votes", "Harris %", "Trump %", "margin")
o6

## ---- quad-static
plot(NA, xlim = c(0, 90), ylim = c(0, 58), las = 1,
     xlab = "margin, absolute value (points)", ylab = "electoral votes")
rect(0, 0, 5, 58, col = adjustcolor("#C41230", alpha.f = 0.07), border = NA)
abline(v = 5, lty = 3, col = "grey50")
text(6.5, 56, "decided by under 5 points", adj = c(0, 0.5), cex = 0.72,
     col = "#C41230")
points(abs(e$margin), e$ev, pch = 19, cex = 0.9,
       col = ifelse(e$close, "#C41230", "#999999"))
text(abs(e$margin)[qi], e$ev[qi], e$abbrev[qi], pos = ifelse(q_left, 2, 4),
     cex = 0.7, col = "grey25")

## ---- quad-d3
lft  <- rep(0L, nrow(e)); lft[qi[q_left]] <- 1L
rows <- paste(sprintf('{"a":"%s","s":"%s","x":%.2f,"ev":%d,"m":%.2f,"L":%d}',
                      e$abbrev, e$state, abs(e$margin), e$ev, e$margin, lft),
              collapse = ",")
cat(sprintf('
<div id="quad" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:18,r:22,b:46,l:52};
const box=d3.select("#quad");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,90]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,58]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(0)).attr("y",M.t).attr("width",x(5)-x(0))
  .attr("height",H-M.b-M.t).attr("fill","#C41230").attr("opacity",0.07);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("margin, absolute value (points)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("electoral votes");
svg.append("text").attr("x",x(5)+8).attr("y",M.t+14).attr("font-size","11px")
  .attr("fill","#C41230").text("decided by under 5 points");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.ev)).attr("r",5)
  .attr("fill",d=>d.x<5?"#C41230":"#999").attr("fill-opacity",0.75)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.s}</b><br>${d.ev} electoral votes<br>margin ${d.m>0?"+":""}${d.m}`)
      .style("left",Math.min(ev.offsetX+14,W-200)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.l").data(D.filter(d=>d.ev>=14||d.x<5)).join("text")
  .attr("class","l").attr("x",d=>d.L?x(d.x)-8:x(d.x)+8).attr("y",d=>y(d.ev)+4)
  .attr("text-anchor",d=>d.L?"end":"start")
  .attr("font-size","10.5px").attr("fill","#444").text(d=>d.a);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The states that decide an election sit in the top-left corner: large, and close.
Hover for detail.</p>
', rows))

## ---- inequality
wy <- st("WY","ev"); ca <- st("CA","ev")
data.frame(
  inequality = c("Small-state bonus", "Winner-take-all"),
  what_it_does = c(
    paste0("Every state gets 2 electors for its senators regardless of size (",
           "WY ", wy, " vs CA ", ca, " electoral votes)"),
    paste0("A plurality takes the whole state, so the losing side of every ",
           "state is erased")),
  who_it_helps = c("Voters in small states, of either party",
                   "Voters in close states, of either party"),
  fixed_by = c("Nothing short of a constitutional amendment",
               "State law — a state could change it tomorrow"))

## ---- prop
data.frame(
  rule = c("Winner-take-all (actual)", "Proportional within each state"),
  Harris = c(sum(e$ev[e$winner == "Harris"]), sum(h_prop)),
  Trump  = c(sum(e$ev[e$winner == "Trump"]),  sum(t_prop)),
  margin = c(abs(sum(e$ev[e$winner == "Trump"]) - sum(e$ev[e$winner == "Harris"])),
             abs(sum(t_prop) - sum(h_prop))))

## ---- propshift-static
par(mar = c(3.6, 4.6, 0.8, 1))
bp <- barplot(dh[dord], border = NA, space = 0.3, las = 1,
              col = ifelse(dh[dord] < 0, "#B2182B", "#2166AC"),
              ylim = c(min(dh) - 6, max(dh) + 7),
              ylab = "electoral votes moved by the rule change", xlab = "")
abline(h = 0, col = "grey35")
bpos <- which(dh[dord] >=  5)
bneg <- which(dh[dord] <= -5)
text(bp[bpos], dh[dord][bpos] + 0.6, e$abbrev[dord][bpos], srt = 90,
     adj = c(0, 0.5), cex = 0.6, col = "grey25")
text(bp[bneg], dh[dord][bneg] - 0.6, e$abbrev[dord][bneg], srt = 90,
     adj = c(1, 0.5), cex = 0.6, col = "grey25")
mtext(sprintf("the %d jurisdictions, ordered by the size of the change",
              nrow(e)), side = 1, line = 1.2, cex = 0.82)
# each annotation sits over the bars it describes: the blue gains on the right,
# the red losses on the left
text(bp[length(bp)], max(dh) + 6,
     sprintf("+%d to Harris, from states she lost", sum(dh[dh > 0])),
     adj = c(1, 0.5), cex = 0.75, col = "#2166AC")
text(bp[1], min(dh) - 4.5,
     sprintf("%d to Trump, from states she carried", -sum(dh[dh < 0])),
     adj = c(0, 0.5), cex = 0.75, col = "#B2182B")

## ---- propshift-d3
rows <- paste(sprintf('{"a":"%s","s":"%s","d":%d,"ev":%d,"hp":%d,"tp":%d,"m":%.2f}',
                      e$abbrev[dord], e$state[dord], dh[dord], e$ev[dord],
                      h_prop[dord], t_prop[dord], e$margin[dord]),
              collapse = ",")
cat(sprintf('
<div id="prsh" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],GAIN=%d,LOSS=%d;
const W=760,H=420,M={t:24,r:20,b:52,l:56};
const box=d3.select("#prsh");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(D.map(d=>d.a)).range([M.l,W-M.r]).padding(0.22);
const ex=d3.extent(D,d=>d.d);
const y=d3.scaleLinear().domain([ex[0]-6,ex[1]+7]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(8));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("electoral votes moved by the rule change");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("the "+D.length+" jurisdictions, ordered by the size of the change");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.a)).attr("width",x.bandwidth())
  .attr("y",d=>d.d>0?y(d.d):y(0)).attr("height",d=>Math.abs(y(d.d)-y(0)))
  .attr("fill",d=>d.d<0?"#B2182B":"#2166AC")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.s}</b> (${d.ev} electoral votes)<br>`+
      `margin ${d.m>0?"+":""}${d.m}<br>proportional split: `+
      `Harris ${d.hp}, Trump ${d.tp}<br>net move: ${d.d>0?"+":""}${d.d} to `+
      (d.d>0?"Harris":d.d<0?"Trump":"nobody"))
      .style("left",Math.min(Math.max(ev.offsetX-120,4),W-300)+"px")
      .style("top",(M.t+2)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#555");
svg.append("g").selectAll("text.l").data(D.filter(d=>Math.abs(d.d)>=5)).join("text")
  .attr("class","l")
  .attr("transform",d=>`translate(${x(d.a)+x.bandwidth()/2},`+
    `${d.d>0?y(d.d)-5:y(d.d)+5}) rotate(-90)`)
  .attr("text-anchor",d=>d.d>0?"start":"end").attr("dy","0.34em")
  .attr("font-size","10px").attr("fill","#444").text(d=>d.a);
svg.append("text").attr("x",W-M.r-4).attr("y",M.t+10).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#2166AC")
  .text("+"+GAIN+" to Harris, from states she lost");
svg.append("text").attr("x",M.l+4).attr("y",H-M.b-6)
  .attr("font-size","11.5px").attr("fill","#B2182B")
  .text(LOSS+" to Trump, from states she carried");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One bar per jurisdiction, ordered by the size of the change. %d of the %d
jurisdictions move by two electoral votes or fewer. Hover for the proportional
split in any state.</p>
', rows, sum(dh[dh > 0]), -sum(dh[dh < 0]), sum(abs(dh) <= 2), nrow(e)))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light tile or square that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#tile text[fill="#222" i],
#shmp text[fill="#222" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: the on-mark chunk pins these fills dark for the dark
# page, so a --paper stroke there would sit dark behind a dark ink, and the
# checker scores the fill against the stroke it touches.
cat('<style>
#quad text[fill="#444" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#tile text[fill="#222" i],
#shmp text[fill="#222" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
