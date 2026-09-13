# zero-population-blocks-code.R -- chunk bodies for the brief of the same name.
#
# Each `## ---- label` block is the body of the chunk carrying that label in
# the brief. knitr::read_chunk() pairs them up at render time; the brief holds
# the labels and options, this file holds the code. Edit here, not there.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

D <- "data"
dd_derived(c("block_pop_hist.csv", "block_size.csv", "facts.csv", "map_frame.csv",
             "map_states.csv", "map_zero.csv", "state_blocks.csv", "zero_land.csv"))

sb <- read.csv(file.path(D, "derived/state_blocks.csv"),
               colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
hp <- read.csv(file.path(D, "derived/block_pop_hist.csv"), stringsAsFactors = FALSE)
MZ <- read.csv(file.path(D, "derived/map_zero.csv"),   stringsAsFactors = FALSE)
MS <- read.csv(file.path(D, "derived/map_states.csv"),
               colClasses = c(st = "character"), stringsAsFactors = FALSE)
FR <- read.csv(file.path(D, "derived/map_frame.csv"),  stringsAsFactors = FALSE)
zlnd <- read.csv(file.path(D, "derived/zero_land.csv"),
                 colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
# Both fact tables are read with the value column as text, so that a name like
# "North Slope Borough, Alaska" and a number like 9226 can live in one frame.
fc <- rbind(
  read.csv(file.path(D, "derived/facts.csv"),
           colClasses = "character", stringsAsFactors = FALSE),
  read.csv(file.path(D, "derived/block_size.csv"),
           colClasses = "character", stringsAsFactors = FALSE))

n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# Every headline number in the prose comes out of facts.csv, which the build
# wrote from the same tables the figures are drawn from. Nothing is recomputed
# here, so the text and the figures cannot drift apart.
FS <- function(k) {                       # a fact as written
  v <- fc$value[fc$name == k]
  if (!length(v)) stop("no such fact: ", k)
  v
}
FV <- function(k) as.numeric(FS(k))       # the same, as a number
TOT     <- FV("blocks_total")
ZERO    <- FV("blocks_zero")
POP     <- FV("pop_total")
PZ      <- FV("pct_zero")
ZLAND   <- FV("zero_land_sqmi")
ZLBLK   <- FV("blocks_zero_land")
ZWBLK   <- FV("blocks_zero_water")
PCTLAND <- 100 * ZLAND / FV("us_land_sqmi")
MED     <- FV("median_block_pop")
MEDINH  <- FV("median_inhab_pop")
UNDER10 <- FV("pct_under_10")
CAP     <- max(hp$pop)                       # top bucket is "CAP and over"
MOST    <- sb[which.max(sb$pct_zero), ]
LEAST   <- sb[which.min(sb$pct_zero), ]

# States the largest single block would swallow, biggest first. Derived rather
# than named, so the comparison cannot go stale if the file is ever rebuilt.
SMALL   <- sb[sb$land_sqmi < FV("biggest_area_sqmi"), ]
SMALL   <- SMALL[order(-SMALL$land_sqmi), ]

RED  <- "#C41230"     # Carnegie red, the empty blocks
GREY <- "#8d99ae"

# Population bins. Widening toward the tail is what lets the whole range sit
# on one honest axis: binned, the empty blocks are 1.4 times the tallest other
# bar rather than eighteen times it, so nothing has to be logged or broken.
BIN_LO  <- c(0, 1, 5, 10, 20, 50, 100, 250, 500, 1000)
BIN_HI  <- c(0, 4, 9, 19, 49, 99, 249, 499, 999, Inf)
BIN_LAB <- c("0", "1-4", "5-9", "10-19", "20-49", "50-99",
             "100-249", "250-499", "500-999", "1,000+")
BIN_N   <- mapply(function(a, b)
  sum(hp$blocks[hp$pop >= a & hp$pop <= min(b, max(hp$pop))]), BIN_LO, BIN_HI)
BIN_CUM <- round(100 * cumsum(BIN_N) / sum(BIN_N), 1)
BIN_PCT <- round(100 * BIN_N / sum(BIN_N), 1)
stopifnot(sum(BIN_N) == TOT)          # the bins must partition every block

# Render every data.frame in this document as a TABLE, not as code output.
# A data.frame printed the ordinary way comes out as a "##"-prefixed block,
# which reads as machinery rather than as a result.
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- data-table
data.frame(
  Level = c("Nation", "Most empty", "Least empty"),
  Where = c("50 states, DC, Puerto Rico", MOST$state, LEAST$state),
  Blocks = n(c(TOT, MOST$blocks, LEAST$blocks)),
  `Blocks with nobody` = n(c(ZERO, MOST$zero_blocks, LEAST$zero_blocks)),
  Percent = paste0(pc(c(PZ, MOST$pct_zero, LEAST$pct_zero)), "%"),
  check.names = FALSE
)

## ---- geometry-helpers
# One SVG subpath per ring, then all of a polygon's rings joined into a single
# path drawn under an even-odd fill rule. That grouping is not cosmetic: the
# dissolved layer is full of holes -- a populated town inside an empty county
# is an interior ring -- and a ring drawn on its own is painted as fill, which
# turns Nevada into a solid block. Integer coordinates in a deliberately large
# viewBox plus relative line commands keep the whole national layer near a
# megabyte.
onepath <- function(X, Y) {
  dx <- X[-1] - X[-length(X)]; dy <- Y[-1] - Y[-length(Y)]
  seg <- paste0(dx, ifelse(dy < 0, "", ","), dy)
  sep <- c("", ifelse(substr(seg[-1], 1, 1) == "-", "", " "))
  paste0("M", X[1], ",", Y[1], "l", paste0(sep, seg, collapse = ""), "Z")
}
polypaths <- function(d) {
  k <- interaction(d$poly, d$ring, drop = TRUE)
  rings <- vapply(split(d, k), function(z) onepath(z$x, z$y), character(1))
  owner <- vapply(split(d, k), function(z) as.character(z$poly[1]), character(1))
  vapply(split(rings, owner), paste, character(1), collapse = "")
}
statepaths <- function(d) {
  k <- interaction(d$poly, d$ring, drop = TRUE)
  rings <- vapply(split(d, k), function(z) onepath(z$x, z$y), character(1))
  owner <- vapply(split(d, k), function(z) as.character(z$st[1]), character(1))
  vapply(split(rings, owner), paste, character(1), collapse = "")
}
# a JSON array of quoted strings, and one of bare numbers
# One card for all three figures: a white panel with a bold title, ruled
# label/value rows and a muted footer. Emitted once, by the first figure.
TIPCSS <- '<style>
.zpb-tip{position:absolute;pointer-events:none;z-index:6;background:#fff;
  color:#12181D;border-radius:6px;padding:11px 14px 9px;min-width:212px;
  font:12px/1.35 inherit;white-space:nowrap;
  box-shadow:0 6px 22px rgba(0,0,0,.20),0 1px 3px rgba(0,0,0,.12)}
.zpb-tip h4{margin:0 0 7px;font-size:15px;font-weight:700;letter-spacing:-.01em}
.zpb-tip table{border-collapse:collapse;width:100%}
.zpb-tip th{font-weight:400;color:#4E5A63;text-align:left;padding:3px 0;
  font-size:11.5px}
.zpb-tip td{text-align:right;padding:3px 0 3px 20px;font-weight:600;
  font-variant-numeric:tabular-nums}
.zpb-tip tr+tr th,.zpb-tip tr+tr td{border-top:1px solid #E7EAEC}
.zpb-tip .foot{margin:8px -14px -9px;padding:7px 14px;border-top:1px solid #E7EAEC;
  background:#F6F8F9;color:#76838C;font-size:11px;border-radius:0 0 6px 6px;
  white-space:normal}
</style>'
jstr <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")
jnum <- function(x) paste0("[", paste0(x, collapse = ","), "]")
# base-R twin of the same idea: rings separated by NA, subtracted by evenodd
drawpolys <- function(d, col, border = NA, lwd = 0.3) {
  for (g in split(d, d$poly)) {
    r  <- split(g, g$ring)
    xs <- unlist(lapply(r, function(k) c(k$x, NA)))
    ys <- unlist(lapply(r, function(k) c(k$y, NA)))
    polypath(xs[-length(xs)], ys[-length(ys)], col = col,
             border = border, lwd = lwd, rule = "evenodd")
  }
}

## ---- map-d3
ZP  <- polypaths(MZ)
SP  <- polypaths(MS)
HIT <- statepaths(MS)
hf  <- names(HIT)
i1  <- match(hf, sb$STATEFP); i2 <- match(hf, zlnd$STATEFP)
PCTL <- round(100 * zlnd$zero_land_sqmi[i2] / sb$land_sqmi[i1], 1)
cat(paste0(TIPCSS, '
<div id="zmap" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const Z=', jstr(ZP), ',S=', jstr(SP), ',H_=', jstr(unname(HIT)), ';
const NM=', jstr(sb$state[i1]), ',PO=', jnum(sb$pop[i1]), ',BL=', jnum(sb$blocks[i1]),
',PZ=', jnum(sb$pct_zero[i1]), ',LA=', jnum(round(zlnd$zero_land_sqmi[i2])),
',PL=', jnum(PCTL), ';
const W=', FR$w, ',H=', FR$h, ';
const wrap=d3.select("#zmap");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;display:block");
svg.append("g").selectAll("path").data(S).join("path").attr("d",d=>d)
  .attr("fill","#ffffff").attr("fill-rule","evenodd");
svg.append("g").selectAll("path").data(Z).join("path").attr("d",d=>d)
  .attr("fill","', RED, '").attr("fill-rule","evenodd");
svg.append("g").selectAll("path").data(S).join("path").attr("d",d=>d)
  .attr("fill","none").attr("stroke","', GREY, '").attr("stroke-width",1.1);
// The hovered state is outlined rather than repainted: recolouring it would
// overwrite the very thing the map is drawn to show.
const hi=svg.append("path").attr("fill","none").attr("stroke","#111")
  .attr("stroke-width",3).attr("pointer-events","none").style("display","none");
const tip=wrap.append("div").attr("class","zpb-tip").style("display","none");
const f=d3.format(",");
const card=(t,rows,foot)=>`<h4>${t}</h4><table>`+
  rows.map(r=>`<tr><th>${r[0]}</th><td>${r[1]}</td></tr>`).join("")+
  `</table>`+(foot?`<div class="foot">${foot}</div>`:"");
const place=(ev)=>{
  const b=wrap.node().getBoundingClientRect(), t=tip.node().getBoundingClientRect();
  let x=ev.clientX-b.left+16, y=ev.clientY-b.top+16;
  if(x+t.width>b.width) x=ev.clientX-b.left-t.width-16;
  if(y+t.height>b.height) y=Math.max(0,ev.clientY-b.top-t.height-16);
  tip.style("left",x+"px").style("top",y+"px");
};
svg.append("g").selectAll("path").data(H_).join("path").attr("d",d=>d)
  .attr("fill","transparent").attr("fill-rule","evenodd").style("cursor","pointer")
  .on("mousemove",function(ev,d){
    const i=H_.indexOf(d);
    hi.attr("d",d).style("display",null);
    tip.style("display","block").html(card(NM[i],[
      ["population",f(PO[i])],
      ["census blocks",f(BL[i])],
      ["blocks with nobody",PZ[i].toFixed(1)+"%"],
      ["unpopulated land",f(LA[i])+" sq mi"],
      ["share of state land",PL[i].toFixed(1)+"%"]],
      "Land area only; blocks that are all water are not drawn."));
    place(ev);
  })
  .on("mouseleave",()=>{tip.style("display","none");hi.style("display","none");});
})();
</script>
'))

## ---- map-static
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, FR$w), ylim = c(FR$h, 0), asp = 1,
     axes = FALSE, xlab = "", ylab = "")
drawpolys(MS, "#ffffff")
drawpolys(MZ, RED)
drawpolys(MS, NA, border = GREY, lwd = 0.3)

## ---- hist-d3
# Binned, so the whole range fits one axis that starts at zero. Bars are counts
# and their lengths are those counts; the panel below is the running share,
# which is what answers "how small is a block, usually".
cat(paste0('
<div id="zhist" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const L=', jstr(BIN_LAB), ',N=', jnum(BIN_N), ',P=', jnum(BIN_PCT),
',C=', jnum(BIN_CUM), ',TOT=', TOT, ';
const W=700,HT=250,HB=120,GAP=34,H=HT+GAP+HB,M={l:64,r:16,t:12,b:26};
const wrap=d3.select("#zhist");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const f=d3.format(",");
const x=d3.scaleBand().domain(L).range([M.l,W-M.r]).padding(0.2);
const y=d3.scaleLinear().domain([0,d3.max(N)]).nice().range([HT-M.b,M.t]);
svg.append("g").attr("stroke","#76838C").attr("opacity",0.28)
  .selectAll("line").data(y.ticks(4)).join("line")
  .attr("x1",M.l).attr("x2",W-M.r).attr("y1",y).attr("y2",y);
const bars=svg.append("g").selectAll("rect").data(L).join("rect")
  .attr("x",d=>x(d)).attr("width",x.bandwidth())
  .attr("y",(d,i)=>y(N[i])).attr("height",(d,i)=>y(0)-y(N[i]))
  .attr("fill",(d,i)=>i===0?"', RED, '":"', GREY, '");
svg.append("g").attr("transform","translate(0,"+(HT-M.b)+")")
  .call(d3.axisBottom(x).tickSizeOuter(0));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(4).tickFormat(f).tickSizeOuter(0));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+HT-M.b)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","11px")
  .attr("fill","#4E5A63").text("census blocks");
// running share
const y2=d3.scaleLinear().domain([0,100]).range([H-M.b,HT+GAP]);
svg.append("g").attr("stroke","#76838C").attr("opacity",0.28)
  .selectAll("line").data([0,50,100]).join("line")
  .attr("x1",M.l).attr("x2",W-M.r).attr("y1",y2).attr("y2",y2);
const pts=L.map((d,i)=>[x(d)+x.bandwidth()/2,y2(C[i])]);
svg.append("path").attr("fill","none").attr("stroke","', RED, '")
  .attr("stroke-width",2)
  .attr("d",d3.line()(pts));
svg.append("g").selectAll("circle").data(L).join("circle")
  .attr("cx",(d,i)=>pts[i][0]).attr("cy",(d,i)=>pts[i][1]).attr("r",3)
  .attr("fill","', RED, '");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickSizeOuter(0));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y2).tickValues([0,50,100]).tickFormat(d=>d+"%").tickSizeOuter(0));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(HT+GAP+H-M.b)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","11px")
  .attr("fill","#4E5A63").text("running share");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-2)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
  .text("people counted in the block, 2020");
const tip=wrap.append("div").attr("class","zpb-tip").style("display","none");
const card=(t,rows)=>`<h4>${t}</h4><table>`+
  rows.map(r=>`<tr><th>${r[0]}</th><td>${r[1]}</td></tr>`).join("")+`</table>`;
// one hit target per bin, spanning both panels
svg.append("g").selectAll("rect.hit").data(L).join("rect")
  .attr("x",d=>x(d)-x.step()*0.1).attr("y",M.t)
  .attr("width",x.step()).attr("height",H-M.b-M.t)
  .attr("fill","transparent").style("cursor","pointer")
  .on("mousemove",function(ev,d){
    const i=L.indexOf(d);
    bars.attr("opacity",(q,j)=>j===i?1:0.45);
    tip.style("display","block").html(card(
      d==="0"?"Blocks with nobody":d+" people",
      [["census blocks",f(N[i])],["share of all blocks",P[i].toFixed(1)+"%"],
       ["this bin and below",C[i].toFixed(1)+"%"]]));
    const b=wrap.node().getBoundingClientRect(), t=tip.node().getBoundingClientRect();
    let px=ev.clientX-b.left+16;
    if(px+t.width>b.width) px=ev.clientX-b.left-t.width-16;
    tip.style("left",px+"px")
       .style("top",Math.max(0,ev.clientY-b.top-t.height-14)+"px");
  })
  .on("mouseleave",()=>{tip.style("display","none");bars.attr("opacity",1);});
})();
</script>
'))

## ---- hist-static
op <- par(mfrow = c(2, 1), mar = c(2.2, 5.6, 0.6, 0.8), oma = c(2.6, 0, 0, 0))
bp <- barplot(BIN_N, names.arg = BIN_LAB, col = ifelse(seq_along(BIN_N) == 1, RED, GREY),
              border = NA, las = 1, yaxt = "n", cex.names = 0.58, space = 0.25)
at <- pretty(c(0, max(BIN_N)), 4)
axis(2, at = at, labels = n(at), las = 1, cex.axis = 0.8)
mtext("census blocks", side = 2, line = 4.4, cex = 0.8)
par(mar = c(2.2, 5.6, 1.4, 0.8))
plot(bp, BIN_CUM, type = "o", pch = 19, cex = 0.7, lwd = 2, col = RED,
     ylim = c(0, 100), xlim = range(bp) + c(-0.5, 0.5), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = bp, labels = BIN_LAB, las = 1, cex.axis = 0.58, tick = FALSE)
axis(2, at = c(0, 50, 100), labels = paste0(c(0, 50, 100), "%"), las = 1, cex.axis = 0.8)
mtext("running share", side = 2, line = 4.4, cex = 0.8)
mtext("people counted in the block, 2020", side = 1, outer = TRUE, line = 1, cex = 0.85)
par(op)

## ---- state-d3
ss <- sb[order(sb$pct_zero), ]
cat(paste0('
<div id="zstate" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const N=', jstr(ss$state), ',V=', jnum(ss$pct_zero), ',BL=', jnum(ss$blocks),
',ZB=', jnum(ss$zero_blocks), ';
const rowH=11,M={t:8,r:40,b:24,l:84},W=700,H=M.t+N.length*rowH+M.b;
const wrap=d3.select("#zstate");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:7px inherit");
const f=d3.format(",");
const x=d3.scaleLinear().domain([0,d3.max(V)]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(N).range([M.t,H-M.b]).padding(0.24);
const bars=svg.append("g").selectAll("rect").data(N).join("rect")
  .attr("x",M.l).attr("y",d=>y(d)).attr("height",y.bandwidth())
  .attr("width",(d,i)=>x(V[i])-M.l).attr("fill","', RED, '");
svg.append("g").selectAll("text").data(N).join("text")
  .attr("x",M.l-5).attr("y",d=>y(d)+y.bandwidth()/2).attr("dy","0.34em")
  .attr("text-anchor","end").attr("font-size","7px").attr("fill","#4E5A63").text(d=>d);
svg.append("g").selectAll("text").data(N).join("text")
  .attr("x",(d,i)=>x(V[i])+4).attr("y",d=>y(d)+y.bandwidth()/2).attr("dy","0.34em")
  .attr("font-size","7px").attr("fill","#76838C").text((d,i)=>V[i].toFixed(1)+"%");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").attr("font-size","9px")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%").tickSizeOuter(0));
const tip=wrap.append("div").attr("class","zpb-tip").style("display","none");
const card=(t,rows)=>`<h4>${t}</h4><table>`+
  rows.map(r=>`<tr><th>${r[0]}</th><td>${r[1]}</td></tr>`).join("")+`</table>`;
bars.style("cursor","pointer")
  .on("mousemove",function(ev,d){
    const i=N.indexOf(d);
    bars.attr("opacity",(q,j)=>j===i?1:0.45);
    tip.style("display","block").html(card(d,[
      ["census blocks",f(BL[i])],["blocks with nobody",f(ZB[i])],
      ["share",V[i].toFixed(2)+"%"]]));
    const b=wrap.node().getBoundingClientRect(), t=tip.node().getBoundingClientRect();
    let px=ev.clientX-b.left+16;
    if(px+t.width>b.width) px=ev.clientX-b.left-t.width-16;
    tip.style("left",px+"px")
       .style("top",Math.max(0,ev.clientY-b.top-t.height/2)+"px");
  })
  .on("mouseleave",()=>{tip.style("display","none");bars.attr("opacity",1);});
})();
</script>
'))

## ---- state-static
ss <- sb[order(sb$pct_zero), ]
par(mar = c(4.4, 7.2, 0.4, 1.6))
barplot(ss$pct_zero, horiz = TRUE, col = RED, border = NA, las = 1,
        names.arg = ss$state, cex.names = 0.42, cex.axis = 0.75,
        xlab = "", space = 0.28)
mtext("blocks with nobody living on them (%)", side = 1, line = 2.6, cex = 0.95)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
