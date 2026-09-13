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
ZP <- polypaths(MZ)                     # the empty land, one path per polygon
SP <- polypaths(MS)                     # state outlines, likewise
HIT <- statepaths(MS)                   # one path per STATE, for hovering
hf  <- names(HIT)
i1  <- match(hf, sb$STATEFP); i2 <- match(hf, zlnd$STATEFP)
cat(paste0('
<div id="zmap" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const Z=', jstr(ZP), ',S=', jstr(SP), ',H_=', jstr(unname(HIT)), ';
const NM=', jstr(sb$state[i1]), ',PO=', jnum(sb$pop[i1]), ',BL=', jnum(sb$blocks[i1]),
',PZ=', jnum(sb$pct_zero[i1]), ',LA=', jnum(round(zlnd$zero_land_sqmi[i2])), ';
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
const TIPCSS=`position:absolute;pointer-events:none;background:#fff;`
 +`border:1px solid #CBD3D8;border-radius:3px;padding:6px 9px;font:11.5px inherit;`
 +`color:#12181D;box-shadow:0 1px 4px rgba(0,0,0,.14);white-space:nowrap;z-index:5`;
const row=(k,v)=>`<div style="display:flex;gap:16px;justify-content:space-between">`
 +`<span style="color:#4E5A63">${k}</span><b>${v}</b></div>`;
const tip=wrap.append("div").attr("style",TIPCSS).style("display","none");
const f=d3.format(",");
svg.append("g").selectAll("path").data(H_).join("path").attr("d",d=>d)
  .attr("fill","transparent").attr("fill-rule","evenodd").style("cursor","pointer")
  .on("mousemove",function(ev,d){
    const i=H_.indexOf(d);
    hi.attr("d",d).style("display",null);
    tip.style("display","block").html(
      `<b style="display:block;margin-bottom:3px">${NM[i]}</b>`+
      row("population",f(PO[i]))+row("census blocks",f(BL[i]))+
      row("blocks with nobody",PZ[i].toFixed(1)+"%")+
      row("unpopulated land",f(LA[i])+" sq mi"));
    const b=wrap.node().getBoundingClientRect();
    let x=ev.clientX-b.left+14, y=ev.clientY-b.top+14;
    const t=tip.node().getBoundingClientRect();
    if(x+t.width>b.width) x=ev.clientX-b.left-t.width-14;
    if(y+t.height>b.height) y=b.height-t.height-4;
    tip.style("left",x+"px").style("top",Math.max(0,y)+"px");
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
# Zero is not a bar here. It is more than eighteen times the tallest of the
# others, and the only ways to seat it beside them -- a logarithmic axis, or a
# broken one -- both break the thing a bar is for, which is that its length is
# its value. So the count that this brief is about is given as a number, and
# the axis is left honest for the blocks that do have somebody on them.
hh <- hp[hp$pop >= 1 & hp$pop <= 100, ]
cat(paste0('
<div id="zhist" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const P=', jnum(hh$pop), ',B=', jnum(hh$blocks), ',TOT=', TOT, ',MED=', MEDINH, ';
const W=700,H=330,M={t:74,r:14,b:40,l:64};
const wrap=d3.select("#zhist");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const f=d3.format(",");
// the headline count, as a number rather than a mark it would dwarf
svg.append("text").attr("x",0).attr("y",30).attr("font-size","27px")
  .attr("font-weight","700").attr("fill","', RED, '").text(f(', ZERO, '));
svg.append("text").attr("x",0).attr("y",50).attr("font-size","11.5px")
  .attr("fill","#4E5A63")
  .text("blocks with nobody — ', pc(PZ), '% of every block in the country");
const x=d3.scaleBand().domain(P).range([M.l,W-M.r]).padding(0.18);
const y=d3.scaleLinear().domain([0,d3.max(B)]).nice().range([H-M.b,M.t]);
svg.append("g").attr("stroke","#76838C").attr("opacity",0.28)
  .selectAll("line").data(y.ticks(4)).join("line")
  .attr("x1",M.l).attr("x2",W-M.r).attr("y1",y).attr("y2",y);
const bars=svg.append("g").selectAll("rect").data(B).join("rect")
  .attr("x",(d,i)=>x(P[i])).attr("width",x.bandwidth())
  .attr("y",d=>y(d)).attr("height",d=>y(0)-y(d))
  .attr("fill","', GREY, '");
// median of the inhabited blocks, the one landmark worth drawing
svg.append("line").attr("x1",x(MED)+x.bandwidth()/2).attr("x2",x(MED)+x.bandwidth()/2)
  .attr("y1",M.t-6).attr("y2",H-M.b).attr("stroke","#76838C")
  .attr("stroke-dasharray","3,3");
svg.append("text").attr("x",x(MED)+x.bandwidth()/2+5).attr("y",M.t+2)
  .attr("font-size","10px").attr("fill","#4E5A63")
  .text("median inhabited block: "+MED+" people");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(P.filter(p=>p%10===0)).tickSizeOuter(0));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(4).tickFormat(f).tickSizeOuter(0));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-4)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
  .text("people counted in the block, 2020");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+H-M.b)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","11px")
  .attr("fill","#4E5A63").text("census blocks");
const TIPCSS=`position:absolute;pointer-events:none;background:#fff;`
 +`border:1px solid #CBD3D8;border-radius:3px;padding:6px 9px;font:11.5px inherit;`
 +`color:#12181D;box-shadow:0 1px 4px rgba(0,0,0,.14);white-space:nowrap;z-index:5`;
const row=(k,v)=>`<div style="display:flex;gap:16px;justify-content:space-between">`
 +`<span style="color:#4E5A63">${k}</span><b>${v}</b></div>`;
const tip=wrap.append("div").attr("style",TIPCSS).style("display","none");
bars.style("cursor","pointer")
  .on("mousemove",function(ev,d){
    const i=B.indexOf(d);
    bars.attr("fill",(q,j)=>j===i?"', RED, '":"', GREY, '");
    tip.style("display","block").html(
      `<b style="display:block;margin-bottom:3px">${P[i]}${P[i]===1?" person":" people"}</b>`+
      row("census blocks",f(d))+row("share of all blocks",(100*d/TOT).toFixed(2)+"%"));
    const b=wrap.node().getBoundingClientRect();
    let px=ev.clientX-b.left+14;
    const t=tip.node().getBoundingClientRect();
    if(px+t.width>b.width) px=ev.clientX-b.left-t.width-14;
    tip.style("left",px+"px").style("top",Math.max(0,ev.clientY-b.top-t.height-12)+"px");
  })
  .on("mouseleave",()=>{tip.style("display","none");bars.attr("fill","', GREY, '");});
})();
</script>
'))

## ---- hist-static
hh <- hp[hp$pop >= 1 & hp$pop <= 100, ]
par(mar = c(4.2, 5.6, 3.6, 0.8))
plot(hh$pop, hh$blocks, type = "h", lend = 1, lwd = 3, col = GREY,
     xlab = "people counted in the block, 2020", ylab = "",
     las = 1, bty = "n", yaxt = "n", xlim = c(0, 100))
at <- pretty(c(0, max(hh$blocks)), 4)
axis(2, at = at, labels = n(at), las = 1)
mtext("census blocks", side = 2, line = 4.4, cex = 0.95)
abline(v = MEDINH, lty = 3, col = "grey40")
text(MEDINH + 2, max(hh$blocks) * 0.92, paste0("median inhabited block: ", MEDINH, " people"),
     adj = 0, cex = 0.72, col = "grey30")
mtext(n(ZERO), side = 3, line = 1.9, adj = 0, cex = 1.5, font = 2, col = RED)
mtext(paste0("blocks with nobody — ", pc(PZ), "% of every block in the country"),
      side = 3, line = 0.6, adj = 0, cex = 0.78, col = "grey30")

## ---- state-d3
ss <- sb[order(sb$pct_zero), ]
cat(paste0('
<div id="zstate" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const N=', jstr(ss$state), ',V=', jnum(ss$pct_zero), ',BL=', jnum(ss$blocks),
',ZB=', jnum(ss$zero_blocks), ';
const rowH=9,M={t:8,r:40,b:24,l:98},W=700,H=M.t+N.length*rowH+M.b;
const wrap=d3.select("#zstate");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:9px inherit");
const f=d3.format(",");
const x=d3.scaleLinear().domain([0,d3.max(V)]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(N).range([M.t,H-M.b]).padding(0.22);
const bars=svg.append("g").selectAll("rect").data(N).join("rect")
  .attr("x",M.l).attr("y",d=>y(d)).attr("height",y.bandwidth())
  .attr("width",(d,i)=>x(V[i])-M.l).attr("fill","', RED, '");
svg.append("g").selectAll("text").data(N).join("text")
  .attr("x",M.l-5).attr("y",d=>y(d)+y.bandwidth()/2).attr("dy","0.34em")
  .attr("text-anchor","end").attr("fill","#4E5A63").text(d=>d);
svg.append("g").selectAll("text").data(N).join("text")
  .attr("x",(d,i)=>x(V[i])+4).attr("y",d=>y(d)+y.bandwidth()/2).attr("dy","0.34em")
  .attr("fill","#76838C").text((d,i)=>V[i].toFixed(1)+"%");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%").tickSizeOuter(0));
const TIPCSS=`position:absolute;pointer-events:none;background:#fff;`
 +`border:1px solid #CBD3D8;border-radius:3px;padding:6px 9px;font:11.5px inherit;`
 +`color:#12181D;box-shadow:0 1px 4px rgba(0,0,0,.14);white-space:nowrap;z-index:5`;
const row=(k,v)=>`<div style="display:flex;gap:16px;justify-content:space-between">`
 +`<span style="color:#4E5A63">${k}</span><b>${v}</b></div>`;
const tip=wrap.append("div").attr("style",TIPCSS).style("display","none");
bars.style("cursor","pointer")
  .on("mousemove",function(ev,d){
    const i=N.indexOf(d);
    bars.attr("opacity",(q,j)=>j===i?1:0.45);
    tip.style("display","block").html(`<b style="display:block;margin-bottom:3px">${d}</b>`+
      row("census blocks",f(BL[i]))+row("blocks with nobody",f(ZB[i]))+
      row("share",V[i].toFixed(2)+"%"));
    const b=wrap.node().getBoundingClientRect();
    const t=tip.node().getBoundingClientRect();
    let px=ev.clientX-b.left+14;
    if(px+t.width>b.width) px=ev.clientX-b.left-t.width-14;
    tip.style("left",px+"px").style("top",Math.max(0,ev.clientY-b.top-t.height/2)+"px");
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
