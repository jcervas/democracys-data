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
dd_derived(c("block_pop_hist.csv", "block_size.csv", "dc_blocks.csv",
             "dc_facts.csv", "dc_frame.csv", "facts.csv", "map_frame.csv",
             "ksut_dots.csv", "map_counties.csv", "map_overview.csv", "map_states.csv",
             "map_zero.csv", "state_bbox.csv", "state_blocks.csv",
             "zero_land.csv"))

sb <- read.csv(file.path(D, "derived/state_blocks.csv"),
               colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
sb$dens <- sb$pop / sb$land_sqmi          # people per square mile of land
hp <- read.csv(file.path(D, "derived/block_pop_hist.csv"), stringsAsFactors = FALSE)
MZ <- read.csv(file.path(D, "derived/map_zero.csv"),
               colClasses = c(st = "character"), stringsAsFactors = FALSE)
bx <- read.csv(file.path(D, "derived/state_bbox.csv"),
               colClasses = c(st = "character"), stringsAsFactors = FALSE)
MOV <- read.csv(file.path(D, "derived/map_overview.csv"), stringsAsFactors = FALSE)
DOT <- read.csv(file.path(D, "derived/ksut_dots.csv"),
                colClasses = c(st = "character"), stringsAsFactors = FALSE)
MCT <- read.csv(file.path(D, "derived/map_counties.csv"),
                colClasses = c(st = "character"), stringsAsFactors = FALSE)
MS <- read.csv(file.path(D, "derived/map_states.csv"),
               colClasses = c(st = "character"), stringsAsFactors = FALSE)
FR <- read.csv(file.path(D, "derived/map_frame.csv"),  stringsAsFactors = FALSE)
zlnd <- read.csv(file.path(D, "derived/zero_land.csv"),
                 colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
# Both fact tables are read with the value column as text, so that a name like
# "North Slope Borough, Alaska" and a number like 9226 can live in one frame.
# Joined here rather than where sb is read, because zlnd is not loaded yet at
# that point. STATEFP is character on both sides; a partial join would be a
# silent lie, so it is asserted.
sb <- merge(sb, zlnd, by = "STATEFP")
stopifnot(nrow(sb) == 52)
sb$pct_land <- 100 * sb$zero_land_sqmi / sb$land_sqmi
sb$per_block <- sb$pop / sb$blocks              # average residents in a block
sb$acres_per_block <- sb$land_sqmi * 640 / sb$blocks

fc <- do.call(rbind, lapply(
  c("facts.csv", "block_size.csv", "dc_facts.csv"),
  function(f) read.csv(file.path(D, "derived", f),
                       colClasses = "character", stringsAsFactors = FALSE)))
DC  <- read.csv(file.path(D, "derived/dc_blocks.csv"), stringsAsFactors = FALSE)
DCF <- read.csv(file.path(D, "derived/dc_frame.csv"),  stringsAsFactors = FALSE)

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

# The largest states whose combined land still fits inside the empty land, so
# the comparison in the opening is computed and cannot go stale.
.o    <- sb[order(-sb$land_sqmi), ]
NBIG  <- sum(cumsum(.o$land_sqmi) <= ZLAND)
BIGST <- .o$state[seq_len(NBIG)]
andlist <- function(x) if (length(x) < 2) x else
  paste0(paste(utils::head(x, -1), collapse = ", "), " and ", utils::tail(x, 1))
ST <- function(code) sb[sb$usps == code, ]      # one state's row, by postal code

# Kansas and Utah are the same size and hold nearly the same number of people,
# so the difference between them is not density. How much more of Utah has
# nobody on it, and the largest state that gap would swallow whole -- both
# computed, so the sentence in the text cannot go stale.
KSUT  <- ST("UT")$zero_land_sqmi - ST("KS")$zero_land_sqmi
.swal <- sb[sb$land_sqmi < KSUT, ]
GAPST <- .swal$state[which.max(.swal$land_sqmi)]
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
# County borders are open lines, not rings: closing them with Z would draw a
# segment straight back to the start of each border.
linepaths <- function(d) {
  k <- interaction(d$poly, d$ring, drop = TRUE)
  segs <- vapply(split(d, k), function(z) {
    p <- onepath(z$x, z$y)
    substr(p, 1, nchar(p) - 1)                 # drop the trailing Z
  }, character(1))
  owner <- vapply(split(d, k), function(z) as.character(z$poly[1]), character(1))
  vapply(split(segs, owner), paste, character(1), collapse = "")
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
  color:#12181D;border-radius:7px;padding:0;white-space:nowrap;
  font:12px/1.4 inherit;box-shadow:0 8px 28px rgba(0,0,0,.22),0 1px 3px rgba(0,0,0,.12)}
  .zpb-tip h4{margin:0;padding:10px 14px 8px;font-size:15.5px;font-weight:700;
  letter-spacing:-.012em;line-height:1.2}
/* The rules are on the ROW, drawn edge to edge, rather than on the cells --
   a border on a padded cell stops where the padding starts and the line
   arrives short of the card. */
.zpb-tip table{border-collapse:collapse;table-layout:auto}
.zpb-tip tr{border-top:1px solid #E7EAEC}
  .zpb-tip th{font-weight:400;color:#4E5A63;text-align:left;
  padding:5px 20px 5px 14px;font-size:11.5px;white-space:nowrap}
  .zpb-tip td{text-align:right;padding:5px 14px 5px 0;font-weight:600;
  font-size:12.5px;white-space:nowrap;font-variant-numeric:tabular-nums lining-nums}
.zpb-tip .foot{padding:7px 15px 8px;border-top:1px solid #E7EAEC;
  background:#F6F8F9;color:#76838C;font-size:11px;line-height:1.35;
  border-radius:0 0 7px 7px}
.zmap-btn{position:absolute;top:0;font:12px inherit;padding:5px 11px;
  border:1px solid #CBD3D8;border-radius:4px;background:#fff;color:#12181D;
  cursor:pointer;z-index:4;user-select:none}
.zmap-btn:hover{background:#F1F4F6}
.zmap-icon{padding:4px 8px;font-size:15px;line-height:1.1}
#zmap:fullscreen{background:#fff;display:flex;align-items:center;
  justify-content:center;padding:0}
#zmap:fullscreen svg{max-height:100vh;max-width:100vw;width:auto;height:auto}
@media print{.zmap-btn{display:none}}
</style>\n'
# A share cannot be negative, and the fitted line goes below zero before it
# reaches the dense end. Truncate it where it crosses, rather than letting it
# run out of the panel and across the axis.
fitline <- function(m) {
  b <- coef(m); xs <- range(log10(sb$dens))
  y <- b[1] + b[2] * xs
  at <- function(v) (v - b[1]) / b[2]          # the x where the fit reaches v
  for (i in 1:2) {
    if (y[i] < 0)   { xs[i] <- at(0);   y[i] <- 0 }
    if (y[i] > 100) { xs[i] <- at(100); y[i] <- 100 }
  }
  list(fx = 10^xs, fy = as.numeric(y))
}
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
SP  <- polypaths(MS)
HIT <- statepaths(MS)
OVP <- polypaths(MOV)                           # the country, drawn coarse
BYS <- lapply(split(MZ,  MZ$st),  polypaths)    # per state, drawn fine
CTY <- lapply(split(MCT, MCT$st), linepaths)    # county lines, per state
hf  <- names(HIT)
i1  <- match(hf, sb$STATEFP); i2 <- match(hf, zlnd$STATEFP)
BX  <- bx[match(hf, bx$st), ]
jarr <- function(keys, lst) paste0("[", paste(vapply(keys, function(k)
  if (is.null(lst[[k]])) "[]" else jstr(unname(lst[[k]])), character(1)),
  collapse = ","), "]")
cat(paste0(TIPCSS, '
<div id="zmap" style="position:relative;margin:1em 0">
<div id="zmap-back" class="zmap-btn" style="left:0;display:none">&#8592; the whole country</div>
<div id="zmap-full" class="zmap-btn zmap-icon" style="right:0" title="Full screen" aria-label="Full screen">&#9974;</div>
</div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=', jstr(SP), ',H_=', jstr(unname(HIT)), ',OV=', jstr(OVP), ';
// The fine geometry rides along as STRINGS. Nothing here becomes an element
// until a state is clicked: the country needs 134,000 vertices to draw, and
// laying out the other 999,000 at load is work for a view nobody is on.
const ZS=', jarr(hf, BYS), ',CT=', jarr(hf, CTY), ';
const BB=', paste0("[", paste(sprintf("[%d,%d,%d,%d]", BX$x0, BX$y0, BX$x1, BX$y1),
    collapse = ","), "]"), ';
const NM=', jstr(sb$state[i1]), ',PO=', jnum(sb$pop[i1]), ',BL=', jnum(sb$blocks[i1]),
',PZ=', jnum(sb$pct_zero[i1]), ',LA=', jnum(round(zlnd$zero_land_sqmi[i2])),
',PL=', jnum(round(100 * zlnd$zero_land_sqmi[i2] / sb$land_sqmi[i1], 1)),
',AR=', jnum(round(sb$land_sqmi[i1])), ',DN=', jnum(round(sb$dens[i1], 1)), ';
const W=', FR$w, ',H=', FR$h, ';
const wrap=d3.select("#zmap"), back=d3.select("#zmap-back");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;display:block;cursor:pointer");
const g=svg.append("g");
const land=g.append("g").selectAll("path").data(H_).join("path").attr("d",d=>d)
  .attr("fill","#ffffff").attr("fill-rule","evenodd");
const coarse=g.append("g").selectAll("path").data(OV).join("path").attr("d",d=>d)
  .attr("fill","', RED, '").attr("fill-rule","evenodd");
const gcty=g.append("g"), gfine=g.append("g");
const border=g.append("g").selectAll("path").data(S).join("path").attr("d",d=>d)
  .attr("fill","none").attr("stroke","', GREY, '").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
const tip=wrap.append("div").attr("class","zpb-tip").style("display","none");
const f=d3.format(",");
const card=(t,rows,foot)=>`<h4>${t}</h4><table>`+
  rows.map(r=>`<tr><th>${r[0]}</th><td>${r[1]}</td></tr>`).join("")+`</table>`+
  (foot?`<div class="foot">${foot}</div>`:"");
let sel=-1;
const zoom=d3.zoom().scaleExtent([1,80]).on("zoom",ev=>g.attr("transform",ev.transform));
svg.call(zoom).on("wheel.zoom",null);
function show(i){
  sel=i;
  land.attr("fill",(d,j)=>i<0||j===i?"#ffffff":"#E4E7E9");
  coarse.style("display",i<0?null:"none");
  gfine.selectAll("path").remove(); gcty.selectAll("path").remove();
  if(i>=0){
    gcty.selectAll("path").data(CT[i]).join("path").attr("d",d=>d)
      .attr("fill","none").attr("stroke","#C3CBD1").attr("stroke-width",0.5)
      .attr("vector-effect","non-scaling-stroke");
    gfine.selectAll("path").data(ZS[i]).join("path").attr("d",d=>d)
      .attr("fill","', RED, '").attr("fill-rule","evenodd");
  }
  back.style("display",i<0?"none":"block");
  const t=i<0?d3.zoomIdentity
    :(()=>{const[x0,y0,x1,y1]=BB[i],k=Math.min(80,0.92/Math.max((x1-x0)/W,(y1-y0)/H));
           return d3.zoomIdentity.translate(W/2,H/2).scale(k)
             .translate(-(x0+x1)/2,-(y0+y1)/2);})();
  svg.transition().duration(760).call(zoom.transform,t);
}
svg.append("g").selectAll("path").data(H_).join("path").attr("d",d=>d)
  .attr("fill","transparent").attr("fill-rule","evenodd")
  .each(function(){ g.node().appendChild(this); })
  .on("click",function(ev,d){ ev.stopPropagation(); const i=H_.indexOf(d);
     tip.style("display","none"); show(i===sel?-1:i); })
  .on("mousemove",function(ev,d){
    const i=H_.indexOf(d);
    if(sel>=0&&i!==sel) return;
    tip.style("display","block").html(card(NM[i],[
      ["Population",f(PO[i])],
      ["Land area",f(AR[i])+" sq mi"],
      ["Land with no residents",f(LA[i])+" sq mi ("+PL[i].toFixed(1)+"%)"],
      ["Census blocks",f(BL[i])],
      ["Density",f(DN[i])+" per sq mi"]],
      sel>=0?"County lines shown for bearings. Land area only; blocks that are all water are not drawn."
            :"Click to zoom to "+NM[i]+"."));
    const b=wrap.node().getBoundingClientRect(), t=tip.node().getBoundingClientRect();
    let x=ev.clientX-b.left+16, y=ev.clientY-b.top+16;
    if(x+t.width>b.width) x=ev.clientX-b.left-t.width-16;
    if(y+t.height>b.height) y=Math.max(0,ev.clientY-b.top-t.height-16);
    tip.style("left",x+"px").style("top",y+"px");
  })
  .on("mouseleave",()=>tip.style("display","none"));
svg.on("click",()=>show(-1));
back.on("click",()=>show(-1));
const full=d3.select("#zmap-full");
full.on("click",ev=>{ ev.stopPropagation();
  const el=wrap.node();
  if(document.fullscreenElement) document.exitFullscreen();
  else if(el.requestFullscreen) el.requestFullscreen();
});
d3.select(document).on("fullscreenchange.zmap",()=>{
  const on=!!document.fullscreenElement;
  full.html(on?"&#10005;":"&#9974;")
      .attr("title",on?"Exit full screen":"Full screen")
      .attr("aria-label",on?"Exit full screen":"Full screen");
});
d3.select(window).on("keydown.zmap",ev=>{ if(ev.key==="Escape") show(-1); });
})();
</script>
'))

## ---- map-static
# Print gets the overview layer: it is the national view, and the per-state
# geometry exists for a zoom that paper does not have.
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, FR$w), ylim = c(FR$h, 0), asp = 1,
     axes = FALSE, xlab = "", ylab = "")
drawpolys(MS,  "#ffffff")
drawpolys(MOV, RED)
drawpolys(MS,  NA, border = GREY, lwd = 0.3)

## ---- ksut-prep
# Kansas and Utah, four panels on ONE scale: where the people are on top,
# where nobody lives underneath. The pairing is the argument -- a scatter can
# say that density and emptiness come apart, and this shows what that looks
# like on the ground.
#
# Every length here is in FRAME units, the same units the geometry is in, so
# type sizes are in the thousands: an SVG viewBox scales its own font sizes,
# and a 15-unit label in a 10,000-unit frame renders at about one pixel.
KU  <- c("20", "49")
KUN <- vapply(KU, function(f) sb$state[sb$STATEFP == f], character(1))
KUS <- function(f, col) sb[[col]][sb$STATEFP == f]
.bb <- lapply(KU, function(f) { a <- MS[MS$st == f, ]
  c(min(a$x), min(a$y), max(a$x), max(a$y)) })
PAD  <- 140
.w   <- vapply(.bb, function(b) b[3] - b[1], 0) + PAD
RH   <- max(vapply(.bb, function(b) b[4] - b[2], 0)) + PAD
GAPY <- 620                                    # room for the second row label
PWT  <- sum(.w); PHT <- 2 * RH + GAPY
.ox  <- c(0, cumsum(.w)[-length(.w)])
.sh  <- function(d, i, row) { b <- .bb[[i]]
  d$x <- d$x + .ox[i] - b[1] + PAD / 2
  d$y <- d$y - b[2] + (RH - (b[4] - b[2])) / 2 + (row - 1) * (RH + GAPY)
  d }
KUP <- lapply(seq_along(KU), function(i) {
  f <- KU[i]
  list(dot = .sh(DOT[DOT$st == f, ], i, 1),
       zt  = polypaths(.sh(MZ[MZ$st == f, ], i, 2)),
       s1  = polypaths(.sh(MS[MS$st == f, ], i, 1)),
       s2  = polypaths(.sh(MS[MS$st == f, ], i, 2)),
       cx  = .ox[i] + .w[i] / 2)
})
KUL1 <- vapply(KU, function(f) sprintf("%s · %s sq mi", sb$state[sb$STATEFP == f],
  n(KUS(f, "land_sqmi"))), character(1))
KUL2 <- vapply(KU, function(f) sprintf("%s people · %s per sq mi",
  n(KUS(f, "pop")), pc(KUS(f, "dens"), 0)), character(1))
KUL3 <- vapply(KU, function(f) sprintf("%s%% of its land has no residents",
  pc(KUS(f, "pct_land"))), character(1))
KUL4 <- vapply(KU, function(f) sprintf("%s blocks · %s acres each",
  n(KUS(f, "blocks")), n(KUS(f, "acres_per_block"))), character(1))

## ---- ksut-d3
cat(paste0('
<div id="zksut" style="margin:1em 0"></div>
<script>
(function(){
const P=[', paste(vapply(seq_along(KU), function(i) paste0(
  '{dx:', jnum(KUP[[i]]$dot$x), ',dy:', jnum(KUP[[i]]$dot$y),
  ',z:', jstr(KUP[[i]]$zt), ',s1:', jstr(KUP[[i]]$s1), ',s2:', jstr(KUP[[i]]$s2),
  ',cx:', round(KUP[[i]]$cx), '}'), character(1)), collapse = ","), '];
const L1=', jstr(unname(KUL1)), ',L2=', jstr(unname(KUL2)),
',L3=', jstr(unname(KUL3)), ',L4=', jstr(unname(KUL4)), ';
const PWT=', PWT, ',RH=', RH, ',GAPY=', GAPY, ',PHT=', PHT, ';
const TOP=950,BOT=560;
const svg=d3.select("#zksut").append("svg")
  .attr("viewBox","0 "+(-TOP)+" "+PWT+" "+(PHT+TOP+BOT))
  .attr("style","max-width:100%;height:auto;display:block");
const T=(x,y,txt,sz,w,col,anch)=>svg.append("text").attr("x",x).attr("y",y)
  .attr("text-anchor",anch||"middle").attr("font-weight",w||400)
  .attr("fill",col||"#12181D").attr("font-size",sz).text(txt);
P.forEach((p,i)=>{
  svg.append("g").selectAll("path").data(p.s1).join("path").attr("d",d=>d)
    .attr("fill","#F4F6F7").attr("fill-rule","evenodd")
    .attr("stroke","', GREY, '").attr("stroke-width",7);
  svg.append("g").selectAll("circle").data(p.dx).join("circle")
    .attr("cx",d=>d).attr("cy",(d,j)=>p.dy[j]).attr("r",8)
    .attr("fill","#12181D").attr("fill-opacity",0.6);
  svg.append("g").selectAll("path").data(p.s2).join("path").attr("d",d=>d)
    .attr("fill","#ffffff").attr("fill-rule","evenodd");
  svg.append("g").selectAll("path").data(p.z).join("path").attr("d",d=>d)
    .attr("fill","', RED, '").attr("fill-rule","evenodd");
  svg.append("g").selectAll("path").data(p.s2).join("path").attr("d",d=>d)
    .attr("fill","none").attr("stroke","', GREY, '").attr("stroke-width",7);
  T(p.cx,-600,L1[i],235,700);
  T(p.cx,-400,L2[i],180,400,"#4E5A63");
  T(p.cx,PHT+250,L3[i],195,700,"', RED, '");
  T(p.cx,PHT+450,L4[i],175,400,"#4E5A63");
});
T(0,-60,"WHERE THE PEOPLE ARE \\u2014 one dot for 500 residents",180,700,"#4E5A63","start");
T(0,RH+GAPY-110,"WHERE NOBODY LIVES \\u2014 blocks with no residents",180,700,"#4E5A63","start");
})();
</script>
'))

## ---- ksut-static
# ONE plot, not a grid of panels: with mfrow and asp = 1 R fits each panel to
# its own limits, and two states of different proportions would then come out
# at two different scales, destroying the only claim this figure makes.
par(mar = c(3.0, 0.3, 3.0, 0.3))
plot(NA, xlim = c(0, PWT), ylim = c(PHT, 0), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
U <- PHT / 100                                  # one percent of the figure
for (i in seq_along(KU)) {
  f <- KU[i]; p <- KUP[[i]]
  s1 <- .sh(MS[MS$st == f, ], i, 1); s2 <- .sh(MS[MS$st == f, ], i, 2)
  drawpolys(s1, "#F4F6F7", border = GREY, lwd = 0.5)
  points(p$dot$x, p$dot$y, pch = 16, cex = 0.085, col = "#12181D99")
  drawpolys(s2, "#ffffff")
  drawpolys(.sh(MZ[MZ$st == f, ], i, 2), RED)
  drawpolys(s2, NA, border = GREY, lwd = 0.5)
  text(p$cx, -5.0 * U, KUL1[i], font = 2, cex = 0.60, xpd = NA)
  text(p$cx, -3.4 * U, KUL2[i], cex = 0.50, col = "#4E5A63", xpd = NA)
  text(p$cx, PHT + 2.1 * U, KUL3[i], font = 2, cex = 0.52, col = RED, xpd = NA)
  text(p$cx, PHT + 3.7 * U, KUL4[i], cex = 0.46, col = "#4E5A63", xpd = NA)
}
text(0, -0.6 * U, "WHERE THE PEOPLE ARE — one dot for 500 residents",
     adj = 0, cex = 0.48, col = "#4E5A63", font = 2, xpd = NA)
text(0, RH + GAPY - 0.9 * U, "WHERE NOBODY LIVES — blocks with no residents",
     adj = 0, cex = 0.48, col = "#4E5A63", font = 2, xpd = NA)

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
      [["Census blocks",f(N[i])],["Share of all blocks",P[i].toFixed(1)+"%"],
       ["This bin and below",C[i].toFixed(1)+"%"]]));
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
      ["Share of its blocks",V[i].toFixed(2)+"%"],
      ["Blocks with no residents",f(ZB[i])],
      ["Census blocks",f(BL[i])]]));
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

## ---- dens-d3
# Two panels on one x. Land leads because land is the thing worth knowing; the
# block panel is underneath as the contrast, and the contrast is the finding.
PAN <- list(list(y = 100 - sb$pct_land, lab = "% of land with residents on it",
                 r = -FV("land_r_log")),
            list(y = 100 - sb$pct_zero, lab = "% of blocks with residents",
                 r = -FV("dens_r_log")))
mk <- function(v) {
  m <- lm(v ~ log10(sb$dens)); rs <- resid(m)
  k <- unique(c(order(-v)[1:3], order(v)[1:2], which.max(sb$dens),
                which.min(sb$dens), order(-abs(rs))[1:3]))
  c(list(show = as.integer(seq_len(nrow(sb)) %in% k)), fitline(m))
}
A <- mk(PAN[[1]]$y); B <- mk(PAN[[2]]$y)
cat(paste0('
<div id="zdens" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=', jstr(sb$usps), ',NM=', jstr(sb$state), ',X=', jnum(round(sb$dens, 2)), ';
const PAN=[{y:', jnum(round(PAN[[1]]$y, 2)), ',lab:"% of land with residents on it",r:',
  -FV("land_r_log"), ',show:', jnum(A$show), ',fx:', jnum(round(A$fx, 3)),
  ',fy:', jnum(round(A$fy, 2)), '},
           {y:', jnum(round(PAN[[2]]$y, 2)), ',lab:"% of blocks with residents",r:',
  -FV("dens_r_log"), ',show:', jnum(B$show), ',fx:', jnum(round(B$fx, 3)),
  ',fy:', jnum(round(B$fy, 2)), '}];
const W=700,PH=214,GAP=40,M={t:16,r:18,b:30,l:52},H=2*PH+GAP+18;
const wrap=d3.select("#zdens");
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:11px inherit");
const f=d3.format(",");
// Log x: the relationship lives in the orders of magnitude, not the levels.
const x=d3.scaleLog().domain([1,d3.max(X)*1.3]).range([M.l,W-M.r]).nice();
const tip=wrap.append("div").attr("class","zpb-tip").style("display","none");
const card=(t,rows)=>`<h4>${t}</h4><table>`+
  rows.map(r=>`<tr><th>${r[0]}</th><td>${r[1]}</td></tr>`).join("")+`</table>`;
const all=[];
PAN.forEach((p,pi)=>{
  const y0=pi*(PH+GAP), y=d3.scaleLinear().domain([0,d3.max(p.y)*1.1]).nice()
    .range([y0+PH-M.b,y0+M.t]);
  svg.append("g").attr("stroke","#76838C").attr("opacity",0.24)
    .selectAll("line").data(y.ticks(4)).join("line")
    .attr("x1",M.l).attr("x2",W-M.r).attr("y1",y).attr("y2",y);
  svg.append("line").attr("x1",x(p.fx[0])).attr("y1",y(p.fy[0]))
    .attr("x2",x(p.fx[1])).attr("y2",y(p.fy[1]))
    .attr("stroke","#76838C").attr("stroke-width",1.6).attr("stroke-dasharray","5,4");
  const dots=svg.append("g").selectAll("circle").data(D).join("circle")
    .attr("cx",(d,i)=>x(X[i])).attr("cy",(d,i)=>y(p.y[i])).attr("r",4.2)
    .attr("fill","', RED, '").attr("fill-opacity",0.85).style("cursor","pointer");
  all.push(dots);
  svg.append("g").selectAll("text").data(D.filter((d,i)=>p.show[i])).join("text")
    .attr("x",d=>x(X[D.indexOf(d)])+7).attr("y",d=>y(p.y[D.indexOf(d)])+3.4)
    .attr("font-size","9px").attr("fill","#4E5A63").text(d=>d);
  svg.append("g").attr("transform","translate(0,"+(y0+PH-M.b)+")")
    .call(d3.axisBottom(x).ticks(5,"~s").tickSizeOuter(0));
  svg.append("g").attr("transform","translate("+M.l+",0)")
    .call(d3.axisLeft(y).ticks(4).tickFormat(d=>d+"%").tickSizeOuter(0));
  svg.append("text").attr("x",M.l).attr("y",y0+10).attr("font-size","11.5px")
    .attr("font-weight","600").attr("fill","#12181D").text(p.lab);
  svg.append("text").attr("x",W-M.r).attr("y",y0+10).attr("text-anchor","end")
    .attr("font-size","11px").attr("fill","#76838C")
    .text("r = "+p.r.toFixed(2));
  dots.on("mousemove",function(ev,d){
    const i=D.indexOf(d);
    all.forEach(g=>g.attr("fill-opacity",(q,j)=>j===i?1:0.25));
    tip.style("display","block").html(card(NM[i],[
      ["Land with residents",PAN[0].y[i].toFixed(1)+"%"],
      ["Blocks with residents",PAN[1].y[i].toFixed(1)+"%"],
      ["Land with no residents",(100-PAN[0].y[i]).toFixed(1)+"%"],
      ["Population density",f(Math.round(X[i]))+" per sq mi"]]));
    const b=wrap.node().getBoundingClientRect(), t=tip.node().getBoundingClientRect();
    let px=ev.clientX-b.left+16;
    if(px+t.width>b.width) px=ev.clientX-b.left-t.width-16;
    tip.style("left",px+"px")
       .style("top",Math.max(0,ev.clientY-b.top-t.height-14)+"px");
  })
  .on("mouseleave",()=>{tip.style("display","none");
    all.forEach(g=>g.attr("fill-opacity",0.85));});
});
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-2).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63")
  .text("people per square mile of land (log scale)");
})();
</script>
'))

## ---- dens-static
op <- par(mfrow = c(2, 1), mar = c(2.6, 4.6, 1.8, 1.2), oma = c(2.4, 0, 0, 0))
for (p in list(list(v = 100 - sb$pct_land, lab = "% of land with residents on it",
                    r = -FV("land_r_log")),
               list(v = 100 - sb$pct_zero, lab = "% of blocks with residents",
                    r = -FV("dens_r_log")))) {
  m <- lm(p$v ~ log10(sb$dens)); rs <- resid(m)
  k <- unique(c(order(-p$v)[1:3], order(p$v)[1:2], which.max(sb$dens),
                which.min(sb$dens), order(-abs(rs))[1:3]))
  plot(sb$dens, p$v, log = "x", pch = 19, col = RED, cex = 0.7,
       xlab = "", ylab = p$lab, las = 1, bty = "n", cex.axis = 0.8, cex.lab = 0.85)
  fl <- fitline(m)
  lines(fl$fx, fl$fy, col = GREY, lwd = 1.8, lty = 2)
  text(sb$dens[k], p$v[k], sb$usps[k], pos = 4, cex = 0.55, col = "#4E5A63")
  mtext(sprintf("r = %.2f", p$r), side = 3, adj = 1, line = 0.2, cex = 0.75, col = "grey35")
}
mtext("people per square mile of land (log scale)", side = 1, outer = TRUE,
      line = 0.9, cex = 0.85)
par(op)

## ---- dc-d3
DCP <- lapply(split(DC, DC$cat), polypaths)
cat(paste0('
<div id="zdc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const P0=', jstr(DCP[["0"]]), ',P1=', jstr(DCP[["1"]]), ',P2=', jstr(DCP[["2"]]), ';
const W=', DCF$w, ',H=', DCF$h, ';
const svg=d3.select("#zdc").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;display:block;margin:0 auto");
const draw=(d,fill)=>svg.append("g").selectAll("path").data(d).join("path")
  .attr("d",q=>q).attr("fill",fill).attr("fill-rule","evenodd")
  .attr("stroke","#ffffff").attr("stroke-width",0.25);
draw(P0,"#EDEFF1");     // somebody lives here
draw(P2,"#C9D6DE");     // all water
draw(P1,"', RED, '");   // nobody, and it is land
const K=[["', RED, '","no residents"],["#EDEFF1","residents"],["#C9D6DE","water"]];
let kx=8;
K.forEach(k=>{
  svg.append("rect").attr("x",kx).attr("y",H-16).attr("width",11).attr("height",11)
    .attr("fill",k[0]).attr("stroke","#ffffff").attr("stroke-width",0.5);
  svg.append("text").attr("x",kx+15).attr("y",H-7).attr("font-size","11px")
    .attr("fill","#4E5A63").text(k[1]);
  kx+=k[1].length*6.2+34;
});
})();
</script>
'))

## ---- dc-static
COL <- c("0" = "#EDEFF1", "1" = RED, "2" = "#C9D6DE")
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, DCF$w), ylim = c(DCF$h, 0), asp = 1,
     axes = FALSE, xlab = "", ylab = "")
for (cc in c("0", "2", "1")) drawpolys(DC[DC$cat == as.integer(cc), ], COL[[cc]],
                                       border = "#ffffff", lwd = 0.1)
legend("bottomleft", bty = "n", cex = 0.62, horiz = TRUE, inset = c(0, 0),
       fill = c(RED, "#EDEFF1", "#C9D6DE"), border = "white",
       legend = c("no residents", "residents", "water"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
