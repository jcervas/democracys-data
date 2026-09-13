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
dd_derived(c("block_pop_hist.csv", "facts.csv", "map_frame.csv", "map_states.csv",
             "map_zero.csv", "state_blocks.csv", "zero_land.csv"))

sb <- read.csv(file.path(D, "derived/state_blocks.csv"),
               colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
hp <- read.csv(file.path(D, "derived/block_pop_hist.csv"), stringsAsFactors = FALSE)
MZ <- read.csv(file.path(D, "derived/map_zero.csv"),   stringsAsFactors = FALSE)
MS <- read.csv(file.path(D, "derived/map_states.csv"), stringsAsFactors = FALSE)
FR <- read.csv(file.path(D, "derived/map_frame.csv"),  stringsAsFactors = FALSE)
zlnd <- read.csv(file.path(D, "derived/zero_land.csv"),
                 colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
fc <- read.csv(file.path(D, "derived/facts.csv"), stringsAsFactors = FALSE)

n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# Every headline number in the prose comes out of facts.csv, which the build
# wrote from the same tables the figures are drawn from. Nothing is recomputed
# here, so the text and the figures cannot drift apart.
FV <- function(k) {
  v <- fc$value[fc$name == k]
  if (!length(v)) stop("no such fact: ", k)
  as.numeric(v)
}
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
jstr <- function(x) paste0("[", paste0('"', x, '"', collapse = ","), "]")
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
ZP <- polypaths(MZ); SP <- polypaths(MS)
cat(paste0('
<div id="zmap" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const Z=', jstr(ZP), ',S=', jstr(SP), ',W=', FR$w, ',H=', FR$h, ';
const svg=d3.select("#zmap").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;display:block");
// land base, then empty blocks, then the outlines over the top
svg.append("g").selectAll("path").data(S).join("path").attr("d",d=>d)
  .attr("fill","#ffffff").attr("fill-rule","evenodd");
svg.append("g").selectAll("path").data(Z).join("path").attr("d",d=>d)
  .attr("fill","', RED, '").attr("fill-rule","evenodd");
svg.append("g").selectAll("path").data(S).join("path").attr("d",d=>d)
  .attr("fill","none").attr("stroke","', GREY, '").attr("stroke-width",1.1);
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
TOPX <- 100L
h <- hp[hp$pop <= TOPX, ]
cat(paste0('
<div id="zhist" style="margin:1em 0"></div>
<script>
(function(){
const P=', paste0("[", paste(h$pop, collapse = ","), "]"),
',B=', paste0("[", paste(h$blocks, collapse = ","), "]"), ';
const W=700,H=300,M={t:12,r:12,b:38,l:78};   // l: room for "3,000,000"
const svg=d3.select("#zhist").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(P).range([M.l,W-M.r]).padding(0.12);
// LOG scale: the zero bar is more than twenty times the next one, so a linear
// axis would show one spike and a flat floor.
const y=d3.scaleLog().domain([1,d3.max(B)]).range([H-M.b,M.t]).nice();
svg.append("g").selectAll("rect").data(B).join("rect")
  .attr("x",(d,i)=>x(P[i])).attr("width",x.bandwidth())
  .attr("y",d=>y(Math.max(d,1))).attr("height",d=>y(1)-y(Math.max(d,1)))
  .attr("fill",(d,i)=>P[i]===0?"', RED, '":"', GREY, '");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(P.filter(p=>p%10===0)).tickSizeOuter(0));
// Explicit decade-and-third ticks, comma-formatted: the same values and the
// same formatting as the print version, so the two cannot read differently.
const TICKS=[1e4,3e4,1e5,3e5,1e6,3e6]
  .filter(v=>v>=d3.min(B)/1.5&&v<=d3.max(B)*1.5);
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickValues(TICKS).tickFormat(d3.format(",")));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-4)
  .attr("text-anchor","middle").attr("font-size","11px")
  .text("people counted in the block, 2020");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+H-M.b)/2)
  .attr("y",13).attr("text-anchor","middle").attr("font-size","11px")
  .text("census blocks (log scale)");
})();
</script>
'))

## ---- hist-static
TOPX <- 100L
h <- hp[hp$pop <= TOPX, ]
# Log ticks at decades and their thirds, written in units a reader can hold in
# mind. The default axis puts 2000000 next to 1000000 in full digits, which
# collides with itself and with the axis title.
par(mar = c(4.2, 6.6, 0.8, 0.8))
plot(h$pop, pmax(h$blocks, 1), type = "h", log = "y", lend = 1, lwd = 3,
     col = ifelse(h$pop == 0, RED, GREY),
     xlab = "people counted in the block, 2020", ylab = "",
     las = 1, bty = "n", yaxt = "n")
at   <- c(1e4, 3e4, 1e5, 3e5, 1e6, 3e6)
keep <- at >= min(h$blocks) / 1.5 & at <= max(h$blocks) * 1.5
axis(2, at = at[keep], labels = n(at[keep]), las = 1)
mtext("census blocks (log scale)", side = 2, line = 5.2, cex = 0.95)

## ---- state-d3
s <- sb[order(sb$pct_zero), ]
cat(paste0('
<div id="zstate" style="margin:1em 0"></div>
<script>
(function(){
const N=', jstr(s$state), ',V=[', paste(s$pct_zero, collapse = ","), '];
const rowH=13,M={t:10,r:44,b:26,l:128},W=700,H=M.t+N.length*rowH+M.b;
const svg=d3.select("#zstate").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:11px inherit");
const x=d3.scaleLinear().domain([0,d3.max(V)]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(N).range([M.t,H-M.b]).padding(0.2);
svg.append("g").selectAll("rect").data(N).join("rect")
  .attr("x",M.l).attr("y",d=>y(d)).attr("height",y.bandwidth())
  .attr("width",(d,i)=>x(V[i])-M.l).attr("fill","', RED, '");
svg.append("g").selectAll("text.l").data(N).join("text")
  .attr("x",M.l-6).attr("y",d=>y(d)+y.bandwidth()/2).attr("dy","0.35em")
  .attr("text-anchor","end").attr("fill","#333").text(d=>d);
svg.append("g").selectAll("text.v").data(N).join("text")
  .attr("x",(d,i)=>x(V[i])+4).attr("y",d=>y(d)+y.bandwidth()/2).attr("dy","0.35em")
  .attr("font-size","10px").attr("fill","#777").text((d,i)=>V[i].toFixed(1)+"%");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%").tickSizeOuter(0));
})();
</script>
'))

## ---- state-static
s <- sb[order(sb$pct_zero), ]
par(mar = c(4, 8.5, 0.5, 2))
bp <- barplot(s$pct_zero, horiz = TRUE, col = RED, border = NA, las = 1,
              names.arg = s$state, cex.names = 0.45, xlab = "blocks with nobody living on them (%)")

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
