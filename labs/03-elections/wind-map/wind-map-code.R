# wind-map-code.R -- chunk bodies for wind-map-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6, dpi = 96,
                      fig.retina = 1)
options(scipen = 999)

# The D3 figures follow the page theme through CSS. The base-R ones are
# rasterised and cannot, so at least stop them carrying a paper colour of
# their own: a transparent PNG sits on whatever paper the reader has, and the
# ink goes to a mid grey that holds up against both. In print, where the paper
# is known to be white, the ink goes back to near-black.
HTMLOUT <- knitr::is_html_output()
if (HTMLOUT) knitr::opts_chunk$set(dev.args = list(bg = "transparent"))
D <- "data"

us <- read.csv(file.path(D, "derived/wind_us.csv"), colClasses = c(county_fips = "character"))
ga <- read.csv(file.path(D, "derived/wind_ga.csv"), colClasses = c(fips = "character"))
uo <- read.csv(file.path(D, "derived/us_outline.csv"))
go <- read.csv(file.path(D, "derived/ga_outline.csv"))
ff <- read.csv(file.path(D, "derived/facts.csv"))

# Every number in this document comes out of F(). Nothing is typed.
FV <- setNames(ff$value, ff$key)
F  <- function(k) {
  stopifnot(k %in% names(FV))
  z <- FV[[k]]
  n <- suppressWarnings(as.numeric(z))
  if (is.na(n)) z else n
}
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
sg <- function(x, k = 1) {                       # signed, R+ / D+ labeled
  v <- as.numeric(x)
  paste0(if (v >= 0) "R+" else "D+", formatC(abs(v), format = "f", digits = k))
}

usf <- us[us$in_frame == "TRUE" | us$in_frame == TRUE, ]

DEGPP <- F("deg_per_point")
RED <- "#C41230"; BLU <- "#2c7fb8"; GRY <- "#8c8c8c"

# ---- length, the same rule the CSVs were built with ------------------------
# Linear on every map: an arrow twice as long is twice the swing. Only the
# kilometres-per-point differs between frames, and each legend prints its own.
LMINF  <- F("len_min_frac")
LMAXUS <- F("len_max_km_us");        NVFUS <- F("net_votes_full_us")
LMAXGA <- F("len_max_km_ga_county"); NVFGA <- F("net_votes_full_ga_county")
len_us <- function(net) LMAXUS * (LMINF + (1-LMINF) * sqrt(pmin(net, NVFUS)/NVFUS))
len_ga <- function(net) LMAXGA * (LMINF + (1-LMINF) * sqrt(pmin(net, NVFGA)/NVFGA))

# ---- one arrow, one rule ---------------------------------------------------
# Every map in this brief reads dx and dy straight out of its CSV. There is
# no second encoding and no per-figure geometry: what wind_geom() wrote is what
# gets drawn, which is why the figures cannot disagree about what an arrow
# means.

# ---- the headline, verified rather than asserted ---------------------------
# "Which way did most counties move" is computed here, on the TWO-PARTY margin
# every arrow draws: gop minus dem over (gop + dem), which holds third parties
# constant. Counts and vote shares are both kept, because counting counties
# and counting voters answer different questions and the prose prints both.
pair <- function(d, a, b) {
  vt  <- d[[paste0("votes_dem_", b)]] + d[[paste0("votes_gop_", b)]]
  agg <- function(dm, gp) 100 * (sum(gp) - sum(dm)) / (sum(gp) + sum(dm))
  list(n        = nrow(d),
       cnt_R    = sum(d$swing > 0),
       pct_R    = 100 * mean(d$swing > 0),
       vsh_R    = 100 * sum(vt[d$swing > 0]) / sum(vt),
       cnt_D    = sum(d$swing <= 0),
       pct_D    = 100 * mean(d$swing <= 0),
       vsh_D    = 100 * sum(vt[d$swing <= 0]) / sum(vt),
       med      = median(d$swing),
       natl     = agg(d[[paste0("votes_dem_", b)]], d[[paste0("votes_gop_", b)]]) -
                  agg(d[[paste0("votes_dem_", a)]], d[[paste0("votes_gop_", a)]]))
}
P2 <- pair(usf, "20", "24")      # 2020 -> 2024

# ---- one arrow, drawn the same way everywhere -----------------------------
# x, y, dx, dy come straight out of the CSVs. Nothing here projects or
# rescales anything, which is why the D3 figures and these base-R figures
# cannot drift apart.
arrow_at <- function(x, y, dx, dy, col, lwd = 1, head = 0.045) {
  # An arrow shorter than one device pixel has no drawable angle; arrows()
  # calls it zero-length and skips it. Draw those as dots instead, so the
  # county is still on the map and nothing is silently missing.
  eps <- diff(par("usr")[1:2]) / (par("pin")[1] * 96)
  z <- sqrt(dx^2 + dy^2) < eps
  cc <- if (length(col) > 1) col else rep(col, length(x))
  if (any(z)) points(x[z], y[z], pch = 19, cex = 0.11, col = cc[z])
  k <- !z
  if (any(k)) arrows(x[k], y[k], (x + dx)[k], (y + dy)[k], length = head,
                     angle = 22, lwd = if (length(lwd) > 1) lwd[k] else lwd,
                     col = cc[k])
}
swing_col <- function(s) ifelse(s > 0, RED, BLU)

# ---- render every data.frame as a TABLE, not as code output ---------------
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- wind-runtime
# The shared runtime for every wind map below: d3, the zoom controls and the
# tooltip formatter, defined once so the four maps cannot drift apart in how
# they behave. Emitted with cat() rather than sprintf() -- it carries no
# substitutions, and sprintf's format string is capped at 8,192 characters.
cat('
<script src="../../_lib/d3.v7.min.js"></script>
<script>
/* ---------------------------------------------------------------------
   Shared by every wind map in this document. Defined once, beside the
   first figure, so the three national maps and the Georgia map cannot
   drift apart in how they zoom or what they say on hover.
   --------------------------------------------------------------------- */
window.windMap = (function(){
  const fmt = d3.format(",");
  /* R+/D+ from a pair of vote counts, on the two-party margin the arrows
     draw. Computed here rather than shipped, because the votes are already
     on the wire and the margin is one subtraction. */
  function marg(dv,rv){
    const t = dv+rv; if(!t) return "";
    const m = 100*(rv-dv)/t;
    return (m>=0?"R+":"D+") + Math.abs(m).toFixed(1);
  }
  /* Signed vote difference, in the same shape as the counts beside it. */
  function dlt(v){ return (v>0?"+":"\u2212") + fmt(Math.abs(v)); }
  /* A card, not a pill. The reader is being asked to weigh two observations
     and a difference, which is a deliberative act, so the tooltip is laid out
     as a small table: parties down the side, the two elections across the top,
     the change in its own column, and the margin underneath. The headline
     restates the arrow in words, because that is the one thing the reader came
     for and it should not have to be computed from the table. */
  function prow(label, cls, v0, v1) {
    return `<tr><td><i class="tt-bar ${cls}-bg"></i>${label}</td>`
         + `<td>${fmt(v0)}</td><td>${fmt(v1)}</td>`
         + `<td>${dlt(v1 - v0)}</td></tr>`;
  }
  function tipHTML(name,state,y0,y1,d0,r0,d1,r1,swing){
    const dir = swing > 0 ? "Republican" : "Democratic";
    const cls = swing > 0 ? "gop" : "dem";
    return `<div class="tt-name">${name}${state ? ", " + state : ""}</div>`
      + `<div class="tt-lede ${cls}-txt">`
      + `${Math.abs(swing).toFixed(1)} pts. more ${dir} `
      + `<span>than ${y0}</span></div>`
      + `<table><thead><tr><th>Party</th><th>${y0}</th><th>${y1}</th>`
      + `<th>Change</th></tr></thead><tbody>`
      + prow("Dem.", "dem", d0, d1) + prow("Rep.", "gop", r0, r1)
      + `</tbody><tfoot><tr><td>Two-party margin</td>`
      + `<td>${marg(d0,r0)}</td><td>${marg(d1,r1)}</td>`
      + `<td><b>${(swing>0?"R+":"D+") + Math.abs(swing).toFixed(1)}</b></td>`
      + `</tr></tfoot></table>`;
  }

  /* Buttons, not the wheel: a wheel handler on a figure this tall makes the
     page impossible to scroll past. Drag still pans. */
  function zoom(id,svg,gz,W,H,y0,y1){
    const z = d3.zoom().scaleExtent([1,14])
      .extent([[0,y0],[W,y1]]).translateExtent([[0,y0],[W,y1]])
      .filter(e => e.type !== "wheel" && !e.button)
      .on("zoom", e => gz.attr("transform", e.transform));
    svg.call(z).style("cursor","grab")
       .on("mousedown.cur",function(){d3.select(this).style("cursor","grabbing");})
       .on("mouseup.cur",  function(){d3.select(this).style("cursor","grab");});
    const bar = d3.select("#"+id).append("div").attr("class","zoombar");
    const mk = (label,aria,fn) => bar.append("button").attr("type","button")
      .attr("aria-label",aria).text(label).on("click",fn);
    /* Animate, unless the reader asked not to or the tab is in the background
       -- a background tab gets no animation frames, so a transition there
       never starts and the button silently does nothing. */
    const still = () => document.hidden ||
      (window.matchMedia && matchMedia("(prefers-reduced-motion: reduce)").matches);
    const go = (fn,arg) => still() ? svg.call(fn,arg)
                                   : svg.transition().duration(220).call(fn,arg);
    const bIn  = mk("+","zoom in",()=>go(z.scaleBy,1.8));
    const bOut = mk("−","zoom out",()=>go(z.scaleBy,1/1.8));
    const bRst = mk("Reset","reset zoom",()=>go(z.transform,d3.zoomIdentity));
    z.on("zoom.ui", e => {
      const k = e.transform.k;
      bOut.attr("disabled", k<=1.001 ? "" : null);
      bRst.attr("disabled", k<=1.001 ? "" : null);
      bIn .attr("disabled", k>=13.99 ? "" : null);
    });
    bOut.attr("disabled",""); bRst.attr("disabled","");
    return z;
  }
  /* Nearest-arrow hover. Chasing a five-pixel target among 3,100 of them is
     not a thing anyone should have to do, so instead of one hit circle per
     county there is ONE capture rectangle over the map and a nearest-arrow
     search underneath it: whatever pixel the cursor is on, the arrow nearest
     that pixel answers. Nothing can be missed and nothing has to be aimed at.
     A ring marks whichever arrow is being read. */
  function hover(id, svg, gz, xy, html, W, y0, y1) {
    /* Nearest ARROW, not nearest arrow-origin. The pick used to run on the
       tails alone, so hovering the middle or the head of a long arrow could
       answer with a different county whose tail happened to be closer. xy may
       therefore be points [x,y] or segments [x0,y0,x1,y1]; segments are
       measured properly, by distance to the nearest point ON the arrow. */
    const seg = xy.length && xy[0].length === 4;
    const near = function (px, py) {
      let best = -1, bd = Infinity;
      for (let i = 0; i < xy.length; i++) {
        const p = xy[i];
        let dx, dy;
        if (seg) {
          const vx = p[2] - p[0], vy = p[3] - p[1];
          const L2 = vx * vx + vy * vy;
          let t = L2 ? ((px - p[0]) * vx + (py - p[1]) * vy) / L2 : 0;
          t = t < 0 ? 0 : t > 1 ? 1 : t;
          dx = px - (p[0] + t * vx); dy = py - (p[1] + t * vy);
        } else { dx = px - p[0]; dy = py - p[1]; }
        const d2 = dx * dx + dy * dy;
        if (d2 < bd) { bd = d2; best = i; }
      }
      return best;
    };
    const host = d3.select("#" + id).node();
    /* Anchored to the RING, not to the cursor. Two earlier tries failed on
       the same corner: clamping mixed viewBox units with CSS pixels, and
       flipping off the cursor still landed on the county, because near the
       right-hand edge the nearest county is always to the LEFT of where the
       pointer is. The ring is the thing being described, so offsetting from
       the ring is what actually guarantees the label never covers it. */
    const place = function (rx, ry, tip) {
      const b  = host.getBoundingClientRect();
      const r  = tip.node().getBoundingClientRect();  /* reflects what was just written */
      const tw = r.width, th = r.height, GAP = 18;
      let lx = rx + GAP;
      if (lx + tw > b.width) lx = rx - GAP - tw;      /* the side with room */
      const ly = ry - th / 2;
      tip.style("left", Math.max(0, Math.min(lx, b.width  - tw)) + "px")
         .style("top",  Math.max(0, Math.min(ly, b.height - th)) + "px");
    };
    const ring = gz.append("circle").attr("r", 7).attr("class", "hilite")
      .attr("fill", "none").attr("stroke-width", 1.6)
      .attr("vector-effect", "non-scaling-stroke").style("display", "none");
    const tip = d3.select("#" + id).append("div").attr("class", "windtip");
    /* Appended last, so it sits above the legends and receives every move.
       Events still bubble to the svg, so drag-to-pan keeps working. */
    const cap = svg.append("rect").attr("x", 0).attr("y", y0)
      .attr("width", W).attr("height", y1 - y0)
      .attr("fill", "transparent").style("cursor", "crosshair");
    cap.on("mousemove", function (e) {
      const t = d3.zoomTransform(svg.node());
      const [mx, my] = d3.pointer(e, svg.node());
      const [lx, ly] = t.invert([mx, my]);
      const i = near(lx, ly);
      if (i < 0) return;
      ring.attr("cx", xy[i][0]).attr("cy", xy[i][1]).style("display", null);
      tip.style("opacity", 1).html(html(i));   /* html first: place() measures it */
      const b  = host.getBoundingClientRect();
      const kk = b.width / svg.node().viewBox.baseVal.width;
      const [sx, sy] = t.apply([xy[i][0], xy[i][1]]);   /* stage -> viewBox */
      place(sx * kk, sy * kk, tip);             /* viewBox -> rendered pixels */
    }).on("mouseleave", function () {
      tip.style("opacity", 0); ring.style("display", "none");
    });
  }

  /* A clipped group that the zoom transform is applied to. Titles and legends
     stay outside it, so they neither move nor scale. The clip has a top edge
     and no real bottom one: it is there to keep zoomed content off the title,
     and everything appended after the stage -- the legends, the footer --
     paints over the map rather than needing to be masked from it. Giving it a
     bottom edge above the frame silently cut the Gulf coast off. */
  function stage(svg,id,W,y0,y1){
    svg.append("clipPath").attr("id",id+"-clip").append("rect")
       .attr("x",0).attr("y",y0).attr("width",W).attr("height",y1-y0);
    return svg.append("g").attr("clip-path",`url(#${id}-clip)`).append("g");
  }
  return {tip:tipHTML, zoom:zoom, stage:stage, hover:hover};
})();
</script>
')

## ---- key-strip-d3
# One row, because there is one encoding. Geometry comes from wind_geom() by
# way of the same constants the CSVs were built with, so the key cannot drift
# from what it documents.
W1 <- 760; H1 <- 190; SC <- 78
sw <- c(-15, -10, -5, 0, 5, 10, 15)
X1 <- function(i) 96 + (i - 1) * 96
ROW <- 118
LL  <- rep(SC, length(sw))   # one length: this key is about angle
th  <- pmax(-90, pmin(90, DEGPP * sw)) * pi / 180
A1 <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f]',
                    X1(seq_along(sw)), ROW, LL * sin(th), -LL * cos(th), sw),
            collapse = ",")
cat(sprintf('
<div id="keystrip" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const A=[%s],W=%d,H=%d,ROW=%d;
const svg=d3.select("#keystrip").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",W/2).attr("y",22).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("Seven margin changes, drawn at one length");
svg.append("text").attr("x",W/2).attr("y",39).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("%s degrees per point of margin change \u00b7 length carries votes, keyed on each map");
const HB=0.42;
const g=svg.append("g").attr("fill","none").attr("stroke-width",2.4)
  .attr("stroke-linecap","round").attr("stroke-linejoin","round");
A.forEach(d=>{
  if(d[4]===0){
    svg.append("line").attr("x1",d[0]).attr("y1",ROW).attr("x2",d[0])
      .attr("y2",ROW-26).attr("class","key").attr("stroke-width",2.4);
    svg.append("path").attr("class","key").attr("stroke","none")
      .attr("fill","currentColor")
      .attr("d",`M${d[0]-4},${ROW-26}L${d[0]+4},${ROW-26}L${d[0]},${ROW-34}Z`);
  } else {
    const dx=d[2],dy=d[3],L=Math.hypot(dx,dy),h=Math.min(7,0.32*L);
    const ux=dx/L,uy=dy/L,c=Math.cos(HB),sn=Math.sin(HB),tx=d[0]+dx,ty=ROW+dy;
    g.append("path").attr("class",d[4]>0?"gop":"dem")
      .attr("d",`M${d[0]},${ROW}L${tx},${ty}`
        +`M${tx},${ty}l${h*(-ux*c+uy*sn)},${h*(-ux*sn-uy*c)}`
        +`M${tx},${ty}l${h*(-ux*c-uy*sn)},${h*( ux*sn-uy*c)}`);
  }
  svg.append("text").attr("x",d[0]).attr("y",ROW+30).attr("text-anchor","middle")
    .attr("font-size","11px").attr("font-weight","600")
    .attr("class",d[4]===0?"foot":(d[4]>0?"gop-txt":"dem-txt"))
    .text(d[4]===0?"none":(d[4]>0?"R+":"D+")+Math.abs(d[4]));
});
})();
</script>
', A1, W1, H1, ROW, pc(DEGPP, 0)))

## ---- key-strip-static
par(mar = c(0.4, 0.4, 2.0, 0.4))
sw <- c(-15, -10, -5, 0, 5, 10, 15)
LL <- rep(0.86, length(sw))  # one length: this key is about angle
plot(NA, xlim = c(-1.05, length(sw) - 0.15), ylim = c(-0.75, 1.05),
     asp = 1, axes = FALSE, ann = FALSE)
for (i in seq_along(sw)) {
  if (sw[i] == 0) {
    segments(i - 1, 0, i - 1, 0.30, lwd = 2.4, col = GRY)
    points(i - 1, 0.30, pch = 17, cex = 0.8, col = GRY)
  } else {
    th <- max(-90, min(90, DEGPP * sw[i])) * pi / 180
    arrow_at(i - 1, 0, LL[i] * sin(th), LL[i] * cos(th), swing_col(sw[i]),
             lwd = 2.4, head = 0.07)
  }
  text(i - 1, -0.46, if (sw[i] == 0) "none" else sg(sw[i], 0),
       cex = 0.7, col = if (sw[i] == 0) GRY else swing_col(sw[i]), font = 2)
}
title("Seven margin changes, drawn at one length", cex.main = 0.95, line = 0.6)
mtext(paste0("angle ", pc(DEGPP, 0),
             " degrees per point of margin change; length carries votes, keyed on each map"),
      side = 3, line = -0.4, cex = 0.66, col = "#666666")

## ---- us-d3
# The legends used to sit at H-92, which on this frame is on top of Florida.
# The map now gets a fixed height and the keys get a band of their own beneath
# it, so nothing is drawn over the country.
W <- 780; PAD <- 8; MAPT <- 52; MAPH <- 424; BAND <- 84
H <- MAPT + MAPH + BAND
xr <- range(uo$x); yr <- range(uo$y)
s  <- min((W - 2*PAD) / diff(xr), MAPH / diff(yr))
SX <- function(x) PAD + (x - xr[1]) * s
SY <- function(y) MAPT + (yr[2] - y) * s

# Rows are ARRAYS, not objects: the same 3,100 arrows cost about a third as
# many bytes. Arrow components keep one decimal,
# because the longest arrow is only about 15 pixels and rounding its components
# to whole pixels would bend a 60-degree arrow to 58 and a short one to 53.
paths <- vapply(split(uo, uo$part), function(z) {
  # Simplify in PIXEL space, not by index: round each vertex to the pixel and
  # drop the ones that land on top of their predecessor. The boundary can move
  # by at most half a pixel, and -- this is the part that matters -- two states
  # that share a border round it to the same pixels, so the border stays shared.
  # Dropping every sixth vertex instead moved some borders by 35 pixels and,
  # because each state was thinned independently, left gaps along every shared
  # edge. On white paper those gaps are white. On any other background they are
  # wedges of it, showing through the middle of the country.
  px <- round(SX(z$x)); py <- round(SY(z$y))
  k  <- c(TRUE, diff(px) != 0 | diff(py) != 0)
  paste0("M", paste(sprintf("%d,%d", px[k], py[k]), collapse = "L"), "Z")
}, character(1))
OUTL <- paste(sprintf('"%s"', paths), collapse = ",")

# Thin arrows first, heavy ones last, so the big counties are not buried under
# the small ones. The base-R figure below sorts the same way; when the two
# renderers disagree about draw order they disagree about the picture.
d <- usf[order(usf$lw), ]
ST <- sort(unique(d$state_name))            # 49 strings, sent once
# Both observations travel, not just their difference: the tooltip has to be
# able to show what the arrow was computed FROM. Four counts per county.
A <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f,%.2f,"%s",%d,%d,%d,%d,%d]',
                   SX(d$x), SY(d$y), d$dx * s, -d$dy * s, d$swing, d$lw,
                   gsub('"', "", sub(" (County|Parish|Borough|city|City).*$", "",
                                     d$county_name)),
                   match(d$state_name, ST) - 1L,
                   d$votes_dem_20, d$votes_gop_20,
                   d$votes_dem_24, d$votes_gop_24), collapse = ",")
STJ <- paste(sprintf('"%s"', ST), collapse = ",")
LEGN <- paste(sprintf('{"s":"%s","L":%.1f,"i":%d}',
                      c("1,000", "5,000", n(NVFUS)),
                      len_us(c(1000, 5000, NVFUS)) * s, 0:2), collapse = ",")
cat(sprintf('

<div id="usw" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const O=[%s],A=[%s],LG=[%s],ST=[%s];
const W=%d,H=%d,BAND=%d,RED="%s",BLU="%s";
const svg=d3.select("#usw").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("How every county moved, 2020 to 2024");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("presidential two-party margin \\u00b7 leaning right = Republican gain, left = Democratic gain");
const gz=windMap.stage(svg,svg.node().parentNode.id,W,42,H-BAND+2);
const g=gz.append("g");
g.selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
// ---- one arrow = one path: shaft plus two barbs -----------------------
// An arrow needs a HEAD. The alternative -- a bare line segment -- has no
// direction at all, and a dot at the far end is worse than nothing, because a
// dot reads as "the place", so the arrow appears to point back toward the
// county from somewhere out in the next state. The barbs are capped at 0.45 of
// the shaft so that a 6-pixel arrow does not become a 6-pixel arrowhead.
const HB=0.42;                                  // barb angle, radians (24 deg)
function arrowPath(d){
  const x=d[0],y=d[1],dx=d[2],dy=d[3],L=Math.hypot(dx,dy);
  if(L<1) return null;                          // sub-pixel: dot at the ORIGIN
  const h=Math.min(2.8,0.45*L),ux=dx/L,uy=dy/L;
  const c=Math.cos(HB),s=Math.sin(HB),tx=x+dx,ty=y+dy;
  const b1x=h*(-ux*c+uy*s),b1y=h*(-ux*s-uy*c);
  const b2x=h*(-ux*c-uy*s),b2y=h*( ux*s-uy*c);
  return `M${x},${y}L${tx},${ty}M${tx},${ty}l${b1x},${b1y}M${tx},${ty}l${b2x},${b2y}`;
}
const ar=gz.append("g").attr("fill","none");
ar.selectAll("path").data(A.filter(d=>arrowPath(d))).join("path")
  .attr("d",arrowPath)
  .attr("class",d=>d[4]>0?"gop":"dem")
  .attr("stroke-opacity",0.62)
  .attr("stroke-width",0.9)
  .attr("stroke-linecap","round").attr("stroke-linejoin","round")
  .attr("vector-effect","non-scaling-stroke");
// A county whose arrow is shorter than one pixel has no drawable angle. It is
// drawn as a dot AT ITS OWN CENTROID -- the same rule the print figure uses --
// so the county is still on the map and nothing is silently missing.
ar.selectAll("circle").data(A.filter(d=>!arrowPath(d))).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",0.6)
  .attr("class",d=>d[4]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.62);
windMap.hover(svg.node().parentNode.id,svg,gz,A.map(d=>[d[0],d[1],d[0]+d[2],d[1]+d[3]]),
  i=>windMap.tip(A[i][6],ST[A[i][7]],2020,2024,A[i][8],A[i][9],A[i][10],A[i][11],A[i][4]),
  W,42,H-BAND+2);
// ---- legend: the length scale, stated, not implied --------------------
const lg=svg.append("g").attr("transform",`translate(16,${H-66})`);
lg.append("text").attr("y",-4).attr("font-size","11px").attr("font-weight","600")
  .attr("class","lbl").text("net votes moved");
LG.forEach((d,i)=>{
  lg.append("line").attr("x1",0).attr("y1",8+i*17).attr("x2",d.L).attr("y2",8+i*17)
    .attr("class","key").attr("stroke-width",1.6).attr("stroke-linecap","round");
  lg.append("text").attr("x",d.L+7).attr("y",12+i*17).attr("font-size","10.5px")
    .attr("class","lbl").text(d.s+" votes"+(d.i===2?" or more":""));});
const lg2=svg.append("g").attr("transform",`translate(${W-206},${H-66})`);
[["gop","shift toward the Republican"],["dem","shift toward the Democrat"]]
 .forEach((r,i)=>{lg2.append("line").attr("x1",0).attr("y1",i*17).attr("x2",22)
   .attr("y2",i*17).attr("class",r[0]).attr("stroke-width",2.2);
  lg2.append("text").attr("x",28).attr("y",i*17+4).attr("font-size","10.5px")
   .attr("class","lbl").text(r[1]);});
svg.append("text").attr("x",16).attr("y",H-10).attr("font-size","10px")
  .attr("class","foot").text("angle = %s degrees per point of margin change \\u00b7 length = net votes that changed hands");
windMap.zoom(svg.node().parentNode.id,svg,gz,W,H,42,H-BAND+2);
})();
</script>
', OUTL, A, LEGN, STJ, W, H, BAND, RED, BLU, round(W/2), round(W/2), pc(DEGPP, 0)))

## ---- us-static
par(mar = c(0.2, 0.2, 2.2, 0.2))
plot(NA, xlim = range(uo$x), ylim = range(uo$y) + c(-330, 40), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(uo, uo$part)) polygon(p$x, p$y, col = "#f6f6f6",
                                      border = "#c9c9c9", lwd = 0.4)
o <- usf[order(usf$lw), ]
arrow_at(o$x, o$y, o$dx, o$dy, adjustcolor(swing_col(o$swing), 0.62),
         lwd = 0.5, head = 0.012)
title("How every county moved, 2020 to 2024", cex.main = 1.0, line = 0.6)
mtext("two-party margin; leaning right = Republican gain, left = Democratic gain",
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
# legend, absolute scale. Rows are spaced in MAP kilometers, so the gaps have
# to be map-sized: 95 km between rows, not 26.
# ly sits low enough that the whole key, including the party swatches at
# ly + 70, clears the bottom of the map. At -60 they landed on Florida.
lx <- min(uo$x) + 60; ly <- min(uo$y) - 105
text(lx, ly + 95, "net votes moved", cex = 0.62, font = 2, col = "#444444", pos = 4)
for (i in 1:3) {
  k  <- c(1000, 5000, NVFUS)[i]
  yy <- ly - (i - 1) * 95
  segments(lx + 6, yy, lx + 6 + len_us(k), yy, lwd = 1.8, col = "#555555")
  text(lx + 6 + len_us(k), yy,
       paste0("  ", n(k), " votes", if (i == 3) " or more" else ""),
       cex = 0.58, col = "#555555", pos = 4)
}
rx <- max(uo$x) - 1150
segments(rx, ly + 70, rx + 110, ly + 70, col = RED, lwd = 2.4)
text(rx + 110, ly + 70, "  shift toward the Republican", cex = 0.58, col = RED, pos = 4)
segments(rx, ly - 25, rx + 110, ly - 25, col = BLU, lwd = 2.4)
text(rx + 110, ly - 25, "  shift toward the Democrat", cex = 0.58, col = BLU, pos = 4)
mtext(paste0("angle = ", pc(DEGPP, 0),
             " degrees per point of margin change; length = net votes moved"),
      side = 1, line = -1.2, cex = 0.58, col = "#888888")

## ---- us-summary
data.frame(
  quantity = c("Counties drawn", "Median county shift", "Counties shifting right (%)",
               "Largest shift right", "Largest shift left",
               "Arrows at the length cap"),
  value = c(n(F("us_arrows")), sg(F("us_swing_median")), pc(F("us_share_R")),
            sg(F("us_swing_max")), sg(F("us_swing_min")), n(F("us_capped_len"))))

## ---- weighting
data.frame(
  question = c("What did the typical county do?",
               "What did the typical county do? (median)",
               "What did the votes do?"),
  answer = c(paste0("mean county shift ", sg(F("us_swing_unweighted"))),
             paste0("median county shift ", sg(F("us_swing_median"))),
             paste0("national two-party margin moved ", sg(F("us_agg_swing")))),
  check.names = FALSE)

## ---- ga-d3
W2 <- 720; H2 <- 470; PD <- 10
gx <- range(go$x); gy <- range(go$y)
s2 <- min((W2 - 200 - 2*PD) / diff(gx), (H2 - 56 - 2*PD) / diff(gy))
GX <- function(x) PD + (x - gx[1]) * s2
GY <- function(y) PD + 44 + (gy[2] - y) * s2
gpaths <- vapply(split(go, go$part), function(z)
  paste0("M", paste(sprintf("%.0f,%.0f", GX(z$x), GY(z$y)), collapse = "L"), "Z"),
  character(1))
GO <- paste(sprintf('"%s"', gpaths), collapse = ",")
GA <- paste(sprintf('{"x":%.1f,"y":%.1f,"u":%.1f,"v":%.1f,"s":%.2f,"w":%.2f,"n":"%s","d0":%d,"r0":%d,"d1":%d,"r1":%d}',
                    GX(ga$x), GY(ga$y), ga$dx * s2, -ga$dy * s2, ga$swing, ga$lw,
                    ga$county, ga$dem_20, ga$gop_20, ga$dem_24, ga$gop_24),
            collapse = ",")
GL <- paste(sprintf('{"s":"%s","L":%.1f,"i":%d}', c("500", n(NVFGA)),
                    len_ga(c(500, NVFGA)) * s2, 0:1), collapse = ",")
cat(sprintf('
<div id="gaw" class="wind-fig" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const O=[%s],A=[%s],LG=[%s];
const W=%d,H=%d,RED="%s",BLU="%s";
const svg=d3.select("#gaw").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("Georgia, county by county, 2020 to 2024");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("Secretary of State returns, both years \\u00b7 %s counties, unchanged since 1945");
const gz=windMap.stage(svg,"gaw",W,42,H);
const g=gz.append("g");
g.selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.5)
  .attr("vector-effect","non-scaling-stroke");
svg.append("defs").selectAll("marker").data(["gR","gB"]).join("marker")
  .attr("id",d=>d).attr("viewBox","0 0 10 10").attr("refX",8).attr("refY",5)
  .attr("markerWidth",4).attr("markerHeight",4).attr("orient","auto")
  .append("path").attr("d","M0,0L10,5L0,10Z").attr("class",d=>d==="gR"?"gop-fill":"dem-fill");
const ar=gz.append("g");
ar.selectAll("line").data(A).join("line")
  .attr("x1",d=>d.x).attr("y1",d=>d.y).attr("x2",d=>d.x+d.u).attr("y2",d=>d.y+d.v)
  .attr("class",d=>d.s>0?"gop":"dem").attr("stroke-opacity",0.85)
  .attr("stroke-width",1.1).attr("stroke-linecap","round")
  .attr("marker-end",d=>d.s>0?"url(#gR)":"url(#gB)");
windMap.hover("gaw",svg,gz,A.map(d=>[d.x,d.y,d.x+d.u,d.y+d.v]),
  i=>windMap.tip(A[i].n,"",2020,2024,A[i].d0,A[i].r0,A[i].d1,A[i].r1,A[i].s),
  W,42,H);
const lg=svg.append("g").attr("transform",`translate(${W-186},96)`);
lg.append("text").attr("y",-6).attr("font-size","11px").attr("font-weight","600")
  .attr("class","lbl").text("net votes moved");
LG.forEach((d,i)=>{
  lg.append("line").attr("x1",0).attr("y1",8+i*18).attr("x2",d.L).attr("y2",8+i*18)
    .attr("class","key").attr("stroke-width",1.8).attr("stroke-linecap","round");
  lg.append("text").attr("x",d.L+7).attr("y",12+i*18).attr("font-size","10.5px")
    .attr("class","lbl").text(d.s+" votes"+(d.i===1?" or more":""));});
[["gop","toward the Republican"],["dem","toward the Democrat"]].forEach((r,i)=>{
  lg.append("line").attr("x1",0).attr("y1",64+i*17).attr("x2",22).attr("y2",64+i*17)
    .attr("stroke",r[0]).attr("stroke-width",2.2);
  lg.append("text").attr("x",28).attr("y",68+i*17).attr("font-size","10.5px")
    .attr("class","lbl").text(r[1]);});
lg.append("text").attr("y",112).attr("font-size","10px").attr("class","foot")
  .text("%s degrees per point");
lg.append("text").attr("y",126).attr("font-size","10px").attr("class","foot")
  .text("(national map: full at %s)");
windMap.zoom("gaw",svg,gz,W,H,42,H);
})();
</script>
', GO, GA, GL, W2, H2, RED, BLU, round((W2 - 190)/2), round((W2 - 190)/2),
   n(F("ga_counties")), pc(DEGPP, 0), n(NVFUS)))

## ---- ga-static
par(mar = c(1.8, 0.2, 2.2, 0.2))
plot(NA, xlim = range(go$x) + c(0, 95), ylim = range(go$y), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(go, go$part)) polygon(p$x, p$y, col = "#f7f7f7",
                                      border = "#cfcfcf", lwd = 0.35)
arrow_at(ga$x, ga$y, ga$dx, ga$dy, swing_col(ga$swing),
         lwd = 0.9, head = 0.028)
title("Georgia, county by county, 2020 to 2024", cex.main = 1.0, line = 0.6)
mtext(paste0("Secretary of State returns, both years; ", n(F("ga_counties")),
             " counties, unchanged since 1945"),
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
lx <- max(go$x) + 6; ly <- max(go$y) - 30
text(lx, ly + 22, "arrow length", cex = 0.6, font = 2, col = "#444444", pos = 4)
for (i in 1:2) {
  k <- c(500, NVFGA)[i]
  segments(lx + 2, ly - (i - 1) * 20, lx + 2 + len_ga(k), ly - (i - 1) * 20,
           lwd = 1.7, col = "#555555")
  text(lx + 4 + len_ga(k), ly - (i - 1) * 20, paste0(n(k), " votes"), cex = 0.55,
       col = "#555555", pos = 4)
}
text(lx + 2, ly - 52, "toward the Republican", cex = 0.55, col = RED, pos = 4)
text(lx + 2, ly - 68, "toward the Democrat", cex = 0.55, col = BLU, pos = 4)
text(lx + 2, ly - 92, paste0(pc(DEGPP, 0), " degrees per point"), cex = 0.52,
     col = "#888888", pos = 4)
text(lx + 2, ly - 106, paste0("(national: full at ", n(NVFUS), ")"), cex = 0.52,
     col = "#888888", pos = 4)

## ---- ga-summary
data.frame(
  quantity = c("Counties drawn", "Georgia margin, 2020", "Georgia margin, 2024",
               "Statewide shift", "Median county shift",
               "Counties shifting right (%)",
               "Biggest shift right", "Biggest shift left"),
  value = c(n(F("ga_counties_drawn")), sg(F("ga_state_margin_20")),
            sg(F("ga_state_margin_24")), sg(F("ga_state_swing")),
            sg(F("ga_swing_median")), pc(F("ga_share_R")),
            paste0(F("ga_swing_max_county"), "  ", sg(F("ga_swing_max"))),
            paste0(F("ga_swing_min_county"), "  ", sg(F("ga_swing_min")))))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
