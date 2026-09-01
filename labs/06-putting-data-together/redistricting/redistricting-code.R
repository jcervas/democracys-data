# redistricting-code.R -- chunk bodies for redistricting-brief.Rmd
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
cd <- read.csv("data/derived/pres_by_cd_2024.csv", stringsAsFactors = FALSE)

# ---- the three statistics ---------------------------------------------------
# All three are computed here from one column, dem_share, so a reader can see
# exactly what each of them is a function of. Sign convention, held to
# throughout and stated in the brief: NEGATIVE means the plan favours
# Republicans, positive means it favours Democrats. That is the efficiency
# gap's own convention, and the other two are oriented to match it so the
# three can be read on one page without a footnote per column.
#
# efficiency gap   (seat share - 50) - 2 * (vote share - 50), the simple form
# mean - median    the median district's share minus the mean district's share
# declination      Warrington (2018): the angle between the two halves of the
#                  sorted vote-share curve, scaled to run roughly -1 to 1. It
#                  is undefined where every district falls on one side of 50,
#                  which is a finding rather than a defect.
decl <- function(v) {
  v  <- sort(v) / 100
  lo <- v[v < 0.5]; hi <- v[v > 0.5]
  if (!length(lo) || !length(hi)) return(NA_real_)
  N  <- length(v)
  th <- atan((0.5 - mean(lo)) / (length(lo) / (2 * N)))
  gm <- atan((mean(hi) - 0.5) / (length(hi) / (2 * N)))
  (2 / pi) * (th - gm)
}

st <- do.call(rbind, lapply(split(cd, cd$state), function(s) {
  v <- s$dem_share
  data.frame(state = s$state[1], n = nrow(s),
             vote = round(mean(v), 1), seats = sum(v > 50),
             seat_pct = round(100 * mean(v > 50), 1),
             mm  = round(median(v) - mean(v), 1),
             dec = round(decl(v), 2))
}))
st$eg <- round((st$seat_pct - 50) - 2 * (st$vote - 50), 1)
# Six districts is the floor for scoring: below it a single seat moves the seat
# share by more than sixteen points, and none of these statistics means much.
big <- st[st$n >= 6, ]
sv  <- function(ab, v) big[[v]][big$state == ab]
pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
row_of <- function(ab) paste(sprintf("%.1f",
  sort(cd$dem_share[cd$state == ab], decreasing = TRUE)), collapse = "  ")

# how far the three rankings actually agree, computed rather than asserted
rho <- function(a, b) round(cor(big[[a]], big[[b]], method = "spearman",
                                use = "complete.obs"), 2)
NODEC <- big$state[is.na(big$dec)]

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
o <- cd[cd$district %in% c("NC-01", "ME-02"),
        c("district", "democrat", "republican", "dem_share", "pres_party",
          "house_rep_party")]
names(o) <- c("district", "Harris %", "Trump %",
              "Democratic % of the two-party vote", "presidential winner",
              "party of the member elected")
o

## ---- statstab
KEY <- c("NC", "WI", "TN", "GA", "LA", "MA", "PA")
o <- big[match(KEY, big$state), c("state", "n", "vote", "seats", "eg", "mm", "dec")]
o$dec <- ifelse(is.na(o$dec), "undefined", pc(o$dec, 2))
names(o) <- c("state", "districts", "Democratic vote %", "Democratic seats",
              "efficiency gap", "mean minus median", "declination")
o

## ---- sp-static
s <- big[order(-big$eg), ]
par(mar = c(4.2, 4.4, 1, 1))
plot(NA, xlim = c(12, 92), ylim = c(0.5, nrow(s) + 0.5), yaxt = "n",
     xlab = "Democratic share of the two-party presidential vote (%)", ylab = "")
axis(2, at = seq_len(nrow(s)), labels = s$state, las = 1, cex.axis = 0.55)
abline(v = 50, lty = 2)
for (i in seq_len(nrow(s))) {
  v <- cd$dem_share[cd$state == s$state[i]]
  points(v, rep(i, length(v)), pch = 19, cex = 0.55,
         col = ifelse(v > 50, "#2166AC", "#B2182B"))
}

## ---- sp-d3
# The shared chart library. One band per state, one dot per district: the
# packing-and-cracking signature is a shape in a strip of dots, so this is the
# form that shows it. First figure in the document, so it loads d3 too.
s   <- big[order(-big$eg), ]
spd <- do.call(rbind, lapply(s$state, function(a) {
  z <- cd[cd$state == a, ]
  data.frame(state = a, district = z$district, share = round(z$dem_share, 1),
             side = ifelse(z$dem_share > 50, "Democratic district",
                                             "Republican district"),
             stringsAsFactors = FALSE)
}))
dd_fig("sp", "dot", spd,
  size = list(w = 760),
  rowHeight = 21, r = 3.6,
  y = list(field = "state"),
  x = list(field = "share", domain = c(12, 92), ticks = 9, fmt = "d",
           label = "Democratic share of the two-party presidential vote (%)"),
  series = list(field = "side", classes = list(
    "Democratic district" = "dem", "Republican district" = "gop")),
  legend = TRUE,
  annotations = list(list(type = "vline", x = 50)),
  tip = dd_tip(c(state = "state", share = "Democratic share"),
               fmt = c(share = "pct1"), title = "district"))

## ---- egmap-prep
sr <- read.csv("data/derived/seat_rings.csv",  stringsAsFactors = FALSE)
sm <- read.csv("data/derived/seat_states.csv", stringsAsFactors = FALSE)
# `seats` in the map file is the House delegation; `seats` in st is Democratic
# seats won -- rename before the merge so neither is silently suffixed
names(sm)[names(sm) == "seats"] <- "house_seats"
sm <- merge(sm, st, by = "state", all.x = TRUE)
# the brief scores the efficiency gap on states with six districts or more;
# the map draws the rest in grey rather than pretending one at-large seat has
# a gap to measure
RAMP_R <- colorRampPalette(c("#F4F3F1", "#C41230"))(101)
RAMP_D <- colorRampPalette(c("#F4F3F1", "#2C7FB8"))(101)
tt <- pmin(abs(sm$eg) / 30, 1)
sm$fill <- ifelse(sm$n < 6, "#DBDBDB",
           ifelse(sm$eg < 0, RAMP_R[round(tt * 100) + 1],
                             RAMP_D[round(tt * 100) + 1]))
# ink by the fill's own luminance, not by ramp position: the blue ramp is
# lighter than the red at the same depth, so a depth threshold flips to white
# too early on blue.  White needs 3:1 against the fill; sRGB luminance under
# ~0.30 gives that with room to spare.
lum <- apply(col2rgb(sm$fill) / 255, 2, function(v) {
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
})
sm$ink <- ifelse(lum < 0.30, "#FFFFFF", "#333333")
sxr <- range(sr$x); syr <- range(sr$y)

## ---- egmap-static
BAND <- 92                                   # legend band below the map
par(mar = rep(0.2, 4))
plot(NA, xlim = sxr, ylim = c(syr[2] + BAND, syr[1]), asp = 1,
     axes = FALSE, ann = FALSE)
o <- order(-sm$house_seats)                 # big states first, so no edge hides
for (a in sm$state[o]) {
  pr <- split(sr[sr$state == a, ], sr$part[sr$state == a])
  xs <- head(unlist(lapply(pr, function(z) c(z$x, NA)), use.names = FALSE), -1)
  ys <- head(unlist(lapply(pr, function(z) c(z$y, NA)), use.names = FALSE), -1)
  polypath(xs, ys, col = sm$fill[sm$state == a], border = "#FFFFFF", lwd = 0.6,
           rule = "evenodd")
}
text(sm$label_x, sm$label_y, sm$state, cex = 0.52, font = 2, col = sm$ink)
legend(sxr[1], syr[2] + 14, xjust = 0, yjust = 0, horiz = TRUE,
       c("map favors Republicans", "map favors Democrats", "under six districts: not scored"),
       fill = c("#C41230", "#2C7FB8", "#DBDBDB"), border = NA, bty = "n",
       cex = 0.6, x.intersp = 0.6)
text(sxr[1] + 8, syr[2] + 62, adj = c(0, 0), cex = 0.58, col = "#555555",
     labels = "area = House seats, 2020 apportionment; depth of color = size of the efficiency gap")

## ---- egmap-d3
# Rounded to the pixel and deduplicated for the embed, the way the mapping
# chapter does it: neighbours round a shared edge to the same pixels, so the
# borders stay shared.
pth <- vapply(split(sr, sr$state), function(z) {
  paste(vapply(split(z, z$part), function(r) {
    x <- round(r$x); y <- round(r$y)
    keep <- c(TRUE, diff(x) != 0 | diff(y) != 0)
    paste0("M", paste0(x[keep], ",", y[keep], collapse = "L"), "Z")
  }, character(1)), collapse = "")
}, character(1))
sj <- sm[match(names(pth), sm$state), ]
SD <- paste(sprintf(
  '{"st":"%s","nm":"%s","seats":%d,"vote":%s,"eg":%s,"f":"%s","k":"%s","lx":%.1f,"ly":%.1f,"p":"%s"}',
  sj$state, sj$name, sj$house_seats,
  sprintf("%.1f", sj$vote),
  ifelse(sj$n < 6 | is.na(sj$eg), "null", sprintf("%.1f", sj$eg)),
  sj$fill, sj$ink, sj$label_x, sj$label_y, pth), collapse = ",")
cat(paste0('
<div id="egm" style="position:relative;margin:1em 0"></div>
<style>
/* Dark labels lie on pale-to-mid fills; a paper-colored halo keeps them
   legible on every fill in both themes without recoloring any fill.  The
   white labels on the saturated fills get NO halo: a paper stroke touches
   the glyph, so the checker (correctly) scores white-on-paper — and white
   on a saturated fill passes on its own.  Verify with
   _lib/check-contrast.js on BOTH themes after touching this figure. */
#egm text[fill="#333333" i] { paint-order:stroke; stroke:var(--paper);
            stroke-width:3px; stroke-linejoin:round; }
</style>
<script>
(function(){
const S=[', SD, '];
const X0=', floor(sxr[1]), ',Y0=', floor(syr[1]),
',W=', ceiling(diff(sxr)) + 2, ',H=', ceiling(diff(syr)) + 2, ';
const box=d3.select("#egm");
const svg=box.append("svg").attr("viewBox",`${X0} ${Y0} ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="Area = House seats, 2020 apportionment. Red: the map favors "+
  "Republicans; blue: Democrats; deeper = larger efficiency gap; grey: under "+
  "six districts. <i>Hover a state for its numbers.</i>";
cap.html(DEF);
const g=svg.append("g");
g.selectAll("path").data(S).join("path").attr("d",d=>d.p)
  .attr("fill",d=>d.f).attr("stroke","#fff").attr("stroke-width",0.8)
  .on("mouseenter",function(e,d){
    d3.select(this).attr("stroke","#111").attr("stroke-width",1.6).raise();
    cap.html("<b>"+d.nm+"</b>: "+d.seats+" seat"+(d.seats>1?"s":"")+
      (d.eg===null
        ? ". Under six districts, so this brief does not score its gap."
        : ". "+d.vote.toFixed(1)+"% Democratic two-party vote; efficiency gap "+
          (d.eg>0?"+":"")+d.eg.toFixed(1)+" ("+
          (d.eg<0?"favors Republicans":"favors Democrats")+")."));})
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#fff").attr("stroke-width",0.8);
    cap.html(DEF);});
svg.append("g").attr("pointer-events","none").selectAll("text")
  .data(S).join("text")
  .attr("x",d=>d.lx).attr("y",d=>d.ly).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("font-weight","600")
  .attr("fill",d=>d.k).text(d=>d.st);
})();
</script>'))

## ---- ts-static
sc <- big[!is.na(big$dec), ]
par(mar = c(4.4, 4.4, 1.0, 1))
plot(NA, xlim = range(sc$dec) + c(-0.08, 0.10), ylim = range(big$eg) + c(-4, 4),
     las = 1, xlab = "declination (negative favors Republicans)",
     ylab = "efficiency gap")
abline(h = 0, v = 0, col = "#BBBBBB")
points(sc$dec, sc$eg, pch = 19, cex = 1.15,
       col = ifelse(sc$eg < 0, "#C41230", "#2c7fb8"))
text(sc$dec, sc$eg, sc$state, pos = 4, offset = 0.35, cex = 0.66,
     col = "#333333")

## ---- ts-d3
sc <- big[!is.na(big$dec), ]
tsd <- data.frame(state = sc$state, dec = sc$dec, eg = sc$eg,
                  lbl = sc$state,
                  cls = ifelse(sc$eg < 0, "gop", "dem"),
                  stringsAsFactors = FALSE)
dd_fig("ts", "scatter", tsd, d3 = FALSE,
  size = list(w = 760, h = 460),
  r = 5.5, opacity = 0.85,
  x = list(field = "dec", domain = c(-0.68, 0.62), ticks = 8, fmt = "f2",
           label = "declination (negative favors Republicans)"),
  y = list(field = "eg", ticks = 7, fmt = "signed0",
           label = "efficiency gap"),
  annotations = list(list(type = "vline", x = 0, dash = FALSE),
                     list(type = "hline", y = 0, dash = FALSE)),
  tip = dd_tip(c(eg = "efficiency gap", dec = "declination"),
               fmt = c(eg = "signed1", dec = "f2"), title = "state"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
