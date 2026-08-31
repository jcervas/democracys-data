# data-sources-code.R -- chunk bodies for data-sources-brief.Rmd
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

naive <- read.csv("data/derived/pres2024_counties.csv", stringsAsFactors = FALSE)
cty <- read.csv("data/derived/pres2024_counties.csv", stringsAsFactors = FALSE,
                colClasses = c(county_fips = "character"))
old <- read.csv("data/derived/pres2020_counties.csv", stringsAsFactors = FALSE,
                colClasses = c(county_fips = "character"))
cen <- read.csv("data/derived/census_counties.csv", stringsAsFactors = FALSE,
                colClasses = c(fips = "character"))
stt <- read.csv("data/derived/pres2024_states.csv", stringsAsFactors = FALSE)

pc <- function(x, k = 2) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# ---- the three joins the chapter walks ----
cty$key <- paste(cty$state_name, cty$county_name)
cen$key <- paste(cen$state_name, cen$name)
by_name <- merge(cty, cen, by.x = "county_name", by.y = "name")
by_key  <- merge(cty, cen, by = "key")
by_fips <- merge(cty, cen, by.x = "county_fips", by.y = "fips")
wrong   <- by_fips[by_fips$county_name != by_fips$name, ]

nw   <- sum(cty$county_name == "Washington County")
anch <- by_fips$total_votes[by_fips$county_fips == "02020"]

# ---- the year-over-year join on FIPS ----
sw <- merge(old[, c("county_fips", "state_name", "county_name",
                    "votes_dem", "votes_gop")],
            cty[, c("county_fips", "votes_dem", "votes_gop")],
            by = "county_fips", suffixes = c("_20", "_24"))
CTSHARE <- length(intersect(old$county_fips[old$state_name == "Connecticut"],
                            cty$county_fips[cty$state_name == "Connecticut"]))

# ---- what became of each reporting unit between 2020 and 2024 ----
matched  <- intersect(old$county_fips, cty$county_fips)
ak20 <- sum(!old$county_fips %in% cty$county_fips & old$state_name == "Alaska")
ct20 <- sum(!old$county_fips %in% cty$county_fips & old$state_name == "Connecticut")
ak24 <- sum(!cty$county_fips %in% old$county_fips & cty$state_name == "Alaska")
ct24 <- sum(!cty$county_fips %in% old$county_fips & cty$state_name == "Connecticut")
dc_new <- sum(!cty$county_fips %in% old$county_fips &
            cty$state_name == "District of Columbia")
Lname <- c("units the two files share", "the District of Columbia",
           "Alaska, 2020 numbering", "Connecticut counties",
           "nothing in the 2020 file")
Lk    <- c(length(matched) - 1, 1, ak20, ct20, ak24 + ct24 + dc_new)
Rname <- c("units the two files share", "DC Ward 1, code 11001",
           "nothing in the 2024 file", "Alaska, 2024 numbering",
           "Connecticut planning regions", "DC Wards 2 to 8")
Rk    <- c(length(matched) - 1, 1, ak20 + ct20, ak24, ct24, dc_new)
TOT   <- sum(Lk)
GAP   <- 0.058          # the small nodes need room or their labels collide
# A ribbon's thickness must be its COUNT, and must be the same at both ends.
# Deriving thickness from the node height instead (with a floor on the node)
# silently rescaled each end by a different factor: the 8-unit Connecticut
# ribbon left one side 5.6x thicker than it arrived at the other, and was drawn
# wider than the 9-unit ribbon beside it. Width now comes from k alone.
RMIN  <- 0.006                     # visibility floor, disclosed in the caption
rib <- data.frame(
  s = c(1, 2, 3, 4, 5, 5, 5), t = c(1, 2, 3, 3, 4, 5, 6),
  k = c(length(matched) - 1, 1, ak20, ct20, ak24, ct24, dc_new),
  kind = c("kept", "collision", "dropped", "dropped",
           "invented", "invented", "invented"), stringsAsFactors = FALSE)
rib$w      <- pmax(rib$k / TOT, RMIN)      # identical at source and target
rib$floored <- rib$k / TOT < RMIN          # drawn at the floor, not to scale
# a node is exactly as tall as the ribbons it carries, so nothing drifts
nodeh <- function(side, m) sapply(seq_len(m), function(j) sum(rib$w[side == j]))
Lh  <- nodeh(rib$s, length(Lname)); Rh <- nodeh(rib$t, length(Rname))
Ly0 <- cumsum(c(0, head(Lh + GAP, -1))); Ry0 <- cumsum(c(0, head(Rh + GAP, -1)))
LL  <- data.frame(k = Lk, h = Lh, y0 = Ly0)
RR  <- data.frame(k = Rk, h = Rh, y0 = Ry0)
ALH <- max(max(Ly0 + Lh), max(Ry0 + Rh))
so <- to <- numeric(nrow(rib))
for (i in seq_len(nrow(rib))) {
  j <- seq_len(i - 1)
  so[i] <- Ly0[rib$s[i]] + sum(rib$w[j][rib$s[j] == rib$s[i]])
  to[i] <- Ry0[rib$t[i]] + sum(rib$w[j][rib$t[j] == rib$t[i]])
}
rib$sy0 <- so; rib$sy1 <- so + rib$w
rib$ty0 <- to; rib$ty1 <- to + rib$w
# color encodes what BECAME of a reporting unit; the legend below names each
rib$col <- c(kept = "#BBBBBB", collision = "#C41230",
             dropped = "#e08214", invented = "#2c7fb8")[rib$kind]
RIBLEG <- data.frame(
  kind = c("kept", "collision", "dropped", "invented"),
  col  = c("#BBBBBB", "#C41230", "#e08214", "#2c7fb8"),
  lab  = c("same code, both files", "same code, different place",
           "in 2020, gone in 2024", "new in 2024, absent from 2020"),
  stringsAsFactors = FALSE)
RIBLEG$k <- sapply(RIBLEG$kind, function(z) sum(rib$k[rib$kind == z]))

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

## ---- fips
data.frame(
  step = c("What the code should look like", "What R made of it",
           "Rows now the wrong length", "Out of"),
  value = c("01001 (five characters, text)",
            paste0(class(naive$county_fips), ": ",
                   format(naive$county_fips[1], big.mark = "")),
            n(sum(nchar(naive$county_fips) == 4)), n(nrow(naive))))

## ---- fips-states
data.frame(states_affected = paste(
  sort(unique(naive$state_name[nchar(naive$county_fips) == 4])), collapse = ", "))

## ---- namejoin
data.frame(
  step = c("Rows of returns going in", "Official counties going in",
           "Rows coming out of a name join",
           "'Washington County' rows in the returns",
           "'Washington County' rows in the census list",
           "'Washington County' rows after the join"),
  value = c(n(nrow(cty)), n(nrow(cen)), n(nrow(by_name)),
            sum(cty$county_name == "Washington County"),
            sum(cen$name == "Washington County"),
            n(sum(by_name$county_name == "Washington County"))))

## ---- wrongjoin
w <- wrong[, c("county_fips", "county_name", "name", "total_votes")]
w$total_votes <- n(w$total_votes)
names(w) <- c("FIPS", "name in the returns", "name in the census list", "votes")
w

## ---- years
data.frame(
  quantity = c("Rows in the 2020 file", "Rows in the 2024 file",
               "2024 codes absent from 2020", "2020 codes absent from 2024",
               "Connecticut codes the two files share"),
  value = c(n(nrow(old)), n(nrow(cty)),
            n(sum(!cty$county_fips %in% old$county_fips)),
            n(sum(!old$county_fips %in% cty$county_fips)),
            CTSHARE))

## ---- bookkeeping
data.frame(
  step = c("Reporting units in the 2024 file", "Reporting units in the 2020 file",
           "Units that matched", "States absent from the result entirely"),
  value = c(n(nrow(cty)), n(nrow(old)), n(nrow(sw)),
            paste(setdiff(cty$state_name, sw$state_name), collapse = " and ")))

## ---- alluvial-static
par(mar = c(0.2, 0.2, 1.6, 0.2))
plot(NA, xlim = c(-0.62, 1.62), ylim = c(ALH + 0.22, -0.04), axes = FALSE,
     xlab = "", ylab = "")
mtext("the 2020 file", side = 3, at = 0.02, adj = 1, line = 0.1, cex = 0.8,
      font = 2)
mtext("the 2024 file", side = 3, at = 0.98, adj = 0, line = 0.1, cex = 0.8,
      font = 2)
xs <- seq(0.06, 0.94, length.out = 40)
sm <- (xs - 0.06) / 0.88; sm <- sm * sm * (3 - 2 * sm)
for (i in seq_len(nrow(rib))) {
  polygon(c(xs, rev(xs)),
          c(rib$sy0[i] + (rib$ty0[i] - rib$sy0[i]) * sm,
            rev(rib$sy1[i] + (rib$ty1[i] - rib$sy1[i]) * sm)),
          col = paste0(rib$col[i], "AA"), border = NA)
}
rect(0.00, LL$y0, 0.06, LL$y0 + LL$h, col = "#555555", border = NA)
rect(0.94, RR$y0, 1.00, RR$y0 + RR$h, col = "#555555", border = NA)
text(-0.03, LL$y0 + LL$h / 2, paste0(Lname, "  (", sapply(Lk, n), ")"),
     adj = 1, cex = 0.62, col = "#333333")
text(1.03, RR$y0 + RR$h / 2, paste0(Rname, "  (", sapply(Rk, n), ")"),
     adj = 0, cex = 0.62, col = "#333333")
text(0.5, rib$sy0[2] - 0.035, cex = 0.62, col = "#C41230",
     labels = paste0("the collision: ", n(rib$k[2]),
                     " row joined to a different place"))

# ---- legend: color was carrying four meanings with nothing to decode it ----
lxc <- c(-0.42, 0.46)[c(1, 2, 1, 2)]                 # two columns, two rows
lyc <- ALH + c(0.055, 0.055, 0.105, 0.105)
rect(lxc, lyc - 0.013, lxc + 0.045, lyc + 0.013, col = RIBLEG$col, border = NA)
text(lxc + 0.056, lyc, paste0(RIBLEG$lab, " (", sapply(RIBLEG$k, n), ")"),
     adj = 0, cex = 0.58, col = "#333333")
text(0.5, ALH + 0.165, adj = c(0.5, 0.5), cex = 0.55, col = "#777777",
     labels = paste0("Ribbon thickness is the number of reporting units, and is",
                     " the same at both ends. The ", sum(rib$floored),
                     " thinnest ribbons are drawn at a minimum width"))
text(0.5, ALH + 0.198, adj = c(0.5, 0.5), cex = 0.55, col = "#777777",
     labels = paste0("so they stay visible; every other ribbon is to scale, ",
                     "across a range of ", n(max(rib$k)), " to ",
                     n(min(rib$k)), " units."))

## ---- alluvial-d3
# This chunk carries the ONE d3 <script src> for the document.
rrows <- paste(sprintf(
  '{"a":%.5f,"b":%.5f,"c":%.5f,"d":%.5f,"k":%d,"col":"%s","kind":"%s","f":"%s","t":"%s"}',
  rib$sy0, rib$sy1, rib$ty0, rib$ty1, rib$k, rib$col, rib$kind,
  Lname[rib$s], Rname[rib$t]), collapse = ",")
lrows <- paste(sprintf('{"y":%.5f,"h":%.5f,"t":"%s","k":%d}',
                       LL$y0, LL$h, Lname, Lk), collapse = ",")
rrws  <- paste(sprintf('{"y":%.5f,"h":%.5f,"t":"%s","k":%d}',
                       RR$y0, RR$h, Rname, Rk), collapse = ",")
lgrows <- paste(sprintf('{"c":"%s","l":"%s","k":%d}',
                        RIBLEG$col, RIBLEG$lab, RIBLEG$k), collapse = ",")
cat(paste0('
<div id="al" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const R=[', rrows, '], L=[', lrows, '], Q=[', rrws, '];
const LEG=[', lgrows, '];
const ALH=', ALH, ';
const W=760,M={t:34,r:220,b:56,l:220};
const bandH=310, H=M.t+M.b+bandH;
const box=d3.select("#al");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const xa=M.l,xb=W-M.r;
const y=v=>M.t+v/ALH*bandH;
svg.append("text").attr("x",xa+8).attr("y",18).attr("text-anchor","end")
  .attr("font-size","13px").attr("font-weight","600").text("the 2020 file");
svg.append("text").attr("x",xb-8).attr("y",18)
  .attr("font-size","13px").attr("font-weight","600").text("the 2024 file");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const f=d3.format(",");
const mid=(xa+xb)/2;
const path=d=>`M${xa},${y(d.a)} C${mid},${y(d.a)} ${mid},${y(d.c)} ${xb},${y(d.c)}`
  +` L${xb},${y(d.d)} C${mid},${y(d.d)} ${mid},${y(d.b)} ${xa},${y(d.b)} Z`;
svg.append("g").selectAll("path").data(R).join("path")
  .attr("d",path).attr("fill",d=>d.col).attr("fill-opacity",0.62)
  .on("mousemove",function(ev,d){
    d3.select(this).attr("fill-opacity",0.9);
    tip.style("opacity",1).html(
      `<b>${f(d.k)} reporting unit${d.k===1?"":"s"}</b><br>${d.f} &rarr; ${d.t}`)
      .style("left",Math.min(ev.offsetX+14,W-340)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",function(){ d3.select(this).attr("fill-opacity",0.62);
    tip.style("opacity",0); });
[[L,xa-10,-1],[Q,xb+10,1]].forEach(([S,tx,dir])=>{
  svg.append("g").selectAll("rect").data(S).join("rect")
    .attr("x",dir<0?xa-9:xb).attr("y",d=>y(d.y)).attr("width",9)
    .attr("height",d=>y(d.h)-M.t).attr("fill","#555");
  svg.append("g").selectAll("text").data(S).join("text")
    .attr("x",tx).attr("y",d=>y(d.y+d.h/2)+4)
    .attr("text-anchor",dir<0?"end":"start").attr("font-size","11px")
    .attr("fill","#333").text(d=>d.t+"  ("+f(d.k)+")");
});
const dc=R.find(d=>d.kind==="collision");
svg.append("text").attr("x",mid).attr("y",y(dc.a)-8).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("the collision: "+f(dc.k)+" row joined to a different place");
// legend: color was carrying four meanings with nothing to decode it
const lg=svg.append("g").attr("transform",`translate(28,${H-M.b+16})`);
let lxp=0;
LEG.forEach(d=>{
  lg.append("rect").attr("x",lxp).attr("y",-9).attr("width",16).attr("height",11)
    .attr("fill",d.c).attr("fill-opacity",0.62);
  const txt=d.l+" ("+f(d.k)+")";
  lg.append("text").attr("x",lxp+21).attr("y",0).attr("font-size","11px")
    .attr("fill","#444").text(txt);
  lxp+=31+txt.length*5.5;
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a ribbon for the number of reporting units it carries.</i></p>
'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
