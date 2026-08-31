# census-coverage-code.R -- chunk bodies for census-coverage-brief.Rmd
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

rd <- function(f, ...) read.csv(file.path("data/derived", f),
                                stringsAsFactors = FALSE, ...)
st  <- rd("states.csv",     colClasses = c(fips = "character"))
mp  <- rd("map_states.csv", colClasses = c(fips = "character", pts = "character"))
ins <- rd("map_insets.csv")
bl  <- rd("map_bins.csv")
race   <- rd("race.csv")
ten    <- rd("tenure.csv")
agesex <- rd("age_sex.csv")
age    <- rd("age.csv")
fc     <- rd("facts.csv")
comp   <- rd("components.csv")
ck     <- rd("checks.csv")

F  <- function(k) fc$value[fc$key == k]
FN <- function(k) as.numeric(F(k))
nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
sg <- function(x) paste0(ifelse(as.numeric(x) > 0, "+", ""), p2(x), "%")
S  <- function(nm, col) st[[col]][st$state == nm]

# the two demographic tables, at the census this brief is about
A20 <- agesex[agesex$year == 2020, ]
T20 <- ten[ten$year == 2020, ]
G   <- function(d, g) d$est[d$group == g]
GR  <- function(g) race$est_2020[race$group == g]
GR10 <- function(g) race$est_2010[race$group == g]

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# ---- the map's two channels -------------------------------------------------
# FILL is the number the Bureau published. OUTLINE is whether the Bureau put its
# mark on that number. Six fill bins, diverging about zero: rust for an
# undercount, slate for an overcount, pale in the middle where the number is
# small in either direction.
RAMP <- c("#8A3B2C", "#C08268", "#EBD6CB", "#C3D8DE", "#5F92A2", "#1C4C5C")
SIGB <- "#111111"      # the outline a marked state gets
INSB <- "#b9b9b9"      # and the one everyone else gets
ACC  <- "#1C4C5C"; WARN <- "#8A3B2C"

MW <- 1000; MH <- 620                      # the borrowed drawing frame
mdec <- function(p) {                      # "x0 y0 dx dy ..." -> coordinates
  v <- as.integer(strsplit(p, " ", fixed = TRUE)[[1]])
  list(x = cumsum(v[c(TRUE, FALSE)]), y = cumsum(v[c(FALSE, TRUE)]))
}
# The District of Columbia is a handful of pixels wide at this scale and the
# brief names it more than once, so it gets a leader out to open water. The
# anchor is its own ring rather than a typed coordinate.
dcz  <- mdec(mp$pts[mp$fips == "11"][1])
DCX  <- mean(range(dcz$x)); DCY <- mean(range(dcz$y))
DCLX <- DCX + 62; DCLY <- DCY - 48

# recomputed here rather than trusted from the numbers written beside them
N_UNDER <- sum(st$direction == "undercount")
N_OVER  <- sum(st$direction == "overcount")
N_NONE  <- sum(!st$bureau_states_it)
stopifnot(N_UNDER == FN("n_under"), N_OVER == FN("n_over"),
          N_NONE == FN("n_none"), nrow(st) == 51)

UND <- st$state[st$direction == "undercount"]
OVR <- st$state[st$direction == "overcount"]

## ---- checks
data.frame(Check = ck$check, Value = ck$value)

## ---- map-d3
rg <- paste(sprintf('["%s","%s"]', mp$fips, mp$pts), collapse = ",")
at <- paste(sprintf('"%s":["%s",%d,%d,%s,%s,%s,%s]',
              st$fips, st$state, st$bin, as.integer(st$bureau_states_it),
              sprintf("%.2f", st$est_2020), sprintf("%.2f", st$se_2020),
              st$implied_people, st$self_response), collapse = ",")
lg <- paste(sprintf('["%s","%s"]', RAMP, bl$label), collapse = ",")
bx <- paste(sprintf('{"n":"%s","x":%d,"y":%d,"w":%d,"h":%d}',
                    ins$label, ins$x0, ins$y0,
                    ins$x1 - ins$x0, ins$y1 - ins$y0), collapse = ",")
cat(sprintf('
<script src="../../_lib/d3.v7.min.js"></script>
<div id="covmap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const RG=[%s], AT={%s}, LG=[%s], BX=[%s];
const W=%d, MH=%d, TOP=170, H=MH+TOP;
const SIGB="%s", INSB="%s";
const svg=d3.select("#covmap").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit;display:block");
function path(p){
  const v=p.split(" ");let x=0,y=0,s="";
  for(let i=0;i<v.length;i+=2){x+=+v[i];y+=+v[i+1];s+=(i?"L":"M")+x+","+(H-y);}
  return s+"Z";
}
const g=svg.append("g");
g.selectAll("path").data(RG).join("path")
  .attr("d",r=>path(r[1]))
  .attr("fill",r=>LG[AT[r[0]][1]-1][0])
  .attr("stroke",r=>AT[r[0]][2]?SIGB:INSB)
  .attr("stroke-width",r=>AT[r[0]][2]?1.8:0.5);
svg.append("line").attr("x1",%.1f).attr("y1",H-%.1f).attr("x2",%.1f)
  .attr("y2",H-%.1f).attr("stroke","#666").attr("stroke-width",0.9);
svg.append("circle").attr("cx",%.1f).attr("cy",H-%.1f).attr("r",2.4).attr("fill","#666");
svg.append("text").attr("x",%.1f).attr("y",H-%.1f+5).attr("font-size","16px")
  .attr("fill","#444").text("D.C.");
BX.forEach(b=>{
  svg.append("rect").attr("x",b.x).attr("y",H-b.y-b.h).attr("width",b.w)
    .attr("height",b.h).attr("fill","none").attr("stroke","#c8c8c8");
  svg.append("text").attr("x",b.x).attr("y",H-b.y-b.h-7).attr("font-size","15px")
    .attr("fill","#777").text(b.n+" \\u2014 inset, not to scale");
});
const L=svg.append("g");
L.append("text").attr("x",30).attr("y",36).attr("font-size","22px")
  .attr("font-weight","600").attr("fill","#333")
  .text("net coverage error, 2020 census");
LG.forEach((d,i)=>{
  L.append("rect").attr("x",30+i*118).attr("y",50).attr("width",114).attr("height",24)
    .attr("fill",d[0]).attr("stroke","#fff");
  L.append("text").attr("x",87+i*118).attr("y",94).attr("text-anchor","middle")
    .attr("font-size","15px").attr("fill","#555").text(d[1]);
});
L.append("rect").attr("x",30).attr("y",114).attr("width",28).attr("height",22)
  .attr("fill","#efefef").attr("stroke",SIGB).attr("stroke-width",1.8);
L.append("text").attr("x",68).attr("y",131).attr("font-size","17px").attr("fill","#444")
  .text("heavy outline: the Bureau states a direction for this state (%d of them)");
L.append("rect").attr("x",30).attr("y",142).attr("width",28).attr("height",22)
  .attr("fill","#efefef").attr("stroke",INSB).attr("stroke-width",0.6);
L.append("text").attr("x",68).attr("y",159).attr("font-size","17px").attr("fill","#444")
  .text("thin outline: it publishes the number and says nothing (%d)");
const box=d3.select("#covmap");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
g.selectAll("path").on("mousemove",function(e,r){
  const a=AT[r[0]], est=+a[3], se=+a[4], ppl=+a[5];
  const w = est<0 ? Math.abs(ppl).toLocaleString()+" missed"
                  : Math.abs(ppl).toLocaleString()+" in excess";
  tip.style("opacity",1).html(`<b>${a[0]}</b><br>`+
    `${est>0?"+":""}${est.toFixed(2)}%% \\u00b1 ${se.toFixed(2)}`+
    `${a[2]?"":" &mdash; published, but the Bureau states no direction"}<br>`+
    `implies ${w}<br>self-response ${a[6]}%%`)
   .style("left",Math.min(e.offsetX+14,box.node().clientWidth-250)+"px")
   .style("top",(e.offsetY-8)+"px");
}).on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', rg, at, lg, bx, MW, MH, SIGB, INSB,
   DCX, DCY, DCLX, DCLY, DCX, DCY, DCLX + 5, DCLY,
   N_UNDER + N_OVER, N_NONE))

## ---- map-static
MTOP <- 170
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0, MW), ylim = c(0, MH + MTOP), asp = 1, axes = FALSE, ann = FALSE)
# fills first, then the marked outlines on top so a heavy border is never
# clipped by the neighbour drawn after it
for (i in seq_len(nrow(mp))) {
  z <- mdec(mp$pts[i])
  polygon(z$x, z$y, col = RAMP[mp$bin[i]], border = INSB, lwd = 0.3)
}
for (i in which(mp$states_it)) {
  z <- mdec(mp$pts[i]); polygon(z$x, z$y, col = NA, border = SIGB, lwd = 0.9)
}
segments(DCX, DCY, DCLX, DCLY, col = "#666666", lwd = 0.6)
points(DCX, DCY, pch = 19, cex = 0.25, col = "#666666")
text(DCLX + 3, DCLY, "D.C.", cex = 0.45, col = "#444444", adj = 0)
rect(ins$x0, ins$y0, ins$x1, ins$y1, border = "#c8c8c8", lwd = 0.5)
text(ins$x0, ins$y1 + 8, paste(ins$label, "- inset, not to scale"),
     cex = 0.42, col = "#777", adj = 0)
text(30, MH + MTOP - 30, "net coverage error, 2020 census",
     cex = 0.62, font = 2, col = "#333", adj = 0)
for (i in seq_along(RAMP)) {
  rect(30 + (i-1)*118, MH + MTOP - 74, 144 + (i-1)*118, MH + MTOP - 50,
       col = RAMP[i], border = "#fff", lwd = 0.4)
  text(87 + (i-1)*118, MH + MTOP - 92, bl$label[i], cex = 0.4, col = "#555")
}
key <- list(list(SIGB, 1.4, sprintf(
       "heavy outline: the Bureau states a direction for this state (%d of them)",
       N_UNDER + N_OVER)),
     list(INSB, 0.4, sprintf(
       "thin outline: it publishes the number and says nothing (%d)", N_NONE)))
for (i in 1:2) {
  yy <- MH + MTOP - 136 - (i-1)*28
  rect(30, yy, 58, yy + 22, col = "#efefef", border = key[[i]][[1]],
       lwd = key[[i]][[2]])
  text(68, yy + 11, key[[i]][[3]], cex = 0.44, col = "#444", adj = 0)
}

## ---- contrast
pick <- c("Illinois", "Louisiana", "Ohio", "District of Columbia", "Montana")
d <- st[match(pick, st$state), ]
data.frame(State = d$state,
           Estimate = sg(d$est_2020),
           Margin_printed_beside_it = p2(d$se_2020),
           The_Bureau_says = ifelse(d$bureau_states_it,
                                    ifelse(d$est_2020 < 0, "undercounted",
                                           "overcounted"),
                                    "nothing"))

## ---- demofig
# One picture of every 2020 demographic estimate this brief has, on one axis,
# with the margin the Bureau prints beside it drawn either side. Not a
# confidence interval the reader is asked to interpret -- the published hedge,
# to scale, so that "the Bureau hedges these far less than the states" is
# something you can see rather than something you have to take on trust.
sel_race <- c("Hispanic or Latino", "Black or African American",
              "Some Other Race", "On Reservation",
              "American Indian or Alaska Native", "Asian",
              "Non-Hispanic White alone")
D <- rbind(
  data.frame(grp = paste0(sub("-to-", " to ", A20$group[grepl("males|females", A20$group)])),
             est = A20$est[grepl("males|females", A20$group)],
             se  = A20$se[grepl("males|females", A20$group)],
             sig = A20$bureau_states_it[grepl("males|females", A20$group)],
             blk = "Age and sex"),
  data.frame(grp = c("Under 5", "5 to 9", "10 to 17"),
             est = age$est[match(c("0 to 4", "5 to 9", "10 to 17"), age$group)],
             se  = age$se[match(c("0 to 4", "5 to 9", "10 to 17"), age$group)],
             sig = age$bureau_states_it[match(c("0 to 4", "5 to 9", "10 to 17"), age$group)],
             blk = "Children"),
  data.frame(grp = T20$group[T20$group != "Total"],
             est = T20$est[T20$group != "Total"],
             se  = T20$se[T20$group != "Total"],
             sig = T20$bureau_states_it[T20$group != "Total"],
             blk = "Tenure"),
  data.frame(grp = sel_race,
             est = race$est_2020[match(sel_race, race$group)],
             se  = race$se_2020[match(sel_race, race$group)],
             sig = race$bureau_states_it[match(sel_race, race$group)],
             blk = "Race and\nHispanic origin"))
D$grp[D$grp == "On Reservation"] <- "AIAN, on reservation"
D$grp[D$grp == "American Indian or Alaska Native"] <- "AIAN, all"
D <- D[rev(seq_len(nrow(D))), ]           # first block ends up at the top
D$y <- seq_len(nrow(D))

op <- par(mar = c(3.8, 9.6, 1.0, 7.6), mgp = c(2.3, 0.6, 0))
BAR <- 2                                  # the published margin, either side
xr <- range(c(D$est - BAR * D$se, D$est + BAR * D$se))
plot(NA, xlim = xr, ylim = c(0.4, nrow(D) + 0.6), axes = FALSE, ann = FALSE)
abline(v = 0, col = "#999999")
abline(v = pretty(xr), col = "#eeeeee", lty = 3)
segments(D$est - BAR * D$se, D$y, D$est + BAR * D$se, D$y,
         col = ifelse(D$sig, "#555555", "#c4c4c4"), lwd = 1.1)
points(D$est, D$y, pch = 21, cex = 1.0, lwd = 0.7,
       col = ifelse(D$sig, "#111111", "#b0b0b0"),
       bg = ifelse(D$sig, ifelse(D$est < 0, WARN, ACC), "#f2f2f2"))
axis(1, at = pretty(xr), labels = paste0(pretty(xr), "%"),
     cex.axis = 0.72, col = "#888888", col.axis = "#444444")
mtext("net coverage error (%)", side = 1, line = 2.1, cex = 0.75, col = "#444444")
text(par("usr")[1] - 0.10 * diff(xr), D$y, D$grp, adj = 1, xpd = NA,
     cex = 0.66, col = "#333333")
# the block labels, once each, against the right margin
for (b in unique(D$blk)) {
  yy <- mean(D$y[D$blk == b])
  text(par("usr")[2] + 0.04 * diff(xr), yy, b, adj = 0, xpd = NA,
       cex = 0.66, font = 2, col = "#666666")
}
par(op)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
