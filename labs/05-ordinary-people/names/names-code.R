# names-code.R -- chunk bodies for names-brief.Rmd
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

rd  <- function(f, ...) read.csv(file.path("data/derived", f),
                                 stringsAsFactors = FALSE, ...)
fct <- rd("facts.csv")
F  <- function(k) fct$value[fct$name == k]                 # as it was written
FN <- function(k) as.numeric(F(k))

cover <- rd("cover.csv")
curve <- rd("curve.csv");         top25 <- rd("top25.csv")
uni   <- rd("unisex.csv");        rinfo <- rd("race_info.csv")
rext  <- rd("race_extremes.csv"); coll  <- rd("collision.csv")
bothw <- rd("both_ways.csv");     disat <- rd("disagreement_top.csv")
# The birth file, which is Social Security rather than the census. Only the
# flow-and-stock figure uses it.
alx   <- rd("ssa_alexa.csv")

RI <- function(w, col) rinfo[[col]][rinfo$which == w]
CL <- function(q) coll$one_in[coll$quantity == q]

n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# ---- one palette for this document ----------------------------------------
# One distinction is drawn in colour anywhere here, so one pair carries it:
# KIND, first name against last name. The two hues are far apart in both hue
# and lightness, so the figure survives being printed grey.
KIND <- c(`first name` = "#00666e", `last name` = "#8A3B2C")

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# JSON for the browser figures. Numbers are formatted HERE, in R, and passed
# through as finished values; the JavaScript never rounds anything, so the
# figures and the print versions below cannot disagree about a digit.
j_num <- function(x, k = 3) formatC(as.numeric(x), format = "f", digits = k)
j_row <- function(...) paste0("[", paste(..., sep = ","), "]")

## ---- sizes

data.frame(
  Table = c("First names", "Last names"),
  Names_published = c(n(F("first_names")), n(F("last_names"))),
  People_covered  = c(n(F("first_people")), n(F("last_people"))),
  Share_of_the_2020_census = paste0(
    c(pc(F("first_coverage_pct")), pc(F("last_coverage_pct"))), "%"),
  Swept_into_ALL_OTHER_NAMES = c(n(F("residual_first")), n(F("residual_last"))))

## ---- top-tab

tt <- top25[top25$rank <= 10, ]
data.frame(
  Rank        = tt$rank[tt$which == "first name"],
  First_name  = tt$name[tt$which == "first name"],
  People      = n(tt$count[tt$which == "first name"]),
  Last_name   = tt$name[tt$which == "last name"],
  People_     = n(tt$count[tt$which == "last name"]))

## ---- curve-d3

# ---------------------------------------------------------------------------
# Figure 1. Cumulative coverage against rank, both name kinds on one pair of
# axes. Log x, because everything that matters happens in the first thousand
# ranks and the tail is 150,000 long.
#
# This chunk carries the ONE d3 <script src> in the document; the two figures
# below use the library loaded here. A second copy would double the payload.
# ---------------------------------------------------------------------------
cv <- curve
mk <- function(w) paste0("[", paste(apply(cv[cv$which == w, ], 1, function(r)
  j_row(r[["rank"]], j_num(r[["cum_pct"]], 3))), collapse = ","), "]")
cat(paste0('
<div id="curve" style="position:relative;margin:0.6em 0 0.4em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const F=', mk("first name"), ', L=', mk("last name"), ';
const HF=', F("half_first"), ', HL=', F("half_last"), ';
const CF="', KIND[["first name"]], '", CL="', KIND[["last name"]], '";
const W=760,H=420,M={t:20,r:22,b:46,l:52};
const svg=d3.select("#curve").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([1,200000]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6,"~s"));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%"));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("how many names you take, most common first");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("share of the country covered");
// the half-way rule, fixed furniture: it does not move with the pointer
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#888").attr("stroke-dasharray","4,3");
svg.append("text").attr("x",W-M.r-4).attr("y",y(50)-5).attr("text-anchor","end")
  .attr("font-size","10.5px").attr("fill","#666").text("half the country");
const line=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
[[L,CL],[F,CF]].forEach(s=>{
  svg.append("path").datum(s[0]).attr("fill","none").attr("stroke",s[1])
    .attr("stroke-width",2.4).attr("d",line);
});
// Each curve passes through its own marker steeply, so the label goes to the
// side the curve is leaving rather than centred on it.
[[HF,CF,"first names",1.3,20,"start"],[HL,CL,"last names",0.77,-12,"end"]].forEach(m=>{
  svg.append("circle").attr("cx",x(m[0])).attr("cy",y(50)).attr("r",4.5)
    .attr("fill","#fff").attr("stroke",m[1]).attr("stroke-width",2);
  svg.append("text").attr("x",x(m[0]*m[3])).attr("y",y(50)+m[4])
    .attr("text-anchor",m[5]).attr("font-size","11px").attr("font-weight","600")
    .attr("fill",m[1]).text(m[0].toLocaleString()+" "+m[2]);
});
// NEITHER CURVE REACHES THE TOP, and a reader is owed the reason inside the
// figure rather than only in the prose: taking every published name still
// leaves out the people whose names fell under the threshold.
[[F,CF,-10],[L,CL,48]].forEach(s=>{
  const e=s[0][s[0].length-1];
  svg.append("text").attr("x",x(e[0])-6).attr("y",y(e[1])+s[2])
    .attr("text-anchor","end").attr("font-size","10.5px").attr("fill",s[1])
    .text("every published name: "+e[1].toFixed(1)+"%");
});
const lg=svg.append("g").attr("transform","translate("+(M.l+16)+","+(M.t+6)+")");
[["first names",CF,0],["last names",CL,17]].forEach(r=>{
  lg.append("line").attr("x1",0).attr("x2",20).attr("y1",r[2]).attr("y2",r[2])
    .attr("stroke",r[1]).attr("stroke-width",2.4);
  lg.append("text").attr("x",26).attr("y",r[2]+4).attr("font-size","11.5px")
    .attr("fill","#333").text(r[1]==CF?"first names":"last names");
});
// ---- the moving readout ----------------------------------------------------
const g=svg.append("g").attr("display","none");
const gl=g.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#111").attr("stroke-dasharray","3,3");
const dots=[CF,CL].map(c=>g.append("circle").attr("r",5).attr("fill",c));
const out=d3.select("#curve").append("p").attr("style",
  "font-size:0.9em;color:#222;margin:0.3em 0 0 0;min-height:2.4em");
const at=(S,r)=>{let i=0; while(i<S.length-1&&S[i][0]<r) i++; return S[i];};
function show(r){
  r=Math.max(1,Math.min(200000,r));
  const a=at(F,r), b=at(L,r);
  g.attr("display",null); gl.attr("x1",x(r)).attr("x2",x(r));
  dots[0].attr("cx",x(a[0])).attr("cy",y(a[1]));
  dots[1].attr("cx",x(b[0])).attr("cy",y(b[1]));
  // The pointer can reach rank 1, where "the 1 most common first names" reads
  // like a bug in the sentence rather than a value on the axis.
  out.html(a[0]==1
    ? "The single most common <b>first name</b> covers <b>"+a[1].toFixed(1)+
      "%</b> of everyone in the file. The most common <b>last name</b> covers <b>"+
      b[1].toFixed(1)+"%</b>."
    : "The <b>"+a[0].toLocaleString()+"</b> most common <b>first names</b> "+
      "cover <b>"+a[1].toFixed(1)+"%</b> of everyone in the file. The same number "+
      "of <b>last names</b> covers <b>"+b[1].toFixed(1)+"%</b>.");
}
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.l-M.r)
  .attr("height",H-M.t-M.b).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){show(Math.round(x.invert(d3.pointer(e,this)[0])));})
  .on("mouseleave",function(){g.attr("display","none");
    out.html("Move the pointer across the figure to read a rank off it.");});
out.html("Move the pointer across the figure to read a rank off it.");
})();
</script>'))

## ---- curve-static

# Same two curves over the same domain as the browser version, with the same
# two markers. Print cannot carry the pointer, so the readout is dropped and
# the caption says the browser version has it.
par(mar = c(4.0, 4.4, 0.6, 1.0), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = c(1, 2e5), ylim = c(0, 100), log = "x", las = 1, xaxt = "n",
     xlab = "how many names you take, most common first",
     ylab = "share of the country covered")
axis(1, at = 10^(0:5), labels = c("1", "10", "100", "1,000", "10,000", "100,000"))
abline(h = 50, lty = 2, col = "#888888")
for (w in c("last name", "first name")) {
  s <- curve[curve$which == w, ]
  lines(s$rank, s$cum_pct, col = KIND[[w]], lwd = 2.4)
}
# Both curves pass through their own marker steeply, so each label is set to
# the side the curve is leaving rather than centred on it: below and to the
# right for first names, above and to the left for last names.
for (m in list(list(FN("half_first"), "first name", "first names", 1.3, -6, 0),
               list(FN("half_last"),  "last name",  "last names",  0.77, 6, 1))) {
  points(m[[1]], 50, pch = 21, bg = "white", col = KIND[[m[[2]]]], cex = 1.2, lwd = 2)
  text(m[[1]] * m[[4]], 50 + m[[5]],
       sprintf("%s %s", format(m[[1]], big.mark = ","), m[[3]]),
       adj = m[[6]], cex = 0.66, font = 2, col = KIND[[m[[2]]]])
}
# Neither curve reaches the top; the end labels say so in the figure.
for (w in c("first name", "last name")) {
  s <- curve[curve$which == w, ]
  e <- s[nrow(s), ]
  text(e$rank, e$cum_pct + if (w == "first name") 4.5 else -14,
       sprintf("every published name: %s%%", pc(e$cum_pct)),
       adj = 1, cex = 0.6, col = KIND[[w]])
}
legend("topleft", c("first names", "last names"), col = KIND, lwd = 2.4,
       bty = "n", cex = 0.7, inset = c(0.02, 0.02))
text(2e5, 53, "half the country", adj = 1, cex = 0.6, col = "#666666")

## ---- cover-tab

data.frame(
  The_most_common = paste(n(cover$top_n), "names"),
  Of_first_names  = paste0(pc(cover$first_pct), "%"),
  Of_last_names   = paste0(pc(cover$last_pct), "%"))

## ---- coll-tab

data.frame(Two_random_Americans = coll$quantity[1:4],
           Chance = paste0("1 in ", n(coll$one_in[1:4])))

## ---- both-tab

data.frame(Name = bothw$name,
           As_a_first_name = n(bothw$as_first_name),
           As_a_last_name  = n(bothw$as_last_name))

## ---- unisex-tab

data.frame(Name = uni$name, People = n(uni$people),
           Female = paste0(pc(uni$pct_female), "%"),
           Male   = paste0(pc(100 - uni$pct_female), "%"))

## ---- race-tab

data.frame(
  Given_only_a = c("First name", "Last name"),
  Modal_guess_is_right = paste0(pc(rinfo$modal_correct_pct), "%"),
  Bits_before = pc(rinfo$bits_before, 2),
  Bits_after  = pc(rinfo$bits_after, 2),
  Uncertainty_removed = paste0(pc(rinfo$pct_of_uncertainty_removed), "%"))

## ---- flow-stock

# ---------------------------------------------------------------------------
# Figure 4. The same name and the same numbers, twice: given each year, then
# added up. TWO PANELS RATHER THAN TWO LINES ON ONE PAIR OF AXES, because the
# quantities are a count per year and a running total and share no scale. A
# second y-axis on the right is the way this figure is usually drawn and it is
# the way a reader is usually misled.
#
# One colour, the document's first-name teal, in both panels: there is one
# series here, and it is the same series both times.
# ---------------------------------------------------------------------------
a <- alx[alx$year >= 1990, ]
TEAL <- KIND[["first name"]]
op <- par(mfrow = c(1, 2), mar = c(3.6, 4.4, 2.2, 0.8), mgp = c(2.6, 0.7, 0))

plot(a$year, a$girls, type = "n", las = 1, xlab = "year of birth",
     ylab = "girls given the name", ylim = c(0, max(a$girls) * 1.12))
abline(v = FN("echo_year"), col = "#bbbbbb", lty = 2)
lines(a$year, a$girls, col = TEAL, lwd = 2.4)
points(FN("echo_year"), a$girls[a$year == FN("echo_year")], pch = 21,
       bg = "white", col = TEAL, lwd = 2, cex = 1.2)
text(FN("echo_year") - 1.5, max(a$girls) * 1.08, "Echo on sale", adj = 1,
     cex = 0.68, col = "#555555")
mtext("The flow: given each year", side = 3, line = 0.5, cex = 0.82,
      font = 2, col = "#333333", adj = 0)

# Six-digit tick labels laid flat need more room than the four-digit ones on
# the left, and at the shared margin the axis title lands on top of them.
par(mar = c(3.6, 6.2, 2.2, 0.8))
plot(a$year, a$cumulative_births, type = "n", las = 1, yaxt = "n", ylab = "",
     xlab = "year of birth", ylim = c(0, max(a$cumulative_births) * 1.12))
yt <- pretty(c(0, max(a$cumulative_births) * 1.12))
axis(2, at = yt, labels = format(yt, big.mark = ",", trim = TRUE), las = 1)
title(ylab = "people ever given it", line = 4.8)
lines(a$year, a$cumulative_births, col = TEAL, lwd = 2.4)
# The two census moments, which are the whole point of this panel.
for (y in c(2010, 2020)) {
  v <- a$cumulative_births[a$year == y]
  segments(y, 0, y, v, col = "#bbbbbb", lty = 2)
  points(y, v, pch = 21, bg = "white", col = TEAL, lwd = 2, cex = 1.2)
  text(y - 1.5, v + max(a$cumulative_births) * 0.06,
       paste0(y, ": ", n(v)), adj = 1, cex = 0.66, col = "#333333")
}
mtext("The stock: added up", side = 3, line = 0.5, cex = 0.82,
      font = 2, col = "#333333", adj = 0)
par(op)

## ---- disagree-tab

data.frame(Rank = disat$rank, Name = disat$name,
           In_the_sex_table  = n(disat$in_the_sex_table),
           In_the_race_table = n(disat$in_the_race_table),
           Gap = disat$gap)

## ---- ai-prompt

cat(ai_prompt(readLines("data/ai-prompt.txt")))
