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
bands <- rd("sex_bands.csv");     uni   <- rd("unisex.csv")
scat  <- rd("sex_scatter.csv");   rinfo <- rd("race_info.csv")
rext  <- rd("race_extremes.csv"); coll  <- rd("collision.csv")
bothw <- rd("both_ways.csv");     disag <- rd("disagreement.csv")
disat <- rd("disagreement_top.csv")
negs  <- rd("negatives.csv");     negt  <- rd("negatives_top.csv")
chk   <- rd("checks.csv")
xf    <- rd("explore_first.csv"); xl <- rd("explore_last.csv")
# The birth file, which is Social Security rather than the census. Only the
# flow-and-stock section uses these.
alx   <- rd("ssa_alexa.csv");     svc  <- rd("ssa_vs_census.csv")
shk   <- rd("ssa_shock.csv");     ris  <- rd("ssa_risers.csv")
scov  <- rd("ssa_coverage.csv");  xbt  <- rd("ssa_totals.csv")
xb    <- rd("ssa_explore.csv")

# The suggestion buttons under the birth-year figure. Picked by the data rather
# than typed, so a rebuild cannot leave a button pointing at a name the figure
# stopped carrying: the three fastest risers, the two most given names that
# finished long ago, and the name this section is about. Each is a different
# shape of curve. Computed HERE rather than in the figure, because the figure
# is browser-only and the prose beneath it names one of these in both outputs.
old_ <- xbt[xbt$median_birth_year < 1950, ]
nwc  <- rd("ssa_newcomers.csv")
# Four shapes, and the newcomer goes last so that the earlier positions keep
# their meaning: the prose below names buttons by position.
CHIPS <- unique(c("ALEXA", head(ris$name, 3),
                  head(old_$name[order(-old_$born_all)], 2),
                  nwc$name[1]))
stopifnot(all(CHIPS %in% xbt$name), length(CHIPS) == 7)
# The last button must be one of the names with no census row, or the sentence
# about it is wrong.
stopifnot(is.na(xbt$census_2020[xbt$name == CHIPS[7]]))

bir <- rd("ssa_births.csv")
# The year a name was at its most common, as a share of that year's births.
peak_year <- function(nm) {
  d <- xb[xb$name == nm, ]
  d$year[which.max((d$female + d$male) / bir$total[match(d$year, bir$year)])]
}
# The paragraph under the figure says the riser has already passed its own
# record and the two old names never came back. Asserted, not trusted: if a
# rebuild ever picks buttons that make those sentences false, it stops here
# rather than printing them.
stopifnot(peak_year(CHIPS[2]) >= 2015,
          peak_year(CHIPS[5]) < 1950, peak_year(CHIPS[6]) < 1950)

RI <- function(w, col) rinfo[[col]][rinfo$which == w]
CL <- function(q) coll$one_in[coll$quantity == q]

n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
mil <- function(x) pc(as.numeric(x) / 1e6, 1)

# ---- one palette for this document ----------------------------------------
# Three unrelated distinctions get drawn here and each is given its own
# channel, so that no colour carries two meanings anywhere in the document.
#   KIND  first name against last name. The document's spine; two hues that
#         are far apart in both hue and lightness so the PDF survives grey.
#   SEX   a pair, deliberately NOT pink and blue: the figure is about names
#         that refuse the binary, and colouring it with the convention it
#         questions would answer the question in the legend.
#   GCOL  the six census race categories, the same six colours the surnames
#         chapter uses, so a reader moving between the two is not relearning.
KIND <- c(`first name` = "#00666e", `last name` = "#8A3B2C")
SEX  <- c(female = "#5e3c99", male = "#e66101")
GCOL <- c("#4d9221", "#C41230", "#e08214", "#2c7fb8", "#999999", "#8856a7")
names(GCOL) <- c("white", "Black", "Am. Indian", "Asian/PI", "two or more",
                 "Hispanic")

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

## ---- raw
cat(paste(readLines("data/raw/first-names-as-it-arrives.txt"), collapse = "\n"))

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

## ---- eff-tab
data.frame(
  Names = c("First names", "Last names"),
  Published = c(n(F("first_names")), n(F("last_names"))),
  Effective_number = c(
    paste("between", n(F("effective_first_low")), "and", n(F("effective_first_high"))),
    paste("between", n(F("effective_last_low")),  "and", n(F("effective_last_high")))),
  Chance_two_people_share_one = paste0("1 in ", c(
    n(CL("two people share a first name")), n(CL("two people share a last name")))))

## ---- coll-tab
data.frame(Two_random_Americans = coll$quantity[1:4],
           Chance = paste0("1 in ", n(coll$one_in[1:4])))

## ---- both-tab
data.frame(Name = bothw$name,
           As_a_first_name = n(bothw$as_first_name),
           As_a_last_name  = n(bothw$as_last_name))

## ---- sex-band-tab
data.frame(
  Names_where_the_minority_sex_is_at_least = paste0(bands$minority_sex_at_least_pct, "%"),
  Names = n(bands$names),
  People = n(bands$people),
  Share_of_the_country = paste0(pc(bands$pct_of_people), "%"))

## ---- sex-d3
# ---------------------------------------------------------------------------
# Figure 2. Every first name with 25,000 bearers or more, placed by the share
# of its bearers who are female. Log y, because the names run from 25,000 to
# 3.5 million and a linear axis would put all but twenty of them on the floor.
# The declared cut is in the caption, not just here.
# ---------------------------------------------------------------------------
s <- scat[order(-scat$people), ]
pts <- paste0("[", paste(apply(s, 1, function(r)
  paste0('["', r[["name"]], '",', r[["people"]], ',',
         j_num(r[["pct_female"]], 2), ']')), collapse = ","), "]")
lab <- uni$name[1:8]
cat(paste0('
<div id="sexfig" style="position:relative;margin:0.6em 0 0.4em 0"></div>
<script>
(function(){
const D=', pts, ', LAB=', paste0('["', paste(lab, collapse = '","'), '"]'), ';
const CFem="', SEX[["female"]], '", CMal="', SEX[["male"]], '";
const W=760,H=430,M={t:20,r:22,b:46,l:64};
const svg=d3.select("#sexfig").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([20000,4000000]).range([H-M.b,M.t]);
// the band where a majority rule is at its least defensible
svg.append("rect").attr("x",x(10)).attr("y",M.t).attr("width",x(90)-x(10))
  .attr("height",H-M.t-M.b).attr("fill","#bbb").attr("fill-opacity",0.16);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(5,"~s"));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("share of the people with this name who are female");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("people with the name");
svg.append("text").attr("x",x(50)).attr("y",M.t+12).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("fill","#666")
  .text("minority sex above 10% — " + ', pc(bands$pct_of_people[bands$minority_sex_at_least_pct == 10]), ' + "% of the country");
const tip=d3.select("#sexfig").append("div").attr("style",
  "position:absolute;pointer-events:none;background:#fff;border:1px solid #bbb;"+
  "padding:4px 7px;font-size:0.82em;border-radius:3px;display:none;box-shadow:0 1px 4px #0002");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d[2])).attr("cy",d=>y(d[1]))
  .attr("r",d=>Math.max(2,Math.sqrt(d[1])/620))
  .attr("fill",d=>d[2]>=50?CFem:CMal).attr("fill-opacity",0.42)
  .attr("stroke",d=>d[2]>=50?CFem:CMal).attr("stroke-opacity",0.6)
  .on("mouseenter",function(e,d){
    tip.style("display","block").html("<b>"+d[0]+"</b><br>"+
      d[1].toLocaleString()+" people<br>"+d[2].toFixed(1)+"% female");})
  .on("mousemove",function(e){
    tip.style("left",Math.min(d3.pointer(e,this.ownerSVGElement.parentNode)[0]+14,W-200)+"px")
       .style("top",(d3.pointer(e,this.ownerSVGElement.parentNode)[1]-6)+"px");})
  .on("mouseleave",function(){tip.style("display","none");});
// a few named anchors, so the figure is readable without hovering
const by={}; D.forEach(d=>by[d[0]]=d);
// Staggered above and below alternately: eight anchors inside a forty-point
// band would otherwise sit on top of one another.
LAB.forEach(function(nm,i){
  const d=by[nm]; if(!d) return;
  svg.append("text").attr("x",x(d[2])).attr("y",y(d[1])+(i%2?16:-10))
    .attr("text-anchor","middle").attr("font-size","10.5px").attr("font-weight","600")
    .attr("fill","#333").text(nm);
});
})();
</script>'))

## ---- sex-static
s <- scat[order(-scat$people), ]
par(mar = c(4.0, 4.6, 0.6, 1.0), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(0, 100), ylim = c(20000, 4e6), log = "y", las = 1, yaxt = "n",
     xlab = "share of the people with this name who are female",
     ylab = "people with the name")
axis(2, at = c(2e4, 1e5, 5e5, 2e6), labels = c("20k", "100k", "500k", "2m"), las = 1)
rect(10, 20000, 90, 4e6, col = "#bbbbbb28", border = NA)
cols <- ifelse(s$pct_female >= 50, SEX[["female"]], SEX[["male"]])
points(s$pct_female, s$people, pch = 21, cex = pmax(0.35, sqrt(s$people) / 1500),
       col = paste0(cols, "99"), bg = paste0(cols, "55"))
lab <- uni$name[1:8]
# Eight anchors in a band that is only forty points wide, so they are staggered
# above and below alternately; centred on the point they would sit on top of
# each other.
for (i in seq_along(lab)) {
  d <- s[s$name == lab[i], ]
  if (!nrow(d)) next
  up <- i %% 2 == 1
  text(d$pct_female, d$people * if (up) 1.55 else 0.62, lab[i],
       cex = 0.6, font = 2)
}
# ASCII only in plot annotations: the PDF device drops glyphs outside Latin-1,
# so an em dash here would render as a substitution warning and a hyphen.
text(50, 3.4e6, sprintf("minority sex above 10%%: %s%% of the country",
                        pc(bands$pct_of_people[bands$minority_sex_at_least_pct == 10])),
     cex = 0.62, col = "#666666")

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

## ---- extremes-tab
ef <- rext[rext$which == "first name", ]
el <- rext[rext$which == "last name", ]
stopifnot(identical(ef$group, el$group))
data.frame(
  Group = el$group,
  Most_distinctive_first_name = paste0(ef$name, " (", pc(ef$pct_in_group), "%)"),
  Most_distinctive_last_name  = paste0(el$name, " (", pc(el$pct_in_group), "%)"),
  Group_share_of_the_file = paste0(pc(el$group_share_of_file), "%"))

## ---- explore-d3
# ---------------------------------------------------------------------------
# Figure 3. The lookup the Census Bureau does not publish. A DECLARED CUT: the
# 2,000 most common names of each kind, because the whole tables are 210,000
# rows and will not travel inside one HTML page. The coverage the cut buys is
# printed in the caption AND in the box itself, so a reader who searches for a
# name that is not there is told why rather than told "no".
# ---------------------------------------------------------------------------
RGRP <- c("white", "black", "aian", "asian", "twoplus", "hispanic")
PRET <- c("white", "Black", "Am. Indian", "Asian/PI", "two or more", "Hispanic")
frow <- function(r) paste0('["', r[["name"]], '",', r[["rank"]], ',', r[["people"]],
                           ',', j_num(r[["pct_female"]], 2), ',[',
                           paste(j_num(r[paste0("pct_", RGRP)], 2), collapse = ","),
                           '],', tolower(r[["also_a_last_name"]]), ']')
lrow <- function(r) paste0('["', r[["name"]], '",', r[["rank"]], ',', r[["people"]],
                           ',null,[',
                           paste(j_num(r[paste0("pct_", RGRP)], 2), collapse = ","),
                           '],', tolower(r[["also_a_first_name"]]), ']')
cat(paste0('
<form id="nmctl" style="margin:0.9em 0 0.5em 0;font-size:0.92em" onsubmit="return false">
  <label for="nmq" style="font-weight:600">Name:</label>
  <input id="nmq" type="text" value="TAYLOR" autocomplete="off" spellcheck="false"
         style="width:11em;padding:3px 6px;margin-left:0.4em;font-size:1em;
                text-transform:uppercase">
  <span style="color:#666;margin-left:0.6em">the ',
  n(F("explore_n")), ' most common of each kind</span>
</form>
<div id="nmout" style="position:relative;margin:0 0 1em 0"></div>
<script>
(function(){
const FIRST=[', paste(apply(xf, 1, frow), collapse = ","), '];
const LAST=[', paste(apply(xl, 1, lrow), collapse = ","), '];
const GRP=', paste0('["', paste(PRET, collapse = '","'), '"]'), ';
const GC=', paste0('["', paste(unname(GCOL[PRET]), collapse = '","'), '"]'), ';
const CFem="', SEX[["female"]], '", CMal="', SEX[["male"]], '";
const KF="', KIND[["first name"]], '", KL="', KIND[["last name"]], '";
// Bound as identifiers rather than pasted inline. A bare numeric literal with
// a method call after it -- 2000.toLocaleString() -- is a JavaScript syntax
// error, because the dot is read as a decimal point.
const NSHOWN=', F("explore_n"), ', POPF=', F("first_people"),
  ', POPL=', F("last_people"), ';
const fi={},li={};
FIRST.forEach(d=>fi[d[0]]=d); LAST.forEach(d=>li[d[0]]=d);
const out=d3.select("#nmout");
const box=out.append("div").attr("style","font-size:0.93em;line-height:1.5");
function bars(sel,pcts,title){
  const t=sel.append("div").attr("style","margin:0.35em 0 0.15em 0;color:#555;font-size:0.9em");
  t.text(title);
  const w=sel.append("div").attr("style","display:flex;height:16px;width:100%;"+
    "border:1px solid #ddd;overflow:hidden;border-radius:2px");
  pcts.forEach((p,i)=>{ if(p<=0) return;
    w.append("div").attr("style","background:"+GC[i]+";width:"+p+"%").attr("title",
      GRP[i]+" "+p.toFixed(1)+"%");});
  const lg=sel.append("div").attr("style","font-size:0.8em;color:#444;margin-top:0.2em");
  pcts.map((p,i)=>[p,i]).sort((a,b)=>b[0]-a[0]).filter(d=>d[0]>=1).forEach(d=>{
    lg.append("span").attr("style","margin-right:0.9em;white-space:nowrap")
      .html("<span style=\'display:inline-block;width:8px;height:8px;background:"+
        GC[d[1]]+";margin-right:3px\'></span>"+GRP[d[1]]+" "+d[0].toFixed(1)+"%");});
}
function card(sel,d,kind,other){
  const c=sel.append("div").attr("style",
    "border-left:3px solid "+(kind=="first"?KF:KL)+";padding:0.15em 0 0.35em 0.7em;margin:0.6em 0");
  c.append("div").attr("style","font-weight:600;font-size:1.05em")
    .text(d[0]+" — as a "+kind+" name");
  c.append("div").attr("style","color:#333")
    .html("rank <b>"+d[1].toLocaleString()+"</b> · <b>"+d[2].toLocaleString()+
      "</b> people · about <b>1 in "+
      Math.round((kind=="first"?POPF:POPL)/d[2]).toLocaleString()+
      "</b> Americans");
  if(d[3]!==null){
    const f=d[3], m=100-f;
    const s=c.append("div").attr("style","margin-top:0.4em");
    s.append("div").attr("style","color:#555;font-size:0.9em").text("sex");
    const w=s.append("div").attr("style","display:flex;height:16px;width:100%;"+
      "border:1px solid #ddd;overflow:hidden;border-radius:2px");
    w.append("div").attr("style","background:"+CFem+";width:"+f+"%");
    w.append("div").attr("style","background:"+CMal+";width:"+m+"%");
    s.append("div").attr("style","font-size:0.8em;color:#444;margin-top:0.2em")
      .html("<span style=\'color:"+CFem+";font-weight:600\'>"+f.toFixed(1)+"% female</span>"+
        " · <span style=\'color:"+CMal+";font-weight:600\'>"+m.toFixed(1)+"% male</span>");
  }
  bars(c,d[4],"race and Hispanic origin");
  // `other` is computed against the WHOLE opposite table, not the 2,000 rows
  // carried into the page, so it can be true for a name whose other card is
  // not shown. Saying which case you are in beats leaving a reader to wonder
  // where the second card went.
  if(other){
    const shown=(kind=="first"?li:fi)[d[0]]!==undefined;
    c.append("div").attr("style","margin-top:0.4em;color:#555;font-size:0.9em")
      .text("This is also somebody’s "+(kind=="first"?"last":"first")+" name"+
        (shown?"." : ", though not a common enough one to be shown here."));
  }
}
function draw(q){
  q=(q||"").toUpperCase().replace(/[^A-Z]/g,"");
  box.html("");
  const a=fi[q], b=li[q];
  if(!a&&!b){
    const near=FIRST.concat(LAST).filter(d=>q.length>1&&d[0].indexOf(q)===0)
      .map(d=>d[0]).filter((v,i,s)=>s.indexOf(v)===i).slice(0,8);
    box.append("p").attr("style","color:#666")
      .html(q.length<2 ? "Type a name."
        : "<b>"+q+"</b> is not among the "+NSHOWN.toLocaleString()+
          " most common names of either kind"+
          (near.length? ". Names here that start that way: <b>"+near.join(", ")+"</b>." : "."));
    return;
  }
  if(a) card(box,a,"first",a[5]);
  if(b) card(box,b,"last",b[5]);
  if(a&&b) box.append("p").attr("style","color:#555;font-size:0.9em;margin-top:0.5em")
    .html("<b>"+q+"</b> is both. A file that transposed its two name columns "+
      "would leave this row looking entirely normal.");
}
const inp=document.getElementById("nmq");
inp.addEventListener("input",function(){draw(inp.value);});
draw(inp.value);
})();
</script>'))

## ---- explore-static
x <- xf[xf$name %in% c("TAYLOR", "JORDAN", "MARIA", "MICHAEL"), ]
data.frame(
  First_name = x$name, Rank = n(x$rank), People = n(x$people),
  Female = paste0(pc(x$pct_female), "%"),
  Largest_group = paste0(
    c("white", "Black", "Am. Indian", "Asian/PI", "two or more", "Hispanic")[
      apply(x[, paste0("pct_", c("white", "black", "aian", "asian", "twoplus",
                                 "hispanic"))], 1, which.max)],
    " ", pc(apply(x[, paste0("pct_", c("white", "black", "aian", "asian",
                                       "twoplus", "hispanic"))], 1, max)), "%"),
  Also_a_surname = ifelse(x$also_a_last_name, "yes", "no"))

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

## ---- shock-tab
# The same window for all four, five years wide, rather than a window chosen
# per name to make each one look as sharp as possible. ALEXA is the shallowest
# of them on this measure and it stays in the table at its real size.
WIN <- 5
sh <- do.call(rbind, lapply(split(shk, shk$name), function(d) {
  before <- d$n[d$year == d$event[1] - 1]
  after  <- d$n[d$year == d$event[1] + WIN]
  data.frame(name = d$name[1], event = d$event[1], before = before, after = after,
             change = 100 * (after / before - 1))
}))
sh <- sh[order(sh$event), ]
WHAT <- c(ALEXA = "Amazon put the Echo on general sale",
          SIRI = "Apple shipped Siri with the iPhone 4S",
          ISIS = "the group took Mosul",
          KATRINA = "the hurricane made landfall")
stopifnot(all(sh$name %in% names(WHAT)), !any(is.na(sh$before)), !any(is.na(sh$after)))
# ISIS did not stay down, and the paragraph after the table says so with these.
is_ <- shk[shk$name == "ISIS", ]
IS_LOW   <- min(is_$n[is_$year >= 2015]); IS_LOW_Y <- is_$year[is_$n == IS_LOW][1]
IS_BACK  <- max(is_$n[is_$year > IS_LOW_Y]); IS_BACK_Y <- is_$year[is_$n == IS_BACK][1]
stopifnot(IS_BACK > IS_LOW)
data.frame(
  Name = sh$name,
  What_happened = unname(WHAT[sh$name]),
  Year = sh$event,
  Given_the_year_before = n(sh$before),
  Five_years_later = n(sh$after),
  Change = paste0(pc(sh$change, 0), "%"))

## ---- risers-tab
# THIAGO never reached the file before 1980, so its peak year is empty rather
# than a year. Printed as a dash: a blank cell reads as a number that went
# missing, and a zero would read as a year.
data.frame(
  Name = ris$name,
  Given_a_year_then = n(ris$given_then),
  Given_a_year_now = n(ris$given_now),
  Busiest_earlier_year = ifelse(is.na(ris$old_peak_year), "—", ris$old_peak_year),
  Given_that_year = ifelse(is.na(ris$old_peak_year), "—", n(ris$old_peak_n)))

## ---- newcomers-tab
data.frame(
  Name = nwc$name,
  First_year_in_the_file = nwc$first_year,
  People_born_by_2010 = n(nwc$born_by_2010),
  Babies_since_2023 = n(nwc$births_recent))

## ---- vs-tab
data.frame(
  Name = svc$name,
  Median_birth_year = svc$median_birth_year,
  Ever_given_the_name = n(svc$born_through_2020),
  Counted_alive_in_2020 = n(svc$census_2020),
  Share_still_counted = paste0(pc(svc$pct_still_counted, 0), "%"))

## ---- birth-d3
# ---------------------------------------------------------------------------
# Figure 5. The birth curve for any of the names carried into the page, with
# the census stock printed beside it.
#
# A SECOND DECLARED CUT, and a different one from Figure 3: the 1,000 most
# common first names in the census, from 1920 on. Census rank rather than birth
# rank, so that every name in the box has both a stock and a flow -- which is
# the comparison this section is about. The coverage it buys is in the caption
# and in the box.
#
# THE LINE IS A RATE, NOT A COUNT. American births went from 2.3 million a year
# to 4.3 million and back to 3.3 million over this window, so a count would show
# a baby boom in every single name. Per 100,000 births takes that out. The count
# is still there, in the readout, for whoever wants it.
# ---------------------------------------------------------------------------
xb <- xb[order(xb$name, xb$year), ]
brow <- function(d) {
  yy <- seq(min(d$year), max(d$year))
  f <- m <- integer(length(yy))
  i <- match(d$year, yy)
  f[i] <- d$female; m[i] <- d$male
  t <- xbt[xbt$name == d$name[1], ]
  # A newcomer has no census row, and NA has to reach the page as JavaScript
  # null rather than as the four letters. Written once here so that the three
  # census fields cannot disagree about which of them went missing.
  cen <- function(v, k = 0) if (is.na(v)) "null" else j_num(v, k)
  paste0('["', d$name[1], '",', yy[1], ',[', paste(f, collapse = ","), '],[',
         paste(m, collapse = ","), '],', cen(t$census_2020), ',',
         cen(t$census_rank), ',', t$born_all, ',', t$median_birth_year, ',',
         cen(t$pct_still_counted, 1), ']')
}
rows <- vapply(split(xb, xb$name), brow, character(1))
chip_js <- paste0('"', paste(CHIPS, collapse = '","'), '"')
bir <- bir[bir$year >= FN("birth_y0"), ]
stopifnot(nrow(bir) == FN("ssa_max") - FN("birth_y0") + 1)
cat(paste0('
<form id="bctl" style="margin:0.9em 0 0.5em 0;font-size:0.92em" onsubmit="return false">
  <label for="bq" style="font-weight:600">Name:</label>
  <input id="bq" type="text" value="ALEXA" autocomplete="off" spellcheck="false"
         style="width:11em;padding:3px 6px;margin-left:0.4em;font-size:1em;
                text-transform:uppercase">
  <span style="color:#666;margin-left:0.6em">', n(F("birth_names")),
  ' names</span>
  <div id="bchips" style="margin:0.5em 0 0 0;color:#666">or try:</div>
</form>
<div id="bout" style="position:relative;margin:0 0 1em 0"></div>
<script>
(function(){
const D=[', paste(rows, collapse = ","), '];
const BIRTHS=[', paste(bir$total, collapse = ","), '];
const Y0=', F("birth_y0"), ', CARD=', F("ssa_card_year"), ';
const NSHOWN=', F("birth_n"), ', LASTY=', F("ssa_max"), ';
const CFem="', SEX[["female"]], '", CMal="', SEX[["male"]], '";
const KF="', KIND[["first name"]], '";
const SSAMIN=', F("ssa_min"), ';
// A sex is drawn as its own line only if it holds at least this share of the
// name. A flat line along the floor is not a series, and a legend naming it as
// one is worse than leaving it off.
const SHOW_SHARE=0.01;
const ix={}; D.forEach(d=>ix[d[0]]=d);
const wrap=d3.select("#bout");
const W=760,H=330,M={t:16,r:20,b:40,l:58};
const svg=wrap.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const out=wrap.append("p").attr("style",
  "font-size:0.9em;color:#222;margin:0.3em 0 0 0;min-height:2.4em");
const card=wrap.append("div").attr("style","font-size:0.93em;line-height:1.5");
const x=d3.scaleLinear().domain([Y0,LASTY]).range([M.l,W-M.r]);
const y=d3.scaleLinear().range([H-M.b,M.t]);
const gx=svg.append("g").attr("transform","translate(0,"+(H-M.b)+")");
const gy=svg.append("g").attr("transform","translate("+M.l+",0)");
// The years before Social Security numbers existed, marked in the figure
// rather than only in the prose. A person born in 1920 is in this file only if
// they lived long enough to apply for a card in 1936 or later.
svg.append("rect").attr("x",x(Y0)).attr("y",M.t).attr("width",x(CARD)-x(Y0))
  .attr("height",H-M.t-M.b).attr("fill","#000").attr("opacity",0.045);
// ANCHORED TO THE LEFT EDGE OF THE BAND, NOT ITS RIGHT EDGE. Ending the label
// at the right edge put its first eight pixels outside the plot, on top of the
// y-axis and the tick labels. Starting it at the left edge cannot leave the
// plot, however wide or narrow the shaded years turn out to be.
// (No apostrophes in here: this whole block is one single-quoted R string.)
svg.append("text").attr("x",x(Y0)+5).attr("y",M.t+12).attr("text-anchor","start")
  .attr("font-size","10px").attr("fill","#777").text("before cards were issued");
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("year of birth");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",13).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("given per 100,000 births");
const plot=svg.append("g");
const hov=svg.append("g").attr("display","none");
const hl=hov.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#111").attr("stroke-dasharray","3,3");
const hd=[hov.append("circle").attr("r",4.5).attr("fill",CFem),
          hov.append("circle").attr("r",4.5).attr("fill",CMal)];
const line=d3.line().x((d,i)=>x(cur.y0+i)).y(d=>y(d));
let cur=null;
function rates(a,y0){return a.map((v,i)=>1e5*v/BIRTHS[y0-Y0+i]);}
function draw(q){
  q=(q||"").toUpperCase().replace(/[^A-Z]/g,"");
  const d=ix[q];
  card.html(""); out.html("");
  if(!d){
    svg.attr("display","none");
    const near=D.filter(r=>q.length>1&&r[0].indexOf(q)===0).map(r=>r[0]).slice(0,8);
    card.append("p").attr("style","color:#666")
      .html(q.length<2 ? "Type a name."
        : "<b>"+q+"</b> is not among the "+NSHOWN.toLocaleString()+
          " most common first names in the 2020 census"+
          (near.length? ". Names here that start that way: <b>"+near.join(", ")+
           "</b>." : ", so its birth curve is not carried in this page."));
    return;
  }
  svg.attr("display",null);
  const f=rates(d[2],d[1]), m=rates(d[3],d[1]);
  cur={y0:d[1],f:f,m:m,fc:d[2],mc:d[3]};
  y.domain([0,d3.max(f.concat(m))*1.08||1]);
  gx.call(d3.axisBottom(x).ticks(8,"d"));
  gy.call(d3.axisLeft(y));
  plot.selectAll("*").remove();
  const both=d3.sum(d[2])+d3.sum(d[3]);
  const big=s=>d3.sum(s)>=SHOW_SHARE*both;
  const shown=[];
  [[f,CFem,"girls",d[2]],[m,CMal,"boys",d[3]]].forEach(s=>{
    if(!big(s[3])) return;
    shown.push(s);
    plot.append("path").datum(s[0]).attr("fill","none").attr("stroke",s[1])
      .attr("stroke-width",2.2).attr("d",line);
  });
  if(shown.length>1){
    const lg=plot.append("g").attr("transform","translate("+(M.l+14)+","+(M.t+8)+")");
    shown.forEach((s,i)=>{
      lg.append("line").attr("x1",0).attr("x2",18).attr("y1",i*16).attr("y2",i*16)
        .attr("stroke",s[1]).attr("stroke-width",2.2);
      lg.append("text").attr("x",24).attr("y",i*16+4).attr("font-size","11.5px")
        .attr("fill","#333").text(s[2]);
    });
  }
  hd[0].attr("display",big(d[2])?null:"none");
  hd[1].attr("display",big(d[3])?null:"none");
  const stock=d[4], rank=d[5], born=d[6], med=d[7], still=d[8];
  // Two versions of the stock line. The second is the whole reason the names
  // with no census row are carried here: a reader who clicks one is told what
  // is missing and why, instead of being shown an empty space.
  const stockLine = stock===null
    ? "The stock: <b>no row in the census file</b>. The name was too rare in "+
      "2010 to be published from 2020, so everyone who has it was counted into "+
      "the ALL OTHER NAMES line instead."
    : "The stock: the 2020 census counts <b>"+stock.toLocaleString()+
      "</b> alive, rank <b>"+rank.toLocaleString()+"</b>, which is <b>"+
      still.toFixed(1)+"%</b> of everyone ever given it.";
  // Same teal rule on every card. A fourth colour for "no census row" would be
  // a fourth meaning for colour in a document that carries three, and the
  // sentence already says it in words.
  card.html("<div style=\'border-left:3px solid "+KF+
    ";padding:0.15em 0 0.35em 0.7em;"+
    "margin:0.6em 0\'><div style=\'font-weight:600;font-size:1.05em\'>"+q+
    "</div><div style=\'color:#333\'>The flow: <b>"+born.toLocaleString()+
    "</b> people have been given it since "+SSAMIN+", half of them before <b>"+
    med+"</b>.</div><div style=\'color:#333\'>"+stockLine+"</div></div>");
  out.html("Move the pointer across the figure to read a year off it.");
}
function show(yr){
  if(!cur) return;
  yr=Math.max(cur.y0,Math.min(cur.y0+cur.f.length-1,yr));
  const i=yr-cur.y0;
  hov.attr("display",null); hl.attr("x1",x(yr)).attr("x2",x(yr));
  hd[0].attr("cx",x(yr)).attr("cy",y(cur.f[i]));
  hd[1].attr("cx",x(yr)).attr("cy",y(cur.m[i]));
  const parts=[];
  if(cur.fc[i]) parts.push("<b style=\'color:"+CFem+"\'>"+
    cur.fc[i].toLocaleString()+"</b> girls");
  if(cur.mc[i]) parts.push("<b style=\'color:"+CMal+"\'>"+
    cur.mc[i].toLocaleString()+"</b> boys");
  out.html("<b>"+yr+"</b>: "+(parts.length?parts.join(" and ")+
    " were given the name":"nobody was given the name")+
    (yr<CARD?" <span style=\'color:#777\'>(before cards were issued, so this year is short)</span>":""));
}
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.l-M.r)
  .attr("height",H-M.t-M.b).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){show(Math.round(x.invert(d3.pointer(e,this)[0])));})
  .on("mouseleave",function(){hov.attr("display","none");
    out.html("Move the pointer across the figure to read a year off it.");});
const inp=document.getElementById("bq");
inp.addEventListener("input",function(){draw(inp.value);});
// The suggestion buttons. Real <button>s rather than styled spans, so that a
// keyboard reaches them and a screen reader calls them buttons.
const CHIPS=[', chip_js, '];
const chipbar=d3.select("#bchips");
const btns=CHIPS.map(nm=>chipbar.append("button").attr("type","button").text(nm)
  .attr("style","margin-left:0.45em;padding:1px 7px;font:inherit;font-size:0.92em;"+
    "cursor:pointer;border:1px solid #ccc;border-radius:10px;background:#f6f6f6;color:#333")
  .on("click",function(){inp.value=nm; draw(nm); mark(nm);}));
function mark(cur){
  btns.forEach((b,i)=>b.attr("style",
    "margin-left:0.45em;padding:1px 7px;font:inherit;font-size:0.92em;cursor:pointer;"+
    "border:1px solid "+(CHIPS[i]===cur?KF:"#ccc")+";border-radius:10px;background:"+
    (CHIPS[i]===cur?"#fff":"#f6f6f6")+";color:"+(CHIPS[i]===cur?KF:"#333")+
    (CHIPS[i]===cur?";font-weight:600":"")));
}
inp.addEventListener("input",function(){mark(inp.value.toUpperCase());});
draw(inp.value); mark(inp.value.toUpperCase());
})();
</script>'))

## ---- birth-static
# Print cannot carry the box, so it gets one name per era instead: the most
# given name whose middle year falls in each of four decades. CHOSEN BY THE
# DATA, not typed out. A hand-picked list here quietly dropped two names that
# are not among the 1,000 the figure carries, and printed four rows as two.
ERAS <- c(1930, 1960, 1990, 2015)
b <- do.call(rbind, lapply(ERAS, function(e) {
  s <- xbt[xbt$median_birth_year >= e & xbt$median_birth_year < e + 10, ]
  s[which.max(s$born_all), ]
}))
stopifnot(nrow(b) == length(ERAS), !any(duplicated(b$name)))
data.frame(
  Name = b$name,
  Median_birth_year = b$median_birth_year,
  Ever_given_the_name = n(b$born_all),
  Counted_alive_in_2020 = n(b$census_2020),
  Census_rank = n(b$census_rank))

## ---- disagree-tab
data.frame(Rank = disat$rank, Name = disat$name,
           In_the_sex_table  = n(disat$in_the_sex_table),
           In_the_race_table = n(disat$in_the_race_table),
           Gap = disat$gap)

## ---- disagree-dist
data.frame(Gap_between_the_two_tables = paste(disag$gap, "people"),
           First_names = n(disag$names))

## ---- neg-tab
data.frame(Table = negs$which,
           Rows_with_a_negative_cell = n(negs$rows_with_a_negative),
           Share = paste0(pc(negs$pct_of_rows), "%"),
           Worst_cell = negs$worst_cell,
           Rows_whose_total_the_repair_changed =
             n(negs$rows - negs$rows_whose_total_is_unchanged))

## ---- neg-top-tab
data.frame(First_name = negt$name, People = n(negt$people),
           Published = paste0(n(negt$published_male), " / ", n(negt$published_female)),
           Research_copy = paste0(n(negt$research_male), " / ", n(negt$research_female)))

## ---- checks
chk

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#sexfig text[fill="#333" i],
#sexfig text[fill="#666" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: the on-mark chunk above pins this fill dark for the dark
# page, so a --paper stroke there would sit dark behind a dark ink, and the
# checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#sexfig text[fill="#666" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
