# officeholder-age-code.R -- chunk bodies for officeholder-age-brief.Rmd
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

D  <- "data"
m  <- read.csv(file.path(D, "derived/members.csv"),         stringsAsFactors = FALSE)
ac <- read.csv(file.path(D, "derived/age_by_congress.csv"), stringsAsFactors = FALSE)
le <- read.csv(file.path(D, "derived/life_expectancy.csv"), stringsAsFactors = FALSE)
et <- read.csv(file.path(D, "derived/entry_tenure.csv"),    stringsAsFactors = FALSE)
pr <- read.csv(file.path(D, "derived/presidents.csv"),      stringsAsFactors = FALSE)
ck <- read.csv(file.path(D, "derived/checks.csv"),          stringsAsFactors = FALSE)

# ---- formatting -------------------------------------------------------------
# R rounds half to even; JavaScript rounds half up. Every number that appears
# in both the D3 figure and the static one is formatted ONCE here, in R, and
# the string is what travels. Do not re-round anything on the JavaScript side.
f0 <- function(x) formatC(x, format = "f", digits = 0)
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
sgn <- function(x, k = 1) paste0(ifelse(x >= 0, "+", "-"),
                                 formatC(abs(x), format = "f", digits = k))
n  <- function(x) format(round(x), big.mark = ",")

# ---- the Congress sitting in a calendar year --------------------------------
# Congress n begins in 1787 + 2n, so it begins in an odd year and sits through
# the even year after it. The Congress sitting in year Y is therefore
# floor((Y - 1787)/2). Life-expectancy years are mostly even, congressional
# years always odd, so this is the join.
sitting <- function(y) floor((y - 1787) / 2)
val <- function(ch, y, col = "median_age")
  ac[[col]][ac$chamber == ch & ac$congress == sitting(y)]

BY <- 1950; EY <- 2018                 # the window where BOTH life-expectancy
                                       # series exist
CBY <- sitting(BY); CEY <- sitting(EY)

A0 <- val("Congress", BY); A1 <- val("Congress", EY); dA <- A1 - A0
E0a <- le$e0_hus[le$year == BY]; E0b <- le$e0_hus[le$year == EY]; dE0 <- E0b - E0a
E6a <- le$e65[le$year == BY];    E6b <- le$e65[le$year == EY];    dE6 <- E6b - E6a
REL <- 100 * dE6 / dE0                 # how much of the headline gain is even
                                       # about people who reached 65

# ---- the decomposition ------------------------------------------------------
dec <- function(ch, y1, y2) {
  a <- ac[ac$chamber == ch & ac$congress == sitting(y1), ]
  b <- ac[ac$chamber == ch & ac$congress == sitting(y2), ]
  c(total = b$mean_age - a$mean_age,
    entry = b$mean_entry_age - a$mean_entry_age,
    stay  = b$mean_years_since_entry - a$mean_years_since_entry)
}
DC <- dec("Congress", BY, EY); DH <- dec("House", BY, EY); DS <- dec("Senate", BY, EY)
NOW <- 2025                            # first year of the newest Congress here
CNOW <- sitting(NOW)

# ---- how long ago was Congress last this young? -----------------------------
CC0    <- ac[ac$chamber == "Congress", ]
TR     <- CC0[CC0$year >= 1965 & CC0$year <= 1995, ]
TR     <- TR[which.min(TR$median_age), ]
PRIOR  <- max(CC0$year[CC0$year < TR$year & CC0$median_age <= TR$median_age])
OLDEST <- CC0$year[CC0$median_age == max(CC0$median_age)]

# ---- render every data.frame in this document as a TABLE --------------------
# A data.frame printed the ordinary way arrives as a "##"-prefixed code block,
# which reads as machinery rather than as a result. Registering knit_print for
# data.frame turns all of them into real tables in HTML and PDF alike. The
# envir argument is required: without it the registration silently fails.
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- cleanrow
m[m$congress == 1 &
  m$bioname %in% c("WASHINGTON, George", "HUNTINGTON, Benjamin", "SHERMAN, Roger"),
  c("congress", "year", "chamber", "icpsr", "bioname", "born", "age",
    "entry_year", "years_since_entry")]

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The median and quartiles of each chamber at every Congress, plus one dot per
# president. Both renderers read age_by_congress.csv and presidents.csv; the
# numbers are formatted in R and travel as strings.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
H <- ac[ac$chamber == "House",   ]; S <- ac[ac$chamber == "Senate", ]
P <- pr[!is.na(pr$age_at_term_start), ]
# Two decimals, not one: chamber quartiles land on .25 and .75, and rounding
# them to a single decimal would make the interactive ribbon disagree with the
# printed one in the third significant figure.
ser <- function(d) paste(sprintf('[%d,%s,%s,%s]', d$year, f2(d$median_age),
                                 f2(d$p25_age), f2(d$p75_age)), collapse = ",")
cat(sprintf('
<div id="f1" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const H=[%s],S=[%s],P=[%s];
const W=760,Hh=430,M={t:26,r:120,b:44,l:46};
const svg=d3.select("#f1").append("svg").attr("viewBox",`0 0 ${W} ${Hh}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1787,%d]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([30,82]).range([Hh-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${Hh-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(Hh)/2).attr("y",13)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#555")
  .text("age, years");
const area=d3.area().x(d=>x(d[0])).y0(d=>y(d[2])).y1(d=>y(d[3]));
const line=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
[[H,"#2c7fb8","House"],[S,"#b3651a","Senate"]].forEach(([d,c,lab])=>{
  svg.append("path").datum(d).attr("d",area).attr("fill",c).attr("fill-opacity",0.13);
  svg.append("path").datum(d).attr("d",line).attr("fill","none")
    .attr("stroke",c).attr("stroke-width",1.9);
  const last=d[d.length-1];
  svg.append("text").attr("x",x(last[0])+6).attr("y",y(last[1])+4)
    .attr("font-size","12px").attr("font-weight","600").attr("fill",c).text(lab);
});
svg.selectAll("circle.p").data(P).join("circle").attr("class","p")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",3.1)
  .attr("fill","#4d9221").attr("fill-opacity",0.85);
svg.append("text").attr("x",W-M.r+6).attr("y",y(%s)-6).attr("font-size","12px")
  .attr("font-weight","600").attr("fill","#4d9221").text("Presidents");
const tip=d3.select("#f1").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("circle.p").on("mousemove",function(e,d){
  tip.style("opacity",1).html(`<b>${d[2]}</b><br>${d[0]}, aged about ${d[1]}`)
   .style("left",Math.min(e.offsetX+14,W-190)+"px").style("top",(e.offsetY-10)+"px");
 }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("text").attr("x",M.l).attr("y",14).attr("font-size","12px")
  .attr("fill","#555").text("shaded band: middle half of the chamber");
})();
</script>
', ser(H), ser(S),
   paste(sprintf('[%d,%s,"%s"]', P$year, f0(P$age_at_term_start),
                 gsub('"', "", sub(",.*$", "", P$bioname))), collapse = ","),
   NOW + 2, f0(P$age_at_term_start[nrow(P)])))

## ---- fig1-static
H <- ac[ac$chamber == "House", ]; S <- ac[ac$chamber == "Senate", ]
P <- pr[!is.na(pr$age_at_term_start), ]
par(mar = c(3.4, 3.6, 1.4, 5.6))
plot(NA, xlim = c(1787, NOW + 2), ylim = c(30, 82), xlab = "", ylab = "",
     xaxs = "i", las = 1, cex.axis = 0.8)
mtext("age, years", 2, line = 2.4, cex = 0.85)
for (z in list(list(H, "#2c7fb8", "House"), list(S, "#b3651a", "Senate"))) {
  d <- z[[1]]; cc <- z[[2]]
  polygon(c(d$year, rev(d$year)), c(d$p25_age, rev(d$p75_age)),
          col = adjustcolor(cc, 0.13), border = NA)
  lines(d$year, d$median_age, col = cc, lwd = 1.9)
  text(max(d$year) + 3, d$median_age[nrow(d)], z[[3]], col = cc, font = 2,
       cex = 0.8, adj = 0, xpd = NA)
}
points(P$year, P$age_at_term_start, pch = 19, cex = 0.6,
       col = adjustcolor("#4d9221", 0.85))
text(NOW + 5, P$age_at_term_start[nrow(P)] + 3, "Presidents", col = "#4d9221",
     font = 2, cex = 0.8, adj = 0, xpd = NA)
mtext("shaded band: middle half of the chamber", 3, line = 0.1, adj = 0,
      cex = 0.75, col = "#555")

## ---- step2-tab
C <- ac[ac$chamber == "Congress", ]
lo <- C[which.min(C$median_age), ]; hi <- C[which.max(C$median_age), ]
trough <- C[C$year >= 1965 & C$year <= 1995, ]
tr <- trough[which.min(trough$median_age), ]
data.frame(
  quantity = c("Youngest Congress on record (median age)",
               "Oldest Congresses on record (median age)",
               "Median age, most recent Congress",
               "Post-war low point",
               "Median senator now", "Median representative now"),
  value = c(paste0(f0(lo$median_age), "  (", lo$year, ")"),
            paste0(f0(hi$median_age), "  (", paste(OLDEST, collapse = ", "), ")"),
            paste0(f0(val("Congress", NOW)), "  (", NOW, ")"),
            paste0(f0(tr$median_age), "  (", tr$year, ")"),
            f0(val("Senate", NOW)), f0(val("House", NOW))))

## ---- step4-tab
data.frame(
  measure = c("Life expectancy at birth", "Remaining life expectancy at 65",
              "Remaining life expectancy at 75",
              "Median age of Congress"),
  `1950` = c(f1(E0a), f1(E6a), "--", f0(A0)),
  `2018` = c(f1(E0b), f1(E6b), f1(le$e75[le$year == EY]), f0(A1)),
  change = c(sgn(dE0), sgn(dE6),
             sgn(le$e75[le$year == EY] - le$e75[le$year == 1980]),
             sgn(dA, 0)),
  check.names = FALSE)

## ---- fig2-d3
BARS <- data.frame(
  lab = c("Life expectancy AT BIRTH", "Median age of CONGRESS",
          "Remaining life expectancy AT 65"),
  v   = c(dE0, dA, dE6),
  col = c("#8c8c8c", "#C41230", "#2c7fb8"), stringsAsFactors = FALSE)
BARS$s <- f1(BARS$v)
cat(sprintf('
<div id="f2" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const B=[%s], CONG=%s;
const W=760,H=252,M={t:44,r:120,b:40,l:250};
const svg=d3.select("#f2").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,12]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(B.map(d=>d.lab)).range([M.t,H-M.b]).padding(0.34);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#555").text("gain between 1950 and 2018, in years");
svg.append("text").attr("x",8).attr("y",20).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Two yardsticks for the same 68 years");
svg.selectAll("rect").data(B).join("rect")
  .attr("x",x(0)).attr("y",d=>y(d.lab)).attr("height",y.bandwidth())
  .attr("width",d=>x(d.v)-x(0)).attr("fill",d=>d.col).attr("fill-opacity",0.88);
svg.selectAll("text.v").data(B).join("text").attr("class","v")
  .attr("x",x(d3.max(B,d=>d.v))+9).attr("y",d=>y(d.lab)+y.bandwidth()/2+4)
  .attr("font-size","12.5px").attr("font-weight","600").attr("fill",d=>d.col)
  .text(d=>"+"+d.s+" yrs");
svg.selectAll("text.l").data(B).join("text").attr("class","l")
  .attr("x",M.l-10).attr("y",d=>y(d.lab)+y.bandwidth()/2+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("fill","#333").text(d=>d.lab);
svg.append("line").attr("x1",x(CONG)).attr("x2",x(CONG)).attr("y1",M.t-8)
  .attr("y2",H-M.b).attr("stroke","#C41230").attr("stroke-width",1.2)
  .attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(CONG)).attr("y",M.t-13).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230").text("Congress");
})();
</script>
', paste(sprintf('{"lab":"%s","v":%s,"col":"%s","s":"%s"}',
                 BARS$lab, f1(BARS$v), BARS$col, BARS$s), collapse = ","), f1(dA)))

## ---- fig2-static
BARS <- data.frame(
  lab = c("Remaining life expectancy AT 65", "Median age of CONGRESS",
          "Life expectancy AT BIRTH"),
  v   = c(dE6, dA, dE0),
  col = c("#2c7fb8", "#C41230", "#8c8c8c"), stringsAsFactors = FALSE)
par(mar = c(3.4, 15.2, 2.0, 3.2))
bp <- barplot(BARS$v, horiz = TRUE, col = adjustcolor(BARS$col, 0.88),
              border = NA, xlim = c(0, 12), names.arg = BARS$lab, las = 1,
              cex.names = 0.72, cex.axis = 0.8, space = 0.55)
abline(v = dA, lty = 2, col = "#C41230")
text(max(BARS$v) + 0.3, bp, paste0("+", f1(BARS$v), " yrs"), adj = 0,
     cex = 0.75, font = 2, col = BARS$col, xpd = NA)
mtext("gain between 1950 and 2018, in years", 1, line = 2.2, cex = 0.8)
mtext("Two yardsticks for the same 68 years", 3, line = 0.4, adj = 0,
      cex = 0.9, font = 2)

## ---- step4-share
data.frame(
  quantity = c("Gain in life expectancy at birth, 1950-2018",
               "Gain in remaining life expectancy at 65, 1950-2018",
               "Share of the headline gain that is about life after 65"),
  value = c(paste0(f1(dE0), " years"), paste0(f1(dE6), " years"),
            paste0(f0(REL), "%")))

## ---- fig3-d3
CC <- ac[ac$chamber == "Congress", ]
rows <- paste(sprintf('[%d,%s,%s]', CC$year, f2(CC$mean_entry_age),
                      f2(CC$mean_years_since_entry)), collapse = ",")
cat(sprintf('
<div id="f3" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=400,M={t:30,r:150,b:42,l:46};
const svg=d3.select("#f3").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1787,%d]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,66]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-H/2).attr("y",13)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#555")
  .text("mean age, years");
const a1=d3.area().x(d=>x(d[0])).y0(y(0)).y1(d=>y(d[1]));
const a2=d3.area().x(d=>x(d[0])).y0(d=>y(d[1])).y1(d=>y(d[1]+d[2]));
svg.append("path").datum(D).attr("d",a1).attr("fill","#2c7fb8").attr("fill-opacity",0.75);
svg.append("path").datum(D).attr("d",a2).attr("fill","#e08214").attr("fill-opacity",0.85);
svg.append("path").datum(D).attr("d",d3.line().x(d=>x(d[0])).y(d=>y(d[1]+d[2])))
  .attr("fill","none").attr("stroke","#333").attr("stroke-width",1.3);
const L=D[D.length-1];
svg.append("text").attr("x",x(L[0])+8).attr("y",y(L[1]+L[2]/2)+4)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#a4590f")
  .text("years since arriving");
svg.append("text").attr("x",x(L[0])+8).attr("y",y(L[1]/2)+4)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#1d5c88")
  .text("age on arriving");
svg.append("text").attr("x",x(L[0])+8).attr("y",y(L[1]+L[2])-6)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#333")
  .text("mean age");
svg.append("text").attr("x",M.l).attr("y",16).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Every Congress, split into the two things that make it old");
})();
</script>
', rows, NOW + 2))

## ---- fig3-static
CC <- ac[ac$chamber == "Congress", ]
par(mar = c(3.4, 3.6, 1.8, 7.4))
plot(NA, xlim = c(1787, NOW + 2), ylim = c(0, 66), xlab = "", ylab = "",
     xaxs = "i", yaxs = "i", las = 1, cex.axis = 0.8)
mtext("mean age, years", 2, line = 2.4, cex = 0.85)
polygon(c(CC$year, rev(CC$year)),
        c(CC$mean_entry_age, rep(0, nrow(CC))),
        col = adjustcolor("#2c7fb8", 0.75), border = NA)
polygon(c(CC$year, rev(CC$year)),
        c(CC$mean_entry_age + CC$mean_years_since_entry, rev(CC$mean_entry_age)),
        col = adjustcolor("#e08214", 0.85), border = NA)
lines(CC$year, CC$mean_age, col = "#333333", lwd = 1.3)
L <- CC[nrow(CC), ]
text(NOW + 4, L$mean_entry_age + L$mean_years_since_entry / 2,
     "years since\narriving", col = "#a4590f", font = 2, cex = 0.7, adj = 0, xpd = NA)
text(NOW + 4, L$mean_entry_age / 2, "age on\narriving", col = "#1d5c88",
     font = 2, cex = 0.7, adj = 0, xpd = NA)
mtext("Every Congress, split into the two things that make it old", 3,
      line = 0.4, adj = 0, cex = 0.9, font = 2)

## ---- step5-tab
data.frame(
  chamber = c("Congress", "House", "Senate"),
  `rise in mean age` = c(sgn(DC["total"], 2), sgn(DH["total"], 2), sgn(DS["total"], 2)),
  `from entering older` = c(sgn(DC["entry"], 2), sgn(DH["entry"], 2), sgn(DS["entry"], 2)),
  `from staying longer` = c(sgn(DC["stay"], 2), sgn(DH["stay"], 2), sgn(DS["stay"], 2)),
  `entering older, share` = c(paste0(f0(100 * DC["entry"] / DC["total"]), "%"),
                              paste0(f0(100 * DH["entry"] / DH["total"]), "%"),
                              paste0(f0(100 * DS["entry"] / DS["total"]), "%")),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
