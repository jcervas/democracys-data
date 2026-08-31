# sweet-spot-code.R -- chunk bodies for sweet-spot-brief.Rmd
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

d   <- read.csv("data/derived/districts.csv",  stringsAsFactors = FALSE)
mem <- read.csv("data/derived/members.csv",    stringsAsFactors = FALSE)
bn  <- read.csv("data/derived/bins.csv",       stringsAsFactors = FALSE)
lub <- read.csv("data/derived/lublin.csv",     stringsAsFactors = FALSE)
sim <- read.csv("data/derived/simulation.csv", stringsAsFactors = FALSE)
pk  <- read.csv("data/derived/sweetspot.csv",  stringsAsFactors = FALSE)
fit <- read.csv("data/derived/fitted.csv",     stringsAsFactors = FALSE)
sc  <- read.csv("data/derived/surname_check.csv", stringsAsFactors = FALSE)
sg  <- read.csv("data/derived/surname_grade.csv", stringsAsFactors = FALSE)
hyp <- read.csv("data/derived/surname_hyphen.csv", stringsAsFactors = FALSE)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

LAB <- c("0-20%", "20-30%", "30-40%", "40-45%", "45-50%", "50-55%",
         "55-60%", "60-70%", "70-80%", "80-100%")
bn$bin  <- factor(bn$bin,  levels = LAB)
lub$bin <- factor(lub$bin, levels = LAB)

# ---- the chamber -----------------------------------------------------------
NDIST  <- nrow(d)
NBLACK <- sum(d$black)
NHISP  <- sum(d$hisp)
NBOTH  <- sum(d$both)
NCHG   <- nrow(mem) - NDIST
BLK_R  <- d[d$black & d$party == "Republican", ]
HSP_R  <- d[d$hisp  & d$party == "Republican", ]

# ---- the six measures of "majority Black" ---------------------------------
BASES <- c(pop_black_low_pct = "Total population, Black alone",
           vap_black_low_pct = "Voting-age population, Black alone",
           cvap_black_low_pct = "Citizen voting-age population, Black alone",
           pop_black_high_pct = "Total population, any-part Black",
           vap_black_high_pct = "Voting-age population, any-part Black",
           cvap_black_high_pct = "Citizen voting-age population, any-part Black")
MAJ <- sapply(names(BASES), function(b) sum(d[[b]] > 50))
MAJ_LO <- min(MAJ); MAJ_HI <- max(MAJ)
over <- sapply(names(BASES), function(b) d[[b]] > 50)
FLIP <- d[rowSums(over) > 0 & rowSums(over) < length(BASES), ]
FLIP <- FLIP[order(-FLIP$pop_black_low_pct), ]

# ---- the 40-50% band, then and now ----------------------------------------
L3 <- lub[lub$table == 3 & lub$panel == "A" & lub$chamber == "U.S. House", ]
L3 <- L3[order(match(L3$bin, LAB)), ]
LUB_4050 <- sum(L3$n[L3$bin %in% c("40-45%", "45-50%")])
LUB_SEN  <- lub[lub$table == 3 & lub$chamber == "State Senate" & lub$bin == "45-50%", ]
LUB_SH   <- lub[lub$table == 3 & lub$chamber == "State House"  & lub$bin == "45-50%", ]

NOW <- bn[bn$group == "Black" & bn$base == "total population", ]
NOW <- NOW[order(match(NOW$bin, LAB)), ]
NOW_4050  <- sum(NOW$n[NOW$bin %in% c("40-45%", "45-50%")])
NOW_4050E <- sum(NOW$elected[NOW$bin %in% c("40-45%", "45-50%")])

# ---- the simulation --------------------------------------------------------
S03  <- pk[pk$sigma == 0.03, ]
P30  <- pk[pk$BD == 0.30, ]
P40  <- pk[pk$BD == 0.40, ]
FIG1A <- P30[P30$sigma == 0.03, ]
SIGMAS <- sort(unique(pk$sigma))

# ---- the sweet spot, looked for ------------------------------------------
band <- d[d$cvap_black_low_pct >= 30 & d$cvap_black_low_pct < 50, ]
BAND_W <- median(band$rep_pct[band$black])
BAND_L <- median(band$rep_pct[!band$black])
sm <- glm(black ~ cvap_black_low_pct + rep_pct + I(rep_pct^2),
          family = binomial, data = d)
SM <- summary(sm)$coefficients
QP <- SM["I(rep_pct^2)", "Pr(>|z|)"]

HALF <- function(m) 100 * fit$half[fit$model == m][1]

# ---- render every data.frame in this document as a TABLE ------------------
knit_print.data.frame <- function(x, ...) {
  nm <- names(x); nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

PAL <- c(win = "#2c7fb8", lose = "#d9a441", warn = "#C41230",
         grey = "#8a8a8a", green = "#4d9221", purple = "#8856a7")

## ---- raw
# The page's markup stays markup: the point is WHERE the identifier sits, which
# is a fact about the HTML and not a field in a record. `A000370` appears only
# inside the photograph's filename.
cat("```\n", paste(c(
"history.house.gov/People/Search?filter=1        (Black Americans)",
"",
'  <div class="result grid_5 alpha">',
'    <a href="/People/Detail/15032409256?current_search_qs=...">',
'      <img alt="ADAMS, Alma" src=".../People/Listing/A/A000370.jpg" />',
'      <span class="name">ADAMS, Alma</span>',
"",
"                                       A000370  <- the join key"),
collapse = "\n"), "\n```\n", sep = "")

## ---- raw-historian
.h <- head(read.csv("data/raw/historian.csv", stringsAsFactors = FALSE,
                    colClasses = "character"), 2)
data.frame(
  Column = names(.h),
  What_it_holds = c(
    "which of the Historian's two lists this member came from",
    "the member's name, as the gallery prints it",
    "the Bioguide ID, scraped out of the photograph's filename",
    "the numeric ID in the member's detail-page URL"),
  Row_1 = unname(unlist(.h[1, ])),
  Row_2 = unname(unlist(.h[2, ])))

## ---- one-row
o <- d[d$key == "SC-06", c("key", "name", "party", "pop_total",
                           "pop_black_low_pct", "vap_black_low_pct",
                           "cvap_black_low_pct", "rep_pct", "black")]
o[, 5:8] <- round(o[, 5:8], 1)
names(o) <- c("district", "member", "party", "population", "Black % of pop",
              "Black % of adults", "Black % of citizen adults",
              "Trump % 2020", "Historian: Black")
o

## ---- file
data.frame(
  quantity = c("Congressional districts", "People who served in the House",
               "Seats that changed occupant", "Districts electing a Black member",
               "Districts electing a Latino member",
               "Members on both Historian lists",
               "Black members elected as Republicans",
               "Latino members elected as Republicans"),
  value = c(n(NDIST), n(nrow(mem)), n(NCHG), n(NBLACK), n(NHISP), n(NBOTH),
            n(nrow(BLK_R)), n(nrow(HSP_R))))

## ---- sigma-static
par(mfrow = c(1, 2), mar = c(4.3, 4.3, 2.6, 1.0))
cols <- colorRampPalette(c("#cfe3f2", "#17527a"))(length(SIGMAS))
for (bd in c(0.30, 0.40)) {
  plot(NA, xlim = c(0, 60), ylim = c(0, 1.02), las = 1,
       xlab = "Republicans in the electorate (%)",
       ylab = if (bd == 0.30) "P(Black Democrat wins the seat)" else "")
  rect(-5, -0.1, 65, 1.2, col = "#fafafa", border = NA)
  for (i in seq_along(SIGMAS)) {
    z <- sim[sim$BD == bd & sim$sigma == SIGMAS[i], ]
    z <- z[order(z$R), ]
    lines(100 * z$R, z$p, col = cols[i], lwd = if (SIGMAS[i] == 0.03) 3 else 1.8)
  }
  pp <- pk[pk$BD == bd, ]
  points(100 * pp$peak_R, pp$peak_p, pch = 19, cex = 0.8, col = PAL["warn"])
  lines(100 * pp$peak_R, pp$peak_p, col = PAL["warn"], lty = 3)
  mtext(sprintf("BD = %d%%", round(100 * bd)), 3, line = 1.0, cex = 0.92)
  mtext(sprintf("peak moves %.1f points across sigma",
                100 * (max(pp$peak_R) - min(pp$peak_R))), 3, line = 0.1,
        cex = 0.74, col = "#666")
  if (bd == 0.30)
    legend("topleft", legend = sprintf("%.3f", SIGMAS), col = cols, lwd = 2,
           bty = "n", cex = 0.66, title = expression(sigma), title.adj = 0)
}

## ---- sigma-d3
# ---------------------------------------------------------------------------
# Seven curves per panel, and the chapter's whole complaint is that the peak
# moves as sigma changes. On paper the reader has to trace one curve out of
# seven overlapping ones. Here selecting a sigma isolates it in BOTH panels and
# marks its peak, which turns "the sweet spot is not one place" from an
# assertion into something you operate.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
BDS <- c(0.30, 0.40)
Rv  <- sort(unique(sim$R))
ser <- unlist(lapply(BDS, function(bd) vapply(SIGMAS, function(s) {
  z <- sim[sim$BD == bd & sim$sigma == s, ]
  z <- z[order(z$R), ]
  paste0('{bd:', bd, ',s:', s, ',p:[',
         paste(formatC(z$p, format = "f", digits = 4), collapse = ","), ']}')
}, character(1))))
pks <- vapply(seq_len(nrow(pk[pk$BD %in% BDS, ])), function(i) {
  q <- pk[pk$BD %in% BDS, ][i, ]
  paste0('{bd:', q$BD, ',s:', q$sigma, ',R:', q$peak_R, ',p:',
         formatC(q$peak_p, format = "f", digits = 4), '}')
}, character(1))
cat(paste0('
<div id="ssg" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', paste(ser, collapse = ","), '];
const PK=[', paste(pks, collapse = ","), '];
const RV=[', paste(formatC(Rv, format = "f", digits = 4), collapse = ","), '];
const SIG=[', paste(SIGMAS, collapse = ","), '];
const BDS=[', paste(BDS, collapse = ","), '];
const WARN="', PAL["warn"], '";
const cols=d3.scaleLinear().domain([0,SIG.length-1])
  .range(["#cfe3f2","#17527a"]);
const W=770,H=400,PW=W/2,M={t:38,r:18,b:48,l:56};
const box=d3.select("#ssg");
const bar=box.append("div").attr("style","margin:0 0 6px;font:11.5px inherit");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
let sel=null;                     // null = show all seven
const panels=BDS.map(function(bd,pi){
  const ox=pi*PW;
  const g=svg.append("g").attr("transform","translate("+ox+",0)");
  const x=d3.scaleLinear().domain([0,60]).range([M.l,PW-M.r]);
  const y=d3.scaleLinear().domain([0,1.02]).range([H-M.b,M.t]);
  g.append("rect").attr("x",M.l).attr("y",M.t).attr("width",PW-M.r-M.l)
   .attr("height",H-M.b-M.t).attr("fill","#fafafa");
  g.append("g").attr("transform","translate(0,"+(H-M.b)+")")
   .call(d3.axisBottom(x).ticks(6));
  g.append("g").attr("transform","translate("+M.l+",0)")
   .call(d3.axisLeft(y).ticks(5));
  g.append("text").attr("x",M.l).attr("y",20).attr("font-size","12px")
   .attr("font-weight","700").text("BD = "+Math.round(100*bd)+"%");
  g.append("text").attr("x",(M.l+PW-M.r)/2).attr("y",H-12)
   .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
   .text("Republicans in the electorate (%)");
  if(pi===0) g.append("text").attr("transform","rotate(-90)")
   .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
   .attr("font-size","11px").attr("fill","#4E5A63")
   .text("P(Black Democrat wins the seat)");
  const line=d3.line().x((v,i)=>x(100*RV[i])).y(v=>y(v));
  const paths=SIG.map(function(s,si){
    const rec=S.find(z=>z.bd===bd&&z.s===s);
    return g.append("path").attr("fill","none").attr("stroke",cols(si))
      .attr("stroke-width",s===0.03?3:1.8).attr("d",line(rec.p))
      .attr("data-sigma",s);
  });
  const pp=PK.filter(z=>z.bd===bd).sort((a,b)=>a.s-b.s);
  const peakLine=g.append("path").attr("fill","none").attr("stroke",WARN)
    .attr("stroke-dasharray","2 3")
    .attr("d",d3.line().x(z=>x(100*z.R)).y(z=>y(z.p))(pp));
  const peakDots=g.selectAll("circle.pk").data(pp).join("circle")
    .attr("class","pk").attr("cx",z=>x(100*z.R)).attr("cy",z=>y(z.p))
    .attr("r",3.4).attr("fill",WARN);
  g.append("rect").attr("x",M.l).attr("y",M.t).attr("width",PW-M.r-M.l)
   .attr("height",H-M.b-M.t).attr("fill","transparent")
   .on("mousemove",function(e){
     const rr=x.invert(d3.pointer(e,this)[0]+M.l)/100;
     let i=d3.bisectCenter(RV,rr); if(i<0)i=0; if(i>=RV.length)i=RV.length-1;
     const shown=sel===null?SIG:[sel];
     const rows=shown.map(function(s){
       const rec=S.find(z=>z.bd===bd&&z.s===s);
       const si=SIG.indexOf(s);
       return "<span style=\\"color:"+cols(si)+"\\">&#9632;</span> \\u03c3="+
         s.toFixed(3)+": "+rec.p[i].toFixed(3);
     }).join("<br>");
     const r=box.node().getBoundingClientRect();
     tip.style("opacity",1)
        .style("left",(e.clientX-r.left+14)+"px")
        .style("top",(e.clientY-r.top-10)+"px")
        .html("<b>BD = "+Math.round(100*bd)+"%</b><br>Republicans: "+
              (100*RV[i]).toFixed(1)+"%<br>"+rows);
   })
   .on("mouseleave",function(){tip.style("opacity",0);});
  return {paths:paths,peakLine:peakLine,peakDots:peakDots};
});
function draw(){
  panels.forEach(function(P){
    P.paths.forEach(function(p,si){
      const on=sel===null||SIG[si]===sel;
      p.attr("opacity",on?1:0.10)
       .attr("stroke-width",SIG[si]===sel?3.4:(SIG[si]===0.03?3:1.8));
    });
    P.peakDots.attr("opacity",z=>sel===null||z.s===sel?1:0.15);
    P.peakLine.attr("opacity",sel===null?1:0.2);
  });
  btns.style("background",s=>s===sel?"#1C4C5C":"#fff")
      .style("color",s=>s===sel?"#fff":"#12181D")
      .style("font-weight",s=>s===sel?"600":"400");
  allb.style("background",sel===null?"#1C4C5C":"#fff")
      .style("color",sel===null?"#fff":"#12181D")
      .style("font-weight",sel===null?"600":"400");
}
bar.append("span").attr("style","margin-right:8px;color:#4E5A63")
   .text("isolate \\u03c3:");
const allb=bar.append("button")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text("all seven").on("click",function(){sel=null;draw();});
const btns=bar.selectAll("button.s").data(SIG).join("button").attr("class","s")
  .attr("style","margin:0 6px 4px 0;padding:3px 9px;border:1px solid #CBD3D8;'
, 'border-radius:3px;cursor:pointer;font:11.5px inherit;background:#fff")
  .text(s=>s.toFixed(3))
  .on("click",function(e,s){sel=(sel===s?null:s);draw();});
draw();
})();
</script>'))

## ---- sigma-table
o <- pk[pk$BD %in% c(0.30, 0.40), ]
o <- data.frame(
  BD = paste0(round(100 * o$BD), "%"),
  sigma = sprintf("%.3f", o$sigma),
  peak_at = paste0(pc(100 * o$peak_R), "%"),
  peak_height = pc(o$peak_p, 3),
  plateau_width = paste0(pc(100 * o$plateau_width, 1), " pts"))
names(o) <- c("BD", "sigma", "Peak at R =", "Height of peak",
              "Width of the near-flat top")
o

## ---- lublin-3a
o <- data.frame(bin = as.character(L3$bin),
                percent_black_elected = ifelse(is.na(L3$pct), "--",
                                               paste0(pc(L3$pct), "%")),
                number_of_cases = n(L3$n))
names(o) <- c("Black share of district", "% electing a Black member (2015)",
              "Number of districts")
o

## ---- now-table
o <- data.frame(bin = as.character(NOW$bin),
                then = ifelse(is.na(L3$pct), "--", paste0(pc(L3$pct), "% of ", L3$n)),
                now  = ifelse(NOW$n == 0, "--",
                              paste0(pc(NOW$pct), "% of ", NOW$n)))
names(o) <- c("Black share of total population",
              "2015, as published (Lublin Table 3A)",
              "118th Congress, rebuilt here")
o

## ---- bins-static
par(mar = c(4.4, 4.6, 2.6, 1.2))
x <- seq_along(LAB)
plot(NA, xlim = c(0.5, length(LAB) + 0.5), ylim = c(0, 105), xaxt = "n",
     las = 1, xlab = "Black share of the district's total population",
     ylab = "% of districts electing a Black member")
rect(3.5, -5, 5.5, 112, col = "#f0f5ea", border = NA)
axis(1, at = x, labels = LAB, cex.axis = 0.7, las = 2)
sizef <- function(v) 0.5 + 2.2 * sqrt(pmax(v, 0)) / sqrt(max(c(L3$n, NOW$n)))
lines(x, L3$pct, col = PAL["grey"], lwd = 1.6, lty = 2)
points(x, L3$pct, pch = 19, cex = sizef(L3$n), col = PAL["grey"])
lines(x, NOW$pct, col = PAL["win"], lwd = 2.2)
points(x, NOW$pct, pch = 19, cex = sizef(NOW$n), col = PAL["win"])
text(4.5, 103, "the band the paper is about", cex = 0.72, col = PAL["green"])
for (i in c(4, 5)) {
  text(i - 0.30, L3$pct[i] - 8, paste0("n=", L3$n[i]), cex = 0.66,
       col = PAL["grey"], adj = 1)
  text(i + 0.30, NOW$pct[i] - 8, paste0("n=", NOW$n[i]), cex = 0.66,
       col = PAL["win"], adj = 0)
}
legend("bottomright", c("2015, as published", "118th Congress"),
       col = c(PAL["grey"], PAL["win"]), lwd = c(1.6, 2.2), lty = c(2, 1),
       pch = 19, bty = "n", cex = 0.76)
mtext("dot area is proportional to the number of districts in the bin", 3,
      line = 0.4, cex = 0.78, col = "#666")

## ---- bins-d3
# A percentage computed from three districts and one computed from thirty-one
# look identical on a line chart, which is why the static twin scales the dots.
# Dot area still has to be judged by eye; the hover just says the denominator.
# An empty bin has no percentage, and R's NA is not a JavaScript token: emitted
# bare it raises a ReferenceError that kills the whole figure. Every value that
# can be missing goes through jn().
jn <- function(v, k = 1) {
  ifelse(is.na(v), "null", formatC(v, format = "f", digits = k))
}
rows <- vapply(seq_along(LAB), function(i) {
  paste0('{bin:"', LAB[i], '",o_pct:', jn(L3$pct[i]),
         ',o_n:', jn(L3$n[i], 0), ',o_e:', jn(L3$elected[i], 0),
         ',n_pct:', jn(NOW$pct[i]),
         ',n_n:', jn(NOW$n[i], 0), ',n_e:', jn(NOW$elected[i], 0), '}')
}, character(1))
cat(paste0('
<div id="ssb" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', paste(rows, collapse = ","), '];
const GREY="', PAL["grey"], '", WIN="', PAL["win"], '", GRN="', PAL["green"], '";
const W=770,H=420,M={t:40,r:20,b:96,l:62};
const box=d3.select("#ssb");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(D.map(d=>d.bin)).range([M.l,W-M.r]).padding(0.5);
const y=d3.scaleLinear().domain([0,105]).range([H-M.b,M.t]);
const maxN=d3.max(D,d=>Math.max(d.o_n,d.n_n));
const rad=d3.scaleSqrt().domain([0,maxN]).range([1.5,11]);
// the band the paper is about
svg.append("rect").attr("x",x("40-45%")-24).attr("y",M.t)
  .attr("width",x("45-50%")-x("40-45%")+48).attr("height",H-M.b-M.t)
  .attr("fill","#f0f5ea");
svg.append("text").attr("x",(x("40-45%")+x("45-50%"))/2).attr("y",M.t-8)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill",GRN)
  .text("the band the paper is about");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x)).selectAll("text")
  .attr("transform","rotate(-45)").attr("text-anchor","end")
  .attr("dx","-0.5em").attr("dy","0.3em").style("font-size","10.5px");
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(6));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",14).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63")
  .text("% of districts electing a Black member");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-12)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
  .text("Black share of the district\\u2019s total population");
// A bin with no districts has no percentage. defined() breaks the line there
// rather than drawing through a gap that does not exist.
[["o_pct",GREY,"3 3",1.6],["n_pct",WIN,null,2.2]].forEach(function(s){
  svg.append("path").attr("fill","none").attr("stroke",s[1])
     .attr("stroke-width",s[3]).attr("stroke-dasharray",s[2])
     .attr("d",d3.line().defined(d=>d[s[0]]!==null)
        .x(d=>x(d.bin)).y(d=>y(d[s[0]]))(D));
});
D.forEach(function(d){
  if(d.o_pct!==null)
    svg.append("circle").attr("cx",x(d.bin)).attr("cy",y(d.o_pct))
       .attr("r",rad(d.o_n)).attr("fill",GREY);
  if(d.n_pct!==null)
    svg.append("circle").attr("cx",x(d.bin)).attr("cy",y(d.n_pct))
       .attr("r",rad(d.n_n)).attr("fill",WIN);
});
const leg=svg.append("g").attr("transform","translate("+(W-M.r-190)+","+(M.t+6)+")");
[["2015, as published",GREY],["118th Congress",WIN]].forEach(function(s,i){
  leg.append("circle").attr("cx",6).attr("cy",i*18).attr("r",5).attr("fill",s[1]);
  leg.append("text").attr("x",18).attr("y",i*18+4).attr("font-size","11px")
     .attr("fill","#4E5A63").text(s[0]);
});
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.selectAll("rect.h").data(D).join("rect").attr("class","h")
  .attr("x",d=>x(d.bin)-22).attr("y",M.t).attr("width",44)
  .attr("height",H-M.b-M.t).attr("fill","transparent")
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+d.bin+" Black</b><br>"+
         "<span style=\\"color:"+GREY+"\\">&#9679;</span> 2015: "+
           (d.o_pct===null?"no districts":
             d.o_pct.toFixed(1)+"% <span style=\\"color:#8a8a8a\\">("+
             d.o_e+" of "+d.o_n+" districts)</span>")+"<br>"+
         "<span style=\\"color:"+WIN+"\\">&#9679;</span> 118th: "+
           (d.n_pct===null?"no districts":
             d.n_pct.toFixed(1)+"% <span style=\\"color:#8a8a8a\\">("+
             d.n_e+" of "+d.n_n+" districts)</span>"));
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
})();
</script>'))

## ---- maj-count
o <- data.frame(measure = unname(BASES), districts = n(MAJ))
names(o) <- c("Measure of \"majority Black\"", "Districts over 50%")
o

## ---- flip-table
o <- data.frame(district = FLIP$key, member = FLIP$name)
for (b in names(BASES)) o[[b]] <- pc(FLIP[[b]])
names(o) <- c("District", "Member", "Pop, alone", "Adults, alone",
              "Cit. adults, alone", "Pop, any-part", "Adults, any-part",
              "Cit. adults, any-part")
o

## ---- flip-static
par(mar = c(4.4, 6.4, 2.6, 8.0))
y <- rev(seq_len(nrow(FLIP)))
plot(NA, xlim = c(41, 57), ylim = c(0.5, nrow(FLIP) + 0.5), yaxt = "n",
     bty = "n", las = 1, xlab = "Black share of the district (%)", ylab = "")
rect(50, 0.2, 60, nrow(FLIP) + 0.8, col = "#f0f5ea", border = NA)
abline(v = 50, col = PAL["green"], lwd = 2, lty = 2)
axis(2, at = y, labels = FLIP$key, las = 1, tick = FALSE, cex.axis = 0.86)
for (i in seq_len(nrow(FLIP))) {
  v <- as.numeric(FLIP[i, names(BASES)])
  segments(min(v), y[i], max(v), y[i], col = "#cfcfcf", lwd = 6)
  points(v, rep(y[i], length(v)), pch = c(19, 17, 15, 1, 2, 0), cex = 1.05,
         col = ifelse(v > 50, PAL["win"], PAL["lose"]))
}
text(50, nrow(FLIP) + 0.75, " Bartlett's line", pos = 4, cex = 0.74,
     col = PAL["green"], xpd = NA)
legend(57.6, nrow(FLIP) + 0.6, c("population", "adults", "citizen adults",
       "  (filled = Black alone,", "   open = any-part Black)"),
       pch = c(19, 17, 15, NA, NA), bty = "n", cex = 0.68, xpd = NA)
mtext("every district that is majority-Black on some measure and not on others",
      3, line = 0.5, cex = 0.78, col = "#666")

## ---- flip-d3
# Six marks per district, distinguished on paper by six point shapes that a
# reader has to decode against a legend. Here each mark says its own name, so
# "which measure is the one over the line" is answerable without the legend.
BLAB <- unname(BASES)
rows <- vapply(seq_len(nrow(FLIP)), function(i) {
  v <- as.numeric(FLIP[i, names(BASES)])
  paste0('{k:"', FLIP$key[i], '",v:[',
         paste(formatC(v, format = "f", digits = 2), collapse = ","), ']}')
}, character(1))
labs <- paste0('"', BLAB, '"', collapse = ",")
cat(paste0('
<div id="ssf" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', paste(rows, collapse = ","), '];
const LAB=[', labs, '];
const WIN="', PAL["win"], '", LOSE="', PAL["lose"], '", GRN="', PAL["green"], '";
const W=770,H=', 90 + 42 * nrow(FLIP), ',M={t:44,r:210,b:46,l:74};
const box=d3.select("#ssf");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([41,57]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.k)).range([M.t,H-M.b]).padding(0.45);
svg.append("rect").attr("x",x(50)).attr("y",M.t-10)
  .attr("width",x(57)-x(50)).attr("height",H-M.b-M.t+16).attr("fill","#f0f5ea");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t-10)
  .attr("y2",H-M.b+6).attr("stroke",GRN).attr("stroke-width",2)
  .attr("stroke-dasharray","5 4");
svg.append("text").attr("x",x(50)+6).attr("y",M.t-16).attr("font-size","11px")
  .attr("fill",GRN).text("Bartlett\\u2019s line");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickSize(0)).selectAll("text").style("font-size","12px");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-10)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
  .text("Black share of the district (%)");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
D.forEach(function(d){
  const cy=y(d.k)+y.bandwidth()/2;
  svg.append("line").attr("x1",x(d3.min(d.v))).attr("x2",x(d3.max(d.v)))
     .attr("y1",cy).attr("y2",cy).attr("stroke","#cfcfcf").attr("stroke-width",6);
  d.v.forEach(function(v,j){
    // filled = "Black alone" (first three), open = any-part Black
    const solid=j<3;
    svg.append("circle").attr("cx",x(v)).attr("cy",cy).attr("r",5)
      .attr("fill",solid?(v>50?WIN:LOSE):"#fff")
      .attr("stroke",v>50?WIN:LOSE).attr("stroke-width",1.8)
      .on("mousemove",function(e){
        const r=box.node().getBoundingClientRect();
        tip.style("opacity",1)
           .style("left",(e.clientX-r.left+14)+"px")
           .style("top",(e.clientY-r.top-10)+"px")
           .html("<b>"+d.k+"</b><br><b>"+LAB[j]+"</b><br>"+v.toFixed(2)+"% \\u2014 "+
             (v>50?"<span style=\\"color:"+WIN+"\\">over the line</span>":
                   "<span style=\\"color:"+LOSE+"\\">under the line</span>")+
             "<hr style=\\"border:none;border-top:1px solid #E4E8EA;margin:4px 0\\">"+
             LAB.map((L,q)=>"&nbsp;"+L+": "+d.v[q].toFixed(2)+"%").join("<br>"));
      })
      .on("mouseleave",function(){tip.style("opacity",0);});
  });
});
const leg=svg.append("g").attr("transform","translate("+(W-M.r+16)+","+M.t+")");
leg.append("text").attr("x",0).attr("y",-8).attr("font-size","10.5px")
   .attr("fill","#4E5A63").text("filled = Black alone");
leg.append("text").attr("x",0).attr("y",6).attr("font-size","10.5px")
   .attr("fill","#4E5A63").text("open = any-part Black");
LAB.forEach(function(L,j){
  leg.append("circle").attr("cx",6).attr("cy",26+j*17).attr("r",4.5)
     .attr("fill",j<3?"#8a8a8a":"#fff").attr("stroke","#8a8a8a")
     .attr("stroke-width",1.6);
  leg.append("text").attr("x",17).attr("y",30+j*17).attr("font-size","10px")
     .attr("fill","#4E5A63").text(L.replace("Total population","Population")
     .replace("Voting-age population","Adults")
     .replace("Citizen voting-age population","Citizen adults")
     .replace(", Black alone","").replace(", any-part Black",""));
});
})();
</script>'))

## ---- halfway
o <- data.frame(
  model = c("Black, total population", "Black, citizen voting-age",
            "Latino, total population", "Latino, citizen voting-age"),
  half = paste0(pc(sapply(c("Black, total population", "Black, citizen voting-age",
                            "Latino, total population", "Latino, citizen voting-age"),
                          HALF)), "%"))
names(o) <- c("Group and population base", "Share giving even odds")
o

## ---- scatter-static
par(mar = c(4.4, 4.6, 2.8, 1.2))
plot(NA, xlim = c(0, 75), ylim = c(0, 80), las = 1,
     xlab = "Black share of citizen voting-age population (%)",
     ylab = "Trump share of the 2020 vote (%)")
rect(30, -5, 50, 85, col = "#f7f3ea", border = NA)
# One line per value of BD in the paper's Figure 1 -- the model does not name a
# single sweet spot, it names one for each assumed Black Democratic share.
abline(h = 100 * S03$peak_R, col = PAL["purple"], lwd = 1.6, lty = 2)
text(74, 100 * max(S03$peak_R) + 3.2,
     "the model's sweet spot, one line per BD", pos = 2, cex = 0.72,
     col = PAL["purple"])
text(40, 78, "30-50% Black", cex = 0.72, col = "#9c8348")
points(d$cvap_black_low_pct[!d$black], d$rep_pct[!d$black], pch = 1, cex = 0.62,
       col = PAL["lose"])
points(d$cvap_black_low_pct[d$black], d$rep_pct[d$black], pch = 19, cex = 0.78,
       col = PAL["win"])
segments(30, BAND_W, 50, BAND_W, col = PAL["win"], lwd = 2.4)
segments(30, BAND_L, 50, BAND_L, col = PAL["lose"], lwd = 2.4)
text(50.6, BAND_W, sprintf(" elected a Black member: median %.1f", BAND_W),
     pos = 4, cex = 0.68, col = PAL["win"], xpd = NA)
text(50.6, BAND_L, sprintf(" did not: median %.1f", BAND_L), pos = 4,
     cex = 0.68, col = PAL["lose"], xpd = NA)
legend("topright", c("elected a Black member", "did not"),
       pch = c(19, 1), col = c(PAL["win"], PAL["lose"]), bty = "n", cex = 0.76)
mtext(sprintf("%d districts of the 118th Congress", NDIST), 3, line = 0.5,
      cex = 0.8, col = "#666")

## ---- scatter-d3
# 435 points, and on paper each one is anonymous. The chapter argues from where
# particular districts sit, so every point can name itself: district, member,
# party, and both coordinates.
esc <- function(z) gsub('"', "'", z, fixed = TRUE)
rows <- paste0('{k:"', d$key, '",m:"', esc(d$name), '",p:"',
               substr(d$party, 1, 1), '",b:', ifelse(d$black, 1, 0),
               ',x:', formatC(d$cvap_black_low_pct, format = "f", digits = 2),
               ',y:', formatC(d$rep_pct, format = "f", digits = 1), '}',
               collapse = ",")
peaks <- paste0('{bd:', S03$BD, ',R:', S03$peak_R, '}', collapse = ",")
cat(paste0('
<div id="ssc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const PKS=[', peaks, '];
const WIN="', PAL["win"], '", LOSE="', PAL["lose"], '", PUR="', PAL["purple"], '";
const BAND_W=', formatC(BAND_W, format = "f", digits = 1),
    ', BAND_L=', formatC(BAND_L, format = "f", digits = 1), ';
const W=770,H=470,M={t:34,r:22,b:50,l:60};
const box=d3.select("#ssc");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,75]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,80]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(30)).attr("y",M.t)
  .attr("width",x(50)-x(30)).attr("height",H-M.b-M.t).attr("fill","#f7f3ea");
svg.append("text").attr("x",(x(30)+x(50))/2).attr("y",M.t-8)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#9c8348")
  .text("30\\u201350% Black");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-12)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#4E5A63")
  .text("Black share of citizen voting-age population (%)");
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63")
  .text("Trump share of the 2020 vote (%)");
PKS.forEach(function(q){
  svg.append("line").attr("x1",M.l).attr("x2",W-M.r)
     .attr("y1",y(100*q.R)).attr("y2",y(100*q.R))
     .attr("stroke",PUR).attr("stroke-width",1.4).attr("stroke-dasharray","4 4");
});
svg.append("text").attr("x",W-M.r-4).attr("y",y(100*d3.max(PKS,q=>q.R))-6)
  .attr("text-anchor","end").attr("font-size","11px").attr("fill",PUR)
  .text("the model\\u2019s sweet spot, one line per BD");
[[BAND_W,WIN,"elected a Black member: median "],
 [BAND_L,LOSE,"did not: median "]].forEach(function(s){
  svg.append("line").attr("x1",x(30)).attr("x2",x(50))
     .attr("y1",y(s[0])).attr("y2",y(s[0]))
     .attr("stroke",s[1]).attr("stroke-width",2.6);
  svg.append("text").attr("x",x(50)+6).attr("y",y(s[0])+4)
     .attr("font-size","10.5px").attr("fill",s[1]).text(s[2]+s[0].toFixed(1));
});
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.selectAll("circle.d").data(D).join("circle").attr("class","d")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y))
  .attr("r",d=>d.b?4:2.6)
  .attr("fill",d=>d.b?WIN:"none")
  .attr("stroke",d=>d.b?WIN:LOSE).attr("stroke-width",1.2)
  .on("mousemove",function(e,d){
    d3.select(this).attr("r",d.b?6:5).raise();
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-10)+"px")
       .html("<b>"+d.k+"</b> \\u2014 "+d.m+" ("+d.p+")<br>"+
         "Black CVAP: "+d.x.toFixed(1)+"%<br>Trump 2020: "+d.y.toFixed(1)+"%<br>"+
         (d.b?"<span style=\\"color:"+WIN+"\\">elected a Black member</span>"
             :"<span style=\\"color:"+LOSE+"\\">did not</span>"));
  })
  .on("mouseleave",function(e,d){
    d3.select(this).attr("r",d.b?4:2.6);
    tip.style("opacity",0);
  });
const leg=svg.append("g").attr("transform","translate("+(W-M.r-186)+","+(M.t+6)+")");
[["elected a Black member",WIN,true],["did not",LOSE,false]].forEach(function(s,i){
  leg.append("circle").attr("cx",6).attr("cy",i*17).attr("r",s[2]?4:2.6)
     .attr("fill",s[2]?s[1]:"none").attr("stroke",s[1]).attr("stroke-width",1.2);
  leg.append("text").attr("x",18).attr("y",i*17+4).attr("font-size","11px")
     .attr("fill","#4E5A63").text(s[0]);
});
svg.append("text").attr("x",M.l).attr("y",20).attr("font-size","11px")
  .attr("fill","#666").text("', NDIST, ' districts of the 118th Congress");
})();
</script>'))

## ---- sweet-fit
o <- data.frame(term = c("Black share of citizen voting-age population",
                         "Trump share", "Trump share, squared"),
                estimate = sprintf("%+.4f", SM[2:4, 1]),
                std_error = pc(SM[2:4, 2], 4),
                p = ifelse(SM[2:4, 4] < 0.001, "<0.001", pc(SM[2:4, 4], 3)))
names(o) <- c("Term", "Estimate", "Std. error", "p")
o

## ---- republicans
o <- BLK_R[order(-BLK_R$rep_pct), ]
o <- data.frame(district = o$key, member = o$name,
                share = pc(o$cvap_black_low_pct), trump = pc(o$rep_pct))
names(o) <- c("District", "Member", "Black % of citizen adults", "Trump % 2020")
o

## ---- sn-match
data.frame(
  quantity = c("Seated members", "Matched on the whole surname",
               "Matched only after splitting a hyphen or space",
               "No entry in the surname file",
               "Matched, but Black share suppressed",
               "Matched, but Hispanic share suppressed"),
  n = c(n(nrow(sc)), n(sum(sc$matched) - sum(grepl("[ -]", sc$last) & sc$matched)),
        n(sum(grepl("[ -]", sc$last) & sc$matched)), n(sum(!sc$matched)),
        n(sum(sc$matched & is.na(sc$pctblack))),
        n(sum(sc$matched & is.na(sc$pcthispanic)))))

## ---- sn-grade
g <- sg[sg$threshold %in% c(25, 50, 75), ]
data.frame(
  group     = g$group,
  cutoff    = paste0(g$threshold, "%"),
  recovered = g$tp,
  missed    = g$fn,
  `false positives` = g$fp,
  precision = pc(g$precision, 2),
  check.names = FALSE)

## ---- sn-top
tp <- sc[sc$black & !is.na(sc$pctblack), c("last", "pctblack")]
tp <- tp[order(-tp$pctblack), ][1:5, ]
bt <- sc[sc$black & !is.na(sc$pctblack), c("last", "pctblack")]
bt <- bt[order(bt$pctblack), ][1:5, ]
data.frame(
  `highest` = paste0(tp$last, " (", pc(tp$pctblack), "%)"),
  `lowest`  = paste0(bt$last, " (", pc(bt$pctblack), "%)"),
  check.names = FALSE)

## ---- bisg-overall
cal <- read.csv("data/derived/bisg_calibration.csv", stringsAsFactors = FALSE)
bim <- read.csv("data/derived/bisg_members.csv",     stringsAsFactors = FALSE)
data.frame(
  labeller = c("District plurality only (ignores the name)",
               "Surname only (ignores the district)",
               "BISG (both)"),
  accuracy = pc(c(mean(bim$geo == bim$truth), mean(bim$surname == bim$truth),
                  mean(bim$bisg == bim$truth)), 3))

## ---- bisg-cal
data.frame(
  `district Black share` = cal$stratum, districts = cal$districts,
  geography = pc(cal$acc_geo, 2), surname = pc(cal$acc_surname, 2),
  BISG = pc(cal$acc_bisg, 2),
  `Black members` = cal$black_members,
  `found by BISG` = cal$black_found_bisg,
  check.names = FALSE)

## ---- bisg-down
dv <- read.csv("data/derived/bisg_downstream.csv", stringsAsFactors = FALSE)
data.frame(`dependent variable` = dv$labels,
           coefficient = pc(dv$coef, 3),
           p = format.pval(dv$p, digits = 2),
           check.names = FALSE)

## ---- prov
p <- read.csv("data/derived/checks.csv", stringsAsFactors = FALSE)
p$ok <- ifelse(is.na(p$ok), "", ifelse(p$ok == "TRUE", "yes", "NO"))
p$expected[is.na(p$expected)] <- ""
names(p) <- c("Check", "Value", "Expected", "Passed")
p

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#ssc text[fill="#2c7fb8" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label the same colour as the line it labels, invisible at 1:1.
# Recolouring would break the label-to-line link, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph.
# LIGHT PAGE ONLY: the on-mark chunk above pins this fill for the dark page,
# so a --paper stroke there would sit dark behind a dark ink, and the checker
# scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#ssc text[fill="#2c7fb8" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
