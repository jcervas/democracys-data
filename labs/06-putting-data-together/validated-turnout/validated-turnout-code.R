# validated-turnout-code.R -- chunk bodies for validated-turnout-brief.Rmd
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

cps <- read.csv("data/derived/cps_turnout.csv", stringsAsFactors = FALSE)
cps <- cps[order(cps$year), ]
cps$gap_people <- cps$reported_voters - cps$actual_votes
cps$gap_pct    <- 100 * cps$gap_people / cps$actual_votes

Y1 <- min(cps$year); Y2 <- max(cps$year)
g   <- function(yr, col) cps[[col]][cps$year == yr]
pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
cnt <- function(x) format(round(x), big.mark = ",")
mn  <- function(x, k = 1) formatC(x / 1e6, format = "f", digits = k)

era <- cut(cps$year, c(1963, 1979, 1999, 2030),
           labels = c("1964–1976", "1980–1996", "2000–2024"))
em  <- tapply(cps$gap_pct, era, mean)
rt  <- cps[!is.na(cps$over_report_pp), ]

# 2024, on the two competing denominators
lst    <- cps[cps$year == Y2, ]
r_cit  <- 100 * lst$actual_votes / lst$citizen_vap
r_vap  <- 100 * lst$actual_votes / (lst$vap_thousands * 1000)

# ---- figure data ----------------------------------------------------------

# one dot per election, packed so none sits on top of another
swarm <- function(x, gap) {
  lev <- c(0, as.vector(rbind(seq_len(6), -seq_len(6))))
  y <- rep(NA_real_, length(x))
  for (k in order(x)) {
    near <- which(!is.na(y) & abs(x - x[k]) < gap)
    y[k] <- lev[which(!(lev %in% y[near]))[1]]
  }
  y
}
cps$swarm <- unsplit(lapply(split(cps$gap_pct, era),
                            function(z) swarm(z, 1.1)), era)
era_n  <- as.integer(table(era))
era_lo <- tapply(cps$gap_pct, era, min); era_hi <- tapply(cps$gap_pct, era, max)

# the same ballots over two denominators, every year both exist
den    <- cps[!is.na(cps$citizen_vap), ]
den$rc <- 100 * den$actual_votes / den$citizen_vap
den$rv <- 100 * den$actual_votes / (den$vap_thousands * 1000)
den$sp <- den$rc - den$rv
den_min <- den[which.min(den$sp), ]; den_max <- den[which.max(den$sp), ]

# the by-group table, as five elections rather than one
gnm  <- c("White (non-Hispanic)", "Black", "Asian", "Hispanic")
gvar <- c("pct_white_nh", "pct_black", "pct_asian", "pct_hispanic")
gcol <- c("#4d9221", "#C41230", "#2c7fb8", "#8856a7")
rg   <- cps[cps$year >= 2008, c("year", gvar)]
rg_sp <- function(y) max(as.numeric(rg[rg$year == y, gvar])) -
                     min(as.numeric(rg[rg$year == y, gvar]))

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- rawcps
# A verbatim capture of the workbook's top-left corner, cells truncated to 14
# characters. NA is what an empty cell reads as -- and the empty cells are the
# point, so they are shown rather than tidied away. The sheet is a grid, so it
# is shown as one: the row numbers and column letters are the spreadsheet's.
data.frame(
  Row = 1:8,
  A = c("Table with ro", "Table A-1. Re", "(Numbers in t", "Year", "NA",
        "NA", "Total, Percen", "2024"),
  B = c("NA", "NA", "NA", "Total voting-", "NA", "NA", "NA", "260363"),
  C = c("NA", "NA", "NA", "Total percent", "NA", "Total populat", "NA",
        "59.3"),
  D = c("NA", "NA", "NA", "NA", "NA", "Citizen popul", "NA", "65.3"))

## ---- cleancps
cps[cps$year %in% c(2024, 2020, 2016),
    c("year", "vap_thousands", "reported_voters", "actual_votes",
      "cps_rate", "actual_rate")]

## ---- one-record
o <- cps[cps$year == 2016,
         c("year", "vap_thousands", "pct_citizen_basis", "reported_voters",
           "citizen_vap", "actual_votes")]
o$vap_thousands   <- cnt(o$vap_thousands * 1000)
o$reported_voters <- cnt(o$reported_voters)
o$citizen_vap     <- cnt(o$citizen_vap)
o$actual_votes    <- cnt(o$actual_votes)
names(o) <- c("year", "voting-age population", "% voted, citizen basis",
              "said they voted", "citizen VAP", "ballots for president")
o

## ---- scope
data.frame(
  quantity = c("Presidential elections in the file", "Period",
               "Households interviewed per wave (approx.)",
               "Elections with a citizen-population basis",
               "Smallest and largest reported electorate"),
  value = c(nrow(cps), paste(Y1, "to", Y2), "60,000",
            paste(sum(!is.na(cps$pct_citizen_basis)), "of", nrow(cps)),
            paste(cnt(min(cps$reported_voters)), "to",
                  cnt(max(cps$reported_voters)))))

## ---- y2016
o <- cps[cps$year == 2016, c("reported_voters", "actual_votes", "gap_people")]
o <- data.frame(quantity = c("Said they voted", "Ballots cast for president",
                             "Difference"),
                people = cnt(unlist(o)))
o

## ---- all-years
o <- data.frame(year = cps$year,
                gap_millions = pc(cps$gap_people / 1e6),
                over_by_pct  = pc(cps$gap_pct))
names(o) <- c("year", "gap (millions of people)", "survey over actual (%)")
o

## ---- band-static
par(mar = c(3.6, 4.8, 1.0, 1.4))
rv <- cps$reported_voters / 1e6; av <- cps$actual_votes / 1e6
plot(NA, xlim = range(cps$year), ylim = c(60, max(rv) * 1.06), las = 1,
     xlab = "", ylab = "millions of people")
polygon(c(cps$year, rev(cps$year)), c(rv, rev(av)),
        col = adjustcolor("#C41230", alpha.f = 0.22), border = NA)
lines(cps$year, rv, lwd = 2.4, col = "#C41230")
lines(cps$year, av, lwd = 2.4, col = "#2c7fb8")
points(cps$year, rv, pch = 19, cex = 0.7, col = "#C41230")
points(cps$year, av, pch = 19, cex = 0.7, col = "#2c7fb8")
legend("topleft", c("said they voted", "ballots cast for president"),
       lwd = 2.4, col = c("#C41230", "#2c7fb8"), bty = "n", cex = 0.8,
       seg.len = 1.6)
wy <- which.max(cps$gap_people)
arrows(cps$year[wy], av[wy], cps$year[wy], rv[wy], code = 3, length = 0.05,
       angle = 90, col = "grey25")
text(cps$year[wy], (rv[wy] + av[wy]) / 2, paste0(" ", mn(max(cps$gap_people)),
     "m"), pos = 4, cex = 0.76, col = "grey20")

## ---- band-d3
rows <- paste(sprintf('{"y":%d,"r":%.3f,"a":%.3f,"g":%.0f,"p":%.2f}',
                      cps$year, cps$reported_voters / 1e6,
                      cps$actual_votes / 1e6, cps$gap_people, cps$gap_pct),
              collapse = ",")
cat(sprintf('
<div id="band" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,H=420,M={t:18,r:26,b:44,l:60};
const box=d3.select("#band");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([60,d3.max(D,d=>d.r)*1.06]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("millions of people");
svg.append("path").datum(D).attr("fill","#C41230").attr("fill-opacity",0.2)
  .attr("d",d3.area().x(d=>x(d.y)).y0(d=>y(d.a)).y1(d=>y(d.r)));
[["r","#C41230","said they voted"],["a","#2c7fb8","ballots cast for president"]]
 .forEach((s,i)=>{
  svg.append("path").datum(D).attr("fill","none").attr("stroke",s[1])
    .attr("stroke-width",2.4).attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d[s[0]])));
  svg.append("text").attr("x",x(D[0].y)+8).attr("y",y(D[0][s[0]])+(i?18:-9))
    .attr("font-size","12px").attr("fill",s[1]).text(s[2]);});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.y)-14).attr("y",M.t).attr("width",28).attr("height",H-M.b-M.t)
  .attr("fill","transparent")
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.y}</b><br>said they voted ${d.r.toFixed(1)}m<br>`+
     `ballots ${d.a.toFixed(1)}m<br>gap ${d3.format(",")(d.g)} (${d.p.toFixed(1)}%%)`)
     .style("left",Math.min(x(d.y)-M.l+20,W-280)+"px").style("top",(M.t+4)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The shaded wedge is the gap. Both series roughly doubled over sixty years; the
wedge did not, which is the finding of this chapter in one picture. The widest year is
%d, at %sm people.</p>
', rows, cps$year[which.max(cps$gap_people)], mn(max(cps$gap_people))))

## ---- era
o <- data.frame(era = names(em), mean_over_report = pc(as.vector(em)))
names(o) <- c("era", "mean over-report (% of actual votes)")
o

## ---- bee-static
par(mar = c(4.4, 9.8, 0.8, 1.4))
lv <- levels(era); K <- length(lv)
# the axis labels go through the PDF device, which has no en dash and would
# substitute a hyphen without saying so. Do the substitution here instead, so
# print and screen differ by a decision rather than by an accident.
lvp <- gsub("\u2013", "-", lv)
plot(NA, xlim = c(-1.5, 15), ylim = c(0.4, K + 0.6), yaxt = "n", bty = "n",
     xlab = "survey electorate over ballot count (%)", ylab = "")
axis(2, at = seq_len(K), labels = rev(lvp), las = 1, tick = FALSE,
     cex.axis = 0.92)
abline(v = 0, col = "grey45")
for (k in seq_len(K)) {
  e <- rev(lv)[k]; z <- cps[era == e, ]
  m <- em[[e]]
  segments(m, k - 0.34, m, k + 0.34, col = "#C41230", lwd = 2.6)
  text(m, k + 0.44, paste0("mean ", pc(m), "%"), cex = 0.72, col = "#C41230")
  points(z$gap_pct, k + z$swarm * 0.115, pch = 21, bg = "#2c7fb8",
         col = "white", cex = 1.5, lwd = 1.2)
}

## ---- bee-d3
rows <- paste(sprintf('{"e":"%s","y":%d,"g":%.2f,"o":%.0f,"p":%.0f}',
                      as.character(era), cps$year, cps$gap_pct, cps$swarm,
                      round(cps$gap_people)), collapse = ",")
ers  <- paste(sprintf('{"e":"%s","m":%.2f,"n":%d}', levels(era),
                      as.vector(em), era_n), collapse = ",")
cat(sprintf('
<div id="bee" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], E=[%s];
const W=740,H=300,M={t:16,r:26,b:48,l:110};
const box=d3.select("#bee");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-1.5,15]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(E.map(d=>d.e)).range([M.t,H-M.b]).padding(0.18);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("survey electorate over ballot count");
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#666");
const yc=e=>y(e)+y.bandwidth()/2;
E.forEach(e=>{
  svg.append("line").attr("x1",x(e.m)).attr("x2",x(e.m))
    .attr("y1",yc(e.e)-y.bandwidth()/2.4).attr("y2",yc(e.e)+y.bandwidth()/2.4)
    .attr("stroke","#C41230").attr("stroke-width",2.6);
  svg.append("text").attr("x",x(e.m)).attr("y",yc(e.e)-y.bandwidth()/2.4-5)
    .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#C41230")
    .text("mean "+e.m.toFixed(1)+"%%");});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.g)).attr("cy",d=>yc(d.e)+d.o*8.5).attr("r",6.5)
  .attr("fill","#2c7fb8").attr("stroke","#fff").attr("stroke-width",1.4)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.y}</b><br>over-report ${d3.format("+.1f")(d.g)}%%<br>`+
     `${d3.format(",")(d.p)} people`)
     .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One dot per presidential election, %d in all. The %d elections of %s run from
%.1f%% to %.1f%%; the %d of %s run from %.1f%% to %.1f%%. The clouds moved; the
averages are a consequence, not the finding.</p>
', rows, ers, nrow(cps),
   era_n[1], levels(era)[1], era_lo[[1]], era_hi[[1]],
   era_n[3], levels(era)[3], era_lo[[3]], era_hi[[3]]))

## ---- gap-static
plot(cps$year, cps$gap_pct, type = "b", pch = 19, lwd = 2.2, col = "#C41230",
     ylim = c(-2, 15), las = 1, xlab = "",
     ylab = "Survey electorate over ballot count (%)")
abline(h = 0, lwd = 1)
segments(1964, em[1], 1976, em[1], col = "grey45", lty = 2, lwd = 2)
segments(1980, em[2], 1996, em[2], col = "grey45", lty = 2, lwd = 2)
segments(2000, em[3], 2024, em[3], col = "grey45", lty = 2, lwd = 2)
text(c(1970, 1988, 2012), c(em[1], em[2], em[3]) + 1.3,
     paste0(formatC(as.vector(em), format = "f", digits = 1), "%"),
     col = "grey35", cex = 0.85)

## ---- gap-d3
pts <- paste(sprintf('{"y":%d,"g":%.2f,"p":%d,"a":%d}',
                     cps$year, cps$gap_pct, round(cps$gap_people),
                     cps$actual_votes), collapse = ",")
ers <- paste(sprintf('{"a":%d,"b":%d,"m":%.2f}',
                     c(1964, 1980, 2000), c(1976, 1996, 2024),
                     as.vector(em)), collapse = ",")
cat(sprintf('
<div id="cpsgap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], E=[%s];
const W=770,H=430,M={t:20,r:24,b:42,l:58};
const svg=d3.select("#cpsgap").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1962,2026]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([-2,15]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>d+"%%").ticks(7));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#333");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("survey electorate over ballot count");
E.forEach(e=>{
  svg.append("line").attr("x1",x(e.a)).attr("x2",x(e.b)).attr("y1",y(e.m)).attr("y2",y(e.m))
    .attr("stroke","#999").attr("stroke-dasharray","5,4").attr("stroke-width",2);
  svg.append("text").attr("x",(x(e.a)+x(e.b))/2).attr("y",y(e.m)-8)
    .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#777")
    .text(d3.format(".1f")(e.m)+"%% average");
});
const ln=d3.line().x(d=>x(d.y)).y(d=>y(d.g));
svg.append("path").datum(D).attr("d",ln).attr("fill","none")
  .attr("stroke","#C41230").attr("stroke-width",2.4);
const tip=d3.select("#cpsgap").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.g)).attr("r",5)
  .attr("fill","#C41230").attr("stroke","#fff").attr("stroke-width",1.4)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(`<b>${d.y}</b><br>over-report ${d3.format("+.1f")(d.g)}%%<br>`+
      `${d3.format(",")(d.p)} people<br>${d3.format(",")(d.a)} ballots`)
      .style("left",Math.min(x(d.y)-M.l+18,W-260)+"px").style("top",(M.t+4)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Each point is one presidential election. Dashed lines are era averages. Hover
for the underlying counts.</p>
', pts, ers))

## ---- mechanisms
data.frame(
  mechanism = c("Over-claiming", "Weighting", "Our own construction"),
  what_it_is = c(
    "Respondents misremember, or say what they think they should have done",
    "The Bureau stretches respondents' answers to cover non-respondents",
    "Derived voter counts, and ballots for president rather than all ballots"),
  which_way = c("inflates the gap", "either direction", "inflates the gap"),
  can_we_see_it = c("No", "No", "Partly"),
  check.names = FALSE)

## ---- denominator
data.frame(
  denominator = c("Citizen voting-age population", "All voting-age population"),
  people = c(cnt(lst$citizen_vap), cnt(lst$vap_thousands * 1000)),
  turnout = c(pc(r_cit), pc(r_vap)),
  check.names = FALSE)

## ---- dumb-static
par(mar = c(4.4, 4.6, 1.8, 4.4))
K <- nrow(den)
plot(NA, xlim = c(45, 70), ylim = c(0.5, K + 0.5), yaxt = "n", bty = "n",
     xlab = "turnout for president (%)", ylab = "")
axis(2, at = seq_len(K), labels = den$year, las = 1, tick = FALSE,
     cex.axis = 0.82)
abline(v = seq(45, 70, 5), col = "grey93")
segments(den$rv, seq_len(K), den$rc, seq_len(K), col = "grey72", lwd = 3.2,
         lend = 1)
points(den$rv, seq_len(K), pch = 19, col = "#e08214", cex = 1.3)
points(den$rc, seq_len(K), pch = 19, col = "#2c7fb8", cex = 1.3)
text(den$rc, seq_len(K), sprintf(" %+.1f", den$sp), pos = 4, cex = 0.74,
     col = "#333", xpd = NA)
legend("top", c("all voting-age population", "citizen voting-age population"),
       pch = 19, col = c("#e08214", "#2c7fb8"), bty = "n", horiz = TRUE,
       cex = 0.74, inset = c(0, -0.07), xpd = NA)

## ---- dumb-d3
rows <- paste(sprintf('{"y":%d,"c":%.2f,"v":%.2f,"s":%.2f,"cv":%.0f,"vv":%.0f}',
                      den$year, den$rc, den$rv, den$sp, den$citizen_vap,
                      den$vap_thousands * 1000), collapse = ",")
cat(sprintf('
<div id="dmb" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=740,H=430,M={t:46,r:74,b:48,l:56};
const box=d3.select("#dmb");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([45,70]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.y)).range([M.t,H-M.b]).padding(0.4);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("turnout for president");
const yc=d=>y(d.y)+y.bandwidth()/2;
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
function hov(sel){sel.on("mousemove",function(e,d){tip.style("opacity",1).html(
   `<b>${d.y}</b><br>citizen VAP ${d.c.toFixed(1)}%% of `+
   `${d3.format(",")(d.cv)}<br>all VAP ${d.v.toFixed(1)}%% of `+
   `${d3.format(",")(d.vv)}<br>${d.s.toFixed(1)} points apart`)
   .style("left",Math.min(e.offsetX+14,W-330)+"px").style("top",(e.offsetY-10)+"px");})
 .on("mouseleave",()=>tip.style("opacity",0));}
hov(svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",d=>x(d.v)).attr("x2",d=>x(d.v)).attr("y1",yc).attr("y2",yc)
  .attr("stroke","#bbb").attr("stroke-width",4).attr("stroke-linecap","round")
  .transition().duration(700).attr("x2",d=>x(d.c)).selection());
hov(svg.append("g").selectAll("circle.v").data(D).join("circle")
  .attr("cx",d=>x(d.v)).attr("cy",yc).attr("r",5.5).attr("fill","#e08214"));
hov(svg.append("g").selectAll("circle.c").data(D).join("circle")
  .attr("cx",d=>x(d.v)).attr("cy",yc).attr("r",5.5).attr("fill","#2c7fb8")
  .transition().duration(700).attr("cx",d=>x(d.c)).selection());
svg.append("g").selectAll("text.s").data(D).join("text")
  .attr("x",d=>x(d.c)+12).attr("y",d=>yc(d)+4).attr("font-size","11.5px")
  .attr("fill","#333").attr("opacity",0).text(d=>"+"+d.s.toFixed(1))
  .transition().delay(700).duration(300).attr("opacity",1);
[["all voting-age population","#e08214",0],
 ["citizen voting-age population","#2c7fb8",250]].forEach(k=>{
  svg.append("circle").attr("cx",M.l+6+k[2]).attr("cy",22).attr("r",5.5).attr("fill",k[1]);
  svg.append("text").attr("x",M.l+18+k[2]).attr("y",26).attr("font-size","11.5px")
    .attr("fill","#333").text(k[0]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Identical ballot counts, two denominators. The rod was %.1f points long in %d
and %.1f points long in %d — the denominator is not a fixed convention but a
growing one, and the growth is immigration, not turnout.</p>
', rows, den_min$sp, den_min$year, den_max$sp, den_max$year))

## ---- missing
o <- cps[cps$year <= 1980, c("year", "reported_voters", "actual_votes",
                             "cps_rate", "actual_rate")]
o$reported_voters <- cnt(o$reported_voters)
o$actual_votes    <- cnt(o$actual_votes)
o$cps_rate    <- ifelse(is.na(o$cps_rate), "—", pc(o$cps_rate))
o$actual_rate <- ifelse(is.na(o$actual_rate), "—", pc(o$actual_rate))
names(o) <- c("year", "said they voted", "ballots", "survey rate (%)",
              "actual rate (%)")
o

## ---- by-race
o <- cps[cps$year >= 2008,
         c("year", "pct_white_nh", "pct_black", "pct_asian", "pct_hispanic")]
o <- o[order(-o$year), ]
names(o) <- c("year", "White (non-Hispanic)", "Black", "Asian", "Hispanic")
o

## ---- slope-static
par(mar = c(3.4, 4.6, 1.0, 10.4))
plot(NA, xlim = c(min(rg$year), max(rg$year)), ylim = c(40, 76), las = 1,
     xlab = "", ylab = "reported turnout among citizens (%)", xaxt = "n")
axis(1, at = rg$year, labels = rg$year, cex.axis = 0.9)
abline(h = seq(40, 75, 5), col = "grey93")
for (k in seq_along(gvar)) {
  lines(rg$year, rg[[gvar[k]]], lwd = 2.6, col = gcol[k])
  points(rg$year, rg[[gvar[k]]], pch = 19, cex = 0.8, col = gcol[k])
  text(max(rg$year), rg[[gvar[k]]][rg$year == max(rg$year)],
       paste0(" ", gnm[k], " ", pc(rg[[gvar[k]]][rg$year == max(rg$year)])),
       pos = 4, cex = 0.74, col = gcol[k], xpd = NA)
}

## ---- slope-d3
ser <- paste(mapply(function(nm, vr, cl)
  sprintf('{"n":"%s","c":"%s","p":[%s]}', nm, cl,
          paste(sprintf("[%d,%.1f]", rg$year, rg[[vr]]), collapse = ",")),
  gnm, gvar, gcol), collapse = ",")
cat(sprintf('
<div id="slp" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=400,M={t:20,r:186,b:44,l:58};
const box=d3.select("#slp");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const yrs=D[0].p.map(d=>d[0]);
const x=d3.scalePoint().domain(yrs).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([40,76]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("reported turnout among citizens");
yrs.forEach(yr=>{svg.append("line").attr("x1",x(yr)).attr("x2",x(yr))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#eee");});
const ln=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach(s=>{
  svg.append("path").attr("d",ln(s.p)).attr("fill","none").attr("stroke",s.c)
    .attr("stroke-width",2.6);
  svg.selectAll(null).data(s.p).join("circle")
    .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",4.5).attr("fill",s.c)
    .on("mousemove",function(e,d){tip.style("opacity",1).html(
       `<b>${s.n}</b><br>${d[0]}: ${d[1].toFixed(1)}%% reported turnout`)
       .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0));
  const last=s.p[s.p.length-1];
  svg.append("text").attr("x",x(last[0])+10).attr("y",y(last[1])+4)
    .attr("font-size","11.5px").attr("fill",s.c)
    .text(s.n+" "+last[1].toFixed(1)+"%%");});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Five elections, four groups, as the Bureau reports them. The spread between the
highest and lowest group was %.1f points in %d and %.1f points in %d, and every
line here inherits every caveat above.</p>
', ser, rg_sp(min(rg$year)), min(rg$year), rg_sp(max(rg$year)), max(rg$year)))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
