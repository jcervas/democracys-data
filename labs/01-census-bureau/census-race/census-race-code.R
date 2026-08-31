# census-race-code.R -- chunk bodies for census-race-brief.Rmd
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

d <- read.csv("data/derived/pl94171_counties.csv", stringsAsFactors = FALSE,
              colClasses = c(fips = "character"))

st <- aggregate(cbind(total, white, nh_white, hispanic, other_race, nh_other,
                      two_or_more, nh_two) ~ state, data = d, FUN = sum)
st$white_pct   <- round(100 * st$white       / st$total, 1)
st$nhwhite_pct <- round(100 * st$nh_white    / st$total, 1)
st$gap         <- round(st$white_pct - st$nhwhite_pct, 1)
st$hisp_pct    <- round(100 * st$hispanic    / st$total, 1)
st$other_pct   <- round(100 * st$other_race  / st$total, 1)
st$two_pct     <- round(100 * st$two_or_more / st$total, 1)
st$ratio       <- round(st$white_pct / st$nhwhite_pct, 2)
# how Hispanic is each of the two residual categories, state by state?
st$other_h     <- 100 * (st$other_race  - st$nh_other) / st$other_race
st$two_h       <- 100 * (st$two_or_more - st$nh_two)   / st$two_or_more
S <- function(s, v) st[[v]][st$state == s]

tot_other  <- sum(d$other_race); nh_other <- sum(d$nh_other)
hisp_other <- tot_other - nh_other
pct_other_h <- 100 * hisp_other / tot_other

tot_two  <- sum(d$two_or_more); nh_two <- sum(d$nh_two)
hisp_two <- tot_two - nh_two
pct_two_h <- 100 * hisp_two / tot_two

d$hisp_pct  <- 100 * d$hispanic    / d$total
d$two_pct   <- 100 * d$two_or_more / d$total
d$other_pct <- 100 * d$other_race  / d$total
d$corr_pct  <- 100 * d$gq_correctional / d$total
d$coll_pct  <- 100 * d$gq_college      / d$total
cor_two   <- cor(d$hisp_pct, d$two_pct)
cor_other <- cor(d$hisp_pct, d$other_pct)
cor_gap   <- cor(st$hisp_pct, st$gap)

top_two <- head(d[order(-d$two_pct), ], 10)
hi_two  <- d[d$state == "Hawaii", ]
hi_top  <- hi_two[which.max(hi_two$two_pct), ]
forest  <- d[order(-d$corr_pct), ][1, ]
alleg   <- d[d$fips == "42003", ]

p1ok <- all(d$one_race + d$two_or_more == d$total)
p2ok <- all(d$hispanic + d$not_hispanic == d$total)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# ---- inputs the figures share, so both output paths draw the same thing ----

# (a) the alluvial: every race answer, split by the ethnicity answer
rc <- data.frame(
  race = c("White alone", "Black alone", "American Indian alone", "Asian alone",
           "Pacific Islander alone", "Some Other Race alone", "Two or more races"),
  tot  = c(sum(d$white), sum(d$black), sum(d$aian), sum(d$asian), sum(d$nhpi),
           sum(d$other_race), sum(d$two_or_more)),
  nonh = c(sum(d$nh_white), sum(d$nh_black), sum(d$nh_aian), sum(d$nh_asian),
           sum(d$nh_nhpi), sum(d$nh_other), sum(d$nh_two)),
  stringsAsFactors = FALSE)
rc$hisp  <- rc$tot - rc$nonh
rc$share <- 100 * rc$hisp / rc$tot
rc <- rc[order(-rc$share), ]
rc$fill <- ifelse(rc$race == "Some Other Race alone", "#C41230",
           ifelse(rc$race == "Two or more races", "#e08214", "#999999"))
gap  <- 0.012 * sum(rc$tot)
rc$y0 <- head(cumsum(c(0, rc$tot + gap)), -1); rc$y1 <- rc$y0 + rc$tot
hgt  <- max(rc$y1)
hsum <- sum(rc$hisp); nsum <- sum(rc$nonh)
rgap <- (hgt - hsum - nsum) / 2
rt   <- data.frame(name = c("Hispanic or Latino", "Not Hispanic or Latino"),
                   tot  = c(hsum, nsum),
                   y0   = c(0, hsum + rgap),
                   fill = c("#8856a7", "#999999"), stringsAsFactors = FALSE)
rt$y1 <- rt$y0 + rt$tot
rc$hy <- head(cumsum(c(rt$y0[1], rc$hisp)), -1)
rc$ny <- head(cumsum(c(rt$y0[2], rc$nonh)), -1)

# (b) county outlines for Texas and New Mexico, from the maps package
cmp  <- maps::map("county", regions = c("texas", "new mexico"), plot = FALSE,
                  fill = TRUE)
cbk  <- c(0L, which(is.na(cmp$x)), length(cmp$x) + 1L)
cfp  <- maps::county.fips
cfp$stem <- sub(":.*", "", cfp$polyname)
cfp$f5   <- sprintf("%05d", cfp$fips)
pfips <- cfp$f5[match(sub(":.*", "", cmp$names), cfp$stem)]
pval  <- d$two_pct[match(pfips, d$fips)]
cutp  <- c(-1, 5, 10, 20, 30, 100)
cpal  <- c("#f0f0f0", "#c6dbef", "#7fb2d8", "#2c7fb8", "#123f5c")
pcol  <- cpal[as.integer(cut(pval, cutp))]
pcol[is.na(pcol)] <- "#ffffff"
ptop  <- pfips %in% top_two$fips
poly_i <- function(i) (cbk[i] + 1L):(cbk[i + 1L] - 1L)

# (c) who is warehoused where: the two group-quarters categories, back to back
gqn <- unique(rbind(head(d[order(-d$corr_pct), ], 6),
                    head(d[order(-d$coll_pct), ], 6)))
gqn <- gqn[order(gqn$coll_pct - gqn$corr_pct), ]

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-row
o <- alleg[, c("county", "state", "total", "white", "black", "asian",
               "two_or_more", "hispanic")]
for (k in c("total","white","black","asian","two_or_more","hispanic"))
  o[[k]] <- n(o[[k]])
names(o) <- c("county", "state", "total", "white", "Black", "Asian",
              "two or more races", "Hispanic")
o

## ---- file
data.frame(
  item = c("Source", "Reference date", "Counties", "States", "People covered",
           "Share of the U.S. population", "Tables kept"),
  value = c("2020 Census Redistricting Data (P.L. 94-171) Summary File",
            "1 April 2020", nrow(d), length(unique(d$state)),
            n(sum(d$total)),
            paste0(pc(100 * sum(d$total) / 331449281, 0), "%"),
            "P1 (race), P2 (Hispanic origin by race), P5 (group quarters)"))

## ---- checks
data.frame(
  test = c("one race + two or more races = total, in every county",
           "Hispanic + not Hispanic = total, in every county"),
  holds = c(ifelse(p1ok, "TRUE", "FALSE"), ifelse(p2ok, "TRUE", "FALSE")),
  counties_tested = c(nrow(d), nrow(d)))

## ---- pl-geo
fold <- function(s, w1 = 68, w2 = 64) {
  o <- character(0)
  while (nchar(s) > w1) { o <- c(o, substr(s, 1, w1))
                          s <- substring(s, w1 + 1); w1 <- w2 }
  c(o, s)
}
foldc <- function(s) {
  p <- fold(s)
  paste(c(p[1], if (length(p) > 1) paste0("    ", p[-1])), collapse = "\n")
}
SH  <- read.csv("data/raw/source-shape.csv", stringsAsFactors = FALSE)
sh  <- function(k) SH$value[SH$name == k]
GEO <- readLines("data/raw/pageo2020-allegheny.txt", warn = FALSE)[1]
S1  <- readLines("data/raw/pa000012020-allegheny.txt", warn = FALSE)[1]
S3  <- readLines("data/raw/pa000032020-allegheny.txt", warn = FALSE)[1]
HI  <- readLines("data/raw/higeo2020-counties.txt", warn = FALSE)
sp  <- function(x) strsplit(x, "|", fixed = TRUE)[[1]]
G <- sp(GEO); A <- sp(S1); B <- sp(S3)
NG <- length(G); NA1 <- length(A); NB <- length(B)

# Only the fields that carry a value: the empty ones are counted rather than
# listed, because "most of this record is empty" is the observation, and 65
# blank rows would bury it rather than make it.
GNE <- which(nzchar(G))
mean_g <- rep("—", NG)
mean_g[3]  <- "summary level — 050 means county"
mean_g[8]  <- "logical record number — the join key"
mean_g[10] <- "FIPS code: 42 Pennsylvania, 003 Allegheny"
mean_g[88] <- "the county's name"
data.frame(
  Field = c(as.character(GNE), "the other 65"),
  Value = c(G[GNE], "(empty)"),
  What_it_means = c(mean_g[GNE],
                    "kinds of geography a county is not"))

## ---- pl-seg1
# All 149 fields carry a number, so listing every one would be a wall. The
# first five are the labels and the sixth is the one the chapter uses; the
# remainder are summarised by the two tables they belong to, which is the
# structure the file itself never states.
data.frame(
  Field = c("1", "2", "3", "4", "5", "6", "7–76", "77–149"),
  Value = c(A[1:6], "counts", "counts"),
  What_it_means = c("file identifier", "state abbreviation",
                    "characteristic iteration", "file sequence number",
                    "logical record number — the join key",
                    "total population of Allegheny County",
                    "the rest of Table P1", "Table P2"))

## ---- pl-fields
KP1 <- 5 + (1:9)                       # P1 fields 1-9:  total .. two or more
KP2 <- 76 + c(2, 3, 5:11)              # P2 fields 2,3,5-11
KEEP <- c(KP1, KP2)
grp <- rep("ignored", NA1)
grp[1:5] <- "label"; grp[KEEP] <- "kept"
col <- c(label = "#B0B0B0", ignored = "#E8E8E8", kept = "#54278F")
par(mar = c(2.4, 0.6, 1.4, 0.6))
plot(NA, xlim = c(0.5, NA1 + 0.5), ylim = c(0, 1), axes = FALSE,
     xlab = "", ylab = "", xaxs = "i")
rect(seq_len(NA1) - 0.46, 0.34, seq_len(NA1) + 0.46, 0.78,
     col = col[grp], border = NA)
seg <- function(a, b, lab, ctr = TRUE) {
  segments(a - 0.46, 0.28, b + 0.46, 0.28, lwd = 1.1, col = "grey35")
  segments(c(a - 0.46, b + 0.46), 0.28, c(a - 0.46, b + 0.46), 0.24,
           lwd = 1.1, col = "grey35")
  text(if (ctr) (a + b) / 2 else a - 0.46, 0.15, lab, cex = 0.66,
       col = "grey25", adj = c(if (ctr) 0.5 else 0, 0.5), xpd = NA)
}
seg(6, 76, "Table P1: race, 71 fields")
seg(77, NA1, "Table P2: Hispanic origin by race, 73 fields")
text(1, 0.86, "1", cex = 0.6, col = "grey45", adj = c(0, 0.5), xpd = NA)
text(76, 0.86, "76", cex = 0.6, col = "grey45", xpd = NA)
text(NA1, 0.86, NA1, cex = 0.6, col = "grey45", adj = c(1, 0.5), xpd = NA)
legend("top", horiz = TRUE, bty = "n", cex = 0.72, pt.cex = 1.4, pch = 15,
       col = col[c("label", "kept", "ignored")],
       legend = c("5 labels", paste(length(KEEP), "kept"),
                  paste(NA1 - 5 - length(KEEP), "ignored")))

## ---- pl-seg3
# Only fifteen fields wide, so this one fits whole.
mean_b <- rep("—", NB)
mean_b[1] <- "file identifier"
mean_b[2] <- "state abbreviation"
mean_b[5] <- "logical record number — joins back to the two files above"
data.frame(
  Field = seq_len(NB),
  Value = ifelse(nzchar(B), B, "(empty)"),
  What_it_means = mean_b)

## ---- pl-clean
src <- c(fips = "geo, field 10", county = "Gazetteer, joined on fips",
         state = "the archive it came out of",
         total = "seg 1, field 6", one_race = "seg 1, field 7",
         white = "seg 1, field 8", black = "seg 1, field 9",
         aian = "seg 1, field 10", asian = "seg 1, field 11",
         nhpi = "seg 1, field 12", other_race = "seg 1, field 13",
         two_or_more = "seg 1, field 14",
         hispanic = "seg 1, field 78", not_hispanic = "seg 1, field 79",
         nh_white = "seg 1, field 81", nh_black = "seg 1, field 82",
         nh_aian = "seg 1, field 83", nh_asian = "seg 1, field 84",
         nh_nhpi = "seg 1, field 85", nh_other = "seg 1, field 86",
         nh_two = "seg 1, field 87",
         gq_total = "seg 3, field 6", gq_correctional = "seg 3, field 8",
         gq_juvenile = "seg 3, field 9", gq_nursing = "seg 3, field 10",
         gq_college = "seg 3, field 13", gq_military = "seg 3, field 14")
al <- d[d$fips == "42003", ]
stopifnot(nrow(al) == 1, all(names(src) %in% names(al)))
stopifnot(setequal(names(src),
                   names(read.csv("data/derived/pl94171_counties.csv", nrows = 1))))

# every claim in the third column, checked against the captured lines
chk <- names(src)[grepl("^seg ", src)]
pos <- as.integer(sub(".*field ", "", src[chk]))
got <- ifelse(grepl("^seg 1", src[chk]), A[pos], B[pos])
stopifnot(all(as.numeric(got) == as.numeric(al[chk])))
stopifnot(al$fips == G[10], al$county == G[88])

o <- data.frame(column = names(src),
                value = vapply(names(src), function(k)
                  format(al[[k]], big.mark = ",", trim = TRUE), character(1)),
                `where it came from` = unname(src), check.names = FALSE)
rownames(o) <- NULL
o

## ---- two-answers
o <- st[order(-st$gap), c("state", "total", "white_pct", "nhwhite_pct", "gap")]
o$total <- n(o$total)
names(o) <- c("state", "total population", "% white (race question)",
              "% non-Hispanic white (both questions)", "gap")
o

## ---- two-static
s <- st[order(st$white_pct), ]
par(mar = c(4.6, 9.5, 1.4, 2))
barplot(rbind(s$nhwhite_pct, s$white_pct - s$nhwhite_pct), horiz = TRUE,
        las = 1, names.arg = s$state, col = c("#2166AC", "#92C5DE"),
        border = NA, xlim = c(0, 80), cex.names = 0.9, xlab = "% white")
legend("bottomright", c("non-Hispanic white", "Hispanic and white"),
       fill = c("#2166AC", "#92C5DE"), border = NA, bty = "n", cex = 0.85)

## ---- two-d3
s <- st[order(st$white_pct), ]
rows <- paste(sprintf('{"s":"%s","nh":%.1f,"gap":%.1f,"w":%.1f}',
                      s$state, s$nhwhite_pct, s$white_pct - s$nhwhite_pct,
                      s$white_pct), collapse = ",")
cat(sprintf('
<div id="two" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,H=310,M={t:16,r:70,b:44,l:104};
const svg=d3.select("#two").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,80]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([H-M.b,M.t]).padding(0.26);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("rect").attr("x",x(0)).attr("y",d=>y(d.s)).attr("height",y.bandwidth())
  .attr("fill","#2166AC").attr("width",0)
  .transition().duration(650).attr("width",d=>x(d.nh)-x(0));
g.append("rect").attr("y",d=>y(d.s)).attr("height",y.bandwidth())
  .attr("fill","#92C5DE").attr("x",d=>x(d.nh)).attr("width",0)
  .transition().delay(650).duration(650).attr("width",d=>x(d.gap)-x(0));
g.append("text").attr("x",d=>x(d.w)+7).attr("y",d=>y(d.s)+y.bandwidth()/2+4)
  .attr("font-size","11.5px").attr("fill","#333").attr("opacity",0)
  .text(d=>d.w.toFixed(1)+"%%")
  .transition().delay(1300).duration(300).attr("opacity",1);
const lg=svg.append("g").attr("transform",`translate(${W-M.r-250},${M.t+2})`);
[["#2166AC","non-Hispanic white"],["#92C5DE","Hispanic and white"]].forEach((r,i)=>{
  lg.append("rect").attr("y",i*17).attr("width",11).attr("height",11).attr("fill",r[0]);
  lg.append("text").attr("x",16).attr("y",i*17+10).attr("font-size","11.5px").text(r[1]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The dark bar answers both questions. The whole bar answers only the race
question. The light segment is the seam between them.</p>
', rows))

## ---- other
o <- st[order(-st$other_pct), c("state", "other_pct", "hisp_pct")]
names(o) <- c("state", "% choosing 'Some Other Race'", "% Hispanic")
o

## ---- other-who
data.frame(
  group = c("Chose 'Some Other Race'", "  of whom Hispanic",
            "  of whom not Hispanic"),
  people = c(n(tot_other), n(hisp_other), n(nh_other)),
  share = c("", paste0(pc(pct_other_h), "%"),
            paste0(pc(100 - pct_other_h), "%")))

## ---- two-race-who
data.frame(
  group = c("Reported two or more races", "  of whom Hispanic",
            "  of whom not Hispanic"),
  people = c(n(tot_two), n(hisp_two), n(nh_two)),
  share = c("", paste0(pc(pct_two_h), "%"), paste0(pc(100 - pct_two_h), "%")))

## ---- sankey-static
NW <- 0.03
rib <- function(ya0, ya1, yb0, yb1, col) {
  t <- seq(0, 1, length.out = 60); s <- (1 - cos(pi * t)) / 2
  xs <- NW + (1 - 2 * NW) * t
  polygon(c(xs, rev(xs)), c(ya0 + (yb0 - ya0) * s, rev(ya1 + (yb1 - ya1) * s)),
          col = col, border = NA)
}
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(-0.33, 1.30), ylim = c(hgt, 0), axes = FALSE, xlab = "",
     ylab = "", yaxs = "i")
for (i in seq_len(nrow(rc))) {
  a <- paste0(rc$fill[i], if (rc$fill[i] == "#999999") "55" else "99")
  rib(rc$y0[i], rc$y0[i] + rc$hisp[i], rc$hy[i], rc$hy[i] + rc$hisp[i], a)
  rib(rc$y0[i] + rc$hisp[i], rc$y1[i], rc$ny[i], rc$ny[i] + rc$nonh[i],
      paste0(rc$fill[i], "33"))
}
rect(0, rc$y0, NW, rc$y1, col = rc$fill, border = NA)
rect(1 - NW, rt$y0, 1, rt$y1, col = rt$fill, border = NA)
text(-0.012, (rc$y0 + rc$y1) / 2, rc$race, pos = 2, cex = 0.72)
text(-0.012, (rc$y0 + rc$y1) / 2 + 0.022 * hgt,
     paste0(pc(rc$share), "% Hispanic"), pos = 2, cex = 0.62, col = "#666666")
text(1.012, (rt$y0 + rt$y1) / 2, rt$name, pos = 4, cex = 0.78, font = 2)
text(1.012, (rt$y0 + rt$y1) / 2 + 0.022 * hgt, n(rt$tot), pos = 4, cex = 0.66,
     col = "#666666")

## ---- sankey-d3
lr <- paste(sprintf(
  '{"r":"%s","t":%d,"h":%d,"nn":%d,"s":%.1f,"y0":%.0f,"y1":%.0f,"hy":%.0f,"ny":%.0f,"c":"%s"}',
  rc$race, rc$tot, rc$hisp, rc$nonh, rc$share, rc$y0, rc$y1, rc$hy, rc$ny,
  rc$fill), collapse = ",")
rr <- paste(sprintf('{"n":"%s","t":%d,"y0":%.0f,"y1":%.0f,"c":"%s"}',
                    rt$name, rt$tot, rt$y0, rt$y1, rt$fill), collapse = ",")
cat(sprintf('
<div id="snk" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const L=[%s],R=[%s],HG=%.0f;
const W=760,H=470,M={t:10,r:210,b:10,l:200},NW=13;
const svg=d3.select("#snk").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,HG]).range([M.t,H-M.b]);
const xa=M.l,xb=W-M.r,xm=(xa+xb)/2;
const band=(a0,a1,b0,b1)=>
  `M${xa+NW},${y(a0)} C${xm},${y(a0)} ${xm},${y(b0)} ${xb},${y(b0)}`+
  ` L${xb},${y(b1)} C${xm},${y(b1)} ${xm},${y(a1)} ${xa+NW},${y(a1)} Z`;
const fmt=d3.format(",");
const cap=d3.select("#snk").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const base="<b>Hover a band.</b> The top two ribbons carry almost all of their people into the Hispanic node \\u2014 %s%% and %s%% of them.";
const g=svg.append("g");
L.forEach(d=>{
  const dim=d.c==="#999999";
  g.append("path").attr("d",band(d.y0,d.y0+d.h,d.hy,d.hy+d.h))
    .attr("fill",d.c).attr("fill-opacity",dim?0.32:0.6).style("cursor","pointer")
    .on("mousemove",()=>cap.html("<b>"+d.r+" \\u2192 Hispanic:</b> "+fmt(d.h)+
      " people, "+d.s.toFixed(1)+"%% of everyone who chose "+d.r+"."))
    .on("mouseleave",()=>cap.html(base));
  g.append("path").attr("d",band(d.y0+d.h,d.y1,d.ny,d.ny+d.nn))
    .attr("fill",d.c).attr("fill-opacity",dim?0.14:0.22).style("cursor","pointer")
    .on("mousemove",()=>cap.html("<b>"+d.r+" \\u2192 not Hispanic:</b> "+fmt(d.nn)+
      " people, "+(100-d.s).toFixed(1)+"%% of everyone who chose "+d.r+"."))
    .on("mouseleave",()=>cap.html(base));});
svg.append("g").selectAll("rect").data(L).join("rect")
  .attr("x",xa).attr("y",d=>y(d.y0)).attr("width",NW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.c);
svg.append("g").selectAll("rect").data(R).join("rect")
  .attr("x",xb-NW).attr("y",d=>y(d.y0)).attr("width",NW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.c);
const lt=svg.append("g").selectAll("g").data(L).join("g");
lt.append("text").attr("x",xa-8).attr("y",d=>(y(d.y0)+y(d.y1))/2-1)
  .attr("text-anchor","end").attr("font-size","12px").text(d=>d.r);
lt.append("text").attr("x",xa-8).attr("y",d=>(y(d.y0)+y(d.y1))/2+12)
  .attr("text-anchor","end").attr("font-size","10.5px").attr("fill","#777")
  .text(d=>d.s.toFixed(1)+"%% Hispanic");
const rtg=svg.append("g").selectAll("g").data(R).join("g");
rtg.append("text").attr("x",xb+9).attr("y",d=>(y(d.y0)+y(d.y1))/2-1)
  .attr("font-size","12.5px").attr("font-weight","600").text(d=>d.n);
rtg.append("text").attr("x",xb+9).attr("y",d=>(y(d.y0)+y(d.y1))/2+13)
  .attr("font-size","10.5px").attr("fill","#777").text(d=>fmt(d.t)+" people");
cap.html(base);
})();
</script>
', lr, rr, hgt, pc(rc$share[1]), pc(rc$share[2])))

## ---- hatch-static
hh <- st[order(st$hisp_pct), ]
yy <- seq_len(nrow(hh))
par(mar = c(4.2, 8.2, 1, 1.4))
plot(NA, xlim = c(0, 100), ylim = c(0.6, 7.5), yaxt = "n", bty = "n",
     xlab = "% Hispanic", ylab = "", las = 1)
abline(v = seq(0, 100, 20), col = "#eeeeee")
segments(hh$hisp_pct, yy, pmax(hh$other_h, hh$two_h), yy, col = "#cccccc",
         lwd = 6)
points(hh$hisp_pct, yy, pch = 22, bg = "white", col = "#555555", cex = 1.25,
       lwd = 1.4)
points(hh$two_h,   yy, pch = 17, col = "#2c7fb8", cex = 1.15)
points(hh$other_h, yy, pch = 19, col = "#C41230", cex = 1.15)
axis(2, at = yy, labels = hh$state, las = 1, tick = FALSE, cex.axis = 0.9)
legend(x = 0, y = 7.7,
       c("share of the state that is Hispanic",
         "of those choosing 'Some Other Race', % Hispanic",
         "of those reporting two or more races, % Hispanic"),
       pch = c(22, 19, 17), col = c("#555555", "#C41230", "#2c7fb8"),
       pt.bg = "white", bty = "n", cex = 0.72)

## ---- hatch-d3
hh <- st[order(-st$hisp_pct), ]
rows <- paste(sprintf('{"s":"%s","h":%.1f,"o":%.1f,"t":%.1f}',
                      hh$state, hh$hisp_pct, hh$other_h, hh$two_h),
              collapse = ",")
cat(sprintf('
<div id="htch" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=330,M={t:66,r:26,b:44,l:112};
const box=d3.select("#htch");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([M.t,H-M.b]).padding(0.5);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).call(g=>g.select(".domain").remove());
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("%% Hispanic");
const cy=d=>y(d.s)+y.bandwidth()/2;
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",d=>x(d.h)).attr("x2",d=>x(Math.max(d.o,d.t)))
  .attr("y1",cy).attr("y2",cy).attr("stroke","#d2d2d2").attr("stroke-width",7);
const sym=(t,s)=>d3.symbol().type(t).size(s)();
svg.append("g").selectAll("path").data(D).join("path")
  .attr("d",sym(d3.symbolSquare,66))
  .attr("transform",d=>`translate(${x(d.h)},${cy(d)})`)
  .attr("fill","#fff").attr("stroke","#555").attr("stroke-width",1.4);
svg.append("g").selectAll("path").data(D).join("path")
  .attr("d",sym(d3.symbolTriangle,62))
  .attr("transform",d=>`translate(${x(d.t)},${cy(d)})`).attr("fill","#2c7fb8");
svg.append("g").selectAll("path").data(D).join("path")
  .attr("d",sym(d3.symbolCircle,60))
  .attr("transform",d=>`translate(${x(d.o)},${cy(d)})`).attr("fill","#C41230");
const key=[["#fff","#555",d3.symbolSquare,"share of the state that is Hispanic"],
 ["#C41230","#C41230",d3.symbolCircle,"of those choosing \\u2018Some Other Race\\u2019, %% Hispanic"],
 ["#2c7fb8","#2c7fb8",d3.symbolTriangle,"of those reporting two or more races, %% Hispanic"]];
const lg=svg.append("g").attr("transform",`translate(${M.l},14)`);
key.forEach((k,i)=>{
  lg.append("path").attr("d",sym(k[2],62)).attr("transform",`translate(6,${i*16})`)
    .attr("fill",k[0]).attr("stroke",k[1]).attr("stroke-width",1.3);
  lg.append("text").attr("x",18).attr("y",i*16+4).attr("font-size","11.5px")
    .attr("fill","#333").text(k[3]);});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",M.l).attr("y",d=>y(d.s)).attr("width",W-M.r-M.l)
  .attr("height",y.bandwidth()).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.s}</b><br>${d.h.toFixed(1)}%% Hispanic<br>`+
      `Some Other Race: ${d.o.toFixed(1)}%% Hispanic<br>`+
      `two or more races: ${d.t.toFixed(1)}%% Hispanic`)
      .style("left",Math.min(ev.offsetX+14,W-280)+"px").style("top",(ev.offsetY-8)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The square is the state. The two markers to its right are the two residual
categories. In %s, %s%% Hispanic overall, %s%% of the people who chose
“Some Other Race” are Hispanic.</p>
', rows, st$state[which.min(st$hisp_pct)],
   pc(min(st$hisp_pct)),
   pc(st$other_h[which.min(st$hisp_pct)])))

## ---- top-two
o <- top_two[, c("county", "state", "total", "two_pct", "hisp_pct")]
o$total <- n(o$total); o$two_pct <- pc(o$two_pct); o$hisp_pct <- pc(o$hisp_pct)
names(o) <- c("county", "state", "population", "% two or more races",
              "% Hispanic")
o

## ---- map-static
xr <- range(cmp$x, na.rm = TRUE); yr <- range(cmp$y, na.rm = TRUE)
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = xr, ylim = yr, asp = 1 / cos(32 * pi / 180), axes = FALSE,
     xlab = "", ylab = "")
for (i in seq_along(cmp$names)) {
  k <- poly_i(i); polygon(cmp$x[k], cmp$y[k], col = pcol[i], border = "white",
                          lwd = 0.25)
}
for (i in which(ptop)) {
  k <- poly_i(i); polygon(cmp$x[k], cmp$y[k], col = NA, border = "#C41230",
                          lwd = 1.3)
}
lx <- xr[1] + 0.02 * diff(xr); ly <- yr[1] + 0.05 * diff(yr)
bw <- 0.055 * diff(xr); bh <- 0.022 * diff(yr)
rect(lx + (0:4) * bw, ly, lx + (1:5) * bw, ly + bh, col = cpal, border = "white")
text(lx + (0:5) * bw, ly - 0.012 * diff(yr),
     c("0", "5", "10", "20", "30", "50%"), cex = 0.6, col = "#555555")
text(lx, ly + bh + 0.016 * diff(yr), "% reporting two or more races", pos = 4,
     offset = -0.2, cex = 0.68)
points(lx + 0.3 * bw, ly - 0.055 * diff(yr), pch = 0, col = "#C41230", cex = 1.1)
text(lx + 0.7 * bw, ly - 0.055 * diff(yr),
     paste("the", nrow(top_two), "counties in the table above"), pos = 4,
     offset = 0, cex = 0.68, col = "#C41230")

## ---- map-d3
pj <- vapply(seq_along(cmp$names), function(i) {
  k <- poly_i(i)
  paste0("[", paste(sprintf('[%.2f,%.2f]', cmp$x[k], cmp$y[k]), collapse = ","),
         "]") }, "")
mn <- d$county[match(pfips, d$fips)]
rows <- paste(sprintf('{"p":%s,"c":"%s","t":%d,"n":"%s","v":%.1f,"h":%.1f}',
                      pj, pcol, as.integer(ptop), mn, pval,
                      d$hisp_pct[match(pfips, d$fips)]), collapse = ",")
cat(sprintf('
<div id="cmap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=560,M=14;
const svg=d3.select("#cmap").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const xs=d3.extent(D.flatMap(d=>d.p.map(q=>q[0])));
const ys=d3.extent(D.flatMap(d=>d.p.map(q=>q[1])));
const k=Math.min((W-2*M)/((xs[1]-xs[0])),(H-2*M)/((ys[1]-ys[0])*1.18));
const X=v=>M+(v-xs[0])*k, Y=v=>H-M-(v-ys[0])*k*1.18;
const line=d3.line().x(q=>X(q[0])).y(q=>Y(q[1]));
const cap=d3.select("#cmap").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const base="<b>Hover a county.</b> The red outlines are the %d counties in the table above. They are not scattered: they sit on the Rio Grande.";
svg.append("g").selectAll("path").data(D).join("path")
  .attr("d",d=>line(d.p)).attr("fill",d=>d.c).attr("stroke","#fff")
  .attr("stroke-width",0.4).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.n+"</b> \\u2014 "+d.v.toFixed(1)+
    "%% report two or more races; "+d.h.toFixed(1)+"%% Hispanic."))
  .on("mouseleave",()=>cap.html(base));
svg.append("g").selectAll("path").data(D.filter(d=>d.t)).join("path")
  .attr("d",d=>line(d.p)).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",1.6).attr("pointer-events","none");
const pal=[%s],brk=["0","5","10","20","30","50%%"];
const lg=svg.append("g").attr("transform",`translate(${M+6},${H-58})`);
lg.append("text").attr("y",-8).attr("font-size","11.5px").attr("fill","#444")
  .text("%% reporting two or more races");
pal.forEach((c,i)=>{lg.append("rect").attr("x",i*34).attr("width",34)
  .attr("height",12).attr("fill",c).attr("stroke","#fff");});
brk.forEach((b,i)=>{lg.append("text").attr("x",i*34).attr("y",25)
  .attr("text-anchor","middle").attr("font-size","10px").attr("fill","#666").text(b);});
cap.html(base);
})();
</script>
', rows, nrow(top_two), paste(sprintf('"%s"', cpal), collapse = ",")))

## ---- scatter-static
plot(d$hisp_pct, d$two_pct, pch = 19, cex = 0.6,
     col = ifelse(d$state == "Hawaii", "#C41230", "#2166AC66"),
     xlab = "% of the county that is Hispanic",
     ylab = "% reporting two or more races", xlim = c(0, 100), ylim = c(0, 50))
points(hi_two$hisp_pct, hi_two$two_pct, pch = 19, col = "#C41230", cex = 1.1)
legend("topleft", c("Hawaii counties", "all other counties"),
       col = c("#C41230", "#2166AC"), pch = 19, bty = "n", cex = 0.85)

## ---- scatter-d3
dd <- d[, c("county", "state", "hisp_pct", "two_pct", "total")]
rows <- paste(sprintf('{"c":"%s","s":"%s","h":%.1f,"t":%.1f,"p":%d}',
                      gsub('"', "", dd$county), dd$state, dd$hisp_pct,
                      dd$two_pct, dd$total), collapse = ",")
cat(sprintf('
<div id="sc" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=430,M={t:18,r:20,b:48,l:58};
const svg=d3.select("#sc").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,50]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(10).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("%% of the county that is Hispanic");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% reporting two or more races");
const tip=d3.select("#sc").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.h)).attr("cy",d=>y(d.t))
  .attr("r",d=>d.s==="Hawaii"?6:3.2)
  .attr("fill",d=>d.s==="Hawaii"?"#C41230":"#2166AC")
  .attr("fill-opacity",d=>d.s==="Hawaii"?0.95:0.4)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(
      `<b>${d.c}, ${d.s}</b><br>${d3.format(",")(d.p)} people<br>`+
      `Hispanic ${d.h.toFixed(1)}%% &middot; two or more races ${d.t.toFixed(1)}%%`)
      .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
const lg=svg.append("g").attr("transform",`translate(${M.l+14},${M.t+4})`);
[["#C41230","Hawaii counties"],["#2166AC","all other counties"]].forEach((r,i)=>{
  lg.append("circle").attr("cy",i*17).attr("r",5).attr("fill",r[0]);
  lg.append("text").attr("x",11).attr("y",i*17+4).attr("font-size","12px").text(r[1]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One dot per county. Hawaii is highlighted because it is the state most people
name when asked where multiracial America is. Hover for the county.</p>
', rows))

## ---- prisons
o <- head(d[order(-d$corr_pct), c("county", "state", "total",
                                  "gq_correctional", "corr_pct")], 6)
o$total <- n(o$total); o$gq_correctional <- n(o$gq_correctional)
o$corr_pct <- pc(o$corr_pct)
names(o) <- c("county", "state", "population", "in a correctional facility",
              "% of the county")
o

## ---- college
o <- head(d[order(-d$coll_pct), c("county", "state", "total", "gq_college",
                                  "coll_pct")], 4)
o$total <- n(o$total); o$gq_college <- n(o$gq_college); o$coll_pct <- pc(o$coll_pct)
names(o) <- c("county", "state", "population", "in student housing",
              "% of the county")
o

## ---- gq-static
lim <- max(gqn$corr_pct, gqn$coll_pct) * 1.12
yy  <- seq_len(nrow(gqn))
par(mar = c(4.2, 10.5, 1, 1.4))
plot(NA, xlim = c(-lim, lim), ylim = c(0.4, nrow(gqn) + 0.6), yaxt = "n",
     xaxt = "n", bty = "n", ylab = "", xlab = "% of the county's population")
at <- pretty(c(0, lim), 5); at <- at[at <= lim]
axis(1, at = c(-rev(at), at[-1]), labels = c(rev(at), at[-1]))
abline(v = 0, col = "#666666")
rect(-gqn$corr_pct, yy - 0.34, 0, yy + 0.34, col = "#C41230", border = NA)
rect(0, yy - 0.34, gqn$coll_pct, yy + 0.34, col = "#2c7fb8", border = NA)
axis(2, at = yy, labels = paste0(sub(" County", "", gqn$county), ", ",
                                 state.abb[match(gqn$state, state.name)]),
     las = 1, tick = FALSE, cex.axis = 0.78, line = -0.4)
mtext("in a correctional facility", side = 3, at = -lim / 2, line = -0.6,
      cex = 0.78, col = "#C41230")
mtext("in student housing", side = 3, at = lim / 2, line = -0.6, cex = 0.78,
      col = "#2c7fb8")

## ---- gq-d3
rows <- paste(sprintf('{"n":"%s","s":"%s","a":%.2f,"b":%.2f,"p":%d}',
                      sub(" County", "", gqn$county),
                      state.abb[match(gqn$state, state.name)],
                      gqn$corr_pct, gqn$coll_pct, gqn$total), collapse = ",")
cat(sprintf('
<div id="gq" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=420,M={t:34,r:26,b:44,l:150};
const svg=d3.select("#gq").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const lim=d3.max(D,d=>Math.max(d.a,d.b))*1.12;
const x=d3.scaleLinear().domain([-lim,lim]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.n+", "+d.s)).range([M.t,H-M.b]).padding(0.3);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>Math.abs(d)+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).call(g=>g.select(".domain").remove())
  .selectAll("text").attr("font-size","11px");
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t-6)
  .attr("y2",H-M.b).attr("stroke","#666");
const key=d=>d.n+", "+d.s;
const cap=d3.select("#gq").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const base="<b>Hover a county.</b> Left of the line, people who cannot vote where they are counted. Right of it, people who can.";
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("y",d=>y(key(d))).attr("height",y.bandwidth()).attr("fill","#C41230")
  .attr("x",x(0)).attr("width",0).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+key(d)+"</b> \\u2014 "+d.a.toFixed(1)+
    "%% of its "+d3.format(",")(d.p)+" people are in a correctional facility."))
  .on("mouseleave",()=>cap.html(base))
  .transition().duration(700).attr("x",d=>x(-d.a)).attr("width",d=>x(0)-x(-d.a));
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("y",d=>y(key(d))).attr("height",y.bandwidth()).attr("fill","#2c7fb8")
  .attr("x",x(0)).attr("width",0).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+key(d)+"</b> \\u2014 "+d.b.toFixed(1)+
    "%% of its "+d3.format(",")(d.p)+" people are in student housing."))
  .on("mouseleave",()=>cap.html(base))
  .transition().duration(700).attr("width",d=>x(d.b)-x(0));
[["#C41230","in a correctional facility",-lim/2],
 ["#2c7fb8","in student housing",lim/2]].forEach(r=>{
  svg.append("text").attr("x",x(r[2])).attr("y",M.t-14).attr("text-anchor","middle")
    .attr("font-size","12px").attr("font-weight","600").attr("fill",r[0]).text(r[1]);});
cap.html(base);
})();
</script>
', rows))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
