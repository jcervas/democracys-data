# surnames-code.R -- chunk bodies for surnames-brief.Rmd
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

# `sn` is the CURRENT surname table -- 2020 -- because that is what BISG runs
# on everywhere in this book. `sn10` is the 2010 tabulation it replaced, kept
# because two sections below are about what the 2010 file refused to publish,
# and that refusal is a fact about the 2010 file rather than about surnames.
sn   <- read.csv("data/derived/census_surnames.csv", stringsAsFactors = FALSE)
sn10 <- read.csv("data/derived/census_surnames_2010.csv", stringsAsFactors = FALSE)
s20  <- read.csv("data/derived/surnames_2020_top100.csv",  stringsAsFactors = FALSE)
s20n <- read.csv("data/derived/surnames_2020_negatives.csv", stringsAsFactors = FALSE)
f20  <- read.csv("data/derived/surnames_2020_facts.csv",   stringsAsFactors = FALSE)
F20  <- function(k) f20$value[f20$key == k]
co <- read.csv("data/derived/county_race.csv", stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))

grp  <- c("pctwhite", "pctblack", "pctapi", "pctaian", "pct2prace", "pcthispanic")
grp6 <- c("white", "black", "api", "aian", "twoplus", "hispanic")
pretty6 <- c("white", "Black", "Asian/PI", "Am. Indian", "two or more",
             "Hispanic")

# ---- one palette for this document ----------------------------------------
# Three different things get colored here, and each gets its own channel so
# that no color carries two meanings. Green and red in particular mean "white"
# and "Black" everywhere in this document and nothing else.
#   GCOL  a group. Categorical, six hues, fixed for the whole document.
#   ICOL  how informative a surname is. An ORDERED quantity, so it gets one
#         hue and varies only in darkness.
#   ACOL  what adding geography does. A before/after pair, in a hue used
#         nowhere else.
#   NCOL  a plain count with no group meaning at all.
GCOL <- c("#4d9221", "#C41230", "#2c7fb8", "#e08214", "#999999", "#8856a7")
names(GCOL) <- pretty6
ICOL <- c(settles = "#d9d9d9", narrows = "#969696", decisive = "#252525")
ACOL <- c(before = "#999999", after = "#00666e")
NCOL <- "#4D4D4D"

natl <- colSums(co[, grp6] * co$pop) / sum(co$pop)
look <- function(nm) sn[sn$name == toupper(nm), ]
bisg <- function(name, fips) {
  s <- look(name)
  p <- c(s$pctwhite, s$pctblack, s$pctapi, s$pctaian, s$pct2prace,
         s$pcthispanic) / 100
  p[is.na(p)] <- 0
  g <- as.numeric(co[co$fips == fips, grp6])
  u <- p * g / natl
  setNames(round(100 * u / sum(u), 1), grp6)
}

sn$top <- apply(sn[, grp], 1, function(x) max(x, na.rm = TRUE))
ok  <- is.finite(sn$top) & !is.na(sn$count)
pop_covered <- sum(sn$count[ok])
p_decisive  <- 100 * sum(sn$count[ok & sn$top > 90]) / pop_covered
n_useless   <- sum(sn$count[ok & sn$top < 60])
p_useless   <- 100 * n_useless / pop_covered

big  <- sn[sn$count > 1000, ]
n90  <- sapply(grp, function(v) sum(big[[v]] > 90, na.rm = TRUE))
names(n90) <- pretty6

# Suppression is a 2010 phenomenon: the 2020 file publishes every cell, having
# noised them instead. These five all describe the older table.
cells <- nrow(sn10) * length(grp)
miss  <- sum(is.na(sn10[, grp]))
rare  <- sn10[sn10$count < 200, ]
rare_black <- 100 * mean(is.na(rare$pctblack))
sn10$bucket <- cut(sn10$count, breaks = c(0, 200, 500, 1000, 5000, 1e7),
                   labels = c("under 200", "200-500", "500-1,000", "1,000-5,000",
                              "over 5,000"))
supp <- 100 * tapply(is.na(sn10$pctblack), sn10$bucket, mean)

gains <- t(sapply(c("SMITH","WILLIAMS","JOHNSON","LEE","NGUYEN","GARCIA"),
  function(nm) { s <- look(nm)
    b <- max(c(s$pctwhite, s$pctblack, s$pctapi, s$pcthispanic), na.rm = TRUE)
    a <- max(bisg(nm, "36005")); c(before = b, after = a, change = a - b) }))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
G  <- function(nm, v) look(nm)[[v]]

# ---- figure data ----------------------------------------------------------

# the thousand most common surnames, on the two shares they share
k1  <- sn[!is.na(sn$rank) & sn$rank >= 1 & sn$rank <= 1000, ]
k1$pctblack[is.na(k1$pctblack)] <- 0
k1_share  <- 100 * sum(k1$count) / pop_covered
k1_w90    <- sum(k1$pctwhite > 90, na.rm = TRUE)
k1_b90    <- sum(k1$pctblack > 90, na.rm = TRUE)
k1_maxb   <- k1[which.max(k1$pctblack), ]
mark      <- c("SMITH", "WILLIAMS", "JOHNSON", "WASHINGTON", "NGUYEN",
               "GARCIA", "LEE", "YODER")

# how the covered population divides by the best a surname can do
brk  <- seq(0, 100, by = 5)
hcut <- cut(sn$top[ok], breaks = brk, include.lowest = TRUE)
hpop <- as.numeric(tapply(sn$count[ok], hcut, sum)); hpop[is.na(hpop)] <- 0
hmid <- brk[-1] - 2.5
hmode <- hmid[which.max(hpop)]

# where each group's surname shares actually sit
dens <- lapply(grp, function(v) {
  x <- big[[v]][!is.na(big[[v]])]
  d <- density(x, from = 0, to = 100, bw = 2.2, n = 160)
  list(x = d$x, y = d$y / max(d$y), med = median(x))
})
names(dens) <- pretty6

# how many names sit in each frequency bucket
supp_n <- as.integer(table(sn10$bucket))


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
o <- look("SMITH")[, c("name", "rank", "count", grp)]
o$count <- n(o$count)
names(o) <- c("surname", "rank", "people holding it", pretty6)
o

## ---- rawsn
# RECONSTRUCTED, not captured -- see the paragraph above. The header case, the
# column names and the "(S)" marker are all read off the build script; the
# values shown are the ones this folder's clean copy carries for those names.
# The header is left upper case on purpose: its case is one of the two findings.
data.frame(
  Column_as_it_arrives = c("NAME", "RANK", "COUNT", "…", "PCTWHITE",
                           "PCTBLACK", "PCTAPI", "PCTAIAN", "PCT2PRACE",
                           "PCTHISPANIC"),
  What_it_holds = c("the surname, upper case",
                    "its rank by frequency among all surnames",
                    "how many people carry it",
                    "columns this chapter does not use",
                    "% of those people who are White alone",
                    "% who are Black alone",
                    "% who are Asian or Pacific Islander alone",
                    "% who are American Indian or Alaska Native alone",
                    "% who are two or more races",
                    "% who are Hispanic, which is a separate question"),
  SMITH = c("SMITH", "1", "2442977", "…", "70.9", "23.11", "0.5", "0.89",
            "2.19", "2.4"),
  VANG  = c("VANG", "726", "48036", "…", "1.37", "(S)", "96.7", "(S)",
            "1.41", "0.42"))

## ---- cleansn
sn[sn$name %in% c("SMITH", "VANG"), c("name", "rank", "count", grp)]

## ---- top2020
o <- head(s20, 8)
for (v in grep("^pct_", names(o), value = TRUE)) o[[v]] <- pc(o[[v]])
o$count <- n(o$count)
names(o) <- c("last name", "rank", "people", "white", "Black", "AIAN",
              "Asian", "two or more", "Hispanic")
o

## ---- neg2020
o <- head(s20n, 6)
o$row_total <- n(o$row_total)
names(o) <- c("last name", "category", "published", "with negatives",
              "people with that name")
o

## ---- decisive
o <- rbind(look("NGUYEN"), look("GARCIA"), look("WASHINGTON"))[, c("name", "count", grp)]
o$count <- n(o$count); names(o) <- c("surname", "people", pretty6)
o

## ---- ambiguous
o <- rbind(look("SMITH"), look("JOHNSON"), look("WILLIAMS"))[, c("name", "rank", "count", grp)]
o$count <- n(o$count); names(o) <- c("surname", "rank", "people", pretty6)
o

## ---- kilo-static
par(mar = c(4.4, 4.6, 1.0, 1.2))
plot(k1$pctwhite, k1$pctblack, xlim = c(0, 100), ylim = c(0, 100), las = 1,
     pch = 16, cex = 0.28 + sqrt(k1$count) / 900,
     col = adjustcolor("#2c7fb8", alpha.f = 0.45),
     xlab = "% of the people holding this surname who are white",
     ylab = "% who are Black")
abline(a = 100, b = -1, lty = 3, col = "grey55")
abline(h = 90, v = 90, lty = 2, col = "#999")
text(45, 93, paste0("more than 90% Black: ", k1_b90, " of the thousand"),
     adj = c(0.5, 0), cex = 0.72, col = "#C41230")
text(87, 52, paste0("more than 90% white:\n", k1_w90), adj = c(1, 0.5),
     cex = 0.72, col = "#4d9221")
mk  <- k1[k1$name %in% mark, ]
lpos <- c(SMITH = 4, WILLIAMS = 4, JOHNSON = 4, WASHINGTON = 4, NGUYEN = 3,
          GARCIA = 4, LEE = 3, YODER = 2)
points(mk$pctwhite, mk$pctblack, pch = 21, cex = 1.25, bg = "#C41230",
       col = "white", lwd = 1.2)
text(mk$pctwhite, mk$pctblack, mk$name, pos = lpos[mk$name], cex = 0.68,
     col = "#333", offset = 0.45)

## ---- kilo-d3
rows <- paste(sprintf('{"x":%.2f,"y":%.2f,"c":%d,"n":"%s"}',
                      k1$pctwhite, k1$pctblack, k1$count,
                      ifelse(k1$name %in% mark, k1$name, "")),
              collapse = ",")
cat(sprintf('
<div id="kilo" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=740,H=470,M={t:16,r:20,b:52,l:60};
const box=d3.select("#kilo");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("%% of the people holding this surname who are white");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% who are Black");
svg.append("line").attr("x1",x(0)).attr("y1",y(100)).attr("x2",x(100)).attr("y2",y(0))
  .attr("stroke","#aaa").attr("stroke-dasharray","3,3");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(90)).attr("y2",y(90))
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("line").attr("x1",x(90)).attr("x2",x(90)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(3)).attr("y",y(92)).attr("font-size","11.5px")
  .attr("fill","#C41230").text("more than 90%% Black: %d of the thousand");
svg.append("text").attr("x",x(91)).attr("y",y(18)).attr("font-size","11.5px")
  .attr("fill","#4d9221").text("more than");
svg.append("text").attr("x",x(91)).attr("y",y(13)).attr("font-size","11.5px")
  .attr("fill","#4d9221").text("90%% white: %d");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",d=>Math.max(1.6,Math.sqrt(d.c)/220))
  .attr("fill",d=>d.n?"#C41230":"#2c7fb8").attr("fill-opacity",d=>d.n?0.95:0.42)
  .attr("stroke",d=>d.n?"#fff":"none").attr("stroke-width",1.2)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.n||"one of the thousand"}</b><br>${d3.format(",")(d.c)} people<br>`+
     `${d.x.toFixed(1)}%% white &middot; ${d.y.toFixed(1)}%% Black`)
     .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.l").data(D.filter(d=>d.n)).join("text")
  .attr("x",d=>x(d.x)).attr("y",d=>y(d.y)-10).attr("text-anchor","middle")
  .attr("font-size","11px").attr("font-weight","600").attr("fill","#333")
  .text(d=>d.n);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a circle for the name and the counts behind it.</i></p>
', rows, k1_b90, k1_w90, k1_share, k1_maxb$name, k1_maxb$pctblack))

## ---- scale
data.frame(
  group = c("People whose surname is more than 90% one group",
            "People whose surname never reaches 60% for any group",
            "People covered by the file"),
  people = c(n(sum(sn$count[ok & sn$top > 90])), n(n_useless), n(pop_covered)),
  share = c(paste0(pc(p_decisive), "%"), paste0(pc(p_useless), "%"), "100%"))

## ---- topdist-static
par(mar = c(4.4, 5.0, 1.0, 1.2))
# how informative a surname is, is an ORDERED quantity: one hue, three
# darknesses. Green and red are reserved for "white" and "Black" throughout
# this document, and this figure is not about either.
fil <- ifelse(hmid > 90, ICOL[["decisive"]],
              ifelse(hmid < 60, ICOL[["settles"]], ICOL[["narrows"]]))
bp <- barplot(hpop / 1e6, space = 0, col = fil, border = "white", las = 1,
              ylim = c(0, max(hpop) / 1e6 * 1.12),
              ylab = "millions of people",
              xlab = "the strongest share any one group reaches for their surname (%)")
axis(1, at = seq(0, 20, by = 2), labels = seq(0, 100, by = 10))
abline(v = c(12, 18), lty = 2, col = "grey35")
text(0.4, max(hpop) / 1e6 * 1.06, paste0("settles nothing (under 60%):\n",
     pc(p_useless), "% of people"), adj = c(0, 1), cex = 0.72,
     col = ICOL[["narrows"]])
text(0.4, max(hpop) / 1e6 * 0.80, paste0("nearly decisive (over 90%):\n",
     pc(p_decisive), "% of people"), adj = c(0, 1), cex = 0.72,
     col = ICOL[["decisive"]])
legend(6.4, max(hpop) / 1e6 * 1.12,
       c("settles nothing (under 60%)", "narrows it (60-90%)",
         "nearly decisive (over 90%)"),
       fill = unname(ICOL), border = "white", bty = "n", cex = 0.68)

## ---- topdist-d3
rows <- paste(sprintf('{"m":%.1f,"p":%.0f}', hmid, hpop), collapse = ",")
cat(sprintf('
<div id="topd" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=740,H=380,M={t:26,r:20,b:52,l:66};
const box=d3.select("#topd");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.p)/1e6*1.1]).range([H-M.b,M.t]);
const bw=(x(5)-x(0));
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("the strongest share any one group reaches for their surname");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("millions of people");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.m)-bw/2+0.5).attr("width",bw-1)
  .attr("fill",d=>d.m>90?"%s":(d.m<60?"%s":"%s"))
  .attr("y",y(0)).attr("height",0)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${(d.m-2.5).toFixed(0)}%%\\u2013${(d.m+2.5).toFixed(0)}%%</b><br>`+
     `${d3.format(",")(d.p)} people<br>${(100*d.p/%.0f).toFixed(1)}%% of everyone covered`)
     .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0))
  .transition().duration(700).delay((d,i)=>i*22)
  .attr("y",d=>y(d.p/1e6)).attr("height",d=>y(0)-y(d.p/1e6));
[[60,"%s"],[90,"%s"]].forEach(t=>{
  svg.append("line").attr("x1",x(t[0])).attr("x2",x(t[0])).attr("y1",M.t-6).attr("y2",H-M.b)
    .attr("stroke",t[1]).attr("stroke-dasharray","5,4");});
svg.append("text").attr("x",x(2)).attr("y",M.t+2).attr("font-size","11.5px")
  .attr("fill","%s").text("settles nothing \\u2014 %.1f%% of people");
svg.append("text").attr("x",x(2)).attr("y",M.t+20).attr("font-size","11.5px")
  .attr("fill","%s").text("nearly decisive \\u2014 %.1f%% of people");
const lg=svg.append("g").attr("transform",`translate(${W-M.r-186},${M.t})`);
[["%s","settles nothing (under 60%%)"],["%s","narrows it (60-90%%)"],
 ["%s","nearly decisive (over 90%%)"]].forEach((v,i)=>{
  lg.append("rect").attr("x",0).attr("y",i*16).attr("width",11).attr("height",11)
    .attr("fill",v[0]).attr("stroke","#ddd");
  lg.append("text").attr("x",17).attr("y",i*16+10).attr("font-size","10.5px")
    .attr("fill","#555").text(v[1]);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a bar for the number of people in that bin. The tallest single bin is
%.0f%%&ndash;%.0f%%; %.1f%% of the population sits left of the 90%% line.</i></p>
', rows,
   ICOL[["decisive"]], ICOL[["settles"]], ICOL[["narrows"]],
   pop_covered,
   ICOL[["narrows"]], ICOL[["decisive"]],
   ICOL[["narrows"]], p_useless,
   ICOL[["decisive"]], p_decisive,
   ICOL[["settles"]], ICOL[["narrows"]], ICOL[["decisive"]],
   hmode - 2.5, hmode + 2.5, 100 - p_decisive))

## ---- ridge-static
par(mar = c(4.4, 10.8, 0.8, 1.4))
ord <- rev(seq_along(pretty6))
cls <- c("#4d9221", "#C41230", "#2c7fb8", "#e08214", "#999999", "#8856a7")
plot(NA, xlim = c(0, 100), ylim = c(0.4, length(pretty6) + 1.15), yaxt = "n",
     bty = "n", xlab = "share of this surname's bearers who are in this group (%)",
     ylab = "")
axis(2, at = seq_along(pretty6), labels = pretty6[ord], las = 1, tick = FALSE,
     cex.axis = 0.92)
for (j in seq_along(pretty6)) {
  d <- dens[[ord[j]]]
  polygon(c(d$x[1], d$x, d$x[length(d$x)]), c(j, j + 1.05 * d$y, j),
          col = adjustcolor(cls[ord[j]], alpha.f = 0.45), border = cls[ord[j]],
          lwd = 1.6)
  segments(d$med, j, d$med, j + 0.16, col = "grey20", lwd = 1.6)
}
abline(v = 90, lty = 2, col = "grey40")
text(89, length(pretty6) + 1.05, "90%", pos = 2, cex = 0.75, col = "grey30")

## ---- ridge-d3
ser <- paste(mapply(function(nm, d)
  sprintf('{"g":"%s","med":%.2f,"p":[%s]}', nm, d$med,
          paste(sprintf("[%.1f,%.4f]", d$x, d$y), collapse = ",")),
  pretty6, dens), collapse = ",")
cat(sprintf('
<div id="ridge" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=740,H=430,M={t:16,r:22,b:52,l:104};
const CL={"white":"#4d9221","Black":"#C41230","Asian/PI":"#2c7fb8",
          "Am. Indian":"#e08214","two or more":"#999999","Hispanic":"#8856a7"};
const box=d3.select("#ridge");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const step=(H-M.b-M.t)/D.length;
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("share of this surname\\u2019s bearers who are in this group");
svg.append("line").attr("x1",x(90)).attr("x2",x(90)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(90)-5).attr("y",M.t+11).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#777").text("90%%");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach((s,i)=>{
  const base=M.t+(i+1)*step;
  const ar=d3.area().x(d=>x(d[0])).y0(base).y1(d=>base-d[1]*step*1.35)
    .curve(d3.curveBasis);
  svg.append("path").attr("d",ar(s.p)).attr("fill",CL[s.g]).attr("fill-opacity",0.42)
    .attr("stroke",CL[s.g]).attr("stroke-width",1.6)
    .on("mousemove",function(e){tip.style("opacity",1).html(
       `<b>${s.g}</b><br>median surname is ${s.med.toFixed(1)}%% ${s.g}`)
       .style("left",Math.min(e.offsetX+14,W-280)+"px").style("top",(e.offsetY-10)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0));
  svg.append("line").attr("x1",x(s.med)).attr("x2",x(s.med)).attr("y1",base)
    .attr("y2",base-step*0.22).attr("stroke","#333").attr("stroke-width",1.6);
  svg.append("text").attr("x",M.l-12).attr("y",base-3).attr("text-anchor","end")
    .attr("font-size","12.5px").attr("fill","#222").text(s.g);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Surnames held by more than a thousand people. Each ridge is scaled to its own
height, so compare where the mass sits, not how tall it is. <i>Hover a ridge for
its median.</i> The Black ridge has a median of %.1f%%.</p>
', ser, dens[["Black"]]$med))

## ---- asym
o <- data.frame(group = names(n90), surnames_over_90pct = as.integer(n90))
o <- o[order(-o$surnames_over_90pct), ]
names(o) <- c("group", "surnames that are more than 90% this group")
o

## ---- asym-prep
# DOTS, not bars. A bar says "compare my length"; on a logarithmic axis that
# length is the logarithm, so a 99-to-1 ratio would be drawn as roughly 2.5
# to 1 and the reader would take the wrong number away from the one figure in
# this document whose entire point is the ratio. A dot says "read my
# position", which is what a log axis is for. The ratio is then stated in
# words next to the two dots it belongs to.
av    <- sort(n90)
azero <- av == 0                      # a true zero has no place on a log axis
aratio <- n90[["white"]] / max(n90[["Black"]], 1)
alab  <- sprintf("%s to 1", format(round(aratio), big.mark = ",", trim = TRUE))
acap  <- sprintf(paste(
  "Surnames held by more than a thousand people. The axis is logarithmic, so",
  "each gridline is ten times the one before it and equal distances are equal",
  "MULTIPLES, not equal counts: white to Black is %s. %s is a true zero and",
  "cannot be placed on a logarithmic axis at all, so it is marked with an",
  "open circle at the left edge."),
  alab, names(av)[azero][1])

## ---- asym-static
par(mar = c(7.6, 8.5, 1.2, 4.2))
plot(NA, xlim = c(1, 40000), ylim = c(0.5, length(av) + 0.5), log = "x",
     axes = FALSE, xlab = "", ylab = "")
abline(v = 10^(0:4), col = "grey92")
axis(1, at = 10^(0:4), labels = c("1", "10", "100", "1,000", "10,000"),
     cex.axis = 0.8, col = "grey70", col.axis = "#666666")
axis(2, at = seq_along(av), labels = names(av), las = 1, tick = FALSE,
     cex.axis = 0.9)
mtext("surnames more than 90% this group (log scale)", side = 1, line = 2.4,
      cex = 0.85)
segments(1, seq_along(av), pmax(av, 1), seq_along(av), col = "grey78",
         lwd = 1.6)
points(pmax(av, 1)[!azero], which(!azero), pch = 19, cex = 1.5,
       col = GCOL[names(av)][!azero])
points(1, which(azero), pch = 21, cex = 1.5, bg = "white",
       col = GCOL[names(av)][azero], lwd = 2)
text(pmax(av, 1), seq_along(av), paste0("  ", format(av, big.mark = ",",
     trim = TRUE)), pos = 4, cex = 0.8, xpd = NA, col = "#333333")
cw <- strwrap(acap, width = 96)
mtext(cw, side = 1, line = 3.9 + (seq_along(cw) - 1) * 0.95, adj = 0,
      cex = 0.66, col = "#555555")

## ---- asym-d3
v <- sort(n90, decreasing = TRUE)
rows <- paste(sprintf('{"g":"%s","v":%d,"t":"%s","c":"%s"}',
                      names(v), as.integer(v),
                      format(as.integer(v), big.mark = ",", trim = TRUE),
                      GCOL[names(v)]),
              collapse = ",")
cat(sprintf('
<div id="asym" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=290,M={t:16,r:74,b:46,l:110};
const svg=d3.select("#asym").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
// dots, not bars: see the comment in the asym-prep chunk
const x=d3.scaleLog().domain([1,40000]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.g)).range([M.t,H-M.b]).padding(0.24);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickValues([1,10,100,1000,10000])
    .tickFormat(d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("surnames more than 90%% this group (log scale)");
[1,10,100,1000,10000].forEach(g=>svg.append("line").attr("x1",x(g)).attr("x2",x(g))
  .attr("y1",M.t-4).attr("y2",H-M.b).attr("stroke","#eee"));
const cy=d=>y(d.g)+y.bandwidth()/2;
svg.append("g").selectAll("line.s").data(D).join("line")
  .attr("x1",x(1)).attr("x2",x(1)).attr("y1",cy).attr("y2",cy)
  .attr("stroke","#c9c9c9").attr("stroke-width",1.8)
  .transition().duration(750).attr("x2",d=>x(Math.max(d.v,1)));
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",x(1)).attr("cy",cy).attr("r",6)
  .attr("fill",d=>d.v>0?d.c:"#fff")
  .attr("stroke",d=>d.c).attr("stroke-width",d=>d.v>0?0:2)
  .transition().duration(750).attr("cx",d=>x(Math.max(d.v,1)));
svg.append("g").selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(Math.max(d.v,1))+11).attr("y",d=>cy(d)+4)
  .attr("font-size","11.5px").attr("fill","#333").attr("opacity",0)
  .text(d=>d.t)
  .transition().delay(750).duration(300).attr("opacity",1);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s</p>
', rows, acap))

## ---- presidents
o <- rbind(look("WASHINGTON"), look("JEFFERSON"), look("LINCOLN"))[,
       c("name", "count", "pctblack", "pctwhite")]
o$count <- n(o$count)
names(o) <- c("surname", "people", "% Black", "% white")
o

## ---- blackest
b <- big[!is.na(big$pctblack), ]
o <- head(b[order(-b$pctblack), c("name", "count", "pctblack", "pctwhite")], 6)
o$count <- n(o$count)
names(o) <- c("surname", "people", "% Black", "% white")
o

## ---- counties
o <- co[co$fips %in% c("42003", "36005", "15003"),
        c("county", "state", "pop", grp6)]
o$pop <- n(o$pop)
for (k in grp6) o[[k]] <- pc(100 * o[[k]])
names(o) <- c("county", "state", "population", pretty6)
o

## ---- flip
o <- rbind(`SMITH in Allegheny County, PA` = bisg("SMITH", "42003"),
           `SMITH in the Bronx, NY`        = bisg("SMITH", "36005"),
           `LEE in Allegheny County, PA`   = bisg("LEE", "42003"),
           `LEE in Honolulu County, HI`    = bisg("LEE", "15003"))
o <- data.frame(name_and_place = rownames(o), o, check.names = FALSE)
names(o) <- c("surname and place", pretty6)
o

## ---- gains
o <- data.frame(surname = rownames(gains),
                before = pc(gains[, "before"]),
                after  = pc(gains[, "after"]),
                change = sprintf("%+.1f", gains[, "change"]))
names(o) <- c("surname", "strongest guess from the name alone",
              "after adding the Bronx", "change")
o

## ---- gains-static
gd <- gains[order(gains[, "change"]), , drop = FALSE]
par(mar = c(4.4, 7.0, 1.6, 3.2))
plot(NA, xlim = c(0, 100), ylim = c(0.6, nrow(gd) + 0.4), yaxt = "n", bty = "n",
     las = 1, ylab = "",
     xlab = "strongest guess about a person's race (%)")
axis(2, at = seq_len(nrow(gd)), labels = rownames(gd), las = 1, tick = FALSE,
     cex.axis = 0.9)
abline(v = seq(0, 100, 20), col = "grey92")
segments(gd[, "before"], seq_len(nrow(gd)), gd[, "after"], seq_len(nrow(gd)),
         lwd = 3.4, col = "grey70", lend = 1)
points(gd[, "before"], seq_len(nrow(gd)), pch = 21, bg = "white", col = "#999",
       cex = 1.5, lwd = 2)
points(gd[, "after"], seq_len(nrow(gd)), pch = 19, col = ACOL[["after"]],
       cex = 1.5)
text(pmax(gd[, "before"], gd[, "after"]), seq_len(nrow(gd)),
     sprintf(" %+.1f", gd[, "change"]), pos = 4, cex = 0.78,
     col = ACOL[["after"]], xpd = NA)
legend("top", c("from the name alone", "after adding the Bronx"),
       pch = c(21, 19), col = unname(ACOL), pt.bg = "white",
       bty = "n", horiz = TRUE, cex = 0.78, inset = c(0, -0.06), xpd = NA)

## ---- gains-d3
gd   <- gains[order(gains[, "change"]), , drop = FALSE]
rows <- paste(sprintf('{"n":"%s","a":%.1f,"b":%.1f,"c":%.1f}',
                      rownames(gd), gd[, "before"], gd[, "after"],
                      gd[, "change"]), collapse = ",")
cat(sprintf('
<div id="gns" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=740,H=330,M={t:42,r:64,b:48,l:100};
const box=d3.select("#gns");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.n)).range([H-M.b,M.t]).padding(0.42);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("strongest guess about a person\\u2019s race");
const yc=d=>y(d.n)+y.bandwidth()/2;
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",d=>x(d.a)).attr("x2",d=>x(d.a)).attr("y1",yc).attr("y2",yc)
  .attr("stroke","#bbb").attr("stroke-width",4).attr("stroke-linecap","round")
  .transition().duration(700).attr("x2",d=>x(d.b));
svg.append("g").selectAll("circle.a").data(D).join("circle")
  .attr("cx",d=>x(d.a)).attr("cy",yc).attr("r",6)
  .attr("fill","#fff").attr("stroke","#999").attr("stroke-width",2);
svg.append("g").selectAll("circle.b").data(D).join("circle")
  .attr("cx",d=>x(d.a)).attr("cy",yc).attr("r",6).attr("fill","%s")
  .transition().duration(700).attr("cx",d=>x(d.b));
svg.append("g").selectAll("text.c").data(D).join("text")
  .attr("x",d=>x(Math.max(d.a,d.b))+11).attr("y",d=>yc(d)+4)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","%s")
  .attr("opacity",0).text(d=>(d.c>0?"+":"")+d.c.toFixed(1))
  .transition().delay(700).duration(300).attr("opacity",1);
const key=[["from the name alone","#fff","%s"],
           ["after adding the Bronx","%s","%s"]];
key.forEach((k,i)=>{
  const kx=M.l+i*190;
  svg.append("circle").attr("cx",kx+6).attr("cy",18).attr("r",6)
    .attr("fill",k[1]).attr("stroke",k[2]).attr("stroke-width",2);
  svg.append("text").attr("x",kx+18).attr("y",22).attr("font-size","11.5px")
    .attr("fill","#333").text(k[0]);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>The hollow end is the name alone; the solid end is the name plus one county.</i></p>
', rows, ACOL[["after"]], ACOL[["after"]], ACOL[["before"]],
   ACOL[["after"]], ACOL[["after"]],
   rownames(gd)[1], rownames(gd)[nrow(gd)], gd[nrow(gd), "change"]))

## ---- suppression
data.frame(
  quantity = c("Cells in the six percentage columns",
               "Cells suppressed by the Census Bureau",
               "Share suppressed",
               "Names held by fewer than 1,000 people",
               "Share of all names in the file"),
  value = c(n(cells), n(miss), paste0(pc(100 * miss / cells), "%"),
            n(sum(sn$count < 1000)),
            paste0(pc(100 * mean(sn$count < 1000)), "%")))

## ---- supp-by-size
o <- data.frame(names_held_by = names(supp),
                pct_black_suppressed = paste0(pc(as.numeric(supp)), "%"),
                names_in_bucket = as.integer(table(sn10$bucket)))
names(o) <- c("names held by", "% with the Black share suppressed",
              "names in this bucket")
o

## ---- supp-static
sv <- as.numeric(supp)
par(mar = c(4.4, 10.6, 1.0, 5.2))
plot(NA, xlim = c(0, 60), ylim = c(0.6, length(sv) + 0.4), yaxt = "n", bty = "n",
     xlab = "% of names in this bucket with the Black share suppressed",
     ylab = "")
axis(2, at = seq_along(sv), labels = rev(levels(sn10$bucket)), las = 1,
     tick = FALSE, cex.axis = 0.9)
abline(v = seq(0, 60, 10), col = "grey92")
yy <- seq_along(sv); rv <- rev(sv); rn <- rev(supp_n)
segments(0, yy, rv, yy, col = "grey75", lwd = 1.8)
# no additive constant: dot AREA is the number of names, as the caption says
points(rv, yy, pch = 19, col = NCOL, cex = 2.6 * sqrt(rn / max(rn)))
text(rv, yy, paste0(" ", pc(rv), "%  (", n(rn), " names)"), pos = 4, cex = 0.74,
     col = "#333", xpd = NA)

## ---- supp-d3
rows <- paste(sprintf('{"b":"%s","v":%.1f,"n":%d}',
                      levels(sn10$bucket), as.numeric(supp), supp_n),
              collapse = ",")
cat(sprintf('
<div id="sup" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=740,H=300,M={t:18,r:170,b:48,l:112};
const box=d3.select("#sup");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,60]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.b)).range([M.t,H-M.b]).padding(0.4);
// range starts at 0 so dot AREA really is the count, as the caption says
const r=d3.scaleSqrt().domain([0,d3.max(D,d=>d.n)]).range([0,13]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("%% of names in this bucket with the Black share suppressed");
const yc=d=>y(d.b)+y.bandwidth()/2;
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",x(0)).attr("x2",x(0)).attr("y1",yc).attr("y2",yc)
  .attr("stroke","#ccc").attr("stroke-width",2)
  .transition().duration(700).attr("x2",d=>x(d.v));
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",x(0)).attr("cy",yc).attr("r",d=>r(d.n)).attr("fill","%s")
  .transition().duration(700).attr("cx",d=>x(d.v));
svg.append("g").selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(d.v)+r(d.n)+8).attr("y",d=>yc(d)+4).attr("font-size","11.5px")
  .attr("fill","#333").attr("opacity",0)
  .text(d=>d.v.toFixed(1)+"%%  ("+d3.format(",")(d.n)+" names)")
  .transition().delay(700).duration(300).attr("opacity",1);
})();
</script>
', rows, NCOL, max(as.numeric(supp)),
   levels(sn10$bucket)[which.max(as.numeric(supp))],
   as.numeric(supp)[length(supp)]))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
