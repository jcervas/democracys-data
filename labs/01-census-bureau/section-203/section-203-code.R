# section-203-code.R -- chunk bodies for section-203-brief.Rmd
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

det <- read.csv("data/derived/sect203_determined.csv", stringsAsFactors = FALSE)
cty <- read.csv("data/derived/sect203_counties.csv",   stringsAsFactors = FALSE)

cv  <- det[det$FLAG_COV == 1, ]
n_juris <- length(unique(cv$S203_GEOID))
lev <- tapply(cv$S203_GEOID, cv$LEVEL, function(x) length(unique(x)))
n_spanish <- sum(cv$LANGUAGE == "Hispanic")

# The statute at 52 U.S.C. 10310(c)(3) names four language minority groups:
# American Indian, Asian American, Alaskan Native, and of Spanish heritage.
# Every category in this file sits inside one of them, which is why the check
# below is written as a NEGATIVE: no category names a language outside the
# four, and a future file that added one would fail here rather than quietly
# widen a claim the chapter makes about who the statute can reach.
N_LANG <- length(unique(cty$LANGUAGE))
OUTSIDE <- grep("Arabic|Haitian|Creole|Russian|Portuguese|Polish|Somali|Amharic|Ukrainian|Yoruba|Swahili|German|Italian|Greek|Hebrew|Yiddish|Armenian|Farsi|Persian|Turkish",
                unique(cty$LANGUAGE), value = TRUE)
stopifnot(length(OUTSIDE) == 0L)

cty$lo <- cty$VACLEP - cty$MVACLEP
cty$hi <- cty$VACLEP + cty$MVACLEP

pass10 <- cty$VACLEP  > 10000
pass5  <- !is.na(cty$LEPPCT) & cty$FLAG5 == 1
n_edu_fail <- sum(cty$FLAG_EDU == 0)
edu_blocks <- sum((pass10 | pass5) & cty$FLAG_EDU == 0)

s10 <- cty[cty$lo < 10000 & cty$hi > 10000, ]
s10 <- s10[order(-s10$VACLEP), ]

p5 <- cty[!is.na(cty$LEPPCT) &
          cty$LEPPCT - cty$MLEPPCT < 5 & cty$LEPPCT + cty$MLEPPCT > 5, ]

odd <- cty[cty$FLAG_COV == 1 & cty$FLAG10 == 0 & cty$FLAG5 == 0, ]
odd <- odd[order(odd$VACLEP), ]

real <- cty$VACLEP > 10000 & cty$FLAG_EDU == 1
ths  <- c(7500, 9000, 10000, 12500, 15000)   # counterfactual lines, chosen here
thtab <- do.call(rbind, lapply(ths, function(t) {
  now <- cty$VACLEP > t & cty$FLAG_EDU == 1
  data.frame(threshold = t, qualifying = sum(now),
             added = sum(now & !real), dropped = sum(real & !now))
}))

lt <- local({
  a <- aggregate(cbind(VACLEP, MVACLEP) ~ LANGUAGE, cty[cty$FLAG_COV == 1, ],
                 median)
  k <- as.data.frame(table(cty$LANGUAGE[cty$FLAG_COV == 1]),
                     stringsAsFactors = FALSE)
  names(k) <- c("LANGUAGE", "counties")
  m <- merge(k, a, by = "LANGUAGE")
  m$moe_pct <- round(100 * m$MVACLEP / m$VACLEP, 1)
  m[order(m$counties, -m$moe_pct), ]
})

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
shorten <- function(x) sub(" alone or in combination", "", x)

# ---- inputs the figures share, so both output paths draw the same thing ----

# (a) determinations by language group; the Native provisions come from the file
lang <- as.data.frame(table(cv$LANGUAGE), stringsAsFactors = FALSE)
names(lang) <- c("language", "determinations")
lang$native <- as.integer(tapply(pmax(cv$FLAG_AIAN, cv$FLAG_ANRC),
                                 cv$LANGUAGE, max)[lang$language])
lang$short  <- shorten(lang$language)
lang <- lang[order(lang$determinations, lang$short), ]
n_native <- sum(lang$native == 1)

# (b) the funnel: how wide is the margin, against how big the jurisdiction is
fun <- cty[cty$VACLEP > 10 & cty$MVACLEP > 0 & cty$VACIT > 0, ]
fun$rel <- 100 * fun$MVACLEP / fun$VACLEP
fun$str <- as.integer(!is.na(fun$LEPPCT) & fun$LEPPCT - fun$MLEPPCT < 5 &
                      fun$LEPPCT + fun$MLEPPCT > 5)
fbin <- cut(log10(fun$VACIT), breaks = seq(1.5, 6.5, 0.5))
fmed <- data.frame(mid = 10 ^ (seq(1.75, 6.25, 0.5)),
                   med = as.numeric(tapply(fun$rel, fbin, median)),
                   n   = as.integer(table(fbin)))
fmed <- fmed[!is.na(fmed$med) & fmed$n >= 10, ]

# (c) the distribution of estimates around the statutory 10,000
hw  <- c(4000, 25000)                                  # window, chosen here
hst <- hist(cty$VACLEP[cty$VACLEP >= hw[1] & cty$VACLEP <= hw[2]],
            breaks = seq(hw[1], hw[2], 1000), plot = FALSE)
hbar <- data.frame(lo = head(hst$breaks, -1), hi = hst$breaks[-1],
                   n = hst$counts)
hbar$over <- hbar$lo >= 10000

# (d) the two counties the whole argument turns on. They are not named here and
# then looked up; they are found: of the counties whose published interval
# contains the statutory line, the one that clears it by least and the one that
# misses it by least. As the file stands those are Philadelphia and Solano.
above <- s10[s10$VACLEP > 10000, ]
below <- s10[s10$VACLEP < 10000, ]
phl <- above[which.min(above$VACLEP - 10000), ]
sol <- below[which.min(10000 - below$VACLEP), ]
duo <- rbind(phl, sol)

# one label style for a county-language row, built once and used by every figure
shortlab <- function(d) paste0(sub(" County", "", d$NAMELSAD), " (",
                               sub(" \\(.*", "", shorten(d$LANGUAGE)), ")")
duo$lab  <- shortlab(duo)
s10o     <- s10[order(s10$VACLEP), ]
s10o$lab <- shortlab(s10o)

# the statute's own number, formatted once, so print and screen say it the same
STATLAB <- paste("the statute:", n(10000))

# ---- the colors, fixed once ----------------------------------------------
# Four meanings, four colors, and no color carries two meanings anywhere in
# the document. "Covered" and "not covered" are one blue/red pair everywhere.
# The statutory line is a fact about the law rather than about either side of
# it, so it is black in every figure, on screen as in print. A row the survey
# cannot place on one side or the other is neither covered-colored nor
# not-covered-colored; it gets its own amber.
COVC  <- "#2c7fb8"     # covered, or clears the bar
NCOVC <- "#C41230"     # not covered, or misses it
STATC <- "#111111"     # the statutory threshold itself
STRDC <- "#d95f02"     # the survey cannot say which side this row is on
MASSC <- "#999999"     # the undistinguished mass of rows behind the point
TRNDC <- "#555555"     # a summary drawn through the data, not a category in it

# The language figure is the one categorical figure in the document. Its
# categories have nothing to do with coverage, so it borrows none of the
# colors above except the gray for "the rest".
LSPAN <- "#1b7837"
LNATV <- "#8856a7"
lang$col <- ifelse(lang$language == "Hispanic", LSPAN,
            ifelse(lang$native == 1, LNATV, MASSC))

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
o <- cty[cty$NAMELSAD == "Los Angeles County" & cty$LANGUAGE == "Hispanic",
         c("NAMELSAD", "LANGUAGE", "VACIT", "VACLEP", "MVACLEP", "LEPPCT",
           "FLAG10", "FLAG5", "FLAG_EDU", "FLAG_COV")]
o$VACIT <- n(o$VACIT); o$VACLEP <- n(o$VACLEP); o$MVACLEP <- n(o$MVACLEP)
names(o) <- c("jurisdiction", "language group", "voting-age citizens",
              "of them, limited English", "margin of error", "% of all",
              "10,000 test", "5% test", "illiteracy test", "covered")
o

## ---- files
data.frame(
  file = c("sect203_counties.csv", "sect203_determined.csv"),
  rows = c(n(nrow(cty)), n(nrow(det))),
  what_it_is = c("every county, every language group with any limited-English population",
                 "every jurisdiction the Bureau made a determination about"),
  covered_rows = c(n(sum(cty$FLAG_COV == 1)), n(nrow(cv))))

## ---- determinations
data.frame(
  quantity = c("Coverage determinations issued",
               "Distinct jurisdictions covered",
               "  of which counties",
               "  of which minor civil divisions",
               "  of which whole states",
               "Determinations for Spanish (recorded as 'Hispanic')"),
  value = c(nrow(cv), n_juris, lev[["County"]], lev[["MCD"]], lev[["State"]],
            n_spanish))

## ---- raws203
# A verbatim capture, folded at the commas. The column count in the prose is
# read back out of the header line rather than asserted beside it.
HD <- paste0("SUMLVL,LEVEL,S203_GEOID,AIAN,ANRC,ST,CNTY,MCD,NAMELSAD,",
             "LANCOUNT,LANGUAGE,POP,MPOP,VAPOP,MVAPOP,VACIT,MVACIT,VACLEP,",
             "MVACLEP,ILLIT,MILLIT,LEPPCT,MLEPPCT,ILLRAT,MILLRAT,FLAG10,",
             "FLAG5,FLAG_EDU,FLAG_ST,FLAG_JURIS,FLAG_AIAN,FLAG_ANRC,FLAG_COV")
RW <- c(
 "010,Nation,0100000US,0000,00000,00,000,NNNNN,,01,Total Population,,,,,",
 ",,,,,,,,1.3200,,,,,,,,,",
 "040,State,0400000US01,0000,00000,01,000,NNNNN,Alabama,02,Hispanic,",
 "208600,",
 "354,125600,240,72020,1196,13190,588,1352,182,0.4000,0.0000,10.3000,",
 "1.3000,,0,1,0,,,,0")
NRAW <- 1L + sum(strsplit(HD, "", fixed = TRUE)[[1]] == ",")
fold <- function(s, w = 71) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  brk <- which(ch == ",")
  out <- character(0); i <- 1L
  while (i <= length(ch)) {
    cand <- brk[brk >= i & brk < i + w]
    j <- if (length(ch) - i + 1L <= w || !length(cand)) length(ch) else max(cand)
    out <- c(out, paste(ch[i:j], collapse = "")); i <- j + 1L
  }
  out
}
# The capture is wrapped at the page width; the two records are rejoined and
# parsed. Empty cells are shown as such rather than trimmed away, because the
# emptiness of the Nation row is what the next paragraph is about.
.r1 <- paste0(RW[1], RW[2])
.r2 <- paste0(RW[3], RW[4], RW[5], RW[6])
.p  <- read.csv(text = paste(HD, .r1, .r2, sep = "\n"),
                stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = character(0))
.s203 <- c(
  SUMLVL = "summary level code — what kind of geography this row is",
  LEVEL = "the same thing in words",
  S203_GEOID = "geographic identifier for the jurisdiction",
  AIAN = "American Indian / Alaska Native area code",
  ANRC = "Alaska Native Regional Corporation code",
  ST = "state FIPS code", CNTY = "county FIPS code",
  MCD = "minor civil division code — NNNNN where none applies",
  NAMELSAD = "the jurisdiction's name",
  LANCOUNT = "numeric code for the language group",
  LANGUAGE = "the same language group, named in words",
  POP = "population of the language group", MPOP = "its margin of error",
  VAPOP = "voting-age population of the group", MVAPOP = "its margin of error",
  VACIT = "voting-age citizens in the group", MVACIT = "its margin of error",
  VACLEP = "voting-age citizens with limited English proficiency",
  MVACLEP = "its margin of error",
  ILLIT = "of those, the number below a fifth-grade education",
  MILLIT = "its margin of error",
  LEPPCT = "limited-English citizens as a percentage of the group",
  MLEPPCT = "its margin of error",
  ILLRAT = "the illiteracy rate — on the Nation row, the statutory threshold",
  MILLRAT = "its margin of error",
  FLAG10 = "meets the 10,000-person trigger",
  FLAG5 = "meets the 5-percent trigger",
  FLAG_EDU = "meets the education (illiteracy) condition",
  FLAG_ST = "covered statewide", FLAG_JURIS = "covered as a jurisdiction",
  FLAG_AIAN = "covered as an American Indian area",
  FLAG_ANRC = "covered as an Alaska Native corporation",
  FLAG_COV = "covered under Section 203, all conditions taken together")
data.frame(
  Column_as_it_arrives = names(.p),
  Row_1_the_Nation = ifelse(nzchar(unlist(.p[1, ])), unlist(.p[1, ]), "(empty)"),
  Row_2_Alabama    = ifelse(nzchar(unlist(.p[2, ])), unlist(.p[2, ]), "(empty)"),
  What_it_holds    = unname(.s203[names(.p)]))

## ---- cleans203
o <- cty[cty$NAMELSAD == "Los Angeles County" &
         cty$LANGUAGE %in% c("Hispanic", "Korean alone or in combination"),
         c("LANGUAGE", "VACIT", "VACLEP", "MVACLEP", "LEPPCT",
           "FLAG10", "FLAG_COV")]
o$LANGUAGE <- sub(" alone or in combination", "", o$LANGUAGE)
o

## ---- languages
o <- as.data.frame(sort(table(cv$LANGUAGE), decreasing = TRUE),
                   stringsAsFactors = FALSE)
names(o) <- c("language group", "determinations")
o$`language group` <- shorten(o$`language group`)
rbind(head(o, 4), data.frame(`language group` = "...", determinations = "",
                             check.names = FALSE), tail(o, 5))

## ---- langs-static
yy <- seq_len(nrow(lang))
par(mar = c(4.2, 12.5, 1, 2))
plot(NA, xlim = c(1, max(lang$determinations) * 1.5), ylim = c(0.5, nrow(lang) + 0.5),
     log = "x", yaxt = "n", bty = "n", ylab = "", las = 1,
     xlab = "jurisdictions covered for this language group")
segments(1, yy, lang$determinations, yy, col = "#cccccc", lwd = 1.6)
points(lang$determinations, yy, pch = 19, cex = 1.4, col = lang$col)
text(lang$determinations, yy, paste0("  ", lang$determinations), pos = 4,
     cex = 0.68, col = "#444444")
axis(2, at = yy, labels = lang$short, las = 1, tick = FALSE, cex.axis = 0.72,
     line = -0.4)
legend("bottomright", bty = "n", cex = 0.72, pch = 19,
       col = c(LSPAN, LNATV, MASSC),
       legend = c("Spanish", "covered under a Native provision somewhere",
                  "every other language group"))

## ---- langs-d3
rows <- paste(sprintf('{"l":"%s","v":%d,"c":"%s"}', lang$short,
                      lang$determinations, lang$col), collapse = ",")
cat(sprintf('
<div id="lng" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s].reverse();
const W=760,H=470,M={t:16,r:60,b:70,l:250};
const svg=d3.select("#lng").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([1,d3.max(D,d=>d.v)*1.5]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.l)).range([M.t,H-M.b]).padding(0.34);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6,"~s"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).call(g=>g.select(".domain").remove())
  .selectAll("text").attr("font-size","10.5px");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-34).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("jurisdictions covered for this language group (log scale)");
const cy=d=>y(d.l)+y.bandwidth()/2;
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",x(1)).attr("x2",x(1)).attr("y1",cy).attr("y2",cy)
  .attr("stroke","#ccc").attr("stroke-width",1.6)
  .transition().duration(750).attr("x2",d=>x(d.v));
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",x(1)).attr("cy",cy).attr("r",5).attr("fill",d=>d.c)
  .transition().duration(750).attr("cx",d=>x(d.v));
svg.append("g").selectAll("text").data(D).join("text")
  .attr("x",d=>x(d.v)+9).attr("y",d=>cy(d)+4).attr("font-size","11px")
  .attr("fill","#444").attr("opacity",0).text(d=>d.v)
  .transition().delay(750).duration(250).attr("opacity",1);
const lg=svg.append("g").attr("transform",`translate(${M.l},${H-10})`);
[["%s","Spanish"],["%s","covered under a Native provision somewhere"],
 ["%s","every other language group"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",i*185+5).attr("r",5).attr("fill",r[0]);
  lg.append("text").attr("x",i*185+14).attr("y",4).attr("font-size","10px")
    .attr("fill","#333").text(r[1]);});
})();
</script>
', rows, LSPAN, LNATV, MASSC))

## ---- rule
data.frame(
  test = c("Rows where the estimate exceeds 10,000",
           "Rows where the file's 10,000 flag is set",
           "Do they agree, row for row?"),
  value = c(sum(pass10), sum(cty$FLAG10 == 1),
            ifelse(identical(which(pass10), which(cty$FLAG10 == 1)),
                   "TRUE", "FALSE")))

## ---- near
o <- s10[, c("NAMELSAD", "LANGUAGE", "VACLEP", "FLAG10", "FLAG5", "FLAG_COV")]
o$LANGUAGE <- shorten(o$LANGUAGE); o$VACLEP <- n(o$VACLEP)
names(o) <- c("county", "language", "estimate", "10,000 test", "5% test",
              "covered")
o

## ---- moe
o <- s10[, c("NAMELSAD", "LANGUAGE", "VACLEP", "MVACLEP", "lo", "hi",
             "FLAG_COV")]
o$LANGUAGE <- shorten(o$LANGUAGE)
for (k in c("VACLEP","MVACLEP","lo","hi")) o[[k]] <- n(o[[k]])
names(o) <- c("county", "language", "estimate", "margin of error",
              "low end", "high end", "covered")
o

## ---- straddle-static
s <- s10o
cl <- ifelse(s$FLAG_COV == 1, COVC, NCOVC)
par(mar = c(4.4, 11.5, 2.4, 2))
plot(s$VACLEP, seq_len(nrow(s)), xlim = range(c(s$lo, s$hi)), pch = 19,
     col = cl, yaxt = "n",
     ylab = "", xlab = "voting-age citizens with limited English")
axis(2, at = seq_len(nrow(s)), labels = s$lab, las = 1, cex.axis = 0.78)
segments(s$lo, seq_len(nrow(s)), s$hi, seq_len(nrow(s)), lwd = 2, col = cl)
abline(v = 10000, lty = 2, lwd = 2, col = STATC)
# the line this whole figure is about, named in print as it is on screen
mtext(STATLAB, side = 3, at = 10000, line = 0.2, cex = 0.78,
      col = STATC, font = 2)
legend("bottomright", c("covered", "not covered"), col = c(COVC, NCOVC),
       pch = 19, bty = "n", cex = 0.85)

## ---- straddle-d3
s <- s10o
rows <- paste(sprintf('{"n":"%s","e":%d,"m":%d,"lo":%d,"hi":%d,"c":%d}',
                      s$lab, s$VACLEP, s$MVACLEP, s$lo, s$hi, s$FLAG_COV),
              collapse = ",")
cat(sprintf('
<div id="str" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=360,M={t:22,r:26,b:48,l:186};
const svg=d3.select("#str").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([d3.min(D,d=>d.lo)-120,d3.max(D,d=>d.hi)+120])
  .range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.n)).range([H-M.b,M.t]).padding(0.34);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d3.format(",")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0))
  .selectAll("text").attr("font-size","10.5px");
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("voting-age citizens with limited English");
svg.append("line").attr("x1",x(10000)).attr("x2",x(10000)).attr("y1",M.t-6)
  .attr("y2",H-M.b).attr("stroke","%s").attr("stroke-width",2)
  .attr("stroke-dasharray","6,4");
svg.append("text").attr("x",x(10000)).attr("y",M.t-10).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","%s")
  .text("%s");
const col=d=>d.c?"%s":"%s";
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("line").attr("y1",d=>y(d.n)+y.bandwidth()/2)
  .attr("y2",d=>y(d.n)+y.bandwidth()/2)
  .attr("x1",d=>x(d.e)).attr("x2",d=>x(d.e))
  .attr("stroke",col).attr("stroke-width",3).attr("stroke-linecap","round")
  .transition().duration(800).attr("x1",d=>x(d.lo)).attr("x2",d=>x(d.hi));
g.append("circle").attr("cx",d=>x(d.e))
  .attr("cy",d=>y(d.n)+y.bandwidth()/2).attr("r",4.6).attr("fill",col);
const lg=svg.append("g").attr("transform",`translate(${M.l+12},${M.t+4})`);
[["%s","covered"],["%s","not covered"]].forEach((r,i)=>{
  lg.append("circle").attr("cy",i*16).attr("r",4.6).attr("fill",r[0]);
  lg.append("text").attr("x",10).attr("y",i*16+4).attr("font-size","11.5px").text(r[1]);});
})();
</script>
', rows, STATC, STATC, STATLAB, COVC, NCOVC, COVC, NCOVC))

## ---- pct
data.frame(
  quantity = c("Counties whose interval straddles the 5% line",
               "  of them covered", "  of them not covered",
               "For comparison: counties straddling the 10,000 line"),
  value = c(nrow(p5), sum(p5$FLAG_COV == 1), sum(p5$FLAG_COV == 0), nrow(s10)))

## ---- pct-ex
o <- head(p5[order(-p5$VACLEP),
             c("NAMELSAD", "LANGUAGE", "VACIT", "VACLEP", "LEPPCT", "MLEPPCT",
               "FLAG_COV")], 5)
o$LANGUAGE <- shorten(o$LANGUAGE); o$VACIT <- n(o$VACIT); o$VACLEP <- n(o$VACLEP)
names(o) <- c("county", "language", "voting-age citizens", "limited English",
              "% of all", "margin on the %", "covered")
o

## ---- funnel-static
# This is the one figure in the document drawn to a raster device rather than
# straight into the PDF. It plots 10,565 points, and as vector art those points
# alone were half the weight of the whole file. Every point is still here; only
# the device changed. The screen version below is unaffected.
par(mar = c(3.8, 4.6, 1.2, 1.4))
plot(NA, xlim = range(fun$VACIT), ylim = c(0.3, max(fun$rel)), log = "xy",
     bty = "n", las = 1, xaxt = "n", yaxt = "n",
     xlab = "voting-age citizens in the jurisdiction",
     ylab = "margin of error, as % of the estimate")
xt <- 10 ^ (2:6); axis(1, at = xt, labels = c("100", "1k", "10k", "100k", "1m"))
yt <- c(0.5, 1, 2, 5, 10, 25, 50, 100)
axis(2, at = yt, labels = paste0(yt, "%"), las = 1)
points(fun$VACIT[fun$str == 0], fun$rel[fun$str == 0], pch = 19, cex = 0.3,
       col = paste0(MASSC, "55"))
points(fun$VACIT[fun$str == 1], fun$rel[fun$str == 1], pch = 19, cex = 1,
       col = STRDC)
lines(fmed$mid, fmed$med, col = TRNDC, lwd = 2.6)
points(fmed$mid, fmed$med, pch = 19, col = TRNDC, cex = 0.8)
legend("topright", bty = "n", cex = 0.76,
       pch = c(19, 19, NA), lwd = c(NA, NA, 2.6), pt.cex = c(0.5, 1, NA),
       col = c(MASSC, STRDC, TRNDC),
       legend = c("every county-language row",
                  "the rows whose interval straddles 5%",
                  "median margin, by jurisdiction size"))

## ---- funnel-d3
rows <- paste(sprintf('[%d,%.1f,%d]', fun$VACIT, fun$rel, fun$str),
              collapse = ",")
med <- paste(sprintf('[%.0f,%.2f]', fmed$mid, fmed$med), collapse = ",")
cat(sprintf('
<div id="fnl" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s].map(a=>({x:a[0],y:a[1],s:a[2]})),MD=[%s];
const W=760,H=430,M={t:18,r:22,b:52,l:64};
const svg=d3.select("#fnl").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain(d3.extent(D,d=>d.x)).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([0.3,d3.max(D,d=>d.y)]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickValues([100,1000,1e4,1e5,1e6])
    .tickFormat(d3.format("~s")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([0.5,1,2,5,10,25,50,100])
    .tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("voting-age citizens in the jurisdiction (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("margin of error, as %% of the estimate");
svg.append("g").selectAll("circle").data(D.filter(d=>!d.s)).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",1.4).attr("fill","%s")
  .attr("fill-opacity",0.35);
svg.append("path").datum(MD).attr("fill","none").attr("stroke","%s")
  .attr("stroke-width",2.6)
  .attr("d",d3.line().x(p=>x(p[0])).y(p=>y(p[1])));
svg.append("g").selectAll("circle").data(D.filter(d=>d.s)).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",4).attr("fill","%s");
const lg=svg.append("g").attr("transform",`translate(${W-M.r-268},${M.t+6})`);
[["%s","every county-language row",2],
 ["%s","the rows whose interval straddles 5%%",4],
 ["%s","median margin, by jurisdiction size",0]].forEach((r,i)=>{
  if(r[2]) lg.append("circle").attr("cx",5).attr("cy",i*16).attr("r",r[2]).attr("fill",r[0]);
  else lg.append("line").attr("x1",0).attr("x2",11).attr("y1",i*16).attr("y2",i*16)
    .attr("stroke",r[0]).attr("stroke-width",2.6);
  lg.append("text").attr("x",17).attr("y",i*16+4).attr("font-size","11px")
    .attr("fill","#333").text(r[1]);});
})();
</script>
', rows, med, MASSC, TRNDC, STRDC, MASSC, STRDC, TRNDC))

## ---- edu
data.frame(
  quantity = c("County-language rows in the file",
               "Rows that fail the illiteracy test",
               "Rows that clear the 10,000 bar",
               "Rows that clear the 5% bar",
               "Rows that clear either bar and are then blocked by the illiteracy test"),
  value = c(n(nrow(cty)), n(n_edu_fail), n(sum(pass10)), n(sum(pass5)),
            n(edu_blocks)))

## ---- odd
o <- head(odd[, c("NAMELSAD", "LANGUAGE", "VACLEP", "LEPPCT", "FLAG_AIAN",
                  "FLAG_ANRC", "FLAG_COV")], 5)
o$LANGUAGE <- shorten(o$LANGUAGE)
names(o) <- c("jurisdiction", "language", "limited-English citizens", "% of all",
              "American Indian provision", "Alaska Native provision", "covered")
o

## ---- moepct
o <- head(lt[, c("LANGUAGE", "counties", "VACLEP", "MVACLEP", "moe_pct")], 6)
o$LANGUAGE <- shorten(o$LANGUAGE)
names(o) <- c("language", "counties covered", "median estimate",
              "median margin", "margin as % of estimate")
o

## ---- thresholds
o <- thtab
o$threshold <- n(o$threshold)
names(o) <- c("if the threshold were", "county-language pairs qualifying",
              "added vs. the real rule", "dropped vs. the real rule")
o

## ---- hist-static
par(mar = c(4.4, 4.4, 1.6, 1.4))
plot(NA, xlim = hw, ylim = c(0, max(hbar$n) * 1.12), bty = "n", las = 1,
     xaxt = "n", xlab = "voting-age citizens with limited English",
     ylab = "county-language rows")
axis(1, at = seq(hw[1], hw[2], 5000),
     labels = paste0(seq(hw[1], hw[2], 5000) / 1000, "k"))
rect(hbar$lo, 0, hbar$hi, hbar$n, border = "white",
     col = ifelse(hbar$over, COVC, NCOVC))
abline(v = 10000, col = STATC, lwd = 2, lty = 2)
text(10000, max(hbar$n) * 1.08, STATLAB, pos = 4, cex = 0.76, col = STATC)
legend("topright", bty = "n", cex = 0.76, fill = c(COVC, NCOVC),
       border = NA, legend = c("clears the bar", "does not"))

## ---- hist-d3
rows <- paste(sprintf('{"a":%d,"b":%d,"n":%d,"o":%d}', hbar$lo, hbar$hi,
                      hbar$n, as.integer(hbar$over)), collapse = ",")
cat(sprintf('
<div id="hst" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=380,M={t:30,r:22,b:52,l:56};
const svg=d3.select("#hst").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([%d,%d]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.n)*1.1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>(d/1000)+"k"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("voting-age citizens with limited English");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("county-language rows");
const cap=d3.select("#hst").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const base="<b>Hover a bar.</b> Each bar counts the county-language rows in a bin one thousand wide. The statutory line at %s falls between two of them.";
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.a)+0.5).attr("width",d=>Math.max(x(d.b)-x(d.a)-1,1))
  .attr("fill",d=>d.o?"%s":"%s").style("cursor","pointer")
  .attr("y",y(0)).attr("height",0)
  .on("mousemove",(e,d)=>cap.html("<b>"+d3.format(",")(d.a)+" to "+
    d3.format(",")(d.b)+"</b> \\u2014 "+d.n+" county-language rows, "+
    (d.o?"all of them above the bar.":"all of them below it.")))
  .on("mouseleave",()=>cap.html(base))
  .transition().delay((d,i)=>i*22).duration(400)
  .attr("y",d=>y(d.n)).attr("height",d=>y(0)-y(d.n));
svg.append("line").attr("x1",x(10000)).attr("x2",x(10000)).attr("y1",M.t-10)
  .attr("y2",H-M.b).attr("stroke","%s").attr("stroke-width",2)
  .attr("stroke-dasharray","6,4");
svg.append("text").attr("x",x(10000)+6).attr("y",M.t-14)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","%s")
  .text("%s");
cap.html(base);
})();
</script>
', rows, hw[1], hw[2], n(10000), COVC, NCOVC, STATC, STATC, STATLAB))

## ---- duo-static
xr <- c(min(duo$lo) - 400, max(duo$hi) + 400)
par(mar = c(4.4, 9.6, 2.2, 3.2))
plot(NA, xlim = xr, ylim = c(0.42, 2.62), yaxt = "n", bty = "n", ylab = "",
     xlab = "voting-age citizens with limited English")
rect(10000, 0.3, xr[2], 2.8, col = paste0(COVC, "11"), border = NA)
abline(v = 10000, lwd = 2, lty = 2, col = STATC)
mtext(STATLAB, side = 3, at = 10000, line = 0.2, cex = 0.78, col = STATC)
cl <- ifelse(duo$FLAG_COV == 1, COVC, NCOVC)
segments(duo$VACLEP[1], 2, duo$VACLEP[2], 1, col = "#777777", lwd = 2.2)
segments(duo$lo, 2:1, duo$hi, 2:1, col = cl, lwd = 7, lend = 1)
points(duo$VACLEP, 2:1, pch = 21, bg = "white", col = cl, cex = 1.5, lwd = 2.4)
axis(2, at = 2:1, labels = duo$lab, las = 1, tick = FALSE, cex.axis = 0.82)
# pushed clear of the statutory line, which otherwise strikes through this text
text(duo$VACLEP + c(280, -280), c(2.42, 0.58), n(duo$VACLEP), cex = 0.74,
     col = cl)
text(duo$hi, 2:1, ifelse(duo$FLAG_COV == 1, "  covered", "  not covered"),
     pos = 4, cex = 0.74, col = cl, xpd = NA)
text(mean(duo$VACLEP), 1.5, paste0(n(duo$VACLEP[1] - duo$VACLEP[2]), " apart"),
     pos = 4, cex = 0.74, col = "#555555")

## ---- duo-d3
rows <- paste(sprintf('{"l":"%s","e":%d,"m":%d,"lo":%d,"hi":%d,"c":%d}',
                      duo$lab, duo$VACLEP, duo$MVACLEP, duo$lo, duo$hi,
                      duo$FLAG_COV), collapse = ",")
cat(sprintf('
<div id="duo" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=250,M={t:40,r:120,b:52,l:190};
const svg=d3.select("#duo").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([d3.min(D,d=>d.lo)-400,d3.max(D,d=>d.hi)+400])
  .range([M.l,W-M.r]);
const yv=[M.t+16,M.t+80];
svg.append("rect").attr("x",x(10000)).attr("y",M.t-14)
  .attr("width",W-M.r-x(10000)).attr("height",H-M.b-M.t+14).attr("fill","%s")
  .attr("fill-opacity",0.07);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d3.format(",")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("voting-age citizens with limited English");
svg.append("line").attr("x1",x(10000)).attr("x2",x(10000)).attr("y1",M.t-14)
  .attr("y2",H-M.b).attr("stroke","%s").attr("stroke-width",2)
  .attr("stroke-dasharray","6,4");
svg.append("text").attr("x",x(10000)).attr("y",M.t-20).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","%s").text("%s");
const col=d=>d.c?"%s":"%s";
svg.append("line").attr("x1",x(D[0].e)).attr("y1",yv[0]).attr("x2",x(D[1].e))
  .attr("y2",yv[1]).attr("stroke","#777").attr("stroke-width",2.2);
svg.append("text").attr("x",(x(D[0].e)+x(D[1].e))/2+8).attr("y",(yv[0]+yv[1])/2+4)
  .attr("font-size","11.5px").attr("fill","#555")
  .text(d3.format(",")(D[0].e-D[1].e)+" apart");
D.forEach((d,i)=>{
  svg.append("line").attr("x1",x(d.e)).attr("x2",x(d.e)).attr("y1",yv[i])
    .attr("y2",yv[i]).attr("stroke",col(d)).attr("stroke-width",8)
    .attr("stroke-linecap","butt")
    .transition().duration(800).attr("x1",x(d.lo)).attr("x2",x(d.hi));
  svg.append("circle").attr("cx",x(d.e)).attr("cy",yv[i]).attr("r",6)
    .attr("fill","#fff").attr("stroke",col(d)).attr("stroke-width",2.6);
  svg.append("text").attr("x",M.l-12).attr("y",yv[i]+4).attr("text-anchor","end")
    .attr("font-size","12px").text(d.l);
  svg.append("text").attr("x",W-M.r+10).attr("y",yv[i]+4).attr("font-size","11.5px")
    .attr("fill",col(d)).text(d.c?"covered":"not covered");});
})();
</script>
', rows, COVC, STATC, STATC, STATLAB, COVC, NCOVC))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#duo text[fill="#555" i]
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
#duo text[fill="#555" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
