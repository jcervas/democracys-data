# policing-code.R -- chunk bodies for policing-brief.Rmd
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
d <- read.csv("data/derived/by_race.csv",  stringsAsFactors = FALSE)
y <- read.csv("data/derived/by_year.csv",  stringsAsFactors = FALSE)
d$share       <- 100 * d$stops / sum(d$stops)
d$search_rate <- 100 * d$searched / d$stops
d$hit_rate    <- 100 * d$contraband_found / d$searched
y$search_rate <- 100 * y$searched / y$stops
y$hit_rate    <- 100 * y$contraband_found / y$searched
r  <- function(x, v) d[[v]][d$race == x]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# ---- the three totals the funnel figure is drawn from ------------------------
# Computed once here, and the percentage labels are formatted once here too. The
# HTML funnel used to compute "5.9% of stops" in JavaScript while the PDF one
# computed it in R; the two agreed by luck, and a change of one stop in the
# source would have been enough to make them disagree in the last digit. Both
# renderers now print these strings.
ST <- sum(d$stops); SE <- sum(d$searched); FD <- sum(d$contraband_found)
SE_PCT <- pc(100 * SE / ST, 1)
FD_PCT <- pc(100 * FD / ST, 2)
FBLU <- "#2c7fb8"; FRED <- "#C41230"; FGRY <- "#9a9a9a"

# the horizontal line in the search-rate/hit-rate figure: one weighted mean,
# used by both renderers rather than recomputed in each
WM <- 100 * FD / SE

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

# ---- the same stops over three real denominators -----------------------------
# American Community Survey counts for San Francisco County, one row per group
# per denominator, each with the ACS table it came from. The fourth column is a
# different question and is built separately below, which is the point of the
# figure.
acs <- read.csv("data/derived/acs_denominators.csv", stringsAsFactors = FALSE,
                colClasses = c(state = "character", county = "character"))
DEN <- c("residents", "adults 18+", "drives to work")
# only the groups the ACS can support at every denominator; "other" has no
# clean ACS counterpart beyond residents, so it is left out rather than faked
BR  <- c("white", "black", "hispanic", "asian/pacific islander")
bench <- do.call(rbind, lapply(DEN, function(dn) {
  s <- acs[acs$denominator == dn, ]
  m <- merge(d[d$race %in% BR, c("race", "stops", "search_rate")], s, by = "race")
  m$rate  <- m$stops / m$count
  m$ratio <- m$rate / m$rate[m$race == "white"]
  m$den   <- dn
  m[, c("race", "den", "ratio")]
}))
# the fourth column is a different question: given a stop, how often a search
srr <- data.frame(race = BR, den = "per stop",
                  ratio = d$search_rate[match(BR, d$race)] /
                          d$search_rate[d$race == "white"])
bench <- rbind(bench, srr)
BCOL <- c(white = "#2c7fb8", black = "#C41230", hispanic = "#e08214",
          `asian/pacific islander` = "#4d9221")
BX   <- c(DEN, "per stop")
bfmt <- function(x) formatC(x, format = "f", digits = 2)
bget <- function(r, dn) bench$ratio[bench$race == r & bench$den == dn]

## ---- schema
data.frame(
  field = c("date, time", "location", "subject_race", "subject_sex, subject_age",
            "reason_for_stop", "search_conducted", "search_basis",
            "contraband_found", "outcome"),
  what_it_holds = c("when the stop happened", "district or intersection",
                    "the officer's perception, entered on the form",
                    "as recorded by the officer",
                    "the violation cited", "TRUE / FALSE",
                    "consent, probable cause, incident to arrest",
                    "TRUE / FALSE, recorded only if a search happened",
                    "warning, citation, arrest"),
  check.names = FALSE)

## ---- counts
data.frame(
  quantity = c("Stops in the file", "Years covered", "Stops that led to a search",
               "Searches that found contraband", "Race categories recorded"),
  value = c(n(ST), paste(min(y$year), "to", max(y$year)),
            n(SE), n(FD), nrow(d)))

## ---- rawstop
# A capture of the first line of the download and of two data rows, with the
# three location fields blanked. The counts quoted in the paragraphs below are
# taken from this text at knit time, not asserted about it.
RAW <- c(
"raw_row_number,date,time,location,lat,lng,district,subject_age,subject_race,subject_sex,type,arrest_made,citation_issued,warning_issued,outcome,contraband_found,search_conducted,search_vehicle,search_basis,reason_for_stop,raw_search_vehicle_description,raw_result_of_contact_description",
"869921,2014-08-01,00:01:00,[redacted],[redacted],[redacted],NA,NA,asian/pacific islander,female,vehicular,FALSE,FALSE,TRUE,warning,NA,FALSE,FALSE,NA,Mechanical or Non-Moving Violation (V.C.),No Search,Warning",
"870048,2014-08-01,14:34:00,[redacted],[redacted],[redacted],NA,NA,white,male,vehicular,FALSE,TRUE,FALSE,citation,TRUE,TRUE,TRUE,other,Moving Violation,\"Search without Consent, Positive Result\",Citation")

# Break a long line after a comma that is outside quotes -- the same rule a CSV
# parser uses, so a quoted field is never torn in half.
fold <- function(s, w = 72) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  brk <- which(ch == "," & !(cumsum(ch == "\"") %% 2 == 1))
  out <- character(0); i <- 1L
  while (i <= length(ch)) {
    cand <- brk[brk >= i & brk < i + w]
    j <- if (length(ch) - i + 1L <= w || !length(cand)) length(ch) else max(cand)
    out <- c(out, paste(ch[i:j], collapse = "")); i <- j + 1L
  }
  out
}
NF <- 1L + sum(strsplit(RAW[1], "", fixed = TRUE)[[1]] == ",")

# Twenty-two columns down the page, the two captured stops beside them. NA is
# left exactly as the file writes it -- it is a string in this file, not a
# missing value R has decided about.
.p <- read.csv(text = paste(RAW, collapse = "\n"), stringsAsFactors = FALSE,
               check.names = FALSE, colClasses = "character",
               na.strings = character(0))
# The Open Policing Project documents its standardised schema; the two `raw_`
# columns are the exception, and are the department's own words carried through
# untranslated, which is what the next paragraphs are about.
.pol <- c(
  raw_row_number = "the row's number in the department's own export",
  date = "date of the stop", time = "time of the stop",
  location = "where it happened — redacted here",
  lat = "latitude — redacted here", lng = "longitude — redacted here",
  district = "police district",
  subject_age = "the driver's age", subject_race = "the driver's race, as standardised by the project",
  subject_sex = "the driver's sex",
  type = "vehicular or pedestrian",
  arrest_made = "whether an arrest followed",
  citation_issued = "whether a citation was issued",
  warning_issued = "whether a warning was issued",
  outcome = "the outcome, collapsed to one word",
  contraband_found = "whether contraband was found",
  search_conducted = "whether any search happened",
  search_vehicle = "whether the vehicle was searched",
  search_basis = "the stated legal basis for the search",
  reason_for_stop = "why the stop was made",
  raw_search_vehicle_description = "the department's own words for the search",
  raw_result_of_contact_description = "the department's own words for the outcome")
data.frame(Column_as_it_arrives = names(.p),
           What_it_holds = unname(.pol[names(.p)]),
           Stop_1 = unname(unlist(.p[1, ])),
           Stop_2 = unname(unlist(.p[2, ])))

## ---- translate
# Parsed out of the capture above rather than typed here, so the mapping shown
# is the mapping in those two rows.
fields <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  cut <- which(ch == "," & !(cumsum(ch == "\"") %% 2 == 1))
  st <- c(1L, cut + 1L); en <- c(cut - 1L, length(ch))
  gsub("^\"|\"$", "",
       vapply(seq_along(st),
              function(i) if (st[i] > en[i]) "" else
                paste(ch[st[i]:en[i]], collapse = ""), character(1)))
}
h <- fields(RAW[1]); g <- function(r, k) fields(RAW[r])[match(k, h)]
data.frame(
  the_department_wrote = c(g(2, "raw_search_vehicle_description"),
                           g(2, "raw_result_of_contact_description"),
                           g(3, "raw_search_vehicle_description"),
                           g(3, "raw_result_of_contact_description")),
  the_release_records = c(
    paste0("search_conducted = ", g(2, "search_conducted"),
           ", contraband_found = ", g(2, "contraband_found")),
    paste0("outcome = ", g(2, "outcome")),
    paste0("search_conducted = ", g(3, "search_conducted"),
           ", contraband_found = ", g(3, "contraband_found")),
    paste0("outcome = ", g(3, "outcome"))),
  check.names = FALSE)

## ---- cleanrace
d[order(-d$stops), c("race", "stops", "searched", "contraband_found", "arrests")]

## ---- stops-by-race
o <- d[order(-d$stops), c("race", "stops", "share")]
o$stops <- n(o$stops); o$share <- pc(as.numeric(gsub(",", "", o$share)), 1)
names(o) <- c("race (officer-perceived)", "stops", "% of all stops")
o

## ---- denominator
data.frame(
  `you would need` = c("Who was driving", "Where and when they were driving",
                       "Who committed a violation", "Who was carrying contraband"),
  `does it exist?` = c("No", "No", "No", "Only for those searched"),
  check.names = FALSE)

## ---- bench-d3
# ---------------------------------------------------------------------------
# Every ratio drawn here was computed in R, in setup, from acs_denominators.csv
# and by_race.csv, and its printed label was formatted there too. Nothing in
# this figure is recomputed in JavaScript.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
rows <- paste(sprintf('{"r":"%s","d":"%s","v":%.4f,"lab":"%s"}',
                      bench$race, bench$den, bench$ratio, bfmt(bench$ratio)),
              collapse = ",")
cat(sprintf('
<div id="bench" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s],XS=%s,COL=%s;
const W=760,H=440,M={t:26,r:150,b:64,l:62};
const box=d3.select("#bench");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scalePoint().domain(XS).range([M.l,W-M.r]).padding(0.55);
const y=d3.scaleLog().domain([0.45,5.6]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([0.5,1,2,3,5]).tickFormat(d=>d+"x"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("stop rate relative to white drivers");
// the band between the last real denominator and the one that does not exist
const gapL=(x(XS[2])+x(XS[3]))/2;
svg.append("rect").attr("x",gapL-46).attr("y",M.t).attr("width",92)
  .attr("height",H-M.b-M.t).attr("fill","#f4f4f4").attr("stroke","#bbb")
  .attr("stroke-dasharray","6,4");
svg.append("text").attr("x",gapL).attr("y",M.t+24).attr("text-anchor","middle")
  .attr("font-size","26px").attr("font-weight","600").attr("fill","#aaa").text("?");
["who was actually","driving, and who","committed a","violation"].forEach((t,i)=>
  svg.append("text").attr("x",gapL).attr("y",M.t+48+i*13).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#888").text(t));
svg.append("text").attr("x",gapL).attr("y",H-M.b-8).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("font-style","italic").attr("fill","#999")
  .text("never counted");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(1)).attr("y2",y(1))
  .attr("stroke","#555").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",M.l+4).attr("y",y(1.30)).attr("font-size","11px")
  .attr("fill","#555").text("same rate as white drivers");
XS.forEach(s=>{
  svg.append("text").attr("x",x(s)).attr("y",H-M.b+18).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("fill","#444").text("per "+s.replace("per ",""));});
svg.append("text").attr("x",(x(XS[0])+x(XS[2]))/2).attr("y",H-M.b+40)
  .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#777")
  .text("denominators that exist");
svg.append("text").attr("x",x(XS[3])).attr("y",H-M.b+40).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#777").text("conditioning instead");
const byR=d3.group(D,d=>d.r);
const ln=d3.line().x(d=>x(d.d)).y(d=>y(d.v));
byR.forEach((v,k)=>{
  const real=v.filter(d=>d.d!=="per stop"), cond=v.filter(d=>d.d==="per stop");
  svg.append("path").attr("d",ln(real)).attr("fill","none").attr("stroke",COL[k])
    .attr("stroke-width",2.4);
  svg.append("path").attr("d",ln([real[real.length-1],cond[0]])).attr("fill","none")
    .attr("stroke",COL[k]).attr("stroke-width",1.4).attr("stroke-dasharray","3,3")
    .attr("opacity",0.55);
  svg.append("g").selectAll("circle").data(v).join("circle")
    .attr("cx",d=>x(d.d)).attr("cy",d=>y(d.v)).attr("r",4.5).attr("fill",COL[k]);
  svg.append("text").attr("x",x(XS[3])+10).attr("y",y(cond[0].v)+4)
    .attr("font-size","11.5px").attr("fill",COL[k]).text(k);
  v.filter(d=>k!=="white"||d.d==="per stop").forEach(d=>
    svg.append("text").attr("x",x(d.d)).attr("y",y(d.v)-9)
      .attr("text-anchor","middle").attr("font-size","10px").attr("fill",COL[k])
      .text(d.lab));
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Log scale, so equal distances are equal ratios.</p>
', rows,
   paste0('["', paste(BX, collapse = '","'), '"]'),
   paste0('{', paste(paste0('"', names(BCOL), '":"', BCOL, '"'),
                     collapse = ","), '}')))

## ---- bench-static
# The same ratios, the same log axis, the same labels: base R for the PDF
# device, D3 above for the browser. Both read `bench`, built once in setup.
par(mar = c(4.9, 4.6, 0.8, 8.6))
xp <- c(1, 2, 3, 4.6)                       # gap before the fourth column
plot(NA, xlim = c(0.75, 4.95), ylim = c(0.45, 5.6), log = "y", axes = FALSE,
     xlab = "", ylab = "stop rate relative to white drivers")
rect(3.55, 0.45, 4.05, 5.6, col = "#f4f4f4", border = "#bbb", lty = 2)
text(3.8, 3.6, "?", cex = 2.1, font = 2, col = "#aaa")
text(3.8, 2.15, "who was actually\ndriving, and who\ncommitted a violation",
     cex = 0.56, col = "#888")
text(3.8, 0.52, "never counted", cex = 0.56, col = "#999", font = 3)
axis(2, at = c(0.5, 1, 2, 3, 5), labels = paste0(c(0.5, 1, 2, 3, 5), "x"),
     las = 1, cex.axis = 0.8)
axis(1, at = xp, labels = paste0("per ", sub("^per ", "", BX)), tick = FALSE,
     cex.axis = 0.76, mgp = c(3, 0.5, 0))
abline(h = 1, lty = 2, col = "#555")
text(0.78, 1.30, "same rate as white drivers", adj = c(0, 0), cex = 0.62,
     col = "#555")
mtext("denominators that exist", side = 1, line = 2.1, at = 2, cex = 0.68,
      col = "#777")
mtext("conditioning instead", side = 1, line = 2.1, at = 4.6, cex = 0.68,
      col = "#777")
# NB: not `for (r in ...)` -- `r()` is this document's global accessor helper
for (rc in BR) {
  v <- sapply(BX, function(z) bget(rc, z))
  lines(xp[1:3], v[1:3], col = BCOL[[rc]], lwd = 2.4)
  lines(xp[3:4], v[3:4], col = BCOL[[rc]], lwd = 1.3, lty = 3)
  points(xp, v, pch = 19, col = BCOL[[rc]], cex = 0.95)
  if (rc != "white")            # white is 1.00 by construction; labels collide
    text(xp, v, bfmt(v), pos = 3, cex = 0.58, col = BCOL[[rc]], offset = 0.42)
  else
    text(xp[4], v[4], bfmt(v[4]), pos = 3, cex = 0.58, col = BCOL[[rc]],
         offset = 0.42)
  text(4.78, v[4], rc, adj = c(0, 0.5), cex = 0.66, col = BCOL[[rc]], xpd = NA)
}

## ---- search-rate
o <- d[order(-d$search_rate), c("race", "stops", "searched", "search_rate")]
o$stops <- n(o$stops); o$searched <- n(o$searched)
o$search_rate <- pc(as.numeric(o$search_rate), 2)
names(o) <- c("race", "stops", "searches", "% of stops searched")
o

## ---- funnel-d3
# Counts and their percentage labels are formatted once, in R, and passed in as
# strings; nothing here is divided in JavaScript.
cat(sprintf('
<div id="fun" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const ST=%d,SE=%d,FD=%d;
const LBL=["","%s","%s","%s"],PCT=["","","%s%% of stops","%s%% of stops"];
const W=760,H=300,MID=132,FULL=150;
const BLU="#2c7fb8",RED="#C41230",GRY="#9a9a9a";
const svg=d3.select("#fun").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const T=(x,y,s,o)=>{const t=svg.append("text").attr("x",x).attr("y",y)
  .attr("text-anchor",(o&&o.a)||"middle").attr("font-size",(o&&o.s)||"12px")
  .attr("fill",(o&&o.c)||"#333");if(o&&o.b)t.attr("font-weight","600");return t.text(s);};
const XS=[60,270,480,690], hh=[0,FULL,FULL*SE/ST,FULL*FD/ST];
const BW=104;
// the denominator that does not exist
svg.append("rect").attr("x",XS[0]-BW/2).attr("y",MID-FULL/2-16).attr("width",BW)
  .attr("height",FULL+32).attr("fill","#f4f4f4").attr("stroke",GRY)
  .attr("stroke-width",1.6).attr("stroke-dasharray","6,4").attr("rx",3);
// on-mark: both sit inside the pale #f4f4f4 box drawn just above.
T(XS[0],MID-14,"everyone",{b:1,c:"#555"}).attr("class","on-mark");
T(XS[0],MID+4,"on the road",{b:1,c:"#555"}).attr("class","on-mark");
T(XS[0],MID+40,"?",{b:1,c:GRY,s:"30px"});
T(XS[0],MID+FULL/2+34,"never counted",{c:GRY,s:"11px"});
// the funnel
for(let i=1;i<3;i++){
  svg.append("path").attr("d",`M${XS[i]+BW/2},${MID-hh[i]/2}L${XS[i+1]-BW/2},${MID-hh[i+1]/2}`+
    `L${XS[i+1]-BW/2},${MID+hh[i+1]/2}L${XS[i]+BW/2},${MID+hh[i]/2}Z`)
    .attr("fill","#c9d9e6").attr("fill-opacity",0.7);}
[1,2,3].forEach(i=>svg.append("rect").attr("x",XS[i]-BW/2)
  .attr("y",MID-hh[i]/2).attr("width",BW).attr("height",Math.max(hh[i],1.2))
  .attr("fill",i===3?RED:BLU).attr("fill-opacity",0.78));
svg.append("defs").append("marker").attr("id","fm").attr("viewBox","0 0 10 10")
  .attr("refX",9).attr("refY",5).attr("markerWidth",6).attr("markerHeight",6)
  .attr("orient","auto").append("path").attr("d","M0,0L10,5L0,10Z").attr("fill","#555");
svg.append("line").attr("x1",XS[0]+BW/2+8).attr("y1",MID).attr("x2",XS[1]-BW/2-8)
  .attr("y2",MID).attr("stroke","#555").attr("stroke-width",1.8)
  .attr("marker-end","url(#fm)");
const LAB=["","STOPPED","SEARCHED","CONTRABAND FOUND"];
[1,2,3].forEach(i=>{
  T(XS[i],MID-FULL/2-30,LAB[i],{b:1,s:"11.5px",c:i===3?RED:BLU});
  T(XS[i],MID-FULL/2-12,LBL[i],{b:1,s:"14px",c:"#222"});
  if(i>1)T(XS[i],MID+FULL/2+22,PCT[i],{c:"#555",s:"11px"});});
svg.append("line").attr("x1",XS[3]).attr("y1",MID+2).attr("x2",XS[3])
  .attr("y2",MID-FULL/2-8).attr("stroke","#777").attr("stroke-width",0.8);
})();
</script>
', ST, SE, FD, n(ST), n(SE), n(FD), SE_PCT, FD_PCT))

## ---- funnel-static
par(mar = rep(0.2, 4))
plot(NA, xlim = c(0, 100), ylim = c(-8, 44), asp = NA, axes = FALSE, ann = FALSE)
H <- 26; xs <- c(6, 34, 62, 90); mid <- 14
h <- c(NA, H, H * SE/ST, H * FD/ST)          # true proportions
# The last band is ~0.23 units tall. Earlier versions raised it to 0.25 so it
# would show up, which quietly made the rarest step look 9% commoner than it is
# while the HTML version drew it honestly. Both now draw the true height and
# mark it with a leader line instead.
rect(xs[1] - 5, mid - H/2 - 3, xs[1] + 9, mid + H/2 + 3, col = "#f4f4f4",
     border = FGRY, lwd = 1.6, lty = 2)
text(xs[1] + 2, mid + 5.5, "everyone", cex = 0.68, font = 2, col = "#555")
text(xs[1] + 2, mid + 1.5, "on the road", cex = 0.68, font = 2, col = "#555")
text(xs[1] + 2, mid - 4.5, "?", cex = 1.7, font = 2, col = FGRY)
text(xs[1] + 2, mid - H/2 - 6.8, "never counted", cex = 0.58, col = FGRY)

for (i in 2:3)
  polygon(c(xs[i] + 9, xs[i+1] - 5, xs[i+1] - 5, xs[i] + 9),
          c(mid + h[i]/2, mid + h[i+1]/2, mid - h[i+1]/2, mid - h[i]/2),
          col = adjustcolor("#c9d9e6", 0.7), border = NA)
for (i in 2:4) {
  rect(xs[i] - 5, mid - h[i]/2, xs[i] + 9, mid + h[i]/2,
       col = adjustcolor(if (i == 4) FRED else FBLU, 0.78), border = NA)
}
# the contraband band is genuinely too thin to see; point at it rather than
# inflate it
segments(xs[4] + 2, mid - h[4]/2 - 3.2, xs[4] + 2, mid - h[4]/2 - 0.4,
         col = FRED, lwd = 0.8)
arrows(xs[1] + 10, mid, xs[2] - 6, mid, length = 0.07, lwd = 1.6, col = "#555")

LAB <- c("", "STOPPED", "SEARCHED", "CONTRABAND FOUND")
CNTL <- c("", n(ST), n(SE), n(FD))
PCTL <- c("", "", paste0(SE_PCT, "% of stops"), paste0(FD_PCT, "% of stops"))
for (i in 2:4) {
  text(xs[i] + 2, mid + H/2 + 8.5, LAB[i], cex = 0.6, font = 2,
       col = if (i == 4) FRED else FBLU)
  text(xs[i] + 2, mid + H/2 + 4.6, CNTL[i], cex = 0.68, font = 2, col = "#222")
  if (i > 2) text(xs[i] + 2, mid - H/2 - 4.2, PCTL[i], cex = 0.58, col = "#555")
}
segments(xs[4] + 2, mid + 0.6, xs[4] + 2, mid + H/2 + 3.2, col = "#777", lwd = 0.7)

## ---- hit-rate
o <- d[order(-d$search_rate), c("race", "searched", "contraband_found", "hit_rate")]
o$searched <- n(o$searched); o$contraband_found <- n(o$contraband_found)
o$hit_rate <- pc(as.numeric(o$hit_rate), 1)
names(o) <- c("race", "searches", "contraband found", "% of searches finding contraband")
o

## ---- scatter-static
# WM, the overall share of searches that found contraband, is computed once in
# setup; the D3 version below is handed the same number rather than summing the
# columns again.
par(mar = c(4.4, 4.6, 0.8, 1.4))
plot(d$search_rate, d$hit_rate, pch = 19, col = "#C41230",
     cex = sqrt(d$stops)/300, xlim = c(0, 18), ylim = c(0, 42), las = 1,
     xlab = "% of stops that led to a search",
     ylab = "% of searches that found contraband")
text(d$search_rate, d$hit_rate, d$race, pos = 4, cex = 0.75)
abline(h = WM, lty = 3, col = "grey50")
# the HTML version labels this line; without the label the PDF reader has no way
# to know what it means
text(17.6, WM + 1.4, "equal thresholds would put every group on one horizontal line",
     adj = c(1, 0), cex = 0.62, col = "#777")
text(0.2, 40.5, "circle area is the number of stops", adj = c(0, 0.5),
     cex = 0.62, col = "#777")

## ---- d3-scatter
rows <- paste(sprintf('{"r":"%s","s":%d,"sr":%.2f,"hr":%.1f,"n":%d,"h":%d}',
                      d$race, d$stops, d$search_rate, d$hit_rate,
                      d$searched, d$contraband_found), collapse = ",")
cat(sprintf('
<div id="pol" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const data=[%s],WM=%.4f;
const W=760,H=440,M={t:20,r:24,b:48,l:56};
const svg=d3.select("#pol").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,18]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,42]).range([H-M.b,M.t]);
const rad=d3.scaleSqrt().domain([0,d3.max(data,d=>d.s)]).range([0,42]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("share of stops that led to a search");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("share of searches that found contraband");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(WM)).attr("y2",y(WM))
  .attr("stroke","#999").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",W-M.r-4).attr("y",y(WM)-6).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#777")
  .text("equal thresholds would put every group on one horizontal line");
const tip=d3.select("#pol").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(data).join("circle")
  .attr("cx",d=>x(d.sr)).attr("cy",d=>y(d.hr)).attr("r",d=>rad(d.s))
  .attr("fill","#C41230").attr("fill-opacity",0.28)
  .attr("stroke","#C41230").attr("stroke-width",1.8)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html(
      `<b>${d.r}</b><br>${d3.format(",")(d.s)} stops<br>`+
      `searched ${d.sr}%% (${d3.format(",")(d.n)})<br>`+
      `contraband in ${d.hr}%% of searches`)
      .style("left",Math.min(e.offsetX+14,W-200)+"px").style("top",(e.offsetY-10)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("g").selectAll("text.lab").data(data).join("text").attr("class","lab")
  .attr("x",d=>x(d.sr)).attr("y",d=>y(d.hr)-rad(d.s)-7).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#333").text(d=>d.r);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Circle area is the number of stops. Hover for detail.</p>
', rows, WM))

## ---- trend-static
ks <- c("white", "black", "hispanic", "asian/pacific islander", "other")
cl <- c("#2c7fb8", "#C41230", "#e08214", "#4d9221", "#999999")
plot(NA, xlim = range(y$year), ylim = c(0, 50), xlab = "", las = 1,
     ylab = "% of searches finding contraband")
for (i in seq_along(ks)) {
  s <- y[y$race == ks[i], ]; s <- s[order(s$year), ]
  lines(s$year, s$hit_rate, col = cl[i], lwd = 2.2)
}
legend("topright", ks, col = cl, lwd = 2.2, bty = "n", cex = 0.8)

## ---- d3-trend
ks <- c("white", "black", "hispanic", "asian/pacific islander", "other")
cl <- c("#2c7fb8", "#C41230", "#e08214", "#4d9221", "#999999")
ser <- paste(mapply(function(k, col) {
  s <- y[y$race == k, ]; s <- s[order(s$year), ]
  sprintf('{"k":"%s","c":"%s","v":[%s]}', k, col,
          paste(sprintf('[%d,%.1f]', s$year, s$hit_rate), collapse = ","))
}, ks, cl), collapse = ",")
cat(sprintf('
<div id="trend" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const S=[%s];
const W=760,H=400,M={t:18,r:150,b:38,l:52};
const svg=d3.select("#trend").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const yrs=S[0].v.map(p=>p[0]);
const x=d3.scaleLinear().domain(d3.extent(yrs)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,50]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(10));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>d+"%%").ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("share of searches finding contraband");
const ln=d3.line().x(p=>x(p[0])).y(p=>y(p[1]));
const g=svg.append("g");
S.forEach(s=>{
  g.append("path").attr("d",ln(s.v)).attr("fill","none")
    .attr("stroke",s.c).attr("stroke-width",2.4).attr("class","ser").attr("data-k",s.k);
  const last=s.v[s.v.length-1];
  svg.append("text").attr("x",W-M.r+8).attr("y",y(last[1])+4)
    .attr("font-size","11.5px").attr("fill",s.c).text(s.k);
});
// vertical readout
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#bbb").attr("opacity",0);
const dots=svg.append("g");
const tip=d3.select("#trend").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const yr=Math.round(x.invert(d3.pointer(e,this)[0]+M.l));
    if(yr<yrs[0]||yr>yrs[yrs.length-1])return;
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const rows=S.map(s=>({k:s.k,c:s.c,val:s.v.find(p=>p[0]===yr)[1]}))
                .sort((a,b)=>b.val-a.val);
    dots.selectAll("circle").data(rows).join("circle")
      .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",4).attr("fill",r=>r.c);
    tip.style("opacity",1).html(`<b>${yr}</b><br>`+
      rows.map(r=>`<span style="color:${r.c}">■</span> ${r.k}: ${r.val}%%`).join("<br>"))
      .style("left",Math.min(x(yr)-M.l+18,W-260)+"px").style("top",(M.t+6)+"px");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);dots.selectAll("circle").remove();tip.style("opacity",0);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the chart to read every group in a given year.</p>
', ser))

## ---- yearcheck
w <- y[y$race == "white",   c("year", "hit_rate")]
b <- y[y$race == "black",   c("year", "hit_rate")]
m <- merge(w, b, by = "year", suffixes = c("_white", "_black"))
m$white_higher <- ifelse(m$hit_rate_white > m$hit_rate_black, "yes", "no")
m$hit_rate_white <- pc(m$hit_rate_white); m$hit_rate_black <- pc(m$hit_rate_black)
names(m) <- c("year", "white hit rate (%)", "Black hit rate (%)", "white higher?")
m

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: on the dark page the fill is lifted and already passes,
# and a --paper stroke would sit dark behind a dark ink there, because the
# checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#pol text[fill="#333" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
