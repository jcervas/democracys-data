# false-matches-code.R -- chunk bodies for false-matches-brief.Rmd
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

# Everything below is read from data/, which data/build-data.R wrote from two
# real registration files. Those files are never copied into this folder and
# nothing here identifies a person: these are counts and distributions only.
FC <- read.csv("data/derived/facts.csv",       stringsAsFactors = FALSE)
KY <- read.csv("data/derived/keys.csv",        stringsAsFactors = FALSE)
CC <- read.csv("data/derived/crosscounty.csv", stringsAsFactors = FALSE)
GS <- read.csv("data/derived/groupsize.csv",   stringsAsFactors = FALSE)
SC <- read.csv("data/derived/scaling.csv",     stringsAsFactors = FALSE)
MD <- read.csv("data/derived/monthday.csv",    stringsAsFactors = FALSE)
GA <- read.csv("data/derived/ga_keys.csv",     stringsAsFactors = FALSE)
BD <- read.csv("data/derived/birthday.csv",    stringsAsFactors = FALSE)

# A missing key must stop the render, not render as nothing. An inline
# `r fx("typo")` that returns character(0) disappears silently and leaves a
# sentence with a hole where a number should be -- which is how a paragraph
# ends up reading "the numerator is ... of them in the consortium data".
fx <- function(k) {
  v <- FC$value[FC$key == k]
  if (length(v) != 1L) stop("facts.csv has no single value for '", k, "'")
  v
}
nn <- function(x) format(as.numeric(x), big.mark = ",", trim = TRUE)
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

# ---- a probability is never rounded up to certainty -------------------------
# A probability below 1 must not print as "100%", and one above 0 must not
# print as "0%". For any finite room the chance of a shared birthday is
# strictly less than 1 -- at 80 people it is 0.9999914 -- and rendering that as
# "100.0%" would assert a certainty the arithmetic does not have. In a chapter
# about a program that treated a probable match as a proven one, that is the
# same error in miniature, so the formatter refuses to commit it.
#
# A value that rounds to 100 at k decimals but is genuinely below 1 prints as
# ">99.9%" (at k = 1); one that rounds to 0 but is above 0 prints as "<0.1%".
# An exact "100%" is reserved for a probability that really is 1, which for the
# birthday problem happens only at 366 people, where the pigeonhole principle
# makes a repeat unavoidable.
#
# `p` is a proportion in [0, 1]; pct100() takes a value already in percent.
#
# `is_one` exists because a stored double is not a reliable witness to
# certainty. The birthday probability underflows to exactly 1.0 at 153 people
# although it is mathematically below 1 until 366, so the caller passes the
# fact rather than letting the float decide.
ppct <- function(p, k = 1, is_one = p >= 1, is_zero = p <= 0) {
  p   <- as.numeric(p)
  d   <- function(v) formatC(v, format = "f", digits = k)
  out <- paste0(d(100 * p), "%")
  out[d(100 * p) == d(100) & !is_one]  <- paste0(">", d(100 - 10^(-k)), "%")
  out[d(100 * p) == d(0)   & !is_zero] <- paste0("<", d(10^(-k)), "%")
  out[is_one]  <- "100%"
  out[is_zero] <- "0%"
  out
}
pct100 <- function(x, k = 1) ppct(as.numeric(x) / 100, k)

# The birthday curve's own formatter: certainty is read off n, never off p.
bpct <- function(n, k = 1)
  ppct(BD$p[match(n, BD$n)], k, is_one = BD$certain[match(n, BD$n)])
kv <- function(lab, col) KY[[col]][KY$label == lab]
gv <- function(lab, col) GA[[col]][GA$label == lab]
cv <- function(q) CC$value[CC$quantity == q]

CCK <- "first + last name + full date of birth"     # the Crosscheck key
NJN <- as.numeric(fx("nj_n"))

# ---- render every data.frame in this document as a TABLE, not code output ---
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# The exact birthday probability, recomputed here so the figure and the prose
# cannot disagree with each other.
pshare <- function(n, d = 365) if (n > d) 1 else 1 - prod((d - seq_len(n) + 1) / d)

# The other question, the one people mistake the first for: the chance that
# somebody in the room shares ONE NAMED person's birthday. Far smaller, and the
# reason the usual guess of 183 feels right.
pmine  <- function(n, d = 365) 1 - (1 - 1 / d)^(max(n - 1, 0))

# THE CLASS SIZE, defined once. The HTML figure's input defaults to it, the PDF
# figure marks it, and the prose and captions read it from here -- so a change
# of roster or an absence is a change in exactly one place.
CLASS  <- 20                       # 19 students and the instructor
EVEN   <- BD$n[which(BD$p > 0.5)[1]]
# The largest room the HTML control allows, and the plotted domain in BOTH
# renderings. Defined here, not in the D3 chunk, because the base-R figure
# needs it too and that chunk does not run when the PDF is built.
XMAX   <- 120

# The reference draw the prose compares the whole state against. It is the same
# row build-data.R used to compute rate_growth, taken from facts.csv rather
# than re-derived here, so the figure, the caption and the ratio in the text
# cannot drift apart from one another.
SCP     <- SC[SC$pairs > 0, ]
SMALL_N <- as.numeric(fx("rate_small_n"))
SMALL_R <- as.numeric(fx("rate_small"))

## ---- shape-fm
data.frame(
  stage = c("The 21 county extracts", "One key per record",
            "Distinct keys", "Keys held by more than one registrant",
            "What ships in data/keys.csv"),
  rows = c(nn(NJN), nn(NJN), nn(kv(CCK, "keys")), nn(kv(CCK, "dup_keys")),
           nn(nrow(KY))),
  a_row_is = c("one registration, 28 columns",
               "one registration, six columns and a key",
               "one name-and-birth-date, however many people hold it",
               "one collision", "one matching rule, with its collision counts"))

## ---- birthday-d3
# ---------------------------------------------------------------------------
# THE EXACT CURVE, not the exponential approximation. Probabilities are
# computed in R and handed to D3 as finished numbers, so this figure and the
# base-R one below plot identical values; the only difference is the hover.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; later figures use the library loaded here.
# ---------------------------------------------------------------------------
B <- BD[BD$n <= XMAX, ]
# EVERY PERCENTAGE IN THIS FIGURE IS FORMATTED HERE, IN R, AND PASSED THROUGH
# AS A STRING. The JavaScript never rounds a probability. That keeps R's
# half-to-even and JS's half-up from disagreeing, and -- more importantly --
# means the reader cannot drive the control to 120 people and be told the
# chance is "100.0%" when it is not. At the top of the range this reads
# ">99.9%", which is the honest statement.
rows <- paste(sprintf('{"n":%d,"p":%.6f,"pm":%.6f,"ps":"%s","pms":"%s","pr":"%s"}',
                      B$n, 100 * B$p, 100 * B$pmine,
                      ppct(B$p, 1, is_one = B$certain),
                      ppct(B$pmine, 1, is_one = rep(FALSE, nrow(B))),
                      nn(B$pairs)),
              collapse = ",")
cat(sprintf('
<form id="bdayctl" style="margin:0.9em 0 0.3em 0;font-size:0.92em"
      onsubmit="return false">
  <label for="bdayn" style="font-weight:600">People in the room:</label>
  <input id="bdayn" type="number" min="2" max="%d" step="1" value="%d"
         style="width:5.2em;padding:2px 4px;margin-left:0.4em;font-size:1em">
  <span style="color:#666;margin-left:0.6em">between 2 and %d &mdash;
  the class has %d</span>
</form>
<div id="bday" style="position:relative;margin:0.2em 0 1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s], CLASS=%d, EVEN=%d, NMIN=2, NMAX=%d;
const W=760,H=400,M={t:22,r:20,b:44,l:52};
const svg=d3.select("#bday").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,NMAX]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("people in the room");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("probability");
// ---- fixed furniture: these do NOT move with the control -------------------
[[50,"even odds"],[100,""]].forEach(g=>{
  svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(g[0])).attr("y2",y(g[0]))
    .attr("stroke","#888").attr("stroke-dasharray","4,3");
  if(g[1]) svg.append("text").attr("x",W-M.r-4).attr("y",y(g[0])-5).attr("text-anchor","end")
    .attr("font-size","10.5px").attr("fill","#666").text(g[1]);
});
svg.append("path").datum(D).attr("fill","#C41230").attr("fill-opacity",0.13)
  .attr("d",d3.area().x(d=>x(d.n)).y0(y(0)).y1(d=>y(d.p)));
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.4).attr("d",d3.line().x(d=>x(d.n)).y(d=>y(d.p)));
// The other question, drawn to scale beside the first: the chance somebody
// shares ONE NAMED person birthday. Same axes, and nowhere close.
svg.append("path").datum(D).attr("fill","none").attr("stroke","#4a6785")
  .attr("stroke-width",1.8).attr("stroke-dasharray","5,3")
  .attr("d",d3.line().x(d=>x(d.n)).y(d=>y(d.pm)));
const lg=svg.append("g").attr("transform","translate("+(M.l+14)+","+(M.t+4)+")");
[["#C41230","any two people in the room share a birthday",0,false],
 ["#4a6785","somebody shares one named person birthday",17,true]].forEach(r=>{
  lg.append("line").attr("x1",0).attr("x2",20).attr("y1",r[2]).attr("y2",r[2])
    .attr("stroke",r[0]).attr("stroke-width",2.4)
    .attr("stroke-dasharray",r[3]?"5,3":null);
  lg.append("text").attr("x",26).attr("y",r[2]+4).attr("font-size","11.5px")
    .attr("fill","#333").text(r[1]);
});
// even odds marker: fixed reference, never moves
const de=D[EVEN-1];
svg.append("circle").attr("cx",x(de.n)).attr("cy",y(de.p)).attr("r",4.5)
  .attr("fill","none").attr("stroke","#111").attr("stroke-width",1.6);
svg.append("text").attr("x",x(de.n)+10).attr("y",y(de.p)-9)
  .attr("font-size","11px").attr("fill","#444")
  .text("even odds: "+de.n+" people, "+de.ps);
// ---- the moving marker -----------------------------------------------------
const mk=svg.append("g");
const mline=mk.append("line").attr("stroke","#111").attr("stroke-dasharray","3,3");
const mdot=mk.append("circle").attr("r",5.5).attr("fill","#111");
const mtext=mk.append("text").attr("font-size","11.5px").attr("font-weight","600");
const out=d3.select("#bday").append("p").attr("style",
  "font-size:0.9em;color:#222;margin:0.4em 0 0 0;min-height:2.6em");
function draw(n){
  const d=D[n-1];
  mline.attr("x1",x(d.n)).attr("x2",x(d.n)).attr("y1",y(0)).attr("y2",y(d.p));
  mdot.attr("cx",x(d.n)).attr("cy",y(d.p));
  const left=x(d.n)>W-260;
  mtext.attr("x",x(d.n)+(left?-10:10)).attr("y",y(d.p)+(d.p>92?24:-11))
    .attr("text-anchor",left?"end":"start")
    .text(d.n+" people: "+d.ps);
  out.html("In a room of <b>"+d.n+"</b> there are <b>"+d.pr+
    "</b> pairs to check. The chance that <b>some two of them</b> share a "+
    "birthday is <b>"+d.ps+"</b>. The chance that somebody shares <b>one "+
    "particular person’s</b> birthday is only <b>"+d.pms+"</b>.");
}
const inp=document.getElementById("bdayn");
function read(){
  let v=parseInt(inp.value,10);
  if(!isFinite(v)) return;                    // empty or non-numeric: leave as is
  v=Math.max(NMIN,Math.min(NMAX,v));
  draw(v);
}
inp.addEventListener("input",read);
inp.addEventListener("change",function(){
  let v=parseInt(inp.value,10);
  if(!isFinite(v)) v=CLASS;                   // restore rather than break
  inp.value=Math.max(NMIN,Math.min(NMAX,v));  // clamp visibly on commit
  read();
});
draw(CLASS);
})();
</script>', XMAX, CLASS, XMAX, CLASS, rows, CLASS, EVEN, XMAX))

## ---- birthday-static
# The same two curves over the same domain as the HTML version, marked at the
# same default class size. Print cannot carry the control, so the marker is
# fixed at CLASS and the caption says the browser version moves. ASCII only in
# the annotation: the PDF device drops non-Latin-1 glyphs from plot text.
B <- BD[BD$n <= XMAX, ]
par(mar = c(4.0, 4.4, 0.6, 1.0), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = c(0, XMAX), ylim = c(0, 100), las = 1,
     xlab = "people in the room", ylab = "probability")
polygon(c(B$n, rev(B$n)), c(100 * B$p, rep(0, nrow(B))),
        col = "#C4123022", border = NA)
abline(h = c(50, 100), lty = 2, col = "#888888")
lines(B$n, 100 * B$p, col = "#C41230", lwd = 2.4)
lines(B$n, 100 * B$pmine, col = "#4a6785", lwd = 1.8, lty = 2)
# Bottom right: the only region both curves leave clear, and well away from
# the two marker labels around x = 20.
legend("bottomright", c("any two people in the room share a birthday",
                        "somebody shares one named person's birthday"),
       col = c("#C41230", "#4a6785"), lwd = c(2.4, 1.8), lty = c(1, 2),
       bty = "n", cex = 0.62, inset = c(0.01, 0.02))
dC <- B[B$n == CLASS, ]; dE <- B[B$n == EVEN, ]
segments(dC$n, 0, dC$n, 100 * dC$p, lty = 3)
points(dC$n, 100 * dC$p, pch = 19, cex = 1.1)
text(dC$n + 2.5, 100 * dC$p - 6,
     sprintf("this class: %d people, %s", dC$n, bpct(CLASS)),
     adj = 0, cex = 0.66, font = 2)
points(dE$n, 100 * dE$p, pch = 1, cex = 1.3, lwd = 1.5)
text(dE$n + 2.5, 100 * dE$p + 7,
     sprintf("even odds: %d people, %s", dE$n, bpct(EVEN)),
     adj = 0, cex = 0.62, col = "#444444")
text(XMAX - 1, 53, "even odds", adj = 1, cex = 0.6, col = "#666666")

## ---- pairs-table
# 366 is in the table because it is the only row where the probability is
# genuinely 1. Every row above it is printed as ">99.9%" rather than "100.0%"
# when it rounds that way, which is the point of including it.
np <- c(10, 20, 23, 50, 100, 366)
data.frame(
  people = nn(np),
  pairs  = nn(choose(np, 2)),
  `chance of a shared birthday` = bpct(np),
  check.names = FALSE)

## ---- cck-table
data.frame(
  quantity = c("Registration records", "Distinct first+last+DOB keys",
               "Keys held by more than one registrant",
               "Registrants sharing a key with someone else",
               "Colliding pairs", "Largest group sharing one key"),
  value = c(nn(NJN), nn(kv(CCK, "keys")), nn(kv(CCK, "dup_keys")),
            nn(kv(CCK, "flagged")), nn(kv(CCK, "pairs")),
            nn(kv(CCK, "max_group"))))

## ---- ladder-d3
# A ladder, on a log axis, because the quantity spans four orders of magnitude
# and a linear axis would render every rung but the top one as a dot on the
# spine. Labels and values are formatted once in R and passed through as
# strings: R rounds half to even and JS rounds half up, and the reader should
# not be able to tell which one drew the figure.
o <- KY[order(KY$pairs), ]
rows <- paste(sprintf('{"l":"%s","p":%d,"ps":"%s","f":"%s","n":"%s","cc":%d}',
                      o$label, o$pairs, nn(o$pairs), nn(o$flagged), o$note,
                      as.integer(o$label == CCK)), collapse = ",")
cat(sprintf('
<div id="ladder" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=330,M={t:30,r:120,b:44,l:330};
const svg=d3.select("#ladder").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([800,3e7]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.l)).range([M.t,H-M.b]).padding(0.45);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(5,"~s"));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("pairs of different records sharing the key (log scale)");
D.forEach(d=>{
  const yy=y(d.l)+y.bandwidth()/2, col=d.cc?"#C41230":"#4a6785";
  svg.append("line").attr("x1",M.l).attr("x2",x(d.p)).attr("y1",yy).attr("y2",yy)
    .attr("stroke",col).attr("stroke-width",d.cc?3:1.8).attr("opacity",0.55);
  svg.append("circle").attr("cx",x(d.p)).attr("cy",yy).attr("r",d.cc?7:5.2)
    .attr("fill",col);
  svg.append("text").attr("x",M.l-9).attr("y",yy+4).attr("text-anchor","end")
    .attr("font-size","11.5px").attr("font-weight",d.cc?"700":"400").text(d.l);
  svg.append("text").attr("x",x(d.p)+11).attr("y",yy+4).attr("font-size","11.5px")
    .attr("font-weight",d.cc?"700":"400").attr("fill",col).text(d.ps);
});
svg.append("text").attr("x",M.l).attr("y",M.t-12).attr("font-size","11px")
  .attr("fill","#C41230").attr("font-weight","600")
  .text("red: the key Crosscheck actually used");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Six rules applied to the same %s New Jersey registration records.</p>', rows, nn(NJN)))

## ---- ladder-static
o <- KY[order(KY$pairs), ]
# The upper x limit runs well past the largest bar so the widest value label
# (eight characters on the top rung) has room instead of being clipped.
par(mar = c(3.8, 17.0, 1.4, 1.0), mgp = c(2.3, 0.6, 0))
plot(NA, xlim = log10(c(800, 4e8)), ylim = c(0.5, nrow(o) + 0.5),
     axes = FALSE, xlab = "pairs sharing the key (log scale)", ylab = "")
at <- c(1e3, 1e4, 1e5, 1e6, 1e7)
axis(1, at = log10(at), labels = c("1k", "10k", "100k", "1M", "10M"),
     cex.axis = 0.7, tcl = -0.25)
cols <- ifelse(o$label == CCK, "#C41230", "#4a6785")
for (i in seq_len(nrow(o))) {
  segments(log10(800), i, log10(o$pairs[i]), i, col = cols[i],
           lwd = if (o$label[i] == CCK) 3 else 1.8)
  points(log10(o$pairs[i]), i, pch = 19, col = cols[i],
         cex = if (o$label[i] == CCK) 1.5 else 1.1)
  mtext(o$label[i], side = 2, at = i, las = 1, cex = 0.58, adj = 1, line = 0.4,
        font = if (o$label[i] == CCK) 2 else 1)
  text(log10(o$pairs[i]) + 0.12, i, nn(o$pairs[i]), adj = 0, cex = 0.6,
       col = cols[i], font = if (o$label[i] == CCK) 2 else 1)
}
mtext("red: the key Crosscheck actually used", side = 3, line = 0.2,
      adj = 0, cex = 0.6, col = "#C41230")

## ---- xc-table
data.frame(
  quantity = CC$quantity,
  value = nn(CC$value))

## ---- scaling-d3
# Real subsamples of the real file, not a model: draw n records at random,
# count the collisions that actually occur, repeat. Zero-collision draws are
# kept in the data but the log axis starts above them.
s <- SC[SC$pairs > 0, ]
rows <- paste(sprintf('{"n":%d,"r":%.3f,"p":%.0f,"ns":"%s","rs":"%s","psx":"%s"}',
                      s$n, s$per100k, s$pairs, nn(s$n), pc(s$per100k, 1),
                      nn(round(s$pairs))), collapse = ",")
cat(sprintf('
<div id="scal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], SMALLN=%d;
const W=760,H=360,M={t:26,r:24,b:46,l:60};
const svg=d3.select("#scal").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLog().domain([1e5,8e6]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,52]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(5,"~s"));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("records in the pool being searched (log scale)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",15).attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("false matches per 100,000 records");
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.4).attr("d",d3.line().x(d=>x(d.n)).y(d=>y(d.r)));
const tip=d3.select("#scal").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.n)).attr("cy",d=>y(d.r)).attr("r",5).attr("fill","#C41230")
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.ns+" records</b><br>"+d.psx+
      " colliding pairs<br>"+d.rs+" per 100,000")
      .style("left",Math.min(d3.pointer(e,this.parentNode)[0]+14,W-220)+"px")
      .style("top",(d3.pointer(e,this.parentNode)[1]-6)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
const a=D.find(d=>d.n===SMALLN), b=D[D.length-1];
svg.append("text").attr("x",x(b.n)).attr("y",y(b.r)-14).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("font-weight","600")
  .text("whole state: "+b.rs+" per 100,000");
svg.append("circle").attr("cx",x(a.n)).attr("cy",y(a.r)).attr("r",5.5)
  .attr("fill","none").attr("stroke","#111").attr("stroke-width",1.6);
svg.append("text").attr("x",x(a.n)+10).attr("y",y(a.r)-9).attr("font-size","11.5px")
  .attr("fill","#444").text("one large county: "+a.rs+" per 100,000");
})();
</script>', rows, SMALL_N))

## ---- scaling-static
s <- SC[SC$pairs > 0, ]
par(mar = c(4.0, 4.6, 1.0, 1.2), mgp = c(2.6, 0.7, 0))
plot(log10(s$n), s$per100k, type = "o", pch = 19, col = "#C41230", lwd = 2.2,
     cex = 0.9, axes = FALSE, ylim = c(0, 52),
     xlab = "records in the pool being searched (log scale)",
     ylab = "false matches per 100,000 records")
at <- c(1e5, 3e5, 1e6, 3e6, 1e7)
axis(1, at = log10(at), labels = c("100k", "300k", "1M", "3M", "10M"),
     cex.axis = 0.75, tcl = -0.25)
axis(2, las = 1, cex.axis = 0.75, tcl = -0.25)
box(bty = "l")
b <- s[nrow(s), ]; a <- s[s$n == SMALL_N, ]
text(log10(b$n), b$per100k - 3.6, sprintf("whole state: %s per 100,000", pc(b$per100k)),
     adj = 1, cex = 0.66, font = 2)
points(log10(a$n), a$per100k, pch = 1, cex = 1.8, lwd = 1.4)
# Anchored to the right of the text and set well above the marker, so the
# label clears the curve rising to its right.
text(log10(a$n) - 0.04, a$per100k + 7.5,
     sprintf("one large county:\n%s per 100,000", pc(a$per100k)),
     adj = 1, cex = 0.66, col = "#444444")

## ---- calendar-d3
# A year is a cycle, so it is drawn as one: 366 spokes, one per calendar date,
# length proportional to the number of registrants carrying it. Radii are
# computed in R.
MD$doy <- seq_len(nrow(MD))
expd <- sum(MD$n) / nrow(MD)
rows <- paste(sprintf('{"d":%d,"n":%d,"md":"%s","ns":"%s"}',
                      MD$doy, MD$n, MD$monthday, nn(MD$n)), collapse = ",")
cat(sprintf('
<div id="cal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], EXP=%f, MAXN=%d;
const W=760,H=430,CX=W/2,CY=H/2,R0=70,R1=185;
const svg=d3.select("#cal").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const r=d3.scaleLinear().domain([0,MAXN]).range([R0,R1]);
const ang=d=>(d.d-1)/366*2*Math.PI-Math.PI/2;
const g=svg.append("g").attr("transform","translate("+CX+","+CY+")");
const tip=d3.select("#cal").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
g.selectAll("line.s").data(D).join("line").attr("class","s")
  .attr("x1",d=>R0*Math.cos(ang(d))).attr("y1",d=>R0*Math.sin(ang(d)))
  .attr("x2",d=>r(d.n)*Math.cos(ang(d))).attr("y2",d=>r(d.n)*Math.sin(ang(d)))
  .attr("stroke",d=>d.md==="01-01"?"#C41230":"#4a6785")
  .attr("stroke-width",d=>d.md==="01-01"?3:1.5)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.md+"</b><br>"+d.ns+" registrants")
      .style("left",Math.min(d3.pointer(e,svg.node())[0]+14,W-190)+"px")
      .style("top",(d3.pointer(e,svg.node())[1]-6)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
// The even-spread ring is drawn AFTER the spokes so it reads on top of them
// rather than being buried in the mass.
g.append("circle").attr("r",r(EXP)).attr("fill","none").attr("stroke","#333")
  .attr("stroke-width",1.2).attr("stroke-dasharray","4,3").attr("pointer-events","none");
// Jan is skipped here: the 1 January callout already labels that spoke, and
// the two collided when both were drawn.
["","Apr","Jul","Oct"].forEach((m,i)=>{
  if(!m) return;
  const a=i/4*2*Math.PI-Math.PI/2;
  g.append("text").attr("x",(R1+18)*Math.cos(a)).attr("y",(R1+18)*Math.sin(a)+4)
    .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#666").text(m);
});
const j=D[0];
g.append("text").attr("x",10).attr("y",-r(j.n)-8).attr("font-size","12px")
  .attr("font-weight","700").attr("fill","#C41230")
  .text("1 January: "+j.ns);
g.append("text").attr("x",0).attr("y",-4).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#666").text("dashed ring =");
g.append("text").attr("x",0).attr("y",10).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#666").text("an even spread");
})();
</script>', rows, expd, max(MD$n)))

## ---- calendar-static
MD$doy <- seq_len(nrow(MD))
expd <- sum(MD$n) / nrow(MD)
R0 <- 0.42; R1 <- 1.0
rr <- R0 + (R1 - R0) * MD$n / max(MD$n)
# SVG measures y downwards and R measures it upwards, so the D3 version's
# angle formula would put 1 January at the BOTTOM here. Negating it puts the
# year the same way round in both renderings: 1 January at the top, running
# clockwise through Apr on the right, Jul at the bottom, Oct on the left.
aa <- pi / 2 - (MD$doy - 1) / nrow(MD) * 2 * pi
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(-1.3, 1.3), ylim = c(-1.25, 1.25), asp = 1, axes = FALSE, ann = FALSE)
th <- seq(0, 2 * pi, length.out = 400)
re <- R0 + (R1 - R0) * expd / max(MD$n)
cols <- ifelse(MD$monthday == "01-01", "#C41230", "#4a6785")
segments(R0 * cos(aa), R0 * sin(aa), rr * cos(aa), rr * sin(aa),
         col = cols, lwd = ifelse(MD$monthday == "01-01", 2.6, 0.8))
# Ring on top of the spokes, not under them.
lines(re * cos(th), re * sin(th), lty = 2, col = "#333333", lwd = 1.1)
# Jan is skipped: the 1 January callout labels that spoke already.
for (i in 1:3) {
  a <- pi / 2 - i / 4 * 2 * pi
  text(1.16 * cos(a), 1.16 * sin(a), c("Apr", "Jul", "Oct")[i],
       cex = 0.68, col = "#666666")
}
text(0.08, rr[1] + 0.07, sprintf("1 January: %s", nn(MD$n[1])), adj = 0,
     cex = 0.7, font = 2, col = "#C41230")
text(0, 0.03, "dashed ring =", cex = 0.62, col = "#666666")
text(0, -0.06, "an even spread", cex = 0.62, col = "#666666")

## ---- years-d3
# A needle plot on a log axis, because the quantity to be shown spans from one
# record to well over a hundred thousand and the interesting features live at
# both ends. The smooth is a loess through the plausible range only: the null
# for an electorate's birth years is an age structure, not a flat line, so a
# departure only counts as an anomaly if it departs from a smooth curve.
YB <- read.csv("data/derived/birthyears.csv", stringsAsFactors = FALSE)
fit <- loess(log10(n) ~ year, YB[YB$year >= 1930 & YB$year <= 2005, ], span = 0.4)
YB$sm <- NA
ok <- YB$year >= 1930 & YB$year <= 2005
YB$sm[ok] <- predict(fit, YB$year[ok])
rows <- paste(sprintf('{"y":%d,"n":%d,"ns":"%s","s":%s}', YB$year, YB$n, nn(YB$n),
                      ifelse(is.na(YB$sm), "null", sprintf("%.4f", YB$sm))),
              collapse = ",")
cat(sprintf('
<div id="yrs" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], SENTN=%d, SENTY=%d;
const W=760,H=340,M={t:52,r:18,b:42,l:56};
const svg=d3.select("#yrs").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1899,2010]).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([1,200000]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(5,"~s"));
svg.append("text").attr("x",(W+M.l)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("birth year on the registration record");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("registrants (log scale)");
const tip=d3.select("#yrs").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("line.n").data(D).join("line").attr("class","n")
  .attr("x1",d=>x(d.y)).attr("x2",d=>x(d.y)).attr("y1",y(1)).attr("y2",d=>y(Math.max(d.n,1)))
  .attr("stroke","#4a6785").attr("stroke-width",3.4)
  .on("mousemove",function(e,d){
    tip.style("opacity",1).html("<b>"+d.y+"</b><br>"+d.ns+" registrants<br>age "+(2026-d.y))
      .style("left",Math.min(d3.pointer(e,svg.node())[0]+14,W-190)+"px")
      .style("top",(d3.pointer(e,svg.node())[1]-6)+"px");
  }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("path").datum(D.filter(d=>d.s!==null)).attr("fill","none")
  .attr("stroke","#C41230").attr("stroke-width",2).attr("stroke-dasharray","5,3")
  .attr("d",d3.line().x(d=>x(d.y)).y(d=>y(Math.pow(10,d.s))));
// The placeholder sits off the left of this axis; say so rather than
// rescaling the whole figure around one bar.
const ax=M.l+6;
svg.append("line").attr("x1",ax).attr("x2",ax).attr("y1",y(1)).attr("y2",M.t-6)
  .attr("stroke","#C41230").attr("stroke-width",3.4);
svg.append("text").attr("x",ax+8).attr("y",M.t-24).attr("font-size","11.5px")
  .attr("font-weight","700").attr("fill","#C41230")
  .text("year "+SENTY+": "+SENTN.toLocaleString()+" records");
svg.append("text").attr("x",ax+8).attr("y",M.t-10).attr("font-size","11px")
  .attr("fill","#666").text("off the left of this axis");
svg.append("text").attr("x",W-M.r).attr("y",M.t-24).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#C41230")
  .text("dashed: a smooth age structure");
})();
</script>', rows, as.integer(fx("sent_n")), as.integer(fx("sent_year"))))

## ---- years-static
YB <- read.csv("data/derived/birthyears.csv", stringsAsFactors = FALSE)
ok <- YB$year >= 1930 & YB$year <= 2005
fit <- loess(log10(n) ~ year, YB[ok, ], span = 0.4)
par(mar = c(3.9, 4.6, 2.6, 1.0), mgp = c(2.5, 0.7, 0))
plot(NA, xlim = c(1899, 2010), ylim = log10(c(1, 200000)), axes = FALSE,
     xlab = "birth year on the registration record",
     ylab = "registrants (log scale)")
axis(1, at = seq(1900, 2010, 20), cex.axis = 0.78, tcl = -0.25)
axis(2, at = 0:5, labels = c("1", "10", "100", "1k", "10k", "100k"),
     las = 1, cex.axis = 0.78, tcl = -0.25)
box(bty = "l")
segments(YB$year, 0, YB$year, log10(pmax(YB$n, 1)), col = "#4a6785", lwd = 2.2)
lines(YB$year[ok], predict(fit, YB$year[ok]), col = "#C41230", lwd = 2, lty = 2)
segments(1899.5, 0, 1899.5, log10(150000), col = "#C41230", lwd = 2.6)
text(1903, log10(150000), sprintf("year %s: %s records, off the left of this axis",
     fx("sent_year"), nn(fx("sent_n"))), adj = 0, cex = 0.62, font = 2, col = "#C41230")
text(2008, log10(150000), "dashed: a smooth age structure", adj = 1, cex = 0.62,
     col = "#C41230")

## ---- sentinel-table
data.frame(
  quantity = c("Records carrying the placeholder date",
               "...as a share of the file",
               "Colliding pairs inside that group",
               "...as a share of all colliding pairs statewide",
               "Colliding pairs in 20 same-sized draws of real dates (mean)",
               "...the worst of those 20 draws"),
  value = c(nn(fx("sent_n")), paste0(fx("sent_pct_records"), "%"),
            nn(fx("sent_pairs")), paste0(fx("sent_pct_pairs"), "%"),
            fx("real_draw_mean"), fx("real_draw_max")))

## ---- fom-table
data.frame(
  file = c("New Jersey registration records, this chapter",
           "National 2012 vote records, Goel et al.",
           "If dates of birth were spread evenly"),
  `first-of-the-month share` = c(pct100(fx("fom_pct"), 2),
                                 paste0(fx("natl_fom_pct"), "%"),
                                 pct100(fx("fom_expected_pct"), 2)),
  check.names = FALSE)

## ---- goel-table
# EVERY ROW NAMES ITS OWN DENOMINATOR. The rows below are not all fractions of
# the same thing -- rows 2-5 are subsets of the Iowa pairings, row 6 is a
# national rate per voter -- and a reader scanning the column would otherwise
# reasonably read the last row as "one in 4,000 of the flagged pairs", which is
# a different and much smaller claim than the one the authors make. Asking
# "out of what?" is the whole subject of this chapter.
data.frame(
  finding = c(
    "Iowa pairings Crosscheck flagged, SSN4 known for both records",
    "of those Iowa pairings, the share that really were one person",
    "of those confirmed duplicates, the number used to vote twice in 2012",
    "of those Iowa pairings, pairs where both voted and the SSN4 matched",
    "of those Iowa pairings, pairs where both voted and the SSN4 differed",
    "double voters per voter nationally in 2012, all US voters as the base"),
  value = c(fx("cc_known_total"), paste0(fx("cc_true_dup_share"), "%"),
            fx("cc_dup_voted_twice"), fx("t1_both_same_ssn"),
            fx("t1_both_diff_ssn"), fx("dbl_rate")))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#bday text[fill="#666" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: on the dark page those fills are lifted or pinned and
# already pass, and a --paper stroke would sit dark behind a dark ink there,
# because the checker scores the fill against the stroke it touches. The
# unfilled selector catches the one label that never chose a colour and rides
# the floor rule to currentColor.
cat('<style>
#bday text[fill="#444" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#bday text[fill="#666" i],
#bday text[fill="currentcolor" i],
#bday text:not([fill])
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
