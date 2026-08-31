# scdb-code.R -- chunk bodies for scdb-brief.Rmd
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

ag <- read.csv("data/derived/agreement.csv",   stringsAsFactors = FALSE)
jj <- read.csv("data/derived/justices.csv",    stringsAsFactors = FALSE)
bt <- read.csv("data/derived/by_term.csv",     stringsAsFactors = FALSE)
cc <- read.csv("data/derived/close_cases.csv", stringsAsFactors = FALSE)

js   <- sort(unique(c(ag$a, ag$b)))
NJ   <- length(js); NPAIR <- nrow(ag)
HOLE <- ag[ag$cases == 0, ]

# --- the scaling, with the hole filled -------------------------------------
mat <- function(a) {
  m <- matrix(NA_real_, NJ, NJ, dimnames = list(js, js)); diag(m) <- 1
  for (i in seq_len(nrow(a))) {
    if (a$cases[i] == 0) next
    r <- a$agree[i] / a$cases[i]
    m[a$a[i], a$b[i]] <- r; m[a$b[i], a$a[i]] <- r
  }
  m
}
M    <- mat(ag)
FILL <- mean(M[upper.tri(M)], na.rm = TRUE)
M[is.na(M)] <- FILL
score <- cmdscale(as.dist(1 - M), k = 1)[, 1]
ord   <- sort(score)
R <- cor(score[jj$justice], jj$pct_conservative)

# --- the scaling, refusing to guess ----------------------------------------
OUT <- c(HOLE$a, HOLE$b)
a2  <- ag[!(ag$a %in% OUT | ag$b %in% OUT), ]
js2 <- sort(unique(c(a2$a, a2$b)))
M2  <- matrix(NA_real_, length(js2), length(js2), dimnames = list(js2, js2))
diag(M2) <- 1
for (i in seq_len(nrow(a2))) {
  r <- a2$agree[i] / a2$cases[i]
  M2[a2$a[i], a2$b[i]] <- r; M2[a2$b[i], a2$a[i]] <- r
}
score2 <- cmdscale(as.dist(1 - M2), k = 1)[, 1]
ord2   <- sort(score2)
R2 <- cor(score2[js2], jj$pct_conservative[match(js2, jj$justice)])

# which adjacent pairs swapped
keep  <- names(ord)[names(ord) %in% js2]
swaps <- which(keep != names(ord2))

# blocs
CONS  <- names(ord)[1:6]; LIBS  <- names(ord)[7:NJ]
SPAN  <- ord[6] - ord[1]
CHASM <- ord[7] - ord[6]

# --- close cases -----------------------------------------------------------
inmaj <- table(unlist(strsplit(cc$majority_bloc, " ")))
jj$close_maj <- as.vector(inmaj[jj$justice])
# Sat for the whole run of one-vote decisions. Only Breyer and Jackson did not:
# Barrett (222 coded cases) was confirmed before the first of them was handed
# down, so her denominator is the same 30 as everyone else's.
FULL <- jj$cases >= 200
jj$close_pct <- ifelse(FULL, 100 * jj$close_maj / nrow(cc), NA)

nm <- function(x, k = 3) formatC(x, format = "f", digits = k)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
nice <- function(x) {                        # ACBarrett -> Barrett
  sub("^[A-Z]+(?=[A-Z][a-z])", "", x, perl = TRUE)
}

# describe the swaps as adjacent pairs rather than as loose positions
swap_txt <- local({
  out <- character(0); i <- 1
  while (i < length(keep)) {
    if (keep[i] != names(ord2)[i] && keep[i] == names(ord2)[i + 1]) {
      out <- c(out, paste(nice(keep[i]), "and", nice(keep[i + 1]))); i <- i + 2
    } else i <- i + 1
  }
  out
})

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- rawscdb
# A verbatim capture of the download's header and of the first justice-vote
# row, read UTF-8. (The build script reads it UTF-8 too, and says why: the
# apostrophe in DOBBS v. JACKSON WOMEN'S HEALTH ORGANIZATION is three bytes,
# and reading the file as latin1 turns it into WOMENaEUR(tm)S in silence.)
HD <- paste0(
 "caseId,docketId,caseIssuesId,voteId,dateDecision,decisionType,usCite,",
 "sctCite,ledCite,lexisCite,term,naturalCourt,chief,docket,caseName,",
 "dateArgument,dateRearg,petitioner,petitionerState,respondent,",
 "respondentState,jurisdiction,adminAction,adminActionState,threeJudgeFdc,",
 "caseOrigin,caseOriginState,caseSource,caseSourceState,lcDisagreement,",
 "certReason,lcDisposition,lcDispositionDirection,declarationUncon,",
 "caseDisposition,caseDispositionUnusual,partyWinning,precedentAlteration,",
 "voteUnclear,issue,issueArea,decisionDirection,decisionDirectionDissent,",
 "authorityDecision1,authorityDecision2,lawType,lawSupp,lawMinor,",
 "majOpinWriter,majOpinAssigner,splitVote,majVotes,minVotes,justice,",
 "justiceName,vote,opinion,direction,majority,firstAgreement,secondAgreement")
VT <- c(
"term                1946",
"caseName            HALLIBURTON OIL WELL CEMENTING CO. v. WALKER ...",
"issueArea           8",
"decisionDirection   2",
"majVotes            8",
"minVotes            1",
"justiceName         HHBurton",
"vote                2",
"opinion             1",
"direction           1",
"majority            1")
NCOL_SCDB <- 1L + sum(strsplit(HD, "", fixed = TRUE)[[1]] == ",")
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
# Sixty-one names read down the page, each with what the Database's codebook
# says it holds. The paragraph below asks the reader to sort them into two
# piles; the descriptions are what make that possible, and they deliberately do
# not do the sorting.
.nm <- strsplit(HD, ",", fixed = TRUE)[[1]]
.d <- c(
 caseId = "identifier for the case",
 docketId = "identifier for the case as docketed",
 caseIssuesId = "identifier for one legal issue within the case",
 voteId = "identifier for one justice's vote — the grain of this file",
 dateDecision = "the day the decision came down",
 decisionType = "how the Court disposed of it: signed opinion, per curiam, and so on",
 usCite = "citation in the United States Reports",
 sctCite = "citation in the Supreme Court Reporter",
 ledCite = "citation in the Lawyers' Edition",
 lexisCite = "citation in LEXIS",
 term = "the Court's term, which begins in October",
 naturalCourt = "which stable set of nine justices was sitting",
 chief = "the Chief Justice at the time",
 docket = "the docket number the Court assigned",
 caseName = "the case's name",
 dateArgument = "the day it was argued",
 dateRearg = "the day it was reargued, if it was",
 petitioner = "coded category of who brought the case",
 petitionerState = "the petitioner's state, where one applies",
 respondent = "coded category of who was sued",
 respondentState = "the respondent's state, where one applies",
 jurisdiction = "how the case reached the Court — appeal, certiorari, and so on",
 adminAction = "the federal agency below, if any",
 adminActionState = "that agency's state, if any",
 threeJudgeFdc = "whether a three-judge district court heard it",
 caseOrigin = "the court where the case began",
 caseOriginState = "that court's state",
 caseSource = "the court the Court took it from",
 caseSourceState = "that court's state",
 lcDisagreement = "whether the record shows disagreement in the court below",
 certReason = "why the Court agreed to hear it — a reason somebody had to read for",
 lcDisposition = "what the court below did",
 lcDispositionDirection = "whether the decision below was liberal or conservative",
 declarationUncon = "whether a law was declared unconstitutional",
 caseDisposition = "what this Court did with it",
 caseDispositionUnusual = "whether the disposition was out of the ordinary",
 partyWinning = "whether the petitioner won",
 precedentAlteration = "whether the Court altered its own precedent",
 voteUnclear = "whether the vote itself was hard to code",
 issue = "the specific legal issue, from a coded list",
 issueArea = "the broad area that issue falls in",
 decisionDirection = "whether the decision was liberal or conservative",
 decisionDirectionDissent = "whether the dissent ran the other way",
 authorityDecision1 = "the basis the Court decided on",
 authorityDecision2 = "a second basis, where there was one",
 lawType = "the kind of law at issue — constitutional, statutory, other",
 lawSupp = "the specific provision",
 lawMinor = "the provision in the Court's own words",
 majOpinWriter = "who wrote the majority opinion",
 majOpinAssigner = "who assigned it",
 splitVote = "whether the vote was recorded in more than one way",
 majVotes = "how many justices were in the majority",
 minVotes = "how many were not",
 justice = "the justice's numeric ID",
 justiceName = "the justice's name",
 vote = "how this justice voted, as a code",
 opinion = "whether this justice wrote an opinion",
 direction = "whether this justice's vote was liberal or conservative",
 majority = "whether this justice was in the majority",
 firstAgreement = "a justice this one joined",
 secondAgreement = "a second justice this one joined")
data.frame(Position = seq_along(.nm),
           Column_name = .nm,
           What_the_codebook_says = unname(.d[.nm]))

## ---- rawscdb2
# VT is already one field per line; splitting on the run of spaces that
# separates name from value turns it into the two columns it always was.
.m <- regmatches(VT, regexec("^(\\S+)\\s{2,}(.*)$", VT))
data.frame(Field = vapply(.m, function(z) z[2], character(1)),
           Value = trimws(vapply(.m, function(z) z[3], character(1))))

## ---- cleanag
head(ag[order(-ag$agree / pmax(ag$cases, 1)), ], 3)

## ---- justices
o <- jj[order(-jj$pct_conservative),
        c("justice", "cases", "pct_conservative", "pct_in_majority")]
o$justice <- nice(o$justice)
names(o) <- c("justice", "cases with a coded direction",
              "% of votes coded conservative", "% of the time in the majority")
o

## ---- agreement
o <- rbind(head(ag[order(-ag$pct), ], 3), head(ag[order(ag$pct), ], 3))
o <- o[, c("a", "b", "agree", "cases", "pct")]
o$a <- nice(o$a); o$b <- nice(o$b)
names(o) <- c("justice", "justice", "voted the same way", "shared cases",
              "% agreement")
o

## ---- heat-prep
RAW  <- mat(ag)                    # NA where the pair never sat together
LO   <- min(RAW[upper.tri(RAW)], na.rm = TRUE)
HI   <- max(RAW[upper.tri(RAW)], na.rm = TRUE)
SHADE <- colorRampPalette(c("#f7f7f7", "#252525"))(100)
gcol  <- function(v) SHADE[pmax(1, pmin(100, round(1 + 99 * (v - LO) / (HI - LO))))]
FILE_ORD <- js                     # the order the file happens to be in
SCAL_ORD <- names(ord)             # the order the scaling below will produce
HOLEA <- HOLE$a; HOLEB <- HOLE$b

## ---- heat-d3
# ---------------------------------------------------------------------------
# The same 45 rates the table above is drawn from, as a matrix, in two orders:
# the file's own and the one the scaling recovers. Colors are computed in R by
# gcol() and passed through as hex, so the browser and the PDF device shade the
# identical cell identically.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
cells <- character(0)
for (i in seq_len(NJ)) for (k in seq_len(NJ)) {
  v <- RAW[FILE_ORD[i], FILE_ORD[k]]
  cells <- c(cells, paste0('{"a":"', nice(FILE_ORD[i]), '","b":"',
    nice(FILE_ORD[k]), '","v":', ifelse(is.na(v), "null",
      formatC(100 * v, format = "f", digits = 1)),
    ',"c":"', ifelse(is.na(v), "#ffffff", gcol(v)), '"}'))
}
key <- paste0('{"p":', formatC(seq(0, 1, length.out = 40), format = "f", digits = 3),
              ',"c":"', SHADE[round(seq(1, 100, length.out = 40))], '"}',
              collapse = ",")
cat(paste0('
<div id="hm" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="hO" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">the order the file is in</button>
  <button id="hS" style="font:12px inherit;padding:4px 10px;cursor:pointer">re-sorted by the recovered order</button>
 </div>
</div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const C=[', paste(cells, collapse = ","), '];
const KEY=[', key, '];
const A=[', paste0('"', nice(FILE_ORD), '"', collapse = ","), '];
const B=[', paste0('"', nice(SCAL_ORD), '"', collapse = ","), '];
const LO=', formatC(100 * LO, format = "f", digits = 1),
    ', HI=', formatC(100 * HI, format = "f", digits = 1), ';
const W=770,H=430,M={t:96,r:196,b:24,l:104};
const svg=d3.select("#hm").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const s=d3.scaleBand().range([M.l,W-M.r]).padding(0.06);
const sy=d3.scaleBand().range([M.t,H-M.b]).padding(0.06);
const g=svg.append("g"), gx=svg.append("g"), gy=svg.append("g");
const tip=d3.select("#hm").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
function draw(ord){
  s.domain(ord); sy.domain(ord);
  g.selectAll("rect").data(C,d=>d.a+"|"+d.b).join(
    e=>e.append("rect").attr("stroke","#fff"),u=>u,x=>x.remove())
   .attr("x",d=>s(d.a)).attr("y",d=>sy(d.b))
   .attr("width",s.bandwidth()).attr("height",sy.bandwidth())
   .attr("fill",d=>d.a===d.b?"#e8e8e8":d.c)
   .attr("stroke",d=>d.v===null?"#C41230":"#fff")
   .attr("stroke-width",d=>d.v===null?2:1)
   .on("mousemove",function(ev,d){ tip.style("opacity",1)
     .html(d.a===d.b?("<b>"+d.a+"</b>"):
       (d.a+" and "+d.b+"<br>"+(d.v===null?"never sat together":d.v+"% agreement")))
     .style("left",Math.min(ev.offsetX+14,W-210)+"px").style("top",(ev.offsetY-6)+"px");})
   .on("mouseleave",()=>tip.style("opacity",0));
  gx.selectAll("text").data(ord,d=>d).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#333"),u=>u,x=>x.remove())
   .attr("transform",d=>"translate("+(s(d)+s.bandwidth()/2+4)+","+(M.t-8)+") rotate(-55)")
   .text(d=>d);
  gy.selectAll("text").data(ord,d=>d).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#333").attr("text-anchor","end"),
    u=>u,x=>x.remove())
   .attr("x",M.l-8).attr("y",d=>sy(d)+sy.bandwidth()/2+4).text(d=>d);
}
draw(A);
d3.select("#hO").on("click",()=>draw(A));
d3.select("#hS").on("click",()=>draw(B));
const kx=W-M.r+42, kt=M.t+18, kh=200;
svg.selectAll("rect.k").data(KEY).join("rect").attr("class","k")
  .attr("x",kx).attr("y",(d,i)=>kt+kh-(i+1)*(kh/KEY.length))
  .attr("width",16).attr("height",kh/KEY.length+0.6).attr("fill",d=>d.c);
svg.append("text").attr("x",kx+22).attr("y",kt+9).attr("font-size","11px").text(HI+"% agreement");
svg.append("text").attr("x",kx+22).attr("y",kt+kh).attr("font-size","11px").text(LO+"%");
svg.append("rect").attr("x",kx).attr("y",kt+kh+22).attr("width",16).attr("height",16)
  .attr("fill","#fff").attr("stroke","#C41230").attr("stroke-width",2);
svg.append("text").attr("x",kx+22).attr("y",kt+kh+34).attr("font-size","11px")
  .attr("fill","#C41230").text("never sat together");
svg.append("rect").attr("x",kx).attr("y",kt+kh+48).attr("width",16).attr("height",16)
  .attr("fill","#e8e8e8").attr("stroke","#fff");
svg.append("text").attr("x",kx+22).attr("y",kt+kh+60).attr("font-size","11px")
  .attr("fill","#555").text("a justice with herself");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover any cell for the pair and the number. The second button re-sorts the same
', NPAIR, ' numbers into the order recovered further down.</p>'))

## ---- heat-static
panel <- function(o, main) {
  plot(NA, xlim = c(0.5, NJ + 0.5), ylim = c(NJ + 0.5, 0.5), axes = FALSE,
       xlab = "", ylab = "")
  mtext(main, side = 3, line = 3.4, cex = 0.8, font = 2)
  for (i in seq_len(NJ)) for (k in seq_len(NJ)) {
    v <- RAW[o[i], o[k]]
    rect(i - 0.5, k - 0.5, i + 0.5, k + 0.5,
         col = if (i == k) "#e8e8e8" else if (is.na(v)) "#ffffff" else gcol(v),
         border = if (is.na(v)) "#C41230" else "#ffffff",
         lwd = if (is.na(v)) 2 else 1)
  }
  axis(2, at = seq_len(NJ), labels = nice(o), las = 1, tick = FALSE,
       cex.axis = 0.58, line = -0.8)
  axis(3, at = seq_len(NJ), labels = nice(o), las = 2, tick = FALSE,
       cex.axis = 0.58, line = -0.8)
}
par(mfrow = c(1, 2), mar = c(2.6, 4.2, 5.4, 0.6))
panel(FILE_ORD, "the order the file is in")
panel(SCAL_ORD, "re-sorted by the recovered order")
par(mfrow = c(1, 1))
# legend: an absolute scale, drawn once, in the bottom margin
par(new = TRUE, mar = c(0.4, 4.2, 0, 0.6), fig = c(0, 1, 0, 0.11))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
for (i in 1:100) rect((i - 1) / 100 * 0.30, 0.45, i / 100 * 0.30, 0.85,
                      col = SHADE[i], border = NA)
text(0, 0.28, paste0(pc(100 * LO), "% agreement"), adj = 0, cex = 0.62)
text(0.30, 0.28, paste0(pc(100 * HI), "%"), adj = 1, cex = 0.62)
rect(0.42, 0.45, 0.46, 0.85, col = "white", border = "#C41230", lwd = 2)
text(0.48, 0.65, "never sat together", adj = 0, cex = 0.62, col = "#C41230")
rect(0.72, 0.45, 0.76, 0.85, col = "#e8e8e8", border = "white")
text(0.78, 0.65, "a justice with herself", adj = 0, cex = 0.62, col = "#555555")

## ---- scaling
o <- data.frame(justice = nice(names(ord)),
                position = nm(as.vector(ord)),
                stringsAsFactors = FALSE)
o

## ---- clusters
data.frame(
  quantity = c(paste0("Span of the ", length(CONS), " on one side"),
               paste0("Span of the ", length(LIBS), " on the other"),
               "Distance across the gap between them"),
  value = c(nm(SPAN), nm(ord[NJ] - ord[7]), nm(CHASM)))

## ---- hole
o <- HOLE[, c("a", "b", "agree", "cases", "pct")]
o$a <- nice(o$a); o$b <- nice(o$b); o$pct <- "undefined"
names(o) <- c("justice", "justice", "voted the same way", "shared cases",
              "% agreement")
o

## ---- fill
data.frame(
  quantity = c("Pairs with a real agreement rate", "Pairs with none",
               "Value used to fill the gap",
               "What that value is"),
  value = c(NPAIR - nrow(HOLE), nrow(HOLE), nm(FILL),
            "the mean agreement across every other pair"))

## ---- refuse
o <- data.frame(rank = seq_along(ord2),
                `with the gap filled` = nice(keep),
                `refusing to guess`   = nice(names(ord2)),
                changed = ifelse(keep == names(ord2), "", "swapped"),
                check.names = FALSE)
o

## ---- d3-line
rA <- paste(sprintf('{"k":"%s","v":%.4f}', nice(names(ord)), as.vector(ord)),
            collapse = ",")
rB <- paste(sprintf('{"k":"%s","v":%.4f}', nice(names(ord2)), as.vector(ord2)),
            collapse = ",")
cat(sprintf('
<div id="scd" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="sA" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">%d justices, gap filled</button>
  <button id="sB" style="font:12px inherit;padding:4px 10px;cursor:pointer">%d justices, refusing to guess</button>
 </div>
</div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const A=[%s], B=[%s];
const W=760,H=190,M={t:60,r:40,b:44,l:40};
const svg=d3.select("#scd").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-0.30,0.32]).range([M.l,W-M.r]);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",M.t).attr("y2",M.t)
  .attr("stroke","#bbb");
svg.append("g").attr("transform",`translate(0,${M.t+18})`)
  .call(d3.axisBottom(x).ticks(7));
svg.append("text").attr("x",M.l).attr("y",H-8).attr("font-size","11px")
  .attr("fill","#666").text("position recovered from agreement alone; the direction of this axis is arbitrary");
const pts=svg.append("g"), labs=svg.append("g");
function draw(d,color){
  pts.selectAll("circle").data(d,q=>q.k).join(
    e=>e.append("circle").attr("cy",M.t).attr("r",7).attr("cx",q=>x(q.v)),
    u=>u, ex=>ex.remove())
    .transition().duration(750).attr("cx",q=>x(q.v)).attr("fill",color);
  labs.selectAll("text").data(d,q=>q.k).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#333")
        .attr("transform",q=>`translate(${x(q.v)},${M.t-14}) rotate(-45)`),
    u=>u, ex=>ex.remove())
    .text(q=>q.k)
    .transition().duration(750)
    .attr("transform",q=>`translate(${x(q.v)},${M.t-14}) rotate(-45)`);
}
draw(A,"#2c7fb8");
d3.select("#sA").on("click",()=>draw(A,"#2c7fb8"));
d3.select("#sB").on("click",()=>draw(B,"#C41230"));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Toggle to see what one missing cell does.</p>
', NJ, length(js2), rA, rB))

## ---- line-static
par(mfrow = c(2, 1), mar = c(3.2, 1, 2.2, 1))
for (z in list(list(ord, paste(NJ, "justices, gap filled"), "#2c7fb8"),
               list(ord2, paste(length(js2), "justices, refusing to guess"), "#C41230"))) {
  plot(z[[1]], rep(0, length(z[[1]])), pch = 19, cex = 1.5, col = z[[3]],
       yaxt = "n", ylab = "", xlab = "", ylim = c(-1, 1),
       xlim = c(-0.30, 0.32), main = z[[2]])
  text(z[[1]], 0.35, nice(names(z[[1]])), srt = 45, adj = 0, cex = 0.7)
  abline(h = 0, col = "grey80")
}
par(mfrow = c(1, 1))
mtext("the direction of this axis is arbitrary: only order and spacing mean anything",
      side = 1, line = 2.0, cex = 0.66, col = "#777777")

## ---- majority
o <- jj[order(-jj$pct_in_majority), c("justice", "pct_conservative", "pct_in_majority")]
o$justice <- nice(o$justice)
names(o) <- c("justice", "% of votes coded conservative",
              "% of the time in the majority")
o

## ---- close
o <- jj[FULL, ]
o <- o[order(-o$close_pct), c("justice", "pct_in_majority", "close_maj", "close_pct")]
o$justice <- nice(o$justice)
o$pct_in_majority <- pc(o$pct_in_majority); o$close_pct <- pc(o$close_pct)
names(o) <- c("justice", "% in majority, all cases",
              paste0("in majority, of the ", nrow(cc), " one-vote cases"),
              "% in majority, close cases")
o

## ---- arch-prep
AR <- data.frame(justice = names(ord), pos = as.vector(ord),
                 stringsAsFactors = FALSE)
AR$all   <- jj$pct_in_majority[match(AR$justice, jj$justice)]
AR$close <- jj$close_pct[match(AR$justice, jj$justice)]
AR$full  <- !is.na(AR$close)
AR$lab   <- nice(AR$justice)
ALLCOL   <- "#333333"    # every case
CLOSECOL <- "#e08214"    # the one-vote cases only
PEAK     <- AR$lab[which.max(AR$all)]
DROP     <- max(AR$all) - max(AR$close, na.rm = TRUE)
# where two justices sit almost on top of each other, drop the second label
AR$up <- TRUE
for (i in 2:nrow(AR))
  if (abs(AR$pos[i] - AR$pos[i - 1]) < 0.03 && abs(AR$all[i] - AR$all[i - 1]) < 9)
    AR$up[i] <- !AR$up[i - 1]

## ---- arch-d3
rows <- paste0("{\"k\":\"", AR$lab, "\",\"x\":",
               formatC(AR$pos, format = "f", digits = 4),
               ",\"a\":", formatC(AR$all, format = "f", digits = 1),
               ",\"c\":", ifelse(AR$full, formatC(AR$close, format = "f", digits = 1),
                                 "null"),
               ",\"u\":", tolower(as.character(AR$up)), "}", collapse = ",")
cat(paste0('
<div id="arch" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[', rows, '];
const W=770,H=380,M={t:22,r:26,b:66,l:56};
const svg=d3.select("#arch").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-0.30,0.32]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([40,106]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x).ticks(7));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickValues([40,50,60,70,80,90,100]).tickFormat(d=>d+"%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("% of the time in the majority");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-30).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("position on the line recovered from agreement alone");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#888")
  .text("The direction of this axis is arbitrary: only the order and the spacing mean anything.");
D.forEach(function(d){
  if(d.c!==null){
    svg.append("line").attr("x1",x(d.x)).attr("x2",x(d.x)).attr("y1",y(d.a)).attr("y2",y(d.c))
      .attr("stroke","#bbb").attr("stroke-width",2);
    svg.append("circle").attr("cx",x(d.x)).attr("cy",y(d.c)).attr("r",5).attr("fill","', CLOSECOL, '");
  }
  svg.append("circle").attr("cx",x(d.x)).attr("cy",y(d.a)).attr("r",5)
    .attr("fill",d.c===null?"#fff":"', ALLCOL, '")
    .attr("stroke","', ALLCOL, '").attr("stroke-width",2);
  svg.append("text").attr("x",x(d.x)).attr("y",y(d.a)+(d.u?-10:16)).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#555").text(d.k);
});
const lg=svg.append("g").attr("transform","translate("+(M.l+8)+","+(M.t+8)+")");
lg.append("circle").attr("cx",6).attr("cy",-4).attr("r",5).attr("fill","', ALLCOL, '");
lg.append("text").attr("x",16).attr("y",0).attr("font-size","11px").text("all cases");
lg.append("circle").attr("cx",96).attr("cy",-4).attr("r",5).attr("fill","', CLOSECOL, '");
lg.append("text").attr("x",106).attr("y",0).attr("font-size","11px")
  .text("the ', nrow(cc), ' one-vote cases");
lg.append("circle").attr("cx",256).attr("cy",-4).attr("r",5).attr("fill","#fff")
  .attr("stroke","', ALLCOL, '").attr("stroke-width",2);
lg.append("text").attr("x",266).attr("y",0).attr("font-size","11px")
  .text("sat for part of the window: no comparable close-case rate");
})();
</script>'))

## ---- arch-static
par(mar = c(5.4, 4.4, 1.6, 1.0))
plot(NA, xlim = c(-0.30, 0.32), ylim = c(40, 106), las = 1, xlab = "", ylab = "",
     yaxt = "n", cex.axis = 0.8)
axis(2, at = seq(40, 100, 10), labels = paste0(seq(40, 100, 10), "%"), las = 1,
     cex.axis = 0.8)
mtext("% of the time in the majority", 2, line = 2.8, cex = 0.85)
mtext("position on the line recovered from agreement alone", 1, line = 2.3,
      cex = 0.85)
mtext(paste("The direction of this axis is arbitrary: only the order and the",
            "spacing mean anything."), 1, line = 3.4, cex = 0.7, col = "#777777")
with(AR[AR$full, ], segments(pos, all, pos, close, col = "#bbbbbb", lwd = 2))
with(AR[AR$full, ], points(pos, close, pch = 19, col = CLOSECOL, cex = 1.1))
points(AR$pos, AR$all, pch = 21, cex = 1.1, lwd = 2, col = ALLCOL,
       bg = ifelse(AR$full, ALLCOL, "white"))
text(AR$pos, AR$all + ifelse(AR$up, 2.8, -3.2), AR$lab, cex = 0.62,
     col = "#555555")
legend("top", c("all cases", paste("the", nrow(cc), "one-vote cases"),
                "sat for part of the window: no comparable rate"),
       pch = c(19, 19, 21), col = c(ALLCOL, CLOSECOL, ALLCOL), horiz = TRUE,
       pt.bg = "white", pt.lwd = 2, bty = "n", cex = 0.6)

## ---- long
data.frame(
  quantity = c("Terms in the series", "First", "Last",
               "Highest share of decisions coded conservative",
               "Lowest"),
  value = c(nrow(bt), min(bt$term), max(bt$term),
            paste0(pc(max(bt$pct_conservative)), "% (", bt$term[which.max(bt$pct_conservative)], ")"),
            paste0(pc(min(bt$pct_conservative)), "% (", bt$term[which.min(bt$pct_conservative)], ")")))

## ---- long-plot
par(mar = c(3.6, 4.2, 1, 1))
plot(bt$term, bt$pct_conservative, type = "l", col = "grey65",
     xlab = "", ylab = "% of decisions coded conservative")
lines(bt$term, filter(bt$pct_conservative, rep(1/5, 5), sides = 2),
      col = "#C41230", lwd = 2.2)
abline(h = 50, lty = 3)

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
#arch text[fill="#555" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
