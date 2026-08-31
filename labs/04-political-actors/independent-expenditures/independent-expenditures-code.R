# independent-expenditures-code.R -- chunk bodies for independent-expenditures-brief.Rmd
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

s  <- read.csv("data/derived/ie_summary.csv",    stringsAsFactors = FALSE)
o  <- read.csv("data/derived/ie_outliers.csv",   stringsAsFactors = FALSE)
cd <- read.csv("data/derived/ie_candidates.csv", stringsAsFactors = FALSE)
cm <- read.csv("data/derived/ie_committees.csv", stringsAsFactors = FALSE)

# The source file's shape. The 19 MB raw FEC file is not committed, so the
# brief cannot count it here; these are the figures data/build-ie-data.R
# recorded at the moment it read the file.
FA   <- read.csv("data/derived/ie_facts.csv", stringsAsFactors = FALSE)
ROWS <- FA$rows
NCOL <- FA$columns

# The exclusion threshold comes from the build rather than from this sentence,
# so that changing the rule changes the chapter instead of contradicting it.
CUT     <- FA$cut_usd
HALF    <- CUT / 2
KEPT    <- FA$largest_kept          # the largest row the rule did NOT remove
INBAND  <- FA$rows_half_to_cut      # counted against the whole file, not the outliers

get <- function(v, k) s$dollars[s$version == v & s$side == k]
RAWS <- get("raw", "support");     RAWO <- get("raw", "oppose")
CLNS <- get("cleaned", "support"); CLNO <- get("cleaned", "oppose")
RAW  <- RAWS + RAWO;               CLN  <- CLNS + CLNO
OUT  <- sum(o$amount)
NOUT <- nrow(o)
SHARE <- 100 * OUT / RAW

# the nine-fold duplicate, isolated
FW   <- o[o$spender == "Food & Water Action", ]
FWN  <- nrow(FW); FWAMT <- sum(FW$amount)
JUNK <- o[o$spender != "Food & Water Action", ]

# what happens if you restore only the duplicated rows
RS <- CLNS + FWAMT; RT <- RS + CLNO

cd$off <- c(P = "President", S = "Senate", H = "House")[cd$office]

# candidate-name splits that survived the cleaning
dupname <- function(prefix) cd[grepl(prefix, cd$candidate), ]
TR <- dupname("^TRUMP"); TE <- dupname("^TESTER")

byoff <- aggregate(cbind(supporting, opposing) ~ off, cd, sum)
byoff$pct_against <- 100 * byoff$opposing / (byoff$supporting + byoff$opposing)
byoff <- byoff[order(-byoff$pct_against), ]

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
d  <- function(x) paste0("$", n(x))
bn <- function(x) paste0("$", pc(x / 1e9, 2), " billion")
ml <- function(x) paste0("$", n(x / 1e6), " million")
dm <- function(x) paste0("$", n(x / 1e6), "m")

# ---- Figure 1: the raw filing behind the Walter White row ------------------
# data/ie_filing_1707511.fec is the filing itself, fetched 10 August 2026 from
# https://docquery.fec.gov/dcdev/posted/1707511.fec (418 bytes). Three records
# separated by newlines, fields delimited by ASCII 0x1C: an HDR, an FEC Form 24
# cover record (F24N) and a single Schedule E line (SE). Field positions below
# are the FEC's v8.4 electronic filing layout, checked against this file.
FLN  <- o$file_num[o$spender == "Gus Associates"]
FLB  <- file.size("data/raw/ie_filing_1707511.fec")
flr  <- readLines("data/raw/ie_filing_1707511.fec", warn = FALSE)
F24F <- strsplit(flr[2], "\x1c")[[1]]
SEF  <- strsplit(flr[3], "\x1c")[[1]]
stopifnot(F24F[1] == "F24N", SEF[1] == "SE", F24F[2] == SEF[2],
          as.numeric(SEF[21]) == o$amount[o$spender == "Gus Associates"],
          toupper(SEF[29]) == "WHITE")
fdate <- function(x) trimws(format(as.Date(x, "%Y%m%d"), "%e %B %Y"))
FEEAMT <- as.numeric(SEF[21])
CARD <- data.frame(
  field = c("Filing committee", "FEC committee ID", "Treasurer", "Payee",
            "Date of expenditure", "Amount", "Stated purpose", "Candidate",
            "Office sought", "Support or oppose", "Memo text"),
  value = c(F24F[5], F24F[2], paste(F24F[12], F24F[11]),
            paste0(SEF[7], ", ", SEF[15], " ", SEF[16]),
            fdate(SEF[20]), d(FEEAMT), SEF[24],
            paste0(SEF[30], " ", SEF[31], " ", SEF[29], "  (", SEF[28], ")"),
            paste0(c(H = "House", S = "Senate", P = "President")[SEF[34]],
                   ", ", SEF[36], "-", SEF[35]),
            paste0(SEF[27], "  (", c(S = "support", O = "oppose")[SEF[27]], ")"),
            SEF[44]),
  stringsAsFactors = FALSE)
CARDH1 <- paste0("FEC FORM ", sub("^F([0-9]+).*$", "\\1", F24F[1]))
CARDH2 <- paste0(F24F[3], "-HOUR NOTICE OF INDEPENDENT EXPENDITURE")
CARDF  <- paste0("Filing ", FLN, ", signed ", fdate(F24F[16]),
                 ". This is the record exactly as the FEC received and",
                 " published it. Nothing on it was checked.")
AMTROW <- which(CARD$field == "Amount")
SOROW  <- which(CARD$field == "Support or oppose")

# ---- Figure 3: support against oppose, one absolute scale ------------------
BF     <- head(cd[order(-cd$total), ], 12)
BF     <- BF[order(BF$total), ]                 # smallest at the bottom
BF$lab <- paste0(BF$candidate, " (", BF$off, ")")
BFLIM  <- ceiling(max(c(BF$supporting, BF$opposing)) / 2e8) * 2e8
BFTK   <- seq(-BFLIM, BFLIM, by = 2e8)
BFTL   <- ifelse(BFTK == 0, "$0", paste0("$", n(abs(BFTK) / 1e6), "m"))
BFNAME <- c("BROWN, SHERROD", "CASEY, ROBERT")  # the two the prose names
BF$note <- ifelse(BF$candidate %in% BFNAME,
                  paste0(pc(BF$pct_against), "% of it against him"), "")

# ---- Figure 4: committees by how much of their money attacks ---------------
# Circle AREA is dollars, so the radius is a square-root scale. The layout
# below is computed once, in R, and used by both versions of the figure: the
# horizontal position is the committee's percentage, the vertical position is
# packing only and means nothing.
BEE      <- cm[order(-cm$amount), ]
RMAX     <- 4.0                                  # virtual units, panel is 100 wide
BEE$r    <- RMAX * sqrt(BEE$amount / max(BEE$amount))
BEE$x    <- BEE$pct_against
BEE$y    <- 0
for (i in seq_len(nrow(BEE))[-1]) {
  offs  <- seq(0.25, 20, by = 0.25)
  for (cy in c(0, as.vector(rbind(offs, -offs)))) {
    dx <- BEE$x[1:(i - 1)] - BEE$x[i]
    dy <- BEE$y[1:(i - 1)] - cy
    if (all(dx * dx + dy * dy >= (BEE$r[1:(i - 1)] + BEE$r[i])^2 - 1e-9)) {
      BEE$y[i] <- cy; break
    }
  }
}
BEEREF  <- c(5e8, 2.5e8, 1e8)                    # size-legend reference dollars
BEERR   <- RMAX * sqrt(BEEREF / max(BEE$amount))
BEERL   <- dm(BEEREF)
BEEBOT  <- 17                                    # legend circles sit on this line
BEETTL  <- 28                                    # legend title line
BEETOP  <- 3                                     # label the largest few
BEELAB  <- paste0(substr(BEE$spender[1:BEETOP], 1, 34), " ",
                  dm(BEE$amount[1:BEETOP]))
# where those labels sit, and where their leader lines meet the circles
BEELX <- c(5, 84, 90); BEELY <- c(-9, 12.5, 19.5)
BEELA <- c(0, 1, 1)                              # 0 = left aligned, 1 = right
bang  <- atan2(BEELY - BEE$y[1:BEETOP], BEELX - BEE$x[1:BEETOP])
BEEEX <- BEE$x[1:BEETOP] + cos(bang) * (BEE$r[1:BEETOP] + 0.3)
BEEEY <- BEE$y[1:BEETOP] + sin(bang) * (BEE$r[1:BEETOP] + 0.3)
BEESY <- BEELY - ifelse(BEELY > BEEEY, 1.2, -1.2)
BEEXL <- c(-4.5, 104.5)          # room for circles centered at 0% and at 100%
BEEYL <- c(-14, 29)              # virtual extents; units are equal on both axes

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- schema
CO <- read.csv("data/derived/ie_columns.csv", stringsAsFactors = FALSE)

# The prose belongs to the chapter; the values belong to the file. Keyed by
# column name so that a column appearing, moving or being renamed upstream
# shows up as a blank description rather than as a quietly wrong one.
HOLDS <- c(
  cand_id          = "FEC ID of the candidate the money is about",
  cand_name        = "that candidate's name",
  spe_id           = "FEC ID of the committee doing the spending",
  spe_nam          = "the spender's name, as the filer typed it",
  ele_type         = "which election: `P` primary, `G` general, and four more — `O` other, `S` special, `R` runoff, `C` convention",
  can_office_state = "state of the office sought",
  can_office_dis   = "district of the office sought — a code, not a quantity",
  can_office       = "office sought: `H`, `S` or `P`",
  cand_pty_aff     = "the candidate's party",
  exp_amo          = "the amount of this one expenditure",
  exp_date         = "the date of the expenditure — often blank, see below",
  agg_amo          = "running total this spender has aggregated for or against this candidate",
  sup_opp          = "`S` support or `O` oppose — the single character this chapter counts",
  pur              = "purpose, in the filer's own words",
  pay              = "who was paid",
  file_num         = "the FEC filing this row came from",
  amndt_ind        = "`N` new filing; `A1`–`A4` a first, second, third or fourth amendment",
  tran_id          = "the filer's own transaction ID",
  image_num        = "the scanned image of the paper filing",
  receipt_dat      = "the date the FEC received it",
  fec_election_yr  = "the election year — the same value on every row, so it carries nothing",
  prev_file_num    = "the filing this one amends, where it amends one",
  dissem_dt        = "the date the message actually went out")

# Set the values in code spans. Two reasons, and the second is the point of the
# column: pandoc turns a straight quote into a curly one, so `" Go America PAC"`
# arrives as `” Go America PAC”` and the leading space -- the whole reason that
# example is there -- stops being visible. A code span is literal and monospaced.
as_code <- function(s) {
  ifelse(grepl("distinct", s),
         sub('"([^"]*)"', '`"\\1"`', s),                    # the example
         vapply(strsplit(s, " · ", fixed = TRUE),           # a closed set
                function(p) paste0("`", p, "`", collapse = " · "), ""))
}

out <- data.frame(
  `#`                    = CO$n,
  `column as it arrives` = CO$column,
  `what is in it`        = as_code(CO$values),
  `blank`                = ifelse(CO$blank == 0, "—", paste0(pc(CO$blank), "%")),
  `what it holds`        = unname(HOLDS[CO$column]),
  check.names = FALSE)
out

## ---- schema-helpers
bl  <- function(k) CO$blank[CO$column == k]          # % of rows left empty
dom <- function(k) CO$values[CO$column == k]          # the values themselves

## ---- clean-ie
ss <- s
ss$dollars <- bn(ss$dollars)
names(ss) <- c("version", "side", "dollars", "% of that version")
ss

## ---- raw
data.frame(
  quantity = c("Filings in the 2024 independent-expenditure file",
               "Total reported", "Spent supporting a candidate",
               "Spent opposing a candidate"),
  value = c(n(ROWS), bn(RAW),
            paste0(bn(RAWS), "  (", pc(100 * RAWS / RAW), "%)"),
            paste0(bn(RAWO), "  (", pc(100 * RAWO / RAW), "%)")))

## ---- outliers
oo <- head(o[order(-o$amount), c("spender", "cand_name", "amount", "pur")], 7)
oo$amount <- d(oo$amount)
names(oo) <- c("filing committee", "candidate named", "amount",
               "stated purpose")
oo

## ---- outlier-share
data.frame(
  quantity = c("Rows in the file", paste("Rows above", ml(CUT)),
               "Their share of all rows", "Their share of all dollars"),
  value = c(n(ROWS), NOUT,
            paste0(pc(100 * NOUT / ROWS, 3), "%"),
            paste0(pc(SHARE), "%")))

## ---- d3-card
esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)
jsa <- function(x) paste0('"', esc(x), '"', collapse = ",")
cat(paste0('
<div id="card" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const F=[', jsa(CARD$field), '], V=[', jsa(CARD$value), '];
const AMT=', AMTROW - 1, ', SO=', SOROW - 1, ';
const W=720,HD=50,RH=30,FT=36,H=HD+RH*F.length+FT;
const svg=d3.select("#card").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
svg.append("rect").attr("x",0.5).attr("y",0.5).attr("width",W-1).attr("height",H-1)
  .attr("fill","#ffffff").attr("stroke","#cccccc");
svg.append("rect").attr("width",W).attr("height",HD).attr("fill","#444444");
svg.append("text").attr("x",18).attr("y",22).attr("fill","#ffffff")
  .attr("font-size","15px").attr("font-weight",700).text("', CARDH1, '");
svg.append("text").attr("x",18).attr("y",39).attr("fill","#dddddd")
  .attr("font-size","11.5px").attr("letter-spacing","0.06em").text("', CARDH2, '");
F.forEach(function(f,i){
  const y0=HD+i*RH;
  if(i===AMT){
    svg.append("rect").attr("x",1).attr("y",y0).attr("width",W-2).attr("height",RH)
      .attr("fill","#efefef");
    svg.append("rect").attr("x",1).attr("y",y0).attr("width",4).attr("height",RH)
      .attr("fill","#333333");
  } else if(i%2===1){
    svg.append("rect").attr("x",1).attr("y",y0).attr("width",W-2).attr("height",RH)
      .attr("fill","#fafafa");
  }
  svg.append("line").attr("x1",1).attr("x2",W-1).attr("y1",y0).attr("y2",y0)
    .attr("stroke","#e8e8e8");
  svg.append("text").attr("x",196).attr("y",y0+RH/2+4).attr("text-anchor","end")
    .attr("font-size","11px").attr("fill","#666666").text(f);
  svg.append("text").attr("x",212).attr("y",y0+RH/2+(i===AMT?5:4))
    .attr("font-size",i===AMT?"16px":"12.5px")
    .attr("font-weight",i===AMT?700:400)
    .attr("fill",i===SO?"#2c7fb8":"#222222").text(V[i]);
});
svg.append("line").attr("x1",1).attr("x2",W-1).attr("y1",H-FT).attr("y2",H-FT)
  .attr("stroke","#cccccc");
svg.append("text").attr("x",18).attr("y",H-FT+22).attr("font-size","11px")
  .attr("fill","#666666").text("', CARDF, '");
})();
</script>
'))

## ---- card-static
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(0, 100), ylim = c(0, 100), xaxs = "i", yaxs = "i",
     axes = FALSE, xlab = "", ylab = "")
NR <- nrow(CARD); RH <- 78 / NR
rect(0, 0, 100, 100, col = "#ffffff", border = "#cccccc")
rect(0, 90, 100, 100, col = "#444444", border = NA)
text(2, 96.6, CARDH1, adj = 0, col = "#ffffff", font = 2, cex = 0.95)
text(2, 92.4, CARDH2, adj = 0, col = "#dddddd", cex = 0.62)
for (i in seq_len(NR)) {
  y1 <- 90 - (i - 1) * RH; y0 <- y1 - RH
  if (i == AMTROW) {
    rect(0, y0, 100, y1, col = "#efefef", border = NA)
    rect(0, y0, 0.8, y1, col = "#333333", border = NA)
  } else if (i %% 2 == 0) {
    rect(0, y0, 100, y1, col = "#fafafa", border = NA)
  }
  segments(0, y1, 100, y1, col = "#e8e8e8")
  text(27, y0 + RH / 2, CARD$field[i], adj = 1, cex = 0.6, col = "#666666")
  text(29, y0 + RH / 2, CARD$value[i], adj = 0,
       cex = if (i == AMTROW) 0.92 else 0.72,
       font = if (i == AMTROW) 2 else 1,
       col = if (i == SOROW) "#2c7fb8" else "#222222")
}
segments(0, 12, 100, 12, col = "#cccccc")
text(2, 7.5, CARDF, adj = 0, cex = 0.56, col = "#666666")
rect(0, 0, 100, 100, col = NA, border = "#cccccc")

## ---- fw
oo <- FW[, c("spender", "cand_name", "amount", "file_num")]
oo$amount <- d(oo$amount)
oo <- head(oo, 4)
names(oo) <- c("filing committee", "candidate", "amount", "FEC file number")
oo

## ---- cleaned
oo <- data.frame(
  version = c("Raw file", "Raw file", paste(NOUT, "rows removed"),
              paste(NOUT, "rows removed")),
  side = c("supporting a candidate", "opposing a candidate",
           "supporting a candidate", "opposing a candidate"),
  dollars = c(bn(RAWS), bn(RAWO), bn(CLNS), bn(CLNO)),
  share = paste0(pc(c(100*RAWS/RAW, 100*RAWO/RAW,
                      100*CLNS/CLN, 100*CLNO/CLN)), "%"),
  stringsAsFactors = FALSE)
names(oo) <- c("version", "direction", "dollars", "% of that version")
oo

## ---- d3-flip
cat(sprintf('
<div id="ie" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const d=[{k:"raw file",s:%.1f,o:%.1f,t:"%s"},
         {k:"%d rows removed",s:%.1f,o:%.1f,t:"%s"}];
const W=740,H=290,M={t:34,r:24,b:34,l:150};
const svg=d3.select("#ie").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleBand().domain(d.map(q=>q.k)).range([M.t,H-M.b]).padding(0.35);
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(5).tickFormat(v=>v+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0))
  .selectAll("text").attr("font-size","12px");
const g=svg.append("g").selectAll("g").data(d).join("g");
g.append("rect").attr("x",M.l).attr("y",q=>y(q.k)).attr("height",y.bandwidth())
  .attr("fill","#2c7fb8").attr("width",0)
  .transition().duration(800).attr("width",q=>x(q.s)-x(0));
g.append("rect").attr("y",q=>y(q.k)).attr("height",y.bandwidth())
  .attr("fill","#C41230").attr("x",M.l).attr("width",0)
  .transition().duration(800).delay(200)
  .attr("x",q=>M.l+x(q.s)-x(0)).attr("width",q=>x(q.o)-x(0));
g.append("text").attr("x",M.l+8).attr("y",q=>y(q.k)+y.bandwidth()/2+4)
  .attr("fill","#fff").attr("font-size","12px")
  .text(q=>q.s.toFixed(1)+"%% supporting");
g.append("text").attr("x",W-M.r-8).attr("y",q=>y(q.k)+y.bandwidth()/2+4)
  .attr("text-anchor","end").attr("fill","#fff").attr("font-size","12px")
  .text(q=>q.o.toFixed(1)+"%% opposing");
g.append("text").attr("x",M.l-10).attr("y",q=>y(q.k)+y.bandwidth()/2+18)
  .attr("text-anchor","end").attr("font-size","11px").attr("fill","#666")
  .text(q=>q.t);
svg.append("text").attr("x",M.l).attr("y",18).attr("font-size","12px")
  .attr("fill","#333").text("Where outside money went, before and after removing %d rows");
})();
</script>
', 100*RAWS/RAW, 100*RAWO/RAW, bn(RAW), NOUT,
   100*CLNS/CLN, 100*CLNO/CLN, bn(CLN), NOUT))

## ---- flip-static
m <- matrix(c(100*RAWS/RAW, 100*RAWO/RAW, 100*CLNS/CLN, 100*CLNO/CLN), nrow = 2)
par(mar = c(3, 4, 2, 8))
barplot(m, names.arg = c("raw file", paste(NOUT, "rows removed")),
        col = c("#2c7fb8", "#C41230"), ylab = "percent of dollars",
        legend.text = c("supporting a candidate", "opposing a candidate"),
        args.legend = list(x = "right", inset = c(-0.28, 0), bty = "n", cex = 0.8, xpd = TRUE))

## ---- threshold
data.frame(
  quantity = c("Smallest row removed", "Largest row kept",
               paste("Rows anywhere in the file between", dm(HALF),
                     "and", dm(CUT))),
  value = c(d(min(o$amount)), d(KEPT), INBAND))

## ---- restore
data.frame(
  version = c(paste(NOUT, "rows removed (as published)"),
              paste("with the", FWN, "duplicate rows restored")),
  supporting = paste0(pc(c(100*CLNS/CLN, 100*RS/RT)), "%"),
  opposing   = paste0(pc(c(100*CLNO/CLN, 100*CLNO/RT)), "%"))

## ---- candidates
oo <- head(cd[, c("candidate", "off", "supporting", "opposing", "pct_against")], 8)
oo$supporting <- d(oo$supporting); oo$opposing <- d(oo$opposing)
oo$pct_against <- paste0(pc(oo$pct_against), "%")
names(oo) <- c("candidate", "office", "spent supporting", "spent opposing",
               "% against")
oo

## ---- d3-butterfly
bfj <- paste0(sapply(rev(seq_len(nrow(BF))), function(i) paste0(
  '{"k":"', BF$lab[i], '","s":', pc(BF$supporting[i], 0),
  ',"o":', pc(BF$opposing[i], 0), ',"note":"', BF$note[i], '"}')),
  collapse = ",")
tkj <- paste0(pc(BFTK, 0), collapse = ",")
tlj <- paste0('"', BFTL, '"', collapse = ",")
cat(paste0('
<div id="bfly" style="margin:1em 0"></div>
<script>
(function(){
const D=[', bfj, '], TK=[', tkj, '], TL=[', tlj, '], LIM=', pc(BFLIM, 0), ';
const W=720,H=430,M={t:46,r:26,b:42,l:184};
const svg=d3.select("#bfly").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-LIM,LIM]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(q=>q.k)).range([M.t,H-M.b]).padding(0.28);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues(TK).tickFormat((v,i)=>TL[i]));
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("text").attr("x",M.l-10).attr("y",q=>y(q.k)+y.bandwidth()/2+4)
  .attr("text-anchor","end").attr("font-size","11px").attr("fill","#333333")
  .text(q=>q.k);
g.append("rect").attr("y",q=>y(q.k)).attr("height",y.bandwidth())
  .attr("x",q=>x(-q.s)).attr("width",q=>x(0)-x(-q.s)).attr("fill","#2c7fb8");
g.append("rect").attr("y",q=>y(q.k)).attr("height",y.bandwidth())
  .attr("x",x(0)).attr("width",q=>x(q.o)-x(0)).attr("fill","#C41230");
g.filter(q=>q.note!=="").append("text").attr("y",q=>y(q.k)+y.bandwidth()/2+4)
  .attr("x",q=>x(q.o)+7).attr("font-size","11px").attr("fill","#C41230")
  .attr("font-weight",700).text(q=>q.note);
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t-6)
  .attr("y2",H-M.b).attr("stroke","#666666");
svg.append("text").attr("x",x(0)-10).attr("y",M.t-16).attr("text-anchor","end")
  .attr("font-size","12px").attr("font-weight",700).attr("fill","#2c7fb8")
  .text("spent supporting");
svg.append("text").attr("x",x(0)+10).attr("y",M.t-16)
  .attr("font-size","12px").attr("font-weight",700).attr("fill","#C41230")
  .text("spent opposing");
})();
</script>
'))

## ---- butterfly-static
par(mar = c(3.2, 13.2, 2.6, 0.8), mgp = c(2, 0.6, 0), cex = 0.9)
plot(NA, xlim = c(-BFLIM, BFLIM), ylim = c(0.45, nrow(BF) + 0.55),
     axes = FALSE, xlab = "", ylab = "", yaxs = "i")
for (i in seq_len(nrow(BF))) {
  rect(-BF$supporting[i], i - 0.34, 0, i + 0.34, col = "#2c7fb8", border = NA)
  rect(0, i - 0.34, BF$opposing[i], i + 0.34, col = "#C41230", border = NA)
  if (nzchar(BF$note[i]))
    text(BF$opposing[i], i, BF$note[i], adj = c(-0.06, 0.5), cex = 0.58,
         col = "#C41230", font = 2)
}
axis(1, at = BFTK, labels = BFTL, cex.axis = 0.68)
axis(2, at = seq_len(nrow(BF)), labels = BF$lab, las = 1, tick = FALSE,
     cex.axis = 0.6, line = -0.4)
segments(0, 0.45, 0, nrow(BF) + 0.8, col = "#666666", xpd = TRUE)
mtext("spent supporting", side = 3, at = -BFLIM / 12, adj = 1, line = 0.4,
      cex = 0.72, font = 2, col = "#2c7fb8")
mtext("spent opposing", side = 3, at = BFLIM / 12, adj = 0, line = 0.4,
      cex = 0.72, font = 2, col = "#C41230")

## ---- by-office
oo <- byoff
oo$supporting <- d(oo$supporting); oo$opposing <- d(oo$opposing)
oo$pct_against <- paste0(pc(oo$pct_against), "%")
names(oo) <- c("office", "spent supporting", "spent opposing", "% against")
oo

## ---- committees
oo <- head(cm[, c("spender", "amount", "pct_against")], 8)
oo$spender <- substr(oo$spender, 1, 42)
oo$amount <- d(oo$amount); oo$pct_against <- paste0(pc(oo$pct_against), "%")
names(oo) <- c("committee", "total spent", "% spent attacking")
oo

## ---- d3-swarm
dots <- paste0(sapply(seq_len(nrow(BEE)), function(i) paste0(
  '{"x":', pc(BEE$x[i], 2), ',"y":', pc(BEE$y[i], 2),
  ',"r":', pc(BEE$r[i], 3), '}')), collapse = ",")
lref <- paste0(sapply(seq_along(BEERR), function(i) paste0(
  '{"r":', pc(BEERR[i], 3), ',"t":"', BEERL[i], '"}')), collapse = ",")
lead <- paste0(sapply(seq_len(BEETOP), function(i) paste0(
  '{"x1":', pc(BEELX[i], 2), ',"y1":', pc(BEESY[i], 2),
  ',"x2":', pc(BEEEX[i], 2), ',"y2":', pc(BEEEY[i], 2),
  ',"lx":', pc(BEELX[i], 2), ',"ly":', pc(BEELY[i], 2),
  ',"a":"', ifelse(BEELA[i] == 0, "start", "end"), '","t":"', BEELAB[i],
  '"}')), collapse = ",")
cat(paste0('
<div id="swarm" style="margin:1em 0"></div>
<script>
(function(){
const D=[', dots, '], R=[', lref, '], A=[', lead, '];
const XL=[', pc(BEEXL[1], 1), ',', pc(BEEXL[2], 1), '];
const YL=[', pc(BEEYL[1], 0), ',', pc(BEEYL[2], 0), '];
const BOT=', pc(BEEBOT, 0), ', TTL=', pc(BEETTL, 0), ';
const W=720,M={t:8,r:14,b:44,l:14};
const PW=W-M.l-M.r, K=PW/(XL[1]-XL[0]), PH=(YL[1]-YL[0])*K, H=PH+M.t+M.b;
const svg=d3.select("#swarm").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(XL).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain(YL).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues([0,25,50,75,100]).tickFormat(v=>v+"%"))
  .call(g=>g.select(".domain").attr("d","M"+x(0)+",0H"+x(100)));
svg.append("text").attr("x",x(50)).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#666666")
  .text("share of the committee\'s own money it spent attacking a candidate");
A.forEach(function(a){
  svg.append("line").attr("x1",x(a.x1)).attr("y1",y(a.y1))
    .attr("x2",x(a.x2)).attr("y2",y(a.y2)).attr("stroke","#999999");
});
D.forEach(function(p){
  svg.append("circle").attr("cx",x(p.x)).attr("cy",y(p.y)).attr("r",p.r*K)
    .attr("fill","#54278F").attr("stroke","#ffffff");
});
A.forEach(function(a){
  svg.append("text").attr("x",x(a.lx)).attr("y",y(a.ly)+4).attr("text-anchor",a.a)
    .attr("font-size","11.5px").attr("font-weight",700).attr("fill","#333333")
    .text(a.t);
});
R.forEach(function(e){
  svg.append("circle").attr("cx",x(6)).attr("cy",y(BOT+e.r)).attr("r",e.r*K)
    .attr("fill","none").attr("stroke","#999999");
  svg.append("line").attr("x1",x(6)).attr("y1",y(BOT+2*e.r))
    .attr("x2",x(12.5)).attr("y2",y(BOT+2*e.r)).attr("stroke","#bbbbbb");
  svg.append("text").attr("x",x(13.5)).attr("y",y(BOT+2*e.r)+4)
    .attr("font-size","10.5px").attr("fill","#666666").text(e.t);
});
svg.append("text").attr("x",x(6-', pc(RMAX, 2), ')).attr("y",y(TTL)+4)
  .attr("font-size","11px").attr("fill","#666666")
  .text("circle area (not radius) = dollars the committee spent");
})();
</script>
'))

## ---- swarm-static
# fig.height is set so that one unit is the same length on both axes and the
# circles are circles: pin is 6.768 x 2.670 inches for a 109 x 43 unit panel.
par(mar = c(3.2, 1.2, 0.4, 1.2), mgp = c(2, 0.6, 0), cex = 0.9)
plot(NA, xlim = BEEXL, ylim = BEEYL, axes = FALSE,
     xlab = "", ylab = "", xaxs = "i", yaxs = "i")
segments(BEELX, BEESY, BEEEX, BEEEY, col = "#999999")
symbols(BEE$x, BEE$y, circles = BEE$r, inches = FALSE, add = TRUE,
        bg = "#54278F", fg = "#ffffff")
for (i in seq_len(BEETOP))
  text(BEELX[i], BEELY[i], BEELAB[i], adj = c(BEELA[i], 0.5), cex = 0.62,
       font = 2, col = "#333333")
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
     cex.axis = 0.72, lwd = 0, lwd.ticks = 1)
segments(0, BEEYL[1], 100, BEEYL[1], col = "#333333")
mtext("share of the committee's own money it spent attacking a candidate",
      side = 1, line = 1.7, cex = 0.66, col = "#666666")
symbols(rep(6, length(BEERR)), BEEBOT + BEERR, circles = BEERR,
        inches = FALSE, add = TRUE, fg = "#999999")
segments(6, BEEBOT + 2 * BEERR, 12.5, BEEBOT + 2 * BEERR, col = "#bbbbbb")
text(13.5, BEEBOT + 2 * BEERR, BEERL, adj = 0, cex = 0.56, col = "#666666")
text(6 - RMAX, BEETTL, "circle area (not radius) = dollars the committee spent",
     adj = 0, cex = 0.6, col = "#666666")

## ---- name-split
oo <- rbind(TR, TE)[, c("candidate", "off", "supporting", "opposing", "pct_against")]
oo$supporting <- d(oo$supporting); oo$opposing <- d(oo$opposing)
oo$pct_against <- paste0(pc(oo$pct_against), "%")
names(oo) <- c("candidate as filed", "office", "supporting", "opposing", "% against")
oo

## ---- trump-merge
data.frame(
  quantity = c("Rows in the table above, Trump", "Combined total",
               "Combined % against",
               "The larger row's % against, read alone"),
  value = c(nrow(TR), d(sum(TR$total)),
            paste0(pc(100 * sum(TR$opposing) / sum(TR$total)), "%"),
            paste0(pc(TR$pct_against[which.max(TR$total)]), "%")))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#card text[fill="#222222" i],
#card text[fill="#2c7fb8" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
