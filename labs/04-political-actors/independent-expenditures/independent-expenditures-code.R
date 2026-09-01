# independent-expenditures-code.R -- chunk bodies for independent-expenditures-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
source("../../_lib/dd-charts.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

s  <- read.csv("data/derived/ie_summary.csv",    stringsAsFactors = FALSE)
o  <- read.csv("data/derived/ie_outliers.csv",   stringsAsFactors = FALSE)
cd <- read.csv("data/derived/ie_candidates.csv", stringsAsFactors = FALSE)

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

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
d  <- function(x) paste0("$", n(x))
bn <- function(x) paste0("$", pc(x / 1e9, 2), " billion")
ml <- function(x) paste0("$", n(x / 1e6), " million")
dm <- function(x) paste0("$", n(x / 1e6), "m")

# ---- Figure 2: the share of each version's dollars spent attacking ----------
# One number per version rather than a stacked pair. The claim is not that the
# file shrank -- it is which side of the halfway line the direction lands on,
# and a single bar against a line at 50% is the smallest form that shows it.
FLIP <- data.frame(
  version = c("the file as published", paste(NOUT, "rows removed")),
  attacking = c(100 * RAWO / RAW, 100 * CLNO / CLN),
  total = c(bn(RAW), bn(CLN)),
  stringsAsFactors = FALSE)

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
# Drawn with the shared chart library. d3 itself is already on the page --
# the filing card above loads it -- so dd_fig() emits only dd-charts.js here.
FLIPC <- as.list(setNames(c("series-1", "series-2"), FLIP$version))
dd_fig("flip", "bar", FLIP,
  size = list(w = 740, h = 210, m = list(t = 30, r = 40, b = 34, l = 168)),
  y = list(field = "version", band = TRUE),
  x = list(field = "attacking", domain = c(0, 100), fmt = "pct1", ticks = 5),
  series = list(field = "version", classes = FLIPC),
  valueLabels = TRUE,
  annotations = list(dd_annot_vline(50, class = "zero")),
  tip = dd_tip(c(attacking = "spent attacking", total = "total in this version"),
               fmt = c(attacking = "pct1"), title = "version"),
  d3 = FALSE)

## ---- flip-static
par(mar = c(4.2, 12.5, 1.2, 2))
bp <- barplot(rev(FLIP$attacking), horiz = TRUE, names.arg = rev(FLIP$version),
              las = 1, xlim = c(0, 100), col = c("#C41230", "#2c7fb8"),
              border = NA, cex.names = 0.8, cex.axis = 0.8,
              xlab = "% of that version's dollars spent attacking a candidate")
abline(v = 50, lty = 2)
text(50, max(bp) + 0.7, "half", adj = c(-0.15, 0.5), cex = 0.7, xpd = TRUE)
text(rev(FLIP$attacking), bp, paste0(pc(rev(FLIP$attacking)), "%"),
     adj = c(-0.2, 0.5), cex = 0.8)

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

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#card text[fill="#222222" i],
#card text[fill="#666666" i],
#card text[fill="#2c7fb8" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
