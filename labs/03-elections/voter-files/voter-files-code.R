# voter-files-code.R -- chunk bodies for voter-files-brief.Rmd
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

source("../../_lib/structure.R")
source("../../_lib/dd-charts.R")

# This brief shows five real registrants AND describes all 127,560. Nothing is
# withheld -- Georgia's list is a public record and this book does not redact
# its sources. The two are not redundant: the sample shows what a record is, and
# the whole-file summary shows what the file does. Reasoning about the file from
# the sample alone is what once put a nonexistent district 4 in this prose.

s    <- read.csv("data/derived/schema.csv",     stringsAsFactors = FALSE)
peek <- read.csv("data/derived/peek.csv",       stringsAsFactors = FALSE,
                 check.names = FALSE, colClasses = "character")
rp   <- read.csv("data/derived/race_party.csv", stringsAsFactors = FALSE, check.names = FALSE)
st   <- read.csv("data/derived/status.csv",     stringsAsFactors = FALSE)
to   <- read.csv("data/derived/turnout.csv",    stringsAsFactors = FALSE, check.names = FALSE)
lost <- read.csv("data/derived/lost.csv",       stringsAsFactors = FALSE)

VOTERS   <- sum(to$in_file_2026)
ACTIVE   <- st$voters[st$status == "ACTIVE"]
INACTIVE <- sum(st$voters[st$status == "INACTIVE"])
NCOL     <- nrow(s)
NBALLOT  <- sum(s$purpose == "Which ballot you get")

grp   <- rowSums(rp[, -1])
blank <- rp[, "no primary ballot on record"] + rp[, "NON-PARTISAN"]
NOSIG <- 100 * sum(blank) / sum(grp)

# The one sampled registrant with a party primary ballot on record, read column
# by column in the prose. Chosen by rule rather than by row number.
PK <- peek[peek[["Last Party Voted"]] %in% c("DEMOCRAT", "REPUBLICAN"), ][1, ]
pk <- function(col) PK[[col]]

to$rate24 <- 100 * to$`2024 general` / to$in_file_2026
big <- to[to$in_file_2026 > 1000, ]

# voters recorded in each election = still in the file + since vanished
els <- c("2020 general", "2022 general", "2024 general")
decay <- data.frame(
  election = els,
  in_file  = sapply(els, function(e) sum(to[[e]])),
  gone     = lost$voters_no_longer_in_file[match(els, lost$election)],
  stringsAsFactors = FALSE)
decay$voted <- decay$in_file + decay$gone
decay$pct_gone <- 100 * decay$gone / decay$voted

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# ---- Figure 1: one record as a field strip ---------------------------------
# Color carries one idea: the ballot-routing block. Everything else is a gray
# ramp, darkest = largest group, so the legend reads as a ranking.
pur    <- sort(table(s$purpose), decreasing = TRUE)
PURN   <- names(pur)
PURC   <- as.integer(pur)
BFIRST <- min(s$n[s$purpose == "Which ballot you get"])
BLAST  <- max(s$n[s$purpose == "Which ballot you get"])
BSHARE <- pc(100 * NBALLOT / NCOL)
PCOL   <- setNames(c("#54278F", "#4F4F4F", "#737373", "#969696",
                     "#B0B0B0", "#C8C8C8", "#DEDEDE", "#F2F2F2")[seq_along(PURN)],
                   PURN)
PTXT   <- setNames(ifelse(seq_along(PURN) <= 3, "#FFFFFF", "#222222"), PURN)
NPR    <- 9                                   # cells per row of the strip
NROWS  <- ceiling(NCOL / NPR)
strip_cap <- paste0(
  "One voter's record: all ", NCOL, " columns in the order the file stores ",
  "them, colored by what each one is for, with the legend counting the ",
  "columns in each group. Columns ", BFIRST, " to ", BLAST,
  " are a single unbroken run of ", NBALLOT, " district codes, ", BSHARE,
  "% of the record.")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- peek
peek[, c("Last Name", "First Name", "Birth Year", "Race", "County Precinct",
         "Registration Date", "Last Party Voted")]

## ---- schema-shape
o <- as.data.frame(table(purpose = s$purpose), stringsAsFactors = FALSE)
names(o) <- c("what the columns are for", "how many columns")
o <- o[order(-o$"how many columns"), ]
o

## ---- strip-d3
cells <- paste0('{"i":', s$n, ',"nm":"', s$column, '","p":',
                match(s$purpose, PURN) - 1, '}', collapse = ",")
fills <- paste0('"', PCOL, '"', collapse = ",")
inks  <- paste0('"', PTXT, '"', collapse = ",")
keys  <- paste0('{"lab":"', PURN, '","cnt":"', PURC, '","c":"', PCOL, '"}',
                collapse = ",")
cat(paste0('
<div id="rec" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', cells, '], F=[', fills, '], K=[', inks, '], L=[', keys, '];
const NPR=', NPR, ', NR=', NROWS, ';
const W=760,M={t:8,r:10,b:6,l:10},LEGH=54;
const CW=(W-M.l-M.r)/NPR, CH=60, H=M.t+NR*CH+LEGH+M.b;
const svg=d3.select("#rec").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
function wrap(t,mx){const w=t.split(" "),o=[];let c="";
  w.forEach(x=>{const j=c?c+" "+x:x;
    if(j.length>mx&&c){o.push(c);c=x;}else{c=j;}});
  if(c)o.push(c);return o;}
const g=svg.selectAll("g.c").data(D).join("g")
  .attr("transform",d=>`translate(${M.l+((d.i-1)%NPR)*CW},${M.t+Math.floor((d.i-1)/NPR)*CH})`);
g.append("rect").attr("x",1.5).attr("y",2).attr("width",CW-3)
  .attr("height",CH-4).attr("rx",3).attr("fill",d=>F[d.p]);
g.append("title").text(d=>"column "+d.i+": "+d.nm+" ("+L[d.p].lab+")");
g.append("text").attr("x",6).attr("y",13).attr("font-size","8px")
  .attr("fill",d=>K[d.p]).attr("fill-opacity",0.8).text(d=>d.i);
g.each(function(d){
  const ln=wrap(d.nm,Math.floor((CW-12)/4.9));
  const fs=ln.length>3?8:9.2;
  d3.select(this).selectAll("text.l").data(ln).join("text")
    .attr("x",CW/2).attr("y",(t,k)=>CH/2+5+(k-(ln.length-1)/2)*(fs+1.4))
    .attr("text-anchor","middle").attr("font-size",fs+"px")
    .attr("fill",K[d.p]).text(t=>t);
});
const lg=svg.selectAll("g.k").data(L).join("g")
  .attr("transform",(d,i)=>`translate(${M.l+(i%4)*((W-M.l-M.r)/4)},${M.t+NR*CH+18+Math.floor(i/4)*20})`);
lg.append("rect").attr("width",13).attr("height",13).attr("y",-10).attr("rx",2)
  .attr("fill",d=>d.c).attr("stroke","#BBBBBB").attr("stroke-width",0.5);
lg.append("text").attr("x",19).attr("font-size","11.5px").attr("fill","#333")
  .text(d=>d.cnt+"  "+d.lab);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">', strip_cap, '</p>
'))

## ---- strip-static
par(mar = c(3.2, 0.4, 0.3, 0.4))
plot(NA, xlim = c(0, NPR), ylim = c(NROWS + 1.30, 0), axes = FALSE,
     xlab = "", ylab = "", xaxs = "i", yaxs = "i")
for (i in seq_len(NCOL)) {
  rr <- (i - 1) %/% NPR; cc <- (i - 1) %% NPR
  x0 <- cc + 0.03; x1 <- cc + 0.97; y0 <- rr + 0.07; y1 <- rr + 0.93
  pp <- s$purpose[i]
  rect(x0, y0, x1, y1, col = PCOL[pp], border = "white", lwd = 0.7)
  cx <- 0.58
  for (k in c(15, 12, 10)) {
    L <- strwrap(s$column[i], width = k)
    if (max(strwidth(L, cex = cx)) <= (x1 - x0) * 0.90) break
  }
  wd <- max(strwidth(L, cex = cx))
  if (wd > (x1 - x0) * 0.90) cx <- cx * (x1 - x0) * 0.90 / wd
  yc <- (y0 + y1) / 2 + 0.04
  for (k in seq_along(L))
    text((x0 + x1) / 2, yc + (k - (length(L) + 1) / 2) * 0.135, L[k],
         cex = cx, col = PTXT[pp])
  text(x0 + 0.03, y0 + 0.11, i, cex = 0.44, adj = c(0, 0.5),
       col = adjustcolor(PTXT[pp], alpha.f = 0.8))
}
for (j in seq_along(PURN)) {
  lx <- ((j - 1) %% 4) * (NPR / 4)
  ly <- NROWS + 0.34 + ((j - 1) %/% 4) * 0.46
  rect(lx, ly - 0.15, lx + 0.20, ly + 0.15, col = PCOL[PURN[j]],
       border = "#BBBBBB", lwd = 0.7)
  text(lx + 0.28, ly, paste0(PURC[j], "  ", PURN[j]), adj = c(0, 0.5),
       cex = 0.64)
}
cw <- strwrap(strip_cap, width = 112)
mtext(cw, side = 1, line = 0.5 + (seq_along(cw) - 1) * 0.95, at = 0, adj = 0,
      cex = 0.62, col = "#666666")

## ---- absent
data.frame(
  `in the file` = c("That you voted, and on which date",
                    "Which party's primary ballot you last requested",
                    "Your race, as one box on a form",
                    "Your address, precinct, and ten district codes"),
  `not in the file` = c("Who you voted for, ever, in anything",
                        "Any statement of party affiliation",
                        "Anything about how you describe yourself now",
                        "Whether any of it is still true"),
  check.names = FALSE)

## ---- party-share
data.frame(
  quantity = c("Registered voters", "Blank — no primary ballot on record",
               "Marked NON-PARTISAN", "Carrying no usable party signal"),
  value = c(n(VOTERS), n(sum(rp[, "no primary ballot on record"])),
            n(sum(rp[, "NON-PARTISAN"])),
            paste0(n(sum(blank)), "  (", pc(NOSIG), "%)")))

## ---- ratios
o <- data.frame(race = rp$race, dem = rp$DEMOCRAT, rep = rp$REPUBLICAN,
                stringsAsFactors = FALSE)
o$`D per R` <- ifelse(o$rep > 0, pc(o$dem / o$rep, 1), NA)
o$`R per D` <- ifelse(o$dem > 0, pc(o$rep / o$dem, 1), NA)
o <- o[o$dem + o$rep > 500, ]
o

## ---- status
o <- st[order(-st$voters), ]
names(o) <- c("status", "reason recorded", "voters")
o$voters <- n(o$voters)
o

## ---- denominator
data.frame(
  `"registered voters" could mean` = c("Everyone on the list",
                                       "Active registrants only"),
  count = c(n(VOTERS), n(ACTIVE)),
  check.names = FALSE)

## ---- turnout
o <- big[order(-big$rate24), c("race", "in_file_2026", "2024 general", "rate24")]
o$in_file_2026 <- n(o$in_file_2026)
o$`2024 general` <- n(o$`2024 general`)
o$rate24 <- pc(o$rate24)
names(o) <- c("race", "in the 2026 file", "recorded voting in 2024",
              "turnout rate (%)")
o

## ---- lost
o <- decay
o$in_file <- n(o$in_file); o$gone <- n(o$gone); o$voted <- n(o$voted)
o$pct_gone <- pc(decay$pct_gone)
o <- o[, c("election", "voted", "in_file", "gone", "pct_gone")]
names(o) <- c("election", "recorded as voting", "still in the 2026 file",
              "no longer in the file", "% vanished")
o

## ---- decay-d3
# Drawn with the shared library (_lib/dd-charts.js). The strip figure above
# already loaded d3, so dd_fig() is told not to emit a second copy of that
# tag; it still emits dd-charts.js once for the document.
dd <- data.frame(
  election = sub(" general", "", decay$election),
  pct_gone = round(decay$pct_gone, 1),
  voted    = decay$voted,
  gone     = decay$gone)
dd_fig("decay", "bar", dd,
  x = list(field = "pct_gone", fmt = "pct1", domain = c(0, 14),
           label = "% of that election's voters missing from the 2026 file"),
  y = list(field = "election", band = TRUE),
  series = list(class = "series-2"),
  rowHeight = 40, valueLabels = TRUE,
  tip = dd_tip(c(voted = "recorded as voting",
                 gone  = "no longer in the file"),
               fmt = c(voted = "comma", gone = "comma"), title = "election"),
  d3 = FALSE)

## ---- decay-static
par(mar = c(4.2, 5.6, 0.6, 2.0))
bp <- barplot(rev(decay$pct_gone), horiz = TRUE,
              names.arg = rev(sub(" general", "", decay$election)), las = 1,
              cex.names = 0.85, col = "#C41230", border = NA,
              xlim = c(0, 14),
              xlab = "% of that election's voters missing from the 2026 file")
text(rev(decay$pct_gone), bp, paste0("  ", rev(pc(decay$pct_gone)), "%"),
     adj = 0, cex = 0.8)

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#rec text[fill="#222222" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
