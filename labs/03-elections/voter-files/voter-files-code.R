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

# This chapter shows five real registrants AND describes all 127,560. Nothing is
# withheld -- Georgia's list is a public record and this book does not redact
# its sources. The two are not redundant: the sample shows what a record is, and
# the whole-file summary shows what the file does. Reasoning about the file from
# the sample alone is what once put a nonexistent district 4 in this prose.

s    <- read.csv("data/derived/schema.csv",     stringsAsFactors = FALSE)
cols <- read.csv("data/derived/columns.csv",    stringsAsFactors = FALSE,
                 check.names = FALSE)
peek <- read.csv("data/derived/peek.csv",       stringsAsFactors = FALSE,
                 check.names = FALSE, colClasses = "character")
hd   <- read.csv("data/derived/headscan.csv",   stringsAsFactors = FALSE)
rp   <- read.csv("data/derived/race_party.csv", stringsAsFactors = FALSE, check.names = FALSE)
st   <- read.csv("data/derived/status.csv",     stringsAsFactors = FALSE)
to   <- read.csv("data/derived/turnout.csv",    stringsAsFactors = FALSE, check.names = FALSE)
lost <- read.csv("data/derived/lost.csv",       stringsAsFactors = FALSE)
regn   <- read.csv("data/derived/registration.csv", stringsAsFactors = FALSE)
REG_YES <- regn$jurisdictions[regn$requirement == "Required"]
REG_ALL <- sum(regn$jurisdictions)

# When registration closes, and by which door. One row per jurisdiction; the
# three date columns are blank where that way of registering is not offered,
# which is a different thing from a late deadline and is counted separately
# below. `listed` separates the two blank rows: North Dakota registers nobody,
# Puerto Rico holds no election this year.
dl   <- read.csv("data/derived/deadlines.csv", stringsAsFactors = FALSE)
DL   <- dl[dl$listed == "deadlines listed", ]
NALL <- nrow(dl)                               # every jurisdiction the tracker covers
NDL  <- nrow(DL)
DOORS  <- as.matrix(DL[, c("online_days", "mail_days", "in_person_days")])
NDOORS <- rowSums(!is.na(DOORS))
DSPREAD <- apply(DOORS, 1, function(v) max(v, na.rm = TRUE) - min(v, na.rm = TRUE))
NMULTI  <- sum(NDOORS >= 2)                    # places with more than one way in
NAGREE  <- sum(DSPREAD[NDOORS >= 2] == 0)      # ... where every date is the same
NDISAGREE <- NMULTI - NAGREE
DWIDEST <- max(DSPREAD)
DWIDESTJ <- DL$jurisdiction[which.max(DSPREAD)]
NEDAY   <- sum(DL$in_person_days == 0)         # in-person deadline is election day
NNOTE   <- sum(DL$same_day_note == "yes")      # ... a separate note says so
NPOST   <- sum(DL$mail_rule == "postmark", na.rm = TRUE)
NRECV   <- sum(DL$mail_rule == "receipt",  na.rm = TRUE)
NOONLINE <- sum(is.na(DL$online_days))
NOMAIL   <- sum(is.na(DL$mail_days))
DEARLY   <- max(DL$in_person_days)
DEARLYJ  <- DL$jurisdiction[which.max(DL$in_person_days)]
# Two of the jurisdictions in this table take a definite article in a
# sentence, and which one lands here is decided by the data.
if (DEARLYJ %in% c("Northern Mariana Islands", "Virgin Islands",
                   "District of Columbia")) DEARLYJ <- paste("the", DEARLYJ)

# What a state charges for its list, from the two published price tables the
# voter-file-access chapter puts side by side. Read from where they sit; the
# addresses of both publishers are in this chapter's sources.
prc <- read.csv("../voter-file-access/data/derived/price.csv", stringsAsFactors = FALSE)
PCM <- prc[prc$bp_price_is_one_number == "yes", ]
PRICE_COMPARABLE <- nrow(PCM)
PRICE_DIFFER     <- sum(PCM$same_price == "no")
dol <- function(x) paste0("$", format(as.numeric(x), big.mark = ",", trim = TRUE))
PRICE_GA_EAC <- dol(prc$eac_price_usd[prc$state == "Georgia"])
PRICE_GA_BP  <- prc$bp_price[prc$state == "Georgia"]

VOTERS   <- sum(to$in_file_2026)
ACTIVE   <- st$voters[st$status == "ACTIVE"]
INACTIVE <- sum(st$voters[st$status == "INACTIVE"])
NCOL     <- nrow(s)
NBALLOT  <- sum(s$purpose == "Which ballot you get")

grp   <- rowSums(rp[, -1])
blank <- rp[, "no primary ballot on record"] + rp[, "NON-PARTISAN"]
NOSIG <- 100 * sum(blank) / sum(grp)

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

# ---- Figure 2: race by "last party voted" ----------------------------------
MCAT <- c("DEMOCRAT", "REPUBLICAN", "NON-PARTISAN",
          "no primary ballot on record")
MCOL <- c("#2166AC", "#4d9221", "#999999", "#D9D9D9")
MTXT <- c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#222222")
mos  <- rp[grp > 1000, ]                      # groups big enough to draw
mos$total <- grp[grp > 1000]
mos  <- mos[order(-mos$total), ]
MW   <- mos$total / sum(mos$total)            # column widths
MP   <- as.matrix(mos[, MCAT]) / mos$total * 100   # % within each column
MWIDE <- MW > 0.06                            # wide enough to label in place
MCOV <- sum(mos$total)
MNOS <- 100 * sum(mos[, "NON-PARTISAN"] +
                  mos[, "no primary ballot on record"]) / MCOV
mos_cap <- paste0(
  "Each column is a race group, as wide as that group's share of registrants; ",
  "the ", nrow(mos), " groups drawn cover ", n(MCOV), " of the file's ",
  n(VOTERS), " registrants. The two gray blocks, non-partisan plus blank, are ",
  pc(MNOS), "% of them. The colors are arbitrary: this column is not party ",
  "registration, so it has no party colors.")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- headscan
hd[c(1:3), ]

## ---- headscan2
hd[4:7, ]

## ---- regstates
data.frame(`Voter registration` = regn$requirement,
           Jurisdictions = regn$jurisdictions,
           check.names = FALSE)

## ---- deadlines
data.frame(
  `How you register` = c("Online", "By mail", "In person"),
  `Jurisdictions offering it` = c(NDL - NOONLINE, NDL - NOMAIL, NDL),
  `Of those, closing on election day` = c(sum(DL$online_days == 0, na.rm = TRUE),
                                          sum(DL$mail_days == 0, na.rm = TRUE),
                                          NEDAY),
  `Earliest closing (days before)` = c(max(DL$online_days, na.rm = TRUE),
                                       max(DL$mail_days, na.rm = TRUE),
                                       DEARLY),
  check.names = FALSE)

## ---- deadlines-mail
data.frame(
  `What the mail deadline counts` = c("The postmark on the envelope",
                                      "The day the form arrives",
                                      "No registration by mail"),
  Jurisdictions = c(NPOST, NRECV, NOMAIL),
  check.names = FALSE)

## ---- peek
peek[, c("Last Name", "First Name", "Birth Year", "Race", "County Precinct",
         "Registration Date", "Last Party Voted")]

## ---- scan
cols

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

## ---- party
o <- rp[order(-grp), ]
o$total <- grp[order(-grp)]
o <- o[, c("race", "DEMOCRAT", "REPUBLICAN", "NON-PARTISAN",
           "no primary ballot on record", "total")]
# short headers: the full names collide in the PDF's LaTeX table
names(o) <- c("race", "Dem.", "Rep.", "Non-partisan", "no primary ballot",
              "total")
o

## ---- party-share
data.frame(
  quantity = c("Registered voters", "Blank — no primary ballot on record",
               "Marked NON-PARTISAN", "Carrying no usable party signal"),
  value = c(n(VOTERS), n(sum(rp[, "no primary ballot on record"])),
            n(sum(rp[, "NON-PARTISAN"])),
            paste0(n(sum(blank)), "  (", pc(NOSIG), "%)")))

## ---- mosaic-d3
mrow <- paste0('{"g":"', mos$race, '","n":', mos$total, ',"w":',
               formatC(MW, format = "f", digits = 6), ',"wide":',
               tolower(MWIDE), ',"p":[',
               apply(matrix(formatC(MP, format = "f", digits = 4),
                            nrow = nrow(mos)), 1, paste, collapse = ","),
               '],"lab":["',
               apply(matrix(paste0(formatC(MP, format = "f", digits = 0), "%"),
                            nrow = nrow(mos)), 1, paste, collapse = '","'),
               '"]}', collapse = ",")
cat(paste0('
<div id="mos" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', mrow, '];
const CAT=["', paste(MCAT, collapse = '","'), '"];
const CL=["', paste(MCOL, collapse = '","'), '"];
const IK=["', paste(MTXT, collapse = '","'), '"];
const W=760,H=430,M={t:112,r:176,b:30,l:52};
const IW=W-M.l-M.r, IH=H-M.t-M.b;
const NARROW=D.filter(d=>!d.wide).length;
const box=d3.select("#mos");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
let a=0; D.forEach(d=>{d.x0=a; a+=d.w; d.x1=a;});
// 100% at the top, 0% at the bottom, exactly as in the static twin
const y=d3.scaleLinear().domain([0,100]).range([M.t+IH,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([0,50,100]).tickFormat(v=>v+"%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+IH/2))
  .attr("y",14).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("share of the group");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
D.forEach(d=>{
  const gp=d.wide?1.6:0.5;
  const X=M.l+d.x0*IW, WD=Math.max(1.2,(d.x1-d.x0)*IW-gp);
  let top=100;
  d.p.forEach((v,j)=>{
    g.append("rect").attr("x",X+gp/2).attr("width",WD)
      .attr("y",y(top)).attr("height",y(top-v)-y(top))
      .attr("fill",CL[j]).attr("stroke","#fff").attr("stroke-width",0.55)
      .on("mousemove",function(e){tip.style("opacity",1)
        .html(`<b>${d.g}</b><br>${CAT[j]}: ${d.lab[j]} of ${d3.format(",")(d.n)}`)
        .style("left",Math.min(e.offsetX+14,W-300)+"px")
        .style("top",(e.offsetY-10)+"px");})
      .on("mouseleave",()=>tip.style("opacity",0));
    if(d.wide&&v>9) g.append("text").attr("x",X+WD/2+gp/2)
      .attr("y",(y(top)+y(top-v))/2+4).attr("text-anchor","middle")
      .attr("font-size","11.5px").attr("fill",IK[j]).text(d.lab[j]);
    top-=v;
  });
});
let s=0;
D.forEach(d=>{
  const cx=M.l+(d.x0+d.x1)/2*IW;
  if(d.wide){svg.append("text").attr("x",cx).attr("y",M.t-9)
    .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#333")
    .text(d.g);}
  else{const ly=M.t-14-(NARROW-1-s)*16; s++;
    svg.append("line").attr("x1",cx).attr("y1",M.t-2)
      .attr("x2",W-M.r+4).attr("y2",ly-4)
      .attr("stroke","#BBBBBB").attr("stroke-width",0.7);
    svg.append("text").attr("x",W-M.r+8).attr("y",ly)
      .attr("font-size","11px").attr("fill","#333").text(d.g);}
});
const lg=svg.selectAll("g.k").data(CAT).join("g")
  .attr("transform",(d,i)=>`translate(${M.l+(i%2)*250},${M.t-64+Math.floor(i/2)*19})`);
lg.append("rect").attr("width",13).attr("height",13).attr("y",-10).attr("rx",2)
  .attr("fill",(d,i)=>CL[i]).attr("stroke","#BBBBBB").attr("stroke-width",0.5);
lg.append("text").attr("x",19).attr("font-size","11.5px").attr("fill","#333")
  .text(d=>d);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">', mos_cap, '</p>
'))

## ---- mosaic-static
par(mar = c(4.2, 3.4, 0.4, 10.4))
edge <- c(0, cumsum(MW))
plot(NA, xlim = c(0, 1), ylim = c(0, 150), axes = FALSE, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i")
sl <- 0
for (k in seq_len(nrow(mos))) {
  gap <- if (MWIDE[k]) 0.0015 else 0.0004        # keep thin columns readable
  l <- edge[k] + gap; r0 <- edge[k + 1] - gap; top <- 100
  for (j in seq_along(MCAT)) {
    h <- MP[k, j]
    rect(l, top - h, r0, top, col = MCOL[j], border = "white", lwd = 0.55)
    if (MWIDE[k] && h > 9)
      text((l + r0) / 2, top - h / 2, paste0(pc(h, 0), "%"), cex = 0.7,
           col = MTXT[j])
    top <- top - h
  }
  ctr <- (l + r0) / 2
  if (MWIDE[k]) {
    text(ctr, 103, mos$race[k], cex = 0.74, adj = c(0.5, 0))
  } else {
    # names of the narrow groups live in the right margin, fanned so that no
    # leader line crosses a label: leftmost column, highest label
    ly <- 106 + (sum(!MWIDE) - 1 - sl) * 12; sl <- sl + 1
    segments(ctr, 102, 1.012, ly, col = "#BBBBBB", lwd = 0.7, xpd = NA)
    text(1.022, ly, mos$race[k], cex = 0.64, adj = c(0, 0.5), xpd = NA)
  }
}
axis(2, at = c(0, 50, 100), labels = c("0%", "50%", "100%"), las = 1,
     cex.axis = 0.78, mgp = c(3, 0.55, 0))
mtext("share of the group", side = 2, line = 2.1, at = 50, cex = 0.78)
for (j in seq_along(MCAT)) {
  lx <- ((j - 1) %% 2) * 0.38; ly <- 141 - ((j - 1) %/% 2) * 13
  rect(lx, ly - 4, lx + 0.022, ly + 4, col = MCOL[j], border = "#BBBBBB",
       lwd = 0.7)
  text(lx + 0.032, ly, MCAT[j], adj = c(0, 0.5), cex = 0.68)
}
cw <- strwrap(mos_cap, width = 122)
mtext(cw, side = 1, line = 1.0 + (seq_along(cw) - 1) * 0.95, at = 0, adj = 0,
      cex = 0.62, col = "#666666")

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

## ---- d3-decay
rowsA <- paste(sprintf('{"k":"%s","v":%.1f,"lab":"%s%%"}',
                       big$race[order(-big$rate24)],
                       big$rate24[order(-big$rate24)],
                       pc(big$rate24[order(-big$rate24)])), collapse = ",")
rowsB <- paste(sprintf('{"k":"%s","v":%.1f,"lab":"%s%% of that election\'s voters"}',
                       sub(" general", "", decay$election), decay$pct_gone,
                       pc(decay$pct_gone)), collapse = ",")
cat(sprintf('
<div id="vf" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="vA" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">2024 turnout rate, by race</button>
  <button id="vB" style="font:12px inherit;padding:4px 10px;cursor:pointer">Share of that election&rsquo;s voters now missing</button>
 </div>
</div>
<script>
(function(){
const A=[%s], B=[%s];
const W=760,H=380,M={t:14,r:150,b:36,l:200};
const svg=d3.select("#vf").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().range([M.l,W-M.r]);
const y=d3.scaleBand().range([M.t,H-M.b]).padding(0.22);
const gx=svg.append("g").attr("transform",`translate(0,${H-M.b})`);
const gy=svg.append("g").attr("transform",`translate(${M.l},0)`);
const bars=svg.append("g"), labs=svg.append("g");
function draw(d,color){
  x.domain([0,d3.max(d,q=>q.v)*1.12]); y.domain(d.map(q=>q.k));
  gx.transition().duration(500).call(d3.axisBottom(x).ticks(6).tickFormat(v=>v+"%%"));
  gy.transition().duration(500).call(d3.axisLeft(y).tickSize(0))
    .selectAll("text").attr("font-size","11px");
  bars.selectAll("rect").data(d,q=>q.k).join(
    e=>e.append("rect").attr("x",M.l).attr("y",q=>y(q.k)).attr("height",y.bandwidth()).attr("rx",2).attr("width",0),
    u=>u, ex=>ex.transition().duration(250).attr("width",0).remove())
    .transition().duration(600)
    .attr("y",q=>y(q.k)).attr("height",y.bandwidth())
    .attr("width",q=>x(q.v)-M.l).attr("fill",color);
  labs.selectAll("text").data(d,q=>q.k).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#555").attr("opacity",0),
    u=>u, ex=>ex.remove())
    .transition().duration(600)
    .attr("x",q=>x(q.v)+6).attr("y",q=>y(q.k)+y.bandwidth()/2+4)
    .attr("opacity",1).text(q=>q.lab);
}
draw(A,"#2c7fb8");
d3.select("#vA").on("click",()=>draw(A,"#2c7fb8"));
d3.select("#vB").on("click",()=>draw(B,"#C41230"));
})();
</script>
', rowsA, rowsB))

## ---- decay-static
par(mfrow = c(1, 2), mar = c(4, 6.5, 2, 1))
k <- order(-big$rate24)
barplot(rev(big$rate24[k]), horiz = TRUE,
        names.arg = rev(substr(big$race[k], 1, 14)), las = 1, cex.names = 0.7,
        col = "#2c7fb8", xlab = "2024 turnout (%)", main = "The finding")
par(mar = c(4, 6.5, 2, 1))
barplot(rev(decay$pct_gone), horiz = TRUE,
        names.arg = rev(sub(" general", "", decay$election)), las = 1,
        cex.names = 0.8, col = "#C41230",
        xlab = "% of voters now missing", main = "The reason it is wrong")
par(mfrow = c(1, 1))

## ---- wrongness
data.frame(
  problem = c("Voted in 2024, has since left the county",
              "Registered in 2025 or 2026",
              "Marked inactive"),
  `what it does` = c("Missing from the numerator AND the denominator",
                     "In the denominator; could not possibly have voted",
                     "In the denominator, or not, depending on the analyst"),
  `pushes the rate` = c("either way",
                        "down",
                        paste0("up by up to ", pc(100 * INACTIVE / ACTIVE), "%")),
  check.names = FALSE)

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#mos text[fill="#222222" i],
#rec text[fill="#222222" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# White labels drawn on pale and mid-toned marks, under 3:1 in BOTH themes.
# A halo fixes them, but the stroke must be dark against a white glyph in
# both themes, and no single token is: on the light page that is var(--ink),
# and on the dark page it is var(--paper). A --paper stroke on the light page
# would make white text worse, not better.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
#mos text[fill="#fff" i],
#mos text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#mos text[fill="#fff" i],
#mos text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
