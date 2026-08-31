# house-competition-code.R -- chunk bodies for house-competition-brief.Rmd
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
b <- read.csv("data/derived/by_year.csv", stringsAsFactors = FALSE)
sp <- b[!is.na(b$pct_split), ]          # years where the split series is defined
S1 <- min(sp$year); S2 <- max(sp$year)
d <- read.csv("data/derived/races.csv",   stringsAsFactors = FALSE)
Y1 <- min(b$year); Y2 <- max(b$year)
v  <- function(yr, col) b[[col]][b$year == yr]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# The share of ALL district-elections in the file with no opponent, pooled over
# every year. The opening sentence quotes it, so it is computed here once and
# used wherever it appears rather than re-derived further down.
UNALL <- 100 * mean(d$uncontested, na.rm = TRUE)
# The two consecutive elections furthest apart on the national series. The point
# is that this gap is bigger than anything the seventy-year trend does.
JUMP  <- which.max(abs(diff(b$pct_uncontested)))
JY1   <- b$year[JUMP]; JY2 <- b$year[JUMP + 1]

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

## ---- one-record
y <- d[d$year == 2014, ]
o <- rbind(head(y[!y$uncontested, ], 2), head(y[y$uncontested, ], 2))
o <- o[, c("year", "state", "stcd", "south", "dv", "incwin", "uncontested", "margin")]
names(o) <- c("year", "state", "district", "South?", "Dem % of two-party vote",
              "incumbent won", "uncontested", "margin from 50")
o

## ---- scope
data.frame(
  quantity = c("Elections covered", "Years", "District-elections",
               "Seats per election", "Source"),
  value = c(nrow(b), paste(Y1, "to", Y2), n(nrow(d)), "435",
            "Jacobson 1946–2014; Clerk of the House 2016–2024"))

## ---- raw-clerk
squash <- function(x) {
  lead <- attr(regexpr("^ *", x), "match.length")
  y <- sub("^ *", "", x)
  y <- gsub("\\.{3,}[. ]*", " ....... ", y)   # shorten the leader dots
  y <- gsub(" {4,}", "   ", y)                # and the gap before the total
  y <- sub(" +$", "", y)
  ind <- ifelse(y == "", 0L, ifelse(lead <= 5, 1L, ifelse(lead <= 12, 4L, 20L)))
  paste0(strrep(" ", ind), y)
}
num  <- function(s) as.numeric(gsub("[^0-9]", "", s))
tail_num <- function(s) num(sub(".*[. ]", "", trimws(s)))
tx24 <- readLines("data/derived/clerk_2024.txt", warn = FALSE)
i24  <- grep("FOR UNITED STATES REPRESENTATIVE", tx24)[1]
cat("```\n", paste(squash(tx24[i24:(i24 + 11)]), collapse = "\n"), "\n```\n",
    sep = "")

## ---- clean-clerk
ch <- read.csv("data/derived/clerk_house.csv", stringsAsFactors = FALSE)
o <- ch[ch$year == 2024 & ch$state == "Alabama",
        c("district", "dem_votes", "rep_votes", "dv", "uncontested")]
o$dem_votes <- n(o$dem_votes); o$rep_votes <- n(o$rep_votes)
o$dv <- ifelse(is.na(o$dv), "—", pc(o$dv, 2))
o$uncontested <- ifelse(o$uncontested == 1, "yes", "no")
names(o) <- c("district", "Democratic votes", "Republican votes",
              "Dem % of two-party vote", "uncontested")
o

## ---- raw-footnote
tx14 <- readLines("data/derived/clerk_2014.txt", warn = FALSE)
i14  <- grep("Ralph Lee Abraham", tx14)[1]
cat("```\n", paste(squash(tx14[i14:(i14 + 2)]), collapse = "\n"), "\n```\n",
    sep = "")

## ---- footnote-cost
MARK <- tail_num(tx14[i14])
REAL <- num(trimws(tx14[i14 + 1]))
la   <- ch[ch$year == 2014 & ch$state == "Louisiana" & ch$district == 5, ]
BADR <- la$rep_votes - REAL + MARK
BADV <- 100 * la$dem_votes / (la$dem_votes + BADR)
data.frame(
  quantity = c("Read literally, Abraham's vote", "What the number really is",
               "District Republican vote, uncorrected",
               "District Republican vote, corrected",
               "Dem % of two-party vote, uncorrected",
               "Dem % of two-party vote, corrected"),
  value = c(n(MARK), n(REAL), n(BADR), n(la$rep_votes),
            paste0(pc(BADV, 2), "%"), paste0(pc(la$dv, 2), "%")))

## ---- defs
data.frame(
  term = c("Uncontested", "Competitive", "Landslide"),
  definition = c("No two-party vote share exists — only one candidate ran",
                 "Winner's share within 5 points of 50%",
                 "Winner's share 20 or more points from 50%"),
  `why this cut` = c("Not a choice; forced by the data",
                     "A margin a normal swing could erase",
                     "A margin no campaign closes"),
  check.names = FALSE)

## ---- headline
data.frame(
  quantity = c(paste("Uncontested races,", Y1), paste("Uncontested races,", Y2),
               "Lowest share, and when", "Highest share, and when",
               "Uncontested races where the incumbent won"),
  value = c(paste0(pc(v(Y1,"pct_uncontested")), "%"),
            paste0(pc(v(Y2,"pct_uncontested")), "%"),
            paste0(pc(min(b$pct_uncontested)), "% in ", b$year[which.min(b$pct_uncontested)]),
            paste0(pc(max(b$pct_uncontested)), "% in ", b$year[which.max(b$pct_uncontested)]),
            paste0(n(sum(d$uncontested & d$incwin == 1, na.rm = TRUE)), " of ",
                   n(sum(d$uncontested, na.rm = TRUE)))))

## ---- inc
un <- sum(d$uncontested, na.rm = TRUE)
iw <- sum(d$uncontested & d$incwin == 1, na.rm = TRUE)
data.frame(
  quantity = c("Uncontested races", "Won by an incumbent", "Share"),
  value = c(n(un), n(iw), paste0(pc(100 * iw / un), "%")))

## ---- region
o <- b[b$year %in% c(1946, 1958, 1970, 1982, 1994, 2006, 2014),
       c("year", "pct_uncontested", "pct_uncontested_south", "pct_uncontested_non_south")]
names(o) <- c("year", "national (%)", "South (%)", "rest of U.S. (%)")
o

## ---- comp
data.frame(
  quantity = c(paste("Competitive (within 5 pts),", Y1),
               paste("Competitive,", Y2), "Fewest competitive, and when",
               paste("Landslides (20+ pts),", Y1), paste("Landslides,", Y2),
               "Most landslides, and when"),
  value = c(paste0(pc(v(Y1,"pct_competitive")), "%"),
            paste0(pc(v(Y2,"pct_competitive")), "%"),
            paste0(pc(min(b$pct_competitive)), "% in ", b$year[which.min(b$pct_competitive)]),
            paste0(pc(v(Y1,"pct_landslide")), "%"),
            paste0(pc(v(Y2,"pct_landslide")), "%"),
            paste0(pc(max(b$pct_landslide)), "% in ", b$year[which.max(b$pct_landslide)])))

## ---- margins-prep
co      <- d[!d$uncontested & !is.na(d$margin), ]
co$dec  <- paste0(10 * (co$year %/% 10), "s")
DEC     <- sort(unique(co$dec))
den     <- lapply(DEC, function(k)
             density(co$margin[co$dec == k], from = 0, to = 50, n = 121, bw = 1.8))
names(den) <- DEC
MAXD    <- max(vapply(den, function(q) max(q$y), 0))
P5      <- vapply(DEC, function(k) 100 * mean(co$margin[co$dec == k] <  5), 0)
P20     <- vapply(DEC, function(k) 100 * mean(co$margin[co$dec == k] >= 20), 0)
NDEC    <- vapply(DEC, function(k) sum(co$dec == k), 0)
UDEC    <- vapply(DEC, function(k)
             100 * mean(d$uncontested[paste0(10 * (d$year %/% 10), "s") == k]), 0)
FILL    <- "#d9d9d9"   # one neutral fill: the decade is already the y position
D5FIRST <- P5[1]; D5LAST <- P5[length(P5)]

## ---- margins-d3
rows <- paste0("{\"k\":\"", DEC,
               "\",\"p5\":\"", formatC(P5, format = "f", digits = 0),
               "\",\"u\":\"", formatC(UDEC, format = "f", digits = 0),
               "\",\"n\":", NDEC,
               ",\"v\":[", vapply(DEC, function(k)
                 paste0("[", formatC(den[[k]]$x, format = "f", digits = 2), ",",
                        formatC(den[[k]]$y / MAXD, format = "f", digits = 4), "]",
                        collapse = ","), ""), "]}", collapse = ",")
cat(paste0('
<div id="mgn" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=[', rows, '];
const W=770,H=470,M={t:58,r:118,b:44,l:56};
const svg=d3.select("#mgn").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,50]).range([M.l,W-M.r]);
const row=(H-M.b-M.t)/S.length, amp=row*1.75;
svg.append("rect").attr("x",x(0)).attr("y",12).attr("width",x(5)-x(0))
  .attr("height",H-M.b-12).attr("fill","#e9f2e4");
svg.append("line").attr("x1",x(20)).attr("x2",x(20)).attr("y1",12).attr("y2",H-M.b)
  .attr("stroke","#8856a7").attr("stroke-dasharray","4 3");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("winner\\u2019s margin from 50%, contested races only");
const ar=d3.area().x(p=>x(p[0])).y1(p=>-p[1]*amp).y0(0).curve(d3.curveBasis);
S.forEach((s,i)=>{
  const gy=M.t+row*(i+0.94);
  const g=svg.append("g").attr("transform","translate(0,"+gy+")");
  g.append("path").datum(s.v).attr("d",ar).attr("fill","#d9d9d9").attr("stroke","#555555")
   .attr("stroke-width",0.9).attr("opacity",0.92);
  g.append("text").attr("x",M.l-8).attr("y",-1).attr("text-anchor","end")
   .attr("font-size","11.5px").attr("fill","#333").text(s.k);
  g.append("text").attr("x",W-M.r+8).attr("y",-4).attr("font-size","10.5px")
   .attr("fill","#4d9221").text(s.p5+"% within 5");
  g.append("text").attr("x",W-M.r+8).attr("y",7).attr("font-size","10.5px")
   .attr("fill","#888").text(s.u+"% had no challenger");
});
svg.append("text").attr("x",x(5)+4).attr("y",22).attr("font-size","11px")
  .attr("fill","#4d9221").attr("font-weight","600").text("within 5 points");
svg.append("text").attr("x",x(20)+4).attr("y",22).attr("font-size","11px")
  .attr("fill","#8856a7").attr("font-weight","600").text("landslide, 20+ points");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Each curve is a decade of contested House races, all drawn to the same height.
The right-hand figures are the share of contested races decided by under five
points, and the share of <em>all</em> races in that decade with no challenger at
all, which are not in the curves.</p>'))

## ---- margins-static
ND  <- length(DEC)
POS <- ND + 1 - seq_along(DEC)          # oldest decade at the top, as in the HTML
par(mar = c(4.0, 5.0, 2.0, 8.4))
plot(NA, xlim = c(0, 50), ylim = c(0.75, ND + 1.5), axes = FALSE,
     xlab = "", ylab = "")
rect(0, 0.75, 5, ND + 1.5, col = "#e2efd9", border = NA)
abline(v = 20, lty = 2, col = "#8856a7")
axis(1, cex.axis = 0.8)
mtext("winner's margin from 50%, contested races only", 1, line = 2.2, cex = 0.85)
for (i in seq_along(DEC)) {
  q  <- den[[DEC[i]]]
  yy <- POS[i] + 1.75 * q$y / MAXD
  polygon(c(q$x, rev(q$x)), c(yy, rep(POS[i], length(q$x))),
          col = FILL, border = "#555555")
}
text(5.4, ND + 1.42, "within 5 points", col = "#4d9221", cex = 0.7, adj = 0)
text(20.4, ND + 1.42, "landslide, 20+ points", col = "#8856a7", cex = 0.7, adj = 0)
axis(2, at = POS, labels = DEC, las = 1, tick = FALSE, cex.axis = 0.82)
text(51, POS + 0.30, paste0(formatC(P5, format = "f", digits = 0),
     "% within 5"), xpd = NA, adj = 0, cex = 0.7, col = "#4d9221")
text(51, POS - 0.02, paste0(formatC(UDEC, format = "f", digits = 0),
     "% had no challenger"), xpd = NA, adj = 0, cex = 0.7, col = "#888888")

## ---- split-cov
o <- b[, c("year", "pct_split", "split_coverage")]
names(o) <- c("year", "split districts (%)", "districts with a presidential figure (%)")
head(o, 6)

## ---- split
data.frame(
  quantity = c(paste("First year the measure exists:", S1),
               paste("Split districts,", S1),
               paste("Split districts,", S2),
               "Highest, and when", "Lowest, and when",
               "Elections where it cannot be computed"),
  value = c("", paste0(pc(sp$pct_split[sp$year == S1]), "%"),
            paste0(pc(sp$pct_split[sp$year == S2]), "%"),
            paste0(pc(max(sp$pct_split)), "% in ", sp$year[which.max(sp$pct_split)]),
            paste0(pc(min(sp$pct_split)), "% in ", sp$year[which.min(sp$pct_split)]),
            sum(is.na(b$pct_split))))

## ---- lines-prep
KS <- c("pct_uncontested", "pct_uncontested_south", "pct_uncontested_non_south",
        "pct_competitive", "pct_landslide", "pct_split")
LB <- c("Uncontested: national average", "Uncontested: South",
        "Uncontested: rest of U.S.", "Competitive (within 5 pts)",
        "Landslides (20+ pts)", "Split districts")
CL <- c("#111111", "#C41230", "#2c7fb8", "#4d9221", "#8856a7", "#e08214")
LW <- c(3.0, 2.2, 2.2, 2.2, 2.2, 2.2)
YMAX <- 5 * ceiling(max(unlist(b[KS]), na.rm = TRUE) / 5)   # spans every series

## ---- lines-static
plot(NA, xlim = range(b$year), ylim = c(0, YMAX), las = 1, xlab = "",
     ylab = "% of House races")
for (i in seq_along(KS)) lines(b$year, b[[KS[i]]], col = CL[i], lwd = LW[i])
# pct_split is NA where no presidential-by-district figure exists; lines() skips it
legend("topright", LB, col = CL, lwd = LW, bty = "n", cex = 0.72)
mtext(paste("Split districts cannot be computed for", sum(is.na(b$pct_split)),
            "elections and are drawn only where they exist."),
      side = 1, line = 2.4, cex = 0.72, col = "#555555")

## ---- d3-lines
# NA must leave R as JSON null: an "NA" in a JavaScript array literal is an
# undefined identifier and kills the whole figure.
jnum <- function(x) ifelse(is.na(x), "null", formatC(x, format = "f", digits = 1))
ser <- paste0("{\"k\":\"", LB, "\",\"c\":\"", CL, "\",\"w\":", LW, ",\"v\":[",
              vapply(KS, function(k)
                paste0("[", b$year, ",", jnum(b[[k]]), "]", collapse = ","), ""),
              "]}", collapse = ",")
cat(paste0('
<div id="hc" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const S=[', ser, '];
const YMAX=', YMAX, ', NSPLIT=', sum(is.na(b$pct_split)), ';
const W=770,H=440,M={t:18,r:24,b:52,l:52};
const svg=d3.select("#hc").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const yrs=S[0].v.map(p=>p[0]);
const x=d3.scaleLinear().domain(d3.extent(yrs)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,YMAX]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(12));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(7));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("% of House races");
svg.append("text").attr("x",M.l).attr("y",H-10).attr("font-size","11px").attr("fill","#666")
  .text("Split districts cannot be computed for "+NSPLIT+
        " elections and are drawn only where they exist.");
const ln=d3.line().defined(p=>p[1]!==null).x(p=>x(p[0])).y(p=>y(p[1]));
const on={}; S.forEach(s=>on[s.k]=true);
const g=svg.append("g");
function draw(){
  g.selectAll("path").data(S.filter(s=>on[s.k]),s=>s.k).join("path")
   .attr("fill","none").attr("stroke",s=>s.c).attr("stroke-width",s=>s.w)
   .attr("d",s=>ln(s.v));
}
draw();
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#bbb").attr("opacity",0);
const dots=svg.append("g");
const tip=d3.select("#hc").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l).attr("height",H-M.b-M.t)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const px=d3.pointer(e,this)[0]+M.l;
    const yr=yrs.reduce((a,c)=>Math.abs(x(c)-px)<Math.abs(x(a)-px)?c:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const rows=S.filter(s=>on[s.k]).map(function(s){
        const p=s.v.find(q=>q[0]===yr);
        return (p&&p[1]!==null)?{k:s.k,c:s.c,val:p[1]}:null;})
      .filter(Boolean).sort((a,b)=>b.val-a.val);
    dots.selectAll("circle").data(rows).join("circle")
      .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",4).attr("fill",r=>r.c);
    tip.style("opacity",1).html("<b>"+yr+"</b><br>"+
      rows.map(r=>"<span style=\\"color:"+r.c+"\\">\\u25a0</span> "+r.k+": "+r.val+"%").join("<br>"))
      .style("left",Math.min(x(yr)-M.l+18,W-300)+"px").style("top",(M.t+4)+"px");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);dots.selectAll("circle").remove();tip.style("opacity",0);});
const leg=d3.select("#hc").append("div").attr("style","margin-top:6px;font-size:12px");
leg.selectAll("span").data(S).join("span")
  .attr("style",s=>"display:inline-block;margin-right:12px;cursor:pointer;color:"+s.c+";font-weight:600")
  .html(s=>"\\u25a0 "+s.k)
  .on("click",function(e,s){ on[s.k]=!on[s.k];
    d3.select(this).style("opacity",on[s.k]?1:0.35); draw(); });
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across for a year-by-year readout. Click a label to hide or show a series.
The heavy black line is the national uncontested share: it is the average of the
two colored uncontested lines, and it sits between them for the whole period.</p>'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
