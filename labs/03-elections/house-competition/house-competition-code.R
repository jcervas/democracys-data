# house-competition-code.R -- chunk bodies for house-competition-brief.Rmd
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
b  <- read.csv("data/derived/by_year.csv",     stringsAsFactors = FALSE)
d  <- read.csv("data/derived/races.csv",       stringsAsFactors = FALSE)
ch <- read.csv("data/derived/clerk_house.csv", stringsAsFactors = FALSE)
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
# Uncontested races, and how many had a sitting member in them.
un <- sum(d$uncontested, na.rm = TRUE)
iw <- sum(d$uncontested & d$incwin == 1, na.rm = TRUE)

# ---- the Clerk excerpts ----------------------------------------------------
# Both verbatim blocks below are read from the extracted text at render time.
# squash() shortens the leader dots and the gap before each total, because in
# the file every line is about 190 characters wide; nothing else is touched.
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
tx14 <- readLines("data/derived/clerk_2014.txt", warn = FALSE)
i14  <- grep("Ralph Lee Abraham", tx14)[1]
MARK <- tail_num(tx14[i14])          # the footnote marker, read as a total
REAL <- num(trimws(tx14[i14 + 1]))   # the vote count on the line below it

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

## ---- raw-clerk
cat("```\n", paste(squash(tx24[i24:(i24 + 11)]), collapse = "\n"), "\n```\n",
    sep = "")

## ---- clean-clerk
o <- ch[ch$year == 2024 & ch$state == "Alabama",
        c("district", "dem_votes", "rep_votes", "dv", "uncontested")]
o$dem_votes <- n(o$dem_votes); o$rep_votes <- n(o$rep_votes)
o$dv <- ifelse(is.na(o$dv), "—", pc(o$dv, 2))
o$uncontested <- ifelse(o$uncontested == 1, "yes", "no")
names(o) <- c("district", "Democratic votes", "Republican votes",
              "Dem % of two-party vote", "uncontested")
o

## ---- raw-footnote
cat("```\n", paste(squash(tx14[i14:(i14 + 2)]), collapse = "\n"), "\n```\n",
    sep = "")

## ---- headline
data.frame(
  quantity = c(paste("Uncontested races,", Y1), paste("Uncontested races,", Y2),
               "Lowest share, and when", "Highest share, and when",
               "Uncontested races where the incumbent won"),
  value = c(paste0(pc(v(Y1,"pct_uncontested")), "%"),
            paste0(pc(v(Y2,"pct_uncontested")), "%"),
            paste0(pc(min(b$pct_uncontested)), "% in ", b$year[which.min(b$pct_uncontested)]),
            paste0(pc(max(b$pct_uncontested)), "% in ", b$year[which.max(b$pct_uncontested)]),
            paste0(n(iw), " of ", n(un))))

## ---- region
o <- b[b$year %in% c(1946, 1958, 1970, 1982, 1994, 2006, 2014),
       c("year", "pct_uncontested", "pct_uncontested_south", "pct_uncontested_non_south")]
names(o) <- c("year", "national (%)", "South (%)", "rest of U.S. (%)")
o

## ---- unc-d3
# Drawn with the shared library (_lib/dd-charts.js); dd_fig() emits the two
# <script src> tags for the document, so the hand-written figure below rides
# on the d3 it loads here. The red and blue are series classes, not parties:
# both lines count the same kind of race, in two halves of the country.
m <- b[, c("year", "pct_uncontested", "pct_uncontested_south",
           "pct_uncontested_non_south")]
dd_fig("unc", "line", m,
  size = list(w = 770, h = 430, m = list(t = 16, r = 24, b = 40, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "pct_uncontested", label = "% of House races with one candidate",
           domain = c(0, 70), fmt = "pct0", ticks = 7),
  series = list(fields = list(
    list(field = "pct_uncontested_south", label = "the South",
         class = "series-2", width = 2.2),
    list(field = "pct_uncontested_non_south", label = "rest of the country",
         class = "series-1", width = 2.2),
    list(field = "pct_uncontested", label = "national average",
         class = "series-3", width = 3))),
  legend = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> the South: "+
        d.pct_uncontested_south.toFixed(1)+"%<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> rest of the country: "+
        d.pct_uncontested_non_south.toFixed(1)+"%<br>"+
      "<span class=\'series-3-txt\'>&#9632;</span> national average: "+
        d.pct_uncontested.toFixed(1)+"%";
  }'))

## ---- unc-static
par(mar = c(2.6, 4.0, 0.8, 0.8), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = range(b$year), ylim = c(0, 70), las = 1, xlab = "",
     ylab = "% of House races with one candidate")
lines(b$year, b$pct_uncontested_south,     col = "#C41230", lwd = 2.2)
lines(b$year, b$pct_uncontested_non_south, col = "#2c7fb8", lwd = 2.2)
lines(b$year, b$pct_uncontested,           col = "#111111", lwd = 3.0)
legend("topright", c("the South", "rest of the country", "national average"),
       col = c("#C41230", "#2c7fb8", "#111111"), lwd = c(2.2, 2.2, 3),
       bty = "n", cex = 0.78)

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
# The d3 <script src> is emitted by the dd_fig() figure above; loading it a
# second time here would silently double the payload once pandoc inlines it.
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
</script>'))

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

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
