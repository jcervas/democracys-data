# lobbying-code.R -- chunk bodies for lobbying-brief.Rmd
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
fl <- read.csv("data/derived/filings.csv",   stringsAsFactors = FALSE)
lo <- read.csv("data/derived/lobbyists.csv", stringsAsFactors = FALSE)
is <- read.csv("data/derived/issues.csv",    stringsAsFactors = FALSE)
en <- read.csv("data/derived/entities.csv",  stringsAsFactors = FALSE)
lo$has <- nchar(lo$former_government_position) > 0
paid   <- fl[fl$amount > 0, ]
# The number of filings the Senate received for the quarter. This one figure is
# not derivable from the retrieved sample -- it is the count the database itself
# reports for Q2 2024, recorded by the build at the moment it fetched.
QTOTAL <- read.csv("data/derived/quarter.csv")$filings_in_quarter
# the single row of filings.csv that the reproduced LD-2 in Figure 2 IS
LD2ROW <- which(grepl("MILLCREEK", toupper(fl$client)) &
                grepl("BARKER LEAVITT", toupper(fl$registrant)))
LD2    <- fl[LD2ROW, ]
stopifnot(nrow(LD2) == 1, LD2$n_issues == 2)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")
d  <- function(x) paste0("$", n(x))

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

## ---- clean-lobby
o <- LD2[, c("client", "registrant", "client_state", "amount", "n_issues",
             "n_lobbyists")]
o$amount <- d(o$amount)
names(o) <- c("client", "registrant", "client state", "amount", "issues",
              "lobbyists")
o

## ---- one-filing
o <- head(paid[order(-paid$amount), ][3:5, ],  3)
o <- o[, c("client", "registrant", "client_state", "amount", "n_issues", "n_lobbyists")]
o$amount <- d(o$amount)
names(o) <- c("client (who pays)", "registrant (who lobbies)", "state",
              "reported for the quarter", "issue areas", "lobbyists named")
o

## ---- sample
data.frame(
  quantity = c("Filings in the quarter (Q2 2024)", "Filings retrieved here",
               "Share", "Why not all of them"),
  value = c(n(QTOTAL), n(nrow(fl)), paste0(pc(100*nrow(fl)/QTOTAL), "%"),
            "API returns 25 records per request to anonymous users"))

## ---- money
data.frame(
  quantity = c("Filings retrieved", "Reporting a dollar figure of $0",
               "Reporting a positive amount", "Smallest positive amount",
               "Median", "Largest"),
  value = c(n(nrow(fl)), n(sum(fl$amount == 0)), n(nrow(paid)),
            d(min(paid$amount)), d(median(paid$amount)), d(max(paid$amount))))

## ---- rounding
data.frame(
  quantity = c("Positive filings", "Amounts that are a multiple of $10,000",
               "Amounts that are a multiple of $5,000"),
  value = c(n(nrow(paid)),
            paste0(n(sum(paid$amount %% 10000 == 0)), "  (",
                   pc(100*mean(paid$amount %% 10000 == 0)), "%)"),
            paste0(n(sum(paid$amount %% 5000 == 0)), "  (",
                   pc(100*mean(paid$amount %% 5000 == 0)), "%)")))

## ---- heap-prep
MONEY <- "#1b7837"                       # green means money everywhere below
XMAX  <- 150000
hp    <- as.data.frame(table(paid$amount), stringsAsFactors = FALSE)
names(hp) <- c("amt", "n"); hp$amt <- as.numeric(hp$amt)
hp$rnd  <- hp$amt %% 10000 == 0
hp$show <- hp$amt <= XMAX
NABOVE  <- sum(paid$amount > XMAX)
NDIST   <- length(unique(paid$amount))
TOP3    <- hp[order(-hp$n), ][1:3, ]
THRESH  <- min(paid$amount)
# The note printed under the figure is built ONCE here, as a string, and handed
# to both renderers. Formatting it twice -- once in R and once in JavaScript --
# is how the browser version and the print version quietly come to disagree
# about a number that is supposed to be the same number.
XMAXLAB <- d(XMAX)
ROUND   <- 10000
ROUNDLAB <- paste0("Green: a multiple of ", d(ROUND), ". Gray: anything else.")
HEAPNOTE <- paste0(NDIST, " distinct figures for ", n(nrow(paid)),
                   " filings; ", NABOVE, " filings above ", XMAXLAB,
                   " are off the right of this axis.")

## ---- heap-d3
rows <- paste0("[", hp$amt[hp$show], ",", hp$n[hp$show], ",",
               as.integer(hp$rnd[hp$show]), "]", collapse = ",")
top  <- paste0("[", TOP3$amt, ",", TOP3$n, "]", collapse = ",")
cat(paste0('
<div id="hp" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '].map(r=>({a:r[0],n:r[1],r:r[2]===1}));
const TOP=[', top, '];
const XMAX=', XMAX, ', THRESH=', THRESH, ';
// One string, formatted once in R, used by this figure and by its printed twin.
const NOTE="', HEAPNOTE, '";
const W=770,H=350,M={t:26,r:22,b:58,l:52};
const svg=d3.select("#hp").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,XMAX]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.n)*1.14]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(7).tickFormat(d=>"$"+d3.format(",")(d)));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("filings reporting exactly that figure");
const g=svg.append("g");
D.forEach(function(d){
  g.append("line").attr("x1",x(d.a)).attr("x2",x(d.a)).attr("y1",y(0)).attr("y2",y(d.n))
   .attr("stroke",d.r?"', MONEY, '":"#bbb").attr("stroke-width",d.r?2.4:1.4);
  g.append("circle").attr("cx",x(d.a)).attr("cy",y(d.n)).attr("r",d.r?3:2)
   .attr("fill",d.r?"', MONEY, '":"#bbb");
});
TOP.forEach(function(t){
  svg.append("text").attr("x",x(t[0])).attr("y",y(t[1])-8).attr("text-anchor","middle")
    .attr("font-size","11px").attr("font-weight","600").attr("fill","', MONEY, '")
    .text("$"+d3.format(",")(t[0])+": "+t[1]);
});
svg.append("line").attr("x1",x(THRESH)).attr("x2",x(THRESH)).attr("y1",M.t-6)
  .attr("y2",H-M.b).attr("stroke","#111").attr("stroke-dasharray","3 3");
svg.append("text").attr("x",x(THRESH)+6).attr("y",M.t-8).attr("font-size","11px")
  .attr("fill","#111").text("$"+d3.format(",")(THRESH)+": the statutory reporting threshold");
svg.append("text").attr("x",M.l).attr("y",H-24).attr("font-size","11px").attr("fill","#666")
  .text("', ROUNDLAB, '");
svg.append("text").attr("x",M.l).attr("y",H-10).attr("font-size","11px").attr("fill","#666")
  .text(NOTE);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
A real price distribution is smooth. This one lands on the round numbers a
person reaches for when asked to estimate.</p>'))

## ---- heap-static
hs <- hp[hp$show, ]
par(mar = c(5.4, 4.2, 1.6, 1.0))
plot(NA, xlim = c(0, XMAX), ylim = c(0, max(hs$n) * 1.14), xlab = "", ylab = "",
     las = 1, xaxt = "n", cex.axis = 0.8)
axis(1, at = seq(0, XMAX, 25000),
     labels = paste0("$", format(seq(0, XMAX, 25000), big.mark = ",", trim = TRUE)),
     cex.axis = 0.78)
mtext("filings reporting exactly that figure", 2, line = 2.8, cex = 0.85)
segments(hs$amt, 0, hs$amt, hs$n, col = ifelse(hs$rnd, MONEY, "#bbbbbb"),
         lwd = ifelse(hs$rnd, 2.4, 1.2))
points(hs$amt, hs$n, pch = 19, cex = ifelse(hs$rnd, 0.5, 0.3),
       col = ifelse(hs$rnd, MONEY, "#bbbbbb"))
text(TOP3$amt, TOP3$n + max(hs$n) * 0.05, paste0(d(TOP3$amt), ": ", TOP3$n),
     cex = 0.66, font = 2, col = MONEY)
abline(v = THRESH, lty = 3)
text(THRESH + 2500, max(hs$n) * 1.10, paste0(d(THRESH), ": the statutory\nreporting threshold"),
     adj = 0, cex = 0.66)
mtext(ROUNDLAB, side = 1, line = 2.6, cex = 0.7, col = "#555555")
mtext(HEAPNOTE, side = 1, line = 3.5, cex = 0.7, col = "#555555")

## ---- ld2
knitr::include_graphics("img/ld2-millcreek-2024q2.png")

## ---- multi
mx <- paid[which.max(paid$n_issues), ]
data.frame(
  quantity = c("Positive filings covering more than one issue area",
               "Share of positive filings",
               "Most issue areas on a single filing",
               "That filing's client", "That filing's reported amount",
               "Dollars attributable to any one of those issues"),
  value = c(n(sum(paid$n_issues > 1)),
            paste0(pc(100*mean(paid$n_issues > 1)), "%"),
            max(paid$n_issues), mx$client, d(mx$amount), "unknown"))

## ---- issues
o <- head(is, 10)
names(o) <- c("issue area", "times mentioned")
o

## ---- entities
o <- head(en, 8)
names(o) <- c("government entity contacted", "times mentioned")
o

## ---- issues-static
top <- head(is, 14)   # same 14 the D3 view shows
par(mar = c(4, 13, 1, 2))
barplot(rev(top$mentions), horiz = TRUE, names.arg = rev(substr(top$issue, 1, 32)),
        las = 1, cex.names = 0.75, col = "#2c7fb8", xlab = "activity mentions")

## ---- entities-static
# second view of the toggle: the government-entity field
tope <- head(en, 14)
nm <- substr(sub(" \\([A-Z]+\\)$", "", tope$entity), 1, 34)  # drop acronym suffix
par(mar = c(4, 14, 2, 2))
barplot(rev(tope$mentions), horiz = TRUE, names.arg = rev(nm),
        las = 1, cex.names = 0.7, col = "#C41230", xlab = "times mentioned",
        main = "Parts of government contacted", cex.main = 0.9)

## ---- d3-toggle
ti <- head(is, 14); te <- head(en, 14)
ri <- paste(sprintf('{"k":"%s","v":%d}', gsub('"', "", ti$issue), ti$mentions), collapse = ",")
re <- paste(sprintf('{"k":"%s","v":%d}', gsub('"', "", te$entity), te$mentions), collapse = ",")
cat(sprintf('
<div id="lob" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="bI" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">Issues lobbied on</button>
  <button id="bE" style="font:12px inherit;padding:4px 10px;cursor:pointer">Parts of government contacted</button>
 </div>
</div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const issues=[%s], ents=[%s];
const W=760,H=430,M={t:14,r:70,b:34,l:230};
const svg=d3.select("#lob").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().range([M.l,W-M.r]);
const y=d3.scaleBand().range([M.t,H-M.b]).padding(0.18);
const gx=svg.append("g").attr("transform",`translate(0,${H-M.b})`);
const gy=svg.append("g").attr("transform",`translate(${M.l},0)`);
const bars=svg.append("g"); const labs=svg.append("g");
function draw(data,color){
  const d=data.slice().sort((a,b)=>b.v-a.v);
  x.domain([0,d3.max(d,q=>q.v)*1.08]); y.domain(d.map(q=>q.k));
  gx.transition().duration(500).call(d3.axisBottom(x).ticks(6));
  gy.transition().duration(500).call(d3.axisLeft(y).tickSize(0))
    .selectAll("text").attr("font-size","11px");
  bars.selectAll("rect").data(d,q=>q.k).join(
    e=>e.append("rect").attr("x",M.l).attr("height",y.bandwidth()).attr("rx",2)
        .attr("y",q=>y(q.k)).attr("width",0),
    u=>u, ex=>ex.transition().duration(300).attr("width",0).remove())
    .transition().duration(600)
    .attr("y",q=>y(q.k)).attr("height",y.bandwidth())
    .attr("width",q=>x(q.v)-M.l).attr("fill",color);
  labs.selectAll("text").data(d,q=>q.k).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#555").attr("opacity",0),
    u=>u, ex=>ex.remove())
    .transition().duration(600)
    .attr("x",q=>x(q.v)+6).attr("y",q=>y(q.k)+y.bandwidth()/2+4)
    .attr("opacity",1).text(q=>d3.format(",")(q.v));
}
draw(issues,"#2c7fb8");
d3.select("#bI").on("click",()=>draw(issues,"#2c7fb8"));
d3.select("#bE").on("click",()=>draw(ents,"#C41230"));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Switch between what lobbyists reported working on and which institutions they
reported contacting.</p>
', ri, re))

## ---- revolving
data.frame(
  quantity = c("Individual lobbyists named", "Disclosing a former government post",
               "Share"),
  value = c(n(nrow(lo)), n(sum(lo$has)), paste0(pc(100*mean(lo$has)), "%")))

## ---- positions
p <- lo$former_government_position[lo$has]
pick <- function(s) p[grep(s, p, fixed = TRUE)][1]
o <- data.frame(`disclosed former position` = c(
  pick("John Cornyn"), pick("Josh Hawley"), pick("Assistant Secretary, HUD"),
  pick("Detailee"), pick("Rush Holt")), check.names = FALSE)
o

## ---- junk
data.frame(`also found in the same field` =
  c("Sr. Vice President", "President and CEO", "Consultant",
    "Associate Vice President for Public Policy",
    "Senior Director, Government Affairs"), check.names = FALSE)

## ---- asym
data.frame(
  field = c("Former government position", "Amount paid"),
  precision = c("Names the specific member, committee or agency",
                "One number per filing, undivided across issues"),
  `usable in aggregate?` = c("No — free text, no vocabulary",
                             "No — cannot be attributed to an issue"),
  `required by` = c("Lobbying Disclosure Act", "Lobbying Disclosure Act"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
