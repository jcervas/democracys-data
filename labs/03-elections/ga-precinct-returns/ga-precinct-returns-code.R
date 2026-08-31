# ga-precinct-returns-code.R -- chunk bodies for ga-precinct-returns-brief.Rmd
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

p  <- read.csv("data/derived/precincts.csv", stringsAsFactors = FALSE, check.names = FALSE)
co <- read.csv("data/derived/counties.csv",  stringsAsFactors = FALSE, check.names = FALSE)
sx <- read.csv("data/derived/structure.csv", stringsAsFactors = FALSE)
rc <- read.csv("data/derived/recount.csv",   stringsAsFactors = FALSE)
vm <- read.csv("data/derived/ga2020_vote_methods.csv", stringsAsFactors = FALSE)

DEM <- "Joseph R. Biden"; REP <- "Donald J. Trump"; LIB <- "Jo Jorgensen"
NPREC <- nrow(p); NCO <- nrow(co)
TOTD <- sum(p[[DEM]]); TOTR <- sum(p[[REP]]); TOTL <- sum(p[[LIB]])
TOTV <- sum(p$total)
MARGIN <- TOTD - TOTR

np <- p[p$total > 0, ]

# where the extremes actually are: the tail is rural, the cap is metropolitan
ONEPREC  <- co$county[co$precincts == 1]
METRO    <- c("Fulton", "DeKalb", "Gwinnett", "Cobb", "Clayton")
BIGROW   <- np[which.max(np$total), ]
METROMAX <- max(np$total[np$county %in% METRO])
METROMED <- median(np$total[np$county %in% METRO])
RESTMED  <- median(np$total[!np$county %in% METRO])
MOSTPREC <- co$county[which.max(co$precincts)]

# vote methods
mm <- tapply(vm$votes, list(vm$candidate, vm$method), sum)
meth <- colnames(mm)
short <- sub(" Votes$", "", meth)
mdem <- mm[DEM, ]; mrep <- mm[REP, ]
mshare <- 100 * mdem / (mdem + mrep)
mtot <- colSums(mm)
ED <- grep("Election Day", meth, value = TRUE)
MB <- grep("Absentee by Mail", meth, value = TRUE)
AV <- grep("Advanced", meth, value = TRUE)

# recount
tot <- aggregate(cbind(original, recount) ~ candidate, rc, sum)
tot$change <- tot$recount - tot$original
RCD <- tot$recount[tot$candidate == DEM]; RCR <- tot$recount[tot$candidate == REP]
RCMARGIN <- RCD - RCR
CHANGED <- length(unique(rc$county[rc$change != 0]))
NCTY <- length(unique(rc$county))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- vintage-setup
s24 <- read.csv("data/derived/ga2024_structure.csv", stringsAsFactors = FALSE)
g <- function(d, k) d$value[d$item == k]

## ---- clean-precincts
o <- p[p$county == "Baker", c("county", "precinct", "registered",
                              "ballots_cast", REP, DEM, "total",
                              "dem_two_party_pct")]
o$registered <- n(o$registered); o$ballots_cast <- n(o$ballots_cast)
o[[REP]] <- n(o[[REP]]); o[[DEM]] <- n(o[[DEM]]); o$total <- n(o$total)
names(o) <- c("county", "precinct", "registered", "ballots cast", "Trump",
              "Biden", "presidential votes", "Dem two-party %")
o

## ---- clean-methods
o <- vm[vm$county == "Baker" & vm$precinct == "Anna", ]
o <- o[order(o$candidate, o$method), c("county", "precinct", "candidate",
                                       "party", "method", "votes")]
o <- o[o$candidate %in% c(DEM, REP), ]
o

## ---- vintages
data.frame(
  the_same_thing = c("the county", "the office", "how a ballot was cast",
                     "the candidate", "registration per precinct"),
  in_2020 = c("Baker", g(sx, "contest"), "Election Day Votes",
              "Donald J. Trump (I) (Rep)", "reported"),
  in_2024 = c("Baker County", g(s24, "contest"), "Election Day",
              "Donald J. Trump (Rep)", "absent"))

## ---- one-row
o <- head(p[p$county == "Appling", ], 3)
o <- o[, c("county", "precinct", "registered", "ballots_cast", DEM, REP,
           "total", "dem_two_party_pct")]
o$registered <- n(o$registered); o$ballots_cast <- n(o$ballots_cast)
o[[DEM]] <- n(o[[DEM]]); o[[REP]] <- n(o[[REP]]); o$total <- n(o$total)
names(o) <- c("county", "precinct", "registered", "ballots cast",
              "Biden", "Trump", "presidential votes", "Dem two-party %")
o

## ---- structure
o <- sx
names(o) <- c("what the source contains", "value")
o

## ---- sizes
data.frame(
  quantity = c("Precincts with at least one presidential vote",
               "Smallest", "25th percentile", "Median", "75th percentile",
               "Largest", "Precincts recording zero presidential votes"),
  value = c(n(nrow(np)), n(min(np$total)),
            n(quantile(np$total, .25)), n(median(np$total)),
            n(quantile(np$total, .75)), n(max(np$total)),
            n(sum(p$total == 0))))

## ---- biggest
o <- head(np[order(-np$total), c("county", "precinct", "total")], 6)
o$one <- ifelse(o$county %in% ONEPREC, "yes", "no")
o$total <- n(o$total)
names(o) <- c("county", "precinct", "presidential votes",
              "whole county is one precinct")
o

## ---- county-spread
o <- rbind(head(co[order(-co$precincts), c("county", "precincts", "total")], 4),
           head(co[order(co$precincts),  c("county", "precincts", "total")], 3))
o$total <- n(o$total)
names(o) <- c("county", "precincts", "presidential votes")
o

## ---- totals
data.frame(
  candidate = c(DEM, REP, LIB, "Total presidential votes",
                "Two-party margin"),
  votes = c(n(TOTD), n(TOTR), n(TOTL), n(TOTV), n(MARGIN)))

## ---- methods
o <- data.frame(method = short,
                votes = n(as.vector(mtot)),
                share = paste0(pc(100 * as.vector(mtot) / sum(mtot)), "%"),
                stringsAsFactors = FALSE)
names(o) <- c("how the ballot was cast", "votes", "% of all votes")
o[order(-mtot), ]

## ---- method-split
o <- data.frame(method = short,
                dem = n(as.vector(mdem)), rep = n(as.vector(mrep)),
                share = paste0(pc(as.vector(mshare)), "%"),
                stringsAsFactors = FALSE)
names(o) <- c("how the ballot was cast", sub(" .*", "", DEM),
              sub(" .*", "", REP), "Dem two-party %")
o[order(-mtot), ]

## ---- d3-methods
ord <- order(-mtot)
rows <- paste(sprintf('{"m":"%s","d":%d,"r":%d,"s":%.1f}',
                      short[ord], mdem[ord], mrep[ord], mshare[ord]),
              collapse = ",")
cat(sprintf('
<div id="gam" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const d=[%s];
const W=760,H=360,M={t:26,r:24,b:40,l:150};
const svg=d3.select("#gam").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y0=d3.scaleBand().domain(d.map(q=>q.m)).range([M.t,H-M.b]).padding(0.28);
const y1=d3.scaleBand().domain(["d","r"]).range([0,y0.bandwidth()]).padding(0.12);
const x=d3.scaleLinear().domain([0,d3.max(d,q=>Math.max(q.d,q.r))*1.14]).range([M.l,W-M.r]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d3.format(".2s")));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y0).tickSize(0))
  .selectAll("text").attr("font-size","11px");
const col={d:"#2166AC",r:"#B2182B"};
const g=svg.append("g").selectAll("g").data(d).join("g")
  .attr("transform",q=>`translate(0,${y0(q.m)})`);
g.selectAll("rect").data(q=>[{k:"d",v:q.d},{k:"r",v:q.r}]).join("rect")
  .attr("x",M.l).attr("y",q=>y1(q.k)).attr("height",y1.bandwidth()).attr("rx",1)
  .attr("fill",q=>col[q.k]).attr("width",0)
  .transition().duration(700).attr("width",q=>x(q.v)-M.l);
g.append("text").attr("x",q=>x(Math.max(q.d,q.r))+7)
  .attr("y",y0.bandwidth()/2+4).attr("font-size","11px").attr("fill","#555")
  .text(q=>q.s.toFixed(1)+"%% Dem two-party");
svg.append("rect").attr("x",M.l).attr("y",6).attr("width",10).attr("height",10).attr("fill",col.d);
svg.append("text").attr("x",M.l+15).attr("y",15).attr("font-size","11px").text("Biden");
svg.append("rect").attr("x",M.l+70).attr("y",6).attr("width",10).attr("height",10).attr("fill",col.r);
svg.append("text").attr("x",M.l+85).attr("y",15).attr("font-size","11px").text("Trump");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The same electorate, split by how the ballot was cast. No other precinct dataset
carries this column.</p>
', rows))

## ---- methods-static
ord <- order(-mtot)
par(mar = c(4, 9, 1, 2))
barplot(rbind(rev(mdem[ord]), rev(mrep[ord])) / 1000, beside = TRUE,
        horiz = TRUE, names.arg = rev(short[ord]), las = 1, cex.names = 0.8,
        col = c("#2166AC", "#B2182B"), xlab = "votes (thousands)")
legend("bottomright", c("Biden", "Trump"), fill = c("#2166AC", "#B2182B"),
       bty = "n", cex = 0.85)

## ---- counterfactual
data.frame(
  `if only these ballots counted` = c(paste0(short[match(ED, meth)], " only"),
                                      paste0(short[match(AV, meth)], " only"),
                                      paste0(short[match(MB, meth)], " only"),
                                      "All ballots"),
  `two-party margin` = c(
    paste0(sub(" .*", "", REP), " +", n(mrep[ED] - mdem[ED])),
    paste0(sub(" .*", "", REP), " +", n(mrep[AV] - mdem[AV])),
    paste0(sub(" .*", "", DEM), " +", n(mdem[MB] - mrep[MB])),
    paste0(sub(" .*", "", DEM), " +", n(MARGIN))),
  check.names = FALSE)

## ---- recount-tot
o <- tot
o$original <- n(o$original); o$recount <- n(o$recount)
o$change <- ifelse(tot$change > 0, paste0("+", n(tot$change)), n(tot$change))
names(o) <- c("candidate", "original count", "recount", "change")
o

## ---- recount-summary
data.frame(
  quantity = c("Counties in the comparison", "Counties where the count changed",
               "Share of counties", "Total votes moved (absolute)",
               "Margin, original count", "Margin, recount",
               "Change in the margin"),
  value = c(NCTY, CHANGED, paste0(pc(100 * CHANGED / NCTY), "%"),
            n(sum(abs(rc$change))), n(MARGIN), n(RCMARGIN),
            n(RCMARGIN - MARGIN)))

## ---- recount-detail
o <- head(rc[order(-abs(rc$change)), ], 6)
o$original <- n(o$original); o$recount <- n(o$recount)
o$change <- ifelse(o$change > 0, paste0("+", n(o$change)), n(o$change))
o <- o[, c("county", "candidate", "original", "recount", "change", "pct_change")]
names(o) <- c("county", "candidate", "original", "recount", "change", "% change")
o

## ---- nesting
data.frame(
  unit = c("Census block", "Precinct"),
  `drawn by` = c("The Census Bureau", "County election officials"),
  `drawn when` = c("Once a decade", "Whenever the county needs to"),
  `must respect the other?` = c("No", "No"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
