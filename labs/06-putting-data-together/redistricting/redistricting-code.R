# redistricting-code.R -- chunk bodies for redistricting-brief.Rmd
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
cd <- read.csv("data/derived/pres_by_cd_2024.csv", stringsAsFactors = FALSE)
dev  <- read.csv("data/derived/deviation.csv", stringsAsFactors = FALSE)
devs <- read.csv("data/derived/deviation_states.csv", stringsAsFactors = FALSE)
DV <- function(plan, col) dev[[col]][dev$plan == plan]
DS <- function(ab, col) devs[[col]][devs$state == ab]

st <- do.call(rbind, lapply(split(cd, cd$state), function(s) {
  v <- s$dem_share; b <- v[v < 50]; a <- v[v > 50]
  data.frame(state = s$state[1], n = nrow(s),
             vote = round(mean(v), 1), seats = sum(v > 50),
             seat_pct = round(100 * mean(v > 50), 1),
             below = if (length(b)) max(b) else NA_real_,
             above = if (length(a)) min(a) else NA_real_,
             hole  = if (length(b) && length(a)) min(a) - max(b) else NA_real_)
}))
st$eg <- round((st$seat_pct - 50) - 2 * (st$vote - 50), 1)
big <- st[st$n >= 6, ]
sv  <- function(ab, v) big[[v]][big$state == ab]

comp <- do.call(rbind, lapply(c(2, 5, 10, 15), function(b)
  data.frame(band = paste0("within ", b, " points of even"),
             districts = sum(abs(cd$dem_share - 50) <= b),
             pct = 100 * mean(abs(cd$dem_share - 50) <= b))))

cross <- cd[!is.na(cd$house_rep_party) & cd$pres_party != cd$house_rep_party, ]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)

# ---- the national distribution of districts ----
brk <- seq(14, 92, by = 2)
hna <- hist(cd$dem_share, breaks = brk, plot = FALSE)

# ---- three states, drawn as densities over the same axis ----
ridge <- c("NC", "WI", "PA")
dens  <- lapply(ridge, function(a)
  density(cd$dem_share[cd$state == a], bw = 3, from = 18, to = 94, n = 180))
names(dens) <- ridge
dmax <- max(sapply(dens, function(d) max(d$y)))

# ---- the two statistics side by side ----
sc  <- big[!is.na(big$hole), ]
nh  <- big[is.na(big$hole), ]

# ---- the seat-vote curve ----
natl  <- mean(cd$dem_share)
swing <- seq(35, 65, by = 0.25) - natl
svc   <- data.frame(vote  = natl + swing,
                    seats = sapply(swing, function(s)
                              100 * mean(cd$dem_share + s > 50)))
seats50 <- 100 * mean(cd$dem_share + (50 - natl) > 50)
vote50  <- min(svc$vote[svc$seats >= 50])
row_of <- function(ab) paste(sprintf("%.1f",
  sort(cd$dem_share[cd$state == ab], decreasing = TRUE)), collapse = "  ")

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

## ---- rawcd
# Verbatim captures, taken when the file was fetched. The counts quoted in the
# prose are read back out of this text at knit time.
RAW <- c(
"'data.frame':\t435 obs. of  9 variables:",
" $ state_abbrev   : chr  \"AK\" \"AL\" ...",
" $ district_code  : chr  \"00\" \"01\" ...",
" $ house_rep      : chr  \"Nick Begich\" \"Barry Moore\" ...",
" $ house_rep_party: chr  \"republican\" \"republican\" ...",
" $ winner         : chr  \"Donald Trump\" \"Donald Trump\" ...",
" $ party_win      : chr  \"republican\" \"republican\" ...",
" $ democrat       : num  41.4 21.9 ...",
" $ republican     : num  54.5 76.9 ...",
" $ icpsr          : int  22503 22140 22515 20301 29701 ...")
RN <- as.integer(sub(".*:\\D*([0-9]+) obs.*", "\\1", RAW[1]))
RC <- as.integer(sub(".*of *([0-9]+) variables.*", "\\1", RAW[1]))

# R's description of the object, split into the three things it says about each
# column. The paragraph below asks what is NOT here, which is a question about
# a list of columns.
.v <- RAW[-1]
.m <- regmatches(.v, regexec("^ \\$ ([A-Za-z_]+)\\s*: (\\w+)\\s+(.*)$", .v))
.cd <- c(
  state_abbrev = "the state's two-letter code",
  district_code = "the district number, as text so 01 keeps its zero",
  house_rep = "who represents the district",
  house_rep_party = "that member's party",
  winner = "who carried the district for president",
  party_win = "that winner's party",
  democrat = "the Democratic presidential share, as a percentage",
  republican = "the Republican share",
  icpsr = "the member's Voteview ID, which is the join to the roll-call file")
.nm <- vapply(.m, function(z) z[2], character(1))
data.frame(
  Column       = .nm,
  What_it_holds = unname(.cd[.nm]),
  Stored_as    = c(chr = "text", num = "number", int = "whole number")[
                    vapply(.m, function(z) z[3], character(1))],
  First_values = trimws(vapply(.m, function(z) z[4], character(1))))

## ---- rawme
# The capture is one printed frame that R wrapped into two column-groups; here
# it is one table. `house_rep_party` on row 198 is an empty string, not a
# missing value, and it is labelled as such because that is the whole point.
data.frame(
  row             = c("197", "198"),
  state_abbrev    = c("ME", "ME"),
  district_code   = c("01", "02"),
  house_rep       = c("Chellie Pingree", "Continuing Ballots"),
  house_rep_party = c("democrat", '"" (empty string)'),
  winner          = c("Kamala Harris", "Donald Trump"),
  party_win       = c("democrat", "republican"),
  democrat        = c("59.78", "44.22"),
  republican      = c("37.97", "53.75"),
  icpsr           = c("20920", "21362"))

## ---- rawny
# Here the missing cells really are NA, not empty strings -- the opposite of
# the row above, in the same file.
data.frame(
  row             = "293",
  state_abbrev    = "NY",
  district_code   = "21",
  house_rep       = "Elise M. Stefanik",
  house_rep_party = "republican",
  winner          = "Donald Trump",
  party_win       = "NA",
  democrat        = "NA",
  republican      = "NA",
  icpsr           = "21541")

## ---- cleanme
cd[cd$state == "ME", c("district", "democrat", "republican", "dem_share",
                       "pres_party", "house_rep", "house_rep_party")]

## ---- one-row
o <- cd[cd$district == "NC-01",
        c("district", "democrat", "republican", "dem_share", "pres_party",
          "house_rep_party")]
names(o) <- c("district", "Harris %", "Trump %",
              "Democratic % of the two-party vote", "presidential winner",
              "party of the member elected")
o

## ---- devtab
data.frame(
  Plan = dev$plan,
  Plans_listed = dev$plans_listed,
  Middle_plan = ifelse(dev$measured_in == "people",
                       paste(formatC(dev$median_range, format = "d"),
                             ifelse(dev$median_range == 1, "person", "people")),
                       paste0(pc(dev$median_range, 2), "%")),
  Widest_plan = ifelse(dev$measured_in == "people",
                       paste0(formatC(dev$widest_range, format = "d",
                                      big.mark = ","), " people (",
                              dev$widest_state, ")"),
                       paste0(pc(dev$widest_range, 2), "% (",
                              dev$widest_state, ")")))

## ---- national
data.frame(
  quantity = c("Districts in the file", "Mean district Democratic share",
               "Districts a Democrat would carry", "As a share of the House"),
  value = c(nrow(cd), paste0(pc(mean(cd$dem_share)), "%"),
            sum(cd$dem_share > 50),
            paste0(pc(100 * mean(cd$dem_share > 50)), "%")))

## ---- competitive
o <- comp; o$pct <- pc(o$pct)
names(o) <- c("districts...", "how many", "% of the House")
o

## ---- natl-static
ym <- max(hna$counts) * 1.24
par(mar = c(4.4, 4.2, 1.0, 1))
plot(hna, col = NA, border = NA, main = "", ylim = c(0, ym), xlim = c(14, 92),
     xlab = "Democratic share of the two-party presidential vote (%)",
     ylab = "districts")
rect(45, 0, 55, ym, col = "#EDEDED", border = NA)
plot(hna, col = "#999999", border = "white", add = TRUE)
segments(50, 0, 50, ym * 0.92, col = "#C41230", lwd = 2)
text(50, ym * 0.96, "the majority line", cex = 0.74, col = "#C41230")
text(14.5, ym * 0.86, adj = 0, cex = 0.72, col = "#555555",
     labels = paste0("shaded: within 5 points of even: ",
                     comp$districts[comp$band == "within 5 points of even"],
                     " of ", nrow(cd), " districts (",
                     pc(comp$pct[comp$band == "within 5 points of even"]), "%)"))
text(14.5, ym * 0.75, adj = 0, cex = 0.72, col = "#555555",
     labels = paste0("nationally there is no hollow in the middle: the hole is a",
                     " property of individual states"))

## ---- natl-d3
lab <- sapply(seq_along(hna$counts), function(i)
  paste(cd$district[cd$dem_share >  hna$breaks[i] &
                    cd$dem_share <= hna$breaks[i + 1]], collapse = ", "))
rows <- paste(sprintf('{"lo":%d,"hi":%d,"c":%d,"d":"%s"}',
                      hna$breaks[-length(hna$breaks)], hna$breaks[-1],
                      hna$counts, lab), collapse = ",")
cat(paste0('
<div id="nh" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const B=[', rows, '];
const W=760,H=400,M={t:20,r:24,b:48,l:52};
const box=d3.select("#nh");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([14,92]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(B,d=>d.c)*1.12]).range([H-M.b,M.t]);
svg.append("rect").attr("x",x(45)).attr("width",x(55)-x(45))
  .attr("y",M.t).attr("height",H-M.b-M.t).attr("fill","#EDEDED");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>d+"%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party presidential vote in the district");
svg.append("text").attr("x",M.l).attr("y",M.t-6).attr("font-size","11px")
  .attr("fill","#666").text("districts");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;max-width:320px");
svg.append("g").selectAll("rect").data(B).join("rect")
  .attr("x",d=>x(d.lo)+0.6).attr("width",x(16)-x(14)-1.2)
  .attr("y",d=>y(d.c)).attr("height",d=>y(0)-y(d.c)).attr("fill","#999999")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.lo}–${d.hi}% Democratic</b><br>${d.c} district${d.c===1?"":"s"}`+
      (d.d?`<br>${d.d}`:""))
      .style("left",Math.min(ev.offsetX+14,W-340)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t+14).attr("y2",H-M.b)
  .attr("stroke","#C41230").attr("stroke-width",2);
svg.append("text").attr("x",x(50)).attr("y",M.t+8).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230").text("the majority line");
svg.append("text").attr("x",x(14.5)).attr("y",M.t+34).attr("font-size","11px")
  .attr("fill","#555").text("shaded: within 5 points of even — ',
      comp$districts[comp$band == "within 5 points of even"], ' of ', nrow(cd),
      ' districts (', pc(comp$pct[comp$band == "within 5 points of even"]), '%)");
svg.append("text").attr("x",x(14.5)).attr("y",M.t+50).attr("font-size","11px")
  .attr("fill","#555")
  .text("nationally there is no hollow in the middle: the hole is a property of individual states");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
All ', nrow(cd), ' districts in bins of two points. Hover a bar for the districts in it.</p>
'))

## ---- rows
data.frame(
  state = c("NC", "WI", "PA"),
  districts_sorted = c(row_of("NC"), row_of("WI"), row_of("PA")))

## ---- ridge-static
gapy <- 1.0
par(mar = c(4.4, 0.6, 0.6, 1))
plot(NA, xlim = c(18, 94), ylim = c(-0.25, length(ridge) * gapy + 0.85),
     yaxt = "n", bty = "n",
     xlab = "Democratic share of the two-party presidential vote (%)", ylab = "")
abline(v = 50, lty = 2, col = "#666666")
for (i in seq_along(ridge)) {
  a  <- ridge[length(ridge) - i + 1]
  d  <- dens[[a]]
  b  <- (i - 1) * gapy
  yy <- b + d$y / dmax * 0.92
  polygon(c(d$x, rev(d$x)), c(yy, rep(b, length(yy))),
          col = "#2c7fb844", border = "#2c7fb8")
  v <- cd$dem_share[cd$state == a]
  segments(v, b, v, b + 0.16, col = ifelse(v > 50, "#2c7fb8", "#C41230"), lwd = 2)
  h <- big$hole[big$state == a]
  if (!is.na(h))
    segments(big$below[big$state == a], b + 0.62,
             big$above[big$state == a], b + 0.62, col = "#C41230", lwd = 2.4)
  text(19, b + 0.42, a, adj = 0, font = 2, cex = 0.9)
  text(19, b + 0.22, adj = 0, cex = 0.6, col = "#C41230",
       labels = if (is.na(h)) "no district below 50: no hole to measure"
                else paste0("empty band ", pc(h), " points wide"))
}
text(18, length(ridge) * gapy + 0.72, adj = 0, cex = 0.7, col = "#555555",
     labels = "ticks are the districts themselves; the red bar spans the empty band")

## ---- ridge-d3
curves <- paste(sapply(ridge, function(a) sprintf(
  '{"st":"%s","x":[%s],"y":[%s],"pts":[%s],"nm":[%s],"hole":%s,"b":%s,"a":%s}',
  a, paste(sprintf("%.1f", dens[[a]]$x), collapse = ","),
  paste(sprintf("%.5f", dens[[a]]$y / dmax), collapse = ","),
  paste(sprintf("%.2f", sort(cd$dem_share[cd$state == a])), collapse = ","),
  paste(sprintf('"%s"', cd$district[cd$state == a][
    order(cd$dem_share[cd$state == a])]), collapse = ","),
  ifelse(is.na(big$hole[big$state == a]), "null",
         sprintf("%.2f", big$hole[big$state == a])),
  ifelse(is.na(big$below[big$state == a]), "null",
         sprintf("%.2f", big$below[big$state == a])),
  ifelse(is.na(big$above[big$state == a]), "null",
         sprintf("%.2f", big$above[big$state == a])))), collapse = ",")
cat(paste0('
<div id="rd" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', curves, '];
const W=760,H=460,M={t:26,r:20,b:46,l:20},band=(H-M.t-M.b)/D.length;
const box=d3.select("#rd");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([18,94]).range([M.l+40,W-M.r]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>d+"%"));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party presidential vote in the district");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t-6).attr("y2",H-M.b)
  .attr("stroke","#666").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",M.l).attr("y",M.t-10).attr("font-size","11px")
  .attr("fill","#555")
  .text("ticks are the districts themselves; the red bar spans the empty band");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach((d,i)=>{
  const base=M.t+(i+1)*band-6;
  const area=d3.area().x((v,k)=>x(d.x[k])).y0(base).y1((v,k)=>base-d.y[k]*(band-14));
  svg.append("path").datum(d.x).attr("d",area)
    .attr("fill","#2c7fb8").attr("fill-opacity",0.27).attr("stroke","#2c7fb8");
  svg.append("g").selectAll("line").data(d.pts).join("line")
    .attr("x1",v=>x(v)).attr("x2",v=>x(v)).attr("y1",base).attr("y2",base-11)
    .attr("stroke",v=>v>50?"#2c7fb8":"#C41230").attr("stroke-width",2)
    .on("mousemove",function(ev,v){
      const k=d.pts.indexOf(v);
      tip.style("opacity",1).html(`<b>${d.nm[k]}</b>: ${v.toFixed(1)}% Democratic`)
        .style("left",Math.min(ev.offsetX+14,W-220)+"px").style("top",(ev.offsetY-10)+"px"); })
    .on("mouseleave",()=>tip.style("opacity",0));
  if(d.hole!==null)
    svg.append("line").attr("x1",x(d.b)).attr("x2",x(d.a))
      .attr("y1",base-34).attr("y2",base-34).attr("stroke","#C41230").attr("stroke-width",2.4);
  svg.append("text").attr("x",M.l).attr("y",base-24).attr("font-size","14px")
    .attr("font-weight","700").text(d.st);
  svg.append("text").attr("x",M.l).attr("y",base-9).attr("font-size","10px")
    .attr("fill","#C41230")
    .text(d.hole===null?"no hole":d.hole.toFixed(1)+" pts");
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The same states as densities on one axis, drawn with a common bandwidth. Hover a
tick for the district.</p>
'))

## ---- eg-worst
o <- head(big[order(big$eg), c("state", "n", "vote", "seats", "seat_pct", "eg")], 5)
names(o) <- c("state", "districts", "Democratic vote %", "Democratic seats",
              "seat %", "efficiency gap")
o

## ---- eg-best
o <- head(big[order(-big$eg), c("state", "n", "vote", "seats", "seat_pct", "eg")], 5)
names(o) <- c("state", "districts", "Democratic vote %", "Democratic seats",
              "seat %", "efficiency gap")
o

## ---- extremes
data.frame(
  quantity = c("Most Democratic district in the country",
               "Most Republican district in the country",
               "Districts at 70% Democratic or more",
               "Districts at 70% Republican or more"),
  value = c(paste0(cd$district[which.max(cd$dem_share)], " at ",
                   pc(max(cd$dem_share)), "%"),
            paste0(cd$district[which.min(cd$dem_share)], " at ",
                   pc(100 - min(cd$dem_share)), "%"),
            sum(cd$dem_share >= 70), sum(cd$dem_share <= 30)))

## ---- egmap-prep
sr <- read.csv("data/derived/seat_rings.csv",  stringsAsFactors = FALSE)
sm <- read.csv("data/derived/seat_states.csv", stringsAsFactors = FALSE)
# `seats` in the map file is the House delegation; `seats` in st is Democratic
# seats won -- rename before the merge so neither is silently suffixed
names(sm)[names(sm) == "seats"] <- "house_seats"
sm <- merge(sm, st, by = "state", all.x = TRUE)
# the chapter scores the efficiency gap on states with six districts or more;
# the map draws the rest in grey rather than pretending one at-large seat has
# a gap to measure
RAMP_R <- colorRampPalette(c("#F4F3F1", "#C41230"))(101)
RAMP_D <- colorRampPalette(c("#F4F3F1", "#2C7FB8"))(101)
tt <- pmin(abs(sm$eg) / 30, 1)
sm$fill <- ifelse(sm$n < 6, "#DBDBDB",
           ifelse(sm$eg < 0, RAMP_R[round(tt * 100) + 1],
                             RAMP_D[round(tt * 100) + 1]))
# ink by the fill's own luminance, not by ramp position: the blue ramp is
# lighter than the red at the same depth, so a depth threshold flips to white
# too early on blue.  White needs 3:1 against the fill; sRGB luminance under
# ~0.30 gives that with room to spare.
lum <- apply(col2rgb(sm$fill) / 255, 2, function(v) {
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
})
sm$ink <- ifelse(lum < 0.30, "#FFFFFF", "#333333")
sxr <- range(sr$x); syr <- range(sr$y)

## ---- egmap-static
BAND <- 92                                   # legend band below the map
par(mar = rep(0.2, 4))
plot(NA, xlim = sxr, ylim = c(syr[2] + BAND, syr[1]), asp = 1,
     axes = FALSE, ann = FALSE)
o <- order(-sm$house_seats)                 # big states first, so no edge hides
for (a in sm$state[o]) {
  pr <- split(sr[sr$state == a, ], sr$part[sr$state == a])
  xs <- head(unlist(lapply(pr, function(z) c(z$x, NA)), use.names = FALSE), -1)
  ys <- head(unlist(lapply(pr, function(z) c(z$y, NA)), use.names = FALSE), -1)
  polypath(xs, ys, col = sm$fill[sm$state == a], border = "#FFFFFF", lwd = 0.6,
           rule = "evenodd")
}
text(sm$label_x, sm$label_y, sm$state, cex = 0.52, font = 2, col = sm$ink)
legend(sxr[1], syr[2] + 14, xjust = 0, yjust = 0, horiz = TRUE,
       c("map favors Republicans", "map favors Democrats", "under six districts: not scored"),
       fill = c("#C41230", "#2C7FB8", "#DBDBDB"), border = NA, bty = "n",
       cex = 0.6, x.intersp = 0.6)
text(sxr[1] + 8, syr[2] + 62, adj = c(0, 0), cex = 0.58, col = "#555555",
     labels = "area = House seats, 2020 apportionment; depth of color = size of the efficiency gap")

## ---- egmap-d3
# Rounded to the pixel and deduplicated for the embed, the way the mapping
# chapter does it: neighbours round a shared edge to the same pixels, so the
# borders stay shared.
pth <- vapply(split(sr, sr$state), function(z) {
  paste(vapply(split(z, z$part), function(r) {
    x <- round(r$x); y <- round(r$y)
    keep <- c(TRUE, diff(x) != 0 | diff(y) != 0)
    paste0("M", paste0(x[keep], ",", y[keep], collapse = "L"), "Z")
  }, character(1)), collapse = "")
}, character(1))
sj <- sm[match(names(pth), sm$state), ]
SD <- paste(sprintf(
  '{"st":"%s","nm":"%s","seats":%d,"vote":%s,"eg":%s,"f":"%s","k":"%s","lx":%.1f,"ly":%.1f,"p":"%s"}',
  sj$state, sj$name, sj$house_seats,
  sprintf("%.1f", sj$vote),
  ifelse(sj$n < 6 | is.na(sj$eg), "null", sprintf("%.1f", sj$eg)),
  sj$fill, sj$ink, sj$label_x, sj$label_y, pth), collapse = ",")
cat(paste0('
<div id="egm" style="position:relative;margin:1em 0"></div>
<style>
/* Dark labels lie on pale-to-mid fills; a paper-colored halo keeps them
   legible on every fill in both themes without recoloring any fill.  The
   white labels on the saturated fills get NO halo: a paper stroke touches
   the glyph, so the checker (correctly) scores white-on-paper — and white
   on a saturated fill passes on its own.  Verify with
   _lib/check-contrast.js on BOTH themes after touching this figure. */
#egm text[fill="#333333" i] { paint-order:stroke; stroke:var(--paper);
            stroke-width:3px; stroke-linejoin:round; }
</style>
<script>
(function(){
const S=[', SD, '];
const X0=', floor(sxr[1]), ',Y0=', floor(syr[1]),
',W=', ceiling(diff(sxr)) + 2, ',H=', ceiling(diff(syr)) + 2, ';
const box=d3.select("#egm");
const svg=box.append("svg").attr("viewBox",`${X0} ${Y0} ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="Area = House seats, 2020 apportionment. Red: the map favors "+
  "Republicans; blue: Democrats; deeper = larger efficiency gap; grey: under "+
  "six districts. <i>Hover a state for its numbers.</i>";
cap.html(DEF);
const g=svg.append("g");
g.selectAll("path").data(S).join("path").attr("d",d=>d.p)
  .attr("fill",d=>d.f).attr("stroke","#fff").attr("stroke-width",0.8)
  .on("mouseenter",function(e,d){
    d3.select(this).attr("stroke","#111").attr("stroke-width",1.6).raise();
    cap.html("<b>"+d.nm+"</b>: "+d.seats+" seat"+(d.seats>1?"s":"")+
      (d.eg===null
        ? ". Under six districts, so the chapter does not score its gap."
        : ". "+d.vote.toFixed(1)+"% Democratic two-party vote; efficiency gap "+
          (d.eg>0?"+":"")+d.eg.toFixed(1)+" ("+
          (d.eg<0?"favors Republicans":"favors Democrats")+")."));})
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#fff").attr("stroke-width",0.8);
    cap.html(DEF);});
svg.append("g").attr("pointer-events","none").selectAll("text")
  .data(S).join("text")
  .attr("x",d=>d.lx).attr("y",d=>d.ly).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("font-weight","600")
  .attr("fill",d=>d.k).text(d=>d.st);
})();
</script>'))

## ---- hole
o <- big[order(-big$hole, na.last = TRUE),
         c("state", "n", "vote", "below", "above", "hole", "eg")]
o <- rbind(head(o, 6), o[o$state %in% c("OH", "PA", "MA"), ])
o$vote <- pc(o$vote); o$below <- pc(o$below); o$above <- pc(o$above)
o$hole <- pc(o$hole)
names(o) <- c("state", "districts", "Democratic vote %",
              "nearest below 50", "nearest above 50", "the hole",
              "efficiency gap")
o

## ---- hole-static
s <- big[order(-big$eg), ]
par(mar = c(4.2, 4.4, 1, 1))
plot(NA, xlim = c(15, 92), ylim = c(0.5, nrow(s) + 0.5), yaxt = "n",
     xlab = "Democratic share of the two-party presidential vote (%)", ylab = "")
axis(2, at = seq_len(nrow(s)), labels = s$state, las = 1, cex.axis = 0.55)
abline(v = 50, lty = 2)
for (i in seq_len(nrow(s))) {
  v <- cd$dem_share[cd$state == s$state[i]]
  if (!is.na(s$hole[i]) && s$hole[i] > 12)
    rect(s$below[i], i - 0.35, s$above[i], i + 0.35, col = "#F2D7DA", border = NA)
  points(v, rep(i, length(v)), pch = 19, cex = 0.55,
         col = ifelse(v > 50, "#2166AC", "#B2182B"))
}

## ---- hole-d3
s <- big[order(-big$eg), ]
pts <- paste(sprintf('{"st":"%s","d":"%s","v":%.2f}', cd$state, cd$district,
                     cd$dem_share)[cd$state %in% s$state], collapse = ",")
sts <- paste(sprintf('{"st":"%s","eg":%.1f,"hole":%s,"b":%s,"a":%s}',
                     s$state, s$eg,
                     ifelse(is.na(s$hole), "null", sprintf("%.1f", s$hole)),
                     ifelse(is.na(s$below), "null", sprintf("%.1f", s$below)),
                     ifelse(is.na(s$above), "null", sprintf("%.1f", s$above))),
              collapse = ",")
cat(sprintf('
<div id="hg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const P=[%s], S=[%s];
const W=760,H=640,M={t:24,r:24,b:46,l:44};
const box=d3.select("#hg");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([12,92]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(S.map(d=>d.st)).range([M.t,H-M.b]).padding(0.28);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(9).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("g").selectAll("rect").data(S.filter(d=>d.hole!==null&&d.hole>12))
  .join("rect").attr("x",d=>x(d.b)).attr("width",d=>x(d.a)-x(d.b))
  .attr("y",d=>y(d.st)).attr("height",y.bandwidth())
  .attr("fill","#C41230").attr("opacity",0.16);
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#666").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party presidential vote in the district");
svg.append("text").attr("x",W-M.r).attr("y",M.t-8).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("shaded: an empty band of more than 12 points around the majority line");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(P).join("circle")
  .attr("cx",d=>x(d.v)).attr("cy",d=>y(d.st)+y.bandwidth()/2).attr("r",3.6)
  .attr("fill",d=>d.v>50?"#2166AC":"#B2182B").attr("fill-opacity",0.85)
  .on("mousemove",function(ev,d){
    const s=S.find(z=>z.st===d.st);
    tip.style("opacity",1).html(
      `<b>${d.d}</b>: ${d.v.toFixed(1)}%% Democratic<br>`+
      `${d.st} efficiency gap ${s.eg>0?"+":""}${s.eg}<br>`+
      `hole: ${s.hole===null?"none (no district below 50%%)":s.hole.toFixed(1)+" points"}`)
      .style("left",Math.min(ev.offsetX+14,W-300)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
States with six or more districts, ordered by efficiency gap. Hover any district
for its state&#39;s statistics.</p>
', pts, sts))

## ---- two-stats-static
gut <- max(sc$hole) + 6
par(mar = c(4.4, 4.4, 1.0, 1))
plot(NA, xlim = c(-1, gut + 3), ylim = range(big$eg) + c(-4, 4), las = 1,
     xlab = "the hole: empty band around the majority line (points)",
     ylab = "efficiency gap")
abline(h = 0, col = "#BBBBBB")
abline(v = median(sc$hole), lty = 3, col = "#999999")
segments(gut - 2.4, min(big$eg) - 4, gut - 2.4, max(big$eg) + 4, col = "#CCCCCC")
points(sc$hole, sc$eg, pch = 19, cex = 1.15, col = "#2c7fb8")
points(rep(gut + 0.6, nrow(nh)), nh$eg, pch = 17, cex = 1.15, col = "#C41230")
key <- c("NC", "WI", "TN", "LA", "PA", "OH", "IL", "OR", "AL", "MD")
i <- sc$state %in% key
text(sc$hole[i], sc$eg[i], sc$state[i], pos = 4, offset = 0.35, cex = 0.68,
     col = "#333333")
text(rep(gut + 0.6, nrow(nh)), nh$eg, nh$state, pos = 2, offset = 0.4,
     cex = 0.68, col = "#C41230")
text(gut + 0.6, min(big$eg) - 2.6, "no district\nbelow 50%", cex = 0.6,
     col = "#C41230")
text(median(sc$hole) + 0.5, max(big$eg) + 3, adj = 0, cex = 0.66, col = "#777777",
     labels = paste0("median hole ", pc(median(sc$hole)), " points"))

## ---- two-stats-d3
rows <- paste(sprintf('{"s":"%s","h":%s,"e":%.1f,"n":%d,"v":%.1f,"seats":%d}',
                      big$state,
                      ifelse(is.na(big$hole), "null", sprintf("%.2f", big$hole)),
                      big$eg, big$n, big$vote, big$seats), collapse = ",")
cat(paste0('
<div id="ts" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '], MED=', median(sc$hole), ', GUT=', max(sc$hole) + 6, ';
const W=760,H=440,M={t:16,r:24,b:48,l:58};
const box=d3.select("#ts");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-1,GUT+3]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([', min(big$eg) - 4, ',', max(big$eg) + 4, '])
  .range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(7).tickFormat(d=>(d>0?"+":"")+d));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#BBBBBB");
svg.append("line").attr("x1",x(MED)).attr("x2",x(MED)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","3,3");
svg.append("line").attr("x1",x(GUT-2.4)).attr("x2",x(GUT-2.4))
  .attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#CCCCCC");
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("the hole: empty band around the majority line (points)");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("efficiency gap");
svg.append("text").attr("x",x(MED)+5).attr("y",M.t+12).attr("font-size","11px")
  .attr("fill","#777").text("median hole ', pc(median(sc$hole)), ' points");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const px=d=>d.h===null?x(GUT+0.6):x(d.h);
const tri=d3.symbol().type(d3.symbolTriangle).size(95)();
const g=svg.append("g");
g.selectAll("circle").data(D.filter(d=>d.h!==null)).join("circle")
  .attr("cx",px).attr("cy",d=>y(d.e)).attr("r",5.5).attr("fill","#2c7fb8")
  .attr("fill-opacity",0.85).on("mousemove",show).on("mouseleave",hide);
g.selectAll("path").data(D.filter(d=>d.h===null)).join("path").attr("d",tri)
  .attr("transform",d=>`translate(${px(d)},${y(d.e)})`).attr("fill","#C41230")
  .on("mousemove",show).on("mouseleave",hide);
g.selectAll("text").data(D).join("text")
  .attr("x",d=>px(d)+(d.h===null?-10:9)).attr("y",d=>y(d.e)+4)
  .attr("text-anchor",d=>d.h===null?"end":"start")
  .attr("font-size","10.5px").attr("pointer-events","none")
  .attr("fill",d=>d.h===null?"#C41230":"#333").text(d=>d.s);
svg.append("text").attr("x",x(GUT+0.6)).attr("y",H-M.b-6).attr("text-anchor","middle")
  .attr("font-size","10px").attr("fill","#C41230").text("no district below 50%");
function show(ev,d){
  tip.style("opacity",1).html(
    `<b>${d.s}</b> — ${d.n} districts<br>Democratic vote ${d.v}%, `+
    `${d.seats} seats<br>efficiency gap ${d.e>0?"+":""}${d.e}<br>`+
    `hole ${d.h===null?"undefined":d.h.toFixed(1)+" points"}`)
    .style("left",Math.min(ev.offsetX+14,W-280)+"px").style("top",(ev.offsetY-10)+"px"); }
function hide(){ tip.style("opacity",0); }
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Every state with six or more districts. States with no district below 50% have no
hole to measure and sit in the gutter at the right. Hover for the detail.</p>
'))

## ---- cross
data.frame(
  quantity = c("Districts electing a member of the party that lost the district's presidential vote",
               "As a share of the House",
               "Range of those districts' Democratic presidential share"),
  value = c(nrow(cross), paste0(pc(100 * nrow(cross) / nrow(cd)), "%"),
            paste0(pc(min(cross$dem_share)), "% to ", pc(max(cross$dem_share)), "%")))

## ---- svc-static
par(mar = c(4.4, 4.4, 1.0, 1))
plot(NA, xlim = c(35, 65), ylim = c(0, 100), las = 1,
     xlab = "national Democratic share of the two-party presidential vote (%)",
     ylab = "Democratic share of the seats (%)")
abline(h = 50, col = "#DDDDDD"); abline(v = 50, col = "#DDDDDD")
segments(35, 35, 65, 65, col = "#999999", lty = 2)
lines(svc$vote, svc$seats, type = "s", col = "#C41230", lwd = 2.6)
points(natl, 100 * mean(cd$dem_share > 50), pch = 19, cex = 1.3, col = "#2c7fb8")
text(35.5, 92, adj = 0, cex = 0.72, col = "#999999",
     labels = "dashed: seats in proportion to votes")
text(35.5, 84, adj = 0, cex = 0.72, col = "#2c7fb8",
     labels = paste0("2024 as it happened: ", pc(natl), "% of the vote, ",
                     pc(100 * mean(cd$dem_share > 50)), "% of the seats"))
text(35.5, 76, adj = 0, cex = 0.72, col = "#C41230",
     labels = paste0("at an even national vote the map returns ", pc(seats50),
                     "% of the seats"))
text(35.5, 68, adj = 0, cex = 0.72, col = "#555555",
     labels = paste0("Democrats need ", pc(vote50),
                     "% of the vote before the map returns half the House"))

## ---- svc-d3
rows <- paste(sprintf('{"v":%.2f,"s":%.2f,"n":%d}', svc$vote, svc$seats,
                      round(svc$seats / 100 * nrow(cd))), collapse = ",")
cat(paste0('
<div id="sv" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const NATL=', natl, ', OBS=', 100 * mean(cd$dem_share > 50), ';
const S50=', seats50, ', V50=', vote50, ';
const W=760,H=440,M={t:16,r:24,b:48,l:56};
const box=d3.select("#sv");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([35,65]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#DDDDDD");
svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("x1",x(50)).attr("x2",x(50))
  .attr("stroke","#DDDDDD");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d=>d+"%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%"));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-10).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("national Democratic share of the two-party presidential vote");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("Democratic share of the seats");
svg.append("line").attr("x1",x(35)).attr("y1",y(35)).attr("x2",x(65)).attr("y2",y(65))
  .attr("stroke","#999").attr("stroke-dasharray","5,4");
svg.append("path").datum(D).attr("fill","none").attr("stroke","#C41230")
  .attr("stroke-width",2.6)
  .attr("d",d3.line().curve(d3.curveStepAfter).x(d=>x(d.v)).y(d=>y(d.s)));
svg.append("circle").attr("cx",x(NATL)).attr("cy",y(OBS)).attr("r",6)
  .attr("fill","#2c7fb8");
const T=[["#999","dashed: seats in proportion to votes"],
  ["#2c7fb8","2024 as it happened: ', pc(natl), '% of the vote, ',
      pc(100 * mean(cd$dem_share > 50)), '% of the seats"],
  ["#C41230","at an even national vote the map returns ', pc(seats50),
      '% of the seats"],
  ["#555","Democrats need ', pc(vote50),
      '% of the vote before the map returns half the House"]];
T.forEach((t,i)=>svg.append("text").attr("x",x(35.4)).attr("y",M.t+16+i*16)
  .attr("font-size","11px").attr("fill",t[0]).text(t[1]));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const v=x.invert(d3.pointer(ev,this)[0]);
    const d=D.reduce((a,c)=>Math.abs(c.v-v)<Math.abs(a.v-v)?c:a);
    tip.style("opacity",1).html(
      `<b>${d.v.toFixed(1)}% of the vote</b><br>${d.n} seats — ${d.s.toFixed(1)}%`)
      .style("left",Math.min(ev.offsetX+14,W-240)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
A uniform swing applied to every district at once. It assumes equal turnout
everywhere, and no district changes hands for any reason but the swing — both
false, which is why this is a property of the map and not a forecast.</p>
'))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#nh text[fill="#c41230" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

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
#ts text[fill="#333" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
