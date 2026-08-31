# data-sources-code.R -- chunk bodies for data-sources-brief.Rmd
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

naive <- read.csv("data/derived/pres2024_counties.csv", stringsAsFactors = FALSE)
cty <- read.csv("data/derived/pres2024_counties.csv", stringsAsFactors = FALSE,
                colClasses = c(county_fips = "character"))
old <- read.csv("data/derived/pres2020_counties.csv", stringsAsFactors = FALSE,
                colClasses = c(county_fips = "character"))
cen <- read.csv("data/derived/census_counties.csv", stringsAsFactors = FALSE,
                colClasses = c(fips = "character"))
stt <- read.csv("data/derived/pres2024_states.csv", stringsAsFactors = FALSE)

# The official returns, fetched from all 51 state chief election offices and
# assembled in ../county-returns/. This chapter is ABOUT the compilation, so the
# compilation stays as the specimen; these are here to say what the states
# actually published.
OFF <- file.path("..", "county-returns", "data")
of24 <- read.csv(file.path(OFF, "derived/pres2024_counties_official.csv"),
                 stringsAsFactors = FALSE, colClasses = c(county_fips = "character"))
of20 <- read.csv(file.path(OFF, "derived/pres2020_counties_official.csv"),
                 stringsAsFactors = FALSE, colClasses = c(county_fips = "character"))
prov <- read.csv(file.path(OFF, "provenance.csv"), stringsAsFactors = FALSE,
                 colClasses = "character")

# how far each file is from AP's certified state totals
apx  <- read.csv(file.path(OFF, "derived/crosscheck_ap_2024.csv"), stringsAsFactors = FALSE)
AP_D    <- sum(apx$votes_dem)
OFF_D   <- sum(of24$votes_dem)
COMP_D  <- sum(cty$votes_dem)
OFF_ERR <- OFF_D  - AP_D
CMP_ERR <- COMP_D - AP_D

# 11001 in each file
N11001_OFF <- sum(of24$county_fips == "11001") + sum(of20$county_fips == "11001")
N11001_CMP <- sum(cty$county_fips == "11001") + sum(old$county_fips == "11001")

# Connecticut, each file, each year
n_ct <- function(d) sum(grepl("Connecticut", d$state_name))
CT_OFF20 <- n_ct(of20); CT_OFF24 <- n_ct(of24)
CT_CMP20 <- n_ct(old);  CT_CMP24 <- n_ct(cty)

# where the two disagree on a major-party count
mrg <- merge(of24[nchar(of24$county_fips)==5, ], cty[nchar(cty$county_fips)==5, ],
             by = "county_fips", suffixes = c(".o", ".c"))
DISAGREE24 <- sum(mrg$votes_dem.o != mrg$votes_dem.c | mrg$votes_gop.o != mrg$votes_gop.c)
mrg20 <- merge(of20[nchar(of20$county_fips)==5, ], old[nchar(old$county_fips)==5, ],
               by = "county_fips", suffixes = c(".o", ".c"))
DISAGREE20 <- sum(mrg20$votes_dem.o != mrg20$votes_dem.c | mrg20$votes_gop.o != mrg20$votes_gop.c)
NOFIPS <- sum(nchar(of24$county_fips) != 5)

cty$other <- cty$total_votes - cty$votes_gop - cty$votes_dem
agg <- aggregate(cbind(votes_dem, votes_gop, total_votes) ~ state_name,
                 data = cty, FUN = sum)
cmp <- merge(agg, stt, by.x = "state_name", by.y = "state")
cmp$hc <- round(100 * cmp$votes_dem / cmp$total_votes, 2)
cmp$tc <- round(100 * cmp$votes_gop / cmp$total_votes, 2)
cmp$dh <- round(cmp$hc - cmp$harris, 2)
cmp$dt <- round(cmp$tc - cmp$trump,  2)
cmp$implied     <- round(cmp$votes_dem / (cmp$harris / 100))
cmp$missing     <- cmp$implied - cmp$total_votes
cmp$missing_pct <- round(100 * cmp$missing / cmp$implied, 2)
cmp$h2c <- 100 * cmp$votes_dem / (cmp$votes_dem + cmp$votes_gop)
cmp$h2s <- 100 * cmp$harris    / (cmp$harris    + cmp$trump)
cmp$d2  <- round(cmp$h2c - cmp$h2s, 2)
# one N for the "missing ballots" chart: the PDF drew 12 states and the HTML 14,
# so the two versions disagreed about how long the tail was
MISSN <- 14
# The "missing ballots" figure paints a bar red above this share. The value was
# typed separately into the base-R chunk and the D3 chunk, so the two renderers
# could have drifted; it now lives here and is passed to both.
MISSRED <- 1.5

cty$key <- paste(cty$state_name, cty$county_name)
cen$key <- paste(cen$state_name, cen$name)
by_name <- merge(cty, cen, by.x = "county_name", by.y = "name")
by_key  <- merge(cty, cen, by = "key")
by_fips <- merge(cty, cen, by.x = "county_fips", by.y = "fips")
wrong   <- by_fips[by_fips$county_name != by_fips$name, ]

sw <- merge(old[, c("county_fips", "state_name", "county_name",
                    "votes_dem", "votes_gop")],
            cty[, c("county_fips", "votes_dem", "votes_gop")],
            by = "county_fips", suffixes = c("_20", "_24"))
sw$r20 <- 100 * sw$votes_gop_20 / (sw$votes_gop_20 + sw$votes_dem_20)
sw$r24 <- 100 * sw$votes_gop_24 / (sw$votes_gop_24 + sw$votes_dem_24)
sw$swing <- round(sw$r24 - sw$r20, 2)
dcrow <- sw[sw$county_fips == "11001", ]
dc20  <- sum(old$total_votes[old$state_name == "District of Columbia"])
dc24  <- cty$total_votes[cty$county_fips == "11001"]
# The opening pages quote this row against the rest of the table. Both numbers
# are computed here, once, so the prose at the top and the table further down
# cannot drift apart: the typical county moved this far, and 11001 moved that
# far, in the opposite direction.
MEDSW <- median(sw$swing)
DCSHR <- 100 * dc24 / dc20         # Ward 1 as a share of the city it replaced

nw <- sum(cty$county_name == "Washington County")
anch <- by_fips$total_votes[by_fips$county_fips == "02020"]

pc <- function(x, k = 2) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# ---- third-party share the county file actually records, by state ----
tp <- aggregate(cbind(other, total_votes) ~ state_name, data = cty, FUN = sum)
tp$share <- 100 * tp$other / tp$total_votes
tile <- merge(stt[, c("state", "abbrev", "col", "row")], tp,
              by.x = "state", by.y = "state_name")
tile$lab <- pc(tile$share)
ramp <- function(v, hi) {
  f <- colorRamp(c("#FFFFFF", "#8856a7"))(pmin(v / hi, 1))
  rgb(f[, 1], f[, 2], f[, 3], maxColorValue = 255)
}
tile$fill <- ramp(tile$share, max(tile$share))
tile$ink  <- ifelse(tile$share > 0.55 * max(tile$share), "#FFFFFF", "#333333")
zero_st   <- tile$abbrev[tile$share == 0]

# ---- the Washington County join, as a bipartite graph ----
ab   <- setNames(stt$abbrev, stt$state)
wret <- sort(ab[cty$state_name[cty$county_name == "Washington County"]])
wcen <- sort(ab[cen$state_name[cen$name == "Washington County"]])
wpair <- expand.grid(l = seq_along(wret), r = seq_along(wcen))
wpair$ok <- wret[wpair$l] == wcen[wpair$r]

# ---- what became of each reporting unit between 2020 and 2024 ----
matched  <- intersect(old$county_fips, cty$county_fips)
ak20 <- sum(!old$county_fips %in% cty$county_fips & old$state_name == "Alaska")
ct20 <- sum(!old$county_fips %in% cty$county_fips & old$state_name == "Connecticut")
ak24 <- sum(!cty$county_fips %in% old$county_fips & cty$state_name == "Alaska")
ct24 <- sum(!cty$county_fips %in% old$county_fips & cty$state_name == "Connecticut")
dc_new <- sum(!cty$county_fips %in% old$county_fips &
            cty$state_name == "District of Columbia")
Lname <- c("units the two files share", "the District of Columbia",
           "Alaska, 2020 numbering", "Connecticut counties",
           "nothing in the 2020 file")
Lk    <- c(length(matched) - 1, 1, ak20, ct20, ak24 + ct24 + dc_new)
Rname <- c("units the two files share", "DC Ward 1, code 11001",
           "nothing in the 2024 file", "Alaska, 2024 numbering",
           "Connecticut planning regions", "DC Wards 2 to 8")
Rk    <- c(length(matched) - 1, 1, ak20 + ct20, ak24, ct24, dc_new)
TOT   <- sum(Lk)
GAP   <- 0.058          # the small nodes need room or their labels collide
# A ribbon's thickness must be its COUNT, and must be the same at both ends.
# Deriving thickness from the node height instead (with a floor on the node)
# silently rescaled each end by a different factor: the 8-unit Connecticut
# ribbon left one side 5.6x thicker than it arrived at the other, and was drawn
# wider than the 9-unit ribbon beside it. Width now comes from k alone.
RMIN  <- 0.006                     # visibility floor, disclosed in the caption
rib <- data.frame(
  s = c(1, 2, 3, 4, 5, 5, 5), t = c(1, 2, 3, 3, 4, 5, 6),
  k = c(length(matched) - 1, 1, ak20, ct20, ak24, ct24, dc_new),
  kind = c("kept", "collision", "dropped", "dropped",
           "invented", "invented", "invented"), stringsAsFactors = FALSE)
rib$w      <- pmax(rib$k / TOT, RMIN)      # identical at source and target
rib$floored <- rib$k / TOT < RMIN          # drawn at the floor, not to scale
# a node is exactly as tall as the ribbons it carries, so nothing drifts
nodeh <- function(side, m) sapply(seq_len(m), function(j) sum(rib$w[side == j]))
Lh  <- nodeh(rib$s, length(Lname)); Rh <- nodeh(rib$t, length(Rname))
Ly0 <- cumsum(c(0, head(Lh + GAP, -1))); Ry0 <- cumsum(c(0, head(Rh + GAP, -1)))
LL  <- data.frame(k = Lk, h = Lh, y0 = Ly0)
RR  <- data.frame(k = Rk, h = Rh, y0 = Ry0)
ALH <- max(max(Ly0 + Lh), max(Ry0 + Rh))
so <- to <- numeric(nrow(rib))
for (i in seq_len(nrow(rib))) {
  j <- seq_len(i - 1)
  so[i] <- Ly0[rib$s[i]] + sum(rib$w[j][rib$s[j] == rib$s[i]])
  to[i] <- Ry0[rib$t[i]] + sum(rib$w[j][rib$t[j] == rib$t[i]])
}
rib$sy0 <- so; rib$sy1 <- so + rib$w
rib$ty0 <- to; rib$ty1 <- to + rib$w
# color encodes what BECAME of a reporting unit; the legend below names each
rib$col <- c(kept = "#BBBBBB", collision = "#C41230",
             dropped = "#e08214", invented = "#2c7fb8")[rib$kind]
RIBLEG <- data.frame(
  kind = c("kept", "collision", "dropped", "invented"),
  col  = c("#BBBBBB", "#C41230", "#e08214", "#2c7fb8"),
  lab  = c("same code, both files", "same code, different place",
           "in 2020, gone in 2024", "new in 2024, absent from 2020"),
  stringsAsFactors = FALSE)
RIBLEG$k <- sapply(RIBLEG$kind, function(z) sum(rib$k[rib$kind == z]))

# ---- five checks, run on every state ----
checks <- c("FIPS mangled on read", "no third-party vote recorded",
            "FIPS absent from the census list", "FIPS matches, name differs",
            "no counterpart in the 2020 file")
sts <- sort(unique(cty$state_name))
fail <- data.frame(state = sts, stringsAsFactors = FALSE)
fail[[checks[1]]] <- sapply(sts, function(s)
  mean(nchar(naive$county_fips[naive$state_name == s]) == 4))
fail[[checks[2]]] <- sapply(sts, function(s) mean(cty$other[cty$state_name == s] == 0))
fail[[checks[3]]] <- sapply(sts, function(s)
  mean(!cty$county_fips[cty$state_name == s] %in% cen$fips))
fail[[checks[4]]] <- sapply(sts, function(s) {
  i <- cty$state_name == s; j <- match(cty$county_fips[i], cen$fips)
  mean(!is.na(j) & cen$name[j] != cty$county_name[i]) })
fail[[checks[5]]] <- sapply(sts, function(s)
  mean(!cty$county_fips[cty$state_name == s] %in% old$county_fips))
ramp2 <- function(v) {
  f <- colorRamp(c("#FFFFFF", "#C41230"))(pmin(pmax(v, 0), 1))
  rgb(f[, 1], f[, 2], f[, 3], maxColorValue = 255)
}
fail$total <- rowSums(fail[, checks])
clean <- sum(fail$total == 0)
hot   <- fail[order(-fail$total), ][fail$total[order(-fail$total)] > 0, ]
hot$abbrev <- ab[hot$state]

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

## ---- ds-raw-returns
source("../../_lib/structure.R")
RF <- "data/raw/2024_US_County_Level_Presidential_Results.csv"
GF <- "data/raw/2024_Gaz_counties_national.txt"
RL <- readLines(RF, warn = FALSE)
rawret <- read.csv(RF, stringsAsFactors = FALSE)
rawgaz <- read.delim(GF)          # no instructions, on purpose
GL <- readLines(GF, warn = FALSE)
set.seed(84355)
.pick <- sort(sample.int(nrow(rawret), 2))

# The file's own header line becomes the left column and two of its rows sit
# beside it, so ten columns can be read down the page instead of across it.
# The four the compilation computed for itself are marked as such: that is the
# distinction the paragraph below draws.
.cells <- lapply(RL[c(1, .pick + 1L)], function(x) trimws(strsplit(x, ",")[[1]]))
.k <- min(lengths(.cells))
.ret <- c(
  state_name = "the state, spelled out",
  county_fips = "county FIPS code, as text",
  county_name = "the county's name",
  votes_gop = "votes for the Republican — counted",
  votes_dem = "votes for the Democrat — counted",
  total_votes = "all votes cast — counted",
  diff = "computed: votes_gop minus votes_dem",
  per_gop = "computed: votes_gop divided by total_votes",
  per_dem = "computed: votes_dem divided by total_votes",
  per_point_diff = "computed: per_gop minus per_dem")
data.frame(Column_as_it_arrives = .cells[[1]][seq_len(.k)],
           What_it_holds = ifelse(.cells[[1]][seq_len(.k)] %in% names(.ret),
                                  unname(.ret[.cells[[1]][seq_len(.k)]]), "—"),
           Row_A = .cells[[2]][seq_len(.k)],
           Row_B = .cells[[3]][seq_len(.k)])

## ---- ds-raw-gaz
# Tab-separated and right-padded. The padding is preserved in the width column
# rather than trimmed away silently, because the padding is the finding.
.g <- lapply(GL[c(1, 2, 1001)], function(x) strsplit(x, "\t")[[1]])
.gk <- min(lengths(.g))
.gz <- c(
  USPS = "the state's two-letter code",
  GEOID = "county FIPS code — the column this chapter joins on",
  ANSICODE = "the Census Bureau's own permanent identifier for the county",
  NAME = "the county's name",
  ALAND = "land area, in square metres",
  AWATER = "water area, in square metres",
  ALAND_SQMI = "the same land area, in square miles",
  AWATER_SQMI = "the same water area, in square miles",
  INTPTLAT = "latitude of an interior point",
  INTPTLONG = "longitude of an interior point — the padded column")
.gn <- trimws(.g[[1]][seq_len(.gk)])
data.frame(Column_as_it_arrives = .gn,
           What_it_holds = ifelse(.gn %in% names(.gz), unname(.gz[.gn]), "—"),
           Row_1 = trimws(.g[[2]][seq_len(.gk)]),
           Row_1000 = trimws(.g[[3]][seq_len(.gk)]),
           Characters_as_stored = nchar(.g[[2]][seq_len(.gk)]))

## ---- ds-scan
s1 <- dd_scan(rawret); s1$file <- "returns"
s2 <- dd_scan(rawgaz); s2$file <- "Gazetteer"
s <- rbind(s1[s1$column %in% c("state_name", "county_fips", "votes_gop",
                               "per_gop"), ],
           s2[s2$column %in% c("USPS", "GEOID", "NAME", "INTPTLONG"), ])
s <- s[, c("file", "column", "stored", "level", "distinct", "mismatch")]
names(s) <- c("file", "column", "stored as", "what it is", "distinct",
              "mismatch")
s

## ---- ds-peek
p <- dd_peek(rawret)[, c("state_name", "county_fips", "county_name",
                         "votes_gop", "votes_dem", "per_gop")]
names(p) <- c("state", "fips", "county", "gop", "dem", "per gop")
p

## ---- ds-clean
q <- cty[match(p$county_fips, as.integer(cty$county_fips)),
         c("state_name", "county_fips", "county_name", "votes_dem",
           "votes_gop", "total_votes")]
rownames(q) <- NULL
names(q) <- c("state", "fips", "county", "dem", "gop", "total")
q

## ---- one-row
o <- cty[cty$county_fips == "42003",
         c("state_name", "county_fips", "county_name",
           "votes_dem", "votes_gop", "total_votes")]
names(o) <- c("state", "FIPS", "county", "Democratic votes",
              "Republican votes", "total votes")
o

## ---- fips
data.frame(
  step = c("What the code should look like", "What R made of it",
           "Rows now the wrong length", "Out of"),
  value = c("01001 (five characters, text)",
            paste0(class(naive$county_fips), ": ",
                   format(naive$county_fips[1], big.mark = "")),
            n(sum(nchar(naive$county_fips) == 4)), n(nrow(naive))))

## ---- fips-states
data.frame(states_affected = paste(
  sort(unique(naive$state_name[nchar(naive$county_fips) == 4])), collapse = ", "))

## ---- agg
data.frame(
  quantity = c("States after aggregating counties", "States agreeing exactly",
               "States where the county sum gives Harris a HIGHER share",
               "States where it gives her a LOWER share",
               "States where the winner changes"),
  value = c(nrow(cmp), sum(cmp$dh == 0 & cmp$dt == 0),
            sum(cmp$dh > 0), sum(cmp$dh < 0),
            sum(ifelse(cmp$hc > cmp$tc, "Harris", "Trump") != cmp$winner)))

## ---- other
z <- tapply(cty$other, cty$state_name, function(x) sum(x == 0))
u <- table(cty$state_name)
data.frame(
  quantity = c("Counties reporting zero third-party votes",
               "Smallest third-party count anywhere",
               "States where EVERY unit reports zero"),
  value = c(n(sum(cty$other == 0)), min(cty$other),
            paste(names(z)[z == u[names(z)]], collapse = ", ")))

## ---- tp-static
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0.5, max(tile$col) + 0.5), ylim = c(max(tile$row) + 0.6, 0.05),
     axes = FALSE, xlab = "", ylab = "")
rect(tile$col - 0.46, tile$row - 0.46, tile$col + 0.46, tile$row + 0.46,
     col = tile$fill, border = "#DDDDDD")
z <- tile$share == 0
rect(tile$col[z] - 0.46, tile$row[z] - 0.46, tile$col[z] + 0.46,
     tile$row[z] + 0.46, border = "#C41230", lwd = 2.6)
text(tile$col, tile$row - 0.13, tile$abbrev, cex = 0.72, font = 2, col = tile$ink)
text(tile$col, tile$row + 0.22, tile$lab, cex = 0.5, col = tile$ink)
gx <- seq(0.6, 2.6, length.out = 40)
rect(gx, 0.14, gx + 0.055, 0.32, col = ramp(seq(0, max(tile$share),
     length.out = 40), max(tile$share)), border = NA)
text(0.55, 0.23, "0", adj = 1, cex = 0.68, col = "#444444")
text(2.72, 0.23, paste0(pc(max(tile$share)), "%"), adj = 0, cex = 0.68,
     col = "#444444")
text(4.3, 0.23, "third-party share the county file records", adj = 0, cex = 0.7,
     col = "#444444")
text(max(tile$col) + 0.5, 0.23, adj = 1, cex = 0.7, col = "#C41230",
     labels = paste0("red outline: recorded as exactly zero (",
                     paste(zero_st, collapse = ", "), ")"))

## ---- tp-d3
rows <- paste(sprintf('{"a":"%s","s":"%s","c":%d,"r":%d,"v":%.2f,"o":%d,"t":%d}',
                      tile$abbrev, tile$state, tile$col, tile$row, tile$share,
                      tile$other, tile$total_votes), collapse = ",")
cat(paste0('
<div id="tp" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '], HI=', max(tile$share), ';
const NC=', max(tile$col), ', NR=', max(tile$row), ';
const W=760,M={t:26,r:8,b:8,l:8},cell=Math.min((W-M.l-M.r)/NC,64);
const H=M.t+M.b+NR*cell;
const col=d3.scaleSequential(d3.interpolate("#FFFFFF","#8856a7")).domain([0,HI]);
const box=d3.select("#tp");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const px=d=>M.l+(d.c-1)*cell, py=d=>M.t+(d.r-1)*cell;
const ink=d=>d.v>0.55*HI?"#fff":"#333";
const key=svg.append("g");
d3.range(40).forEach(i=>{
  key.append("rect").attr("x",M.l+i*4).attr("y",5).attr("width",4).attr("height",11)
    .attr("fill",col(i/39*HI));
});
key.append("text").attr("x",M.l+168).attr("y",15).attr("font-size","11px")
  .attr("fill","#444").text("0 to "+HI.toFixed(2)+"% — third-party share the county file records");
svg.append("text").attr("x",W-M.r).attr("y",15).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("red outline: recorded as exactly zero (', paste(zero_st, collapse = ", "), ')");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const f=d3.format(",");
const g=svg.append("g");
g.selectAll("rect").data(D).join("rect")
  .attr("x",d=>px(d)+1).attr("y",d=>py(d)+1)
  .attr("width",cell-2).attr("height",cell-2)
  .attr("fill",d=>col(d.v))
  .attr("stroke",d=>d.v===0?"#C41230":"#DDDDDD").attr("stroke-width",d=>d.v===0?2.6:1)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.s}</b><br>${f(d.o)} third-party votes recorded<br>`+
      `of ${f(d.t)} ballots — ${d.v.toFixed(2)}%`)
      .style("left",Math.min(ev.offsetX+14,W-260)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
g.selectAll("text.a").data(D).join("text").attr("class","a")
  .attr("x",d=>px(d)+cell/2).attr("y",d=>py(d)+cell/2+1).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","700").attr("pointer-events","none")
  .attr("fill",ink).text(d=>d.a);
g.selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>px(d)+cell/2).attr("y",d=>py(d)+cell/2+15).attr("text-anchor","middle")
  .attr("font-size","10px").attr("pointer-events","none")
  .attr("fill",ink).text(d=>d.v.toFixed(2));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a state for the counts behind its shading.</i></p>
'))

## ---- missing
h <- head(cmp[order(-cmp$missing_pct),
              c("state_name", "total_votes", "implied", "missing", "missing_pct")], 6)
h$total_votes <- n(h$total_votes); h$implied <- n(h$implied); h$missing <- n(h$missing)
names(h) <- c("state", "ballots in county file", "implied statewide total",
              "missing", "% missing")
h

## ---- miss-static
q <- head(cmp[order(-cmp$missing_pct), ], MISSN)
q <- q[order(q$missing_pct), ]
par(mar = c(4.5, 9, 1, 2))
barplot(q$missing_pct, names.arg = q$state_name, horiz = TRUE, las = 1,
        col = ifelse(q$missing_pct > MISSRED, "#C41230", "#4D4D4D"), border = NA,
        cex.names = 0.8, xlab = "% of ballots the county file never saw")

## ---- miss-d3
q <- head(cmp[order(-cmp$missing_pct), ], MISSN)
rows <- paste(sprintf('{"s":"%s","v":%.2f,"m":%d,"t":%d}',
                      q$state_name, q$missing_pct, q$missing, q$total_votes),
              collapse = ",")
cat(sprintf('
<div id="miss" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=420,M={t:16,r:24,b:44,l:150};
const box=d3.select("#miss");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.v)*1.08]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([M.t,H-M.b]).padding(0.22);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(d=>d+"%%"));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y));
svg.append("text").attr("x",(W+M.l-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("share of ballots the county file never saw");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const f=d3.format(",");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",x(0)).attr("y",d=>y(d.s)).attr("height",y.bandwidth())
  .attr("width",d=>x(d.v)-x(0))
  .attr("fill",d=>d.v>%.1f?"#C41230":"#4D4D4D")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.s}</b><br>${f(d.m)} ballots missing<br>${d.v}%% of the implied total`)
      .style("left",Math.min(ev.offsetX+14,W-230)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a bar for ballot counts.</i></p>
', rows, MISSRED))

## ---- namejoin
data.frame(
  step = c("Rows of returns going in", "Official counties going in",
           "Rows coming out of a name join",
           "'Washington County' rows in the returns",
           "'Washington County' rows in the census list",
           "'Washington County' rows after the join"),
  value = c(n(nrow(cty)), n(nrow(cen)), n(nrow(by_name)),
            sum(cty$county_name == "Washington County"),
            sum(cen$name == "Washington County"),
            n(sum(by_name$county_name == "Washington County"))))

## ---- wash-static
par(mar = c(0.2, 0.2, 1.8, 0.2))
plot(NA, xlim = c(-0.16, 1.16), ylim = c(length(wret) + 1.4, 0.2), axes = FALSE,
     xlab = "", ylab = "")
mtext("election returns", side = 3, at = 0, line = 0.2, cex = 0.82, font = 2)
mtext("census county list", side = 3, at = 1, line = 0.2, cex = 0.82, font = 2)
segments(0, wpair$l[!wpair$ok], 1, wpair$r[!wpair$ok],
         col = "#00000012", lwd = 0.5)
segments(0, wpair$l[wpair$ok], 1, wpair$r[wpair$ok], col = "#C41230", lwd = 1.6)
points(rep(0, length(wret)), seq_along(wret), pch = 19, cex = 0.5, col = "#444444")
points(rep(1, length(wcen)), seq_along(wcen), pch = 19, cex = 0.5, col = "#444444")
text(-0.02, seq_along(wret), wret, adj = 1, cex = 0.58, col = "#444444")
text(1.02, seq_along(wcen), wcen, adj = 0, cex = 0.58, col = "#444444")
text(0.5, length(wret) + 1.1, cex = 0.75, col = "#C41230",
     labels = paste0("red: the ", sum(wpair$ok), " joins that are right. gray: the ",
                     n(sum(!wpair$ok)), " that are not."))

## ---- wash-d3
edges <- paste(sprintf('{"l":%d,"r":%d,"k":%d}', wpair$l, wpair$r,
                       as.integer(wpair$ok)), collapse = ",")
nodes <- paste(sprintf('{"i":%d,"a":"%s"}', seq_along(wret), wret), collapse = ",")
cat(paste0('
<div id="wj" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const E=[', edges, '], N=[', nodes, '];
const W=760,H=620,M={t:38,r:60,b:16,l:60};
const box=d3.select("#wj");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const xa=M.l+26,xb=W-M.r-26;
const y=d3.scaleLinear().domain([1,N.length]).range([M.t,H-M.b]);
svg.append("text").attr("x",xa).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600").text("election returns");
svg.append("text").attr("x",xb).attr("y",16).attr("text-anchor","middle")
  .attr("font-size","13px").attr("font-weight","600").text("census county list");
svg.append("text").attr("x",(xa+xb)/2).attr("y",32).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("red: the ', sum(wpair$ok), ' joins that are right. gray: the ',
      n(sum(!wpair$ok)), ' that are not.");
const g=svg.append("g");
g.selectAll("line.e").data(E).join("line").attr("class","e")
  .attr("x1",xa).attr("x2",xb).attr("y1",d=>y(d.l)).attr("y2",d=>y(d.r))
  .attr("stroke",d=>d.k?"#C41230":"#000")
  .attr("stroke-opacity",d=>d.k?1:0.07)
  .attr("stroke-width",d=>d.k?1.6:0.6);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
[[xa,-1,"a"],[xb,1,"b"]].forEach(([px,dir,cls])=>{
  g.selectAll("circle."+cls).data(N).join("circle").attr("class",cls)
    .attr("cx",px).attr("cy",d=>y(d.i)).attr("r",3.2).attr("fill","#444")
    .on("mousemove",function(ev,d){
      g.selectAll("line.e").attr("stroke-opacity",e=>
        (cls==="a"?e.l:e.r)===d.i?(e.k?1:0.55):(e.k?0.25:0.03));
      tip.style("opacity",1).html(
        `<b>Washington County, ${d.a}</b><br>matches ${N.length} rows on the `+
        `other side<br>exactly ${1} of them is the same place`)
        .style("left",Math.min(ev.offsetX+14,W-300)+"px").style("top",(ev.offsetY-10)+"px"); })
    .on("mouseleave",function(){
      g.selectAll("line.e").attr("stroke-opacity",e=>e.k?1:0.07);
      tip.style("opacity",0); });
  g.selectAll("text."+cls).data(N).join("text").attr("class",cls)
    .attr("x",px+dir*8).attr("y",d=>y(d.i)+3.5)
    .attr("text-anchor",dir<0?"end":"start").attr("font-size","10px")
    .attr("fill","#555").attr("pointer-events","none").text(d=>d.a);
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a state to follow the rows its own Washington County produces.</i></p>
'))

## ---- wrongjoin
w <- wrong[, c("county_fips", "county_name", "name", "total_votes")]
w$total_votes <- n(w$total_votes)
names(w) <- c("FIPS", "name in the returns", "name in the census list", "votes")
w

## ---- years
data.frame(
  quantity = c("Rows in the 2020 file", "Rows in the 2024 file",
               "2024 codes absent from 2020", "2020 codes absent from 2024",
               "Connecticut codes the two files share"),
  value = c(n(nrow(old)), n(nrow(cty)),
            n(sum(!cty$county_fips %in% old$county_fips)),
            n(sum(!old$county_fips %in% cty$county_fips)),
            length(intersect(old$county_fips[old$state_name == "Connecticut"],
                             cty$county_fips[cty$state_name == "Connecticut"]))))

## ---- swing
h <- head(sw[order(-sw$swing),
             c("state_name", "county_name", "r20", "r24", "swing")], 5)
h$r20 <- pc(h$r20, 1); h$r24 <- pc(h$r24, 1)
names(h) <- c("state", "county", "Republican % 2020", "Republican % 2024",
              "swing")
h

## ---- bookkeeping
data.frame(
  step = c("Reporting units in the 2024 file", "Reporting units in the 2020 file",
           "Units that matched", "States absent from the result entirely"),
  value = c(n(nrow(cty)), n(nrow(old)), n(nrow(sw)),
            paste(setdiff(cty$state_name, sw$state_name), collapse = " and ")))

## ---- dcrow
o <- dcrow[, c("county_fips", "county_name", "r20", "r24", "swing")]
o$r20 <- pc(o$r20, 2); o$r24 <- pc(o$r24, 2)
names(o) <- c("FIPS", "county", "Republican % 2020", "Republican % 2024", "swing")
o

## ---- alluvial-static
par(mar = c(0.2, 0.2, 1.6, 0.2))
plot(NA, xlim = c(-0.62, 1.62), ylim = c(ALH + 0.22, -0.04), axes = FALSE,
     xlab = "", ylab = "")
mtext("the 2020 file", side = 3, at = 0.02, adj = 1, line = 0.1, cex = 0.8,
      font = 2)
mtext("the 2024 file", side = 3, at = 0.98, adj = 0, line = 0.1, cex = 0.8,
      font = 2)
xs <- seq(0.06, 0.94, length.out = 40)
sm <- (xs - 0.06) / 0.88; sm <- sm * sm * (3 - 2 * sm)
for (i in seq_len(nrow(rib))) {
  polygon(c(xs, rev(xs)),
          c(rib$sy0[i] + (rib$ty0[i] - rib$sy0[i]) * sm,
            rev(rib$sy1[i] + (rib$ty1[i] - rib$sy1[i]) * sm)),
          col = paste0(rib$col[i], "AA"), border = NA)
}
rect(0.00, LL$y0, 0.06, LL$y0 + LL$h, col = "#555555", border = NA)
rect(0.94, RR$y0, 1.00, RR$y0 + RR$h, col = "#555555", border = NA)
text(-0.03, LL$y0 + LL$h / 2, paste0(Lname, "  (", sapply(Lk, n), ")"),
     adj = 1, cex = 0.62, col = "#333333")
text(1.03, RR$y0 + RR$h / 2, paste0(Rname, "  (", sapply(Rk, n), ")"),
     adj = 0, cex = 0.62, col = "#333333")
text(0.5, rib$sy0[2] - 0.035, cex = 0.62, col = "#C41230",
     labels = paste0("the collision: ", n(rib$k[2]),
                     " row joined to a different place"))

# ---- legend: color was carrying four meanings with nothing to decode it ----
lxc <- c(-0.42, 0.46)[c(1, 2, 1, 2)]                 # two columns, two rows
lyc <- ALH + c(0.055, 0.055, 0.105, 0.105)
rect(lxc, lyc - 0.013, lxc + 0.045, lyc + 0.013, col = RIBLEG$col, border = NA)
text(lxc + 0.056, lyc, paste0(RIBLEG$lab, " (", sapply(RIBLEG$k, n), ")"),
     adj = 0, cex = 0.58, col = "#333333")
text(0.5, ALH + 0.165, adj = c(0.5, 0.5), cex = 0.55, col = "#777777",
     labels = paste0("Ribbon thickness is the number of reporting units, and is",
                     " the same at both ends. The ", sum(rib$floored),
                     " thinnest ribbons are drawn at a minimum width"))
text(0.5, ALH + 0.198, adj = c(0.5, 0.5), cex = 0.55, col = "#777777",
     labels = paste0("so they stay visible; every other ribbon is to scale, ",
                     "across a range of ", n(max(rib$k)), " to ",
                     n(min(rib$k)), " units."))

## ---- alluvial-d3
rrows <- paste(sprintf(
  '{"a":%.5f,"b":%.5f,"c":%.5f,"d":%.5f,"k":%d,"col":"%s","kind":"%s","f":"%s","t":"%s"}',
  rib$sy0, rib$sy1, rib$ty0, rib$ty1, rib$k, rib$col, rib$kind,
  Lname[rib$s], Rname[rib$t]), collapse = ",")
lrows <- paste(sprintf('{"y":%.5f,"h":%.5f,"t":"%s","k":%d}',
                       LL$y0, LL$h, Lname, Lk), collapse = ",")
rrws  <- paste(sprintf('{"y":%.5f,"h":%.5f,"t":"%s","k":%d}',
                       RR$y0, RR$h, Rname, Rk), collapse = ",")
lgrows <- paste(sprintf('{"c":"%s","l":"%s","k":%d}',
                        RIBLEG$col, RIBLEG$lab, RIBLEG$k), collapse = ",")
cat(paste0('
<div id="al" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[', rrows, '], L=[', lrows, '], Q=[', rrws, '];
const LEG=[', lgrows, '];
const ALH=', ALH, ';
const W=760,M={t:34,r:220,b:56,l:220};
const bandH=310, H=M.t+M.b+bandH;
const box=d3.select("#al");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const xa=M.l,xb=W-M.r;
const y=v=>M.t+v/ALH*bandH;
svg.append("text").attr("x",xa+8).attr("y",18).attr("text-anchor","end")
  .attr("font-size","13px").attr("font-weight","600").text("the 2020 file");
svg.append("text").attr("x",xb-8).attr("y",18)
  .attr("font-size","13px").attr("font-weight","600").text("the 2024 file");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const f=d3.format(",");
const mid=(xa+xb)/2;
const path=d=>`M${xa},${y(d.a)} C${mid},${y(d.a)} ${mid},${y(d.c)} ${xb},${y(d.c)}`
  +` L${xb},${y(d.d)} C${mid},${y(d.d)} ${mid},${y(d.b)} ${xa},${y(d.b)} Z`;
svg.append("g").selectAll("path").data(R).join("path")
  .attr("d",path).attr("fill",d=>d.col).attr("fill-opacity",0.62)
  .on("mousemove",function(ev,d){
    d3.select(this).attr("fill-opacity",0.9);
    tip.style("opacity",1).html(
      `<b>${f(d.k)} reporting unit${d.k===1?"":"s"}</b><br>${d.f} &rarr; ${d.t}`)
      .style("left",Math.min(ev.offsetX+14,W-340)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",function(){ d3.select(this).attr("fill-opacity",0.62);
    tip.style("opacity",0); });
[[L,xa-10,-1],[Q,xb+10,1]].forEach(([S,tx,dir])=>{
  svg.append("g").selectAll("rect").data(S).join("rect")
    .attr("x",dir<0?xa-9:xb).attr("y",d=>y(d.y)).attr("width",9)
    .attr("height",d=>y(d.h)-M.t).attr("fill","#555");
  svg.append("g").selectAll("text").data(S).join("text")
    .attr("x",tx).attr("y",d=>y(d.y+d.h/2)+4)
    .attr("text-anchor",dir<0?"end":"start").attr("font-size","11px")
    .attr("fill","#333").text(d=>d.t+"  ("+f(d.k)+")");
});
const dc=R.find(d=>d.kind==="collision");
svg.append("text").attr("x",mid).attr("y",y(dc.a)-8).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230")
  .text("the collision: "+f(dc.k)+" row joined to a different place");
// legend: color was carrying four meanings with nothing to decode it
const lg=svg.append("g").attr("transform",`translate(28,${H-M.b+16})`);
let lxp=0;
LEG.forEach(d=>{
  lg.append("rect").attr("x",lxp).attr("y",-9).attr("width",16).attr("height",11)
    .attr("fill",d.c).attr("fill-opacity",0.62);
  const txt=d.l+" ("+f(d.k)+")";
  lg.append("text").attr("x",lxp+21).attr("y",0).attr("font-size","11px")
    .attr("fill","#444").text(txt);
  lxp+=31+txt.length*5.5;
});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a ribbon for the number of reporting units it carries.</i></p>
'))

## ---- checks-static
par(mar = c(1.8, 6.0, 3.6, 0.4))
plot(NA, xlim = c(0.5, length(checks) + 0.5), ylim = c(nrow(hot) + 0.5, 0.5),
     axes = FALSE, xlab = "", ylab = "")
for (j in seq_along(checks)) {
  ln <- strwrap(checks[j], 15)
  for (k in seq_along(ln))
    mtext(ln[k], side = 3, at = j, line = length(ln) - k + 0.1, cex = 0.6)
}
for (j in seq_along(checks)) {
  v <- hot[[checks[j]]]
  rect(j - 0.47, seq_len(nrow(hot)) - 0.44, j + 0.47,
       seq_len(nrow(hot)) + 0.44,
       col = ramp2(v), border = "#FFFFFF", lwd = 1.4)
  nz <- v > 0
  text(j, which(nz), paste0(floor(100 * v[nz] + 0.5), "%"), cex = 0.62,
       col = ifelse(v[nz] > 0.55, "#FFFFFF", "#333333"))
}
text(0.36, seq_len(nrow(hot)), hot$state, adj = 1, cex = 0.66, xpd = NA)
mtext(paste0("the other ", clean, " states are clean on all ", length(checks),
             " checks"), side = 1, line = 0.4, adj = 0, cex = 0.66,
      col = "#4d9221")

## ---- checks-d3
cells <- do.call(rbind, lapply(seq_along(checks), function(j)
  data.frame(j = j, i = seq_len(nrow(hot)), v = hot[[checks[j]]],
             st = hot$state, ck = checks[j], stringsAsFactors = FALSE)))
cells$nn <- round(cells$v * sapply(cells$st, function(s) sum(cty$state_name == s)))
crows <- paste(sprintf('{"j":%d,"i":%d,"v":%.4f,"s":"%s","c":"%s","n":%d,"tot":%d}',
                       cells$j, cells$i, cells$v, cells$st, cells$ck, cells$nn,
                       sapply(cells$st, function(s) sum(cty$state_name == s))),
               collapse = ",")
cat(paste0('
<div id="ck" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const C=[', crows, '];
const CK=[', paste(sprintf('"%s"', checks), collapse = ","), '];
const ST=[', paste(sprintf('"%s"', hot$state), collapse = ","), '];
const W=760,M={t:74,r:14,b:10,l:150};
const cw=(W-M.l-M.r)/CK.length, ch=26;
const H=M.t+M.b+ST.length*ch;
const col=d3.scaleSequential(d3.interpolate("#FFFFFF","#C41230")).domain([0,1]);
const box=d3.select("#ck");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
CK.forEach((t,j)=>{
  const words=t.split(" "); let line="",lines=[];
  words.forEach(w=>{ if((line+" "+w).trim().length>15){lines.push(line.trim());line=w;}
                     else line+=" "+w; });
  lines.push(line.trim());
  lines.forEach((l,k)=>svg.append("text")
    .attr("x",M.l+j*cw+cw/2).attr("y",M.t-8-(lines.length-1-k)*13)
    .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#444").text(l));
});
svg.append("g").selectAll("text").data(ST).join("text")
  .attr("x",M.l-8).attr("y",(d,i)=>M.t+i*ch+ch/2+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#333").text(d=>d);
svg.append("text").attr("x",M.l-8).attr("y",M.t-8).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#4d9221")
  .text("the other ', clean, ' states are clean");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
g.selectAll("rect").data(C).join("rect")
  .attr("x",d=>M.l+(d.j-1)*cw+1).attr("y",d=>M.t+(d.i-1)*ch+1)
  .attr("width",cw-2).attr("height",ch-2).attr("fill",d=>col(d.v))
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.s}</b><br>${d.c}<br>${d.n} of ${d.tot} reporting units`)
      .style("left",Math.min(ev.offsetX+14,W-320)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
g.selectAll("text").data(C.filter(d=>d.v>0)).join("text")
  .attr("x",d=>M.l+(d.j-1)*cw+cw/2).attr("y",d=>M.t+(d.i-1)*ch+ch/2+4)
  .attr("text-anchor","middle").attr("font-size","11px").attr("pointer-events","none")
  // on-mark: the percentage is inside its heatmap cell and is coloured against
  // the cell. The state names to the left share this #333 and are on the page.
  .attr("class","on-mark")
  .attr("fill",d=>d.v>0.55?"#fff":"#333").text(d=>Math.round(d.v*100)+"%");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
<i>Hover a cell for the number of reporting units behind it.</i></p>
'))

## ---- offvcomp
data.frame(
  The_file = c("Official state returns", "The compilation"),
  National_Democratic_2024 = c(n(OFF_D), n(COMP_D)),
  Distance_from_AP = c(sprintf("%+d", OFF_ERR), sprintf("%+d", CMP_ERR)))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#tp text[fill="#333" i]
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
#tp text[fill="#fff" i],
#tp text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#tp text[fill="#fff" i],
#tp text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
