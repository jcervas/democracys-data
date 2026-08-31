# dw-nominate-code.R -- chunk bodies for dw-nominate-brief.Rmd
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
                      dpi = 96, fig.retina = 1)   # these figures are point-heavy
options(scipen = 999)

nom <- read.csv("data/derived/nominate_members.csv", stringsAsFactors = FALSE)

series <- function(ch) {
  x  <- nom[nom$chamber == ch, ]
  cg <- sort(unique(x$congress))
  md <- function(c_, p) median(x$dim1[x$congress == c_ & x$party == p])
  ov <- function(c_) {
    y <- x[x$congress == c_, ]
    sum(y$dim1[y$party == "Republican"] < max(y$dim1[y$party == "Democrat"]))
  }
  data.frame(congress = cg,
             year = sapply(cg, function(c_) x$year[x$congress == c_][1]),
             dem  = sapply(cg, md, p = "Democrat"),
             rep  = sapply(cg, md, p = "Republican"),
             overlap = sapply(cg, ov))
}
H <- series("House"); S <- series("Senate")
H$gap <- H$rep - H$dem; S$gap <- S$rep - S$dem

g <- function(d, cg) d$gap[d$congress == cg]
NOWH <- max(H$congress); NOWS <- max(S$congress)
YRNOW <- H$year[H$congress == NOWH]
Y67 <- H$year[H$congress == 90]

DMOVE <- H$dem[H$congress == NOWH] - H$dem[H$congress == 90]
RMOVE <- H$rep[H$congress == NOWH] - H$rep[H$congress == 90]

LASTOVH <- max(H$congress[H$overlap > 0])
LASTOVS <- max(S$congress[S$overlap > 0])
ZEROH   <- H$year[H$overlap == 0]

G1890 <- max(H$gap[H$year >= 1889 & H$year <= 1901])
Y1890 <- H$year[which(H$gap == G1890)]

d2 <- sapply(c(80, 90, 100, NOWH), function(c_)
  sd(nom$dim2[nom$chamber == "House" & nom$congress == c_]))
d2y <- sapply(c(80, 90, 100, NOWH), function(c_)
  nom$year[nom$chamber == "House" & nom$congress == c_][1])

nm <- function(x, k = 3) formatC(x, format = "f", digits = k)
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# ---- the download itself, for the raw-to-clean section ----------------------
# A verbatim copy of the Voteview download is committed in this course, by the
# `retirements` lab, which needs the same file. Read it from there rather than
# keeping a second 6 MB copy.
source("../../_lib/structure.R")
RAWF <- "../../03-elections/retirements/data/raw/HSall_members.csv"
stopifnot(file.exists(RAWF))
rawnom <- read.csv(RAWF, stringsAsFactors = FALSE)
RAWL   <- readLines(RAWF, warn = FALSE)
stopifnot(length(RAWL) == nrow(rawnom) + 1L)
RAWMB  <- round(file.size(RAWF) / 1e6, 1)

RAWCG <- max(nom$congress)
.cand <- which(rawnom$congress == RAWCG & rawnom$chamber == "House" &
               rawnom$party_code %in% c(100, 200) &
               !is.na(rawnom$nominate_dim1))
set.seed(84355)
.imem  <- .cand[sample.int(length(.cand), 1)]
RAWNM  <- rawnom$bioname[.imem]
.ipres <- which(rawnom$congress == RAWCG & rawnom$chamber == "President")[1]
.iind  <- which(rawnom$congress == RAWCG & rawnom$chamber != "President" &
                !(rawnom$party_code %in% c(100, 200)))[1]
RAWSEL <- RAWL[c(.imem, .ipres, .iind) + 1L]
stopifnot(!any(is.na(RAWSEL)))

NPRES      <- sum(rawnom$chamber == "President")
.s1        <- rawnom[rawnom$chamber %in% c("House", "Senate"), ]
NPARTY     <- length(unique(rawnom$party_code))
.s2        <- .s1[.s1$party_code %in% c(100, 200), ]
NDROPPARTY <- nrow(.s1) - nrow(.s2)
.s3        <- .s2[!is.na(.s2$nominate_dim1), ]
NDROPDIM   <- nrow(.s2) - nrow(.s3)
NSURV      <- sum(.s3$congress >= min(nom$congress))
NDOT       <- sum(grepl("\\.0$", sub("^([^,]*,){6}([^,]*),.*$", "\\2", RAWL[-1])))
NIND       <- length(unique(rawnom$party_code[
                rawnom$congress == RAWCG & rawnom$chamber != "President" &
                !(rawnom$party_code %in% c(100, 200))]))

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- switchers
.k  <- paste(nom$congress, nom$chamber, nom$bioname)
.dp <- unique(.k[duplicated(.k)])
SWN <- length(.dp)
SWP <- sum(vapply(split(nom$party[.k %in% .dp], .k[.k %in% .dp]),
                  function(z) length(unique(z)) > 1L, logical(1)))

## ---- nom-raw-header
# The header does have names -- unlike most files in this book -- so the useful
# rendering is position, name and what the name means. The descriptions are
# Voteview's own, from its field documentation, except where marked; the note
# on `conditional` is read off the file at knit time rather than asserted.
.hd <- trimws(strsplit(RAWL[1], ",")[[1]])
.desc <- c(
  congress = "the numbered two-year Congress this row describes",
  chamber = "House, Senate or President",
  icpsr = "the member's permanent ID — the same person keeps it across every Congress they serve",
  state_icpsr = "ICPSR's own state code, which is not the Census FIPS code",
  district_code = "district number; 0 for senators and for at-large members",
  state_abbrev = "two-letter postal abbreviation",
  party_code = "ICPSR party code: 100 is Democrat, 200 is Republican",
  occupancy = "which occupant of the seat this is in this Congress (0 = only occupant)",
  last_means = "how the member reached office — general election, special election, appointment",
  bioname = "name, as last, first middle",
  bioguide_id = "Biographical Directory of Congress ID — the join key to other congressional sources",
  born = "year of birth",
  died = "year of death, empty for the living",
  nominate_dim1 = "the DW-NOMINATE first dimension, −1 to +1: the liberal–conservative score this chapter is about",
  nominate_dim2 = "the second dimension, historically the cross-cutting one of region and race",
  nominate_log_likelihood = "how well the model fits this member's votes",
  nominate_geo_mean_probability = "average probability the model assigned to the votes actually cast; 1 would be perfect",
  nominate_number_of_votes = "votes the score was estimated from",
  nominate_number_of_errors = "votes the model classifies wrongly",
  conditional = "documented, and empty: every row is missing",
  nokken_poole_dim1 = "dimension 1 re-estimated within each Congress separately, rather than fixed across a career",
  nokken_poole_dim2 = "the same, for dimension 2")
data.frame(Position = seq_along(.hd),
           Column_name = .hd,
           What_it_is = unname(.desc[.hd]))

## ---- nom-raw-rows
# The three rows down the page rather than across it, so that the column whose
# value differs between them can be found by reading one line.
.rows <- lapply(RAWSEL, function(x) trimws(strsplit(x, ",")[[1]]))
.rk <- min(c(length(.hd), lengths(.rows)))
.out <- data.frame(Column = .hd[seq_len(.rk)], stringsAsFactors = FALSE)
for (i in seq_along(.rows))
  .out[[paste0("Row_", i)]] <- ifelse(nzchar(.rows[[i]][seq_len(.rk)]),
                                      .rows[[i]][seq_len(.rk)], "(empty)")
.out

## ---- nom-peek
p <- dd_peek(rawnom)[, c("congress", "chamber", "icpsr", "state_abbrev",
                         "party_code", "bioname", "nominate_dim1")]
names(p) <- c("congress", "chamber", "icpsr", "state", "party code",
              "bioname", "dim1")
p

## ---- nom-scan
sc <- dd_scan(rawnom)
sc <- sc[sc$column %in% c("congress", "icpsr", "district_code", "party_code",
                          "bioname", "born", "nominate_dim1", "conditional"),
         c("column", "stored", "level", "missing", "mismatch")]
names(sc) <- c("field", "stored as", "what it is", "% missing", "mismatch")
sc

## ---- nom-clean
o <- nom[nom$congress == RAWCG & nom$chamber == "House" &
         nom$bioname == RAWNM, ]
o <- o[, c("congress", "year", "chamber", "state", "district_code",
           "bioname", "party", "dim1", "dim2")]
names(o) <- c("congress", "year", "chamber", "state", "district",
              "member", "party", "dim 1", "dim 2")
o

## ---- one-row
o <- nom[nom$chamber == "House" & nom$congress == NOWH, ]
o <- o[order(o$dim1), ]
o <- rbind(head(o, 2), o[round(nrow(o)/2), ], tail(o, 2))
o <- o[, c("congress", "year", "chamber", "state", "bioname", "party",
           "dim1", "dim2")]
o$dim1 <- nm(o$dim1); o$dim2 <- nm(o$dim2)
names(o) <- c("congress", "year", "chamber", "state", "member", "party",
              "dim 1", "dim 2")
o

## ---- coverage
data.frame(
  quantity = c("Member-Congress records", "House records", "Senate records",
               "Congresses covered", "Years covered"),
  value = c(n(nrow(nom)), n(sum(nom$chamber == "House")),
            n(sum(nom$chamber == "Senate")),
            paste0(min(nom$congress), "th–", max(nom$congress), "th"),
            paste0(min(nom$year), "–", max(nom$year))))

## ---- current
cur <- nom[nom$chamber == "House" & nom$congress == NOWH, ]
data.frame(
  quantity = c("Congress", "Year", "Members with a score",
               "Median Democrat", "Median Republican", "Distance between them"),
  value = c(paste0(NOWH, "th"), YRNOW, n(nrow(cur)),
            nm(H$dem[H$congress == NOWH]), nm(H$rep[H$congress == NOWH]),
            nm(g(H, NOWH))))

## ---- strip-prep
DCOL <- "#2166AC"; RCOL <- "#B2182B"          # party, and nothing else, in blue and red
set.seed(84355)
strip <- function(cg) {
  z <- nom[nom$chamber == "House" & nom$congress == cg, ]
  z$jit  <- runif(nrow(z), -1, 1)             # drawn once, used by both formats
  z$year1 <- z$year[1]
  z
}
STRIPS <- lapply(c(NOWH, 90, 105), strip)
names(STRIPS) <- c("now", "c90", "c105")

## ---- current-d3
z <- STRIPS[["now"]]
rows <- paste0("[", formatC(z$dim1, format = "f", digits = 3), ",",
               formatC(z$jit, format = "f", digits = 3), ",",
               ifelse(z$party == "Democrat", 0, 1), "]", collapse = ",")
cat(paste0('
<div id="st0" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '].map(r=>({x:r[0],j:r[1],p:r[2]}));
const W=770,H=170,M={t:34,r:24,b:52,l:24};
const svg=d3.select("#st0").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-1,1]).range([M.l,W-M.r]);
const mid=(M.t+H-M.b)/2, amp=(H-M.b-M.t)/2-4;
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","3 3");
svg.selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>mid+d.j*amp).attr("r",2.6)
  .attr("fill",d=>d.p===0?"', '#2166AC', '":"', '#B2182B', '").attr("opacity",0.45);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x).ticks(9));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",M.t-14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("font-weight","600").text("The House, ', YRNOW, '");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("DW-NOMINATE dimension 1 (liberal to conservative)");
svg.append("circle").attr("cx",M.l+4).attr("cy",M.t-18).attr("r",4).attr("fill","', '#2166AC', '");
svg.append("text").attr("x",M.l+13).attr("y",M.t-14).attr("font-size","11px").text("Democrat");
svg.append("circle").attr("cx",M.l+82).attr("cy",M.t-18).attr("r",4).attr("fill","', '#B2182B', '");
svg.append("text").attr("x",M.l+91).attr("y",M.t-14).attr("font-size","11px").text("Republican");
svg.append("text").attr("x",W-M.r).attr("y",M.t-14).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#666").text("vertical position is random, to separate the dots");
})();
</script>'))

## ---- current-plot-static
z <- STRIPS[["now"]]
par(mar = c(4, 1, 2.4, 1))
plot(z$dim1, z$jit, pch = 19, cex = 0.6,
     col = ifelse(z$party == "Democrat", paste0(DCOL, "73"), paste0(RCOL, "73")),
     yaxt = "n", ylab = "", xlim = c(-1, 1), ylim = c(-1.35, 1.35),
     xlab = "DW-NOMINATE dimension 1 (liberal to conservative)",
     main = paste0("The House, ", YRNOW), cex.main = 0.95)
abline(v = 0, lty = 3, col = "grey60")
legend("top", c("Democrat", "Republican"), pch = 19, col = c(DCOL, RCOL),
       horiz = TRUE, bty = "n", cex = 0.7)
mtext("vertical position is random, to separate the dots", side = 3, line = 0.1,
      adj = 1, cex = 0.62, col = "#666666")

## ---- sixties-d3
pan <- function(k, cg) {
  z <- STRIPS[[k]]
  paste0('{"t":"', cg, 'th Congress (', z$year1[1], ')","d":[',
         paste0("[", formatC(z$dim1, format = "f", digits = 3), ",",
                formatC(z$jit, format = "f", digits = 3), ",",
                ifelse(z$party == "Democrat", 0, 1), "]", collapse = ","), ']}')
}
cat(paste0('
<div id="st1" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const P=[', paste(pan("c90", 90), pan("c105", 105), pan("now", NOWH), sep = ","), '];
const W=770,H=310,M={t:22,r:24,b:44,l:24};
const svg=d3.select("#st1").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-1,1]).range([M.l,W-M.r]);
const rowh=(H-M.t-M.b)/P.length;
P.forEach(function(p,i){
  const top=M.t+i*rowh, mid=top+rowh*0.62, amp=rowh*0.30;
  svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",top+12).attr("text-anchor","middle")
    .attr("font-size","12px").attr("font-weight","600").text(p.t);
  svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",mid-amp).attr("y2",mid+amp)
    .attr("stroke","#999").attr("stroke-dasharray","3 3");
  svg.append("g").selectAll("circle").data(p.d).join("circle")
    .attr("cx",d=>x(d[0])).attr("cy",d=>mid+d[1]*amp).attr("r",2.4)
    .attr("fill",d=>d[2]===0?"#2166AC":"#B2182B").attr("opacity",0.45);
});
svg.append("g").attr("transform","translate(0,"+(H-M.b+6)+")").call(d3.axisBottom(x).ticks(9));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("DW-NOMINATE dimension 1 (liberal to conservative)");
})();
</script>'))

## ---- sixties-static
par(mfrow = c(3, 1), mar = c(3.2, 1, 2.2, 1))
for (k in c("c90", "c105", "now")) {
  z <- STRIPS[[k]]
  plot(z$dim1, z$jit, pch = 19, cex = 0.6,
       col = ifelse(z$party == "Democrat", paste0(DCOL, "73"), paste0(RCOL, "73")),
       yaxt = "n", ylab = "", xlab = "", xlim = c(-1, 1), ylim = c(-1.35, 1.35),
       main = paste0(z$congress[1], "th Congress (", z$year1[1], ")"),
       cex.main = 0.95)
  abline(v = 0, lty = 3, col = "grey60")
}
par(mfrow = c(1, 1))

## ---- gap-table
o <- H[H$congress %in% c(90, 100, 110, NOWH), c("congress", "year", "dem", "rep", "gap")]
o$dem <- nm(o$dem); o$rep <- nm(o$rep); o$gap <- nm(o$gap)
names(o) <- c("congress", "year", "median Democrat", "median Republican",
              "distance")
o

## ---- extremes
o <- rbind(head(H[order(-H$gap), c("congress", "year", "gap")], 3),
           head(H[order(H$gap),  c("congress", "year", "gap")], 3))
o$gap <- nm(o$gap)
names(o) <- c("congress", "year", "distance between party medians")
o

## ---- overlap
o <- H[H$congress %in% 103:110, c("congress", "year", "overlap")]
names(o) <- c("congress", "year", "Republicans left of the rightmost Democrat")
o

## ---- overlap-prep
OVCOL  <- "#4d4d4d"                    # gray means the middle, in both figures
LASTYR <- H$year[H$congress == LASTOVH]
PEAKOV <- H[which.max(H$overlap), ]
EARLY0 <- ZEROH[ZEROH < 1950]

## ---- overlap-d3
rows <- paste0("[", H$year, ",", H$overlap, "]", collapse = ",")
cat(paste0('
<div id="ovl" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[', rows, '].map(r=>({y:r[0],v:r[1]}));
const LAST=', LASTYR, ', LASTN=', H$overlap[H$congress == LASTOVH], ';
const PY=', PEAKOV$year, ', PV=', PEAKOV$overlap, ';
const W=770,H2=330,M={t:34,r:24,b:56,l:52};
const svg=d3.select("#ovl").append("svg").attr("viewBox","0 0 "+W+" "+H2)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.v)*1.18]).range([H2-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H2-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H2-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("members in the overlap");
svg.selectAll("line.n").data(D).join("line").attr("class","n")
  .attr("x1",d=>x(d.y)).attr("x2",d=>x(d.y)).attr("y1",y(0)).attr("y2",d=>y(d.v))
  .attr("stroke",d=>d.y===LAST?"#B2182B":"', OVCOL, '").attr("stroke-width",d=>d.y===LAST?3:2.2);
svg.append("circle").attr("cx",x(LAST)).attr("cy",y(LASTN)).attr("r",4).attr("fill","#B2182B");
svg.append("text").attr("x",x(LAST)-8).attr("y",y(LASTN)-8).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#B2182B").attr("font-weight","600")
  .text(LAST+": the last "+LASTN);
svg.append("text").attr("x",x(PY)).attr("y",y(PV)-8).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#444").text(PY+": "+PV);
svg.append("text").attr("x",M.l).attr("y",H2-16).attr("font-size","11px").attr("fill","#666")
  .text("Republicans sitting to the left of the most conservative Democrat, House, by Congress.");
svg.append("text").attr("x",M.l).attr("y",H2-2).attr("font-size","11px").attr("fill","#666")
  .text("A gap in the needles is a Congress with nobody in the middle: it happened before 1950 too.");
})();
</script>'))

## ---- overlap-static
par(mar = c(5.2, 4.2, 1.4, 1))
plot(H$year, H$overlap, type = "h", lwd = 2.2, col = OVCOL, xlab = "",
     ylab = "members in the overlap", ylim = c(0, max(H$overlap) * 1.18), las = 1)
segments(LASTYR, 0, LASTYR, H$overlap[H$congress == LASTOVH], col = "#B2182B",
         lwd = 3)
points(LASTYR, H$overlap[H$congress == LASTOVH], pch = 19, col = "#B2182B",
       cex = 0.8)
text(LASTYR - 3, H$overlap[H$congress == LASTOVH] + 3,
     paste0(LASTYR, ": the last ", H$overlap[H$congress == LASTOVH]), adj = 1,
     cex = 0.7, col = "#B2182B", font = 2)
text(PEAKOV$year, PEAKOV$overlap + 3, paste0(PEAKOV$year, ": ", PEAKOV$overlap),
     cex = 0.7, col = "#444444")
mtext(paste("Republicans sitting to the left of the most conservative Democrat,",
            "House, by Congress."), side = 1, line = 2.6, cex = 0.72,
      col = "#555555")
mtext(paste("A gap in the needles is a Congress with nobody in the middle:",
            "it happened before 1950 too."), side = 1, line = 3.6, cex = 0.72,
      col = "#555555")

## ---- asymmetry
data.frame(
  party = c("Democrats", "Republicans"),
  `median in that year` = c(nm(H$dem[H$congress == 90]), nm(H$rep[H$congress == 90])),
  `median now` = c(nm(H$dem[H$congress == NOWH]), nm(H$rep[H$congress == NOWH])),
  moved = c(nm(DMOVE), nm(RMOVE)),
  check.names = FALSE)

## ---- chambers
o <- data.frame(
  chamber = c("House", "Senate"),
  `gap in that year` = c(nm(g(H, 90)), nm(g(S, 90))),
  `gap now` = c(nm(g(H, NOWH)), nm(g(S, NOWS))),
  change = c(paste0("+", nm(g(H, NOWH) - g(H, 90))),
             paste0("+", nm(g(S, NOWS) - g(S, 90)))),
  check.names = FALSE)
o

## ---- senate-asym
data.frame(
  chamber = c("House", "Senate"),
  `Democrats moved` = c(nm(DMOVE),
                        nm(S$dem[S$congress == NOWS] - S$dem[S$congress == 90])),
  `Republicans moved` = c(nm(RMOVE),
                          nm(S$rep[S$congress == NOWS] - S$rep[S$congress == 90])),
  `last Congress with any overlap` = c(paste0(LASTOVH, "th"), paste0(LASTOVS, "th")),
  check.names = FALSE)

## ---- d3-gap
rh <- paste(sprintf('{"y":%d,"v":%.3f}', H$year, H$gap), collapse = ",")
rs <- paste(sprintf('{"y":%d,"v":%.3f}', S$year, S$gap), collapse = ",")
cat(sprintf('
<div id="dwn" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const H=[%s], S=[%s];
const W=760,Ht=390,M={t:22,r:24,b:40,l:56};
const svg=d3.select("#dwn").append("svg").attr("viewBox",`0 0 ${W} ${Ht}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(H,q=>q.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0.4,1.0]).range([Ht-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${Ht-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("x",M.l).attr("y",M.t-6).attr("font-size","11px")
  .attr("fill","#666").text("distance between the party medians");
const ln=d3.line().x(q=>x(q.y)).y(q=>y(q.v));
svg.append("path").datum(H).attr("fill","none").attr("stroke","#54278F")
  .attr("stroke-width",2.4).attr("d",ln);
svg.append("path").datum(S).attr("fill","none").attr("stroke","#E08214")
  .attr("stroke-width",2.4).attr("stroke-dasharray","5 3").attr("d",ln);
const tip=d3.select("#dwn").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const hit=svg.append("g");
H.forEach((q,i)=>{ const s=S[i];
  hit.append("rect").attr("x",x(q.y)-4).attr("y",M.t).attr("width",8)
    .attr("height",Ht-M.b-M.t).attr("fill","transparent")
    .on("mousemove",function(e){ tip.style("opacity",1)
      .html(`<b>${q.y}</b><br>House ${q.v.toFixed(3)}<br>Senate ${s?s.v.toFixed(3):"&mdash;"}`)
      .style("left",Math.min(e.offsetX+12,W-140)+"px").style("top",(e.offsetY-10)+"px"); })
    .on("mouseleave",()=>tip.style("opacity",0)); });
svg.append("line").attr("x1",W-230).attr("x2",W-206).attr("y1",M.t+10).attr("y2",M.t+10)
  .attr("stroke","#54278F").attr("stroke-width",2.4);
svg.append("text").attr("x",W-200).attr("y",M.t+14).attr("font-size","11px").text("House");
svg.append("line").attr("x1",W-230).attr("x2",W-206).attr("y1",M.t+28).attr("y2",M.t+28)
  .attr("stroke","#E08214").attr("stroke-width",2.4).attr("stroke-dasharray","5 3");
svg.append("text").attr("x",W-200).attr("y",M.t+32).attr("font-size","11px").text("Senate");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One chamber is redistricted every decade. The other has never been redistricted
in its history. Hover for values.</p>
', rh, rs))

## ---- gap-static
par(mar = c(3.5, 4.2, 1, 1))
plot(H$year, H$gap, type = "l", lwd = 2.4, col = "#54278F", ylim = c(0.4, 1),
     xlab = "", ylab = "distance between party medians")
lines(S$year, S$gap, lwd = 2.4, col = "#E08214", lty = 2)
legend("topleft", c("House", "Senate"), col = c("#54278F", "#E08214"),
       lwd = 2.4, lty = c(1, 2), bty = "n", cex = 0.9)

## ---- properties
data.frame(
  property = c("Relative to the agenda", "Estimated, not observed",
               "Votes, not beliefs", "Elites, not voters"),
  consequence = c(
    "A member who never changes can move if the votes held change",
    "Coordinates carry uncertainty this file does not report",
    "A private dissenter who votes with the party looks like a believer",
    "This describes 535 people, and nothing about the public"))

## ---- dim2
data.frame(year = d2y, `spread of dimension 2` = nm(d2), check.names = FALSE)

## ---- space-prep
CGA <- 80; CGB <- NOWH
sp  <- function(cg) {
  z <- nom[nom$chamber == "House" & nom$congress == cg, ]
  z[, c("dim1", "dim2", "party", "year")]
}
SA <- sp(CGA); SB <- sp(CGB)
SD2  <- c(sd(SA$dim2), sd(SB$dim2))
SD1  <- c(sd(SA$dim1), sd(SB$dim1))
YRA  <- SA$year[1]; YRB <- SB$year[1]
# does anybody in this file ever move?
hk    <- with(nom[nom$chamber == "House", ],
              paste(bioname, state, party))
NMEM  <- length(unique(hk))
NMOVE <- sum(tapply(nom$dim1[nom$chamber == "House"], hk,
                    function(v) length(unique(v))) > 1)

## ---- space-d3
pts <- function(z) paste0("[", formatC(z$dim1, format = "f", digits = 3), ",",
                          formatC(z$dim2, format = "f", digits = 3), ",",
                          ifelse(z$party == "Democrat", 0, 1), "]",
                          collapse = ",")
cat(paste0('
<div id="sp2" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const P=[[', pts(SA), '],[', pts(SB), ']];
const TT=["', YRA, ' (', CGA, 'th Congress)","', YRB, ' (', CGB, 'th Congress)"];
const SD=["', nm(SD2[1]), '","', nm(SD2[2]), '"];
const DC="', DCOL, '", RC="', RCOL, '";
const W=770,H=396,M={t:46,r:16,b:66,l:44},GAP=34;
const pw=(W-M.l-M.r-GAP)/2;
const svg=d3.select("#sp2").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([-1,1]).range([H-M.b,M.t]);
P.forEach(function(D,i){
  const x0=M.l+i*(pw+GAP);
  const x=d3.scaleLinear().domain([-1,1]).range([x0,x0+pw]);
  const g=svg.append("g");
  g.append("rect").attr("x",x0).attr("y",M.t).attr("width",pw).attr("height",H-M.b-M.t)
   .attr("fill","none").attr("stroke","#e2e2e2");
  g.append("line").attr("x1",x0).attr("x2",x0+pw).attr("y1",y(0)).attr("y2",y(0))
   .attr("stroke","#ccc").attr("stroke-dasharray","3 3");
  g.selectAll("circle").data(D).join("circle")
   .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",2.6)
   .attr("fill",d=>d[2]===0?DC:RC).attr("opacity",0.5);
  g.append("g").attr("transform","translate(0,"+(H-M.b)+")")
   .call(d3.axisBottom(x).ticks(5));
  g.append("text").attr("x",x0+pw/2).attr("y",M.t-18).attr("text-anchor","middle")
   .attr("font-size","12px").attr("font-weight","600").text(TT[i]);
  g.append("text").attr("x",x0+pw/2).attr("y",H-M.b+34).attr("text-anchor","middle")
   .attr("font-size","11px").attr("fill","#666").text("dimension 1 (left to right)");
  g.append("text").attr("x",x0+pw/2).attr("y",M.t-3).attr("text-anchor","middle")
   .attr("font-size","11px").attr("fill","#444").text("spread of dimension 2: "+SD[i]);
});
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",13)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("dimension 2");
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(5));
svg.append("circle").attr("cx",M.l+8).attr("cy",H-12).attr("r",4).attr("fill",DC);
svg.append("text").attr("x",M.l+18).attr("y",H-8).attr("font-size","11px").text("Democrat");
svg.append("circle").attr("cx",M.l+96).attr("cy",H-12).attr("r",4).attr("fill",RC);
svg.append("text").attr("x",M.l+106).attr("y",H-8).attr("font-size","11px").text("Republican");
svg.append("text").attr("x",M.l+190).attr("y",H-8).attr("font-size","11px").attr("fill","#666")
  .text("Both panels use the same axes, -1 to +1 on both dimensions.");
})();
</script>'))

## ---- space-static
par(mfrow = c(1, 2), mar = c(4.4, 3.4, 3.4, 0.8), oma = c(1.6, 1.4, 0, 0))
for (z in list(list(SA, YRA, CGA, SD2[1]), list(SB, YRB, CGB, SD2[2]))) {
  d <- z[[1]]
  plot(d$dim1, d$dim2, pch = 19, cex = 0.5, xlim = c(-1, 1), ylim = c(-1, 1),
       col = ifelse(d$party == "Democrat", paste0(DCOL, "80"), paste0(RCOL, "80")),
       xlab = "", ylab = "", las = 1, cex.axis = 0.75,
       main = paste0(z[[2]], " (", z[[3]], "th Congress)"), cex.main = 0.9)
  abline(h = 0, lty = 3, col = "grey70")
  mtext("dimension 1 (left to right)", 1, line = 2.2, cex = 0.72)
  mtext(paste("spread of dimension 2:", nm(z[[4]])), side = 3, line = 0.15,
        cex = 0.66, col = "#444444")
}
par(mfrow = c(1, 1))
mtext("dimension 2", side = 2, line = 0.2, cex = 0.8, outer = TRUE)
mtext(paste("Blue = Democrat, red = Republican. Both panels use the same axes,",
            "-1 to +1 on both dimensions."), side = 1, line = 0.2, cex = 0.7,
      col = "#555555", outer = TRUE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
