# officeholder-age-code.R -- chunk bodies for officeholder-age-brief.Rmd
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

D  <- "data"
m  <- read.csv(file.path(D, "derived/members.csv"),         stringsAsFactors = FALSE)
ac <- read.csv(file.path(D, "derived/age_by_congress.csv"), stringsAsFactors = FALSE)
le <- read.csv(file.path(D, "derived/life_expectancy.csv"), stringsAsFactors = FALSE)
et <- read.csv(file.path(D, "derived/entry_tenure.csv"),    stringsAsFactors = FALSE)
pr <- read.csv(file.path(D, "derived/presidents.csv"),      stringsAsFactors = FALSE)
ck <- read.csv(file.path(D, "derived/checks.csv"),          stringsAsFactors = FALSE)

# ---- formatting -------------------------------------------------------------
# R rounds half to even; JavaScript rounds half up. Every number that appears
# in both the D3 figure and the static one is formatted ONCE here, in R, and
# the string is what travels. Do not re-round anything on the JavaScript side.
f0 <- function(x) formatC(x, format = "f", digits = 0)
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
sgn <- function(x, k = 1) paste0(ifelse(x >= 0, "+", "-"),
                                 formatC(abs(x), format = "f", digits = k))
n  <- function(x) format(round(x), big.mark = ",")

# ---- the Congress sitting in a calendar year --------------------------------
# Congress n begins in 1787 + 2n, so it begins in an odd year and sits through
# the even year after it. The Congress sitting in year Y is therefore
# floor((Y - 1787)/2). Life-expectancy years are mostly even, congressional
# years always odd, so this is the join.
sitting <- function(y) floor((y - 1787) / 2)
val <- function(ch, y, col = "median_age")
  ac[[col]][ac$chamber == ch & ac$congress == sitting(y)]

BY <- 1950; EY <- 2018                 # the window where BOTH life-expectancy
                                       # series exist
CBY <- sitting(BY); CEY <- sitting(EY)

A0 <- val("Congress", BY); A1 <- val("Congress", EY); dA <- A1 - A0
E0a <- le$e0_hus[le$year == BY]; E0b <- le$e0_hus[le$year == EY]; dE0 <- E0b - E0a
E6a <- le$e65[le$year == BY];    E6b <- le$e65[le$year == EY];    dE6 <- E6b - E6a
REL <- 100 * dE6 / dE0                 # how much of the headline gain is even
                                       # about people who reached 65

# ---- the decomposition ------------------------------------------------------
dec <- function(ch, y1, y2) {
  a <- ac[ac$chamber == ch & ac$congress == sitting(y1), ]
  b <- ac[ac$chamber == ch & ac$congress == sitting(y2), ]
  c(total = b$mean_age - a$mean_age,
    entry = b$mean_entry_age - a$mean_entry_age,
    stay  = b$mean_years_since_entry - a$mean_years_since_entry)
}
DC <- dec("Congress", BY, EY); DH <- dec("House", BY, EY); DS <- dec("Senate", BY, EY)
NOW <- 2025                            # first year of the newest Congress here
CNOW <- sitting(NOW)

# ---- age distribution by era, for the ridgeline -----------------------------
# Computed once. Both the D3 figure and the base-R figure draw these exact
# numbers, so the two cannot drift apart.
EDGE <- c(1789, 1829, 1869, 1909, 1949, 1989, NOW + 2)
ELAB <- c("1789-1828", "1829-1868", "1869-1908", "1909-1948", "1949-1988",
          paste0("1989-", NOW))
hs <- m[m$chamber %in% c("House", "Senate") & !is.na(m$age), ]
hs$era <- cut(hs$year, EDGE, right = FALSE, labels = ELAB)
GX <- seq(25, 95, by = 0.5)
RID <- do.call(rbind, lapply(ELAB, function(e) {
  a <- hs$age[hs$era == e]
  d <- density(a, bw = 2.2, from = min(GX), to = max(GX), n = length(GX))
  data.frame(era = e, x = d$x, y = d$y, stringsAsFactors = FALSE)
}))
RMED <- vapply(ELAB, function(e) median(hs$age[hs$era == e]), numeric(1))
R70  <- vapply(ELAB, function(e) 100 * mean(hs$age[hs$era == e] >= 70), numeric(1))
RN   <- vapply(ELAB, function(e) sum(hs$era == e), numeric(1))

# ---- careers, for the scatter -----------------------------------------------
# Only careers that are over. A member still serving has an unfinished tenure,
# and plotting it as though it were finished would drag the recent era down.
CAR <- et[!et$still_serving & !is.na(et$entry_age), ]
CAR$grp <- ifelse(CAR$entry_year < 1900, "entered before 1900",
           ifelse(CAR$entry_year < 1981, "1900-1980", "1981 or later"))
GCOL <- c("entered before 1900" = "#a8b8c4", "1900-1980" = "#2c7fb8",
          "1981 or later"       = "#C41230")

# ---- how long ago was Congress last this young? -----------------------------
CC0    <- ac[ac$chamber == "Congress", ]
TR     <- CC0[CC0$year >= 1965 & CC0$year <= 1995, ]
TR     <- TR[which.min(TR$median_age), ]
PRIOR  <- max(CC0$year[CC0$year < TR$year & CC0$median_age <= TR$median_age])
OLDEST <- CC0$year[CC0$median_age == max(CC0$median_age)]

# ---- deaths in office -------------------------------------------------------
DIO <- !is.na(hs$died) & hs$died >= hs$year & hs$died <= hs$year + 1
hs$decade <- 10 * (hs$year %/% 10)

# ---- render every data.frame in this document as a TABLE --------------------
# A data.frame printed the ordinary way arrives as a "##"-prefixed code block,
# which reads as machinery rather than as a result. Registering knit_print for
# data.frame turns all of them into real tables in HTML and PDF alike. The
# envir argument is required: without it the registration silently fails.
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- whatfile
data.frame(
  fact = c("Rows in the file", "Congresses covered",
           "Chambers in it", "Presidents in it",
           "Rows with no birth year", "What `born` contains"),
  value = c(ck$value[ck$check == "Voteview rows"],
            paste0("1 to ", max(m$congress), "  (1789 to ", max(m$year), ")"),
            "House, Senate, President",
            paste0(ck$value[ck$check == "presidents in the file"], " people, ",
                   ck$value[ck$check == "separate presidencies (Cleveland and Trump twice)"],
                   " presidencies"),
            ck$value[ck$check == "member-Congress rows with no birth year"],
            "a year. Not a date."),
  check.names = FALSE)

## ---- biden
b <- pr[pr$bioname == "BIDEN, Joseph Robinette (Joe), Jr.", ]
# The last column is the only number in this document that does not come out of
# the data files. It comes from his date of birth, 20 November 1942, which the
# Voteview file does not carry -- and that is the point being made.
data.frame(president = "Biden", born = b$born, term_began = b$year,
           age_by_year_arithmetic = b$age_at_term_start,
           actual_age_on_inauguration_day = 78)

## ---- cleveland
ck[ck$check %in% c("people with a birth year on some rows and not others",
                   "who that is",
                   "people carrying two different birth years"), ]

## ---- rawvv
# A verbatim capture of the first four lines of the download. It is quoted here
# rather than re-fetched at knit time, so this block does not need the network;
# every count in the paragraphs below is taken FROM the capture, not asserted
# about it.
RAW <- c(
"congress,chamber,icpsr,state_icpsr,district_code,state_abbrev,party_code,occupancy,last_means,bioname,bioguide_id,born,died,nominate_dim1,nominate_dim2,nominate_log_likelihood,nominate_geo_mean_probability,nominate_number_of_votes,nominate_number_of_errors,conditional,nokken_poole_dim1,nokken_poole_dim2",
"1,President,99869,99,0,USA,5000,,,\"WASHINGTON, George\",,,,,,,,,,,,",
"1,House,4766,1,98,CT,5000,,,\"HUNTINGTON, Benjamin\",H000995,1736.0,1800.0,,,,,,,,,",
"1,House,8457,1,98,CT,5000,,,\"SHERMAN, Roger\",S000349,1721.0,1793.0,0.589,0.307,,,,,,,")

# Split on commas that are OUTSIDE quotes -- the same rule a CSV parser uses,
# and the reason `bioname` is not torn in half by the folding below.
csplit <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  q <- cumsum(ch == "\"") %% 2 == 1
  cut <- which(ch == "," & !q)
  st <- c(1L, cut + 1L); en <- c(cut - 1L, length(ch))
  vapply(seq_along(st),
         function(i) if (st[i] > en[i]) "" else
           paste(ch[st[i]:en[i]], collapse = ""),
         character(1))
}
fold <- function(s, w = 72) {
  p <- csplit(s)
  p[-length(p)] <- paste0(p[-length(p)], ",")
  out <- character(0); cur <- ""
  for (z in p) {
    if (nzchar(cur) && nchar(cur) + nchar(z) > w) { out <- c(out, cur); cur <- z }
    else cur <- paste0(cur, z)
  }
  c(out, cur)
}
HDR <- csplit(RAW[1])
NCOL_RAW <- length(HDR)
# the scaling block, counted as a span: from the first nominate_ column to the
# last, `conditional` included, because it belongs to the same machinery
NOM <- diff(range(grep("nominate|nokken|conditional", HDR))) + 1L
# One column per row of the table, three captured records beside it. Twenty-two
# columns read down a page; folded across one they do not. The descriptions are
# Voteview's own, and match the ones the dw-nominate chapter prints for the
# same file.
.cells <- lapply(RAW, csplit)
.k <- min(lengths(.cells))
.vv <- c(
  congress = "the numbered two-year Congress this row describes",
  chamber = "House, Senate or President",
  icpsr = "the member's permanent ID, kept across every Congress they serve",
  state_icpsr = "ICPSR's own state code, not the Census FIPS code",
  district_code = "district number; 0 for senators and at-large members",
  state_abbrev = "two-letter postal abbreviation",
  party_code = "ICPSR party code: 100 Democrat, 200 Republican",
  occupancy = "which occupant of the seat this is in this Congress",
  last_means = "how the member reached office",
  bioname = "name, as last, first middle",
  bioguide_id = "Biographical Directory ID — the join key to other sources",
  born = "year of birth — the column this chapter is built on",
  died = "year of death, empty for the living",
  nominate_dim1 = "DW-NOMINATE first dimension",
  nominate_dim2 = "DW-NOMINATE second dimension",
  nominate_log_likelihood = "how well the scaling fits this member's votes",
  nominate_geo_mean_probability = "average probability the model gave the votes cast",
  nominate_number_of_votes = "votes the score was estimated from",
  nominate_number_of_errors = "votes the model classifies wrongly",
  conditional = "documented, and empty on every row",
  nokken_poole_dim1 = "dimension 1, re-estimated within each Congress",
  nokken_poole_dim2 = "the same, for dimension 2")
.o <- data.frame(Column = HDR[seq_len(.k)],
                 What_it_is = unname(.vv[HDR[seq_len(.k)]]),
                 stringsAsFactors = FALSE)
for (i in 2:length(.cells))
  .o[[paste0("Record_", i - 1L)]] <-
    ifelse(nzchar(.cells[[i]][seq_len(.k)]), .cells[[i]][seq_len(.k)],
           "(empty)")
.o

## ---- cleanrow
m[m$congress == 1 &
  m$bioname %in% c("WASHINGTON, George", "HUNTINGTON, Benjamin", "SHERMAN, Roger"),
  c("congress", "year", "chamber", "icpsr", "bioname", "born", "age",
    "entry_year", "years_since_entry")]

## ---- cleanagg
ac[ac$congress == 1, c("congress", "year", "chamber", "n", "median_age",
                       "mean_age", "mean_entry_age", "mean_years_since_entry")]

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The median and quartiles of each chamber at every Congress, plus one dot per
# president. Both renderers read age_by_congress.csv and presidents.csv; the
# numbers are formatted in R and travel as strings.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
H <- ac[ac$chamber == "House",   ]; S <- ac[ac$chamber == "Senate", ]
P <- pr[!is.na(pr$age_at_term_start), ]
# Two decimals, not one: chamber quartiles land on .25 and .75, and rounding
# them to a single decimal would make the interactive ribbon disagree with the
# printed one in the third significant figure.
ser <- function(d) paste(sprintf('[%d,%s,%s,%s]', d$year, f2(d$median_age),
                                 f2(d$p25_age), f2(d$p75_age)), collapse = ",")
cat(sprintf('
<div id="f1" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const H=[%s],S=[%s],P=[%s];
const W=760,Hh=430,M={t:26,r:120,b:44,l:46};
const svg=d3.select("#f1").append("svg").attr("viewBox",`0 0 ${W} ${Hh}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1787,%d]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([30,82]).range([Hh-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${Hh-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(Hh)/2).attr("y",13)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#555")
  .text("age, years");
const area=d3.area().x(d=>x(d[0])).y0(d=>y(d[2])).y1(d=>y(d[3]));
const line=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
[[H,"#2c7fb8","House"],[S,"#b3651a","Senate"]].forEach(([d,c,lab])=>{
  svg.append("path").datum(d).attr("d",area).attr("fill",c).attr("fill-opacity",0.13);
  svg.append("path").datum(d).attr("d",line).attr("fill","none")
    .attr("stroke",c).attr("stroke-width",1.9);
  const last=d[d.length-1];
  svg.append("text").attr("x",x(last[0])+6).attr("y",y(last[1])+4)
    .attr("font-size","12px").attr("font-weight","600").attr("fill",c).text(lab);
});
svg.selectAll("circle.p").data(P).join("circle").attr("class","p")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",3.1)
  .attr("fill","#4d9221").attr("fill-opacity",0.85);
svg.append("text").attr("x",W-M.r+6).attr("y",y(%s)-6).attr("font-size","12px")
  .attr("font-weight","600").attr("fill","#4d9221").text("Presidents");
const tip=d3.select("#f1").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.selectAll("circle.p").on("mousemove",function(e,d){
  tip.style("opacity",1).html(`<b>${d[2]}</b><br>${d[0]}, aged about ${d[1]}`)
   .style("left",Math.min(e.offsetX+14,W-190)+"px").style("top",(e.offsetY-10)+"px");
 }).on("mouseleave",()=>tip.style("opacity",0));
svg.append("text").attr("x",M.l).attr("y",14).attr("font-size","12px")
  .attr("fill","#555").text("shaded band: middle half of the chamber");
})();
</script>
', ser(H), ser(S),
   paste(sprintf('[%d,%s,"%s"]', P$year, f0(P$age_at_term_start),
                 gsub('"', "", sub(",.*$", "", P$bioname))), collapse = ","),
   NOW + 2, f0(P$age_at_term_start[nrow(P)])))

## ---- fig1-static
H <- ac[ac$chamber == "House", ]; S <- ac[ac$chamber == "Senate", ]
P <- pr[!is.na(pr$age_at_term_start), ]
par(mar = c(3.4, 3.6, 1.4, 5.6))
plot(NA, xlim = c(1787, NOW + 2), ylim = c(30, 82), xlab = "", ylab = "",
     xaxs = "i", las = 1, cex.axis = 0.8)
mtext("age, years", 2, line = 2.4, cex = 0.85)
for (z in list(list(H, "#2c7fb8", "House"), list(S, "#b3651a", "Senate"))) {
  d <- z[[1]]; cc <- z[[2]]
  polygon(c(d$year, rev(d$year)), c(d$p25_age, rev(d$p75_age)),
          col = adjustcolor(cc, 0.13), border = NA)
  lines(d$year, d$median_age, col = cc, lwd = 1.9)
  text(max(d$year) + 3, d$median_age[nrow(d)], z[[3]], col = cc, font = 2,
       cex = 0.8, adj = 0, xpd = NA)
}
points(P$year, P$age_at_term_start, pch = 19, cex = 0.6,
       col = adjustcolor("#4d9221", 0.85))
text(NOW + 5, P$age_at_term_start[nrow(P)] + 3, "Presidents", col = "#4d9221",
     font = 2, cex = 0.8, adj = 0, xpd = NA)
mtext("shaded band: middle half of the chamber", 3, line = 0.1, adj = 0,
      cex = 0.75, col = "#555")

## ---- step2-tab
C <- ac[ac$chamber == "Congress", ]
lo <- C[which.min(C$median_age), ]; hi <- C[which.max(C$median_age), ]
trough <- C[C$year >= 1965 & C$year <= 1995, ]
tr <- trough[which.min(trough$median_age), ]
data.frame(
  quantity = c("Youngest Congress on record (median age)",
               "Oldest Congresses on record (median age)",
               "Median age, most recent Congress",
               "Post-war low point",
               "Median senator now", "Median representative now"),
  value = c(paste0(f0(lo$median_age), "  (", lo$year, ")"),
            paste0(f0(hi$median_age), "  (", paste(OLDEST, collapse = ", "), ")"),
            paste0(f0(val("Congress", NOW)), "  (", NOW, ")"),
            paste0(f0(tr$median_age), "  (", tr$year, ")"),
            f0(val("Senate", NOW)), f0(val("House", NOW))))

## ---- step4-tab
data.frame(
  measure = c("Life expectancy at birth", "Remaining life expectancy at 65",
              "Remaining life expectancy at 75",
              "Median age of Congress"),
  `1950` = c(f1(E0a), f1(E6a), "--", f0(A0)),
  `2018` = c(f1(E0b), f1(E6b), f1(le$e75[le$year == EY]), f0(A1)),
  change = c(sgn(dE0), sgn(dE6),
             sgn(le$e75[le$year == EY] - le$e75[le$year == 1980]),
             sgn(dA, 0)),
  check.names = FALSE)

## ---- fig2-d3
BARS <- data.frame(
  lab = c("Life expectancy AT BIRTH", "Median age of CONGRESS",
          "Remaining life expectancy AT 65"),
  v   = c(dE0, dA, dE6),
  col = c("#8c8c8c", "#C41230", "#2c7fb8"), stringsAsFactors = FALSE)
BARS$s <- f1(BARS$v)
cat(sprintf('
<div id="f2" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const B=[%s], CONG=%s;
const W=760,H=252,M={t:44,r:120,b:40,l:250};
const svg=d3.select("#f2").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,12]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(B.map(d=>d.lab)).range([M.t,H-M.b]).padding(0.34);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#555").text("gain between 1950 and 2018, in years");
svg.append("text").attr("x",8).attr("y",20).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Two yardsticks for the same 68 years");
svg.selectAll("rect").data(B).join("rect")
  .attr("x",x(0)).attr("y",d=>y(d.lab)).attr("height",y.bandwidth())
  .attr("width",d=>x(d.v)-x(0)).attr("fill",d=>d.col).attr("fill-opacity",0.88);
svg.selectAll("text.v").data(B).join("text").attr("class","v")
  .attr("x",x(d3.max(B,d=>d.v))+9).attr("y",d=>y(d.lab)+y.bandwidth()/2+4)
  .attr("font-size","12.5px").attr("font-weight","600").attr("fill",d=>d.col)
  .text(d=>"+"+d.s+" yrs");
svg.selectAll("text.l").data(B).join("text").attr("class","l")
  .attr("x",M.l-10).attr("y",d=>y(d.lab)+y.bandwidth()/2+4).attr("text-anchor","end")
  .attr("font-size","12px").attr("fill","#333").text(d=>d.lab);
svg.append("line").attr("x1",x(CONG)).attr("x2",x(CONG)).attr("y1",M.t-8)
  .attr("y2",H-M.b).attr("stroke","#C41230").attr("stroke-width",1.2)
  .attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(CONG)).attr("y",M.t-13).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230").text("Congress");
})();
</script>
', paste(sprintf('{"lab":"%s","v":%s,"col":"%s","s":"%s"}',
                 BARS$lab, f1(BARS$v), BARS$col, BARS$s), collapse = ","), f1(dA)))

## ---- fig2-static
BARS <- data.frame(
  lab = c("Remaining life expectancy AT 65", "Median age of CONGRESS",
          "Life expectancy AT BIRTH"),
  v   = c(dE6, dA, dE0),
  col = c("#2c7fb8", "#C41230", "#8c8c8c"), stringsAsFactors = FALSE)
par(mar = c(3.4, 15.2, 2.0, 3.2))
bp <- barplot(BARS$v, horiz = TRUE, col = adjustcolor(BARS$col, 0.88),
              border = NA, xlim = c(0, 12), names.arg = BARS$lab, las = 1,
              cex.names = 0.72, cex.axis = 0.8, space = 0.55)
abline(v = dA, lty = 2, col = "#C41230")
text(max(BARS$v) + 0.3, bp, paste0("+", f1(BARS$v), " yrs"), adj = 0,
     cex = 0.75, font = 2, col = BARS$col, xpd = NA)
mtext("gain between 1950 and 2018, in years", 1, line = 2.2, cex = 0.8)
mtext("Two yardsticks for the same 68 years", 3, line = 0.4, adj = 0,
      cex = 0.9, font = 2)

## ---- step4-share
data.frame(
  quantity = c("Gain in life expectancy at birth, 1950-2018",
               "Gain in remaining life expectancy at 65, 1950-2018",
               "Share of the headline gain that is about life after 65"),
  value = c(paste0(f1(dE0), " years"), paste0(f1(dE6), " years"),
            paste0(f0(REL), "%")))

## ---- step4-checks
ck[ck$check %in% c("years where HUS and CDC both give e(0)",
                   "of those, years where the two agree exactly",
                   "years where they disagree",
                   "largest HUS-vs-CDC disagreement on e(0), years"), ]

## ---- fig3-d3
CC <- ac[ac$chamber == "Congress", ]
rows <- paste(sprintf('[%d,%s,%s]', CC$year, f2(CC$mean_entry_age),
                      f2(CC$mean_years_since_entry)), collapse = ",")
cat(sprintf('
<div id="f3" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[%s];
const W=760,H=400,M={t:30,r:150,b:42,l:46};
const svg=d3.select("#f3").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1787,%d]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,66]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(8));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-H/2).attr("y",13)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#555")
  .text("mean age, years");
const a1=d3.area().x(d=>x(d[0])).y0(y(0)).y1(d=>y(d[1]));
const a2=d3.area().x(d=>x(d[0])).y0(d=>y(d[1])).y1(d=>y(d[1]+d[2]));
svg.append("path").datum(D).attr("d",a1).attr("fill","#2c7fb8").attr("fill-opacity",0.75);
svg.append("path").datum(D).attr("d",a2).attr("fill","#e08214").attr("fill-opacity",0.85);
svg.append("path").datum(D).attr("d",d3.line().x(d=>x(d[0])).y(d=>y(d[1]+d[2])))
  .attr("fill","none").attr("stroke","#333").attr("stroke-width",1.3);
const L=D[D.length-1];
svg.append("text").attr("x",x(L[0])+8).attr("y",y(L[1]+L[2]/2)+4)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#a4590f")
  .text("years since arriving");
svg.append("text").attr("x",x(L[0])+8).attr("y",y(L[1]/2)+4)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#1d5c88")
  .text("age on arriving");
svg.append("text").attr("x",x(L[0])+8).attr("y",y(L[1]+L[2])-6)
  .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#333")
  .text("mean age");
svg.append("text").attr("x",M.l).attr("y",16).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("Every Congress, split into the two things that make it old");
})();
</script>
', rows, NOW + 2))

## ---- fig3-static
CC <- ac[ac$chamber == "Congress", ]
par(mar = c(3.4, 3.6, 1.8, 7.4))
plot(NA, xlim = c(1787, NOW + 2), ylim = c(0, 66), xlab = "", ylab = "",
     xaxs = "i", yaxs = "i", las = 1, cex.axis = 0.8)
mtext("mean age, years", 2, line = 2.4, cex = 0.85)
polygon(c(CC$year, rev(CC$year)),
        c(CC$mean_entry_age, rep(0, nrow(CC))),
        col = adjustcolor("#2c7fb8", 0.75), border = NA)
polygon(c(CC$year, rev(CC$year)),
        c(CC$mean_entry_age + CC$mean_years_since_entry, rev(CC$mean_entry_age)),
        col = adjustcolor("#e08214", 0.85), border = NA)
lines(CC$year, CC$mean_age, col = "#333333", lwd = 1.3)
L <- CC[nrow(CC), ]
text(NOW + 4, L$mean_entry_age + L$mean_years_since_entry / 2,
     "years since\narriving", col = "#a4590f", font = 2, cex = 0.7, adj = 0, xpd = NA)
text(NOW + 4, L$mean_entry_age / 2, "age on\narriving", col = "#1d5c88",
     font = 2, cex = 0.7, adj = 0, xpd = NA)
mtext("Every Congress, split into the two things that make it old", 3,
      line = 0.4, adj = 0, cex = 0.9, font = 2)

## ---- step5-tab
data.frame(
  chamber = c("Congress", "House", "Senate"),
  `rise in mean age` = c(sgn(DC["total"], 2), sgn(DH["total"], 2), sgn(DS["total"], 2)),
  `from entering older` = c(sgn(DC["entry"], 2), sgn(DH["entry"], 2), sgn(DS["entry"], 2)),
  `from staying longer` = c(sgn(DC["stay"], 2), sgn(DH["stay"], 2), sgn(DS["stay"], 2)),
  `entering older, share` = c(paste0(f0(100 * DC["entry"] / DC["total"]), "%"),
                              paste0(f0(100 * DH["entry"] / DH["total"]), "%"),
                              paste0(f0(100 * DS["entry"] / DS["total"]), "%")),
  check.names = FALSE)

## ---- step5-dip
data.frame(
  period = c("1950 to 1980", "1980 to 2025"),
  `change in mean age`   = c(sgn(dec("Congress", 1950, 1980)["total"], 2),
                             sgn(dec("Congress", 1980, NOW)["total"], 2)),
  `change in arrival age`= c(sgn(dec("Congress", 1950, 1980)["entry"], 2),
                             sgn(dec("Congress", 1980, NOW)["entry"], 2)),
  `change in years since arriving` = c(sgn(dec("Congress", 1950, 1980)["stay"], 2),
                                       sgn(dec("Congress", 1980, NOW)["stay"], 2)),
  check.names = FALSE)

## ---- fig4-d3
RH <- 62; RGAP <- 46
ymax <- max(RID$y)
J <- vapply(ELAB, function(e) {
  z <- RID[RID$era == e, ]
  # Every displayed number is passed as a STRING. Handed over as a JSON number,
  # a median of 43.0 arrives in JavaScript as 43 and the D3 figure quietly
  # disagrees with the static one.
  sprintf('{"e":"%s","med":"%s","p70":"%s","n":"%s","y":[%s]}', e, f1(RMED[e]),
          f1(R70[e]), n(RN[e]), paste(sprintf("%.4f", z$y / ymax), collapse = ","))
}, character(1))
cat(sprintf('
<div id="f4" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const E=[%s], GX0=%s, GX1=%s, NG=%d;
const W=760,H=%d,M={t:30,r:150,b:40,l:96};
const svg=d3.select("#f4").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([GX0,GX1]).range([M.l,W-M.r]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#555").text("age, years");
svg.append("line").attr("x1",x(70)).attr("x2",x(70)).attr("y1",M.t-6)
  .attr("y2",H-M.b).attr("stroke","#C41230").attr("stroke-width",1)
  .attr("stroke-dasharray","4,3");
svg.append("text").attr("x",x(70)).attr("y",M.t-11).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#C41230").text("age 70");
const col=d3.scaleSequential().domain([0,E.length-1]).interpolator(d3.interpolatePuBu);
E.forEach((d,i)=>{
  const base=M.t+i*%d+%d;
  const pts=d.y.map((v,j)=>[x(GX0+j*(GX1-GX0)/(NG-1)),base-v*%d]);
  const path="M"+pts.map(p=>p[0].toFixed(1)+","+p[1].toFixed(1)).join("L")+
    `L${x(GX1).toFixed(1)},${base}L${x(GX0).toFixed(1)},${base}Z`;
  svg.append("path").attr("d",path).attr("fill",col(i)).attr("fill-opacity",0.85)
    .attr("stroke","#555").attr("stroke-width",0.7);
  svg.append("text").attr("x",M.l-10).attr("y",base-3).attr("text-anchor","end")
    .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#333").text(d.e);
  svg.append("text").attr("x",W-M.r+8).attr("y",base-3).attr("font-size","11px")
    .attr("fill","#444").text(`median ${d.med} \\u00b7 ${d.p70}%% over 70`);
});
svg.append("text").attr("x",8).attr("y",16).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#333")
  .text("The whole chamber, not the middle of it");
})();
</script>
', paste(J, collapse = ","), f0(min(GX)), f0(max(GX)), length(GX),
   30 + length(ELAB) * RGAP + 52, RGAP, 44, RH))

## ---- fig4-static
ymax <- max(RID$y)
par(mar = c(3.4, 6.6, 1.8, 6.4))
plot(NA, xlim = range(GX), ylim = c(0, length(ELAB) + 0.9), xlab = "", ylab = "",
     axes = FALSE)
axis(1, cex.axis = 0.8); mtext("age, years", 1, line = 2.2, cex = 0.85)
abline(v = 70, lty = 2, col = "#C41230")
text(70, length(ELAB) + 0.8, "age 70", col = "#C41230", cex = 0.7)
ramp <- colorRampPalette(c("#ece7f2", "#3690c0"))(length(ELAB))
for (i in seq_along(ELAB)) {
  e <- ELAB[i]; z <- RID[RID$era == e, ]
  base <- length(ELAB) - i + 0.1
  polygon(c(z$x, max(GX), min(GX)), c(base + 1.25 * z$y / ymax, base, base),
          col = ramp[i], border = "#555555", lwd = 0.6)
  text(min(GX) - 1.5, base + 0.16, e, adj = 1, cex = 0.72, font = 2, xpd = NA)
  text(max(GX) + 1.5, base + 0.16,
       paste0("median ", f1(RMED[e]), "\n", f1(R70[e]), "% over 70"),
       adj = 0, cex = 0.6, col = "#444444", xpd = NA)
}
mtext("The whole chamber, not the middle of it", 3, line = 0.4, adj = 0,
      cex = 0.9, font = 2)

## ---- step6-tab
C <- ac[ac$chamber == "Congress", ]
data.frame(
  quantity = c("Share of Congress aged 65 or over",
               "Share of Congress aged 70 or over",
               "Share of Congress aged 80 or over",
               "Share of the SENATE aged 70 or over"),
  `1950` = c(paste0(f1(val("Congress", BY, "pct_65plus")), "%"),
             paste0(f1(val("Congress", BY, "pct_70plus")), "%"),
             paste0(f1(val("Congress", BY, "pct_80plus")), "%"),
             paste0(f1(val("Senate", BY, "pct_70plus")), "%")),
  `2025` = c(paste0(f1(val("Congress", NOW, "pct_65plus")), "%"),
             paste0(f1(val("Congress", NOW, "pct_70plus")), "%"),
             paste0(f1(val("Congress", NOW, "pct_80plus")), "%"),
             paste0(f1(val("Senate", NOW, "pct_70plus")), "%")),
  check.names = FALSE)

## ---- step6-edges
q <- function(y, p) unname(quantile(hs$age[hs$congress == sitting(y)], p))
QQ <- c(0.05, 0.5, 0.95)
data.frame(
  Congress = c(f0(sitting(BY)), f0(sitting(NOW)), ""),
  year     = c(f0(BY), f0(NOW), "moved by"),
  `5th percentile`  = c(f0(q(BY, QQ[1])), f0(q(NOW, QQ[1])),
                        sgn(q(NOW, QQ[1]) - q(BY, QQ[1]), 0)),
  median            = c(f0(q(BY, QQ[2])), f0(q(NOW, QQ[2])),
                        sgn(q(NOW, QQ[2]) - q(BY, QQ[2]), 0)),
  `95th percentile` = c(f0(q(BY, QQ[3])), f0(q(NOW, QQ[3])),
                        sgn(q(NOW, QQ[3]) - q(BY, QQ[3]), 0)),
  check.names = FALSE)

## ---- step7-tenure
E2 <- et[!et$still_serving & !is.na(et$entry_age) & et$entry_year <= 1985, ]
E2$cohort <- cut(E2$entry_year, c(1789, 1849, 1909, 1949, 1986),
                 right = FALSE,
                 labels = c("1789-1848", "1849-1908", "1909-1948", "1949-1985"))
tt <- do.call(rbind, lapply(levels(E2$cohort), function(g) {
  z <- E2[E2$cohort == g, ]
  data.frame(`entered in` = g, careers = n(nrow(z)),
             `median age on arrival` = f0(median(z$entry_age)),
             `median years served` = f0(median(z$years_served)),
             `served 20 years or more` =
               paste0(f1(100 * mean(z$years_served >= 20)), "%"),
             check.names = FALSE)
}))
tt

## ---- fig5-d3
set.seed(84355)
S5 <- CAR[sample(nrow(CAR), min(2200, nrow(CAR))), ]
S5$jx <- S5$entry_age + runif(nrow(S5), -0.42, 0.42)
S5$jy <- S5$years_served + runif(nrow(S5), -0.7, 0.7)
med <- do.call(rbind, lapply(names(GCOL), function(g) {
  z <- CAR[CAR$grp == g, ]
  data.frame(g = g, ea = median(z$entry_age), ys = median(z$years_served))
}))
cat(sprintf('
<div id="f5" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const P=[%s], MED=[%s];
const W=760,H=430,M={t:26,r:20,b:46,l:56};
const svg=d3.select("#f5").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([24,84]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,56]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(7));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("fill","#555").text("age on first arriving");
svg.append("text").attr("transform","rotate(-90)").attr("x",-H/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#555")
  .text("years served in that chamber");
svg.selectAll("circle.d").data(P).join("circle").attr("class","d")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",2.4)
  .attr("fill",d=>d[2]).attr("fill-opacity",0.42);
MED.forEach(d=>{
  svg.append("circle").attr("cx",x(d.ea)).attr("cy",y(d.ys)).attr("r",7)
    .attr("fill",d.c).attr("stroke","#fff").attr("stroke-width",2);
});
const lg=svg.append("g").attr("transform",`translate(${W-M.r-190},${M.t+2})`);
MED.forEach((d,i)=>{
  lg.append("circle").attr("cy",i*17).attr("r",4.5).attr("fill",d.c);
  lg.append("text").attr("x",11).attr("y",i*17+4).attr("font-size","11px")
    .text(`${d.g} (median ${d.mea}, ${d.mys} yrs)`);
});
svg.append("text").attr("x",M.l).attr("y",14).attr("font-size","12px")
  .attr("fill","#555").text("completed careers only; large dots are the group medians");
})();
</script>
', paste(sprintf('[%.2f,%.2f,"%s"]', S5$jx, S5$jy, GCOL[S5$grp]), collapse = ","),
   paste(sprintf('{"g":"%s","ea":%s,"ys":%s,"c":"%s","mea":"%s","mys":"%s"}',
                 med$g, f1(med$ea), f1(med$ys), GCOL[med$g], f0(med$ea),
                 f0(med$ys)), collapse = ",")))

## ---- fig5-static
set.seed(84355)
S5 <- CAR[sample(nrow(CAR), min(2200, nrow(CAR))), ]
S5$jx <- S5$entry_age + runif(nrow(S5), -0.42, 0.42)
S5$jy <- S5$years_served + runif(nrow(S5), -0.7, 0.7)
med <- do.call(rbind, lapply(names(GCOL), function(g) {
  z <- CAR[CAR$grp == g, ]
  data.frame(g = g, ea = median(z$entry_age), ys = median(z$years_served),
             stringsAsFactors = FALSE)
}))
par(mar = c(3.6, 3.8, 1.6, 1.0))
plot(S5$jx, S5$jy, pch = 19, cex = 0.35, xlim = c(24, 84), ylim = c(0, 56),
     col = adjustcolor(unname(GCOL[S5$grp]), 0.42), xlab = "", ylab = "",
     las = 1, cex.axis = 0.8)
mtext("age on first arriving", 1, line = 2.3, cex = 0.85)
mtext("years served in that chamber", 2, line = 2.5, cex = 0.85)
points(med$ea, med$ys, pch = 21, bg = unname(GCOL[med$g]), col = "white",
       cex = 1.7, lwd = 2)
legend("topright", paste0(med$g, " (median ", f0(med$ea), ", ", f0(med$ys), " yrs)"),
       pch = 19, col = unname(GCOL[med$g]), bty = "n", cex = 0.7)
mtext("completed careers only; large dots are the group medians", 3, line = 0.2,
      adj = 0, cex = 0.72, col = "#555555")

## ---- step7-pipeline
S <- et[et$chamber == "Senate" & !is.na(et$entry_age), ]
data.frame(
  quantity = c("Senators who sat in the House first, all time",
               "Senators who sat in the House first, entering since 1981",
               "Mean Senate arrival age since 1981, WITH prior House service",
               "Mean Senate arrival age since 1981, WITHOUT it"),
  value = c(paste0(f1(100 * mean(S$prior_house)), "%"),
            paste0(f1(100 * mean(S$prior_house[S$entry_year >= 1981])), "%"),
            f1(mean(S$entry_age[S$entry_year >= 1981 & S$prior_house])),
            f1(mean(S$entry_age[S$entry_year >= 1981 & !S$prior_house]))))

## ---- step8-deaths
dec_tab <- data.frame(
  era = c("1900s-1910s", "1920s-1930s", "1960s-1970s", "2010s-2020s"),
  `member-terms ending in death in office` = c(
    paste0(f2(100 * mean(DIO[hs$decade %in% c(1900, 1910)])), "%"),
    paste0(f2(100 * mean(DIO[hs$decade %in% c(1920, 1930)])), "%"),
    paste0(f2(100 * mean(DIO[hs$decade %in% c(1960, 1970)])), "%"),
    paste0(f2(100 * mean(DIO[hs$decade %in% c(2010, 2020)])), "%")),
  check.names = FALSE)
dec_tab

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
