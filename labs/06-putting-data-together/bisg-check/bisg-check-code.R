# bisg-check-code.R -- chunk bodies for bisg-check-brief.Rmd
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

v0 <- read.csv("data/derived/houston_voters.csv", colClasses = c(GEOID20 = "character"))
g  <- read.csv("data/derived/houston_blocks.csv",  colClasses = c(GEOID20 = "character"))
sn <- read.csv("../../01-census-bureau/surnames/data/derived/census_surnames.csv",
               stringsAsFactors = FALSE)

R  <- c("white", "black", "hispanic", "asian", "aian")
PR <- c("white", "Black", "Hispanic", "Asian", "Am. Indian")
sc <- c(white = "pctwhite", black = "pctblack", hispanic = "pcthispanic",
        asian = "pctapi",   aian  = "pctaian")

i        <- match(v0$surname, sn$name)
matched  <- !is.na(i)
p_match  <- 100 * mean(matched)
drop     <- v0[!matched, ]
mix_all  <- 100 * prop.table(table(factor(v0$race,   levels = R)))
mix_drop <- 100 * prop.table(table(factor(drop$race, levels = R)))

v <- v0[matched, ]
S <- as.matrix(sn[i[matched], sc]); S[is.na(S)] <- 0
keep <- rowSums(S) > 0; v <- v[keep, ]; S <- S[keep, , drop = FALSE]
S <- S / rowSums(S)                                     # P(race | surname)

P <- as.matrix(g[, R]); rownames(P) <- g$GEOID20
M <- sweep(P, 2, colSums(P), "/")[v$GEOID20, , drop = FALSE]
empty <- rowSums(M) == 0
M[empty, ] <- matrix(colSums(P) / sum(P), sum(empty), 5, byrow = TRUE)
post <- S * M
degen <- rowSums(post) == 0
post[degen, ] <- S[degen, ]
post <- post / rowSums(post)

sur <- R[max.col(S,    ties.method = "first")]
bis <- R[max.col(post, ties.method = "first")]

acc <- function(pred) sapply(R, function(r) 100 * mean(pred[v$race == r] == r))
a_sur <- acc(sur); a_bis <- acc(bis)
o_sur <- 100 * mean(sur == v$race); o_bis <- 100 * mean(bis == v$race)
base_white <- 100 * mean(v$race == "white")
chg <- a_bis - a_sur

fallback <- empty | degen
fb_black <- sum(v$race[fallback] == "black")

tp   <- post[cbind(seq_len(nrow(post)), match(v$race, R))]
cali <- sapply(R, function(r) mean(tp[v$race == r]))

Mc  <- matrix(colSums(P) / sum(P), nrow(v), 5, byrow = TRUE)
b2  <- R[max.col(S * Mc, ties.method = "first")]
o_c <- 100 * mean(b2 == v$race); b_c <- 100 * mean(b2[v$race == "black"] == "black")

amb   <- apply(S, 1, max) < 0.6
a_amb <- c(surname = 100 * mean(sur[amb] == v$race[amb]),
           bisg    = 100 * mean(bis[amb] == v$race[amb]))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",", trim = TRUE)

# ---- figure data ----------------------------------------------------------
CLR <- c("#4d9221", "#C41230", "#8856a7", "#2c7fb8", "#e08214")
names(CLR) <- R

# Figure 1 · the scored electorate in a hundred squares
mix_v <- 100 * prop.table(table(factor(v$race, levels = R)))
alloc <- function(sh, k = 100) {
  b <- floor(sh * k / 100); o <- order(sh * k / 100 - b, decreasing = TRUE)
  if (k > sum(b)) b[o[seq_len(k - sum(b))]] <- b[o[seq_len(k - sum(b))]] + 1
  b
}
w100 <- alloc(as.numeric(mix_v))
wcell <- rep(R, w100)                       # 100 cells, largest remainder

# Figure 3 · the full confusion matrix
cm  <- table(factor(v$race, levels = R), factor(bis, levels = R))
cmp <- 100 * prop.table(cm, 1)
cm_worst <- which(cmp == max(cmp[row(cmp) != col(cmp)]), arr.ind = TRUE)[1, ]

# Figure 4 · the whole probability the model put on the truth, group by group
cbrk  <- seq(0, 1, by = 0.05)
chist <- lapply(R, function(r) {
  h <- hist(tp[v$race == r], breaks = cbrk, plot = FALSE)
  100 * h$counts / sum(h$counts)
})
names(chist) <- R
cmid  <- cbrk[-1] - 0.025
cmax  <- max(unlist(chist))

# Figure 5 · who the surname list has an opinion about, by area
mos_n  <- as.integer(table(factor(v0$race, levels = R)))
mos_un <- as.integer(table(factor(v0$race[!matched], levels = R)))
mos_p  <- 100 * mos_un / mos_n
mos_w  <- 100 * mos_n / sum(mos_n)
mos_hi <- which.max(mos_p)                     # most-dropped group
mos_wd <- which.max(mos_w)                     # widest column
# one caption, computed once, printed in BOTH formats (see mosaic chunks)
mos_cap <- sprintf(
  paste("Every rectangle is proportional to the number of people in it.",
        "The %s column is the widest (%s%% of all voters) and only %s%% solid;",
        "the %s column is %s%% of voters and %s%% solid."),
  PR[mos_wd], pc(mos_w[mos_wd]), pc(mos_p[mos_wd]),
  PR[mos_hi], pc(mos_w[mos_hi]), pc(mos_p[mos_hi]))

# Figure 6 · every accuracy against the floor available for free
bar9   <- c(a_bis, overall = o_bis)
bar9nm <- c(PR, "ALL VOTERS")

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- reg-header
hdr  <- scan("data/raw/voter-reg-header.txt", what = "", sep = ",",
             quiet = TRUE)
shp  <- read.csv("data/raw/source-shape.csv", stringsAsFactors = FALSE)
SV   <- function(k) shp$value[shp$name == k]
kept <- c("Last Name", "Race")

# The column names as a table rather than a monospace block: the question a
# reader has here is "which of these survive", and that is a column, not
# something to be signalled with a star in a fixed-width layout.
#
# No codebook ships with the extract, so the third column reads the names and
# values rather than quoting a document. Most are plain; the ones that are not
# are said to be not.
.reg <- c(
  "County" = "the county that maintains this registration",
  "Voter Registration Number" = "the state's identifier for this registration",
  "Status" = "active or inactive — a legal status, not a description of a person",
  "Status Reason" = "why the status is what it is",
  "Last Name" = "surname — one of the two columns this chapter is allowed to want",
  "First Name" = "given name", "Middle Name" = "middle name",
  "Suffix" = "Jr, III, and so on",
  "Birth Year" = "year of birth only; Georgia does not publish the day",
  "Residence Street Number" = "house number", "Residence Pre Direction" = "the N in N Main St",
  "Residence Street Name" = "street name", "Residence Street Type" = "St, Ave, Rd",
  "Residence Post Direction" = "the NW in Main St NW",
  "Residence Apt Unit Number" = "apartment or unit",
  "Residence City" = "city", "Residence Zipcode" = "ZIP",
  "County Precinct" = "the precinct code this voter votes in",
  "County Precinct Description" = "that precinct's name in words",
  "Municipal Precinct" = "city precinct code, where the city runs its own",
  "Municipal Precinct Description" = "that precinct's name in words",
  "Congressional District" = "U.S. House district",
  "State Senate District" = "Georgia Senate district",
  "State House District" = "Georgia House district",
  "Judicial District" = "judicial district",
  "County Commission District" = "county commission district",
  "School Board District" = "school board district",
  "City Council District" = "city council district",
  "Municipal School Board District" = "city school board district",
  "Water Board District" = "water board district",
  "Super Council District" = "a Georgia 'super' district, overlaying several ordinary ones",
  "Super Commissioner District" = "the same idea, for the county commission",
  "Super School Board District" = "the same idea, for the school board",
  "Fire District" = "fire district", "Municipality" = "city, where the address is in one",
  "Combo" = "a Georgia term for the unique combination of districts an address falls in — the ballot style",
  "Land Lot" = "colonial-era land survey lot, still carried in Georgia records",
  "Land District" = "the survey district that lot sits in",
  "Registration Date" = "when this registration was made",
  "Race" = "race as recorded by the state — the other column this chapter wants",
  "Gender" = "sex as recorded by the state",
  "Last Modified Date" = "when the record last changed",
  "Date of Last Contact" = "when the county last had contact with this voter",
  "Last Party Voted" = "the last primary ballot taken — Georgia has no party registration",
  "Last Vote Date" = "when this voter last voted",
  "Voter Created Date" = "when the record was created, which is not the registration date",
  "Mailing Street Number" = "mailing address, where it differs from residence",
  "Mailing Street Name" = "mailing address", "Mailing Apt Unit Number" = "mailing address",
  "Mailing City" = "mailing address", "Mailing Zipcode" = "mailing address",
  "Mailing State" = "mailing address", "Mailing Country" = "mailing address")
data.frame(
  Position = seq_along(hdr),
  Column_as_it_arrives = hdr,
  What_it_holds = ifelse(hdr %in% names(.reg), unname(.reg[hdr]), "—"),
  This_chapter = ifelse(hdr %in% kept, "kept", "—"))

## ---- clean-row
o <- v0[1, ]
names(o) <- c("surname", "race the voter reported", "census block")
o

## ---- one-row
o <- head(v0[v0$surname %in% c("AARON"), ], 3)
names(o) <- c("surname", "race the voter reported", "census block")
o

## ---- counts
data.frame(
  quantity = c("Registered voters in the file",
               "Distinct census blocks they live in",
               "Blocks in the county",
               "County population by race (from the 2020 census)"),
  value = c(n(nrow(v0)), n(length(unique(v0$GEOID20))), n(nrow(g)),
            paste(paste0(PR, " ", n(colSums(P))), collapse = ", ")))

## ---- drop
data.frame(
  quantity = c("Voters in the file",
               "Whose surname appears in the Census surname list",
               "Whose surname does not",
               "Voters the method is scored on"),
  value = c(n(nrow(v0)), paste0(n(sum(matched)), " (", pc(p_match), "%)"),
            n(nrow(drop)), n(nrow(v))))

## ---- surname-only
o <- data.frame(group = PR, n = n(as.integer(table(factor(v$race, levels = R)))),
                acc = paste0(pc(a_sur), "%"))
names(o) <- c("the voter actually is", "how many", "surname guesses right")
o

## ---- baseline
data.frame(
  method = c("Surname only", "Guess 'white' for everybody",
             "Difference"),
  overall_accuracy = c(paste0(pc(o_sur), "%"), paste0(pc(base_white), "%"),
                       paste0(sprintf("%+.1f", o_sur - base_white), " points")))

## ---- waffle-static
par(mar = c(0.4, 0.4, 2.2, 0.4))
plot(NA, xlim = c(-0.2, 10.2), ylim = c(11.4, -0.3), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
mtext(paste0("guessing \"white\" every time collects ", pc(base_white),
      "% of the electorate"), side = 3, line = 0.5, cex = 0.9)
for (i in seq_len(100)) {
  cx <- (i - 1) %% 10; cy <- (i - 1) %/% 10
  rect(cx + 0.06, cy + 0.06, cx + 0.94, cy + 0.94,
       col = CLR[[wcell[i]]], border = "white", lwd = 1.4)
}
fl <- w100[1] %/% 10; rm10 <- w100[1] %% 10
lines(c(0, rm10, rm10, 10), c(fl + 1, fl + 1, fl, fl), col = "grey15", lwd = 2.4)
legend(-0.35, 11.1, PR, fill = CLR, border = "white", bty = "n", horiz = TRUE,
       cex = 0.72, x.intersp = 0.35, xpd = NA,
       text.width = c(1.15, 1.15, 1.55, 1.25, 1.6))

## ---- waffle-d3
rows <- paste(sprintf('{"g":"%s","p":%.1f,"n":%d}', PR, as.numeric(mix_v),
                      as.integer(table(factor(v$race, levels = R)))),
              collapse = ",")
cells <- paste(sprintf('"%s"', PR[match(wcell, R)]), collapse = ",")
cat(sprintf('
<div id="waf" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const G=[%s], C=[%s];
const CL={"white":"#4d9221","Black":"#C41230","Hispanic":"#8856a7",
          "Asian":"#2c7fb8","Am. Indian":"#e08214"};
const W=740,H=520,M={t:40,r:20,b:64,l:20};
const box=d3.select("#waf");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const s=Math.min((H-M.t-M.b)/10,(W-M.l-M.r)/10), x0=(W-s*10)/2, y0=M.t;
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const tot={}; G.forEach(g=>tot[g.g]=g);
svg.append("g").selectAll("rect").data(C).join("rect")
  .attr("x",(d,i)=>x0+(i%%10)*s+1).attr("y",(d,i)=>y0+Math.floor(i/10)*s+1)
  .attr("width",s-2).attr("height",s-2).attr("fill",d=>CL[d]).attr("rx",2)
  .attr("opacity",0)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d}</b><br>${tot[d].p.toFixed(1)}%% of the scored voters<br>`+
     `${d3.format(",")(tot[d].n)} people`)
     .style("left",Math.min(e.offsetX+14,W-260)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0))
  .transition().duration(600).delay((d,i)=>i*7).attr("opacity",1);
const wn=C.filter(d=>d==="white").length, fl=Math.floor(wn/10), rm=wn%%10;
svg.append("path")
  .attr("d",`M${x0},${y0+(fl+1)*s} H${x0+rm*s} V${y0+fl*s} H${x0+10*s}`)
  .attr("fill","none").attr("stroke","#222").attr("stroke-width",2.2);
svg.append("text").attr("x",x0).attr("y",y0-12).attr("font-size","12.5px")
  .attr("fill","#222")
  .text("guessing \\u201cwhite\\u201d every time collects %.1f%% of the electorate");
const key=svg.append("g");
G.forEach((g,i)=>{
  const kx=x0+(i%%3)*150, ky=H-M.b+22+Math.floor(i/3)*18;
  key.append("rect").attr("x",kx).attr("y",ky-9).attr("width",12).attr("height",12)
    .attr("fill",CL[g.g]).attr("rx",2);
  key.append("text").attr("x",kx+18).attr("y",ky+1).attr("font-size","11.5px")
    .attr("fill","#333").text(g.g+" "+g.p.toFixed(1)+"%%");});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One square per percent of the %s voters the method is scored on, allocated by
largest remainder. Hover a square for the group total.</p>
', rows, cells, base_white, n(nrow(v))))

## ---- bisg
o <- data.frame(group = PR,
                n = n(as.integer(table(factor(v$race, levels = R)))),
                s = paste0(pc(a_sur), "%"), b = paste0(pc(a_bis), "%"),
                d = sprintf("%+.1f", chg))
names(o) <- c("the voter actually is", "how many", "surname only",
              "surname + block", "change")
o

## ---- overall
data.frame(
  method = c("Surname only", "BISG: surname + block"),
  overall_accuracy = c(paste0(pc(o_sur), "%"), paste0(pc(o_bis), "%")))

## ---- chg-static
par(mar = c(4.2, 4.6, 1.2, 2))
bp <- barplot(chg, names.arg = PR, col = ifelse(chg > 0, "#2c7fb8", "#C41230"),
              border = NA, ylim = c(-12, 27), las = 1,
              ylab = "change in accuracy from adding the block (points)")
abline(h = 0)
text(bp, chg + ifelse(chg > 0, 1.6, -1.6), labels = sprintf("%+.1f", chg),
     cex = 0.85)

## ---- chg-d3
rows <- paste(sprintf('{"g":"%s","v":%.1f,"n":%d,"s":%.1f,"b":%.1f}',
                      PR, chg, as.integer(table(factor(v$race, levels = R))),
                      a_sur, a_bis), collapse = ",")
cat(sprintf('
<div id="chg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=740,H=360,M={t:20,r:24,b:48,l:66};
const svg=d3.select("#chg").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(D.map(d=>d.g)).range([M.l,W-M.r]).padding(0.34);
const y=d3.scaleLinear().domain([-12,27]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${y(0)})`).call(d3.axisBottom(x).tickSize(0))
  .selectAll("text").attr("y",d=>14);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(8).tickFormat(d=>(d>0?"+":"")+d));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("change in accuracy from adding the block (points)");
const tip=d3.select("#chg").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.g)).attr("width",x.bandwidth())
  .attr("fill",d=>d.v>0?"#2c7fb8":"#C41230").attr("rx",2)
  .attr("y",y(0)).attr("height",0)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.g}</b> \\u2014 ${d3.format(",")(d.n)} voters<br>`+
     `surname only ${d.s.toFixed(1)}%% &rarr; BISG ${d.b.toFixed(1)}%%`)
     .style("left",Math.min(e.offsetX+14,W-280)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0))
  .transition().duration(750)
  .attr("y",d=>d.v>0?y(d.v):y(0)).attr("height",d=>Math.abs(y(d.v)-y(0)));
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#333");
svg.append("g").selectAll("text").data(D).join("text")
  .attr("x",d=>x(d.g)+x.bandwidth()/2).attr("y",d=>d.v>0?y(d.v)-7:y(d.v)+15)
  .attr("text-anchor","middle").attr("font-size","12.5px").attr("font-weight","600")
  .attr("fill",d=>d.v>0?"#2c7fb8":"#C41230").attr("opacity",0)
  .text(d=>(d.v>0?"+":"")+d.v.toFixed(1))
  .transition().delay(750).duration(300).attr("opacity",1);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a bar for the underlying accuracies. The overall gain of
%+.1f points conceals every one of these movements.</p>
', rows, o_bis - o_sur))

## ---- conf-static
par(mar = c(1.2, 7.4, 4.4, 1.2))
K <- length(R)
plot(NA, xlim = c(0.5, K + 0.5), ylim = c(K + 0.5, 0.5), axes = FALSE,
     xlab = "", ylab = "", asp = 1)
ramp <- colorRampPalette(c("#ffffff", "#2c7fb8"))(101)
for (i in 1:K) for (j in 1:K) {
  val <- cmp[i, j]
  rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5,
       col = ramp[round(val) + 1], border = "white", lwd = 2)
  text(j, i, pc(val), cex = 0.78, col = if (val > 55) "white" else "#333",
       font = if (i == j) 2 else 1)
}
axis(3, at = 1:K, labels = PR, tick = FALSE, line = -0.6, cex.axis = 0.82)
axis(2, at = 1:K, labels = PR, tick = FALSE, las = 1, cex.axis = 0.82)
mtext("BISG guessed", side = 3, line = 2.2, cex = 0.92)
mtext("the voter actually is", side = 2, line = 5.6, cex = 0.92)

## ---- conf-d3
rows <- paste(apply(expand.grid(i = seq_along(R), j = seq_along(R)), 1,
  function(k) sprintf('{"a":"%s","g":"%s","p":%.1f,"n":%d}',
                      PR[k[["i"]]], PR[k[["j"]]], cmp[k[["i"]], k[["j"]]],
                      cm[k[["i"]], k[["j"]]])), collapse = ",")
cat(sprintf('
<div id="cfm" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], L=[%s];
const W=740,H=430,M={t:64,r:24,b:26,l:130};
const box=d3.select("#cfm");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(L).range([M.l,W-M.r]).padding(0.045);
const y=d3.scaleBand().domain(L).range([M.t,H-M.b]).padding(0.045);
const c=d3.scaleLinear().domain([0,100]).range(["#ffffff","#2c7fb8"]);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.g)).attr("y",d=>y(d.a)).attr("width",x.bandwidth())
  .attr("height",y.bandwidth()).attr("fill",d=>c(d.p))
  .attr("stroke",d=>d.a===d.g?"#222":"#fff").attr("stroke-width",d=>d.a===d.g?2:1.5)
  .on("mousemove",function(e,d){tip.style("opacity",1).html(
     `<b>${d.a} voters guessed ${d.g}</b><br>${d.p.toFixed(1)}%% of them<br>`+
     `${d3.format(",")(d.n)} people`)
     .style("left",Math.min(e.offsetX+14,W-300)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
// on-mark: the number sits inside its confusion-matrix cell and is coloured
// against the cell, not the page. The row and column labels below use the same
// #333 on the page and do want to follow the theme.
svg.append("g").selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(d.g)+x.bandwidth()/2).attr("y",d=>y(d.a)+y.bandwidth()/2+4)
  .attr("text-anchor","middle").attr("font-size","12px").attr("class","on-mark")
  .attr("font-weight",d=>d.a===d.g?"700":"400")
  .attr("fill",d=>d.p>55?"#fff":"#333").text(d=>d.p.toFixed(1)+"%%");
L.forEach(l=>{
  svg.append("text").attr("x",x(l)+x.bandwidth()/2).attr("y",M.t-10)
    .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#333").text(l);
  svg.append("text").attr("x",M.l-10).attr("y",y(l)+y.bandwidth()/2+4)
    .attr("text-anchor","end").attr("font-size","12px").attr("fill","#333").text(l);});
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",22).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("fill","#444").text("BISG guessed");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+H-M.b)/2).attr("y",16)
  .attr("text-anchor","middle").attr("font-size","12.5px").attr("fill","#444")
  .text("the voter actually is");
})();
</script>
', rows, paste(sprintf('"%s"', PR), collapse = ",")))

## ---- fallbacks
data.frame(
  situation = c("Voters living in a block the census records as having nobody in it",
                "Voters whose surname and block flatly contradict each other",
                "Total needing a fallback",
                "  of whom are Black"),
  voters = c(n(sum(empty)), n(sum(degen)), n(sum(fallback)), n(fb_black)))

## ---- calibration
o <- data.frame(group = PR, p = pc(cali, 2), acc = paste0(pc(a_bis), "%"))
names(o) <- c("the voter actually is",
              "mean probability BISG placed on the truth",
              "how often BISG's top guess was right")
o

## ---- cali-static
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 5), mar = c(3.6, 1.0, 2.4, 0.4), oma = c(1.6, 3.4, 0.4, 0.4))
for (k in seq_along(R)) {
  r <- R[k]
  plot(NA, xlim = c(0, 1), ylim = c(0, cmax * 1.06), axes = FALSE,
       xlab = "", ylab = "")
  rect(cbrk[-length(cbrk)], 0, cbrk[-1], chist[[r]], col = CLR[[r]],
       border = "white", lwd = 0.7)
  abline(v = cali[[r]], lwd = 2, col = "#222")
  axis(1, at = c(0, 0.5, 1), labels = c("0", ".5", "1"), cex.axis = 0.95)
  if (k == 1) axis(2, las = 1, cex.axis = 0.95)
  mtext(PR[k], side = 3, line = 0.7, cex = 0.78)
  mtext(paste0("mean ", pc(cali[[r]], 2)), side = 3, line = -0.2, cex = 0.66,
        col = "#555")
}
mtext("probability BISG placed on the truth", side = 1, outer = TRUE,
      line = 0.3, cex = 0.8)
mtext("% of the group's voters", side = 2, outer = TRUE, line = 1.9, cex = 0.8)
par(op)

## ---- cali-d3
ser <- paste(mapply(function(r, pr) sprintf('{"g":"%s","m":%.4f,"h":[%s]}',
    pr, cali[[r]], paste(sprintf("%.2f", chist[[r]]), collapse = ",")),
    R, PR), collapse = ",")
cat(sprintf('
<div id="cal" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], MX=%.2f;
const CL={"white":"#4d9221","Black":"#C41230","Hispanic":"#8856a7",
          "Asian":"#2c7fb8","Am. Indian":"#e08214"};
const W=760,H=250,M={t:34,r:14,b:52,l:44},GAP=14;
const box=d3.select("#cal");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const pw=(W-M.l-M.r-GAP*(D.length-1))/D.length;
const y=d3.scaleLinear().domain([0,MX*1.06]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(4).tickFormat(d=>d+"%%"));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
D.forEach((s,k)=>{
  const ox=M.l+k*(pw+GAP);
  const x=d3.scaleLinear().domain([0,1]).range([ox,ox+pw]);
  const g=svg.append("g");
  g.selectAll("rect").data(s.h).join("rect")
    .attr("x",(d,i)=>x(i*0.05)+0.4).attr("width",x(0.05)-x(0)-0.8)
    .attr("fill",CL[s.g]).attr("y",y(0)).attr("height",0)
    .on("mousemove",function(e,d,i){tip.style("opacity",1).html(
       `<b>${s.g}</b><br>${d.toFixed(1)}%% of them`)
       .style("left",Math.min(e.offsetX+14,W-220)+"px").style("top",(e.offsetY-10)+"px");})
    .on("mouseleave",()=>tip.style("opacity",0))
    .transition().duration(600).delay((d,i)=>i*12)
    .attr("y",d=>y(d)).attr("height",d=>y(0)-y(d));
  g.append("g").attr("transform",`translate(0,${H-M.b})`)
    .call(d3.axisBottom(x).tickValues([0,0.5,1]).tickFormat(d=>d===0?"0":(d===1?"1":".5")));
  g.append("line").attr("x1",x(s.m)).attr("x2",x(s.m)).attr("y1",M.t).attr("y2",y(0))
    .attr("stroke","#222").attr("stroke-width",2);
  g.append("text").attr("x",ox+pw/2).attr("y",M.t-16).attr("text-anchor","middle")
    .attr("font-size","12px").attr("fill","#222").text(s.g);
  g.append("text").attr("x",ox+pw/2).attr("y",M.t-4).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#666").text("mean "+s.m.toFixed(2));
});
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-12).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("probability BISG placed on the truth");
})();
</script>
', ser, cmax))

## ---- county-prior
data.frame(
  prior = c("Block-level (BISG)", "Surname only, no geography",
            "County-level prior"),
  overall = paste0(pc(c(o_bis, o_sur, o_c)), "%"),
  black_voters = paste0(pc(c(a_bis[["black"]], a_sur[["black"]], b_c)), "%"))

## ---- ambiguous
data.frame(
  quantity = c("Voters with an ambiguous surname",
               "Surname only, on those voters",
               "BISG, on those voters"),
  value = c(n(sum(amb)), paste0(pc(a_amb[["surname"]]), "%"),
            paste0(pc(a_amb[["bisg"]]), "%")))

## ---- who-dropped
o <- data.frame(group = PR,
                all = paste0(pc(as.numeric(mix_all)), "%"),
                dropped = paste0(pc(as.numeric(mix_drop)), "%"))
names(o) <- c("group", "share of all registered voters",
              "share of the voters with no surname match")
o

## ---- mosaic-static
par(mar = c(6.0, 4.6, 4.4, 1.2))
wdt <- mos_n / sum(mos_n); edge <- c(0, cumsum(wdt))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
for (k in seq_along(R)) {
  l <- edge[k] + 0.002; rgt <- edge[k + 1] - 0.002
  h <- mos_p[k] / 100
  rect(l, 0, rgt, 1 - h, col = adjustcolor(CLR[[k]], alpha.f = 0.28),
       border = "white", lwd = 1.4)
  rect(l, 1 - h, rgt, 1, col = CLR[[k]], border = "white", lwd = 1.4)
  text((l + rgt) / 2, 1 - h - 0.045, pc(mos_p[k]), cex = 0.74, col = "#222")
  ln <- if (wdt[k] > 0.08) 0.2 else c(0.2, 1.4, 2.6)[k - 2]
  mtext(PR[k], side = 3, at = (l + rgt) / 2, line = ln, cex = 0.74)
}
axis(2, at = c(0, 0.5, 1), labels = c("100%", "50%", "0%"), las = 1,
     cex.axis = 0.8)
mtext("column width = share of all registered voters", side = 1, line = 1.2,
      cex = 0.85)
mtext("solid = no surname match", side = 2, line = 3.2, cex = 0.85)
cw <- strwrap(mos_cap, width = 104)
mtext(cw, side = 1, line = 2.6 + (seq_along(cw) - 1) * 0.95, at = 0, adj = 0,
      cex = 0.68, col = "#555555")

## ---- mosaic-d3
rows <- paste(sprintf('{"g":"%s","n":%d,"u":%d,"p":%.2f,"w":%.5f}',
                      PR, mos_n, mos_un, mos_p, mos_n / sum(mos_n)),
              collapse = ",")
cat(sprintf('
<div id="mos" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const CL={"white":"#4d9221","Black":"#C41230","Hispanic":"#8856a7",
          "Asian":"#2c7fb8","Am. Indian":"#e08214"};
const W=740,H=420,M={t:62,r:20,b:56,l:56};
const box=d3.select("#mos");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const IW=W-M.l-M.r, IH=H-M.t-M.b;
let acc=0; D.forEach(d=>{d.x0=acc; acc+=d.w; d.x1=acc;});
// 0%% at the top, 100%% at the bottom: the solid "no surname match" block
// hangs from the top of each column, exactly as in the static twin.
const y=d3.scaleLinear().domain([0,100]).range([M.t,M.t+IH]);
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(5).tickFormat(d=>d+"%%"));
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const g=svg.append("g");
function hov(sel){sel.on("mousemove",function(e,d){tip.style("opacity",1).html(
   `<b>${d.g}</b><br>${d3.format(",")(d.n)} registered voters<br>`+
   `${d3.format(",")(d.u)} with no surname match (${d.p.toFixed(1)}%%)`)
   .style("left",Math.min(e.offsetX+14,W-320)+"px").style("top",(e.offsetY-10)+"px");})
 .on("mouseleave",()=>tip.style("opacity",0));}
hov(g.selectAll("rect.m").data(D).join("rect")
  .attr("x",d=>M.l+d.x0*IW+1).attr("width",d=>Math.max(1,(d.x1-d.x0)*IW-2))
  .attr("y",d=>y(d.p)).attr("height",d=>M.t+IH-y(d.p))
  .attr("fill",d=>CL[d.g]).attr("fill-opacity",0.26));
hov(g.selectAll("rect.u").data(D).join("rect")
  .attr("x",d=>M.l+d.x0*IW+1).attr("width",d=>Math.max(1,(d.x1-d.x0)*IW-2))
  .attr("y",M.t).attr("height",d=>y(d.p)-M.t).attr("fill",d=>CL[d.g]));
g.selectAll("text.p").data(D).join("text")
  .attr("x",d=>M.l+(d.x0+d.x1)/2*IW).attr("y",d=>y(d.p)+14)
  .attr("text-anchor","middle").attr("font-size","11.5px").attr("fill","#222")
  .text(d=>d.p.toFixed(1)+"%%");
g.selectAll("text.g").data(D).join("text")
  .attr("x",d=>M.l+(d.x0+d.x1)/2*IW).attr("y",(d,i)=>M.t-10-(i%%3)*16)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#333")
  .text(d=>d.g);
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("column width = share of all %s registered voters");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(M.t+M.t+IH)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("solid = no surname match");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s</p>
', rows, n(nrow(v0)), mos_cap))

## ---- floor-static
par(mar = c(4.6, 4.8, 1.2, 1.4))
bp <- barplot(bar9, names.arg = bar9nm, las = 1, ylim = c(0, 100),
              col = c(CLR, "#555"), border = NA, cex.names = 0.84,
              ylab = "BISG identifies this group correctly (%)")
abline(h = base_white, lty = 2, lwd = 2, col = "#222")
text(bp[1], base_white + 4.5, paste0("guess \"white\" every time: ",
     pc(base_white), "%"), adj = c(0, 0), cex = 0.78, col = "#222")
text(bp, bar9 + 3.2, paste0(pc(bar9), "%"), cex = 0.8)
box(bty = "l")

## ---- floor-d3
rows <- paste(sprintf('{"g":"%s","v":%.1f,"c":"%s"}', bar9nm, bar9,
                      c(CLR, "#555555")), collapse = ",")
cat(sprintf('
<div id="flr" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s], FL=%.2f;
const W=740,H=360,M={t:20,r:24,b:52,l:64};
const box=d3.select("#flr");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(D.map(d=>d.g)).range([M.l,W-M.r]).padding(0.32);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).tickSize(0));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("BISG identifies this group correctly");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",d=>x(d.g)).attr("width",x.bandwidth()).attr("fill",d=>d.c).attr("rx",2)
  .attr("y",y(0)).attr("height",0)
  .transition().duration(700).delay((d,i)=>i*70)
  .attr("y",d=>y(d.v)).attr("height",d=>y(0)-y(d.v));
svg.append("g").selectAll("text.v").data(D).join("text")
  .attr("x",d=>x(d.g)+x.bandwidth()/2).attr("y",d=>y(d.v)-7).attr("text-anchor","middle")
  .attr("font-size","12px").attr("font-weight","600").attr("fill","#333")
  .attr("opacity",0).text(d=>d.v.toFixed(1)+"%%")
  .transition().delay(700).duration(300).attr("opacity",1);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(FL)).attr("y2",y(FL))
  .attr("stroke","#222").attr("stroke-dasharray","7,5").attr("stroke-width",2);
svg.append("text").attr("x",M.l+8).attr("y",y(FL)-8).attr("font-size","12px")
  .attr("fill","#222").text("guess \\u201cwhite\\u201d every time: "+FL.toFixed(1)+"%%");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The dashed line is free: it is what a method with no method scores. %d of the
five groups sit below it, and so would the whole electorate if it were not
%.1f%% white.</p>
', rows, base_white, sum(a_bis < base_white), base_white))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#mos text[fill="#222" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: the on-mark chunk above pins this fill dark for the dark
# page, so a --paper stroke there would sit dark behind a dark ink, and the
# checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#mos text[fill="#222" i],
#chg text[fill="currentcolor" i],
#chg text:not([fill])
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
#cfm text[fill="#fff" i],
#cfm text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#cfm text[fill="#fff" i],
#cfm text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
