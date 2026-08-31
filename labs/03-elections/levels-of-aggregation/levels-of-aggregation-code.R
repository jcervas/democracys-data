# levels-of-aggregation-code.R -- chunk bodies for levels-of-aggregation-brief.Rmd
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
ld <- read.csv(file.path(D, "derived/ladder.csv"),        stringsAsFactors = FALSE)
cf <- read.csv(file.path(D, "derived/cvr_facts.csv"),     stringsAsFactors = FALSE)
c2 <- read.csv(file.path(D, "derived/cvr_second.csv"),    stringsAsFactors = FALSE)
gl <- read.csv(file.path(D, "derived/ga_levels.csv"),     stringsAsFactors = FALSE)
lst <- read.csv(file.path(D, "derived/level_stats.csv"),   stringsAsFactors = FALSE)
cn <- read.csv(file.path(D, "derived/consistency.csv"),   stringsAsFactors = FALSE)
nc <- read.csv(file.path(D, "derived/not_counties.csv"),  stringsAsFactors = FALSE)
sv <- read.csv(file.path(D, "derived/seat_vote.csv"),     stringsAsFactors = FALSE)
mt <- read.csv(file.path(D, "derived/method_truth.csv"),  stringsAsFactors = FALSE)
ep <- read.csv(file.path(D, "derived/eco_precinct.csv"),  stringsAsFactors = FALSE)
ec <- read.csv(file.path(D, "derived/eco_county.csv"),    stringsAsFactors = FALSE)
ee <- read.csv(file.path(D, "derived/eco_estimates.csv"), stringsAsFactors = FALSE)
mp <- read.csv(file.path(D, "derived/maup_null.csv"),     stringsAsFactors = FALSE)
md <- read.csv(file.path(D, "derived/maup_draws.csv"),    stringsAsFactors = FALSE)
rp <- read.csv(file.path(D, "derived/rpv_check.csv"),     stringsAsFactors = FALSE)
dc <- read.csv(file.path(D, "derived/decision.csv"),      stringsAsFactors = FALSE)
su <- read.csv(file.path(D, "derived/suitability.csv"),   stringsAsFactors = FALSE)

n  <- function(x) format(round(x), big.mark = ",")
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
V  <- function(d, k, col = "value", key = names(d)[1]) d[[col]][d[[key]] == k]
cv <- function(k) V(cf, k)
sq <- function(k) V(sv, k, key = "measure")
LS <- function(k, col) lst[[col]][lst$level == k]
EE <- function(k, col) ee[[col]][ee$level == k]

PAL <- c("ballot" = "#C41230", "precinct" = "#2c7fb8",
         "election jurisdiction" = "#8856a7", "county" = "#4d9221",
         "congressional district" = "#e08214", "state" = "#999999",
         "nation" = "#333333")
# the four rungs the Georgia walk uses, in the order they appear in ga_levels.csv
PAL4 <- unname(PAL[c("precinct", "county", "congressional district", "state")])
TRUTHC <- "#4d9221"; GOODC <- "#C41230"; CTYC <- "#e08214"

# Dot size in the small-multiples figure. Radius rises with the square root of
# the unit's votes, so AREA tracks votes, but there is a floor: a precinct with
# a few hundred ballots would otherwise be a dot too small to see, and dropping
# it from view would understate exactly the scatter the figure exists to show.
# The floor is stated in the caption rather than hidden in the constant.
R_FLOOR <- 0.5; R_SPAN <- 3.2          # d3 radius, px
CAP_SM  <- paste("Dot area follows the votes cast in the unit, above a floor",
                 "that keeps the smallest units visible. The center of gravity",
                 "never moves; the scatter around it disappears.")

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- clean-levels
nt <- function(x) format(x, big.mark = ",", trim = TRUE)
o <- head(gl[gl$level == "precinct", ], 3)
o$dem_share <- pc(o$dem_share, 2)
o$two_party_votes <- nt(o$two_party_votes)
names(o) <- c("level", "unit", "dem", "rep", "two party votes", "dem share")
o

## ---- level-rows
gac <- as.integer(ld$units_in_georgia[ld$level == "precinct"])
tb  <- table(gl$level)
LV  <- c("precinct", "county", "congressional district", "state")
data.frame(
  level = LV,
  rows_arriving = nt(as.integer(ld$units_in_georgia[match(LV, ld$level)])),
  rows_kept = nt(as.integer(tb[LV])),
  a_row_is = ld$a_row_is[match(LV, ld$level)])

## ---- ladder-tbl
o <- ld[, c("level", "units_nationally", "units_in_georgia",
            "a_row_is", "published_for")]
o$units_nationally <- n(o$units_nationally)
o$units_in_georgia <- ifelse(is.na(ld$units_in_georgia), "\u2014",
                             format(ld$units_in_georgia, big.mark = ",",
                                    trim = TRUE))
names(o) <- c("level", "units nationally", "in Georgia", "a row is",
              "published for")
o

## ---- cvr-second
o <- c2[c2$first %in% c("Begich, Nick", "Peltola, Mary S."), ]
o <- o[o$second != "(no second choice)", ]
o <- do.call(rbind, lapply(split(o, o$first), function(z) head(z, 2)))
o$ballots <- n(o$ballots)
names(o) <- c("ranked first", "ranked second", "ballots")
o

## ---- level-stats
o <- lst[, c("level", "units", "mean_dem_share", "sd_dem_share",
            "min_dem_share", "max_dem_share")]
names(o) <- c("level", "units", "mean dem %", "sd", "min", "max")
o

## ---- truth
o <- mt[order(-mt$total), ]
o$by_mail <- n(o$by_mail); o$not_by_mail <- n(o$not_by_mail); o$total <- n(o$total)
names(o) <- c("candidate", "by mail", "not by mail", "total", "% of them by mail")
o

## ---- eco-d3
set.seed(3)
z <- ep[sample(nrow(ep), 1400), ]
pts <- paste(sprintf('[%.1f,%.1f,%.2f]', z$mail_pct, z$dem_pct,
                     sqrt(z$votes / max(ep$votes))), collapse = ",")
cat(sprintf('
<div id="eco" style="margin:1.1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const P=[%s];
const G0=%.2f,G1=%.2f,T0=%.2f,T1=%.2f;
const W=760,H=400,L=52,R=190,TOP=34,B=44;
const svg=d3.select("#eco").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([L,W-R]);
const y=d3.scaleLinear().domain([-30,210]).range([H-B,TOP]);
const T=(a,b,s,o)=>{const t=svg.append("text").attr("x",a).attr("y",b)
  .attr("text-anchor",(o&&o.a)||"start").attr("font-size",(o&&o.s)||"11px")
  .attr("fill",(o&&o.c)||"#444");if(o&&o.b)t.attr("font-weight","600");return t.text(s);};
svg.append("rect").attr("x",L).attr("y",y(100)).attr("width",W-R-L)
  .attr("height",y(0)-y(100)).attr("fill","#f4f4f4");
T(L+6,y(100)-5,"everything above this line is an impossible share",{s:"10px",c:"#999"});
[-25,0,25,50,75,100,125,150,175,200].forEach(v=>{
  svg.append("line").attr("x1",L).attr("x2",W-R).attr("y1",y(v)).attr("y2",y(v))
    .attr("stroke",v===0||v===100?"#bbb":"#eee").attr("stroke-width",1);
  T(L-7,y(v)+3.5,v,{a:"end",s:"9.5px",c:"#aaa"});});
[0,25,50,75,100].forEach(v=>T(x(v),H-B+15,v,{a:"middle",s:"9.5px",c:"#aaa"}));
svg.append("g").selectAll("circle").data(P).join("circle")
  .attr("cx",d=>x(d[0])).attr("cy",d=>y(d[1])).attr("r",d=>1+3.2*d[2])
  .attr("fill","#2c7fb8").attr("fill-opacity",0.22);
const ln=(a,b,c,w,dash)=>svg.append("line").attr("x1",x(0)).attr("y1",y(a))
  .attr("x2",x(100)).attr("y2",y(b)).attr("stroke",c).attr("stroke-width",w)
  .attr("stroke-dasharray",dash||null);
ln(T0,T1,"%s",2.6);
ln(G0,G1,"%s",2.6,"7,4");
[[T1,"%s","the truth: "+T0.toFixed(1)+"%% → "+T1.toFixed(1)+"%%"],
 [G1,"%s","Goodman: "+G0.toFixed(1)+"%% → "+G1.toFixed(1)+"%%"]]
 .forEach(d=>{svg.append("circle").attr("cx",x(100)).attr("cy",y(d[0])).attr("r",4.5)
   .attr("fill",d[1]);T(x(100)+9,y(d[0])+4,d[2],{c:d[1],b:1,s:"11.5px"});});
svg.append("circle").attr("cx",x(0)).attr("cy",y(T0)).attr("r",4.5).attr("fill","%s");
svg.append("circle").attr("cx",x(0)).attr("cy",y(G0)).attr("r",4.5).attr("fill","%s");
T(8,20,"2,653 Georgia precincts, 2020: mail share against Democratic share",{b:1,s:"12.5px",c:"#222"});
T(L,H-10,"share of the precinct\\u2019s votes cast by mail (%%)",{s:"10.5px",c:"#888"});
["Both lines pass through the","same cloud of precincts.","They differ only in slope,","and the slope is the answer.",
 "","Extrapolate the fitted line","to a precinct that voted","100%% by mail and it returns","a share no share can take."]
 .forEach((s,i)=>T(W-R+9,y(T1)+52+i*14,s,{s:"10px",c:i>4?"#777":"#555"}));
})();
</script>
', pts, EE("precinct","est_not_by_mail"), EE("precinct","est_by_mail"),
   EE("precinct","truth_not_by_mail"), EE("precinct","truth_by_mail"),
   TRUTHC, GOODC, TRUTHC, GOODC, TRUTHC, TRUTHC))

## ---- eco-static
par(mar = c(3.4, 3.6, 2.2, 9.6))
plot(NA, xlim = c(0, 100), ylim = c(-30, 210), axes = FALSE, ann = FALSE)
rect(0, 0, 100, 100, col = "#f4f4f4", border = NA)
abline(h = c(0, 100), col = "#bbb")
axis(1, seq(0, 100, 25), cex.axis = 0.62, col = "#bbb", col.axis = "#888",
     tck = -0.012, mgp = c(2, 0.3, 0))
axis(2, seq(-25, 200, 25), las = 1, cex.axis = 0.6, col = "#bbb",
     col.axis = "#888", tck = -0.012, mgp = c(2, 0.45, 0))
set.seed(3); z <- ep[sample(nrow(ep), 1400), ]
points(z$mail_pct, z$dem_pct, pch = 19,
       cex = 0.18 + 1.0 * sqrt(z$votes / max(ep$votes)),
       col = adjustcolor("#2c7fb8", 0.22))
segments(0, EE("precinct","truth_not_by_mail"), 100, EE("precinct","truth_by_mail"),
         col = TRUTHC, lwd = 2.4)
segments(0, EE("precinct","est_not_by_mail"), 100, EE("precinct","est_by_mail"),
         col = GOODC, lwd = 2.4, lty = 2)
points(c(0, 100, 0, 100),
       c(EE("precinct","truth_not_by_mail"), EE("precinct","truth_by_mail"),
         EE("precinct","est_not_by_mail"),   EE("precinct","est_by_mail")),
       pch = 19, cex = 1.1, col = c(TRUTHC, TRUTHC, GOODC, GOODC))
text(102, EE("precinct","est_by_mail"), sprintf("Goodman: %.1f%% to %.1f%%",
     EE("precinct","est_not_by_mail"), EE("precinct","est_by_mail")),
     adj = 0, cex = 0.62, font = 2, col = GOODC, xpd = NA)
text(102, EE("precinct","truth_by_mail"), sprintf("the truth: %.1f%% to %.1f%%",
     EE("precinct","truth_not_by_mail"), EE("precinct","truth_by_mail")),
     adj = 0, cex = 0.62, font = 2, col = TRUTHC, xpd = NA)
text(3, 106, "everything above this line is an impossible share", adj = 0,
     cex = 0.55, col = "#999")
title("2,653 Georgia precincts, 2020: mail share against Democratic share",
      cex.main = 0.84, adj = 0, line = 0.8)
mtext("share of the precinct's votes cast by mail (%)", side = 1, cex = 0.58,
      col = "#888", line = 1.7)
mtext("Democratic share of the precinct's votes (%)", side = 2, cex = 0.58,
      col = "#888", line = 2.3)
text(104, -14, "Both lines run through\nthe same cloud. They differ\nonly in slope, and the\nslope is the answer.",
     adj = 0, cex = 0.55, col = "#555", xpd = NA)

## ---- eco-table
o <- ee[, c("level", "units", "correlation", "est_by_mail", "truth_by_mail",
            "bound_lower", "bound_upper")]
names(o) <- c("level", "units", "r", "Goodman says (%)", "the truth (%)",
              "bound, lower", "bound, upper")
o

## ---- maup
o <- mp
names(o) <- c("grouping", "units", "correlation", "note")
o

## ---- decision
o <- dc[, c("question", "right_rung", "why", "the_trap")]
names(o) <- c("the question", "the rung", "why that one", "the trap")
o

