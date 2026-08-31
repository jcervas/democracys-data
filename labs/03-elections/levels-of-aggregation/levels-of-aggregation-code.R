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

## ---- ladder-d3
rows <- paste(sprintf(
  '{"l":"%s","u":%d,"g":%s,"r":"%s","c":"%s","p":"%s"}',
  ld$level, ld$units_nationally,
  ifelse(is.na(ld$units_in_georgia), "null", ld$units_in_georgia),
  ld$a_row_is, PAL[ld$level], ld$published_for), collapse = ",")
cat(sprintf('
<div id="lad" style="margin:1.1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s];
const W=760,H=352,X0=178,XW=300,TOP=42,RH=40;
const svg=d3.select("#lad").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const T=(x,y,s,o)=>{const t=svg.append("text").attr("x",x).attr("y",y)
  .attr("text-anchor",(o&&o.a)||"start").attr("font-size",(o&&o.s)||"11px")
  .attr("fill",(o&&o.c)||"#444");if(o&&o.b)t.attr("font-weight","600");
  return t.text(s);};
const lg=d3.scaleLinear().domain([0,Math.log10(D[0].u)]).range([14,XW]);
T(8,20,"every rung of the ladder, and how many units sit on it",{b:1,s:"12.5px",c:"#222"});
T(X0,20,"width \\u221d log\\u2081\\u2080 of the number of units",{s:"10.5px",c:"#888"});
D.forEach((d,i)=>{
  const y=TOP+i*RH, w=lg(Math.log10(d.u));
  svg.append("rect").attr("x",X0-w/2+XW/2).attr("y",y).attr("width",w).attr("height",RH-11)
    .attr("fill",d.c).attr("fill-opacity",0.82).attr("rx",2);
  T(X0-10,y+19,d.l,{a:"end",b:1,s:"12px",c:d.c});
  T(X0+XW/2,y+19,d3.format(",")(d.u),{a:"middle",s:"11px",
     c:(w>62?"#fff":"#444")});
  T(X0+XW+16,y+19,d.r+(d.g===null?"":("  \u00b7  Georgia: "+d3.format(",")(d.g))),
    {s:"10.5px",c:"#444"});
});
T(8,TOP+7*RH+6,"Each step up is irreversible. The rows below are not recoverable from the rows above.",
  {s:"10.5px",c:"#666"});
})();
</script>
', rows))

## ---- ladder-static
par(mar = c(1.4, 0.3, 2.0, 0.3))
plot(NA, xlim = c(0, 100), ylim = c(7.6, -0.9), axes = FALSE, ann = FALSE)
X0 <- 26; XW <- 40
w <- 1 + 39 * log10(ld$units_nationally) / log10(max(ld$units_nationally))
for (i in seq_len(nrow(ld))) {
  cx <- X0 + XW/2
  rect(cx - w[i]/2, i - 1 + 0.10, cx + w[i]/2, i - 1 + 0.72,
       col = adjustcolor(PAL[ld$level[i]], 0.82), border = NA)
  text(X0 - 1.4, i - 1 + 0.44, ld$level[i], adj = 1, cex = 0.72, font = 2,
       col = PAL[ld$level[i]])
  text(cx, i - 1 + 0.44, n(ld$units_nationally[i]), cex = 0.62,
       col = if (w[i] > 8.5) "#ffffff" else "#444444")
  lab <- ld$a_row_is[i]
  if (!is.na(ld$units_in_georgia[i]))
    lab <- paste0(lab, "  \u00b7  Georgia: ", n(ld$units_in_georgia[i]))
  text(X0 + XW + 2, i - 1 + 0.44, lab, adj = 0, cex = 0.56, col = "#444")
}
title("every rung of the ladder, and how many units sit on it",
      cex.main = 0.86, adj = 0, line = 0.6)
mtext("Each step up is irreversible. The rows below are not recoverable from the rows above.",
      side = 1, cex = 0.55, col = "#666", adj = 0, line = 0)

## ---- cvr-tbl
o <- cf[c(1, 2, 5, 6, 7), ]
o$value <- n(o$value)
o$value[3] <- paste0(cf$value[5], "%")
names(o) <- c("quantity", "value")
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

## ---- multiples-d3
mk <- function(k, cap) {
  z <- gl[gl$level == k, ]
  if (nrow(z) > 900) z <- z[sample(nrow(z), 900), ]
  w <- if (all(is.na(z$two_party_votes))) rep(1, nrow(z)) else z$two_party_votes
  paste(sprintf('{"x":%.2f,"w":%.3f}', z$dem_share,
                sqrt(w / max(w, na.rm = TRUE))), collapse = ",")
}
KS <- unique(gl$level)
blocks <- paste(sprintf('{"k":"%s","n":%d,"c":"%s","d":[%s]}',
                        KS, sapply(KS, function(k) sum(gl$level == k)),
                        PAL4, sapply(KS, mk)), collapse = ",")
cat(sprintf('
<div id="sm" style="margin:1.1em 0"></div>
<script>
(function(){
const B=[%s], MEAN=%.4f;
const W=760,H=300,L=132,R=26,TOP=40,PH=58;
const svg=d3.select("#sm").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([L,W-R]);
const T=(a,b,s,o)=>{const t=svg.append("text").attr("x",a).attr("y",b)
  .attr("text-anchor",(o&&o.a)||"start").attr("font-size",(o&&o.s)||"11px")
  .attr("fill",(o&&o.c)||"#444");if(o&&o.b)t.attr("font-weight","600");return t.text(s);};
T(8,20,"the same election, read at four levels",{b:1,s:"12.5px",c:"#222"});
T(W-R,20,"two-party Democratic share (%%)",{a:"end",s:"10.5px",c:"#888"});
svg.append("line").attr("x1",x(MEAN)).attr("x2",x(MEAN)).attr("y1",TOP-8)
  .attr("y2",TOP+B.length*PH-14).attr("stroke","#C41230").attr("stroke-width",1.3)
  .attr("stroke-dasharray","4,3");
T(x(MEAN)+5,TOP-11,"the state total, "+MEAN.toFixed(2)+"%%",{s:"10px",c:"#C41230",b:1});
B.forEach((b,i)=>{
  const y0=TOP+i*PH;
  svg.append("g").selectAll("circle").data(b.d).join("circle")
    .attr("cx",d=>x(d.x)).attr("cy",()=>y0+16+(Math.random()-0.5)*20)
    .attr("r",d=>%.2f+%.2f*d.w).attr("fill",b.c)
    .attr("fill-opacity",b.n>500?0.10:0.62);
  T(L-10,y0+15,b.k,{a:"end",b:1,s:"11.5px",c:b.c});
  T(L-10,y0+27,b.n+" unit"+(b.n>1?"s":""),{a:"end",s:"9.5px",c:"#999"});
});
[0,25,50,75,100].forEach(v=>T(x(v),TOP+B.length*PH-2,v,{a:"middle",s:"9.5px",c:"#aaa"}));
T(8,H-8,"%s",{s:"10px",c:"#666"});
})();
</script>
', blocks, lst$mean_dem_share[lst$level == "state"], R_FLOOR, R_SPAN, CAP_SM))

## ---- multiples-static
KS <- unique(gl$level)
CLS <- PAL4
par(mar = c(4.6, 8.4, 2.2, 0.6))
plot(NA, xlim = c(0, 100), ylim = c(length(KS) + 0.4, 0.4), axes = FALSE, ann = FALSE)
axis(1, at = seq(0, 100, 25), cex.axis = 0.62, col = "#bbb", col.axis = "#888",
     tck = -0.012, mgp = c(2, 0.25, 0))
MEAN <- lst$mean_dem_share[lst$level == "state"]
abline(v = MEAN, col = "#C41230", lty = 2, lwd = 1.2)
text(MEAN + 1.5, 0.5, sprintf("state total, %.2f%%", MEAN), adj = 0, cex = 0.58,
     col = "#C41230", font = 2)
set.seed(7)
for (i in seq_along(KS)) {
  z <- gl[gl$level == KS[i], ]
  w <- if (all(is.na(z$two_party_votes))) rep(1, nrow(z)) else z$two_party_votes
  # same floor-plus-square-root rule as the d3 twin, rescaled to cex units so
  # the largest dot is the size it always was and only the floor changes
  cx <- (R_FLOOR + R_SPAN * sqrt(w / max(w))) * (1.224 / (R_FLOOR + R_SPAN))
  points(z$dem_share, i + runif(nrow(z), -0.24, 0.24), pch = 19,
         cex = cx, col = adjustcolor(CLS[i], if (nrow(z) > 500) 0.09 else 0.62))
  mtext(KS[i], side = 2, at = i - 0.09, las = 1, cex = 0.62, font = 2,
        col = CLS[i], line = 0.3, adj = 1)
  mtext(paste(nrow(z), if (nrow(z) > 1) "units" else "unit"), side = 2,
        at = i + 0.19, las = 1, cex = 0.5, col = "#999", line = 0.3, adj = 1)
}
title("the same election, read at four levels", cex.main = 0.86, adj = 0, line = 0.9)
mtext("two-party Democratic share (%)", side = 1, cex = 0.53, col = "#666",
      line = 1.35)
cw <- strwrap(CAP_SM, width = 108)
mtext(cw, side = 1, line = 2.3 + (seq_along(cw) - 1) * 0.9, adj = 0,
      cex = 0.53, col = "#666")

## ---- consistency
o <- cn[, c("comparison", "from", "against", "difference")]
names(o) <- c("quantity", "counted as", "against", "gap")
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

## ---- maup-d3
cat(sprintf('
<div id="mau" style="margin:1.1em 0"></div>
<script>
(function(){
const D=[%s], RP=%.4f, RC=%.4f;
const W=760,H=196,L=52,R=30,TOP=44,BOT=48;
const svg=d3.select("#mau").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0.40,0.80]).range([L,W-R]);
const T=(a,b,s,o)=>{const t=svg.append("text").attr("x",a).attr("y",b)
  .attr("text-anchor",(o&&o.a)||"start").attr("font-size",(o&&o.s)||"11px")
  .attr("fill",(o&&o.c)||"#444");if(o&&o.b)t.attr("font-weight","600");return t.text(s);};
T(8,20,"500 random regroupings of the same precincts into 159 groups",{b:1,s:"12.5px",c:"#222"});
const bins=d3.bin().domain(x.domain()).thresholds(46)(D);
const yh=d3.scaleLinear().domain([0,d3.max(bins,b=>b.length)]).range([H-BOT,TOP]);
svg.selectAll("rect.b").data(bins).join("rect")
  .attr("x",b=>x(b.x0)).attr("width",b=>Math.max(1,x(b.x1)-x(b.x0)-1))
  .attr("y",b=>yh(b.length)).attr("height",b=>H-BOT-yh(b.length))
  .attr("fill","#999999").attr("fill-opacity",0.55);
[[RP,"#2c7fb8","the 2,653 precincts"],[RC,"#4d9221","the 159 real counties"]].forEach((d,i)=>{
  svg.append("line").attr("x1",x(d[0])).attr("x2",x(d[0])).attr("y1",TOP-14)
    .attr("y2",H-BOT).attr("stroke",d[1]).attr("stroke-width",2.2);
  T(x(d[0])-7,TOP-18,d[2]+" \\u00b7 "+d[0].toFixed(3),
    {c:d[1],b:1,s:"11px",a:"end"});});
d3.range(0.40,0.81,0.05).forEach(v=>T(x(v),H-BOT+16,v.toFixed(2),{a:"middle",s:"9.5px",c:"#aaa"}));
T(L,H-8,"weighted correlation between a unit\\u2019s mail share and its Democratic share",{s:"10.5px",c:"#888"});
})();
</script>
', paste(sprintf("%.4f", md$correlation), collapse = ","),
   mp$correlation[1], mp$correlation[2]))

## ---- maup-static
par(mar = c(3.2, 2.6, 2.2, 1.0))
h <- hist(md$correlation, breaks = 46, plot = FALSE)
plot(NA, xlim = c(0.40, 0.80), ylim = c(0, max(h$counts) * 1.28), axes = FALSE, ann = FALSE)
rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts,
     col = adjustcolor("#999999", 0.55), border = NA)
axis(1, seq(0.40, 0.80, 0.05), cex.axis = 0.6, col = "#bbb", col.axis = "#888",
     tck = -0.02, mgp = c(2, 0.3, 0))
abline(v = mp$correlation[1], col = "#2c7fb8", lwd = 2.2)
abline(v = mp$correlation[2], col = "#4d9221", lwd = 2.2)
text(mp$correlation[1] - 0.005, max(h$counts) * 1.20,
     sprintf("the 2,653 precincts · %.3f", mp$correlation[1]), adj = 1,
     cex = 0.58, font = 2, col = "#2c7fb8")
text(mp$correlation[2] - 0.005, max(h$counts) * 1.20,
     sprintf("the 159 counties · %.3f", mp$correlation[2]), adj = 1,
     cex = 0.58, font = 2, col = "#4d9221")
title("500 random regroupings of the same precincts into 159 groups",
      cex.main = 0.84, adj = 0, line = 0.8)
mtext("weighted correlation between a unit's mail share and its Democratic share",
      side = 1, cex = 0.56, col = "#888", line = 1.6)

## ---- decision
o <- dc[, c("question", "right_rung", "why", "the_trap")]
names(o) <- c("the question", "the rung", "why that one", "the trap")
o

## ---- matrix-d3
QS <- unique(su$question); LV <- unique(su$level)
cells <- paste(sprintf('{"q":%d,"l":%d,"f":%d}',
                       match(su$question, QS) - 1, match(su$level, LV) - 1, su$fit),
               collapse = ",")
cat(sprintf('
<div id="mx" style="margin:1.1em 0"></div>
<script>
(function(){
const C=[%s],QS=[%s],LV=[%s];
const W=760,H=300,L=210,TOP=58,CW=(W-L-16)/LV.length,CH=(H-TOP-30)/QS.length;
const svg=d3.select("#mx").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const FILL={"2":"#4d9221","1":"#e08214","0":"#dddddd","-1":"#C41230"};
const OP  ={"2":0.85,"1":0.55,"0":0.55,"-1":0.20};
const T=(a,b,s,o)=>{const t=svg.append("text").attr("x",a).attr("y",b)
  .attr("text-anchor",(o&&o.a)||"start").attr("font-size",(o&&o.s)||"11px")
  .attr("fill",(o&&o.c)||"#444");if(o&&o.b)t.attr("font-weight","600");return t.text(s);};
T(8,20,"which rung answers which question",{b:1,s:"12.5px",c:"#222"});
LV.forEach((l,j)=>svg.append("text").attr("x",L+j*CW+CW/2).attr("y",TOP-8)
  .attr("transform",`rotate(-32,${L+j*CW+CW/2},${TOP-8})`)
  .attr("text-anchor","end").attr("font-size","10.5px").attr("fill","#666").text(l));
QS.forEach((q,i)=>T(L-8,TOP+i*CH+CH/2+4,q,{a:"end",s:"10.5px",c:"#444"}));
C.forEach(c=>{
  svg.append("rect").attr("x",L+c.l*CW+1).attr("y",TOP+c.q*CH+1)
    .attr("width",CW-2).attr("height",CH-2).attr("rx",2)
    .attr("fill",FILL[c.f]).attr("fill-opacity",OP[c.f]);
  if(c.f===2)svg.append("text").attr("x",L+c.l*CW+CW/2).attr("y",TOP+c.q*CH+CH/2+4)
    .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#fff")
    .attr("font-weight","700").text("\\u25cf");
  if(c.f===-1)svg.append("text").attr("x",L+c.l*CW+CW/2).attr("y",TOP+c.q*CH+CH/2+4)
    .attr("text-anchor","middle").attr("font-size","11px").attr("fill","#C41230").text("\\u00d7");
});
const key=[["the right rung","#4d9221",0.85],["usable, with care","#e08214",0.55],
           ["wrong rung","#dddddd",0.55],["impossible","#C41230",0.20]];
key.forEach((k,i)=>{svg.append("rect").attr("x",L+i*128).attr("y",H-20)
  .attr("width",13).attr("height",13).attr("rx",2).attr("fill",k[1]).attr("fill-opacity",k[2]);
  T(L+i*128+18,H-9,k[0],{s:"10px",c:"#666"});});
})();
</script>
', cells, paste(sprintf('"%s"', QS), collapse = ","),
   paste(sprintf('"%s"', LV), collapse = ",")))

## ---- matrix-static
QS <- unique(su$question); LV <- unique(su$level)
M <- matrix(su$fit[order(match(su$level, LV), match(su$question, QS))],
            nrow = length(QS), dimnames = list(QS, LV))
FILL <- c("-1" = adjustcolor("#C41230", 0.20), "0" = adjustcolor("#dddddd", 0.7),
          "1" = adjustcolor("#e08214", 0.55), "2" = adjustcolor("#4d9221", 0.85))
par(mar = c(2.4, 13.2, 4.2, 0.5))
plot(NA, xlim = c(0, length(LV)), ylim = c(length(QS), 0), axes = FALSE, ann = FALSE)
for (i in seq_along(QS)) for (j in seq_along(LV)) {
  rect(j - 0.97, i - 0.97, j - 0.03, i - 0.03,
       col = FILL[as.character(M[i, j])], border = NA)
  if (M[i, j] == 2)  points(j - 0.5, i - 0.5, pch = 19, col = "#ffffff", cex = 0.42)
  if (M[i, j] == -1) text(j - 0.5, i - 0.5, "×", col = "#C41230", cex = 0.7)
}
for (i in seq_along(QS))
  mtext(QS[i], side = 2, at = i - 0.5, las = 1, cex = 0.6, col = "#444",
        line = 0.3, adj = 1)
text(seq_along(LV) - 0.5, -0.25, LV, srt = 34, adj = 0, cex = 0.6, col = "#666", xpd = NA)
title("which rung answers which question", cex.main = 0.86, adj = 0, line = 2.9)
legend("bottom", inset = c(0, -0.17), horiz = TRUE, bty = "n", cex = 0.55,
       fill = FILL[c("2", "1", "0", "-1")], border = NA,
       legend = c("the right rung", "usable, with care", "wrong rung", "impossible"),
       xpd = NA)

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#mau text[fill="#2c7fb8" i],
#mx text[fill="#c41230" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: on the dark page that fill is lifted and already
# passes, and a --paper stroke would sit dark behind a dark ink there,
# because the checker scores the fill against the stroke it touches.
cat('<style>
#mx text[fill="#666" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#lad text[fill="#444" i],
#mx text[fill="#c41230" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
#lad text[fill="#fff" i],
#lad text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#lad text[fill="#fff" i],
#lad text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
