# gotv-code.R -- chunk bodies for gotv-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.4,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

g <- read.csv("data/derived/gotv_tactics.csv", stringsAsFactors = FALSE)
e <- g[g$effective, ]                      # those with a measurable effect
x <- g[!g$effective, ]                     # those without
# The rows that can be ranked three ways in Figure 1: priced per conversation or
# per piece, with both numeric columns present. A tactic measured per precinct or
# per media market has no contacts_per_vote and cannot sit on an axis of
# contacts. As the file currently stands every effective row qualifies, so this
# is identical to `e` -- but it is computed rather than assumed, because the one
# row where that stopped being true (election festivals) flipped on a judgment
# call that could be revisited.
ec <- e[!is.na(e$contacts_per_vote) & !is.na(e$cost_per_vote), ]

BUDGET  <- 250000    # the campaign budget this brief spends
CALLS   <- 16        # completed volunteer phone conversations per hour.
                     # Table 12-1's own figure, not an assumption of ours:
                     # "At $20 an hour and sixteen contacts per hour."

cpv <- function(t) g$cost_per_vote[g$tactic == t]
cnt <- function(t) g$contacts_per_vote[g$tactic == t]
# the appendix columns, for the worked arithmetic in the prose
eff  <- function(t) g$effect_pp[g$tactic == t]          # pooled effect, points
cil  <- function(t) g$ci_low[g$tactic == t]
cih  <- function(t) g$ci_high[g$tactic == t]
nst  <- function(t) g$n_studies[g$tactic == t]
cpvp <- function(t) g$contacts_per_vote_pooled[g$tactic == t]
BASE <- 40           # an illustrative control-group turnout, for that arithmetic
MAIL  <- "Direct mail, from a campaign"
ROBOX <- "Robocalls, excluding social-pressure studies"
VOL <- "Phone calls, volunteer"
CAN <- "Door-to-door canvassing"

# votes bought by spending the whole budget on one tactic
votes_all_in <- floor(BUDGET / e$cost_per_vote)

# volunteer capacity model: h hours of volunteer time, remainder spent canvassing
mix <- function(h) {
  vv   <- floor(h * CALLS / cnt(VOL))
  cost <- vv * cpv(VOL)
  if (cost >= BUDGET) return(floor(BUDGET / cpv(VOL)))
  vv + floor((BUDGET - cost) / cpv(CAN))
}
crossover <- BUDGET / cpv(VOL) * cnt(VOL) / CALLS   # hours at which money binds

# what a volunteer hour would have to be worth for canvassing to win
breakeven <- (cpv(CAN) - cpv(VOL)) * CALLS / cnt(VOL)
vol_at    <- function(w) cpv(VOL) + cnt(VOL) * w / CALLS

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
d  <- function(x) paste0("$", n(x))
d2 <- function(x) paste0("$", formatC(x, format = "f", digits = 2))

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

## ---- whole-file
o <- g[, c("tactic", "contacts_per_vote", "cost_per_vote", "reliability",
           "effective")]
names(o) <- c("tactic", "contacts per vote", "cost per vote",
              "the book's verdict", "measurable effect?")
o

## ---- whole-file-appendix
o <- g[, c("tactic", "effect_pp", "ci_low", "ci_high", "n_studies")]
names(o) <- c("tactic", "effect (pts)", "CI low", "CI high", "studies")
o

## ---- ineffective
o <- data.frame(
  tactic = x$tactic,
  # "—" where the book prints its asterisk; the robocall variant is the one row
  # here that carries a price, and it carries it because this lab worked it out.
  `cost per vote` = ifelse(is.na(x$cost_per_vote), "—", d(x$cost_per_vote)),
  `point estimate` = ifelse(is.na(x$effect_pp), "not measured",
                            paste0(pc(x$effect_pp, 3), " pts")),
  `95% CI` = ifelse(is.na(x$ci_low), "—",
                    paste0(pc(x$ci_low, 3), " to ", pc(x$ci_high, 3))),
  check.names = FALSE)
o

## ---- robo-fork
r  <- g[g$tactic == "Robocalls", ]
rx <- g[g$tactic == "Robocalls, excluding social-pressure studies", ]
data.frame(
  `estimate` = c("all nine studies", "excluding the two social-pressure studies"),
  `effect (pts)` = c(r$effect_pp, rx$effect_pp),
  `95% CI` = c(paste0(r$ci_low, " to ", r$ci_high),
               paste0(rx$ci_low, " to ", rx$ci_high)),
  `contacts per vote` = c(r$contacts_per_vote, rx$contacts_per_vote),
  `cost per vote` = c(d(r$cost_per_vote), d(rx$cost_per_vote)),
  check.names = FALSE)

## ---- slope-prep
RK <- data.frame(
  tactic = ec$tactic,
  eff    = rank(ec$contacts_per_vote),      # 1 = fewest conversations per vote
  cpc    = rank(ec$cost_per_contact),       # 1 = cheapest conversation
  cpv    = rank(ec$cost_per_vote),          # 1 = cheapest vote
  n_pv   = ec$contacts_per_vote, c_pc = ec$cost_per_contact, c_pv = ec$cost_per_vote,
  stringsAsFactors = FALSE)
RK$isvol <- RK$tactic == VOL
RK$col   <- ifelse(RK$isvol, "#C41230", "#2c7fb8")    # red = volunteer labor
RK$short <- sub("Phone calls, ", "Phone, ", RK$tactic)
RK$short <- sub("Direct mail, ", "Mail, ", RK$short)
RK$short <- sub("Door-to-door canvassing", "Canvassing, door to door", RK$short)
RK$short <- sub("commercial telemarketer", "telemarketer", RK$short)
RHO   <- cor(ec$contacts_per_vote, ec$cost_per_contact, method = "spearman")
# Everything not on the chart, and why. Two reasons, and the figure should not let
# them blur: most were found to have no measurable effect, and some could not be
# ranked against a conversation even if they had been, because they are measured
# per precinct or per media market. The second clause is built conditionally
# because whether any row falls in it depends on the effective flags, which are
# judgment calls -- see the festivals note in build-data.R.
NOTRANK <- g[!(g$tactic %in% ec$tactic), ]
NBLNK   <- sum(!NOTRANK$effective)
NUNIT   <- sum(NOTRANK$effective)
UNITNM  <- paste(tolower(NOTRANK$tactic[NOTRANK$effective]), collapse = ", ")
AREA    <- sum(is.na(NOTRANK$contacts_per_vote))
NOTE    <- paste0(NBLNK, " tactics have no rank in any column: no measurable ",
                  "effect. ", AREA, " of those are in any case measured per ",
                  "precinct or media market, not per contact.")
if (NUNIT > 0)
  NOTE <- paste0(NOTE, " ", NUNIT, " more (", UNITNM,
                 ") works but cannot be ranked against a conversation.")

## ---- slope-d3
rows <- paste0("{\"k\":\"", RK$short, "\",\"c\":\"", RK$col,
               "\",\"r\":[", RK$eff, ",", RK$cpc, ",", RK$cpv,
               "],\"v\":[\"", RK$n_pv, " contacts\",\"$",
               formatC(RK$c_pc, format = "f", digits = 2), "/contact\",\"$",
               RK$c_pv, "/vote\"]}", collapse = ",")
cat(paste0('
<div id="slp" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const COLS=["most effective first","cheapest conversation first","cheapest vote first"];
const W=770,H=360,M={t:52,r:158,b:56,l:196};
const svg=d3.select("#slp").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cx=[M.l,(M.l+W-M.r)/2,W-M.r];
const y=d3.scalePoint().domain([1,2,3,4,5]).range([M.t,H-M.b]).padding(0.2);
COLS.forEach(function(t,i){
  svg.append("text").attr("x",cx[i]).attr("y",M.t-30).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("font-weight","600").attr("fill","#333").text(t);
  svg.append("line").attr("x1",cx[i]).attr("x2",cx[i]).attr("y1",M.t-14)
    .attr("y2",H-M.b+12).attr("stroke","#ddd");
});
[1,2,3,4,5].forEach(r=>svg.append("text").attr("x",8).attr("y",y(r)+4)
  .attr("font-size","11px").attr("fill","#888").text("rank "+r));
const ln=d3.line().x((d,i)=>cx[i]).y(d=>y(d));
D.forEach(function(d){
  svg.append("path").datum(d.r).attr("d",ln).attr("fill","none")
    .attr("stroke",d.c).attr("stroke-width",2.2).attr("opacity",0.9);
  d.r.forEach(function(r,i){
    svg.append("circle").attr("cx",cx[i]).attr("cy",y(r)).attr("r",4).attr("fill",d.c);
    svg.append("text").attr("x",cx[i]).attr("y",y(r)+18).attr("text-anchor","middle")
      .attr("font-size","10px").attr("fill","#666").text(d.v[i]);
  });
  svg.append("text").attr("x",cx[0]-16).attr("y",y(d.r[0])+4).attr("text-anchor","end")
    .attr("font-size","11px").attr("fill",d.c).attr("font-weight","600").text(d.k);
  svg.append("text").attr("x",cx[2]+16).attr("y",y(d.r[2])+4)
    .attr("font-size","11px").attr("fill",d.c).text(d.k);
});
svg.append("text").attr("x",8).attr("y",H-20).attr("font-size","11px")
  .attr("fill","#666")
  .text("', NOTE, '");
svg.append("text").attr("x",8).attr("y",H-6).attr("font-size","11px")
  .attr("fill","#666")
  .text("Ranks come from point estimates. Most carry a published interval wide enough that neighbouring ranks are not distinguishable.");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Left to middle, every line crosses every other: the rank correlation between
effectiveness and cost per conversation is exactly ',
paste0(pc(RHO, 0)), '. Middle to right, the order scrambles again.</p>'))

## ---- slope-static
cx <- c(1, 2, 3)
par(mar = c(3.4, 11.4, 3.0, 9.4))
plot(NA, xlim = c(0.90, 3.10), ylim = c(5.7, 0.35), axes = FALSE, xlab = "", ylab = "")
COLS <- c("most effective\nfirst", "cheapest\nconversation first",
          "cheapest vote\nfirst")
for (i in 1:3) {
  segments(cx[i], 0.7, cx[i], 5.3, col = "#dddddd")
  text(cx[i], 0.28, COLS[i], cex = 0.7, font = 2, xpd = NA)
}
mtext(paste("rank", 1:5), side = 2, at = 1:5, line = 8.6, las = 1,
      cex = 0.62, col = "#888888")
for (j in seq_len(nrow(RK))) {
  rr <- c(RK$eff[j], RK$cpc[j], RK$cpv[j])
  lines(cx, rr, col = RK$col[j], lwd = 2.2)
  points(cx, rr, pch = 19, col = RK$col[j], cex = 0.9)
  lab <- c(paste(RK$n_pv[j], "contacts"),
           paste0("$", formatC(RK$c_pc[j], format = "f", digits = 2), "/contact"),
           paste0("$", RK$c_pv[j], "/vote"))
  # column 1 reads to the right of its dot and column 3 to the left, so the
  # value never lands on top of the tactic name
  text(cx[1] + 0.03, rr[1] + 0.21, lab[1], cex = 0.58, col = "#666666", adj = 0)
  text(cx[2],        rr[2] + 0.21, lab[2], cex = 0.58, col = "#666666")
  text(cx[3] - 0.03, rr[3] + 0.21, lab[3], cex = 0.58, col = "#666666", adj = 1)
  text(cx[1] - 0.10, rr[1], RK$short[j], adj = 1, cex = 0.68, col = RK$col[j],
       font = 2, xpd = NA)
  text(cx[3] + 0.10, rr[3], RK$short[j], adj = 0, cex = 0.68, col = RK$col[j],
       xpd = NA)
}
mtext(NOTE,
      side = 1, line = 0.9, cex = 0.66, col = "#555555")
mtext(paste("Ranks come from point estimates. Most carry a published interval",
            "wide enough that neighbouring ranks are not distinguishable."),
      side = 1, line = 1.9, cex = 0.66, col = "#555555")

## ---- spend
o <- data.frame(tactic = e$tactic, votes = n(votes_all_in))
o <- o[order(-votes_all_in), ]
names(o) <- c("if you spent the whole budget on...", "additional votes")
o

## ---- cross-prep
# The horizontal axis has to reach past the crossover or the vertical line marking
# it lands off the chart. Derived from crossover for the same reason as above.
HMAX  <- ceiling(crossover * 1.3 / 2000) * 2000
HGRID <- seq(0, HMAX, by = 100)
CAP   <- floor(BUDGET / cpv(VOL))        # votes if the headline rate were reachable
V0    <- floor(BUDGET / cpv(CAN))        # votes with no volunteers at all
VVOL  <- pmin(floor(HGRID * CALLS / cnt(VOL)), CAP)
VCAN  <- floor(pmax(BUDGET - VVOL * cpv(VOL), 0) / cpv(CAN))
VTOT  <- VVOL + VCAN
HX    <- round(crossover)

## ---- cross-d3
rows <- paste0("[", HGRID, ",", VVOL, ",", VTOT, "]", collapse = ",")
cat(paste0('
<div id="crx" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[', rows, '].map(r=>({h:r[0],v:r[1],t:r[2]}));
const HX=', HX, ', CAP=', CAP, ', V0=', V0, ', HMAX=', HMAX, ';
const W=770,H=390,M={t:22,r:150,b:52,l:64};
const svg=d3.select("#crx").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,HMAX]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,CAP*1.08]).range([H-M.b,M.t]);
const f=d3.format(",");
const a1=d3.area().x(d=>x(d.h)).y0(y(0)).y1(d=>y(d.v));
const a2=d3.area().x(d=>x(d.h)).y0(d=>y(d.v)).y1(d=>y(d.t));
svg.append("path").datum(D).attr("d",a1).attr("fill","#C41230").attr("opacity",0.85);
svg.append("path").datum(D).attr("d",a2).attr("fill","#2c7fb8").attr("opacity",0.85);
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(CAP)).attr("y2",y(CAP))
  .attr("stroke","#111").attr("stroke-dasharray","4 3");
svg.append("text").attr("x",W-M.r+6).attr("y",y(CAP)-4).attr("font-size","11px")
  .attr("fill","#111").text(f(CAP)+" votes: the rate");
svg.append("text").attr("x",W-M.r+6).attr("y",y(CAP)+9).attr("font-size","11px")
  .attr("fill","#111").text("the table advertises");
svg.append("line").attr("x1",x(HX)).attr("x2",x(HX)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#111").attr("stroke-width",1);
svg.append("text").attr("x",x(HX)-6).attr("y",M.t+12).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#111").text("volunteers are the binding constraint");
svg.append("text").attr("x",x(HX)+6).attr("y",M.t+12).attr("font-size","11px")
  .attr("fill","#111").text("money is");
svg.append("text").attr("x",x(HX)+6).attr("y",M.t+25).attr("font-size","11px")
  .attr("fill","#111").text(f(HX)+" hours");
svg.append("text").attr("x",x(2600)).attr("y",y(620)).attr("font-size","12px")
  .attr("fill","#fff").attr("font-weight","600").text("votes bought by volunteer phoning");
svg.append("text").attr("x",x(1400)).attr("y",y(V0*0.86)).attr("font-size","12px")
  .attr("fill","#fff").attr("font-weight","600").text("votes bought by canvassing");
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).ticks(7).tickFormat(f));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).ticks(6).tickFormat(f));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("volunteer hours available all cycle");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("additional votes");
svg.append("text").attr("x",M.l).attr("y",H-2).attr("font-size","10.5px")
  .attr("fill","#666").text("Assumes ', CALLS, ' completed conversations an hour and the whole ', d(BUDGET), ' spent either way.");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The blue band is money doing the work the volunteers could not. It is thickest
when there are no volunteers and disappears exactly where the campaign finally
has enough of them.</p>'))

## ---- cross-static
par(mar = c(4.6, 4.6, 1.6, 8.2))
plot(NA, xlim = c(0, HMAX), ylim = c(0, CAP * 1.08), las = 1, xlab = "",
     ylab = "", xaxs = "i", yaxs = "i", cex.axis = 0.8, yaxt = "n", xaxt = "n")
axis(1, at = seq(0, HMAX, 2000), labels = n(seq(0, HMAX, 2000)), cex.axis = 0.8)
axis(2, at = seq(0, 5000, 1000), labels = n(seq(0, 5000, 1000)), las = 1,
     cex.axis = 0.8)
mtext("volunteer hours available all cycle", 1, line = 2.4, cex = 0.85)
mtext("additional votes", 2, line = 3.2, cex = 0.85)
polygon(c(HGRID, rev(HGRID)), c(VVOL, rep(0, length(HGRID))), col = "#C41230",
        border = NA)
polygon(c(HGRID, rev(HGRID)), c(VTOT, rev(VVOL)), col = "#2c7fb8", border = NA)
abline(h = CAP, lty = 2)
abline(v = HX)
text(HMAX * 1.008, CAP, paste0(n(CAP), " votes:\nthe rate the\ntable advertises"),
     xpd = NA, adj = 0, cex = 0.66)
text(HX - 250, CAP * 1.045, "volunteers bind", adj = 1, cex = 0.68)
text(HX + 250, CAP * 1.045, paste("money binds from", n(HX), "hours"), adj = 0,
     cex = 0.68)
text(4200, 620, "votes bought by volunteer phoning", col = "white", cex = 0.76,
     font = 2)
text(2600, V0 * 0.86, "votes bought by canvassing", col = "white", cex = 0.76,
     font = 2)
mtext(paste0("Assumes ", CALLS, " completed conversations an hour and the whole ",
             d(BUDGET), " spent either way."), side = 1, line = 3.4, cex = 0.68,
      col = "#555555")

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
