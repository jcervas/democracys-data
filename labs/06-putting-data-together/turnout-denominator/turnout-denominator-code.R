# turnout-denominator-code.R -- chunk bodies for turnout-denominator-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
source("../../_lib/dd-charts.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

mp <- read.csv("data/derived/mp1948.csv",     stringsAsFactors = FALSE)
na <- read.csv("data/derived/national.csv",   stringsAsFactors = FALSE)
st <- read.csv("data/derived/states.csv",     stringsAsFactors = FALSE)
tr <- read.csv("data/derived/state_trends.csv", stringsAsFactors = FALSE)
pr <- read.csv("data/derived/pairs2024.csv",  stringsAsFactors = FALSE)
ck <- read.csv("data/derived/checks.csv",     stringsAsFactors = FALSE)

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("checks.csv", "mp1948.csv", "national.csv", "overlap.csv", "pairs2024.csv", "state_trends.csv", "states.csv"))

cv <- function(k) ck$value[ck$check == k]
n  <- function(x) format(round(x), big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
sg <- function(x, k = 2) sprintf("%+.*f", k, x)

# The static twins run through base-R devices, which cannot restyle for the
# dark page the way the shared library's classes do. Light values here.
RED <- "#C41230"; BLU <- "#2c7fb8"; GRY <- "#8c8c8c"; PUR <- "#54278F"

# ---- the paper's own regression, on the paper's own numbers -----------------
# Reported per ELECTION, not per year: that is the unit the article quotes,
# and the only unit in which the two series are comparable to the published
# figure. Presidential years only -- pooling presidential and midterm turnout
# makes the slope a function of how many of each happen to fall in the window.
PRES <- mp[mp$presidential, ]
W72  <- PRES[PRES$year >= 1972, ]
fit  <- function(y, x) summary(lm(y ~ x))$coefficients[2, ] * 4
B_VAP <- fit(W72$vap_rate, W72$year)
B_VEP <- fit(W72$vep_rate, W72$year)

# ---- 2024, one row, taken apart --------------------------------------------
N24  <- na[na$YEAR == 2024, ]
NC24 <- N24$VAP * N24$NONCITIZEN_PCT / 100
GAP24 <- N24$rate_vep_tb - N24$rate_vap_tb
EXTRA <- N24$VAP * N24$rate_vep_tb / 100 - N24$TOTAL_BALLOTS_COUNTED

# ---- one state, taken apart the same way -----------------------------------
# The state rows carry no overseas line (the publisher does not spread that
# population across states), so the walk is VAP less noncitizens less felons.
TXR  <- st[st$YEAR == 2024 & st$STATE == "Texas", ]
TXNC <- TXR$VAP * TXR$NONCITIZEN_PCT / 100

# ---- the five states, and the one the section is written around ------------
FLIP  <- tr[tr$sign_flip, ]
FLIP  <- FLIP[order(FLIP$b_vap), ]
TX    <- tr[tr$state == "Texas", ]
P_SIG <- sum(c(tr$p_vap, tr$p_vep) < 0.05)      # over all 51 states

# ---- the pair the 2024 section is written around ---------------------------
TOP <- pr[1, ]

# ---- the ineligible share of the voting-age population ---------------------
# The one quantity here with no numerator in it, which is what makes it safe
# to draw across two sources that count votes differently. Both series are
# kept whole and drawn separately; they are never averaged or spliced.
SH_MP <- mp[, c("year", "inelig_pct")]
SH_EP <- data.frame(year = na$YEAR, inelig_pct = na$inelig_pct)

# ---- every state in 2024, on both denominators -----------------------------
S <- st[st$YEAR == 2024, ]
S$hl <- S$STATE %in% c(TOP$state_a, TOP$state_b)

# ---- render every data.frame in this document as a TABLE, not code output --
# These are front-facing documents. A data.frame printed the ordinary way
# comes out as a "##"-prefixed code block, which reads as machinery rather
# than as a result. Registering knit_print for data.frame turns all of them
# into real tables in HTML and PDF alike. The envir argument is required:
# without it the registration silently fails.
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- vep-def
data.frame(
  term = c("Voting-age population", "less noncitizens", "less ineligible felons",
           "plus eligible overseas", "Voting-eligible population"),
  who_that_is = c("everyone aged 18 and over living in the country",
                  "in the country, cannot vote",
                  "citizens, barred by their state over a conviction",
                  "citizens who may vote and do not live here",
                  "the people who could have cast a ballot"),
  check.names = FALSE)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# The two series, presidential years, 1948-2000, with 1972 marked. Drawn with
# the shared library (_lib/dd-charts.js): the two series share a numerator
# exactly, so a single frame with two lines is the whole comparison. dd_fig()
# emits the two <script src> tags for the document.
# ---------------------------------------------------------------------------
P <- data.frame(year = PRES$year,
                vep = round(PRES$vep_rate, 1),
                vap = round(PRES$vap_rate, 1))
dd_fig("f1", "line", P,
  size = list(w = 760, h = 430, m = list(t = 24, r = 120, b = 46, l = 52)),
  x = list(field = "year", fmt = "d", ticks = 8),
  y = list(field = "vep", label = "turnout for president",
           domain = c(44, 66), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "vep", label = "VEP  eligible", class = "series-1"),
    list(field = "vap", label = "VAP  voting-age", class = "series-2"))),
  points = TRUE, endLabels = TRUE,
  annotations = list(dd_annot_vline(1972),
                     dd_annot_text(1972, 65.4, "1972", dx = 5)),
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> voting-age: "+
        d.vap.toFixed(1)+"%<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> eligible: "+
        d.vep.toFixed(1)+"%";
  }'))

## ---- fig1-static
par(mar = c(3.6, 4.0, 1.2, 6.6))
plot(PRES$year, PRES$vep_rate, type = "n", ylim = c(44, 66),
     xlim = c(1946, 2002), xlab = "", ylab = "", axes = FALSE)
abline(v = 1972, lty = 2, col = "#bbbbbb")
axis(1, at = seq(1948, 2000, 8), cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
lines(PRES$year, PRES$vep_rate, col = BLU, lwd = 2)
lines(PRES$year, PRES$vap_rate, col = RED, lwd = 2, lty = 2)
points(PRES$year, PRES$vep_rate, pch = 19, col = BLU, cex = 0.7)
points(PRES$year, PRES$vap_rate, pch = 19, col = RED, cex = 0.7)
text(2001, tail(PRES$vep_rate, 1), "VEP", col = BLU, adj = 0,
     font = 2, cex = 0.8, xpd = NA)
text(2001, tail(PRES$vap_rate, 1), "VAP", col = RED, adj = 0,
     font = 2, cex = 0.8, xpd = NA)
text(1973, 65, "1972", col = "#888888", adj = 0, cex = 0.75)
mtext("turnout for president (%)", 2, line = 2.6, cex = 0.8)

## ---- trend-tab
data.frame(
  denominator = c("Voting-age population (VAP)", "Voting-eligible population (VEP)"),
  `1972` = c(f1(W72$vap_rate[W72$year == 1972]), f1(W72$vep_rate[W72$year == 1972])),
  `2000` = c(f1(W72$vap_rate[W72$year == 2000]), f1(W72$vep_rate[W72$year == 2000])),
  trend_per_election = c(sg(B_VAP[1], 3), sg(B_VEP[1], 3)),
  standard_error = c(f2(B_VAP[2]), f2(B_VEP[2])),
  check.names = FALSE)

## ---- fig2-d3
# The two sources drawn as two series on one frame, joined on year and left
# NA where a source has nothing: they are never averaged or spliced. The
# script tags went out with Figure 1, so this adds nothing to the payload.
I <- merge(data.frame(year = SH_MP$year, article = round(SH_MP$inelig_pct, 2)),
           data.frame(year = SH_EP$year, current = round(SH_EP$inelig_pct, 2)),
           by = "year", all = TRUE)
I <- I[order(I$year), ]
dd_fig("f2", "line", I,
  size = list(w = 760, h = 340, m = list(t = 24, r = 118, b = 44, l = 50)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "current", label = "share that cannot vote",
           domain = c(0, 10), fmt = "pct0", ticks = 5),
  series = list(fields = list(
    list(field = "article", label = "the 2001 article", class = "series-8"),
    list(field = "current", label = "the current file", class = "series-5"))),
  points = 2.6, endLabels = TRUE,
  annotations = list(dd_annot_vline(1972),
                     dd_annot_text(1972, 9.6, "1972", dx = 5)))

## ---- fig2-static
par(mar = c(3.6, 4.0, 1.2, 7.4))
plot(SH_MP$year, SH_MP$inelig_pct, type = "n", ylim = c(0, 10),
     xlim = c(1946, 2026), xlab = "", ylab = "", axes = FALSE)
abline(v = 1972, lty = 2, col = "#bbbbbb")
axis(1, at = seq(1950, 2020, 10), cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
lines(SH_MP$year, SH_MP$inelig_pct, col = GRY, lwd = 2)
lines(SH_EP$year, SH_EP$inelig_pct, col = PUR, lwd = 2)
points(SH_MP$year, SH_MP$inelig_pct, pch = 19, col = GRY, cex = 0.5)
points(SH_EP$year, SH_EP$inelig_pct, pch = 19, col = PUR, cex = 0.5)
text(tail(SH_MP$year, 1) - 1, tail(SH_MP$inelig_pct, 1) + 1.1,
     "the 2001 article", col = GRY, adj = 1, font = 2, cex = 0.7)
text(2027, tail(SH_EP$inelig_pct, 1), "the\ncurrent\nfile", col = PUR,
     adj = 0, font = 2, cex = 0.7, xpd = NA)
mtext("share of voting-age adults who cannot vote (%)", 2, line = 2.6, cex = 0.8)

## ---- decomp
data.frame(
  step = c("Voting-age population, November 2024",
           "less noncitizens",
           "less people in prison, on probation or on parole where that bars voting",
           "plus citizens living overseas who may vote",
           "Voting-eligible population"),
  people = c(n(N24$VAP), paste0("-", n(NC24)),
             paste0("-", n(N24$INELIGIBLE_FELONS_TOTAL)),
             paste0("+", n(N24$ELIGIBLE_OVERSEAS)), n(N24$VEP)))

## ---- flip-tab
data.frame(
  state = FLIP$state,
  noncitizen_share = paste0(f1(FLIP$nc_first), "% to ", f1(FLIP$nc_last), "%"),
  fitted_change_on_VAP = paste0(sg(FLIP$fit_vap, 1), " pts"),
  fitted_change_on_VEP = paste0(sg(FLIP$fit_vep, 1), " pts"),
  check.names = FALSE)

## ---- pair-tab
data.frame(
  denominator = c("Voting-age population", "Voting-eligible population"),
  a = c(paste0(f1(TOP$vap_a), "%"), paste0(f1(TOP$vep_a), "%")),
  b = c(paste0(f1(TOP$vap_b), "%"), paste0(f1(TOP$vep_b), "%")),
  `turned out more` = c(TOP$state_b, TOP$state_a),
  check.names = FALSE) |>
  setNames(c("Denominator", TOP$state_a, TOP$state_b, "Turned out more"))

## ---- fig3-d3
# Every state twice, as one point: the diagonal is where the two denominators
# would agree, and the vertical distance above it is the whole subject of this
# brief. The two states of the reversal table are called out.
J <- data.frame(state = S$STATE,
                vap = round(S$rate_vap_tb, 2),
                vep = round(S$rate_vep_tb, 2),
                grp = ifelse(S$hl, "pair", "other"),
                lbl = ifelse(S$hl, S$STATE, NA_character_),
                stringsAsFactors = FALSE)
dd_fig("f3", "scatter", J,
  size = list(w = 760, h = 430, m = list(t = 20, r = 26, b = 52, l = 56)),
  x = list(field = "vap", label = "turnout on the voting-age population",
           domain = c(42, 74), fmt = "pct0", ticks = 6),
  y = list(field = "vep", label = "turnout on the voting-eligible population",
           domain = c(42, 74), fmt = "pct0", ticks = 6),
  series = list(field = "grp",
                classes = list(pair = "series-2", other = "series-5")),
  r = 5, opacity = 0.6,
  annotations = list(dd_annot_rule(42, 42, 74, 74)),
  tip = dd_tip(c(vap = "on voting-age", vep = "on eligible"),
               fmt = c(vap = "pct1", vep = "pct1"), title = "state"))

## ---- fig3-static
par(mar = c(4.0, 4.2, 1.2, 1.4))
plot(S$rate_vap_tb, S$rate_vep_tb, type = "n", xlim = c(42, 74),
     ylim = c(42, 74), xlab = "", ylab = "", axes = FALSE)
abline(0, 1, lty = 2, col = "#cccccc")
axis(1, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
points(S$rate_vap_tb[!S$hl], S$rate_vep_tb[!S$hl], pch = 19,
       col = adjustcolor(PUR, 0.42), cex = 0.9)
points(S$rate_vap_tb[S$hl], S$rate_vep_tb[S$hl], pch = 19, col = RED, cex = 1.4)
text(S$rate_vap_tb[S$hl] + 0.7, S$rate_vep_tb[S$hl], S$STATE[S$hl],
     adj = 0, cex = 0.72, font = 2, col = RED)
mtext("turnout on the voting-age population", 1, line = 2.4, cex = 0.8)
mtext("turnout on the voting-eligible population", 2, line = 2.8, cex = 0.8)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
