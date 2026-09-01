# validated-turnout-code.R -- chunk bodies for validated-turnout-brief.Rmd
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

cps <- read.csv("data/derived/cps_turnout.csv", stringsAsFactors = FALSE)
cps <- cps[order(cps$year), ]
cps$gap_people <- cps$reported_voters - cps$actual_votes
cps$gap_pct    <- 100 * cps$gap_people / cps$actual_votes

Y1 <- min(cps$year); Y2 <- max(cps$year)
g   <- function(yr, col) cps[[col]][cps$year == yr]
pc  <- function(x, k = 1) formatC(x, format = "f", digits = k)
cnt <- function(x) format(round(x), big.mark = ",")
mn  <- function(x, k = 1) formatC(x / 1e6, format = "f", digits = k)

era <- cut(cps$year, c(1963, 1979, 1999, 2030),
           labels = c("1964–1976", "1980–1996", "2000–2024"))
em  <- tapply(cps$gap_pct, era, mean)

# The static twins run through base-R devices, which cannot restyle for the
# dark page the way the shared library's classes do. Light values here.
RED <- "#C41230"; BLU <- "#2c7fb8"

# the by-group table, as five elections rather than one
gnm  <- c("White (non-Hispanic)", "Black", "Asian", "Hispanic")
gvar <- c("pct_white_nh", "pct_black", "pct_asian", "pct_hispanic")
gcol <- c("#4d9221", "#C41230", "#2c7fb8", "#8856a7")
rg   <- cps[cps$year >= 2008, c("year", gvar)]
rg_sp <- function(y) max(as.numeric(rg[rg$year == y, gvar])) -
                     min(as.numeric(rg[rg$year == y, gvar]))

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- one-record
o <- cps[cps$year == 2016,
         c("year", "vap_thousands", "pct_citizen_basis", "reported_voters",
           "citizen_vap", "actual_votes")]
o$vap_thousands   <- cnt(o$vap_thousands * 1000)
o$reported_voters <- cnt(o$reported_voters)
o$citizen_vap     <- cnt(o$citizen_vap)
o$actual_votes    <- cnt(o$actual_votes)
names(o) <- c("year", "voting-age population", "% voted, citizen basis",
              "said they voted", "citizen VAP", "ballots for president")
o

## ---- band-static
par(mar = c(3.6, 4.8, 1.0, 1.4))
rv <- cps$reported_voters / 1e6; av <- cps$actual_votes / 1e6
plot(NA, xlim = range(cps$year), ylim = c(60, max(rv) * 1.06), las = 1,
     xlab = "", ylab = "millions of people")
polygon(c(cps$year, rev(cps$year)), c(rv, rev(av)),
        col = adjustcolor(RED, alpha.f = 0.22), border = NA)
lines(cps$year, rv, lwd = 2.4, col = RED)
lines(cps$year, av, lwd = 2.4, col = BLU)
points(cps$year, rv, pch = 19, cex = 0.7, col = RED)
points(cps$year, av, pch = 19, cex = 0.7, col = BLU)
legend("topleft", c("said they voted", "ballots cast for president"),
       lwd = 2.4, col = c(RED, BLU), bty = "n", cex = 0.8, seg.len = 1.6)
wy <- which.max(cps$gap_people)
arrows(cps$year[wy], av[wy], cps$year[wy], rv[wy], code = 3, length = 0.05,
       angle = 90, col = "grey25")
text(cps$year[wy], (rv[wy] + av[wy]) / 2, paste0(" ", mn(max(cps$gap_people)),
     "m"), pos = 4, cex = 0.76, col = "grey20")

## ---- band-d3
# Drawn with the shared library (_lib/dd-charts.js): a two-series line with
# the region between the series shaded, which is `band` in DD.fig(). dd_fig()
# emits the two <script src> tags for the document.
B <- data.frame(year     = cps$year,
                reported = round(cps$reported_voters / 1e6, 3),
                counted  = round(cps$actual_votes / 1e6, 3),
                gap      = round(cps$gap_people),
                gap_pct  = round(cps$gap_pct, 2))
dd_fig("band", "line", B,
  size = list(w = 770, h = 430, m = list(t = 18, r = 26, b = 44, l = 60)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "reported", label = "millions of people",
           domain = c(60, max(B$reported) * 1.06), ticks = 6),
  band = list(y0 = "counted", y1 = "reported"),
  series = list(fields = list(
    list(field = "reported", label = "said they voted", class = "series-2"),
    list(field = "counted", label = "ballots cast for president",
         class = "series-1"))),
  points = TRUE, legend = TRUE,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>"+
      "<span class=\'series-2-txt\'>&#9632;</span> said they voted: "+
        d.reported.toFixed(1)+"m<br>"+
      "<span class=\'series-1-txt\'>&#9632;</span> ballots: "+
        d.counted.toFixed(1)+"m<br>gap "+
        d.gap.toLocaleString("en-US")+" ("+d.gap_pct.toFixed(1)+"%)";
  }'))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the figure for an election-by-election readout.</p>')

## ---- gap-static
plot(cps$year, cps$gap_pct, type = "b", pch = 19, lwd = 2.2, col = RED,
     ylim = c(-2, 15), las = 1, xlab = "",
     ylab = "Survey electorate over ballot count (%)")
abline(h = 0, lwd = 1)
segments(1964, em[1], 1976, em[1], col = "grey45", lty = 2, lwd = 2)
segments(1980, em[2], 1996, em[2], col = "grey45", lty = 2, lwd = 2)
segments(2000, em[3], 2024, em[3], col = "grey45", lty = 2, lwd = 2)
text(c(1970, 1988, 2012), c(em[1], em[2], em[3]) + 1.3,
     paste0(formatC(as.vector(em), format = "f", digits = 1), "%"),
     col = "grey35", cex = 0.85)

## ---- gap-d3
# The same series as the static twin, on the shared library. The era averages
# are annotations rather than a second data series: they are summaries of the
# line, and drawing them as rules says so.
G <- data.frame(year = cps$year, gap_pct = round(cps$gap_pct, 2),
                gap = round(cps$gap_people), ballots = cps$actual_votes)
ea <- c(1964, 1980, 2000); eb <- c(1976, 1996, 2024)
ann <- c(list(dd_annot_hline(0, dash = FALSE)),
         lapply(seq_len(3), function(i)
           dd_annot_rule(ea[i], em[[i]], eb[i], em[[i]])),
         lapply(seq_len(3), function(i)
           dd_annot_text((ea[i] + eb[i]) / 2, em[[i]],
                         paste0(pc(em[[i]]), "% average"),
                         anchor = "middle", dy = -8)))
dd_fig("cpsgap", "line", G,
  size = list(w = 770, h = 430, m = list(t = 20, r = 24, b = 42, l = 58)),
  x = list(field = "year", fmt = "d", ticks = 9),
  y = list(field = "gap_pct", label = "survey electorate over ballot count",
           domain = c(-2, 15), fmt = "pct0", ticks = 7),
  series = list(fields = list(
    list(field = "gap_pct", label = "over-report", class = "series-2"))),
  points = TRUE, annotations = ann,
  tip = dd_js('function(d){
    return "<b>"+d.year+"</b><br>over-report "+
      (d.gap_pct>0?"+":"")+d.gap_pct.toFixed(1)+"%<br>"+
      d.gap.toLocaleString("en-US")+" people<br>"+
      d.ballots.toLocaleString("en-US")+" ballots";
  }'))

## ---- by-race
o <- cps[cps$year >= 2008,
         c("year", "pct_white_nh", "pct_black", "pct_asian", "pct_hispanic")]
o <- o[order(-o$year), ]
names(o) <- c("year", "White (non-Hispanic)", "Black", "Asian", "Hispanic")
o

## ---- slope-static
par(mar = c(3.4, 4.6, 1.0, 10.4))
plot(NA, xlim = c(min(rg$year), max(rg$year)), ylim = c(40, 76), las = 1,
     xlab = "", ylab = "reported turnout among citizens (%)", xaxt = "n")
axis(1, at = rg$year, labels = rg$year, cex.axis = 0.9)
abline(h = seq(40, 75, 5), col = "grey93")
for (k in seq_along(gvar)) {
  lines(rg$year, rg[[gvar[k]]], lwd = 2.6, col = gcol[k])
  points(rg$year, rg[[gvar[k]]], pch = 19, cex = 0.8, col = gcol[k])
  text(max(rg$year), rg[[gvar[k]]][rg$year == max(rg$year)],
       paste0(" ", gnm[k], " ", pc(rg[[gvar[k]]][rg$year == max(rg$year)])),
       pos = 4, cex = 0.74, col = gcol[k], xpd = NA)
}

## ---- slope-d3
# Four groups, five elections, on the shared library. The script tags were
# emitted at Figure 1, so this call draws and adds nothing to the payload.
R <- rg[order(rg$year), ]
dd_fig("slp", "line", R,
  size = list(w = 770, h = 400, m = list(t = 20, r = 186, b = 44, l = 58)),
  x = list(field = "year", fmt = "d", ticks = 5),
  y = list(field = "pct_white_nh", label = "reported turnout among citizens",
           domain = c(40, 76), fmt = "pct0", ticks = 6),
  series = list(fields = list(
    list(field = "pct_white_nh", label = gnm[1], class = "series-3"),
    list(field = "pct_black",    label = gnm[2], class = "series-2"),
    list(field = "pct_asian",    label = gnm[3], class = "series-1"),
    list(field = "pct_hispanic", label = gnm[4], class = "series-5"))),
  points = TRUE, endLabels = TRUE)
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the figure to read all four groups off at one election.</p>')

## ---- mechanisms
data.frame(
  mechanism = c("Over-claiming", "Who answers at all", "Our own construction"),
  what_it_is = c(
    "Respondents misremember, or say what they think they should have done",
    "The people who agree to a government interview are unusually civic",
    "Derived voter counts, and ballots for president rather than all ballots"),
  which_way = c("inflates the gap", "either direction", "inflates the gap"),
  can_we_see_it = c("No", "No", "Partly"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
