# candidate-positioning-code.R -- chunk bodies for candidate-positioning-brief.Rmd
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

NOM  <- read.csv("data/derived/nominees.csv",              stringsAsFactors = FALSE)
RACE <- read.csv("data/derived/races.csv",                 stringsAsFactors = FALSE)
CYC  <- read.csv("data/derived/by_cycle.csv",              stringsAsFactors = FALSE)
STAT <- read.csv("data/derived/by_status.csv",             stringsAsFactors = FALSE)
GAPD <- read.csv("data/derived/gap_by_district.csv",       stringsAsFactors = FALSE)
REPL <- read.csv("data/derived/replacements.csv",          stringsAsFactors = FALSE)
DEC  <- read.csv("data/derived/replacement_by_decade.csv", stringsAsFactors = FALSE)
RESP <- read.csv("data/derived/responsiveness.csv",        stringsAsFactors = FALSE)
P96  <- read.csv("data/derived/paper_1996.csv",            stringsAsFactors = FALSE)
FA   <- read.csv("data/derived/facts.csv",                 stringsAsFactors = FALSE)
CK   <- read.csv("data/derived/checks.csv",                stringsAsFactors = FALSE)

dd_derived(c("by_cycle.csv", "by_status.csv", "checks.csv", "facts.csv",
             "gap_by_district.csv", "nominees.csv", "paper_1996.csv", "races.csv",
             "replacement_by_decade.csv", "replacements.csv", "responsiveness.csv"))

F  <- function(k) FA$value[FA$key == k]
FN <- function(k) as.numeric(F(k))
n  <- function(x) format(as.numeric(x), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
r2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)

DEM <- "#2166ac"; REP <- "#b2182b"

# 2024, the figure year: nominees with a usable score and a presidential vote
N24 <- NOM[NOM$cycle == 2024 & !is.na(NOM$position) & !is.na(NOM$rep_pres), ]
R24 <- RACE[RACE$cycle == 2024, ]
fit <- function(p) coef(lm(position ~ rep_pres, data = N24[N24$party == p, ]))
FD <- fit("Democrat"); FR <- fit("Republican")
XL <- range(N24$rep_pres)

# The one 1996 race the published paper names as its exception
X96 <- RACE[RACE$cycle == 1996 & RACE$gap <= 0, ]

# Figure 3's table, one row per decade; a missing region-decade stays NA so
# the line breaks there instead of bridging the gap
w3 <- data.frame(decade = sort(unique(DEC$decade)))
g3 <- function(r) { v <- DEC[DEC$region == r, ]; round(v$mean_shift[match(w3$decade, v$decade)], 3) }
w3$all <- g3("All districts"); w3$out <- g3("Outside the South"); w3$south <- g3("South")

# Figure 4's table, one row per election, both sources side by side
rr <- RESP[RESP$year >= 1952, ]
w4 <- data.frame(year = sort(unique(rr$year)))
get4 <- function(s, p) { v <- rr[rr$source == s & rr$party == p, ]; round(v$shift_40_60[match(w4$year, v$year)], 3) }
SV <- "Roll calls (Voteview), winners only"; SD <- "Campaign donors (DIME), all nominees"
w4$vd <- get4(SV, "Democrat"); w4$vr <- get4(SV, "Republican")
w4$dd <- get4(SD, "Democrat"); w4$dr <- get4(SD, "Republican")

## ---- fig1-d3
pts <- data.frame(rep_pres = round(N24$rep_pres, 1), position = round(N24$position, 3),
                  party = N24$party,
                  who = sprintf("%s (%s-%02d)", tools::toTitleCase(tolower(N24$candidate)),
                                N24$state, N24$district),
                  status = N24$status)
ln <- function(cf, cls) dd_annot_rule(XL[1], cf[1] + cf[2] * XL[1], XL[2], cf[1] + cf[2] * XL[2], class = cls)
dd_fig("fig1", "scatter", pts,
  size = list(w = 770, h = 440, m = list(t = 18, r = 24, b = 48, l = 60)),
  x = list(field = "rep_pres", label = "Republican share of the district's 2024 presidential vote (%)",
           domain = c(10, 90), fmt = "d", ticks = 8),
  y = list(field = "position", label = "position (Democratic average = 0, Republican = 1)",
           domain = c(-0.8, 1.8), fmt = "f1", ticks = 6),
  series = list(field = "party", classes = list(Democrat = "dem", Republican = "gop")),
  r = 3.2, opacity = 0.55, legend = TRUE,
  annotations = list(dd_annot_hline(0), dd_annot_hline(1), ln(FD, "dem"), ln(FR, "gop")),
  tip = dd_tip(c(status = "status", rep_pres = "Republican presidential %", position = "position"),
               fmt = c(rep_pres = "f1", position = "f2"), title = "who"))

## ---- fig1-static
par(mar = c(4.2, 4.4, 0.8, 0.8), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(10, 90), ylim = c(-0.8, 1.8), las = 1,
     xlab = "Republican share of the district's 2024 presidential vote (%)",
     ylab = "position (Democratic avg. = 0, Republican = 1)")
abline(h = c(0, 1), lty = 3, col = "grey60")
d <- N24[N24$party == "Democrat", ];   points(d$rep_pres, d$position, pch = 16, cex = 0.55, col = paste0(DEM, "88"))
r <- N24[N24$party == "Republican", ]; points(r$rep_pres, r$position, pch = 16, cex = 0.55, col = paste0(REP, "88"))
segments(XL[1], FD[1] + FD[2] * XL[1], XL[2], FD[1] + FD[2] * XL[2], col = DEM, lwd = 2)
segments(XL[1], FR[1] + FR[2] * XL[1], XL[2], FR[1] + FR[2] * XL[2], col = REP, lwd = 2)
legend("topleft", c("Democratic nominee", "Republican nominee"), pch = 16,
       col = c(DEM, REP), bty = "n", cex = 0.85)

## ---- fig2-d3
pr <- data.frame(dem = round(R24$dem_position, 3), rep = round(R24$rep_position, 3),
                 who = sprintf("%s-%02d: %s vs %s", R24$state, R24$district,
                               tools::toTitleCase(tolower(R24$dem_candidate)),
                               tools::toTitleCase(tolower(R24$rep_candidate))),
                 seat = R24$seat, gap = round(R24$gap, 2))
dd_fig("fig2", "scatter", pr,
  size = list(w = 770, h = 520, m = list(t = 18, r = 24, b = 48, l = 60)),
  x = list(field = "dem", label = "the Democrat's position", domain = c(-0.8, 1.8), fmt = "f1", ticks = 6),
  y = list(field = "rep", label = "the Republican's position", domain = c(-0.8, 1.8), fmt = "f1", ticks = 6),
  r = 3.4, opacity = 0.6,
  annotations = list(dd_annot_rule(-0.8, -0.8, 1.8, 1.8),
                     dd_annot_text(1.15, 0.95, "the two candidates in the same place", class = "foot")),
  tip = dd_tip(c(seat = "seat", dem = "Democrat", rep = "Republican", gap = "distance apart"),
               fmt = c(dem = "f2", rep = "f2", gap = "f2"), title = "who"))

## ---- fig2-static
par(mar = c(4.2, 4.4, 0.8, 0.8), mgp = c(2.6, 0.7, 0), pty = "s")
plot(R24$dem_position, R24$rep_position, pch = 16, cex = 0.6, col = "#44444477",
     xlim = c(-0.8, 1.8), ylim = c(-0.8, 1.8), las = 1,
     xlab = "the Democrat's position", ylab = "the Republican's position")
abline(0, 1, lty = 2, col = "grey40")
text(1.15, 0.95, "the two candidates\nin the same place", cex = 0.75, col = "grey30", adj = c(0, 1))

## ---- p96-table
knit_print.data.frame <- function(x, ...) {
  knitr::knit_print(knitr::kable(x, row.names = FALSE, align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))
cnt <- grepl("^Races", P96$quantity)          # the two counts print as whole numbers
data.frame(` ` = P96$quantity,
           `The paper, 1996 survey` = ifelse(cnt, n(P96$paper), r2(P96$paper)),
           `Donors, 1996` = ifelse(cnt, n(P96$dime), r2(P96$dime)),
           check.names = FALSE)

## ---- status-table
st <- STAT[STAT$era %in% c("1980-1998", "2012-2024"), ]
cell <- function(e, p, s) r2(st$toward_other_party[st$era == e & st$party == p & st$status == s])
data.frame(
  Candidate = c("Incumbent", "Open-seat candidate", "Challenger"),
  `Democrats, 1980-1998` = sapply(c("incumbent", "open seat", "challenger"), cell, e = "1980-1998", p = "Democrat"),
  `Democrats, 2012-2024` = sapply(c("incumbent", "open seat", "challenger"), cell, e = "2012-2024", p = "Democrat"),
  `Republicans, 1980-1998` = sapply(c("incumbent", "open seat", "challenger"), cell, e = "1980-1998", p = "Republican"),
  `Republicans, 2012-2024` = sapply(c("incumbent", "open seat", "challenger"), cell, e = "2012-2024", p = "Republican"),
  check.names = FALSE, row.names = NULL)

## ---- fig3-d3
dd_fig("fig3", "line", w3,
  size = list(w = 770, h = 400, m = list(t = 18, r = 140, b = 46, l = 60)),
  x = list(field = "decade", label = "decade of the election", fmt = "d", ticks = 8),
  y = list(field = "all", label = "change in the member's position", domain = c(0.5, 1.5), fmt = "f1", ticks = 5),
  series = list(fields = list(
    list(field = "all",   label = "All districts",     class = "series-5"),
    list(field = "south", label = "South",             class = "series-4", width = 1.6),
    list(field = "out",   label = "Outside the South", class = "series-6", width = 1.6))),
  points = TRUE, endLabels = TRUE,
  annotations = list(list(type = "hline", y = 1, class = "zero", dash = FALSE),
                     dd_annot_text(1875, 1, "as far as the national parties are apart",
                                   class = "foot", dy = -6)),
  tip = dd_js('function(d){
    return "<b>"+d.decade+"s</b><br>All districts: "+d.all.toFixed(2)+
      "<br>South: "+(d.south==null?"—":d.south.toFixed(2))+
      "<br>Outside the South: "+d.out.toFixed(2);
  }'))

## ---- fig3-static
par(mar = c(4.2, 4.4, 0.8, 7.5), mgp = c(2.6, 0.7, 0), xpd = FALSE)
cols <- c("All districts" = "#8856A7", "South" = "#A4590F", "Outside the South" = "#00857C")
plot(NA, xlim = range(DEC$decade), ylim = c(0.5, 1.5), las = 1,
     xlab = "decade of the election", ylab = "change in the member's position")
abline(h = 1, col = "grey55")
fld <- c("All districts" = "all", "South" = "south", "Outside the South" = "out")
ly <- sapply(fld, function(f) tail(w3[[f]], 1))
o <- order(ly); for (i in 2:3) if (ly[o[i]] - ly[o[i - 1]] < 0.06) ly[o[i]] <- ly[o[i - 1]] + 0.06
for (g in names(cols)) {
  v <- w3[[fld[g]]]
  lines(w3$decade, v, col = cols[g], lwd = ifelse(g == "All districts", 2.5, 1.5))
  points(w3$decade, v, col = cols[g], pch = 16, cex = 0.6)
  par(xpd = TRUE); text(max(w3$decade) + 4, ly[g], g, col = cols[g], adj = 0, cex = 0.75); par(xpd = FALSE)
}

## ---- fig4-d3
dd_fig("fig4", "line", w4,
  size = list(w = 770, h = 420, m = list(t = 18, r = 150, b = 46, l = 60)),
  x = list(field = "year", label = "election", fmt = "d", ticks = 8),
  y = list(field = "vd", label = "shift from a 40% to a 60% Republican district",
           domain = c(-0.15, 0.5), fmt = "f1", ticks = 6),
  series = list(fields = list(
    list(field = "vd", label = "Democrats, roll calls",   class = "dem"),
    list(field = "vr", label = "Republicans, roll calls", class = "gop"),
    list(field = "dd", label = "Democrats, donors",       class = "dem", width = 1.4),
    list(field = "dr", label = "Republicans, donors",     class = "gop", width = 1.4))),
  endLabels = TRUE,
  annotations = list(list(type = "hline", y = 0, class = "zero", dash = FALSE)),
  hook = dd_js('function(fig){
    fig.svg.selectAll("path.line").filter(function(d,i){ return i >= 2; })
      .attr("stroke-dasharray", "5 4");
  }'),
  tip = dd_js('function(d){
    var f=function(v){ return v==null ? "—" : v.toFixed(2); };
    return "<b>"+d.year+"</b><br>Roll calls: Democrats "+f(d.vd)+", Republicans "+f(d.vr)+
      "<br>Donors: Democrats "+f(d.dd)+", Republicans "+f(d.dr);
  }'))

## ---- fig4-static
par(mar = c(4.2, 4.4, 0.8, 8.5), mgp = c(2.6, 0.7, 0))
plot(NA, xlim = c(1952, 2024), ylim = c(-0.15, 0.5), las = 1,
     xlab = "election", ylab = "shift, 40% to 60% Republican district")
abline(h = 0, col = "grey55")
lines(w4$year, w4$vd, col = DEM, lwd = 2.5); lines(w4$year, w4$vr, col = REP, lwd = 2.5)
lines(w4$year, w4$dd, col = DEM, lwd = 1.4, lty = 2); lines(w4$year, w4$dr, col = REP, lwd = 1.4, lty = 2)
par(xpd = TRUE)
lab <- data.frame(y = c(tail(w4$vd, 1), tail(w4$vr, 1), tail(w4$dd, 1), tail(w4$dr, 1)),
                  t = c("Democrats, roll calls", "Republicans, roll calls", "Democrats, donors", "Republicans, donors"),
                  c = c(DEM, REP, DEM, REP))
lab <- lab[order(lab$y), ]
for (i in 2:nrow(lab)) if (lab$y[i] - lab$y[i - 1] < 0.035) lab$y[i] <- lab$y[i - 1] + 0.035
text(2026, lab$y, lab$t, col = lab$c, adj = 0, cex = 0.72)
par(xpd = FALSE)

## ---- checks-table
knit_print.data.frame <- function(x, ...) {
  knitr::knit_print(knitr::kable(x, row.names = FALSE, align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))
data.frame(Check = CK$check, Found = CK$value, Expected = CK$expected,
           Passed = ifelse(CK$passed, "yes", "NO"), check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
