# census-acs-code.R -- chunk bodies for census-acs-brief.Rmd
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

rd <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
inst <- rd("instrument.csv"); win  <- rd("windows.csv")
vint <- rd("vintages.csv");   ctrl <- rd("controlled.csv")
marg <- rd("margins.csv");    cmp  <- rd("compare.csv")
dvg  <- rd("diverge.csv")

nn <- function(x) format(round(as.numeric(x)), big.mark = ",")
p1 <- function(x) formatC(as.numeric(x), format = "f", digits = 1)
p2 <- function(x) formatC(as.numeric(x), format = "f", digits = 2)
CM <- function(q) cmp$value[cmp$quantity == q]

C1 <- win$counties_published[1]; C5 <- win$counties_published[2]
MISS  <- C5 - C1
MISSP <- 100 * MISS / C5
# the same two windows counted over the fifty states and DC alone, which is
# the universe the decennial chapters use; the difference is Puerto Rico
S1 <- win$counties_states_dc[1];   S5 <- win$counties_states_dc[2]
PR1 <- win$counties_puerto_rico[1]; PR5 <- win$counties_puerto_rico[2]
MISS_S  <- S5 - S1
MISSP_S <- 100 * MISS_S / S5
FLOOR <- win$smallest_place_published[1]
LAST3 <- max(vint$year[vint$series == "3_year" & vint$http == 200])
SENT  <- ctrl$rows_with_no_margin[1]
SENTP <- ctrl$share_with_no_margin[1]
SMALL <- marg$median_margin_pct[1]
BIG   <- marg$median_margin_pct[nrow(marg)]

# The worked example names the county in the first row of the divergence
# table. The name is looked up by FIPS code rather than typed beside the
# numbers, so a rebuild that reorders the table cannot put the wrong name on
# the right figures; an unlisted code falls back to the code itself.
CNAME <- c("48257" = "Kaufman County, Texas, on the east side of Dallas",
           "48397" = "Rockwall County, Texas, northeast of Dallas",
           "48091" = "Comal County, Texas, north of San Antonio",
           "48291" = "Liberty County, Texas, northeast of Houston",
           "12119" = "Sumter County, Florida, home of The Villages",
           "13157" = "Jackson County, Georgia, northeast of Atlanta",
           "48367" = "Parker County, Texas, west of Fort Worth",
           "37019" = "Brunswick County, North Carolina, on the coast")
TOPFIPS <- as.character(dvg$county_fips[1])
TOPNAME <- if (TOPFIPS %in% names(CNAME)) CNAME[[TOPFIPS]] else
           paste("county", TOPFIPS)

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

ACC <- "#1C4C5C"; WARN <- "#8A3B2C"

## ---- insttab
data.frame(The_question = inst$the_question,
           Decennial_census = inst$decennial_census,
           American_Community_Survey = inst$american_community_survey)

## ---- wintab
data.frame(Window = win$window,
           Counties_published = nn(win$counties_published),
           Smallest_place_published = nn(win$smallest_place_published),
           What_it_is = win$what_it_is)

## ---- vinttab
data.frame(Year = vint$year, Series = vint$series,
           Still_published = vint$published, HTTP = vint$http)

## ---- cmptab
data.frame(Quantity = cmp$quantity,
           Value = ifelse(cmp$unit == "%", paste0(p2(cmp$value), "%"),
                          nn(cmp$value)))

## ---- dvgtab
data.frame(County_FIPS = dvg$county_fips, State = dvg$state,
           One_year_2023 = nn(dvg$one_year_2023),
           Five_year_2019_2023 = nn(dvg$five_year_2019_2023),
           Difference_pct = paste0("+", p2(dvg$difference_pct), "%"))

## ---- ctrltab
data.frame(Table = ctrl$table,
           County_rows = nn(ctrl$county_rows),
           Rows_with_no_margin = nn(ctrl$rows_with_no_margin),
           Share = paste0(p1(ctrl$share_with_no_margin), "%"),
           Why = ctrl$why)

## ---- margtab
data.frame(Population_of_county = marg$population_of_county,
           Counties = nn(marg$counties),
           Median_income = paste0("$", nn(marg$median_income)),
           Median_margin = paste0("±$", nn(marg$median_margin)),
           Margin_as_pct = paste0("±", p1(marg$median_margin_pct), "%"))

## ---- marg-static
# The chapter's one figure: the median relative margin on median household
# income, by county size. Six ordered bands, one magnitude each -- a bar
# chart, because length against a shared baseline is the comparison the eye
# does best. Twin of marg-d3 below; keep the two in step.
op <- par(mar = c(4.0, 9.8, 0.6, 3.0), mgp = c(2.4, 0.7, 0))
v  <- rev(marg$median_margin_pct)
lb <- rev(marg$population_of_county)
bp <- barplot(v, horiz = TRUE, col = ACC, border = NA, axes = FALSE,
              names.arg = lb, las = 1, cex.names = 0.78,
              xlim = c(0, max(v) * 1.18))
axis(1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("Median margin of error on median household income, % of the estimate",
      1, line = 2.4, cex = 0.85)
text(v, bp, paste0("±", p1(v), "%"), pos = 4, cex = 0.72,
     col = "#4E5A63", xpd = NA)
par(op)

## ---- marg-d3
# Same six bars, drawn with the shared library (_lib/dd-charts.js). Hovering
# a bar gives the band's county count and the dollar margin under the
# percentage. Twin of marg-static above; keep the two in step.
mfig <- marg[, c("population_of_county", "counties", "median_income",
                 "median_margin", "median_margin_pct")]
dd_fig("margfig", "bar", mfig,
  size = list(w = 770, m = list(l = 150, r = 64)),
  rowHeight = 30,
  x = list(field = "median_margin_pct", fmt = "pct1",
           label = "median margin, % of the estimate"),
  y = list(field = "population_of_county", band = TRUE),
  valueLabels = TRUE,
  tip = dd_tip(c(counties = "counties in this band",
                 median_income = "median income, $",
                 median_margin = "median margin, $",
                 median_margin_pct = "margin as % of estimate"),
               fmt = c(counties = "comma", median_income = "comma",
                       median_margin = "comma", median_margin_pct = "pct1"),
               title = "population_of_county"))

## ---- raw
cat(paste(readLines("data/raw/arrives.txt"), collapse = "\n"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
