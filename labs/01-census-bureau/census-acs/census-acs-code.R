# census-acs-code.R -- chunk bodies for census-acs-brief.Rmd
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
FLOOR <- win$smallest_place_published[1]
LAST3 <- max(vint$year[vint$series == "3_year" & vint$http == 200])
SENT  <- ctrl$rows_with_no_margin[1]
SENTP <- ctrl$share_with_no_margin[1]
SMALL <- marg$median_margin_pct[1]
BIG   <- marg$median_margin_pct[nrow(marg)]

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

## ---- raw
cat(paste(readLines("data/raw/arrives.txt"), collapse = "\n"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
