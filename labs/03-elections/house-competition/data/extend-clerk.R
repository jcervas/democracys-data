# ---------------------------------------------------------------------------
# Extend the House competition dataset from 1946-2014 to 1946-2024,
# using the Clerk of the House's official statistics.
#
# NO MANUAL STEP. `parse-clerk.py` downloads the Clerk PDFs and parses them;
# this script splices the result onto Jacobson and joins The Downballot's
# presidential-by-district figures so split districts extend too. Everything
# runs from a cold start with no key, no account and no guestbook.
#
#   Rscript ../data/fetch-pres-by-cd.R   # presidential by district, 2008-2024
#   python3 parse-clerk.py               # House returns, 2004-2024
#   Rscript extend-clerk.R               # this file
#
# ---------------------------------------------------------------------------
# THE VALIDATION IS THE REASON TO TRUST IT
#
# The Clerk parse and Jacobson overlap for 2004-2014 -- six elections, 2,610
# districts. `validate-clerk.R` compares them:
#
#     median |difference| in the two-party Democratic share:  0.03 points
#     districts within 0.5 points:                            98.1%-99.7%
#     districts disagreeing by more than 2 points:            11 of 2,260
#
# All eleven are explicable and none is random:
#   * LOUISIANA (4).  Jacobson reports the December runoff; the November
#     all-party ballot is what the Clerk prints under this heading. Affects
#     1-3 districts a cycle.
#   * TEXAS 22, 2006 (1).  Tom DeLay resigned and Republicans ran a write-in
#     campaign. Write-ins carry no party in this document and are excluded.
#   * NEW YORK (4).  Fusion ballot lines under a party name this parser does
#     not recognise, so a few thousand votes go unattributed.
#   * NEBRASKA 3 / CONNECTICUT 5 (2).  Small, unexplained, ~2 points.
#
# UNCONTESTED. Jacobson counts a same-party general election (California and
# Washington's top-two) as uncontested, because there is no two-party share.
# This parse flags those separately as `top_two`. Adding them back reproduces
# Jacobson closely: 2006 59 vs 59, 2008 56 vs 56, 2012 49 vs 48, 2014 78 vs 77.
# For continuity the spliced series follows Jacobson's convention.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

SOUTH_NUM <- match(c("Alabama","Arkansas","Florida","Georgia","Louisiana",
                     "Mississippi","North Carolina","South Carolina",
                     "Tennessee","Texas","Virginia"), sort(state.name))

stopifnot(file.exists("derived/clerk_house.csv"))
k   <- read.csv("derived/clerk_house.csv",  stringsAsFactors = FALSE)
jac <- read.csv("derived/races.csv",        stringsAsFactors = FALSE)

new <- k[k$year > max(jac$year), ]
cat(sprintf("Clerk rows after %d: %d races, %s\n", max(jac$year), nrow(new),
            paste(sort(unique(new$year)), collapse = ", ")))

new$dv          <- suppressWarnings(as.numeric(new$dv))
# follow Jacobson: a same-party general has no two-party share, so it is
# recorded the same way an unopposed race is -- see header
new$uncontested <- new$uncontested == 1 | new$top_two == 1
# Rounded, because subtracting near-equal numbers leaves residue that is larger
# than it looks: abs(50.51 - 50) is 0.509999999999998 in double arithmetic, and
# a margin is only ever as precise as the share it came from. `dv` here is two
# decimals; four is beyond generous and keeps every real digit.
new$margin      <- round(abs(new$dv - 50), 4)
new$competitive <- !is.na(new$margin) & new$margin <= 5
new$landslide   <- !is.na(new$margin) & new$margin >= 20
new$south       <- as.integer(new$state_num %in% SOUTH_NUM)
new$midterm     <- as.integer(new$year %% 4 == 2)
new$state       <- new$state_num
new$dvp <- NA; new$incwin <- NA

# ---- presidential by district, on the lines each election used -------------
if (file.exists("derived/pres_by_cd.csv")) {
  pb   <- read.csv("derived/pres_by_cd.csv", stringsAsFactors = FALSE)
  pair <- data.frame(year      = c(2016, 2018, 2020, 2022, 2024),
                     pres_year = c(2016, 2016, 2020, 2020, 2024),
                     lines     = c("2012-2021","2012-2021","2012-2021","2022","2024"),
                     stringsAsFactors = FALSE)
  new <- merge(new, pair, by = "year", all.x = TRUE)
  new <- merge(new, pb[, c("lines","pres_year","stcd","dpres")],
               by = c("lines","pres_year","stcd"), all.x = TRUE)
  cat(sprintf("presidential-by-district matched: %d of %d\n",
              sum(!is.na(new$dpres)), nrow(new)))
} else {
  new$dpres <- NA
  cat("pres_by_cd.csv missing -- run fetch-pres-by-cd.R for split districts\n")
}
new$split_district <- ifelse(!is.na(new$dv) & !is.na(new$dpres),
                             (new$dv > 50) != (new$dpres > 50), NA)

keep <- c("year","state","stcd","south","midterm","dv","dvp","dpres","incwin",
          "uncontested","margin","competitive","landslide","split_district")
jac$source <- "Jacobson"; new$source <- "Clerk of the House"
all <- rbind(jac[, c(keep,"source")], new[, c(keep,"source")])
write.csv(all, "derived/races.csv", row.names = FALSE)

pc  <- function(x, y) round(100 * as.vector(tapply(x, y, mean, na.rm = TRUE)), 1)
yrs <- sort(unique(all$year))
by  <- data.frame(year = yrs, races = as.vector(table(all$year)),
                  pct_uncontested = pc(all$uncontested, all$year),
                  pct_competitive = pc(all$competitive, all$year),
                  pct_landslide   = pc(all$landslide,   all$year))
s <- all[all$south == 1, ]; n <- all[all$south == 0, ]
by$pct_uncontested_south     <- pc(s$uncontested, s$year)
by$pct_uncontested_non_south <- pc(n$uncontested, n$year)
by$pct_split       <- pc(all$split_district, all$year)
by$split_coverage  <- round(100 * as.vector(tapply(!is.na(all$dpres), all$year, mean)), 1)
by$pct_split[by$split_coverage < 90] <- NA
write.csv(by, "derived/by_year.csv", row.names = FALSE)

cat(sprintf("\nwritten: %d elections, %d-%d\n", nrow(by), min(yrs), max(yrs)))
print(tail(by[, c("year","races","pct_uncontested","pct_competitive",
                  "pct_landslide","pct_split")], 8), row.names = FALSE)
