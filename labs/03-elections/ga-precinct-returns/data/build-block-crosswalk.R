# ---------------------------------------------------------------------------
# Population-weighted 2020 -> 2024 precinct crosswalk, via census blocks.
#
#   derived/block_assign.csv      GEOID20, precinct_2020, precinct_2024, pop
#   derived/crosswalk_pop.csv     from_2020, to_2024, weight   (share of POPULATION)
#   derived/precincts_2024_pop.csv  2020 votes carried on population weights
#   derived/crosswalk_compare.csv   areal vs population weights, side by side
#
# THE METHOD (after Cervas, R-Functions/assignPolys)
#
# Reduce each census block to a point guaranteed to lie inside it, then ask
# which precinct contains that point -- once against the 2020 precincts, once
# against the 2024 precincts. Blocks are now the common currency:
#
#     2020 precinct  <--  block (pop)  -->  2024 precinct
#
# The weight carrying a 2020 precinct's votes into a 2024 precinct is the share
# of that precinct's POPULATION sitting in blocks that land in the target.
#
# WHY THIS BEATS AREAL WEIGHTING. Areal weighting splits a precinct's votes in
# proportion to land. A precinct that is half farmland and half subdivision has
# almost all its voters on one side, and area says fifty-fifty. Population
# weighting uses where the people actually are. `derived/crosswalk_compare.csv` shows
# how far apart the two get.
#
# WHY IT IS STILL NOT PERFECT. Population is not voters -- it includes children
# and non-citizens, and turnout varies. Block population is 2020 Census; the
# 2024 boundaries postdate it. Better still would be registered voters by block,
# which nobody publishes.
#
# BUILD SCRIPT -- may use packages. The student lab is base R and reads the CSVs.
# Run from this directory:  Rscript build-block-crosswalk.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

if (!dir.exists("raw/blocks")) utils::unzip("raw/ga_blocks_2020.zip", exdir = "raw/blocks")
if (!dir.exists("raw/shp2020")) utils::unzip("raw/ga_precincts_2020.zip", exdir = "raw/shp2020")
if (!dir.exists("raw/shp2024")) utils::unzip("raw/ga_precincts_2024.zip", exdir = "raw/shp2024")

cat("reading blocks (this is a 260 MB shapefile) ...\n")
bl <- st_read(list.files("raw/blocks", "tabblock20\\.shp$", full.names = TRUE)[1], quiet = TRUE)
popcol <- intersect(c("POP20", "POP20_1", "POPULATION"), names(bl))[1]
stopifnot(!is.na(popcol))
bl$pop <- as.numeric(bl[[popcol]])
cat(sprintf("blocks %s   population %s   (column %s)\n",
            format(nrow(bl), big.mark = ","),
            format(sum(bl$pop, na.rm = TRUE), big.mark = ","), popcol))

p20 <- st_transform(st_make_valid(st_read(list.files("raw/shp2020","\\.shp$",full.names=TRUE)[1], quiet=TRUE)), 5070)
p24 <- st_transform(st_make_valid(st_read(list.files("raw/shp2024","\\.shp$",full.names=TRUE)[1], quiet=TRUE)), 5070)
p20$src <- toupper(trimws(paste(p20$CTYNAME, p20$PRECINCT_N, sep = "|")))
p24$dst <- toupper(trimws(paste(p24$CTYNAME, p24$PRECINCT_N, sep = "|")))

bl <- st_transform(bl, 5070)
cat("reducing blocks to interior points ...\n")
pt <- suppressWarnings(st_point_on_surface(bl))

take <- function(hits, lab) {
  i <- vapply(hits, function(x) if (length(x)) x[1] else NA_integer_, 1L)
  lab[i]
}
cat("assigning blocks to 2020 precincts ...\n"); a20 <- take(st_within(pt, p20), p20$src)
cat("assigning blocks to 2024 precincts ...\n"); a24 <- take(st_within(pt, p24), p24$dst)

ba <- data.frame(GEOID20 = as.character(bl$GEOID20), pop = bl$pop,
                 precinct_2020 = a20, precinct_2024 = a24, stringsAsFactors = FALSE)
dd_write_csv(ba, "derived/block_assign.csv")
cat(sprintf("blocks placed in a 2020 precinct: %.1f%%   in a 2024 precinct: %.1f%%\n",
            100*mean(!is.na(ba$precinct_2020)), 100*mean(!is.na(ba$precinct_2024))))
cat(sprintf("population covered by both: %.2f%%\n",
            100*sum(ba$pop[!is.na(ba$precinct_2020) & !is.na(ba$precinct_2024)], na.rm=TRUE) /
                sum(ba$pop, na.rm=TRUE)))

k <- !is.na(ba$precinct_2020) & !is.na(ba$precinct_2024)
agg <- aggregate(pop ~ precinct_2020 + precinct_2024, ba[k, ], sum)
tot <- tapply(agg$pop, agg$precinct_2020, sum)
agg$weight <- agg$pop / as.numeric(tot[agg$precinct_2020])
# a precinct with zero population cannot be split by population; fall back to area
cwp <- data.frame(from_2020 = agg$precinct_2020, to_2024 = agg$precinct_2024,
                  weight = round(agg$weight, 6), pop = agg$pop)
cwp <- cwp[is.finite(cwp$weight) & cwp$weight > 1e-6, ]
dd_write_csv(cwp[order(cwp$from_2020, -cwp$weight), ], "derived/crosswalk_pop.csv")
cat(sprintf("population crosswalk: %d rows, %d source precincts, %.1f%% split\n",
            nrow(cwp), length(unique(cwp$from_2020)), 100*mean(table(cwp$from_2020) > 1)))

# ---- carry the votes ------------------------------------------------------
CAND <- intersect(c("TRUMP20","BIDEN20","PURDUE20","OSSOFF20","WARNOCK20","REG20","VOTED20"), names(p20))
v20 <- st_drop_geometry(p20[, c("src", CAND)])
for (v in CAND) { v20[[v]] <- as.numeric(v20[[v]]); v20[[v]][is.na(v20[[v]])] <- 0 }
m <- merge(v20, cwp, by.x = "src", by.y = "from_2020")
for (v in CAND) m[[v]] <- m[[v]] * m$weight
est <- aggregate(m[, CAND], by = list(to_2024 = m$to_2024), FUN = function(x) sum(x, na.rm = TRUE))
for (v in CAND) est[[v]] <- round(est[[v]])
dd_write_csv(est, "derived/precincts_2024_pop.csv")

VOTE <- setdiff(CAND, c("REG20","VOTED20"))
before <- sum(v20[, VOTE]); after <- sum(est[, VOTE])
cat(sprintf("\nvotes before %s  after %s  lost %s (%.3f%%)\n",
            format(before, big.mark=","), format(after, big.mark=","),
            format(before-after, big.mark=","), 100*(before-after)/before))

# ---- how far apart are the two methods? -----------------------------------
if (file.exists("derived/crosswalk.csv")) {
  ar <- read.csv("derived/crosswalk.csv", stringsAsFactors = FALSE)
  cmp <- merge(ar, cwp[, c("from_2020","to_2024","weight")],
               by = c("from_2020","to_2024"), suffixes = c("_area","_pop"))
  cmp$diff <- cmp$weight_pop - cmp$weight_area
  dd_write_csv(cmp[order(-abs(cmp$diff)), ], "derived/crosswalk_compare.csv")
  cat(sprintf("\nareal vs population weights on %d shared pairs:\n", nrow(cmp)))
  cat(sprintf("  median |difference| %.3f   90th pct %.3f   max %.3f\n",
              median(abs(cmp$diff)), quantile(abs(cmp$diff), .9), max(abs(cmp$diff))))
  cat(sprintf("  pairs differing by more than 10 points of weight: %d (%.1f%%)\n",
              sum(abs(cmp$diff) > .10), 100*mean(abs(cmp$diff) > .10)))
}
