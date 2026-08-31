# ---------------------------------------------------------------------------
# Population overlap between Texas's 2021 and 2025 congressional plans, via
# 2020 census block groups.
#
#   derived/tx_overlap_pop.csv   old, new, pop, weight
#
# THE METHOD. Reduce each block group to its population-weighted center --
# the point the Census Bureau publishes as the balance point of where that
# block group's residents actually live -- then ask which district contains
# it, once against the 2021 plan and once against the 2025 plan. Block
# groups are then the common currency:
#
#     2021 district  <--  block group (pop)  -->  2025 district
#
# Summing block-group population by (old, new) pair gives the population
# living in the old district that now sits in the new one. That is
# population weighting, not area weighting: a district half farmland and
# half subdivision has nearly all its residents on one side, and area alone
# would call it an even split.
#
# HOW THIS DIFFERS FROM FLORIDA'S, AND WHAT IT COSTS. The companion Florida
# chapter runs the same idea one level finer, on all 390,066 census BLOCKS
# (see ../mid-decade-florida/data/build-block-crosswalk.R). It has to: three
# Florida districts were renumbered, so that chapter needs a defensible
# one-to-one predecessor for each district, and a block-level answer is
# worth the 746 MB download. Texas kept every district number, so nothing
# downstream depends on a predecessor being resolved exactly -- the figure
# only needs to show roughly how much of each district's population is new.
# Block groups deliver that from an 800 KB text file instead.
#
# The approximation is that a block group split by a district line is
# charged whole to whichever district holds its population center, rather
# than being divided. Texas has 18,638 block groups against 38 districts, so
# only the ones a line actually crosses can be wrong, and they are wrong by
# at most their own population. The check printed at the end quantifies it:
# every Texas district holds about 767,000 people by law, so the spread of
# the reconstructed district totals around that number is the error budget.
# Read the resulting shares as good to about a point, not to the decimal.
#
# BUILD SCRIPT -- may use packages (sf). The student lab is base R and reads
# the CSV. Run from this directory: Rscript build-bg-overlap.R
# Needs a network connection on the first run only.
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) download.file(url, dest, mode = "wb", quiet = TRUE)
  prov_report <- function() invisible(FALSE)
}
dir.create("raw", showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

# ---- fetch: TX 2020 block-group centers of population ----------------------
src <- "raw/tx_bg_centers_2020.txt"
if (!file.exists(src)) {
  cat("fetching TX 2020 block-group population centers (about 800 KB) ...\n")
  prov_fetch(paste0("https://www2.census.gov/geo/docs/reference/cenpop2020/",
                    "blkgrp/CenPop2020_Mean_BG48.txt"), src, mode = "wb", quiet = TRUE)
}

bg <- read.csv(src, colClasses = "character", fileEncoding = "UTF-8-BOM")
bg$pop <- as.numeric(bg$POPULATION)
bg$GEOID <- paste0(bg$STATEFP, bg$COUNTYFP, bg$TRACTCE, bg$BLKGRPCE)
stopifnot(nrow(bg) > 18000, !anyNA(bg$pop), !anyDuplicated(bg$GEOID))
cat(sprintf("block groups %s   population %s\n",
            format(nrow(bg), big.mark = ","), format(sum(bg$pop), big.mark = ",")))

pt <- st_as_sf(bg, coords = c("LONGITUDE", "LATITUDE"), crs = 4269)

# ---- the two enacted plans --------------------------------------------------
# File names carry the first election each plan was used for; the prose calls
# them by the year the legislature passed them (2021 and 2025).
old <- st_transform(st_make_valid(st_read("raw/TX-2022.geojson", quiet = TRUE)), 5070)
new <- st_transform(st_make_valid(st_read("raw/TX-2026.geojson", quiet = TRUE)), 5070)
pt  <- st_transform(pt, 5070)

take <- function(hits, lab) {
  i <- vapply(hits, function(x) if (length(x)) x[1] else NA_integer_, 1L)
  lab[i]
}
cat("assigning block groups to 2021 districts ...\n"); a_old <- take(st_within(pt, old), old$id)
cat("assigning block groups to 2025 districts ...\n"); a_new <- take(st_within(pt, new), new$id)

ba <- data.frame(pop = bg$pop, district_2021 = a_old, district_2025 = a_new)
cat(sprintf("placed in a 2021 district: %.2f%%   in a 2025 district: %.2f%%\n",
            100 * mean(!is.na(ba$district_2021)), 100 * mean(!is.na(ba$district_2025))))

k <- !is.na(ba$district_2021) & !is.na(ba$district_2025)
cat(sprintf("population covered by both: %.3f%%\n", 100 * sum(ba$pop[k]) / sum(ba$pop)))

# ---- the weight matrix ------------------------------------------------------
agg <- aggregate(pop ~ district_2021 + district_2025, ba[k, ], sum)
tot <- tapply(agg$pop, agg$district_2021, sum)
agg$weight <- round(agg$pop / as.numeric(tot[as.character(agg$district_2021)]), 6)
names(agg)[1:2] <- c("old", "new")
agg <- agg[order(agg$old, -agg$weight), ]
# Texas has block groups the census counted as empty (a lake, an airfield).
# One straddling a district line contributes a real (old, new) pair carrying
# nobody; it is a row about geography, not about people, so it goes.
empty <- agg$pop == 0
if (any(empty)) cat(sprintf("dropping %d (old, new) pair(s) carrying no population\n",
                            sum(empty)))
agg <- agg[!empty, ]
# Not "pop > 0" -- the line above already guarantees that, so asserting it
# again would be a check that cannot fail. The real invariant is that each
# old district's outflows account for all of it: every person it held in
# 2021 is somewhere in the 2025 map. A broken join shows up here.
share_of_old <- tapply(agg$weight, agg$old, sum)
stopifnot(nrow(agg) > 38, all(agg$weight > 0), all(agg$weight <= 1),
          length(share_of_old) == 38, all(abs(share_of_old - 1) < 0.005))
dd_write_csv(agg, "derived/tx_overlap_pop.csv")

# ---- how much the block-group approximation costs --------------------------
# Every district is drawn to the same population, so the spread of these
# reconstructed totals around the ideal is the method's own error, not a
# fact about the map. It is reported rather than hidden.
ideal <- sum(bg$pop) / 38
d_old <- tapply(ba$pop[k], ba$district_2021[k], sum)
d_new <- tapply(ba$pop[k], ba$district_2025[k], sum)
cat(sprintf("\nideal district population: %s\n", format(round(ideal), big.mark = ",")))
cat(sprintf("reconstructed 2021 districts: %s to %s (worst error %.1f%%)\n",
            format(min(d_old), big.mark = ","), format(max(d_old), big.mark = ","),
            100 * max(abs(d_old - ideal)) / ideal))
cat(sprintf("reconstructed 2025 districts: %s to %s (worst error %.1f%%)\n",
            format(min(d_new), big.mark = ","), format(max(d_new), big.mark = ","),
            100 * max(abs(d_new - ideal)) / ideal))

same <- agg[agg$old == agg$new, ]
cat(sprintf("\ndistricts keeping the most of their own people: %s\n",
            paste(sprintf("%d (%.0f%%)", head(same$old[order(-same$weight)], 4),
                          100 * head(sort(same$weight, decreasing = TRUE), 4)),
                  collapse = ", ")))
cat(sprintf("districts keeping the least: %s\n",
            paste(sprintf("%d (%.0f%%)", head(same$old[order(same$weight)], 6),
                          100 * head(sort(same$weight), 6)), collapse = ", ")))

if (file.exists("../../../_lib/provenance.R")) prov_report()
