# ---------------------------------------------------------------------------
# Population-weighted 2022 -> 2026 congressional district crosswalk for
# Florida, via 2020 census blocks. Same method as
# ../ga-precinct-returns/data/build-block-crosswalk.R, applied to districts
# instead of precincts, with one addition: a one-to-one assignment on top of
# the weights (see below).
#
#   derived/fl_block_assign.csv    GEOID20, district_2022, district_2026, pop
#   derived/fl_overlap_pop.csv     old, new, pop, weight   (full weight matrix)
#   derived/fl_crosswalk_pop.csv   old, new                (the 1-to-1 result)
#
# THE METHOD (after Cervas, R-Functions/assignPolys)
#
# Reduce each census block to a point guaranteed to lie inside it, then ask
# which district contains that point -- once against the 2022 plan, once
# against the 2026 plan. Blocks are now the common currency:
#
#     2022 district  <--  block (pop)  -->  2026 district
#
# Summing block population by (old, new) pair gives the population living in
# the old district that now sits in the new one -- population weighting, not
# area weighting. A district that is half farmland and half subdivision has
# almost all its residents on one side; area alone would call it 50/50.
#
# ONE STEP FURTHER THAN THE PRECINCT VERSION: a precinct crosswalk keeps every
# (old, new) pair with a nonzero weight, because a precinct's voters really
# do get split across several successor precincts. A district-to-district
# STORY (Figure 5's "2022 lines" panel, the district-8 spotlight) wants one
# answer per district: which single old district is district N's real
# predecessor. Since Florida's redraw renumbered a chain of districts, taking
# each new district's individually-best-population match, one at a time,
# does not work either -- two new districts can both want the same old one,
# leaving another old district with no match at all, and the "old lines"
# panel comes up with holes and duplicates.
#
# The fix is a ONE-TO-ONE assignment: solve_LSAP() (the Hungarian algorithm)
# finds the pairing of all 28 old districts to all 28 new districts that
# maximizes total population overlap, subject to every district being used
# exactly once on each side. That is what fl_crosswalk_pop.csv holds, and it
# is what build-geo.py and build-data.R read.
#
# BUILD SCRIPT -- may use packages (sf, clue). The student lab is base R and
# reads the CSVs. Run from this directory: Rscript build-block-crosswalk.R
# Needs a network connection for the ~340 MB Census block shapefile.
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, ...) download.file(url, dest, mode = "wb", quiet = TRUE)
}
dir.create("raw", showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)

suppressPackageStartupMessages({ library(sf); library(clue) })
sf_use_s2(FALSE)

# ---- fetch: FL 2020 census blocks, TIGER/Line (geometry + POP20) ----------
zip <- "raw/fl_blocks_2020.zip"
if (!file.exists(zip)) {
  cat("fetching FL 2020 census blocks (about 340 MB) ...\n")
  prov_fetch("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_12_tabblock20.zip",
             zip, mode = "wb", quiet = TRUE)
}
if (!dir.exists("raw/blocks")) utils::unzip(zip, exdir = "raw/blocks")

cat("reading blocks (a large shapefile; this takes a minute) ...\n")
bl <- st_read(list.files("raw/blocks", "tabblock20\\.shp$", full.names = TRUE)[1],
              query = "SELECT GEOID20, POP20 FROM tl_2020_12_tabblock20", quiet = TRUE)
bl$pop <- as.numeric(bl$POP20)
cat(sprintf("blocks %s   population %s\n",
            format(nrow(bl), big.mark = ","), format(sum(bl$pop), big.mark = ",")))

# ---- the two enacted plans --------------------------------------------------
old <- st_transform(st_make_valid(st_read("raw/FL-2022.geojson", quiet = TRUE)), 5070)
new <- st_transform(st_make_valid(st_read("raw/FL-2026.geojson", quiet = TRUE)), 5070)
bl  <- st_transform(bl, 5070)

cat("reducing blocks to interior points ...\n")
pt <- suppressWarnings(st_point_on_surface(bl))

take <- function(hits, lab) {
  i <- vapply(hits, function(x) if (length(x)) x[1] else NA_integer_, 1L)
  lab[i]
}
cat("assigning blocks to 2022 districts ...\n"); a_old <- take(st_within(pt, old), old$id)
cat("assigning blocks to 2026 districts ...\n"); a_new <- take(st_within(pt, new), new$id)

ba <- data.frame(GEOID20 = bl$GEOID20, pop = bl$pop,
                  district_2022 = a_old, district_2026 = a_new)
dd_write_csv(ba, "derived/fl_block_assign.csv")
cat(sprintf("blocks placed in a 2022 district: %.2f%%   in a 2026 district: %.2f%%\n",
            100 * mean(!is.na(ba$district_2022)), 100 * mean(!is.na(ba$district_2026))))
cat(sprintf("population covered by both: %.3f%%\n",
            100 * sum(ba$pop[!is.na(ba$district_2022) & !is.na(ba$district_2026)]) /
                sum(ba$pop)))

# ---- the full weight matrix, for the record --------------------------------
k <- !is.na(ba$district_2022) & !is.na(ba$district_2026)
agg <- aggregate(pop ~ district_2022 + district_2026, ba[k, ], sum)
tot <- tapply(agg$pop, agg$district_2022, sum)
agg$weight <- round(agg$pop / as.numeric(tot[as.character(agg$district_2022)]), 6)
names(agg)[1:2] <- c("old", "new")
dd_write_csv(agg[order(agg$old, -agg$weight), ], "derived/fl_overlap_pop.csv")

# ---- the one-to-one assignment ---------------------------------------------
# Hungarian algorithm on the 28x28 population matrix: the pairing of every
# old district to a distinct new district that maximizes total population
# carried across. Missing (old, new) pairs -- no shared blocks -- get 0.
M <- matrix(0, 28, 28, dimnames = list(1:28, 1:28))
M[cbind(as.character(agg$old), as.character(agg$new))] <- agg$pop
assign <- solve_LSAP(M, maximum = TRUE)
cw <- data.frame(old = 1:28, new = as.integer(assign))
cw$pop <- M[cbind(as.character(cw$old), as.character(cw$new))]
cw$pct_of_old <- round(100 * cw$pop / as.numeric(tot[as.character(cw$old)]), 1)
dd_write_csv(cw[order(cw$new), ], "derived/fl_crosswalk_pop.csv")

cat("\npopulation-weighted 1-to-1 crosswalk (old -> new), where it differs from same-number:\n")
moved <- cw[cw$old != cw$new, c("old", "new", "pct_of_old")]
print(moved[order(moved$new), ], row.names = FALSE)
cat(sprintf("\ntotal population carried by the assignment: %s of %s (%.2f%%)\n",
            format(sum(cw$pop), big.mark = ","), format(sum(bl$pop), big.mark = ","),
            100 * sum(cw$pop) / sum(bl$pop)))

if (file.exists("../../../_lib/provenance.R")) prov_report()
