# ---------------------------------------------------------------------------
# Build Florida's mid-decade redistricting tables. Companion chapter to
# ../mid-decade/ (Texas); same method as that chapter's build-data.R -- see
# that file's header for the parts common to every state.
#
# SOURCE: reconstructed precinct-level results assembled by Dave's
#   Redistricting (https://davesredistricting.org), re-aggregated there to
#   congressional districts under two different Florida plans and exported
#   one election at a time. The exports are manual browser downloads; the
#   two committed spreadsheets are the archive:
#
#   raw/FL_map_old.csv   13 statewide elections, 2016-2024, summed into the
#                        28 districts of the map enacted in 2022
#   raw/FL_map_new.csv   the SAME 13 elections summed into the 28 districts
#                        of the map enacted in 2026
#
#   Cells are the Democratic share of the two-party vote (Dem / (Dem + Rep)),
#   0 to 1. Same votes in both files; only the district lines differ.
#
# Four files come out:
#
#   derived/district_shares.csv   the long table: one row per district per
#                                 election per plan, 28 * 13 * 2 = 728 rows
#   derived/seats_by_election.csv one row per election: seats each party's
#                                 candidates would have carried under each
#                                 plan, and the Republican gain from the swap
#   derived/district_summary.csv  one row per NEW-plan district: how often
#                                 each party's candidates carried it under
#                                 each plan, and its average share under each
#   derived/facts.csv             the scalars the chapter quotes
#   derived/fl_demographics.csv   population and race/ethnicity composition
#                                 per district, both plans -- pulled straight
#                                 from the same Dave's Redistricting exports
#                                 that carry the district boundaries, since
#                                 the site (branded "Daily District" at build
#                                 time) attaches these percentages to every
#                                 district as GeoJSON properties. No new
#                                 fetch: raw/FL-2022.geojson and
#                                 raw/FL-2026.geojson already have them;
#                                 build-geo.py just never kept them.
#
# THE FLORIDA-ONLY WRINKLE: RENUMBERING. Unlike Texas, three of Florida's
# districts changed number as part of the 2026 redraw. Every "old" value
# below is looked up through CROSSWALK -- the same population-weighted,
# one-to-one pairing build-geo.py uses for the tween map -- not by matching
# district numbers directly. See that script's own comment for how the
# pairing was found (a 2020-census-block assignment problem, solved once by
# build-block-crosswalk.R) and why population beats land area for this.
#
# This script also reads derived/fl_crosswalk_pop.csv, which
# build-block-crosswalk.R writes, for the one number in facts.csv that is
# about the crosswalk itself (how much of the spotlight district's
# population its predecessor actually carries) rather than about an
# election. Run build-block-crosswalk.R at least once before this script.
#
# The geometry beside these tables is built by build-geo.py in this folder.
# Run both from inside data/. Neither touches the network.
# ---------------------------------------------------------------------------

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_report <- function() invisible(FALSE)
  prov_stamp  <- function(...) invisible(NULL)
}
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# new district id -> its population-weighted predecessor's old id
CROSSWALK <- c(`22` = 25, `23` = 22, `25` = 23)

old <- read.csv("raw/FL_map_old.csv", check.names = FALSE)
new <- read.csv("raw/FL_map_new.csv", check.names = FALSE)

elections <- setdiff(names(old), "ID")
stopifnot(setequal(names(old), names(new)),
          nrow(old) == 28, nrow(new) == 28,
          identical(sort(old$ID), 1:28), identical(sort(new$ID), 1:28))
old <- old[order(old$ID), c("ID", elections)]
new <- new[order(new$ID), c("ID", elections)]

# Shares are proportions, all strictly inside (0, 1); a 0 or a 1 would mean a
# district where one party's candidate received no votes at all, which does
# not happen in a statewide race, so it would mean a broken export.
stopifnot(all(old[elections] > 0 & old[elections] < 1),
          all(new[elections] > 0 & new[elections] < 1))

# which OLD district each NEW district is compared against
old_of_new <- new$ID
hit <- new$ID %in% as.integer(names(CROSSWALK))
old_of_new[hit] <- CROSSWALK[as.character(new$ID[hit])]
old_by_newid <- old[match(old_of_new, old$ID), ]

# --- the long table ---------------------------------------------------------
# election labels arrive as "office-year"; keep them, and split the year out
split_year <- function(e) as.integer(sub("^.*-", "", e))

long <- do.call(rbind, lapply(elections, function(e) {
  data.frame(district = new$ID, election = e, year = split_year(e),
             share_old = old_by_newid[[e]], share_new = new[[e]])
}))
stopifnot(nrow(long) == 28 * 13, !anyNA(long))
dd_write_csv(long, "derived/district_shares.csv")

# --- seats by election -------------------------------------------------------
# each plan's own 28 districts, counted independently, so the crosswalk
# cannot affect this seat count either way
seats <- data.frame(
  election = elections,
  year     = split_year(elections),
  dem_old  = colSums(old[elections] > 0.5),
  dem_new  = colSums(new[elections] > 0.5))
seats$rep_old  <- 28 - seats$dem_old
seats$rep_new  <- 28 - seats$dem_new
seats$rep_gain <- seats$rep_new - seats$rep_old
seats <- seats[order(seats$year, seats$election), ]
row.names(seats) <- NULL
# The chapter's central claim is checked here, not asserted: under every one
# of the thirteen elections the new lines elect more Republicans.
stopifnot(all(seats$rep_gain > 0))
dd_write_csv(seats, "derived/seats_by_election.csv")

# --- district summary -------------------------------------------------------
summ <- data.frame(
  district         = new$ID,
  dem_wins_old     = rowSums(old_by_newid[elections] > 0.5),
  dem_wins_new     = rowSums(new[elections] > 0.5),
  mean_old         = rowMeans(old_by_newid[elections]),
  mean_new         = rowMeans(new[elections]),
  pres2024_old     = old_by_newid[["president-2024"]],
  pres2024_new     = new[["president-2024"]])
stopifnot(all(summ$dem_wins_old <= 13), all(summ$dem_wins_new <= 13))
dd_write_csv(summ, "derived/district_summary.csv")

# --- the scalars the chapter quotes -----------------------------------------
SPOTLIGHT <- 25   # the district Larkin v. Moskowitz is being fought over
i        <- new$ID == SPOTLIGHT
spot_old <- old_by_newid[i, elections]
spot_new <- new[i, elections]
ds       <- summ[i, ]

# the share of the SPOTLIGHT district's population that actually came from
# its crosswalked predecessor -- not the predecessor's own vote share, a
# different number this chapter's prose is careful not to conflate with it
xw <- read.csv("derived/fl_crosswalk_pop.csv", stringsAsFactors = FALSE)
spot_pop_pct <- xw$pct_of_old[xw$new == SPOTLIGHT]

facts <- data.frame(
  key = c("n_districts", "n_elections", "n_rows_long",
          "rep_gain_min", "rep_gain_max",
          "rep_seats_old_pres2024", "rep_seats_new_pres2024",
          "spot_district", "spot_old_source", "spot_predecessor_pop_pct",
          "spot_pres2024_old_pct", "spot_pres2024_new_pct",
          "spot_dem_wins_old", "spot_dem_wins_new",
          "spot_bluer_elections", "spot_redder_elections",
          "n_renumbered"),
  value = c(28, length(elections), nrow(long),
            min(seats$rep_gain), max(seats$rep_gain),
            seats$rep_old[seats$election == "president-2024"],
            seats$rep_new[seats$election == "president-2024"],
            SPOTLIGHT, old_of_new[i], spot_pop_pct,
            round(100 * ds$pres2024_old, 1),
            round(100 * ds$pres2024_new, 1),
            ds$dem_wins_old, ds$dem_wins_new,
            sum(spot_new > spot_old), sum(spot_new < spot_old),
            length(CROSSWALK)))
dd_write_csv(facts, "derived/facts.csv")

# --- demographics, straight off the district exports ------------------------
# Properties only, no reprojection needed, so this reads the raw exports
# directly rather than going through build-geo.py's mapshaper pass.
demo_from_geojson <- function(path, plan) {
  gj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  props <- lapply(gj$features, `[[`, "properties")
  data.frame(
    district  = vapply(props, function(p) as.integer(p$id), 1L),
    plan      = plan,
    total_pop = vapply(props, function(p) as.integer(p$TotalPop), 1L),
    total_vap = vapply(props, function(p) as.integer(p$TotalVAP), 1L),
    white_pct = vapply(props, function(p) p$WhitePct, 1),
    black_pct = vapply(props, function(p) p$BlackPct, 1),
    hispanic_pct = vapply(props, function(p) p$HispanicPct, 1),
    asian_pct = vapply(props, function(p) p$AsianPct, 1),
    native_pct = vapply(props, function(p) p$NativePct, 1),
    pacific_pct = vapply(props, function(p) p$PacificPct, 1),
    minority_pct = vapply(props, function(p) p$MinorityPct, 1))
}
demo <- rbind(demo_from_geojson("raw/FL-2022.geojson", "old"),
              demo_from_geojson("raw/FL-2026.geojson", "new"))
demo <- demo[order(demo$plan, demo$district), ]
pct_cols <- c("white_pct", "black_pct", "hispanic_pct", "asian_pct",
              "native_pct", "pacific_pct", "minority_pct")
# Hispanic is an ethnicity, not a race, so it overlaps some of the race
# categories rather than sitting outside them -- the six race/ethnicity
# shares land close to, not exactly at, 100% of a district's population.
stopifnot(nrow(demo) == 28 * 2, !anyNA(demo),
          all(vapply(demo[pct_cols], function(x) all(x > 0 & x < 1), TRUE)),
          all(rowSums(demo[setdiff(pct_cols, c("hispanic_pct", "minority_pct"))]) +
                demo$hispanic_pct > 0.9))
dd_write_csv(demo, "derived/fl_demographics.csv")

# --- report, so the numbers can be read off the build log -------------------
cat("elections:", length(elections), " districts: 28\n")
cat("Republican seats, old map:", min(seats$rep_old), "to", max(seats$rep_old), "\n")
cat("Republican seats, new map:", min(seats$rep_new), "to", max(seats$rep_new), "\n")
cat("Republican gain from the swap:", min(seats$rep_gain), "to",
    max(seats$rep_gain), "-- positive in", sum(seats$rep_gain > 0), "of 13\n")
cat("District", SPOTLIGHT, "(old", old_of_new[i], "), president 2024: old",
    round(100 * ds$pres2024_old, 1), "new", round(100 * ds$pres2024_new, 1), "\n")

if (file.exists("../../../_lib/provenance.R")) {
  prov_report()
  prov_stamp("all")   # the committed raw captures are stamped too
}
