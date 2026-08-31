# ---------------------------------------------------------------------------
# Build the mid-decade redistricting tables.
#
# SOURCE: reconstructed precinct-level results assembled by Dave's
#   Redistricting (https://davesredistricting.org), re-aggregated there to
#   congressional districts under two different Texas plans and exported one
#   election at a time. The exports are manual browser downloads; the two
#   committed spreadsheets are the archive:
#
#   raw/TX_map_old.csv   17 statewide elections, 2016-2024, summed into the
#                        38 districts of the map enacted in 2021
#   raw/TX_map_new.csv   the SAME 17 elections summed into the 38 districts
#                        of the map enacted in 2025
#
#   Cells are the Democratic share of the two-party vote (Dem / (Dem + Rep)),
#   0 to 1. Same votes in both files; only the district lines differ.
#
# Four files come out:
#
#   derived/district_shares.csv   the long table: one row per district per
#                                 election per plan, 38 * 17 * 2 = 1,292 rows
#   derived/seats_by_election.csv one row per election: seats each party's
#                                 candidates would have carried under each
#                                 plan, and the Republican gain from the swap
#   derived/district_summary.csv  one row per district: how often each
#                                 party's candidates carried it under each
#                                 plan, and its average share under each
#   derived/facts.csv             the scalars the chapter quotes
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

old <- read.csv("raw/TX_map_old.csv", check.names = FALSE)
new <- read.csv("raw/TX_map_new.csv", check.names = FALSE)

elections <- setdiff(names(old), "ID")
stopifnot(identical(names(old), names(new)),
          nrow(old) == 38, nrow(new) == 38,
          length(elections) == 17,
          identical(sort(old$ID), 1:38), identical(sort(new$ID), 1:38))
old <- old[order(old$ID), ]
new <- new[order(new$ID), ]

# Shares are proportions, all strictly inside (0, 1); a 0 or a 1 would mean a
# district where one party's candidate received no votes at all, which does
# not happen in a statewide race, so it would mean a broken export.
stopifnot(all(old[elections] > 0 & old[elections] < 1),
          all(new[elections] > 0 & new[elections] < 1))

# --- the long table ---------------------------------------------------------
# election labels arrive as "office-year"; keep them, and split the year out
split_year <- function(e) as.integer(sub("^.*-", "", e))

long <- do.call(rbind, lapply(elections, function(e) {
  data.frame(district = old$ID, election = e, year = split_year(e),
             share_old = old[[e]], share_new = new[[e]])
}))
stopifnot(nrow(long) == 38 * 17, !anyNA(long))
dd_write_csv(long, "derived/district_shares.csv")

# --- seats by election ------------------------------------------------------
seats <- data.frame(
  election = elections,
  year     = split_year(elections),
  dem_old  = colSums(old[elections] > 0.5),
  dem_new  = colSums(new[elections] > 0.5))
seats$rep_old  <- 38 - seats$dem_old
seats$rep_new  <- 38 - seats$dem_new
seats$rep_gain <- seats$rep_new - seats$rep_old
seats <- seats[order(seats$year, seats$election), ]
row.names(seats) <- NULL
# The chapter's central claim is checked here, not asserted: under every one
# of the seventeen elections the new lines elect more Republicans.
stopifnot(all(seats$rep_gain > 0))
dd_write_csv(seats, "derived/seats_by_election.csv")

# --- district summary -------------------------------------------------------
summ <- data.frame(
  district      = old$ID,
  dem_wins_old  = rowSums(old[elections] > 0.5),
  dem_wins_new  = rowSums(new[elections] > 0.5),
  mean_old      = rowMeans(old[elections]),
  mean_new      = rowMeans(new[elections]),
  pres2024_old  = old[["pres-2024"]],
  pres2024_new  = new[["pres-2024"]])
stopifnot(all(summ$dem_wins_old <= 17), all(summ$dem_wins_new <= 17))
dd_write_csv(summ, "derived/district_summary.csv")

# --- the scalars the chapter quotes -----------------------------------------
d28 <- summ[summ$district == 28, ]
facts <- data.frame(
  key = c("n_districts", "n_elections", "n_rows_long",
          "rep_gain_min", "rep_gain_max",
          "rep_seats_old_pres2024", "rep_seats_new_pres2024",
          "d28_pres2024_old_pct", "d28_pres2024_new_pct",
          "d28_dem_wins_old", "d28_dem_wins_new",
          "d28_bluer_elections", "d28_redder_elections"),
  value = c(38, 17, nrow(long),
            min(seats$rep_gain), max(seats$rep_gain),
            seats$rep_old[seats$election == "pres-2024"],
            seats$rep_new[seats$election == "pres-2024"],
            round(100 * d28$pres2024_old, 1),
            round(100 * d28$pres2024_new, 1),
            d28$dem_wins_old, d28$dem_wins_new,
            sum(new[new$ID == 28, elections] > old[old$ID == 28, elections]),
            sum(new[new$ID == 28, elections] < old[old$ID == 28, elections])))
dd_write_csv(facts, "derived/facts.csv")

# --- report, so the numbers can be read off the build log -------------------
cat("elections:", length(elections), " districts:", nrow(old), "\n")
cat("Republican seats, old map:", min(seats$rep_old), "to", max(seats$rep_old), "\n")
cat("Republican seats, new map:", min(seats$rep_new), "to", max(seats$rep_new), "\n")
cat("Republican gain from the swap:", min(seats$rep_gain), "to",
    max(seats$rep_gain), "-- positive in", sum(seats$rep_gain > 0), "of 17\n")
cat("District 28, president 2024: old", round(100 * d28$pres2024_old, 1),
    "new", round(100 * d28$pres2024_new, 1), "\n")

if (file.exists("../../../_lib/provenance.R")) {
  prov_report()
  prov_stamp("all")   # the committed raw captures are stamped too
}
