# ---------------------------------------------------------------------------
# Build the SECOND national wind map: 2016 -> 2020.
#
# Companion to build-data.R, which builds everything else in this folder. This
# file is separate for one reason only: build-data.R fetches a dozen upstream
# sources and rebuilds every CSV here, and the 2016 pair is additive. Running
# this script re-fetches one file and touches two outputs.
#
# WHY THIS EXISTS
#
#   One wind map of one pair of elections invites a conclusion that a second
#   map will not support. 88.5% of counties moved toward Trump between 2020 and
#   2024 and he won; 38.3% moved toward him between 2016 and 2020 and he lost.
#   The county majority happened to agree with the outcome both times -- which
#   is worth knowing mainly because it is the reverse of the lesson a single
#   map is usually wheeled out to teach, and because two cases is not a
#   finding. What the pair does establish is that the headline number moves
#   12.9 points in the earlier pair, and 1.4 in the later one, when "margin"
#   is redefined from two-party to all-votes. Drawing one map without the
#   other hides that entirely.
#
# SOURCE, STATED PLAINLY
#
#   https://raw.githubusercontent.com/MEDSL/county-returns/master/
#       countypres_2000-2016.csv
#   MIT Election Data and Science Lab, "County Presidential Election Returns
#   2000-2016", file version 20190722. Requested 2026-08-12: HTTP 200,
#   4,538,055 bytes; the 2016 slice is 9,474 rows.
#
#   WHY NOT THE REPOSITORY THE OTHER TWO YEARS COME FROM.  2020 and 2024 come
#   from Tony McGovern's compilation, and this file used to as well. It was
#   replaced because that repository's 2016 file is a PRE-CANVASS SCRAPE: it was
#   read off news results pages before the states finished counting, and the
#   late-counted ballots never made it in. The shortfall is not uniform. It sits
#   almost entirely in the vote-by-mail states, which are exactly the states
#   that count slowest -- Arizona held 79% of its final total, California 84%,
#   Utah 87%, Washington 89%, New York 91% -- while Texas and Florida, which
#   finish on the night, were within half a percent.
#
#   Scored against an independent state-level file (jaytimm/PresElectionResults,
#   the source behind ../historical-campaigns/), on the two-party margin these
#   arrows actually draw:
#
#       2016 tonmcg     median state error 0.149 pts   worst Colorado +2.19
#       2016 MEDSL      median state error 0.004 pts   worst Maine    +0.33
#       2020 tonmcg     median state error 0.004 pts
#       2024 tonmcg     median state error 0.003 pts
#
#   So the 2020 and 2024 files are fine and stay; only 2016 was wrong, and it
#   was wrong by roughly forty times the other two. MEDSL's stated source is
#   "official state election data records" -- a canvass-final compilation rather
#   than an election-night one.
#
#   THE COST OF THE SWAP, STATED TOO.  The two ends of this pair now come from
#   sources of different standing: 2016 official, 2020 a press scrape. That is
#   worth saying out loud in a chapter about provenance, and it is still the
#   right trade -- a two-point error in Colorado's 2016 margin is a real error
#   in the arrows, whereas mixed provenance is a caveat about them.
#
#   FOUR THINGS THIS FILE DOES THAT WILL BITE A NAIVE READ:
#
#   1. It is LONG, not wide: three rows per county-year (democrat, republican,
#      and an "Other" row whose party is NA). Votes are pivoted below.
#   2. FIPS is an integer, so 01001 arrives as 1001 and every state below 10 has
#      lost its leading zero. Padded below, and the padding is checked.
#   3. `totalvotes` is NOT the unit's total where a Virginia independent city
#      shares its name with a county. Fairfax County (51059) and Fairfax City
#      (51600) both carry totalvotes = 563,213, which is the two of them added
#      together. Using that column would double-count five Virginia pairs, so
#      the total here is summed from the candidate rows instead.
#   4. Kansas City, Missouri reports separately from the counties it sits in,
#      under the invented code 36000. Jackson County's own row is 173,275 votes
#      and Kansas City's is 128,601; the true county is their sum. Folded below.
#
# WHAT THIS WRITES
#
#   derived/wind_us_1620.csv   national county arrows, 2016 -> 2020, same encoding
#   derived/join_1620.csv      the unit audit for the 2016 -> 2020 join
#   derived/facts.csv          rewritten, with this script's keys added to the ones
#                      build-data.R wrote. Existing keys are never altered.
#
# Run from inside data/:  Rscript build-1620.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE, timeout = 900)

FETCH_DATE_1620 <- "2026-08-12"
OUT <- "."
DS  <- file.path("..", "..", "..", "06-putting-data-together", "data-sources", "data")

say <- function(...) cat(..., "\n", sep = "")

# ---- THE ENCODING. Sourced, not restated: encoding.R holds nothing but the
# constants and wind_geom(), so reading it costs no fetches, and the two maps
# this repository draws of the same country cannot pick up different scales.
source("encoding.R")

source("projection.R")   # albers, PRJ_US, PRJ_GA, prj, state_outline

FACTS <- list()
fact <- function(key, value) { FACTS[[key]] <<- value; invisible(value) }

# ===========================================================================
# 1.  THE 2016 RETURNS
# ===========================================================================

C16_URL <- paste0("https://raw.githubusercontent.com/MEDSL/county-returns/",
                  "master/countypres_2000-2016.csv")
say("[1] 2016 county returns: ", C16_URL)
allyr <- read.csv(C16_URL, colClasses = "character")
raw16 <- allyr[allyr$year == "2016", ]
fact("rows_2016_raw", nrow(raw16))
fact("fetch_date_2016", FETCH_DATE_1620)
fact("url_2016", C16_URL)
fact("medsl_version_2016", paste(unique(raw16$version), collapse = ","))
fact("source_2016", "MIT Election Data and Science Lab, county-returns")

raw16$votes <- as.numeric(raw16$candidatevotes)

# Units with no FIPS at all are ballots the state reports outside any county:
# Connecticut's statewide write-in pile, Maine's UOCAVA (overseas) ballots,
# Rhode Island's federal precinct, Alaska's District 99. They cannot be placed
# on a map, so they go -- but they get counted first, because "dropped" is only
# an honest word when the size of the drop is known.
nofips <- is.na(raw16$FIPS) | raw16$FIPS %in% c("", "NA")
fact("nofips_units_2016", length(unique(paste(raw16$state[nofips],
                                              raw16$county[nofips]))))
fact("nofips_votes_2016", sum(raw16$votes[nofips], na.rm = TRUE))
raw16 <- raw16[!nofips, ]

# Integer FIPS. Pad, then check that the states that lost a leading zero came
# back: Alabama is 01, and if the padding failed there would be no 01xxx rows.
raw16$fips <- sprintf("%05d", as.integer(raw16$FIPS))
stopifnot(all(nchar(raw16$fips) == 5))
stopifnot(length(unique(raw16$fips[substr(raw16$fips, 1, 2) == "01"])) == 67)

# Kansas City -> Jackson County. See note 4 in the header.
kc <- raw16$fips == "36000"
fact("kc_votes_folded_2016", sum(raw16$votes[kc], na.rm = TRUE))
raw16$fips[kc] <- "29095"

# Pivot long -> wide. total_votes is summed from the candidate rows, NOT taken
# from `totalvotes`, for the Virginia reason in note 3.
piv <- function(sel) {
  z <- tapply(raw16$votes[sel], raw16$fips[sel], sum, na.rm = TRUE)
  z[order(names(z))]
}
f_all <- sort(unique(raw16$fips))
gv <- function(z) as.numeric(z[f_all])
c16 <- data.frame(
  county_fips = f_all,
  county_name = raw16$county[match(f_all, raw16$fips)],
  state_abbr  = raw16$state_po[match(f_all, raw16$fips)],
  votes_dem   = gv(piv(raw16$party == "democrat"   & !is.na(raw16$party))),
  votes_gop   = gv(piv(raw16$party == "republican" & !is.na(raw16$party))),
  total_votes = gv(piv(rep(TRUE, nrow(raw16)))))

# Bedford City, Virginia (51515) reverted to a town inside Bedford County in
# 2013. It is still a row here, carrying NA in every vote column. Note that the
# NAs arrive as ZEROS, not as NAs: the pivot above sums with na.rm, and the sum
# of nothing is 0. So the test is for an empty unit, not for a missing one.
dead <- c16$total_votes == 0
fact("empty_units_2016", paste(c16$county_fips[dead], collapse = " "))
fact("empty_units_2016_n", sum(dead))
c16 <- c16[!dead, ]

stopifnot(!anyDuplicated(c16$county_fips))
stopifnot(all(c16$total_votes >= c16$votes_dem + c16$votes_gop))
fact("rows_2016", nrow(c16))
say("    ", nrow(c16), " unique county codes, ",
    format(sum(c16$total_votes), big.mark = ","), " votes")

c20 <- read.csv(file.path(DS, "derived/pres2020_counties.csv"),
                colClasses = c(county_fips = "character"))
stopifnot(nrow(c20) == 3152)
fact("rows_2020_for_1620", nrow(c20))

# ===========================================================================
# 2.  THE UNIT AUDIT, RUN AGAIN FOR A THIRD VINTAGE
#
#     The 2020 -> 2024 pair had three problems: Alaska pseudo-codes, DC
#     reported at two grains, Connecticut's counties replaced by planning
#     regions. A 2016 join is a THIRD vintage and does not have the same
#     three. Connecticut still used its eight counties in both 2016 and 2020,
#     so the CT loss that costs the later pair 1.8 million votes does not
#     arise here at all. Alaska is broken in a different way: the 2016 file
#     repeats the STATEWIDE total on every Alaska row, so any Alaska arrow
#     would be a copy of the same number. It is dropped, as in the later pair.
# ===========================================================================

say("[2] unit audit, 2016 vs 2020")
only16 <- setdiff(c16$county_fips, c20$county_fips)
only20 <- setdiff(c20$county_fips, c16$county_fips)
fact("only_2016", length(only16))
fact("only_2020_vs_2016", length(only20))
fact("in_both_1620", length(intersect(c16$county_fips, c20$county_fips)))

ak16 <- c16[substr(c16$county_fips, 1, 2) == "02", ]
fact("ak_2016_rows", nrow(ak16))
fact("ak_2016_distinct_totals", length(unique(ak16$total_votes)))
# Alaska is broken differently in this source than in the old one. The old file
# repeated the STATEWIDE total on every Alaska row, so the numbers were fake.
# These numbers are real -- but they are State House districts, not boroughs, so
# they have no borough FIPS to join to and no centroid to stand on. Either way
# Alaska cannot be drawn; only the reason has changed, and the reason is the
# part the chapter quotes.
fact("ak_2016_units", nrow(ak16))
fact("ak_2016_unit_kind", "State House districts, coded 02701-02740")

ct16 <- sum(substr(c16$county_fips, 1, 2) == "09")
ct20 <- sum(substr(c20$county_fips, 1, 2) == "09")
fact("ct_2016_units", ct16); fact("ct_2020_units", ct20)

jd <- rbind(
  data.frame(fips = only16, appears_in = "2016 only",
             name = c16$county_name[match(only16, c16$county_fips)],
             votes = c16$total_votes[match(only16, c16$county_fips)]),
  data.frame(fips = only20, appears_in = "2020 only",
             name = c20$county_name[match(only20, c20$county_fips)],
             votes = c20$total_votes[match(only20, c20$county_fips)]))
jd <- jd[order(jd$fips), ]
dd_write_csv(jd, file.path(OUT, "derived/join_1620.csv"))

# ===========================================================================
# 3.  HARMONIZE AND JOIN
#     Exactly the two repairs build-data.R applies to the later pair: Alaska
#     out, DC folded to one District. Connecticut needs no repair here.
# ===========================================================================

say("[3] harmonize and join")
harmonise <- function(d) {
  d <- d[substr(d$county_fips, 1, 2) != "02", ]
  dc <- d[substr(d$county_fips, 1, 2) == "11", ]
  d  <- d[substr(d$county_fips, 1, 2) != "11", ]
  if (nrow(dc)) d <- rbind(d, data.frame(
    county_fips = "11001", county_name = "District of Columbia",
    votes_dem = sum(dc$votes_dem), votes_gop = sum(dc$votes_gop),
    total_votes = sum(dc$total_votes)))
  d
}
k <- c("county_fips", "county_name", "votes_dem", "votes_gop", "total_votes")
h16 <- harmonise(c16[, k]); h20 <- harmonise(c20[, k])
fact("ak_dropped_2016", sum(substr(c16$county_fips, 1, 2) == "02"))
fact("dc_rows_folded_2016", sum(substr(c16$county_fips, 1, 2) == "11"))

w <- merge(h16[, c("county_fips", "county_name", "votes_dem", "votes_gop", "total_votes")],
           h20[, c("county_fips", "votes_dem", "votes_gop", "total_votes")],
           by = "county_fips", suffixes = c("_16", "_20"))
fact("matched_1620", nrow(w))

# What the join threw away, in units and in VOTES, measured against 2020 --
# the later election of this pair, so it is comparable to the later pair's
# figure, which is measured against 2024.
lost20 <- h20[!h20$county_fips %in% w$county_fips, ]
fact("units_lost_1620", nrow(c20) - nrow(w))
fact("votes_lost_1620",
     sum(c20$total_votes) - sum(w$total_votes_20))
fact("votes_lost_1620_pct",
     100 * (sum(c20$total_votes) - sum(w$total_votes_20)) / sum(c20$total_votes))
fact("ak_votes_lost_1620",
     sum(c20$total_votes[substr(c20$county_fips, 1, 2) == "02"]))

cen <- read.csv(file.path(OUT, "derived/county_centroids.csv"), colClasses = c(fips = "character"))
w <- merge(w, cen[, c("fips", "lon", "lat", "pop")], by.x = "county_fips", by.y = "fips")
fact("matched_with_centroid_1620", nrow(w))

w <- w[(w$votes_dem_16 + w$votes_gop_16) > 0 & (w$votes_dem_20 + w$votes_gop_20) > 0, ]
w$margin_16 <- 100 * (w$votes_gop_16 - w$votes_dem_16) / (w$votes_gop_16 + w$votes_dem_16)
w$margin_20 <- 100 * (w$votes_gop_20 - w$votes_dem_20) / (w$votes_gop_20 + w$votes_dem_20)
w$swing     <- w$margin_20 - w$margin_16
xy <- prj(w$lon, w$lat, PRJ_US); w$x <- xy$x; w$y <- xy$y
w <- cbind(w, wind_geom(w$swing, w$votes_dem_20 + w$votes_gop_20, "us"))

FRAME_US <- list(xlim = c(-2500, 2500), ylim = c(-1600, 1600))
w$in_frame <- w$x >= FRAME_US$xlim[1] & w$x <= FRAME_US$xlim[2] &
              w$y >= FRAME_US$ylim[1] & w$y <= FRAME_US$ylim[2]
fr <- w$in_frame
fact("us16_arrows", sum(fr))
fact("us16_outside_frame", sum(!fr))
fact("us16_swing_median", median(w$swing[fr]))
fact("us16_swing_min", min(w$swing[fr]))
fact("us16_swing_max", max(w$swing[fr]))
fact("us16_share_R", 100 * mean(w$swing[fr] > 0))
fact("us16_capped_len", sum(w$capped_len[fr]))
fact("us16_capped_angle", sum(w$capped_angle[fr]))

agg <- function(dm, gp) 100 * (sum(gp) - sum(dm)) / (sum(gp) + sum(dm))
fact("us16_agg_margin_16", agg(w$votes_dem_16[fr], w$votes_gop_16[fr]))
fact("us16_agg_margin_20", agg(w$votes_dem_20[fr], w$votes_gop_20[fr]))
fact("us16_agg_swing", agg(w$votes_dem_20[fr], w$votes_gop_20[fr]) -
                       agg(w$votes_dem_16[fr], w$votes_gop_16[fr]))
fact("us16_swing_unweighted", mean(w$swing[fr]))

vt <- w$votes_dem_20 + w$votes_gop_20
w$lw <- sqrt(vt) / sqrt(max(vt))
dd_write_csv(w[, c("county_fips", "county_name", "lon", "lat", "x", "y", "pop",
                "votes_dem_16", "votes_gop_16", "total_votes_16",
                "votes_dem_20", "votes_gop_20", "total_votes_20",
                "margin_16", "margin_20", "swing", "angle_deg", "len_km",
                "dx", "dy", "lw", "in_frame", "capped_len", "capped_angle")], file.path(OUT, "derived/wind_us_1620.csv"))
say("    ", sum(fr), " arrows written to wind_us_1620.csv")

# ===========================================================================
# 4.  FACTS.  Merge into the file build-data.R wrote, without disturbing it.
# ===========================================================================

new <- data.frame(key = names(FACTS),
                  value = vapply(FACTS, function(z) as.character(dd_num(z))[1], character(1)))
rownames(new) <- NULL
old <- read.csv(file.path(OUT, "derived/facts.csv"), colClasses = "character")
old <- old[!old$key %in% new$key, ]
ff  <- rbind(old, new)
dd_write_csv(ff, file.path(OUT, "derived/facts.csv"))
say("[4] facts.csv now carries ", nrow(ff), " keys (", nrow(new), " from this script)")
