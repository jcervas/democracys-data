# ---------------------------------------------------------------------------
# Build the wind-map lab datasets.
#
# FETCH DATE: 2026-08-10.  Every URL below was requested on that date and the
# response code recorded next to it.  Nothing is typed in by hand.
#
# WHAT THIS BUILDS
#
#   derived/county_centroids.csv     3,221 county population-weighted centroids
#   derived/provenance_audit.csv       per-unit audit of the two national county files
#   derived/audit_summary.csv          the audit collapsed to counts
#   derived/wind_us.csv              3,104 national county arrows, 2020 -> 2024
#   derived/us_outline.csv             projected state outlines for the national map
#   derived/wind_ga.csv                159 Georgia county arrows, 2020 -> 2024,
#                              official Secretary of State returns both years
#   derived/ga_outline.csv             projected Georgia county outlines
#   derived/precinct_join_ladder.csv   what each repair to the precinct name join buys
#   derived/precinct_join_residual.csv where the precinct join still fails, by county
#   derived/office_gap.csv           2,641 Georgia precincts: president vs senate on
#                              the SAME ballots, November 2020
#   derived/wind_ga_counties.csv       159 Georgia counties, 2022 governor baseline,
#                              with the 2026 columns left empty on purpose
#   derived/wind_input_2026.csv        THE ONE FILE THAT CHANGES ON CLASS DAY (a stub)
#   derived/facts.csv                  every scalar the three documents quote
#   derived/parity.csv                 D3 / base-R agreement check
#
# THE 2016 -> 2020 PAIR IS BUILT BY build-1620.R, BESIDE THIS FILE. It fetches
# one more year from the same repository and writes wind_us_1620.csv,
# derived/join_1620.csv, and its own keys into facts.csv. Run it after this script;
# it merges its facts into the file this one writes rather than replacing it.
#
# HOW TO REFRESH FOR 2026 -- see build_2026() at the bottom of this file.
# One function call.  Nothing else in this script changes.
#
# PACKAGES.  Build scripts may use packages; the student-facing .Rmd may not.
# Uses: sf (Georgia geometry), maps (national state outline), jsonlite (the
# Secretary of State export).  The .Rmd files read only the CSVs written here
# and use base R alone.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE, timeout = 900)
suppressMessages({library(sf); library(maps); library(jsonlite)})

FETCH_DATE <- "2026-08-10"
OUT <- "."                       # run this script from inside data/
SIB <- ".."                      # ../.. is the labs root
GA  <- file.path(SIB, "..", "ga-precinct-returns", "data")
DS  <- file.path(SIB, "..", "data-sources", "data")

say <- function(...) cat(..., "\n", sep = "")

# ===========================================================================
# 0.  THE ENCODING.  Lives in encoding.R beside this file, because build-1620.R
#     and reencode.R need the identical definition and a second copy of it is a
#     second thing to forget to change.  It defines DEG_PER_POINT, ANGLE_CAP,
#     LEN_CAP_PTS, BIMODAL_DEG, LEN_MAX_KM, LEN_SCALE and wind_geom().
# ===========================================================================

source("encoding.R")

# ---- Albers equal-area conic.  Pure arithmetic, no proj4 -------------------
# Returned units are kilometres.  Both renderers consume these x/y directly,
# which is how the D3 figure and the base-R figure are guaranteed to use the
# same projection: neither of them projects anything.
source("projection.R")   # albers, PRJ_US, PRJ_GA, prj, state_outline

FACTS <- list()
fact <- function(key, value) { FACTS[[key]] <<- value; invisible(value) }

# ===========================================================================
# 1.  COUNTY POPULATION CENTROIDS
#     https://www2.census.gov/geo/docs/reference/cenpop2020/county/
#         CenPop2020_Mean_CO.txt
#     Requested 2026-08-10: HTTP 200, 171,276 bytes, 3,221 data rows.
#     The header carries a UTF-8 BOM; fileEncoding="UTF-8-BOM" strips it.
#
#     Why population-weighted and not geometric: the arrow should start where
#     the people are.  San Bernardino County's geometric middle is in empty
#     desert 100 km from anyone who voted in it.
# ===========================================================================

CEN_URL <- paste0("https://www2.census.gov/geo/docs/reference/cenpop2020/",
                  "county/CenPop2020_Mean_CO.txt")
say("[1] county centroids: ", CEN_URL)
cen <- read.csv(CEN_URL, colClasses = "character", fileEncoding = "UTF-8-BOM")
stopifnot(nrow(cen) == 3221)
cen <- data.frame(fips = paste0(cen$STATEFP, cen$COUNTYFP),
                  county = cen$COUNAME, state = cen$STNAME,
                  pop = as.numeric(cen$POPULATION),
                  lat = as.numeric(cen$LATITUDE),
                  lon = as.numeric(cen$LONGITUDE))
stopifnot(!any(duplicated(cen$fips)), all(nchar(cen$fips) == 5))
dd_write_csv(cen, file.path(OUT, "derived/county_centroids.csv"))
fact("centroid_rows", nrow(cen))
fact("centroid_fetch", FETCH_DATE)
say("    ", nrow(cen), " centroids, ", sum(cen$pop), " people")

# ===========================================================================
# 2.  THE NATIONAL COUNTY RETURNS, AND WHAT IS WRONG WITH THEM
#
#     PROVENANCE, STATED PLAINLY.  ../../data-sources/data/build-data.R records
#     that both files come from Tony McGovern's compilation
#       https://github.com/tonmcg/US_County_Level_Election_Results_08-24
#     whose own README says the 2024 numbers were scraped from Fox News and
#     the 2020 numbers from Fox News, Politico and the New York Times, and
#     that they "are not authoritative".  There is no federal publisher of
#     county-level presidential returns -- the FEC publishes state and
#     congressional-district totals only.  County returns are published by 51
#     separate state election offices in 51 formats.
#
#     So this file is used here as the CAUTIONARY map, and the audit below is
#     the point of it.
# ===========================================================================

say("[2] national county returns + audit")
c20 <- read.csv(file.path(DS, "derived/pres2020_counties.csv"),
                colClasses = c(county_fips = "character"))
c24 <- read.csv(file.path(DS, "derived/pres2024_counties.csv"),
                colClasses = c(county_fips = "character"))
stopifnot(nrow(c20) == 3152, nrow(c24) == 3160)
fact("rows_2020", nrow(c20)); fact("rows_2024", nrow(c24))

only20 <- setdiff(c20$county_fips, c24$county_fips)
only24 <- setdiff(c24$county_fips, c20$county_fips)
fact("only_2020", length(only20)); fact("only_2024", length(only24))
fact("in_both", length(intersect(c20$county_fips, c24$county_fips)))

# Classify every unit that appears in one file only, plus the units that
# appear in both but do not describe the same place.
cls <- function(f, nm, st, yr) {
  s2 <- substr(f, 1, 2)
  ifelse(s2 == "02",
         "Alaska: the same 40 State House Districts under different pseudo-FIPS",
  ifelse(s2 == "09",
         "Connecticut: eight counties replaced by nine planning regions",
  ifelse(s2 == "11",
         "District of Columbia: one row in 2020, eight wards in 2024",
         "other")))
}
aud <- rbind(
  data.frame(fips = only20, appears_in = "2020 only",
             name = c20$county_name[match(only20, c20$county_fips)],
             state = c20$state_name[match(only20, c20$county_fips)],
             votes = c20$total_votes[match(only20, c20$county_fips)]),
  data.frame(fips = only24, appears_in = "2024 only",
             name = c24$county_name[match(only24, c24$county_fips)],
             state = c24$state_name[match(only24, c24$county_fips)],
             votes = c24$total_votes[match(only24, c24$county_fips)]))
aud$reason <- cls(aud$fips)
aud <- aud[order(aud$fips), ]

# The one that does NOT show up in setdiff() and is therefore the dangerous
# one: FIPS 11001 is present in both files and means different things.
dc20 <- c20[c20$county_fips == "11001", ]
dc24 <- c24[c24$county_fips == "11001", ]
fact("dc_name_2020", dc20$county_name); fact("dc_name_2024", dc24$county_name)
fact("dc_votes_2020", dc20$total_votes); fact("dc_votes_2024", dc24$total_votes)
fact("dc_state_total_2024", sum(c24$total_votes[substr(c24$county_fips,1,2) == "11"]))

# ... and the second silent failure: three of the 2024 Alaska pseudo-FIPS
# collide with real Alaska borough codes in the CENTROID file, so a join to
# the centroids succeeds and puts the arrow in the wrong place.
ak24 <- c24$county_fips[substr(c24$county_fips, 1, 2) == "02"]
ak_hit <- intersect(ak24, cen$fips)
fact("ak_collisions", length(ak_hit))
fact("ak_collision_list", paste(ak_hit, collapse = ", "))
fact("ak_collision_detail",
     paste(sprintf("%s = %s in the returns, %s Borough/Census Area in the centroids",
                   ak_hit, c24$county_name[match(ak_hit, c24$county_fips)],
                   cen$county[match(ak_hit, cen$fips)]), collapse = "; "))

# Connecticut: verified independently against the Census Bureau's own county
# code list rather than taken on trust.
#   2020 vintage  https://www2.census.gov/geo/docs/reference/codes2020/
#                     national_county2020.txt   (HTTP 200, 2026-08-10)
#     -> CT appears as 8 counties, CLASSFP H4, FUNCSTAT N (nonfunctioning).
#   2024 Gazetteer, already committed at ../../data-sources/data/derived/census_counties.csv
#     -> CT appears as 9 planning regions with new FIPS.
ct_now <- tryCatch({
  g <- read.csv(file.path(DS, "derived/census_counties.csv"), colClasses = "character")
  fc <- grep("fips|geoid", names(g), ignore.case = TRUE, value = TRUE)[1]
  nc <- grep("name", names(g), ignore.case = TRUE, value = TRUE)[1]
  g[substr(g[[fc]], 1, 2) == "09", c(fc, nc)]
}, error = function(e) NULL)
if (!is.null(ct_now)) {
  names(ct_now) <- c("fips", "name")
  fact("ct_current_units", nrow(ct_now))
  fact("ct_current_list", paste(ct_now$fips, collapse = ", "))
} else fact("ct_current_units", NA)
fact("ct_old_list", paste(only20[substr(only20,1,2) == "09"], collapse = ", "))
fact("ct_new_list", paste(only24[substr(only24,1,2) == "09"], collapse = ", "))
fact("ak_2020_range", paste0(min(only20[substr(only20,1,2)=="02"]), "-",
                             max(only20[substr(only20,1,2)=="02"])))
fact("ak_2024_range", paste0(min(only24[substr(only24,1,2)=="02"]), "-",
                             max(only24[substr(only24,1,2)=="02"])))

dd_write_csv(aud, file.path(OUT, "derived/provenance_audit.csv"))
asum <- as.data.frame(table(aud$reason, aud$appears_in))
names(asum) <- c("reason", "appears_in", "units")
asum <- asum[asum$units > 0, ]
dd_write_csv(asum, file.path(OUT, "derived/audit_summary.csv"))

# ===========================================================================
# 3.  THE NATIONAL WIND MAP, 2020 -> 2024
#
#     HARMONISATION, done explicitly rather than by letting a join decide:
#       * Alaska  -- DROPPED entirely.  The two files use different pseudo-FIPS
#                    for the same kind of unit, three of the 2024 codes collide
#                    with real borough codes in the centroid file, and Alaska
#                    redistricted its House between the two elections, so even
#                    matching on district number would compare different ground.
#       * DC      -- REPAIRED.  The 2024 wards are summed back to one District
#                    so that 11001 means the same place in both years.
#       * CT      -- DROPPED, and reported.  Eight counties genuinely became
#                    nine planning regions; the 2020 centroid file has no
#                    centroid for a planning region, so there is nowhere to
#                    start the arrow even if the votes could be reallocated.
#       * HI, and anything else outside the conic -- kept in the table, drawn
#                    only if inside the map frame; the count is reported.
# ===========================================================================

say("[3] national wind map")
harmonise <- function(d) {
  d <- d[substr(d$county_fips, 1, 2) != "02", ]                    # Alaska out
  dc <- d[substr(d$county_fips, 1, 2) == "11", ]
  d  <- d[substr(d$county_fips, 1, 2) != "11", ]
  if (nrow(dc)) d <- rbind(d, data.frame(
    state_name = "District of Columbia", county_fips = "11001",
    county_name = "District of Columbia",
    votes_dem = sum(dc$votes_dem), votes_gop = sum(dc$votes_gop),
    total_votes = sum(dc$total_votes)))
  d
}
h20 <- harmonise(c20); h24 <- harmonise(c24)
fact("ak_dropped_2020", sum(substr(c20$county_fips,1,2) == "02"))
fact("ak_dropped_2024", sum(substr(c24$county_fips,1,2) == "02"))
fact("dc_rows_folded", sum(substr(c24$county_fips,1,2) == "11"))

w <- merge(h20[, c("county_fips","state_name","county_name","votes_dem","votes_gop","total_votes")],
           h24[, c("county_fips","votes_dem","votes_gop","total_votes")],
           by = "county_fips", suffixes = c("_20", "_24"))
fact("matched_after_harmonise", nrow(w))
ct_lost <- sum(substr(h20$county_fips, 1, 2) == "09")
fact("ct_units_lost", ct_lost)
fact("ct_votes_lost", sum(h20$total_votes[substr(h20$county_fips,1,2) == "09"]))

w <- merge(w, cen[, c("fips","lon","lat","pop")], by.x = "county_fips", by.y = "fips")
fact("matched_with_centroid", nrow(w))

# ---- what the NAIVE join would have produced ------------------------------
# Merge the two files on county_fips with no harmonisation at all.  It returns
# exactly the same number of rows as the harmonised join, so the row count
# cannot warn you.  One arrow differs, and it differs in DIRECTION.
nv <- merge(c20[, c("county_fips","votes_dem","votes_gop")],
            c24[, c("county_fips","votes_dem","votes_gop")],
            by = "county_fips", suffixes = c("_20", "_24"))
fact("naive_matched", nrow(nv))
nv$m20 <- 100*(nv$votes_gop_20-nv$votes_dem_20)/(nv$votes_gop_20+nv$votes_dem_20)
nv$m24 <- 100*(nv$votes_gop_24-nv$votes_dem_24)/(nv$votes_gop_24+nv$votes_dem_24)
nv$sw <- nv$m24 - nv$m20
fact("naive_dc_margin_20", nv$m20[nv$county_fips == "11001"])
fact("naive_dc_margin_24", nv$m24[nv$county_fips == "11001"])
fact("naive_dc_swing", nv$sw[nv$county_fips == "11001"])
dcm20 <- 100*(dc20$votes_gop - dc20$votes_dem)/(dc20$votes_gop + dc20$votes_dem)
dc24all <- c24[substr(c24$county_fips,1,2) == "11", ]
dcm24 <- 100*(sum(dc24all$votes_gop) - sum(dc24all$votes_dem)) /
              (sum(dc24all$votes_gop) + sum(dc24all$votes_dem))
fact("true_dc_margin_20", dcm20); fact("true_dc_margin_24", dcm24)
fact("true_dc_swing", dcm24 - dcm20)
fact("dc_swing_error", (dcm24 - dcm20) - nv$sw[nv$county_fips == "11001"])
w <- w[(w$votes_dem_20 + w$votes_gop_20) > 0 & (w$votes_dem_24 + w$votes_gop_24) > 0, ]

w$margin_20 <- 100 * (w$votes_gop_20 - w$votes_dem_20) / (w$votes_gop_20 + w$votes_dem_20)
w$margin_24 <- 100 * (w$votes_gop_24 - w$votes_dem_24) / (w$votes_gop_24 + w$votes_dem_24)
w$swing     <- w$margin_24 - w$margin_20
xy <- prj(w$lon, w$lat, PRJ_US); w$x <- xy$x; w$y <- xy$y
w <- cbind(w, wind_geom(w$swing, w$votes_dem_24 + w$votes_gop_24, "us"))

# lower 48 + DC frame; Hawaii sits outside it and is reported, not hidden
FRAME_US <- list(xlim = c(-2500, 2500), ylim = c(-1600, 1600))
w$in_frame <- w$x >= FRAME_US$xlim[1] & w$x <= FRAME_US$xlim[2] &
              w$y >= FRAME_US$ylim[1] & w$y <= FRAME_US$ylim[2]
fact("us_arrows", sum(w$in_frame))
fact("us_outside_frame", sum(!w$in_frame))
fact("us_outside_states", paste(sort(unique(w$state_name[!w$in_frame])), collapse = ", "))
fact("us_swing_median", median(w$swing[w$in_frame]))
fact("us_swing_min", min(w$swing[w$in_frame]))
fact("us_swing_max", max(w$swing[w$in_frame]))
fact("us_share_R", 100 * mean(w$swing[w$in_frame] > 0))
fact("us_capped_len", sum(w$capped_len[w$in_frame]))
fact("us_capped_angle", sum(w$capped_angle[w$in_frame]))
# One arrow per county is itself an editorial choice.  These three numbers say
# what it costs: the aggregate swing of the votes, against the swing of the
# typical county, on exactly the same 3,100 counties.
vt <- w$votes_dem_24 + w$votes_gop_24
fr <- w$in_frame
agg <- function(dm, gp) 100*(sum(gp)-sum(dm))/(sum(gp)+sum(dm))
fact("us_agg_margin_20", agg(w$votes_dem_20[fr], w$votes_gop_20[fr]))
fact("us_agg_margin_24", agg(w$votes_dem_24[fr], w$votes_gop_24[fr]))
fact("us_agg_swing", agg(w$votes_dem_24[fr], w$votes_gop_24[fr]) -
                     agg(w$votes_dem_20[fr], w$votes_gop_20[fr]))
fact("us_swing_unweighted", mean(w$swing[fr]))
fact("us_biggest", w$county_name[fr][which.max(vt[fr])])
fact("us_biggest_state", w$state_name[fr][which.max(vt[fr])])
fact("us_biggest_votes", max(vt[fr]))
fact("us_smallest_votes", min(vt[fr]))
fact("us_size_ratio", round(max(vt[fr]) / min(vt[fr])))
fact("us_median_votes", median(vt[fr]))

w$lw <- sqrt(vt) / sqrt(max(vt))          # arrow thickness, 0..1
dd_write_csv(w[, c("county_fips","state_name","county_name","lon","lat","x","y","pop",
                "votes_dem_20","votes_gop_20","total_votes_20",
                "votes_dem_24","votes_gop_24","total_votes_24",
                "margin_20","margin_24","swing","angle_deg","len_km","dx","dy",
                "lw","in_frame","capped_len","capped_angle")], file.path(OUT, "derived/wind_us.csv"))

# ---- state outlines, projected once, at full resolution -------------------
# See the note over state_outline() in projection.R for why this is no longer
# thinned: independent per-ring thinning opens a hole at every state junction.
out <- state_outline()
dd_write_csv(out, file.path(OUT, "derived/us_outline.csv"))
fact("us_outline_points", nrow(out))

# ===========================================================================
# 4.  GEORGIA, DONE PROPERLY.  2020 -> 2024, THE 159 COUNTIES
#
#     PROVENANCE: both years are the Georgia Secretary of State's own record,
#     not a press reconstruction of it.
#       2020: November 3, 2020 General Election archive, one XML per county,
#             parsed by ../../ga-precinct-returns/data/parse-ga-sos.py into
#             counties.csv.  Behind Cloudflare, so downloaded once by hand.
#       2024: https://results.sos.ga.gov/cdn/results/Georgia/
#                 export-2024NovGen.json   (HTTP 200, fetched 2026-08-10)
#             parsed by the same script into ga2024_counties.csv.
#
#     UNIT: Georgia has had exactly 159 counties since 1945.  No crosswalk is
#     needed, no arrow can be fictional, and the join key takes 159 values
#     that can be checked by eye.
#
#     ARROW ORIGIN: the Census Bureau's population-weighted county centroid --
#     the same construction as the national map, so the two maps are built
#     the same way and differ only in their sourcing.
# ===========================================================================

say("[4] Georgia counties, 2020 -> 2024")
gc20 <- read.csv(file.path(GA, "derived/counties.csv"))
gc24 <- read.csv(file.path(GA, "derived/ga2024_counties.csv"))
fact("ga_rows_2020", nrow(gc20)); fact("ga_rows_2024", nrow(gc24))
jn <- function(s) toupper(gsub("[^A-Za-z]", "", s))
gc20$join <- jn(gc20$county); gc24$join <- jn(gc24$county)
fact("ga_county_join_misses",
     length(union(setdiff(gc20$join, gc24$join), setdiff(gc24$join, gc20$join))))

gg <- merge(
  data.frame(join = gc20$join, county = gc20$county,
             dem_20 = gc20$Joseph.R..Biden, gop_20 = gc20$Donald.J..Trump,
             tot_20 = gc20$total),
  data.frame(join = gc24$join,
             dem_24 = gc24$Kamala.D..Harris, gop_24 = gc24$Donald.J..Trump,
             tot_24 = gc24$total), by = "join")
stopifnot(nrow(gg) == 159)
gac <- cen[cen$state == "Georgia", ]; gac$join <- jn(gac$county)
gg <- merge(gg, gac[, c("join","fips","lon","lat","pop")], by = "join")
stopifnot(nrow(gg) == 159, !any(duplicated(gg$fips)))
fact("ga_counties_drawn", nrow(gg))

gg$margin_20 <- 100 * (gg$gop_20 - gg$dem_20) / (gg$gop_20 + gg$dem_20)
gg$margin_24 <- 100 * (gg$gop_24 - gg$dem_24) / (gg$gop_24 + gg$dem_24)
gg$swing <- gg$margin_24 - gg$margin_20
xy <- prj(gg$lon, gg$lat, PRJ_GA); gg$x <- xy$x; gg$y <- xy$y
gg <- cbind(gg, wind_geom(gg$swing, gg$dem_24 + gg$gop_24, "ga_county"))
gg$lw <- sqrt(gg$gop_24 + gg$dem_24) / sqrt(max(gg$gop_24 + gg$dem_24))
fact("ga_swing_median", median(gg$swing))
fact("ga_swing_min", min(gg$swing)); fact("ga_swing_max", max(gg$swing))
fact("ga_swing_min_county", gg$county[which.min(gg$swing)])
fact("ga_swing_max_county", gg$county[which.max(gg$swing)])
fact("ga_share_R", 100 * mean(gg$swing > 0))
fact("ga_capped_len", sum(gg$capped_len))
fact("ga_state_margin_20", 100*(sum(gg$gop_20)-sum(gg$dem_20))/(sum(gg$gop_20)+sum(gg$dem_20)))
fact("ga_state_margin_24", 100*(sum(gg$gop_24)-sum(gg$dem_24))/(sum(gg$gop_24)+sum(gg$dem_24)))
fact("ga_state_swing",
     (100*(sum(gg$gop_24)-sum(gg$dem_24))/(sum(gg$gop_24)+sum(gg$dem_24))) -
     (100*(sum(gg$gop_20)-sum(gg$dem_20))/(sum(gg$gop_20)+sum(gg$dem_20))))
fact("ga_agg_swing_placeholder", NA); fact("ga_swing_voteweighted",
     sum(gg$swing * (gg$gop_24+gg$dem_24)) / sum(gg$gop_24+gg$dem_24))
fact("ga_swing_unweighted", mean(gg$swing))
fact("ga_ballots_24", sum(gg$tot_24))
# the biggest and smallest counties, for the vote-weighting argument
gg <- gg[order(-(gg$gop_24 + gg$dem_24)), ]
fact("ga_biggest", gg$county[1]); fact("ga_biggest_votes", (gg$gop_24+gg$dem_24)[1])
fact("ga_smallest", gg$county[159]); fact("ga_smallest_votes", (gg$gop_24+gg$dem_24)[159])
fact("ga_size_ratio", round((gg$gop_24+gg$dem_24)[1] / (gg$gop_24+gg$dem_24)[159]))
dd_write_csv(gg[, c("fips","county","lon","lat","x","y","pop","dem_20","gop_20","tot_20",
                 "dem_24","gop_24","tot_24","margin_20","margin_24","swing",
                 "angle_deg","len_km","dx","dy","lw","capped_len","capped_angle")], file.path(OUT, "derived/wind_ga.csv"))

# ---- Georgia county outlines, from the state's own precinct shapefile ------
sh <- st_read(file.path(GA, "raw", "shp2024", "GaPrec_2024-Website-Shapefile.shp"),
              quiet = TRUE)
sh <- st_transform(st_make_valid(sh), 4326)
cty <- aggregate(sh["COUNTY"], by = list(COUNTY = sh$COUNTY), FUN = function(z) z[1])
# Simplify in a planar CRS (EPSG:5070, metres) -- st_simplify's tolerance is
# meaningless on lat/long, which is how a 13 MB outline file happens.
cty <- st_transform(st_simplify(st_transform(cty, 5070), dTolerance = 1200,
                                preserveTopology = TRUE), 4326)
rings <- do.call(rbind, lapply(seq_len(nrow(cty)), function(i) {
  cs <- st_coordinates(st_geometry(cty)[i])
  L <- apply(cs[, setdiff(colnames(cs), c("X","Y")), drop = FALSE], 1,
             paste, collapse = "_")
  o <- prj(cs[, "X"], cs[, "Y"], PRJ_GA)
  data.frame(county = cty$COUNTY[i], part = paste(i, L, sep = "_"),
             x = round(o$x, 2), y = round(o$y, 2))
}))
# drop ring fragments too small to see at print size, but never drop a county
# outright: keep each county's largest ring whatever its size.
tb <- table(rings$part)
keep_part <- names(which(tb >= 6))
biggest <- vapply(split(rings$part, rings$county),
                  function(p) names(which.max(table(p))), character(1))
rings <- rings[rings$part %in% union(keep_part, biggest), ]
stopifnot(length(unique(rings$county)) == 159)
dd_write_csv(rings, file.path(OUT, "derived/ga_outline.csv"))
fact("ga_outline_points", nrow(rings)); fact("ga_counties", nrow(cty))

# ===========================================================================
# 4b.  WHY NOT PRECINCTS?  THE QUESTION ASKED AND ANSWERED WITH A NUMBER.
#
#     Georgia has 2,701 precincts and 159 counties, so precinct level is 17x
#     the resolution.  ../precinct-geography/ has already solved the hard part
#     -- the 2020 votes are carried onto the 2024 precinct map by a
#     population-weighted block crosswalk.  What remains is attaching the 2024
#     returns, and the only available key is the precinct NAME.
#
#     This block runs the join, applies the obvious repairs one at a time, and
#     records what each rung of the ladder buys.  The answer is that it stops
#     short, and stops short in one place in particular.
# ===========================================================================

say("[4b] the precinct join, measured")
pp20 <- read.csv(file.path(GA, "derived/precincts_2024_pop.csv"))   # 2020 on 2024 lines
pp24 <- read.csv(file.path(GA, "derived/ga2024_precincts.csv"))
pp20$cty <- sub("[|].*", "", pp20$to_2024); pp20$prc <- sub(".*[|]", "", pp20$to_2024)
pp24$cty <- toupper(pp24$county);           pp24$prc <- toupper(pp24$precinct)

rung <- function(f, lab) {
  a <- paste(pp20$cty, f(pp20$prc)); b <- paste(pp24$cty, f(pp24$prc))
  data.frame(rung = lab, matched = length(intersect(a, b)),
             unmatched_2020 = length(setdiff(a, b)))
}
r0 <- function(s) toupper(s)
r1 <- function(s) sub("^[0-9]+[ -]+", "", r0(s))                 # ballot order
r2 <- function(s) gsub("\\([^)]*\\)", "", r1(s))                 # city tag
r3 <- function(s) gsub("[^A-Z0-9]", "", r2(s))                   # punctuation
lad <- rbind(rung(r0, "1. exact name, uppercased"),
             rung(r1, "2. strip leading ballot-order number"),
             rung(r2, "3. strip trailing (CITY) tag"),
             rung(r3, "4. strip all punctuation and spaces"))
lad$pct_of_2020 <- round(100 * lad$matched / nrow(pp20), 1)
dd_write_csv(lad, file.path(OUT, "derived/precinct_join_ladder.csv"))
print(lad)
fact("pj_precincts_2020", nrow(pp20)); fact("pj_precincts_2024", nrow(pp24))
fact("pj_exact", lad$matched[1]); fact("pj_best", lad$matched[4])
fact("pj_best_pct", lad$pct_of_2020[4])
fact("pj_lost", nrow(pp20) - lad$matched[4])
fact("pj_lost_pct", round(100 * (nrow(pp20) - lad$matched[4]) / nrow(pp20), 1))

a3 <- paste(pp20$cty, r3(pp20$prc)); b3 <- paste(pp24$cty, r3(pp24$prc))
miss <- pp20[!(a3 %in% b3), ]
byc <- sort(table(miss$cty), decreasing = TRUE)
resid <- data.frame(county = names(byc), unmatched = as.integer(byc))
resid$total_2020 <- as.integer(table(pp20$cty)[resid$county])
resid$ballots_2020 <- as.integer(tapply(pp20$TRUMP20 + pp20$BIDEN20,
                                        pp20$cty, sum)[resid$county])
resid$ballots_unmatched <- as.integer(tapply(miss$TRUMP20 + miss$BIDEN20,
                                             miss$cty, sum)[resid$county])
resid <- resid[order(-resid$unmatched), ]
dd_write_csv(resid, file.path(OUT, "derived/precinct_join_residual.csv"))
fact("pj_worst_county", resid$county[1])
fact("pj_worst_unmatched", resid$unmatched[1])
fact("pj_worst_total", resid$total_2020[1])
fact("pj_worst_ballots", resid$ballots_unmatched[1])
fact("pj_counties_affected", nrow(resid))
fact("pj_ballots_lost", sum(resid$ballots_unmatched))
fact("pj_ballots_lost_pct",
     round(100*sum(resid$ballots_unmatched)/sum(pp20$TRUMP20 + pp20$BIDEN20), 1))
# is the loss random?  compare the D share of matched and unmatched precincts
dem_share <- function(d) 100*sum(d$BIDEN20)/sum(d$BIDEN20 + d$TRUMP20)
fact("pj_dem_share_matched", dem_share(pp20[a3 %in% b3, ]))
fact("pj_dem_share_lost", dem_share(miss))

# ===========================================================================
# 5.  THE OFFICE PROBLEM, MEASURED
#
#     A 2024 -> 2026 arrow would compare a presidential race to a midterm:
#     different office, different electorate, different turnout.  Two of those
#     three can be held fixed exactly, using Georgia's November 2020 ballot,
#     which carried a presidential race and a US Senate race.  Same voters,
#     same day, same precinct, same piece of paper.  Whatever the two offices
#     disagree by is a pure office effect with ZERO turnout difference -- a
#     floor on the error a cross-office arrow carries before it has measured
#     any real swing at all.
#
#     Uses the crosswalked precinct file, but note that no cross-YEAR join is
#     involved here: every number comes from one file and one election.
# ===========================================================================

say("[5] office gap")
og <- pp20
og$two_pres <- og$TRUMP20 + og$BIDEN20
og$two_sen  <- og$PURDUE20 + og$OSSOFF20
og <- og[og$two_pres >= 100 & og$two_sen >= 100, ]
og$margin_pres <- 100 * (og$TRUMP20 - og$BIDEN20) / og$two_pres
og$margin_sen  <- 100 * (og$PURDUE20 - og$OSSOFF20) / og$two_sen
og$gap <- og$margin_sen - og$margin_pres
dd_write_csv(og[, c("to_2024","two_pres","two_sen","margin_pres","margin_sen","gap")], file.path(OUT, "derived/office_gap.csv"))
fact("og_n", nrow(og))
fact("og_state_pres", 100*(sum(og$TRUMP20)-sum(og$BIDEN20))/sum(og$two_pres))
fact("og_state_sen",  100*(sum(og$PURDUE20)-sum(og$OSSOFF20))/sum(og$two_sen))
fact("og_state_gap", (100*(sum(og$PURDUE20)-sum(og$OSSOFF20))/sum(og$two_sen)) -
                     (100*(sum(og$TRUMP20)-sum(og$BIDEN20))/sum(og$two_pres)))
fact("og_median_abs", median(abs(og$gap)))
fact("og_p90_abs", unname(quantile(abs(og$gap), 0.9)))
fact("og_max_abs", max(abs(og$gap)))
fact("og_share_over2", 100*mean(abs(og$gap) > 2))
fact("og_share_over5", 100*mean(abs(og$gap) > 5))
fact("og_ballots", sum(og$two_pres))
fact("og_cor", cor(og$margin_pres, og$margin_sen))
# how big is the office effect next to the thing we are trying to measure?
fact("og_gap_vs_ga_swing", abs((100*(sum(og$PURDUE20)-sum(og$OSSOFF20))/sum(og$two_sen)) -
                               (100*(sum(og$TRUMP20)-sum(og$BIDEN20))/sum(og$two_pres))))

# ===========================================================================
# 6.  THE 2026 SLOT.  BASELINE BUILT NOW; THE SECOND HALF ARRIVES IN NOVEMBER.
#
#     THE OFFICE DECISION, MADE VISIBLE: the class map is
#         Governor 2022  ->  Governor 2026
#     not 2024 -> 2026.  Both are Georgia statewide midterm races for the same
#     office.  Section 5 measures what the alternative would have cost.
#
#     Unit: the 159 Georgia counties, which have not changed since 1945, so no
#     crosswalk is needed and no arrow can be fictional.  (Precinct level would
#     need a 2022->2026 crosswalk, which does not exist yet.)
#
#     SOURCE, official and scriptable:
#       https://results.sos.ga.gov/cdn/results/Georgia/export-2022NovGen.json
#         HTTP 200, 3,891,592 bytes, fetched 2026-08-10, snapshot 2025-01-08.
#       https://results.sos.ga.gov/cdn/results/Georgia/export-2026NovGen.json
#         HTTP 404 on 2026-08-10 -- as it should be; the election is in
#         November.  This is the URL to try on class day.
# ===========================================================================

say("[6] Georgia county baseline, 2022 governor")
GA_URL <- function(y) sprintf(
  "https://results.sos.ga.gov/cdn/results/Georgia/export-%dNovGen.json", y)

# A minimal reader for the SoS export.  Only the county totals are needed, so
# this pulls localResults[].ballotItems[].ballotOptions[] and nothing else.
read_ga_counties <- function(url_or_path, contest_regex) {
  txt <- paste(readLines(url_or_path, warn = FALSE), collapse = "")
  d <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  rows <- list()
  for (co in d$localResults) {
    for (bi in co$ballotItems) {
      if (!grepl(contest_regex, bi$name, ignore.case = TRUE)) next
      for (op in bi$ballotOptions) {
        rows[[length(rows) + 1]] <- data.frame(
          county = sub("\\s+County$", "", co$name),
          contest = bi$name, candidate = op$name,
          party = if (is.null(op$politicalParty)) NA_character_ else op$politicalParty,
          votes = as.numeric(op$voteCount))
      }
    }
  }
  attr(rows, "meta") <- c(d$electionName, d$electionDate, d$createdAt)
  r <- do.call(rbind, rows)
  attr(r, "meta") <- c(name = d$electionName, date = d$electionDate,
                       snapshot = d$createdAt)
  r
}
g22 <- read_ga_counties(GA_URL(2022), "^Governor$")
meta22 <- attr(g22, "meta")
fact("ga22_election", meta22[["name"]]); fact("ga22_date", meta22[["date"]])
fact("ga22_snapshot", substr(meta22[["snapshot"]], 1, 10))
fact("ga22_counties", length(unique(g22$county)))

wide <- function(d) {
  dem <- tapply(d$votes[d$party == "DEM"], d$county[d$party == "DEM"], sum)
  rep <- tapply(d$votes[d$party == "REP"], d$county[d$party == "REP"], sum)
  tot <- tapply(d$votes, d$county, sum)
  data.frame(county = names(tot), dem = as.numeric(dem[names(tot)]),
             gop = as.numeric(rep[names(tot)]), total = as.numeric(tot))
}
b22 <- wide(g22)
fact("ga22_dem", sum(b22$dem)); fact("ga22_gop", sum(b22$gop))
fact("ga22_margin", 100*(sum(b22$gop)-sum(b22$dem))/(sum(b22$gop)+sum(b22$dem)))

gac <- cen[cen$state == "Georgia", ]
gac$join <- toupper(gsub("[^A-Za-z]", "", gac$county))
b22$join <- toupper(gsub("[^A-Za-z]", "", b22$county))
b22 <- merge(b22, gac[, c("join","fips","lon","lat","pop")], by = "join")
stopifnot(nrow(b22) == 159)
xy <- prj(b22$lon, b22$lat, PRJ_GA); b22$x <- xy$x; b22$y <- xy$y
b22$margin_from <- 100*(b22$gop - b22$dem)/(b22$gop + b22$dem)
# the 2026 half, deliberately empty
b22$dem_to <- NA; b22$gop_to <- NA; b22$margin_to <- NA; b22$swing <- NA
dd_write_csv(b22[, c("fips","county","lon","lat","x","y","pop","dem","gop","total",
                  "margin_from","dem_to","gop_to","margin_to","swing")], file.path(OUT, "derived/wind_ga_counties.csv"))

# ---- THE ONE FILE THAT CHANGES ON CLASS DAY -------------------------------
# Schema, documented in wind-map.Rmd.  Drop the 2026 numbers into the two
# empty columns of this file (or regenerate it with build_2026()) and every
# figure in the lab redraws.  unit_id MUST be quoted character: 13001, not
# 13001 as a number -- a sibling lab lost 1,549 of 4,489 GEOIDs to numeric
# coercion and the same trap is set here.
stub <- data.frame(
  unit_id      = b22$fips,
  unit_name    = b22$county,
  state        = "Georgia",
  office       = "Governor",
  year_from    = 2022L,
  year_to      = 2026L,
  votes_dem_from = b22$dem,
  votes_gop_from = b22$gop,
  votes_dem_to = NA_integer_,
  votes_gop_to = NA_integer_)
dd_write_csv(stub, file.path(OUT, "derived/wind_input_2026.csv"))
fact("stub_rows", nrow(stub))
fact("stub_filled", sum(!is.na(stub$votes_dem_to)))

#' Refresh the class map once the 2026 returns exist.
#'
#'   Rscript -e 'source("build-data.R"); build_2026()'
#'
#' Tries the Secretary of State export first.  If it 404s -- which it will
#' until the state posts it -- pass a local file, or hand-edit the two empty
#' columns of wind_input_2026.csv and call build_2026(from_stub = TRUE).
build_2026 <- function(url = GA_URL(2026), office = "^Governor$",
                       from_stub = FALSE) {
  st <- read.csv(file.path(OUT, "derived/wind_input_2026.csv"),
                 colClasses = c(unit_id = "character"))
  if (!from_stub) {
    d <- read_ga_counties(url, office)
    w <- wide(d)
    w$join <- toupper(gsub("[^A-Za-z]", "", w$county))
    st$join <- toupper(gsub("[^A-Za-z]", "", st$unit_name))
    miss <- setdiff(st$join, w$join)
    if (length(miss)) warning("no 2026 row for: ", paste(miss, collapse = ", "))
    st$votes_dem_to <- w$dem[match(st$join, w$join)]
    st$votes_gop_to <- w$gop[match(st$join, w$join)]
    st$join <- NULL
    dd_write_csv(st, file.path(OUT, "derived/wind_input_2026.csv"))
  }
  base <- read.csv(file.path(OUT, "derived/wind_ga_counties.csv"),
                   colClasses = c(fips = "character"))
  i <- match(base$fips, st$unit_id)
  base$dem_to <- st$votes_dem_to[i]; base$gop_to <- st$votes_gop_to[i]
  ok <- !is.na(base$dem_to) & !is.na(base$gop_to) & (base$dem_to + base$gop_to) > 0
  base$margin_to[ok] <- 100*(base$gop_to[ok]-base$dem_to[ok])/(base$gop_to[ok]+base$dem_to[ok])
  base$swing <- base$margin_to - base$margin_from
  dd_write_csv(base, file.path(OUT, "derived/wind_ga_counties.csv"))
  cat("counties with a 2026 result:", sum(ok), "of", nrow(base), "\n")
  invisible(base)
}

# ===========================================================================
# 7.  PARITY.  The D3 figure and the base-R figure must place the same arrow
#     tips.  Both read x, y, dx, dy from the CSVs above and neither projects
#     or rescales anything, so agreement is structural rather than hoped for.
#     This table records the arithmetic so the documents can show it.
# ===========================================================================

say("[7] parity")
chk <- function(d, lab) {
  th <- d$angle_deg * pi / 180
  data.frame(figure = lab, n = nrow(d),
             max_dx_err = max(abs(d$dx - d$len_km * sin(th))),
             max_dy_err = max(abs(d$dy - d$len_km * cos(th))),
             max_len_err = max(abs(sqrt(d$dx^2 + d$dy^2) - d$len_km)))
}
par_tbl <- rbind(chk(w[w$in_frame, ], "national counties"),
                 chk(gg, "Georgia counties"))
dd_write_csv(par_tbl, file.path(OUT, "derived/parity.csv"))
print(par_tbl)

# ===========================================================================
# 8.  FACTS
# ===========================================================================
ff <- data.frame(key = names(FACTS),
                 value = vapply(FACTS, function(z)
                   # rounded HERE and not in fact(), because the facts are
                   # kept at full precision for this chapter's own parity
                   # assertion, which compares a recorded value against a
                   # freshly computed one at 1e-9 and would fail on a
                   # rounded operand.
                   as.character(dd_num(z))[1], character(1)))
rownames(ff) <- NULL
ff <- rbind(ff, data.frame(
  key = c("deg_per_point","angle_cap","len_min_frac",
          "len_max_km_us","net_votes_full_us",
          "len_max_km_ga_county","net_votes_full_ga_county","fetch_date"),
  value = c(DEG_PER_POINT, ANGLE_CAP, LEN_MIN_FRAC,
            LEN_MAX_KM["us"], NET_VOTES_FULL["us"],
            LEN_MAX_KM["ga_county"], NET_VOTES_FULL["ga_county"],
            FETCH_DATE)))
ff <- ff[!is.na(ff$value), ]
dd_write_csv(ff, file.path(OUT, "derived/facts.csv"))
say("done. ", nrow(ff), " facts written.")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
