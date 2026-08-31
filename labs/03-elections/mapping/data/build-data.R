# ---------------------------------------------------------------------------
# Build the `mapping` lab datasets: the 2024 presidential result drawn four
# ways, and the arithmetic of land against people that the drawings encode.
#
# FETCH DATE: 2026-08-11.  Every URL below was requested on that date and the
# byte count and row count of the response recorded beside it.  Nothing in this
# file is typed in by hand.
#
# WHAT THIS BUILDS
#
#   derived/counties.csv        3,109 mapped units: FIPS, name, state, votes, winner,
#                       land area, density, cartogram geometry
#   derived/county_rings.csv    the county outlines, projected, simplified, quantised
#   derived/state_rings.csv     the state outlines, from the same polygons dissolved
#   derived/grid_cells.csv      one cell per county, the grid-cartogram layout
#   derived/dorling.csv         one circle per county, area proportional to votes
#   derived/lorenz.csv          the counties ordered by density, cumulated
#   derived/excluded.csv        every unit in either source that this map cannot draw
#   derived/facts.csv           every scalar the brief quotes
#   derived/parity.csv          the D3 / base-R agreement check
#
# PACKAGES.  Build scripts may use packages; the student-facing .Rmd may not.
# Uses: sf (geometry), rmapshaper (topology-preserving simplification).  The
# brief reads only the CSVs written here and uses base R alone.
#
# RUN FROM INSIDE data/:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE, timeout = 900)
suppressMessages({library(sf); library(rmapshaper)})
sf_use_s2(FALSE)

FETCH_DATE <- "2026-08-11"
OUT <- "."
DS  <- file.path("..", "..", "data-sources", "data")

say <- function(...) cat(..., "\n", sep = "")
FACTS <- list()
fact  <- function(key, value) { FACTS[[key]] <<- value; invisible(value) }

# The two colours, declared once and written into facts.csv so that the D3
# figures, the base-R figures and the prose all read them from the same place.
COL_GOP <- "#B2182B"
COL_DEM <- "#2166AC"

# ===========================================================================
# 1.  COUNTY GEOMETRY.  Cartographic boundary file, 1:5,000,000.
#
#     https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_5m.zip
#     Requested 2026-08-11: HTTP 200, 2,982,952 bytes, 3,235 features.
#
#     WHY THE 2024 VINTAGE AND NOT 2020.  Connecticut abolished its eight
#     counties in 2022 and replaced them with nine planning regions.  The 2024
#     file carries the nine; the 2020 file carries the eight.  The election
#     returns below are reported for the nine.  Choosing the vintage that
#     matches the returns is the difference between nine real units and nine
#     silent join failures.
#
#     WHY ALAND AND NOT THE POLYGON.  Every land area quoted in the brief is
#     the ALAND attribute of this file -- the Census Bureau's own measurement,
#     in square metres, of the county's land surface.  It is an attribute, not
#     a computed property of the outline, so no projection and no simplification
#     performed below can move it.  That is the whole reason for using it.
# ===========================================================================

say("[1] county geometry")
GEO_URL <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_5m.zip"
dir.create(file.path(OUT, "raw/tiger"), showWarnings = FALSE, recursive = TRUE)
zp <- file.path(OUT, "raw/tiger", "cb_2024_us_county_5m.zip")
if (!file.exists(zp)) prov_fetch(GEO_URL, zp, mode = "wb", quiet = TRUE)
unzip(zp, exdir = file.path(OUT, "raw/tiger"), overwrite = TRUE)
fact("geo_url", GEO_URL); fact("geo_bytes", file.size(zp))

sh <- st_read(file.path(OUT, "raw/tiger", "cb_2024_us_county_5m.shp"), quiet = TRUE)
sh$GEOID <- as.character(sh$GEOID)
stopifnot(nrow(sh) == 3235, all(nchar(sh$GEOID) == 5))
fact("geo_features", nrow(sh))

# The frame: the 48 contiguous states and the District of Columbia.  Alaska and
# Hawaii are excluded from the map AND from every number in the brief, so that
# no figure and no sentence covers a different country from the other.
OFF <- c("02", "15", "60", "66", "69", "72", "78")   # AK HI AS GU MP PR VI
fact("dropped_features", sum(sh$STATEFP %in% OFF))
geo <- sh[!(sh$STATEFP %in% OFF), ]
fact("frame_units", nrow(geo))

# ===========================================================================
# 2.  THE RETURNS, AND WHAT IS WRONG WITH THEM
#
#     ../../data-sources/data/derived/pres2024_counties.csv, already in this corpus.
#     ../../data-sources/data/build-data.R records that it is Tony McGovern's
#     compilation, https://github.com/tonmcg/US_County_Level_Election_Results_08-24,
#     scraped from Fox News, Politico and the New York Times, and whose own
#     README says the numbers "are not authoritative".
#
#     There is no federal publisher of county-level presidential returns.  The
#     FEC publishes state and congressional-district totals.  County returns
#     are published by 51 separate state election offices in 51 formats, and
#     every national county map you have ever seen rests on somebody's
#     reconciliation of them.
#
#     THREE UNIT MISMATCHES, all documented in ../../wind-map/data/build-data.R
#     and handled here the same way:
#       * Alaska      -- the returns use 40 State House Districts under
#                        pseudo-FIPS 02001-02040; the geography file has 30
#                        boroughs and census areas.  Nothing joins.  Alaska is
#                        outside the frame in any case and is dropped whole.
#       * DC          -- the returns give eight wards, 11001-11008, where 11001
#                        means Ward 1 rather than the District.  The geography
#                        has one unit.  The eight wards are summed back to one.
#                        This is the dangerous one: a join on FIPS SUCCEEDS,
#                        silently pairing the District's outline with Ward 1's
#                        votes.
#       * Connecticut -- nine planning regions in the returns, nine planning
#                        regions in the 2024 geography.  Matching vintages, so
#                        nothing to repair; but the 2020 population file in
#                        section 5 still has the eight old counties, and that
#                        mismatch is reported rather than papered over.
# ===========================================================================

say("[2] returns")
c24 <- read.csv(file.path(DS, "derived/pres2024_counties.csv"),
                colClasses = c(county_fips = "character"))
stopifnot(nrow(c24) == 3160, all(nchar(c24$county_fips) == 5))
fact("returns_rows", nrow(c24))
fact("returns_votes_all", sum(c24$total_votes))

# what the unrepaired join would have done to the District
dc_raw <- c24[c24$county_fips == "11001", ]
fact("dc_naive_name", dc_raw$county_name)
fact("dc_naive_votes", dc_raw$total_votes)
fact("dc_wards", sum(substr(c24$county_fips, 1, 2) == "11"))

dc <- c24[substr(c24$county_fips, 1, 2) == "11", ]
r  <- c24[substr(c24$county_fips, 1, 2) != "11", ]
r  <- rbind(r, data.frame(
  state_name = "District of Columbia", county_fips = "11001",
  county_name = "District of Columbia",
  votes_dem = sum(dc$votes_dem), votes_gop = sum(dc$votes_gop),
  total_votes = sum(dc$total_votes)))
fact("dc_true_votes", sum(dc$total_votes))
fact("dc_ward_error", sum(dc$total_votes) - dc_raw$total_votes)

fact("ak_rows", sum(substr(c24$county_fips, 1, 2) == "02"))
fact("ak_votes", sum(c24$total_votes[substr(c24$county_fips, 1, 2) == "02"]))
fact("hi_rows", sum(substr(c24$county_fips, 1, 2) == "15"))
fact("hi_votes", sum(c24$total_votes[substr(c24$county_fips, 1, 2) == "15"]))
fact("ct_units", sum(substr(c24$county_fips, 1, 2) == "09"))

# every unit in either source that this map cannot draw, written out in full
exc <- rbind(
  data.frame(fips = c24$county_fips[substr(c24$county_fips, 1, 2) %in% c("02", "15")],
             name = c24$county_name[substr(c24$county_fips, 1, 2) %in% c("02", "15")],
             state = c24$state_name[substr(c24$county_fips, 1, 2) %in% c("02", "15")],
             votes = c24$total_votes[substr(c24$county_fips, 1, 2) %in% c("02", "15")],
             reason = ifelse(substr(c24$county_fips[substr(c24$county_fips, 1, 2) %in%
                               c("02", "15")], 1, 2) == "02",
                             "Alaska reports 40 State House Districts; the geography file has 30 boroughs",
                             "Hawaii lies outside the frame this map draws")),
  data.frame(fips = dc$county_fips[dc$county_fips != "11001"],
             name = dc$county_name[dc$county_fips != "11001"],
             state = "District of Columbia",
             votes = dc$total_votes[dc$county_fips != "11001"],
             reason = "summed into the District of Columbia, which is one unit in the geography"))
dd_write_csv(exc, file.path(OUT, "derived/excluded.csv"))

# ---- the join ------------------------------------------------------------
d <- data.frame(fips = as.character(geo$GEOID), name = as.character(geo$NAME),
                state = as.character(geo$STATE_NAME),
                aland_m2 = as.numeric(geo$ALAND))
stopifnot(!any(duplicated(d$fips)))
miss <- setdiff(d$fips, r$county_fips)
fact("join_missing", length(miss))
stopifnot(length(miss) == 0)
i <- match(d$fips, r$county_fips)
d$votes_dem <- r$votes_dem[i]; d$votes_gop <- r$votes_gop[i]
d$votes_tot <- r$total_votes[i]
d$two <- d$votes_dem + d$votes_gop
stopifnot(all(d$two > 0))
fact("mapped_units", nrow(d))
fact("mapped_votes", sum(d$votes_tot))
fact("mapped_two", sum(d$two))

d$aland_km2 <- d$aland_m2 / 1e6
d$winner    <- ifelse(d$votes_gop >= d$votes_dem, "R", "D")
d$margin    <- 100 * (d$votes_gop - d$votes_dem) / d$two
d$density   <- d$two / d$aland_km2                 # votes per square kilometre

# ===========================================================================
# 3.  THE HEADLINE ARITHMETIC.  Land against people, on exactly the units the
#     four figures draw.  Every one of these is quoted in the brief.
# ===========================================================================

say("[3] land against votes")
R <- d$winner == "R"
fact("n_R", sum(R)); fact("n_D", sum(!R))
fact("pct_counties_R", 100 * mean(R))
fact("land_total_km2", sum(d$aland_km2))
fact("land_R_km2", sum(d$aland_km2[R]))
fact("pct_land_R", 100 * sum(d$aland_km2[R]) / sum(d$aland_km2))
fact("pct_votes_R_counties", 100 * sum(d$two[R]) / sum(d$two))
fact("pct_votes_D_counties", 100 * sum(d$two[!R]) / sum(d$two))
fact("votes_R_counties", sum(d$two[R]))
fact("votes_D_counties", sum(d$two[!R]))

# the actual result on this frame, which is the thing the map is meant to show
fact("gop_votes", sum(d$votes_gop)); fact("dem_votes", sum(d$votes_dem))
fact("gop_share_two", 100 * sum(d$votes_gop) / sum(d$two))
fact("dem_share_two", 100 * sum(d$votes_dem) / sum(d$two))
fact("national_margin", 100 * (sum(d$votes_gop) - sum(d$votes_dem)) / sum(d$two))

# the ratio the whole chapter turns on: ink per vote
fact("km2_per_vote_R", sum(d$aland_km2[R]) / sum(d$two[R]))
fact("km2_per_vote_D", sum(d$aland_km2[!R]) / sum(d$two[!R]))
fact("ink_ratio", (sum(d$aland_km2[R]) / sum(d$two[R])) /
                  (sum(d$aland_km2[!R]) / sum(d$two[!R])))

# concentration
o  <- d[order(-d$two), ]
fact("counties_for_half", which(cumsum(o$two) >= 0.5 * sum(d$two))[1])
fact("land_for_half", 100 * sum(o$aland_km2[seq_len(which(cumsum(o$two) >=
        0.5 * sum(d$two))[1])]) / sum(d$aland_km2))
oL <- d[order(-d$aland_km2), ]
fact("largest_county", oL$name[1]); fact("largest_state", oL$state[1])
fact("largest_km2", oL$aland_km2[1]); fact("largest_votes", oL$two[1])
fact("largest_margin", oL$margin[1])
den <- d[order(-d$density), ]
fact("densest_county", den$name[1]); fact("densest_state", den$state[1])
fact("densest_km2", den$aland_km2[1]); fact("densest_votes", den$two[1])
fact("densest_density", den$density[1])
fact("area_ratio_extreme", oL$aland_km2[1] / den$aland_km2[1])

# the two counties that make the point in one line
big <- d[d$fips == oL$fips[1], ]; sml <- d[d$fips == den$fips[1], ]
fact("pair_area_ratio", big$aland_km2 / sml$aland_km2)
fact("pair_vote_ratio", sml$two / big$two)

fact("median_votes", median(d$two))
fact("median_km2", median(d$aland_km2))
fact("total_two", sum(d$two))

# ===========================================================================
# 4.  PROJECTION, SIMPLIFICATION, QUANTISATION
#
#     PROJECTION: Albers equal-area conic for the contiguous US, with the
#     parameters recorded in ../../../_lib/geo/us-frame.json -- the same
#     projection, fit and frame as the shared base maps in _lib/geo/, so this
#     chapter's counties overlay us-albers.geojson (at the FSCL below).  A
#     chapter that argues about area has no business drawing it in Web
#     Mercator, which inflates the north; on an equal-area projection a square
#     centimetre of paper is the same number of square kilometres wherever it
#     sits.  (Formerly EPSG:5070 -- identical parallels and meridian, so no
#     shape changed when the frame did.)
#
#     SIMPLIFICATION: rmapshaper::ms_simplify, which is topology-aware -- it
#     thins a boundary once and both counties sharing it keep the same thinned
#     line, so no slivers or overlaps open up between neighbours.  keep_shapes
#     guarantees no county is thinned out of existence.
#
#     WHAT SIMPLIFICATION CANNOT TOUCH: every area, density, vote and share in
#     section 3 was computed before this line runs, from ALAND and from the
#     vote columns.  Section 8 re-computes them from the simplified object and
#     confirms they are identical, because none of them reads the outline.
#
#     QUANTISATION: coordinates are turned into integers at twice the shared
#     frame's resolution -- two canvas units to the frame pixel, so the
#     coastline keeps its detail while the vertices stay whole numbers -- and
#     both renderers are handed those integers.  Neither the browser nor the
#     PDF device projects, rescales or rounds anything, so the two figures
#     cannot disagree about where a boundary is.  The figures crop to the
#     CONUS box of the frame: same coordinates as every other chapter's
#     national map, without the empty corner where the frame keeps Alaska
#     and Hawaii -- states this chapter's numbers deliberately exclude.
# ===========================================================================

say("[4] project, simplify, quantise")
poly_only <- function(g) {
  g <- st_make_valid(g)
  if (any(st_geometry_type(g) %in% c("GEOMETRYCOLLECTION")))
    g <- st_collection_extract(g, "POLYGON")
  st_cast(g, "MULTIPOLYGON")
}
FRAME <- jsonlite::fromJSON("../../../_lib/geo/us-frame.json")
FSCL  <- 2L                                # canvas units per frame pixel
geo <- poly_only(st_transform(geo, FRAME$conus_proj))
pts_before <- nrow(st_coordinates(geo))
KEEP <- 0.24
sm <- poly_only(ms_simplify(geo, keep = KEEP, keep_shapes = TRUE, method = "vis"))
pts_after <- nrow(st_coordinates(sm))
fact("simplify_keep", KEEP)
fact("points_before", pts_before); fact("points_after", pts_after)
fact("points_pct", 100 * pts_after / pts_before)
stopifnot(nrow(sm) == nrow(geo), all(sm$GEOID == geo$GEOID))

# The frame fit, applied verbatim from us-frame.json (projection is in km).
qx <- function(x) as.integer(round((FRAME$fit_ox + (x - FRAME$fit_xmin) * FRAME$fit_s) * FSCL))
qy <- function(y) as.integer(round((FRAME$fit_oy + (FRAME$fit_ymax - y) * FRAME$fit_s) * FSCL))
# The crop the figures draw: the frame's CONUS box, padded two frame pixels
# so a coastal vertex this build keeps and the state file thinned away is not
# shaved off at the window's edge.
PADU <- 2L * FSCL
CANVAS_X0 <- as.integer(round(FRAME$conus_box$x * FSCL)) - PADU
CANVAS_Y0 <- as.integer(round(FRAME$conus_box$y * FSCL)) - PADU
CANVAS    <- as.integer(round(FRAME$conus_box$w * FSCL)) + 2L * PADU
CANVAS_H  <- as.integer(round(FRAME$conus_box$h * FSCL)) + 2L * PADU
fact("canvas_x0", CANVAS_X0); fact("canvas_y0", CANVAS_Y0)
fact("canvas_w", CANVAS); fact("canvas_h", CANVAS_H)
fact("frame_scale", FSCL)
fact("km_per_unit", 1 / (FRAME$fit_s * FSCL))

rings_of <- function(obj, idcol) {
  do.call(rbind, lapply(seq_len(nrow(obj)), function(i) {
    cs <- st_coordinates(st_geometry(obj)[i])
    L  <- apply(cs[, setdiff(colnames(cs), c("X", "Y")), drop = FALSE], 1,
                paste, collapse = "_")
    data.frame(id = obj[[idcol]][i], part = paste(i, L, sep = "_"),
               x = qx(cs[, "X"]), y = qy(cs[, "Y"]))
  }))
}
cr <- rings_of(sm, "GEOID")
# Two vertices that quantise to the same integer are the same point on the
# page, so the second one is dropped.  This changes nothing that is drawn and
# it removes several thousand characters from the browser figure.
dup <- c(FALSE, cr$x[-1] == cr$x[-nrow(cr)] & cr$y[-1] == cr$y[-nrow(cr)] &
                cr$part[-1] == cr$part[-nrow(cr)])
fact("dup_vertices_dropped", sum(dup))
cr <- cr[!dup, ]
# Drop ring fragments too small to see at print size -- but never drop a
# county: each county keeps its largest ring whatever that ring's size.
tb  <- table(cr$part)
keepp <- names(which(tb >= 5))
bigp  <- vapply(split(cr$part, cr$id), function(p) names(which.max(table(p))),
                character(1))
cr <- cr[cr$part %in% union(keepp, bigp), ]
stopifnot(length(unique(cr$id)) == nrow(d))
cr$part <- as.integer(factor(cr$part))
dd_write_csv(cr, file.path(OUT, "derived/county_rings.csv"))
fact("ring_points", nrow(cr)); fact("ring_parts", length(unique(cr$part)))

# state outlines, from the same polygons dissolved, so the two layers agree
stg <- aggregate(sm["STATEFP"], by = list(STATEFP = sm$STATEFP),
                 FUN = function(z) z[1])
stg <- ms_simplify(stg, keep = 0.12, keep_shapes = TRUE, method = "vis")
sr <- rings_of(stg, "STATEFP")
tb <- table(sr$part); sr <- sr[sr$part %in% names(which(tb >= 8)), ]
sr$part <- as.integer(factor(sr$part))
dd_write_csv(sr, file.path(OUT, "derived/state_rings.csv"))
fact("state_ring_points", nrow(sr)); fact("states_drawn", length(unique(sr$id)))

# The frame agreement, measured rather than assumed.  The state outlines
# above (dissolved from this build's counties) and the shared base map's
# CONUS states are two simplifications of the same Census geometry placed by
# the same fit, so their bounding boxes must land within a few pixels of one
# another.  If this stops holding, one of the two builds has moved the frame.
ub <- jsonlite::fromJSON("../../../_lib/geo/us-albers.geojson",
                         simplifyVector = FALSE)
ux <- c(Inf, -Inf); uy <- c(Inf, -Inf)
for (f in ub$features) {
  if (f$properties$st %in% c("AK", "HI")) next
  for (poly in f$geometry$coordinates) for (ring in poly) for (p in ring) {
    if (p[[1]] < ux[1]) ux[1] <- p[[1]]; if (p[[1]] > ux[2]) ux[2] <- p[[1]]
    if (p[[2]] < uy[1]) uy[1] <- p[[2]]; if (p[[2]] > uy[2]) uy[2] <- p[[2]]
  }
}
# Three edges anchor on the continent itself -- Cape Alava, West Quoddy
# Head, the Lake of the Woods -- points every simplification keeps.  The
# south edge is the Florida Keys, a chain of ring fragments the base map
# keeps down to Key West and this build's small-ring filter drops (and
# always dropped); its difference is recorded, not asserted on.
misfit <- max(abs(c(min(sr$x) / FSCL - ux[1], max(sr$x) / FSCL - ux[2],
                    min(sr$y) / FSCL - uy[1])))
fact("frame_misfit_px", misfit)
fact("keys_tail_px", uy[2] - max(sr$y) / FSCL)
stopifnot(misfit < 6)

# county centres, on the same canvas, used by both cartograms
ct <- st_coordinates(st_point_on_surface(st_geometry(sm)))
d$cx <- qx(ct[, "X"])[match(d$fips, sm$GEOID)]
d$cy <- qy(ct[, "Y"])[match(d$fips, sm$GEOID)]

# ===========================================================================
# 5.  POPULATION, AND THE VINTAGE THAT DOES NOT MATCH
#
#     https://www2.census.gov/geo/docs/reference/cenpop2020/county/
#         CenPop2020_Mean_CO.txt
#     Requested 2026-08-11: HTTP 200, 171,276 bytes, 3,221 data rows.  Header
#     carries a UTF-8 BOM; fileEncoding="UTF-8-BOM" strips it.
#
#     This file is 2020 vintage, so its Connecticut is the eight counties that
#     were abolished in 2022.  Nine planning regions therefore get no
#     population.  Nothing in the brief's argument depends on it -- every
#     headline number uses votes, which exist for all 3,109 units -- but the
#     population is what makes "land against people" literal, so the gap is
#     counted and reported rather than hidden by an inner join.
# ===========================================================================

say("[5] population")
CEN_URL <- paste0("https://www2.census.gov/geo/docs/reference/cenpop2020/",
                  "county/CenPop2020_Mean_CO.txt")
cen <- read.csv(CEN_URL, colClasses = "character", fileEncoding = "UTF-8-BOM")
stopifnot(nrow(cen) == 3221)
fact("cen_url", CEN_URL); fact("cen_rows", nrow(cen))
cen$fips <- paste0(cen$STATEFP, cen$COUNTYFP)
d$pop <- as.numeric(cen$POPULATION)[match(d$fips, cen$fips)]
fact("pop_missing", sum(is.na(d$pop)))
fact("pop_missing_states",
     paste(sort(unique(d$state[is.na(d$pop)])), collapse = ", "))
fact("pop_missing_votes", sum(d$two[is.na(d$pop)]))
hp <- !is.na(d$pop)
fact("pop_covered", sum(d$pop[hp]))
fact("pop_units", sum(hp))
fact("pop_R", sum(d$pop[hp & R])); fact("pop_D", sum(d$pop[hp & !R]))
fact("pct_pop_R", 100 * sum(d$pop[hp & R]) / sum(d$pop[hp]))
fact("pct_land_R_pop", 100 * sum(d$aland_km2[hp & R]) / sum(d$aland_km2[hp]))

# ===========================================================================
# 6.  THE GRID CARTOGRAM.  One identical square per county, placed as near as
#     possible to where the county actually is.
#
#     THE CONSTRUCTION, stated so it can be argued with.  Lay a square grid
#     over the projected map.  Every county asks for the cell containing its
#     own centre.  Where several ask for the same cell, the one whose centre is
#     nearest that cell's middle keeps it and the others are sent outward in a
#     spiral to the nearest free cell.  Repeat until every county has a cell.
#     Deterministic: no random seed, no hand placement.
#
#     WHAT IT COSTS is measured below, in cells and in kilometres, and quoted
#     in the figure caption.  A grid cartogram that does not report its own
#     displacement is asking to be trusted about the one thing it changed.
# ===========================================================================

say("[6] grid cartogram")
CELL <- 11L                                 # canvas units per cell -- the same
                                            # ground the old 8 covered on the
                                            # old 1440-unit canvas
GPAD <- 14L                                  # spare cells beyond the map edge
# cx/cy are absolute frame coordinates, so the lattice runs from the frame's
# own origin; the empty columns left of the map cost a few kilobytes of the
# `taken` matrix and nothing else
gw <- as.integer(ceiling((CANVAS_X0 + CANVAS) / CELL)) + 2L * GPAD
gh <- as.integer(ceiling((CANVAS_Y0 + CANVAS_H) / CELL)) + 2L * GPAD
fact("grid_cell_units", CELL)
fact("grid_cell_km", CELL * as.numeric(FACTS$km_per_unit))
fact("grid_cols", gw); fact("grid_rows", gh); fact("grid_slots", gw * gh)

ideal_c <- as.integer(floor(d$cx / CELL)) + GPAD
ideal_r <- as.integer(floor(d$cy / CELL)) + GPAD
taken <- matrix(0L, nrow = gw, ncol = gh)     # 0 = free, else county index
assign_col <- integer(nrow(d)); assign_row <- integer(nrow(d))

# spiral offsets, ordered by true distance so "nearest free cell" means nearest
RAD <- 70L
off <- expand.grid(dc = -RAD:RAD, dr = -RAD:RAD)
off <- off[order(off$dc^2 + off$dr^2), ]
off <- as.matrix(off)

# Process order: the county whose centre sits closest to the middle of its own
# ideal cell has the strongest claim on it, so it goes first.  This is the
# only ordering rule, and it does not look at votes, party, area or state.
claim <- (d$cx - ((ideal_c - GPAD) * CELL + CELL / 2))^2 +
         (d$cy - ((ideal_r - GPAD) * CELL + CELL / 2))^2
ord <- order(claim)
for (k in ord) {
  cc <- ideal_c[k]; rw <- ideal_r[k]
  placed <- FALSE
  for (j in seq_len(nrow(off))) {
    a <- cc + off[j, 1]; b <- rw + off[j, 2]
    if (a < 0 || b < 0 || a >= gw || b >= gh) next
    if (taken[a + 1L, b + 1L] == 0L) {
      taken[a + 1L, b + 1L] <- k
      assign_col[k] <- a; assign_row[k] <- b
      placed <- TRUE; break
    }
  }
  stopifnot(placed)
}
d$gcol <- assign_col; d$grow <- assign_row
d$gx <- (assign_col - GPAD) * CELL; d$gy <- (assign_row - GPAD) * CELL
shift_units <- sqrt((d$gx + CELL / 2 - d$cx)^2 + (d$gy + CELL / 2 - d$cy)^2)
d$grid_shift_km <- shift_units * as.numeric(FACTS$km_per_unit)
fact("grid_shift_median_km", median(d$grid_shift_km))
fact("grid_shift_p90_km", unname(quantile(d$grid_shift_km, 0.9)))
fact("grid_shift_max_km", max(d$grid_shift_km))
fact("grid_shift_max_county", d$name[which.max(d$grid_shift_km)])
fact("grid_shift_max_state", d$state[which.max(d$grid_shift_km)])
fact("grid_same_cell", 100 * mean(shift_units < CELL))
fact("grid_within_50km", 100 * mean(d$grid_shift_km < 50))
dd_write_csv(d[, c("fips", "gcol", "grow", "gx", "gy", "winner")], file.path(OUT, "derived/grid_cells.csv"))

# ===========================================================================
# 7.  THE DORLING CARTOGRAM.  One circle per county, AREA proportional to the
#     two-party votes cast in it, pushed apart until none overlaps.
#
#     RADIUS: r = S * sqrt(votes), one constant S for the whole map, printed in
#     the legend, so any two circles can be compared by eye and converted back
#     to votes.  S is chosen so that the circles together cover a fixed share
#     of the frame; it is not fitted to anything.
#
#     RELAXATION: 600 rounds.  In each round a circle that overlaps a
#     neighbour is pushed along the line joining the centres, and every circle
#     is pulled a little way back toward where the county really is.  The pull
#     is what keeps Ohio in Ohio.
# ===========================================================================

say("[7] dorling cartogram")
FILL_SHARE <- 0.42
S <- sqrt(FILL_SHARE * CANVAS * CANVAS_H / (pi * sum(d$two)))
d$r <- S * sqrt(d$two)
fact("dorling_S", S)
fact("dorling_fill_share", FILL_SHARE)
fact("dorling_votes_per_unit2", 1 / (pi * S^2))   # votes per square canvas unit
fact("dorling_r_max", max(d$r)); fact("dorling_r_min", min(d$r))
fact("dorling_biggest", d$name[which.max(d$two)])
fact("dorling_biggest_state", d$state[which.max(d$two)])

px <- as.numeric(d$cx); py <- as.numeric(d$cy)
ox <- px; oy <- py; rr <- d$r; rr0 <- d$r
# candidate neighbours: everything whose centre starts within a generous
# radius, computed once from a coarse bucket grid
BK <- 160                            # bucket size, scaled with the canvas
bx <- floor(px / BK); by <- floor(py / BK)
key <- paste(bx, by)
buck <- split(seq_along(px), key)
cand <- vector("list", length(px))
for (i in seq_along(px)) {
  ks <- paste(rep((bx[i] - 2):(bx[i] + 2), each = 5),
              rep((by[i] - 2):(by[i] + 2), times = 5))
  v <- unlist(buck[ks[ks %in% names(buck)]], use.names = FALSE)
  v <- v[v != i]
  KN <- if (rr0[i] > 11) 46L else 26L
  if (length(v) > KN) {
    dd <- (px[v] - px[i])^2 + (py[v] - py[i])^2
    v <- v[order(dd)][1:KN]
  }
  cand[[i]] <- v
}
ii <- rep(seq_along(cand), lengths(cand))
jj <- unlist(cand, use.names = FALSE)
sel <- ii < jj                       # each pair once
ii <- ii[sel]; jj <- jj[sel]
fact("dorling_pairs", length(ii))
NIT <- 600
for (it in 1:NIT) {
  dx <- px[jj] - px[ii]; dy <- py[jj] - py[ii]
  dist <- sqrt(dx^2 + dy^2); dist[dist < 1e-6] <- 1e-6
  need <- rr[ii] + rr[jj] - dist
  hit <- need > 0
  if (any(hit)) {
    ux <- dx[hit] / dist[hit]; uy <- dy[hit] / dist[hit]
    push <- need[hit] / 2
    mvx <- push * ux; mvy <- push * uy
    px <- px - c(tapply(mvx, factor(ii[hit], levels = seq_along(px)), sum,
                        default = 0))
    py <- py - c(tapply(mvy, factor(ii[hit], levels = seq_along(py)), sum,
                        default = 0))
    px <- px + c(tapply(mvx, factor(jj[hit], levels = seq_along(px)), sum,
                        default = 0))
    py <- py + c(tapply(mvy, factor(jj[hit], levels = seq_along(py)), sum,
                        default = 0))
  }
  att <- 0.055 * (1 - it / NIT) + 0.003
  px <- px + att * (ox - px); py <- py + att * (oy - py)
}
d$dx <- px; d$dy <- py
dsh <- sqrt((px - ox)^2 + (py - oy)^2) * as.numeric(FACTS$km_per_unit)
fact("dorling_shift_median_km", median(dsh))
fact("dorling_shift_max_km", max(dsh))
fact("dorling_shift_max_county", d$name[which.max(dsh)])
fact("dorling_shift_max_state", d$state[which.max(dsh)])
# residual overlap, reported rather than claimed to be zero
dxp <- px[jj] - px[ii]; dyp <- py[jj] - py[ii]
ov <- (rr[ii] + rr[jj]) - sqrt(dxp^2 + dyp^2)
fact("dorling_overlaps", sum(ov > 0.5))
fact("dorling_overlap_max_units", max(c(ov, 0)))
dd_write_csv(data.frame(fips = d$fips, x = round(px), y = round(py),
                     r = round(d$r, 1), winner = d$winner), file.path(OUT, "derived/dorling.csv"))

# ===========================================================================
# 8.  THE CONCENTRATION CURVE.  Counties ordered from the emptiest land to the
#     fullest, then cumulated: how much of the country's land you have crossed
#     against how many of its votes you have collected.
# ===========================================================================

say("[8] concentration curve")
z <- d[order(d$density), ]
lz <- data.frame(rank = seq_len(nrow(z)),
                 cum_land = 100 * cumsum(z$aland_km2) / sum(z$aland_km2),
                 cum_votes = 100 * cumsum(z$two) / sum(z$two),
                 cum_counties = 100 * seq_len(nrow(z)) / nrow(z))
dd_write_csv(data.frame(rank = lz$rank,
                     cum_land = round(lz$cum_land, 4),
                     cum_votes = round(lz$cum_votes, 4),
                     cum_counties = round(lz$cum_counties, 4),
                     winner = z$winner), file.path(OUT, "derived/lorenz.csv"))
at_land <- function(p) lz$cum_votes[which(lz$cum_land >= p)[1]]
at_vote <- function(p) lz$cum_land[which(lz$cum_votes >= p)[1]]
fact("votes_on_half_land", at_land(50))
fact("votes_on_90_land", at_land(90))
fact("land_for_half_votes", at_vote(50))
fact("land_for_90_votes", at_vote(90))
fact("counties_for_half_votes", lz$cum_counties[which(lz$cum_votes >= 50)[1]])
# the same curve read down the county axis instead of the land axis
hc <- which(lz$cum_counties >= 50)[1]
fact("land_at_half_counties", lz$cum_land[hc])
fact("votes_at_half_counties", lz$cum_votes[hc])
fact("counties_at_half_land", lz$cum_counties[which(lz$cum_land >= 50)[1]])
fact("n_counties_for_half_votes", which(lz$cum_votes >= 50)[1])
# where the two winners sit along that ordering
fact("median_density_rank_R", 100 * median(which(z$winner == "R")) / nrow(z))
fact("median_density_rank_D", 100 * median(which(z$winner == "D")) / nrow(z))
fact("pct_land_D", 100 - as.numeric(FACTS$pct_land_R))
# Gini of votes against land
gini <- sum((lz$cum_land - lz$cum_votes)) / sum(lz$cum_land)
fact("conc_gini", gini)

# ===========================================================================
# 9.  PARITY AND SELF-CHECKS.
#
#     (a) Nothing quoted in the brief was moved by simplification.  Re-compute
#         the headline numbers from the simplified, quantised object and
#         compare to section 3, which ran before any thinning.
#     (b) The polygon area actually drawn is not the land area quoted; the
#         difference is measured here so the brief can state it.
#     (c) Both renderers read the same integers.  This table records the
#         agreement arithmetic so the document can show it.
# ===========================================================================

say("[9] parity")
d2 <- d
recomp <- c(pct_counties_R = 100 * mean(d2$winner == "R"),
            pct_land_R = 100 * sum(d2$aland_km2[d2$winner == "R"]) / sum(d2$aland_km2),
            pct_votes_R_counties = 100 * sum(d2$two[d2$winner == "R"]) / sum(d2$two),
            national_margin = 100 * (sum(d2$votes_gop) - sum(d2$votes_dem)) / sum(d2$two))
par_tbl <- data.frame(
  quantity = names(recomp),
  before_simplification = vapply(names(recomp), function(k) as.numeric(FACTS[[k]]),
                                 numeric(1)),
  after_simplification = as.numeric(recomp))
par_tbl$difference <- par_tbl$after_simplification - par_tbl$before_simplification
# drawn polygon area against the Census Bureau's own land measurement
drawn_km2 <- as.numeric(sum(st_area(sm))) / 1e6
fact("drawn_km2", drawn_km2)
fact("drawn_vs_aland_pct", 100 * (drawn_km2 - sum(d$aland_km2)) / sum(d$aland_km2))
# the D3/base-R contract: identical integers, identical arithmetic
par_tbl <- rbind(par_tbl, data.frame(
  quantity = c("county ring vertices", "grid cells", "dorling circles"),
  before_simplification = c(nrow(cr), nrow(d), nrow(d)),
  after_simplification = c(nrow(cr), nrow(d), nrow(d)),
  difference = 0))
dd_write_csv(par_tbl, file.path(OUT, "derived/parity.csv"))
print(par_tbl)
stopifnot(max(abs(par_tbl$difference)) < 1e-9)

# ===========================================================================
# 10.  WRITE
# ===========================================================================

say("[10] write")
dd_write_csv(d[, c("fips", "name", "state", "votes_dem", "votes_gop", "votes_tot",
                "two", "winner", "margin", "aland_km2", "density", "pop",
                "cx", "cy", "gcol", "grow", "gx", "gy", "r", "grid_shift_km")], file.path(OUT, "derived/counties.csv"))

fact("col_gop", COL_GOP); fact("col_dem", COL_DEM)
fact("fetch_date", FETCH_DATE)
ff <- data.frame(key = names(FACTS),
                 value = vapply(FACTS, function(z)
                   # rounded HERE and not in fact(), because the facts are
                   # kept at full precision for this chapter's own parity
                   # assertion, which compares a recorded value against a
                   # freshly computed one at 1e-9 and would fail on a
                   # rounded operand.
                   as.character(dd_num(z))[1], character(1)))
rownames(ff) <- NULL
dd_write_csv(ff, file.path(OUT, "derived/facts.csv"))
say("done. ", nrow(ff), " facts, ", nrow(d), " counties, ", nrow(cr), " ring points.")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
