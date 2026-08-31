# ---------------------------------------------------------------------------
# build-maps.R -- the geometry half of the census-geography lab.
#
# `build-data.py` in this folder answers the question "how many of each kind of
# area are there, and what does its identifier encode?" for the whole country.
# It has no shapes in it at all.  This script supplies the shapes, for ONE
# county, so that the lab's two claims can be SEEN and not merely asserted:
#
#   THE SPINE NESTS.        block -> block group -> tract -> county.  Each unit
#                           dissolves exactly into its parent, and the parent's
#                           GEOID is a PREFIX of the child's.  Drawn as four
#                           panels of the same county at four scales, with one
#                           block and its three ancestors picked out in red.
#
#   THE OTHER MAP DOES NOT. Municipal boundaries (census "places") and ZCTAs are
#                           laid over the same tracts.  Where a boundary crosses
#                           a tract, the tract is SPLIT, and the split is
#                           counted rather than gestured at.
#
# WHY HOUSTON COUNTY, GEORGIA (FIPS 13153).  Georgia is this course's working
# state; the precinct-geography lab uses this same county, so students meet the
# same ground twice.  It also happens to be an unusually clean demonstration:
# 38 tracts, three incorporated cities plus an Air Force base CDP, and a city
# (Warner Robins) that crosses the county line into Peach County -- which is the
# concrete reason a place identifier has no county component in it.
#
# WRITES (all read by the three .Rmd documents, which are base R)
#   derived/map_units.csv    long-format polygon rings: lev, uid, part, x, y
#                    lev is b block, g block group, t tract, c county,
#                    p place, z ZCTA, s the state of Georgia.  Coordinates are kilometres on EPSG:5070
#                    (Albers equal area), shifted so the county's lower-left
#                    corner is the origin, rounded to 10 m.
#   derived/map_ids.csv      lev, uid -> the real GEOID (CHARACTER) and name
#   derived/chain.csv        one block and its ancestors: the GEOID digits, the level
#                    each prefix names, and the uid to highlight in each panel
#   derived/splits.csv       for each overlay geography: how many of the 38 tracts its
#                    boundaries cut through
#   derived/tract_split.csv  the same result tract by tract, so the figures can shade
#                    the split ones instead of asserting a count
#   derived/facts.csv        every scalar the documents quote, name/value/note
#
# SOURCES (all fetched 2026-08-10, all keyless)
#   TIGER/Line 2020 tracts        https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_13_tract.zip
#   TIGER/Line 2020 block groups  https://www2.census.gov/geo/tiger/TIGER2020/BG/tl_2020_13_bg.zip
#   TIGER/Line 2020 places        https://www2.census.gov/geo/tiger/TIGER2020/PLACE/tl_2020_13_place.zip
#   TIGER/Line 2020 ZCTAs         https://www2.census.gov/geo/tiger/TIGER2020/ZCTA520/tl_2020_us_zcta520.zip
#                                 (national, 528 MB -- there is no state file)
#   TIGER/Line 2020 blocks        https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_13_tabblock20.zip
#                                 reused from ../../../03-elections/ga-precinct-returns/data/raw/blocks/
#                                 if already unpacked there, as areal-units does
#   2020 Block Assignment File    https://www2.census.gov/geo/docs/maps-data/data/baf2020/BlockAssign_ST13_GA.zip
#                                 block -> incorporated place / CDP, the Bureau's
#                                 own assignment rather than one of my making
#   ZCTA-to-tract relationship    https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_tract20_natl.txt
#   ZCTA-to-county relationship   https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt
#
# FEATURE COUNTS at the last run (printed again on every run; the script stops
# if they change, so a silent re-vintage cannot slip through)
#   Georgia          2,796 tracts   7,446 block groups   675 places
#   Houston County   3,269 blocks   108 block groups   38 tracts   1 county
#                    163,633 people   4 places touching the county
#                    12 ZCTAs touching the county
#
# Downloads land in ./cache, which is disposable: delete it and the next run
# fetches again.  The CSVs this writes are what the documents read.
#
# BUILD SCRIPT -- may use packages.  The three student-facing documents are base
# R and read only the CSVs.  Run from this directory:  Rscript build-maps.R
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


dir.create("derived", showWarnings = FALSE)

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
options(scipen = 999, stringsAsFactors = FALSE, warn = 1)

ST     <- "13"                          # Georgia
CTY    <- "153"                         # Houston County
CTYFIP <- paste0(ST, CTY)
CTYNM  <- "Houston County, Georgia"
CACHE  <- "cache"
dir.create(CACHE, showWarnings = FALSE)

n <- function(x) format(x, big.mark = ",")
say <- function(...) cat(sprintf(...), "\n", sep = "")

# --- fetching ---------------------------------------------------------------
grab <- function(url, file) {
  p <- file.path(CACHE, file)
  if (!file.exists(p)) {
    say("downloading %s", url)
    prov_fetch(url, p, mode = "wb", quiet = TRUE)
  }
  p
}
shp <- function(url, zipname) {
  z <- grab(url, zipname)
  d <- sub("\\.zip$", "", z)
  if (!dir.exists(d)) utils::unzip(z, exdir = d)
  st_read(list.files(d, "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
}

TIGER <- "https://www2.census.gov/geo/tiger/TIGER2020"
REL   <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520"

# =========================================================================
# 1.  The spine: blocks, block groups, tracts, county
# =========================================================================
# Blocks are a 164 MB download for Georgia. The ga-precinct-returns lab already
# has them unpacked; reuse that copy when it is there, exactly as the
# areal-units build does, and fall back to fetching if it is not.
BLK_LOCAL <- "../../../03-elections/ga-precinct-returns/data/raw/blocks/tl_2020_13_tabblock20.shp"
if (file.exists(BLK_LOCAL)) {
  BLK <- BLK_LOCAL
} else {
  z <- grab(sprintf("%s/TABBLOCK20/tl_2020_13_tabblock20.zip", TIGER),
            "tl_2020_13_tabblock20.zip")
  d <- sub("\\.zip$", "", z)
  if (!dir.exists(d)) utils::unzip(z, exdir = d)
  BLK <- list.files(d, "\\.shp$", full.names = TRUE)[1]
}
say("blocks read from %s", BLK)

gb <- st_read(BLK, quiet = TRUE, query = sprintf(
  "SELECT GEOID20, POP20, HOUSING20, ALAND20 FROM tl_2020_13_tabblock20
   WHERE COUNTYFP20 = '%s'", CTY))
gb$GEOID20 <- as.character(gb$GEOID20)
stopifnot(all(nchar(gb$GEOID20) == 15))

gt_all <- shp(sprintf("%s/TRACT/tl_2020_13_tract.zip", TIGER), "tl_2020_13_tract.zip")
gg_all <- shp(sprintf("%s/BG/tl_2020_13_bg.zip",       TIGER), "tl_2020_13_bg.zip")
gp_all <- shp(sprintf("%s/PLACE/tl_2020_13_place.zip", TIGER), "tl_2020_13_place.zip")
say("Georgia: %s tracts   %s block groups   %s places",
    n(nrow(gt_all)), n(nrow(gg_all)), n(nrow(gp_all)))
stopifnot(nrow(gt_all) == 2796, nrow(gg_all) == 7446, nrow(gp_all) == 675)

gt <- gt_all[gt_all$COUNTYFP == CTY, ]
gg <- gg_all[gg_all$COUNTYFP == CTY, ]
gt$GEOID <- as.character(gt$GEOID); gg$GEOID <- as.character(gg$GEOID)

# The prefix claim, tested on this county's own files before anything is drawn.
stopifnot(all(substr(gb$GEOID20, 1, 5)  == CTYFIP),
          all(substr(gg$GEOID,   1, 5)  == CTYFIP),
          all(substr(gt$GEOID,   1, 5)  == CTYFIP),
          all(substr(gb$GEOID20, 1, 12) %in% gg$GEOID),
          all(substr(gg$GEOID,   1, 11) %in% gt$GEOID))
say("%s: %s blocks   %s block groups   %s tracts   population %s",
    CTYNM, n(nrow(gb)), n(nrow(gg)), n(nrow(gt)), n(sum(gb$POP20)))
stopifnot(nrow(gb) == 3269, nrow(gg) == 108, nrow(gt) == 38,
          sum(gb$POP20) == 163633)

# Every unit is projected to Albers equal area. On a lat/long map at this
# latitude the county would be visibly stretched sideways, and the whole point
# of these panels is that the shapes are the same shapes.
prj <- function(g) st_transform(st_make_valid(g), 5070)
gb <- prj(gb); gg <- prj(gg); gt <- prj(gt)
county <- st_union(gt)

# =========================================================================
# 2.  The overlays: places, ZCTAs
# =========================================================================
gp <- prj(gp_all)
touch <- st_intersects(gp, county, sparse = FALSE)[, 1]
gp <- gp[touch, ]
gp$GEOID <- as.character(gp$GEOID)
# A place is "in" the county if it has real land here, not a stray sliver where
# two boundaries follow the same river a few metres apart.
ov <- as.numeric(st_area(st_intersection(gp, county))) / 1e6
gp <- gp[ov > 0.05, ]
gp <- gp[order(-ov[ov > 0.05]), ]
say("places with land in the county: %d -- %s", nrow(gp),
    paste(gp$NAME, collapse = ", "))
stopifnot(nrow(gp) == 4)

gz_all <- shp(sprintf("%s/ZCTA520/tl_2020_us_zcta520.zip", TIGER),
              "tl_2020_us_zcta520.zip")
zid <- grep("^ZCTA5CE", names(gz_all), value = TRUE)[1]
gz_all$ZCTA <- as.character(gz_all[[zid]])
# The national ZCTA file is 33,791 polygons; take only those whose bounding box
# is anywhere near the county before doing any real geometry on it.
bb <- unname(st_bbox(st_transform(county, 4326)))
box <- st_transform(st_as_sfc(st_bbox(c(
  xmin = bb[1] - 0.3, ymin = bb[2] - 0.3,
  xmax = bb[3] + 0.3, ymax = bb[4] + 0.3), crs = 4326)), st_crs(gz_all))
near <- st_intersects(gz_all, box, sparse = FALSE)[, 1]
gz <- prj(gz_all[near, ])
gz <- gz[st_intersects(gz, county, sparse = FALSE)[, 1], ]
zov <- as.numeric(st_area(st_intersection(gz, county))) / 1e6
gz <- gz[zov > 0.05, ]
say("ZCTAs with land in the county: %d -- %s", nrow(gz),
    paste(sort(gz$ZCTA), collapse = " "))

# =========================================================================
# 3.  Does it nest?  Counted three ways, on the same 38 tracts.
# =========================================================================
# The rule is the same for all three overlays, and it is the definition of
# nesting rather than a proxy for it. Two areas nest if one is inside the
# other. So for every (tract, overlay unit) pair that overlaps at all, ask:
# is the overlay unit inside the tract, or the tract inside the overlay unit?
# If NEITHER, the two boundaries CROSS -- part of the tract is in that unit and
# part is not, and part of that unit is in the tract and part is not. A tract
# with any such crossing is SPLIT, and any statistic published for the overlay
# has to guess how that tract's people fall on either side of the line.
#
# Getting this direction right matters. Block groups divide every tract into
# two or three pieces, but that is nesting, not crossing: each block group is
# INSIDE its tract. A test that only asked "is this tract divided?" would call
# the spine broken.
#
# One per cent is the tolerance at both ends. Below that we are looking at
# digitising noise on a shared river bank, not a boundary that divides anybody.
TOL <- 0.99

# rows = tracts, cols = overlay units, cells = shared land area
crossings <- function(A) {
  At <- rowSums(A); Ag <- colSums(A)
  cross <- A > (1 - TOL) * At &                 # a real piece of the tract
           A < TOL * At &                       # ... but not the whole tract
           A < TOL * rep(Ag, each = nrow(A))    # ... and not the whole unit
  list(split = sum(rowSums(cross) > 0), cross = cross,
       inside = apply(A, 1, max) / At)
}

# --- block groups: the control, and it is a control by construction ---------
bg_area <- tapply(gb$ALAND20, list(substr(gb$GEOID20, 1, 11),
                                   substr(gb$GEOID20, 1, 12)), sum)
bg_area[is.na(bg_area)] <- 0
sp_bg <- crossings(bg_area)

# --- places: the Bureau's own block-to-place assignment ---------------------
baf <- grab(paste0("https://www2.census.gov/geo/docs/maps-data/data/baf2020/",
                   "BlockAssign_ST13_GA.zip"), "BlockAssign_ST13_GA.zip")
bafd <- sub("\\.zip$", "", baf)
if (!dir.exists(bafd)) utils::unzip(baf, exdir = bafd)
ba <- read.delim(file.path(bafd, "BlockAssign_ST13_GA_INCPLACE_CDP.txt"),
                 sep = "|", colClasses = "character")
names(ba) <- c("BLOCKID", "PLACEFP")
ba <- ba[substr(ba$BLOCKID, 3, 5) == CTY, ]
d <- st_drop_geometry(gb)
d$tract <- substr(d$GEOID20, 1, 11)
d$place <- ba$PLACEFP[match(d$GEOID20, ba$BLOCKID)]
stopifnot(!any(is.na(d$place)))
# Blocks in no municipality at all are their own category: unincorporated land
# is where the city limit stops, so it is exactly the thing on the other side
# of the boundary we are counting.
d$place[d$place == ""] <- "none"
pl_area <- tapply(d$ALAND20, list(d$tract, d$place), sum); pl_area[is.na(pl_area)] <- 0
sp_pl <- crossings(pl_area)
dp <- d[d$POP20 > 0, ]
pop_mixed <- sum(tapply(dp$place, dp$tract, function(z) length(unique(z))) > 1)

# --- ZCTAs: the Bureau's own ZCTA-to-tract relationship file ----------------
# Using the published relationship file rather than intersecting the polygons
# myself means the number below is the Census Bureau's own account of how its
# two geographies overlap, not an artifact of my tolerance settings.
zt <- read.delim(grab(paste0(REL, "/tab20_zcta520_tract20_natl.txt"),
                      "tab20_zcta520_tract20_natl.txt"),
                 sep = "|", colClasses = "character", check.names = FALSE)
names(zt)[1] <- sub("^﻿", "", names(zt)[1])
zt$AREALAND_PART <- as.numeric(zt$AREALAND_PART)
zh <- zt[substr(zt$GEOID_TRACT_20, 1, 5) == CTYFIP, ]
stopifnot(length(unique(zh$GEOID_TRACT_20)) == nrow(gt))
zc_area <- tapply(zh$AREALAND_PART, list(zh$GEOID_TRACT_20, zh$GEOID_ZCTA5_20), sum)
zc_area[is.na(zc_area)] <- 0
sp_zc <- crossings(zc_area)

# how many ZCTAs here are in more than one county -- and one place is, too
zcnty <- read.delim(grab(paste0(REL, "/tab20_zcta520_county20_natl.txt"),
                         "tab20_zcta520_county20_natl.txt"),
                    sep = "|", colClasses = "character", check.names = FALSE)
names(zcnty)[1] <- sub("^﻿", "", names(zcnty)[1])
zcnty$AREALAND_PART <- as.numeric(zcnty$AREALAND_PART)
zs <- zcnty[zcnty$GEOID_ZCTA5_20 %in% gz$ZCTA & zcnty$AREALAND_PART > 0, ]
zspan <- tapply(zs$GEOID_COUNTY_20, zs$GEOID_ZCTA5_20, function(x) length(unique(x)))
say("ZCTAs here spanning more than one county: %d of %d",
    sum(zspan > 1), length(zspan))

# Warner Robins, the city that runs off the edge of the county
wr <- gp[gp$NAME == "Warner Robins", ]
wr_in  <- as.numeric(st_area(st_intersection(wr, county))) / 1e6
wr_all <- as.numeric(st_area(wr)) / 1e6
wr_cty <- gt_all$COUNTYFP[st_intersects(prj(gt_all), wr, sparse = FALSE)[, 1]]
wr_cty <- unique(wr_cty[
  as.numeric(st_area(st_intersection(prj(gt_all)[st_intersects(prj(gt_all), wr,
    sparse = FALSE)[, 1], ], wr))) > 1e4])
say("Warner Robins: %.1f of %.1f sq km inside the county; counties touched: %s",
    wr_in, wr_all, paste(sort(wr_cty), collapse = " "))

splits <- data.frame(
  overlay = c("block group", "census place (city limits)", "ZCTA (ZIP approximation)"),
  role    = c("on the spine", "crosscutting", "crosscutting"),
  units   = c(nrow(gg), nrow(gp), nrow(gz)),
  tracts_total = nrow(gt),
  tracts_split = c(sp_bg$split, sp_pl$split, sp_zc$split),
  source  = c("TIGER/Line block land area, aggregated by GEOID prefix",
              "2020 Block Assignment File, block -> place",
              "2020 ZCTA-to-tract relationship file"))
splits$pct_split <- round(100 * splits$tracts_split / splits$tracts_total)
write.csv(splits, "derived/splits.csv", row.names = FALSE)
print(splits[, c("overlay", "role", "units", "tracts_split", "pct_split")])

# Which tracts, one row each, so the figures can shade the split ones rather
# than the reader taking the count on trust. `uid` is the tract's row in the
# geometry written below, which is what map_units.csv is keyed by.
ts <- data.frame(geoid = gt$GEOID, uid = seq_len(nrow(gt)))
pick <- function(cr, ids) as.integer(rowSums(cr)[match(ts$geoid, ids)] > 0)
ts$split_bg    <- pick(sp_bg$cross, rownames(bg_area))
ts$split_place <- pick(sp_pl$cross, rownames(pl_area))
ts$split_zcta  <- pick(sp_zc$cross, rownames(zc_area))
stopifnot(!anyNA(ts), sum(ts$split_bg) == sp_bg$split,
          sum(ts$split_place) == sp_pl$split, sum(ts$split_zcta) == sp_zc$split)
write.csv(ts, "derived/tract_split.csv", row.names = FALSE)

# =========================================================================
# 4.  Geometry out, as long-format rings
# =========================================================================
# Kilometres on EPSG:5070, shifted so the county's lower-left corner is (0,0).
# Absolute Albers coordinates here are around x=1080, y=1230 km; shifting drops
# four characters from every number in the file and changes no shape.
ORG <- st_bbox(county)
LEV <- c(block = "b", bg = "g", tract = "t", county = "c", place = "p",
         zcta = "z", state = "s")
rings <- function(g, lev, digits = 2) {
  gm <- st_geometry(g)
  out <- vector("list", length(gm))
  for (i in seq_along(gm)) {
    p <- st_coordinates(gm[i])
    part <- as.integer(factor(paste(
      p[, "L1"], if ("L2" %in% colnames(p)) p[, "L2"] else 1,
      if ("L3" %in% colnames(p)) p[, "L3"] else 1)))
    out[[i]] <- data.frame(lev = unname(LEV[lev]), uid = i, part = part,
                           x = round((p[, "X"] - ORG["xmin"]) / 1000, digits),
                           y = round((p[, "Y"] - ORG["ymin"]) / 1000, digits))
  }
  do.call(rbind, out)
}
thin <- function(g, tol) st_simplify(g, dTolerance = tol, preserveTopology = TRUE)

# The blocks are simplified by 30 m and every other layer by 15 m, which
# sounds reckless for a figure whose
# whole argument is where two lines do and do not coincide -- so here is the
# arithmetic. The county is about 44 km across and is drawn about 210 pixels
# wide, so one pixel is 210 m on the ground. Simplification moves a vertex by
# at most the tolerance, so two layers that share a boundary can drift apart by
# at most 30 m, an eighth of one pixel. A boundary
# that really does cross a tract crosses it by hundreds of metres or more.
# Left unsimplified the file is 2.2 MB, most of it river bank.
# Georgia itself, so the zoom sequence can start where the identifier does.
# 300 m on a 400 km state is a third of a pixel at the size that panel is drawn.
state <- st_sf(geometry = st_union(prj(gt_all)))
zbox <- st_as_sfc(st_bbox(st_buffer(county, 3000)))
mu <- rbind(
  rings(thin(state, 300), "state"),
  rings(thin(gb, 30), "block"),
  rings(thin(gg, 15), "bg"),
  rings(thin(gt, 15), "tract"),
  rings(thin(st_sf(geometry = county), 15), "county"),
  rings(thin(gp, 15), "place"),
  rings(thin(st_intersection(gz, zbox), 15), "zcta"))
mu <- mu[!is.na(mu$x), ]
np <- ave(mu$x, mu$lev, mu$uid, mu$part, FUN = length)
mu <- mu[np >= 4, ]
write.csv(mu, "derived/map_units.csv", row.names = FALSE)
say("map_units.csv: %s coordinate rows, %s", n(nrow(mu)),
    format(structure(file.size("derived/map_units.csv"), class = "object_size"),
           units = "auto"))
print(table(mu$lev))

ids <- rbind(
  data.frame(lev = "b", uid = seq_len(nrow(gb)), geoid = gb$GEOID20,
             name = paste("Block", substr(gb$GEOID20, 12, 15)), pop = gb$POP20),
  data.frame(lev = "g", uid = seq_len(nrow(gg)), geoid = gg$GEOID,
             name = paste("Block Group", substr(gg$GEOID, 12, 12)), pop = NA),
  data.frame(lev = "t", uid = seq_len(nrow(gt)), geoid = gt$GEOID,
             name = gt$NAMELSAD, pop = NA),
  data.frame(lev = "c", uid = 1L, geoid = CTYFIP, name = CTYNM,
             pop = sum(gb$POP20)),
  data.frame(lev = "s", uid = 1L, geoid = ST, name = "Georgia", pop = NA),
  data.frame(lev = "p", uid = seq_len(nrow(gp)), geoid = gp$GEOID,
             name = gp$NAMELSAD, pop = NA),
  data.frame(lev = "z", uid = seq_len(nrow(gz)), geoid = gz$ZCTA,
             name = paste("ZCTA", gz$ZCTA), pop = NA))
write.csv(ids, "derived/map_ids.csv", row.names = FALSE)

# =========================================================================
# 5.  One block, and the three units it is inside
# =========================================================================
# Chosen by rule, not by eye: of the tracts the Warner Robins city limit runs
# through, take the one with the most people, and inside it the most populous
# block that is inside the city. That way the block picked out in the nesting
# figure is also a block the non-nesting figure has something to say about.
wrfp <- substr(wr$GEOID, 3, 7)
cand <- d[d$place == wrfp, ]
tr_pop <- tapply(cand$POP20, cand$tract, sum)
mixed  <- rownames(pl_area)[rowSums(sp_pl$cross) > 0]
tr_pick <- names(sort(tr_pop[names(tr_pop) %in% mixed], decreasing = TRUE))[1]
bl <- cand[cand$tract == tr_pick, ]
bl <- bl[order(-bl$POP20), ][1, ]
BID <- bl$GEOID20
say("chain block: %s  (pop %d, tract %s)", BID, bl$POP20, tr_pick)

chain <- data.frame(
  level  = c("state", "county", "tract", "block group", "block"),
  digits = c(substr(BID, 1, 2), substr(BID, 3, 5), substr(BID, 6, 11),
             substr(BID, 12, 12), substr(BID, 13, 15)),
  geoid  = c(substr(BID, 1, 2), substr(BID, 1, 5), substr(BID, 1, 11),
             substr(BID, 1, 12), BID),
  lev    = c("s", "c", "t", "g", "b"),
  name   = c("Georgia", CTYNM,
             gt$NAMELSAD[match(substr(BID, 1, 11), gt$GEOID)],
             paste("Block Group", substr(BID, 12, 12)),
             paste("Block", substr(BID, 13, 15))))
chain$id_digits <- nchar(chain$geoid)
chain$uid <- c(1L,
               1L,
               match(substr(BID, 1, 11), gt$GEOID),
               match(substr(BID, 1, 12), gg$GEOID),
               match(BID, gb$GEOID20))
write.csv(chain, "derived/chain.csv", row.names = FALSE)
print(chain[, c("level", "digits", "geoid", "name")])

# =========================================================================
# 6.  Scalars
# =========================================================================
pl_pop <- tapply(d$POP20, d$place, sum)
facts <- data.frame(rbind(
  c("county_name",   CTYNM, "the county mapped throughout"),
  c("county_fips",   CTYFIP, "state 13 + county 153"),
  c("n_blocks",      nrow(gb), "2020 census blocks in the county"),
  c("n_bg",          nrow(gg), "block groups"),
  c("n_tracts",      nrow(gt), "census tracts"),
  c("n_places",      nrow(gp), "census places with land in the county"),
  c("n_zctas",       nrow(gz), "ZCTAs with land in the county"),
  c("pop",           sum(gb$POP20), "2020 population, summed from blocks"),
  c("county_sq_mi",  round(as.numeric(st_area(county)) / 2589988, 1),
    "land and water, Albers"),
  c("tracts_split_bg",    sp_bg$split, "tracts a block group boundary splits"),
  c("tracts_split_place", sp_pl$split, "tracts a city limit splits"),
  c("tracts_split_zcta",  sp_zc$split, "tracts a ZCTA boundary splits"),
  c("tracts_pop_mixed_place", pop_mixed,
    "tracts with population both inside and outside a municipality"),
  c("pop_unincorporated", pl_pop[["none"]],
    "people in the county living in no municipality"),
  c("pop_wr", pl_pop[[wrfp]], "people in the Warner Robins city limits"),
  c("wr_tracts", length(unique(cand$tract)),
    "tracts the Warner Robins city limit reaches into"),
  c("wr_counties", length(wr_cty), "counties Warner Robins lies in"),
  c("wr_sq_km_in_county", round(wr_in, 1), "Warner Robins land inside Houston"),
  c("wr_sq_km_total", round(wr_all, 1), "Warner Robins land in total"),
  c("zctas_multi_county", sum(zspan > 1),
    "of the county's ZCTAs that are in more than one county"),
  c("zcta_max_counties", max(zspan), "counties the widest-spanning ZCTA is in"),
  c("blocks_per_tract", round(nrow(gb) / nrow(gt)), "mean blocks per tract"),
  c("fetch_date", "2026-08-10", "every source above fetched on this date")))
names(facts) <- c("name", "value", "note")
write.csv(facts, "derived/facts.csv", row.names = FALSE)
print(facts)

say("\nwrote: map_units.csv map_ids.csv chain.csv splits.csv tract_split.csv facts.csv")
say("cache/ holds the downloads and can be deleted")

# ---- the rungs above the state ---------------------------------------------
# The brief's one national figure: the four regions and nine divisions, the
# two summary levels above the state that never enter a GEOID. Outlines are
# the course's standard base map (../../../_lib/geo/us-albers.geojson: the
# Bureau's cartographic boundary file on the composite Albers, pre-projected
# to a 1152 x 748.8 frame, y down, so nothing here projects anything). The
# grouping is typed from the Bureau's "Statistical Groupings of States and
# Counties" and checked structurally: 51 units, 9 divisions, 4 regions,
# every state in exactly one of each. `regional-shift` re-derives the same
# grouping and verifies it against the Bureau's published region totals.
us <- st_read("../../../_lib/geo/us-albers.geojson", quiet = TRUE)
st_crs(us) <- NA                     # pre-projected plane units, not degrees
stopifnot(nrow(us) == 51)

DIVS <- list(
  "New England"        = c("CT", "ME", "MA", "NH", "RI", "VT"),
  "Middle Atlantic"    = c("NJ", "NY", "PA"),
  "East North Central" = c("IL", "IN", "MI", "OH", "WI"),
  "West North Central" = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  "South Atlantic"     = c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV"),
  "East South Central" = c("AL", "KY", "MS", "TN"),
  "West South Central" = c("AR", "LA", "OK", "TX"),
  "Mountain"           = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
  "Pacific"            = c("AK", "CA", "HI", "OR", "WA"))
REG_OF_DIV <- c(
  "New England" = "Northeast", "Middle Atlantic" = "Northeast",
  "East North Central" = "Midwest", "West North Central" = "Midwest",
  "South Atlantic" = "South", "East South Central" = "South",
  "West South Central" = "South", "Mountain" = "West", "Pacific" = "West")
div_of <- unlist(lapply(names(DIVS), function(k)
  setNames(rep(k, length(DIVS[[k]])), DIVS[[k]])))
stopifnot(length(div_of) == 51, !anyNA(div_of[us$st]))
us$division <- unname(div_of[us$st])
us$region   <- unname(REG_OF_DIV[us$division])

# state rings in frame coordinates, keyed the way the brief's helpers expect
usm <- do.call(rbind, lapply(seq_len(nrow(us)), function(i) {
  m <- st_coordinates(us[i, ])
  data.frame(uid  = us$st[i],
             part = as.integer(interaction(m[, "L2"], m[, "L1"], drop = TRUE)),
             x = m[, 1], y = m[, 2])
}))
write.csv(usm, "derived/us_map.csv", row.names = FALSE)

# division outlines: dissolve each division's states, keep the union's
# border. The base map was simplified before projection with shared borders
# keeping shared vertices, so the planar union closes without slivers.
usd <- do.call(rbind, lapply(names(DIVS), function(k) {
  u <- st_union(st_geometry(us)[us$division == k])
  m <- st_coordinates(st_cast(u, "MULTILINESTRING"))
  data.frame(division = k, part = as.integer(m[, "L1"]),
             x = round(m[, 1], 1), y = round(m[, 2], 1))
}))
stopifnot(length(unique(usd$division)) == 9)
write.csv(usd, "derived/us_divisions.csv", row.names = FALSE)

# one row per state: grouping, the label anchor the base map carries, and
# drawn area (frame px^2), which decides which states have room for a label
ua <- data.frame(uid = us$st, fips = us$fips, name = us$name,
                 region = us$region, division = us$division,
                 label_x = us$label_x, label_y = us$label_y,
                 area = round(as.numeric(st_area(us))))
write.csv(ua, "derived/us_ids.csv", row.names = FALSE)
say("wrote: us_map.csv us_divisions.csv us_ids.csv (the national figure)")
