# ---------------------------------------------------------------------------
# THE PROJECTION, and the state outline drawn in it.
#
# Sourced by build-data.R, build-1620.R and rebuild-outline.R.  It was written
# out identically in the first two, which is one copy too many for a thing that
# has to agree with itself: if the arrows and the coastline are projected by
# different code, nothing in the output says so.
# ---------------------------------------------------------------------------

# ---- The book's one national projection -----------------------------------
# The national map is drawn on the same frame as every other chapter's:
# ../../../_lib/geo/us-frame.json records the projection that built the shared
# base maps, and prj() below applies it to arrows and coastline alike, so the
# two cannot disagree with each other -- and this chapter's map cannot
# disagree with the rest of the book's.  (The spherical arithmetic that used
# to live here had the same parallels and meridian; retiring it moved every
# point by a few kilometres of ellipsoid-versus-sphere, and moved the arrows
# and the coastline together.)
#
# Returned units are still projected KILOMETRES, y north-positive: the arrow
# encoding is written in kilometres (encoding.R) and the legend prints them.
# Both renderers consume these x/y directly, which is how the D3 figure and
# the base-R figure are guaranteed to use the same projection: neither of
# them projects anything.  The shared frame's pixels are an exact affine of
# this plane; the constants for going either way sit in us-frame.json.
#
# Georgia keeps its own Albers.  The state panels are this chapter's local
# view, not the shared national frame, and a Georgia drawn on the national
# parallels would be needlessly slanted.

suppressMessages(library(sf))

FRAME <- jsonlite::fromJSON("../../../_lib/geo/us-frame.json")

PRJ_US <- FRAME$conus_proj
PRJ_GA <- "+proj=aea +lat_1=30.6 +lat_2=34.4 +lat_0=32.7 +lon_0=-83.4 +units=km"

prj <- function(lon, lat, p) {
  ok <- !(is.na(lon) | is.na(lat))
  out <- data.frame(x = rep(NA_real_, length(lon)), y = NA_real_)
  if (any(ok)) {
    pt <- st_sfc(st_multipoint(cbind(lon[ok], lat[ok])), crs = 4326)
    xy <- st_coordinates(st_transform(pt, p))
    out$x[ok] <- xy[, "X"]; out$y[ok] <- xy[, "Y"]
  }
  out
}

# ---- the state outline, from the shared base map --------------------------
# us-albers.geojson, CONUS plus the District; Alaska and Hawaii are the base
# map's insets and this map reports them as outside its frame.  The base file
# stores frame pixels, y down; the affine above converts them back to this
# projection's kilometres exactly.
#
# The old outline came from the `maps` package and could not be thinned: its
# rings were stored per state, so any independent thinning opened a
# triangular hole at every three-state junction (the note that used to stand
# here recorded north-west Nevada, the Wyoming corners, the Oklahoma
# panhandle).  The base map's outlines were simplified once, topology-aware,
# by mapshaper -- two states sharing a border keep the same vertices by
# construction -- so the junction holes cannot open at any thinning.  About
# as many points as before, and unlike before, the Florida Keys survive.
state_outline <- function() {
  gj <- jsonlite::fromJSON("../../../_lib/geo/us-albers.geojson",
                           simplifyVector = FALSE)
  km_x <- function(px) FRAME$fit_xmin + (px - FRAME$fit_ox) / FRAME$fit_s
  km_y <- function(px) FRAME$fit_ymax - (px - FRAME$fit_oy) / FRAME$fit_s
  parts <- list(); k <- 0L
  for (f in gj$features) {
    if (f$properties$st %in% c("AK", "HI")) next
    for (poly in f$geometry$coordinates) for (ring in poly) {
      k <- k + 1L
      xs <- vapply(ring, function(p) p[[1]], 0)
      ys <- vapply(ring, function(p) p[[2]], 0)
      parts[[k]] <- data.frame(part = k, x = km_x(xs), y = km_y(ys))
    }
  }
  round(do.call(rbind, parts), 1)
}
