# Repair pass over us-apportionment.geojson, run by build-apportionment.js.
#
# The studio's outlines were quantised to 0.5 px for browser drawing, which
# left self-touching rings in 30 of the 50 states -- invisible when filled,
# fatal to any planar operation (st_area, st_intersection, point-in-polygon).
# The two TIGER-derived base maps are valid; this makes the third match.
#
# st_make_valid changes no state's area by more than 0.008 px^2 in the
# 1152 x 748.8 frame. Coordinates are re-rounded to 0.01 (0.1 re-opens five
# rings). The file's frame/note members and every property pass through
# untouched: only geometry coordinates are rewritten.

suppressMessages({ library(sf); library(jsonlite) })
sf_use_s2(FALSE)

f  <- "us-apportionment.geojson"
fc <- fromJSON(f, simplifyVector = FALSE)

fc$features <- lapply(fc$features, function(feat) {
  g <- st_multipolygon(lapply(feat$geometry$coordinates, function(poly)
    lapply(poly, function(ring)
      do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]]))))))
  g <- st_make_valid(g)
  if (inherits(g, "POLYGON"))          g <- st_multipolygon(list(g))
  if (inherits(g, "GEOMETRYCOLLECTION"))
    g <- st_union(st_collection_extract(st_sfc(g), "POLYGON"))[[1]]
  if (inherits(g, "POLYGON"))          g <- st_multipolygon(list(g))
  g <- st_multipolygon(lapply(g, function(poly) lapply(poly, function(r) round(r, 2))))
  stopifnot(st_is_valid(g))
  feat$geometry$coordinates <- lapply(g, function(poly)
    lapply(poly, function(ring) lapply(seq_len(nrow(ring)), function(i) ring[i, ])))
  feat
})

writeLines(toJSON(fc, auto_unbox = TRUE, digits = NA), f)
cat("repaired ", f, ": all geometries valid\n", sep = "")
