# build-map-geometry.R -- the geometry half of the zero-population-blocks brief.
#
# Takes the national zero-population layer built by the mapshaper pipeline
# (see build-zero-pop-pipeline.sh) plus the Bureau's state outlines, projects
# both to Albers USA, and writes them as long-format tables of INTEGER figure
# coordinates. Storing figure coordinates rather than meters keeps the CSV to
# four digits a number, and guarantees the HTML and the PDF draw the same
# shapes from the same numbers.
#
# Both layers are scaled by the STATE frame, not their own extents, so the
# empty land and the outlines cannot drift apart.

suppressPackageStartupMessages(library(sf))

ZERO   <- Sys.getenv("ZERO_GEOJSON", "raw/us_zero_pop_blocks.geojson")
STATES <- Sys.getenv("STATES_GEOJSON", "raw/states_500k.geojson")
D      <- Sys.getenv("DERIVED", "derived")
W      <- 2000L          # figure width in viewBox units (~2.3 km per unit)
SIMP   <- Sys.getenv("SIMPLIFY", "2%")
dir.create(D, recursive = TRUE, showWarnings = FALSE)

ms <- function(...) {
  cmd <- paste("mapshaper-xl 12gb", paste(..., collapse = " "))
  if (system(cmd, ignore.stderr = TRUE) != 0L) stop("mapshaper failed: ", cmd)
}
tz <- tempfile(fileext = ".json"); ts <- tempfile(fileext = ".json")

# -proj must come last: filtering or projecting in the wrong order leaves
# mapshaper with dangling arc ids, and precision= before a projection would
# round LONGITUDE to whole degrees.
ms("-i", shQuote(ZERO),   "-dissolve -simplify", SIMP, "keep-shapes",
   "-proj albersusa -o", shQuote(tz), "format=geojson precision=1")
ms("-i", shQuote(STATES), "-proj albersusa -o", shQuote(ts),
   "format=geojson precision=1")

# albersusa drops paths it cannot place (Puerto Rico, the island areas),
# leaving empty polygons behind; st_coordinates cannot bind those.
poly <- function(f) {
  g <- st_geometry(st_read(f, quiet = TRUE))
  g <- g[!st_is_empty(g)]                    # BEFORE casting, not after:
  g <- st_collection_extract(g, "POLYGON")   # st_cast over empty geometries
  st_cast(g, "POLYGON")                      # silently drops most of the layer
}
z <- poly(tz); s <- poly(ts)

bb <- st_bbox(s)                                  # the frame, from the states
sc <- W / (bb["xmax"] - bb["xmin"])
H  <- as.integer(round((bb["ymax"] - bb["ymin"]) * sc))

# Long format, one row per vertex, y increasing DOWN the page (screen order).
# Consecutive duplicates after rounding are dropped; a ring that rounds away
# to fewer than three distinct points is dropped with it.
#
# Rings are keyed by POLYGON as well as by ring, and that pairing has to
# survive into the figure. The dissolved empty-land layer is full of holes --
# a populated town inside an empty county reads as an interior ring -- so a
# ring drawn on its own gets painted as fill and the hole disappears. Both
# renderings therefore draw one path per polygon, all of its rings together,
# under an even-odd fill rule.
flatten <- function(g) {
  cc <- st_coordinates(g)
  key <- paste(cc[, "L2"], cc[, "L1"])            # polygon, then ring
  ord <- unique(key)
  out <- lapply(ord, function(k) {
    i <- which(key == k)
    x <- as.integer(round((cc[i, "X"] - bb["xmin"]) * sc))
    y <- as.integer(round((bb["ymax"] - cc[i, "Y"]) * sc))
    d <- c(TRUE, x[-1] != x[-length(x)] | y[-1] != y[-length(y)])
    x <- x[d]; y <- y[d]
    if (length(x) < 3) return(NULL)
    data.frame(poly = cc[i[1], "L2"], ring = cc[i[1], "L1"], x = x, y = y)
  })
  do.call(rbind, out[!vapply(out, is.null, logical(1))])
}

zf <- flatten(z); sf_ <- flatten(s)
write.csv(zf, file.path(D, "map_zero.csv"),   row.names = FALSE)
write.csv(sf_, file.path(D, "map_states.csv"), row.names = FALSE)
write.csv(data.frame(w = W, h = H), file.path(D, "map_frame.csv"), row.names = FALSE)

# Land area of the empty blocks, per state, from the ALAND20 the Bureau
# publishes on every block. ALAND20 excludes water, so this is the land
# figure whether or not water blocks were kept in the layer.
# Attributes via mapshaper rather than sf: GeoJSON's large integers come back
# from st_read as a list column that will not coerce.
ta <- tempfile(fileext = ".csv")
ms("-i", shQuote(ZERO), "-o", shQuote(ta), "format=csv")
zl <- read.csv(ta, colClasses = c(STATEFP = "character"), stringsAsFactors = FALSE)
a  <- tapply(as.numeric(zl$ALAND20), zl$STATEFP, sum)
# N_BLOCKS counts only the blocks in the drawn layer, which is the land-only
# one. The gap between this and the API's zero-block count is the water blocks
# -- real, empty, and deliberately not on the map.
b  <- tapply(as.numeric(zl$N_BLOCKS), zl$STATEFP, sum)
zl <- data.frame(STATEFP = names(a),
                 zero_land_sqmi = round(as.numeric(a) / 2589988.110336, 1),
                 zero_blocks_land = as.integer(b[names(a)]),
                 stringsAsFactors = FALSE)
write.csv(zl, file.path(D, "zero_land.csv"), row.names = FALSE)

cat(sprintf("frame %dx%d | empty land: %s polygons, %s rings, %s vertices | states: %s rings\n",
            W, H,
            format(length(unique(zf$poly)), big.mark = ","),
            format(nrow(unique(zf[c("poly", "ring")])), big.mark = ","),
            format(nrow(zf), big.mark = ","),
            format(nrow(unique(sf_[c("poly", "ring")])), big.mark = ",")))

# ---- national totals -------------------------------------------------------
# The headline numbers are sums over the per-state tables. Writing them down
# here, once, means the prose and the figures read the same value instead of
# each recomputing it.
sb <- read.csv(file.path(D, "state_blocks.csv"), stringsAsFactors = FALSE)
hp <- read.csv(file.path(D, "block_pop_hist.csv"), stringsAsFactors = FALSE)
cu <- cumsum(hp$blocks); ih <- hp[hp$pop > 0, ]
fx <- function(k, v, note) data.frame(name = k, value = v, note = note,
                                      stringsAsFactors = FALSE)
facts <- rbind(
  fx("blocks_total",     sum(sb$blocks),       "census blocks, 50 states + DC + PR"),
  fx("blocks_zero",      sum(sb$zero_blocks),  "blocks recording no population"),
  fx("pop_total",        sum(sb$pop),          "2020 census population counted"),
  fx("pct_zero",         round(100 * sum(sb$zero_blocks) / sum(sb$blocks), 2),
                                               "share of blocks with nobody (%)"),
  fx("blocks_zero_land", sum(zl$zero_blocks_land), "empty blocks having land area"),
  fx("blocks_zero_water", sum(sb$zero_blocks) - sum(zl$zero_blocks_land),
                                               "empty blocks that are water only"),
  fx("zero_land_sqmi",   round(sum(zl$zero_land_sqmi)), "land area of empty blocks (sq mi)"),
  fx("us_land_sqmi",     3533038,              "U.S. land area, sq mi (Census 2020)"),
  fx("median_block_pop", hp$pop[which(cu >= sum(hp$blocks) / 2)[1]], "median block population"),
  fx("median_inhab_pop", ih$pop[which(cumsum(ih$blocks) >= sum(ih$blocks) / 2)[1]],
                                               "median population, inhabited blocks"),
  fx("pct_under_10",     round(100 * sum(hp$blocks[hp$pop <= 10]) / sum(hp$blocks), 1),
                                               "share of blocks holding 10 people or fewer (%)")
)
write.csv(facts, file.path(D, "facts.csv"), row.names = FALSE)
cat("wrote facts.csv\n")
