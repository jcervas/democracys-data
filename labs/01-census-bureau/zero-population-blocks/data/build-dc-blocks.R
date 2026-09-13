# build-dc-blocks.R -- the District of Columbia, block by block.
#
# One city drawn at full block resolution, as the close-up of the point that
# sits alone on the right of the density figure: the densest place in the
# country, with a quarter of its blocks holding nobody.
#
# Source: the same TIGER/Line block file as the national map, for state 11.
#   https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_11_tabblock20.zip
# POP20 comes prepended to the geography, so this needs no second source.
#
# Blocks are classed three ways rather than two. A block with nobody on it
# because it is the Potomac is a different fact about a city than a block with
# nobody on it because it is a park, and collapsing the two would let the
# rivers do the argument's work for it.

suppressPackageStartupMessages(library(sf))

TIG <- Sys.getenv("TIGER_DIR", "raw/tiger")
D   <- Sys.getenv("DERIVED", "derived")
W   <- 760L                      # figure width, in viewBox units
dir.create(D, recursive = TRUE, showWarnings = FALSE)

zip <- file.path(TIG, "tl_2020_11_tabblock20.zip")
if (!file.exists(zip))
  download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/",
                       "tl_2020_11_tabblock20.zip"), zip, quiet = TRUE, mode = "wb")
work <- tempfile(); dir.create(work); unzip(zip, exdir = work)
b <- st_read(list.files(work, pattern = "[.]shp$", full.names = TRUE),
             quiet = TRUE)

# NAD83 / UTM 18N: meters, and the right zone for the District, so areas and
# distances on this figure are true rather than Mercator-stretched.
b <- st_transform(b, 26918)
b$POP20   <- as.numeric(as.character(b$POP20))
b$ALAND20 <- as.numeric(as.character(b$ALAND20))
b$cat <- ifelse(b$POP20 > 0, 0L, ifelse(b$ALAND20 > 0, 1L, 2L))

# Enough simplification to halve the vertex count and none of the shape: at
# this width one figure unit is about 25 meters, so anything finer is below
# what the page can draw anyway.
b <- st_simplify(b, dTolerance = 8, preserveTopology = TRUE)
b <- b[!st_is_empty(st_geometry(b)), ]

g  <- st_cast(st_collection_extract(st_geometry(b), "POLYGON"), "POLYGON")
own <- attr(g, "ids")
cat_by_poly <- rep(b$cat, if (is.null(own)) 1L else own)

bb <- st_bbox(g)
sc <- W / (bb["xmax"] - bb["xmin"])
H  <- as.integer(round((bb["ymax"] - bb["ymin"]) * sc))

cc  <- st_coordinates(g)
key <- paste(cc[, "L2"], cc[, "L1"])
rows <- lapply(unique(key), function(k) {
  i <- which(key == k)
  x <- as.integer(round((cc[i, "X"] - bb["xmin"]) * sc))
  y <- as.integer(round((bb["ymax"] - cc[i, "Y"]) * sc))
  d <- c(TRUE, x[-1] != x[-length(x)] | y[-1] != y[-length(y)])
  x <- x[d]; y <- y[d]
  if (length(x) < 3) return(NULL)
  data.frame(poly = cc[i[1], "L2"], ring = cc[i[1], "L1"],
             cat = cat_by_poly[cc[i[1], "L2"]], x = x, y = y)
})
out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
write.csv(out, file.path(D, "dc_blocks.csv"), row.names = FALSE)
write.csv(data.frame(w = W, h = H), file.path(D, "dc_frame.csv"), row.names = FALSE)

fx <- function(k, v, n) data.frame(name = k, value = v, note = n,
                                   stringsAsFactors = FALSE)
M2MI <- 2589988.110336
write.csv(rbind(
  fx("dc_blocks",       nrow(b),                       "DC census blocks"),
  fx("dc_zero",         sum(b$cat > 0),                "blocks with nobody"),
  fx("dc_zero_land",    sum(b$cat == 1),               "of those, having land"),
  fx("dc_zero_water",   sum(b$cat == 2),               "of those, all water"),
  fx("dc_land_sqmi",    round(sum(b$ALAND20) / M2MI, 1), "DC land area, sq mi"),
  fx("dc_empty_sqmi",   round(sum(b$ALAND20[b$cat == 1]) / M2MI, 1),
                        "land area of the empty blocks, sq mi"),
  fx("dc_empty_land_pct", round(100 * sum(b$ALAND20[b$cat == 1]) /
                                sum(b$ALAND20), 1), "that, as a share of DC land (%)")
), file.path(D, "dc_facts.csv"), row.names = FALSE)

cat(sprintf("frame %dx%d | %s blocks -> %s polygons, %s vertices\n", W, H,
            format(nrow(b), big.mark = ","),
            format(length(unique(out$poly)), big.mark = ","),
            format(nrow(out), big.mark = ",")))
