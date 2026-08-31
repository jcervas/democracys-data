# ---------------------------------------------------------------------------
# Build the two geographic base maps shared by every chapter:
#
#   us-albers.geojson   50 states + DC, composite Albers (Alaska scaled and
#                       moved below the Southwest, Hawaii beside it), built
#                       from the Census cartographic boundary file -- the
#                       generalized TIGER/Line -- not borrowed from anyone
#                       else's basemap.
#   us-grid.geojson     the equal-weight cartogram: one square per state
#                       (51, DC included), same size, roughly geographic
#                       positions. The layout is the tile grid the
#                       electoral-map chapter uses.
#
# Both are written in the SHARED FRAME all three base maps use: 1152 x 748.8,
# y down (SVG convention), coordinates rounded to 0.1. Nothing that reads
# these files projects anything: draw with d3.geoIdentity() or base-R
# polygon(). The third base map in the set, us-apportionment.geojson, is
# built by build-apportionment.js from the cartogram-studio solver.
#
# GEOMETRY NOTES
#
#   Simplification is rmapshaper (topology-preserving), never per-ring
#   thinning. The wind-map chapter records why: rings thinned independently
#   choose different vertices along shared borders, and every state junction
#   grows a triangular hole that only shows once the page can be dark.
#
#   The composite projection is the standard construction: CONUS in Albers
#   29.5/45.5 centred on -96 (the same parameters as the wind-map chapter),
#   Alaska in Albers 55/65 on -154 at 0.35 scale, Hawaii in Albers 8/18 on
#   -157, each moved into the empty ocean south of the mainland.
#
# RUN FROM INSIDE geo/:  Rscript build-geo.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive. Downloads go through prov_fetch(),
# which records url, bytes, hash and row count in PROVENANCE.tsv and prints a
# banner when a source moves under us. See ../provenance.R. If the helper is
# missing the build still runs.
if (file.exists("../provenance.R")) {
  source("../provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}

dir.create("raw", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE, timeout = 900)
suppressMessages({ library(sf); library(rmapshaper); library(jsonlite) })
sf_use_s2(FALSE)

say <- function(...) cat(..., "\n", sep = "")

# The shared frame. 1152 x 748.8 is the cartogram studio's design frame; the
# other two base maps adopt it so a figure can swap basemaps without touching
# its viewBox.
FRAME_W <- 1152
FRAME_H <- 748.8
MARGIN  <- 10

# Write a FeatureCollection with the frame recorded in the file itself, so a
# reader opening the .geojson cold learns the coordinate convention without
# finding the README.
write_fc <- function(features, note, file) {
  fc <- list(
    type = "FeatureCollection",
    frame = list(width = FRAME_W, height = FRAME_H, y = "down"),
    note = note,
    features = features
  )
  writeLines(toJSON(fc, auto_unbox = TRUE, digits = NA), file)
  say("wrote ", file, " (", length(features), " features, ",
      round(file.size(file) / 1024), " KB)")
}

# ===========================================================================
# 1.  THE COMPOSITE ALBERS MAP.
#
#     https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_state_5m.zip
#     Cartographic boundary file, 1:5,000,000, 2024 vintage -- the same
#     vintage the mapping chapter uses for counties.
# ===========================================================================

zip_url  <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_state_5m.zip"
zip_path <- file.path("raw", "cb_2024_us_state_5m.zip")
if (!file.exists(zip_path)) prov_fetch(zip_url, zip_path, label = "cb_2024_us_state_5m")
unzip(zip_path, exdir = file.path("raw", "cb_2024_us_state_5m"), overwrite = TRUE)

states <- st_read(file.path("raw", "cb_2024_us_state_5m"), quiet = TRUE)
keep   <- states$STUSPS %in% c(state.abb, "DC")
states <- states[keep, c("STATEFP", "STUSPS", "NAME")]
stopifnot(nrow(states) == 51)

# Simplify BEFORE projecting, once, for all three pieces, so shared borders
# keep shared vertices.
states <- ms_simplify(states, keep = 0.25, keep_shapes = TRUE)

aea <- function(lat1, lat2, lon0, lat0)
  sprintf("+proj=aea +lat_1=%s +lat_2=%s +lat_0=%s +lon_0=%s +units=km", lat1, lat2, lat0, lon0)

conus <- st_transform(states[!states$STUSPS %in% c("AK", "HI"), ], aea(29.5, 45.5, -96, 37.5))
ak    <- st_transform(states[states$STUSPS == "AK", ],            aea(55, 65, -154, 50))
hi    <- st_transform(states[states$STUSPS == "HI", ],            aea(8, 18, -157, 13))

# Place the insets by bounding-box arithmetic against CONUS, in km, y still up.
cb <- st_bbox(conus); cw <- cb["xmax"] - cb["xmin"]; ch <- cb["ymax"] - cb["ymin"]

move <- function(g, scale, x, y, corner) {
  # scale about the centroid of the bbox, then put the named corner at (x, y)
  b <- st_bbox(g)
  g <- (st_geometry(g) - st_centroid(st_as_sfc(b))) * scale
  b <- st_bbox(g)
  dx <- x - if (corner %in% c("ll", "ul")) b["xmin"] else b["xmax"]
  dy <- y - if (corner %in% c("ll", "lr")) b["ymin"] else b["ymax"]
  g + c(dx, dy)
}

# Alaska: 0.35 scale, lower-left region, its top edge a little above the
# CONUS bottom edge. Hawaii: full scale, to Alaska's right, below Arizona.
st_geometry(ak) <- move(ak, 0.35, cb["xmin"] - 0.02 * cw, cb["ymin"] + 0.13 * ch, "ul")
akb <- st_bbox(ak)
st_geometry(hi) <- move(hi, 1.00, akb["xmax"] + 0.03 * cw, cb["ymin"] - 0.06 * ch, "ul")

st_crs(ak) <- st_crs(conus); st_crs(hi) <- st_crs(conus)
composite <- rbind(conus, ak, hi)

# Fit the whole composite to the frame and flip y to screen convention.
b <- st_bbox(composite)
s <- min((FRAME_W - 2 * MARGIN) / (b["xmax"] - b["xmin"]),
         (FRAME_H - 2 * MARGIN) / (b["ymax"] - b["ymin"]))
ox <- (FRAME_W - s * (b["xmax"] - b["xmin"])) / 2
oy <- (FRAME_H - s * (b["ymax"] - b["ymin"])) / 2

to_frame <- function(m)
  cbind(round(ox + (m[, 1] - b["xmin"]) * s, 1),
        round(oy + (b["ymax"] - m[, 2]) * s, 1))

feature_of <- function(row) {
  g <- st_geometry(row)[[1]]
  if (inherits(g, "POLYGON")) g <- list(g)
  coords <- lapply(g, function(poly) lapply(poly, to_frame))
  # a label anchor guaranteed to fall inside the state, not just at the
  # centroid (which for Florida or Michigan can land in the sea)
  lab <- to_frame(st_coordinates(st_point_on_surface(st_geometry(row)))[, 1:2, drop = FALSE])
  list(
    type = "Feature",
    properties = list(st = row$STUSPS, fips = row$STATEFP, name = row$NAME,
                      label_x = lab[1, 1], label_y = lab[1, 2]),
    geometry = list(type = "MultiPolygon", coordinates = coords)
  )
}

composite <- composite[order(composite$STUSPS), ]
write_fc(
  lapply(seq_len(nrow(composite)), function(i) feature_of(composite[i, ])),
  paste("50 states + DC, composite Albers (CONUS 29.5/45.5 on -96; AK 0.35x;",
        "HI). Census cartographic boundary file, 1:5m, 2024 vintage,",
        "simplified by mapshaper. Pre-projected plane coordinates."),
  "us-albers.geojson"
)

# The frame transform, written out so that OTHER builds can put geometry on
# this exact frame. A chapter that draws counties (or anything else) projects
# with `conus_proj` and applies frame_x/frame_y below; its coordinates then
# overlay us-albers.geojson exactly. Without this file every chapter would
# refit its own bbox and the frames would drift apart by a margin nobody set.
#
#   frame_x = fit_ox + (X - fit_xmin) * fit_s
#   frame_y = fit_oy + (fit_ymax - Y) * fit_s      (y flips to screen)
#
# X/Y in the projection's km. conus_box is the CONUS states' own bbox already
# on the frame -- the crop a CONUS-only figure wants for its viewBox.
cbf <- st_bbox(conus)
cx0 <- ox + (cbf["xmin"] - b["xmin"]) * s
cy0 <- oy + (b["ymax"] - cbf["ymax"]) * s
writeLines(toJSON(list(
  frame = list(width = FRAME_W, height = FRAME_H, y = "down"),
  conus_proj = aea(29.5, 45.5, -96, 37.5),
  ak_proj = aea(55, 65, -154, 50),
  hi_proj = aea(8, 18, -157, 13),
  fit_xmin = unname(b["xmin"]), fit_ymax = unname(b["ymax"]),
  fit_s = unname(s), fit_ox = unname(ox), fit_oy = unname(oy),
  conus_box = list(x = round(unname(cx0), 1), y = round(unname(cy0), 1),
                   w = round(unname((cbf["xmax"] - cbf["xmin"]) * s), 1),
                   h = round(unname((cbf["ymax"] - cbf["ymin"]) * s), 1))
), auto_unbox = TRUE, digits = NA), "us-frame.json")
say("wrote us-frame.json (the frame transform, for other builds)")

# ===========================================================================
# 2.  THE EQUAL-WEIGHT GRID.  One square per state, DC included.
#     The layout is the electoral-map chapter's tile grid.
# ===========================================================================

grid <- read.csv(text = "st,col,row
AK,1,1
ME,11,1
VT,10,2
NH,11,2
WA,1,3
ID,2,3
MT,3,3
ND,4,3
MN,5,3
WI,6,3
MI,7,3
NY,9,3
MA,10,3
RI,11,3
OR,1,4
NV,2,4
WY,3,4
SD,4,4
IA,5,4
IL,6,4
IN,7,4
OH,8,4
PA,9,4
NJ,10,4
CT,11,4
CA,1,5
UT,2,5
CO,3,5
NE,4,5
MO,5,5
KY,6,5
WV,7,5
VA,8,5
MD,9,5
DE,10,5
AZ,2,6
NM,3,6
KS,4,6
AR,5,6
TN,6,6
NC,7,6
SC,8,6
DC,9,6
OK,4,7
LA,5,7
MS,6,7
AL,7,7
GA,8,7
HI,1,8
TX,4,8
FL,9,8")
stopifnot(nrow(grid) == 51, !any(duplicated(grid$st)),
          !any(duplicated(paste(grid$col, grid$row))))

fips_of <- setNames(states$STATEFP, states$STUSPS)
name_of <- setNames(states$NAME,    states$STUSPS)

SIDE <- 84; GAP <- 8
ncol_ <- max(grid$col); nrow_ <- max(grid$row)
gw <- ncol_ * SIDE + (ncol_ - 1) * GAP
gh <- nrow_ * SIDE + (nrow_ - 1) * GAP
stopifnot(gw <= FRAME_W - 2 * MARGIN, gh <= FRAME_H - 2 * MARGIN)
x0 <- (FRAME_W - gw) / 2
y0 <- (FRAME_H - gh) / 2

square <- function(col, row) {
  x <- x0 + (col - 1) * (SIDE + GAP)
  y <- y0 + (row - 1) * (SIDE + GAP)
  list(list(  # one ring, closed, clockwise in screen coords
    c(x, y), c(x + SIDE, y), c(x + SIDE, y + SIDE), c(x, y + SIDE), c(x, y)
  ))
}

grid <- grid[order(grid$st), ]
write_fc(
  lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    list(
      type = "Feature",
      properties = list(
        st = g$st, fips = unname(fips_of[g$st]), name = unname(name_of[g$st]),
        col = g$col, row = g$row,
        label_x = x0 + (g$col - 1) * (SIDE + GAP) + SIDE / 2,
        label_y = y0 + (g$row - 1) * (SIDE + GAP) + SIDE / 2
      ),
      geometry = list(type = "Polygon", coordinates = square(g$col, g$row))
    )
  }),
  paste("Equal-weight state cartogram: one", SIDE, "px square per state,",
        "DC included, tile-grid positions. Every state counts the same."),
  "us-grid.geojson"
)

prov_report()
