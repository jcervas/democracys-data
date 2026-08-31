# ---------------------------------------------------------------------------
# Build the two nationwide OVERLAY layers that sit on the same frame as
# us-albers.geojson:
#
#   us-counties.geojson   2020 counties (3,143: 50 states + DC), from the
#                         TIGER/Line county file the residual-votes chapter
#                         already committed -- read IN PLACE from that
#                         chapter's raw/, never copied here.
#   us-cd-119.geojson     congressional districts of the 119th Congress
#                         (435 + the DC delegate seat), from the Census
#                         cartographic boundary file, fetched at run time
#                         into a temp directory (the zip has no business
#                         being committed twice).
#
# Both are pre-projected into the SHARED FRAME: 1152 x 748.8, y down,
# coordinates rounded to 0.1, the same composite Albers as us-albers --
# CONUS in 29.5/45.5 on -96, Alaska in 55/65 at 0.35 scale, Hawaii in 8/18,
# insets in the identical positions. A figure can draw counties or districts
# over the state outlines and every shared border coincides to the rounding.
#
# HOW THE INSETS ARE PLACED. build-geo.R positions Alaska and Hawaii by
# bounding-box arithmetic on the simplified STATE geometry, and us-frame.json
# records only the final composite fit. So this build re-runs the state
# pipeline (same raw file, same ms_simplify(keep = 0.25)), asserts that the
# recomputed fit matches us-frame.json to full precision -- proof the
# pipeline is still deterministic -- and reads the inset affines off it.
# The affines are then written into us-frame.json's registry so nothing has
# to re-derive them.
#
# Simplification is mapshaper (topology-preserving), once, BEFORE projecting
# -- per-ring thinning opens triangular holes at every junction; see
# build-geo.R. The full-resolution TIGER county file is cut hard: at national
# scale a county border a few frame-pixels long does not need 400 vertices.
#
# RUN FROM INSIDE geo/:  Rscript build-layers.R
# (set GEO_TMP to choose where the temp download/unzip goes)
# ---------------------------------------------------------------------------

source("geo-common.R")
suppressMessages(library(rmapshaper))

TMP <- Sys.getenv("GEO_TMP", tempdir())
dir.create(TMP, showWarnings = FALSE, recursive = TRUE)

KEEP_CTY <- "0.15%"   # of removable vertices in the full-res TIGER county file
KEEP_CD  <- "4%"     # of removable vertices in the 1:500k CD file

CTY_SHP <- "../../03-elections/residual-votes/data/raw/tiger/us/tl_2020_us_county.shp"
CD_URL  <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_cd119_500k.zip"

# ===========================================================================
# 1.  THE FRAME, AND THE INSET AFFINES, RE-DERIVED AND VERIFIED.
# ===========================================================================

FRAME <- load_frame()
fit <- list(xmin = FRAME$fit_xmin, ymax = FRAME$fit_ymax, s = FRAME$fit_s,
            ox = FRAME$fit_ox, oy = FRAME$fit_oy)

states <- st_read(file.path("raw", "cb_2024_us_state_5m"), quiet = TRUE)
states <- states[states$STUSPS %in% c(state.abb, "DC"), c("STATEFP", "STUSPS", "NAME")]
stopifnot(nrow(states) == 51)
states <- ms_simplify(states, keep = 0.25, keep_shapes = TRUE)

conus_st <- st_transform(states[!states$STUSPS %in% c("AK", "HI"), ], FRAME$conus_proj)
ak_st    <- st_transform(states[states$STUSPS == "AK", ],            FRAME$ak_proj)
hi_st    <- st_transform(states[states$STUSPS == "HI", ],            FRAME$hi_proj)

cb <- st_bbox(conus_st); cw <- cb["xmax"] - cb["xmin"]; ch <- cb["ymax"] - cb["ymin"]

move <- function(g, scale, x, y, corner) {          # verbatim from build-geo.R
  b <- st_bbox(g)
  g <- (st_geometry(g) - st_centroid(st_as_sfc(b))) * scale
  b <- st_bbox(g)
  dx <- x - if (corner %in% c("ll", "ul")) b["xmin"] else b["xmax"]
  dy <- y - if (corner %in% c("ll", "lr")) b["ymin"] else b["ymax"]
  g + c(dx, dy)
}

# The move is scale-then-translate with no rotation, so one tracked vertex
# recovers the whole affine.
p0 <- st_coordinates(st_geometry(ak_st))[1, 1:2]
ak_moved <- move(ak_st, 0.35, cb["xmin"] - 0.02 * cw, cb["ymin"] + 0.13 * ch, "ul")
p1 <- st_coordinates(ak_moved)[1, 1:2]
AK_AFF <- list(scale = 0.35, dx = unname(p1[1] - 0.35 * p0[1]),
                             dy = unname(p1[2] - 0.35 * p0[2]))
akb <- st_bbox(ak_moved)
q0 <- st_coordinates(st_geometry(hi_st))[1, 1:2]
hi_moved <- move(hi_st, 1.00, akb["xmax"] + 0.03 * cw, cb["ymin"] - 0.06 * ch, "ul")
q1 <- st_coordinates(hi_moved)[1, 1:2]
HI_AFF <- list(scale = 1, dx = unname(q1[1] - q0[1]), dy = unname(q1[2] - q0[2]))

# Determinism check: the composite this run built must land on the frame
# EXACTLY where the original build put it, or the insets would be placed
# against a fit that no longer exists. If this stops the build, the raw file
# or a library changed under us -- rebuild us-albers first, then this.
st_geometry(ak_st) <- ak_moved; st_geometry(hi_st) <- hi_moved
st_crs(ak_st) <- st_crs(conus_st); st_crs(hi_st) <- st_crs(conus_st)
b2 <- st_bbox(rbind(conus_st, ak_st, hi_st))
fit2 <- fit_of(b2)
stopifnot(
  isTRUE(all.equal(fit2$s,    fit$s,    tolerance = 1e-12)),
  isTRUE(all.equal(fit2$xmin, fit$xmin, tolerance = 1e-9)),
  isTRUE(all.equal(fit2$ymax, fit$ymax, tolerance = 1e-9))
)
say("frame fit re-derived and verified against us-frame.json")

xy_conus <- function(m) frame_xy(m, fit)
xy_ak    <- function(m) frame_xy(cbind(AK_AFF$scale * m[, 1] + AK_AFF$dx,
                                       AK_AFF$scale * m[, 2] + AK_AFF$dy), fit)
xy_hi    <- function(m) frame_xy(cbind(m[, 1] + HI_AFF$dx, m[, 2] + HI_AFF$dy), fit)
xy_for   <- function(fp) if (fp == "02") xy_ak else if (fp == "15") xy_hi else xy_conus

# mapshaper CLI (the sys route): simplify once on the unprojected file, keep
# shared borders shared, keep every shape.
simplify_cli <- function(src, keep, fields, filter, dst) {
  status <- system2("mapshaper", c(
    shQuote(src),
    "-filter", shQuote(filter),
    "-simplify", "weighted", "keep-shapes", keep,
    "-filter-fields", fields,
    "-o", "format=geojson", "precision=0.000001", shQuote(dst)),
    stdout = FALSE, stderr = FALSE)
  stopifnot(status == 0)
  st_read(dst, quiet = TRUE)
}

# ===========================================================================
# 2.  COUNTIES.  TIGER/Line 2020, read in place from the residual-votes
#     chapter. STATEFP <= 56 keeps the 50 states + DC and drops the
#     territories, which have no inset in the composite.
# ===========================================================================

stopifnot(file.exists(CTY_SHP))
prov_local(CTY_SHP, "tl_2020_us_county (in place, residual-votes chapter)")

cty <- simplify_cli(CTY_SHP, KEEP_CTY, "GEOID,NAME,STATEFP",
                    'STATEFP <= "56"', file.path(TMP, "cty-simplified.geojson"))
stopifnot(nrow(cty) == 3143, !any(duplicated(cty$GEOID)))

# Honolulu County legally runs to Kure Atoll, 25 degrees west of Oahu. The
# cb-5m state file generalizes the Northwestern Hawaiian Islands away, so the
# HI inset was sized without them -- left in, they trail across the bottom of
# the frame and off its west edge. Crop the HI counties to the main islands
# (everything east of 161 W keeps Niihau and Kaula, drops Nihoa onward).
hi_i <- which(cty$STATEFP == "15")
suppressWarnings(st_geometry(cty)[hi_i] <- st_geometry(
  st_crop(cty[hi_i, ], xmin = -161, ymin = 15, xmax = -150, ymax = 25)))

cty_parts <- list(
  st_transform(cty[!cty$STATEFP %in% c("02", "15"), ], FRAME$conus_proj),
  st_transform(cty[cty$STATEFP == "02", ],             FRAME$ak_proj),
  st_transform(cty[cty$STATEFP == "15", ],             FRAME$hi_proj))

cty_features <- unlist(lapply(cty_parts, function(part) {
  lapply(seq_len(nrow(part)), function(i) {
    row <- part[i, ]
    list(type = "Feature",
         properties = list(GEOID = row$GEOID, NAME = row$NAME,
                           STATEFP = row$STATEFP),
         geometry = geom_json(frame_coords(st_geometry(row), xy_for(row$STATEFP))))
  })
}), recursive = FALSE)
cty_features <- cty_features[order(vapply(cty_features, function(f) f$properties$GEOID, ""))]

write_fc(cty_features,
  paste("2020 counties (3,143: 50 states + DC), TIGER/Line 2020, simplified",
        "by mapshaper, same composite Albers and frame as us-albers.geojson.",
        "Drawing geometry: shared borders coincide to the 0.1 rounding."),
  "us-counties.geojson")

# ===========================================================================
# 3.  CONGRESSIONAL DISTRICTS, 119th CONGRESS.  Census cartographic boundary
#     file, 1:500k, 2024 vintage -- fetched to a temp dir, recorded in
#     PROVENANCE.tsv, not committed.
# ===========================================================================

cd_zip <- file.path(TMP, basename(CD_URL))
if (!file.exists(cd_zip)) prov_fetch(CD_URL, cd_zip, label = "cb_2024_us_cd119_500k")
unzip(cd_zip, exdir = file.path(TMP, "cd119"), overwrite = TRUE)
cd_shp <- list.files(file.path(TMP, "cd119"), "\\.shp$", full.names = TRUE)[1]

cds <- simplify_cli(cd_shp, KEEP_CD, "GEOID,STATEFP,CD119FP",
                    'STATEFP <= "56" && CD119FP != "ZZ"',
                    file.path(TMP, "cd-simplified.geojson"))
stopifnot(nrow(cds) == 436, !any(duplicated(cds$GEOID)))   # 435 seats + DC delegate

usps_of <- setNames(states$STUSPS, states$STATEFP)
cd_parts <- list(
  st_transform(cds[!cds$STATEFP %in% c("02", "15"), ], FRAME$conus_proj),
  st_transform(cds[cds$STATEFP == "02", ],             FRAME$ak_proj),
  st_transform(cds[cds$STATEFP == "15", ],             FRAME$hi_proj))

cd_features <- unlist(lapply(cd_parts, function(part) {
  lapply(seq_len(nrow(part)), function(i) {
    row <- part[i, ]
    xy  <- xy_for(row$STATEFP)
    lab <- xy(st_coordinates(st_point_on_surface(st_make_valid(st_geometry(row))))[, 1:2, drop = FALSE])
    list(type = "Feature",
         properties = list(GEOID = row$GEOID, STATEFP = row$STATEFP,
                           CD = row$CD119FP, st = unname(usps_of[row$STATEFP]),
                           label_x = lab[1, 1], label_y = lab[1, 2]),
         geometry = geom_json(frame_coords(st_geometry(row), xy)))
  })
}), recursive = FALSE)
cd_features <- cd_features[order(vapply(cd_features, function(f) f$properties$GEOID, ""))]

write_fc(cd_features,
  paste("Congressional districts, 119th Congress (435 + DC delegate; CD",
        "\"00\" = at-large, \"98\" = DC). Census cartographic boundary file",
        "1:500k, 2024 vintage, simplified by mapshaper, same composite",
        "Albers and frame as us-albers.geojson."),
  "us-cd-119.geojson")

# ===========================================================================
# 4.  THE REGISTRY.  One entry per nationwide file, the composite recipe
#     spelled out once per entry so a consumer needs nothing else. The
#     original top-level keys of us-frame.json are preserved by
#     registry_update() for the chapters that already read them.
# ===========================================================================

frame_rec <- list(width = FRAME_W, height = FRAME_H, y = "down")
national <- function(file, n, id_prop, source, vintage, note = NULL) {
  e <- list(file = file, kind = "nation", frame = frame_rec,
            proj = list(conus = FRAME$conus_proj, ak = FRAME$ak_proj, hi = FRAME$hi_proj),
            fit = fit, insets = list(ak = AK_AFF, hi = HI_AFF),
            bounds = bounds_of_file(file), features = n, id_property = id_prop,
            source = source, vintage = vintage, built = format(Sys.Date()))
  if (!is.null(note)) e$note <- note
  e
}
abstract <- function(file, n, id_prop, source, vintage, note) {
  list(file = file, kind = "nation", frame = frame_rec,
       bounds = bounds_of_file(file), features = n, id_property = id_prop,
       source = source, vintage = vintage, built = format(Sys.Date()), note = note)
}

registry_update(list(
  "us-albers.geojson" = national("us-albers.geojson", 51, "st",
    "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_state_5m.zip", "2024"),
  "us-counties.geojson" = national("us-counties.geojson", length(cty_features), "GEOID",
    "labs/03-elections/residual-votes/data/raw/tiger/us/tl_2020_us_county.shp (TIGER/Line 2020, read in place)",
    "2020"),
  "us-cd-119.geojson" = national("us-cd-119.geojson", length(cd_features), "GEOID",
    CD_URL, "2024 (119th Congress)"),
  "us-grid.geojson" = abstract("us-grid.geojson", 51, "st",
    "drawn by build-geo.R (tile-grid layout, electoral-map chapter)", "n/a",
    "abstract cartogram: coordinates are drawn on the frame, not projected"),
  "us-apportionment.geojson" = abstract("us-apportionment.geojson", 50, "st",
    "cartogram-studio solver over cb_2024_us_state_5m (see build-apportionment.js)", "2020 apportionment",
    "abstract cartogram: coordinates are drawn on the frame, not projected")
))

stamp_geo(c("us-counties.geojson", "us-cd-119.geojson", "us-frame.json"),
          "build-layers.R")
prov_report()
