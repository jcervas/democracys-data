# ---------------------------------------------------------------------------
# Build the PER-STATE base maps in states/: geometry that at least two
# chapters draw, promoted out of the chapter that first collected it and
# re-projected so the STATE fills the standard 1152 x 748.8 y-down plane.
#
#   states/TX-2022.geojson      TX congressional plan enacted 2021 (38)
#   states/TX-2026.geojson      TX plan enacted 2025, first used 2026 (38)
#   states/FL-2022.geojson      FL plan used 2022-2024 (28)
#   states/FL-2026.geojson      FL plan first used 2026 (28)
#   states/GA-vtd-2020.geojson  GA precincts, Nov 2020 general (dissolved)
#   states/GA-vtd-2024.geojson  GA precincts, Nov 2024 general (dissolved)
#
# The sources stay in their chapters' data/raw (mid-decade, mid-decade-
# florida, ga-precinct-returns) -- they are read IN PLACE, never copied here.
# The district files are Dave's Redistricting exports; the GA precincts are
# the Georgia SoS shapefiles. Analytic attributes (returns, demographics)
# stay in the chapters; these files carry identifying properties only.
#
# EACH STATE GETS ONE FRAME, NOT ONE PER FILE. TX-2022 and TX-2026 are
# fitted with the same transform (computed over the union of both plans'
# bboxes), so the old and new plan overlay exactly -- same for FL and for
# the two GA vintages. Every frame is recorded in us-frame.json's registry:
#
#   frame_x = ox + (X - xmin) * s
#   frame_y = oy + (ymax - Y) * s        X/Y in conus_proj km
#
# All three states are CONUS, so the projection is the shared frame's
# conus_proj -- district shapes here are the same shapes a national figure
# draws, just fitted to the state's own plane.
#
# GA's 2020 shapefile declares a bare GRS-1980 geographic CRS with no datum;
# it is NAD83 in everything but name (the 2024 file says so), and the build
# assigns EPSG:4269 before projecting.
#
# The GA files carry duplicate CTYSOSID rows (noncontiguous precincts split
# into parts); the build dissolves on CTYSOSID so the id is a real key.
#
# RUN FROM INSIDE geo/:  Rscript build-states.R
# (set GEO_TMP to choose where mapshaper's intermediates go)
# ---------------------------------------------------------------------------

source("geo-common.R")

TMP <- Sys.getenv("GEO_TMP", tempdir())
dir.create(TMP, showWarnings = FALSE, recursive = TRUE)
dir.create("states", showWarnings = FALSE)

KEEP_PLAN <- "10%"   # district plans: block-resolution exports, state scale
KEEP_VTD  <- "3%"    # GA precincts: 27 MB shapefiles, ~2,700 shapes

MID  <- "../../06-putting-data-together/mid-decade/data/raw"
MIDF <- "../../06-putting-data-together/mid-decade-florida/data/raw"
GA   <- "../../03-elections/ga-precinct-returns/data/raw"

FRAME  <- load_frame()
CPROJ  <- FRAME$conus_proj
built  <- format(Sys.Date())
new_entries <- list()
written <- character()

ms <- function(src, dst, args) {
  status <- system2("mapshaper", c(shQuote(src), args,
                    "-o", "format=geojson", "precision=0.000001", shQuote(dst)),
                    stdout = FALSE, stderr = FALSE)
  stopifnot(status == 0)
  dst
}

# Simplify in place-of-origin coordinates (once, topology-preserving, before
# projecting -- the same doctrine as the national builds), then project to
# the shared conus_proj and hand back an sf in km.
prep <- function(src, keep, extra = character()) {
  dst <- file.path(TMP, paste0(basename(src), ".simplified.geojson"))
  ms(src, dst, c(extra, "-simplify", "weighted", "keep-shapes", keep))
  g <- st_read(dst, quiet = TRUE)
  if (is.na(st_crs(g)) || is.na(st_crs(g)$epsg)) st_crs(g) <- 4269
  st_transform(g, CPROJ)
}

# Fit shared by every file of one state, then write each file against it.
state_features <- function(g, fit, props_of) {
  lapply(seq_len(nrow(g)), function(i) {
    row <- g[i, ]
    lab <- frame_xy(st_coordinates(st_point_on_surface(
             st_make_valid(st_geometry(row))))[, 1:2, drop = FALSE], fit)
    p <- props_of(row)
    p$label_x <- lab[1, 1]; p$label_y <- lab[1, 2]
    list(type = "Feature", properties = p,
         geometry = geom_json(frame_coords(st_geometry(row),
                                           function(m) frame_xy(m, fit))))
  })
}

entry_state <- function(file, st, fit, n, id_prop, source, vintage, mate) {
  list(file = file, kind = "state", st = st,
       frame = list(width = FRAME_W, height = FRAME_H, y = "down"),
       proj = CPROJ, fit = fit, bounds = bounds_of_file(file),
       features = n, id_property = id_prop, source = source,
       vintage = vintage, shares_frame_with = mate, built = built)
}

# ===========================================================================
# 1.  THE TX AND FL PLAN PAIRS (Dave's Redistricting exports, committed by
#     the mid-decade chapters).
# ===========================================================================

plan_pair <- function(st, src_old, src_new, n_expect, vny) {
  if (!file.exists(src_old) || !file.exists(src_new)) {
    say("SKIP ", st, ": missing ", src_old, " or ", src_new)
    return(invisible(NULL))
  }
  prov_local(src_old, paste0(st, " old plan (in place)"))
  prov_local(src_new, paste0(st, " new plan (in place)"))
  old <- prep(src_old, KEEP_PLAN); new <- prep(src_new, KEEP_PLAN)
  stopifnot(nrow(old) == n_expect, nrow(new) == n_expect)
  fit <- fit_of(st_bbox(c(st_geometry(old), st_geometry(new))))
  for (k in 1:2) {
    g   <- if (k == 1) old else new
    vny_k <- vny[k]
    out <- sprintf("states/%s-%s.geojson", st, vny_k)
    g <- g[order(as.integer(g$id)), ]
    feats <- state_features(g, fit, function(row)
      list(st = st, district = as.integer(row$id),
           name = row[["state-district"]]))
    write_fc(feats, paste0(st, " congressional plan first used ", vny_k,
      " (", n_expect, " districts), Dave's Redistricting export, simplified",
      " by mapshaper, projected with the shared conus_proj and fitted so ",
      st, " fills the frame. ", st, "-", vny[1], " and ", st, "-", vny[2],
      " share this fit and overlay exactly."), out)
    src <- if (k == 1) src_old else src_new
    new_entries[[out]] <<- entry_state(out, st, fit, n_expect, "district",
      paste0(sub("\\.\\./\\.\\./", "labs/", src),
             " (Dave's Redistricting export, read in place)"),
      paste0("plan first used ", vny_k),
      sprintf("states/%s-%s.geojson", st, vny[c(2, 1)][k]))
    written <<- c(written, out)
  }
}

plan_pair("TX", file.path(MID,  "TX-2022.geojson"), file.path(MID,  "TX-2026.geojson"), 38, c("2022", "2026"))
plan_pair("FL", file.path(MIDF, "FL-2022.geojson"), file.path(MIDF, "FL-2026.geojson"), 28, c("2022", "2026"))

# ===========================================================================
# 2.  THE GA PRECINCT PAIR (Georgia SoS shapefiles, committed by the
#     ga-precinct-returns chapter).
# ===========================================================================

ga20 <- file.path(GA, "shp2020", "VTD2020-Shape.shp")
ga24 <- file.path(GA, "shp2024", "GaPrec_2024-Website-Shapefile.shp")
if (file.exists(ga20) && file.exists(ga24)) {
  prov_local(ga20, "GA VTDs 2020 (in place)")
  prov_local(ga24, "GA VTDs 2024 (in place)")
  dis <- c("-dissolve", "CTYSOSID",
           "copy-fields=FIPS2,CTYNAME,PRECINCT_I,PRECINCT_N")
  g20 <- prep(ga20, KEEP_VTD, dis)
  g24 <- prep(ga24, KEEP_VTD, dis)
  stopifnot(!any(duplicated(g20$CTYSOSID)), !any(duplicated(g24$CTYSOSID)),
            nrow(g20) > 2500, nrow(g24) > 2500)
  fit <- fit_of(st_bbox(c(st_geometry(g20), st_geometry(g24))))
  for (k in 1:2) {
    g <- if (k == 1) g20 else g24; yr <- c("2020", "2024")[k]
    out <- sprintf("states/GA-vtd-%s.geojson", yr)
    g <- g[order(g$CTYSOSID), ]
    feats <- state_features(g, fit, function(row)
      list(id = row$CTYSOSID, fips2 = row$FIPS2, county = row$CTYNAME,
           precinct = row$PRECINCT_I, name = row$PRECINCT_N))
    write_fc(feats, paste0("Georgia precincts (VTDs), November ", yr,
      " general (", nrow(g), " after dissolving split precincts on",
      " CTYSOSID), Georgia SoS shapefile, simplified by mapshaper,",
      " projected with the shared conus_proj and fitted so GA fills the",
      " frame. The 2020 and 2024 files share this fit."), out)
    new_entries[[out]] <- entry_state(out, "GA", fit, nrow(g), "id",
      paste0(sub("\\.\\./\\.\\./", "labs/", if (k == 1) ga20 else ga24),
             " (Georgia SoS, read in place)"),
      paste0("November ", yr, " general"),
      sprintf("states/GA-vtd-%s.geojson", c("2024", "2020")[k]))
    written <- c(written, out)
  }
} else {
  say("SKIP GA: precinct shapefiles not found (the chapter's raw/ is a",
      " manual download; see its data/README.md)")
}

# ===========================================================================
# 3.  REGISTRY + STAMPS.
# ===========================================================================

if (length(new_entries)) registry_update(new_entries)
stamp_geo(c(written, "us-frame.json"), "build-states.R")
prov_report()
