# ---------------------------------------------------------------------------
# Geometry for the maps in redlining-brief.Rmd
#
# WHAT WAS MISSING. A lab about the 1937 residential security maps shipped no
# HOLC polygon at all: `derived/tracts.csv` carries a GEOID and a grade letter and no
# coordinates. The brief's Step 2 figure was therefore a schematic that said so
# in its own caption. This script fetches the digitised maps themselves so the
# brief can draw the actual object it is about.
#
# SOURCES  (fetched 2026-08-10)
#
#   HOLC areas, national
#     https://dsl.richmond.edu/panorama/redlining/static/mappinginequality.json
#     10,563,682 bytes, FeatureCollection, 10,154 features, 314 city surveys.
#     Properties: area_id city state city_survey category grade label
#                 residential commercial industrial fill
#     `grade` is the grade key (NOT `holc_grade`, which does not exist here and
#     returns NULL for every feature). `fill` is HOLC's own colour for the
#     grade, so the maps are drawn in the palette of the source rather than one
#     invented here.
#
#   HOLC areas, per city (used for the appraisal sheet lookup)
#     https://dsl.richmond.edu/panorama/redlining/static/citiesData/<KEY>/geojson.json
#     KEY is <STATE><City><Year>. The year is not in the national file, so the
#     key is resolved by probing and validating the response -- a wrong year
#     returns the site's 1,390-byte HTML shell with status 200, not a 404, so
#     the check is "does it parse as a FeatureCollection", not the status code.
#     Verified: OHCleveland1939 (231,460 bytes, 192 features),
#               PAPhiladelphia1937 (92,010 bytes, 83 features).
#
#   Area description sheet (the appraiser's own words)
#     https://dsl.richmond.edu/panorama/redlining/static/citiesData/<KEY>/areaDescriptions/<LABEL>.json
#
#   Census tract polygons: TIGER/Line 2020, already cached in tiger/.
#
# CREDIT. Nelson, Winling, Marciano, Connolly et al., "Mapping Inequality:
# Redlining in New Deal America", Digital Scholarship Lab, University of
# Richmond, CC BY-NC-SA. The brief credits it in "Sources"; the lab credits it
# in "Where the data came from".
#
# WHY CLEVELAND AND PHILADELPHIA. Cleveland has the largest within-city A-vs-D
# gap in cities.csv and is what Step 6 asserts; Philadelphia is the largest
# reversal and is what Step 8 asserts. Cleveland is drawn; both are measured.
#
# GEOID IS TEXT. A tract identifier is 11 digits and the ones in California
# (state 06) begin with a zero. Read as a number they lose the leading digit,
# match nothing in TIGER, and silently drop a third of the file. Both this
# script and the brief read the column with colClasses = c(GEOID = "character").
#
# FULL RESOLUTION FOR NUMBERS, SIMPLIFIED FOR DRAWING. Every share, count and
# overlap in the outputs is computed from unsimplified geometry in projected
# metres. Simplification happens afterwards and only to the coordinates that
# get written out, so no reported number depends on how coarse the drawing is.
#
# BUILD SCRIPT -- may use packages. Run from this directory:
#   Rscript build-brief-figures.R
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
dir.create("raw", showWarnings = FALSE)

suppressPackageStartupMessages({library(sf); library(jsonlite)})
sf_use_s2(FALSE)
options(stringsAsFactors = FALSE)

MI_NATIONAL <- "https://dsl.richmond.edu/panorama/redlining/static/mappinginequality.json"
MI_CITY     <- "https://dsl.richmond.edu/panorama/redlining/static/citiesData/%s/geojson.json"
MI_SHEET    <- "https://dsl.richmond.edu/panorama/redlining/static/citiesData/%s/areaDescriptions/%s.json"
FETCHED     <- "2026-08-10"

CITIES <- c("Cleveland", "Philadelphia")
STATE  <- c(Cleveland = "OH", Philadelphia = "PA")
UTM    <- c(Cleveland = 26917, Philadelphia = 26918)   # UTM 17N / 18N, metres
DRAWN  <- "Cleveland"                                   # the city that gets mapped

dir.create("raw/holc", showWarnings = FALSE)
sz <- function(f) format(structure(file.size(f), class = "object_size"), units = "auto")
n_ <- function(x) format(x, big.mark = ",")

# A miss on this host is a 200 carrying the single-page-app shell. Anything that
# is not a FeatureCollection is treated as a miss.
grab <- function(url, dest) {
  if (!file.exists(dest)) prov_fetch(url, dest, quiet = TRUE)
  dest
}
# st_make_valid and ms_simplify can both hand back a GEOMETRYCOLLECTION; every
# step that produces geometry is funnelled through this so the type stays
# MULTIPOLYGON and st_coordinates keeps working.
polys <- function(x) st_cast(st_collection_extract(st_make_valid(x), "POLYGON"),
                            "MULTIPOLYGON")
is_fc <- function(path) {
  if (!file.exists(path) || file.size(path) < 5000) return(FALSE)
  ok <- tryCatch({
    j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    identical(j$type, "FeatureCollection") && length(j$features) > 0
  }, error = function(e) FALSE)
  ok
}

# ---- 1. tracts.csv -----------------------------------------------------------
tr <- read.csv("derived/tracts.csv", colClasses = c(GEOID = "character"))
stopifnot(all(nchar(tr$GEOID) == 11))
cat(sprintf("tracts.csv: %s rows, %d cities, %d states\n",
            n_(nrow(tr)), length(unique(tr$city)),
            length(unique(substr(tr$GEOID, 1, 2)))))
cat(sprintf("  GEOIDs beginning with a zero: %s of %s\n",
            n_(sum(substr(tr$GEOID, 1, 1) == "0")), n_(nrow(tr))))

# ---- 2. the HOLC maps --------------------------------------------------------
nat <- grab(MI_NATIONAL, "raw/holc/mappinginequality.json")
stopifnot(is_fc(nat))
cat(sprintf("\nMapping Inequality national file: %s, fetched %s\n", sz(nat), FETCHED))
H <- st_read(nat, quiet = TRUE)
H$grade <- trimws(ifelse(is.na(H$grade), "", as.character(H$grade)))
H$city  <- as.character(H$city); H$state <- as.character(H$state)
cat(sprintf("  %s features, %d city surveys\n",
            n_(nrow(H)), length(unique(paste(H$city, H$state)))))
print(table(ifelse(H$grade == "", "(none)", H$grade)))

ci <- read.csv("derived/cities.csv")
have <- paste(ci$city, "") %in% paste(H$city, "")   # city names match cities.csv exactly
cat(sprintf("  of the %d cities in cities.csv, %d appear in the HOLC file\n",
            nrow(ci), sum(ci$city %in% H$city)))

HG <- H[H$grade %in% c("A", "B", "C", "D"), ]
cat(sprintf("  graded A-D: %s of %s areas\n", n_(nrow(HG)), n_(nrow(H))))

# HOLC's own colour per grade, taken from the file rather than chosen here
FILL <- tapply(as.character(HG$fill), HG$grade, function(z) names(sort(table(z), TRUE))[1])
cat("  HOLC fill colours from the source:",
    paste(names(FILL), unname(FILL), collapse = "  "), "\n")

# ---- 3. national join check --------------------------------------------------
states <- sort(unique(substr(tr$GEOID, 1, 2)))
dir.create("raw/tiger", showWarnings = FALSE)
geoids <- character(0)
for (s in states) {
  z <- file.path("raw/tiger", sprintf("tl_2020_%s_tract.zip", s))
  if (!file.exists(z))
    prov_fetch(sprintf(
      "https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_%s_tract.zip", s),
      z, quiet = TRUE)
  d <- file.path("raw/tiger", s)
  if (!dir.exists(d)) unzip(z, exdir = d)
  shp <- list.files(d, "\\.shp$", full.names = TRUE)[1]
  lay <- tools::file_path_sans_ext(basename(shp))
  g <- st_read(shp, query = sprintf("SELECT GEOID FROM \"%s\"", lay), quiet = TRUE)
  geoids <- c(geoids, g$GEOID)
}
tr$matched <- tr$GEOID %in% geoids
cat(sprintf("\nJOIN: %s of %s tracts (%.2f%%) matched a TIGER 2020 tract polygon\n",
            n_(sum(tr$matched)), n_(nrow(tr)), 100 * mean(tr$matched)))
worse <- as.character(as.numeric(tr$GEOID))
cat(sprintf("  read as a number instead, %s of %s GEOIDs lose a digit and match nothing\n",
            n_(sum(nchar(worse) < 11)), n_(nrow(tr))))
stopifnot(identical(sprintf("%011.0f", as.numeric(tr$GEOID)), tr$GEOID))

# ---- 4. per city: HOLC areas, tracts, and the overlap between them -----------
tract_of <- function(state_fips, geoids_wanted = NULL, bbox = NULL) {
  shp <- list.files(file.path("raw/tiger", state_fips), "\\.shp$", full.names = TRUE)[1]
  lay <- tools::file_path_sans_ext(basename(shp))
  q <- sprintf("SELECT GEOID, INTPTLAT, INTPTLON, ALAND FROM \"%s\"", lay)
  if (!is.null(geoids_wanted))
    q <- sprintf("%s WHERE GEOID IN (%s)", q,
                 paste0("'", geoids_wanted, "'", collapse = ","))
  st_read(shp, query = q, quiet = TRUE)
}

MIX <- list(); HOLC <- list(); META <- list(); ZOOM <- list()

for (CITY in CITIES) {
  epsg <- unname(UTM[CITY])
  hc <- HG[HG$city == CITY & HG$state == STATE[CITY], ]
  hc <- polys(st_transform(st_make_valid(hc), epsg))
  hc$area_km2 <- as.numeric(st_area(hc)) / 1e6
  cat(sprintf("\n== %s == %d graded HOLC areas, %.1f km2\n",
              CITY, nrow(hc), sum(hc$area_km2)))

  # every 2020 tract in the state that touches a graded area: the tracts the
  # rule could have assigned, including the ones it drops
  stf <- substr(tr$GEOID[tr$city == CITY][1], 1, 2)
  hu  <- st_union(hc)
  all_t <- tract_of(stf)
  # TIGER ships water-only tracts (ALAND == 0, e.g. Lake Erie). They contain no
  # population, are in no crosswalk, and would draw as empty shapes.
  all_t <- all_t[all_t$ALAND > 0, ]
  all_t <- st_transform(st_make_valid(all_t), epsg)
  touch <- lengths(st_intersects(all_t, hu)) > 0
  tt <- all_t[touch, ]
  cat(sprintf("   %s tracts overlap a graded area; %s of them are in tracts.csv\n",
              n_(nrow(tt)), n_(sum(tt$GEOID %in% tr$GEOID))))

  # the rule: the tract's internal point, tested against the graded polygons
  pt <- st_as_sf(data.frame(GEOID = tt$GEOID,
                            lon = as.numeric(tt$INTPTLON),
                            lat = as.numeric(tt$INTPTLAT)),
                 coords = c("lon", "lat"), crs = 4326)
  pt <- st_transform(pt, epsg)
  hit <- st_within(pt, hc)
  tt$centre_grade <- vapply(hit, function(i)
    if (length(i)) hc$grade[i[1]] else "", character(1))

  # how much of each tract's land actually carries each grade, at full resolution
  hd <- aggregate(hc["grade"], by = list(grade = hc$grade), FUN = function(x) x[1])
  hd <- st_make_valid(hd)
  inter <- suppressWarnings(st_intersection(tt["GEOID"], hd["grade"]))
  inter$a <- as.numeric(st_area(inter))
  wide <- tapply(inter$a, list(inter$GEOID, inter$grade), sum)
  wide[is.na(wide)] <- 0
  M <- data.frame(GEOID = rownames(wide), wide[, c("A", "B", "C", "D"), drop = FALSE])
  names(M) <- c("GEOID", "aA", "aB", "aC", "aD")
  tt$area_m2 <- as.numeric(st_area(tt))
  M <- merge(M, data.frame(GEOID = tt$GEOID, area_m2 = tt$area_m2,
                           centre_grade = tt$centre_grade), by = "GEOID")
  gr_area <- M$aA + M$aB + M$aC + M$aD
  for (g in c("A", "B", "C", "D"))
    M[[paste0("p", g)]] <- round(100 * M[[paste0("a", g)]] / M$area_m2, 2)
  M$pU <- round(100 * pmax(0, M$area_m2 - gr_area) / M$area_m2, 2)
  P <- as.matrix(M[, c("pA", "pB", "pC", "pD")])
  M$modal_grade <- c("A", "B", "C", "D")[max.col(P, ties.method = "first")]
  M$modal_grade[rowSums(P) == 0] <- ""
  # shares OF THE GRADED LAND: what fraction of the part of this tract that was
  # graded at all carries the modal grade, and what fraction carries the grade
  # the rule assigned to the whole tract
  M$dom_share  <- round(apply(P, 1, max) / pmax(1e-9, rowSums(P)) * 100, 2)
  own <- match(M$centre_grade, c("A", "B", "C", "D"))
  M$own_share  <- round(ifelse(is.na(own), NA,
                    P[cbind(seq_len(nrow(P)), pmax(1, own))] /
                    pmax(1e-9, rowSums(P))) * 100, 2)
  M$graded     <- round(100 * gr_area / M$area_m2, 2)
  M$city <- CITY
  M$in_csv <- M$GEOID %in% tr$GEOID

  # does the rule, re-run here, reproduce the grade tracts.csv ships?
  chk <- merge(M[, c("GEOID", "centre_grade")],
               tr[tr$city == CITY, c("GEOID", "grade")], by = "GEOID")
  cat(sprintf("   rule re-run here agrees with tracts.csv on %s of %s (%.1f%%)\n",
              n_(sum(chk$centre_grade == chk$grade)), n_(nrow(chk)),
              100 * mean(chk$centre_grade == chk$grade)))

  ass <- M[M$centre_grade != "", ]
  cat(sprintf("   assigned a grade: %s tracts; centre grade differs from the grade\n",
              n_(nrow(ass))))
  cat(sprintf("     covering most of the tract in %s of them (%.1f%%)\n",
              n_(sum(ass$centre_grade != ass$modal_grade)),
              100 * mean(ass$centre_grade != ass$modal_grade)))
  cat(sprintf("     the assigned grade covers under half the tract's graded land in %s (%.1f%%)\n",
              n_(sum(ass$own_share < 50)), 100 * mean(ass$own_share < 50)))
  drop <- M[M$centre_grade == "" & M$graded > 0, ]
  cat(sprintf("   dropped though their land overlaps a graded area: %s tracts",
              n_(nrow(drop))))
  cat(sprintf(" (up to %.0f%% of one tract's land)\n", max(c(0, drop$graded))))

  MIX[[CITY]]  <- M
  HOLC[[CITY]] <- hc
  ag <- data.frame(city = CITY, grade = names(tapply(hc$area_km2, hc$grade, sum)),
                   n = as.vector(table(hc$grade)),
                   area_km2 = round(as.vector(tapply(hc$area_km2, hc$grade, sum)), 2))
  ag$pct_area <- round(100 * ag$area_km2 / sum(ag$area_km2), 2)
  # how far out each grade sits, so the brief's sentence about where the D areas
  # are is a measurement rather than an impression of the picture
  mid <- st_coordinates(st_centroid(st_union(hc)))
  ctr <- st_coordinates(st_point_on_surface(hc))
  km  <- sqrt((ctr[, 1] - mid[1])^2 + (ctr[, 2] - mid[2])^2) / 1000
  ag$km_out <- round(as.vector(tapply(km, hc$grade, mean))[match(ag$grade,
                     sort(unique(hc$grade)))], 2)
  a2 <- aggregate(cbind(total, black) ~ grade, tr[tr$city == CITY, ], sum)
  ag <- merge(ag, a2, by = "grade", all.x = TRUE)
  ag$pct_black <- round(100 * ag$black / ag$total, 2)
  META[[CITY]] <- ag
  print(ag, row.names = FALSE)

  if (CITY == DRAWN) ZOOM$tt <- tt
}

# ---- 5. the window for the close-up ------------------------------------------
# Picked by a stated rule, not by eye. Among tracts that are at least 70%
# covered by graded areas (so the example is about mixed grades, not about
# sitting on the edge of the survey) and whose centre point lands in a grade
# that is NOT the grade covering most of their land, and that involve a D area
# -- D being the grade every result in this brief turns on -- take the one where
# the disagreement is widest. A window is then centred on it.
M <- MIX[[DRAWN]]
cand <- M[M$centre_grade != "" & M$centre_grade != M$modal_grade & M$graded > 70 &
          (M$centre_grade == "D" | M$modal_grade == "D"), ]
cand <- cand[order(cand$own_share - cand$dom_share), ]
focus <- cand$GEOID[1]
cat(sprintf("\nwindow centred on tract %s: centre point in %s, most of its land %s\n",
            focus, cand$centre_grade[1], cand$modal_grade[1]))
cat(sprintf("  its land is %s\n", paste(sprintf("%.0f%% %s",
      unlist(cand[1, c("pA","pB","pC","pD","pU")]), c("A","B","C","D","ungraded")),
      collapse = ", ")))

epsg <- unname(UTM[DRAWN])
tt <- ZOOM$tt
fc <- st_centroid(st_geometry(tt[tt$GEOID == focus, ]))
xy <- st_coordinates(fc)
WIN_KM <- 5.6
win <- st_sfc(st_polygon(list(cbind(
  xy[1] + c(-1, 1, 1, -1, -1) * WIN_KM * 500,
  xy[2] + c(-1, -1, 1, 1, -1) * WIN_KM * 500))), crs = epsg)

# ---- 6. write coordinates ----------------------------------------------------
# Local kilometres from one shared origin per city, so the HOLC layer and the
# tract layer are in the same frame and can be drawn over each other.
rings <- function(g, id, OX, OY, dp = 3) {
  cc <- st_coordinates(g)
  grp <- interaction(cc[, "L1"], if ("L2" %in% colnames(cc)) cc[, "L2"] else 1,
                     if ("L3" %in% colnames(cc)) cc[, "L3"] else 1, drop = TRUE)
  do.call(rbind, lapply(seq_along(levels(grp)), function(i) {
    z <- cc[grp == levels(grp)[i], , drop = FALSE]
    data.frame(id = id, part = i,
               x = round((z[, "X"] - OX) / 1000, dp),
               y = round((z[, "Y"] - OY) / 1000, dp))
  }))
}
ringset <- function(s, OX, OY, dp = 3)
  do.call(rbind, lapply(seq_len(nrow(s)),
    function(i) rings(st_geometry(s)[i], i, OX, OY, dp)))

hc <- HOLC[[DRAWN]]
hc <- hc[order(hc$grade, hc$label), ]
bb <- st_bbox(hc); OX <- unname(bb["xmin"]); OY <- unname(bb["ymin"])

# The HOLC outlines are NOT simplified. The source polygons were digitised
# from hand-drawn 1930s sheets and are already sparse -- the whole city is a few
# thousand vertices -- so there is nothing to gain by thinning them and a
# straight reason not to: the second figure is about where two boundaries fail
# to meet, and a simplified boundary would manufacture some of that.
hs <- hc
cat(sprintf("\nHOLC outlines drawn at source resolution: %s coordinates\n",
            n_(nrow(st_coordinates(hs)))))

HR <- ringset(hs, OX, OY, 2)
HR$city <- DRAWN
lp <- suppressWarnings(st_coordinates(st_point_on_surface(hs)))
HA <- data.frame(city = DRAWN, id = seq_len(nrow(hs)), label = hs$label,
                 grade = hs$grade, fill = as.character(hs$fill),
                 area_km2 = round(hs$area_km2, 3),
                 lx = round((lp[, "X"] - OX) / 1000, 2),
                 ly = round((lp[, "Y"] - OY) / 1000, 2))

# The close-up draws both layers exactly as they are: clipped to the window,
# not thinned. The figure exists to show where a 1937 boundary and a 2020
# boundary fail to meet, so any thinning would be inventing part of the answer.
hz <- aggregate(hc["grade"], by = list(grade = hc$grade), FUN = function(x) x[1])
hz <- polys(suppressWarnings(st_intersection(st_make_valid(hz), win)))
HZ <- do.call(rbind, lapply(seq_len(nrow(hz)), function(i) {
  r <- rings(st_geometry(hz)[i], i, OX, OY, 3); r$grade <- hz$grade[i]; r }))
HZ$city <- DRAWN

# tracts, clipped to the window: only what the close-up draws is written
tw <- tt[lengths(st_intersects(tt, win)) > 0, ]
tws <- polys(suppressWarnings(st_intersection(st_make_valid(tw), win)))
tws <- tws[order(tws$GEOID), ]
TR <- ringset(tws, OX, OY, 3)
TR$city <- DRAWN
mm <- M[match(tws$GEOID, M$GEOID), ]
pp <- st_coordinates(st_transform(
  st_as_sf(data.frame(lon = as.numeric(tws$INTPTLON), lat = as.numeric(tws$INTPTLAT)),
           coords = c("lon", "lat"), crs = 4326), epsg))
TA <- data.frame(city = DRAWN, id = seq_len(nrow(tws)), GEOID = tws$GEOID,
                 centre_grade = mm$centre_grade, modal_grade = mm$modal_grade,
                 dom_share = mm$dom_share, own_share = mm$own_share,
                 graded = mm$graded,
                 pA = mm$pA, pB = mm$pB, pC = mm$pC, pD = mm$pD, pU = mm$pU,
                 focus = as.integer(tws$GEOID == focus),
                 cx = round((pp[, "X"] - OX) / 1000, 2),
                 cy = round((pp[, "Y"] - OY) / 1000, 2))
# the window itself, in the same frame, so both documents clip identically
wb <- st_bbox(win)
WIN <- data.frame(city = DRAWN,
                  x0 = round((wb["xmin"] - OX) / 1000, 2),
                  x1 = round((wb["xmax"] - OX) / 1000, 2),
                  y0 = round((wb["ymin"] - OY) / 1000, 2),
                  y1 = round((wb["ymax"] - OY) / 1000, 2))

# ---- 7. the appraiser's own sheet -------------------------------------------
# One D area of the drawn city, quoted verbatim in the brief as the document it
# is. The key is resolved by probing candidate years and validating the reply.
KEYS <- c(Cleveland = "OHCleveland1939", Philadelphia = "PAPhiladelphia1937")
key <- NA
for (y in 1935:1941) {
  k <- sprintf("%s%s%d", STATE[DRAWN], gsub("[^A-Za-z]", "", DRAWN), y)
  f <- file.path("raw/holc", paste0(k, ".json"))
  ok <- tryCatch({ grab(sprintf(MI_CITY, k), f); is_fc(f) }, error = function(e) FALSE)
  cat(sprintf("  probe %s: %s\n", k, if (isTRUE(ok)) sprintf("%s, %d features",
      sz(f), length(fromJSON(f, simplifyVector = FALSE)$features)) else "not a survey"))
  if (isTRUE(ok)) { key <- k; break } else unlink(f)
}
stopifnot(!is.na(key), identical(key, unname(KEYS[DRAWN])))

QUOTE <- NULL
sheet_label <- "D9"
sf_path <- file.path("raw/holc", sprintf("%s_%s.json", key, sheet_label))
ok <- tryCatch({
  if (!file.exists(sf_path))
    prov_fetch(sprintf(MI_SHEET, key, sheet_label), sf_path, quiet = TRUE)
  TRUE }, error = function(e) FALSE)
if (ok && file.size(sf_path) > 200) {
  s <- fromJSON(sf_path, simplifyVector = FALSE)
  txt   <- s[["8"]]                       # clarifying remarks
  infil <- s[["1"]][["e"]]                # "infiltration of"
  natx  <- s[["1"]][["c"]][["2"]]         # "foreign born"/nationalities
  neg   <- s[["1"]][["d"]]                # "negro" percentage
  hood  <- s[["9"]][["1"]]; dt <- s[["9"]][["4"]]
  QUOTE <- data.frame(city = DRAWN, key = key, label = sheet_label, grade = "D",
                      neighborhood = hood, dated = dt,
                      infiltration = infil, negro = neg, nationalities = natx,
                      remarks = txt)
  cat(sprintf("\nappraisal sheet %s %s (%s, dated %s): %d characters of remarks\n",
              key, sheet_label, hood, dt, nchar(txt)))
}

# ---- 8. out ------------------------------------------------------------------
# EVERY tract that overlaps a graded area, not only the ones the rule kept, so
# the brief can count what the rule dropped as well as what it assigned.
MX <- do.call(rbind, MIX)
MX$in_csv <- as.integer(MX$in_csv)
MX <- MX[, c("city", "GEOID", "in_csv", "centre_grade", "modal_grade",
             "dom_share", "own_share", "graded",
             "pA", "pB", "pC", "pD", "pU")]
MX <- MX[order(MX$city, MX$centre_grade, -MX$own_share), ]
# what the source actually returned, so the brief's Sources section quotes
# measurements of the fetch rather than numbers somebody typed
SRC <- data.frame(source = "Mapping Inequality, DSL, University of Richmond",
                  url = MI_NATIONAL, fetched = FETCHED,
                  bytes = file.size(nat), features = nrow(H),
                  graded = nrow(HG), city_surveys = length(unique(paste(H$city, H$state))),
                  sheet_key = if (!is.null(QUOTE)) QUOTE$key else NA,
                  sheet_label = if (!is.null(QUOTE)) QUOTE$label else NA)
write.csv(SRC, "derived/fig_source.csv", row.names = FALSE)
write.csv(HZ[, c("city", "grade", "id", "part", "x", "y")], "derived/fig_zoom_holc.csv",
          row.names = FALSE)
write.csv(HR, "derived/fig_holc_rings.csv", row.names = FALSE)
write.csv(HA, "derived/fig_holc_attr.csv", row.names = FALSE)
write.csv(do.call(rbind, META), "derived/fig_holc_meta.csv", row.names = FALSE)
write.csv(TR, "derived/fig_zoom_tracts.csv", row.names = FALSE)
write.csv(TA, "derived/fig_zoom_attr.csv", row.names = FALSE)
write.csv(WIN, "derived/fig_zoom_win.csv", row.names = FALSE)
write.csv(MX, "derived/fig_mix.csv", row.names = FALSE)
if (!is.null(QUOTE)) write.csv(QUOTE, "derived/fig_quote.csv", row.names = FALSE)
for (f in c("derived/fig_holc_rings.csv", "derived/fig_holc_attr.csv", "derived/fig_holc_meta.csv",
            "derived/fig_zoom_holc.csv", "derived/fig_zoom_tracts.csv", "derived/fig_zoom_attr.csv",
            "derived/fig_zoom_win.csv", "derived/fig_mix.csv", "derived/fig_quote.csv", "derived/fig_source.csv"))
  if (file.exists(f)) cat(sprintf("%-22s %6s rows  %s\n", f,
      n_(nrow(read.csv(f))), sz(f)))

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
