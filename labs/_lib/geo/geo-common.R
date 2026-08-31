# ---------------------------------------------------------------------------
# geo-common.R -- shared machinery for the geo/ build scripts.
#
# build-geo.R predates this file and is self-contained; the later builds
# (build-layers.R, build-states.R) source this instead of repeating it.
# Everything here serves one rule: every file in geo/ is PRE-PROJECTED into
# a fixed 1152 x 748.8 y-down plane, so a D3 geoIdentity() path and a base-R
# polygon() draw the identical map.
# ---------------------------------------------------------------------------

options(scipen = 999, stringsAsFactors = FALSE, timeout = 900)
suppressMessages({ library(sf); library(jsonlite) })
sf_use_s2(FALSE)

say <- function(...) cat(..., "\n", sep = "")

FRAME_W <- 1152
FRAME_H <- 748.8
MARGIN  <- 10
FRAME_FILE <- "us-frame.json"

# The provenance helper, if installed (it is, at labs/_lib/). If missing the
# builds still run; they just leave no PROVENANCE.tsv / BUILD-STAMP.tsv trail.
if (file.exists("../provenance.R")) {
  source("../provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    utils::download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
  .prov_record <- function(...) invisible(NULL)
  .prov_sha <- function(path) paste0("md5:", unname(tools::md5sum(path)))
}

# Record a LOCAL source in PROVENANCE.tsv -- a file promoted from a chapter's
# data/raw rather than downloaded. The url column carries the repo-relative
# path, so the same drift check (bytes/hash moved since first capture) works
# on sources that never crossed the network.
prov_local <- function(path, label = NULL) {
  if (!file.exists(path)) return(invisible(NULL))
  rel <- sub(".*?/labs/", "labs/", normalizePath(path))
  .prov_record(paste0("file:", rel), path, label)
  invisible(path)
}

# BUILD-STAMP.tsv, same eight columns as the chapter contract. provenance.R's
# prov_stamp() assumes a chapter layout (derived/ + raw/) and prunes rows for
# files outside them, which in geo/ -- where outputs sit at the top level --
# would erase every other build's rows. So the geo builds stamp through this
# merge instead: same format, no pruning beyond files that are actually gone.
stamp_geo <- function(files, script) {
  cols <- c("script", "stamped_on", "stamp_source", "file",
            "bytes", "sha256", "rows", "file_mtime")
  old <- if (file.exists("BUILD-STAMP.tsv")) {
    read.delim("BUILD-STAMP.tsv", stringsAsFactors = FALSE, colClasses = "character")
  } else {
    as.data.frame(setNames(rep(list(character()), length(cols)), cols))
  }
  files <- files[file.exists(files)]
  info  <- file.info(files)
  new <- data.frame(
    script = script, stamped_on = format(Sys.Date()), stamp_source = "build",
    file = files, bytes = as.character(info$size),
    sha256 = vapply(files, .prov_sha, character(1), USE.NAMES = FALSE),
    rows = NA_character_,
    file_mtime = format(info$mtime, "%Y-%m-%d"), stringsAsFactors = FALSE)
  old <- old[file.exists(old$file) & !old$file %in% new$file, , drop = FALSE]
  out <- rbind(old, new); out <- out[order(out$file), , drop = FALSE]
  write.table(out, "BUILD-STAMP.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
  say("  [stamp] ", script, ": ", nrow(new), " file(s) stamped")
  invisible(out)
}

# --- frame arithmetic -------------------------------------------------------

load_frame <- function() fromJSON(FRAME_FILE, simplifyVector = TRUE)

# A fit is the affine that puts projected km onto the frame:
#   frame_x = ox + (X - xmin) * s
#   frame_y = oy + (ymax - Y) * s        (y flips to screen convention)
fit_of <- function(bbox, margin = MARGIN) {
  s <- min((FRAME_W - 2 * margin) / (bbox["xmax"] - bbox["xmin"]),
           (FRAME_H - 2 * margin) / (bbox["ymax"] - bbox["ymin"]))
  list(xmin = unname(bbox["xmin"]), ymax = unname(bbox["ymax"]), s = unname(s),
       ox = unname((FRAME_W - s * (bbox["xmax"] - bbox["xmin"])) / 2),
       oy = unname((FRAME_H - s * (bbox["ymax"] - bbox["ymin"])) / 2))
}

frame_xy <- function(m, fit, digits = 1) {
  cbind(round(fit$ox + (m[, 1] - fit$xmin) * fit$s, digits),
        round(fit$oy + (fit$ymax - m[, 2]) * fit$s, digits))
}

# Rounding to 0.1 makes neighbouring vertices coincide; the duplicates draw
# nothing and cost bytes. Dedupe consecutive points, keep the ring closed,
# and report NULL for a ring that rounding collapsed below a triangle.
clean_ring <- function(m) {
  n <- nrow(m)
  if (n < 4) return(NULL)
  dup <- c(FALSE, m[-1, 1] == m[-n, 1] & m[-1, 2] == m[-n, 2])
  m <- m[!dup, , drop = FALSE]
  n <- nrow(m)
  if (n < 3) return(NULL)
  if (m[1, 1] != m[n, 1] || m[1, 2] != m[n, 2]) m <- rbind(m, m[1, ])
  if (nrow(m) < 4) return(NULL)
  m
}

# One sf row -> GeoJSON MultiPolygon coordinates on the frame. `xy` is the
# km-to-frame function for this row (it differs for the AK/HI insets). Rings
# that rounding collapsed are dropped -- but never all of them: the largest
# polygon always survives, so no unit vanishes from the map.
frame_coords <- function(geom, xy) {
  g <- geom[[1]]
  if (inherits(g, "POLYGON")) g <- list(g)
  polys <- lapply(g, function(poly) Filter(Negate(is.null), lapply(poly, function(r) clean_ring(xy(r)))))
  polys <- Filter(length, polys)
  if (!length(polys)) {  # everything collapsed: keep the biggest ring raw
    areas <- vapply(g, function(poly) nrow(poly[[1]]), numeric(1))
    r <- xy(g[[which.max(areas)]][[1]])
    if (nrow(r) < 4) r <- rbind(r, r[1, ], r[1, ], r[1, ])[1:4, ]
    polys <- list(list(r))
  }
  polys
}

# GeoJSON geometry member for a frame_coords() result: a plain Polygon where
# there is one polygon (most counties, most precincts) -- the "MultiPolygon"
# wrapper on 3,000 single-ring shapes is pure bytes.
geom_json <- function(polys) {
  if (length(polys) == 1) list(type = "Polygon", coordinates = polys[[1]])
  else list(type = "MultiPolygon", coordinates = polys)
}

# Write a FeatureCollection with the frame recorded in the file itself, the
# same shape build-geo.R writes.
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

# --- the frame registry -----------------------------------------------------
#
# us-frame.json began as a single frame record (build-geo.R still writes those
# keys) and consumers read it that way -- the wind-map and mapping chapters
# take FRAME$conus_proj, FRAME$fit_* and FRAME$conus_box off the top level.
# Those keys are therefore PRESERVED VERBATIM. The registry lives beside them
# in `frames`: one entry per shared file, keyed by filename.
registry_update <- function(entries) {
  reg <- if (file.exists(FRAME_FILE)) load_frame() else list()
  if (is.null(reg$frames)) reg$frames <- list()
  for (nm in names(entries)) reg$frames[[nm]] <- entries[[nm]]
  reg$frames <- reg$frames[order(names(reg$frames))]
  writeLines(toJSON(reg, auto_unbox = TRUE, digits = NA, null = "null"), FRAME_FILE)
  say("updated ", FRAME_FILE, " (", length(reg$frames), " registered frames)")
  invisible(reg)
}

# Frame-plane bounds of a written .geojson, for the registry. jsonlite
# simplifies each ring to a numeric matrix; anything else recurses.
bounds_of_file <- function(file) {
  fc <- fromJSON(file)
  xr <- c(Inf, -Inf); yr <- c(Inf, -Inf)
  hit <- function(r, v) c(min(r[1], v), max(r[2], v))
  walk <- function(cc) {
    if (is.numeric(cc)) {
      m <- if (is.matrix(cc)) cc else matrix(cc, ncol = 2)
      xr <<- hit(xr, m[, 1]); yr <<- hit(yr, m[, 2])
    } else for (c2 in cc) walk(c2)
  }
  walk(fc$features$geometry$coordinates)
  round(c(xr[1], yr[1], xr[2], yr[2]), 1)
}
