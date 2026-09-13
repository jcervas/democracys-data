# build-state-maps.R -- the national map, built one state at a time.
#
# The overview and the zoomed-in views are the same geometry, so there is one
# dataset rather than two. What makes that possible is matching each state's
# simplification to how large THAT state is drawn when it fills the figure --
# about 700 pixels -- instead of applying one national tolerance. Rhode Island
# is kept fine and Texas is thinned hard, and the totals come out level: about
# seven thousand vertices a state either way.
#
# Everything lands in one shared frame, 40,000 units across the country, or
# roughly 116 m per unit. That is deliberately far finer than the overview
# needs: at 2,000 units Rhode Island would be thirty units wide and would fall
# apart the moment it was magnified.

suppressPackageStartupMessages(library(sf))

SRC    <- Sys.getenv("STATE_GEOJSON_DIR", "raw/state")
STATES <- Sys.getenv("STATES_GEOJSON", "raw/states_500k.geojson")
D      <- Sys.getenv("DERIVED", "derived")
W      <- 40000L
FILLPX <- 700                       # how wide a state is drawn when zoomed
dir.create(D, recursive = TRUE, showWarnings = FALSE)

ms <- function(...) {
  cmd <- paste("mapshaper-xl 8gb", paste(..., collapse = " "))
  if (system(cmd, ignore.stderr = TRUE) != 0L) stop("mapshaper failed: ", cmd)
}
poly <- function(f, keep = NULL) {
  g <- st_read(f, quiet = TRUE)
  if (is.null(keep)) g <- st_geometry(g) else g <- g[, keep, drop = FALSE]
  g <- g[!st_is_empty(st_geometry(g)), ]
  st_cast(st_collection_extract(g, "POLYGON"), "POLYGON")
}

# The frame comes from the state outlines, so the blocks and the borders
# cannot drift apart.
ts <- tempfile(fileext = ".json"); ts2 <- tempfile(fileext = ".json")
ms("-i", shQuote(STATES), "-proj albersusa -o", shQuote(ts),
   "format=geojson precision=1")
# Borders are borders. At this frame the outlines arrive with a quarter of a
# million vertices, far past what a 1px stroke can show even with one state
# filling the figure; 120 m is under a pixel for the smallest of them.
ms("-i", shQuote(ts), "-simplify interval=120 -o", shQuote(ts2),
   "format=geojson precision=1")
s  <- poly(ts2, "GEOID")
bb <- st_bbox(s)
sc <- W / (bb["xmax"] - bb["xmin"])
H  <- as.integer(round((bb["ymax"] - bb["ymin"]) * sc))

flatten <- function(g, ids = NULL, st = NULL) {
  if (inherits(g, "sf")) g <- st_geometry(g)   # st_coordinates on sf loses dimnames
  cc  <- st_coordinates(g)
  key <- paste(cc[, "L2"], cc[, "L1"])
  rows <- lapply(unique(key), function(k) {
    i <- which(key == k)
    x <- as.integer(round((cc[i, "X"] - bb["xmin"]) * sc))
    y <- as.integer(round((bb["ymax"] - cc[i, "Y"]) * sc))
    d <- c(TRUE, x[-1] != x[-length(x)] | y[-1] != y[-length(y)])
    x <- x[d]; y <- y[d]
    if (length(x) < 3) return(NULL)
    out <- data.frame(poly = cc[i[1], "L2"], ring = cc[i[1], "L1"], x = x, y = y)
    if (!is.null(ids)) out$st <- ids[cc[i[1], "L2"]]
    if (!is.null(st))  out$st <- st
    out
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

files <- sort(list.files(SRC, pattern = "^zero_pop_[0-9]{2}\\.geojson$",
                         full.names = TRUE))
stopifnot(length(files) > 0)
t1 <- tempfile(fileext = ".json"); t2 <- tempfile(fileext = ".json")
parts <- list(); boxes <- list()

for (f in files) {
  fips <- sub(".*zero_pop_([0-9]{2})\\.geojson$", "\\1", f)
  ms("-i", shQuote(f), "-dissolve -proj albersusa -o", shQuote(t1),
     "format=geojson precision=1")
  g  <- poly(t1)
  # Albers USA has no frame for Puerto Rico or the island areas, so they
  # project to nothing. They are absent from this map, and the brief says so.
  if (length(g) == 0) { cat(sprintf("  %s  not placed by albersusa, skipped\n", fips)); next }
  gb <- st_bbox(g)
  tol <- max(gb["xmax"] - gb["xmin"], gb["ymax"] - gb["ymin"]) / FILLPX
  # one token: ms() joins its arguments with spaces, so "interval=" and the
  # number have to be pasted, not passed separately
  ms("-i", shQuote(t1),
     paste0("-simplify interval=", format(round(tol), scientific = FALSE)),
     "-o", shQuote(t2), "format=geojson precision=1")
  g <- poly(t2)
  p <- flatten(g, st = fips)
  # polygon ids restart per state, so make them unique across the country
  p$poly <- paste0(fips, "_", p$poly)
  parts[[fips]] <- p
  boxes[[fips]] <- data.frame(
    st = fips,
    x0 = min(p$x), y0 = min(p$y), x1 = max(p$x), y1 = max(p$y))
  cat(sprintf("  %s  tol %6.0f m  %8s vertices\n", fips, tol,
              format(nrow(p), big.mark = ",")))
}

zf <- do.call(rbind, parts)
sf_ <- flatten(s, ids = s$GEOID)
write.csv(zf,  file.path(D, "map_zero.csv"),   row.names = FALSE)
write.csv(sf_, file.path(D, "map_states.csv"), row.names = FALSE)
write.csv(data.frame(w = W, h = H), file.path(D, "map_frame.csv"), row.names = FALSE)

# Per-state extents, for the zoom. Taken from the STATE OUTLINE rather than
# from the blocks: zooming to the blocks would crop a state whose empty land
# sits in one corner.
sb <- do.call(rbind, lapply(split(sf_, sf_$st), function(z)
  data.frame(st = z$st[1], x0 = min(z$x), y0 = min(z$y),
             x1 = max(z$x), y1 = max(z$y))))
write.csv(sb, file.path(D, "state_bbox.csv"), row.names = FALSE)

cat(sprintf("frame %dx%d | %s states | %s vertices | states layer %s vertices\n",
            W, H, length(parts), format(nrow(zf), big.mark = ","),
            format(nrow(sf_), big.mark = ",")))
