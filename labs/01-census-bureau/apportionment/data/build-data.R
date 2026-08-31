# ---------------------------------------------------------------------------
# Build the apportionment dataset.
#
# Two files end up in this folder:
#
#   derived/apportionment_2020.csv   every state's 2020 apportionment population, the
#                            seats it received, the change since 2010, and the
#                            overseas population folded into its count
#   derived/state_rings.csv          the fifty state outlines, projected and reduced to
#                            integer canvas coordinates, for the map figure
#
# Run this script from inside the data/ folder. It needs a network connection;
# the committed output means the lab does not.
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

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# U.S. Census Bureau, 2020 Census Apportionment Results
#   https://www.census.gov/data/tables/2020/dec/2020-apportionment-data.html
#     Table 1 — apportionment population and resulting seats
#     Table 3 — overseas population, by home state
#
# APPORTIONMENT POPULATION is not the same as resident population. It is the
# resident population PLUS federal employees serving overseas (military and
# civilian) and their dependents, assigned back to a home state. Table 3 is
# that adjustment, and it is worth a look: it is small, it is not evenly
# spread, and in 2020 it was larger than the margin that decided the last
# seat in the House.

base <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/data/",
               "apportionment/apportionment-2020-table")

dir.create("raw", showWarnings = FALSE)

grab <- function(n) {
  f <- paste0("raw/apportionment-2020-table", n, ".xlsx")
  prov_fetch(paste0(base, n, ".xlsx"), f, mode = "wb", quiet = TRUE)
  f
}

# The workbooks carry three title rows and a footnoted TOTAL row. Rather than
# hardcode row offsets that a future release would silently break, keep every
# row whose first cell is text and whose second is a number, then drop TOTAL.
read_table <- function(path, cols) {
  z <- utils::unzip(path, exdir = tempdir())
  sheet  <- grep("sheet1.xml$", z, value = TRUE)
  shared <- grep("sharedStrings.xml$", z, value = TRUE)

  ss <- character(0)
  if (length(shared)) {
    x  <- paste(readLines(shared, warn = FALSE), collapse = "")
    si <- regmatches(x, gregexpr("<si>.*?</si>", x))[[1]]
    ss <- vapply(si, function(s) {
      t <- regmatches(s, gregexpr("<t[^>]*>[^<]*</t>", s))[[1]]
      paste0(gsub("<[^>]+>", "", t), collapse = "")
    }, character(1))
  }

  x  <- paste(readLines(sheet, warn = FALSE), collapse = "")
  rr <- regmatches(x, gregexpr("<row[^>]*>.*?</row>", x))[[1]]

  out <- lapply(rr, function(row) {
    cc <- regmatches(row, gregexpr("<c[^>]*>(<v>[^<]*</v>)?</c>", row))[[1]]
    vapply(cc, function(cell) {
      v <- sub(".*<v>([^<]*)</v>.*", "\\1", cell)
      if (!grepl("<v>", cell)) return(NA_character_)
      if (grepl('t="s"', cell)) ss[as.integer(v) + 1L] else v
    }, character(1))
  })

  keep <- Filter(function(r) length(r) >= 2 &&
                   !is.na(r[1]) && is.na(suppressWarnings(as.numeric(r[1]))) &&
                   !is.na(suppressWarnings(as.numeric(r[2]))), out)
  d <- do.call(rbind, lapply(keep, function(r) {
    data.frame(state = trimws(r[1]),
               v2 = suppressWarnings(as.numeric(r[2])),
               v3 = if (length(r) >= 3) suppressWarnings(as.numeric(r[3])) else NA)
  }))
  d <- d[!grepl("^TOTAL", toupper(d$state)), ]
  names(d) <- cols
  d
}

t1 <- read_table(grab("01"), c("state", "app_pop", "seats"))
t3 <- read_table(grab("03"), c("state", "overseas", "drop"))
t3$drop <- NULL

ap <- merge(t1, t3, by = "state", all.x = TRUE)
ap$seats <- as.integer(ap$seats)

cat("states:", nrow(ap), " seats:", sum(ap$seats), "\n")
stopifnot(nrow(ap) == 50, sum(ap$seats) == 435)

ap$resident_pop <- ap$app_pop - ap$overseas
ap$people_per_seat <- round(ap$app_pop / ap$seats)
ap <- ap[order(-ap$app_pop), ]

write.csv(ap, "derived/apportionment_2020.csv", row.names = FALSE)

# --- The Bureau's own priority values ---------------------------------------
#
#   derived/priority_values.csv   the published Huntington-Hill queue: which
#                                 state took each House seat from 51 to 445,
#                                 which of that state's own seats it was, and
#                                 the priority value it was bought at
#
# U.S. Census Bureau, Priority Values for 2020 Census Apportionment
#   https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/
#     2020PriorityValues.xlsx
#
# This is the arithmetic itself, published. It runs ten seats PAST the 435 the
# statute allows, so it also names who would take seats 436 through 445 -- the
# only place the counterfactual comes from the Bureau rather than from a
# recomputation here.
#
# Same shape problem as the other two workbooks: three title rows, then a real
# header, so the same "first cell text, second cell number" test finds the data.

pv_path <- "raw/2020PriorityValues.xlsx"
prov_fetch(paste0("https://www2.census.gov/programs-surveys/decennial/2020/",
                  "data/apportionment/2020PriorityValues.xlsx"),
           pv_path, mode = "wb", quiet = TRUE)

pv_raw <- read_table(pv_path, c("state", "house_seat", "state_seat"))

# read_table() only carries three columns across and the fourth is the one that
# matters, so the priority column is pulled from the same sheet separately.
pv_cells <- local({
  z  <- utils::unzip(pv_path, exdir = file.path(tempdir(), "pv"))
  sh <- grep("sharedStrings.xml$", z, value = TRUE)
  ss <- character(0)
  if (length(sh)) {
    x  <- paste(readLines(sh, warn = FALSE), collapse = "")
    si <- regmatches(x, gregexpr("<si>.*?</si>", x))[[1]]
    ss <- vapply(si, function(q) {
      tt <- regmatches(q, gregexpr("<t[^>]*>[^<]*</t>", q))[[1]]
      paste0(gsub("<[^>]+>", "", tt), collapse = "")
    }, character(1))
  }
  x  <- paste(readLines(grep("sheet1.xml$", z, value = TRUE), warn = FALSE),
              collapse = "")
  rr <- regmatches(x, gregexpr("<row[^>]*>.*?</row>", x))[[1]]
  out <- lapply(rr, function(row) {
    cc <- regmatches(row, gregexpr("<c[^>]*>(<v>[^<]*</v>)?</c>", row))[[1]]
    vapply(cc, function(cell) {
      v <- sub(".*<v>([^<]*)</v>.*", "\\1", cell)
      if (!grepl("<v>", cell)) return(NA_character_)
      if (grepl('t="s"', cell)) ss[as.integer(v) + 1L] else v
    }, character(1), USE.NAMES = FALSE)
  })
  Filter(function(r) length(r) >= 4 && !is.na(r[1]) &&
           is.na(suppressWarnings(as.numeric(r[1]))) &&
           !is.na(suppressWarnings(as.numeric(r[2]))), out)
})

pv <- data.frame(
  state      = trimws(vapply(pv_cells, function(r) r[1], character(1))),
  house_seat = as.integer(vapply(pv_cells, function(r) r[2], character(1))),
  state_seat = as.integer(vapply(pv_cells, function(r) r[3], character(1))),
  priority   = as.numeric(vapply(pv_cells, function(r) r[4], character(1))),
  stringsAsFactors = FALSE)
pv <- pv[order(pv$house_seat), ]

# The published queue must start at seat 51 -- the fifty guaranteed seats are
# not in it, because they are not bought -- and every state named must be one
# of the fifty in Table 1.
stopifnot(nrow(pv) > 380, min(pv$house_seat) == 51L,
          identical(pv$house_seat, seq(51L, max(pv$house_seat))),
          all(pv$state %in% ap$state), all(pv$priority > 0),
          all(diff(pv$priority) < 0))

# And it must be the same queue the formula produces from the population
# column. priority = population / sqrt(k * (k - 1)) for a state's kth seat.
own <- pv$priority - ap$app_pop[match(pv$state, ap$state)] /
  sqrt(pv$state_seat * (pv$state_seat - 1))
cat("published priority values reproduced from the population column:",
    max(abs(own)) < 1e-6, "\n")
stopifnot(max(abs(own)) < 1e-6)

write.csv(pv, "derived/priority_values.csv", row.names = FALSE)
cat("priority_values.csv ->", nrow(pv), "rows, seats",
    min(pv$house_seat), "to", max(pv$house_seat), "\n")

# --- Check the file against the method that produced it ---------------------
#
# If Huntington-Hill run over this population column does not reproduce the
# seat column, something is wrong with the file and the lab is built on sand.

hh <- function(pop, total = 435) {
  s <- setNames(rep(1L, length(pop)), names(pop))
  for (i in seq_len(total - length(pop))) {
    pv <- pop / sqrt(s * (s + 1))
    w  <- names(which.max(pv))
    s[w] <- s[w] + 1L
  }
  s
}
p <- setNames(ap$app_pop, ap$state)
check <- hh(p)
cat("Huntington-Hill reproduces the official seats:",
    identical(as.integer(check[ap$state]), ap$seats), "\n")

# The last seat, and the state that missed it.
s <- setNames(rep(1L, nrow(ap)), ap$state)
for (i in 1:384) { pv <- p/sqrt(s*(s+1)); w <- names(which.max(pv)); s[w] <- s[w]+1L }
pv <- p/sqrt(s*(s+1)); last <- names(which.max(pv)); last_pv <- max(pv)
s[last] <- s[last] + 1L
pv <- p/sqrt(s*(s+1)); nextup <- names(which.max(pv))
need <- last_pv * sqrt(s[nextup] * (s[nextup] + 1))

cat("seat 435 ->", last, "\n")
cat("next in line ->", nextup, "— short by",
    ceiling(need - p[nextup]), "people\n")

# --- State outlines, for the map -------------------------------------------
#
# U.S. Census Bureau, 2020 Cartographic Boundary Files, states, 1:20,000,000
#   https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_20m.zip
#
# The map has to carry all fifty states, because apportionment is the one
# subject where Alaska and Hawaii are not optional: every state is guaranteed a
# seat, and the guarantee is most of what a one-seat state has. So they are
# moved into insets rather than dropped. That is a lie about where they are,
# and it is the standard lie; the alternative is a map that omits two of the
# fifty units it claims to be about.
#
# Geometry is written out as integer canvas coordinates rather than lon/lat.
# The figure does not project anything at draw time, and the browser never
# loads a geographic library.

suppressPackageStartupMessages({
  library(sf)
  library(rmapshaper)
})

geo_url <- paste0("https://www2.census.gov/geo/tiger/GENZ2020/shp/",
                  "cb_2020_us_state_20m.zip")
zf <- tempfile(fileext = ".zip")
prov_fetch(geo_url, zf, mode = "wb", quiet = TRUE)
gd <- tempfile(); dir.create(gd); unzip(zf, exdir = gd)
sts <- st_read(gd, quiet = TRUE)

# The fifty states and nothing else: DC and the territories have no seat, and
# this chapter's table has fifty rows.
sts <- sts[sts$NAME %in% ap$state, ]
stopifnot(nrow(sts) == 50)

sts <- st_transform(sts, 5070)          # CONUS Albers equal-area
sts <- ms_simplify(sts, keep = 0.15, keep_shapes = TRUE, method = "vis")

# Scale a state about its own centroid and drop it at a chosen point.
inset <- function(obj, nm, scale, target) {
  i <- which(obj$NAME == nm)
  g <- st_geometry(obj)[i]
  ctr <- st_centroid(st_union(g))
  st_geometry(obj)[i] <- st_sfc((g - ctr) * scale + st_sfc(st_point(target)),
                                crs = st_crs(obj))
  obj
}
sts <- inset(sts, "Alaska", 0.37, c(-1980000,  500000))
sts <- inset(sts, "Hawaii", 1.30, c(-1255000,  355000))

# Quantise to an integer canvas, y flipped so that north is up on a screen
# whose origin is the top-left corner.
bb     <- st_bbox(sts)
CANVAS <- 960
sc     <- CANVAS / (bb["xmax"] - bb["xmin"])
qx <- function(v) as.integer(round((v - bb["xmin"]) * sc))
qy <- function(v) as.integer(round((bb["ymax"] - v) * sc))

rings <- do.call(rbind, lapply(seq_len(nrow(sts)), function(i) {
  cs <- st_coordinates(st_geometry(sts)[i])
  L  <- apply(cs[, setdiff(colnames(cs), c("X", "Y")), drop = FALSE], 1,
              paste, collapse = "_")
  data.frame(state = sts$NAME[i], part = paste(i, L, sep = "_"),
             x = qx(cs[, "X"]), y = qy(cs[, "Y"]))
}))

# Two vertices that land on the same pixel are the same point on the page.
dup <- c(FALSE, rings$x[-1] == rings$x[-nrow(rings)] &
                rings$y[-1] == rings$y[-nrow(rings)] &
                rings$part[-1] == rings$part[-nrow(rings)])
rings <- rings[!dup, ]

# Drop slivers too small to see, but never drop a state: each keeps its
# largest ring however small that ring is.
tb    <- table(rings$part)
keepp <- names(which(tb >= 6))
bigp  <- vapply(split(rings$part, rings$state),
                function(p) names(which.max(table(p))), character(1))
rings <- rings[rings$part %in% union(keepp, bigp), ]
rings$part <- as.integer(factor(rings$part))
stopifnot(length(unique(rings$state)) == 50)

write.csv(rings, "derived/state_rings.csv", row.names = FALSE)
cat("state_rings.csv ->", nrow(rings), "points,",
    length(unique(rings$part)), "rings,",
    length(unique(rings$state)), "states\n")

cat("\ndone.\n")

# --- Seat change, 2010 to 2020 ----------------------------------------------
#
# derived/seat_change_rings.csv    every state carved into as many equal-area
#                                  cells as it held at its peak across the two
#                                  counts, each cell marked retained / gained /
#                                  lost, plus the state outlines
# derived/seat_change_labels.csv   the state abbreviations and the +1/-1 marks
#
# Source: the published data behind
#   https://jonathancervas.com/maps/apportionment-2010-2020/
# One cell is one seat and every cell is the same size nationally, so a state
# that lost a seat simply has one cell more than it now needs. Which cell
# carries the mark is presentational: a seat is apportioned to a state, not to
# a place inside it.
#
# The states are drawn APART. Each carries its own affine transform in the
# source -- x' = tx + x * scale -- which scales a state by its seat count and
# moves it clear of its neighbours. That transform is applied here, so the
# coordinates in derived/ are final and the figure projects nothing.
#
# State labels start from the source's precomputed labelX/labelY and are then
# resolved by the placement rule set out below -- the same rule the static PNGs
# of this map use, so the deck and the brief agree about where a label goes.
# anchor says which side of the point the text hangs on: left, right or centre.
sc_src <- "raw/apportionment-2010-2020.json"
if (file.exists(sc_src) && requireNamespace("jsonlite", quietly = TRUE)) {
  sj  <- jsonlite::fromJSON(sc_src, simplifyVector = FALSE)
  scx <- 960 / sj$meta$width

  rings_of <- function(g)
    if (identical(g$type, "MultiPolygon"))
      unlist(g$coordinates, recursive = FALSE) else g$coordinates

  st_v <- character(0); part_v <- integer(0); status_v <- character(0)
  x_v <- numeric(0); y_v <- numeric(0); part <- 0L
  bb <- list()

  for (s in sj$states) {
    k <- s$cartogram$scale; tx <- s$cartogram$tx; ty <- s$cartogram$ty
    put <- function(r, status) {
      x <- (tx + vapply(r, function(p) p[[1]], 0) * k) * scx
      y <- (ty + vapply(r, function(p) p[[2]], 0) * k) * scx
      keep <- c(TRUE, round(x[-1],1) != round(x[-length(x)],1) |
                      round(y[-1],1) != round(y[-length(y)],1))
      x <- round(x[keep], 1); y <- round(y[keep], 1)
      part <<- part + 1L
      st_v <<- c(st_v, rep(s$st, length(x)));  part_v <<- c(part_v, rep(part, length(x)))
      status_v <<- c(status_v, rep(status, length(x)))
      x_v <<- c(x_v, x); y_v <<- c(y_v, y)
    }
    for (cl in s$cellList) for (r in rings_of(cl$cell)) put(r, cl$status)
    for (r in rings_of(s$outline)) put(r, "outline")
    o <- st_v == s$st
    bb[[s$st]] <- c(min(x_v[o]), max(x_v[o]), min(y_v[o]), max(y_v[o]))
  }

  rings <- data.frame(st = st_v, part = part_v, status = status_v,
                      x = x_v, y = y_v, stringsAsFactors = FALSE)
  write.csv(rings, "derived/seat_change_rings.csv", row.names = FALSE)

  # One row per state: the two seat counts the cartogram is built from. The
  # rings say which cells moved; this says how many seats each state held
  # before and after, which is what a reader hovering a shape wants to read.
  # name is here so the abbreviation can be joined to apportionment_2020.csv,
  # which is keyed on the full name.
  scst <- do.call(rbind, lapply(sj$states, function(s)
    data.frame(st = s$st, name = s$name, seats_2010 = s$seatsFrom,
               seats_2020 = s$seatsTo, change = s$change,
               stringsAsFactors = FALSE)))
  scst <- scst[order(scst$st), ]
  write.csv(scst, "derived/seat_change_states.csv", row.names = FALSE)

  # --- labels ---------------------------------------------------------------
  #
  # Same placement rule the static PNGs of this map use, so the deck and the
  # brief agree. It starts from the source's own layout$labelX / labelY -- those
  # are already in final space and are not put through the cartogram transform
  # -- and then enforces one property, applied to all fifty alike:
  #
  #   a label must be either wholly inside its own state, or wholly clear of
  #   every state with a margin, and must not overlap another label; if it is
  #   neither, move it the shortest distance that makes it one or the other,
  #   preferring the side it already leans to and the direction away from its
  #   own centroid, and never so far that another state becomes the nearest.
  #
  # The anchors as shipped are deliberately tight -- most of them touch their
  # own state somewhere, which is fine at either extreme and unreadable in
  # between: a label straddling an outline has half its letters on grey and half
  # on white. That straddle is the defect, not the contact, so the rule sorts
  # labels into "inside" and "outside" rather than pushing every one clear.
  #
  # Two things differ from the SVG version of the same rule:
  #
  #   * Text metrics come from R, not from a font table. cex is a size relative
  #     to the device rather than to the map, so a label's size *in map units*
  #     depends on the figure's physical size; the FIG_* constants below mirror
  #     the {r seat-change-map} chunk of apportionment-brief.Rmd and have to
  #     track it. Every distance in the rule is then a multiple of the measured
  #     cap height, so nothing here is a number copied out of the 1152-wide
  #     source frame into this 960-wide one.
  #   * The brief renders on two devices -- pdf for LaTeX, png for HTML -- and
  #     they disagree about width by up to 8% on the kerned pairs (PA, VA, WA).
  #     Both are measured and the wider is used.
  FIG_W <- 7.2; FIG_H <- 5.6                  # fig.width / fig.height
  FIG_MAR  <- c(0.2, 0.2, 0.2, 0.2)           # par(mar=)
  FIG_XPAD <- c(-16, 16); FIG_YPAD <- c(-16, 34)   # xlim / ylim padding
  LAB_CEX <- 0.50; CELL_CEX <- 0.46; LAB_FONT <- 2
  BORDER_LWD <- 1.0                           # the dark state border

  codes <- vapply(sj$states, function(s) s$st, "")
  ns    <- length(codes)
  sidx  <- setNames(seq_len(ns), codes)
  cell_lab <- unlist(lapply(sj$states, function(s)
    vapply(Filter(function(cc) !is.null(cc$label), s$cellList),
           function(cc) cc$label, "")))

  # Measure text the way the figure will draw it: same device, same figure size,
  # same plot window, same cex and font. strwidth()/strheight() then return user
  # units -- map units -- directly.
  measure <- function(open) {
    open(); on.exit(dev.off())
    par(mar = FIG_MAR)
    plot(NA, xlim = range(rings$x) + FIG_XPAD,
         ylim = rev(range(rings$y) + FIG_YPAD),
         asp = 1, axes = FALSE, xlab = "", ylab = "")
    list(w  = vapply(codes, strwidth, 0, cex = LAB_CEX, font = LAB_FONT, units = "user"),
         h  = abs(strheight("W", cex = LAB_CEX, font = LAB_FONT, units = "user")),
         cw = vapply(sub("−", "-", cell_lab), strwidth, 0,
                     cex = CELL_CEX, font = LAB_FONT, units = "user"),
         ch = abs(strheight("1", cex = CELL_CEX, font = LAB_FONT, units = "user")),
         upi = diff(par("usr")[1:2]) / par("pin")[1])
  }
  m_pdf <- measure(function() pdf(NULL, width = FIG_W, height = FIG_H))
  m_png <- tryCatch(measure(function() png(tempfile(fileext = ".png"),
                    width = FIG_W, height = FIG_H, units = "in", res = 96)),
                    error = function(e) NULL)
  lw  <- if (is.null(m_png)) m_pdf$w  else pmax(m_pdf$w,  m_png$w)
  lh  <- if (is.null(m_png)) m_pdf$h  else max( m_pdf$h,  m_png$h)
  cw  <- if (is.null(m_png)) m_pdf$cw else pmax(m_pdf$cw, m_png$cw)
  chh <- if (is.null(m_png)) m_pdf$ch else max( m_pdf$ch, m_png$ch)
  upi <- m_pdf$upi                            # user units per inch
  BORDER_W <- BORDER_LWD * upi / 96           # the dark line, in user units

  # Every distance as a multiple of the cap height, so the rule travels between
  # frames. The ratios are the ones verified on the 1152-wide SVG.
  MARGIN    <- BORDER_W / 2 + 0.158 * lh   # daylight beyond the drawn border
  BLEED     <- 0.030 * lh                  # advance width vs real ink
  PAD_LABEL <- 0.169 * lh                  # daylight between two labels
  MAX_NUDGE <- 3.38  * lh
  STEP      <- 0.056 * lh

  # --- outline edges, in a bucket index ---------------------------------------
  # 34,000 edges; a label box only ever has to be tested against the handful in
  # the buckets it covers.
  outl <- rings[rings$status == "outline", ]
  np <- nrow(outl); pp <- outl$part
  seg    <- which(pp[-np] == pp[-1])                 # consecutive pairs in a part
  lastp  <- c(which(pp[-np] != pp[-1]), np)          # last row of each part
  firstp <- c(1L, lastp[-length(lastp)] + 1L)
  i1 <- c(seg, lastp); i2 <- c(seg + 1L, firstp)     # + each ring's closing edge
  ex1 <- outl$x[i1]; ey1 <- outl$y[i1]
  ex2 <- outl$x[i2]; ey2 <- outl$y[i2]
  est <- unname(sidx[outl$st[i1]])
  ke  <- !(ex1 == ex2 & ey1 == ey2)                  # drop zero-length edges
  ex1 <- ex1[ke]; ey1 <- ey1[ke]; ex2 <- ex2[ke]; ey2 <- ey2[ke]; est <- est[ke]
  NE  <- length(ex1)
  eminx <- pmin(ex1, ex2); emaxx <- pmax(ex1, ex2)
  eminy <- pmin(ey1, ey2); emaxy <- pmax(ey1, ey2)

  BS  <- 8
  gx0 <- min(eminx) - 40; gy0 <- min(eminy) - 40
  NBX <- as.integer((max(emaxx) + 40 - gx0) %/% BS) + 1L
  NBY <- as.integer((max(emaxy) + 40 - gy0) %/% BS) + 1L
  bxa <- as.integer((eminx - gx0) %/% BS); bxb <- as.integer((emaxx - gx0) %/% BS)
  bya <- as.integer((eminy - gy0) %/% BS); byb <- as.integer((emaxy - gy0) %/% BS)
  nbx <- bxb - bxa + 1L; nby <- byb - bya + 1L
  ei  <- rep.int(seq_len(NE), nbx * nby)
  jj  <- sequence(nbx * nby) - 1L
  bkey <- (bya[ei] + jj %/% nbx[ei]) * NBX + (bxa[ei] + jj %% nbx[ei]) + 1L
  BUCK <- unname(split(ei, factor(bkey, levels = seq_len(NBX * NBY))))
  eof_state <- split(seq_len(NE), est)

  BBX <- do.call(rbind, bb[codes])                   # x0 x1 y0 y1 per state
  cen <- t(vapply(sj$states, function(s)
    c((s$cartogram$tx + s$centroid[[1]] * s$cartogram$scale) * scx,
      (s$cartogram$ty + s$centroid[[2]] * s$cartogram$scale) * scx), c(0, 0)))

  # --- primitives -------------------------------------------------------------
  # rect_hits(): which states' outlines cross this rectangle. Separating-axis
  # test -- the rectangle's two axes, then the edge's own normal -- which is
  # exact for a segment against an axis-aligned box.
  rect_hits <- function(r) {
    a  <- max(0L, as.integer((r[1] - gx0) %/% BS))
    b  <- min(NBX - 1L, as.integer((r[3] - gx0) %/% BS))
    c0 <- max(0L, as.integer((r[2] - gy0) %/% BS))
    d  <- min(NBY - 1L, as.integer((r[4] - gy0) %/% BS))
    if (b < a || d < c0) return(integer(0))
    i <- unique(unlist(BUCK[as.vector(outer(seq.int(c0, d) * NBX,
                                            seq.int(a, b), "+")) + 1L],
                       use.names = FALSE))
    if (!length(i)) return(integer(0))
    i <- i[!(emaxx[i] < r[1] | eminx[i] > r[3] |
             emaxy[i] < r[2] | eminy[i] > r[4])]
    if (!length(i)) return(integer(0))
    dx <- ex2[i] - ex1[i]; dy <- ey2[i] - ey1[i]
    s1 <- dx * (r[2] - ey1[i]) - dy * (r[1] - ex1[i])
    s2 <- dx * (r[2] - ey1[i]) - dy * (r[3] - ex1[i])
    s3 <- dx * (r[4] - ey1[i]) - dy * (r[3] - ex1[i])
    s4 <- dx * (r[4] - ey1[i]) - dy * (r[1] - ex1[i])
    unique(est[i[!((s1 > 0 & s2 > 0 & s3 > 0 & s4 > 0) |
                   (s1 < 0 & s2 < 0 & s3 < 0 & s4 < 0))]])
  }
  pip <- function(px, py, si) {                      # even-odd ray cast
    i  <- eof_state[[as.character(si)]]
    cr <- (ey1[i] > py) != (ey2[i] > py)
    if (!any(cr)) return(FALSE)
    j <- i[cr]
    (sum(px < ex1[j] + (py - ey1[j]) * (ex2[j] - ex1[j]) / (ey2[j] - ey1[j]))
     %% 2L) == 1L
  }
  state_at <- function(px, py) {
    for (i in which(px >= BBX[, 1] & px <= BBX[, 2] &
                    py >= BBX[, 3] & py <= BBX[, 4]))
      if (pip(px, py, i)) return(i)
    0L
  }
  infl <- function(r, d) c(r[1] - d, r[2] - d, r[3] + d, r[4] + d)
  ovl  <- function(a, b) !(a[3] < b[1] || b[3] < a[1] || a[4] < b[2] || b[4] < a[2])

  # A box whose margin crosses no outline is wholly on one side of every state,
  # and its centre says which: inside this state, or out in the white.
  lab_mode <- function(r, si) {
    q <- infl(r, MARGIN + BLEED)
    if (length(rect_hits(q))) return(NA_character_)
    s <- state_at((q[1] + q[3]) / 2, (q[2] + q[4]) / 2)
    if (s == 0L) return("out")
    if (s == si) return("in")
    NA_character_
  }

  # --- the +1 / -1 marks, which are also obstacles ----------------------------
  cellsl <- do.call(rbind, lapply(sj$states, function(s) {
    k <- s$cartogram$scale; tx <- s$cartogram$tx; ty <- s$cartogram$ty
    m <- Filter(function(cc) !is.null(cc$label), s$cellList)
    if (!length(m)) return(NULL)
    do.call(rbind, lapply(m, function(cc) data.frame(kind = "cell", st = s$st,
      label = cc$label,
      x = round((tx + cc$centroid[[1]] * k) * scx, 1),
      y = round((ty + cc$centroid[[2]] * k) * scx, 1),
      anchor = "centre", stringsAsFactors = FALSE)))
  }))
  cell_rects <- lapply(seq_len(nrow(cellsl)), function(i)
    c(cellsl$x[i] - cw[i] / 2, cellsl$y[i] - chh / 2,
      cellsl$x[i] + cw[i] / 2, cellsl$y[i] + chh / 2))

  # --- natural anchors, and which side each label already leans to ------------
  natr <- lapply(seq_len(ns), function(k) {
    s <- sj$states[[k]]
    lx <- s$layout$labelX * scx; ly <- s$layout$labelY * scx
    c(lx, ly - lh / 2, lx + lw[[k]], ly + lh / 2)     # the cap box
  })
  lean <- vapply(seq_len(ns), function(k) {
    r <- natr[[k]]
    g <- expand.grid(x = seq(r[1], r[3], length.out = 9),
                     y = seq(r[2], r[4], length.out = 5))
    if (mean(mapply(function(a, b) state_at(a, b) != 0L, g$x, g$y)) >= 0.5)
      "in" else "out"
  }, "")

  thin <- lapply(seq_len(ns), function(k) {
    o <- which(rings$st == codes[k] & rings$status == "outline")
    o <- o[seq(1, length(o), length.out = min(length(o), 300))]
    cbind(rings$x[o], rings$y[o])
  })
  nearest <- function(r) {
    px <- (r[1] + r[3]) / 2; py <- (r[2] + r[4]) / 2
    which.min(vapply(thin, function(m)
      min((m[, 1] - px)^2 + (m[, 2] - py)^2), 0))
  }

  # --- search -----------------------------------------------------------------
  # Rings of growing radius, 24 directions each; inside a ring the direction
  # nearest "away from my own centroid" is tried first.
  ANG <- seq(0, 2 * pi, length.out = 25)[-25]
  RAD <- seq(STEP, MAX_NUDGE, by = STEP)
  place  <- natr
  moved  <- setNames(rep(0, ns), codes)
  relaxed <- character(0)

  for (pass in 1:8) {
    dirty <- FALSE
    for (k in seq_len(ns)) {
      others <- c(place[-k], cell_rects)
      okf <- function(r, pad) {
        m <- lab_mode(r, k)
        if (is.na(m)) return(NA_character_)
        if (any(vapply(others, ovl, TRUE, a = infl(r, pad + BLEED))))
          return(NA_character_)
        m
      }
      if (!is.na(okf(place[[k]], PAD_LABEL))) next
      dirty <- TRUE
      nat  <- natr[[k]]
      away <- atan2((nat[2] + nat[4]) / 2 - cen[k, 2],
                    (nat[1] + nat[3]) / 2 - cen[k, 1])
      ord  <- ANG[order(abs(atan2(sin(ANG - away), cos(ANG - away))))]
      own  <- nearest(nat) == k
      got  <- NULL
      # Two constraints of unequal weight: straddling an outline is the defect
      # this rule exists to remove, crowding a neighbouring label is a nuisance.
      # So the label-to-label gap is what gives way when a pocket is too tight.
      for (pad in c(PAD_LABEL, PAD_LABEL / 2, 0)) {
        for (want in unique(c(lean[k], "in", "out")))
          for (rr in RAD) {
            for (a in ord) {
              dx <- rr * cos(a); dy <- rr * sin(a)
              cand <- nat + c(dx, dy, dx, dy)
              if (!identical(okf(cand, pad), want)) next
              if (own && nearest(cand) != k) next
              got <- list(cand, sqrt(dx^2 + dy^2), pad); break
            }
            if (!is.null(got)) break
          }
        if (!is.null(got)) break
      }
      if (!is.null(got)) {
        place[[k]] <- got[[1]]; moved[k] <- got[[2]]
        if (got[[3]] < PAD_LABEL) relaxed <- union(relaxed, codes[k])
      }
    }
    if (!dirty) break
  }

  # --- resolve to a point and an anchor ---------------------------------------
  # x is an EDGE of the resolved box, not its centre, and the edge chosen is the
  # one further from the state: if a device sets the text a little wider than
  # measured, it then grows away from the shape rather than into it. y is always
  # the vertical middle of the cap box, so the figure draws with adj[2] = 0.5
  # and no further offset. A label placed inside its own state is centred.
  mode_f <- vapply(seq_len(ns), function(k) {
    m <- lab_mode(place[[k]], k); if (is.na(m)) "STRADDLE" else m }, "")
  lab <- do.call(rbind, lapply(seq_len(ns), function(k) {
    r <- place[[k]]; mid <- (r[1] + r[3]) / 2
    if (mode_f[k] == "in")        an <- "centre"
    else if (mid < cen[k, 1])     an <- "right"      # text hangs left of x
    else                          an <- "left"
    data.frame(kind = "state", st = codes[k], label = codes[k],
               x = round(switch(an, left = r[1], right = r[3], centre = mid), 1),
               y = round((r[2] + r[4]) / 2, 1),
               anchor = an, stringsAsFactors = FALSE)
  }))
  write.csv(rbind(lab, cellsl), "derived/seat_change_labels.csv", row.names = FALSE)

  message(sprintf(
    "seat-change labels: %d moved of %d (max %.1f, median %.1f); inside own state: %s; %sstraddling: %s",
    sum(moved > 0), ns, max(moved), median(moved[moved > 0]),
    paste(codes[mode_f == "in"], collapse = " "),
    if (length(relaxed)) paste0("label gap relaxed for ", paste(relaxed, collapse = " "), "; ") else "",
    if (any(mode_f == "STRADDLE")) paste(codes[mode_f == "STRADDLE"], collapse = " ") else "none"))
}

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
