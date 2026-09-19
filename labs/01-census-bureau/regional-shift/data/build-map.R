# ---------------------------------------------------------------------------
# build-map.R — the seat-shift cartogram, exported as a file you can edit
#
# Writes derived/seatshift-1960-2020.json: a STATIC map. Every coordinate,
# every color and every mark is decided here, once, and the brief only draws
# what this file says. Nothing about the picture is computed at render time,
# so the way to change the picture is to edit the JSON, not to re-run a solver
# inside the document. That is the same arrangement as the published map at
#   https://jonathancervas.com/maps/apportionment-2020-2030/
# and this file is deliberately the same shape as that one's JSON, so a
# renderer written for either will read the other.
#
# TWO STEPS, TWO LANGUAGES
#
#   build-map-solve.js   runs the cartogram studio's solver headless: scales
#                        each state so its drawn area is proportional to its
#                        seats, drops it on the hand-drawn slot, relaxes until
#                        nothing overlaps, and carves it into equal-area cells.
#                        Leaves derived/.seatshift-solve.json.
#   this file            clips those cells to the state outline, decides which
#                        of them carry the seats that moved, and writes the
#                        map out. Deletes the intermediate.
#
# WHAT THE MAP SAYS. Each state is carved into max(seats 1960, seats 2020)
# equal-area cells, so every cell in the country is one House seat at the same
# size, and a state is drawn at whichever end of the window it was largest.
# |change| of those cells are marked: teal where the state gained, orange
# where it lost. New York is drawn at its 1960 size, 41 cells, and 15 of them
# are marked as gone.
#
# WHICH cell is marked is presentation, not a finding. A seat is apportioned
# to a state, not to a place inside it. The rule, the same one the published
# map uses: start from the cell furthest from the state's center, so the
# change reads at the margin rather than the middle; keep to the state's
# largest landmass; and for a multi-seat change grow the block outward by
# nearest neighbor, so it reads as one group rather than confetti.
#
# RUN FROM INSIDE data/:  Rscript build-map.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})

Y1 <- 1960
Y2 <- 2020
SOLVE <- "derived/.seatshift-solve.json"
OUT   <- "derived/seatshift-1960-2020.json"

# Teal gained, orange lost, gray held.
#
# NOT the published map's blue-and-vermilion, and not because that pair was
# bad. This map has three classes, which leaves a free choice of hue, and a
# red-and-blue pair at three classes is read as party by anyone who has seen
# an election map. Nothing here is partisan -- a seat moves because people
# moved -- so the diverging pair is teal against orange, which carries the
# same gain-and-loss reading and stays apart under every kind of color
# blindness.
#
# A four-way categorical palette is the other case, and not a violation of
# the same rule: four hues that stay apart have to span the circle, and red
# and blue earn their place in one. The four Census regions in this
# chapter's second figure keep them.
#
# Carried in the file so a renderer that has no stylesheet still draws the
# right map, and matched to the light values of the theme tokens the brief
# paints with, so screen and paper agree until dark mode moves them.
PALETTE <- list(retained = "#B9BEC4", gained = "#00857C", lost = "#A4590F")

# ---- 1. the solver's output ----------------------------------------------
if (!file.exists(SOLVE)) {
  msg <- system2("node", "build-map-solve.js", stdout = TRUE, stderr = TRUE)
  cat(paste(msg, collapse = "\n"), "\n")
  if (!file.exists(SOLVE))
    stop("build-map-solve.js did not write ", SOLVE, call. = FALSE)
}
sol <- fromJSON(SOLVE, simplifyVector = FALSE)
message("solver: ", length(sol$states), " states, ",
        sum(vapply(sol$states, function(s) s$cells, 0)), " cells")

# ---- 2. geometry helpers --------------------------------------------------
# Everything below works in the state's OWN unscaled frame, exactly as the
# published JSON does. The per-state {scale, tx, ty} is what moves it onto the
# 1152 x 748.8 page, and keeping the two apart is what makes the file
# editable: nudge a state by changing two numbers, and its cells, its outline
# and its label all follow.

ring_mat <- function(ring) do.call(rbind, lapply(ring, function(p) as.numeric(unlist(p))))

close_ring <- function(m) if (all(m[1, ] == m[nrow(m), ])) m else rbind(m, m[1, ])

as_sfc <- function(g) {
  parts <- if (identical(g$type, "Polygon")) list(g$coordinates) else g$coordinates
  st_sfc(st_multipolygon(lapply(parts, function(p)
    lapply(p, function(r) close_ring(ring_mat(r))))))
}

# 0.1 unit precision, the same the _lib/geo files are written at. Rounding in
# the shared frame -- never per-ring -- is what keeps two cells' common edge on
# the same vertices, so no seam opens between them.
r1 <- function(v) round(v, 1)

# One sfc polygon or multipolygon back out as GeoJSON-shaped lists.
sfc_to_json <- function(x) {
  g <- st_geometry(x)[[1]]
  parts <- switch(class(g)[2],
    POLYGON      = list(unclass(g)),
    MULTIPOLYGON = unclass(g),
    stop("unexpected geometry: ", class(g)[2]))
  # Rounding is what keeps two cells' common edge on identical vertices, but it
  # also collapses anything smaller than the grid. A ring that rounds down to
  # fewer than three distinct corners is not a shape any more, so it is dropped
  # here rather than written out as a geometry nothing can read back.
  parts <- lapply(parts, function(p) {
    rings <- lapply(p, function(r) {
      m <- r1(r)
      m <- m[c(TRUE, rowSums(abs(diff(m))) > 0), , drop = FALSE]
      if (nrow(m) > 1 && any(m[1, ] != m[nrow(m), ])) m <- rbind(m, m[1, ])
      m
    })
    Filter(function(m) nrow(m) >= 4, rings)
  })
  parts <- Filter(length, parts)
  if (!length(parts)) stop("a geometry rounded away entirely", call. = FALSE)
  parts <- lapply(parts, function(p) lapply(p, function(m)
    lapply(seq_len(nrow(m)), function(i) unname(m[i, ]))))
  if (length(parts) == 1L)
    list(type = "Polygon", coordinates = parts[[1]])
  else
    list(type = "MultiPolygon", coordinates = parts)
}

# ---- 3. clip the cells, mark the ones that moved --------------------------
# The solver returns power cells: convex, and cut to the state's bounding box
# rather than to the state. Intersecting them with the outline is what turns
# them into pieces of the state, and is also the check that the carving is
# sound -- the clipped pieces have to add back up to the whole state.

states <- lapply(sol$states, function(s) {
  outline <- st_make_valid(as_sfc(s$outline))

  clipped <- lapply(s$cellPolys, function(ring) {
    cell <- st_make_valid(st_sfc(st_polygon(list(close_ring(ring_mat(ring))))))
    g <- st_make_valid(st_intersection(outline, cell))
    # a clip that grazes a border can come back with a stray line or point
    # attached; keep the area and drop the rest
    if (!all(st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON")))
      g <- st_collection_extract(g, "POLYGON")
    if (length(g) == 0 || sum(as.numeric(st_area(g))) == 0)
      stop(s$st, ": a cell clipped away to nothing", call. = FALSE)
    st_union(g)
  })

  # the carving has to tile the state: no gaps, no double counting
  covered <- sum(vapply(clipped, function(g) sum(as.numeric(st_area(g))), 0))
  whole   <- sum(as.numeric(st_area(outline)))
  if (abs(covered - whole) / whole > 0.001)
    stop(s$st, ": cells cover ", round(100 * covered / whole, 2),
         "% of the state, not 100%", call. = FALSE)

  # cell interior points, and the largest landmass, for the marking rule
  ctr <- do.call(rbind, lapply(clipped, function(g)
    st_coordinates(st_point_on_surface(g))[1, 1:2]))

  parts <- st_cast(outline, "POLYGON")
  main  <- parts[which.max(as.numeric(st_area(parts)))]
  pts   <- st_sfc(lapply(seq_len(nrow(ctr)), function(i) st_point(ctr[i, ])))
  on_main <- as.logical(st_intersects(pts, main, sparse = FALSE))

  # draw the block from the edge inward: furthest cell first, then nearest
  # neighbor, so |change| cells read as one group at the state's margin
  k <- abs(s$change)
  marked <- integer(0)
  if (k > 0) {
    pool <- which(on_main)
    if (length(pool) < k) pool <- seq_len(nrow(ctr))    # islands only: use all
    home <- as.numeric(unlist(s$centroid))
    d0 <- sqrt(rowSums((ctr[pool, , drop = FALSE] -
                        rep(home, each = length(pool)))^2))
    marked <- pool[which.max(d0)]
    while (length(marked) < k) {
      rest <- setdiff(pool, marked)
      if (!length(rest)) rest <- setdiff(seq_len(nrow(ctr)), marked)
      dn <- vapply(rest, function(i)
        min(sqrt(rowSums((ctr[marked, , drop = FALSE] -
                          rep(ctr[i, ], each = length(marked)))^2))), 0)
      marked <- c(marked, rest[which.min(dn)])
    }
  }

  # number the cells top to bottom, left to right, so a reader editing the
  # file can find the one they are looking at
  ord <- order(round(ctr[, 2] / 8), ctr[, 1])
  status <- rep("retained", nrow(ctr))
  status[marked] <- if (s$change > 0) "gained" else "lost"
  lab <- ifelse(status == "gained", "+1", ifelse(status == "lost", "\u22121", NA))

  sc <- s$cartogram
  bb <- st_bbox(outline)
  list(
    st = s$st, fips = s$fips, name = s$name,
    seatsFrom = s$seatsFrom, seatsTo = s$seatsTo, change = s$change,
    cells = s$cells,
    bbox = r1(as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")])),
    centroid = r1(as.numeric(unlist(s$centroid))),
    layout = list(
      x = r1(bb[["xmin"]] * sc$scale + sc$tx),
      y = r1(bb[["ymin"]] * sc$scale + sc$ty),
      # the hand-drawn label anchors are authored in the layout's own units;
      # the same conversion the solver applies to the slots puts them on the page
      labelX = r1(as.numeric(unlist(s$label))[1] / sol$design$w * sol$frame$width),
      labelY = r1(as.numeric(unlist(s$label))[2] / sol$design$h * sol$frame$height)),
    cartogram = list(scale = round(sc$scale, 5),
                     tx = round(sc$tx, 2), ty = round(sc$ty, 2)),
    outline = sfc_to_json(outline),
    cellList = lapply(seq_along(ord), function(j) {
      i <- ord[j]
      list(k = j, status = status[i],
           label = if (is.na(lab[i])) NULL else lab[i],
           color = PALETTE[[status[i]]],
           centroid = r1(as.numeric(ctr[i, ])),
           cell = sfc_to_json(clipped[[i]]))
    }))
})
names(states) <- NULL
message("clipped ", sum(vapply(states, function(s) s$cells, 0)), " cells")

# ---- 4. where each state's label goes -------------------------------------
# The hand-drawn anchors are deliberately tight: most of them touch their own
# state somewhere. Touching is fine; STRADDLING is the defect -- a label with
# half its letters on the state's fill and half on the page is unreadable.
# Drawn raw, WA, OR, CA, WV, VA, NC, SC, AL and GA all land in that in-between.
#
# So every label is made one thing or the other. A state big enough to hold
# the box gets its label inside, on the part of it that did not change hands,
# so the text never sits on a colored mark and never needs a second color.
# Everything else is pushed straight out from the state until it is wholly
# clear of every state, and then labels that still overlap each other are
# separated along the same line. The result is stored, not recomputed: the
# brief draws labelX and labelY as it finds them.
#
# Sizes are in frame units, so they scale with the map: LBL_SIZE is the type
# size the placement assumes, and the brief passes the matching pixel size.

LBL_SIZE <- 14                       # frame units; 9.2 px on a 760-wide figure
LBL_W    <- 1.55 * LBL_SIZE          # a two-letter postal code, with tracking
LBL_H    <- 0.80 * LBL_SIZE
LBL_PAD  <- 0.30 * LBL_SIZE          # the margin "wholly clear" has to hold

box_at <- function(x, y, w = LBL_W, h = LBL_H)
  st_sfc(st_polygon(list(rbind(c(x - w/2, y - h/2), c(x + w/2, y - h/2),
                               c(x + w/2, y + h/2), c(x - w/2, y + h/2),
                               c(x - w/2, y - h/2)))))

place_on_page <- function(x, sc)      # a state's own frame -> the page
  (x * sc$scale) + matrix(c(sc$tx, sc$ty), nrow = 1)

shift_geom <- function(g, sc) st_make_valid(g * sc$scale + c(sc$tx, sc$ty))

# Fifty separate outlines, NOT their union: "is anything under this label" is
# an any() over fifty cheap tests, and dissolving fifty freshly transformed
# polygons into one is the step GEOS refuses on a degenerate vertex.
all_states <- do.call(c, lapply(states, function(s)
  shift_geom(as_sfc(s$outline), s$cartogram)))

# the part of each state that kept its seats: where a label can sit on gray
page_held <- lapply(states, function(s) {
  held <- s$cellList[vapply(s$cellList, function(c) c$status == "retained", TRUE)]
  if (!length(held)) return(NULL)
  g <- st_union(st_buffer(st_make_valid(
         do.call(c, lapply(held, function(c) as_sfc(c$cell)))), 0))
  shift_geom(g, s$cartogram)
})

anchor <- t(vapply(states, function(s)
  c(s$layout$labelX, s$layout$labelY), numeric(2)))
center <- t(vapply(seq_along(states), function(i)
  place_on_page(matrix(states[[i]]$centroid, nrow = 1),
                states[[i]]$cartogram)[1, ], numeric(2)))

# the direction a label leans, from its own state's middle towards its anchor
lean <- anchor - center
len  <- sqrt(rowSums(lean^2))
lean[len < 1e-6, ] <- c(0, -1)
len[len < 1e-6] <- 1
lean <- lean / len

pos   <- anchor
place <- rep("outside", length(states))

for (i in seq_along(states)) {
  # 1. inside, if the untouched part of the state can hold the box clear of
  #    its own edge -- tested at the widest point of that part
  held <- page_held[[i]]
  if (!is.null(held)) {
    parts <- st_cast(held, "POLYGON")
    parts <- parts[order(-as.numeric(st_area(parts)))]
    for (j in seq_along(parts)) {
      p <- st_point_on_surface(parts[j])
      xy <- st_coordinates(p)[1, 1:2]
      if (as.logical(st_covered_by(box_at(xy[1], xy[2]), parts[j], sparse = FALSE))) {
        pos[i, ] <- xy
        place[i] <- "inside"
        break
      }
    }
  }
  if (place[i] == "inside") next

  # 2. otherwise push it straight out until nothing is under it
  clear <- function(xy)
    !any(st_intersects(st_buffer(box_at(xy[1], xy[2]), LBL_PAD),
                       all_states, sparse = FALSE))
  if (clear(anchor[i, ])) next
  moved <- FALSE
  for (d in seq(2, 90, by = 2)) {
    cand <- anchor[i, ] + d * lean[i, ]
    if (clear(cand)) { pos[i, ] <- cand; moved <- TRUE; break }
  }
  if (!moved)
    message("label ", states[[i]]$st, ": no clear position within 90 units; ",
            "left on its anchor")
}

# 3. labels that now sit on each other: separate them along the same line.
#    Only the outside ones can move -- an inside label is anchored to the one
#    place its state has room for.
free <- which(place == "outside")
for (pass in 1:24) {
  hit <- FALSE
  for (a in seq_along(free)) for (b in seq_along(free)) {
    if (b <= a) next
    i <- free[a]; j <- free[b]
    dx <- pos[i, 1] - pos[j, 1]; dy <- pos[i, 2] - pos[j, 2]
    ox <- LBL_W + LBL_PAD - abs(dx); oy <- LBL_H + LBL_PAD - abs(dy)
    if (ox <= 0 || oy <= 0) next
    hit <- TRUE
    push <- if (ox / LBL_W < oy / LBL_H) c(sign(dx) * ox / 2, 0)
            else                          c(0, sign(dy) * oy / 2)
    if (all(push == 0)) push <- lean[i, ] * 2
    pos[i, ] <- pos[i, ] + push
    pos[j, ] <- pos[j, ] - push
  }
  if (!hit) break
}

for (i in seq_along(states)) {
  states[[i]]$layout$anchorX <- states[[i]]$layout$labelX
  states[[i]]$layout$anchorY <- states[[i]]$layout$labelY
  states[[i]]$layout$labelX  <- r1(pos[i, 1])
  states[[i]]$layout$labelY  <- r1(pos[i, 2])
  states[[i]]$layout$labelPlace <- place[i]
}
message("labels: ", sum(place == "inside"), " inside, ",
        sum(place == "outside"), " outside")

# ---- 5. the totals the file has to agree with -----------------------------
chg <- vapply(states, function(s) s$change, 0)
names(chg) <- vapply(states, function(s) s$st, "")
gross <- sum(chg[chg > 0])
stopifnot(sum(vapply(states, function(s) s$seatsFrom, 0)) == 435,
          sum(vapply(states, function(s) s$seatsTo, 0)) == 435,
          sum(chg) == 0)

count <- function(w) sum(vapply(states, function(s)
  sum(vapply(s$cellList, function(c) c$status == w, TRUE)), 0))
stopifnot(count("gained") == gross, count("lost") == gross)

chg <- chg[chg != 0]
chg <- chg[order(chg, names(chg))]

out <- list(
  meta = list(
    title = sprintf("Where the House moved, %d to %d", Y1, Y2),
    subtitle = sprintf("%d seats changed states; %d of the %d states kept the delegation they had",
                       gross, sum(vapply(states, function(s) s$change == 0, TRUE)),
                       length(states)),
    # No build timestamp. A wall clock in the file would make every rebuild a
    # different file, and the corpus already records when each output was
    # written, by which script, at what hash: data/BUILD-STAMP.tsv. Leaving it
    # out is what lets a rebuild from unchanged inputs come back byte for byte
    # identical, so the stamp stays true until the map really changes.
    from = Y1, to = Y2, projected = FALSE,
    width = sol$frame$width, height = sol$frame$height,
    coordinateSpace = paste("Pixels in a", sol$frame$width, "x", sol$frame$height,
                            "frame, y increasing downward. Ready for SVG."),
    method = paste("Each state is carved into max(seatsFrom, seatsTo) equal-area cells,",
                   "so every cell in the country is the same size and a state is drawn",
                   "at its peak seat count. |change| cells are marked as the seats that moved."),
    cellChoice = paste("A seat is apportioned to a state, not to a place within it, so which",
                       "cell carries the mark is presentational, not a claim about where a seat",
                       "was won or lost. Rule: start from the cell furthest from the state",
                       "centroid, restrict to the state's largest landmass, and grow a",
                       "multi-seat block by nearest neighbor so it reads as one group."),
    render = list(
      cartogram = "draw cell.cell and outline with the state's cartogram transform: x*scale+tx, y*scale+ty",
      color = "fill by cell.color, or by cell.status against your own palette; label gained and lost cells with cell.label"),
    editing = paste("This file is the map. Move a state by changing its cartogram.tx and",
                    "cartogram.ty; resize it with cartogram.scale; move a label with",
                    "layout.labelX and layout.labelY; move a mark by swapping the status",
                    "and label of two cells in the same state. Nothing is recomputed at",
                    "render time. Rebuild from scratch with Rscript build-map.R."),
    solver = c(sol$solve, list(
      note = paste("Solved headless by the cartogram studio's own solver",
                   "(jcervas.github.io/maps/cartogram-studio/), sources in",
                   "_lib/geo/raw/. areaDivisor 3.2 rather than the studio default 2.9:",
                   "510 cells need more room than 435, and 3.2 is the setting at which",
                   "the hand-drawn slot layout still converges."))),
    sources = list(
      seats = "derived/seatchange.csv, from the Census Bureau apportionment time series",
      geometry = "_lib/geo/raw/studio.json, Census cartographic boundary files via the createMaps pipeline")),
  palette = PALETTE,
  totals = list(
    seatsMoved = gross,
    statesChanged = length(chg),
    cells = sum(vapply(states, function(s) s$cells, 0)),
    retained = count("retained"), gained = count("gained"), lost = count("lost")),
  changes = as.list(chg),
  states = states)

write_json(out, OUT, auto_unbox = TRUE, digits = NA, null = "null", pretty = FALSE)
unlink(SOLVE)

cat(sprintf("\nwrote %s: %d states, %d cells, %d seats moved between %d states (%.0f KB)\n",
            OUT, length(states), out$totals$cells, gross, length(chg),
            file.size(OUT) / 1024))
print(chg)
