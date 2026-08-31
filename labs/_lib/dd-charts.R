# dd-charts.R -- the R side of the shared chart library (_lib/dd-charts.js).
#
# A chapter sources this from its setup chunk, next to structure.R:
#
#   source("../../_lib/dd-charts.R")
#
# and then a D3 chunk is a data frame and one call:
#
#   dd_fig("bars", "bar", moved,
#          x = list(field = "change", fmt = "signed0"),
#          y = list(field = "name", band = TRUE),
#          series = list(field = "region",
#                        classes = list(Northeast = "series-1", South = "series-2")),
#          catLabels = "inline", valueLabels = TRUE, legend = TRUE,
#          tip = dd_tip(c(region = "region", change = "seats"),
#                       fmt = c(change = "signed0"), title = "name"))
#
# Everything here is serialisation: the config the chapter writes as R lists
# is the config DD.fig() receives as JSON, one to one, so the API documented
# at the top of dd-charts.js is the whole story. The only things that cannot
# travel through JSON are functions, and dd_js() carries those as verbatim
# JavaScript for the tip, slider and hook escape hatches.
#
# Base R throughout, like the chapters it serves.

# one flag per render: source() runs fresh for each output format, so the
# guard resets exactly when the document does
.dd <- new.env(parent = emptyenv())
.dd$libs_emitted <- FALSE

# ---- where _lib is from here ------------------------------------------------
# Chapters sit at labs/NN-part/slug/, two levels under labs/, so the answer is
# almost always "../../_lib/". Computed rather than assumed: walk up from the
# working directory until a _lib/dd-charts.js appears, so a chapter that ever
# sits one level deeper (or a test render from labs/ itself) still resolves.
dd_lib_prefix <- function() {
  d <- getwd()
  ups <- ""
  for (i in 0:8) {
    if (file.exists(file.path(d, "_lib", "dd-charts.js")))
      return(paste0(ups, "_lib/"))
    nd <- dirname(d)
    if (identical(nd, d)) break
    d <- nd
    ups <- paste0(ups, "../")
  }
  "../../_lib/"                      # the chapter convention, as a fallback
}

#' Emit the two <script src> tags, once per render.
#'
#' Call it from the first D3 chunk (dd_fig() calls it for you). `d3 = FALSE`
#' is for a chapter whose FIRST figure is still hand-written and already
#' carries the d3 tag: a second copy would silently double the payload once
#' pandoc inlines it, which is the one thing the corpus is strict about.
dd_libs <- function(d3 = TRUE) {
  if (isTRUE(.dd$libs_emitted)) return(invisible(NULL))
  .dd$libs_emitted <- TRUE
  p <- dd_lib_prefix()
  if (isTRUE(d3))
    cat(sprintf('<script src="%sd3.v7.min.js"></script>\n', p))
  cat(sprintf('<script src="%sdd-charts.js"></script>\n', p))
  invisible(NULL)
}

# ---- verbatim JavaScript ----------------------------------------------------
#' Mark a string as JavaScript to pass through unquoted: a tip function, a
#' slider onchange, a hook. Everything else in a config is data.
dd_js <- function(code) {
  structure(paste(code, collapse = "\n"), class = "dd_js")
}

# Walk a config, pull every dd_js() out into a token, and drop the NULLs that
# an optional argument leaves behind (toJSON would turn a bare NULL into {}).
.dd_prepare <- function(x, store) {
  if (inherits(x, "dd_js")) {
    tok <- sprintf("@@DDJS%d@@", length(store$js) + 1L)
    store$js[[tok]] <- unclass(x)
    return(tok)
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- x[!vapply(x, is.null, logical(1))]
    return(lapply(x, .dd_prepare, store = store))
  }
  x
}

dd_json <- function(cfg) {
  store <- new.env(parent = emptyenv())
  store$js <- list()
  cfg <- .dd_prepare(cfg, store)
  out <- as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, digits = NA,
                                       dataframe = "rows", na = "null",
                                       null = "null"))
  for (tok in names(store$js))
    out <- sub(paste0('"', tok, '"'), store$js[[tok]], out, fixed = TRUE)
  out
}

# ---- the one call -----------------------------------------------------------
#' Emit a figure: the container div, and the DD.fig() call that draws into it.
#'
#' @param id      the div id, unique within the chapter (check-figures.py
#'                reads duplicate ids as a figure emitted twice)
#' @param type    "bar" "dot" "line" "area" "step" "scatter" "slope"
#'                "dumbbell" "choropleth" "smallmult"
#' @param data    a data frame, serialised row-wise; or NULL for a map that
#'                carries its data in `values`
#' @param ...     the rest of the config, as R lists: x, y, series, size,
#'                annotations, tip, slider, hook, and the per-type options
#'                documented in dd-charts.js
#' @param height  shorthand for size$h
#' @param libs    emit the script tags first if they have not been yet
#' @param d3      passed to dd_libs(): FALSE when an earlier hand-written
#'                figure already loaded d3
dd_fig <- function(id, type, data = NULL, ..., height = NULL,
                   libs = TRUE, d3 = TRUE) {
  cfg <- list(type = type, ...)
  if (!is.null(data)) cfg$data <- data
  if (!is.null(height)) {
    if (is.null(cfg$size)) cfg$size <- list()
    cfg$size$h <- height
  }
  if (isTRUE(libs)) dd_libs(d3 = d3)
  cat(sprintf('<div class="dd-fig" id="%s"></div>\n<script>DD.fig("#%s", %s);</script>\n',
              id, id, dd_json(cfg)))
  invisible(NULL)
}

# ---- tooltips ---------------------------------------------------------------
#' The declarative tooltip: named vector of field -> label, an optional named
#' vector of field -> format name (see DD.fmt in dd-charts.js), and the field
#' whose value heads the box in bold. For anything richer, pass
#' tip = dd_js("function(d){ return ... }") to dd_fig() instead.
dd_tip <- function(fields, fmt = NULL, title = NULL) {
  items <- lapply(names(fields), function(f) {
    fm <- if (!is.null(fmt) && f %in% names(fmt)) unname(fmt[[f]]) else "plain"
    list(f, unname(fields[[f]]), fm)
  })
  list(title = title, fields = items)
}

# ---- annotations ------------------------------------------------------------
# Constructors, so a chapter's annotation list reads as what it draws. Each is
# a plain list; dd_fig(annotations = list(dd_annot_vline(20), ...)).
dd_annot_vline <- function(x, class = NULL, dash = TRUE)
  list(type = "vline", x = x, class = class, dash = dash)

dd_annot_hline <- function(y, class = NULL, dash = TRUE)
  list(type = "hline", y = y, class = class, dash = dash)

dd_annot_band <- function(from, to, axis = "x", class = NULL)
  list(type = "band", from = from, to = to, axis = axis, class = class)

dd_annot_text <- function(x, y, text, class = NULL, anchor = "start",
                          size = 11, weight = NULL, dx = 0, dy = 0, px = FALSE)
  list(type = "text", x = x, y = y, text = text, class = class,
       anchor = anchor, size = size, weight = weight, dx = dx, dy = dy,
       px = px)

dd_annot_rule <- function(x1, y1, x2, y2, class = NULL)
  list(type = "rule", x1 = x1, y1 = y1, x2 = x2, y2 = y2, class = class)

# ---- maps -------------------------------------------------------------------
#' Read one of the _lib/geo files and return data.frame(id, d, ...): one SVG
#' path d-string per feature, on the file's own pre-projected 1152 x 748.8
#' y-down frame.
#'
#' This is the wind-map chapter's recipe factored out: round every vertex
#' (here to 0.1, the precision the geo files are written at), drop the ones
#' that land on their predecessor, and join the rings. Rounding in the shared
#' coordinate space -- never per-ring thinning -- is what keeps two states'
#' common border on the same vertices, so no background wedge ever shows
#' through a seam. Nothing is projected: the files are already flat.
#'
#' @param file      path to a _lib/geo *.geojson
#' @param id_field  the property to carry as the id ("st", "fips", "name")
#' @return data.frame(id, d, name, lx, ly); attr(, "frame") = c(width, height)
dd_geo_paths <- function(file, id_field = "st") {
  gj <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  num1 <- function(v) sub("\\.0$", "", sprintf("%.1f", v))   # 540.0 -> 540
  ring_d <- function(ring) {
    x <- round(vapply(ring, function(p) as.numeric(p[[1]]), 0), 1)
    y <- round(vapply(ring, function(p) as.numeric(p[[2]]), 0), 1)
    keep <- c(TRUE, diff(x) != 0 | diff(y) != 0)
    paste0("M", paste(num1(x[keep]), num1(y[keep]), sep = ",", collapse = "L"), "Z")
  }
  feat_d <- function(g) {
    rings <- switch(g$type,
      Polygon      = g$coordinates,
      MultiPolygon = unlist(g$coordinates, recursive = FALSE),
      stop("dd_geo_paths(): geometry type ", g$type,
           " -- the _lib/geo files hold polygons only"))
    paste(vapply(rings, ring_d, character(1)), collapse = "")
  }

  rows <- lapply(gj$features, function(f) {
    p <- f$properties
    if (is.null(p[[id_field]]))
      stop("dd_geo_paths(): no property '", id_field, "' in ", file)
    data.frame(id   = as.character(p[[id_field]]),
               d    = feat_d(f$geometry),
               name = if (is.null(p$name)) NA_character_ else p$name,
               lx   = if (is.null(p$label_x)) NA_real_ else p$label_x,
               ly   = if (is.null(p$label_y)) NA_real_ else p$label_y,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  fr <- gj$frame
  attr(out, "frame") <- if (is.null(fr)) c(1152, 748.8) else
    c(as.numeric(fr$width), as.numeric(fr$height))
  out
}

#' The geo config for dd_fig(type = "choropleth"): the paths (id + d, plus
#' any cls/lx/ly/lab columns present) and the frame the file declared, which
#' DD asserts against the fixed 1152 x 748.8.
dd_geo <- function(paths) {
  keep <- intersect(c("id", "d", "cls", "lx", "ly", "lab"), names(paths))
  list(paths = paths[, keep, drop = FALSE],
       frame = as.numeric(attr(paths, "frame")))
}

# ---- the shared diverging bins ----------------------------------------------
#' Quantise a signed value onto the ramp-d5..ramp-r5 classes brief.css
#' defines: n bins each side of zero, capped at +/- cap, negative = dem side,
#' positive = gop side. Mirrors DD.rampClass() in dd-charts.js.
dd_ramp_class <- function(v, cap = 30, n = 5) {
  side <- ifelse(v > 0, "r", "d")
  k <- pmin(n, pmax(1, ceiling(abs(v) / (cap / n))))
  out <- paste0("ramp-", side, k)
  out[v == 0] <- "ramp-d1"
  out[is.na(v)] <- "land"
  out
}

#' The matching key: one item per bin, labelled at the bin edges, for
#' dd_fig(key = dd_ramp_key(...)). `left`/`right` name the two directions.
dd_ramp_key <- function(cap = 30, n = 5, fmt = "signed0",
                        title = NULL, left = NULL, right = NULL) {
  f <- switch(fmt,
    signed0 = function(x) sprintf("%+d", as.integer(x)),
    signed1 = function(x) sprintf("%+.1f", x),
    d       = function(x) sprintf("%d", as.integer(x)),
    function(x) format(x))
  items <- c(
    lapply(rev(seq_len(n)), function(i)
      list(cls = paste0("ramp-d", i), lab = -i * cap / n)),
    lapply(seq_len(n), function(i)
      list(cls = paste0("ramp-r", i), lab = i * cap / n)))
  items <- lapply(items, function(it)
    list(cls = it$cls, label = f(it$lab)))
  list(title = title, items = items, left = left, right = right)
}
