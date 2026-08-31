# ---------------------------------------------------------------------------
# Knowing the shape of a file before asking it anything.
#
# Three functions, all base R, no dependencies, usable from any chapter:
#
#   dd_scan(df)      One row per column: what R stored it as, what it actually
#                    is, how many distinct values, how much is missing, and an
#                    example value. The MISMATCH column is the point.
#   dd_peek(df)      Five random rows, reproducibly. Not head() -- see below.
#   dd_colmap(x)     The whole schema as a field of labelled boxes, coloured by
#                    group. HTML gets an SVG, PDF gets the base-R equivalent.
#
# WHY RANDOM ROWS AND NOT head(). head() shows the first five rows, and the
# first five rows of a real file are almost never typical. They are the
# alphabetically first county, the earliest term, the lowest FIPS code, or
# whatever the export happened to sort by. Files also tend to be *tidiest* at
# the top: the oldest records have had the most correction passes, and the
# weird ones sort to the end. Five random rows are five ordinary rows, which is
# what a reader needs in order to picture the file. The seed is fixed so the
# same five appear every time the chapter is built -- a figure that changes on
# every knit is not a figure anyone can write prose about.
#
# WHY "STORED" AND "IS" ARE DIFFERENT COLUMNS, and why this matters more than
# anything else here. R reports a storage class: integer, character, numeric.
# That is not the measurement level, and the gap between them is where the
# corpus's real errors live. A congressional district stored as an integer is
# not a quantity -- district 12 is not twice district 6, and averaging them is
# nonsense that R will perform without complaint. A GEOID stored as a numeric
# has already lost its leading zero and no longer joins. A birth year is a
# number you may subtract but not sum. dd_scan() names both, and flags the rows
# where the storage type invites an operation the variable cannot support.
#
# LEVELS USED, and they are deliberately coarse:
#   identifier   distinguishes rows; arithmetic on it is always wrong
#   code         a category wearing a number (district, FIPS, precinct)
#   dichotomous  exactly two observed values
#   categorical  a small closed set of values
#   date         a point in time
#   count        a non-negative whole number of things
#   continuous   a measured quantity
#   text         free prose, not a category
#   constant     one value throughout; carries no information in this file
#   empty        no non-missing values at all
#
# REAL ROWS. dd_peek() shows real rows, and for the voter-file chapters that is
# the point: these are public records, published by the state, and a chapter
# that shows a scrubbed imitation of a public file teaches students that the
# file is more secret than it is. Georgia's extract is a public record; so are
# the New Jersey county extracts. Show the rows.
#
# The one column worth a second thought is birth date, and the two files differ:
# Georgia publishes BIRTH YEAR only, while the New Jersey extracts carry a FULL
# DATE OF BIRTH -- which is exactly why false-matches uses New Jersey, since
# full DOB is the field the Crosscheck matching rule actually keys on. Name plus
# full DOB is a different object from name plus birth year, whatever the
# publication status of either. `redact =` remains available for that case; it
# is not applied by default anywhere.
#
# Used by: any chapter. Source it with
#   source("../_lib/structure.R")
# ---------------------------------------------------------------------------

# --- levels -----------------------------------------------------------------

DD_LEVELS <- c("identifier", "code", "dichotomous", "categorical", "date",
               "count", "continuous", "text", "constant", "empty")

# Names that mean "this number is a label". Matched case-insensitively against
# the column name, and only ever used to RECLASSIFY a numeric column as a code
# -- never to override something already obviously categorical.
DD_CODE_RX <- paste0("(^|[^a-z])(geoid|fips|zcta|ansi|",
                     "district|precinct|ward|tract|block|blkgrp|",
                     "cd|sldu|sldl|statefp|countyfp|zip|zipcode|",
                     "id|code|no|num|number)([^a-z]|$)")

DD_ID_RX <- "(^|[^a-z])(id|uuid|key|registration|voter ?reg)([^a-z]|$)"

.dd_is_datish <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(TRUE)
  if (!is.character(x)) return(FALSE)
  v <- x[!is.na(x) & nzchar(x)]
  if (!length(v)) return(FALSE)
  v <- utils::head(v, 200)
  pat <- "^\\d{4}-\\d{2}-\\d{2}|^\\d{1,2}/\\d{1,2}/\\d{2,4}$|^\\d{8}$"
  mean(grepl(pat, v)) > 0.9
}

.dd_whole <- function(x) {
  v <- x[!is.na(x)]
  length(v) > 0 && all(abs(v - round(v)) < 1e-9)
}

dd_level <- function(x, nm = "") {
  v  <- x[!is.na(x)]
  if (is.character(x)) v <- v[nzchar(v)]
  nu <- length(unique(v))

  if (!length(v))           return("empty")
  if (nu == 1L)             return("constant")
  if (.dd_is_datish(x))     return("date")
  if (nu == 2L)             return("dichotomous")

  nml <- tolower(nm)
  if (is.character(x)) {
    # A character column that is nearly all distinct is an identifier, not a
    # category -- unless it is short and code-shaped, in which case it is a code.
    if (nu / length(v) > 0.9 && length(v) > 20) {
      if (grepl(DD_CODE_RX, nml) && mean(nchar(v)) <= 12) return("code")
      return("identifier")
    }
    if (grepl(DD_ID_RX, nml) && nu / length(v) > 0.5)     return("identifier")
    if (mean(nchar(v)) > 40)                              return("text")
    if (grepl(DD_CODE_RX, nml) && mean(nchar(v)) <= 12)   return("code")
    return("categorical")
  }

  if (is.numeric(x)) {
    # A year is not a category and not a quantity you may sum. It orders, it
    # differences, and it belongs with the dates. Without this, `term`, `year`
    # and `congress` -- which appear all over the corpus -- get flagged as
    # numbers-standing-in-for-labels, which is noise, not a finding.
    if (grepl("(^|[^a-z])(year|term|congress|cycle|vintage)([^a-z]|$)", nml) &&
        .dd_whole(x) && min(v) >= 1700 && max(v) <= 2100)
      return("date")
    if (grepl(DD_ID_RX, nml) && .dd_whole(x) && nu / length(v) > 0.9)
      return("identifier")
    if (grepl(DD_CODE_RX, nml) && .dd_whole(x))           return("code")
    # "Few distinct whole numbers" only means categorical when there are enough
    # rows for "few" to be evidence. In an eight-row file every column has few
    # distinct values, and calling a cost per vote a category because the table
    # is short is worse than saying nothing. The cut scales with the file.
    if (.dd_whole(x) && nu <= min(20L, max(2L, ceiling(length(v) / 3))))
      return("categorical")
    if (.dd_whole(x) && min(v) >= 0)                      return("count")
    return("continuous")
  }

  if (is.logical(x)) return("dichotomous")
  "categorical"
}

# Storage classes that invite an operation the level cannot support.
.dd_mismatch <- function(stored, level) {
  num <- stored %in% c("integer", "numeric", "double")
  chr <- stored %in% c("character", "factor")
  if (num && level %in% c("identifier", "code"))
    return("stored as a number; arithmetic on it is meaningless")
  if (num && level == "categorical")
    return("stored as a number; the values are labels, not amounts")
  if (chr && level %in% c("count", "continuous"))
    return("stored as text; will not sort or sum until converted")
  if (chr && level == "date")
    return("stored as text; will sort wrongly unless the format is ISO")
  ""
}

# --- 1. the scan ------------------------------------------------------------

dd_scan <- function(df, examples = TRUE, max_example = 22) {
  stopifnot(is.data.frame(df))
  nms <- names(df)
  out <- data.frame(n = seq_along(nms), column = nms, stringsAsFactors = FALSE)

  out$stored <- vapply(df, function(x) class(x)[1], character(1))
  out$level  <- vapply(seq_along(df), function(i) dd_level(df[[i]], nms[i]),
                       character(1))
  out$distinct <- vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1))
  out$missing  <- vapply(df, function(x) {
    m <- is.na(x) | (is.character(x) & !nzchar(as.character(x)))
    round(100 * mean(m), 1)
  }, numeric(1))
  out$mismatch <- mapply(.dd_mismatch, out$stored, out$level, USE.NAMES = FALSE)

  if (examples) {
    out$example <- vapply(df, function(x) {
      v <- x[!is.na(x)]
      if (is.character(v)) v <- v[nzchar(v)]
      if (!length(v)) return("")
      s <- as.character(v[1])
      if (nchar(s) > max_example) paste0(substr(s, 1, max_example - 1), "…")
      else s
    }, character(1))
  }
  rownames(out) <- NULL
  out
}

# --- 2. five random rows ----------------------------------------------------

# `redact` is a named list: column name -> replacement value (recycled). It is
# OFF unless asked for. Public files are shown as they are; see REAL ROWS above.
# Where it is used, the replacement should be obviously fake -- "EXAMPLE",
# "PAT Q" -- so no reader mistakes a substitution for a capture.
dd_peek <- function(df, n = 5, seed = 84355, cols = NULL, redact = NULL,
                    sorted = TRUE) {
  stopifnot(is.data.frame(df))
  n <- min(n, nrow(df))
  old <- if (exists(".Random.seed", .GlobalEnv)) .GlobalEnv$.Random.seed else NULL
  set.seed(seed)
  i <- sort(sample.int(nrow(df), n))
  if (!is.null(old)) .GlobalEnv$.Random.seed <- old else rm(".Random.seed", envir = .GlobalEnv)

  out <- df[if (sorted) i else sample(i), , drop = FALSE]
  if (!is.null(cols)) out <- out[, intersect(cols, names(out)), drop = FALSE]
  for (k in names(redact)) if (k %in% names(out)) out[[k]] <- redact[[k]]
  rownames(out) <- NULL
  attr(out, "dd_rows") <- i
  out
}

# --- 3. the field map -------------------------------------------------------

# Grey ramp plus one accent, matching the corpus. The accent goes to whichever
# group is largest, because that is the shape of the file worth seeing first.
DD_RAMP <- c("#54278F", "#4F4F4F", "#737373", "#969696",
             "#B0B0B0", "#C8C8C8", "#DEDEDE", "#F2F2F2", "#FAFAFA")

dd_palette <- function(groups) {
  tb  <- sort(table(groups), decreasing = TRUE)
  key <- names(tb)
  fill <- setNames(DD_RAMP[seq_along(key)], key)
  fill[is.na(fill)] <- "#FAFAFA"
  ink  <- setNames(ifelse(seq_along(key) <= 3, "#FFFFFF", "#222222"), key)
  list(fill = fill, ink = ink, order = key, counts = as.integer(tb))
}

.dd_esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  gsub('"', "&quot;", s, fixed = TRUE)
}

.dd_wrap <- function(t, mx) {
  w <- strsplit(t, " ", fixed = TRUE)[[1]]; o <- character(0); c <- ""
  for (x in w) {
    j <- if (nzchar(c)) paste(c, x) else x
    if (nchar(j) > mx && nzchar(c)) { o <- c(o, c); c <- x } else c <- j
  }
  if (nzchar(c)) o <- c(o, c)
  o
}

# x: either a data.frame to scan, or a schema with `column` and a group column.
# by: the grouping column name when x is a schema; ignored when x is raw data
#     (raw data is grouped by `level`).
dd_colmap <- function(x, by = "level", per_row = 9, caption = NULL,
                      html = knitr::is_html_output()) {
  s <- if (all(c("column") %in% names(x)) && by %in% names(x)) x else dd_scan(x)
  if (!"column" %in% names(s)) stop("dd_colmap: need a `column` column")
  if (!by %in% names(s)) by <- "level"
  s$.g <- as.character(s[[by]])
  s$.n <- seq_len(nrow(s))

  pal <- dd_palette(s$.g)
  NPR <- per_row
  NR  <- ceiling(nrow(s) / NPR)

  if (html) {
    W <- 760; ML <- 10; MR <- 10; MT <- 8; CH <- 60
    LEGR <- ceiling(length(pal$order) / 4)
    H <- MT + NR * CH + 18 + LEGR * 20 + 6
    CW <- (W - ML - MR) / NPR
    p <- character(0)
    for (i in seq_len(nrow(s))) {
      g  <- s$.g[i]; nm <- s$column[i]
      tx <- ML + ((i - 1) %% NPR) * CW
      ty <- MT + floor((i - 1) / NPR) * CH
      ln <- .dd_wrap(nm, floor((CW - 12) / 4.9))
      fs <- if (length(ln) > 3) 8 else 9.2
      lab <- paste0(vapply(seq_along(ln), function(k)
        sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-size="%.1fpx" fill="%s">%s</text>',
                CW / 2, CH / 2 + 5 + (k - (length(ln) + 1) / 2) * (fs + 1.4),
                fs, pal$ink[g], .dd_esc(ln[k])), character(1)), collapse = "")
      p <- c(p, sprintf(paste0('<g transform="translate(%.2f,%.0f)">',
        '<rect x="1.5" y="2" width="%.2f" height="%.0f" rx="3" fill="%s"/>',
        '<title>column %d: %s (%s)</title>',
        '<text x="6" y="13" font-size="8px" fill="%s" fill-opacity="0.8">%d</text>',
        '%s</g>'),
        tx, ty, CW - 3, CH - 4, pal$fill[g], i, .dd_esc(nm), .dd_esc(g),
        pal$ink[g], i, lab))
    }
    for (k in seq_along(pal$order)) {
      g <- pal$order[k]
      lx <- ML + ((k - 1) %% 4) * ((W - ML - MR) / 4)
      ly <- MT + NR * CH + 18 + floor((k - 1) / 4) * 20
      p <- c(p, sprintf(paste0('<g transform="translate(%.0f,%.0f)">',
        '<rect width="13" height="13" y="-10" rx="2" fill="%s" stroke="#BBBBBB" stroke-width="0.5"/>',
        '<text x="19" font-size="11.5px" fill="#333">%d  %s</text></g>'),
        lx, ly, pal$fill[g], pal$counts[k], .dd_esc(g)))
    }
    cat(sprintf(paste0('<div style="margin:1em 0">',
      '<svg viewBox="0 0 %d %d" style="max-width:100%%;height:auto;font:12px inherit">%s</svg>',
      '%s</div>'), W, H, paste(p, collapse = ""),
      if (is.null(caption)) "" else
        sprintf('<p style="font-size:0.85em;color:#666;margin-top:0.2em">%s</p>',
                caption)))
    return(invisible(s))
  }

  # --- PDF: same geometry in base R ---
  op <- par(mar = c(0.4 + 1.4 * ceiling(length(pal$order) / 4), 0.4, 0.3, 0.4))
  on.exit(par(op), add = TRUE)
  LEGR <- ceiling(length(pal$order) / 4)
  plot(NA, xlim = c(0, NPR), ylim = c(NR + 0.28 + 0.34 * LEGR, 0), axes = FALSE,
       xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  for (i in seq_len(nrow(s))) {
    rr <- (i - 1) %/% NPR; cc <- (i - 1) %% NPR; g <- s$.g[i]
    x0 <- cc + 0.03; x1 <- cc + 0.97; y0 <- rr + 0.07; y1 <- rr + 0.93
    rect(x0, y0, x1, y1, col = pal$fill[g], border = "white", lwd = 0.7)
    cx <- 0.58
    for (k in c(15, 12, 10)) {
      L <- strwrap(s$column[i], width = k)
      if (max(strwidth(L, cex = cx)) <= (x1 - x0) * 0.90) break
    }
    wd <- max(strwidth(L, cex = cx))
    if (wd > (x1 - x0) * 0.90) cx <- cx * (x1 - x0) * 0.90 / wd
    yc <- (y0 + y1) / 2 + 0.04
    for (k in seq_along(L))
      text((x0 + x1) / 2, yc + (k - (length(L) + 1) / 2) * 0.135, L[k],
           cex = cx, col = pal$ink[g])
    text(x0 + 0.03, y0 + 0.11, i, cex = 0.44, adj = c(0, 0.5), col = pal$ink[g])
  }
  for (k in seq_along(pal$order)) {
    g <- pal$order[k]
    lx <- ((k - 1) %% 4) * (NPR / 4) + 0.03
    ly <- NR + 0.30 + floor((k - 1) / 4) * 0.34
    rect(lx, ly, lx + 0.16, ly + 0.20, col = pal$fill[g], border = "#BBBBBB",
         lwd = 0.5)
    text(lx + 0.22, ly + 0.10, paste0(pal$counts[k], "  ", g), cex = 0.62,
         adj = c(0, 0.5), col = "#333333")
  }
  invisible(s)
}
