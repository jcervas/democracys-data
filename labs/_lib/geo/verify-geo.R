# ---------------------------------------------------------------------------
# verify-geo.R -- read every shared base map back and check the contract:
#
#   * every coordinate sits inside the 1152 x 748.8 frame
#   * the feature count is what the README promises
#   * every feature carries the documented id property, and it is unique
#     (label anchors, where a file has them, sit inside the frame too)
#   * us-frame.json still has the legacy top-level keys the wind-map and
#     mapping chapters read, and a registry entry for every file checked
#
# RUN FROM INSIDE geo/:  Rscript verify-geo.R     (exits non-zero on failure)
# ---------------------------------------------------------------------------

suppressMessages(library(jsonlite))
options(scipen = 999)

FW <- 1152; FH <- 748.8
ok_all <- TRUE
say <- function(...) cat(..., "\n", sep = "")

check <- function(file, n_expect, id_prop, n_min = NULL) {
  fc <- fromJSON(file, simplifyVector = FALSE)
  n  <- length(fc$features)
  xr <- c(Inf, -Inf); yr <- c(Inf, -Inf)
  hit <- function(r, v) c(min(r[1], v), max(r[2], v))
  walk <- function(cc) {
    if (is.list(cc[[1]]) && !is.list(cc[[1]][[1]]) && is.numeric(cc[[1]][[1]])) {
      xr <<- hit(xr, vapply(cc, `[[`, 0, 1))       # cc is a ring of [x, y]
      yr <<- hit(yr, vapply(cc, `[[`, 0, 2))
    } else for (c2 in cc) walk(c2)
  }
  ids <- character(n)
  for (i in seq_len(n)) {
    f <- fc$features[[i]]
    walk(f$geometry$coordinates)
    ids[i] <- as.character(f$properties[[id_prop]] %||% NA)
    lx <- f$properties$label_x
    if (!is.null(lx) && (lx < 0 || lx > FW ||
        f$properties$label_y < 0 || f$properties$label_y > FH))
      ids[i] <- paste0(ids[i], " [label off frame]")
  }
  n_ok  <- if (is.null(n_min)) n == n_expect else n >= n_min
  b_ok  <- xr[1] >= 0 && xr[2] <= FW && yr[1] >= 0 && yr[2] <= FH
  id_ok <- !any(is.na(ids)) && !any(duplicated(ids))
  ok    <- n_ok && b_ok && id_ok
  ok_all <<- ok_all && ok
  say(sprintf("%-28s %s  %4d features (want %s)  bounds [%.1f,%.1f]x[%.1f,%.1f]  id=%s %s",
      file, if (ok) "OK  " else "FAIL", n,
      if (is.null(n_min)) n_expect else paste0(">=", n_min),
      xr[1], xr[2], yr[1], yr[2], id_prop,
      if (id_ok) "unique" else "MISSING/DUP"))
  invisible(ok)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

say("frame is ", FW, " x ", FH, ", y down; every file must fit inside it\n")
check("us-albers.geojson",        51, "st")
check("us-grid.geojson",          51, "st")
check("us-apportionment.geojson", 50, "st")
check("us-counties.geojson",    3143, "GEOID")
check("us-cd-119.geojson",       436, "GEOID")
check("states/TX-2022.geojson",   38, "district")
check("states/TX-2026.geojson",   38, "district")
check("states/FL-2022.geojson",   28, "district")
check("states/FL-2026.geojson",   28, "district")
check("states/GA-vtd-2020.geojson", NA, "id", n_min = 2500)
check("states/GA-vtd-2024.geojson", NA, "id", n_min = 2500)

# The registry, and the legacy shape older consumers read off the top level.
reg <- fromJSON("us-frame.json", simplifyVector = FALSE)
legacy <- c("frame", "conus_proj", "ak_proj", "hi_proj",
            "fit_xmin", "fit_ymax", "fit_s", "fit_ox", "fit_oy", "conus_box")
miss <- setdiff(legacy, names(reg))
say("")
if (length(miss)) { ok_all <- FALSE; say("FAIL us-frame.json lost legacy keys: ", paste(miss, collapse = ", ")) }
files <- c("us-albers.geojson", "us-grid.geojson", "us-apportionment.geojson",
           "us-counties.geojson", "us-cd-119.geojson",
           sprintf("states/%s.geojson", c("TX-2022", "TX-2026", "FL-2022",
                                          "FL-2026", "GA-vtd-2020", "GA-vtd-2024")))
unreg <- setdiff(files, names(reg$frames))
if (length(unreg)) { ok_all <- FALSE; say("FAIL not in registry: ", paste(unreg, collapse = ", ")) }
say("us-frame.json: legacy top-level keys ",
    if (length(miss)) "BROKEN" else "intact", "; ",
    length(reg$frames), " registry entries; every checked file ",
    if (length(unreg)) "NOT " else "", "registered")

say("")
say(if (ok_all) "verify-geo: ALL CHECKS PASSED" else "verify-geo: FAILURES ABOVE")
if (!ok_all) quit(status = 1)
