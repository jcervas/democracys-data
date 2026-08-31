# ---------------------------------------------------------------------------
# Geometry for the national map in residual-votes-brief.Rmd
#
# The brief knits to HTML *and* PDF and must not carry a shapefile. This script
# does the spatial work once and writes a small, plain CSV of county outlines
# that base R can `polygon()` and D3 can turn into paths. Nothing downstream
# needs sf.
#
#   derived/fig_map_counties.csv  one row per polygon ring: id, part, packed coordinates
#   derived/fig_map_attr.csv      one row per county: fips, class, residual rate
#   derived/fig_map_frame.csv     inset boxes and their labels
#   derived/fig_map_meta.csv      the counts the caption quotes
#
# WHY A MAP. The 103 arithmetically impossible counties are the brief's central
# finding and had no picture at all: the Step 6 dot strip shows that they exist
# and how far below zero they sit, but not WHERE they are -- and where turns out
# to matter, because they cluster by state, which is what an administrative
# reporting failure looks like and what a voter-behaviour story would not.
#
# FIPS IS TEXT. A county FIPS is 5 digits and every county in states 01-09
# begins with a zero. read.csv() type-converts the quoted "01001" to the integer
# 1001, which matches no TIGER county. Both this script and the brief read the
# column with colClasses = c(fips = "character").
#
# SIMPLIFICATION. keep = 0.0015 in one topology-aware ms_simplify call over all
# 3,108 contiguous counties: shared arcs are simplified identically, so no gaps
# or overlaps appear between neighbours. This is aggressive -- about ten vertices
# per county -- and it is defensible here in a way it would not be everywhere.
# The figure is evidence about which counties fall in which CLASS, and a county
# is roughly eight pixels across at the rendered width; nothing in the argument
# rests on the shape of a county line. (Contrast ga-precinct-returns, where the
# figure IS about boundaries moving and the simplification had to be chosen so
# it could not manufacture the movement.) Coordinates are then rounded to a
# 1520-unit frame drawn at 760 px, i.e. half-pixel precision. Rounding is applied
# per vertex, so shared vertices land on the same integer and borders stay shut.
#
# ALASKA AND HAWAII are drawn in insets at their own scales and are labelled as
# such. They are not comparable in area to the rest of the frame and the figure
# says so. Puerto Rico and the other territories are not in either data file and
# are not drawn.
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

suppressPackageStartupMessages({library(sf); library(rmapshaper)})
sf_use_s2(FALSE)

co <- read.csv("derived/counties.csv",  stringsAsFactors = FALSE, colClasses = c(fips = "character"))
an <- read.csv("derived/anomalies.csv", stringsAsFactors = FALSE, colClasses = c(fips = "character"))
un <- read.csv("derived/unusable_states.csv", stringsAsFactors = FALSE)
stopifnot(all(nchar(co$fips) == 5), all(nchar(an$fips) == 5))
cat(sprintf("counties.csv %s rows, anomalies.csv %d rows, joined total %s\n",
            format(nrow(co), big.mark = ","), nrow(an),
            format(nrow(co) + nrow(an), big.mark = ",")))

# ---- 1. boundaries ---------------------------------------------------------
dir.create("raw/tiger", showWarnings = FALSE)
Z <- "raw/tiger/tl_2020_us_county.zip"
if (!file.exists(Z))
  prov_fetch("https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/tl_2020_us_county.zip",
             Z, quiet = TRUE)
if (!dir.exists("raw/tiger/us")) utils::unzip(Z, exdir = "raw/tiger/us")
cty <- st_read("raw/tiger/us/tl_2020_us_county.shp", quiet = TRUE)[, c("GEOID", "STATEFP", "NAMELSAD")]

# state names, for the states that contribute no row at all to either data file
# and so cannot supply their own name (Wisconsin and Alaska are both entirely
# absent from the join, which is itself something the map is there to show)
ZS <- "raw/tiger/tl_2020_us_state.zip"
if (!file.exists(ZS))
  prov_fetch("https://www2.census.gov/geo/tiger/TIGER2020/STATE/tl_2020_us_state.zip",
             ZS, quiet = TRUE)
if (!dir.exists("raw/tiger/st")) utils::unzip(ZS, exdir = "raw/tiger/st")
STN <- st_drop_geometry(st_read("raw/tiger/st/tl_2020_us_state.shp", quiet = TRUE))[, c("STATEFP", "NAME")]
names(STN) <- c("st", "state")

TERR <- c("60", "66", "69", "72", "78")            # AS GU MP PR VI: in no data file
cat(sprintf("TIGER 2020: %s counties, %d of them in the five territories\n",
            format(nrow(cty), big.mark = ","), sum(cty$STATEFP %in% TERR)))
cty <- cty[!cty$STATEFP %in% TERR, ]

# ---- 2. the join, reported honestly ---------------------------------------
have <- c(co$fips, an$fips)
cat(sprintf("\nJOIN: %s of %s data rows (%.2f%%) matched a TIGER 2020 county polygon\n",
            format(sum(have %in% cty$GEOID), big.mark = ","),
            format(length(have), big.mark = ","),
            100 * mean(have %in% cty$GEOID)))
missing <- setdiff(have, cty$GEOID)
if (length(missing)) cat("  data rows with no polygon:", paste(missing, collapse = " "), "\n")
nodata <- setdiff(cty$GEOID, have)
cat(sprintf("  the reverse gap: %d of the %s states-and-DC counties never entered the join\n",
            length(nodata), format(nrow(cty), big.mark = ",")))
print(sort(table(cty$STATEFP[cty$GEOID %in% nodata]), decreasing = TRUE))

# the same join done the wrong way, to size the bug this script guards against
wrong <- as.character(as.integer(co$fips))
cat(sprintf("  read as a number instead, %s of %s FIPS codes lose a leading zero\n",
            format(sum(nchar(wrong) < 5), big.mark = ","),
            format(nrow(co), big.mark = ",")))

# ---- 3. classify every drawn county ---------------------------------------
UNUS <- sort(unique(co$state_name[!co$state_usable]))
cat(sprintf("\nstates set aside: %s (%d counties in counties.csv)\n",
            paste(UNUS, collapse = ", "), sum(!co$state_usable)))
stopifnot(setequal(UNUS, un$state))

A <- data.frame(fips = cty$GEOID, stringsAsFactors = FALSE)
A$rate  <- co$residual_rate[match(A$fips, co$fips)]
A$usab  <- co$state_usable[match(A$fips, co$fips)]
A$state <- co$state_name[match(A$fips, co$fips)]
A$name  <- co$county_name[match(A$fips, co$fips)]
A$state[is.na(A$state)] <- an$state_name[match(A$fips[is.na(A$state)], an$fips)]
A$name[is.na(A$name)]   <- an$county_name[match(A$fips[is.na(A$name)], an$fips)]
# the 150 counties in neither file have no name in either file either; TIGER's
# NAMELSAD supplies one in the same style ("Autauga County", "Kusilvak Census
# Area") so a tooltip reads the same whichever source the name came from
tn <- setNames(cty$NAMELSAD, cty$GEOID)
nm <- is.na(A$name)
A$name[nm] <- tn[A$fips[nm]]
cat(sprintf("county names taken from TIGER because neither data file has the row: %d\n",
            sum(nm)))
A$arate <- an$residual_rate[match(A$fips, an$fips)]

# precedence, and it follows the brief's own accounting in Step 9: the 103
# impossible counties are a set apart from counties.csv, so they are classed
# first; then the three set-aside states; then the rate bins; then no data.
A$class <- ifelse(A$fips %in% an$fips, "impossible",
           ifelse(!is.na(A$usab) & !A$usab, "setaside",
           ifelse(is.na(A$rate), "nodata", "ok")))
A$rate[A$class == "impossible"] <- A$arate[A$class == "impossible"]
print(table(A$class))
stopifnot(sum(A$class == "impossible") == nrow(an))
stopifnot(sum(A$class == "setaside")   == sum(!co$state_usable))
stopifnot(sum(A$class == "ok")         == sum(co$state_usable))

# bins for the usable counties. Open at the top because the right tail runs to
# the Wabash County value the brief calls impossible-in-the-other-direction;
# a continuous ramp would spend the whole scale on one county.
BRK <- c(0, 0.5, 1, 1.5, 2.5, 5, Inf)
ok  <- A$class == "ok"
A$bin <- NA_integer_
A$bin[ok] <- as.integer(cut(A$rate[ok], BRK, include.lowest = TRUE, right = FALSE))
cat("\nrate bins among usable counties:\n")
print(setNames(as.vector(table(A$bin[ok])),
               c("[0,0.5)", "[0.5,1)", "[1,1.5)", "[1.5,2.5)", "[2.5,5)", "5+")))

# ---- 4. project: CONUS Albers, Alaska and Hawaii as insets ----------------
prep <- function(d, crs, keep) {
  d <- st_cast(st_transform(st_make_valid(d), crs), "MULTIPOLYGON")
  st_cast(st_make_valid(ms_simplify(d, keep = keep, keep_shapes = TRUE,
                                    explode = FALSE)), "MULTIPOLYGON")
}
conus <- prep(cty[!cty$STATEFP %in% c("02", "15"), ], 5070, 0.0015)

ak <- cty[cty$STATEFP == "02", ]
ak <- prep(ak, 3338, 0.010)

# Hawaii: the main islands only. Honolulu County legally runs 1,500 km northwest
# to Kure Atoll; drawing that would put 99% of the inset in empty ocean and
# shrink the islands anyone can see to nothing. The atolls hold 0 population.
hi <- cty[cty$STATEFP == "15", ]
hi <- suppressWarnings(st_crop(hi, st_bbox(c(xmin = -160.6, ymin = 18.5,
                                             xmax = -154.5, ymax = 22.5),
                                           crs = st_crs(hi))))
hi <- prep(hi, 26904, 0.02)

cat(sprintf("\nsimplified: CONUS %s, Alaska %s, Hawaii %s coordinates\n",
            format(nrow(st_coordinates(conus)), big.mark = ","),
            format(nrow(st_coordinates(ak)),    big.mark = ","),
            format(nrow(st_coordinates(hi)),    big.mark = ",")))

# lay the three pieces out in one 1520 x H frame
FW  <- 1520
cb  <- st_bbox(conus)
S   <- FW / unname(cb["xmax"] - cb["xmin"])
FH  <- ceiling(unname(cb["ymax"] - cb["ymin"]) * S)
place <- function(g, s, dx, dy, bb = st_bbox(g))
  list(g = g, s = s, dx = dx, dy = dy, bb = bb)

akb <- st_bbox(ak); hib <- st_bbox(hi)
AKS <- (FW * 0.215) / unname(akb["xmax"] - akb["xmin"])
HIS <- (FW * 0.105) / unname(hib["xmax"] - hib["xmin"])
PIECES <- list(
  place(conus, S,   -unname(cb["xmin"])  * S,          -unname(cb["ymin"])  * S),
  place(ak,    AKS, -unname(akb["xmin"]) * AKS + 8,    -unname(akb["ymin"]) * AKS + 8),
  place(hi,    HIS, -unname(hib["xmin"]) * HIS + 360,  -unname(hib["ymin"]) * HIS + 14))

# ---- 5. pack rings ---------------------------------------------------------
# One row per ring. `pts` is "x0 y0 dx dy dx dy ..." -- the first vertex in the
# 1520-unit frame, then integer steps. Three thousand counties of absolute
# coordinates come to 304 KB; the same rings as steps come to well under half
# that, because a step between neighbouring vertices is one or two digits where
# an absolute coordinate is three or four. Decoding is one line in either
# language:
#     R   v <- as.integer(strsplit(p, " ")[[1]])
#         x <- cumsum(v[c(TRUE, FALSE)]); y <- cumsum(v[c(FALSE, TRUE)])
#     JS  v.split(" ").map(Number), then a running sum over alternate entries
#
# Rounding is per vertex, so a vertex shared by two counties rounds to the same
# integer in both and the shared border does not open up. Consecutive duplicates
# created by the rounding are dropped; a ring that collapses below 3 points is
# dropped, and the number of those is printed rather than passed over.
collapsed <- 0
pack <- function(PIECES, key) {
  out <- list()
  for (P in PIECES) {
    g <- P$g
    for (i in seq_len(nrow(g))) {
      cc <- st_coordinates(st_geometry(g)[i])
      grp <- interaction(cc[, "L1"], if ("L2" %in% colnames(cc)) cc[, "L2"] else 1,
                         if ("L3" %in% colnames(cc)) cc[, "L3"] else 1, drop = TRUE)
      for (j in seq_along(levels(grp))) {
        z <- cc[grp == levels(grp)[j], , drop = FALSE]
        x <- as.integer(round(z[, "X"] * P$s + P$dx))
        y <- as.integer(round(z[, "Y"] * P$s + P$dy))
        k <- c(TRUE, x[-1] != x[-length(x)] | y[-1] != y[-length(y)])
        x <- x[k]; y <- y[k]
        if (length(x) < 3) { collapsed <<- collapsed + 1; next }
        dx <- c(x[1], diff(x)); dy <- c(y[1], diff(y))
        stopifnot(identical(cumsum(dx), x), identical(cumsum(dy), y))
        out[[length(out) + 1]] <- data.frame(
          id = g[[key]][i], part = j,
          pts = paste(as.vector(rbind(dx, dy)), collapse = " "),
          stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}
nvert <- function(d) sum(lengths(gregexpr(" ", d$pts)) + 1) / 2

R <- pack(PIECES, "GEOID"); names(R)[1] <- "fips"
cat(sprintf("\ncounties: %s rings, %s vertices; %d rings collapsed below 3 points and were dropped\n",
            format(nrow(R), big.mark = ","), format(nvert(R), big.mark = ","), collapsed))
cat(sprintf("counties with at least one ring drawn: %s of %s\n",
            format(length(unique(R$fips)), big.mark = ","),
            format(nrow(cty), big.mark = ",")))

# ---- 5b. state outlines, dissolved from the SIMPLIFIED counties -----------
# Dissolving after simplification rather than simplifying the state file makes
# the state line the union of county lines exactly. Simplifying the two layers
# separately would leave state borders drifting off the county borders they are
# made of, which the reader would see as a defect in the data.
dis <- function(g) {
  d <- st_cast(st_make_valid(ms_dissolve(g, field = "STATEFP")), "MULTIPOLYGON")
  d[order(d$STATEFP), ]
}
SP <- lapply(PIECES, function(P) { P$g <- dis(P$g); P })
SO <- pack(SP, "STATEFP"); names(SO)[1] <- "st"
cat(sprintf("state outlines: %d rings, %s vertices\n",
            nrow(SO), format(nvert(SO), big.mark = ",")))

A <- A[A$fips %in% R$fips, ]
A$rate <- round(A$rate, 3)
# state names live in their own 51-row lookup rather than being repeated on
# every one of 3,143 county rows
A$st <- substr(A$fips, 1, 2)
SL <- STN[STN$st %in% A$st, ]
SL <- SL[order(SL$st), ]
stopifnot(setequal(SL$st, unique(A$st)))
# where a data file did supply a state name it must agree with TIGER's
chk <- unique(A[!is.na(A$state), c("st", "state")])
stopifnot(all(chk$state == SL$state[match(chk$st, SL$st)]))
cat(sprintf("\nstate lookup: %d state/DC codes, %d of them naming themselves in the data files\n",
            nrow(SL), nrow(chk)))
write.csv(SL, "derived/fig_map_states.csv", row.names = FALSE)
write.csv(SO, "derived/fig_map_state_lines.csv", row.names = FALSE)
write.csv(R, "derived/fig_map_counties.csv", row.names = FALSE)
write.csv(A[, c("fips", "name", "class", "bin", "rate")],
          "derived/fig_map_attr.csv", row.names = FALSE)

FR <- data.frame(
  piece = c("Alaska", "Hawaii"),
  x = c(8, 360), y = c(8, 14),
  w = round(c(FW * 0.215, FW * 0.105)),
  h = round(c(unname(akb["ymax"] - akb["ymin"]) * AKS,
              unname(hib["ymax"] - hib["ymin"]) * HIS)),
  note = c("inset, not to scale", "inset, main islands only, not to scale"))
write.csv(FR, "derived/fig_map_frame.csv", row.names = FALSE)

# ---- 6. the numbers the caption quotes ------------------------------------
u <- co[co$state_usable, ]
# states with no drawn county in the join at all -- the whole state is "no data"
whole <- names(which(tapply(A$class == "nodata", A$st, all)))
cat(sprintf("\nstates absent from the join entirely: %s\n",
            paste(SL$state[match(whole, SL$st)], collapse = ", ")))
M <- data.frame(
  key = c("frame_w", "frame_h", "n_drawn", "n_impossible", "n_setaside",
          "n_nodata", "n_ok", "median_ok", "n_states_absent", "n_va_nodata",
          "worst_impossible_gap", "impossible_states", "top_impossible_state",
          "top_impossible_n"),
  value = c(FW, FH, nrow(A), sum(A$class == "impossible"),
            sum(A$class == "setaside"), sum(A$class == "nodata"),
            sum(A$class == "ok"), round(median(u$residual_rate), 3),
            length(whole),
            sum(A$class == "nodata" & A$st == "51"),
            max(an$total_votes - an$ballots),
            length(unique(an$state_name)),
            names(sort(table(an$state_name), decreasing = TRUE))[1],
            max(table(an$state_name))),
  stringsAsFactors = FALSE)
write.csv(data.frame(state = SL$state[match(whole, SL$st)]),
          "derived/fig_map_absent.csv", row.names = FALSE)
write.csv(M, "derived/fig_map_meta.csv", row.names = FALSE)
print(M, row.names = FALSE)

sz <- function(f) format(structure(file.size(f), class = "object_size"), units = "auto")
cat(sprintf("\nfig_map_counties.csv %s | fig_map_attr.csv %s | frame %s | meta %s\n",
            sz("derived/fig_map_counties.csv"), sz("derived/fig_map_attr.csv"),
            sz("derived/fig_map_frame.csv"), sz("derived/fig_map_meta.csv")))
