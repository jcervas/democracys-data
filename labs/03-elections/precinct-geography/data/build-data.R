# ---------------------------------------------------------------------------
# Geometry for the Houston County figures in precinct-geography-brief.Rmd
#
# The brief must knit to HTML *and* PDF and must not carry a shapefile inside
# it. This script does the spatial work once and writes small, plain CSVs of
# coordinates that base R can `polygon()` and D3 can turn into paths. Nothing
# downstream needs sf.
#
#   derived/fig_houston.csv       Houston County precinct outlines, 2020 and 2024
#   derived/fig_houston_lab.csv   a label point per precinct, flagged new / retired
#   derived/fig_rozr_blocks.csv   the census blocks inside HOUSTON|ROZR
#   derived/fig_rozr_pts.csv      one interior point per block, with population
#   derived/fig_rozr_outline.csv  those blocks dissolved by the 2024 precinct they land in
#   derived/fig_rozr_meta.csv     the numbers the caption quotes
#   derived/fig_twosource.csv     the shapefile's vote totals against the SoS returns
#
# WHY HOUSTON. It is the county the instructor can speak about from memory, and
# it is small enough -- 16 precincts in 2020 -- that the whole name-by-name
# inventory fits on a page. It is also a sharper instance of the brief's Step 6
# than the state as a whole: area calls 15 of its 16 precincts split, population
# calls one.
#
# WHY ROZR. It is the only Houston precinct that population weighting agrees is
# split, and the split created a brand-new 2024 precinct (PEC) out of its
# southern half. Area says 44/56, people say 57/43.
#
# SIMPLIFICATION. Both years are simplified in ONE mapshaper call so that shared
# arcs are detected across the two layers. Boundaries that genuinely did not
# move are therefore simplified identically in both panels, and the only
# differences visible between the panels are real ones. Simplifying the years
# separately would manufacture wobble on unchanged lines -- exactly the thing
# the figure is supposed to be evidence about.
#
# ---------------------------------------------------------------------------
# WHAT THIS READS, AND WHY IT DOES NOT UNZIP ANYTHING
# ---------------------------------------------------------------------------
#
# Everything comes from the SIBLING CHAPTER ga-precinct-returns, which acquired
# the Secretary of State's exports and the census blocks and built the
# crosswalks. This chapter draws no data of its own -- it does the spatial work
# for one county and writes small CSVs of coordinates.
#
#   ../../ga-precinct-returns/data/raw/shp2020/    2020 precinct shapefile
#   ../../ga-precinct-returns/data/raw/shp2024/    2024 precinct shapefile
#   ../../ga-precinct-returns/data/raw/blocks/     2020 census blocks, Georgia
#   ../../ga-precinct-returns/data/derived/        block_assign, crosswalk,
#                                                  crosswalk_pop, precincts
#
# THOSE THREE DIRECTORIES ARE UNZIPPED BY THE SIBLING'S OWN BUILD, not by this
# one. A chapter never writes into another chapter's folder (DATA-LAYOUT rule
# 4), so if they are missing this script STOPS and says which build to run
# rather than reaching over and extracting 260 MB into somebody else's raw/.
#
# BUILD SCRIPT -- may use packages. Run from this directory:
#   Rscript build-data.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({library(sf); library(rmapshaper)})
sf_use_s2(FALSE)

dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")

D     <- "../../ga-precinct-returns/data"
CTY   <- "HOUSTON"
FOCUS <- "HOUSTON|ROZR"
UTM   <- 26917                      # UTM 17N, metres, correct for middle Georgia

# The sibling's build unzips these into its own raw/. If one is missing, say so
# and stop -- do not extract into another chapter's folder.
sh <- function(d, z) {
  p <- file.path(D, "raw", d)
  if (!dir.exists(p))
    stop("missing ", p, "\n  Run ga-precinct-returns/data/build-block-crosswalk.R first; ",
         "it unzips raw/", z, " into raw/", d, ".", call. = FALSE)
  p
}
d20 <- sh("shp2020", "ga_precincts_2020.zip")
d24 <- sh("shp2024", "ga_precincts_2024.zip")
dbl <- sh("blocks",  "ga_blocks_2020.zip")

rd <- function(d, pat) st_transform(st_make_valid(
        st_read(list.files(d, pat, full.names = TRUE)[1], quiet = TRUE)), UTM)

p20 <- rd(d20, "shp$")
p24 <- rd(d24, "shp$")
p20$key <- toupper(trimws(paste(p20$CTYNAME, p20$PRECINCT_N, sep = "|")))
p24$key <- toupper(trimws(paste(p24$CTYNAME, p24$PRECINCT_N, sep = "|")))

# PRECINCT_I is the county's short code; PRECINCT_N its name. For most Houston
# precincts they are the same string, but the three long 2024 names have short
# codes (NHSC, WELL) and the figures need something that fits inside a polygon.
p20$code <- toupper(trimws(p20$PRECINCT_I))
p24$code <- toupper(trimws(p24$PRECINCT_I))

f20 <- p20[!is.na(p20$CTYNAME) & toupper(p20$CTYNAME) == CTY, c("key", "code")]
f24 <- p24[!is.na(p24$CTYNAME) & toupper(p24$CTYNAME) == CTY, c("key", "code")]
cat(sprintf("%s precincts: 2020 = %d, 2024 = %d\n", CTY, nrow(f20), nrow(f24)))

# ---- 1. county precinct outlines, both years, simplified together ----------
f20$yr <- 2020; f24$yr <- 2024
both <- st_cast(rbind(f20, f24), "MULTIPOLYGON")
sm   <- st_cast(ms_simplify(both, keep = 0.06, keep_shapes = TRUE, explode = FALSE),
                "MULTIPOLYGON")
cat(sprintf("simplified: %s -> %s coordinates\n",
            format(nrow(st_coordinates(both)), big.mark = ","),
            format(nrow(st_coordinates(sm)),   big.mark = ",")))

bb <- st_bbox(sm); OX <- unname(bb["xmin"]); OY <- unname(bb["ymin"])

# long format: one row per vertex. `part` separates rings of a multipolygon.
rings <- function(g, yr, key) {
  cc <- st_coordinates(g)
  grp <- interaction(cc[, "L1"], if ("L2" %in% colnames(cc)) cc[, "L2"] else 1,
                     if ("L3" %in% colnames(cc)) cc[, "L3"] else 1, drop = TRUE)
  do.call(rbind, lapply(seq_along(levels(grp)), function(i) {
    z <- cc[grp == levels(grp)[i], , drop = FALSE]
    data.frame(yr = yr, key = key, part = i,
               x = round((z[, "X"] - OX) / 1000, 3),
               y = round((z[, "Y"] - OY) / 1000, 3))
  }))
}
out <- do.call(rbind, lapply(seq_len(nrow(sm)),
        function(i) rings(st_geometry(sm)[i], sm$yr[i], sm$key[i])))
out$name <- sub("^[^|]*\\|", "", out$key)
dd_write_csv(out[, c("yr", "name", "part", "x", "y")], "derived/fig_houston.csv")
cat(sprintf("fig_houston.csv  %s rows, %s\n", format(nrow(out), big.mark = ","),
            format(structure(file.size("derived/fig_houston.csv"), class = "object_size"), units = "auto")))

# ---- 2. label points, and which names are new / retired -------------------
lab <- function(g, yr, key, code) {
  p <- suppressWarnings(st_coordinates(st_point_on_surface(g)))
  data.frame(yr = yr, name = sub("^[^|]*\\|", "", key), code = code,
             x = round((p[1, "X"] - OX) / 1000, 3), y = round((p[1, "Y"] - OY) / 1000, 3))
}
L <- do.call(rbind, lapply(seq_len(nrow(sm)),
       function(i) lab(st_geometry(sm)[i], sm$yr[i], sm$key[i], sm$code[i])))
n20 <- L$name[L$yr == 2020]; n24 <- L$name[L$yr == 2024]
L$status <- ifelse(L$yr == 2020 & !(L$name %in% n24), "retired",
            ifelse(L$yr == 2024 & !(L$name %in% n20), "new", "kept"))
L$focus <- L$name == sub("^[^|]*\\|", "", FOCUS)

# a 2020 precinct counts as SPLIT if population-weighting sends at least 1% of
# its people to more than one 2024 precinct. The 1% floor keeps a handful of
# people on the wrong side of a redrawn line from being called a split.
POP <- read.csv(file.path(D, "derived/crosswalk_pop.csv"), stringsAsFactors = FALSE)
POP <- POP[POP$weight >= 0.01, ]
ntg <- table(POP$from_2020)
L$split <- ifelse(L$yr == 2020, ntg[paste0(CTY, "|", L$name)] > 1, NA)
L$split[is.na(L$split)] <- FALSE
dd_write_csv(L, "derived/fig_houston_lab.csv")
cat(sprintf("retired 2020 names %d; new 2024 names %d; names in both %d; 2020 split %d of %d\n",
            sum(L$status == "retired"), sum(L$status == "new"),
            sum(L$yr == 2020 & L$status == "kept"),
            sum(L$yr == 2020 & L$split), sum(L$yr == 2020)))

# ---- 3. the census blocks inside one 2020 precinct ------------------------
ba <- read.csv(file.path(D, "derived/block_assign.csv"), stringsAsFactors = FALSE,
               colClasses = c(GEOID20 = "character"))
keep <- ba[!is.na(ba$precinct_2020) & ba$precinct_2020 == FOCUS, ]
cat(sprintf("\n%s: %d blocks, %s people\n", FOCUS, nrow(keep),
            format(sum(keep$pop), big.mark = ",")))

bl <- st_read(list.files(dbl, "tabblock20\\.shp$", full.names = TRUE)[1],
              query = sprintf("SELECT GEOID20, POP20 FROM tl_2020_13_tabblock20 WHERE GEOID20 IN (%s)",
                              paste0("'", keep$GEOID20, "'", collapse = ",")), quiet = TRUE)
bl <- st_transform(st_make_valid(bl), UTM)
bl <- merge(bl, keep[, c("GEOID20", "pop", "precinct_2024")], by = "GEOID20")
stopifnot(nrow(bl) == nrow(keep), sum(bl$pop) == sum(keep$pop))

bsm <- ms_simplify(bl, keep = 0.20, keep_shapes = TRUE)
wb  <- st_bbox(bsm); WX <- unname(wb["xmin"]); WY <- unname(wb["ymin"])
brings <- function(g, id) {
  cc <- st_coordinates(g)
  grp <- interaction(cc[, "L1"], if ("L2" %in% colnames(cc)) cc[, "L2"] else 1,
                     if ("L3" %in% colnames(cc)) cc[, "L3"] else 1, drop = TRUE)
  do.call(rbind, lapply(seq_along(levels(grp)), function(i) {
    z <- cc[grp == levels(grp)[i], , drop = FALSE]
    data.frame(id = id, part = i,
               x = round((z[, "X"] - WX) / 1000, 4), y = round((z[, "Y"] - WY) / 1000, 4))
  }))
}
tgt <- sub("^[^|]*\\|", "", bsm$precinct_2024)
bsm$tgt <- ifelse(is.na(tgt), "unplaced", tgt)
B <- do.call(rbind, lapply(seq_len(nrow(bsm)), function(i) brings(st_geometry(bsm)[i], i)))
B$tgt <- bsm$tgt[B$id]; B$pop <- bsm$pop[B$id]
dd_write_csv(B, "derived/fig_rozr_blocks.csv")

bp <- suppressWarnings(st_coordinates(st_point_on_surface(bsm)))
P <- data.frame(id = seq_len(nrow(bsm)), tgt = bsm$tgt, pop = bsm$pop,
                x = round((bp[, "X"] - WX) / 1000, 4), y = round((bp[, "Y"] - WY) / 1000, 4))
dd_write_csv(P, "derived/fig_rozr_pts.csv")

# dissolve the blocks by 2024 target, so the figure can draw the boundary that
# actually did the splitting rather than leaving it implied by a colour change
dis <- ms_dissolve(bsm, field = "tgt")
dis <- st_cast(st_make_valid(dis), "MULTIPOLYGON")
DS <- do.call(rbind, lapply(seq_len(nrow(dis)),
        function(i) { z <- brings(st_geometry(dis)[i], i); z$tgt <- dis$tgt[i]; z }))
dd_write_csv(DS[, c("tgt", "id", "part", "x", "y")], "derived/fig_rozr_outline.csv")
cat(sprintf("fig_rozr_blocks.csv %s rows, %s\n", format(nrow(B), big.mark = ","),
            format(structure(file.size("derived/fig_rozr_blocks.csv"), class="object_size"), units="auto")))
print(aggregate(pop ~ tgt, P, function(x) c(blocks = length(x), pop = sum(x))))

# ---- 4. the numbers the caption quotes ------------------------------------
ar <- read.csv(file.path(D, "derived/crosswalk.csv"),     stringsAsFactors = FALSE)
po <- read.csv(file.path(D, "derived/crosswalk_pop.csv"), stringsAsFactors = FALSE)
A  <- ar[ar$from_2020 == FOCUS, ]; O <- po[po$from_2020 == FOCUS, ]
v  <- st_drop_geometry(p20[p20$key == FOCUS, c("VOTED20", "TRUMP20", "BIDEN20")])
BAL <- as.numeric(v$VOTED20)

tg <- union(O$to_2024, A$to_2024[A$weight > 0.01])
M <- data.frame(
  target = sub("^[^|]*\\|", "", tg),
  w_area = round(A$weight[match(tg, A$to_2024)], 6),
  w_pop  = round(O$weight[match(tg, O$to_2024)], 6),
  pop    = O$pop[match(tg, O$to_2024)])
M$w_area[is.na(M$w_area)] <- 0
M$w_pop[is.na(M$w_pop)]   <- 0
M$pop[is.na(M$pop)]       <- 0
M$ballots_area  <- round(BAL * M$w_area)
M$ballots_pop   <- round(BAL * M$w_pop)
M$ballots_total <- BAL          # constant; the precinct's 2020 ballots cast
M <- M[order(-M$w_pop, -M$w_area), ]
dd_write_csv(M, "derived/fig_rozr_meta.csv")
cat(sprintf("\n%s cast %s ballots in 2020\n", FOCUS, format(BAL, big.mark = ",")))
print(M, row.names = FALSE)

# ---- 5. the two-source check the brief's Step 8 rests on ------------------
# The shapefile carries its own copy of the 2020 vote totals. Step 8 asks
# whether that copy agrees with the Secretary of State's published returns.
# Recomputed here rather than quoted, so the brief and the key can print it
# from a built file. Join on COUNTY|PRECINCT; the precincts that fail to join
# are the ones whose names the two agencies spell differently, which is the
# whole reason the build stopped joining in the first place.
sv <- st_drop_geometry(p20[, c("CTYNAME", "PRECINCT_N", "TRUMP20", "BIDEN20")])
sv$key <- toupper(trimws(paste(sv$CTYNAME, sv$PRECINCT_N, sep = "|")))
sr <- read.csv(file.path(D, "derived/precincts.csv"), stringsAsFactors = FALSE, check.names = FALSE)
sr$key <- toupper(trimws(paste(sr$county, sr$precinct, sep = "|")))
mm <- merge(sv[!duplicated(sv$key), ], sr[!duplicated(sr$key), ], by = "key")
ok <- as.numeric(mm$TRUMP20) == mm[["Donald J. Trump"]] &
      as.numeric(mm$BIDEN20) == mm[["Joseph R. Biden"]]
ok[is.na(ok)] <- FALSE
hh <- grepl(paste0("^", CTY, "\\|"), mm$key)
TS <- data.frame(
  scope     = c("Georgia", CTY),
  matched   = c(nrow(mm), sum(hh)),
  identical = c(sum(ok), sum(ok & hh)),
  differing = c(sum(!ok), sum(!ok & hh)),
  shapefile_precincts = c(nrow(sv), sum(grepl(paste0("^", CTY, "\\|"), sv$key))))
dd_write_csv(TS, "derived/fig_twosource.csv")
cat("\ntwo-source check (shapefile totals vs Secretary of State returns):\n")
print(TS, row.names = FALSE)

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
