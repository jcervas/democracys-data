# ---------------------------------------------------------------------------
# build-areal-units.R -- the modifiable areal unit problem, on Georgia's 2020
# census blocks.  Time is held fixed.  Only the UNIT varies.
#
# TWO EFFECTS, BUILT SEPARATELY
#
#   SCALE   the same blocks aggregated up the census hierarchy
#           block -> block group -> tract -> county.  Nothing is redrawn; the
#           units simply get bigger.  Because block group and tract GEOIDs are
#           PREFIXES of the block GEOID, this needs no spatial operation at all.
#
#   ZONING  the same blocks partitioned into the SAME NUMBER of units of the
#           SAME POPULATION, with only the orientation of the cuts changed.
#           Fulton County, 6 units (the size of its Board of Commissioners),
#           180 partitions at one-degree rotations.  Nothing about the people
#           changes.  The number of majority-Black units does.
#
# WRITES (all read by the three .Rmd documents, which are base R)
#   derived/facts.csv            single scalars, name/value/note
#   derived/ladder_state.csv     the scale ladder, all of Georgia
#   derived/ladder_fulton.csv    the scale ladder, Fulton County only
#   derived/dens_fulton.csv      population-weighted density of unit Black share by level
#   derived/map_units.csv        polygon rings for the Fulton choropleth panels
#   derived/map_vals.csv         unit -> Black share, population, fill colour
#   derived/dots_fulton.csv      dot-density points, 1 dot = 1,000 people
#   derived/county_outline.csv   Fulton's outline
#   derived/zoning_sweep.csv     family x angle x unit: population, Black population, share
#   derived/zoning_summary.csv   family x angle: majority-Black units, spread, deviation,
#                        contiguity
#   derived/zoning_plan_geo.csv    dissolved boundaries of the four headline plans
#   derived/zoning_plan_units.csv  the six units of each of the four headline plans
#
# SOURCES (fetched 2026-08-10)
#   derived/ga_block_race.csv        built by build-block-race.R in this folder, from
#                            the 2020 Census P.L. 94-171 file.  232,717 rows.
#   tl_2020_13_tabblock20    TIGER/Line 2020 Georgia blocks, already present at
#                            ../../../03-elections/ga-precinct-returns/data/raw/blocks/. 232,717 rows.
#                            INTPTLAT20/INTPTLON20 are the Census Bureau's own
#                            interior points -- the same idea as assignPolys
#                            (Cervas, R-Functions), computed upstream.
#   raw/tl_2020_13_bg.zip        https://www2.census.gov/geo/tiger/TIGER2020/BG/tl_2020_13_bg.zip
#   raw/tl_2020_13_tract.zip     https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_13_tract.zip
#
# BUILD SCRIPT -- may use packages.  The student documents are base R and read
# only the CSVs above.  Run from this directory:  Rscript build-areal-units.R
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


source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
options(scipen = 999, stringsAsFactors = FALSE)
set.seed(20260810)

BLOCKS <- "../../../03-elections/ga-precinct-returns/data/raw/blocks/tl_2020_13_tabblock20.shp"
CTY    <- "121"                       # Fulton County, Georgia
CTYNM  <- "Fulton County"
K      <- 6L                          # Fulton's Board of Commissioners districts
stopifnot(file.exists(BLOCKS))

pcT <- function(x, k = 1) formatC(x, format = "f", digits = k)

# =========================================================================
# 1.  Blocks with race.  GEOIDs are CHARACTER.
# =========================================================================
b <- read.csv("derived/ga_block_race.csv", colClasses = c(GEOID20 = "character"))
stopifnot(all(nchar(b$GEOID20) == 15), nrow(b) == 232717,
          sum(b$pop) == 10711908)
cat(sprintf("blocks %s   population %s   any-part Black %s (%.2f%%)\n",
            format(nrow(b), big.mark = ","), format(sum(b$pop), big.mark = ","),
            format(sum(b$black_any), big.mark = ","),
            100 * sum(b$black_any) / sum(b$pop)))

# The census hierarchy is literally a string prefix.  No geometry required.
b$bg     <- substr(b$GEOID20, 1,  12)
b$tract  <- substr(b$GEOID20, 1,  11)
b$county <- substr(b$GEOID20, 1,   5)
stopifnot(length(unique(nchar(b$bg))) == 1)

# =========================================================================
# 2.  The scale ladder
# =========================================================================
# Every statistic here is POPULATION-WEIGHTED.  Unweighted unit averages would
# let a 3-person block count as much as a 6,000-person tract, which is its own
# (different) mistake.
roll <- function(d, key) {
  a <- data.frame(
    unit  = names(tapply(d$pop, d[[key]], sum)),
    pop   = as.numeric(tapply(d$pop,           d[[key]], sum)),
    black = as.numeric(tapply(d$black_any,     d[[key]], sum)),
    vap   = as.numeric(tapply(d$vap,           d[[key]], sum)))
  a$share <- ifelse(a$pop > 0, 100 * a$black / a$pop, NA_real_)
  a$kid   <- ifelse(a$pop > 0, 100 * (a$pop - a$vap) / a$pop, NA_real_)
  a
}
wmean <- function(x, w) sum(x * w) / sum(w)
wsd   <- function(x, w) sqrt(sum(w * (x - wmean(x, w))^2) / sum(w))
wcor  <- function(x, y, w) {
  mx <- wmean(x, w); my <- wmean(y, w)
  sum(w * (x - mx) * (y - my)) /
    sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
}
wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[findInterval(p, cw) + 1L]
}

ladder <- function(d, keys, labels) {
  out <- NULL
  BT <- sum(d$black_any)
  for (i in seq_along(keys)) {
    a <- roll(d, keys[i])
    k <- a$pop > 0
    a <- a[k, ]
    maj <- a$share > 50
    out <- rbind(out, data.frame(
      level          = labels[i],
      units          = nrow(a),
      median_pop     = round(median(a$pop)),
      mean_share     = wmean(a$share, a$pop),
      sd_share       = wsd(a$share, a$pop),
      p10_share      = wq(a$share, a$pop, 0.10),
      p90_share      = wq(a$share, a$pop, 0.90),
      max_share      = max(a$share),
      units_majority = sum(maj),
      pct_black_in_majority = 100 * sum(a$black[maj]) / BT,
      corr_child     = wcor(a$share, a$kid, a$pop)))
  }
  out
}

LKEY <- c("GEOID20", "bg", "tract", "county")
LLAB <- c("Census block", "Block group", "Census tract", "County")
ls_state <- ladder(b, LKEY, LLAB)
cat("\n---- scale ladder, all of Georgia ----\n"); print(ls_state, row.names = FALSE)

f <- b[b$county == paste0("13", CTY), ]
ls_ful <- ladder(f, LKEY, LLAB)
cat(sprintf("\n---- scale ladder, %s (%s blocks) ----\n", CTYNM,
            format(nrow(f), big.mark = ",")))
print(ls_ful, row.names = FALSE)

dd_write_csv(ls_state, "derived/ladder_state.csv")
dd_write_csv(ls_ful, "derived/ladder_fulton.csv")

# =========================================================================
# 3.  Population-weighted density of unit Black share, by level (Fulton)
# =========================================================================
# One common bandwidth for all four levels, so the collapse in spread is a
# property of the data and not of the smoother.
BW <- 3
dens <- NULL
for (i in seq_along(LKEY)) {
  a <- roll(f, LKEY[i]); a <- a[a$pop > 0, ]
  if (nrow(a) < 2) {                                  # the county: one number
    dens <- rbind(dens, data.frame(level = LLAB[i], x = a$share, y = NA_real_))
    next
  }
  dd <- density(a$share, weights = a$pop / sum(a$pop), bw = BW,
                from = 0, to = 100, n = 201)
  dens <- rbind(dens, data.frame(level = LLAB[i], x = dd$x, y = dd$y))
}
dens$x <- round(dens$x, 3); dens$y <- round(dens$y, 6)
dd_write_csv(dens, "derived/dens_fulton.csv")

# =========================================================================
# 4.  Geometry for the Fulton choropleth panels
# =========================================================================
q <- function(where) sprintf(
  "SELECT GEOID20, INTPTLAT20, INTPTLON20 FROM tl_2020_13_tabblock20 WHERE %s", where)
cat("\nreading Fulton block geometry ...\n")
gb <- st_read(BLOCKS, query = sprintf(
  "SELECT GEOID20 FROM tl_2020_13_tabblock20 WHERE COUNTYFP20 = '%s'", CTY),
  quiet = TRUE)
gb <- st_transform(st_make_valid(gb), 5070)
stopifnot(nrow(gb) == nrow(f))

getshp <- function(url, zipname, pat) {
  if (!file.exists(zipname)) {
    cat("downloading", url, "\n")
    prov_fetch(url, zipname, mode = "wb", quiet = TRUE)
  }
  d <- sub("\\.zip$", "", zipname)
  if (!dir.exists(d)) utils::unzip(zipname, exdir = d)
  st_read(list.files(d, pat, full.names = TRUE)[1], quiet = TRUE)
}
gg <- getshp("https://www2.census.gov/geo/tiger/TIGER2020/BG/tl_2020_13_bg.zip",
             "raw/tl_2020_13_bg.zip", "\\.shp$")
gt <- getshp("https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_13_tract.zip",
             "raw/tl_2020_13_tract.zip", "\\.shp$")
gg <- st_transform(st_make_valid(gg[gg$COUNTYFP == CTY, "GEOID"]), 5070)
gt <- st_transform(st_make_valid(gt[gt$COUNTYFP == CTY, "GEOID"]), 5070)
cat(sprintf("block groups %d   tracts %d\n", nrow(gg), nrow(gt)))
stopifnot(nrow(gg) == length(unique(f$bg)), nrow(gt) == length(unique(f$tract)))

outline <- st_union(gt)

# One colour ramp, computed HERE, so the HTML and the PDF are painted from the
# same hex strings and cannot drift apart.
RAMP <- colorRampPalette(c("#f7f7f7", "#d9e6f2", "#8fbedd", "#3f8dc0",
                           "#1f5d8c", "#12395a"))(101)
fillfor <- function(s) ifelse(is.na(s), "#e6e6e6", RAMP[pmin(101L, pmax(1L, floor(s + 0.5) + 1L))])

# Long-format rings, in kilometres, rounded to 100 m -- about a fifth of a pixel
# at the size these panels are drawn.  Units are numbered rather than named so
# that the coordinate table stays small enough to inline in an HTML document;
# `derived/map_vals.csv` carries the number back to the real GEOID.
LEV <- c("Census block" = "b", "Block group" = "g", "Census tract" = "t",
         "County" = "c")
rings <- function(g, level, digits = 1) {
  out <- vector("list", length(st_geometry(g)))
  for (i in seq_along(out)) {
    p <- st_coordinates(st_geometry(g)[i])
    part <- as.integer(factor(paste(
      p[, "L1"], if ("L2" %in% colnames(p)) p[, "L2"] else 1,
      if ("L3" %in% colnames(p)) p[, "L3"] else 1)))
    out[[i]] <- data.frame(lev = unname(LEV[level]), uid = i, part = part,
                           x = round(p[, "X"] / 1000, digits),
                           y = round(p[, "Y"] / 1000, digits))
  }
  do.call(rbind, out)
}
thin <- function(g, tol) st_simplify(g, dTolerance = tol, preserveTopology = TRUE)

# Blocks with nobody in them carry no Black share and would be drawn grey; the
# county fill underneath is the same grey, so they are simply left out.  That
# removes 3,074 polygons and changes nothing visible.
cat("simplifying and flattening geometry ...\n")
keep_b <- f$pop[match(gb$GEOID20, f$GEOID20)] > 0
gbk <- gb[keep_b, ]
cat(sprintf("populated blocks drawn: %s of %s\n",
            format(nrow(gbk), big.mark = ","), format(nrow(gb), big.mark = ",")))

mu <- rbind(
  rings(thin(gbk, 150),    "Census block"),
  rings(thin(gg,   80),    "Block group"),
  rings(thin(gt,   50),    "Census tract"),
  rings(thin(outline, 50), "County"))
mu <- mu[!is.na(mu$x), ]
# drop rings that collapsed to nothing under simplification
np <- ave(mu$x, mu$lev, mu$uid, mu$part, FUN = length)
mu <- mu[np >= 4, ]
dd_write_csv(mu, "derived/map_units.csv")
cat(sprintf("map_units.csv: %s coordinate rows, %s\n",
            format(nrow(mu), big.mark = ","),
            format(structure(file.size("derived/map_units.csv"), class = "object_size"),
                   units = "auto")))

# uid is the row order of the geometry object at each level; line the values up
# with it so the .Rmd can join on lev + uid alone.
vals <- NULL
ids  <- list(gbk$GEOID20, gg$GEOID, gt$GEOID, "13121")
for (i in seq_along(LKEY)) {
  a <- roll(f, LKEY[i])
  a <- a[match(ids[[i]], a$unit), ]
  vals <- rbind(vals, data.frame(
    lev = unname(LEV[LLAB[i]]), uid = seq_along(ids[[i]]), unit = ids[[i]],
    pop = a$pop, black = a$black, share = round(a$share, 2)))
}
# two Fulton block groups and one tract hold nobody (an airport ramp, a river
# reach, a park); they get the grey no-data fill and are excluded from every
# statistic above.
vals$pop[is.na(vals$pop)] <- 0; vals$black[is.na(vals$black)] <- 0
cat(sprintf("units with no population: %d\n", sum(is.na(vals$share))))
vals$fill <- fillfor(vals$share)
dd_write_csv(vals, "derived/map_vals.csv")

oc <- rings(thin(outline, 50), "County")
dd_write_csv(oc[, c("part", "x", "y")], "derived/county_outline.csv")

# =========================================================================
# 5.  Interior points, and the rotating equal-population partitions
# =========================================================================
cat("\nreading Fulton interior points ...\n")
ip <- st_drop_geometry(st_read(BLOCKS, query = q(sprintf("COUNTYFP20 = '%s'", CTY)),
                               quiet = TRUE))
ip$lat <- as.numeric(ip$INTPTLAT20); ip$lon <- as.numeric(ip$INTPTLON20)
pt <- st_transform(st_as_sf(ip, coords = c("lon", "lat"), crs = 4326), 5070)
xy <- st_coordinates(pt) / 1000
ip$x <- xy[, 1]; ip$y <- xy[, 2]
r <- f[match(ip$GEOID20, f$GEOID20), ]
stopifnot(!anyNA(r$pop), identical(r$GEOID20, ip$GEOID20))
ip$pop <- r$pop; ip$black <- r$black_any; ip$vap <- r$vap; ip$bvap <- r$black_any_vap
CX <- mean(range(ip$x)); CY <- mean(range(ip$y))
TOT <- sum(ip$pop); QUOTA <- TOT / K

# ---- two families of equal-population partition ---------------------------
#
# Both families use the same 11,748 blocks, produce the same 6 units, and give
# every unit the same population.  They differ only in the RULE that draws the
# lines, and within each family only in one angle.  Neither rule looks at race.
#
#   STRIP  K parallel slabs perpendicular to direction theta, cut so each slab
#          holds a sixth of the county.  180 plans, theta = 0..179 degrees.
#
#   SPLIT  recursive bisection -- the shortest-splitline idea that has actually
#          been proposed as a redistricting algorithm.  Cut the county into 3+3
#          along theta, each half into 1+2 along theta+90, the 2 into 1+1 along
#          theta+180.  180 plans, theta = 0..179 degrees.  These come out
#          compact and district-shaped.

part_strip <- function(theta) {
  a <- theta * pi / 180
  s <- (ip$x - CX) * cos(a) + (ip$y - CY) * sin(a)
  o <- order(s, ip$GEOID20)
  cum <- cumsum(ip$pop[o])
  g <- findInterval(cum - ip$pop[o] / 2, seq_len(K - 1L) * QUOTA) + 1L
  grp <- integer(nrow(ip)); grp[o] <- g
  grp
}
part_split <- function(theta) {
  rec <- function(idx, k, th) {
    if (k == 1L) return(list(idx))
    kA <- k %/% 2L; a <- th * pi / 180
    s <- ip$x[idx] * cos(a) + ip$y[idx] * sin(a)
    o <- idx[order(s, ip$GEOID20[idx])]
    j <- which.min(abs(cumsum(ip$pop[o]) - sum(ip$pop[idx]) * kA / k))
    c(rec(o[1:j], kA, th + 90), rec(o[(j + 1L):length(o)], k - kA, th + 90))
  }
  pieces <- rec(seq_len(nrow(ip)), K, theta)
  grp <- integer(nrow(ip))
  for (i in seq_along(pieces)) grp[pieces[[i]]] <- i
  grp
}
FAM <- list(strip = part_strip, split = part_split)

# ---- contiguity, for every plan -------------------------------------------
# A unit that is in two pieces is not a district.  Build the block adjacency
# graph once (queen contiguity on the block polygons) and walk each unit's
# induced subgraph.  Blocks separated by water do not touch, so an island
# reads as disconnected -- which is what a court would say too.
cat("building block adjacency ...\n")
NB <- st_intersects(gb)
NB <- NB[match(ip$GEOID20, gb$GEOID20)]
remap <- match(gb$GEOID20, ip$GEOID20)
NB <- lapply(NB, function(z) remap[z])
stopifnot(length(NB) == nrow(ip))

contig <- function(grp) {
  ok <- 0L; worst <- 100
  for (u in sort(unique(grp))) {
    idx <- which(grp == u)
    inu <- logical(nrow(ip)); inu[idx] <- TRUE
    seen <- logical(nrow(ip)); comp <- numeric(0)
    for (s0 in idx) {                      # every component, not just the first
      if (seen[s0]) next
      seen[s0] <- TRUE; stk <- s0; m <- ip$pop[s0]; nn <- 1L
      while (length(stk)) {
        v <- stk[length(stk)]; stk <- stk[-length(stk)]
        w <- NB[[v]]; w <- w[inu[w] & !seen[w]]
        if (length(w)) { seen[w] <- TRUE; stk <- c(stk, w); m <- m + sum(ip$pop[w]); nn <- nn + length(w) }
      }
      comp <- c(comp, m)
    }
    if (length(comp) == 1L) ok <- ok + 1L
    worst <- min(worst, 100 * max(comp) / sum(ip$pop[idx]))
  }
  c(units_contig = ok, largest_piece_pct = worst)
}

summ <- function(grp) {
  pp <- as.numeric(tapply(ip$pop,   grp, sum))
  bb <- as.numeric(tapply(ip$black, grp, sum))
  vv <- as.numeric(tapply(ip$vap,   grp, sum))
  cc <- as.numeric(tapply(ip$bvap,  grp, sum))
  o  <- order(-bb / pp)                      # units ordered most Black first
  list(pop = pp[o], black = bb[o], share = 100 * bb[o] / pp[o],
       bvap = 100 * cc[o] / vv[o],
       dev = 100 * max(abs(pp - QUOTA)) / QUOTA)
}

TH <- 0:179
sw <- NULL; sm <- NULL
for (fam in names(FAM)) {
  cat(sprintf("building %d '%s' partitions ...\n", length(TH), fam))
  for (th in TH) {
    grp <- FAM[[fam]](th); S <- summ(grp); C <- contig(grp)
    sw <- rbind(sw, data.frame(family = fam, angle = th, unit = seq_len(K),
                               pop = S$pop, black = S$black,
                               share = round(S$share, 3), bvap = round(S$bvap, 3)))
    sm <- rbind(sm, data.frame(family = fam, angle = th,
                               n_majority      = sum(S$share > 50),
                               n_majority_bvap = sum(S$bvap  > 50),
                               min_share = round(min(S$share), 3),
                               max_share = round(max(S$share), 3),
                               spread    = round(max(S$share) - min(S$share), 3),
                               max_dev_pct = round(S$dev, 3),
                               units_contig = C[["units_contig"]],
                               largest_piece_pct = round(C[["largest_piece_pct"]], 2)))
  }
}
dd_write_csv(sw, "derived/zoning_sweep.csv")
dd_write_csv(sm, "derived/zoning_summary.csv")

cat(sprintf("\n---- %d equal-population partitions of the same blocks ----\n", nrow(sm)))
print(table(family = sm$family, `majority-Black units` = sm$n_majority))
cat(sprintf("population deviation, worst unit of any plan: %.2f%% of the quota\n",
            max(sm$max_dev_pct)))

# ---- what "contiguous" can mean here ---------------------------------------
# STRICT contiguity -- every one of a unit's blocks reachable from every other
# through shared boundary -- is satisfied by NO plan in either family, and the
# reason is not that the units are shaped like inkblots.  Georgia's block layer
# assigns the bed of the Chattahoochee, the interstate right-of-way and the
# airport apron to blocks of their own, and those blocks do not share a boundary
# with anything on the far side.  Any cut across the county therefore strands a
# handful of empty or near-empty blocks in a component of their own.
#
# So the operational criterion is EFFECTIVE contiguity: every unit's largest
# connected component must hold at least 99% of that unit's people.  A unit that
# passes is one whose population is a single connected mass, whatever the
# slivers do.  This is stated in the documents rather than hidden.
CONTIG_THR <- 99
cat(sprintf("plans whose six units are each ONE connected piece (strict): %d of %d\n",
            sum(sm$units_contig == K), nrow(sm)))
cat(sprintf("strict contiguous-unit count per plan, range %d to %d of %d\n",
            min(sm$units_contig), max(sm$units_contig), K))
cn <- sm[sm$largest_piece_pct >= CONTIG_THR, ]
stopifnot(nrow(cn) > 0)
cat(sprintf("plans effectively contiguous (every unit >= %g%% of its people in one piece): %d of %d (%d 'split', %d 'strip')\n",
            CONTIG_THR, nrow(cn), nrow(sm),
            sum(cn$family == "split"), sum(cn$family == "strip")))
cat(sprintf("among effectively contiguous plans, majority-Black units range %d to %d\n",
            min(cn$n_majority), max(cn$n_majority)))
print(table(family = cn$family, `majority-Black units` = cn$n_majority))

# The two headline plans: fewest majority-Black units anywhere, and most.
i_lo <- order(sm$n_majority,  sm$max_share)[1]
i_hi <- order(-sm$n_majority, -sm$max_share)[1]
# and the same two extremes restricted to the effectively contiguous plans
kk   <- which(sm$largest_piece_pct >= CONTIG_THR)
j_lo <- kk[order(cn$n_majority,  cn$max_share)[1]]
j_hi <- kk[order(-cn$n_majority, -cn$max_share)[1]]
PL <- list(fewest = sm[i_lo, ], most = sm[i_hi, ],
           fewest_contig = sm[j_lo, ], most_contig = sm[j_hi, ])
for (nmz in names(PL)) cat(sprintf(
  "%-7s majority-Black units: %s %d deg -> %d of %d   (units %.1f%% to %.1f%%)\n",
  nmz, PL[[nmz]]$family, PL[[nmz]]$angle, PL[[nmz]]$n_majority, K,
  PL[[nmz]]$min_share, PL[[nmz]]$max_share))

# ---- the two headline plans, as geography ---------------------------------
# Dissolve the blocks of each unit into one polygon.  This is also the only
# check on contiguity there is: a unit that comes back as several polygons is
# not a district anyone could run an election in.
plan_geo <- function(fam, theta, tag) {
  grp <- FAM[[fam]](theta)
  grp <- grp[match(gb$GEOID20, ip$GEOID20)]
  u <- aggregate(gb["GEOID20"], by = list(unit = grp), FUN = function(z) z[1])
  u <- u[order(u$unit), ]
  # Blocks carry river and interstate slivers, so a dissolved unit always comes
  # back as a few dozen polygons.  What matters is whether one of them holds
  # essentially the whole unit.
  bits <- lapply(st_geometry(u), function(z) {
    ar <- as.numeric(st_area(st_cast(st_sfc(z, crs = 5070), "POLYGON")))
    sort(ar / sum(ar), decreasing = TRUE)
  })
  npoly    <- vapply(bits, function(z) sum(z >= 0.01), 1L)   # pieces >= 1% of area
  biggest  <- vapply(bits, function(z) 100 * z[1], 0)
  S <- summ(grp)
  # summ() sorts units by Black share; put the geometry in the same order
  pp <- as.numeric(tapply(ip$pop, grp, sum)); bb <- as.numeric(tapply(ip$black, grp, sum))
  o  <- order(-bb / pp)
  u <- u[o, ]; npoly <- npoly[o]; biggest <- biggest[o]
  g <- rings(thin(st_geometry(u), 60), "County")
  g$lev <- tag; names(g)[names(g) == "uid"] <- "unit"
  list(geo   = g[, c("lev", "unit", "part", "x", "y")],
       units = data.frame(plan = tag, family = fam, angle = theta,
                          unit = seq_len(K), pop = S$pop, black = S$black,
                          share = round(S$share, 2), bvap = round(S$bvap, 2),
                          pieces = npoly, pct_largest = round(biggest, 2),
                          fill = fillfor(S$share)))
}
G <- lapply(names(PL), function(nmz)
  plan_geo(PL[[nmz]]$family, PL[[nmz]]$angle, nmz))
pg <- do.call(rbind, lapply(G, `[[`, "geo"))
pu <- do.call(rbind, lapply(G, `[[`, "units"))
names(pg)[names(pg) == "lev"] <- "plan"
dd_write_csv(pg, "derived/zoning_plan_geo.csv")
dd_write_csv(pu, "derived/zoning_plan_units.csv")
cat("\n---- the two headline plans ----\n")
print(pu[, setdiff(names(pu), "fill")], row.names = FALSE)

# ---- dot density: one dot = DOT people, at block interior points -----------
DOT <- 400
mkdots <- function(count, tag) {
  n <- floor(count / DOT + runif(length(count)))
  i <- rep(seq_along(n), n)
  data.frame(kind = tag,
             x = round(ip$x[i] + rnorm(length(i), 0, 0.16), 2),
             y = round(ip$y[i] + rnorm(length(i), 0, 0.16), 2))
}
dots <- rbind(mkdots(ip$black, "Black"), mkdots(ip$pop - ip$black, "not Black"))
dots <- dots[sample(nrow(dots)), ]
dd_write_csv(dots, "derived/dots_fulton.csv")
cat(sprintf("\ndots: %s Black, %s not Black (1 dot = %s people)\n",
            format(sum(dots$kind == "Black"), big.mark = ","),
            format(sum(dots$kind != "Black"), big.mark = ","),
            format(DOT, big.mark = ",")))

# =========================================================================
# 6.  Facts.  Every number quoted in the three .Rmd documents comes from here
#     or from one of the tables above.  Nothing is typed by hand.
# =========================================================================
LS <- function(l, cc) ls_state[[cc]][ls_state$level == l]
LF <- function(l, cc) ls_ful[[cc]][ls_ful$level == l]
PLV <- function(tag, cc) pu[[cc]][pu$plan == tag]
FCT <- function(nm, v, note) data.frame(name = nm, value = v, note = note)

facts <- rbind(
  FCT("ga_blocks",     nrow(b),       "Georgia census blocks, 2020"),
  FCT("ga_pop",        sum(b$pop),    "Georgia 2020 census population"),
  FCT("ga_black_any",  sum(b$black_any), "Georgians of any part Black"),
  FCT("ga_black_pct",  round(100 * sum(b$black_any) / sum(b$pop), 2),
      "any part Black, share of Georgia (%)"),
  FCT("ga_black_alone", sum(b$black_alone), "Black alone"),
  FCT("ga_black_alone_pct", round(100 * sum(b$black_alone) / sum(b$pop), 2),
      "Black alone, share of Georgia (%)"),
  FCT("ga_bgs",        length(unique(b$bg)),    "Georgia block groups"),
  FCT("ga_tracts",     length(unique(b$tract)), "Georgia census tracts"),
  FCT("ga_counties",   length(unique(b$county)),"Georgia counties"),
  FCT("ful_blocks",    nrow(f),                 "Fulton census blocks"),
  FCT("ful_blocks_pop", sum(f$pop > 0),         "Fulton blocks with anyone in them"),
  FCT("ful_bgs",       length(unique(f$bg)),    "Fulton block groups"),
  FCT("ful_tracts",    length(unique(f$tract)), "Fulton census tracts"),
  FCT("ful_pop",       sum(f$pop),              "Fulton 2020 population"),
  FCT("ful_black",     sum(f$black_any),        "Fulton any part Black"),
  FCT("ful_black_pct", round(100 * sum(f$black_any) / sum(f$pop), 2),
      "Fulton any part Black, share (%)"),
  FCT("ful_bvap_pct",  round(100 * sum(f$black_any_vap) / sum(f$vap), 2),
      "Fulton any part Black share of voting-age population (%)"),
  FCT("K",             K,             "units in every partition"),
  FCT("quota",         round(QUOTA),  "people per unit"),
  FCT("n_plans",       nrow(sm),      "equal-population partitions built"),
  FCT("n_angles",      length(TH),    "rotations per family"),
  FCT("max_dev_pct",   round(max(sm$max_dev_pct), 2),
      "largest population deviation from the quota, any unit of any plan (%)"),
  FCT("maj_min",       min(sm$n_majority), "fewest majority-Black units, any plan"),
  FCT("maj_max",       max(sm$n_majority), "most majority-Black units, any plan"),
  FCT("plans_zero_maj", sum(sm$n_majority == 0), "plans with no majority-Black unit"),
  FCT("plans_two_maj", sum(sm$n_majority == 2), "plans with exactly two majority-Black units"),
  FCT("plans_strict_contig", sum(sm$units_contig == K),
      "plans whose six units are each ONE connected piece (strict)"),
  FCT("contig_thr",    CONTIG_THR,
      "effective-contiguity threshold: % of a unit's people in its largest piece"),
  FCT("plans_contig",  nrow(cn), "effectively contiguous plans"),
  FCT("contig_maj_min", min(cn$n_majority), "fewest majority-Black units among effectively contiguous plans"),
  FCT("contig_maj_max", max(cn$n_majority), "most majority-Black units among effectively contiguous plans"),
  FCT("lo_contig",     PL$fewest$units_contig, "strictly contiguous units in the fewest-majority plan (of 6)"),
  FCT("hi_contig",     PL$most$units_contig,   "strictly contiguous units in the most-majority plan (of 6)"),
  FCT("lo_piece",      PL$fewest$largest_piece_pct,
      "fewest-majority plan: worst unit's largest piece, % of its people"),
  FCT("hi_piece",      PL$most$largest_piece_pct,
      "most-majority plan: worst unit's largest piece, % of its people"),
  FCT("clo_family",    PL$fewest_contig$family, "rule family, fewest-majority effectively contiguous plan"),
  FCT("clo_angle",     PL$fewest_contig$angle,  "rotation, fewest-majority effectively contiguous plan (deg)"),
  FCT("clo_maj",       PL$fewest_contig$n_majority, "majority-Black units, that plan"),
  FCT("clo_min",       PL$fewest_contig$min_share, "lowest unit Black share, that plan (%)"),
  FCT("clo_max",       PL$fewest_contig$max_share, "highest unit Black share, that plan (%)"),
  FCT("chi_family",    PL$most_contig$family,  "rule family, most-majority effectively contiguous plan"),
  FCT("chi_angle",     PL$most_contig$angle,   "rotation, most-majority effectively contiguous plan (deg)"),
  FCT("chi_maj",       PL$most_contig$n_majority, "majority-Black units, that plan"),
  FCT("chi_min",       PL$most_contig$min_share, "lowest unit Black share, that plan (%)"),
  FCT("chi_max",       PL$most_contig$max_share, "highest unit Black share, that plan (%)"),
  FCT("spread_min",    min(sm$spread), "smallest gap between a plan's most- and least-Black unit (points)"),
  FCT("spread_max",    max(sm$spread), "largest gap between a plan's most- and least-Black unit (points)"),
  FCT("bvap_maj_min",  min(sm$n_majority_bvap), "fewest majority-BVAP units, any plan"),
  FCT("bvap_maj_max",  max(sm$n_majority_bvap), "most majority-BVAP units, any plan"),
  FCT("lo_family",     PL$fewest$family,  "rule family of the fewest-majority plan"),
  FCT("lo_angle",      PL$fewest$angle,   "rotation of the fewest-majority plan (degrees)"),
  FCT("lo_maj",        PL$fewest$n_majority, "majority-Black units, fewest-majority plan"),
  FCT("lo_min",        PL$fewest$min_share,  "lowest unit Black share, fewest-majority plan (%)"),
  FCT("lo_max",        PL$fewest$max_share,  "highest unit Black share, fewest-majority plan (%)"),
  FCT("lo_pieces",     max(PLV("fewest", "pieces")), "most pieces >=1% of area in any unit of that plan"),
  FCT("lo_largest",    round(min(PLV("fewest", "pct_largest")), 1), "smallest 'largest piece' share of unit area, that plan (%)"),
  FCT("hi_family",     PL$most$family,    "rule family of the most-majority plan"),
  FCT("hi_angle",      PL$most$angle,     "rotation of the most-majority plan (degrees)"),
  FCT("hi_maj",        PL$most$n_majority, "majority-Black units, most-majority plan"),
  FCT("hi_min",        PL$most$min_share, "lowest unit Black share, most-majority plan (%)"),
  FCT("hi_max",        PL$most$max_share, "highest unit Black share, most-majority plan (%)"),
  FCT("hi_pieces",     max(PLV("most", "pieces")), "most pieces >=1% of area in any unit of that plan"),
  FCT("hi_largest",    round(min(PLV("most", "pct_largest")), 1), "smallest 'largest piece' share of unit area, that plan (%)"),
  FCT("sd_block",      round(LF("Census block", "sd_share"), 1),
      "Fulton, weighted SD of unit Black share, blocks (points)"),
  FCT("sd_tract",      round(LF("Census tract", "sd_share"), 1),
      "Fulton, weighted SD of unit Black share, tracts (points)"),
  FCT("ga_sd_block",   round(LS("Census block", "sd_share"), 1),
      "Georgia, weighted SD of unit Black share, blocks (points)"),
  FCT("ga_sd_county",  round(LS("County", "sd_share"), 1),
      "Georgia, weighted SD of unit Black share, counties (points)"),
  FCT("ga_maj_block",  round(LS("Census block", "pct_black_in_majority"), 1),
      "% of Black Georgians in a majority-Black block"),
  FCT("ga_maj_tract",  round(LS("Census tract", "pct_black_in_majority"), 1),
      "% of Black Georgians in a majority-Black tract"),
  FCT("ga_maj_county", round(LS("County", "pct_black_in_majority"), 1),
      "% of Black Georgians in a majority-Black county"),
  FCT("ga_maj_counties", LS("County", "units_majority"), "majority-Black Georgia counties"),
  FCT("ful_maj_block", round(LF("Census block", "pct_black_in_majority"), 1),
      "% of Black Fulton residents in a majority-Black block"),
  FCT("ful_maj_tract", round(LF("Census tract", "pct_black_in_majority"), 1),
      "% of Black Fulton residents in a majority-Black tract"),
  FCT("ga_corr_block", round(LS("Census block",  "corr_child"), 3),
      "Georgia, weighted corr(Black share, under-18 share), blocks"),
  FCT("ga_corr_tract", round(LS("Census tract",  "corr_child"), 3),
      "Georgia, weighted corr(Black share, under-18 share), tracts"),
  FCT("ga_corr_county", round(LS("County", "corr_child"), 3),
      "Georgia, weighted corr(Black share, under-18 share), counties"),
  FCT("dot_people",    DOT,      "people per dot in the dot map"),
  FCT("dens_bw",       BW,       "bandwidth of every density curve (percentage points)"),
  FCT("fetch_date",    20260810, "source fetch date, yyyymmdd"),
  # The P.L. 94-171 zip is a 34 MB download and is not committed, so its size is
  # recorded HERE, once, rather than measured by the chapter at render time --
  # a chapter that stats a file it does not ship cannot be read from a clone.
  FCT("pl_zip_mb",
      if (file.exists("raw/ga2020.pl.zip"))
        round(file.size("raw/ga2020.pl.zip") / 1e6, 1) else NA,
      "size of the Georgia P.L. 94-171 download, MB"))
dd_write_csv(facts, "derived/facts.csv")

cat("\n================ headline ================\n")
cat(sprintf("%s: %s people, %s any part Black (%.1f%%), %s of voting age population.\n",
            CTYNM, format(sum(f$pop), big.mark = ","),
            format(sum(f$black_any), big.mark = ","),
            100 * sum(f$black_any) / sum(f$pop),
            pcT(100 * sum(f$black_any_vap) / sum(f$vap))))
cat(sprintf("Same %s blocks, %d units, %s people each (worst deviation %.2f%%).\n",
            format(nrow(f), big.mark = ","), K, format(round(QUOTA), big.mark = ","),
            max(sm$max_dev_pct)))
cat(sprintf("  %s rule at %d degrees -> %d majority-Black units (%.1f%% to %.1f%%)\n",
            PL$fewest$family, PL$fewest$angle, PL$fewest$n_majority,
            PL$fewest$min_share, PL$fewest$max_share))
cat(sprintf("  %s rule at %d degrees -> %d majority-Black units (%.1f%% to %.1f%%)\n",
            PL$most$family, PL$most$angle, PL$most$n_majority,
            PL$most$min_share, PL$most$max_share))
cat(sprintf("Restricted to the %d effectively contiguous plans: %d to %d majority-Black units.\n",
            nrow(cn), min(cn$n_majority), max(cn$n_majority)))
cat("\nfiles written:\n")
for (ff in c("derived/facts.csv","derived/ladder_state.csv","derived/ladder_fulton.csv","derived/dens_fulton.csv",
             "derived/map_units.csv","derived/map_vals.csv","derived/county_outline.csv","derived/dots_fulton.csv",
             "derived/zoning_sweep.csv","derived/zoning_summary.csv","derived/zoning_plan_geo.csv",
             "derived/zoning_plan_units.csv"))
  cat(sprintf("  %-24s %s\n", ff,
              format(structure(file.size(ff), class = "object_size"), units = "auto")))

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
