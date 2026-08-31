# ---------------------------------------------------------------------------
# Build the overplotting dataset: every populated census block in Georgia,
# prepared three ways.
#
# Six files end up in derived/:
#
#   derived/grid.csv     the scatter, quantised to the pixel grid it is drawn
#                        on -- lossless at the size the figure is printed
#   derived/hex.csv      the same points hex-binned, at three bin sizes
#   derived/contour.csv  contour lines of a 2-D kernel density of the same
#   derived/marginal.csv the Black-share axis on its own, in 2-point bins
#   derived/counts.csv   how many points, pixels, hexes and blocks there are
#   derived/facts.csv    single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# The 2020 Census P.L. 94-171 redistricting file for Georgia, block level,
# already extracted and committed by the areal-units chapter as
#   ../../areal-units/data/derived/ga_block_race.csv
# That chapter documents the extraction; this one re-reads its output because
# the blocks are the same blocks and there is no reason to parse the pipe-
# delimited original twice.
#
# `black_any` is the any-part Black count, not Black-alone. The two differ, the
# difference matters legally, and the areal-units and sweet-spot chapters are
# where that argument lives. This chapter uses any-part throughout and says so.

SRC <- "../../areal-units/data/derived/ga_block_race.csv"
stopifnot(file.exists(SRC))
b <- read.csv(SRC, stringsAsFactors = FALSE, colClasses = c(GEOID20 = "character"))

NBLOCK <- nrow(b)
NEMPTY <- sum(b$pop == 0)

# --- The two axes -----------------------------------------------------------
#
# A BLOCK WITH NO PEOPLE HAS NO RACIAL COMPOSITION. 67,000-odd Georgia blocks
# are unpopulated -- they are lakes, interchanges, industrial parcels, the
# middle of a runway -- and a share of zero people is not zero per cent, it is
# undefined. They are dropped here, which is the only defensible thing to do
# and is still a decision that removes 29% of the rows before any figure is
# drawn. The count is written out so the brief has to account for it.

d <- b[b$pop > 0, ]
d$share <- 100 * d$black_any / d$pop
d$lpop  <- log10(d$pop)
NPOINT <- nrow(d)

XLO <- 0; XHI <- ceiling(max(d$lpop) * 10) / 10      # log10 population
YLO <- 0; YHI <- 100                                  # per cent

# --- 1. The scatter, quantised ----------------------------------------------
#
# 165,000 points cannot travel to a browser as 165,000 points, and they do not
# need to: at the size this figure is printed, most of them land on a pixel
# another point already occupies. Quantising to the drawing grid is therefore
# LOSSLESS FOR THE PICTURE -- the same dots in the same places -- while being
# an order of magnitude smaller. The count in each cell is kept, because the
# number of points hidden under a dot is the subject of the chapter.

GW <- 700; GH <- 450
gx <- pmin(GW, pmax(1, ceiling((d$lpop  - XLO) / (XHI - XLO) * GW)))
gy <- pmin(GH, pmax(1, ceiling((d$share - YLO) / (YHI - YLO) * GH)))
key <- gx + (gy - 1) * GW
tb  <- table(key)
grid <- data.frame(
  gx = as.integer(((as.integer(names(tb)) - 1) %% GW) + 1),
  gy = as.integer(((as.integer(names(tb)) - 1) %/% GW) + 1),
  n  = as.integer(tb), stringsAsFactors = FALSE)
grid <- grid[order(-grid$n), ]
write.csv(grid, "derived/grid.csv", row.names = FALSE)

NPIX   <- nrow(grid)
NHIDE  <- NPOINT - NPIX
MAXPIX <- max(grid$n)

# --- 2. Hex bins ------------------------------------------------------------
#
# d3 does not ship a hexbin: d3-hexbin is a separate module, and this book does
# not load a second library to draw one shape. The binning is done here, in
# thirty lines, and the browser is handed cells.
#
# A pointy-top hex grid. Rows are spaced by 1.5r vertically and by sqrt(3)r
# horizontally, with alternate rows offset by half a step. A point is assigned
# to the nearer of the two candidate centres, which is what makes the cells
# hexagons rather than rectangles.

hexbin <- function(x, y, r) {
  dy <- r * 1.5
  dx <- r * sqrt(3)
  py <- y / dy
  pj <- round(py)
  px <- x / dx - (pj %% 2) / 2
  pi_ <- round(px)
  # the two candidate centres: this row and the row above/below
  c1x <- (pi_ + (pj %% 2) / 2) * dx; c1y <- pj * dy
  pj2 <- pj + ifelse(py > pj, 1, -1)
  pi2 <- round(x / dx - (pj2 %% 2) / 2)
  c2x <- (pi2 + (pj2 %% 2) / 2) * dx; c2y <- pj2 * dy
  d1 <- (x - c1x)^2 + (y - c1y)^2
  d2 <- (x - c2x)^2 + (y - c2y)^2
  use2 <- d2 < d1
  cx <- ifelse(use2, c2x, c1x); cy <- ifelse(use2, c2y, c1y)
  k  <- paste(round(cx, 6), round(cy, 6))
  agg <- table(k)
  parts <- do.call(rbind, strsplit(names(agg), " ", fixed = TRUE))
  data.frame(cx = as.numeric(parts[, 1]), cy = as.numeric(parts[, 2]),
             n = as.integer(agg), stringsAsFactors = FALSE)
}

# radii in DATA units, chosen so the three read as coarse / middling / fine
RADII <- c(6, 3.2, 1.7)
hx <- do.call(rbind, lapply(RADII, function(r) {
  h <- hexbin(d$lpop / (XHI - XLO) * 100, d$share, r)   # x rescaled to 0-100
  h$r <- r
  h
}))
hx$cx <- round(hx$cx, 4); hx$cy <- round(hx$cy, 4)
write.csv(hx[, c("r", "cx", "cy", "n")], "derived/hex.csv", row.names = FALSE)

# --- 3. A density, and its contours -----------------------------------------
#
# The third way of showing the same points. Computed on a coarse grid with a
# fixed bandwidth -- fixed, because a bandwidth chosen per figure is the
# bin-width problem again in a form the reader cannot see.

kd <- MASS::kde2d(d$lpop / (XHI - XLO) * 100, d$share, n = 80,
                  lims = c(0, 100, 0, 100), h = c(9, 12))
lv <- pretty(range(kd$z), 9)
lv <- lv[lv > 0]
cl <- contourLines(kd$x, kd$y, kd$z, levels = lv)
ct <- do.call(rbind, lapply(seq_along(cl), function(i) {
  data.frame(id = i, level = round(cl[[i]]$level, 8),
             x = round(cl[[i]]$x, 3), y = round(cl[[i]]$y, 3),
             stringsAsFactors = FALSE)
}))
write.csv(ct, "derived/contour.csv", row.names = FALSE)

# --- 4. The marginal ---------------------------------------------------------

mb <- seq(0, 100, 2)
mh <- hist(d$share, breaks = mb, plot = FALSE)
marg <- data.frame(lo = head(mb, -1), hi = tail(mb, -1),
                   n = as.integer(mh$counts), stringsAsFactors = FALSE)
write.csv(marg, "derived/marginal.csv", row.names = FALSE)

# --- 5. Counts and facts -----------------------------------------------------

cnt <- data.frame(
  what = c("blocks in the file", "with nobody living in them",
           "populated, and drawn", "pixels those points occupy",
           "points hidden under another point",
           "most points on a single pixel"),
  value = c(NBLOCK, NEMPTY, NPOINT, NPIX, NHIDE, MAXPIX),
  stringsAsFactors = FALSE)
write.csv(cnt, "derived/counts.csv", row.names = FALSE)

# --- The axis is not continuous ---------------------------------------------
#
# A share is a fraction, and in a block of seven people it is a fraction with
# seven in the denominator. Most of the plane in Figure 1 is therefore not
# merely empty but UNREACHABLE, and the arcs the scatter shows are the loci of
# k/n for small n. This is checked rather than asserted: for blocks of ten or
# fewer, every observed share must be exactly k/n.
small <- d[d$pop <= 10, ]
stopifnot(all(abs(small$share -
                  100 * round(small$share * small$pop / 100) / small$pop) < 1e-9))
NLE10  <- sum(d$pop <= 10)
NLEMED <- sum(d$pop <= median(d$pop))
SH15   <- length(unique(round(d$share[d$pop <= 5], 6)))
N15    <- sum(d$pop <= 5)
PEX0   <- 100 * mean(d$share == 0)
PEX100 <- 100 * mean(d$share == 100)

STATE <- 100 * sum(b$black_any) / sum(b$pop)
facts <- data.frame(
  key = c("blocks", "empty", "pct_empty", "points", "pixels", "hidden",
          "pct_hidden", "maxpix", "state_share", "med_pop", "mean_pop",
          "lo5", "pct_lo5", "hi95", "pct_hi95", "mid", "pct_mid",
          "xhi", "hex_coarse", "hex_fine", "n_hex_coarse", "n_hex_fine",
          "n_le10", "pct_le10", "n_lemed", "pct_lemed", "n_le5", "shares_le5",
          "pct_exact0", "pct_exact100"),
  value = c(NBLOCK, NEMPTY, round(100 * NEMPTY / NBLOCK, 1), NPOINT, NPIX,
            NHIDE, round(100 * NHIDE / NPOINT, 1), MAXPIX,
            round(STATE, 1), median(d$pop), round(mean(d$pop), 1),
            sum(d$share < 5), round(100 * mean(d$share < 5), 1),
            sum(d$share > 95), round(100 * mean(d$share > 95), 1),
            sum(d$share >= 40 & d$share <= 60),
            round(100 * mean(d$share >= 40 & d$share <= 60), 1),
            XHI, max(RADII), min(RADII),
            sum(hx$r == max(RADII)), sum(hx$r == min(RADII)),
            NLE10, round(100 * NLE10 / NPOINT, 1),
            NLEMED, round(100 * NLEMED / NPOINT, 1),
            N15, SH15, round(PEX0, 1), round(PEX100, 1)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("grid.csv     ->", NPIX, "occupied pixels from", NPOINT, "points\n")
cat("hex.csv      ->", nrow(hx), "cells across", length(RADII), "bin sizes\n")
cat("contour.csv  ->", length(cl), "contour rings\n")
cat("marginal.csv ->", nrow(marg), "bins\n")
cat("\n", NHIDE, " of ", NPOINT, " points (",
    round(100 * NHIDE / NPOINT, 1), "%) land where another already is\n", sep = "")
cat("most points on one pixel:", MAXPIX, "\n")
cat("done.\n")

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
