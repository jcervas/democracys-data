# ---------------------------------------------------------------------------
# The 2016 -> 2024 pair: an eight-year span, built by JOINING THE TWO PAIRS
# THAT ARE ALREADY ON DISK.  Nothing is fetched.
#
#   derived/wind_us_1620.csv  carries votes_dem_16 / votes_gop_16  (built by build-1620.R)
#   derived/wind_us.csv       carries votes_dem_24 / votes_gop_24  (built by build-data.R)
#
# Both were assembled from the same repository with the same harmonisation, so
# the eight-year margin is a subtraction, not a new measurement.  Asking the
# servers again for votes this repository already holds could only introduce a
# difference; joining what is here cannot.
#
# WHAT THE JOIN COSTS.  It is its own vintage problem, and the chapter says so:
# the 2016 and 2020 files use Connecticut's eight historic county codes and the
# 2024 file uses the nine planning regions that replaced them, so Connecticut
# cannot appear on an eight-year map at all.  It appears on the 2016-to-2020
# map, because both ends of THAT pair predate the change.  A data problem
# belongs to a pair of vintages, not to a dataset.
#
# Run from inside data/, after build-data.R and build-1620.R:
#   Rscript build-1624.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)
source("encoding.R")

say <- function(...) cat(..., "\n", sep = "")

A <- read.csv("derived/wind_us_1620.csv", colClasses = c(county_fips = "character"))
B <- read.csv("derived/wind_us.csv",      colClasses = c(county_fips = "character"))

say("[1] join")
keepA <- c("county_fips", "county_name", "lon", "lat", "x", "y", "pop",
           "votes_dem_16", "votes_gop_16", "total_votes_16", "swing")
keepB <- c("county_fips", "state_name", "votes_dem_24", "votes_gop_24",
           "total_votes_24", "swing", "in_frame")
w <- merge(A[, keepA], B[, keepB], by = "county_fips",
           suffixes = c("_1620", "_2024"))

onlyA <- setdiff(A$county_fips, B$county_fips)
onlyB <- setdiff(B$county_fips, A$county_fips)
say("  joined ", nrow(w), "; only in 2016-20: ", length(onlyA),
    "; only in 2024: ", length(onlyB))

w <- w[w$in_frame %in% c(TRUE, "TRUE"), ]
names(w)[names(w) == "swing_1620"] <- "swing_a"    # 2016 -> 2020
names(w)[names(w) == "swing_2024"] <- "swing_b"    # 2020 -> 2024

say("[2] margins")
w$margin_16 <- 100 * (w$votes_gop_16 - w$votes_dem_16) /
                     (w$votes_gop_16 + w$votes_dem_16)
w$margin_24 <- 100 * (w$votes_gop_24 - w$votes_dem_24) /
                     (w$votes_gop_24 + w$votes_dem_24)
w$swing     <- w$margin_24 - w$margin_16
w$in_frame  <- TRUE

vt   <- w$votes_dem_24 + w$votes_gop_24
w$lw <- sqrt(vt) / sqrt(max(vt))
w    <- cbind(w, wind_geom(w$swing, vt, "us"))

w <- w[, c("county_fips", "state_name", "county_name", "lon", "lat", "x", "y",
           "pop", "votes_dem_16", "votes_gop_16", "total_votes_16",
           "votes_dem_24", "votes_gop_24", "total_votes_24",
           "margin_16", "margin_24", "swing", "swing_a", "swing_b",
           "angle_deg", "len_km", "dx", "dy", "lw", "in_frame",
           "capped_len", "capped_angle")]
dd_write_csv(w, "derived/wind_us_1624.csv")
say("  wrote wind_us_1624.csv, ", nrow(w), " rows")

# ---- does an eight-year span cancel or accumulate? ------------------------
# The question the third map exists to answer. If the two four-year swings were
# mean-reverting -- places that lurched one way lurching back -- they would be
# NEGATIVELY correlated, and the eight-year swing would be LESS dispersed than
# two independent four-year swings laid end to end. Both quantities are
# computed rather than asserted, because the answer is not obvious in advance
# and it is the opposite of the intuitive one.
say("[3] cancel or accumulate")
# UNWEIGHTED AND VOTE-WEIGHTED, both, because they are answers to different
# questions and only one of them is about voters. An unweighted county
# correlation gives Loving County, Texas -- 96 two-party votes -- the same say
# as Los Angeles County's 3.6 million. Grofman and Cervas (2024) catalogue that
# as a distinct arithmetic fallacy in election claims: "failing to weight
# units". Reporting only the unweighted figure here would commit it.
wm   <- function(x, wt) sum(wt * x) / sum(wt)
wcov <- function(x, y, wt) {
  mx <- wm(x, wt); my <- wm(y, wt); sum(wt * (x - mx) * (y - my)) / sum(wt)
}
wcor <- function(x, y, wt) wcov(x, y, wt) / sqrt(wcov(x, x, wt) * wcov(y, y, wt))

r       <- cor(w$swing_a, w$swing_b)
rw      <- wcor(w$swing_a, w$swing_b, vt)
sd_obs  <- sd(w$swing)
sd_ind  <- sqrt(var(w$swing_a) + var(w$swing_b))   # if the halves were unrelated
sd_obsw <- sqrt(wcov(w$swing, w$swing, vt))
sd_indw <- sqrt(wcov(w$swing_a, w$swing_a, vt) + wcov(w$swing_b, w$swing_b, vt))
fit     <- lm(swing_b ~ swing_a, data = w)
fitw    <- lm(swing_b ~ swing_a, data = w, weights = vt)
# How top-heavy the electorate is: the count of counties holding half the vote.
nhalf   <- which(cumsum(sort(vt, decreasing = TRUE)) >= sum(vt) / 2)[1]
agg <- function(dm, gp) 100 * (sum(gp) - sum(dm)) / (sum(gp) + sum(dm))

FT <- list(
  us1624_arrows        = nrow(w),
  us1624_share_R       = 100 * mean(w$swing > 0),
  us1624_vshare_R      = 100 * sum(vt[w$swing > 0]) / sum(vt),
  us1624_swing_median  = median(w$swing),
  us1624_swing_min     = min(w$swing),
  us1624_swing_max     = max(w$swing),
  us1624_swing_sd      = sd_obs,
  us1624_sd_if_indep   = sd_ind,
  us1624_sd_a          = sd(w$swing_a),
  us1624_sd_b          = sd(w$swing_b),
  us1624_halves_cor    = r,
  us1624_halves_cor_w  = rw,
  us1624_slope         = unname(coef(fit)[2]),
  us1624_slope_w       = unname(coef(fitw)[2]),
  us1624_int           = unname(coef(fit)[1]),
  us1624_int_w         = unname(coef(fitw)[1]),
  us1624_swing_sd_w    = sd_obsw,
  us1624_sd_if_indep_w = sd_indw,
  us1624_half_vote_n   = nhalf,
  # The scatter in the brief is drawn on a square [-20, 30] frame so that its
  # tilt is a real angle; this is how many counties that frame leaves out.
  us1624_off_frame     = sum(w$swing_a < -20 | w$swing_a > 30 |
                             w$swing_b < -20 | w$swing_b > 30),
  us1624_weight_ratio  = max(vt) / min(vt),
  us1624_same_dir      = 100 * mean(sign(w$swing_a) == sign(w$swing_b)),
  us1624_D_then_R      = 100 * mean(w$swing_a <= 0 & w$swing_b > 0),
  us1624_R_then_D      = 100 * mean(w$swing_a > 0 & w$swing_b <= 0),
  us1624_R_both        = 100 * mean(w$swing_a > 0 & w$swing_b > 0),
  us1624_D_both        = 100 * mean(w$swing_a <= 0 & w$swing_b <= 0),
  us1624_margin_16     = agg(w$votes_dem_16, w$votes_gop_16),
  us1624_margin_24     = agg(w$votes_dem_24, w$votes_gop_24),
  us1624_ct_lost       = length(onlyA),
  us1624_capped        = sum(w$capped_len),
  us1624_top_county    = w$county_name[which.max(w$swing)],
  us1624_top_state     = w$state_name[which.max(w$swing)],
  us1624_top_swing     = max(w$swing),
  us1624_top_m16       = w$margin_16[which.max(w$swing)],
  us1624_top_m24       = w$margin_24[which.max(w$swing)],
  us1624_bot_county    = w$county_name[which.min(w$swing)],
  us1624_bot_state     = w$state_name[which.min(w$swing)],
  us1624_bot_swing     = min(w$swing))
FT$us1624_agg_swing <- FT$us1624_margin_24 - FT$us1624_margin_16

for (k in names(FT)) say("  ", k, " = ", FT[[k]])

say("[4] facts")
ff <- read.csv("derived/facts.csv", colClasses = "character")
for (k in names(FT)) {
  # rounded here: numbers turn into strings at this line
  v <- as.character(dd_num(FT[[k]]))
  if (k %in% ff$key) ff$value[ff$key == k] <- v
  else ff <- rbind(ff, data.frame(key = k, value = v))
}
dd_write_csv(ff, "derived/facts.csv")
say("  ", nrow(ff), " facts")
say("done.")
