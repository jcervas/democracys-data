# ---------------------------------------------------------------------------
# Re-apply encoding.R to the CSVs that are already built.
#
# WHY THIS EXISTS.  build-data.R and build-1620.R fetch from the Census, from a
# GitHub returns repository and from the Georgia Secretary of State.  Changing
# how an arrow is DRAWN should not require asking three servers for the same
# votes again, and it should not be able to change the votes by accident.  This
# script touches nothing but the geometry columns -- angle_deg, len_km, dx, dy,
# capped_len, capped_angle -- which are pure functions of `swing`, plus the
# encoding keys in facts.csv and the parity check.
#
# Run from inside data/:   Rscript reencode.R
# A full rebuild (Rscript build-data.R && Rscript build-1620.R) produces the
# identical geometry, because both read the same encoding.R.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)
source("encoding.R")

say <- function(...) cat(..., "\n", sep = "")

GEOM <- c("angle_deg", "net_votes", "len_km", "dx", "dy",
          "capped_len", "capped_angle")

# Length is a function of votes now, so each file has to say which pair of vote
# columns is the LATER observation -- that is the electorate the arrow is about.
reencode <- function(file, map, dcol, rcol) {
  d <- read.csv(file, colClasses = "character", check.names = FALSE)
  stopifnot("swing" %in% names(d), all(c(dcol, rcol) %in% names(d)))
  g   <- wind_geom(as.numeric(d$swing),
                   as.numeric(d[[dcol]]) + as.numeric(d[[rcol]]), map)
  old <- as.numeric(d$len_km)
  for (k in GEOM) d[[k]] <- if (is.logical(g[[k]])) toupper(g[[k]]) else g[[k]]
  dd_write_csv(d, file)
  say("  ", file, ": ", nrow(d), " rows, ", map,
      "  median len ", round(median(old), 1), " -> ", round(median(g$len_km), 1),
      " km,  max ", round(max(old), 1), " -> ", round(max(g$len_km), 1), " km")
  invisible(g)
}

say("[1] geometry")
gus  <- reencode("derived/wind_us.csv",      "us",        "votes_dem_24", "votes_gop_24")
g16  <- reencode("derived/wind_us_1620.csv", "us",        "votes_dem_20", "votes_gop_20")
gga  <- reencode("derived/wind_ga.csv",      "ga_county", "dem_24",       "gop_24")

# ---- crowding, measured. The number the length scale is chosen against -----
say("[2] crowding")
nn <- function(P) vapply(seq_len(nrow(P)), function(i) {
  d <- sqrt((P[, 1] - P[i, 1])^2 + (P[, 2] - P[i, 2])^2); d[i] <- Inf; min(d)
}, numeric(1))
crowd <- function(file, g) {
  d <- read.csv(file)
  k <- if ("in_frame" %in% names(d)) d$in_frame %in% c(TRUE, "TRUE") else TRUE
  s <- nn(as.matrix(d[k, c("x", "y")]))
  L <- g$len_km[k]
  c(over1 = 100 * mean(L > s), over3 = 100 * mean(L > 3 * s))
}
cw <- rbind(`national counties` = crowd("derived/wind_us.csv", gus),
            `Georgia counties`  = crowd("derived/wind_ga.csv", gga))
print(round(cw, 1))

# ---- facts.csv: the encoding keys only ------------------------------------
say("[3] facts")
ff  <- read.csv("derived/facts.csv", colClasses = "character")
put <- function(k, v) {
  # rounded here: numbers turn into strings at this line
  v <- as.character(dd_num(v))
  if (k %in% ff$key) ff$value[ff$key == k] <<- v
  else ff <<- rbind(ff, data.frame(key = k, value = v))
}
put("len_min_frac",         LEN_MIN_FRAC)
put("deg_per_point",        DEG_PER_POINT)
put("len_max_km_us",        LEN_MAX_KM[["us"]])
put("net_votes_full_us",    NET_VOTES_FULL[["us"]])
put("net_votes_full_ga_county", NET_VOTES_FULL[["ga_county"]])
put("len_max_km_ga_county", LEN_MAX_KM[["ga_county"]])
# Two keys from the two-encoding era. Removed rather than left at stale values,
# so anything still asking for them fails loudly.
ff <- ff[!ff$key %in% c("len_scale_us","bimodal_deg","len_cap_pts",
                        "km_per_point_us","km_per_point_ga_county",
                        "km_per_point_ga_precinct"), ]
# Crowding, so the brief can quote it instead of asserting it.
put("us_netvotes_max",  round(max(gus$net_votes)))
put("us_netvotes_ratio", round(max(gus$net_votes) / median(gus$net_votes)))
put("us_atmax_pct",      round(100 * mean(gus$capped_len), 1))
put("us_capped_angle_pct", round(100 * mean(abs(gus$angle_deg) >= ANGLE_CAP), 1))
put("ga_capped_angle_pct", round(100 * mean(abs(gga$angle_deg) >= ANGLE_CAP), 1))
put("us_over1_nn", round(cw["national counties", "over1"], 1))
put("us_over3_nn", round(cw["national counties", "over3"], 1))
put("ga_over3_nn", round(cw["Georgia counties",  "over3"], 1))
write.csv(dd_signif(ff), "derived/facts.csv")
say("  ", nrow(ff), " facts")

# ---- parity: dx/dy still equal len * (sin, cos) of angle -------------------
say("[4] parity")
chk <- function(lbl, file) {
  d  <- read.csv(file)
  th <- d$angle_deg * pi / 180
  data.frame(figure = lbl, n = nrow(d),
             max_dx_err  = max(abs(d$dx - d$len_km * sin(th))),
             max_dy_err  = max(abs(d$dy - d$len_km * cos(th))),
             max_len_err = max(abs(sqrt(d$dx^2 + d$dy^2) - d$len_km)))
}
par_tbl <- rbind(chk("national counties", "derived/wind_us.csv"),
                 chk("Georgia counties",  "derived/wind_ga.csv"))
dd_write_csv(par_tbl, "derived/parity.csv")
print(par_tbl)
say("done.")
