# ---------------------------------------------------------------------------
# Build the distributions lab dataset.
#
# Three files end up in derived/:
#
#   derived/districts.csv  one row per House race, 1946-2024: the Democratic
#                          share of the two-party vote, and whether the race
#                          was uncontested
#   derived/by_year.csv    one row per election year: the summaries this
#                          chapter is about, computed the same way every time
#   derived/facts.csv      single numbers the brief quotes, so the prose and
#                          the figures cannot drift apart
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

# raw/ holds sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# The House competition chapter in this same corpus already assembles every
# House race from two sources and writes the result to
#
#   ../../house-competition/data/derived/races.csv
#
# which is Gary C. Jacobson's dataset for 1946-2014 and the Clerk of the
# House's own returns for 2016-2024. This chapter reads that file rather than
# re-deriving it: the same numbers should not be built twice in one book, and
# the sibling script documents its own repairs.
#
# ONE PROPERTY OF THAT FILE DECIDES THIS WHOLE CHAPTER.
#
# `dv` -- the Democratic share of the two-party vote -- is MISSING when a race
# was uncontested. That is not a defect. With one candidate on the ballot there
# is no two-party share to record. But it means the variable this chapter draws
# cannot represent the least competitive races in the country, and every
# summary below is computed on a set that has already had its tail removed.
# The brief says so out loud; the counts are written here so it can.

SRC <- "../../house-competition/data/derived/races.csv"
stopifnot(file.exists(SRC))
r <- read.csv(SRC, stringsAsFactors = FALSE)

# `stcd` is a state-district code; keep it as text so nothing is lost, and
# carry the year, the share and the uncontested flag. Nothing else is used.
d <- data.frame(
  year        = as.integer(r$year),
  stcd        = as.character(r$stcd),
  dv          = as.numeric(r$dv),
  uncontested = as.logical(r$uncontested),
  source      = r$source,
  stringsAsFactors = FALSE)
d <- d[!is.na(d$year), ]
d <- d[order(d$year, d$stcd), ]

# Every row with a share must be a contested race, and every uncontested race
# must be missing its share. If that ever stops being true the chapter's whole
# argument about the excluded tail is wrong, so it is checked rather than said.
stopifnot(all(is.na(d$dv[which(d$uncontested)])))
stopifnot(!any(is.na(d$dv[which(!d$uncontested)])))

write.csv(d, "derived/districts.csv", row.names = FALSE)
cat("districts.csv ->", nrow(d), "races,",
    length(unique(d$year)), "elections\n")

# --- Per-year summaries -----------------------------------------------------
#
# The point of the chapter is that these summaries agree with each other and
# still mislead, so they are computed once, here, and never recomputed in the
# brief. `within5` and `within10` are shares of CONTESTED races.

peak_of <- function(v, lo, hi) {
  # the mode of a smoothed density, restricted to one side of the midpoint
  if (length(v) < 8) return(NA_real_)
  z <- v[v >= lo & v < hi]
  if (length(z) < 8) return(NA_real_)
  k <- density(z)
  round(k$x[which.max(k$y)], 2)
}

yrs <- sort(unique(d$year))
by <- do.call(rbind, lapply(yrs, function(y) {
  z <- d[d$year == y, ]
  v <- z$dv[!is.na(z$dv)]
  data.frame(
    year        = y,
    seats       = nrow(z),
    contested   = length(v),
    uncontested = sum(z$uncontested, na.rm = TRUE),
    mean        = round(mean(v), 2),
    median      = round(median(v), 2),
    sd          = round(sd(v), 2),
    p25         = round(quantile(v, 0.25, names = FALSE), 2),
    p75         = round(quantile(v, 0.75, names = FALSE), 2),
    within5     = round(100 * mean(abs(v - 50) <= 5), 2),
    within10    = round(100 * mean(abs(v - 50) <= 10), 2),
    peak_dem    = peak_of(v, 50, 100),
    peak_rep    = peak_of(v, 0, 50),
    # how many races sit in the five points below the midpoint, against the
    # five points on either side of that -- the dip, as three integers
    band_40_45  = sum(v >= 40 & v < 45),
    band_45_50  = sum(v >= 45 & v < 50),
    band_50_55  = sum(v >= 50 & v < 55),
    stringsAsFactors = FALSE)
}))
write.csv(by, "derived/by_year.csv", row.names = FALSE)
cat("by_year.csv   ->", nrow(by), "elections\n")

# --- The numbers the prose quotes -------------------------------------------

LAST <- max(yrs)
b <- by[by$year == LAST, ]
v <- d$dv[d$year == LAST & !is.na(d$dv)]

facts <- data.frame(
  key = c("last_year", "seats", "contested", "uncontested",
          "mean", "median", "sd", "p25", "p75",
          "within5", "within10", "peak_rep", "peak_dem",
          "band_40_45", "band_45_50", "band_50_55",
          "in_45_55", "first_year", "elections"),
  value = c(LAST, b$seats, b$contested, b$uncontested,
            b$mean, b$median, b$sd, b$p25, b$p75,
            b$within5, b$within10, b$peak_rep, b$peak_dem,
            b$band_40_45, b$band_45_50, b$band_50_55,
            sum(v >= 45 & v <= 55), min(yrs), length(yrs)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("\n", LAST, ": ", b$seats, " seats, ", b$contested, " contested, ",
    b$uncontested, " uncontested\n", sep = "")
cat("mean ", b$mean, "  median ", b$median,
    "  in the 45-50 band: ", b$band_45_50,
    " (against ", b$band_40_45, " and ", b$band_50_55, " either side)\n", sep = "")
cat("\ndone.\n")

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
