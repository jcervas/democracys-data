# ---------------------------------------------------------------------------
# Build the House competition dataset, 1946-2014.
#
#   derived/by_year.csv    One row per election year: races, uncontested, competitive,
#                  landslides, split by region.
#   derived/races.csv      One row per race, trimmed to the columns this lab uses.
#
# SOURCE. Gary C. Jacobson's House elections dataset, 1946-2014, held locally:
#   .../GitHub/Data/Elections/House/_archive/house_jacobson.dta
# Stata format readable by base R's `foreign` (version <= 12).
#
# ---------------------------------------------------------------------------
# TWO THINGS ABOUT THIS FILE THAT WILL RUIN AN ANALYSIS IF MISSED
#
# 1. NEARLY HALF THE ROWS ARE EMPTY. The file has 30,005 rows; 14,777 of them
#    have no year and no data at all -- padding. Dropping rows with a missing
#    year leaves 15,228 real races, which is 435 seats x 35 elections (plus a
#    handful of extras). Anything computed before that drop is nonsense.
#
# 2. `dv` IS MISSING WHEN THE RACE WAS UNCONTESTED. There is no separate
#    uncontested flag. `dv` is the Democratic share of the two-party vote, and
#    with only one candidate there is no two-party share to record. The
#    evidence that this is what missingness means:
#      * of 2,361 races with dv missing, `incwin` = 1 in 2,221 -- the incumbent
#        won, which is what happens when nobody runs against you;
#      * missingness is 38.7% in the South against 7.1% elsewhere, and it falls
#        steadily as the one-party South ends.
#    **This is an inference about a coding convention, not a documented flag**,
#    and the lab says so.
#
# 3. THE COLUMN NAMES ARE TRAPS, AND AN EARLIER VERSION OF THIS SCRIPT FELL IN.
#
#    `dv`    Democratic share of the two-party HOUSE vote, this election
#    `dvp`   Democratic share of the two-party HOUSE vote, the PREVIOUS election
#    `dpres` Democratic share of the two-party PRESIDENTIAL vote in the district
#
#    `dvp` is "d-v-previous", not "d-v-presidential". An earlier build read it
#    as the presidential vote and computed split districts as
#    `(dv > 50) != (dvp > 50)`, which is not a split district at all -- it is a
#    seat that changed party. It then blamed the file's own `split` column for
#    disagreeing.
#
#    The file was right. Using `dpres` instead:
#      * the computed flag agrees with the file's `split` column 100% of the
#        time, across every year;
#      * 2012 gives 25 split districts against the file's 26 -- the one-district
#        gap is an uncontested race, where `dv` is NA and no two-party share
#        exists to compare.
#    Verified independently against The Downballot's presidential-by-district
#    calculations: Jacobson's `dpres` for AL-01 in 2012 is 37.70; theirs is
#    37.70161.
#
#    We therefore use the file's own `split` column, which handles uncontested
#    races (it uses the winner's party, not a two-party share), and keep the
#    computed version alongside it so the two can be compared in class.
#
# 4. `split` IS A SENTINEL WHERE `dpres` IS MISSING, AND THAT MATTERS.
#    Nobody computed presidential results by congressional district for the
#    1940s, so `dpres` is empty for 1946, 1948 and 1950 -- and `split` is 1 for
#    all 435 rows in those years, which would report a 100% split-district rate.
#    Two more years are partial: 1962 and 1966 carry `dpres` for roughly half
#    the House.
#
#    So the split-district series is only defined where `dpres` exists. This
#    script masks it elsewhere and writes a `split_coverage` column giving the
#    share of districts the year's figure actually rests on. Any year below
#    90% coverage should not be plotted as a point on a trend.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

# THE SOURCE IS COMMITTED, in raw/, because it cannot be fetched.
#
# Gary Jacobson's House elections file is not published at a URL that a script
# can call. It circulates between researchers, and the copy this chapter was
# built from lived at an absolute path inside one person's Google Drive:
#
#   My Drive/GitHub/Data/Elections/House/_archive/house_jacobson.dta
#
# Which meant the chapter could only be rebuilt on one laptop. A build that
# depends on a path outside the repository is not reproducible; it just fails
# somewhere else. The file is 1.6 MB -- 30,005 rows, 20 columns, 1946-2014 --
# so it is committed here and read from raw/ like any other source.
#
#   sha256  76d42ee8d02f9469c6d1553384d3439cd2e1b74f...  (first 40 hex)
#
# Set HOUSE_JACOBSON_DTA to point somewhere else if a newer copy turns up.
SRC <- Sys.getenv("HOUSE_JACOBSON_DTA", unset = "raw/house_jacobson.dta")

if (!file.exists(SRC)) stop("Jacobson file not found at:\n  ", SRC)

raw <- foreign::read.dta(SRC)
cat(sprintf("raw rows: %s\n", format(nrow(raw), big.mark = ",")))

d <- raw[!is.na(raw$year), ]
cat(sprintf("after dropping empty rows: %s  (%s dropped)\n",
            format(nrow(d), big.mark = ","),
            format(nrow(raw) - nrow(d), big.mark = ",")))
stopifnot(nrow(d) > 15000, nrow(d) < 16000)

# SEVEN SIGNIFICANT DIGITS, BECAUSE THAT IS ALL A STATA `float` HOLDS.
#
# `dv`, `dvp` and `dpres` are stored in the .dta as Stata floats -- 32 bits,
# about 7.2 decimal digits. R reads them into 64-bit doubles, which faithfully
# preserves the 32-bit value and then prints all of it: Jacobson's 57.8 arrives
# as 57.7999992370605, and his 38.4417 as 38.4416999816895. Written out at full
# width, the file claimed a district's presidential vote share to thirteen
# decimal places -- 12,623 of 15,647 `dpres` values, and about the same share of
# the other two.
#
# None of those digits is a measurement. They are the residue of writing a
# decimal fraction in binary, and reading them as precision is exactly the
# mistake this chapter teaches students not to make.
#
# The cut is at seven SIGNIFICANT digits rather than a fixed number of decimal
# places, because the three columns do not share a precision: `dpres` is given
# to two decimals throughout, `dv` runs to four in the 2002 rows, and `dvp` to
# five. Rounding all of them to two would delete real digits from 321 `dv`
# values. Seven significant digits is the width of the storage, so it removes
# the noise and nothing else -- checked: every value here is unchanged when cast
# back to 32-bit, and 10,000-odd per column get shorter.
f32_clean <- function(x) signif(x, 7)
d$dv    <- f32_clean(d$dv)
d$dvp   <- f32_clean(d$dvp)
d$dpres <- f32_clean(d$dpres)

d$uncontested <- is.na(d$dv)

# MARGIN IS ROUNDED TO DECIMALS, NOT TO SIGNIFICANT DIGITS, and the difference
# matters here in a way it does not above. Subtracting two near-equal numbers
# throws away the leading digits and promotes the residue: `dv` carries an
# absolute error around 3e-6, so a margin of 0.03 inherits that same absolute
# error -- a relative error of one part in ten thousand. Significant digits are
# the wrong ruler for a quantity like that, because they would faithfully
# preserve seven digits of a number that is only accurate to five. `dv` is four
# decimals at its finest, so a margin cannot be better than four either.
d$margin      <- round(abs(d$dv - 50), 4)    # NA when uncontested
d$competitive <- !is.na(d$margin) & d$margin <= 5
d$landslide   <- !is.na(d$margin) & d$margin >= 20
# split district: presidential and House winners of different parties.
# The file's own `split` column is authoritative (it can classify uncontested
# races); the computed version is kept for comparison. See note 3 above.
# defined ONLY where the district presidential vote exists -- see note 4
d$split_district <- ifelse(is.na(d$dpres), NA, d$split == 1)
d$split_computed <- ifelse(!is.na(d$dv) & !is.na(d$dpres),
                           (d$dv > 50) != (d$dpres > 50), NA)
agree <- mean(d$split_district == d$split_computed, na.rm = TRUE)
cat(sprintf("split: file column vs computed from dv/dpres agree %.1f%%\n", 100 * agree))
stopifnot(agree > 0.99)

write.csv(d[, c("year", "state", "stcd", "south", "midterm", "dv", "dvp",
                "dpres", "incwin", "uncontested", "margin", "competitive",
                "landslide", "split_district")],
          "derived/races.csv", row.names = FALSE)

pc <- function(x, y) round(100 * as.vector(tapply(x, y, mean, na.rm = TRUE)), 1)
yrs <- sort(unique(d$year))
by <- data.frame(
  year          = yrs,
  races         = as.vector(table(d$year)),
  pct_uncontested = pc(d$uncontested, d$year),
  pct_competitive = round(100 * as.vector(tapply(d$competitive, d$year,
                       function(x) mean(x))), 1),
  pct_landslide   = round(100 * as.vector(tapply(d$landslide, d$year,
                       function(x) mean(x))), 1),
  stringsAsFactors = FALSE)

s <- d[d$south == 1, ]; n <- d[d$south == 0, ]
by$pct_uncontested_south     <- pc(s$uncontested, s$year)
by$pct_uncontested_non_south <- pc(n$uncontested, n$year)
by$pct_split <- round(100 * as.vector(tapply(d$split_district, d$year,
                     function(x) mean(x, na.rm = TRUE))), 1)
by$split_coverage <- round(100 * as.vector(tapply(!is.na(d$dpres), d$year, mean)), 1)
by$pct_split[by$split_coverage < 90] <- NA        # too thin to report
write.csv(by, "derived/by_year.csv", row.names = FALSE)

cat(sprintf("\nelections: %d, %d-%d\n", nrow(by), min(yrs), max(yrs)))
dec <- function(v) round(tapply(by[[v]], 10 * (by$year %/% 10), mean), 1)
cat("\nuncontested by decade:      "); cat(sprintf("%5.1f", dec("pct_uncontested")))
cat("\n  South:                    "); cat(sprintf("%5.1f", dec("pct_uncontested_south")))
cat("\n  non-South:                "); cat(sprintf("%5.1f", dec("pct_uncontested_non_south")))
cat("\ncompetitive (within 5 pts): "); cat(sprintf("%5.1f", dec("pct_competitive")))
cat("\nlandslides (20+ pts):       "); cat(sprintf("%5.1f", dec("pct_landslide")))
cat("\n\nsplit-district coverage below 90% (masked): ")
cat(paste(by$year[by$split_coverage < 90], collapse = ", "))
cat("\n")

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
