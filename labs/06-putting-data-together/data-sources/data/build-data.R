# ---------------------------------------------------------------------------
# Build the data-sources datasets.
#
# Four files end up in this folder:
#
#   derived/pres2024_counties.csv  county-level 2024 presidential returns (3,160 rows)
#   derived/pres2020_counties.csv  county-level 2020 presidential returns (3,152 rows)
#   derived/census_counties.csv    the Census Bureau's official county list (3,144 rows)
#   derived/pres2024_states.csv    state-level 2024 returns -- a verbatim copy of the
#                          `electoral-map` file, so this lab stands on its own
#
# Run this script from inside the data/ folder. It needs a network connection;
# the whole point of committing the outputs is that the lab does not.
#
# Everything here is downloaded, trimmed, checked, and written. No value is
# edited by hand.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- 1. County returns ------------------------------------------------------
#
# Source: Tony McGovern, "United States General Election Presidential Results
# by District, Ward, or County from 2008 to 2024"
#   https://github.com/tonmcg/US_County_Level_Election_Results_08-24
#   branch: master   DOI: 10.5281/zenodo.14223604
#
# The repository's own README is unusually candid about provenance: the 2024
# numbers were scraped from Fox News's results pages, the 2020 numbers from Fox
# News, Politico and the New York Times. It says outright that the results
# "are not authoritative." That honesty is why this file is a good teaching
# dataset -- the seams are visible.

#
# FROZEN, NOT FETCHED. Both files are committed verbatim in raw/ and the build
# reads them from there. The chapter's whole subject is a compilation that
# nobody is obliged to maintain -- no correction path, no custodian, no
# undertaking that it will exist next year -- and a chapter making that argument
# should not itself stop working the day the argument comes true.
#
# It also makes the specimen stable. Three chapters (`data-sources`, `mapping`,
# `wind-map`) examine THIS file and quote numbers out of it. A silent upstream
# edit would change those numbers underneath the prose, which is the one kind of
# drift the corpus cannot detect by rebuilding.
#
# The captures were verified byte-for-byte against the live addresses on
# 2026-08-13 before being frozen; raw/README.txt records what each one is.
#
# TO REFRESH -- a deliberate act, not a side effect of running the build:
#   curl -sL -o raw/2024_US_County_Level_Presidential_Results.csv \
#     https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/master/2024_US_County_Level_Presidential_Results.csv
#   curl -sL -o raw/2020_US_County_Level_Presidential_Results.csv \
#     https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/master/2020_US_County_Level_Presidential_Results.csv
# then re-run this script and read what changed. The row-count assertions below
# will fail loudly if the shape moved.

base <- paste0("https://raw.githubusercontent.com/tonmcg/",
               "US_County_Level_Election_Results_08-24/master/")

grab_counties <- function(year) {
  src <- sprintf("raw/%d_US_County_Level_Presidential_Results.csv", year)
  if (!file.exists(src))
    stop(sprintf(paste0("%s is missing. It is a committed specimen, not a ",
                        "download -- see the refresh commands at the top of ",
                        "this script. Upstream: %s%d_US_County_Level_",
                        "Presidential_Results.csv"), src, base, year))
  d <- read.csv(src, colClasses = c(county_fips = "character"))
  # Keep only what the lab uses. The source also carries diff, per_gop,
  # per_dem and per_point_diff, all of which are derivable from these six.
  d <- d[, c("state_name", "county_fips", "county_name",
             "votes_dem", "votes_gop", "total_votes")]
  d <- d[order(d$county_fips), ]
  rownames(d) <- NULL
  d
}

c24 <- grab_counties(2024)
c20 <- grab_counties(2020)

stopifnot(nrow(c24) == 3160, nrow(c20) == 3152)
stopifnot(!any(is.na(c24)), !any(is.na(c20)))
stopifnot(all(nchar(c24$county_fips) == 5), all(nchar(c20$county_fips) == 5))
stopifnot(!any(duplicated(c24$county_fips)), !any(duplicated(c20$county_fips)))
stopifnot(length(unique(c24$state_name)) == 51)

# Sanity check against the certified national result. The certified 2024
# totals are Trump 77,302,580 and Harris 75,017,613. We should land close but
# low, because this source does not capture every jurisdiction's write-ins.
cat("2024 national, this file:  Trump", sum(c24$votes_gop),
    " Harris", sum(c24$votes_dem), " Total", sum(c24$total_votes), "\n")
cat("2024 national, certified:  Trump 77302580  Harris 75017613\n")
stopifnot(abs(sum(c24$votes_gop) - 77302580) < 50000)
stopifnot(abs(sum(c24$votes_dem) - 75017613) < 50000)

write.csv(c24, "derived/pres2024_counties.csv", row.names = FALSE)
write.csv(c20, "derived/pres2020_counties.csv", row.names = FALSE)


# --- 2. The Census Bureau's county list -------------------------------------
#
# Source: 2024 Gazetteer Files, Counties, national file
#   https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/
# This is the current legal geography: 3,144 county-equivalents across the 50
# states and DC. We drop the land-area and centroid columns and the outlying
# territories (PR, GU, VI, AS, MP, UM), none of which cast presidential votes.

#
# FROZEN, like the compilation. The capture in raw/ is the file from inside the
# Bureau's zip, unchanged -- right-padding and all, which is why the chapter can
# show what fixed-width columns look like. Verified byte-for-byte against the
# live zip on 2026-08-13.
#
# The Bureau is a real custodian with a correction path, so this is frozen for a
# different reason than the compilation was: the chapter's Connecticut claim is
# a comparison BETWEEN TWO VINTAGES, and a comparison in which one side can move
# is not a comparison. Refreshing the 2024 side without the 2020 side would
# quietly turn the receipt into an assertion.
#
# TO REFRESH:
#   curl -sL -o /tmp/gaz.zip \
#     https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_counties_national.zip
#   unzip -o -d raw/ /tmp/gaz.zip 2024_Gaz_counties_national.txt

gaz_url <- paste0("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/",
                  "2024_Gazetteer/2024_Gaz_counties_national.zip")
gaz_raw <- "raw/2024_Gaz_counties_national.txt"
if (!file.exists(gaz_raw))
  stop(sprintf("%s is missing. It is a committed specimen, not a download -- see the refresh command above. Upstream: %s",
               gaz_raw, gaz_url))
gz <- read.delim(gaz_raw, colClasses = "character")
gz$USPS <- trimws(gz$USPS)
gz$NAME <- trimws(gz$NAME)

abbrev  <- c(state.abb, "DC")
fullnm  <- c(state.name, "District of Columbia")
gz <- gz[gz$USPS %in% abbrev, ]

cen <- data.frame(
  fips         = gz$GEOID,
  name         = gz$NAME,
  state_abbrev = gz$USPS,
  state_name   = fullnm[match(gz$USPS, abbrev)]
)
cen <- cen[order(cen$fips), ]
rownames(cen) <- NULL

stopifnot(nrow(cen) == 3144, !any(duplicated(cen$fips)), !any(is.na(cen)))
stopifnot(length(unique(cen$state_name)) == 51)
write.csv(cen, "derived/census_counties.csv", row.names = FALSE)


# --- 3. Documented check: Connecticut's codes really did change -------------
#
# The lab claims Connecticut swapped counties for planning regions. Here is the
# receipt, straight from two vintages of the Census Bureau's own code list.
# (This block only prints; it writes nothing.)

# Frozen for the reason given above: this is the OTHER side of the vintage
# comparison, and the receipt only holds if both sides are held still.
#   refresh: curl -sL -o raw/national_county2020.txt \
#     https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt
v2020_url <- paste0("https://www2.census.gov/geo/docs/reference/codes2020/",
                    "national_county2020.txt")
v2020_raw <- "raw/national_county2020.txt"
if (!file.exists(v2020_raw))
  stop(sprintf("%s is missing. It is a committed specimen, not a download. Upstream: %s",
               v2020_raw, v2020_url))
v2020 <- read.delim(v2020_raw, sep = "|", colClasses = "character")
v2020$fips <- paste0(v2020$STATEFP, v2020$COUNTYFP)

cat("\nConnecticut in the 2020 Census vintage:\n")
print(v2020[v2020$STATE == "CT", c("fips", "COUNTYNAME")], row.names = FALSE)
cat("\nConnecticut in the 2024 Census vintage (what we bundled):\n")
print(cen[cen$state_abbrev == "CT", c("fips", "name")], row.names = FALSE)

stopifnot(sum(v2020$STATE == "CT") == 8, sum(cen$state_abbrev == "CT") == 9)
stopifnot(all(cen$fips[cen$state_abbrev == "CT"] %in%
              c("09110","09120","09130","09140","09150",
                "09160","09170","09180","09190")))


# --- 4. State-level returns -------------------------------------------------
#
# Copied verbatim from ../../../03-elections/electoral-map/data/derived/pres2024_states.csv so
# that this lab folder is self-contained. That file was built by the `electoral-map`
# script from jaytimm's PresElectionResults compilation; see
# ../../../03-elections/electoral-map/data/build-data.R for how. Its vote columns are
# PERCENTAGES, not counts -- which is the first thing this lab has to reckon
# with.

src <- "../../../03-elections/electoral-map/data/derived/pres2024_states.csv"
if (file.exists(src)) {
  file.copy(src, "derived/pres2024_states.csv", overwrite = TRUE)
  st <- read.csv("derived/pres2024_states.csv")
  stopifnot(nrow(st) == 51, sum(st$ev) == 538)
  cat("\ncopied pres2024_states.csv:", nrow(st), "rows\n")
} else {
  warning("electoral-map state file not found; pres2024_states.csv not refreshed.")
}


# --- 5. Report --------------------------------------------------------------

cat("\n--- written ---\n")
for (f in c("derived/pres2024_counties.csv", "derived/pres2020_counties.csv",
            "derived/census_counties.csv", "derived/pres2024_states.csv")) {
  if (file.exists(f)) {
    cat(sprintf("%-24s %5d rows  %6.0f KB\n", f,
                nrow(read.csv(f)), file.size(f) / 1024))
  }
}

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
