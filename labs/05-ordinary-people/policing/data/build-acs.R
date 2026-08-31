# ---------------------------------------------------------------------------
# build-acs.R -- the candidate denominators for the benchmark figure.
#
# WHAT THIS IS FOR
#
# The brief's spine (Steps 3-5) is that the stop counts in `derived/by_race.csv` have
# no honest denominator. To SHOW that rather than assert it, the brief plots
# the Black:white and Hispanic:white stop-rate ratio recomputed under every
# denominator anybody actually reaches for, and then under the two that need
# no denominator at all (search rate, hit rate). This script fetches the real
# denominators. Nothing here is invented; if a category could not be matched
# honestly it was dropped rather than approximated.
#
# THE VINTAGE. The stop file covers 2007-2016. Two ACS 5-year releases tile
# that window exactly -- 2007-2011 and 2012-2016 -- so both are fetched and
# averaged. The script prints how far the two vintages disagree; if that ever
# grows large, the averaging stops being defensible.
#
# THE CATEGORY PROBLEM, WHICH IS NOT FIXABLE. The stop file's race field is
# the OFFICER'S PERCEPTION, ticked on a form at the roadside. Every ACS
# variable below is SELF-REPORTED race and ethnicity. These are different
# measurements of different things and the brief says so, twice. The figure
# built from this file exists to show that the comparison moves around under
# every denominator you try -- not to endorse any one of them.
#
# WHERE THE MATCH IS IMPERFECT, AND IT IS:
#
#   residents  B03002 is a clean match: its categories are mutually exclusive
#              and Hispanic origin is separated out, so white/Black/Hispanic/
#              Asian+PI/other partition the population with no double count.
#
#   adults     B01001H is white alone NOT Hispanic, but B01001B and B01001D/E
#              are "alone" iterations that INCLUDE Hispanics of those races.
#              So the Black and Asian adult counts are slightly inflated
#              relative to their B03002 equivalents, which pushes their ratios
#              slightly DOWN. The script prints the size of that inflation.
#
#   drivers    Same iteration problem as adults. Worse: the universe is
#              "workers 16 and over" who live in San Francisco. It counts a
#              resident who drives to a job in San Mateo and misses the
#              commuter from Oakland driving into the city -- which is the
#              brief's own objection, arriving inside the best denominator
#              available. That is the point, and the figure labels it.
#
#   other      Deliberately NOT built for the adult and driver rungs. The
#              only available iterations are AIAN + some-other-race + two-or-
#              more, and "some other race alone" is overwhelmingly people who
#              also report Hispanic origin, so the category would double-count
#              against Hispanic. Fewer denominators, not an invented one.
#
# SOURCE. U.S. Census Bureau, American Community Survey 5-year estimates,
# via the Census Data API. San Francisco County, CA = state 06, county 075.
# FIPS codes are handled as CHARACTER throughout; "06" and "075" lose their
# leading zeros the moment anything treats them as numbers.
#
#   https://api.census.gov/data/2011/acs/acs5   (2007-2011)
#   https://api.census.gov/data/2016/acs/acs5   (2012-2016)
#
# Fetched 2026-08-10. Row counts and byte sizes are recorded by ../../../_lib/
# provenance.R into PROVENANCE.tsv on every run; a source that moves prints a
# banner.
#
# A key is required by the API as of 2025. Free from
# https://api.census.gov/data/key_signup.html. Put it in ~/.Renviron as
#   CENSUS_API_KEY='...'
# and it is picked up below. It is never written into this file.
#
# Run from this directory:  Rscript build-acs.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

source("../../../_lib/provenance.R")

KEY <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(KEY)) stop("set CENSUS_API_KEY in ~/.Renviron -- see the header")

STATE <- "06"; COUNTY <- "075"          # character. always. see header.
YEARS <- c("2011", "2016")              # 2007-2011 and 2012-2016 5-year

# ---- fetch -----------------------------------------------------------------

get_acs <- function(year, vars) {
  url <- paste0("https://api.census.gov/data/", year, "/acs/acs5",
                "?get=NAME,", paste(vars, collapse = ","),
                "&for=county:", COUNTY, "&in=state:", STATE)
  dest <- file.path(tempdir(), paste0("acs", year, "_", substr(vars[1], 1, 7), ".json"))
  prov_fetch(paste0(url, "&key=", KEY), dest,
             label = paste(year, substr(vars[1], 1, 7)))
  txt <- paste(readLines(dest, warn = FALSE), collapse = "")
  # [["NAME","V1",...],["San Francisco...","123",...]]  -- parse without jsonlite
  cells <- regmatches(txt, gregexpr('"[^"]*"', txt))[[1]]
  cells <- gsub('"', "", cells)
  hdr <- cells[seq_along(vars) + 1L]             # skip NAME
  val <- cells[length(cells) - 2L - rev(seq_along(vars)) + 1L]
  stopifnot(identical(hdr, vars))
  setNames(as.numeric(val), vars)
}

# One request per (year, table-family) so a single failure is legible.
sum_vars <- function(v, keys) sum(v[keys])

# B03002 -- Hispanic or Latino origin by race. Mutually exclusive categories.
B03 <- c("B03002_001E", "B03002_003E", "B03002_004E", "B03002_005E",
         "B03002_006E", "B03002_007E", "B03002_008E", "B03002_009E",
         "B03002_012E")

# B01001{H,B,I,D,E} -- sex by age, race iterations. 18+ = total - under-18.
U18 <- c("_003E", "_004E", "_005E", "_006E", "_018E", "_019E", "_020E", "_021E")
age_vars <- function(it) paste0("B01001", it, c("_001E", U18))

# B08105{H,B,I,D,E} -- means of transportation to work. drove alone + carpooled.
drv_vars <- function(it) paste0("B08105", it, c("_002E", "_003E"))

rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)
allages <- list()   # B01001{it}_001E, all ages, for the iteration check below

for (yr in YEARS) {
  v3 <- get_acs(yr, B03)
  add(year = yr, denominator = "residents", race = "white",
      count = sum_vars(v3, "B03002_003E"), source = "B03002_003E")
  add(year = yr, denominator = "residents", race = "black",
      count = sum_vars(v3, "B03002_004E"), source = "B03002_004E")
  add(year = yr, denominator = "residents", race = "hispanic",
      count = sum_vars(v3, "B03002_012E"), source = "B03002_012E")
  add(year = yr, denominator = "residents", race = "asian/pacific islander",
      count = sum_vars(v3, c("B03002_006E", "B03002_007E")), source = "B03002_006E+007E")
  add(year = yr, denominator = "residents", race = "other",
      count = sum_vars(v3, c("B03002_005E", "B03002_008E", "B03002_009E")),
      source = "B03002_005E+008E+009E")

  its <- list(white = "H", black = "B", hispanic = "I")
  for (nm in names(its)) {
    va <- get_acs(yr, age_vars(its[[nm]]))
    add(year = yr, denominator = "adults 18+", race = nm,
        count = va[[1]] - sum(va[-1]), source = paste0("B01001", its[[nm]], " 18+"))
    allages[[paste(yr, nm)]] <- va[[1]]
    vd <- get_acs(yr, drv_vars(its[[nm]]))
    add(year = yr, denominator = "drives to work", race = nm,
        count = sum(vd), source = paste0("B08105", its[[nm]], "_002E+003E"))
  }
  # asian/pacific islander needs two iterations summed at both rungs
  va <- get_acs(yr, age_vars("D")); ve <- get_acs(yr, age_vars("E"))
  add(year = yr, denominator = "adults 18+", race = "asian/pacific islander",
      count = (va[[1]] - sum(va[-1])) + (ve[[1]] - sum(ve[-1])),
      source = "B01001D+B01001E 18+")
  allages[[paste(yr, "asian/pacific islander")]] <- va[[1]] + ve[[1]]
  vd <- get_acs(yr, drv_vars("D")); vf <- get_acs(yr, drv_vars("E"))
  add(year = yr, denominator = "drives to work", race = "asian/pacific islander",
      count = sum(vd) + sum(vf), source = "B08105D+B08105E _002E+003E")
}

a <- do.call(rbind, rows)

# ---- how much do the two vintages disagree? --------------------------------

sh <- do.call(rbind, lapply(split(a, list(a$year, a$denominator), drop = TRUE),
  function(g) data.frame(year = g$year[1], denominator = g$denominator[1],
                         race = g$race, share = 100 * g$count / sum(g$count))))
cat("\n  share of each denominator, by ACS vintage (%):\n")
w <- reshape(sh, idvar = c("denominator", "race"), timevar = "year",
             direction = "wide")
w$drift <- round(w$share.2016 - w$share.2011, 2)
w$share.2011 <- round(w$share.2011, 2); w$share.2016 <- round(w$share.2016, 2)
print(w[order(w$denominator, -w$share.2016), ], row.names = FALSE)
cat(sprintf("\n  largest vintage-to-vintage drift: %.2f percentage points\n",
            max(abs(w$drift))))

# ---- how much does the "alone" iteration inflate Black and Asian? ----------

r16 <- a[a$year == "2016" & a$denominator == "residents", ]
cat("\n  iteration check (2012-2016), like for like, ALL AGES. B01001{B,D,E}\n")
cat("  are race-ALONE iterations and include Hispanics of that race; the\n")
cat("  B03002 figures exclude them. The excess below is the size of the\n")
cat("  category mismatch carried by the adult and driver rungs:\n")
for (nm in c("black", "asian/pacific islander")) {
  nh <- r16$count[r16$race == nm]
  al <- allages[[paste("2016", nm)]]
  cat(sprintf("    %-24s B03002 not-Hispanic %s ; race-alone %s  (+%.1f%%)\n",
              nm, format(nh, big.mark = ","), format(al, big.mark = ","),
              100 * (al - nh) / nh))
}

# ---- average the two vintages and write ------------------------------------

out <- aggregate(count ~ denominator + race, data = a,
                 FUN = function(x) round(mean(x)))
src <- a[a$year == "2016", c("denominator", "race", "source")]
out <- merge(out, src, by = c("denominator", "race"))
out$state <- STATE; out$county <- COUNTY      # character, not numeric
out <- out[order(match(out$denominator, c("residents", "adults 18+", "drives to work")),
                 -out$count), ]

write.csv(out, "derived/acs_denominators.csv", row.names = FALSE)
cat(sprintf("\n  wrote acs_denominators.csv : %d rows, %d denominators\n",
            nrow(out), length(unique(out$denominator))))
print(out, row.names = FALSE)

prov_report()
