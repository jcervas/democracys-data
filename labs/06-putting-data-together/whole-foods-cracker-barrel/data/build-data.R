# ---------------------------------------------------------------------------
# whole-foods-cracker-barrel/data/build-data.R
#
# Rebuilds every number in the brief. Run from this directory:
#
#     Rscript build-data.R
#
# SOURCES
# OpenStreetMap contributors, United States objects tagged with the Wikidata
# identifiers for Whole Foods Market (Q1809448) and Cracker Barrel (Q4492609),
# retrieved through the Overpass API and committed to raw/. Cracker Barrel Old
# Country Store, Inc., Annual Report on Form 10-K for fiscal 2025, Item 1, for
# the store and state counts the volunteer file is audited against. Tony
# McGovern, US County Level Presidential Results 2008-2024, for all five
# elections, checked for 2024 against the certified state returns assembled in
# county-returns/. U.S. Census Bureau: 2024 and 2020 Cartographic Boundary
# Files (counties), 2024 Gazetteer Files (counties), and Vintage 2024
# Population Estimates. U.S. Department of Agriculture, Economic Research
# Service, County-level Data Sets: Education.
#
# WHAT THE CHAPTER NEEDS AND WHERE IT COMES FROM
#
# The metric under test joins two things. The returns are the easy half: the
# corpus already argues about them at length in returns-source/ and
# county-returns/, and this build reads the same GitHub compilation the rest of
# Part II reads, checked against the certified assembly next door.
#
# The hard half is the store list, because there is no such file. Neither chain
# is obliged to publish where its stores are. Cracker Barrel files a COUNT with
# the SEC -- 657 stores in 43 states -- and no addresses. Whole Foods is a
# subsidiary of Amazon and files nothing at all about itself; the commercial
# trackers that sell the number disagree with each other by about ten percent.
# Both companies run a store locator and both locators are JavaScript, so
# neither can be read by a script. Probed 13 Aug 2026:
#
#   https://www.crackerbarrel.com/sitemap.xml    -> 200, no location URLs in it
#   https://www.crackerbarrel.com/api/...        -> 404 with an HTML body
#   https://www.wholefoodsmarket.com/stores      -> 200, an empty React shell
#
# So the inventory used here is OpenStreetMap, queried through Overpass on the
# brand's Wikidata identifier. That is a volunteer file, which is a real
# limitation and also the chapter's subject: the independent variable in a
# famous political metric is a crowd-sourced guess at a private fact.
#
# The Overpass result is COMMITTED to raw/. It has to be: the same query run
# next week returns a different answer, so a chapter that refetched silently
# would quietly restate its own findings. provenance.R prints a banner when it
# moves.
# ---------------------------------------------------------------------------

source("../../../_lib/provenance.R")
source("../../../_lib/precision.R")

suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})
sf::sf_use_s2(FALSE)

options(timeout = 600, stringsAsFactors = FALSE)
CACHE <- Sys.getenv("DD_WFCB_CACHE", unset = file.path(tempdir(), "wfcb"))
dir.create(CACHE,      showWarnings = FALSE, recursive = TRUE)
dir.create("raw",      showWarnings = FALSE)
dir.create("derived",  showWarnings = FALSE)

say <- function(...) cat(sprintf(...), "\n", sep = "")
FACTS <- list()
fact  <- function(k, v) FACTS[[k]] <<- as.character(v)

# ---------------------------------------------------------------------------
# 1. THE STORE LIST
#
# Overpass answers a GET, so the whole query is in the URL and provenance.R
# records the query itself rather than a bare hostname. `nwr` because a store
# is tagged as a node in some places and as a building outline in others;
# `out center` collapses the outlines to a point so both arrive as coordinates.
#
# The match is on brand:wikidata, not on the name. Names are typed by hand by
# thousands of people -- "Whole Foods", "Whole Foods Market", "WholeFoods",
# "Whole Foods Market 365" -- and a name match would silently drop whichever
# spellings nobody thought of.
# ---------------------------------------------------------------------------

BRANDS <- data.frame(
  brand  = c("Whole Foods Market", "Cracker Barrel"),
  short  = c("whole_foods", "cracker_barrel"),
  qid    = c("Q1809448", "Q4492609"))

overpass_url <- function(qid) {
  q <- sprintf(paste0('[out:json][timeout:300];',
                      'area["ISO3166-1"="US"][admin_level=2]->.us;',
                      '(nwr["brand:wikidata"="%s"](area.us););',
                      'out center;'), qid)
  paste0("https://overpass-api.de/api/interpreter?data=", URLencode(q, reserved = TRUE))
}

read_overpass <- function(path, brand) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  el <- j$elements
  d <- do.call(rbind, lapply(el, function(e) {
    lat <- if (!is.null(e$lat)) e$lat else e$center$lat
    lon <- if (!is.null(e$lon)) e$lon else e$center$lon
    if (is.null(lat) || is.null(lon)) return(NULL)
    data.frame(osm_type = e$type, osm_id = e$id, lat = lat, lon = lon,
               addr_state = if (is.null(e$tags[["addr:state"]])) NA_character_
                            else e$tags[["addr:state"]],
               addr_city  = if (is.null(e$tags[["addr:city"]])) NA_character_
                            else e$tags[["addr:city"]])
  }))
  d$brand <- brand
  attr(d, "osm_base") <- j$osm3s$timestamp_osm_base
  d
}

# The committed capture is authoritative and is NOT refetched by default.
#
# Every other source in this corpus can be pulled fresh because pulling it
# fresh returns the same thing. This one does not: a store opens, a mapper
# fixes a tag, and the chapter's headline numbers move. A build that refetched
# silently would give a different answer to the same question every time it
# ran, and the printed brief would drift away from its own data. So:
#
#     Rscript build-data.R                 rebuilds from raw/, reproducibly
#     WFCB_REFRESH=1 Rscript build-data.R  re-queries Overpass and reports drift
#
# The public Overpass instance is rate-limited and does fail, so the refresh
# path retries rather than leaving a half-written file behind.
REFRESH <- nzchar(Sys.getenv("WFCB_REFRESH"))

# Downloads to a scratch file and only moves it into raw/ once it parses. A
# dropped connection here writes a truncated but perfectly plausible JSON file,
# and overwriting the good capture with it would destroy the one copy of a
# source that cannot be fetched again.
fetch_overpass <- function(url, dest, label, tries = 4) {
  tmp <- tempfile(fileext = ".json")
  for (k in seq_len(tries)) {
    ok <- tryCatch({
      prov_fetch(url, tmp, mode = "wb", quiet = TRUE)
      length(jsonlite::fromJSON(tmp, simplifyVector = FALSE)$elements) > 100
    }, error = function(e) {
      say("  overpass attempt %d failed: %s", k, conditionMessage(e)); FALSE })
    if (isTRUE(ok)) {
      file.copy(tmp, dest, overwrite = TRUE)
      .prov_record(url, dest, label)
      return(invisible(dest))
    }
    Sys.sleep(15 * k)
  }
  stop("Overpass did not answer after ", tries, " attempts. The committed ",
       "capture in raw/ is untouched; rerun without WFCB_REFRESH.")
}

stores <- list()
for (i in seq_len(nrow(BRANDS))) {
  f <- file.path("raw", paste0(BRANDS$short[i], "_osm.json"))
  u <- overpass_url(BRANDS$qid[i])
  if (REFRESH || !file.exists(f)) fetch_overpass(u, f, BRANDS$brand[i])
  else say("  %-20s using committed capture (WFCB_REFRESH=1 to re-query)",
           BRANDS$short[i])
  stores[[i]] <- read_overpass(f, BRANDS$brand[i])
  say("  %-20s %d locations (OSM base %s)", BRANDS$short[i],
      nrow(stores[[i]]), attr(stores[[i]], "osm_base"))
}
fact("osm_base", substr(attr(stores[[1]], "osm_base"), 1, 10))
stores <- do.call(rbind, stores)

fact("n_wf_osm", sum(stores$brand == "Whole Foods Market"))
fact("n_cb_osm", sum(stores$brand == "Cracker Barrel"))

# Guards on the one source that can silently return something else. A brand
# whose Wikidata id was retagged, or a query that hit a rate limit and came
# back with a fragment, both produce a valid file with far too few rows.
stopifnot(sum(stores$brand == "Whole Foods Market") > 400,
          sum(stores$brand == "Cracker Barrel")     > 500,
          !anyNA(stores$lat), !anyNA(stores$lon),
          all(stores$lat > 17 & stores$lat < 72),
          all(stores$lon > -180 & stores$lon < -65))

# ---------------------------------------------------------------------------
# 2. WHICH COUNTY IS EACH STORE IN
#
# A point in a polygon, which sounds like nothing and is where the geography
# decisions live. The 2024 cartographic boundary file is the same vintage as
# the 2024 returns, which matters in exactly one state: Connecticut replaced
# its counties with planning regions, and 2024 returns are published on the new
# shapes while every earlier year in this build is published on the old ones.
# ---------------------------------------------------------------------------

CB_URL <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_500k.zip"
cbzip  <- file.path(CACHE, "cb_2024_us_county_500k.zip")
if (!file.exists(cbzip)) prov_fetch(CB_URL, cbzip, label = "county boundaries")
unzip(cbzip, exdir = file.path(CACHE, "cbshp"), overwrite = FALSE)
cty <- sf::st_read(file.path(CACHE, "cbshp", "cb_2024_us_county_500k.shp"), quiet = TRUE)
cty <- cty[cty$STATEFP %in% sprintf("%02d", c(1:56, 11)), ]
cty <- cty[!cty$STATEFP %in% c("60", "66", "69", "72", "78"), ]   # territories

pts <- sf::st_as_sf(stores, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
pts <- sf::st_transform(pts, sf::st_crs(cty))
hit <- sf::st_within(pts, cty)
idx <- vapply(hit, function(x) if (length(x)) x[1] else NA_integer_, integer(1))

# Stores that land in no county are on the wrong side of a simplified coastline
# or carry a coordinate somebody typed wrong. Snap them to the nearest county
# and record how many there were, because "we assigned it to the nearest thing"
# is a decision and not a lookup.
n_miss <- sum(is.na(idx))
if (n_miss > 0) {
  near <- sf::st_nearest_feature(pts[is.na(idx), ], cty)
  d_km <- as.numeric(sf::st_distance(
    sf::st_transform(pts[is.na(idx), ], 5070),
    sf::st_transform(cty[near, ], 5070), by_element = TRUE)) / 1000
  idx[is.na(idx)] <- near
}
fact("stores_snapped", n_miss)
fact("stores_snapped_max_km", if (n_miss > 0) sprintf("%.1f", max(d_km)) else "0")

stores$fips  <- cty$GEOID[idx]
stores$state <- cty$STUSPS[idx]
stores$county_name <- cty$NAMELSAD[idx]

# THE SAME STORES ON THE OLD GEOGRAPHY.
#
# Connecticut abolished its counties as statistical units and the census
# replaced them with nine planning regions, published from 2022. The 2024
# returns are on the new shapes and the 2008-2020 returns are on the old ones,
# so a single point-in-polygon against a single vintage puts every Connecticut
# store in a unit that four of the five elections have never heard of --
# silently, because a missing join produces a zero and a zero is a valid store
# count. Left uncorrected this file reported that Connecticut contained no
# Whole Foods county until 2024, which is false: it contained four all along.
#
# So the assignment is done twice, once per vintage, and each election reads
# the one it is published on. Outside Connecticut the two agree, and the build
# checks that rather than assuming it.
CB20_URL <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_500k.zip"
cb20 <- file.path(CACHE, "cb_2020_us_county_500k.zip")
if (!file.exists(cb20)) prov_fetch(CB20_URL, cb20, label = "county boundaries, 2020 vintage")
unzip(cb20, exdir = file.path(CACHE, "cbshp20"), overwrite = FALSE)
old_cty <- sf::st_read(file.path(CACHE, "cbshp20", "cb_2020_us_county_500k.shp"), quiet = TRUE)
old_cty <- old_cty[!old_cty$STATEFP %in% c("60", "66", "69", "72", "78"), ]

pts20 <- sf::st_transform(pts, sf::st_crs(old_cty))
h20   <- sf::st_within(pts20, old_cty)
i20   <- vapply(h20, function(x) if (length(x)) x[1] else NA_integer_, integer(1))
if (anyNA(i20)) i20[is.na(i20)] <- sf::st_nearest_feature(pts20[is.na(i20), ], old_cty)
stores$fips_2020 <- old_cty$GEOID[i20]

differ <- stores$fips != stores$fips_2020
fact("vintage_differ",    sum(differ))
fact("vintage_differ_ct", sum(differ & substr(stores$fips, 1, 2) == "09"))

dd_write_csv(stores[, c("brand", "osm_type", "osm_id", "lat", "lon",
                        "fips", "fips_2020", "county_name", "state")],
             "derived/stores.csv")

say("  assigned %d stores to counties (%d snapped, %d differ by vintage)",
    nrow(stores), n_miss, sum(differ))

# ---------------------------------------------------------------------------
# 3. THE CHECK NOBODY CAN RUN ON WHOLE FOODS
#
# Cracker Barrel's 10-K states a total and a state count on a stated date, so
# the volunteer file can be audited against a filing. These two numbers are
# typed in by hand from the PDF; they are the only hand-entered figures in the
# build.
# ---------------------------------------------------------------------------

TENK <- list(as_of = "12 September 2025", stores = 657L, states = 43L,
             url = "https://investor.crackerbarrel.com/static-files/881c3ef3-0a78-4e60-9fc8-6d2a340bdbc8")
cb <- stores[stores$brand == "Cracker Barrel", ]
fact("cb_10k_stores", TENK$stores)
fact("cb_10k_states", TENK$states)
fact("cb_10k_asof",   TENK$as_of)
fact("cb_osm_states", length(unique(cb$state)))
fact("cb_coverage",   sprintf("%.1f", 100 * nrow(cb) / TENK$stores))

wf <- stores[stores$brand == "Whole Foods Market", ]
fact("wf_osm_states", length(unique(wf$state)))

# The three commercial trackers quoted in the brief, typed in with their dates.
trackers <- data.frame(
  source = c("Cracker Barrel 10-K (FY2025)",
             "OpenStreetMap, this build",
             "ScrapeHero", "storelocators.com", "Statista / company reporting"),
  brand  = c("Cracker Barrel", "Cracker Barrel", "Whole Foods Market",
             "Whole Foods Market", "Whole Foods Market"),
  stores = c(TENK$stores, nrow(cb), 537L, 517L, 570L),
  as_of  = c("12 Sep 2025", format(as.Date(FACTS$osm_base), "%d %b %Y"),
             "15 Jul 2026", "Aug 2026", "2025"),
  kind   = c("SEC filing", "volunteer survey", "commercial tracker",
             "commercial tracker", "commercial tracker"))
trackers$stores_osm <- c(nrow(cb), nrow(cb), nrow(wf), nrow(wf), nrow(wf))
dd_write_csv(trackers, "derived/inventory_check.csv")

by_state <- as.data.frame(table(cb$state), stringsAsFactors = FALSE)
names(by_state) <- c("state", "osm_stores")
dd_write_csv(by_state[order(-by_state$osm_stores), ], "derived/cb_by_state.csv")

# The volunteer file says 44 states and the filing says 43, so at least one
# store on the map is not a store. The candidates are the states holding
# exactly one, and the discrepancy resolves to a specific restaurant: Maine's
# only Cracker Barrel, in South Portland, closed permanently in January 2025 --
# eight months before the filing date, and it is still on the map today. Its
# OSM object is also the only one of the six with no `website` tag, which is
# the sort of tell that is obvious afterwards and invisible before.
singles <- names(which(table(cb$state) == 1))
fact("cb_single_states", paste(sort(singles), collapse = ", "))
fact("cb_n_single_states", length(singles))
me <- cb[cb$state == "ME", ]
fact("cb_me_stores", nrow(me))
fact("cb_states_gap", length(unique(cb$state)) - TENK$states)

# ---------------------------------------------------------------------------
# 4. THE RETURNS
#
# One publisher for all five elections, deliberately: mixing compilations
# across years would put a source change inside a trend line. The certified
# assembly in county-returns/ is used as a check on 2024 rather than as the
# series, for the same reason.
# ---------------------------------------------------------------------------

TON <- "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/master/"

pad <- function(x) sprintf("%05d", as.integer(x))

old <- prov_read_csv(paste0(TON, "US_County_Level_Presidential_Results_08-16.csv"),
                     label = "county returns 2008-2016")
ret <- list()
for (y in c(2008, 2012)) {
  ret[[as.character(y)]] <- data.frame(
    fips = pad(old$fips_code), year = y,
    dem = old[[paste0("dem_", y)]], gop = old[[paste0("gop_", y)]],
    total = old[[paste0("total_", y)]])
}
for (y in c(2016, 2020, 2024)) {
  d <- prov_read_csv(paste0(TON, y, "_US_County_Level_Presidential_Results.csv"),
                     label = paste("county returns", y))
  fp <- if ("combined_fips" %in% names(d)) d$combined_fips else d$county_fips
  ret[[as.character(y)]] <- data.frame(
    fips = pad(fp), year = y, dem = d$votes_dem, gop = d$votes_gop,
    total = d$total_votes)
}
ret <- do.call(rbind, ret)
ret <- ret[!is.na(ret$dem) & !is.na(ret$gop), ]

# Two codes that are not stable across the five files, and both would have
# passed silently. Neither is a mistake by the compiler; both are the country
# changing under a join key.
#
# SHANNON COUNTY, South Dakota, was renamed Oglala Lakota in 2015 and given a
# new FIPS code. The 2008-2016 file uses 46113 and the 2020 and 2024 files use
# 46102, so a naive panel carries TWO rows for one place, each missing the
# other's elections.
fact("recoded_shannon", sum(ret$fips == "46113"))
ret$fips[ret$fips == "46113"] <- "46102"

# THE DISTRICT OF COLUMBIA is one row in 2008-2020 and EIGHT WARDS in 2024.
# Wards are not counties, they are not what the census publishes a county
# population for, and the shapefile has no such shapes -- so every Whole Foods
# in Washington landed in the single county polygon 11001, which the 2024
# returns call Ward 1. Left alone this file would have reported the District as
# a 40,000-vote county that Trump nearly won. The wards are summed back to the
# District.
dc <- substr(ret$fips, 1, 2) == "11"
fact("dc_ward_rows", sum(dc & ret$year == 2024))
if (any(dc)) {
  d <- ret[dc, ]
  agg <- aggregate(cbind(dem, gop, total) ~ year, d, sum)
  agg$fips <- "11001"
  ret <- rbind(ret[!dc, ], agg[, names(ret)[names(ret) != "win"]])
}

ret$win <- ifelse(ret$dem > ret$gop, "D", ifelse(ret$gop > ret$dem, "R", NA))

# One row per county per election, and no county carried twice. A duplicate
# here is what the Shannon and District fixes above exist to prevent, so the
# assertion is the thing that would notice if a future file reintroduced one.
stopifnot(!any(duplicated(ret[, c("fips", "year")])),
          all(nchar(ret$fips) == 5),
          length(unique(ret$year)) == 5)

# Alaska reports presidential votes by State House district, so it has no
# county-level return to carry. bellwether/ drops it for the same reason and
# this build follows, which is why every proportion here is of fifty
# jurisdictions rather than fifty-one.
ak <- substr(ret$fips, 1, 2) == "02"
fact("ak_rows_dropped", sum(ak))
ret <- ret[!ak, ]
stores_ak <- sum(substr(stores$fips, 1, 2) == "02")
fact("stores_in_ak", stores_ak)

YEARS <- sort(unique(ret$year))

# ---------------------------------------------------------------------------
# 5. THE CROSSCHECK AGAINST THE CERTIFIED FILE
# ---------------------------------------------------------------------------

OFF <- "../../county-returns/data/derived/pres2024_counties_official.csv"
if (file.exists(OFF)) {
  off <- read.csv(OFF, colClasses = c(county_fips = "character"))
  off <- off[!is.na(off$county_fips) & nchar(off$county_fips) == 5, ]
  r24 <- ret[ret$year == 2024, ]
  m <- merge(off, r24, by.x = "county_fips", by.y = "fips")
  m$win_off <- ifelse(m$votes_dem > m$votes_gop, "D", "R")
  fact("cert_matched",   nrow(m))
  fact("cert_win_diff",  sum(m$win_off != m$win))
  fact("cert_dem_diff",  sum(m$votes_dem != m$dem))
} else {
  fact("cert_matched", NA); fact("cert_win_diff", NA); fact("cert_dem_diff", NA)
}

# ---------------------------------------------------------------------------
# 6. COUNTY COVARIATES -- the alternatives the store list is tested against
# ---------------------------------------------------------------------------

PEP <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/co-est2024-alldata.csv"
pop <- prov_read_csv(PEP, label = "county population estimates", fileEncoding = "latin1")
pop <- pop[pop$SUMLEV == 50, ]
pop$fips <- paste0(sprintf("%02d", pop$STATE), sprintf("%03d", pop$COUNTY))
pop <- pop[, c("fips", "POPESTIMATE2024")]
names(pop)[2] <- "pop"

GAZ <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_counties_national.zip"
gz  <- file.path(CACHE, "gaz.zip")
if (!file.exists(gz)) prov_fetch(GAZ, gz, label = "county land area")
gzf <- unzip(gz, exdir = CACHE, overwrite = TRUE)
gaz <- read.delim(gzf[grep("counties", gzf)][1], colClasses = "character")
names(gaz) <- trimws(names(gaz))
gaz <- data.frame(fips = gaz$GEOID, land_sqmi = as.numeric(gaz$ALAND_SQMI))

EDU <- "https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/48747/Education.csv"
ed  <- prov_read_csv(EDU, label = "county education", fileEncoding = "latin1")
names(ed) <- c("fips", "state", "area", "attribute", "value")
ed <- ed[ed$attribute == "Percent of adults with a bachelor's degree or higher, 2018-22", ]
ed <- data.frame(fips = pad(ed$fips), ba_pct = as.numeric(ed$value))

# ---------------------------------------------------------------------------
# 7. THE COUNTY TABLE
# ---------------------------------------------------------------------------

counts <- function(d, key, nm) {
  t <- as.data.frame(table(d[[key]]), stringsAsFactors = FALSE)
  names(t) <- c("fips", nm)
  t
}
base <- unique(ret[, "fips", drop = FALSE])
for (a in list(list(wf, "fips",      "n_wf"),      list(cb, "fips",      "n_cb"),
               list(wf, "fips_2020", "n_wf_2020"), list(cb, "fips_2020", "n_cb_2020"))) {
  base <- merge(base, counts(a[[1]], a[[2]], a[[3]]), all.x = TRUE)
  base[[a[[3]]]][is.na(base[[a[[3]]]])] <- 0
}
base$has_wf <- base$n_wf > 0
base$has_cb <- base$n_cb > 0
base$has_wf_2020 <- base$n_wf_2020 > 0
base$has_cb_2020 <- base$n_cb_2020 > 0

# Every store must have landed on a county the returns also know about, or the
# store list and the election are describing different countries. Alaska is the
# stated exception and holds no store of either brand.
stopifnot(sum(base$n_wf) + sum(base$n_wf_2020) == 2 * nrow(wf),
          sum(base$n_cb) + sum(base$n_cb_2020) == 2 * nrow(cb),
          sum(base$has_wf) > 0, sum(base$has_cb) > 0)
base$cat <- ifelse( base$has_wf &  base$has_cb, "both",
             ifelse( base$has_wf & !base$has_cb, "whole foods only",
              ifelse(!base$has_wf &  base$has_cb, "cracker barrel only", "neither")))

base <- merge(base, pop, all.x = TRUE)
base <- merge(base, gaz, all.x = TRUE)
base <- merge(base, ed,  all.x = TRUE)
base$density <- base$pop / base$land_sqmi

for (y in YEARS) {
  r <- ret[ret$year == y, ]
  base[[paste0("win", y)]]   <- r$win[match(base$fips, r$fips)]
  base[[paste0("dem", y)]]   <- r$dem[match(base$fips, r$fips)]
  base[[paste0("gop", y)]]   <- r$gop[match(base$fips, r$fips)]
  base[[paste0("total", y)]] <- r$total[match(base$fips, r$fips)]
}
dd_write_csv(base, "derived/counties.csv")

fact("n_counties",   nrow(base))
fact("n_wf_counties", sum(base$has_wf))
fact("n_cb_counties", sum(base$has_cb))
fact("n_both",    sum(base$cat == "both"))
fact("n_wfonly",  sum(base$cat == "whole foods only"))
fact("n_cbonly",  sum(base$cat == "cracker barrel only"))
fact("n_neither", sum(base$cat == "neither"))

# ---------------------------------------------------------------------------
# 8. THE METRIC
#
# Wasserman's own definition, which the 2020 thread states explicitly: a county
# with both stores is counted in BOTH columns. So the two shares are not
# complements and the "gap" is a difference between two overlapping subsets.
# ---------------------------------------------------------------------------

dshare_counties <- function(sel, y) {
  w <- base[[paste0("win", y)]][sel]
  100 * mean(w == "D", na.rm = TRUE)
}
dshare_votes <- function(sel, y) {
  d <- base[[paste0("dem", y)]][sel]; g <- base[[paste0("gop", y)]][sel]
  100 * sum(d, na.rm = TRUE) / sum(d + g, na.rm = TRUE)
}

# Each election is scored on the geography it was published on: 2024 on the
# planning regions, everything earlier on the counties they replaced.
sel_wf <- function(y) if (y >= 2024) base$has_wf else base$has_wf_2020
sel_cb <- function(y) if (y >= 2024) base$has_cb else base$has_cb_2020

gap <- do.call(rbind, lapply(YEARS, function(y) data.frame(
  year = y,
  wf_counties = dshare_counties(sel_wf(y), y),
  cb_counties = dshare_counties(sel_cb(y), y),
  wf_votes    = dshare_votes(sel_wf(y), y),
  cb_votes    = dshare_votes(sel_cb(y), y),
  nat_votes   = dshare_votes(rep(TRUE, nrow(base)), y),
  n_wf_counties = sum(sel_wf(y) & !is.na(base[[paste0("win", y)]])),
  n_cb_counties = sum(sel_cb(y) & !is.na(base[[paste0("win", y)]])))))
gap$gap_counties <- gap$wf_counties - gap$cb_counties
gap$gap_votes    <- gap$wf_votes    - gap$cb_votes

# Shares are shares. A negative or above-100 value here would mean a selection
# vector had gone out of alignment with the returns columns.
stopifnot(all(gap$wf_counties >= 0 & gap$wf_counties <= 100),
          all(gap$cb_counties >= 0 & gap$cb_counties <= 100),
          all(gap$wf_votes    >= 0 & gap$wf_votes    <= 100),
          all(gap$cb_votes    >= 0 & gap$cb_votes    <= 100))
dd_write_csv(gap, "derived/gap.csv")

# The four exclusive categories, 2024 -- the form Wasserman's 2020 thread used.
cats <- do.call(rbind, lapply(c("whole foods only", "both",
                                "cracker barrel only", "neither"), function(k) {
  s <- base$cat == k
  data.frame(category = k, counties = sum(s),
             dem_counties_pct = dshare_counties(s, 2024),
             votes = sum(base$total2024[s], na.rm = TRUE),
             dem_votes_pct = dshare_votes(s, 2024))
}))
cats$vote_share <- 100 * cats$votes / sum(cats$votes)
# The four categories are exclusive and exhaustive, which is what makes them
# addable -- unlike the two overlapping columns the metric is usually stated in.
stopifnot(sum(cats$counties) == nrow(base),
          abs(sum(cats$vote_share) - 100) < 1e-6)
cats$county_share <- 100 * cats$counties / nrow(base)
cats$med_pop     <- sapply(cats$category, function(k) median(base$pop[base$cat == k], na.rm = TRUE))
cats$med_density <- sapply(cats$category, function(k) median(base$density[base$cat == k], na.rm = TRUE))
cats$med_ba      <- sapply(cats$category, function(k) median(base$ba_pct[base$cat == k], na.rm = TRUE))
dd_write_csv(cats, "derived/categories.csv")

# The exclusive comparison. Wasserman's own definition puts a county with both
# stores in both columns, which barely matters when the unit is counties (116
# of them) and matters enormously when the unit is votes: the both-stores
# counties are the large metros, so they dominate BOTH sides of a vote-weighted
# gap and shrink it by construction. Reported separately so the two effects --
# the definition and the aggregation -- do not get blamed on each other.
wfo <- base$cat == "whole foods only"; cbo <- base$cat == "cracker barrel only"
fact("excl_wf_votes",  sprintf("%.1f", dshare_votes(wfo, 2024)))
fact("excl_cb_votes",  sprintf("%.1f", dshare_votes(cbo, 2024)))
fact("excl_gap_votes", sprintf("%.1f", dshare_votes(wfo, 2024) - dshare_votes(cbo, 2024)))
fact("excl_gap_counties",
     sprintf("%.0f", dshare_counties(wfo, 2024) - dshare_counties(cbo, 2024)))

# ---------------------------------------------------------------------------
# 9. THE ANACHRONISM
#
# The op-ed reports 1992, 2000 and 2008 using the store list as it stood in
# 2011. This build reports the same elections using the store list as it stands
# now. Nothing about those elections changed. The difference is the store list.
# ---------------------------------------------------------------------------

PUBLISHED <- data.frame(
  year = c(2008L),
  wf_published = c(81), cb_published = c(36),
  note = "Wasserman, Washington Post, 28 September 2011")
anach <- merge(PUBLISHED, gap[, c("year", "wf_counties", "cb_counties",
                                  "gap_counties")], by = "year")
anach$gap_published <- anach$wf_published - anach$cb_published
anach$drift_wf  <- anach$wf_counties  - anach$wf_published
anach$drift_cb  <- anach$cb_counties  - anach$cb_published
anach$drift_gap <- anach$gap_counties - anach$gap_published
dd_write_csv(anach, "derived/anachronism.csv")
fact("drift_gap_2008", sprintf("%.1f", abs(anach$drift_gap[anach$year == 2008])))

# A second comparison against a second publication of the same metric. In
# December 2020 Wasserman posted the four exclusive categories rather than the
# two overlapping ones; those figures are transcribed here and recomputed on
# the 2020 geography with today's stores.
cat20 <- do.call(rbind, lapply(
  list(c("whole foods only", 95), c("both", 77),
       c("cracker barrel only", 18), c("neither", 12)), function(p) {
    k <- p[1]
    s <- switch(k,
      "whole foods only"    =  base$has_wf_2020 & !base$has_cb_2020,
      "both"                =  base$has_wf_2020 &  base$has_cb_2020,
      "cracker barrel only" = !base$has_wf_2020 &  base$has_cb_2020,
      "neither"             = !base$has_wf_2020 & !base$has_cb_2020)
    data.frame(category = k, counties = sum(s & !is.na(base$win2020)),
               published = as.numeric(p[2]),
               recomputed = dshare_counties(s, 2020))
  }))
cat20$drift <- cat20$recomputed - cat20$published
dd_write_csv(cat20, "derived/published_2020.csv")

# ---------------------------------------------------------------------------
# 10. DOES THE STORE ADD ANYTHING?
#
# Match on count, not on threshold: take the N densest counties, where N is the
# number of Whole Foods counties, and ask the same question of them. If a
# variable the store list is standing in for produces a bigger gap than the
# store list does, the store list is not carrying information of its own.
# ---------------------------------------------------------------------------

topn <- function(v, n) {
  s <- rep(FALSE, length(v))
  s[order(-v, na.last = NA)[seq_len(n)]] <- TRUE
  s
}
n_wfc <- sum(base$has_wf)
n_cbc <- sum(base$has_cb)

mk <- function(label, sel) data.frame(
  rule = label, counties = sum(sel),
  dem_counties_pct = dshare_counties(sel, 2024),
  dem_votes_pct    = dshare_votes(sel, 2024),
  overlap_wf_pct   = 100 * sum(sel & base$has_wf) / n_wfc)

alt <- rbind(
  mk("has a Whole Foods",                                    base$has_wf),
  mk(sprintf("the %d densest counties", n_wfc),              topn(base$density, n_wfc)),
  mk(sprintf("the %d most-educated counties", n_wfc),        topn(base$ba_pct,  n_wfc)),
  mk(sprintf("the %d most populous counties", n_wfc),        topn(base$pop,     n_wfc)))
dd_write_csv(alt, "derived/alternatives.csv")

# How much of the store list is predictable from density alone: of the N
# densest counties, how many actually have a Whole Foods.
dens_top <- topn(base$density, n_wfc)
fact("overlap_dens", sum(dens_top & base$has_wf))
fact("overlap_dens_pct", sprintf("%.0f", 100 * sum(dens_top & base$has_wf) / n_wfc))
edu_top <- topn(base$ba_pct, n_wfc)
fact("overlap_edu", sum(edu_top & base$has_wf))
fact("overlap_edu_pct", sprintf("%.0f", 100 * sum(edu_top & base$has_wf) / n_wfc))

# ---------------------------------------------------------------------------
# 11. COUNTIES ARE NOT PEOPLE
# ---------------------------------------------------------------------------

fact("wf_pop_share", sprintf("%.1f", 100 * sum(base$pop[base$has_wf], na.rm = TRUE) /
                                          sum(base$pop, na.rm = TRUE)))
fact("cb_pop_share", sprintf("%.1f", 100 * sum(base$pop[base$has_cb], na.rm = TRUE) /
                                          sum(base$pop, na.rm = TRUE)))
fact("neither_pop_share",
     sprintf("%.1f", 100 * sum(base$pop[base$cat == "neither"], na.rm = TRUE) /
                           sum(base$pop, na.rm = TRUE)))
fact("neither_county_pct", sprintf("%.0f", 100 * mean(base$cat == "neither")))

# Median county size in each category -- the mechanism behind the county/vote
# divergence, in one number each.
fact("med_pop_wf", format(median(base$pop[base$has_wf], na.rm = TRUE), big.mark = ","))
fact("med_pop_cb", format(median(base$pop[base$has_cb], na.rm = TRUE), big.mark = ","))
fact("med_pop_neither",
     format(median(base$pop[base$cat == "neither"], na.rm = TRUE), big.mark = ","))
# Stated in the closing paragraph, computed here so the sentence cannot drift
# away from the table above it.
fact("pop_ratio_cb_neither",
     sprintf("%.1f", median(base$pop[base$has_cb], na.rm = TRUE) /
                     median(base$pop[base$cat == "neither"], na.rm = TRUE)))

# ---------------------------------------------------------------------------
# 12. CONNECTICUT, the one place where the geography itself moved
# ---------------------------------------------------------------------------

ct <- base[substr(base$fips, 1, 2) == "09", ]
fact("ct_units", nrow(ct))
fact("ct_2024_missing", sum(is.na(ct$win2024)))
fact("ct_2016_missing", sum(is.na(ct$win2016)))
fact("ct_stores", sum(stores$state == "CT", na.rm = TRUE))

# Every county-year the panel is missing, by cause, so the brief can say how
# many rather than "some".
miss <- do.call(rbind, lapply(YEARS, function(y)
  data.frame(year = y, missing = sum(is.na(base[[paste0("win", y)]])))))
dd_write_csv(miss, "derived/missing.csv")

# ---------------------------------------------------------------------------
# 13. HEADLINE FIGURES FOR 2024
# ---------------------------------------------------------------------------

g24 <- gap[gap$year == 2024, ]
fact("wf24",  sprintf("%.0f", g24$wf_counties))
fact("cb24",  sprintf("%.0f", g24$cb_counties))
fact("gap24", sprintf("%.0f", g24$gap_counties))
fact("wf24v", sprintf("%.1f", g24$wf_votes))
fact("cb24v", sprintf("%.1f", g24$cb_votes))
fact("gap24v", sprintf("%.1f", g24$gap_votes))
fact("nat24v", sprintf("%.1f", g24$nat_votes))
g08 <- gap[gap$year == 2008, ]
fact("gap08", sprintf("%.0f", g08$gap_counties))
fact("gap08v", sprintf("%.1f", g08$gap_votes))

fw <- data.frame(key = names(FACTS), value = unlist(FACTS), row.names = NULL)
dd_write_csv(fw, "derived/facts.csv")

say("")
say("  %d Whole Foods, %d Cracker Barrel -> %d counties",
    nrow(wf), nrow(cb), nrow(base))
say("  2024: D carried %s%% of Whole Foods counties, %s%% of Cracker Barrel",
    FACTS$wf24, FACTS$cb24)
say("  gap on counties %s points, on votes %s points",
    FACTS$gap24, FACTS$gap24v)
say("  wrote %d files to derived/", length(list.files("derived")))
prov_report()

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
