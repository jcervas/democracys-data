# ---------------------------------------------------------------------------
# build-data.R — regional shift lab
#
# ONE source file, and it is the good kind: the Census Bureau's own
# apportionment time series, 1910-2020, in a single CSV with no key required.
#
#   https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/
#     apportionment.csv
#
# 684 data rows. Three kinds of row, distinguished by `Geography Type`:
#   State  (52 per year: 50 states + DC + Puerto Rico)
#   Region (4 per year:  the Bureau's own four-region totals)
#   Nation (1 per year)
#
# THREE THINGS THIS SCRIPT HAS TO SURVIVE
#
# 1. Every numeric column is a QUOTED STRING WITH THOUSANDS SEPARATORS
#    ("2,138,093"). read.csv gives you character. as.numeric() on it gives
#    you NA, silently, for every value above 999. Strip the commas first.
#
# 2. Alaska and Hawaii are in the file from 1910, with BLANK representative
#    counts until 1960. They were territories. Any 50-state seat analysis
#    that starts before 1960 is comparing different countries, which is why
#    this lab's seat window is 1960-2020 and says so.
#
# 3. Puerto Rico is a State-type row and is NOT in the national total, and
#    DC is in the national total but has no representative. Summing the
#    State rows without thinking gives you a country with 3.3 million extra
#    people in 2020.
#
# The script checks all three rather than trusting them: it reconstructs the
# Bureau's own Region and Nation rows from the State rows and stops if they
# disagree.
#
# OUTPUTS
#   derived/states.csv     one row per state-year: population, seats, region, division,
#                  and the three competing "South" flags
#   derived/regions.csv    one row per region-year: population, share of nation, seats
#   derived/nation.csv     one row per year: population, House size, people per seat
#   derived/seatchange.csv one row per state: seats 1960, seats 2020, cumulative change
#   derived/southdefs.csv  one row per year x South definition: share of national pop
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_signif(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

URL <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/data/",
              "apportionment/apportionment.csv")

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
  raw <- read.csv(prov_fetch(URL, "raw/apportionment_raw.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
} else {
  if (!file.exists("raw/apportionment_raw.csv"))
    prov_fetch(URL, "raw/apportionment_raw.csv", quiet = TRUE)
  raw <- read.csv("raw/apportionment_raw.csv", stringsAsFactors = FALSE,
                  check.names = FALSE)
}

message("raw rows: ", nrow(raw))

# ---- 1. de-comma -----------------------------------------------------------
num <- function(x) as.numeric(gsub(",", "", trimws(as.character(x))))

d <- data.frame(
  name    = trimws(raw[["Name"]]),
  gtype   = trimws(raw[["Geography Type"]]),
  year    = num(raw[["Year"]]),
  pop     = num(raw[["Resident Population"]]),
  pctchg  = num(raw[["Percent Change in Resident Population"]]),
  reps    = num(raw[["Number of Representatives"]]),
  repchg  = num(raw[["Change in Number of Representatives"]]),
  pop_per_rep = num(raw[["Average Apportionment Population Per Representative"]]),
  stringsAsFactors = FALSE)

# the de-comma check: nothing above 999 may have gone missing
stopifnot(!any(is.na(d$pop)))
stopifnot(max(d$pop) > 3e8)

# ---- 2. region and division lookup ----------------------------------------
# U.S. Census Bureau, Statistical Groupings of States and Counties
# (Geographic Areas Reference Manual, ch. 6). Four regions, nine divisions.
NE_NEW <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island",
            "Vermont")
NE_MID <- c("New Jersey","New York","Pennsylvania")
MW_ENC <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
MW_WNC <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota",
            "South Dakota")
S_SA   <- c("Delaware","District of Columbia","Florida","Georgia","Maryland",
            "North Carolina","South Carolina","Virginia","West Virginia")
S_ESC  <- c("Alabama","Kentucky","Mississippi","Tennessee")
S_WSC  <- c("Arkansas","Louisiana","Oklahoma","Texas")
W_MTN  <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah",
            "Wyoming")
W_PAC  <- c("Alaska","California","Hawaii","Oregon","Washington")

divs <- list("New England" = NE_NEW, "Middle Atlantic" = NE_MID,
             "East North Central" = MW_ENC, "West North Central" = MW_WNC,
             "South Atlantic" = S_SA, "East South Central" = S_ESC,
             "West South Central" = S_WSC, "Mountain" = W_MTN, "Pacific" = W_PAC)
div_of <- unlist(lapply(names(divs), function(k)
  setNames(rep(k, length(divs[[k]])), divs[[k]])))
reg_of <- c(setNames(rep("Northeast", length(c(NE_NEW, NE_MID))), c(NE_NEW, NE_MID)),
            setNames(rep("Midwest",   length(c(MW_ENC, MW_WNC))), c(MW_ENC, MW_WNC)),
            setNames(rep("South",     length(c(S_SA, S_ESC, S_WSC))), c(S_SA, S_ESC, S_WSC)),
            setNames(rep("West",      length(c(W_MTN, W_PAC))), c(W_MTN, W_PAC)))

# the three competing definitions of "the South"
CONFED <- c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi",
            "North Carolina","South Carolina","Tennessee","Texas","Virginia")
CENSUS_SOUTH <- names(reg_of)[reg_of == "South"]                 # 16 states + DC
SUNBELT <- c(CENSUS_SOUTH, names(reg_of)[reg_of == "West"])      # South + West
# the states the two narrower definitions disagree about
BORDER <- setdiff(CENSUS_SOUTH, CONFED)

st <- d[d$gtype == "State" & d$name != "Puerto Rico", ]
stopifnot(length(unique(st$name)) == 51)   # 50 states + DC

st$region   <- unname(reg_of[st$name])
st$division <- unname(div_of[st$name])
stopifnot(!any(is.na(st$region)))

st$south_census  <- st$name %in% CENSUS_SOUTH
st$south_confed  <- st$name %in% CONFED
st$sunbelt       <- st$name %in% SUNBELT
st$border_south  <- st$name %in% BORDER

# ---- 3. reconstruct the Bureau's own Region and Nation rows ----------------
# If our state->region map is right, these must reproduce the file exactly.
mine <- aggregate(pop ~ region + year, st, sum)
theirs <- d[d$gtype == "Region", c("name", "year", "pop")]
theirs$region <- sub(" Region$", "", theirs$name)
chk <- merge(mine, theirs[, c("region", "year", "pop")], by = c("region", "year"),
             suffixes = c("_ours", "_bureau"))
bad <- chk[chk$pop_ours != chk$pop_bureau, ]
if (nrow(bad)) { print(bad); stop("region map disagrees with the Bureau") }
message("region check: ", nrow(chk), " region-years reproduce the Bureau exactly")

nat <- d[d$gtype == "Nation", c("year", "pop", "reps", "pop_per_rep", "pctchg")]
natmine <- aggregate(pop ~ year, st, sum)
stopifnot(all(natmine$pop[order(natmine$year)] == nat$pop[order(nat$year)]))
message("nation check: state rows (minus Puerto Rico, including DC) sum to the ",
        "published national total in all ", nrow(nat), " years")

# seats: the state rows must sum to the published House size
sr <- aggregate(reps ~ year, st, function(x) sum(x, na.rm = TRUE))
seatchk <- merge(sr, nat[, c("year", "reps")], by = "year", suffixes = c("_states", "_nation"))
print(seatchk)

# ---- 4. the 1960-2020 window ----------------------------------------------
# Alaska and Hawaii have no representatives before 1960; the 50-state House
# only exists from then. Recorded rather than assumed:
ah <- st[st$name %in% c("Alaska", "Hawaii"), c("name", "year", "reps")]
message("Alaska/Hawaii year-rows with no representative count: ",
        sum(is.na(ah$reps)), " of ", nrow(ah),
        " (last such year: ", max(ah$year[is.na(ah$reps)]), ")")

Y1 <- 1960; Y2 <- 2020
w <- st[st$year %in% c(Y1, Y2) & st$name != "District of Columbia", ]
s1 <- w[w$year == Y1, ]; s2 <- w[w$year == Y2, ]
sc <- merge(s1[, c("name", "region", "division", "reps", "pop",
                   "south_census", "south_confed", "sunbelt")],
            s2[, c("name", "reps", "pop")], by = "name",
            suffixes = c("_1960", "_2020"))
sc$change <- sc$reps_2020 - sc$reps_1960
sc$rank_1960 <- rank(-sc$reps_1960, ties.method = "min")
sc$rank_2020 <- rank(-sc$reps_2020, ties.method = "min")
sc$pop_growth <- 100 * (sc$pop_2020 / sc$pop_1960 - 1)
sc <- sc[order(-sc$change, -sc$reps_2020), ]
stopifnot(sum(sc$reps_1960) == 435, sum(sc$reps_2020) == 435)
stopifnot(sum(sc$change) == 0)

# ---- 5. regional shares ----------------------------------------------------
reg <- aggregate(cbind(pop) ~ region + year, st, sum)
reg$reps <- aggregate(reps ~ region + year, st, function(x) sum(x, na.rm = TRUE))$reps
tot <- setNames(nat$pop, nat$year)
reg$share <- 100 * reg$pop / tot[as.character(reg$year)]
reg$seat_share <- 100 * reg$reps / setNames(nat$reps, nat$year)[as.character(reg$year)]
reg <- reg[order(reg$region, reg$year), ]

# ---- 6. the sensitivity table ---------------------------------------------
defs <- list(
  "Census South (16 states + DC)" = CENSUS_SOUTH,
  "Confederate South (11 states)" = CONFED,
  "Sun Belt (South + West)"       = SUNBELT,
  "Border South only (DC, DE, KY, MD, OK, WV)" = BORDER,
  "Northeast"                     = names(reg_of)[reg_of == "Northeast"])
sd <- do.call(rbind, lapply(names(defs), function(k) {
  sel <- st[st$name %in% defs[[k]], ]
  a <- aggregate(pop ~ year, sel, sum)
  data.frame(definition = k, year = a$year, pop = a$pop,
             share = 100 * a$pop / tot[as.character(a$year)],
             n_states = length(defs[[k]]), stringsAsFactors = FALSE)
}))
sd <- sd[order(sd$definition, sd$year), ]

# ---- 7. write --------------------------------------------------------------
wr <- function(x, f) { write.csv(dd_signif(x), f, row.names = FALSE); message("wrote ", f,
                                 " (", nrow(x), " rows)") }
wr(st[order(st$name, st$year),
      c("name","year","pop","pctchg","reps","repchg","region","division",
        "south_census","south_confed","sunbelt","border_south")], "derived/states.csv")
wr(reg, "derived/regions.csv")
wr(nat[order(nat$year), ], "derived/nation.csv")
wr(sc, "derived/seatchange.csv")
wr(sd, "derived/southdefs.csv")

if (exists("prov_report")) prov_report()

# ---- 8. map geometry -------------------------------------------------------
# The brief's map of regions and divisions needs state outlines, and they are
# their own Census product: the 2020 cartographic boundary file at
# 1:20,000,000, the generalized, shoreline-clipped edition the Bureau draws its
# own national maps from. (The TIGER/Line state file is the LEGAL boundary; it
# puts Michigan halfway across Lake Michigan.) One zip, no key.
MAPURL <- paste0("https://www2.census.gov/geo/tiger/GENZ2020/shp/",
                 "cb_2020_us_state_20m.zip")
if (!file.exists("raw/cb_2020_us_state_20m.zip"))
  prov_fetch(MAPURL, "raw/cb_2020_us_state_20m.zip")
unzip("raw/cb_2020_us_state_20m.zip", exdir = "raw/cb_2020_us_state_20m",
      overwrite = TRUE)

suppressPackageStartupMessages(library(sf))
g <- st_read("raw/cb_2020_us_state_20m/cb_2020_us_state_20m.shp", quiet = TRUE)
g <- g[g$NAME %in% names(reg_of), c("NAME", "STUSPS")]
stopifnot(nrow(g) == 51)   # 50 states + DC; Puerto Rico and territories drop out
# Halve the vertex count again (topology-aware, shared borders stay shared):
# at the width these figures print at, the difference is invisible, and the
# self-contained HTML carries every vertex twice (once per format).
g <- rmapshaper::ms_simplify(g, keep = 0.5, keep_shapes = TRUE)

# The composite layout of every published national map: the lower 48 on the
# national Albers equal-area (EPSG:5070), Alaska and Hawaii projected on their
# own Albers and parked below the Southwest, Alaska at reduced scale.
conus <- st_transform(g[!g$NAME %in% c("Alaska", "Hawaii"), ], 5070)
ak    <- st_transform(g[g$NAME == "Alaska", ], 3338)
hi    <- st_transform(g[g$NAME == "Hawaii", ], paste(
  "+proj=aea +lat_1=19 +lat_2=24 +lat_0=20 +lon_0=-157 +datum=NAD83 +units=m"))

bbc <- st_bbox(conus)
BW <- as.numeric(bbc["xmax"] - bbc["xmin"])
BH <- as.numeric(bbc["ymax"] - bbc["ymin"])
park <- function(x, s, cx, cy) {
  geo <- st_geometry(x)
  ctr <- as.numeric(st_coordinates(st_centroid(st_union(geo))))
  st_geometry(x) <- (geo - ctr) * s + c(cx, cy)
  st_crs(x) <- NA
  x
}
ak <- park(ak, 0.35, bbc["xmin"] + 0.13 * BW, bbc["ymin"] - 0.02 * BH)
hi <- park(hi, 1.00, bbc["xmin"] + 0.35 * BW, bbc["ymin"] - 0.06 * BH)
st_crs(conus) <- NA
usa <- st_cast(rbind(conus, ak, hi), "MULTIPOLYGON")

# Flatten to drawing coordinates: width 760, y growing DOWNWARD (SVG order),
# so the interactive and the printed figure trace identical shapes.
fb <- st_bbox(usa)
SW <- 760
SC <- SW / as.numeric(fb["xmax"] - fb["xmin"])
SH <- as.numeric(fb["ymax"] - fb["ymin"]) * SC
svg_xy <- function(m) cbind(x = (m[, 1] - as.numeric(fb["xmin"])) * SC,
                            y = (as.numeric(fb["ymax"]) - m[, 2]) * SC)

mp <- do.call(rbind, lapply(seq_len(nrow(usa)), function(i) {
  m <- st_coordinates(usa[i, ])
  data.frame(name  = usa$NAME[i], abbr = usa$STUSPS[i],
             piece = as.integer(interaction(m[, "L2"], m[, "L1"], drop = TRUE)),
             svg_xy(m), stringsAsFactors = FALSE)
}))
mp$region   <- unname(reg_of[mp$name])
mp$division <- unname(div_of[mp$name])
stopifnot(!any(is.na(mp$region)), !any(is.na(mp$division)),
          length(unique(mp$name)) == 51)

# Division outlines: dissolve the states of each division and keep the border
# of the union. Drawn heavier than state lines, exactly as on the Bureau's map.
dvl <- do.call(rbind, lapply(names(divs), function(k) {
  u <- st_union(st_geometry(usa)[usa$NAME %in% divs[[k]]])
  m <- st_coordinates(st_cast(u, "MULTILINESTRING"))
  data.frame(division = k, piece = as.integer(m[, "L1"]),
             svg_xy(m), stringsAsFactors = FALSE)
}))
stopifnot(length(unique(dvl$division)) == 9)

# Label anchors: a point inside every state, plus one inside every region.
pos <- st_point_on_surface(st_geometry(usa))
lab <- data.frame(kind = "state", name = usa$NAME, abbr = usa$STUSPS,
                  region = unname(reg_of[usa$NAME]),
                  svg_xy(st_coordinates(pos)),
                  area = as.numeric(st_area(usa)) * SC^2,
                  stringsAsFactors = FALSE)
rlab <- do.call(rbind, lapply(unique(lab$region), function(r) {
  u <- st_point_on_surface(st_union(st_geometry(usa)[reg_of[usa$NAME] == r &
                                                     !usa$NAME %in% c("Alaska", "Hawaii")]))
  data.frame(kind = "region", name = r, abbr = "", region = r,
             svg_xy(st_coordinates(u)), area = NA, stringsAsFactors = FALSE)
}))
lab <- rbind(lab, rlab)

message("map: ", nrow(mp), " boundary points, height ", round(SH), " at width ", SW)
wr(mp[, c("name", "abbr", "region", "division", "piece", "x", "y")],
   "derived/statemap.csv")
wr(dvl, "derived/divmap.csv")
wr(lab, "derived/maplabels.csv")

# ---- 9. the headline numbers, printed so the build cannot lie -------------
cat("\n--- cumulative seat change 1960-2020, by region ---\n")
print(tapply(sc$change, sc$region, sum))
cat("\n--- biggest gainers and losers ---\n")
print(head(sc[, c("name","region","reps_1960","reps_2020","change")], 6))
print(tail(sc[, c("name","region","reps_1960","reps_2020","change")], 6))
cat("\n--- regional population share, 1960 vs 2020 ---\n")
print(reshape(reg[reg$year %in% c(1960, 2020), c("region","year","share")],
              idvar = "region", timevar = "year", direction = "wide"))
cat("\n--- people per representative ---\n")
print(nat[, c("year","reps","pop_per_rep")])

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
