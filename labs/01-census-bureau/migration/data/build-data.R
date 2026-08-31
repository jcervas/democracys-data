# build-data.R -- Democracy's Data, migration lab
# ---------------------------------------------------------------------------
# Reads five Census sources from data/raw/ and writes the small CSVs that the
# brief, the lab and the key all read. Nothing downstream re-parses a
# spreadsheet; nothing downstream invents a number.
#
# The raw files are large (the county-to-county workbook alone is 30 MB) and are
# NOT committed. Run get_raw() below, or the shell block in the header comment
# of each source section, to fetch them again.
#
# Packages used here only: readxl (spreadsheets), sf (state outlines).
# The .Rmd files that consume these CSVs use base R only.
#
# FETCHED 2026-08-10 from the URLs in URLS below. Sizes as fetched:
#   State_to_State_Migration_Table_2024_T13.xlsx           186 KB
#   State_of_Residence_By_Place_of_Birth_Table_2024_T13.xlsx 100 KB
#   CenPop2020_Mean_ST.txt                                   2 KB
#   tab-a-1.xls                                             49 KB
#   county-to-county-2016-2020-ins-outs-nets-gross.xlsx     29 MB
#   cb_2020_us_state_20m.zip                               184 KB
#
# WRITES (row counts as of that fetch; re-running prints them again):
#   derived/flows.csv         2,652  ordered state pairs (52 x 52 minus the diagonal)
#   derived/states.csv           52  50 states + DC + Puerto Rico
#   derived/arcs.csv            147  arcs for the two hub states, both directions
#   derived/map_states.csv      132  exterior rings, delta-encoded
#   derived/map_insets.csv        3  Alaska / Hawaii / Puerto Rico boxes
#   derived/mobility.csv         65  years of the CPS back-series, 1948-2020
#   derived/county_focus.csv     40  largest inflows to the focus county
#   derived/meta.csv             36  key/value pairs every .Rmd reads its prose from
#
# Re-running against the same raw files reproduces all eight byte for byte.
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


source("../../../_lib/precision.R")   # dd_signif(): six significant digits
dir.create("derived", showWarnings = FALSE)

suppressPackageStartupMessages({library(readxl); library(sf)})
options(stringsAsFactors = FALSE, scipen = 999)

FETCHED <- "2026-08-10"   # the day the five releases were pulled

HERE <- "."                       # run with data/ as the working directory
RAW  <- file.path(HERE, "raw")
dir.create(RAW, showWarnings = FALSE, recursive = TRUE)

URLS <- c(
  s2s      = "https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/2024/state-to-state-migration/State_to_State_Migration_Table_2024_T13.xlsx",
  pob      = "https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/2024/place-of-birth-acs-2024/State_of_Residence_By_Place_of_Birth_Table_2024_T13.xlsx",
  cenpop   = "https://www2.census.gov/geo/docs/reference/cenpop2020/CenPop2020_Mean_ST.txt",
  hist     = "https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/time-series/historic/tab-a-1.xls",
  c2c      = "https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/2020/county-to-county-migration-2016-2020/county-to-county-migration-flows/county-to-county-2016-2020-ins-outs-nets-gross.xlsx",
  cbstate  = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_20m.zip")

FILES <- c(s2s = "State_to_State_Migration_Table_2024_T13.xlsx",
           pob = "State_of_Residence_By_Place_of_Birth_Table_2024_T13.xlsx",
           cenpop = "CenPop2020_Mean_ST.txt", hist = "tab-a-1.xls",
           c2c = "county-to-county-2016-2020-ins-outs-nets-gross.xlsx",
           cbstate = "cb_2020_us_state_20m.zip")

get_raw <- function() {
  for (k in names(URLS)) {
    f <- file.path(RAW, FILES[[k]])
    if (!file.exists(f)) prov_fetch(URLS[[k]], f, mode = "wb", quiet = TRUE)
  }
  z <- file.path(RAW, FILES[["cbstate"]])
  if (!dir.exists(file.path(RAW, "cb_state"))) unzip(z, exdir = file.path(RAW, "cb_state"))
}
get_raw()

num <- function(x) suppressWarnings(as.numeric(x))   # "N" and "X" become NA

# ===========================================================================
# 1. STATE-TO-STATE FLOWS, ACS 2024 1-YEAR
# ===========================================================================
# The workbook's "tool_input" sheet is already long-format: one row per
# ordered pair. Columns 1-4 are the pair and its estimate; columns 6-10 carry
# the per-state in/out totals. Both are used.
#
# Two flag values matter and both become NA:
#   X  not applicable  -- the 52 diagonal cells (a state with itself)
#   N  "insufficient number of sample cases"  -- 279 pairs the ACS will not
#      report at all. These are not zeroes. Step 5 of the brief turns on this.

ti <- as.data.frame(read_excel(file.path(RAW, FILES[["s2s"]]), sheet = "tool_input",
                               col_names = FALSE, .name_repair = "minimal"))

fl <- data.frame(from_state = ti[-1, 1], to_state = ti[-1, 2],
                 est = num(ti[-1, 3]), moe = num(ti[-1, 4]),
                 flag = ifelse(ti[-1, 3] %in% c("N", "X"), ti[-1, 3], ""))
fl <- fl[!is.na(fl$from_state), ]
stopifnot(nrow(fl) == 52 * 52, sum(fl$flag == "X") == 52)
fl <- fl[fl$from_state != fl$to_state, ]              # drop the diagonal

# 90 percent margins of error (footnote 3 of the source table). A flow is
# "distinguishable from zero" when the lower bound of that interval clears
# zero, i.e. when the estimate exceeds its own margin of error.
fl$lo  <- pmax(0, fl$est - fl$moe)
fl$hi  <- fl$est + fl$moe
fl$sig <- !is.na(fl$est) & fl$est > fl$moe

# per-state totals, from the same sheet's second block
tot <- data.frame(state = ti[-1, 6], in_est = num(ti[-1, 7]), in_moe = num(ti[-1, 8]),
                  out_est = num(ti[-1, 9]), out_moe = num(ti[-1, 10]))
tot <- tot[!is.na(tot$state), ]
stopifnot(nrow(tot) == 52)

# ---- movers from abroad, from the full "Table" sheet ----------------------
# That sheet carries two origins the tool_input sheet drops: "U.S. Island
# Areas" and "foreign country". The second is the whole international story at
# this grain, and it is the only one the source gives us at state level.
tb <- as.data.frame(read_excel(file.path(RAW, FILES[["s2s"]]), sheet = "Table",
                               col_names = FALSE, .name_repair = "minimal"))
tb <- tb[9:nrow(tb), ]; names(tb) <- c("to_state", "from_state", "est", "moe")
tb <- tb[!is.na(tb$to_state) & !is.na(tb$from_state), ]
ab <- tb[tb$from_state == "Foreign country", ]
stopifnot(nrow(ab) == 52)
tot$foreignorigin_est <- num(ab$est)[match(tot$state, ab$to_state)]

# ---- population, movers within state, movers from abroad ------------------
# "Movers to current residence from abroad" is the Bureau's own published
# quantity and carries its own margin of error, so it is taken from the
# supplemental sheet rather than rebuilt. It is slightly LARGER than the
# "Foreign country" origin above, because it also counts arrivals from the U.S.
# Island Areas -- people who were already U.S. citizens or nationals. Both
# columns are kept so the brief can be exact about which it means.
sp <- as.data.frame(read_excel(file.path(RAW, FILES[["s2s"]]),
                               sheet = "Supplemental - Current Res",
                               col_names = FALSE, .name_repair = "minimal"))
sp <- sp[9:nrow(sp), ]
names(sp) <- c("state", "pop1", "pop1_moe", "same", "same_moe", "within",
               "within_moe", "fromstate", "fromstate_moe", "abroad", "abroad_moe")
sp <- sp[!is.na(sp$state), ]
US_POP1        <- num(sp$pop1[sp$state == "United States5"])
US_ABROAD      <- num(sp$abroad[sp$state == "United States5"])
US_FROMSTATE   <- num(sp$fromstate[sp$state == "United States5"])
US_WITHIN      <- num(sp$within[sp$state == "United States5"])
tot$pop1       <- num(sp$pop1)[match(tot$state, sp$state)]
tot$within_est <- num(sp$within)[match(tot$state, sp$state)]
tot$abroad_est <- num(sp$abroad)[match(tot$state, sp$state)]
tot$abroad_moe <- num(sp$abroad_moe)[match(tot$state, sp$state)]
stopifnot(!any(is.na(tot$pop1)), !any(is.na(tot$abroad_est)))

# net migration, and the margin of error of a difference. The Census rule for a
# derived difference is the root of the sum of the squared margins; it treats
# the two as independent, which for in-movers and out-movers is close enough
# and is what the Bureau itself instructs users to do.
tot$net     <- tot$in_est - tot$out_est
tot$net_moe <- sqrt(tot$in_moe^2 + tot$out_moe^2)
tot$net_sig <- abs(tot$net) > tot$net_moe
# How many of the 51 possible origins does each state have a *usable* inflow
# from? This is the quantity that decides whether an arc map of a given state is
# worth drawing at all, and it scales with the state's size -- which is why the
# map in Step 3 is honest for California and would not be for Wyoming.
sig_in_n  <- tapply(fl$sig, fl$to_state,   sum)
sig_out_n <- tapply(fl$sig, fl$from_state, sum)
tot$sig_in_n  <- as.vector(sig_in_n)[match(tot$state,  names(sig_in_n))]
tot$sig_out_n <- as.vector(sig_out_n)[match(tot$state, names(sig_out_n))]

tot$net_per1k    <- 1000 * tot$net / tot$pop1
tot$in_per1k     <- 1000 * tot$in_est / tot$pop1
tot$out_per1k    <- 1000 * tot$out_est / tot$pop1
tot$abroad_per1k <- 1000 * tot$abroad_est / tot$pop1

# ===========================================================================
# 2. PLACE OF BIRTH, ACS 2024
# ===========================================================================
pb <- as.data.frame(read_excel(file.path(RAW, FILES[["pob"]]), sheet = "Table",
                               col_names = FALSE, .name_repair = "minimal"))
pb <- pb[9:nrow(pb), ]; names(pb) <- c("res", "pob", "est", "moe")
pb <- pb[!is.na(pb$res) & !is.na(pb$pob), ]
pb$est <- num(pb$est); pb$moe <- num(pb$moe)

pbs <- as.data.frame(read_excel(file.path(RAW, FILES[["pob"]]),
                                sheet = "Supplemental Table",
                                col_names = FALSE, .name_repair = "minimal"))
pbs <- pbs[9:nrow(pbs), ]; names(pbs) <- c("res", "pop", "pop_moe", "us_est",
                                           "us_moe", "ab_est", "ab_moe")
pbs <- pbs[!is.na(pbs$res), ]
pbs$pop <- num(pbs$pop)

# EVERY component comes from the detailed table, not from the supplemental
# sheet's two-way split. The supplemental sheet's "place of birth abroad" column
# lumps Puerto Rico and the U.S. Island Areas in with foreign countries, and
# people born in Puerto Rico are U.S. citizens by birth. Using it would have put
# 564,000 Puerto Rico-born Floridians in the same bucket as immigrants.
pick <- function(p) { z <- pb[pb$pob == p, ]; z$est[match(tot$state, z$res)] }
born_here <- pb[pb$res == pb$pob, c("res", "est", "moe")]
names(born_here) <- c("state", "born_state_est", "born_state_moe")
tot <- merge(tot, born_here, by = "state", all.x = TRUE)
tot$pop_total    <- pbs$pop[match(tot$state, pbs$res)]
tot$born_pr_est     <- pick("Puerto Rico")
tot$born_island_est <- pick("U.S. Island Areas")
tot$born_foreign_est <- pick("Foreign country")
zf <- pb[pb$pob == "Foreign country", ]
tot$born_foreign_moe <- zf$moe[match(tot$state, zf$res)]
# born in a different U.S. state (or DC): the detailed table minus everything else
oth <- pb[pb$res != pb$pob & !(pb$pob %in% c("Foreign country", "U.S. Island Areas",
                                             "Puerto Rico")), ]
os  <- tapply(oth$est, oth$res, sum, na.rm = TRUE)
tot$born_otherstate_est <- as.vector(os)[match(tot$state, names(os))]
stopifnot(!any(is.na(tot$born_state_pct <- 100 * tot$born_state_est / tot$pop_total)))
# the five components must reconstruct the published total, to rounding. For
# Puerto Rico's own row born_pr_est IS born_state_est, so it is checked and then
# zeroed to stop it being counted twice as an outside origin.
# A few of the smallest components ("U.S. Island Areas" in a small state) are
# suppressed as "N" and arrive as NA. They are treated as zero HERE, for the
# reconstruction check only; the stored columns keep the NA, because "we do not
# know" and "nobody was born there" are different claims.
z0  <- function(x) ifelse(is.na(x), 0, x)
chk <- with(tot, z0(born_state_est) + z0(born_otherstate_est) +
              z0(born_island_est) + z0(born_foreign_est) +
              ifelse(state == "Puerto Rico", 0, z0(born_pr_est)))
stopifnot(max(abs(chk - tot$pop_total) / tot$pop_total) < 0.002)
tot$born_pr_est[tot$state == "Puerto Rico"] <- 0
tot$born_otherstate_pct <- 100 * tot$born_otherstate_est / tot$pop_total
tot$born_foreign_pct    <- 100 * tot$born_foreign_est    / tot$pop_total
tot$born_pr_pct         <- 100 * tot$born_pr_est         / tot$pop_total
stopifnot(!any(is.na(tot$born_foreign_pct)))

# 50 states + DC. Puerto Rico is in the flow data and on the map, but it is not
# a state and its place-of-birth categories are not comparable, so it is flagged
# rather than silently ranked alongside them.
tot$is_state <- tot$state != "Puerto Rico"

US_POP_TOTAL <- pbs$pop[pbs$res == "United States5"]
US_BORN_STATE_PCT <- 100 * sum(tot$born_state_est[tot$is_state]) / US_POP_TOTAL
US_FOREIGN_PCT    <- 100 * sum(tot$born_foreign_est[tot$is_state]) / US_POP_TOTAL

# ===========================================================================
# 3. STATE CENTROIDS AND MAP FRAME
# ===========================================================================
# Population-weighted mean centre of each state, 2020 Census. The arc endpoints
# are these, not geometric centroids: an arc into California should land where
# Californians are, not in the middle of the Mojave.
cp <- read.csv(file.path(RAW, FILES[["cenpop"]]), colClasses = c(STATEFP = "character"))
names(cp) <- tolower(names(cp))
cp$latitude <- num(cp$latitude); cp$longitude <- num(cp$longitude)

# Albers equal-area conic, written out because the .Rmd files may not have any
# spatial package and the build must be reproducible from base arithmetic.
albers <- function(lon, lat, lon0, lat0, p1, p2) {
  d <- pi / 180
  n <- (sin(p1 * d) + sin(p2 * d)) / 2
  C <- cos(p1 * d)^2 + 2 * n * sin(p1 * d)
  rho0 <- sqrt(C - 2 * n * sin(lat0 * d)) / n
  rho  <- sqrt(C - 2 * n * sin(lat * d)) / n
  th   <- n * (lon - lon0) * d
  list(x = rho * sin(th), y = rho0 - rho * cos(th))
}
PROJ <- list(conus = list(lon0 = -96,  lat0 = 37.5, p1 = 29.5, p2 = 45.5),
             ak    = list(lon0 = -152, lat0 = 60,   p1 = 55,   p2 = 65),
             hi    = list(lon0 = -157, lat0 = 20,   p1 = 8,    p2 = 18),
             pr    = list(lon0 = -66,  lat0 = 18,   p1 = 17,   p2 = 19))
piece_of <- function(fips) ifelse(fips == "02", "ak",
                          ifelse(fips == "15", "hi",
                          ifelse(fips == "72", "pr", "conus")))

# ---- state outlines, simplified ------------------------------------------
sh <- st_read(file.path(RAW, "cb_state"), quiet = TRUE)
sh <- sh[sh$STATEFP %in% c(sprintf("%02d", 1:56), "72"), ]
sh <- st_transform(sh, 4326)
sh <- st_simplify(sh, dTolerance = 0.06, preserveTopology = TRUE)

rings_of <- function(g) {
  g <- st_geometry(g)[[1]]
  if (inherits(g, "POLYGON")) g <- list(g)
  out <- list()
  for (p in g) out <- c(out, list(p[[1]]))       # exterior ring only
  out
}

FRAME_W <- 1000; FRAME_H <- 620
# where each projected piece is placed inside the frame (x, y, w, h), y up
BOX <- list(conus = c(30, 78, 940, 520),
            ak    = c(18,  30, 210, 150),
            hi    = c(250, 30, 105,  62),
            pr    = c(392, 30,  86,  46))

# collect every ring of every state, in its own projection, then fit each
# piece's rings into its box with one shared scale so shapes are not distorted
rr <- list()
for (i in seq_len(nrow(sh))) {
  fp <- sh$STATEFP[i]; pc <- piece_of(fp); P <- PROJ[[pc]]
  for (rg in rings_of(sh[i, ])) {
    lon <- rg[, 1]
    # The Aleutians run past 180 degrees east and come back as positive
    # longitudes. Left alone they project to the far side of the world, blow up
    # Alaska's bounding box and leave a smear across the inset. Unwrap them.
    if (pc == "ak") lon[lon > 0] <- lon[lon > 0] - 360
    z <- albers(lon, rg[, 2], P$lon0, P$lat0, P$p1, P$p2)
    rr[[length(rr) + 1]] <- list(fips = fp, piece = pc, x = z$x, y = z$y)
  }
}
fitpar <- list()
for (pc in names(BOX)) {
  k <- vapply(rr, function(z) z$piece == pc, logical(1))
  xs <- unlist(lapply(rr[k], `[[`, "x")); ys <- unlist(lapply(rr[k], `[[`, "y"))
  b  <- BOX[[pc]]
  s  <- min(b[3] / diff(range(xs)), b[4] / diff(range(ys)))
  fitpar[[pc]] <- list(s = s, x0 = min(xs), y0 = min(ys),
                       ox = b[1] + (b[3] - diff(range(xs)) * s) / 2,
                       oy = b[2] + (b[4] - diff(range(ys)) * s) / 2)
}
# NOTE the y flip: Albers y grows north, and both renderers here draw with y up,
# so no flip is applied. The HTML renderer flips once, at draw time.
to_frame <- function(pc, x, y) {
  f <- fitpar[[pc]]
  list(x = f$ox + (x - f$x0) * f$s, y = f$oy + (y - f$y0) * f$s)
}

# integer-delta encoding, as in the residual-votes lab: "x0 y0 dx dy dx dy ..."
enc <- function(x, y) {
  xi <- as.integer(round(x)); yi <- as.integer(round(y))
  k <- c(TRUE, diff(xi) != 0 | diff(yi) != 0)
  xi <- xi[k]; yi <- yi[k]
  paste(c(rbind(c(xi[1], diff(xi)), c(yi[1], diff(yi)))), collapse = " ")
}
map_rows <- do.call(rbind, lapply(rr, function(z) {
  f <- to_frame(z$piece, z$x, z$y)
  if (length(f$x) < 4) return(NULL)
  data.frame(fips = z$fips, piece = z$piece, pts = enc(f$x, f$y))
}))

# ---- centroids into the same frame ---------------------------------------
cp$piece <- piece_of(cp$statefp)
cxy <- do.call(rbind, lapply(seq_len(nrow(cp)), function(i) {
  P <- PROJ[[cp$piece[i]]]
  z <- albers(cp$longitude[i], cp$latitude[i], P$lon0, P$lat0, P$p1, P$p2)
  f <- to_frame(cp$piece[i], z$x, z$y)
  data.frame(state = cp$stname[i], fips = cp$statefp[i], piece = cp$piece[i],
             lat = cp$latitude[i], lon = cp$longitude[i],
             mx = f$x, my = f$y)
}))
stopifnot(nrow(cxy) == 52, !anyDuplicated(cxy$state))   # 50 states + DC + PR
tot <- merge(tot, cxy[, c("state", "fips", "piece", "lat", "lon", "mx", "my")],
             by = "state", all.x = TRUE)
stopifnot(!any(is.na(tot$mx)))

# ===========================================================================
# 4. THE ARC MAP: pick the focus state FROM THE DATA
# ===========================================================================
# The state worth drawing is the one whose in and out flows differ most -- the
# most asymmetric mover, in absolute people. Chosen by the data, not by taste.
FOCUS <- tot$state[which.max(abs(tot$net))]
# And the state whose map is LEAST drawable: fewest inflows that clear their own
# margin of error. Step 6 draws this one beside the focus state, because the
# contrast is the argument.
CONTRAST <- tot$state[tot$is_state][which.min(tot$sig_in_n[tot$is_state])]

arc_rows <- function(focus, dir) {
  d <- if (dir == "in") fl[fl$to_state == focus, ]
       else             fl[fl$from_state == focus, ]
  d$other <- if (dir == "in") d$from_state else d$to_state
  d <- d[!is.na(d$est) & d$est > 0, ]
  o <- tot[match(d$other, tot$state), c("mx", "my")]
  h <- tot[match(focus,   tot$state), c("mx", "my")]
  # the arc runs origin -> destination, so "in" flows point at the focus state
  if (dir == "in") { x0 <- o$mx; y0 <- o$my; x1 <- h$mx; y1 <- h$my }
  else             { x0 <- h$mx; y0 <- h$my; x1 <- o$mx; y1 <- o$my }
  # quadratic Bezier: control point at the chord midpoint pushed perpendicular,
  # always to the same side of the direction of travel, so the arcs bundle
  dx <- x1 - x0; dy <- y1 - y0; L <- sqrt(dx^2 + dy^2)
  k  <- 0.20
  cx <- (x0 + x1) / 2 - dy / pmax(L, 1) * L * k
  cy <- (y0 + y1) / 2 + dx / pmax(L, 1) * L * k
  data.frame(hub = focus, dir = dir, other = d$other, est = d$est, moe = d$moe,
             sig = d$sig,
             x0 = round(x0, 1), y0 = round(y0, 1), cx = round(cx, 1),
             cy = round(cy, 1), x1 = round(x1, 1), y1 = round(y1, 1))
}
arcs <- rbind(arc_rows(FOCUS, "in"),    arc_rows(FOCUS, "out"),
              arc_rows(CONTRAST, "in"), arc_rows(CONTRAST, "out"))
arcs <- arcs[order(arcs$hub, arcs$dir, arcs$est), ] # small first, big on top

# ===========================================================================
# 5. HISTORICAL MOBILITY RATES (CPS, 1948-)
# ===========================================================================
# A DIFFERENT INSTRUMENT from everything above: the Current Population Survey,
# not the ACS. It is the only source that reaches back past 2005, and the two
# are not interchangeable. The brief says so where the figure appears.
hs <- as.data.frame(read_excel(file.path(RAW, FILES[["hist"]]), col_names = FALSE,
                               .name_repair = "minimal"))
pct_start <- which(hs[[1]] == "PERCENT")[1]
hp <- hs[(pct_start + 1):nrow(hs), 1:10]
names(hp) <- c("period", "total", "same_res", "movers", "diff_res", "same_county",
               "diff_county", "dc_total", "diff_state", "abroad")
hp <- hp[!is.na(hp$period) & grepl("^[12][0-9]{3}-", hp$period), ]
hp$year <- as.integer(substr(hp$period, 6, 9))       # the later of the two years
for (v in c("movers", "same_county", "dc_total", "diff_state", "abroad"))
  hp[[v]] <- num(hp[[v]])
hp <- hp[!is.na(hp$movers) & !is.na(hp$diff_state), ]
hp <- hp[order(hp$year), ]
hp$same_state_diff_county <- hp$dc_total
mob <- hp[, c("year", "period", "movers", "same_county",
              "same_state_diff_county", "diff_state", "abroad")]
mob <- mob[!duplicated(mob$year), ]

# ===========================================================================
# 6. COUNTY-TO-COUNTY CLOSE-UP
# ===========================================================================
# One sheet of the workbook, for the focus state. County-to-county is a FIVE
# YEAR ACS estimate (2016-2020); everything above is a ONE YEAR estimate
# (2024). They are different instruments over different windows and the brief
# refuses to put them on the same axis.
c2c <- as.data.frame(read_excel(file.path(RAW, FILES[["c2c"]]), sheet = FOCUS,
                                col_names = FALSE, .name_repair = "minimal",
                                skip = 3))
names(c2c) <- c("st_b", "cty_b", "st_a", "cty_a", "state_b", "county_b",
                "state_a", "county_a", "flow", "flow_moe", "counter",
                "counter_moe", "net", "net_moe", "gross", "gross_moe")
c2c <- c2c[!is.na(c2c$county_b), ]
for (v in c("flow", "flow_moe", "counter", "counter_moe", "net", "net_moe"))
  c2c[[v]] <- num(c2c[[v]])
# the county of the focus state with the largest gross migration, again chosen
# by the data
gr <- tapply(num(c2c$gross), c2c$county_b, sum, na.rm = TRUE)
FOCUS_COUNTY <- names(gr)[which.max(gr)]
cf <- c2c[c2c$county_b == FOCUS_COUNTY, ]
# "Counterflow" is movement INTO county B from the other county; "Flow" is
# movement OUT of B to it. (Source header: "Flow from Geography B to Geography
# A".) Keep both, named for what they are.
cf$into_b   <- cf$counter;     cf$into_b_moe   <- cf$counter_moe
cf$out_of_b <- cf$flow;        cf$out_of_b_moe <- cf$flow_moe
# A blank margin of error in this file accompanies an estimate of zero. Neither
# a missing margin nor a zero estimate can clear its own interval, so both fail.
cf$sig_in   <- !is.na(cf$into_b) & !is.na(cf$into_b_moe) & cf$into_b > cf$into_b_moe
cf <- cf[!is.na(cf$into_b), ]
cfo <- cf[order(-cf$into_b),
          c("state_a", "county_a", "into_b", "into_b_moe", "sig_in",
            "out_of_b", "out_of_b_moe")]
county_focus <- head(cfo, 40)

# ===========================================================================
# 7. WRITE
# ===========================================================================
W <- function(d, f) write.csv(dd_signif(d), file.path(HERE, f), row.names = FALSE, na = "")

W(fl[, c("from_state", "to_state", "est", "moe", "lo", "hi", "sig", "flag")],
  "derived/flows.csv")
W(tot[order(tot$state), c("state", "fips", "piece", "is_state", "pop1", "pop_total",
      "in_est", "in_moe", "out_est", "out_moe", "net", "net_moe", "net_sig",
      "in_per1k", "out_per1k", "net_per1k", "sig_in_n", "sig_out_n",
      "within_est", "abroad_est", "abroad_moe", "abroad_per1k", "foreignorigin_est",
      "born_state_est", "born_state_moe", "born_state_pct",
      "born_otherstate_est", "born_otherstate_pct",
      "born_foreign_est", "born_foreign_moe", "born_foreign_pct",
      "born_pr_est", "born_pr_pct", "born_island_est",
      "lat", "lon", "mx", "my")], "derived/states.csv")
W(arcs, "derived/arcs.csv")
W(map_rows, "derived/map_states.csv")
W(mob, "derived/mobility.csv")
W(county_focus, "derived/county_focus.csv")
# the three inset boxes, so both renderers can outline and label them
W(do.call(rbind, lapply(c("ak", "hi", "pr"), function(p) {
  k <- vapply(rr, function(z) z$piece == p, logical(1))
  xy <- lapply(rr[k], function(z) to_frame(p, z$x, z$y))
  data.frame(piece = p,
             label = c(ak = "Alaska", hi = "Hawaii", pr = "Puerto Rico")[[p]],
             x0 = floor(min(unlist(lapply(xy, `[[`, "x")))) - 8,
             x1 = ceiling(max(unlist(lapply(xy, `[[`, "x")))) + 8,
             y0 = floor(min(unlist(lapply(xy, `[[`, "y")))) - 8,
             y1 = ceiling(max(unlist(lapply(xy, `[[`, "y")))) + 8)
})), "derived/map_insets.csv")

nsig  <- sum(fl$sig); nrep <- sum(!is.na(fl$est)); npair <- nrow(fl)
nfail <- npair - nsig                       # suppressed, zero, or not distinguishable
volf  <- 100 * sum(fl$est[!fl$sig], na.rm = TRUE) / sum(fl$est, na.rm = TRUE)
mt <- data.frame(key = c(
  "focus_state", "contrast_state", "focus_county", "frame_w", "frame_h",
  "n_pairs", "n_reported", "n_suppressed", "n_sig", "n_fail", "n_zero_est",
  "pct_sig_of_pairs", "pct_fail_of_pairs", "pct_volume_in_failed_pairs",
  "median_est_sig", "median_est_fail", "largest_fail_est", "largest_fail_moe",
  "largest_fail_pair",
  "us_pop1", "us_pop_total", "us_movers_between_states",
  "us_movers_within_state", "us_movers_from_abroad",
  "us_born_state_pct", "us_foreign_pct",
  "n_states_net_sig", "acs_year", "c2c_window", "cps_first", "cps_last",
  "county_pairs_shown", "county_pairs_sig", "county_total_pairs",
  "county_all_sig", "county_all_pairs",
  # the day the five releases were fetched, so the chapter can state it without
  # reading this file to find out
  "fetched"),
  value = c(
  FOCUS, CONTRAST, FOCUS_COUNTY, FRAME_W, FRAME_H,
  npair, nrep, sum(is.na(fl$est)), nsig, nfail,
  sum(!is.na(fl$est) & fl$est == 0),
  round(100 * nsig / npair, 1), round(100 * nfail / npair, 1), round(volf, 2),
  round(median(fl$est[fl$sig])), round(median(fl$est[!fl$sig], na.rm = TRUE)),
  max(fl$est[!fl$sig], na.rm = TRUE),
  fl$moe[!fl$sig][which.max(fl$est[!fl$sig])],
  local({ i <- which(!fl$sig)[which.max(fl$est[!fl$sig])]
          paste(fl$from_state[i], "to", fl$to_state[i]) }),
  US_POP1, US_POP_TOTAL, US_FROMSTATE, US_WITHIN, US_ABROAD,
  round(US_BORN_STATE_PCT, 1), round(US_FOREIGN_PCT, 1),
  sum(tot$net_sig[tot$is_state]), 2024, "2016-2020", min(mob$year), max(mob$year),
  nrow(county_focus), sum(county_focus$sig_in), nrow(cfo),
  sum(cfo$sig_in), nrow(cfo), FETCHED))
W(mt, "derived/meta.csv")

cat("focus state :", FOCUS, " contrast:", CONTRAST, "\n")
cat("focus county:", FOCUS_COUNTY, "\n")
cat("pairs", npair, " reported", nrep, " significant", nsig,
    sprintf(" (%.1f%% of reported)\n", 100 * nsig / nrep))
cat("wrote:", paste(c("flows", "states", "arcs", "map_states", "mobility",
                      "county_focus", "meta"), collapse = ", "), "\n")

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
