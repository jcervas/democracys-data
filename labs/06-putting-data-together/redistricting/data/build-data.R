# ---------------------------------------------------------------------------
# Build the redistricting dataset.
#
# Three files end up in this folder:
#
#   derived/pres_by_cd_2024.csv   the 2024 presidential vote in each of the 435
#                         congressional districts, with the party that won the
#                         House seat there
#   derived/seat_rings.csv        the House-seat cartogram outlines, from the
#                         shared base map in labs/_lib/geo
#   derived/seat_states.csv       one row per state: seats and label anchor
#   derived/deviation.csv         how equal in population the 2010-cycle plans
#                         were, one row per kind of plan
#   derived/deviation_states.csv  the same, per state
#
# Run this script from inside the data/ folder. It needs a network connection;
# the committed output means the lab does not.
#
# ONE SOURCE IS KEYED IN AND NEEDS NO NETWORK. raw/ncsl-appendix-c.tsv is
# Appendix C of the National Conference of State Legislatures' "Redistricting
# Law 2020" (Denver: NCSL, October 2019), transcribed: for every state, how
# far apart the largest and smallest district were in the plans drawn after
# the 2010 census. Congressional plans are reported in PEOPLE and legislative
# plans in PER CENT, which is not an inconsistency -- it is the two different
# legal standards showing through. The book is a PDF, so a rebuild cannot
# catch a correction, and the chapter says so.
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


dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Jason Timm, "PresElectionResults"
#   https://github.com/jaytimm/PresElectionResults  (branch: master)
#   data/pres_by_cd.rda
#
# WHY THIS DATASET RATHER THAN HOUSE RESULTS.
#
# To ask whether a districting plan is biased, you need to hold voters constant
# and vary only the lines. House results cannot do that: they mix the map
# together with incumbency, candidate quality, money, and uncontested seats.
# The standard move in redistricting analysis -- and in litigation -- is to take
# a single statewide election and ask how its votes fall across the districts.
# The 2024 presidential race is the same contest in every district in the
# country, so any difference in how those votes convert to seats is a property
# of the map.

url <- paste0("https://raw.githubusercontent.com/jaytimm/",
              "PresElectionResults/master/data/pres_by_cd.rda")

# The source is kept in raw/, not fetched to a tempfile: the corpus preserves
# every source as it arrived, so this build can re-run after the repository
# moves or disappears.
dir.create("raw", showWarnings = FALSE)
rda <- "raw/pres_by_cd.rda"
if (!file.exists(rda)) prov_fetch(url, rda, mode = "wb", quiet = TRUE)
env <- new.env(); load(rda, envir = env)
d <- as.data.frame(get(ls(env)[1], envir = env))

cat("downloaded", nrow(d), "congressional districts\n")
stopifnot(nrow(d) == 435,
          all(c("state_abbrev", "district_code", "house_rep",
                "house_rep_party", "party_win", "democrat", "republican")
              %in% names(d)))

# --- Known gaps, documented rather than patched ------------------------------
#
# One district carries no presidential figures at all.

gap <- d[is.na(d$democrat) | is.na(d$republican), ]
cat("districts with no presidential vote recorded:", nrow(gap), "\n")
print(gap[, c("state_abbrev", "district_code", "house_rep")])

# Maine reports ranked-choice contests in a way this compilation did not fully
# resolve; ME-02's House winner is recorded as "Continuing Ballots" with no
# party. The presidential figures for the district are fine, so the row is kept
# and the House party is set to NA rather than being guessed at.
d$house_rep_party[d$house_rep_party == "" |
                  d$house_rep == "Continuing Ballots"] <- NA

d <- d[!is.na(d$democrat) & !is.na(d$republican), ]

# --- Derive ------------------------------------------------------------------

d$dem_share <- round(100 * d$democrat / (d$democrat + d$republican), 2)
d$pres_party <- d$party_win
d$district <- paste0(d$state_abbrev, "-", d$district_code)

out <- d[, c("state_abbrev", "district_code", "district",
             "democrat", "republican", "dem_share",
             "pres_party", "house_rep", "house_rep_party")]
names(out)[1:2] <- c("state", "cd")
out <- out[order(out$state, out$cd), ]

# --- Checks the lab depends on ----------------------------------------------

cat("\ndistricts retained:", nrow(out), "of 435\n")
cat("Democratic-majority districts:", sum(out$dem_share > 50), "=",
    round(100 * sum(out$dem_share > 50) / nrow(out), 1), "%\n")
cat("mean district Democratic share:", round(mean(out$dem_share), 1), "%\n")
cat("competitive (45-55%):", sum(out$dem_share >= 45 & out$dem_share <= 55),
    "=", round(100 * mean(out$dem_share >= 45 & out$dem_share <= 55), 1), "%\n")

cross <- out[!is.na(out$house_rep_party) & out$pres_party != out$house_rep_party, ]
cat("crossover districts (presidential and House winners differ):",
    nrow(cross), "\n")

stopifnot(nrow(out) == 434, all(out$dem_share > 0 & out$dem_share < 100))

write.csv(out, "derived/pres_by_cd_2024.csv", row.names = FALSE)
cat("\nwrote pres_by_cd_2024.csv:", nrow(out), "rows\n")

# --- The seat map ------------------------------------------------------------
#
# labs/_lib/geo/us-apportionment.geojson is the book's House-seat cartogram:
# every state drawn with area proportional to its 2020 apportionment, on the
# shared frame all the book's national maps use. The brief reads plain CSV,
# so the rings and the label anchors are written out here, and the map's seat
# counts are asserted against the district rows above -- a map that disagreed
# with its own table would be worse than no map.
gj <- jsonlite::fromJSON("../../../_lib/geo/us-apportionment.geojson",
                         simplifyVector = FALSE)
rings <- list(); meta <- list(); k <- 0L
for (f in gj$features) {
  p <- f$properties
  meta[[length(meta) + 1L]] <- data.frame(
    state = p$st, name = p$name, seats = p$seats_2020,
    label_x = p$label_x, label_y = p$label_y)
  for (poly in f$geometry$coordinates) for (ring in poly) {
    k <- k + 1L
    rings[[k]] <- data.frame(state = p$st, part = k,
                             x = vapply(ring, function(q) q[[1]], 0),
                             y = vapply(ring, function(q) q[[2]], 0))
  }
}
rings <- do.call(rbind, rings); meta <- do.call(rbind, meta)

# 435 seats; one district per seat in the table, except New York, which is
# down one row because NY-21 carries no presidential figures.
per <- as.data.frame(table(state = out$state), stringsAsFactors = FALSE)
m <- merge(meta, per, by = "state")
stopifnot(nrow(m) == 50, sum(meta$seats) == 435,
          all(m$Freq == m$seats | (m$state == "NY" & m$Freq == m$seats - 1)))

write.csv(rings, "derived/seat_rings.csv", row.names = FALSE)
write.csv(meta, "derived/seat_states.csv", row.names = FALSE)
cat("wrote seat_rings.csv:", nrow(rings), "points;",
    "seat_states.csv:", nrow(meta), "states\n")

# --- How equal in population the plans had to be -----------------------------
#
# THE POINT OF THIS TABLE. Every map this chapter measures satisfies the one
# hard constitutional rule about districting, and satisfies it to the person.
# Equal population is not what separates a gerrymander from a fair map. The
# two standards are visibly different, though, and the difference is in the
# units the appendix has to use: congressional plans come out equal to within
# a handful of PEOPLE, legislative plans to within a few PER CENT.

dev <- read.delim("raw/ncsl-appendix-c.tsv", sep = "\t", quote = "",
                  stringsAsFactors = FALSE, na.strings = "")
stopifnot(nrow(dev) == 50L, !any(duplicated(dev$state)))

# Six states have one at-large House seat and so no congressional plan; the
# appendix leaves those cells empty. Nebraska is unicameral and has no house
# row. Nothing is filled in for either.
stopifnot(sum(is.na(dev$cong_people_range)) == 6L,
          sum(is.na(dev$house_pct_range))   == 1L,
          sum(is.na(dev$senate_pct_range))  == 0L)
stopifnot(all(dev$cong_people_range >= 0, na.rm = TRUE),
          all(dev$house_pct_range   >= 0, na.rm = TRUE),
          all(dev$senate_pct_range  >= 0, na.rm = TRUE))
write.csv(dev, "derived/deviation_states.csv", row.names = FALSE)

# One row per kind of plan: how many the appendix lists, the middle value,
# and the widest with the state that holds it.
worst <- function(v) dev$state[which.max(v)]
deviation <- data.frame(
  plan = c("Congressional", "State house", "State senate"),
  measured_in = c("people", "per cent", "per cent"),
  plans_listed = c(sum(!is.na(dev$cong_people_range)),
                   sum(!is.na(dev$house_pct_range)),
                   sum(!is.na(dev$senate_pct_range))),
  median_range = c(median(dev$cong_people_range, na.rm = TRUE),
                   median(dev$house_pct_range,   na.rm = TRUE),
                   median(dev$senate_pct_range,  na.rm = TRUE)),
  widest_range = c(max(dev$cong_people_range, na.rm = TRUE),
                   max(dev$house_pct_range,   na.rm = TRUE),
                   max(dev$senate_pct_range,  na.rm = TRUE)),
  widest_state = c(worst(dev$cong_people_range), worst(dev$house_pct_range),
                   worst(dev$senate_pct_range)))

# How many congressional plans came out equal to within two people, and how
# many legislative plans sit inside the 10% band that Brown v. Thomson treats
# as a minor deviation. Both are counted, not asserted.
deviation$within_tight <- c(
  sum(dev$cong_people_range <= 2,  na.rm = TRUE),
  sum(dev$house_pct_range   < 10,  na.rm = TRUE),
  sum(dev$senate_pct_range  < 10,  na.rm = TRUE))
write.csv(deviation, "derived/deviation.csv", row.names = FALSE)

cat("\ndeviation.csv : population equality of the 2010-cycle plans\n")
print(deviation, row.names = FALSE)

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
