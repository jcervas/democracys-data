# ---------------------------------------------------------------------------
# Build the follower-counts dataset: how big is a member of Congress's audience,
# and what does each platform mean by the number it gives you.
#
# SOURCE. raw/followers-2026-08-15.csv, frozen, made by collect.mjs. One row per
# member of Congress per platform -- X, Instagram, Bluesky -- for all 537 sitting
# members, including every member the platform had nothing for. The misses are
# recorded rather than dropped, because the pattern in who is missing is the
# larger part of what this chapter finds.
#
# THE HANDLES ARE NOT PUBLISHED BY THE PLATFORMS. X and Instagram handles come
# from unitedstates/congress-legislators, a roster maintained by volunteers.
# Bluesky handles are not in that roster at all and were guessed against the
# chamber's own domain, which is why a Bluesky hit cannot be the wrong person:
# only the Senate can put a name under senate.gov.
#
# WHAT THIS SCRIPT DOES NOT CLAIM. It does not say who is most followed in
# Congress. No platform here supports that sentence: Bluesky is missing one
# whole party, Instagram is missing a quarter of the members, and X publishes
# only a rounded figure. Saying so is the chapter.
#
# Run from this directory:  Rscript build-data.R      (offline; reads raw/)
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE, warn = 1)

SCAN_DATE <- "2026-08-15"
say <- function(...) cat(sprintf(...), "\n", sep = "")

FACTS <- list()
fact  <- function(key, value, note) {
  FACTS[[key]] <<- list(value = dd_num(value), note = note); invisible(value)
}
CHECKS <- list()
check <- function(label, value) {
  CHECKS[[length(CHECKS) + 1L]] <<- list(check = label, value = value)
  say("  check: %-62s %s", label, value); invisible(value)
}

f <- read.csv(sprintf("raw/followers-%s.csv", SCAN_DATE), colClasses = "character")
f$exact     <- suppressWarnings(as.numeric(f$exact))
f$got_count <- f$status == "ok"

# The snapshot must be complete before anything is computed from it. A partly
# collected file still builds most of these tables, and the tables look fine --
# so say plainly which platform is short rather than failing later inside a
# subset that happens to be empty.
EXPECTED <- c("bluesky", "instagram", "x")
have <- table(f$platform)
if (!all(EXPECTED %in% names(have)) || length(unique(have)) != 1L) {
  stop(sprintf(paste0("raw/followers-%s.csv is incomplete: %s.\n",
                      "  Every platform needs one row per member. Re-run:  node collect.mjs"),
               SCAN_DATE,
               paste(sprintf("%s=%d", names(have), as.integer(have)), collapse = ", ")))
}

fact("scan_date", SCAN_DATE, "the day every profile was read")
fact("n_members", length(unique(f$bioguide_id)), "sitting members of Congress")
fact("n_platforms", length(unique(f$platform)), "platforms asked about each one")
fact("n_rows", nrow(f), "member-platform pairs, misses included")
check("every member was looked for on every platform",
      all(table(f$bioguide_id) == length(unique(f$platform))))

# --- reading a number a platform meant for a human --------------------------
#
# X and Instagram write the count for a reader, not for a file: "99.3K", "1.2M".
# That is a picture of a number and has to be turned back into one before
# anything can be sorted by it. The K and the M are the whole of the rule.

parse_disp <- function(x) {
  x <- gsub(",", "", trimws(x))
  mult <- ifelse(grepl("M$", x), 1e6, ifelse(grepl("K$", x), 1e3, 1))
  suppressWarnings(as.numeric(gsub("[KM]$", "", x))) * mult
}
f$displayed_n <- parse_disp(f$displayed)

# --- who each platform has, and who it does not -----------------------------

cov <- do.call(rbind, lapply(split(f, f$platform), function(d) {
  data.frame(platform = d$platform[1],
             members = nrow(d),
             handle_on_file = sum(d$handle != ""),
             count_read = sum(d$got_count),
             no_handle = sum(d$status == "no-handle-on-file"),
             no_account = sum(d$status == "no-account-found"),
             other_miss = sum(!d$got_count & !d$status %in%
                              c("no-handle-on-file", "no-account-found")),
             pct_covered = round(100 * sum(d$got_count) / nrow(d), 1))
}))
cov <- cov[order(-cov$count_read), ]
row.names(cov) <- NULL
dd_write_csv(cov, "derived/coverage.csv")

for (p in cov$platform) {
  fact(paste0("covered_", p), cov$count_read[cov$platform == p],
       paste("members whose follower count", p, "actually yielded"))
}
check("no platform covers the whole chamber", all(cov$count_read < cov$members))
check("coverage differs between platforms rather than being one wall",
      length(unique(cov$count_read)) > 1)

# --- the finding that decides what the data can be used for -----------------
#
# Coverage is not a random sample of Congress. On Bluesky it is one party.

party <- do.call(rbind, lapply(split(f, list(f$platform, f$party), drop = TRUE), function(d) {
  data.frame(platform = d$platform[1], party = d$party[1],
             members = nrow(d), covered = sum(d$got_count),
             pct = round(100 * sum(d$got_count) / nrow(d), 1))
}))
party <- party[order(party$platform, -party$covered), ]
row.names(party) <- NULL
dd_write_csv(party, "derived/party_coverage.csv")

bsky <- f[f$platform == "bluesky", ]
rep_bsky <- sum(bsky$got_count & bsky$party == "Republican")
dem_bsky <- sum(bsky$got_count & bsky$party == "Democrat")
fact("bsky_dem", dem_bsky, "Democrats with a verified .gov Bluesky account")
fact("bsky_rep", rep_bsky, "Republicans with one")
fact("bsky_rep_total", sum(bsky$party == "Republican"), "Republicans in Congress")
fact("bsky_dem_pct", round(100 * dem_bsky / sum(bsky$party == "Democrat"), 1),
     "percent of Democrats present on Bluesky")
check("the Bluesky gap is a party gap, not a size gap",
      rep_bsky < dem_bsky / 10)

# The same gap, measured on the platform where it is smallest, so the reader can
# see that the instrument varies and the chamber does not.
xr <- f[f$platform == "x", ]
x_rep_pct <- round(100 * sum(xr$got_count & xr$party == "Republican") /
                   sum(xr$party == "Republican"), 1)
x_dem_pct <- round(100 * sum(xr$got_count & xr$party == "Democrat") /
                   sum(xr$party == "Democrat"), 1)
fact("x_rep_pct", x_rep_pct, "percent of Republicans whose X count was read")
fact("x_dem_pct", x_dem_pct, "percent of Democrats whose X count was read")
check("X covers both parties at rates within 25 points of each other",
      abs(x_dem_pct - x_rep_pct) < 25)

# --- the handles the roster has but the platform does not -------------------
#
# A handle on file that 404s is a different failure from no handle at all: the
# crosswalk is not empty, it is WRONG. These are worth separating because the
# wrong ones are not scattered at random. A member who moves from the House to
# the Senate, or becomes Speaker, renames the account -- and the roster goes on
# saying Rep. The test is mechanical: a handle that still begins "Rep" attached
# to somebody now sitting in the Senate cannot be current.

stale <- f[f$status == "handle-not-found", ]
stale$rep_handle <- grepl("^rep", stale$handle, ignore.case = TRUE)
stale$promoted   <- stale$rep_handle & stale$chamber == "Senate"

stale_out <- data.frame(
  member = paste0(stale$first_name, " ", stale$last_name),
  party = stale$party, chamber = stale$chamber, platform = stale$platform,
  handle_on_file = stale$handle,
  says_rep_but_sits_in_senate = stale$promoted)
stale_out <- stale_out[order(-stale_out$says_rep_but_sits_in_senate,
                             stale_out$member), ]
dd_write_csv(stale_out, "derived/stale_handles.csv")

fact("n_stale", nrow(stale), "handles on file that the platform does not have")
fact("n_promoted", sum(stale$promoted),
     "of them belonging to a member who moved up to the Senate and renamed")
check("a wrong handle is a different failure from a missing one",
      nrow(stale) > 0 && sum(f$status == "no-handle-on-file") > 0)
check("stale handles concentrate among members whose job changed",
      sum(stale$promoted) >= 1)

# --- is the fuller number Instagram carries really the follower count? ------
#
# Instagram writes the count for a reader -- "349K" -- and carries a fuller
# number in the same page. The fuller one is what the member table below
# publishes, so it has to be the right quantity: the page also holds a post
# count and a following count, and picking up the wrong field would be
# invisible in the output. The test is that it lands beside the count the
# profile shows a human. A post count sits orders of magnitude away and fails
# immediately. The tolerance is one unit of whatever abbreviation the profile
# used -- "3M" gives away everything below a million -- plus a little for
# movement, because a follower count does not hold still between two reads of
# one page.

unit_of <- function(x) {
  x <- trimws(x); ifelse(grepl("M$", x), 1e6, ifelse(grepl("K$", x), 1e3, 1))
}
ig <- f[f$platform == "instagram" & f$got_count &
        !is.na(f$exact) & !is.na(f$displayed_n), ]
ig$agrees <- abs(ig$exact - ig$displayed_n) <= unit_of(ig$displayed) + 0.01 * ig$exact
fact("ig_pairs", nrow(ig), "Instagram profiles giving both a shown and a fuller count")
fact("ig_disagree", sum(!ig$agrees),
     "where the two are too far apart to be the same quantity")
check("the exact number harvested is the follower count and not another field",
      mean(ig$agrees) > 0.98)

# --- every member, every platform, in one table -----------------------------
#
# The tables above are aggregates, and an aggregate is where a reader loses the
# ability to check. Congress is small enough to print whole, so it is printed
# whole: one row per member, one column per platform, and a MISS NAMED rather
# than left blank. A blank cell reads as zero followers to anyone in a hurry,
# and none of these are zero -- they are three different failures, and which
# one it is matters more than the number would have.

miss_label <- c("no-handle-on-file" = "no handle",
                "handle-not-found"  = "handle wrong",
                "no-account-found"  = "—")

shown <- function(d) {
  v <- unname(miss_label[d$status])
  v[is.na(v)] <- d$status[is.na(v)]        # an unlabelled status shows itself
  if (d$platform[1] == "x") {
    # X publishes only the rounded picture, so the table reproduces what the
    # profile prints rather than inventing digits X did not give.
    v[d$got_count] <- d$displayed[d$got_count]
  } else {
    v[d$got_count] <- formatC(d$exact[d$got_count], format = "d", big.mark = ",")
  }
  v
}

mem <- f[f$platform == "x", c("bioguide_id", "last_name", "first_name",
                              "party", "state", "chamber")]
col <- lapply(split(f, f$platform),
              function(d) shown(d[match(mem$bioguide_id, d$bioguide_id), ]))

all_m <- data.frame(
  state     = mem$state,
  chamber   = mem$chamber,
  member    = paste0(mem$last_name, ", ", mem$first_name),
  party     = substr(mem$party, 1, 1),
  x         = col$x,
  instagram = col$instagram,
  bluesky   = col$bluesky)
# Ordered the way a person looks themselves up: by state, senators first.
all_m <- all_m[order(all_m$state, all_m$chamber != "Senate", all_m$member), ]
row.names(all_m) <- NULL
dd_write_csv(all_m, "derived/all_members.csv")

fact("n_delegates", sum(!mem$state %in% state.abb),
     "members from places with no vote on the House floor")

# The coverage table and the full table are computed from the same file by
# different routes, so they are made to agree here rather than assumed to. A
# cell holding a count begins with a digit; a named miss does not.
counts_in_table <- vapply(c("x", "instagram", "bluesky"),
                          function(p) sum(grepl("^[0-9]", all_m[[p]])), 0L)
check("the full member table agrees with the coverage table, platform by platform",
      all(counts_in_table[cov$platform] == cov$count_read))

# --- the three platforms disagree about who is biggest ----------------------
#
# The follower column carries the platform's own words, not a number derived
# from them. X prints "11.5M" and nothing finer, so writing 11,500,000 into this
# table would put six digits of confidence in a file that has three.

best <- function(p) {
  d <- f[f$platform == p & f$got_count, ]
  v <- if (p == "bluesky") d$exact else ifelse(!is.na(d$exact) & d$exact > 0, d$exact, d$displayed_n)
  d$val <- v; d <- d[order(-d$val), ]
  data.frame(platform = p, member = paste0(d$first_name[1], " ", d$last_name[1]),
             party = d$party[1],
             followers = if (p == "x") d$displayed[1]
                         else formatC(d$val[1], format = "d", big.mark = ","),
             covered = nrow(d))
}
tops <- do.call(rbind, lapply(sort(unique(f$platform)), best))
dd_write_csv(tops, "derived/platform_leaders.csv")
fact("n_distinct_leaders", length(unique(tops$member)),
     "different people the three platforms name as the most followed")
check("the platforms do not agree on one answer", length(unique(tops$member)) > 1)

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("\ndone: %d facts, %d checks", nrow(facts), length(CHECKS))

# ---------------------------------------------------------------------------
# Build stamp -- see ../../../_lib/provenance.R. Guarded, because a missing
# helper must not fail a build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
