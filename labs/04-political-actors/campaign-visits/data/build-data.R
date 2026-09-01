# ---------------------------------------------------------------------------
# Build the campaign-visits datasets.
#
# Three files end up in this folder:
#
#   derived/campaign_visits_2024.csv  every 2024 presidential campaign stop tracked by
#                             the Associated Press
#   derived/pres2024_states.csv       state-level 2024 returns -- the same file used in
#                             Weeks 1 and 2, copied so this lab stands alone
#   derived/ap_snapshots.csv          two captures of the SAME AP tracker, so the lab can
#                             show how much a "finished" source kept moving
#
# Run this script from inside the data/ folder. It needs a network connection;
# the whole point of committing the outputs is that the lab does not.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- 1. Campaign stops ------------------------------------------------------
#
# Source: Associated Press, "Tracking the 2024 presidential campaign trail"
#   https://interactives.ap.org/embeds/ziEfd/54/
#   data: https://interactives.ap.org/embeds/ziEfd/54/dataset.csv
#
# THE VERSION NUMBER IS PART OF THE CITATION. The embed URL carries a version
# and AP kept publishing new ones after the election. This lab was originally
# built against /50/, which stops on 1 November 2024 -- four days before the
# election, with the final weekend missing. /54/ runs through 4 November and
# also BACKFILLS events into the period /50/ already claimed to cover.
#
# Both versions are still served, so the script fetches each one and writes the
# comparison to ap_snapshots.csv. The lab and the brief compute the difference
# from that file rather than asserting it.
#
# Other things to know about the coverage:
#
#   * It counts *events*, not time, money, or audience. A rally of 20,000 and a
#     stop at a diner are both one row.
#   * Nothing compelled any of it. It is a news organisation's log, not a filing.

ap <- function(ver)
  read.csv(sprintf("https://interactives.ap.org/embeds/ziEfd/%s/dataset.csv", ver),
           stringsAsFactors = FALSE, check.names = FALSE,
           colClasses = c(fips = "character"))

VERSION <- "54"                     # the capture the lab is built on
PRIOR   <- "50"                     # the capture it used to be built on

raw <- ap(VERSION)

cat("downloaded", nrow(raw), "campaign stops from version", VERSION, "\n")
stopifnot(nrow(raw) > 300,
          all(c("date","city","county","state","candidate","event-type") %in% names(raw)))

v <- raw[, c("date", "city", "county", "state", "postal", "lat", "long",
             "fips", "candidate", "event-type", "notes")]
names(v)[names(v) == "event-type"] <- "event_type"

v$date <- as.Date(v$date, format = "%m/%d/%y")
stopifnot(!any(is.na(v$date)))
v <- v[order(v$date), ]

cat("date range:", format(min(v$date)), "to", format(max(v$date)), "\n")
cat("candidates:\n"); print(table(v$candidate))
cat("states appearing:", length(unique(v$state)), "\n")

write.csv(v, "derived/campaign_visits_2024.csv", row.names = FALSE)

# --- 1b. How much the source moved after we first captured it ---------------
#
# The point is not that AP was careless. It is that a public dataset with no
# statutory deadline has no moment at which it is finished, and a citation
# without a version number does not identify what was read.

old <- ap(PRIOR)
old$date <- as.Date(old$date, format = "%m/%d/%y")

snap <- data.frame(
  version    = c(PRIOR, VERSION),
  rows       = c(nrow(old), nrow(v)),
  last_event = c(format(max(old$date)), format(max(v$date))),
  states     = c(length(unique(old$state)), length(unique(v$state))),
  # rows dated on or before the older capture's final day -- i.e. how many
  # events each version reports for a window BOTH versions claim to cover
  rows_in_shared_window = c(sum(old$date <= max(old$date)),
                            sum(v$date   <= max(old$date))),
  stringsAsFactors = FALSE)

write.csv(snap, "derived/ap_snapshots.csv", row.names = FALSE)

cat("\n--- the source kept moving ---\n"); print(snap)
cat("rows added after the older capture's last day:",
    sum(v$date > max(old$date)), "\n")
cat("rows backfilled INTO the window it already covered:",
    sum(v$date <= max(old$date)) - nrow(old), "\n")

# --- 2. State results -------------------------------------------------------
#
# Identical to the `electoral-map` file. Copied rather than re-derived so that students
# can join on it without hunting through other folders, and so the numbers
# match exactly what they have already seen.

src <- file.path("..", "..", "..", "03-elections", "electoral-map", "data", "derived/pres2024_states.csv")
stopifnot(file.exists(src))
st <- read.csv(src, stringsAsFactors = FALSE)
stopifnot(nrow(st) == 51)
write.csv(st, "derived/pres2024_states.csv", row.names = FALSE)

# --- 3. Check the join before the lab depends on it -------------------------

miss <- setdiff(unique(v$state), st$state)
cat("\nstate names in the visit file with no match in the results file:",
    if (length(miss) == 0) "none" else paste(miss, collapse = ", "), "\n")
stopifnot(length(miss) == 0)

vs <- as.data.frame(table(v$state)); names(vs) <- c("state", "visits")
m <- merge(st, vs, by = "state", all.x = TRUE); m$visits[is.na(m$visits)] <- 0
cat("states with zero visits:", sum(m$visits == 0), "of", nrow(m), "\n")
cat("visits accounted for after join:", sum(m$visits), "of", nrow(v), "\n")

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
