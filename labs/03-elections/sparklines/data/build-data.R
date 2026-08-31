# ---------------------------------------------------------------------------
# Build the sparklines dataset: every state's presidential vote, 1976 to 2024,
# both as it was cast and as it stood against the country that year.
#
# Four files end up in derived/:
#
#   derived/series.csv   one row per state per election: the Democratic share
#                        of the two-party vote, and that share minus the
#                        national one
#   derived/states.csv   one row per state: where it started, where it ended,
#                        how far it moved and how far it wandered
#   derived/national.csv the national two-party share, one row per election
#   derived/facts.csv    single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Two files this corpus already builds and commits, in the historical-campaigns
# chapter, which documents where each came from:
#
#   ../../historical-campaigns/data/derived/pres_states_1864_2024.csv
#       presidential vote shares by state and year
#   ../../historical-campaigns/data/derived/pres_national.csv
#       the national popular vote, by candidate and party
#
# WHY 1976. Every state and the District of Columbia has an unbroken series
# from 1976 forward, so the table has no gaps to explain away, and the party
# system on either end of it is recognisably the same one. Going back further
# adds elections in which the words "Democrat" and "Republican" meant something
# different in the South than they do now, which is a real subject and is the
# historical-campaigns chapter's, not this one's.

SS <- "../../historical-campaigns/data/derived/pres_states_1864_2024.csv"
NN <- "../../historical-campaigns/data/derived/pres_national.csv"
stopifnot(file.exists(SS), file.exists(NN))
st <- read.csv(SS, stringsAsFactors = FALSE)
na <- read.csv(NN, stringsAsFactors = FALSE)

FROM <- 1976
st <- st[st$year >= FROM, ]

# --- The two-party share ----------------------------------------------------
#
# The Democratic share OF THE TWO MAJOR PARTIES, not of all votes. 1992 is the
# reason: Ross Perot took 18.9% of the national vote, and a share-of-everything
# series would show both major parties collapsing that year in every state at
# once -- an artifact of who else was on the ballot rather than anything about
# the states. Dividing by the two-party total removes it.

st$two <- 100 * st$democrat / (st$democrat + st$republican)
stopifnot(!any(is.na(st$two)))

# The national baseline is the actual national popular vote, not the average of
# the state shares. Those differ, because states are not the same size, and the
# average of fifty-one percentages is a number about states rather than voters.
nat <- do.call(rbind, lapply(sort(unique(st$year)), function(y) {
  z <- na[na$year == y & na$party %in% c("Democratic", "Republican"), ]
  stopifnot(nrow(z) == 2)
  d <- z$pop_votes[z$party == "Democratic"]
  r <- z$pop_votes[z$party == "Republican"]
  data.frame(year = y, national_two = round(100 * d / (d + r), 4),
             stringsAsFactors = FALSE)
}))
write.csv(nat, "derived/national.csv", row.names = FALSE)

st <- merge(st, nat, by = "year")
st$rel <- round(st$two - st$national_two, 4)
st$two <- round(st$two, 4)
st <- st[order(st$state_abbrev, st$year), ]
write.csv(st[, c("year", "state_abbrev", "two", "rel", "national_two")],
          "derived/series.csv", row.names = FALSE)

YEARS <- sort(unique(st$year))
NY <- length(YEARS)

# Every state must have every election, or the sparklines are not comparable
# and the table would silently draw shorter lines for some rows.
k <- table(st$state_abbrev)
stopifnot(all(k == NY))

# --- Per state ---------------------------------------------------------------

su <- do.call(rbind, lapply(split(st, st$state_abbrev), function(z) {
  z <- z[order(z$year), ]
  # a "flip" is an election whose winner differs from the one before it, read
  # off the two-party share rather than the winner column so that a state
  # carried by a third party does not break the sequence
  side <- z$two > 50
  data.frame(
    state = z$state_abbrev[1],
    first = round(z$two[1], 2), last = round(z$two[NY], 2),
    change = round(z$two[NY] - z$two[1], 2),
    lo = round(min(z$two), 2), hi = round(max(z$two), 2),
    range = round(max(z$two) - min(z$two), 2),
    rel_first = round(z$rel[1], 2), rel_last = round(z$rel[NY], 2),
    rel_change = round(z$rel[NY] - z$rel[1], 2),
    dem_wins = sum(side), flips = sum(side[-1] != side[-NY]),
    stringsAsFactors = FALSE)
}))
su <- su[order(su$change), ]
write.csv(su, "derived/states.csv", row.names = FALSE)

# --- Facts -------------------------------------------------------------------

up  <- su[which.max(su$change), ]
dn  <- su[which.min(su$change), ]
stb <- su[which.min(su$range), ]
vol <- su[which.max(su$range), ]
relup <- su[which.max(su$rel_change), ]
reldn <- su[which.min(su$rel_change), ]

# how many states move in the SAME direction as the country between any two
# consecutive elections -- the quantity the relative view removes
sw <- do.call(rbind, lapply(2:NY, function(i) {
  a <- st[st$year == YEARS[i - 1], c("state_abbrev", "two")]
  b <- st[st$year == YEARS[i],     c("state_abbrev", "two")]
  m <- merge(a, b, by = "state_abbrev", suffixes = c("_a", "_b"))
  nd <- nat$national_two[nat$year == YEARS[i]] -
        nat$national_two[nat$year == YEARS[i - 1]]
  data.frame(year = YEARS[i], nat_move = round(nd, 2),
             same = sum(sign(m$two_b - m$two_a) == sign(nd)),
             n = nrow(m), stringsAsFactors = FALSE)
}))
sw$pct_same <- round(100 * sw$same / sw$n, 1)

facts <- data.frame(
  key = c("from", "to", "elections", "states", "rows",
          "up_state", "up_change", "up_first", "up_last",
          "dn_state", "dn_change", "dn_first", "dn_last",
          "stable_state", "stable_range", "volatile_state", "volatile_range",
          "relup_state", "relup_change", "reldn_state", "reldn_change",
          "nat_lo", "nat_lo_year", "nat_hi", "nat_hi_year",
          "median_same", "min_same", "flips_max", "flips_max_state",
          "never_flipped"),
  value = c(FROM, max(YEARS), NY, nrow(su), nrow(st),
            up$state, up$change, up$first, up$last,
            dn$state, dn$change, dn$first, dn$last,
            stb$state, stb$range, vol$state, vol$range,
            relup$state, relup$rel_change, reldn$state, reldn$rel_change,
            round(min(nat$national_two), 1),
            nat$year[which.min(nat$national_two)],
            round(max(nat$national_two), 1),
            nat$year[which.max(nat$national_two)],
            median(sw$pct_same), min(sw$pct_same),
            max(su$flips), su$state[which.max(su$flips)],
            sum(su$flips == 0)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("series.csv ->", nrow(st), "rows,", nrow(su), "states x", NY, "elections\n")
cat("national two-party D share:", round(min(nat$national_two), 1), "to",
    round(max(nat$national_two), 1), "\n\n")
cat("largest move toward the Democrats:", up$state, up$change, "\n")
cat("largest move away:               ", dn$state, dn$change, "\n")
cat("steadiest:", stb$state, "range", stb$range, "points\n")
cat("states that never changed side:", sum(su$flips == 0), "of", nrow(su), "\n")
cat("median share of states moving WITH the country:", median(sw$pct_same), "%\n")
cat("done.\n")

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
