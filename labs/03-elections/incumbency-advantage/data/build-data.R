# ---------------------------------------------------------------------------
# Build the incumbency-advantage dataset from raw House candidate returns,
# 1976-2024.
#
#   derived/races.csv     One row per district-year: the two-party Democratic
#                  vote share, the same district's share two years earlier
#                  (when that comparison is legitimate), and whether the
#                  previous winner is on the ballot again.
#   derived/rdd.csv        races.csv narrowed to the close-election design:
#                  both this election and the one two years before it are
#                  standard, contested, two-party races.
#   derived/by_decade.csv  The close-election jump, fit separately by decade.
#   derived/binned.csv     races.csv's rdd rows, averaged into 2-point bins
#                  of the lagged margin, for the scatter in the brief.
#
# SOURCE. MIT Election Data and Science Lab, 2017, "U.S. House 1976-2024",
#   https://doi.org/10.7910/DVN/IG0UN2, Harvard Dataverse, V15. The file
#   sits behind a Dataverse guestbook (a name/email/purpose form a browser
#   must submit before the download starts); a scripted request gets HTTP
#   400 and "You may not download this file without the required Guestbook
#   response." So it is committed here rather than fetched. See ai-prompt.txt.
#
# ---------------------------------------------------------------------------
# WHAT THIS FILE HAS TO BUILD THAT THE CODEBOOK DOES NOT HAND YOU
#
# There is no `incumbent` column anywhere in this file. Every one of the
# 33,805 rows is a candidate's vote total for one party line in one district
# in one year, and "was this candidate already the officeholder" is not
# among the twenty variables the codebook lists. Getting to incumbency at
# all means resolving three problems first, none of which is optional:
#
# 1. FUSION VOTING. A candidate can appear on more than one party's line in
#    the same race -- 2,677 rows here carry `fusion_ticket = TRUE` -- and a
#    two-party vote share computed from the row tagged DEMOCRAT alone
#    undercounts every fusion candidate who also ran on, say, New York's
#    Working Families line. Fixed by summing candidatevotes across every row
#    sharing a (year, state, district, candidate) key before anything else
#    is computed -- exactly what the codebook's own notes recommend.
#
# 2. THE MINNESOTA TRAP. From 1975 to 1995 the Minnesota Republican Party's
#    legal name was "Independent-Republican," and this file records that
#    label verbatim -- it is what the ballot said. A `party == "REPUBLICAN"`
#    filter, which is the obvious thing to write, silently drops every
#    Minnesota Republican nominee in that window. All eight of Minnesota's
#    1992 House races carry the party string this way, and a naive filter
#    manufactures eight phantom uncontested Democratic landslides that
#    November -- a state sweep that did not happen. Fixed by matching a
#    short list of a party's known legal aliases (below), not the bare
#    string.
#
# 3. RUNOFFS DOUBLE AS A SECOND ROW, NOT A REPLACEMENT ROW. Four
#    district-years in this file went to a runoff -- Texas's 8th, 9th and
#    25th in 1996 (Texas required an outright majority to win a House seat
#    outright until the rule was dropped after that year) and Louisiana's
#    5th in 2002 (Louisiana's all-party "jungle primary" doubles as the
#    November general, with a December runoff when nobody clears 50%; the
#    same construction is flagged in this book's house-competition
#    chapter). Both rounds are stored as ordinary GEN rows for the same
#    candidates, distinguished only by `runoff`. Summing candidatevotes
#    across them, as step 1 would otherwise do, adds a candidate's
#    first-round total to their runoff total and reports an election that
#    never happened. Fixed by keeping only the runoff round wherever one
#    exists for a district-year, and dropping the preliminary round.
#
# None of these three is rare enough to ignore and none is signaled by a
# flag column; each was found by testing "does the naive version make
# sense," not by reading documentation that does not exist for it.
#
# ---------------------------------------------------------------------------
# WHAT COUNTS AS THE SAME SEAT ACROSS TWO ELECTIONS
#
# A single "incumbency advantage" chapter can be about the personal
# incumbent (does the same named human being sit down in the same chair) or
# about the incumbent party (did whichever party held the seat keep it).
# This build constructs both, because they answer different questions and
# because the second is what makes the causal design in the brief work
# without the first.
#
# PERSONAL incumbency needs the previous winner's name to survive into the
# next election's candidate list. Names here are recorded as they appeared
# on that year's ballot -- sometimes with a middle initial, a "JR." or a
# stray backslash-escaped quote around a nickname -- so matching uses a
# normalized form (uppercase, punctuation and generational suffixes
# stripped). This will still miss a remarriage name change or a genuine
# misspelling; the brief says so.
#
# PARTY incumbency needs only this election's and the previous one's
# two-party vote shares in the same district. It asks nothing about who
# was on the ballot, which is exactly what makes it usable as a
# close-election design: near a 50-50 split, which party crossed the line
# last time is approximately a coin flip, uncontaminated by which
# individual chose to run again.
#
# THE DISTRICT NUMBER RESET. Either kind of pairing requires "the same
# district" to mean something, and once a decade, when the lines are
# redrawn, it does not: California's 12th in 2010 and California's 12th
# in 2012 are
# different pieces of land. This build pairs an election only with the one
# exactly two years before it IN THE SAME REDISTRICTING CYCLE (new lines
# take effect in 1982, 1992, 2002, 2012 and 2022), so every pairing that
# would straddle a remap is simply not made. A district that a MID-decade
# court-ordered remap changed anyway -- Texas in 2004, Pennsylvania in
# 2018, North Carolina several times -- is not caught by this rule; see
# the brief's "what this cannot tell you."
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)

SRC <- "raw/1976-2024-house.tab"
if (!file.exists(SRC)) stop("MEDSL file not found at: ", SRC)
raw <- read.csv(SRC, stringsAsFactors = FALSE)
cat(sprintf("raw rows: %s\n", format(nrow(raw), big.mark = ",")))
stopifnot(nrow(raw) == 33805)

# ---- universe: general elections, not special, the 50 states, real candidates
g <- raw[raw$stage == "GEN" & !raw$special &
         raw$state != "DISTRICT OF COLUMBIA" & !raw$writein, ]
GENERAL_ROWS <- nrow(g)
cat(sprintf("general, non-special, 50-state, non-write-in rows: %s\n",
            format(GENERAL_ROWS, big.mark = ",")))

# ---- fix 3: keep only the runoff round where a runoff exists ---------------
runoff_keys <- unique(paste(g$state[which(g$runoff %in% TRUE)],
                             g$district[which(g$runoff %in% TRUE)],
                             g$year[which(g$runoff %in% TRUE)]))
gkey <- paste(g$state, g$district, g$year)
g <- g[!(gkey %in% runoff_keys & !(g$runoff %in% TRUE)), ]
cat(sprintf("district-years resolved to their runoff round: %d\n", length(runoff_keys)))

# ---- fix 2: a party's known legal aliases, not the bare string -------------
DEM_LABELS <- c("DEMOCRAT", "DEMOCRATIC", "DEMOCRATIC-FARMER-LABOR",
                "DEMOCRATIC-FARM-LABOR", "DEMOCRATIC-NPL",
                "DEMOCRATIC-NONPARTISAN LEAGUE", "FOGLIETTA (DEMOCRAT)")
REP_LABELS <- c("REPUBLICAN", "INDEPENDENT-REPUBLICAN", "REPUBLICAN, LIBERTARIAN")
g$is_dem_row <- g$party %in% DEM_LABELS
g$is_rep_row <- g$party %in% REP_LABELS

naive_rep <- g$party %in% "REPUBLICAN"
MN_TRAP_ROWS <- sum(g$party %in% "INDEPENDENT-REPUBLICAN")
mn_key <- paste(g$state, g$district, g$year)
ir_keys <- unique(mn_key[g$party %in% "INDEPENDENT-REPUBLICAN"])
MN_TRAP_PHANTOM <- sum(vapply(ir_keys, function(k) !any(naive_rep[mn_key == k]), logical(1)))
cat(sprintf("Minnesota trap: %d rows, %d of them the only Republican in their race\n",
            MN_TRAP_ROWS, MN_TRAP_PHANTOM))
cat(sprintf("(the fixed party-alias list also picks up %d other row(s) a bare\n",
            sum(g$is_rep_row) - sum(naive_rep) - MN_TRAP_ROWS),
    " 'REPUBLICAN' filter would miss -- Vermont 2024's fusion-labeled ",
    "'REPUBLICAN, LIBERTARIAN')\n", sep = "")

# ---- fix 1: sum a candidate's votes across every party line they hold -----
agg <- aggregate(candidatevotes ~ year + state + district + candidate, data = g, sum)
isd <- aggregate(is_dem_row ~ year + state + district + candidate, data = g, any)
isr <- aggregate(is_rep_row ~ year + state + district + candidate, data = g, any)
agg <- merge(merge(agg, isd), isr)

key <- paste(agg$year, agg$state, agg$district)
nD <- tapply(agg$is_dem_row, key, sum); nR <- tapply(agg$is_rep_row, key, sum)

# ---- one row per district-year ---------------------------------------------
races <- unique(g[, c("year", "state", "district")])
races$key <- paste(races$year, races$state, races$district)
races$nD <- as.integer(nD[races$key]); races$nR <- as.integer(nR[races$key])
# >1 candidate on the same party's line: California's and Washington's
# top-two primaries, and Louisiana's jungle primary, can put two
# Democrats or two Republicans on the same November ballot and nobody
# from the other party. A two-party SHARE has no meaning in that race.
races$nonstandard <- races$nD > 1 | races$nR > 1

norm_name <- function(x) {
  x <- toupper(x)
  x <- gsub("[.,\"]", "", x)
  x <- gsub("\\\\", "", x)
  x <- gsub("\\b(JR|SR|II|III|IV)\\b", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
agg$candidate_norm <- norm_name(agg$candidate)
split_agg <- split(agg, key)

dem_votes  <- rep(NA_real_, nrow(races)); rep_votes   <- rep(NA_real_, nrow(races))
winner_norm <- rep(NA_character_, nrow(races)); n_candidates <- rep(NA_integer_, nrow(races))
for (i in seq_len(nrow(races))) {
  sub <- split_agg[[races$key[i]]]
  if (is.null(sub)) next
  dsub <- sub[sub$is_dem_row, , drop = FALSE]
  rsub <- sub[sub$is_rep_row, , drop = FALSE]
  if (nrow(dsub)) dem_votes[i] <- sum(dsub$candidatevotes)
  if (nrow(rsub)) rep_votes[i] <- sum(rsub$candidatevotes)
  winner_norm[i] <- sub$candidate_norm[which.max(sub$candidatevotes)]
  n_candidates[i] <- nrow(sub)
}
races$dem_votes <- dem_votes; races$rep_votes <- rep_votes
races$winner_norm <- winner_norm; races$n_candidates <- n_candidates

races$uncontested <- is.na(races$dem_votes) | races$dem_votes == 0 |
                      is.na(races$rep_votes) | races$rep_votes == 0
races$standard <- !races$nonstandard & !races$uncontested
races$dv <- ifelse(races$standard, 100 * races$dem_votes / (races$dem_votes + races$rep_votes), NA)
# Three decimals is far past what a hand count needs; it just keeps write.csv
# from spilling a binary fraction out to fifteen digits (check-tables.py's
# "spurious precision" rule).
races$dv <- round(races$dv, 3)
races$margin <- round(races$dv - 50, 3)

cat(sprintf("\nrace universe: %d district-years, %d-%d\n",
            nrow(races), min(races$year), max(races$year)))
cat(sprintf("  standard two-party races: %d\n", sum(races$standard)))
cat(sprintf("  uncontested:              %d\n", sum(races$uncontested)))
cat(sprintf("  nonstandard (top-two/jungle, same-party pair): %d\n", sum(races$nonstandard)))

# ---- redistricting cycles + within-cycle lag -------------------------------
REDISTRICTING_YEARS <- c(1972, 1982, 1992, 2002, 2012, 2022)
races$cycle <- findInterval(races$year, REDISTRICTING_YEARS)

races <- races[order(races$state, races$district, races$year), ]
idx_by_key <- setNames(seq_len(nrow(races)), races$key)
cand_by_race <- lapply(split_agg, function(sub) sub$candidate_norm)

races$lag_dv <- NA_real_; races$lag_margin <- NA_real_; races$lag_standard <- NA
races$personal_incumbent_running <- NA
races$personal_incumbent_reelected <- NA
races$valid_pair <- FALSE

for (r in seq_len(nrow(races))) {
  prevkey <- paste(races$year[r] - 2, races$state[r], races$district[r])
  pr <- idx_by_key[prevkey]
  if (is.na(pr)) next
  if (races$cycle[r] != races$cycle[pr]) next   # crosses a redistricting boundary
  races$valid_pair[r] <- TRUE
  races$lag_dv[r] <- races$dv[pr]; races$lag_margin[r] <- races$margin[pr]
  races$lag_standard[r] <- races$standard[pr]
  lw <- races$winner_norm[pr]
  cur_names <- cand_by_race[[races$key[r]]]
  running <- !is.na(lw) && lw %in% cur_names
  races$personal_incumbent_running[r] <- running
  races$personal_incumbent_reelected[r] <- running && identical(races$winner_norm[r], lw)
}

vp <- races[races$valid_pair, ]
cat(sprintf("\nvalid within-cycle pairs: %d\n", nrow(vp)))
cat(sprintf("  previous winner on the ballot again: %d (%.1f%%)\n",
            sum(vp$personal_incumbent_running), 100 * mean(vp$personal_incumbent_running)))
run <- vp[vp$personal_incumbent_running, ]
cat(sprintf("  reelected when running again:        %d (%.1f%%)\n",
            sum(run$personal_incumbent_reelected), 100 * mean(run$personal_incumbent_reelected)))

write.csv(races[, c("year", "state", "district", "dv", "margin", "standard",
                     "uncontested", "nonstandard", "n_candidates", "cycle",
                     "valid_pair", "lag_dv", "lag_margin", "lag_standard",
                     "personal_incumbent_running", "personal_incumbent_reelected")],
          "derived/races.csv", row.names = FALSE)

# ---- the close-election design: party incumbency, no name-matching needed -
rdd <- vp[vp$standard & vp$lag_standard %in% TRUE & !is.na(vp$lag_margin), ]
rdd$treat <- rdd$lag_margin >= 0   # Democrats held the seat coming into this election
cat(sprintf("\nRDD sample (both elections standard, contested, two-party): %d\n", nrow(rdd)))

fit_jump <- function(d, h) {
  loc <- d[abs(d$lag_margin) <= h, ]
  f <- lm(dv ~ lag_margin * treat, data = loc)
  s <- summary(f)$coefficients
  data.frame(bandwidth = h, n = nrow(loc), jump = s["treatTRUE", "Estimate"],
             se = s["treatTRUE", "Std. Error"])
}
pooled <- do.call(rbind, lapply(c(20, 10, 5), fit_jump, d = rdd))
cat("\npooled close-election estimate, by bandwidth:\n"); print(pooled, row.names = FALSE)

write.csv(rdd[, c("year", "state", "district", "dv", "lag_dv", "lag_margin", "treat",
                   "personal_incumbent_running")],
          "derived/rdd.csv", row.names = FALSE)

# ---- the same design, restricted to where the SAME PERSON is on the ballot
# again -- the personal-incumbency version of the close-election design.
# Nothing about the design changes; the sample does. rdd already carries
# personal_incumbent_running from races.csv (built from name-matching, not
# from this step), so this is a filter, not a new construction.
rdd_p <- rdd[rdd$personal_incumbent_running %in% TRUE, ]
pooled_p <- do.call(rbind, lapply(c(20, 10, 5), fit_jump, d = rdd_p))
cat("\npersonal-incumbency close-election estimate, by bandwidth:\n")
print(pooled_p, row.names = FALSE)

# The complement -- the party held the seat but a different person is running
# this time (the old winner retired, lost a primary, or moved on) -- for the
# brief's discussion of how much of the pooled estimate personal incumbency
# accounts for.
rdd_o <- rdd[rdd$personal_incumbent_running %in% FALSE, ]
open_p <- fit_jump(rdd_o, 10)
cat("\nsame comparison where a DIFFERENT person now holds the seat (bandwidth 10):\n")
print(open_p, row.names = FALSE)

# ---- the same design, fit by decade ----------------------------------------
rdd$decade <- 10 * (rdd$year %/% 10)
by_decade <- do.call(rbind, lapply(sort(unique(rdd$decade)), function(dec) {
  r <- fit_jump(rdd[rdd$decade == dec, ], h = 10)
  data.frame(decade = dec, n = r$n, jump = r$jump, se = r$se)
}))
by_decade <- by_decade[by_decade$n >= 30, ]
cat("\nby decade (bandwidth 10):\n"); print(by_decade, row.names = FALSE)
by_decade$jump <- round(by_decade$jump, 4); by_decade$se <- round(by_decade$se, 4)
write.csv(by_decade, "derived/by_decade.csv", row.names = FALSE)

# ---- binned scatter for the brief's figure ---------------------------------
near <- rdd[abs(rdd$lag_margin) <= 20, ]
near$bin <- round(near$lag_margin / 2) * 2
binned <- aggregate(dv ~ bin, data = near, mean)
binned$dv <- round(binned$dv, 3)
binned$n <- as.integer(table(near$bin)[as.character(binned$bin)])
binned$side <- ifelse(binned$bin < 0, "below", "above")
binned <- binned[order(binned$bin), ]
write.csv(binned, "derived/binned.csv", row.names = FALSE)

# ---- the same binning, personal-incumbency sample only ---------------------
near_p <- rdd_p[abs(rdd_p$lag_margin) <= 20, ]
near_p$bin <- round(near_p$lag_margin / 2) * 2
binned_p <- aggregate(dv ~ bin, data = near_p, mean)
binned_p$dv <- round(binned_p$dv, 3)
binned_p$n <- as.integer(table(near_p$bin)[as.character(binned_p$bin)])
binned_p$side <- ifelse(binned_p$bin < 0, "below", "above")
binned_p <- binned_p[order(binned_p$bin), ]
write.csv(binned_p, "derived/binned_personal.csv", row.names = FALSE)

# ---- headline counts the brief quotes verbatim -----------------------------
headline <- data.frame(
  quantity = c("general, non-special, 50-state, non-write-in candidate rows",
               "district-year races, 1976-2024 (50 states, general, non-special)",
               "standard contested two-party races", "uncontested races",
               "nonstandard same-party pairs (top-two / jungle primary)",
               "valid within-cycle pairs", "previous winner on the ballot again",
               "reelected when running again",
               "Minnesota 'Independent-Republican' rows fixed",
               "of those, the only Republican row in their race",
               "runoff district-years resolved to their final round"),
  value = c(GENERAL_ROWS, nrow(races), sum(races$standard), sum(races$uncontested),
            sum(races$nonstandard), nrow(vp), sum(vp$personal_incumbent_running),
            sum(run$personal_incumbent_reelected), MN_TRAP_ROWS, MN_TRAP_PHANTOM,
            length(runoff_keys)))
write.csv(headline, "derived/headline.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Build stamp: records every file under raw/ and derived/ -- size, hash, row
# count, and today's date -- into BUILD-STAMP.tsv. See ../../../_lib/provenance.R.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
