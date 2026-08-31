# build-data.R
# Run with the working directory set to this file's folder (data/).
#
# SOURCE. The ALARM Project's 50-State Redistricting Simulations, Harvard
# Dataverse, doi:10.7910/DVN/SLCD3E: an enacted congressional plan and 5,000
# neutral simulated plans for every state with more than one district.
#
# Downloaded here into a temporary folder, not raw/, because the 44 state
# files run to about 750 MB together, and a course repository has no
# business holding that. Only the small tables this chapter actually prints
# are kept, in derived/.
#
# For every state with more than one congressional district, the dataset
# gives one table with a row per district per redistricting plan. The
# `draw` column says which plan: "cd_2020" is the plan the state actually
# enacted after the 2020 census; the 5,000 draws numbered 1 through 5000 are
# computer-drawn plans that follow the same legal rules -- equal population,
# contiguous, reasonably compact -- but were never told which party or which
# voters they were drawing toward. That is what makes them a fair
# counterfactual for "what would this state's map look like if nobody was
# trying to help anyone."
#
# Two things this script computes:
#
#   1. COMPETITIVENESS. A district is "competitive" at a threshold if its
#      simulated two-party vote share sits within that many points of 50-50.
#      Counted in the enacted plan and averaged across the 5,000 neutral
#      plans, for four thresholds.
#
#   2. BLACK REPRESENTATION UNDER PRESSURE. Restricted to the 20 states
#      Republicans controlled after the 2020 census -- the only states where
#      a Republican-favoring gerrymander is a real possibility, since the
#      party has to hold the pen to draw one. A district counts as
#      "Black-plurality" when Black voting-age population is the largest of
#      every reported racial and ethnic group in it (white, Black, Hispanic,
#      and five smaller categories) -- not merely larger than white VAP
#      alone, which is a narrower and more defensible reading of "Black
#      voters have the most say here" than any single pairwise comparison.
#      Three counts, per state: the enacted plan; the mean across the 5,000
#      neutral plans; and, among all plans tied for that state's highest
#      Republican-seat count, whichever has the fewest Black-plurality
#      districts -- the most adversarial map available inside the ensemble.
#
# Pure base R plus jsonlite, to read the Dataverse file listing.

if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("install.packages('jsonlite') -- needed only to read the Dataverse file listing")

DOI      <- "doi:10.7910/DVN/SLCD3E"
API_LIST <- paste0("https://dataverse.harvard.edu/api/datasets/:persistentId/?persistentId=", DOI)
ENACTED  <- "cd_2020"
THRESHOLDS <- c(0.025, 0.05, 0.075, 0.10)
VAP_GROUPS <- c("vap_white", "vap_black", "vap_hisp", "vap_aian",
                "vap_asian", "vap_nhpi", "vap_other", "vap_two")

GOP_STATES <- c("TX", "FL", "GA", "NC", "SC", "TN", "AL", "MS", "LA", "AR",
                "MO", "KS", "OK", "IA", "NE", "IN", "OH", "KY", "WV", "UT")

dir.create("derived", showWarnings = FALSE)
tmp <- tempfile("alarm_"); dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

# ---- 1. fetch -----------------------------------------------------------
message("Querying Dataverse for the file list...")
meta  <- jsonlite::fromJSON(API_LIST, simplifyDataFrame = FALSE)
files <- meta$data$latestVersion$files
want  <- Filter(function(f) grepl("_cd_2020_stats\\.tab$", f$dataFile$filename), files)
message(sprintf("%d state files to download into a temp folder.", length(want)))

for (f in want) {
  state <- sub("_cd_2020_stats\\.tab$", "", f$dataFile$filename)
  dest  <- file.path(tmp, paste0(state, ".csv"))
  url   <- paste0("https://dataverse.harvard.edu/api/access/datafile/", f$dataFile$id,
                  "?format=original")
  message("  fetching ", state, " ...")
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
}

STATES <- sort(sub("\\.csv$", "", list.files(tmp, pattern = "\\.csv$")))

read_state <- function(state, cols) {
  path <- file.path(tmp, paste0(state, ".csv"))
  hdr  <- names(read.csv(path, nrows = 1, check.names = FALSE))
  cc   <- rep("NULL", length(hdr))
  cc[match("draw", hdr)] <- "character"
  cc[match(cols, hdr)]   <- "numeric"
  read.csv(path, colClasses = cc, check.names = FALSE)
}

# ---- 2. competitiveness ---------------------------------------------------
message("Computing competitiveness by state and threshold...")
comp_rows <- list()
for (state in STATES) {
  d <- read_state(state, c("ndv", "nrv"))
  d$ndshare <- d$ndv / (d$ndv + d$nrv)
  enac <- d[d$draw == ENACTED, ]
  sim  <- d[d$draw != ENACTED, ]
  for (thr in THRESHOLDS) {
    enac_n <- sum(abs(enac$ndshare - 0.5) <= thr)
    per_plan <- tapply(as.integer(abs(sim$ndshare - 0.5) <= thr), sim$draw, sum)
    comp_rows[[length(comp_rows) + 1L]] <- data.frame(
      state = state, n_districts = nrow(enac), threshold = thr,
      enacted = enac_n, neutral_mean = mean(per_plan),
      stringsAsFactors = FALSE)
  }
}
comp <- do.call(rbind, comp_rows)
write.csv(comp, "derived/competitive_by_state.csv", row.names = FALSE)

comp_nat <- do.call(rbind, lapply(THRESHOLDS, function(thr) {
  s <- comp[comp$threshold == thr, ]
  data.frame(threshold = thr, states = nrow(s), districts = sum(s$n_districts),
             enacted = sum(s$enacted), neutral_mean = sum(s$neutral_mean))
}))
write.csv(comp_nat, "derived/competitive_national.csv", row.names = FALSE)

# ---- 3. Black-plurality representation under a maximized gerrymander -----
message("Computing Black-plurality representation for the 20 GOP-controlled states...")
black_rows <- list()
for (state in GOP_STATES) {
  d <- read_state(state, c("ndv", "nrv", "total_vap", VAP_GROUPS))
  vap_mat   <- as.matrix(d[, VAP_GROUPS])
  max_group <- VAP_GROUPS[max.col(vap_mat, ties.method = "first")]
  d$is_black <- max_group == "vap_black"
  d$is_rep   <- (d$ndv / (d$ndv + d$nrv)) < 0.5

  enac <- d[d$draw == ENACTED, ]
  sim  <- d[d$draw != ENACTED, ]
  actual_black <- sum(enac$is_black)

  black_by_plan <- tapply(as.integer(sim$is_black), sim$draw, sum)
  rep_by_plan   <- tapply(as.integer(sim$is_rep),   sim$draw, sum)

  max_rep    <- max(rep_by_plan)
  tied       <- names(rep_by_plan)[rep_by_plan == max_rep]
  gerry_plan <- tied[which.min(black_by_plan[tied])]
  gerry_black <- black_by_plan[[gerry_plan]]

  black_rows[[length(black_rows) + 1L]] <- data.frame(
    state = state, n_districts = nrow(enac),
    actual_black = actual_black, neutral_black_mean = mean(black_by_plan),
    gerry_black = gerry_black, n_plans_tied_at_max = length(tied),
    stringsAsFactors = FALSE)
}
black <- do.call(rbind, black_rows)

# ---- Known data staleness, corrected by hand -----------------------------
# The "cd_2020" plan in this dataset is the map each state enacted right
# after the 2020 census -- used for the 2022 election -- and it predates two
# court orders that added a second Black-opportunity district for 2024
# onward. Alabama's Allen v. Milligan (2023) and Louisiana's Robinson v.
# Ardoin (2024) -- the very map Louisiana v. Callais (2026) later ruled on --
# each ordered a second such district. The dataset's own numbers confirm the
# staleness: Alabama's next-highest district after its one flagged seat
# carries only 29% Black VAP, and Louisiana's only 33% -- both plainly the
# pre-remedy map. This script adds one seat to each state's enacted count by
# hand, which is what "the file is not current" looks like when a chapter
# has to correct for it rather than just report it.
CORRECTION <- c(AL = 1L, LA = 1L)
for (st in names(CORRECTION))
  black$actual_black[black$state == st] <- black$actual_black[black$state == st] + CORRECTION[st]

write.csv(black, "derived/black_by_state.csv", row.names = FALSE)

black_nat <- data.frame(
  states = nrow(black), districts = sum(black$n_districts),
  actual = sum(black$actual_black), neutral_mean = sum(black$neutral_black_mean),
  gerrymander = sum(black$gerry_black))
write.csv(black_nat, "derived/black_national.csv", row.names = FALSE)

# ---- 4. facts.csv, for the inline numbers in the brief --------------------
c5 <- comp[comp$threshold == 0.05, ]
tx <- c5[c5$state == "TX", ]
gap <- c5; gap$gap <- gap$neutral_mean - gap$enacted
top_gap <- gap[order(-gap$gap), ][1, ]

fx <- data.frame(
  key = c(
    "n_states", "n_districts",
    "enacted5", "neutral5", "pct_enacted5", "pct_neutral5",
    "tx_enacted5", "tx_neutral5",
    "top_gap_state", "top_gap_enacted", "top_gap_neutral",
    "gop_n_states", "gop_n_districts",
    "black_actual", "black_neutral", "black_gerry",
    "black_pct_cut",
    "ga_actual", "ga_gerry",
    "single_seat_states"
  ),
  value = c(
    nrow(comp[comp$threshold == 0.05, ]), sum(c5$n_districts),
    sum(c5$enacted), round(sum(c5$neutral_mean), 1),
    round(100 * sum(c5$enacted) / sum(c5$n_districts), 1),
    round(100 * sum(c5$neutral_mean) / sum(c5$n_districts), 1),
    tx$enacted, round(tx$neutral_mean, 1),
    top_gap$state, top_gap$enacted, round(top_gap$neutral_mean, 1),
    nrow(black), sum(black$n_districts),
    sum(black$actual_black), round(sum(black$neutral_black_mean), 1), sum(black$gerry_black),
    round(100 * (sum(black$actual_black) - sum(black$gerry_black)) / sum(black$actual_black), 0),
    black$actual_black[black$state == "GA"], black$gerry_black[black$state == "GA"],
    paste(sort(black$state[black$actual_black == 1]), collapse = ", ")
  ),
  stringsAsFactors = FALSE
)
write.csv(fx, "derived/facts.csv", row.names = FALSE)

# --- WHO DRAWS THE CONGRESSIONAL MAP -----------------------------------------
#
# THE POINT OF THIS TABLE. This chapter compares an enacted map against maps
# nobody steered, and it talks throughout about which party "held the pen."
# That phrasing assumes a legislature is holding it. For a quarter of the
# states with a congressional map to draw, that is not who holds it.
#
# Keyed in from the CONGRESSIONAL column of Exhibit 5.1 of the National
# Conference of State Legislatures' "Redistricting Law 2020" (Denver: NCSL,
# October 2019), plus the sidebar on p. 91 correcting the common belief that
# Iowa has a commission -- it does not; nonpartisan legislative staff draft
# the maps and the General Assembly may accept or reject but not amend the
# first two sets.
#
# NOTHING IS FETCHED FOR THIS. The book is a PDF, so a rebuild cannot catch an
# amendment, and several states changed their process after it went to press.
# The chapter says so.
#
# The at-large label is the 2020 apportionment, not NCSL's: Montana gained a
# second seat in 2020, so its commission now has a congressional map to draw
# and it is counted here as a commission state.

who <- read.delim("raw/ncsl-exhibit-5-1.tsv", sep = "\t", quote = "",
                  stringsAsFactors = FALSE)
stopifnot(nrow(who) == 50L, !any(duplicated(who$state)),
          !any(is.na(who$congressional_authority)))

AT_LARGE <- "One at-large seat, no map to draw"
# The 44 states with a map must be the 44 states the simulations cover.
stopifnot(sum(who$congressional_authority != AT_LARGE) == 44L)

draws <- as.data.frame(table(who$congressional_authority),
                       stringsAsFactors = FALSE)
names(draws) <- c("who_draws_it", "states")
draws <- draws[order(-draws$states), ]
draws <- draws[draws$who_draws_it != AT_LARGE, ]
stopifnot(sum(draws$states) == 44L)
write.csv(draws, "derived/who_draws.csv", row.names = FALSE)
write.csv(who,   "derived/who_draws_states.csv", row.names = FALSE)

message("who_draws.csv: ", paste(draws$states, draws$who_draws_it,
                                 collapse = "; "))

message("Done. Wrote derived/competitive_by_state.csv, competitive_national.csv, ",
        "black_by_state.csv, black_national.csv, facts.csv")
