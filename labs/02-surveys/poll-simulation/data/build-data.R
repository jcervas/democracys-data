# ---------------------------------------------------------------------------
# Build the poll-simulation dataset.
#
# One file ends up in this folder:
#
#   derived/pres2024_states.csv   the same state-level 2024 results used in Weeks 1, 2
#                         and 6, copied so this lab stands on its own
#
# This lab is mostly a SIMULATION, which is deliberate. Every other lab in the
# course hands you real data and asks what it shows. This one builds a
# population where we already know the answer, then polls it — because the only
# way to see how badly a poll can miss is to know the truth in advance, and in
# real life you never do.
#
# The real 2024 result is the truth the simulation is built around:
#
#   Trump   77,302,580   49.8%   ->  50.75% of the two-party vote
#   Harris  75,017,613   48.3%   ->  49.25% of the two-party vote
#   margin: 1.5 points
#
# (Source: the national file built by `historical-campaigns`, from jaytimm's compilation.)
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

src <- file.path("..", "..", "..", "03-elections", "electoral-map", "data", "derived/pres2024_states.csv")
stopifnot(file.exists(src))

st <- read.csv(src, stringsAsFactors = FALSE)
stopifnot(nrow(st) == 51)
write.csv(st, "derived/pres2024_states.csv", row.names = FALSE)

cat("wrote pres2024_states.csv:", nrow(st), "rows\n")

# --- The demonstration this lab is built on ---------------------------------
#
# A poll of 1,000 has a margin of error of about 3 points. A poll of 100,000
# has one of about 0.3. Neither number tells you anything at all about bias.

set.seed(1)
truth <- 0.4925                      # Harris share of the two-party vote

show <- function(n, resp_gap = 0) {
  # resp_gap: how much likelier a Harris voter is to answer the phone
  pop <- c(rep(1, round(1e6 * truth)), rep(0, 1e6 - round(1e6 * truth)))
  w   <- ifelse(pop == 1, 1 + resp_gap, 1)
  s   <- sample(pop, n, prob = w, replace = TRUE)
  moe <- 1.96 * sqrt(0.25 / n) * 100
  c(n = n, estimate = round(100 * mean(s), 2), moe = round(moe, 2),
    error = round(100 * mean(s) - 100 * truth, 2))
}

cat("\nno non-response problem:\n")
for (n in c(1000, 10000, 100000)) print(show(n))

cat("\nHarris voters 20% likelier to respond:\n")
for (n in c(1000, 10000, 100000)) print(show(n, resp_gap = 0.20))

cat("\nNote what happens to `moe` and what happens to `error`.\n")
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
