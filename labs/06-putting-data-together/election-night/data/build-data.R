# ---------------------------------------------------------------------------
# Build the election-night datasets.
#
# THIS LAB IS DIFFERENT FROM THE OTHERS. It is taught on Thursday 5 November
# 2026, two days after an election that -- at the time this script was written
# -- had not happened yet. So it is built in two halves:
#
#   derived/senate_2026_landscape.csv   THE MAP, buildable today. Every Class 2 Senate
#                               seat up in 2026, who holds it, and how the state
#                               voted for president in 2024.
#
#   derived/rehearsal_house_2024.csv    THE METHOD, rehearsed on data that exists. The
#                               2024 House result in each district next to the
#                               presidential baseline for that district. Every
#                               post-election chunk in the lab runs on this, so
#                               the code is proven before election night.
#
# ON 5 NOVEMBER 2026: run build_results_2026() below, then change ONE line at
# the top of Part 4 of the lab. Everything else is already working.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# SIBLING DATA, AND THE ONE ORDERING CONFLICT IN THE BOOK.
#
# Two files this script reads are built by other chapters. Reading a
# neighbour's derived/ is ordinary here -- roughly forty builds in the corpus
# do it, and `course-map.md` lists them all. Fifteen of those read a file whose
# chapter is taught later, and fourteen are filing quirks: nothing forces the
# reader to have met the other chapter, and a rebuild in session order is a
# convenience, not a requirement.
#
# This one is different, and it is the reason the helper below exists.
# `redistricting` owns pres_by_cd_2024.csv and is taught in Session 14, on
# 1 December. This lab is taught in Session 10, on 5 November, because that is
# when the election is -- the date cannot move. Wipe derived/ and rebuild the
# corpus in session order and this script reaches for a file that will not be
# built for another month. It would fail two days after an election, which is
# the worst morning of the year to be reading a path expression.
#
# The fix is not to copy redistricting's recipe here, which would leave two
# places to fix when the source moves. It is to run the OWNER's build script.
# The owner keeps the recipe; this build just declines to be blocked by the
# calendar. If that cannot be done, the error says which chapter to build and
# how, instead of "file.exists(cd_path) is not TRUE".
# ---------------------------------------------------------------------------
sibling <- function(path, owner, note) {
  if (file.exists(path)) return(path)

  build <- file.path(dirname(dirname(path)), "build-data.R")
  cat("\n", basename(path), " is not here yet.\n",
      "  It belongs to `", owner, "`. ", note, "\n",
      "  Running that chapter's own build script rather than duplicating it:\n",
      "    ", build, "\n\n", sep = "")

  if (!file.exists(build))
    stop(basename(path), " is missing and so is the script that makes it (",
         build, "). Build `", owner, "` by hand, then re-run this.",
         call. = FALSE)

  # chdir = TRUE because the owner's script addresses its own raw/ and derived/
  # relatively; local = a fresh environment so its variables cannot land in
  # ours. It fetches from the network only if its own raw/ is empty.
  source(build, chdir = TRUE, local = new.env())

  if (!file.exists(path))
    stop("`", owner, "`'s build ran but did not produce ", basename(path),
         ". Check that build for an error -- it may have been unable to reach ",
         "its source.", call. = FALSE)

  cat("  built ", basename(path), ".\n\n", sep = "")
  path
}

# ===========================================================================
# PART 1 -- THE 2026 MAP  (runs today)
# ===========================================================================
#
# Sources:
#   @unitedstates/congress-legislators, legislators-current.csv
#     https://unitedstates.github.io/congress-legislators/legislators-current.csv
#   2024 presidential results by state -- the `electoral-map` file.
#
# The Senate is divided into three classes; one class faces the voters every
# two years. Class 2 is up in 2026. Which seats those are is a fact about the
# calendar, not a prediction, so it can be built months in advance.

leg_url <- "https://unitedstates.github.io/congress-legislators/legislators-current.csv"
leg <- read.csv(leg_url, stringsAsFactors = FALSE)

sen <- leg[leg$type == "sen" & leg$senate_class == 2, ]
cat("Class 2 Senate seats up in 2026:", nrow(sen), "\n")

# Class 2 is not the whole ballot. A seat vacated mid-term is filled at the
# next general election whatever class it belongs to, and 2026 has two: the
# Florida and Ohio seats given up by Rubio and Vance, held in the meantime by
# appointees. They are Senate races on the 2026 ballot in every sense that
# matters on election night -- results arrive, a winner is called -- so a
# landscape built only from senate_class == 2 would leave a reader with
# nowhere to put two of the night's results. The senate-2026 brief counts the
# same 35 contests; if these two lists ever disagree again, one of them is
# wrong about what is on the ballot.
# Both vacancies are class 3 seats, so select the seat rather than the state:
# Florida and Ohio each still have a class 1 senator who is NOT on this ballot.
SPECIALS <- data.frame(state = c("FL", "OH"), senate_class = c(3L, 3L))
sp <- merge(leg[leg$type == "sen", ], SPECIALS)
stopifnot(nrow(sp) == nrow(SPECIALS), !any(sp$state %in% sen$state))
sen <- rbind(sen, sp[, names(sen)])
cat("plus", nrow(sp), "special elections:", paste(sp$state, collapse = " "),
    "-- total on the 2026 ballot:", nrow(sen), "\n")
print(table(sen$party))

st_path <- file.path("..", "..", "..", "03-elections", "electoral-map", "data",
                     "derived", "pres2024_states.csv")
# Session 6, so in session order this one is already built. Routed through the
# helper anyway: a rebuild does not have to happen in session order.
st_path <- sibling(st_path, "electoral-map",
                   "It is taught in Session 6, ahead of this lab.")
st <- read.csv(st_path, stringsAsFactors = FALSE)

land <- merge(
  sen[, c("state", "last_name", "first_name", "party")],
  st[, c("abbrev", "state", "margin", "ev")],
  by.x = "state", by.y = "abbrev")
names(land) <- c("state", "senator_last", "senator_first", "party",
                 "state_name", "pres24_margin", "ev")

# Positive margin = Trump won the state in 2024.
land$pres24_winner <- ifelse(land$pres24_margin > 0, "Trump", "Harris")
land$abs_margin    <- abs(land$pres24_margin)

# A seat is "crossover" when the party holding it lost that state for president.
land$hostile_turf <- (land$party == "Republican" & land$pres24_winner == "Harris") |
                     (land$party == "Democrat"   & land$pres24_winner == "Trump")

land <- land[order(land$abs_margin), ]

cat("\nseats where the holding party lost the state in 2024:",
    sum(land$hostile_turf), "\n")
print(land[land$hostile_turf,
           c("state", "senator_last", "party", "pres24_winner", "pres24_margin")],
      row.names = FALSE)

write.csv(land, "derived/senate_2026_landscape.csv", row.names = FALSE)

# IMPORTANT CAVEAT, carried into the lab:
# `senator_last` is whoever holds the seat right now. It is NOT necessarily
# who will be on the ballot. Retirements, primary defeats, appointments and
# deaths all break that assumption, and several will have happened between
# this file being built and the election. Treat the column as "the seat
# currently held by" and check the actual candidates before relying on it.

# ===========================================================================
# PART 2 -- THE REHEARSAL  (runs today, on 2024)
# ===========================================================================
#
# The post-election half of the lab asks one question: where did the result
# differ from what the baseline implied, and what explains the gap? That
# question can be rehearsed on 2024, where both halves already exist.

cd_path <- file.path("..", "..", "redistricting", "data", "derived",
                     "pres_by_cd_2024.csv")
# The ordering conflict described at the top of this file: Session 14 owns this
# file, and this lab runs in Session 10 on a date that cannot move.
cd_path <- sibling(cd_path, "redistricting",
                   "It is taught in Session 14, five weeks after this lab.")
cd <- read.csv(cd_path, stringsAsFactors = FALSE)

reh <- cd[!is.na(cd$house_rep_party),
          c("district", "state", "cd", "dem_share", "pres_party",
            "house_rep", "house_rep_party")]

# The baseline prediction: whoever won the district for president "should"
# win the House seat.
reh$baseline   <- reh$pres_party
reh$actual     <- reh$house_rep_party
reh$as_expected <- reh$baseline == reh$actual

cat("\nrehearsal districts:", nrow(reh), "\n")
cat("seats that went as the presidential baseline implied:",
    sum(reh$as_expected), "=",
    round(100 * mean(reh$as_expected), 1), "%\n")
cat("seats that did not:", sum(!reh$as_expected), "\n")

write.csv(reh, "derived/rehearsal_house_2024.csv", row.names = FALSE)

# ===========================================================================
# PART 3 -- AFTER THE ELECTION  (run this on 5 November 2026)
# ===========================================================================
#
# There is no national source of live election returns -- as `data-sources` explained
# at length, the United States has no national election administrator. The
# options on the morning of 5 November, in rough order of preference:
#
#   1. The Associated Press results pages, if you have access.
#   2. A compilation such as jaytimm/PresElectionResults, which has been
#      reliable and is the source used in Weeks 4 and 15. It will not be
#      updated within two days of the election.
#   3. Wikipedia's "2026 United States Senate elections" article, which is
#      updated fast and is usually accurate for called races. Fine for a
#      classroom, not for publication.
#   4. Typing 33 numbers in by hand from a news site. This takes ten minutes
#      and is completely defensible. Say in class that you did it.
#
# Whichever you use, the lab needs a file with these columns:
#
#   state          two-letter abbreviation
#   winner_party   "Democrat" or "Republican"
#   dem_pct        Democratic share of the two-party vote, a number
#
# Write it to results_senate_2026.csv in this folder. The lab's Part 4 then
# needs one line changed. A template with the right columns and NA values is
# written below so the shape is unambiguous.

template <- data.frame(
  state        = land$state,
  winner_party = NA_character_,
  dem_pct      = NA_real_)
write.csv(template, "derived/results_senate_2026_TEMPLATE.csv", row.names = FALSE)
cat("\nwrote results_senate_2026_TEMPLATE.csv --",
    nrow(template), "rows awaiting 3 November\n")

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
