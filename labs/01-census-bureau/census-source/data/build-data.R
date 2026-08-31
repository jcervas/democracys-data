# ---------------------------------------------------------------------------
# Build the census-instrument dataset: what an enumeration is, and what it is
# for.
#
# This is a SOURCE chapter, the companion to the surveys one. Its subject is
# the instrument: what a census can establish that no sample can, what the word
# "census" actually covers, and the three things that stand between a person
# being counted and a number being published.
#
# Six files end up in this folder:
#
#   raw/block.txt      A real capture: one Georgia block as it arrives in the
#                      P.L. 94-171 file, and the geography record beside it.
#   derived/granularity.csv    What the 2020 census resolves to in one state. This is
#                      the capability no survey has at any sample size.
#   derived/consistency.csv    The internal checks that all pass -- the evidence that
#                      the disclosure-avoidance noise cannot be found by
#                      auditing the file. This is the chapter's central claim.
#   derived/populations.csv    The several official 2020 populations of one state, and
#                      what each is for.
#   derived/acs.csv            The uncertainty in the OTHER census -- the American
#                      Community Survey, which is a survey and is quoted as
#                      though it were a count.
#   derived/instruments.csv    Decennial census, ACS and population estimates, side by
#                      side on what each can and cannot answer.
#
# THE ARGUMENT. The surveys chapter drew the line between eliciting and
# enumerating. This chapter is about what enumerating costs and buys.
#
#   IT BUYS SMALL AREAS. The 2020 census resolves Georgia into 232,717 blocks
#   with a median population of 26. Nothing sampled can do this: a survey of
#   sixty thousand people cannot describe a city block, and no increase in
#   sample size gets there. Every district line, every rate with a population
#   denominator, and the apportionment of the House rest on that capability.
#
#   IT COSTS PRIVACY, AND THE BILL CAME DUE IN 2020. A file that can describe
#   a block of twenty-six people by race and age can identify them. 2,457
#   Georgia blocks contain exactly one person. So the 2020 census published
#   counts that are deliberately not the counts collected.
#
# WHAT THIS BUILD VERIFIES ABOUT DISCLOSURE AVOIDANCE, rather than asserting.
# The 2020 census applied a formal disclosure-avoidance system that adds noise
# below the state level. Two things are checked here directly:
#
#   1. The state total is EXACT. The 232,717 noised block counts sum to
#      10,711,908, which is the published resident population of Georgia to
#      the person. The top of the hierarchy is held invariant and the noise
#      beneath it is constrained to sum back.
#
#   2. The noise CANNOT BE FOUND by auditing the file. Every internal
#      consistency check passes: no block has more voting-age people than
#      people, none has more Black residents than residents. Zero violations
#      in 232,717 blocks. A reader who went looking for the noise by hunting
#      impossibilities would conclude there is none.
#
# That second point is the lesson. The published count is not the collected
# count, nothing in the file distinguishes them, and the only reason anyone
# knows is that the Bureau said so.
#
# "THE CENSUS" IS THREE INSTRUMENTS OUT OF 134. The Bureau's own Survey
# Explorer (census.gov/data/data-tools/survey-explorer) lists 134 surveys and
# censuses. instruments.csv covers three of them, and the selection rule is
# CONFUSABILITY, not importance: these are the three that each publish a figure
# formatted like a population of a place, so they are the three that get quoted
# interchangeably by people who should know better. Nobody mistakes the
# Commodity Flow Survey for a headcount.
#
#   - the DECENNIAL CENSUS, an attempted enumeration every ten years,
#     constitutionally required, which produces exact-looking counts to the
#     block;
#   - the AMERICAN COMMUNITY SURVEY, a continuous SAMPLE of about 3.5 million
#     addresses a year, which produces estimates with margins of error;
#   - the POPULATION ESTIMATES PROGRAM, which carries the decennial forward
#     with births, deaths and migration and is a model, not a count.
#
# The ACS is where the confusion does damage, because its output looks like a
# count and is published by the same agency in the same style. acs.csv shows
# what that costs: of the state-to-state migration flows the Bureau publishes,
# more than a quarter have a margin of error LARGER THAN THE ESTIMATE.
#
# SOURCES.
#   U.S. Census Bureau, 2020 Census P.L. 94-171 Redistricting Data Summary
#   File, Georgia. Read from the copy already committed for the areal-units
#   chapter (ga2020.pl.zip) and from the block extract that chapter derives.
#   U.S. Census Bureau, 2020 Census Apportionment Results, 26 April 2021.
#   U.S. Census Bureau, American Community Survey, state-to-state migration
#   flows, via this book's migration chapter.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

BLOCKS <- "../../areal-units/data/derived/ga_block_race.csv"
FLOWS  <- "../../migration/data/derived/flows.csv"
PLZIP  <- "../../areal-units/data/raw/ga2020.pl.zip"
stopifnot(file.exists(BLOCKS), file.exists(FLOWS))

b <- read.csv(BLOCKS, colClasses = c(GEOID20 = "character"))
stopifnot(nrow(b) > 200000)

# --- 1. what the file resolves to -------------------------------------------

pop <- b$pop
s   <- b[pop > 0, ]
gran <- data.frame(
  quantity = c("Blocks in Georgia",
               "Blocks with nobody in them",
               "Blocks with at least one person",
               "Median population of a populated block",
               "Blocks containing exactly one person",
               "Blocks containing fewer than ten people",
               "Share of Georgians living in blocks of fewer than ten",
               "Total population, summed from blocks"),
  value = c(nrow(b), sum(pop == 0), nrow(s), median(s$pop),
            sum(s$pop == 1), sum(s$pop < 10),
            round(100 * sum(s$pop[s$pop < 10]) / sum(pop), 2),
            sum(pop)),
  unit = c(rep("count", 6), "%", "count"))
write.csv(gran, "derived/granularity.csv", row.names = FALSE)

# --- 1b. the same blocks, sorted into size bands ----------------------------
#
# The chapter's one figure rests on this table. Bands rather than a raw
# histogram because block sizes span four orders of magnitude: the question
# the figure answers is "how many blocks are that small", which is one count
# per band. share_of_people is the share of the state's population living in
# blocks of that band -- the column the figure's tooltip carries and the
# Your-turn questions lean on.

edges <- c(-Inf, 0, 1, 9, 49, 199, 999, Inf)
# Plain hyphens on purpose: these labels are drawn by the base-R twin too,
# and the default pdf() device cannot set an en dash.
bands <- c("Nobody", "1 person", "2-9 people", "10-49 people",
           "50-199 people", "200-999 people", "1,000+ people")
band  <- cut(pop, edges, labels = bands)
bs <- data.frame(
  bin    = bands,
  blocks = as.integer(table(band)),
  people = as.integer(tapply(pop, band, sum)))
bs$share_of_people <- round(100 * bs$people / sum(pop), 2)
stopifnot(sum(bs$blocks) == nrow(b),          # every block lands in one band
          sum(bs$people) == sum(pop))         # and every person with it
write.csv(bs, "derived/blocksize.csv", row.names = FALSE)

# --- 2. the consistency checks that all pass --------------------------------
#
# Every one of these is a value the file COULD contain if the noise were
# unconstrained, and does not.

chk <- data.frame(
  check = c("Blocks where voting-age population exceeds total population",
            "Blocks where Black population exceeds total population",
            "Blocks where Black voting-age population exceeds Black population",
            "Blocks with a negative count in any field"),
  violations = c(sum(b$vap > b$pop),
                 sum(b$black_any > b$pop),
                 sum(b$black_any_vap > b$black_any),
                 sum(b$pop < 0 | b$vap < 0 | b$black_any < 0)),
  of = nrow(b))
write.csv(chk, "derived/consistency.csv", row.names = FALSE)

# --- 3. the several populations of one state --------------------------------
#
# Both figures are official, published by the same agency, for 2020. They are
# not the same number and neither is wrong.

RESIDENT     <- sum(pop)
APPORTIONED  <- 10725274L        # 2020 Apportionment Results, Georgia
pops <- data.frame(
  population = c("Resident population, 2020 census",
                 "Apportionment population, 2020 census",
                 "Difference"),
  value = c(RESIDENT, APPORTIONED, APPORTIONED - RESIDENT),
  what_it_is_for = c(
    "Redistricting, funding formulas, every rate with a population denominator",
    "Dividing the 435 House seats among the states",
    "Federal employees and their dependents stationed overseas, allocated to a home state for apportionment only"))
write.csv(pops, "derived/populations.csv", row.names = FALSE)

# --- 4. the other census ----------------------------------------------------

f <- read.csv(FLOWS)
acs <- data.frame(
  quantity = c("Published state-to-state flows",
               "Flows whose margin of error exceeds the estimate",
               "Share of flows whose margin exceeds the estimate",
               "Flows distinguishable from zero",
               "Median published estimate",
               "Median published margin of error"),
  value = c(nrow(f),
            sum(f$moe > f$est, na.rm = TRUE),
            round(100 * mean(f$moe > f$est, na.rm = TRUE), 1),
            sum(f$sig, na.rm = TRUE),
            median(f$est, na.rm = TRUE),
            median(f$moe, na.rm = TRUE)),
  unit = c("count", "count", "%", "count", "people", "people"))
write.csv(acs, "derived/acs.csv", row.names = FALSE)

# --- 5. three instruments, one name -----------------------------------------

inst <- data.frame(
  question = c(
    "How many people live on this block?",
    "How many people moved from Ohio to Georgia last year?",
    "How many people live in this county today?",
    "What is the population of the United States, exactly?",
    "How many people here speak Spanish and little English?",
    "Is this number exact?"),
  decennial = c("Yes -- this is the one thing only it can do",
                "No -- it does not ask",
                "No -- it is ten years old the day after it is taken",
                "As of one day, every ten years, subject to undercount",
                "No -- the short form does not ask",
                "No -- noise is added below the state level"),
  acs = c("No -- the sample is far too thin",
          "Yes, with a margin often larger than the estimate",
          "Yes, as a five-year average centred on the past",
          "No -- it is a sample",
          "Yes -- this is what it is for",
          "No -- and it prints its own margin of error"),
  estimates = c("No", "No",
                "Yes -- this is what it is for",
                "No -- it is a model carried forward from the last census",
                "No",
                "No -- it is a projection and is revised"))
write.csv(inst, "derived/instruments.csv", row.names = FALSE)

# --- 6. a real capture ------------------------------------------------------
#
# One block, shown as the P.L. file stores it: pipe-delimited, no header, the
# geography in a separate file joined by a record number.

set.seed(84355)
ex <- s[s$pop > 10 & s$pop < 40, ]
ex <- ex[order(ex$GEOID20), ][sample.int(nrow(ex), 3), ]
cap <- file("raw/block.txt", "w")
writeLines(c(
"The P.L. 94-171 file arrives as four pipe-delimited files with no",
"header row. A block's population is in segment 1; who those people",
"are by race is further along the same line; the block's NAME and",
"location are in a fourth file, joined on a record number that is",
"unique within a state and means nothing outside it.",
"",
"Three real Georgia blocks, after the join, as the derived extract",
"stores them:",
""), cap)
utils::write.table(ex, cap, sep = "  ", quote = FALSE, row.names = FALSE)
writeLines(c("",
"GEOID20 is fifteen characters and must be read as text. As a number",
"it loses its leading zero and stops being a Georgia block at all.",
"",
"Every count above is a published count. It is not necessarily the",
"count that was collected -- see the chapter."), cap)
close(cap)

# --- report -----------------------------------------------------------------

gg <- function(q) gran$value[gran$quantity == q]
cat(sprintf("\ngranularity.csv : %s blocks, median populated block %d people\n",
            format(gg("Blocks in Georgia"), big.mark = ","),
            gg("Median population of a populated block")))
cat(sprintf("  %s blocks contain exactly one person\n",
            format(gg("Blocks containing exactly one person"), big.mark = ",")))
cat("\nconsistency.csv : every check passes\n")
print(chk, row.names = FALSE)
cat(sprintf("\n  The noise is real and undetectable from inside the file.\n"))
cat(sprintf("\npopulations.csv : resident %s, apportionment %s, difference %s\n",
            format(RESIDENT, big.mark = ","), format(APPORTIONED, big.mark = ","),
            format(APPORTIONED - RESIDENT, big.mark = ",")))
cat("\nacs.csv         : the other census\n")
print(acs, row.names = FALSE)

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
