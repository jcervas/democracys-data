# ---------------------------------------------------------------------------
# Build the bisg-check dataset: BISG checked against the truth.
#
# Two files end up in this folder:
#
#   derived/houston_voters.csv   One row per registered voter in Houston County, GA,
#                        with SURNAME, SELF-REPORTED RACE, and CENSUS BLOCK.
#   derived/houston_blocks.csv   Population by race for each of the county's blocks.
#
# WHY THIS COUNTY. Georgia is one of a small number of states that records
# race on the voter registration form -- collected for Voting Rights Act
# compliance, not for researchers. That makes it one of the only places where
# BISG can be *graded*. Everywhere else you infer race and never find out.
#
# SOURCE. Houston County, GA voter registration and the geocoded block
# assignment, from the working data for the 2026 Houston County redistricting
# matter. Block-level census counts are 2020 PL 94-171 / DHC tables for
# Georgia.
#
# COLUMNS DROPPED ON PURPOSE. The registration file has 53 columns including
# full name, birth year, street address and mailing address. None of that is
# needed to score a surname model, so only three fields are carried forward:
# surname, race, block. First names, addresses and registration numbers are
# not in the committed file.
#
# TWO DEFINITIONAL CHOICES, both of which move the numbers:
#
#   * BLACK is ANY-PART BLACK (alone or in combination, including Hispanic
#     Black). This matches Voting Rights Act practice and matches what a voter
#     ticking one box on a form most likely means. The alternative -- Black
#     alone, not Hispanic -- gives 51,992 for this county against 56,520 for
#     any-part, a gap of 4,528 people, or 8.7%. Same census, same county.
#
#   * The five categories are therefore NOT perfectly mutually exclusive: a
#     Hispanic Black resident is counted in both. BISG renormalises across
#     categories anyway, so the effect is small, but it is real and it is the
#     kind of thing that is never mentioned in a methods section.
#
# GROUND TRUTH IS SELF-REPORT, AND ONLY SELF-REPORT. The source file contains
# an `is_bisg_imputed` flag: some records had race filled in *by BISG* where
# the voter had not supplied it. Those rows are dropped here. Scoring BISG
# against BISG would produce a beautiful and meaningless result.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE, scipen = 999)

SRC <- file.path("/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu",
                 "My Drive/Redistricting/2026/Houston County/Superseding Report/data")
stopifnot(dir.exists(SRC))

# --- block-level population by race ----------------------------------------
b <- as.data.frame(readRDS(file.path(SRC, "census/census.ga.rds"))[["GA"]]$block)
b$GEOID20 <- paste0("13", b$county, b$tract, b$block)      # RDS stores state as "GA"

pb <- read.csv(file.path(SRC, "census/pop_blocks.csv"), colClasses = c(GEOID20 = "character"))
b  <- merge(b, pb[, c("GEOID20", "pop_black")], by = "GEOID20")
stopifnot(nrow(b) > 3000)

# P12I is White alone non-Hispanic; it matches pop_white exactly, which is the
# check that the table codes mean what we think. P12J (Black alone NH) does
# NOT match pop_black -- see the definitional note above.
# (compare values, not storage types -- one file reads as numeric, the other integer)
stopifnot(isTRUE(all.equal(as.numeric(b$P12I_001N),
                           as.numeric(pb$pop_white[match(b$GEOID20, pb$GEOID20)]))))

blocks <- data.frame(
  GEOID20  = b$GEOID20,
  white    = b$P12I_001N,                       # White alone, not Hispanic
  black    = b$pop_black,                       # ANY-PART Black
  hispanic = b$P12H_001N,                       # Hispanic, any race
  asian    = b$P12L_001N + b$P12M_001N,         # Asian + NHPI alone, NH
  aian     = b$P12K_001N                        # AIAN alone, NH
)
write.csv(blocks, "derived/houston_blocks.csv", row.names = FALSE)

# --- voters -----------------------------------------------------------------
m   <- read.csv(file.path(SRC, "voter_race_master.csv"), colClasses = "character")
reg <- read.csv(file.path(SRC, "voter-reg/71754 - Houston County.csv"), colClasses = "character")

map <- c("White" = "white", "Black" = "black", "Hispanic" = "hispanic",
         "Asian/Pacific Islander" = "asian",
         "American Indian/Alaskan Native" = "aian")

# --- what the sources looked like before any of this --------------------------
# The brief shows the raw registration extract's shape. It cannot read the
# extract itself, which is not public and stays out of this folder, so the two
# things it can honestly show are recorded here: the column NAMES (structure,
# not data) and a handful of counts. No row of the extract is copied out.
dir.create("raw", showWarnings = FALSE)
writeLines(readLines(file.path(SRC, "voter-reg/71754 - Houston County.csv"),
                     n = 1), "raw/voter-reg-header.txt")
write.csv(data.frame(
  name  = c("reg_rows", "reg_cols", "master_rows", "imputed_rows",
            "other_unknown_rows", "no_geoid_rows"),
  value = c(nrow(reg), ncol(reg), nrow(m),
            sum(m$is_bisg_imputed == "TRUE"),
            sum(m$is_bisg_imputed == "FALSE" & !(m$race_self %in% names(map))),
            sum(m$is_bisg_imputed == "FALSE" & m$race_self %in% names(map) &
                  !nzchar(m$GEOID20)))),
  "raw/source-shape.csv", row.names = FALSE)

m <- m[m$is_bisg_imputed == "FALSE" &        # self-report only
       m$race_self %in% names(map) &         # drops Other and Unknown
       nzchar(m$GEOID20), ]
m$race <- map[m$race_self]

reg$surname <- toupper(trimws(reg$Last.Name))
v <- merge(m[, c("Voter.Registration.Number", "race", "GEOID20")],
           reg[, c("Voter.Registration.Number", "surname")],
           by = "Voter.Registration.Number")

v <- v[v$GEOID20 %in% blocks$GEOID20 & nzchar(v$surname), ]
v <- v[, c("surname", "race", "GEOID20")]          # <- identifiers dropped here
v <- v[order(v$surname), ]
write.csv(v, "derived/houston_voters.csv", row.names = FALSE)

cat("wrote houston_voters.csv:", nrow(v), "voters,",
    length(unique(v$GEOID20)), "blocks\n")
cat("wrote houston_blocks.csv:", nrow(blocks), "blocks\n\n")
print(sort(table(v$race), decreasing = TRUE))
cat("\ncounty population by race (the geographic prior):\n")
print(sapply(blocks[, -1], sum))
cat("\nNote: no name, address, birth year or registration number is carried\n")
cat("into the committed file. Three columns only.\n")

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
