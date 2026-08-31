# ---------------------------------------------------------------------------
# Build the rpv dataset: ecological inference checked against the truth.
#
# One file ends up in this folder:
#
#   derived/houston_primary.csv   One row per county precinct in Houston County, GA,
#                         for the 21 May 2024 General Primary. Carries what an
#                         expert witness would observe (racial composition of
#                         the electorate, and the D/R ballot split) and, in two
#                         extra columns, the answer they are trying to infer.
#
# WHY THIS ELECTION. Ecological inference exists because the ballot is secret:
# you know a precinct is 40% Black and gave 45% of its ballots to the Democrat,
# and from that you must infer how Black voters behaved. Normally the inference
# can never be checked -- that is the whole problem.
#
# A PARTISAN PRIMARY IS THE EXCEPTION. In Georgia, requesting a party's primary
# ballot is a matter of public record, and so is self-reported race. So for this
# one election we know, voter by voter, the thing ecological inference is trying
# to estimate. That makes it gradeable, in exactly the way the `bisg-check` chapter
# was gradeable, and for the same underlying reason: Georgia collects race for
# Voting Rights Act compliance.
#
# WHAT THIS IS NOT. Requesting a Democratic primary ballot is not the same as
# voting Democratic in November, and a primary electorate is not the general
# electorate -- 18,133 people voted in this primary against 82,114 in the
# general. The lab is about whether the METHOD recovers a known quantity, not
# about how Houston County votes in November.
#
# COLUMNS DROPPED ON PURPOSE. Nothing individual-level is written out. The
# registration file has 50+ columns including full name, birth year and street
# address; the vote-history file is one row per person. All of it is collapsed
# to 17 precinct rows before anything is saved. Unlike the `bisg-check` file, this
# one contains no individual records at all.
#
# THREE DEFINITIONAL CHOICES, all of which move the numbers:
#
#   * TWO GROUPS ONLY. Restricted to voters whose self-reported race is Black
#     or White, which is standard in Section 2 cases (minority vs non-minority)
#     and is what makes a two-column ecological analysis well posed. It drops
#     1,135 of 17,805 primary voters (6.4%) -- Hispanic, Asian, American
#     Indian, Other, Unknown. Those people voted; they are simply not in a
#     framework that has two columns.
#
#   * NO BISG ANYWHERE. Rows whose race was filled in by imputation rather
#     than reported by the voter (`is_bisg_imputed`) are dropped. Grading an
#     inference against another inference would produce a beautiful and
#     meaningless result -- the same trap flagged in the `bisg-check` builder.
#
#   * NON-PARTISAN BALLOTS EXCLUDED. 328 voters took a primary ballot without
#     requesting either party's. They are not Democrats or Republicans and are
#     not counted as either.
#
# SOURCE. Houston County, GA voter registration (extract 71754), the state's
# voter-history files, and self-reported race, from the working data for the
# 2026 Houston County redistricting matter.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

D <- file.path("/Users/cervas/Library/CloudStorage",
               "GoogleDrive-jcervas@andrew.cmu.edu/My Drive/Redistricting",
               "2026/Houston County/Superseding Report/data")

# Registration numbers appear with and without leading zeros across vintages.
nid <- function(x) as.character(as.integer(trimws(as.character(x))))

# --- who voted, and in which party's primary -------------------------------
h <- read.csv(file.path(D, "vote-history", "2024.csv"),
              check.names = FALSE, colClasses = "character")

# 03/12/2024 and 05/21/2024 BOTH carry the label "GENERAL PRIMARY" in the raw
# file -- only the date separates the presidential preference primary from the
# general primary. Filtering on ElectionType alone would silently merge them.
h <- h[h$"Election Date" == "05/21/2024" &
       h$Party %in% c("DEMOCRAT", "REPUBLICAN"), ]
h$id <- nid(h$"Voter Registration Number")
h <- h[!duplicated(h$id), ]        # the file repeats a row per ballot style

# --- self-reported race, imputation removed --------------------------------
m <- read.csv(file.path(D, "voter_race_master.csv"), colClasses = "character")
m$id <- nid(m$Voter.Registration.Number)
m <- m[m$is_bisg_imputed == "FALSE" & m$race_self %in% c("Black", "White"), ]

# --- precinct --------------------------------------------------------------
r <- read.csv(file.path(D, "voter-reg", "71754 - Houston County.csv"),
              check.names = FALSE, colClasses = "character")
r$id <- nid(r$"Voter Registration Number")
r$precinct <- trimws(r$"County Precinct Description")

x <- merge(h[, c("id", "Party")],   m[, c("id", "race_self")], by = "id")
x <- merge(x, r[, c("id", "precinct")], by = "id")

stopifnot(nrow(x) > 15000)

# --- collapse to precincts; nothing individual survives this line -----------
f <- function(cond) as.vector(tapply(cond, x$precinct, sum))
p <- data.frame(
  precinct  = sort(unique(x$precinct)),
  voters    = f(rep(1, nrow(x))),
  black     = f(x$race_self == "Black"),
  white     = f(x$race_self == "White"),
  dem       = f(x$Party == "DEMOCRAT"),
  rep       = f(x$Party == "REPUBLICAN"),
  # The two columns the method is trying to infer. Kept so the lab can be
  # graded; not used until Part 6.
  black_dem = f(x$race_self == "Black" & x$Party == "DEMOCRAT"),
  white_dem = f(x$race_self == "White" & x$Party == "DEMOCRAT"),
  stringsAsFactors = FALSE)

# With two races and two parties these must be exact, not approximate. If a
# third category ever leaks in, this is where it shows up.
stopifnot(all(p$voters == p$black + p$white),
          all(p$voters == p$dem   + p$rep),
          all(p$black_dem <= p$black), all(p$white_dem <= p$white))

# --- the join, shown once, before it is collapsed --------------------------
#
# THE CHAPTER'S ARGUMENT REQUIRES THAT THE INDIVIDUAL LEVEL BE VISIBLE EXACTLY
# ONCE. What ecological inference is trying to recover is the joined record --
# race, party, precinct, one voter at a time -- and a reader who never sees it
# has no picture of what is being estimated. So four joined records are written
# here with the registration number REPLACED by a row label. No name, no
# address and no registration number is in this folder; the four rows carry
# only the three columns the method is about, which are the three columns
# every precinct total below is built from.
set.seed(84355)
jp <- x[sort(sample.int(nrow(x), 4)), c("Party", "race_self", "precinct")]
jp <- data.frame(voter = paste0("voter ", seq_len(nrow(jp))), jp,
                 stringsAsFactors = FALSE)
write.csv(jp, "derived/join_peek.csv", row.names = FALSE)

# What each of the three sources contributed, and how many rows survived each
# step. The chapter quotes these rather than asserting them.
jc <- data.frame(
  step = c("Vote-history rows, 21 May 2024, D or R ballot",
           "Self-reported race, imputation removed, Black or white",
           "Registration extract rows carrying a precinct",
           "Joined on registration number, all three present",
           "Precinct rows written"),
  rows = c(nrow(h), nrow(m), nrow(r), nrow(x), nrow(p)),
  stringsAsFactors = FALSE)
write.csv(jc, "derived/join_counts.csv", row.names = FALSE)

write.csv(p, "derived/houston_primary.csv", row.names = FALSE)

cat(sprintf("%d precincts, %d voters\n", nrow(p), sum(p$voters)))
cat(sprintf("TRUTH  black Dem %.1f%%   white Dem %.1f%%\n",
            100 * sum(p$black_dem) / sum(p$black),
            100 * sum(p$white_dem) / sum(p$white)))

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
