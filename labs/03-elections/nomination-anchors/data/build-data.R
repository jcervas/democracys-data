# ---------------------------------------------------------------------------
# Build the data for the `nomination-anchors` chapter.
#
#   raw/table-10-1-motives.csv       55 nominations: candidate, anchor, motive
#   raw/table-05-2-coordination.csv  32 races where coordination was observed
#   raw/table-09-1-faction.csv       33 Republican nominees: faction, PAC money
#   raw/table-06-marginals.csv       the anchoring counts, as printed
#   raw/chapter-6-assignments.csv    the per-race anchor forms stated in prose
#
#   derived/nominations.csv   the joined table, one row per nomination
#   derived/by_anchor.csv     anchor form x motive
#   derived/by_faction.csv    faction x coordination, Republicans
#   derived/pac_by_anchor.csv business PAC money by anchor form, Republicans
#   derived/checks.csv        the validation results
#
# SOURCE. Bawn, K., Brown, K., Ocampo, A. X., Patterson, S. Jr., Ray, J. L. and
# Zaller, J. (2026). *Parties on the Ground: A Study of Nominations for the
# House of Representatives.* University of Chicago Press.
# doi:10.7208/chicago/9780226853314
#
# THE SOURCE IS A BOOK, WHICH IS WHY raw/ LOOKS LIKE THIS. There is no file to
# download. The evidence is printed as twenty-three tables across eleven
# chapters, and the four that share a key are transcribed here. What sits in
# raw/ is therefore a transcription, not a capture: it is what a person read off
# a printed page. Those pages are the record, and they are cited above.
#
# The dollar figures, race codes, candidate names and counts are facts, and
# facts are what this reads. The CLASSIFICATIONS -- which candidate counts as an
# insurgent, what an anchor is, which motive applies -- are the authors' own
# judgments, developed over 346 interviews. Nothing here re-does that work or
# claims it. Every classification is theirs and is labelled as theirs.
#
# THE COLUMN THAT WAS NEVER PRINTED. Tables 6.2 and 6.3 report how many winners
# were anchored by each form -- 28 by coordination, 9 by money, 6 by grassroots
# campaigning, 4 by campaign know-how, 8 by nothing the authors could identify.
# They do not say WHICH race is which. That column exists nowhere in the book.
#
# It is recoverable, and recovering it is most of what this script does:
#
#   1. Table 5.2 names 32 races and annotates some "(1st and 2nd)" or "(2nd)",
#      marking races where the coordinated candidate lost. Expanding those
#      annotations gives 34 candidates, 30 of them winners -- which is what
#      chapter 6 says in words.
#   2. Chapter 6 moves two of those 30 winners to the money column, saying so
#      by name. 30 - 2 = 28.
#   3. Chapter 6 names the other seven money anchors, the four EMILYs List
#      winners, and the grassroots cases, in prose. Those are in
#      raw/chapter-6-assignments.csv with the sentence each came from.
#   4. Whatever is left over is the answer.
#
# Four counts then have to land on four numbers the book already printed, and
# they are asserted below rather than hoped for. If a future correction to the
# book changes any of them, this build stops.
#
# TWO ERRATA, corrected here and carried in a column so no reader loses them:
#
#   * Table 9.1c prints `FL-19  Jolly`. David Jolly won FL-13; Curt Clawson won
#     FL-19. Chapters 5, 8 and 10 all use FL-13R for Jolly.
#   * Table 10.1 prints `MI-4D  Lawrence`. Brenda Lawrence won MI-14D, and MI-4R
#     is Moolenaar, who appears separately in the same table.
#
#   Both were found by joining. Neither is visible in the table it appears in,
#   because a district code is only wrong relative to another district code.
#
# ONE DISAGREEMENT BETWEEN TABLES, flagged and not resolved. Table 10.1 gives
# CA-45R, TX-36R and WI-6R an anchor. Tables 6.2 and 6.3 count all three among
# the eight winners with no anchor. Chapter 8 explains two of them: they fell
# under a dollar threshold, and "if New Majority had spent as much on Mimi
# Walters as Club for Growth regularly spent on its candidates ... we would have
# classified these races as anchored." Chapter 6 says the third was left open
# for want of interviews. Both readings are kept, in `anchor` and `anchor_form`,
# and the rows are marked.
#
# Run from inside this data/ folder:  Rscript build-data.R
# ---------------------------------------------------------------------------

options(scipen = 999, stringsAsFactors = FALSE)
dir.create("derived", showWarnings = FALSE)

CK <- data.frame(check = character(), value = character())
add <- function(k, v) CK[nrow(CK) + 1L, ] <<- c(k, as.character(v))

rd <- function(f) read.csv(file.path("raw", f), stringsAsFactors = FALSE)
MO <- rd("table-10-1-motives.csv")
CO <- rd("table-05-2-coordination.csv")
FA <- rd("table-09-1-faction.csv")
MG <- rd("table-06-marginals.csv")
A6 <- rd("chapter-6-assignments.csv")

stopifnot(nrow(MO) == 55, nrow(CO) == 32, nrow(FA) == 33)
add("nominations in the study", nrow(MO))
add("of those, races where coordination was observed", nrow(CO))
add("Republican nominees with a faction code", nrow(FA))

# --- 1. the key -------------------------------------------------------------
# A race code is state, district, party: PA-13D is Pennsylvania's thirteenth,
# Democratic side. Montana's at-large seat is MT-AL, so the district part is
# not always a number and is not treated as one.
MO$state    <- sub("-.*", "", MO$race)
MO$party    <- substr(MO$race, nchar(MO$race), nchar(MO$race))
MO$district <- substr(sub("^[^-]*-", "", MO$race), 1,
                      nchar(sub("^[^-]*-", "", MO$race)) - 1)
stopifnot(all(MO$party %in% c("D", "R")))
add("Republican nominations", sum(MO$party == "R"))
add("Democratic nominations", sum(MO$party == "D"))

# Every race named in a subsidiary table must exist in the spine. This is the
# check that found both errata: a district code that matches nothing.
orphan <- setdiff(c(CO$race, FA$race, A6$race), MO$race)
if (length(orphan))
  stop("race code(s) in a subsidiary table with no row in table 10.1: ",
       paste(orphan, collapse = ", "))
add("race codes in subsidiary tables matching no nomination", length(orphan))

# --- 2. coordination, and who it was for ------------------------------------
# "1st and 2nd" is two candidates in one race. Expanding the annotation is what
# makes the winner count recoverable.
CO$n_candidates <- ifelse(CO$beneficiary == "1st and 2nd", 2L, 1L)
CO$for_winner   <- grepl("1st", CO$beneficiary)
CO$for_loser    <- grepl("2nd", CO$beneficiary)

NCAND <- sum(CO$n_candidates)
NWIN  <- sum(CO$for_winner)
NLOSE <- sum(CO$for_loser)
add("candidates with coordinated support", NCAND)
add("of those, who won the nomination", NWIN)
add("of those, who lost", NLOSE)
stopifnot(NCAND == 34, NWIN == 30, NLOSE == 4)   # ch.6, in words

# --- 3. the column that was never printed -----------------------------------
coord_win <- CO$race[CO$for_winner]
named     <- setNames(A6$anchor_form, A6$race)

MO$anchor_form <- ifelse(
  MO$race %in% names(named), named[MO$race],
  ifelse(MO$race %in% coord_win, "coordination", "none or unclassified"))
MO$anchor_form_source <- ifelse(
  MO$race %in% names(named), "named in chapter 6",
  ifelse(MO$race %in% coord_win, "coordination, from table 5.2",
         "left over: stated in no table and no sentence"))
MO$anchored <- MO$anchor_form != "none or unclassified"

# The four counts that have to land on the book's own printed figures. This is
# the whole warrant for the derived column, so it is asserted, not reported.
for (i in seq_len(nrow(MG))) {
  got  <- sum(MO$anchor_form == MG$anchor_form[i])
  want <- MG$first_place[i]
  if (got != want)
    stop("anchor form '", MG$anchor_form[i], "': derived ", got,
         " winners, table 6.3 prints ", want)
  add(paste0("winners anchored by ", MG$anchor_form[i],
             " (table 6.3 prints ", want, ")"), got)
}
add("anchored winners (table 6.2 prints 47)", sum(MO$anchored))
stopifnot(sum(MO$anchored) == 47)

# --- 4. the rest of the join ------------------------------------------------
MO$coordination      <- MO$race %in% CO$race
MO$coordination_form <- CO$form[match(MO$race, CO$race)]
MO$coordination_form[is.na(MO$coordination_form)] <- ""
MO$faction           <- FA$faction[match(MO$race, FA$race)]
MO$business_pac_usd  <- FA$business_pac_usd[match(MO$race, FA$race)]

CLUSTER <- c("win general election" = "voter interest",
             "group benefit" = "policy demander interest",
             "ideology or values" = "policy demander interest",
             "district interest" = "party interest",
             "control local government" = "party interest",
             "other party interest" = "party interest",
             "ambiguous party case" = "unclear",
             "motive unclear" = "unclear")
MO$motive_cluster <- CLUSTER[MO$motive]
stopifnot(!any(is.na(MO$motive_cluster)))

MO$errata <- ""
MO$errata[MO$race == "MI-14D"] <- "table 10.1 prints MI-4D; Lawrence won MI-14D"
MO$errata[MO$race == "FL-13R"] <- "table 9.1c prints FL-19 for Jolly; Jolly won FL-13"
MO$tables_disagree <- MO$race %in% c("CA-45R", "TX-36R", "WI-6R")
add("rows correcting a district code printed in the book", sum(nzchar(MO$errata)))
add("rows where table 10.1 and tables 6.2/6.3 disagree", sum(MO$tables_disagree))

# --- 5. the summaries the chapter prints ------------------------------------
FORMS <- MG$anchor_form
MOTIVES <- c("win general election", "group benefit", "ideology or values",
             "district interest", "control local government",
             "other party interest", "ambiguous party case", "motive unclear")
by_anchor <- as.data.frame.matrix(
  table(factor(MO$motive, MOTIVES), factor(MO$anchor_form, FORMS)))
by_anchor <- cbind(motive = rownames(by_anchor), by_anchor)
rownames(by_anchor) <- NULL

G <- MO[MO$party == "R" & !is.na(MO$faction), ]
by_faction <- do.call(rbind, lapply(sort(unique(G$faction)), function(f) {
  z <- G[G$faction == f, ]
  data.frame(faction = f, nominees = nrow(z),
             coordinated = sum(z$coordination),
             pct_coordinated = round(100 * mean(z$coordination), 1))
}))

pac <- do.call(rbind, lapply(FORMS, function(f) {
  v <- G$business_pac_usd[G$anchor_form == f]
  if (!length(v)) return(NULL)
  data.frame(anchor_form = f, nominees = length(v),
             median_pac = round(median(v)), mean_pac = round(mean(v)))
}))

add("insurgents in a coordinated race",
    sprintf("%d of %d", by_faction$coordinated[by_faction$faction == "insurgent"],
            by_faction$nominees[by_faction$faction == "insurgent"]))
add("party-backed establishment nominees in a coordinated race",
    sprintf("%d of %d",
            by_faction$coordinated[by_faction$faction == "establishment (party-backed)"],
            by_faction$nominees[by_faction$faction == "establishment (party-backed)"]))
add("median business PAC money, grassroots-anchored Republicans",
    sprintf("$%s", format(pac$median_pac[pac$anchor_form == "grassroots"], big.mark = ",")))
add("median business PAC money, coordination-anchored Republicans",
    sprintf("$%s", format(pac$median_pac[pac$anchor_form == "coordination"], big.mark = ",")))

# The book's own published medians, recomputed from the transcription. If the
# transcription had a digit wrong, these would not come back.
for (f in sort(unique(FA$faction))) {
  v <- FA$business_pac_usd[FA$faction == f]
  add(paste0("median business PAC money, ", f),
      sprintf("$%s (n = %d)", format(median(v), big.mark = ","), length(v)))
}
stopifnot(median(FA$business_pac_usd[FA$faction == "insurgent"]) == 7250,
          median(FA$business_pac_usd[FA$faction == "establishment (business-backed)"]) == 130800,
          median(FA$business_pac_usd[FA$faction == "establishment (party-backed)"]) == 15500)

KEEP <- c("race", "state", "district", "party", "candidate", "anchor",
          "anchor_form", "anchor_form_source", "anchored", "motive",
          "motive_cluster", "coordination", "coordination_form", "faction",
          "business_pac_usd", "fits_framework", "bolded", "errata",
          "tables_disagree")
w <- function(d, f) write.csv(d, file.path("derived", f), row.names = FALSE)
w(MO[, KEEP],   "nominations.csv")
w(by_anchor,    "by_anchor.csv")
w(by_faction,   "by_faction.csv")
w(pac,          "pac_by_anchor.csv")
w(CK,           "checks.csv")

cat(sprintf(paste0(
  "\n  nominations            %d  (%d Republican, %d Democratic)\n",
  "  anchor form derived    %d races, of which %d left over\n",
  "  coordination           %d races, %d candidates, %d of them winners\n",
  "  errata corrected       %d\n",
  "  tables disagreeing     %d\n"),
  nrow(MO), sum(MO$party == "R"), sum(MO$party == "D"),
  nrow(MO), sum(MO$anchor_form_source == "left over: stated in no table and no sentence"),
  nrow(CO), NCAND, NWIN, sum(nzchar(MO$errata)), sum(MO$tables_disagree)))

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
