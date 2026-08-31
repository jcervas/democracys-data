# ---------------------------------------------------------------------------
# Build the roll-call-instrument dataset: a record the recorded chose to make.
#
# The fifth SOURCE chapter, after surveys, the census, voter files and election
# returns. Its subject is the instrument: what a recorded legislative vote
# establishes that nothing else can, and the two ways it is a SELECTED record
# rather than a complete one.
#
# Four files end up in this folder:
#
#   derived/volume.csv    Roll calls per Congress, both chambers, 1st to the present.
#   derived/margins.csv   How lopsided recorded votes are. A vote nobody contests
#                 carries no information about who disagrees with whom.
#   derived/content.csv   How often the file records WHAT was being voted on, by era.
#                 This is the finding.
#   derived/questions.csv What the recorded votes are about, where that is recorded.
#
# THE ARGUMENT. Election returns record an outcome and are complete: every
# ballot counted appears in some total. A roll call is different in a way that
# is easy to miss, because it looks just as official.
#
# THE CONSTITUTION DOES NOT REQUIRE RECORDED VOTES. Article I, Section 5 says
# the yeas and nays shall be entered on the Journal "at the Desire of one fifth
# of those Present". A recorded vote therefore happens when somebody WANTS one.
# Voice votes and unanimous consent leave no record of who was on which side,
# and most legislative business is conducted that way. **The roll call record
# is a sample of decisions, selected by the people being recorded**, and
# nothing in the file marks what is missing.
#
# THE SECOND SELECTION IS THE AGENDA. A bill that dies in committee is never
# voted on at all. A member's voting record is a record of the questions
# leadership chose to put, and the questions never put are invisible in exactly
# the way a survey's unasked questions are invisible.
#
# WHAT THE FILE IS SILENT ABOUT, and this is the part that surprised the person
# who built this chapter. Voteview's roll call file has a field for the
# question being voted on. It is EMPTY for 69% of all roll calls, and the
# emptiness is not scattered:
#
#   Congresses   1-100 : question recorded for   0.0% of roll calls
#   Congresses 101-119 : question recorded for  98-100%
#
# For the first two centuries the record is complete about BEHAVIOUR and empty
# about CONTENT. You can see exactly how every member voted on 78,314
# occasions where nobody wrote down what the occasion was. Any scaling of those
# votes -- DW-NOMINATE included -- is built on the pattern of agreement alone,
# because for most of the series there is nothing else there.
#
# AND NOT ALL ROLL CALLS ARE THE SAME KIND OF THING. Only about 2% are final
# passage. The rest are amendments, procedural motions, cloture, tabling and
# nominations, and a member may vote against a bill's final passage having
# voted for every amendment that made it, or the reverse. A scaling that treats
# them alike is making an assumption, not reading a fact.
#
# SOURCE. Voteview, HSall_rollcalls.csv -- every recorded vote in the House and
# Senate, 1st Congress to the present, with margins and NOMINATE midpoints.
#   https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.csv
# Researcher-assembled, not a government file: Poole, Rosenthal, Lewis and
# colleagues built it from the Journals and later from Clerk and Senate feeds.
# It is quasi-handed-to-you -- someone else did the compiling, and the
# compilation's gaps are inherited along with its convenience.
#
# Run from this directory:  Rscript build-data.R   (downloads ~30 MB)
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

URL <- "https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.csv"
LOCAL <- "raw/HSall_rollcalls.csv"
if (!file.exists(LOCAL)) {
  dir.create("raw", showWarnings = FALSE)
  prov_fetch(URL, LOCAL, mode = "wb", quiet = TRUE)
}
d <- read.csv(LOCAL, colClasses = "character")
stopifnot(nrow(d) > 100000)

d$congress <- as.integer(d$congress)
d$yea <- as.integer(d$yea_count)
d$nay <- as.integer(d$nay_count)
d$question <- trimws(d$vote_question)
d$has_q <- nzchar(d$question)
cat(sprintf("%s roll calls, Congress %d to %d\n",
            format(nrow(d), big.mark = ","), min(d$congress), max(d$congress)))

# --- 1. how many recorded votes, and when -----------------------------------

v <- do.call(rbind, lapply(sort(unique(d$congress)), function(c) {
  k <- d$congress == c
  data.frame(congress = c,
             house  = sum(k & d$chamber == "House"),
             senate = sum(k & d$chamber == "Senate"))
}))
v$total <- v$house + v$senate
write.csv(v, "derived/volume.csv", row.names = FALSE)

# --- 2. how lopsided ---------------------------------------------------------
#
# A vote of 430-0 tells you nothing about who disagrees with whom. Scaling
# methods need disagreement, and a large share of the record has almost none.

k <- !is.na(d$yea) & !is.na(d$nay) & (d$yea + d$nay) > 0
m <- d[k, ]
m$tot <- m$yea + m$nay
m$minority <- pmin(m$yea, m$nay) / m$tot

mar <- data.frame(
  band = c("Unanimous -- nobody on the losing side",
           "Losing side under 10%",
           "Losing side 10-20%",
           "Losing side 20-40%",
           "Close -- losing side over 40%"),
  roll_calls = c(sum(m$minority == 0),
                 sum(m$minority > 0 & m$minority < 0.10),
                 sum(m$minority >= 0.10 & m$minority < 0.20),
                 sum(m$minority >= 0.20 & m$minority <= 0.40),
                 sum(m$minority > 0.40)))
mar$pct <- round(100 * mar$roll_calls / sum(mar$roll_calls), 1)
write.csv(mar, "derived/margins.csv", row.names = FALSE)

# --- 3. what the file says the vote was about -------------------------------

eras <- list(c(1, 50), c(51, 80), c(81, 100), c(101, 110), c(111, 119))
con <- do.call(rbind, lapply(eras, function(r) {
  k <- d$congress >= r[1] & d$congress <= r[2]
  data.frame(era = sprintf("%d-%d", r[1], r[2]),
             years = sprintf("%d-%d", 1789 + 2 * (r[1] - 1), 1789 + 2 * r[2] - 1),
             roll_calls = sum(k),
             question_recorded_pct = round(100 * mean(d$has_q[k]), 1))
}))
con <- rbind(con, data.frame(era = "all", years = "1789-present",
                             roll_calls = nrow(d),
                             question_recorded_pct = round(100 * mean(d$has_q), 1)))
write.csv(con, "derived/content.csv", row.names = FALSE)

# --- 4. what they are about, where it is recorded ---------------------------

q <- as.data.frame(sort(table(d$question[d$has_q]), decreasing = TRUE),
                   stringsAsFactors = FALSE)
names(q) <- c("question", "roll_calls")
q <- head(q, 10)
q$pct_of_recorded <- round(100 * q$roll_calls / sum(d$has_q), 1)
q$pct_of_all      <- round(100 * q$roll_calls / nrow(d), 1)
write.csv(q, "derived/questions.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

cat("\nvolume.csv   : recorded votes per Congress\n")
print(v[v$congress %in% c(1, 20, 50, 80, 90, 100, 110, max(v$congress)), ],
      row.names = FALSE)
cat("\nmargins.csv  : how lopsided\n")
print(mar, row.names = FALSE)
cat("\ncontent.csv  : is the question recorded?\n")
print(con, row.names = FALSE)
cat(sprintf("\n  %s of %s roll calls have no recorded question (%.1f%%).\n",
            format(sum(!d$has_q), big.mark = ","),
            format(nrow(d), big.mark = ","), 100 * mean(!d$has_q)))
cat("\nquestions.csv: what the recent ones are about\n")
print(q[, c("question", "roll_calls", "pct_of_all")], row.names = FALSE)

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
