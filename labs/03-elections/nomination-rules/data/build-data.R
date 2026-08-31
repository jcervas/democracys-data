# ---------------------------------------------------------------------------
# Build the data for the `nomination-rules` chapter.
#
#   derived/contests.csv    one row per contested House primary, 2004-2022:
#                           candidates, total vote, the leader's share, and
#                           whether a runoff followed
#   derived/runoffs.csv     every runoff: who led the primary, who won the
#                           runoff, and whether those are the same person
#   derived/by_year.csv     one row per cycle
#   derived/brackets.csv    per state, the highest leader share that still went
#                           to a runoff and the lowest that did not -- the
#                           threshold, as far as the outcomes pin it down
#   derived/checks.csv      the validation results
#
# SOURCE. Federal Election Commission, *Federal Elections*, the biennial
# compilations for 2004 through 2022.
# https://www.fec.gov/introduction-campaign-finance/election-results-and-voting-information/
#
# NOTHING IS DOWNLOADED HERE. The ten workbooks were fetched, pinned by their
# numeric document identifier and byte count, and committed by the `retirements`
# chapter, which is the only place in this book that fetches them. This chapter
# reads that copy:
#
#     ../../retirements/data/raw/federalelections<year>.xls[x]
#
# Sixty megabytes of workbook has no business existing twice in one repository,
# and a second copy is a second thing to drift. The provenance record for these
# files -- the URLs, the fetch date, the byte counts -- lives with the chapter
# that fetched them.
#
# WHAT THIS CHAPTER NEEDS THAT ITS SIBLINGS DO NOT. `retirements` and
# `primary-defeats` read these workbooks for incumbents. This chapter ignores
# incumbency entirely and reads every candidate, because the question is about
# the counting rule rather than about who was already in office.
#
# THE FEC RECORDS THE RUNOFF, WHICH IS THE WHOLE OPPORTUNITY. Every state's
# nominating rule is written in its own election code, and there is no federal
# table of those rules. But the Commission reports a runoff vote when one
# happened. So where a runoff was held is a fact in this file, and the
# THRESHOLD that triggered it can be bracketed from the outcomes: the highest
# leader share that still went to a runoff sits below the rule, and the lowest
# that did not sits above it. How tight that bracket is depends entirely on
# which contests happened to occur, which is why brackets.csv reports the
# bracket and not a number.
#
# SIX PROPERTIES OF THESE WORKBOOKS DECIDE HOW THEY HAVE TO BE READ. All six
# are documented at greater length in `primary-defeats`, which found them:
#
#   * The sheet holding the House is called something different in nearly every
#     edition, including the Commission's own typo, `2012 US House & Senate
#     Resuts`. Sheets are found by pattern, never by position.
#   * Column counts run from 20 to 30 and the headers are renamed between
#     editions. Columns are found by header text, never by position. A
#     positional read runs clean and silently returns the wrong column.
#   * `PRIMARY VOTES` is not numeric. It holds `Unopposed`, `*`, `#`, `**` and
#     at least one value of `115**`. Coercing the column blindly turns an
#     unopposed candidacy into a missing value, and there are hundreds of them.
#   * The district field is not a district number. It reads `07 - FULL TERM`,
#     and a special election to finish somebody's unexpired term appears in the
#     same sheet as `07 - UNEXPIRED TERM`. Those are different elections and the
#     unexpired ones are dropped.
#   * Party labels contain characters that are not spaces, including U+00A0.
#     Trimming ordinary whitespace leaves two Republican parties in one district.
#   * House and Senate share a sheet in the earlier editions. They are separated
#     on the Commission's own candidate identifier, which begins `H` for House.
#
# WHAT A CONTEST IS HERE. One row per state, district, party and cycle: the
# nomination that was actually at stake. A candidate on two party lines is two
# contests, which is correct -- they were two nominations.
#
# WHAT IS DELIBERATELY NOT COUNTED. Louisiana, which runs a nonpartisan November
# ballot with a December runoff in most of these years, has no party primary to
# read and is excluded. California and Washington's top-two ballots pool the
# parties, so a party's "leader share" is not comparable, and they are excluded
# from the share statistics and marked in contests.csv rather than dropped.
#
# Run from inside this data/ folder:  Rscript build-data.R
# ---------------------------------------------------------------------------

options(scipen = 999, stringsAsFactors = FALSE)
dir.create("derived", showWarnings = FALSE)
stopifnot(requireNamespace("readxl", quietly = TRUE))

RAW <- normalizePath(file.path("..", "..", "retirements", "data", "raw"),
                     mustWork = FALSE)
if (!dir.exists(RAW))
  stop("the FEC workbooks live with the retirements chapter and were not found at:\n  ", RAW)

CK <- data.frame(check = character(), value = character())
add <- function(k, v) CK[nrow(CK) + 1L, ] <<- c(k, as.character(v))
n <- function(x) format(x, big.mark = ",", trim = TRUE)

YEARS <- seq(2004, 2022, by = 2)
TOPTWO <- c("CA", "WA")     # all-party ballots; shares not comparable
SKIP   <- "LA"              # no party primary to read in most of these years

# Find a column by what its header says, tolerating the renaming between
# editions. Returns the first match or NA.
col_of <- function(d, pattern) {
  i <- grep(pattern, toupper(names(d)))
  if (!length(i)) NA_integer_ else i[1]
}

# Primary vote cells that are not numbers. "Unopposed" means the candidate was
# the only one filed, which is a real outcome and not a missing value.
UNOPP <- "^\\s*UNOPPOSED"
as_votes <- function(x) {
  s <- trimws(as.character(x))
  v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", s)))
  v[grepl(UNOPP, toupper(s))] <- NA_real_          # handled separately
  v[!grepl("[0-9]", s)] <- NA_real_                # "*", "#", "**"
  v
}

read_cycle <- function(yr) {
  f <- Sys.glob(file.path(RAW, sprintf("federalelections%d.xls*", yr)))
  if (!length(f)) stop("no workbook for ", yr)
  sh <- readxl::excel_sheets(f[1])
  hit <- grep("house.*(result|resut)|result.*house", sh, ignore.case = TRUE)
  if (!length(hit)) stop("no House results sheet in ", basename(f[1]))
  d <- suppressWarnings(suppressMessages(
    readxl::read_excel(f[1], sheet = sh[hit[1]], .name_repair = "unique")))
  d <- as.data.frame(d)

  # The full-name column is spelled four different ways across the ten
  # editions -- `CANDIDATE NAME`, `CANDIDATE NAME (LAST, FIRST)` and
  # `LAST NAME, FIRST` among them -- so the name is assembled from the last and
  # first columns instead. Those two are unambiguous in every edition.
  ix <- c(st = col_of(d, "^STATE ABBREVIATION"), dist = col_of(d, "^D$|^DISTRICT"),
          id = col_of(d, "^FEC ID"),
          last = col_of(d, "^LAST NAME$|^CANDIDATE NAME \\(LAST\\)$"),
          first = col_of(d, "^FIRST NAME$|^CANDIDATE NAME \\(FIRST\\)$"),
          party = col_of(d, "^PARTY"), pv = col_of(d, "^PRIMARY VOTES$|^PRIMARY$"),
          rv = col_of(d, "^RUNOFF VOTES$|^RUNOFF$"))
  if (any(is.na(ix)))
    stop(yr, ": could not find column(s) ", paste(names(ix)[is.na(ix)], collapse = ", "))

  nm <- paste0(trimws(as.character(d[[ix["last"]]])), ", ",
               trimws(as.character(d[[ix["first"]]])))
  nm[is.na(d[[ix["last"]]])] <- NA_character_
  z <- data.frame(year = yr, st = d[[ix["st"]]], dist = d[[ix["dist"]]],
                  id = d[[ix["id"]]], name = nm,
                  party_raw = d[[ix["party"]]],
                  pv_raw = d[[ix["pv"]]], rv_raw = d[[ix["rv"]]])
  z <- z[!is.na(z$st) & !is.na(z$id), ]
  z$id <- toupper(trimws(z$id))
  z <- z[grepl("^H", z$id), ]                       # House only
  z$dist <- as.character(z$dist)
  z <- z[!grepl("UNEXPIRED", toupper(z$dist)), ]    # a different election
  z$dnum <- sub("^0*", "", sub("[^0-9].*$", "", trimws(z$dist)))
  z$dnum[z$dnum == ""] <- "AL"
  # THE PARTY COLUMN IS SPELLED TWO WAYS AND CARRIES WRITE-INS.
  # 2006 and 2010 label the major parties `DEM` and `REP`; every other edition
  # uses `D` and `R`. Reading only the one-letter form silently drops two whole
  # cycles -- 3,700 candidate rows -- and nothing warns, because the cycles just
  # contribute no contests.
  #
  # The column also holds write-in lines: `W`, and `WD` and `WR` for a write-in
  # campaign on a party's behalf. Those are not candidacies for the nomination
  # and are excluded. A cross-filed line like `R/W` is a Republican who also
  # holds a write-in line, so the base party is kept.
  #
  # U+00A0 and friends are not spaces to trimws(); strip anything non-printing.
  pr <- toupper(gsub("[^A-Z/]", "", toupper(as.character(z$party_raw))))
  z$party <- ifelse(grepl("^(R|REP)(/|$)", pr), "R",
             ifelse(grepl("^(D|DEM)(/|$)", pr), "D", pr))
  z$unopposed <- grepl(UNOPP, toupper(trimws(as.character(z$pv_raw))))
  z$pv <- as_votes(z$pv_raw)
  z$rv <- as_votes(z$rv_raw)
  z
}

ALL <- do.call(rbind, lapply(YEARS, read_cycle))
add("cycles read", length(YEARS))
add("House candidate rows, 2004-2022", n(nrow(ALL)))
add("of those, marked Unopposed rather than given a vote total",
    n(sum(ALL$unopposed)))
stopifnot(nrow(ALL) > 15000, sum(ALL$unopposed) > 500)

# The two major parties only. A minor-party line rarely holds a contest and its
# "primary" is often a convention reported in the same column.
NPRE <- nrow(ALL)
ALL <- ALL[ALL$party %in% c("D", "R"), ]
add("of those, on a Democratic or Republican line",
    sprintf("%s of %s", n(nrow(ALL)), n(NPRE)))
ALL <- ALL[ALL$st != SKIP, ]
add("Democratic and Republican House candidacies", n(nrow(ALL)))

# --- contests ---------------------------------------------------------------
key <- with(ALL, paste(year, st, dnum, party, sep = "|"))
sp  <- split(seq_len(nrow(ALL)), key)

mk <- function(i) {
  z <- ALL[i, ]
  z <- z[order(-z$pv), ]
  votes <- z$pv[!is.na(z$pv)]
  if (length(votes) < 2) return(NULL)              # not a contest
  tot <- sum(votes)
  if (tot <= 0) return(NULL)
  ro <- sum(!is.na(z$rv)) >= 2
  # Compare the runoff winner to the primary leader by row rather than by name.
  # A name is a string that can differ by a nickname or a suffix; a row is the
  # candidate. `z` is already sorted by primary vote, so row 1 is the leader.
  z$.row <- seq_len(nrow(z))
  rz <- z[!is.na(z$rv), ]
  rz <- rz[order(-rz$rv), ]
  data.frame(
    year = z$year[1], st = z$st[1], district = z$dnum[1], party = z$party[1],
    candidates = length(votes), total_votes = tot,
    leader = z$name[1], leader_votes = z$pv[1],
    leader_share = 100 * z$pv[1] / tot,
    second = z$name[2], second_share = 100 * z$pv[2] / tot,
    runoff = ro,
    runoff_winner = if (ro) rz$name[1] else NA_character_,
    leader_won_runoff = if (ro) rz$.row[1] == 1L else NA,
    top_two_state = z$st[1] %in% TOPTWO)
}
CT <- do.call(rbind, lapply(sp, mk))
CT <- CT[order(CT$year, CT$st, CT$district, CT$party), ]
rownames(CT) <- NULL

add("contested House primaries with usable vote totals", n(nrow(CT)))
add("of those, in all-party (top-two) states, shares not comparable",
    n(sum(CT$top_two_state)))
stopifnot(nrow(CT) > 2000)

# Party primaries in ordinary states: everything the share statistics use.
P <- CT[!CT$top_two_state, ]
add("party primaries used for share statistics", n(nrow(P)))

# --- runoffs ----------------------------------------------------------------
RO <- P[P$runoff, ]
NRO <- nrow(RO)
NOV <- sum(!RO$leader_won_runoff)
add("runoffs held", NRO)
add("of those, won by the primary leader", NRO - NOV)
add("of those, won by somebody who did not lead the primary", NOV)
add("share of runoffs that overturned the primary leader",
    sprintf("%d of %d", NOV, NRO))
stopifnot(NRO > 40, NOV > 5)

# --- what the rule would have caught elsewhere ------------------------------
NOR <- P[!P$runoff, ]
for (thr in c(50, 40, 35)) {
  k <- sum(NOR$leader_share < thr)
  add(sprintf("primaries settled outright whose winner took under %d%%", thr),
      sprintf("%s of %s (%.1f%%)", n(k), n(nrow(NOR)), 100 * k / nrow(NOR)))
}
add("primaries settled outright whose winner took under half the vote",
    n(sum(NOR$leader_share < 50)))

# --- the threshold, as far as the outcomes pin it down ----------------------
# The rule is in each state's election code, which this file does not contain.
# What it contains is outcomes, and outcomes bracket the rule.
BR <- do.call(rbind, lapply(sort(unique(P$st[P$runoff])), function(s) {
  z <- P[P$st == s, ]
  hi <- max(z$leader_share[z$runoff])
  lo <- if (any(!z$runoff)) min(z$leader_share[!z$runoff]) else NA_real_
  data.frame(state = s, runoffs = sum(z$runoff), contests = nrow(z),
             highest_share_still_runoff = round(hi, 1),
             lowest_share_no_runoff = round(lo, 1),
             bracket_width = round(lo - hi, 1))
}))
BR <- BR[order(-BR$bracket_width), ]   # widest first; non-closing last
add("states that held at least one House runoff, 2004-2022", nrow(BR))
ok <- BR[!is.na(BR$bracket_width) & BR$bracket_width > 0, ]
ok <- ok[order(ok$bracket_width), ]
add("tightest bracket around a state's threshold",
    sprintf("%s, %.1f to %.1f percent", ok$state[1],
            ok$highest_share_still_runoff[1], ok$lowest_share_no_runoff[1]))

# A bracket must not be impossible. If the highest share that went to a runoff
# ever exceeds the lowest that did not, either the rule changed inside the
# window or a row has been read wrongly, and either way it must be looked at.
BR$closes <- !is.na(BR$bracket_width) & BR$bracket_width > 0
bad <- BR$state[!BR$closes]
add("states whose runoff bracket closes", sum(BR$closes))
add("states whose runoff bracket does not close", length(bad))
if (length(bad)) add("of those, states named", paste(bad, collapse = ", "))

# For those states, show the rule moving. Nineteen years is long enough for a
# legislature to change its own threshold, and a bracket computed across the
# whole window then averages two different rules into one impossible range.
# Splitting by cycle is what makes the change visible.
# Per cycle: the same bracket, computed inside one election rather than across
# nineteen years. Where the bracket closes in the early cycles at one value and
# in the later cycles at another, the threshold moved and the two values say
# roughly where it moved from and to.
ERA <- do.call(rbind, lapply(bad, function(s) {
  do.call(rbind, lapply(sort(unique(P$year)), function(y) {
    z <- P[P$st == s & P$year == y, ]
    if (!nrow(z)) return(NULL)
    lo40 <- z[z$leader_share < 40, ]
    if (!nrow(lo40)) return(NULL)
    hi <- if (any(z$runoff)) max(z$leader_share[z$runoff]) else NA_real_
    lo <- if (any(!z$runoff)) min(z$leader_share[!z$runoff]) else NA_real_
    data.frame(state = s, year = y,
               contests_under_40 = nrow(lo40),
               went_to_runoff = sum(lo40$runoff),
               highest_share_still_runoff = round(hi, 1),
               lowest_share_no_runoff = round(lo, 1))
  }))
}))
if (!is.null(ERA)) {
  w0 <- function(d, f) write.csv(d, file.path("derived", f), row.names = FALSE)
  w0(ERA, "nonclosing_by_year.csv")
  add("cycles shown for states whose bracket does not close", nrow(ERA))
}

BY <- do.call(rbind, lapply(sort(unique(P$year)), function(y) {
  z <- P[P$year == y, ]
  data.frame(year = y, contests = nrow(z), runoffs = sum(z$runoff),
             overturned = sum(!z$leader_won_runoff[z$runoff]),
             under_half = sum(!z$runoff & z$leader_share < 50),
             median_leader_share = round(median(z$leader_share), 1))
}))

w <- function(d, f) write.csv(d, file.path("derived", f), row.names = FALSE)
CT$leader_share <- round(CT$leader_share, 2)
CT$second_share <- round(CT$second_share, 2)
RO$leader_share <- round(RO$leader_share, 2)
RO$second_share <- round(RO$second_share, 2)
w(CT, "contests.csv"); w(RO[, c("year","st","district","party","candidates",
  "leader","leader_share","second","second_share","runoff_winner",
  "leader_won_runoff")], "runoffs.csv")
w(BY, "by_year.csv"); w(BR, "brackets.csv"); w(CK, "checks.csv")

cat(sprintf(paste0(
  "\n  contested primaries    %s across %d cycles\n",
  "  runoffs                %d, of which %d overturned the primary leader\n",
  "  settled under half     %s of %s primaries decided outright\n",
  "  states with a runoff   %d, of which %d bracket cleanly\n"),
  n(nrow(CT)), length(YEARS), NRO, NOV,
  n(sum(NOR$leader_share < 50)), n(nrow(NOR)),
  nrow(BR), sum(BR$closes)))

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
