# ---------------------------------------------------------------------------
# Build the voter-files dataset: what is in a voter registration file.
#
# Eight files end up in this folder. Seven are summaries of the whole file;
# derived/peek.csv is five real registrants, unredacted:
#
#   derived/schema.csv       The 53 columns of a real state voter file, grouped by what
#                    they are for. Column NAMES only -- no values.
#   derived/status.csv       Counts by registration status and the reason recorded.
#   derived/race_party.csv   Race by "last party voted" -- the crosstab that shows
#                    Georgia has no party registration.
#   derived/turnout.csv      By race, for three general elections: how many are in the
#                    current file, how many are recorded as having voted, and
#                    HOW MANY VOTERS THE CURRENT FILE HAS LOST.
#   derived/lost.csv         The same loss, per election, on its own: voters recorded as
#                    voting who are no longer in the current file.
#   derived/headscan.csv     What the first few rows would have told you, against what
#                    the whole file says -- the lab's argument for never
#                    trusting a head(). The registration-number rows carry the
#                    file's true minimum and maximum, which is the point: the
#                    smallest is 00002779 and the top of the file never hints
#                    at it.
#   derived/columns.csv      What each of eight discussed columns IS -- how R stores it
#                    if you do not intervene, what the variable actually is,
#                    how many distinct values it takes and how often it is
#                    blank, over all 127,560 rows.
#   derived/peek.csv         FIVE REAL REGISTRANTS, drawn at random with
#                    set.seed(84355) and printed as the state published them:
#                    names, registration numbers and street addresses
#                    included. Nothing is withheld. Georgia's list is a public
#                    record and this book does not redact its sources.
#
# WHY BOTH A SAMPLE AND SUMMARIES. The five rows show what a record IS: the
# fields, the padding, the empty party column, the 00:00:00.000 stamped onto
# every date. The summaries show what the FILE does, which five rows cannot --
# `Congressional District` reads like a quantity across five rows and is a
# two-valued category across all 127,560. Carrying only the sample once let the
# brief claim a district 4 that does not exist here. Carrying only the
# summaries loses the thing a reader most needs to see once: an actual row.
#
# WHY GEORGIA. It records race on the registration form -- collected for
# Voting Rights Act compliance -- which makes turnout-by-race computable from
# administrative records rather than from a survey. `validated-turnout` shows what
# happens when you ask people instead. `bisg-check` and `rpv` both build on this
# file, which is why it is introduced here first.
#
# THE FINDING THIS FILE EXISTS TO SUPPORT. A voter file is a SNAPSHOT of a
# live administrative system, not a historical record. Match the 2026 file
# against vote history and the match rate decays the further back you go:
# 2024 loses 3,798 voters, 2022 loses 5,677, 2020 loses 13,803. Those people
# moved, died, or were removed. Any retrospective turnout rate computed from a
# current file silently deletes them -- and they are not a random sample.
#
# SOURCES, AND THEY ARE TWO. Houston County, GA registration extract 71754,
# from the working data for the 2026 Houston County redistricting matter. The
# state's voter-history files did NOT come with it: they are a separate public
# record, requested separately, and the match between them is made here rather
# than by the state. That is why nid() below is not a formatting nicety --
# nothing upstream ever guaranteed the two files write a registration number
# the same way, because nothing upstream ever put them side by side.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

D <- file.path("/Users/cervas/Library/CloudStorage",
               "GoogleDrive-jcervas@andrew.cmu.edu/My Drive/Redistricting",
               "2026/Houston County/Superseding Report/data")

nid <- function(x) as.character(as.integer(trimws(as.character(x))))

r <- read.csv(file.path(D, "voter-reg", "71754 - Houston County.csv"),
              check.names = FALSE, colClasses = "character")
r$id <- nid(r$"Voter Registration Number")
cn <- names(r)[1:53]

# --- 1. the schema, grouped by purpose ------------------------------------
grp <- rep(NA_character_, 53)
grp[1]            <- "County"
grp[c(2,5,6,7,8)] <- "Who you are"
grp[c(9,40,41)]   <- "Demographics"
grp[10:17]        <- "Where you live"
grp[18:38]        <- "Which ballot you get"
grp[c(3,4)]       <- "Whether you count"
grp[c(39,42,43,44,45,46)] <- "When things happened"
grp[47:53]        <- "Where to mail you things"
stopifnot(!any(is.na(grp)))

schema <- data.frame(n = 1:53, column = cn, purpose = grp, stringsAsFactors = FALSE)
write.csv(schema, "derived/schema.csv", row.names = FALSE)

# --- 2. status -------------------------------------------------------------
st <- as.data.frame(table(status = r$Status, reason = r$"Status Reason"),
                    stringsAsFactors = FALSE)
st <- st[st$Freq > 0, ]
st <- st[order(-st$Freq), ]
names(st)[3] <- "voters"
write.csv(st, "derived/status.csv", row.names = FALSE)

# --- 3. race by "last party voted" ----------------------------------------
# Georgia has NO party registration. This column records the last party BALLOT
# a voter requested in a primary -- which is why most of the file is blank.
lp <- r$"Last Party Voted"
lp[trimws(lp) == ""] <- "no primary ballot on record"
rp <- as.data.frame.matrix(table(race = r$Race, last_party = lp))
rp <- data.frame(race = rownames(rp), rp, check.names = FALSE)
write.csv(rp, "derived/race_party.csv", row.names = FALSE)

# --- 4. turnout by race, and what the snapshot lost -----------------------
races <- c("WHITE", "BLACK", "HISPANIC/LATINO", "ASIAN/PACIFIC ISLANDER",
           "AMERICAN INDIAN", "ALASKAN NATIVE", "OTHER", "UNKNOWN")
els <- list(c("2020 general", "2020.csv", "11/03/2020"),
            c("2022 general", "2022.csv", "11/08/2022"),
            c("2024 general", "2024.csv", "11/05/2024"))

to <- data.frame(race = races,
                 in_file_2026 = as.vector(table(factor(r$Race, levels = races))),
                 stringsAsFactors = FALSE)
lost <- setNames(numeric(length(els)), sapply(els, `[`, 1))

for (e in els) {
  h <- read.csv(file.path(D, "vote-history", e[2]),
                check.names = FALSE, colClasses = "character")
  h <- h[h$"Election Date" == e[3], ]
  h$id <- nid(h$"Voter Registration Number")
  h <- h[!duplicated(h$id), ]
  v <- r$Race[match(h$id, r$id)]
  to[[e[1]]] <- as.vector(table(factor(v, levels = races)))
  lost[e[1]] <- sum(is.na(v))          # voted then, absent from the file now
}

write.csv(to, "derived/turnout.csv", row.names = FALSE)
write.csv(data.frame(election = names(lost), voters_no_longer_in_file = lost),
          "derived/lost.csv", row.names = FALSE)

# --- 5. what each column IS, described over the whole file -----------------
#
# TWO THINGS ARE WRITTEN HERE, and they do different jobs. Keep both.
#
# peek.csv is FIVE REAL REGISTRANTS, exactly as Georgia published them --
# names, registration numbers, street addresses, all of it. Nothing is
# withheld: the list is a public record, this book does not redact, and a
# reader who has never seen a row of one of these files does not really know
# what the chapter is talking about. They are drawn at random from a fixed
# seed rather than off the top, because the top of this file is sorted
# newest-first and misrepresents it -- which is the point headscan.csv makes.
#
# columns.csv is the same eight columns described over all 127,560 rows. It is
# NOT a redacted substitute for the five rows; it answers a question the five
# cannot. On five rows `Congressional District` shows five values and reads
# like a quantity. Over the whole file it takes exactly two, 002 and 008,
# which is what makes it a category -- and getting that wrong is not
# hypothetical: the brief's prose had drifted into naming a district 4 that
# does not exist in this county, and only the whole-file count caught it.
#
# So: the sample shows you what a record IS, the summary shows you what the
# file DOES. Neither substitutes for the other.

DESC <- c("Voter Registration Number", "Status", "County Precinct",
          "Congressional District", "Birth Year", "Registration Date",
          "Race", "Last Party Voted")
LEVEL <- c("Voter Registration Number" = "identifier, not a quantity",
           "Status"                    = "category",
           "County Precinct"           = "category, written as a code",
           "Congressional District"     = "category, written as a number",
           "Birth Year"                = "interval: may be differenced, never summed",
           "Registration Date"         = "date, stored as text",
           "Race"                      = "category",
           "Last Party Voted"          = "category, and often blank")
set.seed(84355)
pk <- r[sort(sample.int(nrow(r), 5)), cn]
write.csv(pk, "derived/peek.csv", row.names = FALSE)

cols <- do.call(rbind, lapply(DESC, function(k) {
  v <- r[[k]]
  data.frame(column   = k,
             stored   = class(utils::type.convert(as.character(v), as.is = TRUE))[1],
             level    = unname(LEVEL[k]),
             distinct = length(unique(trimws(as.character(v)))),
             blank    = sum(trimws(as.character(v)) == ""),
             stringsAsFactors = FALSE)
}))
write.csv(cols, "derived/columns.csv", row.names = FALSE)

# The contrast that makes the random draw worth the trouble. `Last Party Voted`
# is the column, because the extract is sorted newest-registrant-first and that
# column fills in only once somebody has voted in a primary. head() therefore
# reports it as empty; over the whole county it is populated for more than
# two-thirds. Every figure quoted in the brief is computed here.
blank <- function(x) 100 * mean(is.na(x) | !nzchar(trimws(x)))
lpv   <- r$"Last Party Voted"
ry    <- suppressWarnings(as.integer(substr(trimws(r$"Registration Date"), 1, 4)))
ry    <- ry[!is.na(ry) & ry > 1800]
hs    <- function(k) blank(head(lpv, k))
hd    <- data.frame(
  measure = c("Last Party Voted blank, first 5 rows (%)",
              "Last Party Voted blank, first 50 rows (%)",
              "Last Party Voted blank, whole file (%)",
              "Registration year, first 5 rows",
              "Registration year, whole file",
              "Registration number, first 5 rows",
              "Registration number, whole file"),
  value = c(sprintf("%.1f", hs(5)), sprintf("%.1f", hs(50)),
            sprintf("%.1f", blank(lpv)),
            paste(range(head(ry, 5)), collapse = " to "),
            paste(range(ry), collapse = " to "),
            paste(range(head(trimws(r$"Voter Registration Number"), 5)),
                  collapse = " to "),
            paste(range(trimws(r$"Voter Registration Number")), collapse = " to ")),
  stringsAsFactors = FALSE)
write.csv(hd, "derived/headscan.csv", row.names = FALSE)

# --- Which states make you register at all ----------------------------------
#
# THE POINT OF THIS TABLE. The chapter opens on it. Registering is not voting;
# it is a separate act, required almost everywhere and not quite everywhere,
# and the one state that dropped it is the proof that it is a choice rather
# than a fact of nature.
#
# KEYED IN, from raw/registration-requirement.tsv. North Dakota abolished
# voter registration in 1951 and has not reinstated it; every other state and
# the District of Columbia require it. Nothing is fetched, the book is not a
# live tracker of state law, and the chapter says so.

reg <- read.delim("raw/registration-requirement.tsv", sep = "\t", quote = "",
                  stringsAsFactors = FALSE)
stopifnot(nrow(reg) == 51L, !any(duplicated(reg$state)),
          all(reg$registration %in% c("Required", "Not required")))
# Exactly one holdout, and it is the one the chapter names.
stopifnot(sum(reg$registration == "Not required") == 1L,
          reg$state[reg$registration == "Not required"] == "North Dakota")

registration <- data.frame(
  requirement = c("Required", "Not required"),
  jurisdictions = c(sum(reg$registration == "Required"),
                    sum(reg$registration == "Not required")))
write.csv(registration, "derived/registration.csv", row.names = FALSE)
cat("registration required in", registration$jurisdictions[1],
    "of", nrow(reg), "jurisdictions\n")

# --- When registration closes, and by which door ----------------------------
#
# THE POINT OF THIS TABLE. The table above says registration is required in 50
# of 51 jurisdictions. It does not say what being required costs anybody, and
# the history this chapter opens with is entirely about that: how many days
# before the election the books close, and what counts as having registered in
# time. This is that question answered for one election, the general of
# 3 November 2026, in every state and territory that holds one.
#
# FETCHED 2026-08-29 from NPR's registration tracker,
# https://apps.npr.org/voter-registration-2026-mail/
# HTTP 200 -- an index page and 56 jurisdiction pages, 14,614 to 15,654 bytes
# each, every one carrying "Last revised July 14, 2026".
#
# WHAT IT IS. A newsroom's compilation, reported by Hansi Lo Wang and
# fact-checked by two named checkers, not a government record and not the
# statute. There is no federal register of registration deadlines: the
# Election Assistance Commission's own survey asks election offices about
# their deadlines after the fact, and state law lives in fifty separate
# codes. A hand-kept tracker is what exists, and it is dated, which is the
# thing a frozen table of state law is not.
#
# WHY THE WHOLE PAGE IS KEPT. Each jurisdiction page is saved under
# raw/npr-registration-2026/ exactly as it was served. The deadlines are
# marked up in it -- data-deadline="online|mail|in-person|same-day" -- so the
# parse below reads structure rather than prose, and everything the parse
# throws away is still on disk. Several pages carry footnotes this table does
# not model: Guam's earlier deadline for registering at the DMV, Nebraska's
# separate online and county-office dates, South Carolina's weekend opening.
#
# THREE THINGS THE PARSE KEEPS THAT A ONE-COLUMN DEADLINE TABLE LOSES.
#
#   1. Each jurisdiction has up to three deadlines, one per door: online, by
#      mail, in person. They often differ. Any table with a single
#      `registration deadline` column has silently picked one of the three.
#   2. The mail deadline is two different rules wearing one label. Most
#      jurisdictions count the postmark; the rest count the day the form
#      arrives. Two states printing the same date are not asking the same
#      thing of a voter, and subtracting dates will not show it.
#   3. Whether a door is open at all. A jurisdiction with no online
#      registration has no online deadline, and an empty cell there is not a
#      late deadline. It is an absent option.
#
# CROSS-CHECK. North Dakota's page carries no deadlines at all, and says why:
# "North Dakota is the only U.S. state where voter registration is not
# required." That is an independent, dated confirmation of the keyed-in table
# above, from a source that was fetched. It is asserted below.
#
# Puerto Rico's page carries no deadlines for the opposite reason -- it holds
# no territory-wide election in 2026 -- which is why the two blank rows in
# this table mean entirely different things.

NPR_DIR  <- "raw/npr-registration-2026"
NPR_BASE <- "https://apps.npr.org/voter-registration-2026-mail"
ELECTION_DAY <- as.Date("2026-11-03")

# Fetch. The index lists the jurisdiction pages; each is captured whole.
# Set DD_REFETCH=1 to pull them again over an existing capture -- the tracker
# is revised during a cycle, and provenance.R prints a banner naming every
# page whose bytes moved.
dir.create(NPR_DIR, showWarnings = FALSE, recursive = TRUE)
npr_get <- function(url, dest) {
  if (!file.exists(dest) || nzchar(Sys.getenv("DD_REFETCH"))) {
    if (exists("prov_fetch")) invisible(prov_fetch(url, dest, label = basename(dest)))
    else utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}
if (!exists("prov_fetch") && file.exists("../../../_lib/provenance.R"))
  source("../../../_lib/provenance.R")
npr_get(paste0(NPR_BASE, "/"), file.path(NPR_DIR, "index.html"))

idx   <- paste(readLines(file.path(NPR_DIR, "index.html"), warn = FALSE), collapse = "\n")
pages <- regmatches(idx, gregexpr("(?<=<a href= )[a-z0-9-]+\\.html", idx, perl = TRUE))[[1]]
pages <- sort(unique(pages))
stopifnot(length(pages) == 56L)          # 50 states, DC, and five territories
for (p in pages) npr_get(paste0(NPR_BASE, "/", p), file.path(NPR_DIR, p))

# Parse. Three helpers, because the markup is regular and the prose is not.
untag <- function(x) {
  x <- gsub("<[^>]*>", " ", x)
  x <- gsub("&nbsp;|&#160;", " ", x)
  x <- gsub("&amp;", "&", x)
  trimws(gsub("[[:space:]]+", " ", x))
}
first <- function(txt, pat) {
  m <- regexpr(pat, txt, perl = TRUE)
  if (m == -1L) NA_character_ else regmatches(txt, m)
}
every <- function(txt, pat) regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]

MONTH <- c(Jan = 1, Feb = 2, March = 3, April = 4, May = 5, June = 6,
           July = 7, Aug = 8, Sept = 9, Oct = 10, Nov = 11, Dec = 12)

# "Postmarked by Monday, Oct. 19" -> the rule and the date, separately. The
# rule is the whole point: it is what the date MEANS.
npr_rule <- function(t) first(t, "^(Register by|Postmarked by|Received by)")
npr_date <- function(t) {
  d <- first(t, "(Jan|Feb|March|April|May|June|July|Aug|Sept|Oct|Nov|Dec)\\.? [0-9]{1,2}")
  if (is.na(d)) return(as.Date(NA))
  mo <- sub("\\.? [0-9]+$", "", d)
  as.Date(sprintf("2026-%02d-%02d", MONTH[[mo]], as.integer(sub("^[A-Za-z.]+ ", "", d))))
}

dl <- do.call(rbind, lapply(pages, function(p) {
  h <- paste(readLines(file.path(NPR_DIR, p), warn = FALSE), collapse = "\n")
  nm <- untag(first(h, "(?s)<h1 class=\"statepage-hed\">.*?</h1>"))
  nm <- trimws(sub("^How to register to vote in", "", nm))
  sec <- first(h, "(?s)<section data-deadline-type=\"general\".*?</section>")

  row <- data.frame(jurisdiction = nm,
                    online_date = as.Date(NA), online_days = NA_integer_,
                    mail_date = as.Date(NA), mail_days = NA_integer_,
                    mail_rule = NA_character_,
                    in_person_date = as.Date(NA), in_person_days = NA_integer_,
                    same_day_note = NA_character_,
                    listed = "no 2026 general election listed",
                    stringsAsFactors = FALSE)
  if (is.na(sec)) return(row)

  row$listed <- "deadlines listed"
  row$same_day_note <- "no"
  for (b in every(sec, "(?s)data-deadline=\"[^\"]+\".*?(?=data-deadline=\"|\\z)")) {
    meth <- sub("^data-deadline=\"([^\"]+)\".*$", "\\1", first(b, "data-deadline=\"[^\"]+\""))
    # The same-day block is a sentence, not a deadline. Its dates belong to an
    # early-voting window and must never be read as one, so only its presence
    # is kept.
    if (meth == "same-day") { row$same_day_note <- "yes"; next }
    d <- first(b, "(?s)<span class=\"deadline-description\">.*?</span>")
    t <- untag(if (is.na(d)) b else d)
    dt <- npr_date(t)
    if (meth == "online") { row$online_date <- dt; row$online_days <- as.integer(ELECTION_DAY - dt) }
    if (meth == "mail") {
      row$mail_date <- dt; row$mail_days <- as.integer(ELECTION_DAY - dt)
      row$mail_rule <- c("Postmarked by" = "postmark", "Received by" = "receipt")[[npr_rule(t)]]
    }
    if (meth == "in-person") { row$in_person_date <- dt; row$in_person_days <- as.integer(ELECTION_DAY - dt) }
  }
  row
}))

# North Dakota registers nobody, so it publishes no deadline. This is the
# cross-check: two sources, gathered years and ways apart, agreeing on the one
# exception the chapter opens with.
nd <- dl[dl$jurisdiction == "North Dakota", ]
stopifnot(nrow(nd) == 1L, is.na(nd$in_person_days), is.na(nd$mail_days),
          is.na(nd$online_days))
dl$listed[dl$jurisdiction == "North Dakota"] <- "no registration required"

# Nothing may be silently half-parsed: a jurisdiction with deadlines listed
# must have an in-person one, since that is the door every one of them keeps
# open, and every date must fall in 2026 before the election.
stopifnot(nrow(dl) == 56L, !any(duplicated(dl$jurisdiction)))
stopifnot(all(!is.na(dl$in_person_days[dl$listed == "deadlines listed"])))
dts <- c(dl$online_date, dl$mail_date, dl$in_person_date)
dts <- dts[!is.na(dts)]
stopifnot(all(dts <= ELECTION_DAY), all(dts >= as.Date("2026-01-01")))
stopifnot(all(dl$mail_rule[!is.na(dl$mail_rule)] %in% c("postmark", "receipt")))
stopifnot(sum(dl$listed == "deadlines listed") == 54L)

dl <- dl[order(dl$jurisdiction), ]
write.csv(dl, "derived/deadlines.csv", row.names = FALSE, na = "")

cat(sum(dl$listed == "deadlines listed"), "jurisdictions with a 2026 general-election deadline\n")
cat("  mail deadline counts the postmark in", sum(dl$mail_rule == "postmark", na.rm = TRUE),
    "and the day it arrives in", sum(dl$mail_rule == "receipt", na.rm = TRUE), "\n")
cat("  no online registration in", sum(dl$listed == "deadlines listed" & is.na(dl$online_days)),
    "| no mail registration in", sum(dl$listed == "deadlines listed" & is.na(dl$mail_days)), "\n")
spread <- apply(dl[, c("online_days", "mail_days", "in_person_days")], 1,
                function(v) if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm = TRUE) - min(v, na.rm = TRUE)))
cat("  the three doors close on the same day in", sum(spread == 0, na.rm = TRUE),
    "and up to", max(spread, na.rm = TRUE), "days apart\n")

cat(sprintf("%d voters, %d columns\n", nrow(r), length(cn)))
cat("columns that decide which ballot you get:", sum(grp == "Which ballot you get"), "\n")
cat("voters who voted but are no longer in the 2026 file:\n")
print(lost)

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
