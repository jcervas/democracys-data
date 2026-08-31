# ---------------------------------------------------------------------------
# Build the lobbying chapter's four tables from the Lobbying Disclosure Act
# database.
#
#   raw/q2-2024-page-NN.json   the API's own answers, 60 pages of 25 filings,
#                              exactly as they arrived. Committed, because the
#                              sample cannot otherwise be recovered -- see
#                              WHY THE PAGES ARE COMMITTED below.
#   derived/filings.csv        one row per filing: client, registrant, client
#                              state, dollar amount, count of issue areas,
#                              count of distinct lobbyists
#   derived/issues.csv         general issue area x times mentioned
#   derived/entities.csv       government entity contacted x times mentioned
#   derived/lobbyists.csv      one row per distinct lobbyist: name and the
#                              covered position they disclosed, if any
#   derived/quarter.csv        the two numbers that describe the sample itself:
#                              how many filings the quarter holds, and how many
#                              of them are in here
#
# SOURCE. United States Senate / Clerk of the House, Lobbying Disclosure Act
# database, filings API, second quarter of 2024.
#
#   https://lda.gov/api/v1/filings/?filing_year=2024&filing_period=second_quarter&filing_type=Q2
#
# Keyless: no account, no token, no fee. **The host has moved.** The chapter
# cites `lda.senate.gov/api/`, which now answers 301 and redirects to `lda.gov`.
# The redirect is followed here rather than papered over, and the old host is
# left in the URL below so that the day it stops redirecting is the day this
# script fails loudly instead of quietly fetching nothing.
#
# THE FILTER IS THE WHOLE QUERY, AND GETTING IT WRONG IS SILENT. Ask the API
# for the second quarter of 2024 without `filing_type=Q2` and it will happily
# answer -- with 23,989 records of which the first hundred are 90 registrations,
# 7 amended registrations and 3 reports. Registrations carry `null` in both
# money fields, so an unfiltered pull produces a table of the right shape,
# the right size, and almost entirely zeros. `filing_type=Q2` is what makes
# this a quarterly-report file. It is asserted below, per page, rather than
# trusted.
#
# THE PIPE IS NARROW, AND THAT IS THE CHAPTER'S POINT. Anonymous callers get
# 25 records per request and roughly 15 requests a minute. 60 pages is 1,500
# filings -- 7.9% of the quarter. The chapter is built on rates and rankings
# and never on totals, precisely because the denominator here is a decision
# about how long somebody was willing to wait, not a fact about lobbying.
#
# WHY THE PAGES ARE COMMITTED. This file is a rebuild: the original build
# script was lost, and the four CSVs sat in the folder for a while with nothing
# that could regenerate them. Recovering the method was only possible because
# the API's default ordering is stable -- page 1 today still opens with the
# same filing that opens filings.csv. That is luck, and it is not a thing to
# rely on twice. The 60 JSON pages are therefore committed, and every number in
# the chapter is rebuilt from them offline. Re-fetch deliberately:
#
#     LOBBY_REFRESH=1 Rscript build-data.R
#
# and expect the sample to move when you do: the quarter's Q2 count is a live
# figure, and filings are amended for years afterwards.
#
# WHAT IS THROWN AWAY HERE, all of it deliberate and all of it discussed in the
# chapter:
#
#   * The activity DESCRIPTIONS. Free text, one per issue area, and the only
#     place in the filing that says what the lobbying was actually about. The
#     table keeps a count of activities and drops every word of them.
#   * The REGISTRANT's state. A filing carries two, and this keeps the
#     client's, which suits "who buys influence" and defeats "where is the
#     industry".
#   * The FILING KEY on the lobbyist rows. lobbyists.csv is a list of people,
#     not of people-in-filings, so no lobbyist can be joined to the client
#     paying them. That kills the obvious question -- do former staffers
#     command higher fees -- and it is a decision made here, not a limit of the
#     form.
#
# TWO COUNTING RULES THAT ARE NOT OBVIOUS:
#
#   n_lobbyists counts DISTINCT PEOPLE, not entries. The form asks who lobbied
#   on each issue area, so a firm working two issues with the same two people
#   lists four entries and employs two lobbyists. Counting entries would double
#   them.
#
#   Lobbyists are deduplicated on the API's own lobbyist id, not on name. The
#   result is 773 people of whom 15 share a name with somebody else in the
#   list, and a dozen share both a name and a disclosed position. Those are not
#   duplicate rows to be cleaned away: the database issues an id per person per
#   registrant, so one human being who lobbies through two firms is two ids and
#   belongs in the list twice. Deduplicating on the printed name instead would
#   silently merge distinct people who happen to be namesakes.
#
# TWO OTHER DIFFERENCES FROM THE FILES THIS REPLACES, both of them ordering.
# The old lobbyists.csv came out in no order at all -- the arbitrary order of a
# Python set, which is stable within a run and meaningless between them. It is
# written in first-appearance order here. Nothing in the chapter depends on it:
# the five disclosed positions the brief quotes resolve identically under both
# orders, which is checked at the bottom of this script rather than assumed.
# The old files were also CRLF, alone in a corpus of LF; they are LF now.
#
# ONE THING THE OLD BUILD DID THAT THIS ONE DOES NOT. The CSVs this replaces
# cut `client` and `registrant` at exactly 70 characters -- a clean prefix, mid
# word, on 34 of the 1,500 rows. Nothing in the chapter asked for it and nothing
# in the chapter shows it: every client the brief prints is short enough to
# survive, so the truncation was invisible in the rendered document and is
# invisible in every number computed from it. It is not reproduced here. Names
# now arrive whole, which means those 34 rows differ from the file that shipped
# before, and that is the only intended difference between them.
#
# THE MONEY COLUMN IS TWO COLUMNS. A firm lobbying for a client reports
# `income`; an organisation lobbying for itself reports `expenses`; a filing
# with nothing to report leaves both null. All three collapse into one
# `amount`, and the third becomes 0 -- which is why the chapter says the
# distinction between "reported nothing" and "had no box to report in" is
# destroyed here, in this line, and not by the filers.
#
# Run from inside this data/ folder.
# ---------------------------------------------------------------------------

options(scipen = 999, stringsAsFactors = FALSE)

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

stopifnot(requireNamespace("jsonlite", quietly = TRUE))

# Write a CSV that quotes only the fields that need it. write.csv() quotes every
# character field and the header too, which is valid CSV and is not what these
# four files have looked like since they were first written; matching the older
# writer keeps the diff to what actually changed in the data.
write_min_csv <- function(df, path) {
  esc <- function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    need <- grepl('[",\n]', v)
    v[need] <- paste0('"', gsub('"', '""', v[need]), '"')
    v
  }
  lines <- c(paste(names(df), collapse = ","),
             do.call(paste, c(lapply(df, esc), sep = ",")))
  writeLines(lines, path, useBytes = TRUE)
}

# Count in FIRST-APPEARANCE order, then sort by frequency. The order matters
# only for ties, and only for how the chapter's tables read -- but table() sorts
# its levels alphabetically, so counting that way would silently re-rank every
# pair of issue areas that happen to be mentioned the same number of times.
count_in_order <- function(x) {
  lv <- unique(x)
  n  <- as.integer(table(factor(x, levels = lv)))
  d  <- data.frame(value = lv, mentions = n)
  d[order(-d$mentions), ]                     # order() is stable: ties keep lv
}

API    <- "https://lda.senate.gov/api/v1/filings/"   # 301s to lda.gov; followed
YEAR   <- 2024
PERIOD <- "second_quarter"
FTYPE  <- "Q2"
PAGES  <- 60                 # 60 x 25 = the 1,500 filings the chapter reports
PAUSE  <- 4.5                # seconds between requests; the limit is ~15/min

page_url  <- function(p) sprintf("%s?filing_year=%d&filing_period=%s&filing_type=%s&page=%d",
                                 API, YEAR, PERIOD, FTYPE, p)
page_file <- function(p) sprintf("raw/q2-2024-page-%02d.json", p)

REFRESH <- nzchar(Sys.getenv("LOBBY_REFRESH"))

# --- 1. the pages -----------------------------------------------------------
for (p in seq_len(PAGES)) {
  f <- page_file(p)
  if (REFRESH || !file.exists(f)) {
    invisible(prov_fetch(page_url(p), f, label = sprintf("LDA Q2 2024 page %d", p)))
    Sys.sleep(PAUSE)
  }
}
cat("pages on disk:", sum(file.exists(vapply(seq_len(PAGES), page_file, ""))), "of", PAGES, "\n")

pages <- lapply(seq_len(PAGES), function(p)
  jsonlite::fromJSON(page_file(p), simplifyVector = FALSE))

# The count the database reports for the whole query. It is the one number in
# the chapter that the sample cannot produce, so it is carried out of the API
# response rather than typed into the brief.
QTOTAL <- pages[[1]]$count
stopifnot(is.numeric(QTOTAL), QTOTAL > 0)

recs <- unlist(lapply(pages, `[[`, "results"), recursive = FALSE)
stopifnot(length(recs) == PAGES * 25)

# Every record must be the filing type asked for. If the API ever starts
# ignoring the filter, this is where the build stops.
stopifnot(all(vapply(recs, function(r) identical(r$filing_type, FTYPE), TRUE)))

# --- 2. filings.csv ---------------------------------------------------------
# income, else expenses, else nothing reported. See the header.
amount_of <- function(r) {
  v <- if (!is.null(r$income)) r$income else if (!is.null(r$expenses)) r$expenses else 0
  as.numeric(v)
}

# Distinct people, by the API's lobbyist id. See the header.
lobbyist_ids <- function(r) {
  ids <- unlist(lapply(r$lobbying_activities, function(a)
    vapply(a$lobbyists, function(L) L$lobbyist$id, numeric(1))))
  unique(ids)
}

blank <- function(x) if (is.null(x)) "" else as.character(x)

filings <- data.frame(
  client       = vapply(recs, function(r) blank(r$client$name), ""),
  registrant   = vapply(recs, function(r) blank(r$registrant$name), ""),
  client_state = vapply(recs, function(r) blank(r$client$state), ""),
  amount       = vapply(recs, amount_of, 0),
  n_issues     = vapply(recs, function(r) length(r$lobbying_activities), 0L),
  n_lobbyists  = vapply(recs, function(r) length(lobbyist_ids(r)), 0L)
)
write_min_csv(filings, "derived/filings.csv")

# --- 3. issues.csv and entities.csv -----------------------------------------
# Both arrive already counted to mentions, which is as deep as this chapter can
# go: a mention is one activity block naming one code or one institution, with
# no date, no person and no dollar figure attached to it.
issue_codes <- unlist(lapply(recs, function(r)
  vapply(r$lobbying_activities, function(a) blank(a$general_issue_code_display), "")))
issues <- count_in_order(issue_codes)
names(issues) <- c("issue", "mentions")
write_min_csv(issues, "derived/issues.csv")

entity_names <- unlist(lapply(recs, function(r)
  unlist(lapply(r$lobbying_activities, function(a)
    vapply(a$government_entities, function(g) blank(g$name), "")))))
entities <- count_in_order(entity_names)
names(entities) <- c("entity", "mentions")
# The chapter shows the top eight and the table is a ranking, so it is cut at
# 40. The cut is recorded here because a truncated table that does not say it
# is truncated is the kind of thing this course is about.
ENT_KEEP <- 40
entities <- head(entities, ENT_KEEP)
write_min_csv(entities, "derived/entities.csv")

# --- 4. lobbyists.csv -------------------------------------------------------
# One row per distinct lobbyist id, in the order the API first names them.
#
# The covered position is the FIRST NON-EMPTY one the person gave, not the first
# one they gave. The same lobbyist is listed once per issue area they worked,
# and the covered-position box is not always filled in on every one of those
# entries -- 25 people in this sample name a former post on one activity and
# leave it blank on another. Taking whatever came first would drop a quarter of
# those, and would make "share disclosing a former post" a fact about the order
# the API returned activities in. Taking the first non-empty one answers the
# question the chapter actually asks of this column: did they disclose at all.
flat <- do.call(rbind, lapply(recs, function(r) {
  rows <- lapply(r$lobbying_activities, function(a) {
    if (!length(a$lobbyists)) return(NULL)
    data.frame(
      id   = vapply(a$lobbyists, function(L) L$lobbyist$id, numeric(1)),
      name = vapply(a$lobbyists, function(L) trimws(paste(
                      blank(L$lobbyist$first_name), blank(L$lobbyist$last_name))), ""),
      pos  = vapply(a$lobbyists, function(L) blank(L$covered_position), "")
    )
  })
  do.call(rbind, rows)
}))
first_id  <- flat[!duplicated(flat$id), ]                  # appearance order
disclosed <- flat[nchar(flat$pos) > 0, ]
disclosed <- disclosed[!duplicated(disclosed$id), ]        # first non-empty post
lobbyists <- data.frame(
  name = first_id$name,
  pos  = disclosed$pos[match(first_id$id, disclosed$id)]
)
lobbyists$pos[is.na(lobbyists$pos)] <- ""
names(lobbyists) <- c("name", "former_government_position")
write_min_csv(lobbyists, "derived/lobbyists.csv")

# --- 5. quarter.csv ---------------------------------------------------------
write_min_csv(data.frame(
  quarter            = "2024 Q2",
  filings_in_quarter = QTOTAL,
  filings_retrieved  = nrow(filings),
  api_url            = page_url(1)),
  "derived/quarter.csv")

# --- 6. what has to be true -------------------------------------------------
# Each of these is a claim the brief makes in prose. If the source moves under
# the chapter, the chapter should stop building rather than print a new number
# under an old sentence.
stopifnot(
  nrow(filings)   == 1500,
  all(filings$n_issues    >= 1),          # a Q2 report with no activity block
  all(filings$n_lobbyists >= 1),          #   would be a filing about nothing
  min(filings$amount[filings$amount > 0]) == 5000,   # the statutory threshold
  nrow(issues) > 50,
  nrow(entities) == ENT_KEEP,
  nrow(lobbyists) > 500,
  sum(nchar(lobbyists$former_government_position) > 0) > 100
)

# The brief's table of disclosed positions is built by grepping this column for
# five particular strings, chosen to show the range from "names a senator" to
# "is not a government job at all". A rebuild that dropped any of them would
# print a table with a blank row and say nothing about it.
for (s in c("John Cornyn", "Josh Hawley", "Assistant Secretary, HUD",
            "Detailee", "Rush Holt")) {
  if (!any(grepl(s, lobbyists$former_government_position, fixed = TRUE)))
    stop("the brief quotes a disclosed position containing '", s,
         "' and this build produced none")
}

cat(sprintf(paste0(
  "\n  quarter holds       %s filings\n",
  "  retrieved           %s  (%.1f%%)\n",
  "  reporting $0        %s\n",
  "  smallest positive   $%s   (statutory threshold)\n",
  "  median positive     $%s\n",
  "  issue areas         %s\n",
  "  entities kept       %s of %s\n",
  "  lobbyists           %s, %s disclosing a former post\n"),
  format(QTOTAL, big.mark = ","), format(nrow(filings), big.mark = ","),
  100 * nrow(filings) / QTOTAL,
  format(sum(filings$amount == 0), big.mark = ","),
  format(min(filings$amount[filings$amount > 0]), big.mark = ","),
  format(median(filings$amount[filings$amount > 0]), big.mark = ","),
  nrow(issues), ENT_KEEP, length(unique(entity_names)),
  nrow(lobbyists), sum(nchar(lobbyists$former_government_position) > 0)))

invisible(prov_report())

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
